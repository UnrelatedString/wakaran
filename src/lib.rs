use proc_macro::*;
use yaml_rust::{ YamlLoader, Yaml };
use yaml_rust::yaml::Hash;
use quote::quote;
use quote::format_ident;
use quote::ToTokens;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;
use proc_macro2::Literal as Literal2;
use proc_macro2::Ident as Ident2;
use proc_macro2::Group as Group2;
use syn::{ Result, Path, Expr, Token , LitStr};
use syn::parse::{ ParseStream, Parser };
use syn::punctuated::Punctuated;

macro_rules! panick {
    ($($arg:tt)*) => {
        return quote!(compile_error!($($arg),*)).into()
    }
}

// Pretty sure replacing expect! easily is impossible but it's all YAML stuff anyways

fn parse_include_args(input: ParseStream) -> Result<(LitStr, syn::Ident)> {
    let path = input.parse()?;
    input.parse::<Token![as]>()?;
    let table_name = input.parse()?;
    Ok((path, table_name))
}

/// Use like
///
/// ```rust
/// wakaran::include_keys! {
///     "./loc.yml" as localization_keys
/// }
/// ```
///
/// to include key-value mappings from a YAML file
/// as well as an enum `Language` representing available language options
/// (which generates with `Serialize` and `Deserialize` implementations
/// if the `serde` feature is specified)
/// as a table in the form of nested modules
/// with the specified name for the top-level module.
///
/// The YAML file is expected to be formatted in two documents
/// (separated by a line containing nothing but `---`).
/// The first document should contain only a list of language codes.
/// The second document should contain arbitrarily nested maps
/// where every leaf node is a map from every language code to a string
/// or array of strings.
#[proc_macro]
pub fn include_keys(input: TokenStream) -> TokenStream {

    let (path, table_name) = syn::parse_macro_input!(input with parse_include_args);

    // Disappointed there doesn't seem to be an easy way to get a char iterator out of a BufReader
    let yaml = std::fs::read_to_string(path.value()).expect("Arg 1 to include_keys! should be valid file!");
    let docs = YamlLoader::load_from_str(&yaml).expect("Keys should be valid YAML!");

    let (langs, keys) = match docs.as_slice() {
        [Yaml::Array(raw_langs), Yaml::Hash(keys)] => {
            let langs = raw_langs.into_iter()
                                 .map(|s| s.as_str().map(format_lang_code))
                                 .collect::<Option<Vec<Ident2>>>()
                                 .expect("All language codes should be strings!");
            (langs, keys)
        },
        _ => panick!("Keys should contain langs array and keys mapping in separate YAML documents!")
    };
    
    let enum_annotations = if cfg!(feature = "serde") {
        quote!( #[derive(serde::Deserialize, serde::Serialize)] )
    } else {
        quote!()
    };

    let langs_defs = quote! {
        #enum_annotations
        pub enum Language {
            #(#langs),*
        }

        pub trait Localizable<T: 'static + ?Sized> {
            #(const #langs: &'static T;)*
            fn localize(&self, lang: Language) -> &'static T {
                match lang {
                    #(Language::#langs => Self::#langs),*
                }
            }
        }
    };

    let keys_defs = tabulate(format_ident!("keys"), keys);

    quote! {
        mod #table_name {
            #langs_defs
            #keys_defs
        }
    }.into()
}

fn format_lang_code(code: &str) -> Ident2 {
    let sanitized = code.replace("-", "_");
    format_ident!("{sanitized}")
}

fn key_ident(key: &Yaml) -> Ident2 {
    let key_str = key.as_str().expect("All localization keys should be strings!");
    // honestly just
    format_lang_code(key_str)
}

fn tabulate(name: Ident2, hash: &Hash) -> TokenStream2 {
    // Doesn't bother validating langs since it's just a compiler error if those are wrong anyways

    if let Some(hashes) = hash.into_iter().map(
        |(key, value)|
            value.as_hash().map(|h| (key_ident(key), h))
    ).collect::<Option<Vec<(Ident2, &Hash)>>>() {
        let children = hashes.into_iter().map(
            |(key, value)|
                tabulate(key, value)
        );
        quote! {
            pub mod #name {
                use super::Localizable; // peak comedy right here
                #(#children)*
            }
        }
    } else if let Some(str_keys) = hash.into_iter().map(
        |(key, value)|
            value.as_str().map(|s| (key_ident(key), s))
    ).collect::<Option<Vec<(Ident2, &str)>>>() {
        let mappings = str_keys.into_iter().map(
            |(lang, string)| {
                let lit = Literal2::string(string);
                quote! {
                    const #lang: &str = #lit;
                }
            }
        );
        quote! {
            pub struct #name;
            impl Localizable<str> for #name {
                #(#mappings)*
            }
        }
    } else if let Some(arr_keys) = hash.into_iter().map(
        |(key, value)|
            value.as_vec().map(
                |a| {
                    let strs = a.into_iter()
                                .map(Yaml::as_str)
                                .collect::<Option<Vec<&str>>>()
                                .expect("Arrays should only contain strings!");
                    (key_ident(key), strs)
                })
    ).collect::<Option<Vec<(Ident2, Vec<&str>)>>>() {
        let mappings = arr_keys.into_iter().map(
            |(lang, array)| {
                let str_lits = array.into_iter().map(Literal2::string);
                quote! {
                    const #lang: [&str] = [#(#str_lits),*];
                }
            }
        );
        quote! {
            pub struct #name;
            impl Localizable<[&str]> #name {
                #(#mappings)*
            }
        }
    } else {
        panick!("Keyed values should contain either all maps, all strings, or all arrays!")
    }
}

/// Recursively localizes every key within the item. Macro arguments are of the form
///
/// ```rust
/// #[wakaran::localize(path::to::table[language_expression])]
/// ```
///
/// where `language_expression` is patched into the macro's output to be evaluated
/// in the local scope, at runtime,
/// to a variant of the `Language` enum determining
/// the string or array value to be used.
///
/// Key expressions use the ad-hoc syntax `$hierarchical.keys`,
/// where the hierarchy begins one layer *below* the top level module
/// constituting the table itself.
#[proc_macro_attribute]
pub fn localize(args: TokenStream, input: TokenStream) -> TokenStream {
    let (table_path, lang_expr) = syn::parse_macro_input!(args with parse_localize_args);
    let parse_body = LocContextParser {table_path: &table_path, lang_expr: &lang_expr}.parser();
    syn::parse_macro_input!(input with parse_body).into()
}

/// 
#[proc_macro]
pub fn localize_block(input: TokenStream) -> TokenStream {
    syn::parse_macro_input!(input with parse_localize_block)
}

fn parse_localize_block(stream: ParseStream) -> Result<TokenStream> {
    stream.parse::<Token![use]>()?;
    let (table_path, lang_expr) = stream.call(parse_localize_args)?;
    stream.parse::<Token![;]>().map(|_| ()).or_else(|_| stream.parse::<Token![in]>().map(|_| ()))?;
    let parse_body = LocContextParser {table_path: &table_path, lang_expr: &lang_expr};
    parse_body.parse_from_stream(stream).map(Into::into)
}

fn parse_localize_args(stream: ParseStream) -> Result<(Path, Expr)> {
    let table_path = Path::parse_mod_style(stream)?;
    let raw_lang_expr; syn::bracketed!(raw_lang_expr in stream);
    let lang_expr = raw_lang_expr.parse()?;
    Result::Ok((table_path, lang_expr))
}

// I COULD make this customizable. For the sake of not wasting a third day on this, I won't
mod why_is_this_pub {
    syn::custom_punctuation!(LocPrefix, $);
}

use why_is_this_pub::*;

fn parse_loc_substitution(stream: ParseStream) -> Result<Punctuated<syn::Ident, Token![.]>> {
    stream.parse::<LocPrefix>()?;
    stream.call(Punctuated::parse_separated_nonempty)
}

#[derive(Clone)]
struct LocContextParser<'a, T, L> {
    table_path: &'a T,
    lang_expr: &'a L,
}

impl<'a, T: ToTokens + Clone, L: ToTokens + Clone> LocContextParser<'a, T, L> {
    fn parse_from_stream(&self, stream: ParseStream) -> Result<TokenStream2> {
        let mut built = Vec::new();
        while !stream.is_empty() {
            if let Ok(punctuated) = parse_loc_substitution(stream) {
                let hierarchy = punctuated.iter();
                let table_path = self.table_path;
                let lang_expr = self.lang_expr;
                built.extend(
                    quote!( #table_path::Localizable::localize(
                        &#table_path::keys::#(#hierarchy)::*, #lang_expr
                    ) )
                )
            } else {
                let tt = stream.step(
                    |cursor| cursor.token_tree().ok_or_else(|| stream.error("literally how?"))
                )?;
                built.push(
                    if let TokenTree2::Group(group) = tt {
                        let deep = self.clone().parser().parse2(group.stream())?;
                        Group2::new(group.delimiter(), deep).into()
                    } else {
                        tt
                    }
                )
            }
        }
        Ok(TokenStream2::from_iter(built))
    }

    fn parser(self) -> impl 'a + FnOnce(ParseStream) -> Result<TokenStream2> {
        move |stream: ParseStream| self.parse_from_stream(stream)
    }

}


// TODO: extra mechanism for forcing one language choice on a key and/or having 
//       one language substitute for another / blank out entirely, so I can do,
//       like, something on the character select screen like Melty Blood does
//       where the Japanese name is in big text and the English name is also
//       right there under it but hypothetically speaking if I were to add a
//       third language option it could keep the Japanese but replace the 
//       English with that third language--which is to say, when the game is
//       set to Japanese, it forces the small text into English iff it's Japanese
//       (but put this in the trait, not a macro--should be able to handle
//       programmatically with passing loc structs around, for stuff like
//       associating character names with character data)
