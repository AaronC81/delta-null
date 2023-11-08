use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{DeriveInput, parse_macro_input, Data};

/// A derive proc to apply to `InstructionOpcode`.
/// 
/// Generates methods for converting to/from mnemonics:
/// 
/// ```ignore
/// InstructionOpcode::from_mnemonic("d_mov") // => Some(InstructionOpcode::DMov)
/// InstructionOpcode::DMov.mnemonic() // => "d_mov"
/// ```
#[proc_macro_derive(InstructionOpcodeMnemonics)]
pub fn derive_instruction_opcode_mnemonics(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let Data::Enum(enum_data) = input.data else {
        panic!("only supported on enums");
    };

    let mut from_mnemonic_cases = TokenStream2::new();
    let mut to_mnemonic_cases = TokenStream2::new();

    for variant in &enum_data.variants {
        let var_name = &variant.ident;
        let mnemonic = pascal_to_snake_case(&var_name.to_string());

        from_mnemonic_cases.extend(quote! {
            #mnemonic => ::std::option::Option::Some(Self::#var_name),
        });
        to_mnemonic_cases.extend(quote! {
            Self::#var_name => #mnemonic,
        })
    }

    TokenStream::from(quote! {
        impl #name {
            pub fn from_mnemonic(mnemonic: &str) -> ::std::option::Option<#name> {
                match mnemonic {
                    #from_mnemonic_cases
                    _ => ::std::option::Option::None,
                }
            }

            pub fn mnemonic(&self) -> &str {
                match self {
                    #to_mnemonic_cases
                }
            }
        }
    })
}

fn pascal_to_snake_case(name: &str) -> String {
    let mut result = String::new();

    for (i, char) in name.chars().enumerate() {
        if char.is_ascii_uppercase() {
            // Don't add an underscore if we're at the beginning
            // We want "SomeThing" -> "some_thing", not "_some_thing"
            if i != 0 {
                result.push('_');
            }

            result.push(char.to_ascii_lowercase());
        } else {
            result.push(char);
        }
    }

    result
}
