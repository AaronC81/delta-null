use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::quote;
use syn::{DeriveInput, parse_macro_input, Data, Fields, Type};

/// A derive proc-macro to apply to `InstructionOpcode`.
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

/// A derive proc-macro to apply to `Instruction`.
///
/// Generates a method on `InstructionOpcode` to attempt to construct an `Instruction` given
/// `AnyOperand`s:
/// 
/// ```ignore
/// InstructionOpcode::Nop.build(&[]) // => Some(Instruction::Nop)
/// InstructionOpcode::Inc.build(&[]) // => None
/// InstructionOpcode::Inc.build(&[AnyOperand::R(AnyRegister::G(GPR::R0))]) // => Some(Instruction::Inc { reg: GPR::R0 })
/// ```
#[proc_macro_derive(InstructionBuild)]
pub fn derive_instruction_build(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let Data::Enum(enum_data) = input.data else {
        panic!("only supported on enums");
    };

    let mut cases = TokenStream2::new();
    for variant in &enum_data.variants {
        let var_name = &variant.ident;
        let arm = match &variant.fields {
            Fields::Unit => {
                // No-operand instructions, like `nop`, are easy to construct
                // They don't expect any operands, so that's the only thing to check
                quote! {
                    if !ops.is_empty() { return None; }
                    Some(Instruction::#var_name)
                }
            },

            Fields::Named(fields) => {
                // Create a sequence of let-else expressions to ensure that each operand is the
                // expected kind of operand (register, immediate, etc)
                let mut validations = TokenStream2::new();
                for (i, field) in fields.named.iter().enumerate() {
                    let op_id = syn::Ident::new(&format!("operand_{i}"), Span::call_site());
                    let pattern = match &field.ty {
                        Type::Path(p) => {
                            if p.path.segments.len() != 1 {
                                panic!("unsupported type {p:#?} (path too long)");
                            }

                            let name = p.path.segments[0].ident.to_string();
                            match name.as_ref() {
                                "u8"  => quote!{ AnyOperand::I(#op_id) },
                                "GPR" => quote!{ AnyOperand::R(AnyRegister::G(#op_id)) },
                                "SPR" => quote!{ AnyOperand::R(AnyRegister::S(#op_id)) },
                                "DR"  => quote!{ AnyOperand::R(AnyRegister::D(#op_id)) },
                                _ => panic!("unsupported type {name} (unknown name)")
                            }
                        }
                        t => panic!("unsupported type {t:?} (unknown type kind)"),
                    };

                    validations.extend(quote! {
                        let #pattern = ops[#i] else { return None };
                    })
                }

                // Create field initialisers for each field of the instruction, which use the match
                // variables pulled out by the let-elses
                let mut field_inits = TokenStream2::new();
                for (i, field) in fields.named.iter().enumerate() {
                    let op_id = syn::Ident::new(&format!("operand_{i}"), Span::call_site());
                    let field_name = &field.ident.as_ref().unwrap();
                    field_inits.extend(quote! {
                        #field_name: #op_id,
                    })
                }

                // Check length, then check matches, then construct
                let number_of_fields = fields.named.len();
                quote!{
                    if ops.len() != #number_of_fields { return None; }
                    #validations
                    Some(Instruction::#var_name { #field_inits })
                }
            },

            Fields::Unnamed(_) => panic!("variant {} has unnamed fields, which is not supported", variant.ident),
        };

        cases.extend(quote! {
            Self::#var_name => { #arm },
        });
    }

    TokenStream::from(quote! {
        impl InstructionOpcode {
            pub fn build(&self, ops: &[AnyOperand]) -> Option<Instruction> {
                match self {
                    #cases
                }
            }
        }
    })
}
