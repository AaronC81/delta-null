use std::{fs::OpenOptions, io::{Write, stdout}, process::exit};

use clap::Parser as ClapParser;
use clap_stdin::FileOrStdin;
use delta_null_core_instructions::ToAssembly;
use delta_null_lang_backend::ir::{PrintIR, PrintOptions};
use delta_null_lang_backend_core::compile_module;
use delta_null_lang_frontend::code_to_module;

#[derive(ClapParser, Debug)]
struct Args {
    /// Input code file, or `-` to read stdin (end with ^D)
    input: FileOrStdin,

    /// Output file, or stdout if not given
    #[arg(short = 'o', long)]
    output_file: Option<String>,

    /// Print IR and stop, without executing the compiler backend
    #[arg(long)]
    to_ir: bool,
}

fn main() {
    let args = Args::parse();

    // Read and compile
    let input = args.input.to_string();
    let module = code_to_module(&input).unwrap();
    if args.to_ir {
        println!("{}", module.functions.into_iter()
            .map(|f| f.print_ir(&PrintOptions::new()))
            .collect::<Vec<_>>()
            .join("\n"));
        exit(0)
    }

    let asm = compile_module(&module).unwrap();

    // Open output
    let mut output_handle: Box<dyn Write> =
        if let Some(file) = args.output_file {
            Box::new(OpenOptions::new()
                .write(true)
                .create(true)
                .open(file)
                .unwrap())
        } else {
            Box::new(stdout())
        };

    // Write assembly
    let asm_code = asm.into_iter()
        .map(|i| i.to_assembly())
        .collect::<Vec<_>>()
        .join("\n");
    output_handle.write(asm_code.as_bytes()).unwrap();
}
