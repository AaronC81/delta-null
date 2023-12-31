use std::{fs::OpenOptions, io::{Write, stdout}, process::exit};

use clap::{Parser as ClapParser, ValueEnum};
use clap_stdin::FileOrStdin;
use delta_null_core_instructions::ToAssembly;
use delta_null_lang_backend::ir::{PrintIR, PrintOptions};
use delta_null_lang_backend_core::compile_module;
use delta_null_lang_frontend::code_to_module;

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum IrFormat {
    /// Text listing
    Text,

    /// GraphViz DOT control-flow graph
    Dot,
}

#[derive(ClapParser, Debug)]
struct Args {
    /// Input code file, or `-` to read stdin (end with ^D)
    input: FileOrStdin,

    /// Output file, or stdout if not given
    #[arg(short = 'o', long)]
    output_file: Option<String>,

    /// Print IR in given format and stop, without executing the compiler backend
    #[arg(long)]
    ir: Option<IrFormat>,
}

fn main() {
    let args = Args::parse();

    // Run frontend
    let filename = match &args.input.source {
        clap_stdin::Source::Stdin => "<stdin>".to_owned(),
        clap_stdin::Source::Arg(f) => f.to_owned(),
    };
    let input = args.input.contents().unwrap();
    let module = match code_to_module(&input, &filename) {
        Ok(m) => m,
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
            exit(1)
        }
    };

    // `--ir` stops here
    if let Some(ir_format) = args.ir {
        let content = match ir_format {
            IrFormat::Text => module.functions.into_iter()
                .map(|f| f.print_ir(&PrintOptions::new()))
                .collect::<Vec<_>>()
                .join("\n"),
            IrFormat::Dot => module.print_ir_as_graph(&PrintOptions::new()),
        };
        println!("{}", content);
        exit(0)
    }

    // Run backend
    let asm = match compile_module(&module) {
        Ok(a) => a,
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
            exit(1);
        }
    };

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
    output_handle.write_all(asm_code.as_bytes()).unwrap();
}
