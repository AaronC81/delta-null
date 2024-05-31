use std::{fs::OpenOptions, io::{stdout, Write}, path::PathBuf, process::exit};

use clap::{Parser as ClapParser, ValueEnum};
use clap_stdin::FileOrStdin;
use delta_null_core_instructions::ToAssembly;
use delta_null_lang_backend::ir::{PrintIR, PrintOptions};
use delta_null_lang_backend_core::compile_module;
use delta_null_lang_frontend::{node, parse_all_modules, source::SourceInputType, translate_one_module};

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

fn graceful_unwrap<T, E: std::fmt::Display>(result: Result<T, Vec<E>>) -> T {
    match result {
        Ok(m) => m,
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
            exit(1)
        }
    }
}

fn main() {
    let args = Args::parse();

    // Run frontend
    let input_type = match &args.input.source {
        clap_stdin::Source::Stdin => SourceInputType::Buffer,
        clap_stdin::Source::Arg(f) => SourceInputType::File(PathBuf::from(f).canonicalize().unwrap()),
    };
    let input = args.input.contents().unwrap();
    
    // Parse all modules
    let parsed_modules = graceful_unwrap(parse_all_modules(&input, input_type));

    // Concatenate all of the modules into one big one
    let mut module = node::Module::new(SourceInputType::Buffer);
    for parsed_module in parsed_modules {
        module.items.extend(parsed_module.items)
    }

    // Translate our one big module
    let module = graceful_unwrap(translate_one_module(module));

    // `--ir` stops here
    if let Some(ir_format) = args.ir {
        let content = match ir_format {
            IrFormat::Text => module.functions.iter()
                .map(|f| f.print_ir(&PrintOptions::new()))
                .collect::<Vec<_>>()
                .join("\n"),
            IrFormat::Dot => module.print_ir_as_graph(&PrintOptions::new()),
        };
        println!("{}", content);
        exit(0)
    }

    // Run backend
    let asm = graceful_unwrap(compile_module(&module));

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
