//! Light CLI wrapper around the `delta-null-core-assembler` project.
//! All assembly parsing and building logic is written here.

use std::{process::exit, fs::OpenOptions, io::{stdout, Write}, error::Error};

use clap::{Parser as ClapParser, ValueEnum};
use clap_stdin::FileOrStdin;
use delta_null_core_assembler::{Parser, Builder, Tokenizer};

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum OutputFormat {
    /// Space-separated hexadecimal words: 1234 ABCD 5678 ...
    AsciiHex,

    /// Raw bitstream of big-endian words
    Raw,
}

#[derive(ClapParser, Debug)]
struct Args {
    /// Input assembly code file, or `-` to read stdin (end with ^D)
    input: FileOrStdin,

    /// Output format for assembled bytes
    #[arg(short = 'f', long, default_value = "ascii-hex")]
    output_format: OutputFormat,

    /// Output file, or stdout if not given
    #[arg(short = 'o', long)]
    output_file: Option<String>,

    /// Start address to assemble from
    #[arg(short = 's', long, default_value = "0", value_parser = clap_num::maybe_hex::<u16>)]
    start_address: u16,
}

fn main() {
    let args = Args::parse();

    // Tokenize code
    let code = args.input.to_string();
    let mut tokenizer = Tokenizer::new(code.chars().peekable());
    let tokens = unwrap_or_abort(tokenizer.tokenize(), "Tokenize");

    // Parse code
    let mut parser = Parser::from_tokens(&tokens);
    let items = unwrap_or_abort(parser.parse(), "Parse");

    // Build
    let mut builder = Builder::new();
    let words = unwrap_or_abort(builder.build(&items, args.start_address), "Build");

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

    // Transform into output format and write
    match args.output_format {
        OutputFormat::AsciiHex => {
            let mut is_first = true;
            for word in words {
                if !is_first {
                    output_handle.write(&[b' ']).unwrap();
                }
                is_first = false;
                output_handle.write(format!("{word:0>4X}").as_bytes()).unwrap();
            }
            output_handle.write(&[b'\n']).unwrap();
        },
        OutputFormat::Raw => {
            for word in words {
                output_handle.write(&word.to_be_bytes()).unwrap();
            }
        },
    }
}

fn unwrap_or_abort<T>(result: Result<T, Vec<impl Error>>, stage: &str) -> T {
    match result {
        Ok(t) => t,
        Err(errors) => {
            eprintln!("{stage} errors occurred.\n");
            for error in errors {
                eprintln!("{error}");
            }
            exit(1)
        }
    }
}
