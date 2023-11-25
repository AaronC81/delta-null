//! Light CLI wrapper around the `delta-null-core-assembler` project.
//! All assembly parsing and building logic is written here.

use std::{process::exit, fs::OpenOptions, io::{stdout, Write}, error::Error};

use clap::{Parser as ClapParser, ValueEnum, CommandFactory, error::ErrorKind};
use clap_stdin::FileOrStdin;
use delta_null_core_assembler::{Parser, Builder, Tokenizer};
use delta_null_core_instructions::{InstructionOpcode, Instruction};
use serde_json::{Value, json};
use strum::IntoEnumIterator;

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

    /// Instead of normal operation, print a JSON configuration snippet for syntax highlighting,
    /// compatible with the `fabiospampinato.vscode-highlight` extension for Visual Studio Code
    #[arg(long, default_value = "false")]
    generate_highlight_config: bool,
}

fn main() {
    let args = Args::parse();

    if args.generate_highlight_config {
        println!("{}", serde_json::to_string_pretty(&generate_highlight_config()).unwrap());
        exit(0);
    }

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

fn generate_highlight_config() -> Value {
    // Collect instruction mnemonics into a regex
    let mnemonics = InstructionOpcode::iter()
        .map(|op| op.mnemonic().to_string())
        .collect::<Vec<_>>()
        .join("|");
    let mnemonic_regex = format!("\\b({mnemonics})\\b");

    json!({
        "highlight.regexes": {
            // Labels
            "\\b([A-Za-z0-9_]+:)": {
                "regexFlags": "g",
                "filterFileRegex": ".*\\.dna",
                "decorations": [
                    { "color": "bisque" }
                ]
            },

            // Directives
            "(\\.put|\\.word)\\b": {
                "regexFlags": "g",
                "filterFileRegex": ".*\\.dna",
                "decorations": [
                    { "color": "plum" }
                ]
            },

            // Comments
            "(;.+\\n)": {
                "regexFlags": "g",
                "filterFileRegex": ".*\\.dna",
                "decorations": [
                    { "color": "olivedrab" }
                ]
            },
            
            // Registers
            "\\b(r0|r1|r2|r3|r4|r5|r6|r7|ip|rp|sp|ef)\\b": {
                "regexFlags": "g",
                "filterFileRegex": ".*\\.dna",
                "decorations": [
                    { "color": "lightskyblue" }
                ]
            },

            // Instruction mnemonics
            mnemonic_regex: {
                "regexFlags": "g",
                "filterFileRegex": ".*\\.dna",
                "decorations": [
                    { "color": "cornflowerblue" }
                ]
            }
        }
    })
}
