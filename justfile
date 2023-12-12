default: test

alias b := build
build:
    cargo build

alias t := test
test: build
    cargo test -q
    cd core/gateware && pytest

alias a := assemble
assemble *ARGS: build
    cd "{{invocation_directory()}}" && cargo run --bin delta-null-core-assembler-bin {{ARGS}}

alias c := compile
compile *ARGS: build
    cd "{{invocation_directory()}}" && cargo run --bin delta-null-lang-compiler {{ARGS}}

alias eb := emulator-backend
emulator-backend: build
    cargo run --bin delta-null-core-emulator-backend

alias ef := emulator-frontend
emulator-frontend: build
    cargo run --bin delta-null-core-emulator-frontend

update-vscode-config: build
    cargo run --bin delta-null-core-assembler-bin /dev/null --generate-highlight-config > .vscode/settings.json
