default: test

alias b := build
build:
    cargo build

alias t := test
test: build
    cargo test -q
    cd core/gateware && pytest

alias a := assemble
assemble FILE: build
    cargo run --bin delta-null-core-assembler-bin {{join(invocation_directory(), FILE)}}

alias eb := emulator-backend
emulator-backend: build
    cargo run --bin delta-null-core-emulator-backend

alias ef := emulator-frontend
emulator-frontend: build
    cargo run --bin delta-null-core-emulator-frontend
