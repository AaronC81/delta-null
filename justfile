default: test

alias b := build
build:
    cargo build

alias t := test
test: build
    cargo test -q
    cd core/gateware && pytest
