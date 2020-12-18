#!/usr/bin/env bash

pacman -Syu rustup rust-analyzer
rustup toolchain install stable

# install std sources for rust-analyzer
rustup component add rust-src

cargo install cargo-edit
