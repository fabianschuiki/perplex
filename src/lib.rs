// Copyright (c) 2018 Fabian Schuiki

//! A grammar analyzer and parser generator for almost-context-free languages.

#![deny(missing_docs)]

extern crate bit_set;
extern crate indexmap;
#[macro_use]
extern crate log;
extern crate perplex_runtime;

pub mod grammar;
pub mod first;
pub mod item_set;
pub mod machine;
pub mod backend;
mod honalee;
pub mod lexer;
pub mod parser;
pub mod glr;
pub mod ext;

/// A pretty printer.
pub struct Pretty<C, T> {
    ctx: C,
    item: T,
}

impl<C, T> Pretty<C, T> {
    pub(crate) fn new(ctx: C, item: T) -> Pretty<C, T> {
        Pretty { ctx, item }
    }
}
