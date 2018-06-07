// Copyright (c) 2018 Fabian Schuiki

//! A grammar analyzer and parser generator for almost-context-free languages.

#![deny(missing_docs)]

extern crate bit_set;

pub mod grammar;
pub mod first;

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
