// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate memmap;
extern crate perplex;

use std::fs::File;

use clap::{App, Arg};
use memmap::Mmap;
use perplex::grammar::{Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
use perplex::backend::{generate_parser, Backend};
use perplex::lexer::Lexer;

fn main() {
    // Parse the command line arguments.
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .arg(
            Arg::with_name("GRAMMAR")
                .help("The input grammar to process")
                .required(true)
                .index(1),
        )
        .get_matches();

    // Parse the input grammar.
    let _grammar = {
        let path = matches.value_of("GRAMMAR").unwrap();
        let file = File::open(path).expect("failed to open grammar file");
        let mmap = unsafe { Mmap::map(&file).expect("failed to memory map the grammar file") };
        let text = unsafe { std::str::from_utf8_unchecked(&mmap) };
        let lex = Lexer::new(text.char_indices());
        let tkn: Vec<_> = lex.collect();
        println!("{:?}", tkn);
    };
}
