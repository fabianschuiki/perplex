// Copyright (c) 2018 Fabian Schuiki

//! Implementation of the Honalee Algorithm for item set generation.

use grammar::{self, Grammar};
use item_set::{Item, ItemSet};

/// Construct the item sets for a grammar.
#[allow(unused_variables)]
#[allow(unused_mut)]
pub fn construct_item_sets(grammar: &Grammar) -> Vec<ItemSet> {
    let mut done_list: Vec<ItemSet> = vec![];
    let mut todo_list: Vec<ItemSet> = vec![];
    let mut inc_list: Vec<ItemSet> = vec![];
    let mut come_from: Option<usize> = None;

    // Create the initial item set.
    let initial = ItemSet {
        items: vec![
            Item {
                rule: grammar::ACCEPT,
                lookahead: grammar::END,
                marker: 0,
            },
        ],
        complete: false,
    };
    println!("initial item set: {}", initial.pretty(grammar));

    vec![]
}
