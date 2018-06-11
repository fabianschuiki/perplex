// Copyright (c) 2018 Fabian Schuiki

//! Implementation of the Honalee Algorithm for item set generation.

use std::collections::VecDeque;

use grammar::{self, Grammar};
use first::FirstSets;
use item_set::{Item, ItemSet};

/// Construct the item sets for a grammar.
#[allow(unused_variables)]
#[allow(unused_mut)]
#[allow(unused_assignments)]
pub fn construct_item_sets(grammar: &Grammar) -> Vec<ItemSet> {
    let mut done_list: Vec<ItemSet> = vec![];
    let mut todo_list: Vec<ItemSet> = vec![];
    let mut inc_list: VecDeque<ItemSet> = VecDeque::new();
    let mut come_from: Option<usize> = None;

    // Compute the first sets for the grammar.
    let first_sets = FirstSets::compute(grammar);

    // Create the initial item set.
    let initial = ItemSet {
        items: vec![
            Item {
                rule: grammar::ACCEPT,
                lookahead: grammar::END,
                marker: 0,
            },
        ],
        kernel: 1,
    };
    println!("initial item set: {}", initial.pretty(grammar));
    todo_list.push(initial);

    // The main loop.
    while !todo_list.is_empty() || !inc_list.is_empty() {
        // Phase 1: Calculate the closure over all todo item sets and either
        // merge them with an existing set, or add them to the incomplete list
        // for transition generation.
        for mut item_set in todo_list.drain(..) {
            item_set.closure(grammar, &first_sets);
            println!("closed: {}", item_set.pretty(grammar));
            // TODO: try merge, upon fail add to inc_list
            inc_list.push_back(item_set);
        }

        // Phase 2: For one incomplete item set, compute the transitions and
        // spawn subsequent item sets, which will then be processed in phase 1
        // of the next iteration.
        if let Some(item_set) = inc_list.pop_front() {
            println!("completed: {}", item_set.pretty(grammar));
            come_from = Some(done_list.len());
            done_list.push(item_set);
        } else {
            come_from = None;
        }
    }

    done_list
}
