// Copyright (c) 2018 Fabian Schuiki

//! Implementation of the Honalee Algorithm for item set generation.

use std::collections::VecDeque;

use bit_set::BitSet;

use grammar::{self, Grammar, NonterminalId, Symbol};
use first::FirstSets;
use item_set::{Action, Item, ItemSet};

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
    let initial = ItemSet::with_items(
        0,
        vec![
            Item {
                rule: grammar::ACCEPT,
                lookahead: grammar::END,
                marker: 0,
            },
        ],
    );
    println!("initial item set: {}", initial.pretty(grammar));
    todo_list.push(initial);

    // The main loop.
    let mut tmp_set_id = 1; // ID for new sets
    let mut inc_set_id = 1; // ID for sets after merging
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
        if let Some(mut item_set) = inc_list.pop_front() {
            println!("completing:");
            println!("{}", item_set.pretty(grammar));
            item_set
                .item_actions
                .resize(item_set.items.len(), BitSet::new());

            let root_symbol = Symbol::Nonterminal(NonterminalId::from_usize(0));
            let mut treated = BitSet::with_capacity(item_set.items.len());
            for i in 0..item_set.items.len() {
                if treated.contains(i) {
                    continue;
                }
                let item = item_set.items[i];
                let symbol = if item.rule == grammar::ACCEPT {
                    if item.marker != 0 {
                        continue;
                    }
                    &root_symbol
                } else {
                    let symbols = grammar.rule(item.rule).symbols();
                    if item.marker < symbols.len() {
                        &symbols[item.marker] // TODO: use proper marker math
                    } else {
                        continue;
                    }
                };
                println!("- shift {}", symbol.pretty(grammar));

                let mut new_set = ItemSet::new(tmp_set_id);
                tmp_set_id += 1;
                let action_id = item_set.actions.len();
                item_set
                    .actions
                    .push((symbol.clone(), Action::Shift(new_set.id)));

                for n in i..item_set.items.len() {
                    if treated.contains(n) {
                        continue;
                    }
                    let item2 = item_set.items[n];
                    let symbol2 = if item2.rule == grammar::ACCEPT {
                        if item.marker != 0 {
                            continue;
                        }
                        &root_symbol
                    } else {
                        let symbols = grammar.rule(item2.rule).symbols();
                        if item2.marker < symbols.len() {
                            &symbols[item2.marker]
                        } else {
                            continue;
                        }
                    };
                    if symbol2 != symbol {
                        continue;
                    }
                    new_set.items.push(Item {
                        rule: item2.rule,
                        lookahead: item2.lookahead,
                        marker: item2.marker + 1,
                    });
                    item_set.item_actions[n].insert(action_id);
                    treated.insert(n);
                }

                new_set.kernel = new_set.items.len();

                println!("{}", new_set.pretty(grammar));
                todo_list.push(new_set);
            }

            come_from = Some(done_list.len());
            done_list.push(item_set);
        } else {
            come_from = None;
        }
    }

    done_list
}
