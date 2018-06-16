// Copyright (c) 2018 Fabian Schuiki

//! Implementation of the Honalee Algorithm for item set generation.

use std::collections::{BTreeSet, HashMap};

use bit_set::BitSet;

use grammar::{self, Grammar, NonterminalId, RuleId, Symbol};
use first::FirstSets;
use item_set::{Action, Item, ItemSet, ItemSets, KernelCores};

/// Construct the item sets for a grammar.
#[allow(unused_variables)]
#[allow(unused_mut)]
#[allow(unused_assignments)]
pub(crate) fn construct_item_sets(grammar: &Grammar) -> ItemSets {
    let mut done_list: Vec<ItemSet> = vec![];
    let mut todo_list: Vec<ItemSet> = vec![];
    let mut inc_list: BTreeSet<usize> = BTreeSet::<usize>::new();
    let mut come_from: Option<usize> = None;
    let mut merge_hint: HashMap<KernelCores, Vec<usize>> = HashMap::new();

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
        'todo_sets: for mut item_set in todo_list.drain(..) {
            item_set.closure(grammar, &first_sets);
            println!("phase 1: {}", item_set.pretty(grammar));

            // Make sure there is a list of actions for each item.
            item_set
                .item_actions
                .resize(item_set.items.len(), BitSet::new());

            // Generate the reduce actions of this item set.
            let mut reduce_lookup: HashMap<Symbol, RuleId> = HashMap::new();
            for i in 0..item_set.items.len() {
                let item = &item_set.items[i];
                let rule = if item.rule == grammar::ACCEPT {
                    if item.marker == 1 {
                        RuleId::from_usize(0)
                    } else {
                        continue;
                    }
                } else {
                    let symbols = grammar.rule(item.rule).symbols();
                    if item.marker == symbols.len() {
                        item.rule
                    } else {
                        continue;
                    }
                };
                let action_id = item_set.actions.len();
                item_set
                    .actions
                    .push((item.lookahead.into(), Action::Reduce(rule)));
                item_set.item_actions[i].insert(action_id);
                reduce_lookup.insert(item.lookahead.into(), rule);
            }

            // Consider all done item sets with the same kernel item cores as
            // potential candidates to merge this item set into.
            for &index in merge_hint
                .get(&item_set.kernel_item_cores())
                .iter()
                .flat_map(|i| i.iter())
            {
                println!("- maybe can be merged with {}", index,);
                println!("reduce_lookup: {:?}", reduce_lookup);
                println!("merge_set.actions: {:?}", done_list[index].actions);
                println!("{}", done_list[index].pretty(grammar));

                // Make sure that merging would not produce any conflicts.
                let no_conflicts = done_list[index].actions.iter().all(
                    |&(ref symbol, merge_rule)| match reduce_lookup.get(symbol) {
                        Some(&rule) if Action::Reduce(rule) != merge_rule => false,
                        _ => true,
                    },
                );
                if no_conflicts {
                    println!("  - no conflicts");
                    if let Some(come_from) = come_from {
                        done_list[come_from]
                            .replace_actions(Action::Shift(item_set.id), Action::Shift(index));
                    }
                    // done_list[index].merge(item_set);
                    // inc_list.insert(index);
                    continue 'todo_sets;
                }
            }
            // TODO: try merge, upon fail add to inc_list

            // Add the item set to the done list and mark it as incomplete.
            let id = done_list.len();
            if id != item_set.id {
                if let Some(come_from) = come_from {
                    done_list[come_from]
                        .replace_actions(Action::Shift(item_set.id), Action::Shift(id));
                }
                item_set.id = id;
            }
            merge_hint
                .entry(item_set.kernel_item_cores())
                .or_insert_with(|| Vec::new())
                .push(id);
            done_list.push(item_set);
            inc_list.insert(id);
            // inc_list.push_back(item_set);
        }

        // Phase 2: For one incomplete item set, compute the transitions and
        // spawn subsequent item sets, which will then be processed in phase 1
        // of the next iteration.
        if let Some(&index) = inc_list.iter().next() {
            inc_list.remove(&index);
            let mut item_set = &mut done_list[index];
            println!("phase 2:");
            println!("{}", item_set.pretty(grammar));

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

            come_from = Some(index);
        } else {
            come_from = None;
        }
    }

    ItemSets::new(done_list)
}
