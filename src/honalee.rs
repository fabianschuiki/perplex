// Copyright (c) 2018 Fabian Schuiki

//! Implementation of the Honalee Algorithm for item set generation.

use std::collections::{BTreeSet, HashMap};

use bit_set::BitSet;

use grammar::{self, Grammar, NonterminalId, RuleId, Symbol};
use first::FirstSets;
use item_set::{Action, Item, ItemSet, ItemSetId, ItemSets, KernelCores};

/// Construct the item sets for a grammar.
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
        ItemSetId::from_usize(0),
        vec![
            Item {
                rule: grammar::ACCEPT,
                lookahead: grammar::END,
                marker: 0,
                action: None,
            },
        ],
    );
    debug!("running honalee algorithm on {}", initial.pretty(grammar));
    todo_list.push(initial);

    // The main loop.
    let mut next_id = 1; // ID for new sets
    while !todo_list.is_empty() || !inc_list.is_empty() {
        // Phase 1: Calculate the closure over all todo item sets and either
        // merge them with an existing set, or add them to the incomplete list
        // for transition generation.
        'todo_sets: for mut item_set in todo_list.drain(..) {
            item_set.closure(grammar, &first_sets);
            // trace!("phase 1: {}", item_set.pretty(grammar));

            // Generate the reduce actions of this item set.
            let mut reduce_lookup: HashMap<Symbol, RuleId> = HashMap::new();
            let mut todo_has_conflict = false;
            for i in 0..item_set.items.len() {
                let item = &mut item_set.items[i];
                let rule = if item.rule == grammar::ACCEPT {
                    if item.marker == 1 {
                        grammar::ACCEPT
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
                item.action = Some((item.lookahead.into(), Action::Reduce(rule)));
                todo_has_conflict |= reduce_lookup.insert(item.lookahead.into(), rule).is_some();
            }

            // Consider all done item sets with the same kernel item cores as
            // potential candidates to merge this item set into.
            if !todo_has_conflict {
                trace!(
                    "- considering merge of {:?} with cores {:?}",
                    item_set.id,
                    item_set.kernel_item_cores()
                );
                for &index in merge_hint
                    .get(&item_set.kernel_item_cores())
                    .iter()
                    .flat_map(|i| i.iter())
                {
                    trace!("- maybe i{} can be merged with i{}", item_set.id, index);
                    // trace!("reduce_lookup: {:?}", reduce_lookup);
                    // trace!("merge_set.actions: {:?}", done_list[index].actions);
                    // trace!("{}", done_list[index].pretty(grammar));

                    // Make sure that merging would not produce any conflicts.
                    let no_conflicts = done_list[index].actions().all(|&(symbol, merge_rule)| {
                        match reduce_lookup.get(&symbol) {
                            Some(&rule) if Action::Reduce(rule) != merge_rule => false,
                            _ => true,
                        }
                    });
                    if no_conflicts {
                        debug!("merging i{} into i{}", item_set.id, index);
                        trace!("{}", item_set.pretty(grammar));
                        trace!("{}", done_list[index].pretty(grammar));
                        if let Some(come_from) = come_from {
                            done_list[come_from].replace_actions(
                                Action::Shift(item_set.id),
                                Action::Shift(ItemSetId::from_usize(index)),
                            );
                        }
                        done_list[index].merge(item_set);
                        trace!("merged: {}", done_list[index].pretty(grammar));
                        inc_list.insert(index);
                        continue 'todo_sets;
                    }
                }
            }

            // Add the item set to the done list and mark it as incomplete.
            let id = ItemSetId::from_usize(done_list.len());
            if id != item_set.id {
                debug!("renaming {:?} to {:?}", item_set.id, id);
                if let Some(come_from) = come_from {
                    done_list[come_from]
                        .replace_actions(Action::Shift(item_set.id), Action::Shift(id));
                }
                item_set.id = id;
            }
            debug!("closed {}", item_set.pretty(grammar));
            trace!(
                "- {:?} cores {:?}",
                item_set.id,
                item_set.kernel_item_cores()
            );
            merge_hint
                .entry(item_set.kernel_item_cores())
                .or_insert_with(|| Vec::new())
                .push(id.as_usize());
            done_list.push(item_set);
            inc_list.insert(id.as_usize());
        }

        // Phase 2: For one incomplete item set, compute the transitions and
        // spawn subsequent item sets, which will then be processed in phase 1
        // of the next iteration.
        if let Some(&index) = inc_list.iter().next() {
            inc_list.remove(&index);
            let mut item_set = &mut done_list[index];
            debug!("adding shifts for {:?}", item_set.id);

            let root_symbol = Symbol::Nonterminal(NonterminalId::from_usize(0));
            let mut treated = BitSet::with_capacity(item_set.items.len());
            for i in 0..item_set.items.len() {
                let item = item_set.items[i];
                if treated.contains(i) {
                    continue;
                }
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

                let mut new_set = ItemSet::new(ItemSetId::from_usize(next_id));
                next_id += 1;

                let mut any_updated = false;
                for n in i..item_set.items.len() {
                    let item2 = &mut item_set.items[n];
                    if treated.contains(n) {
                        continue;
                    }
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
                        action: None,
                    });
                    if item2.action.is_none() {
                        item2.action = Some((symbol.clone(), Action::Shift(new_set.id)));
                        any_updated = true;
                    }
                    treated.insert(n);
                }
                if !any_updated {
                    continue;
                }

                new_set.kernel = new_set.items.len();

                trace!("- shift {} -> {:?}", symbol.pretty(grammar), new_set.id);
                // trace!("{}", new_set.pretty(grammar));
                todo_list.push(new_set);
            }

            come_from = Some(index);
        } else {
            come_from = None;
        }
    }

    // Compress the item sets.
    let mut sets = ItemSets::new(done_list);
    sets.compress();
    sets
}
