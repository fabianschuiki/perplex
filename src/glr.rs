// Copyright (c) 2018 Fabian Schuiki

//! Analysis of the GLR/LR(k) behaviour of a grammar.

use indexmap::{IndexMap, IndexSet};

use grammar::{Grammar, Symbol};
use item_set::{Action, ItemSetId, ItemSets};

/// An analysis of conflicts and their resolvability via GLR/LR(k).
#[derive(Debug)]
pub struct GlrAnalysis {}

impl GlrAnalysis {
    /// Analyze a grammar.
    pub fn compute(_grammar: &Grammar, item_sets: &ItemSets) -> GlrAnalysis {
        let conflicts = find_conflicts(item_sets);
        println!("Conflicts: {:#?}", conflicts);
        GlrAnalysis {}
    }
}

/// Find the conflict points in the item sets of a grammar where the same symbol
/// would lead to two different actions.
pub fn find_conflicts(item_sets: &ItemSets) -> Vec<Conflict> {
    let mut conflicts = Vec::new();
    for is in item_sets.all().iter() {
        // No conflicts possible if there are less than two items.
        if is.items().len() < 2 {
            continue;
        }

        // Create a Symbol -> {Action} mapping.
        let mut table: IndexMap<Symbol, IndexSet<Action>> = IndexMap::new();
        for &(symbol, action) in is.actions() {
            table
                .entry(symbol)
                .or_insert_with(|| IndexSet::new())
                .insert(action);
        }

        // Filter out all items with more than one possible action.
        conflicts.extend(table.into_iter().filter_map(|(symbol, actions)| {
            if actions.len() > 1 {
                Some(Conflict {
                    item_set: is.id(),
                    symbol: symbol,
                    actions: actions.into_iter().collect(),
                })
            } else {
                None
            }
        }));
    }
    conflicts
}

/// A conflict between multiple actions triggered by the same symbol.
#[derive(Debug)]
pub struct Conflict {
    item_set: ItemSetId,
    symbol: Symbol,
    actions: Vec<Action>,
}
