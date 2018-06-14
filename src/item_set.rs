// Copyright (c) 2018 Fabian Schuiki

//! Item sets derived from a grammar.

use std::fmt;
use std::collections::HashSet;

use bit_set::BitSet;

use Pretty;
use grammar::{self, Grammar, NonterminalId, RuleId, Symbol, TerminalId};
use first::FirstSets;

/// An item set.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemSet {
    /// The id of this item set.
    pub(crate) id: usize,
    /// The items in the set.
    pub(crate) items: Vec<Item>,
    /// The number of kernel items.
    pub(crate) kernel: usize,
    /// The actions to be taken for different symbols.
    pub(crate) actions: Vec<(Symbol, Action)>,
    /// An item-to-action mapping.
    pub(crate) item_actions: Vec<BitSet>,
}

/// A single item.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    /// The rule of the item.
    pub(crate) rule: RuleId,
    /// The lookahead terminal.
    pub(crate) lookahead: TerminalId,
    /// The position of the marker within the rule.
    pub(crate) marker: usize,
}

/// An action to be taken upon encountering a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    /// Shift the symbol and go to the given item set.
    Shift(usize),
    /// Reduce with the given rule.
    Reduce(RuleId),
}

impl ItemSet {
    /// Create a new item set.
    pub fn new(id: usize) -> ItemSet {
        ItemSet {
            id: id,
            items: Vec::new(),
            kernel: 0,
            actions: Vec::new(),
            item_actions: Vec::new(),
        }
    }

    /// Create a new item set with certain items.
    pub fn with_items(id: usize, items: Vec<Item>) -> ItemSet {
        let mut set = ItemSet::new(id);
        set.kernel = items.len();
        set.items = items;
        set
    }

    /// Get the items in the set.
    pub fn items(&self) -> &[Item] {
        &self.items
    }

    /// Get a pretty printer for this item set.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    /// Compute the closure of the item set.
    pub fn closure(&mut self, grammar: &Grammar, first_sets: &FirstSets) {
        compute_closure(self, grammar, first_sets)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a ItemSet> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "i{}:", self.item.id)?;
        for (index, item) in self.item.items.iter().enumerate() {
            if index > 0 {
                write!(f, "\n")?;
            }
            write!(f, "    {} {}", index, item.pretty(self.ctx))?;
            if index < self.item.kernel {
                write!(f, "*")?;
            }
            if self.item.item_actions.len() > index {
                for action_id in &self.item.item_actions[index] {
                    let (ref symbol, action) = self.item.actions[action_id];
                    write!(f, " ({}, {})", symbol.pretty(self.ctx), action)?;
                }
            }
        }
        if self.item.items.is_empty() {
            write!(f, "    <empty>")?;
        }
        Ok(())
    }
}

impl Item {
    /// Get the rule this item represents.
    pub fn rule(&self) -> RuleId {
        self.rule
    }

    /// Get the lookahead terminal of this item.
    pub fn lookahead(&self) -> TerminalId {
        self.lookahead
    }

    /// Get the position of the marker within the rule.
    pub fn marker(&self) -> usize {
        self.marker
    }

    /// Get a pretty printer for this item.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Item> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.item.rule == grammar::ACCEPT {
            write!(f, "[$accept ->")?;
            if self.item.marker == 0 {
                write!(f, " .")?;
            }
            write!(f, " {}", NonterminalId::from_usize(0).pretty(self.ctx))?;
            if self.item.marker == 1 {
                write!(f, " .")?;
            }
        } else {
            let rule = self.ctx.rule(self.item.rule);
            write!(f, "[{} ->", rule.name().pretty(self.ctx))?;
            let symbols = rule.symbols();
            for symbol in &symbols[0..self.item.marker] {
                write!(f, " {}", symbol.pretty(self.ctx))?;
            }
            write!(f, " .")?;
            for symbol in &symbols[self.item.marker..] {
                write!(f, " {}", symbol.pretty(self.ctx))?;
            }
        }
        write!(f, ", {}]", self.item.lookahead.pretty(self.ctx))?;
        Ok(())
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Action::Shift(id) => write!(f, "i{}", id),
            Action::Reduce(grammar::ACCEPT) => write!(f, "$accept"),
            Action::Reduce(id) => write!(f, "r{}", id.as_usize()),
        }
    }
}

/// Compute the closure of an item set.
fn compute_closure(item_set: &mut ItemSet, grammar: &Grammar, first_sets: &FirstSets) {
    let mut done: HashSet<Item> = item_set.items.iter().cloned().collect();
    let mut tail = 0;
    while item_set.items.len() > tail {
        let item = item_set.items[tail];
        tail += 1;
        if item.rule == grammar::ACCEPT {
            if item.marker == 0 {
                for &rule_id in grammar.rules_for_nonterminal(NonterminalId::from_usize(0)) {
                    let new_item = Item {
                        rule: rule_id,
                        lookahead: item.lookahead,
                        marker: 0,
                    };
                    if !done.contains(&new_item) {
                        done.insert(new_item);
                        item_set.items.push(new_item);
                    }
                }
            }
        } else {
            let symbols = grammar.rule(item.rule).symbols();
            if item.marker == symbols.len() {
                continue;
            }
            match symbols[item.marker] {
                Symbol::Terminal(_) => (),
                Symbol::Nonterminal(id) => {
                    // Compute the follow set for the nonterminal.
                    let (mut follow_set, epsilon) =
                        compute_follow_set(&symbols[item.marker + 1..], grammar, first_sets);
                    if epsilon {
                        follow_set.insert(item.lookahead.as_usize());
                    }

                    // Generate the new items.
                    for &rule_id in grammar.rules_for_nonterminal(id) {
                        for fs in &follow_set {
                            let new_item = Item {
                                rule: rule_id,
                                lookahead: TerminalId::from_usize(fs),
                                marker: 0,
                            };
                            if !done.contains(&new_item) {
                                done.insert(new_item);
                                item_set.items.push(new_item);
                            }
                        }
                    }
                }
                _ => unimplemented!(),
            }
        }
    }
}

/// Compute the follow set of a sequence of symbols.
fn compute_follow_set<'a, I>(
    symbols: I,
    grammar: &Grammar,
    first_sets: &FirstSets,
) -> (BitSet, bool)
where
    I: IntoIterator<Item = &'a Symbol>,
{
    let mut set = BitSet::with_capacity(grammar.terminal_id_bound());
    for symbol in symbols.into_iter() {
        match *symbol {
            Symbol::Terminal(id) => {
                set.insert(id.as_usize());
                return (set, false);
            }
            Symbol::Nonterminal(id) => for &rule_id in grammar.rules_for_nonterminal(id) {
                let fs = first_sets.get(rule_id).unwrap();
                set.union_with(&fs.symbols);
                if !fs.has_epsilon {
                    return (set, false);
                }
            },
            Symbol::Group(ref symbols) => {
                let (subset, epsilon) = compute_follow_set(symbols.iter(), grammar, first_sets);
                set.union_with(&subset);
                if !epsilon {
                    return (set, false);
                }
            }
            Symbol::Optional(ref symbol) => {
                set.union_with(
                    &compute_follow_set(Some(symbol.as_ref()).into_iter(), grammar, first_sets).0,
                );
            }
        }
    }
    (set, true)
}