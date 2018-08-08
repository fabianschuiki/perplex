// Copyright (c) 2018 Fabian Schuiki

//! Item sets derived from a grammar.

use std;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::collections::{BTreeSet, HashSet};
use std::iter::{once, repeat};
use std::mem::replace;

use bit_set::BitSet;

use Pretty;
use grammar::{self, Grammar, NonterminalId, RuleId, Symbol, TerminalId};
use first::FirstSets;
use honalee;

/// All item sets of a grammar.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemSets(Vec<ItemSet>);

/// An item set.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemSet {
    /// The id of this item set.
    pub(crate) id: ItemSetId,
    /// The items in the set.
    pub(crate) items: Vec<Item>,
    /// The number of kernel items.
    pub(crate) kernel: usize,
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
    /// The action for this item.
    pub(crate) action: Option<(Symbol, Action)>,
}

/// An action to be taken upon encountering a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    /// Shift the symbol and go to the given item set.
    Shift(ItemSetId),
    /// Reduce with the given rule.
    Reduce(RuleId),
}

/// A unique item set identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemSetId(usize);

impl ItemSets {
    /// Create a new list of item sets.
    pub fn new(sets: Vec<ItemSet>) -> ItemSets {
        ItemSets(sets)
    }

    /// Compute the item sets for a grammar.
    pub fn compute(grammar: &Grammar) -> ItemSets {
        honalee::construct_item_sets(grammar)
    }

    /// Get the item sets in the grammar.
    pub fn all(&self) -> &[ItemSet] {
        &self.0
    }

    /// Get a pretty printer for this item set.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    /// Compress all item sets.
    pub fn compress(&mut self) {
        for is in &mut self.0 {
            is.compress();
        }
    }
}

impl Index<ItemSetId> for ItemSets {
    type Output = ItemSet;

    fn index(&self, index: ItemSetId) -> &ItemSet {
        &self.0[index.as_usize()]
    }
}

impl IndexMut<ItemSetId> for ItemSets {
    fn index_mut(&mut self, index: ItemSetId) -> &mut ItemSet {
        &mut self.0[index.as_usize()]
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a ItemSets> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (is, sep) in self.item.0.iter().zip(once("").chain(repeat("\n"))) {
            write!(f, "{}{}", sep, is.pretty(self.ctx))?;
        }
        Ok(())
    }
}

impl ItemSet {
    /// Create a new item set.
    pub fn new(id: ItemSetId) -> ItemSet {
        ItemSet {
            id: id,
            items: Vec::new(),
            kernel: 0,
        }
    }

    /// Create a new item set with certain items.
    pub fn with_items(id: ItemSetId, items: Vec<Item>) -> ItemSet {
        let mut set = ItemSet::new(id);
        set.kernel = items.len();
        set.items = items;
        set
    }

    /// Get the id of the item set.
    pub fn id(&self) -> ItemSetId {
        self.id
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

    /// Compute the kernel item cores.
    ///
    /// The returned struct can be used to compare two item sets for equality in
    /// their kernel item cores.
    pub fn kernel_item_cores(&self) -> KernelCores {
        let set: BTreeSet<_> = self.items[0..self.kernel]
            .iter()
            .map(|item| (item.rule, item.marker))
            .collect();
        KernelCores(set.into_iter().collect())
    }

    /// Replace all occurrences of one action with another.
    pub fn replace_actions(&mut self, from: Action, to: Action) {
        for &mut (_, ref mut action) in self.actions_mut() {
            if *action == from {
                *action = to;
            }
        }
    }

    /// Get an iterator over the actions in the set.
    pub fn actions(&self) -> Actions {
        Actions(self.items.iter())
    }

    /// Get a mutable iterator over the actions in the set.
    pub fn actions_mut(&mut self) -> ActionsMut {
        ActionsMut(self.items.iter_mut())
    }

    /// Merge another item set into this.
    pub fn merge(&mut self, other: ItemSet) {
        let mut present: HashSet<Item> = self.items
            .iter()
            .cloned()
            .map(|mut item| {
                item.action = None;
                item
            })
            .collect();
        for (index, item) in other.items.into_iter().enumerate() {
            let mut item_na = item;
            item_na.action = None;
            if present.insert(item_na) {
                if index < other.kernel {
                    self.items.insert(self.kernel, item);
                    self.kernel += 1;
                } else {
                    self.items.push(item);
                }
            }
        }
    }

    /// Compress the item set.
    ///
    /// This will remove redundant items and replace their lookahead token with
    /// a `#`.
    pub fn compress(&mut self) {
        let mut present = HashSet::<Item>::new();
        let items = replace(&mut self.items, Vec::new());
        for (index, mut item) in items.into_iter().enumerate() {
            if index < self.kernel {
                self.items.push(item);
            } else {
                if item.is_shift() {
                    item.lookahead = grammar::NIL;
                }
                if present.insert(item) {
                    self.items.push(item);
                }
            }
        }
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
            if let Some((symbol, action)) = item.action {
                write!(f, " ({}, {})", symbol.pretty(self.ctx), action)?;
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

    /// Get the action associated with this item.
    pub fn action(&self) -> Option<(Symbol, Action)> {
        self.action
    }

    /// Change the action of this item.
    pub fn set_action(&mut self, action: Option<(Symbol, Action)>) {
        self.action = action
    }

    /// Get a pretty printer for this item.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    /// Whether this item has a shift action.
    pub fn is_shift(&self) -> bool {
        self.action
            .map(|(_, action)| action.is_shift())
            .unwrap_or(false)
    }

    /// Whether this item has a reduce action.
    pub fn is_reduce(&self) -> bool {
        self.action
            .map(|(_, action)| action.is_reduce())
            .unwrap_or(false)
    }

    /// Whether this item has an action.
    pub fn has_action(&self) -> bool {
        self.action.is_some()
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

impl Action {
    /// Whether this is a shift action.
    pub fn is_shift(&self) -> bool {
        match *self {
            Action::Shift(_) => true,
            _ => false,
        }
    }

    /// Whether this is a reduce action.
    pub fn is_reduce(&self) -> bool {
        match *self {
            Action::Reduce(_) => true,
            _ => false,
        }
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

impl ItemSetId {
    /// Create an item set id from a usize.
    pub fn from_usize(id: usize) -> ItemSetId {
        ItemSetId(id)
    }

    /// Obtain the id as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}

impl fmt::Display for ItemSetId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
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
                        action: None,
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
                                action: None,
                            };
                            if !done.contains(&new_item) {
                                done.insert(new_item);
                                item_set.items.push(new_item);
                            }
                        }
                    }
                }
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
            Symbol::Nonterminal(id) => {
                let fs = first_sets.get(id);
                set.union_with(&fs.symbols);
                if !fs.has_epsilon {
                    return (set, false);
                }
            }
        }
    }
    (set, true)
}

/// A list of kernel item cores.
///
/// The entries are sorted such that two item sets with the same kernel item
/// cores but different order will produce the same KernelCores struct.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KernelCores(Vec<(RuleId, usize)>);

/// An iterator over the actions of an item set.
pub struct Actions<'a>(std::slice::Iter<'a, Item>);

/// A mutable iterator over the actions of an item set.
pub struct ActionsMut<'a>(std::slice::IterMut<'a, Item>);

impl<'a> Iterator for Actions<'a> {
    type Item = &'a (Symbol, Action);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.0.next() {
            if let Some(ref action) = item.action {
                return Some(action);
            }
        }
        None
    }
}

impl<'a> Iterator for ActionsMut<'a> {
    type Item = &'a mut (Symbol, Action);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.0.next() {
            if let Some(ref mut action) = item.action {
                return Some(action);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use grammar::{Grammar, Rule, END};
    use item_set::ItemSets;

    /// Ensure that left recursive rules that have a nonterminal after the
    /// recursion produce all the required lookaheads. This is a regression test
    /// to catch the following:
    ///
    ///     A : A B | B ;
    ///     B : c ;
    ///
    /// Would generated the following items in the first set:
    ///
    ///     [A -> . A B, $end]
    ///     [A -> . B, $end]
    ///     [B -> . c, $end]
    ///
    /// But would be missing the following:
    ///
    ///     [A -> . A B, c]
    ///     [A -> . B, c]
    ///     [B -> . c, c]
    #[test]
    fn left_recursion() {
        let mut g = Grammar::new();
        let a = g.add_nonterminal("A");
        let b = g.add_nonterminal("B");
        let c = g.add_terminal("c");
        g.add_rule(Rule::new(a, vec![a.into(), b.into()]));
        g.add_rule(Rule::new(a, vec![b.into()]));
        g.add_rule(Rule::new(b, vec![c.into()]));
        let is = ItemSets::compute(&g);
        println!("{}", is.pretty(&g));
        let la_exp = vec![END, c];
        for is in is.all()[2..].iter() {
            let la_act: Vec<_> = is.items().iter().map(|i| i.lookahead).collect();
            assert_eq!(la_act, la_exp);
        }
    }
}
