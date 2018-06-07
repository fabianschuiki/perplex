// Copyright (c) 2018 Fabian Schuiki

//! Item sets derived from a grammar.

use std::fmt;
use Pretty;
use grammar::{self, Grammar, NonterminalId, RuleId, TerminalId};

/// An item set.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemSet {
    /// The items in the set.
    pub(crate) items: Vec<Item>,
    /// Whether the set has been fully constructed.
    pub(crate) complete: bool,
}

impl ItemSet {
    /// Get the items in the set.
    pub fn items(&self) -> &[Item] {
        &self.items
    }

    /// Get a pretty printer for this item set.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a ItemSet> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (index, item) in self.item.items.iter().enumerate() {
            if index > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{} {}", index, item.pretty(self.ctx))?;
        }
        if self.item.items.is_empty() {
            write!(f, "<empty>")?;
        }
        Ok(())
    }
}

/// A single item.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    /// The rule of the item.
    pub(crate) rule: RuleId,
    /// The lookahead terminal.
    pub(crate) lookahead: TerminalId,
    /// The position of the marker within the rule.
    pub(crate) marker: usize,
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
