// Copyright (c) 2018 Fabian Schuiki

//! Data structures representing a grammar.

use std;
use std::collections::HashMap;

/// A grammar.
#[derive(Debug)]
pub struct Grammar {
    rules: Vec<Rule>,
    nonterms: HashMap<String, NonterminalId>,
    terms: HashMap<String, TerminalId>,
}

/// A single rule within a grammar.
#[derive(Debug)]
pub struct Rule {
    name: NonterminalId,
    symbols: Vec<Symbol>,
}

/// A symbol of a production.
#[derive(Debug)]
pub enum Symbol {
    /// A terminal.
    Terminal(TerminalId),
    /// A nonterminal.
    Nonterminal(NonterminalId),
    /// A group of symbols.
    Group(Vec<Symbol>),
    /// An optional symbol.
    Optional(Box<Symbol>),
}

/// A unique nonterminal identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonterminalId(usize);

/// A unique terminal identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(usize);

/// An iterator over the rules of a grammar.
pub type RulesIter<'a> = std::slice::Iter<'a, Rule>;

impl Grammar {
    /// Create a new empty grammar.
    pub fn new() -> Grammar {
        Grammar {
            rules: Vec::new(),
            nonterms: HashMap::new(),
            terms: HashMap::new(),
        }
    }

    /// Add a nonterminal.
    pub fn add_nonterminal<S: Into<String>>(&mut self, name: S) -> NonterminalId {
        let next_id = NonterminalId(self.nonterms.len());
        *self.nonterms.entry(name.into()).or_insert(next_id)
    }

    /// Add a terminal.
    pub fn add_terminal<S: Into<String>>(&mut self, name: S) -> TerminalId {
        let next_id = TerminalId(self.terms.len());
        *self.terms.entry(name.into()).or_insert(next_id)
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// The rules in this grammar.
    pub fn rules(&self) -> RulesIter {
        self.rules.iter()
    }
}

impl Rule {
    /// Create a new empty rule.
    pub fn new(name: NonterminalId, symbols: Vec<Symbol>) -> Rule {
        Rule {
            name: name,
            symbols: symbols,
        }
    }

    /// The name of this rule.
    pub fn name(&self) -> NonterminalId {
        self.name
    }

    /// The symbols in this production.
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }
}

impl From<TerminalId> for Symbol {
    fn from(id: TerminalId) -> Symbol {
        Symbol::Terminal(id)
    }
}

impl From<NonterminalId> for Symbol {
    fn from(id: NonterminalId) -> Symbol {
        Symbol::Nonterminal(id)
    }
}
