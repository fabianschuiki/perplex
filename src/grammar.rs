// Copyright (c) 2018 Fabian Schuiki

//! Data structures representing a grammar.

use std;
use std::fmt;
use std::collections::HashMap;
use Pretty;

/// A grammar.
#[derive(Debug, Clone)]
pub struct Grammar {
    rules: Vec<Rule>,
    nonterms: HashMap<String, NonterminalId>,
    terms: HashMap<String, TerminalId>,
    nonterm_names: Vec<String>,
    nonterm_rules: Vec<Vec<RuleId>>,
    term_names: Vec<String>,
}

/// A single rule within a grammar.
#[derive(Debug, Clone)]
pub struct Rule {
    name: NonterminalId,
    symbols: Vec<Symbol>,
}

/// A symbol of a production.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// A terminal.
    Terminal(TerminalId),
    /// A nonterminal.
    Nonterminal(NonterminalId),
}

/// A unique nonterminal identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonterminalId(usize);

/// A unique terminal identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(usize);

/// A unique rule identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleId(usize);

/// The start rule `$accept -> S`.
pub const ACCEPT: RuleId = RuleId(std::usize::MAX);

/// The special end of input terminal `$end`.
pub const END: TerminalId = TerminalId(0);

/// An iterator over the rules of a grammar.
pub type RulesIter<'a> = std::slice::Iter<'a, Rule>;

/// an iterator over the rule IDs of a grammar.
pub type RuleIdsIter<'a> = std::slice::Iter<'a, RuleId>;

impl Grammar {
    /// Create a new empty grammar.
    pub fn new() -> Grammar {
        Grammar {
            rules: Vec::new(),
            nonterms: HashMap::new(),
            terms: HashMap::new(),
            nonterm_names: Vec::new(),
            nonterm_rules: Vec::new(),
            term_names: Vec::new(),
        }
    }

    /// Add a nonterminal.
    pub fn add_nonterminal<S: Into<String>>(&mut self, name: S) -> NonterminalId {
        let name = name.into();
        let next_id = NonterminalId(self.nonterm_names.len());
        if let Some(&id) = self.nonterms.get(&name) {
            id
        } else {
            self.nonterms.insert(name.clone(), next_id);
            self.nonterm_names.push(name);
            self.nonterm_rules.push(Vec::new());
            next_id
        }
    }

    /// Add a terminal.
    pub fn add_terminal<S: Into<String>>(&mut self, name: S) -> TerminalId {
        let name = name.into();
        let next_id = TerminalId(self.term_names.len() + 1);
        if let Some(&id) = self.terms.get(&name) {
            id
        } else {
            self.terms.insert(name.clone(), next_id);
            self.term_names.push(name);
            next_id
        }
    }

    /// Get the name of a nonterminal.
    pub fn nonterminal_name(&self, id: NonterminalId) -> &str {
        &self.nonterm_names[id.as_usize()]
    }

    /// Get the name of a terminal.
    pub fn terminal_name(&self, id: TerminalId) -> &str {
        if id == END {
            "$end"
        } else {
            &self.term_names[id.as_usize() - 1]
        }
    }

    /// The upper bound on nonterminal IDs.
    ///
    /// Basically returns the largest nonterminal ID + 1. Can be used as
    /// capacity for containers that will hold terminals.
    pub fn nonterminal_id_bound(&self) -> usize {
        self.nonterm_names.len()
    }

    /// The upper bound on terminal IDs.
    ///
    /// Basically returns the largest terminal ID + 1. Can be used as capacity
    /// for containers that will hold terminals.
    pub fn terminal_id_bound(&self) -> usize {
        self.term_names.len() + 1
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, rule: Rule) {
        self.nonterm_rules[rule.name().as_usize()].push(RuleId::from_usize(self.rules.len()));
        self.rules.push(rule);
    }

    /// The rules in this grammar.
    pub fn rules(&self) -> RulesIter {
        self.rules.iter()
    }

    /// The rules for a specific nonterminal in the grammar.
    pub fn rules_for_nonterminal(&self, id: NonterminalId) -> RuleIdsIter {
        self.nonterm_rules[id.as_usize()].iter()
    }

    /// Access a single rule of this grammar.
    ///
    /// Panics if the id is the builtin `ACCEPT` nonterminal, which represents
    /// the virtual root rule.
    pub fn rule(&self, id: RuleId) -> &Rule {
        if id == ACCEPT {
            panic!("rule() called for builtin ACCEPT rule");
        }
        &self.rules[id.as_usize()]
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

impl Symbol {
    /// Get a pretty printer for this symbol.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
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

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.item {
            Symbol::Terminal(id) => write!(f, "{}", id.pretty(self.ctx)),
            Symbol::Nonterminal(id) => write!(f, "{}", id.pretty(self.ctx)),
        }
    }
}

impl NonterminalId {
    /// Create a nonterminal id from a usize.
    pub fn from_usize(id: usize) -> NonterminalId {
        NonterminalId(id)
    }

    /// Obtain the id as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }

    /// Get a pretty printer for this nonterminal.
    pub fn pretty(self, grammar: &Grammar) -> Pretty<&Grammar, Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, NonterminalId> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ctx.nonterminal_name(self.item))
    }
}

impl TerminalId {
    /// Create a terminal id from a usize.
    pub fn from_usize(id: usize) -> TerminalId {
        TerminalId(id)
    }

    /// Obtain the id as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }

    /// Get a pretty printer for this terminal.
    pub fn pretty(self, grammar: &Grammar) -> Pretty<&Grammar, Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, TerminalId> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ctx.terminal_name(self.item))
    }
}

impl RuleId {
    /// Create a rule id from a usize.
    pub fn from_usize(id: usize) -> RuleId {
        RuleId(id)
    }

    /// Obtain the id as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}
