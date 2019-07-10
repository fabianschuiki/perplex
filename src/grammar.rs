// Copyright (c) 2018 Fabian Schuiki

//! Data structures representing a grammar.

use std;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Index, IndexMut};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    /// A terminal.
    Terminal(TerminalId),
    /// A nonterminal.
    Nonterminal(NonterminalId),
}

/// A unique nonterminal identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonterminalId(usize);

/// A unique terminal identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(usize);

/// A unique rule identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleId(usize);

/// The start rule `$accept -> S`.
pub const ACCEPT: RuleId = RuleId(0);

/// The start nonterminal `$accept`.
pub const ACCEPT_NONTERMINAL: NonterminalId = NonterminalId(0);

/// The special end of input terminal `$end`.
pub const END: TerminalId = TerminalId(0);

/// The special *don't care*/nil terminal `#`.
pub const NIL: TerminalId = TerminalId(1);

/// The lowest ID a user-defined terminal can have. This is the first index
/// after all the builtin terminals.
const LOWEST_TERMINAL_ID: usize = 2;

/// An iterator over the rules of a grammar.
pub type RulesIter<'a> = std::slice::Iter<'a, Rule>;

/// an iterator over the rule IDs of a grammar.
pub type RuleIdsIter<'a> = std::slice::Iter<'a, RuleId>;

impl Grammar {
    /// Create a new empty grammar.
    pub fn new() -> Grammar {
        let mut g = Grammar {
            rules: Vec::new(),
            nonterms: HashMap::new(),
            terms: HashMap::new(),
            nonterm_names: Vec::new(),
            nonterm_rules: Vec::new(),
            term_names: Vec::new(),
        };
        g.add_nonterminal("$accept");
        g.add_rule(Rule::new(
            ACCEPT_NONTERMINAL,
            vec![Symbol::Nonterminal(NonterminalId(1))],
        ));
        g
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
        let next_id = TerminalId(self.term_names.len() + LOWEST_TERMINAL_ID);
        if let Some(&id) = self.terms.get(&name) {
            id
        } else {
            self.terms.insert(name.clone(), next_id);
            self.term_names.push(name);
            next_id
        }
    }

    /// Lookup a nonterminal by name.
    ///
    /// Panics if the nonterminal does not exist.
    pub fn nonterminal<S: AsRef<str>>(&self, name: S) -> NonterminalId {
        let name = name.as_ref();
        match self.get_nonterminal(name) {
            Some(x) => x,
            None => panic!("nonterminal `{}` not found", name),
        }
    }

    /// Lookup a terminal by name.
    ///
    /// Panics if the terminal does not exist.
    pub fn terminal<S: AsRef<str>>(&self, name: S) -> TerminalId {
        let name = name.as_ref();
        match self.get_terminal(name) {
            Some(x) => x,
            None => panic!("terminal `{}` not found", name),
        }
    }

    /// Find a nonterminal.
    pub fn get_nonterminal<S: AsRef<str>>(&self, name: S) -> Option<NonterminalId> {
        self.nonterms.get(name.as_ref()).cloned()
    }

    /// Find a terminal.
    pub fn get_terminal<S: AsRef<str>>(&self, name: S) -> Option<TerminalId> {
        self.terms.get(name.as_ref()).cloned()
    }

    /// Get all nonterminals in the grammar.
    pub fn nonterminals(&self) -> impl Iterator<Item = NonterminalId> {
        (0..self.nonterminal_id_bound()).map(NonterminalId::from_usize)
    }

    /// Get all terminals in the grammar.
    pub fn terminals(&self) -> impl Iterator<Item = TerminalId> {
        (LOWEST_TERMINAL_ID..self.terminal_id_bound()).map(TerminalId::from_usize)
    }

    /// Get the name of a nonterminal.
    pub fn nonterminal_name(&self, id: NonterminalId) -> &str {
        &self.nonterm_names[id.as_usize()]
    }

    /// Get the name of a terminal.
    pub fn terminal_name(&self, id: TerminalId) -> &str {
        match id {
            END => "$end",
            NIL => "#",
            _ => &self.term_names[id.as_usize() - LOWEST_TERMINAL_ID],
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
        self.term_names.len() + LOWEST_TERMINAL_ID
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, rule: Rule) -> RuleId {
        let id = RuleId::from_usize(self.rules.len());
        self.nonterm_rules[rule.name().as_usize()].push(id);
        self.rules.push(rule);
        id
    }

    /// The rules in this grammar.
    pub fn rules(&self) -> RulesIter {
        self.rules.iter()
    }

    /// The rules for a specific nonterminal in the grammar.
    pub fn rules_for_nonterminal(&self, id: NonterminalId) -> RuleIdsIter {
        self.nonterm_rules[id.as_usize()].iter()
    }
}

impl Index<RuleId> for Grammar {
    type Output = Rule;

    fn index(&self, index: RuleId) -> &Rule {
        &self.rules[index.as_usize()]
    }
}

impl IndexMut<RuleId> for Grammar {
    fn index_mut(&mut self, index: RuleId) -> &mut Rule {
        &mut self.rules[index.as_usize()]
    }
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.rules();
        match iter.next() {
            Some(r) => write!(f, "{}", r.pretty(self))?,
            None => return write!(f, "<empty grammar>"),
        };
        for r in iter {
            write!(f, "\n{}", r.pretty(self))?;
        }
        Ok(())
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

    /// Get a pretty printer for this rule.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Rule> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} ->", self.item.name().pretty(self.ctx))?;
        for symbol in &self.item.symbols {
            write!(f, " {}", symbol.pretty(self.ctx))?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Symbol {
    /// Get a pretty printer for this symbol.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    /// Check whether this symbol is a terminal.
    pub fn is_terminal(&self) -> bool {
        match *self {
            Symbol::Terminal(..) => true,
            _ => false,
        }
    }

    /// Check whether this symbol is a nonterminal.
    pub fn is_nonterminal(&self) -> bool {
        match *self {
            Symbol::Nonterminal(..) => true,
            _ => false,
        }
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

impl fmt::Display for NonterminalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "nt{}", self.0)
    }
}

impl fmt::Debug for NonterminalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
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

impl fmt::Display for TerminalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl fmt::Debug for TerminalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
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

impl fmt::Display for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl fmt::Debug for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
