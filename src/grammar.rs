// Copyright (c) 2018 Fabian Schuiki

//! Data structures representing a grammar.

use std;
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};

/// A grammar.
#[derive(Debug)]
pub struct Grammar {
    next_id: usize,
    rules: BTreeMap<RuleId, Rule>,
    names: HashMap<String, RuleId>,
}

/// A single rule within a grammar.
#[derive(Debug)]
pub struct Rule {
    id: RuleId,
    name: String,
    next_id: usize,
    prods: BTreeMap<ProductionId, Production>,
}

/// A single production within a rule.
#[derive(Debug)]
pub struct Production {
    id: ProductionId,
    symbols: Vec<Symbol>,
}

/// A symbol of a production.
#[derive(Debug)]
pub enum Symbol {
    /// A terminal.
    Terminal(TerminalId),
    /// A nonterminal.
    Nonterminal(RuleId),
    /// A group of symbols.
    Group(Vec<Symbol>),
    /// An optional symbol.
    Optional(Box<Symbol>),
}

/// A unique rule identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleId(usize);

/// A unique production identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProductionId(RuleId, usize);

/// A unique terminal identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(usize);

/// An iterator over the rules of a grammar.
pub type RulesIter<'a> = std::collections::btree_map::Iter<'a, RuleId, Rule>;

/// An iterator over the productions of a rule.
pub type ProductionsIter<'a> = std::collections::btree_map::Iter<'a, ProductionId, Production>;

impl Grammar {
    /// Create a new empty grammar.
    pub fn new() -> Grammar {
        Grammar {
            next_id: 0,
            rules: BTreeMap::new(),
            names: HashMap::new(),
        }
    }

    /// Allocate a new rule id.
    pub fn alloc_id(&mut self) -> RuleId {
        let i = self.next_id;
        self.next_id += 1;
        RuleId(i)
    }

    /// Allocate a new rule name.
    ///
    /// If a rule with this name has already been registered, returns that id.
    /// Otherwise a new id is allocated and returned.
    pub fn alloc_name(&mut self, name: Cow<str>) -> RuleId {
        if let Some(&id) = self.names.get(name.as_ref()) {
            return id;
        }
        let id = self.alloc_id();
        self.names.insert(name.into_owned(), id);
        id
    }

    /// Add a rule to the grammar.
    ///
    /// Panics if the rule's id and name have not previously been allocated.
    pub fn add_rule(&mut self, rule: Rule) {
        if !self.names.contains_key(&rule.name) {
            panic!("rule name \"{}\" not allocated", rule.name);
        }
        if let Some(existing) = self.rules.insert(rule.id, rule) {
            panic!("rule \"{}\" overwritten", existing.name);
        }
    }

    /// The rules in this grammar.
    pub fn rules(&self) -> RulesIter {
        self.rules.iter()
    }
}

impl Rule {
    /// Create a new empty rule.
    pub fn new(id: RuleId, name: String) -> Rule {
        Rule {
            id: id,
            name: name,
            next_id: 0,
            prods: BTreeMap::new(),
        }
    }

    /// Allocate a new production id.
    pub fn alloc_id(&mut self) -> ProductionId {
        let i = self.next_id;
        self.next_id += 1;
        ProductionId(self.id, i)
    }

    /// Add a production to the rule.
    ///
    /// Panics if a production with the same id has already been added.
    pub fn add_production(&mut self, prod: Production) {
        if let Some(existing) = self.prods.insert(prod.id, prod) {
            panic!("production {:?} overwritten", existing);
        }
    }

    /// The id of this rule.
    pub fn id(&self) -> RuleId {
        self.id
    }

    /// The name of this rule.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The productions in this rule.
    pub fn productions(&self) -> ProductionsIter {
        self.prods.iter()
    }
}

impl Production {
    /// Create a new production.
    pub fn new(id: ProductionId, symbols: Vec<Symbol>) -> Production {
        Production {
            id: id,
            symbols: symbols,
        }
    }

    /// The id of this production.
    pub fn id(&self) -> ProductionId {
        self.id
    }

    /// The symbols in this production.
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }
}
