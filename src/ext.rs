// Copyright (c) 2018 Fabian Schuiki

//! Data structures representing an extended grammar.

use std::collections::HashMap;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

use backend;
use grammar;
use lower;

use Pretty;

/// An extended grammar.
#[derive(Debug, Clone)]
pub struct Grammar {
    next_sequence_id: SequenceId,
    next_symbol_id: SymbolId,
    terms: Vec<Terminal>,
    term_names: HashMap<String, TerminalId>,
    nonterms: Vec<Nonterminal>,
    nonterm_names: HashMap<String, NonterminalId>,
    end_pattern: Option<String>,
}

impl Grammar {
    /// Create a new grammar.
    pub fn new() -> Grammar {
        Grammar {
            next_sequence_id: SequenceId(0),
            next_symbol_id: SymbolId(0),
            terms: Vec::new(),
            term_names: HashMap::new(),
            nonterms: Vec::new(),
            nonterm_names: HashMap::new(),
            end_pattern: None,
        }
    }

    /// Add a terminal to the grammar.
    pub fn add_terminal(&mut self, mut term: Terminal) -> TerminalId {
        match self.term_names.get(&term.name) {
            Some(&id) => id,
            None => {
                let id = TerminalId(self.terms.len());
                term.id = id;
                self.term_names.insert(term.name.clone(), id);
                self.terms.push(term);
                id
            }
        }
    }

    /// Build a terminal.
    pub fn make_terminal<S: Into<String>>(&mut self, name: S) -> TerminalBuilder {
        TerminalBuilder {
            term: Terminal::new(name),
        }
    }

    /// Add a nonterminal to the grammar.
    pub fn add_nonterminal(&mut self, mut nonterm: Nonterminal) -> NonterminalId {
        match self.nonterm_names.get(&nonterm.name) {
            Some(&id) => id,
            None => {
                let id = NonterminalId(self.nonterms.len());
                nonterm.id = id;
                self.nonterm_names.insert(nonterm.name.clone(), id);
                self.nonterms.push(nonterm);
                id
            }
        }
    }

    /// Build a nonterminal.
    pub fn make_nonterminal<S: Into<String>>(&mut self, name: S) -> NonterminalBuilder {
        NonterminalBuilder {
            nonterm: Nonterminal::new(name),
        }
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, lhs: NonterminalId, mut rhs: Sequence) -> RuleId {
        rhs.assign_id(&mut self.next_sequence_id, &mut self.next_symbol_id);
        let rules = &mut self[lhs].rules;
        let id = RuleId(lhs, rules.len());
        rules.push(Rule { lhs: lhs, rhs: rhs });
        id
    }

    /// Build a rule.
    pub fn make_rule<F>(&mut self, lhs: NonterminalId, f: F) -> RuleId
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.add_rule(lhs, f(SequenceBuilder::new()).build())
    }

    /// Set the match pattern of the special end-of-input terminal.
    pub fn set_end_pattern(&mut self, pat: String) {
        self.end_pattern = Some(pat);
    }

    /// Get the match pattern of the special end-of-input terminal.
    pub fn end_pattern(&self) -> Option<&String> {
        self.end_pattern.as_ref()
    }

    /// Get an iterator over the terminals.
    pub fn terminals(&self) -> Iter<Terminal> {
        self.terms.iter()
    }

    /// Get a mutable iterator over the terminals.
    pub fn terminals_mut(&mut self) -> IterMut<Terminal> {
        self.terms.iter_mut()
    }

    /// Get an iterator over the nonterminals.
    pub fn nonterminals(&self) -> Iter<Nonterminal> {
        self.nonterms.iter()
    }

    /// Get a mutable iterator over the nonterminals.
    pub fn nonterminals_mut(&mut self) -> IterMut<Nonterminal> {
        self.nonterms.iter_mut()
    }

    /// Lower the extended grammar to a regular grammar and accompanying backend
    /// description.
    pub fn lower(&self) -> (grammar::Grammar, backend::Backend) {
        lower::Context::new(self).lower()
    }
}

impl Index<TerminalId> for Grammar {
    type Output = Terminal;
    fn index(&self, idx: TerminalId) -> &Terminal {
        &self.terms[idx.0]
    }
}

impl IndexMut<TerminalId> for Grammar {
    fn index_mut(&mut self, idx: TerminalId) -> &mut Terminal {
        &mut self.terms[idx.0]
    }
}

impl Index<NonterminalId> for Grammar {
    type Output = Nonterminal;
    fn index(&self, idx: NonterminalId) -> &Nonterminal {
        &self.nonterms[idx.0]
    }
}

impl IndexMut<NonterminalId> for Grammar {
    fn index_mut(&mut self, idx: NonterminalId) -> &mut Nonterminal {
        &mut self.nonterms[idx.0]
    }
}

impl Index<RuleId> for Grammar {
    type Output = Rule;
    fn index(&self, idx: RuleId) -> &Rule {
        &self[idx.0][idx]
    }
}

impl IndexMut<RuleId> for Grammar {
    fn index_mut(&mut self, idx: RuleId) -> &mut Rule {
        &mut self[idx.0][idx]
    }
}

/// A terminal within a grammar.
///
/// Terminals describe the tokens consumed when parsing a grammar. They are the
/// minimal non-divisible fragment of information that is being dealt around.
#[derive(Debug, Clone)]
pub struct Terminal {
    /// The id of this terminal.
    pub id: TerminalId,
    /// The name of the terminal.
    pub name: String,
    /// The human-readable name of the terminal.
    pub nice_name: Option<String>,
    /// The type of the data this terminal carries, if any.
    pub data_type: Option<String>,
    /// The match pattern of this terminal.
    pub match_pattern: Option<String>,
}

impl Terminal {
    /// Create a new terminal.
    pub fn new<S: Into<String>>(name: S) -> Terminal {
        Terminal {
            id: ORPHAN_TERMINAL,
            name: name.into(),
            nice_name: None,
            data_type: None,
            match_pattern: None,
        }
    }

    /// Get a human-readable name of the terminal.
    ///
    /// If no such name has been explicitly specified, the regular name of the
    /// terminal is returned.
    pub fn nice_name(&self) -> &str {
        self.nice_name.as_ref().unwrap_or(&self.name)
    }
}

/// A builder for terminals.
pub struct TerminalBuilder {
    term: Terminal,
}

impl TerminalBuilder {
    /// Specify the human-readable name of the terminal.
    pub fn nice_name<S: Into<String>>(mut self, v: S) -> Self {
        self.term.nice_name = Some(v.into());
        self
    }

    /// Specify whether the terminal contains data.
    pub fn data_type<S: Into<String>>(mut self, v: S) -> Self {
        self.term.data_type = Some(v.into());
        self
    }

    /// Specify the match pattern of the terminal.
    pub fn match_pattern<S: Into<String>>(mut self, v: S) -> Self {
        self.term.match_pattern = Some(v.into());
        self
    }

    /// Build the terminal.
    pub fn build(self, grammar: &mut Grammar) -> TerminalId {
        grammar.add_terminal(self.term)
    }
}

/// A unique terminal identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TerminalId(pub usize);

impl TerminalId {
    /// Get a pretty printer for this terminal id.
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
        write!(f, "{}", self.ctx[self.item].name)
    }
}

/// The id of a terminal that has not been added to a grammar.
pub const ORPHAN_TERMINAL: TerminalId = TerminalId(::std::usize::MAX);

/// A nonterminal within a grammar.
///
/// Nonterminals describe the production rules for parsing a grammar. They
/// consist of multiple sequences of symbols which can be reduced to one datum.
#[derive(Debug, Clone)]
pub struct Nonterminal {
    /// The id of this nonterminal.
    pub id: NonterminalId,
    /// The name of the nonterminal.
    pub name: String,
    /// The human-readable name of the nonterminal.
    pub nice_name: Option<String>,
    /// The externally defined type this nonterminal yields.
    pub extern_type: Option<String>,
    /// The name the synthesized AST node for this nonterminal should have.
    pub synth_name: Option<String>,
    /// The rules of this nonterminal.
    rules: Vec<Rule>,
}

impl Nonterminal {
    /// Create a new nonterminal.
    pub fn new<S: Into<String>>(name: S) -> Nonterminal {
        Nonterminal {
            id: ORPHAN_NONTERMINAL,
            name: name.into(),
            nice_name: None,
            extern_type: None,
            synth_name: None,
            rules: Vec::new(),
        }
    }

    /// Get a human-readable name of the nonterminal.
    ///
    /// If no such name has been explicitly specified, the regular name of the
    /// nonterminal is returned.
    pub fn nice_name(&self) -> &str {
        self.nice_name.as_ref().unwrap_or(&self.name)
    }

    /// Get an iterator over the rules.
    pub fn rules(&self) -> Iter<Rule> {
        self.rules.iter()
    }

    /// Get a mutable iterator over the rules.
    pub fn rules_mut(&mut self) -> IterMut<Rule> {
        self.rules.iter_mut()
    }
}

impl Index<RuleId> for Nonterminal {
    type Output = Rule;
    fn index(&self, idx: RuleId) -> &Rule {
        &self.rules[idx.1]
    }
}

impl IndexMut<RuleId> for Nonterminal {
    fn index_mut(&mut self, idx: RuleId) -> &mut Rule {
        &mut self.rules[idx.1]
    }
}

/// A builder for nonterminals.
pub struct NonterminalBuilder {
    nonterm: Nonterminal,
}

impl NonterminalBuilder {
    /// Specify the human-readable name of the nonterminal.
    pub fn nice_name(mut self, v: String) -> Self {
        self.nonterm.nice_name = Some(v);
        self
    }

    /// Specify the externally defined type of the nonterminal.
    pub fn external_type(mut self, v: String) -> Self {
        self.nonterm.extern_type = Some(v);
        self
    }

    /// Specify the synthesized AST node name.
    pub fn synth_name(mut self, v: String) -> Self {
        self.nonterm.synth_name = Some(v);
        self
    }

    /// Build the nonterminal.
    pub fn build(self, grammar: &mut Grammar) -> NonterminalId {
        grammar.add_nonterminal(self.nonterm)
    }
}

/// A unique nonterminal identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonterminalId(pub usize);

impl NonterminalId {
    /// Get a pretty printer for this nonterminal id.
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
        write!(f, "{}", self.ctx[self.item].name)
    }
}

/// The id of a nonterminal that has not been added to a grammar.
pub const ORPHAN_NONTERMINAL: NonterminalId = NonterminalId(::std::usize::MAX);

/// A rule within a grammar.
#[derive(Debug, Clone)]
pub struct Rule {
    /// The nonterminal this rule reduces to.
    pub lhs: NonterminalId,
    /// The sequence of symbols that this rule represents.
    pub rhs: Sequence,
}

impl Rule {
    /// Get a pretty printer for this rule.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Rule> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.item.lhs.pretty(self.ctx),
            self.item.rhs.pretty(self.ctx)
        )
    }
}

/// A unique rule identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleId(pub NonterminalId, pub usize);

impl fmt::Display for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}.{}", (self.0).0, self.1)
    }
}

impl fmt::Debug for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// A sequence of symbols.
#[derive(Debug, Clone)]
pub struct Sequence {
    /// The unique id of this sequence.
    pub id: SequenceId,
    /// The symbols of this sequence.
    pub symbols: Vec<Symbol>,
    /// The externally defined reduction function that reduces this sequence.
    pub extern_reducer: Option<String>,
    /// The name the synthesized AST node for this nonterminal should have.
    pub synth_name: Option<String>,
}

impl Sequence {
    /// Create a new sequence.
    pub fn new(symbols: Vec<Symbol>) -> Sequence {
        Sequence {
            id: ORPHAN_SEQUENCE,
            symbols: symbols,
            extern_reducer: None,
            synth_name: None,
        }
    }

    /// Get a pretty printer for this sequence.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    fn assign_id(&mut self, next_seq_id: &mut SequenceId, next_sym_id: &mut SymbolId) {
        self.id = *next_seq_id;
        next_seq_id.0 += 1;
        for sym in &mut self.symbols {
            sym.assign_id(next_seq_id, next_sym_id);
        }
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Sequence> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.item.symbols.iter();
        if let Some(symbol) = iter.next() {
            write!(f, "{}", symbol.pretty(self.ctx))?;
            for symbol in iter {
                write!(f, " {}", symbol.pretty(self.ctx))?;
            }
            Ok(())
        } else {
            write!(f, "ε")
        }
    }
}

/// A unique sequence identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SequenceId(pub usize);

impl fmt::Display for SequenceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

impl fmt::Debug for SequenceId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// The id of a nonterminal that has not been added to a grammar.
pub const ORPHAN_SEQUENCE: SequenceId = SequenceId(::std::usize::MAX);

/// A sequence builder.
#[derive(Debug, Clone)]
pub struct SequenceBuilder {
    seq: Sequence,
}

impl SequenceBuilder {
    /// Create a new sequence builder.
    pub fn new() -> SequenceBuilder {
        SequenceBuilder {
            seq: Sequence::new(vec![]),
        }
    }

    /// Add a terminal.
    pub fn terminal(mut self, id: TerminalId) -> Self {
        self.seq.symbols.push(Symbol::terminal(id));
        self
    }

    /// Add a nonterminal.
    pub fn nonterminal(mut self, id: NonterminalId) -> Self {
        self.seq.symbols.push(Symbol::nonterminal(id));
        self
    }

    /// Add a group of symbols.
    pub fn group<F>(mut self, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.seq
            .symbols
            .push(Symbol::group(f(SequenceBuilder::new()).build()));
        self
    }

    /// Make the last symbol optional.
    pub fn maybe(mut self) -> Self {
        let s = self
            .seq
            .symbols
            .pop()
            .expect("no symbol that can be made optional");
        self.seq.symbols.push(Symbol::maybe(s));
        self
    }

    /// Make the last n symbols a choice.
    pub fn choice(mut self, n: usize) -> Self {
        let pos = self.seq.symbols.len() - n;
        let s = self.seq.symbols.split_off(pos);
        self.seq.symbols.push(Symbol::choice(s));
        self
    }

    /// Repeat the last symbol.
    pub fn repeat(mut self, allow_empty: bool) -> Self {
        let s = self
            .seq
            .symbols
            .pop()
            .expect("no symbol that can be repeated");
        self.seq.symbols.push(Symbol::repeat(s, None, allow_empty));
        self
    }

    /// Repeat the last symbol separated by another symbol.
    pub fn repeat_separated(mut self, separator: Symbol, allow_empty: bool) -> Self {
        let s = self
            .seq
            .symbols
            .pop()
            .expect("no symbol that can be repeated");
        self.seq
            .symbols
            .push(Symbol::repeat(s, Some(separator), allow_empty));
        self
    }

    /// Specify the last symbol's name.
    pub fn name<S: Into<String>>(mut self, name: S) -> Self {
        self.seq.symbols.last_mut().unwrap().name = Some(name.into());
        self
    }

    /// Mark the last symbol as to be ignored.
    pub fn ignore(mut self) -> Self {
        self.seq.symbols.last_mut().unwrap().ignore = true;
        self
    }

    /// Specify the sequence's externally defined reduction function.
    pub fn extern_reducer(mut self, v: String) -> Self {
        self.seq.extern_reducer = Some(v);
        self
    }

    /// Specify the synthesized AST node name.
    pub fn synth_name(mut self, v: String) -> Self {
        self.seq.synth_name = Some(v);
        self
    }

    /// Build the sequence.
    pub fn build(self) -> Sequence {
        self.seq
    }
}

/// A symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The unique id of this symbol.
    pub id: SymbolId,
    /// The symbol kind. Contains the actual data.
    pub kind: SymbolKind,
    /// The name of the symbol.
    pub name: Option<String>,
    /// Whether the symbol should be ignored during reductions.
    pub ignore: bool,
}

impl Symbol {
    /// Create a new symbol.
    pub fn new(kind: SymbolKind) -> Symbol {
        Symbol {
            id: ORPHAN_SYMBOL,
            kind: kind,
            name: None,
            ignore: false,
        }
    }

    /// Create a terminal symbol.
    pub fn terminal(id: TerminalId) -> Symbol {
        Symbol::new(SymbolKind::Terminal(id))
    }

    /// Create a nonterminal symbol.
    pub fn nonterminal(id: NonterminalId) -> Symbol {
        Symbol::new(SymbolKind::Nonterminal(id))
    }

    /// Create a group.
    pub fn group(sequence: Sequence) -> Symbol {
        Symbol::new(SymbolKind::Group(sequence))
    }

    /// Create an optional symbol.
    pub fn maybe(symbol: Symbol) -> Symbol {
        Symbol::new(SymbolKind::Maybe(Box::new(symbol)))
    }

    /// Create a choice symbol.
    pub fn choice<I: IntoIterator<Item = Symbol>>(choices: I) -> Symbol {
        Symbol::new(SymbolKind::Choice(choices.into_iter().collect()))
    }

    /// Create a repeated symbol.
    pub fn repeat(repeated: Symbol, separator: Option<Symbol>, allow_empty: bool) -> Symbol {
        Symbol::new(SymbolKind::Repeat(
            Box::new(repeated),
            separator.map(Box::new),
            allow_empty,
        ))
    }

    /// Get a pretty printer for this symbol.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }

    fn assign_id(&mut self, next_seq_id: &mut SequenceId, next_sym_id: &mut SymbolId) {
        self.id = *next_sym_id;
        next_sym_id.0 += 1;
        match self.kind {
            SymbolKind::Terminal(..) => (),
            SymbolKind::Nonterminal(..) => (),
            SymbolKind::Group(ref mut seq) => seq.assign_id(next_seq_id, next_sym_id),
            SymbolKind::Maybe(ref mut sym) => sym.assign_id(next_seq_id, next_sym_id),
            SymbolKind::Choice(ref mut syms) => for sym in syms {
                sym.assign_id(next_seq_id, next_sym_id)
            },
            SymbolKind::Repeat(ref mut repeated, ref mut separator, _) => {
                repeated.assign_id(next_seq_id, next_sym_id);
                if let Some(ref mut separator) = *separator {
                    separator.assign_id(next_seq_id, next_sym_id);
                }
            }
        }
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.item.kind.pretty(self.ctx))
    }
}

/// A symbol kind.
#[derive(Debug, Clone)]
pub enum SymbolKind {
    /// A terminal. For example `a`.
    Terminal(TerminalId),
    /// A nonterminal. For example `A`.
    Nonterminal(NonterminalId),
    /// A group of symbols. For example `(a b)`.
    Group(Sequence),
    /// An optional symbol. For example `a?`.
    Maybe(Box<Symbol>),
    /// A choice between multiple symbols. For example `a|b|c`.
    Choice(Vec<Symbol>),
    /// A repetition of a symbol, optionally separated by a symbol. The tuple is
    /// of the form `(repeated, separator, allow_empty)`.
    Repeat(Box<Symbol>, Option<Box<Symbol>>, bool),
}

impl SymbolKind {
    /// Get a pretty printer for this symbol kind.
    pub fn pretty<'a>(&'a self, grammar: &'a Grammar) -> Pretty<&'a Grammar, &'a Self> {
        Pretty::new(grammar, self)
    }
}

impl<'a> fmt::Display for Pretty<&'a Grammar, &'a SymbolKind> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.item {
            SymbolKind::Terminal(id) => write!(f, "{}", id.pretty(self.ctx)),
            SymbolKind::Nonterminal(id) => write!(f, "{}", id.pretty(self.ctx)),
            SymbolKind::Group(ref seq) => write!(f, "({})", seq.pretty(self.ctx)),
            SymbolKind::Maybe(ref sym) => write!(f, "{}?", sym.pretty(self.ctx)),
            SymbolKind::Choice(ref choices) => {
                let mut iter = choices.iter();
                if let Some(symbol) = iter.next() {
                    write!(f, "({}", symbol.pretty(self.ctx))?;
                    for symbol in iter {
                        write!(f, " | {}", symbol.pretty(self.ctx))?;
                    }
                    write!(f, ")")
                } else {
                    write!(f, "(nil)")
                }
            }
            SymbolKind::Repeat(ref repeated, ref separator, allow_empty) => {
                write!(f, "{}", repeated.pretty(self.ctx))?;
                if let &Some(ref separator) = separator {
                    write!(f, "[{}]", separator.pretty(self.ctx))?;
                }
                write!(
                    f,
                    "{}",
                    match allow_empty {
                        true => '*',
                        false => '+',
                    }
                )
            }
        }
    }
}

/// A unique symbol identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(pub usize);

impl fmt::Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "y{}", self.0)
    }
}

impl fmt::Debug for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// The id of a symbol that has not been added to a grammar.
pub const ORPHAN_SYMBOL: SymbolId = SymbolId(::std::usize::MAX);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sequence_printing() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("A").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        let t_c = g.make_terminal("c").build(&mut g);

        assert_eq!(format!("{}", Symbol::terminal(t_a).pretty(&g)), "a");
        assert_eq!(format!("{}", Symbol::nonterminal(nt_a).pretty(&g)), "A");
        assert_eq!(
            format!(
                "{}",
                Symbol::group(Sequence::new(vec![
                    Symbol::terminal(t_b),
                    Symbol::terminal(t_c),
                ])).pretty(&g)
            ),
            "(b c)"
        );
        assert_eq!(
            format!("{}", Symbol::maybe(Symbol::terminal(t_a)).pretty(&g)),
            "a?"
        );
        assert_eq!(
            format!(
                "{}",
                Symbol::choice(vec![
                    Symbol::terminal(t_a),
                    Symbol::terminal(t_b),
                    Symbol::terminal(t_c),
                ]).pretty(&g)
            ),
            "(a | b | c)"
        );
        assert_eq!(
            format!(
                "{}",
                Symbol::repeat(Symbol::terminal(t_a), None, false).pretty(&g)
            ),
            "a+"
        );
        assert_eq!(
            format!(
                "{}",
                Symbol::repeat(Symbol::terminal(t_a), Some(Symbol::terminal(t_b)), false)
                    .pretty(&g)
            ),
            "a[b]+"
        );
        assert_eq!(
            format!(
                "{}",
                Symbol::repeat(Symbol::terminal(t_a), None, true).pretty(&g)
            ),
            "a*"
        );
        assert_eq!(
            format!(
                "{}",
                Symbol::repeat(Symbol::terminal(t_a), Some(Symbol::terminal(t_b)), true).pretty(&g)
            ),
            "a[b]*"
        );
    }

    fn test_sequence_builder<F, S>(s: S, g: &Grammar, f: F)
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
        S: fmt::Display,
    {
        assert_eq!(
            format!("{}", f(SequenceBuilder::new()).build().pretty(g)),
            format!("{}", s)
        );
    }

    #[test]
    fn sequence_builder() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("A").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        let t_c = g.make_terminal("c").build(&mut g);

        test_sequence_builder("a b c", &g, |s| s.terminal(t_a).terminal(t_b).terminal(t_c));
        test_sequence_builder("a (A b) c", &g, |s| {
            s.terminal(t_a)
                .group(|s| s.nonterminal(nt_a).terminal(t_b))
                .terminal(t_c)
        });
        test_sequence_builder("a (A | c) b", &g, |s| {
            s.terminal(t_a)
                .nonterminal(nt_a)
                .terminal(t_c)
                .choice(2)
                .terminal(t_b)
        });
        test_sequence_builder("a b? c", &g, |s| {
            s.terminal(t_a).terminal(t_b).maybe().terminal(t_c)
        });
        test_sequence_builder("a b+ c*", &g, |s| {
            s.terminal(t_a)
                .terminal(t_b)
                .repeat(false)
                .terminal(t_c)
                .repeat(true)
        });
        test_sequence_builder("a b[a]+ c[a]*", &g, |s| {
            s.terminal(t_a)
                .terminal(t_b)
                .repeat_separated(Symbol::terminal(t_a), false)
                .terminal(t_c)
                .repeat_separated(Symbol::terminal(t_a), true)
        });
    }

    #[test]
    fn grammar_building() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("A").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        let r0 = g.make_rule(nt_a, |s| s.nonterminal(nt_a).terminal(t_b));
        let r1 = g.make_rule(nt_a, |s| s.terminal(t_b));
        assert_eq!(format!("{}", g[r0].pretty(&g)), "A -> A b");
        assert_eq!(format!("{}", g[r1].pretty(&g)), "A -> b");
    }
}
