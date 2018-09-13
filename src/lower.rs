// Copyright (c) 2018 Fabian Schuiki

//! Lowering of extended grammars to basic grammars.

use std::collections::HashMap;

use backend;
use ext;
use grammar;

/// Context information for lowering a grammar.
pub struct Context<'a> {
    source: &'a ext::Grammar,
    grammar: grammar::Grammar,
    backend: backend::Backend,
}

impl<'a> Context<'a> {
    /// Create a new lowering context.
    pub fn new(source: &'a ext::Grammar) -> Context<'a> {
        Context {
            source: source,
            grammar: grammar::Grammar::new(),
            backend: backend::Backend::new(),
        }
    }

    /// Perform the lowering.
    ///
    /// Consumes the context.
    pub fn lower(mut self) -> (grammar::Grammar, backend::Backend) {
        lower_grammar(&mut self);
        (self.grammar, self.backend)
    }
}

/// Lower the extended grammar to a regular grammar and accompanying backend
/// description.
fn lower_grammar(ctx: &mut Context) {
    // Declare the terminals.
    let term_map: HashMap<ext::TerminalId, grammar::TerminalId> = ctx
        .source
        .terminals()
        .enumerate()
        .map(|(i, t)| {
            let id = ctx.grammar.add_terminal(t.name.clone());
            if let Some(pat) = t.match_pattern.clone() {
                ctx.backend.add_terminal(id, pat);
            }
            (ext::TerminalId(i), id)
        })
        .collect();

    // Specify the special end-of-input pattern.
    if let Some(pat) = ctx.source.end_pattern().cloned() {
        ctx.backend.add_terminal(grammar::END, pat);
    }

    // Declare the nonterminals.
    let nonterm_map: HashMap<ext::NonterminalId, grammar::NonterminalId> = ctx
        .source
        .nonterminals()
        .enumerate()
        .map(|(i, nt)| {
            let id = ctx.grammar.add_nonterminal(nt.name.clone());
            if let Some(ext) = nt.extern_type.clone() {
                ctx.backend.add_nonterminal(id, ext);
            }
            (ext::NonterminalId(i), id)
        })
        .collect();

    // Add the rules to the grammar.
    for nt in ctx.source.nonterminals() {
        for rule in nt.rules() {
            let rule_id =
                lower_sequence(ctx, nonterm_map[&nt.id], &rule.rhs, &term_map, &nonterm_map);
            if let Some(rf) = rule.rhs.extern_reducer.clone() {
                ctx.backend.add_reduction_function(rule_id, rf);
            }
        }
    }
    // for d in &desc.rules {
    //     let id = rule_map[&d.name];
    // }
}

fn lower_sequence(
    ctx: &mut Context,
    id: grammar::NonterminalId,
    seq: &ext::Sequence,
    term_map: &HashMap<ext::TerminalId, grammar::TerminalId>,
    nonterm_map: &HashMap<ext::NonterminalId, grammar::NonterminalId>,
) -> grammar::RuleId {
    let mut flattened = Vec::new();
    for symbol in &seq.symbols {
        lower_symbol(ctx, id, symbol, term_map, nonterm_map, &mut flattened);
    }
    let rule_id = ctx.grammar.add_rule(grammar::Rule::new(id, flattened));
    rule_id
}

fn lower_symbol(
    ctx: &mut Context,
    id: grammar::NonterminalId,
    symbol: &ext::Symbol,
    term_map: &HashMap<ext::TerminalId, grammar::TerminalId>,
    nonterm_map: &HashMap<ext::NonterminalId, grammar::NonterminalId>,
    into: &mut Vec<grammar::Symbol>,
) {
    match symbol.kind {
        ext::SymbolKind::Terminal(id) => into.push(term_map[&id].into()),
        ext::SymbolKind::Nonterminal(id) => into.push(nonterm_map[&id].into()),
        ext::SymbolKind::Group(ref seq) => {
            let subid = pick_subrule_name(id, &mut ctx.grammar);
            into.push(subid.into());
            let mut symbols = Vec::new();
            for symbol in &seq.symbols {
                lower_symbol(ctx, subid, symbol, term_map, nonterm_map, &mut symbols);
            }
            ctx.grammar.add_rule(grammar::Rule::new(subid, symbols));
        }
        ext::SymbolKind::Maybe(ref s) => {
            let subid = pick_subrule_name(id, &mut ctx.grammar);
            into.push(subid.into());
            let mut symbols = Vec::new();
            lower_symbol(ctx, subid, s, term_map, nonterm_map, &mut symbols);
            ctx.grammar.add_rule(grammar::Rule::new(subid, vec![]));
            ctx.grammar.add_rule(grammar::Rule::new(subid, symbols));
        }
        ext::SymbolKind::Repeat(ref seq, ref sep, allow_empty) => {
            let subid = pick_subrule_name(id, &mut ctx.grammar);
            into.push(subid.into());
            let subid = match allow_empty {
                true => {
                    let subid2 = pick_subrule_name(subid, &mut ctx.grammar);
                    ctx.grammar.add_rule(grammar::Rule::new(subid, vec![]));
                    ctx.grammar
                        .add_rule(grammar::Rule::new(subid, vec![subid2.into()]));
                    subid2
                }
                false => subid,
            };
            let mut symbols_seq = Vec::new();
            let mut symbols_sep = Vec::new();
            lower_symbol(ctx, subid, seq, term_map, nonterm_map, &mut symbols_seq);
            if let Some(ref sep) = *sep {
                lower_symbol(ctx, subid, sep, term_map, nonterm_map, &mut symbols_sep);
            }
            ctx.grammar
                .add_rule(grammar::Rule::new(subid, symbols_seq.clone()));
            let mut symbols = vec![grammar::Symbol::from(subid)];
            symbols.extend(symbols_sep);
            symbols.extend(symbols_seq);
            ctx.grammar.add_rule(grammar::Rule::new(subid, symbols));
        }
        ext::SymbolKind::Choice(ref choices) => {
            for choice in choices {
                let subid = pick_subrule_name(id, &mut ctx.grammar);
                into.push(subid.into());
                let mut symbols = Vec::new();
                lower_symbol(ctx, subid, choice, term_map, nonterm_map, &mut symbols);
                ctx.grammar.add_rule(grammar::Rule::new(subid, symbols));
            }
        }
    }
}

fn pick_subrule_name(
    id: grammar::NonterminalId,
    grammar: &mut grammar::Grammar,
) -> grammar::NonterminalId {
    let mut name = String::from(grammar.nonterminal_name(id));
    while grammar.get_nonterminal(&name).is_some() {
        name.push('\'');
    }
    grammar.add_nonterminal(name)
}
