// Copyright (c) 2018 Fabian Schuiki

//! Abstract Syntax Tree synthesis.
//!
//! This module implements a way of mapping an extended grammar to an Abstract
//! Syntax Tree automatically. Nodes of the tree are generated based on the
//! rules of the grammar, and the user may supply some of the nodes and
//! reduction functions themself.

use std::collections::HashMap;
use indexmap::IndexMap;

use ext::{Grammar, Nonterminal, NonterminalId, Sequence, SequenceId, Symbol, SymbolKind,
          TerminalId};

/// A synthesis context.
///
/// This struct is used to hold all intermediate values that are generated
/// during synthesis.
#[derive(Debug, Default)]
struct Context {
    nonterm_types: HashMap<NonterminalId, Type>,
    seq_types: HashMap<SequenceId, Type>,
    nodes: Vec<Node>,
    names: HashMap<String, usize>,
}

fn alloc_name<S: AsRef<str>>(ctx: &mut Context, name: S) -> String {
    let mut buffer = String::new();
    let mut capitalize = true;
    for c in name.as_ref().chars() {
        if c.is_alphabetic() {
            match capitalize {
                true => buffer.extend(c.to_uppercase()),
                false => buffer.extend(c.to_lowercase()),
            };
            capitalize = false;
        } else {
            capitalize = true;
        }
    }
    let count = {
        let v = ctx.names.entry(buffer.clone()).or_insert(0);
        *v += 1;
        *v
    };
    if count > 1 {
        buffer = format!("{}{}", buffer, count);
    }
    buffer
}

/// A type in the AST.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    /// An external type defined by the user.
    Extern(String),
    /// The type of a terminal.
    Terminal(TerminalId),
    /// The type of a nonterminal.
    Nonterminal(NonterminalId),
    /// The type of a sequence.
    Sequence(SequenceId),
    /// An optional type.
    Maybe(Box<Type>),
    /// A choice type.
    Choice(Vec<Type>),
    /// An array of a type.
    Array(Box<Type>),
    /// The AST node with the given index.
    Node(usize),
}

fn synth(grammar: &Grammar) {
    let mut ctx = Context::default();
    for nt in grammar.nonterminals() {
        map_nonterminal(&mut ctx, nt);
    }
    println!("{:#?}", ctx);

    for n in &ctx.nodes {
        println!("");
        codegen_node(&ctx, n);
    }
}

fn map_nonterminal(ctx: &mut Context, nt: &Nonterminal) -> Type {
    let seqs: Vec<_> = nt.rules()
        .map(|rule| map_sequence(ctx, &rule.rhs, &nt.name))
        .collect();
    let ty = if seqs.len() == 1 {
        seqs.into_iter().next().unwrap()
    } else {
        let ty = Type::Node(ctx.nodes.len());
        let node = Node {
            name: alloc_name(ctx, &nt.name),
            kind: NodeKind::Enum(seqs),
        };
        ctx.nodes.push(node);
        ty
    };
    ctx.nonterm_types.insert(nt.id, ty.clone());
    ty
}

fn map_sequence(ctx: &mut Context, seq: &Sequence, name_stem: &str) -> Type {
    let mut fields = IndexMap::<String, Type>::new();
    for symbol in &seq.symbols {
        let ty = map_symbol(ctx, symbol, name_stem);
        if let Some(ref name) = symbol.name {
            if fields.insert(name.clone(), ty).is_some() {
                panic!("symbol name `{}` used multiple times", name);
            }
        }
    }

    let ty = Type::Node(ctx.nodes.len());
    let node = Node {
        name: alloc_name(ctx, name_stem),
        kind: NodeKind::Union(fields.into_iter().collect()),
    };
    ctx.nodes.push(node);
    ctx.seq_types.insert(seq.id, ty);
    Type::Sequence(seq.id)
}

fn map_symbol(ctx: &mut Context, symbol: &Symbol, name_stem: &str) -> Type {
    match symbol.kind {
        SymbolKind::Terminal(t) => Type::Terminal(t),
        SymbolKind::Nonterminal(t) => Type::Nonterminal(t),
        SymbolKind::Group(ref g) => map_sequence(ctx, g, name_stem),
        SymbolKind::Maybe(ref sym) => Type::Maybe(Box::new(map_symbol(ctx, &*sym, name_stem))),
        SymbolKind::Choice(ref syms) => {
            Type::Choice(syms.iter().map(|s| map_symbol(ctx, s, name_stem)).collect())
        }
        SymbolKind::Repeat(ref rep, ref sep, _) => {
            let t = map_symbol(ctx, rep, name_stem);
            if let Some(ref sep) = *sep {
                map_symbol(ctx, sep, name_stem);
            }
            Type::Array(Box::new(t))
        }
    }
}

/// A synthesized AST.
///
/// This struct holds all the information necessary to synthesis AST nodes and
/// reduction functions for an extended grammar.
pub struct AstSynth {}

impl AstSynth {
    /// Synthesize an AST for a grammar.
    pub fn with_grammar(grammar: &Grammar) -> AstSynth {
        synth(grammar);
        AstSynth {}
    }
}

/// A node in the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node {
    /// The name of the node.
    pub name: String,
    /// The kind of the node.
    pub kind: NodeKind,
}

/// A node kind.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// A union node in the AST.
    ///
    /// Union nodes create one single type of node for all rules associated with a
    /// nonterminal. Fields that are only present in some of the rules are given an
    /// optional type.
    Union(Vec<(String, Type)>),

    /// An enumerated node in the AST.
    ///
    /// Enumerated nodes have separate variants for each node type returned by the
    /// rules associated with a nonterminal.
    Enum(Vec<Type>),
}

#[allow(dead_code)]
fn codegen_node(ctx: &Context, node: &Node) {
    match node.kind {
        NodeKind::Union(ref fields) => {
            println!("pub struct {} {{", node.name);
            for &(ref name, ref ty) in fields {
                println!("    pub {}: {},", name, codegen_type(ctx, ty));
            }
            println!("}}");
        }
        NodeKind::Enum(ref variants) => {
            println!("pub enum {} {{", node.name);
            for (i, v) in variants.iter().enumerate() {
                println!("    V{}({}),", i, codegen_type(ctx, v));
            }
            println!("}}");
        }
    }
}

fn codegen_type(ctx: &Context, ty: &Type) -> String {
    match *ty {
        Type::Extern(ref e) => e.clone(),
        Type::Terminal(_id) => "()".into(),
        Type::Nonterminal(id) => codegen_type(ctx, &ctx.nonterm_types[&id]),
        Type::Sequence(id) => codegen_type(ctx, &ctx.seq_types[&id]),
        Type::Maybe(ref ty) => format!("Option<{}>", codegen_type(ctx, &*ty)),
        Type::Choice(ref _choices) => panic!("type codegen for choices not yet supported"),
        Type::Array(ref ty) => format!("Vec<{}>", codegen_type(ctx, &*ty)),
        Type::Node(index) => ctx.nodes[index].name.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn singleton_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        let _r = g.make_rule(nt_a, |s| {
            s.terminal(t_a).name("name").terminal(t_b).name("type")
        });
        synth(&mut g);
    }

    #[test]
    fn nested_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let nt_b = g.make_nonterminal("item").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.nonterminal(nt_b).name("item").terminal(t_a).name("name")
        });
        g.make_rule(nt_b, |s| {
            s.terminal(t_a).name("name").terminal(t_b).name("type")
        });
        synth(&mut g);
    }

    #[test]
    fn multiple_rules() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        g.make_rule(nt_a, |s| s.terminal(t_a).name("item"));
        g.make_rule(nt_a, |s| s.terminal(t_b).name("type"));
        synth(&mut g);
    }

    #[test]
    fn optional_symbols() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.terminal(t_a)
                .maybe()
                .name("type")
                .terminal(t_b)
                .name("name")
        });
        synth(&mut g);
    }

    #[test]
    fn repeated_symbols() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        let t_b = g.make_terminal("b").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.terminal(t_a)
                .repeat(false)
                .name("type")
                .terminal(t_b)
                .name("name")
        });
        synth(&mut g);
    }

    #[test]
    fn recursive_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.nonterminal(nt_a).name("base").terminal(t_a).name("next")
        });
        g.make_rule(nt_a, |s| s.terminal(t_a).name("first"));
        synth(&mut g);
    }
}
