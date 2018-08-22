// Copyright (c) 2018 Fabian Schuiki

//! Abstract Syntax Tree synthesis.
//!
//! This module implements a way of mapping an extended grammar to an Abstract
//! Syntax Tree automatically. Nodes of the tree are generated based on the
//! rules of the grammar, and the user may supply some of the nodes and
//! reduction functions themself.

use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

use ext::{
    Grammar, Nonterminal, NonterminalId, Sequence, SequenceId, Symbol, SymbolKind, TerminalId,
};

/// A synthesis context.
///
/// This struct is used to hold all intermediate values that are generated
/// during synthesis.
#[derive(Debug)]
struct Context<'a> {
    grammar: &'a Grammar,
    nonterm_types: HashMap<NonterminalId, Type>,
    seq_types: HashMap<SequenceId, Type>,
    sym_types: HashMap<SequenceId, Vec<Type>>,
    nodes: Vec<Node>,
    names: HashMap<String, usize>,
    reduce_fns: HashMap<SequenceId, Vec<usize>>,
}

impl<'a> Context<'a> {
    fn new(grammar: &'a Grammar) -> Context<'a> {
        Context {
            grammar: grammar,
            nonterm_types: HashMap::new(),
            seq_types: HashMap::new(),
            sym_types: HashMap::new(),
            nodes: Vec::new(),
            names: HashMap::new(),
            reduce_fns: HashMap::new(),
        }
    }
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
    let mut ctx = Context::new(grammar);
    for nt in grammar.nonterminals() {
        map_nonterminal(&mut ctx, nt);
    }
    // println!("{:#?}", ctx);

    for n in &ctx.nodes {
        println!("");
        codegen_node(&ctx, grammar, n);
    }

    for (&seqid, f) in &ctx.reduce_fns {
        println!("");
        codegen_reduce_fn(&ctx, seqid, f);
    }
}

fn map_nonterminal(ctx: &mut Context, nt: &Nonterminal) -> Type {
    trace!("mapping nonterminal {}", nt.name);
    let seqs: Vec<_> = nt
        .rules()
        .enumerate()
        .map(|(i, rule)| map_sequence(ctx, &rule.rhs, &format!("{}_variant_{}", nt.name, i + 1)))
        .collect();
    // let ty = if seqs.len() == 1 {
    //     seqs.into_iter().next().unwrap()
    // } else {
    let ty = Type::Node(ctx.nodes.len());
    let node = Node {
        name: alloc_name(ctx, &nt.name),
        kind: NodeKind::Enum(seqs),
    };
    ctx.nodes.push(node);
    // ty
    // };
    ctx.nonterm_types.insert(nt.id, ty.clone());
    ty
}

fn map_sequence(ctx: &mut Context, seq: &Sequence, name_stem: &str) -> Type {
    trace!("mapping sequence {}", seq.pretty(ctx.grammar));

    // Check if any symbol in the sequence is named. If not, we just gather up
    // the value-producing symbols as a tuple.
    let any_names = seq.symbols.iter().any(|s| s.name.is_some());
    if !any_names {
        map_sequence_tuple(ctx, seq, name_stem)
    } else {
        map_sequence_struct(ctx, seq, name_stem)
    }
}

fn map_sequence_tuple(ctx: &mut Context, seq: &Sequence, name_stem: &str) -> Type {
    let fields: Vec<_> = seq
        .symbols
        .iter()
        .map(|s| map_symbol(ctx, s, name_stem))
        .collect();
    ctx.sym_types.insert(seq.id, fields.clone());
    let reduce_fn = (0..fields.len()).collect();
    ctx.reduce_fns.insert(seq.id, reduce_fn);
    let ty = Type::Node(ctx.nodes.len());
    let node = Node {
        name: alloc_name(ctx, name_stem),
        kind: NodeKind::Tuple(fields),
    };
    ctx.nodes.push(node);
    ctx.seq_types.insert(seq.id, ty.clone());
    ty
}

fn map_sequence_struct(ctx: &mut Context, seq: &Sequence, name_stem: &str) -> Type {
    let mut fields = IndexMap::<String, Type>::new();
    let mut sym_tys = Vec::new();
    for symbol in &seq.symbols {
        let ty = map_symbol(ctx, symbol, name_stem);
        sym_tys.push(ty.clone());
        if let Some(ref name) = symbol.name {
            if fields.insert(name.clone(), ty).is_some() {
                panic!("symbol name `{}` used multiple times", name);
            }
        } else if let Type::Maybe(ty) = ty {
            match *ty {
                Type::Node(index) => match ctx.nodes[index].kind {
                    NodeKind::Union(ref fs) => fields.extend(
                        fs.iter()
                            .cloned()
                            .map(|(name, ty)| (name, Type::Maybe(Box::new(ty)))),
                    ),
                    _ => warn!("enum symbol not added to AST due to lacking name"),
                },
                t => warn!(
                    "symbol with type {:?} not added to AST due to lacking name",
                    t
                ),
            }
        }
    }

    ctx.sym_types.insert(seq.id, sym_tys);
    let ty = Type::Node(ctx.nodes.len());
    let node = Node {
        name: alloc_name(ctx, name_stem),
        kind: NodeKind::Union(fields.into_iter().collect()),
    };
    ctx.nodes.push(node);
    ctx.seq_types.insert(seq.id, ty.clone());
    ty
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

    /// A tuple node in the AST.
    Tuple(Vec<Type>),

    /// An enumerated node in the AST.
    ///
    /// Enumerated nodes have separate variants for each node type returned by the
    /// rules associated with a nonterminal.
    Enum(Vec<Type>),
}

#[allow(dead_code)]
fn codegen_node(ctx: &Context, grammar: &Grammar, node: &Node) {
    match node.kind {
        NodeKind::Union(ref fields) => {
            println!("pub struct {} {{", node.name);
            for &(ref name, ref ty) in fields {
                println!("    pub {}: {},", name, codegen_type(ctx, grammar, ty));
            }
            println!("}}");
        }
        NodeKind::Tuple(ref fields) => {
            println!("pub struct {} (", node.name);
            for ty in fields {
                println!("    pub {},", codegen_type(ctx, grammar, ty));
            }
            println!(");");
        }
        NodeKind::Enum(ref variants) => {
            println!("pub enum {} {{", node.name);
            for (i, v) in variants.iter().enumerate() {
                println!("    V{}({}),", i, codegen_type(ctx, grammar, v));
            }
            println!("}}");
        }
    }
}

fn codegen_type(ctx: &Context, grammar: &Grammar, ty: &Type) -> String {
    match *ty {
        Type::Extern(ref e) => e.clone(),
        Type::Terminal(id) => grammar[id].data_type.clone().unwrap_or("()".into()),
        Type::Nonterminal(id) => codegen_type(ctx, grammar, &ctx.nonterm_types[&id]),
        Type::Sequence(id) => codegen_type(ctx, grammar, &ctx.seq_types[&id]),
        Type::Maybe(ref ty) => format!("Option<{}>", codegen_type(ctx, grammar, &*ty)),
        Type::Choice(ref _choices) => panic!("type codegen for choices not yet supported"),
        Type::Array(ref ty) => format!("Vec<{}>", codegen_type(ctx, grammar, &*ty)),
        Type::Node(index) => ctx.nodes[index].name.clone(),
    }
}

fn codegen_reduce_fn(ctx: &Context, seqid: SequenceId, mapping: &[usize]) {
    println!("// sequence {}", seqid);

    // Assemble the arguments.
    let used_syms: HashSet<_> = mapping.iter().cloned().collect();
    let mut args = Vec::new();
    for (i, ty) in ctx.sym_types[&seqid].iter().enumerate() {
        let prefix = if used_syms.contains(&i) { "" } else { "_" };
        args.push(format!(
            "{}arg{}: {}",
            prefix,
            i,
            codegen_type(ctx, ctx.grammar, ty)
        ));
    }

    let args = args.into_iter().fold(String::new(), |mut s, i| {
        if !s.is_empty() {
            s.push_str(", ");
        }
        s.push_str(&i);
        s
    });

    let ty = &ctx.seq_types[&seqid];
    let rety = codegen_type(ctx, ctx.grammar, ty);
    println!("pub fn reduce_{}({}) -> {} {{", seqid, args, rety);
    let node_id = match *ty {
        Type::Node(id) => id,
        _ => panic!("reduction function should yield non-node type"),
    };
    let node = &ctx.nodes[node_id];
    match node.kind {
        NodeKind::Union(ref fields) => {
            println!("    {} {{", node.name);
            for (&(ref name, _), &i) in fields.iter().zip(mapping.iter()) {
                println!("        {}: arg{},", name, i);
            }
            println!("    }}");
        }
        NodeKind::Tuple(ref _fields) => {
            println!("    {} (", node.name);
            for &i in mapping {
                println!("        arg{},", i);
            }
            println!("    )");
        }
        NodeKind::Enum(ref _variants) => {}
    }
    println!("}}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn singleton_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("b").data_type("String").build(&mut g);
        let _r = g.make_rule(nt_a, |s| {
            s.terminal(t_a).name("name").terminal(t_a).name("type")
        });
        synth(&mut g);
    }

    #[test]
    fn nested_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let nt_b = g.make_nonterminal("item").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.nonterminal(nt_b).name("item").terminal(t_a).name("name")
        });
        g.make_rule(nt_b, |s| {
            s.terminal(t_a).name("name").terminal(t_a).name("type")
        });
        synth(&mut g);
    }

    #[test]
    fn multiple_rules() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| s.terminal(t_a).name("item"));
        g.make_rule(nt_a, |s| s.terminal(t_a).name("type"));
        synth(&mut g);
    }

    #[test]
    fn optional_symbols() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.terminal(t_a)
                .maybe()
                .name("type")
                .terminal(t_a)
                .name("name")
        });
        synth(&mut g);
    }

    #[test]
    fn optional_flattened_symbols() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.group(|s| s.terminal(t_a).name("type").terminal(t_a).name("qualifier"))
                .maybe()
                .terminal(t_a)
                .name("name")
        });
        synth(&mut g);
    }

    #[test]
    fn repeated_symbols() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.terminal(t_a)
                .repeat(false)
                .name("type")
                .terminal(t_a)
                .name("name")
        });
        synth(&mut g);
    }

    #[test]
    fn recursive_rule() {
        let mut g = Grammar::new();
        let nt_a = g.make_nonterminal("root").build(&mut g);
        let t_a = g.make_terminal("a").data_type("String").build(&mut g);
        g.make_rule(nt_a, |s| {
            s.nonterminal(nt_a).name("base").terminal(t_a).name("next")
        });
        g.make_rule(nt_a, |s| s.terminal(t_a).name("first"));
        synth(&mut g);
    }
}
