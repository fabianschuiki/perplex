// Copyright (c) 2018 Fabian Schuiki

//! Abstract Syntax Tree synthesis.
//!
//! This module implements a way of mapping an extended grammar to an Abstract
//! Syntax Tree automatically. Nodes of the tree are generated based on the
//! rules of the grammar, and the user may supply some of the nodes and
//! reduction functions themself.

use indexmap::{IndexMap, IndexSet};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::usize;

use ext::{
    Grammar, Nonterminal, NonterminalId, Rule, Sequence, SequenceId, Symbol, SymbolKind, TerminalId,
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
    nonterm_nodes: Vec<NonterminalNode<'a>>,
    seq_nodes: Vec<SequenceNode<'a>>,
    sym_nodes: Vec<SymbolNode<'a>>,
    nonterm_mapping: HashMap<NonterminalId, NonterminalNodeId>,
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
            nonterm_nodes: Vec::new(),
            seq_nodes: Vec::new(),
            sym_nodes: Vec::new(),
            nonterm_mapping: HashMap::new(),
        }
    }

    fn add_nonterminal_node(&mut self, node: NonterminalNode<'a>) -> NonterminalNodeId {
        let id = NonterminalNodeId(self.nonterm_nodes.len());
        self.nonterm_mapping.insert(node.nonterminal.id, id);
        self.nonterm_nodes.push(node);
        id
    }

    fn add_sequence_node(&mut self, node: SequenceNode<'a>) -> SequenceNodeId {
        let id = SequenceNodeId(self.seq_nodes.len());
        self.seq_nodes.push(node);
        id
    }

    fn add_symbol_node(&mut self, node: SymbolNode<'a>) -> SymbolNodeId {
        let id = SymbolNodeId(self.sym_nodes.len());
        self.sym_nodes.push(node);
        id
    }
}

// Allow indexing into the context by node IDs.

impl<'a> Index<NonterminalNodeId> for Context<'a> {
    type Output = NonterminalNode<'a>;
    fn index(&self, idx: NonterminalNodeId) -> &NonterminalNode<'a> {
        &self.nonterm_nodes[idx.0]
    }
}

impl<'a> IndexMut<NonterminalNodeId> for Context<'a> {
    fn index_mut(&mut self, idx: NonterminalNodeId) -> &mut NonterminalNode<'a> {
        &mut self.nonterm_nodes[idx.0]
    }
}

impl<'a> Index<SequenceNodeId> for Context<'a> {
    type Output = SequenceNode<'a>;
    fn index(&self, idx: SequenceNodeId) -> &SequenceNode<'a> {
        &self.seq_nodes[idx.0]
    }
}

impl<'a> IndexMut<SequenceNodeId> for Context<'a> {
    fn index_mut(&mut self, idx: SequenceNodeId) -> &mut SequenceNode<'a> {
        &mut self.seq_nodes[idx.0]
    }
}

impl<'a> Index<SymbolNodeId> for Context<'a> {
    type Output = SymbolNode<'a>;
    fn index(&self, idx: SymbolNodeId) -> &SymbolNode<'a> {
        &self.sym_nodes[idx.0]
    }
}

impl<'a> IndexMut<SymbolNodeId> for Context<'a> {
    fn index_mut(&mut self, idx: SymbolNodeId) -> &mut SymbolNode<'a> {
        &mut self.sym_nodes[idx.0]
    }
}

fn alloc_name<S: AsRef<str>>(ctx: &mut Context, name: S) -> String {
    let mut buffer = String::new();
    let mut capitalize = true;
    for c in name.as_ref().chars() {
        if c.is_alphanumeric() {
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
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Node(NodeId),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Extern(ref name) => write!(f, "`{}`", name),
            Type::Terminal(id) => write!(f, "typeof({})", id),
            Type::Nonterminal(id) => write!(f, "typeof({})", id),
            Type::Sequence(id) => write!(f, "typeof({})", id),
            Type::Maybe(ref ty) => write!(f, "{}?", ty),
            Type::Choice(ref tys) => {
                let mut iter = tys.iter();
                let first = iter.next();
                if let Some(first) = first {
                    write!(f, "{}", first)?;
                    for t in iter {
                        write!(f, "|{}", t)?;
                    }
                } else {
                    write!(f, "nil")?;
                }
                Ok(())
            }
            Type::Array(ref ty) => write!(f, "[{}]", ty),
            Type::Node(id) => write!(f, "n{}", id.0),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

fn synth(grammar: &Grammar) -> AstSynth {
    let mut ctx = Context::new(grammar);
    for nt in grammar.nonterminals() {
        map_nonterminal(&mut ctx, nt);

        // Create a new node for this nonterminal.
        let mut node = NonterminalNode {
            nonterminal: nt,
            rules: vec![],
            children: Default::default(),
        };
        map_nonterminal_new(&mut ctx, &mut node);
        let _id = ctx.add_nonterminal_node(node);
    }
    debug!("spawned {} nonterminal nodes", ctx.nonterm_nodes.len());
    debug!("spawned {} sequence nodes", ctx.seq_nodes.len());
    debug!("spawned {} symbol nodes", ctx.sym_nodes.len());
    // println!("{:#?}", ctx);

    // Gather child node information and find recursions in the tree.
    for i in (0..ctx.nonterm_nodes.len()).map(|i| NonterminalNodeId(i)) {
        gather_children_nonterminal(&mut ctx, i);
    }
    if !ctx.nonterm_nodes.is_empty() {
        find_recursions(&mut ctx, NonterminalNodeId(0), &mut HashSet::new());
    }

    // Map the proto tree into a synthesized AST.
    let mut synth = AstSynth {
        nodes: Vec::new(),
        nonterm_types: HashMap::new(),
    };
    for i in 0..ctx.nonterm_nodes.len() {
        synth_nonterminal_node(NonterminalNodeId(i), &mut synth, &ctx);
    }
    debug!("synthesized {:#?}", synth);
    synth

    // for n in &ctx.nodes {
    //     println!("");
    //     codegen_node(&ctx, grammar, n);
    // }

    // for (&seqid, f) in &ctx.reduce_fns {
    //     println!("");
    //     codegen_reduce_fn(&ctx, seqid, f);
    // }
}

/// A synthesis tree node for a nonterminal.
#[derive(Debug)]
pub struct NonterminalNode<'a> {
    nonterminal: &'a Nonterminal,
    rules: Vec<SequenceNodeId>,
    children: Children,
    // TODO: Add reduction function details.
}

/// A synthesis tree node for a sequence.
#[derive(Debug)]
pub struct SequenceNode<'a> {
    nonterminal: &'a Nonterminal,
    rule: &'a Rule,
    parent: Option<&'a Sequence>,
    sequence: &'a Sequence,
    tuple: Vec<SymbolNodeId>,
    named: IndexMap<String, SymbolNodeId>,
    children: Children,
    // TODO: Add reduction function details.
}

/// A synthesis node for a symbol.
#[derive(Debug)]
pub struct SymbolNode<'a> {
    nonterminal: &'a Nonterminal,
    rule: &'a Rule,
    parent: Option<&'a Sequence>,
    sequence: &'a Sequence,
    offset: usize,
    symbol: &'a Symbol,
    children: Children,
    recursive: bool,
}

/// A set of child nodes.
#[derive(Debug, Default)]
pub struct Children {
    nonterminals: IndexSet<NonterminalNodeId>,
    sequences: IndexSet<SequenceNodeId>,
    symbols: IndexSet<SymbolNodeId>,
}

impl Children {
    fn extend(&mut self, other: &Children) {
        self.nonterminals.extend(other.nonterminals.iter().cloned());
        self.sequences.extend(other.sequences.iter().cloned());
        self.symbols.extend(other.symbols.iter().cloned());
    }
}

/// A unique nonterminal node identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonterminalNodeId(pub usize);

/// A unique sequence node identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SequenceNodeId(pub usize);

/// A unique symbol node identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNodeId(pub usize);

fn map_nonterminal_new<'a>(ctx: &mut Context<'a>, node: &mut NonterminalNode<'a>) {
    trace!("mapping nonterminal {:?}", node.nonterminal.id);

    // Spawn the nodes for each of the rules.
    for rule in node.nonterminal.rules() {
        let mut seqnode = SequenceNode {
            nonterminal: node.nonterminal,
            rule: rule,
            parent: None,
            sequence: &rule.rhs,
            tuple: Vec::new(),
            named: IndexMap::new(),
            children: Default::default(),
        };
        map_sequence_new(ctx, &mut seqnode);
        let id = ctx.add_sequence_node(seqnode);
        node.rules.push(id);
    }
}

fn map_sequence_new<'a>(ctx: &mut Context<'a>, node: &mut SequenceNode<'a>) {
    trace!("mapping sequence {:?}", node.sequence.id);

    // Spawn the nodes for each of the symbols.
    for (offset, sym) in node.sequence.symbols.iter().enumerate() {
        let mut symnode = SymbolNode {
            nonterminal: node.nonterminal,
            rule: node.rule,
            parent: node.parent,
            sequence: node.sequence,
            offset: offset,
            symbol: sym,
            children: Default::default(),
            recursive: false,
        };
        map_symbol_new(ctx, &mut symnode);
        let id = ctx.add_symbol_node(symnode);

        // Add this symbol to the tuple and named fields if appropriate.
        node.tuple.push(id);
        if let Some(ref name) = sym.name {
            node.named.insert(name.clone(), id);
        }
    }
}

fn map_symbol_new<'a>(ctx: &mut Context<'a>, node: &mut SymbolNode<'a>) {
    trace!("mapping symbol {:?}.{}", node.sequence.id, node.offset);
    match node.symbol.kind {
        SymbolKind::Terminal(_) => (),
        SymbolKind::Nonterminal(_) => (),
        SymbolKind::Group(ref g) => {
            // TODO: track the sequence id.
            let mut seqnode = SequenceNode {
                nonterminal: node.nonterminal,
                rule: node.rule,
                parent: Some(node.sequence),
                sequence: g,
                tuple: Vec::new(),
                named: IndexMap::new(),
                children: Default::default(),
            };
            map_sequence_new(ctx, &mut seqnode);
            ctx.add_sequence_node(seqnode);
        }
        SymbolKind::Maybe(ref sym) => {
            // TODO: track the symbol id.
            let mut symnode = SymbolNode {
                nonterminal: node.nonterminal,
                rule: node.rule,
                parent: node.parent,
                sequence: node.sequence,
                offset: usize::MAX,
                symbol: sym,
                children: Default::default(),
                recursive: false,
            };
            map_symbol_new(ctx, &mut symnode);
            ctx.add_symbol_node(symnode);
        }
        SymbolKind::Choice(ref syms) => {
            for sym in syms {
                // TODO: track the symbol id.
                let mut symnode = SymbolNode {
                    nonterminal: node.nonterminal,
                    rule: node.rule,
                    parent: node.parent,
                    sequence: node.sequence,
                    offset: usize::MAX,
                    symbol: sym,
                    children: Default::default(),
                    recursive: false,
                };
                map_symbol_new(ctx, &mut symnode);
                ctx.add_symbol_node(symnode);
            }
        }
        SymbolKind::Repeat(ref rep, ref sep, _) => {
            let rep_id = {
                let mut symnode = SymbolNode {
                    nonterminal: node.nonterminal,
                    rule: node.rule,
                    parent: node.parent,
                    sequence: node.sequence,
                    offset: usize::MAX,
                    symbol: rep,
                    children: Default::default(),
                    recursive: false,
                };
                map_symbol_new(ctx, &mut symnode);
                ctx.add_symbol_node(symnode)
            };
            let sep_id = if let Some(ref sep) = *sep {
                let mut symnode = SymbolNode {
                    nonterminal: node.nonterminal,
                    rule: node.rule,
                    parent: node.parent,
                    sequence: node.sequence,
                    offset: usize::MAX,
                    symbol: sep,
                    children: Default::default(),
                    recursive: false,
                };
                map_symbol_new(ctx, &mut symnode);
                Some(ctx.add_symbol_node(symnode))
            } else {
                None
            };
            // TODO: track the symbol ids.
        }
    }
}

fn gather_children_nonterminal(ctx: &mut Context, nt: NonterminalNodeId) {
    let mut children = Children::default();
    for rule in ctx[nt].rules.clone() {
        gather_children_sequence(ctx, rule);
        children.extend(&ctx[rule].children);
    }
    trace!("nonterminal {:?} {:#?}", nt, children);
    ctx[nt].children = children;
}

fn gather_children_sequence(ctx: &mut Context, seq: SequenceNodeId) {
    let mut children = Children::default();
    for sym in ctx[seq].tuple.clone() {
        gather_children_symbol(ctx, sym);
        children.symbols.insert(sym);
        children.extend(&ctx[sym].children);
    }
    trace!("sequence {:?} {:#?}", seq, children);
    ctx[seq].children = children;
}

fn gather_children_symbol(ctx: &mut Context, sym: SymbolNodeId) {
    let mut children = Children::default();
    match ctx[sym].symbol.kind {
        SymbolKind::Nonterminal(id) => {
            children.nonterminals.insert(ctx.nonterm_mapping[&id]);
        }
        _ => (),
    }
    trace!("symbol {:?} {:#?}", sym, children);
    ctx[sym].children = children;
}

fn find_recursions(
    ctx: &mut Context,
    node_id: NonterminalNodeId,
    stack: &mut HashSet<NonterminalNodeId>,
) -> bool {
    if !stack.insert(node_id) {
        return true;
    }
    for sym in ctx[node_id].children.symbols.clone() {
        // find_recursions_sequence(ctx, &ctx[rule], stack);
        let recursive = match ctx[sym].symbol.kind {
            SymbolKind::Nonterminal(id) => {
                let node_id = ctx.nonterm_mapping[&id];
                find_recursions(ctx, node_id, stack)
            }
            _ => false,
        };
        ctx[sym].recursive = recursive;
        if recursive {
            debug!("recursion in {:?}", sym);
        }
    }
    return false;
}

fn map_nonterminal(ctx: &mut Context, nt: &Nonterminal) -> Type {
    let seqs: Vec<_> = nt
        .rules()
        .enumerate()
        .map(|(i, rule)| {
            (
                format!("V{}", i),
                map_sequence(ctx, &rule.rhs, &format!("{}_v{}", nt.name, i)),
            )
        })
        .collect();
    // let ty = if seqs.len() == 1 {
    //     seqs.into_iter().next().unwrap()
    // } else {
    let ty = Type::Node(NodeId(ctx.nodes.len()));
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
    // trace!("mapping sequence {}", seq.pretty(ctx.grammar));

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
    let ty = Type::Node(NodeId(ctx.nodes.len()));
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
                Type::Node(index) => match ctx.nodes[index.0].kind {
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
    let ty = Type::Node(NodeId(ctx.nodes.len()));
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
#[derive(Debug)]
pub struct AstSynth {
    nodes: Vec<Node>,
    nonterm_types: HashMap<NonterminalId, Type>,
}

impl AstSynth {
    /// Synthesize an AST for a grammar.
    pub fn with_grammar(grammar: &Grammar) -> AstSynth {
        synth(grammar)
    }

    /// Add a node to the synthesized tree and return its id.
    fn add_node(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    /// Register a type for a nonterminal.
    ///
    /// Panics if a type has already been registered for the same nonterminal.
    fn register_nonterminal_type(&mut self, id: NonterminalId, ty: Type) {
        if self.nonterm_types.insert(id, ty).is_some() {
            panic!("type for nonterminal {:?} already registered", id);
        }
    }
}
impl Index<NodeId> for AstSynth {
    type Output = Node;
    fn index(&self, idx: NodeId) -> &Node {
        &self.nodes[idx.0]
    }
}

impl IndexMut<NodeId> for AstSynth {
    fn index_mut(&mut self, idx: NodeId) -> &mut Node {
        &mut self.nodes[idx.0]
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

/// A unique node identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

/// A node kind.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// A union node in the AST.
    ///
    /// Union nodes create one single type of node for all rules associated with a
    /// nonterminal. Fields that are only present in some of the rules are given an
    /// optional type.
    Union(Vec<(String, Type)>),

    /// A tuple type in the AST.
    Tuple(Vec<Type>),

    /// An enumerated node in the AST.
    ///
    /// Enumerated nodes have separate variants for each node type returned by the
    /// rules associated with a nonterminal.
    Enum(Vec<(String, Type)>),
}

/// Synthesize the node corresponding to a nonterminal.
fn synth_nonterminal_node(node_id: NonterminalNodeId, synth: &mut AstSynth, ctx: &Context) -> Type {
    debug!("synth {:?} ({})", node_id, ctx[node_id].nonterminal.name);

    // Create the node.
    let node = &ctx[node_id];
    let id = synth.add_node(Node {
        name: format!("nonterm_{}", node.nonterminal.name),
        kind: NodeKind::Enum(Vec::new()),
    });
    trace!("created {:?}", id);
    synth.register_nonterminal_type(node.nonterminal.id, Type::Node(id));

    // Populate the node body.
    let variants = node
        .rules
        .iter()
        .map(|&seq_id| {
            (
                format!("variant_{}", seq_id.0),
                synth_sequence_node(seq_id, synth, ctx),
            )
        })
        .collect();
    synth[id].kind = NodeKind::Enum(variants);

    Type::Node(id)
}

fn synth_sequence_node(node_id: SequenceNodeId, synth: &mut AstSynth, ctx: &Context) -> Type {
    debug!("synth {:?}", node_id);

    // Create the node.
    let node = &ctx[node_id];
    let id = synth.add_node(Node {
        name: format!("seq_{}", node.sequence.id),
        kind: NodeKind::Enum(Vec::new()),
    });
    trace!("created {:?}", id);

    // Populate the node body.
    let kind = if node.named.is_empty() {
        trace!("impl as tuple {:?}", id);
        let fields = node
            .tuple
            .iter()
            .map(|&sym_id| synth_symbol_node(sym_id, synth, ctx))
            .collect();
        NodeKind::Tuple(fields)
    } else {
        trace!("impl as struct {:?}", id);
        let fields = node
            .named
            .iter()
            .map(|(name, &sym_id)| (name.clone(), synth_symbol_node(sym_id, synth, ctx)))
            .collect();
        NodeKind::Union(fields)
    };
    // synth[id].kind = kind;

    Type::Node(id)
}

fn synth_symbol_node(node_id: SymbolNodeId, synth: &mut AstSynth, ctx: &Context) -> Type {
    debug!("synth {:?}", node_id);

    // Create the node.
    let node = &ctx[node_id];
    let id = synth.add_node(Node {
        name: format!("seq_{}_symbol_{}", node.sequence.id, node.offset),
        kind: NodeKind::Enum(Vec::new()),
    });

    Type::Node(id)
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
            for &(ref i, ref v) in variants.iter() {
                println!("    {}({}),", i, codegen_type(ctx, grammar, v));
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
        Type::Node(index) => ctx.nodes[index.0].name.clone(),
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
    let node = &ctx.nodes[node_id.0];
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
