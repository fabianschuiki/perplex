// Copyright (c) 2018 Fabian Schuiki

//! Abstract Syntax Tree synthesis.
//!
//! This module implements a way of mapping an extended grammar to an Abstract
//! Syntax Tree automatically. Nodes of the tree are generated based on the
//! rules of the grammar, and the user may supply some of the nodes and
//! reduction functions themself.

use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::usize;

use ext::{
    Grammar, Nonterminal, NonterminalId, Rule, Sequence, SequenceId, Symbol, SymbolId, SymbolKind,
    TerminalId,
};

/// A synthesis context.
///
/// This struct is used to hold all intermediate values that are generated
/// during synthesis.
#[derive(Debug)]
struct Context<'a> {
    grammar: &'a Grammar,
    nonterm_nodes: Vec<NonterminalNode<'a>>,
    seq_nodes: Vec<SequenceNode<'a>>,
    sym_nodes: Vec<SymbolNode<'a>>,
    nonterm_mapping: HashMap<NonterminalId, NonterminalNodeId>,
}

impl<'a> Context<'a> {
    fn new(grammar: &'a Grammar) -> Context<'a> {
        Context {
            grammar: grammar,
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

// fn alloc_name<S: AsRef<str>>(ctx: &mut Context, name: S) -> String {
//     let mut buffer = String::new();
//     let mut capitalize = true;
//     for c in name.as_ref().chars() {
//         if c.is_alphanumeric() {
//             match capitalize {
//                 true => buffer.extend(c.to_uppercase()),
//                 false => buffer.extend(c.to_lowercase()),
//             };
//             capitalize = false;
//         } else {
//             capitalize = true;
//         }
//     }
//     let count = {
//         let v = ctx.names.entry(buffer.clone()).or_insert(0);
//         *v += 1;
//         *v
//     };
//     if count > 1 {
//         buffer = format!("{}{}", buffer, count);
//     }
//     buffer
// }

/// A type in the AST.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    /// The `nothing` type.
    Nil,
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
    /// Keep something on the heap. Used for recursive types.
    Heap(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Nil => write!(f, "nil"),
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
                    write!(f, "<empty choice>")?;
                }
                Ok(())
            }
            Type::Array(ref ty) => write!(f, "[{}]", ty),
            Type::Node(id) => write!(f, "n{}", id.0),
            Type::Heap(ref ty) => write!(f, "&{}", ty),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// A synthesis tree node for a nonterminal.
#[derive(Debug)]
pub struct NonterminalNode<'a> {
    nonterminal: &'a Nonterminal,
    rules: Vec<SequenceNodeId>,
    children: Children,
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
    kind: SymbolNodeKind,
    children: Children,
    recursive: bool,
}

/// The various forms a synthesis node for a symbol can take.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum SymbolNodeKind {
    None,
    Terminal(TerminalId),
    Nonterminal(NonterminalId),
    Group(SequenceNodeId),
    Maybe(SymbolNodeId),
    Choice(Vec<SymbolNodeId>),
    Repeat(SymbolNodeId, Option<SymbolNodeId>),
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

/// Map a grammar to a synthesized AST.
fn map_grammar(grammar: &Grammar) -> AstSynth {
    let mut ctx = Context::new(grammar);

    // Create nodes for each of the nonterminals.
    for nt in grammar.nonterminals() {
        map_nonterminal(&mut ctx, nt);
    }
    debug!("spawned {} nonterminal nodes", ctx.nonterm_nodes.len());
    debug!("spawned {} sequence nodes", ctx.seq_nodes.len());
    debug!("spawned {} symbol nodes", ctx.sym_nodes.len());
    // println!("{:#?}", ctx);

    // Gather child node information and find recursions in the tree.
    for i in 0..ctx.nonterm_nodes.len() {
        gather_children_nonterminal(&mut ctx, NonterminalNodeId(i));
    }
    if !ctx.nonterm_nodes.is_empty() {
        find_recursions(&mut ctx, NonterminalNodeId(0), &mut IndexSet::new());
    }

    // Map the proto tree into a synthesized AST.
    let mut synth = AstSynth {
        nodes: Vec::new(),
        reducers: Vec::new(),
        reducers2: IndexMap::new(),
        nonterm_types: HashMap::new(),
        seq_reducers: HashMap::new(),
    };
    for i in 0..ctx.nonterm_nodes.len() {
        synth_nonterminal_node(NonterminalNodeId(i), &mut synth, &ctx);
    }
    trace!("synthesized {:#?}", synth);
    synth
}

/// Map a nonterminal to a nonterminal node.
fn map_nonterminal<'a>(ctx: &mut Context<'a>, nonterminal: &'a Nonterminal) -> NonterminalNodeId {
    trace!("mapping nonterminal {:?}", nonterminal.id);
    let mut node = NonterminalNode {
        nonterminal: nonterminal,
        rules: vec![],
        children: Default::default(),
    };

    // Spawn the nodes for each of the rules.
    for rule in node.nonterminal.rules() {
        node.rules
            .push(map_sequence(ctx, node.nonterminal, rule, None, &rule.rhs));
    }

    ctx.add_nonterminal_node(node)
}

/// Map a sequence to a sequence node.
fn map_sequence<'a>(
    ctx: &mut Context<'a>,
    nonterm: &'a Nonterminal,
    rule: &'a Rule,
    parent: Option<&'a Sequence>,
    sequence: &'a Sequence,
) -> SequenceNodeId {
    trace!("mapping sequence {:?}", sequence.id);

    let mut node = SequenceNode {
        nonterminal: nonterm,
        rule: rule,
        parent: parent,
        sequence: sequence,
        tuple: Vec::new(),
        named: IndexMap::new(),
        children: Default::default(),
    };

    // Spawn the nodes for each of the symbols and add them to the tuple and
    // named fields if appropriate.
    for (offset, sym) in node.sequence.symbols.iter().enumerate() {
        let id = map_symbol(ctx, &node, offset, sym);
        node.tuple.push(id);
        if let Some(ref name) = sym.name {
            node.named.insert(name.clone(), id);
        }
    }

    ctx.add_sequence_node(node)
}

/// Map a symbol to a symbol node.
fn map_symbol<'a>(
    ctx: &mut Context<'a>,
    seq_node: &SequenceNode<'a>,
    offset: usize,
    symbol: &'a Symbol,
) -> SymbolNodeId {
    trace!("mapping symbol {:?}.{}", seq_node.sequence.id, offset);
    let mut node = SymbolNode {
        nonterminal: seq_node.nonterminal,
        rule: seq_node.rule,
        parent: seq_node.parent,
        sequence: seq_node.sequence,
        offset: offset,
        symbol: symbol,
        kind: SymbolNodeKind::None,
        children: Default::default(),
        recursive: false,
    };

    match symbol.kind {
        SymbolKind::Terminal(id) => node.kind = SymbolNodeKind::Terminal(id),
        SymbolKind::Nonterminal(id) => node.kind = SymbolNodeKind::Nonterminal(id),
        SymbolKind::Group(ref g) => {
            node.kind = SymbolNodeKind::Group(map_sequence(
                ctx,
                node.nonterminal,
                node.rule,
                Some(node.sequence),
                g,
            ));
        }
        SymbolKind::Maybe(ref sym) => {
            node.kind = SymbolNodeKind::Maybe(map_symbol(ctx, seq_node, usize::MAX, sym));
        }
        SymbolKind::Choice(ref syms) => {
            let mut ids = vec![];
            for sym in syms {
                ids.push(map_symbol(ctx, seq_node, usize::MAX, sym));
            }
            node.kind = SymbolNodeKind::Choice(ids);
        }
        SymbolKind::Repeat(ref rep, ref sep, _) => {
            let rep_id = map_symbol(ctx, seq_node, usize::MAX, rep);
            let sep_id = if let Some(ref sep) = *sep {
                Some(map_symbol(ctx, seq_node, usize::MAX, sep))
            } else {
                None
            };
            node.kind = SymbolNodeKind::Repeat(rep_id, sep_id);
        }
    }

    ctx.add_symbol_node(node)
}

fn gather_children_nonterminal(ctx: &mut Context, nt: NonterminalNodeId) {
    let mut children = Children::default();
    for rule in ctx[nt].rules.clone() {
        gather_children_sequence(ctx, rule);
        children.sequences.insert(rule);
        children.extend(&ctx[rule].children);
    }
    ctx[nt].children = children;
}

fn gather_children_sequence(ctx: &mut Context, seq: SequenceNodeId) {
    let mut children = Children::default();
    for sym in ctx[seq].tuple.clone() {
        gather_children_symbol(ctx, sym);
        children.symbols.insert(sym);
        children.extend(&ctx[sym].children);
    }
    ctx[seq].children = children;
}

fn gather_children_symbol(ctx: &mut Context, sym: SymbolNodeId) {
    let mut children = Children::default();
    match ctx[sym].kind.clone() {
        SymbolNodeKind::None => (),
        SymbolNodeKind::Terminal(_) => (),
        SymbolNodeKind::Nonterminal(id) => {
            children.nonterminals.insert(ctx.nonterm_mapping[&id]);
        }
        SymbolNodeKind::Group(seq) => {
            gather_children_sequence(ctx, seq);
            children.sequences.insert(seq);
            children.extend(&ctx[seq].children);
        }
        SymbolNodeKind::Maybe(sym) => {
            gather_children_symbol(ctx, sym);
            children.symbols.insert(sym);
            children.extend(&ctx[sym].children);
        }
        SymbolNodeKind::Choice(syms) => for sym in syms {
            gather_children_symbol(ctx, sym);
            children.symbols.insert(sym);
            children.extend(&ctx[sym].children);
        },
        SymbolNodeKind::Repeat(rep, sep) => {
            gather_children_symbol(ctx, rep);
            children.symbols.insert(rep);
            children.extend(&ctx[rep].children);
            if let Some(sep) = sep {
                gather_children_symbol(ctx, sep);
                children.symbols.insert(sep);
                children.extend(&ctx[sep].children);
            }
        }
    }
    ctx[sym].children = children;
}

/// Find recursive symbols in the subtree of a nonterminal.
fn find_recursions(
    ctx: &mut Context,
    node_id: NonterminalNodeId,
    stack: &mut IndexSet<NonterminalNodeId>,
) -> bool {
    if !stack.insert(node_id) {
        return true;
    }
    for sym in ctx[node_id].children.symbols.clone() {
        let recursive = match ctx[sym].symbol.kind {
            SymbolKind::Nonterminal(id) => {
                let node_id = ctx.nonterm_mapping[&id];
                find_recursions(ctx, node_id, stack)
            }
            _ => false,
        };
        if ctx[sym].recursive ^ recursive {
            debug!("recursion found:",);
            for &nt in stack.iter() {
                debug!(" - {}", ctx[nt].nonterminal.id.pretty(ctx.grammar));
            }
            debug!("    -> {}", ctx[sym].symbol.pretty(ctx.grammar));
        }
        ctx[sym].recursive |= recursive;
    }
    stack.remove(&node_id);
    false
}

/// A synthesized AST.
///
/// This struct holds all the information necessary to synthesis AST nodes and
/// reduction functions for an extended grammar.
#[derive(Debug)]
pub struct AstSynth {
    nodes: Vec<Node>,
    reducers: Vec<Rc<ReducerNode>>,
    reducers2: IndexMap<SynthSequence, Reducer>,
    nonterm_types: HashMap<NonterminalId, Type>,
    seq_reducers: HashMap<SequenceId, ReducerId>,
}

impl AstSynth {
    /// Synthesize an AST for a grammar.
    pub fn with_grammar(mut grammar: Grammar) -> (AstSynth, Grammar) {
        let synth = map_grammar(&grammar);
        synth.apply(&mut grammar);
        (synth, grammar)
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

    /// Register a reducer.
    fn register_reducer(&mut self, reducer: Reducer) {
        let sseq = reducer.sequence;
        trace!("register reducer for {:?}: {:?}", sseq, reducer);
        if self.reducers2.insert(sseq, reducer).is_some() {
            panic!("reducer for sequence {:?} already registered", sseq);
        }
    }

    /// Generate the code for the AST nodes.
    pub fn generate_ast(&self) -> String {
        let mut out = String::new();
        for node in &self.nodes {
            if !node.suppress {
                if !out.is_empty() {
                    out.push_str("\n\n");
                }
                out.push_str(&self.generate_ast_node(node));
            }
        }
        out
    }

    /// Generate the code for a single AST node.
    pub fn generate_ast_node(&self, node: &Node) -> String {
        let mut out = String::new();
        match node.kind {
            NodeKind::Struct(ref fields) => {
                out.push_str(&format!("pub struct {} {{", node.name));
                for &(ref name, ref ty) in fields {
                    out.push_str(&format!("\n    {}: {},", name, self.generate_type(ty)));
                }
                out.push_str("\n}");
            }
            NodeKind::Tuple(ref fields) => {
                out.push_str(&format!("pub struct {} (", node.name));
                for ty in fields {
                    out.push_str(&format!("\n    {},", self.generate_type(ty)));
                }
                out.push_str("\n);");
            }
            NodeKind::Enum(ref variants) => {
                out.push_str(&format!("pub enum {} {{", node.name));
                for &(ref name, ref ty) in variants {
                    out.push_str(&format!("\n    {}({}),", name, self.generate_type(ty)));
                }
                out.push_str("\n}");
            }
        }
        out
    }

    /// Generate the code for the reduction functions.
    pub fn generate_reducers(&self) -> String {
        let mut out = String::new();
        for (_, reducer) in &self.reducers2 {
            if !out.is_empty() {
                out.push_str("\n\n");
            }
            out.push_str(&self.generate_reducer(reducer));
        }
        out
    }

    /// Generate the code for a single reduction function.
    pub fn generate_reducer(&self, reducer: &Reducer) -> String {
        let mut out = String::new();
        let mut tmp_index = 0;
        let mut args = String::new();
        for (i, ty) in reducer.args.iter().enumerate() {
            if !args.is_empty() {
                args.push_str(", ");
            }
            args.push_str(&format!("arg{}: {}", i, self.generate_type(ty)));
        }
        out.push_str(&format!(
            "pub fn {}({}) -> {} {{",
            reducer.name,
            args,
            self.generate_type(&reducer.ty)
        ));
        // out.push_str(&format!("\n    // {:?}", reducer.root));
        let root = self.generate_reducer_node(&reducer.root, &mut out, &mut tmp_index);
        out.push_str(&format!("\n    {}", root));
        out.push_str("\n}");
        out
    }

    /// Generate the code for a single reducer node.
    pub fn generate_reducer_node(
        &self,
        node: &ReducerNode,
        out: &mut String,
        tmp_index: &mut usize,
    ) -> String {
        match *node {
            ReducerNode::Pick2(_sseq, offset) => format!("arg{}", offset),
            ReducerNode::MakeTuple(node_id, ref fields) => {
                let mut args = String::new();
                for node in fields {
                    if !args.is_empty() {
                        args.push_str(", ");
                    }
                    args.push_str(&self.generate_reducer_node(node, out, tmp_index));
                }
                format!("{}({})", self[node_id].name, args)
            }
            ReducerNode::MakeStruct(node_id, ref fields) => {
                let mut args = String::new();
                for (name, node) in fields {
                    if !args.is_empty() {
                        args.push_str(", ");
                    }
                    args.push_str(name);
                    args.push_str(": ");
                    args.push_str(&self.generate_reducer_node(node, out, tmp_index));
                }
                format!("{} {{ {} }}", self[node_id].name, args)
            }
            ReducerNode::MakeEnum(node_id, variant_index, ref value) => format!(
                "{}::{}({})",
                self[node_id].name,
                self[node_id].as_enum()[variant_index].0,
                self.generate_reducer_node(value, out, tmp_index)
            ),
            ReducerNode::Some(ref node) => {
                format!("Some({})", self.generate_reducer_node(node, out, tmp_index))
            }
            ReducerNode::None => "None".into(),
            ReducerNode::Head(ref node) => {
                format!("vec![{}]", self.generate_reducer_node(node, out, tmp_index))
            }
            ReducerNode::Tail(ref head, ref tail) => {
                let head_value = self.generate_reducer_node(head, out, tmp_index);
                let i = *tmp_index;
                *tmp_index += 1;
                out.push_str(&format!("\n    let mut tmp{} = {};", i, head_value));
                let tail_value = self.generate_reducer_node(tail, out, tmp_index);
                out.push_str(&format!("\n    tmp{}.push({});", i, tail_value));
                format!("tmp{}", i)
            }
            ReducerNode::RepeatEmpty => "Vec::new()".into(),
            _ => panic!("invalid reducer node {:?}", node),
        }
    }

    /// Generate the code for a type.
    pub fn generate_type(&self, ty: &Type) -> String {
        match *ty {
            Type::Nil => "()".into(),
            Type::Extern(ref name) => name.clone(),
            Type::Maybe(ref ty) => format!("Option<{}>", self.generate_type(ty)),
            Type::Array(ref ty) => format!("Vec<{}>", self.generate_type(ty)),
            Type::Heap(ref ty) => format!("Box<{}>", self.generate_type(ty)),
            Type::Node(id) => self[id].name.clone(),
            _ => panic!("unsupported type `{}`", ty),
        }
    }

    /// Register the synthesized AST nodes and reduction functions with a
    /// grammar.
    fn apply(&self, grammar: &mut Grammar) {
        for nt in grammar.nonterminals_mut() {
            if let Some(ty) = self.nonterm_types.get(&nt.id) {
                nt.extern_type = Some(self.generate_type(ty));
            }
            for rule in nt.rules_mut() {
                self.apply_sequence(&mut rule.rhs);
            }
        }
    }

    fn apply_sequence(&self, sequence: &mut Sequence) {
        if let Some(ref reducer) = self.reducers2.get(&SynthSequence::Regular(sequence.id)) {
            // sequence.extern_type = Some(self.generate_type(&reducer.ty));
            sequence.extern_reducer = Some(reducer.name.clone());
        }
        for symbol in &mut sequence.symbols {
            self.apply_symbol(symbol);
        }
    }

    fn apply_symbol(&self, symbol: &mut Symbol) {
        match symbol.kind {
            SymbolKind::Terminal(_) | SymbolKind::Nonterminal(_) => (),
            SymbolKind::Group(ref mut s) => self.apply_sequence(s),
            SymbolKind::Maybe(ref mut s) => self.apply_symbol(s),
            SymbolKind::Repeat(ref mut rep, ref mut sep, _) => {
                self.apply_symbol(rep);
                if let Some(ref mut sep) = *sep {
                    self.apply_symbol(sep);
                }
            }
            SymbolKind::Choice(ref mut symbols) => {
                for symbol in symbols {
                    self.apply_symbol(symbol);
                }
            }
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

impl Index<ReducerId> for AstSynth {
    type Output = Rc<ReducerNode>;
    fn index(&self, idx: ReducerId) -> &Rc<ReducerNode> {
        &self.reducers[idx.0]
    }
}

impl IndexMut<ReducerId> for AstSynth {
    fn index_mut(&mut self, idx: ReducerId) -> &mut Rc<ReducerNode> {
        &mut self.reducers[idx.0]
    }
}

/// A node in the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node {
    /// The name of the node.
    pub name: String,
    /// The kind of the node.
    pub kind: NodeKind,
    /// Whether the node should be suppressed in the output. This happens if the
    /// rule that generated the node has an external type defined.
    pub suppress: bool,
}

impl Node {
    /// Return the struct fields of this node.
    ///
    /// Panics if the node is not a struct.
    pub fn as_struct(&self) -> &[(String, Type)] {
        match self.kind {
            NodeKind::Struct(ref x) => x,
            _ => panic!("node is not a struct"),
        }
    }

    /// Return the tuple fields of this node.
    ///
    /// Panics if the node is not a tuple.
    pub fn as_tuple(&self) -> &[Type] {
        match self.kind {
            NodeKind::Tuple(ref x) => x,
            _ => panic!("node is not a tuple"),
        }
    }

    /// Return the enum variants of this node.
    ///
    /// Panics if the node is not an enum.
    pub fn as_enum(&self) -> &[(String, Type)] {
        match self.kind {
            NodeKind::Enum(ref x) => x,
            _ => panic!("node is not an enum"),
        }
    }
}

/// A unique node identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

/// A node kind.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeKind {
    /// A union node in the AST.
    ///
    /// Struct nodes create one single type of node for all rules associated
    /// with a nonterminal. Fields that are only present in some of the rules
    /// are given an optional type.
    Struct(Vec<(String, Type)>),

    /// A tuple type in the AST.
    Tuple(Vec<Type>),

    /// An enumerated node in the AST.
    ///
    /// Enumerated nodes have separate variants for each node type returned by
    /// the rules associated with a nonterminal.
    Enum(Vec<(String, Type)>),
}

/// A synthesized sequence.
///
/// This may either be a sequence as it appears in an extended grammar, or one
/// of the additional sequences synthesized by maybe, choice, and repeat
/// operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SynthSequence {
    /// A sequence as it appears in an extended grammar.
    Regular(SequenceId),
    /// A sequence synthesized by a maybe operator.
    Maybe(MaybeSequence, SymbolId),
    /// A sequence synthesized by a repeat operator.
    Repeat(RepeatSequence, SymbolId),
}

/// A sequence synthesized by a maybe operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MaybeSequence {
    /// The empty branch of the operator.
    Empty,
    /// The non-empty branch of the operator.
    Nonempty,
}

/// A sequence synthesized by a repeat operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RepeatSequence {
    /// The empty branch of the `*` operator.
    Empty,
    /// The non-empty branch of the `*` operator.
    Nonempty,
    /// The first iteration of the operator.
    Head,
    /// The subsequent iterations of the operator.
    Tail,
}

/// A synthesized reduction function.
#[derive(Debug)]
pub struct Reducer {
    /// The name of the reducer.
    pub name: String,
    /// The sequence which is reduced.
    pub sequence: SynthSequence,
    /// The argument types.
    pub args: Vec<Type>,
    /// The resulting type.
    pub ty: Type,
    /// The root of the sequence of reduction operations.
    pub root: ReducerNode,
}

/// A step in a reduction function.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum ReducerNode {
    /// Pick one of the symbols of the sequence to be reduced.
    Pick(usize),
    Pick2(SynthSequence, usize),
    MakeTuple(NodeId, Vec<Rc<ReducerNode>>),
    MakeStruct(NodeId, Vec<(String, Rc<ReducerNode>)>),
    Either(Rc<ReducerNode>, Rc<ReducerNode>),
    Some(Rc<ReducerNode>),
    None,
    Head(Rc<ReducerNode>),
    Tail(Rc<ReducerNode>, Rc<ReducerNode>),
    RepeatEmpty,
    MakeEnum(NodeId, usize, Rc<ReducerNode>),
}

/// A unique reducer identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReducerId(pub usize);

/// Synthesize the node corresponding to a nonterminal.
fn synth_nonterminal_node(node_id: NonterminalNodeId, synth: &mut AstSynth, ctx: &Context) -> Type {
    // Don't synthesize the node if already done so.
    if let Some(ty) = synth
        .nonterm_types
        .get(&ctx[node_id].nonterminal.id)
        .cloned()
    {
        return ty;
    }

    // Create the node.
    let node = &ctx[node_id];
    let suppress = node.nonterminal.extern_type.is_some();
    debug!("synth {:?} ({})", node_id, node.nonterminal.name);
    let id = synth.add_node(Node {
        name: node
            .nonterminal
            .synth_name
            .clone()
            .unwrap_or_else(|| format!("nonterm_{}", node.nonterminal.name)),
        kind: NodeKind::Enum(Vec::new()),
        suppress: suppress,
    });
    trace!("created {:?}", id);
    let node_ty = match node.nonterminal.extern_type {
        Some(ref et) => Type::Extern(et.clone()),
        None => Type::Node(id),
    };
    synth.register_nonterminal_type(node.nonterminal.id, node_ty.clone());

    // Populate the node body.
    let mut variants = Vec::new();
    for (i, &seq_id) in node.rules.iter().enumerate() {
        let (ty, mut reducer) = synth_sequence_node(seq_id, synth, ctx);
        let name = ctx[seq_id]
            .sequence
            .synth_name
            .clone()
            .unwrap_or_else(|| format!("variant_{}", seq_id.0));
        variants.push((name, ty));
        if !suppress && ctx[seq_id].sequence.extern_reducer.is_none() {
            reducer.ty = node_ty.clone();
            reducer.root = ReducerNode::MakeEnum(id, i, Rc::new(reducer.root));
            synth.register_reducer(reducer);
        }
    }
    synth[id].kind = NodeKind::Enum(variants);

    node_ty
}

fn synth_sequence_node(
    node_id: SequenceNodeId,
    synth: &mut AstSynth,
    ctx: &Context,
) -> (Type, Reducer) {
    debug!("synth {:?}", node_id);

    // Create the node.
    let node = &ctx[node_id];
    let id = synth.add_node(Node {
        name: node
            .sequence
            .synth_name
            .clone()
            .unwrap_or_else(|| format!("seq_{}", node.sequence.id)),
        kind: NodeKind::Enum(Vec::new()),
        suppress: node.parent.is_none() && node.nonterminal.extern_type.is_some(),
    });
    trace!("created {:?}", id);

    // Map the symbols to types.
    let sym_tys: IndexMap<_, _> = node
        .tuple
        .iter()
        .map(|&sym_id| (sym_id, synth_symbol_node(sym_id, synth, ctx)))
        .collect();

    // Populate the node body.
    let sseq = SynthSequence::Regular(node.sequence.id);
    let (kind, reducer) = if node.named.is_empty() {
        let fields = node
            .tuple
            .iter()
            .map(|&sym_id| sym_tys[&sym_id].clone())
            .collect();
        let reducers = node
            .tuple
            .iter()
            .map(|&sym_id| Rc::new(ReducerNode::Pick2(sseq, ctx[sym_id].offset)))
            .collect();
        (
            NodeKind::Tuple(fields),
            ReducerNode::MakeTuple(id, reducers),
        )
    } else {
        let fields = node
            .named
            .iter()
            .map(|(name, &sym_id)| (name.clone(), sym_tys[&sym_id].clone()))
            .collect();
        let reducers = node
            .named
            .iter()
            .map(|(name, &sym_id)| {
                (
                    name.clone(),
                    Rc::new(ReducerNode::Pick2(sseq, ctx[sym_id].offset)),
                )
            })
            .collect();
        (
            NodeKind::Struct(fields),
            ReducerNode::MakeStruct(id, reducers),
        )
    };
    synth[id].kind = kind;

    // Wrap up the reducer.
    let reducer = Reducer {
        name: format!("reduce_seq_{}", node.sequence.id),
        sequence: sseq,
        args: sym_tys.into_iter().map(|(_, ty)| ty).collect(),
        ty: Type::Node(id),
        root: reducer,
    };

    (Type::Node(id), reducer)
}

fn synth_symbol_node(node_id: SymbolNodeId, synth: &mut AstSynth, ctx: &Context) -> Type {
    debug!("synth {:?}", node_id);

    let node = &ctx[node_id];
    match node.kind {
        SymbolNodeKind::None => panic!("symbol kind should have been set"),
        SymbolNodeKind::Terminal(id) => ctx.grammar[id]
            .data_type
            .as_ref()
            .map(|dt| Type::Extern(dt.clone()))
            .unwrap_or(Type::Nil),
        SymbolNodeKind::Nonterminal(id) => {
            let ty = synth_nonterminal_node(ctx.nonterm_mapping[&id], synth, ctx);
            if node.recursive {
                Type::Heap(Box::new(ty))
            } else {
                ty
            }
        }
        SymbolNodeKind::Group(id) => {
            let (ty, reducer) = synth_sequence_node(id, synth, ctx);
            synth.register_reducer(reducer);
            ty
        }
        SymbolNodeKind::Maybe(id) => {
            let inner_ty = synth_symbol_node(id, synth, ctx);
            let ty = Type::Maybe(Box::new(inner_ty.clone()));

            let seq_a = SynthSequence::Maybe(MaybeSequence::Empty, node.symbol.id);
            let seq_b = SynthSequence::Maybe(MaybeSequence::Nonempty, node.symbol.id);

            let red_a = ReducerNode::None;
            let red_b = ReducerNode::Some(Rc::new(ReducerNode::Pick2(seq_b, 0)));

            trace!("maybe node would yield the following reducers:");
            trace!(" - A: {:?}", red_a);
            trace!(" - B: {:?}", red_b);

            synth.register_reducer(Reducer {
                name: format!("reduce_symbol_{}_empty", node.symbol.id),
                sequence: seq_a,
                args: vec![],
                ty: ty.clone(),
                root: red_a,
            });
            synth.register_reducer(Reducer {
                name: format!("reduce_symbol_{}_nonempty", node.symbol.id),
                sequence: seq_b,
                args: vec![inner_ty],
                ty: ty.clone(),
                root: red_b,
            });

            ty
        }
        SymbolNodeKind::Choice(ref ids) => {
            let id = synth.add_node(Node {
                name: format!("seq_{}_symbol_{}", node.sequence.id, node.offset),
                kind: NodeKind::Enum(Vec::new()),
                suppress: false,
            });
            let variants = ids
                .iter()
                .map(|&id| {
                    (
                        format!("variant_{}", id.0),
                        synth_symbol_node(id, synth, ctx),
                    )
                })
                .collect();
            synth[id].kind = NodeKind::Enum(variants);
            Type::Node(id)
        }
        SymbolNodeKind::Repeat(id, _) => {
            let inner_ty = synth_symbol_node(id, synth, ctx);
            let ty = Type::Array(Box::new(inner_ty.clone()));

            // TODO: check for allow_empty
            if true {
                let seq_a = SynthSequence::Repeat(RepeatSequence::Empty, node.symbol.id);
                let seq_b = SynthSequence::Repeat(RepeatSequence::Nonempty, node.symbol.id);

                let red_a = ReducerNode::RepeatEmpty;
                let red_b = ReducerNode::Pick2(seq_b, 0);

                synth.register_reducer(Reducer {
                    name: format!("reduce_symbol_{}_empty", node.symbol.id),
                    sequence: seq_a,
                    args: vec![],
                    ty: ty.clone(),
                    root: red_a,
                });
                synth.register_reducer(Reducer {
                    name: format!("reduce_symbol_{}_nonempty", node.symbol.id),
                    sequence: seq_b,
                    args: vec![ty.clone()],
                    ty: ty.clone(),
                    root: red_b,
                });
            }

            let seq_c = SynthSequence::Repeat(RepeatSequence::Head, node.symbol.id);
            let seq_d = SynthSequence::Repeat(RepeatSequence::Tail, node.symbol.id);

            let red_c = ReducerNode::Head(Rc::new(ReducerNode::Pick2(seq_c, 0)));
            let red_d = ReducerNode::Tail(
                Rc::new(ReducerNode::Pick2(seq_d, 0)),
                Rc::new(ReducerNode::Pick2(seq_d, 2)),
            );

            synth.register_reducer(Reducer {
                name: format!("reduce_symbol_{}_head", node.symbol.id),
                sequence: seq_c,
                args: vec![inner_ty.clone()],
                ty: ty.clone(),
                root: red_c,
            });
            synth.register_reducer(Reducer {
                name: format!("reduce_symbol_{}_tail", node.symbol.id),
                sequence: seq_d,
                args: vec![ty.clone(), Type::Nil, inner_ty.clone()],
                ty: ty.clone(),
                root: red_d,
            });

            ty
        }
    }
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
