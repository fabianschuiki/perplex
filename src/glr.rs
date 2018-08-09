// Copyright (c) 2018 Fabian Schuiki

//! Analysis of the GLR/LR(k) behaviour of a grammar.

use std::fmt;
use std::ops::{Index, IndexMut};
use std::collections::{HashSet, VecDeque};
use indexmap::{IndexMap, IndexSet};

use grammar::{Grammar, Symbol, TerminalId};
use item_set::{Action, ItemSetId, ItemSets};

/// An analysis of conflicts and their resolvability via GLR/LR(k).
#[derive(Debug)]
pub struct GlrAnalysis {}

impl GlrAnalysis {
    /// Analyze a grammar.
    pub fn compute(grammar: &Grammar, item_sets: &ItemSets) -> GlrAnalysis {
        let conflicts = find_conflicts(item_sets);
        for conflict in conflicts {
            let arc = find_conflict_arc(&conflict, grammar, item_sets);
            println!("{:?}", arc);
        }
        GlrAnalysis {}
    }
}

/// Find the conflict points in the item sets of a grammar where the same symbol
/// would lead to two different actions.
pub fn find_conflicts(item_sets: &ItemSets) -> Vec<Conflict> {
    let mut conflicts = Vec::new();
    for is in item_sets.all().iter() {
        // No conflicts possible if there are less than two items.
        if is.items().len() < 2 {
            continue;
        }

        // Create a Symbol -> {Action} mapping.
        let mut table: IndexMap<Symbol, IndexSet<Action>> = IndexMap::new();
        for &(symbol, action) in is.actions() {
            table
                .entry(symbol)
                .or_insert_with(|| IndexSet::new())
                .insert(action);
        }

        // Filter out all items with more than one possible action.
        conflicts.extend(table.into_iter().filter_map(|(symbol, actions)| {
            if actions.len() > 1 {
                Some(Conflict {
                    item_set: is.id(),
                    symbol: symbol,
                    actions: actions.into_iter().collect(),
                })
            } else {
                None
            }
        }));
    }
    conflicts
}

/// A conflict between multiple actions triggered by the same symbol.
#[derive(Debug)]
pub struct Conflict {
    item_set: ItemSetId,
    symbol: Symbol,
    actions: Vec<Action>,
}

/// Find the arc of a conflict.
pub fn find_conflict_arc(
    conflict: &Conflict,
    grammar: &Grammar,
    item_sets: &ItemSets,
) -> ConflictArc {
    println!("analyzing {:#?}", conflict);
    // Initialize the conflict arc with one root node. Then advance that node
    // such that each item set it contains is at a location where a shift may
    // occur.
    let mut arc = ConflictArc {
        nodes: vec![
            ConflictNode {
                rank: 0,
                item_sets: vec![conflict.item_set],
                parents: vec![],
                shifts: vec![],
                resolved: false,
            },
        ],
        ranks: vec![vec![ConflictNodeId(0)]],
    };
    advance_node_to_shift(&mut arc, ConflictNodeId(0), grammar, item_sets);

    // Iteratively advance the arc through the state space until all ambiguities
    // have been resolved (i.e. when no more ranks are spawned).
    for i in 0.. {
        if i > 1000 {
            panic!("conflict arc analysis did not converge after 1000 iterations");
        }
        let any_spawned = spawn_next_rank(&mut arc, grammar, item_sets);
        if !any_spawned {
            break;
        }
        for node_id in arc.ranks.last().unwrap().clone() {
            advance_node_to_shift(&mut arc, node_id, grammar, item_sets);
        }
    }

    arc
}

/// Advance the item sets of a node to points in the graph where a shift action
/// will occur. This may remove existing item sets and is likely to add new
/// ones.
fn advance_node_to_shift(
    arc: &mut ConflictArc,
    node: ConflictNodeId,
    grammar: &Grammar,
    item_sets: &ItemSets,
) {
    // We use a set to keep track of the visited nodes. A todo deque/set hybrid
    // ensures that we process items in-order, but never have an item in the
    // queue more than once.
    let mut visited: HashSet<ItemSetId> = HashSet::new();
    let mut todo: VecDeque<ItemSetId> = VecDeque::new();
    todo.extend(arc[node].item_sets.iter().cloned());
    visited.extend(todo.iter().cloned());

    // Keep working off the todo queue.
    let mut new_item_sets = Vec::new();
    while let Some(id) = todo.pop_front() {
        // println!("processing {:?}", id);

        let mut any_shifts = false;
        for &(symbol, action) in item_sets[id].actions() {
            match (symbol, action) {
                // Observe whether there are any shift actions.
                (Symbol::Terminal(_), Action::Shift(_)) => any_shifts = true,
                // Apply reduce actions.
                (Symbol::Terminal(_), Action::Reduce(rule_id)) => {
                    // Count the number of terminals this reduction would remove
                    // from the stack, since we're only tracing terminals in the
                    // arc.
                    let len = grammar[rule_id]
                        .symbols()
                        .iter()
                        .cloned()
                        .filter(Symbol::is_terminal)
                        .count();
                    // println!(" - reduction {:?} covers {} terminals", rule_id, len);
                    if len > arc[node].rank {
                        panic!(
                            concat!(
                                "reduction {} in i{} escapes parser subspace",
                                " ({} terminals on the stack, but reduction would remove {})"
                            ),
                            action,
                            id,
                            arc[node].rank,
                            len
                        );
                    }

                    // Step back `len` nodes.
                    let mut nodes = IndexSet::new();
                    nodes.insert(node);
                    for _ in 0..len {
                        nodes = nodes
                            .into_iter()
                            .flat_map(|node_id| arc[node_id].parents.iter().cloned())
                            .collect();
                    }

                    // Determine the name of the nonterminal that results from
                    // the reduction.
                    let nt = grammar[rule_id].name();

                    // Perform the goto actions at the destination item sets
                    // determined by the nodes found above.
                    for &id in nodes.into_iter().flat_map(|id| arc[id].item_sets.iter()) {
                        // println!(
                        //     " - lookup goto with {} at node {:?}",
                        //     nt.pretty(grammar),
                        //     id
                        // );
                        for &(symbol, action) in item_sets[id].actions() {
                            if symbol == Symbol::Nonterminal(nt) {
                                match action {
                                    Action::Shift(target) => if !visited.contains(&target) {
                                        todo.push_back(target);
                                        visited.insert(target);
                                    },
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        }

        // If there were any actions that shift terminals, add the item set to
        // the node for further inspection.
        if any_shifts {
            // println!(" - has shifts");
            new_item_sets.push(id);
        }
    }

    arc[node].item_sets = new_item_sets;
}

/// Trace through all shift actions in the last rank of the arc and thus spawn
/// the next rank of nodes. Returns true if any nodes were spawned, or false if
/// all conflicts were resolved.
fn spawn_next_rank(arc: &mut ConflictArc, grammar: &Grammar, item_sets: &ItemSets) -> bool {
    let new_rank = arc.ranks.len();
    let mut new_rank_nodes = vec![];

    for node_id in arc.ranks.last().unwrap().clone() {
        println!("expanding {}", node_id);

        // Determine the item sets that are destinations of shift actions. Group
        // these sets by the terminal that causes the shift. This then indicates
        // if there is any ambiguity involved in reading a terminal.
        let mut shifts: IndexMap<TerminalId, IndexSet<ItemSetId>> = IndexMap::new();

        for &is_id in arc[node_id].item_sets.iter() {
            println!(" - tracing from {} i{}", node_id, is_id);
            for &(symbol, action) in item_sets[is_id].actions() {
                if let (Symbol::Terminal(t), Action::Shift(target)) = (symbol, action) {
                    println!(" - {} -> i{}", t.pretty(grammar), target);
                    shifts.entry(t).or_insert_with(IndexSet::new).insert(target);
                }
            }
        }

        // Create new nodes in the conflict arc for each shift that causes
        // an ambiguity as indicated by multiple target item sets. All other
        // shifts indicate a resolution of ambiguity.
        println!(" - shifts from {} {:?}", node_id, shifts);
        let mut all_resolved = true;
        for (terminal, mut item_sets) in shifts {
            if item_sets.len() > 1 {
                println!(
                    " - shift {} has item sets {:?}",
                    terminal.pretty(grammar),
                    item_sets
                );
                let new_node_id = ConflictNodeId(arc.nodes.len());
                let new_node = ConflictNode {
                    rank: new_rank,
                    item_sets: item_sets.into_iter().collect(),
                    parents: vec![node_id],
                    shifts: vec![],
                    resolved: false,
                };
                arc.nodes.push(new_node);
                new_rank_nodes.push(new_node_id);
                arc[node_id]
                    .shifts
                    .push((terminal, ConflictEdge::Ambiguous(new_node_id)));
                all_resolved = false;
            } else {
                println!(" - shift {} resolves ambiguity", terminal.pretty(grammar));
                arc[node_id]
                    .shifts
                    .push((terminal, ConflictEdge::Resolved(item_sets.pop().unwrap())));
            }
        }
        arc[node_id].resolved = all_resolved;
    }

    if new_rank_nodes.is_empty() {
        false
    } else {
        arc.ranks.push(new_rank_nodes);
        true
    }
}

/// A subspace of a parser's state space within which a conflict is active.
#[derive(Debug)]
pub struct ConflictArc {
    nodes: Vec<ConflictNode>,
    ranks: Vec<Vec<ConflictNodeId>>,
}

/// A unique identifier for a node in a conflict arc.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictNodeId(usize);

impl fmt::Display for ConflictNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

impl fmt::Debug for ConflictNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Index<ConflictNodeId> for ConflictArc {
    type Output = ConflictNode;

    fn index(&self, index: ConflictNodeId) -> &ConflictNode {
        &self.nodes[index.0]
    }
}

impl IndexMut<ConflictNodeId> for ConflictArc {
    fn index_mut(&mut self, index: ConflictNodeId) -> &mut ConflictNode {
        &mut self.nodes[index.0]
    }
}

/// A node in a conflict arc.
#[derive(Debug)]
pub struct ConflictNode {
    rank: usize,
    item_sets: Vec<ItemSetId>,
    parents: Vec<ConflictNodeId>,
    shifts: Vec<(TerminalId, ConflictEdge)>,
    resolved: bool,
}

/// An edge from one conflict node to another.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum ConflictEdge {
    Resolved(ItemSetId),
    Ambiguous(ConflictNodeId),
}
