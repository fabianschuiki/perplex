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
            println!("{:#?}", arc);
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
                lanes: vec![
                    ConflictLane {
                        parents: vec![],
                        seq: vec![conflict.item_set],
                    },
                ],
                shifts: vec![],
                resolved: false,
            },
        ],
        ranks: vec![vec![ConflictNodeId(0)]],
    };
    advance_node_to_shift(&mut arc, ConflictNodeId(0), grammar, item_sets);
    println!("{:#?}", arc);
    // spawn_next_rank(&mut arc, grammar, item_sets);
    // println!("{:#?}", arc);
    // advance_node_to_shift(&mut arc, ConflictNodeId(1), grammar, item_sets);
    // println!("{:#?}", arc);

    // Iteratively advance the arc through the state space until all ambiguities
    // have been resolved (i.e. when no more ranks are spawned).
    for i in 0.. {
        if i > 1000 {
            panic!("conflict arc analysis did not converge after 1000 iterations");
        }
        println!("step {}", i);
        let any_spawned = spawn_next_rank(&mut arc, grammar, item_sets);
        if !any_spawned {
            break;
        }
        // println!("{:#?}", arc);
        for node_id in arc.ranks.last().unwrap().clone() {
            advance_node_to_shift(&mut arc, node_id, grammar, item_sets);
        }
        // println!("{:#?}", arc);
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
    let mut visited: HashSet<ConflictLane> = HashSet::new();
    let mut todo: VecDeque<ConflictLane> = VecDeque::new();
    todo.extend(arc[node].lanes.drain(..));
    visited.extend(todo.iter().cloned());

    // Keep working off the todo queue.
    let mut new_lanes = Vec::new();

    while let Some(lane) = todo.pop_front() {
        let id = lane.last();
        println!("advancing lane {:?} ({:?})", lane, id);

        let mut any_shifts = false;
        let mut visited_reductions = HashSet::new();

        for &(symbol, action) in item_sets[id].actions() {
            match (symbol, action) {
                // Observe whether there are any shift actions.
                (Symbol::Terminal(_), Action::Shift(_)) => any_shifts = true,
                // Apply reduce actions.
                (Symbol::Terminal(_), Action::Reduce(rule_id)) => {
                    if !visited_reductions.insert(rule_id) {
                        continue;
                    }

                    // Determine the name of the nonterminal that results from
                    // the reduction.
                    let nt = grammar[rule_id].name();

                    // Count the number of symbols in this reduction. We use
                    // this number to backtrack through the arc nodes.
                    let len = grammar[rule_id].symbols().len();
                    println!(
                        " - reduction r{} covers {} symbols",
                        rule_id.as_usize(),
                        len
                    );

                    // Step back `len` symbols. This requires carefully stepping
                    // back through the current lane's sequence, and its parent
                    // sequences.
                    let mut lanes = Vec::new();
                    backtrack_lane(&lane, len, &arc, &mut lanes);

                    // Perform the goto actions for each of the backtracked
                    // lanes.
                    for lane in lanes {
                        let id = lane.last();
                        println!(" - lookup goto {} at {:?}", nt.pretty(grammar), id);
                        let mut visited_targets = HashSet::new();
                        for &(symbol, action) in item_sets[id].actions() {
                            if symbol == Symbol::Nonterminal(nt) {
                                match action {
                                    Action::Shift(target) => {
                                        if !visited_targets.insert(target) {
                                            continue;
                                        }
                                        println!("    - goto {:?}", target);
                                        let mut new_lane = lane.clone();
                                        new_lane.seq.push(target);
                                        if !visited.contains(&new_lane) {
                                            visited.insert(new_lane.clone());
                                            todo.push_back(new_lane);
                                        }
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        }

        // If there were any actions that shift terminals, add the lane to the
        // node.
        if any_shifts {
            println!(" - has shifts");
            new_lanes.push(lane);
        }
    }

    arc[node].lanes = new_lanes;
}

/// Backtrack a certain number of symbols in a lane. This may involve more
/// symbols than the lane is long, in which case the function propagates to the
/// parents of the lane.
fn backtrack_lane(
    lane: &ConflictLane,
    len: usize,
    arc: &ConflictArc,
    into: &mut Vec<ConflictLane>,
) {
    println!("    - backtrack {} symbols in lane {:?}", len, lane);
    if len < lane.seq.len() {
        into.push(ConflictLane {
            parents: lane.parents.clone(),
            seq: lane.seq[0..lane.seq.len() - len].into(),
        });
    } else {
        if lane.parents.is_empty() {
            panic!(
                "backtracking {} symbols in lane {:?} escapes parser subspace",
                len, lane,
            );
        }
        for &(node_id, lane_id) in lane.parents.iter() {
            backtrack_lane(&arc[node_id][lane_id], len - lane.seq.len(), arc, into);
        }
    }
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
        let mut parents: IndexMap<ItemSetId, IndexSet<ConflictLaneId>> = IndexMap::new();

        for (lane_id, item_set) in arc[node_id]
            .lanes
            .iter()
            .enumerate()
            .map(|(i, l)| (ConflictLaneId(i), l.last()))
        {
            println!(" - tracing {} from {:?}", node_id, item_set);
            for &(symbol, action) in item_sets[item_set].actions() {
                if let (Symbol::Terminal(t), Action::Shift(target)) = (symbol, action) {
                    println!("    - {} -> i{}", t.pretty(grammar), target);
                    shifts.entry(t).or_insert_with(IndexSet::new).insert(target);
                    parents
                        .entry(target)
                        .or_insert_with(IndexSet::new)
                        .insert(lane_id);
                }
            }
        }

        if shifts.is_empty() {
            panic!("arrived in deadlock with lanes {:#?}", arc[node_id].lanes);
        }

        // Create new nodes in the conflict arc for each shift that causes
        // an ambiguity as indicated by multiple target item sets. All other
        // shifts indicate a resolution of ambiguity.
        println!(" - shifts = {:?}", shifts);
        println!(" - parents = {:?}", parents);
        let mut all_resolved = true;
        for (terminal, mut item_sets) in shifts {
            if item_sets.len() > 1 {
                println!(
                    " - shift {} has item sets {:?}",
                    terminal.pretty(grammar),
                    item_sets
                );
                let new_lanes = item_sets
                    .into_iter()
                    .map(|item_set| ConflictLane {
                        parents: parents[&item_set]
                            .iter()
                            .map(|&lane_id| (node_id, lane_id))
                            .collect(),
                        seq: vec![item_set],
                    })
                    .collect();
                let new_node_id = ConflictNodeId(arc.nodes.len());
                let new_node = ConflictNode {
                    rank: new_rank,
                    lanes: new_lanes,
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
///
/// A conflict arc is described as nodes with edges in between, the former of
/// which corresponds to a parser state just before a shift action, and the
/// latter to the actual terminals shifted. Each node contains a list of lanes
/// which describe multiple states the parser has to be in due to ambiguities.
/// Each lane represents a sequence of reduce actions and has separate backward
/// edges to the lanes in previous nodes. The nodes are given a rank which
/// represents the number of terminals shifted up to that point. As such the
/// graph describes parallel execution of the parser state machine.
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
    lanes: Vec<ConflictLane>,
    shifts: Vec<(TerminalId, ConflictEdge)>,
    resolved: bool,
}

/// A unique identifier for a lane in a conflict node.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictLaneId(usize);

impl fmt::Display for ConflictLaneId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "nl{}", self.0)
    }
}

impl fmt::Debug for ConflictLaneId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Index<ConflictLaneId> for ConflictNode {
    type Output = ConflictLane;

    fn index(&self, index: ConflictLaneId) -> &ConflictLane {
        &self.lanes[index.0]
    }
}

impl IndexMut<ConflictLaneId> for ConflictNode {
    fn index_mut(&mut self, index: ConflictLaneId) -> &mut ConflictLane {
        &mut self.lanes[index.0]
    }
}

/// An edge from one conflict node to another.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum ConflictEdge {
    Resolved(ItemSetId),
    Ambiguous(ConflictNodeId),
}

/// An individual point in the state space tracked by a conflict node. Lanes
/// represent the sequences of reductions that happen in between shifts. They
/// may have back-edges to lanes in earlier nodes to represent the state of the
/// stack at various points.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictLane {
    parents: Vec<(ConflictNodeId, ConflictLaneId)>,
    seq: Vec<ItemSetId>,
}

impl ConflictLane {
    /// Return the last item set in the lane.
    ///
    /// Panics if the lane is empty.
    pub fn last(&self) -> ItemSetId {
        self.seq
            .last()
            .expect("lane has no items, which is forbidden")
            .clone()
    }
}

impl fmt::Debug for ConflictLane {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}->{:?}", self.parents, self.seq)
    }
}
