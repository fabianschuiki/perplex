// Copyright (c) 2018 Fabian Schuiki

//! Analysis of the GLR/LR(k) behaviour of a grammar.

use std::fmt;
use std::ops::{Index, IndexMut};
use std::collections::{HashSet, VecDeque};
use indexmap::{IndexMap, IndexSet};

use grammar::{Grammar, RuleId, Symbol, TerminalId};
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
            trace!("{:#?}", arc);
            let reconvs = find_reconvergences(&arc);
            trace!("reconvergences {:#?}", reconvs);
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
    debug!("analyzing {:#?}", conflict);
    // Initialize the conflict arc with one root node. Then advance that node
    // such that each item set it contains is at a location where a shift may
    // occur.
    let mut arc = ConflictArc {
        origin: conflict.item_set,
        nodes: vec![
            ConflictNode {
                rank: 0,
                lanes: vec![
                    ConflictLane {
                        parents: vec![],
                        ancestors: vec![],
                        seq: vec![conflict.item_set],
                        states: vec![conflict.item_set],
                    },
                ],
                shifts: vec![],
                resolved: false,
                reconverged: false,
            },
        ],
        ranks: vec![vec![ConflictNodeId(0)]],
    };
    advance_node_to_shift(&mut arc, ConflictNodeId(0), grammar, item_sets);
    trace!("{:#?}", arc);
    // spawn_next_rank(&mut arc, grammar, item_sets);
    // trace!("{:#?}", arc);
    // advance_node_to_shift(&mut arc, ConflictNodeId(1), grammar, item_sets);
    // trace!("{:#?}", arc);

    // Iteratively advance the arc through the state space until all ambiguities
    // have been resolved (i.e. when no more ranks are spawned).
    for i in 0.. {
        if i > 1000 {
            panic!("conflict arc analysis did not converge after 1000 iterations");
        }
        debug!("step {}", i);
        let any_spawned = spawn_next_rank(&mut arc, grammar, item_sets);
        if !any_spawned {
            break;
        }
        // trace!("{:#?}", arc);
        for node_id in arc.ranks.last().unwrap().clone() {
            advance_node_to_shift(&mut arc, node_id, grammar, item_sets);
        }
        // trace!("{:#?}", arc);
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
    // We use a queue of unfinished lanes to iteratively advance them up to full
    // reduction.
    let mut todo: VecDeque<ConflictLane> = VecDeque::new();
    todo.extend(arc[node].lanes.drain(..));

    // Keep working off the todo queue.
    let mut new_lanes = Vec::new();

    while let Some(current_lane) = todo.pop_front() {
        let id = current_lane.last();
        debug!("advancing lane {:?} ({:?})", current_lane, id);

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
                    trace!(
                        " - reduction r{} covers {} symbols",
                        rule_id.as_usize(),
                        len
                    );

                    // Step back `len` symbols. This requires carefully stepping
                    // back through the current lane's sequence, and its parent
                    // sequences.
                    let mut lanes = Vec::new();
                    backtrack_lane(&current_lane, len, rule_id, &arc, item_sets, &mut lanes);

                    // Perform the goto actions for each of the backtracked
                    // lanes.
                    for mut lane in lanes {
                        lane.ancestors = current_lane.ancestors.clone();
                        lane.states = current_lane.states.clone();

                        let id = lane.last();
                        trace!(" - lookup goto {} at {:?}", nt.pretty(grammar), id);
                        let mut visited_targets = HashSet::new();
                        for &(symbol, action) in item_sets[id].actions() {
                            if symbol == Symbol::Nonterminal(nt) {
                                match action {
                                    Action::Shift(target) => {
                                        if !visited_targets.insert(target) {
                                            continue;
                                        }
                                        trace!("    - goto {:?}", target);
                                        let mut new_lane = lane.clone();
                                        new_lane.seq.push(target);
                                        new_lane.states.push(target);
                                        todo.push_back(new_lane);
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
            trace!(" - has shifts");
            new_lanes.push(current_lane);
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
    rule_id: RuleId,
    arc: &ConflictArc,
    item_sets: &ItemSets,
    into: &mut Vec<ConflictLane>,
) {
    trace!("    - backtrack {} symbols in lane {:?}", len, lane);
    if len < lane.seq.len() {
        into.push(ConflictLane {
            parents: lane.parents.clone(),
            ancestors: vec![],
            seq: lane.seq[0..lane.seq.len() - len].into(),
            states: vec![],
        });
    } else {
        // If this is the conflict root, we have to extrapolate backwards where
        // this reduce action might land us in the state space.
        if lane.parents.is_empty() {
            let mut origins = IndexSet::new();
            origins.insert(lane.last());
            trace!("    - extrapolating from {:?}", origins);
            for marker in (0..len).rev() {
                origins = item_sets
                    .all()
                    .iter()
                    .filter_map(|is| {
                        if is.items()
                            .iter()
                            .filter(|item| item.rule() == rule_id && item.marker() == marker)
                            .any(|item| match item.action() {
                                Some((_, Action::Shift(target))) => origins.contains(&target),
                                _ => false,
                            }) {
                            Some(is.id)
                        } else {
                            None
                        }
                    })
                    .collect();
                trace!("    - extrapolated to {:?} (marker = {})", origins, marker);
            }
            into.extend(origins.into_iter().map(|id| ConflictLane {
                parents: vec![],
                ancestors: vec![],
                seq: vec![id],
                states: vec![],
            }));
        } else {
            for &(node_id, lane_id) in lane.parents.iter() {
                backtrack_lane(
                    &arc[node_id][lane_id],
                    len - lane.seq.len(),
                    rule_id,
                    arc,
                    item_sets,
                    into,
                );
            }
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
        debug!("expanding {}", node_id);

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
            trace!(" - tracing {} from {:?}", node_id, item_set);
            for &(symbol, action) in item_sets[item_set].actions() {
                if let (Symbol::Terminal(t), Action::Shift(target)) = (symbol, action) {
                    trace!("    - {} -> i{}", t.pretty(grammar), target);
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
        trace!(" - shifts = {:?}", shifts);
        trace!(" - parents = {:?}", parents);
        let mut all_resolved = true;
        for (terminal, mut item_sets) in shifts {
            let reconverged = item_sets.iter().any(|is| parents[is].len() > 1);
            if item_sets.len() > 1 || reconverged {
                trace!(
                    " - shift {} has item sets {:?}",
                    terminal.pretty(grammar),
                    item_sets
                );
                let new_lanes = item_sets
                    .into_iter()
                    .map(|item_set| {
                        let new_parents: Vec<_> = parents[&item_set]
                            .iter()
                            .map(|&lane_id| (node_id, lane_id))
                            .collect();
                        ConflictLane {
                            parents: new_parents.clone(),
                            ancestors: new_parents,
                            seq: vec![item_set],
                            states: vec![item_set],
                        }
                    })
                    .collect();
                let new_node_id = ConflictNodeId(arc.nodes.len());
                let new_node = ConflictNode {
                    rank: new_rank,
                    lanes: new_lanes,
                    shifts: vec![],
                    resolved: false,
                    reconverged: reconverged,
                };
                arc.nodes.push(new_node);
                new_rank_nodes.push(new_node_id);
                arc[node_id]
                    .shifts
                    .push((terminal, ConflictEdge::Ambiguous(new_node_id)));
                all_resolved = false;
            } else {
                trace!(" - shift {} resolves ambiguity", terminal.pretty(grammar));
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

/// Find all points of reconvergence in an arc.
pub fn find_reconvergences(arc: &ConflictArc) -> Vec<Reconv> {
    let mut v = vec![];
    for (node_id, node) in arc.nodes.iter().enumerate() {
        let node_id = ConflictNodeId(node_id);
        for (lane_id, lane) in node.lanes.iter().enumerate() {
            let lane_id = ConflictLaneId(lane_id);
            if lane.ancestors.len() > 1 {
                v.push(Reconv {
                    at: (node_id, lane_id),
                    from: lane.ancestors.clone(),
                });
            }
        }
    }
    v
}

/// Find the local ambiguity associated with a point of reconvergence.
pub fn find_local_ambiguity(reconv: &Reconv, arc: &ConflictArc) -> LocalAmbiguity {
    // Trace state progressions from the ambiguity backwards.
    debug!("find ambiguity at {:?}", reconv);
    let traces: Vec<_> = reconv
        .from
        .iter()
        .flat_map(|&(node_id, lane_id)| {
            let states = arc[reconv.at.0][reconv.at.1].states.clone();
            trace_states_backwards(node_id, lane_id, arc, states).into_iter()
        })
        .collect();
    trace!(" - traced {:?}", traces);

    // Find the common prefix and suffix of the traces.
    let shortest = traces.iter().map(Vec::len).min().unwrap();

    let mut prefix_len = 0;
    for i in 0..shortest {
        let state = traces[0][i];
        if traces.iter().any(|trace| trace[i] != state) {
            break;
        }
        prefix_len = i + 1;
    }
    assert!(prefix_len > 0, "prefix length should be at least one state, since the traces contain the initial common state");

    let mut suffix_len = 0;
    for i in 0..shortest {
        let state = traces[0][traces[0].len() - i - 1];
        if traces
            .iter()
            .any(|trace| trace[trace.len() - i - 1] != state)
        {
            break;
        }
        suffix_len = i + 1;
    }
    assert!(suffix_len > 0, "suffix length should be at least one state, since the traces contain the final common state");

    trace!(" - common prefix = {}, suffix = {}", prefix_len, suffix_len);

    // Wrap things up in a struct.
    LocalAmbiguity {
        first: traces[0][prefix_len - 1],
        last: traces[0][traces[0].len() - suffix_len],
        seqs: traces
            .into_iter()
            .map(|trace| trace[prefix_len..trace.len() - suffix_len].into())
            .collect(),
    }
}

fn trace_states_backwards(
    node: ConflictNodeId,
    lane: ConflictLaneId,
    arc: &ConflictArc,
    tail: Vec<ItemSetId>,
) -> Vec<Vec<ItemSetId>> {
    let lane = &arc[node][lane];
    let mut states = lane.states.clone();
    states.extend(tail.iter());
    if lane.ancestors.is_empty() {
        vec![states]
    } else {
        lane.ancestors
            .iter()
            .flat_map(|&(node_id, lane_id)| {
                trace_states_backwards(node_id, lane_id, arc, states.clone()).into_iter()
            })
            .collect()
    }
}

/// Propose a grammar change to resolve a local ambiguity.
pub fn resolve_local_ambiguity(
    ambig: &LocalAmbiguity,
    arc: &ConflictArc,
    grammar: &Grammar,
    item_sets: &ItemSets,
) {
    debug!("resolving {:?}", ambig);

    // Identify the common nonterminals.
    let nonterms = {
        let mut sets = ambig.seqs.iter().map(|seq| {
            item_sets[*seq.last().unwrap()]
                .items()
                .iter()
                .map(|item| grammar[item.rule].name())
                .collect::<HashSet<_>>()
        });
        let mut intersected = sets.next().unwrap();
        for set in sets {
            intersected.retain(|nt| set.contains(nt));
        }
        intersected
    };
    trace!(" - nonterminals {:?}", nonterms);

    // Identify the rules in the grammar that need to be unified.
    let unify_rules: IndexSet<_> = ambig
        .seqs
        .iter()
        .flat_map(|seq| item_sets[*seq.last().unwrap()].items().iter())
        .filter_map(|item| {
            let id = item.rule();
            if nonterms.contains(&grammar[id].name()) {
                Some(id)
            } else {
                None
            }
        })
        .collect();

    for &id in &unify_rules {
        trace!(" - unify {:?} {}", id, grammar[id].pretty(grammar));
    }

    // Find the common prefix and suffix among the rules to be unified.
    let shortest = unify_rules
        .iter()
        .map(|&id| grammar[id].symbols().len())
        .min()
        .unwrap();

    let prefix_len = (0..shortest)
        .take_while(|&i| {
            let mut iter = unify_rules.iter().cloned();
            let symbol = grammar[iter.next().unwrap()].symbols()[i];
            iter.all(|id| grammar[id].symbols()[i] == symbol)
        })
        .last()
        .map(|i| i + 1)
        .unwrap_or(0);

    let suffix_len = (0..shortest)
        .take_while(|&i| {
            let mut iter = unify_rules.iter().cloned();
            let symbols = grammar[iter.next().unwrap()].symbols();
            let symbol = symbols[symbols.len() - i - 1];
            iter.all(|id| grammar[id].symbols()[grammar[id].symbols().len() - i - 1] == symbol)
        })
        .last()
        .map(|i| i + 1)
        .unwrap_or(0);

    trace!(" - common prefix = {}, suffix = {}", prefix_len, suffix_len);

    // Compute the replacement rule.
    let unify_seqs: Vec<Vec<_>> = unify_rules
        .iter()
        .map(|&id| {
            let symbols = grammar[id].symbols();
            symbols[prefix_len..(symbols.len() - suffix_len)].into()
        })
        .collect();
    trace!(" - unify sequences {:?}", unify_seqs);
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
    origin: ItemSetId,
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
    reconverged: bool,
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
///
/// The lane keeps track of two separate lists of predecessors:
///
/// - *parents* is concerned with the stack and points to previous possible
///   stack states. If a lane has to backtrack beyond the initial point of
///   conflict due to a reduction, this list becomes empty. Use this to find all
///   possible stacks.
/// - *ancestors* is concerned with the sequence of states and points to the
///   lanes in previous nodes that spawned this lane. Use this to find all
///   possible state sequences.
///
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictLane {
    parents: Vec<(ConflictNodeId, ConflictLaneId)>,
    ancestors: Vec<(ConflictNodeId, ConflictLaneId)>,
    seq: Vec<ItemSetId>,
    states: Vec<ItemSetId>,
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
        write!(
            f,
            "{:?}{:?}->{:?}{:?}",
            self.ancestors, self.parents, self.states, self.seq
        )
    }
}

/// A point of reconvergence in a conflict arc.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reconv {
    at: (ConflictNodeId, ConflictLaneId),
    from: Vec<(ConflictNodeId, ConflictLaneId)>,
}

impl fmt::Debug for Reconv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}->{:?}", self.from, self.at)
    }
}

/// A local ambiguity in the grammar.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalAmbiguity {
    first: ItemSetId,
    seqs: Vec<Vec<ItemSetId>>,
    last: ItemSetId,
}
