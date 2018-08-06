// Copyright (c) 2018 Fabian Schuiki

//! Representation of a parsing state machine and tree structure.

use std;
use std::fmt;
use std::collections::{BTreeMap, VecDeque};
use std::ops::{Index, IndexMut};

use grammar::{RuleId, Symbol};
use item_set::{self, ItemSetId, ItemSets};

/// A state machine.
pub struct StateMachine {
    states: Vec<State>,
}

/// A parser state.
///
/// This is basically a set of mappings from symbols to actions.
pub struct State {
    id: StateId,
    item_set: ItemSetId,
    actions: BTreeMap<Symbol, Action>,
}

/// An action to be taken upon encountering a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    /// Shift the symbol and go to the given state.
    Shift(StateId),
    /// Reduce with the given rule.
    Reduce(RuleId),
}

impl StateMachine {
    /// Create a state machine from a list of item sets.
    pub fn try_from(item_sets: &ItemSets) -> Result<StateMachine, Vec<String>> {
        let root_state_id = StateId::from_usize(0);
        let root_item_set_id = ItemSetId::from_usize(0);
        let mut mapping = BTreeMap::<ItemSetId, StateId>::new();
        mapping.insert(root_item_set_id, root_state_id);
        let mut sm = StateMachine {
            states: vec![
                State {
                    id: root_state_id,
                    item_set: root_item_set_id,
                    actions: BTreeMap::new(),
                },
            ],
        };
        let mut todo = VecDeque::<StateId>::new();
        todo.push_back(root_state_id);
        let mut issues = Vec::new();

        while let Some(state_id) = todo.pop_front() {
            let is = sm[state_id].item_set;
            let mut actions = BTreeMap::new();
            for &(symbol, action) in item_sets[is].actions() {
                let action = match action {
                    item_set::Action::Shift(is) => if let Some(&target_sid) = mapping.get(&is) {
                        Action::Shift(target_sid)
                    } else {
                        let id = StateId::from_usize(sm.states.len());
                        mapping.insert(is, id);
                        sm.states.push(State {
                            id: id,
                            item_set: is,
                            actions: BTreeMap::new(),
                        });
                        todo.push_back(id);
                        Action::Shift(id)
                    },
                    item_set::Action::Reduce(r) => Action::Reduce(r),
                };
                if let Some(existing) = actions.insert(symbol, action) {
                    if existing != action {
                        issues.push(format!("conflict between {:?} and {:?}", existing, action));
                    }
                }
            }
            sm[state_id].actions = actions;
        }

        if issues.is_empty() {
            Ok(sm)
        } else {
            Err(issues)
        }
    }

    /// All states in the state machine.
    pub fn states(&self) -> States {
        States(self.states.iter())
    }
}

impl Index<StateId> for StateMachine {
    type Output = State;

    fn index(&self, index: StateId) -> &State {
        &self.states[index.as_usize()]
    }
}

impl IndexMut<StateId> for StateMachine {
    fn index_mut(&mut self, index: StateId) -> &mut State {
        &mut self.states[index.as_usize()]
    }
}

impl State {
    /// Get the unique identifier of this state.
    pub fn id(&self) -> StateId {
        self.id
    }

    /// An iterator over the terminals and associated actions.
    pub fn actions(&self) -> Actions {
        Actions(self.actions.iter())
    }
}

/// An iterator over the states of a state machine.
pub struct States<'a>(std::slice::Iter<'a, State>);

impl<'a> Iterator for States<'a> {
    type Item = &'a State;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

/// An iterator over the actions of a state.
pub struct Actions<'a>(std::collections::btree_map::Iter<'a, Symbol, Action>);

impl<'a> Iterator for Actions<'a> {
    type Item = (Symbol, Action);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(&s, &a)| (s, a))
    }
}

/// A unique state identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateId(usize);

impl StateId {
    /// Create a state id from a usize.
    pub fn from_usize(id: usize) -> StateId {
        StateId(id)
    }

    /// Obtain the id as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}

impl fmt::Display for StateId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
