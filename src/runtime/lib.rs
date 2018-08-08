// Copyright (c) 2018 Fabian Schuiki

//! A collection of facilities required by perplex-generated parsers.

#![deny(missing_docs)]

/// A parser that can be passed to generated states.
///
/// The code generator emits a list of state and goto functions which are
/// generic over the parser passed to them. This trait describes the minimal set
/// of functionality that such a parser must support.
pub trait Parser {
    /// The type of terminals the parser emits.
    type Terminal;
    /// The type of nonterminals the parser emits. This is likely an enum over
    /// all nonterminals generated automatically.
    type Nonterminal;

    /// Peek at the next terminal in the sequence without shifting it.
    #[inline(always)]
    fn peek(&mut self) -> &Self::Terminal;

    /// Push the next terminal onto the stack.
    ///
    /// This function is called by shift actions in the parser.
    #[inline(always)]
    fn shift(&mut self, state_fn: fn(&mut Self), goto_fn: fn(&mut Self, Self::Nonterminal));

    /// Push a nonterminal onto the stack.
    ///
    /// This function is called after a reduce action in the parser.
    #[inline(always)]
    fn goto(
        &mut self,
        nonterminal: Self::Nonterminal,
        state_fn: fn(&mut Self),
        goto_fn: fn(&mut Self, Self::Nonterminal),
    );

    /// Reduce the tail of the stack to a nonterminal.
    ///
    /// The parser should split the last `length` symbols off the stack and call
    /// the given function `f` with them as argument. This function is called by
    /// the parser to initiate a reduction.
    #[inline(always)]
    fn reduce<F: Fn(Vec<Symbol<Self::Terminal, Self::Nonterminal>>) -> Self::Nonterminal>(
        &mut self,
        length: usize,
        f: F,
    );

    /// Accept the last symbol on the stack as the parse result.
    ///
    /// Called by the parser when it has finished parsing the root rule.
    fn accept(&mut self);
}

/// A symbol on the parse stack.
///
/// Generic over the type of terminal `T` and nonterminal `NT`.
#[allow(missing_docs)]
pub enum Symbol<T, NT> {
    Terminal(T),
    Nonterminal(NT),
}

impl<T, NT> Symbol<T, NT> {
    /// Return the terminal wrapped in this symbol.
    ///
    /// Panics if the symbol is not a terminal.
    #[inline(always)]
    pub fn unwrap_terminal(self) -> T {
        match self {
            Symbol::Terminal(x) => x,
            _ => panic!("symbol is not a terminal"),
        }
    }

    /// Return the nonterminal wrapped in this symbol.
    ///
    /// Panics if the symbol is not a nonterminal.
    #[inline(always)]
    pub fn unwrap_nonterminal(self) -> NT {
        match self {
            Symbol::Nonterminal(x) => x,
            _ => panic!("symbol is not a nonterminal"),
        }
    }
}

/// A description of a parser's state space.
pub trait StateSpace {
    /// The type of terminals the parser emits.
    type Terminal;
    /// The type of nonterminals the parser emits.
    type Nonterminal;
    /// The type of the reduction produce by the root rule.
    type Root;

    /// Return the root state function.
    fn root_state_fn<P: Parser<Terminal = Self::Terminal, Nonterminal = Self::Nonterminal>>(
) -> fn(&mut P);

    /// Return the root goto function.
    fn root_goto_fn<P: Parser<Terminal = Self::Terminal, Nonterminal = Self::Nonterminal>>(
) -> fn(&mut P, Self::Nonterminal);
}

/// A parser state machine.
///
/// This struct implements the parse stack and driver for the parser. It is
/// generic over the means by which terminals arrive at the input.
pub struct ParserMachine<I: ParserInput, S: StateSpace> {
    input: I,
    result: Option<S::Nonterminal>,
    stack: Vec<StackEntry<I, S>>,
}

/// An entry on the parser stack.
///
/// Associates a terminal or nonterminal with a state/goto function pair. These
/// are used to drive the parser forward.
struct StackEntry<I: ParserInput, S: StateSpace> {
    symbol: Symbol<S::Terminal, S::Nonterminal>,
    state_fn: fn(&mut ParserMachine<I, S>),
    goto_fn: fn(&mut ParserMachine<I, S>, S::Nonterminal),
}

impl<I: ParserInput<Item = S::Terminal>, S: StateSpace> ParserMachine<I, S> {
    /// Create a new parser state machine.
    pub fn new(input: I) -> ParserMachine<I, S> {
        ParserMachine {
            input: input,
            result: None,
            stack: Vec::new(),
        }
    }

    fn step(&mut self) {
        if self.result.is_some() {
            return;
        }
        let state_fn = self.stack
            .last()
            .map(|e| e.state_fn)
            .unwrap_or(S::root_state_fn());
        state_fn(self);
    }

    /// Run the parser to completion.
    pub fn run(mut self) -> S::Nonterminal {
        while self.result.is_none() {
            self.step()
        }
        self.result.unwrap()
    }
}

impl<I: Iterator, S: StateSpace<Terminal = Option<I::Item>>> ParserMachine<IterInput<I>, S> {
    /// Create a new parser state machine from an iterator.
    pub fn from_iter(input: I) -> ParserMachine<IterInput<I>, S> {
        ParserMachine::new(IterInput::new(input))
    }
}

impl<I, S> Parser for ParserMachine<I, S>
where
    I: ParserInput<Item = S::Terminal>,
    S: StateSpace,
{
    type Terminal = S::Terminal;
    type Nonterminal = S::Nonterminal;

    fn peek(&mut self) -> &S::Terminal {
        self.input.peek()
    }

    fn shift(&mut self, state_fn: fn(&mut Self), goto_fn: fn(&mut Self, S::Nonterminal)) {
        let terminal = self.input.next();
        self.stack.push(StackEntry {
            symbol: Symbol::Terminal(terminal),
            state_fn: state_fn,
            goto_fn: goto_fn,
        });
    }

    fn goto(
        &mut self,
        nonterminal: S::Nonterminal,
        state_fn: fn(&mut Self),
        goto_fn: fn(&mut Self, S::Nonterminal),
    ) {
        self.stack.push(StackEntry {
            symbol: Symbol::Nonterminal(nonterminal),
            state_fn: state_fn,
            goto_fn: goto_fn,
        });
    }

    fn reduce<F: Fn(Vec<Symbol<S::Terminal, S::Nonterminal>>) -> S::Nonterminal>(
        &mut self,
        length: usize,
        f: F,
    ) {
        let at = self.stack.len() - length;
        let args = self.stack.drain(at..).map(|e| e.symbol).collect();
        let goto_fn = self.stack
            .last()
            .map(|e| e.goto_fn)
            .unwrap_or(S::root_goto_fn());
        let nonterminal = f(args);
        goto_fn(self, nonterminal);
    }

    fn accept(&mut self) {
        self.result = Some(self.stack.pop().unwrap().symbol.unwrap_nonterminal());
    }
}

/// An stream of tokens that can be used as parser input.
pub trait ParserInput {
    /// The token type produced by the stream.
    type Item;

    /// Peek at the next item in the stream.
    #[inline(always)]
    fn peek(&mut self) -> &Self::Item;

    /// Consume the next item in the stream.
    #[inline(always)]
    fn next(&mut self) -> Self::Item;

    /// Returns the end-of-stream marker.
    ///
    /// The stream will continue to produce this marker once reaching the end.
    /// Code calling peek() or next() must compare the result to this marker to
    /// dedect the end of the input.
    #[inline(always)]
    fn marker() -> Self::Item;
}

/// A wrapper for using iterators as parser input.
pub struct IterInput<I: Iterator> {
    iter: I,
    peek: Option<I::Item>,
}

impl<I: Iterator> IterInput<I> {
    /// Create a new iterator input.
    pub fn new(mut iter: I) -> IterInput<I> {
        IterInput {
            peek: iter.next(),
            iter: iter,
        }
    }
}

impl<I: Iterator> ParserInput for IterInput<I> {
    type Item = Option<I::Item>;

    fn peek(&mut self) -> &Self::Item {
        &self.peek
    }

    fn next(&mut self) -> Self::Item {
        std::mem::replace(&mut self.peek, self.iter.next())
    }

    fn marker() -> Self::Item {
        None
    }
}
