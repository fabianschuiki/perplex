// Copyright (c) 2018 Fabian Schuiki
#![allow(dead_code)]

//! A lexer for grammar descriptions.

use std::iter::Peekable;

/// The tokens that may appear in a grammar description.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Code(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Period,
    Colon,
    Comma,
    Semicolon,
    Pipe,
}

impl Token {
    /// Return the identifier.
    ///
    /// Panics if the token is not an identifier.
    pub fn unwrap_ident(self) -> String {
        match self {
            Token::Ident(i) => i,
            _ => panic!("token {:?} is not an identifier", self),
        }
    }
    /// Return the code.
    ///
    /// Panics if the token is not a code.
    pub fn unwrap_code(self) -> String {
        match self {
            Token::Code(i) => i,
            _ => panic!("token {:?} is not a code", self),
        }
    }
}

/// The keywords that may appear in a grammar description.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Token,
    Epsilon,
    End,
}

/// A lexer for perplex grammar descriptions.
pub struct Lexer<T: Iterator<Item = (usize, char)>> {
    input: Peekable<T>,
}

impl<T: Iterator<Item = (usize, char)>> Lexer<T> {
    /// Create a new lexer.
    pub fn new(input: T) -> Lexer<T> {
        Lexer {
            input: input.peekable(),
        }
    }
}

/// Return the next non-whitespace input character, skipping comments.
fn next_relevant<I: Iterator<Item = (usize, char)>>(
    input: &mut Peekable<I>,
) -> Option<(usize, char)> {
    'outer: while let Some((p, c)) = input.next() {
        if c == '/' {
            match input.peek() {
                Some(&(_, '/')) => {
                    input.next();
                    while let Some((_, c)) = input.next() {
                        if c == '\n' {
                            continue 'outer;
                        }
                    }
                    return None; // end of input reached
                }
                Some(&(_, '*')) => {
                    input.next();
                    while let Some((_, c)) = input.next() {
                        if c != '*' {
                            continue;
                        }
                        if let Some(&(_, c)) = input.peek() {
                            if c == '/' {
                                input.next();
                                continue 'outer;
                            }
                        }
                    }
                    return None; // end of input reached
                }
                _ => (),
            }
        }
        if !c.is_whitespace() {
            return Some((p, c));
        }
    }
    None
}

/// Checks whether a character is a valid symbol in teh grammar description.
fn is_symbol(c: char) -> bool {
    match c {
        '(' | ')' | '.' | ':' | ',' | ';' | '|' => true,
        _ => false,
    }
}

impl<T: Iterator<Item = (usize, char)>> Iterator for Lexer<T> {
    type Item = (usize, usize, Token);

    fn next(&mut self) -> Option<(usize, usize, Token)> {
        // Fetch the first character and see what we can do with it.
        let (sp, sc) = match next_relevant(&mut self.input) {
            Some(x) => x,
            None => return None,
        };
        let mut sl = sp + sc.len_utf8();
        let tkn = match sc {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '.' => Token::Period,
            ':' => Token::Colon,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '|' => Token::Pipe,
            '\'' => {
                let mut buffer = String::new();
                buffer.push(sc);
                let mut escaped = false;
                while let Some((ep, ec)) = self.input.next() {
                    escaped = if buffer.ends_with('\\') && !escaped {
                        buffer.pop();
                        true
                    } else {
                        false
                    };
                    buffer.push(ec);
                    sl = ep + ec.len_utf8();
                    if ec == '\'' && !escaped {
                        break;
                    }
                }
                Token::Ident(buffer)
            }
            '`' => {
                let mut buffer = String::new();
                let mut escaped = false;
                while let Some((ep, ec)) = self.input.next() {
                    escaped = if buffer.ends_with('\\') && !escaped {
                        buffer.pop();
                        true
                    } else {
                        false
                    };
                    sl = ep + ec.len_utf8();
                    if ec == '`' && !escaped {
                        break;
                    }
                    buffer.push(ec);
                }
                Token::Code(buffer)
            }
            _ => {
                let mut buffer = String::new();
                buffer.push(sc);
                while let Some(&(ep, ec)) = self.input.peek() {
                    if ec.is_whitespace() || is_symbol(ec) {
                        break;
                    }
                    buffer.push(ec);
                    sl = ep + ec.len_utf8();
                    self.input.next();
                }
                match buffer.as_str() {
                    "token" => Token::Keyword(Keyword::Token),
                    "epsilon" => Token::Keyword(Keyword::Epsilon),
                    "end" => Token::Keyword(Keyword::End),
                    _ => Token::Ident(buffer),
                }
            }
        };
        Some((sp, sl, tkn))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Token::*;
    use super::Keyword as Kw;

    fn lex<S: AsRef<str>>(input: S) -> Vec<Token> {
        let lex = Lexer::new(input.as_ref().char_indices());
        lex.map(|(_, _, tkn)| tkn).collect()
    }

    #[test]
    fn tokens1() {
        assert_eq!(
            lex("token IDENT;"),
            vec![Keyword(Kw::Token), Ident("IDENT".into()), Semicolon]
        );
    }

    #[test]
    fn tokens2() {
        assert_eq!(
            lex("ident_list : ident_list ',' IDENT | IDENT ;"),
            vec![
                Ident("ident_list".into()),
                Colon,
                Ident("ident_list".into()),
                Ident("','".into()),
                Ident("IDENT".into()),
                Pipe,
                Ident("IDENT".into()),
                Semicolon,
            ]
        );
    }

    #[test]
    fn comment_single_line() {
        assert_eq!(lex("| // comment\n ; // comment"), vec![Pipe, Semicolon]);
    }

    #[test]
    fn comment_inline() {
        assert_eq!(lex("| /* comment */ ;"), vec![Pipe, Semicolon]);
    }

    #[test]
    fn comment_multiple_lines() {
        assert_eq!(lex("| /* comment \n comment */ ;"), vec![Pipe, Semicolon]);
    }

    #[test]
    fn quoted_escapes() {
        assert_eq!(
            lex("token 'some \\'stuff\\''"),
            vec![Keyword(Kw::Token), Ident("'some 'stuff''".into())]
        );
    }

    #[test]
    fn quoted_escapes_backslash() {
        assert_eq!(
            lex("token 'abc \\\\ def'"),
            vec![Keyword(Kw::Token), Ident("'abc \\ def'".into())]
        );
    }
}
