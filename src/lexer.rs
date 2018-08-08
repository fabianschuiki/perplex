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
    LParen,
    RParen,
    Period,
    Colon,
    Comma,
    Semicolon,
    Pipe,
}

/// The keywords that may appear in a grammar description.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Token,
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

/// Return the next non-whitespace input character.
fn next_relevant<I: Iterator<Item = (usize, char)>>(input: &mut I) -> Option<(usize, char)> {
    while let Some((p, c)) = input.next() {
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
            '.' => Token::Period,
            ':' => Token::Colon,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '|' => Token::Pipe,
            '\'' => {
                let mut buffer = String::new();
                buffer.push(sc);
                while let Some((ep, ec)) = self.input.next() {
                    buffer.push(ec);
                    sl = ep + ec.len_utf8();
                    if ec == '\'' {
                        break;
                    }
                }
                Token::Ident(buffer)
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

    #[test]
    fn tokens1() {
        let input = "token IDENT;";
        let lex = Lexer::new(input.char_indices());
        let tkn: Vec<_> = lex.map(|(_, _, tkn)| tkn).collect();
        assert_eq!(
            format!("{:?}", tkn),
            "[Keyword(Token), Ident(\"IDENT\"), Semicolon]"
        );
    }

    #[test]
    fn tokens2() {
        let input = "ident_list : ident_list ',' IDENT | IDENT ;";
        let lex = Lexer::new(input.char_indices());
        let tkn: Vec<_> = lex.map(|(_, _, tkn)| tkn).collect();
        assert_eq!(
            format!("{:?}", tkn),
            "[Ident(\"ident_list\"), Colon, Ident(\"ident_list\"), Ident(\"\\',\\'\"), Ident(\"IDENT\"), Pipe, Ident(\"IDENT\"), Semicolon]"
        );
    }
}
