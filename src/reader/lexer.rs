use std::fmt;

type Result<T> = std::result::Result<T, String>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Location {
    offset: usize,
}

impl Location {
    pub(crate) fn new(offset: usize) -> Self {
        Self { offset }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "offset: {}", self.offset)
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum Token {
    EndList {
        start: Location,
    },
    EndMap {
        start: Location,
    },
    EndVector {
        start: Location,
    },
    Integer {
        value: String,
        start: Location,
        end: Location,
    },
    Keyword {
        name: String,
        start: Location,
        end: Location,
    },
    LineComment {
        comment: String,
        start: Location,
        end: Location,
    },
    StartList {
        start: Location,
    },
    StartMap {
        start: Location,
    },
    StartVector {
        start: Location,
    },
    String {
        value: String,
        start: Location,
        end: Location,
    },
    Symbol {
        name: String,
        start: Location,
        end: Location,
    },
}

impl Token {
    fn end_list(start: Location) -> Self {
        Self::EndList { start }
    }

    fn end_map(start: Location) -> Self {
        Self::EndMap { start }
    }

    fn end_vector(start: Location) -> Self {
        Self::EndVector { start }
    }

    fn integer(value: String, start: Location, end: Location) -> Self {
        Self::Integer { value, start, end }
    }

    fn keyword(name: String, start: Location, end: Location) -> Self {
        Self::Keyword { name, start, end }
    }

    fn line_comment(comment: String, start: Location, end: Location) -> Self {
        Self::LineComment {
            comment,
            start,
            end,
        }
    }

    fn start_list(start: Location) -> Self {
        Self::StartList { start }
    }

    fn start_map(start: Location) -> Self {
        Self::StartMap { start }
    }

    fn start_vector(start: Location) -> Self {
        Self::StartVector { start }
    }

    fn string(value: String, start: Location, end: Location) -> Self {
        Self::String { value, start, end }
    }

    fn symbol(name: String, start: Location, end: Location) -> Self {
        Self::Symbol { name, start, end }
    }
}

pub(super) struct Lexer;

impl Lexer {
    pub(super) fn from_str(src: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut current = Location::new(0);

        while current.offset < src.chars().count() {
            let remaining = &src[current.offset..];
            let parsed = Self::parse_whitespace(remaining, &mut tokens, &current)
                .or_else(|| Self::parse_start_list(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_start_map(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_start_vector(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_end_list(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_end_map(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_end_vector(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_symbol(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_integer(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_keyword(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_string(remaining, &mut tokens, &current))
                .or_else(|| Self::parse_line_comment(remaining, &mut tokens, &current));

            match parsed {
                Some(n) => current.offset += n,
                None => {
                    return Err(format!(
                        "reader: unknown character '{}'",
                        remaining.chars().next().unwrap(),
                    ))
                }
            }
        }

        Ok(tokens)
    }

    fn parse_end_list(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with(')') {
            tokens.push(Token::end_list(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_end_map(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with('}') {
            tokens.push(Token::end_map(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_end_vector(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with(']') {
            tokens.push(Token::end_vector(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_integer(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        let mut chars = src.chars();
        if chars.next().unwrap().is_numeric() {
            let parsed = chars.take_while(|c| c.is_numeric()).count();

            let mut end = current.clone();
            end.offset += parsed + 1;

            tokens.push(Token::integer(
                src[..parsed + 1].to_owned(),
                current.clone(),
                end,
            ));
            Some(parsed + 1)
        } else {
            None
        }
    }

    fn parse_keyword(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with(':') {
            let parsed = src
                .chars()
                .skip(1)
                .take_while(|c| c.is_alphanumeric() || ['-', '!'].contains(c))
                .count();

            let mut end = current.clone();
            end.offset += parsed + 1;

            tokens.push(Token::keyword(
                src[1..parsed + 1].to_owned(),
                current.clone(),
                end,
            ));
            Some(parsed + 1)
        } else {
            None
        }
    }

    fn parse_line_comment(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with(';') {
            let parsed = src.lines().next().map(str::len).unwrap_or(0);
            let mut end = current.clone();
            end.offset += parsed;

            tokens.push(Token::line_comment(
                src[1..parsed].to_owned(),
                current.clone(),
                end,
            ));
            Some(parsed)
        } else {
            None
        }
    }

    fn parse_start_list(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with('(') {
            tokens.push(Token::start_list(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_string(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with('"') {
            let parsed = src.chars().skip(1).take_while(|c| *c != '"').count();

            let mut end = current.clone();
            end.offset += parsed + 2;

            tokens.push(Token::string(
                src[1..parsed + 1].to_owned(),
                current.clone(),
                end,
            ));
            Some(parsed + 2)
        } else {
            None
        }
    }

    fn parse_symbol(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        let mut chars = src.chars();
        if chars.next().unwrap().is_alphabetic() {
            let parsed = chars
                .take_while(|c| c.is_alphanumeric() || ['-', '!'].contains(c))
                .count();

            let mut end = current.clone();
            end.offset += parsed + 1;

            tokens.push(Token::symbol(
                src[..parsed + 1].to_owned(),
                current.clone(),
                end,
            ));
            Some(parsed + 1)
        } else {
            None
        }
    }

    fn parse_start_map(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with('{') {
            tokens.push(Token::start_map(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_start_vector(src: &str, tokens: &mut Vec<Token>, current: &Location) -> Option<usize> {
        if src.starts_with('[') {
            tokens.push(Token::start_vector(current.clone()));
            Some(1)
        } else {
            None
        }
    }

    fn parse_whitespace(src: &str, _: &mut Vec<Token>, _: &Location) -> Option<usize> {
        let whitespaces = src.chars().take_while(|c| c.is_whitespace()).count();

        if whitespaces > 0 {
            Some(whitespaces)
        } else {
            None
        }
    }
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;

    pub(in crate::reader) fn end_list(start: usize) -> Token {
        Token::end_list(Location::new(start))
    }

    pub(in crate::reader) fn end_map(start: usize) -> Token {
        Token::end_map(Location::new(start))
    }

    pub(in crate::reader) fn end_vector(start: usize) -> Token {
        Token::end_vector(Location::new(start))
    }

    pub(in crate::reader) fn integer(value: impl Into<String>, start: usize, end: usize) -> Token {
        Token::integer(value.into(), Location::new(start), Location::new(end))
    }

    pub(in crate::reader) fn keyword(name: impl Into<String>, start: usize, end: usize) -> Token {
        Token::keyword(name.into(), Location::new(start), Location::new(end))
    }

    pub(in crate::reader) fn line_comment(
        comment: impl Into<String>,
        start: usize,
        end: usize,
    ) -> Token {
        Token::line_comment(comment.into(), Location::new(start), Location::new(end))
    }

    pub(in crate::reader) fn start_list(start: usize) -> Token {
        Token::start_list(Location::new(start))
    }

    pub(in crate::reader) fn start_map(start: usize) -> Token {
        Token::start_map(Location::new(start))
    }

    pub(in crate::reader) fn start_vector(start: usize) -> Token {
        Token::start_vector(Location::new(start))
    }

    pub(in crate::reader) fn string(value: impl Into<String>, start: usize, end: usize) -> Token {
        Token::string(value.into(), Location::new(start), Location::new(end))
    }

    pub(in crate::reader) fn symbol(name: impl Into<String>, start: usize, end: usize) -> Token {
        Token::symbol(name.into(), Location::new(start), Location::new(end))
    }

    #[test]
    fn test_end_list() {
        assert_eq!(Lexer::from_str(")"), Ok(vec![end_list(0)]));

        assert_eq!(Lexer::from_str("))"), Ok(vec![end_list(0), end_list(1)]));

        assert_eq!(Lexer::from_str(" ) )"), Ok(vec![end_list(1), end_list(3)]));
    }

    #[test]
    fn test_end_map() {
        assert_eq!(Lexer::from_str("}"), Ok(vec![end_map(0)]));

        assert_eq!(Lexer::from_str("}}"), Ok(vec![end_map(0), end_map(1)]));

        assert_eq!(Lexer::from_str(" } }"), Ok(vec![end_map(1), end_map(3)]));
    }

    #[test]
    fn test_end_vector() {
        assert_eq!(Lexer::from_str("]"), Ok(vec![end_vector(0)]));

        assert_eq!(
            Lexer::from_str("]]"),
            Ok(vec![end_vector(0), end_vector(1)])
        );

        assert_eq!(
            Lexer::from_str(" ] ]"),
            Ok(vec![end_vector(1), end_vector(3)])
        );
    }

    #[test]
    fn test_integer() {
        assert_eq!(Lexer::from_str("0"), Ok(vec![integer("0", 0, 1)]));

        assert_eq!(Lexer::from_str("123"), Ok(vec![integer("123", 0, 3)]));

        assert_eq!(
            Lexer::from_str("456 789"),
            Ok(vec![integer("456", 0, 3), integer("789", 4, 7)])
        );
    }

    #[test]
    fn test_keyword() {
        assert_eq!(
            Lexer::from_str(":keyword"),
            Ok(vec![keyword("keyword", 0, 8)])
        );

        assert_eq!(
            Lexer::from_str(":abc123"),
            Ok(vec![keyword("abc123", 0, 7)])
        );

        assert_eq!(
            Lexer::from_str(":1-2-3 :4-5-6"),
            Ok(vec![keyword("1-2-3", 0, 6), keyword("4-5-6", 7, 13)])
        );
    }

    #[test]
    fn test_line_comment() {
        assert_eq!(Lexer::from_str(";"), Ok(vec![line_comment("", 0, 1)]));

        assert_eq!(
            Lexer::from_str(";comment"),
            Ok(vec![line_comment("comment", 0, 8)])
        );

        assert_eq!(
            Lexer::from_str("; comment"),
            Ok(vec![line_comment(" comment", 0, 9)])
        );

        assert_eq!(
            Lexer::from_str("; comment;another comment"),
            Ok(vec![line_comment(" comment;another comment", 0, 25)])
        );

        assert_eq!(
            Lexer::from_str("; comment ; another comment"),
            Ok(vec![line_comment(" comment ; another comment", 0, 27)])
        );

        assert_eq!(
            Lexer::from_str(
                "; comment
                 ; another comment"
            ),
            Ok(vec![
                line_comment(" comment", 0, 9),
                line_comment(" another comment", 27, 44)
            ])
        );
    }

    #[test]
    fn test_start_list() {
        assert_eq!(Lexer::from_str("("), Ok(vec![start_list(0)]));

        assert_eq!(
            Lexer::from_str("(("),
            Ok(vec![start_list(0), start_list(1)])
        );

        assert_eq!(
            Lexer::from_str(" ( ("),
            Ok(vec![start_list(1), start_list(3)])
        );
    }

    #[test]
    fn test_start_map() {
        assert_eq!(Lexer::from_str("{"), Ok(vec![start_map(0)]));

        assert_eq!(Lexer::from_str("{{"), Ok(vec![start_map(0), start_map(1)]));

        assert_eq!(
            Lexer::from_str(" { {"),
            Ok(vec![start_map(1), start_map(3)])
        );
    }

    #[test]
    fn test_start_vector() {
        assert_eq!(Lexer::from_str("["), Ok(vec![start_vector(0)]));

        assert_eq!(
            Lexer::from_str("[["),
            Ok(vec![start_vector(0), start_vector(1)])
        );

        assert_eq!(
            Lexer::from_str(" [ ["),
            Ok(vec![start_vector(1), start_vector(3)])
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(Lexer::from_str(r#""""#), Ok(vec![string("", 0, 2)]));

        assert_eq!(
            Lexer::from_str(r#""string""#),
            Ok(vec![string("string", 0, 8)])
        );

        assert_eq!(
            Lexer::from_str(r#""abc 123""#),
            Ok(vec![string("abc 123", 0, 9)])
        );

        assert_eq!(
            Lexer::from_str(r#""hello" "world""#),
            Ok(vec![string("hello", 0, 7), string("world", 8, 15)])
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(Lexer::from_str("symbol"), Ok(vec![symbol("symbol", 0, 6)]));

        assert_eq!(Lexer::from_str("abc123"), Ok(vec![symbol("abc123", 0, 6)]));

        assert_eq!(
            Lexer::from_str("hello world"),
            Ok(vec![symbol("hello", 0, 5), symbol("world", 6, 11)])
        );
    }
}
