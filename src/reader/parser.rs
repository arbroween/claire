use super::lexer::{Location, Token};
use std::fmt;
use std::string::String as RString;

type Result<T> = std::result::Result<T, RString>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Span {
    start: Location,
    end: Location,
}

impl Span {
    pub(crate) fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} to {}", self.start, self.end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Comment {
    span: Span,
    comment: RString,
}

impl Comment {
    fn new(span: Span, comment: RString) -> Self {
        Self { span, comment }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Keyword {
    span: Span,
    name: RString,
}

impl Keyword {
    pub(crate) fn new(span: Span, name: RString) -> Self {
        Self { span, name }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ":{}", self.name)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Integer {
    span: Span,
    value: i32,
}

impl Integer {
    pub(crate) fn new(span: Span, value: i32) -> Self {
        Self { span, value }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct String {
    span: Span,
    value: RString,
}

impl String {
    pub(crate) fn new(span: Span, value: RString) -> Self {
        Self { span, value }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Symbol {
    span: Span,
    name: RString,
}

impl Symbol {
    pub(crate) fn new(span: Span, name: RString) -> Self {
        Self { span, name }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ReaderData {
    Commented {
        comments: Vec<Comment>,
        data: Box<Self>,
    },
    Integer(Integer),
    Keyword(Keyword),
    List {
        span: Span,
        items: Vec<Self>,
    },
    Map {
        span: Span,
        items: Vec<Self>,
    },
    String(String),
    Symbol(Symbol),
    Vector {
        span: Span,
        items: Vec<Self>,
    },
}

impl ReaderData {
    fn commented(data: Self, comments: &mut Vec<Comment>) -> Self {
        if comments.is_empty() {
            data
        } else {
            Self::Commented {
                comments: comments.drain(..).collect(),
                data: Box::new(data),
            }
        }
    }

    fn integer(span: Span, value: i32) -> Self {
        Self::Integer(Integer::new(span, value))
    }

    fn keyword(span: Span, name: RString) -> Self {
        Self::Keyword(Keyword::new(span, name))
    }

    fn list(span: Span, items: Vec<Self>) -> Self {
        Self::List { span, items }
    }

    fn map(span: Span, items: Vec<Self>) -> Self {
        Self::Map { span, items }
    }

    fn string(span: Span, value: RString) -> Self {
        Self::String(String::new(span, value))
    }

    fn symbol(span: Span, name: RString) -> Self {
        Self::Symbol(Symbol::new(span, name))
    }

    fn vector(span: Span, items: Vec<Self>) -> Self {
        Self::Vector { span, items }
    }

    fn fmt_list(f: &mut fmt::Formatter<'_>, _: &Span, items: &[ReaderData]) -> fmt::Result {
        let mut output: RString = "(".to_owned();
        if !items.is_empty() {
            output.push_str(&items[0].to_string());
            for item in &items[1..] {
                output.push(' ');
                output.push_str(&item.to_string());
            }
        }
        output.push(')');
        write!(f, "{}", output)
    }

    fn fmt_map(f: &mut fmt::Formatter<'_>, _: &Span, items: &[ReaderData]) -> fmt::Result {
        let mut output: RString = "{".to_owned();
        if !items.is_empty() {
            output.push_str(&items[0].to_string());
            for item in &items[1..] {
                output.push(' ');
                output.push_str(&item.to_string());
            }
        }
        output.push('}');
        write!(f, "{}", output)
    }

    fn fmt_vector(f: &mut fmt::Formatter<'_>, _: &Span, items: &[ReaderData]) -> fmt::Result {
        let mut output: RString = "[".to_owned();
        if !items.is_empty() {
            output.push_str(&items[0].to_string());
            for item in &items[1..] {
                output.push(' ');
                output.push_str(&item.to_string());
            }
        }
        output.push(']');
        write!(f, "{}", output)
    }

    pub(crate) fn span(&self) -> &Span {
        match self {
            Self::Commented { data, .. } => data.span(),
            Self::Integer(integer) => integer.span(),
            Self::Keyword(keyword) => keyword.span(),
            Self::List { span, .. } => span,
            Self::Map { span, .. } => span,
            Self::String(string) => string.span(),
            Self::Symbol(symbol) => symbol.span(),
            Self::Vector { span, .. } => span,
        }
    }
}

impl fmt::Display for ReaderData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Commented { data, .. } => write!(f, "{}", data),
            Self::Integer(integer) => write!(f, "{}", integer),
            Self::Keyword(keyword) => write!(f, "{}", keyword),
            Self::List { span, items } => Self::fmt_list(f, span, items),
            Self::Map { span, items } => Self::fmt_map(f, span, items),
            Self::String(string) => write!(f, "{}", string),
            Self::Symbol(symbol) => write!(f, "{}", symbol),
            Self::Vector { span, items } => Self::fmt_vector(f, span, items),
        }
    }
}

pub(super) struct Parser;

impl Parser {
    pub(super) fn parse(tokens: &mut Vec<Token>) -> Result<ReaderData> {
        let mut comments = Vec::new();
        tokens.reverse();
        loop {
            match tokens.pop() {
                Some(Token::StartList { start }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_list(tokens, start)?,
                        &mut comments,
                    ));
                }
                Some(Token::StartMap { start }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_map(tokens, start)?,
                        &mut comments,
                    ));
                }
                Some(Token::StartVector { start }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_vector(tokens, start)?,
                        &mut comments,
                    ));
                }
                Some(Token::Symbol { name, start, end }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_symbol(name, start, end)?,
                        &mut comments,
                    ));
                }
                Some(Token::Integer { value, start, end }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_integer(value, start, end)?,
                        &mut comments,
                    ));
                }
                Some(Token::Keyword { name, start, end }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_keyword(name, start, end)?,
                        &mut comments,
                    ));
                }
                Some(Token::String { value, start, end }) => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        Self::parse_string(value, start, end)?,
                        &mut comments,
                    ));
                }
                Some(Token::LineComment {
                    comment,
                    start,
                    end,
                }) => {
                    comments.push(Comment::new(Span::new(start, end), comment));
                }
                Some(Token::EndList { start }) => {
                    return Err(format!("ParseError: unexpected ')' at {}", start));
                }
                Some(Token::EndMap { start }) => {
                    return Err(format!("ParseError: unexpected '}}' at {}", start));
                }
                Some(Token::EndVector { start }) => {
                    return Err(format!("ParseError: unexpected ']' at {}", start));
                }
                None => return Err("ParseError: empty input".to_owned()),
            }
        }
    }

    fn parse_integer(value: RString, start: Location, end: Location) -> Result<ReaderData> {
        let span = Span::new(start, end);
        match value.parse() {
            Ok(value) => Ok(ReaderData::integer(span, value)),
            Err(err) => Err(format!(
                "ParseError: unexpected invalid integer literal at {}: {}",
                span, err
            )),
        }
    }

    fn parse_keyword(name: RString, start: Location, end: Location) -> Result<ReaderData> {
        Ok(ReaderData::keyword(Span::new(start, end), name))
    }

    fn parse_list(tokens: &mut Vec<Token>, start: Location) -> Result<ReaderData> {
        let mut comments = Vec::new();
        let mut items: Vec<ReaderData> = Vec::new();

        tokens.reverse();
        while let Some(token) = tokens.pop() {
            match token {
                Token::Symbol { name, start, end } => {
                    items.push(Self::parse_symbol(name, start, end)?)
                }
                Token::Integer { value, start, end } => {
                    items.push(Self::parse_integer(value, start, end)?)
                }
                Token::Keyword { name, start, end } => {
                    items.push(Self::parse_keyword(name, start, end)?)
                }
                Token::String { value, start, end } => {
                    items.push(Self::parse_string(value, start, end)?)
                }
                Token::EndList { start: end } => {
                    tokens.reverse();
                    return Ok(ReaderData::list(Span::new(start, end), items));
                }
                Token::StartList { start } => {
                    tokens.reverse();
                    items.push(Self::parse_list(tokens, start)?);
                    tokens.reverse();
                }
                Token::StartMap { start } => {
                    tokens.reverse();
                    items.push(Self::parse_map(tokens, start)?);
                    tokens.reverse();
                }
                Token::StartVector { start } => {
                    tokens.reverse();
                    items.push(Self::parse_vector(tokens, start)?);
                    tokens.reverse();
                }
                Token::LineComment {
                    comment,
                    start,
                    end,
                } => {
                    comments.push(Comment::new(Span::new(start, end), comment));
                }
                Token::EndMap { start: end } => {
                    return Err(format!(
                        "ParseError: list starting at {} expected ')', found lone '}}' at {}",
                        start, end
                    ))
                }
                Token::EndVector { start: end } => {
                    return Err(format!(
                        "ParseError: list starting at {} expected ')', found lone ']' at {}",
                        start, end
                    ))
                }
            }
        }

        Err("ParseError: expected ')', found end of input".to_owned())
    }

    fn parse_map(tokens: &mut Vec<Token>, start: Location) -> Result<ReaderData> {
        let mut comments = Vec::new();
        let mut items: Vec<ReaderData> = Vec::new();

        tokens.reverse();
        while let Some(token) = tokens.pop() {
            match token {
                Token::Symbol { name, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_symbol(name, start, end)?,
                        &mut comments,
                    ));
                }
                Token::Integer { value, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_integer(value, start, end)?,
                        &mut comments,
                    ));
                }
                Token::Keyword { name, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_keyword(name, start, end)?,
                        &mut comments,
                    ));
                }
                Token::String { value, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_string(value, start, end)?,
                        &mut comments,
                    ));
                }
                Token::EndMap { start: end } => {
                    tokens.reverse();
                    if items.len() % 2 == 0 {
                        return Ok(ReaderData::commented(
                            ReaderData::map(Span::new(start, end), items),
                            &mut comments,
                        ));
                    } else {
                        let last_key = items.pop().unwrap();
                        return Err(format!("ParseError: map starting at {} expected key value pairs, found lone key {} at {}", start, last_key, last_key.span()));
                    }
                }
                Token::StartList { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_list(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::StartMap { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_map(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::StartVector { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_vector(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::LineComment {
                    comment,
                    start,
                    end,
                } => {
                    comments.push(Comment::new(Span::new(start, end), comment));
                }
                Token::EndList { start: end } => {
                    return Err(format!(
                        "ParseError: map starting at {} expected '}}', found lone ')' at {}",
                        start, end
                    ))
                }
                Token::EndVector { start: end } => {
                    return Err(format!(
                        "ParseError: map starting at {} expected '}}', found lone ']' at {}",
                        start, end
                    ))
                }
            }
        }

        Err("ParseError: expected '}', found end of input".to_owned())
    }

    fn parse_string(value: RString, start: Location, end: Location) -> Result<ReaderData> {
        Ok(ReaderData::string(Span::new(start, end), value))
    }

    fn parse_symbol(name: RString, start: Location, end: Location) -> Result<ReaderData> {
        Ok(ReaderData::symbol(Span::new(start, end), name))
    }

    fn parse_vector(tokens: &mut Vec<Token>, start: Location) -> Result<ReaderData> {
        let mut comments = Vec::new();
        let mut items: Vec<ReaderData> = Vec::new();

        tokens.reverse();
        while let Some(token) = tokens.pop() {
            match token {
                Token::Symbol { name, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_symbol(name, start, end)?,
                        &mut comments,
                    ));
                }
                Token::Integer { value, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_integer(value, start, end)?,
                        &mut comments,
                    ));
                }
                Token::Keyword { name, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_keyword(name, start, end)?,
                        &mut comments,
                    ));
                }
                Token::String { value, start, end } => {
                    items.push(ReaderData::commented(
                        Self::parse_string(value, start, end)?,
                        &mut comments,
                    ));
                }
                Token::EndVector { start: end } => {
                    tokens.reverse();
                    return Ok(ReaderData::commented(
                        ReaderData::vector(Span::new(start, end), items),
                        &mut comments,
                    ));
                }
                Token::StartList { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_list(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::StartMap { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_map(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::StartVector { start } => {
                    tokens.reverse();
                    items.push(ReaderData::commented(
                        Self::parse_vector(tokens, start)?,
                        &mut comments,
                    ));
                    tokens.reverse();
                }
                Token::LineComment {
                    comment,
                    start,
                    end,
                } => {
                    comments.push(Comment::new(Span::new(start, end), comment));
                }
                Token::EndList { start: end } => {
                    return Err(format!(
                        "ParseError: vector starting at {} expected ']', found lone ')' at {}",
                        start, end
                    ))
                }
                Token::EndMap { start: end } => {
                    return Err(format!(
                        "ParseError: vector starting at {} expected ']', found lone '}}' at {}",
                        start, end
                    ))
                }
            }
        }

        Err("ParseError: expected ']', found end of input".to_owned())
    }
}

#[cfg(test)]
pub(in crate::reader) mod tests {
    use super::super::lexer::tests::{self as token};
    use super::*;

    pub(in crate::reader) fn comment(
        comment: impl Into<RString>,
        start: usize,
        end: usize,
    ) -> Comment {
        Comment::new(
            Span::new(Location::new(start), Location::new(end)),
            comment.into(),
        )
    }

    pub(in crate::reader) fn commented(comments: Vec<Comment>, data: ReaderData) -> ReaderData {
        let mut comments = comments;
        ReaderData::commented(data, &mut comments)
    }

    pub(in crate::reader) fn integer(value: i32, start: usize, end: usize) -> ReaderData {
        ReaderData::integer(Span::new(Location::new(start), Location::new(end)), value)
    }

    pub(in crate::reader) fn keyword(
        name: impl Into<RString>,
        start: usize,
        end: usize,
    ) -> ReaderData {
        ReaderData::keyword(
            Span::new(Location::new(start), Location::new(end)),
            name.into(),
        )
    }

    pub(in crate::reader) fn list(items: Vec<ReaderData>, start: usize, end: usize) -> ReaderData {
        ReaderData::list(Span::new(Location::new(start), Location::new(end)), items)
    }

    pub(in crate::reader) fn map(items: Vec<ReaderData>, start: usize, end: usize) -> ReaderData {
        ReaderData::map(Span::new(Location::new(start), Location::new(end)), items)
    }

    pub(in crate::reader) fn string(
        value: impl Into<RString>,
        start: usize,
        end: usize,
    ) -> ReaderData {
        ReaderData::string(
            Span::new(Location::new(start), Location::new(end)),
            value.into(),
        )
    }

    pub(in crate::reader) fn symbol(
        name: impl Into<RString>,
        start: usize,
        end: usize,
    ) -> ReaderData {
        ReaderData::symbol(
            Span::new(Location::new(start), Location::new(end)),
            name.into(),
        )
    }

    pub(in crate::reader) fn vector(
        items: Vec<ReaderData>,
        start: usize,
        end: usize,
    ) -> ReaderData {
        ReaderData::vector(Span::new(Location::new(start), Location::new(end)), items)
    }

    #[test]
    fn test_integer() {
        let mut tokens = vec![token::integer("0", 0, 1)];
        assert_eq!(Parser::parse(&mut tokens), Ok(integer(0, 0, 1)));

        let mut tokens = vec![token::integer("123", 5, 8)];
        assert_eq!(Parser::parse(&mut tokens), Ok(integer(123, 5, 8)));
    }

    #[test]
    fn test_keyword() {
        let mut tokens = vec![token::keyword("keyword", 1, 8)];
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("keyword", 1, 8)));

        let mut tokens = vec![
            token::keyword("too", 7, 11),
            token::keyword("many", 13, 18),
            token::keyword("keywords", 21, 30),
            token::keyword("right", 31, 37),
            token::keyword("now", 39, 43),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("too", 7, 11)));
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("many", 13, 18)));
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("keywords", 21, 30)));
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("right", 31, 37)));
        assert_eq!(Parser::parse(&mut tokens), Ok(keyword("now", 39, 43)));
    }

    #[test]
    fn test_list() {
        let mut tokens = vec![token::start_list(0), token::end_list(1)];
        assert_eq!(Parser::parse(&mut tokens), Ok(list(Vec::new(), 0, 1)));

        let mut tokens = vec![
            token::start_list(0),
            token::start_list(1),
            token::end_list(2),
            token::end_list(3),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(list(vec![list(Vec::new(), 1, 2)], 0, 3))
        );

        let mut tokens = vec![
            token::start_list(0),
            token::end_list(1),
            token::start_list(2),
            token::end_list(3),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(list(Vec::new(), 0, 1)));
        assert_eq!(Parser::parse(&mut tokens), Ok(list(Vec::new(), 2, 3)));

        let mut tokens = vec![
            token::start_list(0),
            token::symbol("symbol", 1, 7),
            token::end_list(7),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(list(vec![symbol("symbol", 1, 7)], 0, 7))
        );

        let mut tokens = vec![
            token::start_list(0),
            token::integer("1", 1, 2),
            token::integer("23", 3, 5),
            token::end_list(6),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(list(vec![integer(1, 1, 2), integer(23, 3, 5)], 0, 6))
        );
    }

    #[test]
    fn test_map() {
        let mut tokens = vec![token::start_map(0), token::end_map(1)];
        assert_eq!(Parser::parse(&mut tokens), Ok(map(Vec::new(), 0, 1)));

        let mut tokens = vec![
            token::start_map(0),
            token::start_map(1),
            token::end_map(2),
            token::start_map(4),
            token::end_map(5),
            token::end_map(6),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(map(
                vec![map(Vec::new(), 1, 2), map(Vec::new(), 4, 5)],
                0,
                6
            ))
        );

        let mut tokens = vec![
            token::start_map(0),
            token::end_map(1),
            token::start_map(2),
            token::end_map(3),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(map(Vec::new(), 0, 1)));
        assert_eq!(Parser::parse(&mut tokens), Ok(map(Vec::new(), 2, 3)));

        let mut tokens = vec![
            token::start_map(0),
            token::symbol("key", 1, 4),
            token::symbol("value", 5, 10),
            token::end_map(10),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(map(
                vec![symbol("key", 1, 4), symbol("value", 5, 10)],
                0,
                10
            ))
        );

        let mut tokens = vec![
            token::start_map(0),
            token::integer("1", 1, 2),
            token::integer("23", 3, 5),
            token::integer("456", 6, 9),
            token::integer("7890", 10, 14),
            token::end_map(14),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(map(
                vec![
                    integer(1, 1, 2),
                    integer(23, 3, 5),
                    integer(456, 6, 9),
                    integer(7890, 10, 14),
                ],
                0,
                14
            ))
        );
    }

    #[test]
    fn test_string() {
        let mut tokens = vec![token::string("", 0, 2)];
        assert_eq!(Parser::parse(&mut tokens), Ok(string("", 0, 2)));

        let mut tokens = vec![token::string("string", 0, 8)];
        assert_eq!(Parser::parse(&mut tokens), Ok(string("string", 0, 8)));

        let mut tokens = vec![
            token::string("one", 7, 12),
            token::string("string", 14, 22),
            token::string("then", 25, 31),
            token::string("a", 34, 37),
            token::string("second", 39, 47),
            token::string("finally", 48, 57),
            token::string("a", 61, 64),
            token::string("third", 66, 73),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(string("one", 7, 12)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("string", 14, 22)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("then", 25, 31)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("a", 34, 37)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("second", 39, 47)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("finally", 48, 57)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("a", 61, 64)));
        assert_eq!(Parser::parse(&mut tokens), Ok(string("third", 66, 73)));
    }

    #[test]
    fn test_symbol() {
        let mut tokens = vec![token::symbol("symbol", 0, 6)];
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("symbol", 0, 6)));

        let mut tokens = vec![
            token::symbol("many", 7, 11),
            token::symbol("symbols", 13, 20),
            token::symbol("one", 23, 26),
            token::symbol("at", 28, 30),
            token::symbol("a", 34, 35),
            token::symbol("time", 36, 40),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("many", 7, 11)));
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("symbols", 13, 20)));
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("one", 23, 26)));
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("at", 28, 30)));
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("a", 34, 35)));
        assert_eq!(Parser::parse(&mut tokens), Ok(symbol("time", 36, 40)));
    }

    #[test]
    fn test_vector() {
        let mut tokens = vec![token::start_vector(0), token::end_vector(1)];
        assert_eq!(Parser::parse(&mut tokens), Ok(vector(Vec::new(), 0, 1)));

        let mut tokens = vec![
            token::start_vector(0),
            token::start_vector(1),
            token::end_vector(2),
            token::end_vector(3),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(vector(vec![vector(Vec::new(), 1, 2)], 0, 3))
        );

        let mut tokens = vec![
            token::start_vector(0),
            token::end_vector(1),
            token::start_vector(2),
            token::end_vector(3),
        ];
        assert_eq!(Parser::parse(&mut tokens), Ok(vector(Vec::new(), 0, 1)));
        assert_eq!(Parser::parse(&mut tokens), Ok(vector(Vec::new(), 2, 3)));

        let mut tokens = vec![
            token::start_vector(0),
            token::symbol("symbol", 1, 7),
            token::end_vector(7),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(vector(vec![symbol("symbol", 1, 7)], 0, 7))
        );

        let mut tokens = vec![
            token::start_vector(0),
            token::integer("1", 1, 2),
            token::integer("23", 3, 5),
            token::end_vector(6),
        ];
        assert_eq!(
            Parser::parse(&mut tokens),
            Ok(vector(vec![integer(1, 1, 2), integer(23, 3, 5)], 0, 6))
        );
    }
}
