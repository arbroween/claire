#![warn(clippy::all)]

use core::convert::TryInto;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

mod analyzer;
mod reader;
mod vm;

#[derive(Clone, Debug)]
struct Location {
    offset: usize,
}

impl Location {
    fn new(offset: usize) -> Self {
        Self { offset }
    }
}

const _KEYWORDS: [&str; 51] = [
    "and",
    "begin",
    "case",
    "case-lambda",
    "cond",
    "cond-expand",
    "define",
    "define-library",
    "define-record-type",
    "define-syntax",
    "define-values",
    "delay",
    "delay-force",
    "do",
    "else",
    "except",
    "export",
    "guard",
    "if",
    "import",
    "include",
    "include-ci",
    "include-library-declarations",
    "lambda",
    "let",
    "let*",
    "letrec",
    "letrec*",
    "letrec-syntax",
    "let-syntax",
    "let-values",
    "let*-values",
    "library",
    "not",
    "only",
    "or",
    "parametrize",
    "quasiquote",
    "quote",
    "raise",
    "rename",
    "set!",
    "syntax-error",
    "syntax-rules",
    "unless",
    "unquote",
    "unquote-splicing",
    "when",
    "=>",
    "_",
    "...",
];

#[derive(Debug)]
enum Token {
    Atom {
        ident: String,
        start: Location,
        end: Location,
    },
    CloseParen {
        start: Location,
    },
    LineComment {
        comment: String,
        start: Location,
        end: Location,
    },
    OpenParen {
        start: Location,
    },
    OpenVector {
        start: Location,
    },
}

impl Token {
    fn ident(ident: &str, start: Location, end: Location) -> Self {
        Self::Atom {
            ident: ident.to_owned(),
            start,
            end,
        }
    }

    fn close_paren(start: Location) -> Self {
        Self::CloseParen { start }
    }

    fn line_comment(comment: String, start: Location, end: Location) -> Self {
        Self::LineComment {
            comment,
            start,
            end,
        }
    }

    fn open_paren(start: Location) -> Self {
        Self::OpenParen { start }
    }

    fn open_vector(start: Location) -> Self {
        Self::OpenVector { start }
    }

    fn from_str(src: &str) -> Vec<Self> {
        let mut current = Location::new(0);
        let mut tokens = Vec::new();
        while current.offset < src.chars().count() {
            let parsed_chars = match parse_whitespace(&src[current.offset..]) {
                None => match parse_line_comment(&src[current.offset..], &current, &mut tokens) {
                    None => {
                        match parse_open_vector(&src[current.offset..], &current, &mut tokens) {
                            None => {
                                match parse_open_paren(
                                    &src[current.offset..],
                                    &current,
                                    &mut tokens,
                                ) {
                                    None => {
                                        match parse_atom(
                                            &src[current.offset..],
                                            &current,
                                            &mut tokens,
                                        ) {
                                            None => match parse_close_paren(
                                                &src[current.offset..],
                                                &current,
                                                &mut tokens,
                                            ) {
                                                None => panic!(
                                                    "Could not parse {} at {}",
                                                    src, current.offset
                                                ),
                                                Some(n) => n,
                                            },
                                            Some(n) => n,
                                        }
                                    }
                                    Some(n) => n,
                                }
                            }
                            Some(n) => n,
                        }
                    }
                    Some(n) => n,
                },
                Some(n) => n,
            };
            current.offset += parsed_chars;
        }
        tokens
    }
}

fn parse_atom<'src>(src: &'src str, start: &Location, tokens: &mut Vec<Token>) -> Option<usize> {
    let ident: String = src
        .chars()
        .take_while(|c| {
            c.is_alphanumeric()
                || [
                    '!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^',
                    '_', '~', /* added to support #t et #f */ '#',
                ]
                .contains(c)
        })
        .collect();
    match ident.len() {
        0 => None,
        len => {
            let mut end = start.clone();
            end.offset += len;
            tokens.push(Token::ident(&src[..len], start.clone(), end));
            Some(len)
        }
    }
}

fn parse_close_paren(src: &str, start: &Location, tokens: &mut Vec<Token>) -> Option<usize> {
    if src.starts_with(')') {
        tokens.push(Token::close_paren(start.clone()));
        Some(1)
    } else {
        None
    }
}

fn parse_line_comment(src: &str, start: &Location, tokens: &mut Vec<Token>) -> Option<usize> {
    if src.starts_with(';') {
        let comment: String = src[1..].lines().take(1).collect();
        let comment_len = comment.len() + 1;
        let mut end = start.clone();
        end.offset += comment_len;
        tokens.push(Token::line_comment(comment, start.clone(), end));
        Some(comment_len)
    } else {
        None
    }
}

fn parse_open_paren(src: &str, start: &Location, tokens: &mut Vec<Token>) -> Option<usize> {
    if src.starts_with('(') {
        tokens.push(Token::open_paren(start.clone()));
        Some(1)
    } else {
        None
    }
}

fn parse_open_vector(src: &str, start: &Location, tokens: &mut Vec<Token>) -> Option<usize> {
    if src.starts_with("#(") {
        tokens.push(Token::open_vector(start.clone()));
        Some(2)
    } else {
        None
    }
}

fn parse_whitespace(src: &str) -> Option<usize> {
    let whitespaces: String = src.chars().take_while(|c| c.is_whitespace()).collect();
    match whitespaces.len() {
        0 => None,
        len => Some(len),
    }
}

type Result<T> = core::result::Result<T, String>;

#[derive(Clone, Debug)]
struct Span {
    start: Location,
    end: Location,
}

impl Span {
    fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
enum SExpr {
    Atom { ident: String, span: Span },
    List { exprs: Vec<SExpr>, span: Span },
    Vector { exprs: Vec<SExpr>, span: Span },
}

impl SExpr {
    fn ident(ident: String, start: Location, end: Location) -> Self {
        Self::Atom {
            ident,
            span: Span::new(start, end),
        }
    }

    fn list(exprs: Vec<Self>, start: Location, end: Location) -> Self {
        Self::List {
            exprs,
            span: Span::new(start, end),
        }
    }

    fn vector(exprs: Vec<Self>, start: Location, end: Location) -> Self {
        Self::Vector {
            exprs,
            span: Span::new(start, end),
        }
    }

    fn from_str(tokens: &mut Vec<Token>) -> Result<Self> {
        tokens.reverse();
        match tokens.pop() {
            None => Err("empty input".to_owned()),
            Some(Token::Atom { ident, start, end }) => Ok(SExpr::ident(ident, start, end)),
            Some(Token::CloseParen { .. }) => Err("Invalid ')' before any '('".to_owned()),
            Some(Token::LineComment { .. }) => {
                tokens.reverse();
                Self::from_str(tokens)
            }
            Some(Token::OpenParen { start }) => Self::parse_list(start, tokens),
            Some(Token::OpenVector { start }) => Self::parse_vector(start, tokens),
        }
    }

    fn parse_list(start: Location, tokens: &mut Vec<Token>) -> Result<Self> {
        let mut exprs = Vec::new();
        let mut end = None;
        while let Some(token) = tokens.pop() {
            match token {
                Token::Atom { ident, start, end } => exprs.push(SExpr::ident(ident, start, end)),
                Token::CloseParen { start } => {
                    end = Some(start);
                    break;
                }
                Token::LineComment { .. } => {}
                Token::OpenParen { start } => exprs.push(SExpr::parse_list(start, tokens)?),
                Token::OpenVector { start } => exprs.push(SExpr::parse_vector(start, tokens)?),
            }
        }
        match end {
            None => Err(format!("paren opened at {} is never closed", start.offset)),
            Some(end) => Ok(SExpr::list(exprs, start, end)),
        }
    }

    fn parse_vector(start: Location, tokens: &mut Vec<Token>) -> Result<Self> {
        let mut exprs = Vec::new();
        let mut end = None;
        while let Some(token) = tokens.pop() {
            match token {
                Token::Atom { ident, start, end } => exprs.push(SExpr::ident(ident, start, end)),
                Token::CloseParen { start } => {
                    end = Some(start);
                    break;
                }
                Token::LineComment { .. } => {}
                Token::OpenParen { start } => exprs.push(SExpr::parse_list(start, tokens)?),
                Token::OpenVector { start } => exprs.push(SExpr::parse_vector(start, tokens)?),
            }
        }
        match end {
            None => Err(format!("vector opened at {} is never closed", start.offset)),
            Some(end) => Ok(SExpr::vector(exprs, start, end)),
        }
    }
}

#[derive(Clone, Debug)]
enum Expr {
    Begin {
        span: Span,
        exprs: Vec<Expr>,
    },
    Call {
        span: Span,
        operator: Box<Expr>,
        operands: Vec<Expr>,
    },
    FnDefinition {
        span: Span,
        ident: String,
        params: Vec<String>,
        body: Box<Expr>,
    },
    If {
        span: Span,
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Option<Box<Expr>>,
    },
    Integer {
        span: Span,
        value: i32,
    },
    Lambda {
        span: Span,
        params: Vec<String>,
        body: Box<Expr>,
    },
    Null {
        span: Span,
    },
    RecordDefinition {
        span: Span,
        name: String,
        pred: String,
        constructor: String,
        fields: Vec<String>,
    },
    Var {
        span: Span,
        ident: String,
    },
    VarDefinition {
        span: Span,
        ident: String,
        value: Box<Expr>,
    },
    Vector {
        span: Span,
        exprs: Vec<Expr>,
    },
}

impl Expr {
    fn from_sexpr(sexpr: SExpr) -> Result<Self> {
        match sexpr {
            SExpr::Atom { span, ident } => Self::parse_atom(span, ident),
            SExpr::List { span, exprs } => Self::parse_list(span, exprs),
            SExpr::Vector { span, exprs } => Self::parse_vector(span, exprs),
        }
    }

    fn parse_atom(span: Span, ident: String) -> Result<Self> {
        match ident {
            _ if ident.chars().all(char::is_numeric) => Ok(Self::Integer {
                span,
                value: ident.parse().unwrap(),
            }),
            _ => Ok(Self::Var { span, ident }),
        }
    }

    fn parse_list(span: Span, sexprs: Vec<SExpr>) -> Result<Self> {
        let mut sexprs = sexprs;
        if sexprs.is_empty() {
            Err("Invalid empty list expression".to_owned())
        } else {
            sexprs.reverse();
            match sexprs.pop().unwrap() {
                SExpr::Atom { ident, .. } if ident == "begin" => {
                    if sexprs.is_empty() {
                        Err("Invalid begin expression without arguments".to_owned())
                    } else {
                        sexprs.reverse();
                        Self::parse_begin(span, sexprs)
                    }
                }
                SExpr::Atom { ident, .. } if ident == "define" => match sexprs.pop() {
                    None => Err("Invalid definition".to_owned()),
                    Some(SExpr::Atom { ident, .. }) => {
                        sexprs.reverse();
                        Self::parse_var_definition(span, ident, sexprs)
                    }
                    Some(SExpr::List { mut exprs, .. }) => {
                        exprs.reverse();
                        match exprs.pop() {
                            None => Err(
                                "Invalid function definition without identifier nor parameters"
                                    .to_owned(),
                            ),
                            Some(SExpr::Atom { ident, .. }) => {
                                exprs.reverse();
                                sexprs.reverse();
                                Self::parse_fn_definition(span, ident, exprs, sexprs)
                            }
                            Some(expr @ SExpr::List { .. }) => Err(format!(
                                "Invalid identifier for function definition : {:?}",
                                expr
                            )),
                            Some(expr @ SExpr::Vector { .. }) => Err(format!(
                                "Invalid identifier for function definition : {:?}",
                                expr
                            )),
                        }
                    }
                    Some(expr @ SExpr::Vector { .. }) => Err(format!(
                        "Invalid identifier for variable definition: {:?}",
                        expr
                    )),
                },
                SExpr::Atom { ident, .. } if ident == "define-record-type" => {
                    sexprs.reverse();
                    Self::parse_record_definition(span, sexprs)
                }
                SExpr::Atom { ident, .. } if ident == "if" => {
                    sexprs.reverse();
                    Self::parse_if(span, sexprs)
                }
                SExpr::Atom { ident, .. } if ident == "lambda" => match sexprs.pop() {
                    Some(SExpr::List { exprs, .. }) => {
                        sexprs.reverse();
                        Self::parse_lambda(span, exprs, sexprs)
                    }
                    Some(SExpr::Atom { .. }) => Err(
                        "Invalid lambda expression with single expression instead of parameters list"
                            .to_owned(),
                    ),
                    Some(SExpr::Vector {..}) => Err("Invalid lambda expression with vector literal instead of parameters list".to_owned()),
                    None => Err("Invalid lambda expression without parameters list nor body".to_owned()),
                },
                operator => {
                    sexprs.reverse();
                    Self::parse_call(span, operator, sexprs)
                }
            }
        }
    }

    fn parse_begin(span: Span, sexprs: Vec<SExpr>) -> Result<Self> {
        let exprs: Result<Vec<_>> = sexprs.into_iter().map(Self::from_sexpr).collect();
        Ok(Self::Begin {
            span,
            exprs: exprs?,
        })
    }

    fn parse_call(span: Span, operator: SExpr, operands: Vec<SExpr>) -> Result<Self> {
        let operands: Result<Vec<_>> = operands.into_iter().map(Self::from_sexpr).collect();

        Ok(Self::Call {
            span,
            operator: Box::new(Self::from_sexpr(operator)?),
            operands: operands?,
        })
    }

    fn parse_if(span: Span, sexprs: Vec<SExpr>) -> Result<Self> {
        let mut sexprs = sexprs;
        sexprs.reverse();
        match (sexprs.pop(), sexprs.pop(), sexprs.pop()) {
            (Some(test), Some(consequent), Some(alternate)) => {
                if !sexprs.is_empty() {
                    return Err(
                        "Invalid if expression with too many expressions in its body".to_owned(),
                    );
                }

                Ok(Self::If {
                    span,
                    test: Box::new(Self::from_sexpr(test)?),
                    consequent: Box::new(Self::from_sexpr(consequent)?),
                    alternate: Some(Box::new(Self::from_sexpr(alternate)?)),
                })
            }
            (Some(test), Some(consequent), None) => Ok(Self::If {
                span,
                test: Box::new(Self::from_sexpr(test)?),
                consequent: Box::new(Self::from_sexpr(consequent)?),
                alternate: None,
            }),
            (Some(_), None, None) => {
                Err("Invalid if expression without branch expressions".to_owned())
            }
            (None, None, None) => {
                Err("Invalid if expression without test nor branch expressions".to_owned())
            }
            _ => unreachable!(),
        }
    }

    fn parse_lambda(span: Span, params: Vec<SExpr>, body: Vec<SExpr>) -> Result<Self> {
        let params: Vec<Result<_>> = params.into_iter().map(Self::from_sexpr).collect();
        if params.iter().any(|param| match param {
            Ok(Self::Var { .. }) => false,
            _ => true,
        }) {
            return Err("Invalid parameter in lambda expression".to_owned());
        }

        let mut body = body;
        match body.len() {
            0 => Err("Invalid lambda expression without body".to_owned()),
            1 => Ok(Self::Lambda {
                span,
                params: params
                    .into_iter()
                    .map(|param| match param {
                        Ok(Self::Var { ident, .. }) => ident,
                        _ => unreachable!(),
                    })
                    .collect(),
                body: Box::new(Self::from_sexpr(body.pop().unwrap())?),
            }),
            _ => Ok(Self::Lambda {
                span: span.clone(),
                params: params
                    .into_iter()
                    .map(|param| match param {
                        Ok(Self::Var { ident, .. }) => ident,
                        _ => unreachable!(),
                    })
                    .collect(),
                body: Box::new(Self::parse_begin(span, body)?),
            }),
        }
    }

    fn parse_fn_definition(
        span: Span,
        ident: String,
        params: Vec<SExpr>,
        body: Vec<SExpr>,
    ) -> Result<Self> {
        let params: Vec<Result<_>> = params.into_iter().map(Self::from_sexpr).collect();
        if params.iter().any(|param| match param {
            Ok(Self::Var { .. }) => false,
            _ => true,
        }) {
            return Err("Invalid parameter in function definition".to_owned());
        }

        let mut body = body;
        match body.len() {
            0 => Err("Invalid function definition without body".to_owned()),
            1 => Ok(Self::FnDefinition {
                span,
                ident,
                params: params
                    .into_iter()
                    .map(|param| match param {
                        Ok(Self::Var { ident, .. }) => ident,
                        _ => unreachable!(),
                    })
                    .collect(),
                body: Box::new(Self::from_sexpr(body.pop().unwrap())?),
            }),
            _ => Ok(Self::FnDefinition {
                span: span.clone(),
                ident,
                params: params
                    .into_iter()
                    .map(|param| match param {
                        Ok(Self::Var { ident, .. }) => ident,
                        _ => unreachable!(),
                    })
                    .collect(),
                body: Box::new(Self::parse_begin(span, body)?),
            }),
        }
    }

    fn parse_constructor_list(sexprs: Vec<SExpr>) -> Result<(String, Vec<String>)> {
        let mut sexprs = sexprs;
        sexprs.reverse();
        match sexprs.pop() {
            Some(SExpr::Atom { ident: constructor, .. }) => {
                sexprs.reverse();
                let mut fields = Vec::with_capacity(sexprs.len());

                for sexpr in sexprs {
                    match sexpr {
                        SExpr::Atom { ident, .. } => {
                            fields.push(ident);
                        }
                        _ => return Err(format!("Invalid record definition: expected the constructor list to only contains identifiers, but found {:?}", sexpr)),
                    }
                }

                Ok((constructor, fields))
            }
            Some(expr) => Err(format!("Invalid record definition: expected the constructor list to start with the constructor name, but found {:?}", expr)),
            None => Err("Invalid record definition: expected the constructor list to start with the constructor name, but found an empty list".to_owned()),
        }
    }

    fn parse_field_definition(sexpr: SExpr) -> Result<(String, Option<String>, Option<String>)> {
        match sexpr {
            SExpr::List { mut exprs, .. } => {
                exprs.reverse();
                match exprs.pop() {
                    Some(SExpr::Atom { ident, .. }) => {
                        Ok((ident, None, None))
                    }
                    Some(expr) => Err(format!("Invalid record definition: expected field definition list to start with an identifier, but found {:?}", expr)),
                    None => Err("Invalid record definition: expected field definition list to start with an identifier, but found empty list".to_owned()),
                }
            }
            _ => Err(format!(
                "Invalid record definition: expected field definition to be a list but found {:?}",
                sexpr
            )),
        }
    }

    fn parse_record_definition(span: Span, sexprs: Vec<SExpr>) -> Result<Self> {
        let mut sexprs = sexprs;
        sexprs.reverse();
        match (sexprs.pop(), sexprs.pop(), sexprs.pop()) {
            (
                Some(SExpr::Atom { ident: name, .. }),
                Some(SExpr::List { exprs, .. }),
                Some(SExpr::Atom { ident: pred, .. }),
            ) => {
                let (constructor, fields) = Self::parse_constructor_list(exprs)?;
                sexprs.reverse();
                let field_definitions: Result<Vec<_>> = sexprs.into_iter().map(Self::parse_field_definition).collect();
                let mut field_definitions = field_definitions?;

                dbg!(&fields);
                dbg!(&field_definitions);
                if let Some((name, _, _)) = field_definitions.iter().find(|(name, _, _)| !fields.contains(name)) {
                    return Err(format!("Invalid record definition: the field '{}' is declared but does not appears in the constructor list", name));
                }

                for field in fields.iter() {
                    match field_definitions.iter().position(|(name, _, _)| field == name) {
                        Some(index) => {
                            let (_, _accessor, _modifier) = field_definitions.remove(index);
                        }
                        None => return Err(format!("Invalid record definition: the field '{}' appears in the constructor list but is not declared", field)),
                    }
                }

                Ok(Expr::RecordDefinition { span, name, pred, constructor, fields })
            }
            (
                Some(SExpr::Atom { .. }),
                Some(expr),
                Some(SExpr::Atom { .. }),
            ) => Err(format!(
                "Invalid record definition: expected constructor list, but found {:?}",
                expr
            )),
            (
                Some(SExpr::Atom { .. }),
                None,
                Some(SExpr::Atom { .. }),
            ) => Err("Invalid record definition: missing constructor list".to_owned()),
            (Some(SExpr::Atom { .. }), Some(expr), _) => Err(format!(
                "Invalid record definition: expected identifier for record predicate, but found {:?}",
                expr
            )),
            (
                Some(SExpr::Atom { .. }),
                _,
                None,
            ) => Err("Invalid record definition: missing identifier for record predicate".to_owned()),
            (Some(expr), _, _) => Err(format!(
                "Invalid record definition: expected identifier for record name, but found {:?}",
                expr
            )),
            (
                None,
                _,
                _,
            ) => Err("Invalid record definition: missing identifier for record name".to_owned()),
        }
    }

    fn parse_var_definition(span: Span, ident: String, sexprs: Vec<SExpr>) -> Result<Self> {
        let mut sexprs = sexprs;
        sexprs.reverse();
        match sexprs.pop() {
            Some(value) => {
                if sexprs.is_empty() {
                    Ok(Self::VarDefinition {
                        span,
                        ident,
                        value: Box::new(Self::from_sexpr(value)?),
                    })
                } else {
                    Err("Invalid variable definition has more than 1 value expression".to_owned())
                }
            }
            None => Err("Invalid variable definition without value expression".to_owned()),
        }
    }

    fn parse_vector(span: Span, sexprs: Vec<SExpr>) -> Result<Self> {
        let exprs: Result<Vec<_>> = sexprs.into_iter().map(Self::from_sexpr).collect();
        Ok(Self::Vector {
            span,
            exprs: exprs?,
        })
    }
}

#[derive(Clone, Debug)]
enum Value {
    Boolean(bool),
    Integer(i32),
    Lambda {
        params: Vec<String>,
        env: Env,
        body: Box<Expr>,
    },
    NativeFn(fn(Vec<Value>) -> Result<Value>),
    Null,
    Vector(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(false) => write!(f, "#f"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Integer(value) => write!(f, "{}", value),
            Self::Lambda {
                params,
                env: _,
                body,
            } => write!(f, "(lambda ({}) {:?})", params.join(" "), *body),
            Self::NativeFn(_) => write!(f, "#<native function>"),
            Self::Null => write!(f, "'()"),
            Self::Vector(elements) => {
                let elements: Vec<_> = elements.iter().map(ToString::to_string).collect();
                write!(f, "#({})", elements.join(" "))
            }
        }
    }
}

impl Value {
    const FALSE: Self = Self::Boolean(false);
    const NULL: Self = Self::Null;
    const TRUE: Self = Self::Boolean(true);

    fn is_falsy(&self) -> bool {
        match self {
            Value::Boolean(false) => true,
            _ => false,
        }
    }

    fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(false) => false,
            _ => true,
        }
    }

    fn is_vector(&self) -> bool {
        match self {
            Value::Vector(_) => true,
            _ => false,
        }
    }
}

impl From<bool> for Value {
    fn from(boolean: bool) -> Self {
        if boolean {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }
}

#[derive(Clone, Debug)]
struct Env {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    outer: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            outer: None,
        }
    }

    fn with_builtins() -> Self {
        let env = Env::new();

        // boolean
        env.set("#f".to_owned(), Value::FALSE).unwrap();
        env.set("#t".to_owned(), Value::TRUE).unwrap();
        env.set("and".to_owned(), Value::NativeFn(and)).unwrap();
        env.set("boolean?".to_owned(), Value::NativeFn(boolean_qmark))
            .unwrap();
        env.set("not".to_owned(), Value::NativeFn(not)).unwrap();
        env.set("or".to_owned(), Value::NativeFn(or)).unwrap();

        // integer
        env.set("=".to_owned(), Value::NativeFn(eq)).unwrap();
        env.set("<".to_owned(), Value::NativeFn(lt)).unwrap();
        env.set(">".to_owned(), Value::NativeFn(gt)).unwrap();
        env.set("<=".to_owned(), Value::NativeFn(le)).unwrap();
        env.set(">=".to_owned(), Value::NativeFn(ge)).unwrap();
        env.set("+".to_owned(), Value::NativeFn(add)).unwrap();
        env.set("-".to_owned(), Value::NativeFn(sub)).unwrap();
        env.set("*".to_owned(), Value::NativeFn(mul)).unwrap();
        env.set("/".to_owned(), Value::NativeFn(div)).unwrap();
        env.set("eqv?".to_owned(), Value::NativeFn(eqv_qmark))
            .unwrap();
        env.set("exact?".to_owned(), Value::NativeFn(exact_qmark))
            .unwrap();
        env.set("integer?".to_owned(), Value::NativeFn(integer_qmark))
            .unwrap();
        env.set("modulo".to_owned(), Value::NativeFn(modulo))
            .unwrap();

        // pair
        env.set("null?".to_owned(), Value::NativeFn(null_qmark))
            .unwrap();

        // vector
        env.set("vector?".to_owned(), Value::NativeFn(vector_qmark))
            .unwrap();
        env.set("vector-length".to_owned(), Value::NativeFn(vector_length))
            .unwrap();

        env
    }

    fn get(&self, name: &str) -> Option<Value> {
        match self.bindings.borrow().get(name).cloned() {
            value @ Some(_) => value,
            None => self.outer.as_deref().and_then(|outer| outer.get(name)),
        }
    }

    fn local_env(&self) -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            outer: Some(Box::new(self.clone())),
        }
    }

    fn set(&self, name: String, value: Value) -> Result<()> {
        self.bindings.borrow_mut().insert(name, value);
        Ok(())
    }
}

struct Interpreter;

impl Interpreter {
    fn new() -> Self {
        Self
    }

    fn execute(&mut self, env: &mut Env, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Begin { mut exprs, .. } => {
                exprs.reverse();
                match exprs.pop() {
                    Some(tail_expr) => {
                        exprs.reverse();
                        if let Some(err) = exprs
                            .into_iter()
                            .find_map(|expr| self.execute(env, expr).err())
                        {
                            Err(format!(
                                "Error when evaluating expressions in begin: {}",
                                err
                            ))
                        } else {
                            self.execute(env, tail_expr)
                        }
                    }
                    None => Ok(Value::NULL),
                }
            }
            Expr::Call {
                operator, operands, ..
            } => match self.execute(env, *operator) {
                Ok(Value::Lambda {
                    params,
                    env: lambda_env,
                    body,
                }) => {
                    if params.len() == operands.len() {
                        let args: Result<Vec<_>> = operands
                            .into_iter()
                            .map(|operand| self.execute(env, operand))
                            .collect();
                        let args = args?;
                        let mut lambda_env = lambda_env.local_env();
                        params
                            .into_iter()
                            .zip(args.into_iter())
                            .for_each(|(ident, value)| lambda_env.set(ident, value).unwrap());
                        self.execute(&mut lambda_env, *body)
                    } else {
                        return Err(format!(
                            "function expected {} arguments, got {}",
                            params.len(),
                            operands.len()
                        ));
                    }
                }
                Ok(Value::NativeFn(function)) => {
                    let args: Result<Vec<_>> = operands
                        .into_iter()
                        .map(|operand| self.execute(env, operand))
                        .collect();
                    return function(args?);
                }
                Ok(value) => {
                    return Err(format!("Invalid call expression with operator '{}'", value))
                }
                Err(err) => {
                    return Err(format!(
                        "Error when evaluating operator in call expression: {}",
                        err
                    ))
                }
            },
            Expr::FnDefinition {
                ident,
                params,
                body,
                ..
            } => {
                env.set(
                    ident,
                    Value::Lambda {
                        params,
                        env: env.clone(),
                        body,
                    },
                )?;
                return Ok(Value::NULL);
            }
            Expr::If {
                test,
                consequent,
                alternate,
                ..
            } => match self.execute(env, *test) {
                Ok(Value::Boolean(false)) => {
                    if let Some(tail_expr) = alternate {
                        self.execute(env, *tail_expr)
                    } else {
                        return Ok(Value::NULL);
                    }
                }
                Ok(_) => self.execute(env, *consequent),
                Err(err) => {
                    return Err(format!(
                        "Error when evaluating test in if expression: {}",
                        err,
                    ))
                }
            },
            Expr::Integer { value, .. } => return Ok(Value::Integer(value)),
            Expr::Lambda { params, body, .. } => {
                return Ok(Value::Lambda {
                    params,
                    env: env.clone(),
                    body,
                })
            }
            Expr::Null { .. } => return Ok(Value::NULL),
            Expr::RecordDefinition { .. } => return Ok(Value::NULL),
            Expr::Var { ident, .. } => match env.get(&ident) {
                Some(value) => return Ok(value),
                None => return Err(format!("Variable with name '{}' is not defined", ident)),
            },
            Expr::VarDefinition { ident, value, .. } => {
                let value = self.execute(env, *value)?;
                env.set(ident, value)?;
                return Ok(Value::NULL);
            }
            Expr::Vector { exprs, .. } => {
                let elements: Result<Vec<_>> = exprs
                    .into_iter()
                    .map(|expr| self.execute(env, expr))
                    .collect();
                return Ok(Value::Vector(elements?));
            }
        }
    }
}

// boolean

fn boolean_qmark(args: Vec<Value>) -> Result<Value> {
    match args.len() {
        0 => Err("Invalid call to function 'boolean?' without an argument".to_owned()),
        1 => {
            let mut args = args;
            match args.pop().unwrap() {
                Value::Boolean(_) => Ok(Value::TRUE),
                _ => Ok(Value::FALSE),
            }
        }
        n => Err(format!(
            "Invalid call to function 'boolean?': expected 1 argument, got {} arguments",
            n
        )),
    }
}

fn and(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match args.pop() {
        Some(tail_expr) => {
            args.reverse();
            if args.iter().all(Value::is_truthy) {
                Ok(tail_expr)
            } else {
                Ok(Value::FALSE)
            }
        }
        None => Ok(Value::FALSE),
    }
}

fn not(args: Vec<Value>) -> Result<Value> {
    match args.len() {
        0 => Err("Invalid call to function 'not' without an argument".to_owned()),
        1 => {
            let mut args = args;
            Ok(args.pop().unwrap().is_falsy().into())
        }
        n => Err(format!(
            "Invalid call to function 'not': expected 1 argument, got {} arguments",
            n
        )),
    }
}

fn or(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match args.pop() {
        Some(tail_expr) => {
            args.reverse();
            if let Some(arg) = args.into_iter().find(|arg| arg.is_truthy()) {
                Ok(arg)
            } else {
                Ok(tail_expr)
            }
        }
        None => Ok(Value::FALSE),
    }
}

// integer

fn add(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok(Value::Integer(l + r)),
        (Some(l @ Value::Integer(_)), None) => Ok(l),
        _ => Err("Invalid arguments to function +".to_owned()),
    }
}

fn div(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok(Value::Integer(l / r)),
        _ => Err("Invalid arguments to function /".to_owned()),
    }
}

fn eq(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok((l == r).into()),
        _ => Err("Invalid arguments to function =".to_owned()),
    }
}

fn eqv_qmark(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Boolean(l)), Some(Value::Boolean(r))) if l == r => Ok(Value::TRUE),
        (Some(Value::Integer(l)), Some(Value::Integer(r))) if l == r => Ok(Value::TRUE),
        (Some(Value::Null), Some(Value::Null)) => Ok(Value::TRUE),
        _ => Ok(Value::FALSE),
    }
}

fn exact_qmark(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match args.pop() {
        Some(Value::Integer(_)) => Ok(Value::TRUE),
        Some(_) => Ok(Value::FALSE),
        None => Err("Invalid call to function exact?: expected one argument, got none".to_owned()),
    }
}

fn ge(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok((l >= r).into()),
        _ => Err("Invalid arguments to function =".to_owned()),
    }
}

fn gt(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok((l > r).into()),
        _ => Err("Invalid arguments to function =".to_owned()),
    }
}

fn integer_qmark(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match args.pop() {
        Some(Value::Integer(_)) => Ok(Value::TRUE),
        Some(_) => Ok(Value::FALSE),
        None => {
            Err("Invalid call to function integer?: expected one argument, got none".to_owned())
        }
    }
}

fn le(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok((l <= r).into()),
        _ => Err("Invalid arguments to function =".to_owned()),
    }
}

fn lt(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok((l < r).into()),
        _ => Err("Invalid arguments to function =".to_owned()),
    }
}

fn modulo(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => {
            if r != 0 {
                Ok(Value::Integer(l % r))
            } else {
                Err("Invalid zero diviser argument when calling function modulo".to_owned())
            }
        }
        _ => Err("Invalid arguments to function modulo".to_owned()),
    }
}

fn mul(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok(Value::Integer(l * r)),
        (Some(l @ Value::Integer(_)), None) => Ok(l),
        _ => Err("Invalid arguments to function *".to_owned()),
    }
}

fn sub(args: Vec<Value>) -> Result<Value> {
    let mut args = args;
    args.reverse();
    match (args.pop(), args.pop()) {
        (Some(Value::Integer(l)), Some(Value::Integer(r))) => Ok(Value::Integer(l - r)),
        (Some(Value::Integer(l)), None) => Ok(Value::Integer(-l)),
        _ => Err("Invalid arguments to function -".to_owned()),
    }
}

// pair

fn null_qmark(args: Vec<Value>) -> Result<Value> {
    match args.len() {
        0 => Err("Invalid call to function 'null?' without arguments".to_owned()),
        1 => {
            let mut args = args;
            Ok(args.pop().unwrap().is_null().into())
        }
        n => Err(format!(
            "Invalid call to function 'null?': expected 1 argument, got {} arguments",
            n
        )),
    }
}

// vector

fn vector_qmark(args: Vec<Value>) -> Result<Value> {
    match args.len() {
        0 => Err("Invalid call to function 'vector?' without arguments".to_owned()),
        1 => {
            let mut args = args;
            Ok(args.pop().unwrap().is_vector().into())
        }
        n => Err(format!(
            "Invalid call to function 'vector?': expected 1 argument, got {} arguments",
            n
        )),
    }
}

fn vector_length(args: Vec<Value>) -> Result<Value> {
    match args.len() {
        0 => Err("Invalid call to function 'vector-length' without arguments".to_owned()),
        1 => {
            let mut args = args;
            match args.pop().unwrap() {
                Value::Vector(vector) => Ok(Value::Integer(
                    vector
                        .len()
                        .try_into()
                        .expect("vector length is too big to fit within 32 bits"),
                )),
                value => Err(format!(
                    "Invalid call to function 'vector-length': expected a vector, got {}",
                    value
                )),
            }
        }
        n => Err(format!(
            "Invalid call to function 'vector-length': expected 1 argument, got {} arguments",
            n
        )),
    }
}

fn main() {
    let src = "(begin
        (define (exact-integer? z)
            (and (integer? z) (exact? z)))
        (define (zero? z)
            (= z 0))
        (define (positive? x)
            (> x 0))
        (define (negative? x)
            (< x 0))
        (define (odd? n)
            (= (modulo n 2) 1))
        (define (even? n)
            (zero? (modulo n 2)))
        (define (abs x)
            (if (< x 0)
                (- x)
                x))
        (define (gcd n1 n2)
            (if (= n2 0)
                n1
                (gcd n2 (modulo n1 n2))))
        (define (lcm n1 n2)
            (/ (abs (* n1 n2)) (gcd n1 n2)))
        (define (square z)
            (* z z))
        (define-record-type pair
            (cons x y)
            pair?
            (x car set-car!)
            (y cdr set-cdr!))
        (define (recur n)
            (if (> n 0)
                (recur (- n 1))))
        (recur 10)
    )";
    let _pair = "(begin
        (define-record-type pair
            (cons x y)
            pair?
            (x car set-car!)
            (y cdr set-cdr!))
        ;; caar - cddddr
        (define (caar p)
            (car (car p)))
        (define (cadr p)
            (car (cdr p)))
        (define (cdar p)
            (cdr (car p)))
        (define (cddr p)
            (cdr (cdr p)))
        (define (caaar p)
            (car (car (car p))))
        (define (caadr p)
            (car (car (cdr p))))
        (define (cadar p)
            (car (cdr (car p))))
        (define (caddr p)
            (car (cdr (cdr p))))
        (define (cdaar p)
            (cdr (car (car p))))
        (define (cdadr p)
            (cdr (car (cdr p))))
        (define (cddar p)
            (cdr (cdr (car p))))
        (define (cdddr p)
            (cdr (cdr (cdr p))))
        (define (caaaar p)
            (car (car (car (car p)))))
        (define (caaadr p)
            (car (car (car (cdr p)))))
        (define (caadar p)
            (car (car (cdr (car p)))))
        (define (caaddr p)
            (car (car (cdr (cdr p)))))
        (define (cadaar p)
            (car (cdr (car (car p)))))
        (define (cadadr p)
            (car (cdr (car (cdr p)))))
        (define (caddar p)
            (car (cdr (cdr (car p)))))
        (define (cadddr p)
            (car (cdr (cdr (cdr p)))))
        (define (cdaaar p)
            (cdr (car (car (car p)))))
        (define (cdaadr p)
            (cdr (car (car (cdr p)))))
        (define (cdadar p)
            (cdr (car (cdr (car p)))))
        (define (cdaddr p)
            (cdr (car (cdr (car p)))))
        (define (cddaar p)
            (cdr (cdr (car (car p)))))
        (define (cddadr p)
            (cdr (cdr (car (cdr p)))))
        (define (cdddar p)
            (cdr (cdr (cdr (car p)))))
        (define (cddddr p)
            (cdr (cdr (cdr (cdr p)))))
        ;; lists
        (define (list? xs)
            (or (null? xs)
                (if (pair? xs)
                    (list? (cdr xs))
                    #f)))
        (define (length xs)
            (define (go xs acc)
                (if (null? xs)
                    acc
                    (go (cdr xs) (+ acc 1))))
            (go xs 0))
    )";
    let mut tokens = Token::from_str(src);
    println!("tokens: {:#?}", tokens);
    let sexpr = SExpr::from_str(&mut tokens).unwrap();
    println!("sexpressions: {:#?}", sexpr);
    let expr = Expr::from_sexpr(sexpr).unwrap();
    println!("expressions: {:#?}", expr);
    let mut env = Env::with_builtins();
    let mut interpreter = Interpreter::new();
    let result = interpreter.execute(&mut env, expr).unwrap();
    println!("result: {:#?}", result);
}
