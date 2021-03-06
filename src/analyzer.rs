use super::reader::{Integer, Keyword, ReaderData, Span, String, Symbol};
use std::string::String as RString;

type Result<T> = std::result::Result<T, RString>;

#[derive(Debug, Eq, Hash, PartialEq)]
enum Ast {
    Call {
        span: Span,
        operator: Box<Self>,
        operands: Vec<Self>,
    },
    Def {
        span: Span,
        name: Symbol,
        value: Option<Box<Self>>,
    },
    Do {
        span: Span,
        body: Vec<Self>,
    },
    Fn {
        span: Span,
        params: Vec<Self>,
        body: Vec<Self>,
    },
    If {
        span: Span,
        test: Box<Self>,
        then: Box<Self>,
        else_: Option<Box<Self>>,
    },
    Integer(Integer),
    Keyword(Keyword),
    Let {
        span: Span,
        bindings: Vec<(Self, Self)>,
        body: Vec<Self>,
    },
    Map {
        span: Span,
        entries: Vec<(Self, Self)>,
    },
    String(String),
    Symbol(Symbol),
    Vector {
        span: Span,
        items: Vec<Self>,
    },
}

impl Ast {
    fn call(span: Span, operator: Self, operands: Vec<Self>) -> Self {
        Self::Call {
            span,
            operator: Box::new(operator),
            operands,
        }
    }

    fn def(span: Span, name: Symbol, value: Option<Ast>) -> Self {
        Self::Def {
            span,
            name,
            value: value.map(Box::new),
        }
    }

    fn do_(span: Span, body: Vec<Self>) -> Self {
        Self::Do { span, body }
    }

    fn fn_(span: Span, params: Vec<Self>, body: Vec<Self>) -> Self {
        Self::Fn { span, params, body }
    }

    fn if_(span: Span, test: Self, then: Self, else_: Option<Self>) -> Self {
        Self::If {
            span,
            test: Box::new(test),
            then: Box::new(then),
            else_: else_.map(Box::new),
        }
    }

    fn integer(integer: Integer) -> Self {
        Self::Integer(integer)
    }

    fn keyword(keyword: Keyword) -> Self {
        Self::Keyword(keyword)
    }

    fn let_(span: Span, bindings: Vec<(Self, Self)>, body: Vec<Self>) -> Self {
        Self::Let {
            span,
            bindings,
            body,
        }
    }

    fn map(span: Span, entries: Vec<(Self, Self)>) -> Self {
        Self::Map { span, entries }
    }

    fn string(string: String) -> Self {
        Self::String(string)
    }

    fn vector(span: Span, items: Vec<Self>) -> Self {
        Self::Vector { span, items }
    }
}

impl From<Symbol> for Ast {
    fn from(symbol: Symbol) -> Self {
        Self::Symbol(symbol)
    }
}

struct Analyzer;

impl Analyzer {
    fn analyze(data: ReaderData) -> Result<Ast> {
        match data {
            ReaderData::Commented { data, .. } => Self::analyze(*data),
            ReaderData::Integer(integer) => Ok(Ast::integer(integer)),
            ReaderData::Keyword(keyword) => Ok(Ast::keyword(keyword)),
            ReaderData::List { span, items } => Self::analyze_list(span, items),
            ReaderData::Map { span, items } => Self::analyze_map(span, items),
            ReaderData::String(string) => Ok(Ast::string(string)),
            ReaderData::Symbol(symbol) => Self::analyze_symbol(symbol),
            ReaderData::Vector { span, items } => Self::analyze_vector(span, items),
        }
    }

    fn analyze_symbol(symbol: Symbol) -> Result<Ast> {
        Ok(symbol.into())
    }

    fn analyze_list(span: Span, items: Vec<ReaderData>) -> Result<Ast> {
        let mut items = items;
        items.reverse();
        match items.pop() {
            Some(ReaderData::Symbol(symbol)) => match symbol.name() {
                "def" => {
                    items.reverse();
                    Self::analyze_def(span, items)
                }
                "do" => {
                    items.reverse();
                    Self::analyze_do(span, items)
                }
                "fn" => {
                    items.reverse();
                    Self::analyze_fn(span, items)
                }
                "if" => {
                    items.reverse();
                    Self::analyze_if(span, items)
                }
                "let" => {
                    items.reverse();
                    Self::analyze_let(span, items)
                }
                _ => {
                    items.reverse();
                    Self::analyze_call(span, ReaderData::Symbol(symbol), items)
                }
            },
            Some(operator) => {
                items.reverse();
                Self::analyze_call(span, operator, items)
            }
            None => {
                unimplemented!("AnalyzerError: lists literals and empty lists are not implemented")
            }
        }
    }

    fn analyze_map(span: Span, items: Vec<ReaderData>) -> Result<Ast> {
        let mut entries = Vec::with_capacity(items.len() / 2);
        for pair in items.chunks_exact(2) {
            match pair {
                [key, value] => {
                    entries.push((Self::analyze(key.clone())?, Self::analyze(value.clone())?));
                }
                _ => unreachable!(),
            }
        }
        Ok(Ast::map(span, entries))
    }

    fn analyze_vector(span: Span, items: Vec<ReaderData>) -> Result<Ast> {
        let items: Result<Vec<_>> = items.into_iter().map(Self::analyze).collect();
        Ok(Ast::vector(span, items?))
    }

    fn analyze_call(span: Span, operator: ReaderData, operands: Vec<ReaderData>) -> Result<Ast> {
        let operands: Result<Vec<_>> = operands.into_iter().map(Self::analyze).collect();
        Ok(Ast::call(span, Self::analyze(operator)?, operands?))
    }

    fn analyze_def(span: Span, forms: Vec<ReaderData>) -> Result<Ast> {
        let mut forms = forms;
        forms.reverse();
        match forms.pop() {
            Some(ReaderData::Symbol(name)) => match forms.pop().map(Self::analyze) {
                Some(Ok(value)) => {
                    forms.reverse();
                    Ok(Ast::def(span, name, Some(value)))
                }
                None => {
                    forms.reverse();
                    Ok(Ast::def(span, name, None))
                }
                Some(Err(err)) => Err(err),
            },
            Some(data) => Err(format!(
                "AnalyzerError: invalid def form: expected the name of the var, found '{}' at {}",
                data,
                data.span()
            )),
            None => Err(format!(
                "AnalyzerError: invalid def form: missing the name of the var at {}",
                span
            )),
        }
    }

    fn analyze_do(span: Span, forms: Vec<ReaderData>) -> Result<Ast> {
        let forms: Result<Vec<_>> = forms.into_iter().map(Self::analyze).collect();
        Ok(Ast::do_(span, forms?))
    }

    fn analyze_fn(span: Span, forms: Vec<ReaderData>) -> Result<Ast> {
        let mut forms = forms;
        forms.reverse();
        match forms.pop() {
            Some(ReaderData::Vector { items, .. }) => {
                let params: Result<Vec<_>> = items.into_iter().map(Self::analyze).collect();

                forms.reverse();
                let body: Result<Vec<_>> = forms.into_iter().map(Self::analyze).collect();
                Ok(Ast::fn_(span, params?, body?))
            }
            Some(data) => Err(format!(
                "AnalyzerError: invalid fn form: expected a vector of parameters, found '{}' at {}",
                data,
                data.span()
            )),
            None => Err(format!(
                "AnalyzerError: invalid let form: missing vector of parameters at {}",
                span
            )),
        }
    }

    fn analyze_if(span: Span, forms: Vec<ReaderData>) -> Result<Ast> {
        let mut forms = forms;
        forms.reverse();
        match (forms.pop(), forms.pop()) {
            (Some(test), Some(then)) => {
                let test = Self::analyze(test)?;
                let then = Self::analyze(then)?;
                match forms.pop() {
                    Some(else_) => Ok(Ast::if_(span, test, then, Some(Self::analyze(else_)?))),
                    None => Ok(Ast::if_(span, test, then, None)),
                }
            }
            (Some(_), None) => Err(format!("AnalyzerError: invalid if form: expected an expression to evaluate if the condition is true at {}", span)),
            (None, _) => Err(format!("AnalyzerError: invalid if form: missing test condition at {}", span)),
        }
    }

    fn analyze_let(span: Span, forms: Vec<ReaderData>) -> Result<Ast> {
        let mut forms = forms;
        forms.reverse();
        match forms.pop() {
            Some(ReaderData::Vector { items, .. }) => {
                let mut bindings = Vec::with_capacity(items.len() / 2);
                for pair in items.chunks_exact(2) {
                    match pair {
                        [symbol, value] => {
                            bindings.push((
                                Self::analyze(symbol.clone())?,
                                Self::analyze(value.clone())?,
                            ));
                        }
                        _ => unreachable!(),
                    }
                }

                forms.reverse();
                let body: Result<Vec<_>> = forms.into_iter().map(Self::analyze).collect();
                Ok(Ast::let_(span, bindings, body?))
            }
            Some(data) => Err(format!(
                "AnalyzerError: invalid let form: expected a vector of bindings, found '{}' at {}",
                data,
                data.span()
            )),
            None => Err(format!(
                "AnalyzerError: invalid let form: missing vector of bindings at {}",
                span
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::{Location, Reader};

    fn call(operator: Ast, operands: Vec<Ast>, start: usize, end: usize) -> Ast {
        Ast::call(
            Span::new(Location::new(start), Location::new(end)),
            operator,
            operands,
        )
    }

    fn def(name: Symbol, value: Option<Ast>, start: usize, end: usize) -> Ast {
        Ast::def(
            Span::new(Location::new(start), Location::new(end)),
            name,
            value,
        )
    }

    fn do_(body: Vec<Ast>, start: usize, end: usize) -> Ast {
        Ast::do_(Span::new(Location::new(start), Location::new(end)), body)
    }

    fn fn_(params: Vec<Ast>, body: Vec<Ast>, start: usize, end: usize) -> Ast {
        Ast::fn_(
            Span::new(Location::new(start), Location::new(end)),
            params,
            body,
        )
    }

    fn if_(test: Ast, then: Ast, else_: Option<Ast>, start: usize, end: usize) -> Ast {
        Ast::if_(
            Span::new(Location::new(start), Location::new(end)),
            test,
            then,
            else_,
        )
    }

    fn integer(value: i32, start: usize, end: usize) -> Ast {
        Ast::integer(Integer::new(
            Span::new(Location::new(start), Location::new(end)),
            value,
        ))
    }

    fn keyword(name: impl Into<RString>, start: usize, end: usize) -> Ast {
        Ast::keyword(Keyword::new(
            Span::new(Location::new(start), Location::new(end)),
            name.into(),
        ))
    }

    fn let_(bindings: Vec<(Ast, Ast)>, body: Vec<Ast>, start: usize, end: usize) -> Ast {
        Ast::let_(
            Span::new(Location::new(start), Location::new(end)),
            bindings,
            body,
        )
    }

    fn map(entries: Vec<(Ast, Ast)>, start: usize, end: usize) -> Ast {
        Ast::map(Span::new(Location::new(start), Location::new(end)), entries)
    }

    fn string(value: impl Into<RString>, start: usize, end: usize) -> Ast {
        Ast::string(String::new(
            Span::new(Location::new(start), Location::new(end)),
            value.into(),
        ))
    }

    fn symbol(name: impl Into<RString>, start: usize, end: usize) -> Symbol {
        Symbol::new(
            Span::new(Location::new(start), Location::new(end)),
            name.into(),
        )
    }

    fn vector(items: Vec<Ast>, start: usize, end: usize) -> Ast {
        Ast::vector(Span::new(Location::new(start), Location::new(end)), items)
    }

    #[test]
    fn test_call() {
        let data = Reader::from_str("(find {:a 1 :b 2} :c)").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![call(
                symbol("find", 1, 5).into(),
                vec![
                    map(
                        vec![
                            (keyword("a", 7, 9), integer(1, 10, 11)),
                            (keyword("b", 12, 14), integer(2, 15, 16))
                        ],
                        6,
                        16
                    ),
                    keyword("c", 18, 20)
                ],
                0,
                20
            )])
        );
    }

    #[test]
    fn test_def() {
        let data = Reader::from_str(r#"(def project "claire")"#).unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![def(
                symbol("project", 5, 12),
                Some(string("claire", 13, 21)),
                0,
                21
            )])
        );
    }

    #[test]
    fn test_do() {
        let data = Reader::from_str(r#"(do (step-1) (step-2) (step-3))"#).unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![do_(
                vec![
                    call(symbol("step-1", 5, 11).into(), Vec::new(), 4, 11),
                    call(symbol("step-2", 14, 20).into(), Vec::new(), 13, 20),
                    call(symbol("step-3", 23, 29).into(), Vec::new(), 22, 29),
                ],
                0,
                30
            )])
        );
    }

    #[test]
    fn test_fn() {
        let data =
            Reader::from_str(r#"(apply (fn [name] (str "Hello, " name "!")) "world")"#).unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![call(
                symbol("apply", 1, 6).into(),
                vec![
                    fn_(
                        vec![symbol("name", 12, 16).into()],
                        vec![call(
                            symbol("str", 19, 22).into(),
                            vec![
                                string("Hello, ", 23, 32),
                                symbol("name", 33, 37).into(),
                                string("!", 38, 41),
                            ],
                            18,
                            41
                        )],
                        7,
                        42
                    ),
                    string("world", 44, 51),
                ],
                0,
                51
            )])
        );
    }

    #[test]
    fn test_if() {
        let data = Reader::from_str("(if (mark answer) :correct :wrong)").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![if_(
                call(
                    symbol("mark", 5, 9).into(),
                    vec![symbol("answer", 10, 16).into()],
                    4,
                    16
                ),
                keyword("correct", 18, 26),
                Some(keyword("wrong", 27, 33)),
                0,
                33
            )])
        );
    }

    #[test]
    fn test_integer() {
        let data = Reader::from_str("1234567890").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(results, Ok(vec![integer(1_234_567_890, 0, 10)]));
    }

    #[test]
    fn test_keyword() {
        let data = Reader::from_str(":keyword").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(results, Ok(vec![keyword("keyword", 0, 8)]));
    }

    #[test]
    fn test_let() {
        let data = Reader::from_str("(let [x 1 y 2 z 3] [x y z])").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![let_(
                vec![
                    (symbol("x", 6, 7).into(), integer(1, 8, 9)),
                    (symbol("y", 10, 11).into(), integer(2, 12, 13)),
                    (symbol("z", 14, 15).into(), integer(3, 16, 17)),
                ],
                vec![vector(
                    vec![
                        symbol("x", 20, 21).into(),
                        symbol("y", 22, 23).into(),
                        symbol("z", 24, 25).into(),
                    ],
                    19,
                    25
                )],
                0,
                26
            )])
        );
    }

    #[test]
    fn test_map() {
        let data = Reader::from_str("(merge {:a :b :c :d} {1 2 3 4})").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![call(
                symbol("merge", 1, 6).into(),
                vec![
                    map(
                        vec![
                            (keyword("a", 8, 10), keyword("b", 11, 13)),
                            (keyword("c", 14, 16), keyword("d", 17, 19)),
                        ],
                        7,
                        19
                    ),
                    map(
                        vec![
                            (integer(1, 22, 23), integer(2, 24, 25)),
                            (integer(3, 26, 27), integer(4, 28, 29)),
                        ],
                        21,
                        29
                    ),
                ],
                0,
                30
            )])
        );
    }

    #[test]
    fn test_string() {
        let data = Reader::from_str(r#""string""#).unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(results, Ok(vec![string("string", 0, 8)]));
    }

    #[test]
    fn test_symbol() {
        let data = Reader::from_str("symbol").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(results, Ok(vec![symbol("symbol", 0, 6).into()]));
    }

    #[test]
    fn test_vector() {
        let data = Reader::from_str("[1 2 3]").unwrap();
        let results: Result<Vec<_>> = data.into_iter().map(Analyzer::analyze).collect();

        assert_eq!(
            results,
            Ok(vec![vector(
                vec![integer(1, 1, 2), integer(2, 3, 4), integer(3, 5, 6)],
                0,
                6
            )])
        );
    }
}
