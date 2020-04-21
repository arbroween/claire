use self::lexer::Lexer;
pub(crate) use self::lexer::Location;
use self::parser::Parser;
pub(crate) use self::parser::{Integer, Keyword, ReaderData, Span, String, Symbol};
use std::string::String as RString;

mod lexer;
mod parser;

type Result<T> = std::result::Result<T, RString>;

pub(crate) struct Reader;

impl Reader {
    pub(crate) fn from_str(src: &str) -> Result<Vec<ReaderData>> {
        let mut data: Vec<ReaderData> = Vec::new();

        let mut tokens = Lexer::from_str(src)?;
        while !tokens.is_empty() {
            data.push(Parser::parse(&mut tokens)?);
        }

        Ok(data)
    }
}

#[cfg(test)]
mod tests {
    use super::parser::tests::{self as parser};
    use super::*;

    #[test]
    fn test_concat_string() {
        let src = r#"(str "test, " "micro, " 1 2 3)"#;

        assert_eq!(
            Reader::from_str(src),
            Ok(vec![parser::list(
                vec![
                    parser::symbol("str", 1, 4),
                    parser::string("test, ", 5, 13),
                    parser::string("micro, ", 14, 23),
                    parser::integer(1, 24, 25),
                    parser::integer(2, 26, 27),
                    parser::integer(3, 28, 29),
                ],
                0,
                29
            )])
        );
    }

    #[test]
    fn test_find_roots() {
        let src = "
            ; given a vector of coefficients,
            ; find the square roots of the corresponding quadratic equation
            (defn find-roots [a b c]
                (let [delta (substracts (square b) (times 4 a c))]
                    (if (positive delta)
                        ; there are two solutions
                        [(divides (sum (opposite b) (sqrt delta)) (times 2 a))
                         (divides (substracts (opposite b) (sqrt delta)) (times 2 a))]
                        (if (zero delta)
                            ; there is a double solution
                            [(divides (opposite b) (times 2 a))]
                            ; there are no solutions in the real numbers
                            []))))
        ";

        let result = vec![parser::commented(
            vec![
                parser::comment(" given a vector of coefficients,", 13, 46),
                parser::comment(
                    " find the square roots of the corresponding quadratic equation",
                    59,
                    122,
                ),
            ],
            parser::list(
                vec![
                    parser::symbol("defn", 136, 140),
                    parser::symbol("find-roots", 141, 151),
                    parser::vector(
                        vec![
                            parser::symbol("a", 153, 154),
                            parser::symbol("b", 155, 156),
                            parser::symbol("c", 157, 158),
                        ],
                        152,
                        158,
                    ),
                    parser::list(
                        vec![
                            parser::symbol("let", 177, 180),
                            parser::vector(
                                vec![
                                    parser::symbol("delta", 182, 187),
                                    parser::list(
                                        vec![
                                            parser::symbol("substracts", 189, 199),
                                            parser::list(
                                                vec![
                                                    parser::symbol("square", 201, 207),
                                                    parser::symbol("b", 208, 209),
                                                ],
                                                200,
                                                209,
                                            ),
                                            parser::list(
                                                vec![
                                                    parser::symbol("times", 212, 217),
                                                    parser::integer(4, 218, 219),
                                                    parser::symbol("a", 220, 221),
                                                    parser::symbol("c", 222, 223),
                                                ],
                                                211,
                                                223,
                                            ),
                                        ],
                                        188,
                                        224,
                                    ),
                                ],
                                181,
                                225,
                            ),
                            parser::list(
                                vec![
                                    parser::symbol("if", 248, 250),
                                    parser::list(
                                        vec![
                                            parser::symbol("positive", 252, 260),
                                            parser::symbol("delta", 261, 266),
                                        ],
                                        251,
                                        266,
                                    ),
                                    parser::vector(
                                        vec![
                                            parser::list(
                                                vec![
                                                    parser::symbol("divides", 344, 351),
                                                    parser::list(
                                                        vec![
                                                            parser::symbol("sum", 353, 356),
                                                            parser::list(
                                                                vec![
                                                                    parser::symbol(
                                                                        "opposite", 358, 366,
                                                                    ),
                                                                    parser::symbol("b", 367, 368),
                                                                ],
                                                                357,
                                                                368,
                                                            ),
                                                            parser::list(
                                                                vec![
                                                                    parser::symbol(
                                                                        "sqrt", 371, 375,
                                                                    ),
                                                                    parser::symbol(
                                                                        "delta", 376, 381,
                                                                    ),
                                                                ],
                                                                370,
                                                                381,
                                                            ),
                                                        ],
                                                        352,
                                                        382,
                                                    ),
                                                    parser::list(
                                                        vec![
                                                            parser::symbol("times", 385, 390),
                                                            parser::integer(2, 391, 392),
                                                            parser::symbol("a", 393, 394),
                                                        ],
                                                        384,
                                                        394,
                                                    ),
                                                ],
                                                343,
                                                395,
                                            ),
                                            // from here
                                            parser::list(
                                                vec![
                                                    parser::symbol("divides", 423, 430),
                                                    parser::list(
                                                        vec![
                                                            parser::symbol("substracts", 432, 442),
                                                            parser::list(
                                                                vec![
                                                                    parser::symbol(
                                                                        "opposite", 444, 452,
                                                                    ),
                                                                    parser::symbol("b", 453, 454),
                                                                ],
                                                                443,
                                                                454,
                                                            ),
                                                            parser::list(
                                                                vec![
                                                                    parser::symbol(
                                                                        "sqrt", 457, 461,
                                                                    ),
                                                                    parser::symbol(
                                                                        "delta", 462, 467,
                                                                    ),
                                                                ],
                                                                456,
                                                                467,
                                                            ),
                                                        ],
                                                        431,
                                                        468,
                                                    ),
                                                    parser::list(
                                                        vec![
                                                            parser::symbol("times", 471, 476),
                                                            parser::integer(2, 477, 478),
                                                            parser::symbol("a", 479, 480),
                                                        ],
                                                        470,
                                                        480,
                                                    ),
                                                ],
                                                422,
                                                481,
                                            ),
                                        ],
                                        342,
                                        482,
                                    ),
                                    parser::list(
                                        vec![
                                            parser::symbol("if", 509, 511),
                                            parser::list(
                                                vec![
                                                    parser::symbol("zero", 513, 517),
                                                    parser::symbol("delta", 518, 523),
                                                ],
                                                512,
                                                523,
                                            ),
                                            parser::vector(
                                                vec![parser::list(
                                                    vec![
                                                        parser::symbol("divides", 612, 619),
                                                        parser::list(
                                                            vec![
                                                                parser::symbol(
                                                                    "opposite", 621, 629,
                                                                ),
                                                                parser::symbol("b", 630, 631),
                                                            ],
                                                            620,
                                                            631,
                                                        ),
                                                        parser::list(
                                                            vec![
                                                                parser::symbol("times", 634, 639),
                                                                parser::integer(2, 640, 641),
                                                                parser::symbol("a", 642, 643),
                                                            ],
                                                            633,
                                                            643,
                                                        ),
                                                    ],
                                                    611,
                                                    644,
                                                )],
                                                610,
                                                645,
                                            ),
                                            parser::vector(Vec::new(), 748, 749),
                                        ],
                                        508,
                                        750,
                                    ),
                                ],
                                247,
                                751,
                            ),
                        ],
                        176,
                        752,
                    ),
                ],
                135,
                753,
            ),
        )];

        assert_eq!(Reader::from_str(src), Ok(result));
    }

    #[test]
    fn test_map_literal() {
        let src = "{:zero 0 :one 1 :twenty-two 22 :three-hundred-and-thirty-three 333}";

        assert_eq!(
            Reader::from_str(src),
            Ok(vec![parser::map(
                vec![
                    parser::keyword("zero", 1, 6),
                    parser::integer(0, 7, 8),
                    parser::keyword("one", 9, 13),
                    parser::integer(1, 14, 15),
                    parser::keyword("twenty-two", 16, 27),
                    parser::integer(22, 28, 30),
                    parser::keyword("three-hundred-and-thirty-three", 31, 62),
                    parser::integer(333, 63, 66),
                ],
                0,
                66
            )])
        );
    }
}
