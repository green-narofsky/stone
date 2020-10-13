use ::anyhow::Result;
use ::std::path::{Path, PathBuf};
fn whitespace(input: &str) -> ::nom::IResult<&str, &str> {
    use ::nom::branch::alt;
    use ::nom::bytes::complete::tag;
    alt((tag(" "), tag("\t"), tag("\n")))(input)
}
macro_rules! tagged_enum {
    ($(#[$attr:meta])* $p:vis $e:ident : $parser_name:ident {
        $($(#[$var_attr:meta])* $v:ident : $tag_name:literal),*$(,)?
    }) => {
        $(#[$attr])*
            $p enum $e {
                $($(#[$var_attr])* $v),*
            }
        fn $parser_name(input: &str) -> ::nom::IResult<&str, $e> {
            use ::nom::bytes::complete::tag;
            use ::nom::branch::alt;
            use ::nom::combinator::peek;
            alt(($(|i| {
                let (i, v) = tag($tag_name)(i).map(|(i, _)| (i, $e::$v))?;
                peek(whitespace)(i)?; // Handle keywords at the start of normal identifiers
                Ok((i, v))
            }),*))(input)
        }
    }
}

tagged_enum! {
    #[derive(Debug, Copy, Clone)]
    pub(crate) CommandWord : command_word {
        Print : "print",
        Let : "let",
        Save : "save",
        Load : "load",
        Remount : "mount",
    }
}

#[derive(Debug)]
pub(crate) struct Identifier<'a> {
    pub(crate) name: &'a str,
}
fn identifier(input: &str) -> nom::IResult<&str, Identifier> {
    use ::nom::bytes::complete::take_while1;
    let (input, ident): (&str, &str) = take_while1(char::is_alphabetic)(input)?;
    Ok((input, Identifier { name: ident }))
}

#[derive(Debug)]
pub(crate) enum Literal<'a> {
    String(&'a str),
    // We don't have a way to express these yet.
    // Bytes(&'a [u8]),
    Integer(i64),
}
fn literal(input: &str) -> ::nom::IResult<&str, Literal> {
    use ::nom::branch::alt;
    use ::nom::bytes::complete::tag;
    use ::nom::bytes::complete::{take_until, take_while1};
    alt((
        |i| {
            let (i, _) = tag("\"")(i)?;
            let (i, s) = take_until("\"")(i)?; // TODO: handle escaped quotes
            let (i, _) = tag("\"")(i)?;
            Ok((i, Literal::String(s)))
        },
        |i| {
            let (i, int): (&str, &str) = take_while1(|c: char| c.is_digit(10))(i)?;
            let int = match int.parse::<i64>() {
                Err(_) => return Err(::nom::Err::Failure((i, ::nom::error::ErrorKind::TooLarge))),
                Ok(x) => x,
            };
            Ok((i, Literal::Integer(int)))
        },
    ))(input)
}
/// Parse out a
fn string(input: &str) -> ::nom::IResult<&str, &[u8]> {
    todo!()
}
/// Parse out a path from a format that we accept.
fn path(input: &str) -> ::nom::IResult<&str, PathBuf> {
    todo!()
}

#[derive(Debug)]
pub(crate) enum PrintArg<'a> {
    Ident(Identifier<'a>),
    Lit(Literal<'a>),
}

#[derive(Debug)]
pub(crate) enum Expression<'a> {
    Lit(Literal<'a>),
}
fn expression(input: &str) -> ::nom::IResult<&str, Expression> {
    // TODO: support more complex expressions
    let (input, lit) = literal(input)?;
    Ok((input, Expression::Lit(lit)))
}

#[derive(Debug)]
pub(crate) enum Command<'a> {
    Print(PrintArg<'a>),
    Bind(Identifier<'a>, Expression<'a>),
    SaveHello,
    LoadHello,
    Remount(&'a Path),
}
impl Command<'_> {
    pub(crate) fn parse<'a>(input: &'a str) -> Result<Command<'a>> {
        use ::nom::branch::alt;
        use ::nom::bytes::complete::tag;
        use ::nom::multi::{many0, many1};
        use ::nom::sequence::tuple;
        match command_word(input) {
            Ok((input, CommandWord::Print)) => {
                let (input, (_, arg)) = tuple((
                    many1(whitespace),
                    alt((
                        |i| {
                            let (i, ident) = identifier(i)?;
                            Ok((i, PrintArg::Ident(ident)))
                        },
                        |i| {
                            let (i, lit) = literal(i)?;
                            Ok((i, PrintArg::Lit(lit)))
                        },
                    )),
                ))(input)
                .map_err(|e| ::anyhow::anyhow!("{}", e))?;
                let remaining_input = input.trim();
                if remaining_input.len() > 0 {
                    ::anyhow::bail!("unexpected trailing input: {}", remaining_input)
                } else {
                    Ok(Command::Print(arg))
                }
            }
            Ok((input, CommandWord::Let)) => {
                let (input, (_, ident, _, _, _, exp)) = tuple((
                    many1(whitespace),
                    identifier,
                    many0(whitespace),
                    tag("="),
                    many0(whitespace),
                    expression,
                ))(input)
                .map_err(|e| ::anyhow::anyhow!("{}", e))?;
                let remaining_input = input.trim();
                if remaining_input.len() > 0 {
                    ::anyhow::bail!("unexpected trailing input: {}", remaining_input)
                } else {
                    Ok(Command::Bind(ident, exp))
                }
            }
            Ok((input, CommandWord::Save)) => Ok(Command::SaveHello),
            Ok((input, CommandWord::Load)) => Ok(Command::LoadHello),
            Ok((input, CommandWord::Remount)) => {
                let (input, _) = many1(whitespace)(input)
                    .expect("command word parser should've verified presence of whitespace");
                // This is obviously wrong. This ends up including the trailing newline.
                // And, escapes are unusable.
                // I really should have a more principled way of handling command arguments.
                let path: &Path = input.as_ref();
                Ok(Command::Remount(path))
            }
            Err(e) => Err(::anyhow::anyhow!("{}", e)),
        }
    }
}
