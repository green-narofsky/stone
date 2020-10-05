// TODO: A basic REPL.
use ::std::path::{PathBuf, Path};
use ::std::fs::File;
use ::structopt::StructOpt;
use ::serde::{Serialize, Deserialize};
use ::anyhow::Result;
use ::lasso::{Rodeo, Spur};

/// Driver binary for Stone
#[derive(StructOpt, Debug)]
enum Opt {
    /// Start a REPL.
    Run {
        /// Path to image for REPL.
        image: Option<PathBuf>,
    },
    /// Print the contents of an image.
    View {
        /// Path to image.
        image: PathBuf,
    }
}

#[derive(Serialize, Deserialize)]
enum Value {
    Integer(i64),
    String(Spur),
}

#[derive(Serialize, Deserialize)]
struct Binding {
    name: Spur,
    value: Value,
    // Bindings are currently uniquely identified
    // by their position in the Vec, but consider
    // doing something else to assure that, in the case
    // that we change up the storage scheme.
}

fn serialize_rodeo<S>(rodeo: &Rodeo, serializer: S) -> Result<S::Ok, S::Error>
where S: ::serde::Serializer {
    use ::serde::ser::SerializeSeq;
    let mut vec = serializer.serialize_seq(Some(rodeo.len()))?;
    for v in rodeo.strings() {
        vec.serialize_element(v)?;
    }
    vec.end()
}
fn deserialize_rodeo<'de, D>(deserializer: D) -> Result<Rodeo, D::Error>
where D: ::serde::Deserializer<'de> {
    // There's probably a much better way to do this.
    let vec: Vec<String> = Deserialize::deserialize(deserializer)?;
    let mut rodeo = Rodeo::new();
    for string in vec {
        rodeo.get_or_intern(string);
    }
    Ok(rodeo)
}

// Heh, perhaps this ought to be called a pebble.
#[derive(Serialize, Deserialize)]
struct Image {
    /// String interner.
    #[serde(serialize_with = "serialize_rodeo", deserialize_with = "deserialize_rodeo")]
    strings: Rodeo,
    /// At least for now, we store bindings in order.
    bindings: Vec<Binding>,
}
impl Image {
    fn new() -> Self {
        Self {
            strings: Rodeo::new(),
            bindings: Vec::new(),
        }
    }
    fn find_binding(&self, name: Spur) -> Option<&Binding> {
        for bind in self.bindings.iter().rev() {
            if bind.name == name {
                return Some(bind);
            }
        }
        None
    }
    /// Push a binding, shadowing any previous bindings with the same name.
    fn push_binding(&mut self, name: Spur, value: Value) {
        self.bindings.push(Binding { name, value })
    }
    /// Get the key for a string, interning it if it does not yet exist.
    fn get_or_intern_string(&mut self, val: String) -> Spur {
        self.strings.get_or_intern(val)
    }
}

/// Load an image from a path.
/// If the given file doesn't exist, then we create a blank image.
fn load_image(path: &Path) -> Result<Image> {
    use ::std::io::ErrorKind::*;
    match File::open(path) {
        Ok(file) => Ok(::bincode::deserialize_from(file)?),
        Err(e) => match e.kind() {
            NotFound => Ok(Image::new()),
            _ => Err(e)?,
        },
    }
}

/// Write an image to the given path.
fn write_image(path: &Path, image: &Image) -> Result<()> {
    let file = File::create(path)?;
    ::bincode::serialize_into(file, image)?;
    Ok(())
}

/// Save the image, if a path was given.
fn save(path: Option<&Path>, image: &Image) -> Result<()> {
    match path {
        Some(path) => write_image(path, image)?,
        None => (),
    }
    Ok(())
}

mod parse {
    use ::anyhow::Result;
    fn whitespace(input: &str) -> ::nom::IResult<&str, &str> {
        use ::nom::branch::alt;
        use ::nom::bytes::complete::tag;
        alt((tag(" "), tag("\t"), tag("\n")))(input)
    }
    macro_rules! tagged_enum {
        ($p:vis $e:ident : $parser_name:ident {
            $($v:ident : $tag_name:literal),*$(,)?
        }) => {
            #[derive(Debug, Copy, Clone)]
            $p enum $e {
                $($v),*
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
        pub(crate) Keyword : keyword {
            Print : "print",
            Let : "let",
        }
    }

    #[derive(Debug)]
    pub(crate) struct Identifier<'a> {
        name: &'a str,
    }
    fn identifier(input: &str) -> nom::IResult<&str, Identifier> {
        use ::nom::bytes::complete::take_while1;
        let (input, ident): (&str, &str) = take_while1(char::is_alphabetic)(input)?;
        Ok((input, Identifier { name: ident }))
    }

    #[derive(Debug)]
    pub(crate) enum Literal<'a> {
        String(&'a str),
        Integer(i64),
    }
    fn literal(input: &str) -> ::nom::IResult<&str, Literal> {
        use ::nom::branch::alt;
        use ::nom::bytes::complete::{take_until, take_while1};
        use ::nom::bytes::complete::tag;
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

    #[derive(Debug)]
    pub(crate) enum PrintArg<'a> {
        Ident(Identifier<'a>),
        Lit(Literal<'a>),
    }

    #[derive(Debug)]
    pub(crate) enum Command<'a> {
        Print(PrintArg<'a>),
        Bind(&'a str, &'a str),
    }
    impl Command<'_> {
        pub(crate) fn parse<'a>(input: &'a str) -> Result<Command<'a>> {
            use ::nom::sequence::tuple;
            use ::nom::bytes::complete::tag;
            use ::nom::multi::many1;
            use ::nom::branch::alt;
            match keyword(input) {
                Ok((input, Keyword::Print)) => {
                    let (input, (_, arg)) = tuple((many1(whitespace), alt((
                        |i| {
                            let (i, ident) = identifier(i)?;
                            Ok((i, PrintArg::Ident(ident)))
                        },
                        |i| {
                            let (i, lit) = literal(i)?;
                            Ok((i, PrintArg::Lit(lit)))
                        }))
                    ))(input).map_err(|e| ::anyhow::anyhow!("{}", e))?;
                    let remaining_input = input.trim();
                    if remaining_input.len() > 0 {
                        ::anyhow::bail!("unexpected trailing input: {}", remaining_input)
                    } else {
                        Ok(Command::Print(arg))
                    }
                },
                Ok((input, Keyword::Let)) => todo!("let bindings"),
                Err(e) => Err(::anyhow::anyhow!("{}", e)),
            }
        }
    }
}

fn main() -> Result<()> {
    let opt = Opt::from_args();
    match opt {
        Opt::Run { image: image_path } => {
            let image = if let Some(ref image) = image_path {
                load_image(&image)?
            } else {
                Image::new()
            };
            let stdin = ::std::io::stdin();
            let mut input = String::new();
            while let Ok(amt) = stdin.read_line(&mut input) {
                if amt == 0 {
                    // We've reached End Of File. (EOF)
                    // Or, well, end of user input. Since we're a REPL.
                    save(image_path.as_deref(), &image)?;
                    break;
                }

                println!("command: {:?}", parse::Command::parse(&input));

                input.clear();
            }
        },
        Opt::View { image } => {
            println!("{}", serde_json::to_string(&load_image(&image)?)?);
        }
    }

    Ok(())
}
