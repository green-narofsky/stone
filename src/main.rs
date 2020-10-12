// TODO: A basic REPL.
use ::anyhow::Result;
use ::lasso::{Rodeo, Spur};
use ::serde::{Deserialize, Serialize};
use ::std::collections::HashMap;
use ::std::fs::File;
use ::std::path::{Path, PathBuf};
use ::structopt::StructOpt;
mod jit;

/// Driver binary for Stone
#[derive(StructOpt, Debug)]
enum Opt {
    /// Start a REPL.
    Mount {
        /// Path to image for REPL.
        image: Option<PathBuf>,
    },
    /// Print the contents of an image.
    View {
        /// Path to image.
        image: PathBuf,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
enum Type {
    Integer,
    Float,
    Boolean,
    Unit,
    String,
    Type,
}

struct Signature {
    params: Vec<Type>,
    ret: Type,
}

macro_rules! perhaps_new {
    ($a:tt) => {
        $a
    };
    ($a:ty => $con:expr) => {
        $con
    };
}

macro_rules! prim_funcs {
    ($(#[$attr:meta])* $vis:vis $group:ident (? $parse_err:tt $(=> $parse_err_new:expr)?) {
        $($name:ident : $vari:ident ($($par_ty:ident),*) -> $ret_ty:ident = ($($par_name:ident),*) => $exp:expr),*
    $(,)?}) => {
        $(#[$attr])*
        $vis enum $group {
            $($vari),*
        }
        impl $group {
            $vis fn sig(&self) -> Signature {
                match self {
                    $(Self::$vari => Signature {
                        params: ::std::vec![$(Type::$par_ty),*],
                        ret: Type::$ret_ty,
                    }),*
                }
            }
            $vis fn eval(&self, args: Vec<Value>) -> Value {
                match self {
                    $(Self::$vari => {
                        let mut it = args.into_iter();
                        let exp = |$($par_name),*| {
                            $exp
                        };
                        Value::$ret_ty(exp(
                            $(
                                match it.next().unwrap() {
                                    Value::$par_ty(x) => x,
                                    x => panic!(
                                        "type mismatch on primitive function {}: expected {:?}, got {:?}",
                                        stringify!($name), Type::$par_ty, x.type_of()
                                    ),
                                }
                            ),*
                        ))
                    }),*
                }
            }
            $vis fn name(&self) -> &'static str {
                match self {
                    $(Self::$vari => stringify!($name)),*
                }
            }
        }
        impl ::core::str::FromStr for $group {
            type Err = $parse_err;
            fn from_str(input: &str) -> Result<$group, Self::Err> {
                match input {
                    $(stringify!($name) => Ok($group::$vari)),*,
                    _ => Err(perhaps_new!($parse_err $(=> $parse_err_new)?)),
                }
            }
        }
    }
}

/// Failure to parse a pure primitive function from a string.
#[derive(Debug)]
struct PrimParseError;

prim_funcs! {
    /// Primitive functions with no side effects.
    #[derive(PartialEq, Eq, Debug)]
    PrimitivePureFunction (?PrimParseError) {
        sum  : Sum(Integer, Integer) -> Integer = (x, y) => x + y,
        diff : Difference(Integer, Integer) -> Integer = (x, y) => x - y,
        quot : Quotient(Integer, Integer) -> Integer = (x, y) => x / y,
        prod : Product(Integer, Integer) -> Integer = (x, y) => x * y,
    }
}

#[cfg(test)]
mod prim_test {
    use super::PrimitivePureFunction;
    #[test]
    fn parsing_primitives() {
        assert_eq!(
            "sum".parse::<PrimitivePureFunction>().unwrap(),
            PrimitivePureFunction::Sum
        );
        assert_eq!(
            "diff".parse::<PrimitivePureFunction>().unwrap(),
            PrimitivePureFunction::Difference
        );
        assert_eq!(
            "quot".parse::<PrimitivePureFunction>().unwrap(),
            PrimitivePureFunction::Quotient
        );
        assert_eq!(
            "prod".parse::<PrimitivePureFunction>().unwrap(),
            PrimitivePureFunction::Product
        );
    }
}

struct CallTree {}

struct PureFunction {
    sig: Signature,
    expression: (),
}

/// A sum type.
struct SumType {
    variants: Vec<Type>,
}

/// An instance of a sum type.
struct SumValue {}

/// A product type.
struct ProductType {
    name: String,
    fields: Vec<Type>,
}

// Note that for the prim_funcs macro, we require
// the Type enum and Value enum to have the same name
// for every variant.
#[derive(Serialize, Deserialize, Debug)]
enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Unit,
    // Probably not going to stick with interning all strings,
    // because it'd make mutation quite expensive: clone for every change.
    // I will, however, have a reference type.
    // This all is just a sketch at the moment.
    // Essentially, the only string type we have at the moment is &'static str.
    String(Spur),
    Type(Type),
    // PureFunction(PureFunction),
}
impl Value {
    fn type_of(&self) -> Type {
        match self {
            Value::Integer(_) => Type::Integer,
            Value::Float(_) => Type::Float,
            Value::Boolean(_) => Type::Boolean,
            Value::String(_) => Type::String,
            Value::Type(_) => Type::Type,
            Value::Unit => Type::Unit,
        }
    }
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
where
    S: ::serde::Serializer,
{
    use ::serde::ser::SerializeSeq;
    let mut vec = serializer.serialize_seq(Some(rodeo.len()))?;
    for v in rodeo.strings() {
        vec.serialize_element(v)?;
    }
    vec.end()
}
fn deserialize_rodeo<'de, D>(deserializer: D) -> Result<Rodeo, D::Error>
where
    D: ::serde::Deserializer<'de>,
{
    // There's probably a much better way to do this.
    let vec: Vec<String> = Deserialize::deserialize(deserializer)?;
    let mut rodeo = Rodeo::new();
    for string in vec {
        rodeo.get_or_intern(string);
    }
    Ok(rodeo)
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct BinaryKey {
    val: u32,
}
/// A thing for us to store machine code in.
/// Actual Stone code should not have direct access to this.
/// The REPL, however, should provide control of this binary cache.
/// I haven't sorted out the exact caching model,
/// but surely there are gains to be had here.
#[derive(Serialize, Deserialize)]
struct BinaryIndex {
    map: HashMap<u32, Vec<u8>>,
}
impl BinaryIndex {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    fn insert(&mut self, data: Vec<u8>) -> BinaryKey {
        let key = self.map.len() as u32;
        self.map.insert(key, data);
        BinaryKey { val: key }
    }
    fn get(&self, key: BinaryKey) -> Option<&[u8]> {
        self.map.get(&key.val).map(|x| &**x)
    }
    fn get_mut(&mut self, key: BinaryKey) -> Option<&mut Vec<u8>> {
        self.map.get_mut(&key.val)
    }
}

// Heh, perhaps this ought to be called a pebble.
#[derive(Serialize, Deserialize)]
struct Image {
    /// String interner.
    #[serde(
        serialize_with = "serialize_rodeo",
        deserialize_with = "deserialize_rodeo"
    )]
    strings: Rodeo,
    /// At least for now, we store bindings in order.
    bindings: Vec<Binding>,
    // bindex: BinaryIndex,
    hello: Option<(usize, Vec<u8>)>,
}
impl Image {
    fn new() -> Self {
        Self {
            strings: Rodeo::new(),
            bindings: Vec::new(),
            //bindex: BinaryIndex::new(),
            hello: None,
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
    fn get_or_intern_string(&mut self, val: &str) -> Spur {
        self.strings.get_or_intern(val)
    }
    /// Reify a parsed expression against this image.
    /// Note that this does not perform evaluation.
    fn reify_parsed_expression(&mut self, exp: parse::Expression) -> Expression {
        match exp {
            parse::Expression::Lit(lit) => match lit {
                parse::Literal::Integer(int) => Expression::Literal(Value::Integer(int)),
                parse::Literal::String(string) => {
                    Expression::Literal(Value::String(self.get_or_intern_string(string)))
                }
            },
        }
    }
    /// Perform evaluation of expression.
    fn eval_expression(&mut self, exp: Expression) -> Value {
        match exp {
            Expression::Literal(val) => val,
        }
    }
}

#[derive(Debug)]
enum Expression {
    Literal(Value),
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
    use ::std::path::Path;
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
                    Err(_) => {
                        return Err(::nom::Err::Failure((i, ::nom::error::ErrorKind::TooLarge)))
                    }
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
}

fn main() -> Result<()> {
    let opt = Opt::from_args();
    match opt {
        Opt::Mount { image: image_path } => {
            let mut image = if let Some(ref image) = image_path {
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
                match parse::Command::parse(&input) {
                    Ok(parse::Command::Bind(ident, exp)) => {
                        let exp = image.reify_parsed_expression(exp);
                        println!("exp: {:?}", exp);
                        let val = image.eval_expression(exp);
                        let key = image.get_or_intern_string(ident.name);
                        image.push_binding(key, val);
                    }
                    Ok(parse::Command::Print(arg)) => match arg {
                        parse::PrintArg::Lit(parse::Literal::Integer(int)) => println!("{}", int),
                        parse::PrintArg::Lit(parse::Literal::String(string)) => {
                            println!("{}", string)
                        }
                        parse::PrintArg::Ident(ident) => {
                            let key = image.get_or_intern_string(ident.name);
                            if let Some(binding) = image.find_binding(key) {
                                match binding.value {
                                    Value::Integer(int) => println!("{}", int),
                                    Value::Float(float) => println!("{}", float),
                                    Value::Boolean(boolean) => println!("{}", boolean),
                                    Value::Unit => println!("()"),
                                    Value::String(key) => {
                                        println!("{}", image.strings.resolve(&key))
                                    }
                                    Value::Type(ty) => println!("{:?}", ty),
                                }
                            } else {
                                eprintln!("binding {} does not exist", ident.name);
                            }
                        }
                    },
                    Ok(parse::Command::SaveHello) => {
                        let (buf, start) = jit::make_x64_linux_hello();
                        image.hello = Some((start.0, buf.to_vec()));
                    }
                    Ok(parse::Command::LoadHello) => {
                        if let Some((start, ref buf)) = image.hello {
                            let start = ::dynasmrt::AssemblyOffset(start);
                            let mut mmapbuf =
                                ::dynasmrt::mmap::MutableBuffer::new(buf.len()).unwrap();
                            mmapbuf.set_len(buf.len());
                            for (i, x) in buf.iter().enumerate() {
                                mmapbuf[i] = *x;
                            }
                            let exec_buf = mmapbuf.make_exec().unwrap();
                            let hello_fn: extern "win64" fn() =
                                unsafe { ::core::mem::transmute(exec_buf.ptr(start)) };
                            hello_fn();
                        } else {
                            println!("Machine code cache not initialized.");
                        }
                    }
                    Ok(parse::Command::Remount(path)) => {
                        println!("Path: {:?}", path);
                    }
                    Err(e) => eprintln!("parse error: {}", e),
                }

                input.clear();
            }
        }
        Opt::View { image } => {
            println!("{}", serde_json::to_string(&load_image(&image)?)?);
        }
    }
    Ok(())
}
