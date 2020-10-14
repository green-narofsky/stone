// TODO: A basic REPL.
// A goal is that we have a "UB exception", where in interpreted code
// all UB aborts the program.
// Much like Miri for Rust.
use ::anyhow::Result;
use ::core::convert::TryFrom;
use ::lasso::{Rodeo, Spur};
use ::serde::{Deserialize, Serialize};
use ::std::collections::HashMap;
use ::std::fs::File;
use ::std::path::{Path, PathBuf};
use ::structopt::StructOpt;
mod jit;
mod parse;

/// Niche optimization, lol.
mod niche {
    // TODO: macro-ify this so we have NonMax- types and conversions
    // for every width integer
    use ::core::convert::TryFrom;
    use ::core::num::NonZeroUsize;
    use ::core::ops::{Add, AddAssign};
    use ::serde::{Deserialize, Serialize};
    /// Like `NonZeroUsize`, but the niche is `usize::MAX`.
    #[derive(Serialize, Deserialize, Debug, Clone, Copy)]
    pub(crate) struct NonMaxUsize {
        internal: NonZeroUsize,
    }
    impl NonMaxUsize {
        pub(crate) unsafe fn new_unchecked(val: usize) -> Self {
            Self {
                internal: NonZeroUsize::new_unchecked(!val),
            }
        }
    }
    impl TryFrom<usize> for NonMaxUsize {
        type Error = ::core::num::TryFromIntError;
        fn try_from(val: usize) -> Result<Self, Self::Error> {
            Ok(Self {
                internal: NonZeroUsize::try_from(!val)?,
            })
        }
    }
    impl From<NonMaxUsize> for usize {
        fn from(val: NonMaxUsize) -> Self {
            !Self::from(val.internal)
        }
    }
    impl Add<usize> for NonMaxUsize {
        type Output = NonMaxUsize;
        fn add(self, rhs: usize) -> Self::Output {
            Self::try_from(usize::from(self) + rhs).expect("expected non-max usize")
        }
    }
    impl AddAssign<usize> for NonMaxUsize {
        fn add_assign(&mut self, rhs: usize) {
            *self = *self + rhs;
        }
    }
}

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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
enum Type {
    Integer,
    Float,
    Boolean,
    Unit,
    String,
    Pointer(PointerKind, Box<Type>),
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

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
enum PointerKind {
    Shared,
    Unique,
}

/// A pointer to a chunk.
#[derive(Serialize, Deserialize, Debug)]
struct Pointer {
    kind: PointerKind,
    location: Location,
    id: PointerId,
    chunk_type: Type,
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
    Pointer(Pointer),
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
            Value::Pointer(Pointer {
                kind, chunk_type, ..
            }) => Type::Pointer(*kind, Box::new(chunk_type.clone())),
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

/// Tracking the existence of valid pointers to a chunk.
#[derive(Serialize, Deserialize, Debug)]
enum PointerCount {
    Shared {
        // Inclusive lower bound.
        lower_bound: u64,
        // Exclusive upper bound.
        next: u64,
    },
    Unique {
        id: u64,
    },
    None {
        next: u64,
    },
}
impl PointerCount {
    fn new() -> Self {
        Self::None { next: 0 }
    }
    /// Create a new shared pointer.
    fn shared(&mut self) -> PointerId {
        match self {
            Self::Shared { next, .. } => {
                let id = PointerId { id: *next };
                *next += 1;
                id
            }
            Self::Unique { id } => {
                let id = *id;
                *self = Self::Shared {
                    lower_bound: id + 1,
                    next: id + 2,
                };
                PointerId { id: id + 1 }
            }
            Self::None { next } => {
                let id = *next;
                *self = Self::Shared {
                    lower_bound: id,
                    next: id + 1,
                };
                PointerId { id }
            }
        }
    }
    /// Create a new unique pointer.
    fn unique(&mut self) -> PointerId {
        match self {
            Self::Shared { next, .. } => {
                let id = *next + 1;
                *self = Self::Unique { id };
                PointerId { id }
            }
            Self::Unique { id } => {
                let new_id = *id + 1;
                *id = new_id;
                PointerId { id: new_id }
            }
            Self::None { next } => {
                let id = *next;
                *self = Self::Unique { id };
                PointerId { id }
            }
        }
    }
    /// Load a pointer invalidation.
    fn invalidate(&mut self, invalidation: PointerInvalidation) {
        use PointerInvalidation::*;
        match (invalidation, &self) {
            (OwningRead, Self::Unique { id }) => {
                let next = *id + 1;
                *self = Self::None { next };
            }
            (OwningRead, _) => (),
            (OwningMutate, Self::Shared { next, .. }) => *self = Self::None { next: *next },
            (OwningMutate, Self::Unique { id }) => *self = Self::None { next: *id + 1 },
            (OwningMutate, _) => (),
        }
    }
    /// Check if a given combination of pointer kind and ID is valid.
    fn is_valid(&self, kind: PointerKind, ptr: PointerId) -> bool {
        match (self, kind) {
            (Self::Shared { lower_bound, next }, PointerKind::Shared) => {
                ptr.id >= *lower_bound && ptr.id < *next
            }
            (Self::Unique { id }, PointerKind::Unique) => ptr.id == *id,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum PointerInvalidation {
    OwningRead,
    OwningMutate,
}

/// Internal identifier for a pointer, used for validation.
#[derive(Serialize, Deserialize, Debug)]
struct PointerId {
    id: u64,
}

/// A place in memory.
#[derive(Serialize, Deserialize, Debug)]
struct Chunk {
    value: Value,
    chunk_type: Type,
    pointers: PointerCount,
}
struct TypeMismatch;
impl Chunk {
    fn new(value: Value, chunk_type: Type) -> Result<Self, TypeMismatch> {
        if value.type_of() == chunk_type {
            Ok(Self {
                value,
                chunk_type,
                pointers: PointerCount::new(),
            })
        } else {
            Err(TypeMismatch)
        }
    }
    fn from_val(value: Value) -> Self {
        Self {
            chunk_type: value.type_of(),
            pointers: PointerCount::new(),
            value,
        }
    }
}

// We'll want to avoid saving freed chunks in images, later.
// But for now, we keep them around so we don't have to rewrite all
// existing `Location`s in the process of saving.
#[derive(Serialize, Deserialize)]
enum ChunkSlot {
    Used {
        chunk: Chunk,
        generation: u64,
    },
    Free {
        // Might as well store the free list embedded in
        // the vector of chunks.
        // We could just as well only increment the generation
        // when chunks are freed, and store a separate table
        // of free slots. This would slightly increase the speed
        // of chunk accesses, and decrease the speed of chunk allocation.
        // This slight increase would be due to only having to check
        // the generation, instead of having to check first if the
        // slot is in use before checking the generation.
        next: Option<niche::NonMaxUsize>,
        generation: u64,
    },
}
impl ChunkSlot {
    fn empty(next: Option<niche::NonMaxUsize>) -> Self {
        Self::Free {
            generation: 0,
            next,
        }
    }
    fn used(chunk: Chunk) -> Self {
        Self::Used {
            generation: 0,
            chunk,
        }
    }
}

/// The position of a place in memory.
#[derive(Serialize, Deserialize, Debug)]
struct Location {
    offset: niche::NonMaxUsize,
    generation: u64,
}

/// A big heap of data, lol.
// This needs to support destroying chunks,
// and all pointers to destroyed chunks are invalidated.
// We could accomplish this by giving chunks an invalidated state,
// and simply checking that on every access.
// That would become untenable in situations with frequent reallocation,
// however.
// We could put a layer of indirection between chunks
// and pointers, with some sort of global map that uses what would ordinarily be
// offsets as keys instead.
// This shrinks the memory usage considerably, but slows down pointer dereferences.
// Considering how frequent pointers will be, and how free to use I intend to
// make them in the compiled version, this isn't a very good option either.
// We could instead store an extra generation key in each chunk and pointer,
// which would allow us to reuse the space from destroyed chunks,
// while not requiring a map lookup for every access.
// This has the cost of requiring a few more bytes of storage for every
// chunk and pointer, but the speedup should make up for the (relatively tiny)
// cost of copying those around.
#[derive(Serialize, Deserialize)]
struct Memory {
    // This is inefficient, but necessary for making sure
    // the interpreted version and compiled version share the same semantics.
    // I have some details written out in another document.
    chunks: Vec<ChunkSlot>,
    free: Option<niche::NonMaxUsize>,
}
/// Currently this is impossible, but I look forward
/// to when fallible allocation becomes available.
enum AllocError {}

/// Error for when an invalid `Location` is used.
// This is UB!
struct InvalidLocation;

impl Memory {
    fn new() -> Self {
        Self {
            chunks: Vec::new(),
            free: None,
        }
    }
    /// Create a new chunk containing the given value.
    // This should be the only way to actually obtain a `Location`.
    fn insert(&mut self, val: Value) -> Result<Location, AllocError> {
        match self.free {
            Some(x) => {
                let slot: &mut ChunkSlot = &mut self.chunks[usize::from(x)];
                let (generation, next) = match slot {
                    ChunkSlot::Free { generation, next } => (*generation + 1, *next),
                    ChunkSlot::Used { .. } => unreachable!("attempted reuse of non-free chunk"),
                };
                self.free = next;
                *slot = ChunkSlot::Used {
                    chunk: Chunk::from_val(val),
                    generation,
                };
                Ok(Location {
                    offset: x,
                    generation,
                })
            }
            None => {
                // SAFETY: The `Vec` length limit is actually `isize::MAX`,
                // which is smaller than `usize::MAX`. Therefore,
                // there is no way the returned value of `self.chunks.len()` is `usize::MAX`.
                let offset = unsafe { niche::NonMaxUsize::new_unchecked(self.chunks.len()) };
                self.chunks.push(ChunkSlot::used(Chunk::from_val(val)));
                Ok(Location {
                    generation: 0,
                    offset,
                })
            }
        }
    }
    /// A `Location` is valid if and only if the following conditions are met:
    ///  - Its `offset` points to an existing `ChunkSlot`.
    ///  - Its `generation` key matches the corresponding `ChunkSlot`'s `generation` key.
    ///  - The indicated `ChunkSlot` is alive.
    ///
    /// Note that the first condition in that list is currently infallible,
    /// due to restrictions on the creation of `Location`s.
    fn validate_location(&self, target: Location) -> Result<(), InvalidLocation> {
        match self.chunks[usize::from(target.offset)] {
            ChunkSlot::Used { generation, .. } if generation == target.generation => Ok(()),
            _ => Err(InvalidLocation),
        }
    }
    /// Replace the value a chunk contains with a new one.
    /// Returns the old value of the destination chunk.
    fn replace(&mut self, dest: Location, src: Value) -> Result<Value, InvalidLocation> {
        todo!("memory value replacement")
    }
    /// Read the value in a chunk.
    fn read(&self, src: Location) -> Result<Value, InvalidLocation> {
        todo!("reading memory")
    }
    /// Swap the values of two chunks.
    fn swap(&mut self, x: Location, y: Location) -> Result<(), InvalidLocation> {
        todo!("memory swap")
    }
    /// Destroy a chunk.
    fn destroy(&mut self, loc: Location) -> Result<(), InvalidLocation> {
        todo!("memory chunk destruction")
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
    /// Memory! Finally, we can store stuff.
    data: Memory,
    // bindex: BinaryIndex,
    hello: Option<(usize, Vec<u8>)>,
}
impl Image {
    fn new() -> Self {
        Self {
            strings: Rodeo::new(),
            bindings: Vec::new(),
            data: Memory::new(),
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
                                match &binding.value {
                                    Value::Integer(int) => println!("{}", int),
                                    Value::Float(float) => println!("{}", float),
                                    Value::Boolean(boolean) => println!("{}", boolean),
                                    Value::Unit => println!("()"),
                                    Value::String(key) => {
                                        println!("{}", image.strings.resolve(&key))
                                    }
                                    Value::Type(ty) => println!("{:?}", ty),
                                    Value::Pointer(Pointer {
                                        kind: PointerKind::Shared,
                                        chunk_type,
                                        ..
                                    }) => println!("*{:?}", chunk_type),
                                    Value::Pointer(Pointer {
                                        kind: PointerKind::Unique,
                                        chunk_type,
                                        ..
                                    }) => println!("*uniq {:?}", chunk_type),
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
