#![cfg_attr(feature = "nightly", feature(min_const_generics))]

//! Attempting to establish a type level guarantee
//! that all slice references created and returned during parsing
//! point into the same underlying slice.
//!
//! This is needed for us to safely calculate offsets of those references
//! into the slice for the purpose of reporting information,
//! without either passing around offsets in place of or in addition to
//! the references we're throwing around.
//!
//! (I intend to do a few benchmarks to verify that this matters.)
//!
//! This is *almost* given to us for free, but
//! a misbehaving combinator might do something like `Box::leak`
//! to give back a reference with the appropriate lifetime.
use ::core::marker::PhantomData;
use ::trait_match::sealed;

#[cfg(feature = "nightly")]
mod nightly {
    use ::core::marker::PhantomData;
    use super::ParseSlice;
    #[derive(Debug)]
    #[repr(transparent)]
    struct ParseArray<T, ID, const N: usize> {
        id: PhantomData<ID>,
        buf: [T; N],
    }
    impl<T, ID, const N: usize> ParseArray<T, ID, N> {
        /// Constructs a `ParseArray<T, ID, N>` from a `[T; N]`.
        /// # Safety
        /// The `ID` type argument must be unique across the whole program.
        unsafe fn new(buf: [T; N]) -> Self {
            Self {
                id: PhantomData,
                buf,
            }
        }
        /// Creates a DST reference to this array.
        fn unsize(&self) -> &ParseSlice<T, ID> {
            unsafe { ::core::mem::transmute(&self.buf as &[T]) }
        }
        /// Creates a mutable DST reference to this array.
        fn unsize_mut(&mut self) -> &mut ParseSlice<T, ID> {
            unsafe { ::core::mem::transmute(&mut self.buf as &mut [T]) }
        }
        fn into_inner(self) -> [T; N] {
            self.buf
        }
        fn inner_ref(&self) -> &[T; N] {
            &self.buf
        }
        fn inner_ref_mut(&mut self) -> &mut [T; N] {
            &mut self.buf
        }
    }
    #[cfg(test)]
    #[test]
    fn unsizing() {
        let mut arr = ParseArray {
            id: PhantomData::<()>,
            buf: [1, 2, 3],
        };
        let a: &ParseSlice<i32, ()> = arr.unsize();
        println!("{:?}", a);
        let b: &mut ParseSlice<i32, ()> = arr.unsize_mut();
        println!("{:?}", b);
    }
}

/// A type tagged slice that makes taking the offset between references to it a safe operation.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParseSlice<T, ID> {
    /// A marker type that allows us to declare functions
    /// that only operate on references from a single allocated object.
    /// Or, even, functions that require specific arguments to
    /// be from the same allocated object, with others that are allowed
    /// to be from different allocated objects.
    id: PhantomData<ID>,
    buf: [T],
}
impl<T, ID> ParseSlice<T, ID> {
    /// Constructs a `&ParseSlice<T, ID>` from a `&[T]`.
    /// # Safety
    /// Must be from the same allocation as any other slice
    /// reference given the same `ID` type argument.
    pub unsafe fn from_slice(buf: &[T]) -> &ParseSlice<T, ID> {
        // SAFETY: ParseSlice<T, ID> is `#[repr(transparent)]` with
        // its only non ZST field being `[T]`. So, transmuting a
        // `&[T]` to a `&ParseSlice<T, _>` should be fine.
        #[allow(unused_unsafe)]
        unsafe { ::core::mem::transmute(buf) }
    }
    /// Constructs a `&mut ParseSlice<T, ID>` from a `&mut [T]`.
    /// # Safety
    /// Must be from the same allocation as any other slice reference
    /// given the same `ID` type argument.
    pub unsafe fn from_slice_mut(buf: &mut [T]) -> &mut ParseSlice<T, ID> {
        #[allow(unused_unsafe)]
        unsafe { ::core::mem::transmute(buf) }
    }
    pub fn slice_ref(&self) -> &[T] {
        &self.buf
    }
    pub fn slice_ref_mut(&mut self) -> &mut [T] {
        &mut self.buf
    }
    pub fn offset_from(&self, other: &Self) -> isize {
        // SAFETY: The safety requirement for constructing the argument
        // `ParseSlice`s is identical to the safety requirement
        // for invoking this method. They are from the same allocated object.
        unsafe { self.buf.as_ptr().offset_from(other.buf.as_ptr()) }
    }
    pub fn get<Idx: ParseSliceIndex<T, ID>>(&self, idx: Idx) -> Option<&Idx::Output> {
        idx.get(self)
    }
    pub fn get_mut<Idx: ParseSliceIndex<T, ID>>(&mut self, idx: Idx) -> Option<&mut Idx::Output> {
        idx.get_mut(self)
    }
}


// Work around limitation in my #[sealed] macro.
type RangeIndex =          ::core::ops::Range<usize>;
type RangeFromIndex =      ::core::ops::RangeFrom<usize>;
use ::core::ops::RangeFull;
type RangeInclusiveIndex = ::core::ops::RangeInclusive<usize>;
type RangeToIndex =        ::core::ops::RangeTo<usize>;
type RangeToInclusiveIndex =    ::core::ops::RangeToInclusive<usize>;

// Essentially stolen from the Rust standard library, haha.
// Unifies some inherent methods with `Index` and `IndexMut` implementation.
#[sealed(RangeIndex, RangeFromIndex, RangeFull,
         RangeInclusiveIndex, RangeToIndex, RangeToInclusiveIndex, usize)]
pub trait ParseSliceIndex<T, ID> {
    type Output: ?Sized;
    fn get(self, slice: &ParseSlice<T, ID>) -> Option<&Self::Output>;
    fn get_mut(self, slice: &mut ParseSlice<T, ID>) -> Option<&mut Self::Output>;
}

impl<T, ID, PI> ::core::ops::Index<PI> for ParseSlice<T, ID>
where PI: ParseSliceIndex<T, ID>,
{
    type Output = PI::Output;
    fn index(&self, index: PI) -> &Self::Output {
        index.get(self).unwrap()
    }
}
impl<T, ID, PI> ::core::ops::IndexMut<PI> for ParseSlice<T, ID>
where PI: ParseSliceIndex<T, ID>,
{
    fn index_mut(&mut self, index: PI) -> &mut Self::Output {
        index.get_mut(self).unwrap()
    }
}

macro_rules! impl_range_index {
    ($t:ty) => {
        impl<T, ID> ParseSliceIndex<T, ID> for $t {
            type Output = ParseSlice<T, ID>;
            fn get(self, slice: &ParseSlice<T, ID>) -> Option<&Self::Output> {
                // SAFETY: Since we're creating this slice from our current allocated
                // object, it's definitely from the same allocated object as us,
                // so therefore it is allowed to have the same `ID` type argument.
                slice.buf.get(self).map(|buf| unsafe { ParseSlice::from_slice(buf) })
            }
            fn get_mut(self, slice: &mut ParseSlice<T, ID>) -> Option<&mut Self::Output> {
                // SAFETY: Same as in `self.get`.
                slice.buf.get_mut(self).map(|buf| unsafe { ParseSlice::from_slice_mut(buf) })
            }
        }
    }
}

impl_range_index!(::core::ops::Range<usize>);
impl_range_index!(::core::ops::RangeFrom<usize>);
impl_range_index!(::core::ops::RangeFull);
impl_range_index!(::core::ops::RangeInclusive<usize>);
impl_range_index!(::core::ops::RangeTo<usize>);
impl_range_index!(::core::ops::RangeToInclusive<usize>);

impl<T, ID> ParseSliceIndex<T, ID> for usize {
    type Output = T;
    fn get(self, slice: &ParseSlice<T, ID>) -> Option<&Self::Output> {
        slice.buf.get(self)
    }
    fn get_mut(self, slice: &mut ParseSlice<T, ID>) -> Option<&mut Self::Output> {
        slice.buf.get_mut(self)
    }
}

/// Safely create a `&ParseSlice`.
/// ```ignore
/// let a = vec![1, 2, 3];
/// let b = parse_ref!(&a, Lol);
/// let c = &b[1..2];
/// assert_eq!(c.offset_from(b), 1);
/// ```
/// Takes a slice reference and identifier as arguments.
/// The identifier does not need to be unique,
/// but making it so will make type mismatch compile errors nicer to read.
#[macro_export]
macro_rules! parse_ref {
    ($e:expr, $id:ident) => {
        {
            #[derive(Copy, Clone)]
            struct $id;
            // SAFETY: Since the `$id` type is unusable outside
            // of this block, it is therefore unique across the whole program.
            // Since it is globally unique, we meet the safety
            // requirement of ParseRef::new, of our coming from the same
            // allocated object as any other ParseRef with the same type tag.
            unsafe { crate::ParseSlice::<_, $id>::from_slice($e) }
        }
    }
}
/// Safely create a `&mut ParseSlice`, as with [`parse_ref`].
#[macro_export]
macro_rules! parse_ref_mut {
    ($e:expr, $id:ident) => {
        {
            struct $id;
            // SAFETY: Same as with `parse_ref!`.
            unsafe { crate::ParseSlice::<_, $id>::from_slice_mut($e) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseSlice, parse_ref};
    #[test]
    fn get_offset() {
        let a = "Hello, world.";
        let b = a.as_bytes();
        let c = &b[3..];
        // SAFETY: These two pointers are from the same allocated object.
        let offset = unsafe { c.as_ptr().offset_from(b.as_ptr()) };
        assert_eq!(offset, 3);
    }
    // This test intentionally triggers UB, just to make sure Miri will catch
    // the issue that this crate exists to prevent.
    #[cfg(miri)]
    #[test]
    #[ignore]
    fn should_fail() {
        let a = "Hello, world.";
        let b = "Hello, world. Again!";
        let c = a.as_bytes().as_ptr();
        let d = b.as_bytes().as_ptr();
        // SAFETY: THIS INTENTIONALLY CAUSES UNDEFINED BEHAVIOR.
        // The two pointers are from separate allocated objects.
        let offset = unsafe { c.offset_from(d) };
        println!("Offset: {}", offset);
    }
    #[cfg(test)]
    #[test]
    fn safe_get_offset() {
        let a = vec![1, 2, 3];
        let b = parse_ref!(&a, Lol);
        let c = &b[1..2];
        assert_eq!(c.offset_from(b), 1);
    }
    #[cfg(test)]
    #[test]
    fn indexing() {
        let a = vec![1, 2, 3];
        let b = parse_ref!(&a, Lol);
        let c = &b[1];
    }
}
