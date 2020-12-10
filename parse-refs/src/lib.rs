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

#[derive(Debug)]
#[repr(transparent)]
struct ParseSlice<T, ID> {
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
    unsafe fn from_slice(buf: &[T]) -> &ParseSlice<T, ID> {
        unsafe { ::core::mem::transmute(buf) }
    }
    /// Constructs a `&mut ParseSlice<T, ID>` from a `&mut [T]`.
    /// # Safety
    /// Must be from the same allocation as any other slice reference
    /// given the same `ID` type argument.
    unsafe fn from_slice_mut(buf: &mut [T]) -> &mut ParseSlice<T, ID> {
        unsafe { ::core::mem::transmute(buf) }
    }
    fn slice_ref(&self) -> &[T] {
        &self.buf
    }
    fn slice_ref_mut(&mut self) -> &mut [T] {
        &mut self.buf
    }
    fn offset_from(&self, other: &Self) -> isize {
        // SAFETY: The safety requirement for constructing the argument
        // `ParseSlice`s is identical to the safety requirement
        // for invoking this method. They are from the same allocated object.
        unsafe { self.buf.as_ptr().offset_from(other.buf.as_ptr()) }
    }
    // TODO: implement indexing methods
}

impl<T, ID> ::core::ops::Index<::core::ops::Range<usize>> for ParseSlice<T, ID> {
    type Output = ParseSlice<T, ID>;
    fn index(&self, index: ::core::ops::Range<usize>) -> &Self::Output {
        // SAFETY: Since we're creating this slice from our current allocated
        // object, it's definitely from the same allocated object as us,
        // so therefore it is allowed to have the same `ID` type argument.
        unsafe { ParseSlice::from_slice(self.buf.index(index)) }
    }
}

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
            unsafe { ParseSlice::<_, $id>::from_slice($e) }
        }
    }
}
#[macro_export]
macro_rules! parse_ref_mut {
    ($e:expr, $id:ident) => {
        struct $id;
        // SAFETY: Same as with `parse_ref!`.
        unsafe { ParseSlice::<_, $id>::from_slice_mut($e) }
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
}
