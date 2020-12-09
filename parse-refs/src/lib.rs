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
    #[repr(transparent)]
    struct ParseArray<T, ID, const N: usize> {
        id: PhantomData<ID>,
        buf: [T; N],
    }
    impl<T, ID, const N: usize> ParseArray<T, ID, N> {
        fn unsize(&self) -> &ParseSlice<T, ID> {
            unsafe { ::core::mem::transmute(&self.buf as &[T]) }
        }
    }
    #[repr(transparent)]
    struct ParseSlice<T, ID> {
        id: PhantomData<ID>,
        buf: [T],
    }
    #[cfg(test)]
    #[test]
    fn unsizing() {
        let arr = ParseArray {
            id: PhantomData::<()>,
            buf: [1, 2, 3],
        };
        let a: &ParseSlice<i32, ()> = arr.unsize();
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
struct ParseBuf<'a, T, ID> {
    buf: &'a [T],
    /// A marker type that allows us to declare functions
    /// that only operate on references from a single allocated object.
    /// Or, even, functions that require specific arguments to
    /// be from the same allocated object, with others that are allowed
    /// to be from different allocated objects.
    id: PhantomData<ID>,
}
impl<'a, T, ID> ParseBuf<'a, T, ID> {
    unsafe fn new(buf: &'a [T]) -> Self {
        Self {
            id: PhantomData,
            buf,
        }
    }
}

macro_rules! into_parse_buf {
    ($e:expr, $id:ident) => {
        {
            #[derive(Copy, Clone)]
            struct $id {
                _priv: [(); 0],
            }

            
        }
    }
}
// Implement Index and IndexMut to provide these custom slice references.
// If `pointer::offset_from` works backwards, we only need one type.
// Otherwise, we need to distinguish between the origin pointer and
// the rest.
// This may be affected by slice references' provenance automatically shrinking.
// In that case, we may be able to avoid that by holding onto raw pointers directly.
// It is unclear to me what effects that would have on LLVM's ability to optimize,
// however.
struct ParseRef<'a, T, ID> {
    buf: &'a [T],
    id: ID,
}



#[cfg(test)]
mod tests {
    #[test]
    fn get_offset() {
        let a = "Hello, world.";
        let b = a.as_bytes();
        let c = &b[3..];
        // SAFETY: These two pointers are from the same allocated object.
        let offset = unsafe { c.as_ptr().offset_from(b.as_ptr()) };
        println!("Offset: {}", offset);
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
}
