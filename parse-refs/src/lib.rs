//! Attempting to establish a type level guarantee
//! that all slice references created and returned during parsing
//! point into the same underlying slice.
//!
//! This is needed for us to safely calculate offsets between those references.
use ::core::marker::PhantomData;
use ::core::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};
use ::core::ops::{Index, IndexMut};
use ::core::ops::{Deref, DerefMut};

/// A type tagged wrapper around `?Sized` types, built around
/// letting you compute offsets between references from single allocated objects.
// This is intentionally not Copy or Clone.
// Copying a `Tagged<DST, ID>` to a new location immediately breaks its single invariant.
// Note that *moving* a `Tagged<DST, ID>` doesn't break its invariant, because
// moving requires unique access. Unique access, given the invariant is otherwise upheld,
// implies that there exist no references with the same `ID` argument.
// That in turn means that although moving changes the location of the `Tagged<DST, ID>`,
// it retains the property of being the same allocated object as all other `Tagged<DST, ID>`s
// with the same `ID` argument, since there are no others.
// This is actually not entirely the case. If you can move a *segment* of a `Tagged<DST, ID>`
// out while retaining the same `ID` type argument, you break the invariant.
// For example, split_at_mut on slices could give you this, if you also had a way to...
// create a `Tagged<DST, ID>` with that same ID argument ahead of time to replace it with.
// Fortunately, there is no way to do this with safe code.
// We must simply remember to be careful of that: The *only* way to construct these safely
// *must* be to either:
//  A) Get it from one of the `tag_*` macros.
//  B) Make it out of one you already have.
// TODO: Figure out if Cell types are relevant.
#[repr(transparent)]
pub struct Tagged<DST: ?Sized, ID> {
    id: PhantomData<ID>,
    buf: DST,
}

/// This trait determines what offsets are calculated in units of,
/// when using the `Tagged::offset_from` method.
/// Implementors of this trait must guarantee that all `&Tagged` references
/// to them with the same `ID` type argument are separated by exact
/// multiples of the size of `TaggedUnit::Item`.
pub unsafe trait TaggedUnit {
    type Item;
}
unsafe impl<T> TaggedUnit for [T] {
    type Item = T;
}
unsafe impl TaggedUnit for str {
    type Item = u8;
}
/// All `Sized` types are their own unit.
// Note that the `Sized` bound here is implicit.
unsafe impl<T> TaggedUnit for T {
    type Item = T;
}

impl<DST: ?Sized, ID> Tagged<DST, ID>
where DST: TaggedUnit,
{
    // This safety condition may need to be strengthened for this library to be sound.
    // To "the ID type argument must be unique",
    // or the slightly weaker "must have the same provenance as any other
    // tagged reference given the same ID type argument".
    /// Constructs a `&Tagged<DST, ID>` from a `&DST`.
    /// # Safety
    /// Must be from the same allocation as any other tagged
    /// reference given the same `ID` type argument.
    pub unsafe fn from_untagged(buf: &DST) -> &Self {
        #[allow(unused_unsafe)]
        unsafe { ::core::mem::transmute(buf) }
    }
    /// Constructs a `&mut Tagged<DST, ID>` from a `&mut DST`.
    /// # Safety
    /// Must be from the same allocation as any other tagged
    /// reference given the same `ID` type argument.
    pub unsafe fn from_untagged_mut(buf: &mut DST) -> &mut Self {
        #[allow(unused_unsafe)]
        unsafe { ::core::mem::transmute(buf) }
    }
    pub fn offset_from(&self, other: &Tagged<DST, ID>) -> isize {
        // This method is what this whole library exists to enable.
        // If this is unsound, rewrite the rest of the library.
        // SAFETY: Meeting the safey requirement for constructing
        // `&Tagged<DST, ID>`s implies meeting the safety requirement
        // for invoking `offset_from` between two raw pointers derived
        // from those references.
        unsafe {
            (&self.buf as *const DST as *const <DST as TaggedUnit>::Item)
                .offset_from(&other.buf as *const DST as *const <DST as TaggedUnit>::Item)
        }
    }

    pub fn get<Idx: TaggedDSTIndex<DST, ID>>(&self, idx: Idx) -> Option<&Idx::Output> {
        idx.get(self)
    }
    pub fn get_mut<Idx: TaggedDSTIndex<DST, ID>>(&mut self, idx: Idx) -> Option<&mut Idx::Output> {
        idx.get_mut(self)
    }
}

pub trait TaggedDSTIndex<DST: ?Sized, ID> {
    type Output: ?Sized;
    fn get(self, buf: &Tagged<DST, ID>) -> Option<&Self::Output>;
    fn get_mut(self, buf: &mut Tagged<DST, ID>) -> Option<&mut Self::Output>;
}

// This macro has only been verified for the `[T]` and `str` DST types,
// for the range types used below in particular.
macro_rules! impl_range_dst_index {
    ($dst:ty $( [ $par:ident ])? , $t:ty) => {
        impl<$($par,)? ID> TaggedDSTIndex<$dst, ID> for $t {
            type Output = Tagged<$dst, ID>;
            fn get(self, buf: &Tagged<$dst, ID>) -> Option<&Self::Output> {
                // SAFETY: Since we're creating this slice from our current allocated
                // object, it's definitely from the same allocated object as us,
                // so therefore it is allowed to have the same `ID` type argument.
                <$dst>::get(&buf.buf, self).map(|buf| unsafe { Tagged::from_untagged(buf) })
            }
            fn get_mut(self, buf: &mut Tagged<$dst, ID>) -> Option<&mut Self::Output> {
                // SAFETY: Same as directly above.
                <$dst>::get_mut(&mut buf.buf, self).map(|buf| unsafe { Tagged::from_untagged_mut(buf) })
            }
        }
    }
}

impl_range_dst_index!(str, Range<usize>);
impl_range_dst_index!(str, RangeFrom<usize>);
impl_range_dst_index!(str, RangeFull);
impl_range_dst_index!(str, RangeInclusive<usize>);
impl_range_dst_index!(str, RangeTo<usize>);
impl_range_dst_index!(str, RangeToInclusive<usize>);
impl_range_dst_index!([T][T], Range<usize>);
impl_range_dst_index!([T][T], RangeFrom<usize>);
impl_range_dst_index!([T][T], RangeFull);
impl_range_dst_index!([T][T], RangeInclusive<usize>);
impl_range_dst_index!([T][T], RangeTo<usize>);
impl_range_dst_index!([T][T], RangeToInclusive<usize>);

impl<T, ID> TaggedDSTIndex<[T], ID> for usize {
    type Output = Tagged<T, ID>;
    fn get(self, buf: &Tagged<[T], ID>) -> Option<&Self::Output> {
        // If the provenance of `&Tagged<T, ID>` is smaller than the provenance of
        // `&Tagged<[T], ID>` here, this may be unsound-
        // if, additionally, the allocated objects mentioned in `offset_from`'s docs
        // correspond to pointer provenances.
        // If that's the case, change `Self::Output` to `T` and remove the
        // `.map(...)` in here.
        // Same goes for `get_mut`, of course.

        // SAFETY: Since we're creating this reference to an item in our slice from...
        // our current slice, it's definitely from the same allocated object as us.
        // So, it's allowed to have the same `ID` type argument.
        <[T]>::get(&buf.buf, self).map(|item| unsafe { Tagged::from_untagged(item) })
    }
    fn get_mut(self, buf: &mut Tagged<[T], ID>) -> Option<&mut Self::Output> {
        // SAFETY: Since we're creating this reference to an item in our slice from...
        // our current slice, it's definitely from the same allocated object as us.
        // So, it's allowed to have the same `ID` type argument.
        <[T]>::get_mut(&mut buf.buf, self).map(|item| unsafe { Tagged::from_untagged_mut(item) })
    }
}

impl<T: ?Sized, ID, Idx> Index<Idx> for Tagged<T, ID>
where Idx: TaggedDSTIndex<T, ID>
{
    type Output = Idx::Output;
    fn index(&self, index: Idx) -> &Self::Output {
        index.get(self).unwrap()
    }
}
impl<T: ?Sized, ID, Idx> IndexMut<Idx> for Tagged<T, ID>
where Idx: TaggedDSTIndex<T, ID>
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        index.get_mut(self).unwrap()
    }
}

// SEMVER: Since Rust may add new methods to the slice types at any time,
// adding inherent methods to the `Tagged<DST, ID>` type will be a SemVer break.
// Since the given reference no longer has its tag,
// there is no way it can be used to break our invariant.
impl<T: ?Sized, ID> Deref for Tagged<T, ID> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}
// Since the given reference no longer has its tag,
// there is no way it can be used to break our invariant.
impl<T: ?Sized, ID> DerefMut for Tagged<T, ID> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}

/// Safely construct a tagged reference.
#[macro_export]
macro_rules! tag_ref {
    // Since the tag type can't be named outside its block expression,
    // it doesn't matter what it's name is.
    // Given that, we can provide a somewhat descriptive default.
    ($e:expr) => { $crate::tag_ref!($e, UnnamedTag) };
    ($e:expr , $id:ident) => {
        {
            struct $id;
            // SAFETY: Since the `$id` type is unusable outside of this block,
            // it is therefore unique across the whole program.
            // Global uniqueness implies meeting the requirement of only
            // sharing a type tag with references to the same allocated object.
            unsafe { $crate::Tagged::<_, $id>::from_untagged($e) }
        }
    }
}

/// Safely construct a tagged mutable reference.
#[macro_export]
macro_rules! tag_ref_mut {
    // Since the tag type can't be named outside its block expression,
    // it doesn't matter what it's name is.
    // Given that, we can provide a somewhat descriptive default.
    ($e:expr) => { $crate::tag_ref_mut!($e, UnnamedTag) };
    ($e:expr , $id:ident) => {
        {
            struct $id;
            // SAFETY: Since the `$id` type is unusable outside of this block,
            // it is therefore unique across the whole program.
            // Global uniqueness implies meeting the requirement of only
            // sharing a type tag with references to the same allocated object.
            unsafe { $crate::Tagged::<_, $id>::from_untagged_mut($e) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Tagged, tag_ref as parse_ref};
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
        let b = parse_ref!(&*a, Lol);
        let c = &b[1..2];
        assert_eq!(c.offset_from(b), 1);
    }
    #[cfg(test)]
    #[test]
    fn indexing() {
        let a = vec![1, 2, 3];
        let b = parse_ref!(&*a, Lol);
        let c = &b[1];
    }
}
