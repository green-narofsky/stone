//! Generalizing this library's API to be usable on string slices as well.
use ::core::marker::PhantomData;
use ::core::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};
use ::core::ops::{Index, IndexMut};

/// A type tagged wrapper around `?Sized` types, built around
/// letting you compute offsets between references from single allocated objects.
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn slice_offsets() {
        let a = b"Hello, world!";
        let b: &Tagged<[u8], ()> = unsafe { Tagged::from_untagged(a) };
        let c = &b[1..];
        let d = &b[4..];
        assert_eq!(d.offset_from(c), 3);
    }
    #[test]
    fn item_offsets() {
        let a = b"Hello, world!";
        let b: &Tagged<[u8], ()> = unsafe { Tagged::from_untagged(a) };
        let c = &b[1];
        let d = &b[4];
        assert_eq!(d.offset_from(c), 3);
    }
}