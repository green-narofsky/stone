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

impl<DST: ?Sized, ID> Tagged<DST, ID> {
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

impl<T, ID, Idx> Index<Idx> for Tagged<T, ID>
where Idx: TaggedDSTIndex<T, ID>
{
    type Output = Idx::Output;
    fn index(&self, index: Idx) -> &Self::Output {
        index.get(self).unwrap()
    }
}
impl<T, ID, Idx> IndexMut<Idx> for Tagged<T, ID>
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
    fn strings() {
        let string = "Hello, world!";
        let a: &'static Tagged<str, ()> = unsafe { Tagged::from_untagged(string) };
    }
}
