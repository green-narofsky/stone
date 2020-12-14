use ::core::marker::PhantomData;
use ::core::ops::Range;
#[repr(transparent)]
pub struct Tagged<DST: ?Sized, ID> {
    id: PhantomData<ID>,
    buf: DST,
}

impl<DST: ?Sized, ID> Tagged<DST, ID> {
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

impl<ID> TaggedDSTIndex<str, ID> for Range<usize> {
    type Output = Tagged<str, ID>;
    fn get(self, buf: &Tagged<str, ID>) -> Option<&Self::Output> {
        // SAFETY: Since we're creating this slice from our current allocated
        // object, it's definitely from the same allocated object as us,
        // so therefore it is allowed to have the same `ID` type argument.
        <str>::get(&buf.buf, self).map(|buf| unsafe { Tagged::from_untagged(buf) })
    }
    fn get_mut(self, buf: &mut Tagged<str, ID>) -> Option<&mut Self::Output> {
        // SAFETY: Same as in `self.get`.
        <str>::get_mut(&mut buf.buf, self).map(|buf| unsafe { Tagged::from_untagged_mut(buf) })
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
