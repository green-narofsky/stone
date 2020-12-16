//! Generalizing this library's API to be usable on string slices as well.

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn slice_offsets() {
        let a = b"Hello, world!";
        let b: &Tagged<[u8], _> = tag_ref!(a);
        let c = &b[1..];
        let d = &b[4..];
        assert_eq!(d.offset_from(c), 3);
    }
    #[test]
    fn vec_offsets() {
        let a = vec![1, 2, 3];
        let b = tag_ref!(&*a);
        let c = &b[1..2];
        assert_eq!(c.offset_from(b), 1);
    }
    #[test]
    fn indexing() {
        let a = vec![1, 2, 3];
        let b = tag_ref!(&*a);
        let c = &b[1];
    }
    #[test]
    fn item_offsets() {
        let a = b"Hello, world!";
        let b: &Tagged<[u8], _> = tag_ref!(a);
        let c = &b[1];
        let d = &b[4];
        assert_eq!(d.offset_from(c), 3);
    }
}
