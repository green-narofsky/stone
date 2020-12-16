//! Attempting to establish a type level guarantee
//! that all slice references created and returned during parsing
//! point into the same underlying slice.
//!
//! This is needed for us to safely calculate offsets between those references.
pub mod string;

#[cfg(test)]
mod tests {
    use super::{string::Tagged, tag_ref as parse_ref};
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
