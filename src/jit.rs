//! Let's try some JIT compiling.
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

fn make_x64_hello() -> (dynasmrt::mmap::ExecutableBuffer, dynasmrt::AssemblyOffset) {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let string = "Hello, world.";
    dynasm!(ops
            ; .arch x64
            ; ->hello:
            ; .bytes string.as_bytes()
    );
    let hello = ops.offset();

    dynasm!(ops
            ; .arch x64
            ; lea rcx, [->hello]
            ; xor edx, edx
            ; mov dl, BYTE string.len() as i8
            ; mov rax, QWORD print as i64
            ; sub rsp, BYTE 0x28
            ; call rax
            ; add rsp, BYTE 0x28
            ; ret
    );

    let buf = ops.finalize().unwrap();
    (buf, hello)
}

pub fn make_x64_linux_hello() -> (dynasmrt::mmap::ExecutableBuffer, dynasmrt::AssemblyOffset)  {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let string = "Hello, world.\n";
    dynasm!(ops
            ; .arch x64
            ; ->hello:
            ; .bytes string.as_bytes()
    );
    let hello = ops.offset();
    dynasm!(ops
            ; .arch x64
            ; mov rdx, string.len() as i32
            ; lea rsi, [->hello]
            ; mov rax, 1   // Write command?
            ; mov rdi, rax // Set destination to stdout
            ; syscall
            ; ret
    );
    let buf = ops.finalize().unwrap();
    (buf, hello)
}

pub fn x64_hello() {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let string = "Hello, world.";
    dynasm!(ops
            ; .arch x64
            ; ->hello:
            ; .bytes string.as_bytes()
    );
    let hello = ops.offset();

    dynasm!(ops
            ; .arch x64
            ; lea rcx, [->hello]
            ; xor edx, edx
            ; mov dl, BYTE string.len() as i8
            ; mov rax, QWORD print as i64
            ; sub rsp, BYTE 0x28
            ; call rax
            ; add rsp, BYTE 0x28
            ; ret
    );

    let buf = ops.finalize().unwrap();
    let hello_fn: extern "win64" fn() -> bool = unsafe { ::core::mem::transmute(buf.ptr(hello)) };
    assert!(hello_fn());
}

pub extern "win64" fn print(buffer: *const u8, length: u64) -> bool {
    use std::{io, slice};
    use std::io::Write;
    io::stdout()
        .write_all(unsafe { slice::from_raw_parts(buffer, length as usize) })
        .is_ok()
}

#[cfg(test)]
mod test {
    #[test]
    fn it_works() {
        super::x64_hello();
    }
    #[test]
    fn maybe() {
        let (buf, start) = super::make_x64_hello();
        let hello_fn: extern "win64" fn() -> bool = unsafe { ::core::mem::transmute(buf.ptr(start)) };
        assert!(hello_fn());
    }
}
