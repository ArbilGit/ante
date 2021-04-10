use crate::parser::ast::{ Ast, AstId };
use std::alloc::{ alloc, dealloc, Layout };

/// Describes a Vec that grows downward in memory
pub struct AstVec<'a> {
    ptr: *mut Ast<'a>,
    pub len: usize,
    pub cap: usize,
}

impl<'a> AstVec<'a> {
    pub fn new() -> AstVec<'a> {
        let mut vec = AstVec { ptr: std::ptr::null_mut(), len: 0, cap: 16 };
        vec.reserve(16);
        vec
    }

    pub fn push(&mut self, node: Ast<'a>) -> AstId {
        if self.len == self.cap {
            self.reserve(self.cap);
        }

        unsafe {
            let ptr = self.ptr.offset(self.cap as isize - self.len as isize);
            std::ptr::write(ptr, node);
        }

        let ret = self.len;
        self.len += 1;
        AstId(ret)
    }

    pub fn get(&mut self, index: usize) -> &mut Ast<'a> {
        unsafe {
            self.ptr.offset(self.cap as isize - index as isize).as_mut().unwrap()
        }
    }

    pub fn as_ptr(&self) -> *const Ast<'a> {
        self.ptr
    }

    fn reserve(&mut self, count: usize) {
        self.cap += count;
        unsafe {
            let dest = alloc(Layout::array::<Ast<'a>>(self.cap).unwrap()) as *mut Ast<'a>;

            if self.len > 0 {
                std::ptr::copy_nonoverlapping(self.ptr, dest.offset(count as isize), count);
            }

            dealloc(self.ptr as *mut u8, Layout::array::<Ast<'a>>(self.cap - count).unwrap());
            self.ptr = dest;
        }
    }
}
