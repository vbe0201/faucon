use std::collections::HashMap;
use std::mem;

use ahash::RandomState;

// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FileId(u32);

impl FileId {
    pub const DUMMY: FileId = FileId(u32::MAX);
}

pub struct Interner {
    map: HashMap<&'static str, FileId, RandomState>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(mut cap: usize) -> Self {
        cap = cap.next_power_of_two();

        Interner {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            self.full.push(mem::replace(
                &mut self.buf,
                String::with_capacity((cap.max(name.len()) + 1).next_power_of_two()),
            ));
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        // SAFETY: `interned` points to a valid string slice which is guaranteed to us as we
        // obtained it from `self.buf`. We would have already panicked OR violated Rust's
        // static safety if this wasn't the case.
        &*(interned as *const str)
    }

    pub fn get(&self, name: &str) -> Option<FileId> {
        self.map.get(name).copied()
    }

    pub fn lookup(&self, id: &FileId) -> &str {
        self.vec[id.0 as usize]
    }

    pub fn intern(&mut self, name: &str) -> FileId {
        self.map.get(name).copied().unwrap_or_else(|| {
            let id = FileId(self.map.len() as u32);
            let name = unsafe { self.alloc(name) };

            self.map.insert(name, id);
            self.vec.push(name);

            debug_assert!(self.lookup(&id) == name);
            debug_assert!(self.intern(name) == id);

            id
        })
    }
}
