use std::{
    fs,
    io::{self, Write},
};

/// Trait for readers/writers whose length can be set. This is intended for
/// file handles and wrappers around file handles.
pub trait SetLen {
    /// Set the length of this file, truncating data if the length is less than
    /// the current and zero-filling if it is greater.
    fn set_len(&self, new_len: u64) -> io::Result<()>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ValueData {
    /// zip files limit themselves to 32-bit offsets, so we will too
    pub offset: u32,
    pub header_size: u32,
    pub size: u32,
    pub crc32: u32,
}

impl SetLen for fs::File {
    fn set_len(&self, new_len: u64) -> io::Result<()> {
        fs::File::set_len(self, new_len)
    }
}

impl<T> SetLen for io::BufReader<T>
where
    T: SetLen,
{
    fn set_len(&self, new_len: u64) -> io::Result<()> {
        self.get_ref().set_len(new_len)
    }
}

impl<T> SetLen for io::BufWriter<T>
where
    T: SetLen + Write,
{
    fn set_len(&self, new_len: u64) -> io::Result<()> {
        self.get_ref().set_len(new_len)
    }
}
