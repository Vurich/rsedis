use crate::util::{SetLen, ValueData};
use positioned_io::{ReadAt, Size, WriteAt};
use std::{
    convert::TryInto,
    fmt,
    hash::Hasher,
    io::{self, Read, Seek, Write},
};

#[derive(Debug)]
pub struct Tracking<File> {
    inner: File,
    length: u64,
    initial_length: u64,
    crc32: crc32fast::Hasher,
}

impl<File> Tracking<File> {
    pub fn new(inner: File, initial_length: u64) -> Self {
        Tracking {
            inner,
            length: 0,
            initial_length,
            crc32: Default::default(),
        }
    }

    pub fn length(&self) -> u64 {
        self.length
    }

    pub fn total_length(&self) -> u64 {
        self.length + self.initial_length
    }

    pub fn crc32(&self) -> u64 {
        self.crc32.finish()
    }
}

impl<File> Tracking<File>
where
    File: Size,
{
    pub fn open(inner: File, initial_length: u64, crc32: u32) -> io::Result<Self> {
        let size = inner
            .size()?
            .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidData))?;
        let length = size
            .checked_sub(initial_length)
            .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidData))?;

        Ok(Tracking {
            inner,
            length,
            initial_length,
            crc32: crc32fast::Hasher::new_with_initial(crc32),
        })
    }
}

impl<File> Tracking<File>
where
    File: Seek,
{
    /// Go back in time to a previous state of the database. This logically truncates
    /// the file, but to avoid SIGBUS being thrown if the database file is memmapped
    /// in other processes we don't actually truncate the file here (see `finalize`).
    ///
    /// However, there is a race condition here where if a process is trying to read
    /// some data that gets overwritten it will see invalid data, but the expectation
    /// is that the reader will assert the checksums when it rereads the data from
    /// the database.
    pub fn reset(&mut self, value: ValueData) -> io::Result<()> {
        self.length = value.size as u64;
        self.seek(io::SeekFrom::Start(self.total_length()))?;
        self.crc32 = crc32fast::Hasher::new_with_initial(value.crc32);

        Ok(())
    }
}

impl<File> Tracking<File>
where
    File: Write + SetLen,
{
    /// Flushes changes to disk in a way that does _not_ guarantee consistency with
    /// other connections to the database.
    pub fn finalize(&mut self) -> io::Result<()> {
        self.inner.set_len(self.total_length())?;
        self.flush()?;

        Ok(())
    }
}

impl<File> Seek for Tracking<File>
where
    File: Seek,
{
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        use std::convert::TryFrom;

        match pos {
            io::SeekFrom::End(pos) => self.inner.seek(io::SeekFrom::Start(
                i64::try_from(self.total_length())
                    .ok()
                    .and_then(|len| len.checked_add(pos))
                    .and_then(|pos| pos.try_into().ok())
                    .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidData))?,
            )),
            other => self.inner.seek(other),
        }
    }
}

impl<File> Size for Tracking<File> {
    fn size(&self) -> io::Result<Option<u64>> {
        Ok(Some(self.total_length()))
    }
}

// TODO: This is _really bad_, it completely breaks our CRC32 tracking for the file
//       as a whole. Find a way around this!
impl<File> WriteAt for Tracking<File>
where
    File: WriteAt,
{
    fn write_at(&mut self, pos: u64, buf: &[u8]) -> io::Result<usize> {
        self.inner.write_at(pos, buf)
    }

    fn write_all_at(&mut self, pos: u64, buf: &[u8]) -> io::Result<()> {
        self.inner.write_all_at(pos, buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl<File> ReadAt for Tracking<File>
where
    File: ReadAt,
{
    fn read_at(&self, pos: u64, buf: &mut [u8]) -> io::Result<usize> {
        self.inner.read_at(pos, buf)
    }

    fn read_exact_at(&self, pos: u64, buf: &mut [u8]) -> io::Result<()> {
        self.inner.read_exact_at(pos, buf)
    }
}

impl<File> Read for Tracking<File>
where
    File: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buf)
    }

    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        self.inner.read_vectored(bufs)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.inner.read_to_end(buf)
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        self.inner.read_to_string(buf)
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        self.inner.read_exact(buf)
    }
}

impl<File> Write for Tracking<File>
where
    File: Write,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let out = self.inner.write(buf)?;
        let buf = &buf[..out];
        self.length += buf.len() as u64;
        self.crc32.update(buf);

        Ok(out)
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        let out = self.inner.write_vectored(bufs)?;

        let mut remaining = out;

        for buf in bufs {
            let len = remaining.min(buf.len());

            let buf = &buf[..len];
            self.length += buf.len() as u64;
            self.crc32.update(buf);

            remaining = match remaining.checked_sub(len) {
                Some(remaining) => remaining,
                None => break,
            };
        }

        Ok(out)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.inner.write_all(buf)?;

        self.length += buf.len() as u64;
        self.crc32.update(buf);

        Ok(())
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        self.inner.write_fmt(fmt)
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }
}
