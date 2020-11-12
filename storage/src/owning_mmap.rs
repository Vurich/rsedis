use std::io;

pub struct OwningMmap<File> {
    file: File,
    map: io::Cursor<MmapMut>,
    current_length: usize,
}

impl<T> Deref for OwningMmap<T> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &*self.map.get_ref()[..current_length]
    }
}

impl<T> DerefMut for OwningMmap<T> {
    fn deref_mut(&self) -> &mut Self::Target {
        &mut *self.map.get_mut()[..current_length]
    }
}

impl<File> io::Write for OwningMmap<File>
where
    File: io::Write,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.map
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
