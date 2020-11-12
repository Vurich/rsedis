//! # Jigawatt: A Time-Traveling Database
//!
//! Jigawatt is a key-value store with undo, where its on-disk storage format
//! is compatible with zip and so can be understood by anything that understands
//! zip (although undo history isn't available to tools which only understand
//! zip). It has built-in fault detection and recovery, as well as guarantees
//! on maximum amount of lost data on crash. However, it has limitations, and
//! works best on datasets with high read/write traffic but only a relatively
//! small number of relatively short keys.
//!
//! Almost all data can be written to the database in an append-only way. The
//! only exception is when new changes are written after an undo operation.
//! This removes all ability to redo and truncates the file. However, if you
//! only undo and redo (without writing new changes further back in history)
//! then this is still written append-only.
//!
//! The database can detect faults as it has a fixed-size header at the end
//! of the file which contains a signature, which is only written after a
//! checkpoint. If the file does not end with this header then we know that
//! we crashed during a write, and so we can start attempting to recover
//! lost data. This can be done by scanning for local file headers and
//! checking the checksums included in them. If the checksum matches the
//! data then the data had been fully written, and we can consider it
//! recovered. Otherwise we discard that value and revert to the value it
//! had at the previous checkpoint. The intention is that checkpoints are
//! relatively frequent.
//!
//! The way it works is essentially that the database file is a recursive
//! zip file, where it contains a `.undo` file that is a zip file of the
//! previous state and a `.redo` file which is the zip file of the next
//! state (if we're currently in the database's history). However, the
//! naive way of doing this would be incredibly inefficient, so we (ab)use
//! some things about the zip specification to make this efficient and
//! useful.
//!
//! Firstly, files in a zip file can overlap. Secondly, the only files
//! that are considered valid are those in the central directory, which
//! comes at the very end of the file. Thirdly, any data between those
//! files must be completely ignored. Finally, zip files can contain
//! files that don't have their length and checksum in their header,
//! but instead whose length and checksum are only stored in the central
//! directory.
//!
//! The way it works is that we start each file with two "local file
//! headers". Zip archives have both a central file header (at the
//! end of the file) and a local file header, which comes just before
//! the file's data. The central file header contains a pointer to the
//! local file header, but the two contain some redundant data. The
//! first local file header we start the file with is one that states
//! the filename as `.undo`, and specifies that its length and checksum
//! are unknown. The second is the same but for `.redo`. To make sure
//! that both headers have their data start at the same point we mark
//! the `.undo` header as having an extra data field whose length is
//! the length of the `.redo` header. Even though once extracted these
//! substrings become invalid zip files, we still need them to contain
//! the same data because otherwise the checksums will be incorrect.
//!
//! Then, since zip files can act as append-only because only the
//! very last central directory is read, we can add a _central_ file
//! header which points to the start of the file as the local header,
//! and specifies the length as being a substring of the current zip file,
//! such that the end of that substring is a previous central directory.
//! For redo we can do the same, but instead of pointing to the start of
//! the file we point to the start of the `.redo` local file header.
//! Unfortunately, zip files require files whose lengths and checksums are
//! not known when they are started to end with a small header which
//! restates the length and checksum (presumably for redundancy and to
//! keep the data as local as possible to the current read head). This
//! means that we have to emit this small header whenever we start
//! writing again after emitting a central directory. We could emit this
//! header at the same time that we emit the central directory but mark
//! it as extra data, but I've avoided doing that for now because the
//! abuse of extra data is already pretty suspect when it comes to the
//! `.undo` local file header and I'd rather not increase the number of
//! places that we do that in case it causes the file to be misinterpreted
//! by some programs.
//!
//! With this model, going back in time can just be truncating the file, but
//! in order to preserve redo history until it should be overwritten by new
//! data we make undos append-only and only truncate just before we rewrite
//! history.
//!
//! For more information on how the zip file format works, check out the
//! [Wikipedia article on Zip](https://en.wikipedia.org/wiki/Zip_%28file_format%29)
//! and [this diagram of the structure from some university
//! website](https://users.cs.jmu.edu/buchhofp/forensics/formats/pkzip.html).

#![cfg_attr(feature = "nightly", feature(external_doc, test))]
#![cfg_attr(feature = "nightly", doc(include = "../README.md"))]
#![deny(private_in_public)]
#![warn(missing_docs)]

mod tracking_file;
mod util;

use tracking_file::Tracking;
pub use util::SetLen;
use util::ValueData;

use beef::lean::Cow;
use positioned_io::{ReadAt, Size, WriteAt};
use std::{
    borrow::Borrow,
    collections::HashMap,
    convert::TryInto,
    hash::Hash,
    io::{self, Read, Seek, Write},
    mem,
};

type OrSavedState<T> = Result<T, Option<T>>;

/// Central database connection type. This is entirely single-threaded, although because
/// a lot of the state uses immutable data structures anyway it would probably be
/// relatively easy to write a thread-safe version of this if necessary.
///
/// Two connections cannot write to the same database, although many readers can exist
/// as long as a single writer exists without any locking.
///
/// > WARNING: Currently, if the database returns an error then it will probably be left
/// >          in an undefined state, which will _probably_ work (since to my knowledge
/// >          we never leave it in a completely incorrect state) but may have very
/// >          unexpected behavior. This will need to be fixed in the future, but should
/// >          be kept in mind for now.
// TODO: We could do compaction by essentially storing two backing files, and then when
//       we reach `history_size` states we create a new backing file containing only
//       the current state, then when the new one reaches `history_size` states we
//       clear the old one and swap the two. This would mean that we have a maximum
//       memory usage of `history_size * 3`, but it also means that we never have
//       less than `history_size` undo steps available.
// TODO: Use a memory map instead of calling out to the system constantly, especially
//       for reads.
#[derive(Debug)]
pub struct Db<File = std::fs::File> {
    backing_file: Tracking<File>,
    pending: OrSavedState<HashMap<Cow<'static, str>, ValueData, fxhash::FxBuildHasher>>,
    state: Option<Vec<(Cow<'static, str>, ValueData)>>,
    cur: Option<ValueData>,
    redo: Option<ValueData>,
    options: DbOptions,
}

/// Options when opening the database.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct DbOptions {
    /// The maximum possible size of the history.
    pub history_size: u16,
}

impl Default for DbOptions {
    fn default() -> Self {
        Self { history_size: 100 }
    }
}

#[derive(Default)]
struct LocalFileOptions {
    size: u32,
    crc32: u32,
}

const UNDO_NAME: &str = ".undo";
const REDO_NAME: &str = ".redo";

const INITIAL_HEADER_SIZE: u32 =
    local_header_size(UNDO_NAME) as u32 + local_header_size(REDO_NAME) as u32;

impl<File> Db<File> {}

impl<File> Db<File>
where
    File: Read + ReadAt + Write + WriteAt + Seek,
{
    /// Create a new database file, and write the initial data necessary.
    pub fn new(mut file: File, options: DbOptions) -> io::Result<Self> {
        write_local_header(
            &mut file,
            None,
            UNDO_NAME,
            None,
            local_header_size(REDO_NAME) as u16,
        )?;
        write_local_header(&mut file, None, REDO_NAME, None, 0)?;

        let mut out = Self {
            backing_file: Tracking::new(file, INITIAL_HEADER_SIZE as u64),
            pending: Ok(Default::default()),
            state: Some(Default::default()),
            cur: None,
            redo: None,
            options,
        };
        out.checkpoint()?;
        Ok(out)
    }

    /// Create a checkpoint that you can roll back to using `undo`, or syncronise the
    /// current history to disk (i.e. if you've done an `undo` operation this will
    /// write that you have done so to disk, without clearing the available redo steps).
    pub fn checkpoint(&mut self) -> io::Result<()> {
        if self.state.is_none() {
            self.read_state()?;
        }

        let mut values = self.state.take().expect(
            "We should have been able to read the state, and \
            if not then we should have errored out beforehand",
        );

        if let Some(value) = self.cur.take() {
            let value = ValueData {
                header_size: INITIAL_HEADER_SIZE,
                offset: INITIAL_HEADER_SIZE,
                ..value
            };

            match values.binary_search_by_key(&UNDO_NAME, |(k, _)| &k[..]) {
                Ok(i) => values[i].1 = value,
                Err(i) => values.insert(i, (Cow::borrowed(UNDO_NAME), value)),
            }
        }

        match (
            values.binary_search_by_key(&REDO_NAME, |(k, _)| &k[..]),
            self.redo,
        ) {
            (Ok(i), Some(redo)) => values[i].1 = redo,
            (Ok(i), None) => {
                values.remove(i);
            }
            (Err(i), Some(redo)) => values.insert(i, (Cow::borrowed(REDO_NAME), redo)),
            (Err(_), None) => {}
        }

        for (k, v) in mem::replace(&mut self.pending, Err(None)).unwrap_or_default() {
            match values.binary_search_by_key(&&k[..], |(k, _)| &k[..]) {
                Ok(i) => values[i].1 = v,
                Err(i) => values.insert(i, (k, v)),
            }
        }

        let cd_start = self.backing_file.total_length();

        for (k, v) in &values {
            write_central_file_header(
                &mut self.backing_file,
                k,
                v.offset - v.header_size,
                LocalFileOptions {
                    size: v.size,
                    crc32: v.crc32,
                },
                k == UNDO_NAME || k == REDO_NAME,
            )?;
        }

        let cd_size = self.backing_file.total_length() - cd_start;

        write_eocd(
            &mut self.backing_file,
            values.len() as _,
            cd_start as _,
            cd_size as _,
        )?;

        self.cur = Some(ValueData {
            header_size: INITIAL_HEADER_SIZE as _,
            size: self.backing_file.length() as _,
            offset: INITIAL_HEADER_SIZE as _,
            crc32: self.backing_file.crc32() as _,
        });
        self.state = Some(values);

        io::Write::flush(&mut self.backing_file)?;

        Ok(())
    }

    /// Insert a new value into the database. This will write the value to the backing file immediately,
    /// although if you insert the same key many times it will simply overwrite the value and will
    /// not continue appending. This data is considered ephemeral until `.checkpoint` is called and
    /// it is not guaranteed to be consistent, although it is recoverable in the case of a crash.
    pub fn insert(&mut self, key: impl Into<Cow<'static, str>>, value: &[u8]) -> io::Result<()> {
        use std::collections::hash_map::Entry;

        let mut pending =
            mem::replace(&mut self.pending, Ok(Default::default())).unwrap_or_default();

        if self.redo.is_some() {
            self.redo = None;
            self.backing_file.reset(self.cur.unwrap())?;
        }

        match pending.entry(key.into()) {
            Entry::Occupied(mut entry) => {
                let &ValueData { offset, size, .. } = entry.get();

                let value = if value.len() <= size as usize {
                    let out = write_value(
                        &mut self.backing_file,
                        Some(offset.into()),
                        entry.key(),
                        value,
                    )?;
                    out
                } else {
                    write_value(&mut self.backing_file, None, entry.key(), value)?
                };

                *entry.get_mut() = value;
            }
            Entry::Vacant(vacant) => {
                let value = write_value(&mut self.backing_file, None, vacant.key(), value)?;
                vacant.insert(value);
            }
        }

        self.pending = Ok(pending);

        Ok(())
    }
}

impl<File> Db<File>
where
    File: Write + SetLen,
{
    /// Sync all changes to disk, leaving the database file as a valid .zip file. This is
    /// an extremely cheap operation, as it just flushes all writes and then truncates the
    /// file, but it's very important that it is done when no other connections to the
    /// database exist because this could cause the reader to receive SIGBUS when accessing
    /// data in the truncated portion.
    pub fn finalize(&mut self) -> io::Result<()> {
        self.backing_file.finalize()
    }
}

/// A type that allows efficiently reading a value from the file without an extra copy
/// (other than the copy from kernel-space to userspace).
// TODO: Implement optional checksum confirmation
pub struct ValueReader<'a, File> {
    file: &'a File,
    value: ValueData,
}

impl<'a, File> ValueReader<'a, File> {
    fn new(file: &'a File, value: ValueData) -> Self {
        Self { file, value }
    }

    fn advance(&mut self, by: u32) {
        self.value.offset += by;
        self.value.size -= by;
    }
}

impl<File> ValueReader<'_, File>
where
    File: ReadAt,
{
    /// Convenience method to convert this directly into a heap-allocated vector
    pub fn into_bytes(self) -> io::Result<Vec<u8>> {
        let mut vec = vec![0; self.value.size as usize];
        self.file
            .read_exact_at(self.value.offset as u64, &mut vec)?;

        Ok(vec)
    }
}

impl<File> Read for ValueReader<'_, File>
where
    File: ReadAt,
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let buf_len = buf.len();
        let out = self.file.read_at(
            self.value.offset as u64,
            &mut buf[..(self.value.size as usize).min(buf_len)],
        )?;
        self.advance(out as u32);
        Ok(out)
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        if buf.len() > self.value.size as usize {
            return Err(io::Error::from(io::ErrorKind::UnexpectedEof));
        }

        self.file.read_exact_at(self.value.offset as u64, buf)?;
        self.advance(buf.len() as u32);

        Ok(())
    }
}

impl<File> Db<File>
where
    File: Read + Seek + Size,
{
    /// Open an existing database file for editing. As much of this should be done
    /// lazily as possible.
    pub fn open(file: File, options: DbOptions) -> io::Result<Self> {
        // TODO: How do we store the current CRC32? We can use the data descriptor, maybe.
        let backing_file = Tracking::open(file, INITIAL_HEADER_SIZE as u64, 0)?;

        let cur = ValueData {
            header_size: INITIAL_HEADER_SIZE as _,
            size: backing_file.length() as _,
            offset: INITIAL_HEADER_SIZE as _,
            crc32: backing_file.crc32() as _,
        };

        Ok(Self {
            backing_file,
            pending: Err(None),
            state: None,
            cur: Some(cur),
            redo: None,
            options,
        })
    }
}

/// The return value for undo and redo operations.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum HistoryState {
    /// The undo/redo was a no-op, because we reached the end of history. Calling `undo`/`redo`
    /// again will never start returning `CanContinue` unless the database is modified.
    ReachedEndOfHistory,
    /// The undo/redo changed the state of the database, and there may be more states available
    /// if you continue to call undo/redo.
    CanContinue,
}

impl<File> Db<File>
where
    File: Read + ReadAt + Seek,
{
    /// Step backwards in history. This does nothing except change where `get` reads the data
    /// from. If you want these changes to be persisted to disk, call `checkpoint`.
    pub fn undo(&mut self) -> io::Result<HistoryState> {
        match mem::replace(&mut self.pending, Err(None)) {
            Ok(pending) => {
                self.pending = Err(Some(pending));
                Ok(HistoryState::CanContinue)
            }
            Err(pending) => {
                self.pending = Err(pending);

                let range = self.get_range(UNDO_NAME)?;

                if let Some(range) = range {
                    self.state = None;

                    self.redo = self.cur.take().map(|cur| ValueData {
                        header_size: local_header_size(REDO_NAME) as u32,
                        ..cur
                    });
                    self.cur = Some(range);

                    Ok(HistoryState::CanContinue)
                } else {
                    Ok(HistoryState::ReachedEndOfHistory)
                }
            }
        }
    }

    /// Step forwards in history. This does nothing except change where `get` reads the data
    /// from. If you want these changes to be persisted to disk, call `checkpoint`.
    pub fn redo(&mut self) -> io::Result<HistoryState> {
        if let Some(range) = self.redo {
            self.backing_file.reset(range)?;
            self.state = None;
            self.cur = Some(range);

            Ok(HistoryState::CanContinue)
        } else {
            let range = self.get_range(REDO_NAME)?;

            if let Some(range) = range {
                self.backing_file.reset(range)?;
                self.state = None;
                self.cur = Some(range);

                Ok(HistoryState::CanContinue)
            } else {
                match mem::replace(&mut self.pending, Err(None)) {
                    Ok(pending) => {
                        self.pending = Ok(pending);
                        Ok(HistoryState::ReachedEndOfHistory)
                    }
                    Err(Some(pending)) => {
                        self.pending = Ok(pending);
                        Ok(HistoryState::CanContinue)
                    }
                    Err(None) => Ok(HistoryState::ReachedEndOfHistory),
                }
            }
        }
    }
}

impl<File> Db<File>
where
    File: Read + ReadAt + Seek,
{
    /// The number of items in the database. This doesn't currently take into account pending items
    pub fn len(&self) -> io::Result<usize> {
        use byteorder::ReadBytesExt;

        if let Some(state) = &self.state {
            Ok(state.len())
        } else if let Some(cur) = &self.cur {
            let mut buf = [0u8; mem::size_of::<u16>()];

            self.backing_file.read_exact_at(
                cur.offset as u64 + cur.size as u64 - EOCD_SIZE as u64
                    + mem::size_of::<u32>() as u64,
                &mut buf,
            )?;

            Ok((&mut &buf[..]).read_u16::<byteorder::LittleEndian>()? as usize)
        } else {
            Ok(0)
        }
    }

    /// Whether the database is empty. This doesn't currently take into account pending items.
    pub fn is_empty(&self) -> io::Result<bool> {
        Ok(self.len()? == 0)
    }

    fn state(&mut self) -> io::Result<&[(Cow<'static, str>, ValueData)]> {
        if self.state.is_none() {
            self.read_state()?;
        }

        Ok(self.state.as_ref().map(|v| &v[..]).unwrap_or(&[]))
    }

    fn read_state(&mut self) -> io::Result<()> {
        let cur = self.cur.ok_or(io::ErrorKind::InvalidData)?;

        let eocd = read_eocd(
            &mut self.backing_file,
            Some(cur.offset as u64 + cur.size as u64 - EOCD_SIZE as u64),
        )?;

        let mut buffer = Vec::with_capacity(eocd.cd_count as usize);

        let pos = self.backing_file.seek(io::SeekFrom::Current(0))?;

        self.backing_file
            .seek(io::SeekFrom::Start(eocd.cd_start as u64))?;

        for _ in 0..eocd.cd_count {
            let central_file_header = read_central_header(&mut self.backing_file)?;

            let name = match &central_file_header.name[..] {
                UNDO_NAME => Cow::borrowed(UNDO_NAME),
                REDO_NAME => Cow::borrowed(REDO_NAME),
                _ => Cow::owned(central_file_header.name),
            };

            buffer.push((name, central_file_header.value));
        }

        self.state = Some(buffer);

        self.backing_file.seek(io::SeekFrom::Start(pos))?;

        Ok(())
    }
}

impl<File> Db<File>
where
    File: Read + ReadAt + Seek,
{
    fn get_range(&mut self, key: impl Borrow<str>) -> io::Result<Option<ValueData>> {
        let key = key.borrow();

        if let Some(val) = self
            .pending
            .as_ref()
            .ok()
            .and_then(|pending| pending.get(key))
        {
            Ok(Some(*val))
        } else {
            let state = self.state()?;

            if let Ok(index) = state.binary_search_by_key(&key, |(k, _)| &k[..]) {
                Ok(state.get(index).map(|&(_, v)| v))
            } else {
                Ok(None)
            }
        }
    }

    /// Read a value from the database. Right now this calls out to the system in order
    /// to read the data, but in the future it's possible that we could use a memory
    /// map to make this operation no more expensive in the worst case than the two
    /// required hashmap lookups.
    pub fn get(
        &mut self,
        key: impl Borrow<str>,
    ) -> io::Result<Option<ValueReader<'_, impl ReadAt>>> {
        if let Some(range) = self.get_range(key)? {
            Ok(Some(ValueReader::new(&self.backing_file, range)))
        } else {
            Ok(None)
        }
    }
}

fn quick_crc32(buf: &[u8]) -> u32 {
    let mut hasher = crc32fast::Hasher::default();
    hasher.update(buf);

    hasher.finalize() as u32
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CentralFileHeader {
    name: String,
    value: ValueData,
}

fn read_central_header<File>(mut file: File) -> io::Result<CentralFileHeader>
where
    File: Read,
{
    use byteorder::ReadBytesExt;

    let mut bytes = [0; CENTRAL_FILE_HEADER_SIZE];
    file.read_exact(&mut bytes)?;

    let mut bytes = io::Cursor::new(&bytes[..]);

    let header = bytes.read_u32::<byteorder::LittleEndian>()?;

    if header != CENTRAL_FILE_HEADER_SIGNATURE {
        return Err(io::Error::from(io::ErrorKind::InvalidData));
    }

    bytes.seek(io::SeekFrom::Start(16))?;
    let crc32 = bytes.read_u32::<byteorder::LittleEndian>()?;
    let size = bytes.read_u32::<byteorder::LittleEndian>()?;
    let _uncompressed_size = bytes.read_u32::<byteorder::LittleEndian>()?;
    let filename_length = bytes.read_u16::<byteorder::LittleEndian>()?;
    bytes.seek(io::SeekFrom::Start(42))?;
    let offset = bytes.read_u32::<byteorder::LittleEndian>()?;

    let mut out = vec![0; filename_length as usize];

    file.read_exact(&mut out)?;

    let out = std::str::from_utf8(&out)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
        .to_string();

    // HACK
    let header_size = if out == UNDO_NAME {
        INITIAL_HEADER_SIZE
    } else {
        local_header_size(&out) as u32
    };

    Ok(CentralFileHeader {
        name: out,
        value: ValueData {
            size,
            header_size,
            offset: offset + header_size,
            crc32,
        },
    })
}

#[derive(Debug, Copy, Clone, Hash)]
struct Eocd {
    cd_count: u16,
    cd_start: u32,
    #[allow(dead_code)]
    cd_size: u32,
}

fn read_eocd<File>(mut file: File, pos: Option<u64>) -> io::Result<Eocd>
where
    File: Read + ReadAt,
{
    use byteorder::ReadBytesExt;

    let mut bytes = [0; EOCD_SIZE];
    if let Some(pos) = pos {
        file.read_exact_at(pos, &mut bytes)?;
    } else {
        file.read_exact(&mut bytes)?;
    }

    let mut bytes = io::Cursor::new(&bytes);

    let header = bytes.read_u32::<byteorder::LittleEndian>()?;

    if header != EOCD_HEADER {
        return Err(io::Error::from(io::ErrorKind::InvalidData));
    }

    bytes.seek(io::SeekFrom::Start(10))?;

    Ok(Eocd {
        cd_count: bytes.read_u16::<byteorder::LittleEndian>()?,
        cd_size: bytes.read_u32::<byteorder::LittleEndian>()?,
        cd_start: bytes.read_u32::<byteorder::LittleEndian>()?,
    })
}

fn write_value<File>(
    mut file: File,
    pos: Option<u64>,
    name: &str,
    data: &[u8],
) -> io::Result<ValueData>
where
    File: Write + WriteAt + Size,
{
    let crc32 = quick_crc32(&data[..]);

    write_local_header(
        file.by_ref(),
        pos.map(|pos| pos - LOCAL_HEADER_SIZE as u64),
        &name,
        Some(LocalFileOptions {
            size: data.len() as _,
            crc32,
        }),
        0,
    )?;

    let size = data.len() as u32;
    let offset = if let Some(pos) = pos {
        file.write_all_at(pos, &data)?;
        pos
    } else {
        let offset = file
            .size()?
            .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidInput))?;
        file.write_all(&data)?;
        offset
    }
    .try_into()
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    Ok(ValueData {
        offset,
        header_size: local_header_size(name) as _,
        size,
        crc32,
    })
}

const LOCAL_HEADER_SIZE: usize = 30;
const LOCAL_HEADER_SIGNATURE: u32 = 0x04034b50;

const fn local_header_size(name: &str) -> usize {
    name.len() + LOCAL_HEADER_SIZE
}

fn write_local_header<File: Write + WriteAt>(
    mut file: File,
    pos: Option<u64>,
    name: &str,
    options: Option<LocalFileOptions>,
    extra_len: u16,
) -> io::Result<()> {
    use byteorder::WriteBytesExt as _;

    let mut buf = [0; LOCAL_HEADER_SIZE];
    let mut writer = io::Cursor::new(&mut buf[..]);

    // Local file header signature
    writer.write_u32::<byteorder::LittleEndian>(LOCAL_HEADER_SIGNATURE)?;
    // Version needed to extract
    writer.write_all(&[0x1, 0x0])?;

    // General-purpose flags
    if options.is_some() {
        writer.write_u16::<byteorder::LittleEndian>(0b00000000_00000000)?;
    } else {
        // Size not known when local header is emitted
        writer.write_u16::<byteorder::LittleEndian>(0b00000000_00001000)?;
    }

    // Compression method (store)
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // File last modification time
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // File last modification date
    writer.write_u16::<byteorder::LittleEndian>(0)?;

    let LocalFileOptions { size, crc32 } = options.unwrap_or_default();

    // CRC32 hash
    writer.write_u32::<byteorder::LittleEndian>(crc32)?;
    // Compressed size
    writer.write_u32::<byteorder::LittleEndian>(size)?;
    // Uncompressed size (same as compressed because we use `store` for now)
    writer.write_u32::<byteorder::LittleEndian>(size)?;

    // File name length
    writer.write_u16::<byteorder::LittleEndian>(
        name.len()
            .try_into()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
    )?;
    // Extra field length
    writer.write_u16::<byteorder::LittleEndian>(extra_len)?;

    if let Some(pos) = pos {
        file.write_all_at(pos, &buf)?;
        file.write_all_at(pos + LOCAL_HEADER_SIZE as u64, name.as_bytes())?;
    } else {
        file.write_all(&buf)?;
        file.write_all(name.as_bytes())?;
    }

    Ok(())
}

const CENTRAL_FILE_HEADER_SIZE: usize = 46;
const CENTRAL_FILE_HEADER_SIGNATURE: u32 = 0x02014b50;

fn write_central_file_header<File: Write>(
    mut file: File,
    name: &str,
    offset: u32,
    LocalFileOptions { size, crc32 }: LocalFileOptions,
    dynamic_size: bool,
) -> io::Result<()> {
    use byteorder::WriteBytesExt;

    let mut buf = [0; CENTRAL_FILE_HEADER_SIZE];
    let mut writer = io::Cursor::new(&mut buf[..]);

    // Central file header signature
    writer.write_u32::<byteorder::LittleEndian>(CENTRAL_FILE_HEADER_SIGNATURE)?;
    // Version made by
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // Version needed to extract (minimum)
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // General purpose bit flag
    if dynamic_size {
        // Size not known when local header is emitted
        writer.write_u16::<byteorder::LittleEndian>(0b00000000_00001000)?;
    } else {
        writer.write_u16::<byteorder::LittleEndian>(0b00000000_00000000)?;
    }

    // Compression method (store)
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // File last modification time
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // File last modification date
    writer.write_u16::<byteorder::LittleEndian>(0)?;

    // CRC32 hash
    writer.write_u32::<byteorder::LittleEndian>(crc32)?;
    // Compressed size
    writer.write_u32::<byteorder::LittleEndian>(size)?;
    // Uncompressed size (same as compressed because we use `store` for now)
    writer.write_u32::<byteorder::LittleEndian>(size)?;

    // File name length
    writer.write_u16::<byteorder::LittleEndian>(
        name.len()
            .try_into()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
    )?;
    // Extra field length
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // File comment length
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // Disk number where file starts
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // Internal file attributes
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // External file attributes
    writer.write_u32::<byteorder::LittleEndian>(0)?;
    // Relative offset of local file header
    writer.write_u32::<byteorder::LittleEndian>(offset)?;

    file.write_all(&buf)?;
    file.write_all(name.as_bytes())?;

    Ok(())
}

const EOCD_SIZE: usize = 22;
const EOCD_HEADER: u32 = 0x06054b50;

fn write_eocd<File: Write>(
    mut file: File,
    cd_count: u16,
    cd_start: u32,
    cd_size: u32,
) -> io::Result<()> {
    use byteorder::WriteBytesExt;

    let mut buf = [0; EOCD_SIZE];
    let mut writer = io::Cursor::new(&mut buf[..]);

    // EOCD signature
    writer.write_u32::<byteorder::LittleEndian>(EOCD_HEADER)?;
    // Current disk
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // Central directory start disk
    writer.write_u16::<byteorder::LittleEndian>(0)?;
    // Central directory records on current disk
    writer.write_u16::<byteorder::LittleEndian>(cd_count)?;
    // Total directory records
    writer.write_u16::<byteorder::LittleEndian>(cd_count)?;
    // Central directory size
    writer.write_u32::<byteorder::LittleEndian>(cd_size)?;
    // Central directory offset
    writer.write_u32::<byteorder::LittleEndian>(cd_start)?;
    // Comment length
    writer.write_u16::<byteorder::LittleEndian>(0)?;

    file.write_all(&buf)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Db, DbOptions, HistoryState};
    use std::fs::OpenOptions;

    #[test]
    fn it_works() {
        const FOO_VAL_1: &[u8] = b"Test";
        const FOO_VAL_2: &[u8] = b"Something else";
        const BAR_VAL_1: &[u8] = b"One";
        const BAR_VAL_2: &[u8] = b"Not one";
        const BAZ_VAL: &[u8] = b"Two";
        const QUX_VAL: &[u8] = b"Three";

        let filename = format!(
            "/tmp/test-{}.zip",
            std::env::current_exe()
                .unwrap()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
        );

        {
            let file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .read(true)
                .open(&filename)
                .unwrap();

            let mut db = Db::new(file, DbOptions { history_size: 100 }).unwrap();

            db.insert("foo", FOO_VAL_1).unwrap();
            db.checkpoint().unwrap();

            db.insert("foo", FOO_VAL_2).unwrap();
            db.insert("bar", BAR_VAL_1).unwrap();
            db.insert("baz", BAZ_VAL).unwrap();
            db.insert("qux", QUX_VAL).unwrap();

            db.checkpoint().unwrap();

            db.insert("bar", BAR_VAL_2).unwrap();

            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.redo().unwrap(), HistoryState::CanContinue);

            let combined = String::from_utf8(db.get("foo").unwrap().unwrap().into_bytes().unwrap())
                .unwrap()
                + std::str::from_utf8(&db.get("bar").unwrap().unwrap().into_bytes().unwrap())
                    .unwrap();

            db.insert("foobar", b"hello world!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                .unwrap();
            db.insert("foobar", combined.as_bytes()).unwrap();

            assert_eq!(db.redo().unwrap(), HistoryState::ReachedEndOfHistory);

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_2)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAR_VAL_1)
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(QUX_VAL)
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAZ_VAL)
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(combined.as_bytes())
            );

            db.checkpoint().unwrap();

            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_1)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );

            db.checkpoint().unwrap();

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_1)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );

            db.finalize().unwrap();
        }

        {
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(filename)
                .unwrap();

            let mut db = Db::open(file, DbOptions { history_size: 100 }).unwrap();

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_1)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );

            db.redo().unwrap();

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_2)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAR_VAL_1)
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(QUX_VAL)
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAZ_VAL)
            );

            db.checkpoint().unwrap();

            db.finalize().unwrap();
        }
    }
}

#[cfg(all(test, feature = "nightly"))]
mod benches {
    use super::{Db, DbOptions, HistoryState};
    use std::fs::OpenOptions;

    extern crate test;

    const FOO_VAL_1: &[u8] = b"Test";
    const FOO_VAL_2: &[u8] = b"Something else";
    const BAR_VAL_1: &[u8] = b"One";
    const BAR_VAL_2: &[u8] = b"Not one";
    const BAZ_VAL: &[u8] = b"Two";
    const QUX_VAL: &[u8] = b"Three";

    #[bench]
    fn insert(b: &mut test::Bencher) {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .read(true)
            .open(format!(
                "/tmp/test-{}.zip",
                std::env::current_exe()
                    .unwrap()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
            ))
            .unwrap();

        let mut db = Db::new(file, DbOptions { history_size: 100 }).unwrap();

        b.iter(|| {
            db.insert("foo", FOO_VAL_1).unwrap();
            db.insert("bar", BAR_VAL_1).unwrap();
            db.insert("baz", BAZ_VAL).unwrap();
            db.insert("qux", QUX_VAL).unwrap();
            db.checkpoint().unwrap();

            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            db.checkpoint().unwrap();
        });
    }

    #[bench]
    fn get(b: &mut test::Bencher) {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .read(true)
            .open(format!(
                "/tmp/test-{}.zip",
                std::env::current_exe()
                    .unwrap()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
            ))
            .unwrap();

        let mut db = Db::new(file, DbOptions { history_size: 100 }).unwrap();

        db.insert("foo", FOO_VAL_1).unwrap();
        db.insert("bar", BAR_VAL_1).unwrap();
        db.insert("baz", BAZ_VAL).unwrap();
        db.insert("qux", QUX_VAL).unwrap();
        db.checkpoint().unwrap();

        b.iter(|| {
            test::black_box(db.get("bar").unwrap().unwrap().into_bytes().unwrap());
        });
    }

    #[bench]
    fn integration(b: &mut test::Bencher) {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .read(true)
            .open(format!(
                "/tmp/test-{}.zip",
                std::env::current_exe()
                    .unwrap()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
            ))
            .unwrap();

        let mut db = Db::new(file, DbOptions { history_size: 100 }).unwrap();

        b.iter(|| {
            db.insert("foo", FOO_VAL_1).unwrap();
            db.checkpoint().unwrap();

            db.insert("foo", FOO_VAL_2).unwrap();
            db.insert("bar", BAR_VAL_1).unwrap();
            db.insert("baz", BAZ_VAL).unwrap();
            db.insert("qux", QUX_VAL).unwrap();

            db.checkpoint().unwrap();

            db.insert("bar", BAR_VAL_2).unwrap();

            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.redo().unwrap(), HistoryState::CanContinue);

            let combined = String::from_utf8(db.get("foo").unwrap().unwrap().into_bytes().unwrap())
                .unwrap()
                + std::str::from_utf8(&db.get("bar").unwrap().unwrap().into_bytes().unwrap())
                    .unwrap();

            db.insert("foobar", b"hello world!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                .unwrap();
            db.insert("foobar", combined.as_bytes()).unwrap();

            assert_eq!(db.redo().unwrap(), HistoryState::ReachedEndOfHistory);

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_2)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAR_VAL_1)
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(QUX_VAL)
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(BAZ_VAL)
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(combined.as_bytes())
            );

            db.checkpoint().unwrap();

            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);
            assert_eq!(db.undo().unwrap(), HistoryState::CanContinue);

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_1)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );

            db.checkpoint().unwrap();

            assert_eq!(
                db.get("foo")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                Some(FOO_VAL_1)
            );
            assert_eq!(
                db.get("bar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("qux")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("baz")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
            assert_eq!(
                db.get("foobar")
                    .unwrap()
                    .map(|r| r.into_bytes().unwrap())
                    .as_ref()
                    .map(|v| &v[..]),
                None
            );
        });
    }
}
