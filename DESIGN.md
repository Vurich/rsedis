# Jigawatt new design proposal

The existing design of the storage format is quite elegant as it doesn't extend the zip format in
any way, it merely abuses certain properties of it. I propose a new format which _does_ extend zip,
while keeping compatibility (both in terms of project files being readable by external tools, and in
terms of project files created by external tools being readable by Jigawatt).

The file will start with a header, which will look like so:

```
magic number b"JGWT"             (u32)
crc32 of the file                (u32)
length of allocation information (u64)
offset to allocation information (u64)
current history depth            (u64)
length of history information    (u64)
history buffer start pointer     (u64)
offset to history information    (u64)
```

Any zip file which doesn't include this header (or that includes it but with an invalid CRC32) will
be loaded but without history, and with any area not taken up by a file in the central directory
being considered free space. Originally I planned to have the metadata be handled by magic files,
but despite the fact that this reuses standard zip features we actually do _not_ want this metadata
to be preserved if the file was edited by hand, as there is no way to ensure that the metadata is
still accurate and users will not be surprised by their undo history being lost after manually
editing the project files by hand.

Allocation information is just a list of ranges, with all the complicated work being handled by
[`range-alloc`][rangealloc]. The only complex part of this is that ideally the allocation metadata
would itself be allocated, but this can be handled by the fact that the maximum size of the
allocation metadata after an allocation is at most one more than the size before an allocation. We
can simply preallocate space for one extra element.

The history information takes the form of a circular buffer of [HAMT][hamt]s, with each node of the
HAMT being just a file entry. This is represented by the index of the 0th element in the buffer,
which is incremented when the oldest element is removed from the buffer, and the current history
depth, which is used for undo/redo. Undo is simply stepping back one element (increasing the depth
by one) and redo is simply stepping forward by one element (decreasing the depth by one).

Each local file header acts as a node in the HAMT, by having an "extra field" with the ID `b"JG"`
(`0x4a47`). This extra field looks like so:

```
struct HistoryFile {
    refcount: u16, // TODO: Could have a different size
    children: [FileOffset; 32],
}
```

I won't get into the details of how a HAMT works here, but essentially we descend the trie by
consuming the hash of the key in chunks of 5 bits (5 being `log2(32)`), using that 5 bit section as
an index into the children array, until we either find the exact key we're looking for or we find an
empty space. To edit a node, if the refcount is 1 we can edit it in-place and if it is greater than
1 we clone the node, edit it, and then step back to the parent. If the refcount of the parent is 1
we edit the relevant pointer in its children array to be a pointer to the new node, if the refcount
is greater than 1 we clone the parent, edit it, and then recursively step back up the trie repeating
this process.

With this scheme, instead of each history step being a strict superset (in terms of serialised
bytes) of the last, we have a single file with a single central directory - so that other tools can
easily access our data - while still efficiently serialising the concept of history and allowing
space in the file to be reused as much as possible to prevent the size of the file from constantly
expanding.

[hamt]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[rangealloc]: https://crates.io/crates/range-alloc
