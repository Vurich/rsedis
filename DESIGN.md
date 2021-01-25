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

## Data types

Redis has a few more complex data types that are interesting to us: lists, sets, hashmaps and sorted
sets. We don't really care about hyper-log-logs or bitmaps. My current idea is to have a similar
process to our HAMT history map, i.e. to take methods used to implement functional data structures
and to apply them to file formats, but we're limited by our goal that we ideally want to be able to
round-trip through a stock zip file format parser and lose nothing but metadata and history.

### Representation (in zip format)

So, my idea is to have each of these types be represented as a folder. We can represent each
individual element of these types as a file within that folder - sets and hashmaps are easy, where
for a set the file name can be a hash of the file and for a hashmap he file name is just the key.
Sets can actually have anything for the file name, as long as two of the names don't overlap,
because even if the user edits the file name the zip file format requires the hash of the file to be
included in its header. Sorted sets are a little bit more difficult, since we would ideally have the
file names represent the order to make the re-sorting set required when importing an externally-
generated zip file as short as possible, but this is really just an optimisation and we can
ultimately choose arbitrary names just like sets because if we've lost the metadata after a round
trip we would have to re-sort the data anyway in case it has been changed.

Lists are more difficult, as you can add and remove elements at arbitrary locations, which changes
the order. We have two options: update the file name in the local file header when we emit the
central file directory - which is kinda hacky and means that the shared data between history steps
gets mutated - or use a scheme that continues to represent the order without having to mutate
anything. For the latter, the best scheme I can imagine is for the first element to be called `z`,
then `zz`, then `zzz`, etc. This means that we can add an element between `z` and `zz` by calling it
`zy`, add an element between `zy` and `zz` by calling it `zyz`, and so on. A value before `z` would
be called `y`. Eventually, if we insert enough elements, we'll require a reshuffle of the names. We
can change the `z` to be an earlier letter like `d`, which would allow us to add more elements after
`d` before needing to make the name longer. This isn't a great scheme, and would probably lead to
some pretty confused users, so I think that my favourite option is to simply edit the file names in
the local file header to represent the position that the element is _currently_ in, instead of
having the names be globally immutable. This isn't great, but since we can trivially store the
"true" data structure in our own metadata and since we're not really using the zip file names at
runtime - only when writing the project file and when reading an externally-produced zip file - I
think it's a reasonable compromise. 

### Representation (internal)

Since we've committed to having our own internal metadata which has its own representation of data,
where we care more about zip-compatibility than reusing zip features for our own ends, it's
relatively easy for us to implement these types efficiently.

Maps and sets can just be HAMTs - implemented identically to the history HAMT. Lists can be a
bitmapped vector trie, which is basically just a HAMT where the "hash" is the index of the element
instead of a hash of its key. Ordered sets, if we need them at all, can be a B-tree. These
structures can all be made immutable with very little cloning. The root database HAMT can point to
the root nodes of these trees, which means that changing a single element in these collections does
not require cloning the whole thing, merely a subset thereof. The elements of the collections can be
zip-compatible files - i.e. they can include the local file header. This means the central file
directory can just point directly to an element of the collection.

[hamt]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[rangealloc]: https://crates.io/crates/range-alloc
