# Jigawatt: A Time-Traveling Database

Jigawatt is a key-value store with undo, where its on-disk storage format
is compatible with zip and so can be understood by anything that understands
zip (although undo history isn't available to tools which only understand
zip). It has built-in fault detection and recovery, as well as guarantees
on maximum amount of lost data on crash. However, it has limitations, and
works best on datasets with high read/write traffic but only a relatively
small number of relatively short keys.

Almost all data can be written to the database in an append-only way. The
only exception is when new changes are written after an undo operation.
This removes all ability to redo and truncates the file. However, if you
only undo and redo (without writing new changes further back in history)
then this is still written append-only.

The database can detect faults as it has a fixed-size header at the end
of the file which contains a signature, which is only written after a
checkpoint. If the file does not end with this header then we know that
we crashed during a write, and so we can start attempting to recover
lost data. This can be done by scanning for local file headers and
checking the checksums included in them. If the checksum matches the
data then the data had been fully written, and we can consider it
recovered. Otherwise we discard that value and revert to the value it
had at the previous checkpoint. The intention is that checkpoints are
relatively frequent.

The way it works is essentially that the database file is a recursive
zip file, where it contains a `.undo` file that is a zip file of the
previous state and a `.redo` file which is the zip file of the next
state (if we're currently in the database's history). However, the
naive way of doing this would be incredibly inefficient, so we (ab)use
some things about the zip specification to make this efficient and
useful.

Firstly, files in a zip file can overlap. Secondly, the only files
that are considered valid are those in the central directory, which
comes at the very end of the file. Thirdly, any data between those
files must be completely ignored. Finally, zip files can contain
files that don't have their length and checksum in their header,
but instead whose length and checksum are only stored in the central
directory.

The way it works is that we start each file with two "local file
headers". Zip archives have both a central file header (at the
end of the file) and a local file header, which comes just before
the file's data. The central file header contains a pointer to the
local file header, but the two contain some redundant data. The
first local file header we start the file with is one that states
the filename as `.undo`, and specifies that its length and checksum
are unknown. The second is the same but for `.redo`. To make sure
that both headers have their data start at the same point we mark
the `.undo` header as having an extra data field whose length is
the length of the `.redo` header. Even though once extracted these
substrings become invalid zip files, we still need them to contain
the same data because otherwise the checksums will be incorrect.

Then, since zip files can act as append-only because only the
very last central directory is read, we can add a _central_ file
header which points to the start of the file as the local header,
and specifies the length as being a substring of the current zip file,
such that the end of that substring is a previous central directory.
For redo we can do the same, but instead of pointing to the start of
the file we point to the start of the `.redo` local file header.
Unfortunately, zip files require files whose lengths and checksums are
not known when they are started to end with a small header which
restates the length and checksum (presumably for redundancy and to
keep the data as local as possible to the current read head). This
means that we have to emit this small header whenever we start
writing again after emitting a central directory. We could emit this
header at the same time that we emit the central directory but mark
it as extra data, but I've avoided doing that for now because the
abuse of extra data is already pretty suspect when it comes to the
`.undo` local file header and I'd rather not increase the number of
places that we do that in case it causes the file to be misinterpreted
by some programs.

With this model, going back in time can just be truncating the file, but
in order to preserve redo history until it should be overwritten by new
data we make undos append-only and only truncate just before we rewrite
history.

For more information on how the zip file format works, check out the
[Wikipedia article on Zip](https://en.wikipedia.org/wiki/Zip_%28file_format%29)
and [this diagram of the structure from some university
website](https://users.cs.jmu.edu/buchhofp/forensics/formats/pkzip.html).

