Synopsis
--------

_infgen_ is a deflate stream disassembler. It will read a gzip, zlib, or raw
deflate stream, and output a readable description of the contents.

Motivation
----------

_infgen_ permits the examination of deflate compressed data for instructional
purposes, to see how the data is compressed, and for debugging deflate
compressors.

Installation
------------

Simply compile `infgen.c`, and provide the compressed data to stdin. The
disassembled output will be written to stdout.

Test
----

    gzip < infgen.c | ./infgen

will display the disassembled result of compressing the _infgen_ source code.

Use:

    infgen -h

to see the command options.

Documentation
-------------

A list of all of the command options and detailed technical documentation can
be found in the comments at the start of [infgen.c](infgen.c)

License
-------

This code is under the zlib license, found in the source file and LICENSE.
