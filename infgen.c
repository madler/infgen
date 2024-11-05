/*
  infgen version 3.3, 20 June 2024

  Copyright (C) 2005-2024 Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Mark Adler    madler@alumni.caltech.edu
 */

/*
 Read a zlib, gzip, png, or raw deflate stream and write a defgen-compatible or
 simple binary encoded stream representing that input to stdout. This is based
 on the puff.c code to decompress deflate streams. Note that neither the zlib
 nor the gzip trailer is checked against the uncompressed data (in fact the
 uncompressed data is never generated) -- only the fact that the trailer is
 present is checked.

 Usage: infgen [-d[d]] [-q[q]] [-i] [-s] [-r] [-b[b]] < foo.gz > foo.def
    or: infgen [-d[d]] [-q[q]] [-i] [-s] [-r] [-b[b]] foo.gz > foo.def

 where foo.gz is a gzip file (it could have been a zlib, png, or raw deflate
 stream as well), and foo.def is a defgen description of the file or stream,
 which is in a readable text format (unless -b is used). For png files, the
 output is a description of only the zlib stream extracted from the IDAT
 blocks.

 The description includes the literal/length and distance code lengths for
 dynamic blocks. The -d (dynamic) option generates directives to exactly
 reconstruct the dynamic block headers. With -d, the code lengths are still
 included, but now as comments instead of directives. The -dd option is the
 same as -d, but with the bit sequences for each item shown as a comment after
 the item. The -s (statistics) option writes out comments with statistics for
 each deflate block and totals at the end.

 The -q (quiet) option supresses the dynamic block code lengths, whether as
 directives or as comments. The -qq (really quiet) option supresses the output
 of all deflate stream descriptions, leaving only the header and trailer
 information. However if -qq is used with -s, the statistics information on the
 deflate stream is still included.

 The -i (info) option generates additional directives for gzip or zlib headers
 that permit their exact reconstruction. For png files, -i will show chunk
 types and lengths as comments, but not the contents, other than IDAT chunks.

 The -r (raw) option forces the interpretation of the input as a raw deflate
 stream, for those cases where the start of a raw stream happens to mimic one
 of the other headers. The -b (binary) option writes a compact binary format
 instead of the defgen format. In that case, all other options except -r are
 ignored. The -bb option includes compressed-data bit counts in the output.

 Both the defgen and compact binary formats are described below.
 */

/*

 defgen format:

    Content:

    The defgen format consists of lines of comments and directives. Each line
    is terminated by a single new line character '\n', though it may be written
    as "\r\n" on systems with end-of-line conversions. defgen accepts either.
    The directives are used to construct (or reconstruct) a deflate stream.
    Each directive is a word at the start of the line, possibly followed by
    parameters.

    Comments:

    defgen lines whose first character is an exclamation mark ('!') are
    comments, and are ignored by defgen. Blank lines are also ignored. All
    other lines are directives. If an exclamation mark appears after a
    directive and not following a single quote, then it and the characters
    after it are a comment and are ignored. infgen-generated informational
    comments are described below.

    Headers and trailers:

    The "gzip" directive writes a gzip header. It is optionally preceded by
    directives bearing information to be contained in the gzip header: "name",
    "comment", "extra", "text', "time", "xfl", "os", and "hcrc". The "name",
    "comment", and "extra" directives use the same parameter format as "data"
    and "literal" described below, and may be repeated over multiple lines for
    long content. For "name" and "comment", the parameters do not include the
    terminating zero. For example:

    name 'linux-3.1.6.tar

    The "time", "os", and "xfl" (extra flags) directives each have a single
    numeric parameter. "os" and "xfl" have a parameter in the range of 0..255,
    and "time" is in the the range of 0..2^32-1. infgen adds a comment after
    the time parameter with the local time zone interpretation of that value.
    If "os" is not present, it is taken to be 3 (for Unix). If "xfl" or "time"
    is not present, the value is taken to be zero. "text" and "hcrc" have no
    parameter. "text" sets the text flag. hcrc signals a two-byte crc of the
    header.

    The "crc" directive writes the CRC-32 of the uncompressed data in
    little-endian order. The "length" directive writes the length of the
    uncompressed data, modulo 2^32, in little-endian order. The combination of
    a crc and a length in that order is the gzip trailer. Either or both can
    optionally have a numeric parameter in the range 0..2^32-1 which would be
    used in place of the value derived from the data. infgen does not write
    those parameters.

    The "zlib" directive writes a zlib header. The zlib directive has an
    optional numeric parameter which is the log-base-2 of the window size, in
    the range 8..15. If there is no parameter, 15 is assumed. zlib may be
    preceded by the "level" directive, which has one parameter: the compression
    level used when compressing in the range 0..3. If level is not present, it
    is taken to be 2. zlib may also be preceded by a "dict" directive with the
    dictionary id as the numeric parameter, in the range 0..2^32-1.

    The "adler" directive writes the adler checksum of the uncompressed data in
    big-endian order. This is the zlib trailer. adler may optionally have a
    numeric parameter in the range 0..2^32-1 that is used in place of the
    actual adler checksum of the data.

    Deflate blocks:

    Deflate data between zlib or gzip headers and trailers, or raw deflate
    data, consists of a series of deflate blocks. They are begun by the block
    type directives: "stored", "fixed", or "dynamic", and all end with "end"
    after the contents of the block. The last block has the directive "last" on
    its own line before the block type directive. The "stored" directive has an
    optional parameter which is the data that fills in the dummy bits to get to
    a byte boundary. If the parameter is not present, those bits are assumed to
    be zero. An additional "block3" block type indicates the illegal bit
    pattern for a fourth block type.

    Block headers:

    Fixed blocks have no header, and proceed immediately to the data after the
    fixed directive.

    A stored block header has as many bits as needed to go to the next byte
    boundary (see the "stored" parameter above), followed by four bytes of
    block length information. There is no directive for the length of the
    stored block, as it is implied by the amount of data up to the next end
    directive.

    A dynamic block has a header that describes the Huffman codes used to
    represent the literal/length and distance codes in the block. That
    description is itself compressed with a third code. The dynamic header is
    represented in one of two ways, or not at all. If there is no description
    of the header, then the block data can be used to construct an optimal set
    of Huffman codes for the contained symbols, and an optimum way to encode
    them in the header. In that case, the data immediately follows the dynamic
    directive.

    The first explicit way to describe the header is to list the number of bits
    in each literal/length and distance code. This is done with the "litlen"
    and "dist" directives. Each directive has two numerical parameters: the
    symbol index and the number of bits. E.g. "litlen 40 9" or "dist 16 5". In
    this case, dynamic is followed by all of the litlen directives, which is
    followed by all the dist directives. The litlen symbol must be in the range
    0..285, and dist symbol must be in the range 0..29. The number of bits for
    both must be in the range 1..15. Only the symbols coded are listed. The
    header description is complete upon encountering the first "literal",
    "match", or "end".

    The second, more explicit way to describe the header is to list the actual
    contents of the header, from which the code lengths are derived. This is
    done with the directives "count", "code", "lens", "repeat", and "zeros".
    count has two parameters: the number of length code lengths, (257..286) and
    the number of distance code lengths (1..30). code has two numerical
    parameters, the symbol index (0..18) and the number of bits for that symbol
    (1..7). lens has any number of parameters in 0..15, where each is the
    length of the corresponding literal/length or distance code. A zero length
    means that that symbol has no code and does not appear in the block. repeat
    and zeros each have one parameter which is the number of times to repeat a
    bit length. repeat repeats the most recent length 3..6 times. zeros repeats
    zeros 3..138 times. dynamic is followed by all of the code directives, and
    then by the len directives, with repeat and zeros directives mixed in. The
    header description is complete upon encountering the first "literal",
    "match", or "end".

    Data:

    All compressed data is represented using the directives: "data", "literal",
    and "match". "data" and "literal" have the same parameters and both
    directly represent bytes of data. "data" may be used only in stored blocks
    and literal may be used only in fixed or dynamic blocks. The parameters of
    data and literal are a series of decimal numbers separated by spaces,
    followed by a string of printable characters. Each decimal number is in the
    range 0..255, and represents one byte of data. The string is a single
    quote, followed by any number of characters in the range 32..126. A single
    quote may appear within the string meaning a single quote in the data -- it
    does not end the string. The string is ended by the end of line or any
    other character not in the range 32..126. To append a comment to a line
    with a string, a tab ('\t') can end the string, which may then be followed
    by blank space and an exclamation mark for the comment. Either the numbers
    or the string are optional.

    match has two numerical parameters. The first is the length of the match,
    in 3..258. The second is the distance back, in 1..32768.

    The data and the current block ends with the "end" directive.

    The "end" of a block that was started with "last" marks the end of the
    deflate stream. If that last block does not end at a bit boundary, the
    "bound" directive has a single numeric parameter with the fill bits, where
    those bits would be shifted up to fill in the last byte. If bound is not
    present, the bits up to the byte boundary are filled with zeros. infgen
    outputs the bound directive only when the fill bits are not all zeros.

    infgen comments:

    infgen starts with a comment line indicating the version of infgen that
    generated the defgen format output. E.g. "! infgen 3.1 output".

    infgen inserts an empty comment, a line with just an exclamation mark,
    before each header, deflate block, and trailers.

    If the -d option is used, then the litlen and dist directives are written
    as comments. E.g. "! litlen 40 9". If the -dd option is used, then each
    deflate stream element, other than stored bytes, is appended to each
    directive as a comment with a series of bit sequences shown as 0's and 1's.
    In this case literals are always one per line. The bits in each sequence
    are shown from most significant to least significant, as they appeared in
    the compressed data. For directives with multiple components, e.g. Huffman
    codes and extra bits, each component is shown as one bit sequence with the
    components separated by spaces. The sequences are shown in reverse order.
    In that way, if the spaces are removed, the bits are in the order they
    are pulled from the compressed data, reading right to left. So in:

        match 18 680            ! 10100111 1011 1 1101011

    1101011 is the Huffman code and 1 is the extra bit for length 18. Then 1011
    is the Huffman code and 10100111 are the extra bits for distance 680. If
    these bits happened to start at a byte boundary, then the first byte would
    be 11101011 or 0xeb, then second byte would be 01111011 or 0x7b. The third
    byte would have the low nybble 1010, or 0xa.

    With the -s option, infgen will generate statistics comments, all of which
    begin with "! stats ". There are statistics for each deflate block, and
    summary statistics after the last deflate block. The statistics comments
    are as follows:

    "! stats table n:m" gives the total number of bytes and bits in the dynamic
    block header, not including the three block identifier bits. For example,
    "! stats table 58:6" indicating 58 bytes and 6 bits = 470 bits.

    "! stats literals x.x bits each (n/m)" follows a fixed or dynamic block and
    gives the average number of bits per literal, the total number of bits for
    the literals in the block, and the number of literals in the block. For
    example, "! stats literals 5.7 bits each (3793/664)". If the block has no
    literals, then "! stats literals none" will be written.

    "! stats matches x.x% (n x x.x)" follows a fixed or dynamic block and gives
    the percentage of the uncompressed bytes in the block that came from
    matches, the number of matches in the block, and the average match length.
    For example, "! stats matches 82.6% (183 x 17.2)". If the block has no
    matches, then "! stats matches none" will be written.

    "! stats stored length n" follows each stored block and gives the number of
    uncompressed bytes in the stored block, which does not include the stored
    header. For example: "! stats stored length 838" is a stored block with
    838 bytes.

    "! stats inout n:m (i) j k" follows any block and gives the total number of
    bytes and bits in the block, including the three-bit block identifier, the
    total number of symbols in the block (a literal and a match each count as
    one symbol), the number of uncompressed bytes generated by the block, and
    the maximum reach of the distances to data before the block. For example,
    "! stats inout 1889:4 (1906) 3810 -1718" is a block with 1889 bytes and 4
    bits, 1906 symbols, 3810 uncompressed bytes, and maximum reach of 1718
    bytes before the block by a match in the block. If the block does not reach
    before itself, the reach value is zero.

    After the last deflate block, total statistics are output. They all begin
    with "! stats total ". The block input and output amounts are summed for
    example as: "! stats total inout 93232233:0 (55120762) 454563840", with the
    same format as "! stats inout", except without the reach.

    "! stats total block average 34162.3 uncompressed" states for example that
    the average number of uncompressed bytes per block was 34162.3. Similarly
    "! stats total block average 4142.5 symbols" states that there were 4142.5
    symbols on average per block. "! stats total literals 6.9 bits each" states
    that there were 6.9 bits used on average per literal. Lastly the matches
    are summed: "! stats total matches 95.2% (33314520 x 13.0)" with the same
    format as "! stats matches".

 */

/*
    Compact binary format (-b) deflate content description (gzip and zlib
    headers and trailers are ignored):

    0..0x7f:     high byte of distance-1, followed by low byte of
                 distance-1, followed by length-3 (three bytes total)
    0x80..0xfe:  literals 0..0x7e
    0xff:        prefix byte, followed by ...
     0, 1:       stored block (1 = last), followed by leftover bits (one byte)
     2, 3:       fixed block (3 = last)
     4, 5:       dynamic block (5 = last), then header terminated by a 0 byte
     6, 7:       invalid block (7 = last)
     8:          end of deflate stream, followed by leftover bits (one byte)
     9..0x7e:    reserved (not used)
     0x7f..0xff: literals 0x7f..0xff

    dynamic block header:

    The binary dynamic block header description is terminated by a zero, and
    does not contain any zeros before that, in order to simplify decoding when
    the header is not of interest. The raw header is described, in order to
    permit exact reconstruction if desired. The header is this sequence of
    bytes:

    nlen - 256  number of length codes minus 256 (1..30, meaning 257..286)
    ndist       number of distance codes (1..30)
    ncode       number of code length codes (4..19)
    ncode *     ncode bytes follow:
        len+1   code length plus one (1..8, meaning 0..7)
    opcodes *   enough opcodes follow to desribe nlen + ndist codes
        opcode  each byte is 1..16 for lengths 0..15, or 17..20 to repeat the
                    the last length 3..6 times, or 21..156 to repeat zeros
                    3..138 times
    0           a zero byte terminates the header description

    Literals are coded on average to 1.5 bytes, though often less since low
    literals are more common. Length-distance pairs are coded as three bytes.
    The coded form will be approximately 20% to 40% larger than the compressed
    form.

    --- Extensions to -b format when the -bb option is given ---

    The leftover bits after a stored block header or the end of the stream have
    a 1 bit above them so that the number of leftover bits can be determined.
    For example 0x80 means seven 0 bits, and 0x01 means no leftover bits.

    A variable-length unsigned integer is represented in little-endian order
    with seven bits in each byte and the high bit set, except for the last byte
    which has the high bit clear. The last byte cannot be zero unless the value
    being represented is 0. If the value is 1 or more, then there are no zero
    bytes in the representation. The bit counts below are written as variable-
    length unsigned integers with values assured to be greater than zero.

    0xff:        prefix byte, followed by ...
     9:          total number of bits in the preceding block - 9
                 number of bits in the header - 2
                 number of bits in the literal codes + 1 (0 + 1 for stored)
                 number of bits in the match codes + 1 (0 + 1 for stored)
                 a terminating 0 byte
     10..0x3f:   reserved (not used) -- assume these are followed by a zero-
                 terminated sequence of bytes, like 4, 5, and 9 above (this
                 permits compatible future use)
     0x40..0x7e: reserved (not used) -- assume these are followed by nothing
 */

/* Version history:
   1.0  20 Jan 2005  First version
   1.1  27 Feb 2005  Clean up for distribution
   1.2  27 Feb 2005  Remove comments for non-existent return code
                     Check for distances too far back and issue warning
   1.3  23 Jul 2006  Provide option to turn off dynamic trees
                     Add option for statistics comments in output
                     Process concatenated streams
                     Show the gzip file name if present
                     Replace cryptic error codes with descriptive messages
                     Correct error messages for incomplete deflate stream
   1.4  21 Mar 2007  Add -d option for showing the raw dynamic block header
                       information as it comes in, as comments (for checking
                       initial gzip/deflate fragments for sensibility)
                     Allow multiple options after the initial dash
   1.5   9 Jan 2008  Treat no symbol for end-of-block as an error
                     Fix error in use of error message table (inferr[])
   1.6  12 Apr 2008  Add stored block length comment for -s option
   1.7  25 Jul 2008  Add some diagnostic information to distance too far back
                     Synchronize stdout and stderr for error messages
   1.8   5 Dec 2008  Fix output header to match version
                     Add -r (raw) option to ignore faux zlib/gzip headers
                     Check distance too far back vs. zlib header window size
   1.9   9 Jun 2009  Add hack to avoid MSDOS end-of-line conversions
                     Avoid VC compiler warning
   2.0   4 Nov 2011  Change fprintf to fputs to avoid warning
                     Add block statistics on literals
                     Allow bad zlib header method to proceed as possible raw
                     Fix incorrect lenlen and distlen comments with -d
   2.1  13 Jan 2013  Use uintmax_t for counts instead of unsigned long
                     Fix bug: show block end stats only when -s specified
                     Make the inout comment format a tad more readable
                     Fix stored length stat comment to start with stats
                     Add -q (quiet) option to not output literals and matches
                     Add maximum reach before current block to stats inout
                     Add -i (info) and extra, name, and comment gzip directives
                     Check for ungetc() failure (only one guaranteed)
                     Normally put out only litlen and dist for dynamic header
                     Put out codes, lenlen, distlen, and repeat for -d
                     For -d, still write litlen and dist, but as comments
                     Delete extraneous code comments for -d
                     Have repeat directive use -1 to indicate copy last length
                     Remove extraneous symbol index from lenlen and distlen
                     Replace repeat directive with repeat and zeros directives
                     Add window size to zlib directive, if not 15
                     Add level and dict directives for zlib headers
                     Add extensive comments on the infgen output format
   2.2  10 Feb 2013  Don't show gzip header extra directive if -i not given
                     Don't show zlib header info directives if -i not given
                     Note hcrc directive in format description
                     Add "text" directive for that bit in gzip header flags
                     Add "count" directive for dynamic headers
                     Change "lenlen" and "distlen" directives to just "lens"
                     Check for invalid code length codes in dynamic blocks
                     Change "static" to "fixed" to be consistent with RFC 1951
                     Add a compact binary output format (-b)
                     Support an input path on the command line
                     Detect when input is from tty, show help in that case
                     Change options -n to -q, and -q to -qq
                     Build struct state in main()
                     Add local time description as a comment in time directive
   2.3  18 Jul 2015  Distinguish incomplete from oversubscribed codes
                     Use symbols for error codes
                     Move all if statement actions to next line
                     Show version in help
   2.4   2 Jan 2017  Fix erroneous declaration of i/o error on devices
   2.5  24 Jul 2021  Set window size from zlib header
                     Add -dd option to show the bit sequences for each item
   2.6  22 Aug 2021  Fix bug in binary (-b) output for repeats and zeros
   2.7   7 Jan 2022  Fix bit ordering in comments with the -dd option
   2.8   9 Jan 2022  Fix bug for gzip header extra field when -i not given
                     Add annotations in comments for gzip extra sub-fields
                     Discriminate non-binary comment with brackets
   3.0  10 Aug 2022  Update to zlib license
   3.1  19 Jul 2023  Detect and extract the zlib data from PNG files
   3.2  26 Jul 2023  Check PNG chunk CRCs
   3.3  20 Jun 2024  Add -bb option to include bit counts in binary output
 */

#define IG_VERSION "3.3"

#include <stdio.h>          // putc(), getc(), ungetc(), fputs(), fflush(),
                            // fopen(), fclose(), fprintf(), vfprintf(),
                            // fread(), stdout, stderr, FILE, EOF
#include <stdlib.h>         // exit()
#include <string.h>         // strerror(), memcmp()
#include <errno.h>          // errno
#include <time.h>           // time_t, gmtime(), asctime()
#include <stdarg.h>         // va_list, va_start(), va_end()
#include <inttypes.h>       // intmax_t, PRIuMAX
#include <setjmp.h>         // jmp_buf, setjmp(), longjmp()
#include <unistd.h>         // isatty()
#include "zlib.h"           // crc32(), get_crc_table()

#if defined(MSDOS) || defined(OS2) || defined(_WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) _setmode(_fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define local static

/*
 * infgen() return codes:
 *
 *   1:  available deflate data did not terminate
 *   0:  successful inflate
 *  -1:  invalid block type (type == 3)
 *  -2:  stored block length did not match one's complement
 *  -3:  dynamic block code description: too many length or distance codes
 *  -4:  dynamic block code description: code lengths codes oversubscribed
 *  -5:  dynamic block code description: code lengths codes incomplete
 *  -6:  dynamic block code description: repeat lengths with no first length
 *  -7:  dynamic block code description: repeat more than specified lengths
 *  -8:  dynamic block code description: literal/length code oversubscribed
 *  -9:  dynamic block code description: literal/length code incomplete
 * -10:  dynamic block code description: distance code oversubscribed
 * -11:  dynamic block code description: distance code incomplete
 * -12:  dynamic block code description: missing end-of-block code
 * -13:  invalid literal/length or distance code in fixed or dynamic block
 */

// infgen() return code symbols.
#define IG_INCOMPLETE 1
#define IG_OK 0
#define IG_BLOCK_TYPE_ERR -1
#define IG_STORED_LENGTH_ERR -2
#define IG_TOO_MANY_CODES_ERR -3
#define IG_CODE_LENGTHS_CODE_OVER_ERR -4
#define IG_CODE_LENGTHS_CODE_UNDER_ERR -5
#define IG_REPEAT_NO_FIRST_ERR -6
#define IG_REPEAT_TOO_MANY_ERR -7
#define IG_LITLEN_CODE_OVER_ERR -8
#define IG_LITLEN_CODE_UNDER_ERR -9
#define IG_DIST_CODE_OVER_ERR -10
#define IG_DIST_CODE_UNDER_ERR -11
#define IG_NO_END_CODE_ERR -12
#define IG_BAD_CODE_ERR -13

// infgen() negative return code messages.
local const char *inferr[] = {
    /*  -1 */ "invalid block type (3)",
    /*  -2 */ "stored block length complement mismatch",
    /*  -3 */ "too many length or distance codes",
    /*  -4 */ "code lengths code is oversubscribed",
    /*  -5 */ "code lengths code is incomplete",
    /*  -6 */ "length repeat with no first length",
    /*  -7 */ "repeat more lengths than available",
    /*  -8 */ "literal/length code is oversubscribed",
    /*  -9 */ "literal/length code is incomplete",
    /* -10 */ "distance code is oversubscribed",
    /* -11 */ "distance code is incomplete",
    /* -12 */ "missing end-of-block code",
    /* -13 */ "invalid code"
};
#define IG_ERRS (sizeof(inferr)/sizeof(char *))

// Print an error message and exit. Return a value to use in an expression,
// even though the function will never return.
local inline int bail(char *fmt, ...) {
    fflush(stdout);
    fputs("infgen error: ", stderr);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    putc('\n', stderr);
    exit(1);
    return 0;
}

// Print a warning to stderr.
local inline void warn(char *fmt, ...) {
    fflush(stdout);
    fputs("infgen warning: ", stderr);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    putc('\n', stderr);
}

// Maximums for allocations and loops. It is not useful to change these --
// they are fixed by the deflate format.
#define MAXBITS 15              // maximum bits in a code
#define MAXLCODES 286           // maximum number of literal/length codes
#define MAXDCODES 30            // maximum number of distance codes
#define MAXCODES (MAXLCODES+MAXDCODES)  // maximum codes lengths to read
#define FIXLCODES 288           // number of fixed literal/length codes
#define MAXDIST 32768           // maximum match distance
#define MAXSEQS 20              // maximum number of bit groups to save

// infgen() input and output state.
struct state {
    // Output state.
    int binary;                 // true to write compact binary format
    int info;                   // true to write informational comments
    int data;                   // true to output literals and matches
    int tree;                   // true to output dynamic tree
    int draw;                   // true to output dynamic descriptor
    int stats;                  // true to output statistics
    int col;                    // state within data line
    unsigned max;               // maximum distance (bytes so far)
    unsigned win;               // window size from zlib header or 32K
    FILE *out;                  // output file

    // Input state.
    int bitcnt;                 // number of bits in bit buffer
    int bitbuf;                 // bit buffer
    long chunk;                 // bytes left in this png chunk, or -1
    uint32_t crc;               // running ~CRC of current chunk data
    z_crc_t const *table;       // CRC table for one-byte updates
    FILE *in;                   // input file
    int seqs;                   // number of bit sequences saved
    short seq[MAXSEQS];         // bits in each sequence
    char len[MAXSEQS];          // length of each sequence in bits

    // Current block statistics.
    unsigned reach;             // maximum distance before current block
    unsigned headlen;           // bits in block header
    uintmax_t blockin;          // bits in for current block
    uintmax_t blockout;         // bytes out for current block
    uintmax_t symbols;          // number of symbols (or stored bytes)
    uintmax_t matches;          // number of matches
    uintmax_t matchlen;         // total length of matches
    uintmax_t litbits;          // number of bits in literals
    uintmax_t matbits;          // number of bits in matches

    // Total statistics.
    uintmax_t blocks;           // total number of deflate blocks
    uintmax_t inbits;           // total deflate bits in
    uintmax_t outbytes;         // total uncompressed bytes out
    uintmax_t symbnum;          // total number of symbols
    uintmax_t matchnum;         // total number of matches
    uintmax_t matchtot;         // total length of matches
    uintmax_t littot;           // total bits in literals

    // Input limit error return state for bits() and decode().
    jmp_buf env;
};

#define LINELEN 79      // target line length for data and literal commands
#define SEQCOL 24       // column in which to start bit sequence comments

// Go to column SEQCOL using tabs.
local void seqtab(struct state *s) {
    s->col = abs(s->col);
    putc('\t', s->out);         // at least one tab to end literal string
    s->col = (s->col & ~7) + 8;
    while (s->col + 8 <= SEQCOL) {
        putc('\t', s->out);
        s->col += 8;
    }
    while (s->col < SEQCOL) {
        putc(' ', s->out);
        s->col++;
    }
}

// Write the bits that composed the last item, as a comment starting in column
// SEQCOL. This assumes that tab stops are at multiples of eight.
local inline void putbits(struct state *s) {
    if (s->draw > 1) {
        // Start a comment at column SEQCOL.
        seqtab(s);
        putc('!', s->out);

        // Write the sequences in reverse order, since they were read from
        // bottom up. In each sequence, write the most to least significant
        // bit, i.e. the usual order.
        while (s->seqs) {
            s->seqs--;
            short seq = s->seq[s->seqs];
            int len = s->len[s->seqs];
            if (len) {
                putc(' ', s->out);
                do {
                    fputc('0' + ((seq >> --len) & 1), s->out);
                } while (len);
            }
        }
    }

    // End the comment and the line.
    putc('\n', s->out);
    s->col = 0;
}

// Write token at the start of a line and val as a character or decimal value,
// continuing the line. Keep the line length reasonable and using string
// literals whenever possible. If seq is true and s->draw > 1, also display the
// sequences of bits that led to this value.
local inline void putval(int val, char *token, int seq, struct state *s) {
    seq = seq && s->draw > 1;

    // New line if too long or decimal after string.
    if (s->col == 0 || abs(s->col) > LINELEN - 4 ||
        (s->col < 0 && (val < 0x20 || val > 0x7e)) || seq) {
        if (s->col)
            putc('\n', s->out);
        s->col = fprintf(s->out, "%s", token);
    }

    // String literal (already range-checked above).
    if (s->col < 0) {
        putc(val, s->out);
        s->col--;
    }

    // New string literal (mark with negative lit).
    else if (val >= 0x20 && val <= 0x7e) {
        s->col += fprintf(s->out, " '%c", val);
        s->col = -s->col;
    }

    // Decimal literal.
    else
        s->col += fprintf(s->out, " %u", val);

    // Append a comment with the sequences of bits, if requested.
    if (seq)
        putbits(s);
}

// Return the first byte of the next IDAT chunk, or EOF if there are no more
// non-empty IDAT chunks. Set s->chunk to the number of remaining bytes in the
// chunk.
local int idat(struct state *s) {
    for (;;) {
        unsigned char head[13];     // preceding CRC + next length and type
        head[12] = 0;
        size_t got = fread(head, 1, 12, s->in);
        if (got >= 4) {
            // check CRC
            uint32_t crc = head[3] + ((uint32_t)head[2] << 8) +
                           ((uint32_t)head[1] << 16) +
                           ((uint32_t)head[0] << 24);
            if (crc != ~s->crc)
                warn("corrupt PNG");
        }
        if (got < 12) {
            if (got != 4)
                warn("invalid PNG structure");
            s->chunk = 0;
            return EOF;
        }

        // Get the chunk length.
        s->chunk = head[7] + ((long)head[6] << 8) + ((long)head[5] << 16) +
                   ((long)head[4] << 24);

        if (s->info) {
            // Show the chunk information.
            if (s->col) {
                putc('\n', s->out);
                s->col = 0;
            }
            fprintf(s->out, "! PNG %s (%ld)\n", head + 8, s->chunk);
        }

        // Initialize the CRC with the chunk type.
        s->crc = ~crc32(crc32(0, Z_NULL, 0), head + 8, 4);

        if (s->chunk == 0)
            // Even if this is an IDAT, an empty one is useless. Get the next
            // chunk.
            continue;

        if (memcmp(head + 8, "IDAT", 4) == 0) {
            // Found an IDAT chunk -- return the first byte.
            s->chunk--;
            return getc(s->in);
        }

        // Skip over the non-IDAT chunk data, updating the CRC. The chunk CRC
        // will remain to be read.
        unsigned char junk[8192];       // read buffer for a non-seekable skip
        do {
            long get = s->chunk > (long)sizeof(junk) ? sizeof(junk) : s->chunk;
            long got = fread(junk, 1, get, s->in);
            s->crc = ~crc32(~s->crc, junk, got);
            s->chunk -= got;
            if (got != get) {
                warn("invalid PNG structure");
                return EOF;
            }
        } while (s->chunk);
    }
}

// Return the next byte of the deflate data, or EOF on end of input. For png
// files, this will read deflate data from each IDAT chunk until it is
// exhausted, and then will look for the next IDAT chunk. The chunk CRC is
// updated.
local inline int get(struct state *s) {
    if (s->chunk == -1)
        return getc(s->in);
    int ch = s->chunk-- ? getc(s->in) : idat(s);
    if (ch == EOF)
        return ch;
    s->crc = (s->crc >> 8) ^ s->table[(s->crc ^ ch) & 0xff];
    return ch;
}

// Return need bits from the input stream. This always leaves less than
// eight bits in the buffer. bits() works properly for need == 0.
//
// Format notes:
//
// - Bits are stored in bytes from the least significant bit to the most
//   significant bit. Therefore bits are dropped from the bottom of the bit
//   buffer, using shift right, and new bytes are appended to the top of the
//   bit buffer, using shift left.
local inline int bits(struct state *s, int need) {
    // Load at least need bits into val.
    long val = s->bitbuf;
    while (s->bitcnt < need) {
        int next = get(s);
        if (next == EOF)
            longjmp(s->env, 1);                 // out of input
        val |= (long)(next) << s->bitcnt;       // load eight bits
        s->bitcnt += 8;
    }

    // Drop need bits and update buffer, always with 0..7 bits left. Leave need
    // bits in val.
    s->bitbuf = (int)(val >> need);
    s->bitcnt -= need;
    s->blockin += need;
    val &= (1L << need) - 1;

    // Save bit sequence.
    if (s->draw > 1 && s->seqs < MAXSEQS) {
        s->seq[s->seqs] = val;
        s->len[s->seqs] = need;
        s->seqs++;
    }

    // Return need bits.
    return (int)val;
}

// Show and accumulate statistics at end of block.
local void end(struct state *s) {
    if (s->stats)
        fprintf(s->out, "! stats inout %" PRIuMAX ":%" PRIuMAX
                        " (%" PRIuMAX ") %" PRIuMAX " %s%u\n",
                s->blockin >> 3, s->blockin & 7, s->symbols, s->blockout,
                s->reach ? "-" : "", s->reach);
    s->blocks++;
    s->inbits += s->blockin;
    s->outbytes += s->blockout;
    s->symbnum += s->symbols;
}

// Process a stored block.
local int stored(struct state *s) {
    // Discard leftover bits from current byte (assumes s->bitcnt < 8).
    (void)bits(s, s->bitcnt);
    if (s->draw > 1) {
        s->col = 0;
        putbits(s);
    }

    // Get length and check against its one's complement.
    unsigned len = bits(s, 16);
    unsigned cmp = bits(s, 16);
    if (len != (~cmp & 0xffff))
        return IG_STORED_LENGTH_ERR;            // didn't match complement!
    if (s->stats) {
        if (s->col) {
            putc('\n', s->out);
            s->col = 0;
        }
        fprintf(s->out, "! stats stored length %u\n", len);
    }

    // Update max distance.
    if (s->max < s->win) {
        if (len > s->win - s->max)
            s->max = s->win;
        else
            s->max += len;
    }

    // Copy len bytes from in to out.
    s->headlen = s->blockin;
    while (len--) {
        int octet = get(s);
        s->blockin += 8;
        if (octet == EOF)
            return IG_INCOMPLETE;               // not enough input
        if (s->binary) {
            if (octet < 0x7f)
                putc(octet + 0x80, s->out);
            else {
                putc(0xff, s->out);
                putc(octet, s->out);
            }
        }
        if (s->data)
            putval(octet, "data", 0, s);
        s->blockout++;
        s->symbols++;
    }

    // Done with a valid stored block.
    if (s->data) {
        if (s->col) {
            putc('\n', s->out);
            s->col = 0;
        }
        fputs("end\n", s->out);
    }
    if (s->stats)
        end(s);
    return IG_OK;
}

// Huffman code decoding tables. count[1..MAXBITS] is the number of symbols of
// each length, which for a canonical code are stepped through in order.
// symbol[] are the symbol values in canonical order, where the number of
// entries is the sum of the counts in count[]. The decoding process can be
// seen in the function decode() below.
struct huffman {
    short *count;       // number of symbols of each length
    short *symbol;      // canonically ordered symbols
};

// Decode a code from the stream s using huffman table h. Return the symbol or
// a negative value if there is an error. If all of the lengths are zero, i.e.
// an empty code, or if the code is incomplete and an invalid code is received,
// then IG_BAD_CODE_ERR is returned after reading MAXBITS bits.
local inline int decode(struct state *s, struct huffman *h) {
    int bitbuf = s->bitbuf;     // bits to decode from the input
    int left = s->bitcnt;       // number of bits in bitbuf
    int len = 1;                // length of code in consideration
    int code = 0;               // len bits pulled from bitbuf
    int first = 0;              // first code of length len
    int index = 0;              // index of that code in the symbol table
    short *next = h->count + 1; // pointer to number of codes of next length
    for (;;) {
        while (left--) {
            code |= bitbuf & 1;
            bitbuf >>= 1;
            int count = *next++;
            if (code < first + count) {
                // This code is length len. Save bit sequence.
                if (s->draw > 1 && s->seqs < MAXSEQS) {
                    // Reverse the code for showing in the comment.
                    int rev = 0;
                    for (int i = 0; i < len; i++)
                        rev = (rev << 1) | ((code >> i) & 1);
                    s->seq[s->seqs] = rev;
                    s->len[s->seqs] = len;
                    s->seqs++;
                }

                // Update state.
                s->bitbuf = bitbuf;
                s->bitcnt = (s->bitcnt - len) & 7;
                s->blockin += len;

                // Return symbol.
                return h->symbol[index + (code - first)];
            }

            // Update to find a code of the next length.
            index += count;
            first += count;
            first <<= 1;
            code <<= 1;
            len++;
        }

        // Need to load more bits from the input into bitbuf.
        left = (MAXBITS+1) - len;
        if (left == 0)
            break;
        bitbuf = get(s);
        if (bitbuf == EOF)
            longjmp(s->env, 1);         // out of input
        if (left > 8)
            left = 8;
    }
    return IG_BAD_CODE_ERR;             // ran out of codes
}

// Given the list of code lengths length[0..n-1] representing a canonical
// Huffman code for n symbols, construct the tables required to decode those
// codes. Those tables are the number of codes of each length, and the symbols
// sorted by length, retaining their original order within each length. The
// return value is zero for a complete code set, negative for an over-
// subscribed code set, and positive for an incomplete code set. The tables can
// be used if the return value is zero or positive, but they cannot be used if
// the return value is negative. If the return value is zero, it is not
// possible for decode() using that table to return an error--any stream of
// enough bits will resolve to a symbol. If the return value is positive, then
// it is possible for decode() using that table to return an error for received
// codes past the end of the incomplete lengths.
//
// Not used by decode(), but used for error checking, h->count[0] is the number
// of the n symbols not in the code. So n - h->count[0] is the number of codes.
// This is useful for checking for incomplete codes that have more than one
// symbol, which is an error in a dynamic block.
//
// Assumption: for all i in 0..n-1, 0 <= length[i] <= MAXBITS
//
// This is assured by the construction of the length arrays in dynamic() and
// fixed() and is not verified by construct().
local int construct(struct huffman *h, short *length, int n) {
    // Count the number of codes of each length.
    for (int len = 0; len <= MAXBITS; len++)
        h->count[len] = 0;
    for (int symbol = 0; symbol < n; symbol++)
        (h->count[length[symbol]])++;   // assumes lengths are within bounds
    if (h->count[0] == n)               // no codes!
        return 0;                       // complete, but decode() will fail

    // Check for an over-subscribed or incomplete set of lengths.
    int left = 1;                       // one possible code of zero length
    for (int len = 1; len <= MAXBITS; len++) {
        left <<= 1;                     // one more bit, double codes left
        left -= h->count[len];          // deduct count from possible codes
        if (left < 0)
            return left;                // over-subscribed--return negative
    }                                   // left > 0 means incomplete

    // Generate offsets into symbol table for each length for sorting.
    short offs[MAXBITS+1];      // offsets in symbol table for each length
    offs[1] = 0;
    for (int len = 1; len < MAXBITS; len++)
        offs[len + 1] = offs[len] + h->count[len];

    // Put symbols in table sorted by length, by symbol order within each
    // length.
    for (int symbol = 0; symbol < n; symbol++)
        if (length[symbol] != 0)
            h->symbol[offs[length[symbol]]++] = symbol;

    // Return zero for complete set, positive for incomplete set.
    return left;
}

// Decode literal/length and distance codes until an end-of-block code.
local int codes(struct state *s,
                struct huffman *lencode,
                struct huffman *distcode) {
    static const short lens[29] = { // size base for length codes 257..285
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258};
    static const short lext[29] = { // extra bits for length codes 257..285
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0};
    static const short dists[30] = { // offset base for distance codes 0..29
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577};
    static const short dext[30] = { // extra bits for distance codes 0..29
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13};

    // Decode literals and length/distance pairs.
    int symbol;
    do {
        uintmax_t beg = s->blockin;
        symbol = decode(s, lencode);
        s->symbols++;
        if (symbol < 0)
            return symbol;              // invalid symbol
        if (symbol < 256) {             // literal: symbol is the byte
            // Write out the literal.
            if (s->binary) {
                if (symbol < 0x7f)
                    putc(symbol + 0x80, s->out);
                else {
                    putc(0xff, s->out);
                    putc(symbol, s->out);
                }
            }
            if (s->data)
                putval(symbol, "literal", 1, s);
            s->blockout += 1;
            if (s->max < s->win)
                s->max++;
            s->litbits += s->blockin - beg;
        }
        else if (symbol > 256) {        // length
            // Get and compute length.
            if (symbol >= MAXLCODES)
                return IG_BAD_CODE_ERR;         // invalid fixed code
            symbol -= 257;
            int len = lens[symbol] + bits(s, lext[symbol]);

            // Get distance.
            symbol = decode(s, distcode);
            if (symbol < 0)
                return symbol;                  // invalid symbol
            unsigned dist = dists[symbol] + bits(s, dext[symbol]);

            // Check distance and write match.
            if (s->binary) {
                putc((dist -1) >> 8, s->out);
                putc(dist - 1, s->out);
                putc(len - 3, s->out);
            }
            if (s->data) {
                if (s->col) {
                    putc('\n', s->out);
                    s->col = 0;
                }
                s->col = fprintf(s->out, "match %d %u", len, dist);
                putbits(s);
            }
            if (dist > s->max) {
                warn("distance too far back (%u/%u)", dist, s->max);
                s->max = MAXDIST;       // issue warning only once
            }

            // Update state for match.
            if (dist > s->blockout) {
                dist -= s->blockout;
                if (dist > s->reach)
                    s->reach = dist;
            }
            s->blockout += len;
            s->matches++;
            s->matchlen += len;
            if (s->max < s->win) {
                if (len > (int)(s->win - s->max))
                    s->max = s->win;
                else
                    s->max += len;
            }
            s->matbits += s->blockin - beg;
        }
    } while (symbol != 256);            // end of block symbol
    s->symbols--;

    // Write end of block code.
    if (s->data) {
        if (s->col) {
            putc('\n', s->out);
            s->col = 0;
        }
        fputs("end", s->out);
        s->col = 3;
        putbits(s);
    }
    if (s->stats) {
        if (s->symbols != s->matches)
            fprintf(s->out, "! stats literals %.1f bits each (%" PRIuMAX
                            "/%" PRIuMAX ")\n",
                    s->litbits / (double)(s->symbols - s->matches),
                    s->litbits, s->symbols - s->matches);
        else
            fprintf(s->out, "! stats literals none\n");
        s->littot += s->litbits;
        if (s->matches) {
            fprintf(s->out, "! stats matches %.1f%% (%" PRIuMAX " x %.1f)\n",
                    100 * (s->matchlen / (double)(s->blockout)),
                    s->matches, s->matchlen / (double)(s->matches));
            s->matchnum += s->matches;
            s->matchtot += s->matchlen;
        }
        else
            fprintf(s->out, "! stats matches none\n");
        end(s);
    }

    // Done with a valid fixed or dynamic block.
    return IG_OK;
}

// Process a fixed codes block.
local int fixed(struct state *s) {
    static short lencnt[MAXBITS+1], lensym[FIXLCODES];
    static short distcnt[MAXBITS+1], distsym[MAXDCODES];
    static struct huffman lencode = {lencnt, lensym};
    static struct huffman distcode = {distcnt, distsym};

    // Build fixed huffman tables if first call (not thread safe).
    static int virgin = 1;
    if (virgin) {
        int symbol;
        short lengths[FIXLCODES];

        // Literal/length table.
        for (symbol = 0; symbol < 144; symbol++)
            lengths[symbol] = 8;
        for (; symbol < 256; symbol++)
            lengths[symbol] = 9;
        for (; symbol < 280; symbol++)
            lengths[symbol] = 7;
        for (; symbol < FIXLCODES; symbol++)
            lengths[symbol] = 8;
        construct(&lencode, lengths, FIXLCODES);

        // Distance table.
        for (symbol = 0; symbol < MAXDCODES; symbol++)
            lengths[symbol] = 5;
        construct(&distcode, lengths, MAXDCODES);

        // Do this just once.
        virgin = 0;
    }

    // Decode data until end-of-block code.
    s->headlen = s->blockin;
    return codes(s, &lencode, &distcode);
}

// Process a dynamic codes block.
local int dynamic(struct state *s) {
    // Get number of lengths in each table, check lengths.
    if (s->data && s->col) {
        putc('\n', s->out);
        s->col = 0;
    }
    int nlen = bits(s, 5) + 257;
    int ndist = bits(s, 5) + 1;
    int ncode = bits(s, 4) + 4;
    if (nlen > MAXLCODES || ndist > MAXDCODES)
        return IG_TOO_MANY_CODES_ERR;       // bad counts
    if (s->binary) {
        putc(nlen - 256, s->out);
        putc(ndist, s->out);
        putc(ncode, s->out);
    }
    if (s->draw) {
        s->col = fprintf(s->out, "count %d %d %d", nlen, ndist, ncode);
        putbits(s);
    }

    // Read code length code lengths (really), missing lengths are zero.
    short lengths[MAXCODES];            // descriptor code lengths
    static const short order[19] =      // permutation of code length codes
        {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};
    int index;
    for (index = 0; index < ncode; index++) {
        int len = bits(s, 3);
        lengths[order[index]] = len;
        if (s->binary)
            putc(len + 1, s->out);
        if (s->draw && len) {
            s->col = fprintf(s->out, "code %d %d", order[index], len);
            putbits(s);
        }
    }
    for (; index < 19; index++)
        lengths[order[index]] = 0;

    // Build huffman table for code lengths codes (use lencode temporarily).
    short lencnt[MAXBITS+1], lensym[MAXLCODES];         // lencode memory
    struct huffman lencode = {lencnt, lensym};          // length code
    int err = construct(&lencode, lengths, 19);
    if (err < 0)
        return IG_CODE_LENGTHS_CODE_OVER_ERR;   // oversubscribed
    else if (err > 0)
        return IG_CODE_LENGTHS_CODE_UNDER_ERR;  // incomplete

    // Read length/literal and distance code length tables.
    index = 0;
    while (index < nlen + ndist) {
        int symbol = decode(s, &lencode);
        if (symbol < 0)
            return symbol;              // invalid symbol
        if (symbol < 16) {              // length in 0..15
            if (s->binary)
                putc(symbol + 1, s->out);
            if (s->draw)
                putval(symbol, "lens", 1, s);
            lengths[index++] = symbol;
        }
        else {                          // repeat instruction
            int len = -1;               // assume repeating zeros
            if (symbol == 16) {         // repeat last length 3..6 times
                if (index == 0)
                    return IG_REPEAT_NO_FIRST_ERR;  // no last length!
                len = lengths[index - 1];           // last length
                symbol = 3 + bits(s, 2);
            }
            else if (symbol == 17)      // repeat zero 3..10 times
                symbol = 3 + bits(s, 3);
            else                        // == 18, repeat zero 11..138 times
                symbol = 11 + bits(s, 7);
            if (index + symbol > nlen + ndist)
                return IG_REPEAT_TOO_MANY_ERR;  // too many lengths!
            if (s->binary)
                putc(symbol + (len == -1 ? 18 : 14), s->out);
            if (s->draw) {
                if (s->col) {
                    putc('\n', s->out);
                    s->col = 0;
                }
                s->col = fprintf(s->out, "%s %d",
                                 len == -1 ? "zeros" : "repeat", symbol);
                putbits(s);
            }
            if (len == -1)
                len = 0;
            while (symbol--)            // repeat last or zero symbol times
                lengths[index++] = len;
        }
    }
    if (s->binary)
        putc(0, s->out);
    if (s->draw && s->col) {
        putc('\n', s->out);
        s->col = 0;
    }
    if (s->stats)
        fprintf(s->out, "! stats table %" PRIuMAX ":%" PRIuMAX "\n",
                (s->blockin - 3) >> 3, (s->blockin - 3) & 7);

    // Write literal/length and distance code lengths.
    if (s->tree) {
        for (index = 0; index < nlen; index++)
            if (lengths[index] != 0)
                fprintf(s->out, "%slitlen %d %d\n", s->draw ? "! " : "",
                        index, lengths[index]);
        for (; index < nlen + ndist; index++)
            if (lengths[index] != 0)
                fprintf(s->out, "%sdist %d %d\n", s->draw ? "! " : "",
                        index - nlen, lengths[index]);
    }

    // Check for end-of-block code -- there better be one!
    if (lengths[256] == 0)
        return IG_NO_END_CODE_ERR;

    // Build huffman table for literal/length codes.
    err = construct(&lencode, lengths, nlen);
    if (err < 0)
        return IG_LITLEN_CODE_OVER_ERR;
    else if (err > 0 && nlen - lencode.count[0] != 1)
        return IG_LITLEN_CODE_UNDER_ERR;    // incomplete with one code ok

    // Build huffman table for distance codes.
    short distcnt[MAXBITS+1], distsym[MAXDCODES];       // distcode memory
    struct huffman distcode = {distcnt, distsym};       // distance code
    err = construct(&distcode, lengths + nlen, ndist);
    if (err < 0)
        return IG_DIST_CODE_OVER_ERR;
    else if (err > 0 && ndist - distcode.count[0] != 1)
        return IG_DIST_CODE_UNDER_ERR;      // incomplete with one code ok

    // Decode data until end-of-block code.
    s->headlen = s->blockin;
    return codes(s, &lencode, &distcode);
}

// Write val as a variable-length integer to out.
local void putvar(uintmax_t u, FILE *out) {
    while (u > 0x7f) {
        putc((u & 0x7f) | 0x80, out);
        u >>= 7;
    }
    putc(u, out);
}

// Inflate in to out, writing a defgen description of the input stream. On
// success, the return value of infgen() is IG_OK (0). If there is an error in
// the source data, i.e. it is not in the deflate format, then a negative value
// is returned. If there is not enough input available, then IG_INCOMPLETE is
// returned.
//
// infgen()'s return codes are documented near the top of this source file.
local int infgen(struct state *s) {
    // Initialize input state.
    s->bitcnt = 0;
    s->bitbuf = 0;
    s->seqs = 0;

    // Initialize output state.
    s->col = 0;
    s->max = 0;

    // Initialize statistics.
    s->blocks = 0;
    s->inbits = 0;
    s->outbytes = 0;
    s->symbnum = 0;
    s->matchnum = 0;
    s->matchtot = 0;
    s->littot = 0;

    // Return if bits() or decode() tries to read past available input.
    int err = 0;
    if (setjmp(s->env) != 0)            // if came back here via longjmp()
        err = IG_INCOMPLETE;            // then skip do-loop, return error
    else {
        // Process blocks until last block or error.
        int last;
        do {
            if (s->data)
                fputs("!\n", s->out);
            s->reach = 0;
            s->blockin = 0;
            s->blockout = 0;
            s->symbols = 0;
            s->matches = 0;
            s->matchlen = 0;
            s->litbits = 0;
            s->matbits = 0;
            last = bits(s, 1);          // one if last block
            if (s->data && last) {
                fputs("last", s->out);
                s->col = 4;
                putbits(s);
            }
            int type = bits(s, 2);      // block type 0..3
            if (s->binary) {
                putc(0xff, s->out);
                putc((type << 1) + last, s->out);
            }
            switch (type) {
            case 0:
                if (s->binary)
                    putc(s->bitbuf + (s->binary > 1 ? 1 << s->bitcnt : 0),
                         s->out);
                if (s->data) {
                    fputs("stored", s->out);
                    s->col = 6;
                    if (s->bitbuf)
                        s->col += fprintf(s->out, " %d", s->bitbuf);
                    putbits(s);
                }
                err = stored(s);
                break;
            case 1:
                if (s->data) {
                    fputs("fixed", s->out);
                    s->col = 5;
                    putbits(s);
                }
                err = fixed(s);
                break;
            case 2:
                if (s->data) {
                    fputs("dynamic", s->out);
                    s->col = 7;
                    putbits(s);
                }
                err = dynamic(s);
                break;
            default:    // 3
                if (s->data) {
                    fputs("block3", s->out);
                    s->col = 6;
                    putbits(s);
                }
                err = IG_BLOCK_TYPE_ERR;
            }
            if (err != IG_OK)
                break;                  // return with error
            if (s->binary > 1) {
                putc(0xff, s->out);
                putc(9, s->out);
                putvar(s->blockin - 9, s->out);
                putvar(s->headlen - 2, s->out);
                putvar(s->litbits + 1, s->out);
                putvar(s->matbits + 1, s->out);
                putc(0, s->out);
            }
        } while (!last);
    }

    // Finish off dangling literal line.
    if (s->data && s->col)
        putc('\n', s->out);
    s->col = 0;

    // Write the leftovers information.
    if (s->binary) {
        putc(0xff, s->out);
        putc(8, s->out);
        putc(s->bitbuf + (s->binary > 1 ? 1 << s->bitcnt : 0), s->out);
    }
    if (s->data && s->bitcnt && s->bitbuf)
        s->col += fprintf(s->out, "bound %d", s->bitbuf);
    if (s->draw > 1 && s->bitcnt && s->seqs < MAXSEQS) {
        s->seq[s->seqs] = s->bitbuf;
        s->len[s->seqs] = s->bitcnt;
        s->seqs++;
        putbits(s);
    }
    else if (s->data && s->bitcnt && s->bitbuf)
        putc('\n', s->out);

    // Write final statistics.
    if (s->stats) {
        fprintf(s->out, "! stats total inout %" PRIuMAX ":%" PRIuMAX
                       " (%" PRIuMAX ") %" PRIuMAX "\n",
                s->inbits >> 3, s->inbits & 7, s->symbnum, s->outbytes);
        fprintf(s->out, "! stats total block average %.1f uncompressed\n",
                s->outbytes / (double)s->blocks);
        fprintf(s->out, "! stats total block average %.1f symbols\n",
                s->symbnum / (double)s->blocks);
        fprintf(s->out, "! stats total literals %.1f bits each\n",
                s->littot / (double)(s->symbnum - s->matchnum));
        if (s->matchnum)
            fprintf(s->out, "! stats total matches %.1f%% (%" PRIuMAX
                           " x %.1f)\n",
                    100 * (s->matchtot / (double)(s->outbytes)),
                    s->matchnum, s->matchtot / (double)(s->matchnum));
        else
            fprintf(s->out, "! stats total no matches\n");
    }

    // Return error state.
    return err;
}

// Provide help for the command options.
local void help(void) {
    fputs(
          "\n"
          "infgen " IG_VERSION "\n"
          "Usage:\n"
          "\n"
          "  infgen [-d[d]q[q]isrb[b]] input_path > output_path\n"
          "  infgen [-d[d]q[q]isrb[b]] < input_path > output_path\n"
          "\n"
          "    -d   Write raw dynamic header (code lengths in comments)\n"
          "    -dd  Also show the bits for each element displayed\n"
          "    -q   Do not write dynamic code lengths (comments or not)\n"
          "    -qq  Do not write deflate stream description at all\n"
          "    -i   Include detailed gzip / zlib header descriptions\n"
          "    -s   Include deflate block statistics (as comments)\n"
          "    -r   Assume raw deflate data -- do not look for headers\n"
          "    -b   Write compact binary format (only -r honored)\n"
          "    -bb  Write compact binary format with bit counts\n"
          "\n",
          stderr);
}

// Get the next byte of input, or abort if none.
#define NEXT(in) ((n = getc(in)) != EOF ? n : (s.col ? putc('\n', s.out) : 0, \
                  bail("unexpected end of input")))

// Read a gzip, zlib, or raw deflate stream from stdin or a provided path, and
// write a defgen description of the stream to stdout.
int main(int argc, char **argv) {
    // Process command line options.
    char *path = NULL;
    int head = 1;
    int wrap = 1;
    struct state s;
    s.info = 0;
    s.binary = 0;
    s.data = 1;
    s.tree = 1;
    s.draw = 0;
    s.stats = 0;
    s.win = MAXDIST;
    s.chunk = -1;
    while (--argc) {
        char *arg = *++argv;
        if (*arg++ != '-') {
            if (path != NULL)
                bail("only one input file permitted (%s)", arg - 1);
            path = arg - 1;
            continue;
        }
        while (*arg)
            switch (*arg++) {
            case 'i':  s.info = 1;      break;
            case 'b':  s.binary++;      break;
            case 'q':
                if (s.tree)
                    s.tree = 0;
                else
                    s.data = 0;
                break;
            case 'd':  s.draw++;        break;
            case 's':  s.stats = 1;     break;
            case 'r':  head = 0;        break;
            case 'h':  help();          return 0;
            default:
                bail("invalid option '%c' (type infgen for help)", *--arg);
            }
    }
    if (s.data == 0)
        s.draw = 0;

    // Set input and output.
    if (path == NULL) {
        if (isatty(0)) {
            help();
            return 0;
        }
        errno = 0;          // isatty(0) false leaves errno as ENOTTY
        s.in = stdin;
        SET_BINARY_MODE(s.in);
    }
    else {
        s.in = fopen(path, "rb");
        if (s.in == NULL)
            bail("could not open input file %s", path);
    }
    s.out = stdout;
    if (s.binary) {
        wrap = s.info = s.data = s.tree = s.draw = s.stats = 0;
        SET_BINARY_MODE(s.out);
    }
    s.col = 0;

    // Say what wrote this.
    if (wrap)
        fputs("! infgen " IG_VERSION " output\n", s.out);

    // Process concatenated streams.
    int ret;
    do {
        // Skip header, if any, save header type as trailer size.
        ret = getc(s.in);
        int n = getc(s.in);
        unsigned val = ((unsigned)ret << 8) + (unsigned)n;
        int trail;
        if (ret == EOF) {
            // nothing after the last stream, or empty file
            ret = 0;
            break;
        }
        else if (head && n != EOF && val == 0x1f8b) {
            // gzip header
            if (wrap)
                fputs("!\n", s.out);
            if (NEXT(s.in) != 8)
                bail("unknown gzip compression method %d", n);
            ret = NEXT(s.in);
            if (ret & 0xe0)
                bail("reserved gzip flags set (%02x)", ret);
            if (s.info && (ret & 1))
                fputs("text\n", s.out);
            unsigned long num = NEXT(s.in);
            num += NEXT(s.in) << 8;
            num += NEXT(s.in) << 16;
            num += NEXT(s.in) << 24;
            if (s.info && num) {
                time_t t = num;
                s.col = fprintf(s.out, "time %lu", num);
                seqtab(&s);
                char at[64];
                strncpy(at, asctime(gmtime(&t)), sizeof(at) - 1);
                at[sizeof(at) - 1] = 0;
                char *end = at + strlen(at) - 1;
                if (*end == '\n')
                    *end = 0;
                fprintf(s.out, "! [UTC %s]\n", at);
                s.col = 0;
            }
            val = NEXT(s.in);
            if (s.info && val)
                fprintf(s.out, "xfl %u\n", val);
            val = NEXT(s.in);
            if (s.info && val != 3)
                fprintf(s.out, "os %u\n", val);
            if (ret & 4) {              // extra field
                val = NEXT(s.in);
                val += NEXT(s.in) << 8;
                if (val == 0) {
                    if (s.info)
                        fputs("extra '\n", s.out);
                }
                else {
                    unsigned sub = 0;   // offset within sub-field
                    char id[3] = {0};   // sub-field ID
                    unsigned len = 0;   // sub-field content length
                    int ok = 1;         // false if sub-fields invalid
                    do {
                        NEXT(s.in);
                        if (s.info) {
                            putval(n, "extra", 0, &s);
                            if (ok) {
                                if (sub < 2)
                                    // sub-field ID byte
                                    id[sub] = n;
                                else if (sub == 2)
                                    // low byte of sub-field content length
                                    len = n;
                                else if (sub == 3) {
                                    // high byte of sub-field content length
                                    len += (unsigned)n << 8;
                                    if (len < val) {
                                        // sub-field fits in extra field
                                        seqtab(&s);
                                        fprintf(s.out, "! [id='%s' len=%u]\n",
                                                id, len);
                                        s.col = 0;
                                        if (len == 0) {
                                            sub = 0;
                                            continue;
                                        }
                                    }
                                    else
                                        // sub-field doesn't fit -- invalid
                                        ok = 0;
                                }
                                else {
                                    // sub-field content
                                    if (--len == 0) {
                                        if (s.col) {
                                            putc('\n', s.out);
                                            s.col = 0;
                                        }
                                        sub = 0;
                                        continue;
                                    }
                                }
                                sub++;
                            }
                        }
                    } while (--val);
                    if (s.info && (!ok || (sub > 0 && sub < 4) || len)) {
                        // invalid sub-field structure
                        if (s.col) {
                            putc('\n', s.out);
                            s.col = 0;
                        }
                        seqtab(&s);
                        fputs("! [invalid sub-field structure]\n", s.out);
                        s.col = 0;
                    }
                }
                if (s.info && s.col) {
                    putc('\n', s.out);
                    s.col = 0;
                }
            }
            if (ret & 8) {              // file name
                if (NEXT(s.in) == 0) {
                    if (s.info)
                        fputs("name '\n", s.out);
                }
                else
                    do {
                        if (s.info)
                            putval(n, "name", 0, &s);
                    } while (NEXT(s.in) != 0);
                if (s.info && s.col) {
                    putc('\n', s.out);
                    s.col = 0;
                }
            }
            if (ret & 16) {             // comment field
                if (NEXT(s.in) == 0) {
                    if (s.info)
                        fputs("comment '\n", s.out);
                }
                else
                    do {
                        if (s.info)
                            putval(n, "comment", 0, &s);
                    } while (NEXT(s.in) != 0);
                if (s.info && s.col) {
                    putc('\n', s.out);
                    s.col = 0;
                }
            }
            if (ret & 2) {              // header crc
                NEXT(s.in);
                NEXT(s.in);
                if (s.info)
                    fputs("hcrc\n", s.out);
            }
            trail = 8;
            if (wrap)
                fputs("gzip\n", s.out);
        }
        else if (head && n != EOF && val == (137 << 8) + 'P') {
            // png file. Verify the remainder of "PNG".
            if (NEXT(s.in) != 'N' || NEXT(s.in) != 'G')
                bail("invalid PNG header");
            if (s.info)
                fputs("!\n", s.out);

            // Now we are four bytes before the start of first png chunk. We
            // set those four bytes of header to be checked as if they are the
            // CRC of a preceding chunk.
            s.crc = ~0x0d0a1a0a;
            s.table = get_crc_table();

            // Get what should be a zlib header.
            s.chunk = 0;
            ret = get(&s);
            n = get(&s);
            val = ((unsigned)ret << 8) + (unsigned)n;
            if (n == EOF || val % 31 || (ret & 0xf) != 8 || (ret >> 4) > 7)
                bail("invalid zlib header in IDAT");
            goto zlib;
        }
        else if (head && n != EOF && val % 31 == 0 && (ret & 0xf) == 8 &&
                 (ret >> 4) < 8) {
            // zlib header.
          zlib:
            if (wrap)
                fputs("!\n", s.out);
            if (s.info && (val & 0xe0) != 0x80)   // compression level
                fprintf(s.out, "level %d\n", (val >> 6) & 3);
            if (val & 0x20) {                   // preset dictionary
                if (s.chunk != -1)
                    bail("preset dictionary not valid in PNG");
                unsigned long num = NEXT(s.in);
                num = (num << 8) + NEXT(s.in);
                num = (num << 8) + NEXT(s.in);
                num = (num << 8) + NEXT(s.in);
                if (s.info)
                    fprintf(s.out, "dict %lu\n", num);
            }
            ret = (ret >> 4) + 8;
            s.win = 1U << ret;          // set window size from header
            trail = 4;
            if (s.info && ret != 15)
                fprintf(s.out, "zlib %d\n", ret);
            else if (wrap)
                fputs("zlib\n", s.out);
        }
        else {
            // Raw deflate data, put non-header bytes back (assumes two ok).
            ungetc(n, s.in);
            ret = ungetc(ret, s.in);    // this should work, but ...
            if (ret == EOF)             // only one ungetc() guaranteed
                bail("could not ungetc() a second time (!)");
            trail = 0;
        }

        // Process compressed data to produce a defgen description.
        ret = infgen(&s);

        // Check return value and trailer size.
        if (ret > 0)
            warn("incomplete deflate data");
        else if (ret < 0)
            warn("invalid deflate data -- %s",
                 -ret > 0 && -ret <= (int)IG_ERRS ?
                    inferr[-1 - ret] : "unknown");
        else {
            n = 0;
            while (n < trail && get(&s) != EOF)
                n++;
            if (n < trail) {
                warn("incomplete %s trailer", trail == 4 ? "zlib" : "gzip");
                ret = 2;
            }
        }

        // Write defgen trailer (note: trailer is not validated).
        if (ret == 0 && wrap) {
            if (trail == 4)
                fputs("!\nadler\n", s.out);
            else if (trail == 8)
                fputs("!\ncrc\nlength\n", s.out);
        }

        if (s.chunk != -1) {
            // Parse remainder of PNG file.
            fputs("!\n", s.out);
            if (s.chunk || get(&s) != EOF)
                warn("invalid PNG file structure");
            break;
        }
    } while (ret == 0);

    // Done.
    fflush(s.out);
    if (path != NULL)
        fclose(s.in);
    if ((ferror(s.in) || ferror(s.out)) && errno)
        bail("i/o error: %s", strerror(errno));
    return ret;
}
