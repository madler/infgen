/*
 * infgen.c
 * Copyright (C) 2005-2013 Mark Adler, all rights reserved.
 * Version 2.1  13 Jan 2013
 *
 * Read a zlib, gzip, or raw deflate stream from stdin and write a defgen
 * compatible stream representing that input to stdout (though any specific
 * zlib or gzip header information will be lost).  This is based on the puff.c
 * code to decompress deflate streams.  Note that neither the zlib nor the gzip
 * trailer is checked against the uncompressed data (in fact the uncompressed
 * data is never generated) -- all that is checked is that the trailer is
 * present.
 *
 * Usage: infgen [-q] [-n] [-d] [-s] [-r] < foo.gz > foo.def
 *
 * where foo.gz is a gzip file (it could have been a zlib or raw deflate stream
 * as well), and foo.def is a defgen description of the file or stream, which
 * is a readable text format.  The -q option supresses the output of literals,
 * matches, and tree descriptions.  The -n option supresses just the tree
 * descriptions in the output.  The -d option shows the raw dynamic header
 * information as comments.  The -s option writes out comments with statistics
 * for each deflate block and totals at the end.  The -r option forces
 * interpretation of the input as a raw deflate stream, for those cases where
 * the start of a raw stream coincidentally mimics a valid zlib header.
 */

/*

 defgen format:

    Content:

    The defgen format consists of lines of comments and directives.  Each line
    is terminated by a single new line character ('\n').  The directives are
    used to construct (or reconstruct) a deflate stream.  Each directive is a
    word at the start of the line, possibly followed by parameters.

    Comments:

    defgen lines whose first character is an exclamation mark ('!') are
    comments, and are ignored by defgen.  Blank lines are also ignored.  All
    other lines are directives.  (More on infgen-generated informational
    comments below.)

    Headers and trailers:

    The "gzip" directive writes a gzip header.  It is optionally preceded by
    directives bearing information to be contained in the gzip header: "name",
    "comment", "extra", "time", "os", and "xfl".  The "name", "comment", and
    "extra" directives use the same parameter format as "data" and "literal"
    described below, and may be repeated over multiple lines for long content.
    For "name" and "comment", the parameters do not include the terminating
    zero.  For example:

    name 'linux-3.1.6.tar

    The "time", "os", and "xfl" (extra flags) directives each have a single
    numeric parameter. "os" and "xfl" have a parameter in the range of 0..255,
    and "time" is in the the range of 0..2^32-1.  If "os" is not present, it is
    taken to be 3 (for Unix).  If "xfl" or "time" is not present, the value is
    taken to be zero.

    The "crc" directive writes the CRC-32 of the uncompressed data in
    little-endian order.  The "length" directive writes the length of the
    uncompressed data, modulo 2^32, in little-endian order.  The combination of
    a crc and a length in that order is the gzip trailer.  Either or both can
    optionally have a numeric parameter in the range 0..2^32-1 which would be
    used in place of the value derived from the data.

    The "zlib" directive writes a zlib header.  The zlib directive has an
    optional numeric parameter which is the log-base-2 of the window size, in
    the range 8..15.  If there is no parameter, 15 is assumed.  zlib may be
    preceded by the "level" directive, which has one parameter: the compression
    level used when compressing in the range 0..3.  If level is not present, it
    is taken to be 2. zlib may also be preceded by a "dict" directive with the
    dictionary id as the numeric parameter, in the range 0..2^32-1.

    The "adler" directive writes the adler checksum of the uncompressed data in
    big-endian order.  This is the zlib trailer.  adler may optionally have a
    numeric parameter in the range 0..2^32-1 that is used in place of the
    actual adler checksum of the data.

    Deflate blocks:

    Deflate data between zlib or gzip headers and trailers, or raw deflate
    data, consists of a series of deflate blocks.  They are begun by the block
    type directives: "stored", "static", or "dynamic", and all end with "end"
    after the contents of the block. The last block has the directive "last" on
    its own line before the block type directive.  The "stored" directive has
    an optional parameter which is the data that fills in the dummy bits to get
    to a byte boundary.  If the parameter is not present, those bits are
    assumed to be zero.  An additional "block3" block type indicates the
    illegal bit pattern for a fourth block type.

    Block headers:

    Static blocks have no header, and proceed immediately to the data after the
    static directive.

    A stored block header has as many bits as needed to go to the next byte
    boundary (see the "stored" parameter above), followed by four bytes of
    block length information.  There is no directive for the length of the
    stored block, as it is implied by the amount of data up to the next end
    directive.

    A dynamic block has a header that describes the Huffman codes used to
    represent the literal/length and distance codes in the block.  That
    description is itself compressed with a third code.  The dynamic header is
    represented in one of two ways, or not at all.  If there is no description
    of the header, then the block data can be used to construct an optimal set
    of Huffman codes for the contained symbols, and an optimum way to encode
    them in the header.  In that case, the data immediately follows the dynamic
    directive.

    The first explicit way to describe the header is to list the number of bits
    in each literal/length and distance code.  This is done with the "litlen"
    and "dist" directives.  Each directive has two numerical parameters: the
    symbol index and the number of bits.  E.g. "litlen 40 9" or "dist 16 5". In
    this case, dynamic is followed by all of the litlen directives, which is
    followed by all the dist directives.  The litlen symbol must be in the
    range 0..285, and dist symbol must be in the range 0..29.  The number of
    bits for both must be in the range 1..15.  Only the symbols coded are
    listed.  The header description is complete upon encountering the first
    "literal", "match", or "end".

    The second, more proscriptive way to describe the header is to list the
    actual contents of the header, from which the code lengths are derived.
    This is done with the directives "code", "lenlen", "distlen", "repeat", and
    "zeros".  code has two numerical parameters, the symbol index and the
    number of bits for that symbol. For code, the symbol index is in 0..18 and
    bits are in 1..7.  lenlen, distlen, repeat, and zeros each have one
    numerical parameter.  For lenlen and distlen it is the number of bits in
    0..15.  Zero means that that symbol has no code and does not appear in the
    block.  For repeat and zeros it is the number of times to repeat a bit
    length.  repeat repeats the most recent length 3..6 times.  zeroes repeats
    zeros 3..138 times.  dynamic is followed by all of the code directives, and
    then by the lenlen followed by distlen directives, with repeat and zeros
    directives mixed in.  The header description is complete upon encountering
    the first "literal", "match", or "end".

    Data:

    All compressed data is represented using three directives: "data",
    "literal", and "match".  "data" and "literal" have the same parameters and
    both directly represent bytes of data.  "data" is used only in stored
    blocks and literal is used only in static or dynamic blocks.  The
    parameters of data and literal are a series of decimal numbers separated by
    spaces, followed by a string of printable characters preceded by a single
    quote and ended by the end of line.  A single quote may appear within the
    string meaning a single quote in the data -- it does not end the string.
    Either the numbers or the string are optional. Each decimal number is in
    the range 0..255, and represents one byte of data.  The string can only
    contain printable characters in the range 32..126.  All other byte values
    must be represented as a decimal number.  match has two numerical
    parameters. The first is the length of the match, in 3..258.  The second is
    the distance back, in 1..32768.  The data ends with the "end" directive.

    After the last "end":

    If the last block does not end at a bit boundary, the "bound" directive has
    a single numeric parameter with the filler bits.  If bound is not present,
    the bits up to the byte boundary are filled with zeros.

    infgen comments:

    infgen starts with a comment line indicating the version of infgen that
    generated the defgen format output.  E.g. "! infgen 2.1 output".

    infgen inserts an empty comment, a line with just an exclamation mark,
    before each header, deflate block, and trailers.

    If the -d option is used and the -n option is not used, then the litlen
    and dist directives are written as comments.  E.g. "! litlen 40 9".  If -n
    is used, then they are not written at all.

    With the -s option, infgen will generate statistics comments, all of which
    begin with "! stats ".  There are statistics for each deflate block, and
    summary statistic after the last deflate block.  The statistics comments
    are:

    "! stats table n:m" gives the total number of bytes and bits in the dynamic
    block header, not including the three block identifier bits.  For example,
    "! stats table 58:6" indicating 58 bytes and 6 bits = 470 bits.

    "! stats literals x.x bits each (n/m)" follows a static or dynamic block
    and gives the average number of bits per literal, the total number of bits
    for the literals in the block, and the number of literals in the block. For
    example, "! stats literals 5.7 bits each (3793/664)".  If the block has no
    literals, then "! stats literals none" will be written.

    "! stats matches x.x% (n x x.x)" follows a static or dynamic block and
    gives the percentage of the uncompressed bytes in the block that came from
    matches, the number of matches in the block, and the average match length.
    For example, "! stats matches 82.6% (183 x 17.2)".  If the block has no
    matches, then "! stats matches none" will be written.

    "! stats stored length n" follows each stored block and gives the number of
    uncompressed bytes in the stored block, which does not include the stored
    header.  For example: "! stats stored length 838" is a stored block with
    838 bytes.

    "! stats inout n:m (i) j k" follows any block and gives the total number of
    bytes and bits in the block, including the three-bit block identifier, the
    total number of symbols in the block (a literal and a match each count as
    one symbol), the number of uncompressed bytes generated by the block, and
    the maximum reach of the distances to data before the block.  For example,
    "! stats inout 1889:4 (1906) 3810 -1718" is a block with 1889 bytes and 4
    bits, 1906 symbols, 3810 uncompressed bytes, and maximum reach of 1718
    bytes before the block by a match in the block.  If the block does not
    reach before itself, the reach value is zero.

    After the last deflate block, total statistics are output.  They all begin
    with "! stats total ".  The block input and output amounts are summed for
    example as: "! stats total inout 93232233:0 (55120762) 454563840", with the
    same format as "! stats inout", except without the reach.

    "! stats total block average 34162.3 uncompressed" states for example that
    the average number of uncompressed bytes per block was 34162.3.  Similarly
    "! stats total block average 4142.5 symbols" states that there were 4142.5
    symbols on average per block.  "! stats total literals 6.9 bits each"
    states that there were 6.9 bits used on average per literal.  Lastly the
    matches are summed: "! stats total matches 95.2% (33314520 x 13.0)" with
    the same format as "! stats matches".

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
 */

#include <stdio.h>          /* putc(), fprintf(), getc(), fputs(), fflush(), */
                            /* ungetc() (we assume two ungetc()'s are ok) */
#include <stdlib.h>         /* exit() */
#include <stdint.h>         /* intmax_t */
#include <inttypes.h>       /* PRIuMAX */
#include <string.h>         /* strcmp() */
#include <setjmp.h>         /* setjmp(), longjmp() */

#if defined(MSDOS) || defined(OS2) || defined(_WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) _setmode(_fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define local static

/*
 * Maximums for allocations and loops.  It is not useful to change these --
 * they are fixed by the deflate format.
 */
#define MAXBITS 15              /* maximum bits in a code */
#define MAXLCODES 286           /* maximum number of literal/length codes */
#define MAXDCODES 30            /* maximum number of distance codes */
#define MAXCODES (MAXLCODES+MAXDCODES)  /* maximum codes lengths to read */
#define FIXLCODES 288           /* number of fixed literal/length codes */
#define MAXDIST 32768U          /* maximum match distance */

/* infgen() input and output state */
struct state {
    /* output state */
    int data;                   /* true to output literals and matches */
    int tree;                   /* true to output dynamic tree */
    int draw;                   /* true to output dynamic descriptor */
    int stats;                  /* true to output statistics */
    int col;                    /* state within data line */
    unsigned max;               /* maximum distance (bytes so far) */
    unsigned win;               /* window size from zlib header or 32K */
    FILE *out;                  /* output file */

    /* input state */
    int bitcnt;                 /* number of bits in bit buffer */
    int bitbuf;                 /* bit buffer */
    FILE *in;                   /* input file */

    /* current block statistics */
    unsigned reach;             /* maximum distance before current block */
    uintmax_t blockin;          /* bits in for current block */
    uintmax_t blockout;         /* bytes out for current block */
    uintmax_t symbols;          /* number of symbols (or stored bytes) */
    uintmax_t matches;          /* number of matches */
    uintmax_t matchlen;         /* total length of matches */
    uintmax_t litbits;          /* number of bits in literals */

    /* total statistics */
    uintmax_t blocks;           /* total number of deflate blocks */
    uintmax_t inbits;           /* total deflate bits in */
    uintmax_t outbytes;         /* total uncompressed bytes out */
    uintmax_t symbnum;          /* total number of symbols */
    uintmax_t matchnum;         /* total number of matches */
    uintmax_t matchtot;         /* total length of matches */
    uintmax_t littot;           /* total bits in literals */

    /* input limit error return state for bits() and decode() */
    jmp_buf env;
};

#define LINELEN 79      /* target line length for data and literal commands */

/* Write a byte in a literal or data defgen command, keeping the line length
   reasonable and using string literals whenever possible. */
local void putval(int val, char *command, int *col, FILE *out)
{
    /* new line if too long or decimal after string */
    if (*col == 0 || (*col < 0 ? -*col : *col) > LINELEN - 4 ||
        (*col < 0 && (val < 0x20 || val > 0x7e))) {
        if (*col)
            putc('\n', out);
        *col = fputs(command, out);
    }

    /* string literal (already range-checked above) */
    if (*col < 0) {
        putc(val, out);
        (*col)--;
    }

    /* new string literal (mark with negative lit) */
    else if (val >= 0x20 && val <= 0x7e) {
        *col += fprintf(out, " '%c", val);
        *col = -*col;
    }

    /* decimal literal */
    else
        *col += fprintf(out, " %u", val);
}

/*
 * Return need bits from the input stream.  This always leaves less than
 * eight bits in the buffer.  bits() works properly for need == 0.
 *
 * Format notes:
 *
 * - Bits are stored in bytes from the least significant bit to the most
 *   significant bit.  Therefore bits are dropped from the bottom of the bit
 *   buffer, using shift right, and new bytes are appended to the top of the
 *   bit buffer, using shift left.
 */
local int bits(struct state *s, int need)
{
    int next;           /* next byte from input */
    long val;           /* bit accumulator (can use up to 20 bits) */

    /* load at least need bits into val */
    val = s->bitbuf;
    while (s->bitcnt < need) {
        next = getc(s->in);
        if (next == EOF) longjmp(s->env, 1);    /* out of input */
        val |= (long)(next) << s->bitcnt;       /* load eight bits */
        s->bitcnt += 8;
    }

    /* drop need bits and update buffer, always zero to seven bits left */
    s->bitbuf = (int)(val >> need);
    s->bitcnt -= need;
    s->blockin += need;

    /* return need bits, zeroing the bits above that */
    return (int)(val & ((1L << need) - 1));
}

/*
 * Show statistics at end of block.
 */
local void end(struct state *s)
{
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

/*
 * Process a stored block.
 */
local int stored(struct state *s)
{
    unsigned len;       /* length of stored block */
    unsigned cmp;       /* should be one's complement of len */
    int octet;          /* byte to copy */

    /* discard leftover bits from current byte (assumes s->bitcnt < 8) */
    s->blockin += s->bitcnt;
    s->bitcnt = 0;
    s->bitbuf = 0;

    /* get length and check against its one's complement */
    len = getc(s->in);
    len += (unsigned)(getc(s->in)) << 8;
    cmp = getc(s->in);
    octet = getc(s->in);
    if (octet == EOF) return 1;                 /* not enough input */
    cmp += (unsigned)octet << 8;
    if (len != (~cmp & 0xffff)) return -2;      /* didn't match complement! */
    s->blockin += 32;
    if (s->stats) {
        if (s->col) { putc('\n', s->out); s->col = 0; }
        fprintf(s->out, "! stats stored length %u\n", len);
    }

    /* update max distance */
    if (s->max < s->win) {
        if (len > s->win - s->max)
            s->max = s->win;
        else
            s->max += len;
    }

    /* copy len bytes from in to out */
    while (len--) {
        octet = getc(s->in);
        s->blockin += 8;
        if (octet == EOF) return 1;             /* not enough input */
        if (s->data)
            putval(octet, "data", &s->col, s->out);
        s->blockout++;
        s->symbols++;
    }

    /* done with a valid stored block */
    if (s->data) {
        if (s->col) { putc('\n', s->out); s->col = 0; }
        fputs("end\n", s->out);
    }
    if (s->stats)
        end(s);
    return 0;
}

/*
 * Huffman code decoding tables.  count[1..MAXBITS] is the number of symbols of
 * each length, which for a canonical code are stepped through in order.
 * symbol[] are the symbol values in canonical order, where the number of
 * entries is the sum of the counts in count[].  The decoding process can be
 * seen in the function decode() below.
 */
struct huffman {
    short *count;       /* number of symbols of each length */
    short *symbol;      /* canonically ordered symbols */
};

/*
 * Decode a code from the stream s using huffman table h.  Return the symbol or
 * a negative value if there is an error.  If all of the lengths are zero, i.e.
 * an empty code, or if the code is incomplete and an invalid code is received,
 * then -10 is returned after reading MAXBITS bits.
 */
local int decode(struct state *s, struct huffman *h)
{
    int len;            /* current number of bits in code */
    int code;           /* len bits being decoded */
    int first;          /* first code of length len */
    int count;          /* number of codes of length len */
    int index;          /* index of first code of length len in symbol table */
    int bitbuf;         /* bits from stream */
    int left;           /* bits left in next or left to process */
    short *next;        /* next number of codes */

    bitbuf = s->bitbuf;
    left = s->bitcnt;
    code = first = index = 0;
    len = 1;
    next = h->count + 1;
    while (1) {
        while (left--) {
            code |= bitbuf & 1;
            bitbuf >>= 1;
            count = *next++;
            if (code < first + count) { /* if length len, return symbol */
                s->bitbuf = bitbuf;
                s->bitcnt = (s->bitcnt - len) & 7;
                s->blockin += len;
                return h->symbol[index + (code - first)];
            }
            index += count;             /* else update for next length */
            first += count;
            first <<= 1;
            code <<= 1;
            len++;
        }
        left = (MAXBITS+1) - len;
        if (left == 0) break;
        bitbuf = getc(s->in);
        if (bitbuf == EOF) longjmp(s->env, 1);          /* out of input */
        if (left > 8) left = 8;
    }
    return -10;                         /* ran out of codes */
}

/*
 * Given the list of code lengths length[0..n-1] representing a canonical
 * Huffman code for n symbols, construct the tables required to decode those
 * codes.  Those tables are the number of codes of each length, and the symbols
 * sorted by length, retaining their original order within each length.  The
 * return value is zero for a complete code set, negative for an over-
 * subscribed code set, and positive for an incomplete code set.  The tables
 * can be used if the return value is zero or positive, but they cannot be used
 * if the return value is negative.  If the return value is zero, it is not
 * possible for decode() using that table to return an error--any stream of
 * enough bits will resolve to a symbol.  If the return value is positive, then
 * it is possible for decode() using that table to return an error for received
 * codes past the end of the incomplete lengths.
 *
 * Not used by decode(), but used for error checking, h->count[0] is the number
 * of the n symbols not in the code.  So n - h->count[0] is the number of
 * codes.  This is useful for checking for incomplete codes that have more than
 * one symbol, which is an error in a dynamic block.
 *
 * Assumption: for all i in 0..n-1, 0 <= length[i] <= MAXBITS
 * This is assured by the construction of the length arrays in dynamic() and
 * fixed() and is not verified by construct().
 */
local int construct(struct huffman *h, short *length, int n)
{
    int symbol;         /* current symbol when stepping through length[] */
    int len;            /* current length when stepping through h->count[] */
    int left;           /* number of possible codes left of current length */
    short offs[MAXBITS+1];      /* offsets in symbol table for each length */

    /* count number of codes of each length */
    for (len = 0; len <= MAXBITS; len++)
        h->count[len] = 0;
    for (symbol = 0; symbol < n; symbol++)
        (h->count[length[symbol]])++;   /* assumes lengths are within bounds */
    if (h->count[0] == n)               /* no codes! */
        return 0;                       /* complete, but decode() will fail */

    /* check for an over-subscribed or incomplete set of lengths */
    left = 1;                           /* one possible code of zero length */
    for (len = 1; len <= MAXBITS; len++) {
        left <<= 1;                     /* one more bit, double codes left */
        left -= h->count[len];          /* deduct count from possible codes */
        if (left < 0) return left;      /* over-subscribed--return negative */
    }                                   /* left > 0 means incomplete */

    /* generate offsets into symbol table for each length for sorting */
    offs[1] = 0;
    for (len = 1; len < MAXBITS; len++)
        offs[len + 1] = offs[len] + h->count[len];

    /*
     * put symbols in table sorted by length, by symbol order within each
     * length
     */
    for (symbol = 0; symbol < n; symbol++)
        if (length[symbol] != 0)
            h->symbol[offs[length[symbol]]++] = symbol;

    /* return zero for complete set, positive for incomplete set */
    return left;
}

/*
 * Decode literal/length and distance codes until an end-of-block code.
 */
local int codes(struct state *s,
                struct huffman *lencode,
                struct huffman *distcode)
{
    int symbol;         /* decoded symbol */
    int len;            /* length for copy */
    unsigned dist;      /* distance for copy */
    uintmax_t beg;      /* bit count at start of symbol */
    static const short lens[29] = { /* Size base for length codes 257..285 */
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258};
    static const short lext[29] = { /* Extra bits for length codes 257..285 */
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0};
    static const short dists[30] = { /* Offset base for distance codes 0..29 */
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577};
    static const short dext[30] = { /* Extra bits for distance codes 0..29 */
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13};

    /* decode literals and length/distance pairs */
    do {
        beg = s->blockin;
        symbol = decode(s, lencode);
        s->symbols++;
        if (symbol < 0) return symbol;  /* invalid symbol */
        if (symbol < 256) {             /* literal: symbol is the byte */
            /* write out the literal */
            if (s->data)
                putval(symbol, "literal", &s->col, s->out);
            s->blockout += 1;
            if (s->max < s->win)
                s->max++;
            s->litbits += s->blockin - beg;
        }
        else if (symbol > 256) {        /* length */
            /* get and compute length */
            symbol -= 257;
            if (symbol >= 29) return -10;       /* invalid fixed code */
            len = lens[symbol] + bits(s, lext[symbol]);

            /* get distance */
            symbol = decode(s, distcode);
            if (symbol < 0) return symbol;      /* invalid symbol */
            dist = dists[symbol] + bits(s, dext[symbol]);

            /* check distance and write match */
            if (s->data) {
                if (s->col) { putc('\n', s->out); s->col = 0; }
                fprintf(s->out, "match %d %u\n", len, dist);
            }
            if (dist > s->max) {
                fflush(stdout);
                fprintf(stderr,
                        "infgen warning: distance too far back (%u/%u)\n",
                        dist, s->max);
                s->max = MAXDIST;       /* issue warning only once */
            }

            /* update state for match */
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
        }
    } while (symbol != 256);            /* end of block symbol */
    s->symbols--;

    /* write end of block code */
    if (s->data) {
        if (s->col) { putc('\n', s->out); s->col = 0; }
        fputs("end\n", s->out);
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

    /* done with a valid fixed or dynamic block */
    return 0;
}

/*
 * Process a fixed codes block.
 */
local int fixed(struct state *s)
{
    static int virgin = 1;
    static short lencnt[MAXBITS+1], lensym[FIXLCODES];
    static short distcnt[MAXBITS+1], distsym[MAXDCODES];
    static struct huffman lencode = {lencnt, lensym};
    static struct huffman distcode = {distcnt, distsym};

    /* build fixed huffman tables if first call (may not be thread safe) */
    if (virgin) {
        int symbol;
        short lengths[FIXLCODES];

        /* literal/length table */
        for (symbol = 0; symbol < 144; symbol++)
            lengths[symbol] = 8;
        for (; symbol < 256; symbol++)
            lengths[symbol] = 9;
        for (; symbol < 280; symbol++)
            lengths[symbol] = 7;
        for (; symbol < FIXLCODES; symbol++)
            lengths[symbol] = 8;
        construct(&lencode, lengths, FIXLCODES);

        /* distance table */
        for (symbol = 0; symbol < MAXDCODES; symbol++)
            lengths[symbol] = 5;
        construct(&distcode, lengths, MAXDCODES);

        /* do this just once */
        virgin = 0;
    }

    /* decode data until end-of-block code */
    return codes(s, &lencode, &distcode);
}

/*
 * Process a dynamic codes block.
 */
local int dynamic(struct state *s)
{
    int nlen, ndist, ncode;             /* number of lengths in descriptor */
    int index;                          /* index of lengths[] */
    int err;                            /* construct() return value */
    short lengths[MAXCODES];            /* descriptor code lengths */
    short lencnt[MAXBITS+1], lensym[MAXLCODES];         /* lencode memory */
    short distcnt[MAXBITS+1], distsym[MAXDCODES];       /* distcode memory */
    struct huffman lencode = {lencnt, lensym};          /* length code */
    struct huffman distcode = {distcnt, distsym};       /* distance code */
    static const short order[19] =      /* permutation of code length codes */
        {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

    /* get number of lengths in each table, check lengths */
    if (s->data && s->col) { putc('\n', s->out); s->col = 0; }
    nlen = bits(s, 5) + 257;
    ndist = bits(s, 5) + 1;
    ncode = bits(s, 4) + 4;
    if (nlen > MAXLCODES || ndist > MAXDCODES)
        return -3;                      /* bad counts */

    /* read code length code lengths (really), missing lengths are zero */
    for (index = 0; index < ncode; index++)
        lengths[order[index]] = bits(s, 3);
    for (; index < 19; index++)
        lengths[order[index]] = 0;

    /* write code length code lengths */
    if (s->draw)
        for (index = 0; index < 19; index++)
            if (lengths[index] != 0)
                fprintf(s->out, "code %d %d\n", index, lengths[index]);

    /* build huffman table for code lengths codes (use lencode temporarily) */
    err = construct(&lencode, lengths, 19);
    if (err != 0) return -4;            /* require complete code set here */

    /* read length/literal and distance code length tables */
    index = 0;
    while (index < nlen + ndist) {
        int symbol;             /* decoded value */
        int len;                /* last length to repeat */

        symbol = decode(s, &lencode);
        if (symbol < 16) {              /* length in 0..15 */
            if (s->draw) {
                if (index < nlen)
                    fprintf(s->out, "lenlen %d\n", symbol);
                else
                    fprintf(s->out, "distlen %d\n", symbol);
            }
            lengths[index++] = symbol;
        }
        else {                          /* repeat instruction */
            len = -1;                   /* assume repeating zeros */
            if (symbol == 16) {         /* repeat last length 3..6 times */
                if (index == 0) return -5;      /* no last length! */
                len = lengths[index - 1];       /* last length */
                symbol = 3 + bits(s, 2);
            }
            else if (symbol == 17)      /* repeat zero 3..10 times */
                symbol = 3 + bits(s, 3);
            else                        /* == 18, repeat zero 11..138 times */
                symbol = 11 + bits(s, 7);
            if (index + symbol > nlen + ndist)
                return -6;              /* too many lengths! */
            if (s->draw)
                fprintf(s->out, "%s %d\n", len == -1 ? "repeat" : "zeros",
                        symbol);
            if (len == -1)
                len = 0;
            while (symbol--)            /* repeat last or zero symbol times */
                lengths[index++] = len;
        }
    }
    if (s->stats)
        fprintf(s->out, "! stats table %" PRIuMAX ":%" PRIuMAX "\n",
                (s->blockin - 3) >> 3, (s->blockin - 3) & 7);

    /* write literal/length and distance code lengths */
    if (s->tree) {
        for (index = 0; index < nlen; index++)
            if (lengths[index] != 0)
                fprintf(s->out, "%slitlen %d %d\n", s->draw ? "! " : "",
                        index, lengths[index]);
        for (index = nlen; index < nlen + ndist; index++)
            if (lengths[index] != 0)
                fprintf(s->out, "%sdist %d %d\n", s->draw ? "! " : "",
                        index - nlen, lengths[index]);
    }

    /* check for end-of-block code -- there better be one! */
    if (lengths[256] == 0)
        return -9;

    /* build huffman table for literal/length codes */
    err = construct(&lencode, lengths, nlen);
    if (err < 0 || (err > 0 && nlen - lencode.count[0] != 1))
        return -7;      /* only allow incomplete codes if just one code */

    /* build huffman table for distance codes */
    err = construct(&distcode, lengths + nlen, ndist);
    if (err < 0 || (err > 0 && ndist - distcode.count[0] != 1))
        return -8;      /* only allow incomplete codes if just one code */

    /* decode data until end-of-block code */
    return codes(s, &lencode, &distcode);
}

/*
 * Inflate in to out, writing a defgen description of the input stream.
 * On success, the return value of infgen() is zero.  If there is an error in
 * the source data, i.e. it is not in the deflate format, then a negative value
 * is returned.  If there is not enough input available, then a positive error
 * is returned.
 *
 * The return codes are:
 *
 *   1:  available deflate data did not terminate
 *   0:  successful inflate
 *  -1:  invalid block type (type == 3)
 *  -2:  stored block length did not match one's complement
 *  -3:  dynamic block code description: too many length or distance codes
 *  -4:  dynamic block code description: code lengths codes incomplete
 *  -5:  dynamic block code description: repeat lengths with no first length
 *  -6:  dynamic block code description: repeat more than specified lengths
 *  -7:  dynamic block code description: invalid literal/length code lengths
 *  -8:  dynamic block code description: invalid distance code lengths
 *  -9:  dynamic block code description: missing end-of-block code
 * -10:  invalid literal/length or distance code in fixed or dynamic block
 */
local int infgen(FILE *in, FILE *out, unsigned win,
                 int data, int tree, int draw, int stats)
{
    struct state s;             /* input/output state */
    int last, type;             /* block information */
    int err;                    /* return value */

    /* initialize output state */
    s.data = data;
    s.tree = tree;
    s.draw = draw;
    s.stats = stats;
    s.col = 0;
    s.max = 0;
    s.win = win;
    s.out = out;

    /* initialize input state */
    s.bitcnt = 0;
    s.bitbuf = 0;
    s.in = in;

    /* initialize statistics */
    s.blocks = 0;
    s.inbits = 0;
    s.outbytes = 0;
    s.symbnum = 0;
    s.matchnum = 0;
    s.matchtot = 0;
    s.littot = 0;

    /* return if bits() or decode() tries to read past available input */
    if (setjmp(s.env) != 0)             /* if came back here via longjmp() */
        err = 1;                        /* then skip do-loop, return error */
    else {
        /* process blocks until last block or error */
        do {
            if (s.data)
                fputs("!\n", s.out);
            s.reach = 0;
            s.blockin = 0;
            s.blockout = 0;
            s.symbols = 0;
            s.matches = 0;
            s.matchlen = 0;
            s.litbits = 0;
            last = bits(&s, 1);         /* one if last block */
            if (s.data && last)
                fputs("last\n", s.out);
            type = bits(&s, 2);         /* block type 0..3 */
            switch (type) {
            case 0:
                if (s.data) {
                    if (s.bitbuf)
                        fprintf(s.out, "stored %d\n", s.bitbuf);
                    else
                        fputs("stored\n", s.out);
                }
                err = stored(&s);
                break;
            case 1:
                if (s.data)
                    fputs("static\n", s.out);
                err = fixed(&s);
                break;
            case 2:
                if (s.data)
                    fputs("dynamic\n", s.out);
                err = dynamic(&s);
                break;
            default:
                if (s.data)
                    fputs("block3\nend\n", s.out);
                err = -1;
            }
            if (err != 0) break;        /* return with error */
        } while (!last);
    }

    /* finish off dangling literal line */
    if (s.data && s.col) putc('\n', s.out);

    /* write the leftovers information */
    if (s.data && s.bitcnt && s.bitbuf)
        fprintf(s.out, "bound %d\n", s.bitbuf);

    /* write final statistics */
    if (s.stats) {
        fprintf(s.out, "! stats total inout %" PRIuMAX ":%" PRIuMAX
                       " (%" PRIuMAX ") %" PRIuMAX "\n",
                s.inbits >> 3, s.inbits & 7, s.symbnum, s.outbytes);
        fprintf(s.out, "! stats total block average %.1f uncompressed\n",
                s.outbytes / (double)s.blocks);
        fprintf(s.out, "! stats total block average %.1f symbols\n",
                s.symbnum / (double)s.blocks);
        fprintf(s.out, "! stats total literals %.1f bits each\n",
                s.littot / (double)(s.symbnum - s.matchnum));
        if (s.matchnum)
            fprintf(s.out, "! stats total matches %.1f%% (%" PRIuMAX
                           " x %.1f)\n",
                    100 * (s.matchtot / (double)(s.outbytes)),
                    s.matchnum, s.matchtot / (double)(s.matchnum));
        else
            fprintf(s.out, "! stats total no matches\n");
    }

    /* return error state */
    return err;
}

/* infgen() negative return code messages */
local char *inferr[] = {
    "invalid block type (3)",
    "stored block length complement mismatch",
    "too many length or distance codes",
    "code lengths code is incomplete",
    "length repeat with no first length",
    "repeat more lengths than available",
    "invalid literal/length code set",
    "invalid distance code set",
    "missing end-of-block code",
    "invalid code"
};

/* print error message and exit (return a value to use in expression) */
local int bail(char *why)
{
    fflush(stdout);
    fprintf(stderr, "infgen error: %s\n", why);
    exit(1);
    return 0;
}

/* get next byte of input, or abort if none */
#define NEXT(in) ((n = getc(in)) != EOF ? n : \
                  (putc('\n', out), bail("unexpected end of input")))

/* Read a gzip, zlib, or raw deflate stream from stdin, and write a defgen
   description of the stream to stdout. defgen only provides simple headers
   for gzip and zlib streams, so most header information is discarded. */
int main(int argc, char **argv)
{
    int info, data, tree, draw, stats, head, ret, trail, n;
    unsigned val, win;
    unsigned long num;
    FILE *in, *out;
    char *arg;
    int col;

    /* process command line options */
    info = 0;
    data = 1;
    tree = 1;
    draw = 0;
    stats = 0;
    head = 1;
    win = MAXDIST;
    while (--argc) {
        arg = *++argv;
        if (*arg++ != '-') {
            fprintf(stderr, "infgen takes input through stdin\n");
            return 1;
        }
        while (*arg)
            switch (*arg++) {
            case 'i':  info = 1;  break;
            case 'q':  data = 0;  /* fall to -n to also not output trees */
            case 'n':  tree = 0;  break;
            case 'd':  draw = 1;  break;
            case 's':  stats = 1;  break;
            case 'r':  head = 0;  break;
            default:
                fprintf(stderr, "infgen error: invalid option %c\n", *--arg);
                return 1;
            }
    }

    /* set input and output */
    in = stdin;
    out = stdout;
    SET_BINARY_MODE(in);
    col = 0;

    /* say what wrote this */
    fputs("! infgen 2.1 output\n", out);

    /* process concatenated streams */
    do {
        /* skip header, if any, save header type as trailer size */
        ret = getc(in);
        n = getc(in);
        val = ((unsigned)ret << 8) + (unsigned)n;   /* magic two bytes */
        if (ret == EOF) {
            /* nothing after the last stream, or empty file */
            ret = 0;
            break;
        }
        else if (head && n != EOF && val == 0x1f8b) {
            /* gzip header */
            fputs("!\n", out);
            if (NEXT(in) != 8) bail("unknown gzip compression method");
            ret = NEXT(in);
            if (ret & 0xe0) bail("reserved gzip flags set");
            num = NEXT(in);
            num += NEXT(in) << 8;
            num += NEXT(in) << 16;
            num += NEXT(in) << 24;
            if (info && num)
                fprintf(out, "time %lu\n", num);
            val = NEXT(in);
            if (info && val)
                fprintf(out, "xfl %u\n", val);
            val = NEXT(in);
            if (info && val != 3)
                fprintf(out, "os %u\n", val);
            if (ret & 4) {              /* extra field */
                val = NEXT(in);
                val += NEXT(in) << 8;
                if (val == 0)
                    fputs("extra '\n", out);
                else
                    do {
                        if (info)
                            putval(NEXT(in), "extra", &col, out);
                    } while (--val);
                if (info && col) { putc('\n', out); col = 0; }
            }
            if (ret & 8) {              /* file name */
                if (NEXT(in) == 0) {
                    if (info)
                        fputs("name '\n", out);
                }
                else
                    do {
                        if (info)
                            putval(n, "name", &col, out);
                    } while (NEXT(in) != 0);
                if (info && col) { putc('\n', out); col = 0; }
            }
            if (ret & 16) {             /* comment field */
                if (NEXT(in) == 0) {
                    if (info)
                        fputs("comment '\n", out);
                }
                else
                    do {
                        if (info)
                            putval(n, "comment", &col, out);
                    } while (NEXT(in) != 0);
                if (info && col) { putc('\n', out); col = 0; }
            }
            if (ret & 2) {              /* header crc */
                NEXT(in);
                NEXT(in);
                if (info)
                    fputs("hcrc\n", out);
            }
            trail = 8;
            fputs("gzip\n", out);
        }
        else if (head && n != EOF && val % 31 == 0 && (ret & 0xf) == 8 &&
                 (ret >> 4) < 8) {
            /* zlib header */
            fputs("!\n", out);
            if ((val & 0xe0) != 0x80)       /* compression level */
                fprintf(out, "level %d\n", (val >> 6) & 3);
            if (val & 0x20) {               /* preset dictionary */
                num = NEXT(in);
                num = (num << 8) + NEXT(in);
                num = (num << 8) + NEXT(in);
                num = (num << 8) + NEXT(in);
                fprintf(out, "dict %lu\n", num);
            }
            ret = (ret >> 4) + 8;
            win = 1U << ret;        /* window size */
            trail = 4;
            if (ret != 15)
                fprintf(out, "zlib %d\n", ret);
            else
                fputs("zlib\n", out);
        }
        else {
            /* raw deflate data, put non-header bytes back (assumes two ok) */
            ungetc(n, in);
            ret = ungetc(ret, in);  /* this should work, but ... */
            if (ret == EOF)         /* only one ungetc() guaranteed */
                bail("could not ungetc() (!)");
            trail = 0;
        }

        /* process compressed data to produce a defgen description */
        ret = infgen(in, out, win, data, tree, draw, stats);
        fflush(stdout);

        /* check return value and trailer size */
        if (ret > 0)
            fputs("infgen warning: incomplete deflate data\n", stderr);
        else if (ret < 0)
            fprintf(stderr, "infgen warning: invalid deflate data -- %s\n",
                    -ret > 0 && -ret <= 10 ? inferr[-1 - ret] : "unknown");
        else {
            n = 0;
            while (n < trail && getc(in) != EOF)
                n++;
            if (n < trail) {
                fprintf(stderr, "infgen warning: incomplete %s trailer\n",
                        trail == 4 ? "zlib" : "gzip");
                ret = 2;
            }
        }

        /* write defgen trailer (note: trailer was not validated) */
        if (ret == 0) {
            if (trail == 4)
                fputs("!\nadler\n", out);
            else if (trail == 8)
                fputs("!\ncrc\nlength\n", out);
        }
    } while (ret == 0);

    /* done */
    return ret;
}
