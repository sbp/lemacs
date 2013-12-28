#ifndef _EMACS_ISO_WIDE_H
#define _EMACS_ISO_WIDE_H

/* The following macros are designed for SunOS 5.0 wide characters,
   in which the single byte ISO Latin-1 character 1xxxxxxx are represented

		00110000 00000000 00000000 0xxxxxxx

   For wide character systems which maintain the numeric value of all
   single-byte characters, IN_TABLE_DOMAIN can simply be defined

		(0 <= (c) && (c) <= 0xff)

   and no funky ISO_WIDE_TO_BYTE conversions are needed. */

/* Can't use isascii() because we want wide char argument */
#define IS_ASCII(c)     (0 <= (c) && (c) <= 0x7f)

#define IS_ISO_WIDE(c)  (0x30000000 <= (c) && (c) <= 0x3000007f)
#define IS_ISO_BYTE(c)  (0x80 <= (c) && (c) <= 0xff)

#define IN_TABLE_DOMAIN(c)  (IS_ASCII (c) || IS_ISO_WIDE (c))

#define ISO_WIDE_TO_BYTE(c)  ((c) & 0x0000007f | 0x80)
#define ISO_BYTE_TO_WIDE(c)  ((c) & 0x7f | 0x30000000)

#define WIDE_TO_BYTE(c)  (IS_ISO_WIDE (c) ? ISO_WIDE_TO_BYTE (c) : (c))
#define BYTE_TO_WIDE(c)  (IS_ISO_BYTE (c) ? ISO_BYTE_TO_WIDE (c) : (c))

#endif /* _EMACS_ISO_WIDE_H */
