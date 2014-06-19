
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

#ifndef _AES_DEFS_
#define _AES_DEFS_

// 1. Standard types for AES cryptography source code

typedef unsigned char   u1byte; // an 8 bit unsigned character type
typedef unsigned short  u2byte; // a 16 bit unsigned integer type
typedef unsigned long   u4byte; // a 32 bit unsigned integer type

typedef signed char     s1byte; // an 8 bit signed character type
typedef signed short    s2byte; // a 16 bit signed integer type
typedef signed long     s4byte; // a 32 bit signed integer type

// 2. Standard interface for AES cryptographic routines

#define LITTLE_ENDIAN

enum dir_flag { enc = 1, dec = 2, both = 3 };

#ifdef  __cplusplus

#   define  STATIC

    class AES
    {
    protected:
        dir_flag    mode;
    public:
        AES(void) : mode(both) { };
        virtual char *name(void) = 0;
        virtual void set_key(const u1byte key[], const u4byte key_bits, const enum dir_flag f = both) = 0;
        virtual void encrypt(const u1byte in_blk[], u1byte out_blk[]) = 0;
        virtual void decrypt(const u1byte in_blk[], u1byte out_blk[]) = 0;
    };

#   define  AESREF AES&
#   define  IFREF   std::ifstream&
#   define  OFREF   std::ofstream&
#   define  IFILE   std::ifstream
#   define  OFILE   std::ofstream

#else

#   define  inline  __inline
#   define  STATIC  static
#   define  bool    int
#   define  false   0
#   define  true    1

    typedef char*   (*name_alg)(void);
    typedef void    (*key_alg)(const u1byte key[], const u4byte key_bits, const enum dir_flag f);
    typedef void    (*enc_alg)(const u1byte in_blk[], u1byte out_blk[]);
    typedef void    (*dec_alg)(const u1byte in_blk[], u1byte out_blk[]);

    typedef struct
    {   name_alg    name;
        key_alg     set_key;
        enc_alg     encrypt;
        dec_alg     decrypt;
    } alg_struct;

    extern enum dir_flag mode;  // in C the mode flag is declared in aes_aux.c

#   define AESREF   alg_struct
#   define  IFREF   FILE*
#   define  OFREF   FILE*
#   define  IFILE   FILE*
#   define  OFILE   FILE*

#endif

// 3. Basic macros for speeding up generic operations

// Circular rotate of 32 bit values

#ifdef _MSC_VER

#  include <stdlib.h>
#  pragma intrinsic(_lrotr,_lrotl)
#  define rotr(x,n) _lrotr(x,n)
#  define rotl(x,n) _lrotl(x,n)

#else

#define rotr(x,n)   (((x) >> ((int)((n) & 0x1f))) | ((x) << ((int)((32 - ((n) & 0x1f))))))
#define rotl(x,n)   (((x) << ((int)((n) & 0x1f))) | ((x) >> ((int)((32 - ((n) & 0x1f))))))

#endif

// Invert byte order in a 32 bit variable

#define bswap(x)    (rotl(x, 8) & 0x00ff00ff | rotr(x, 8) & 0xff00ff00)

// Put or get a 32 bit word (v) in machine order from a byte address in (x)

#ifdef  LITTLE_ENDIAN

#define u4byte_in(x)        (*(u4byte*)(x))
#define u4byte_out(x, v)    (*(u4byte*)(x) = (v))

#else

#define u4byte_in(x)        bswap(*(u4byte)(x))
#define u4byte_out(x, v)    (*(u4byte*)(x) = bswap(v))

#endif

#endif