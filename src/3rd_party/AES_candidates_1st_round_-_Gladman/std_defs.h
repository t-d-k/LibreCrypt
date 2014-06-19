
/* 1. Standard types for AES cryptography source code               */

typedef unsigned char   u1byte; /* an 8 bit unsigned character type */
typedef unsigned short  u2byte; /* a 16 bit unsigned integer type   */
typedef unsigned long   u4byte; /* a 32 bit unsigned integer type   */

typedef signed char     s1byte; /* an 8 bit signed character type   */
typedef signed short    s2byte; /* a 16 bit signed integer type     */
typedef signed long     s4byte; /* a 32 bit signed integer type     */

/* 2. Standard interface for AES cryptographic routines             */

/* These are all based on 32 bit unsigned values and will therefore */
/* require endian conversions for big-endian architectures          */

#ifdef  __cplusplus
    extern "C"
    {
#endif

    char **cipher_name(void);
    u4byte *set_key(void *context, const u4byte in_key[], const u4byte key_len);
    void encrypt(void *context, const u4byte in_blk[4], u4byte out_blk[4]);
    void decrypt(void *context, const u4byte in_blk[4], u4byte out_blk[4]);

#ifdef  __cplusplus
    };
#endif

/* 3. Basic macros for speeding up generic operations               */

/* Circular rotate of 32 bit values                                 */

#ifdef _MSC_VER

#  include <stdlib.h>
#  pragma intrinsic(_lrotr,_lrotl)
#  define rotr(x,n) _lrotr(x,n)
#  define rotl(x,n) _lrotl(x,n)

#else

#define rotr(x,n)   (((x) >> ((int)(n))) | ((x) << (32 - (int)(n))))
#define rotl(x,n)   (((x) << ((int)(n))) | ((x) >> (32 - (int)(n))))

#endif

/* Invert byte order in a 32 bit variable                           */

#define bswap(x)    (rotl(x, 8) & 0x00ff00ff | rotr(x, 8) & 0xff00ff00)

/* Extract byte from a 32 bit quantity (little endian notation)     */ 

#define byte(x,n)   ((u1byte)((x) >> (8 * n)))

/* For inverting byte order in input/output 32 bit words if needed  */

#ifdef  BLOCK_SWAP
#define BYTE_SWAP
#define WORD_SWAP
#endif

#ifdef  BYTE_SWAP
#define io_swap(x)  bswap(x)
#else
#define io_swap(x)  (x)
#endif

/* For inverting the byte order of input/output blocks if needed    */

#ifdef  WORD_SWAP

#define get_block(x)                            \
    ((u4byte*)(x))[0] = io_swap(in_blk[3]);     \
    ((u4byte*)(x))[1] = io_swap(in_blk[2]);     \
    ((u4byte*)(x))[2] = io_swap(in_blk[1]);     \
    ((u4byte*)(x))[3] = io_swap(in_blk[0])

#define put_block(x)                            \
    out_blk[3] = io_swap(((u4byte*)(x))[0]);    \
    out_blk[2] = io_swap(((u4byte*)(x))[1]);    \
    out_blk[1] = io_swap(((u4byte*)(x))[2]);    \
    out_blk[0] = io_swap(((u4byte*)(x))[3])

#define get_key(x,len)                          \
    ((u4byte*)(x))[4] = ((u4byte*)(x))[5] =     \
    ((u4byte*)(x))[6] = ((u4byte*)(x))[7] = 0;  \
    switch((((len) + 63) / 64)) {               \
    case 2:                                     \
    ((u4byte*)(x))[0] = io_swap(in_key[3]);     \
    ((u4byte*)(x))[1] = io_swap(in_key[2]);     \
    ((u4byte*)(x))[2] = io_swap(in_key[1]);     \
    ((u4byte*)(x))[3] = io_swap(in_key[0]);     \
    break;                                      \
    case 3:                                     \
    ((u4byte*)(x))[0] = io_swap(in_key[5]);     \
    ((u4byte*)(x))[1] = io_swap(in_key[4]);     \
    ((u4byte*)(x))[2] = io_swap(in_key[3]);     \
    ((u4byte*)(x))[3] = io_swap(in_key[2]);     \
    ((u4byte*)(x))[4] = io_swap(in_key[1]);     \
    ((u4byte*)(x))[5] = io_swap(in_key[0]);     \
    break;                                      \
    case 4:                                     \
    ((u4byte*)(x))[0] = io_swap(in_key[7]);     \
    ((u4byte*)(x))[1] = io_swap(in_key[6]);     \
    ((u4byte*)(x))[2] = io_swap(in_key[5]);     \
    ((u4byte*)(x))[3] = io_swap(in_key[4]);     \
    ((u4byte*)(x))[4] = io_swap(in_key[3]);     \
    ((u4byte*)(x))[5] = io_swap(in_key[2]);     \
    ((u4byte*)(x))[6] = io_swap(in_key[1]);     \
    ((u4byte*)(x))[7] = io_swap(in_key[0]);     \
    }

#else

#define get_block(x)                            \
    ((u4byte*)(x))[0] = io_swap(in_blk[0]);     \
    ((u4byte*)(x))[1] = io_swap(in_blk[1]);     \
    ((u4byte*)(x))[2] = io_swap(in_blk[2]);     \
    ((u4byte*)(x))[3] = io_swap(in_blk[3])

#define put_block(x)                            \
    out_blk[0] = io_swap(((u4byte*)(x))[0]);    \
    out_blk[1] = io_swap(((u4byte*)(x))[1]);    \
    out_blk[2] = io_swap(((u4byte*)(x))[2]);    \
    out_blk[3] = io_swap(((u4byte*)(x))[3])

#define get_key(x,len)                          \
    ((u4byte*)(x))[4] = ((u4byte*)(x))[5] =     \
    ((u4byte*)(x))[6] = ((u4byte*)(x))[7] = 0;  \
    switch((((len) + 63) / 64)) {               \
    case 4:                                     \
    ((u4byte*)(x))[6] = io_swap(in_key[6]);     \
    ((u4byte*)(x))[7] = io_swap(in_key[7]);     \
    case 3:                                     \
    ((u4byte*)(x))[4] = io_swap(in_key[4]);     \
    ((u4byte*)(x))[5] = io_swap(in_key[5]);     \
    case 2:                                     \
    ((u4byte*)(x))[0] = io_swap(in_key[0]);     \
    ((u4byte*)(x))[1] = io_swap(in_key[1]);     \
    ((u4byte*)(x))[2] = io_swap(in_key[2]);     \
    ((u4byte*)(x))[3] = io_swap(in_key[3]);     \
    }

#endif
