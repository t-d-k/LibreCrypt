
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         DFC designed by a team at CNRS and France Telecom            */
/*                                                                      */
/* which is a candidate algorithm in the Advanced Encryption Standard   */
/* programme of the US National Institute of Standards and Technology.  */
/*                                                                      */
/* Copyright in this implementation is held by Dr B R Gladman but I     */
/* hereby give permission for its free direct or derivative use subject */
/* to acknowledgment of its origin and compliance with any conditions   */
/* that the originators of the algorithm place on its exploitation.     */
/*                                                                      */
/* Dr Brian Gladman 14th January 1999									*/
/*                                                                      */
/* My thanks go to Serge Vaudenay of the Ecole Normale Superieure       */
/* for providing test vectors. This implementation has also been        */
/* tested with an independent implementation by Dr Russell Bradford     */
/* (Department of Mathematical Sciences, University of Bath, Bath,      */
/* UK) and checks out.   My thanks go to Russell for his help in        */
/* comparing our implementations and finding bugs (and for help in      */
/* resolving 'endian' issues before test vectors became available).     */

/* Timing data for DFC (dfc.c)

Core timing without I/O endian conversion:

Algorithm: dfc (dfc.c)

128 bit key:
Key Setup:    5222 cycles
Encrypt:      1203 cycles =    21.3 mbits/sec
Decrypt:      1244 cycles =    20.6 mbits/sec
Mean:         1224 cycles =    20.9 mbits/sec

192 bit key:
Key Setup:    5203 cycles
Encrypt:      1288 cycles =    19.9 mbits/sec
Decrypt:      1235 cycles =    20.7 mbits/sec
Mean:         1262 cycles =    20.3 mbits/sec

256 bit key:
Key Setup:    5177 cycles
Encrypt:      1178 cycles =    21.7 mbits/sec
Decrypt:      1226 cycles =    20.9 mbits/sec
Mean:         1202 cycles =    21.3 mbits/sec

Full timing with I/O endian conversion:

128 bit key:
Key Setup:    5227 cycles
Encrypt:      1247 cycles =    20.5 mbits/sec
Decrypt:      1222 cycles =    20.9 mbits/sec
Mean:         1235 cycles =    20.7 mbits/sec

192 bit key:
Key Setup:    5252 cycles
Encrypt:      1215 cycles =    21.1 mbits/sec
Decrypt:      1265 cycles =    20.2 mbits/sec
Mean:         1240 cycles =    20.6 mbits/sec

256 bit key:
Key Setup:    5174 cycles
Encrypt:      1247 cycles =    20.5 mbits/sec
Decrypt:      1206 cycles =    21.2 mbits/sec
Mean:         1227 cycles =    20.9 mbits/sec

*/

/* The EES string is as follows (the abstract contains an error in 
   the last line of this sequence which changes KC and KD):

    0xb7e15162, 0x8aed2a6a, 0xbf715880, 0x9cf4f3c7, 
    0x62e7160f, 0x38b4da56, 0xa784d904, 0x5190cfef, 
    0x324e7738, 0x926cfbe5, 0xf4bf8d8d, 0x8c31d763,
    0xda06c80a, 0xbb1185eb, 0x4f7c7b57, 0x57f59584, 

    0x90cfd47d, 0x7c19bb42, 0x158d9554, 0xf7b46bce, 
    0xd55c4d79, 0xfd5f24d6, 0x613c31c3, 0x839a2ddf,
    0x8a9a276b, 0xcfbfa1c8, 0x77c56284, 0xdab79cd4, 
    0xc2b3293d, 0x20e9e5ea, 0xf02ac60a, 0xcc93ed87, 

    0x4422a52e, 0xcb238fee, 0xe5ab6add, 0x835fd1a0,
    0x753d0a8f, 0x78e537d2, 0xb95bb79d, 0x8dcaec64, 
    0x2c1e9f23, 0xb829b5c2, 0x780bf387, 0x37df8bb3, 
    0x00d01334, 0xa0d0bd86, 0x45cbfa73, 0xa6160ffe,

    0x393c48cb, 0xbbca060f, 0x0ff8ec6d, 0x31beb5cc, 
    0xeed7f2f0, 0xbb088017, 0x163bc60d, 0xf45a0ecb, 
    0x1bcd289b, 0x06cbbfea, 0x21ad08e1, 0x847f3f73,
    0x78d56ced, 0x94640d6e, 0xf0d3d37b, 0xe67008e1, 
    
    0x86d1bf27, 0x5b9b241d, 0xeb64749a, 0x47dfdfb9, 

    Where:

    EES = RT(0) | RT(1) | ... | RT(63) | KD | KC

    Note that the abstract describing DFC is written 
    in big endian notation with the most significant 
    digits of a sequence of digits placed at the low
    index positions in arrays. This format is used
    here and is only converted to machine format at
    the point that maths is done on any numbers in 
    the round function.
    
    The key input is thus treated as an array of 32 
    bit words numbered from 0..3, 0..5 or 0..7 
    depending on key length.  The first (leftmost) 
    bit of this key string as defined in the DFC 
    abstract is the most significant bit of word 0 
    and the rightmost bit of this string is the least 
    signicant bit of the highest numbered key word.

    The input and output blocks for the cipher are 
    also treated as arrays of 32 bit words numbered
    from 0..3.  The most significant bit of word 0 is
    the 1st (leftmost) bit of the 128 bit input string 
    and the least significant bit of word 3 is the 
    last (rightmost) bit.
    
    Note that the inputs, the output and the key are
    in Intel little endian format when BYTE_SWAP is 
    defined
*/

#define BYTE_SWAP

#ifdef CORE_TIME
#  undef BYTE_SWAP
#endif

#include "../std_defs.h"

static char *alg_name[] = { "dfc", "dfc.c", "dfc" };

char **cipher_name()
{
    return alg_name;
};

/* The following arrays are all stored in big endian    */
/* format with 32 bit words at lower array positions    */
/* being more significant in multi-word values          */

u4byte rt64[64] = 
{
    0xb7e15162, 0x8aed2a6a, 0xbf715880, 0x9cf4f3c7, 
    0x62e7160f, 0x38b4da56, 0xa784d904, 0x5190cfef, 
    0x324e7738, 0x926cfbe5, 0xf4bf8d8d, 0x8c31d763,
    0xda06c80a, 0xbb1185eb, 0x4f7c7b57, 0x57f59584, 

    0x90cfd47d, 0x7c19bb42, 0x158d9554, 0xf7b46bce, 
    0xd55c4d79, 0xfd5f24d6, 0x613c31c3, 0x839a2ddf,
    0x8a9a276b, 0xcfbfa1c8, 0x77c56284, 0xdab79cd4, 
    0xc2b3293d, 0x20e9e5ea, 0xf02ac60a, 0xcc93ed87, 

    0x4422a52e, 0xcb238fee, 0xe5ab6add, 0x835fd1a0,
    0x753d0a8f, 0x78e537d2, 0xb95bb79d, 0x8dcaec64, 
    0x2c1e9f23, 0xb829b5c2, 0x780bf387, 0x37df8bb3, 
    0x00d01334, 0xa0d0bd86, 0x45cbfa73, 0xa6160ffe,

    0x393c48cb, 0xbbca060f, 0x0ff8ec6d, 0x31beb5cc, 
    0xeed7f2f0, 0xbb088017, 0x163bc60d, 0xf45a0ecb, 
    0x1bcd289b, 0x06cbbfea, 0x21ad08e1, 0x847f3f73,
    0x78d56ced, 0x94640d6e, 0xf0d3d37b, 0xe67008e1, 
};

u4byte  kc = 0xeb64749a;

u4byte  kd2[2] = 
{
    0x86d1bf27, 0x5b9b241d  
};

u4byte  ka2[6] = 
{
    0xb7e15162, 0x8aed2a6a, 
    0xbf715880, 0x9cf4f3c7, 
    0x62e7160f, 0x38b4da56, 
};

u4byte  kb2[6] =
{
    0xa784d904, 0x5190cfef, 
    0x324e7738, 0x926cfbe5, 
    0xf4bf8d8d, 0x8c31d763,
};

u4byte  ks8[8] = 
{   0xda06c80a, 0xbb1185eb, 0x4f7c7b57, 0x57f59584, 
    0x90cfd47d, 0x7c19bb42, 0x158d9554, 0xf7b46bce,
};

u4byte l_key[32];

#define lo(x)   ((x) & 0x0000ffff)
#define hi(x)   ((x) >> 16)

#define mult_64(r,x,y)                                          \
    x0 = lo(x[1]); x1 = hi(x[1]); x2 = lo(x[0]); x3 = hi(x[0]); \
    y0 = lo(y[1]); y1 = hi(y[1]); y2 = lo(y[0]); y3 = hi(y[0]); \
    t0 = x0 * y0; r[0] = lo(t0); c = hi(t0);                    \
    t0 = x0 * y1; t1 = x1 * y0; c += lo(t0) + lo(t1);           \
    r[0] += (c << 16); c = hi(c) + hi(t0) + hi(t1);             \
    t0 = x0 * y2; t1 = x1 * y1; t2 = x2 * y0;                   \
    c += lo(t0) + lo(t1) + lo(t2); r[1] = lo(c);                \
    c = hi(c) + hi(t0) + hi(t1) + hi(t2);                       \
    t0 = x0 * y3; t1 = x1 * y2; t2 = x2 * y1; t3 = x3 * y0;     \
    c += lo(t0) + lo(t1) + lo(t2) + lo(t3); r[1] += (c << 16);  \
    c = hi(c) + hi(t0) + hi(t1) + hi(t2) + hi(t3);              \
    t0 = x1 * y3; t1 = x2 * y2; t2 = x3 * y1;                   \
    c += lo(t0) + lo(t1) + lo(t2); r[2] = lo(c);                \
    c = hi(c) + hi(t0) + hi(t1) + hi(t2);                       \
    t0 = x2 * y3; t1 = x3 * y2; c += lo(t0) + lo(t1);           \
    r[2] += (c << 16); c = hi(c) + hi(t0) + hi(t1);             \
    r[3] = c + x3 * y3

#define add_64(r,hi,lo)     \
    if((r[0] += lo) < lo)   \
        if(!++r[1])         \
            if(!++r[2])     \
                ++r[3];     \
    if((r[1] += hi) < hi)   \
        if(!++r[2])         \
            ++r[3]
    
#define mult_13(r)                  \
    c = 13 * lo((r)[0]);            \
    d = hi((r)[0]);                 \
    (r)[0] = lo(c);                 \
    c = hi(c) + 13 * d;             \
    (r)[0] += (c << 16);            \
    c = hi(c) + 13 * lo((r)[1]);    \
    d = hi((r)[1]);                 \
    (r)[1] = lo(c);                 \
    c = hi(c) + 13 * d;             \
    (r)[1] += (c << 16);            \
    (r)[2] = hi(c)

/* Where necessary this is where conversion from big endian to  */
/* little endian format is performed.  Since all the maths is   */
/* little endian care is needed when 64 bit blocks are being    */
/* used to get them in the right order by reversing the order   */
/* in which these are stored. This applies to the key array     */
/* which gives the two values A and B and to the constant KD.   */
/* Since the input and output blocks are big endian we also     */
/* have to invert the order of the 32 bit words in the 64 bit   */
/* blocks being processed.                                      */ 

void r_fun(u4byte outp[2], const u4byte inp[2], const u4byte key[4])
{   u4byte  acc[5], x0, x1, x2, x3, y0, y1, y2, y3, t0, t1, t2, t3, c, d;

    mult_64(acc, inp, key);  add_64(acc, key[2], key[3]);

    /* we need the value in the accumulator mod 2^64 + 13 so if */
    /* the accumulator value is hi * 2^64 + lo we need to find  */
    /* a k value such that r = hi * 2^64 + lo - k * (2^64 + 13) */
    /* is 0 <= r < 2^64 + 13.  We can see that k will be close  */
    /* to hi in value - it may equal hi but will not be greater */
    /* and we can let k = hi - e with e >= 0 so that r is given */
    /* by r = e * (2^64 + 13) + lo - 13 * hi. If we compute the */
    /* lo - 13 * hi value, the overflow into the top 64 bits of */
    /* the accumulator has to be 'zeroed' by the e * (2^64 + 13)*/
    /* term and this sets the e value (in fact such an overlow  */
    /* is only removed when the lower word is higher than 12).  */

    mult_13(acc + 2);   /* multiply top of accumulator by 13    */

    /* calculate lo - 13 * hi in acc[0] and acc[1] with any     */
    /* overflow into top 64 bits in c                           */

    d = acc[0]; acc[0] -= acc[2]; c = (acc[0] > d ? 1 : 0);

    d = acc[1]; acc[1] -= acc[3] + c;
    c = (acc[1] > d ? 1 : (acc[1] == d ? c : 0));

    c = 13 * (acc[4] + c);  /* overflow into top 64 bits of acc */

    if(((acc[0] += c) < c) && !(++acc[1]))
    {
        if(acc[0] > 12)

            acc[0] -= 13;
    }

    /* do the confusion permutation */

    d = acc[1] ^ kc; c = acc[0] ^ rt64[acc[1] >> 26];  
    
    c += kd2[0] + ((d += kd2[1]) < kd2[1] ? 1 : 0);

    outp[0] ^= c; outp[1] ^= d; 
};

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u4byte  i, lk[32], rk[4];

    for(i = 0; i < key_len / 32; ++i)

        lk[i] = io_swap(in_key[i]);

    /* pad the key with the KS array            */

    for(i = 0; i < 8 - key_len / 32; ++i)    /* K|KS */

        lk[i + key_len / 32] = ks8[i];

    /* do the reordering of the key parameters  */
    /* the OAP[1]|OBP[1]|OAP[2]... sequence is  */
    /* at lk[0]... and the other at lk[16]...   */
    
    lk[18] = lk[5]; lk[19] = lk[2]; /* EBP */ 
    lk[16] = lk[1]; lk[17] = lk[6]; /* EAP */
    lk[ 2] = lk[4]; lk[ 3] = lk[3]; /* OBP */
    lk[ 0] = lk[0]; lk[ 1] = lk[7]; /* OAP */

    /* create other elements using KA and KB    */

    for(i = 0; i < 6; i += 2)
    {
        lk[i + i +   4] = lk[ 0] ^ ka2[i];      /* OAP[i] ms */
        lk[i + i +   5] = lk[ 1] ^ ka2[i + 1];  /* OAP[i] ls */
        lk[i + i +   6] = lk[ 2] ^ kb2[i];      /* OBP[i] ms */
        lk[i + i +   7] = lk[ 3] ^ kb2[i + 1];  /* OBP[i] ls */
        lk[i + i +  20] = lk[16] ^ ka2[i];      /* EAP[i] ms */
        lk[i + i +  21] = lk[17] ^ ka2[i + 1];  /* EAP[i] ls */
        lk[i + i +  22] = lk[18] ^ kb2[i];      /* EBP[i] ms */
        lk[i + i +  23] = lk[19] ^ kb2[i + 1];  /* EBP[i] ls */
    }

    rk[0] = rk[1] = rk[2] = rk[3] = 0;

    /* do the 4 round key mixing encryption     */

    for(i = 0; i < 32; i += 8)
    {
        r_fun(rk, rk + 2, lk);      /*  R2|R1   */
        r_fun(rk + 2, rk, lk +  4); /*  R2|R3   */
        r_fun(rk, rk + 2, lk +  8); /*  R4|R3   */
        r_fun(rk + 2, rk, lk + 12); /*  R4|R5   */

        /* keep key in big endian format with   */
        /* the most significant 32 bit words    */
        /* first (lowest) in the key schedule   */
        /* - note that the upper and lower 64   */
        /* bit blocks are in inverse order at   */
        /* this point in the loop               */

        l_key[i + 0] = rk[2]; l_key[i + 1] = rk[3]; 
        l_key[i + 2] = rk[0]; l_key[i + 3] = rk[1]; 

        r_fun(rk + 2, rk, lk + 16); /*  R1|R2   */
        r_fun(rk, rk + 2, lk + 20); /*  R3|R2   */
        r_fun(rk + 2, rk, lk + 24); /*  R3|R4   */  
        r_fun(rk, rk + 2, lk + 28); /*  R5|R4   */

        l_key[i + 4] = rk[0]; l_key[i + 5] = rk[1]; 
        l_key[i + 6] = rk[2]; l_key[i + 7] = rk[3];
    }
    
    return l_key;
};

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4];

    /* the input/output format is big endian -  */
    /* any reversals needed are performed when  */
    /* maths is done in the round function      */

    blk[0] = io_swap(in_blk[0]); blk[1] = io_swap(in_blk[1]);
    blk[2] = io_swap(in_blk[2]); blk[3] = io_swap(in_blk[3]);

    r_fun(blk, blk + 2, l_key +  0); /*  R2|R1  */ 
    r_fun(blk + 2, blk, l_key +  4); /*  R2|R3  */
    r_fun(blk, blk + 2, l_key +  8); /*  R4|R3  */
    r_fun(blk + 2, blk, l_key + 12); /*  R4|R5  */
    r_fun(blk, blk + 2, l_key + 16); /*  R6|R5  */
    r_fun(blk + 2, blk, l_key + 20); /*  R6|R7  */
    r_fun(blk, blk + 2, l_key + 24); /*  R8|R7  */
    r_fun(blk + 2, blk, l_key + 28); /*  R8|R9  */

    /* swap order to obtain the result R9|R8    */

    out_blk[0] = io_swap(blk[2]); out_blk[1] = io_swap(blk[3]);
    out_blk[2] = io_swap(blk[0]); out_blk[3] = io_swap(blk[1]);
};

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4];

    /* the input/output format is big endian -  */
    /* any reversals needed are performed when  */
    /* maths is done in the round function      */

    blk[0] = io_swap(in_blk[0]); blk[1] = io_swap(in_blk[1]);
    blk[2] = io_swap(in_blk[2]); blk[3] = io_swap(in_blk[3]);

    r_fun(blk, blk + 2, l_key + 28); /*  R7|R8  */
    r_fun(blk + 2, blk, l_key + 24); /*  R7|R6  */
    r_fun(blk, blk + 2, l_key + 20); /*  R5|R6  */
    r_fun(blk + 2, blk, l_key + 16); /*  R5|R4  */
    r_fun(blk, blk + 2, l_key + 12); /*  R3|R4  */
    r_fun(blk + 2, blk, l_key +  8); /*  R3|R2  */
    r_fun(blk, blk + 2, l_key +  4); /*  R1|R2  */
    r_fun(blk + 2, blk, l_key     ); /*  R1|R0  */ 

    /* swap order to obtain the result R1|R0    */

    out_blk[0] = io_swap(blk[2]); out_blk[1] = io_swap(blk[3]);
    out_blk[2] = io_swap(blk[0]); out_blk[3] = io_swap(blk[1]);   
};
