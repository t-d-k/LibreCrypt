
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         HPC-128 by Richard Schroeppel                                */
/*                                                                      */
/* which is a candidate algorithm in the Advanced Encryption Standard   */
/* programme of the US National Institute of Standards and Technology.  */
/*                                                                      */
/* Copyright in this implementation is held by Dr B R Gladman but I     */
/* hereby give permission for its free direct or derivative use subject */
/* to acknowledgment of its origin and compliance with any conditions   */
/* that the originators of the algorithm place on its exploitation.     */
/*                                                                      */
/* Dr Brian Gladman                               14th January 1999     */

/* Timing data for HPC (hpc.c)

Core timing without I/O endian conversion:

128 bit key:
Key Setup:  120749 cycles
Encrypt:      1429 cycles =    17.9 mbits/sec
Decrypt:      1599 cycles =    16.0 mbits/sec
Mean:         1514 cycles =    16.9 mbits/sec

192 bit key:
Key Setup:  120754 cycles
Encrypt:      1477 cycles =    17.3 mbits/sec
Decrypt:      1599 cycles =    16.0 mbits/sec
Mean:         1538 cycles =    16.6 mbits/sec

256 bit key:
Key Setup:  120731 cycles
Encrypt:      1462 cycles =    17.5 mbits/sec
Decrypt:      1590 cycles =    16.1 mbits/sec
Mean:         1526 cycles =    16.8 mbits/sec

Full timing with I/O endian conversion:

128 bit key:
Key Setup:  118606 cycles
Encrypt:      1479 cycles =    17.3 mbits/sec
Decrypt:      1599 cycles =    16.0 mbits/sec
Mean:         1539 cycles =    16.6 mbits/sec

192 bit key:
Key Setup:  118538 cycles
Encrypt:      1480 cycles =    17.3 mbits/sec
Decrypt:      1583 cycles =    16.2 mbits/sec
Mean:         1532 cycles =    16.7 mbits/sec

256 bit key:
Key Setup:  118532 cycles
Encrypt:      1465 cycles =    17.5 mbits/sec
Decrypt:      1599 cycles =    16.0 mbits/sec
Mean:         1532 cycles =    16.7 mbits/sec

*/

#define BYTE_SWAP

#ifdef CORE_TIME
#  undef BYTE_SWAP
#endif

#include "../std_defs.h"

static char *alg_name[] = { "hpc", "hpc.c", "hpc" };

char **cipher_name()
{
    return alg_name;
}

typedef u4byte  u8byte[2];

u8byte  l_key[286];
u8byte  spice[8] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

u8byte  p119 = { 0xa23249d6, 0x2b992ddf };
u8byte  e19  = { 0xc0b36173, 0x25b946eb };
u8byte  r220 = { 0xe9e17158, 0xc442f56b };

#define xor_eq(x,y)     (x)[0] ^= (y)[0]; (x)[1] ^= (y)[1]
#define and_eq(x,y)     (x)[0] &= (y)[0]; (x)[1] &= (y)[1]
#define  or_eq(x,y)     (x)[0] |= (y)[0]; (x)[1] |= (y)[1]

#define add_eq(x,y)     (x)[1] += (y)[1] + (((x)[0] += (y)[0]) < (y)[0] ? 1 : 0)
#define sub_eq(x,y)     xs = (x)[0]; (x)[1] -= (y)[1] + (((x)[0] -= (y)[0]) > xs ? 1 : 0)   

#define lsh_eq(x,n)                                         \
    if((n) > 31)                                            \
    {   (x)[1] = (x)[0] << ((n) & 31); (x)[0] = 0;          \
    }                                                       \
    else if((n) > 0)                                        \
    {   (x)[1] = ((x)[1] << (n)) | ((x)[0] >> (-(n) & 31)); \
        (x)[0] = (x)[0] << (n);                             \
    }

#define rsh_eq(x,n)                                         \
    if((n) > 31)                                            \
    {   (x)[0] = (x)[1] >> ((n) & 31); (x)[1] = 0;          \
    }                                                       \
    else if((n) > 0)                                        \
    {   (x)[0] = ((x)[0] >> (n)) | ((x)[1] << (-(n) & 31)); \
        (x)[1] = (x)[1] >> (n);                             \
    }

#define lo(x)   ((x) & 0x0000ffff)
#define hi(x)   ((x) >> 16)

void mult_64(u8byte r, const u8byte x, const u8byte y)
{   u4byte  x0, x1, x2, x3, y0, y1, y2, y3, t0, t1, t2, t3, c;

    x0 = lo(x[0]); x1 = hi(x[0]); x2 = lo(x[1]); x3 = hi(x[1]);
    y0 = lo(y[0]); y1 = hi(y[0]); y2 = lo(y[1]); y3 = hi(y[1]);

    t0 = x0 * y0; r[0] = lo(t0); c = hi(t0);

    t0 = x0 * y1; t1 = x1 * y0; c += lo(t0) + lo(t1);
    r[0] += (c << 16); c = hi(c) + hi(t0) + hi(t1);

    t0 = x0 * y2; t1 = x1 * y1; t2 = x2 * y0;
    c += lo(t0) + lo(t1) + lo(t2); r[1] = lo(c);
    c = hi(c) + hi(t0) + hi(t1) + hi(t2);

    t0 = x0 * y3; t1 = x1 * y2; t2 = x2 * y1; t3 = x3 * y0;
    c += lo(t0) + lo(t1) + lo(t2) + lo(t3); r[1] += (c << 16); 
/*
    c = hi(c) + hi(t0) + hi(t1) + hi(t2) + hi(t3);

    t0 = x1 * y3; t1 = x2 * y2; t2 = x3 * y1;
    c += lo(t0) + lo(t1) + lo(t2); r[1][0] = lo(c);
    c = hi(c) + hi(t0) + hi(t1) + hi(t2);

    t0 = x2 * y3; t1 = x3 * y2; c += lo(t0) + lo(t1);
    r[1][0] += (c << 16); c = hi(c) + hi(t0) + hi(t1);

    r[1][1] = c + x3 * y3;
*/
};

u8byte  l_key[286]; /* storage for the key schedule         */

/* initialise the key schedule from the user supplied key   */

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u8byte  s[8], t;
    u4byte  i, j, xs;

    l_key[0][0] = p119[0] + 3; l_key[0][1] = p119[1];
    t[0] = key_len; t[1] = 0; mult_64(l_key[1], e19, t);
    l_key[2][0] = (r220[0] << 3) | (r220[1] >> 29); 
    l_key[2][1] = (r220[1] << 3) | (r220[0] >> 29); 

    for(i = 3; i < 256; ++i)
    {
        t[0] = l_key[i - 3][0]; t[1] = l_key[i - 3][1]; lsh_eq(t, 41);
        l_key[i][0] = l_key[i - 3][0]; l_key[i][1] = l_key[i - 3][1]; 
        rsh_eq(l_key[i], 23); 
        or_eq(l_key[i], t); 
        xor_eq(l_key[i], l_key[i - 2]); 
        add_eq(l_key[i], l_key[i - 1]);
    }
    
    l_key[0][1] ^= io_swap(in_key[0]); l_key[0][0] ^= io_swap(in_key[1]);
    l_key[1][1] ^= io_swap(in_key[2]); l_key[1][0] ^= io_swap(in_key[3]);

    if(key_len > 128)
    {
        l_key[2][1] ^= io_swap(in_key[4]); l_key[2][0] ^= io_swap(in_key[5]);
    }

    if(key_len > 192)
    {
        l_key[3][1] ^= io_swap(in_key[6]); l_key[3][0] ^= io_swap(in_key[7]);
    }

    for(i = 0; i < 8; ++i)
    {
        s[i][0] = l_key[248 + i][0]; s[i][1] = l_key[248 + i][1];
    }
            
    for(j = 0; j < 3; ++j)
        for(i = 0; i < 256; ++i)
        {
            t[0] = l_key[i][0]; t[1] = l_key[i][1]; xor_eq(t, l_key[(i + 83) & 255]); 
            add_eq(t, l_key[s[0][0] & 255]);
            xor_eq(s[0], t); add_eq(s[1], s[0]); xor_eq(s[3], s[2]); 
            sub_eq(s[5], s[4]); xor_eq(s[7], s[6]);
            t[0] = s[0][0]; t[1] = s[0][1]; rsh_eq(t, 13); add_eq(s[3], t);
            t[0] = s[1][0]; t[1] = s[1][1]; lsh_eq(t, 11); xor_eq(s[4], t);
            t[0] = s[3][0]; t[1] = s[3][1]; 
            lsh_eq(t, s[1][0] & 31); 
            xor_eq(s[5], t);
            t[0] = s[2][0]; t[1] = s[2][1]; rsh_eq(t, 17); add_eq(s[6], t);
            t[0] = s[3][0]; t[1] = s[3][1]; add_eq(t, s[4]); or_eq(s[7], t);
            sub_eq(s[2], s[5]);
            t[0] = s[6][0] ^ i; t[1] = s[6][1]; sub_eq(s[0], t);
            t[0] = s[5][0]; t[1] = s[5][1]; add_eq(t, p119); xor_eq(s[1], t);
            t[0] = s[7][0]; t[1] = s[7][1]; rsh_eq(t, j); add_eq(s[2], t);
            xor_eq(s[2], s[1]); sub_eq(s[4], s[3]); 
            xor_eq(s[6], s[5]); add_eq(s[0], s[7]);
            l_key[i][0] = s[2][0]; l_key[i][1] = s[2][1]; add_eq(l_key[i], s[6]);
        }

    for(i = 0; i < 30; ++i)
    {
        l_key[256 + i][0] = l_key[i][0]; l_key[256 + i][1] = l_key[i][1];
    }
        
    return (u4byte*)l_key;
};

/* encrypt a block of text  */

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u8byte  s0, s1, k, kk, t;
    u4byte  tt, xs;
    s4byte  i;

    s0[1] = io_swap(in_blk[0]); s0[0] = io_swap(in_blk[1]);
    s1[1] = io_swap(in_blk[2]); s1[0] = io_swap(in_blk[3]);

    add_eq(s0, l_key[128]); add_eq(s1, l_key[129]);

    for(i = 0; i < 8; ++i)
    {
        tt = s0[0] & 255; k[0] = l_key[tt][0]; k[1] = l_key[tt][1]; 
        add_eq(s1, k);  lsh_eq(k, 8); xor_eq(s0, k); xor_eq(s1, s0);
        t[0] = s1[0]; t[1] = s1[1]; rsh_eq(t, 11); sub_eq(s0, t);
        t[0] = s1[0]; t[1] = s1[1]; lsh_eq(t, 2); xor_eq(s0, t);
        sub_eq(s0, spice[i ^ 4]);
        t[0] = s0[0]; t[1] = s0[1]; lsh_eq(t, 32);  
        kk[0] = p119[0] + 128; kk[1] = p119[1]; xor_eq(t, kk); add_eq(s0, t);
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 17); xor_eq(s0, t);
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 34); xor_eq(s0, t);       
        t[0] = spice[i][0]; t[1] = spice[i][1]; xor_eq(s0, t);
        lsh_eq(t, 5); add_eq(s0, t); 
        t[0] = spice[i][0]; t[1] = spice[i][1]; rsh_eq(t, 4);
        add_eq(s1, t); xor_eq(s0, t);
        t[0] = s0[0]; t[1] = s0[1]; lsh_eq(t, 22 + (s0[0] & 31)); add_eq(s0, t);
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 23); xor_eq(s0, t);
        sub_eq(s0, spice[i ^ 7]);
        tt = s0[0] & 255; k[0] = l_key[tt][0]; k[1] = l_key[tt][1]; 
        tt += 3 * i + 1; kk[0] = l_key[tt][0]; kk[1] = l_key[tt][1]; 
        xor_eq(s1, k); t[0] = kk[0]; t[1] = kk[1]; lsh_eq(t, 8); 
        xor_eq(s0, t); xor_eq(kk, k); 
        t[0] = kk[0]; t[1] = kk[1]; rsh_eq(t, 5); add_eq(s1, t);
        t[0] = kk[0]; t[1] = kk[1]; lsh_eq(t, 12); sub_eq(s0, t);
        kk[0] &= ~255; xor_eq(s0, kk);  add_eq(s1, s0);
        t[0] = s1[0]; t[1] = s1[1]; lsh_eq(t, 3); add_eq(s0, t);
        xor_eq(s0, spice[i ^ 2]); add_eq(s0, l_key[144 + i]);
        t[0] = s0[0]; t[1] = s0[1]; lsh_eq(t, 22); add_eq(s0, t);
        t[0] = s1[0]; t[1] = s1[1]; rsh_eq(t, 4); xor_eq(s0, t);
        add_eq(s0, spice[i ^ 1]);
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 33 + i); xor_eq(s0, t);
    }

    add_eq(s0, l_key[136]); add_eq(s1, l_key[137]);

    out_blk[0] = io_swap(s0[1]); out_blk[1] = io_swap(s0[0]);
    out_blk[2] = io_swap(s1[1]); out_blk[3] = io_swap(s1[0]);
};

/* decrypt a block of text  */

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u8byte  s0, s1, k, kk, t;
    u4byte  tt, xs;
    s4byte  i;

    s0[1] = io_swap(in_blk[0]); s0[0] = io_swap(in_blk[1]);
    s1[1] = io_swap(in_blk[2]); s1[0] = io_swap(in_blk[3]);

    sub_eq(s0, l_key[136]); sub_eq(s1, l_key[137]);

    for(i = 7; i >= 0; --i)
    {
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 33 + i); xor_eq(s0, t);
        sub_eq(s0, spice[i ^ 1]);
        t[0] = s1[0]; t[1] = s1[1]; rsh_eq(t, 4); xor_eq(s0, t);
        k[0] = s0[0]; k[1] = s0[1]; lsh_eq(k, 22);  
        t[0] = s0[0]; t[1] = s0[1]; sub_eq(t, k); 
        lsh_eq(t, 22); sub_eq(s0, t); sub_eq(s0, l_key[144 + i]);
        xor_eq(s0, spice[i ^ 2]); t[0] = s1[0]; t[1] = s1[1]; lsh_eq(t, 3); 
        sub_eq(s0, t); sub_eq(s1, s0); 
        tt = s0[0] & 255; k[0] = l_key[tt][0]; k[1] = l_key[tt][1];
        tt += 3 * i + 1; kk[0] = l_key[tt][0]; kk[1] = l_key[tt][1]; xor_eq(kk, k); 
        t[0] = kk[0] & ~255; t[1] = kk[1]; xor_eq(s0, t);
        t[0] = kk[0]; t[1] = kk[1]; lsh_eq(t, 12); add_eq(s0, t);
        t[0] = kk[0]; t[1] = kk[1]; rsh_eq(t, 5); sub_eq(s1, t);
        kk[0] = l_key[tt][0]; kk[1] = l_key[tt][1]; lsh_eq(kk, 8); 
        xor_eq(s0, kk); xor_eq(s1, k); add_eq(s0, spice[i ^ 7]); 
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 23); xor_eq(s0, t);
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 46); xor_eq(s0, t);
        tt = 22 + (s0[0] & 31); t[0] = s0[0]; t[1] = s0[1]; lsh_eq(t, tt);
        kk[0] = s0[0]; kk[1] = s0[1]; sub_eq(kk, t); lsh_eq(kk, tt); sub_eq(s0, kk);
        t[0] = kk[0] = spice[i][0]; t[1] = kk[1] = spice[i][1]; rsh_eq(kk, 4);
        xor_eq(s0, kk); sub_eq(s1, kk); k[0] = t[0]; k[1] = t[1]; lsh_eq(k, 5); 
        sub_eq(s0, k); xor_eq(s0, t); 
        t[0] = s0[0]; t[1] = s0[1]; rsh_eq(t, 17); xor_eq(s0, t);
        t[0] = p119[0] + 128; t[1] = p119[1]; k[0] = s0[0]; k[1] = s0[1];
        sub_eq(k, t); lsh_eq(k, 32); xor_eq(t, k); sub_eq(s0, t);
        add_eq(s0, spice[i ^ 4]); t[0] = s1[0]; t[1] = s1[1]; lsh_eq(t, 2); 
        xor_eq(s0, t); t[0] = s1[0]; t[1] = s1[1]; rsh_eq(t, 11); add_eq(s0, t);
        xor_eq(s1, s0); tt = s0[0] & 255; k[0] = l_key[tt][0]; k[1] = l_key[tt][1];
        t[0] = k[0]; t[1] = k[1]; lsh_eq(t, 8); xor_eq(s0, t); sub_eq(s1, k);
    }

    sub_eq(s0, l_key[128]); sub_eq(s1, l_key[129]);

    out_blk[0] = io_swap(s0[1]); out_blk[1] = io_swap(s0[0]);
    out_blk[2] = io_swap(s1[1]); out_blk[3] = io_swap(s1[0]);
};
