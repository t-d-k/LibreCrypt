
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         E2 by Nippon Telegraph and Telephone (NTT) Japan             */
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
/* In accordance with the wishes of NTT this implementation is made     */
/* available for academic and study purposes only.   I gratefully       */
/* acknowledge the contributions made by Kazumaro Aoki of NTT Japan     */
/* for ways to increase the speed of this implementation.               */

/* Timing data for E2 (e28.c)

Core timing without I/O endian conversion:

128 bit key:
Key Setup:    9473 cycles
Encrypt:       687 cycles =    37.3 mbits/sec
Decrypt:       691 cycles =    37.0 mbits/sec
Mean:          689 cycles =    37.2 mbits/sec

192 bit key:
Key Setup:    9540 cycles
Encrypt:       696 cycles =    36.8 mbits/sec
Decrypt:       693 cycles =    36.9 mbits/sec
Mean:          695 cycles =    36.9 mbits/sec

256 bit key:
Key Setup:    9913 cycles
Encrypt:       691 cycles =    37.0 mbits/sec
Decrypt:       706 cycles =    36.3 mbits/sec
Mean:          699 cycles =    36.6 mbits/sec

Full timing with I/O endian conversion:

128 bit key:
Key Setup:    9598 cycles
Encrypt:       730 cycles =    35.1 mbits/sec
Decrypt:       723 cycles =    35.4 mbits/sec
Mean:          727 cycles =    35.2 mbits/sec

192 bit key:
Key Setup:    9393 cycles
Encrypt:       730 cycles =    35.1 mbits/sec
Decrypt:       720 cycles =    35.6 mbits/sec
Mean:          725 cycles =    35.3 mbits/sec

256 bit key:
Key Setup:    9720 cycles
Encrypt:       727 cycles =    35.2 mbits/sec
Decrypt:       721 cycles =    35.5 mbits/sec
Mean:          724 cycles =    35.4 mbits/sec

*/

#ifndef CORE_TIME
#define BYTE_SWAP
#endif

#include "../std_defs.h"

static char *alg_name[] = { "e2", "e2.c", "e2" };

char **cipher_name()
{
    return alg_name;
};

u1byte  s_box[] =
{
 0xe1, 0x42, 0x3e, 0x81, 0x4e, 0x17, 0x9e, 0xfd, 0xb4, 0x3f, 0x2c, 0xda,
 0x31, 0x1e, 0xe0, 0x41, 0xcc, 0xf3, 0x82, 0x7d, 0x7c, 0x12, 0x8e, 0xbb,
 0xe4, 0x58, 0x15, 0xd5, 0x6f, 0xe9, 0x4c, 0x4b, 0x35, 0x7b, 0x5a, 0x9a,
 0x90, 0x45, 0xbc, 0xf8, 0x79, 0xd6, 0x1b, 0x88, 0x02, 0xab, 0xcf, 0x64,
 0x09, 0x0c, 0xf0, 0x01, 0xa4, 0xb0, 0xf6, 0x93, 0x43, 0x63, 0x86, 0xdc,
 0x11, 0xa5, 0x83, 0x8b, 0xc9, 0xd0, 0x19, 0x95, 0x6a, 0xa1, 0x5c, 0x24,
 0x6e, 0x50, 0x21, 0x80, 0x2f, 0xe7, 0x53, 0x0f, 0x91, 0x22, 0x04, 0xed,
 0xa6, 0x48, 0x49, 0x67, 0xec, 0xf7, 0xc0, 0x39, 0xce, 0xf2, 0x2d, 0xbe,
 0x5d, 0x1c, 0xe3, 0x87, 0x07, 0x0d, 0x7a, 0xf4, 0xfb, 0x32, 0xf5, 0x8c,
 0xdb, 0x8f, 0x25, 0x96, 0xa8, 0xea, 0xcd, 0x33, 0x65, 0x54, 0x06, 0x8d,
 0x89, 0x0a, 0x5e, 0xd9, 0x16, 0x0e, 0x71, 0x6c, 0x0b, 0xff, 0x60, 0xd2,
 0x2e, 0xd3, 0xc8, 0x55, 0xc2, 0x23, 0xb7, 0x74, 0xe2, 0x9b, 0xdf, 0x77,
 0x2b, 0xb9, 0x3c, 0x62, 0x13, 0xe5, 0x94, 0x34, 0xb1, 0x27, 0x84, 0x9f,
 0xd7, 0x51, 0x00, 0x61, 0xad, 0x85, 0x73, 0x03, 0x08, 0x40, 0xef, 0x68,
 0xfe, 0x97, 0x1f, 0xde, 0xaf, 0x66, 0xe8, 0xb8, 0xae, 0xbd, 0xb3, 0xeb,
 0xc6, 0x6b, 0x47, 0xa9, 0xd8, 0xa7, 0x72, 0xee, 0x1d, 0x7e, 0xaa, 0xb6,
 0x75, 0xcb, 0xd4, 0x30, 0x69, 0x20, 0x7f, 0x37, 0x5b, 0x9d, 0x78, 0xa3,
 0xf1, 0x76, 0xfa, 0x05, 0x3d, 0x3a, 0x44, 0x57, 0x3b, 0xca, 0xc7, 0x8a,
 0x18, 0x46, 0x9c, 0xbf, 0xba, 0x38, 0x56, 0x1a, 0x92, 0x4d, 0x26, 0x29,
 0xa2, 0x98, 0x10, 0x99, 0x70, 0xa0, 0xc5, 0x28, 0xc1, 0x6d, 0x14, 0xac,
 0xf9, 0x5f, 0x4f, 0xc4, 0xc3, 0xd1, 0xfc, 0xdd, 0xb2, 0x59, 0xe6, 0xb5,
 0x36, 0x52, 0x4a, 0x2a
};

u4byte  l_box[4][256];
u4byte  lb_init = 0;

#define v_0     0x67452301
#define v_1     0xefcdab89

/* s_fun(s_fun(s_fun(v)))           */

#define k2_0    0x30d32e58
#define k2_1    0xb89e4984

/* s_fun(s_fun(s_fun(s_fun(v))))    */

#define k3_0    0x0957cfec
#define k3_1    0xd800502e

#define bp_fun(a,b,c,d,e,f,g,h)     \
    u = (e ^ g) & 0x00ffff00;       \
    v = (f ^ h) & 0x0000ffff;       \
    a = e ^ u; c = g ^ u;           \
    b = f ^ v; d = h ^ v

#define ibp_fun(a,b,c,d,e,f,g,h)    \
    u = (e ^ g) & 0xff0000ff;       \
    v = (f ^ h) & 0xffff0000;       \
    a = e ^ u; c = g ^ u;           \
    b = f ^ v; d = h ^ v

#define bp2_fun(x,y)                \
    w = (x ^ y) & 0x00ff00ff;       \
    x ^= w; y ^= w;                 \

#define s_fun(x,y)          \
    p = x; q = x >> 8;      \
    r = y; s = y >> 8;      \
    x  = l_box[0][r & 255]; \
    y  = l_box[0][p & 255]; \
    p >>=  16; r >>=  16;   \
    x |= l_box[1][q & 255]; \
    y |= l_box[1][s & 255]; \
    x |= l_box[2][r & 255]; \
    y |= l_box[2][p & 255]; \
    x |= l_box[3][p >> 8];  \
    y |= l_box[3][r >> 8]

#define sx_fun(x,y)         \
    p = x >>  8;            \
    q = x >> 16;            \
    x  = l_box[0][x & 255]; \
    x |= l_box[1][p & 255]; \
    x |= l_box[2][q & 255]; \
    x |= l_box[3][q >> 8];  \
    p = y >>  8;            \
    q = y >> 16;            \
    y  = l_box[0][y & 255]; \
    y |= l_box[1][p & 255]; \
    y |= l_box[2][q & 255]; \
    y |= l_box[3][q >> 8]

#define spx_fun(x,y)        \
    sx_fun(x,y);            \
    y ^= x;                 \
    x ^= rotr(y, 16);       \
    y ^= rotr(x, 8);        \
    x ^= y

#define sp_fun(x,y)         \
    s_fun(x,y);             \
    y ^= x;                 \
    x ^= rotr(y, 16);       \
    y ^= rotr(x, 8);        \
    x ^= y

#define sr_fun(x,y)         \
    p = x; q = x >> 8;      \
    r = y; s = y >> 8;      \
    y  = l_box[1][p & 255]; \
    x  = l_box[1][r & 255]; \
    p >>= 16; r >>= 16;     \
    x |= l_box[2][q & 255]; \
    y |= l_box[2][s & 255]; \
    y |= l_box[3][p & 255]; \
    x |= l_box[3][r & 255]; \
    x |= l_box[0][r >>  8]; \
    y |= l_box[0][p >>  8]

#define f_fun(a,b,c,d,k)            \
    u = c ^ *(k); v = d ^ *(k + 1); \
    sp_fun(u, v);                   \
    u ^= *(k + 2); v ^= *(k + 3);   \
    sr_fun(u, v);                   \
    a ^= v;                         \
    b ^= u

#define byte_adr(x,n)   *(((u1byte*)&x)+n)

u4byte  l_key[72];

u4byte  mod_inv(u4byte x)
{   u4byte  y1, y2, a, b, q;

    y1 = ~((-x) / x); y2 = 1;

    a = x; b = y1 * x;

    for(;;)
    {
        q = a / b; 
        
        if((a -= q * b) == 0)

            return (x * y1 == 1 ? y1 : -y1);
        
        y2 -= q * y1;

        q = b / a; 
        
        if((b -= q * a) == 0)
        
            return (x * y2 == 1 ? y2 : -y2);

        y1 -= q * y2;
    }
};

void g_fun(u4byte y[8], u4byte l[8], u4byte v[2])
{   u4byte  p,q;

    spx_fun(y[0], y[1]); spx_fun(v[0], v[1]); 
    l[0] = v[0] ^= y[0]; l[1] = v[1] ^= y[1];

    spx_fun(y[2], y[3]); spx_fun(v[0], v[1]); 
    l[2] = v[0] ^= y[2]; l[3] = v[1] ^= y[3];

    spx_fun(y[4], y[5]); spx_fun(v[0], v[1]);  
    l[4] = v[0] ^= y[4]; l[5] = v[1] ^= y[5];

    spx_fun(y[6], y[7]); spx_fun(v[0], v[1]); 
    l[6] = v[0] ^= y[6]; l[7] = v[1] ^= y[7];
};

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u4byte  lk[8], v[2], lout[8];
    u4byte  i, j, k, w;

    if(!lb_init)
    {
        for(i = 0; i < 256; ++i)
        {
            l_box[0][i] = ((u4byte)(s_box[i]));
            l_box[1][i] = ((u4byte)(s_box[i])) <<  8;
            l_box[2][i] = ((u4byte)(s_box[i])) << 16;
            l_box[3][i] = ((u4byte)(s_box[i])) << 24;
        }

        lb_init = 1;
    }

    v[0] = bswap(v_0); v[1] = bswap(v_1);

    lk[0] = io_swap(in_key[0]); lk[1] = io_swap(in_key[1]);
    lk[2] = io_swap(in_key[2]); lk[3] = io_swap(in_key[3]);

    lk[4] = io_swap(key_len > 128 ? in_key[4] : k2_0);
    lk[5] = io_swap(key_len > 128 ? in_key[5] : k2_1);

    lk[6] = io_swap(key_len > 192 ? in_key[6] : k3_0);
    lk[7] = io_swap(key_len > 192 ? in_key[7] : k3_1);

    g_fun(lk, lout, v);

    for(i = 0; i < 8; ++i)
    {
        g_fun(lk, lout, v);

        for(j = 0; j < 4; ++j)
        {
            // this is complex because of a byte swap in each 32 bit output word

            k = 2 * (48 - 16 * j + 2 * (i / 2) - i % 2);

            ((u1byte*)l_key)[k + 3]   = ((u1byte*)lout)[j];
            ((u1byte*)l_key)[k + 2]   = ((u1byte*)lout)[j + 16];

            ((u1byte*)l_key)[k + 19]  = ((u1byte*)lout)[j +  8];
            ((u1byte*)l_key)[k + 18]  = ((u1byte*)lout)[j + 24];

            ((u1byte*)l_key)[k + 131] = ((u1byte*)lout)[j +  4];
            ((u1byte*)l_key)[k + 130] = ((u1byte*)lout)[j + 20];

            ((u1byte*)l_key)[k + 147] = ((u1byte*)lout)[j + 12];
            ((u1byte*)l_key)[k + 146] = ((u1byte*)lout)[j + 28];
        }
    }

    for(i = 52; i < 60; ++i)
    {
        l_key[i] |= 1; l_key[i + 12] = mod_inv(l_key[i]);
    }

    for(i = 0; i < 48; i += 4)
    {
        bp2_fun(l_key[i], l_key[i + 1]);
    }

    return (u4byte*)&l_key;
};

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte      a,b,c,d,p,q,r,s,u,v;

    p = io_swap(in_blk[0]); q = io_swap(in_blk[1]); 
    r = io_swap(in_blk[2]); s = io_swap(in_blk[3]);
    
    p ^= l_key[48]; q ^= l_key[49]; r ^= l_key[50]; s ^= l_key[51]; 
    p *= l_key[52]; q *= l_key[53]; r *= l_key[54]; s *= l_key[55];

    bp_fun(a, b, c, d, p, q, r, s);

    f_fun(a, b, c, d, l_key);
    f_fun(c, d, a, b, l_key +  4);
    f_fun(a, b, c, d, l_key +  8);
    f_fun(c, d, a, b, l_key + 12);
    f_fun(a, b, c, d, l_key + 16);
    f_fun(c, d, a, b, l_key + 20);
    f_fun(a, b, c, d, l_key + 24);
    f_fun(c, d, a, b, l_key + 28);
    f_fun(a, b, c, d, l_key + 32);
    f_fun(c, d, a, b, l_key + 36);
    f_fun(a, b, c, d, l_key + 40);
    f_fun(c, d, a, b, l_key + 44);

    ibp_fun(p, q, r, s, a, b, c, d);        
    
    p *= l_key[68]; q *= l_key[69]; r *= l_key[70]; s *= l_key[71]; 
    p ^= l_key[60]; q ^= l_key[61]; r ^= l_key[62]; s ^= l_key[63];
    
    out_blk[0] = io_swap(p); out_blk[1] = io_swap(q);
    out_blk[2] = io_swap(r); out_blk[3] = io_swap(s);
};

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte      a,b,c,d,p,q,r,s,u,v;

    p = io_swap(in_blk[0]); q = io_swap(in_blk[1]); 
    r = io_swap(in_blk[2]); s = io_swap(in_blk[3]);

    p ^= l_key[60]; q ^= l_key[61]; r ^= l_key[62]; s ^= l_key[63];
    p *= l_key[56]; q *= l_key[57]; r *= l_key[58]; s *= l_key[59];

    bp_fun(a, b, c, d, p, q, r, s);

    f_fun(a, b, c, d, l_key + 44);
    f_fun(c, d, a, b, l_key + 40);

    f_fun(a, b, c, d, l_key + 36);
    f_fun(c, d, a, b, l_key + 32);

    f_fun(a, b, c, d, l_key + 28);
    f_fun(c, d, a, b, l_key + 24);
    
    f_fun(a, b, c, d, l_key + 20);
    f_fun(c, d, a, b, l_key + 16);

    f_fun(a, b, c, d, l_key + 12);
    f_fun(c, d, a, b, l_key +  8);
    
    f_fun(a, b, c, d, l_key +  4);
    f_fun(c, d, a, b, l_key);

    ibp_fun(p, q, r, s, a, b, c, d);        
    
    p *= l_key[64]; q *= l_key[65]; r *= l_key[66]; s *= l_key[67]; 
    p ^= l_key[48]; q ^= l_key[49]; r ^= l_key[50]; s ^= l_key[51];

    out_blk[0] = io_swap(p); out_blk[1] = io_swap(q); 
    out_blk[2] = io_swap(r); out_blk[3] = io_swap(s);
};
