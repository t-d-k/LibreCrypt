
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         LOKI97 by Brown and Pieprzyk                                 */
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

/* Timing data for LOKI97 (loki.c)

Core timing without I/O endian conversion:

128 bit key:
Key Setup:    7430 cycles
Encrypt:      2134 cycles =    12.0 mbits/sec
Decrypt:      2192 cycles =    11.7 mbits/sec
Mean:         2163 cycles =    11.8 mbits/sec

192 bit key:
Key Setup:    7303 cycles
Encrypt:      2138 cycles =    12.0 mbits/sec
Decrypt:      2189 cycles =    11.7 mbits/sec
Mean:         2164 cycles =    11.8 mbits/sec

256 bit key:
Key Setup:    7166 cycles
Encrypt:      2131 cycles =    12.0 mbits/sec
Decrypt:      2184 cycles =    11.7 mbits/sec
Mean:         2158 cycles =    11.9 mbits/sec

Full timing with I/O endian conversion:

128 bit key:
Key Setup:    7582 cycles
Encrypt:      2174 cycles =    11.8 mbits/sec
Decrypt:      2235 cycles =    11.5 mbits/sec
Mean:         2205 cycles =    11.6 mbits/sec

192 bit key:
Key Setup:    7477 cycles
Encrypt:      2167 cycles =    11.8 mbits/sec
Decrypt:      2223 cycles =    11.5 mbits/sec
Mean:         2195 cycles =    11.7 mbits/sec

256 bit key:
Key Setup:    7365 cycles
Encrypt:      2177 cycles =    11.8 mbits/sec
Decrypt:      2194 cycles =    11.7 mbits/sec
Mean:         2186 cycles =    11.7 mbits/sec

*/

#define BYTE_SWAP

#ifdef CORE_TIME
#  undef BYTE_SWAP
#endif

#include "../std_defs.h"

static char *alg_name[] = { "loki", "loki.c", "loki97" };

char **cipher_name()
{
    return alg_name;
};

#define S1_SIZE     13
#define S1_LEN      (1 << S1_SIZE)
#define S1_MASK     (S1_LEN - 1)
#define S1_HMASK    (S1_MASK & ~0xff)
#define S1_POLY     0x2911

#define S2_SIZE     11
#define S2_LEN      (1 << S2_SIZE)
#define S2_MASK     (S2_LEN - 1)
#define S2_HMASK    (S2_MASK & ~0xff)
#define S2_POLY     0x0aa7

u4byte  delta[2] = { 0x7f4a7c15, 0x9e3779b9 };

u1byte  sb1[S1_LEN];    // GF(2^11) S box
u1byte  sb2[S2_LEN];    // GF(2^11) S box
u4byte  prm[256][2];
u4byte  init_done = 0;

u4byte  l_key[96];

#define add_eq(x,y)     (x)[1] += (y)[1] + (((x)[0] += (y)[0]) < (y)[0] ? 1 : 0)
#define sub_eq(x,y)     xs = (x)[0]; (x)[1] -= (y)[1] + (((x)[0] -= (y)[0]) > xs ? 1 : 0)   

u4byte ff_mult(u4byte a, u4byte b, u4byte tpow, u4byte mpol)
{   u4byte  r, s, m;

    r = s = 0; m = (1 << tpow); 

    while(b)
    {
        if(b & 1)
        
            s ^= a;
            
        b >>= 1; a <<= 1;
        
        if(a & m)
        
            a ^= mpol;
    }

    return s;
};

void init_tables(void)
{   u4byte  i, j, v;

    // initialise S box 1

    for(i = 0; i < S1_LEN; ++i)
    {
        j = v = i ^ S1_MASK; v = ff_mult(v, j, S1_SIZE, S1_POLY);
        sb1[i] = (u1byte)ff_mult(v, j, S1_SIZE, S1_POLY);
    } 
    // initialise S box 2

    for(i = 0; i < S2_LEN; ++i)
    {
        j = v = i ^ S2_MASK; v = ff_mult(v, j, S2_SIZE, S2_POLY);
        sb2[i] = (u1byte)ff_mult(v, j, S2_SIZE, S2_POLY);
    }

    // initialise permutation table

    for(i = 0; i < 256; ++i)
    {
        prm[i][0] = ((i &  1) << 7) | ((i &  2) << 14) | ((i &  4) << 21) | ((i &   8) << 28);
        prm[i][1] = ((i & 16) << 3) | ((i & 32) << 10) | ((i & 64) << 17) | ((i & 128) << 24);
    }
};

void f_fun(u4byte res[2], const u4byte in[2], const u4byte key[2])
{   u4byte  i, tt[2], pp[2];

    tt[0] = in[0] & ~key[0] | in[1] & key[0];
    tt[1] = in[1] & ~key[0] | in[0] & key[0];

    i = sb1[((tt[1] >> 24) | (tt[0] << 8)) & S1_MASK];
    pp[0]  = prm[i][0] >> 7; pp[1]  = prm[i][1] >> 7;
    i = sb2[(tt[1] >> 16) & S2_MASK];
    pp[0] |= prm[i][0] >> 6; pp[1] |= prm[i][1] >> 6;
    i = sb1[(tt[1] >>  8) & S1_MASK];
    pp[0] |= prm[i][0] >> 5; pp[1] |= prm[i][1] >> 5;
    i = sb2[tt[1] & S2_MASK]; 
    pp[0] |= prm[i][0] >> 4; pp[1] |= prm[i][1] >> 4;
    i = sb2[((tt[0] >> 24) | (tt[1] << 8)) & S2_MASK];
    pp[0] |= prm[i][0] >> 3; pp[1] |= prm[i][1] >> 3;
    i = sb1[(tt[0] >> 16) & S1_MASK]; 
    pp[0] |= prm[i][0] >> 2; pp[1] |= prm[i][1] >> 2;
    i = sb2[(tt[0] >>  8) & S2_MASK];      
    pp[0] |= prm[i][0] >> 1; pp[1] |= prm[i][1] >> 1;
    i = sb1[tt[0] & S1_MASK];          
    pp[0] |= prm[i][0];      pp[1] |= prm[i][1];

    res[0] ^=  sb1[byte(pp[0], 0) | (key[1] <<  8) & S1_HMASK]
            | (sb1[byte(pp[0], 1) | (key[1] <<  3) & S1_HMASK] << 8)
            | (sb2[byte(pp[0], 2) | (key[1] >>  2) & S2_HMASK] << 16)
            | (sb2[byte(pp[0], 3) | (key[1] >>  5) & S2_HMASK] << 24);
    res[1] ^=  sb1[byte(pp[1], 0) | (key[1] >>  8) & S1_HMASK]
            | (sb1[byte(pp[1], 1) | (key[1] >> 13) & S1_HMASK] << 8)
            | (sb2[byte(pp[1], 2) | (key[1] >> 18) & S2_HMASK] << 16)
            | (sb2[byte(pp[1], 3) | (key[1] >> 21) & S2_HMASK] << 24);
};

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u4byte  i, k1[2], k2[2], k3[2], k4[2], del[2], tt[2], sk[2];

    if(!init_done)
    {
        init_tables(); init_done = 1;
    }

    k4[0] = io_swap(in_key[1]); k4[1] = io_swap(in_key[0]);
    k3[0] = io_swap(in_key[3]); k3[1] = io_swap(in_key[2]);

    switch ((key_len + 63) / 64)
    {
    case 2:
        k2[0] = 0; k2[1] = 0; f_fun(k2, k3, k4);
        k1[0] = 0; k1[1] = 0; f_fun(k1, k4, k3);
        break;
    case 3:
        k2[0] = io_swap(in_key[5]); k2[1] = io_swap(in_key[4]);
        k1[0] = 0; k1[1] = 0; f_fun(k1, k4, k3);
        break;
    case 4: 
        k2[0] = io_swap(in_key[5]); k2[1] = io_swap(in_key[4]);
        k1[0] = io_swap(in_key[7]); k1[1] = io_swap(in_key[6]);
    }

    del[0] = delta[0]; del[1] = delta[1];

    for(i = 0; i < 48; ++i)
    {
        tt[0] = k1[0]; tt[1] = k1[1]; 
        add_eq(tt, k3); add_eq(tt, del); add_eq(del, delta);
        sk[0] = k4[0]; sk[1] = k4[1];
        k4[0] = k3[0]; k4[1] = k3[1];
        k3[0] = k2[0]; k3[1] = k2[1];
        k2[0] = k1[0]; k2[1] = k1[1];
        k1[0] = sk[0]; k1[1] = sk[1];
        f_fun(k1, tt, k3);
        l_key[i + i] = k1[0]; l_key[i + i + 1] = k1[1];
    }

    return l_key;
};

#define r_fun(l,r,k)        \
    add_eq((l),(k));        \
    f_fun((r),(l),(k) + 2); \
    add_eq((l), (k) + 4)

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4];

    blk[3] = io_swap(in_blk[0]); blk[2] = io_swap(in_blk[1]);
    blk[1] = io_swap(in_blk[2]); blk[0] = io_swap(in_blk[3]);

    r_fun(blk, blk + 2, l_key +  0);
    r_fun(blk + 2, blk, l_key +  6);
    r_fun(blk, blk + 2, l_key + 12);
    r_fun(blk + 2, blk, l_key + 18);
    r_fun(blk, blk + 2, l_key + 24);
    r_fun(blk + 2, blk, l_key + 30);
    r_fun(blk, blk + 2, l_key + 36);
    r_fun(blk + 2, blk, l_key + 42);
    r_fun(blk, blk + 2, l_key + 48);
    r_fun(blk + 2, blk, l_key + 54);
    r_fun(blk, blk + 2, l_key + 60);
    r_fun(blk + 2, blk, l_key + 66);
    r_fun(blk, blk + 2, l_key + 72);
    r_fun(blk + 2, blk, l_key + 78);
    r_fun(blk, blk + 2, l_key + 84);
    r_fun(blk + 2, blk, l_key + 90);

    out_blk[3] = io_swap(blk[2]); out_blk[2] = io_swap(blk[3]);
    out_blk[1] = io_swap(blk[0]); out_blk[0] = io_swap(blk[1]);
};

#define ir_fun(l,r,k)       \
    sub_eq((l),(k) + 4);    \
    f_fun((r),(l),(k) + 2); \
    sub_eq((l),(k))

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4], xs;

    blk[3] = io_swap(in_blk[0]); blk[2] = io_swap(in_blk[1]);
    blk[1] = io_swap(in_blk[2]); blk[0] = io_swap(in_blk[3]);

    ir_fun(blk, blk + 2, l_key + 90); 
    ir_fun(blk + 2, blk, l_key + 84);
    ir_fun(blk, blk + 2, l_key + 78); 
    ir_fun(blk + 2, blk, l_key + 72);
    ir_fun(blk, blk + 2, l_key + 66); 
    ir_fun(blk + 2, blk, l_key + 60);
    ir_fun(blk, blk + 2, l_key + 54); 
    ir_fun(blk + 2, blk, l_key + 48);
    ir_fun(blk, blk + 2, l_key + 42); 
    ir_fun(blk + 2, blk, l_key + 36);
    ir_fun(blk, blk + 2, l_key + 30); 
    ir_fun(blk + 2, blk, l_key + 24);
    ir_fun(blk, blk + 2, l_key + 18); 
    ir_fun(blk + 2, blk, l_key + 12);
    ir_fun(blk, blk + 2, l_key +  6); 
    ir_fun(blk + 2, blk, l_key);

    out_blk[3] = io_swap(blk[2]); out_blk[2] = io_swap(blk[3]);
    out_blk[1] = io_swap(blk[0]); out_blk[0] = io_swap(blk[1]);   
};
