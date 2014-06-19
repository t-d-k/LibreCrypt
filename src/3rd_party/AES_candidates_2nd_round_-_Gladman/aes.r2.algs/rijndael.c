
// Copyright in this code is held by Dr B.R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin
// and subject to any constraints placed on the use of the algorithm by
// its designers (if such constraints may exist, this will be indicated
// below).
//
// Dr. B. R. Gladman                               . 25th January 2000.
//
// This is an implementation of Rijndael, an encryption algorithm designed
// by Daemen and Rijmen and submitted as a candidate algorithm for the
// Advanced Encryption Standard programme of the US National Institute of
// Standards and Technology.
//
// The designers of Rijndael have not placed any constraints on the use of
// this algorithm.

#include "aes_defs.h"
#include "rijndael.h"

#define LARGE_TABLES

#ifdef  __cplusplus
    namespace
    {
#endif

STATIC u1byte  pow_tab[256];
STATIC u1byte  log_tab[256];
STATIC u1byte  sbx_tab[256];
STATIC u1byte  isb_tab[256];
STATIC u4byte  rco_tab[ 10];
STATIC u4byte  ft_tab[4][256];
STATIC u4byte  it_tab[4][256];

#ifdef  LARGE_TABLES
  STATIC u4byte  fl_tab[4][256];
  STATIC u4byte  il_tab[4][256];
#endif

STATIC u4byte  tab_gen = 0;

inline u1byte f_mult(u1byte a, u1byte b)
{   u1byte aa = log_tab[a], cc = aa + log_tab[b];

    return pow_tab[cc + (cc < aa ? 1 : 0)];

}

// Extract byte from a 32 bit quantity (little endian notation)

#define byte(x,n)   ((u1byte)((x) >> (8 * (n))))

#define ff_mult(a,b)    (a && b ? f_mult(a, b) : 0)

#define f_rn(bo, bi, n, k)                          \
    bo[n] =  ft_tab[0][byte(bi[n],0)] ^             \
             ft_tab[1][byte(bi[(n + 1) & 3],1)] ^   \
             ft_tab[2][byte(bi[(n + 2) & 3],2)] ^   \
             ft_tab[3][byte(bi[(n + 3) & 3],3)] ^ *(k + n)

#define i_rn(bo, bi, n, k)                          \
    bo[n] =  it_tab[0][byte(bi[n],0)] ^             \
             it_tab[1][byte(bi[(n + 3) & 3],1)] ^   \
             it_tab[2][byte(bi[(n + 2) & 3],2)] ^   \
             it_tab[3][byte(bi[(n + 1) & 3],3)] ^ *(k + n)

#ifdef LARGE_TABLES

#define ls_box(x)                \
    ( fl_tab[0][byte(x, 0)] ^    \
      fl_tab[1][byte(x, 1)] ^    \
      fl_tab[2][byte(x, 2)] ^    \
      fl_tab[3][byte(x, 3)] )

#define f_rl(bo, bi, n, k)                          \
    bo[n] =  fl_tab[0][byte(bi[n],0)] ^             \
             fl_tab[1][byte(bi[(n + 1) & 3],1)] ^   \
             fl_tab[2][byte(bi[(n + 2) & 3],2)] ^   \
             fl_tab[3][byte(bi[(n + 3) & 3],3)] ^ *(k + n)

#define i_rl(bo, bi, n, k)                          \
    bo[n] =  il_tab[0][byte(bi[n],0)] ^             \
             il_tab[1][byte(bi[(n + 3) & 3],1)] ^   \
             il_tab[2][byte(bi[(n + 2) & 3],2)] ^   \
             il_tab[3][byte(bi[(n + 1) & 3],3)] ^ *(k + n)

#else

#define ls_box(x)                            \
    ((u4byte)sbx_tab[byte(x, 0)] <<  0) ^    \
    ((u4byte)sbx_tab[byte(x, 1)] <<  8) ^    \
    ((u4byte)sbx_tab[byte(x, 2)] << 16) ^    \
    ((u4byte)sbx_tab[byte(x, 3)] << 24)

#define f_rl(bo, bi, n, k)                                      \
    bo[n] = (u4byte)sbx_tab[byte(bi[n],0)] ^                    \
        rotl(((u4byte)sbx_tab[byte(bi[(n + 1) & 3],1)]),  8) ^  \
        rotl(((u4byte)sbx_tab[byte(bi[(n + 2) & 3],2)]), 16) ^  \
        rotl(((u4byte)sbx_tab[byte(bi[(n + 3) & 3],3)]), 24) ^ *(k + n)

#define i_rl(bo, bi, n, k)                                      \
    bo[n] = (u4byte)isb_tab[byte(bi[n],0)] ^                    \
        rotl(((u4byte)isb_tab[byte(bi[(n + 3) & 3],1)]),  8) ^  \
        rotl(((u4byte)isb_tab[byte(bi[(n + 2) & 3],2)]), 16) ^  \
        rotl(((u4byte)isb_tab[byte(bi[(n + 1) & 3],3)]), 24) ^ *(k + n)

#endif

STATIC void gen_tabs(void)
{   u4byte  i, t;
    u1byte  p, q;

    // log and power tables for GF(2**8) finite field with
    // 0x011b as modular polynomial - the simplest prmitive
    // root is 0x03, used here to generate the tables

    for(i = 0,p = 1; i < 256; ++i)
    {
        pow_tab[i] = (u1byte)p; log_tab[p] = (u1byte)i;

        p ^=  (p << 1) ^ (p & 0x80 ? 0x01b : 0);
    }

    log_tab[1] = 0;

    for(i = 0,p = 1; i < 10; ++i)
    {
        rco_tab[i] = p;

        p = (p << 1) ^ (p & 0x80 ? 0x01b : 0);
    }

    for(i = 0; i < 256; ++i)
    {
        p = (i ? pow_tab[255 - log_tab[i]] : 0);
        q  = ((p >> 7) | (p << 1)) ^ ((p >> 6) | (p << 2));
        p ^= 0x63 ^ q ^ ((q >> 6) | (q << 2));
        sbx_tab[i] = p; isb_tab[p] = (u1byte)i;
    }

    for(i = 0; i < 256; ++i)
    {
        p = sbx_tab[i];

#ifdef  LARGE_TABLES

        t = p; fl_tab[0][i] = t;
        fl_tab[1][i] = rotl(t,  8);
        fl_tab[2][i] = rotl(t, 16);
        fl_tab[3][i] = rotl(t, 24);
#endif
        t = ((u4byte)ff_mult(2, p)) |
            ((u4byte)p <<  8) |
            ((u4byte)p << 16) |
            ((u4byte)ff_mult(3, p) << 24);

        ft_tab[0][i] = t;
        ft_tab[1][i] = rotl(t,  8);
        ft_tab[2][i] = rotl(t, 16);
        ft_tab[3][i] = rotl(t, 24);

        p = isb_tab[i];

#ifdef  LARGE_TABLES

        t = p; il_tab[0][i] = t;
        il_tab[1][i] = rotl(t,  8);
        il_tab[2][i] = rotl(t, 16);
        il_tab[3][i] = rotl(t, 24);
#endif
        t = ((u4byte)ff_mult(14, p)) |
            ((u4byte)ff_mult( 9, p) <<  8) |
            ((u4byte)ff_mult(13, p) << 16) |
            ((u4byte)ff_mult(11, p) << 24);

        it_tab[0][i] = t;
        it_tab[1][i] = rotl(t,  8);
        it_tab[2][i] = rotl(t, 16);
        it_tab[3][i] = rotl(t, 24);
    }

    tab_gen = 1;
}

#define star_x(x) (((x) & 0x7f7f7f7f) << 1) ^ ((((x) & 0x80808080) >> 7) * 0x1b)

#define imix_col(y,x)       \
    u   = star_x(x);        \
    v   = star_x(u);        \
    w   = star_x(v);        \
    t   = w ^ (x);          \
   (y)  = u ^ v ^ w;        \
   (y) ^= rotr(u ^ t,  8) ^ \
          rotr(v ^ t, 16) ^ \
          rotr(t,24)

#ifdef  __cplusplus
    }   // end of anonymous namespace
#endif

char* RIJNDAEL(name(void))
{
    return "rijndael";
}

// initialise the key schedule from the user supplied key

#define loop4(i)                                    \
{   t = rotr(t,  8); t = ls_box(t) ^ rco_tab[i];    \
    t ^= context->e_key[4 * i];     context->e_key[4 * i + 4] = t;    \
    t ^= context->e_key[4 * i + 1]; context->e_key[4 * i + 5] = t;    \
    t ^= context->e_key[4 * i + 2]; context->e_key[4 * i + 6] = t;    \
    t ^= context->e_key[4 * i + 3]; context->e_key[4 * i + 7] = t;    \
}

#define loop6(i)                                    \
{   t = rotr(t,  8); t = ls_box(t) ^ rco_tab[i];    \
    t ^= context->e_key[6 * i];     context->e_key[6 * i + 6] = t;    \
    t ^= context->e_key[6 * i + 1]; context->e_key[6 * i + 7] = t;    \
    t ^= context->e_key[6 * i + 2]; context->e_key[6 * i + 8] = t;    \
    t ^= context->e_key[6 * i + 3]; context->e_key[6 * i + 9] = t;    \
    t ^= context->e_key[6 * i + 4]; context->e_key[6 * i + 10] = t;   \
    t ^= context->e_key[6 * i + 5]; context->e_key[6 * i + 11] = t;   \
}

#define loop8(i)                                    \
{   t = rotr(t,  8); ; t = ls_box(t) ^ rco_tab[i];  \
    t ^= context->e_key[8 * i];     context->e_key[8 * i + 8] = t;    \
    t ^= context->e_key[8 * i + 1]; context->e_key[8 * i + 9] = t;    \
    t ^= context->e_key[8 * i + 2]; context->e_key[8 * i + 10] = t;   \
    t ^= context->e_key[8 * i + 3]; context->e_key[8 * i + 11] = t;   \
    t  = context->e_key[8 * i + 4] ^ ls_box(t);    \
    context->e_key[8 * i + 12] = t;                \
    t ^= context->e_key[8 * i + 5]; context->e_key[8 * i + 13] = t;   \
    t ^= context->e_key[8 * i + 6]; context->e_key[8 * i + 14] = t;   \
    t ^= context->e_key[8 * i + 7]; context->e_key[8 * i + 15] = t;   \
}

void RIJNDAEL(set_key)(RIJNDAEL(CONTEXT) *context, const u1byte in_key[], const u4byte key_len, const enum dir_flag f)
{   u4byte  i, t, u, v, w;

    if(!tab_gen)

        gen_tabs();

    mode = f;

    context->k_len = (key_len + 31) / 32;

    context->e_key[0] = u4byte_in(in_key     );
    context->e_key[1] = u4byte_in(in_key +  4);
    context->e_key[2] = u4byte_in(in_key +  8);
    context->e_key[3] = u4byte_in(in_key + 12);

#if(0)

    {   u4byte  *k1, *k2, *km, *rcp;
        if(context->k_len > 4)
        {
            context->e_key[4] = u4byte_in(in_key + 16);
            context->e_key[5] = u4byte_in(in_key + 20);
        }

        if(context->k_len > 6)
        {
            context->e_key[6] = u4byte_in(in_key + 24);
            context->e_key[7] = u4byte_in(in_key + 28);
        }

        rcp = rco_tab; k1 = context->e_key;
        k2 = k1 + context->k_len;
        km = k1 + 5 * context->k_len + 24;
        t = *(k2 - 1);

        switch(context->k_len)
        {
            case 4: while(k2 < km)
                    {
                        t = ls_box(rotr(t,  8)) ^ *rcp++;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                    }
                    break;

            case 6: while(k2 < km)
                    {
                        t = ls_box(rotr(t,  8)) ^ *rcp++;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                    }
                    break;

            case 8: while(k2 < km)
                    {
                        t = ls_box(rotr(t,  8)) ^ *rcp++;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t = ls_box(t);
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                        t ^= *k1++; *k2++ = t;
                    }
                    break;
        }
    }

#else

    switch(context->k_len)
    {
        case 4: t = context->e_key[3];
                for(i = 0; i < 10; ++i)
                    loop4(i);
                break;

        case 6: context->e_key[4] = u4byte_in(in_key + 16);
                t = context->e_key[5] = u4byte_in(in_key + 20);
                for(i = 0; i < 8; ++i)
                    loop6(i);
                break;

        case 8: context->e_key[4] = u4byte_in(in_key + 16);
                context->e_key[5] = u4byte_in(in_key + 20);
                context->e_key[6] = u4byte_in(in_key + 24);
                t = context->e_key[7] = u4byte_in(in_key + 28);
                for(i = 0; i < 7; ++i)
                    loop8(i);
                break;
    }

#endif

    if(mode != enc)
    {
        context->d_key[0] = context->e_key[0]; context->d_key[1] = context->e_key[1];
        context->d_key[2] = context->e_key[2]; context->d_key[3] = context->e_key[3];

        for(i = 4; i < 4 * context->k_len + 24; ++i)
        {
            imix_col(context->d_key[i], context->e_key[i]);
        }
    }

    return;
}

// encrypt a block of text

#define f_nround(bo, bi, k) \
    f_rn(bo, bi, 0, k);     \
    f_rn(bo, bi, 1, k);     \
    f_rn(bo, bi, 2, k);     \
    f_rn(bo, bi, 3, k);     \
    k += 4

#define f_lround(bo, bi, k) \
    f_rl(bo, bi, 0, k);     \
    f_rl(bo, bi, 1, k);     \
    f_rl(bo, bi, 2, k);     \
    f_rl(bo, bi, 3, k)

void RIJNDAEL(encrypt)(RIJNDAEL(CONTEXT) *context, const u1byte in_blk[16], u1byte out_blk[16])
{   u4byte  b0[4], b1[4], *kp;

    b0[0] = u4byte_in(in_blk    ) ^ context->e_key[0];
    b0[1] = u4byte_in(in_blk +  4) ^ context->e_key[1];
    b0[2] = u4byte_in(in_blk + 8) ^ context->e_key[2];
    b0[3] = u4byte_in(in_blk + 12) ^ context->e_key[3];

    kp = context->e_key + 4;

    if(context->k_len > 6)
    {
        f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    }

    if(context->k_len > 4)
    {
        f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    }

    f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    f_nround(b1, b0, kp); f_nround(b0, b1, kp);
    f_nround(b1, b0, kp); f_lround(b0, b1, kp);

    u4byte_out(out_blk,      b0[0]); u4byte_out(out_blk +  4, b0[1]);
    u4byte_out(out_blk +  8, b0[2]); u4byte_out(out_blk + 12, b0[3]);
}

// decrypt a block of text

#define i_nround(bo, bi, k) \
    i_rn(bo, bi, 0, k);     \
    i_rn(bo, bi, 1, k);     \
    i_rn(bo, bi, 2, k);     \
    i_rn(bo, bi, 3, k);     \
    k -= 4

#define i_lround(bo, bi, k) \
    i_rl(bo, bi, 0, k);     \
    i_rl(bo, bi, 1, k);     \
    i_rl(bo, bi, 2, k);     \
    i_rl(bo, bi, 3, k)

void RIJNDAEL(decrypt)(RIJNDAEL(CONTEXT) *context, const u1byte in_blk[16], u1byte out_blk[16])
{   u4byte  b0[4], b1[4], *kp;

    b0[0] = u4byte_in(in_blk     ) ^ context->e_key[4 * context->k_len + 24];
    b0[1] = u4byte_in(in_blk +  4) ^ context->e_key[4 * context->k_len + 25];
    b0[2] = u4byte_in(in_blk +  8) ^ context->e_key[4 * context->k_len + 26];
    b0[3] = u4byte_in(in_blk + 12) ^ context->e_key[4 * context->k_len + 27];

    kp = context->d_key + 4 * (context->k_len + 5);

    if(context->k_len > 6)
    {
        i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    }

    if(context->k_len > 4)
    {
        i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    }

    i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    i_nround(b1, b0, kp); i_nround(b0, b1, kp);
    i_nround(b1, b0, kp); i_lround(b0, b1, kp);

    u4byte_out(out_blk,     b0[0]); u4byte_out(out_blk +  4, b0[1]);
    u4byte_out(out_blk + 8, b0[2]); u4byte_out(out_blk + 12, b0[3]);
}
