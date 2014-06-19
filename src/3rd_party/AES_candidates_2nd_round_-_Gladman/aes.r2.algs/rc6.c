
// Copyright in this code is held by Dr B.R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin
// and subject to any constraints placed on the use of the algorithm by
// its designers (if such constraints may exist, this will be indicated
// below).
//
// Dr. B. R. Gladman                               . 25th January 2000.
//
// This is an implementation of RC6, an encryption algorithm designed by
// by Ron Rivest (RSA Labs) and submitted as a candidate algorithm for the
// Advanced Encryption Standard programme of the US National Institute of
// Standards and Technology.
//
// The designers of the RC6 algorithm may have placed restrictions on its
// use that will need to be complied with in any use of this code.

#include "aes_defs.h"
#include "rc6.h"

char* RC6(name(void))
{
    return "rc6";
}

#define f_rnd(i,a,b,c,d)                        \
        u = rotl(d * (d + d + 1), 5);           \
        t = rotl(b * (b + b + 1), 5);           \
        a = rotl(a ^ t, u) + context->l_key[i];     \
        c = rotl(c ^ u, t) + context->l_key[i + 1]

#define i_rnd(i,a,b,c,d)                        \
        u = rotl(d * (d + d + 1), 5);           \
        t = rotl(b * (b + b + 1), 5);           \
        c = rotr(c - context->l_key[i + 1], t) ^ u; \
        a = rotr(a - context->l_key[i], u) ^ t

// initialise the key schedule from the user supplied key

void RC6(set_key)(RC6(CONTEXT) *context, const u1byte in_key[], const u4byte key_len, const enum dir_flag f)
{   u4byte  l[8], i, j, k, a, b, t;

    context->l_key[0] = 0xb7e15163;

    for(k = 1; k < 44; ++k)

        context->l_key[k] = context->l_key[k - 1] + 0x9e3779b9;

    for(k = 0; k < key_len / 32; ++k)

        l[k] = u4byte_in(in_key + 4 * k);

    t = (key_len / 32) - 1;         // t = (key_len / 32);

    a = b = i = j = 0;

    for(k = 0; k < 132; ++k)
    {   a = rotl(context->l_key[i] + a + b, 3); b += a;
        b = rotl(l[j] + b, b);
        context->l_key[i] = a; l[j] = b;
        i = (i == 43 ? 0 : i + 1);  // i = (i + 1) % 44;
        j = (j == t ? 0 : j + 1);   // j = (j + 1) % t;
    }

    return;
}

// encrypt a block of text

void RC6(encrypt)(RC6(CONTEXT) *context, const u1byte in_blk[16], u1byte out_blk[16])
{   u4byte  a,b,c,d,t,u;

    a = u4byte_in(in_blk    ); b = u4byte_in(in_blk +  4) + context->l_key[0];
    c = u4byte_in(in_blk + 8); d = u4byte_in(in_blk + 12) + context->l_key[1];

    f_rnd( 2,a,b,c,d); f_rnd( 4,b,c,d,a);
    f_rnd( 6,c,d,a,b); f_rnd( 8,d,a,b,c);
    f_rnd(10,a,b,c,d); f_rnd(12,b,c,d,a);
    f_rnd(14,c,d,a,b); f_rnd(16,d,a,b,c);
    f_rnd(18,a,b,c,d); f_rnd(20,b,c,d,a);
    f_rnd(22,c,d,a,b); f_rnd(24,d,a,b,c);
    f_rnd(26,a,b,c,d); f_rnd(28,b,c,d,a);
    f_rnd(30,c,d,a,b); f_rnd(32,d,a,b,c);
    f_rnd(34,a,b,c,d); f_rnd(36,b,c,d,a);
    f_rnd(38,c,d,a,b); f_rnd(40,d,a,b,c);

    u4byte_out(out_blk,     a + context->l_key[42]); u4byte_out(out_blk +  4, b);
    u4byte_out(out_blk + 8, c + context->l_key[43]); u4byte_out(out_blk + 12, d);
}

// decrypt a block of text

void RC6(decrypt)(RC6(CONTEXT) *context, const u1byte in_blk[16], u1byte out_blk[16])
{   u4byte  a,b,c,d,t,u;

    d = u4byte_in(in_blk + 12); c = u4byte_in(in_blk + 8) - context->l_key[43];
    b = u4byte_in(in_blk +  4); a = u4byte_in(in_blk    ) - context->l_key[42];

    i_rnd(40,d,a,b,c); i_rnd(38,c,d,a,b);
    i_rnd(36,b,c,d,a); i_rnd(34,a,b,c,d);
    i_rnd(32,d,a,b,c); i_rnd(30,c,d,a,b);
    i_rnd(28,b,c,d,a); i_rnd(26,a,b,c,d);
    i_rnd(24,d,a,b,c); i_rnd(22,c,d,a,b);
    i_rnd(20,b,c,d,a); i_rnd(18,a,b,c,d);
    i_rnd(16,d,a,b,c); i_rnd(14,c,d,a,b);
    i_rnd(12,b,c,d,a); i_rnd(10,a,b,c,d);
    i_rnd( 8,d,a,b,c); i_rnd( 6,c,d,a,b);
    i_rnd( 4,b,c,d,a); i_rnd( 2,a,b,c,d);

    u4byte_out(out_blk + 12, d - context->l_key[1]); u4byte_out(out_blk + 8, c);
    u4byte_out(out_blk +  4, b - context->l_key[0]); u4byte_out(out_blk,     a);
}
