
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         Serpent by Ross Anderson, Eli Biham and Lars Knudsen         */
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

/* Timing data for Serpent (serpent.c)

Core timing without I/O endian conversion:

128 bit key:
Key Setup:    2402 cycles
Encrypt:       952 cycles =    26.9 mbits/sec
Decrypt:       914 cycles =    28.0 mbits/sec
Mean:          933 cycles =    27.4 mbits/sec

192 bit key:
Key Setup:    2449 cycles
Encrypt:       952 cycles =    26.9 mbits/sec
Decrypt:       914 cycles =    28.0 mbits/sec
Mean:          933 cycles =    27.4 mbits/sec

256 bit key:
Key Setup:    2349 cycles
Encrypt:       952 cycles =    26.9 mbits/sec
Decrypt:       914 cycles =    28.0 mbits/sec
Mean:          933 cycles =    27.4 mbits/sec

Full timing with I/O endian conversion:

128 bit key:
Key Setup:    2415 cycles
Encrypt:       985 cycles =    26.0 mbits/sec
Decrypt:       954 cycles =    26.8 mbits/sec
Mean:          970 cycles =    26.4 mbits/sec

192 bit key:
Key Setup:    2438 cycles
Encrypt:       985 cycles =    26.0 mbits/sec
Decrypt:       954 cycles =    26.8 mbits/sec
Mean:          970 cycles =    26.4 mbits/sec

256 bit key:
Key Setup:    2463 cycles
Encrypt:       985 cycles =    26.0 mbits/sec
Decrypt:       954 cycles =    26.8 mbits/sec
Mean:          970 cycles =    26.4 mbits/sec

*/

#define BLOCK_SWAP

#ifdef CORE_TIME
#  undef    BLOCK_SWAP
#endif

#include "../std_defs.h"

static char *alg_name[] = { "serpent", "serpent.c", "serpent" };

char **cipher_name()
{
    return alg_name;
}

/* Partially optimised Serpent S Box boolean functions derived  */
/* using a recursive descent analyser but without a full search */
/* of all subtrees. This set of S boxes is the result of work   */
/* by Sam Simpson and Brian Gladman using the spare time on a   */
/* cluster of high capacity servers to search for S boxes with  */
/* this customised search engine.                               */
/*                                                              */
/* Copyright:   Dr B. R Gladman                                 */
/*              and Sam Simpson (s.simpson@mia.co.uk)           */ 
/*              17th December 1998                              */
/*                                                              */
/* We hereby give permission for information in this file to be */
/* used freely subject only to acknowledgement of its origin    */

/* 15 terms */

#define sb0(a,b,c,d,e,f,g,h)    \
    t1 = a ^ d;     \
    t2 = a & d;     \
    t3 = c ^ t1;    \
    t6 = b & t1;    \
    t4 = b ^ t3;    \
    t10 = ~t3;      \
    h = t2 ^ t4;    \
    t7 = a ^ t6;    \
    t14 = ~t7;      \
    t8 = c | t7;    \
    t11 = t3 ^ t7;  \
    g = t4 ^ t8;    \
    t12 = h & t11;  \
    f = t10 ^ t12;  \
    e = t12 ^ t14

/* 15 terms */

#define ib0(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = a ^ b;     \
    t3 = t1 | t2;   \
    t4 = d ^ t3;    \
    t7 = d & t2;    \
    t5 = c ^ t4;    \
    t8 = t1 ^ t7;   \
    g = t2 ^ t5;    \
    t11 = a & t4;   \
    t9 = g & t8;    \
    t14 = t5 ^ t8;  \
    f = t4 ^ t9;    \
    t12 = t5 | f;   \
    h = t11 ^ t12;  \
    e = h ^ t14

/* 14 terms!  */

#define sb1(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = b ^ t1;    \
    t3 = a | t2;    \
    t4 = d | t2;    \
    t5 = c ^ t3;    \
    g = d ^ t5;     \
    t7 = b ^ t4;    \
    t8 = t2 ^ g;    \
    t9 = t5 & t7;   \
    h = t8 ^ t9;    \
    t11 = t5 ^ t7;  \
    f = h ^ t11;    \
    t13 = t8 & t11; \
    e = t5 ^ t13

/* 17 terms */

#define ib1(a,b,c,d,e,f,g,h)    \
    t1 = a ^ d;     \
    t2 = a & b;     \
    t3 = b ^ c;     \
    t4 = a ^ t3;    \
    t5 = b | d;     \
    t7 = c | t1;    \
    h = t4 ^ t5;    \
    t8 = b ^ t7;    \
    t11 = ~t2;      \
    t9 = t4 & t8;   \
    f = t1 ^ t9;    \
    t13 = t9 ^ t11; \
    t12 = h & f;    \
    g = t12 ^ t13;  \
    t15 = a & d;    \
    t16 = c ^ t13;  \
    e = t15 ^ t16

/* 16 terms */

#define sb2(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = b ^ d;     \
    t3 = c & t1;    \
    t13 = d | t1;   \
    e = t2 ^ t3;    \
    t5 = c ^ t1;    \
    t6 = c ^ e;     \
    t7 = b & t6;    \
    t10 = e | t5;   \
    h = t5 ^ t7;    \
    t9 = d | t7;    \
    t11 = t9 & t10; \
    t14 = t2 ^ h;   \
    g = a ^ t11;    \
    t15 = g ^ t13;  \
    f = t14 ^ t15

/* 16 terms */

#define ib2(a,b,c,d,e,f,g,h)    \
    t1 = b ^ d;     \
    t2 = ~t1;       \
    t3 = a ^ c;     \
    t4 = c ^ t1;    \
    t7 = a | t2;    \
    t5 = b & t4;    \
    t8 = d ^ t7;    \
    t11 = ~t4;      \
    e = t3 ^ t5;    \
    t9 = t3 | t8;   \
    t14 = d & t11;  \
    h = t1 ^ t9;    \
    t12 = e | h;    \
    f = t11 ^ t12;  \
    t15 = t3 ^ t12; \
    g = t14 ^ t15

/* 17 terms */

#define sb3(a,b,c,d,e,f,g,h)    \
    t1 = a ^ c;     \
    t2 = d ^ t1;    \
    t3 = a & t2;    \
    t4 = d ^ t3;    \
    t5 = b & t4;    \
    g = t2 ^ t5;    \
    t7 = a | g;     \
    t8 = b | d;     \
    t11 = a | d;    \
    t9 = t4 & t7;   \
    f = t8 ^ t9;    \
    t12 = b ^ t11;  \
    t13 = g ^ t9;   \
    t15 = t3 ^ t8;  \
    h = t12 ^ t13;  \
    t16 = c & t15;  \
    e = t12 ^ t16

/* 16 term solution that performs less well than 17 term one
   in my environment (PPro/PII)                                  

#define sb3(a,b,c,d,e,f,g,h)    \
    t1 = a ^ b;     \
    t2 = a & c;     \
    t3 = a | d;     \
    t4 = c ^ d;     \
    t5 = t1 & t3;   \
    t6 = t2 | t5;   \
    g = t4 ^ t6;    \
    t8 = b ^ t3;    \
    t9 = t6 ^ t8;   \
    t10 = t4 & t9;  \
    e = t1 ^ t10;   \
    t12 = g & e;    \
    f = t9 ^ t12;   \
    t14 = b | d;    \
    t15 = t4 ^ t12; \
    h = t14 ^ t15
*/

/* 17 terms */

#define ib3(a,b,c,d,e,f,g,h)    \
    t1 = b ^ c;     \
    t2 = b | c;     \
    t3 = a ^ c;     \
    t7 = a ^ d;     \
    t4 = t2 ^ t3;   \
    t5 = d | t4;    \
    t9 = t2 ^ t7;   \
    e = t1 ^ t5;    \
    t8 = t1 | t5;   \
    t11 = a & t4;   \
    g = t8 ^ t9;    \
    t12 = e | t9;   \
    f = t11 ^ t12;  \
    t14 = a & g;    \
    t15 = t2 ^ t14; \
    t16 = e & t15;  \
    h = t4 ^ t16

/* 15 terms */

#define sb4(a,b,c,d,e,f,g,h)    \
    t1 = a ^ d;     \
    t2 = d & t1;    \
    t3 = c ^ t2;    \
    t4 = b | t3;    \
    h = t1 ^ t4;    \
    t6 = ~b;        \
    t7 = t1 | t6;   \
    e = t3 ^ t7;    \
    t9 = a & e;     \
    t10 = t1 ^ t6;  \
    t11 = t4 & t10; \
    g = t9 ^ t11;   \
    t13 = a ^ t3;   \
    t14 = t10 & g;  \
    f = t13 ^ t14

/* 17 terms */

#define ib4(a,b,c,d,e,f,g,h)    \
    t1 = c ^ d;     \
    t2 = c | d;     \
    t3 = b ^ t2;    \
    t4 = a & t3;    \
    f = t1 ^ t4;    \
    t6 = a ^ d;     \
    t7 = b | d;     \
    t8 = t6 & t7;   \
    h = t3 ^ t8;    \
    t10 = ~a;       \
    t11 = c ^ h;    \
    t12 = t10 | t11;\
    e = t3 ^ t12;   \
    t14 = c | t4;   \
    t15 = t7 ^ t14; \
    t16 = h | t10;  \
    g = t15 ^ t16

/* 16 terms */

#define sb5(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = a ^ b;     \
    t3 = a ^ d;     \
    t4 = c ^ t1;    \
    t5 = t2 | t3;   \
    e = t4 ^ t5;    \
    t7 = d & e;     \
    t8 = t2 ^ e;    \
    t10 = t1 | e;   \
    f = t7 ^ t8;    \
    t11 = t2 | t7;  \
    t12 = t3 ^ t10; \
    t14 = b ^ t7;   \
    g = t11 ^ t12;  \
    t15 = f & t12;  \
    h = t14 ^ t15

/* 16 terms */

#define ib5(a,b,c,d,e,f,g,h)    \
    t1 = ~c;        \
    t2 = b & t1;    \
    t3 = d ^ t2;    \
    t4 = a & t3;    \
    t5 = b ^ t1;    \
    h = t4 ^ t5;    \
    t7 = b | h;     \
    t8 = a & t7;    \
    f = t3 ^ t8;    \
    t10 = a | d;    \
    t11 = t1 ^ t7;  \
    e = t10 ^ t11;  \
    t13 = a ^ c;    \
    t14 = b & t10;  \
    t15 = t4 | t13; \
    g = t14 ^ t15

/* 15 terms */

#define sb6(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = a ^ d;     \
    t3 = b ^ t2;    \
    t4 = t1 | t2;   \
    t5 = c ^ t4;    \
    f = b ^ t5;     \
    t13 = ~t5;      \
    t7 = t2 | f;    \
    t8 = d ^ t7;    \
    t9 = t5 & t8;   \
    g = t3 ^ t9;    \
    t11 = t5 ^ t8;  \
    e = g ^ t11;    \
    t14 = t3 & t11; \
    h = t13 ^ t14

/* 15 terms */

#define ib6(a,b,c,d,e,f,g,h)    \
    t1 = ~a;        \
    t2 = a ^ b;     \
    t3 = c ^ t2;    \
    t4 = c | t1;    \
    t5 = d ^ t4;    \
    t13 = d & t1;   \
    f = t3 ^ t5;    \
    t7 = t3 & t5;   \
    t8 = t2 ^ t7;   \
    t9 = b | t8;    \
    h = t5 ^ t9;    \
    t11 = b | h;    \
    e = t8 ^ t11;   \
    t14 = t3 ^ t11; \
    g = t13 ^ t14

/* 17 terms */

#define sb7(a,b,c,d,e,f,g,h)    \
    t1 = ~c;        \
    t2 = b ^ c;     \
    t3 = b | t1;    \
    t4 = d ^ t3;    \
    t5 = a & t4;    \
    t7 = a ^ d;     \
    h = t2 ^ t5;    \
    t8 = b ^ t5;    \
    t9 = t2 | t8;   \
    t11 = d & t3;   \
    f = t7 ^ t9;    \
    t12 = t5 ^ f;   \
    t15 = t1 | t4;  \
    t13 = h & t12;  \
    g = t11 ^ t13;  \
    t16 = t12 ^ g;  \
    e = t15 ^ t16

/* 17 terms */

#define ib7(a,b,c,d,e,f,g,h)    \
    t1 = a & b;     \
    t2 = a | b;     \
    t3 = c | t1;    \
    t4 = d & t2;    \
    h = t3 ^ t4;    \
    t6 = ~d;        \
    t7 = b ^ t4;    \
    t8 = h ^ t6;    \
    t11 = c ^ t7;   \
    t9 = t7 | t8;   \
    f = a ^ t9;     \
    t12 = d | f;    \
    e = t11 ^ t12;  \
    t14 = a & h;    \
    t15 = t3 ^ f;   \
    t16 = e ^ t14;  \
    g = t15 ^ t16

#define k_xor(r,a,b,c,d)    \
    a ^= l_key[4 * r +  8]; \
    b ^= l_key[4 * r +  9]; \
    c ^= l_key[4 * r + 10]; \
    d ^= l_key[4 * r + 11]

#define k_set(r,a,b,c,d)    \
    a = l_key[4 * r +  8];  \
    b = l_key[4 * r +  9];  \
    c = l_key[4 * r + 10];  \
    d = l_key[4 * r + 11]

#define k_get(r,a,b,c,d)    \
    l_key[4 * r +  8] = a;  \
    l_key[4 * r +  9] = b;  \
    l_key[4 * r + 10] = c;  \
    l_key[4 * r + 11] = d

/* the linear transformation and its inverse    */

#define rot(a,b,c,d)    \
    a = rotl(a, 13);    \
    c = rotl(c, 3);     \
    d ^= c ^ (a << 3);  \
    b ^= a ^ c;         \
    d = rotl(d, 7);     \
    b = rotl(b, 1);     \
    a ^= b ^ d;         \
    c ^= d ^ (b << 7);  \
    a = rotl(a, 5);     \
    c = rotl(c, 22)

#define irot(a,b,c,d)   \
    c = rotr(c, 22);    \
    a = rotr(a, 5);     \
    c ^= d ^ (b << 7);  \
    a ^= b ^ d;         \
    d = rotr(d, 7);     \
    b = rotr(b, 1);     \
    d ^= c ^ (a << 3);  \
    b ^= a ^ c;         \
    c = rotr(c, 3);     \
    a = rotr(a, 13)

u4byte  l_key[140]; /* storage for the key schedule         */

/* initialise the key schedule from the user supplied key   */

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u4byte  i,lk,a,b,c,d,e,f,g,h;
    u4byte  t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16;

    if(key_len < 0 || key_len > 256)

        return (u4byte*)0;

    i = 0; lk = (key_len + 31) / 32;
    
    while(i < lk)
    {
#ifdef  BLOCK_SWAP
        l_key[i] = io_swap(in_key[lk - i - 1]);
#else
        l_key[i] = in_key[i];
#endif  
        i++;
    }

    if(key_len < 256)
    {
        while(i < 8)

            l_key[i++] = 0;

        i = key_len / 32; lk = 1 << key_len % 32; 

        l_key[i] = l_key[i] & (lk - 1) | lk;
    }

    for(i = 0; i < 132; ++i)
    {
        lk = l_key[i] ^ l_key[i + 3] ^ l_key[i + 5] 
                                ^ l_key[i + 7] ^ 0x9e3779b9 ^ i;

        l_key[i + 8] = (lk << 11) | (lk >> 21); 
    }

    k_set( 0,a,b,c,d);sb3(a,b,c,d,e,f,g,h);k_get( 0,e,f,g,h);
    k_set( 1,a,b,c,d);sb2(a,b,c,d,e,f,g,h);k_get( 1,e,f,g,h);
    k_set( 2,a,b,c,d);sb1(a,b,c,d,e,f,g,h);k_get( 2,e,f,g,h);
    k_set( 3,a,b,c,d);sb0(a,b,c,d,e,f,g,h);k_get( 3,e,f,g,h);
    k_set( 4,a,b,c,d);sb7(a,b,c,d,e,f,g,h);k_get( 4,e,f,g,h);
    k_set( 5,a,b,c,d);sb6(a,b,c,d,e,f,g,h);k_get( 5,e,f,g,h);
    k_set( 6,a,b,c,d);sb5(a,b,c,d,e,f,g,h);k_get( 6,e,f,g,h);
    k_set( 7,a,b,c,d);sb4(a,b,c,d,e,f,g,h);k_get( 7,e,f,g,h);
    k_set( 8,a,b,c,d);sb3(a,b,c,d,e,f,g,h);k_get( 8,e,f,g,h);
    k_set( 9,a,b,c,d);sb2(a,b,c,d,e,f,g,h);k_get( 9,e,f,g,h);
    k_set(10,a,b,c,d);sb1(a,b,c,d,e,f,g,h);k_get(10,e,f,g,h);
    k_set(11,a,b,c,d);sb0(a,b,c,d,e,f,g,h);k_get(11,e,f,g,h);
    k_set(12,a,b,c,d);sb7(a,b,c,d,e,f,g,h);k_get(12,e,f,g,h);
    k_set(13,a,b,c,d);sb6(a,b,c,d,e,f,g,h);k_get(13,e,f,g,h);
    k_set(14,a,b,c,d);sb5(a,b,c,d,e,f,g,h);k_get(14,e,f,g,h);
    k_set(15,a,b,c,d);sb4(a,b,c,d,e,f,g,h);k_get(15,e,f,g,h);
    k_set(16,a,b,c,d);sb3(a,b,c,d,e,f,g,h);k_get(16,e,f,g,h);
    k_set(17,a,b,c,d);sb2(a,b,c,d,e,f,g,h);k_get(17,e,f,g,h);
    k_set(18,a,b,c,d);sb1(a,b,c,d,e,f,g,h);k_get(18,e,f,g,h);
    k_set(19,a,b,c,d);sb0(a,b,c,d,e,f,g,h);k_get(19,e,f,g,h);
    k_set(20,a,b,c,d);sb7(a,b,c,d,e,f,g,h);k_get(20,e,f,g,h);
    k_set(21,a,b,c,d);sb6(a,b,c,d,e,f,g,h);k_get(21,e,f,g,h);
    k_set(22,a,b,c,d);sb5(a,b,c,d,e,f,g,h);k_get(22,e,f,g,h);
    k_set(23,a,b,c,d);sb4(a,b,c,d,e,f,g,h);k_get(23,e,f,g,h);
    k_set(24,a,b,c,d);sb3(a,b,c,d,e,f,g,h);k_get(24,e,f,g,h);
    k_set(25,a,b,c,d);sb2(a,b,c,d,e,f,g,h);k_get(25,e,f,g,h);
    k_set(26,a,b,c,d);sb1(a,b,c,d,e,f,g,h);k_get(26,e,f,g,h);
    k_set(27,a,b,c,d);sb0(a,b,c,d,e,f,g,h);k_get(27,e,f,g,h);
    k_set(28,a,b,c,d);sb7(a,b,c,d,e,f,g,h);k_get(28,e,f,g,h);
    k_set(29,a,b,c,d);sb6(a,b,c,d,e,f,g,h);k_get(29,e,f,g,h);
    k_set(30,a,b,c,d);sb5(a,b,c,d,e,f,g,h);k_get(30,e,f,g,h);
    k_set(31,a,b,c,d);sb4(a,b,c,d,e,f,g,h);k_get(31,e,f,g,h);
    k_set(32,a,b,c,d);sb3(a,b,c,d,e,f,g,h);k_get(32,e,f,g,h);

    return l_key;
};

/* encrypt a block of text  */

void encrypt(const u4byte in_blk[4], u4byte out_blk[])
{   u4byte  a,b,c,d,e,f,g,h;
    u4byte  t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16;
    
#ifdef  BLOCK_SWAP
    a = io_swap(in_blk[3]); b = io_swap(in_blk[2]); 
    c = io_swap(in_blk[1]); d = io_swap(in_blk[0]);
#else
    a = in_blk[0]; b = in_blk[1]; c = in_blk[2]; d = in_blk[3];
#endif

    k_xor( 0,a,b,c,d); sb0(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor( 1,e,f,g,h); sb1(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor( 2,a,b,c,d); sb2(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor( 3,e,f,g,h); sb3(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor( 4,a,b,c,d); sb4(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor( 5,e,f,g,h); sb5(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor( 6,a,b,c,d); sb6(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor( 7,e,f,g,h); sb7(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor( 8,a,b,c,d); sb0(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor( 9,e,f,g,h); sb1(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(10,a,b,c,d); sb2(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(11,e,f,g,h); sb3(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(12,a,b,c,d); sb4(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(13,e,f,g,h); sb5(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(14,a,b,c,d); sb6(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(15,e,f,g,h); sb7(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(16,a,b,c,d); sb0(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(17,e,f,g,h); sb1(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(18,a,b,c,d); sb2(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(19,e,f,g,h); sb3(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(20,a,b,c,d); sb4(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(21,e,f,g,h); sb5(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(22,a,b,c,d); sb6(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(23,e,f,g,h); sb7(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(24,a,b,c,d); sb0(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(25,e,f,g,h); sb1(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(26,a,b,c,d); sb2(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(27,e,f,g,h); sb3(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(28,a,b,c,d); sb4(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(29,e,f,g,h); sb5(e,f,g,h,a,b,c,d); rot(a,b,c,d); 
    k_xor(30,a,b,c,d); sb6(a,b,c,d,e,f,g,h); rot(e,f,g,h); 
    k_xor(31,e,f,g,h); sb7(e,f,g,h,a,b,c,d); k_xor(32,a,b,c,d); 
    
#ifdef  BLOCK_SWAP
    out_blk[3] = io_swap(a); out_blk[2] = io_swap(b); 
    out_blk[1] = io_swap(c); out_blk[0] = io_swap(d);
#else
    out_blk[0] = a; out_blk[1] = b; out_blk[2] = c; out_blk[3] = d;
#endif
};

/* decrypt a block of text  */

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  a,b,c,d,e,f,g,h;
    u4byte  t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16;
    
#ifdef  BLOCK_SWAP
    a = io_swap(in_blk[3]); b = io_swap(in_blk[2]); 
    c = io_swap(in_blk[1]); d = io_swap(in_blk[0]);
#else
    a = in_blk[0]; b = in_blk[1]; c = in_blk[2]; d = in_blk[3];
#endif

    k_xor(32,a,b,c,d); ib7(a,b,c,d,e,f,g,h); k_xor(31,e,f,g,h);
    irot(e,f,g,h); ib6(e,f,g,h,a,b,c,d); k_xor(30,a,b,c,d);
    irot(a,b,c,d); ib5(a,b,c,d,e,f,g,h); k_xor(29,e,f,g,h);
    irot(e,f,g,h); ib4(e,f,g,h,a,b,c,d); k_xor(28,a,b,c,d);
    irot(a,b,c,d); ib3(a,b,c,d,e,f,g,h); k_xor(27,e,f,g,h);
    irot(e,f,g,h); ib2(e,f,g,h,a,b,c,d); k_xor(26,a,b,c,d);
    irot(a,b,c,d); ib1(a,b,c,d,e,f,g,h); k_xor(25,e,f,g,h);
    irot(e,f,g,h); ib0(e,f,g,h,a,b,c,d); k_xor(24,a,b,c,d);
    irot(a,b,c,d); ib7(a,b,c,d,e,f,g,h); k_xor(23,e,f,g,h);
    irot(e,f,g,h); ib6(e,f,g,h,a,b,c,d); k_xor(22,a,b,c,d);
    irot(a,b,c,d); ib5(a,b,c,d,e,f,g,h); k_xor(21,e,f,g,h);
    irot(e,f,g,h); ib4(e,f,g,h,a,b,c,d); k_xor(20,a,b,c,d);
    irot(a,b,c,d); ib3(a,b,c,d,e,f,g,h); k_xor(19,e,f,g,h);
    irot(e,f,g,h); ib2(e,f,g,h,a,b,c,d); k_xor(18,a,b,c,d);
    irot(a,b,c,d); ib1(a,b,c,d,e,f,g,h); k_xor(17,e,f,g,h);
    irot(e,f,g,h); ib0(e,f,g,h,a,b,c,d); k_xor(16,a,b,c,d);
    irot(a,b,c,d); ib7(a,b,c,d,e,f,g,h); k_xor(15,e,f,g,h);
    irot(e,f,g,h); ib6(e,f,g,h,a,b,c,d); k_xor(14,a,b,c,d);
    irot(a,b,c,d); ib5(a,b,c,d,e,f,g,h); k_xor(13,e,f,g,h);
    irot(e,f,g,h); ib4(e,f,g,h,a,b,c,d); k_xor(12,a,b,c,d);
    irot(a,b,c,d); ib3(a,b,c,d,e,f,g,h); k_xor(11,e,f,g,h);
    irot(e,f,g,h); ib2(e,f,g,h,a,b,c,d); k_xor(10,a,b,c,d);
    irot(a,b,c,d); ib1(a,b,c,d,e,f,g,h); k_xor( 9,e,f,g,h);
    irot(e,f,g,h); ib0(e,f,g,h,a,b,c,d); k_xor( 8,a,b,c,d);
    irot(a,b,c,d); ib7(a,b,c,d,e,f,g,h); k_xor( 7,e,f,g,h);
    irot(e,f,g,h); ib6(e,f,g,h,a,b,c,d); k_xor( 6,a,b,c,d);
    irot(a,b,c,d); ib5(a,b,c,d,e,f,g,h); k_xor( 5,e,f,g,h);
    irot(e,f,g,h); ib4(e,f,g,h,a,b,c,d); k_xor( 4,a,b,c,d);
    irot(a,b,c,d); ib3(a,b,c,d,e,f,g,h); k_xor( 3,e,f,g,h);
    irot(e,f,g,h); ib2(e,f,g,h,a,b,c,d); k_xor( 2,a,b,c,d);
    irot(a,b,c,d); ib1(a,b,c,d,e,f,g,h); k_xor( 1,e,f,g,h);
    irot(e,f,g,h); ib0(e,f,g,h,a,b,c,d); k_xor( 0,a,b,c,d);
    
#ifdef  BLOCK_SWAP
    out_blk[3] = io_swap(a); out_blk[2] = io_swap(b); 
    out_blk[1] = io_swap(c); out_blk[0] = io_swap(d);
#else
    out_blk[0] = a; out_blk[1] = b; out_blk[2] = c; out_blk[3] = d;
#endif
};
