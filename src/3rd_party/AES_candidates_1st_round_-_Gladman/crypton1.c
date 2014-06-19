
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         CRYPTON (v1) by Chae Hoon Lim of Future Systms Inc           */
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

/* Timing data for CRYPTON (v1.0) (crypton1.c)

128 bit key:
Key Setup:    744/1270 cycles (encrypt/decrypt)
Encrypt:       476 cycles =    53.8 mbits/sec
Decrypt:       470 cycles =    54.5 mbits/sec
Mean:          473 cycles =    54.1 mbits/sec

192 bit key:
Key Setup:    748/1284 cycles (encrypt/decrypt)
Encrypt:       469 cycles =    54.6 mbits/sec
Decrypt:       470 cycles =    54.5 mbits/sec
Mean:          470 cycles =    54.5 mbits/sec

256 bit key:
Key Setup:    784/1323 cycles (encrypt/decrypt)
Encrypt:       470 cycles =    54.5 mbits/sec
Decrypt:       469 cycles =    54.6 mbits/sec
Mean:          470 cycles =    54.5 mbits/sec

*/

#include "../std_defs.h"

static char *alg_name[] = { "crypton1", "crypton1.c", "" };

char **cipher_name()
{
    return alg_name;
}

#define msk(n)      ((0x000000ff >> n) * 0x01010101)
#define brotl(x,n)  ((((x) & msk(n)) << (n)) | (((x) & ~msk(n)) >> (8 - (n))))

#define gamma_tau(i)                                                    \
    b0[i] = (((u4byte)s_box[(i + 2) & 3][byte(b1[0],i)]      ) |        \
             ((u4byte)s_box[(i + 3) & 3][byte(b1[1],i)] <<  8) |        \
             ((u4byte)s_box[ i         ][byte(b1[2],i)] << 16) |        \
             ((u4byte)s_box[(i + 1) & 3][byte(b1[3],i)] << 24))

#define mb_0    0xcffccffc
#define mb_1    0xf33ff33f
#define mb_2    0xfccffccf
#define mb_3    0x3ff33ff3

#define row_perm(x)             \
    (     (x)      & mb_0) ^    \
    (rotl((x),  8) & mb_1) ^    \
    (rotl((x), 16) & mb_2) ^    \
    (rotl((x), 24) & mb_3)

//  rotl(row_perm(x), 8)    row_perm(rotr(x,8))

#define fr0(y,x,i,k)                                    \
    (y)[i] = s_tab[ (i)         ][byte((x)[0],i)] ^     \
             s_tab[((i) + 1) & 3][byte((x)[1],i)] ^     \
             s_tab[((i) + 2) & 3][byte((x)[2],i)] ^     \
             s_tab[((i) + 3) & 3][byte((x)[3],i)] ^ (k)

#define fr1(y,x,i,k)                                    \
    (y)[i] = s_tab[((i) + 2) & 3][byte((x)[0],i)] ^     \
             s_tab[((i) + 3) & 3][byte((x)[1],i)] ^     \
             s_tab[ (i)         ][byte((x)[2],i)] ^     \
             s_tab[((i) + 1) & 3][byte((x)[3],i)] ^ (k)

#define f0_rnd(kp)                                  \
    fr0(b1, b0, 0,(kp)[0]); fr0(b1, b0, 1,(kp)[1]); \
    fr0(b1, b0, 2,(kp)[2]); fr0(b1, b0, 3,(kp)[3])

#define f1_rnd(kp)                                  \
    fr1(b0, b1, 0,(kp)[0]); fr1(b0, b1, 1,(kp)[1]); \
    fr1(b0, b1, 2,(kp)[2]); fr1(b0, b1, 3,(kp)[3])

#define  mc0  0xacacacac
#define  mc1  0x59595959 
#define  mc2  0xb2b2b2b2
#define  mc3  0x65656565

u1byte  p0[16] = { 15, 14, 10,  1, 11,  5,  8, 13,  9,  3,  2,  7,  0,  6,  4, 12 };
u1byte  p1[16] = { 11, 10, 13,  7,  8, 14,  0,  5, 15,  6,  3,  4,  1,  9,  2, 12 };
u1byte ip0[16] = { 12,  3, 10,  9, 14,  5, 13, 11,  6,  8,  2,  4, 15,  7,  1,  0 };
u1byte ip1[16] = {  6, 12, 14, 10, 11,  7,  9,  3,  4, 13,  1,  0, 15,  2,  5,  8 };

u4byte  tab_gen = 0;
u1byte  s_box[4][256];
u4byte  s_tab[4][256];
u4byte  ce[52];
u4byte  cd[52];

u4byte l_key[104];
u4byte *e_key = l_key + 52;
u4byte *d_key = l_key;

void gen_tab(void)
{   u4byte  i, xl, xr, y, yl, yr;

    for(i = 0; i < 256; ++i)
    {
        xl = p1[i >> 4]; xr = p0[i & 15];

        yl  = (xl & 0x0e) ^ ((xl << 3) & 0x08) ^ ((xl >> 3) & 0x01)
            ^ ((xr << 1) & 0x0a) ^ ((xr << 2) & 0x04) 
            ^ ((xr >> 2) & 0x02) ^ ((xr >> 1) & 0x01);
            
        yr  = (xr & 0x0d) ^ ((xr << 1) & 0x04) ^ ((xr >> 1) & 0x02)
            ^ ((xl >> 1) & 0x05) ^ ((xl << 2) & 0x08)
            ^ ((xl << 1) & 0x02) ^ ((xl >> 2) & 0x01);
            
        y = ip0[yl] | (ip1[yr] << 4); 
        
        yr = ((y << 3) | (y >> 5)) & 255; xr = ((i << 3) | (i >> 5)) & 255;
        yl = ((y << 1) | (y >> 7)) & 255; xl = ((i << 1) | (i >> 7)) & 255;

        s_box[0][i]  = (u1byte)yl; s_box[1][i] = (u1byte)yr;
        s_box[2][xl] = (u1byte)y; s_box[3][xr] = (u1byte)y;

        s_tab[0][ i] = (yl * 0x01010101) & 0x3fcff3fc;
        s_tab[1][ i] = (yr * 0x01010101) & 0xfc3fcff3;
        s_tab[2][xl] = (y * 0x01010101) & 0xf3fc3fcf;
        s_tab[3][xr] = (y * 0x01010101) & 0xcff3fc3f;
    }

    xl = 0xa54ff53a;

    for(i = 0; i < 13; ++i)
    {
        ce[4 * i + 0] = xl ^ mc0; 
        ce[4 * i + 1] = xl ^ mc1; 
        ce[4 * i + 2] = xl ^ mc2; 
        ce[4 * i + 3] = xl ^ mc3; 

        yl = row_perm(i & 1 ? xl : rotr(xl,16));

        cd[4 * (12 - i) + 0] = yl ^ mc0;
        cd[4 * (12 - i) + 1] = rotl(yl, 24) ^ mc1;
        cd[4 * (12 - i) + 2] = rotl(yl, 16) ^ mc2;
        cd[4 * (12 - i) + 3] = rotl(yl,  8) ^ mc3;

        xl += 0x3c6ef372;
    }
};

/* initialise the key schedule from the user supplied key   */

u4byte  kp[4] = { 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f };
u4byte  kq[4] = { 0x9b05688c, 0x1f83d9ab, 0x5be0cd19, 0xcbbb9d5d };

#define h0_block(n,r0,r1)                           \
    e_key[4 * n +  8] = rotl(e_key[4 * n + 0], r0); \
    e_key[4 * n +  9] = rc ^ e_key[4 * n + 1];      \
    e_key[4 * n + 10] = rotl(e_key[4 * n + 2], r1); \
    e_key[4 * n + 11] = rc ^ e_key[4 * n + 3]

#define h1_block(n,r0,r1)                           \
    e_key[4 * n +  8] = rc ^ e_key[4 * n + 0];      \
    e_key[4 * n +  9] = rotl(e_key[4 * n + 1], r0); \
    e_key[4 * n + 10] = rc ^ e_key[4 * n + 2];      \
    e_key[4 * n + 11] = rotl(e_key[4 * n + 3], r1)

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{   u4byte  i, j, t0, t1, tu[4], tv[4], ek[8], dk[8];

    if(!tab_gen)
    {
        gen_tab(); tab_gen = 1;
    }

    tu[0] = tv[0] = tu[1] = tv[1] = tu[2] = tv[2] = tu[3] = tv[3] = 0;

    switch((key_len + 63) / 64)
    {
    case 4: tu[3] =  byte(in_key[6],0) 
                  | (byte(in_key[6],2) <<  8)
                  | (byte(in_key[7],0) << 16)
                  | (byte(in_key[7],2) << 24);
            tv[3] =  byte(in_key[6],1) 
                  | (byte(in_key[6],3) <<  8)
                  | (byte(in_key[7],1) << 16)
                  | (byte(in_key[7],3) << 24);
    case 3: tu[2] =  byte(in_key[4],0) 
                  | (byte(in_key[4],2) <<  8)
                  | (byte(in_key[5],0) << 16)
                  | (byte(in_key[5],2) << 24);
            tv[2] =  byte(in_key[4],1) 
                  | (byte(in_key[4],3) <<  8)
                  | (byte(in_key[5],1) << 16)
                  | (byte(in_key[5],3) << 24);
    case 2: tu[0] =  byte(in_key[0],0) 
                  | (byte(in_key[0],2) <<  8)
                  | (byte(in_key[1],0) << 16)
                  | (byte(in_key[1],2) << 24);
            tv[0] =  byte(in_key[0],1) 
                  | (byte(in_key[0],3) <<  8)
                  | (byte(in_key[1],1) << 16)
                  | (byte(in_key[1],3) << 24);

            tu[1] =  byte(in_key[2],0) 
                  | (byte(in_key[2],2) <<  8)
                  | (byte(in_key[3],0) << 16)
                  | (byte(in_key[3],2) << 24);
            tv[1] =  byte(in_key[2],1) 
                  | (byte(in_key[2],3) <<  8)
                  | (byte(in_key[3],1) << 16)
                  | (byte(in_key[3],3) << 24);
    }

    fr0(ek, tu, 0, 0); fr0(ek, tu, 1, 0);
    fr0(ek, tu, 2, 0); fr0(ek, tu, 3, 0);
    fr1(ek + 4, tv, 0, 0); fr1(ek + 4, tv, 1, 0);
    fr1(ek + 4, tv, 2, 0); fr1(ek + 4, tv, 3, 0);

    t0 = ek[0] ^ ek[1] ^ ek[2] ^ ek[3];
    t1 = ek[4] ^ ek[5] ^ ek[6] ^ ek[7];

    ek[0] ^= t1; ek[1] ^= t1; ek[2] ^= t1; ek[3] ^= t1;
    ek[4] ^= t0; ek[5] ^= t0; ek[6] ^= t0; ek[7] ^= t0;

    d_key[48] = ek[0] ^ ce[0];
    d_key[49] = ek[1] ^ ce[1];
    d_key[50] = ek[2] ^ ce[2];
    d_key[51] = ek[3] ^ ce[3];

    dk[0] = brotl(row_perm(rotr(ek[2],16)), 4);
    dk[1] = brotl(row_perm(rotr(ek[3],24)), 2);
    dk[2] = row_perm(rotr(ek[0],24));
    dk[3] = brotl(row_perm(ek[1]), 2);

    dk[4] = brotl(row_perm(ek[7]), 6);
    dk[5] = brotl(row_perm(rotr(ek[4],24)), 6);
    dk[6] = brotl(row_perm(ek[5]), 4);
    dk[7] = brotl(row_perm(rotr(ek[6],16)), 4);

    for(i = j = 0; i < 13; ++i, j += 4)
    {
        if(i & 1)
        {
            e_key[j]     = ek[4] ^ ce[j];
            e_key[j + 1] = ek[5] ^ ce[j + 1];
            e_key[j + 2] = ek[6] ^ ce[j + 2];
            e_key[j + 3] = ek[7] ^ ce[j + 3];

            t1 = ek[7]; ek[7] = rotl(ek[6], 16); ek[6] = rotl(ek[5],  8);
            ek[5] = brotl(ek[4], 2); ek[4] = brotl(t1, 2);
        }
        else
        {
            e_key[j]     = ek[0] ^ ce[j];
            e_key[j + 1] = ek[1] ^ ce[j + 1];
            e_key[j + 2] = ek[2] ^ ce[j + 2];
            e_key[j + 3] = ek[3] ^ ce[j + 3];

            t1 = ek[0]; 
            ek[0] = rotl(ek[1], 24); ek[1] = rotl(ek[2], 16);
            ek[2] = brotl(ek[3], 6); ek[3] = brotl(t1, 6);
        }
    }

    for(i = j = 0; i < 12; ++i, j += 4)
    {
        if(i & 1)
        {
            d_key[j]     = dk[4] ^ cd[j];
            d_key[j + 1] = dk[5] ^ cd[j + 1];
            d_key[j + 2] = dk[6] ^ cd[j + 2];
            d_key[j + 3] = dk[7] ^ cd[j + 3];

            t1 = dk[5]; 
            dk[5] = rotl(dk[6], 16); dk[6] = rotl(dk[7], 24); 
            dk[7] = brotl(dk[4], 6); dk[4] = brotl(t1, 6); 
        }
        else
        {
            d_key[j]     = dk[0] ^ cd[j];
            d_key[j + 1] = dk[1] ^ cd[j + 1];
            d_key[j + 2] = dk[2] ^ cd[j + 2];
            d_key[j + 3] = dk[3] ^ cd[j + 3];

            t1 = dk[2]; 
            dk[2] = rotl(dk[1],  8); dk[1] = rotl(dk[0], 16);
            dk[0] = brotl(dk[3], 2); dk[3] = brotl(t1, 2);
        }
    }

    e_key[48] = row_perm(rotr(e_key[48],16));
    e_key[49] = row_perm(rotr(e_key[49], 8));
    e_key[50] = row_perm(e_key[50]);
    e_key[51] = row_perm(rotr(e_key[51],24));

    return l_key;
};

/* encrypt a block of text  */

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  b0[4], b1[4];

    b0[0] = in_blk[0] ^ e_key[0]; b0[1] = in_blk[1] ^ e_key[1];
    b0[2] = in_blk[2] ^ e_key[2]; b0[3] = in_blk[3] ^ e_key[3];

    f0_rnd(e_key +  4); f1_rnd(e_key +  8);
    f0_rnd(e_key + 12); f1_rnd(e_key + 16);
    f0_rnd(e_key + 20); f1_rnd(e_key + 24);
    f0_rnd(e_key + 28); f1_rnd(e_key + 32);
    f0_rnd(e_key + 36); f1_rnd(e_key + 40);
    f0_rnd(e_key + 44);

    gamma_tau(0); gamma_tau(1); gamma_tau(2); gamma_tau(3);

    out_blk[0] = b0[0] ^ e_key[48]; out_blk[1] = b0[1] ^ e_key[49];
    out_blk[2] = b0[2] ^ e_key[50]; out_blk[3] = b0[3] ^ e_key[51];
};

/* decrypt a block of text  */

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  b0[4], b1[4];

    b0[0] = in_blk[0] ^ d_key[0]; b0[1] = in_blk[1] ^ d_key[1];
    b0[2] = in_blk[2] ^ d_key[2]; b0[3] = in_blk[3] ^ d_key[3];

    f0_rnd(d_key +  4); f1_rnd(d_key +  8);
    f0_rnd(d_key + 12); f1_rnd(d_key + 16);
    f0_rnd(d_key + 20); f1_rnd(d_key + 24);
    f0_rnd(d_key + 28); f1_rnd(d_key + 32);
    f0_rnd(d_key + 36); f1_rnd(d_key + 40);
    f0_rnd(d_key + 44);

    gamma_tau(0); gamma_tau(1); gamma_tau(2); gamma_tau(3);

    out_blk[0] = b0[0] ^ d_key[48]; out_blk[1] = b0[1] ^ d_key[49];
    out_blk[2] = b0[2] ^ d_key[50]; out_blk[3] = b0[3] ^ d_key[51];
};
