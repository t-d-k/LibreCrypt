
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         MAGENTA by Deutche Telekom                                   */
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

/* Timing data for MAGENTA (magenta.c)

128 bit key:
Key Setup:      30 cycles
Encrypt:      6539 cycles =     3.9 mbits/sec
Decrypt:      6534 cycles =     3.9 mbits/sec
Mean:         6537 cycles =     3.9 mbits/sec

192 bit key:
Key Setup:      25 cycles
Encrypt:      6531 cycles =     3.9 mbits/sec
Decrypt:      6528 cycles =     3.9 mbits/sec
Mean:         6530 cycles =     3.9 mbits/sec

256 bit key:
Key Setup:      37 cycles
Encrypt:      8711 cycles =     2.9 mbits/sec
Decrypt:      8705 cycles =     2.9 mbits/sec
Mean:         8708 cycles =     2.9 mbits/sec

*/

#define LARGE_TABLES

#include "../std_defs.h"

static char *alg_name[] = { "magenta", "magenta.c", "magenta" };

char **cipher_name()
{
    return alg_name;
}

#define GF_POLY 0x0165;

u4byte  k_len;
u4byte  l_key[16];  /* storage for the key schedule         */

u1byte  f_tab[256];
u4byte  tab_init = 0;

#ifdef  LARGE_TABLES
u4byte  fl_tab[4][256];
#endif

void init_tab(void)
{   u4byte  i, f;

    f = 1;

    for(i = 0; i < 255; ++i)
    {
        f_tab[i] = (u1byte)f; 
        
#ifdef  LARGE_TABLES
        fl_tab[0][i] = f;
        fl_tab[1][i] = f <<  8;
        fl_tab[2][i] = f << 16;
        fl_tab[3][i] = f << 24;
#endif

        f <<= 1;

        if(f & 0x100)

            f ^= GF_POLY;
    }

    f_tab[255] = 0;

#ifdef  LARGE_TABLES
    fl_tab[0][255] = fl_tab[1][255] = 0;
    fl_tab[2][255] = fl_tab[3][255] = 0;
#endif

};

#ifdef  LARGE_TABLES

void pi_fun(u4byte y[4], const u4byte x[4])
{
    y[0] = fl_tab[0][byte(x[0],0) ^ f_tab[byte(x[2],0)]]
         | fl_tab[1][byte(x[2],0) ^ f_tab[byte(x[0],0)]]
         | fl_tab[2][byte(x[0],1) ^ f_tab[byte(x[2],1)]]
         | fl_tab[3][byte(x[2],1) ^ f_tab[byte(x[0],1)]];

    y[1] = fl_tab[0][byte(x[0],2) ^ f_tab[byte(x[2],2)]]
         | fl_tab[1][byte(x[2],2) ^ f_tab[byte(x[0],2)]]
         | fl_tab[2][byte(x[0],3) ^ f_tab[byte(x[2],3)]]
         | fl_tab[3][byte(x[2],3) ^ f_tab[byte(x[0],3)]];

    y[2] = fl_tab[0][byte(x[1],0) ^ f_tab[byte(x[3],0)]]
         | fl_tab[1][byte(x[3],0) ^ f_tab[byte(x[1],0)]]
         | fl_tab[2][byte(x[1],1) ^ f_tab[byte(x[3],1)]]
         | fl_tab[3][byte(x[3],1) ^ f_tab[byte(x[1],1)]];

    y[3] = fl_tab[0][byte(x[1],2) ^ f_tab[byte(x[3],2)]]
         | fl_tab[1][byte(x[3],2) ^ f_tab[byte(x[1],2)]]
         | fl_tab[2][byte(x[1],3) ^ f_tab[byte(x[3],3)]]
         | fl_tab[3][byte(x[3],3) ^ f_tab[byte(x[1],3)]];
};

#else

void pi_fun(u4byte y[4], const u4byte x[4])
{
    y[0] =  f_tab[byte(x[0],0) ^ f_tab[byte(x[2],0)]]
         | (f_tab[byte(x[2],0) ^ f_tab[byte(x[0],0)]] <<  8)
         | (f_tab[byte(x[0],1) ^ f_tab[byte(x[2],1)]] << 16)
         | (f_tab[byte(x[2],1) ^ f_tab[byte(x[0],1)]] << 24);

    y[1] =  f_tab[byte(x[0],2) ^ f_tab[byte(x[2],2)]]
         | (f_tab[byte(x[2],2) ^ f_tab[byte(x[0],2)]] <<  8)
         | (f_tab[byte(x[0],3) ^ f_tab[byte(x[2],3)]] << 16)
         | (f_tab[byte(x[2],3) ^ f_tab[byte(x[0],3)]] << 24);

    y[2] =  f_tab[byte(x[1],0) ^ f_tab[byte(x[3],0)]]
         | (f_tab[byte(x[3],0) ^ f_tab[byte(x[1],0)]] <<  8)
         | (f_tab[byte(x[1],1) ^ f_tab[byte(x[3],1)]] << 16)
         | (f_tab[byte(x[3],1) ^ f_tab[byte(x[1],1)]] << 24);

    y[3] =  f_tab[byte(x[1],2) ^ f_tab[byte(x[3],2)]]
         | (f_tab[byte(x[3],2) ^ f_tab[byte(x[1],2)]] <<  8)
         | (f_tab[byte(x[1],3) ^ f_tab[byte(x[3],3)]] << 16)
         | (f_tab[byte(x[3],3) ^ f_tab[byte(x[1],3)]] << 24);
};

#endif

void e3_fun(u4byte x[4])
{   u4byte  u[4],v[4];

    u[0] = x[0]; u[1] = x[1]; u[2] = x[2]; u[3] = x[3];

    pi_fun(v, u); pi_fun(u, v); pi_fun(v, u); pi_fun(u, v); 
    
    v[0] = byte(u[0], 0) | (byte(u[0], 2) << 8) | (byte(u[1], 0) << 16) | (byte(u[1], 2) << 24);
    v[1] = byte(u[2], 0) | (byte(u[2], 2) << 8) | (byte(u[3], 0) << 16) | (byte(u[3], 2) << 24);
    v[2] = byte(u[0], 1) | (byte(u[0], 3) << 8) | (byte(u[1], 1) << 16) | (byte(u[1], 3) << 24);
    v[3] = byte(u[2], 1) | (byte(u[2], 3) << 8) | (byte(u[3], 1) << 16) | (byte(u[3], 3) << 24);
    
    u[0] = x[0] ^ v[0]; u[1] = x[1] ^ v[1]; u[2] = x[2] ^ v[2]; u[3] = x[3] ^ v[3];

    pi_fun(v, u); pi_fun(u, v); pi_fun(v, u); pi_fun(u, v); 
    
    v[0] = byte(u[0], 0) | (byte(u[0], 2) << 8) | (byte(u[1], 0) << 16) | (byte(u[1], 2) << 24);
    v[1] = byte(u[2], 0) | (byte(u[2], 2) << 8) | (byte(u[3], 0) << 16) | (byte(u[3], 2) << 24);
    v[2] = byte(u[0], 1) | (byte(u[0], 3) << 8) | (byte(u[1], 1) << 16) | (byte(u[1], 3) << 24);
    v[3] = byte(u[2], 1) | (byte(u[2], 3) << 8) | (byte(u[3], 1) << 16) | (byte(u[3], 3) << 24);
    
    u[0] = x[0] ^ v[0]; u[1] = x[1] ^ v[1]; u[2] = x[2] ^ v[2]; u[3] = x[3] ^ v[3];

    pi_fun(v, u); pi_fun(u, v); pi_fun(v, u); pi_fun(u, v); 
    
    v[0] = byte(u[0], 0) | (byte(u[0], 2) << 8) | (byte(u[1], 0) << 16) | (byte(u[1], 2) << 24);
    v[1] = byte(u[2], 0) | (byte(u[2], 2) << 8) | (byte(u[3], 0) << 16) | (byte(u[3], 2) << 24);
    v[2] = byte(u[0], 1) | (byte(u[0], 3) << 8) | (byte(u[1], 1) << 16) | (byte(u[1], 3) << 24);
    v[3] = byte(u[2], 1) | (byte(u[2], 3) << 8) | (byte(u[3], 1) << 16) | (byte(u[3], 3) << 24);
    
    x[0] = v[0]; x[1] = v[1];
}; 

/* initialise the key schedule from the user supplied key   */

u4byte *set_key(const u4byte in_key[], const u4byte key_len)
{
    if(!tab_init)
    {
        init_tab(); tab_init = 1;
    }

    k_len = (key_len + 63) / 64;

    switch(k_len)
    {
        case 2:
            l_key[ 0] = in_key[0]; l_key[ 1] = in_key[1]; 
            l_key[ 2] = in_key[0]; l_key[ 3] = in_key[1]; 
            l_key[ 4] = in_key[2]; l_key[ 5] = in_key[3]; 
            l_key[ 6] = in_key[2]; l_key[ 7] = in_key[3]; 
            l_key[ 8] = in_key[0]; l_key[ 9] = in_key[1]; 
            l_key[10] = in_key[0]; l_key[11] = in_key[1]; 
            break;
        case 3:
            l_key[ 0] = in_key[0]; l_key[ 1] = in_key[1]; 
            l_key[ 2] = in_key[2]; l_key[ 3] = in_key[3]; 
            l_key[ 4] = in_key[4]; l_key[ 5] = in_key[5]; 
            l_key[ 6] = in_key[4]; l_key[ 7] = in_key[5]; 
            l_key[ 8] = in_key[2]; l_key[ 9] = in_key[3]; 
            l_key[10] = in_key[0]; l_key[11] = in_key[1]; 
            break;
        case 4:
            l_key[ 0] = in_key[0]; l_key[ 1] = in_key[1]; 
            l_key[ 2] = in_key[2]; l_key[ 3] = in_key[3]; 
            l_key[ 4] = in_key[4]; l_key[ 5] = in_key[5]; 
            l_key[ 6] = in_key[6]; l_key[ 7] = in_key[7]; 
            l_key[ 8] = in_key[6]; l_key[ 9] = in_key[7]; 
            l_key[10] = in_key[4]; l_key[11] = in_key[5]; 
            l_key[12] = in_key[2]; l_key[13] = in_key[3]; 
            l_key[14] = in_key[0]; l_key[15] = in_key[1]; 
    }

    return l_key;
};

#define r_fun(x,y,k)                \
    tt[0] = (y)[0]; tt[1] = (y)[1]; \
    tt[2] = (k)[0]; tt[3] = (k)[1]; \
    e3_fun(tt);                     \
    (x)[0] ^= tt[0]; (x)[1] ^= tt[1]

/* encrypt a block of text  */

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4], tt[4];

    blk[0] = in_blk[0]; blk[1] = in_blk[1];
    blk[2] = in_blk[2]; blk[3] = in_blk[3];
    
    r_fun(blk, blk + 2, l_key);
    r_fun(blk + 2, blk, l_key +  2);

    r_fun(blk, blk + 2, l_key +  4);
    r_fun(blk + 2, blk, l_key +  6);

    r_fun(blk, blk + 2, l_key +  8);
    r_fun(blk + 2, blk, l_key + 10);

    if(k_len == 4)
    {
        r_fun(blk, blk + 2, l_key + 12);
        r_fun(blk + 2, blk, l_key + 14);
    }

    out_blk[0] = blk[0]; out_blk[1] = blk[1];
    out_blk[2] = blk[2]; out_blk[3] = blk[3];
};

/* decrypt a block of text  */

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  blk[4], tt[4];

    blk[2] = in_blk[0]; blk[3] = in_blk[1];
    blk[0] = in_blk[2]; blk[1] = in_blk[3];

    r_fun(blk, blk + 2, l_key);
    r_fun(blk + 2, blk, l_key +  2);

    r_fun(blk, blk + 2, l_key +  4);
    r_fun(blk + 2, blk, l_key +  6);

    r_fun(blk, blk + 2, l_key +  8);
    r_fun(blk + 2, blk, l_key + 10);

    if(k_len == 4)
    {
        r_fun(blk, blk + 2, l_key + 12);
        r_fun(blk + 2, blk, l_key + 14);
    }

    out_blk[2] = blk[0]; out_blk[3] = blk[1];
    out_blk[0] = blk[2]; out_blk[1] = blk[3];
};
