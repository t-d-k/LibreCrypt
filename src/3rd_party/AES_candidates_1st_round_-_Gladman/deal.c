
/* This is an independent implementation of the encryption algorithm:   */
/*                                                                      */
/*         DEAL by Richard Outerbridge and Lars Knudsen                 */
/*                                                                      */
/* which is a candidate algorithm in the Advanced Encryption Standard   */
/* programme of the US National Institute of Standards and Technology.  */
/*                                                                      */
/* Copyright in this implementation is held by Dr B R Gladman but I     */
/* hereby give permission for its free direct or derivative use subject */
/* to acknowledgment of its origin and compliance with any conditions   */
/* that the originators of the algorithm place on its exploitation.     */
/*                                                                      */
/* Dr Brian Gladman  14th January 1999									*/

/* Timing Data for DEAL (deal.c)

With unmodified DES

128 bit key:
Key Setup:    8635 cycles
Encrypt:      2682 cycles =     9.5 mbits/sec
Decrypt:      2670 cycles =     9.6 mbits/sec
Mean:         2676 cycles =     9.6 mbits/sec

192 bit key:
Key Setup:    8651 cycles
Encrypt:      2677 cycles =     9.6 mbits/sec
Decrypt:      2669 cycles =     9.6 mbits/sec
Mean:         2673 cycles =     9.6 mbits/sec

256 bit key:
Key Setup:   11522 cycles
Encrypt:      3541 cycles =     7.2 mbits/sec
Decrypt:      3535 cycles =     7.2 mbits/sec
Mean:         3538 cycles =     7.2 mbits/sec

With modified DES

128 bit key:
Key Setup:    8635 cycles
Encrypt:      2339 cycles =    10.9 mbits/sec
Decrypt:      2365 cycles =    10.8 mbits/sec
Mean:         2352 cycles =    10.9 mbits/sec

192 bit key:
Key Setup:    8653 cycles
Encrypt:      2358 cycles =    10.9 mbits/sec
Decrypt:      2363 cycles =    10.8 mbits/sec
Mean:         2361 cycles =    10.8 mbits/sec

256 bit key:
Key Setup:   11698 cycles
Encrypt:      3115 cycles =     8.2 mbits/sec
Decrypt:      3088 cycles =     8.3 mbits/sec
Mean:         3102 cycles =     8.3 mbits/sec

*/

#define MODIFIED_DES

#include "../std_defs.h"
#include "../des/des.h"

#ifdef  MODIFIED_DES
  void des_ecm(const void *i_blk, void *o_blk, void *key);
#else
#  define   des_ecm des_ec
#endif

static char *alg_name[] = { "deal", "deal.c", "deal" };

char **cipher_name()
{
    return alg_name;
}

u4byte  ks_key[2] = { 0x67452301, 0xefcdab89 };

u4byte  ks_ks[32];  /* storage for the local key schedule   */
u4byte  ks_init = 0;

u4byte  k_len;      /* copy of key_length                   */ 
u4byte  l_key[256]; /* storage for key schedule             */

/* initialise the key schedule from the user supplied key   */

u4byte *set_key(const u4byte key_blk[], const u4byte key_len)
{   u4byte  lk[2];

    if(!ks_init)
    {
        des_ky(ks_key, ks_ks); ks_init = 1;
    }

    k_len = (key_len + 63) / 64;

    switch(k_len)
    {
        case 2:
            lk[0] = key_blk[0]; 
            lk[1] = key_blk[1]; 
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key);

            lk[0] ^= key_blk[2]; 
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key + 32);
              
            lk[0] ^= key_blk[0] ^ 0x00000080;  
            lk[1] ^= key_blk[1];
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key + 64);
              
            lk[0] ^= key_blk[2] ^ 0x00000040;  
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key + 96);
              
            lk[0] ^= key_blk[0] ^ 0x00000020;  
            lk[1] ^= key_blk[1];
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key + 128);
              
            lk[0] ^= key_blk[2] ^ 0x00000010;  
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key + 160);
            break;
        case 3:
            lk[0] = key_blk[0]; 
            lk[1] = key_blk[1]; 
            des_ec(lk, lk, ks_ks); 
            des_ky(lk, l_key);

            lk[0] ^= key_blk[2]; 
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 32);
              
            lk[0] ^= key_blk[4];  
            lk[1] ^= key_blk[5];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 64);
              
            lk[0] ^= key_blk[0] ^ 0x00000080;  
            lk[1] ^= key_blk[1];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 96);
              
            lk[0] ^= key_blk[2] ^ 0x00000040;  
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 128);
              
            lk[0] ^= key_blk[4] ^ 0x00000020;  
            lk[1] ^= key_blk[5];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 160);
            break;
        case 4:
            lk[0] = key_blk[0]; 
            lk[1] = key_blk[1]; 
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key);

            lk[0] ^= key_blk[2]; 
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 32);
              
            lk[0] ^= key_blk[4];  
            lk[1] ^= key_blk[5];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 64);
              
            lk[0] ^= key_blk[6];  
            lk[1] ^= key_blk[7];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 96);
              
            lk[0] ^= key_blk[0] ^ 0x00000080;  
            lk[1] ^= key_blk[1];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 128);
              
            lk[0] ^= key_blk[2] ^ 0x00000040;  
            lk[1] ^= key_blk[3];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 160);
            lk[0] ^= key_blk[4] ^ 0x00000020;  
            lk[1] ^= key_blk[5];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 192);
              
            lk[0] ^= key_blk[6] ^ 0x00000010;  
            lk[1] ^= key_blk[7];
            des_ec(lk, lk, ks_ks); des_ky(lk, l_key + 224);
            break;
    }   
    return l_key;
};

/* encrypt a block of text  */

void encrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  a[2], b[2], c[2], tt;

    a[0] = in_blk[0]; a[1] = in_blk[1];
    b[0] = in_blk[2]; b[1] = in_blk[3];
    
#ifdef  MODIFIED_DES
    ip(a[1], a[0]); ip(b[1], b[0]);
#endif

    des_ecm(a, c, l_key +   0); b[0] ^= c[0]; b[1] ^= c[1];
    des_ecm(b, c, l_key +  32); a[0] ^= c[0]; a[1] ^= c[1];

    des_ecm(a, c, l_key +  64); b[0] ^= c[0]; b[1] ^= c[1];
    des_ecm(b, c, l_key +  96); a[0] ^= c[0]; a[1] ^= c[1];

    des_ecm(a, c, l_key + 128); b[0] ^= c[0]; b[1] ^= c[1];
    des_ecm(b, c, l_key + 160); a[0] ^= c[0]; a[1] ^= c[1];

    if(k_len == 4)
    {
        des_ecm(a, c, l_key + 192); b[0] ^= c[0]; b[1] ^= c[1];
        des_ecm(b, c, l_key + 224); a[0] ^= c[0]; a[1] ^= c[1];

    }

#ifdef  MODIFIED_DES
    fp(a[1], a[0]); fp(b[1], b[0]);
#endif

    out_blk[0] = a[0]; out_blk[1] = a[1];
    out_blk[2] = b[0]; out_blk[3] = b[1];
};

/* decrypt a block of text  */

void decrypt(const u4byte in_blk[4], u4byte out_blk[4])
{   u4byte  a[2], b[2], c[2], tt;

    a[0] = in_blk[0]; a[1] = in_blk[1];
    b[0] = in_blk[2]; b[1] = in_blk[3];

#ifdef  MODIFIED_DES
    ip(a[1], a[0]); ip(b[1], b[0]);
#endif
     
    if(k_len == 4)
    {
        des_ecm(b, c, l_key + 224); a[0] ^= c[0]; a[1] ^= c[1];
        des_ecm(a, c, l_key + 192); b[0] ^= c[0]; b[1] ^= c[1];
    }

    des_ecm(b, c, l_key + 160); a[0] ^= c[0]; a[1] ^= c[1];
    des_ecm(a, c, l_key + 128); b[0] ^= c[0]; b[1] ^= c[1];

    des_ecm(b, c, l_key +  96); a[0] ^= c[0]; a[1] ^= c[1];
    des_ecm(a, c, l_key +  64); b[0] ^= c[0]; b[1] ^= c[1];

    des_ecm(b, c, l_key +  32); a[0] ^= c[0]; a[1] ^= c[1];
    des_ecm(a, c, l_key +   0); b[0] ^= c[0]; b[1] ^= c[1];

#ifdef  MODIFIED_DES
    fp(a[1], a[0]); fp(b[1], b[0]);
#endif

    out_blk[0] = a[0]; out_blk[1] = a[1];
    out_blk[2] = b[0]; out_blk[3] = b[1];
};
