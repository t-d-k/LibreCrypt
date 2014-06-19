
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

// Measure the Encryption, Decryption and Key Setup Times for an AES
// Algorithms Using the Pentium Time Stamp Counter

// Dr Brian Gladman (                           )  2nd December 1999
//
// Permission for free use is hereby given provided that the origin 
// of this code is acknowledged. 
//  
//  usage: aes_tmr /a:[12345]
//  where:
//          /a:[12345] algorithms to be tested
//  and:
//          1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish

#ifdef  __cplusplus
#   include <iostream>
#   include <fstream>
#else
#   include <stdio.h>
#endif

#include "aes_defs.h"
#include "aes_aux.h"
#include "mars.h"
#include "rc6.h"
#include "rijndael.h"
#include "serpent.h"
#include "twofish.h"

#if defined(_M_IX86)

#   define  PROCESSOR   "PII"   // Processor
#   define  PRO_CLOCK   200     // Processor clock frequency in MHz

    void cycles(u4byte *rtn)    // read the Pentium Time Stamp Counter
    {    __asm   
        {
        _emit   0x0f            // complete pending operations
        _emit   0xa2
        _emit   0x0f            // read time stamp counter
        _emit   0x31
        mov     ebx,rtn
        mov     [ebx],eax
        _emit   0x0f            // complete pending operations
        _emit   0xa2
        }
    }

const unsigned int loops = 100; // number of timing loops

#elif defined(__arm)
 
#   define  PROCESSOR   "ARM"   // Processor
#   define  PRO_CLOCK   20      // Processor clock frequency in MHz

#  ifdef __thumb
#       define SemiSWI 0xAB     // Define Angel Semihosting SWI to be Thumb one
#   else
#       define SemiSWI 0x123456 // Define Angel Semihosting SWI to be ARM one
#   endif

    __swi(SemiSWI) unsigned int AngelSWI(unsigned op);

#   define GetMicroseconds() AngelSWI(0x88)
#   define GetCycles()       AngelSWI(0x89)

    void cycles(u4byte *rtn)        // read the ARM cycle count
    {
        *rtn = AngelSWI(0x89);
    }

const unsigned int loops = 1;   // number of timing loops

#endif

// measure cycles for an encryption call

u4byte e_cycles(const u4byte key_len, AESREF alg)
{   u1byte  pt[16], ct[16], key[32];
    u4byte  i, cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    block_rndfill(key, 32);

    // set up a random plain text

    block_rndfill(pt, 16);

    // do a set_key in case it is necessary

    alg.set_key(key, key_len, both); c1 = c2 = 0xffffffff;

    // do an encrypt to remove any 'first time through' effects

    alg.encrypt(pt, ct);

    for(i = 0; i < loops; ++i)
    {
        block_rndfill(pt, 16);

        // time one and two encryptions

        cycles(&cy0);
        alg.encrypt(pt, ct);
        cycles(&cy1);
        alg.encrypt(pt, ct);
        alg.encrypt(pt, ct);
        cycles(&cy2);

        cy2 -= cy1; cy1 -= cy0;     // time for one and two calls

        c1 = (c1 > cy1 ? cy1 : c1); // find minimum values over the loops

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1; // return one call timing
}

// measure cycles for a decryption call

u4byte d_cycles(const u4byte key_len, AESREF alg)
{   u1byte  pt[16], ct[16], key[32];
    u4byte  i, cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    block_rndfill(key, 32);

    // set up a random plain text

    block_rndfill(pt, 16);

    // do a set_key in case it is necessary

    alg.set_key(key, key_len, both); c1 = c2 = 0xffffffff;

    // do an decrypt to remove any 'first time through' effects

    alg.decrypt(pt, ct);
 
    for(i = 0; i < loops; ++i)
    {
        block_rndfill(pt, 16);

        // time one and two encryptions

        cycles(&cy0);
        alg.decrypt(pt, ct);
        cycles(&cy1);
        alg.decrypt(pt, ct);
        alg.decrypt(pt, ct);
        cycles(&cy2);

        cy2 -= cy1; cy1 -= cy0;     // time for one and two calls

        c1 = (c1 > cy1 ? cy1 : c1); // find minimum values over the loops

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1; // return one call timing
}

// measure cycles for a key setup

#pragma optimize("", off)       // VC++ optimise has problems here 

u4byte k_cycles(const u4byte key_len, AESREF alg, const enum dir_flag f)
{   u1byte  key[32];
    u4byte  i, cy0, cy1, cy2, c1, c2;

    // set up a random key of 256 bits

    block_rndfill(key, 32);

    // do an set_key to remove any 'first time through' effects

    alg.set_key(key, key_len, f); c1 = c2 = 0xffffffff;

    for(i = 0; i < loops; ++i)
    {
        block_rndfill(key, 32);

        // time one and two encryptions

        cycles(&cy0);
        alg.set_key(key, key_len, f);
        cycles(&cy1);
        alg.set_key(key, key_len, f);
        alg.set_key(key, key_len, f);
        cycles(&cy2);

        cy2 -= cy1; cy1 -= cy0;     // time for one and two calls

        c1 = (c1 > cy1 ? cy1 : c1); // find minimum values over the loops

        c2 = (c2 > cy2 ? cy2 : c2);
    }

    return c2 - c1; // return one call timing
}

#pragma optimize("", on)

static u4byte   kl[3] = { 128, 192, 256 };
STATIC u4byte   ekt[3];
STATIC u4byte   dkt[3];
STATIC u4byte   et[3];
STATIC u4byte   dt[3];

void output(OFREF outf, const inx)
{   char    tstr[16];
    u4byte  av;

    put_string(outf, "\n// "); put_dec(tstr, kl[inx]); put_string(outf, tstr); 
    put_string(outf, " Bit:");

    put_string(outf, "   Key Setup: "); put_dec(tstr, ekt[inx]); put_string(outf, tstr); 
    put_char(outf, '/'); put_dec(tstr, dkt[inx]); put_string(outf, tstr); 
    put_string(outf, " cycles");

    put_string(outf, "\n// Encrypt:   "); put_dec(tstr, et[inx]); put_string(outf, tstr); 
    put_string(outf, " cycles = "); av = (1280 * PRO_CLOCK) / et[inx]; put_dec(tstr, av / 10); 
    put_string(outf, tstr); put_char(outf, '.'); put_char(outf, (char)('0' + (av % 10))); 
    put_string(outf, " mbits/sec");

    put_string(outf, "\n// Decrypt:   "); put_dec(tstr, dt[inx]); put_string(outf, tstr); 
    put_string(outf, " cycles = "); av = (1280 * PRO_CLOCK) / dt[inx]; put_dec(tstr, av / 10); 
    put_string(outf, tstr); put_char(outf, '.'); put_char(outf, (char)('0' + (av % 10))); 
    put_string(outf, " mbits/sec");
#if(0)
    av = (et[inx] + dt[inx] + 1) >> 1;
    put_string(outf, "\n// Mean:      "); put_dec(tstr, av); put_string(outf, tstr); 
    put_string(outf, " cycles = "); av = (1280 * PRO_CLOCK) / av; put_dec(tstr, av / 10); 
    put_string(outf, tstr); put_char(outf, '.'); put_char(outf, (char)('0' + (av % 10))); 
    put_string(outf, " mbits/sec");
#endif
}

void time(AESREF alg)
{   u4byte  i;

    for(i = 0; i < 3; ++i)
    {
        ekt[i] = k_cycles(kl[i], alg, enc); 
        dkt[i] = k_cycles(kl[i], alg, dec); 
        et[i] = e_cycles(kl[i], alg); 
        dt[i] = d_cycles(kl[i], alg);
    }
}

void do_out(OFREF outf, AESREF alg)
{
    put_string(outf, "// Algorithm: "); put_string(outf, alg.name());
    output(outf, 0); output(outf, 1); output(outf, 2); put_char(outf, '\n');
}

void header(OFREF outf)
{   char    tstr[16];

    put_string(outf, "\n// Processor: "); put_string(outf, PROCESSOR);
    put_string(outf, " ("); put_dec(tstr, PRO_CLOCK); 
    put_string(outf, tstr); put_string(outf, " MHz)\n");
}

int main(int argc, char *argv[])
{   OFILE   outf;
#ifdef  __cplusplus
    mars        _mars;
    rc6         _rc6;
    rijndael    _rijndael;
    serpent     _serpent;
    twofish     _twofish;
#else
    alg_struct  _mars;
    alg_struct  _rc6;
    alg_struct  _rijndael;
    alg_struct  _serpent;
    alg_struct  _twofish;

    _mars.name = MARS(name);
    _mars.set_key = MARS(set_key);
    _mars.encrypt = MARS(encrypt);
    _mars.decrypt = MARS(decrypt);

    _rc6.name = RC6(name);
    _rc6.set_key = RC6(set_key);
    _rc6.encrypt = RC6(encrypt);
    _rc6.decrypt = RC6(decrypt);

    _rijndael.name = RIJNDAEL(name);
    _rijndael.set_key = RIJNDAEL(set_key);
    _rijndael.encrypt = RIJNDAEL(encrypt);
    _rijndael.decrypt = RIJNDAEL(decrypt);

    _serpent.name = SERPENT(name);
    _serpent.set_key = SERPENT(set_key);
    _serpent.encrypt = SERPENT(encrypt);
    _serpent.decrypt = SERPENT(decrypt);

    _twofish.name = TWOFISH(name);
    _twofish.set_key = TWOFISH(set_key);
    _twofish.encrypt = TWOFISH(encrypt);
    _twofish.decrypt = TWOFISH(decrypt);
#endif

    if(argc != 2 && argc != 3)
    {
        con_string("\nusage: aes_tmr /a:[12345]");
        con_string("\nwhere:");
        con_string("\n        /a:[12345] algorithms to be tested");
        con_string("\nand:");
        con_string("\n        1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish");
        con_string("\n\n"); exit(0);
    }

    outf = open_ofile(outf, argc == 3 ? argv[2] : "CON"); header(outf);

    if(test_args(argc, argv, 'a', '1'))
    {
        time(_mars); do_out(outf, _mars);
    }
    if(test_args(argc, argv, 'a', '2'))
    {
        time(_rc6); do_out(outf, _rc6);
    }
    if(test_args(argc, argv, 'a', '3'))
    {
        time(_rijndael); do_out(outf, _rijndael);
    }
    if(test_args(argc, argv, 'a', '4'))
    {
        time(_serpent); do_out(outf, _serpent);
    }
    if(test_args(argc, argv, 'a', '5'))
    {
        time(_twofish); do_out(outf, _twofish);
    }

    close_ofile(outf); return 0;
}
