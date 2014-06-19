
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

// AES Algorithm Test - Generate local test files for KAT and Monte Carlo
// tests and optionally compare these files with a  reference set of test
// files
//
// usage: aes_gav /a:[12345] /t:[kec] [/c] [/h]
// where:
//         /a:[12345] algorithms to be tested
//         /t:[kec]   type of test
//         /c         compare output and reference file(s)
//         /h         use author's byte order (serpent)
// and:
//         1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish
//         k: generate ECB Known Answer Test files
//         e: generate ECB Monte Carlo Test files
//         c: generate CBC Monte Carlo Test files
//
// The reference files have a ".txt" extension while those generated use
// ".txt" if in a different directory or ".dat" otherwise. The directory
// paths for files are set below. Note that, when compared with the NIST
// test vector sequences, this code implements one additional (all zero)
// test vector as the first vector in each set (test 0).

#ifdef  __cplusplus
#   include <iostream>
#   include <fstream>
#else
#   include <stdio.h>
#endif

#include "aes_config.h"
#include "aes_defs.h"
#include "aes_aux.h"
#include "mars.h"
#include "rc6.h"
#include "rijndael.h"
#include "serpent.h"
#include "twofish.h"

#if KEEP_USER_HAPPY
#   define  OUT_DOTS(x)     \
    {                       \
        if(!(x % 40))       \
            con_char('\n'); \
        con_char('.');      \
    }
#else
#   define  OUT_DOTS(x)
#endif

// Serpent hack to get test agreement because the test vectors for this
// algorithm assemble the key vectors in the reverse direction in memory

bool    serpent_hack = false;

// Outputs a test vector file header

void header(OFREF outf, int type, char alg_name[])
{
    put_string(outf, "=======================================================");
    put_string(outf, "\nAuthor:    Dr B R Gladman (                           )");
    put_string(outf, "\nTest:      "); put_string(outf, type < 4 ? "ECB " : "CBC ");
    switch(type)
    {
        case  0:    put_string(outf, "Variable Key Known Answer Tests"); break;
        case  1:    put_string(outf, "Variable Text Known Answer Tests"); break;
        case  2:
        case  4:    put_string(outf, "Monte Carlo (Encryption) Tests"); break;
        case  3:
        case  5:    put_string(outf, "Monte Carlo (Decryption) Tests"); break;
    }

    put_string(outf, "\nAlgorithm: "); put_string(outf, alg_name);
    put_string(outf, "\nFilename:  "); put_string(outf, type < 4 ? "ecb_" : "cbc_");
    switch(type)
    {
        case  0:    put_string(outf, "vk.txt\n"); break;
        case  1:    put_string(outf, "vt.txt\n"); break;
        case  2:
        case  4:    put_string(outf, "e_m.txt\n"); break;
        case  3:
        case  5:    put_string(outf, "d_m.txt\n"); break;
    }
    put_string(outf, "=======================================================");
}

// Test of Electronic Code Book (ECB) Mode with Fixed Key and Variable Text

void ecb_vt(OFREF outf, AESREF alg)
{   u4byte      i, j;
    u1byte      pt[16], ct[16], key[32], dummy;

    block_clear(key, 32);   // all zero key

    for(i = 0; i < 3; ++i)  // key lengthes 128, 192 and 256
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        put_char(outf, '\n');

        block_out(3, key, outf, 16 + 8 * i);    // output key value

        alg.set_key(key, 128 + 64 * i, both);   // set key value

        for(j = 0; j <= 128; ++j)               // 129 test vectors including
        {                                       // an all zero one
            block_out(1, &dummy, outf, j);      // output test number
            block_clear(pt, 16);                // set all zero plain text

            if(j)                               // set bit (j-1) if j <> 0

                *(pt + (j - 1) / 8) = 0x80 >> (j - 1) % 8;

            block_out(4, pt, outf, 16);         // output plaintext

            if(serpent_hack)                    // reverse plaintext
                                                // block for Serpent
                block_reverse(pt, 16);

            alg.encrypt(pt , ct);               // do encryption

            if(serpent_hack)                    // reverse ciphertext
                                                // block for Serpent
                block_reverse(ct, 16);

            block_out(5, ct, outf, 16);         // output ciphertext
        }
    }
}

// Test of Electronic Code Book (ECB) Mode with Fixed Text and Variable Key

void ecb_vk(OFREF outf, AESREF alg)
{   u4byte      i, j;
    u1byte      pt[16], ct[16], key[32], dummy;

    block_clear(pt, 16);    // all zero plaintext

    for(i = 0; i < 3; ++i)  // key lengths of 128, 192 and 256 bits
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        put_char(outf, '\n');

        block_out(4, pt, outf, 16);         // output plaintext

        for(j = 0; j <= 128 + 64 * i; ++j)  // 129, 193 or 257 tests
        {
            block_out(1, &dummy, outf, j);  // output test number
            block_clear(key, 32);

            if(j)                           // set bit (j-1) if j <> 0

                *(key + (j - 1) / 8) = 0x80 >> (j - 1) % 8;

            block_out(3, key, outf, 16 + 8 * i);    // output key value

            if(serpent_hack)                // reverse key block for Serpent

                block_reverse(key, 16 + 8 * i);

            alg.set_key(key, 128 + 64 * i, enc);    // set key value
            alg.encrypt(pt , ct);                   // alg.encrypt

            if(serpent_hack)                // reverse ciphertext for Serpent

                block_reverse(ct, 16);

            block_out(5, ct, outf, 16);     // output ciphertext
        }
    }
}

// Monte Carlo Encryption Test of Electronic Code Book (ECB) Mode

void ecb_me(OFREF outf, AESREF alg)
{   u4byte  i, j, k;
    u1byte  pt[16], ct[32], key[32], dummy;

    for(i = 0; i < 3; ++i)  // key lengths of 128, 192 and 256 bits
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        block_clear(pt, 16); block_clear(key, 32);  // clear initial plaintext
                                                    // and key blocks
        block_copy(ct + 16, pt, 16);                // put plaintext in upper half
                                                    // of double length buffer
        for(j = 0; j < 400; j++)                    // 400 Monte Carlo tests
        {
            OUT_DOTS(j);

            block_out(1, &dummy, outf, j);      // output test number
            block_out(3, key, outf, 16 + 8 * i);// output key
            block_out(4, pt, outf, 16);         // output plaintext

            if(serpent_hack)    // reverse both input blocks for Serpent
            {
                block_reverse(key, 16 + 8 * i); block_reverse(ct + 16, 16);
            }

            alg.set_key(key, 128 + 64 * i, enc);    // set the key

            for(k = 0; k < 5000; ++k)       // 10000 encryptions alternating
            {                               // upper and lower blocks in ct
                alg.encrypt(ct + 16, ct); alg.encrypt(ct, ct + 16);
            }

            // compile next key as defined by NIST

            block_xor(key, ct + 16 - 8 * i, 16 + 8 * i);

            if(serpent_hack)    // put blocks in normal mode for Serpent
            {
                block_reverse(key, 16 + 8 * i); block_reverse(ct + 16, 16);
            }

            block_out(5, ct + 16, outf, 16);    // output ciphertext
            block_copy(pt, ct + 16, 16);        // copy cipertext as next plaintext
        }
    }
}

// Monte Carlo Decryption Test of Electronic Code Book (ECB) Mode

void ecb_md(OFREF outf, AESREF alg)
{   u4byte  i, j, k;
    u1byte  pt[16], ct[32], key[32], dummy;

    for(i = 0; i < 3; ++i)  // key lengths 128, 192 and 256 bits
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        block_clear(pt, 16);            // clear initial plaintext and key
        block_clear(key, 32);
        block_copy(ct + 16, pt, 16);    // copy plaintext into upper half
                                        // of double length ciphertext block
        for(j = 0; j < 400; j++)        // 400 Monte Carlo tests
        {
            OUT_DOTS(j);

            block_out(1, &dummy, outf, j);          // output test number
            block_out(3, key, outf, 16 + 8 * i);    // output key
            block_out(5, pt, outf, 16);             // output plaintext

            if(serpent_hack)            // reverse input blocks for Serpent
            {
                block_reverse(key, 16 + 8 * i); block_reverse(ct + 16, 16);
            }

            alg.set_key(key, 128 + 64 * i, dec);    // set key

            for(k = 0; k < 5000; ++k)   // 10000 decryptions alternating
            {                           // upper and lower blocks in ct
                alg.decrypt(ct + 16, ct); alg.decrypt(ct, ct + 16);
            }

            // compile next key as defined by NIST

            block_xor(key, ct + 16 - 8 * i, 16 + 8 * i);

            if(serpent_hack)    // put blocks in normal mode for Serpent
            {
                block_reverse(key, 16 + 8 * i); block_reverse(ct + 16, 16);
            }

            block_out(4, ct + 16, outf, 16);    // output ciphertext
            block_copy(pt, ct + 16, 16);        // set ciphertext as next plaintext
        }
    }
}

// Monte Carlo Encryption Test of Cipher Block Chaining (CBC) Mode

void cbc_me(OFREF outf, AESREF alg)
{   u4byte  i, j, k;
    u1byte  ct[32], key[32], dummy;

    for(i = 0; i < 3; ++i)  // key lengths 128, 192 and 256 bits
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        block_clear(key, 32);   // clear key: KEY[0]
        block_clear(ct, 32);    // clear ct:  PT[0], ct + 16: IV[0]

        for(j = 0; j < 400; j++)            // 400 Monte Carlo tests
        {
            OUT_DOTS(j);

            block_out(1, &dummy, outf, j);      // output test number
            block_out(3, key, outf, 16 + 8 * i);// output key
            block_out(2, ct + 16, outf, 16);    // output initialisation vector
            block_out(4, ct, outf, 16);         // output plaintext

            if(serpent_hack)            // reverse key block if Serpent
            {
                block_reverse(key, 16 + 8 * i);
            }

            alg.set_key(key, 128 + 64 * i, enc);    // set key

            for(k = 0; k < 5000; ++k)       // 10000 encryptions, two at a time
            {
                block_xor(ct, ct + 16, 16); // do CBC chaining

                if(serpent_hack)            // reverse input block for Serpent
                {
                    block_reverse(ct, 16);
                }

                alg.encrypt(ct, ct);        // do block encryption

                if(serpent_hack)            // reverse output block for Serpent
                {
                    block_reverse(ct, 16);
                }

                block_xor(ct + 16, ct, 16); // do CBC chaining

                if(serpent_hack)            // reverse input block for Serpent
                {
                    block_reverse(ct + 16, 16);
                }

                alg.encrypt(ct + 16, ct + 16);  // do block encryption

                if(serpent_hack)        // reverse output block for Serpent
                {
                    block_reverse(ct + 16, 16);
                }
            }

            block_out(5, ct + 16, outf, 16);    // output ciphertext

            if(serpent_hack)                    // reverse blocks for Serpent
            {
                block_reverse(ct, 16); block_reverse(ct + 16, 16);
            }

            // compile next key as defined by NIST

            block_xor(key, ct + 16 - 8 * i, 16 + 8 * i);

            if(serpent_hack)    // put blocks in normal order for Serpent
            {
                block_reverse(key, 16 + 8 * i);
                block_reverse(ct, 16); block_reverse(ct + 16, 16);
            }
        }
    }
}

// Monte Carlo Encryption Test of Cipher Block Chaining (CBC) Mode

void cbc_md(OFREF outf, AESREF alg)
{   u4byte  i, j, k;
    u1byte  pt[16], ct[32], key[32], dummy;

    for(i = 0; i < 3; ++i)  // key lengths 128, 192 and 256 bits
    {
        block_out(0, &dummy, outf, 128 + 64 * i);   // output key length;

        block_clear(key, 32);   // clear key: KEY[0]
        block_clear(ct, 32);    // clear ct: IV[0] ct + 16: CT[0]

        for(j = 0; j < 400; j++)            // 400 Monte Carlo tests
        {
            OUT_DOTS(j);

            block_out(1, &dummy, outf, j);      // output test number
            block_out(3, key, outf, 16 + 8 * i);// output key
            block_out(2, ct, outf, 16);         // output initialisation vector
            block_out(5, ct + 16, outf, 16);    // output ciphertext

            if(serpent_hack)            // reverse key block for Serpent
            {
                block_reverse(key, 16 + 8 * i);
            }

            alg.set_key(key, 128 + 64 * i, dec);    // set key

            for(k = 0; k < 5000; ++k)   // 10000 encryptions, two at a time
            {
                if(serpent_hack)        // reverse input block for Serpent
                {
                    block_reverse(ct + 16, 16);
                }

                alg.decrypt(ct + 16, pt);   // do block decryption

                if(serpent_hack)        // reverse input and output blocks
                {                       // for Serpent
                    block_reverse(ct + 16, 16); block_reverse(pt, 16);
                }

                block_xor(ct, pt, 16);  // do CBC chaining

                if(serpent_hack)        // reverse output block for Serpent
                {
                    block_reverse(ct, 16);
                }

                alg.decrypt(ct, pt);        // do block decryption

                if(serpent_hack)            // reverse input and output blocks
                {                           // for Serpent
                    block_reverse(ct, 16); block_reverse(pt, 16);
                }

                block_xor(ct + 16, pt, 16); // do CBC chaining
            }

            block_out(4, ct + 16, outf, 16);// output plaintext

            if(serpent_hack)    // reverse blocks for Serpent
            {
                block_reverse(ct, 16); block_reverse(ct + 16, 16);
            }

            // compile next key as defined by NIST

            block_xor(key, ct + 16 - 8 * i, 16 + 8 * i);

            if(serpent_hack)    // put blocks in normal order for Serpent
            {
                block_reverse(ct, 16); block_reverse(ct + 16, 16);
                block_reverse(key, 16 + 8 * i);
            }
        }
    }
}

// Synchronise two comparison files if they get out of step

int sync(int nbr, IFREF inf, char str[], bool outp)
{   int     ty, nn;

    for(;;)
    {
        ty = find_line(inf, str);

        if(ty < 0)

            return ty;

        if(ty == 1)
        {
            nn = get_dec(str + 2);

            if(nn >= nbr)

                return nn;
        }

        if(outp)
        {
            con_string("\n  "); con_string(str);
        }
    }
}

// Compare two test vector files

void comp_vecs(const char *fn1, const char *fn2)
{
    char            str1[128], str2[128], tstr[16];
    int             ty1, ty2, no1, no2, err_cnt, np_cnt;
    bool            req;
    IFILE           if1;
    IFILE           if2;

    err_cnt = np_cnt = 0; req = true;

    if(!(if1 = open_ifile(if1, fn1)))   // open first file
    {
        con_string("\n*** 1st file ("); con_string(fn1);
        con_string(") not found ***)"); return;
    }

    if(!(if2 = open_ifile(if2, fn2)))   // open second file
    {
        con_string("\n*** 2nd file ("); con_string(fn2);
        con_string(") not found ***"); return;
    }

    for(;;)         // while there is still input
    {
        if(req)     // if another line needs to be input
        {
            ty1 = find_line(if1, str1); ty2 = find_line(if2, str2);
        }

        if(ty1 < 0 && ty2 < 0)      // if end of file on both files

            break;

        if(ty1 < 0 || ty2 < 0)      // if end of file on one file
        {
            con_char('\n'); con_string(fn1);
            con_string(ty1 < 0 ? " short" : " long");
            con_string("er than "); con_string(fn2); break;
        }

        if(ty1 == 1)    // if 'test number' line in file 1

            no1 = get_dec(str1 + 2);

        if(ty2 == 1)    // if 'test number' line in file 2

            no2 = get_dec(str2 + 2);

        if(cmp_nocase(str1, str2) == 0)
        {
            req = true; continue;   // if lines are the same continue
        }

        if(ty1 == 1 && ty2 == 1)    // if not the same but both are at a
        {                           // 'test number' line
            np_cnt += abs(no2 - no1); req = false;

            if(no2 < no1)   // extra tests in file 2
            {
                con_string("\nextra test(s) in "); con_string(fn2);
                con_string(":\n  "); con_string(str2);
                no2 = sync(no1, if2, str2, np_cnt < 10); // skip tests in file 2
            }

            if(no1 < no2)   // extra test in file 1
            {
                con_string("\nextra test(s) in "); con_string(fn1);
                con_string(":\n  "); con_string(str1);
                no1 = sync(no2, if1, str1, np_cnt < 10);// skip tests in file 1
            }
        }
        else if(ty1 != ty2) // cannot synchronise test vector files
        {
            con_string("\n*** synchronisation error tests ");
            put_dec(tstr, no1); con_string(tstr);
            con_string(" and ");
            put_dec(tstr, no2); con_string(tstr);
            con_string(" ***");
            return;
        }
        else if(ty1 >= 0)   // test vector mismatch
        {
            con_string("\n*** mismatch error test ");
            put_dec(tstr, no1); con_string(tstr);
            con_string(" *** ");

            if(++err_cnt < 6)
            {
                con_string("\n  line 1: "); con_string(str1);
                con_string("\n  line 2: "); con_string(str2);
            }
        }
    }

    if(np_cnt && !err_cnt)  // all tests present match

        con_string("\nother tests match\n");

    else if(!err_cnt)       // some failed matches
    {
        con_char('\n'); con_string(fn1);
        con_string(" and "); con_string(fn2);
        con_string(" match\n");
    }

    close_ifile(if1); close_ifile(if2);
}

// array of functions to call for each test

typedef void (*f_p)(OFREF outf, AESREF alg);
f_p f_ptr[6] = { ecb_vk, ecb_vt, ecb_me, ecb_md, cbc_me, cbc_md };

// do the tests for each algorithm

void do_tests(bool do_cmp, bool ttype[3], AESREF alg)
{   char    a_name[16], name1[128], name2[128], *sp1, *sp2;
    int     i;
    OFILE   outf;

    copy_str(a_name, alg.name());
    con_string("\nGenerate and verify tests for "); con_string(a_name); con_char('\n');

    for(i = 0; i < 3; ++i)  // for each type of test /k /e /c (2 tests each)
    {
        if(ttype[i])        // if this test required
        {
            // name of file for output of generated test vectors

            sp1 = copy_str(name1, out_path);
            sp2 = copy_str(name2, ref_path);
            sp1 = copy_str(sp1, a_name);
            sp2 = copy_str(sp2, a_name);
            copy_str(sp1, aes_name[2 * i]);
            copy_str(sp2, aes_name[2 * i]);

            if(outf = open_ofile(outf, name1))      // if output file is open write it
            {
                header(outf, 2 * i, a_name);
                f_ptr[2 * i](outf, alg);
                put_char(outf, '\n'); close_ofile(outf);

                if(do_cmp)  // compare it with reference if required

                    comp_vecs(name2, name1);
            }

            // name of file for output and test vectors

            copy_str(sp1, aes_name[2 * i + 1]);
            copy_str(sp2, aes_name[2 * i + 1]);

            if(outf = open_ofile(outf, name1))      // if output file is open write it
            {
                header(outf, 2 * i + 1, a_name);
                f_ptr[2 * i + 1](outf, alg);
                put_char(outf, '\n'); close_ofile(outf);

                if(do_cmp)  // compare it with reference if required

                    comp_vecs(name2, name1);
            }
        }
    }
}

int main(int argc, char *argv[])
{   bool        do_cmp, tyf[3];

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

    if(argc == 1)
    {
        con_string("\nusage: aes_gav /a:[12345] /t:[kec] [/c] [/h]");
        con_string("\nwhere:");
        con_string("\n        /a:[12345] algorithms to be tested");
        con_string("\n        /t:[kec]   type of test");
        con_string("\n        /c         compare output and reference file(s)");
        con_string("\n        /h         use author's byte order (serpent)");
        con_string("\nwhere:");
        con_string("\n        1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish");
        con_string("\n        k: generate ECB Known Answer Test files");
        con_string("\n        e: generate ECB Monte Carlo Test files");
        con_string("\n        c: generate CBC Monte Carlo Test files");
        con_string("\n\n"); exit(0);
    }

    do_cmp = test_args(argc, argv, 'c', '\0');
    tyf[0] = test_args(argc, argv, 't', 'k');
    tyf[1] = test_args(argc, argv, 't', 'e');
    tyf[2] = test_args(argc, argv, 't', 'c');

    serpent_hack = false;

    if(test_args(argc, argv, 'a', '1'))

        do_tests(do_cmp, tyf, _mars);

    if(test_args(argc, argv, 'a', '2'))

        do_tests(do_cmp, tyf, _rc6);

    if(test_args(argc, argv, 'a', '3'))

        do_tests(do_cmp, tyf, _rijndael);

    if(test_args(argc, argv, 'a', '4'))
    {
        serpent_hack = test_args(argc, argv, 'h', '\0');

        do_tests(do_cmp, tyf, _serpent);

        serpent_hack = false;
    }

    if(test_args(argc, argv, 'a', '5'))

        do_tests(do_cmp, tyf, _twofish);

    return 0;
}
