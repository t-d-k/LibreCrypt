
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

// AES Algorithm Test - Generate KAT and Monte Carlo test results and 
// compare the answers with the reference test vectors in a standard 
// set of test files
//
// usage: aes_rav /a:[12345] /t:[kec] [/h]
// where:
//         /a:[12345] algorithms to be included in the test
//         /t:[kec]   type of test
//         /h         use author's byte order (serpent)
// and:
//         1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish
//         k: run ECB Known Answer tests  
//         e: run ECB Monte Carlo tests
//         c: run CBC Monte Carlo tests
//
// Note that, when compared with the NIST test vector sequences, this code 
// implements one additional (all zero) test vector as the first vector in 
// each set (test 0).

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

enum test_type { ecb_vk, ecb_vt, ecb_me, ecb_md, cbc_me, cbc_md };

void ref_test(const char *in_file, const unsigned int it_cnt, enum test_type t_type, AESREF alg)
{   u4byte          i, kl, test_no, cnt, e_cnt;
    u1byte          key[32], pt[16], iv[16], ect[16], act[32];
    char            str[128], tstr[16];
    int             ty;
    IFILE           inf;

    con_string("\nTest file: "); con_string(in_file); con_string("\nStatus: \n");

    if(!(inf = open_ifile(inf, in_file)))   // reference file for test vectors
    {                                       // if file is not present
        con_string("error in running test\n"); return;
    }

    cnt = 0; e_cnt = test_no = 0;

    for(;;)                         // while there are tests
    {
        ty = find_line(inf, str);   // input a line

        if(ty < 0)                  // until end of file

            break;

        switch(ty)      // process line type
        {
          case 0:   kl = get_dec(str + 8); continue;        // key length
          case 1:   test_no = get_dec(str + 2); continue;   // test number
          case 2:   block_in(iv, str + 3); continue;        // init vector
          case 3:   block_in(key, str + 4); continue;       // key
          case 4:   block_in(pt, str + 3);                  // plaintext
                    if(t_type != ecb_md && t_type != cbc_md)
                        continue;
                    break;
          case 5:   block_in(ect, str + 3);                 // ciphertext
                    if(t_type == ecb_md || t_type == cbc_md)
                        continue;
                    break;
        }

        if(serpent_hack)
        
            block_reverse(key, kl / 8);

        alg.set_key(key, kl, both); // set the key

        if(it_cnt > 100)

            OUT_DOTS(test_no);

        if(t_type == ecb_md || t_type == cbc_md)
        {
            block_copy(act, ect, 16);           // encrypted text to low block

            if(t_type == cbc_md)                // CBC Monte Carlo decryption
            {
                block_copy(act + 16, iv, 16);   // IV to high block

                for(i = 0; i < it_cnt; i += 2)  // do decryptions two at a time
                {
                    if(serpent_hack)
                
                        block_reverse(act, 16);

                    alg.decrypt(act, ect);      // decrypt low block

                    if(serpent_hack)
                    {
                        block_reverse(act, 16); block_reverse(ect, 16);

                    }

                    block_xor(act + 16, ect, 16);// xor into high block

                    if(serpent_hack)
                
                        block_reverse(act + 16, 16);

                    alg.decrypt(act + 16, ect); // decrypt high block

                    if(serpent_hack)
                    {               
                        block_reverse(ect, 16); block_reverse(act + 16, 16);
                    }

                    block_xor(act, ect, 16);    // xor into low block
                }
            }
            else    // ECB Monte Carlo decryption 
            {
                if(serpent_hack)

                    block_reverse(act, 16);

                for(i = 0; i < it_cnt; ++i)
        
                    alg.decrypt(act, act);
        
                if(serpent_hack)

                    block_reverse(act, 16);
            }

            if(!block_cmp(pt, act, 16))
            {
                con_string("\n\ndecryption error on test "); 
                put_dec(tstr, test_no); con_string(tstr); e_cnt++;
            }

            if(t_type == ecb_md)    // test encryption if ECB mode
            {
                if(serpent_hack)

                    block_reverse(act, 16);

                for(i = 0; i < it_cnt; ++i)

                    alg.encrypt(act, act); 

                if(serpent_hack)

                    block_reverse(act, 16);

                if(!block_cmp(ect, act, 16))
                {   
                    con_string("\n\nencryption error on test ");
                    put_dec(tstr, test_no); con_string(tstr); e_cnt++;
                }
            }
        }
        else    // if(t_type == ecb_me || t_type == cbc_me || ecb_vk || ecb_vt)
        {
            if(t_type == cbc_me)                        // CBC Monte Carlo encryption
            {
                block_copy(act, iv, 16); 
                block_copy(act + 16, pt, 16);           // copy IV and plaintext

                for(i = 0; i < it_cnt; i += 2)
                {
                    block_xor(act + 16, act, 16);       // xor low block into high block

                    if(serpent_hack)

                        block_reverse(act + 16, 16);

                    alg.encrypt(act + 16, act + 16);    // encrypt high block

                    if(serpent_hack)

                        block_reverse(act + 16, 16);

                    block_xor(act, act + 16, 16);       // xor high block into low block

                    if(serpent_hack)

                        block_reverse(act, 16);

                    alg.encrypt(act, act);              // encrypt low block
        
                    if(serpent_hack)

                        block_reverse(act, 16);
                }
            }
            else    // ECB Monte Carlo encryption
            {
                block_copy(act, pt, 16);

                if(serpent_hack)

                    block_reverse(act, 16);

                for(i = 0; i < it_cnt; ++i)
        
                    alg.encrypt(act, act);

                if(serpent_hack)
                
                    block_reverse(act, 16);
            }

            if(!block_cmp(ect, act, 16))
            {
                    con_string("\n\nencryption error on test ");
                    put_dec(tstr, test_no); con_string(tstr); e_cnt++;
            }
        
            if(t_type != cbc_me)    // if ECB mode test decrytpion
            {
                if(serpent_hack)
                
                    block_reverse(act, 16);

                for(i = 0; i < it_cnt; ++i)

                    alg.decrypt(act, act); 

                if(serpent_hack)
                
                    block_reverse(act, 16);

                if(!block_cmp(pt, act, 16))
                {   
                    con_string("\n\ndecryption error on test ");
                    put_dec(tstr, test_no); con_string(tstr); e_cnt++;
                }
            }
        }
    }

    close_ifile(inf);

    if(e_cnt > 0)   // report any errors
    {
        put_dec(tstr, e_cnt); con_string("\n"); con_string(tstr);
        con_string(" errors during test\n");
    }
    else            // else report all is well

        con_string("\nall tests correct\n");
}

void do_tests(const bool vkt, const bool ecb, const bool cbc, AESREF alg)
{   char    path[128], *sp;

    con_string("\nRun tests for the "); con_string(alg.name()); con_string(" algorithm"); 

    sp = copy_str(path, ref_path);  sp = copy_str(sp, alg.name());
    
    if(vkt)
    {
        copy_str(sp, aes_name[0]); ref_test(path, 1, ecb_vk, alg);
        copy_str(sp, aes_name[1]); ref_test(path, 1, ecb_vt, alg);
    }

    if(ecb)
    {
        copy_str(sp, aes_name[2]);  ref_test(path, 10000, ecb_me, alg);
        copy_str(sp, aes_name[3]);  ref_test(path, 10000, ecb_md, alg);
    }

    if(cbc)
    {
        copy_str(sp, aes_name[4]);  ref_test(path, 10000, cbc_me, alg);
        copy_str(sp, aes_name[5]);  ref_test(path, 10000, cbc_md, alg);
    }
}

int main(int argc, char *argv[])
{   bool        vkt, ecb, cbc;
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
        con_string("\nusage: aes_rav /a:[12345] /t:[kec] [/h]");
        con_string("\nwhere:");
        con_string("\n        /a:[12345] algorithms to be tested");
        con_string("\n        /t:[kec]   type of test");
        con_string("\n        /h         use author's byte order (serpent)");
        con_string("\nand:");
        con_string("\n        1:mars, 2:rc6, 3:rijndael, 4:serpent, 5:twofish");
        con_string("\n        k: run ECB Known Answer tests");  
        con_string("\n        e: run ECB Monte Carlo tests");
        con_string("\n        c: run CBC Monte Carlo tests");
        con_string("\n\n"); exit(0);
    }

    vkt = test_args(argc, argv, 't', 'k');
    ecb = test_args(argc, argv, 't', 'e');
    cbc = test_args(argc, argv, 't', 'c');
    
    serpent_hack = false;

    if(test_args(argc, argv, 'a', '1'))

        do_tests(vkt, ecb, cbc, _mars);
    
    if(test_args(argc, argv, 'a', '2'))

        do_tests(vkt, ecb, cbc, _rc6);
    
    if(test_args(argc, argv, 'a', '3'))

        do_tests(vkt, ecb, cbc, _rijndael);
    
    if(test_args(argc, argv, 'a', '4'))
    {
        serpent_hack = test_args(argc, argv, 'h', '\0');
        do_tests(vkt, ecb, cbc, _serpent); serpent_hack = false;
    }

    if(test_args(argc, argv, 'a', '5'))

        do_tests(vkt, ecb, cbc, _twofish);

    return 0;
}
