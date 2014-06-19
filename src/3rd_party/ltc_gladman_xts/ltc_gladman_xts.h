// Description: XTS Implementation of Gladman Library Cyphers
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// This file is based on tomlibcrypt API, modified to use the Gladman library
// cypher implementations


#ifndef _ltc_gladman_xts_H
#define _ltc_gladman_xts_H   1

#include "FreeOTFEPlatform.h"

#include "aes_defs.h"

#define LTC_XTS_MODE 1

#define LTC_ARGCHK 

#define CRYPT_OK           0
#define CRYPT_INVALID_ARG  1

#define XMEMCPY memcpy

typedef void (*ltc_gladman_set_key)(void *context, const u1byte in_key[], const u4byte key_bits, const enum dir_flag f);
typedef void (*ltc_gladman_encrypt)(void *context, const u1byte in_blk[16], u1byte out_blk[16]);
typedef void (*ltc_gladman_decrypt)(void *context, const u1byte in_blk[16], u1byte out_blk[16]);


/** Start XTS mode
   @param cipher      The index of the cipher to use
   @param key1        The encrypt key
   @param key2        The tweak encrypt key
   @param keylen      The length of the keys (each) in octets
   @param num_rounds  The number of rounds for the cipher (0 == default)
   @param xts         [out] XTS structure
   Returns CRYPT_OK upon success.
*/
int xts_start(
   void *context,
                              int  cipher,
              const unsigned char *key1, 
              const unsigned char *key2, 
                    unsigned long  keylen,
                              int  num_rounds
//ltc-orig                    symmetric_xts *xts
);


/** Terminate XTS state 
   @param XTS    The state to terminate
*/
void xts_done(
//ltc-orig  symmetric_xts *xts
);


/** XTS Encryption
  @param pt     [in]  Plaintext
  @param ptlen  Length of plaintext (and ciphertext)
  @param ct     [out] Ciphertext
  @param tweak  [in] The 128--bit encryption tweak (e.g. sector number)
  @param xts    The XTS structure
  Returns CRYPT_OK upon success
*/
int xts_encrypt(
   void *context,
   ltc_gladman_set_key MethodSetKey, 
   ltc_gladman_encrypt MethodEncrypt, 
   int Key1Length,  // In bits
   FREEOTFEBYTE* Key1,
   int Key2Length,  // In bits
   FREEOTFEBYTE* Key2,
   const unsigned char *pt, unsigned long ptlen,
         unsigned char *ct,
   const unsigned char *tweak
//ltc-orig  symmetric_xts *xts
);


/** XTS Decryption
  @param ct     [in] Ciphertext
  @param ptlen  Length of plaintext (and ciphertext)
  @param pt     [out]  Plaintext
  @param tweak  [in] The 128--bit encryption tweak (e.g. sector number)
  @param xts    The XTS structure
  Returns CRYPT_OK upon success
*/
int xts_decrypt(
   void *context,
   ltc_gladman_set_key MethodSetKey, 
   ltc_gladman_encrypt MethodEncrypt, 
   ltc_gladman_decrypt MethodDecrypt, 
   int Key1Length,  // In bits
   FREEOTFEBYTE* Key1,
   int Key2Length,  // In bits
   FREEOTFEBYTE* Key2,
   const unsigned char *ct, unsigned long ptlen,
         unsigned char *pt,
   const unsigned char *tweak
//ltc-orig  symmetric_xts *xts
);


/** multiply by x 
  @param I      The value to multiply by x (LFSR shift)
*/
void xts_mult_x(unsigned char *I);


#endif
