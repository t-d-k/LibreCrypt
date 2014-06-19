// Description: XTS Implementation of Gladman Library Cyphers
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// This file is a modified version of LibTomCrypt's xts_init.c, which
// has been changed to use the Gladman library cypher implementations


/* LibTomCrypt, modular cryptographic library -- Tom St Denis
 *
 * LibTomCrypt is a library that provides various cryptographic
 * algorithms in a highly modular and flexible manner.
 *
 * The library is free for all purposes without any express
 * guarantee it works.
 *
 * Tom St Denis, tomstdenis@gmail.com, http://libtom.org
 */
//ltc-orig #include "tomcrypt.h"
#include "ltc_gladman_xts.h"

/** 
  Source donated by Elliptic Semiconductor Inc (www.ellipticsemi.com) to the LibTom Projects
*/

#ifdef LTC_XTS_MODE


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
)
{
   int err;

   err = CRYPT_OK;

   /* check inputs */
   LTC_ARGCHK(key1  != NULL);
   LTC_ARGCHK(key2  != NULL);
//ltc-orig   LTC_ARGCHK(xts   != NULL);

//ltc-orig   /* check if valid */
//ltc-orig   if ((err = cipher_is_valid(cipher)) != CRYPT_OK) {
//ltc-orig      return err;
//ltc-orig   }

//ltc-orig   if (cipher_descriptor[cipher].block_length != 16) {
//ltc-orig      return CRYPT_INVALID_ARG;
//ltc-orig   }

//ltc-orig   /* schedule the two ciphers */
//ltc-orig   if ((err = cipher_descriptor[cipher].setup(key1, keylen, num_rounds, &xts->key1)) != CRYPT_OK) {
//ltc-orig      return err;
//ltc-orig   }
//ltc-orig   if ((err = cipher_descriptor[cipher].setup(key2, keylen, num_rounds, &xts->key2)) != CRYPT_OK) {
//ltc-orig      return err;
//ltc-orig   }
//ltc-orig   xts->cipher = cipher;

   return err;
}

#endif

/* $Source: /cvs/libtom/libtomcrypt/src/modes/xts/xts_init.c,v $ */
/* $Revision: 1.4 $ */
/* $Date: 2007/03/10 23:59:09 $ */

