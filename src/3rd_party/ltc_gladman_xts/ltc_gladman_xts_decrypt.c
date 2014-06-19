// Description: XTS Implementation of Gladman Library Cyphers
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// This file is a modified version of tomlibcrypt's xts_decrypt.c, which
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

static int tweak_uncrypt(
   void *context,
   ltc_gladman_decrypt MethodDecrypt, 
   const unsigned char *C, unsigned char *P, unsigned char *T
//ltc-orig  symmetric_xts *xts
)
{
   unsigned long x;
   int err;

   err = CRYPT_OK;

   /* tweak encrypt block i */
#ifdef LTC_FAST
   for (x = 0; x < 16; x += sizeof(LTC_FAST_TYPE)) {
      *((LTC_FAST_TYPE*)&P[x]) = *((LTC_FAST_TYPE*)&C[x]) ^ *((LTC_FAST_TYPE*)&T[x]);
   }
#else
   for (x = 0; x < 16; x++) {
       P[x] = C[x] ^ T[x];
   }
#endif
     
//ltc-orig   err = cipher_descriptor[xts->cipher].ecb_decrypt(P, P, &xts->key1);  
   MethodDecrypt(context, P, P);

#ifdef LTC_FAST
   for (x = 0; x < 16; x += sizeof(LTC_FAST_TYPE)) {
      *((LTC_FAST_TYPE*)&P[x]) ^=  *((LTC_FAST_TYPE*)&T[x]);
   }
#else
   for (x = 0; x < 16; x++) {
       P[x] = P[x] ^ T[x];
   }
#endif

   /* LFSR the tweak */
   xts_mult_x(T);

   return err;
}   

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
)
{
   unsigned char PP[16], CC[16], T[16];
   unsigned long i, m, mo, lim;
   int           err;

   /* check inputs */
   LTC_ARGCHK(pt    != NULL);
   LTC_ARGCHK(ct    != NULL);
   LTC_ARGCHK(tweak != NULL);
//ltc-orig   LTC_ARGCHK(xts   != NULL);

//ltc-orig   /* check if valid */
//ltc-orig   if ((err = cipher_is_valid(xts->cipher)) != CRYPT_OK) {
//ltc-orig      return err;
//ltc-orig   }

   /* get number of blocks */
   m  = ptlen >> 4;
   mo = ptlen & 15;

   /* must have at least one full block */
   if (m == 0) {
      return CRYPT_INVALID_ARG;
   }

   /* encrypt the tweak */
//ltc-orig   if ((err = cipher_descriptor[xts->cipher].ecb_encrypt(tweak, T, &xts->key2)) != CRYPT_OK) {
//ltc-orig      return err;
//ltc-orig   }
   MethodSetKey(context, Key2, Key2Length, enc);
   MethodEncrypt(context, tweak, T);

   /* for i = 0 to m-2 do */
   if (mo == 0) {
      lim = m;
   } else {
      lim = m - 1;
   }

   MethodSetKey(context, Key1, Key1Length, dec); 
   for (i = 0; i < lim; i++) {
//ltc-orig      err = tweak_uncrypt(ct, pt, T, xts);
      err = tweak_uncrypt(context, MethodDecrypt, ct, pt, T);
      ct += 16;
      pt += 16;
   }
   
   /* if ptlen not divide 16 then */
   if (mo > 0) {
      XMEMCPY(CC, T, 16);
      xts_mult_x(CC);

      /* PP = tweak decrypt block m-1 */
//ltc-orig      if ((err = tweak_uncrypt(ct, PP, CC, xts)) != CRYPT_OK) {
      if ((err = tweak_uncrypt(context, MethodDecrypt, ct, PP, CC)) != CRYPT_OK) {
         return err;
      }

      /* Pm = first ptlen % 16 bytes of PP */
      for (i = 0; i < mo; i++) {
          CC[i]    = ct[16+i];
          pt[16+i] = PP[i];
      }
      for (; i < 16; i++) {
          CC[i] = PP[i];
      }

      /* Pm-1 = Tweak uncrypt CC */
//ltc-orig      if ((err = tweak_uncrypt(CC, pt, T, xts)) != CRYPT_OK) {
      if ((err = tweak_uncrypt(context, MethodDecrypt, CC, pt, T)) != CRYPT_OK) {
         return err;
      }
   }

   return CRYPT_OK;
}

#endif

/* $Source: /cvs/libtom/libtomcrypt/src/modes/xts/xts_decrypt.c,v $ */
/* $Revision: 1.5 $ */
/* $Date: 2007/05/12 14:05:56 $ */

