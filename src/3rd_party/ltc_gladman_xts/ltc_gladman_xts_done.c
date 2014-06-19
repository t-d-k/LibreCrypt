// Description: XTS Implementation of Gladman Library Cyphers
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// This file is a modified version of tomlibcrypt's xts_done.c, which
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

/** Terminate XTS state 
   @param XTS    The state to terminate
*/
void xts_done(
//ltc-orig  symmetric_xts *xts
)
{
//ltc-orig   LTC_ARGCHKVD(xts != NULL);
//ltc-orig   cipher_descriptor[xts->cipher].done(&xts->key1);
//ltc-orig   cipher_descriptor[xts->cipher].done(&xts->key2);
}

#endif

/* $Source: /cvs/libtom/libtomcrypt/src/modes/xts/xts_done.c,v $ */
/* $Revision: 1.4 $ */
/* $Date: 2007/03/10 23:59:09 $ */

