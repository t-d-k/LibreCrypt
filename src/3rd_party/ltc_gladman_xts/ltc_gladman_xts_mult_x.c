// Description: XTS Implementation of Gladman Library Cyphers
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// This file is a modified version of tomlibcrypt's xts_mult_x.c, which
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

/** multiply by x 
  @param I      The value to multiply by x (LFSR shift)
*/
void xts_mult_x(unsigned char *I)
{
  int x;
  unsigned char t, tt;

  for (x = t = 0; x < 16; x++) {
     tt   = I[x] >> 7;
     I[x] = ((I[x] << 1) | t) & 0xFF;
     t    = tt;
  }
  if (tt) {
     I[0] ^= 0x87;
  } 
}

#endif

/* $Source: /cvs/libtom/libtomcrypt/src/modes/xts/xts_mult_x.c,v $ */
/* $Revision: 1.4 $ */
/* $Date: 2007/03/10 23:59:09 $ */

