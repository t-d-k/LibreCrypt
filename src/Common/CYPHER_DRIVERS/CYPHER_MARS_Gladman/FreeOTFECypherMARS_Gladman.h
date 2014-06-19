// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherMARS_Gladman_H
#define _FreeOTFECypherMARS_Gladman_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <objbase.h>  // Required before iniguid.h
#endif
// initguid.h required to initialize DEFINE_GUIDs 
//  - MUST BE INCLUDED EXACTLY ONCE IN THE EXECUTABLE
#include <initguid.h>  

// =========================================================================
// Const definitions

// These values are in DECIMAL
#define DRIVER_CYPHER_IMPL_VERSION_MAJOR     05
#define DRIVER_CYPHER_IMPL_VERSION_MINOR     00
#define DRIVER_CYPHER_IMPL_VERSION_BUILD   0000

#define DRIVER_CYPHER_IMPL_VERSION  (                                      \
                         (DRIVER_CYPHER_IMPL_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_CYPHER_IMPL_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_CYPHER_IMPL_VERSION_BUILD * 0x00000001)   \
                        )


#define CYPHERS_SUPPORTED   6

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE                 "MARS (Gladman library)"
DEFINE_GUID(DRIVER_GUID,         0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x01);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_MARS_128_CBC  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_128_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x02);

#define DRIVER_CIPHER_TITLE_MARS_192_CBC  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_192_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x04);

#define DRIVER_CIPHER_TITLE_MARS_256_CBC  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_256_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x05);

#define DRIVER_CIPHER_TITLE_MARS_128_XTS  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_128_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x06);

#define DRIVER_CIPHER_TITLE_MARS_192_XTS  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_192_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x07);

#define DRIVER_CIPHER_TITLE_MARS_256_XTS  "MARS"
DEFINE_GUID(CIPHER_GUID_MARS_256_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x08);


// =========================================================================
// Various internal functions

#define MARS_BLOCK_SIZE 128

// =========================================================================

#endif

