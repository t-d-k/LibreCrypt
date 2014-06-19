// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherTwofish_ltc_H
#define _FreeOTFECypherTwofish_ltc_H   1

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
#define DRIVER_CYPHER_IMPL_VERSION_MAJOR     03
#define DRIVER_CYPHER_IMPL_VERSION_MINOR     00
#define DRIVER_CYPHER_IMPL_VERSION_BUILD   0000

#define DRIVER_CYPHER_IMPL_VERSION  (                                      \
                         (DRIVER_CYPHER_IMPL_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_CYPHER_IMPL_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_CYPHER_IMPL_VERSION_BUILD * 0x00000001)   \
                        )


#define CYPHERS_SUPPORTED   9

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE                 "Twofish (libtomcrypt library)"
DEFINE_GUID(DRIVER_GUID,         0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_TWOFISH_128_CBC  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_128_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x01);

#define DRIVER_CIPHER_TITLE_TWOFISH_192_CBC  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_192_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x02);

#define DRIVER_CIPHER_TITLE_TWOFISH_256_CBC  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_256_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x03);

#define DRIVER_CIPHER_TITLE_TWOFISH_128_LRW  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_128_LRW, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x01, 0x01);

#define DRIVER_CIPHER_TITLE_TWOFISH_192_LRW  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_192_LRW, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x01, 0x02);

#define DRIVER_CIPHER_TITLE_TWOFISH_256_LRW  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_256_LRW, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x01, 0x03);

#define DRIVER_CIPHER_TITLE_TWOFISH_128_XTS  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_128_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02, 0x01);

#define DRIVER_CIPHER_TITLE_TWOFISH_192_XTS  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_192_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02, 0x02);

#define DRIVER_CIPHER_TITLE_TWOFISH_256_XTS  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_256_XTS, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02, 0x03);


// =========================================================================
// Various internal functions

NTSTATUS
InitLTCCypher(
    OUT  int *cipher
);

// =========================================================================

#endif

