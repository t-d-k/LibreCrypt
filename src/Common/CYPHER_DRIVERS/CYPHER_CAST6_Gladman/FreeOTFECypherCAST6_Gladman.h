// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherCAST6_Gladman_H
#define _FreeOTFECypherCAST6_Gladman_H   1

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


#define CYPHERS_SUPPORTED   5

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE                 "CAST6 (Gladman library)"
DEFINE_GUID(DRIVER_GUID,         0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x01);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_CAST6_128_CBC  "CAST6"
DEFINE_GUID(CIPHER_GUID_CAST6_128_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x02);

#define DRIVER_CIPHER_TITLE_CAST6_160_CBC  "CAST6"
DEFINE_GUID(CIPHER_GUID_CAST6_160_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x03);

#define DRIVER_CIPHER_TITLE_CAST6_192_CBC  "CAST6"
DEFINE_GUID(CIPHER_GUID_CAST6_192_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x04);

#define DRIVER_CIPHER_TITLE_CAST6_224_CBC  "CAST6"
DEFINE_GUID(CIPHER_GUID_CAST6_224_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x05);

#define DRIVER_CIPHER_TITLE_CAST6_256_CBC  "CAST6"
DEFINE_GUID(CIPHER_GUID_CAST6_256_CBC, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x06);


// =========================================================================
// Various internal functions

#define CAST6_BLOCK_SIZE 128

// =========================================================================

#endif

