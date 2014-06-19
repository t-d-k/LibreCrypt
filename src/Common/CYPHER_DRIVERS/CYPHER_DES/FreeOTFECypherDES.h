// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherDES_H
#define _FreeOTFECypherDES_H   1

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


#define CYPHERS_SUPPORTED   2

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE                 "DES"
DEFINE_GUID(DRIVER_GUID,         0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x01);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_DES  "DES"
DEFINE_GUID(CIPHER_GUID_DES, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x02);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_3DES  "3DES"
DEFINE_GUID(CIPHER_GUID_3DES, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x03);


// =========================================================================
// Various internal functions

NTSTATUS
InitLTCDESCypher(
    OUT  int *cipher
);

NTSTATUS
InitLTC3DESCypher(
    OUT  int *cipher
);

// =========================================================================

#endif

