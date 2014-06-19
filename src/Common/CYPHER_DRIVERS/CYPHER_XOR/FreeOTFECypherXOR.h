// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherXOR_H
#define _FreeOTFECypherXOR_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <objbase.h>  // Required before iniguid.h
#endif
// initguid.h required to initialize DEFINE_GUIDs 
//  - MUST BE INCLUDED EXACTLY ONCE IN THE EXECUTABLE
#include <initguid.h>  

#include "FreeOTFEPlatform.h"

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


#define CYPHERS_SUPPORTED   1

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE              "XOR"
DEFINE_GUID(DRIVER_GUID,      0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x01);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_XOR  "XOR"
DEFINE_GUID(CIPHER_GUID_XOR, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x02);


// =========================================================================
// Function headers

// Internal function to carry out actual XOR encryption
NTSTATUS
_xor_data(
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      int DataLength,  // In bytes
    IN      FREEOTFEBYTE* InData,
    OUT     FREEOTFEBYTE* OutData
);


// =========================================================================
// =========================================================================


#endif

