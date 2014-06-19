// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherTwofish_HifnCS_H
#define _FreeOTFECypherTwofish_HifnCS_H   1

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


#define CYPHERS_SUPPORTED   3

// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE                 "Twofish (Hi/fn and Counterpane Systems library)"
DEFINE_GUID(DRIVER_GUID,         0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x01);

// NOTE: ASCII, not WCHAR
#define DRIVER_CIPHER_TITLE_TWOFISH_128  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_128, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x02);

#define DRIVER_CIPHER_TITLE_TWOFISH_192  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_192, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x03);

#define DRIVER_CIPHER_TITLE_TWOFISH_256  "Twofish"
DEFINE_GUID(CIPHER_GUID_TWOFISH_256, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x04);


// =========================================================================
// =========================================================================

#endif

