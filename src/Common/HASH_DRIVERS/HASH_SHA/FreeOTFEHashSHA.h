// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEHashSHA_H
#define _FreeOTFEHashSHA_H   1

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
#define DRIVER_HASH_IMPL_VERSION_MAJOR     03
#define DRIVER_HASH_IMPL_VERSION_MINOR     00
#define DRIVER_HASH_IMPL_VERSION_BUILD   0000

#define DRIVER_HASH_IMPL_VERSION  (                                      \
                         (DRIVER_HASH_IMPL_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_HASH_IMPL_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_HASH_IMPL_VERSION_BUILD * 0x00000001)   \
                        )


#define HASHES_SUPPORTED     5


// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE              "SHA Family"
DEFINE_GUID(DRIVER_GUID,      0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x01);


// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_SHA1    "SHA-1"
DEFINE_GUID(HASH_GUID_SHA1,   0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x02);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_SHA224  "SHA-224"
DEFINE_GUID(HASH_GUID_SHA224, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x03);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_SHA256  "SHA-256"
DEFINE_GUID(HASH_GUID_SHA256, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x04);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_SHA384  "SHA-384"
DEFINE_GUID(HASH_GUID_SHA384, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x05);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_SHA512  "SHA-512"
DEFINE_GUID(HASH_GUID_SHA512, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x06);


// =========================================================================
// =========================================================================

#endif

