// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEHashMD_H
#define _FreeOTFEHashMD_H   1

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


#define HASHES_SUPPORTED     3


// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE              "MD Family"
DEFINE_GUID(DRIVER_GUID,      0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x01);


// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_MD2     "MD2"
DEFINE_GUID(HASH_GUID_MD2,    0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x02);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_MD4     "MD4"
DEFINE_GUID(HASH_GUID_MD4,    0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x03);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_MD5     "MD5"
DEFINE_GUID(HASH_GUID_MD5,    0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x04);


// =========================================================================
// =========================================================================

#endif

