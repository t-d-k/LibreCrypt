// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEHashRIPEMD_H
#define _FreeOTFEHashRIPEMD_H   1

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
#define DRIVER_HASH_IMPL_VERSION_MAJOR     04
#define DRIVER_HASH_IMPL_VERSION_MINOR     00
#define DRIVER_HASH_IMPL_VERSION_BUILD   0000

#define DRIVER_HASH_IMPL_VERSION  (                                      \
                         (DRIVER_HASH_IMPL_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_HASH_IMPL_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_HASH_IMPL_VERSION_BUILD * 0x00000001)   \
                        )


#define HASHES_SUPPORTED     5


// NOTE: ASCII, not WCHAR
#define DRIVER_TITLE              "RIPEMD Family"
DEFINE_GUID(DRIVER_GUID,      0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x01);


// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_RMD128  "RIPEMD-128"
DEFINE_GUID(HASH_GUID_RMD128, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x02);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_RMD160  "RIPEMD-160"
DEFINE_GUID(HASH_GUID_RMD160, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x03);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_RMD160_2xA_LINUX  "RIPEMD-160 (Linux; Twice, with A)"
DEFINE_GUID(HASH_GUID_RMD160_2xA_LINUX, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x04);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_RMD256  "RIPEMD-256"
DEFINE_GUID(HASH_GUID_RMD256, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x05);

// NOTE: ASCII, not WCHAR
#define DRIVER_HASH_TITLE_RMD320  "RIPEMD-320"
DEFINE_GUID(HASH_GUID_RMD320, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x06);


// =========================================================================
// =========================================================================

#endif

