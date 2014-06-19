// Description: FreeOTFE Hash Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEHashAPI_H
#define _FreeOTFEHashAPI_H   1

// FreeOTFEAPIConsts.h is included for the definition of DEVICE_FREEOTFE_ROOT, etc
#include "FreeOTFEAPIConsts.h"
#include "FreeOTFEHashAPICommon.h"


// =========================================================================
// Const definitions

#define DEVICE_HASH_BASE_NAME          L"\\FreeOTFEHash"

#define DEVICE_HASH_DIR_NAME           DEVICE_FREEOTFE_ROOT L"\\Hash"
#define DEVICE_HASH_SYMLINK_DIR_NAME   DEVICE_SYMLINK_FREEOTFE_ROOT
#define DEVICE_HASH_SYMLINK_PREFIX     DEVICE_HASH_SYMLINK_DIR_NAME DEVICE_HASH_BASE_NAME



#define IOCTL_FREEOTFEHASH_IDENTIFYDRIVER    CTL_CODE(                     \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x800,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FREEOTFEHASH_IDENTIFYSUPPORTED    CTL_CODE(                     \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x801,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FREEOTFEHASH_HASHDATA    CTL_CODE(                     \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x802,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FREEOTFEHASH_INTLDETAILS  CTL_CODE(                     \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x803,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )


// =========================================================================
// =========================================================================

#endif

