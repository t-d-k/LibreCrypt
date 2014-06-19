// Description: FreeOTFE Cypher Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherAPI_H
#define _FreeOTFECypherAPI_H   1

// FreeOTFEAPIConsts.h is included for the definition of DEVICE_FREEOTFE_ROOT, etc
#include "FreeOTFEAPIConsts.h"
#include "FreeOTFECypherAPICommon.h"


// =========================================================================
// Const definitions

#define DEVICE_CYPHER_BASE_NAME          L"\\FreeOTFECypher"

#define DEVICE_CYPHER_DIR_NAME           DEVICE_FREEOTFE_ROOT L"\\Cypher"
#define DEVICE_CYPHER_SYMLINK_DIR_NAME   DEVICE_SYMLINK_FREEOTFE_ROOT
#define DEVICE_CYPHER_SYMLINK_PREFIX     DEVICE_CYPHER_SYMLINK_DIR_NAME DEVICE_CYPHER_BASE_NAME



#define IOCTL_FreeOTFECypher_IDENTIFYDRIVER    CTL_CODE(             \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x800,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_IDENTIFYSUPPORTED_v1    CTL_CODE(       \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x801,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_INTLDETAILS_v1  CTL_CODE(               \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x802,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_ENCRYPT      CTL_CODE(                  \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x803,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_DECRYPT      CTL_CODE(                  \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x804,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_ENCRYPTSECTOR  CTL_CODE(                \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x805,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_DECRYPTSECTOR  CTL_CODE(                \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x806,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_IDENTIFYSUPPORTED_v3  CTL_CODE(         \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x807,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )

#define IOCTL_FreeOTFECypher_INTLDETAILS_v3  CTL_CODE(               \
                                                FILE_DEVICE_UNKNOWN, \
                                                0x808,               \
                                                METHOD_BUFFERED,     \
                                                FILE_READ_DATA       \
                                               )


// =========================================================================
// =========================================================================

#endif

