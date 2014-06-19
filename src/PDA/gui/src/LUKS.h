// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _LUKS_H
#define _LUKS_H   1

#include <windef.h>  // Required for BOOL
#include "LUKSAPI.h"
#include "DriverInterfaceTwo.h"

// =========================================================================
// Functions...

// Determine if the specified volume is a Linux LUKS volume
BOOL LUKS_IsLUKSVolume(WCHAR* volumeFilename);

BOOL LUKS_DumpLUKSDataToFile(
    MODULE_DETAILS_MAIN* mainDriver,

    WCHAR* filename, 
    unsigned char* userKey,
    BOOL baseIVCypherOnHashLength,

    WCHAR* dumpFilename
);

// Note: This will only populate the LUKS members of the header; it will not
//       populate the FreeOTFE driver details
BOOL LUKS_ReadLUKSHeader(
    WCHAR* filename, 
    LUKS_HEADER_EXT* LUKSHeaderExt
);

// If the hash and cypher members of the LUKS header passed in are populated,
// this function will populate struct with the the FreeOTFE driver details
// (e.g. driver name/GUID) that correspond to that hash/cypher
// Note: Must be called *after* ReadLUKSHeader(...)
BOOL LUKS_MapToFreeOTFE(
    BOOL baseIVCypherOnHashLength,
    LUKS_HEADER_EXT* LUKSHeader
);

BOOL LUKS_RecoverMasterKey(
    MODULE_DETAILS_MAIN* mainDriver,

    WCHAR* filename,
    unsigned char* userPassword,
    BOOL baseIVCypherOnHashLength,

    LUKS_HEADER_EXT* LUKSHeader,
    int* keySlot,
    BYTE** keyMaterial  // CALLER is responsible for freeing off the pointer this is set to 
                        // Buffer will be (LUKSHeader->key_bytes) long 
);


// =========================================================================
// =========================================================================

#endif

