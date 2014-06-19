// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


// This file details the functions that the encryption algorithm MUST implement

#ifndef _FreeOTFEHashImpl_H
#define _FreeOTFEHashImpl_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <ntstatus.h>  // Required for NTSTATUS
#endif
#ifdef FOTFE_PC_DRIVER
#include <ntstatus.h>  // Required for NTSTATUS
#include <ntddk.h>
#endif

#include "FreeOTFEHashAPICommon.h"
//xxx - junk #include "FreeOTFEHashAPI.h"
//xxx - junk #include "FreeOTFEHashDriver.h"
//xxx - junk #include "FreeOTFEHashAPICommon.h"


// Hash driver init function
// devExt - The device extension to have it's "Driver identification and
//          hashes supported" members initialized
NTSTATUS
ImpHashDriverExtDetailsInit(
    IN OUT  HASH_DRIVER_INFO* infoBlock
);


// Hash driver cleardown function
// devExt - The device extension to have it's "Driver identification and
//          hashes supported" members cleared down
NTSTATUS
ImpHashDriverExtDetailsCleardown(
    IN OUT  HASH_DRIVER_INFO* infoBlock
);


// Hash data function
// Hashes "Data", setting "Hash" to the resulting hash
// DataLength - (in bits) The length of "Data"
// Data - The data to be hashed
// HashLength - (in bits) On calling this function, HashLength must be set to the 
//              length of the "Hash" buffer. This function will then change 
//              HashLength to be the size of the resulting hash
// Hash - This will be set to the resulting hash
NTSTATUS
ImpHashHashData(
    IN      GUID* HashGUID,
    IN      unsigned int DataLength,  // In bits
    IN      FREEOTFEBYTE* Data,
    IN OUT  unsigned int* HashLength,  // In bits
    OUT     FREEOTFEBYTE* Hash
);


// =========================================================================
// =========================================================================

#endif

