// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEMACHash_H
#define _FreeOTFEMACHash_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFEHashAPICommon.h"


// =========================================================================
// Function headers

// Generate a MAC based on hashing
NTSTATUS
ImplMACHash(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      unsigned int DataLength,  // In bits
    IN      unsigned char* Data,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     unsigned char* MAC
);



// =========================================================================
// =========================================================================
// =========================================================================

#endif


