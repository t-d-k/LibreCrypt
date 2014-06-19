// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEMACHMAC_H
#define _FreeOTFEMACHMAC_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFEHashAPICommon.h"

// =========================================================================
// Function headers

// Generate HMAC
NTSTATUS
ImplMACHMAC(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      int tBits,  // In *bits*
    IN      unsigned int KLength,  // In bits
    IN      unsigned char* K,
    IN      unsigned int textLength,  // In bits
    IN      unsigned char* text,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     unsigned char* MAC
);



// =========================================================================
// =========================================================================
// =========================================================================

#endif


