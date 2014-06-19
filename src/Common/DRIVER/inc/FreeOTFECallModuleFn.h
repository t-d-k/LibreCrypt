// Description: FreeOTFE Perform Module Function Call
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECallModuleFn_H
#define _FreeOTFECallModuleFn_H   1

#include "FreeOTFEPlatform.h"
#include "FreeOTFEContext.h"

#ifdef FOTFE_PDA
#include <winnt.h>  // Required for LARGE_INTEGER
#endif
#ifdef FOTFE_PC_DRIVER
// xxx - junk #include <winnt.h>  // Required for LARGE_INTEGER
#include <ntdef.h>  // Required for LARGE_INTEGER
// xxx - junk #include <ntddk.h>  // Required for PBYPE
#endif


// =========================================================================

// Call the relevant encryption function
NTSTATUS
BlockEncrypt(
    IN DEVICE_CONTEXT* DeviceContext,
    IN LARGE_INTEGER fileOffset,  // The offset within the *volume file* where the
                                  // data was obtained from
    IN FREEOTFEBYTE* plaintextBuffer,
    IN FREEOTFEBYTE* cyphertextBuffer
);

// Call the relevant decryption function
NTSTATUS
BlockDecrypt(
    IN DEVICE_CONTEXT* DeviceContext,
    IN LARGE_INTEGER fileOffset,  // The offset within the *volume file* where the
                                  // data was obtained from
    IN FREEOTFEBYTE* cyphertextBuffer,
    IN FREEOTFEBYTE* plaintextBuffer
);

// Call the relevant hash function
NTSTATUS
DataHash(
    IN      MODULE_DETAILS_HASH* hashModule,
    IN      unsigned int dataLength,  // In bits
    IN      FREEOTFEBYTE* dataBuffer,
    IN OUT  unsigned int* hashLength,  // In bits
    IN      FREEOTFEBYTE* hashBuffer    
);


// =========================================================================
// =========================================================================

#endif
