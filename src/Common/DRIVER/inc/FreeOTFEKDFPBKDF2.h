// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEKDFPBKDF2_H
#define _FreeOTFEKDFPBKDF2_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFEHashAPICommon.h"


// =========================================================================
// Definitions
// If the key derivation function isn't given a target number of bits for
// the key it is to generate, it will default to his
// Note: This is the number of *bits* the derived key will have
#define DEFAULT_PBKDF2_KEY_SIZE 512


// =========================================================================
// Function headers

// Derive key
NTSTATUS
ImplKDFPBKDF2(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      unsigned int PLength,  // In bits
    IN      unsigned char* P,
    IN      unsigned int SLength,  // In bits
    IN      unsigned char* S,
    IN      unsigned int c,  // Iterations
    IN      int dkLenBits,  // In *bits*

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     unsigned char* DerivedKey
);


// This is the PBKDF2 PRF function "F"  
// The PRF used is HMAC
NTSTATUS
PBKDF2_F(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      unsigned int PLength,  // In bits
    IN      unsigned char* P,
    IN      unsigned int SLength,  // In bits
    IN      unsigned char* S,
    IN      unsigned int c,
    IN      unsigned int i,
    OUT     unsigned char* T_
);


// =========================================================================
// =========================================================================
// =========================================================================

#endif


