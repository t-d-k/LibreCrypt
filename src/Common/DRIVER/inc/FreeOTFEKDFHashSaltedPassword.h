// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEKDFHashSaltedPassword_H
#define _FreeOTFEKDFHashSaltedPassword_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFEHashAPICommon.h"


// =========================================================================
// Function headers

// Generate a key based on the hash of the salted password
NTSTATUS
ImplKDFHashSaltedPassword(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      unsigned int Iterations,
    IN      unsigned int PasswordLength,  // In bits
    IN      unsigned char* Password,
    IN      unsigned int SaltLength,  // In bits
    IN      unsigned char* Salt,

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     unsigned char* DerivedKey
);



// =========================================================================
// =========================================================================
// =========================================================================

#endif


