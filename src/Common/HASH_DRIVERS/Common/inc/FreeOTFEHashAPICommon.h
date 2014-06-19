// Description: FreeOTFE Hash Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEHashAPITypes_H
#define _FreeOTFEHashAPITypes_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <WinDef.h>  // Required for ULONG
#endif
#ifdef FOTFE_PC_DLL     
#include <windows.h>  // Required for definition of GUID
#endif
#ifdef FOTFE_PC_DRIVER  
#include <ntdef.h>  // Required for ULONG and GUID
#endif


// =========================================================================
// Const definitions

#define MAX_HASH_TITLE 256


// Maximum hash length
// This should be set to the MAXIMUM hash length that the any hash may generate.
// This const is used when generating sector IVs for volumes which use the hash
// of the sector ID as the sector IV. It is used then in order to allocate a suitably sized
// buffer before hashing the sector ID.
// Note: This value is in *bits*
#define FREEOTFE_MAX_HASH_LENGTH 1024


// =========================================================================
// Type definitions

typedef struct _HASH {
    GUID HashGUID;
    char Title[MAX_HASH_TITLE];
    ULONG VersionID;
    int Length;  // In bits
    int BlockSize;  // In bits
} HASH, *PHASH;

typedef struct _HASH_DRIVER_INFO {
    // Identification...
    GUID DriverGUID;
    char DriverTitle[MAX_HASH_TITLE];
    ULONG DriverVersionID;

    // Hashes supported...
    unsigned int HashCount;
    HASH* HashDetails;  // This is a *pointer* to an *array* of "HashCount"
                        // HASH structures
} HASH_DRIVER_INFO, *PHASH_DRIVER_INFO;


typedef struct _DIOC_HASH_IDENTIFYDRIVER {    
    GUID DriverGUID;
    char Title[MAX_HASH_TITLE];
    ULONG VersionID;
    unsigned int CountHashes;
} DIOC_HASH_IDENTIFYDRIVER, *PDIOC_HASH_IDENTIFYDRIVER;


typedef struct _DIOC_HASH_IDENTIFYSUPPORTED {    
    unsigned int BufCount;  // The number of *elements* in the "Hashes" array
    HASH Hashes[1];  // Variable length
} DIOC_HASH_IDENTIFYSUPPORTED, *PDIOC_HASH_IDENTIFYSUPPORTED;


typedef struct _DIOC_HASH_DATA_IN {
    GUID HashGUID;
    unsigned int DataLength;  // In bits
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_HASH_DATA_IN, *PDIOC_HASH_DATA_IN;


// IF THIS IS CHANGED - remember to change the definition of how 
// "userBufferSizeBytes" is calculated
typedef struct _DIOC_HASH_DATA_OUT {
    unsigned int HashLength;  // In bits
    FREEOTFEBYTE Hash[1];  // Variable length
} DIOC_HASH_DATA_OUT, *PDIOC_HASH_DATA_OUT;


// This definition must be kept in sync with "ImpHashHashData"
typedef NTSTATUS (* PDataHashFn)(
                                 GUID*,
                                 unsigned int,
                                 FREEOTFEBYTE*,
                                 unsigned int*, 
                                 FREEOTFEBYTE*
                                );

// This definition must be kept in sync with "GetHashDetails"
typedef NTSTATUS (* PHashDetailsFn)(
                                    GUID*,
                                    HASH*
                                   );

typedef struct _DIOC_HASH_INTLDETAILS {
    PHashDetailsFn FnHashDetails;
    PDataHashFn FnHash;    
} DIOC_HASH_INTLDETAILS, *PDIOC_HASH_INTLDETAILS;


// =========================================================================
// =========================================================================

#endif

