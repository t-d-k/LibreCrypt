// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _LUKSAFS_H
#define _LUKSAFS_H   1

#include "windef.h"  // Required for BOOL


// =========================================================================
// Functions...

BOOL LUKSAFS_AFSplit(
    int inputLength,
    BYTE* inputData,
    int n,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    int outputLength,  // Length of outputData buffer in bytes
    BYTE* outputData
);

BOOL LUKSAFS_AFMerge(
    BYTE* inputData,
    int inputLength,  // In bytes
    int n,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    int outputLength,  // Size of outputData buffer, in bytes
    BYTE* outputData  // This must be outputLength bytes long
);


// =========================================================================
// =========================================================================

#endif

