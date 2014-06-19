// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAlib_H
#define _FreeOTFE4PDAlib_H   1

#include "FreeOTFEPlatform.h"

#include <stdio.h>
#include <windows.h>  // Required for WCHAR
#include <Wincrypt.h>  // Required for CryptAcquireContext, etc


#define FREEOTFE_DEBUG_LEVEL_NOT_READ   0xDEADBEEF
// Read the debug level from the specified file
// Returns: The debug level, or FREEOTFE_DEBUG_LEVEL_NOT_READ on failure
DWORD ReadDebugLevelFromFile(WCHAR* Filename);

BOOL
GetMaxSizeFile(
    IN  HANDLE FileHandle,
    OUT PLARGE_INTEGER MaxSize
);

BOOL
GetFileSize_Filename(
    IN  WCHAR* Filename, 
    OUT PLARGE_INTEGER FileSize
);

// Return TRUE if the file exists, otherwise FALSE
BOOL
CheckFileExists(
    WCHAR* Filename
);

BOOL GenerateRNGDataMSCryptoAPI(
    int bytesRequired,
    FREEOTFEBYTE* randomData
);


// =========================================================================
// =========================================================================

#endif

