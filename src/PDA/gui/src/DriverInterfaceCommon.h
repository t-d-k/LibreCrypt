// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _DriverInterfaceCommon_H
#define _DriverInterfaceCommon_H   1

#include "FreeOTFE4PDADriverList.h"


// =========================================================================
// Constants...

#define STR_UNKNOWN  _("<Unknown>")
#define STR_NONE     _("<None>")
#define MAX_PRETTYPRINTED_SECTORIVGENMETHOD  30
#define MAX_PRETTYPRINTED_KDFALGORITHM       25
#define MAX_PRETTYPRINTED_MACALGORITHM       10

// Relativly arbitary value; must be large enough to store a prettyprinted
// hash/cypher title
#define MAX_PRETTYPRINTED_TITLE  1024

// =========================================================================
// Functions...

int driver_GetDriverFilenames(
    WCHAR* filenamePattern, 
    WCHAR*** filenamesList,
    LNKLIST_DISABLED_DRIVER* ExcludeFilenamesList
);

void driver_FreeDriverFilenames(WCHAR*** filenamesList);

WCHAR* driver_AllocMainDLLFullFilename(BOOL withPath);
void driver_FreeMainDLLFullFilename(WCHAR* fullFilename);
WCHAR* driver_AllocMainDLLFullPath(BOOL inclSlash);
void driver_FreeMainDLLFullPath(WCHAR* fullFilename);

// =========================================================================
// =========================================================================

#endif

