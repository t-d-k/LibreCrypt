// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _DriverInterfaceHash_H
#define _DriverInterfaceHash_H   1

#include "FreeOTFEPlatform.h"
#include "FreeOTFE4PDAHashAPI.h"
#include "SDULinkedList.h"


// =========================================================================
// Constants....

#define STR_FORMAT_FULL_HASH_TITLE         TEXT("%ls")
#define STR_FORMAT_FULL_HASH_TECH_TITLE    TEXT("%ls (%d/%d)")


// =========================================================================
// Structures...

typedef struct _DLL_DETAILS_HASH {
    WCHAR* Filename;
    HINSTANCE Lib;

    PHashDLLFnIdentifyDriver     FnIdentifyDriver;
    PHashDLLFnIdentifySupported  FnIdentifySupported;
    PHashDLLFnGetHashDetails     FnGetHashDetails;
    PHashDLLFnHash               FnHash;
} DLL_DETAILS_HASH, *PDLL_DETAILS_HASH;


// =========================================================================
// Globals...
extern LNKLIST_ITEM* G_driver_CacheDLLDetailsHash;


// =========================================================================
// Functions...

BOOL driver_HashLoadDLL(WCHAR* Filename, DLL_DETAILS_HASH* DLLDetails);
void driver_HashUnloadDLL(DLL_DETAILS_HASH* DLLDetails);
// !!! CAUTION !!!
// Because this uses the indexes into the linked list, the cached load
// is NOT currently threadsafe
// To make threadsafe, change referencing by index, to walking through the
// linked list
BOOL driver_HashLoadDLLCached(WCHAR* Filename, DLL_DETAILS_HASH* DLLDetails);
void driver_HashUnloadDLLsCached();

BOOL driver_HashGetDriverDetails(WCHAR* HashFilename, HASH_DRIVER_INFO* driverInfo);

void driver_HashFreeDriverDetails(HASH_DRIVER_INFO* driverInfo);
BOOL
driver_HashGetImplDetails(
    WCHAR* HashFilename, 
    GUID* HashImpl, 
    HASH* ImplDetails
);

void driver_HashPrettyprintAlgTitle(
    HASH* hashInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);
void driver_HashPrettyprintAlgTechTitle(
    HASH* hashInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);
BOOL driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    WCHAR* buffer,
    int bufferSize  // In bytes
);
BOOL driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID_char(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    char* buffer,
    int bufferSize  // In bytes
);

BOOL driver_HashData(
    WCHAR* HashDriverFilename,
    GUID HashGUID,
    unsigned int BufferSizeIn,  // In *bits*
    unsigned char* BufferIn,
    unsigned int* ptrBufferSizeOut,  // In *bits*
    unsigned char* BufferOut
);

// As driver_HashData(...), but the driver is cached instead of beging
// free'd off after use
// USe driver_HashUnloadDLLsCached(...) to free off all cached drivers
BOOL driver_HashDataCached(
    WCHAR* HashDriverFilename,
    GUID HashGUID,
    unsigned int BufferSizeIn,  // In *bits*
    unsigned char* BufferIn,
    unsigned int* ptrBufferSizeOut,  // In *bits*
    unsigned char* BufferOut
);


// =========================================================================
// =========================================================================

#endif

