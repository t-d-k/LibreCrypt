// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _DriverInterfaceCypher_H
#define _DriverInterfaceCypher_H   1

#include "FreeOTFEPlatform.h"
#include "FreeOTFE4PDACypherAPI.h"
#include "SDULinkedList.h"


// =========================================================================
// Constants....

#define STR_FORMAT_FULL_CYPHER_TITLE       TEXT("%ls (%d bit %ls)")
#define STR_FORMAT_FULL_CYPHER_TECH_TITLE  TEXT("%ls (%ls; %d/%d)")


// =========================================================================
// Structures...

typedef struct _DLL_DETAILS_CYPHER {
    WCHAR* Filename;
    HINSTANCE Lib;

    PCypherDLLFnIdentifyDriver           FnIdentifyDriver;
    PCypherDLLFnIdentifySupported_v1     FnIdentifySupported_v1;
    PCypherDLLFnIdentifySupported_v3     FnIdentifySupported_v3;
    PCypherDLLFnGetCypherDetails_v1      FnGetCypherDetails_v1;
    PCypherDLLFnGetCypherDetails_v3      FnGetCypherDetails_v3;
    PCypherDLLFnEncrypt                  FnEncrypt;
    PCypherDLLFnEncryptWithASCII         FnEncryptWithASCII;
    PCypherDLLFnDecrypt                  FnDecrypt;
    PCypherDLLFnDecryptWithASCII         FnDecryptWithASCII;
    PCypherDLLFnEncryptSector            FnEncryptSector;
    PCypherDLLFnEncryptSectorWithASCII   FnEncryptSectorWithASCII;
    PCypherDLLFnDecryptSector            FnDecryptSector;
    PCypherDLLFnDecryptSectorWithASCII   FnDecryptSectorWithASCII;
} DLL_DETAILS_CYPHER, *PDLL_DETAILS_CYPHER;


// =========================================================================
// Globals...
extern LNKLIST_ITEM* G_driver_CacheDLLDetailsCypher;


// =========================================================================
// Functions...

BOOL driver_CypherLoadDLL(WCHAR* Filename, DLL_DETAILS_CYPHER* DLLDetails);
void driver_CypherUnloadDLL(DLL_DETAILS_CYPHER* DLLDetails);
// !!! CAUTION !!!
// Because this uses the indexes into the linked list, the cached load
// is NOT currently threadsafe
// To make threadsafe, change referencing by index, to walking through the
// linked list
BOOL driver_CypherLoadDLLCached(WCHAR* Filename, DLL_DETAILS_CYPHER* DLLDetails);
void driver_CypherUnloadDLLsCached();

BOOL driver_CypherGetDriverDetails(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v3* driverInfo
);
void driver_CypherFreeDriverDetails(CYPHER_DRIVER_INFO_v3* driverInfo);
BOOL driver_CypherGetImplDetails(
    WCHAR* CypherFilename, 
    GUID* CypherImpl, 
    CYPHER_v3* ImplDetails
);

BOOL driver_CypherEncryptSectorData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* plaintextBuffer,
    FREEOTFEBYTE* encryptedBuffer
);

BOOL driver_CypherDecryptSectorData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* encryptedBuffer,
    FREEOTFEBYTE* plaintextBuffer
);

void driver_CypherPrettyprintAlgMode(
    CYPHER_MODE mode,
    WCHAR* buffer
);
void driver_CypherPrettyprintAlgTitle(
    CYPHER_v3* cypherInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);
void driver_CypherPrettyprintAlgTechTitle(
    CYPHER_v3* cypherInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);
BOOL driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    WCHAR* buffer,
    int bufferSize  // In bytes
);
BOOL driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID_char(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    char* buffer,
    int bufferSize  // In bytes
);

// =========================================================================
// =========================================================================

#endif

