// Description: FreeOTFE Hash Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAHashAPI_H
#define _FreeOTFE4PDAHashAPI_H   1

#include "FreeOTFEPlatform.h"

#include "FreeOTFEHashAPICommon.h"

// Required for definition of ULONG
#include <windef.h>

// Following is required for the "_T" macro
/// #include <tchar.h>

//#ifdef FOTFE_PDA
//#include <objbase.h>
//#endif
//#include <initguid.h>  // Required for DEFINE_GUID and other GUID defintions


// =========================================================================
// Exported functions

// Note that the PC DLL version uses straight ASCII text, while the PDA
// version uses unicode

#ifdef FOTFE_PDA
#define DLLEXPORT_HASH_IDENTIFYDRIVER TEXT("HashIdentifyDriver")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_HASH_IDENTIFYDRIVER "HashIdentifyDriver"
#endif
typedef DWORD (* PHashDLLFnIdentifyDriver)(
                                 DIOC_HASH_IDENTIFYDRIVER*
                                );
DWORD
HashIdentifyDriver(
    DIOC_HASH_IDENTIFYDRIVER* Buffer
);


#ifdef FOTFE_PDA
#define DLLEXPORT_HASH_IDENTIFYSUPPORTED TEXT("HashIdentifySupported")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_HASH_IDENTIFYSUPPORTED "HashIdentifySupported"
#endif
typedef DWORD (* PHashDLLFnIdentifySupported)(
                                 DWORD,
                                 DIOC_HASH_IDENTIFYSUPPORTED*
                                );
DWORD
HashIdentifySupported(
    DWORD BufferSize,  // In bytes
    DIOC_HASH_IDENTIFYSUPPORTED* Buffer
);


#ifdef FOTFE_PDA
#define DLLEXPORT_HASH_GETHASHDETAILS TEXT("HashGetHashDetails")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_HASH_GETHASHDETAILS "HashGetHashDetails"
#endif
typedef DWORD (* PHashDLLFnGetHashDetails)(
                                 GUID*,
                                 HASH*
                                );
DWORD
HashGetHashDetails(
    IN     GUID* HashGUID,
    IN OUT HASH* HashDetails
);


#ifdef FOTFE_PDA
#define DLLEXPORT_HASH_HASH TEXT("HashHash")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_HASH_HASH "HashHash"
#endif
typedef DWORD (* PHashDLLFnHash)(
                          GUID*,
                          unsigned int,
                          FREEOTFEBYTE*,
                          unsigned int*, 
                          FREEOTFEBYTE*
                         );
DWORD
HashHash(
    GUID* HashGUID,
    unsigned int BufferSizeIn,
    FREEOTFEBYTE* BufferIn,
    unsigned int* ptrBufferSizeOut,
    FREEOTFEBYTE* BufferOut
);


// =========================================================================
// =========================================================================

#endif

