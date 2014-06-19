// Description: FreeOTFE Cypher Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDACypherAPI_H
#define _FreeOTFE4PDACypherAPI_H   1

#include "FreeOTFEPlatform.h"

#include "FreeOTFECypherAPICommon.h"

// Required for definition of ULONG
#include <windef.h>

// Following is required for the "_T" macro
/// #include <tchar.h>

#include <objbase.h>


// =========================================================================
// Exported functions

// Note that the PC DLL version uses straight ASCII text, while the PDA
// version uses unicode

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_IDENTIFYDRIVER TEXT("CypherIdentifyDriver")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_IDENTIFYDRIVER "CypherIdentifyDriver"
#endif
typedef DWORD (* PCypherDLLFnIdentifyDriver)(
                                 DIOC_CYPHER_IDENTIFYDRIVER*
                                );
DWORD
CypherIdentifyDriver(
    DIOC_CYPHER_IDENTIFYDRIVER* Buffer
);


// Note: "_v1" missing to give backward compatibility
#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 TEXT("CypherIdentifySupported")
//#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 TEXT("CypherIdentifySupported_v1")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 "CypherIdentifySupported"
//#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 "CypherIdentifySupported_v1"
#endif
typedef DWORD (* PCypherDLLFnIdentifySupported_v1)(
                                 unsigned int,
                                 DIOC_CYPHER_IDENTIFYSUPPORTED_v1*
                                );
DWORD
CypherIdentifySupported_v1(
    unsigned int BufferSize,  // In bytes
    DIOC_CYPHER_IDENTIFYSUPPORTED_v1* Buffer
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3 TEXT("CypherIdentifySupported_v3")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3 "CypherIdentifySupported_v3"
#endif
typedef DWORD (* PCypherDLLFnIdentifySupported_v3)(
                                 unsigned int,
                                 DIOC_CYPHER_IDENTIFYSUPPORTED_v3*
                                );
DWORD
CypherIdentifySupported_v3(
    unsigned int BufferSize,  // In bytes
    DIOC_CYPHER_IDENTIFYSUPPORTED_v3* Buffer
);

// Note: "_v1" missing to give backward compatibility
#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 TEXT("CypherGetCypherDetails")
//#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 TEXT("CypherGetCypherDetails_v1")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 "CypherGetCypherDetails"
//#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 "CypherGetCypherDetails_v1"
#endif
typedef DWORD (* PCypherDLLFnGetCypherDetails_v1)(
                                 GUID*,
                                 CYPHER_v1*
                                );
DWORD
CypherGetCypherDetails_v1(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v1* CypherDetails
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3 TEXT("CypherGetCypherDetails_v3")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3 "CypherGetCypherDetails_v3"
#endif
typedef DWORD (* PCypherDLLFnGetCypherDetails_v3)(
                                 GUID*,
                                 CYPHER_v3*
                                );
DWORD
CypherGetCypherDetails_v3(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v3* CypherDetails
);


#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_ENCRYPT TEXT("CypherEncrypt")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_ENCRYPT "CypherEncrypt"
#endif
typedef DWORD (* PCypherDLLFnEncrypt)(
                            DWORD,
                            DIOC_CYPHER_DATA_IN*,
                            DWORD,
                            DIOC_CYPHER_DATA_OUT* 
                         );
DWORD
CypherEncrypt(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_ENCRYPTSECTOR TEXT("CypherEncryptSector")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_ENCRYPTSECTOR "CypherEncryptSector"
#endif
typedef DWORD (* PCypherDLLFnEncryptSector)(
                            DWORD,
                            DIOC_CYPHER_SECTOR_DATA_IN*,
                            DWORD,
                            DIOC_CYPHER_DATA_OUT* 
                         );
DWORD
CypherEncryptSector(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_SECTOR_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
);


#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_ENCRYPTWITHASCII TEXT("CypherEncryptWithASCII")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_ENCRYPTWITHASCII "CypherEncryptWithASCII"
#endif
typedef DWORD (* PCypherDLLFnEncryptWithASCII)(
                            IN      GUID*,
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      char*,  // ASCII representation of "Key"
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      int,  // In bytes
                            IN      FREEOTFEBYTE*,
                            OUT     FREEOTFEBYTE*
                         );
DWORD
CypherEncryptWithASCII(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII TEXT("CypherEncryptSectorWithASCII")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII "CypherEncryptSectorWithASCII"
#endif
typedef DWORD (* PCypherDLLFnEncryptSectorWithASCII)(
                            IN      GUID*,
                            IN      LARGE_INTEGER,
                            IN      int,  
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      char*,  // ASCII representation of "Key"
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      int,  // In bytes
                            IN      FREEOTFEBYTE*,
                            OUT     FREEOTFEBYTE*
                         );
DWORD
CypherEncryptSectorWithASCII(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize,  
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
);


#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_DECRYPT TEXT("CypherDecrypt")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_DECRYPT "CypherDecrypt"
#endif
typedef DWORD (* PCypherDLLFnDecrypt)(
                            DWORD,
                            DIOC_CYPHER_DATA_IN*,
                            DWORD,
                            DIOC_CYPHER_DATA_OUT* 
                         );
DWORD
CypherDecrypt(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_DECRYPTSECTOR TEXT("CypherDecryptSector")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_DECRYPTSECTOR "CypherDecryptSector"
#endif
typedef DWORD (* PCypherDLLFnDecryptSector)(
                            DWORD,
                            DIOC_CYPHER_SECTOR_DATA_IN*,
                            DWORD,
                            DIOC_CYPHER_DATA_OUT* 
                         );
DWORD
CypherDecryptSector(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_SECTOR_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
);


#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_DECRYPTWITHASCII TEXT("CypherDecryptWithASCII")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_DECRYPTWITHASCII "CypherDecryptWithASCII"
#endif
typedef DWORD (* PCypherDLLFnDecryptWithASCII)(
                            IN      GUID*,
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      char*,  // ASCII representation of "Key"
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      int,  // In bytes
                            IN      FREEOTFEBYTE*,
                            OUT     FREEOTFEBYTE*
                         );
DWORD
CypherDecryptWithASCII(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
);

#ifdef FOTFE_PDA
#define DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII TEXT("CypherDecryptSectorWithASCII")
#endif
#ifdef FOTFE_PC_DLL
#define DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII "CypherDecryptSectorWithASCII"
#endif
typedef DWORD (* PCypherDLLFnDecryptSectorWithASCII)(
                            IN      GUID*,
                            IN      LARGE_INTEGER,
                            IN      int,  
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      char*,  // ASCII representation of "Key"
                            IN      int,  // In bits
                            IN      FREEOTFEBYTE*,
                            IN      int,  // In bytes
                            IN      FREEOTFEBYTE*,
                            OUT     FREEOTFEBYTE*
                         );
DWORD
CypherDecryptSectorWithASCII(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize,  
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
);


// =========================================================================
// =========================================================================

#endif

