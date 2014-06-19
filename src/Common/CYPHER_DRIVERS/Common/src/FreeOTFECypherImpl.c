// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFECypherImpl.h"


// =========================================================================
// =========================================================================
// =========================================================================

// =========================================================================
// Cypher driver init function
// driverInfo - The structure to be initialized
NTSTATUS
ImpCypherDriverExtDetailsInit_v1(
    IN OUT CYPHER_DRIVER_INFO_v1* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;
    CYPHER_DRIVER_INFO_v3 driverInfo_v3;
    unsigned int i;

    status = ImpCypherDriverExtDetailsInit_v3(&driverInfo_v3);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Failed to ImpCypherDriverExtDetailsInit_v3.\n"));
        return status;
        }


    // -- POPULATE DRIVER IDENTIFICATION --
    FREEOTFE_MEMZERO(driverInfo->DriverTitle, sizeof(driverInfo->DriverTitle));
    FREEOTFE_MEMCPY(
                  driverInfo->DriverTitle,
                  driverInfo_v3.DriverTitle,
                  strlen(driverInfo_v3.DriverTitle)
                 );

    driverInfo->DriverGUID = driverInfo_v3.DriverGUID;
    driverInfo->DriverVersionID = driverInfo_v3.DriverVersionID;


    // -- POPULATE cyphers SUPPORTED --
    driverInfo->CypherCount = driverInfo_v3.CypherCount;
    driverInfo->CypherDetails = FREEOTFE_MEMALLOC(
                                                  (sizeof(CYPHER_v1) * driverInfo_v3.CypherCount)
                                                 );    

    for (i = 0; i < driverInfo_v3.CypherCount; i++)
        {
        CopyCYPHER_v3ToCYPHER_v1(
                             &(driverInfo_v3.CypherDetails[i]),
                             &(driverInfo->CypherDetails[i])
                             );
        }

    return status;
}


// =========================================================================
// Cypher driver cleardown function
NTSTATUS
ImpCypherDriverExtDetailsCleardown_v1(    
    IN OUT CYPHER_DRIVER_INFO_v1* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDriverExtDetailsCleardown\n")));

    if (driverInfo->CypherDetails != NULL)
        {
        FREEOTFE_FREE(driverInfo->CypherDetails);
        }

    driverInfo->CypherDetails = NULL;
    driverInfo->CypherCount = 0;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDriverExtDetailsCleardown\n")));

    return status;
}


// =========================================================================
NTSTATUS
ImpCypherEncryptData(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
)
{
    return ImpCypherEncryptSectorData(
                                      CypherGUID,
                                      FREEOTFE_v1_DUMMY_SECTOR_ID,
                                      FREEOTFE_v1_DUMMY_SECTOR_SIZE,                                            // 
                                      KeyLength,
                                      Key,
                                      KeyASCII,
                                      IVLength,
                                      IV,
                                      PlaintextLength,
                                      PlaintextData,
                                      CyphertextData
                                     );
}

// =========================================================================
NTSTATUS
ImpCypherDecryptData(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
)
{
    return ImpCypherDecryptSectorData(
                                      CypherGUID,
                                      FREEOTFE_v1_DUMMY_SECTOR_ID,
                                      FREEOTFE_v1_DUMMY_SECTOR_SIZE,                                            // 
                                      KeyLength,
                                      Key,
                                      KeyASCII,
                                      IVLength,
                                      IV,
                                      CyphertextLength,
                                      CyphertextData,
                                      PlaintextData
                                     );
}


// =========================================================================
// LRW uses big endian
void
LARGE_INTEGER__To__INTEGER_128_BigEndian(
    IN      LARGE_INTEGER i64Value,
    OUT     INTEGER_128 blockIdx
)
{
    blockIdx[ 0] = 0;
    blockIdx[ 1] = 0;  // Note: Most significant bits are all zero;
    blockIdx[ 2] = 0;  //       We only support up to 2^64, which should cover 
    blockIdx[ 3] = 0;  //       everyone's needs for the forseeable 
    blockIdx[ 4] = 0;  //       future - though this could easily be extended 
    blockIdx[ 5] = 0;  //       if needed
    blockIdx[ 6] = 0;
    blockIdx[ 7] = 0;
    blockIdx[ 8] = (unsigned char)(i64Value.HighPart >> 24) & 0xFF;
    blockIdx[ 9] = (unsigned char)(i64Value.HighPart >> 16) & 0xFF;
    blockIdx[10] = (unsigned char)(i64Value.HighPart >>  8) & 0xFF;
    blockIdx[11] = (unsigned char)(i64Value.HighPart      ) & 0xFF;
    blockIdx[12] = (unsigned char)(i64Value.LowPart  >> 24) & 0xFF;
    blockIdx[13] = (unsigned char)(i64Value.LowPart  >> 16) & 0xFF;
    blockIdx[14] = (unsigned char)(i64Value.LowPart  >>  8) & 0xFF;
    blockIdx[15] = (unsigned char)(i64Value.LowPart       ) & 0xFF;
}


// =========================================================================
// XTS uses little endian
void
LARGE_INTEGER__To__INTEGER_128_LittleEndian(
    IN      LARGE_INTEGER i64Value,
    OUT     INTEGER_128 blockIdx
)
{
    blockIdx[ 0] = (unsigned char)(i64Value.LowPart       ) & 0xFF;
    blockIdx[ 1] = (unsigned char)(i64Value.LowPart  >>  8) & 0xFF;
    blockIdx[ 2] = (unsigned char)(i64Value.LowPart  >> 16) & 0xFF;
    blockIdx[ 3] = (unsigned char)(i64Value.LowPart  >> 24) & 0xFF;
    blockIdx[ 4] = (unsigned char)(i64Value.HighPart      ) & 0xFF;
    blockIdx[ 5] = (unsigned char)(i64Value.HighPart >>  8) & 0xFF;
    blockIdx[ 6] = (unsigned char)(i64Value.HighPart >> 16) & 0xFF;
    blockIdx[ 7] = (unsigned char)(i64Value.HighPart >> 24) & 0xFF;
    blockIdx[ 8] = 0;
    blockIdx[ 9] = 0;  // Note: Least significant bits are all zero;
    blockIdx[10] = 0;  //       We only support up to 2^64, which should cover 
    blockIdx[11] = 0;  //       everyone's needs for the forseeable 
    blockIdx[12] = 0;  //       future - though this could easily be extended 
    blockIdx[13] = 0;  //       if needed
    blockIdx[14] = 0;
    blockIdx[15] = 0;
}


// =========================================================================
// =========================================================================
