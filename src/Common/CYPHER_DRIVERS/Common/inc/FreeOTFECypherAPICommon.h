// Description: FreeOTFE Cypher Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherAPICommon_H
#define _FreeOTFECypherAPICommon_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <WinDef.h>  // Required for ULONG
#endif
#ifdef FOTFE_PC_DRIVER
#include <ntdef.h>  // Required for ULONG
#endif

#include "FreeOTFElib.h" // For ZeroLargeInteger()

// =========================================================================
// Const definitions

#define MAX_CYPHER_TITLE 256

// Maximum cypher blocksize
// This should be set to the MAXIMUM cypher blocksize possible.
// This is used for allocating storage for IVs; i.e.:
//   1) In the FreeOTFE driver when generating sector IVs
//   2) So that the length of the DIOC passed to the cypher driver is more deterministic, which
//      makes creating and populating this struct easier
// If a cypher uses a blocksize greater than this value (e.g. a cypher driver which 
// uses an arbitary blocksize), then attempts to encrypt/decrypt may fail if the IVs passed to
// the encryption/decryption routines may not be large enough.
// Note: This value is in *bits*
#define FREEOTFE_MAX_CYPHER_BLOCKSIZE    512

// These consts are used to supply a dummy sector ID/size to the sector
// encrypt/decrypt routines, where there is none/it's inconseqeuential
#define FREEOTFE_v1_DUMMY_SECTOR_ID     ZeroLargeInteger()
#define FREEOTFE_v1_DUMMY_SECTOR_SIZE   512

// =========================================================================
// Type definitions

typedef enum _CYPHER_MODE {
    CYPHER_MODE_NONE    =    0,
    CYPHER_MODE_ECB     =    1,
    CYPHER_MODE_CBC     =    2,
    CYPHER_MODE_LRW     =    3,
    CYPHER_MODE_XTS     =    4,
    CYPHER_MODE_UNKNOWN = 9999
} CYPHER_MODE, *PCYPHER_MODE;
// The following are used for iterating through all valid cypher modes
#define _CYPHER_MODE_FIRST  CYPHER_MODE_NONE
#define _CYPHER_MODE_LAST   CYPHER_MODE_XTS

typedef struct _CYPHER_v1 {
    GUID CypherGUID;
    char Title[MAX_CYPHER_TITLE];
    CYPHER_MODE Mode;
    // KeySizeUnderlying = -1 for *any* keySize
    int KeySizeUnderlying;  // In bits; nominal cypher keysize
    // BlockSize = -1 for *any* BlockSize
    int BlockSize;  // In bits
    ULONG VersionID;
} CYPHER_v1, *PCYPHER_v1;

typedef struct _CYPHER_v3 {
    GUID CypherGUID;
    // WARNING: IF THE SIZE OF THIS EVER CHANGES; check implementation where
    //          CYPHER (the older version) is populated - in
    //          particular, check it doesn't attempt to populate the old
    //          version beyond it's buffer
    char Title[MAX_CYPHER_TITLE];
    CYPHER_MODE Mode;
    // KeySizeUnderlying = -1 for *any* keySize
    int KeySizeUnderlying;  // In bits; nominal cypher keysize
    // BlockSize = -1 for *any* BlockSize
    int BlockSize;  // In bits
    ULONG VersionID;
    // KeySizeRequired = -1 for *any* keysize
    int KeySizeRequired;  // In bits; size of key required given cypher mode
} CYPHER_v3, *PCYPHER_v3;

typedef struct _CYPHER_DRIVER_INFO_v1 {
    // Driver identification...
    GUID DriverGUID;
    char DriverTitle[MAX_CYPHER_TITLE];
    ULONG DriverVersionID;

    // Cyphers supported...
    unsigned int CypherCount;
    CYPHER_v1* CypherDetails;  // This is a *pointer* to an *array* of
                               // "CypherCount" CYPHER structures
} CYPHER_DRIVER_INFO_v1, *PCYPHER_DRIVER_INFO_v1;

typedef struct _CYPHER_DRIVER_INFO_v3 {
    // Driver identification...
    GUID DriverGUID;
    // WARNING: IF THE SIZE OF THIS EVER CHANGES; check implementation where
    //          CYPHER_DRIVER_INFO (the older version) is populated - in
    //          particular, check it doesn't attempt to populate the old
    //          version beyond it's buffer
    char DriverTitle[MAX_CYPHER_TITLE];
    ULONG DriverVersionID;

    // Cyphers supported...
    unsigned int CypherCount;
    CYPHER_v3* CypherDetails;  // This is a *pointer* to an *array* of
                               // "CypherCount" CYPHER structures
} CYPHER_DRIVER_INFO_v3, *PCYPHER_DRIVER_INFO_v3;


typedef struct _DIOC_CYPHER_IDENTIFYDRIVER {    
    GUID DriverGUID;
    char Title[MAX_CYPHER_TITLE];
    ULONG VersionID;
    unsigned int CountCyphers;
} DIOC_CYPHER_IDENTIFYDRIVER, *PDIOC_CYPHER_IDENTIFYDRIVER;


typedef struct _DIOC_CYPHER_IDENTIFYSUPPORTED_v1 {    
    unsigned int BufCount;  // The number of *elements* in the "Cyphers" array
    CYPHER_v1 Cyphers[1];  // Variable length
} DIOC_CYPHER_IDENTIFYSUPPORTED_v1, *PDIOC_CYPHER_IDENTIFYSUPPORTED_v1;

typedef struct _DIOC_CYPHER_IDENTIFYSUPPORTED_v3 {    
    unsigned int BufCount;  // The number of *elements* in the "Cyphers" array
    CYPHER_v3 Cyphers[1];  // Variable length
} DIOC_CYPHER_IDENTIFYSUPPORTED_v3, *PDIOC_CYPHER_IDENTIFYSUPPORTED_v3;


typedef struct _DIOC_CYPHER_DATA_IN {
    GUID CypherGUID;
    int KeyLength;  // In *bits*
    int IVLength;  // In *bits*
                   // Note: Not all cyphers use an IV (e.g. in EBC mode)
                   //       If specified though, IVLength, and IV, should 
                   //       be equal to the blocksize of the cypher being used
    int DataLength;  // In *bytes*
    FREEOTFEBYTE Key[1];  // Variable length
    FREEOTFEBYTE IV[1];  // Variable length
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_CYPHER_DATA_IN, *PDIOC_CYPHER_DATA_IN;

typedef struct _DIOC_CYPHER_SECTOR_DATA_IN {
    GUID CypherGUID;

    // Note: SectorID and SectorSize are used to determine the block index
    //       within the volume
    // Note: LRW defines the first sector as 1. The cypher *implementation*
    //       should be take this into account, and the fact this is zero-based,
    //       when determining block ID
    // Note: From the example in the IEEE standards document, XTS appears to
    //       index from 0
    LARGE_INTEGER SectorID;  // Zero based onwards.
    int SectorSize;  // In bytes

    int KeyLength;  // In *bits*
    int IVLength;  // In *bits*
                   // Note: Not all cyphers use an IV (e.g. in EBC mode)
                   //       If specified though, IVLength, and IV, should 
                   //       be equal to the blocksize of the cypher being used
    int DataLength;  // In *bytes*
    FREEOTFEBYTE Key[1];  // Variable length
    FREEOTFEBYTE IV[1];  // Variable length
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_CYPHER_SECTOR_DATA_IN, *PDIOC_CYPHER_SECTOR_DATA_IN;


// When used, this buffer must be the same size as the "Data" member
// of DIOC_CYPHER_DATA_IN
typedef struct _DIOC_CYPHER_DATA_OUT {
    char Data[1];  // Variable length
} DIOC_CYPHER_DATA_OUT, *PDIOC_CYPHER_DATA_OUT;


// This definition must be kept in sync with "ImpCypherEncryptData"
typedef NTSTATUS (* PDataEncryptFn)(
                                    GUID*,
                                    int, 
                                    FREEOTFEBYTE*, 
                                    char*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    FREEOTFEBYTE*
                                   );


// This definition must be kept in sync with "ImpCypherDecryptData"
typedef NTSTATUS (* PDataDecryptFn)(
                                    GUID*,
                                    int, 
                                    FREEOTFEBYTE*, 
                                    char*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    FREEOTFEBYTE*
                                   );

// This definition must be kept in sync with "ImpCypherEncryptSectorData"
typedef NTSTATUS (* PSectorDataEncryptFn)(
                                    GUID*,
                                    LARGE_INTEGER, 
                                    int, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    char*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    FREEOTFEBYTE*
                                   );


// This definition must be kept in sync with "ImpCypherDecryptSectorData"
typedef NTSTATUS (* PSectorDataDecryptFn)(
                                    GUID*,
                                    LARGE_INTEGER, 
                                    int, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    char*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    int, 
                                    FREEOTFEBYTE*, 
                                    FREEOTFEBYTE*
                                   );

// This definition must be kept in sync with "GetCypherDetails_v1"
typedef NTSTATUS (* PCypherDetails_v1Fn)(
                                    GUID*,
                                    CYPHER_v1*
                                   );

// This definition must be kept in sync with "GetCypherDetails_v3"
typedef NTSTATUS (* PCypherDetails_v3Fn)(
                                    GUID*,
                                    CYPHER_v3*
                                   );

// Internal details
typedef struct _DIOC_CYPHER_INTL_DETAILS_v1 {
    PCypherDetails_v1Fn  FnCypherDetails_v1;
    PDataEncryptFn       FnEncrypt;
    PDataDecryptFn       FnDecrypt;
} DIOC_CYPHER_INTL_DETAILS_v1, *PDIOC_CYPHER_INTL_DETAILS_v1;

typedef struct _DIOC_CYPHER_INTL_DETAILS_v3 {
    PCypherDetails_v1Fn  FnCypherDetails_v1;
    // ! WARNING !
    // The next one isn't populated for older (pre v3) cypher drivers
    PCypherDetails_v3Fn  FnCypherDetails_v3;
    PDataEncryptFn       FnEncrypt;
    PDataDecryptFn       FnDecrypt;
    // ! WARNING !
    // The next ones aren't populated for older (pre v3) cypher drivers
    PSectorDataEncryptFn   FnEncryptSector;
    PSectorDataDecryptFn   FnDecryptSector;
} DIOC_CYPHER_INTL_DETAILS_v3, *PDIOC_CYPHER_INTL_DETAILS_v3;


void
CopyCYPHER_v3ToCYPHER_v1(
    IN     CYPHER_v3*  Source,
    OUT    CYPHER_v1*  Dest
);

void
CopyCYPHER_v1ToCYPHER_v3(
    IN     CYPHER_v1*  Source,
    OUT    CYPHER_v3*  Dest
);

void
CopyCYPHER_DRIVER_INFO_v1ToCYPHER_DRIVER_INFO_v3(
    IN     CYPHER_DRIVER_INFO_v1*  Source,
    OUT    CYPHER_DRIVER_INFO_v3*  Dest
);

// =========================================================================
// =========================================================================

#endif

