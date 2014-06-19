// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _DriverInterfaceTwo_H
#define _DriverInterfaceTwo_H   1

#include "FreeOTFE4PDAAPI.h"  // Required for main DLL function definitions
#include "FreeOTFEHashAPICommon.h"
#include "FreeOTFECypherAPICommon.h"
#include "DriverInterfaceCommon.h"
#include "DriverInterface.h"

#include <WinNT.h>  // Required for WCHAR


// =========================================================================
// Variables used in dumping...
static BOOL _driver_DumpFlag = FALSE;
static int _driver_DumpCriticalDataKeyBits;
static FREEOTFEBYTE* _driver_DumpCriticalDataKey = NULL;
static int _driver_DumpCheckMACBits;
static FREEOTFEBYTE* _driver_DumpCheckMAC = NULL;
static int _driver_DumpPlaintextEncryptedBlockBits;
static FREEOTFEBYTE* _driver_DumpPlaintextEncryptedBlock = NULL;
static int _driver_DumpVolumeDetailsBlockBits;
static FREEOTFEBYTE* _driver_DumpVolumeDetailsBlock = NULL;


// =========================================================================
// Structures...

typedef struct _MODULE_DETAILS_MAIN {
    WCHAR* Filename;
    HINSTANCE Lib;
    
    PMainDLLFnDSK_Init       FnDSK_Init;
    PMainDLLFnDSK_Deinit     FnDSK_Deinit;
    PMainDLLFnDSK_Open       FnDSK_Open;
    PMainDLLFnDSK_Close      FnDSK_Close;
    PMainDLLFnDSK_IOControl  FnDSK_IOControl;
    PMainDLLFnDSK_Seek       FnDSK_Seek;
    PMainDLLFnDSK_Read       FnDSK_Read;
    PMainDLLFnDSK_Write      FnDSK_Write;
    PMainDLLFnDSK_PowerDown  FnDSK_PowerDown;
    PMainDLLFnDSK_PowerUp    FnDSK_PowerUp;

    PMainDLLFnDeriveKey  FnDeriveKey;
    PMainDLLFnMACData    FnMACData;
} MODULE_DETAILS_MAIN, *PMODULE_DETAILS_MAIN;


typedef struct _CDB_DETAILS {
  int CDBFormatID;
  
  LARGE_INTEGER PartitionSize;
  unsigned int VolumeFlags;
  SECTOR_IV_GEN_METHOD SectorIVGenMethod;
  
  char DefaultDrive;  // Not used in PDA version, but included to allow 
                      // password changing on volumes used by the PC version
  
  ULONG MasterKeyLength;  // In bits
  ULONG VolumeIVLength;  // In bits
  FREEOTFEBYTE* MasterKey;    
  FREEOTFEBYTE* VolumeIV;    
} CDB_DETAILS, *PCDB_DETAILS;

typedef struct _CDB_METADATA {
    WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
    GUID HashGUID;
    WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
    GUID CypherGUID;
    KDF_ALGORITHM KDFAlgorithm;
    MAC_ALGORITHM MACAlgorithm;
} CDB_METADATA, *PCDB_METADATA;

typedef struct _DRIVER_AND_ALG {
    WCHAR PrettyName[MAX_PRETTYPRINTED_TITLE];
    WCHAR DriverFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID DriverImplAlgGUID;

    // Only used for hashes...
    CYPHER_v3 Cypher;
    // Only used for cyphers...
    HASH Hash;
} DRIVER_AND_ALG, *PDRIVER_AND_ALG;


// =========================================================================
// Functions...

// Key derivation function
BOOL driver_DeriveKey(
    MODULE_DETAILS_MAIN* mainDriver,

    KDF_ALGORITHM kdfAlgorithm,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    WCHAR* cypherKernelModeDeviceName,
    GUID cypherGUID,
    char* Password,
    unsigned int PasswordLength,  // In bytes
    FREEOTFEBYTE* Salt,
    unsigned int SaltLength, // In *bits*
    unsigned int Iterations,
    unsigned int dkLenBits,  // In *bits*
    FREEOTFEBYTE* DK
);


BOOL driver_MACData(
    MODULE_DETAILS_MAIN* mainDriver,

    MAC_ALGORITHM macAlgorithm,
    const WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    const WCHAR* cypherKernelModeDeviceName,
    GUID cypherGUID,
    unsigned int KeyLength,
    FREEOTFEBYTE* Key,
    unsigned int DataLength,
    FREEOTFEBYTE* Data,
    FREEOTFEBYTE** MACOut,
    int* tBits
);


MOUNT_RESULT driver_Mount(
    WCHAR* Mountpoint,
    WCHAR* Filename,
    WCHAR* Keyfile,
    LARGE_INTEGER OffsetWithinFile,
    BOOL CDBAtOffset,
    unsigned char* UserPassword,
    unsigned int SaltLength,  // In *bits*
    unsigned int KeyIterations,
    BOOL ReadOnly,
    BOOL RevertVolTimestamps
);

MOUNT_RESULT driver_MountLUKS(
    WCHAR* Mountpoint,
    WCHAR* Filename,
    unsigned char* UserPassword,
    BOOL ReadOnly,
    BOOL BaseIVCypherOnHashLength,
    LARGE_INTEGER SizeLimit,
    BOOL RevertVolTimestamps
);

BOOL driver_GetAllAlgorithmDriverDetails(
    int* countDriversHash,
    WCHAR*** driverFilenamesHash,
    HASH_DRIVER_INFO*** driverInfoHash,

    int* countDriversCypher,
    WCHAR*** driverFilenamesCypher,
    CYPHER_DRIVER_INFO_v3*** driverInfoCypher,

    BOOL ExcludeDisabledDrivers
);

void driver_FreeAllAlgorithmDriverDetails(
    int* countDriversHash,
    WCHAR*** driverFilenamesHash,
    HASH_DRIVER_INFO*** driverInfoHash,

    int* countDriversCypher,
    WCHAR*** driverFilenamesCypher,
    CYPHER_DRIVER_INFO_v3*** driverInfoCypher
);

BOOL driver_GetAllAlgorithmDriverOptions(
    int* CountImplHash,
    DRIVER_AND_ALG** HashImpl,

    int* CountImplCypher,
    DRIVER_AND_ALG** CypherImpl,

    BOOL ExcludeDisabledDrivers
);

void driver_FreeAllAlgorithmDriverOptions(
    int* CountImplHash,
    DRIVER_AND_ALG** HashImpl,

    int* CountImplCypher,
    DRIVER_AND_ALG** CypherImpl
);

BOOL
driver_CreateVolumeFile(
    WCHAR* Filename,
    WCHAR* KeyFilename,
    LARGE_INTEGER Offset,
    char* UserPassword,
    int SaltLength,  // In *bits*
    int KeyIterations,
    FREEOTFEBYTE* RandomPool,
    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
);

// Create a new keyfile, given a volume with CDB or existing keyfile
BOOL
driver_CreateKeyfile(
    WCHAR* ExistingFilename,
    LARGE_INTEGER Offset,
    char* ExistingUserPassword,
    unsigned int ExistingSaltLength,  // In *bits*
    unsigned int ExistingKeyIterations,

    WCHAR* KeyfileFilename,
    char* KeyfileUserPassword,
    unsigned int KeyfileSaltLength,  // In *bits*
    unsigned int KeyfileKeyIterations,

    FREEOTFEBYTE* RandomPool
);

// Change a volume/keyfile's details (e.g. password)
BOOL
driver_ChangeDetails(
    WCHAR* Filename,
    LARGE_INTEGER Offset,

    char* CurrentUserPassword,
    unsigned int CurrentSaltLength,  // In *bits*
    unsigned int CurrentKeyIterations,

    char* NewUserPassword,
    unsigned int NewSaltLength,  // In *bits*
    unsigned int NewKeyIterations,

    FREEOTFEBYTE* RandomPool
);

void driver_FreeCriticalDump();

void driver_DumpWCHAR(FILE* hFile, WCHAR* Data);
void driver_AddStdDumpHeader(
	FILE* hFile,
	char* title
);
void driver_AddStdDumpSection(
    FILE* hFile,
    char* sectionTitle
);
void driver_AddUnderline(
    FILE* hFile,
    char UnderlineChar,
    unsigned int Length
);

BOOL
driver_DumpCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    char* SrcUserPassword,
    unsigned int SrcSaltLength,  // In *bits*
    unsigned int SrcKeyIterations,

    WCHAR* DestFilename
);

BOOL
driver_DumpLUKS(
    WCHAR* SrcFilename,
    char* SrcUserPassword,
    BOOL SrcBaseIVCypherOnHashLength,

    WCHAR* DestFilename
);

BOOL
driver_BackupCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    WCHAR* DestFilename
);

BOOL
driver_RestoreCDB(
    WCHAR* SrcFilename,
    WCHAR* DestFilename,
    LARGE_INTEGER DestOffset
);

BOOL driver_ReadWritePlaintextToVolume(
    MODULE_DETAILS_MAIN* mainDriver,

    BOOL readNotWrite,

    WCHAR* volFilename,
    char* VolumeKey,
    int VolumeKeyLength,
    SECTOR_IV_GEN_METHOD SectorIVGenMethod,
    WCHAR* IVHashDriver,
    GUID IVHashGUID,
    WCHAR* IVCypherDriver,
    GUID IVCypherGUID,
    WCHAR* MainCypherDriver,
    GUID MainCypherGUID,
    int VolumeFlags,

    LARGE_INTEGER dataOffset,  // Offset from within mounted volume from where to read/write data
    int dataLength,  // Length of data to read/write. In bytes
    BYTE* data,  // Data to read/write

    LARGE_INTEGER Offset,
    LARGE_INTEGER Size
);

BOOL driver_ReadWritePlaintextToVolume_Sectors(
    MODULE_DETAILS_MAIN* mainDriver,

    BOOL readNotWrite,

    WCHAR* volFilename,
    char* VolumeKey,
    int VolumeKeyLength,  // In *bits*
    SECTOR_IV_GEN_METHOD SectorIVGenMethod,
    WCHAR* IVHashDriver,
    GUID IVHashGUID,
    WCHAR* IVCypherDriver,
    GUID IVCypherGUID,
    WCHAR* MainCypherDriver,
    GUID MainCypherGUID,
    int VolumeFlags,

    DWORD SectorStart,  // Offset from within mounted volume from where to read/write data
    DWORD SectorCount,  // Length of data to read/write. In bytes
    BYTE* data,  // Data to read/write

    LARGE_INTEGER Offset,
    LARGE_INTEGER Size
);


// =========================================================================
// =========================================================================

#endif

