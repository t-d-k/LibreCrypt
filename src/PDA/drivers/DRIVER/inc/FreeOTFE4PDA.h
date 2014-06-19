// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDA_H
#define _FreeOTFE4PDA_H   1

#include "FreeOTFEPlatform.h"

#include "FreeOTFE4PDAAPI.h"
#include "FreeOTFE4PDAHashAPI.h"
#include "FreeOTFE4PDACypherAPI.h"
#include "FreeOTFEContext.h"

#include <Diskio.h>  // Required for SG_REQ definition


// =========================================================================
// Const definitions

// Mandatory this is 512 for WINCE; see the DISK_INFO struct
#define BYTES_PER_SECTOR        FREEOTFE_BYTES_PER_SECTOR
#define MAND_BYTES_PER_SECTOR   BYTES_PER_SECTOR

// These figures are used; no real reason for these *exact* numbers - though
// they're what my SD and CF card use
// Tracks per cylinder = the number of heads
#define TRACKS_PER_CYLINDER  4
#define SECTORS_PER_TRACK    32

// Encrypt in 512 byte "sectors"
#define ENCRYPTION_BLOCK_SIZE 512

// Maximum MAC length
// This should be set to the MAXIMUM MAC length that the any MAC may generate.
// Note: This value is in *bits*
#define FREEOTFE_MAX_MAC_LENGTH 1024

#define DEFAULT_PROFILE TEXT("Default")

// From the MSDN article on IOCTL_DISK_GET_STORAGEID:
//   "For Advanced Technology Attachment (ATA) disk devices, the
//   identifiers consist of 20 bytes for a manufacturer identifier
//   string, and also 10 bytes for the serial number of the disk."
// Note: Isn't that the other way round?! From judging by actual
//       SD/CF cards...
#define STORAGE_MANUFACTURER          "SarahDean"
#define STORAGE_MAX_LEN_MANUFACTURER  20
// Note: Serial number probably *should* be random, and stored as
//       part of the volume
#define STORAGE_SERIALNUM             "1234567890"
#define STORAGE_MAX_LEN_SERIALNUM     10


// =========================================================================
// Global variables...
extern BOOL G_contextMgrForceDismounts;


// =========================================================================
// Function headers

// --------------------------------------
// DLL main function...

BOOL WINAPI DllMain(
  HANDLE hinstDLL, 
  DWORD dwReason, 
  LPVOID lpvReserved
);


// --------------------------------------
// IOCTL functions...

// Template forward declaration for IOCTLs:
//BOOL
//IOCTL_Std_XXXXXXXXX(
//    DEVICE_CONTEXT* devContext,
//    PBYTE pBufIn,
//    DWORD dwLenIn,
//    PBYTE pBufOut,
//    DWORD dwLenOut,
//    PDWORD pdwActualOut 
//);

#ifdef FOTFE_PDA 
BOOL
IOCTL_Std_DiskDeviceInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
);
#endif

BOOL
IOCTL_Std_DiskGetInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);

BOOL
IOCTL_Std_DiskSetInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
);

BOOL
IOCTL_Std_DiskFormatMedia(
    DEVICE_CONTEXT* devContext,
    PDWORD pdwActualOut 
);

BOOL
IOCTL_Std_DiskReadWrite(
    DEVICE_CONTEXT* devContext,
    BOOL readNotWrite,
    PBYTE pBuf,
    DWORD dwLen,
    PDWORD pdwActualOut 
);

BOOL
IOCTL_Std_DiskGetStorageID(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);

BOOL
IOCTL_Std_DiskInitialized(
    DEVICE_CONTEXT* devContext,
    PDWORD pdwActualOut 
);

#ifdef FOTFE_PDA 
BOOL
IOCTL_Std_DiskGetName(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);
#endif

BOOL
IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);

#ifdef FOTFE_PDA
BOOL
IOCTL_FOTFE_SetUserDevHandle(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
);
#endif
#ifdef FOTFE_PDA
BOOL
IOCTL_FOTFE_GetUserDevHandle(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);
#endif

BOOL
IOCTL_FOTFE_SetForceDismount(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
);


// --------------------------------------
// Driver specific...

BOOL
GetMaxSizePartition(
    IN  WCHAR* DeviceName,
    OUT PLARGE_INTEGER MaxSize
);

void
OverwriteDeviceSensitive(DEVICE_CONTEXT* devContext);

// Process a *valid* SG_REQ read/write request
BOOL
ProcessValidReadWrite(
    DEVICE_CONTEXT* devContext,
    BOOL readNotWrite,  // Set to TRUE to read, FALSE to write
    SG_REQ* req,
    DWORD dwLen,
    PDWORD pdwActualOut
);

// Perform an actual read from the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
DWORD
ActualReadFile(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    IN      PBYTE Buffer
);

// Perform an actual write to the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
DWORD
ActualWriteFile(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    OUT     PBYTE Buffer
);

// Read/write data to the specified device
DWORD
ReadWriteData(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      BOOL ReadNotWrite,
    IN      PLARGE_INTEGER PartitionOffset,
    IN      DWORD BufferLength,
    IN OUT  PBYTE Buffer
);

BOOL
GetDeviceDetailsHash(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_HASH* HashDetails
);

// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsHash(...)
void
FreeDeviceDetailsHash(
    OUT     MODULE_DETAILS_HASH* HashDetails
);

BOOL
GetDeviceDetailsCypher_v3_v1(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
);

BOOL
GetDeviceDetailsCypher_v1(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
);

BOOL
GetDeviceDetailsCypher_v3(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
);

// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v3_v1(
    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
);

// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v1(
    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
);

// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v3(
    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
);

NTSTATUS
DeriveKey(
    IN      KDF_ALGORITHM KDFAlgorithm,
    IN      WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID HashGUID,
    IN      WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID CypherGUID,
    IN      int Iterations,
    IN      int LengthWanted,
    IN      unsigned int PasswordLength,  // In bits
    IN      unsigned char* Password,
    IN      unsigned int SaltLength,  // In bits
    IN      unsigned char* Salt,

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     unsigned char* DerivedKey
);

NTSTATUS
GenerateMAC(
    IN      MAC_ALGORITHM MACAlgorithm,
    IN      WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID HashGUID,
    IN      WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID CypherGUID,
    IN      int LengthWanted,  // In bits
    IN      unsigned int KeyLength,  // In bits
    IN      unsigned char* Key,
    IN      unsigned int DataLength,  // In bits
    IN      unsigned char* Data,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     unsigned char* MAC
);


// =========================================================================
// =========================================================================

#endif

