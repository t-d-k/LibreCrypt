// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _DriverInterface_H
#define _DriverInterface_H   1

#include "FreeOTFE4PDAAPI.h"
#include "FreeOTFE4PDARegistry.h"

#include "FreeOTFE4PDADriverList.h" // Used for disabling drivers

#include <windows.h>  // Required for UINT, BOOL

// =========================================================================
// Enums...
typedef enum _MOUNT_RESULT {
    MR_MOUNTED_OK            =    1,
    MR_FILE_NOT_FOUND        =    2,
    MR_CDB_DECRYPT_FAILED    =    3,
    MR_ACTIVATEDEVICE_FAILED =    4,
    MR_OUT_OF_MEMORY         =    5,
    MR_NOT_A_LUKS_VOLUME     =    6,
    MR_UNKNOWN               = 9999
} MOUNT_RESULT, *PMOUNT_RESULT;


// =========================================================================
// Variables used to hold disabled drivers...
// (In FreeOTFE4PDA, this just holds a pointer to the entries in the 
// options stuct)
extern LNKLIST_DISABLED_DRIVER* G_driver_DisabledHashDrivers;
extern LNKLIST_DISABLED_DRIVER* G_driver_DisabledCypherDrivers;


// =========================================================================

BOOL driver_VersionID(DWORD* versionID);
BOOL driver_GetMountedCount(int* count);
BOOL driver_MountedVolumes(
    int detailsActiveSize,
    REGDETAILS_ACTIVE *detailsActive
);
BOOL driver_GetVolumeInfo_Mountpoint(
    WCHAR* mountpoint,
    DIOC_DISK_DEVICE_STATUS* volumeDetails
);
BOOL driver_GetVolumeInfo_DeviceName(
    WCHAR* deviceName, 
    DIOC_DISK_DEVICE_STATUS* volumeDetails
);
BOOL driver_Dismount(WCHAR* mountpoint, BOOL force);
BOOL driver_DismountAll(BOOL force);

BOOL driver_SetUserAppHandle(WCHAR* mountpoint, HANDLE hActiveDevice);
HANDLE driver_GetUserAppHandle(WCHAR* mountpoint);

MOUNT_RESULT _driver_MountDIOC(DIOC_MOUNT* mountDetails);

void driver_PrettyprintSectorIVGenMethod(
    SECTOR_IV_GEN_METHOD sectorIVGenMethod, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);
void driver_PrettyprintSectorIVGenMethod_char(
    SECTOR_IV_GEN_METHOD sectorIVGenMethod, 
    char* buffer,
    int bufferSize  // In bytes
);

void driver_PrettyprintKDFAlgorithm(
    KDF_ALGORITHM algorithm, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);

void driver_PrettyprintMACAlgorithm(
    MAC_ALGORITHM algorithm, 
    WCHAR* buffer,
    int bufferSize  // In bytes
);

// =========================================================================
// =========================================================================

#endif

