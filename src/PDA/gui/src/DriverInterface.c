// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "DriverInterface.h"
#include "DriverInterfaceCommon.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDARegistry.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAAPIConsts.h"

#include "SDUGeneral.h"
#include "SDUi18n.h"

#include <stdio.h>  // Needed for wcsncpy(...)
#include <StoreMgr.h>  // Needed for BLOCK_DRIVER_GUID_STRING


// =========================================================================
// Define externals declared in .h file
LNKLIST_DISABLED_DRIVER* G_driver_DisabledHashDrivers = NULL;
LNKLIST_DISABLED_DRIVER* G_driver_DisabledCypherDrivers = NULL;


// =========================================================================
// Forward declarations...
MOUNT_RESULT _driver_MountDIOC_DLLOption(DIOC_MOUNT* mountDetails, BOOL fullPath);


// =========================================================================
// Obtain driver version ID
// Note: Version ID taken from DLL's version info block
BOOL driver_VersionID(DWORD* versionID)
{
    BOOL retval = FALSE;
    WCHAR* DLLFilename;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_VersionID\n")));

    DLLFilename = driver_AllocMainDLLFullFilename(TRUE);
    if (DLLFilename == NULL)
        {
        // Do nothing; retval already set to FALSE
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("UNABLE TO GET FILENAME FOR MAIN DLL FILENAME")));
        }
    else
        {
        retval = SDUGetVersionInfoShort(DLLFilename, versionID);
        SecZeroAndFreeWCHARMemory(DLLFilename);
        }

    if (!(retval))
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED TO GET VERSION ID\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Debug level   : %d\n"), FreeOTFEDebugLevel));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Driver version: 0x%08x (v%02d.%02d.%04d)\n"),
                                     *versionID, 
                                     (*versionID & 0xFF000000) / 0x00FF0000,
                                     (*versionID & 0x00FF0000) / 0x0000FF00,
                                     (*versionID & 0x0000FFFF)
                                    ));
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_VersionID\n")));
    return retval;
}


// =========================================================================
BOOL driver_GetMountedCount(int* count)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_GetMountedCount\n")));

    (*count) = RegDetailsGetAllActive(0, NULL);
    retval = ((*count) != -1);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_GetMountedCount\n")));
    return retval;
}


// =========================================================================
// Retrieve details of all mounted volumes.
// detailsActiveSize - Size in bytes of detailsActive buffer.                
// detailsActive - Actually an array of REGDETAILS_ACTIVE, this will be
//                 filled in as appropriate
// Note: On failure, the contents of detailsActive are undefined 
// Note: This function will fail if detailsActiveSize doens't reflect the
//       number of mounted volumes
// IMPORTANT: If this function succeeds, it is the caller's responsibility
//            to free of the WCHAR*s, etc in each of the populated
//            REGDETAILS_ACTIVE structures 
BOOL driver_MountedVolumes(
    int detailsActiveSize,  // Size in bytes of detailsActive buffer
    REGDETAILS_ACTIVE *detailsActive  // Actually an array of REGDETAILS_ACTIVE
)
{
    BOOL retval = FALSE;
    int count;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_MountedVolumes\n")));

    count = RegDetailsGetAllActive(
                                   detailsActiveSize,
                                   detailsActive
                                  );

    retval = (
              (count != -1) &&
              (count == (detailsActiveSize / sizeof(detailsActive[0])))
             );

    if (!(retval))
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to get mounted volumes\n")));
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_MountedVolumes\n")));
    return retval;
}


// =========================================================================
BOOL driver_GetVolumeInfo_DeviceName(
    WCHAR* deviceName, 
    DIOC_DISK_DEVICE_STATUS* volumeDetails
)
{
    BOOL retval = FALSE;
    DWORD bytesReturned;
    HANDLE fileHandle;
    DWORD desiredAccess;
    DWORD fileFlags;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_GetVolumeInfo_DeviceName\n")));

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Trying to open device: %ls...\n"), deviceName));

    desiredAccess = 0; //(GENERIC_READ | GENERIC_WRITE);
    // Note: FILE_ATTRIBUTE_NORMAL only valid if used *alone*
    fileFlags = FILE_ATTRIBUTE_NORMAL;

    fileHandle = CreateFile(
                            deviceName,
                            desiredAccess,  
                            (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                            NULL,
                            OPEN_EXISTING, 
                            fileFlags, 
                            NULL
                           ); 

    if (fileHandle == INVALID_HANDLE_VALUE) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to open device: %ls\n"), deviceName));
        }
    else
        {
        retval = DeviceIoControl(
                                 fileHandle, 
                                 IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS, 
                                 NULL, 
                                 0, 
                                 volumeDetails,
                                 sizeof(*volumeDetails),
                                 &bytesReturned,
                                 NULL
                                );

        if (retval)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("DIOC call OK\n")));
            }
        else
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("DIOC call failed\n")));
            }

        retval = (bytesReturned == sizeof(*volumeDetails));

        // Slightly paranoid, but...
        if (!(retval))
            {
            SecZeroMemory(volumeDetails, sizeof(*volumeDetails));
            }

        CloseHandle(fileHandle);
        }

    if (retval)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Returning TRUE\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Returning FALSE\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_GetVolumeInfo_DeviceName\n")));
    return retval;
}


// =========================================================================
BOOL driver_GetVolumeInfo_Mountpoint(
    WCHAR* mountpoint,
    DIOC_DISK_DEVICE_STATUS* volumeDetails
)
{
    BOOL retval = FALSE;
    int cnt;
    REGDETAILS_ACTIVE* mountedDetails;
    BOOL found = FALSE;
    BOOL allOK = FALSE;
    int i;
    int detailsByteSize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_GetVolumeInfo_Mountpoint\n")));

    detailsByteSize = 0;

    // Get details for all volumes
    allOK = driver_GetMountedCount(&cnt);
    if (allOK)
        {
        detailsByteSize = (cnt * sizeof(*mountedDetails)); 
        mountedDetails = malloc(detailsByteSize);
        if (mountedDetails == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for all volumes mounted\n")));
            allOK = FALSE;
            }
        else
            {
            allOK = driver_MountedVolumes(detailsByteSize, mountedDetails);
            if (!(allOK))
                {
                SecZeroAndFreeMemory(mountedDetails, detailsByteSize);
                mountedDetails = NULL;
                }
            }
        }

    if (allOK) 
        {
        for(i = 0; i < cnt; i++)
            {
            // Identify the volume we're interested in
            if (wcscmp(
                       mountpoint, 
                       mountedDetails[i].Mountpoint
                      ) == 0)
                {
                found = TRUE;

                allOK = driver_GetVolumeInfo_DeviceName(
                                    mountedDetails[i].Name,
                                    volumeDetails
                                   );

                break;
                }
            }
        }

    RegDetailsFreeAllActive(cnt, mountedDetails);
    SecZeroAndFreeMemory(mountedDetails, detailsByteSize);

    retval = (allOK && found);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_GetVolumeInfo_Mountpoint\n")));
    return retval;
}


// =========================================================================
// Attempt to mount using the details supplied.
// First we attempt to mount passing the device manager the full path and
// filename to the main DLL in the registry key
// If that fails, we try again, but without the full path to the main DLL
// in which case, Windows Mobile *should* pick up the DLLs from the
// \Windows directory by default
MOUNT_RESULT _driver_MountDIOC(DIOC_MOUNT* mountDetails)
{
    BOOL retval = MR_UNKNOWN;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_MountDIOC\n")));

    retval = _driver_MountDIOC_DLLOption(mountDetails, TRUE);
    if (retval != MR_MOUNTED_OK)
        {
        retval = _driver_MountDIOC_DLLOption(mountDetails, FALSE);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_MountDIOC\n")));
    return retval;
}


// =========================================================================
// Debug - If the next line is uncommented, then debug messages will be
// displayed to the user during mounting
//#define _DEBUG_ACTIVATEDEVICEEX 1
MOUNT_RESULT _driver_MountDIOC_DLLOption(DIOC_MOUNT* mountDetails, BOOL fullPath)
{
    BOOL retval = FALSE;
    REGDETAILS_BUILTIN regdetailsBuiltin;
    REGDETAILS_PROFILE regdetailsProfile;
    WCHAR* keyBuiltin;    
    HANDLE hActiveDevice = NULL;
    int regDeviceID;
#ifdef _DEBUG_ACTIVATEDEVICEEX
    DWORD errorCode;  // xxx - lplp
#endif
  
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_MountDIOC_DLLOption\n")));

    regDeviceID = RegGetNextRegNumber();

    regdetailsBuiltin.Prefix       = DRIVER_PREFIX;
    regdetailsBuiltin.Dll          = driver_AllocMainDLLFullFilename(fullPath);
    regdetailsBuiltin.FriendlyName = DRIVER_FRIENDLYNAME;
    regdetailsBuiltin.Order        = 0;
    regdetailsBuiltin.Ioctl        = 4;
    regdetailsBuiltin.IClass       = BLOCK_DRIVER_GUID_STRING;
    regdetailsBuiltin.Profile      = NULL;  // Determined automatically

    regdetailsProfile.Name         = DRIVER_PROFILE_NAME;
    regdetailsProfile.Folder       = mountDetails->Mountpoint;
    regdetailsProfile.AutoMount    = 1;
    regdetailsProfile.AutoPart     = 1;
    regdetailsProfile.AutoFormat   = 1;
    regdetailsProfile.MountFlags   = 0;

    // Create driver registry entries...
    RegDetailsSetAll(regDeviceID, regdetailsBuiltin, regdetailsProfile);

    // Activate driver...
    RegKeyGenerate(regDeviceID, &keyBuiltin, NULL);
#ifdef _DEBUG_ACTIVATEDEVICEEX
    MsgInfo(NULL, regdetailsBuiltin.Dll);  // xxx - lplp
    MsgInfo(NULL, TEXT("About to activate..."));  // xxx - lplp
    SetLastError(0);  // xxx - lplp
#endif
    hActiveDevice = ActivateDeviceEx(keyBuiltin, NULL, 0, mountDetails);
#ifdef _DEBUG_ACTIVATEDEVICEEX
    // hActiveDevice = NULL;  // xxx - lplp
    errorCode = GetLastError();  // xxx - lplp
#endif
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ActivateDeviceEx returned handle: 0x%0.8x\n"), hActiveDevice));
    SecZeroAndFreeWCHARMemory(keyBuiltin);

    // On mount failure, purge registry entries created
    if (hActiveDevice == NULL)
        {
#ifdef _DEBUG_ACTIVATEDEVICEEX
        MsgError(NULL, TEXT("Activation failure"));  // xxx - lplp
        MsgErrorDWORD_AsHex(NULL, errorCode);  // xxx - lplp
        SDUDisplayLastError_TextForCode(errorCode);  // xxx - lplp
#endif
        retval = MR_ACTIVATEDEVICE_FAILED;
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Mount failed; ActivateDeviceEx returned NULL handle\n")));
        RegDetailsDeleteAllByID(regDeviceID, TRUE);
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Device activated OK\n")));
        // Pass the active handle received to the driver to store
        if (!(driver_SetUserAppHandle(
                                      (WCHAR*)(mountDetails->Mountpoint),
                                      hActiveDevice
                                     )))
            {
            // Problem!
            // <shrugs> Kill the device...
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to pass userspace device handle to device!\n")));
            DeactivateDevice(hActiveDevice);
            hActiveDevice = NULL;
            }

        }

    if (hActiveDevice != NULL) 
        {
        retval = MR_MOUNTED_OK;
        }

    // Free DLL filename as it's no longer needed
    driver_FreeMainDLLFullFilename(regdetailsBuiltin.Dll);
    regdetailsBuiltin.Dll = NULL;

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_MountDIOC_DLLOption\n")));
    return retval;
}


// =========================================================================
// Note: It it the *callers* responsibility to call CloseHandle(...) on the 
//       value returned after use
// Returns INVALID_HANDLE_VALUE on failure
HANDLE driver_GetDeviceHandle(WCHAR* deviceName)
{
    DWORD desiredAccess;
    DWORD fileFlags;
    HANDLE fileHandle;
    WCHAR pathMountpoint[FREEOTFE_MAX_FILENAME_LENGTH];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_GetDeviceHandle\n")));

    desiredAccess = 0; //(GENERIC_READ | GENERIC_WRITE);
    // Note: FILE_ATTRIBUTE_NORMAL only valid if used *alone*
    fileFlags = FILE_ATTRIBUTE_NORMAL;

    fileHandle = CreateFile(
                            deviceName,
                            desiredAccess,  
                            (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                            NULL,
                            OPEN_EXISTING, 
                            fileFlags, 
                            NULL
                           ); 

    if (fileHandle == INVALID_HANDLE_VALUE) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to open device: %ls\n"), deviceName));
        }

    SecZeroMemory(pathMountpoint, sizeof(pathMountpoint));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_GetDeviceHandle\n")));
    return fileHandle;
}


// =========================================================================
// Get handle returned by ActivateDeviceEx from device's internal store
HANDLE driver_GetUserAppHandle(WCHAR* mountpoint)
{
    HANDLE retval = NULL;
    DIOC_USER_DEVICE_HANDLE DIOCBuffer;
    DWORD bytesReturned;
    HANDLE fileHandle;
    DIOC_DISK_DEVICE_STATUS volumeDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_GetUserAppHandle\n")));

    fileHandle = INVALID_HANDLE_VALUE;
    if (driver_GetVolumeInfo_Mountpoint(mountpoint, &volumeDetails))
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Trying %ls...\n"), volumeDetails.DeviceName));
        fileHandle = driver_GetDeviceHandle(volumeDetails.DeviceName);
        SecZeroMemory(&volumeDetails, sizeof(volumeDetails));
        }

    if (fileHandle == INVALID_HANDLE_VALUE) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to driver_GetDeviceHandle failed to get valid handle.\n")));
        }
    else
        { 
        DIOCBuffer.UserSpaceDeviceHandle = 0;
        if (!(DeviceIoControl(
                                  fileHandle, 
                                  IOCTL_FREEOTFE_USER_DEV_HANDLE_GET, 
                                  NULL, 
                                  0, 
                                  &DIOCBuffer, 
                                  sizeof(DIOCBuffer), 
                                  &bytesReturned,
                                  NULL
                                )))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("DIOC returned FALSE\n")));
            }
        else if (bytesReturned != sizeof(DIOCBuffer))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("DIOC returned too-small buffer\n")));
            }
        else
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Userspace handle obtained from driver: %0.8x\n"), DIOCBuffer.UserSpaceDeviceHandle));
            retval = DIOCBuffer.UserSpaceDeviceHandle;
            }

        SecZeroMemory(&DIOCBuffer, sizeof(DIOCBuffer));

        CloseHandle(fileHandle);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_GetUserAppHandle\n")));
    return retval;
}


// =========================================================================
// Set handle returned by ActivateDeviceEx on device's internal store
BOOL driver_SetUserAppHandle(WCHAR* mountpoint, HANDLE hActiveDevice)
{
    BOOL retval = FALSE;
    DIOC_USER_DEVICE_HANDLE DIOCBuffer;
    DWORD bytesReturned;
    HANDLE fileHandle;
    DIOC_DISK_DEVICE_STATUS volumeDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_SetUserAppHandle\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Userspace handle to set: %0.8x\n"), hActiveDevice));

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Trying %ls...\n"), volumeDetails.DeviceName));
    fileHandle = INVALID_HANDLE_VALUE;
    if (driver_GetVolumeInfo_Mountpoint(mountpoint, &volumeDetails))
        {
        fileHandle = driver_GetDeviceHandle(volumeDetails.DeviceName);
        SecZeroMemory(&volumeDetails, sizeof(volumeDetails));
        }

    if (fileHandle == INVALID_HANDLE_VALUE) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to driver_GetDeviceHandle failed to get valid handle.\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Handle to opened device: 0x%0.8x\n"), fileHandle));
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Active device handle   : 0x%0.8x\n"), hActiveDevice));
        DIOCBuffer.UserSpaceDeviceHandle = hActiveDevice;
        retval = DeviceIoControl(
                                  fileHandle, 
                                  IOCTL_FREEOTFE_USER_DEV_HANDLE_SET, 
                                  &DIOCBuffer, 
                                  sizeof(DIOCBuffer), 
                                  NULL, 
                                  0, 
                                  &bytesReturned,
                                  NULL
                                );

        SecZeroMemory(&DIOCBuffer, sizeof(DIOCBuffer));

        if (retval)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Userspace handle passed successfully to device\n")));
            }
        else
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to pass userspace handle on device\n")));
            }

        CloseHandle(fileHandle);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_SetUserAppHandle\n")));
    return retval;
}


// =========================================================================
// Set handle returned by ActivateDeviceEx on device's internal store
BOOL driver_SetForceDismounts(WCHAR* mountpoint, BOOL force)
{
    BOOL retval = FALSE;
    DIOC_FORCE_DISMOUNTS DIOCBuffer;
    DWORD bytesReturned;
    HANDLE fileHandle;
    DIOC_DISK_DEVICE_STATUS volumeDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_SetForceDismounts\n")));

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Trying %ls...\n"), volumeDetails.DeviceName));
    fileHandle = INVALID_HANDLE_VALUE;
    if (driver_GetVolumeInfo_Mountpoint(mountpoint, &volumeDetails))
        {
        fileHandle = driver_GetDeviceHandle(volumeDetails.DeviceName);
        SecZeroMemory(&volumeDetails, sizeof(volumeDetails));
        }

    if (fileHandle == INVALID_HANDLE_VALUE) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to driver_GetDeviceHandle failed to get valid handle.\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Handle to opened device: 0x%0.8x\n"), fileHandle));
        DIOCBuffer.ForceDismounts = force;
        retval = DeviceIoControl(
                                  fileHandle, 
                                  IOCTL_FREEOTFE_SET_FORCE_DISMOUNT, 
                                  &DIOCBuffer, 
                                  sizeof(DIOCBuffer), 
                                  NULL, 
                                  0, 
                                  &bytesReturned,
                                  NULL
                                );

        SecZeroMemory(&DIOCBuffer, sizeof(DIOCBuffer));

        if (retval)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Set force/non-force dismount DIOC called successfully\n")));
            }
        else
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to set force/non-force dismount on driver\n")));
            }

        CloseHandle(fileHandle);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_SetForceDismounts\n")));
    return retval;
}


// =========================================================================
BOOL driver_Dismount(WCHAR* mountpoint, BOOL force)
{
    BOOL retval = FALSE;
    DIOC_DISK_DEVICE_STATUS volumeDetails;
  
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dismount: %ls\n"), mountpoint));
    if (force)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("FORCED DISMOUNT\n")));
        }
    
    if (driver_GetVolumeInfo_Mountpoint(mountpoint, &volumeDetails)) 
        {
        if (volumeDetails.ActivateDeviceHandle != NULL)
            {
            driver_SetForceDismounts(mountpoint, force);
            retval = DeactivateDevice(volumeDetails.ActivateDeviceHandle);

            if (retval)
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dismount OK\n")));
                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dismount FAILED\n")));
                }
            }
        }

    // If we could deactivate the device (if applicable), proceed to remove
    // the registry entries...
    if (retval || force) 
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Deleting registry entries...\n")));
        retval = RegDetailsDeleteAllByKey(volumeDetails.BuiltinKey, TRUE);
        }

    SecZeroMemory(&volumeDetails, sizeof(volumeDetails));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_Dismount\n")));
    return retval;
}


// =========================================================================
BOOL driver_DismountAll(BOOL force)
{
    BOOL retval = FALSE;
    BOOL allOK;
    int i;
    int cnt = 0;
    REGDETAILS_ACTIVE* mountedDetails = NULL;
    int detailsByteSize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_DismountAll\n")));

    detailsByteSize = 0;

    allOK = driver_GetMountedCount(&cnt);
    if (allOK)
        {
        detailsByteSize = (cnt * sizeof(*mountedDetails)); 
        mountedDetails = malloc(detailsByteSize);
        if (mountedDetails == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for all volumes mounted\n")));
            allOK = FALSE;
            }
        else
            {
            allOK = driver_MountedVolumes(detailsByteSize, mountedDetails);
            if (!(allOK))
                {
                SecZeroAndFreeMemory(mountedDetails, detailsByteSize);
                mountedDetails = NULL;
                }
            }
        }

    if (allOK) 
        {
        retval = TRUE;
        for(i = 0; i < cnt; i++)
            {
            allOK = driver_Dismount(mountedDetails[i].Mountpoint, force);
            if (!(allOK))
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable dismount: %ls\n"), mountedDetails[i].Mountpoint));
                }

            retval = (retval && allOK);
            }
        }

    RegDetailsFreeAllActive(cnt, mountedDetails);
    SecZeroAndFreeMemory(mountedDetails, detailsByteSize);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_DismountAll\n")));
    return retval;
}


// =========================================================================
void driver_PrettyprintSectorIVGenMethod(
    SECTOR_IV_GEN_METHOD sectorIVGenMethod, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    switch (sectorIVGenMethod)
        {
        case SCTRIVGEN_NONE:
            {
            wcsncpy(
                    buffer,
                    _("Null IV"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case SCTRIVGEN_32BIT_SECTOR_ID:
            {
            wcsncpy(
                    buffer,
                    _("32 bit sector ID"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case SCTRIVGEN_64BIT_SECTOR_ID:
            {
            wcsncpy(
                    buffer,
                    _("64 bit sector ID"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case SCTRIVGEN_HASH_32BIT_SECTOR_ID:
            {
            wcsncpy(
                    buffer,
                    _("Hashed 32 bit sector ID"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case SCTRIVGEN_HASH_64BIT_SECTOR_ID:
            {
            wcsncpy(
                    buffer,
                    _("Hashed 64 bit sector ID"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case SCTRIVGEN_ESSIV:
            {
            wcsncpy(
                    buffer,
                    _("ESSIV"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        default:
            {
            wcsncpy(
                    buffer,
                    STR_UNKNOWN,
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }
        }
}


// =========================================================================
void driver_PrettyprintSectorIVGenMethod_char(
    SECTOR_IV_GEN_METHOD sectorIVGenMethod, 
    char* buffer,
    int bufferSize  // In bytes
)
{
    WCHAR tmpBuffer[MAX_PRETTYPRINTED_TITLE];

    driver_PrettyprintSectorIVGenMethod(
                        sectorIVGenMethod,
                        tmpBuffer,
                        sizeof(tmpBuffer)
                        );
    wcstombs(buffer, tmpBuffer, bufferSize);

    SecZeroMemory(tmpBuffer, sizeof(tmpBuffer));
}


// =========================================================================
void driver_PrettyprintKDFAlgorithm(
    KDF_ALGORITHM algorithm, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    switch (algorithm)
        {
        case KDF_HASH:
            {
            wcsncpy(
                    buffer,
                    _("Hash"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case KDF_PBKDF2:
            {
            wcsncpy(
                    buffer,
                    _("PKCS #5 PBKDF2 (HMAC)"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        default:
            {
            wcsncpy(
                    buffer,
                    STR_UNKNOWN,
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }
        }
}


// =========================================================================
void driver_PrettyprintMACAlgorithm(
    MAC_ALGORITHM algorithm, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    switch (algorithm)
        {
        case MAC_HASH:
            {
            wcsncpy(
                    buffer,
                    _("Hash"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        case MAC_HMAC:
            {
            wcsncpy(
                    buffer,
                    _("HMAC"),
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }

        default:
            {
            wcsncpy(
                    buffer,
                    STR_UNKNOWN,
                    (bufferSize / sizeof(buffer[0]))
                   );
            break;
            }
        }
}


// =========================================================================
// =========================================================================
