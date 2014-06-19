// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include <storemgr.h>  // Required for STORAGE_DEVICE_TYPE_PCCARD, etc

#include "SDUGeneral.h"
#include "SDUHexDump.h"

#include "FreeOTFENULLGUID.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "LUKS.h"

#include <windev.h>  // Required for use in Diskio.h (CTL_CODE definition)
#include <Diskio.h>  // Required for SG_REQ definition


// =========================================================================
// Constants...

// In *bits*
#define CDB_MAX_MAC_LENGTH   512 

// These are artifical maximums; they must be enough to store the results of
// the DIOC calls when the amount of data output is variable 
// In *bits*
#define MAX_DERIVED_KEY_LENGTH  (1024 * 1024)
// In *bits*
#define MAX_MAC_LENGTH          (1024 * 1024)


// In *bits*
#define MML 512

// The CDB version ID to be used in all new CDBs
#define CDB_FORMAT_ID 4

// Old CDB versions
#define CDB_FORMAT_ID_3 3

// Most appropriate device type...
// Although not clear in the documentation, this *appears* to be a bitmask:
//
// STORAGE_DEVICE_TYPE_PCIIDE             00000000000000000000000000000001
// STORAGE_DEVICE_TYPE_FLASH              00000000000000000000000000000010
// STORAGE_DEVICE_TYPE_ATA                00000000000000000000000000000100
// STORAGE_DEVICE_TYPE_ATAPI              00000000000000000000000000010000
// STORAGE_DEVICE_TYPE_PCCARD             00000000000000000000000000100000
// STORAGE_DEVICE_TYPE_CFCARD             00000000000000000000000001000000
// STORAGE_DEVICE_TYPE_SRAM               00000000000000000000000010000000
// STORAGE_DEVICE_TYPE_DVD                00000000000000000000000100000000
// STORAGE_DEVICE_TYPE_CDROM              00000000000000000000001000000000
// STORAGE_DEVICE_TYPE_USB                00000000000000000000010000000000
// STORAGE_DEVICE_TYPE_1394               00000000000000000000100000000000
// STORAGE_DEVICE_TYPE_DOC                00000000000000000001000000000000
// STORAGE_DEVICE_TYPE_UNKNOWN            00100000000000000000000000000000
// Drive itself is removable
// STORAGE_DEVICE_TYPE_REMOVABLE_DRIVE    01000000000000000000000000000000
// Just the media is removable ex.
// CDROM, FLOPPY
// STORAGE_DEVICE_TYPE_REMOVABLE_MEDIA    10000000000000000000000000000000
//
// From testing physical cards:
//
// SD card:
// 0xa1fc6760                             10100001111111000110011101100000
//
// CF card:
// 0x40000024                             01000000000000000000000000100100

// We simulate something reasonable...
#define DEFAULT_MOUNT_DEVICETYPE  (STORAGE_DEVICE_TYPE_PCCARD | STORAGE_DEVICE_TYPE_REMOVABLE_DRIVE)


// =========================================================================
// Forward declarations...
void driver_UnloadMainDLL(MODULE_DETAILS_MAIN* DLLDetails);

BOOL
_driver_BackupRestoreCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    WCHAR* DestFilename,
    LARGE_INTEGER DestOffset
);

DIOC_MOUNT* _driver_AllocateDIOCMount(
    BOOL FullMount,

    WCHAR* Mountpoint,
    WCHAR* Filename,
    BOOL ReadOnly,

    LARGE_INTEGER Offset,
    LARGE_INTEGER PartitionSize,

    FREEOTFEBYTE* MasterKey,
    ULONG MasterKeyLength,  // In *bits*
    FREEOTFEBYTE* VolumeIV,
    ULONG VolumeIVLength,  // In *bits*
    SECTOR_IV_GEN_METHOD SectorIVGenMethod,
    WCHAR* IVHashDriver,
    GUID IVHashGUID,
    WCHAR* IVCypherDriver,
    GUID IVCypherGUID,
    WCHAR* MainCypherDriver,
    GUID MainCypherGUID,

    BOOL RevertVolTimestamps, 
    unsigned int VolumeFlags,

    FREEOTFEBYTE* Metadata,
    ULONG MetadataLength,  // In bytes

    int* DIOCMountSize
);

void _driver_FreeDIOCMount(
    DIOC_MOUNT* DIOCMount,
    int DIOCMountSize
);


// =========================================================================
// Note: Ignores any random padding data
BOOL _driver_ParseVolumeDetailsBlock(
    unsigned int rawDataSize,  // In bytes
    FREEOTFEBYTE* rawData,
    CDB_DETAILS* volumeDetailsBlock
)
{
    BOOL allOK = TRUE;
    FREEOTFEBYTE* ptr;
    int CDBFormatID;
    LARGE_INTEGER tmpLargeInteger;
    DWORD tmpDWORD;
    char tmpChar;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_ParseVolumeDetailsBlock\n")));

    ptr = rawData;

    if (allOK)
        {
        // 8 bits: CDB Format ID...
        ptr = SDUUnpack_char(ptr, &tmpChar);
        CDBFormatID = tmpChar;
		volumeDetailsBlock->CDBFormatID = CDBFormatID;

        // We only support CDB v3 format and later (i.e. Desktop FreeOTFE
        // v00.54.00 and later)
        if (CDBFormatID < CDB_FORMAT_ID_3)
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Unsupported CDB version (CDB is version: %d)\n"), CDBFormatID));
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        // 32 bits: Volume flags...
        ptr = SDUUnpack_DWORD(ptr, &tmpDWORD);
        volumeDetailsBlock->VolumeFlags = tmpDWORD;
        }

    if (allOK)
        {
        // 64 bits: Partition length...
        ptr = SDUUnpack_LARGE_INTEGER(ptr, &tmpLargeInteger);
        volumeDetailsBlock->PartitionSize.QuadPart = tmpLargeInteger.QuadPart;
        }

    if (allOK)
        {
        // 32 bits: Master key length...
        ptr = SDUUnpack_DWORD(ptr, &tmpDWORD);
        volumeDetailsBlock->MasterKeyLength = tmpDWORD;

        // Note: If decrypted badly, can't rely on: criticalData.MasterKeyLen to be
        //       valid
        //       We check if the volume details block's master key length implies
        //       would put idx beyond the amount of data we have
        if (((ptr - rawData) + (volumeDetailsBlock->MasterKeyLength / 8)) >
              rawDataSize) 
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Invalid MasterKeyLength in CDB (length: %d *bits*)\n"), volumeDetailsBlock->MasterKeyLength));
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        // Variable bits: Master key...
        volumeDetailsBlock->MasterKey = malloc((volumeDetailsBlock->MasterKeyLength / 8));
        if (volumeDetailsBlock->MasterKey == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for MasterKey (length: %d *bits*)\n"), volumeDetailsBlock->MasterKeyLength));
            allOK = FALSE;
            }
        else
            {
            memcpy(volumeDetailsBlock->MasterKey, ptr, (volumeDetailsBlock->MasterKeyLength / 8));
            ptr += (volumeDetailsBlock->MasterKeyLength / 8);
            }
        }

    if (allOK)
        {
        // 8 bits: Requested drive letter...
        // Not used under WinCE, but preserved in case user wants to change
        // password
        ptr = SDUUnpack_char(ptr, &tmpChar);
        volumeDetailsBlock->DefaultDrive = tmpChar;
        }

    if (allOK)
        {
        // 32 bits: Volume IV length...
        ptr = SDUUnpack_DWORD(ptr, &tmpDWORD);
        volumeDetailsBlock->VolumeIVLength = tmpDWORD;

        // Note: If decrypted badly, can't rely on: criticalData.VolumeIVLength to be
        //       valid
        //       We check if the volume details block's master key length implies
        //       would put idx beyond the amount of data we have
        if (((ptr - rawData) + (volumeDetailsBlock->VolumeIVLength / 8)) >
              rawDataSize) 
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Invalid VolumeIVLength in CDB (length: %d *bits*)\n"), volumeDetailsBlock->VolumeIVLength));
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        // Variable bits: Volume IV...
        volumeDetailsBlock->VolumeIV = malloc((volumeDetailsBlock->VolumeIVLength / 8));
        if (volumeDetailsBlock->VolumeIV == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for VolumeIV (length: %d *bits*)\n"), volumeDetailsBlock->VolumeIVLength));
            allOK = FALSE;
            }
        else
            {
            memcpy(volumeDetailsBlock->VolumeIV, ptr, (volumeDetailsBlock->VolumeIVLength / 8));
            ptr += (volumeDetailsBlock->VolumeIVLength / 8);
            }
        }

    if (allOK)
        {
        // 8 bits: Sector IV generation method...
        ptr = SDUUnpack_char(ptr, &tmpChar);
        volumeDetailsBlock->SectorIVGenMethod = (SECTOR_IV_GEN_METHOD)tmpChar;
        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_ParseVolumeDetailsBlock\n")));
    return allOK;
}


// =========================================================================
// Key derivation function
// Note: *Caller* is responsible for freeing of DK
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
)
{
    BOOL retval = FALSE;
    DIOC_DERIVE_KEY_IN* DIOCIn;
    DIOC_DERIVE_KEY_OUT* DIOCOut;
    DWORD DIOCInSize;
    DWORD DIOCOutSize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_DeriveKey\n")));

    DIOCInSize = (
                  (
                   sizeof(*DIOCIn) - 
                   sizeof(DIOCIn->Password)
                  ) - 
                  sizeof(DIOCIn->Salt)
                 ) + 
                 PasswordLength +
                 (SaltLength / 8);

    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    DIOCOutSize = sizeof(*DIOCOut) -
                  sizeof(DIOCOut->DerivedKey) +
                  (max(MAX_DERIVED_KEY_LENGTH, dkLenBits) / 8);

    DIOCIn = malloc(DIOCInSize);
    DIOCOut = malloc(DIOCOutSize);


    if (
        (DIOCIn != NULL) &&
        (DIOCOut != NULL) 
       ) 
        {
        DIOCIn->KDFAlgorithm = kdfAlgorithm;
        wcscpy(DIOCIn->HashDeviceName, hashKernelModeDeviceName);
        DIOCIn->HashGUID = hashGUID;
        wcscpy(DIOCIn->CypherDeviceName, cypherKernelModeDeviceName);
        DIOCIn->CypherGUID = cypherGUID;
        DIOCIn->Iterations = Iterations;
        DIOCIn->LengthWanted = dkLenBits;
        DIOCIn->PasswordLength = (PasswordLength * 8);
        DIOCIn->SaltLength = SaltLength;
        memcpy(DIOCIn->Password, Password, PasswordLength);
        memcpy((DIOCIn->Password + PasswordLength), Salt, (SaltLength / 8));

        if (mainDriver->FnDeriveKey(
                                    DIOCInSize,
                                    DIOCIn,
                                    DIOCOutSize,
                                    DIOCOut
                                   ) == ERROR_SUCCESS)
            {
            if (DIOCOut->DerivedKeyLength >= dkLenBits)
                {                
                memcpy(DK, DIOCOut->DerivedKey, (dkLenBits / 8));
                retval = TRUE;
                }
            }
        }

    SecZeroAndFreeMemory(DIOCIn, DIOCInSize);
    SecZeroAndFreeMemory(DIOCOut, DIOCOutSize);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_DeriveKey\n")));
    return retval;
}


// =========================================================================
// Generate MAC of data using hash driver/encryption driver identified
// Note: "tBits" is in *bits* not *bytes*
// tBits - Set the the number of bits to return; set to -ve value for no
//         truncation/padding
//         If tBits is larger than the number of bits the MAC would normally
//         be, then the MAC will be right-padded with NULLs
//         If tBits is set to a -ve number, then this function will return
//         an MAC of variable length
//         This will be set to the length of MACOut on exit
// MACOut - This will be set to point to the MAC generated. This function
//          allocates memory for the MAC; it is is *callers* responsibility to
//          free this off after use
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
)
{
    BOOL retval = FALSE;
    DIOC_GENERATE_MAC_IN* DIOCIn;
    DIOC_GENERATE_MAC_OUT* DIOCOut;
    DWORD DIOCInSize;
    DWORD DIOCOutSize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_MACData\n")));

    DIOCInSize = sizeof(*DIOCIn) - 
                 sizeof(DIOCIn->Key) - 
                 sizeof(DIOCIn->Data) + 
                 (KeyLength / 8) +
                 (DataLength / 8);

    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    DIOCOutSize = (sizeof(*DIOCOut) - sizeof(DIOCOut->MAC)) +
                  (max(MAX_MAC_LENGTH, *tBits) / 8);

    DIOCIn = malloc(DIOCInSize);
    DIOCOut = malloc(DIOCOutSize);

    if (
        (DIOCIn != NULL) &&
        (DIOCOut != NULL) 
       ) 
        {
        DIOCIn->MACAlgorithm = macAlgorithm;
        wcscpy(DIOCIn->HashDeviceName, hashKernelModeDeviceName);
        DIOCIn->HashGUID = hashGUID;
        wcscpy(DIOCIn->CypherDeviceName, cypherKernelModeDeviceName);
        DIOCIn->CypherGUID = cypherGUID;
        DIOCIn->LengthWanted = *tBits;
        DIOCIn->KeyLength = KeyLength;
        DIOCIn->DataLength = DataLength;
        memcpy(DIOCIn->Key, Key, (KeyLength / 8));
        memcpy((DIOCIn->Key + (KeyLength / 8)), Data, (DataLength / 8));

        if (mainDriver->FnMACData(
                                  DIOCInSize,
                                  DIOCIn,
                                  DIOCOutSize,
                                  DIOCOut
                                 ) == ERROR_SUCCESS)
            {
            if ((int)DIOCOut->MACLength >= *tBits)
                {                
                *MACOut = malloc((DIOCOut->MACLength / 8));
                if (*MACOut == NULL)
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc MAC return buffer\n")));
                    }
                else
                    {
                    *tBits = DIOCOut->MACLength;                
                    memcpy(*MACOut, DIOCOut->MAC, (DIOCOut->MACLength / 8));
                    retval = TRUE;
                    }
                }
            }
        }

    
    SecZeroAndFreeMemory(DIOCIn, DIOCInSize);
    SecZeroAndFreeMemory(DIOCOut, DIOCOutSize);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_MACData\n")));
    return retval;
}


// =========================================================================
// ReadNotWrite - Set to TRUE to read, FALSE to write
BOOL _driver_ReadWriteRawCDB(
    WCHAR* Filename,
    LARGE_INTEGER Offset,
    BOOL ReadNotWrite,
    FREEOTFEBYTE (*CDB)[CRITICAL_DATA_LENGTH / 8]
)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_ReadWriteRawCDB\n")));

    retval = SDUReadWriteFile(
                             Filename,
                             Offset,
                             ReadNotWrite,
                             sizeof(*CDB),
                             (BYTE*)CDB
                            );

    if (retval)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB write OK\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB write FAILED.\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_ReadWriteRawCDB\n")));
    return retval;
}


// =========================================================================
BOOL driver_LoadMainDLL(MODULE_DETAILS_MAIN* DLLDetails)
{
    BOOL retval = TRUE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_LoadMainDLL\n")));

    // Get library path and filename...
    if (retval)
        {
        DLLDetails->Filename = driver_AllocMainDLLFullFilename(TRUE);
        if (DLLDetails->Filename == NULL)
            {
            retval = FALSE;
            }
        }

    // Load library...
    if (retval)
        {
        DLLDetails->Lib = LoadLibrary(DLLDetails->Filename);
        if (DLLDetails->Lib == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to load library\n")));
            retval = FALSE;
            }
        }

    // Get library function address...
    if (retval)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_INIT));
        DLLDetails->FnDSK_Init = (PMainDLLFnDSK_Init)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_INIT
                                        );
        if (DLLDetails->FnDSK_Init == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_DEINIT));
        DLLDetails->FnDSK_Deinit = (PMainDLLFnDSK_Deinit)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_DEINIT
                                        );
        if (DLLDetails->FnDSK_Deinit == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_OPEN));
        DLLDetails->FnDSK_Open = (PMainDLLFnDSK_Open)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_OPEN
                                        );
        if (DLLDetails->FnDSK_Open == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_CLOSE));
        DLLDetails->FnDSK_Close = (PMainDLLFnDSK_Close)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_CLOSE
                                        );
        if (DLLDetails->FnDSK_Close == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_IOCONTROL));
        DLLDetails->FnDSK_IOControl = (PMainDLLFnDSK_IOControl)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_IOCONTROL
                                        );
        if (DLLDetails->FnDSK_IOControl == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_SEEK));
        DLLDetails->FnDSK_Seek = (PMainDLLFnDSK_Seek)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_SEEK
                                        );
        if (DLLDetails->FnDSK_Seek == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_READ));
        DLLDetails->FnDSK_Read = (PMainDLLFnDSK_Read)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_READ
                                        );
        if (DLLDetails->FnDSK_Read == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_WRITE));
        DLLDetails->FnDSK_Write = (PMainDLLFnDSK_Write)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_WRITE
                                        );
        if (DLLDetails->FnDSK_Write == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_POWERDOWN));
        DLLDetails->FnDSK_PowerDown = (PMainDLLFnDSK_PowerDown)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_POWERDOWN
                                        );
        if (DLLDetails->FnDSK_PowerDown == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DSK_POWERUP));
        DLLDetails->FnDSK_PowerUp = (PMainDLLFnDSK_PowerUp)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DSK_POWERUP
                                        );
        if (DLLDetails->FnDSK_PowerUp == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }



        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_DERIVEKEY));
        DLLDetails->FnDeriveKey = (PMainDLLFnDeriveKey)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_DERIVEKEY
                                        );
        if (DLLDetails->FnDeriveKey == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_MAIN_MACDATA));
        DLLDetails->FnMACData = (PMainDLLFnMACData)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_MAIN_MACDATA
                                        );
        if (DLLDetails->FnMACData == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        }


    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        driver_UnloadMainDLL(DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_LoadMainDLL\n")));
    return retval;
}


// =========================================================================
void driver_UnloadMainDLL(MODULE_DETAILS_MAIN* DLLDetails)
{
    if (DLLDetails != NULL)
        {
     	// DLL handle
        if ((DLLDetails->Lib) != NULL)
            {
	        FreeLibrary(DLLDetails->Lib);
            DLLDetails->Lib = NULL;
            }

        driver_FreeMainDLLFullFilename(DLLDetails->Filename);
        }

    SecZeroMemory(DLLDetails, sizeof(*DLLDetails));
}


// =========================================================================
// Note: It is the *callers* responsibility to free of the values returned
BOOL driver_GetAllAlgorithmDriverDetails(
    int* countDriversHash,
    WCHAR*** driverFilenamesHash,
    HASH_DRIVER_INFO*** driverInfoHash,

    int* countDriversCypher,
    WCHAR*** driverFilenamesCypher,
    CYPHER_DRIVER_INFO_v3*** driverInfoCypher,

    BOOL ExcludeDisabledDrivers
)
{
    BOOL allOK = TRUE;
    int i;
    LNKLIST_DISABLED_DRIVER* excludeDriverList;

    // Exclude hash drivers?
    excludeDriverList = NULL;
    if (ExcludeDisabledDrivers)
        {
        excludeDriverList = G_driver_DisabledHashDrivers;
        }
    // Identify all hash drivers...
    // !! WARNING !!
    // IF OPTIMISATION IS TURNED ON, THIS CALL OVERWRITES allOK?!!
    *countDriversHash = driver_GetDriverFilenames(
                                          FREEOTFE_DLL_PATTERN_HASH,
                                          driverFilenamesHash,
                                          excludeDriverList
                                         );
    if (*countDriversHash < 0) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to obtain list of hash drivers\n")));
        allOK = FALSE;
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("List of hash drivers found (total: %d): \n"), *countDriversHash));
        for(i = 0; i < *countDriversHash; i++)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Hash driver %d: %ls\n"), (i+1), (*driverFilenamesHash)[i]));
            }
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("End listing hash drivers.\n")));
        }

    // Exclude cypher drivers?
    excludeDriverList = NULL;
    if (ExcludeDisabledDrivers)
        {
        excludeDriverList = G_driver_DisabledCypherDrivers;
        }
    // Identify all cypher drivers...
    *countDriversCypher = driver_GetDriverFilenames(
                                          FREEOTFE_DLL_PATTERN_CYPHER,
                                          driverFilenamesCypher,
                                          excludeDriverList
                                         );
    if (*countDriversCypher < 0) 
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to obtain list of cypher drivers\n")));
        allOK = FALSE;
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("List of cypher drivers found (total: %d): \n"), *countDriversCypher));
        for(i = 0; i < *countDriversCypher; i++)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Cypher driver %d: %ls\n"), (i+1), (*driverFilenamesCypher)[i]));
            }
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("End listing cypher drivers.\n")));
        }


    // Identify all hash algorithms supported by each of the hash drivers
    *driverInfoHash = calloc(*countDriversHash, sizeof(**driverInfoHash));
    if (*driverInfoHash == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc driverInfoHash ARRAY\n")));
        allOK = FALSE;
        }
    else
        {
        for(i = 0; i < *countDriversHash; i++)
            {
            (*driverInfoHash)[i] = malloc(sizeof(*((*driverInfoHash)[0])));
            if ((*driverInfoHash)[i] == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc driverInfoHash individual\n")));
                allOK = FALSE;
                }
            else
                {
                if (!(driver_HashGetDriverDetails(
                                            (*driverFilenamesHash)[i],
                                            (*driverInfoHash)[i]
                                           )))
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("No driver details obtained for hash DLL\n")));
                    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Hash DLL was: %ls\n"), (*driverFilenamesHash)[i]));
                    SecZeroAndFreeMemory(
                                         (*driverInfoHash)[i], 
                                         sizeof(*((*driverInfoHash)[0]))
                                        );
                    (*driverInfoHash)[i] = NULL;
                    }
                }
            }
        }


    // Identify all cypher algorithms supported by each of the cypher drivers
    *driverInfoCypher = calloc(*countDriversCypher, sizeof(**driverInfoCypher));
    if (*driverInfoCypher == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc driverInfoCypher ARRAY\n")));
        allOK = FALSE;
        }
    else
        {
        for(i = 0; i < *countDriversCypher; i++)
            {
            (*driverInfoCypher)[i] = malloc(sizeof(*((*driverInfoCypher)[0])));
            if ((*driverInfoCypher)[i] == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc driverInfoCypher individual\n")));
                allOK = FALSE;
                }
            else
                {
                if (!(driver_CypherGetDriverDetails(
                                            (*driverFilenamesCypher)[i],
                                            (*driverInfoCypher)[i]
                                           )))
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("No driver details obtained for cypher DLL\n")));
                    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Cypher DLL was: %ls\n"), (*driverFilenamesCypher)[i]));
                    SecZeroAndFreeMemory(
                                         (*driverInfoCypher)[i], 
                                         sizeof(*((*driverInfoCypher)[0]))
                                        );
                    (*driverInfoCypher)[i] = NULL;
                    }
                }
            }
        }


    return allOK;
}

// =========================================================================
void driver_FreeAllAlgorithmDriverDetails(
    int* countDriversHash,
    WCHAR*** driverFilenamesHash,
    HASH_DRIVER_INFO*** driverInfoHash,

    int* countDriversCypher,
    WCHAR*** driverFilenamesCypher,
    CYPHER_DRIVER_INFO_v3*** driverInfoCypher
)
{
    int i;

    for(i = 0; i < *countDriversHash; i++)
        {
        driver_HashFreeDriverDetails((*driverInfoHash)[i]);

        SecZeroAndFreeMemory(
                             (*driverInfoHash)[i], 
                             sizeof(*((*driverInfoHash)[0]))
                            );
        }
    SecZeroAndFreeMemory(
                  *driverInfoHash, 
                  ((*countDriversHash) * sizeof(*(*driverInfoHash)))
                 );

    for(i = 0; i < *countDriversCypher; i++)
        {
        driver_CypherFreeDriverDetails((*driverInfoCypher)[i]);

        SecZeroAndFreeMemory(
                             (*driverInfoCypher)[i], 
                             sizeof(*((*driverInfoCypher)[0]))
                            );
        }
    SecZeroAndFreeMemory(
                  *driverInfoCypher, 
                  ((*countDriversCypher) * sizeof(*(*driverInfoCypher)))
                 );

    driver_FreeDriverFilenames(driverFilenamesHash);
    driver_FreeDriverFilenames(driverFilenamesCypher);

    *driverFilenamesHash = NULL;
    *driverInfoHash = NULL;
    *driverFilenamesCypher = NULL;
    *driverInfoCypher = NULL;

    *countDriversHash = 0;
    *countDriversCypher = 0;
}


// =========================================================================
// Count number of hash/cypher implementations
// Returns CountImplHash and CountImplCypher
void _driver_GetAllAlgorithmDriverOptions_Count(
    WCHAR** driverFilenamesHash,
    HASH_DRIVER_INFO** driverInfoHash,

    WCHAR** driverFilenamesCypher,
    CYPHER_DRIVER_INFO_v3** driverInfoCypher,

    int* CountImplHash,
    int* CountImplCypher,

    BOOL ExcludeDisabledDrivers
)
{
    int hashDriverIdx;
    int cypherDriverIdx;
    HASH_DRIVER_INFO* currDriverInfoHash;
    CYPHER_DRIVER_INFO_v3* currDriverInfoCypher;

    *CountImplHash = 0;
    hashDriverIdx = 0;
    while (driverFilenamesHash[hashDriverIdx] != NULL)
        {
        currDriverInfoHash = driverInfoHash[hashDriverIdx];

        // If we got the DLL filename, but no hash details, skip this DLL file
        if (currDriverInfoHash != NULL)
            {
            *CountImplHash += currDriverInfoHash->HashCount;
            }

        hashDriverIdx++;
        }


    *CountImplCypher = 0;
    cypherDriverIdx = 0;
    while (driverFilenamesCypher[cypherDriverIdx] != NULL)
        {
        currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];

        // If we got the DLL filename, but no cypher details, skip this DLL file
        if (currDriverInfoCypher != NULL)
            {
            *CountImplCypher += currDriverInfoCypher->CypherCount;
            }

        cypherDriverIdx++;
        }

}


// =========================================================================
// Note: It is up the *caller* to free off the returned values
//       e.g. By driver_FreeAllAlgorithmDriverOptions(...)
BOOL driver_GetAllAlgorithmDriverOptions(
    int* CountImplHash,
    DRIVER_AND_ALG** HashImpl,

    int* CountImplCypher,
    DRIVER_AND_ALG** CypherImpl,

    BOOL ExcludeDisabledDrivers
)
{
    BOOL allOK = FALSE;

    int countDriversHash;
    int countDriversCypher;
    WCHAR** driverFilenamesHash;
    WCHAR** driverFilenamesCypher;
    HASH_DRIVER_INFO** driverInfoHash;
    CYPHER_DRIVER_INFO_v3** driverInfoCypher;

    int hashDriverIdx;
    int cypherDriverIdx;
    unsigned int hashImplIdx;
    unsigned int cypherImplIdx;

    WCHAR* currHashDriverFilename;
    WCHAR* currCypherDriverFilename;
    HASH_DRIVER_INFO* currDriverInfoHash;
    CYPHER_DRIVER_INFO_v3* currDriverInfoCypher;
    HASH* currHashImpl;
    CYPHER_v3* currCypherImpl;

    int implIdx;

    if (driver_GetAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher,

                                        ExcludeDisabledDrivers
                                        ))
        {
        _driver_GetAllAlgorithmDriverOptions_Count(
                                                   driverFilenamesHash,
                                                   driverInfoHash,

                                                   driverFilenamesCypher,
                                                   driverInfoCypher,

                                                   CountImplHash,
                                                   CountImplCypher,

                                                   ExcludeDisabledDrivers
                                                  );

        // At this point, we know exactly how many cypher/hash algorithms
        // there are.
        // Malloc storage to hold their details, and populate.

        *HashImpl = calloc(*CountImplHash, sizeof(DRIVER_AND_ALG));
        *CypherImpl = calloc(*CountImplCypher, sizeof(DRIVER_AND_ALG));

        if (
            (*HashImpl == NULL) ||
            (*CypherImpl == NULL)
           )
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for implementation summary\n")));
            }
        else
            {
            implIdx = 0;
            cypherDriverIdx = 0;
            while (driverFilenamesCypher[cypherDriverIdx] != NULL)
                {
                currCypherDriverFilename = driverFilenamesCypher[cypherDriverIdx];
                currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];

                // If we got the DLL filename, but no cypher details, skip this DLL file
                if (currDriverInfoCypher == NULL)
                    {
                    DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Optimisation skipping cypher DLL filename due to lack of driver details\n")));
                    cypherDriverIdx++;
                    continue;
                    }

                for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)
                    {
                    currCypherImpl = &(currDriverInfoCypher->CypherDetails[cypherImplIdx]);

                    driver_CypherPrettyprintAlgTitle(
                                                     currCypherImpl, 
                                                     (*CypherImpl)[implIdx].PrettyName,
                                                     sizeof((*CypherImpl)[implIdx].PrettyName)
                                                    );

                    wcscpy(
                           (*CypherImpl)[implIdx].DriverFilename,
                           currCypherDriverFilename
                          );
                    (*CypherImpl)[implIdx].DriverImplAlgGUID = currCypherImpl->CypherGUID;

                    (*CypherImpl)[implIdx].Cypher = *currCypherImpl;

                    implIdx++;
                    }

                cypherDriverIdx++;
                }


            implIdx = 0;
            hashDriverIdx = 0;
            while (driverFilenamesHash[hashDriverIdx] != NULL)
                {
                currHashDriverFilename = driverFilenamesHash[hashDriverIdx];
                currDriverInfoHash = driverInfoHash[hashDriverIdx];

                // If we got the DLL filename, but no hash details, skip this DLL file
                if (currDriverInfoHash == NULL)
                    {
                    DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Optimisation skipping hash DLL filename due to lack of driver details\n")));
                    hashDriverIdx++;
                    continue;
                    }

                for (hashImplIdx = 0; hashImplIdx < currDriverInfoHash->HashCount; hashImplIdx++)
                    {
                    currHashImpl = &(currDriverInfoHash->HashDetails[hashImplIdx]);

                    driver_HashPrettyprintAlgTitle(
                                                     currHashImpl, 
                                                     (*HashImpl)[implIdx].PrettyName,
                                                     sizeof((*HashImpl)[implIdx].PrettyName)
                                                    );

                    wcscpy(
                           (*HashImpl)[implIdx].DriverFilename,
                           currHashDriverFilename
                          );
                    (*HashImpl)[implIdx].DriverImplAlgGUID = currHashImpl->HashGUID;

                    (*HashImpl)[implIdx].Hash = *currHashImpl;

                    implIdx++;
                    }

                hashDriverIdx++;
                }


            allOK = TRUE;
            }

        }

        
    // Cleanup...
    driver_FreeAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher
                                        );
    if (!(allOK))
        {
        driver_FreeAllAlgorithmDriverOptions(
                                            CountImplHash,
                                            HashImpl,

                                            CountImplCypher,
                                            CypherImpl
                                           );
        }

    return allOK;
}


// =========================================================================
void driver_FreeAllAlgorithmDriverOptions(
    int* CountImplHash,
    DRIVER_AND_ALG** HashImpl,

    int* CountImplCypher,
    DRIVER_AND_ALG** CypherImpl
)
{
    SecZeroAndFreeMemory(
                         *HashImpl,
                         ((*CountImplHash) * sizeof(DRIVER_AND_ALG))
                        );
    SecZeroAndFreeMemory(
                         *CypherImpl,
                         ((*CountImplCypher) * sizeof(DRIVER_AND_ALG))
                        );

    *HashImpl = NULL;
    *CypherImpl = NULL;

    *CountImplHash = 0;
    *CountImplCypher = 0;
}


// =========================================================================
// Cleardown any dump data
void driver_FreeCriticalDump()
{
	SecZeroAndFreeMemory(
		                 _driver_DumpCriticalDataKey,
						 (_driver_DumpCriticalDataKeyBits / 8)
						);
	SecZeroAndFreeMemory(
		                 _driver_DumpCheckMAC,
						 (_driver_DumpCheckMACBits / 8)
						);
	SecZeroAndFreeMemory(
		                 _driver_DumpPlaintextEncryptedBlock,
						 (_driver_DumpPlaintextEncryptedBlockBits / 8)
						);
	SecZeroAndFreeMemory(
		                 _driver_DumpVolumeDetailsBlock,
						 (_driver_DumpVolumeDetailsBlockBits / 8)
						);

	_driver_DumpCriticalDataKeyBits = 0;
	_driver_DumpCriticalDataKey = NULL;
	_driver_DumpCheckMACBits = 0;
	_driver_DumpCheckMAC = NULL;
	_driver_DumpPlaintextEncryptedBlockBits = 0;
	_driver_DumpPlaintextEncryptedBlock = NULL;
	_driver_DumpVolumeDetailsBlockBits = 0;
	_driver_DumpVolumeDetailsBlock = NULL;

	_driver_DumpFlag = FALSE;
}


// =========================================================================
// Store any dump data
_driver_StoreCriticalDump(
	int CriticalDataKeyBits,
	FREEOTFEBYTE* CriticalDataKey,
	int CheckMACBits,
	FREEOTFEBYTE* CheckMAC,
	int PlaintextEncryptedBlockBits,
	FREEOTFEBYTE* PlaintextEncryptedBlock,
	int VolumeDetailsBlockBits,
	FREEOTFEBYTE* VolumeDetailsBlock
)
{
	// Additional: Store internal information, if dumping
	if (_driver_DumpFlag) 
		{
		// Store sizes and allocate storage...

		// Critical data key...
		_driver_DumpCriticalDataKeyBits = CriticalDataKeyBits;
		_driver_DumpCriticalDataKey = malloc(_driver_DumpCriticalDataKeyBits / 8);

		// Check MAC...
		_driver_DumpCheckMACBits = CheckMACBits;
		_driver_DumpCheckMAC = malloc(_driver_DumpCheckMACBits / 8);

		// Encrypted block...
		_driver_DumpPlaintextEncryptedBlockBits = PlaintextEncryptedBlockBits;
		_driver_DumpPlaintextEncryptedBlock = malloc(_driver_DumpPlaintextEncryptedBlockBits / 8);

		// Volume details block...
		_driver_DumpVolumeDetailsBlockBits = VolumeDetailsBlockBits;
		_driver_DumpVolumeDetailsBlock = malloc(_driver_DumpVolumeDetailsBlockBits / 8);

		// Check OK
		if (
			(_driver_DumpCriticalDataKey == NULL) ||
			(_driver_DumpCheckMAC == NULL) ||
			(_driver_DumpPlaintextEncryptedBlock == NULL) ||
			(_driver_DumpVolumeDetailsBlock == NULL)
		   )
			{
			driver_FreeCriticalDump();
			}
		else
			{
			// Store dump...

			// Critical data key...
			memcpy(
				   _driver_DumpCriticalDataKey, 
				   CriticalDataKey, 
				   (_driver_DumpCriticalDataKeyBits / 8)
				  );

			// Check MAC...
			memcpy(
				   _driver_DumpCheckMAC, 
				   CheckMAC, 
				   (_driver_DumpCheckMACBits / 8)
				  );

			// Encrypted block...
			memcpy(
				   _driver_DumpPlaintextEncryptedBlock, 
				   PlaintextEncryptedBlock, 
				   (_driver_DumpPlaintextEncryptedBlockBits / 8)
				  );
		    
			// Volume details block...
			memcpy(
				   _driver_DumpVolumeDetailsBlock, 
				   VolumeDetailsBlock, 
				   (_driver_DumpVolumeDetailsBlockBits / 8)
				  );
			}
		}

}


// =========================================================================
// Read and decrypt v2/v3 format CDB
BOOL _driver_ReadVolumeCriticalData_v2_v3(
    WCHAR* Filename,
    LARGE_INTEGER OffsetWithinFile,
    char* UserPassword,
    unsigned int SaltLength,  // In *bits*
    unsigned int KeyIterations,

    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
)
{
    // Pointer to a list of strings (pointers to strings)
    WCHAR** driverFilenamesHash;
    WCHAR** driverFilenamesCypher;
    int countDriversHash;
    int countDriversCypher;
    MODULE_DETAILS_MAIN mainDriver;
    KDF_ALGORITHM currKDFAlg;
    MAC_ALGORITHM currMACAlg;
    int hashDriverIdx;
    int cypherDriverIdx;
    unsigned int hashImplIdx;
    unsigned int cypherImplIdx;
    WCHAR* currHashDriverFilename;
    WCHAR* currCypherDriverFilename;
    // Pointer to a list of algorithms the drivers support (pointers to structs)
    HASH_DRIVER_INFO** driverInfoHash;
    CYPHER_DRIVER_INFO_v3** driverInfoCypher;
    HASH_DRIVER_INFO* currDriverInfoHash;
    CYPHER_DRIVER_INFO_v3* currDriverInfoCypher;
    HASH* currHashImpl;
    CYPHER_v3* currCypherImpl;
    WCHAR* tmpGUIDStr;
    int maxCDKSize_bits;
    int maxIVSize_bits;
    int cdkSize_bits;
    unsigned int criticalDataKeySize;
    FREEOTFEBYTE* criticalDataKey;
    int leb_bits;
    FREEOTFEBYTE* IV;
    FREEOTFEBYTE* salt;
    FREEOTFEBYTE* encryptedBlock;
    FREEOTFEBYTE plaintextEncryptedBlock[CRITICAL_DATA_LENGTH / 8];
    BOOL allOK;
    FREEOTFEBYTE* generatedMAC;
    int MACTest;
    BOOL foundOK;
    FREEOTFEBYTE* paddedCheckMAC;
    FREEOTFEBYTE* volumeDetailsBlock;
    int MACOutSize;  
    FREEOTFEBYTE CDB[(CRITICAL_DATA_LENGTH / 8)];
    unsigned int IVSize;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_ReadVolumeCriticalData_v2_v3\n")));
    
    // Numbers...
    maxCDKSize_bits = 0;
    maxIVSize_bits = 0;
    countDriversCypher = 0;
    criticalDataKeySize = 0;
    IVSize = 0;
    countDriversHash = 0;
    countDriversCypher = 0;

    // Pointers...
    criticalDataKey = NULL;
    IV = NULL;
    driverInfoHash = NULL;
    driverInfoCypher = NULL;
    driverFilenamesHash = NULL;
    driverFilenamesCypher = NULL;
    

    // Structs...
    memset(&mainDriver, 0, sizeof(mainDriver));

    /*
    Load main FreeOTFE lib (needed for KDF, MAC, etc)
    Identify all hash drivers
    Identify all cypher drivers
    Identify all hash algorithms supported by each of the hash drivers
    Identify all cypher algorithms supported by each of the cypher drivers

    Loop through all KDFs... (NOT ATM - we only support v3 CDBs)
      Loop through all hash libs...
        Loop through all hashes supported by current hash lib...
          Loop through all cypher libs...
            Loop through all cyphers supported by current cypher lib...
              Loop through all MACs... (NOT ATM - we only support v3 CDBs)
                If the combination can successfully decode the CDB, return
                the matching combination

    */

    foundOK = FALSE;
    allOK = TRUE;


    if (!(_driver_ReadWriteRawCDB(
                        Filename,
                        OffsetWithinFile,
                        TRUE,
                        &CDB
                       )))
        {
        DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Unable to read raw CDB\n")));
        allOK = FALSE;
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Raw CDB read in OK.\n")));
        }


    if (allOK)
        {
        // Load main FreeOTFE lib (needed for KDF, MAC, etc)
        allOK = driver_LoadMainDLL(&mainDriver);
        }

    if (allOK)
        {
        allOK = driver_GetAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher,

                                        TRUE
                                        );
        }

    if (allOK)
        {
        // !!! OPTIMISATION !!!
        // Determine the longest cypher key and IV size
        maxIVSize_bits = 0;
        maxCDKSize_bits = 0;
        cypherDriverIdx = 0;
        while (driverFilenamesCypher[cypherDriverIdx] != NULL)
            {
            currCypherDriverFilename = driverFilenamesCypher[cypherDriverIdx];
            currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];

            // Precache cypher drivers
            // This improves processing speed as hash drivers are already 
            // in memory when an attempt is made to load them
            driver_CypherLoadDLLCached(currCypherDriverFilename, NULL);

            // If we got the DLL filename, but no cypher details, skip this DLL file
            if (currDriverInfoCypher == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Optimisation skipping cypher DLL filename due to lack of driver details\n")));
                cypherDriverIdx++;
                continue;
                }

            for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)
                {
                currCypherImpl = &(currDriverInfoCypher->CypherDetails[cypherImplIdx]);

                // Determine size of "critical data key"...
                cdkSize_bits = currCypherImpl->KeySizeRequired;  // In *bits*
                if (cdkSize_bits < 0)
                    {
                    cdkSize_bits = 512;
                    }

                maxIVSize_bits = max(maxIVSize_bits, currCypherImpl->BlockSize);
                maxCDKSize_bits = max(maxCDKSize_bits, cdkSize_bits);
                }

            cypherDriverIdx++;
            }

        IV = NULL;
        criticalDataKeySize = (maxCDKSize_bits / 8);
        criticalDataKey = malloc(criticalDataKeySize);
        if (criticalDataKey == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for criticalDataKey\n")));        
            allOK = FALSE;
            }
        else
            {   
            IV = malloc((maxIVSize_bits / 8));
            if (IV == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for IV\n")));        
                allOK = FALSE;
                }
            }

        salt = CDB;
        encryptedBlock = salt + (SaltLength / 8);

        if (allOK)
            {
            // v3 CDBs use PBKDF2 as the KDF, and HMAC as the MAC
            currKDFAlg = KDF_PBKDF2;
            currMACAlg = MAC_HMAC;

            // Loop through all hash libs...
            hashDriverIdx = 0;
            while (driverFilenamesHash[hashDriverIdx] != NULL)
                {
                currHashDriverFilename = driverFilenamesHash[hashDriverIdx];
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Testing hash driver (%d): %ls\n"), (hashDriverIdx+1), currHashDriverFilename));
                currDriverInfoHash = driverInfoHash[hashDriverIdx];
                if (currDriverInfoHash == NULL)
                    {
                    DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping; no supported algorithm details obtained.\n")));
                    // Skip out...
                    hashDriverIdx++;
                    continue;
                    }

                // Precache hash drivers
                // This improves processing speed as hash drivers are already 
                // in memory when an attempt is made to load them
                driver_HashLoadDLLCached(currHashDriverFilename, NULL);

                // Loop through all hashes supported by current hash lib...
                for (hashImplIdx = 0; hashImplIdx < currDriverInfoHash->HashCount; hashImplIdx++)
                    {
                    currHashImpl = &(currDriverInfoHash->HashDetails[hashImplIdx]);
                    GUIDToWCHAR(&(currHashImpl->HashGUID), &tmpGUIDStr);
                    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Hash implementation: %ls\n"), tmpGUIDStr));
                    SecZeroAndFreeWCHARMemory(tmpGUIDStr);

                    // v2 and later volumes won't work with hash algorithms with <= 0 length, or
                    // blocksize, as HMAC/PBKDF2 require these to be fixed
                    //  - skip these, and move onto the next hash algorithm (if any)
                    if (
                        (currHashImpl->Length <= 0) ||
                        (currHashImpl->BlockSize <= 0)
                       ) 
                        {
                        // Skip - can't use
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Skipping hash; either length or blocksize <= 0.\n")));
                        continue;
                        }

                    // !!! OPTIMISATION !!!
                    // Because we only allow PBKDF2, which:
                    //   a) Doesn't need a cypher
                    //   b) Short keys are identical to truncated longer keys
                    // We derive the key *here*, outside the cypher loop
                    if (currKDFAlg != KDF_PBKDF2) 
                        {
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("PBKDF2 assumed - optimisation failed\n")));
                        //MsgError('PBKDF2 assumed - optimisation failed');
                        allOK = FALSE;
                        break;
                        }

                    if (!(driver_DeriveKey(
                                    &mainDriver,

                                    currKDFAlg,
                                    currHashDriverFilename,
                                    currHashImpl->HashGUID,
                                    TEXT(""),
                                    FREEOTFE_NULL_GUID,
                                    UserPassword,
                                    strlen(UserPassword),
                                    CDB, // Salt at start of CDB
                                    SaltLength, // In bits
                                    KeyIterations,
                                    maxCDKSize_bits,  // In bits
                                    criticalDataKey  // Derived key
                                   )))
                        {
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Skipping hash; unable to derive key.\n")));
                        continue;
                        }


                    // Loop through all cypher libs...
                    cypherDriverIdx = 0;
                    while (driverFilenamesCypher[cypherDriverIdx] != NULL)
                        {
                        currCypherDriverFilename = driverFilenamesCypher[cypherDriverIdx];
                        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Testing cypher driver (%d): %ls\n"), (cypherDriverIdx+1), currCypherDriverFilename));
                        currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];
                        if (currDriverInfoCypher == NULL)
                            {
                            // Skip out...
                            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping; no supported algorithm details obtained.\n")));
                            cypherDriverIdx++;
                            continue;
                            }
                        
                        // Loop through all cyphers supported by current cypher lib...
                        for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)
                            {
                            currCypherImpl = &(currDriverInfoCypher->CypherDetails[cypherImplIdx]);
                            GUIDToWCHAR(&(currCypherImpl->CypherGUID), &tmpGUIDStr);
                            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Cypher implementation: %ls\n"), tmpGUIDStr));
                            SecZeroAndFreeWCHARMemory(tmpGUIDStr);

                            // Determine size of "critical data key"...
                            cdkSize_bits = currCypherImpl->KeySizeRequired;  // In *bits*
                            if (cdkSize_bits < 0) 
                                {
                                cdkSize_bits = 512;
                                }


                            // Calculate "leb"
                            if (currCypherImpl->BlockSize > 8) 
                                {
                                leb_bits = ((CRITICAL_DATA_LENGTH - SaltLength) / currCypherImpl->BlockSize) * currCypherImpl->BlockSize;
                                }
                            else
                                {
                                leb_bits = (CRITICAL_DATA_LENGTH - SaltLength);
                                }

                            // Zero the IV for decryption, in case previous was "dirty"
                            IVSize = 0;
                            if (currCypherImpl->BlockSize > 0)
                                {
                                IVSize = currCypherImpl->BlockSize;
                                memset(IV, 0, (IVSize / 8));
                                }

                            if (!(driver_CypherDecryptSectorData(
                                         currCypherDriverFilename,
                                         currCypherImpl->CypherGUID,
                                         FREEOTFE_v1_DUMMY_SECTOR_ID,
                                         FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                         cdkSize_bits,
                                         criticalDataKey,
                                         IVSize,
                                         IV,
                                         (leb_bits / 8),
                                         encryptedBlock,
                                         plaintextEncryptedBlock
                                        )))
                                {
                                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to decrypt.\n")));
                                // display MSG
                                allOK = FALSE;
                                }
                            else
                                {
                                paddedCheckMAC = plaintextEncryptedBlock;
                                volumeDetailsBlock = (paddedCheckMAC + (CDB_MAX_MAC_LENGTH / 8));

                                MACOutSize = -1;
                                if (!(driver_MACData(
                                                  &mainDriver,

                                                  currMACAlg,
                                                  currHashDriverFilename,
                                                  currHashImpl->HashGUID,
                                                  currCypherDriverFilename,
                                                  currCypherImpl->CypherGUID,
                                                  cdkSize_bits,
                                                  criticalDataKey,
                                                  (leb_bits - MML),
                                                  volumeDetailsBlock,
                                                  &generatedMAC,
                                                  &MACOutSize
                                                 )))
                                    {
                                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to MAC data.\n")));
                                    // dispay msg
                                    allOK = FALSE;
                                    }
                                else
                                    {
                                    // Check if we've found a valid hash/cypher combination
                                    MACTest = memcmp(
                                                     generatedMAC, 
                                                     paddedCheckMAC, 
                                                     (min(
                                                          MACOutSize, 
                                                          CDB_MAX_MAC_LENGTH
                                                         ) / 8)
                                                    );
                                    SecZeroAndFreeMemory(generatedMAC, (MACOutSize / 8));
                                    if (MACTest == 0)
                                        {
                                        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("SUCCESS! Found valid hash/cypher combination!\n")));

										CDBMetaData->KDFAlgorithm = currKDFAlg;
										CDBMetaData->MACAlgorithm = currMACAlg;

                                        wcscpy(
                                               CDBMetaData->HashDeviceName,
                                               currHashDriverFilename
                                              );
                                        CDBMetaData->HashGUID = currHashImpl->HashGUID;

                                        wcscpy(
                                               CDBMetaData->CypherDeviceName,
                                               currCypherDriverFilename
                                              );
                                        CDBMetaData->CypherGUID = currCypherImpl->CypherGUID;

                                        if (!(_driver_ParseVolumeDetailsBlock(
                                                                      ((leb_bits - MML) / 8),  // In bytes
                                                                      volumeDetailsBlock, 
                                                                      CDBDetails
                                                                     )))
                                            {
                                            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("ERROR: FAILED to parse plaintext volume details block to structure\n")));
                                            // At this point, there has been some failure (e.g.
                                            // critical data block version ID or block structure
                                            // incorrect)
                                            // Note that we *don't* set allOK to FALSE; this failure
                                            // is valid and only indicates that the current
                                            // cypher/hash combination are incorrect for the volume;
                                            // not that something has gone wrong with the mount
                                            // operation
                                            }
                                        else
                                            {
                                            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Parsed OK...\n")));
                                            // Sanity check...
                                            if (
                                                (currCypherImpl->KeySizeRequired >= 0) &&
                                                (CDBDetails->MasterKeyLength != currCypherImpl->KeySizeRequired)
                                               ) 
                                                {
                                                DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("ERROR: Sanity check failed(!)\n")));
                                                // Skip to next loop iteration...
                                                continue;
                                                }
                                            else
                                                {
                                                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Got valid decryption cypher/hash combination\n")));

                                                foundOK = TRUE;

												// Store information for dump
												// purposes
												_driver_StoreCriticalDump(
														  cdkSize_bits,
														  criticalDataKey,
														  min(
														      MACOutSize, 
															  CDB_MAX_MAC_LENGTH
															 ),
														  paddedCheckMAC,
														  leb_bits,
														  plaintextEncryptedBlock,
														  (leb_bits - MML),
														  volumeDetailsBlock
														 );

                                                break; // xxx - it would be nicer to note the 
                                                       // valid combination and continue - let 
                                                       // the user decide which valid 
                                                       // combination to use
                                                }
                                            }

                                        }
                                    }
                                }
                            }  // for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)

                        if (foundOK) 
                            {
                            break;
                            }
                        cypherDriverIdx++;
                        }  // while (driverFilenamesCypher[cypherDriverIdx] != NULL)
                    if (foundOK) 
                        {
                        break;
                        }
                    }  // for (hashImplIdx = 0; hashImplIdx < currDriverInfoHash->HashCount; hashImplIdx++)
                if (foundOK) 
                    {
                    break;
                    }
                hashDriverIdx++;
                }  // while (driverFilenamesHash[hashDriverIdx] != NULL)
            }
        }


    // Cleanup...
    SecZeroMemory(plaintextEncryptedBlock, sizeof(plaintextEncryptedBlock));
    SecZeroAndFreeMemory(criticalDataKey, criticalDataKeySize);
    SecZeroAndFreeMemory(IV, (maxIVSize_bits / 8));

    driver_FreeAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher
                                        );

    driver_UnloadMainDLL(&mainDriver);
    driver_HashUnloadDLLsCached();
    driver_CypherUnloadDLLsCached();

    SecZeroMemory(CDB, sizeof(CDB));

    if (foundOK)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB decoded OK\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB decode FAILED.\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_ReadVolumeCriticalData_v2_v3\n")));
    return foundOK;
}


// =========================================================================
// Read and decrypt CDB
BOOL _driver_ReadVolumeCriticalData(
    WCHAR* Filename,
    LARGE_INTEGER OffsetWithinFile,
    char* UserPassword,
    unsigned int SaltLength,  // In *bits*
    unsigned int KeyIterations,

    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
)
{
    return _driver_ReadVolumeCriticalData_v2_v3(
                                             Filename,
                                             OffsetWithinFile,
                                             UserPassword,
                                             SaltLength,  
                                             KeyIterations,

                                             CDBDetails,
                                             CDBMetaData
                                            );
}


// =========================================================================
void _driver_SecZeroCDBData(
    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_SecZeroCDB\n")));

    if (CDBDetails != NULL)
        {
        if (CDBDetails->MasterKey != NULL)
            {
            SecZeroAndFreeMemory(
                                 CDBDetails->MasterKey,
                                 (CDBDetails->MasterKeyLength / 8)
                                );
            }

        if (CDBDetails->VolumeIV != NULL)
            {
            SecZeroAndFreeMemory(
                                 CDBDetails->VolumeIV,
                                 (CDBDetails->VolumeIVLength / 8)
                                );
            }

        SecZeroMemory(CDBDetails, sizeof(CDBDetails));
        }

    if (CDBMetaData != NULL)
        {
        SecZeroMemory(CDBMetaData, sizeof(CDBMetaData));
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_SecZeroCDB\n")));
}


// =========================================================================
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
)
{
    MOUNT_RESULT retval = MR_UNKNOWN;
    CDB_DETAILS CDBDetails;
    CDB_METADATA CDBMetadata;
    DIOC_MOUNT* DIOCMount;
    DWORD DIOCMountSize;
    WCHAR* CDBFile;
    LARGE_INTEGER CDBFileOffset;
    DWORD metadataLength; // In bytes
    LARGE_INTEGER useOffset;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_Mount\n")));

    metadataLength = 0; // Metadata not used anywhere yet.

    CDBFile = Filename;
    CDBFileOffset.QuadPart = OffsetWithinFile.QuadPart;
    if (Keyfile != NULL)
        {
        if (wcslen(Keyfile) > 0)
            {
            CDBFile = Keyfile;
            CDBFileOffset.QuadPart = 0;
            }
        }

    // Zero structs to initialize...
    memset(&CDBDetails, 0, sizeof(CDBDetails));
    memset(&CDBMetadata, 0, sizeof(CDBMetadata));

    if (!(_driver_ReadVolumeCriticalData(
                                         CDBFile,
                                         CDBFileOffset,
                                         UserPassword,
                                         SaltLength,  // In *bits*
                                         KeyIterations,

                                         &CDBDetails,
                                         &CDBMetadata
                                        )))
        {
        retval = MR_CDB_DECRYPT_FAILED;
        }
    else
        {
        useOffset.QuadPart = OffsetWithinFile.QuadPart;
        if (CDBAtOffset)
            {
            useOffset.QuadPart += (CRITICAL_DATA_LENGTH / 8);
            }
        
        DIOCMount =  _driver_AllocateDIOCMount(
                                        TRUE,

                                        Mountpoint,
                                        Filename,
                                        ReadOnly,

                                        useOffset,
                                        CDBDetails.PartitionSize,

                                        CDBDetails.MasterKey,
                                        CDBDetails.MasterKeyLength,
                                        CDBDetails.VolumeIV,
                                        CDBDetails.VolumeIVLength,
                                        CDBDetails.SectorIVGenMethod,
                                        CDBMetadata.HashDeviceName,
                                        CDBMetadata.HashGUID,
                                        CDBMetadata.CypherDeviceName,
                                        CDBMetadata.CypherGUID,
                                        CDBMetadata.CypherDeviceName,
                                        CDBMetadata.CypherGUID,

                                        RevertVolTimestamps,
                                        CDBDetails.VolumeFlags,

                                        (unsigned char*)NULL,
                                        0,

                                        &DIOCMountSize
                                       );
        if (DIOCMount == NULL)
            {
            retval = MR_OUT_OF_MEMORY;
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCMount struct (size: %d bytes)\n"), DIOCMountSize));
            }
        else
            {
            retval = _driver_MountDIOC(DIOCMount);

            _driver_FreeDIOCMount(DIOCMount, DIOCMountSize);
            }

        }


    // Clear sensitive data block...
    _driver_SecZeroCDBData(&CDBDetails, &CDBMetadata);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_Mount\n")));
    return retval;
}


// =========================================================================
MOUNT_RESULT driver_MountLUKS(
    WCHAR* Mountpoint,
    WCHAR* Filename,
    unsigned char* UserPassword,
    BOOL ReadOnly,
    BOOL BaseIVCypherOnHashLength,
    LARGE_INTEGER SizeLimit,
    BOOL RevertVolTimestamps
)
{
    MOUNT_RESULT retval = MR_UNKNOWN;
    BOOL allOK;
    MODULE_DETAILS_MAIN mainDriver;
    BYTE* keyMaterial;
    LUKS_HEADER_EXT LUKSHeader;
    int keySlot;
    DIOC_MOUNT* DIOCMount;
    DWORD DIOCMountSize;
    DWORD metadataLength; // In bytes
    LARGE_INTEGER useOffset;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_MountLUKS\n")));

    metadataLength = 0; // Metadata not used anywhere yet.
    allOK = TRUE;

    keyMaterial = NULL;

    if (allOK)
        {
        // Load main FreeOTFE lib
        allOK = driver_LoadMainDLL(&mainDriver);
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to load main driver\n")));            
            }
        }

    // Sanity check it's a LUKS volume
    if (allOK)
        {
        allOK = LUKS_IsLUKSVolume(Filename);
        if (!(allOK))
            {
            retval = MR_NOT_A_LUKS_VOLUME;
            }
        }

    if (allOK)
        {
        if (!(LUKS_RecoverMasterKey(
                                    &mainDriver,

                                    Filename,
                                    UserPassword,
                                    BaseIVCypherOnHashLength,

                                    &LUKSHeader,
                                    &keySlot,
                                    &keyMaterial 
                                   )))
            {
            retval = MR_CDB_DECRYPT_FAILED;
            allOK = FALSE;
            }
        }


    if (allOK)
        {
        useOffset.QuadPart = (LUKSHeader.rawHeader.payload_offset * LUKS_SECTOR_SIZE);
        DIOCMount = _driver_AllocateDIOCMount(
            TRUE,

            Mountpoint,
            Filename,
            ReadOnly,

            useOffset,
            SizeLimit,

            keyMaterial,
            (LUKSHeader.rawHeader.key_bytes * 8),  // In *bits*
            NULL,
            0,  // In *bits*
            LUKSHeader.sectorIVGenMethod,
            LUKSHeader.IVHashKernelModeDeviceName,
            LUKSHeader.IVHashGUID,
            LUKSHeader.IVCypherKernelModeDeviceName,
            LUKSHeader.IVCypherGUID,
            LUKSHeader.cypherKernelModeDeviceName,
            LUKSHeader.cypherGUID,

            RevertVolTimestamps, 
            0,  // Volume flags

            NULL,
            0,

            &DIOCMountSize
            );
        if (DIOCMount == NULL)
            {
            retval = MR_OUT_OF_MEMORY;
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCMount struct (size: %d bytes)\n"), DIOCMountSize));
            }
        else
            {
            retval = _driver_MountDIOC(DIOCMount);
        
            SecZeroAndFreeMemory(DIOCMount, DIOCMountSize);
            }
        }

    SecZeroAndFreeMemory(keyMaterial, LUKSHeader.rawHeader.key_bytes);
    SecZeroMemory(&LUKSHeader, sizeof(LUKSHeader));
    driver_UnloadMainDLL(&mainDriver);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_MountLUKS\n")));
    return retval;
}


// =========================================================================
void
_driver_DeleteFile(WCHAR* Filename)
{
    if (Filename != NULL)
        {
        DeleteFile(Filename);
        }
}


// =========================================================================
// Create file
BOOL
_driver_CreateFile(
    WCHAR* Filename,
    LARGE_INTEGER Size
)
{
    BOOL retval = FALSE;
    HANDLE fileHandle;
    DWORD desiredAccess;
    DWORD fileFlags;
    DWORD newFilePos;
    BOOL filePosOK;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_CreateFile\n")));


    desiredAccess = (GENERIC_READ | GENERIC_WRITE);
    // Note: FILE_ATTRIBUTE_NORMAL only valid if used *alone*
    //fileFlags = FILE_ATTRIBUTE_NORMAL;
    fileFlags = FILE_FLAG_WRITE_THROUGH;
    fileHandle = CreateFile(
                            Filename,
                            desiredAccess,  
                            (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                            NULL,
                            CREATE_NEW, 
                            fileFlags, 
                            NULL
                           ); 
    if (fileHandle == INVALID_HANDLE_VALUE) 
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to create volume file\n")));
        retval = FALSE;
        } 
    else
        {
        newFilePos = SetFilePointer( 
                                    fileHandle, 
                                    Size.LowPart, 
                                    &(Size.HighPart), 
                                    FILE_BEGIN
                                   );
        filePosOK = TRUE;
        if (newFilePos == FAILED_SETFILEPOINTER) 
            {
            filePosOK = (GetLastError() == NO_ERROR);
            }
        if (filePosOK) 
            {
            if (SetEndOfFile(fileHandle))
                {
                retval = TRUE;
                }
            }
        CloseHandle(fileHandle);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_CreateFile\n")));
    return retval;
}


// =========================================================================
// Encrypt and write v3 format CDB
BOOL
_driver_WriteVolumeCriticalData_v3(
    WCHAR* CDBFilename, 
    LARGE_INTEGER CDBOffset, 
    char* UserPassword,
    int SaltLength,  // In *bits*
    int KeyIterations,
    FREEOTFEBYTE* RandomPool,
    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
)
{
    BOOL allOK = TRUE;
    HASH hashInfo;
    CYPHER_v3 cypherInfo;
    FREEOTFEBYTE* ptr = NULL;
    FREEOTFEBYTE* ptrSalt = NULL;
    FREEOTFEBYTE* randomPoolPos = NULL;
    FREEOTFEBYTE CDB[CRITICAL_DATA_LENGTH / 8];
    FREEOTFEBYTE VolumeDetailsBlock[CRITICAL_DATA_LENGTH / 8];  // Must be big enough
    FREEOTFEBYTE EncryptedBlock_Plaintext[CRITICAL_DATA_LENGTH / 8];  // Must be big enough
    FREEOTFEBYTE EncryptedBlock_Cyphertext[CRITICAL_DATA_LENGTH / 8];  // Must be big enough
    int leb_bits;  // In *bits*
    int paddingOneLen;  // In *bytes*
    int paddingTwoLen;  // In *bytes*
    int paddingThreeLen;  // In *bytes*
    char tmpChar;
    MODULE_DETAILS_MAIN mainDriver;
    int cdkSize_bits;
    FREEOTFEBYTE* criticalDataKey = NULL;
    FREEOTFEBYTE* generatedMAC = NULL;
    int usedMAC_bytes;
    int MACOutSize;  
    FREEOTFEBYTE* IV = NULL;
    

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_WriteVolumeCriticalData_v3\n")));
    
    cdkSize_bits = 0;

    // Structs...
    memset(&mainDriver, 0, sizeof(mainDriver));
    memset(CDB, 0, sizeof(CDB));

    randomPoolPos = RandomPool;

    if (allOK)
        {
        // Load main FreeOTFE lib (needed for KDF, MAC, etc)
        allOK = driver_LoadMainDLL(&mainDriver);
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to load main driver\n")));            
            }
        }

    if (allOK)
        {
        allOK = driver_HashGetImplDetails(
                                          CDBMetaData->HashDeviceName,
                                          &(CDBMetaData->HashGUID),
                                          &hashInfo
                                         );
        }

    if (allOK)
        {
        allOK = driver_CypherGetImplDetails(
                                          CDBMetaData->CypherDeviceName,
                                          &(CDBMetaData->CypherGUID),
                                          &cypherInfo
                                         );
        }

    if (allOK)
        {
        // Calculate "leb"
        if (cypherInfo.BlockSize > 8) 
            {
            leb_bits = ((CRITICAL_DATA_LENGTH - SaltLength) / cypherInfo.BlockSize) * cypherInfo.BlockSize;
            }
        else
            {
            leb_bits = (CRITICAL_DATA_LENGTH - SaltLength);
            }
        }


    // Generate the Volume Details Block...
    if (allOK)
        {
        ptr = VolumeDetailsBlock;

        // 8 bits: CDB Format ID...
        tmpChar = CDB_FORMAT_ID;
        ptr = SDUPack_char(ptr, tmpChar);

        // 32 bits: Volume flags...
        ptr = SDUPack_DWORD(ptr, CDBDetails->VolumeFlags);

        // 64 bits: Partition length...
        ptr = SDUPack_LARGE_INTEGER(ptr, CDBDetails->PartitionSize);

        // 32 bits: Master key length...
        ptr = SDUPack_DWORD(ptr, CDBDetails->MasterKeyLength);

        // Variable bits: Master key...
        memcpy(
               ptr,
               CDBDetails->MasterKey, 
               (CDBDetails->MasterKeyLength / 8)
              );
        ptr += (CDBDetails->MasterKeyLength / 8);

        // 8 bits: Requested drive letter...
        // Ignore under WinCE...
        tmpChar = CDBDetails->DefaultDrive;
        ptr = SDUPack_char(ptr, tmpChar);

        // 32 bits: Volume IV length...
        ptr = SDUPack_DWORD(ptr, CDBDetails->VolumeIVLength);

        // Variable bits: Volume IV...
        memcpy(
               ptr,
               CDBDetails->VolumeIV, 
               (CDBDetails->VolumeIVLength / 8)
              );
        ptr += (CDBDetails->VolumeIVLength / 8);

        // 8 bits: Sector IV generation method...
        tmpChar = (int)CDBDetails->SectorIVGenMethod;
        ptr = SDUPack_char(ptr, tmpChar);

        // Random padding #2...
        paddingTwoLen = (((leb_bits - MML) / 8) - (ptr - VolumeDetailsBlock));
        memcpy(ptr, randomPoolPos, paddingTwoLen);
        randomPoolPos += paddingTwoLen;
        }


    // Malloc storage for critical data key and IV
    if (allOK)
        {
        // Determine size of "critical data key"...
        cdkSize_bits = cypherInfo.KeySizeRequired;  // In *bits*
        if (cdkSize_bits < 0)
            {
            cdkSize_bits = 512;
            }

        IV = NULL;
        criticalDataKey = malloc((cdkSize_bits / 8));
        if (criticalDataKey == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for criticalDataKey\n")));        
            allOK = FALSE;
            }
        else
            {   
            IV = malloc((cypherInfo.BlockSize / 8));
            if (IV == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for IV\n")));        
                allOK = FALSE;
                }
            }
        }

    if (allOK)
        {
        // Encrypt the Volume Details Block...
        ptrSalt = randomPoolPos;
        randomPoolPos += (SaltLength / 8);

        allOK = driver_DeriveKey(
                        &mainDriver,

                        KDF_PBKDF2,
                        CDBMetaData->HashDeviceName,
                        CDBMetaData->HashGUID,
                        TEXT(""),
                        FREEOTFE_NULL_GUID,
                        UserPassword,
                        strlen(UserPassword),
                        ptrSalt,
                        SaltLength, // In bits
                        KeyIterations,
                        cdkSize_bits,  // In bits
                        criticalDataKey  // Derived key
                       );
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to derive critical data key\n")));
            }
        }


    // Generate check MAC and build up the plaintext encrypted block
    if (allOK)
        {
        MACOutSize = -1;
        allOK = driver_MACData(
                               &mainDriver,
                               MAC_HMAC,  // v3 CDB always HMAC
                               CDBMetaData->HashDeviceName,
                               CDBMetaData->HashGUID,
                               CDBMetaData->CypherDeviceName,
                               CDBMetaData->CypherGUID,
                               cdkSize_bits,
                               criticalDataKey,
                               (leb_bits - MML),
                               VolumeDetailsBlock,
                               &generatedMAC,
                               &MACOutSize
                              );
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to create check MAC\n")));
            }
        else
            {
            ptr = EncryptedBlock_Plaintext;

            // Check MAC...
            usedMAC_bytes = (min(MML, MACOutSize) / 8);
            memcpy(
                   ptr,
                   generatedMAC,
                   usedMAC_bytes                   
                  );
            ptr += usedMAC_bytes;

            // Random padding #3...
            paddingThreeLen = ((MML / 8) - usedMAC_bytes);
            memcpy(
                   ptr,
                   randomPoolPos,
                   paddingThreeLen                   
                  );
            randomPoolPos += paddingThreeLen;
            ptr += paddingThreeLen;

            // Volume details block...
            memcpy(
                   ptr,
                   VolumeDetailsBlock,
                   ((leb_bits - MML) / 8)
                  );

            SecZeroAndFreeMemory(generatedMAC, (MACOutSize / 8));
            }
        }

                                    
    if (allOK)
        {
        // Zero the IV for encryption
        if (cypherInfo.BlockSize > 0)
            {
            memset(IV, 0, (cypherInfo.BlockSize / 8));
            }
        allOK = driver_CypherEncryptSectorData(
                     CDBMetaData->CypherDeviceName,
                     CDBMetaData->CypherGUID,
                     FREEOTFE_v1_DUMMY_SECTOR_ID,
                     FREEOTFE_v1_DUMMY_SECTOR_SIZE,

                     cypherInfo.KeySizeRequired,
                     criticalDataKey,
                     cypherInfo.BlockSize,
                     IV,
                     (leb_bits / 8),
                     EncryptedBlock_Plaintext,
                     EncryptedBlock_Cyphertext
                    );
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to encrypt data.\n")));
            }
        }


    if (allOK)
        {
        // Populate the full CDB...
        ptr = CDB;
        // Salt...
        memcpy(ptr, ptrSalt, (SaltLength / 8));
        ptr += (SaltLength / 8);
        // Volume details block...
        memcpy(ptr, EncryptedBlock_Cyphertext, (leb_bits / 8));
        ptr += (leb_bits / 8);
        // Random padding #1...
        paddingOneLen = (((CRITICAL_DATA_LENGTH - SaltLength) - leb_bits) / 8);
        memcpy(ptr, randomPoolPos, paddingOneLen);
        randomPoolPos += paddingOneLen;
        }


    // Write the encrypted CDB to the file...
    if (allOK)
        {
        allOK = _driver_ReadWriteRawCDB(
                                     CDBFilename,
                                     CDBOffset,
                                     FALSE,
                                     &CDB
                                    );
        }

    driver_UnloadMainDLL(&mainDriver);


    SecZeroMemory(&hashInfo, sizeof(hashInfo));
    SecZeroMemory(&cypherInfo, sizeof(cypherInfo));
    SecZeroMemory(CDB, sizeof(CDB));
    SecZeroMemory(VolumeDetailsBlock, sizeof(VolumeDetailsBlock));
    SecZeroMemory(EncryptedBlock_Plaintext, sizeof(EncryptedBlock_Plaintext));
    SecZeroMemory(EncryptedBlock_Cyphertext, sizeof(EncryptedBlock_Cyphertext));
    SecZeroMemory(&leb_bits, sizeof(leb_bits));
    SecZeroMemory(&paddingOneLen, sizeof(paddingOneLen));
    SecZeroMemory(&paddingTwoLen, sizeof(paddingTwoLen));
    SecZeroMemory(&paddingThreeLen, sizeof(paddingThreeLen));
    SecZeroMemory(&tmpChar, sizeof(tmpChar));
    SecZeroMemory(&mainDriver, sizeof(mainDriver));
    SecZeroMemory(criticalDataKey, (cdkSize_bits / 8));
    SecZeroMemory(&cdkSize_bits, sizeof(cdkSize_bits));
    SecZeroMemory(&usedMAC_bytes, sizeof(usedMAC_bytes));
    SecZeroMemory(&MACOutSize, sizeof(MACOutSize));
    SecZeroMemory(&IV, sizeof(IV));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_WriteVolumeCriticalData_v3\n")));
    return allOK;
}


// =========================================================================
// Encrypt and write CDB using latest CDB format
BOOL
_driver_WriteVolumeCriticalData(
    WCHAR* CDBFilename, 
    LARGE_INTEGER CDBOffset, 
    char* UserPassword,
    int SaltLength,  // In *bits*
    int KeyIterations,
    FREEOTFEBYTE* RandomPool,
    CDB_DETAILS* CDBDetails,
    CDB_METADATA* CDBMetaData
)
{
    return _driver_WriteVolumeCriticalData_v3(
                                              CDBFilename, 
                                              CDBOffset, 
                                              UserPassword,
                                              SaltLength,  // In *bits*
                                              KeyIterations,
                                              RandomPool,
                                              CDBDetails,
                                              CDBMetaData
                                             );

}


// ========================================================================
// KeyFilename - set to NULL for no keyfile (store CDB in volume file)
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
)
{
    BOOL retval = TRUE;
    WCHAR* CDBFilename;
    LARGE_INTEGER CDBOffset;
    LARGE_INTEGER volFileSize;
    LARGE_INTEGER tmpLARGE_INTEGER;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CreateVolumeFile\n")));

    if (retval)
        {
        if (!(SDUCheckFileExists(Filename)))
            {
            volFileSize = CDBDetails->PartitionSize;
            if (KeyFilename == NULL)
                {
                volFileSize.QuadPart += (CRITICAL_DATA_LENGTH / 8);
                }

            // Create volume file...
            retval = _driver_CreateFile(Filename, volFileSize);
            }
        }

    if (retval)
        {
        if (KeyFilename != NULL)
            {
            // Create keyfile...
            tmpLARGE_INTEGER.QuadPart = (CRITICAL_DATA_LENGTH / 8);
            retval = _driver_CreateFile(KeyFilename, tmpLARGE_INTEGER);
            }
        }


    if (retval)
        {
        // Identify where to write the CDB to
        CDBFilename = KeyFilename;
        CDBOffset.QuadPart = 0;
        if (CDBFilename == NULL)
            {
            CDBFilename = Filename;
            CDBOffset = Offset;
            }

        // Write CDB...
        retval = _driver_WriteVolumeCriticalData(
                                  CDBFilename, 
                                  CDBOffset, 
                                  UserPassword,
                                  SaltLength,  // In *bits*
                                  KeyIterations,
                                  RandomPool,
                                  CDBDetails,
                                  CDBMetaData
                                 );
        }

    if (!(retval))
        {
        // Create failed; delete any files created
        _driver_DeleteFile(Filename);
        _driver_DeleteFile(KeyFilename);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CreateVolumeFile\n")));
    return retval;
}


// =========================================================================
// Update a CDB, writing it to a different file, if needed
// Note: BOTH source AND destination files *must* exist before this
//       function is called
BOOL
_driver_UpdateCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    char* SrcUserPassword,
    int SrcSaltLength,  // In *bits*
    int SrcKeyIterations,

    WCHAR* DestFilename,
    LARGE_INTEGER DestOffset,
    char* DestUserPassword,
    int DesSaltLength,  // In *bits*
    int DestKeyIterations,

    FREEOTFEBYTE* RandomPool
)
{
    BOOL retval = TRUE;
    CDB_DETAILS CDBDetails;
    CDB_METADATA CDBMetaData;
    
    // Sanity; source file must exist
    if (retval)
        {
        retval = (
                  SDUCheckFileExists(SrcFilename) &&
                  SDUCheckFileExists(DestFilename)
                 );
        }

    if (retval)
        {
        // Read and decrypt existing CDB...
        retval = _driver_ReadVolumeCriticalData(
                                                SrcFilename,
                                                SrcOffset,
                                                SrcUserPassword,
                                                SrcSaltLength,
                                                SrcKeyIterations,

                                                &CDBDetails,
                                                &CDBMetaData
                                               );
        }

    if (retval)
        {
        // Encrypt and write out CDB with new details...
        retval = _driver_WriteVolumeCriticalData(
                                  DestFilename, 
                                  DestOffset, 
                                  DestUserPassword,
                                  DesSaltLength,
                                  DestKeyIterations,

                                  RandomPool,

                                  &CDBDetails,
                                  &CDBMetaData
                                 );
        }

    SecZeroMemory(&CDBDetails, sizeof(CDBDetails));
    SecZeroMemory(&CDBMetaData, sizeof(CDBMetaData));

    return retval;
}


// =========================================================================
void
_driver_DumpBinary(FILE* hFile, int DataLength, void* Data)
{
	unsigned int bufSize;
	char* buf;

	bufSize = SDUPrettyPrintHexBufReq(DataLength, 8);
	buf = malloc(bufSize);
	if (buf == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory to dump binary data\n")));
        }
    else
		{
		if (SDUPrettyPrintHex(
							  Data,
							  0,
						 	  DataLength,
							  buf,
							  bufSize,
							  8
						     ) == 0)
			{
			fprintf(hFile, "%s", buf);
			}

		SecZeroAndFreeMemory(buf, bufSize);
		}

}


// =========================================================================
void
driver_DumpWCHAR(FILE* hFile, WCHAR* Data)
{
	char* buf;
	int bufSize;
    
	if (Data != NULL)
		{
		// +1 for terminating NULL
		bufSize = wcslen(Data) + 1;
		buf = malloc(bufSize);
		if (buf == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory to dump WCHAR data\n")));
            }
        else
			{
			memset(buf, 0, bufSize);
			wcstombs(buf, Data, wcslen(Data));
			fprintf(hFile, "%s", buf);
			SecZeroAndFreeMemory(buf, bufSize);
			}
		}

}


// =========================================================================
// Write a nicely formatted string, decoding one bit of a bitmapped value
// bit - The zero-offset bit (i.e. the LSB is bit 0)
void
_driver_DumpCriticalDataToFileBitmap(
	FILE* hFile,
	DWORD value,
	int bit,
	char* unsetMeaning,
	char* setMeaning
)
{
	DWORD x;

	x = 1 << bit;

	if ((value & x) == x) 
		{
		fprintf(hFile, "Bit %d: 1 (%s)\n", bit, setMeaning);
		}
	else
		{
		fprintf(hFile, "Bit %d: 0 (%s)\n", bit, unsetMeaning);
		}

}


// =========================================================================
void driver_AddStdDumpHeader(
	FILE* hFile,
	char* title
)
{
    OSVERSIONINFO osVer;
    LPWSTR strVersion;
    DWORD driverVersion;

    fprintf(hFile, "%s\n", title);
    driver_AddUnderline(hFile, '=', strlen(title));
    fprintf(hFile, "\n");
    driver_AddStdDumpSection(hFile, "Dump Created By");
    fprintf(hFile, "Platform              : PDA (v");

    if (GetVersionEx(&osVer))
        {
        fprintf(
                hFile, 
                "%d.%d.%d [%s]",
                osVer.dwMajorVersion,
                osVer.dwMinorVersion,
                osVer.dwBuildNumber,
                osVer.szCSDVersion
                );
        }
    else
        {
        fprintf(hFile, "???");
        }
    fprintf(hFile, ")\n");

    fprintf(hFile, "Application version   : ");
    if (SDUGetVersionInfoShortFmtAlloc(
                                        NULL,
                                        &strVersion
                                        ))
        {
        driver_DumpWCHAR(hFile, strVersion);
        free(strVersion);
        }	
    // Commented out; this flag is part of main.h - which we don't want #included
    // in the driver interface!
    //		if (APP_BETA_FLAG)
    //			{
    //			fprintf(hFile, " BETA");
    //			}
    fprintf(hFile, "\n");

    fprintf(hFile, "Driver ID             : ");
    if (driver_VersionID(&driverVersion))
        {
        fprintf(
                hFile, 
                "v%d.%0.2d.%0.4d", 
                (driverVersion & 0xFF000000) / 0x00FF0000,
                (driverVersion & 0x00FF0000) / 0x0000FF00,
                (driverVersion & 0x0000FFFF)
                );
        }
    else
        {
        fprintf(hFile, "Unable to determine driver version");
        }
    fprintf(hFile, "\n");
}


// =========================================================================
void driver_AddStdDumpSection(
    FILE* hFile,
    char* sectionTitle
)
{
    fprintf(hFile, "%s\n", sectionTitle);
    driver_AddUnderline(hFile, '-', strlen(sectionTitle));
}


// =========================================================================
void driver_AddUnderline(
    FILE* hFile,
    char UnderlineChar,
    unsigned int Length
)
{
    unsigned int i;

    for(i=0; i < Length; i++)
        {
        fprintf(hFile, "%c", UnderlineChar);
        }
    fprintf(hFile, "\n");
}


// =========================================================================
// Dump CDB details to human-readable text file.
BOOL
driver_DumpCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    char* SrcUserPassword,
    unsigned int SrcSaltLength,  // In *bits*
    unsigned int SrcKeyIterations,

    WCHAR* DestFilename
)
{
    BOOL retval = TRUE;
    FREEOTFEBYTE CDB[(CRITICAL_DATA_LENGTH / 8)];
    CDB_DETAILS CDBDetails;
    CDB_METADATA CDBMetaData;
    FILE* hFile;
    SYSTEMTIME readTimeDiff;
    SYSTEMTIME readTimeStop;
    SYSTEMTIME readTimeStart;
    FILETIME tmpReadTimeDiff;
    FILETIME tmpReadTimeStop;
    FILETIME tmpReadTimeStart;
    HASH hashInfo;
    CYPHER_v3 cypherInfo;
    WCHAR* tmpStringGIUD;
    WCHAR strPrettyTitleHash[MAX_PRETTYPRINTED_TITLE];
    WCHAR strPrettyTitleCypher[MAX_PRETTYPRINTED_TITLE];
    WCHAR strPrettySectorIVGenMethod[MAX_PRETTYPRINTED_SECTORIVGENMETHOD];
    WCHAR strPrettyMACAlgorithm[MAX_PRETTYPRINTED_MACALGORITHM];
    WCHAR strPrettyKDFAlgorithm[MAX_PRETTYPRINTED_KDFALGORITHM];

    // Sanity; source file must exist
    if (retval)
        {
        retval = (
                  SDUCheckFileExists(SrcFilename) &&
                  (!(SDUCheckFileExists(DestFilename)))
                 );
        }

	if (retval)
		{
		// Get a copy of the raw CDB data...
		if (!(_driver_ReadWriteRawCDB(
							SrcFilename,
							SrcOffset,
							TRUE,
							&CDB
						   )))
			{
			DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Dumper unable to read raw CDB\n")));
			retval = FALSE;
			}
		else
			{
			DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dumper raw CDB read in OK.\n")));
			}
		}

    // Get rid of PC-Lint warning
    memset(&CDBDetails, 0, sizeof(CDBDetails));
    memset(&CDBMetaData, 0, sizeof(CDBMetaData));
    if (retval)
        {
        // Read and decrypt existing CDB...
        _driver_DumpFlag = TRUE;
        GetLocalTime(&readTimeStart);
        retval = _driver_ReadVolumeCriticalData(
                                                SrcFilename,
                                                SrcOffset,
                                                SrcUserPassword,
                                                SrcSaltLength,
                                                SrcKeyIterations,

                                                &CDBDetails,
                                                &CDBMetaData
                                               );
        GetLocalTime(&readTimeStop);
        }

    if (retval)
        {
        // Dump CDB...
		hFile = _wfopen(DestFilename, TEXT("w+"));

        driver_AddStdDumpHeader(hFile, "Critical Data Block Dump");

        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "User Supplied Information");
        fprintf(hFile, "Filename (user mode)  : ");
		driver_DumpWCHAR(hFile, SrcFilename);
        fprintf(hFile, "\n");
        fprintf(hFile, "Filename (kernel mode): <n/a in PDA version>\n");
        fprintf(hFile, "Password              : \n");
        fprintf(hFile, "  Length: %d bits\n", (strlen(SrcUserPassword) * 8));
        fprintf(hFile, "  Data  : \n");
		_driver_DumpBinary(hFile, strlen(SrcUserPassword), SrcUserPassword);
        fprintf(hFile, "Offset                : %d bytes\n", SrcOffset.QuadPart);
        fprintf(hFile, "Salt length           : %d bits\n", SrcSaltLength);
        fprintf(hFile, "Key iterations        : %d\n", SrcKeyIterations);

        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "Plaintext Information");
        fprintf(hFile, "Salt data             :\n");
		_driver_DumpBinary(hFile, (SrcSaltLength / 8), CDB);

        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "Dump Performance");
		SystemTimeToFileTime(&readTimeStart, &tmpReadTimeStart);
		SystemTimeToFileTime(&readTimeStop, &tmpReadTimeStop);
		tmpReadTimeDiff.dwHighDateTime = 0;  // Assume non-absurd amount of time
		tmpReadTimeDiff.dwLowDateTime = (
			                             tmpReadTimeStop.dwLowDateTime -
										 tmpReadTimeStart.dwLowDateTime
										);
		FileTimeToSystemTime(&tmpReadTimeDiff, &readTimeDiff);
        fprintf(
			    hFile,
			    "Time to process CDB   : %d hours, %d mins, %d.%d secs\n",
				readTimeDiff.wHour,
				readTimeDiff.wMinute,
				readTimeDiff.wSecond,
				readTimeDiff.wMilliseconds
			   );
        
        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "Autodetermined Information");

        if (driver_HashGetImplDetails(
                                       CDBMetaData.HashDeviceName,
                                       &(CDBMetaData.HashGUID),
                                       &hashInfo
                                      ))
			{
            memset(strPrettyTitleHash, 0, sizeof(strPrettyTitleHash));
            driver_HashPrettyprintAlgTechTitle(
                                           &hashInfo, 
                                           strPrettyTitleHash,
                                           sizeof(strPrettyTitleHash)
                                          );
			}
		else
			{
			wcscpy(
				   strPrettyTitleHash,
				   TEXT("ERROR: Unable to determine hash title?!")
				  );
			}


        if (driver_CypherGetImplDetails(
                                       CDBMetaData.CypherDeviceName,
                                       &(CDBMetaData.CypherGUID),
                                       &cypherInfo
                                      ))
			{
            memset(strPrettyTitleCypher, 0, sizeof(strPrettyTitleCypher));
            driver_CypherPrettyprintAlgTechTitle(
                                           &cypherInfo, 
                                           strPrettyTitleCypher,
                                           sizeof(strPrettyTitleCypher)
                                          );
			}
		else
			{
			wcscpy(
				   strPrettyTitleCypher,
				   TEXT("ERROR: Unable to determine cypher title?!")
				  );
			}


        fprintf(hFile, "Hash pretty title     : ");
		driver_DumpWCHAR(hFile, strPrettyTitleHash);
        fprintf(hFile, "\n");
        fprintf(hFile, "Hash driver KM name   : ");
		driver_DumpWCHAR(hFile, CDBMetaData.HashDeviceName);
        fprintf(hFile, "\n");
		fprintf(hFile, "Hash GUID             : ");
		GUIDToWCHAR(&CDBMetaData.HashGUID, &tmpStringGIUD);
		driver_DumpWCHAR(hFile, tmpStringGIUD);
		SecZeroAndFreeWCHARMemory(tmpStringGIUD);
        fprintf(hFile, "\n");

        fprintf(hFile, "Cypher pretty title   : ");
		driver_DumpWCHAR(hFile, strPrettyTitleCypher);
        fprintf(hFile, "\n");
        fprintf(hFile, "Cypher driver KM name :");
		driver_DumpWCHAR(hFile, CDBMetaData.CypherDeviceName);
        fprintf(hFile, "\n");
        fprintf(hFile, "Cypher GUID           : ");
		GUIDToWCHAR(&CDBMetaData.CypherGUID, &tmpStringGIUD);
		driver_DumpWCHAR(hFile, tmpStringGIUD);
		SecZeroAndFreeWCHARMemory(tmpStringGIUD);
        fprintf(hFile, "\n");

		fprintf(hFile, "Critical data key     : \n");
        fprintf(hFile, "  KDF   : ");
        driver_PrettyprintKDFAlgorithm(
                            CDBMetaData.KDFAlgorithm,
                            strPrettyKDFAlgorithm,
                            sizeof(strPrettyKDFAlgorithm)
                           );
		driver_DumpWCHAR(hFile, strPrettyKDFAlgorithm);
        fprintf(hFile, "\n");
        fprintf(hFile, "  Length: %d bits\n", _driver_DumpCriticalDataKeyBits);
        fprintf(hFile, "  Key   : \n");
		_driver_DumpBinary(
			               hFile,
						   (_driver_DumpCriticalDataKeyBits / 8),
						   _driver_DumpCriticalDataKey
						  );
        fprintf(hFile, "Plaintext encrypted block: \n");
        fprintf(hFile, "  Length: %d bits\n", _driver_DumpPlaintextEncryptedBlockBits);
        fprintf(hFile, "  Data  : \n");
		_driver_DumpBinary(
			               hFile,
						   (_driver_DumpPlaintextEncryptedBlockBits / 8),
						   _driver_DumpPlaintextEncryptedBlock
						  );
        fprintf(hFile, "Check MAC             : \n");
		fprintf(hFile, "  MAC algorithm: ");
        driver_PrettyprintMACAlgorithm(
                            CDBMetaData.MACAlgorithm,
                            strPrettyMACAlgorithm,
                            sizeof(strPrettyMACAlgorithm)
                           );
		driver_DumpWCHAR(hFile, strPrettyMACAlgorithm);
        fprintf(hFile, "\n");

		fprintf(hFile, "  Length       : %d bits\n", _driver_DumpCheckMACBits);
        fprintf(hFile, "  Data         : \n");
		_driver_DumpBinary(
			               hFile,
						   (_driver_DumpCheckMACBits / 8),
						   _driver_DumpCheckMAC
						  );
        fprintf(hFile, "Volume details block  : \n");
        fprintf(hFile, "  Length       : %d bits\n", _driver_DumpVolumeDetailsBlockBits);
        fprintf(hFile, "  Data         : \n");
		_driver_DumpBinary(
			               hFile,
						   (_driver_DumpVolumeDetailsBlockBits / 8),
						   _driver_DumpVolumeDetailsBlock
						  );

        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "Volume Details Block");
        fprintf(hFile, "CDB format ID         : %d\n", CDBDetails.CDBFormatID);

        // Decode the VolumeFlags to human-readable format
        fprintf(hFile, "Volume flags          : %d\n", CDBDetails.VolumeFlags);
        fprintf(hFile, "                        ");
		_driver_DumpCriticalDataToFileBitmap(
								 hFile,
                                 CDBDetails.VolumeFlags,
                                 VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART,
                                 "Sector ID zero is at the start of the encrypted data",
                                 "Sector ID zero is at the start of the host file/partition"
                                );

		fprintf(hFile, "Partition length      : %d bytes\n", CDBDetails.PartitionSize.QuadPart);

        fprintf(hFile, "Master key            : \n");
        fprintf(hFile, "  Length: %d bits\n", CDBDetails.MasterKeyLength);
        fprintf(hFile, "  Key   :\n");
		_driver_DumpBinary(
			               hFile,
						   (CDBDetails.MasterKeyLength / 8),
						   CDBDetails.MasterKey
						  );
        fprintf(hFile, "Sector IV generation  : ");
        driver_PrettyprintSectorIVGenMethod(
                            CDBDetails.SectorIVGenMethod,
                            strPrettySectorIVGenMethod,
                            sizeof(strPrettySectorIVGenMethod)
                           );
		driver_DumpWCHAR(hFile, strPrettySectorIVGenMethod);
        fprintf(hFile, "\n");

        fprintf(hFile, "Volume IV             : \n");
        fprintf(hFile, "  Length : %d bits\n", CDBDetails.VolumeIVLength);
        fprintf(hFile, "  IV data:\n");
		_driver_DumpBinary(
			               hFile,
						   (CDBDetails.VolumeIVLength / 8),
						   CDBDetails.VolumeIV
						  );

        if (CDBDetails.DefaultDrive == 0)
			{
			fprintf(hFile, "Requested drive letter: None; use default\n");
			}
        else
			{
			fprintf(hFile, "Requested drive letter: %c:\n", ('A' + CDBDetails.DefaultDrive - 1));
			}

        fprintf(hFile, "\n");
        fprintf(hFile, "\n");
        driver_AddStdDumpSection(hFile, "Encrypted CDB");
		_driver_DumpBinary(
			               hFile,
						   (CRITICAL_DATA_LENGTH / 8),
						   CDB
						  );

		fclose(hFile);
        }

	driver_FreeCriticalDump();
	_driver_DumpFlag = FALSE;

	SecZeroMemory(CDB, sizeof(CDB));
	SecZeroMemory(&CDBDetails, sizeof(CDBDetails));
    SecZeroMemory(&CDBMetaData, sizeof(CDBMetaData));

    return retval;
}


// =========================================================================
// Dump LUKS details to human-readable text file.
BOOL
driver_DumpLUKS(
    WCHAR* SrcFilename,
    char* SrcUserPassword,
    BOOL SrcBaseIVCypherOnHashLength,

    WCHAR* DestFilename
)
{
    BOOL allOK;
    MODULE_DETAILS_MAIN mainDriver;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_MountLUKS\n")));

    allOK = TRUE;

    if (allOK)
        {
        // Load main FreeOTFE lib
        allOK = driver_LoadMainDLL(&mainDriver);
        if (!(allOK))
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to load main driver\n")));            
            }
        }

    if (allOK)
        {
        allOK = LUKS_DumpLUKSDataToFile(
                                        &mainDriver,

                                        SrcFilename,
                                        SrcUserPassword,
                                        SrcBaseIVCypherOnHashLength,

                                        DestFilename
                                        );
        }

    driver_UnloadMainDLL(&mainDriver);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_MountLUKS\n")));
    return allOK;
}


// =========================================================================
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
)
{
    BOOL retval = TRUE;
    LARGE_INTEGER tmpLARGE_INTEGER;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CreateKeyfile\n")));

    // Sanity; existing file exists, keyfile doens't
    retval = (
              SDUCheckFileExists(ExistingFilename) &&
              (!(SDUCheckFileExists(KeyfileFilename)))
             );

    // Create keyfile
    if (retval)
        {
        // Create volume file...
        tmpLARGE_INTEGER.QuadPart = (CRITICAL_DATA_LENGTH / 8);
        retval = _driver_CreateFile(KeyfileFilename, tmpLARGE_INTEGER);
        }


    // Write CDB to new keyfile
    if (retval)
        {
        tmpLARGE_INTEGER.QuadPart = 0;

        retval = _driver_UpdateCDB(
                             ExistingFilename,
                             Offset,
                             ExistingUserPassword,
                             ExistingSaltLength,
                             ExistingKeyIterations,

                             KeyfileFilename,
                             tmpLARGE_INTEGER,
                             KeyfileUserPassword,
                             KeyfileSaltLength,
                             KeyfileKeyIterations,

                             RandomPool
                            );
        }

    if (!(retval))
        {
        // Keyfile create failed; delete any file created
        _driver_DeleteFile(KeyfileFilename);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CreateKeyfile\n")));
    return retval;
}


// =========================================================================
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
)
{
    BOOL retval;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_ChangeDetails\n")));

    retval = _driver_UpdateCDB(
                             Filename,
                             Offset,
                             CurrentUserPassword,
                             CurrentSaltLength,
                             CurrentKeyIterations,

                             Filename,
                             Offset,
                             NewUserPassword,
                             NewSaltLength,
                             NewKeyIterations,

                             RandomPool
                            );
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_ChangeDetails\n")));
    return retval;
}


// =========================================================================
BOOL
driver_BackupCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    WCHAR* DestFilename
)
{
    LARGE_INTEGER zero;
    BOOL retval;
    LARGE_INTEGER size;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_BackupCDB\n")));

    retval = (
              SDUCheckFileExists(SrcFilename) &&
              (!(SDUCheckFileExists(DestFilename)))
             );

    if (retval)
        {
        size.QuadPart = (CRITICAL_DATA_LENGTH / 8);
        retval = _driver_CreateFile(DestFilename, size);
        }

    if (retval)
        {
        zero.QuadPart = 0;
        retval = _driver_BackupRestoreCDB(
                                          SrcFilename,
                                          SrcOffset,
                                          DestFilename,
                                          zero
                                         );

        // On failure, delete the file created...
        if (!(retval))
            {
            _driver_DeleteFile(DestFilename);
            }
    }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_BackupCDB\n")));
    return retval;
}


// =========================================================================
BOOL
driver_RestoreCDB(
    WCHAR* SrcFilename,
    WCHAR* DestFilename,
    LARGE_INTEGER DestOffset
)
{
    LARGE_INTEGER zero;
    BOOL retval;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_RestoreCDB\n")));

    retval = (
              SDUCheckFileExists(SrcFilename) &&
              SDUCheckFileExists(DestFilename)
             );

    if (retval)
        {
        zero.QuadPart = 0;
        retval = _driver_BackupRestoreCDB(
                                          SrcFilename,
                                          zero,
                                          DestFilename,
                                          DestOffset
                                         );
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_RestoreCDB\n")));
    return retval;
}


// =========================================================================
BOOL
_driver_BackupRestoreCDB(
    WCHAR* SrcFilename,
    LARGE_INTEGER SrcOffset,
    WCHAR* DestFilename,
    LARGE_INTEGER DestOffset
)
{
    FREEOTFEBYTE CDB[(CRITICAL_DATA_LENGTH / 8)];
    BOOL retval;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_BackupRestoreCDB\n")));

    retval = FALSE;

    if (!(_driver_ReadWriteRawCDB(
                                  SrcFilename,
                                  SrcOffset,
                                  TRUE,
                                  &CDB
                                 )))
        {
        DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Backup/restore unable to read raw CDB\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Backup/restore raw CDB read in OK.\n")));

        if (!(_driver_ReadWriteRawCDB(
                                      DestFilename,
                                      DestOffset,
                                      FALSE,
                                      &CDB
                                     )))
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Backup/restore unable to write raw CDB\n")));
            }
        else
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Backup/restore raw CDB write OK.\n")));
            retval = TRUE;
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_BackupRestoreCDB\n")));
	return retval;
}


// =========================================================================
// Use the FreeOTFE device driver to mount the specified volume, and either:
//   *) Read in and decrypt specified data, returning the decrypted version
//   *) Encrypt specified data, and write to volume
// readNotWrite - Set to TRUE to read, FALSE to write
BOOL driver_ReadWritePlaintextToVolume(
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

    LARGE_INTEGER dataOffset,  // Offset from within mounted volume from where to read/write data
    int dataLength,  // Length of data to read/write. In bytes
    BYTE* data,  // Data to read/write

    LARGE_INTEGER Offset,
    LARGE_INTEGER Size
)
{
    BOOL allOK;
    DWORD sectorStart;
    DWORD sectorCnt;
    FREEOTFEBYTE* tmpData;

    allOK = TRUE;

    sectorCnt = (dataLength / FREEOTFE_BYTES_PER_SECTOR);
    if ((dataLength % FREEOTFE_BYTES_PER_SECTOR) != 0)
        {
        sectorCnt++;
        }

    sectorStart = (DWORD)(dataOffset.QuadPart / FREEOTFE_BYTES_PER_SECTOR);
    if ((dataOffset.QuadPart % FREEOTFE_BYTES_PER_SECTOR) != 0)
        {
        sectorCnt++;
        }

    tmpData = malloc(sectorCnt * FREEOTFE_BYTES_PER_SECTOR);
    if (tmpData == NULL)
        {
        allOK = FALSE;
        }

    if (allOK)
        {
        allOK = driver_ReadWritePlaintextToVolume_Sectors(
                                        mainDriver,

                                        TRUE,

                                        volFilename,
                                        VolumeKey,
                                        VolumeKeyLength,
                                        SectorIVGenMethod,
                                        IVHashDriver,
                                        IVHashGUID,
                                        IVCypherDriver,
                                        IVCypherGUID,
                                        MainCypherDriver,
                                        MainCypherGUID,
                                        VolumeFlags,

                                        sectorStart,
                                        sectorCnt,
                                        tmpData,

                                        Offset,
                                        Size
                                        );

        }

    if (allOK)
        {
        if (readNotWrite)
            {
            memcpy(data, &(tmpData[(dataOffset.QuadPart % FREEOTFE_BYTES_PER_SECTOR)]), dataLength);
            }
        else
            {
            memcpy(&(tmpData[(dataOffset.QuadPart % FREEOTFE_BYTES_PER_SECTOR)]), data, dataLength);

            allOK = driver_ReadWritePlaintextToVolume_Sectors(
                                            mainDriver,

                                            FALSE,

                                            volFilename,
                                            VolumeKey,
                                            VolumeKeyLength,
                                            SectorIVGenMethod,
                                            IVHashDriver,
                                            IVHashGUID,
                                            IVCypherDriver,
                                            IVCypherGUID,
                                            MainCypherDriver,
                                            MainCypherGUID,
                                            VolumeFlags,

                                            sectorStart,
                                            sectorCnt,
                                            tmpData,

                                            Offset,
                                            Size
                                            );

            }

        }

    SecZeroAndFreeMemory(tmpData, (sectorCnt * FREEOTFE_BYTES_PER_SECTOR));

    return allOK;
}


// =========================================================================
// Use the FreeOTFE device driver to mount the specified volume, and either:
//   *) Read in and decrypt specified data, returning the decrypted version
//   *) Encrypt specified data, and write to volume
// readNotWrite - Set to TRUE to read, FALSE to write
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
)
{
    BOOL allOK;
    DIOC_MOUNT* DIOCMount;
    int DIOCMountSize;
    DWORD deviceHandle;
    DWORD openHandle;
    SG_REQ sgReq;
    DWORD bytesTransferred;


    allOK = TRUE;

    DIOCMountSize = 0;
    DIOCMount = NULL;

    deviceHandle = 0;
    openHandle = 0;

    if (allOK)
        {
        DIOCMount =  _driver_AllocateDIOCMount(            
                                TRUE,

                                NULL,
                                volFilename,
                                readNotWrite,

                                Offset,
                                Size,

                                VolumeKey,
                                VolumeKeyLength,  // In *bits*
                                NULL,
                                0,
                                SectorIVGenMethod,
                                IVHashDriver,
                                IVHashGUID,
                                IVCypherDriver,
                                IVCypherGUID,
                                MainCypherDriver,
                                MainCypherGUID,

                                TRUE,
                                0,

                                (unsigned char*)NULL,
                                0,

                                &DIOCMountSize
                                );
        if (DIOCMount == NULL)
            {
            allOK = FALSE;
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCMount struct (size: %d bytes)\n"), DIOCMountSize));
            }

        }


    if (allOK)
        {
        deviceHandle = mainDriver->FnDSK_Init(NULL, DIOCMount);
        allOK = (deviceHandle != 0);
        }

    if (allOK)
        {
        openHandle = mainDriver->FnDSK_Open(
                        deviceHandle,
                        0,
                        0
                        );
        allOK = (openHandle != 0);
        }

    if (allOK)
        {
        sgReq.sr_start = SectorStart;
        sgReq.sr_num_sec = SectorCount;
        sgReq.sr_num_sg = 1;
        sgReq.sr_status = ERROR_SUCCESS;
        sgReq.sr_callback = NULL;

        sgReq.sr_sglist[0].sb_buf = data;  // lplp - needs to be mapped??
        sgReq.sr_sglist[0].sb_len = (SectorCount * FREEOTFE_BYTES_PER_SECTOR);

        allOK = mainDriver->FnDSK_IOControl(
                            openHandle,
                            IOCTL_DISK_READ,
                            (BYTE*)&sgReq,
                            sizeof(sgReq),
                            NULL,
                            0,
                            &bytesTransferred
                            );

        }


    if (openHandle != 0)
        {
        mainDriver->FnDSK_Close(openHandle);
        openHandle = 0;
        }

    if (deviceHandle != 0)
        {
        mainDriver->FnDSK_Deinit(deviceHandle);
        deviceHandle = 0;
        }

    _driver_FreeDIOCMount(DIOCMount, DIOCMountSize);

    return allOK;
}



// =========================================================================
DIOC_MOUNT* _driver_AllocateDIOCMount(
    BOOL FullMount,

    WCHAR* Mountpoint,
    WCHAR* Filename,
    BOOL ReadOnly,

    LARGE_INTEGER Offset,
    LARGE_INTEGER PartitionSize,

    FREEOTFEBYTE* MasterKey,
    ULONG MasterKeyLength,  // In *bits*
    FREEOTFEBYTE* VolumeIV,
    ULONG VolumeIVLength,  // In *bits*
    SECTOR_IV_GEN_METHOD SectorIVGenMethod,
    WCHAR* IVHashDriver,
    GUID IVHashGUID,
    WCHAR* IVCypherDriver,
    GUID IVCypherGUID,
    WCHAR* MainCypherDriver,
    GUID MainCypherGUID,

    BOOL RevertVolTimestamps,    
    unsigned int VolumeFlags,

    FREEOTFEBYTE* Metadata,
    ULONG MetadataLength,  // In bytes

    int* DIOCMountSize
)
{
    DIOC_MOUNT* retval;
    int localMountsize;
    FREEOTFEBYTE* ptrMasterKey;    
    FREEOTFEBYTE* ptrVolumeIV;    
    //FREEOTFEBYTE* ptrMetaData;

    localMountsize = (
                     sizeof(*retval) - 
                     sizeof(retval->MasterKey) - 
                     sizeof(retval->VolumeIV) - 
                     sizeof(retval->MetaData) +
                     MasterKeyLength + 
                     VolumeIVLength +
                     MetadataLength
                    );

    if (DIOCMountSize != NULL)
        {
        *DIOCMountSize = localMountsize;
        }
        
    retval = malloc(localMountsize);
    if (retval == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc retval struct (size: %d bytes)\n"), localMountsize));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Setting up mount DIOC buffer...\n")));
        
        memset(retval, 0, localMountsize);

        // Populate DIOC structure...
        if (Mountpoint == NULL)
            {
            FullMount = FALSE;
            }
        retval->FullMount = FullMount;
        if (Mountpoint != NULL)
            {
            wcscpy(retval->Mountpoint, Mountpoint);
            }
        wcscpy(retval->Filename, Filename);

        retval->DataStart.QuadPart = Offset.QuadPart;

        if (PartitionSize.QuadPart == 0)
            {
            retval->DataEnd.QuadPart = 0;
            }
        else
            {
            retval->DataEnd.QuadPart = retval->DataStart.QuadPart +
                                          PartitionSize.QuadPart; 
            }

        retval->SectorIVGenMethod = SectorIVGenMethod;
        wcscpy(retval->IVHashDeviceName, IVHashDriver);
        retval->IVHashGUID = IVHashGUID;
        wcscpy(retval->IVCypherDeviceName, IVCypherDriver);
        retval->IVCypherGUID = IVCypherGUID;
        wcscpy(retval->MainCypherDeviceName, MainCypherDriver);
        retval->MainCypherGUID = MainCypherGUID;

        retval->ReadOnly = ReadOnly;

        retval->MountSource = MNTSRC_FILE;

        retval->DeviceType = DEFAULT_MOUNT_DEVICETYPE;

        retval->VolumeFlags = VolumeFlags;
        if (RevertVolTimestamps)
            {
            retval->VolumeFlags &= ~VOL_FLAGS_NORMAL_TIMESTAMPS;
            }
        else
            {
            retval->VolumeFlags |= VOL_FLAGS_NORMAL_TIMESTAMPS;
            }
    
        retval->MasterKeyLength = MasterKeyLength;
        retval->VolumeIVLength = VolumeIVLength;
        retval->MetaDataLength = MetadataLength;

        ptrMasterKey = (PCHAR)&(retval->MasterKey);
        ptrVolumeIV  = ptrMasterKey + (retval->MasterKeyLength / 8);    
        //ptrMetaData  = ptrVolumeIV + (retval->VolumeIVLength / 8);    

        memcpy(
               ptrMasterKey,
               MasterKey, 
               (retval->MasterKeyLength / 8)
              );
        if (retval->VolumeIVLength > 0)
            {
            memcpy(
                 ptrVolumeIV,
                 VolumeIV, 
                 (retval->VolumeIVLength / 8)
                );
            }
        // Metadata unused atm
        //memcpy(
        //       ptrMetaData,
        //       Metadata, 
        //       (retval-->MetaDataLength)
        //      );
        }

    return retval;
}


// =========================================================================
void _driver_FreeDIOCMount(
    DIOC_MOUNT* DIOCMount,
    int DIOCMountSize
)
{
    SecZeroAndFreeMemory(DIOCMount, DIOCMountSize);
}

// =========================================================================
// =========================================================================

