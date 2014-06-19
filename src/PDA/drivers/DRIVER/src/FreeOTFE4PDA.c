// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFENULLGUID.h"  // Must be first
#include "FreeOTFEPlatform.h"

#include "FreeOTFE4PDAContextMgrDevice.h"
#include "FreeOTFE4PDAContextMgrOpen.h"
#include "FreeOTFE4PDA.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAAPI.h"
#ifdef FOTFE_PDA
#include "FreeOTFE4PDARegistry.h"
#endif
#include "FreeOTFEGenerateBlockIV.h"
#include "FreeOTFECallModuleFn.h"
#include "FreeOTFE4PDAAPIConsts.h"

// KDF implementations...
#include "FreeOTFEKDFHashSaltedPassword.h"
#include "FreeOTFEKDFPBKDF2.h"

// MAC implementations...
#include "FreeOTFEMACHash.h"
#include "FreeOTFEMACHMAC.h"

#include "SDUGeneral.h"
#ifdef FOTFE_PDA        
#include <Pkfuncs.h>  // Required for MapPtrToProcess(...)
#endif

BOOL G_contextMgrForceDismounts = FALSE;


// =========================================================================
BOOL WINAPI DllMain(
  HANDLE hinstDLL, 
  DWORD dwReason, 
  LPVOID lpvReserved
)
{
    BOOL retval = TRUE;
    int majorVersion;
    int minorVersion;
    int revisionVersion;
    int buildVersion;
#if DBG
    static BOOL setDebugLevel = FALSE;
    // Default to all on
//    ULONG default_DebugLevel  = 0xFFFFFFFF;  
    // Default to all except verbose debug
    ULONG default_DebugLevel  = 
                                DEBUGLEV_ERROR |
                                DEBUGLEV_WARN  |
                                DEBUGLEV_INFO  |
                                DEBUGLEV_ENTER |
                                DEBUGLEV_EXIT;
    DWORD useDebugLevel;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DllMain\n")));

    if (!(setDebugLevel))
        {
        useDebugLevel = ReadDebugLevelFromFile(DEBUGLEVEL_FILE);
        if (useDebugLevel == FREEOTFE_DEBUG_LEVEL_NOT_READ)
            {
            useDebugLevel = default_DebugLevel;
            }

        FreeOTFEDebugLevel = useDebugLevel;
        setDebugLevel  = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Debug level   : %d\n"), FreeOTFEDebugLevel));
#endif

    if (!(SDUGetVersionInfo(
				            NULL,
				            &majorVersion,
				            &minorVersion, 
				            &revisionVersion, 
				            &buildVersion
				           )))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Driver version: <unable to determine>\n")));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Driver version: v%02d.%02d.%02d.%04d\n"),
                                        majorVersion, 
                                        minorVersion,
                                        revisionVersion,
                                        buildVersion
                                       ));
        }


    switch (dwReason)
        {
        case DLL_PROCESS_ATTACH:
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_ATTACH\n")));
            // We really don't care about getting
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls; disable them
            DisableThreadLibraryCalls(hinstDLL);
   
            retval = contextMgrDevice_Init();
            if (retval)
                {
                retval = contextMgrOpen_Init();
                }

            break;
            }

        case DLL_THREAD_ATTACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_ATTACH\n")));
            break;
            }

        case DLL_THREAD_DETACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_DETACH\n")));
            break;
            }

        case DLL_PROCESS_DETACH:
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_DETACH\n")));
            contextMgrDevice_Deinit();
            contextMgrOpen_Deinit();
            retval = TRUE;
            break;
            }

        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DllMain\n")));
    return retval;
}


// =========================================================================
DWORD DSK_Init(
    LPCTSTR pContext,
    LPCVOID lpvBusContext
)
{
    DWORD retval;
    DEVICE_CONTEXT* devContext = NULL;
    DIOC_MOUNT* DIOCBuffer = NULL; 
    PCHAR ptrMasterKey;    
    PCHAR ptrVolumeIV;    
    PCHAR ptrMetaData;
    BOOL allOK = TRUE;
    DWORD desiredAccess;
    DWORD fileFlags;
    LARGE_INTEGER dataEnd;
    LARGE_INTEGER determinedMaxSize;
    DWORD i;
    FREEOTFEBYTE tmpHashBuffer[FREEOTFE_MAX_HASH_LENGTH];
    unsigned int tmpHashBufferUsed;
    unsigned int useHashBits;
    LARGE_INTEGER tmpLargeInt;
#ifdef FOTFE_PDA
    REGDETAILS_BUILTIN regdetailsBuiltin;
    int profileStrLen;
#endif

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Init\n")));
#ifdef FOTFE_PDA
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Registry key (Active): %ls\n"), pContext));
#endif
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Mount struct         : 0x%0.8x\n"), lpvBusContext));

    DIOCBuffer = (DIOC_MOUNT*)lpvBusContext;        
    // Check param passed in...
    if (allOK)
        {
        // Check the struct passed in is valid.
        // Note that we can't do any other validation than this; we have to assume
        // the struct passed in is valid
        if (DIOCBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Init context passed in invalid.\n")));
            allOK = FALSE;
            }
        }

#ifdef FOTFE_PDA
    if (allOK)
        {
        // Check the struct passed in is valid
        if (
            (DIOCBuffer->FullMount) &&
            (pContext == NULL)
           ) 
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Registry key passed in invalid.\n")));
            allOK = FALSE;
            }
        }
#endif



    // Allocate persistant memory to store disk metadata...
    if (allOK)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to malloc persistant disk metadata...\n")));    
        devContext = malloc(sizeof(*devContext));
        if (devContext == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc persistant disk metadata.\n")));
            allOK = FALSE;
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("malloc'd disk metadata.\n")));    
            }
        }

    
    // From here on in, we're initing the new disk metadata


    if (allOK)
        {
        // Blank struct
        memset(devContext, 0, sizeof(*devContext));
        }

    // Initialize critical section...
    if (allOK)
        {
        devContext->CriticalSection = malloc(sizeof(*devContext->CriticalSection));
        if (devContext->CriticalSection == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc CriticalSection.\n")));
            allOK = FALSE;
            }
        else
            {
            InitializeCriticalSection(devContext->CriticalSection);
            }
        }

    // Initialize various simple members...
    if (allOK)
        {
        devContext->OpenCount = 0;
        devContext->Mounted = TRUE;  // If it can't mount, we'll be destroying this
                                     // struct, so we may as well set "Mounted" to
                                     // TRUE, assuming everything's OK
#ifdef FOTFE_PDA        
        devContext->FullMount = DIOCBuffer->FullMount;
#else
        devContext->FullMount = FALSE;
#endif
        }

#ifdef FOTFE_PDA        
    // Get active registry key entries...
    if (allOK)
        {
        if (!(devContext->FullMount))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Not a full mount; not getting Active registry key entries.\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting Active registry key entries...\n")));
            if (!(RegDetailsGetActiveByKey(
                                     (WCHAR*)pContext,
                                     //FALSE,
                                     &(devContext->RegdetailsActive)
                                    )))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Failed to get active registry entries.\n")));
                allOK = FALSE;
                }
            else
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Active registry key entries:\n")));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ARK builtin key : %ls\n"), devContext->RegdetailsActive.Key));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ARK name        : %ls\n"), devContext->RegdetailsActive.Name));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ARK) mountpoint: %ls\n"), devContext->RegdetailsActive.Mountpoint));
                // The "Hnd" key appears to be set to the same value as returned by
                // ActivateDeviceEx, however this doesn't apear documented; hence we set the
                // handle explicitly on the device so we can retrieve it the device there
                // later
                //DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ARK Hnd        : 0x%0.8x\n"), devContext->RegdetailsActive.Hnd));
                }
            }
        }
#endif

    // Initialize various simple members...
    if (allOK)
        {
        devContext->DismountPending = FALSE;
        devContext->MountSource = DIOCBuffer->MountSource;
        devContext->VolumeFlags = DIOCBuffer->VolumeFlags;
        }

    // Volume filename...
    if (allOK)
        {
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO, 
                        (TEXT("Filename: %ls\n"), 
                        DIOCBuffer->Filename)
                       );
        devContext->zzFilename = calloc(
                                        // +1 for NULL terminator
                                        (wcslen(DIOCBuffer->Filename) + 1),
                                        sizeof(devContext->zzFilename[0])
                                       );
        if (devContext->zzFilename == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to calloc memory to store filename\n")));
            allOK = FALSE;
            }
        else
            {
            wcscpy(devContext->zzFilename, DIOCBuffer->Filename);
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("Volume filename: %ls\n"), 
                            devContext->zzFilename)
                           );
            }
        }

#ifdef FOTFE_PDA
    // Mountpoint...
    if (allOK)
        {
        devContext->Mountpoint = NULL;
        if (!(devContext->FullMount))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Not a full mount; skipping storing mountpoint.\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("Mountpoint: %ls\n"), 
                            DIOCBuffer->Mountpoint)
                           );
            devContext->Mountpoint = calloc(
                                            // +1 for NULL terminator
                                            (wcslen(DIOCBuffer->Mountpoint) + 1),
                                            sizeof(devContext->Mountpoint[0])
                                           );
            if (devContext->Mountpoint == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to calloc memory to store mountpoint\n")));
                allOK = FALSE;
                }
            else
                {
                wcscpy(devContext->Mountpoint, DIOCBuffer->Mountpoint);
                DEBUGOUTMAINDRV(
                                DEBUGLEV_INFO, 
                                (TEXT("Mountpoint: %ls\n"), 
                                devContext->Mountpoint)
                               );
                }
            }
        }
#endif

    // Get file attributes...
    // Note: File attributes obtained before opening file, file timestamps
    //       obtained after opening file
    if (allOK)
        {
        if (devContext->VolumeFlags & VOL_FLAGS_NORMAL_TIMESTAMPS)
            {
            // Don't store file attributes if we're not going to restore them
            // later on
            // If VOL_FLAGS_NORMAL_TIMESTAMPS bit is *set*, we *don't* restore
            // them
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("NOT storing file attributes\n")));
            devContext->FileAttributesStored = FALSE;
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("STORING file attributes\n")));
            devContext->FileAttributes = GetFileAttributes( 
                                                       devContext->zzFilename
                                                      );
            devContext->FileAttributesStored = TRUE;
            }
        }

    // Volume file handle...
    if (allOK)
        {
        desiredAccess = GENERIC_READ;
        if (!(DIOCBuffer->ReadOnly))
            {
            desiredAccess |= GENERIC_WRITE;
            }
        // Note: FILE_ATTRIBUTE_NORMAL only valid if used *alone*
        //fileFlags = FILE_ATTRIBUTE_NORMAL;
        fileFlags = FILE_FLAG_WRITE_THROUGH;
        // lplp??
        // fileFlags |= FILE_FLAG_NO_BUFFERING;
        if (DIOCBuffer->ReadOnly)
            {
            fileFlags |= FILE_ATTRIBUTE_READONLY;
            }

        devContext->FileHandle = CreateFile(
                                            devContext->zzFilename,
                                            desiredAccess,  
                                            (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                                            NULL,
                                            OPEN_EXISTING, 
                                            fileFlags, 
                                            NULL
                                           ); 
        if (devContext->FileHandle == INVALID_HANDLE_VALUE) 
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to open volume file\n")));
            allOK = FALSE;
            } 
        }

    // Get file timestamps...
    // Note: File attributes obtained before opening file, file timestamps
    //       obtained after opening file
    if (allOK)
        {
        if (devContext->VolumeFlags & VOL_FLAGS_NORMAL_TIMESTAMPS)
            {
            // Don't store file attributes if we're not going to restore them
            // later on
            // If VOL_FLAGS_NORMAL_TIMESTAMPS bit is *set*, we *don't* restore
            // them
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("NOT storing file timestamps\n")));
            devContext->FileTimestampsStored = FALSE;
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("STORING file timestamps\n")));
            devContext->FileTimestampsStored = GetFileTime( 
                                                       devContext->FileHandle, 
                                                       &(devContext->CreationTime), 
                                                       &(devContext->LastAccessTime), 
                                                       &(devContext->LastWriteTime) 
                                                      );
            }
        }

    // Volume file size and starting offset...
    if (allOK)
        {
        devContext->DataOffset = DIOCBuffer->DataStart;

        // Store the size of the file/partition for later use...
        if (devContext->MountSource == MNTSRC_PARTITION) 
            {
            allOK = GetMaxSizePartition(
                                        devContext->zzFilename,
                                        &determinedMaxSize
                                       );
            }
        else
            {
            allOK = GetMaxSizeFile(
                                   devContext->FileHandle,
                                   &determinedMaxSize
                                  );
            }
        if (!(allOK))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to determine max possible size.\n")));
            }                          
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Size of actual file/partition: %lld bytes\n"), determinedMaxSize.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Starting offset: %lld bytes\n"), devContext->DataOffset.QuadPart));


        // Encrypted data start/end offsets
        dataEnd = DIOCBuffer->DataEnd;
        if (dataEnd.QuadPart == 0)
            {
            dataEnd = determinedMaxSize;
            }

        // Sanity check...
        if (
            (devContext->DataOffset.QuadPart >= dataEnd.QuadPart) ||
            (dataEnd.QuadPart > determinedMaxSize.QuadPart)
            )
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Invalid value passed as encrypted data start/end.\n")));
            allOK = FALSE;
            }

        // The size of the partition
        devContext->PartitionSize.QuadPart = (dataEnd.QuadPart - 
                                                 devContext->DataOffset.QuadPart);
    }

    // Setup DiskGeometry member...
    if (allOK)
        {
        // Note: When WinCE formats the volume, it loses:
        //
        //         (di_sectors * di_heads) + di_sectors
        //
        //       sectors as overheads. This is the same with the CF and SD cards
        //       I tested.
#ifdef FOTFE_PDA 
        tmpLargeInt.QuadPart = devContext->PartitionSize.QuadPart / BYTES_PER_SECTOR;
        devContext->DiskGeometry.di_total_sectors = tmpLargeInt.LowPart;
        devContext->DiskGeometry.di_bytes_per_sect = BYTES_PER_SECTOR;
        devContext->DiskGeometry.di_sectors = SECTORS_PER_TRACK;
        devContext->DiskGeometry.di_heads = TRACKS_PER_CYLINDER;
        tmpLargeInt.QuadPart = 
                                (
                                  // Encrypted partition size...
                                  devContext->PartitionSize.QuadPart
                                ) / (
                                      devContext->DiskGeometry.di_heads *
                                      devContext->DiskGeometry.di_sectors *
                                      devContext->DiskGeometry.di_bytes_per_sect
                                    );
        devContext->DiskGeometry.di_cylinders = tmpLargeInt.LowPart;
#else
        devContext->DiskGeometry.BytesPerSector = BYTES_PER_SECTOR;
        devContext->DiskGeometry.SectorsPerTrack = SECTORS_PER_TRACK;
        devContext->DiskGeometry.TracksPerCylinder = TRACKS_PER_CYLINDER;
//        devContext->DiskGeometry.MediaType = RemovableMedia;
        devContext->DiskGeometry.MediaType = FixedMedia;
        tmpLargeInt.QuadPart = 
                                (
                                  // Encrypted partition size...
                                  devContext->PartitionSize.QuadPart
                                ) / (
                                      devContext->DiskGeometry.TracksPerCylinder *
                                      devContext->DiskGeometry.SectorsPerTrack *
                                      devContext->DiskGeometry.BytesPerSector
                                    );
        devContext->DiskGeometry.Cylinders.QuadPart = tmpLargeInt.QuadPart;
#endif

#ifdef FOTFE_PDA 
        // DISK_INFO_FLAG_PAGEABLE can be enabled; see: 
        //   http://support.microsoft.com/default.aspx?scid=kb;en-us;Q260874
        // BUT! It shouldn't be enabled as from the MSDN, this would require
        //      read and write requests to be synchronous and do not involve
        //      memory manager calls, loader operations, or thread switches.
        // We could set DISK_INFO_FLAG_CHS_UNCERTAIN, since it's only a simulation,
        // but since we can supply reasonable values...
        // DISK_INFO_FLAG_MBR - not strictly needed(?), but the SD cards and CF cards
        // I tested have it set, and since that's what we're emulating...
        devContext->DiskGeometry.di_flags = (
                                             // DISK_INFO_FLAG_CHS_UNCERTAIN |
                                             DISK_INFO_FLAG_MBR | 
                                             DISK_INFO_FLAG_PAGEABLE 
                                            );
#endif

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Simulated disk geometry:\n")));
#ifdef FOTFE_PDA 
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_total_sectors : %d\n"),
                        devContext->DiskGeometry.di_total_sectors)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_bytes_per_sect: %d\n"),
                        devContext->DiskGeometry.di_bytes_per_sect)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_cylinders     : %d\n"),
                        devContext->DiskGeometry.di_cylinders)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_heads         : %d\n"),
                        devContext->DiskGeometry.di_heads)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_sectors       : %d\n"),
                        devContext->DiskGeometry.di_sectors)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  di_flags         : %d\n"),
                        devContext->DiskGeometry.di_flags)
                       );
#else
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  MediaType       : %d\n"),
                        devContext->DiskGeometry.MediaType)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  BytesPerSector   : %d\n"),
                        devContext->DiskGeometry.BytesPerSector)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  Cylinders        : %d\n"),
                        devContext->DiskGeometry.Cylinders)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  TracksPerCylinder: %d\n"),
                        devContext->DiskGeometry.TracksPerCylinder)
                       );
        DEBUGOUTMAINDRV(
                        DEBUGLEV_INFO,
                        (TEXT("  SectorsPerTrack  : %d\n"),
                        devContext->DiskGeometry.SectorsPerTrack)
                       );
#endif
        }

    if (allOK)
        {
        // The size of the disk (i.e. including hidden sectors)
#ifdef FOTFE_PDA 
        devContext->DiskSize.QuadPart = 
                devContext->DiskGeometry.di_cylinders *
                devContext->DiskGeometry.di_heads *
                devContext->DiskGeometry.di_sectors *
                devContext->DiskGeometry.di_bytes_per_sect;
#else
        devContext->DiskSize.QuadPart = 
                devContext->DiskGeometry.Cylinders.QuadPart *
                devContext->DiskGeometry.TracksPerCylinder *
                devContext->DiskGeometry.SectorsPerTrack *
                devContext->DiskGeometry.BytesPerSector;
#endif
        }

    // Initialize various simple members...
    if (allOK)
        {
        // From the DISK_INFO struct definition, WinCE block devices are only
        // allowed to have a block size of 512 bytes
        // THIS COULD CAUSE PROBLEMS IN FUTURE? For example, a CDROM drive needs
        // this set to 2048 - assuming it's to match the PC version of FreeOTFE...
        devContext->FileSectorSize = MAND_BYTES_PER_SECTOR;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Using file sector size: %d\n"), devContext->FileSectorSize));

#ifdef FOTFE_PDA 
        tmpLargeInt.QuadPart = (devContext->DataOffset.QuadPart % devContext->DiskGeometry.di_bytes_per_sect);
#else
        tmpLargeInt.QuadPart = (devContext->DataOffset.QuadPart % devContext->DiskGeometry.BytesPerSector);
#endif
        devContext->DataOffsetModVirtualSectorSize = tmpLargeInt.LowPart;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("DataOffsetModVirtualSectorSize: %d\n"), devContext->DataOffsetModVirtualSectorSize));

        devContext->ReadOnly = DIOCBuffer->ReadOnly;
        devContext->EncryptionBlockSize = ENCRYPTION_BLOCK_SIZE;
        }

#ifdef FOTFE_PDA
    // Start setting up StorageDeviceInfo member...
    if (allOK)
        {
        if (!(devContext->FullMount))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Not a full mount; skipping setting up StorageDeviceInfo member.\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Setting up StrageDeviceInfo member...\n")));
            devContext->StorageDeviceInfo.cbSize = sizeof(devContext->StorageDeviceInfo);
    
            // Default storage profile in case none set on builtin registry profile        
            // entry...
            wcscpy(devContext->StorageDeviceInfo.szProfile, DEFAULT_PROFILE);
            // From the Active registry key, get the "Builtin" registry key entries
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO,
                            (TEXT("Registry key (Builtin) : %ls\n"), 
                            devContext->RegdetailsActive.Key)
                           );
            if (RegDetailsGetBuiltinByKey(
                                     devContext->RegdetailsActive.Key,
                                     &regdetailsBuiltin
                                    ))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Got Builtin registry entries.\n")));
                if (
                    (wcslen(regdetailsBuiltin.Profile) * 
                              sizeof(devContext->StorageDeviceInfo.szProfile[0])) >
                       sizeof(devContext->StorageDeviceInfo.szProfile)
                   )
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Builtin registry entry identifying profile too long.\n")));
                    DEBUGOUTMAINDRV(
                                    DEBUGLEV_ERROR, 
                                    (TEXT("Max allowed by struct: %d\n"), 
                                    sizeof(devContext->StorageDeviceInfo.szProfile))
                                   );
                    DEBUGOUTMAINDRV(
                                    DEBUGLEV_ERROR, 
                                    (TEXT("Builtin entry length : %d\n"), 
                                    (wcslen(regdetailsBuiltin.Profile) * 
                                      sizeof(devContext->StorageDeviceInfo.szProfile[0])))
                                   );
                    allOK = FALSE;
                    }
                else
                    {
                    profileStrLen = wcslen(regdetailsBuiltin.Profile);
                    wcsncpy(
                            devContext->StorageDeviceInfo.szProfile, 
                            regdetailsBuiltin.Profile, 
                            min(
                                (sizeof(devContext->StorageDeviceInfo.szProfile) / 
                                    sizeof(devContext->StorageDeviceInfo.szProfile[0])),
                                profileStrLen
                               ) 
                           );
                    }
                }
            RegDetailsFree(&regdetailsBuiltin, NULL, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Set storage profile name as: %ls\n"), devContext->StorageDeviceInfo.szProfile));
    
            devContext->StorageDeviceInfo.dwDeviceClass = STORAGE_DEVICE_CLASS_BLOCK;
            devContext->StorageDeviceInfo.dwDeviceType = DIOCBuffer->DeviceType;
            if (DIOCBuffer->ReadOnly)
                {
                devContext->StorageDeviceInfo.dwDeviceFlags = STORAGE_DEVICE_FLAG_READONLY;
                }
            else
                {
                devContext->StorageDeviceInfo.dwDeviceFlags = STORAGE_DEVICE_FLAG_READWRITE;
                }
            }

        }
    // Debug dump simulated storage device details...
    if (allOK)
        {
        if (devContext->FullMount)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Simulated storage device:\n")));
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("  cbSize       : %d\n"),
                            devContext->StorageDeviceInfo.cbSize)
                           );
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("  szProfile    : %ls\n"), 
                            devContext->StorageDeviceInfo.szProfile)
                           );
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("  dwDeviceClass: %d\n"), 
                            devContext->StorageDeviceInfo.dwDeviceClass)
                           );
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("  dwDeviceType : %d\n"), 
                            devContext->StorageDeviceInfo.dwDeviceType)
                           );
            DEBUGOUTMAINDRV(
                            DEBUGLEV_INFO, 
                            (TEXT("  dwDeviceFlags: %d\n"), 
                            devContext->StorageDeviceInfo.dwDeviceFlags)
                           );
            }
        }
#endif

    // Initialize various simple members...
    if (allOK)
        {
        devContext->PreventMediaRemoval = TRUE;        
        }

    // --- OBTAIN THE HASH DRIVER ---
    // Blank, in case an IV hash driver wasn't passed in
    if (allOK)
        {
        // If an IV hash algorithm *was* passed in, get it's details and store them
        if (
            (DIOCBuffer->IVHashDeviceName[0] != 0) &&
            (!(IsEqualGUID(&(DIOCBuffer->IVHashGUID), &FREEOTFE_NULL_GUID)))
           )
            {
            devContext->IVHash.HashGUID = DIOCBuffer->IVHashGUID;
            allOK = GetDeviceDetailsHash(
                                    (WCHAR*)&DIOCBuffer->IVHashDeviceName,
                                    &DIOCBuffer->IVHashGUID,

                                    &(devContext->IVHash)
                                    );
            if (!(allOK))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to get IV hash device\n")));
                }
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("No IV hash algorithm specified.\n")));
            devContext->IVHash.DeviceName = NULL;
            }
        }


    // --- OBTAIN THE IV CYPHER DRIVER ---
    if (allOK)
        {
        // If a IV cypher algorithm *was* passed in, get it's details and store them
        if (
            (DIOCBuffer->IVCypherDeviceName[0] != 0) &&
            (!(IsEqualGUID(&(DIOCBuffer->IVCypherGUID), &FREEOTFE_NULL_GUID)))
           )
            {
            devContext->IVCypher.CypherGUID = DIOCBuffer->IVCypherGUID;
            allOK = GetDeviceDetailsCypher_v3_v1(
                                            (WCHAR*)&DIOCBuffer->IVCypherDeviceName,
                                            &DIOCBuffer->IVCypherGUID,                                                     

                                            &devContext->IVCypher
                                        );
            if (!(allOK))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to get IV cypher device\n")));
                }     
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("No IV cypher specified.\n")));
            devContext->IVCypher.DeviceName = NULL;
            }
        }


    // --- OBTAIN THE MAIN CYPHER DRIVER ---
    if (allOK)
        {
        devContext->MainCypher.CypherGUID = DIOCBuffer->MainCypherGUID;
        allOK = GetDeviceDetailsCypher_v3_v1(
                                        (WCHAR*)&DIOCBuffer->MainCypherDeviceName,
                                        &DIOCBuffer->MainCypherGUID,                                                     

                                        &devContext->MainCypher
                                    );
        if (!(allOK))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to get main cypher device\n")));
            }     
        }


    // Identify pointers to variable length members of mount struct
    if (allOK)
        {
        // This may seem a little weird, but is required as the position of various members
        // within the struct is variable
        ptrMasterKey = (PCHAR)&(DIOCBuffer->MasterKey);
        ptrVolumeIV  = ptrMasterKey + (DIOCBuffer->MasterKeyLength / 8);    
        ptrMetaData  = ptrVolumeIV + (DIOCBuffer->VolumeIVLength / 8);    
        }

    // Copy master key...
    if (allOK)
        {
        devContext->MasterKeyLength = DIOCBuffer->MasterKeyLength;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("MasterKeyLength: %d bits\n"), devContext->MasterKeyLength));

        devContext->MasterKey = malloc((devContext->MasterKeyLength / 8));
        if (devContext->MasterKey == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for MasterKey\n")));
            allOK = FALSE;
            }
        else
            {
            memcpy(
                   devContext->MasterKey, 
                   ptrMasterKey, 
                   (devContext->MasterKeyLength / 8)
                  );
            for(i = 0; i < (devContext->MasterKeyLength / 8); i++)
                { 
                DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- MasterKey [%.2d]: %.2x\n"), i, devContext->MasterKey[i]));
                }                                

            // Divide by 8 to get bytes from bits, then multiply by 2 to get nibbles
            // Note: +2 in order to store two terminating NULLs; just in case
            devContext->MasterKeyASCII = malloc((((devContext->MasterKeyLength / 8) * 2) + 2));
            if (devContext->MasterKeyASCII == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for MasterKeyASCII\n")));
                allOK = FALSE;
                }
            else
                {
                // Convert the data passed in to it's ASCII representation
                ConvertDataToASCIIRep(
                                      devContext->MasterKeyLength,
                                      devContext->MasterKey,
                                      devContext->MasterKeyASCII
                                     );

                // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
                //       DEBUG BUILDS
                //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
                //       this debug code gets removed
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- Key as ASCII: %s\n"), devContext->MasterKeyASCII));
                }
            }
        }

    // Initialize various simple members...
    if (allOK)
        {
        devContext->PreventMediaRemoval = TRUE;        
        devContext->SectorIVGenMethod = DIOCBuffer->SectorIVGenMethod;
        }

    if (allOK)
        {
        // --- HASH THE MASTER KEY FOR ESSIV (IV) USE ---
        // WARNING: Must get the cypher, hash and SectorIVGenMethod *first*
        // Note: Because this is for ESSIV use, we only keep the first (cypher keysize) bits of
        //       the hash; rightpadding as necessary

        // If we're not using ESSIV to generate IVs, we don't bother hashing the key to generate
        // an ESSIV key; we act as though the hash algorithm just returned no data.
        if (devContext->SectorIVGenMethod != SCTRIVGEN_ESSIV)
            {
            devContext->ESSIVKeyLength = 0;
            devContext->ESSIVKey = NULL;
            devContext->ESSIVKeyASCII = NULL;
            }
        else
            {
            // We use a temp buffer as we can't guarantee how long the hash output will be
            // e.g. if it's a variable length hash like "unhashed/NULL"
            tmpHashBufferUsed = (sizeof(tmpHashBuffer) * 8);  // Convert from bytes to bits

            allOK = (DataHash(
                                &(devContext->IVHash),
                                devContext->MasterKeyLength,
                                devContext->MasterKey,
                                &tmpHashBufferUsed,  // In bits
                                tmpHashBuffer
                                ) == STATUS_SUCCESS);

            if (!(allOK))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to hash key for ESSIV IVs\n")));
                // Overwrite potentially sensitive data in temp buffer...
                SecZeroMemory(tmpHashBuffer, sizeof(tmpHashBuffer));
                }
            
            if (allOK)
                {
                devContext->ESSIVKeyLength = tmpHashBufferUsed;
                useHashBits = tmpHashBufferUsed;
                // Handle if the keysize is fixed
                if (devContext->IVCypher.Details.KeySizeRequired >= 0)
                    {
                    devContext->ESSIVKeyLength = devContext->IVCypher.Details.KeySizeRequired;
                    useHashBits = min(tmpHashBufferUsed, (unsigned int)devContext->IVCypher.Details.KeySizeRequired);
                    }

                // Divide by 8 to get bytes from bits
                devContext->ESSIVKey = malloc((devContext->ESSIVKeyLength / 8));
                if (devContext->ESSIVKey == NULL)
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for ESSIVKey\n")));
                    allOK = FALSE;
                    }
                }
            if (allOK)
                {
                // Zero the buffer in in order to ensure that unused buffer
                // remaining after the key is zero'd, just in case the cypher
                // used is badly behaved
                memset(devContext->ESSIVKey, 0, (devContext->ESSIVKeyLength / 8));
                memcpy(
                       devContext->ESSIVKey,
                       tmpHashBuffer,
                       (useHashBits / 8)
                      );

                // NOTE: THE ESSIV KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
                //       DEBUG BUILDS.
                //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
                //       this debug code gets removed
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- ESSIV key length: %d bits\n"), devContext->ESSIVKeyLength));
                for(i = 0; i < (devContext->ESSIVKeyLength / 8); i++)
                    { 
                    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- ESSIV key [%.2d]: %.2x\n"), i, devContext->ESSIVKey[i]));
                    }                                

                // Divide by 8 to get bytes from bits, then multiply by 2 to get nibbles
                // Note: +2 in order to store two terminating NULLs; just in case
                devContext->ESSIVKeyASCII = malloc(
                                              (((devContext->ESSIVKeyLength / 8) * 2) + 2)
                                                  );
                if (devContext->ESSIVKeyASCII == NULL)
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for ESSIVKeyASCII\n")));
                    allOK = FALSE;
                    }
                }
            if (allOK)
                {
                // Convert the data passed in to it's ASCII representation
                ConvertDataToASCIIRep(
                                      devContext->ESSIVKeyLength,
                                      devContext->ESSIVKey,
                                      devContext->ESSIVKeyASCII
                                     );

                // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
                //       DEBUG BUILDS
                //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
                //       this debug code gets removed
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- ESSIV key as ASCII: %s\n"), devContext->ESSIVKeyASCII));
                }
            }
        }

    // Copy volume IV...
    if (allOK)
        {
        devContext->VolumeIVLength = DIOCBuffer->VolumeIVLength;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("VolumeIVLength: %d bits\n"), devContext->VolumeIVLength));

        devContext->VolumeIV = NULL;
        if (devContext->VolumeIVLength > 0) 
            {
            devContext->VolumeIV = malloc((devContext->VolumeIVLength / 8));
            if (devContext->VolumeIV == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for VolumeIV\n")));
                allOK = FALSE;
                }
            else
                {
                memcpy(
                       devContext->VolumeIV, 
                       ptrVolumeIV, 
                       (devContext->VolumeIVLength / 8)
                      );
                for(i = 0; i < (devContext->VolumeIVLength / 8); i++)
                    { 
                    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- VolumeIV [%.2d]: %.2x\n"), i, devContext->VolumeIV[i]));
                    }
                }
            }
        }

    // Copy any user metadata
    if (allOK)
        {
        devContext->MetaDataLength = DIOCBuffer->MetaDataLength;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("MetaDataLength: %d bytes\n"), devContext->MetaDataLength));

        devContext->MetaData = malloc(devContext->MetaDataLength);
        if (devContext->MetaData == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for MetaData\n")));
            allOK = FALSE;
            }
        else
            {
            memcpy(
                   devContext->MetaData, 
                   ptrMetaData, 
                   devContext->MetaDataLength
                  );
            for(i = 0; i < (devContext->MetaDataLength); i++)
                { 
                DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- MetaData [%.2d]: %.2x\n"), i, devContext->MetaData[i]));
                }                                
            }
        }


    // Store the device context, obtain our device mgr handle
    retval = 0;
    if (allOK)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Obtaining device handle...\n")));
        retval = contextMgrDevice_AddDevice(devContext);
        }

    if (retval == 0)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("FAILED TO INIT.\n")));
        OverwriteDeviceSensitive(devContext);
        SecZeroAndFreeMemory(devContext, sizeof(devContext));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Device handle returned: 0x%0.8x\n"), retval));
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Init\n")));
    return retval;
}


// =========================================================================
// Overwrite sensitive parts of the specified device's extension
// Given a device context, cleardown, free off and overwrite all sensitive
// parts (e.g. encryption/decryption keys, etc)
// !!! WARNING !!!
// This must only ever be called on a disk device which has been UNMOUNTED,
// with nothing remaining to be processed
void
OverwriteDeviceSensitive(DEVICE_CONTEXT* devContext)
{

    // Mutex...
    DeleteCriticalSection(devContext->CriticalSection);
    SecZeroAndFreeMemory(
                         devContext->CriticalSection,
                         sizeof(*devContext->CriticalSection)
                        );
	// Number of times opened
	devContext->OpenCount = 0;

    // Flag if a volume is mounted or not
    devContext->Mounted = FALSE;

#ifdef FOTFE_PDA 
    // User app device handle
    devContext->UserSpaceDeviceHandle = 0;
#endif

#ifdef FOTFE_PDA
    // Registry info...
    if (devContext->FullMount)
        {
        RegDetailsFree(NULL, NULL, &devContext->RegdetailsActive);
        }
#endif

    // Flag if a volume is being dismounted or not
    devContext->DismountPending = TRUE;
    
    // Mount source (e.g. the volume file is a partition or file)
    devContext->MountSource = MNTSRC_UNKNOWN; 

#ifdef FOTFE_PDA 
    // Mountpoint
    if (devContext->FullMount)
        {
        SecZeroAndFreeWCHARMemory(devContext->Mountpoint);
        }
#endif

    // Handle to the file
    if (devContext->FileHandle != NULL)
        {
        // Restore timestamps, if needed
        if (!(devContext->FileTimestampsStored))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("NOT restoring file timestamps\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("RESTORING file timestamps\n")));
            SetFileTime( 
                        devContext->FileHandle, 
                        &(devContext->CreationTime), 
                        &(devContext->LastAccessTime), 
                        &(devContext->LastWriteTime) 
                       );
            }

        CloseHandle(devContext->FileHandle);
        }
    devContext->FileHandle = NULL;

    if (devContext->zzFilename != NULL)
        {
        // Restore attributes, if needed
        if (!(devContext->FileAttributesStored))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("NOT restoring file attributes\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("RESTORING file attributes\n")));
            SetFileAttributes( 
                        devContext->zzFilename, 
                        devContext->FileAttributes
                       );
            }
        }
    // Filename of any mounted volume
    SecZeroAndFreeWCHARMemory(devContext->zzFilename);

    // Flag if file attributes have been stored (e.g. FALSE for partitions, etc)
    devContext->FileAttributesStored = FALSE;
    // Flag if file timestamps have been stored (e.g. FALSE for partitions, etc)
    devContext->FileAttributesStored = FALSE;
	// If stored, the file timestamps
    SecZeroMemory(&devContext->CreationTime, sizeof(devContext->CreationTime));
    SecZeroMemory(&devContext->LastAccessTime, sizeof(devContext->LastAccessTime));
    SecZeroMemory(&devContext->LastWriteTime, sizeof(devContext->LastWriteTime));

    // Start of encrypted data within the file
    devContext->DataOffset.QuadPart = 0;

    // Simulated disk geometry
    devContext->PartitionSize.QuadPart = 0; 
    SecZeroMemory(&devContext->DiskGeometry, sizeof(devContext->DiskGeometry));
    devContext->DiskSize.QuadPart = 0;  

    // The "sector size" in which blocks should actually be read/written to the volume
    // file/partition
    devContext->FileSectorSize = 0;
    devContext->DataOffsetModVirtualSectorSize = 0; 

    // Encryption block size
    devContext->EncryptionBlockSize = 0;

    // Readonly flag
    devContext->ReadOnly = TRUE;

#ifdef FOTFE_PDA
    // Storage device info (i.e. the type of device emulated)
    if (devContext->FullMount)
        {
        SecZeroMemory(&devContext->StorageDeviceInfo, sizeof(devContext->StorageDeviceInfo));
        }
#endif

    // Prevent media removal; for removable disks only
    devContext->PreventMediaRemoval = TRUE; 

    // -----
	// IV Hash device ID
    FreeDeviceDetailsHash(&(devContext->IVHash));

    // -----
	// IV cypher device ID
    FreeDeviceDetailsCypher_v3(&(devContext->IVCypher));

    // -----
	// Main cypher device ID
    FreeDeviceDetailsCypher_v3(&(devContext->MainCypher));

    // -----
    // Key to be used for encryption/decryption
    SecZeroAndFreeMemory(
                         devContext->MasterKey, 
                         (devContext->MasterKeyLength / 8)
                        );
    SecZeroAndFreeMemory(
                         devContext->MasterKeyASCII, 
                         ((devContext->MasterKeyLength / 8) * 2)
                        );
    devContext->MasterKeyLength = 0;

    // Key to be used for ESSIV generation (*if* *required*)
    SecZeroAndFreeMemory(
                         devContext->ESSIVKey, 
                         (devContext->ESSIVKeyLength / 8)
                        );
    SecZeroAndFreeMemory(
                         devContext->ESSIVKeyASCII, 
                         ((devContext->ESSIVKeyLength / 8) * 2)
                        );
    devContext->ESSIVKeyLength = 0;

    // Volume IV to be used to encrypt/decrypt each sector
    SecZeroAndFreeMemory(
                         devContext->VolumeIV, 
                         (devContext->VolumeIVLength / 8)
                        );
    devContext->VolumeIVLength = 0;

    // Various flags
    devContext->VolumeFlags = 0;

    // Sector IV generation method
    devContext->SectorIVGenMethod = SCTRIVGEN_UNKNOWN;

    // Metadata
    SecZeroAndFreeMemory(devContext->MetaData, devContext->MetaDataLength);
    devContext->MetaDataLength = 0;
}


// =========================================================================
// Determine the max size of a partition
// DeviceName - The device name of the partition
BOOL
GetMaxSizePartition(
    IN  WCHAR* DeviceName,
    OUT PLARGE_INTEGER MaxSize
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetMaxSizePartition\n")));

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("!!!!! GetMaxSizePartition NOT YET IMPLEMENTED !!!!!\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("!!!!! GetMaxSizePartition NOT YET IMPLEMENTED !!!!!\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("!!!!! GetMaxSizePartition NOT YET IMPLEMENTED !!!!!\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("!!!!! GetMaxSizePartition NOT YET IMPLEMENTED !!!!!\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("!!!!! GetMaxSizePartition NOT YET IMPLEMENTED !!!!!\n")));
/*
xxx - not yet implemented

Use:

    HANDLE WINAPI OpenPartition(
  HANDLE hStore,
  LPCTSTR szPartitionName
);

    BOOL WINAPI GetPartitionInfo(
  HANDLE hPartition,
  PPARTINFO pPartInfo
);

    BOOL CloseHandle( 
  HANDLE hObject
); 
*/

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetMaxSizePartition\n")));
    return retval;
}


// =========================================================================
BOOL DSK_Deinit(
    DWORD hDevice
)
{
    BOOL retval = FALSE;
	DEVICE_CONTEXT* devContext;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Deinit\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Device handle: 0x%0.8x\n"), hDevice));

    devContext = contextMgrDevice_DeleteDevice(hDevice);
    if (devContext == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to delete from device list\n")));
        // Note: Can get to here if there are handles open on the device
        // Unable to do anything...
        // Note: Device manager appears to ignore the return value from this
        //       function?!
        }
    else
        {
#ifdef FOTFE_PDA
        if (devContext->FullMount)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Removing registry active details...\n")));
            RegDetailsFree(NULL, NULL, &devContext->RegdetailsActive);
            }
#endif

        // Note: This will also close the file handle
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Overwriting device context...\n")));
        OverwriteDeviceSensitive(devContext);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Freeing device context...\n")));
        SecZeroAndFreeMemory(devContext, sizeof(*devContext));

        retval = TRUE;
        }

    // Revert force dismounts (this may also be getting used by open file
    // handles, etc)
    G_contextMgrForceDismounts = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Deinit\n")));
    return retval;
}


// =========================================================================
DWORD DSK_Open(
    DWORD hDevice,
    DWORD AccessCode,
    DWORD ShareMode 
)
{
    DWORD retval = 0;  // Zero indicates device can't be opened
	DEVICE_CONTEXT* devContext;
    DEVICE_LIST_ITEM* listItemDevice;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Open\n")));

    listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
    if (devContext == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
        }
    else
        {
        retval = contextMgrOpen_AddOpenContext(hDevice);
        }
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle returned: 0x%0.8x\n"), retval));
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Open\n")));
    return retval;
}


// =========================================================================
BOOL DSK_Close(
    DWORD hOpen 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Close\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle: 0x%0.8x\n"), hOpen));

    contextMgrOpen_DeleteOpenContext(hOpen);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Close\n")));
    return retval;
}


// =========================================================================
BOOL DSK_IOControl(
    DWORD hOpen,
    DWORD dwCode,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut
)
{
    BOOL retval = FALSE;
	DEVICE_CONTEXT* devContext;
    OPEN_LIST_ITEM* openDetails;
    DWORD hDevice = 0;
    DWORD outputBufferUsed = 0;
    DEVICE_LIST_ITEM* listItemDevice = NULL;

    devContext = NULL;
    openDetails = contextMgrOpen_GetOpenContext(hOpen);
    if (openDetails == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get open handle details: %0.8x\n"), hOpen));
        }
    else
        {
        hDevice = openDetails->hDevice;
        listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
        if (devContext == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
            }
        }

    if (devContext != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_IOControl\n")));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Device handle : 0x%0.8x\n"), openDetails->hDevice));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle   : 0x%0.8x\n"), hOpen));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("IOControl code: 0x%0.8x\n"), dwCode));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("IN buffer     : 0x%0.8x\n"), pBufIn));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("IN size       : %d\n"), dwLenIn));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("OUT buffer    : 0x%0.8x\n"), pBufOut));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("OUT size      : %d\n"), dwLenOut));

        if (devContext->Mounted)
            {
            switch (dwCode)
                {
#ifdef FOTFE_PDA 
                case IOCTL_DISK_DEVICE_INFO:
                    {
                    retval = IOCTL_Std_DiskDeviceInfo(
                                                      devContext,
                                                      pBufIn,
                                                      dwLenIn,
                                                      &outputBufferUsed 
                                                     );
                    break;
                    }
#endif

                case IOCTL_DISK_FORMAT_MEDIA:
                    {
                    retval = IOCTL_Std_DiskFormatMedia(
                                                       devContext,
                                                       &outputBufferUsed 
                                                      );
                    retval = TRUE;
                    break;
                    }

                case DISK_IOCTL_FORMAT_MEDIA:
                    {
                    retval = IOCTL_Std_DiskFormatMedia(
                                                       devContext,
                                                       &outputBufferUsed 
                                                      );
                    break;
                    }

                // WM5.0 and later
                //case IOCTL_DISK_GET_SECTOR_ADDR:
                //    {
                //    // xxx - implement
                //    break;
                //    }

                case IOCTL_DISK_GET_STORAGEID:
                    {
                    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
                    break;
                    // This IOCTL *has* been implemented, but it always returns the
                    // same serial number for *all* volumes.
                    // Not sure what impact that will have; hence disabled for now.
                    retval = IOCTL_Std_DiskGetStorageID(
                                                        devContext,
                                                        pBufOut,
                                                        dwLenOut,
                                                        &outputBufferUsed 
                                                       );
                    break;
                    }

                case DISK_IOCTL_GETINFO:
                    {
                    // Note that IOCTL_DISK_GETINFO and DISK_IOCTL_GETINFO invert
                    // the in/out buffers
                    retval = IOCTL_Std_DiskGetInfo(
                                                   devContext,
                                                   pBufIn,
                                                   dwLenIn,
                                                   &outputBufferUsed 
                                                  );
                    break;
                    }

                case IOCTL_DISK_GETINFO:
                    {
                    // Note that IOCTL_DISK_GETINFO and DISK_IOCTL_GETINFO invert
                    // the in/out buffers
                    retval = IOCTL_Std_DiskGetInfo(
                                                   devContext,
                                                   pBufOut,
                                                   dwLenOut,
                                                   &outputBufferUsed 
                                                  );
                    break;
                    }

                case DISK_IOCTL_INITIALIZED:
                    {
                    retval = IOCTL_Std_DiskInitialized(devContext, &outputBufferUsed);
                    break;
                    }

#ifdef FOTFE_PDA 
                case IOCTL_DISK_GETNAME:
                case DISK_IOCTL_GETNAME:
                    {
                    retval = IOCTL_Std_DiskGetName(
                                                   devContext,
                                                   pBufOut,
                                                   dwLenOut,
                                                   &outputBufferUsed 
                                                  );
                    break;
                    }
#endif

                case IOCTL_DISK_READ:
                case DISK_IOCTL_READ:
                    {
                    retval = IOCTL_Std_DiskReadWrite(
                                                     devContext, 
                                                     TRUE, 
                                                     pBufIn, 
                                                     dwLenIn, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }

                // Windows CE .NET 4.2 and later
                //case IOCTL_DISK_SECURE_WIPE:
                //    {
                //    // xxx - implement
                //    break;
                //    }

                case IOCTL_DISK_SETINFO:
                case DISK_IOCTL_SETINFO:
                    {
                    retval = IOCTL_Std_DiskSetInfo(
                                                   devContext,
                                                   pBufIn,
                                                   dwLenIn,
                                                   &outputBufferUsed 
                                                  );
                    break;
                    }

                case IOCTL_DISK_WRITE:
                case DISK_IOCTL_WRITE:
                    {
                    retval = IOCTL_Std_DiskReadWrite(
                                                     devContext, 
                                                     FALSE, 
                                                     pBufIn, 
                                                     dwLenIn, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }

                // FreeOTFE specific codes...

                case IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS:
                    {
                    retval = IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(
                                                     devContext, 
                                                     pBufOut, 
                                                     dwLenOut, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }

#ifdef FOTFE_PDA
                case IOCTL_FREEOTFE_USER_DEV_HANDLE_SET:
                    {
                    retval = IOCTL_FOTFE_SetUserDevHandle(
                                                     devContext, 
                                                     pBufIn, 
                                                     dwLenIn, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }
#endif

#ifdef FOTFE_PDA 
                case IOCTL_FREEOTFE_USER_DEV_HANDLE_GET:
                    {
                    retval = IOCTL_FOTFE_GetUserDevHandle(
                                                     devContext, 
                                                     pBufOut, 
                                                     dwLenOut, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }
#endif

                case IOCTL_FREEOTFE_SET_FORCE_DISMOUNT:
                    {
                    retval = IOCTL_FOTFE_SetForceDismount(
                                                     devContext, 
                                                     pBufIn, 
                                                     dwLenIn, 
                                                     &outputBufferUsed
                                                    );
                    break;
                    }
                    
                default:
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("Unrecognised DIOC code.\n")));
                    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
                    }

                }

            }
        }


    contextMgrOpen_ReleaseOpenContext(openDetails);
    contextMgrDevice_ReleaseDevice(listItemDevice);

    // Protect against caller passing NULL as pdwActualOut - happens for:
    //
    //   DISK_IOCTL_INITIALIZED
    //   DISK_IOCTL_WRITE
    //
    // (and possibly others)
    // Ugly as hell, but that's WinCE...
    if (pdwActualOut == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Return size: <NULL PTR PASSED IN>\n")));
        }
    else
        {
        *pdwActualOut = outputBufferUsed;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Return size: %d\n"), *pdwActualOut));
        }
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Returning TRUE.\n")));
        SetLastError(ERROR_SUCCESS);
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Returning FALSE.\n")));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_IOControl\n")));
    return retval;
}


// =========================================================================
DWORD DSK_Seek(
    DWORD hOpen,
    long Amount,
    WORD Type 
)
{
    DWORD retval = (DWORD)-1;
    DEVICE_CONTEXT* devContext = NULL;
    OPEN_LIST_ITEM* openDetails;
    DWORD hDevice = 0;
    DEVICE_LIST_ITEM* listItemDevice = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Seek\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle: 0x%0.8x\n"), hOpen));

    openDetails = contextMgrOpen_GetOpenContext(hOpen);
    if (openDetails == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get open handle details: %0.8x\n"), hOpen));
        }
    else
        {
        listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
        if (devContext == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
            }
        }

    if (devContext != NULL)
        {
        hDevice = openDetails->hDevice;
        switch (Type)
            {
            case FILE_BEGIN:
                {
                if (
                    (Amount < 0) ||
                    (abs(Amount) > devContext->DiskSize.QuadPart)
                   )
                    {
                    // Beyond disk...
                    // Do nothing; retval already set to -1
                    }
                else
                    {
                    openDetails->Position.QuadPart = Amount;
                    retval = openDetails->Position.LowPart;
                    }

                break;
                }

            case FILE_CURRENT:
                {
                if (
                    (
                     (Amount < 0) &&
                     (abs(Amount) > openDetails->Position.QuadPart)
                    ) ||
                    (
                     (Amount >= 0) &&
                     ((openDetails->Position.QuadPart + Amount) > devContext->DiskSize.QuadPart)
                    )
                   )
                    {
                    // Beyond or before disk...
                    // Do nothing; retval already set to -1
                    }
                else
                    {
                    openDetails->Position.QuadPart = openDetails->Position.QuadPart +
                                                     Amount;
                    retval = openDetails->Position.LowPart;
                    }

                break;
                }

            case FILE_END:
                {
                if (
                    (Amount > 0) ||
                    (abs(Amount) > devContext->DiskSize.QuadPart)
                   )
                    {
                    // Before disk...
                    // Do nothing; retval already set to -1
                    }
                else
                    {
                    // Yes, +Amount; "Amount" should be a -ve number
                    openDetails->Position.QuadPart = devContext->DiskSize.QuadPart +
                                                     Amount;
                    retval = openDetails->Position.LowPart;
                    }

                break;
                }

            default:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unrecognised value passed for \"Type\": %d\n"), Type));
                // Do nothing; retval already set to -1
                break;
                }

            }
        }


    contextMgrOpen_ReleaseOpenContext(openDetails);
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Seek\n")));
    return retval;
}


// =========================================================================
DWORD DSK_Read(
    DWORD hOpen,
    LPVOID pBuffer,
    DWORD Count 
)
{
    DWORD retval = 0;
    OPEN_LIST_ITEM* openDetails;
	DEVICE_CONTEXT* devContext;
    DWORD hDevice;
    DEVICE_LIST_ITEM* listItemDevice = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Read\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle: 0x%0.8x\n"), hOpen));

    devContext = NULL;
    openDetails = contextMgrOpen_GetOpenContext(hOpen);
    if (openDetails == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get open handle details: %0.8x\n"), hOpen));
        }
    else
        {
        hDevice = openDetails->hDevice;
        listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
        if (devContext == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
            }
        }

    if (devContext != NULL)
        {
        // xxx - implement
        //retval = Count, if successful;
        }


    contextMgrOpen_ReleaseOpenContext(openDetails);
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Read\n")));
    return retval;
}


// =========================================================================
DWORD DSK_Write(
    DWORD hOpen,
    LPCVOID pBuffer,
    DWORD Count 
)
{
    DWORD retval = 0;
    OPEN_LIST_ITEM* openDetails;
	DEVICE_CONTEXT* devContext;
    DWORD hDevice;
    DEVICE_LIST_ITEM* listItemDevice = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_Write\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Open handle: 0x%0.8x\n"), hOpen));

    devContext = NULL;
    openDetails = contextMgrOpen_GetOpenContext(hOpen);
    if (openDetails == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get open handle details: %0.8x\n"), hOpen));
        }
    else
        {
        hDevice = openDetails->hDevice;
        listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
        if (devContext == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
            }
        }

    if (devContext != NULL)
        {
        // xxx - implement
        //retval = Count, if successful;
        }


    contextMgrOpen_ReleaseOpenContext(openDetails);
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_Write\n")));
    return retval;
}


// =========================================================================
void DSK_PowerDown(
    DWORD hDevice 
)
{
    DEVICE_CONTEXT* devContext;
    DEVICE_LIST_ITEM* listItemDevice;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_PowerDown\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Device handle: 0x%0.8x\n"), hDevice));

    listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
    if (devContext == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
        }
    else
        {

        // xxx - implement

        }
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_PowerDown\n")));
}


// =========================================================================
void DSK_PowerUp(
    DWORD hDevice 
)
{
    DEVICE_CONTEXT* devContext;
    DEVICE_LIST_ITEM* listItemDevice;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DSK_PowerUp\n")));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Device handle: 0x%0.8x\n"), hDevice));

    listItemDevice = contextMgrDevice_GetDevice(hDevice, &devContext);
    if (devContext == NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to get device: %0.8x\n"), hDevice));
        }
    else
        {

        // xxx - implement

        }
    contextMgrDevice_ReleaseDevice(listItemDevice);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DSK_PowerUp\n")));
}


// =========================================================================
#ifdef FOTFE_PDA
BOOL
IOCTL_Std_DiskDeviceInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskDeviceInfo\n")));

    if (
        (pBufIn != NULL) &&
        (dwLenIn >= sizeof(devContext->StorageDeviceInfo)) &&
        (devContext->FullMount)
       )
        {
        *((STORAGEDEVICEINFO*)pBufIn) = devContext->StorageDeviceInfo;
        *pdwActualOut = sizeof(devContext->StorageDeviceInfo);
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskDeviceInfo\n")));
    return retval;
}
#endif


// =========================================================================
BOOL
IOCTL_Std_DiskGetInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBuf,
    DWORD dwLen,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskGetInfo\n")));

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("pBufOut: 0x%0.8x\n"), pBuf));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwLenOut: %d\n"), dwLen));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("want size: %d\n"), sizeof(devContext->DiskGeometry)));

    if (
        (pBuf != NULL) &&
        (dwLen >= sizeof(devContext->DiskGeometry))
       )
        {
#ifdef FOTFE_PDA 
        *((DISK_INFO*)pBuf) = devContext->DiskGeometry;
#else
        *((DISK_GEOMETRY*)pBuf) = devContext->DiskGeometry;
#endif
        *pdwActualOut = sizeof(devContext->DiskGeometry);
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskGetInfo\n")));
    return retval;
}


// =========================================================================
BOOL
IOCTL_Std_DiskSetInfo(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskSetInfo\n")));

    if (
        (pBufIn != NULL) &&
        (dwLenIn == sizeof(devContext->DiskGeometry))
       )
        {
#ifdef FOTFE_PDA 
        devContext->DiskGeometry = *((DISK_INFO*)pBufIn);
#else
        devContext->DiskGeometry = *((DISK_GEOMETRY*)pBufIn);
#endif
        *pdwActualOut = 0;
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskSetInfo\n")));
    return retval;
}


// =========================================================================
BOOL
IOCTL_Std_DiskFormatMedia(
    DEVICE_CONTEXT* devContext,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskFormatMedia\n")));

    // Do nothing...
    *pdwActualOut = 0;
    retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskFormatMedia\n")));
    return retval;
}


// =========================================================================
BOOL
IOCTL_Std_DiskReadWrite(
    DEVICE_CONTEXT* devContext,
    BOOL readNotWrite,  // Set to TRUE to read, FALSE to write
    PBYTE pBuf,  // An SG_REQ*
    DWORD dwLen,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    SG_REQ* req;
    DWORD totalSGBufferSize;
    DWORD calcSize;
    DWORD i;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskReadWrite\n")));
    if (readNotWrite) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("READ\n")));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("WRITE\n")));
        }

    *pdwActualOut = 0;

    EnterCriticalSection(devContext->CriticalSection);

    // Check size of min input buffer
    req = (SG_REQ*)pBuf;
    if (dwLen < sizeof(*req))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Input buffer size: %d\n"), dwLen));
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Min input buffer size: %d\n"), sizeof(*req)));
        }
    else
        {
        // Check size of actual input buffer
        // -1 here as the first SG buffer is already counted as part of the SG_REQ struct
        calcSize = sizeof(*req) + (sizeof(req->sr_sglist[0]) * (req->sr_num_sg - 1));
        if (dwLen < calcSize)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Input buffer size: %d\n"), dwLen));
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Expected input buffer size: %d\n"), calcSize));
            }
        // Ensure not trying to write to readonly volume
        else if ((!(readNotWrite)) && (devContext->ReadOnly))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Reject: Write protected.\n")));
            req->sr_status = ERROR_WRITE_PROTECT;
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("sr_start  : %d\n"), req->sr_start));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("sr_num_sec: %d\n"), req->sr_num_sec));

            // Ensure that the read/write is valid (isn't trying to operate 
            // past the length of the drive
            // Note: It's ">" and not ">=" (e.g. reading 512 bytes on a 512 
            //       byte disk, starting from offset 0)
            // Note: Don't have to check that the amount of data is a multiple
            //       of the sector size; an SG_REQ struct *can* only be a 
            //       multiple of this value
            if ( 
#ifdef FOTFE_PDA 
                ((req->sr_start + req->sr_num_sec) * devContext->DiskGeometry.di_bytes_per_sect)
                         > devContext->PartitionSize.QuadPart
#else
                ((req->sr_start + req->sr_num_sec) * devContext->DiskGeometry.BytesPerSector)
                         > devContext->PartitionSize.QuadPart
#endif
                         ) {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Invalid request; rejecting (sector not found).\n")));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Request:\n")));
#ifdef FOTFE_PDA 
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Starting offset      : %d\n"), (req->sr_start * devContext->DiskGeometry.di_bytes_per_sect)));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  I/O Length           : %d\n"), (req->sr_num_sec * devContext->DiskGeometry.di_bytes_per_sect)));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Device PartitionSize : %lld\n"), devContext->PartitionSize.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Device BytesPerSector: %d\n"), devContext->DiskGeometry.di_bytes_per_sect));
#else
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Starting offset      : %d\n"), (req->sr_start * devContext->DiskGeometry.BytesPerSector)));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  I/O Length           : %d\n"), (req->sr_num_sec * devContext->DiskGeometry.BytesPerSector)));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Device PartitionSize : %lld\n"), devContext->PartitionSize.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Device BytesPerSector: %d\n"), devContext->DiskGeometry.BytesPerSector));
#endif                
                req->sr_status = ERROR_SECTOR_NOT_FOUND;
                } 
            else
                {
                // Sanity check; have we have been supplied with a large 
                // enough set of scatter buffers?
                totalSGBufferSize = 0;
                for(i=0; i<req->sr_num_sg; i++)
                    {
                    totalSGBufferSize += req->sr_sglist[i].sb_len;
                    }
#ifdef FOTFE_PDA 
                if (totalSGBufferSize < (req->sr_num_sec * devContext->DiskGeometry.di_bytes_per_sect))
#else
                if (totalSGBufferSize < (req->sr_num_sec * devContext->DiskGeometry.BytesPerSector))
#endif
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Invalid request; rejecting (total SG buffers not big enough).\n")));
#ifdef FOTFE_PDA 
                    DEBUGOUTMAINDRV(
                                    DEBUGLEV_INFO, 
                                    (TEXT("Total buffers required : %d bytes\n"),
                                    (req->sr_num_sec * devContext->DiskGeometry.di_bytes_per_sect))
                                   );
#else
                    DEBUGOUTMAINDRV(
                                    DEBUGLEV_INFO, 
                                    (TEXT("Total buffers required : %d bytes\n"),
                                    (req->sr_num_sec * devContext->DiskGeometry.BytesPerSector))
                                   );
#endif                
                    DEBUGOUTMAINDRV(
                                    DEBUGLEV_INFO, 
                                    (TEXT("Total buffers suppliced: %d bytes\n"), 
                                    totalSGBufferSize)
                                   );
                    req->sr_status = ERROR_INSUFFICIENT_BUFFER;
                    }
                else
                    {
                    // Read/write request valid; process.
                    retval = ProcessValidReadWrite(
                                                   devContext,
                                                   readNotWrite,
                                                   req,
                                                   dwLen,
                                                   pdwActualOut 
                                                  );
                    }
                }
            }
        }

    LeaveCriticalSection(devContext->CriticalSection);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Status: %d (ERROR_SUCCESS is: %d)\n"), req->sr_status, ERROR_SUCCESS));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskReadWrite\n")));
    return retval;
}
    
// =========================================================================
// Process a *valid* SG_REQ read/write request
BOOL
ProcessValidReadWrite(
    DEVICE_CONTEXT* devContext,
    BOOL readNotWrite,  // Set to TRUE to read, FALSE to write
    SG_REQ* req,
    DWORD dwLen,
    PDWORD pdwActualOut
)
{
    BOOL retval;
    LARGE_INTEGER pos;
    DWORD currSGIdx;
    DWORD currSGSize;
    PBYTE currSGBuf;
    LARGE_INTEGER sector64bit;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("ProcessValidReadWrite\n")));

    // Assume everything goes OK...
    req->sr_status = ERROR_SUCCESS;
    retval = TRUE;

    // Short circuit if there's nothing to actually do...
    if (dwLen == 0)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Zero length request. Completing.\n")));
        // Nothing to do; already defaults to success
        }
    else
        {
        // Must use a 64 bit int as temp store, otherwise pos.QuadPart will be 
        // calculated using 32 bit artithmetic - resulting in reading/writing 
        // the wrong sector if it goes past 4GB
        sector64bit.QuadPart = req->sr_start;
#ifdef FOTFE_PDA 
        pos.QuadPart = (sector64bit.QuadPart * devContext->DiskGeometry.di_bytes_per_sect);
#else
        pos.QuadPart = (sector64bit.QuadPart * devContext->DiskGeometry.BytesPerSector);
#endif

        currSGIdx = 0;
        while (currSGIdx < req->sr_num_sg)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Buffer           : %d\n"), currSGIdx));
            currSGSize = req->sr_sglist[currSGIdx].sb_len;
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Size             : %d\n"), currSGSize));
            currSGBuf = req->sr_sglist[currSGIdx].sb_buf;                
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ptr              : 0x%0.8x\n"), currSGBuf));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("offset           : %lld\n"), pos.QuadPart));
#ifdef FOTFE_PDA 
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("sector           : %d\n"), (pos.QuadPart / devContext->DiskGeometry.di_bytes_per_sect)));
#else
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("sector           : %d\n"), (pos.QuadPart / devContext->DiskGeometry.BytesPerSector)));
#endif

#ifdef FOTFE_PDA 
            // Prevent caller from attempting to get us to write to memory
            // caller isn't allowed to...
            // Note: MapPtrToProcess(...) depreciated; using MapCallerPtr(...)
            //       instead
            if (devContext->FullMount)
                {
                currSGBuf = MapCallerPtr(currSGBuf, currSGSize);
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ptr (map caller) : 0x%0.8x\n"), currSGBuf));
                }
#endif
            if (currSGBuf != NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("currSGSize: %d\n"), currSGSize));
                req->sr_status = ReadWriteData(
                                               devContext,
                                               readNotWrite,
                                               &pos,
                                               currSGSize,
                                               currSGBuf
                                              );
                if (req->sr_status != ERROR_SUCCESS)
                    {
                    // Bail...
                    break;
                    }

                pos.QuadPart += currSGSize;
                }

            currSGIdx++;
            }
        
        if (req->sr_status != ERROR_SUCCESS)
            {
            retval = FALSE;
            }
        else
            {
            // Not particularly clear what this should be set to;
            // assuming the size of the input buffer
            //*pdwActualOut = dataTransferred;
            //*pdwActualOut = sizeof(*req);
            *pdwActualOut = dwLen;
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("ProcessValidReadWrite\n")));
    return retval;
}


// =========================================================================
// This function is OPTIONAL; the driver doesn't actually need it.
BOOL
IOCTL_Std_DiskGetStorageID(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    STORAGE_IDENTIFICATION* ptrStorageID;
    DWORD sizeRequired;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskGetStorageID\n")));

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("Input buffer size: %d\n"), dwLenOut));
    if (dwLenOut < sizeof(*ptrStorageID))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("Buffer size less than minimum\n")));
        SetLastError(ERROR_INSUFFICIENT_BUFFER);
        }
    else
        {
        ptrStorageID = (STORAGE_IDENTIFICATION*)pBufOut;

        // If the buffer's not big enough for both the struct and our data, fail
        // but set dwSize in the struct
        sizeRequired = sizeof(*ptrStorageID) +
                       min(
                           strlen(STORAGE_MANUFACTURER),
                           STORAGE_MAX_LEN_MANUFACTURER
                          )+ // Storage manufacturer
                       1 +  // NULL terminator
                       min(
                           strlen(STORAGE_SERIALNUM),
                           STORAGE_MAX_LEN_SERIALNUM
                          )+ // Serial number
                       1;  // NULL terminator
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("Correct buffer size: %d\n"), sizeRequired));
        if (dwLenOut < sizeRequired)
            {
            ptrStorageID->dwSize = sizeRequired;
            // Not clear if we should set this to 0, on the basis that they
            // *will* be valid if the user supplies a big enough buffer...
            ptrStorageID->dwFlags = (
                                    MANUFACTUREID_INVALID | 
                                    SERIALNUM_INVALID
                                   );
            *pdwActualOut = sizeof(*ptrStorageID);
            SetLastError(ERROR_INSUFFICIENT_BUFFER);
            }
        else
            {
            ptrStorageID->dwSize = sizeRequired;
            ptrStorageID->dwFlags = 0; // Both valid
            ptrStorageID->dwManufactureIDOffset = sizeof(*ptrStorageID);
            ptrStorageID->dwSerialNumOffset = sizeof(*ptrStorageID) + 
                                              min(
                                                  strlen(STORAGE_MANUFACTURER),
                                                  STORAGE_MAX_LEN_MANUFACTURER
                                                 ) +
                                              1;  // NULL terminator
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwManufactureIDOffset: %d\n"), ptrStorageID->dwManufactureIDOffset));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwSerialNumOffset: %d\n"), ptrStorageID->dwSerialNumOffset));

            // Manufacturer string
            strncpy(
                    (char*)(pBufOut + (ptrStorageID->dwManufactureIDOffset)),
                    STORAGE_MANUFACTURER,
                    (min(
                         strlen(STORAGE_MANUFACTURER),
                         STORAGE_MAX_LEN_MANUFACTURER
                        ) + 1) // +1 for terminating NULL
                   );
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwManufactureIDOffset: %d\n"), ptrStorageID->dwManufactureIDOffset));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwSerialNumOffset: %d\n"), ptrStorageID->dwSerialNumOffset));

            // Serial number
            // Note: Serial number probably *should* be random, and stored as
            //       part of the volume
            strncpy(
                    (char*)(pBufOut + (ptrStorageID->dwSerialNumOffset)),
                    STORAGE_SERIALNUM,
                    (min(
                         strlen(STORAGE_SERIALNUM),
                         STORAGE_MAX_LEN_SERIALNUM 
                        ) + 1) // +1 for terminating NULL
                   );
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwManufactureIDOffset: %d\n"), ptrStorageID->dwManufactureIDOffset));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwSerialNumOffset: %d\n"), ptrStorageID->dwSerialNumOffset));

            *pdwActualOut = sizeRequired;
            retval = TRUE;
            }

        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskGetStorageID\n")));
    return retval;
}


// =========================================================================
BOOL
IOCTL_Std_DiskInitialized(
    DEVICE_CONTEXT* devContext,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskInitialized\n")));

    // Do nothing...
    *pdwActualOut = 0;
    retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskInitialized\n")));
    return retval;
}


// =========================================================================
#ifdef FOTFE_PDA 
BOOL
IOCTL_Std_DiskGetName(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    REGDETAILS_BUILTIN detailsBuiltin;
    REGDETAILS_PROFILE detailsProfile;
    DWORD sizeRequired;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_Std_DiskGetName\n")));

    *pdwActualOut = 0;
    
    if (devContext->FullMount)
        {
        if (RegDetailsGetAllByKey(
                                  devContext->RegdetailsActive.Key,
                                  &detailsBuiltin,
                                  &detailsProfile
                                 ))
            {
            sizeRequired = ((wcslen(detailsProfile.Folder) + 1) * sizeof(WCHAR));
            if (dwLenOut >= sizeRequired)
                {
                wcscpy((WCHAR*)pBufOut, detailsProfile.Folder); 
                *pdwActualOut = sizeRequired;
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Returning folder name: %ls\n"), pBufOut));
                retval = TRUE;
                }
    
            }
    
        RegDetailsFree(
                       &detailsBuiltin,
                       &detailsProfile,
                       NULL
                      );
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_Std_DiskGetName\n")));
    return retval;
}
#endif


// =========================================================================
// Read/write data to the specified device
// IMPORTANT: Sanity check as to whether the volume is readonly *must* have
//            been already carried out *before* calling this function.
// IMPORTANT: Sanity check to ensure that the read/write doesn't go past the
//            end of the volume *must* have already been carried out *before*
//            calling this function.
// IMPORTANT: Sanity check to ensure that the length of data read/written is a
//            multiple of the sector size *must* have already been carried out
//            *before* calling this function.
// Returns: Request status (SG_REQ.sr_status)
DWORD
ReadWriteData(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      BOOL ReadNotWrite,
    IN      PLARGE_INTEGER PartitionOffset,
    IN      DWORD BufferLength,
    IN OUT  PBYTE Buffer
)
{
    DWORD status;
    PBYTE tmpBuffer;
    unsigned int offset;
    unsigned int tmpBufferSize;
    LARGE_INTEGER fileOffset;
    LARGE_INTEGER tmpOffset;
        
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("ReadWriteData\n")));
    
    status = ERROR_GEN_FAILURE;
    
    if (ReadNotWrite) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("zzzREAD - READ - READ\n")));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("zzzWRITE - WRITE - WRITE\n")));
        }
        
    if (Buffer != NULL)
        {
        tmpBufferSize = BufferLength;
        tmpBuffer = malloc(tmpBufferSize);
        if (tmpBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc tmp buffer for read/write operation\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Got tmp read/write buffer\n")));
            fileOffset.QuadPart = PartitionOffset->QuadPart + DeviceContext->DataOffset.QuadPart;
            if (!(ReadNotWrite))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ReadWriteData WRITE\n")));            
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Request:\n")));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Partition offset: %lld\n"), PartitionOffset->QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  File offset     : %lld\n"), fileOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  I/O Length      : %d\n"), tmpBufferSize));

                // Encrypt in sector sized chunks
                // Note: The caller must have already ensured that
                //       BufferLength is a multiple of the number of bytes
                ///      per sector
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Encrypting data...\n")));
                status = ERROR_SUCCESS;
                for(offset=0; offset<tmpBufferSize; offset += DeviceContext->EncryptionBlockSize)
                    {
                    tmpOffset.QuadPart = (fileOffset.QuadPart + offset);
                    if (BlockEncrypt(
                                       DeviceContext,
                                       tmpOffset,
                                       &Buffer[offset],
                                       &tmpBuffer[offset]
                                      ) != STATUS_SUCCESS)
                        {
                        status = ERROR_GEN_FAILURE;
                        break;
                        }
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Encrypted with status: %d (success is %d)\n"), status, ERROR_SUCCESS));
                
                if (status == ERROR_SUCCESS)
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Writing...\n")));
                    status = ActualWriteFile(
                                        DeviceContext,
                                        &fileOffset,
                                        tmpBufferSize,    
                                        tmpBuffer
                                    );
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Written with status: %d (success is %d)\n"), status, ERROR_SUCCESS));
                    }
                                
                }
            else
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ReadWriteData READ\n")));            
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Request:\n")));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  Partition offset: %lld\n"), PartitionOffset->QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  File offset     : %lld\n"), fileOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("-----  I/O Length      : %d\n"), tmpBufferSize));

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Reading...\n")));
                status = ActualReadFile(
                                    DeviceContext,
                                    &fileOffset,
                                    tmpBufferSize,    
                                    tmpBuffer
                                );
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Read with status: %d (success is %d)\n"), status, ERROR_SUCCESS));

                if (status == ERROR_SUCCESS)
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Decrypting data...\n")));
                    // Decrypt in sector sized chunks
                    // Note: The caller must have already ensured that
                    //       BufferLength is a multiple of the number of bytes
                    ///      per sector
                    for(offset=0; offset<tmpBufferSize; offset += DeviceContext->EncryptionBlockSize)
                        {
                        tmpOffset.QuadPart = (fileOffset.QuadPart + offset);
                        if (BlockDecrypt(
                                           DeviceContext,
                                           tmpOffset,
                                           &tmpBuffer[offset],
                                           &Buffer[offset]
                                          ) != STATUS_SUCCESS)
                            {
                            status = ERROR_GEN_FAILURE;
                            break;
                            }
                        }
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Decrypted with status: %d (success is %d)\n"), status, ERROR_SUCCESS));
                    }

                }
                
                
            SecZeroAndFreeMemory(tmpBuffer, tmpBufferSize);
            }
        }
        

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("ThreadReadWriteData returning %d\n"), status));
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("ReadWriteData\n")));
    
    return status;
}


// =========================================================================
// Perform an actual read from the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
DWORD
ActualReadFile(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    IN      PBYTE Buffer
)
{
    DWORD status;
    LARGE_INTEGER transferOffset;
    PBYTE transferBuffer = NULL;
    ULONG transferBufferLength;
    ULONG preData;
    ULONG postData;
    LARGE_INTEGER tmpLargeInt;
    DWORD bytesTransferred;
    DWORD newFilePos;
    BOOL filePosOK;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("ActualReadFile\n")));

    // WARNING: PC version assumes SUCCESS; PDA version assumes FAILURE
    // xxx - change to ERROR_READ_FAULT?
    status = ERROR_GEN_FAILURE;

    // Round the offset *down* to the volume file/partition's sector size
    tmpLargeInt.QuadPart = (Offset->QuadPart % DeviceContext->FileSectorSize);
    preData = tmpLargeInt.LowPart;

    // Round the offset *up* to the volume file/partition's sector size
    postData = 0;
    tmpLargeInt.QuadPart = ((Offset->QuadPart + BufferLength) % DeviceContext->FileSectorSize);
    if (tmpLargeInt.LowPart != 0)
        {
        postData = DeviceContext->FileSectorSize - tmpLargeInt.LowPart;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("preData : %d\n"), preData));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("postData: %d\n"), postData));

    transferOffset.QuadPart = Offset->QuadPart;
    transferBufferLength = BufferLength;
    transferBuffer = Buffer;
    if (
        (preData != 0) ||
        (postData != 0)
       )
        {
        transferOffset.QuadPart = (Offset->QuadPart - preData);
        transferBufferLength = preData + BufferLength + postData;
        transferBuffer = malloc(transferBufferLength);
        if (transferBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc transferBuffer (%d bytes)\n"), transferBufferLength));
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Requested data length : %d\n"), BufferLength));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Actual transfer length: %d\n"), transferBufferLength));

    // Sanity
    if (transferBuffer != NULL)
        {
        newFilePos = SetFilePointer( 
                                    DeviceContext->FileHandle, 
                                    transferOffset.LowPart, 
                                    &(transferOffset.HighPart), 
                                    FILE_BEGIN
                                   );
        filePosOK = TRUE;
        if (newFilePos == FAILED_SETFILEPOINTER) 
            {
            filePosOK = (GetLastError() == NO_ERROR);
            }
        if (filePosOK) 
            {
            if (ReadFile( 
                         DeviceContext->FileHandle, 
                         transferBuffer, 
                         transferBufferLength, 
                         &bytesTransferred, 
                         NULL
                        ))
                {
                if (bytesTransferred == transferBufferLength)
                    {
                    if (
                        (preData != 0) ||
                        (postData != 0)
                       )
                        {
                        FREEOTFE_MEMCPY(
                                      Buffer,
                                      &transferBuffer[preData],
                                      BufferLength
                                     );
                        }

                    status = ERROR_SUCCESS;
                    }
                }
            }

    }

    if (
        (preData != 0) ||
        (postData != 0)
       )
        {
        if (transferBuffer != NULL)
            {
            SecZeroAndFreeMemory(transferBuffer, transferBufferLength);
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("ActualReadFile\n")));
    return status;
}


// =========================================================================
// Perform an actual write to the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
DWORD
ActualWriteFile(
    IN      DEVICE_CONTEXT* DeviceContext,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    OUT     PBYTE Buffer
)
{
    DWORD status;
    LARGE_INTEGER transferOffset;
    PBYTE transferBuffer = NULL;
    ULONG transferBufferLength;
    ULONG preData;
    ULONG postData;
    LARGE_INTEGER tmpLargeInt;
    DWORD bytesTransferred;
    DWORD newFilePos;
    BOOL filePosOK;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("ActualWriteFile\n")));

    // WARNING: PC version assumes SUCCESS; PDA version assumes FAILURE
    // xxx - change to ERROR_WRITE_FAULT?
    status = ERROR_GEN_FAILURE;

    // Round the offset *down* to the volume file/partition's sector size
    tmpLargeInt.QuadPart = (Offset->QuadPart % DeviceContext->FileSectorSize);
    preData = tmpLargeInt.LowPart;

    // Round the offset *up* to the volume file/partition's sector size
    postData = 0;
    tmpLargeInt.QuadPart = ((Offset->QuadPart + BufferLength) % DeviceContext->FileSectorSize);
    if (tmpLargeInt.LowPart != 0)
        {
        postData = DeviceContext->FileSectorSize - tmpLargeInt.LowPart;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("preData : %d\n"), preData));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("postData: %d\n"), postData));

    transferOffset.QuadPart = Offset->QuadPart;
    transferBufferLength = BufferLength;
    transferBuffer = Buffer;
    // If there is pre/post data, read in whatever's already on the disk in order to
    // preserve it
    if (
        (preData != 0) ||
        (postData != 0)
       )
        {
        transferOffset.QuadPart = (Offset->QuadPart - preData);
        transferBufferLength = preData + BufferLength + postData;
        transferBuffer = malloc(transferBufferLength);

        if (transferBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to malloc transferBuffer (%d bytes)\n"), transferBufferLength));
            }
        else
            {
            // Preserve encrypted data already stored between volume's sector boundry, and the
            // offset where we are to start writing
            if (preData != 0)
                {
                status = ActualReadFile(
                            DeviceContext,
                            &transferOffset,
                            preData,
                            transferBuffer
                            );
                }

            // Preserve encrypted data already stored after the buffer to be written ends, if
            // it doesn't end of a sector boundry, and the
            if (postData != 0)
                {
                // Yes, this is correct - think about it
                tmpLargeInt.QuadPart = (Offset->QuadPart + BufferLength);
                status = ActualReadFile(
                            DeviceContext,
                            &tmpLargeInt,
                            postData,
                            // Yes, this is also correct - think about it
                            &transferBuffer[(preData + BufferLength)]
                            );
                }

            memcpy(
                   &transferBuffer[preData],
                   Buffer,
                   BufferLength
                  );
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Requested data length : %d\n"), BufferLength));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Actual transfer length: %d\n"), transferBufferLength));

    if (transferBuffer != NULL)
        {
        newFilePos = SetFilePointer( 
                                    DeviceContext->FileHandle, 
                                    transferOffset.LowPart, 
                                    &(transferOffset.HighPart), 
                                    FILE_BEGIN
                                   );
        filePosOK = TRUE;
        if (newFilePos == FAILED_SETFILEPOINTER) 
            {
            filePosOK = (GetLastError() == NO_ERROR);
            }
        if (filePosOK) 
            {
            if (WriteFile(
                          DeviceContext->FileHandle,
                          transferBuffer,
                          transferBufferLength,
                          &bytesTransferred, 
                          NULL
                         ))
                {
                if (bytesTransferred == transferBufferLength)
                    {
                    status = ERROR_SUCCESS;
                    }
                }
            }

        }

    if (
        (preData != 0) ||
        (postData != 0)
       )
        {
        if (transferBuffer != NULL)
            {
            SecZeroAndFreeMemory(transferBuffer, transferBufferLength);
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("ActualWriteFile\n")));
    return status;
}


// =========================================================================
BOOL
IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    DIOC_DISK_DEVICE_STATUS* DIOCBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus\n")));

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Want DIOC size: %d bytes\n"), sizeof(*DIOCBuffer)));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Got DIOC size : %d bytes\n"), dwLenOut));
    if (
        (pBufOut != NULL) &&
        (dwLenOut == sizeof(*DIOCBuffer))
       )
        {
        DIOCBuffer = (DIOC_DISK_DEVICE_STATUS*)pBufOut;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Populating structure...\n")));

#ifdef FOTFE_PDA 
        wcscpy(DIOCBuffer->Mountpoint, devContext->Mountpoint);
#endif
        wcscpy(DIOCBuffer->Filename, devContext->zzFilename);

#ifdef FOTFE_PDA 
        DIOCBuffer->ActivateDeviceHandle = devContext->UserSpaceDeviceHandle;
        if (devContext->FullMount)
            {
            wcscpy(DIOCBuffer->BuiltinKey, devContext->RegdetailsActive.Key);
            wcscpy(DIOCBuffer->DeviceName, devContext->RegdetailsActive.Name);
            }
#endif

        if (devContext->IVHash.DeviceName != NULL)
            {
            wcscpy(DIOCBuffer->IVHashDeviceName, devContext->IVHash.DeviceName);
            DIOCBuffer->IVHashGUID = devContext->IVHash.HashGUID;
            }
        else
            {
            ZeroMemory(DIOCBuffer->IVHashDeviceName, sizeof(DIOCBuffer->IVHashDeviceName));
            ZeroMemory(&(DIOCBuffer->IVHashGUID), sizeof(DIOCBuffer->IVHashGUID));
            }

        if (devContext->IVCypher.DeviceName != NULL)
            {
            wcscpy(DIOCBuffer->IVCypherDeviceName, devContext->IVCypher.DeviceName);
            DIOCBuffer->IVCypherGUID = devContext->IVCypher.CypherGUID;
            }
        else
            {
            ZeroMemory(DIOCBuffer->IVCypherDeviceName, sizeof(DIOCBuffer->IVCypherDeviceName));
            ZeroMemory(&(DIOCBuffer->IVCypherGUID), sizeof(DIOCBuffer->IVCypherGUID));
            }
        
        wcscpy(DIOCBuffer->MainCypherDeviceName, devContext->MainCypher.DeviceName);
        DIOCBuffer->MainCypherGUID = devContext->MainCypher.CypherGUID;

        DIOCBuffer->DismountPending = devContext->DismountPending;
        DIOCBuffer->ReadOnly = devContext->ReadOnly;

        DIOCBuffer->VolumeFlags = devContext->VolumeFlags;
        DIOCBuffer->SectorIVGenMethod = devContext->SectorIVGenMethod;

        DIOCBuffer->MetaDataLength = devContext->MetaDataLength;

        *pdwActualOut = sizeof(*DIOCBuffer);
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus\n")));
    return retval;
}


// =========================================================================
#ifdef FOTFE_PDA 
BOOL
IOCTL_FOTFE_SetUserDevHandle(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    DIOC_USER_DEVICE_HANDLE* DIOCBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_FOTFE_SetUserDevHandle\n")));

    if (
        (pBufIn != NULL) &&
        (dwLenIn == sizeof(*DIOCBuffer))
       )
        {
        DIOCBuffer = (DIOC_USER_DEVICE_HANDLE*)pBufIn; 

        devContext->UserSpaceDeviceHandle = DIOCBuffer->UserSpaceDeviceHandle;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Set handle to: %0.8x\n"), devContext->UserSpaceDeviceHandle));

        *pdwActualOut = 0;
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_FOTFE_SetUserDevHandle\n")));
    return retval;
}
#endif


// =========================================================================
#ifdef FOTFE_PDA 
BOOL
IOCTL_FOTFE_GetUserDevHandle(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    DIOC_USER_DEVICE_HANDLE* DIOCBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_FOTFE_GetUserDevHandle\n")));

    if (
        (pBufOut != NULL) &&
        (dwLenOut == sizeof(*DIOCBuffer))
       )
        {
        DIOCBuffer = (DIOC_USER_DEVICE_HANDLE*)pBufOut; 

        DIOCBuffer->UserSpaceDeviceHandle = devContext->UserSpaceDeviceHandle;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting handle: %0.8x\n"), devContext->UserSpaceDeviceHandle));
        *pdwActualOut = sizeof(DIOC_USER_DEVICE_HANDLE);
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_FOTFE_GetUserDevHandle\n")));
    return retval;
}
#endif


// =========================================================================
BOOL
IOCTL_FOTFE_SetForceDismount(
    DEVICE_CONTEXT* devContext,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PDWORD pdwActualOut 
)
{
    BOOL retval = FALSE;
    DIOC_FORCE_DISMOUNTS* DIOCBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("IOCTL_FOTFE_SetForceDismount\n")));

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("pBufIn: 0x%0.8x\n"), pBufIn));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("dwLenIn: %d\n"), dwLenIn));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("DIOCBuffer size expected: %d\n"), sizeof(*DIOCBuffer)));

    if (
        (pBufIn != NULL) &&
        (dwLenIn == sizeof(*DIOCBuffer))
       )
        {
        DIOCBuffer = (DIOC_FORCE_DISMOUNTS*)pBufIn; 

        G_contextMgrForceDismounts = DIOCBuffer->ForceDismounts;

        // WE ALWAYS FORCE DISMOUNTS.
        // This is because the device manager/filesystem driver always grabs
        // one open handle on the device
        //xxx - can we improve this? only check if openhandles > 1 instead of 0?
        G_contextMgrForceDismounts = TRUE;

        if (G_contextMgrForceDismounts)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Dismounts WILL BE FORCED.\n")));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Dismounts not forced\n")));
            }

        *pdwActualOut = 0;
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("IOCTL_FOTFE_SetForceDismount\n")));
    return retval;
}


// =========================================================================
BOOL
GetDeviceDetailsHash(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_HASH* HashDetails
)
{
    BOOL retval = TRUE;
    PHashDLLFnGetHashDetails fnGetDetails;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetDeviceDetailsHash\n")));

    // NULL the DeviceName in case it has to be cleared down due to failure
    HashDetails->DeviceName = NULL;

    // Load library...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Loading hash library: %ls\n"), deviceName));
        HashDetails->Lib = LoadLibrary(deviceName);
        if (HashDetails->Lib == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to load library\n")));
            retval = FALSE;
            }
        }

    // Get library function address...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_GETHASHDETAILS));
        fnGetDetails = (PHashDLLFnGetHashDetails)GetProcAddress(
                                        HashDetails->Lib, 
                                        DLLEXPORT_HASH_GETHASHDETAILS
                                       );
        if (fnGetDetails == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_HASH));
        HashDetails->FnHash = (PHashDLLFnHash)GetProcAddress(
                                         HashDetails->Lib,
                                         DLLEXPORT_HASH_HASH
                                        );
        if (HashDetails->FnHash == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        }

    // Get hash details...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting hash details...\n")));
        if (fnGetDetails(supportGUID, &(HashDetails->Details)) != STATUS_SUCCESS)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get hash details\n")));
            retval = FALSE;
            }

        }
    
    // Copy driver name, GUID...
    if (retval)
        {
        // +1 for terminating NULL
        HashDetails->DeviceName = calloc(
                                 (wcslen(deviceName) + 1),
                                 sizeof(deviceName[0])
                                );
        if (HashDetails->DeviceName == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory to copy device name\n")));
            retval = FALSE;
            }
        else
            {
            wcscpy(HashDetails->DeviceName, deviceName);
            }

        HashDetails->HashGUID = *supportGUID;
        }


    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        FreeDeviceDetailsHash(HashDetails);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetDeviceDetailsHash\n")));
    return retval;
}


// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsHash(...)
void
FreeDeviceDetailsHash(
    OUT     MODULE_DETAILS_HASH* HashDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("FreeDeviceDetailsHash\n")));

    SecZeroAndFreeWCHARMemory(HashDetails->DeviceName);
    HashDetails->DeviceName = NULL;

    SecZeroMemory(
                  &(HashDetails->HashGUID),
                  sizeof(HashDetails->HashGUID)
                 ); 

	// IV Hash device handle
    if (HashDetails->Lib != NULL)
        {
        if ((HashDetails->Lib) != NULL)
            {
	        FreeLibrary(HashDetails->Lib);
            HashDetails->Lib = NULL;
            }
        }

	// IV Hash device internal details
    SecZeroMemory(&(HashDetails->Details), sizeof(HashDetails->Details));

    HashDetails->FnHash = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("FreeDeviceDetailsHash\n")));
}


// =========================================================================
// Call GetDeviceDetailsCypher_v3(...), falling back to
// GetDeviceDetailsCypher_v1(...) if that fails
BOOL
GetDeviceDetailsCypher_v3_v1(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    BOOL allOK;
    MODULE_DETAILS_CYPHER_v1 tmpCypherDetails_v1;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetDeviceDetailsCypher_v3_v1\n")));

    allOK = GetDeviceDetailsCypher_v3(deviceName, supportGUID, CypherDetails);
    if (!(allOK))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("GetDeviceDetailsCypher_v3 failed; falling back to GetDeviceDetailsCypher_v1...\n")));
        allOK = GetDeviceDetailsCypher_v1(
                                           deviceName, 
                                           supportGUID,
                                           &tmpCypherDetails_v1
                                          );
        if (allOK)
            { 
            // Repack into v3 structure
            MoveMODULE_DETAILS_CYPHER_v1ToMODULE_DETAILS_CYPHER_v3(
                &tmpCypherDetails_v1,
                CypherDetails
                );
            }

        // DO NOT FREE OFF/CLEARDOWN tmpCypherDetails_v1!
        // Its content is being used by tmpCypherDetails_v1
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetDeviceDetailsCypher_v3_v1\n")));

    return allOK;
}

// =========================================================================
BOOL
GetDeviceDetailsCypher_v1(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
)
{
    BOOL retval = TRUE;
    PCypherDLLFnGetCypherDetails_v1 fnGetDetails;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetDeviceDetailsCypher_v1\n")));

    // NULL the DeviceName in case it has to be cleared down due to failure
    CypherDetails->DeviceName = NULL;

    // Load library...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Loading cypher library: %ls\n"), deviceName));
        CypherDetails->Lib = LoadLibrary(deviceName);
        if (CypherDetails->Lib == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to load library\n")));
            retval = FALSE;
            }
        }

    // Get library function address...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1));
        fnGetDetails = (PCypherDLLFnGetCypherDetails_v1)GetProcAddress(
                                        CypherDetails->Lib, 
                                        DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1
                                       );
        if (fnGetDetails == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPTWITHASCII));
        CypherDetails->FnEncrypt = (PCypherDLLFnEncryptWithASCII)GetProcAddress(
                                         CypherDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPTWITHASCII
                                        );
        if (CypherDetails->FnEncrypt == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPTWITHASCII));
        CypherDetails->FnDecrypt = (PCypherDLLFnDecryptWithASCII)GetProcAddress(
                                         CypherDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPTWITHASCII
                                        );
        if (CypherDetails->FnDecrypt == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        }

    // Get cypher details...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting cypher details...\n")));
        if (fnGetDetails(supportGUID, &(CypherDetails->Details)) != STATUS_SUCCESS)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get cypher details\n")));
            retval = FALSE;
            }
        }
    
    // Copy driver name, GUID...
    if (retval)
        {
        // +1 for terminating NULL
        CypherDetails->DeviceName = calloc(
                                 (wcslen(deviceName) + 1),
                                 sizeof(deviceName[0])
                                );
        if (CypherDetails->DeviceName == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory to copy device name\n")));
            retval = FALSE;
            }
        else
            {
            wcscpy(CypherDetails->DeviceName, deviceName);
            }

        CypherDetails->CypherGUID = *supportGUID;
        }


    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        FreeDeviceDetailsCypher_v1(CypherDetails);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetDeviceDetailsCypher_v1\n")));
    return retval;
}

// =========================================================================
BOOL
GetDeviceDetailsCypher_v3(
    IN      WCHAR* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    BOOL retval = TRUE;
    MODULE_DETAILS_CYPHER_v1 cypherDetails_v1;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetDeviceDetailsCypher_v3\n")));


    // NULL the DeviceName in case it has to be cleared down due to failure
    CypherDetails->DeviceName = NULL;

    // First get those members common to the v1 API...
    retval = GetDeviceDetailsCypher_v1(
                                    deviceName,
                                    supportGUID,
                                    &cypherDetails_v1
                                   );

    // Load library...
    if (retval)
        {
        MoveMODULE_DETAILS_CYPHER_v1ToMODULE_DETAILS_CYPHER_v3(
            &cypherDetails_v1,
            CypherDetails
            );

        // Get v3 API library function address...
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII));
        CypherDetails->FnEncryptSector = (PCypherDLLFnEncryptSectorWithASCII)GetProcAddress(
                                         CypherDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII
                                        );
        if (CypherDetails->FnEncryptSector == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII));
        CypherDetails->FnDecryptSector = (PCypherDLLFnDecryptSectorWithASCII)GetProcAddress(
                                         CypherDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII
                                        );
        if (CypherDetails->FnDecryptSector == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        // DO NOT FREE OFF/CLEARDOWN tmpCypherDetails_v1!
        // Its content is being used by tmpCypherDetails_v1
        }

    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        FreeDeviceDetailsCypher_v3(CypherDetails);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetDeviceDetailsCypher_v1\n")));
    return retval;
}


// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v1(
    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("FreeDeviceDetailsCypher_v1\n")));

    SecZeroAndFreeWCHARMemory(CypherDetails->DeviceName);
    CypherDetails->DeviceName = NULL;

    SecZeroMemory(
                  &(CypherDetails->CypherGUID),
                  sizeof(CypherDetails->CypherGUID)
                 ); 

	// IV Hash device handle
    if (CypherDetails->Lib != NULL)
        {
        if ((CypherDetails->Lib) != NULL)
            {
	        FreeLibrary(CypherDetails->Lib);
            CypherDetails->Lib = NULL;
            }
        }
	
	// IV Hash device internal details
    SecZeroMemory(&(CypherDetails->Details), sizeof(CypherDetails->Details));

    CypherDetails->FnEncrypt = NULL;
    CypherDetails->FnDecrypt = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("FreeDeviceDetailsCypher_v1\n")));
}

// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v3(
    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("FreeDeviceDetailsCypher_v3\n")));

    SecZeroAndFreeWCHARMemory(CypherDetails->DeviceName);
    CypherDetails->DeviceName = NULL;

    SecZeroMemory(
                  &(CypherDetails->CypherGUID),
                  sizeof(CypherDetails->CypherGUID)
                 ); 

	// IV Hash device handle
    if (CypherDetails->Lib != NULL)
        {
        if ((CypherDetails->Lib) != NULL)
            {
	        FreeLibrary(CypherDetails->Lib);
            CypherDetails->Lib = NULL;
            }
        }
	
	// IV Hash device internal details
    SecZeroMemory(&(CypherDetails->Details), sizeof(CypherDetails->Details));

    CypherDetails->FnEncrypt       = NULL;
    CypherDetails->FnDecrypt       = NULL;
    CypherDetails->FnDecryptSector = NULL;
    CypherDetails->FnEncryptSector = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("FreeDeviceDetailsCypher_v3\n")));
}


// =========================================================================
DWORD
MainDeriveKey(
    IN  DWORD SizeBufferIn,
    IN  DIOC_DERIVE_KEY_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_DERIVE_KEY_OUT*  DIOCBufferOut
)
{
    DWORD status;
    FREEOTFEBYTE* tmpOutput;
    int tmpLengthBits;  // In *bits*
    int userBufferSizeBytes;  // In *bytes*
    unsigned int useBits;  // In *bits*
    int lengthWanted;  // In *bits*
    unsigned int i;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("MainDeriveKey\n")));

    status = STATUS_SUCCESS;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (SizeBufferIn <
            (
             sizeof(*DIOCBufferIn) 
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("inBuffer size wrong size (expect min: %d; got: %d)\n"),
            (
             sizeof(*DIOCBufferIn)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
            ),
            SizeBufferIn
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    // Actual check...
    if (SizeBufferIn <
            (
             sizeof(*DIOCBufferIn)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
              + (DIOCBufferIn->PasswordLength / 8)
              + (DIOCBufferIn->SaltLength / 8)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("inBuffer size wrong size (expect actual: %d; got: %d)\n"),
            (
             sizeof(*DIOCBufferIn)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
              + (DIOCBufferIn->PasswordLength / 8)
              + (DIOCBufferIn->SaltLength / 8)
            ),
            SizeBufferIn
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (SizeBufferOut <
            sizeof(*DIOCBufferOut)-sizeof(DIOCBufferOut->DerivedKey))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect min: %d; got: %d)\n"),
            sizeof(*DIOCBufferOut)-sizeof(DIOCBufferOut->DerivedKey),
            SizeBufferOut
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        

    // Request valid so far, process...    


    // We have to allocate a buffer to hold the output, before it can be copied
    // out to the output buffer; otherwise, the input buffer gets garbled as
    // data is written to the output buffer

    // Match the user's buffer; if it's too small, the operation will kick
    // it out
    // This calculates the size of the variable part of the struct
    // Yes, this should be correct: The total size of the user's output buffer,
    // less the size of the struct - but then plus the size of
    // the variable parts reserved within the buffer
    userBufferSizeBytes = SizeBufferOut - 
                          sizeof(*DIOCBufferOut) +
                          sizeof(DIOCBufferOut->DerivedKey);
    tmpOutput = FREEOTFE_MEMALLOC(userBufferSizeBytes); 

    // The size of the buffer in bits; the algorithm will read this value, then
    // overwrite it with the actual size of the output (in bits)
    tmpLengthBits = (userBufferSizeBytes * 8);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Allocated output buffer for: %d bits\n"), tmpLengthBits));
    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to process...\n")));


    // This call runs the correct algorithm and generates the output
    if (DIOCBufferIn->LengthWanted == 0)
        {
        // Optimisation - if the user wants a zero length output, don't
        // bother processing, just return zero length output.
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Zero length output requested; returning nothing\n")));
        tmpLengthBits = 0;
        status = STATUS_SUCCESS;
        }
    else
        {
        status = DeriveKey(
                    DIOCBufferIn->KDFAlgorithm,
                    DIOCBufferIn->HashDeviceName,
                    DIOCBufferIn->HashGUID,
                    DIOCBufferIn->CypherDeviceName,
                    DIOCBufferIn->CypherGUID,
                    DIOCBufferIn->Iterations,
                    DIOCBufferIn->LengthWanted,
                    DIOCBufferIn->PasswordLength,  // In bits
                    (FREEOTFEBYTE*)&DIOCBufferIn->Password,  // Variable length
                    DIOCBufferIn->SaltLength,  // In bits
                    // This may seem a little weird, but is required as the
                    // position of the salt within the struct is variable
                    &DIOCBufferIn->Password[(DIOCBufferIn->PasswordLength / 8)],  // Variable length

                    &tmpLengthBits,  // In bits
                    (FREEOTFEBYTE*)tmpOutput  // Variable length
                    );
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("DK Done with status: %d (want status: %d)\n"), status, STATUS_SUCCESS));
    if (NT_SUCCESS(status)) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("tmp buffer was: %d bits, output was %d bits\n"), (userBufferSizeBytes * 8), tmpLengthBits));


        // Preserve input buffer value, to ensure we don't overwrite it with
        // the output buffer
        lengthWanted = DIOCBufferIn->LengthWanted;


        // Actual check...

        // Ensure output DIOC butter is large enough to store the output
        // Note that we can't carry out this check until we've created the
        // output value, in case the algorithm used produces variable length
        // output

        // If the user specified an output length, we only use those bits
        if (lengthWanted >= 0) 
            {
            if (SizeBufferOut <
                    sizeof(*DIOCBufferOut)+(lengthWanted/8)-sizeof(DIOCBufferOut->DerivedKey))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect actual: %d; got: %d)\n"),
                    sizeof(*DIOCBufferOut)+(lengthWanted/8)-sizeof(DIOCBufferOut->DerivedKey),
                    SizeBufferOut
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                // If the algorithm doens't generate enough bits, we right-pad
                // with NULLs
                if (lengthWanted > tmpLengthBits)
                    {
                    // Cleardown the buffer beyond the output
                    FREEOTFE_MEMZERO(
                                &DIOCBufferOut->DerivedKey,
                                (lengthWanted / 8)
                                );

                    useBits = tmpLengthBits;
                    }
                else
                    {
                    useBits = lengthWanted;
                    }
                FREEOTFE_MEMCPY(&DIOCBufferOut->DerivedKey, tmpOutput, (useBits / 8));
                DIOCBufferOut->DerivedKeyLength = lengthWanted;
                }
            }
        else
            // User didn't specify an output length - we use the entire
            // algorithm's output
            {
            if (SizeBufferOut <
                    sizeof(*DIOCBufferOut)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->DerivedKey))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect actual: %d; got: %d)\n"),
                    sizeof(*DIOCBufferOut)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->DerivedKey),
                    SizeBufferOut
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                FREEOTFE_MEMCPY(&DIOCBufferOut->DerivedKey, tmpOutput, (tmpLengthBits / 8));
                DIOCBufferOut->DerivedKeyLength = tmpLengthBits;
                }
            }
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED")));
        }

    SecZeroMemory(tmpOutput, userBufferSizeBytes);
    FREEOTFE_FREE(tmpOutput);


    if (NT_SUCCESS(status)) 
        {
        // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- DerivedKey length: %d bits\n"), DIOCBufferOut->DerivedKeyLength));
        for(i = 0; i < (DIOCBufferOut->DerivedKeyLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- DerivedKey [%.2d]: %.2x\n"), i, DIOCBufferOut->DerivedKey[i]));
            }                                
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("MainDeriveKey\n")));
    return status;
}


// =========================================================================
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
    IN      FREEOTFEBYTE* Password,
    IN      unsigned int SaltLength,  // In bits
    IN      FREEOTFEBYTE* Salt,

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     FREEOTFEBYTE* DerivedKey
)
{
    NTSTATUS status;

    BOOLEAN HashFlag;
    MODULE_DETAILS_HASH Hash;

    BOOLEAN CypherFlag;
    MODULE_DETAILS_CYPHER_v3 Cypher;

    BOOLEAN gotHashDetails;
    BOOLEAN gotCypherDetails;

    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DeriveKey\n")));


    status = STATUS_SUCCESS;

    Hash.FnHash    = NULL;
    Cypher.FnEncrypt       = NULL;
    Cypher.FnDecrypt       = NULL;
    Cypher.FnEncryptSector = NULL;
    Cypher.FnDecryptSector = NULL;

    gotHashDetails = FALSE;
    gotCypherDetails = FALSE;

    // Flags to indicate if we were passed hash/cypher details
    HashFlag   = (wcslen(HashDeviceName) > 0);
    CypherFlag = (wcslen(CypherDeviceName) > 0);


    // Locate the appropriate HASH algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (HashFlag)
            {
            gotHashDetails = GetDeviceDetailsHash(
                                HashDeviceName,
                                &HashGUID,

                                &Hash
	                            );
            if (!(gotHashDetails))
                {
                status = STATUS_INVALID_PARAMETER;
                }
            }
        }

    // Locate the appropriate CYPHER algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (CypherFlag)
            {
            gotCypherDetails = GetDeviceDetailsCypher_v3_v1(
                                CypherDeviceName,
                                &CypherGUID,

                                &Cypher
                                );
            if (!(gotCypherDetails))
                {
                status = STATUS_INVALID_PARAMETER;
                }
            }
        }


    // Sanity checking; if a hash is involved, and it has an undefined or zero
    // blocksize or output length; we only proceed if the algorithm is KDF_HASH
    // This is because other KDF implementations (e.g. PBKDF2) aren't designed
    // for use with such hashes
    if (NT_SUCCESS(status))
        {
        if (
            // If it's not a KDF_HASH...
            (KDFAlgorithm != KDF_HASH) &&
            // ...and we have a hash function involved...
            HashFlag &&
            // ..with an undefined blocksize/output length
            (
             (Hash.Details.BlockSize <= 0) ||
             (Hash.Details.Length <= 0)
            )
           )
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("KDFs can only be called with algorithms that have a defined block & output length, unless hash KDF being used\n")));
            status = STATUS_INVALID_PARAMETER;
            }
        }


    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Processing with algorithm: %d\n"), KDFAlgorithm));
        if (KDFAlgorithm == KDF_HASH)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Must supply a hash when using KDF_HASH\n")));
                }
            else
                {
                status = ImplKDFHashSaltedPassword(
                                    Hash.FnHash,
                                    Hash.HashGUID,
                                    Iterations,
                                    PasswordLength,  // In bits
                                    Password,
                                    SaltLength,  // In bits
                                    Salt,

                                    DerivedKeyLength,  // In bits
                                    DerivedKey
                                );
                }
            }
        else if (KDFAlgorithm == KDF_PBKDF2)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Must supply a hash when using KDF_PBKDF2\n")));
                }
            else
                {
                status = ImplKDFPBKDF2(
                                    Hash.FnHash,
                                    Hash.HashGUID,
                                    Hash.Details,

                                    PasswordLength,  // In bits
                                    Password,
                                    SaltLength,  // In bits
                                    Salt,
                                    Iterations,
                                    LengthWanted,  // In bits

                                    DerivedKeyLength,  // In bits
                                    DerivedKey
                                );
                }
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unrecognised KDF algorithm!\n")));
            }

        }


    if (gotCypherDetails)
        {
        FreeDeviceDetailsCypher_v3(&Cypher);
        }

    if (gotHashDetails)
        {
        FreeDeviceDetailsHash(&Hash);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DeriveKey\n")));

    return status;
}


// =========================================================================
DWORD
MainMACData(
    IN  DWORD SizeBufferIn,
    IN  DIOC_GENERATE_MAC_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_GENERATE_MAC_OUT*  DIOCBufferOut
)
{
    DWORD status;
    FREEOTFEBYTE* tmpOutput;
    int tmpLengthBits;  // In *bits*
    int userBufferSizeBytes;  // In *bytes*
    unsigned int useBits;  // In *bits*
    int lengthWanted;  // In *bits*
    unsigned int i;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("MainMACData\n")));

    status = STATUS_SUCCESS;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (SizeBufferIn <
            (
             sizeof(*DIOCBufferIn) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("inBuffer size wrong size (expect min: %d; got: %d)\n"),
            (
             sizeof(*DIOCBufferIn) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
            ),
            SizeBufferIn
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    // Actual check...
    if (SizeBufferIn <
            (
             sizeof(*DIOCBufferIn) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)
              + (DIOCBufferIn->DataLength / 8)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("inBuffer size wrong size (expect actual: %d; got: %d)\n"),
            (
             sizeof(*DIOCBufferIn)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)
              + (DIOCBufferIn->DataLength / 8)
            ),
            SizeBufferIn
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (SizeBufferOut <
            sizeof(*DIOCBufferOut)-sizeof(DIOCBufferOut->MAC))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect min: %d; got: %d)\n"),
            sizeof(*DIOCBufferOut)-sizeof(DIOCBufferOut->MAC),
            SizeBufferOut
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        

    // Request valid so far, process...    


    // We have to allocate a buffer to hold the output, before it can be copied
    // out to the output buffer; otherwise, the input buffer gets garbled as
    // data is written to the output buffer

    // Match the user's buffer; if it's too small, the operation will kick
    // it out
    // This calculates the size of the variable part of the struct
    // Yes, this should be correct: The total size of the user's output buffer,
    // less the size of the struct - but then plus the size of
    // the variable parts reserved within the buffer
    userBufferSizeBytes = SizeBufferOut - 
                          sizeof(*DIOCBufferOut) +
                          sizeof(DIOCBufferOut->MAC);
    tmpOutput = FREEOTFE_MEMALLOC(userBufferSizeBytes); 

    // The size of the buffer in bits; the algorithm will read this value, then
    // overwrite it with the actual size of the output (in bits)
    tmpLengthBits = (userBufferSizeBytes * 8);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Allocated output buffer for: %d bits\n"), tmpLengthBits));
    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to process...\n")));


    // This call runs the correct algorithm and generates the output
    if (DIOCBufferIn->LengthWanted == 0)
        {
        // Optimisation - if the user wants a zero length output, don't
        // bother processing, just return zero length output.
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Zero length output requested; returning nothing\n")));
        tmpLengthBits = 0;
        status = STATUS_SUCCESS;
        }
    else
        {
        status = GenerateMAC(
                    DIOCBufferIn->MACAlgorithm,
                    (WCHAR*)DIOCBufferIn->HashDeviceName,
                    DIOCBufferIn->HashGUID,
                    (WCHAR*)DIOCBufferIn->CypherDeviceName,
                    DIOCBufferIn->CypherGUID,
                    DIOCBufferIn->LengthWanted,
                    DIOCBufferIn->KeyLength,  // In bits
                    (FREEOTFEBYTE*)&DIOCBufferIn->Key,  // Variable length
                    DIOCBufferIn->DataLength,  // In bits
                    // This may seem a little weird, but is required as the
                    // position of the data within the struct is variable
                    &DIOCBufferIn->Key[(DIOCBufferIn->KeyLength / 8)],  // Variable length

                    &tmpLengthBits,  // In bits
                    (FREEOTFEBYTE*)tmpOutput  // Variable length
                    );
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("MAC Done with status: %d (want status: %d)\n"), status, STATUS_SUCCESS));
    if (NT_SUCCESS(status)) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("tmp buffer was: %d bits, output was %d bits\n"), (userBufferSizeBytes * 8), tmpLengthBits));


        // Preserve input buffer value, to ensure we don't overwrite it with
        // the output buffer
        lengthWanted = DIOCBufferIn->LengthWanted;


        // Actual check...

        // Ensure output DIOC butter is large enough to store the output
        // Note that we can't carry out this check until we've created the
        // output value, in case the algorithm used produces variable length
        // output

        // If the user specified an output length, we only use those bits
        if (lengthWanted >= 0) 
            {
            if (SizeBufferOut <
                    sizeof(*DIOCBufferOut)+(lengthWanted/8)-sizeof(DIOCBufferOut->MAC))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT(">=0 outBuffer size wrong size (expect actual: %d; got: %d)\n"),
                    sizeof(*DIOCBufferOut)+(lengthWanted/8)-sizeof(DIOCBufferOut->MAC),
                    SizeBufferOut
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                // If the algorithm doens't generate enough bits, we right-pad
                // with NULLs
                if (lengthWanted > tmpLengthBits)
                    {
                    // Cleardown the buffer beyond the output
                    FREEOTFE_MEMZERO(
                                &DIOCBufferOut->MAC,
                                (lengthWanted / 8)
                                );

                    useBits = tmpLengthBits;
                    }
                else
                    {
                    useBits = lengthWanted;
                    }
                FREEOTFE_MEMCPY(&DIOCBufferOut->MAC, tmpOutput, (useBits / 8));
                DIOCBufferOut->MACLength = lengthWanted;
                }
            }
        else
            // User didn't specify an output length - we use the entire
            // algorithm's output
            {
            if (SizeBufferOut <
                    sizeof(*DIOCBufferOut)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->MAC))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("<0 outBuffer size wrong size (expect actual: %d; got: %d)\n"),
                    sizeof(*DIOCBufferOut)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->MAC),
                    SizeBufferOut
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                FREEOTFE_MEMCPY(&DIOCBufferOut->MAC, tmpOutput, (tmpLengthBits / 8));
                DIOCBufferOut->MACLength = tmpLengthBits;
                }
            }

        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED")));
        }

    SecZeroMemory(tmpOutput, userBufferSizeBytes);
    FREEOTFE_FREE(tmpOutput);


    if (NT_SUCCESS(status)) 
        {
        // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- MAC length: %d bits\n"), DIOCBufferOut->MACLength));
        for(i = 0; i < (DIOCBufferOut->MACLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("----- MAC [%.2d]: %.2x\n"), i, DIOCBufferOut->MAC[i]));
            }                                
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("MainMACData\n")));
    return status;
}


// =========================================================================
NTSTATUS
GenerateMAC(
    IN      MAC_ALGORITHM MACAlgorithm,
    IN      WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID HashGUID,
    IN      WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID CypherGUID,
    IN      int LengthWanted,  // In bits
    IN      unsigned int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      unsigned int DataLength,  // In bits
    IN      FREEOTFEBYTE* Data,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     FREEOTFEBYTE* MAC
)
{
    NTSTATUS status;

    BOOLEAN HashFlag;
    MODULE_DETAILS_HASH Hash;

    BOOLEAN CypherFlag;
    MODULE_DETAILS_CYPHER_v3 Cypher;

    BOOLEAN gotHashDetails;
    BOOLEAN gotCypherDetails;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GenerateMAC\n")));


    status = STATUS_SUCCESS;

    Hash.FnHash    = NULL;
    Cypher.FnEncrypt       = NULL;
    Cypher.FnDecrypt       = NULL;
    Cypher.FnEncryptSector = NULL;
    Cypher.FnDecryptSector = NULL;

    // Flags to indicate if we were passed hash/cypher details
    HashFlag   = (wcslen(HashDeviceName) > 0);
    CypherFlag = (wcslen(CypherDeviceName) > 0);

    gotHashDetails = FALSE;
    gotCypherDetails = FALSE;

    // Locate the appropriate HASH algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (HashFlag)
            {
            gotHashDetails = GetDeviceDetailsHash(
                                HashDeviceName,
                                &HashGUID,

                                &Hash
                                );
            if (!(gotHashDetails))
                {
                status = STATUS_INVALID_PARAMETER;
                }
            }
        }

    // Locate the appropriate CYPHER algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (CypherFlag)
            {
            gotCypherDetails = GetDeviceDetailsCypher_v3_v1(
                                CypherDeviceName,
                                &CypherGUID,

                                &Cypher
                                );
            if (!(gotCypherDetails))
                {
                status = STATUS_INVALID_PARAMETER;
                }
            }
        }


    // Sanity checking; if a hash is involved, and it has an undefined or zero
    // blocksize or output length; we only proceed if the algorithm is MAC_HASH
    // This is because other MAC implementations (e.g. HMAC) aren't designed
    // for use with such hashes
    if (
        // If it's not a MAC_HASH...
        (MACAlgorithm != MAC_HASH) &&
        // ...and we have a hash function involved...
        (Hash.FnHash != NULL) &&
        // ..with an undefined blocksize/output length
        (
         (Hash.Details.BlockSize <= 0) ||
         (Hash.Details.Length <= 0)
        )
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("MACs can only be generated with algorithms that have a defined block & output length\n")));
        status = STATUS_INVALID_PARAMETER;
        }


    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Processing with algorithm: %d\n"), MACAlgorithm));
        if (MACAlgorithm == MAC_HASH)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Must supply a hash when using MAC_HASH\n")));
                }
            else
                {
                status = ImplMACHash(
                                    Hash.FnHash,
                                    Hash.HashGUID,
                                    DataLength,  // In bits
                                    Data,

                                    MACLength,  // In bits
                                    MAC
                                );
                }
            }
        else if (MACAlgorithm == MAC_HMAC)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Must supply a hash when using MAC_HMAC\n")));
                }
            else
                {
                status = ImplMACHMAC(
                                    Hash.FnHash,
                                    Hash.HashGUID,
                                    Hash.Details,
                                    LengthWanted,
                                    KeyLength,  // In bits
                                    Key,
                                    DataLength,  // In bits
                                    Data,

                                    MACLength,  // In bits
                                    MAC
                                );
                }
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unrecognised MAC algorithm!\n")));
            }

        }


    if (gotCypherDetails)
        {
        FreeDeviceDetailsCypher_v3(&Cypher);
        }

    if (gotHashDetails)
        {
        FreeDeviceDetailsHash(&Hash);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GenerateMAC\n")));

    return status;
}


// =========================================================================
// =========================================================================

