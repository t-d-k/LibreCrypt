// Description: FreeOTFE Device Driver
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


// #defines for debug purposes:
//
// #define DBG_CREATEINITDISKS 4
//   - If defined, then when the driver starts, it will automatically create the specified
//     number of disk devices
//   - Should all be COMMENTED OUT for release





#include <ntddk.h>
#include <stdio.h>

// Required for std disk device driver I/O control (IOCTL) codes
#include <ntdddisk.h>
// Required for std CDROM IOCTLs and structs (e.g. IOCTL_CDROM_READ_TOC)
#include <ntddcdrm.h>

#include <ntverp.h>  // Needed for VER_PRODUCTBUILD

#include "IFSRelated.h"

#include "FreeOTFEDebug.h"
#include "FreeOTFENULLGUID.h"
#include "FreeOTFEDriverlib.h"
#include "FreeOTFEDriverConstsCommon.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGenerateBlockIV.h"
#include "FreeOTFECallModuleFn.h"
#include "FreeOTFEPlatform.h"

#include "FreeOTFE.h"
#include "FreeOTFEAPI.h"

#include "FreeOTFEHashAPI.h"
#include "FreeOTFECypherAPI.h"

// MAC implementations
#include "FreeOTFEMACHash.h"
#include "FreeOTFEMACHMAC.h"

// KDF implementations
#include "FreeOTFEKDFHashSaltedPassword.h"
#include "FreeOTFEKDFPBKDF2.h"

// Globals.
// Not nice, but...
HANDLE DirFreeOTFERoot  = NULL;
HANDLE DirFreeOTFEDisks = NULL;


// =========================================================================
// This routine is the driver's entry point, called by the I/O system
// to load the driver.  The driver's entry points are initialized.
// In DBG mode, this routine also examines the registry for special
// debug parameters.
NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    )
{
    NTSTATUS status = STATUS_SUCCESS;
    UNICODE_STRING devDirName;
    OBJECT_ATTRIBUTES dirObjAttribs;
    PDEVICE_OBJECT mainDevObj;
    PDEVICE_EXTENSION mainDevExt;
#ifdef DBG_CREATEINITDISKS
    int i;   
    PDEVICE_OBJECT dbgTmpDevObj;
#endif
    
    // The following debug handling was ripped from the MS DDK "floppy.c"
    // example
#if DBG
    // We use this to query into the registry as to whether we
    // should break at driver entry.
    RTL_QUERY_REGISTRY_TABLE paramTable[3];
    ULONG default_ShouldBreak = 0;           // Default to 0; no break
//    ULONG default_DebugLevel  = 0xFFFFFFFF;  // Default to all on
    ULONG default_DebugLevel  = 0x0000001F;  // All except verbose debug
    ULONG debugLevel = 0;
    ULONG shouldBreak = 0;
    PWCHAR path;
    ULONG pathLength;
    
    // Since the registry path parameter is a "counted" UNICODE string, it
    // might not be zero terminated.  For a very short time allocate memory
    // to hold the registry path zero terminated so that we can use it to
    // delve into the registry.
    //
    // NOTE NOTE!!!! This is not an architected way of breaking into
    // a driver.  It happens to work for this driver because the original
    // DDK example code for DriverEntry happened to be written in this manner.
    pathLength = RegistryPath->Length + sizeof(WCHAR);

    if (path = FREEOTFE_MEMALLOC(pathLength)) {

        SecZeroMemory(&paramTable[0], sizeof(paramTable));
        SecZeroMemory(path, pathLength);
        RtlMoveMemory(path, RegistryPath->Buffer, RegistryPath->Length);

        paramTable[0].Flags         = RTL_QUERY_REGISTRY_DIRECT;
        paramTable[0].Name          = L"BreakOnEntry";
        paramTable[0].EntryContext  = &shouldBreak;
        paramTable[0].DefaultType   = REG_DWORD;
        paramTable[0].DefaultData   = &default_ShouldBreak;
        paramTable[0].DefaultLength = sizeof(ULONG);

        paramTable[1].Flags         = RTL_QUERY_REGISTRY_DIRECT;
        paramTable[1].Name          = L"DebugLevel";
        paramTable[1].EntryContext  = &debugLevel;
        paramTable[1].DefaultType   = REG_DWORD;
        paramTable[1].DefaultData   = &default_DebugLevel;
        paramTable[1].DefaultLength = sizeof(ULONG);

        if (!(NT_SUCCESS(RtlQueryRegistryValues(
                            RTL_REGISTRY_ABSOLUTE | RTL_REGISTRY_OPTIONAL,
                            path,
                            &paramTable[0],
                            NULL,
                            NULL)))) {

            shouldBreak = default_ShouldBreak;
            debugLevel = default_DebugLevel;
        }

        FREEOTFE_FREE(path);
    }

#if DBG
    FreeOTFEDebugLevel = debugLevel;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Debug level   : %d\n", FreeOTFEDebugLevel));
#endif
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Driver version: 0x%08x (v%02d.%02d.%04d)\n",
                                     DRIVER_VERSION, 
                                     (DRIVER_VERSION & 0xFF000000) / 0x00FF0000,
                                     (DRIVER_VERSION & 0x00FF0000) / 0x0000FF00,
                                     (DRIVER_VERSION & 0x0000FFFF)
                                    ));
    
    if (shouldBreak == 1)
        {
        DbgBreakPoint();
        }
#endif

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("DriverEntry\n"));
   
    // Create main device dir
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Creating dir object (main)...\n"));
    status = CreateDeviceDir(
                DEVICE_FREEOTFE_ROOT,
                &DirFreeOTFERoot
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (main)\n"));
        return status;
        }

    // Create disk device dir
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Creating dir object (disks)...\n"));
    status = CreateDeviceDir(
                DEVICE_DISK_DIR_NAME,
                &DirFreeOTFEDisks
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (disks)\n"));
        ZwClose(DirFreeOTFERoot);
        return status;
        }

    // Create main device
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Creating main device...\n"));
    status = CreateMainDevice(DriverObject, &mainDevObj);
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Call to CreateMainDevice FAILED.\n"));
        return status;
        }

    // Store the device dir handle for closure on unload
    mainDevExt = (PDEVICE_EXTENSION)mainDevObj->DeviceExtension;

#ifdef DBG_CREATEINITDISKS
    // Create initial disk devices
    for (i=1; i<=DBG_CREATEINITDISKS; i++)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Creating disk #%d device...\n", i));
        status = CreateDiskDevice(DriverObject, &dbgTmpDevObj);
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Call to CreateDiskDevice FAILED.\n"));
            return status;
            }
        }
#endif    

    // Initialize the driver object with this driver's entry points.
    DriverObject->MajorFunction[IRP_MJ_CREATE] = 
                                            FreeOTFE_MF_DispatchCreate;
                                            
    DriverObject->MajorFunction[IRP_MJ_CLOSE] =
                                            FreeOTFE_MF_DispatchClose;
                                            
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] =
                                            FreeOTFE_MF_DispatchDeviceControl;
                                            
    DriverObject->MajorFunction[IRP_MJ_READ] =
                                            FreeOTFE_MF_DispatchRead;
                                            
    DriverObject->MajorFunction[IRP_MJ_WRITE] =
                                            FreeOTFE_MF_DispatchWrite;
    
    DriverObject->MajorFunction[IRP_MJ_FLUSH_BUFFERS] = 
                                            FreeOTFE_MF_DispatchFlushBuffers;
    
// xxxop - prevent hibernate, standby, etc if drives are mounted
// xxxop - will finish implementing this once my testbox is configured to handle
//         hibernate & standby
//    DriverObject->MajorFunction[IRP_MJ_POWER] =
//                                            FreeOTFE_MF_DispatchPower;
                                            
    DriverObject->MajorFunction[IRP_MJ_SYSTEM_CONTROL] =
                                            FreeOTFE_MF_DispatchSystemControl;
 
    DriverObject->DriverUnload = DriverUnload;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("DriverEntry\n"));    

    return status;
}


// =========================================================================
// Unload the driver from the system
VOID
DriverUnload(
    IN PDRIVER_OBJECT DriverObject
)
{
    PDEVICE_EXTENSION devExt;
    PDEVICE_OBJECT devObj;
    NTSTATUS status;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("DriverUnload\n"));
    
    // TODOzzz: Dismount all first!!!???
    
    // Walk through all the device objects, shutting them down and deleting
    // them
    devObj = DriverObject->DeviceObject;
    while (devObj != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Found a device to destroy; destroying it...\n"));
        devObj = DestroyDevice(devObj);
        }

    status = ZwClose(DirFreeOTFERoot);
    status = ZwClose(DirFreeOTFEDisks);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("DriverUnload\n"));

    return;
}


// =========================================================================
// Overwrite sensitive parts of the specified device's extension
// Given a device object, cleardown, free off and overwrite all sensitive
// parts of it's device extension which relate to DISK DEVICES
// (e.g. encryption/decryption keys, etc)
// !!! WARNING !!!
// This must only ever be called on a disk device which has been UNMOUNTED,
// with nothing remaining to be processed on it's thread's IRP queue
// Returns: Status
NTSTATUS
OverwriteDeviceSensitive(
  IN  PDEVICE_OBJECT devObj
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PDEVICE_EXTENSION devExt;
    NTSTATUS tmpStatus;
    IO_STATUS_BLOCK ioStatusBlock;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("OverwriteDeviceSensitive\n"));

    devExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;


    devExt->Mounted = FALSE;
    devExt->DismountPending = FALSE;
    

    // Tear down any filename...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("(Filename)...\n"));  
    if (devExt->zzFilename.Buffer != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off filename unicode buffer...\n")); 
        SecZeroMemory(
                        devExt->zzFilename.Buffer,
                        sizeof(devExt->zzFilename.MaximumLength)
                       );
        FREEOTFE_FREE(devExt->zzFilename.Buffer);
        devExt->zzFilename.Buffer = NULL;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }

    // File object pointer
    if (devExt->FileObject != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Dereferencing file object...\n")); 
        ObDereferenceObject(devExt->FileObject);
        devExt->FileObject = NULL;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));      
        }

    // Closedown any open
    // Return value ignored
    FileClose(
              &devExt->FileHandle,
              devExt->FileAttributesStored,
              &devExt->FileAttributes,
              &ioStatusBlock
             );

    // Timestamps/file attributes
    devExt->FileAttributesStored = FALSE;
    SecZeroMemory(
                  &devExt->FileAttributes,
                  sizeof(devExt->FileAttributes)
                 );

    // Information required for impersonation
    // Return value ignored
    ClientSecurityDestroy(&devExt->ClientContext);

    // Offset
    devExt->DataOffset.QuadPart = 0;

    // Encrypted block size
    devExt->EncryptionBlockSize = 0;

    // Simulated disk geometry
    devExt->HiddenSectors = 0;
    SecZeroMemory(
                  &devExt->DiskGeometry,
                  sizeof(devExt->DiskGeometry)
                 );
    SecZeroMemory(
                  &devExt->DiskSize,
                  sizeof(devExt->DiskSize)
                 );
    SecZeroMemory(
                  &devExt->PartitionSize,
                  sizeof(devExt->PartitionSize)
                 );

    // Readonly flag
    devExt->ReadOnly = TRUE;
    
    // Mount source (e.g. the volume file is a partition or file)
    devExt->MountSource = MNTSRC_UNKNOWN;

    // Storage media type (i.e. the type of device emulated)
    devExt->StorageMediaType = Unknown;

    // Prevent media removal; for removable disks only
    devExt->PreventMediaRemoval = FALSE;       


    // Tear down any IV hash related...
    FreeDeviceDetailsHash(&devExt->IVHash);

    // Tear down any IV cypher related...
    FreeDeviceDetailsCypher_v3(&devExt->IVCypher);

    // Tear down any main cypher related...
    FreeDeviceDetailsCypher_v3(&devExt->MainCypher);

    // ...any key
    if (devExt->MasterKey != NULL)
        {
        SecZeroMemory(
                    devExt->MasterKey,
                    (devExt->MasterKeyLength / 8)
                    );
        FREEOTFE_FREE(devExt->MasterKey);
        devExt->MasterKey = NULL;
        }
    if (devExt->MasterKeyASCII != NULL)
        {
        SecZeroMemory(
                    devExt->MasterKeyASCII,
                    ((devExt->MasterKeyLength / 8) * 2)
                    );
        FREEOTFE_FREE(devExt->MasterKeyASCII);
        devExt->MasterKeyASCII = NULL;
        }
    devExt->MasterKeyLength = 0;

    // ...any ESSIV key
    if (devExt->ESSIVKey != NULL)
        {
        SecZeroMemory(
                    devExt->ESSIVKey,
                    (devExt->ESSIVKeyLength / 8)
                    );
        FREEOTFE_FREE(devExt->ESSIVKey);
        devExt->ESSIVKey = NULL;
        }
    if (devExt->ESSIVKeyASCII != NULL)
        {
        SecZeroMemory(
                    devExt->ESSIVKeyASCII,
                    ((devExt->ESSIVKeyLength / 8) * 2)
                    );
        FREEOTFE_FREE(devExt->ESSIVKeyASCII);
        devExt->ESSIVKeyASCII = NULL;
        }
    devExt->ESSIVKeyLength = 0;


    // ...any volume IV
    if (devExt->VolumeIV != NULL)
        {
        SecZeroMemory(
                    devExt->VolumeIV,
                    (devExt->VolumeIVLength / 8)
                    );
        FREEOTFE_FREE(devExt->VolumeIV);
        devExt->VolumeIV = NULL;
        }
    devExt->VolumeIVLength = 0;

    // ...any volume flags
    devExt->VolumeFlags = 0;

    // ...any sector IV generation method
    devExt->SectorIVGenMethod = SCTRIVGEN_UNKNOWN;


    // ...any metadata
    if (devExt->MetaData != NULL)
        {
        SecZeroMemory(
                    devExt->MetaData,
                    devExt->MetaDataLength
                    );
        FREEOTFE_FREE(devExt->MetaData);
        devExt->MetaData = NULL;
        }
    devExt->MetaDataLength = 0;


    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("OverwriteDeviceSensitive\n"));
    
    return status;
}


// =========================================================================
// Destroy the specified device object, regardless of it's state
// Note: This can be called to destroy *either* the main device, *or* a
//       disk device
// Returns: The next device object
PDEVICE_OBJECT
DestroyDevice(
    IN PDEVICE_OBJECT devObj    
)
{
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;
    PDEVICE_OBJECT nextDevObj;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("DestroyDevice\n"));
        
    nextDevObj = devObj->NextDevice;
    
    devExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;    

    // Signal the device's thread to terminate, and wait until it does.
    devExt->TerminateThread = TRUE;
    CancelAllQueuedIRPs(devObj);
    if (devExt->ThreadObject != NULL) 
        {    
        // Make sure the thread wakes up, so it can see that it should terminate
        KeReleaseSemaphore(&devExt->IRPQueueSemaphore,
                            0,  // No priority boost
                            1,  // Increment semaphore by 1
                            TRUE );// WaitForXxx after this call

        // Wait for the thread to terminate
        KeWaitForSingleObject(devExt->ThreadObject,
                            Executive,
                            KernelMode,
                            FALSE,
                            NULL );

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Wait completed; thread terminated.\n"));
        
        // Release the thread object ref
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Dereferencing ThreadObject...\n"));
        ObDereferenceObject(devExt->ThreadObject);
        devExt->ThreadObject = NULL;
        }
    

    // Main device specific
    if (devExt->IsMainDevice) 
        {
        // Tear down any symbolic device name link
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Tearing down main device specific...\n"));
        if (devExt->zzSymbolicLinkName.Buffer != NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Tearing down symbolic device link...\n"));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to delete symlink: %ls\n", devExt->zzSymbolicLinkName.Buffer));
            status = IoDeleteSymbolicLink(&devExt->zzSymbolicLinkName);
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
                }
            
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off symlink unicode buffer...\n")); 
            SecZeroMemory(
                          devExt->zzSymbolicLinkName.Buffer,
                          sizeof(devExt->zzSymbolicLinkName.MaximumLength)
                         );
            FREEOTFE_FREE(devExt->zzSymbolicLinkName.Buffer);
            devExt->zzSymbolicLinkName.Buffer = NULL;
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
            }
        }

    
    // Tear down any device name
    if (devExt->zzDeviceName.Buffer != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off devname unicode buffer...\n"));
        SecZeroMemory(
                      devExt->zzDeviceName.Buffer,
                      sizeof(devExt->zzDeviceName.MaximumLength)
                     );
        FREEOTFE_FREE(devExt->zzDeviceName.Buffer);
        devExt->zzDeviceName.Buffer = NULL;
        }
    

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   

    // Nuke any disk-related information
    OverwriteDeviceSensitive(devObj);
    
    // Belt & braces - overwrite the whole device extension
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Overwriting device extension memory...\n"));   
    SecZeroMemory(devExt, sizeof(DEVICE_EXTENSION));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   

    // Get rid of the device object...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Deleting device...\n"));   
    IoDeleteDevice(devObj);
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
    

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("DestroyDevice\n"));
    
    return nextDevObj;
}


// =========================================================================
// Handle create IRPs
NTSTATUS
FreeOTFE_MF_DispatchCreate(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchCreate\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchCreate\n"));
    
    return STATUS_SUCCESS;
}


// =========================================================================
// Handle close IRPs
NTSTATUS
FreeOTFE_MF_DispatchClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchClose\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchClose\n"));
    
    return STATUS_SUCCESS;
}


// =========================================================================
// Handle device control IRPs
NTSTATUS
FreeOTFE_MF_DispatchDeviceControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;
    NTSTATUS status;
    PDEVICE_OBJECT queueDeviceObject = NULL;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    
    devExt = DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);

    status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;


    // If the request is to be queued for later processing, set "queueDeviceObject" to the
    // device object which is to have the IRP added to it's thread queue.
    // Otherwise:
    //   On success, set "status" to a success code, and "Irp->IoStatus.Information" to the
    //   number of bytes being sent back (returned) to the caller
    //   On failure, set "status" to a failure code; "Irp->IoStatus.Information" will be set
    //   to 0 before it's completed


    switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
        {
        case IOCTL_FREEOTFE_VERSION:
            {
            status = IOCTL_FreeOTFEIOCTL_Version(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFE_CREATE:
            {
            status = IOCTL_FreeOTFEIOCTL_Create(DeviceObject, Irp);
            break;
            }
            

        case IOCTL_FREEOTFE_DESTROY:
            {
            status = IOCTL_FreeOTFEIOCTL_Destroy(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFE_GET_RAW:
            {
            status = IOCTL_FreeOTFEIOCTL_GetRaw(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFE_SET_RAW:
            {
            status = IOCTL_FreeOTFEIOCTL_SetRaw(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_DERIVE_KEY:
            {
            status = IOCTL_FreeOTFEIOCTL_DeriveKey(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_GENERATE_MAC:
            {
            status = IOCTL_FreeOTFEIOCTL_GenerateMAC(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_MOUNT:
            {
            status = IOCTL_FreeOTFEIOCTL_Mount(
                                               DeviceObject,
                                               Irp,
                                               &queueDeviceObject
                                              );
            break;
            }


        case IOCTL_FREEOTFE_DOS_MOUNTPOINT_CREATE:
            {
            status = IOCTL_FreeOTFEIOCTL_DOSMountpointCreate(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_DOS_MOUNTPOINT_DELETE:
            {
            status = IOCTL_FreeOTFEIOCTL_DOSMountpointDelete(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_LDREU:
            {
            status = IOCTL_FreeOTFEIOCTL_LDREU(DeviceObject, Irp);
            break;
            }

        case IOCTL_FREEOTFE_DISMOUNT:
            {
            status = IOCTL_FreeOTFEIOCTL_Dismount(
                                                  DeviceObject,
                                                  Irp,
                                                  &queueDeviceObject
                                                 );
            break;
            }


        case IOCTL_FREEOTFE_GET_DISK_DEVICE_COUNT:
            {
            status = IOCTL_FreeOTFEIOCTL_GetDiskDeviceCount(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFE_GET_DISK_DEVICE_LIST:
            {
            status = IOCTL_FreeOTFEIOCTL_GetDiskDeviceList(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS:
            {
            status = IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(DeviceObject, Irp);        
            break;
            }

        case IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA:
            {
            status = IOCTL_FreeOTFEIOCTL_GetDiskDeviceMetaData(DeviceObject, Irp);        
            break;
            }


        //  --------------------------------------------------------------
        //  -- MS Windows standard disk IOCTL handling below this point --
        //  --------------------------------------------------------------
        
       
        case IOCTL_DISK_FORMAT_TRACKS:
        case IOCTL_DISK_FORMAT_TRACKS_EX:
            {
            status = IOCTL_Std_DiskFormatTracks(DeviceObject, Irp);
            break;
            }


        case IOCTL_DISK_IS_WRITABLE:
            {
            status = IOCTL_Std_DiskIsWritable(DeviceObject, Irp);
            break;
            }


        case IOCTL_DISK_GET_DRIVE_GEOMETRY:
        case IOCTL_CDROM_GET_DRIVE_GEOMETRY:
            {
            status = IOCTL_Std_DiskGetDriveGeometry(DeviceObject, Irp);
            break;
            }

        // xxxop - implement IOCTL_DISK_GET_DRIVE_GEOMETRY_EX
        // xxxop - implement IOCTL_CDROM_GET_DRIVE_GEOMETRY_EX

        case IOCTL_DISK_GET_DRIVE_LAYOUT:
            {
            status = IOCTL_Std_DiskGetDriveLayout(DeviceObject, Irp);
            break;
            }

#if (VER_PRODUCTBUILD >= 2600)
        // Windows XP and later only
        case IOCTL_DISK_GET_DRIVE_LAYOUT_EX:
            {
            status = IOCTL_Std_DiskGetDriveLayoutEx(DeviceObject, Irp);
            break;
            }
#endif

#if (VER_PRODUCTBUILD >= 2600)
        // Windows XP and later only
        case IOCTL_DISK_GET_LENGTH_INFO:
            {
            status = IOCTL_Std_DiskGetLengthInfo(DeviceObject, Irp);
            break;
            }
#endif
                        

        case IOCTL_DISK_GET_PARTITION_INFO:
            {
            status = IOCTL_Std_DiskGetPartitionInfo(DeviceObject, Irp);
            break;
            }
            
#if (VER_PRODUCTBUILD >= 2600)
        // Windows XP and later only
        case IOCTL_DISK_GET_PARTITION_INFO_EX:
            {
            status = IOCTL_Std_DiskGetPartitionInfoEx(DeviceObject, Irp);
            break;
            }
#endif
            
            
        case IOCTL_DISK_SET_PARTITION_INFO:
            {
            status = IOCTL_Std_DiskSetPartitionInfo(DeviceObject, Irp);
            break;
            }


#if (VER_PRODUCTBUILD >= 2600)
        // Windows XP and later only
        case IOCTL_DISK_SET_PARTITION_INFO_EX:
            {
            status = IOCTL_Std_DiskSetPartitionInfoEx(DeviceObject, Irp);
            break;
            }
#endif
            

        case IOCTL_DISK_VERIFY:
            {
            status = IOCTL_Std_DiskVerify(DeviceObject, Irp);
            break;
            }
            

        case IOCTL_DISK_CHECK_VERIFY:
        case IOCTL_CDROM_CHECK_VERIFY:
        case IOCTL_STORAGE_CHECK_VERIFY:
        case IOCTL_STORAGE_CHECK_VERIFY2:
            {                       
            status = IOCTL_Std_CheckVerify(DeviceObject, Irp);
            break;
            }
            

        // These two are only really valid for removable drives
        // They set/clear the media's avility to eject
        case IOCTL_DISK_MEDIA_REMOVAL:
        case IOCTL_STORAGE_MEDIA_REMOVAL:
            {
            status = IOCTL_Std_MediaRemoval(DeviceObject, Irp);
            break;
            }


        // These two are only really valid for removable drives
        // Equivilent of dismount
        case IOCTL_STORAGE_EJECT_MEDIA:
            {
            status = IOCTL_Std_StorageEjectMedia(
                                                 DeviceObject,
                                                 Irp,
                                                 &queueDeviceObject
                                                );
            break;
            }

        
        case IOCTL_CDROM_READ_TOC:
            {
            status = IOCTL_Std_CDROMReadTOC(DeviceObject, Irp);
            break;
            }

        // TODOzzz: Any more standard calls which can be implemented?


        default:
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("IoControl code is UNRECOGNISED (0x%x).\n",
                        irpSp->Parameters.DeviceIoControl.IoControlCode));
            status = STATUS_NOT_IMPLEMENTED;
            }
        }

        
        
    // If we want to queue the IRP, this should have been flagged by setting
    // "queueDeviceObject" to be the object which receives the queued IRP.
    // Otherwise we complete the request.
    if (queueDeviceObject != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queuing IRP...\n"));
        status = QueueIRPToThread(queueDeviceObject, Irp);    

        // Check if the IRP was queued successfully...
        if (status == STATUS_PENDING)
            {
            // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queued IRP.\n"));
            }
        else
            {
            Irp->IoStatus.Status = status;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to queue IRP.\n"));
            }

        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Completing IRP...\n"));
        // If there was a failure, return nothing; this will be set as appropriate if there
        // is no problem
        if (!(NT_SUCCESS(status)))
            {
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_Version(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_VERSION DIOCBuffer;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_Version\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_VERSION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_VERSION),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDIOC_VERSION)Irp->AssociatedIrp.SystemBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Reporting driver as version: 0x%08x (v%02d.%02d.%04d)\n",
                                     DRIVER_VERSION, 
                                     (DRIVER_VERSION & 0xFF000000) / 0x00FF0000,
                                     (DRIVER_VERSION & 0x00FF0000) / 0x0000FF00,
                                     (DRIVER_VERSION & 0x0000FFFF)
                                    ));
    DIOCBuffer->VersionID = DRIVER_VERSION;

    Irp->IoStatus.Information = sizeof(DIOC_VERSION);
    status = STATUS_SUCCESS;

    // Uncomment to execute test code
    // TestCode();

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_Version\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_Create(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DISK_DEVICE_CREATE DIOCBufferIn;
    PDIOC_DEVICE_NAME DIOCBufferOut;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    PDEVICE_EXTENSION tgtDevExt;
    ULONG createDeviceType;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_Create\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_DISK_DEVICE_CREATE))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DISK_DEVICE_CREATE),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DEVICE_NAME))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DEVICE_NAME),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...
    DIOCBufferIn = (PDIOC_DISK_DEVICE_CREATE)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_DEVICE_NAME)Irp->AssociatedIrp.SystemBuffer;

    // Store the input, as when we write to the output we'll overwrite it
    createDeviceType = DIOCBufferIn->DeviceType;


    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Creating disk device...\n"));
    status = CreateDiskDevice(
                              DeviceObject->DriverObject,
                              createDeviceType,
                              &tgtDeviceObj
                             );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Call to CreateDiskDevice FAILED.\n")); 
        return status;
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Disk device created OK.\n"));
    
    
    // Set the output buffer to contain the devicename of the device just
    // created
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying device name to output buffer.\n"));
    tgtDevExt = (PDEVICE_EXTENSION)tgtDeviceObj->DeviceExtension;

    RtlUnicodeStringToAnsiString(
                                    &tmpANSIDeviceName,
                                    &tgtDevExt->zzDeviceName,
                                    TRUE
                                ); 
    // Copy the resulting ANSI string into the buffer
    RtlZeroMemory(
                    DIOCBufferOut->DeviceName,
                    sizeof(DIOCBufferOut->DeviceName)
                    );
    RtlCopyMemory(            
                    &DIOCBufferOut->DeviceName,
                    tmpANSIDeviceName.Buffer,
                    tmpANSIDeviceName.Length
                    );
    RtlFreeAnsiString(&tmpANSIDeviceName);

    Irp->IoStatus.Information = sizeof(DIOC_DEVICE_NAME);
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_Create\n"));
    return status;
}

    
// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_Destroy(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DEVICE_NAME DIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    PDEVICE_EXTENSION tgtDevExt;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_Destroy\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_DEVICE_NAME))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DEVICE_NAME),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...
    DIOCBuffer = (PDIOC_DEVICE_NAME)Irp->AssociatedIrp.SystemBuffer;

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Locating disk device...\n"));

    status = GetNamedCharDevice(
                                DeviceObject->DriverObject,
                                (PCHAR)&DIOCBuffer->DeviceName,
                                &tgtDeviceObj
                                );
                            
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to locate required disk device.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }
        
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to destroy device...\n"));
    DestroyDevice(tgtDeviceObj);
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Device destroyed.\n"));            
    
    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_Destroy\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_Mount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_MOUNT DIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    PDEVICE_EXTENSION tgtDevExt;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_Mount\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    *QueueDeviceObject = NULL;


    DIOCBuffer = (PDIOC_MOUNT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_MOUNT) -
             sizeof(DIOCBuffer->MasterKey) - 
             sizeof(DIOCBuffer->VolumeIV) -
             sizeof(DIOCBuffer->MetaData) 
            )
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_MOUNT) -
             sizeof(DIOCBuffer->MasterKey) - 
             sizeof(DIOCBuffer->VolumeIV) -
             sizeof(DIOCBuffer->MetaData) 
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Key length supplied is apparently: %d bits\n", DIOCBuffer->MasterKeyLength));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("VolumeIV length supplied is apparently: %d bits\n", DIOCBuffer->VolumeIVLength));
    // Actual check...
    // Note: "/ 8" to convert bits to bytes
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_MOUNT) + 
             (DIOCBuffer->MasterKeyLength / 8) - 
             sizeof(DIOCBuffer->MasterKey) +
             (DIOCBuffer->VolumeIVLength / 8) - 
             sizeof(DIOCBuffer->VolumeIV) + 
             (DIOCBuffer->MetaDataLength) - 
             sizeof(DIOCBuffer->MetaData)
            )
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_MOUNT) + 
             (DIOCBuffer->MasterKeyLength / 8) - 
             sizeof(DIOCBuffer->MasterKey) +
             (DIOCBuffer->VolumeIVLength / 8) - 
             sizeof(DIOCBuffer->VolumeIV) + 
             (DIOCBuffer->MetaDataLength) - 
             sizeof(DIOCBuffer->MetaData)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...

    // Locate the device on which the file is to be mounted...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Locating disk device...\n"));
    status = GetNamedCharDevice(
                                DeviceObject->DriverObject,
                                (PCHAR)&DIOCBuffer->DiskDeviceName,
                                &tgtDeviceObj
                                );
                            
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to locate required disk device.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }
    
    
    // Ensure that the device isn't already mounted...
    tgtDevExt = (PDEVICE_EXTENSION)tgtDeviceObj->DeviceExtension;
    if (tgtDevExt->Mounted)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Cannot mount device; it's already mounted.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }

        
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to flag that IRP should be queued for target device...\n"));

    *QueueDeviceObject = tgtDeviceObj;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_Mount\n"));
    return status;
}


// =========================================================================
// Set raw data in the volume/keyfile
NTSTATUS
IOCTL_FreeOTFEIOCTL_SetRaw(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_SET_RAW_DATA DIOCBuffer;
    
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_SetRaw\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    // Check size of INPUT buffer
    // Minimum check...
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_SET_RAW_DATA) 
              - sizeof(DIOCBuffer->Data)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_SET_RAW_DATA) 
              - sizeof(DIOCBuffer->Data)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    DIOCBuffer = (PDIOC_SET_RAW_DATA)Irp->AssociatedIrp.SystemBuffer;

    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_SET_RAW_DATA)
              - sizeof(DIOCBuffer->Data)
              + DIOCBuffer->DataLength
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_SET_RAW_DATA)
              - sizeof(DIOCBuffer->Data)
              + DIOCBuffer->DataLength
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Request valid, process...


    status = SetGetRawChar(
                TRUE,
                (PCHAR)&DIOCBuffer->Filename,
                DIOCBuffer->Offset,
                DIOCBuffer->DataLength,
                (FREEOTFEBYTE*)&DIOCBuffer->Data
                );
    Irp->IoStatus.Information = 0;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_SetRaw\n"));
    return status;
}


// =========================================================================
// Get raw data from the volume/keyfile
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetRaw(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_GET_RAW_DATA_IN inDIOCBuffer;
    PDIOC_GET_RAW_DATA_OUT outDIOCBuffer;
    ULONG bytesWanted;
    
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GetRaw\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_GET_RAW_DATA_IN))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_GET_RAW_DATA_IN),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    inDIOCBuffer = (PDIOC_GET_RAW_DATA_IN)Irp->AssociatedIrp.SystemBuffer;

    // Check size of OUTPUT buffer
    // Note: No minimum check; we just return the data
    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            (
             sizeof(DIOC_GET_RAW_DATA_OUT)
              - sizeof(outDIOCBuffer->Data)
              + inDIOCBuffer->DataLength  // Yes, this is correct
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_GET_RAW_DATA_OUT)
              - sizeof(outDIOCBuffer->Data)
              + inDIOCBuffer->DataLength  // Yes, this is correct
            ),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    outDIOCBuffer = (PDIOC_GET_RAW_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;


    // Preserve length of data wanted; the output will overwrite this
    bytesWanted = inDIOCBuffer->DataLength;

    status = SetGetRawChar(
                FALSE,
                (PCHAR)&inDIOCBuffer->Filename,
                inDIOCBuffer->Offset,
                inDIOCBuffer->DataLength,
                (FREEOTFEBYTE*)&outDIOCBuffer->Data
                );

    if (status == STATUS_SUCCESS)
        {
        Irp->IoStatus.Information = bytesWanted;
        }
    else
        {
        Irp->IoStatus.Information = 0;
        }
   

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GetRaw\n"));
    return status;
}


// =========================================================================
// Write/Read raw data from volume
NTSTATUS
SetGetRawChar(
    IN     BOOLEAN DoWrite,
    IN     PCHAR Filename,
    IN     LARGE_INTEGER Offset,
    IN     ULONG DataLength,
    IN OUT FREEOTFEBYTE* Data
)
{
    NTSTATUS status;
    ANSI_STRING tmpANSIFilename;
    UNICODE_STRING tmpUNICODEFilename;
    
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("SetGetRawChar\n"));


    // Convert the ASCII filename into a UNICODE_STRING
    tmpANSIFilename.Length = (USHORT)strlen(Filename);
    tmpANSIFilename.MaximumLength = tmpANSIFilename.Length + 1;  
    tmpANSIFilename.Buffer = FREEOTFE_MEMALLOC(
                                         tmpANSIFilename.MaximumLength
                                        );
    RtlZeroMemory(
                  tmpANSIFilename.Buffer,
                  tmpANSIFilename.MaximumLength
                 );
    RtlCopyMemory(
                  tmpANSIFilename.Buffer,
                  Filename,
                  tmpANSIFilename.Length
                 );

    status = RtlAnsiStringToUnicodeString(
                                          &tmpUNICODEFilename,
                                          &tmpANSIFilename,
                                          TRUE
                                         );
    if (!(NT_SUCCESS(status)))
        {
        FREEOTFE_FREE(tmpANSIFilename.Buffer);
        }
    else
        {
        status = SetGetRawUnicode(
                    DoWrite,
                    &tmpUNICODEFilename,
                    Offset,
                    DataLength,
                    Data
                   ); 

        RtlFreeUnicodeString(&tmpUNICODEFilename);
        FREEOTFE_FREE(tmpANSIFilename.Buffer);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("SetGetRawChar\n"));
    return status;
}


// =========================================================================
// Write/Read raw data from volume
NTSTATUS
SetGetRawUnicode(
    IN     BOOLEAN DoWrite,
    IN     PUNICODE_STRING Filename,
    IN     LARGE_INTEGER Offset,
    IN     ULONG DataLength,
    IN OUT FREEOTFEBYTE* Data
)
{
    NTSTATUS status;
    FILE_BASIC_INFORMATION FileAttributes;
    OBJECT_ATTRIBUTES fileObjAttribs;
    HANDLE fileHandle;
    IO_STATUS_BLOCK ioStatusBlock;
    BOOLEAN fileAttributesStored;
    FILE_BASIC_INFORMATION fileAttributes;
    ACCESS_MASK access;
    NTSTATUS tmpStatus;
    FILE_STANDARD_INFORMATION fileStdInfo;  
    PSECURITY_CLIENT_CONTEXT ClientContext;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("SetGetRawUnicode\n"));

    
    if (DoWrite)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Write raw requested\n"));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Read raw requested\n"));
        }

    status = ClientSecurityCreate(&ClientContext);
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to create security client\n"));
        return status;
        }

    // Note: We allow intermediate buffering as "FILE_NO_INTERMEDIATE_BUFFERING"
    //       would require all offsets and read/writes to be a multiple of the
    //       sector size - reducing flexability
    status = FileOpen(
                      Filename,
                      ClientContext,
                      (!(DoWrite)),
                      TRUE,
                      &fileHandle,
                      &ioStatusBlock,
                      &fileAttributesStored,
                      &fileAttributes
                     );

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Offset    : %lld.\n", Offset.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("DataLength: %d.\n", DataLength));
        if (DoWrite)
            {
            status = ZwWriteFile(
                                fileHandle,
                                NULL,
                                NULL,
                                NULL,
                                &ioStatusBlock,
                                Data,
                                DataLength,
                                &Offset,
                                NULL
                            );
            }
        else
            {
            status = ZwReadFile(
                                fileHandle,
                                NULL,
                                NULL,
                                NULL,
                                &ioStatusBlock,
                                Data,
                                DataLength, 
                                &Offset,
                                NULL
                            );
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Read/write status: %d; wanted: %d.\n", status, STATUS_SUCCESS));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Read/written: %d bytes (only valid if status is success).\n", ioStatusBlock.Information));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to read/write raw (%d %d).\n", status, ioStatusBlock.Status));
            status = STATUS_INVALID_PARAMETER;
            }
        else if (ioStatusBlock.Information != DataLength)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to read/write all data. Transferred: %d bytes; expected: %d bytes.\n", ioStatusBlock.Information, DataLength));
            status = STATUS_INVALID_PARAMETER;
            }
    

        // Return value ignored
        FileClose(
                  &fileHandle,
                  fileAttributesStored,
                  &fileAttributes,
                  &ioStatusBlock
                 );

        }

    // Return value ignored
    ClientSecurityDestroy(&ClientContext);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("SetGetRawUnicode\n"));

    return status;
}


// =========================================================================
// Derive a key
NTSTATUS
IOCTL_FreeOTFEIOCTL_DeriveKey(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DERIVE_KEY_IN DIOCBufferIn;
    PDIOC_DERIVE_KEY_OUT DIOCBufferOut;
    FREEOTFEBYTE* tmpOutput;
    int tmpLengthBits;  // In *bits*
    int userBufferSizeBytes;  // In *bytes*
    unsigned int useBits;  // In *bits*
    int lengthWanted;  // In *bits*
    unsigned int i;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_DeriveKey\n"));

    status = STATUS_SUCCESS;

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_DERIVE_KEY_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_DERIVE_KEY_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_DERIVE_KEY_IN) 
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_DERIVE_KEY_IN)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_DERIVE_KEY_IN)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
              + (DIOCBufferIn->PasswordLength / 8)
              + (DIOCBufferIn->SaltLength / 8)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_DERIVE_KEY_IN)
              - sizeof(DIOCBufferIn->Password)
              - sizeof(DIOCBufferIn->Salt)
              + (DIOCBufferIn->PasswordLength / 8)
              + (DIOCBufferIn->SaltLength / 8)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DERIVE_KEY_OUT)-sizeof(DIOCBufferOut->DerivedKey))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_DERIVE_KEY_OUT)-sizeof(DIOCBufferOut->DerivedKey),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
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
    userBufferSizeBytes = (irpSp->Parameters.DeviceIoControl.OutputBufferLength) - 
                          sizeof(DIOCBufferOut) +
                          sizeof(DIOCBufferOut->DerivedKey);
    tmpOutput = FREEOTFE_MEMALLOC(userBufferSizeBytes); 

    // The size of the buffer in bits; the algorithm will read this value, then
    // overwrite it with the actual size of the output (in bits)
    tmpLengthBits = (userBufferSizeBytes * 8);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Allocated output buffer for: %d bits\n", tmpLengthBits));
    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to process...\n"));


    // This call runs the correct algorithm and generates the output
    if (DIOCBufferIn->LengthWanted == 0)
        {
        // Optimisation - if the user wants a zero length output, don't
        // bother processing, just return zero length output.
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Zero length output requested; returning nothing\n"));
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

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("DK Done with status: %d (want status: %d)\n", status, STATUS_SUCCESS));
    if (NT_SUCCESS(status)) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("tmp buffer was: %d bits, output was %d bits\n", (userBufferSizeBytes * 8), tmpLengthBits));


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
            if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
                    sizeof(DIOC_DERIVE_KEY_OUT)+(lengthWanted/8)-sizeof(DIOCBufferOut->DerivedKey))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
                    sizeof(DIOC_DERIVE_KEY_OUT)+(lengthWanted/8)-sizeof(DIOCBufferOut->DerivedKey),
                    irpSp->Parameters.DeviceIoControl.OutputBufferLength
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
                    RtlZeroMemory(
                                &DIOCBufferOut->DerivedKey,
                                (lengthWanted / 8)
                                );

                    useBits = tmpLengthBits;
                    }
                else
                    {
                    useBits = lengthWanted;
                    }
                RtlCopyMemory(&DIOCBufferOut->DerivedKey, tmpOutput, (useBits / 8));
                DIOCBufferOut->DerivedKeyLength = lengthWanted;
                }
            }
        else
            // User didn't specify an output length - we use the entire
            // algorithm's output
            {
            if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
                    sizeof(DIOC_DERIVE_KEY_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->DerivedKey))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
                    sizeof(DIOC_DERIVE_KEY_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->DerivedKey),
                    irpSp->Parameters.DeviceIoControl.OutputBufferLength
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                RtlCopyMemory(&DIOCBufferOut->DerivedKey, tmpOutput, (tmpLengthBits / 8));
                DIOCBufferOut->DerivedKeyLength = tmpLengthBits;
                }
            }

        Irp->IoStatus.Information = sizeof(DIOC_DERIVE_KEY_OUT)+(DIOCBufferOut->DerivedKeyLength/8)-sizeof(DIOCBufferOut->DerivedKey);
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("FAILED"));
        }

    SecZeroMemory(tmpOutput, userBufferSizeBytes);
    FREEOTFE_FREE(tmpOutput);


    if (NT_SUCCESS(status)) 
        {
        // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- DerivedKey length: %d bits\n", DIOCBufferOut->DerivedKeyLength));
        for(i = 0; i < (DIOCBufferOut->DerivedKeyLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- DerivedKey [%.2d]: %.2x\n", i, DIOCBufferOut->DerivedKey[i]));
            }                                
        }



    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_DeriveKey\n"));

    return status;
}

    
// =========================================================================
// Generate a MAC
NTSTATUS
IOCTL_FreeOTFEIOCTL_GenerateMAC(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_GENERATE_MAC_IN DIOCBufferIn;
    PDIOC_GENERATE_MAC_OUT DIOCBufferOut;
    FREEOTFEBYTE* tmpOutput;
    int tmpLengthBits;  // In *bits*
    int userBufferSizeBytes;  // In *bytes*
    unsigned int useBits;  // In *bits*
    int lengthWanted;  // In *bits*
    unsigned int i;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GenerateMAC\n"));

    status = STATUS_SUCCESS;

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_GENERATE_MAC_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_GENERATE_MAC_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_GENERATE_MAC_IN) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_GENERATE_MAC_IN) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_GENERATE_MAC_IN) 
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)
              + (DIOCBufferIn->DataLength / 8)
            )
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_GENERATE_MAC_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)
              + (DIOCBufferIn->DataLength / 8)
            ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access length related
    // members for actual check)
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_GENERATE_MAC_OUT)-sizeof(DIOCBufferOut->MAC))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_GENERATE_MAC_OUT)-sizeof(DIOCBufferOut->MAC),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
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
    userBufferSizeBytes = (irpSp->Parameters.DeviceIoControl.OutputBufferLength) - 
                          sizeof(DIOCBufferOut) +
                          sizeof(DIOCBufferOut->MAC);
    tmpOutput = FREEOTFE_MEMALLOC(userBufferSizeBytes); 

    // The size of the buffer in bits; the algorithm will read this value, then
    // overwrite it with the actual size of the output (in bits)
    tmpLengthBits = (userBufferSizeBytes * 8);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Allocated output buffer for: %d bits\n", tmpLengthBits));
    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to process...\n"));


    // This call runs the correct algorithm and generates the output
    if (DIOCBufferIn->LengthWanted == 0)
        {
        // Optimisation - if the user wants a zero length output, don't
        // bother processing, just return zero length output.
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Zero length output requested; returning nothing\n"));
        tmpLengthBits = 0;
        status = STATUS_SUCCESS;
        }
    else
        {
        status = GenerateMAC(
                    DIOCBufferIn->MACAlgorithm,
                    DIOCBufferIn->HashDeviceName,
                    DIOCBufferIn->HashGUID,
                    DIOCBufferIn->CypherDeviceName,
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

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("MAC Done with status: %d (want status: %d)\n", status, STATUS_SUCCESS));
    if (NT_SUCCESS(status)) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("tmp buffer was: %d bits, output was %d bits\n", (userBufferSizeBytes * 8), tmpLengthBits));


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
            if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
                    sizeof(DIOC_GENERATE_MAC_OUT)+(lengthWanted/8)-sizeof(DIOCBufferOut->MAC))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
                    sizeof(DIOC_GENERATE_MAC_OUT)+(lengthWanted/8)-sizeof(DIOCBufferOut->MAC),
                    irpSp->Parameters.DeviceIoControl.OutputBufferLength
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
                    RtlZeroMemory(
                                &DIOCBufferOut->MAC,
                                (lengthWanted / 8)
                                );

                    useBits = tmpLengthBits;
                    }
                else
                    {
                    useBits = lengthWanted;
                    }
                RtlCopyMemory(&DIOCBufferOut->MAC, tmpOutput, (useBits / 8));
                DIOCBufferOut->MACLength = lengthWanted;
                }
            }
        else
            // User didn't specify an output length - we use the entire
            // algorithm's output
            {
            if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
                    sizeof(DIOC_GENERATE_MAC_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->MAC))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
                    sizeof(DIOC_GENERATE_MAC_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->MAC),
                    irpSp->Parameters.DeviceIoControl.OutputBufferLength
                    ));
                status = STATUS_INVALID_BUFFER_SIZE;
                }
            else
                {
                RtlCopyMemory(&DIOCBufferOut->MAC, tmpOutput, (tmpLengthBits / 8));
                DIOCBufferOut->MACLength = tmpLengthBits;
                }
            }

        Irp->IoStatus.Information = sizeof(DIOC_GENERATE_MAC_OUT)+(DIOCBufferOut->MACLength/8)-sizeof(DIOCBufferOut->MAC);
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("FAILED"));
        }

    SecZeroMemory(tmpOutput, userBufferSizeBytes);
    FREEOTFE_FREE(tmpOutput);


    if (NT_SUCCESS(status)) 
        {
        // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- MAC length: %d bits\n", DIOCBufferOut->MACLength));
        for(i = 0; i < (DIOCBufferOut->MACLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- MAC [%.2d]: %.2x\n", i, DIOCBufferOut->MAC[i]));
            }                                
        }



    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GenerateMAC\n"));

    return status;
}


// =========================================================================
NTSTATUS
GenerateMAC(
    IN      MAC_ALGORITHM MACAlgorithm,
    IN      CHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID HashGUID,
    IN      CHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
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
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GenerateMAC\n"));


    status = STATUS_SUCCESS;

    Hash.FnHash    = NULL;
    Cypher.FnEncrypt       = NULL;
    Cypher.FnDecrypt       = NULL;
    Cypher.FnEncryptSector = NULL;
    Cypher.FnDecryptSector = NULL;

    // Flags to indicate if we were passed hash/cypher details
    HashFlag   = (strlen(HashDeviceName) > 0);
    CypherFlag = (strlen(CypherDeviceName) > 0);


    // Locate the appropriate HASH algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (HashFlag)
            {
            status = GetDeviceDetailsHash(
                                HashDeviceName,
                                &HashGUID,

                                &Hash
                                );
            }
        }

    // Locate the appropriate CYPHER algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (CypherFlag)
            {
            status = GetDeviceDetailsCypher_v3_v1(
                                CypherDeviceName,
                                &CypherGUID,

                                &Cypher
                                );
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
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("MACs can only be generated with algorithms that have a defined block & output length\n"));
        status = STATUS_INVALID_PARAMETER;
        }


    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Processing with algorithm: %d\n", MACAlgorithm));
        if (MACAlgorithm == MAC_HASH)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Must supply a hash when using MAC_HASH\n"));
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
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Must supply a hash when using MAC_HMAC\n"));
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
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unrecognised MAC algorithm!\n"));
            }

        }


    if (CypherFlag)
        {
        FreeDeviceDetailsCypher_v3(&Cypher);
        }

    if (HashFlag)
        {
        FreeDeviceDetailsHash(&Hash);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GenerateMAC\n"));

    return status;
}


// =========================================================================
// Lock, dismount, remove, eject and unlock
// Originally added for Vista, where the user can't do this if UAC is
// operational, and the userspace software isn't running with escalated privs
NTSTATUS
IOCTL_FreeOTFEIOCTL_LDREU(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_LDREU DIOCBufferIn;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_LDREU\n"));

    status = STATUS_SUCCESS;

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_LDREU)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
             sizeof(*DIOCBufferIn))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(*DIOCBufferIn),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        

    // Request valid so far, process...    


    status = LDREUChar(
                   DeviceObject,
                   DIOCBufferIn->DriveFile,
                   DIOCBufferIn->DeviceName,
                   DIOCBufferIn->Emergency
                  );

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("LDREU Done with status: %d (want status: %d)\n", status, STATUS_SUCCESS));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_LDREU\n"));

    return status;
}


// =========================================================================
// Create DOS mountpoint
// i.e. Symlink under
//   \DosDevices\Q:
//   \DosDevices\Global\Q:
NTSTATUS
IOCTL_FreeOTFEIOCTL_DOSMountpointCreate(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DOS_MOUNTPOINT DIOCBufferIn;
    PDEVICE_OBJECT tgtDeviceObject;
    PUNICODE_STRING ptrLinkName;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_DOSMountpointCreate\n"));

    status = STATUS_SUCCESS;

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_DOS_MOUNTPOINT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
             sizeof(*DIOCBufferIn))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(*DIOCBufferIn),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        

    // Request valid so far, process...    

    // Sanity check; we're linking a device we know about, right? No funny
    // business passing us junk, OK?
    status = GetNamedCharDevice(
                       DeviceObject->DriverObject,
                       DIOCBufferIn->DiskDeviceName,
                       &tgtDeviceObject
                      );

    // Get the symlink name
    if (NT_SUCCESS(status))
        {
        status = AllocateDOSDeviceSymlinkTargetName_CharMountpoint(
                                                    DIOCBufferIn->Mountpoint,
                                                    DIOCBufferIn->Global,
                                                    &ptrLinkName
                                                   );
        if (!(NT_SUCCESS(status)))
            {
            ptrLinkName = NULL;
            }
        }

    if (NT_SUCCESS(status))
        {
        status = CreateSymlink(
                               DIOCBufferIn->DiskDeviceName,
                               ptrLinkName
                              );
        }

    FreeDOSDeviceSymlinkTargetName(ptrLinkName);

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Symlink created OK\n"));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to create symlink\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_DOSMountpointCreate\n"));
    return status;
}


// =========================================================================
// Allocate a UNICODE string with the appropriate \DosDevices\... symlink name
// in it
// !! NOTE !!
// Currently only uses the FIRST LETTER OF MOUNTPOINT AS A DRIVELETTER
// !! IMPORTANT !!
// It is the *callers* responsibility to free off SymLinkName after use, e.g.
// by calling FreeDOSDeviceSymlinkTargetName(...)
NTSTATUS
AllocateDOSDeviceSymlinkTargetName_CharMountpoint(
    IN  char* Mountpoint,
    IN  BOOLEAN Global,
    OUT PUNICODE_STRING* SymLinkName
)
{
    NTSTATUS status;
    WCHAR driveLetter;
    PUNICODE_STRING ptrUNICODEMountpoint;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("AllocateDOSDeviceSymlinkTargetName_CharMountpoint\n"));

    // Convert mountpoint to UNICODE
    status = AllocateUnicodeString(
                                   Mountpoint,
                                   &ptrUNICODEMountpoint
                                  );
    if (!(NT_SUCCESS(status)))
        {
        ptrUNICODEMountpoint = NULL;
        }
    else
        {
        driveLetter = ptrUNICODEMountpoint->Buffer[0];
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got drive letter: %c from mountpoint\n", driveLetter));
        FreeUnicodeString(ptrUNICODEMountpoint);
        }

    if (NT_SUCCESS(status))
        {
        status = AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter(
                                    driveLetter,
                                    Global,
                                    SymLinkName
                                    );
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("AllocateDOSDeviceSymlinkTargetName_CharMountpoint\n"));
    return status;
}


// =========================================================================
// Allocate a UNICODE string with the appropriate \DosDevices\... symlink name
// in it
// !! IMPORTANT !!
// It is the *callers* responsibility to free off SymLinkName after use, e.g.
// by calling FreeDOSDeviceSymlinkTargetName(...)
NTSTATUS
AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter(
    IN  WCHAR DriveLetter,
    IN  BOOLEAN Global,
    OUT PUNICODE_STRING* SymLinkName
)
{
    NTSTATUS status;
    PUNICODE_STRING ptrTmpLinkName;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter\n"));

    status = STATUS_SUCCESS;

    ptrTmpLinkName = NULL;

    if (NT_SUCCESS(status))
        {
        ptrTmpLinkName = FREEOTFE_MEMALLOC(sizeof(*ptrTmpLinkName));
        if (ptrTmpLinkName == NULL)
            {
            status = STATUS_NO_MEMORY;
            }
        }

    if (NT_SUCCESS(status))
        {
        // Generate the symlink name
        // We add on sizeof(WCHAR) to include the terminating #NULL - required 
        // since although the UNICODE_STRING string doesn't require it, swprintf   
        // adds one on anyway
        ptrTmpLinkName->MaximumLength = max(sizeof(DEVICE_DOSDEVICES), sizeof(DEVICE_DOSDEVICES_GLOBAL)) +
                                 sizeof(DEVICE_SLASH) +
                                 sizeof(WCHAR) + // Drive letter
                                 sizeof(WCHAR);  // Terminating NULL
        ptrTmpLinkName->Buffer = FREEOTFE_MEMALLOC(ptrTmpLinkName->MaximumLength);
        RtlZeroMemory(ptrTmpLinkName->Buffer, ptrTmpLinkName->MaximumLength);  

        // e.g.:
        //   \DosDevices\Q:
        //   \DosDevices\Global\Q:
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Drive letter: %c\n", DriveLetter));
        if (Global)
            {
            ptrTmpLinkName->Length = (USHORT)swprintf(
                                            ptrTmpLinkName->Buffer,
                                            DEVICE_DOSDEVICES_GLOBAL DEVICE_SLASH L"%c:",
                                            DriveLetter
                                            );
            }
        else
            {
            ptrTmpLinkName->Length = (USHORT)swprintf(
                                            ptrTmpLinkName->Buffer,
                                            DEVICE_DOSDEVICES DEVICE_SLASH L"%c:",
                                            DriveLetter
                                            );
            }
        // swprintf returns the number of WCHARs, not the length in bytes
        ptrTmpLinkName->Length = ptrTmpLinkName->Length * sizeof(WCHAR);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Symlink name: %ls\n", ptrTmpLinkName->Buffer));
        }

    if (NT_SUCCESS(status))
        {
        *SymLinkName = ptrTmpLinkName;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK with AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter\n"));
        }
    else
        {
        *SymLinkName = NULL;
        FreeDOSDeviceSymlinkTargetName(ptrTmpLinkName);
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter\n"));
    return status;
}


// =========================================================================
VOID
FreeDOSDeviceSymlinkTargetName(
    PUNICODE_STRING SymLinkName
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeDOSDeviceSymlinkTargetName\n"));

    if (SymLinkName != NULL)
        {
        if (SymLinkName->Buffer != NULL)
            {
            FREEOTFE_FREE(SymLinkName->Buffer);
            }
        FREEOTFE_FREE(SymLinkName);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeDOSDeviceSymlinkTargetName\n"));
}


// =========================================================================
// Delete DOS mountpoint
// i.e. Symlink under
//   \DosDevices\Q:
//   \DosDevices\Global\Q:
NTSTATUS
IOCTL_FreeOTFEIOCTL_DOSMountpointDelete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DOS_MOUNTPOINT DIOCBufferIn;
    PUNICODE_STRING ptrLinkName;
    HANDLE linkHandle;
    OBJECT_ATTRIBUTES fileObjAttribs;
    UNICODE_STRING linkTarget;
    ULONG linkTargetLength;
    PUNICODE_STRING ptrUNICODEUserDiskDeviceName;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_DOSMountpointDelete\n"));

    status = STATUS_SUCCESS;

    linkTarget.Buffer = NULL;
    ptrUNICODEUserDiskDeviceName = NULL;

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_DOS_MOUNTPOINT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
             sizeof(*DIOCBufferIn))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(*DIOCBufferIn),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        

    // Request valid so far, process...    

    // Convert DiskDeviceName passed in by user to UNICODE
    if (NT_SUCCESS(status))
        {
        status = AllocateUnicodeString(
                                    DIOCBufferIn->DiskDeviceName,
                                    &ptrUNICODEUserDiskDeviceName
                                    );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to convert user DiskDeviceName to UNICODE\n"));
            ptrUNICODEUserDiskDeviceName = NULL;
            }
        }

    // Sanity check; the symlink is to a FreeOTFE device, right?
    // Check by ensuring what the user passed in begins with the FreeOTFE
    // disk device device prefix: DEVICE_DISK_PREFIX
    if (NT_SUCCESS(status))
        {
        // Size check
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("User claimed target of the symlink to be deleted is: %ls\n", ptrUNICODEUserDiskDeviceName->Buffer));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("FreeOTFE disk symlink targets begin with: %ls\n", DEVICE_DISK_PREFIX));

        if ((ptrUNICODEUserDiskDeviceName->Length / sizeof(WCHAR)) <
                 wcslen(DEVICE_DISK_PREFIX))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Length of symlink target passed in too short to be a FreeOTFE disk device\n"));
            status = STATUS_INVALID_PARAMETER;
            }
        }
    if (NT_SUCCESS(status))
        {
        // Value check
        if (wcsncmp(
                    DEVICE_DISK_PREFIX,
                    ptrUNICODEUserDiskDeviceName->Buffer,
                    wcslen(DEVICE_DISK_PREFIX)
                   ) != 0)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Symlink target passed in doesn't start with expected text\n"));
            status = STATUS_INVALID_PARAMETER;
            }
        }


    // Get the symlink name
    if (NT_SUCCESS(status))
        {
        status = AllocateDOSDeviceSymlinkTargetName_CharMountpoint(
                                                    DIOCBufferIn->Mountpoint,
                                                    DIOCBufferIn->Global,
                                                    &ptrLinkName
                                                   );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to allocate DOS Device symlink target name\n"));
            ptrLinkName = NULL;
            }
        }

    if (NT_SUCCESS(status))
        {
        InitializeObjectAttributes(
                                &fileObjAttribs,
                                ptrLinkName,
                                (OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE),
                                NULL, // Root dir not needed since it's a fully
                                      // qualified name
                                NULL  // Security attribs
                                );

        status = ZwOpenSymbolicLinkObject(
                                        &linkHandle,
                                        GENERIC_READ,
                                        &fileObjAttribs
                                        );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("ZwOpenSymbolicLinkObject FAILED\n"));
            }

        if (NT_SUCCESS(status))
            {
            // +1 for terminating NULL
            linkTarget.Length = 0;
            linkTarget.MaximumLength = (FREEOTFE_MAX_FILENAME_LENGTH + 1) * sizeof(WCHAR);
            linkTarget.Buffer = FREEOTFE_MEMALLOC(linkTarget.MaximumLength);
            RtlZeroMemory(linkTarget.Buffer, linkTarget.MaximumLength);  
            
            status = ZwQuerySymbolicLinkObject(
                                               linkHandle,
                                               &linkTarget,
                                               &linkTargetLength
                                              );
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("ZwQuerySymbolicLinkObject FAILED\n"));
                }
            ZwClose(linkHandle);
            }

        }

    // Check that the target of the symlink is the device we expect
    // This is needed in case some other driver replaced the symlink with
    // something else (e.g. a USB drive which just *dumped* itself over
    // our stuff, or if two users logged onto the PC at the same time
    // mounted different volumes at global/local scope under the same drive
    // letter
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("User said symlink target was: %ls\n", ptrUNICODEUserDiskDeviceName->Buffer));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...is actually              : %ls\n", linkTarget.Buffer));

        // Size check
        if (ptrUNICODEUserDiskDeviceName->Length != linkTarget.Length)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Actual target length differs from user supplied symlink target (%d v. %d bytes)\n", 
                                    ptrUNICODEUserDiskDeviceName->Length,
                                    linkTarget.Length));
            status = STATUS_INVALID_PARAMETER;
            }
        }
    if (NT_SUCCESS(status))
        {
        // Value check
        if (wcsncmp(
                    ptrUNICODEUserDiskDeviceName->Buffer,
                    linkTarget.Buffer,
                    (linkTarget.Length / sizeof(WCHAR))
                ) != 0)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Symlink target passed in doesn't start match expected targer\n"));
            status = STATUS_INVALID_PARAMETER;
            }
        }


    if (NT_SUCCESS(status))
        {
        status = IoDeleteSymbolicLink(ptrLinkName);
        }

    SecZeroAndFreeMemory(linkTarget.Buffer, linkTarget.MaximumLength);
    FreeDOSDeviceSymlinkTargetName(ptrLinkName);
    FreeUnicodeString(ptrUNICODEUserDiskDeviceName);

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Symlink deleted OK\n"));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to deleted symlink\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_DOSMountpointDelete\n"));
    return status;
}


// =========================================================================
// Create symlink
// Filename - Existing file
// LinkName - Link to be created
NTSTATUS
CreateSymlink(
    IN      CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      PUNICODE_STRING LinkName
)
{
    NTSTATUS status;
    PUNICODE_STRING ptrUNICODEFilename;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("CreateSymlink\n"));


    status = AllocateUnicodeString(Filename, &ptrUNICODEFilename); 
    if (!(NT_SUCCESS(status)))
        {
        ptrUNICODEFilename = NULL;
        }

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Create symlink: %ls\n",
                                LinkName->Buffer));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Linking to existing file: %ls\n",
                                ptrUNICODEFilename->Buffer));

        status = IoCreateSymbolicLink(LinkName, ptrUNICODEFilename);
        }

    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        }

    FreeUnicodeString(ptrUNICODEFilename);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("CreateSymlink\n"));
    return status;
}


// =========================================================================
// Convert ANSI string to UNICODE string, allocating memory as appropriate
// !! IMPORTANT !!
// It is the *callers* responsibility to free off the UNICODE string after use
// e.g. By using FreeUnicodeString(...)
NTSTATUS
AllocateUnicodeString(
     IN   CHAR* ansiString,
     OUT  PUNICODE_STRING* ptrUNICODEString
)
{
    NTSTATUS status;
    ANSI_STRING tmpANSIString;
    PUNICODE_STRING tmpPtrUNICODEString;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, ("AllocateUnicodeString"));

    status = STATUS_INTERNAL_ERROR;

    if (ansiString != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, ("UNICODE convering: %s\n", ansiString));

        tmpPtrUNICODEString = FREEOTFE_MEMALLOC(sizeof(*ptrUNICODEString));

        // Convert the ASCII filename into a UNICODE_STRING
        tmpANSIString.Length = (USHORT)strlen(ansiString);
        tmpANSIString.MaximumLength = tmpANSIString.Length + 1;  
        tmpANSIString.Buffer = FREEOTFE_MEMALLOC(tmpANSIString.MaximumLength);
        RtlZeroMemory(
                    tmpANSIString.Buffer,
                    tmpANSIString.MaximumLength
                    );
        RtlCopyMemory(
                    tmpANSIString.Buffer,
                    ansiString,
                    tmpANSIString.Length
                    );

        status = RtlAnsiStringToUnicodeString(
                                            tmpPtrUNICODEString,
                                            &tmpANSIString,
                                            TRUE
                                            );
        FREEOTFE_FREE(tmpANSIString.Buffer);

        if (!(NT_SUCCESS(status)))
            {
            FREEOTFE_FREE(tmpPtrUNICODEString);
            }

        }

    if (NT_SUCCESS(status))
        {
        *ptrUNICODEString = tmpPtrUNICODEString;
        }
    else
        {
        *ptrUNICODEString = NULL;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, ("AllocateUnicodeString"));
    return status;
}


// =========================================================================
// Free off unicode string allocated by AllocateUnicodeString(...)
VOID
FreeUnicodeString(
    PUNICODE_STRING ptrUNICODEString
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, ("FreeUnicodeString"));

    if (ptrUNICODEString != NULL)
        {
        RtlFreeUnicodeString(ptrUNICODEString);
        FREEOTFE_FREE(ptrUNICODEString);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, ("FreeUnicodeString"));

}


// =========================================================================
NTSTATUS
LDREUChar(
    IN PDEVICE_OBJECT DeviceObject,
    IN      CHAR DriveFile[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      CHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      BOOLEAN  Emergency
)
{
    NTSTATUS status;
    ANSI_STRING tmpANSIDriveFile;
    UNICODE_STRING tmpUNICODEDriveFile;
    ANSI_STRING tmpANSIDeviceName;
    UNICODE_STRING tmpUNICODEDeviceName;
    
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("LDREUChar\n"));


    status = STATUS_SUCCESS;

    if (NT_SUCCESS(status))
        {
        // Convert the ASCII filename into a UNICODE_STRING
        tmpANSIDriveFile.Length = (USHORT)strlen(DriveFile);
        tmpANSIDriveFile.MaximumLength = tmpANSIDriveFile.Length + 1;  
        tmpANSIDriveFile.Buffer = FREEOTFE_MEMALLOC(tmpANSIDriveFile.MaximumLength);
        RtlZeroMemory(
                    tmpANSIDriveFile.Buffer,
                    tmpANSIDriveFile.MaximumLength
                    );
        RtlCopyMemory(
                    tmpANSIDriveFile.Buffer,
                    DriveFile,
                    tmpANSIDriveFile.Length
                    );

        status = RtlAnsiStringToUnicodeString(
                                            &tmpUNICODEDriveFile,
                                            &tmpANSIDriveFile,
                                            TRUE
                                            );
        if (!(NT_SUCCESS(status)))
            {
            FREEOTFE_FREE(tmpANSIDriveFile.Buffer);
            }
        }

    if (NT_SUCCESS(status))
        {
        // Convert the ASCII filename into a UNICODE_STRING
        tmpANSIDeviceName.Length = (USHORT)strlen(DeviceName);
        tmpANSIDeviceName.MaximumLength = tmpANSIDeviceName.Length + 1;  
        tmpANSIDeviceName.Buffer = FREEOTFE_MEMALLOC(tmpANSIDeviceName.MaximumLength);
        RtlZeroMemory(
                    tmpANSIDeviceName.Buffer,
                    tmpANSIDeviceName.MaximumLength
                    );
        RtlCopyMemory(
                    tmpANSIDeviceName.Buffer,
                    DeviceName,
                    tmpANSIDeviceName.Length
                    );

        status = RtlAnsiStringToUnicodeString(
                                            &tmpUNICODEDeviceName,
                                            &tmpANSIDeviceName,
                                            TRUE
                                            );
        if (!(NT_SUCCESS(status)))
            {
            FREEOTFE_FREE(tmpANSIDeviceName.Buffer);
            }
        }


    if (NT_SUCCESS(status))
        {
        status = LDREUUnicode(
                    DeviceObject,
                    &tmpUNICODEDriveFile,
                    DeviceName,
                    &tmpUNICODEDeviceName,
                    Emergency
                   ); 

        RtlFreeUnicodeString(&tmpUNICODEDriveFile);
        FREEOTFE_FREE(tmpANSIDriveFile.Buffer);

        RtlFreeUnicodeString(&tmpUNICODEDeviceName);
        FREEOTFE_FREE(tmpANSIDeviceName.Buffer);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("LDREUChar\n"));
    return status;
}


// =========================================================================
// DeviceNameAnsi and DeviceNameUnicode must be the same string!
NTSTATUS
LDREUUnicode(
    IN PDEVICE_OBJECT DeviceObject,
    IN      PUNICODE_STRING DriveFile,
    IN      CHAR DeviceNameAnsi[FREEOTFE_MAX_FILENAME_LENGTH],  // ANSI version
    IN      PUNICODE_STRING DeviceNameUnicode,  // UNICODE version
    IN      BOOLEAN  Emergency
)
{
    NTSTATUS status;
    HANDLE driveDevice;
    ULONG BytesReturned;
    PREVENT_MEDIA_REMOVAL pmr;
    OBJECT_ATTRIBUTES fileObjAttribs;
    IO_STATUS_BLOCK ioStatusBlock;
    ACCESS_MASK desiredAccess;
    DIOC_DISMOUNT dismountDIOC;
    NTSTATUS tmpStatus;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("LDREUUnicode\n"));


    status = STATUS_SUCCESS;


    // We attempt to open the drive, and then carry out various operations to
    // shut the drive down.
    // If were operating in an emergency, then we don't care if some of these
    // operations fail - it's an EMERGENCY! JUST GET RID OF THE %$!# DRIVE!


    // Initialize to flag that we haven't opened it
    driveDevice = NULL;
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ZwCreateFile: %ls\n", DriveFile->Buffer));

        status = FileOpen(
                          DriveFile,
                          NULL,
                          TRUE,
                          TRUE,
                          &driveDevice,
                          &ioStatusBlock,
                          NULL,
                          NULL
                         );
        if (!(NT_SUCCESS(status)))
            {
            DriveFile = NULL;
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("ZwCreateFile FAILED\n"));
            }
        }


    if (
        (NT_SUCCESS(status)) ||
        (Emergency && (DriveFile != NULL))
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("lockvolume\n"));

        status = ZwFsControlFile(
                                 driveDevice,
                                 NULL,
                                 NULL,
                                 NULL,
                                 &ioStatusBlock,
                                 FSCTL_LOCK_VOLUME,
                                 NULL,
                                 0,
                                 NULL,
                                 0
                                );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("lockvolume - FAILED.\n"));
            }
        }


    if (
        (NT_SUCCESS(status)) ||
        (Emergency && (DriveFile != NULL))
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("dismountvolume\n"));

        status = ZwFsControlFile(
                                 driveDevice,
                                 NULL,
                                 NULL,
                                 NULL,
                                 &ioStatusBlock,
                                 FSCTL_DISMOUNT_VOLUME,
                                 NULL,
                                 0,
                                 NULL,
                                 0
                                );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("dismountvolume - FAILED.\n"));
            }
        }


    if (
        (NT_SUCCESS(status)) ||
        (Emergency && (DriveFile != NULL))
       )
        {
        pmr.PreventMediaRemoval = FALSE;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("mediaremoval\n"));
        status = SendFOTFEDiskDeviceDIOCUnicode(
                              DeviceObject,
                              DeviceNameUnicode, 
                              IOCTL_STORAGE_MEDIA_REMOVAL, 
                              sizeof(pmr),
                              &pmr
                             );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("FAILED TO IOCTL_STORAGE_MEDIA_REMOVAL\n"));
            }
        }

    if (
        (NT_SUCCESS(status)) ||
        (Emergency && (DriveFile != NULL))
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ejectmedia\n"));
        status = SendFOTFEDiskDeviceDIOCUnicode(
                              DeviceObject,
                              DeviceNameUnicode,
                              IOCTL_STORAGE_EJECT_MEDIA,
                              0,
                              NULL
                             );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("FAILED TO IOCTL_STORAGE_EJECT_MEDIA\n"));
            }
        }


    // An explicit call to IOCTL_FREEOTFE_DISMOUNT should be redundant; the
    // IOCTL_STORAGE_EJECT_MEDIA should have already done this
    if (
        (NT_SUCCESS(status)) ||
        (Emergency)
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("dismountdevice\n"));
        RtlCopyMemory(            
                      dismountDIOC.DiskDeviceName,
                      DeviceNameAnsi,
                      sizeof(dismountDIOC.DiskDeviceName)
                     );
        dismountDIOC.Emergency = Emergency;
        // Yes, this should be OK - sending it to the target device, which will
        // effectivly flush it's queue (if an emergency) and queue the dismount
        status = SendFOTFEDiskDeviceDIOCUnicode(
                              DeviceObject,
                              DeviceNameUnicode,
                              IOCTL_FREEOTFE_DISMOUNT,
                              sizeof(dismountDIOC),
                              &dismountDIOC                              
                             );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("FAILED TO IOCTL_FREEOTFE_DISMOUNT\n"));
            }
        }


    if (
        (NT_SUCCESS(status)) ||
        (Emergency && (DriveFile != NULL))
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("unlock\n"));
        status = ZwFsControlFile(
                                 driveDevice,
                                 NULL,
                                 NULL,
                                 NULL,
                                 &ioStatusBlock,
                                 FSCTL_UNLOCK_VOLUME,
                                 NULL,
                                 0,
                                 NULL,
                                 0
                                );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("unlock - FAILED.\n"));
            }
        }


    if (DriveFile != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("closehandle\n"));
        // Note that we can't do:
        //   allOK := allOK and CloseHandle(driveDevice);"
        // here because lazy evaluation will prevent CloseHandle(...) from being
        // called if allOK is FALSE - and we *want* CloseHandle(...) called
        // regardless, otherwise we end up with FreeOTFE never closing this file
        // handle, and "leaking" this handle
        tmpStatus = FileClose(&driveDevice, FALSE, NULL, &ioStatusBlock);
        // ...i.e. If we were OK up to this point, set status to that returned
        // by FileClose(...) - but don't set directly; if there were earlier
        // problems, we don't want status to be set to sucess just because the
        // file could be closed!
        if (NT_SUCCESS(status))
            {
            status = tmpStatus;
            }
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("LDREUUnicode\n"));

    return status;
}


// =========================================================================
// Send simple DIOC to device
NTSTATUS
SendFOTFEDiskDeviceDIOCChar(
    IN PDEVICE_OBJECT DeviceObject,
    IN      char*   TgtDeviceName,
    IN      ULONG   ControlCode,
    IN      ULONG   InBufferSize,
    IN      void*   InBuffer
)
{
    NTSTATUS status;
    ANSI_STRING tmpANSIdeviceName;
    UNICODE_STRING tmpUNICODEdeviceName;
    
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("SendFOTFEDiskDeviceDIOCChar\n"));

    status = STATUS_SUCCESS;

    if (NT_SUCCESS(status))
        {
        // Convert the ASCII filename into a UNICODE_STRING
        tmpANSIdeviceName.Length = (USHORT)strlen(TgtDeviceName);
        tmpANSIdeviceName.MaximumLength = tmpANSIdeviceName.Length + 1;  
        tmpANSIdeviceName.Buffer = FREEOTFE_MEMALLOC(tmpANSIdeviceName.MaximumLength);
        RtlZeroMemory(
                    tmpANSIdeviceName.Buffer,
                    tmpANSIdeviceName.MaximumLength
                    );
        RtlCopyMemory(
                    tmpANSIdeviceName.Buffer,
                    TgtDeviceName,
                    tmpANSIdeviceName.Length
                    );

        status = RtlAnsiStringToUnicodeString(
                                            &tmpUNICODEdeviceName,
                                            &tmpANSIdeviceName,
                                            TRUE
                                            );
        if (!(NT_SUCCESS(status)))
            {
            FREEOTFE_FREE(tmpANSIdeviceName.Buffer);
            }
        }


    if (NT_SUCCESS(status))
        {
        status = SendFOTFEDiskDeviceDIOCUnicode(
                                 DeviceObject,
                                 &tmpUNICODEdeviceName,
                                 ControlCode,
                                 InBufferSize,
                                 InBuffer
                                );

        RtlFreeUnicodeString(&tmpUNICODEdeviceName);
        FREEOTFE_FREE(tmpANSIdeviceName.Buffer);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("SendFOTFEDiskDeviceDIOCChar\n"));
    return status;
}


// =========================================================================
// Emulate a DeviceIOControl(...) call within the driver.
// srcDeviceObject - This is optional, and may be set to NULL. If it's
//                   non-NULL, then if the call to IoGetDeviceObjectPointer(...)
//                   fails, it won't fallback to using the list of disk device
//                   objects it knows about
// InBufferSize - Size of InBuffer in bytes
// InBuffer - Buffer passed *to* the device named
NTSTATUS
SendFOTFEDiskDeviceDIOCUnicode(
    IN PDEVICE_OBJECT srcDeviceObject,
    IN      PUNICODE_STRING TgtDeviceName,
    IN      ULONG   ControlCode,
    IN      ULONG   InBufferSize,
    IN      void*   InBuffer
)
{
    NTSTATUS status;
    PIRP tmpIrp;
    PIO_STACK_LOCATION irpSp;
    KEVENT event;
    ANSI_STRING tmpANSIname;
    PFILE_OBJECT FileObject;
    PDEVICE_OBJECT DeviceObject;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("SendFOTFEDiskDeviceDIOCUnicode\n"));
    
    tmpIrp = NULL;

    status = STATUS_SUCCESS;

    FileObject = NULL;
    DeviceObject = NULL;

    if (NT_SUCCESS(status))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoGetDeviceObjectPointer...\n"));
        status = IoGetDeviceObjectPointer(
                                          TgtDeviceName,
                                          FILE_ALL_ACCESS,
                                          &FileObject,
                                          &DeviceObject
                                         ); 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got ptr to device with status: %d (want status: %d)\n", status, STATUS_SUCCESS));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to get device object for devicename: %ls\n", TgtDeviceName->Buffer));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Attempting to fallback to known FreeOTFE disk devices...\n"));
            if (srcDeviceObject != NULL)
                {
                status = GetNamedUnicodeDevice (
                                                srcDeviceObject->DriverObject,
                                                TgtDeviceName, 
                                                &DeviceObject
                                               );
                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get device object AT ALL for DIOC call\n"));
                    }
                }

            }
        }
        

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoAllocateIrp...\n"));
        tmpIrp = IoAllocateIrp(
                               DeviceObject->StackSize,
                               FALSE
                              );
                          
        if (tmpIrp == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoAllocateIrp failure.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        }
    

    if (NT_SUCCESS(status))
        {                         
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up IRP...\n"));
        tmpIrp->Flags = (IRP_BUFFERED_IO | IRP_INPUT_OPERATION);
        tmpIrp->IoStatus.Status = STATUS_NOT_SUPPORTED;
    
        irpSp = IoGetNextIrpStackLocation(tmpIrp);

        irpSp->MajorFunction = IRP_MJ_DEVICE_CONTROL;

        // IOCTL_FREEOTFEHASH_INTLDETAILS uses METHOD_BUFFERED, so we use
        // "AssociatedIrp.SystemBuffer"
        tmpIrp->AssociatedIrp.SystemBuffer = InBuffer;

        irpSp->Parameters.DeviceIoControl.OutputBufferLength = 0;
        irpSp->Parameters.DeviceIoControl.InputBufferLength  = InBufferSize;
        irpSp->Parameters.DeviceIoControl.IoControlCode      = ControlCode;
    

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up event and completion routine...\n"));
        KeInitializeEvent(&event, NotificationEvent, FALSE);        
        IoSetCompletionRoutine(tmpIrp, SynchCompletionRoutine, &event, TRUE, TRUE, TRUE);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoCallDriver...\n"));
        status = IoCallDriver(DeviceObject, tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoCallDriver done.\n"));

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoCallDriver failed.\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to wait for event...\n"));
            KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Event got.\n"));
            }            
        }
    

    if (NT_SUCCESS(status))
        {
        // Irp is complete, grab the status and free the irp
        status = tmpIrp->IoStatus.Status;

        if (status == STATUS_SUCCESS)
            {    
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Status OK\n"));
            }
        }


    // Cleanup...
    if (tmpIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoFreeIrp...\n"));
        IoFreeIrp(tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoFreeIrp done.\n"));
        }


    // Dereference the device if out call to IoGetDeviceObjectPointer(...)
    // was successful
    // http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm - says use ObDereferenceObject
    if (FileObject != NULL)
        {
        ObDereferenceObject(FileObject);
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("SendFOTFEDiskDeviceDIOCUnicode\n"));
    return status;
}


// =========================================================================
NTSTATUS
DeriveKey(
    IN      KDF_ALGORITHM KDFAlgorithm,
    IN      CHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      GUID HashGUID,
    IN      CHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
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

    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("DeriveKey\n"));


    status = STATUS_SUCCESS;

    Hash.FnHash    = NULL;
	Cypher.FnEncrypt = NULL;
    Cypher.FnDecrypt = NULL;
	Cypher.FnEncryptSector = NULL;
    Cypher.FnDecryptSector = NULL;

    gotHashDetails = FALSE;
    gotCypherDetails = FALSE;

    // Flags to indicate if we were passed hash/cypher details
    HashFlag   = (strlen(HashDeviceName) > 0);
    CypherFlag = (strlen(CypherDeviceName) > 0);


    // Locate the appropriate HASH algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (HashFlag)
            {
            status = GetDeviceDetailsHash(
                                HashDeviceName,
                                &HashGUID,

                                &Hash
	                            );
            gotHashDetails = (NT_SUCCESS(status));
            }
        }

    // Locate the appropriate CYPHER algorithm (if appropriate)
    if (NT_SUCCESS(status))
        {
        if (CypherFlag)
            {
            status = GetDeviceDetailsCypher_v3_v1(
                                CypherDeviceName,
                                &CypherGUID,

                                &Cypher
                                );
            gotCypherDetails = (NT_SUCCESS(status));
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
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("KDFs can only be called with algorithms that have a defined block & output length, unless hash KDF being used\n"));
            status = STATUS_INVALID_PARAMETER;
            }
        }


    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Processing with algorithm: %d\n", KDFAlgorithm));
        if (KDFAlgorithm == KDF_HASH)
            {
            // Sanity check - we have a hash algorithm, right?
            if (Hash.FnHash == NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Must supply a hash when using KDF_HASH\n"));
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
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Must supply a hash when using KDF_PBKDF2\n"));
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
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unrecognised KDF algorithm!\n"));
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

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("DeriveKey\n"));

    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_Dismount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DISMOUNT DIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    PDEVICE_EXTENSION tgtDevExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_Dismount\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    *QueueDeviceObject = NULL;


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_DISMOUNT))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DISMOUNT),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...
    DIOCBuffer = (PDIOC_DISMOUNT)Irp->AssociatedIrp.SystemBuffer;

    // Locate the device to dismount...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Locating disk device...\n"));
    status = GetNamedCharDevice(
                                DeviceObject->DriverObject,
                                (PCHAR)&DIOCBuffer->DiskDeviceName,
                                &tgtDeviceObj
                                );
                            
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to locate required disk device.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }

    
    // Ensure that the device *is* already mounted...
    // (Returns success if it was already dismounted)
    tgtDevExt = (PDEVICE_EXTENSION)tgtDeviceObj->DeviceExtension;
    if (!(tgtDevExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Cannot dismount device; it's not mounted!\n"));
        status = STATUS_SUCCESS;
        }
    else
        {        
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to flag that IRP should be queued for target device...\n"));
    
        // Prevent any further IRPs from being queued for that disk device.
        tgtDevExt->DismountPending = TRUE;
    
        // If it's an emergency, we cancel all outstanding IRPs queued for that disk device,
        // *before* we queue the dismount IRP
        if (DIOCBuffer->Emergency)
            {
            CancelAllQueuedIRPs(tgtDeviceObj);
            }

        // Queue the dismount request; it's now the next one that will be processed by the
        // disk device in question
        *QueueDeviceObject = tgtDeviceObj;
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_Dismount\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceCount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DISK_DEVICE_COUNT DIOCBuffer;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceCount\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DISK_DEVICE_COUNT))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DISK_DEVICE_COUNT),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDIOC_DISK_DEVICE_COUNT)Irp->AssociatedIrp.SystemBuffer;

    // Count the number of *disk* devices (mounted or not)
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Counting disk devices...\n"));
    count = 0;
    currDevObj = DeviceObject->DriverObject->DeviceObject;
    while (currDevObj!=NULL)
        {
        tgtDevExt = (PDEVICE_EXTENSION)currDevObj->DeviceExtension;
        if (!(tgtDevExt->IsMainDevice))
            {
            count++;
            }
        
        currDevObj = currDevObj->NextDevice;      
        }   // while

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Disk devices count is: %d\n", count));
    DIOCBuffer->Count = count;

    Irp->IoStatus.Information = sizeof(DIOC_DISK_DEVICE_COUNT);
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceCount\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceList(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DEVICE_NAME_LIST outDIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    PDEVICE_EXTENSION tgtDevExt;
    ULONG count;
    PDEVICE_OBJECT currDevObj;
    ULONG inMax;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceList\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    status = STATUS_SUCCESS;


    outDIOCBuffer = (PDIOC_DEVICE_NAME_LIST)Irp->AssociatedIrp.SystemBuffer;

    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access outDIOCBuffer->Data
    // for actual check)
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DEVICE_NAME_LIST)-sizeof(outDIOCBuffer->DeviceName))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_DEVICE_NAME_LIST)-sizeof(outDIOCBuffer->DeviceName),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Determine the max number of device names we can store
    inMax =
       (
         // The size of the buffer, less the array of cypher details
         irpSp->Parameters.DeviceIoControl.OutputBufferLength - 
           (sizeof(DIOC_DEVICE_NAME_LIST)-sizeof(outDIOCBuffer->DeviceName))
       ) /
       // Divide by the size of each cypher details struct to give the array length
       sizeof(outDIOCBuffer->DeviceName);
       
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Request is sufficiently large to store %d disk device details\n", inMax));
    
        
    // Request valid, process...
        
    // Copy all disk device names (mounted or not)
    outDIOCBuffer->DeviceCount = 0;
    currDevObj = DeviceObject->DriverObject->DeviceObject;
    while (currDevObj != NULL)
        {
        tgtDevExt = (PDEVICE_EXTENSION)currDevObj->DeviceExtension;
        
        if (!(tgtDevExt->IsMainDevice))
            {
            outDIOCBuffer->DeviceCount++;
            
            if (outDIOCBuffer->DeviceCount > inMax)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer not large enough to store all device names\n"));
                status = STATUS_INVALID_BUFFER_SIZE;                
                break;
                }

            // Set the output buffer to contain the devicename of the device
            // just located
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying device name to output buffer (%ls).\n", tgtDevExt->zzDeviceName.Buffer));



            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Blanking: %d bytes\n", sizeof(outDIOCBuffer->DeviceName[0])));
        
            RtlUnicodeStringToAnsiString(
                                        &tmpANSIDeviceName,
                                        &tgtDevExt->zzDeviceName,
                                        TRUE
                                        ); 
            // Copy the resulting ANSI string into the output buffer
            RtlZeroMemory(
                          &outDIOCBuffer->DeviceName[(outDIOCBuffer->DeviceCount-1)][0],
                          sizeof(outDIOCBuffer->DeviceName[0])
                         );
            if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->DeviceName[0]))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit device name! (struct has: %d; need: %d\n",
                                sizeof(outDIOCBuffer->DeviceName[0]),
                                tmpANSIDeviceName.Length
                                ));
                status = STATUS_INVALID_BUFFER_SIZE;                
                break;
                }
            else
                {
                RtlCopyMemory(            
                            &outDIOCBuffer->DeviceName[(outDIOCBuffer->DeviceCount-1)][0],
                            tmpANSIDeviceName.Buffer,
                            tmpANSIDeviceName.Length
                            );
                }
            RtlFreeAnsiString(&tmpANSIDeviceName);
            }
        
        currDevObj = currDevObj->NextDevice;      
        }   // while


    if NT_SUCCESS(status)
        {
        Irp->IoStatus.Information = sizeof(DIOC_DEVICE_NAME_LIST)+(outDIOCBuffer->DeviceCount * sizeof(outDIOCBuffer->DeviceName))-sizeof(outDIOCBuffer->DeviceName);
        }
    else
        {
        Irp->IoStatus.Information = 0;
        }
    

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceList\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DEVICE_NAME inDIOCBuffer;
    PDIOC_DISK_DEVICE_STATUS outDIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    ANSI_STRING tmpANSIFilename;
    PDEVICE_EXTENSION tgtDevExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    status = STATUS_SUCCESS;


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_DEVICE_NAME))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DEVICE_NAME),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DISK_DEVICE_STATUS))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DISK_DEVICE_STATUS),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    inDIOCBuffer = (PDIOC_DEVICE_NAME)Irp->AssociatedIrp.SystemBuffer;
    outDIOCBuffer = (PDIOC_DISK_DEVICE_STATUS)Irp->AssociatedIrp.SystemBuffer;

    // Locate the device on which the file is to be mounted...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Locating disk device...\n"));
    status = GetNamedCharDevice(
                                DeviceObject->DriverObject,
                                (PCHAR)&inDIOCBuffer->DeviceName,
                                &tgtDeviceObj
                                );
                            
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to locate required disk device.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }
    
    
    tgtDevExt = (PDEVICE_EXTENSION)tgtDeviceObj->DeviceExtension;
    
    outDIOCBuffer->Mounted           = tgtDevExt->Mounted;
    outDIOCBuffer->DismountPending   = tgtDevExt->DismountPending;
    outDIOCBuffer->ReadOnly          = tgtDevExt->ReadOnly;
    outDIOCBuffer->VolumeFlags       = tgtDevExt->VolumeFlags;
    outDIOCBuffer->SectorIVGenMethod = tgtDevExt->SectorIVGenMethod;
    outDIOCBuffer->MetaDataLength    = tgtDevExt->MetaDataLength;


    
    // Copy the devicename...
    // Set the output buffer to contain the devicename of the device
    // just located
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying device name to output buffer.\n"));                        

    RtlUnicodeStringToAnsiString(
                                &tmpANSIDeviceName,
                                &tgtDevExt->zzDeviceName,
                                TRUE
                                ); 
    // Copy the resulting ANSI string into the buffer
    RtlZeroMemory(
                outDIOCBuffer->DeviceName,
                sizeof(outDIOCBuffer->DeviceName)
                );
    if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->DeviceName))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit DISK device name! (struct has: %d; need: %d\n",
                        sizeof(outDIOCBuffer->DeviceName),
                        tmpANSIDeviceName.Length
                       ));
        status = STATUS_INVALID_BUFFER_SIZE;                
        }
    else
        {
        RtlCopyMemory(            
                    &outDIOCBuffer->DeviceName,
                    tmpANSIDeviceName.Buffer,
                    tmpANSIDeviceName.Length
                    );
        }
    RtlFreeAnsiString(&tmpANSIDeviceName);


    // Filename...
    RtlZeroMemory(
                  outDIOCBuffer->Filename,
                  sizeof(outDIOCBuffer->Filename)
                 );
    if (tgtDevExt->Mounted)
        {
        // Copy the filename...
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying filename to output buffer.\n"));                        
        RtlUnicodeStringToAnsiString(
                                    &tmpANSIFilename,
                                    &tgtDevExt->zzFilename,
                                    TRUE
                                    ); 
        if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->Filename))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit FILENAME! (struct has: %d; need: %d\n",
                            sizeof(outDIOCBuffer->Filename),
                            tmpANSIDeviceName.Length
                        ));
            status = STATUS_INVALID_BUFFER_SIZE;                
            }
        else
            {
            // Copy the resulting ANSI string into the buffer
            RtlCopyMemory(            
                        &outDIOCBuffer->Filename,
                        tmpANSIFilename.Buffer,
                        tmpANSIFilename.Length
                        );
            }
        RtlFreeAnsiString(&tmpANSIFilename);
        }
        

    // IV hash driver...
    RtlZeroMemory(
                  outDIOCBuffer->IVHashDeviceName,
                  sizeof(outDIOCBuffer->IVHashDeviceName)
                 );
    if (tgtDevExt->Mounted)
        {
        outDIOCBuffer->IVHashGUID = tgtDevExt->IVHash.HashGUID;
        
        // Copy the HashDeviceName...
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying HashDeviceName to output buffer.\n"));                        
        RtlUnicodeStringToAnsiString(
                                    &tmpANSIFilename,
                                    &tgtDevExt->IVHash.DeviceName,
                                    TRUE
                                    ); 
        if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->IVHashDeviceName))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit HASH device name! (struct has: %d; need: %d\n",
                            sizeof(outDIOCBuffer->IVHashDeviceName),
                            tmpANSIDeviceName.Length
                        ));
            status = STATUS_INVALID_BUFFER_SIZE;                
            }
        else
            {
            // Copy the resulting ANSI string into the buffer
            RtlCopyMemory(            
                        &outDIOCBuffer->IVHashDeviceName,
                        tmpANSIFilename.Buffer,
                        tmpANSIFilename.Length
                        );
            }
        RtlFreeAnsiString(&tmpANSIFilename);
        }
        

    // IV cypher driver...
    RtlZeroMemory(
                  outDIOCBuffer->IVCypherDeviceName,
                  sizeof(outDIOCBuffer->IVCypherDeviceName)
                 );
    if (tgtDevExt->Mounted)
        {
        outDIOCBuffer->IVCypherGUID = tgtDevExt->IVCypher.CypherGUID;

        // Copy the CypherDeviceName...
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying IVCypherDeviceName to output buffer.\n"));                        
        RtlUnicodeStringToAnsiString(
                                    &tmpANSIFilename,
                                    &tgtDevExt->IVCypher.DeviceName,
                                    TRUE
                                    ); 
        if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->IVCypherDeviceName))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit CYPHER device name! (struct has: %d; need: %d\n",
                            sizeof(outDIOCBuffer->IVCypherDeviceName),
                            tmpANSIDeviceName.Length
                        ));
            status = STATUS_INVALID_BUFFER_SIZE;                
            }
        else
            {
            // Copy the resulting ANSI string into the buffer
            RtlCopyMemory(            
                        &outDIOCBuffer->IVCypherDeviceName,
                        tmpANSIFilename.Buffer,
                        tmpANSIFilename.Length
                        );
            }
        RtlFreeAnsiString(&tmpANSIFilename);
        }
        
        
    // Main cypher driver...
    RtlZeroMemory(
                  outDIOCBuffer->MainCypherDeviceName,
                  sizeof(outDIOCBuffer->MainCypherDeviceName)
                 );
    if (tgtDevExt->Mounted)
        {
        outDIOCBuffer->MainCypherGUID = tgtDevExt->MainCypher.CypherGUID;

        // Copy the CypherDeviceName...
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Copying MainCypherDeviceName to output buffer.\n"));                        
        RtlUnicodeStringToAnsiString(
                                    &tmpANSIFilename,
                                    &tgtDevExt->MainCypher.DeviceName,
                                    TRUE
                                    ); 
        if (tmpANSIDeviceName.Length > sizeof(outDIOCBuffer->MainCypherDeviceName))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Device name too large to fit CYPHER device name! (struct has: %d; need: %d)\n",
                            sizeof(outDIOCBuffer->MainCypherDeviceName),
                            tmpANSIDeviceName.Length
                        ));
            status = STATUS_INVALID_BUFFER_SIZE;                
            }
        else
            {
            // Copy the resulting ANSI string into the buffer
            RtlCopyMemory(            
                        &outDIOCBuffer->MainCypherDeviceName,
                        tmpANSIFilename.Buffer,
                        tmpANSIFilename.Length
                        );
            }
        RtlFreeAnsiString(&tmpANSIFilename);
        }
        
        
    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = sizeof(DIOC_DISK_DEVICE_STATUS);
        }
    else
        {
        Irp->IoStatus.Information = 0;
        }
        

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceMetaData(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DEVICE_NAME inDIOCBuffer;
    PDIOC_DISK_DEVICE_METADATA outDIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    ANSI_STRING tmpANSIFilename;
    PDEVICE_EXTENSION tgtDevExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceMetaData\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    status = STATUS_SUCCESS;


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_DEVICE_NAME))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DEVICE_NAME),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_DISK_DEVICE_METADATA))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_DISK_DEVICE_METADATA),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Request valid, process...
    inDIOCBuffer = (PDIOC_DEVICE_NAME)Irp->AssociatedIrp.SystemBuffer;
    outDIOCBuffer = (PDIOC_DISK_DEVICE_METADATA)Irp->AssociatedIrp.SystemBuffer;

    // Locate the device on which the file is to be mounted...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Locating disk device...\n"));
    status = GetNamedCharDevice(
                                DeviceObject->DriverObject,
                                (PCHAR)&inDIOCBuffer->DeviceName,
                                &tgtDeviceObj
                                );
                            
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to locate required disk device.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }
    
    
    tgtDevExt = (PDEVICE_EXTENSION)tgtDeviceObj->DeviceExtension;


    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            (
             sizeof(DIOC_DISK_DEVICE_METADATA) + 
             tgtDevExt->MetaDataLength - 
             sizeof(outDIOCBuffer->MetaData)
            )
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_DISK_DEVICE_METADATA) + 
             tgtDevExt->MetaDataLength - 
             sizeof(outDIOCBuffer->MetaData)
            ),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Metadata being returned consists of %d bytes\n", tgtDevExt->MetaDataLength));

    outDIOCBuffer->MetaDataLength = tgtDevExt->MetaDataLength;
    RtlCopyMemory(            
                &outDIOCBuffer->MetaData,
                tgtDevExt->MetaData,
                tgtDevExt->MetaDataLength
                );

        
    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = (
                                    sizeof(DIOC_DISK_DEVICE_METADATA) + 
                                    tgtDevExt->MetaDataLength - 
                                    sizeof(outDIOCBuffer->MetaData)
                                    );
        }
    else
        {
        Irp->IoStatus.Information = 0;
        }
        

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEIOCTL_GetDiskDeviceMetaData\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskFormatTracks(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DISK_DEVICE_COUNT DIOCBuffer;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskFormatTracks\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    // Do nothing - status is already STATUS_SUCCESS
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskFormatTracks\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskIsWritable(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_DISK_DEVICE_COUNT DIOCBuffer;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskIsWritable\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;
    

    // Default to the drive being writable
    status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;
    
    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        }
    else if (devExt->ReadOnly)
        {
        status = STATUS_MEDIA_WRITE_PROTECTED;
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskIsWritable\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskGetDriveGeometry(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PDISK_GEOMETRY DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetDriveGeometry\n"));

    devExt = DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DISK_GEOMETRY))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DISK_GEOMETRY),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDISK_GEOMETRY)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }
        
    RtlCopyMemory(            
                  DIOCBuffer,
                  &devExt->DiskGeometry,
                  sizeof(DISK_GEOMETRY)
                 );

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Reporting disk geometry as:\n"));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Cylinders        : %lld\n", DIOCBuffer->Cylinders.QuadPart));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- MediaType        : %d\n", DIOCBuffer->MediaType));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- TracksPerCylinder: %d\n", DIOCBuffer->TracksPerCylinder));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- SectorsPerTrack  : %d\n", DIOCBuffer->SectorsPerTrack));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- BytesPerSector   : %d\n", DIOCBuffer->BytesPerSector));

    Irp->IoStatus.Information = sizeof(DISK_GEOMETRY);
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetDriveGeometry\n"));
    return status;
}


// =========================================================================
// 
#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetLengthInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PGET_LENGTH_INFORMATION DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetLengthInfo\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(GET_LENGTH_INFORMATION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(GET_LENGTH_INFORMATION),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PGET_LENGTH_INFORMATION)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }
    DIOCBuffer->Length.QuadPart = devExt->PartitionSize.QuadPart;

    Irp->IoStatus.Information = sizeof(GET_LENGTH_INFORMATION);
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetLengthInfo\n"));
    return status;
}
#endif


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskGetPartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PPARTITION_INFORMATION DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetPartitionInfo\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(PARTITION_INFORMATION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(PARTITION_INFORMATION),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PPARTITION_INFORMATION)Irp->AssociatedIrp.SystemBuffer;


#if (VER_PRODUCTBUILD >= 2600)
    // Windows XP and later only
    status = PopulatePartitionInfo(
                DeviceObject,
                DIOCBuffer,
                NULL
                );
#else
    status = PopulatePartitionInfo(
                DeviceObject,
                DIOCBuffer
                );
#endif

    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = sizeof(PARTITION_INFORMATION);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetPartitionInfo\n"));
    return status;
}


// =========================================================================
// 
#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetPartitionInfoEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PPARTITION_INFORMATION_EX DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetPartitionInfoEx\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(PARTITION_INFORMATION_EX))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(PARTITION_INFORMATION_EX),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PPARTITION_INFORMATION_EX)Irp->AssociatedIrp.SystemBuffer;


    status = PopulatePartitionInfo(
                DeviceObject,
                NULL,
                DIOCBuffer
                );

    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = sizeof(PARTITION_INFORMATION_EX);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetPartitionInfoEx\n"));
    return status;
}
#endif


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskGetDriveLayout(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PDRIVE_LAYOUT_INFORMATION DIOCBuffer;
    PDEVICE_EXTENSION devExt;
    ULONG outputSize;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetDriveLayout\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DRIVE_LAYOUT_INFORMATION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DRIVE_LAYOUT_INFORMATION),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Actual size of OUTPUT buffer
    // We have exactly one partition
    outputSize = (sizeof(DRIVE_LAYOUT_INFORMATION) + sizeof(PARTITION_INFORMATION));
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            outputSize )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            outputSize,
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_BUFFER_TOO_SMALL;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDRIVE_LAYOUT_INFORMATION)Irp->AssociatedIrp.SystemBuffer;


    DIOCBuffer->PartitionCount = 1;
    DIOCBuffer->Signature      = 0;
#if (VER_PRODUCTBUILD >= 2600)
    // Windows XP and later only
    status = PopulatePartitionInfo(
                DeviceObject,
                &(DIOCBuffer->PartitionEntry[0]),
                NULL
                );
#else
    status = PopulatePartitionInfo(
                DeviceObject,
                &(DIOCBuffer->PartitionEntry[0])
                );
#endif

    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = outputSize;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetDriveLayout\n"));
    return status;
}


// =========================================================================
// 
#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetDriveLayoutEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PDRIVE_LAYOUT_INFORMATION_EX DIOCBuffer;
    PDEVICE_EXTENSION devExt;
    ULONG outputSize;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskGetDriveLayoutEx\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DRIVE_LAYOUT_INFORMATION_EX))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DRIVE_LAYOUT_INFORMATION_EX),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Actual size of OUTPUT buffer
    // We have exactly one partition
    outputSize = (sizeof(DRIVE_LAYOUT_INFORMATION_EX) + sizeof(PARTITION_INFORMATION));
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            outputSize )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            outputSize,
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_BUFFER_TOO_SMALL;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDRIVE_LAYOUT_INFORMATION_EX)Irp->AssociatedIrp.SystemBuffer;


    DIOCBuffer->PartitionStyle = PARTITION_STYLE_MBR;
    DIOCBuffer->PartitionCount = 1;
    DIOCBuffer->Mbr.Signature = 0;

    status = PopulatePartitionInfo(
                DeviceObject,
                NULL,
                &(DIOCBuffer->PartitionEntry[0])
                );

    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = outputSize;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskGetDriveLayoutEx\n"));
    return status;
}
#endif


// =========================================================================
// Populate PARTITION_INFORMATION/PARTITION_INFORMATION_EX structures
// PartitionInfo - This may be set to NULL if it is not needed
// PartitionInfoEx - This may be set to NULL if it is not needed
#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
// Populate PARTITION_INFORMATION/PARTITION_INFORMATION_EX structures
NTSTATUS
PopulatePartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    OUT PPARTITION_INFORMATION PartitionInfo,
    OUT PPARTITION_INFORMATION_EX PartitionInfoEx
)
#else
// Populate PARTITION_INFORMATION/PARTITION_INFORMATION_EX structures
NTSTATUS
PopulatePartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    OUT PPARTITION_INFORMATION PartitionInfo
)
#endif
{
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("PopulatePartitionInfo\n"));

    devExt = DeviceObject->DeviceExtension;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }

    if (PartitionInfo != NULL)
        {                    
        PartitionInfo->HiddenSectors = devExt->HiddenSectors;
        PartitionInfo->StartingOffset.QuadPart = PartitionInfo->HiddenSectors * devExt->DiskGeometry.BytesPerSector;
        PartitionInfo->PartitionLength.QuadPart = devExt->PartitionSize.QuadPart;
        // Partition number is 1-based for HDDs, 0-based for CDROMs/DVDs
        PartitionInfo->PartitionNumber = 1;
        if (EmulatingCDOrDVD(DeviceObject))
            {
            PartitionInfo->PartitionNumber = 0;
            }
        // The type of the partition can't really be determined, so we
        // shrug and say "PARTITION_ENTRY_UNUSED"
        PartitionInfo->PartitionType = PARTITION_ENTRY_UNUSED;
        PartitionInfo->BootIndicator = FALSE;
        // The partition *is* recognised as a partition...
        PartitionInfo->RecognizedPartition = TRUE;
        PartitionInfo->RewritePartition = FALSE;


        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Reporting partition information as:\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- StartingOffset     : %lld\n", PartitionInfo->StartingOffset.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionLength    : %lld\n", PartitionInfo->PartitionLength.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- HiddenSectors      : %d\n", PartitionInfo->HiddenSectors));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionNumber    : %d\n", PartitionInfo->PartitionNumber));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionType      : %d\n", PartitionInfo->PartitionType));

        if (PartitionInfo->BootIndicator)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- BootIndicator      : TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- BootIndicator      : FALSE\n"));
            }

        if (PartitionInfo->RecognizedPartition)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RecognizedPartition: TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RecognizedPartition: FALSE\n"));
            }

        if (PartitionInfo->RewritePartition)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RewritePartition   : TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RewritePartition   : FALSE\n"));
            }

        status = STATUS_SUCCESS;
        }


#if (VER_PRODUCTBUILD >= 2600)
    // Windows XP and later only
    if (PartitionInfoEx != NULL)
        {                    
        PartitionInfoEx->PartitionStyle = PARTITION_STYLE_MBR;

        PartitionInfoEx->Mbr.HiddenSectors = devExt->HiddenSectors;
        PartitionInfoEx->StartingOffset.QuadPart = devExt->HiddenSectors * devExt->DiskGeometry.BytesPerSector;
        PartitionInfoEx->PartitionLength.QuadPart = devExt->PartitionSize.QuadPart;
        // Partition number is 1-based for HDDs, 0-based for CDROMs/DVDs
        PartitionInfoEx->PartitionNumber = 1;
        if (EmulatingCDOrDVD(DeviceObject))
            {
            PartitionInfoEx->PartitionNumber = 0;
            }
        PartitionInfoEx->RewritePartition = FALSE;
        // The type of the partition can't really be determined, so we
        // shrug and say "PARTITION_ENTRY_UNUSED"
        PartitionInfoEx->Mbr.PartitionType = PARTITION_ENTRY_UNUSED;
        PartitionInfoEx->Mbr.BootIndicator = FALSE;
        // The partition *is* recognised as a partition...
        PartitionInfoEx->Mbr.RecognizedPartition = TRUE;


        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Reporting partition information**EX** as:\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionStyle  : %d (%d is PARTITION_STYLE_MBR)\n", PartitionInfoEx->PartitionStyle, PARTITION_STYLE_MBR));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- StartingOffset  : %lld\n", PartitionInfoEx->StartingOffset.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionLength : %lld\n", PartitionInfoEx->PartitionLength.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionNumber : %d\n", PartitionInfoEx->PartitionNumber));
        if (PartitionInfoEx->RewritePartition)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RewritePartition: TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- RewritePartition: FALSE\n"));
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- MBR             : \n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       PartitionType      : %d\n", PartitionInfoEx->Mbr.PartitionType));
        if (PartitionInfoEx->Mbr.BootIndicator)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       BootIndicator      : TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       BootIndicator      : FALSE\n"));
            }
        if (PartitionInfoEx->Mbr.RecognizedPartition)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       RecognizedPartition: TRUE\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       RecognizedPartition: FALSE\n"));
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("                       HiddenSectors      : %d\n", PartitionInfoEx->Mbr.HiddenSectors));

        status = STATUS_SUCCESS;
        }
#endif

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("PopulatePartitionInfo\n"));

    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskSetPartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PSET_PARTITION_INFORMATION DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskSetPartitionInfo\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;
    

    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(SET_PARTITION_INFORMATION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(SET_PARTITION_INFORMATION),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
        
    if (devExt->ReadOnly)
        {
        status = STATUS_MEDIA_WRITE_PROTECTED;
        return status;
        }     
                    
    // Request valid, process...
    DIOCBuffer = (PSET_PARTITION_INFORMATION)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        status = STATUS_UNSUCCESSFUL;
        return status;
        }

    // Ummm... We don't *actually* do anything; just return success...
    DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Not actually DOING anything about this IOCTL - just return success\n"));

    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskSetPartitionInfo\n"));
    return status;
}


// =========================================================================
// 
#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskSetPartitionInfoEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PSET_PARTITION_INFORMATION_EX DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskSetPartitionInfoEx\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(SET_PARTITION_INFORMATION_EX))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(SET_PARTITION_INFORMATION_EX),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
        
    if (devExt->ReadOnly)
        {
        status = STATUS_MEDIA_WRITE_PROTECTED;
        return status;
        }     
                    
    // Request valid, process...
    DIOCBuffer = (PSET_PARTITION_INFORMATION_EX)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        status = STATUS_UNSUCCESSFUL;
        return status;
        }

    // Ummm... We don't *actually* do anything; just return success...
    DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Not actually DOING anything about this IOCTL - just return success\n"));
    
    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskSetPartitionInfoEx\n"));
    return status;
}
#endif


// =========================================================================
// 
NTSTATUS
IOCTL_Std_DiskVerify(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PVERIFY_INFORMATION DIOCBuffer;
    PDEVICE_EXTENSION devExt;
    ULONGLONG bytesRemaining;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_DiskVerify\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(VERIFY_INFORMATION))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(VERIFY_INFORMATION),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
        
    // Request valid, process...
    DIOCBuffer = (PVERIFY_INFORMATION)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }


    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;
    // Perform a bounds check on the sector range
    // We have "-1" as an the size is the total size (*size* indexes from 1), but the
    // offset indexes from 0
    if ((DIOCBuffer->StartingOffset.QuadPart > (devExt->PartitionSize.QuadPart-1)) ||
        (DIOCBuffer->StartingOffset.QuadPart < 0))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Requested sector starting outside partition\n"));
        status = STATUS_NONEXISTENT_SECTOR;
        }
    else
        {
        // We do *not* have "-1" here as bytesRemaining should be the total number of bytes
        // after the offset supplied
        bytesRemaining = devExt->PartitionSize.QuadPart - DIOCBuffer->StartingOffset.QuadPart;

        if (DIOCBuffer->Length > bytesRemaining)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Requested sector going beyond partition\n"));
            status = STATUS_NONEXISTENT_SECTOR;
            }
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_DiskVerify\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_CheckVerify(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PULONG DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_CheckVerify\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }


    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;

    // Check size of OUTPUT buffer
    // Note that an output buffer is *optional*
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength >=
            sizeof(ULONG))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...Also wants the count of times the media has changed; simulating 0 times.\n"));
        
        DIOCBuffer = (PULONG)Irp->AssociatedIrp.SystemBuffer;
        
        // Simulate zero media changes
        *DIOCBuffer = 0;
        Irp->IoStatus.Information = sizeof(ULONG);
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_CheckVerify\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_MediaRemoval(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PPREVENT_MEDIA_REMOVAL DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_MediaRemoval\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // These two are only really valid for removable drives
    // They set/clear the media's ability to eject


    // Check size of INPUT buffer
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(PREVENT_MEDIA_REMOVAL))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(PREVENT_MEDIA_REMOVAL),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        

    // Request valid, process...
    DIOCBuffer = (PPREVENT_MEDIA_REMOVAL)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }
        
    // Store the request
    // NOTE: We don't want to prevent the user from carrying out
    //       an emergency dismount!
    devExt->PreventMediaRemoval = DIOCBuffer->PreventMediaRemoval;
    
    Irp->IoStatus.Information = 0;
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_MediaRemoval\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_StorageEjectMedia(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_StorageEjectMedia\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // These two are only really valid for removable drives
    // Equivilent of dismount


    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_NO_MEDIA_IN_DEVICE;
        return status;
        }
        
    // NOTE: This doesn't prevent the user from carrying out
    //       an emergency dismount via the FreeOTFE IOCTL, just if
    //       they try using the device eject mechanism
    if (devExt->PreventMediaRemoval)
        {
        status = STATUS_INVALID_DEVICE_REQUEST;
        }
    else
        {
        // Queue for dismount
        *QueueDeviceObject = DeviceObject;
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_StorageEjectMedia\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_Std_CDROMReadTOC(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   tgtDevExt;
    ULONG count;
    PCDROM_TOC DIOCBuffer;
    PDEVICE_EXTENSION devExt;
    LARGE_INTEGER totalDataSectors;
    LARGE_INTEGER startDataSector;
    LARGE_INTEGER tmpSector;
    unsigned int tmpLen;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("IOCTL_Std_CDROMReadTOC\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);
    devExt = DeviceObject->DeviceExtension;


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(CDROM_TOC))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(CDROM_TOC),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        

    // Request valid, process...
    DIOCBuffer = (PCDROM_TOC)Irp->AssociatedIrp.SystemBuffer;

    if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        return status;
        }

    // The total number of emulated sectors the track has
    totalDataSectors.QuadPart = devExt->PartitionSize.QuadPart / devExt->DiskGeometry.BytesPerSector;

    // The first data track...
    DIOCBuffer->TrackData[0].Control = 4;  // Data track
    DIOCBuffer->TrackData[0].Adr     = 1;
    DIOCBuffer->TrackData[0].TrackNumber = 1;
    // ...Two second lead-in prior to the data track (150 frames)
    DIOCBuffer->TrackData[0].Address[0] = 0;   
    DIOCBuffer->TrackData[0].Address[1] = 0;  // MSF - Minutes
    DIOCBuffer->TrackData[0].Address[2] = 2;  // MSF - Seconds (Fixed at 2)
    DIOCBuffer->TrackData[0].Address[3] = 0;  // MSF - Frames

    // The lead-out...
    DIOCBuffer->TrackData[1].Control = 4;  // Data track
    DIOCBuffer->TrackData[1].Adr     = 1;
    DIOCBuffer->TrackData[1].TrackNumber = 0xAA;  // Lead-out indicator
    DIOCBuffer->TrackData[1].Address[0] = 0;
    // ...The address where the lead-out starts from (in sectors)
    startDataSector.QuadPart = totalDataSectors.QuadPart +
                                // Add on the sectors used by the lead-in
                                + (DIOCBuffer->TrackData[0].Address[1] * CD_FRAMES_PER_MINUTE)
                                + (DIOCBuffer->TrackData[0].Address[2] * CD_FRAMES_PER_SECOND)
                                + (DIOCBuffer->TrackData[0].Address[3]);
    // Convert to MSF (Minutes/Seconds/Frames) format required by the structure
    // One frame == one sector

    // Note: The "modulus problem" or "divide problem" with MS Windows 2000:
    //       With 64 bit numbers the following:
    //
    //         x = a % b
    //
    //       can cause problems when the resulting binary is used under MS Windows 2000.
    //       Specifically, when the driver is loaded/installed, Windows 2000 comes up with the
    //       following error:
    //
    //         "The <driver filename> device driver could not locate the entry point _alldvrm
    //         in driver ntoskrnl.exe"
    //
    //       This problem is only seen in MS Windows 2000 and when running the "free" (release)
    //       version of the built software - the "checked" (debug) version works OK(?)
    //       It is caused by the MS Windows Server 2003 DDK optimising the resulting binary to
    //       use "_alldvrm" - which doesn't exist in Windows 2000.
    //
    //       Solution: By rewriting the this line in the following manner:
    //
    //         tmp = a / b
    //         x = a - (tmp * b)
    //
    //       The compiled binary will work correctly.
    //
    //       See USENET post:
    //       Newsgroup: comp.os.ms-windows.programmer.nt.kernel-mode:
    //       Message-ID: <24b0a2da.0208211128.a414da9@posting.google.com>
    //       Date: 21 Aug 2002 12:28:31 -0700
    //       From: Gianluca Varenni
    //       which is archived on google at:
    //       http://groups-beta.google.com/group/comp.os.ms-windows.programmer.nt.kernel-mode/msg/d008e13f3c0c6ef6?dmode=source

    

    // Yes, this is correct.
    // The number of minutes is the total size div the frams per min.
    tmpSector.QuadPart = startDataSector.QuadPart;
    // Note: Address[1] == MSF Minutes
    DIOCBuffer->TrackData[1].Address[1] = (UCHAR)(tmpSector.QuadPart / CD_FRAMES_PER_MINUTE);

    // Yes, this is correct.
    // We take off the number of sectors represented in the previous calculation by using
    // mod...
    // (Important: See "modulus problem" above for explanation re the following INTENTIONALLY
    // COMMENTED OUT/REWRITTEN line)
    // tmpSector.QuadPart = tmpSector.QuadPart % CD_FRAMES_PER_MINUTE;
    tmpSector.QuadPart = tmpSector.QuadPart - (DIOCBuffer->TrackData[1].Address[1] * CD_FRAMES_PER_MINUTE);
    // ...then div by frames per second to give the number of *whole* seconds
    // Note: Address[2] == MSF Seconds
    DIOCBuffer->TrackData[1].Address[2] = (UCHAR)(tmpSector.QuadPart / CD_FRAMES_PER_SECOND);

    // Yes, this is correct.
    // We take off the number of sectors represented in the previous calculation by using
    // mod...
    // (Important: See "modulus problem" above for explanation re the following INTENTIONALLY
    // COMMENTED OUT/REWRITTEN line)
    // tmpSector.QuadPart = tmpSector.QuadPart % CD_FRAMES_PER_SECOND;
    tmpSector.QuadPart = tmpSector.QuadPart - (DIOCBuffer->TrackData[1].Address[2] * CD_FRAMES_PER_SECOND);
    // ...then div by frames per second to give the number of *frames* remaining
    // Note: Address[3] == MSF Frames
    DIOCBuffer->TrackData[1].Address[3] = (UCHAR)tmpSector.QuadPart;

    // FWIW, the following a TRUE STATEMENT:
    // startDataSector.QuadPart == 
    //                              (DIOCBuffer->TrackData[1].Address[1] * CD_FRAMES_PER_MINUTE)
    //                            + (DIOCBuffer->TrackData[1].Address[2] * CD_FRAMES_PER_SECOND)
    //                            + (DIOCBuffer->TrackData[1].Address[3])

    tmpLen = sizeof(*DIOCBuffer)                     // Size of the buffer
                - sizeof(DIOCBuffer->Length)         // Don't include the length of
                                                     // "Length" member
                - sizeof(DIOCBuffer->TrackData)      // The struct defines this as
                                                     // large enough to hold the max
                                                     // number of tracks (100), but
                                                     // we only use 2
                + sizeof(DIOCBuffer->TrackData[0])   // Size of the first tracks's
                                                     // data
                + sizeof(DIOCBuffer->TrackData[1]);  // Size of the second tracks's
    DIOCBuffer->Length[0] = tmpLen & 0xFF00;
    DIOCBuffer->Length[1] = tmpLen & 0x00FF;
    DIOCBuffer->FirstTrack = 1;
    DIOCBuffer->LastTrack  = 1;


    Irp->IoStatus.Information = tmpLen + sizeof(DIOCBuffer->Length); // We include the length
                                                                     // of "Length" member this
                                                                     // time
    status = STATUS_SUCCESS;


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("IOCTL_Std_CDROMReadTOC\n"));
    return status;
}


// =========================================================================
// Handle read IRPs
NTSTATUS
FreeOTFE_MF_DispatchRead(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    PIO_STACK_LOCATION irpSp;
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchRead\n"));
    
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    // If the device in the process of being removed, or isn't mounted,
    // fail the request
    if ( (devExt->TerminateThread) || (devExt->DismountPending) )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Delete pending - returning no success\n"));
        status = STATUS_DELETE_PENDING;
        Irp->IoStatus.Information = 0;
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;        
        }    
    else if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        Irp->IoStatus.Information = 0;
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;
        }
      
    irpSp = IoGetCurrentIrpStackLocation(Irp);
      
      
    // Ensure that the IRP is valid (isn't trying to read past the length
    // of the drive
    // Also ensures the number of bytes being read is a multiple of the sector size
    // Note: It's ">" and not ">=" (e.g. reading 512 bytes on a 512 byte disk, starting from
    //       offset 0)
    if ( 
         ((irpSp->Parameters.Read.ByteOffset.QuadPart +
           irpSp->Parameters.Read.Length)
                   > devExt->PartitionSize.QuadPart) ||
          ((irpSp->Parameters.Read.Length &
           (devExt->DiskGeometry.BytesPerSector - 1)) != 0 )
       ) {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Invalid request; rejecting.\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Request:\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Starting offset      : %lld\n", irpSp->Parameters.Read.ByteOffset.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  I/O Length           : %d\n", irpSp->Parameters.Read.Length));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Device PartitionSize : %lld\n", devExt->PartitionSize.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Device BytesPerSector: %d\n", devExt->DiskGeometry.BytesPerSector));

        status = STATUS_INVALID_PARAMETER;
    } else {
        // Verify that user is really expecting some I/O operation to
        // occur.
        if (irpSp->Parameters.Read.Length != 0)
            {
            // Queue request to thread.
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queueing IRP.\n"));
            // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
            status = QueueIRPToThread(DeviceObject, Irp);
            }
        else
            {
            // Complete this zero length request with no boost.
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Zero length request. Completing IRP.\n"));
            Irp->IoStatus.Information = 0;
            status = STATUS_SUCCESS;
            }
        }


    if (status != STATUS_PENDING)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Completing IRP with status: 0x%x\n", status));
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        }
    

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchRead\n"));
    
    return status;
}


// =========================================================================
// Handle write IRPs
NTSTATUS
FreeOTFE_MF_DispatchWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    PIO_STACK_LOCATION irpSp;
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchWrite\n"));
    
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    // If the device in the process of being removed, or isn't mounted,
    // fail the request
    if ( (devExt->TerminateThread) || (devExt->DismountPending) )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Delete pending - returning no success\n"));
        status = STATUS_DELETE_PENDING;
        Irp->IoStatus.Information = 0;
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;        
        }    
    else if (!(devExt->Mounted))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Not mounted - returning no success\n"));
        status = STATUS_DEVICE_NOT_READY;
        Irp->IoStatus.Information = 0;
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;
        }
        
        
    // Cannot write to a device mounted as readonly
    if (devExt->ReadOnly)
        {
        status = STATUS_ACCESS_DENIED;
        Irp->IoStatus.Information = 0;
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;        
        }    


      
    irpSp = IoGetCurrentIrpStackLocation(Irp);
      
      
    // Ensure that the IRP is valid (isn't trying to write past the length
    // of the drive
    // Also ensures the number of bytes being read is a multiple of the sector size
    // Note: It's ">" and not ">=" (e.g. reading 512 bytes on a 512 byte disk, starting from
    //       offset 0)
    if ( 
         ((irpSp->Parameters.Write.ByteOffset.QuadPart +
           irpSp->Parameters.Write.Length)
                   > devExt->PartitionSize.QuadPart) ||
          ((irpSp->Parameters.Write.Length &
           (devExt->DiskGeometry.BytesPerSector - 1)) != 0 )
       ) {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Invalid request; rejecting.\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Request:\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Starting offset      : %lld\n", irpSp->Parameters.Write.ByteOffset.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  I/O Length           : %d\n", irpSp->Parameters.Write.Length));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Device PartitionSize : %lld\n", devExt->PartitionSize.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Device BytesPerSector: %d\n", devExt->DiskGeometry.BytesPerSector));

        status = STATUS_INVALID_PARAMETER;
    } else {
        // Verify that user is really expecting some I/O operation to
        // occur.
        if (irpSp->Parameters.Write.Length != 0)
            {
            // Queue request to thread.
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queueing IRP.\n"));
            // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
            status = QueueIRPToThread(DeviceObject, Irp);
            }
        else
            {
            // Complete this zero length request with no boost.
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Zero length request. Completing IRP.\n"));
            Irp->IoStatus.Information = 0;
            status = STATUS_SUCCESS;
            }
        }


    if (status != STATUS_PENDING)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Completing IRP with status: 0x%x\n", status));
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        }
    

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchWrite\n"));
    
    return status;
}


// =========================================================================
// Handle flush buffers IRPs
NTSTATUS
FreeOTFE_MF_DispatchFlushBuffers(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchDispatchFlushBuffers\n"));
    
    // We have to wait for any IRPs remaining in the queue to complete, before
    // we can complete this IRP.
    // To do this, we simply queue the IRP; when it's processed from the queue
    // which will be when all prior IRPs have been completed (flushed), it 
    // flushes the volume file and completes.

    // Queue request to thread.
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queueing IRP.\n"));
    // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
    status = QueueIRPToThread(DeviceObject, Irp);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchDispatchFlushBuffers\n"));
    return status;
}


// =========================================================================
// Handle power IRPs
NTSTATUS
FreeOTFE_MF_DispatchPower(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = Irp->IoStatus.Status;
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;
    POWER_STATE_TYPE type;
    POWER_STATE state;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchPower\n"));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);
    
    type = irpSp->Parameters.Power.Type;
    state = irpSp->Parameters.Power.State;

    switch(irpSp->MinorFunction)
        {
        case IRP_MN_QUERY_POWER:
            {
            BOOLEAN preventPowerChange = FALSE;
            PDEVICE_OBJECT currDevObj;
    
        
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IRP_MN_QUERY_POWER: Type: %d; State: %d\n", type, state));


            // Check if shutdown...
            if ((type == SystemPowerState) &&
                    (state.SystemState > PowerSystemHibernate))
                {
                // This is a shutdown request. Pass that.
                // xxxop - ensure all devices are dismounted                
                status = STATUS_SUCCESS;
                break;
                }

            // Check if there are mounted devices...
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Searching for mounted devices...\n"));
            currDevObj = DeviceObject->DriverObject->DeviceObject;       
            preventPowerChange = FALSE;
            while (currDevObj!=NULL)
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Device found, checking if it's a mounted FreeOTFE disk device...\n"));
                devExt = (PDEVICE_EXTENSION)(currDevObj->DeviceExtension);

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Device is: %ls\n", devExt->zzDeviceName.Buffer));
                if (devExt->Mounted)
                    {
                    preventPowerChange = TRUE;
                    break;
                    }
            
                }

            if (preventPowerChange)
                {      
                // You want to standby, hibernate or something like that while
                // one or more volumes is mounted? No chance!
                // Hibernation could cause the encryption passwords, etc to be
                // written to disk; that's a security hazard.
                // Standby, etc? Well, if the system's going into standby,
                // this implies that the user has been away (or is going away,
                // in case the user hit their "standby" button). Again, a
                // security hazard.
                // Encrypted volumes must be dismounted first.
                DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Power query: Failing it\n"));

                PoStartNextPowerIrp(Irp);
                Irp->IoStatus.Information = 0;
                Irp->IoStatus.Status = STATUS_DEVICE_NOT_READY;
                IoCompleteRequest(Irp, IO_NO_INCREMENT);            
                return STATUS_DEVICE_NOT_READY;          
                }
            
            status = STATUS_SUCCESS;
            break;
            }
      

        case IRP_MN_SET_POWER:
            {
            // xxxop - fail IF THERE ARE MOUNTED DEVICES
            // xxxop - otherwise, permit
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IRP_MN_SET_POWER: Type: %d; State: %d\n", type, state));
            // xxxop - implement sensibly
            status = STATUS_SUCCESS;
            break;
            }
            

        default:
            {
            break;
            }
            
        }  // switch


    PoStartNextPowerIrp(Irp);
//xxxop - needed???    IoSkipCurrentIrpStackLocation(Irp);
//xxxop - needed???    status = PoCallDriver(devExt->TargetObject, Irp);

    
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchPower\n"));

    return status;
}


// =========================================================================
// Handle system control IRPs
NTSTATUS
FreeOTFE_MF_DispatchSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchSystemControl\n"));
// TODOzzz: implement
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchSystemControl\n"));
    return STATUS_NOT_IMPLEMENTED;
}


// =========================================================================
// Create the main device object; the one which user code will normally
// talk to when creating new devices, carrying out general driver
// queries, etc
// DeviceObject will be set to the newly created device object
NTSTATUS
CreateMainDevice (
    IN PDRIVER_OBJECT DriverObject,
    OUT PDEVICE_OBJECT *DeviceObject
    )
{
    NTSTATUS status;
    UNICODE_STRING devName;
    PDEVICE_EXTENSION devExtension;
    HANDLE threadHandle;
    PDEVICE_OBJECT devObj;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("CreateMainDevice\n"));

    ASSERT(DriverObject != NULL);


    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Building device name...\n"));
    // We subtract a sizeof(WCHAR) to discount the #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf
    // adds one on anyway
    devName.MaximumLength = sizeof(DEVICE_MAIN_NAME) + sizeof(WCHAR);
    devName.Buffer = FREEOTFE_MEMALLOC(devName.MaximumLength);    
    RtlZeroMemory(devName.Buffer, devName.MaximumLength);
    
    devName.Length = (USHORT)swprintf(devName.Buffer, DEVICE_MAIN_NAME);
    // swprintf returns the number of WCHARs, not the length in bytes
    devName.Length = devName.Length * sizeof(WCHAR);
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("xdevNameLength: %d\n",
                             devName.Length));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("xdevNameMaximumLength: %d\n",
                             devName.MaximumLength));

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to create device: %ls\n", devName.Buffer));
    // Hmmm... I'd prefer to use FILE_DEVICE_UNKNOWN, but since
    // IOCTL control codes pass through the device type, and we just
    // pass then through... 
    // (See other call to IoCreateDevice in this file for details why we *don't*
    // use FILE_DEVICE_VIRTUAL_DISK)
    status = IoCreateDevice(
                            DriverObject,
                            sizeof(DEVICE_EXTENSION),
                            &devName,
                            FILE_DEVICE_UNKNOWN,
                            0,
                            FALSE,
                            &devObj
                           );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        return status;
        }

    devExtension = (PDEVICE_EXTENSION)devObj->DeviceExtension;
    // Purging the device extension memory sets all pointers to NULL
    RtlZeroMemory(devExtension, sizeof(DEVICE_EXTENSION));

    devExtension->zzDeviceName = devName;
    devExtension->IsMainDevice = TRUE;
    
    devExtension->ThreadObject = NULL;
    devExtension->Mounted = FALSE;
    devExtension->DismountPending = FALSE;
    devExtension->ClientContext = NULL;    
    devExtension->zzFilename.Buffer = NULL;
    devExtension->IVHash.DeviceName.Buffer = NULL;
    devExtension->MainCypher.DeviceName.Buffer = NULL;
    devExtension->zzSymbolicLinkName.Buffer = NULL;
    devExtension->PreventMediaRemoval = TRUE;

    // Create symlink; his allows user applications to CreateFile with
    // "SymbolicName"
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Building symlink name...\n"));    
    // We add on sizeof(WCHAR) to include the terminating #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    devExtension->zzSymbolicLinkName.MaximumLength = sizeof(DEVICE_SYMLINK_MAIN_NAME) + sizeof(WCHAR);
    devExtension->zzSymbolicLinkName.Buffer = FREEOTFE_MEMALLOC(devExtension->zzSymbolicLinkName.MaximumLength);
    RtlZeroMemory(devExtension->zzSymbolicLinkName.Buffer, devExtension->zzSymbolicLinkName.MaximumLength);
    devExtension->zzSymbolicLinkName.Length = (USHORT)swprintf(
	                            devExtension->zzSymbolicLinkName.Buffer,
	                            DEVICE_SYMLINK_MAIN_NAME
                                );
    // swprintf returns the number of WCHARs, not the length in bytes
    devExtension->zzSymbolicLinkName.Length = devExtension->zzSymbolicLinkName.Length * sizeof(WCHAR);

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unicoded symlink name: %ls\n",
                             devExtension->zzSymbolicLinkName.Buffer));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Length: %d\n",
                             devExtension->zzSymbolicLinkName.Length));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("MaximumLength: %d\n",
                             devExtension->zzSymbolicLinkName.MaximumLength));
    

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to create dosdevice symlink: %ls -> %ls\n", devExtension->zzSymbolicLinkName.Buffer, devExtension->zzDeviceName.Buffer));
    status = IoCreateSymbolicLink(&devExtension->zzSymbolicLinkName, &devExtension->zzDeviceName);
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Done, checking status...\n"));
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK\n"));

    devObj->Flags |= DO_DIRECT_IO;

    devObj->Characteristics |= FILE_READ_ONLY_DEVICE;


    // (Some of the bits following are taken from the DDK src/general/cancel example)
    

    // This is used to serialize access to the queue.
    KeInitializeSpinLock(&devExtension->IRPQueueLock);
    KeInitializeSemaphore(&devExtension->IRPQueueSemaphore, 0, MAXLONG );

    // Initialize the pending Irp devicequeue
    InitializeListHead(&devExtension->PendingIRPQueue);

    // Initialize the cancel safe queue
    IoCsqInitialize(&devExtension->CancelSafeQueue,
                    CSQInsertIrp,
                    CSQRemoveIrp,
                    CSQPeekNextIrp,
                    CSQAcquireLock,
                    CSQReleaseLock,
                    CSQCompleteCanceledIrp);


    // Create a thread for the device object
    devExtension->TerminateThread = FALSE;
    status = PsCreateSystemThread(
                                  &threadHandle,
                                  (ACCESS_MASK)0,
                                  NULL,
                                  (HANDLE)0L, // )0L because it's a
                                               // driver-created thread
                                  NULL,
                                  FreeOTFEThread,
                                  devObj
                                 );

    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Create thread FAILED.\n"));        
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK\n"));

    // Convert the Thread object handle into a pointer to the Thread object
    // itself. Then close the handle.
    status = ObReferenceObjectByHandle(
                            threadHandle,
                            THREAD_ALL_ACCESS,
                            NULL,
                            KernelMode,
                            &devExtension->ThreadObject,
                            NULL
                           );
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Done processing thread; checking status.\n"));
    if (!(NT_SUCCESS(status)))
      {
      DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status FAILED.\n"));        
      ZwClose(threadHandle);
      DestroyDevice(devObj);
      return status;
      }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK\n"));

    // Close the thread handle  
    ZwClose(threadHandle);


    // This is an easy thing to overlook - we do *not* have to unset the
    // device's DO_DEVICE_INITIALIZING "Flags" as this function is carried out
    // undef DriverEntry, and not something like AddDevice
   
    *DeviceObject = devObj;
    
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("CreateMainDevice\n"));

    return STATUS_SUCCESS;
}


// =========================================================================
// Cancel all IRPs currently on the queue
void
CancelAllQueuedIRPs(
    IN PDEVICE_OBJECT DeviceObject
)
{
    PIRP pendingIrp;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("CancelAllQueuedIRPs\n"));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    // We only check one member of the struct; this will tell us if the CancelSafeQueue has
    // been initialised or not
    if (devExt->CancelSafeQueue.CsqRemoveIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cancelling all queued IRPs.\n"));
        while(pendingIrp = IoCsqRemoveNextIrp(&devExt->CancelSafeQueue, NULL))
            {
            // Cancel the IRP
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cancelling an IRP...\n"));
            pendingIrp->IoStatus.Information = 0;
            pendingIrp->IoStatus.Status = STATUS_CANCELLED;
            IoCompleteRequest(pendingIrp, IO_NO_INCREMENT);
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("CancelAllQueuedIRPs\n"));
}


// =========================================================================
// Create a disk device object
// DeviceObject will be set to the newly created device object
NTSTATUS
CreateDiskDevice (
    IN PDRIVER_OBJECT DriverObject,
    IN ULONG DeviceType,
    OUT PDEVICE_OBJECT *DeviceObject
    )
{
    NTSTATUS status;
    UNICODE_STRING devName;
    PDEVICE_EXTENSION devExtension;
    HANDLE threadHandle;
    PDEVICE_OBJECT devObj;
    int i;
    

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("CreateDiskDevice\n"));

    ASSERT(DriverObject != NULL);

    // We add on sizeof(WCHAR) to include the terminating #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    devName.MaximumLength = sizeof(DEVICE_DISK_PREFIX) +
                            sizeof(DEVICE_DISK_NUMBER) +
                            sizeof(WCHAR);
    devName.Buffer = FREEOTFE_MEMALLOC(devName.MaximumLength);    
    i = 0;
    do
        {
        i++;
        
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Building device name...\n"));

        RtlZeroMemory(devName.Buffer, devName.MaximumLength);        
        devName.Length = (USHORT)swprintf(devName.Buffer, DEVICE_DISK_PREFIX L"%d", i);
        // swprintf returns the number of WCHARs, not the length in bytes
        devName.Length = devName.Length * sizeof(WCHAR);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Name length: %d %d\n", devName.Length, devName.MaximumLength));

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Attempting to create device: %ls\n", devName.Buffer));
        // We use FILE_DEVICE_DISK as FILE_DEVICE_VIRTUAL_DISK can
        // make NTFS-only Windows XP systems unstable?
        // See MS Knowledgebase article: Q257405 for further details
        // http://support.microsoft.com/default.aspx?scid=kb;en-us;257405
        status = IoCreateDevice(
                                DriverObject,
                                sizeof(DEVICE_EXTENSION),
                                &devName,
                                DeviceType,
                                0,
                                FALSE,
                                &devObj
                            );
        if (
            (status == STATUS_OBJECT_NAME_EXISTS) ||
            (status == STATUS_OBJECT_NAME_COLLISION)
           )
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unable to create device; name exists/name collision.\n"));
            }
        else if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Straight failure when attempting to create device.\n"));
            }
         
        } while (
                 (i < MAX_DEVICES) &&
                 (
                   (status == STATUS_OBJECT_NAME_EXISTS) ||
                   (status == STATUS_OBJECT_NAME_COLLISION)
                 )
                );
                
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        return status;
        }


    devExtension = (PDEVICE_EXTENSION)devObj->DeviceExtension;
    // Purging the device extension memory sets all pointers to NULL
    RtlZeroMemory(devExtension, sizeof(DEVICE_EXTENSION));

    devExtension->zzDeviceName = devName;
    devExtension->IsMainDevice = FALSE;
    
    devExtension->ThreadObject = NULL;
    devExtension->Mounted = FALSE;
    devExtension->DismountPending = FALSE;
    devExtension->zzFilename.Buffer = NULL;
    devExtension->IVHash.DeviceName.Buffer = NULL;
    devExtension->MainCypher.DeviceName.Buffer = NULL;
    devExtension->zzSymbolicLinkName.Buffer = NULL;
    devExtension->PreventMediaRemoval = TRUE;

    devObj->Flags |= DO_DIRECT_IO;
    

    // (Some of the bits following are taken from the DDK src/general/cancel example)
    

    // This is used to serialize access to the queue.
    KeInitializeSpinLock(&devExtension->IRPQueueLock);
    KeInitializeSemaphore(&devExtension->IRPQueueSemaphore, 0, MAXLONG);

    // Initialize the pending Irp devicequeue
    InitializeListHead(&devExtension->PendingIRPQueue);

    // Initialize the cancel safe queue
    IoCsqInitialize(&devExtension->CancelSafeQueue,
                    CSQInsertIrp,
                    CSQRemoveIrp,
                    CSQPeekNextIrp,
                    CSQAcquireLock,
                    CSQReleaseLock,
                    CSQCompleteCanceledIrp);


    // Create a thread for the device object
    devExtension->TerminateThread = FALSE;
    status = PsCreateSystemThread(
                                  &threadHandle,
                                  (ACCESS_MASK)0,
                                  NULL,
                                  (HANDLE)0L, // 0L because it's a
                                              // driver-created thread
                                  NULL,
                                  FreeOTFEThread,
                                  devObj
                                 );

    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Create thread FAILED.\n"));        
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK\n"));

    // Convert the Thread object handle into a pointer to the Thread object
    // itself. Then close the handle.
    status = ObReferenceObjectByHandle(
                            threadHandle,
                            THREAD_ALL_ACCESS,
                            NULL,
                            KernelMode,
                            &devExtension->ThreadObject,
                            NULL
                           );
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Done processing thread; checking status.\n"));
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status FAILED.\n"));        
        ZwClose(threadHandle);
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK\n"));

    // Close the thread handle  
    ZwClose(threadHandle);

    status = ClientSecurityCreate(&devExtension->ClientContext);
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Done creating client security; checking status.\n"));
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Status FAILED.\n"));        
        DestroyDevice(devObj);
        return status;
        }

    // This is an easy thing to overlook; we don't need to do it when
    // creating the main device as that's carried out under DriverEntry;
    // but when creating devices (e.g. in AddDevice, we must, MUST, **MUST**
    // do this!
    devObj->Flags &= ~DO_DEVICE_INITIALIZING;

    *DeviceObject = devObj;
    
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("CreateDiskDevice\n"));

    return STATUS_SUCCESS;
}


// =========================================================================
VOID CSQInsertIrp (
    IN PIO_CSQ   Csq,
    IN PIRP              Irp
    )
{
    PDEVICE_EXTENSION   devExtension;

    devExtension = CONTAINING_RECORD(Csq, 
                                 DEVICE_EXTENSION, CancelSafeQueue);

    InsertTailList(&devExtension->PendingIRPQueue, 
                         &Irp->Tail.Overlay.ListEntry);
}


// =========================================================================
VOID CSQRemoveIrp(
    IN  PIO_CSQ Csq,
    IN  PIRP    Irp
    )
{
    PDEVICE_EXTENSION   devExtension;

    devExtension = CONTAINING_RECORD(Csq, 
                                 DEVICE_EXTENSION, CancelSafeQueue);
    
    RemoveEntryList(&Irp->Tail.Overlay.ListEntry);
}


// =========================================================================
PIRP CSQPeekNextIrp(
    IN  PIO_CSQ Csq,
    IN  PIRP    Irp,
    IN  PVOID   PeekContext
    )
{
    PDEVICE_EXTENSION      devExtension;
    PIRP                    nextIrp = NULL;
    PLIST_ENTRY             nextEntry;
    PLIST_ENTRY             listHead;
    PIO_STACK_LOCATION     irpStack;

    devExtension = CONTAINING_RECORD(Csq, 
                             DEVICE_EXTENSION, CancelSafeQueue);
    
    listHead = &devExtension->PendingIRPQueue;

    // 
    // If the IRP is NULL, we will start peeking from the listhead, else
    // we will start from that IRP onwards. This is done under the
    // assumption that new IRPs are always inserted at the tail.
    //
        
    if(Irp == NULL) {       
        nextEntry = listHead->Flink;
    } else {
        nextEntry = Irp->Tail.Overlay.ListEntry.Flink;
    }
    
    while(nextEntry != listHead) {
        
        nextIrp = CONTAINING_RECORD(nextEntry, IRP, Tail.Overlay.ListEntry);

        irpStack = IoGetCurrentIrpStackLocation(nextIrp);
        
        //
        // If context is present, continue until you find a matching one.
        // Else you break out as you got next one.
        //
        
        if(PeekContext) {
            if(irpStack->FileObject == (PFILE_OBJECT) PeekContext) {       
                break;
            }
        } else {
            break;
        }
        nextIrp = NULL;
        nextEntry = nextEntry->Flink;
    }

    return nextIrp;
    
}


// =========================================================================
VOID CSQAcquireLock(
    IN  PIO_CSQ Csq,
    OUT PKIRQL  Irql
    )
{
    PDEVICE_EXTENSION   devExtension;

    devExtension = CONTAINING_RECORD(Csq, 
                                 DEVICE_EXTENSION, CancelSafeQueue);

    KeAcquireSpinLock(&devExtension->IRPQueueLock, Irql);
}


// =========================================================================
VOID CSQReleaseLock(
    IN PIO_CSQ Csq,
    IN KIRQL   Irql
    )
{
    PDEVICE_EXTENSION   devExtension;

    devExtension = CONTAINING_RECORD(Csq, 
                                 DEVICE_EXTENSION, CancelSafeQueue);
    
    KeReleaseSpinLock(&devExtension->IRPQueueLock, Irql);
}


// =========================================================================
VOID CSQCompleteCanceledIrp(
    IN  PIO_CSQ             pCsq,
    IN  PIRP                Irp
    )
{
    Irp->IoStatus.Status = STATUS_CANCELLED;
    Irp->IoStatus.Information = 0;
    DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Cancelled IRP.\n"));    
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
}


// =========================================================================
// Queue IRP
NTSTATUS
QueueIRPToThread(
    IN  PDEVICE_OBJECT DeviceObject,    
    IN  PIRP           Irp
)
{
    PDEVICE_EXTENSION devExt;
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("QueueIrpToThread\n"));
    
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    
    status = STATUS_PENDING;                
    
    // Sanity check; if the target device isn't mounted, and it's not a mount
    // IOCTL, then return appropriate status
    irpSp = IoGetCurrentIrpStackLocation(Irp);
    if (
        (irpSp->Parameters.DeviceIoControl.IoControlCode != IOCTL_FREEOTFE_MOUNT) &&
        (irpSp->Parameters.DeviceIoControl.IoControlCode != IOCTL_FREEOTFE_DISMOUNT)
       )
        {
        if ( (!(devExt->Mounted)) || (devExt->DismountPending) )
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cannot queue IRP for unmounted/dismount pending device if IOCTL isn't a mount/dismount; not queueing\n"));
            status = STATUS_NO_MEDIA_IN_DEVICE;
            }
        
        }
        
        
    // Queue the IRP, if we're still supposed to
    if (status == STATUS_PENDING)
        {
        // Note: IoCsqInsertIrp marks the IRP pending.
        IoCsqInsertIrp(&devExt->CancelSafeQueue, Irp, NULL);
    
        // A semaphore remains signaled as long as its count is greater than 
        // zero, and non-signaled when the count is zero. Following function 
        // increments the semaphore count by 1.
        KeReleaseSemaphore(
                            &devExt->IRPQueueSemaphore,
                            0,  // No priority boost
                            1,  // Increment semaphore by 1
                            FALSE  // No WaitForXxx after this call
                           );
        }
              
    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("QueueIrpToThread\n"));
    
    return status;
}    


// =========================================================================
// This is the main thread that removes IRP from the queue
// and processes (peforms I/O on) it.
// Context - this is set to the device object
VOID
FreeOTFEThread(
    IN PVOID Context
    )
{
    NTSTATUS status;
    PDEVICE_OBJECT devObj;
    PDEVICE_EXTENSION devExt;
    PIRP Irp;
    PIO_STACK_LOCATION irpSp;
    unsigned int i;    
    
        
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeOTFEThread\n"));
    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting threads priority...\n"));
    KeSetPriorityThread(KeGetCurrentThread(), LOW_REALTIME_PRIORITY);

    devObj = (PDEVICE_OBJECT)Context;
    devExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Entering endless loop for queued event signals...\n"));
    // This while loop is terminated by breaking out of it when the thread is to terminate
    while (TRUE)
        {
        // Wait until signalled that there is one or more IRP queued...
        status = KeWaitForSingleObject(
            &devExt->IRPQueueSemaphore,
            Executive,
            KernelMode,
            FALSE,
            NULL
            );
        
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queued event signalled.\n"));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread is for device: %ls\n", devExt->zzDeviceName.Buffer));
            
        // If we're supposed to terminate the thread, bail out...
        if (devExt->TerminateThread) 
           {
           DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread has been requested to terminate!.\n"));
           break;  // Thread terminates after this loop terminates
           }

           
        // Remove a pending IRP from the queue.
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread getting next queued IRP.\n"));
        Irp = IoCsqRemoveNextIrp(&devExt->CancelSafeQueue, NULL);

                
        if (!Irp)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Queued IRP was cancelled; next.\n"));
            continue; // go back to waiting
            }
            
            
        // Default...
        status = STATUS_NOT_IMPLEMENTED;
        Irp->IoStatus.Information = 0;


// xxxop - if the device's DISMOUNT PENDING FLAG is set, then fail the IRP, unless it IRP was a dismount or destroy request

        irpSp = IoGetCurrentIrpStackLocation(Irp);

        switch (irpSp->MajorFunction)
            {
            case IRP_MJ_READ:
            case IRP_MJ_WRITE:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD: IRP_MJ_READ/IRP_MJ_WRITE\n"));
                if (!(devExt->Mounted))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Attempt to read/write from/to unmounted device.\n"));
                    status = STATUS_DEVICE_NOT_READY;
                    break;
                    }

                if (
                    (irpSp->MajorFunction == IRP_MJ_WRITE) &
                    (devExt->ReadOnly)
                   )
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Attempt to write to readonly volume.\n"));
                    status = STATUS_MEDIA_WRITE_PROTECTED;
                    break;
                    }
                    
                    
                if (irpSp->MajorFunction == IRP_MJ_READ) 
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- begin - READ - READ - READ --\n"));
                    }
                else if (irpSp->MajorFunction == IRP_MJ_WRITE) 
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- begin - WRITE - WRITE - WRITE --\n"));
                    }
                else
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- begin - UNKNOWN - UNKNOWN - UNKNOWN --\n"));
                    }
                
                status = ThreadReadWriteData(devObj, Irp);

                if (irpSp->MajorFunction == IRP_MJ_READ) 
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- end - READ - READ - READ --\n"));
                    }
                else if (irpSp->MajorFunction == IRP_MJ_WRITE) 
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- end - WRITE - WRITE - WRITE --\n"));
                    }
                else
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-- end - UNKNOWN - UNKNOWN - UNKNOWN --\n"));
                    }
                
                break;
                }
                
                                
            case IRP_MJ_DEVICE_CONTROL:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD: IRP_MJ_DEVICE_CONTROL\n"));
                switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
                    {
                    // NOTE: THIS CASE BREAKS OUT WHEN IT HITS AN ERROR - AT THE END OF
                    //       THE SWITCH STATEMENT, IF THERE WAS AN ERROR, AND THIS CASE
                    //       WAS USED, OverwriteDeviceSensitive(...) IS CALLED TO CLEARDOWN
                    case IOCTL_FREEOTFE_MOUNT:
                        {
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD IoControl code is IOCTL_FREEOTFE_MOUNT\n"));
                        status = ThreadMount(devObj, Irp);
                        // Cleardown device extension in case of failure
                        if (!(NT_SUCCESS(status)))
                            {
                            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD: Clearing down after failed mount.\n"));
                            OverwriteDeviceSensitive(devObj);
                            }

                        break;
                        }
                        
                        
                    case IOCTL_STORAGE_EJECT_MEDIA:
                    case IOCTL_FREEOTFE_DISMOUNT:
                        {
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD IoControl code is IOCTL_STORAGE_EJECT_MEDIA/IOCTL_FREEOTFE_DISMOUNT\n"));
                        status = ThreadDismount(devObj, Irp);
                        break;
                        }
                        
                        
                        
                    default:
                        {
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD: Unrecognised IRP_MJ_DEVICE_CONTROL IRP.\n"));
                        // We don't need to set the status; it's defauled above to STATUS_NOT_IMPLEMENTED
                        }
                        
                    }   // switch (irpSp->Parameters.DeviceIoControl.IoControlCode)


                break;
                }   // case IRP_MJ_DEVICE_CONTROL:


            case IRP_MJ_FLUSH_BUFFERS:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("THREAD: IRP_MJ_FLUSH_BUFFERS\n"));
                status = ThreadFlushBuffers(devObj, Irp);
                break;
                }   // case IRP_MJ_DEVICE_CONTROL:
                

            default:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("THREAD: Unrecognised IRP MajorFunction.\n"));
                }
                        
            }   // switch (irpSp->MajorFunction)

                
        
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread completing IRP (%d).\n", status));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread setting IoStatus.Information = 0\n"));
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);           
        }   // while (TRUE)
        
        
    // This part will only be reached if the thread has been signalled to
    // terminate
    devExt = NULL;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Terminating thread...\n"));
    status = PsTerminateSystemThread(STATUS_SUCCESS);
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Thread did NOT terminate OK\n"));
        }        
        
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Thread dropping out.\n"));           

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeOTFEThread\n"));
}

// =========================================================================
// Return the device object with the specified target device name
NTSTATUS
GetNamedCharDevice (
    IN PDRIVER_OBJECT DriverObject,
    IN PCHAR TgtDeviceName, 
    OUT PDEVICE_OBJECT* DeviceObject
    )
{
    NTSTATUS status;
    PDIOC_DEVICE_NAME DIOCBuffer;
    PDEVICE_OBJECT tgtDeviceObj;
    ANSI_STRING tmpANSIDeviceName;
    UNICODE_STRING tmpUnicodeDeviceName;
    PDEVICE_EXTENSION tgtDevExt;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetNamedDevice (char*)\n"));

    tmpANSIDeviceName.Length = (USHORT)strlen(TgtDeviceName);
    tmpANSIDeviceName.MaximumLength = tmpANSIDeviceName.Length + 1;  
    tmpANSIDeviceName.Buffer = FREEOTFE_MEMALLOC(tmpANSIDeviceName.MaximumLength);
    RtlZeroMemory(
                  tmpANSIDeviceName.Buffer,
                  tmpANSIDeviceName.MaximumLength
                 );
    RtlCopyMemory(
                  tmpANSIDeviceName.Buffer,
                  TgtDeviceName,
                  tmpANSIDeviceName.Length
                 );

    status = RtlAnsiStringToUnicodeString(
                                          &tmpUnicodeDeviceName,
                                          &tmpANSIDeviceName,
                                          TRUE
                                         );
    if (NT_SUCCESS(status))
        {
        status = GetNamedUnicodeDevice(
                                       DriverObject,
                                       &tmpUnicodeDeviceName, 
                                       DeviceObject
                                      );
        RtlFreeUnicodeString(&tmpUnicodeDeviceName);
        }
    FREEOTFE_FREE(tmpANSIDeviceName.Buffer);
    

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetNamedDevice (char*)\n"));
    
    return status;
}


// =========================================================================
// Return the device object with the specified target device name
NTSTATUS
GetNamedUnicodeDevice (
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING TgtDeviceName, 
    OUT PDEVICE_OBJECT* DeviceObject
    )
{
    PDEVICE_OBJECT      currDevObj;
    PDEVICE_EXTENSION   devExt;
    NTSTATUS            status;
  
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetNamedDevice (UNICODE_STRING)\n"));
     
  
    status = STATUS_UNSUCCESSFUL;
  
    *DeviceObject = NULL;

    // Locate the first device with a matching device name
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Searching for named device: %ls\n", TgtDeviceName->Buffer));
    currDevObj = DriverObject->DeviceObject;                    
    while (currDevObj!=NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Device found, checking if it's the named FreeOTFE disk device...\n"));
        devExt = (PDEVICE_EXTENSION)(currDevObj->DeviceExtension);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Device is: %ls\n", devExt->zzDeviceName.Buffer));


        // Note: We carry out the comparison case-INsensitive
        if (RtlEqualUnicodeString(
                                  &devExt->zzDeviceName,
                                  TgtDeviceName,
                                  FALSE
                                 ))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Named device found.\n"));
            *DeviceObject = currDevObj;
            status = STATUS_SUCCESS;
            break;        
            } 
      
        currDevObj = currDevObj->NextDevice;      
        }   // while


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetNamedDevice (UNICODE_STRING)\n"));

    return status;
}


// =========================================================================
// Read/write data to the specified device
// Returns status, but does *not* complete the IRP
NTSTATUS
ThreadFlushBuffers(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
)
{
    NTSTATUS status;
    FREEOTFEBYTE* buffer;
    FREEOTFEBYTE* tmpBuffer;
    unsigned int offset;
    unsigned int tmpBufferSize;
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;
    LARGE_INTEGER fileOffset;
    LARGE_INTEGER tmpOffset;
        
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ThreadFlushBuffers\n"));
    
    status = STATUS_SUCCESS;

    devExt = DeviceObject->DeviceExtension;

    // Flush...
    // Because we opened any volume file using the FILE_WRITE_THROUGH
    // create option, there should be nothing to flush.

    Irp->IoStatus.Status = status;
    Irp->IoStatus.Information = 0;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ThreadFlushBuffers\n"));
    
    return status;
}


// =========================================================================
// Read/write data to the specified device
// Returns status, but does *not* complete the IRP
// IMPORTANT: Sanity check as to whether the volume is readonly *must* have been already
//            carried out *before* calling this function.
// IMPORTANT: Sanity check to ensure that the read/write doesn't go past the end of the
//            volume *must* have already been carried out *before* calling this function.
// IMPORTANT: Sanity check to ensure that the length of data read/written is a multiple
//            of the sector size *must* have already been carried out *before* calling this function.
NTSTATUS
ThreadReadWriteData(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
)
{
    NTSTATUS status;
    FREEOTFEBYTE* buffer;
    FREEOTFEBYTE* tmpBuffer;
    unsigned int offset;
    unsigned int tmpBufferSize;
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;
    LARGE_INTEGER fileOffset;
    LARGE_INTEGER tmpOffset;
        
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ThreadReadWriteData\n"));
    
    status = STATUS_DISK_OPERATION_FAILED;
    
    devExt = DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);    

    if (irpSp->MajorFunction == IRP_MJ_READ) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("zzzREAD - READ - READ\n"));
        }
    else if (irpSp->MajorFunction == IRP_MJ_WRITE) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("zzzWRITE - WRITE - WRITE\n"));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("zzzUNKNOWN - UNKNOWN - UNKNOWN\n"));
        }
    

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData: entry iostatus: %d\n", Irp->IoStatus.Status));
        
    // Because the device uses DO_DIRECT_IO, the buffer at Irp->MdlAddress
    // is used
    buffer = (PVOID)MmGetSystemAddressForMdlSafe(Irp->MdlAddress, NormalPagePriority);
    if (buffer != NULL)
        {
        tmpBufferSize = irpSp->Parameters.Read.Length;
        if (irpSp->MajorFunction == IRP_MJ_WRITE)
            {
            tmpBufferSize = irpSp->Parameters.Write.Length;
            }
        tmpBuffer = FREEOTFE_MEMALLOC(tmpBufferSize);
        if (tmpBuffer != NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData: Got read/write buffer\n"));
            if (irpSp->MajorFunction == IRP_MJ_WRITE)
                {
                fileOffset.QuadPart = irpSp->Parameters.Write.ByteOffset.QuadPart + devExt->DataOffset.QuadPart;

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData WRITE\n"));            
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Request:\n"));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Partition offset: %lld\n", irpSp->Parameters.Write.ByteOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  File offset     : %lld\n", fileOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  I/O Length      : %d\n", tmpBufferSize));

                // Encrypt in sector sized chunks
                // Note: The "FreeOTFE_MF_DispatchRead" function will have already
                //       ensured that tmpBufferSize is a multiple of the number of
                //       bytes per sector
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Encrypting data...\n"));
                for(offset=0; offset<tmpBufferSize; offset+=devExt->EncryptionBlockSize)
                    {
                    tmpOffset.QuadPart = (fileOffset.QuadPart + offset);
                    status = BlockEncrypt(
                                        devExt,
                                        tmpOffset,
                                        &buffer[offset],
                                        &tmpBuffer[offset]
                                        );
                    if (!(NT_SUCCESS(status)))
                        {
                        break;
                        }
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Encrypted with status: %d (success is %d)\n", status, STATUS_SUCCESS));
                
                if (NT_SUCCESS(status))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Writing...\n"));
                    status = ActualWriteFile(
                                        DeviceObject,
                                        &fileOffset,
                                        tmpBufferSize,    
                                        tmpBuffer
                                    );
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Written with status: %d (success is %d)\n", status, STATUS_SUCCESS));
                    }
                                
                }
            else
                {
                fileOffset.QuadPart = irpSp->Parameters.Read.ByteOffset.QuadPart + devExt->DataOffset.QuadPart;

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData READ\n"));            
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Request:\n"));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Partition offset: %lld\n", irpSp->Parameters.Read.ByteOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  File offset     : %lld\n", fileOffset.QuadPart));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  I/O Length      : %d\n", tmpBufferSize));

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Reading...\n"));
                status = ActualReadFile(
                                    DeviceObject,
                                    &fileOffset,
                                    tmpBufferSize,    
                                    tmpBuffer
                                );
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Read with status: %d (success is %d)\n", status, STATUS_SUCCESS));

                if (NT_SUCCESS(status))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Decrypting data...\n"));
                    // Decrypt in sector sized chunks
                    // Note: The "FreeOTFE_MF_DispatchRead" function will have 
                    //       already ensured that tmpBufferSize is a multiple 
                    //       of the number of bytes per sector
                    for(offset=0; offset<tmpBufferSize; offset+=devExt->EncryptionBlockSize)
                        {
                        tmpOffset.QuadPart = (fileOffset.QuadPart + offset);
                        status = BlockDecrypt(
                                               devExt,
                                               tmpOffset,
                                               &tmpBuffer[offset],
                                               &buffer[offset]
                                              );
                        if (!(NT_SUCCESS(status)))
                            {
                            break;
                            }
                        }
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Decrypted with status: %d (success is %d)\n", status, STATUS_SUCCESS));
                    }

                }
                
                
            SecZeroMemory(tmpBuffer, tmpBufferSize);
            FREEOTFE_FREE(tmpBuffer);
            }
        }
        

    Irp->IoStatus.Status = status;
    if (NT_SUCCESS(status))
        {
        Irp->IoStatus.Information = tmpBufferSize;
        }
    else
        {
        Irp->IoStatus.Information = 0;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData request returning: %d bytes\n", Irp->IoStatus.Information));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData returning %d\n", status));

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ThreadReadWriteData: exit iostatus: %d\n", Irp->IoStatus.Status));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ThreadReadWriteData\n"));
    
    return status;
}


// =========================================================================
// Perform an actual read from the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
NTSTATUS
ActualReadFile(
    IN      PDEVICE_OBJECT DeviceObject,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    IN      FREEOTFEBYTE* Buffer
)
{
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;
    LARGE_INTEGER transferOffset;
    FREEOTFEBYTE* transferBuffer = NULL;
    ULONG transferBufferLength;
    ULONG preData;
    ULONG postData;
    LARGE_INTEGER tmpLargeInt;
    IO_STATUS_BLOCK tmpStatusBlock;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ActualReadFile\n"));

    // WARNING: PC version assumes SUCCESS; PDA version assumes FAILURE
    status = STATUS_SUCCESS;

    devExt = DeviceObject->DeviceExtension;

    // Round the offset *down* to the volume file/partition's sector size
    tmpLargeInt.QuadPart = (Offset->QuadPart % devExt->FileSectorSize);
    preData = tmpLargeInt.LowPart;

    // Round the offset *up* to the volume file/partition's sector size
    postData = 0;
    tmpLargeInt.QuadPart = ((Offset->QuadPart + BufferLength) % devExt->FileSectorSize);
    if (tmpLargeInt.LowPart != 0)
        {
        postData = devExt->FileSectorSize - tmpLargeInt.LowPart;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("preData : %d\n", preData));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("postData: %d\n", postData));

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
        transferBuffer = FREEOTFE_MEMALLOC(transferBufferLength);
        if (transferBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to malloc transferBuffer (%d bytes)\n", transferBufferLength));
            status = STATUS_NO_MEMORY;
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Requested data length : %d\n", BufferLength));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Actual transfer length: %d\n", transferBufferLength));

    // Sanity
    if (NT_SUCCESS(status))
        {
        status = ZwReadFile(
                            devExt->FileHandle,
                            NULL,
                            NULL,
                            NULL,
                            &tmpStatusBlock,
                            transferBuffer,
                            transferBufferLength,
                            &transferOffset,
                            NULL
                        );

        if (NT_SUCCESS(status))
            {
            if (tmpStatusBlock.Information != transferBufferLength)
                {
                status = STATUS_END_OF_FILE;
                }
            }

        if (NT_SUCCESS(status))
            {
            if (
                (preData != 0) ||
                (postData != 0)
               )
                {
                RtlCopyMemory(
                            Buffer,
                            &transferBuffer[preData],
                            BufferLength
                            );
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
            FREEOTFE_FREE(transferBuffer);
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ActualReadFile\n"));

    return status;
}


// =========================================================================
// Perform an actual write to the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
NTSTATUS
ActualWriteFile(
    IN      PDEVICE_OBJECT DeviceObject,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    OUT     FREEOTFEBYTE* Buffer
)
{
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;
    LARGE_INTEGER transferOffset;
    FREEOTFEBYTE* transferBuffer = NULL;
    ULONG transferBufferLength;
    ULONG preData;
    ULONG postData;
    LARGE_INTEGER tmpLargeInt;
    IO_STATUS_BLOCK tmpStatusBlock;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ActualWriteFile\n"));

    // WARNING: PC version assumes SUCCESS; PDA version assumes FAILURE
    status = STATUS_SUCCESS;

    devExt = DeviceObject->DeviceExtension;

    // Round the offset *down* to the volume file/partition's sector size
    tmpLargeInt.QuadPart = (Offset->QuadPart % devExt->FileSectorSize);
    preData = tmpLargeInt.LowPart;

    // Round the offset *up* to the volume file/partition's sector size
    postData = 0;
    tmpLargeInt.QuadPart = ((Offset->QuadPart + BufferLength) % devExt->FileSectorSize);
    if (tmpLargeInt.LowPart != 0)
        {
        postData = devExt->FileSectorSize - tmpLargeInt.LowPart;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("preData : %d\n", preData));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("postData: %d\n", postData));

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
        transferBuffer = FREEOTFE_MEMALLOC(transferBufferLength);

        if (transferBuffer == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to malloc transferBuffer (%d bytes)\n"), transferBufferLength));
            status = STATUS_NO_MEMORY;
            }
        else
            {
            // Preserve encrypted data already stored between volume's sector boundry, and the
            // offset where we are to start writing
            if (preData != 0)
                {
                status = ActualReadFile(
                            DeviceObject,
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
                            DeviceObject,
                            &tmpLargeInt,
                            postData,
                            // Yes, this is also correct - think about it
                            &transferBuffer[(preData + BufferLength)]
                            );
                }

            RtlCopyMemory(
                        &transferBuffer[preData],
                        Buffer,
                        BufferLength
                        );
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Requested data length : %d\n", BufferLength));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Actual transfer length: %d\n", transferBufferLength));

    if (NT_SUCCESS(status))
        {
        status = ZwWriteFile(
                            devExt->FileHandle,
                            NULL,
                            NULL,
                            NULL,
                            &tmpStatusBlock,
                            transferBuffer,
                            transferBufferLength,
                            &transferOffset,
                            NULL
                        );

        if (NT_SUCCESS(status))
            {
            if (tmpStatusBlock.Information != transferBufferLength)
                {
                status = STATUS_END_OF_FILE;
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
            FREEOTFE_FREE(transferBuffer);
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ActualWriteFile\n"));

    return status;
}


// =========================================================================
// Mount the specified device
// Note: This function is not particularly nice; has multiple "return" points
// Note: If this function does not succeed, the device's device extension MUST be overwritten
//       by the caller; it could well contain sensitive information as this function does
//       not clean itsself up. Again, not particularly nice
// Returns status, but does *not* complete the IRP
NTSTATUS
ThreadMount(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
)
{
    PDIOC_MOUNT DIOCBuffer;
    ANSI_STRING tmpANSIFilename;
    PDEVICE_EXTENSION tgtDevExt;
    ULONG fileAttributes;
    OBJECT_ATTRIBUTES fileObjAttribs;
    LARGE_INTEGER determinedMaxSize;
    LARGE_INTEGER dataEnd;
    unsigned int i;
    NTSTATUS status = STATUS_SUCCESS;
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;
    FREEOTFEBYTE tmpHashBuffer[FREEOTFE_MAX_HASH_LENGTH];
    unsigned int tmpHashBufferUsed;
    unsigned int useHashBits;
    PDEVICE_OBJECT fileHostDevice;
    LARGE_INTEGER tmpLargeInt;
    PCHAR ptrMasterKey;    
    PCHAR ptrVolumeIV;    
    PCHAR ptrMetaData;


    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ThreadMount\n"));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBuffer = (PDIOC_MOUNT)Irp->AssociatedIrp.SystemBuffer;


    // This may seem a little weird, but is required as the position of various members
    // within the struct is variable
    ptrMasterKey = (PCHAR)&(DIOCBuffer->MasterKey);
    ptrVolumeIV  = ptrMasterKey + (DIOCBuffer->MasterKeyLength / 8);    
    ptrMetaData  = ptrVolumeIV + (DIOCBuffer->VolumeIVLength / 8);    


    // --- COPY THE VOLUME FLAGS ---
    devExt->VolumeFlags = DIOCBuffer->VolumeFlags;

    // --- UNICODE FILENAME ---
    // Unicode the filename and copy into "devObj->zzFilename"
    // This is +1 in order to allow for the NULL terminator
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unicoding filename to devExt.\n"));
    tmpANSIFilename.Length = (USHORT)strlen(DIOCBuffer->Filename);
    tmpANSIFilename.MaximumLength = tmpANSIFilename.Length + 1;
    tmpANSIFilename.Buffer = FREEOTFE_MEMALLOC(tmpANSIFilename.MaximumLength);
    RtlZeroMemory(
                tmpANSIFilename.Buffer,
                tmpANSIFilename.MaximumLength
                );
    RtlCopyMemory(
                    tmpANSIFilename.Buffer,
                    DIOCBuffer->Filename,
                    tmpANSIFilename.Length
                );

    // +1 in order to include the terminating NULL
    devExt->zzFilename.MaximumLength = (strlen(DIOCBuffer->Filename) + 1) * sizeof(WCHAR);
    devExt->zzFilename.Length = 0;
    devExt->zzFilename.Buffer = FREEOTFE_MEMALLOC(devExt->zzFilename.MaximumLength);
    RtlZeroMemory(devExt->zzFilename.Buffer, devExt->zzFilename.MaximumLength);        
    status = RtlAnsiStringToUnicodeString(
                                        &devExt->zzFilename,
                                        &tmpANSIFilename,
                                        FALSE
                                        );
    FREEOTFE_FREE(tmpANSIFilename.Buffer);

    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Failed to RtlAnsiStringToUnicodeString filename.\n"));                
        return status;
        }
        
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Filename is: %ls\n", devExt->zzFilename.Buffer));    
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Filename length: %d %d\n", devExt->zzFilename.Length, devExt->zzFilename.MaximumLength));    



    // --- OBTAIN THE HASH DRIVER ---
    // Blank, in case an IV hash driver wasn't passed in
    memset(&(devExt->IVHash), 0, sizeof(devExt->IVHash));
    // If an IV hash algorithm *was* passed in, get it's details and store them
    if (
        (DIOCBuffer->IVHashDeviceName[0] != 0) &&
        (!(IsEqualGUID(&(DIOCBuffer->IVHashGUID), &FREEOTFE_NULL_GUID)))
       )
        {
        devExt->IVHash.HashGUID = DIOCBuffer->IVHashGUID;
        status = GetDeviceDetailsHash(
                                (char*)&DIOCBuffer->IVHashDeviceName,
                                &DIOCBuffer->IVHashGUID,

                                &devExt->IVHash
                                );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get IV hash device (%d)\n", status));
            return status;
            }
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("No IV hash algorithm specified.\n"));
        }


    // --- OBTAIN THE IV CYPHER DRIVER ---
    // Blank, in case a IV cypher driver wasn't passed in
    memset(&(devExt->IVCypher), 0, sizeof(devExt->IVCypher));
    // If a IV cypher algorithm *was* passed in, get it's details and store them
    if (
        (DIOCBuffer->IVCypherDeviceName[0] != 0) &&
        (!(IsEqualGUID(&(DIOCBuffer->IVCypherGUID), &FREEOTFE_NULL_GUID)))
       )
        {
        devExt->IVCypher.CypherGUID = DIOCBuffer->IVCypherGUID;
        status = GetDeviceDetailsCypher_v3_v1(
                                        (char*)&DIOCBuffer->IVCypherDeviceName,
                                        &DIOCBuffer->IVCypherGUID,                                                     

                                        &devExt->IVCypher
                                    );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get IV cypher device (%d)\n", status));
            return status;
            }     
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("No IV cypher specified.\n"));
        }


    // --- OBTAIN THE MAIN CYPHER DRIVER ---
    devExt->MainCypher.CypherGUID = DIOCBuffer->MainCypherGUID;
    status = GetDeviceDetailsCypher_v3_v1(
                                    (char*)&DIOCBuffer->MainCypherDeviceName,
                                    &DIOCBuffer->MainCypherGUID,                                                     

                                    &devExt->MainCypher
                                );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get main cypher device (%d)\n", status));
        return status;
        }     


    // --- COPY THE MASTER KEY ---
    // KeyLength must = the keysize
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher's REQUIRED keysize: %d bits\n", devExt->MainCypher.Details.KeySizeRequired));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Master key: %d bits\n", DIOCBuffer->MasterKeyLength));

    if ((devExt->MainCypher.Details.KeySizeRequired > 0) && (devExt->MainCypher.Details.KeySizeRequired != DIOCBuffer->MasterKeyLength))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's required keysize\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }

    // Note: This is in *bits*
    devExt->MasterKeyLength = DIOCBuffer->MasterKeyLength;

    // Divide by 8 to get bytes from bits
    devExt->MasterKey = FREEOTFE_MEMALLOC((devExt->MasterKeyLength / 8));
	// Zero the buffer in in order to ensure that unused buffer
	// remaining after the key is zero'd, just in case the cypher
	// used is badly behaved
	RtlZeroMemory(
				    devExt->MasterKey,
					(devExt->MasterKeyLength / 8)
					);
	RtlCopyMemory(
					devExt->MasterKey,
					ptrMasterKey,
					(devExt->MasterKeyLength / 8)
					);

    // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Key length: %d bits\n", devExt->MasterKeyLength));
    for(i = 0; i < (devExt->MasterKeyLength / 8); i++)
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key [%.2d]: %.2x\n", i, devExt->MasterKey[i]));
        }                                


    // Divide by 8 to get bytes from bits, then multiply by 2 to get nibbles
    // Note: +2 in order to store two terminating NULLs; just in case
    devExt->MasterKeyASCII = FREEOTFE_MEMALLOC((((devExt->MasterKeyLength / 8) * 2) + 2));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                        devExt->MasterKeyLength,
                        devExt->MasterKey,
                        devExt->MasterKeyASCII
                        );

    // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
    //       DEBUG BUILDS
    //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Key as ASCII: %s\n", devExt->MasterKeyASCII));


    // --- TYPE OF MEDIA ---
    // Store the type of media to be emulated for later use...
    devExt->StorageMediaType = DIOCBuffer->StorageMediaType;


    // --- COPY THE SECTOR IV GENERATION METHOD ---
    devExt->SectorIVGenMethod = DIOCBuffer->SectorIVGenMethod;

            
    // --- HASH THE MASTER KEY FOR ESSIV (IV) USE ---
    // WARNING: Must get the cypher, hash and SectorIVGenMethod *first*
    // Note: Because this is for ESSIV use, we only keep the first (cypher key required) bits of
    //       the hash; rightpadding as necessary

    // If we're not using ESSIV to generate IVs, we don't bother hashing the key to generate
    // an ESSIV key; we act as though the hash algorithm just returned no data.
    if (devExt->SectorIVGenMethod != SCTRIVGEN_ESSIV)
        {
        devExt->ESSIVKeyLength = 0;
        devExt->ESSIVKey = NULL;
        devExt->ESSIVKeyASCII = NULL;
        }
    else
        {
        // We use a temp buffer as we can't guarantee how long the hash output will be
        // e.g. if it's a variable length hash like "unhashed/NULL"
        tmpHashBufferUsed = (sizeof(tmpHashBuffer) * 8);  // Convert from bytes to bits

        status = DataHash(
                          &(devExt->IVHash),
                          devExt->MasterKeyLength,
                          devExt->MasterKey,
                          &tmpHashBufferUsed,  // In bits
                          tmpHashBuffer
                         );
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to hash key for ESSIV IVs (%d)\n", status));
            // Overwrite potentially sensitive data in temp buffer...
            SecZeroMemory(tmpHashBuffer, sizeof(tmpHashBuffer));
            return status;
            }

        devExt->ESSIVKeyLength = tmpHashBufferUsed;
        useHashBits = tmpHashBufferUsed;
        // Handle if the keysize is fixed
        if (devExt->IVCypher.Details.KeySizeRequired >= 0)
            {
            devExt->ESSIVKeyLength = devExt->IVCypher.Details.KeySizeRequired;
            useHashBits = min(tmpHashBufferUsed, (unsigned int)devExt->IVCypher.Details.KeySizeRequired);
            }

        // Divide by 8 to get bytes from bits
        devExt->ESSIVKey = FREEOTFE_MEMALLOC((devExt->ESSIVKeyLength / 8));

        // Zero the buffer in in order to ensure that unused buffer
        // remaining after the key is zero'd, just in case the cypher
        // used is badly behaved
        RtlZeroMemory(
                    devExt->ESSIVKey,
                    (devExt->ESSIVKeyLength / 8)
                    );
        RtlCopyMemory(
                    devExt->ESSIVKey,
                    tmpHashBuffer,
                    (useHashBits / 8)
                    );

        // NOTE: THE ESSIV KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- ESSIV key length: %d bits\n", devExt->ESSIVKeyLength));
        for(i = 0; i < (devExt->ESSIVKeyLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, ("----- ESSIV key [%.2d]: %.2x\n", i, devExt->ESSIVKey[i]));
            }


        // Divide by 8 to get bytes from bits, then multiply by 2 to get nibbles
        // Note: +2 in order to store two terminating NULLs; just in case
        devExt->ESSIVKeyASCII = FREEOTFE_MEMALLOC((((devExt->ESSIVKeyLength / 8) * 2) + 2));

        // Convert the data passed in to it's ASCII representation
        ConvertDataToASCIIRep(
                            devExt->ESSIVKeyLength,
                            devExt->ESSIVKey,
                            devExt->ESSIVKeyASCII
                            );

        // NOTE: THE KEY IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- ESSIV key as ASCII: %s\n", devExt->ESSIVKeyASCII));
        }


    // --- COPY THE VOLUME IV ---
    // Volume IV must = the blocksize, or be set to zero
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher's blocksize: %d bits\n", devExt->MainCypher.Details.BlockSize));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Volume IV length: %d bits\n", DIOCBuffer->VolumeIVLength));
    if (
        ((devExt->MainCypher.Details.BlockSize > 0) && (devExt->MainCypher.Details.BlockSize != DIOCBuffer->VolumeIVLength)) &&
        (DIOCBuffer->VolumeIVLength != 0)
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Volume IV must = the cypher's blocksize or zero\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }

    // Note: This is in *bits*
    devExt->VolumeIVLength = DIOCBuffer->VolumeIVLength;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- VolumeIV length: %d bits\n", devExt->VolumeIVLength));

    devExt->VolumeIV = NULL;
    if (devExt->VolumeIVLength > 0)
        {
        // Divide by 8 to get bytes from bits
        devExt->VolumeIV = FREEOTFE_MEMALLOC((devExt->VolumeIVLength / 8));
        RtlCopyMemory(
                    devExt->VolumeIV,
                    ptrVolumeIV,
                    (devExt->VolumeIVLength / 8)
                    );

        // NOTE: THE VOLUME IV IS ONLY DUMPED OUT IF DEBUG IS ENABLED; ONLY ON
        //       DEBUG BUILDS.
        //       Normal release builds cause DEBUGOUTMAINDRV to be a NOP, so
        //       this debug code gets removed
        for(i = 0; i < (devExt->VolumeIVLength / 8); i++)
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, ("----- VolumeIV [%.2d]: %.2x\n", i, devExt->VolumeIV[i]));
            }                                
        }


    // --- COPY THE METADATA ---
    devExt->MetaDataLength = DIOCBuffer->MetaDataLength;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Metadata length: %d bytes\n", devExt->MetaDataLength));

    devExt->MetaData = FREEOTFE_MEMALLOC(devExt->MetaDataLength);
	RtlCopyMemory(
					devExt->MetaData,
                    ptrMetaData,
					devExt->MetaDataLength
					);
    for(i = 0; i < (devExt->MetaDataLength); i++)
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, ("----- MetaData [%.2d]: %.2x\n", i, devExt->MetaData[i]));
        }                                


    // --- GET THE VOLUME FILE ---    
    devExt->MountSource = DIOCBuffer->MountSource;
    devExt->ReadOnly = DIOCBuffer->ReadOnly;

    if (
        (DIOCBuffer->StorageMediaType == RemovableMedia) ||
        EmulatingCDOrDVD(DeviceObject)
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Mount as removable.\n"));
        DeviceObject->Characteristics |= FILE_REMOVABLE_MEDIA;
        }

    DeviceObject->Characteristics &= ~FILE_READ_ONLY_DEVICE;
    if (devExt->ReadOnly) 
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Mount as readonly.\n"));
        DeviceObject->Characteristics |= FILE_READ_ONLY_DEVICE;
        }


    // Open the file
    // Note: We *don't* allow intermediate buffering as we want all of our
    //       reads/writes to hit the disk platters immediatly. This does
    //       reduce our flexability in reading/writing, but we can put up with
    //       that in exchange for the security (integrity) we get as a result.
    status = FileOpen(
                      &devExt->zzFilename,
                      devExt->ClientContext,
                      devExt->ReadOnly,
                      FALSE,
                      &devExt->FileHandle,
                      &Irp->IoStatus,
                      &devExt->FileAttributesStored,
                      &devExt->FileAttributes
                     );

    // Get file object pointer...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Getting file object pointer...\n"));
    devExt->FileObject = NULL;
    status = ObReferenceObjectByHandle(
                            devExt->FileHandle,
                            FILE_ALL_ACCESS,
                            *IoFileObjectType,
                            KernelMode,
                            &devExt->FileObject,
                            NULL
                           );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get file object pointer (status: %d).\n", status));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }

    // Junk our stored file attributes, if we're not going to restore them later
    // If VOL_FLAGS_NORMAL_TIMESTAMPS bit is *set*, we *don't* restore them
    if (devExt->VolumeFlags & VOL_FLAGS_NORMAL_TIMESTAMPS)
        {
        devExt->FileAttributesStored = FALSE;
        }

    // Store the size of the file/partition for later use...
    if (devExt->MountSource == MNTSRC_PARTITION) 
        {
        status = GetMaxSizePartition(
                                     &devExt->zzFilename,
                                     &determinedMaxSize
                                    );
        }
    else
        {
        status = GetMaxSizeFile(
                            devExt->FileHandle,
                            &determinedMaxSize
                           );
        }
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unknown max possible size (%d %d).\n", status, Irp->IoStatus.Status));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }                          
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Size of actual file/partition: %lld bytes\n", determinedMaxSize.QuadPart));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Starting offset: %lld bytes\n", DIOCBuffer->DataStart.QuadPart));


    // Encrypted data start/end offsets
    devExt->DataOffset = DIOCBuffer->DataStart;
    dataEnd = DIOCBuffer->DataEnd;
    if (dataEnd.QuadPart == 0)
        {
        dataEnd = determinedMaxSize;
        }

    // Sanity check...
    if (
        (devExt->DataOffset.QuadPart >= dataEnd.QuadPart) ||
        (dataEnd.QuadPart > determinedMaxSize.QuadPart)
        )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Invalid value passed as encrypted data start/end.\n"));
        status = STATUS_INVALID_PARAMETER;
        return status;
        }


    // The size of the partition
    devExt->PartitionSize.QuadPart = (dataEnd.QuadPart - devExt->DataOffset.QuadPart);


    // Store simulated disk geometry...
    // Note: The device characteristics must also take RemovableMedia into account
    // devExt->DiskGeometry.MediaType = Unknown;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Storage media type: %d\n", DIOCBuffer->StorageMediaType));
    if (
        (DIOCBuffer->StorageMediaType == RemovableMedia) ||
        EmulatingCDOrDVD(DeviceObject)
       )
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Mounting as RemovableMedia.\n"));
        devExt->DiskGeometry.MediaType = RemovableMedia;
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Mounting as FixedMedia.\n"));
        devExt->DiskGeometry.MediaType = FixedMedia;
        }
    
    // Encryption block size
    devExt->EncryptionBlockSize = ENCRYPTION_BLOCK_SIZE;

    // Disk geometry
    devExt->DiskGeometry.TracksPerCylinder = TRACKS_PER_CYLINDER;
    devExt->DiskGeometry.SectorsPerTrack   = SECTORS_PER_TRACK;

    // Emulated bytes per sector changes for CD/DVD emulated devices
    devExt->DiskGeometry.BytesPerSector = HDD_BYTES_PER_SECTOR;
    if (EmulatingCDOrDVD(DeviceObject))
        {
        devExt->DiskGeometry.BytesPerSector = CD_BYTES_PER_SECTOR;
        }

    // Note that in computing the number of cylinders, we add an additional track in.
    // This is because we add a "full track added to the partition". This is done so that the
    // "virtual disk geometry" is big enough to store both the encypted partition, *and* an
    // extra track (which doens't really exist) which represents the hidden sectors.
    // Hidden sectors required for the reasons outlined elsewhere in this source

    // For additional information on the hidden sectors, see:
    //   src\storage\class\disk\disk.c
    // in the Windows DDK
    // Specifically, search for "Test the partition alignment before getting to involved"
    // within this file

    // Personal comment: Example of real test HDD under Windows 2000 had:
    //   Disk geometry:
    //     Cylinders: 832
    //     TracksPerCylinder: 4
    //     SectorsPerTrack: 63
    //     BytesPerSector: 512
    //     Disk Size: 107347968
    //   Partition details(25MB, 26MB, 27MB; all primary partitions; the latter two 
    //   unformatted IIRC):
    //     First partition
    //       PartitionNumber 1
    //       PartitionType   4
    //       HiddenSectors   63
    //       StartingOffset  32256
    //       PartitionLength 26159616
    //     Second partition
    //       PartitionNumber 2
    //       PartitionType   6
    //       HiddenSectors   51156
    //       StartingOffset  26191872
    //       PartitionLength 27224064
    //     Third partition
    //       PartitionNumber 3
    //       PartitionType   6
    //       HiddenSectors   104328
    //       StartingOffset  53415936
    //       PartitionLength 28256256

    // Personal comment: Example of real test CD under Windows XP had:
    // CD had 2007040 of storage, according to MS Windows Explorer's properties page for it
    //   Disk geometry:
    //     Cylinders: 0
    //     TracksPerCylinder: 64
    //     SectorsPerTrack: 32
    //     BytesPerSector: 2048
    //     Signature: 1
    //     PartitionCount: 1
    //     First partition
    //       PartitionNumber 0
    //       PartitionType   11
    //       HiddenSectors   0
    //       StartingOffset  0
    //       PartitionLength 2007040
    //   CDROM TOC:
    //     Length[0]: 0
    //     Length[1]: 18
    //     FirstTrack: 1
    //     LastTrack: 1
    //     TrackData[0].Control: 4
    //     TrackData[0].Adr: 1
    //     TrackData[0].TrackNumber: 1
    //     TrackData[0].Address[0]: 0x00
    //     TrackData[0].Address[1]: 0x00
    //     TrackData[0].Address[2]: 0x02
    //     TrackData[0].Address[3]: 0x00
    //     TrackData[1].Control: 4
    //     TrackData[1].Adr: 1
    //     TrackData[1].TrackNumber: 170
    //     TrackData[1].Address[0]: 0x00
    //     TrackData[1].Address[1]: 0x00
    //     TrackData[1].Address[2]: 0x0f
    //     TrackData[1].Address[3]: 0x05

    // Personal comment: Example of real test DVD under Windows XP had:
    // DVD had 4479766528 of storage, according to MS Windows Explorer's properties page for it
    //   Disk geometry:
    //     Cylinders: 1068
    //     TracksPerCylinder: 64
    //     SectorsPerTrack: 32
    //     BytesPerSector: 2048
    //     Signature: 1
    //     PartitionCount: 1
    //     First partition
    //       PartitionNumber 0
    //       PartitionType   11
    //       HiddenSectors   0
    //       StartingOffset  0
    //       PartitionLength 4480303104
    //   CDROM TOC:
    //     Length[0]: 0
    //     Length[1]: 18
    //     FirstTrack: 1
    //     LastTrack: 1
    //     TrackData[0].Control: 7
    //     TrackData[0].Adr: 1
    //     TrackData[0].TrackNumber: 1
    //     TrackData[0].Address[0]: 0x00
    //     TrackData[0].Address[1]: 0x00
    //     TrackData[0].Address[2]: 0x00
    //     TrackData[0].Address[3]: 0x00
    //     TrackData[1].Control: 7
    //     TrackData[1].Adr: 1
    //     TrackData[1].TrackNumber: 170
    //     TrackData[1].Address[0]: 0x00
    //     TrackData[1].Address[1]: 0x21
    //     TrackData[1].Address[2]: 0x61
    //     TrackData[1].Address[3]: 0x80


    // For HDDs, note that we spoof a full track as being hidden.
    // This is done becase Windows 2000 cannot format the virtual volume as FAT/FAT32 if
    // the HiddenSectors and StartingOffset are 0
    // (Note: It will format as NTFS in that situation though... Weird?)
    devExt->HiddenSectors = 0;
    if (!(EmulatingCDOrDVD(DeviceObject)))
        {
        // An extra track for hidden sectors...
        devExt->HiddenSectors = devExt->DiskGeometry.SectorsPerTrack;
        }

    // Determine the number of cylinderes
    devExt->DiskGeometry.Cylinders.QuadPart =
        (
          // Encrypted partition size...
          devExt->PartitionSize.QuadPart +
          // Hidden sectors...
          (
           // Note: No "devExt->DiskGeometry.TracksPerCylinder" here
           devExt->HiddenSectors *
           devExt->DiskGeometry.BytesPerSector
          )
        ) / (
              devExt->DiskGeometry.TracksPerCylinder *
              devExt->DiskGeometry.SectorsPerTrack *
              devExt->DiskGeometry.BytesPerSector
            );
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cylinders initially: %lld\n", devExt->DiskGeometry.Cylinders.QuadPart));

    // Because the division may round down, we do a sanity check to ensure that the number of
    // cylinders is enough
    // But only for HDDs!
    if (!(EmulatingCDOrDVD(DeviceObject)))
        {
        if (
            (
            // Encrypted partition size...
            devExt->PartitionSize.QuadPart +
            // Hidden sectors...
            (
            devExt->HiddenSectors *
            devExt->DiskGeometry.BytesPerSector
            )
            ) > (
                devExt->DiskGeometry.Cylinders.QuadPart * 
                devExt->DiskGeometry.TracksPerCylinder *
                devExt->DiskGeometry.SectorsPerTrack *
                devExt->DiskGeometry.BytesPerSector
                )
        )
            {
            // Increment the number of cylinders by one
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cylinders rounded down; incrementing\n"));
            devExt->DiskGeometry.Cylinders.QuadPart++;
            }
        }
       

    // The size of the disk (i.e. including hidden sectors)
    devExt->DiskSize.QuadPart = 
            devExt->DiskGeometry.Cylinders.QuadPart *
            devExt->DiskGeometry.TracksPerCylinder *
            devExt->DiskGeometry.SectorsPerTrack *
            devExt->DiskGeometry.BytesPerSector;
        

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Simulated disk geometry is:\n"));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- MediaType        : %d\n", devExt->DiskGeometry.MediaType));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- TracksPerCylinder: %d\n", devExt->DiskGeometry.TracksPerCylinder));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- SectorsPerTrack  : %d\n", devExt->DiskGeometry.SectorsPerTrack));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- BytesPerSector   : %d\n", devExt->DiskGeometry.BytesPerSector));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Cylinders        : %lld\n", devExt->DiskGeometry.Cylinders.QuadPart));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- DiskSize         : %lld\n", devExt->DiskSize.QuadPart));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- PartitionSize    : %lld\n", devExt->PartitionSize.QuadPart));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Additionally:\n"));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Storage media type: %d\n", devExt->StorageMediaType));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Hidden sectors    : %lld\n", devExt->HiddenSectors));
    

    // Determine the lowest possible value which is a multiple of both SECTOR_SIZE and the
    // sector size of the partition/drive the volume file is stored on
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Determining underlying sector sizes...\n"));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Virtual drive's sector size: %d\n", devExt->DiskGeometry.BytesPerSector));
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("My deviceobject sector size: %d\n", DeviceObject->SectorSize));
    devExt->FileSectorSize = devExt->DiskGeometry.BytesPerSector;  // Default to not significant
    // fileHostDevice = devExt->FileObject->DeviceObject;  // This gives 0 for CDROM
    fileHostDevice = IoGetRelatedDeviceObject(devExt->FileObject);  // This gives 2048 for CDROM
    if (fileHostDevice != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("File sector size: %d\n", fileHostDevice->SectorSize));
        if (fileHostDevice->SectorSize > 0)
            {
            devExt->FileSectorSize = fileHostDevice->SectorSize;
            }
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Using file sector size: %d\n", devExt->FileSectorSize));
    tmpLargeInt.QuadPart = (devExt->DataOffset.QuadPart % devExt->DiskGeometry.BytesPerSector);
    devExt->DataOffsetModVirtualSectorSize = tmpLargeInt.LowPart;
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("DataOffsetModVirtualSectorSize: %d\n", devExt->DataOffsetModVirtualSectorSize));


    // Flag that the device is mounted  
    devExt->DismountPending = FALSE;
    devExt->Mounted = TRUE;
        
    // If we got this far, everything must be OK
    status = STATUS_SUCCESS;

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("File opened successfully.\n"));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ThreadMount\n"));

    return status;
}

// =========================================================================
// Identify if emulating a CD or DVD device
// Returns TRUE if emulating a CD/DVD device, otherwise FALSE
BOOLEAN
EmulatingCDOrDVD(
    IN PDEVICE_OBJECT DeviceObject
)
{
    PDEVICE_EXTENSION devExt;
    BOOLEAN isCDorDVD;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("EmulatingCDOrDVD\n"));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    isCDorDVD = (
                 (devExt->StorageMediaType == CD_ROM)  ||  // Opt_Disk - CD
                 (devExt->StorageMediaType == CD_R)    ||  // Opt_Disk - CD-Recordable (Write Once)
                 (devExt->StorageMediaType == CD_RW)   ||  // Opt_Disk - CD-Rewriteable
                 (devExt->StorageMediaType == DVD_ROM) ||  // Opt_Disk - DVD-ROM
                 (devExt->StorageMediaType == DVD_R)   ||  // Opt_Disk - DVD-Recordable (Write Once)
                 (devExt->StorageMediaType == DVD_RW)  ||  // Opt_Disk - DVD-Rewriteable
                 (devExt->StorageMediaType == DVD_RAM)     // Opt_Disk - DVD-RAM
                );

    if (isCDorDVD)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("YES - should emulate a CD/DVD\n"));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("NO - should NOT emulate a CD/DVD\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("EmulatingCDOrDVD\n"));

    return isCDorDVD;
}


// =========================================================================
// Dismount the specified device
// Returns status, but does *not* complete the IRP
NTSTATUS
ThreadDismount(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
)
{
    BOOLEAN tmpMounted;
    NTSTATUS status;
    PDEVICE_EXTENSION devExt;
    PIO_STACK_LOCATION irpSp;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ThreadDismount\n"));

    status = STATUS_SUCCESS;

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);
    
    // Flag as unmounted
    tmpMounted = devExt->Mounted;
    devExt->Mounted = FALSE;

    status = FileClose(
                       &devExt->FileHandle,
                       devExt->FileAttributesStored,
                       &devExt->FileAttributes,
                       &Irp->IoStatus
                      );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to close file,\n"));
        status = STATUS_INVALID_PARAMETER;  
        // Restore previous
        devExt->Mounted = tmpMounted;
        return status;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   

    OverwriteDeviceSensitive(DeviceObject);

    // Flag as not dismount pending
    devExt->DismountPending = FALSE;
    
    // If we got this far, everything must be OK
    status = STATUS_SUCCESS;

    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("File closed successfully.\n"));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ThreadDismount\n"));

    return status;
}


// =========================================================================
// Populate devExt with all details relevant to the hash driver to be used
// Note: Must use ObDereferenceObject to free off, according to:
//       http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm
// This freeing off is handled automatically if you call FreeDeviceDetailsHash(...)
NTSTATUS
GetDeviceDetailsHash(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_HASH* HashDetails
)
{
    NTSTATUS status;
    DIOC_HASH_INTLDETAILS DIOCBuffer;
    PIRP tmpIrp;
    PIO_STACK_LOCATION irpSp;
    KEVENT event;
    ANSI_STRING tmpANSIname;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetDeviceDetailsHash\n"));
    
    tmpIrp = NULL;

    
    // --- UNICODE HASH DEVICENAME ---
    // Unicode the filename and copy into "devObj->HashDeviceName"
    // This is +1 in order to allow for the NULL terminator
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unicoding hash devicename to devExt.\n"));
    tmpANSIname.Length = (USHORT)strlen(deviceName);
    tmpANSIname.MaximumLength = tmpANSIname.Length + 1;
    tmpANSIname.Buffer = FREEOTFE_MEMALLOC(tmpANSIname.MaximumLength);
    RtlZeroMemory(
                tmpANSIname.Buffer,
                tmpANSIname.MaximumLength
                );
    RtlCopyMemory(
                  tmpANSIname.Buffer,
                  deviceName,
                  tmpANSIname.Length
                 );

    // +1 in order to include the terminating NULL
    HashDetails->DeviceName.MaximumLength = (strlen(deviceName) + 1) * sizeof(WCHAR);
    HashDetails->DeviceName.Length = 0;
    HashDetails->DeviceName.Buffer = FREEOTFE_MEMALLOC(HashDetails->DeviceName.MaximumLength);
    RtlZeroMemory(HashDetails->DeviceName.Buffer, HashDetails->DeviceName.MaximumLength);
    status = RtlAnsiStringToUnicodeString(
                                          &(HashDetails->DeviceName),
                                          &tmpANSIname,
                                          FALSE
                                         );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Failed to RtlAnsiStringToUnicodeString hash devicename.\n"));                
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Hash devicename is: %ls\n", HashDetails->DeviceName.Buffer));    
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Hash devicename length: %d %d\n", HashDetails->DeviceName.Length, HashDetails->DeviceName.MaximumLength));
        }

    FREEOTFE_FREE(tmpANSIname.Buffer);

    HashDetails->HashGUID = *supportGUID;

    if (NT_SUCCESS(status))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoGetDeviceObjectPointer...\n"));
        status = IoGetDeviceObjectPointer(
                                          &(HashDetails->DeviceName),
                                          FILE_ALL_ACCESS,
                                          &(HashDetails->FileObject),
                                          &(HashDetails->DeviceObject)
                                         ); 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got ptr to hash device: status: %d (want status: %d)\n", status, STATUS_SUCCESS));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to get device object for devicename: %ls\n", HashDetails->DeviceName.Buffer));
            }
        }
        

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoAllocateIrp...\n"));
        tmpIrp = IoAllocateIrp(
                               HashDetails->DeviceObject->StackSize,
                               FALSE
                              );
                          
        if (tmpIrp == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoAllocateIrp failure.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        }
    

    if (NT_SUCCESS(status))
        {                         
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up IRP...\n"));
        tmpIrp->Flags = (IRP_BUFFERED_IO | IRP_INPUT_OPERATION);
        tmpIrp->IoStatus.Status = STATUS_NOT_SUPPORTED;
    
        irpSp = IoGetNextIrpStackLocation(tmpIrp);

        irpSp->MajorFunction = IRP_MJ_DEVICE_CONTROL;

        // IOCTL_FREEOTFEHASH_INTLDETAILS uses METHOD_BUFFERED, so we use
        // "AssociatedIrp.SystemBuffer"
        tmpIrp->AssociatedIrp.SystemBuffer = &DIOCBuffer;

        irpSp->Parameters.DeviceIoControl.OutputBufferLength = sizeof(DIOCBuffer);
        irpSp->Parameters.DeviceIoControl.InputBufferLength  = 0;
        irpSp->Parameters.DeviceIoControl.IoControlCode      = IOCTL_FREEOTFEHASH_INTLDETAILS;
    

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up event and completion routine...\n"));
        KeInitializeEvent(&event, NotificationEvent, FALSE);        
        IoSetCompletionRoutine(tmpIrp, SynchCompletionRoutine, &event, TRUE, TRUE, TRUE);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoCallDriver...\n"));
        status = IoCallDriver(HashDetails->DeviceObject, tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoCallDriver done.\n"));

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoCallDriver failed.\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to wait for event...\n"));
            KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Event got.\n"));
            }            
        }
    

    if (NT_SUCCESS(status))
        {
        // Irp is complete, grab the status and free the irp
        status = tmpIrp->IoStatus.Status;

        if (status == STATUS_SUCCESS)
            {    
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Status OK - storing function pointers.\n"));
            HashDetails->FnHash = DIOCBuffer.FnHash;

            // Get the details of the specific hash to be used
            status = DIOCBuffer.FnHashDetails(supportGUID, &(HashDetails->Details));
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get specific hash details\n"));
                }
            }
        }


    // Cleanup...
    if (tmpIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoFreeIrp...\n"));
        IoFreeIrp(tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoFreeIrp done.\n"));
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetDeviceDetailsHash\n"));

    return status;
}


// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsHash(...)
void
FreeDeviceDetailsHash(
    OUT     MODULE_DETAILS_HASH* HashDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeDeviceDetailsHash\n"));

    // ...hash devicename
    if (HashDetails->DeviceName.Buffer != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off device name unicode buffer...\n")); 
        SecZeroMemory(
                        HashDetails->DeviceName.Buffer,
                        sizeof(HashDetails->DeviceName.MaximumLength)
                        );
        FREEOTFE_FREE(HashDetails->DeviceName.Buffer);
        HashDetails->DeviceName.Buffer = NULL;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }

    // ...GUID
    SecZeroMemory(
                &(HashDetails->HashGUID),
                sizeof(HashDetails->HashGUID)
                );

    // ...any hash internal details
    SecZeroMemory(
                &(HashDetails->Details),
                sizeof(HashDetails->Details)
                );
    HashDetails->FnHash = NULL;

    // ...dereference the hash device
    // http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm - says use ObDereferenceObject
    if (HashDetails->FileObject != NULL)
        {
        ObDereferenceObject(HashDetails->FileObject);
        }

    // ...any driver pointers/references
    HashDetails->FileObject = NULL;
    HashDetails->DeviceObject = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeDeviceDetailsHash\n"));
}


// =========================================================================
// Call GetDeviceDetailsCypher_v3(...), falling back to
// GetDeviceDetailsCypher_v1(...) if that fails
NTSTATUS
GetDeviceDetailsCypher_v3_v1(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    NTSTATUS status;
    MODULE_DETAILS_CYPHER_v1 tmpCypherDetails_v1;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetDeviceDetailsCypher_v3_v1\n"));

    status = GetDeviceDetailsCypher_v3(deviceName, supportGUID, CypherDetails);
    if (!(NT_SUCCESS(status)))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("GetDeviceDetailsCypher_v3 failed; falling back to GetDeviceDetailsCypher_v1...\n"));
        status = GetDeviceDetailsCypher_v1(
                                           deviceName, 
                                           supportGUID,
                                           &tmpCypherDetails_v1
                                          );
        if (NT_SUCCESS(status))
            { 
            // Move into v3 structure
            MoveMODULE_DETAILS_CYPHER_v1ToMODULE_DETAILS_CYPHER_v3(
                &tmpCypherDetails_v1,
                CypherDetails
                );
            }

        // DO NOT FREE OFF/CLEARDOWN tmpCypherDetails_v1!
        // Its content is being used by tmpCypherDetails_v1
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetDeviceDetailsCypher_v3_v1\n"));

    return status;
}

// =========================================================================
// Populate devExt with all details relevant to the cypher driver to be used
// Note: Must use ObDereferenceObject to free off, according to:
//       http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm
// This freeing off is handled automatically if you call FreeDeviceDetailsCypher(...)
NTSTATUS
GetDeviceDetailsCypher_v1(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
)
{
    NTSTATUS status;
    DIOC_CYPHER_INTL_DETAILS_v1 DIOCBuffer;
    PIRP tmpIrp;
    PIO_STACK_LOCATION irpSp;
    KEVENT event;
    ANSI_STRING tmpANSIname;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetDeviceDetailsCypher_v1\n"));
    
    tmpIrp = NULL;

 
    // --- UNICODE CYPHER DEVICENAME ---
    // Unicode the filename and copy into "devObj->CypherDeviceName"
    // This is +1 in order to allow for the NULL terminator
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unicoding cypher devicename to devExt.\n"));
    tmpANSIname.Length = (USHORT)strlen(deviceName);
    tmpANSIname.MaximumLength = tmpANSIname.Length + 1;
    tmpANSIname.Buffer = FREEOTFE_MEMALLOC(tmpANSIname.MaximumLength);
    RtlZeroMemory(
                tmpANSIname.Buffer,
                tmpANSIname.MaximumLength
                );
    RtlCopyMemory(
                  tmpANSIname.Buffer,
                  deviceName,
                  tmpANSIname.Length
                 );

    // +1 in order to include the terminating NULL
    CypherDetails->DeviceName.MaximumLength = (strlen(deviceName) + 1) * sizeof(WCHAR);
    CypherDetails->DeviceName.Length = 0;
    CypherDetails->DeviceName.Buffer = FREEOTFE_MEMALLOC(CypherDetails->DeviceName.MaximumLength);
    RtlZeroMemory(
                  CypherDetails->DeviceName.Buffer, 
                  CypherDetails->DeviceName.MaximumLength
                 );
    status = RtlAnsiStringToUnicodeString(
                                          &(CypherDetails->DeviceName),
                                          &tmpANSIname,
                                          FALSE
                                         );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Failed to RtlAnsiStringToUnicodeString cypher devicename.\n"));                
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher devicename is: %ls\n", CypherDetails->DeviceName.Buffer));    
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher devicename length: %d %d\n", CypherDetails->DeviceName.Length, CypherDetails->DeviceName.MaximumLength));
        }

    FREEOTFE_FREE(tmpANSIname.Buffer);

    CypherDetails->CypherGUID = *supportGUID;

    if (NT_SUCCESS(status))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoGetDeviceObjectPointer...\n"));
        status = IoGetDeviceObjectPointer(
                                          &(CypherDetails->DeviceName),
                                          FILE_ALL_ACCESS,
                                          &(CypherDetails->FileObject),
                                          &(CypherDetails->DeviceObject)
                                         ); 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got ptr to cypher device: status: %d (want status: %d)\n", status, STATUS_SUCCESS));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to get device object for devicename: %ls\n", CypherDetails->DeviceName.Buffer));
            }
        }
        

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoAllocateIrp...\n"));
        tmpIrp = IoAllocateIrp(
                               CypherDetails->DeviceObject->StackSize,
                               FALSE
                              );
                          
        if (tmpIrp == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoAllocateIrp failure.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        }
    

    if (NT_SUCCESS(status))
        {                         
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up IRP...\n"));
        tmpIrp->Flags = (IRP_BUFFERED_IO | IRP_INPUT_OPERATION);
        tmpIrp->IoStatus.Status = STATUS_NOT_SUPPORTED;
    
        irpSp = IoGetNextIrpStackLocation(tmpIrp);

        irpSp->MajorFunction = IRP_MJ_DEVICE_CONTROL;

        // IOCTL_FreeOTFECypher_INTLDETAILS uses METHOD_BUFFERED, so we use
        // "AssociatedIrp.SystemBuffer"
        tmpIrp->AssociatedIrp.SystemBuffer = &DIOCBuffer;

        irpSp->Parameters.DeviceIoControl.OutputBufferLength = sizeof(DIOCBuffer);
        irpSp->Parameters.DeviceIoControl.InputBufferLength  = 0;
        irpSp->Parameters.DeviceIoControl.IoControlCode      = IOCTL_FreeOTFECypher_INTLDETAILS_v1;
    

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up event and completion routine...\n"));
        KeInitializeEvent(&event, NotificationEvent, FALSE);        
        IoSetCompletionRoutine(tmpIrp, SynchCompletionRoutine, &event, TRUE, TRUE, TRUE);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoCallDriver...\n"));
        status = IoCallDriver(CypherDetails->DeviceObject, tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoCallDriver done.\n"));

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoCallDriver failed.\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to wait for event...\n"));
            KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Event got.\n"));
            }            
        }
    

    if (NT_SUCCESS(status))
        {
        // Irp is complete, grab the status and free the irp
        status = tmpIrp->IoStatus.Status;

        if (status == STATUS_SUCCESS)
            {    
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Status OK - storing function pointers.\n"));
            CypherDetails->FnEncrypt = DIOCBuffer.FnEncrypt;
            CypherDetails->FnDecrypt = DIOCBuffer.FnDecrypt;

            // Get the details of the specific cypher to be used
            status = DIOCBuffer.FnCypherDetails_v1(supportGUID, &(CypherDetails->Details));
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get specific cypher details\n"));
                }
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to call and get cypher driver internal details?!\n"));
            }
        }



    // Cleanup...
    if (tmpIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoFreeIrp...\n"));
        IoFreeIrp(tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoFreeIrp done.\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetDeviceDetailsCypher_v1\n"));

    return status;
}

// =========================================================================
// Populate devExt with all details relevant to the cypher driver to be used
// Note: Must use ObDereferenceObject to free off, according to:
//       http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm
// This freeing off is handled automatically if you call FreeDeviceDetailsCypher(...)
NTSTATUS
GetDeviceDetailsCypher_v3(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    NTSTATUS status;
    DIOC_CYPHER_INTL_DETAILS_v3 DIOCBuffer;
    PIRP tmpIrp;
    PIO_STACK_LOCATION irpSp;
    KEVENT event;
    ANSI_STRING tmpANSIname;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetDeviceDetailsCypher_v3\n"));
    
    tmpIrp = NULL;

 
    // --- UNICODE CYPHER DEVICENAME ---
    // Unicode the filename and copy into "devObj->CypherDeviceName"
    // This is +1 in order to allow for the NULL terminator
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Unicoding cypher devicename to devExt.\n"));
    tmpANSIname.Length = (USHORT)strlen(deviceName);
    tmpANSIname.MaximumLength = tmpANSIname.Length + 1;
    tmpANSIname.Buffer = FREEOTFE_MEMALLOC(tmpANSIname.MaximumLength);
    RtlZeroMemory(
                tmpANSIname.Buffer,
                tmpANSIname.MaximumLength
                );
    RtlCopyMemory(
                  tmpANSIname.Buffer,
                  deviceName,
                  tmpANSIname.Length
                 );

    // +1 in order to include the terminating NULL
    CypherDetails->DeviceName.MaximumLength = (strlen(deviceName) + 1) * sizeof(WCHAR);
    CypherDetails->DeviceName.Length = 0;
    CypherDetails->DeviceName.Buffer = FREEOTFE_MEMALLOC(CypherDetails->DeviceName.MaximumLength);
    RtlZeroMemory(
                  CypherDetails->DeviceName.Buffer, 
                  CypherDetails->DeviceName.MaximumLength
                 );
    status = RtlAnsiStringToUnicodeString(
                                          &(CypherDetails->DeviceName),
                                          &tmpANSIname,
                                          FALSE
                                         );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Failed to RtlAnsiStringToUnicodeString cypher devicename.\n"));                
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher devicename is: %ls\n", CypherDetails->DeviceName.Buffer));    
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Cypher devicename length: %d %d\n", CypherDetails->DeviceName.Length, CypherDetails->DeviceName.MaximumLength));
        }

    FREEOTFE_FREE(tmpANSIname.Buffer);

    CypherDetails->CypherGUID = *supportGUID;

    if (NT_SUCCESS(status))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoGetDeviceObjectPointer...\n"));
        status = IoGetDeviceObjectPointer(
                                          &(CypherDetails->DeviceName),
                                          FILE_ALL_ACCESS,
                                          &(CypherDetails->FileObject),
                                          &(CypherDetails->DeviceObject)
                                         ); 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got ptr to cypher device: status: %d (want status: %d)\n", status, STATUS_SUCCESS));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to get device object for devicename: %ls\n", CypherDetails->DeviceName.Buffer));
            }
        }
        

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoAllocateIrp...\n"));
        tmpIrp = IoAllocateIrp(
                               CypherDetails->DeviceObject->StackSize,
                               FALSE
                              );
                          
        if (tmpIrp == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoAllocateIrp failure.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        }
    

    if (NT_SUCCESS(status))
        {                         
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up IRP...\n"));
        tmpIrp->Flags = (IRP_BUFFERED_IO | IRP_INPUT_OPERATION);
        tmpIrp->IoStatus.Status = STATUS_NOT_SUPPORTED;
    
        irpSp = IoGetNextIrpStackLocation(tmpIrp);

        irpSp->MajorFunction = IRP_MJ_DEVICE_CONTROL;

        // IOCTL_FreeOTFECypher_INTLDETAILS uses METHOD_BUFFERED, so we use
        // "AssociatedIrp.SystemBuffer"
        tmpIrp->AssociatedIrp.SystemBuffer = &DIOCBuffer;

        irpSp->Parameters.DeviceIoControl.OutputBufferLength = sizeof(DIOCBuffer);
        irpSp->Parameters.DeviceIoControl.InputBufferLength  = 0;
        irpSp->Parameters.DeviceIoControl.IoControlCode      = IOCTL_FreeOTFECypher_INTLDETAILS_v3;
    

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up event and completion routine...\n"));
        KeInitializeEvent(&event, NotificationEvent, FALSE);        
        IoSetCompletionRoutine(tmpIrp, SynchCompletionRoutine, &event, TRUE, TRUE, TRUE);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoCallDriver...\n"));
        status = IoCallDriver(CypherDetails->DeviceObject, tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoCallDriver done.\n"));

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoCallDriver failed.\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to wait for event...\n"));
            KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Event got.\n"));
            }            
        }
    

    if (NT_SUCCESS(status))
        {
        // Irp is complete, grab the status and free the irp
        status = tmpIrp->IoStatus.Status;

        if (status == STATUS_SUCCESS)
            {    
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Status OK - storing function pointers.\n"));
            CypherDetails->FnEncrypt = DIOCBuffer.FnEncrypt;
            CypherDetails->FnDecrypt = DIOCBuffer.FnDecrypt;
            CypherDetails->FnEncryptSector = DIOCBuffer.FnEncryptSector;
            CypherDetails->FnDecryptSector = DIOCBuffer.FnDecryptSector;

            // Get the details of the specific cypher to be used
            status = DIOCBuffer.FnCypherDetails_v3(supportGUID, &(CypherDetails->Details));
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get specific cypher details\n"));
                }
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to call and get cypher driver internal details?!\n"));
            }
        }



    // Cleanup...
    if (tmpIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoFreeIrp...\n"));
        IoFreeIrp(tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoFreeIrp done.\n"));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetDeviceDetailsCypher_v3\n"));

    return status;
}


// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v1(
    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeDeviceDetailsCypher_v1\n"));

    if (CypherDetails->DeviceName.Buffer != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off cypher device name unicode buffer...\n")); 
        SecZeroMemory(
                    CypherDetails->DeviceName.Buffer,
                    sizeof(CypherDetails->DeviceName.MaximumLength)
                    );
        FREEOTFE_FREE(CypherDetails->DeviceName.Buffer);
        CypherDetails->DeviceName.Buffer = NULL;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }

    // ...GUID
    SecZeroMemory(
                &(CypherDetails->CypherGUID),
                sizeof(CypherDetails->CypherGUID)
                );

    // ...any cypher internal details
    SecZeroMemory(
                &(CypherDetails->Details),
                sizeof(CypherDetails->Details)
                );
    CypherDetails->FnEncrypt       = NULL;
    CypherDetails->FnDecrypt       = NULL;

    // ...dereference the cypher device
    // http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm - says use ObDereferenceObject
    if (CypherDetails->FileObject != NULL)
        {
        ObDereferenceObject(CypherDetails->FileObject);
        }

    // ...any driver pointers/references
    CypherDetails->FileObject = NULL;
    CypherDetails->DeviceObject = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeDeviceDetailsCypher_v1\n"));
}


// =========================================================================
// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsCypher(...)
void
FreeDeviceDetailsCypher_v3(
    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FreeDeviceDetailsCypher_v3\n"));

    if (CypherDetails->DeviceName.Buffer != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Freeing off cypher device name unicode buffer...\n")); 
        SecZeroMemory(
                    CypherDetails->DeviceName.Buffer,
                    sizeof(CypherDetails->DeviceName.MaximumLength)
                    );
        FREEOTFE_FREE(CypherDetails->DeviceName.Buffer);
        CypherDetails->DeviceName.Buffer = NULL;
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }

    // ...GUID
    SecZeroMemory(
                &(CypherDetails->CypherGUID),
                sizeof(CypherDetails->CypherGUID)
                );

    // ...any cypher internal details
    SecZeroMemory(
                &(CypherDetails->Details),
                sizeof(CypherDetails->Details)
                );
    CypherDetails->FnEncrypt       = NULL;
    CypherDetails->FnDecrypt       = NULL;
    CypherDetails->FnEncryptSector = NULL;
    CypherDetails->FnDecryptSector = NULL;

    // ...dereference the cypher device
    // http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm - says use ObDereferenceObject
    if (CypherDetails->FileObject != NULL)
        {
        ObDereferenceObject(CypherDetails->FileObject);
        }

    // ...any driver pointers/references
    CypherDetails->FileObject = NULL;
    CypherDetails->DeviceObject = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FreeDeviceDetailsCypher_v3\n"));
}


// =========================================================================
// This function is a modified version of "ParSynchCompletionRoutine" from
// utils.c in the MS DDK
// Routine Description:
//    This routine is for use with synchronous IRP processing.
//    All it does is signal an event, so the driver knows it
//    can continue.
// Arguments:
//    DriverObject - Pointer to driver object created by system.
//    Irp          - Irp that just completed
//    Event        - Event we'll signal to say Irp is done
NTSTATUS
SynchCompletionRoutine(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PKEVENT Event
    )
{
    UNREFERENCED_PARAMETER(DeviceObject);
    UNREFERENCED_PARAMETER(Irp);

    KeSetEvent(Event, 0, FALSE);
    return STATUS_MORE_PROCESSING_REQUIRED;
}


// =========================================================================
// Determine the max size of a file
NTSTATUS
GetMaxSizeFile(
    IN  HANDLE FileHandle,
    OUT PLARGE_INTEGER MaxSize
)
{
    NTSTATUS status;
    IO_STATUS_BLOCK ioStatusBlock;
    FILE_STANDARD_INFORMATION fileStdInfo;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetMaxSizeFile\n"));

    // Store the size of the file for later use...
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Attempting to get FILE size...\n")); 

    status = ZwQueryInformationFile(
                                    FileHandle,
                                    &ioStatusBlock,
                                    &fileStdInfo,
                                    sizeof(fileStdInfo),
                                    FileStandardInformation
                                    );
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Successfully got max size (%d %d).\n", status, ioStatusBlock.Status));
        *MaxSize = fileStdInfo.EndOfFile;
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Failed to get file size (%d %d).\n", status, ioStatusBlock.Status));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetMaxSizeFile\n"));

    return status;;
}


// =========================================================================
// Determine the max size of a partition
// DeviceName - The device name of the partition
//              e.g. \Device\Harddisk1\Partition1
NTSTATUS
GetMaxSizePartition(
    IN  PUNICODE_STRING DeviceName,
    OUT PLARGE_INTEGER MaxSize
)
{
    NTSTATUS status;
    PARTITION_INFORMATION partitionInfo;
    PIRP tmpIrp;
    PIO_STACK_LOCATION irpSp;
    KEVENT event;
	PFILE_OBJECT fileObject;
	PDEVICE_OBJECT deviceObject;
    UNICODE_STRING devName;
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("GetMaxSizePartition\n"));
    
    tmpIrp = NULL;

    fileObject = NULL;
    deviceObject = NULL;

    status = STATUS_SUCCESS;


    if (NT_SUCCESS(status))
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoGetDeviceObjectPointer...\n"));
        status = IoGetDeviceObjectPointer(
                                          DeviceName,
                                          FILE_READ_ATTRIBUTES,
                                          &fileObject,
                                          &deviceObject
                                         ); 
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got ptr to device: status: %d (want status: %d)\n", status, STATUS_SUCCESS));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to get device object for devicename: %ls\n", DeviceName->Buffer));
            fileObject = NULL;
            deviceObject = NULL;
            }
        }


    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoAllocateIrp...\n"));
        tmpIrp = IoAllocateIrp(
                               deviceObject->StackSize,
                               FALSE
                              );
                          
        if (tmpIrp == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoAllocateIrp failure.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        }
    

    if (NT_SUCCESS(status))
        {                         
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up IRP...\n"));
        tmpIrp->Flags = (IRP_BUFFERED_IO | IRP_INPUT_OPERATION);
        tmpIrp->IoStatus.Status = STATUS_NOT_SUPPORTED;
    
        irpSp = IoGetNextIrpStackLocation(tmpIrp);

        irpSp->MajorFunction = IRP_MJ_DEVICE_CONTROL;

        // It uses METHOD_BUFFERED, so we use "AssociatedIrp.SystemBuffer"
        tmpIrp->AssociatedIrp.SystemBuffer = &partitionInfo;

        irpSp->Parameters.DeviceIoControl.OutputBufferLength = sizeof(partitionInfo);
        irpSp->Parameters.DeviceIoControl.InputBufferLength  = 0;
        irpSp->Parameters.DeviceIoControl.IoControlCode      = IOCTL_DISK_GET_PARTITION_INFO;
    

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting up event and completion routine...\n"));
        KeInitializeEvent(&event, NotificationEvent, FALSE);        
        IoSetCompletionRoutine(tmpIrp, SynchCompletionRoutine, &event, TRUE, TRUE, TRUE);

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoCallDriver...\n"));
        status = IoCallDriver(deviceObject, tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoCallDriver done.\n"));

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("IoCallDriver failed.\n"));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to wait for event...\n"));
            KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Event got.\n"));
            }            
        }
    

    if (NT_SUCCESS(status))
        {
        // Irp is complete, grab the status and free the irp
        status = tmpIrp->IoStatus.Status;

        if (status == STATUS_SUCCESS)
            {    
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Status OK - setting up to return partition size.\n"));
            *MaxSize = partitionInfo.PartitionLength;
            }
        }


    // Cleanup...
    if (tmpIrp != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("About to IoFreeIrp...\n"));
        IoFreeIrp(tmpIrp);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("IoFreeIrp done.\n"));
        }

    // ...dereference the hash device
    // http://www.osr.com/ddk/kmarch/devobjts_8zdz.htm - says use ObDereferenceObject
    if (fileObject != NULL)
        {
        ObDereferenceObject(fileObject);
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("GetMaxSizePartition\n"));

    return status;
}


// =========================================================================
NTSTATUS
ClientSecurityCreate(
    PSECURITY_CLIENT_CONTEXT *ClientContext  // Pointer to a pointer
)
{
    NTSTATUS status;
    SECURITY_QUALITY_OF_SERVICE ClientSecurityQos;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ClientSecurityCreate\n"));

    status = STATUS_INTERNAL_ERROR;

    // Only go ahead if the pointer points to a pointer which is notNULL
    if (ClientContext != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ClientContext ptr points to pointer...\n"));
        // Information required for impersonation
        ClientSecurityQos.Length = sizeof(ClientSecurityQos);
        ClientSecurityQos.ImpersonationLevel = SecurityImpersonation;
        ClientSecurityQos.ContextTrackingMode = SECURITY_STATIC_TRACKING;
        ClientSecurityQos.EffectiveOnly = FALSE;

        *ClientContext = FREEOTFE_MEMALLOC(sizeof(*(*ClientContext)));
        status = SeCreateClientSecurity(
                                        PsGetCurrentThread(),
                                        &ClientSecurityQos,
                                        FALSE, // Not a remote client
                                        (*ClientContext)
                                    );
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ClientSecurityCreate\n"));

    return status;
}


// =========================================================================
NTSTATUS
ClientSecurityDestroy(
    PSECURITY_CLIENT_CONTEXT *ClientContext  // Pointer to a pointer
)
{
    NTSTATUS status;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("ClientSecurityDestroy\n"));

    status = STATUS_INTERNAL_ERROR;

    if (ClientContext != NULL)
        {
        // Dereferencing the pointer we're passed in *must* give a valid
        // (not NULL) pointer
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("ClientContext ptr points to pointer. Deleting...\n"));
        if ((*ClientContext) != NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...Deleting client security...\n"));
            SeDeleteClientSecurity((*ClientContext));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...Clearing memory used...\n"));
            SecZeroMemory(*ClientContext, sizeof(*(*ClientContext)));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...Freeing memory used...\n"));
            FREEOTFE_FREE(*ClientContext);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("...NULLing pointer's pointer\n"));
            *ClientContext = NULL;
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("ClientSecurityDestroy\n"));

    return status;
}


// =========================================================================
// Note: Specifying IntermediateBufferingOK ("FILE_NO_INTERMEDIATE_BUFFERING")
//       to FALSE requires all offsets and read/writes to be a multiple of the 
//       sector size - reducing flexability.
//       !! WARNING !!
//       !! WARNING !!
//       This may be a requirement ANYWAY with partition access
//       !! WARNING !!
//       !! WARNING !!
// AttributesStored - May be set to NULL if not needed
// FileAttributes - May be set to NULL if not needed
// If AttributesStored is NULL, FileAttributes must also be NULL, and vice
// versa
NTSTATUS
FileOpen(
    IN      PUNICODE_STRING Filename,
    IN      PSECURITY_CLIENT_CONTEXT ClientContext,
    IN      FREEOTFEBOOL ReadOnly,
    IN      FREEOTFEBOOL IntermediateBufferingOK,
    OUT     PHANDLE FileHandle,
    OUT     PIO_STATUS_BLOCK IoStatusBlock,
    OUT     PBOOLEAN AttributesStored,
    OUT     PFILE_BASIC_INFORMATION FileAttributes
)
{
    NTSTATUS status;
    OBJECT_ATTRIBUTES fileObjAttribs;
    IO_STATUS_BLOCK ioStatusBlock;
    ACCESS_MASK desiredAccess;
    NTSTATUS tmpStatus;
    ULONG createOptions;
    ULONG shareAccess;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FileOpen\n"));

    status = STATUS_SUCCESS;


    if (ClientContext != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Setting impersonation...\n"));
        status = SeImpersonateClientEx(ClientContext, NULL);
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to impersonate (%d).\n", status));
            }
        }

    if (NT_SUCCESS(status))
        {
        // Setup the filename to an object
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Filename: %ls\n", Filename->Buffer));
        InitializeObjectAttributes(
                                &fileObjAttribs,
                                Filename,
                                (OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE),
                                NULL, // Root dir not needed since it's a fully
                                        // qualified name
                                NULL  // Security attribs
                                );

        desiredAccess = (
                         GENERIC_READ           |
                         GENERIC_WRITE          |
                         FILE_READ_ATTRIBUTES   |
                         FILE_WRITE_ATTRIBUTES
                        );
        if (ReadOnly) 
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Opening as readonly.\n"));
            // Remove write access...
            desiredAccess &= ~GENERIC_WRITE;
            }


        shareAccess = (FILE_SHARE_READ | FILE_SHARE_WRITE);

        createOptions = (
                         FILE_NON_DIRECTORY_FILE       |
                         FILE_RANDOM_ACCESS            |
                         FILE_WRITE_THROUGH            |
                         FILE_SYNCHRONOUS_IO_NONALERT
                        );
        if (!(IntermediateBufferingOK))
            {
            createOptions |= FILE_NO_INTERMEDIATE_BUFFERING;
            }


        // Open the file
        status = ZwCreateFile(
                            FileHandle,
                            desiredAccess,
                            &fileObjAttribs,
                            IoStatusBlock,
                            NULL,
                            FILE_ATTRIBUTE_NORMAL,
                            shareAccess,
                            FILE_OPEN,  // Open existing file; fail if it doesn't exist
                            createOptions,
                            NULL,
                            0
                            );
        if (!(NT_SUCCESS(status)))
            {
            // Retry, ditching FILE_WRITE_ATTRIBUTES (e.g. may be a CDROM)...
            desiredAccess &= ~FILE_WRITE_ATTRIBUTES;

            DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to open with attributes writable, ditching them and retrying...\n"));
            status = ZwCreateFile(
                                FileHandle,
                                desiredAccess,
                                &fileObjAttribs,
                                IoStatusBlock,
                                NULL,
                                FILE_ATTRIBUTE_NORMAL,
                                shareAccess,
                                FILE_OPEN,  // Open existing file; fail if it doesn't exist
                                createOptions,
                                NULL,
                                0
                                );
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to open file (%d %d).\n", status, IoStatusBlock->Status));
                status = STATUS_INVALID_PARAMETER;  
                }
            }

        if (NT_SUCCESS(status))
            {
            if (
                (FileAttributes == NULL) ||
                (AttributesStored == NULL)
               )
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Not bothering to get file attributes.\n"));
                }
            else
                {
                // Store the file timestamps/attributes for later use...
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Attempting to get file timestamps/attributes...\n")); 
                tmpStatus = ZwQueryInformationFile(
                                                *FileHandle,
                                                IoStatusBlock,
                                                FileAttributes,
                                                sizeof(*FileAttributes),
                                                FileBasicInformation
                                            );
                *AttributesStored = NT_SUCCESS(tmpStatus);
                if (!(*AttributesStored))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to get file timestamps/attributes (%d %d).\n", status, IoStatusBlock->Status));
                    }                          
                }
            }

        // Revert impersonation
        if (ClientContext != NULL)
            {
            PsRevertToSelf();
            }
        }


    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FileOpen\n"));

    return status;
}


// =========================================================================
// FileAttributes - May be set to NULL if AttributesStored is FALSE
NTSTATUS
FileClose(
    IN OUT  PHANDLE FileHandle,
    IN      FREEOTFEBOOL AttributesStored,
    IN      PFILE_BASIC_INFORMATION FileAttributes,
    OUT     PIO_STATUS_BLOCK IoStatusBlock
)
{
    NTSTATUS status;
    NTSTATUS tmpStatus;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("FileClose\n"));

    status = STATUS_SUCCESS;


    if (FileHandle != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Valid pointer to a filehandle passed in...\n"));

        // Sanity; the filehandle's valid, right?
        if (*FileHandle != NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Filehandle pointed to is not NULL...\n"));
            // Reset file timestamps/attributes
            if (
                (AttributesStored) &&
                (FileAttributes != NULL)
               )
                {
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Resetting file timestamps/attributes...\n"));   
                tmpStatus = ZwSetInformationFile(
                                            *FileHandle,
                                            IoStatusBlock,
                                            FileAttributes,
                                            sizeof(*FileAttributes),
                                            FileBasicInformation
                                            );
                if (!(NT_SUCCESS(tmpStatus)))
                    {
                    // xxxop - this should be passed back, so the user can be warned...
                    DEBUGOUTMAINDRV(DEBUGLEV_WARN, ("Unable to reset file timestamps/attributes (%d %d).\n", status, IoStatusBlock->Status));
                    }
                }


            // Close the file
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Closing the file...\n"));
            status = ZwClose(*FileHandle);
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Unable to close file\n"));
                }
            else
                {
                // Just in case...
                *FileHandle = NULL;
                }

            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("FileClose\n"));

    return status;
}


// =========================================================================
NTSTATUS
TestCode()
{
    NTSTATUS status;
    MODULE_DETAILS_HASH Hash;
    unsigned int i, j;
    unsigned int inLen;
    unsigned int outLen;
    FREEOTFEBYTE* inBuf;
    FREEOTFEBYTE* outBuf;
    GUID hashGUID;
    LARGE_INTEGER startTime;
    LARGE_INTEGER stopTime;
    LARGE_INTEGER diffTime;    
    unsigned int interations;



    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, ("TestCode\n"));

    hashGUID = TEST_GUID_0x0F;

    inBuf = FREEOTFE_MEMALLOC(1024);
    outBuf = FREEOTFE_MEMALLOC(1024);
    RtlZeroMemory(
                inBuf,
                1024
                );
    RtlZeroMemory(
                outBuf,
                1024
                );

    KeQuerySystemTime(&startTime);

    status = GetDeviceDetailsHash(
                        "\\Device\\FreeOTFE\\Hash\\{00000000-0000-0000-0000-00000000000E}",
                        &hashGUID,

                        &Hash
                        );

    if (NT_SUCCESS(status))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("Got hash details...\n"));

        inBuf[ 0] = 'p';
        inBuf[ 1] = 'a';
        inBuf[ 2] = 's';
        inBuf[ 3] = 's';
        inBuf[ 4] = 'w';
        inBuf[ 5] = 'o';
        inBuf[ 6] = 'r';
        inBuf[ 7] = 'd';
        inBuf[ 8] = 'p';
        inBuf[ 9] = 'a';
        inBuf[10] = 's';
        inBuf[11] = 's';
        inBuf[12] = 'w';
        inBuf[13] = 'o';
        inBuf[14] = 'r';
        inBuf[15] = 'd';

        inLen = (16 * 8);
        outLen = (1024 * 8);

        interations = 2;
        // interations = 2048 * 2;

        for (i = 1; i<=interations; i++)
            {
            /*
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Input length: %d bits\n", inLen));
            for(j = 0; j < (inLen / 8); j++)
                { 
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Input [%.2d]: %.2x\n", j, inBuf[j]));
                }      
            */

            status = Hash.FnHash(
			    			        &hashGUID,
				    				inLen,
					    			inBuf,
						    		&outLen,
							    	outBuf
							       );

            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, ("Call to hash driver failed (i=%d)\n", i));
                break;
                }

            for (j = 0; j < (outLen / 8); j++)
                {
                inBuf[j] = outBuf[j];
                inLen = outLen;
                }

            }

        KeQuerySystemTime(&stopTime);
        diffTime.QuadPart = stopTime.QuadPart - startTime.QuadPart;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Start time: %lld\n", startTime.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Stop time : %lld\n", stopTime.QuadPart));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("-----  Diff time : %lld\n", diffTime.QuadPart));
        if (NT_SUCCESS(status))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Key length: %d bits\n", outLen));
            for(j = 0; j < (outLen / 8); j++)
                { 
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, ("----- Key [%.2d]: %.2x\n", j, outBuf[j]));
                }                                
            }

        }


    FREEOTFE_FREE(outBuf);
    FREEOTFE_FREE(inBuf);


    // Tear down any hash related...
    // ...hash GUID
    SecZeroMemory(
                  &hashGUID,
                  sizeof(hashGUID)
                 );
    FreeDeviceDetailsHash(&Hash);

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, ("TestCode\n"));

    return status;
}


// =========================================================================
// =========================================================================

