// Description: FreeOTFE Cypher Device Driver
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <ntddk.h>
#include <stdio.h>

// Required for std disk device driver I/O control (IOCTL) codes
#include <ntdddisk.h>


#include "FreeOTFECypherDriver.h"
#include "FreeOTFECypherAPI.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEDriverlib.h"
#include "FreeOTFElib.h"
#include "FreeOTFECypherImpl.h"


// Globals.
// Not nice, but...
HANDLE DirFreeOTFERoot   = NULL;
HANDLE DirFreeOTFECypher = NULL;


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
    int i;
    
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

        RtlZeroMemory( &paramTable[0], sizeof(paramTable) );
        RtlZeroMemory( path, pathLength);
        RtlMoveMemory( path, RegistryPath->Buffer, RegistryPath->Length );

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

        if (!NT_SUCCESS(RtlQueryRegistryValues(
                            RTL_REGISTRY_ABSOLUTE | RTL_REGISTRY_OPTIONAL,
                            path,
                            &paramTable[0],
                            NULL,
                            NULL))) {

            shouldBreak = default_ShouldBreak;
            debugLevel = default_DebugLevel;
        }

        FREEOTFE_FREE(path);
    }

#if DBG
    FreeOTFEDebugLevel = debugLevel;
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Debug level: %d\n", FreeOTFEDebugLevel));
#endif
    
    if (shouldBreak == 1)
        {
        DbgBreakPoint();
        }

#endif

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("DriverEntry\n"));

    // Create main device dir
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Creating dir object (main)...\n"));

    status = CreateDeviceDir(
                DEVICE_FREEOTFE_ROOT,
                &DirFreeOTFERoot
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (main)\n"));
        return status;
        }

    // Create cypher device dir
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Creating dir object (cypher)...\n"));
    status = CreateDeviceDir(
                DEVICE_CYPHER_DIR_NAME,
                &DirFreeOTFECypher
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (cypher)\n"));
        ZwClose(DirFreeOTFERoot);
        return status;
        }

    // Create main device
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Creating main device...\n"));
    status = CreateDevice(DriverObject, &mainDevObj);
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Call to CreateDevice FAILED.\n"));
        return status;
        }
    
    // Store the device dir handle for closure on unload
    mainDevExt = (PDEVICE_EXTENSION)mainDevObj->DeviceExtension;
    

    // Initialize the driver object with this driver's entry points.
    DriverObject->MajorFunction[IRP_MJ_CREATE] = 
                                            FreeOTFE_MF_DispatchCreate;
                                            
    DriverObject->MajorFunction[IRP_MJ_CLOSE] =
                                            FreeOTFE_MF_DispatchClose;
                                            
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] =
                                            FreeOTFE_MF_DispatchDeviceControl;
                                            

    DriverObject->DriverUnload = DriverUnload;


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("DriverEntry\n"));    

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
    
    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("DriverUnload\n"));
    
    
    // Walk through all the device objects, shutting them down and deleting
    // them
    devObj = DriverObject->DeviceObject;
    while (devObj != NULL)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Found a device to destroy; destroying it...\n"));
        devObj = DestroyDevice(devObj);          
        }

    status = ZwClose(DirFreeOTFERoot);
    status = ZwClose(DirFreeOTFECypher);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("DriverUnload\n"));

    return;
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

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("DestroyDevice\n"));
        
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

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Wait completed; thread terminated.\n"));
        
        // Release the thread object ref
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Dereferencing ThreadObject...\n"));
        ObDereferenceObject(devExt->ThreadObject);
        devExt->ThreadObject = NULL;
        }
    

    // Tear down any symbolic device name link
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Tearing down main device specific...\n"));
    if (devExt->zzSymbolicLinkName.Buffer != NULL)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Tearing down symbolic device link...\n"));
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to delete symlink: %ls\n", devExt->zzSymbolicLinkName.Buffer));
        status = IoDeleteSymbolicLink(&devExt->zzSymbolicLinkName);
        if (!NT_SUCCESS(status))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
            }
        
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Freeing off symlink unicode buffer...\n")); 
        SecZeroMemory(
                        devExt->zzSymbolicLinkName.Buffer,
                        sizeof(devExt->zzSymbolicLinkName.MaximumLength)
                        );
        FREEOTFE_FREE(devExt->zzSymbolicLinkName.Buffer);
        devExt->zzSymbolicLinkName.Buffer = NULL;
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }
        
    
    // Tear down any device name
    if (devExt->zzDeviceName.Buffer != NULL)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Freeing off devname unicode buffer...\n"));
        SecZeroMemory(
                      devExt->zzDeviceName.Buffer,
                      sizeof(devExt->zzDeviceName.MaximumLength)
                     );
        FREEOTFE_FREE(devExt->zzDeviceName.Buffer);
        devExt->zzDeviceName.Buffer = NULL;
        }
    

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));   

    // Cleardown details of cyphers supported
    ImpCypherDriverExtDetailsCleardown_v3(&(devExt->DriverInfo_v3));
    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));   

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Overwriting device extension memory...\n"));   
    SecZeroMemory(devExt, sizeof(DEVICE_EXTENSION));
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));   

    // Get rid of the device object...
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Deleting device...\n"));   
    IoDeleteDevice(devObj);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));   
    

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("DestroyDevice\n"));
    
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
    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchCreate\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchCreate\n"));
    
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
    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchClose\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchClose\n"));
    
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
    
    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    
    devExt = DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);

    status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;


    switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
        {
        case IOCTL_FreeOTFECypher_IDENTIFYDRIVER:
            {
            status = IOCTL_FreeOTFECypherIOCTL_IdentifyDriver(DeviceObject, Irp);
            break;
            }


        case IOCTL_FreeOTFECypher_IDENTIFYSUPPORTED_v1:
            {
            status = IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v1(DeviceObject, Irp);
            break;
            }

        case IOCTL_FreeOTFECypher_IDENTIFYSUPPORTED_v3:
            {
            status = IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v3(DeviceObject, Irp);
            break;
            }

        case IOCTL_FreeOTFECypher_ENCRYPT:
        case IOCTL_FreeOTFECypher_DECRYPT:
        case IOCTL_FreeOTFECypher_ENCRYPTSECTOR:
        case IOCTL_FreeOTFECypher_DECRYPTSECTOR:
            {
            queueDeviceObject = DeviceObject;
            break;
            }


        case IOCTL_FreeOTFECypher_INTLDETAILS_v1:
            {
            status = IOCTL_FreeOTFECypherIOCTL_IntlDetails_v1(DeviceObject, Irp);
            break;
            }

        case IOCTL_FreeOTFECypher_INTLDETAILS_v3:
            {
            status = IOCTL_FreeOTFECypherIOCTL_IntlDetails_v3(DeviceObject, Irp);
            break;
            }


        default:
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_WARN, ("IoControl code is UNRECOGNISED (%x).\n",
                        irpSp->Parameters.DeviceIoControl.IoControlCode));
            status = STATUS_NOT_IMPLEMENTED;
            }
        }
        
        
    // If we want to queue the IRP, this should have been flagged,
    // otherwise we complete the reqest
    if (queueDeviceObject != NULL)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Queuing IRP...\n"));
        status = QueueIRPToThread(queueDeviceObject, Irp);    

        // Check if the IRP was queued successfully...
        if (status == STATUS_PENDING)
            {
            // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Queued IRP.\n"));
            }
        else
            {
            Irp->IoStatus.Status = status;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Failed to queue IRP.\n"));
            }

        }
    else
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Completing IRP...\n"));
        // If there was a failure, return nothing; this will be set as appropriate if there
        // is no problem
        if (!(NT_SUCCESS(status)))
            {
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK.\n"));
        }

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    return status;
}



// =========================================================================
// IOCTL to obtain function pointers to encrypt, decrypt functions
// Technique as described in:            
// The NT Insider, Vol 7, Issue 1, Jan-Feb 2000
// Beyond IRPs: Driver to Driver Communications 
// http://www.osronline.com/article.cfm?id=177
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IntlDetails_v1(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_INTL_DETAILS_v1 DIOCBuffer;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_IntlDetails_v1\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_INTL_DETAILS_v1))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_INTL_DETAILS_v1),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDIOC_CYPHER_INTL_DETAILS_v1)Irp->AssociatedIrp.SystemBuffer;

    // Populate output buffer with with function pointers;
    DIOCBuffer->FnCypherDetails_v1 = GetCypherDetails_v1;
    DIOCBuffer->FnEncrypt          = ImpCypherEncryptData;
    DIOCBuffer->FnDecrypt          = ImpCypherDecryptData;
    

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_INTL_DETAILS_v1);
    status = STATUS_SUCCESS;


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_IntlDetails_v1\n"));
    return status;
}


// =========================================================================
// IOCTL to obtain function pointers to encrypt, decrypt functions
// Technique as described in:            
// The NT Insider, Vol 7, Issue 1, Jan-Feb 2000
// Beyond IRPs: Driver to Driver Communications 
// http://www.osronline.com/article.cfm?id=177
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IntlDetails_v3(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_INTL_DETAILS_v3 DIOCBuffer;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_IntlDetails_v3\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_INTL_DETAILS_v3))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_INTL_DETAILS_v3),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDIOC_CYPHER_INTL_DETAILS_v3)Irp->AssociatedIrp.SystemBuffer;

    // Populate output buffer with with function pointers;
    DIOCBuffer->FnCypherDetails_v1 = GetCypherDetails_v1;
    DIOCBuffer->FnCypherDetails_v3 = GetCypherDetails_v3;
    DIOCBuffer->FnEncrypt          = ImpCypherEncryptData;
    DIOCBuffer->FnDecrypt          = ImpCypherDecryptData;
    DIOCBuffer->FnEncryptSector    = ImpCypherEncryptSectorData;
    DIOCBuffer->FnDecryptSector    = ImpCypherDecryptSectorData;
    

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_INTL_DETAILS_v3);
    status = STATUS_SUCCESS;


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_IntlDetails_v3\n"));
    return status;
}


// =========================================================================
// IOCTL to get cypher driver identification
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifyDriver(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_IDENTIFYDRIVER DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_IdentifyDriver\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_IDENTIFYDRIVER))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_IDENTIFYDRIVER),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    DIOCBuffer = (PDIOC_CYPHER_IDENTIFYDRIVER)Irp->AssociatedIrp.SystemBuffer;

    // Populate output buffer
    DIOCBuffer->DriverGUID = devExt->DriverInfo_v3.DriverGUID;
    RtlZeroMemory(DIOCBuffer->Title, sizeof(DIOCBuffer->Title));
    RtlCopyMemory(
                  DIOCBuffer->Title,
                  devExt->DriverInfo_v3.DriverTitle,
                  strlen(devExt->DriverInfo_v3.DriverTitle)
                 );
    DIOCBuffer->VersionID = devExt->DriverInfo_v3.DriverVersionID;
    DIOCBuffer->CountCyphers = devExt->DriverInfo_v3.CypherCount;


    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_IDENTIFYDRIVER);


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_IdentifyDriver\n"));
    return status;
}


// =========================================================================
// IOCTL to get cyphers supported
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v1(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_IDENTIFYSUPPORTED_v1 DIOCBuffer;
    PDEVICE_EXTENSION devExt;
    unsigned int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_IdentifySupported\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBuffer = (PDIOC_CYPHER_IDENTIFYSUPPORTED_v1)Irp->AssociatedIrp.SystemBuffer;

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v1)-sizeof(DIOCBuffer->Cyphers))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v1)-sizeof(DIOCBuffer->Cyphers),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Request valid, process...

    // Setup the input parameter passed to ImpCypherIdentifySupported, so that it's aware
    // of how large the array of cypher details is
    DIOCBuffer->BufCount =
       (
         // The size of the buffer, less the array of cypher details
         irpSp->Parameters.DeviceIoControl.OutputBufferLength - 
           (sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v1)-sizeof(DIOCBuffer->Cyphers))
       ) /
       // Divide by the size of each cypher details struct to give the array length
       sizeof(DIOCBuffer->Cyphers);
       
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Request is sufficiently large to store %d cypher details\n", DIOCBuffer->BufCount));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    if (DIOCBuffer->BufCount < devExt->DriverInfo_v3.CypherCount)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer not large enough to store enough (can store: %d, requires: )\n",
            DIOCBuffer->BufCount,
            devExt->DriverInfo.CypherCount
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;
        }

    // Request valid, process...


    DIOCBuffer->BufCount = devExt->DriverInfo_v3.CypherCount;

    for (i = 0; i < devExt->DriverInfo_v3.CypherCount; i++)
        {
        CopyCYPHER_v3ToCYPHER_v1(
                                 &(devExt->DriverInfo_v3.CypherDetails[i]),
                                 &(DIOCBuffer->Cyphers[i])
                                );
        }

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v1)+(DIOCBuffer->BufCount * sizeof(DIOCBuffer->Cyphers))-sizeof(DIOCBuffer->Cyphers);


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_IdentifySupported\n"));
    return status;
}

// =========================================================================
// IOCTL to get cyphers supported
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v3(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_IDENTIFYSUPPORTED_v3 DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v3\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBuffer = (PDIOC_CYPHER_IDENTIFYSUPPORTED_v3)Irp->AssociatedIrp.SystemBuffer;

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v3)-sizeof(DIOCBuffer->Cyphers))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v3)-sizeof(DIOCBuffer->Cyphers),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Request valid, process...

    // Setup the input parameter passed to ImpCypherIdentifySupported, so that it's aware
    // of how large the array of cypher details is
    DIOCBuffer->BufCount =
       (
         // The size of the buffer, less the array of cypher details
         irpSp->Parameters.DeviceIoControl.OutputBufferLength - 
           (sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v3)-sizeof(DIOCBuffer->Cyphers))
       ) /
       // Divide by the size of each cypher details struct to give the array length
       sizeof(DIOCBuffer->Cyphers);
       
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Request is sufficiently large to store %d cypher details\n", DIOCBuffer->BufCount));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    if (DIOCBuffer->BufCount < devExt->DriverInfo_v3.CypherCount)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer not large enough to store enough (can store: %d, requires: )\n",
            DIOCBuffer->BufCount,
            devExt->DriverInfo_v3.CypherCount
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;
        }

    // Request valid, process...


    DIOCBuffer->BufCount = devExt->DriverInfo_v3.CypherCount;
    RtlCopyMemory(
                  &DIOCBuffer->Cyphers[0],
                  devExt->DriverInfo_v3.CypherDetails,
                  (sizeof(CYPHER_v3) * devExt->DriverInfo_v3.CypherCount)
                 );
    

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_IDENTIFYSUPPORTED_v3)+(DIOCBuffer->BufCount * sizeof(DIOCBuffer->Cyphers))-sizeof(DIOCBuffer->Cyphers);


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v3\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_Encrypt(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_DATA_IN DIOCBufferIn;
    PDIOC_CYPHER_DATA_OUT DIOCBufferOut;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_Encrypt\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_CYPHER_DATA_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_CYPHER_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access DIOCBufferIn->Data
    // for actual check)
    if (
        irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_CYPHER_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
            )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
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
             sizeof(DIOC_CYPHER_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->IVLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->DataLength)  // Already counted in bytes
             )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->IVLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->DataLength)  // Already counted in bytes
             ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Actual check...
    // NOTE THAT THIS USES THE DataLength MEMBER OF THE INPUT BUFFER - THE
    // OUTPUT IS THE SAME SIZE AS THE INPUT
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_DATA_OUT)+(DIOCBufferIn->DataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_DATA_OUT)+DIOCBufferIn->DataLength-sizeof(DIOCBufferOut->Data),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&DIOCBufferIn->Key;
    ptrInIV   = ptrInKey + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key));
    ptrInData = ptrInIV + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key length: %d bits\n", DIOCBufferIn->KeyLength));
    for(i = 0; i < (DIOCBufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key [%.2d]: %.2x\n", i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV length: %d bits\n", DIOCBufferIn->IVLength));
    for(i = 0; i < (DIOCBufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV [%.2d]: %.2x\n", i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data length: %d bytes\n", DIOCBufferIn->DataLength));
    for(i = 0; i < (DIOCBufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [%.2d]: %.2x\n", i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [..]: (Debug output truncated to only show first few bytes)\n"));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to get cypher details...\n"));
    status = GetCypherDetails_v3(
                              &(DIOCBufferIn->CypherGUID),
                              &CypherDetails
                             );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to locate requested cypher in driver\n"));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Cypher details got OK.\n"));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((DIOCBufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n",
                                        CypherDetails.BlockSize,
                                        (DIOCBufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != DIOCBufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n", CypherDetails.BlockSize, DIOCBufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != DIOCBufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n", CypherDetails.KeySize, DIOCBufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Sanity checks OK\n"));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((DIOCBufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("keyASCIIBufferLen = %d bytes\n", keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                          DIOCBufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = DIOCBufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to encrypt...\n"));
        status = ImpCypherEncryptData(
                                    &DIOCBufferIn->CypherGUID,
                                    DIOCBufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    DIOCBufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    DIOCBufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done encryption: %d (want status: %d)\n", status, STATUS_SUCCESS));

        RtlCopyMemory(&DIOCBufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_DATA_OUT)+(dataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data); 

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_Encrypt\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_Decrypt(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_DATA_IN DIOCBufferIn;
    PDIOC_CYPHER_DATA_OUT DIOCBufferOut;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_Decrypt\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_CYPHER_DATA_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_CYPHER_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access DIOCBufferIn->Data
    // for actual check)
    if (
        irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_CYPHER_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
            )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
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
             sizeof(DIOC_CYPHER_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
             + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key))
             + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV))
             + (DIOCBufferIn->DataLength * sizeof(DIOCBufferIn->Data))
             )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
             + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key))
             + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV))
             + (DIOCBufferIn->DataLength * sizeof(DIOCBufferIn->Data))
             ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Actual check...
    // NOTE THAT THIS USES THE DataLength MEMBER OF THE INPUT BUFFER - THE
    // OUTPUT IS THE SAME SIZE AS THE INPUT
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_DATA_OUT)+(DIOCBufferIn->DataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_DATA_OUT)+DIOCBufferIn->DataLength-sizeof(DIOCBufferOut->Data),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&DIOCBufferIn->Key;
    ptrInIV   = ptrInKey + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key));
    ptrInData = ptrInIV + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key length: %d bits\n", DIOCBufferIn->KeyLength));
    for(i = 0; i < (DIOCBufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key [%.2d]: %.2x\n", i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV length: %d bits\n", DIOCBufferIn->IVLength));
    for(i = 0; i < (DIOCBufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV [%.2d]: %.2x\n", i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data length: %d bytes\n", DIOCBufferIn->DataLength));
    for(i = 0; i < (DIOCBufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [%.2d]: %.2x\n", i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [..]: (Debug output truncated to only show first few bytes)\n"));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to get cypher details...\n"));
    status = GetCypherDetails_v3(
                              &(DIOCBufferIn->CypherGUID),
                              &CypherDetails
                             );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to locate requested cypher in driver\n"));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Cypher details got OK.\n"));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((DIOCBufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n",
                                        CypherDetails.BlockSize,
                                        (DIOCBufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != DIOCBufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n", CypherDetails.BlockSize, DIOCBufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != DIOCBufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n", CypherDetails.KeySize, DIOCBufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Sanity checks OK\n"));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((DIOCBufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("keyASCIIBufferLen = %d bytes\n", keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                          DIOCBufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = DIOCBufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to decrypt...\n"));
        status = ImpCypherDecryptData(
                                    &DIOCBufferIn->CypherGUID,
                                    DIOCBufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    DIOCBufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    DIOCBufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done decryption: %d (want status: %d)\n", status, STATUS_SUCCESS));

        RtlCopyMemory(&DIOCBufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_DATA_OUT)+(dataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data); 

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_Decrypt\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_EncryptSector(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_SECTOR_DATA_IN DIOCBufferIn;
    PDIOC_CYPHER_DATA_OUT DIOCBufferOut;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_EncryptSector\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_CYPHER_SECTOR_DATA_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_CYPHER_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access DIOCBufferIn->Data
    // for actual check)
    if (
        irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
            )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
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
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->IVLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->DataLength)  // Already counted in bytes
             )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->IVLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->DataLength)  // Already counted in bytes
             ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Actual check...
    // NOTE THAT THIS USES THE DataLength MEMBER OF THE INPUT BUFFER - THE
    // OUTPUT IS THE SAME SIZE AS THE INPUT
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_DATA_OUT)+(DIOCBufferIn->DataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_DATA_OUT)+DIOCBufferIn->DataLength-sizeof(DIOCBufferOut->Data),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&DIOCBufferIn->Key;
    ptrInIV   = ptrInKey + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key));
    ptrInData = ptrInIV + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key length: %d bits\n", DIOCBufferIn->KeyLength));
    for(i = 0; i < (DIOCBufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key [%.2d]: %.2x\n", i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV length: %d bits\n", DIOCBufferIn->IVLength));
    for(i = 0; i < (DIOCBufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV [%.2d]: %.2x\n", i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data length: %d bytes\n", DIOCBufferIn->DataLength));
    for(i = 0; i < (DIOCBufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [%.2d]: %.2x\n", i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [..]: (Debug output truncated to only show first few bytes)\n"));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to get cypher details...\n"));
    status = GetCypherDetails_v3(
                              &(DIOCBufferIn->CypherGUID),
                              &CypherDetails
                             );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to locate requested cypher in driver\n"));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Cypher details got OK.\n"));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((DIOCBufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n",
                                        CypherDetails.BlockSize,
                                        (DIOCBufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != DIOCBufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n", CypherDetails.BlockSize, DIOCBufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != DIOCBufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n", CypherDetails.KeySize, DIOCBufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Sanity checks OK\n"));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((DIOCBufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("keyASCIIBufferLen = %d bytes\n", keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                          DIOCBufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = DIOCBufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to encrypt...\n"));
        status = ImpCypherEncryptSectorData(
                                    &DIOCBufferIn->CypherGUID,
                                    DIOCBufferIn->SectorID,
                                    DIOCBufferIn->SectorSize,
                                    DIOCBufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    DIOCBufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    DIOCBufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done encryption: %d (want status: %d)\n", status, STATUS_SUCCESS));

        RtlCopyMemory(&DIOCBufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_DATA_OUT)+(dataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data); 

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_EncryptSector\n"));
    return status;
}


// =========================================================================
// 
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_DecryptSector(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_CYPHER_SECTOR_DATA_IN DIOCBufferIn;
    PDIOC_CYPHER_DATA_OUT DIOCBufferOut;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFECypherIOCTL_DecryptSector\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_CYPHER_SECTOR_DATA_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_CYPHER_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access DIOCBufferIn->Data
    // for actual check)
    if (
        irpSp->Parameters.DeviceIoControl.InputBufferLength <
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
            )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
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
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
             + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key))
             + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV))
             + (DIOCBufferIn->DataLength * sizeof(DIOCBufferIn->Data))
             )
       )
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            (
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
             - sizeof(DIOCBufferIn->Key)
             - sizeof(DIOCBufferIn->IV)
             - sizeof(DIOCBufferIn->Data)
             + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key))
             + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV))
             + (DIOCBufferIn->DataLength * sizeof(DIOCBufferIn->Data))
             ),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Actual check...
    // NOTE THAT THIS USES THE DataLength MEMBER OF THE INPUT BUFFER - THE
    // OUTPUT IS THE SAME SIZE AS THE INPUT
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_CYPHER_DATA_OUT)+(DIOCBufferIn->DataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_CYPHER_DATA_OUT)+DIOCBufferIn->DataLength-sizeof(DIOCBufferOut->Data),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

    // Request valid, process...

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&DIOCBufferIn->Key;
    ptrInIV   = ptrInKey + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key));
    ptrInData = ptrInIV + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key length: %d bits\n", DIOCBufferIn->KeyLength));
    for(i = 0; i < (DIOCBufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Key [%.2d]: %.2x\n", i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV length: %d bits\n", DIOCBufferIn->IVLength));
    for(i = 0; i < (DIOCBufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- IV [%.2d]: %.2x\n", i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data length: %d bytes\n", DIOCBufferIn->DataLength));
    for(i = 0; i < (DIOCBufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [%.2d]: %.2x\n", i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, ("----- Data [..]: (Debug output truncated to only show first few bytes)\n"));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to get cypher details...\n"));
    status = GetCypherDetails_v3(
                              &(DIOCBufferIn->CypherGUID),
                              &CypherDetails
                             );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Unable to locate requested cypher in driver\n"));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Cypher details got OK.\n"));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((DIOCBufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n",
                                        CypherDetails.BlockSize,
                                        (DIOCBufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != DIOCBufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n", CypherDetails.BlockSize, DIOCBufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != DIOCBufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n", CypherDetails.KeySize, DIOCBufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Sanity checks OK\n"));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((DIOCBufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("keyASCIIBufferLen = %d bytes\n", keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                          DIOCBufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = DIOCBufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to decrypt...\n"));
        status = ImpCypherDecryptSectorData(
                                    &DIOCBufferIn->CypherGUID,
                                    DIOCBufferIn->SectorID,
                                    DIOCBufferIn->SectorSize,
                                    DIOCBufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    DIOCBufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    DIOCBufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done decryption: %d (want status: %d)\n", status, STATUS_SUCCESS));

        RtlCopyMemory(&DIOCBufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    Irp->IoStatus.Information = sizeof(DIOC_CYPHER_DATA_OUT)+(dataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data); 

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFECypherIOCTL_DecryptSector\n"));
    return status;
}


// =========================================================================
// Create the main device object; the one which user code will normally
// talk to when creating new devices, carrying out general driver
// queries, etc
// DeviceObject will be set to the newly created device object
NTSTATUS
CreateDevice (
    IN PDRIVER_OBJECT DriverObject,
    OUT PDEVICE_OBJECT *DeviceObject
    )
{
    NTSTATUS status;
    UNICODE_STRING devName;
    PDEVICE_EXTENSION devExtension;
    HANDLE threadHandle;
    PDEVICE_OBJECT devObj;
    ANSI_STRING tmpANSIName;
    GUID driverGUID;
    CYPHER_DRIVER_INFO_v3 tmpDevExt;
    

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("CreateDevice\n"));

    ASSERT(DriverObject != NULL);

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Building device name...\n"));
    // We subtrace a sizeof(WCHAR) to discount the #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    
    // This is a bit of a hack to get the driver's GUID
    ImpCypherDriverExtDetailsInit_v3(&tmpDevExt);
    driverGUID = tmpDevExt.DriverGUID;
    ImpCypherDriverExtDetailsCleardown_v3(&tmpDevExt);

    devName.MaximumLength = sizeof(DEVICE_CYPHER_DIR_NAME)
                            // ^^ dir name
                            + sizeof(WCHAR)
                            // ^^ slash
                            + GUID_STRING_REP_UNICODE_BYTE_LENGTH
                            // ^^ GUID length
                            + sizeof(WCHAR);
                            // ^^ terminating NULL

    devName.Buffer = FREEOTFE_MEMALLOC(devName.MaximumLength);    
    RtlZeroMemory(devName.Buffer, devName.MaximumLength);
    
    // Note the "/" in the format string
    devName.Length = (USHORT)swprintf(devName.Buffer, L"%s\\", DEVICE_CYPHER_DIR_NAME);
    // swprintf returns the number of WCHARs, not the length in bytes
    devName.Length = devName.Length * sizeof(WCHAR);
    
    status = AppendGUIDToUnicodeString(driverGUID, &devName);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("AppendGUIDToUnicodeString NOT OK\n"));
        return status;
        }

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("xdevNameLength: %d\n",
                                   devName.Length));
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("xdevNameMaximumLength: %d\n",
                                   devName.MaximumLength));

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to create device: %ls\n", devName.Buffer));
    // Hmmm... I'd prefer to use FILE_DEVICE_UNKNOWN, but since
    // IOCTL control codes pass through the device type, and we just
    // pass then through... 
    // (See other call to IoCreateDevice in this file for details why we don't
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
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        return status;
        }


    devExtension = (PDEVICE_EXTENSION)devObj->DeviceExtension;
    devExtension->zzDeviceName = devName;
    devExtension->ThreadObject = NULL;
    devExtension->zzSymbolicLinkName.Buffer = NULL;

    status = ImpCypherDriverExtDetailsInit_v3(&(devExtension->DriverInfo_v3));
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Failed to ImpCypherDriverExtDetailsInit_v3.\n"));
        DestroyDevice(devObj);
        return status;
        }

    // Create symlink; his allows user applications to CreateFile with
    // "SymbolicName"
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Building symlink name...\n"));    
    // We add on sizeof(WCHAR) to include the terminating #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    devExtension->zzSymbolicLinkName.MaximumLength = sizeof(DEVICE_CYPHER_SYMLINK_PREFIX)
                                                     // ^^ dir name
                                                     + sizeof(WCHAR)
                                                     // ^^ slash
                                                     + GUID_STRING_REP_UNICODE_BYTE_LENGTH
                                                     // ^^ GUID length
                                                     + sizeof(WCHAR);
                                                     // ^^ terminating NULL

    devExtension->zzSymbolicLinkName.Buffer = FREEOTFE_MEMALLOC(devExtension->zzSymbolicLinkName.MaximumLength);
    RtlZeroMemory(devExtension->zzSymbolicLinkName.Buffer, devExtension->zzSymbolicLinkName.MaximumLength);
    devExtension->zzSymbolicLinkName.Length = (USHORT)swprintf(
	                            devExtension->zzSymbolicLinkName.Buffer,
	                            L"%s",
	                            DEVICE_CYPHER_SYMLINK_PREFIX
                                   );
    // swprintf returns the number of WCHARs, not the length in bytes
    devExtension->zzSymbolicLinkName.Length = devExtension->zzSymbolicLinkName.Length * sizeof(WCHAR);

    status = AppendGUIDToUnicodeString(driverGUID, &devExtension->zzSymbolicLinkName);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("AppendGUIDToUnicodeString NOT OK\n"));
        DestroyDevice(devObj);
        return status;
        }

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Unicoded symlink name: %ls\n",
                             devExtension->zzSymbolicLinkName.Buffer));
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Length: %d\n",
                             devExtension->zzSymbolicLinkName.Length));
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("MaximumLength: %d\n",
                             devExtension->zzSymbolicLinkName.MaximumLength));
    

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("About to create dosdevice symlink: %ls -> %ls\n", devExtension->zzSymbolicLinkName.Buffer, devExtension->zzDeviceName.Buffer));
    status = IoCreateSymbolicLink(&devExtension->zzSymbolicLinkName, &devExtension->zzDeviceName);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done, checking status...\n"));
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK\n"));

    devObj->Flags |= DO_DIRECT_IO;

    devObj->Characteristics |= FILE_READ_ONLY_DEVICE;


    // (Some of the bits following are taken from the DDK src/general/cancel example)
    

    // This is used to serailize access to the queue.
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

    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Create thread FAILED.\n"));        
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK\n"));

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
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Done processing thread; checking status.\n"));
    if (!NT_SUCCESS(status))
      {
      DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Status FAILED.\n"));        
      ZwClose(threadHandle);
      DestroyDevice(devObj);
      return status;
      }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("OK\n"));

    // Close the thread handle  
    ZwClose(threadHandle);


    // This is an easy thing to overlook - we do *not* have to unset the
    // device's DO_DEVICE_INITIALIZING "Flags" as this function is carried out
    // undef DriverEntry, and not something like AddDevice
   
    *DeviceObject = devObj;
    
    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("CreateDevice\n"));

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
    DEBUGOUTCYPHERDRV(DEBUGLEV_WARN, ("Cancelled IRP.\n"));    
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
}


// =========================================================================
NTSTATUS
QueueIRPToThread(
    IN  PDEVICE_OBJECT DeviceObject,    
    IN  PIRP           Irp
)
{
    PDEVICE_EXTENSION devExt;
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("QueueIrpToThread\n"));
    
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
    
    status = STATUS_PENDING;                
      
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

            
    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("QueueIrpToThread\n"));
    
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
    
        
    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("FreeOTFEThread\n"));
    
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Setting threads priority...\n"));
    KeSetPriorityThread(KeGetCurrentThread(), LOW_REALTIME_PRIORITY);

    devObj = (PDEVICE_OBJECT)Context;
    devExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Entering endless loop for queued event signals...\n"));
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
        
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Queued event signalled.\n"));
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread is for device: %ls\n", devExt->zzDeviceName.Buffer));
            
        // If we're supposed to terminate the thread, bail out...
        if (devExt->TerminateThread) 
           {
           DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread has been requested to terminate!.\n"));
           break;  // Thread terminates after this loop terminates
           }
           
           
        // Remove a pending IRP from the queue.
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread getting next queued IRP.\n"));
        Irp = IoCsqRemoveNextIrp(&devExt->CancelSafeQueue, NULL);

        if (!Irp)
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Queued IRP was cancelled; next.\n"));
            continue; // go back to waiting
            }


        // Default...
        status = STATUS_NOT_IMPLEMENTED;
        Irp->IoStatus.Information = 0;


        irpSp = IoGetCurrentIrpStackLocation(Irp);

        switch (irpSp->MajorFunction)
            {
            case IRP_MJ_DEVICE_CONTROL:
                {
                DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("THREAD: IRP_MJ_DEVICE_CONTROL\n"));
                switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
                    {
                    case IOCTL_FreeOTFECypher_ENCRYPT:
                        {
                        status = IOCTL_FreeOTFECypherIOCTL_Encrypt(devObj, Irp);
                        break;
                        }

                    case IOCTL_FreeOTFECypher_DECRYPT:
                        {
                        status = IOCTL_FreeOTFECypherIOCTL_Decrypt(devObj, Irp);
                        break;
                        }

                    case IOCTL_FreeOTFECypher_ENCRYPTSECTOR:
                        {
                        status = IOCTL_FreeOTFECypherIOCTL_EncryptSector(devObj, Irp);
                        break;
                        }

                    case IOCTL_FreeOTFECypher_DECRYPTSECTOR:
                        {
                        status = IOCTL_FreeOTFECypherIOCTL_DecryptSector(devObj, Irp);
                        break;
                        }

                    default:
                        {
                        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("THREAD: Unrecognised IRP_MJ_DEVICE_CONTROL IRP.\n"));
                        }
                        
                    }   // switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
                break;
                }   // case IRP_MJ_DEVICE_CONTROL:
                
                default:
                    {
                    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("THREAD: Unrecognised IRP MajorFunction.\n"));
                    }
                        
            }   // switch (irpSp->MajorFunction)

                
        
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread completing IRP (%d).\n", status));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread setting IoStatus.Information = 0\n"));
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);           
        }   // while (TRUE)
        
        
    // This part will only be reached if the thread has been signalled to
    // terminate
    devExt = NULL;
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Terminating thread...\n"));
    status = PsTerminateSystemThread(STATUS_SUCCESS);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Thread did NOT terminate OK\n"));
        }        
        
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Thread dropping out.\n"));           

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("FreeOTFEThread\n"));
}


// =========================================================================
NTSTATUS
GetCypherDetails_v1(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v1* CypherDetails
)
{
    NTSTATUS status = STATUS_NOT_FOUND;
    CYPHER_DRIVER_INFO_v1 tmpDevExt;
    unsigned int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("GetCypherDetails_v1\n"));
    
    // This is a bit of a hack to get the details...
    ImpCypherDriverExtDetailsInit_v1(&tmpDevExt);
    for (i = 0; i<tmpDevExt.CypherCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.CypherDetails[i].CypherGUID, CypherGUID))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Located relevant details\n"));
            RtlCopyMemory(
                          CypherDetails,
                          &tmpDevExt.CypherDetails[i],
                          sizeof(CYPHER_v1)
                         );

            status = STATUS_SUCCESS;
            }
        }

    ImpCypherDriverExtDetailsCleardown_v1(&tmpDevExt);


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("GetCypherDetails_v1\n"));

    return status;
}

// =========================================================================
NTSTATUS
GetCypherDetails_v3(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v3* CypherDetails
)
{
    NTSTATUS status = STATUS_NOT_FOUND;
    CYPHER_DRIVER_INFO_v3 tmpDevExt;
    unsigned int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("GetCypherDetails_v3\n"));
    
    // This is a bit of a hack to get the details...
    ImpCypherDriverExtDetailsInit_v3(&tmpDevExt);
    for (i = 0; i<tmpDevExt.CypherCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.CypherDetails[i].CypherGUID, CypherGUID))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Located relevant details\n"));
            RtlCopyMemory(
                          CypherDetails,
                          &tmpDevExt.CypherDetails[i],
                          sizeof(CYPHER_v3)
                         );

            status = STATUS_SUCCESS;
            }
        }

    ImpCypherDriverExtDetailsCleardown_v3(&tmpDevExt);


    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("GetCypherDetails_v3\n"));

    return status;
}


// =========================================================================
// =========================================================================


