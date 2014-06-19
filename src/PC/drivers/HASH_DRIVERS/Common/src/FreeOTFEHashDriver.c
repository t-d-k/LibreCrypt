// Description: FreeOTFE Hash Device Driver
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


#include "FreeOTFEHashDriver.h"
#include "FreeOTFEHashAPI.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEDriverlib.h"
#include "FreeOTFElib.h"
#include "FreeOTFEHashImpl.h"


// Globals.
// Not nice, but...
HANDLE DirFreeOTFERoot = NULL;
HANDLE DirFreeOTFEHash = NULL;


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
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Debug level: %d\n", FreeOTFEDebugLevel));
#endif
    
    if (shouldBreak == 1)
        {
        DbgBreakPoint();
        }

#endif

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("DriverEntry\n"));

    // Create main device dir
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Creating dir object (main)...\n"));
    status = CreateDeviceDir(
                DEVICE_FREEOTFE_ROOT,
                &DirFreeOTFERoot
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (main)\n"));
        return status;
        }

    // Create hash device dir
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Creating dir object (hash)...\n"));
    status = CreateDeviceDir(
                DEVICE_HASH_DIR_NAME,
                &DirFreeOTFEHash
               );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Unable to CreateDeviceDir (hash)\n"));
        ZwClose(DirFreeOTFERoot);
        return status;
        }

    // Create main device
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Creating main device...\n"));
    status = CreateDevice(DriverObject, &mainDevObj);
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Call to CreateDevice FAILED.\n"));
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


    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("DriverEntry\n"));    

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
    
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("DriverUnload\n"));
    
    
    // Walk through all the device objects, shutting them down and deleting
    // them
    devObj = DriverObject->DeviceObject;
    while (devObj != NULL)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Found a device to destroy; destroying it...\n"));
        devObj = DestroyDevice(devObj);          
        }

    status = ZwClose(DirFreeOTFERoot);
    status = ZwClose(DirFreeOTFEHash);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("DriverUnload\n"));

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

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("DestroyDevice\n"));
        
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

        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Wait completed; thread terminated.\n"));
        
        // Release the thread object ref
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Dereferencing ThreadObject...\n"));
        ObDereferenceObject(devExt->ThreadObject);
        devExt->ThreadObject = NULL;
        }
    

    // Tear down any symbolic device name link
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Tearing down main device specific...\n"));
    if (devExt->zzSymbolicLinkName.Buffer != NULL)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Tearing down symbolic device link...\n"));
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("About to delete symlink: %ls\n", devExt->zzSymbolicLinkName.Buffer));
        status = IoDeleteSymbolicLink(&devExt->zzSymbolicLinkName);
        if (!NT_SUCCESS(status))
            {
            DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
            }
        
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Freeing off symlink unicode buffer...\n")); 
        SecZeroMemory(
                        devExt->zzSymbolicLinkName.Buffer,
                        sizeof(devExt->zzSymbolicLinkName.MaximumLength)
                        );
        FREEOTFE_FREE(devExt->zzSymbolicLinkName.Buffer);
        devExt->zzSymbolicLinkName.Buffer = NULL;
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));   
        }
        
    
    // Tear down any device name
    if (devExt->zzDeviceName.Buffer != NULL)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Freeing off devname unicode buffer...\n"));
        SecZeroMemory(
                      devExt->zzDeviceName.Buffer,
                      sizeof(devExt->zzDeviceName.MaximumLength)
                     );
        FREEOTFE_FREE(devExt->zzDeviceName.Buffer);
        devExt->zzDeviceName.Buffer = NULL;
        }
    

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));   
    
    // Cleardown details of hashes supported
    ImpHashDriverExtDetailsCleardown(&(devExt->DriverInfo));

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));   

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Overwriting device extension memory...\n"));   
    SecZeroMemory(devExt, sizeof(DEVICE_EXTENSION));
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));   

    // Get rid of the device object...
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Deleting device...\n"));   
    IoDeleteDevice(devObj);
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));   
    

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("DestroyDevice\n"));
    
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
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchCreate\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchCreate\n"));
    
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
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchClose\n"));
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = FILE_OPENED;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchClose\n"));
    
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
    
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    
    devExt = DeviceObject->DeviceExtension;
    irpSp = IoGetCurrentIrpStackLocation(Irp);

    status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;


    switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
        {
        case IOCTL_FREEOTFEHASH_IDENTIFYDRIVER:
            {
            status = IOCTL_FreeOTFEHashIOCTL_IdentifyDriver(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFEHASH_IDENTIFYSUPPORTED:
            {
            status = IOCTL_FreeOTFEHashIOCTL_IdentifySupported(DeviceObject, Irp);
            break;
            }


        case IOCTL_FREEOTFEHASH_HASHDATA:
            {
            queueDeviceObject = DeviceObject;
            break;
            }


        case IOCTL_FREEOTFEHASH_INTLDETAILS:
            {
            status = IOCTL_FreeOTFEHashIOCTL_IntlDetails(DeviceObject, Irp);
            break;
            }


        default:
            {
            DEBUGOUTHASHDRV(DEBUGLEV_WARN, ("IoControl code is UNRECOGNISED (%x).\n",
                        irpSp->Parameters.DeviceIoControl.IoControlCode));
            status = STATUS_NOT_IMPLEMENTED;
            }
        }
        
        
    // If we want to queue the IRP, this should have been flagged,
    // otherwise we complete the reqest
    if (queueDeviceObject != NULL)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Queuing IRP...\n"));
        status = QueueIRPToThread(queueDeviceObject, Irp);    

        // Check if the IRP was queued successfully...
        if (status == STATUS_PENDING)
            {
            // Note: IoCsqInsertIrp in QueueIRPToThread marks the IRP pending.
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Queued IRP.\n"));
            }
        else
            {
            Irp->IoStatus.Status = status;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Failed to queue IRP.\n"));
            }

        }
    else
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Completing IRP...\n"));
        // If there was a failure, return nothing; this will be set as appropriate if there
        // is no problem
        if (!(NT_SUCCESS(status)))
            {
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK.\n"));
        }

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("FreeOTFE_MF_DispatchDeviceControl\n"));
    
    return status;
}



// =========================================================================
// IOCTL to obtain function pointers to encrypt, decrypt functions
// Technique as described in:            
// The NT Insider, Vol 7, Issue 1, Jan-Feb 2000
// Beyond IRPs: Driver to Driver Communications 
// http://www.osronline.com/article.cfm?id=177
NTSTATUS
IOCTL_FreeOTFEHashIOCTL_IntlDetails(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_HASH_INTLDETAILS DIOCBuffer;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEHashIOCTL_IntlDetails\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_HASH_INTLDETAILS))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_HASH_INTLDETAILS),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    DIOCBuffer = (PDIOC_HASH_INTLDETAILS)Irp->AssociatedIrp.SystemBuffer;

    // Populate output buffer with with function pointers;
    DIOCBuffer->FnHash        = ImpHashHashData;
    DIOCBuffer->FnHashDetails = GetHashDetails;    

    Irp->IoStatus.Information = sizeof(DIOC_HASH_INTLDETAILS);
    status = STATUS_SUCCESS;


    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEHashIOCTL_IntlDetails\n"));
    return status;
}


// =========================================================================
// IOCTL to get hash driver identification
NTSTATUS
IOCTL_FreeOTFEHashIOCTL_IdentifyDriver(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp;
    PDIOC_HASH_IDENTIFYDRIVER DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEHashIOCTL_IdentifyDriver\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);


    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_HASH_IDENTIFYDRIVER))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect: %d; got: %d)\n",
            sizeof(DIOC_HASH_IDENTIFYDRIVER),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
        
    // Request valid, process...
    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    DIOCBuffer = (PDIOC_HASH_IDENTIFYDRIVER)Irp->AssociatedIrp.SystemBuffer;

    // Populate output buffer
    DIOCBuffer->DriverGUID = devExt->DriverInfo.DriverGUID;
    RtlZeroMemory(DIOCBuffer->Title, sizeof(DIOCBuffer->Title));
    RtlCopyMemory(
                  DIOCBuffer->Title,
                  devExt->DriverInfo.DriverTitle,
                  strlen(devExt->DriverInfo.DriverTitle)
                 );
    DIOCBuffer->VersionID = devExt->DriverInfo.DriverVersionID;
    DIOCBuffer->CountHashes = devExt->DriverInfo.HashCount;


    Irp->IoStatus.Information = sizeof(DIOC_HASH_IDENTIFYDRIVER);


    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEHashIOCTL_IdentifyDriver\n"));

    return status;
}


// =========================================================================
// IOCTL to get hashes supported
NTSTATUS
IOCTL_FreeOTFEHashIOCTL_IdentifySupported(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp;
    PDIOC_HASH_IDENTIFYSUPPORTED DIOCBuffer;
    PDEVICE_EXTENSION devExt;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEHashIOCTL_IdentifySupported\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBuffer = (PDIOC_HASH_IDENTIFYSUPPORTED)Irp->AssociatedIrp.SystemBuffer;

    // Check size of OUTPUT buffer
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_HASH_IDENTIFYSUPPORTED)-sizeof(DIOCBuffer->Hashes))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_HASH_IDENTIFYSUPPORTED)-sizeof(DIOCBuffer->Hashes),
            irpSp->Parameters.DeviceIoControl.OutputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Setup the input parameter passed to ImpHashIdentifySupported, so that it's aware
    // of how large the array of hash details is
    DIOCBuffer->BufCount =
       (
         // The size of the buffer, less the array of hash details
         irpSp->Parameters.DeviceIoControl.OutputBufferLength - 
           (sizeof(DIOC_HASH_IDENTIFYSUPPORTED)-sizeof(DIOCBuffer->Hashes))
       ) /
       // Divide by the size of each hash details struct to give the array length
       sizeof(DIOCBuffer->Hashes);
       
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Request is sufficiently large to store %d hash details\n", DIOCBuffer->BufCount));

    devExt = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;

    if (DIOCBuffer->BufCount < devExt->DriverInfo.HashCount)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer not large enough to store enough (can store: %d, requires: )\n",
            DIOCBuffer->BufCount,
            devExt->DriverInfo.HashCount
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;
        }

    // Request valid, process...


    DIOCBuffer->BufCount = devExt->DriverInfo.HashCount;
    RtlCopyMemory(
                  &DIOCBuffer->Hashes[0],
                  devExt->DriverInfo.HashDetails,
                  (sizeof(HASH) * devExt->DriverInfo.HashCount)
                 );
    

    Irp->IoStatus.Information = sizeof(DIOC_HASH_IDENTIFYSUPPORTED)+(DIOCBuffer->BufCount * sizeof(DIOCBuffer->Hashes))-sizeof(DIOCBuffer->Hashes);


    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEHashIOCTL_IdentifySupported\n"));

    return status;
}


// =========================================================================
// IOCTL to hash data
NTSTATUS
IOCTL_FreeOTFEHashIOCTL_Hash(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
)
{
    NTSTATUS status;
    PIO_STACK_LOCATION irpSp;
    PDIOC_HASH_DATA_IN DIOCBufferIn;
    PDIOC_HASH_DATA_OUT DIOCBufferOut;
    FREEOTFEBYTE* tmpOutput;
    unsigned int tmpLengthBits;  // In *bits*
    int userBufferSizeBytes;  // In *bytes*

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("IOCTL_FreeOTFEHashIOCTL_Hash\n"));

    irpSp = IoGetCurrentIrpStackLocation(Irp);

    DIOCBufferIn = (PDIOC_HASH_DATA_IN)Irp->AssociatedIrp.SystemBuffer;
    DIOCBufferOut = (PDIOC_HASH_DATA_OUT)Irp->AssociatedIrp.SystemBuffer;

    // Check size of INPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access "length" parts
    // for actual check)
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_HASH_DATA_IN)-sizeof(DIOCBufferIn->Data))            
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_HASH_DATA_IN)-sizeof(DIOCBufferIn->Data),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }
    // Actual check...
    if (irpSp->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(DIOC_HASH_DATA_IN)+(DIOCBufferIn->DataLength/8)-sizeof(DIOCBufferIn->Data))            
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("inBuffer size wrong size (expect actual: %d; got: %d)\n",
            sizeof(DIOC_HASH_DATA_IN)+(DIOCBufferIn->DataLength/8)-sizeof(DIOCBufferIn->Data),
            irpSp->Parameters.DeviceIoControl.InputBufferLength
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }


    // Check size of OUTPUT buffer
    // Minimum check...
    // (Done first in order to ensure that we can later access "length" parts
    // for actual check)
    if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(DIOC_HASH_DATA_OUT)-sizeof(DIOCBufferOut->Hash))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect min: %d; got: %d)\n",
            sizeof(DIOC_HASH_DATA_OUT)-sizeof(DIOCBufferOut->Hash),
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
                          sizeof(DIOCBufferOut->Hash);
    tmpOutput = FREEOTFE_MEMALLOC(userBufferSizeBytes); 

    // The size of the buffer in bits; the algorithm will read this value, then
    // overwrite it with the actual size of the output (in bits)
    tmpLengthBits = (userBufferSizeBytes * 8);

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Input buffer length is: %d bits\n", DIOCBufferIn->DataLength));
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Allocated output buffer for: %d bits\n", tmpLengthBits));
    
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("About to process...\n"));
    status = ImpHashHashData(
                             &DIOCBufferIn->HashGUID,
                             DIOCBufferIn->DataLength,  // In bits
                             (FREEOTFEBYTE*)&DIOCBufferIn->Data,
                             &tmpLengthBits,  // In bits
                             (FREEOTFEBYTE*)tmpOutput
                            );

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Done with status: %d (want status: %d)\n", status, STATUS_SUCCESS));
    if (NT_SUCCESS(status)) 
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("tmp buffer was: %d bits, output was %d bits\n", (userBufferSizeBytes * 8), tmpLengthBits));

        // Actual check...

        // Ensure output DIOC butter is large enough to store the output
        // Note that we can't carry out this check until we've created the
        // output value, in case the algorithm used produces variable length
        // output
        if (irpSp->Parameters.DeviceIoControl.OutputBufferLength <
                sizeof(DIOC_HASH_DATA_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->Hash))
            {
            DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("outBuffer size wrong size (expect actual: %d; got: %d)\n",
                sizeof(DIOC_HASH_DATA_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->Hash),
                irpSp->Parameters.DeviceIoControl.OutputBufferLength
                ));
            status = STATUS_INVALID_BUFFER_SIZE;
            }
        else
            {
            RtlCopyMemory(&DIOCBufferOut->Hash, tmpOutput, (tmpLengthBits / 8));
            DIOCBufferOut->HashLength = tmpLengthBits;
            Irp->IoStatus.Information = sizeof(DIOC_HASH_DATA_OUT)+(tmpLengthBits/8)-sizeof(DIOCBufferOut->Hash); 
            }

        }
    else
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("FAILED"));
        }

    SecZeroMemory(tmpOutput, userBufferSizeBytes);
    FREEOTFE_FREE(tmpOutput);


    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("IOCTL_FreeOTFEHashIOCTL_Hash\n"));

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
    HASH_DRIVER_INFO tmpDevExt;
    

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("CreateDevice\n"));

    ASSERT(DriverObject != NULL);

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Building device name...\n"));
    // We subtrace a sizeof(WCHAR) to discount the #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    
    // This is a bit of a hack to get the driver's GUID
    ImpHashDriverExtDetailsInit(&tmpDevExt);
    driverGUID = tmpDevExt.DriverGUID;
    ImpHashDriverExtDetailsCleardown(&tmpDevExt);

    devName.MaximumLength = sizeof(DEVICE_HASH_DIR_NAME)
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
    devName.Length = (USHORT)swprintf(devName.Buffer, L"%s\\", DEVICE_HASH_DIR_NAME);
    // swprintf returns the number of WCHARs, not the length in bytes
    devName.Length = devName.Length * sizeof(WCHAR);
    
    status = AppendGUIDToUnicodeString(driverGUID, &devName);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("AppendGUIDToUnicodeString NOT OK\n"));
        return status;
        }

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("xdevNameLength: %d\n",
                                    devName.Length));
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("xdevNameMaximumLength: %d\n",
                                    devName.MaximumLength));

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("About to create device: %ls\n", devName.Buffer));
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
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        return status;
        }


    devExtension = (PDEVICE_EXTENSION)devObj->DeviceExtension;
    devExtension->zzDeviceName = devName;
    devExtension->ThreadObject = NULL;
    devExtension->zzSymbolicLinkName.Buffer = NULL;

    status = ImpHashDriverExtDetailsInit(&(devExtension->DriverInfo));
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Failed to ImpHashDriverExtDetailsInit.\n"));
        DestroyDevice(devObj);
        return status;
        }

    // Create symlink; his allows user applications to CreateFile with
    // "SymbolicName"
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Building symlink name...\n"));    
    // We add on sizeof(WCHAR) to include the terminating #NULL - required 
    // since although the UNICODE_STRING string doesn't require it, swprintf   
    // adds one on anyway
    devExtension->zzSymbolicLinkName.MaximumLength = sizeof(DEVICE_HASH_SYMLINK_PREFIX)
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
	                            DEVICE_HASH_SYMLINK_PREFIX
                                   );
    // swprintf returns the number of WCHARs, not the length in bytes
    devExtension->zzSymbolicLinkName.Length = devExtension->zzSymbolicLinkName.Length * sizeof(WCHAR);

    status = AppendGUIDToUnicodeString(driverGUID, &devExtension->zzSymbolicLinkName);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("AppendGUIDToUnicodeString NOT OK\n"));
        DestroyDevice(devObj);
        return status;
        }

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Unicoded symlink name: %ls\n",
                             devExtension->zzSymbolicLinkName.Buffer));
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Length: %d\n",
                             devExtension->zzSymbolicLinkName.Length));
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("MaximumLength: %d\n",
                             devExtension->zzSymbolicLinkName.MaximumLength));
    

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("About to create dosdevice symlink: %ls -> %ls\n", devExtension->zzSymbolicLinkName.Buffer, devExtension->zzDeviceName.Buffer));
    status = IoCreateSymbolicLink(&devExtension->zzSymbolicLinkName, &devExtension->zzDeviceName);
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Done, checking status...\n"));
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Status NOT OK\n"));
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK\n"));

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
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Create thread FAILED.\n"));        
        DestroyDevice(devObj);
        return status;
        }
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK\n"));

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
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Done processing thread; checking status.\n"));
    if (!NT_SUCCESS(status))
      {
      DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Status FAILED.\n"));        
      ZwClose(threadHandle);
      DestroyDevice(devObj);
      return status;
      }
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("OK\n"));

    // Close the thread handle  
    ZwClose(threadHandle);


    // This is an easy thing to overlook - we do *not* have to unset the
    // device's DO_DEVICE_INITIALIZING "Flags" as this function is carried out
    // undef DriverEntry, and not something like AddDevice
   
    *DeviceObject = devObj;
    
    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("CreateDevice\n"));

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
    DEBUGOUTHASHDRV(DEBUGLEV_WARN, ("Cancelled IRP.\n"));    
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

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("QueueIrpToThread\n"));
    
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

            
    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("QueueIrpToThread\n"));
    
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
    
        
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("FreeOTFEThread\n"));
    
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Setting threads priority...\n"));
    KeSetPriorityThread(KeGetCurrentThread(), LOW_REALTIME_PRIORITY);

    devObj = (PDEVICE_OBJECT)Context;
    devExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Entering endless loop for queued event signals...\n"));
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
        
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Queued event signalled.\n"));
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread is for device: %ls\n", devExt->zzDeviceName.Buffer));
            
        // If we're supposed to terminate the thread, bail out...
        if (devExt->TerminateThread) 
           {
           DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread has been requested to terminate!.\n"));
           break;  // Thread terminates after this loop terminates
           }
           
           
        // Remove a pending IRP from the queue.
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread getting next queued IRP.\n"));
        Irp = IoCsqRemoveNextIrp(&devExt->CancelSafeQueue, NULL);

        if (!Irp)
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Queued IRP was cancelled; next.\n"));
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
                DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("THREAD: IRP_MJ_DEVICE_CONTROL\n"));
                switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
                    {
                    case IOCTL_FREEOTFEHASH_HASHDATA:
                        {
                        status = IOCTL_FreeOTFEHashIOCTL_Hash(devObj, Irp);
                        break;
                        }
                        
                    default:
                        {
                        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("THREAD: Unrecognised IRP_MJ_DEVICE_CONTROL IRP.\n"));
                        }
                        
                    }   // switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
                break;
                }   // case IRP_MJ_DEVICE_CONTROL:
                
                default:
                    {
                    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("THREAD: Unrecognised IRP MajorFunction.\n"));
                    }
                        
            }   // switch (irpSp->MajorFunction)

                
        
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread completing IRP (%d).\n", status));
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread setting IoStatus.Information = 0\n"));
            Irp->IoStatus.Information = 0;
            }                
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);           
        }   // while (TRUE)
        
        
    // This part will only be reached if the thread has been signalled to
    // terminate
    devExt = NULL;
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Terminating thread...\n"));
    status = PsTerminateSystemThread(STATUS_SUCCESS);
    if (!NT_SUCCESS(status))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, ("Thread did NOT terminate OK\n"));
        }        
        
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Thread dropping out.\n"));           

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("FreeOTFEThread\n"));
}


// =========================================================================
NTSTATUS
GetHashDetails(
    IN     GUID* HashGUID,
    IN OUT HASH* HashDetails
)
{
    NTSTATUS status = STATUS_NOT_FOUND;
    HASH_DRIVER_INFO tmpDevExt;
    unsigned int i;
    
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, ("GetHashDetails\n"));

    // This is a bit of a hack to get the details...
    ImpHashDriverExtDetailsInit(&tmpDevExt);

    for (i = 0; i<tmpDevExt.HashCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.HashDetails[i].HashGUID, HashGUID))
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, ("Located relevant details\n"));

            RtlCopyMemory(
                          HashDetails,
                          &tmpDevExt.HashDetails[i],
                          sizeof(HASH)
                         );

            status = STATUS_SUCCESS;
            }
        }

    ImpHashDriverExtDetailsCleardown(&tmpDevExt);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, ("GetHashDetails\n"));

    return status;
}


// =========================================================================
// =========================================================================


