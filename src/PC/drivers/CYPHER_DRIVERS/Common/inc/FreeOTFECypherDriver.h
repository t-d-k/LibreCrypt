// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFECypherDriver_H
#define _FreeOTFECypherDriver_H   1


// The following includes are required in order to implement Cancel-Safe
// IRP Queues
#include <ntddk.h>
//#include <wdm.h>
#include <csq.h>


#include "FreeOTFECypherAPI.h"


// =========================================================================
// Const definitions

// =========================================================================
// Type definitions

// These values are in DECIMAL
#define DRIVER_CYPHER_VERSION_MAJOR     05
#define DRIVER_CYPHER_VERSION_MINOR     00
#define DRIVER_CYPHER_VERSION_BUILD   0000

#define DRIVER_CYPHER_VERSION  (                                      \
                         (DRIVER_CYPHER_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_CYPHER_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_CYPHER_VERSION_BUILD * 0x00000001)   \
                        )


typedef struct _DEVICE_EXTENSION {

    // Device's name
    UNICODE_STRING zzDeviceName;

    // Symbolic link name
    UNICODE_STRING zzSymbolicLinkName;
    
    // ------------------------------------------------------
    // Driver identification and cyphers supported

    // Driver identification...
    CYPHER_DRIVER_INFO_v3 DriverInfo_v3;

    // ------------------------------------------------------
    // Thread/IRP queue related items follow    
    
    // Flag to signal that the thread should terminate
    BOOLEAN TerminateThread;
    
    // Required for w98; event that the thread flags to indicate it's finished
    // (Credit to Walter Oney)
    KEVENT ThreadTerminated;
    
    // The device's IRP processing thread
    PETHREAD ThreadObject;
    
    // Event flagging that an IRP has been queued
    KEVENT ThreadIRPQueuedEvent;
                     
    // Irps waiting to be processed are queued here
    LIST_ENTRY   PendingIRPQueue;

    //  SpinLock to protect access to the queue
    KSPIN_LOCK IRPQueueLock;
    
    IO_CSQ CancelSafeQueue;   
    KSEMAPHORE IRPQueueSemaphore;


    // ------------------------------------------------------
 
  
} DEVICE_EXTENSION, *PDEVICE_EXTENSION;


// =========================================================================
// Function headers

// Driver entry point
NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
);
    
    
// Driver unload
VOID
DriverUnload(
    IN PDRIVER_OBJECT DriverObject
);
    

// Handle create IRPs
NTSTATUS
FreeOTFE_MF_DispatchCreate(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Handle close IRPs
NTSTATUS
FreeOTFE_MF_DispatchClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Handle device control IRPs
NTSTATUS
FreeOTFE_MF_DispatchDeviceControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
CreateDevice (
    IN PDRIVER_OBJECT   DriverObject,
    OUT PDEVICE_OBJECT  *DeviceObject
);


PDEVICE_OBJECT
DestroyDevice(
    IN PDEVICE_OBJECT devObj    
);

    
VOID
FreeOTFEThread(
    IN PVOID Context
);
    
 
 
VOID CSQInsertIrp (
    IN PIO_CSQ   Csq,
    IN PIRP              Irp
);

VOID CSQRemoveIrp(
    IN  PIO_CSQ Csq,
    IN  PIRP    Irp
);

PIRP CSQPeekNextIrp(
    IN  PIO_CSQ Csq,
    IN  PIRP    Irp,
    IN  PVOID   PeekContext
);

VOID CSQAcquireLock(
    IN  PIO_CSQ Csq,
    OUT PKIRQL  Irql
);

VOID CSQReleaseLock(
    IN PIO_CSQ Csq,
    IN KIRQL   Irql
);

VOID CSQCompleteCanceledIrp(
    IN  PIO_CSQ             pCsq,
    IN  PIRP                Irp
);


// Cancel all IRPs currently on the queue
void
CancelAllQueuedIRPs(
    IN PDEVICE_OBJECT DeviceObject
);


// Queue the specified IRP for the given DeviceObject
NTSTATUS
QueueIRPToThread(
    IN  PDEVICE_OBJECT DeviceObject,    
    IN  PIRP           Irp
);


// IOCTL to get driver identification
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifyDriver(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

// IOCTL to get cyphers supported
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v1(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

// IOCTL to get cyphers supported
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IdentifySupported_v3(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFECypherIOCTL_Encrypt(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFECypherIOCTL_Decrypt(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFECypherIOCTL_EncryptSector(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFECypherIOCTL_DecryptSector(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// IOCTL to obtain function pointers to encrypt, decrypt functions
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IntlDetails_v1(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

// IOCTL to obtain function pointers to encrypt, decrypt functions
NTSTATUS
IOCTL_FreeOTFECypherIOCTL_IntlDetails_v3(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
GetCypherDetails_v1(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v1* CypherDetails
);

NTSTATUS
GetCypherDetails_v3(
    IN     GUID*   CypherGUID,
    IN OUT CYPHER_v3* CypherDetails
);


// =========================================================================
// =========================================================================

#endif

