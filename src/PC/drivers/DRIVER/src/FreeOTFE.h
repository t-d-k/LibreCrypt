// Description: FreeOTFE Device Driver
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE_H
#define _FreeOTFE_H   1


// The following includes are required in order to implement Cancel-Safe
// IRP Queues
#include <ntddk.h>
//#include <wdm.h>
#include <csq.h>

#include <ntverp.h>  // Needed for VER_PRODUCTBUILD

#include <initguid.h>  // Required for TEST_GUID_...

#include "FreeOTFECypherAPI.h"
#include "FreeOTFEHashAPI.h"
#include "FreeOTFEPlatform.h"


// =========================================================================
// Const definitions

// These values are in DECIMAL
#define DRIVER_VERSION_MAJOR     05
#define DRIVER_VERSION_MINOR     00
#define DRIVER_VERSION_BUILD   0000

#define DRIVER_VERSION  (                                      \
                         (DRIVER_VERSION_MAJOR * 0x01000000) + \
                         (DRIVER_VERSION_MINOR * 0x00010000) + \
                         (DRIVER_VERSION_BUILD * 0x00000001)   \
                        )

#define MAX_DEVICES 26

// Note: See also the definition of ENCRYPTION_BLOCK_SIZE
#define HDD_BYTES_PER_SECTOR 512
#define CD_BYTES_PER_SECTOR  2048
// These figures are used; no real reason for these *exact* numbers, except
// that they're the same defaults as used by Windows NT for SCSI devices
// when it can't get the real values from the SCSI BIOS.
// See MS Knowledgebase article Q258281 for further details
// Tracks per cylinder = the number of heads
#define TRACKS_PER_CYLINDER 64
#define SECTORS_PER_TRACK 32


// Definition used for testing purposes; dummy GUID with 0x0F as it's last byte
DEFINE_GUID(TEST_GUID_0x0F,  0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0F);


#define CD_FRAMES_PER_SECOND 75
#define CD_FRAMES_PER_MINUTE (CD_FRAMES_PER_SECOND * 60)

// From winioctl.h
#define FSCTL_LOCK_VOLUME               CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  6, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_UNLOCK_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  7, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_DISMOUNT_VOLUME           CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  8, METHOD_BUFFERED, FILE_ANY_ACCESS)


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


// Handle read IRPs
NTSTATUS
FreeOTFE_MF_DispatchRead(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Handle write IRPs
NTSTATUS
FreeOTFE_MF_DispatchWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Handle flush buffers IRPs
NTSTATUS
FreeOTFE_MF_DispatchFlushBuffers(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

    
// Handle power IRPs
NTSTATUS
FreeOTFE_MF_DispatchPower(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Handle system control IRPs
NTSTATUS
FreeOTFE_MF_DispatchSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

NTSTATUS
CreateMainDevice (
    IN PDRIVER_OBJECT   DriverObject,
    OUT PDEVICE_OBJECT  *DeviceObject
);


NTSTATUS
CreateDiskDevice (
    IN PDRIVER_OBJECT   DriverObject,
    IN ULONG DeviceType,
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


NTSTATUS
GetNamedCharDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PCHAR TgtDeviceName, 
    OUT PDEVICE_OBJECT* DeviceObject
);
    
    
NTSTATUS
GetNamedUnicodeDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING TgtDeviceName, 
    OUT PDEVICE_OBJECT* DeviceObject
);


// Flush buffers on the specified device
NTSTATUS
ThreadFlushBuffers(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
);

// Read/write data to the specified device
NTSTATUS
ThreadReadWriteData(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
);


// Mount the specified device
NTSTATUS
ThreadMount(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
);


// Dismount the specified device
NTSTATUS
ThreadDismount(
    IN PDEVICE_OBJECT DeviceObject,
    IN OUT PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_Version(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_Create(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);
    
    
NTSTATUS
IOCTL_FreeOTFEIOCTL_Destroy(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


// Write raw data to volume
NTSTATUS
IOCTL_FreeOTFEIOCTL_SetRaw(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

    
// Read raw data from volume
NTSTATUS
IOCTL_FreeOTFEIOCTL_GetRaw(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

// Derive a key
// This is implemented in the kernel mode driver as implementing it in userspace (e.g. the
// Delphi frontend) takes *far* too long to process the data due to the repeated calls to hash
// functions, etc
NTSTATUS
IOCTL_FreeOTFEIOCTL_DeriveKey(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

    
// Generate a MAC
// Implmented in the kernel mode driver because it's required by the key derivation process
NTSTATUS
IOCTL_FreeOTFEIOCTL_GenerateMAC(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_Mount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
);
    
    
NTSTATUS
IOCTL_FreeOTFEIOCTL_DOSMountpointCreate(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

NTSTATUS
IOCTL_FreeOTFEIOCTL_DOSMountpointDelete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

NTSTATUS
IOCTL_FreeOTFEIOCTL_LDREU(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_Dismount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceCount(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceList(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceStatus(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);
      

NTSTATUS
IOCTL_FreeOTFEIOCTL_GetDiskDeviceMetaData(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);

NTSTATUS
IOCTL_Std_DiskFormatTracks(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_DiskIsWritable(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_DiskGetDriveGeometry(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_DiskGetDriveLayout(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetDriveLayoutEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);
#endif

#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetLengthInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);
#endif


NTSTATUS
IOCTL_Std_DiskGetPartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
NTSTATUS
IOCTL_Std_DiskGetPartitionInfoEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);
#endif


NTSTATUS
IOCTL_Std_DiskSetPartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_DiskSetPartitionInfoEx(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_DiskVerify(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_CheckVerify(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_MediaRemoval(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
IOCTL_Std_StorageEjectMedia(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PDEVICE_OBJECT *QueueDeviceObject
);


NTSTATUS
IOCTL_Std_CDROMReadTOC(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
);


NTSTATUS
GetDeviceDetailsHash(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_HASH* HashDetails
);

// Overwrite, free off and release everything that may have been
// created/obtained by a prior call to GetDeviceDetailsHash(...)
void
FreeDeviceDetailsHash(
    OUT     MODULE_DETAILS_HASH* HashDetails
);


NTSTATUS
GetDeviceDetailsCypher_v1(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v1* CypherDetails
);

NTSTATUS
GetDeviceDetailsCypher_v3(
    IN      char* deviceName,
    IN      GUID* supportGUID,

    OUT     MODULE_DETAILS_CYPHER_v3* CypherDetails
);

NTSTATUS
GetDeviceDetailsCypher_v3_v1(
    IN      char* deviceName,
    IN      GUID* supportGUID,

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
SynchCompletionRoutine(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PKEVENT Event
);


// Overwrite sensitive parts of the specified device's extension
NTSTATUS
OverwriteDeviceSensitive(
    IN  PDEVICE_OBJECT devObj
);


    
// Write/Read raw data to/from volume
// DoWrite - set to TRUE to write, FALSE to get
NTSTATUS
SetGetRawChar(
    IN     FREEOTFEBOOL DoWrite,
    IN     PCHAR Filename,
    IN     LARGE_INTEGER Offset,
    IN     ULONG DataLength,
    IN OUT FREEOTFEBYTE* Data
);
    
// Write/Read raw data to/from volume
// DoWrite - set to TRUE to write, FALSE to get
NTSTATUS
SetGetRawUnicode(
    IN     FREEOTFEBOOL DoWrite,
    IN     PUNICODE_STRING Filename,
    IN     LARGE_INTEGER Offset,
    IN     ULONG DataLength,
    IN OUT FREEOTFEBYTE* Data
);
    
    
// Determine the max size of a file
NTSTATUS
GetMaxSizeFile(
    IN  HANDLE FileHandle,
    OUT PLARGE_INTEGER MaxSize
);

// Determine the max size of a partition
// DeviceName - The device name of the partition
//              e.g. \Device\Harddisk1\Partition1
NTSTATUS
GetMaxSizePartition(
    IN  PUNICODE_STRING DeviceName,
    OUT PLARGE_INTEGER MaxSize
);


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
);


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
);

// Perform an actual read from the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
NTSTATUS
ActualReadFile(
    IN      PDEVICE_OBJECT DeviceObject,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    IN      FREEOTFEBYTE* Buffer
);

// Perform an actual write to the encrypted volume
// This can be supplied with an arbitary Offset and BufferLength
NTSTATUS
ActualWriteFile(
    IN      PDEVICE_OBJECT DeviceObject,
    IN      PLARGE_INTEGER Offset,
    IN      ULONG BufferLength,
    OUT     FREEOTFEBYTE* Buffer
);

#if (VER_PRODUCTBUILD >= 2600)
// Windows XP and later only
// Populate PARTITION_INFORMATION/PARTITION_INFORMATION_EX structures
NTSTATUS
PopulatePartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    OUT PPARTITION_INFORMATION PartitionInfo,
    OUT PPARTITION_INFORMATION_EX PartitionInfoEx
);
#else
// Populate PARTITION_INFORMATION/PARTITION_INFORMATION_EX structures
NTSTATUS
PopulatePartitionInfo(
    IN PDEVICE_OBJECT DeviceObject,
    OUT PPARTITION_INFORMATION PartitionInfo
);
#endif

// Identify if emulating a CD or DVD device
FREEOTFEBOOL
EmulatingCDOrDVD(
    IN PDEVICE_OBJECT DeviceObject
);


NTSTATUS
ClientSecurityCreate(
    OUT     PSECURITY_CLIENT_CONTEXT *ClientContext  // Pointer to a pointer
);

NTSTATUS
ClientSecurityDestroy(
    IN OUT  PSECURITY_CLIENT_CONTEXT *ClientContext  // Pointer to a pointer
);


NTSTATUS
FileOpen(
    IN      PUNICODE_STRING Filename,
    IN      PSECURITY_CLIENT_CONTEXT ClientContext,
    IN      FREEOTFEBOOL ReadOnly,
    IN      FREEOTFEBOOL IntermediateBufferingOK,
    OUT     PHANDLE FileHandle,
    OUT     PIO_STATUS_BLOCK IoStatusBlock,
    OUT     PBOOLEAN GotFileAttributes,
    OUT     PFILE_BASIC_INFORMATION FileAttributes
);

NTSTATUS
FileClose(
    IN OUT  PHANDLE FileHandle,
    IN      FREEOTFEBOOL AttributesStored,
    IN      PFILE_BASIC_INFORMATION FileAttributes,
    OUT     PIO_STATUS_BLOCK IoStatusBlock
);

NTSTATUS
LDREUChar(
    IN PDEVICE_OBJECT DeviceObject,
    IN      CHAR DriveFile[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      CHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      BOOLEAN  Emergency
);

NTSTATUS
LDREUUnicode(
    IN PDEVICE_OBJECT DeviceObject,
    IN      PUNICODE_STRING DriveFile,
    IN      CHAR DeviceNameAnsi[FREEOTFE_MAX_FILENAME_LENGTH],  // ANSI version
    IN      PUNICODE_STRING DeviceName,
    IN      BOOLEAN  Emergency
);


NTSTATUS
SendFOTFEDiskDeviceDIOCChar(
    IN PDEVICE_OBJECT DeviceObject,
    IN      char* TgtDeviceName,
    IN      ULONG   ControlCode,
    IN      ULONG   InBufferSize,
    IN      void*   InBuffer
);

NTSTATUS
SendFOTFEDiskDeviceDIOCUnicode(
    IN PDEVICE_OBJECT DeviceObject,
    IN      PUNICODE_STRING TgtDeviceName,
    IN      ULONG   ControlCode,
    IN      ULONG   InBufferSize,
    IN      void*   InBuffer
);


NTSTATUS
AllocateUnicodeString(
     IN   CHAR* ansiString,
     OUT  PUNICODE_STRING* pUNICODEString
);

VOID
FreeUnicodeString(
    PUNICODE_STRING ptrUNICODEString
);

NTSTATUS
CreateSymlink(
    IN      CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH],
    IN      PUNICODE_STRING LinkName
);

NTSTATUS
AllocateDOSDeviceSymlinkTargetName_CharMountpoint(
    IN  char* Mountpoint,
    IN  BOOLEAN Global,
    OUT PUNICODE_STRING* SymLinkName
);

NTSTATUS
AllocateDOSDeviceSymlinkTargetName_WCHARDriveLetter(
    IN  WCHAR DriveLetter,
    IN  BOOLEAN Global,
    OUT PUNICODE_STRING* SymLinkName
);

VOID
FreeDOSDeviceSymlinkTargetName(
    PUNICODE_STRING SymLinkName
);


// This function is for testing new functionality ONLY
NTSTATUS TestCode();


// =========================================================================
// =========================================================================
// =========================================================================

#endif


