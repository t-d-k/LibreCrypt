// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#ifndef _FreeOTFEContext_H
#define _FreeOTFEContext_H   1

#include "FreeOTFEPlatform.h"

// This *must* be early, in order to prevent problems with ntddstor.h
#include "FreeOTFEAPITypes.h"

#ifdef FOTFE_PDA
#include <Diskio.h>  // Required for DISK_INFO
#include <Storemgr.h>  // Required for STORAGEDEVICEINFO

#include "FreeOTFE4PDARegistry.h" // Required for REGDETAILS_ACTIVE
#endif
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
#include "FreeOTFE4PDAHashAPI.h"  // Required for hash function type
#include "FreeOTFE4PDACypherAPI.h"  // Required for hash function type
#endif
#ifdef FOTFE_PC_DRIVER
//#include <windef.h>  // Required for DWORD
//#include <Winioctl.h>  // Required for DISK_GEOMETRY
#include <ntdddisk.h>  // Required for DISK_GEOMETRY
#include "IFSRelated.h"  // Requird for PSECURITY_CLIENT_CONTEXT
#endif

#include "FreeOTFEHashAPICommon.h"  // Required for HASH
#include "FreeOTFECypherAPICommon.h"  // Required for CYPHER


// =========================================================================
// Type definitions

typedef struct _MODULE_DETAILS_HASH {
#ifdef FOTFE_PDA
    WCHAR* DeviceName;
#endif
#ifdef FOTFE_PC_DLL
    WCHAR* DeviceName;  // Included for convenience; set to DLL library
#endif
#ifdef FOTFE_PC_DRIVER
    UNICODE_STRING DeviceName;
#endif
    GUID HashGUID;

    // IV Hash device handle
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    HINSTANCE Lib;
#endif
#ifdef FOTFE_PC_DRIVER
    PFILE_OBJECT FileObject;
    PDEVICE_OBJECT DeviceObject;
#endif
	
	// IV Hash device internal details
    HASH Details;
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    PHashDLLFnHash FnHash;
#endif
#ifdef FOTFE_PC_DRIVER
    PDataHashFn FnHash;
#endif
} MODULE_DETAILS_HASH, *PMODULE_DETAILS_HASH;



typedef struct _MODULE_DETAILS_CYPHER_v1 {
#ifdef FOTFE_PDA
    WCHAR* DeviceName;
#endif
#ifdef FOTFE_PC_DLL     
    WCHAR* DeviceName;  // Included for convenience; set to DLL library
#endif
#ifdef FOTFE_PC_DRIVER  
    UNICODE_STRING DeviceName;
#endif
    GUID CypherGUID;

    // IV cypher device handle
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    HINSTANCE Lib;
#endif
#ifdef FOTFE_PC_DRIVER
    PFILE_OBJECT FileObject;
    PDEVICE_OBJECT DeviceObject;
#endif

    // IV cypher device internal details
    CYPHER_v1 Details;
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    PCypherDLLFnEncryptWithASCII FnEncrypt;
    PCypherDLLFnDecryptWithASCII FnDecrypt;
#endif
#ifdef FOTFE_PC_DRIVER
    PDataEncryptFn FnEncrypt;
    PDataDecryptFn FnDecrypt;
#endif

} MODULE_DETAILS_CYPHER_v1, *PMODULE_DETAILS_CYPHER_v1;

typedef struct _MODULE_DETAILS_CYPHER_v3 {
#ifdef FOTFE_PDA
    WCHAR* DeviceName;
#endif
#ifdef FOTFE_PC_DLL     
    WCHAR* DeviceName;  // Included for convenience; set to DLL library
#endif
#ifdef FOTFE_PC_DRIVER  
    UNICODE_STRING DeviceName;
#endif
    GUID CypherGUID;

    // IV cypher device handle
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    HINSTANCE Lib;
#endif
#ifdef FOTFE_PC_DRIVER
    PFILE_OBJECT FileObject;
    PDEVICE_OBJECT DeviceObject;
#endif

    // IV cypher device internal details
    CYPHER_v3 Details;
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    PCypherDLLFnEncryptWithASCII       FnEncrypt;
    PCypherDLLFnDecryptWithASCII       FnDecrypt;
    // ! WARNING !
    // The next ones aren't populated for older (pre v3) cypher drivers
    PCypherDLLFnEncryptSectorWithASCII FnEncryptSector;
    PCypherDLLFnDecryptSectorWithASCII FnDecryptSector;
#endif
#ifdef FOTFE_PC_DRIVER
    PDataEncryptFn       FnEncrypt;
    PDataDecryptFn       FnDecrypt;
    // ! WARNING !
    // The next ones aren't populated for older (pre v3) cypher drivers
    PSectorDataEncryptFn FnEncryptSector;
    PSectorDataDecryptFn FnDecryptSector;
#endif

} MODULE_DETAILS_CYPHER_v3, *PMODULE_DETAILS_CYPHER_v3;



typedef struct _DEVICE_EXTENSION {

    // ------------------------------------------------------
    // Disk device items ONLY
    
    // Flag if a volume is mounted or not
    FREEOTFEBOOL Mounted;

    // Flag if a volume is being dismounted or not
    // i.e. This flag determines if any more IRPs should be queued for
    // the thread or not. If set, no further IRPs will be accepted
    FREEOTFEBOOL DismountPending;
    
    // Mount source (e.g. the volume file is a partition or file)
    MOUNT_SOURCE MountSource; 

    // Filename of any mounted volume
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR* zzFilename;
#endif
#ifdef FOTFE_PC_DRIVER
    UNICODE_STRING zzFilename;
#endif

    // Handle to the volume file
    HANDLE FileHandle;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    // Flag if file attributes have been stored (e.g. FALSE for partitions, etc)
    FREEOTFEBOOL FileAttributesStored;
    // If stored, the file attributes (e.g. archive bit set)
    DWORD FileAttributes;
    // Flag if file timestamps have been stored (e.g. FALSE for partitions, etc)
    FREEOTFEBOOL FileTimestampsStored;
	// If stored, the file timestamps
    FILETIME CreationTime;
    FILETIME LastAccessTime;
    FILETIME LastWriteTime;
#endif
#ifdef FOTFE_PC_DRIVER
    // Flag if file attributes have been stored (e.g. FALSE for partitions, etc)
    FREEOTFEBOOL FileAttributesStored;
    // If stored, the file timestamps/attributes when opened
    FILE_BASIC_INFORMATION FileAttributes;
#endif

    // Start of encrypted data within the file
    // Note: We don't need to store the end offset, as this can be determined by using the
    //       DiskGrometry/DiskSize members
    LARGE_INTEGER DataOffset;

    // Simulated disk geometry
    LARGE_INTEGER PartitionSize; 
#ifdef FOTFE_PDA
    DISK_INFO DiskGeometry;
#endif
#ifdef FOTFE_PC_DLL
    DISK_GEOMETRY DiskGeometry;
#endif
#ifdef FOTFE_PC_DRIVER
    DISK_GEOMETRY DiskGeometry;
#endif
    LARGE_INTEGER DiskSize;  

#ifdef FOTFE_PC_DRIVER
//lplp - is this needed for the PC DLL???
    // Count of hidden sectors (if any)
    ULONG HiddenSectors;
#endif

    // The "sector size" in which blocks should actually be read/written to the volume
    // file/partition
    ULONG FileSectorSize;
    // DataOffset % DiskGeometry->SectorSize
    ULONG DataOffsetModVirtualSectorSize; 

    // Encryption block size
    // This *should* always be set to the sector size of the emulated device, but isn't always
    // (e.g. Linux ISO images have an emulated 2048 byte sector size, but encrypt in 512 byte
    // blocks)
    ULONG EncryptionBlockSize;

    // Readonly flag
    FREEOTFEBOOL ReadOnly;

    // If this is FALSE, don't touch the registry settings - only operate as 
    // simple DLL, not driver DLL
    // In this case, the following will not be populated:
    //
    //   StorageDeviceInfo
    //   RegdetailsActive
    //
    FREEOTFEBOOL FullMount;

#ifdef FOTFE_PDA
    // Storage device info (i.e. the type of device emulated)
    STORAGEDEVICEINFO StorageDeviceInfo;
#endif
#ifdef FOTFE_PC_DRIVER  
    // Storage media type (i.e. the type of device emulated)
    STORAGE_MEDIA_TYPE StorageMediaType;
#endif


    // Prevent media removal; for removable disks only
    FREEOTFEBOOL PreventMediaRemoval;
        
    // Sector IV generation method
    SECTOR_IV_GEN_METHOD SectorIVGenMethod;

    // -----
	// IV Hash device ID
    MODULE_DETAILS_HASH IVHash;

    // -----
	// IV cypher device ID
    MODULE_DETAILS_CYPHER_v3 IVCypher;

    // -----
    // Main cypher device ID
    MODULE_DETAILS_CYPHER_v3 MainCypher;


    // -----
    // Key to be used for encryption/decryption
    ULONG MasterKeyLength;  // This value is in *bits*
    FREEOTFEBYTE *MasterKey;
    // MasterKeyASCII is the ASCII representation of MasterKey
    // This is stored as all AES candidates use this format for supplying
    // their keys (blame NIST for that...)
    unsigned char *MasterKeyASCII;  // Hex ASCIIZ nibbles

    // Key to be used for ESSIV generation (*if* *required*)
    ULONG ESSIVKeyLength;  // This value is in *bits*
    FREEOTFEBYTE *ESSIVKey;
    // ESSIVKeyASCII is the ASCII representation of ESSIVKey
    // This is stored as all AES candidates use this format for supplying
    // their keys (blame NIST for that...)
    FREEOTFEBYTE *ESSIVKeyASCII;  // Hex ASCIIZ nibbles

    // Volume IV to be used to encrypt/decrypt each sector
    ULONG VolumeIVLength;  // This value is in *bits*
    FREEOTFEBYTE *VolumeIV;

    // -----
    // Various flags
    unsigned int VolumeFlags;

    // Metadata
    // This is used to store arbitary user-mode data when the volume is mounted.
    // This data will be returned whenever the status is the disk is returned
    ULONG MetaDataLength;  // This value is in *bytes*
    FREEOTFEBYTE *MetaData;

    // ------------------------------------------------------
    // ------------------------------------------------------
    // ------------------------------------------------------
 
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // WinCE specific

    // Mutex...
    LPCRITICAL_SECTION CriticalSection;

    // Number of times opened
    DWORD OpenCount;

#endif
#ifdef FOTFE_PDA
    // Registry info...
    // From MSDN:
    //   "The initialization function can read and create new values in the
    //   Active key; however, it is not permitted to access the key after the
    //   initialization function returns."
    // - so we store it's contents here.
    REGDETAILS_ACTIVE RegdetailsActive;

    // Mountpoint (also in registry)
    WCHAR* Mountpoint;

    // Handle to device, as returned to the user app by ActivateDeviceEx(...)
    HANDLE UserSpaceDeviceHandle;
#endif
#ifdef FOTFE_PC_DRIVER
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // PC kernel driver specific

    // Allow differentiation between the main device and disk devices
    FREEOTFEBOOL IsMainDevice;
    
    // Device's name
    UNICODE_STRING zzDeviceName;

    // ------------------------------------------------------
    // Main device items ONLY
    
    // Symbolic link name
    UNICODE_STRING zzSymbolicLinkName;
    
    // Security context for volume file handle
    PSECURITY_CLIENT_CONTEXT ClientContext;

    // File object of volume file handle
    PFILE_OBJECT FileObject;

    // ------------------------------------------------------
    // Thread/IRP queue related items follow    
    
    // Flag to signal that the thread should terminate
    FREEOTFEBOOL TerminateThread;
    
    // The device's IRP processing thread
    PETHREAD ThreadObject;
    
    // Irps waiting to be processed are queued here
    LIST_ENTRY   PendingIRPQueue;

    //  SpinLock to protect access to the queue
    KSPIN_LOCK IRPQueueLock;
    
    IO_CSQ CancelSafeQueue;   
    KSEMAPHORE IRPQueueSemaphore;
#endif

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

// xxx - get rid of the above for this
#define DEVICE_CONTEXT DEVICE_EXTENSION 


// =========================================================================

void
MoveMODULE_DETAILS_CYPHER_v1ToMODULE_DETAILS_CYPHER_v3 (
    IN     MODULE_DETAILS_CYPHER_v1*  Source,
    OUT    MODULE_DETAILS_CYPHER_v3*  Dest
);


// =========================================================================
// =========================================================================
// =========================================================================

#endif

