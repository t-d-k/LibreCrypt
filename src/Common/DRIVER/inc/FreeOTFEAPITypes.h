// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEAPITypes_H
#define _FreeOTFEAPITypes_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <windef.h>  // Required for definition of ULONG
#endif
#ifdef FOTFE_PC_DLL
#include <windef.h>  // Required for definition of ULONG
#endif
#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>  // Required for ntddstor.h
//xxx - junk #include <windef.h>
#include <ntdef.h>  // Required for ULONG in ntddstor.h
#include <ntddstor.h>  // Required for definition of STORAGE_MEDIA_TYPE
#endif

#include "FreeOTFEAPIConstsCommon.h"



// =========================================================================
// Type definitions

// Mount source
typedef enum _MOUNT_SOURCE {
    MNTSRC_FILE             =    1,
    MNTSRC_PARTITION        =    2,
    MNTSRC_UNKNOWN          = 9999
} MOUNT_SOURCE, *PMOUNT_SOURCE;


// IV generation method
typedef enum _SECTOR_IV_GEN_METHOD {
    SCTRIVGEN_NONE                 =   0,
    SCTRIVGEN_32BIT_SECTOR_ID      =   1,
    SCTRIVGEN_64BIT_SECTOR_ID      =   2,
    SCTRIVGEN_HASH_32BIT_SECTOR_ID =   3,
    SCTRIVGEN_HASH_64BIT_SECTOR_ID =   4,
    SCTRIVGEN_ESSIV                =   5,
    SCTRIVGEN_UNKNOWN              = 255
} SECTOR_IV_GEN_METHOD, *PSECTOR_IV_GEN_METHOD;


// Key derivation algorithm to use
typedef enum _KDF_ALGORITHM {
    KDF_HASH     =    1,
    KDF_PBKDF2   =    2,
    KDF_UNKNOWN  = 9999
} KDF_ALGORITHM, *PKDF_ALGORITHM;
//const KDF_ALGORITHM ARR_KDF_ALGORITHM[] = {KDF_HASH, KDF_PBKDF2, KDF_UNKNOWN};

// MAC algorithm to use
typedef enum _MAC_ALGORITHM {
    MAC_HASH     =    1,
    MAC_HMAC     =    2,
    MAC_UNKNOWN  = 9999
} MAC_ALGORITHM, *PMAC_ALGORITHM;
//const MAC_ALGORITHM ARR_MAC_ALGORITHM[] = {MAC_HASH, MAC_HMAC, MAC_UNKNOWN};


typedef struct _DIOC_VERSION {
    ULONG VersionID;    
} DIOC_VERSION, *PDIOC_VERSION;


typedef struct _DIOC_DISK_DEVICE_CREATE {
    ULONG  DeviceType;  // Set to one of the system defined FILE_DEVICE_XXX
                        // constants
} DIOC_DISK_DEVICE_CREATE, *PDIOC_DISK_DEVICE_CREATE;


// This struct returns the device kernel mode name which can then be passed
// in a "DefineDosDevice" call in user mode
typedef struct _DIOC_DEVICE_NAME {
    CHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
} DIOC_DEVICE_NAME, *PDIOC_DEVICE_NAME;


// This struct returns a list of device kernel mode names
typedef struct _DIOC_DEVICE_NAME_LIST {
    ULONG DeviceCount;
    CHAR DeviceName[1][FREEOTFE_MAX_FILENAME_LENGTH];  // Variable number of device names
} DIOC_DEVICE_NAME_LIST, *PDIOC_DEVICE_NAME_LIST;


typedef struct _DIOC_MOUNT {
#ifdef FOTFE_PC_DRIVER
    CHAR DiskDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
#endif

#ifdef FOTFE_PDA
    // If this is FALSE, don't touch the registry settings - only operate as 
    // simple DLL, not driver DLL
    FREEOTFEBOOL FullMount;

    // Note: Under WinCE, mountpoint also passed in via the registry key set
    //       prior to mount
    WCHAR Mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
#endif

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif

    // Start and end offsets of encrypted partition within the file
    // If DataEnd is zero, it will be set to the last byte in the file
    LARGE_INTEGER DataStart;
    LARGE_INTEGER DataEnd;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR IVHashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR IVHashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID IVHashGUID;
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR IVCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR IVCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID IVCypherGUID;
    
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR MainCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR MainCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID MainCypherGUID;

    BOOLEAN ReadOnly;
    MOUNT_SOURCE MountSource;  // Identifies what the encrypted data is stored on
#ifdef FOTFE_PDA
    // Identifies what the virtual device should emulate
    // This should be populated with the dwDeviceType member of a
    // STORAGEDEVICEINFO struct
    DWORD DeviceType;  
#endif
#ifdef FOTFE_PC_DRIVER
    STORAGE_MEDIA_TYPE StorageMediaType;  // Identifies what the virtual device should emulate
#endif

    unsigned int VolumeFlags;
    SECTOR_IV_GEN_METHOD SectorIVGenMethod;

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->VolumeIVLength".
    //       If the order was: "MasterKey", "VolumeIVLength", "VolumeIV", then we could not access
    //       "VolumeIVLength" directly, and would have to take "MasterKeyLength" into account before
    //       accessing it.
    ULONG MasterKeyLength;  // In bits
    ULONG VolumeIVLength;  // In bits
    ULONG MetaDataLength;  // In bytes
    CHAR MasterKey[1];    
    CHAR VolumeIV[1];    
    CHAR MetaData[1];
} DIOC_MOUNT, *PDIOC_MOUNT;


typedef struct _DIOC_DISMOUNT {
    CHAR DiskDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    BOOLEAN Emergency;
} DIOC_DISMOUNT, *PDIOC_DISMOUNT;


typedef struct _DIOC_DISK_DEVICE_COUNT {
    ULONG Count;    
} DIOC_DISK_DEVICE_COUNT, *PDIOC_DISK_DEVICE_COUNT;



typedef struct _DIOC_DISK_DEVICE_STATUS {
#ifdef FOTFE_PDA
    WCHAR Mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
#endif
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER  
    CHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif

#ifdef FOTFE_PC_DRIVER
    BOOLEAN Mounted;
#endif
    
#ifdef FOTFE_PDA
    HANDLE ActivateDeviceHandle;
    WCHAR BuiltinKey[FREEOTFE_MAX_REGKEY_LENGTH];    
    WCHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH];  // e.g. DSK4:
#endif

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR IVHashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR IVHashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID IVHashGUID;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR IVCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR IVCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID IVCypherGUID;
    
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR MainCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR MainCypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID MainCypherGUID;

    BOOLEAN DismountPending;
    BOOLEAN ReadOnly;

    unsigned int VolumeFlags;
    SECTOR_IV_GEN_METHOD SectorIVGenMethod;

    ULONG MetaDataLength;  // In bytes
} DIOC_DISK_DEVICE_STATUS, *PDIOC_DISK_DEVICE_STATUS;


typedef struct _DIOC_DISK_DEVICE_METADATA {
    ULONG MetaDataLength;  // In bytes
    CHAR MetaData[1];
} DIOC_DISK_DEVICE_METADATA, *PDIOC_DISK_DEVICE_METADATA;


typedef struct _DIOC_SET_RAW_DATA {
    CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];
    LARGE_INTEGER Offset;
    ULONG DataLength;
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_SET_RAW_DATA, *PDIOC_SET_RAW_DATA;


typedef struct _DIOC_GET_RAW_DATA_IN {
    CHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];
    LARGE_INTEGER Offset;
    ULONG DataLength;
} DIOC_GET_RAW_DATA_IN, *PDIOC_GET_RAW_DATA_IN;

typedef struct _DIOC_GET_RAW_DATA_OUT {
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_GET_RAW_DATA_OUT, *PDIOC_GET_RAW_DATA_OUT;


typedef struct _DIOC_DERIVE_KEY_IN {
    KDF_ALGORITHM KDFAlgorithm;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID HashGUID;
    
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID CypherGUID;

    unsigned int Iterations;
    unsigned int LengthWanted;  // In bits - the length of the output key wanted

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "PasswordLength", "Password", "SaltLength", "Salt", then we could not access
    //       "SaltLength" directly, and would have to take "PasswordLength" into account before
    //       accessing it.
    unsigned int PasswordLength;  // In bits
    unsigned int SaltLength;  // In bits
    FREEOTFEBYTE Password[1];  // Variable length
    FREEOTFEBYTE Salt[1];  // Variable length
} DIOC_DERIVE_KEY_IN, *PDIOC_DERIVE_KEY_IN;

typedef struct _DIOC_DERIVE_KEY_OUT {
    unsigned int DerivedKeyLength;  // In bits
    FREEOTFEBYTE DerivedKey[1];  // Variable length
} DIOC_DERIVE_KEY_OUT, *PDIOC_DERIVE_KEY_OUT;


typedef struct _DIOC_GENERATE_MAC_IN {
    MAC_ALGORITHM MACAlgorithm;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR HashDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID HashGUID;
    
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    WCHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
#ifdef FOTFE_PC_DRIVER
    CHAR CypherDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];    
#endif
    GUID CypherGUID;

    int LengthWanted;

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "KeyLength", "Key", "DataLength", "Data", then we could not access
    //       "DataLength" directly, and would have to take "KeyLength" into account before
    //       accessing it.
    unsigned int KeyLength;  // In bits
    unsigned int DataLength;  // In bits
    FREEOTFEBYTE Key[1];  // Variable length
    FREEOTFEBYTE Data[1];  // Variable length
} DIOC_GENERATE_MAC_IN, *PDIOC_GENERATE_MAC_IN;

typedef struct _DIOC_GENERATE_MAC_OUT {
    unsigned int MACLength;  // In bits
    FREEOTFEBYTE MAC[1];  // Variable length
} DIOC_GENERATE_MAC_OUT, *PDIOC_GENERATE_MAC_OUT;

#ifdef FOTFE_PDA
typedef struct _DIOC_USER_DEVICE_HANDLE {
    HANDLE UserSpaceDeviceHandle;
} DIOC_USER_DEVICE_HANDLE, *PDIOC_USER_DEVICE_HANDLE;
#endif

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
typedef struct _DIOC_FORCE_DISMOUNTS {
    BOOL ForceDismounts;
} DIOC_FORCE_DISMOUNTS, *PDIOC_FORCE_DISMOUNTS;
#endif

#ifdef FOTFE_PC_DRIVER
typedef struct _DIOC_LDREU {
    CHAR DriveFile[FREEOTFE_MAX_FILENAME_LENGTH];
    CHAR DeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    BOOLEAN Emergency;
} DIOC_LDREU, *PDIOC_LDREU;
#endif

#ifdef FOTFE_PC_DRIVER
typedef struct _DIOC_DOS_MOUNTPOINT {
    CHAR DiskDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    BOOLEAN Global;
    // Not just a char, for futureproofing API to allow for proper mountpoints;
    // at present, only the first character is relevant
    CHAR Mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
} DIOC_DOS_MOUNTPOINT, *PDIOC_DOS_MOUNTPOINT;
#endif


// =========================================================================
// =========================================================================

#endif

