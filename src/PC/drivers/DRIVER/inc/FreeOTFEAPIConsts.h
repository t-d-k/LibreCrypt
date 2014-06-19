// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEAPIConsts_H
#define _FreeOTFEAPIConsts_H   1


// =========================================================================
// Const definitions

#define DEVICE_SLASH                  L"\\"

#define MAIN_DEVICE_NAME              L"FreeOTFE"
#define DEVICE_BASE_NAME              L"\\" MAIN_DEVICE_NAME
#define DEVICE_FREEOTFE_ROOT          L"\\Device" DEVICE_BASE_NAME
#define DEVICE_DOSDEVICES             L"\\DosDevices"
#define DEVICE_DOSDEVICES_GLOBAL      L"\\DosDevices\\Global"
#define DEVICE_SYMLINK_FREEOTFE_ROOT  DEVICE_DOSDEVICES

// Main device
#define DEVICE_MAIN_NAME              DEVICE_FREEOTFE_ROOT DEVICE_BASE_NAME
#define DEVICE_SYMLINK_MAIN_NAME      DEVICE_SYMLINK_FREEOTFE_ROOT DEVICE_BASE_NAME

// Disk devices
#define DEVICE_DISK_DIR_NAME          DEVICE_FREEOTFE_ROOT L"\\Disks"
#define DEVICE_DISK_PREFIX            DEVICE_DISK_DIR_NAME L"\\Disk"
// The following is just padding to represent the number tacked onto the
// end of DEVICE_DISK_PREFIX
#define DEVICE_DISK_NUMBER            L"zzzzz"



#define IOCTL_FREEOTFE_VERSION         CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x800,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

                                               
#define IOCTL_FREEOTFE_CREATE          CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x805,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )
                                               
#define IOCTL_FREEOTFE_SET_RAW         CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x809,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )
                                               
#define IOCTL_FREEOTFE_GET_RAW         CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x80A,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )
                                               
#define IOCTL_FREEOTFE_MOUNT           CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x801,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

#define IOCTL_FREEOTFE_DISMOUNT        CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x802,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

#define IOCTL_FREEOTFE_DESTROY         CTL_CODE(                   \
                                                FILE_DEVICE_DISK,  \
                                                0x806,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// This returns the number of *disk* devices which exist
#define IOCTL_FREEOTFE_GET_DISK_DEVICE_COUNT CTL_CODE(             \
                                                FILE_DEVICE_DISK,  \
                                                0x807,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// This returns the kernel mode devicenames of all *disk* devices which exist
#define IOCTL_FREEOTFE_GET_DISK_DEVICE_LIST CTL_CODE(              \
                                                FILE_DEVICE_DISK,  \
                                                0x808,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// CTL_CODE with 0x803 moved to FreeOTFEAPIConstsCommon.h
// CTL_CODE with 0x804 moved to FreeOTFEAPIConstsCommon.h


// Derive a key
#define IOCTL_FREEOTFE_DERIVE_KEY      CTL_CODE(                   \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x810,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Generate a MAC
#define IOCTL_FREEOTFE_GENERATE_MAC    CTL_CODE(                   \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x811,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Encrypt a block
#define IOCTL_FREEOTFE_ENCRYPT_BLOCK    CTL_CODE(                  \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x812,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Decrypt a block
#define IOCTL_FREEOTFE_DECRYPT_BLOCK    CTL_CODE(                  \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x813,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Lock, dismount, remove, eject and unlock
// Originally added for Vista, where the user can't do this if UAC is
// operational, and the userspace software isn't running with escalated privs
#define IOCTL_FREEOTFE_LDREU            CTL_CODE(                   \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x814,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Create DOS symlink for device
#define IOCTL_FREEOTFE_DOS_MOUNTPOINT_CREATE  CTL_CODE(            \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x815,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Create DOS symlink for device
#define IOCTL_FREEOTFE_DOS_MOUNTPOINT_DELETE  CTL_CODE(            \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x816,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )


// =========================================================================
// =========================================================================

#endif

