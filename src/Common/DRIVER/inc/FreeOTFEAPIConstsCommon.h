// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEAPIConstsCommon_H
#define _FreeOTFEAPIConstsCommon_H   1

#include "FreeOTFEPlatform.h"


// =========================================================================
// Const definitions

// Length in characters (not bytes)
// This is the max number of chars in:
//   *) A FreeOTFE device name
//   *) A volume's filename
// There's no *real* reason to use, this - and this does create a limitation (YUK!)
// The only reason this is used is so that creating and populating the DIOC buffers is
// slightly easier (the stuct lengths are more deterministic with this const in place)
// Things may be rewritten slightly in the future to remove this const
// xxxop - remove this const; dynamically alloc DIOC buffer which uses it
// Note: Should be (MAX_PATH + 1) (+1 to include terminating NULL character
//       (i.e. 260) at least for full compatibility with FAT32 systems
#define FREEOTFE_MAX_FILENAME_LENGTH   1024

#ifdef FOTFE_PDA
// Length in characters (not bytes)
// This is the max number of chars in a registry key's path
// There's no *real* reason to use, this - and this does create a limitation (YUK!)
// The only reason this is used is so that creating and populating the DIOC buffers is
// slightly easier (the stuct lengths are more deterministic with this const in place)
// Things may be rewritten slightly in the future to remove this const
// xxxop - remove this const; dynamically alloc DIOC buffer which uses it
#define FREEOTFE_MAX_REGKEY_LENGTH 1024
#endif


// The size of the critical data block
#define CRITICAL_DATA_LENGTH  4096  // In *bits*


// VolumeFlags bits:
#define VOL_FLAGS_USE_SECTOR_ID_AS_IV         1  // Bit 0
#define VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART     2  // Bit 1
// Bit 2 unused
#define VOL_FLAGS_HASH_SECTOR_ID_BEFORE_USE   8  // Bit 3
#define VOL_FLAGS_NORMAL_TIMESTAMPS          16  // Bit 4


// This returns status information on the named *disk* device
#define IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS  CTL_CODE(           \
                                                FILE_DEVICE_DISK,  \
                                                0x803,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// This returns metadata on the named *disk* device, as passed in when it was mounted
#define IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA  CTL_CODE(         \
                                                FILE_DEVICE_DISK,  \
                                                0x804,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )


// =========================================================================
// =========================================================================

#endif

