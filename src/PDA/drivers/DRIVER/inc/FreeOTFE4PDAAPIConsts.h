// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAAPIConsts_H
#define _FreeOTFE4PDAAPIConsts_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA        
#include <Windev.h>  // Required for CTL_CODE, etc
#endif


// =========================================================================
// Const definitions

// Pass to the driver the device handle returned to user app by
// ActivateDeviceEx
#define IOCTL_FREEOTFE_USER_DEV_HANDLE_SET CTL_CODE(               \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x815,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )

// Get to the driver the device handle returned to user app by
// ActivateDeviceEx
#define IOCTL_FREEOTFE_USER_DEV_HANDLE_GET CTL_CODE(               \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x816,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )
// Get to the driver the device handle returned to user app by
// ActivateDeviceEx
#define IOCTL_FREEOTFE_SET_FORCE_DISMOUNT CTL_CODE(               \
                                                FILE_DEVICE_UNKNOWN,  \
                                                0x817,             \
                                                METHOD_BUFFERED,   \
                                                FILE_READ_DATA     \
                                               )


// =========================================================================
// =========================================================================

#endif

