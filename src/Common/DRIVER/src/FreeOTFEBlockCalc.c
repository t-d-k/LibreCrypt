// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"
#include "FreeOTFEBlockCalc.h"


// =========================================================================
// =========================================================================
LARGE_INTEGER
GetBlockID(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER fileOffset  // The offset within the *volume file* where
                                       // the data was obtained from
)
{
    LARGE_INTEGER blockID;

    // Identify block ID, based on whether block ID zero is the first
    // "devContext->EncryptionBlockSize" bytes from the start of the hosting volume
    // file or the first "devContext->EncryptionBlockSize" bytes from the start of the
    // encrypted partition
    if (devContext->VolumeFlags & VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART)
        {
        // This is correct for default linux - block IDs start, and are relative
        // to, the start of the hosting file; though this may be changed by the use
        // of the "@" with the linux "losetup" command
        blockID.QuadPart = (
                            fileOffset.QuadPart /
                                    devContext->EncryptionBlockSize
                            );
        }
    else
        {
        // This is correct for FreeOTFE; block IDs start from, and are relative to,
        // the start of the encrypted pratition within the volume file
        // This means that if the critical data block is removed, and the volume file
        // then begins at a 0 byte offset within the volume file, it is still usable
        // Note that this driver has no knowledge of the critical data block - that's
        // all decoded and handled by the userspace application; this driver only
        // dumbly handles encryption/decryption of the encrypted partition - it's 
        // typically supplied a CRITICAL_DATA_LENGTH offset into the file by the 
        // userspace application.
        blockID.QuadPart = (
                            (fileOffset.QuadPart - devContext->DataOffset.QuadPart) /
                                    devContext->EncryptionBlockSize
                            );
        }  // ELSE PART - if (devContext->VolumeFlags & VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART)

    return blockID;
}


// =========================================================================
// =========================================================================
