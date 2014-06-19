// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEMACHash.h"

#include "FreeOTFEDebug.h"


// =========================================================================
// Generate a MAC based on hashing
NTSTATUS
ImplMACHash(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      unsigned int DataLength,  // In bits
    IN      unsigned char* Data,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     unsigned char* MAC
)
{
    NTSTATUS status;

    DEBUGOUTMACDRV(DEBUGLEV_ENTER, ("ImplMACHash\n"));

    status = FnHash(
					&HashGUID,
					DataLength,
					Data,
					MACLength,
					MAC
                   );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMACDRV(DEBUGLEV_ERROR, ("MAC call to hash driver failed\n"));
        }

    // Note: No need to explicitly set *MACLength as FnHash does this for us

    DEBUGOUTMACDRV(DEBUGLEV_EXIT, ("ImplMACHash\n"));

    return status;
}

// =========================================================================
// =========================================================================


