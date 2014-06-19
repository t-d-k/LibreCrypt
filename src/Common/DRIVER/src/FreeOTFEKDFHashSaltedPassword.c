// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEKDFHashSaltedPassword.h"

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEPlatform.h"


// =========================================================================
// Generate a key based on the hash of the salted password
NTSTATUS
ImplKDFHashSaltedPassword(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      unsigned int Iterations,
    IN      unsigned int PasswordLength,  // In bits
    IN      unsigned char* Password,
    IN      unsigned int SaltLength,  // In bits
    IN      unsigned char* Salt,

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     unsigned char* DerivedKey
)
{
    NTSTATUS status;
    unsigned char* tmpBuffer;
    unsigned int tmpBufferSizeBytes;
    unsigned char* tmpIterateBuffer;
    unsigned int tmpIterateBufferSizeBytes;
    unsigned int i;

    DEBUGOUTKDFDRV(DEBUGLEV_ENTER, ("ImplKDFHashSaltedPassword\n"));


    tmpBufferSizeBytes = ((PasswordLength + SaltLength) / 8);
    tmpBuffer = FREEOTFE_MEMALLOC(tmpBufferSizeBytes);    

    // Add the salt onto the end of the password before hashing the combined
    // password/salt
    FREEOTFE_MEMCPY(
        tmpBuffer,
        Password,
        (PasswordLength / 8)
        );
    FREEOTFE_MEMCPY(
        &tmpBuffer[(PasswordLength / 8)],
        Salt,
        (SaltLength / 8)
        );

    status = FnHash(
					&HashGUID,
					(tmpBufferSizeBytes * 8),  // Get bits from bytes
					tmpBuffer,
					DerivedKeyLength,
					DerivedKey
                   );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("First KDF call to hash driver failed\n"));
        }
    else
        {
        // Iterate, if needed...
        if (Iterations >= 2)
            {
            tmpIterateBufferSizeBytes = (*DerivedKeyLength / 8);
            tmpIterateBuffer = FREEOTFE_MEMALLOC(tmpIterateBufferSizeBytes);
            for (i = 2; i <= Iterations; i++)
                {
                // Copy to temp buffer...
                FREEOTFE_MEMCPY(
                    tmpIterateBuffer,
                    DerivedKey,
                    tmpIterateBufferSizeBytes
                    );

                // Only pass a buffer that we can copy back into
                // Note: This value should never be bigger than the original
                //       value passed in, otherwise the original hash would
                //       fail
                *DerivedKeyLength = (tmpIterateBufferSizeBytes * 8);

                // Hash temp buffer...
                status = FnHash(
					            &HashGUID,
					            (tmpIterateBufferSizeBytes * 8),  // Get bits from bytes
					            tmpIterateBuffer,
					            DerivedKeyLength,
					            DerivedKey
                            );

                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("Subsequent KDF call to hash driver failed\n"));
                    break;
                    }

                }

            SecZeroMemory(tmpIterateBuffer, tmpIterateBufferSizeBytes);
            FREEOTFE_FREE(tmpIterateBuffer);
            }
        }



    // Note: No need to explicitly set *MACLength as FnHash does this for us

    SecZeroMemory(tmpBuffer, tmpBufferSizeBytes);
    FREEOTFE_FREE(tmpBuffer);


    DEBUGOUTKDFDRV(DEBUGLEV_EXIT, ("ImplKDFHashSaltedPassword\n"));

    return status;
}

// =========================================================================
// =========================================================================


