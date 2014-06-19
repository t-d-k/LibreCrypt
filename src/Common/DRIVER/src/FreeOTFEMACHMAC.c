// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"

#include "FreeOTFEMACHMAC.h"

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"

#ifdef FOTFE_PC_DLL
#include <winnt.h>
#endif

// =========================================================================
// Generate HMAC
NTSTATUS
ImplMACHMAC(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      int tBits,  // In *bits*
    IN      unsigned int KLength,  // In bits
    IN      unsigned char* K,
    IN      unsigned int textLength,  // In bits
    IN      unsigned char* text,

    IN OUT  unsigned int* MACLength,  // In bits
    OUT     unsigned char* MAC
)
{
    NTSTATUS status;

    unsigned int KLengthBytes;  // In *bytes*

    unsigned int B;  // In *bytes*
    unsigned int i;
    unsigned char* Kzero;
    
    unsigned char* step2TmpBuf;
    unsigned char* step4OutBuf;
    unsigned char* step5OutBuf;
    unsigned char* step6OutBuf;
    unsigned char* step7OutBuf;
    unsigned char* step8OutBuf;
    unsigned char* step9OutBuf;

    unsigned int step2TmpBufSize;  // In *bytes*
    unsigned int step5OutBufSize;  // In *bytes*
    unsigned int step6OutBufSize;  // In *bytes*
    unsigned int step8OutBufSize;  // In *bytes*
    unsigned int step9OutBufSize;  // In *bytes*

    unsigned int step6ValidBits;
    unsigned int step9ValidBits;

    unsigned int hashValidBits;
    unsigned int hashUseLenBytes;

    unsigned int outputBits;


    DEBUGOUTMACDRV(DEBUGLEV_ENTER, ("ImplMACHMAC\n"));

    status = STATUS_SUCCESS;

    Kzero = NULL;

    step4OutBuf = NULL;
    step5OutBuf = NULL;
    step6OutBuf = NULL;
    step7OutBuf = NULL;
    step8OutBuf = NULL;
    step9OutBuf = NULL;

    // Sanity checks
    // This should be picked up by the caller; this is belt & braces
    if (
        (HashDetails.BlockSize <= 0) ||
        (HashDetails.Length <= 0)
       )
        {
        DEBUGOUTMACDRV(DEBUGLEV_ERROR, ("HMAC implementation called with a hash that has an undefined blocksize/output\n"));
        status = STATUS_INTERNAL_ERROR;
        }


    B = (HashDetails.BlockSize / 8);
    KLengthBytes = (KLength / 8);


    // Compute the HMAC...

    // ---------
    // FIPS-198 Steps 1-3
    // i.e. Set Kzero to be a "B" length string
    if (NT_SUCCESS(status))
        {
        Kzero = FREEOTFE_MEMALLOC(B);    

        if (KLengthBytes == B)
            {
            // Step 1
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 1: Executing...\n"));
            // Set K0 = K
            FREEOTFE_MEMCPY(
                            Kzero,
                            K,
                            B
                           );
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 2: n/a; skipped\n"));
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 3: n/a; skipped\n"));
            }
        else if (KLengthBytes > B)
            {
            // Step 2
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 1: n/a; skipped\n"));
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 2: Executing...\n"));
            // Hash K to get a hashlength string, then pad out to B bytes
            // with NULLs
            step2TmpBufSize = (HashDetails.Length / 8);   // Divide by 8 to get bytes from bits
            hashValidBits = HashDetails.Length;  // In *bits*
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Allocating tmp buffer (%d bytes)...\n", step2TmpBufSize));
            step2TmpBuf = FREEOTFE_MEMALLOC(step2TmpBufSize);

            status = FnHash(
					        &HashGUID,
					        KLength,
					        K,
					        &hashValidBits,
					        step2TmpBuf
                        );
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMACDRV(DEBUGLEV_ERROR, ("HMAC Step 2: Call to hash driver failed\n"));
                status = STATUS_INTERNAL_ERROR;
                }
            else
                {
                hashUseLenBytes = min(B, (hashValidBits / 8));
                DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Hash output: %d bits (%d bytes)...\n", hashValidBits, (hashValidBits / 8)));
                DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Using first %d bytes...\n", hashUseLenBytes));
                // Copy the hash value to K0
                FREEOTFE_MEMCPY(
                    Kzero,
                    step2TmpBuf,
                    hashUseLenBytes
                    );
                DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Step 2 Kzero: Padding end with NULLs (%d bytes of nulls)...\n", (B - hashUseLenBytes)));
                // Pad K0 with NULLs
                SecZeroMemory(&Kzero[hashUseLenBytes], (B - hashUseLenBytes));
                }            

            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Zeroing and freeing hash buffer\n"));
            SecZeroMemory(step2TmpBuf, step2TmpBufSize);
            FREEOTFE_FREE(step2TmpBuf);
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 3: n/a; skipped\n"));
            }
        else
            {
            // Step 3
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 1: n/a; skipped\n"));
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 2: n/a; skipped\n"));
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 3: Executing...\n"));
            FREEOTFE_MEMCPY(
                Kzero,
                K,
                KLengthBytes
                );
            DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Step 3 Kzero: Padding end with NULLs (%d bytes of nulls)...\n", (B - KLengthBytes)));
            // Pad K0 with NULLs
            SecZeroMemory(&Kzero[KLengthBytes], (B - KLengthBytes));
            }
        }

    // ---------
    // FIPS-198 Step 4
    // XOR K0 with ipad (0x36)
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 4: Executing...\n"));
        step4OutBuf = FREEOTFE_MEMALLOC(B);
        for (i = 0; i < B; i++)
            {
            step4OutBuf[i] = Kzero[i] ^ 0x36;
            }
        }


    // ---------
    // FIPS-198 Step 5
    // Append input text onto the end of the output from step 4
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 5: Executing...\n"));
        step5OutBufSize = (B + (textLength / 8));
        step5OutBuf = FREEOTFE_MEMALLOC(step5OutBufSize);
        FREEOTFE_MEMCPY(
            step5OutBuf,
            step4OutBuf,
            B
            );
        FREEOTFE_MEMCPY(
            &step5OutBuf[B],
            text,
            (textLength / 8)
            );
        }

    // ---------
    // FIPS-198 Step 6
    // Apply H to the output from step 5
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 6: Executing...\n"));
        step6ValidBits = HashDetails.Length;
        step6OutBufSize = (step6ValidBits / 8);
        step6OutBuf = FREEOTFE_MEMALLOC(step6OutBufSize);
        status = FnHash(
					    &HashGUID,
					    (step5OutBufSize * 8),
					    step5OutBuf,
					    &step6ValidBits,
					    step6OutBuf
                    );
        }

    // ---------
    // FIPS-198 Step 7
    // XOR K0 with opad (0x5C)
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 7: Executing...\n"));
        step7OutBuf = FREEOTFE_MEMALLOC(B);
        for (i = 0; i < B; i++)
            {
            step7OutBuf[i] = Kzero[i] ^ 0x5C;
            }
        }

    // ---------
    // FIPS-198 Step 8
    // Append the result from step 6 to step 7
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 8: Executing...\n"));
        step8OutBufSize = (B + (step6ValidBits / 8));
        step8OutBuf = FREEOTFE_MEMALLOC(step8OutBufSize);
        FREEOTFE_MEMCPY(
            step8OutBuf,
            step7OutBuf,
            B
            );
        FREEOTFE_MEMCPY(
            &step8OutBuf[B],
            step6OutBuf,
            (step6ValidBits / 8)
            );
        }

    // ---------
    // FIPS-198 Step 9
    // Apply H to the output from step 8
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 9: Executing...\n"));
        step9ValidBits = HashDetails.Length;
        step9OutBufSize = (step9ValidBits / 8);
        step9OutBuf = FREEOTFE_MEMALLOC(step9OutBufSize);
        status = FnHash(
					    &HashGUID,
					    (step8OutBufSize * 8),
					    step8OutBuf,
					    &step9ValidBits,
					    step9OutBuf
                    );
        }


    // ---------
    // FIPS-198 Step 10
    // Select the leftmost t bytes of output from step 9
    if (NT_SUCCESS(status))
        {
        DEBUGOUTMACDRV(DEBUGLEV_INFO, ("HMAC Step 10: Executing...\n"));
        // Truncate, if necessary, to tBits of data
        if (tBits >= 0)
            {
            outputBits = tBits;
            }
        else
            {
            outputBits = step9ValidBits;
            }

        // Sanity check on the buffer...
        if (*MACLength < outputBits)
            {
            DEBUGOUTMACDRV(DEBUGLEV_ERROR, ("Buffer too small; supplied %d bits, need %d bits\n", MACLength, outputBits));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            // Copy the appropriate number of bits to the output buffer
            FREEOTFE_MEMCPY(
                MAC,
                step9OutBuf,
                (outputBits / 8)  // Convert to bytes
                );
            *MACLength = outputBits;
            }

        }


    // Cleanup...
    DEBUGOUTMACDRV(DEBUGLEV_INFO, ("Freeing off any used buffers...\n"));

    if (step9OutBuf != NULL)
        {
        SecZeroMemory(step9OutBuf, step9OutBufSize);
        FREEOTFE_FREE(step9OutBuf);
        }
    if (step8OutBuf != NULL)
        {
        SecZeroMemory(step8OutBuf, step8OutBufSize);
        FREEOTFE_FREE(step8OutBuf);
        }
    if (step7OutBuf != NULL)
        {
        SecZeroMemory(step7OutBuf, B);
        FREEOTFE_FREE(step7OutBuf);
        }
    if (step6OutBuf != NULL)
        {
        SecZeroMemory(step6OutBuf, step6OutBufSize);
        FREEOTFE_FREE(step6OutBuf);
        }
    if (step5OutBuf != NULL)
        {
        SecZeroMemory(step5OutBuf, step5OutBufSize);
        FREEOTFE_FREE(step5OutBuf);
        }
    if (step4OutBuf != NULL)
        {
        SecZeroMemory(step4OutBuf, B);
        FREEOTFE_FREE(step4OutBuf);
        }
    if (Kzero != NULL)
        {
        SecZeroMemory(Kzero, B);
        FREEOTFE_FREE(Kzero);
        }
    

    DEBUGOUTMACDRV(DEBUGLEV_EXIT, ("ImplMACHMAC\n"));

    return status;
}

// =========================================================================
// =========================================================================


