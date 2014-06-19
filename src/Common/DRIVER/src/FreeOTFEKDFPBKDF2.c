// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEKDFPBKDF2.h"

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFEMACHMAC.h"


// =========================================================================
// Generate PBKDF2 key based on HMAC
NTSTATUS
ImplKDFPBKDF2(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      unsigned int PLength,  // In bits
    IN      unsigned char* P,
    IN      unsigned int SLength,  // In bits
    IN      unsigned char* S,
    IN      unsigned int c,  // Iterations
    IN      int dkLenBits,  // In *bits*

    IN OUT  unsigned int* DerivedKeyLength,  // In bits
    OUT     unsigned char* DerivedKey
)
{
    NTSTATUS status;

    unsigned int i;

    unsigned int l;
//    unsigned int r;  // In *bytes*
    unsigned int dkLen;  // In *bytes*
    unsigned int hLen;  // In *bytes*

    unsigned char* T_;
    unsigned int T_SizeBytes; // In *bytes*


    DEBUGOUTKDFDRV(DEBUGLEV_ENTER, ("ImplKDFPBKDF2\n"));

    status = STATUS_SUCCESS;

    T_ = NULL;
    T_SizeBytes = 0;


    // Sanity check
    // This should be picked up by the caller; this is belt & braces
    if (
        (HashDetails.BlockSize <= 0) ||
        (HashDetails.Length <= 0)
       )
        {
        DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("PBKDF2 implementation called with a hash that has an undefined/zero length/blocksize\n"));
        status = STATUS_INTERNAL_ERROR;
        }
    else
        {
        // This is PBKDF2 based on HMAC - the HMAC function returns the same
        // number of bytes as is in the hash it uses
        hLen = (HashDetails.Length / 8);
        }


    if (dkLenBits < 0)
        {
        dkLenBits = DEFAULT_PBKDF2_KEY_SIZE;
        }
    dkLen = (dkLenBits / 8);
    DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("dkLenBits = %d bits\n", dkLenBits));
    DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("dkLen = %d bytes\n", dkLen));


    // Sanity check
    // This should be picked up by the caller; this is belt & braces
    if ((unsigned int)dkLenBits > *DerivedKeyLength)
        {
        // The output buffer can store the requested number of bits, right?
        // Note that dkLenBits can be -ve to indicate the full key length is to be
        // returned
        DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("Requested number of bits is larger than supplied buffer\n"));
        status = STATUS_BUFFER_TOO_SMALL;
        }


    // Step 1
    // 1. If dkLen > (2^32 - 1) * hLen, output "derived key too long" and
    //    stop.

    DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("PBKDF2 Step 1: n/a; skipped...\n"));
    // (Step skipped - because dkLen is an integer, it can never be more
    // than 2^(31-1) - far less than (2^32 - 1) * hLen)


    // Step 2
    // 2. Let l be the number of hLen-octet blocks in the derived key,
    //    rounding up, and let r be the number of octets in the last
    //    block:
    //
    //              l = CEIL (dkLen / hLen) ,
    //              r = dkLen - (l - 1) * hLen .
    //
    //    Here, CEIL (x) is the "ceiling" function, i.e. the smallest
    //    integer greater than, or equal to, x.
    if (NT_SUCCESS(status))
        {
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("PBKDF2 Step 2: Executing...\n"));
        // Note: div always rounds down towards zero
        l = (dkLen / hLen);
        // Because div always rounds down towards zero, we may need to increment it
        // by one
        if (dkLen > (l * hLen))
            {
            l++;
            }


        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("dkLen = %d bytes\n", dkLen));
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("hLen = %d bytes\n", hLen));
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("l = %d\n", l));
//        r = dkLen - ((l - 1) * hLen);
//        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("r = %d bytes\n", r));
        }




    // Step 3
    // 3. For each block of the derived key apply the function F
    if (NT_SUCCESS(status))
        {
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("PBKDF2 Step 3: Executing...\n"));
        T_SizeBytes = (hLen * l);
        T_ = FREEOTFE_MEMALLOC(T_SizeBytes);

        for(i = 1; i<=l; i++)
            {
            DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("Loop: i = %d\n", i));
            if (!(NT_SUCCESS(PBKDF2_F(
                                    FnHash,
                                    HashGUID,
                                    HashDetails,

                                    PLength,
                                    P,
                                    SLength,
                                    S,
                                    c,
                                    i,

                                    // This is correct (-1), as our loop starts
                                    // from 1
                                    &T_[((i-1) * hLen)]
                                ))))
                {
                DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("Call to PBKDF2 function \"F\" failed.\n"));
                status = STATUS_INTERNAL_ERROR;
                break;
                }
            }
        }



    // Step 4
    // 4. Concatenate the blocks and extract the first dkLen octets to
    //    produce a derived key DK:
    //
    //              DK = T_1 || T_2 ||  ...  || T_l<0..r-1>
    if (NT_SUCCESS(status))
        {
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("PBKDF2 Step 4: Executing...\n"));
        FREEOTFE_MEMCPY(
                      DerivedKey,
                      T_,
                      (dkLenBits / 8)
                     );
        }


    // Step 5
    // 5. Output the derived key DK.
    if (NT_SUCCESS(status))
        {
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("PBKDF2 Step 5: Executing...\n"));
        *DerivedKeyLength = dkLenBits;
        }


    // Cleanup...
    DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("Freeing off any used buffers...\n"));

    if (T_ != NULL)
        {
        SecZeroMemory(T_, T_SizeBytes);
        FREEOTFE_FREE(T_);
        }

    DEBUGOUTKDFDRV(DEBUGLEV_EXIT, ("ImplKDFPBKDF2\n"));

    return status;
}


// =========================================================================
// This is the PBKDF2 PRF function "F"  
// The PRF used is HMAC
NTSTATUS
PBKDF2_F(
    IN      PDataHashFn FnHash,
    IN      GUID HashGUID,
    IN      HASH HashDetails,
    IN      unsigned int PLength,  // In bits
    IN      unsigned char* P,
    IN      unsigned int SLength,  // In bits
    IN      unsigned char* S,
    IN      unsigned int c,
    IN      unsigned int i,
    OUT     unsigned char* T_
)
{
    NTSTATUS status;
    unsigned char* U_;
    unsigned char* tmpU_Buffer;
    unsigned char* saltAndCount;
    unsigned int hLen;  // In *bytes*
    unsigned int hLenBits;  // In *Bits*
    unsigned int actualHLenBits;  // In *bits*
    unsigned int saltAndCountSizeBytes;  // In *bytes*
    unsigned int j, k;


    DEBUGOUTKDFDRV(DEBUGLEV_ENTER, ("PBKDF2_F\n"));


    status = STATUS_SUCCESS;

    U_ = NULL;
    tmpU_Buffer = NULL;
    saltAndCount = NULL;


    // From PKCS#5:

    // function F is defined as the exclusive-or sum of the
    // first c iterates of the underlying pseudorandom function PRF
    // applied to the password P and the concatenation of the salt S
    // and the block index i:
    //
    //           F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c
    //
    // where
    //           U_1 = PRF (P, S || INT (i)) ,
    //           U_2 = PRF (P, U_1) ,
    //           ...
    //           U_c = PRF (P, U_{c-1}) .
    //
    // Here, INT (i) is a four-octet encoding of the integer i, most
    // significant octet first.


    DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("Iterations requested: %d\n", c));

    // This is PBKDF2 based on HMAC - the HMAC function returns the same
    // number of bytes as is in the hash it uses
    hLen = (HashDetails.Length / 8);
    hLenBits = HashDetails.Length;

    // Convert bits to bytes, and add 4 (specified in PKCS #5) for the "i"
    saltAndCountSizeBytes = ((SLength / 8) + 4);
    saltAndCount = FREEOTFE_MEMALLOC(saltAndCountSizeBytes);
    FREEOTFE_MEMCPY(
                    saltAndCount,
                    S,
                    (SLength / 8)
                    );
    // Concatenate the salt
    // Note: *Bitwise* AND :)
    saltAndCount[ (SLength / 8)   ] = ((i & 0xFF000000) / 0x01000000);
    saltAndCount[((SLength / 8)+1)] = ((i & 0x00FF0000) / 0x00010000);
    saltAndCount[((SLength / 8)+2)] = ((i & 0x0000FF00) / 0x00000100);
    saltAndCount[((SLength / 8)+3)] = ((i & 0x000000FF) / 0x00000001);

    U_ = FREEOTFE_MEMALLOC(hLen);
    tmpU_Buffer = FREEOTFE_MEMALLOC(hLen);

    // Process U_1
    if (NT_SUCCESS(status))
        {
        actualHLenBits = hLenBits;
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("First HMAC: SLength = %d\n", SLength));
        DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("First HMAC: actualHLenBits = %d\n", actualHLenBits));
        if (!(NT_SUCCESS(ImplMACHMAC(
                                    FnHash,
                                    HashGUID,
                                    HashDetails,
                                    -1,  // Retrieve full HMAC
                                    PLength,
                                    P,
                                    (saltAndCountSizeBytes * 8), // In *bits*
                                    saltAndCount,

                                    &actualHLenBits,  // In bits
                                    U_
                                ))))
            {
            DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("First call to HMAC function \"F\" failed.\n"));
            status = STATUS_INTERNAL_ERROR;
            }
        else
            {
            // Sanity check
            if (actualHLenBits != hLenBits)
                {
                DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("HMAC function didn't return expected number of bits?!\n"));
                status = STATUS_INTERNAL_ERROR;
                }
            else
                {
                // Copy to output buffer - note that this will be XORd if
                // there's more c>1
                FREEOTFE_MEMCPY(
                                T_,
                                U_,
                                (actualHLenBits / 8)
                                );
                }
            }
        }


    // Process subsequent U_n
    if (NT_SUCCESS(status))
        {
        for(j = 2; j <= c; j++)
            {
            actualHLenBits = hLenBits;
            DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("Subsequent HMAC: actualHLenBits = %d\n", actualHLenBits));
            if (!(NT_SUCCESS(ImplMACHMAC(
                                        FnHash,
                                        HashGUID,
                                        HashDetails,
                                        -1,  // Retrieve full HMAC
                                        PLength,
                                        P,
                                        hLenBits,  // In bits
                                        U_,

                                        &actualHLenBits,  // In bits
                                        tmpU_Buffer
                                    ))))
                {
                DEBUGOUTKDFDRV(DEBUGLEV_ERROR, ("Call to HMAC function \"F\" failed.\n"));
                status = STATUS_INTERNAL_ERROR;
                break;
                }
            else
                {
                // Move from temp buffer to U_ buffer
                DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("Copying (%d)\n", hLen));
                FREEOTFE_MEMCPY(
                            U_,
                            tmpU_Buffer,
                            hLen
                            );

                // XOR with previous iteration
                DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("XORing...\n"));
                for(k = 0; k < hLen; k++)
                    {
                    T_[k] ^= U_[k];
                    }
                DEBUGOUTKDFDRV(DEBUGLEV_INFO, ("OK.\n"));
                }
            }
        }


    // Note: No need to set the output "T_" explicitly, as this is done during
    //       processing


    if (tmpU_Buffer != NULL)
        {
        SecZeroMemory(tmpU_Buffer, hLen);
        FREEOTFE_FREE(tmpU_Buffer);
        }
    if (U_ != NULL)
        {
        SecZeroMemory(U_, hLen);
        FREEOTFE_FREE(U_);
        }
    if (saltAndCount != NULL)
        {
        SecZeroMemory(saltAndCount, saltAndCountSizeBytes);
        FREEOTFE_FREE(saltAndCount);
        }



    DEBUGOUTKDFDRV(DEBUGLEV_EXIT, ("PBKDF2_F\n"));

    return status;
}


// =========================================================================
// =========================================================================


