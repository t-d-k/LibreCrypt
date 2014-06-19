// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DLL
#include <windows.h>
#endif

#include "FreeOTFEGenerateBlockIV.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFECallModuleFn.h"
#include "FreeOTFEBlockCalc.h"


// =========================================================================
// Generate block IV: Sector ID
NTSTATUS
GenerateBlockIV_SectorID(
    IN      LARGE_INTEGER blockID,
    IN      unsigned int blockIDBits,  // The number of bits in blockID to be used
                                       // Set to either 32 or 64 bits
    IN      unsigned int blockIVLength,  // The size of the required IV (and length of
	                                     // "blockIV"), in bits
    OUT     unsigned char* blockIV  // Pointer to where the IV should be written
                                    // Note: Caller must pre-initialise to 0x00s
)
{
    NTSTATUS status;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("GenerateBlockIV_SectorID\n")));

    status = STATUS_SUCCESS;

    // Sanity check...
    if (blockIVLength < blockIDBits)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("blockIDBits must be >= blockIVLength (blockIDBits: %d; blockIVLength: %d)\n"),
                                         blockIDBits, blockIVLength));
        status = STATUS_INTERNAL_ERROR; 
        }
    else
        {
        // Generate the IV...
        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("%d bit block IDs being used\n"), blockIDBits));
        if (blockIDBits == 32)
            {
  		    // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
		    *(ULONG*)blockIV                 = blockID.LowPart;
            }
        else if (blockIDBits == 64)
            {
  		    // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
		    *(ULONG*)blockIV                 = blockID.LowPart;
		    // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
		    // The IV is offset by sizeof(ULONG), since the IV is in unsigned chars,
		    // before it is cast
		    *(ULONG*)(blockIV+sizeof(ULONG)) = blockID.HighPart;
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("blockIDBits must be set to either 32 or 64 (set to: %d)\n"), blockIDBits));
            status = STATUS_INTERNAL_ERROR; 
            }

        }


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("GenerateBlockIV_SectorID\n")));

    return status;
}


// =========================================================================
// Generate block IV: Hashed block ID
NTSTATUS
GenerateBlockIV_HashedSectorID(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER blockID,
    IN      unsigned int blockIDBits,  // The number of bits in blockID to be used
                                       // Set to either 32 or 64 bits
    IN      unsigned int blockIVLength,  // The size of the required IV (and length of
	                                     // "blockIV"), in bits
    OUT     unsigned char* blockIV  // Pointer to where the IV should be written
                                    // Note: Caller must pre-initialise to 0x00s
)
{
    NTSTATUS status;
    unsigned char hashOutput[FREEOTFE_MAX_HASH_LENGTH / 8];  // Divide by 8 to get bytes from bits
    unsigned int hashLength;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("GenerateBlockIV_HashedSectorID\n")));

    status = STATUS_SUCCESS;

    // Generate the IV...
    hashLength = sizeof(hashOutput) * 8;  // Multiply by 8 to get size of buffer in *bits*

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("%d bit block IDs being used\n"), blockIDBits));
    if (blockIDBits == 32)
        {
        // Only use the LSB 32 bits        
        status = DataHash(
                          &(devContext->IVHash),
                          blockIDBits,  // Size in bits
                          (FREEOTFEBYTE*)&(blockID.LowPart),
                          &hashLength,  // Size in bits
                          (FREEOTFEBYTE*)&hashOutput
                         );
        }
    else if (blockIDBits == 64)
        {
        // Use the full 64-bit block ID
        status = DataHash(
                          &(devContext->IVHash),
                          blockIDBits,  // Size in bits
                          (FREEOTFEBYTE*)&blockID,
                          &hashLength,  // Size in bits
                          (FREEOTFEBYTE*)&hashOutput
                         );
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("blockIDBits must be set to either 32 or 64 (set to: %d)\n"), blockIDBits));
        status = STATUS_INTERNAL_ERROR; 
        }


    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Failed to hash block ID to form IV.\n")));
        }
    else
        {
        // Copy the first 'n' bits of the hash to be the block IV
        FREEOTFE_MEMCPY(            
                      blockIV,
                      &hashOutput,
                      ((min(blockIVLength, hashLength)) / 8)  // Divide by 8 to get bytes from bits
                      );
        }


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("GenerateBlockIV_HashedSectorID\n")));

    return status;
}


// =========================================================================
// Generate block IV: ESS_IV
// From Linux's dm_crypt kernel source (drivers/md/dm-crypt.c):
//  * ess_iv: "encrypted sector|salt initial vector", the sector number is
//  *         encrypted with the bulk cipher using a salt as key. The salt
//  *         should be derived from the bulk cipher's key via hashing.
// i.e.:
//   Let salt = malloc(IV length) bits of memory (This is done when first mounting)
//   Set salt to be hash(key) (This is done when first mounting)
//   Encrypt the n-bit block ID using the above salt as the key
//     - Note: Linux's dm-crypt volumes use 64 bit block IDs
//   Use the result as the IV
NTSTATUS
GenerateBlockIV_ESSIV(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER blockID,
    IN      unsigned int blockIDBits,  // The number of bits in blockID to be used
                                        // Set to either 32 or 64 bits
    IN      unsigned int blockIVLength,  // The size of the required IV (and length of
	                                      // "blockIV"), in bits
    OUT     unsigned char* blockIV  // Pointer to where the IV should be written
                                     // Note: Caller must pre-initialise to 0x00s
)
{
    NTSTATUS status;
    unsigned char cypherInput[FREEOTFE_MAX_CYPHER_BLOCKSIZE / 8];  // Divide by 8 to get bytes from bits
    unsigned char cypherOutput[FREEOTFE_MAX_CYPHER_BLOCKSIZE / 8];  // Divide by 8 to get bytes from bits
    unsigned int encryptLengthBytes;
    unsigned int useBitsFromHash;


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("GenerateBlockIV_ESSIV\n")));

    status = STATUS_SUCCESS;

    // Generate the IV...
    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("%d bit block IDs being used\n"), blockIDBits));
    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Block ID is: %lld\n"), blockID.QuadPart));
    FREEOTFE_MEMZERO(&cypherInput, sizeof(cypherInput));
    if (blockIDBits == 32)
        {
        // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
        *(ULONG*)cypherInput                 = blockID.LowPart;
        }
    else if (blockIDBits == 64)
        {
        // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
        *(ULONG*)cypherInput                 = blockID.LowPart;
        // Cast the IV (unsigned cast) as a ptr to a ULONG, then dereference to set
        // The IV is offset by sizeof(ULONG), since the IV is in unsigned chars,
        // before it is cast
        *(ULONG*)(cypherInput+sizeof(ULONG)) = blockID.HighPart;
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("blockIDBits must be set to either 32 or 64 (set to: %d)\n"), blockIDBits));
        status = STATUS_INTERNAL_ERROR; 
        }

    encryptLengthBytes = (devContext->IVCypher.Details.BlockSize / 8);
    // Allow for IV cyphers with a variable length blocksize
    if (devContext->IVCypher.Details.BlockSize < 0) 
        {
        encryptLengthBytes = (blockIDBits / 8);
        }

    if (NT_SUCCESS(status))
        {
        // Note: We use a NULL IV for this encryption

        if (devContext->IVCypher.FnEncryptSector != NULL) 
            {
		    status = devContext->IVCypher.FnEncryptSector(
                                                    &devContext->IVCypher.CypherGUID,
                                                    blockID,
                                                    devContext->EncryptionBlockSize,
                                                    devContext->ESSIVKeyLength,
                                                    (char*)devContext->ESSIVKey,
                                                    (char*)devContext->ESSIVKeyASCII,
                                                    0,
                                                    NULL,
                                                    encryptLengthBytes,
                                                    (char*)&cypherInput,
                                                    (char*)&cypherOutput
                                                   );
            }
        else if (devContext->IVCypher.FnEncrypt != NULL) 
            {
		    status = devContext->IVCypher.FnEncrypt(
                                                    &devContext->IVCypher.CypherGUID,
                                                    devContext->ESSIVKeyLength,
                                                    (char*)devContext->ESSIVKey,
                                                    (char*)devContext->ESSIVKeyASCII,
                                                    0,
                                                    NULL,
                                                    encryptLengthBytes,
                                                    (char*)&cypherInput,
                                                    (char*)&cypherOutput
                                                   );
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("No encryption function?!\n")));
            status = STATUS_INVALID_PARAMETER;
            }

        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("ESSIV encrypt failed\n")));
            }

        }


    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Failed to ESSIV to form IV.\n")));
        }
    else
        {
        // The number of useful bits we can take from the hash
        useBitsFromHash = (min(blockIVLength, (encryptLengthBytes * 8)));
        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Number of useful bits from cypher: %d\n"), useBitsFromHash));

        // If the block IV is larger than the number of bits the hash returns, ensure that
        // the remainder of the block IV is blanked
        if (useBitsFromHash < blockIVLength)
            {
            FREEOTFE_MEMZERO(blockIV, (blockIVLength / 8));
            }
                                                
        // Copy the first 'n' bits of the hash to be the block IV
        FREEOTFE_MEMCPY(            
                      blockIV,
                      &cypherOutput,
                      (useBitsFromHash / 8)  // Divide by 8 to get bytes from bits
                      );
        }

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("GenerateblockIV_ESSIV\n")));

    return status;
}


// =========================================================================
// Generate block IV
NTSTATUS
GenerateBlockIV(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER fileOffset,  // The offset within the *volume file* where
                                       // the data was obtained from
    IN      unsigned int blockIVLength,  // The size of the required IV (and length of
	                                 // "sectorIV"), in bits
    OUT     unsigned char* blockIV
)
{
    NTSTATUS status;
    LARGE_INTEGER blockID;
    unsigned int useBytes;
    unsigned int i;
    unsigned int j;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("GenerateBlockIV\n")));

    status = STATUS_SUCCESS;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Block IV length is: %d\n"), blockIVLength));
    // Belt 'n braces - we should never be called if there isn't a fixed blocksize...
    if (blockIVLength <= 0)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("IV length requested is <= 0\n")));
        status = STATUS_INVALID_PARAMETER;
        }
    else
        {
        // Generate the IV...
        // Clear down the IV so we have a NULL IV/baseline IV to work from...
        FREEOTFE_MEMZERO(
                         blockIV,
                         (blockIVLength / 8)
                        );

        blockID = GetBlockID(devContext, fileOffset);

        DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Block IV generation method: %d\n"), devContext->SectorIVGenMethod));
        switch (devContext->SectorIVGenMethod)
            {
            case SCTRIVGEN_NONE:
                {
                // Do nothing - sector IV already zero'd
                break;
                }

            case SCTRIVGEN_32BIT_SECTOR_ID:
                {
                status = GenerateBlockIV_SectorID(
                            blockID,
                            32,
                            blockIVLength,
                            blockIV
                            );
                break;
                }

            case SCTRIVGEN_64BIT_SECTOR_ID:
                {
                status = GenerateBlockIV_SectorID(
                            blockID,
                            64,
                            blockIVLength,
                            blockIV
                            );
                break;
                }

            case SCTRIVGEN_HASH_32BIT_SECTOR_ID:
                {
                status = GenerateBlockIV_HashedSectorID(
                            devContext,
                            blockID,
                            32,
                            blockIVLength,
                            blockIV
                            );
                break;
                }

            case SCTRIVGEN_HASH_64BIT_SECTOR_ID:
                {
                status = GenerateBlockIV_HashedSectorID(
                            devContext,
                            blockID,
                            64,
                            blockIVLength,
                            blockIV
                            );
                break;
                }

            case SCTRIVGEN_ESSIV:
                {
                status = GenerateBlockIV_ESSIV(
                            devContext,
                            blockID,
                            64,
                            blockIVLength,
                            blockIV
                            );
                break;
                }

            default:
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unknown block IV generation method: %d\n"), devContext->SectorIVGenMethod));
                status = STATUS_INVALID_PARAMETER;;
                }

            }


        // If we have a per-volume IV, XOR it with IV...
	if (NT_SUCCESS(status))
            {
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Block ID: %lld bits\n"), blockID.QuadPart));
            for(j = 0; j < (blockIVLength / 8); j++)
                { 
                DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Block IV [%.2d]: %.2x\n"), j, blockIV[j]));
                }      

            useBytes = (min(blockIVLength, devContext->VolumeIVLength) / 8);
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("XORing block IV with %d bits of volume IV...\n"), (useBytes * 8)));
            for(i = 0; i < useBytes; i++)
                {
                blockIV[i] ^= devContext->VolumeIV[i];
                }

            if (useBytes > 0)
                {
                for(j = 0; j < (blockIVLength / 8); j++)
                    { 
                    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Actual block IV [%.2d]: %.2x\n"), j, blockIV[j]));
                    }      
                }

            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("IV generation failed (status: %d)\n"), status));            
            }

        }  // ELSE PART - if (blockIVLength <= 0)


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("GenerateBlockIV\n")));

    return status;
}


// =========================================================================
// =========================================================================
