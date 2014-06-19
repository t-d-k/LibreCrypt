// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFECallModuleFn.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEGenerateBlockIV.h"
#include "FreeOTFEBlockCalc.h"


// =========================================================================
// =========================================================================
// Call the relevant encryption function
NTSTATUS
BlockEncrypt(
    IN DEVICE_CONTEXT* DeviceContext,
    IN LARGE_INTEGER fileOffset,  // The offset within the *volume file* where the
                                  // data was obtained from
    IN FREEOTFEBYTE* plaintextBuffer,
    IN FREEOTFEBYTE* cyphertextBuffer
)
{
    NTSTATUS status = STATUS_SUCCESS;
    char blockIV[FREEOTFE_MAX_CYPHER_BLOCKSIZE / 8];
    unsigned int blockIVLength;  // In *bits*
    LARGE_INTEGER sectorID;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("BlockEncrypt\n")));

    // Sanity check
    if (
        (DeviceContext->MainCypher.FnEncryptSector != NULL) ||
        (DeviceContext->MainCypher.FnEncrypt != NULL)
       )
        {
        // If the blocksize isn't defined, we can't use an IV
        if (DeviceContext->MainCypher.Details.BlockSize > 0) 
            {
            // Note: The block IV length is equal to the block length        
            blockIVLength = DeviceContext->MainCypher.Details.BlockSize;
            status = GenerateBlockIV(
                                    DeviceContext,
                                    fileOffset,
                                    blockIVLength,  // In bits
                                    (unsigned char*)&blockIV
                                    );
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to generate block IV\n")));
                return status;
                }
            }
        else  // ELSE PART - if (DeviceContext->CypherDetails.BlockSize > 0) 
            {
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Cypher block size is 0; no IV\n")));
            blockIVLength = 0;
            }

        if (NT_SUCCESS(status))
            {
            if (DeviceContext->MainCypher.FnEncryptSector != NULL)
                {
                sectorID = GetBlockID(DeviceContext, fileOffset);

                status = DeviceContext->MainCypher.FnEncryptSector(
                                       &DeviceContext->MainCypher.CypherGUID,
                                       sectorID,
                                       DeviceContext->EncryptionBlockSize,
                                       DeviceContext->MasterKeyLength,
                                       (char*)DeviceContext->MasterKey,
                                       (char*)DeviceContext->MasterKeyASCII,                          
                                       blockIVLength,
                                       (char*)&blockIV,
                                       DeviceContext->EncryptionBlockSize,
                                       plaintextBuffer,
                                       cyphertextBuffer
                                      );
                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FnEncryptSector call failed\n")));
                    }
                }
            else if (DeviceContext->MainCypher.FnEncrypt != NULL)
                {
                status = DeviceContext->MainCypher.FnEncrypt(
                                   &DeviceContext->MainCypher.CypherGUID,
                                       DeviceContext->MasterKeyLength,
                                       (char*)DeviceContext->MasterKey,
                                       (char*)DeviceContext->MasterKeyASCII,                          
                                       blockIVLength,
                                       (char*)&blockIV,
                                       DeviceContext->EncryptionBlockSize,
                                       plaintextBuffer,
                                       cyphertextBuffer
                                      );
                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FnEncrypt call failed\n")));
                    }
                }
            }
        }
    else  // ELSE PART - if no function that we can call
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("No encryption function...\n")));
        status = STATUS_INVALID_PARAMETER;
        }


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("BlockEncrypt\n")));
    return status;
}


// =========================================================================
// Call the relevant decryption function
NTSTATUS
BlockDecrypt(
    IN DEVICE_CONTEXT* DeviceContext,
    IN LARGE_INTEGER fileOffset,  // The offset within the *volume file* where the
                                  // data was obtained from
    IN FREEOTFEBYTE* cyphertextBuffer,
    IN FREEOTFEBYTE* plaintextBuffer
)
{
    NTSTATUS status = STATUS_SUCCESS;
    char blockIV[FREEOTFE_MAX_CYPHER_BLOCKSIZE / 8];
    unsigned int blockIVLength;  // In *bits*
    LARGE_INTEGER sectorID;

    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_ENTER, (TEXT("BlockDecrypt\n")));

    // Sanity check
    if (
        (DeviceContext->MainCypher.FnDecryptSector != NULL) ||
        (DeviceContext->MainCypher.FnDecrypt != NULL)
       )
        {
        // If the blocksize isn't defined, we can't use an IV
        if (DeviceContext->MainCypher.Details.BlockSize > 0) 
            {
            // Note: The block IV length is equal to the block length
            blockIVLength = DeviceContext->MainCypher.Details.BlockSize;
            status = GenerateBlockIV(
                                    DeviceContext,
                                    fileOffset,
                                    blockIVLength,  // In bits
                                    (unsigned char*)&blockIV
                                    );
            if (!(NT_SUCCESS(status)))
                {
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to generate block IV\n")));
                return status;
                }
            }
        else  // ELSE PART - if (DeviceContext->CypherDetails.BlockSize > 0) 
            {
            DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("Cypher block size is 0; no IV\n")));
            blockIVLength = 0;
            }

        if (NT_SUCCESS(status))
            {
            if (DeviceContext->MainCypher.FnDecryptSector != NULL)
                {
                sectorID = GetBlockID(DeviceContext, fileOffset);

                status = DeviceContext->MainCypher.FnDecryptSector(
                                       &DeviceContext->MainCypher.CypherGUID,
                                       sectorID,
                                       DeviceContext->EncryptionBlockSize,
                                       DeviceContext->MasterKeyLength,
                                       (char*)DeviceContext->MasterKey,
                                       (char*)DeviceContext->MasterKeyASCII,                          
                                       blockIVLength,
                                       (char*)&blockIV,
                                       DeviceContext->EncryptionBlockSize,
                                       cyphertextBuffer,
                                       plaintextBuffer
                                      );
                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FnDecryptSector call failed\n")));
                    }
                }
            else if (DeviceContext->MainCypher.FnDecrypt != NULL)
                {
                status = DeviceContext->MainCypher.FnDecrypt(
                                       &DeviceContext->MainCypher.CypherGUID,
                                       DeviceContext->MasterKeyLength,
                                       (char*)DeviceContext->MasterKey,
                                       (char*)DeviceContext->MasterKeyASCII,                          
                                       blockIVLength,
                                       (char*)&blockIV,
                                       DeviceContext->EncryptionBlockSize,
                                       cyphertextBuffer,
                                       plaintextBuffer
                                      );
                if (!(NT_SUCCESS(status)))
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FnDecrypt call failed\n")));
                    }
                }
            }
        }
    else  // ELSE PART - if no function that we can call
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("No decryption function...\n")));
        status = STATUS_INVALID_PARAMETER;
        }


    DEBUGOUTMAINDRV(DEBUGLEV_VERBOSE_EXIT, (TEXT("BlockDecrypt\n")));
    return status;
}


// =========================================================================
// Call the relevant hash function
NTSTATUS
DataHash(
    IN      MODULE_DETAILS_HASH* hashModule,
    IN      unsigned int dataLength,  // In bits
    IN      FREEOTFEBYTE* dataBuffer,
    IN OUT  unsigned int* hashLength,  // In bits
    IN      FREEOTFEBYTE* hashBuffer    
)
{
    NTSTATUS retval;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("DataHash\n")));

    retval = hashModule->FnHash(
                                &(hashModule->HashGUID),
                                dataLength,
                                dataBuffer,
                                hashLength,
                                hashBuffer
                               );

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("DataHash\n")));
    return retval;
}


// =========================================================================
// =========================================================================
