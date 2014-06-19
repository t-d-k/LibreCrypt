// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "LUKSAFS.h"
#include "SDUEndianIntegers.h"
#include "DriverInterfaceHash.h"
#include "FreeOTFElib.h"

#include <stdlib.h>  // Required for random(...)

// =========================================================================
// Forward declarations...

// Note: *CALLER* is responsible for freeing off returned data
BYTE* LUKSAFS_GetRandom(int len);

BOOL LUKSAFS_RepeatedHash(
    WCHAR* hashKernelModeDeviceName,
    HASH* hashDetails,

    int BufferLength,  // Size of "input" and "output" buffers
    BYTE* input,
    BYTE* output
); 

BOOL LUKSAFS_Process(
    BYTE* stripeData,
    int totalBlockCount,
    int blockLength,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    BYTE* output  // This buffer MUST BE at least "blockLength" long
);

// IMPORTANT: blockNumber indexes from ***1***, not 0
BOOL LUKSAFS_GetBlock(
    BYTE* stripeData,
    int totalBlockCount,
    int blockLength,
    int blockNumber,
    BYTE* output  // The buffer passed in here must be at *least* blockLength bytes long
);



// ----------------------------------------------------------------------------
// Note: *CALLER* is responsible for freeing off returned data
BYTE* LUKSAFS_GetRandom(int len)
{
    BYTE* retval;
    int i;

    retval = malloc(len);
    if (retval != NULL)
        {
        for (i = 0; i < len; i++)
            {
            // xxx -       retval[i] = random(); - lplp - need to restrict random(...) to returning values 0-255
            retval[i] = 0;
            }
        }

    return retval;
}


// ----------------------------------------------------------------------------
// Hash input repeatedly until a string with the same length is produced
BOOL LUKSAFS_RepeatedHash(
    WCHAR* hashKernelModeDeviceName,
    HASH* hashDetails,

    int BufferLength,  // Size of "input" and "output" buffers
    BYTE* input,
    BYTE* output
)
{
    BOOL allOK;
    BYTE* hashOutput;
    int digestSizeBytes;
    int inputDivDigestBytes;
    int inputModDigestBytes;
    int i;
    DWORD tmpDWORD; 
    int tmpStringLength;
//    HASH hashDetails;
    BYTE* tmpString;
    SDU_BIG_ENDIAN_32 tmpBE;
    unsigned int hashOutputLength;


    // This memset *should* be redundant - only included to make debugging easier
    memset(output, 0, BufferLength);

    digestSizeBytes = 0;
    inputDivDigestBytes = 0;
    inputModDigestBytes = 0;

    tmpString = NULL;
    hashOutput = NULL;

    allOK = TRUE;

//    allOK = driver_HashGetImplDetails(
//                                    hashKernelModeDeviceName,
//                                    &hashGUID,
//                                    &hashDetails
//                                    );

    if (allOK) 
        {    
        // Hash output
        //digestSizeBytes = (hashDetails.Length / 8);
        digestSizeBytes = (hashDetails->Length / 8);

        inputDivDigestBytes = (BufferLength / digestSizeBytes);
        inputModDigestBytes = (BufferLength % digestSizeBytes);

        tmpStringLength = digestSizeBytes + sizeof(tmpBE);
        tmpString = malloc(tmpStringLength);
        hashOutput = malloc(digestSizeBytes);
        allOK = (
                 (tmpString != NULL) &&
                 (hashOutput != NULL)
                ); 
        }

    if (allOK)
        {
        for (i = 0; i <= (inputDivDigestBytes-1); i++)
            {
            tmpDWORD = i;
            SDUDWORDToBigEndian32(tmpDWORD, &tmpBE);
            memcpy(tmpString, &tmpBE, sizeof(tmpBE));
            memcpy(&(tmpString[sizeof(tmpBE)]), &(input[i * digestSizeBytes]), digestSizeBytes);

            hashOutputLength = (digestSizeBytes * 8);  // In *bits*
            allOK = driver_HashDataCached(
                                    hashKernelModeDeviceName,
                                    hashDetails->HashGUID,
                                    (tmpStringLength * 8),  // In *bits*
                                    tmpString,
                                    &hashOutputLength,  // In *bits*
                                    hashOutput
                                   );
            if (!(allOK)) 
                {
                break;
                }

            memcpy(&(output[i*digestSizeBytes]), hashOutput, digestSizeBytes);
            }  // for i:=0 to (inputDivDigestBytes-1) do

        }

    if (allOK) 
        {
        if (inputModDigestBytes > 0) 
            {
            tmpDWORD = inputDivDigestBytes;
            SDUDWORDToBigEndian32(tmpDWORD, &tmpBE);
            memcpy(tmpString, &tmpBE, sizeof(tmpBE));
            memcpy(&(tmpString[sizeof(tmpBE)]), &(input[i * digestSizeBytes]), inputModDigestBytes);

            hashOutputLength = (digestSizeBytes * 8);  // In *bits*
            allOK = driver_HashDataCached(
                                    hashKernelModeDeviceName,
                                    hashDetails->HashGUID,
                                    ((inputModDigestBytes + sizeof(tmpBE)) * 8),  // In *bits*
                                    tmpString,
                                    &hashOutputLength,  // In *bits*
                                    hashOutput
                                    );

            if (allOK)
                {
                memcpy(
                       &(output[inputDivDigestBytes * digestSizeBytes]), 
                       hashOutput, 
                       inputModDigestBytes
                      );
                }
            }

        }

    SecZeroAndFreeMemory(hashOutput, digestSizeBytes);
    SecZeroAndFreeMemory(tmpString, tmpStringLength);

    return allOK;
}


// ----------------------------------------------------------------------------
BOOL LUKSAFS_AFSplit(
    int inputLength,
    BYTE* inputData,
    int n,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    int outputLength,  // Length of outputData buffer in bytes
    BYTE* outputData
)
{
    BOOL allOK;
    int j;
    BYTE* randomStripeData;
    BYTE* calcStripe;
    BYTE* processedStripes;
    int blockLength;
    int randomStripeLength;

    allOK = TRUE;

    randomStripeData = NULL;
    calcStripe = NULL;

    blockLength = inputLength;

    randomStripeLength = (inputLength * (n - 1));

    // Sanity check
    if (outputLength < (randomStripeLength + blockLength))
        {
        allOK = FALSE;
        }

    if (allOK)
        {
        processedStripes = malloc(blockLength);
        if (processedStripes == NULL)
            {
            allOK = FALSE;
            }
        }

    randomStripeData = LUKSAFS_GetRandom(randomStripeLength);
    if (randomStripeData == NULL)
        {
        allOK = FALSE;
        }

    if (allOK)
        {
        allOK = LUKSAFS_Process(
                                randomStripeData,
                                n,
                                blockLength,
                                hashKernelModeDeviceName,
                                hashGUID,
                                processedStripes
                               );
        }

    if (allOK)
        {
        // XOR last hash output with input
        calcStripe = malloc(blockLength);
        if (calcStripe == NULL)
            {
            allOK = FALSE;
            }
        }
    if (allOK)
        {
        for (j = 0; j < blockLength; j++)
            {
            calcStripe[j] = (processedStripes[j] ^ inputData[j]);
            }
        }

    // Store result as random block "n" as output
    if (allOK)
        {
        memcpy(outputData, randomStripeData, randomStripeLength);
        memcpy(&(outputData[randomStripeLength]), calcStripe, blockLength);
        }


    SecZeroAndFreeMemory(processedStripes, blockLength);
    SecZeroAndFreeMemory(calcStripe, blockLength);
    SecZeroAndFreeMemory(randomStripeData, randomStripeLength);

    return allOK;
}


// ----------------------------------------------------------------------------
// Process blocks 1 - (n-1)
BOOL LUKSAFS_Process(
    BYTE* stripeData,
    int totalBlockCount,
    int blockLength,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    BYTE* output  // This buffer MUST BE at least "blockLength" long
)
{
    BOOL allOK;
    BYTE* prev;
    int i;
    int j;
    BYTE* tmpData;
    BYTE* hashedBlock;
    BYTE* blockN;
    HASH hashDetails;

    allOK = TRUE;

    if (allOK)
        {
        prev = malloc(blockLength);
        hashedBlock = malloc(blockLength);
        blockN = malloc(blockLength);
        tmpData = malloc(blockLength);
        allOK = (
                 (prev != NULL) &&
                 (hashedBlock != NULL) &&
                 (blockN != NULL) &&
                 (tmpData != NULL)
                ); 
        }

    if (allOK)
        {
        allOK = driver_HashGetImplDetails(
                                        hashKernelModeDeviceName,
                                        &hashGUID,
                                        &hashDetails
                                        );
        }

    if (allOK)
        {
        memset(prev, 0, blockLength);

        // -1 because we don't process the last block
        for (i = 1; i <= (totalBlockCount-1); i++)
            {
            // Get block "i"
            LUKSAFS_GetBlock(stripeData, totalBlockCount, blockLength, i, blockN);

            // XOR previous block output with current stripe
            for (j = 0; j <= (blockLength - 1); j++)
                {
                tmpData[j] = (prev[j] ^ blockN[j]);
                }

            // Hash output
//            LUKSAFS_RepeatedHash(
//                                hashKernelModeDeviceName,
//                                hashGUID,
//                                blockLength,
//                                &(tmpData[0]),
//                                &(hashedBlock[0])
//                                );
            LUKSAFS_RepeatedHash(
                                hashKernelModeDeviceName,
                                &hashDetails,

                                blockLength,
                                &(tmpData[0]),
                                &(hashedBlock[0])
                                );

            // Setup for next iteration
            memcpy(prev, hashedBlock, blockLength);
            };

        // Store last hashing result as output
        memcpy(output, prev, blockLength);
        }


    SecZeroAndFreeMemory(tmpData, blockLength);
    SecZeroAndFreeMemory(blockN, blockLength);
    SecZeroAndFreeMemory(hashedBlock, blockLength);
    SecZeroAndFreeMemory(prev, blockLength);

    // Unload any cached hash drivers used
    driver_HashUnloadDLLsCached();

    return allOK;
}


// ----------------------------------------------------------------------------
// Get the "blockNumber" block out of "totalBlockCount"
// IMPORTANT: blockNumber indexes from ***1***, not 0
BOOL LUKSAFS_GetBlock(
    BYTE* stripeData,
    int totalBlockCount,
    int blockLength,
    int blockNumber,
    BYTE* output  // The buffer passed in here must be at *least* blockLength bytes long
)
{
    BOOL allOK;
    int i;

    allOK = TRUE;

    // xxx - sanity checking

    // Get block "i"
    for (i = 0; i <= (blockLength-1); i++)
        {
        // -1 because block numbers start from 1
        output[i] = stripeData[(((blockNumber-1) * blockLength) + i)];
        }

    return allOK;
}


// ----------------------------------------------------------------------------
// Note: This expects an output buffer of (inputLength / n) bytes or larger
BOOL LUKSAFS_AFMerge(
    BYTE* inputData,
    int inputLength,  // In bytes

    int n,
    WCHAR* hashKernelModeDeviceName,
    GUID hashGUID,
    int outputLength,  // Size of outputData buffer, in bytes. Only included as sanity check
    BYTE* outputData  // This must be outputLength bytes long
)
{
    BOOL allOK;
    int i;
    BYTE* processedStripes;
    int blockLength;
    BYTE* lastBlock;

    allOK = TRUE;

    blockLength = (inputLength / n);

    // Sanity check...
    if (outputLength < blockLength)
        {
        allOK = FALSE;
        }

    if (allOK)
        {
        lastBlock = malloc(blockLength);
        if (lastBlock == NULL)
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        processedStripes = malloc(blockLength);
        if (processedStripes == NULL)
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        allOK = LUKSAFS_Process(
                        inputData,
                        n,
                        blockLength,
                        hashKernelModeDeviceName,
                        hashGUID,
                        processedStripes
                       );
        }

    // Get block "i"
    if (allOK)
        {
        allOK = LUKSAFS_GetBlock(inputData, n, blockLength, n, lastBlock);
        }


    if (allOK)
        {
        // XOR last hash output with last block to obtain original data
        for (i = 0; i < blockLength; i++)
            {
            outputData[i] = (processedStripes[i] ^ lastBlock[i]);
            }
        }

    SecZeroAndFreeMemory(processedStripes, blockLength);
    SecZeroAndFreeMemory(lastBlock, blockLength);

    return allOK;
}

// =========================================================================
// =========================================================================
