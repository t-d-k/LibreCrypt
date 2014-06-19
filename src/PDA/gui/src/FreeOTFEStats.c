// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdlib.h>

#include "SDUGeneral.h"

#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "DriverInterface.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "FreeOTFEStats.h"
#include "FreeOTFEDriverConstsCommon.h"

#define STATS_CYPHER_CYCLES         1000
//#define STATS_CYPHER_CYCLES         1
// Data block size in bytes
#define STATS_CYPHER_DATABLOCKSIZE  ENCRYPTION_BLOCK_SIZE

#define STATS_HASH_CYCLES        1000
//#define STATS_HASH_CYCLES          0
// Hash input size in *bits*
#define STATS_HASH_INPUT_SIZE    256
// Max hash output buffer size in *bits*
#define STATS_HASH_OUTPUT_SIZE  1024

// =========================================================================
// Forward declarations...
BOOL _DriverStats_Cypher(FILE* ReportFile);
BOOL _DriverStats_Hash(FILE* ReportFile);
void _DriverStats_ReportTimeDiff(
    FILE* ReportFile, 
    DWORD timeStart, 
    DWORD timeEnd
);


// =========================================================================
BOOL GenerateDriverStatsReport(WCHAR* ReportFilename)
{
    BOOL allOK;
    FILE* ReportFile;
    DWORD timeReportStart;
    DWORD timeReportEnd;

    allOK = TRUE;

    ReportFile = _wfopen(ReportFilename, TEXT("w"));
    if (ReportFile == NULL)
        {
        allOK = FALSE;
        }
    else
        {
        fwprintf(ReportFile, TEXT("FreeOTFE4PDA Statistics Report\n"));
        fwprintf(ReportFile, TEXT("==============================\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));

        fwprintf(ReportFile, TEXT("!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));
        fwprintf(ReportFile, TEXT("!!!  IMPORTANT NOTICE  !!!\n"));
        fwprintf(ReportFile, TEXT("!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("The statistics produced by this software are MEANINGLESS OUTSIDE FREEOTFE, and should NOT be used for comparison purposes with other software products.\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("The cypher/hash routines are called in a manner intended to generate comparative statistics that are only valid when compared BETWEEN FREEOTFE DRIVERS ONLY.\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("In short, don't use the information shown in this report for benchmarking anything other than FreeOTFE drivers against other *FreeOTFE* *drivers* - the figures generated have no value beyond this, and you'll only end up comparing apples with oranges!\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));

        timeReportStart = GetTickCount();
        _DriverStats_Cypher(ReportFile);
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));
        _DriverStats_Hash(ReportFile);
        timeReportEnd = GetTickCount();
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("Report Summary\n"));
        fwprintf(ReportFile, TEXT("--------------\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("Time to generate report: "));
        _DriverStats_ReportTimeDiff(
                                    ReportFile, 
                                    timeReportStart, 
                                    timeReportEnd
                                   );
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));

        fwprintf(ReportFile, TEXT("End of Report.\n"));
        fwprintf(ReportFile, TEXT("\n"));

        fclose(ReportFile);
        }

    return allOK;
}


// =========================================================================
void _DriverStats_ReportTimeDiff(
    FILE* ReportFile, 
    DWORD timeStart, 
    DWORD timeEnd
)
{
    DWORD timeDiff;
    DWORD timeDiffMins, timeDiffSecs, timeDiffMSecs;

    timeDiff = (timeEnd - timeStart);

    timeDiffSecs = (timeDiff / 1000);
    timeDiffMSecs = (timeDiff % 1000);
    timeDiffMins = (timeDiffSecs / 60);
    timeDiffSecs = (timeDiffSecs % 60);

    fwprintf(
             ReportFile,
             TEXT("%d:%02d.%03d"),
             timeDiffMins,
             timeDiffSecs,
             timeDiffMSecs
            );
}


// =========================================================================
void _ReportWarningBanner(FILE* ReportFile)
{
    fwprintf(ReportFile, TEXT("\n"));
    fwprintf(ReportFile, TEXT("+++++++++++++\n"));
    fwprintf(ReportFile, TEXT("++ WARNING ++\n"));
    fwprintf(ReportFile, TEXT("+++++++++++++\n"));
    fwprintf(ReportFile, TEXT("\n"));
}


// =========================================================================
void _ReportDriverImplFailures(
    FILE* ReportFile, 
    int failedDrivers, 
    int failedImpl
)
{
    if (failedDrivers > 0) 
        {
        _ReportWarningBanner(ReportFile);
        fwprintf(ReportFile, TEXT("%d of the drivers tested has one or more FAILURES.\n"), failedDrivers);
        fwprintf(ReportFile, TEXT("(Implementation failures for all drivers: %d)\n"), failedImpl);
        fwprintf(ReportFile, TEXT("\n"));
        }
}


// =========================================================================
void _ReportDriverCount(
    FILE* ReportFile, 
    WCHAR* driverTypeText,
    int driverCount 
)
{
    if (driverCount <= 0) 
        {
        _ReportWarningBanner(ReportFile);
        fwprintf(ReportFile, TEXT("No %s drivers could be found.\n"), driverTypeText);
        fwprintf(ReportFile, TEXT("\n"));
        }
    else
        {
        fwprintf(
                 ReportFile, 
                 TEXT("Total %s drivers processed: %d\n"), 
                 driverTypeText, 
                 driverCount
                );
        }
}


// =========================================================================
// Generate cypher stats
BOOL _DriverStats_Cypher(FILE* ReportFile)
{
    // Pointer to a list of strings (pointers to strings)
    WCHAR** driverFilenamesHash;
    WCHAR** driverFilenamesCypher;
    int countDriversHash;
    int countDriversCypher;
    int cypherDriverIdx;
    unsigned int cypherImplIdx;
    WCHAR* currCypherDriverFilename;
    // Pointer to a list of algorithms the drivers support (pointers to structs)
    HASH_DRIVER_INFO** driverInfoHash;
    CYPHER_DRIVER_INFO_v3** driverInfoCypher;
    CYPHER_DRIVER_INFO_v3* currDriverInfoCypher;
    CYPHER_v3* currCypherImpl;
    WCHAR* tmpGUIDStr;
    int maxCDKSize_bits;
    int maxIVSize_bits;
    int cdkSize_bits;
    unsigned int criticalDataKeySizeRequired;
    FREEOTFEBYTE* criticalDataKey;
    FREEOTFEBYTE* IV;
    BOOL allOK;
    int BlockSize;  // In bytes
    FREEOTFEBYTE* BlockIn;
    FREEOTFEBYTE* BlockOut;
    int Cycles;
    int currCycle;
    DWORD timeAllStart;
    DWORD timeAllEnd;
    DWORD timeDriverStart;
    DWORD timeDriverEnd;
    DWORD timeCurrStart;
    DWORD timeCurrEnd;
    WCHAR prettyTitle[MAX_PRETTYPRINTED_TITLE];
    BOOL failureFoundDriver;
    BOOL failureFoundImpl;
    int failedDrivers;
    int failedImpl;
    unsigned int IVSize;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_DriverStats_Cypher\n")));
    
    // Stats for...
    BlockSize = STATS_CYPHER_DATABLOCKSIZE;
    Cycles = STATS_CYPHER_CYCLES;

    // Numbers...
    maxCDKSize_bits = 0;
    maxIVSize_bits = 0;
    countDriversCypher = 0;
    criticalDataKeySizeRequired = 0;
    cypherDriverIdx = 0;
    failedDrivers = 0;
    failedImpl = 0;
    timeAllStart = 0;
    timeAllEnd = 0;

    // Pointers...
    criticalDataKey = NULL;
    IV = NULL;
    driverInfoHash = NULL;
    driverInfoCypher = NULL;
    driverFilenamesHash = NULL;
    driverFilenamesCypher = NULL;
    BlockIn = NULL;
    BlockOut = NULL;
    

    /*
    Identify all cypher drivers
    Identify all cypher algorithms supported by each of the cypher drivers

    Loop through all cypher libs...
      Loop through all cyphers supported by current cypher lib...
    */

    allOK = TRUE;

    if (allOK)
        {
        allOK = driver_GetAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher,

                                        FALSE
                                        );
        }

    // Setup the block to encrypt/decrypt
    if (allOK)
        {
        BlockIn = malloc(BlockSize);
        BlockOut = malloc(BlockSize);
        if (
            (BlockIn == NULL) ||
            (BlockOut == NULL)
           )
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        fwprintf(ReportFile, TEXT("FreeOTFE Cyphers\n"));
        fwprintf(ReportFile, TEXT("----------------\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("Data block size: %d bytes\n"), BlockSize);
        fwprintf(ReportFile, TEXT("Cycles         : %d\n"), Cycles);
        fwprintf(ReportFile, TEXT("\n"));
        }

    if (allOK)
        {
        // !!! OPTIMISATION !!!
        // Determine the longest cypher key and IV size
        maxIVSize_bits = 0;
        maxCDKSize_bits = 0;
        while (driverFilenamesCypher[cypherDriverIdx] != NULL)
            {
            currCypherDriverFilename = driverFilenamesCypher[cypherDriverIdx];
            currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];

            // If we got the DLL filename, but no cypher details, skip this DLL file
            if (currDriverInfoCypher == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Optimisation skipping cypher DLL filename due to lack of driver details\n")));
                cypherDriverIdx++;
                continue;
                }

            for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)
                {
                currCypherImpl = &(currDriverInfoCypher->CypherDetails[cypherImplIdx]);

                // Determine size of "critical data key"...
                cdkSize_bits = currCypherImpl->KeySizeRequired;  // In *bits*
                if (cdkSize_bits < 0)
                    {
                    cdkSize_bits = 512;
                    }

                maxIVSize_bits = max(maxIVSize_bits, currCypherImpl->BlockSize);
                maxCDKSize_bits = max(maxCDKSize_bits, cdkSize_bits);
                }

            cypherDriverIdx++;
            }

        IV = NULL;
        criticalDataKeySizeRequired = (maxCDKSize_bits / 8);
        criticalDataKey = malloc(criticalDataKeySizeRequired);
        if (criticalDataKey == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for criticalDataKey\n")));        
            allOK = FALSE;
            }
        else
            {   
            IV = malloc((maxIVSize_bits / 8));
            if (IV == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for IV\n")));        
                allOK = FALSE;
                }
            }

        if (allOK)
            {
            failedImpl = 0;

            timeAllStart = GetTickCount();

            // Loop through all cypher libs...
            while (driverFilenamesCypher[cypherDriverIdx] != NULL)
                {
                currCypherDriverFilename = driverFilenamesCypher[cypherDriverIdx];
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Testing cypher driver (%d): %ls\n"), (cypherDriverIdx+1), currCypherDriverFilename));
                fwprintf(ReportFile, TEXT("------------------------\n"));
                fwprintf(ReportFile, TEXT("Driver         : %s\n"), currCypherDriverFilename);
                fwprintf(ReportFile, TEXT("\n"));
                timeDriverStart = GetTickCount();
                currDriverInfoCypher = driverInfoCypher[cypherDriverIdx];
                if (currDriverInfoCypher == NULL)
                    {
                    // Skip out...
                    DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping; no supported algorithm details obtained.\n")));
                    fwprintf(ReportFile, (TEXT("+++ FAILURE: Skipping driver; no supported algorithm details obtained.\n")));
                    fwprintf(ReportFile, TEXT("\n"));
                    failedDrivers++;
                    cypherDriverIdx++;
                    continue;
                    }
                
                // Loop through all cyphers supported by current cypher lib...
                failureFoundDriver = FALSE;
                for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)
                    {
                    failureFoundImpl = FALSE;

                    currCypherImpl = &(currDriverInfoCypher->CypherDetails[cypherImplIdx]);
                    GUIDToWCHAR(&(currCypherImpl->CypherGUID), &tmpGUIDStr);
                    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Cypher implementation: %ls\n"), tmpGUIDStr));
                    fwprintf(ReportFile, TEXT("Implementation : %s\n"), tmpGUIDStr);
                    driver_CypherPrettyprintAlgTechTitle(currCypherImpl, prettyTitle, sizeof(prettyTitle));
                    fwprintf(ReportFile, TEXT("Pretty title   : %s\n"), prettyTitle);
                    SecZeroAndFreeWCHARMemory(tmpGUIDStr);

                    // Determine size of "critical data key"...
                    cdkSize_bits = currCypherImpl->KeySizeRequired;  // In *bits*
                    if (cdkSize_bits < 0) 
                        {
                        cdkSize_bits = 512;
                        }

                    // Zero the key for decryption, in case previous was "dirty"
                    if (currCypherImpl->BlockSize > 0)
                        {
                        memset(criticalDataKey, 0, (cdkSize_bits / 8));
                        }

                    // Zero the IV for decryption, in case previous was "dirty"
                    IVSize = 0;
                    if (currCypherImpl->BlockSize > 0)
                        {
                        IVSize = currCypherImpl->BlockSize;
                        memset(IV, 0, (IVSize / 8));
                        }

                    // Encryption...
                    memset(BlockIn, 0, BlockSize);
                    memset(BlockOut, 0, BlockSize);                        
                    timeCurrStart = GetTickCount();
                    for (currCycle = 1; currCycle <= Cycles; currCycle++)
                        {
                        if (!(driver_CypherEncryptSectorData(
                                     currCypherDriverFilename,
                                     currCypherImpl->CypherGUID,
                                     FREEOTFE_v1_DUMMY_SECTOR_ID,
                                     FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                     cdkSize_bits,
                                     criticalDataKey,
                                     IVSize,
                                     IV,
                                     BlockSize,
                                     BlockIn,
                                     BlockOut
                                    )))
                            {
                            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to encrypt.\n")));
                            fwprintf(ReportFile, TEXT("+++ FAILURE in encrypting data block: %d\n"), currCycle);
                            failureFoundImpl = TRUE;
                            break;
                            }
                        }
                    timeCurrEnd = GetTickCount();

                    fwprintf(ReportFile, TEXT("Encryption time: "));
                    _DriverStats_ReportTimeDiff(
                                                ReportFile, 
                                                timeCurrStart, 
                                                timeCurrEnd
                                               );
                    fwprintf(ReportFile, TEXT("\n"));


                    // Decryption...
                    memset(BlockIn, 0, BlockSize);
                    memset(BlockOut, 0, BlockSize);                        
                    timeCurrStart = GetTickCount();
                    for (currCycle = 1; currCycle <= Cycles; currCycle++)
                        {
                        if (!(driver_CypherDecryptSectorData(
                                     currCypherDriverFilename,
                                     currCypherImpl->CypherGUID,
                                     FREEOTFE_v1_DUMMY_SECTOR_ID,
                                     FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                     cdkSize_bits,
                                     criticalDataKey,
                                     IVSize,
                                     IV,
                                     BlockSize,
                                     BlockIn,
                                     BlockOut
                                    )))
                            {
                            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to decrypt.\n")));
                            fwprintf(ReportFile, TEXT("+++ FAILURE in decrypting data block: %d\n"), currCycle);
                            failureFoundImpl = TRUE;
                            break;
                            }
                        }
                    timeCurrEnd = GetTickCount();

                    fwprintf(ReportFile, TEXT("Decryption time: "));
                    _DriverStats_ReportTimeDiff(
                                                ReportFile, 
                                                timeCurrStart, 
                                                timeCurrEnd
                                               );
                    fwprintf(ReportFile, TEXT("\n"));
                    fwprintf(ReportFile, TEXT("\n"));

                    if (failureFoundImpl)
                        {
                        failureFoundDriver = TRUE;
                        failedImpl++;
                        failureFoundImpl = FALSE;
                        }
                    }  // for (cypherImplIdx = 0; cypherImplIdx < currDriverInfoCypher->CypherCount; cypherImplIdx++)

                timeDriverEnd = GetTickCount();

                fwprintf(ReportFile, TEXT("Driver time    : "));
                _DriverStats_ReportTimeDiff(
                                            ReportFile, 
                                            timeDriverStart, 
                                            timeDriverEnd
                                           );
                fwprintf(ReportFile, TEXT("\n"));
                fwprintf(ReportFile, TEXT("\n"));

                if (failureFoundDriver)
                    {
                    failedDrivers++;
                    failureFoundDriver = FALSE;
                    }

                cypherDriverIdx++;
                }  // while (driverFilenamesCypher[cypherDriverIdx] != NULL)
            }

        timeAllEnd = GetTickCount();
        }

    // Finish report and close report file...
    if (ReportFile != NULL)
        {
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));
        _ReportDriverCount(ReportFile, TEXT("cypher"), cypherDriverIdx);
        fwprintf(ReportFile, TEXT("Time to process               : "));
        _DriverStats_ReportTimeDiff(ReportFile, timeAllStart, timeAllEnd);
        fwprintf(ReportFile, TEXT("\n"));

        _ReportDriverImplFailures(ReportFile, failedDrivers, failedImpl);
        }


    // Cleanup...
    SecZeroAndFreeMemory(BlockIn, BlockSize);
    SecZeroAndFreeMemory(BlockOut, BlockSize);

    SecZeroAndFreeMemory(criticalDataKey, criticalDataKeySizeRequired);
    SecZeroAndFreeMemory(IV, (maxIVSize_bits / 8));

    driver_FreeAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher
                                        );

    if (allOK)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Operation completed OK\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Operation FAILED.\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_DriverStats_Cypher\n")));
    return allOK;
}

// =========================================================================
// Generate cypher stats
BOOL _DriverStats_Hash(FILE* ReportFile)
{
    // Pointer to a list of strings (pointers to strings)
    WCHAR** driverFilenamesHash;
    WCHAR** driverFilenamesCypher;
    int countDriversHash;
    int countDriversCypher;
    int hashDriverIdx;
    unsigned int hashImplIdx;
    WCHAR* currHashDriverFilename;
    // Pointer to a list of algorithms the drivers support (pointers to structs)
    HASH_DRIVER_INFO** driverInfoHash;
    CYPHER_DRIVER_INFO_v3** driverInfoCypher;
    HASH_DRIVER_INFO* currDriverInfoHash;
    HASH* currHashImpl;
    WCHAR* tmpGUIDStr;
    unsigned int criticalDataKeySizeRequired;
    BOOL allOK;
    FREEOTFEBYTE* BlockIn;
    FREEOTFEBYTE* BlockOut;
    unsigned int BlockSizeIn;
    unsigned int BlockSizeOut;
    unsigned int tmpBlockSize;
    int Cycles;
    int currCycle;
    DWORD timeAllStart;
    DWORD timeAllEnd;
    DWORD timeDriverStart;
    DWORD timeDriverEnd;
    DWORD timeCurrStart;
    DWORD timeCurrEnd;
    WCHAR prettyTitle[MAX_PRETTYPRINTED_TITLE];
    BOOL failureFoundDriver;
    BOOL failureFoundImpl;
    int failedDrivers;
    int failedImpl;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_DriverStats_Hash\n")));
    
    // Stats for...
    BlockSizeIn = STATS_HASH_INPUT_SIZE;
    BlockSizeOut = STATS_HASH_OUTPUT_SIZE;
    Cycles = STATS_HASH_CYCLES;

    // Numbers...
    countDriversCypher = 0;
    criticalDataKeySizeRequired = 0;
    hashDriverIdx = 0;
    timeAllStart = 0;
    timeAllEnd = 0;
    failedDrivers = 0;
    failedImpl = 0;

    // Pointers...
    driverInfoHash = NULL;
    driverInfoCypher = NULL;
    driverFilenamesHash = NULL;
    driverFilenamesCypher = NULL;
    

    /*
    Identify all hash drivers
    Identify all hash algorithms supported by each of the hash drivers

    Loop through all hash libs...
      Loop through all hashes supported by current hash lib...
    */

    allOK = TRUE;


    if (allOK)
        {
        allOK = driver_GetAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher,

                                        FALSE
                                        );
        }

    // Setup the block to hash
    if (allOK)
        {
        BlockIn = malloc((BlockSizeIn / 8));
        BlockOut = malloc((BlockSizeOut / 8));
        if (
            (BlockIn == NULL) ||
            (BlockOut == NULL)
           )
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        fwprintf(ReportFile, TEXT("FreeOTFE Hashes\n"));
        fwprintf(ReportFile, TEXT("---------------\n"));
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("Input block size : %d bits\n"), BlockSizeIn);
        fwprintf(ReportFile, TEXT("Output block size: %d bits\n"), BlockSizeOut);
        fwprintf(ReportFile, TEXT("Cycles           : %d\n"), Cycles);
        fwprintf(ReportFile, TEXT("\n"));
        }

    if (allOK)
        {
        timeAllStart = GetTickCount();

        // Loop through all hash libs...
        while (driverFilenamesHash[hashDriverIdx] != NULL)
            {
            currHashDriverFilename = driverFilenamesHash[hashDriverIdx];
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Testing hash driver (%d): %ls\n"), (hashDriverIdx+1), currHashDriverFilename));
            fwprintf(ReportFile, TEXT("------------------------\n"));
            fwprintf(ReportFile, TEXT("Driver         : %s\n"), currHashDriverFilename);
            fwprintf(ReportFile, TEXT("\n"));
            timeDriverStart = GetTickCount();
            currDriverInfoHash = driverInfoHash[hashDriverIdx];
            if (currDriverInfoHash == NULL)
                {
                // Skip out...
                DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping; no supported algorithm details obtained.\n")));
                fwprintf(ReportFile, (TEXT("+++ FAILURE: Skipping driver; no supported algorithm details obtained.\n")));
                fwprintf(ReportFile, TEXT("\n"));
                hashDriverIdx++;
                continue;
                }

            // Loop through all hashes supported by current hash lib...
            failureFoundDriver = FALSE;
            for (hashImplIdx = 0; hashImplIdx < currDriverInfoHash->HashCount; hashImplIdx++)
                {
                failureFoundImpl = FALSE;

                currHashImpl = &(currDriverInfoHash->HashDetails[hashImplIdx]);
                GUIDToWCHAR(&(currHashImpl->HashGUID), &tmpGUIDStr);
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Hash implementation: %ls\n"), tmpGUIDStr));
                fwprintf(ReportFile, TEXT("Implementation : %s\n"), tmpGUIDStr);
                driver_HashPrettyprintAlgTechTitle(currHashImpl, prettyTitle, sizeof(prettyTitle));
                fwprintf(ReportFile, TEXT("Pretty title   : %s\n"), prettyTitle);
                SecZeroAndFreeWCHARMemory(tmpGUIDStr);

                // v2 and later volumes won't work with hash algorithms with <= 0 length, or
                // blocksize, as HMAC/PBKDF2 require these to be fixed
                //  - skip these, and move onto the next hash algorithm (if any)
                if (
                    (currHashImpl->Length <= 0) ||
                    (currHashImpl->BlockSize <= 0)
                   ) 
                    {
                    // Skip - can't use
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Skipping hash; either length or blocksize <= 0.\n")));
                    fwprintf(ReportFile, TEXT("Skipping hash; either length or blocksize <= 0.\n"));
                    fwprintf(ReportFile, TEXT("\n"));
                    failureFoundDriver++;
                    continue;
                    }

                memset(BlockIn, 0, BlockSizeIn);
                memset(BlockOut, 0, BlockSizeOut);                        
                timeCurrStart = GetTickCount();
                for (currCycle = 1; currCycle <= Cycles; currCycle++)
                    {
                    tmpBlockSize = BlockSizeOut;
                    if (!(driver_HashData(
                                 currHashDriverFilename,
                                 currHashImpl->HashGUID,
                                 BlockSizeIn,
                                 BlockIn,
                                 &tmpBlockSize,
                                 BlockOut
                                )))
                        {
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to hash.\n")));
                        fwprintf(ReportFile, TEXT("+++ FAILURE in hashing data block: %d\n"), currCycle);
                        failureFoundImpl = TRUE;
                        break;
                        }
                    }
                timeCurrEnd = GetTickCount();

                fwprintf(ReportFile, TEXT("Hash time      : "));
                _DriverStats_ReportTimeDiff(
                                            ReportFile, 
                                            timeCurrStart, 
                                            timeCurrEnd
                                           );
                fwprintf(ReportFile, TEXT("\n"));
                fwprintf(ReportFile, TEXT("\n"));

                if (failureFoundImpl)
                    {
                    failureFoundDriver = TRUE;
                    failedImpl++;
                    failureFoundImpl = FALSE;
                    }
                }  // for (hashImplIdx = 0; hashImplIdx < currDriverInfoHash->HashCount; hashImplIdx++)

            timeDriverEnd = GetTickCount();

            fwprintf(ReportFile, TEXT("Driver time    : "));
            _DriverStats_ReportTimeDiff(
                                        ReportFile, 
                                        timeDriverStart, 
                                        timeDriverEnd
                                       );
            fwprintf(ReportFile, TEXT("\n"));
            fwprintf(ReportFile, TEXT("\n"));

            if (failureFoundDriver)
                {
                failedDrivers++;
                failureFoundDriver = FALSE;
                }

            hashDriverIdx++;
            }  // while (driverFilenamesHash[hashDriverIdx] != NULL)

        timeAllEnd = GetTickCount();
        }

    // Finish report and close report file...
    if (ReportFile != NULL)
        {
        fwprintf(ReportFile, TEXT("\n"));
        fwprintf(ReportFile, TEXT("\n"));
        _ReportDriverCount(ReportFile, TEXT("hash"), hashDriverIdx);
        fwprintf(ReportFile, TEXT("Time to process             : "));
        _DriverStats_ReportTimeDiff(ReportFile, timeAllStart, timeAllEnd);
        fwprintf(ReportFile, TEXT("\n"));

        _ReportDriverImplFailures(ReportFile, failedDrivers, failedImpl);
        }


    // Cleanup...
    driver_FreeAllAlgorithmDriverDetails(
                                        &countDriversHash,
                                        &driverFilenamesHash,
                                        &driverInfoHash,

                                        &countDriversCypher,
                                        &driverFilenamesCypher,
                                        &driverInfoCypher
                                        );

    if (allOK)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Operation completed OK\n")));
        }
    else
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Operation FAILED.\n")));
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_DriverStats_Hash\n")));
    return allOK;
}


// =========================================================================
// =========================================================================

