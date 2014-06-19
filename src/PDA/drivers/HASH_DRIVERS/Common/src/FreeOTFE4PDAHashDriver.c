// Description: FreeOTFE Hash Device Driver
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"

#include <stdio.h>

#include "SDUGeneral.h"

#include "FreeOTFE4PDAHashDriver.h"
#include "FreeOTFEHashAPICommon.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFEHashImpl.h"
#include "FreeOTFE4PDAHashAPI.h"


// =========================================================================
BOOL WINAPI DllMain(
  HANDLE hinstDLL, 
  DWORD dwReason, 
  LPVOID lpvReserved
)
{
    BOOL retval = TRUE;
    int majorVersion;
    int minorVersion;
    int revisionVersion;
    int buildVersion;
#if DBG
    static BOOL setDebugLevel = FALSE;
    // Default to all on
//    ULONG default_DebugLevel  = 0xFFFFFFFF;  
    // Default to all except verbose debug
    ULONG default_DebugLevel  = 
                                DEBUGLEV_ERROR |
                                DEBUGLEV_WARN  |
                                DEBUGLEV_INFO  |
                                DEBUGLEV_ENTER |
                                DEBUGLEV_EXIT;
    DWORD useDebugLevel;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("DllMain\n")));

    if (!(setDebugLevel))
        {
        useDebugLevel = ReadDebugLevelFromFile(DEBUGLEVEL_FILE);
        if (useDebugLevel == FREEOTFE_DEBUG_LEVEL_NOT_READ)
            {
            useDebugLevel = default_DebugLevel;
            }

        FreeOTFEDebugLevel = useDebugLevel;
        setDebugLevel  = TRUE;
        }

    DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("Debug level   : %d\n"), FreeOTFEDebugLevel));
#endif

    if (!(SDUGetVersionInfo(
				            NULL,
				            &majorVersion,
				            &minorVersion, 
				            &revisionVersion, 
				            &buildVersion
				           )))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("Driver version: <unable to determine>\n")));
        }
    else
        {
        DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("Driver version: v%02d.%02d.%02d.%04d\n"),
                                        majorVersion, 
                                        minorVersion,
                                        revisionVersion,
                                        buildVersion
                                       ));
        }
        

    switch (dwReason)
        {
        case DLL_PROCESS_ATTACH:
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_ATTACH\n")));
            // We really don't care about getting
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls; disable
            DisableThreadLibraryCalls(hinstDLL);
            break;
            }

        case DLL_THREAD_ATTACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTHASHDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_ATTACH\n")));
            break;
            }

        case DLL_THREAD_DETACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTHASHDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_DETACH\n")));
            break;
            }

        case DLL_PROCESS_DETACH:
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_DETACH\n")));
            break;
            }

        }

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("DllMain\n")));
    return retval;
}


// =========================================================================
// IOCTL to get hash driver identification
DWORD
HashIdentifyDriver(
    DIOC_HASH_IDENTIFYDRIVER* Buffer
)
{
    DWORD status = ERROR_SUCCESS;
    HASH_DRIVER_INFO tmpDriverInfo;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("IdentifyDriver\n")));

    // Populate output buffer
    ImpHashDriverExtDetailsInit(&tmpDriverInfo);

    Buffer->DriverGUID = tmpDriverInfo.DriverGUID;
    FREEOTFE_MEMZERO(Buffer->Title, sizeof(Buffer->Title));
    FREEOTFE_MEMCPY(
                    Buffer->Title,
                    tmpDriverInfo.DriverTitle,
                    strlen(tmpDriverInfo.DriverTitle)
                   );
    Buffer->VersionID = tmpDriverInfo.DriverVersionID;
    Buffer->CountHashes = tmpDriverInfo.HashCount;

    ImpHashDriverExtDetailsCleardown(&tmpDriverInfo);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("IdentifyDriver\n")));

    return status;
}


// =========================================================================
// IOCTL to get hashes supported
DWORD
HashIdentifySupported(
    DWORD BufferSize,
    DIOC_HASH_IDENTIFYSUPPORTED* Buffer
)
{
    DWORD status = ERROR_SUCCESS;
    HASH_DRIVER_INFO tmpDriverInfo;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("IdentifySupported\n")));

    // Check size of OUTPUT buffer
    if (BufferSize <
            sizeof(*Buffer)-sizeof(Buffer->Hashes))
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect min: %d; got: %d)\n"),
            sizeof(*Buffer)-sizeof(Buffer->Hashes),
            BufferSize
            ));
        status = ERROR_INSUFFICIENT_BUFFER;
        return status;            
        }

        
    // Setup the input parameter passed to ImpHashIdentifySupported, so that it's aware
    // of how large the array of hash details is
    Buffer->BufCount =
       (
         // The size of the buffer, less the array of hash details
         BufferSize - 
           (sizeof(*Buffer)-sizeof(Buffer->Hashes))
       ) /
       // Divide by the size of each hash details struct to give the array length
       sizeof(Buffer->Hashes);
       
    DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("Request is sufficiently large to store %d hash details\n"), Buffer->BufCount));

    ImpHashDriverExtDetailsInit(&tmpDriverInfo);

    if (Buffer->BufCount < tmpDriverInfo.HashCount)
        {
        DEBUGOUTHASHDRV(DEBUGLEV_ERROR, (TEXT("outBuffer not large enough to store enough (can store: %d, requires: )\n"),
            Buffer->BufCount,
            tmpDriverInfo.HashCount
            ));
        status = ERROR_INSUFFICIENT_BUFFER;
        ImpHashDriverExtDetailsCleardown(&tmpDriverInfo);
        return status;
        }


    // Request valid, process...


    Buffer->BufCount = tmpDriverInfo.HashCount;
    FREEOTFE_MEMCPY(
                    &Buffer->Hashes[0],
                    tmpDriverInfo.HashDetails,
                    (sizeof(HASH) * tmpDriverInfo.HashCount)
                   );
     
    ImpHashDriverExtDetailsCleardown(&tmpDriverInfo);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("IdentifySupported\n")));

    return status;
}


// =========================================================================
// IOCTL to hash data
DWORD
HashHash(
    IN      GUID* HashGUID,
    IN      unsigned int DataLength,  // In bits
    IN      unsigned char* Data,
    IN OUT  unsigned int* HashLength,  // In bits
    OUT     unsigned char* Hash
)
{
    DWORD status;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("Hash\n")));

    status = ImpHashHashData(
                             HashGUID,
                             DataLength,  // In bits
                             Data, 
                             HashLength,  // In bits
                             Hash
                            );

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("Hash\n")));

    return status;
}




// =========================================================================
DWORD
HashGetHashDetails(
    IN     GUID* HashGUID,
    IN OUT HASH* HashDetails
)
{
    DWORD status = ERROR_NOT_FOUND;
    HASH_DRIVER_INFO tmpDriverInfo;
    unsigned int i;
    
    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("GetHashDetails\n")));

    ImpHashDriverExtDetailsInit(&tmpDriverInfo);

    for (i = 0; i<tmpDriverInfo.HashCount; i++)
        {
        if (IsEqualGUID(&tmpDriverInfo.HashDetails[i].HashGUID, HashGUID))
            {
            DEBUGOUTHASHDRV(DEBUGLEV_INFO, (TEXT("Located relevant details\n")));
            FREEOTFE_MEMCPY(
                            HashDetails,
                            &tmpDriverInfo.HashDetails[i],
                            sizeof(HASH)
                           );

            status = ERROR_SUCCESS;
            }
        }

    ImpHashDriverExtDetailsCleardown(&tmpDriverInfo);

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("GetHashDetails\n")));

    return status;
}


// =========================================================================
// =========================================================================


