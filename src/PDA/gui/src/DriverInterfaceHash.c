// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCommon.h"


// =========================================================================
// Define externals declared in .h file
LNKLIST_ITEM* G_driver_CacheDLLDetailsHash = NULL;


// =========================================================================
BOOL driver_HashLoadDLL(WCHAR* Filename, DLL_DETAILS_HASH* DLLDetails)
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("driver_HashLoadDLL\n")));

    // Get library path and filename...
    if (retval)
        {
        // +1 for terminating NULL
        DLLDetails->Filename = calloc((wcslen(Filename) + 1), sizeof(DLLDetails->Filename[0]));
        if (DLLDetails->Filename == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Unable to malloc memory to copy driver filename\n")));
            retval = FALSE;
            }
        else
            {
            wcscpy(DLLDetails->Filename, Filename);
            }
        }

    // Load library...
    if (retval)
        {
        DLLDetails->Lib = LoadLibrary(DLLDetails->Filename);
        if (DLLDetails->Lib == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to load library\n")));
            retval = FALSE;
            }
        }

    // Get library function address...
    if (retval)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_IDENTIFYDRIVER));
        DLLDetails->FnIdentifyDriver = (PHashDLLFnIdentifyDriver)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_HASH_IDENTIFYDRIVER
                                        );
        if (DLLDetails->FnIdentifyDriver == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_IDENTIFYSUPPORTED));
        DLLDetails->FnIdentifySupported = (PHashDLLFnIdentifySupported)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_HASH_IDENTIFYSUPPORTED
                                        );
        if (DLLDetails->FnIdentifySupported == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_GETHASHDETAILS));
        DLLDetails->FnGetHashDetails = (PHashDLLFnGetHashDetails)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_HASH_GETHASHDETAILS
                                        );
        if (DLLDetails->FnGetHashDetails == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_HASH_HASH));
        DLLDetails->FnHash = (PHashDLLFnHash)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_HASH_HASH
                                        );
        if (DLLDetails->FnHash == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        }


    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        driver_HashUnloadDLL(DLLDetails);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("driver_HashLoadDLL\n")));
    return retval;
}


// =========================================================================
void driver_HashUnloadDLL(DLL_DETAILS_HASH* DLLDetails)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("driver_HashUnloadDLL\n")));

    if (DLLDetails != NULL)
        {
      	// DLL handle
        if ((DLLDetails->Lib) != NULL)
            {
	        FreeLibrary(DLLDetails->Lib);
            DLLDetails->Lib = NULL;
            }

        SecZeroAndFreeWCHARMemory(DLLDetails->Filename);

        SecZeroMemory(DLLDetails, sizeof(*DLLDetails));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("driver_HashUnloadDLL\n")));
}


// =========================================================================
// malloc a HASH_DRIVER_INFO structure, and populate as appropriate; returning
// a pointer to the structure in driverInfo
BOOL driver_HashGetDriverDetails(WCHAR* HashFilename, HASH_DRIVER_INFO* driverInfo)
{
    BOOL retval = FALSE;
    DLL_DETAILS_HASH DLLDetails;
    DIOC_HASH_IDENTIFYDRIVER identifyBuf;
    DWORD supportedBufSize;
    DIOC_HASH_IDENTIFYSUPPORTED* supportedBuf;
    HASH* hashArray;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashGetDriverDetails\n")));

    if (driver_HashLoadDLL(HashFilename, &DLLDetails))
        {
        if (DLLDetails.FnIdentifyDriver(&identifyBuf) != ERROR_SUCCESS)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifyDriver FAILED!")));
            }
        else
            {
            supportedBufSize = (
                                sizeof(*supportedBuf) -
                                sizeof(HASH) +                                 
                                (sizeof(HASH) * identifyBuf.CountHashes)
                               );
            supportedBuf = malloc(supportedBufSize);
            if (supportedBuf == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCBuffer FnIdentifySupported call\n")));
                }
            else
                {
                if (DLLDetails.FnIdentifySupported(
                                                   supportedBufSize, 
                                                   supportedBuf
                                                  ) != ERROR_SUCCESS)
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifySupported FAILED!")));
                    }
                else
                    {
                    hashArray = calloc(supportedBuf->BufCount, sizeof(HASH));
                    if (hashArray == NULL)
                        {
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc buffer for list of supported to be returned\n")));
                        }
                    else
                        {
                        driverInfo->DriverGUID = identifyBuf.DriverGUID;
                        memcpy(
                               driverInfo->DriverTitle,
                               identifyBuf.Title,
                               sizeof(identifyBuf.Title)
                              );
                        driverInfo->DriverVersionID = identifyBuf.VersionID;
                        driverInfo->HashCount = identifyBuf.CountHashes;

                        memcpy(
                               hashArray, 
                               supportedBuf->Hashes,
                               (supportedBuf->BufCount * sizeof(*hashArray))
                              );

                        driverInfo->HashDetails = hashArray;

                        retval = TRUE;
                        }
                    }

                SecZeroAndFreeMemory(supportedBuf, supportedBufSize);
                }

            SecZeroMemory(&identifyBuf, sizeof(identifyBuf));
            }

        driver_HashUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashGetDriverDetails\n")));
    return retval;
}


// =========================================================================
void driver_HashFreeDriverDetails(HASH_DRIVER_INFO* driverInfo)
{
    if (driverInfo != NULL)
        {
        SecZeroMemory(driverInfo, sizeof(*driverInfo));
        SecZeroAndFreeMemory(
                             driverInfo->HashDetails, 
                             (driverInfo->HashCount * sizeof(HASH))
                            );
        }
}


// =========================================================================
void driver_HashPrettyprintAlgTitle(
    HASH* hashInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];

    // Convert straight ASCII title to UNICODE
    memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
    mbstowcs(
             tmpStrBuf, 
             hashInfo->Title, 
             strlen(hashInfo->Title)
            );

    // Prettyprint to buffer
    _snwprintf(
              buffer,
              (bufferSize / sizeof(buffer[0])),
              STR_FORMAT_FULL_HASH_TITLE,
              tmpStrBuf
             );

    SecZeroMemory(tmpStrBuf, sizeof(tmpStrBuf));
}

// =========================================================================
void driver_HashPrettyprintAlgTechTitle(
    HASH* hashInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];

    // Convert straight ASCII title to UNICODE
    memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
    mbstowcs(
             tmpStrBuf, 
             hashInfo->Title, 
             strlen(hashInfo->Title)
            );

    // Prettyprint to buffer
    _snwprintf(
              buffer,
              (bufferSize / sizeof(buffer[0])),
              STR_FORMAT_FULL_HASH_TECH_TITLE,
              tmpStrBuf,
              hashInfo->Length,
              hashInfo->BlockSize
             );

    SecZeroMemory(tmpStrBuf, sizeof(tmpStrBuf));
}


// =========================================================================
BOOL driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    BOOL retval = FALSE;
    HASH hashInfo;

    if (driver_HashGetImplDetails(
                        ImplDriverFilename,
                        &ImplGUID,
                        &hashInfo
                        ))
        {
        driver_HashPrettyprintAlgTechTitle(
            &hashInfo,
            buffer,
            bufferSize
            );

        retval = TRUE;
        }

    SecZeroMemory(&hashInfo, sizeof(hashInfo));

    return retval;
}


// =========================================================================
BOOL driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID_char(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    char* buffer,
    int bufferSize  // In bytes
)
{
    BOOL retval = FALSE;
    WCHAR tmpBuffer[MAX_PRETTYPRINTED_TITLE];

    if (driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID(
                        ImplDriverFilename,
                        ImplGUID,
                        tmpBuffer,
                        sizeof(tmpBuffer)
                        ))
        {
        wcstombs(buffer, tmpBuffer, bufferSize);
        retval = TRUE;
        }

    SecZeroMemory(tmpBuffer, sizeof(tmpBuffer));

    return retval;
}


// =========================================================================
BOOL
driver_HashGetImplDetails(
    WCHAR* HashFilename, 
    GUID* HashImpl, 
    HASH* ImplDetails
)
{
    BOOL retval = FALSE;
    HASH_DRIVER_INFO hashInfo;
    unsigned int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashGetImplDetails\n")));

    if (driver_HashGetDriverDetails(
                                    HashFilename,
                                    &hashInfo
                                   ))
        {
        for (i = 0; i < hashInfo.HashCount; i++)
            {
            if (IsEqualGUID(
                            HashImpl,
                            &(hashInfo.HashDetails[i].HashGUID)
                           ))
                {
                *ImplDetails = hashInfo.HashDetails[i];
                retval = TRUE;
                break;
                }
            }

        driver_HashFreeDriverDetails(&hashInfo);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashGetImplDetails\n")));
    return retval;
}


// =========================================================================
BOOL driver_HashData(
    WCHAR* HashDriverFilename,
    GUID HashGUID,
    unsigned int BufferSizeIn,  // In *bits*
    unsigned char* BufferIn,
    unsigned int* ptrBufferSizeOut,  // In *bits*
    unsigned char* BufferOut
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_HASH DLLDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashData\n")));

    if (driver_HashLoadDLL(HashDriverFilename, &DLLDetails))
        {
        retval = (DLLDetails.FnHash(
                                    &HashGUID,
                                    BufferSizeIn,
                                    BufferIn,
                                    ptrBufferSizeOut,
                                    BufferOut
                                   ) == ERROR_SUCCESS);

        driver_HashUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashData\n")));
    return retval;
}


// =========================================================================
BOOL driver_HashDataCached(
    WCHAR* HashDriverFilename,
    GUID HashGUID,
    unsigned int BufferSizeIn,  // In *bits*
    unsigned char* BufferIn,
    unsigned int* ptrBufferSizeOut,  // In *bits*
    unsigned char* BufferOut
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_HASH DLLDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashData\n")));

    if (driver_HashLoadDLLCached(HashDriverFilename, &DLLDetails))
        {
        retval = (DLLDetails.FnHash(
                                    &HashGUID,
                                    BufferSizeIn,
                                    BufferIn,
                                    ptrBufferSizeOut,
                                    BufferOut
                                   ) == ERROR_SUCCESS);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashData\n")));
    return retval;
}


// =========================================================================
BOOL driver_HashLoadDLLCached(WCHAR* Filename, DLL_DETAILS_HASH* DLLDetails)
{
    DLL_DETAILS_HASH* currItem;
    BOOL found;
    int cnt;
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashLoadDLLCached\n")));

    found = FALSE;

    cnt = SDULLCountItems(G_driver_CacheDLLDetailsHash);
    for(i = 0; i < cnt; i++)
        {
        SDULLGetItem(G_driver_CacheDLLDetailsHash, i, (void*)(&currItem));
        if (wcscmp(currItem->Filename, Filename) == 0)
            {
            found = TRUE;
            if (DLLDetails != NULL)
                {
                *DLLDetails = *currItem;
                }
            break;
            }
        }


    // If the DLL wasn't found in the cache, load it in and add to the cache
    if (!(found))
        {
        currItem = malloc(sizeof(*currItem));
        if (currItem != NULL)
            {
            if (driver_HashLoadDLL(Filename, currItem))
                {
                if (SDULLAddItem(&G_driver_CacheDLLDetailsHash, (void*)currItem))
                    {
                    found = TRUE;
                    if (DLLDetails != NULL)
                        {
                        *DLLDetails = *currItem;
                        }
                    }
                else
                    {
                    driver_HashUnloadDLL(currItem);
                    }
                }

            if (!(found))
                {
                SecZeroAndFreeMemory(currItem, sizeof(*currItem));
                }
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashLoadDLLCached\n")));
    return found;
}


// =========================================================================
void driver_HashUnloadDLLsCached()
{
    DLL_DETAILS_HASH* currItem;
    int cnt;
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_HashUnloadDLLsCached\n")));

    cnt = SDULLCountItems(G_driver_CacheDLLDetailsHash);
    for(i = 0; i < cnt; i++)
        {
        SDULLGetItem(G_driver_CacheDLLDetailsHash, i, (void*)(&currItem));
        driver_HashUnloadDLL(currItem);
        SecZeroAndFreeMemory(currItem, sizeof(*currItem));
        }

    // Pass FALSE here, as securely overwritten just above
    SDULLRemoveAllItems(&G_driver_CacheDLLDetailsHash, FALSE);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_HashUnloadDLLsCached\n")));
}


// =========================================================================
// =========================================================================
