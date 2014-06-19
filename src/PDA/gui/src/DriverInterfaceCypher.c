// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "SDUi18n.h"


// =========================================================================
// Define externals declared in .h file
LNKLIST_ITEM* G_driver_CacheDLLDetailsCypher = NULL;


// =========================================================================
// Forward declarations...
BOOL _driver_CypherGetDriverDetails_v1(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v1* driverInfo
);

BOOL _driver_CypherGetDriverDetails_v3(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v3* driverInfo
);

void _driver_CypherFreeDriverDetails_v1(CYPHER_DRIVER_INFO_v1* driverInfo);
void _driver_CypherFreeDriverDetails_v3(CYPHER_DRIVER_INFO_v3* driverInfo);

BOOL _driver_CypherEncryptDecryptSectorData(
    BOOL EncryptNotDecrypt,
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int BufferSize, // In bytes
    FREEOTFEBYTE* BufferIn,
    FREEOTFEBYTE* BufferOut
);

BOOL _driver_CypherEncryptDecryptData(
    BOOL EncryptNotDecrypt,
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int BufferSize, // In bytes
    FREEOTFEBYTE* BufferIn,
    FREEOTFEBYTE* BufferOut
);

// This function should be pretty much redundant now...
BOOL _driver_CypherEncryptData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* plaintextBuffer,
    FREEOTFEBYTE* encryptedBuffer
);

// This function should be pretty much redundant now...
BOOL _driver_CypherDecryptData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* encryptedBuffer,
    FREEOTFEBYTE* plaintextBuffer
);


// =========================================================================
BOOL driver_CypherLoadDLL(WCHAR* Filename, DLL_DETAILS_CYPHER* DLLDetails)
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("driver_CypherLoadDLL\n")));

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
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_IDENTIFYDRIVER));
        DLLDetails->FnIdentifyDriver = (PCypherDLLFnIdentifyDriver)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_IDENTIFYDRIVER
                                        );
        if (DLLDetails->FnIdentifyDriver == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1));
        DLLDetails->FnIdentifySupported_v1 = (PCypherDLLFnIdentifySupported_v1)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1
                                        );
        if (DLLDetails->FnIdentifySupported_v1 == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1));
        DLLDetails->FnGetCypherDetails_v1 = (PCypherDLLFnGetCypherDetails_v1)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1
                                        );
        if (DLLDetails->FnGetCypherDetails_v1 == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPT));
        DLLDetails->FnEncrypt = (PCypherDLLFnEncrypt)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPT
                                        );
        if (DLLDetails->FnEncrypt == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPTWITHASCII));
        DLLDetails->FnEncryptWithASCII = (PCypherDLLFnEncryptWithASCII)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPTWITHASCII
                                        );
        if (DLLDetails->FnEncryptWithASCII == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPT));
        DLLDetails->FnDecrypt = (PCypherDLLFnDecrypt)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPT
                                        );
        if (DLLDetails->FnDecrypt == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPTWITHASCII));
        DLLDetails->FnDecryptWithASCII = (PCypherDLLFnDecryptWithASCII)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPTWITHASCII
                                        );
        if (DLLDetails->FnDecryptWithASCII == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address\n")));
            retval = FALSE;
            }


        // ----------------------------
        // Cypher API 3 - This won't exist for earlier DLLs
        // ----------------------------

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3));
        DLLDetails->FnIdentifySupported_v3 = (PCypherDLLFnIdentifySupported_v3)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3
                                        );
        if (DLLDetails->FnIdentifySupported_v3 == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address - assuming this is an old DLL which doesn\'t support this API \n")));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3));
        DLLDetails->FnGetCypherDetails_v3 = (PCypherDLLFnGetCypherDetails_v3)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3
                                        );
        if (DLLDetails->FnGetCypherDetails_v3 == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address\n")));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPTSECTOR));
        DLLDetails->FnEncryptSector = (PCypherDLLFnEncryptSector)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPTSECTOR
                                        );
        if (DLLDetails->FnEncryptSector == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address - assuming this is an old DLL which doesn\'t support this API \n")));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII));
        DLLDetails->FnEncryptSectorWithASCII = (PCypherDLLFnEncryptSectorWithASCII)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII
                                        );
        if (DLLDetails->FnEncryptSectorWithASCII == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address - assuming this is an old DLL which doesn\'t support this API \n")));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPTSECTOR));
        DLLDetails->FnDecryptSector = (PCypherDLLFnDecryptSector)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPTSECTOR
                                        );
        if (DLLDetails->FnDecryptSector == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address - assuming this is an old DLL which doesn\'t support this API \n")));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Getting proc address for: %ls\n"), DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII));
        DLLDetails->FnDecryptSectorWithASCII = (PCypherDLLFnDecryptSectorWithASCII)GetProcAddress(
                                         DLLDetails->Lib,
                                         DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII
                                        );
        if (DLLDetails->FnDecryptSectorWithASCII == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("FAILED to get proc address - assuming this is an old DLL which doesn\'t support this API \n")));
            }

        // Sanity check; if any of the "sector" encrypt/decrypt functions
        // couldn't be obtained - but at least one of them has, fail.
        if (
            (
             (DLLDetails->FnIdentifySupported_v3 == NULL) ||
             (DLLDetails->FnGetCypherDetails_v3 == NULL) ||
             (DLLDetails->FnEncryptSector == NULL) ||
             (DLLDetails->FnEncryptSectorWithASCII == NULL) ||
             (DLLDetails->FnDecryptSector == NULL) ||
             (DLLDetails->FnDecryptSectorWithASCII == NULL) 
            ) &&
            (
             (DLLDetails->FnIdentifySupported_v3 != NULL) ||
             (DLLDetails->FnGetCypherDetails_v3 != NULL) ||
             (DLLDetails->FnEncryptSector != NULL) ||
             (DLLDetails->FnEncryptSectorWithASCII != NULL) ||
             (DLLDetails->FnDecryptSector != NULL) ||
             (DLLDetails->FnDecryptSectorWithASCII != NULL) 
            ) 
           )
            {
            DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get proc address for one of the sector based encryption/decryption operations, but did get another of them - something's wrong! This API appears half supported!\n")));
            retval = FALSE;
            }
        }


    // In case of failure, shutdown any loaded lib, overwrite, and free off...
    if (!(retval))
        {
        driver_CypherUnloadDLL(DLLDetails);
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("driver_CypherLoadDLL\n")));
    return retval;
}


// =========================================================================
void driver_CypherUnloadDLL(DLL_DETAILS_CYPHER* DLLDetails)
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("driver_CypherUnloadDLL\n")));

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

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("driver_CypherUnloadDLL\n")));
}


// =========================================================================
// Attempt to obtain cypher driver details using v3 API, falling back to v1
// on failure
BOOL driver_CypherGetDriverDetails(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v3* driverInfo
)
{
    BOOL retval = FALSE;
    CYPHER_DRIVER_INFO_v1 cypherDriverInfo_v1;

    retval = _driver_CypherGetDriverDetails_v3(CypherFilename, driverInfo);
    if (!(retval))
        {
        retval = _driver_CypherGetDriverDetails_v1(
                                                  CypherFilename,
                                                  &cypherDriverInfo_v1
                                                 );
        if (retval)
            {
            // Move into v3 structure
            CopyCYPHER_DRIVER_INFO_v1ToCYPHER_DRIVER_INFO_v3(
                &cypherDriverInfo_v1,
                driverInfo
                );
            }

        _driver_CypherFreeDriverDetails_v1(&cypherDriverInfo_v1);
        }

    return retval;
}

// =========================================================================
// malloc a CYPHER_DRIVER_INFO structure, and populate as appropriate; returning
// a pointer to the structure in driverInfo
BOOL _driver_CypherGetDriverDetails_v1(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v1* driverInfo
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_CYPHER DLLDetails;
    DIOC_CYPHER_IDENTIFYDRIVER identifyBuf;
    DWORD supportedBufSize;
    DIOC_CYPHER_IDENTIFYSUPPORTED_v1* supportedBuf;
    CYPHER_v1* cypherArray;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CypherGetDriverDetails_v1\n")));

    if (driver_CypherLoadDLL(CypherFilename, &DLLDetails))
        {
        if (DLLDetails.FnIdentifyDriver(&identifyBuf) != ERROR_SUCCESS)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifyDriver FAILED!")));
            }
        else
            {
            supportedBufSize = (
                                sizeof(*supportedBuf) -
                                sizeof(*cypherArray) +                                 
                                (sizeof(*cypherArray) * identifyBuf.CountCyphers)
                               );
            supportedBuf = malloc(supportedBufSize);
            if (supportedBuf == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCBuffer FnIdentifySupported call\n")));
                }
            else
                {
                if (DLLDetails.FnIdentifySupported_v1(
                                                   supportedBufSize, 
                                                   supportedBuf
                                                  ) != ERROR_SUCCESS)
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifySupported FAILED!")));
                    }
                else
                    {
                    cypherArray = calloc(
                                         supportedBuf->BufCount, 
                                         sizeof(*cypherArray)
                                        );
                    if (cypherArray == NULL)
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
                        driverInfo->CypherCount = identifyBuf.CountCyphers;

                        memcpy(
                               cypherArray, 
                               supportedBuf->Cyphers,
                               (supportedBuf->BufCount * sizeof(*cypherArray))
                              );

                        driverInfo->CypherDetails = cypherArray;

                        retval = TRUE;
                        }
                    }

                SecZeroAndFreeMemory(supportedBuf, supportedBufSize);
                }

            SecZeroMemory(&identifyBuf, sizeof(identifyBuf));
            }

        driver_CypherUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CypherGetDriverDetails_v1\n")));
    return retval;
}

// =========================================================================
// malloc a CYPHER_DRIVER_INFO structure, and populate as appropriate; returning
// a pointer to the structure in driverInfo
BOOL _driver_CypherGetDriverDetails_v3(
    WCHAR* CypherFilename, 
    CYPHER_DRIVER_INFO_v3* driverInfo
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_CYPHER DLLDetails;
    DIOC_CYPHER_IDENTIFYDRIVER identifyBuf;
    DWORD supportedBufSize;
    DIOC_CYPHER_IDENTIFYSUPPORTED_v3* supportedBuf;
    CYPHER_v3* cypherArray;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_CypherGetDriverDetails_v3\n")));

    if (driver_CypherLoadDLL(CypherFilename, &DLLDetails))
        {
        if (DLLDetails.FnIdentifySupported_v3 == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("v3 API not supported")));
            }
        else if (DLLDetails.FnIdentifyDriver(&identifyBuf) != ERROR_SUCCESS)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifyDriver FAILED!")));
            }
        else
            {
            supportedBufSize = (
                                sizeof(*supportedBuf) -
                                sizeof(*cypherArray) +                                 
                                (sizeof(*cypherArray) * identifyBuf.CountCyphers)
                               );
            supportedBuf = malloc(supportedBufSize);
            if (supportedBuf == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOCBuffer FnIdentifySupported call\n")));
                }
            else
                {
                if (DLLDetails.FnIdentifySupported_v3(
                                                   supportedBufSize, 
                                                   supportedBuf
                                                  ) != ERROR_SUCCESS)
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Call to FnIdentifySupported FAILED!")));
                    }
                else
                    {
                    cypherArray = calloc(
                                         supportedBuf->BufCount, 
                                         sizeof(*cypherArray)
                                        );
                    if (cypherArray == NULL)
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
                        driverInfo->CypherCount = identifyBuf.CountCyphers;

                        memcpy(
                               cypherArray, 
                               supportedBuf->Cyphers,
                               (supportedBuf->BufCount * sizeof(*cypherArray))
                              );

                        driverInfo->CypherDetails = cypherArray;

                        retval = TRUE;
                        }
                    }

                SecZeroAndFreeMemory(supportedBuf, supportedBufSize);
                }

            SecZeroMemory(&identifyBuf, sizeof(identifyBuf));
            }

        driver_CypherUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_CypherGetDriverDetails_v3\n")));
    return retval;
}


// =========================================================================
void driver_CypherFreeDriverDetails(CYPHER_DRIVER_INFO_v3* driverInfo)
{
    _driver_CypherFreeDriverDetails_v3(driverInfo);
}

// =========================================================================
void _driver_CypherFreeDriverDetails_v1(CYPHER_DRIVER_INFO_v1* driverInfo)
{
    if (driverInfo != NULL)
        {
        SecZeroMemory(driverInfo, sizeof(*driverInfo));
        SecZeroAndFreeMemory(
                             driverInfo->CypherDetails, 
                             (driverInfo->CypherCount * sizeof(CYPHER_v1))
                            );
        }
}

// =========================================================================
void _driver_CypherFreeDriverDetails_v3(CYPHER_DRIVER_INFO_v3* driverInfo)
{
    if (driverInfo != NULL)
        {
        SecZeroMemory(driverInfo, sizeof(*driverInfo));
        SecZeroAndFreeMemory(
                             driverInfo->CypherDetails, 
                             (driverInfo->CypherCount * sizeof(CYPHER_v3))
                            );
        }
}

// =========================================================================
// EncryptNotDecrypt - Set to TRUE to encrypt, FALSE to decrypt
BOOL _driver_CypherEncryptDecryptSectorData(
    BOOL EncryptNotDecrypt,
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int BufferSize, // In bytes
    FREEOTFEBYTE* BufferIn,
    FREEOTFEBYTE* BufferOut
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_CYPHER DLLDetails;
    DIOC_CYPHER_SECTOR_DATA_IN* DIOCIn;
    unsigned int DIOCInSize;    
    FREEOTFEBYTE* ptrKey;
    FREEOTFEBYTE* ptrIV;
    FREEOTFEBYTE* ptrData;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_CypherEncryptDecryptSectorData\n")));

    if (driver_CypherLoadDLL(CypherDriverFilename, &DLLDetails))
        {
        if (
            (DLLDetails.FnEncryptSector == NULL) ||
            (DLLDetails.FnDecryptSector == NULL)
           )
            {
            // v3 API not present; fallback to v1 API (no sector information)
            retval = _driver_CypherEncryptDecryptData(
                                                    EncryptNotDecrypt,
                                                    CypherDriverFilename,
                                                    CypherGUID,
                                                    KeySize, // In *bits*
                                                    Key,
                                                    IVSize, // In *bits*
                                                    IV,
                                                    BufferSize, // In bytes
                                                    BufferIn,
                                                    BufferOut
                                                   );
            }
        else
            {
            DIOCInSize = sizeof(*DIOCIn) - 
                         sizeof(DIOCIn->Key) - 
                         sizeof(DIOCIn->IV) - 
                         sizeof(DIOCIn->Data) +
                         (KeySize / 8) +
                         (IVSize / 8) + 
                         BufferSize;

            DIOCIn = malloc(DIOCInSize);
            if (DIOCIn == NULL) 
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOC in buffer\n")));
                }
            else
                {
                DIOCIn->CypherGUID = CypherGUID;
                DIOCIn->SectorID = SectorID;
                DIOCIn->SectorSize = SectorSize;
                DIOCIn->KeyLength = KeySize;
                DIOCIn->IVLength = IVSize;
                DIOCIn->DataLength = BufferSize;

                ptrKey  = DIOCIn->Key;
                ptrIV   = ptrKey + (KeySize / 8);
                ptrData = ptrIV + (IVSize / 8);

                memcpy(ptrKey, Key, (KeySize / 8));
                memcpy(ptrIV, IV, (IVSize / 8));
                memcpy(ptrData, BufferIn, BufferSize);
                    
                if (EncryptNotDecrypt)
                    {
                    retval = (DLLDetails.FnEncryptSector(
                                                   DIOCInSize,
                                                   DIOCIn,
                                                   BufferSize,
                                                   (DIOC_CYPHER_DATA_OUT*)BufferOut
                                                  ) == ERROR_SUCCESS);
                    }
                else
                    {
                    retval = (DLLDetails.FnDecryptSector(
                                                   DIOCInSize,
                                                   DIOCIn,
                                                   BufferSize,
                                                   (DIOC_CYPHER_DATA_OUT*)BufferOut
                                                  ) == ERROR_SUCCESS);
                    }

                SecZeroAndFreeMemory(DIOCIn, DIOCInSize);
                }
            }

        driver_CypherUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_CypherEncryptDecryptSectorData\n")));
    return retval;
}


// =========================================================================
// EncryptNotDecrypt - Set to TRUE to encrypt, FALSE to decrypt
BOOL _driver_CypherEncryptDecryptData(
    BOOL EncryptNotDecrypt,
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int BufferSize, // In bytes
    FREEOTFEBYTE* BufferIn,
    FREEOTFEBYTE* BufferOut
)
{
    BOOL retval = FALSE;
    DLL_DETAILS_CYPHER DLLDetails;
    DIOC_CYPHER_DATA_IN* DIOCIn;
    unsigned int DIOCInSize;    
    FREEOTFEBYTE* ptrKey;
    FREEOTFEBYTE* ptrIV;
    FREEOTFEBYTE* ptrData;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_driver_CypherEncryptDecryptData\n")));

    if (driver_CypherLoadDLL(CypherDriverFilename, &DLLDetails))
        {
        DIOCInSize = sizeof(*DIOCIn) - 
                     sizeof(DIOCIn->Key) - 
                     sizeof(DIOCIn->IV) - 
                     sizeof(DIOCIn->Data) +
                     (KeySize / 8) +
                     (IVSize / 8) + 
                     BufferSize;

        DIOCIn = malloc(DIOCInSize);
        if (DIOCIn == NULL) 
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc DIOC in buffer\n")));
            }
        else
            {
            DIOCIn->CypherGUID = CypherGUID;
            DIOCIn->KeyLength = KeySize;
            DIOCIn->IVLength = IVSize;
            DIOCIn->DataLength = BufferSize;

            ptrKey  = DIOCIn->Key;
            ptrIV   = ptrKey + (KeySize / 8);
            ptrData = ptrIV + (IVSize / 8);

            memcpy(ptrKey, Key, (KeySize / 8));
            memcpy(ptrIV, IV, (IVSize / 8));
            memcpy(ptrData, BufferIn, BufferSize);
                
            if (EncryptNotDecrypt)
                {
                retval = (DLLDetails.FnEncrypt(
                                               DIOCInSize,
                                               DIOCIn,
                                               BufferSize,
                                               (DIOC_CYPHER_DATA_OUT*)BufferOut
                                              ) == ERROR_SUCCESS);
                }
            else
                {
                retval = (DLLDetails.FnDecrypt(
                                               DIOCInSize,
                                               DIOCIn,
                                               BufferSize,
                                               (DIOC_CYPHER_DATA_OUT*)BufferOut
                                              ) == ERROR_SUCCESS);
                }

            SecZeroAndFreeMemory(DIOCIn, DIOCInSize);
            }

        driver_CypherUnloadDLL(&DLLDetails);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_driver_CypherEncryptDecryptData\n")));
    return retval;
}


// =========================================================================
BOOL _driver_CypherEncryptData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* plaintextBuffer,
    FREEOTFEBYTE* encryptedBuffer
)
{
    return _driver_CypherEncryptDecryptData(
                               TRUE,
                               CypherDriverFilename,
                               CypherGUID,
                               KeySize, // In *bits*
                               Key,
                               IVSize, // In *bits*
                               IV,
                               blockSize, // In bytes
                               plaintextBuffer,
                               encryptedBuffer
                              );
}


// =========================================================================
BOOL _driver_CypherDecryptData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* encryptedBuffer,
    FREEOTFEBYTE* plaintextBuffer
)
{
    return _driver_CypherEncryptDecryptData(
                               FALSE,
                               CypherDriverFilename,
                               CypherGUID,
                               KeySize, // In *bits*
                               Key,
                               IVSize, // In *bits*
                               IV,
                               blockSize, // In bytes
                               encryptedBuffer,
                               plaintextBuffer
                              );
}


// =========================================================================
BOOL driver_CypherEncryptSectorData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* plaintextBuffer,
    FREEOTFEBYTE* encryptedBuffer
)
{
    return _driver_CypherEncryptDecryptSectorData(
                               TRUE,
                               CypherDriverFilename,
                               CypherGUID,
                               SectorID,
                               SectorSize,
                               KeySize, // In *bits*
                               Key,
                               IVSize, // In *bits*
                               IV,
                               blockSize, // In bytes
                               plaintextBuffer,
                               encryptedBuffer
                              );
}


// =========================================================================
BOOL driver_CypherDecryptSectorData(
    WCHAR* CypherDriverFilename,
    GUID CypherGUID,
    LARGE_INTEGER SectorID,
    unsigned int SectorSize,
    unsigned int KeySize, // In *bits*
    FREEOTFEBYTE* Key,
    unsigned int IVSize, // In *bits*
    FREEOTFEBYTE* IV,
    unsigned int blockSize, // In bytes
    FREEOTFEBYTE* encryptedBuffer,
    FREEOTFEBYTE* plaintextBuffer
)
{
    return _driver_CypherEncryptDecryptSectorData(
                               FALSE,
                               CypherDriverFilename,
                               CypherGUID,
                               SectorID,
                               SectorSize,
                               KeySize, // In *bits*
                               Key,
                               IVSize, // In *bits*
                               IV,
                               blockSize, // In bytes
                               encryptedBuffer,
                               plaintextBuffer
                              );
}


// =========================================================================
void driver_CypherPrettyprintAlgTitle(
    CYPHER_v3* cypherInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    WCHAR strMode[10];  // Enough to hold any cypher mode as a string
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];

    // Convert straight ASCII title to UNICODE
    memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
    mbstowcs(
             tmpStrBuf, 
             cypherInfo->Title, 
             strlen(cypherInfo->Title)
            ); 

    // Convert cypher mode to UNICODE string representation
    memset(strMode, 0, sizeof(strMode));
    driver_CypherPrettyprintAlgMode(
                                    cypherInfo->Mode,
                                    strMode
                                   );

    // Prettyprint to buffer
    _snwprintf(
              buffer,
              (bufferSize / sizeof(buffer[0])),
              STR_FORMAT_FULL_CYPHER_TITLE,
              tmpStrBuf,
              cypherInfo->KeySizeUnderlying,
              strMode
             );

    SecZeroMemory(tmpStrBuf, sizeof(tmpStrBuf));
}


// =========================================================================
void driver_CypherPrettyprintAlgTechTitle(
    CYPHER_v3* cypherInfo, 
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{
    WCHAR strMode[10];  // Enough to hold any cypher mode as a string
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];

    // Convert straight ASCII title to UNICODE
    memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
    mbstowcs(
             tmpStrBuf, 
             cypherInfo->Title, 
             strlen(cypherInfo->Title)
            ); 

    // Convert cypher mode to UNICODE string representation
    memset(strMode, 0, sizeof(strMode));
    driver_CypherPrettyprintAlgMode(
                                    cypherInfo->Mode,
                                    strMode
                                   );

    // Prettyprint to buffer
    _snwprintf(
              buffer,
              (bufferSize / sizeof(buffer[0])),
              STR_FORMAT_FULL_CYPHER_TECH_TITLE,
              tmpStrBuf,
              strMode,
              cypherInfo->KeySizeUnderlying,
              cypherInfo->BlockSize,
              strMode
             );

    SecZeroMemory(tmpStrBuf, sizeof(tmpStrBuf));
}


// =========================================================================
BOOL driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    WCHAR* buffer,
    int bufferSize  // In bytes
)
{   
    BOOL retval = FALSE;
    CYPHER_v3 cypherInfo;

    if (driver_CypherGetImplDetails(
                        ImplDriverFilename,
                        &ImplGUID,
                        &cypherInfo
                        ))
        {
        driver_CypherPrettyprintAlgTechTitle(
            &cypherInfo,
            buffer,
            bufferSize
            );

        retval = TRUE;
        }

    SecZeroMemory(&cypherInfo, sizeof(cypherInfo));

    return retval;
}


// =========================================================================
BOOL driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID_char(
    WCHAR* ImplDriverFilename,
    GUID ImplGUID,
    char* buffer,
    int bufferSize  // In bytes
)
{   
    BOOL retval = FALSE;
    WCHAR tmpBuffer[MAX_PRETTYPRINTED_TITLE];

    if (driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID(
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
// !! IMPORTANT !!
// Buffer **MUST** be large enough to store mode as UNICODE string!
// (Obvious, but...)
void driver_CypherPrettyprintAlgMode(
    CYPHER_MODE mode,
    WCHAR* buffer
)
{
    switch (mode)
        {
        case CYPHER_MODE_NONE:
            {
            wcscpy(buffer, _("None"));
            break;
            }

        case CYPHER_MODE_ECB:
            {
            wcscpy(buffer, _("ECB"));
            break;
            }

        case CYPHER_MODE_CBC:
            {
            wcscpy(buffer, _("CBC"));
            break;
            }

        case CYPHER_MODE_LRW:
            {
            wcscpy(buffer, _("LRW"));
            break;
            }

        case CYPHER_MODE_XTS:
            {
            wcscpy(buffer, _("XTS"));
            break;
            }

        default:
            {
            wcscpy(buffer, STR_UNKNOWN);
            break;
            }
        }

}


// =========================================================================
BOOL
driver_CypherGetImplDetails(
    WCHAR* CypherFilename, 
    GUID* CypherImpl, 
    CYPHER_v3* ImplDetails
)
{
    BOOL retval = FALSE;
    CYPHER_DRIVER_INFO_v3 cypherInfo;
    unsigned int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CypherGetImplDetails\n")));

    if (driver_CypherGetDriverDetails(
                                    CypherFilename,
                                    &cypherInfo
                                   ))
        {
        for (i = 0; i < cypherInfo.CypherCount; i++)
            {
            if (IsEqualGUID(
                            CypherImpl,
                            &(cypherInfo.CypherDetails[i].CypherGUID)
                           ))
                {
                *ImplDetails = cypherInfo.CypherDetails[i];
                retval = TRUE;
                break;
                }
            }

        driver_CypherFreeDriverDetails(&cypherInfo);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CypherGetImplDetails\n")));
    return retval;
}


// =========================================================================
BOOL driver_CypherLoadDLLCached(WCHAR* Filename, DLL_DETAILS_CYPHER* DLLDetails)
{
    DLL_DETAILS_CYPHER* currItem;
    BOOL found;
    int cnt;
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CypherLoadDLLCached\n")));

    found = FALSE;

    cnt = SDULLCountItems(G_driver_CacheDLLDetailsCypher);
    for(i = 0; i < cnt; i++)
        {
        SDULLGetItem(G_driver_CacheDLLDetailsCypher, i, (void*)(&currItem));
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
            if (driver_CypherLoadDLL(Filename, currItem))
                {
                if (SDULLAddItem(&G_driver_CacheDLLDetailsCypher, (void*)currItem))
                    {
                    found = TRUE;
                    if (DLLDetails != NULL)
                        {
                        *DLLDetails = *currItem;
                        }
                    }
                else
                    {
                    driver_CypherUnloadDLL(currItem);
                    }
                }

            if (!(found))
                {
                SecZeroAndFreeMemory(currItem, sizeof(*currItem));
                }
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CypherLoadDLLCached\n")));
    return found;
}


// =========================================================================
void driver_CypherUnloadDLLsCached()
{
    DLL_DETAILS_CYPHER* currItem;
    int cnt;
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("driver_CypherUnloadDLLsCached\n")));

    cnt = SDULLCountItems(G_driver_CacheDLLDetailsCypher);
    for(i = 0; i < cnt; i++)
        {
        SDULLGetItem(G_driver_CacheDLLDetailsCypher, i, (void*)(&currItem));
        driver_CypherUnloadDLL(currItem);
        SecZeroAndFreeMemory(currItem, sizeof(*currItem));
        }

    // Pass FALSE here, as securely overwritten just above
    SDULLRemoveAllItems(&G_driver_CacheDLLDetailsCypher, FALSE);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("driver_CypherUnloadDLLsCached\n")));
}


// =========================================================================
// =========================================================================
