// Description: Common FreeOTFE library functions
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFElib.h"
#include "FreeOTFEDebug.h"

#include <stdio.h>
#include "FreeOTFEPlatform.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAAPI.h"

#define FAILED_GETFILESIZE        0xFFFFFFFF

// =========================================================================
// Read the debug level from the specified file
// Returns: The debug level, or FREEOTFE_DEBUG_LEVEL_NOT_READ on failure
DWORD ReadDebugLevelFromFile(WCHAR* Filename)
{
    DWORD retval = FREEOTFE_DEBUG_LEVEL_NOT_READ;
    HANDLE fileHandle;
    CHAR strDebugLevel[10]; // 10 should be enough
    DWORD bytesTransferred;

    if (Filename != NULL)
        {
        // Open file and get debug level to use.
        fileHandle = CreateFile(
                                Filename,
                                GENERIC_READ,  
                                (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                                NULL,
                                OPEN_EXISTING, 
                                FILE_ATTRIBUTE_READONLY, 
                                NULL
                               ); 
        if (fileHandle == INVALID_HANDLE_VALUE) 
            { 
            DEBUGOUTMAINDRV(DEBUGLEV_WARN, (TEXT("No file containing debug level opened; using default level.\n")));
            } 
        else
            {
            memset(strDebugLevel, 0, sizeof(strDebugLevel));
            ReadFile( 
                     fileHandle,
                     strDebugLevel, 
                     (sizeof(*strDebugLevel) - 1), // -1 to exclude
                                                   // terminating NULL
                     &bytesTransferred, 
                     NULL
                    );
            if (bytesTransferred <= 0)
                {            
                DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("FAILURE: Debug level could not be read from file.\n")));
                }
            else
                {
                retval = atoi(strDebugLevel);
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Read debug level from file: %d\n"), retval));
                }

            CloseHandle(fileHandle);
            }

        }

    return retval;
}


// =========================================================================
// Determine the max size of a file
BOOL
GetMaxSizeFile(
    IN  HANDLE FileHandle,
    OUT PLARGE_INTEGER MaxSize
)
{
    BOOL retval = FALSE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("GetMaxSizeFile\n")));

    MaxSize->LowPart = GetFileSize( 
                                   FileHandle,
                                   (DWORD*)&(MaxSize->HighPart)
                                  ); 

    // Failure if return value is 0xFFFFFFFF and GetLastError returns a value
    // other than NO_ERROR
    if (!(
          (MaxSize->LowPart == FAILED_GETFILESIZE) &&
          (GetLastError() != NO_ERROR)
        ))
        {
        // If not error, then we're OK
        retval = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("GetMaxSizeFile\n")));
    return retval;
}


// =========================================================================
BOOL
GetFileSize_Filename(
    IN  WCHAR* Filename, 
    OUT PLARGE_INTEGER FileSize
)
{
    BOOL retval = FALSE;
    HANDLE hFile;

    hFile = CreateFile(
                        Filename,
                        0, // Query only  
                        (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                        NULL,
                        OPEN_EXISTING, 
                        FILE_ATTRIBUTE_NORMAL, 
                        NULL
                       ); 
    if (hFile == INVALID_HANDLE_VALUE) 
        { 
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("Unable to open file to get filesize\n")));
        // Do nothing; retval already set to FALSE
        } 
    else
        {
        retval = GetMaxSizeFile(hFile, FileSize);
        CloseHandle(hFile);
        }

    return retval;
}


// =========================================================================
BOOL GenerateRNGDataMSCryptoAPI(
    int bytesRequired,
    FREEOTFEBYTE* randomData
)
{
    BOOL allOK = FALSE;
    HCRYPTPROV hProv;


    if (CryptAcquireContext(
                            &hProv,
                            TEXT(""),
                            TEXT(""),
                            PROV_RSA_FULL,
                            CRYPT_VERIFYCONTEXT
                            ))
        {
        // Cleardown (just in case)...
        memset(randomData, 0, bytesRequired);

        allOK = CryptGenRandom(hProv, bytesRequired, randomData);

        CryptReleaseContext(hProv, 0);
        }

    return allOK;
}


// =========================================================================
// =========================================================================


