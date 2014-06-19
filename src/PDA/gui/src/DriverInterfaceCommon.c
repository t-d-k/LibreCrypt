// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "SDUGeneral.h" // Required for SDUParsePath

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "DriverInterfaceCommon.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "FreeOTFE4PDAAPI.h"
    

// =========================================================================
// Note: As well as returning the count of files found, the list of
//       filenames will also have a terminating NULL
// Note: This means that *regardless* of how many files are found, 
//       driver_FreeDriverFilenames must *always* be called to clean up
// Returns -1 on error
int driver_GetDriverFilenames(
    WCHAR* filenamePattern, 
    WCHAR*** filenamesList,
    LNKLIST_DISABLED_DRIVER* ExcludeFilenamesList
)
{
    int count;
    WIN32_FIND_DATA findRec;
    HANDLE hFind;
    BOOL fileFound;
    WCHAR* currFilename;
    WCHAR** tmpFilenamesList;
    WCHAR ourFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;
    WCHAR* mainDLLPath;
    int pathAndFilenameLen;

    DEBUGOUTDRV(DEBUGLEV_ENTER, (TEXT("driver_GetDriverFilenames\n")));

    // We search only the directory in which FreeOTFE.exe (this executable)
    // is located
    GetModuleFileName(
                      NULL, 
                      ourFilename, 
                      (sizeof(ourFilename) / sizeof(ourFilename[0]))
                     ); 
    SDUParsePath(
                 ourFilename, 
                 &uncMachine, 
                 &drive, 
                 &path, 
                 &filename, 
                 &fileExtension
                );

    // Overwrite the filename part with the search pattern (which 
    // includes the DLL subdir)
    wcscpy(filename, filenamePattern);

    count = 0;
    tmpFilenamesList = malloc(sizeof(*tmpFilenamesList));
    if (tmpFilenamesList == NULL)
        {
        DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for tmpFilenamesList\n")));
        count = -1;
        }
    else
        {
        tmpFilenamesList[count] = NULL;

        DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("Searching: %ls\n"), ourFilename));
        hFind = FindFirstFile(ourFilename, &findRec);
        if (hFind == INVALID_HANDLE_VALUE)
            {
            DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("FindFirstFile failed.\n")));
            }
        else
            {
            DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("FindFirstFile OK.\n")));
            fileFound = TRUE;
            while (fileFound)
                {
                if (
                    // Skip directories with matching names...
                    ((findRec.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0) &&
                    // And any on the excluded list...
                    (!(IsDriverDisabled(
                                        ExcludeFilenamesList, 
                                        findRec.cFileName
                                       )))
                   )
                    {
                    // FindFiles only returns the filenames, but we need the full
                    mainDLLPath = driver_AllocMainDLLFullPath(TRUE);
                    if (mainDLLPath == NULL)
                        {
                        DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to get main DLL path.\n")));
                        count = -1;
                        break;
                        }
                    else
                        {
                        pathAndFilenameLen = wcslen(mainDLLPath) + 
                                             wcslen(findRec.cFileName);
                    
                        // +1 to include terminating NULL
                        currFilename = calloc(
                                              (pathAndFilenameLen + 1), 
                                              sizeof(*currFilename)
                                             );
                        if (currFilename == NULL)
                            {
                            DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for DLL path & filename\n")));
                            count = -1;
                            break;
                            }
                        else
                            {
                            wcscpy(currFilename, mainDLLPath);
                            wcscat(currFilename, findRec.cFileName);
                        
                            count++;
                            // +1 in order to allow for an extra NULL entry on the end
                            tmpFilenamesList = realloc(
                                                       tmpFilenamesList,
                                                       ((count+1) * sizeof(*tmpFilenamesList))
                                                      );
                            if (tmpFilenamesList == NULL)
                                {
                                DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to REALLOC memory for tmpFilenamesList\n")));
                                count = -1;
                                break;
                                }
                            else{
                                DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("Found file: %ls\n"), findRec.cFileName));
                                DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("Incl path : %ls\n"), currFilename));

                                tmpFilenamesList[count-1] = currFilename;
                                tmpFilenamesList[count] = NULL;
                                }
                            }

                        // Path no longer needed
                        driver_FreeMainDLLFullPath(mainDLLPath);
                        }
                    }

                fileFound = FindNextFile(hFind, &findRec);
                }

            FindClose(hFind);
            }
        }

    *filenamesList = tmpFilenamesList;

    DEBUGOUTDRV(DEBUGLEV_EXIT, (TEXT("driver_GetDriverFilenames\n")));
    return count;
}


// =========================================================================
void driver_FreeDriverFilenames(WCHAR*** filenamesList)
{
    int count;
    WCHAR* fre;

    if (filenamesList != NULL)
        {
        count = 0;
        if (*filenamesList != NULL)
            {
            // Free off all the filenames...
            count = 0;
            while ((*filenamesList)[count] != NULL)
                {
                fre = (*filenamesList)[count];
                // DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("Freeing off (%d): %ls\n"), (count+1), fre));
                SecZeroAndFreeWCHARMemory(fre);
                count++;
                }

            // Finally, free the list off...
            SecZeroAndFreeMemory(**filenamesList, (count * sizeof(WCHAR*)));
            }
        *filenamesList = NULL;
        }
}


// =========================================================================
// Allocate memory for the
// Note: It it the caller's responsibility to free off the memory allocated
// Returns NULL on failure
WCHAR* driver_AllocMainDLLFullFilename(BOOL withPath)
{
    BOOL allOK;
    WCHAR* fullFilename;
    WCHAR tmpFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;

    DEBUGOUTDRV(DEBUGLEV_ENTER, (TEXT("driver_AllocAndGetMainDLLFullFilename\n")));

    fullFilename = NULL;
    allOK = FALSE;

    memset(tmpFilename, 0, sizeof(tmpFilename));
    if (withPath)
        {
        if (GetModuleFileName(NULL, tmpFilename, sizeof(tmpFilename)) != 0)
            {
            // Replace .exe filename with DLL filename
            SDUParsePath(
                         tmpFilename, 
                         &uncMachine,
                         &drive,
                         &path,
                         &filename,
                         &fileExtension
                        );
            if (filename != NULL)
                {
                wcscpy(filename, FREEOTFE_DLL_MAIN);
                allOK = TRUE;
                }
            }
        }
    else
        {
        wcscpy(tmpFilename, FREEOTFE_DLL_MAIN);
        allOK = TRUE;
        }
              
    if (!(allOK))
        {
        DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("FAILED to get the main DLL filename\n")));
        }
    else
        {
        // +1 for NULL terminator
        fullFilename = calloc((wcslen(tmpFilename) + 1), sizeof(*fullFilename));
        if (fullFilename == NULL)
            {
            DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc enough memory to store main driver path & filename\n")));
            }
        else
            {
            wcscpy(fullFilename, tmpFilename);
            DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("Main driver DLL: %ls\n"), fullFilename));
            }
        }

    DEBUGOUTDRV(DEBUGLEV_EXIT, (TEXT("driver_AllocAndGetMainDLLFullFilename\n")));
    return fullFilename;
}


// =========================================================================
void driver_FreeMainDLLFullFilename(WCHAR* fullFilename)
{
    WCHAR* tmpFilename;
    size_t len;
    
    // The user may have changed the filename previously returned (e.g. to
    // insert NULLs into the middle of it)
    // Because this means we can't find the size of the full filename,
    // we get it again, just to get it's size.
    tmpFilename = driver_AllocMainDLLFullFilename(FALSE);
    if (tmpFilename == NULL)
        {
        len = wcslen(fullFilename);
        }
    else
        {
        len = wcslen(tmpFilename);
        }
    SecZeroAndFreeMemory(fullFilename, (len * sizeof(*fullFilename)));

    len = max(len, wcslen(tmpFilename));

    SecZeroAndFreeWCHARMemory(tmpFilename);
}


// =========================================================================
// inclSlash - Set to TRUE to include a trailing "\" on the end of the path,
//             or FALSE for no trailing "\"
WCHAR* driver_AllocMainDLLFullPath(BOOL inclSlash)
{
    WCHAR* retval = NULL;
    WCHAR* mainDLLPathAndFilename;
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;
    int pathLen;

    mainDLLPathAndFilename = driver_AllocMainDLLFullFilename(TRUE);
    if (mainDLLPathAndFilename == NULL)
        {
        DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to get main DLL path & filename.\n")));
        }
    else
        {
        SDUParsePath(
                     mainDLLPathAndFilename, 
                     &uncMachine, 
                     &drive, 
                     &path, 
                     &filename, 
                     &fileExtension
                    );

        pathLen = wcslen(path) - wcslen(filename);
        // +1 for the terminating NULL
        retval = calloc((pathLen + 1), sizeof(*retval));
        if (retval == NULL)
            {
            DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for DLL path\n")));
            }
        else
            {
            // Initialise allocated memory
            memset(retval, 0, ((pathLen + 1) * sizeof(*retval)));

            // Truncate the main DLL path/filename, so we only copy the path
            wcsncpy(retval, path, pathLen);

            // Strip off any trailing slash, if needed, by replacing it with
            // a terminating NULL; or just set terminating NULL at end
            if (inclSlash)
                {
                retval[pathLen] = 0;
                }
            else
                {
                retval[(pathLen-1)] = 0;
                }

            }
        }
                
    return retval;
}


// =========================================================================
void driver_FreeMainDLLFullPath(WCHAR* fullPath)
{
    WCHAR* tmpPath;
    int len;
    
    // The user may have changed the filename previously returned (e.g. to
    // insert NULLs into the middle of it)
    // Because this means we can't find the size of the full filename,
    // we get it again, just to get it's size.
    tmpPath = driver_AllocMainDLLFullPath(FALSE);
    if (tmpPath == NULL)
        {
        len = wcslen(fullPath);
        }
    else
        {
        len = wcslen(tmpPath);
        }

    SecZeroAndFreeMemory(fullPath, (len * sizeof(*fullPath)));

    SecZeroAndFreeWCHARMemory(tmpPath);
}


// =========================================================================
// =========================================================================
