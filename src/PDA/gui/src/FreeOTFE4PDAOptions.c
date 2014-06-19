// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdlib.h>
#include <windows.h> // Required for INI read/write routines

#include "SDUGeneral.h"

#include "FreeOTFEDebug.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAOptions.h"
#include "FreeOTFEDriverConstsCommon.h"
#include "FreeOTFE4PDAAPI.h"

// =========================================================================
// Forward declarations...
BOOL itob(int Value);
int btoi(BOOL Value);


// =========================================================================
// Consts...

//#define EXE_MS_FILE_EXPLORER   TEXT("\\Windows\\MSFEXPLORE")
//#define EXE_MS_FILE_EXPLORER   TEXT("\\Windows\\FEXPLORE")
#define EXE_MS_FILE_EXPLORER  TEXT("FEXPLORE")

#define F4P_INI_SECTION  TEXT("FreeOTFE4PDA")

#define F4P_INI_CONFIGVERSION                TEXT("ConfigVersion")
#define   F4P_INIDFLT_CONFIGVERSION            2
#define F4P_INI_SAVESETTINGS                 TEXT("SaveSettings")
#define   F4P_INIDFLT_SAVESETTINGS             FALSE
#define F4P_INI_MNTPNTUSEVOLFILEMAME         TEXT("MountpointVolFilename")
#define   F4P_INIDFLT_MNTPNTUSEVOLFILEMAME     FALSE
#define F4P_INI_MNTPNTDEFAULT                TEXT("MountpointDefault")
#define   F4P_INIDFLT_MNTPNTDEFAULT            TEXT("FreeOTFE storage")
#define F4P_INI_EXPLORER                     TEXT("ExplorerExe")
#define   F4P_INIDFLT_EXPLORER                 EXE_MS_FILE_EXPLORER
#define F4P_INI_EXPLOREONMOUNT               TEXT("ExploreOnMount") 
#define   F4P_INIDFLT_EXPLOREONMOUNT           FALSE
#define F4P_INI_LARGEICONS                   TEXT("LargeIcons") 
#define   F4P_INIDFLT_LARGEICONS               TRUE
#define F4P_INI_SOFTKEY_MENUS                TEXT("SoftkeyMenus") 
#define   F4P_INIDFLT_SOFTKEY_MENUS            TRUE 
#define F4P_INI_REVERTVOLTIMESTAMPS          TEXT("RevertVolTimestamps") 
#define   F4P_INIDFLT_REVERTVOLTIMESTAMPS      FALSE
#define F4P_INI_LANGUAGECODE                 TEXT("LanguageCode")
#define   F4P_INIDFLT_LANGUAGECODE             TEXT("")

#define F4P_INI_SECTION_HASHES   TEXT("Hashes")
#define F4P_INI_SECTION_CYPHERS  TEXT("Cyphers")
#define F4P_INI_DISABLEDCOUNT                TEXT("DisabledCount") 
#define   F4P_INIDFLT_DISABLEDCOUNT            0


// =========================================================================
// Allocate memory for the INI filename
// Note: It it the caller's responsibility to free off the memory allocated
WCHAR* _AllocMainINIFullFilename()
{
    WCHAR* fullFilename;
    WCHAR tmpFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;

    DEBUGOUTDRV(DEBUGLEV_ENTER, (TEXT("_AllocMainINIFullFilename\n")));

    fullFilename = NULL;
    if (GetModuleFileName(NULL, tmpFilename, sizeof(tmpFilename)) != 0)
        {
        // Replace .exe filename with INI filename
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
            wcscpy(filename, FREEOTFE_INI);
                
            // +1 for NULL terminator
            fullFilename = calloc((wcslen(tmpFilename) + 1), sizeof(*fullFilename));
            if (fullFilename == NULL)
                {
                DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc enough memory to store INI path & filename\n")));
                }
            else
                {
                wcscpy(fullFilename, tmpFilename);
                DEBUGOUTDRV(DEBUGLEV_INFO, (TEXT("INI filename: %ls\n"), fullFilename));
                }
            }
        }

    DEBUGOUTDRV(DEBUGLEV_EXIT, (TEXT("_AllocMainINIFullFilename\n")));
    return fullFilename;
}


// =========================================================================
void _FreeMainINIFullFilename(WCHAR* fullFilename)
{
    WCHAR* tmpFilename;
    int len;
    
    // The user may have changed the filename previously returned (e.g. to
    // insert NULLs into the middle of it)
    // Because this means we can't find the size of the full filename,
    // we get it again, just to get it's size.
    tmpFilename = _AllocMainINIFullFilename();
    if (tmpFilename == NULL)
        {
        len = wcslen(fullFilename);
        }
    else
        {
        len = wcslen(tmpFilename);
        }

    SecZeroAndFreeMemory(fullFilename, (len * sizeof(*fullFilename)));

    SecZeroAndFreeWCHARMemory(tmpFilename);
}


// =========================================================================
#define MAX_INI_LINE  1024
BOOL GotoSection(FILE* hFile, WCHAR* Section)
{
    WCHAR line[MAX_INI_LINE];
    BOOL retval;

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("SEEK section: %s\n"), Section));

    retval = FALSE;

    if (hFile != NULL)
        {
        fseek(hFile, 0, SEEK_SET);
        while (!(feof(hFile)))
            {
            memset(line, 0, sizeof(line));
            fgetws(line, sizeof(line), hFile);
            if (wcsncmp(line, TEXT("["), 1) == 0)
                {
                // 1 because we skip the first "["
                if (wcsncmp(&(line[1]), Section, wcslen(Section)) == 0)
                    {
                    retval = TRUE;
                    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Section found.\n")));
                    break;
                    }
                }

            }
        }

    return retval;
}


// =========================================================================
// Note: It is the *callers* responsibility to free off "Value"
BOOL GetValueString(FILE* hFile, WCHAR* Name, WCHAR** Value)
{
    WCHAR line[MAX_INI_LINE];
    BOOL retval;
    fpos_t storedFilePos;
    WCHAR* readName;
    WCHAR* readValue;

    retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("READ Name: %s\n"), Name));

    if (hFile != NULL)
        {
        fgetpos(hFile, &storedFilePos);

        memset(line, 0, sizeof(line));
        while (
               (!(feof(hFile))) &&
               (wcsncmp(line, TEXT("["), 1) != 0)
              )
            {
            memset(line, 0, sizeof(line));
            fgetws(line, sizeof(line), hFile);

            readName = wcstok(line, TEXT("="));
            if (readName != NULL)
                {
                if (wcscmp(readName, Name) == 0)
                    {
                    readValue = wcstok(NULL, TEXT("\n"));
                    // Handle case if no value specified (empty string)
                    if (readValue == NULL)
                        {
                        readValue = (WCHAR*)SDU_EMPTY_STRINGW;
                        }
                    // +1 to include terminating NULL
                    *Value = calloc((wcslen(readValue) + 1), sizeof(*readValue));
                    if (*Value == NULL)
                        {
                        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc for INI value read in\n")));
                        }
                    else
                        {
                        wcscpy(*Value, readValue);
                        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("READ Value: %s\n"), *Value));
                        retval = TRUE;                
                        }
                    break;
                    }
                }
            }

        fsetpos(hFile, &storedFilePos);
        }

    return retval;
}


// =========================================================================
// Note: It is the *callers* responsibility to free off "Value"
void GetValueStringDflt(FILE* hFile, WCHAR* Name, WCHAR** Value, WCHAR* Default)
{
    BOOL retval;

    retval = GetValueString(hFile, Name, Value);
    if (!(retval))
        {
        // +1 to include terminating NULL
        *Value = calloc((wcslen(Default) + 1), sizeof(*Default));
        if (*Value == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc value for default.\n")));
            }
        else
            {
            wcscpy(*Value, Default);
            }
        }

}


// =========================================================================
BOOL GetValueInt(FILE* hFile, WCHAR* Name, int* Value)
{
    WCHAR* strValue;
    BOOL retval;

    *Value = 0;

    retval = GetValueString(hFile, Name, &strValue);

    if (retval)
        {
        *Value = _wtoi(strValue);
        SecZeroAndFreeWCHARMemory(strValue);
        }

    return retval;
}


// =========================================================================
// Note: It is the *callers* responsibility to free off "Value"
void GetValueIntDflt(FILE* hFile, WCHAR* Name, int* Value, int Default)
{
    BOOL retval;

    retval = GetValueInt(hFile, Name, Value);
    if (!(retval))
        {
        *Value = Default;
        }

}


// =========================================================================
BOOL GetValueBool(FILE* hFile, WCHAR* Name, BOOL* Value)
{
    int intValue;
    BOOL retval;

    *Value = FALSE;

    retval = GetValueInt(hFile, Name, &intValue);
    if (retval)
        {
        *Value = itob(intValue);
        }

    return retval;
}


// =========================================================================
BOOL GetValueBoolDflt(FILE* hFile, WCHAR* Name, BOOL* Value, BOOL Default)
{
    BOOL retval;

    retval = GetValueBool(hFile, Name, Value);
    if (!(retval))
        {
        *Value = Default;
        }

    return retval;
}


// =========================================================================
OPTIONS* OptionsCopy(OPTIONS* SrcOpts)
{
    OPTIONS* retval;
    WCHAR* tmpMntPntDefault;
    WCHAR* tmpExplorerExe;
    WCHAR* tmpLanguageCode;
    LNKLIST_DISABLED_DRIVER* tmpDisabledHashes;
    LNKLIST_DISABLED_DRIVER* tmpDisabledCyphers;

    retval = malloc(sizeof(*retval));

    // +1 to include terminating NULL
    tmpMntPntDefault = calloc(
                              (wcslen(SrcOpts->MntPntDefault) + 1),
                              sizeof(*tmpMntPntDefault)
                             );
    // +1 to include terminating NULL
    tmpExplorerExe = calloc(
                            (wcslen(SrcOpts->ExplorerExe) + 1),
                            sizeof(*tmpExplorerExe)
                           );
    // +1 to include terminating NULL
    tmpLanguageCode = calloc(
                            (wcslen(SrcOpts->LanguageCode) + 1),
                            sizeof(*tmpLanguageCode)
                           );

    tmpDisabledHashes  = DeepCopyDisabledDrivers(SrcOpts->DisabledHashes);
    tmpDisabledCyphers = DeepCopyDisabledDrivers(SrcOpts->DisabledCyphers);

    if (
        (retval == NULL) ||
        (tmpMntPntDefault == NULL) ||
        (tmpExplorerExe == NULL) ||
        (tmpLanguageCode == NULL)
       )
        {
        DEBUGOUTDRV(DEBUGLEV_ERROR, (TEXT("Unable to malloc enough memory to make copy of options\n")));
        SecZeroAndFreeMemory(retval, sizeof(*retval));

        SecZeroAndFreeMemory(
                             tmpMntPntDefault,
                             (wcslen(SrcOpts->MntPntDefault) * sizeof(*tmpMntPntDefault))
                            );
        SecZeroAndFreeMemory(
                             tmpExplorerExe,
                             (wcslen(SrcOpts->ExplorerExe) * sizeof(*tmpExplorerExe))
                            );
        SecZeroAndFreeMemory(
                             tmpLanguageCode,
                             (wcslen(SrcOpts->LanguageCode) * sizeof(*tmpLanguageCode))
                            );

        FreeAllDisabledDrivers(&tmpDisabledHashes);
        FreeAllDisabledDrivers(&tmpDisabledCyphers);
        }
    else
        {
        memcpy(retval, SrcOpts, sizeof(*SrcOpts));

        wcscpy(tmpMntPntDefault, SrcOpts->MntPntDefault);
        wcscpy(tmpExplorerExe, SrcOpts->ExplorerExe);
        wcscpy(tmpLanguageCode, SrcOpts->LanguageCode);
        retval->MntPntDefault = tmpMntPntDefault;
        retval->ExplorerExe = tmpExplorerExe;
        retval->LanguageCode = tmpLanguageCode;

        retval->DisabledHashes = tmpDisabledHashes;
        retval->DisabledCyphers = tmpDisabledCyphers;
        }

    return retval;
}


// =========================================================================
// Note: This will *not* free off "Opts"
void OptionsFree(OPTIONS* Opts)
{
    if (Opts != NULL)
        {
        SecZeroAndFreeWCHARMemory(Opts->MntPntDefault);
        SecZeroAndFreeWCHARMemory(Opts->ExplorerExe);
        SecZeroAndFreeWCHARMemory(Opts->LanguageCode);

        FreeAllDisabledDrivers(&(Opts->DisabledHashes));
        FreeAllDisabledDrivers(&(Opts->DisabledCyphers));

        SecZeroMemory(Opts, sizeof(*Opts));
        }
}


// =========================================================================
void ReadDisabledDrivers(
    FILE* hFile,
    WCHAR* Section,
    LNKLIST_DISABLED_DRIVER** DisabledDrivers
)
{
    int disabledCnt;
    int i;
    WCHAR* disabledFilename;
    WCHAR idxAsString[100];  // String to store string representation of index integer

    FreeAllDisabledDrivers(DisabledDrivers);
    if (GotoSection(hFile, Section))
        {
        GetValueIntDflt(
                        hFile,
                        F4P_INI_DISABLEDCOUNT,
                        &disabledCnt,
                        F4P_INIDFLT_DISABLEDCOUNT
                        );

        for (i = 1; i <= disabledCnt; i++)
            {
            _itow(i, (WCHAR*)&idxAsString, 10);
            GetValueStringDflt(
                            hFile,
                            idxAsString,
                            &disabledFilename,
                            TEXT("")
                            );

            AddDisabledDriver(DisabledDrivers, disabledFilename);
            SecZeroAndFreeWCHARMemory(disabledFilename);
            }
        }

}


OPTIONS* OptionsRead()
{
    OPTIONS* retval;
    FILE* hFile = NULL;
    WCHAR* iniFilename;
    BOOL allOK;

    allOK = TRUE;

    if (allOK)
        {
        retval = malloc(sizeof(*retval));
        allOK = (retval != NULL);
        }

    if (allOK)
        {
        iniFilename = _AllocMainINIFullFilename();
        if (iniFilename == NULL)
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        hFile = _wfopen(iniFilename, TEXT("r"));
        if (hFile == NULL)
            {
            allOK = FALSE;
            }
        }

    if (allOK)
        {
        allOK = GotoSection(hFile, F4P_INI_SECTION);
        }

    // NOTE: We *DO* call the GetValue...Dlft(...) in order to default
    //       the options

    if (retval != NULL)
        {
        GetValueIntDflt(
                        hFile,
                        F4P_INI_CONFIGVERSION,
                        &(retval->ConfigVersion),
                        F4P_INIDFLT_CONFIGVERSION
                       );

        GetValueBoolDflt(
                        hFile,
                        F4P_INI_MNTPNTUSEVOLFILEMAME,
                        &(retval->MntPntUseVolFilename),
                        F4P_INIDFLT_MNTPNTUSEVOLFILEMAME
                       );
        GetValueStringDflt(
                           hFile,
                           F4P_INI_MNTPNTDEFAULT,
                           &(retval->MntPntDefault),
                           F4P_INIDFLT_MNTPNTDEFAULT
                          );

        GetValueStringDflt(
                           hFile,
                           F4P_INI_EXPLORER,
                           &(retval->ExplorerExe),
                           F4P_INIDFLT_EXPLORER
                          );
        GetValueBoolDflt(
                        hFile,
                        F4P_INI_EXPLOREONMOUNT,
                        &(retval->ExploreOnMount),
                        F4P_INIDFLT_EXPLOREONMOUNT
                       );    

        GetValueBoolDflt(
                        hFile,
                        F4P_INI_LARGEICONS,
                        &(retval->LargeIcons),
                        F4P_INIDFLT_LARGEICONS
                       );    
        GetValueBoolDflt(
                        hFile,
                        F4P_INI_REVERTVOLTIMESTAMPS,
                        &(retval->RevertVolTimestamps),
                        F4P_INIDFLT_REVERTVOLTIMESTAMPS
                       );    

        GetValueStringDflt(
                           hFile,
                           F4P_INI_LANGUAGECODE,
                           &(retval->LanguageCode),
                           F4P_INIDFLT_LANGUAGECODE
                          );

        // If we're under WM5 and later, the default for using softkey menus
        // is TRUE
        // If we're under an earlier version, we force this to FALSE
        retval->SoftkeyMenus = (SDUGetOSVersion_PDA() >= 
                                        SDU_OS_VER_PDA_WM2005);
        if (retval->SoftkeyMenus)
            {
            GetValueBoolDflt(
                            hFile,
                            F4P_INI_SOFTKEY_MENUS,
                            &(retval->SoftkeyMenus),
                            F4P_INIDFLT_SOFTKEY_MENUS
                           );    
            }

        GetValueBoolDflt(
                        hFile,
                        F4P_INI_SAVESETTINGS,
                        &(retval->SaveSettings),
                        F4P_INIDFLT_SAVESETTINGS
                       );

        ReadDisabledDrivers(
                        hFile,
                        F4P_INI_SECTION_HASHES,
                        &(retval->DisabledHashes)
                        );
        ReadDisabledDrivers(
                        hFile,
                        F4P_INI_SECTION_CYPHERS,
                        &(retval->DisabledCyphers)
                        );
        }

    if (hFile != NULL)
        {
        fclose(hFile);
        }

    return retval;
}


// =========================================================================
// Convert BOOL into integer
// This function is to ensure that if the definition of TRUE/FALSE (i.e. 1/0)
// changes, the values stored within the config file can still be read/written
// correctly
int btoi(BOOL Value)
{
    int retval;

    retval = 0;
    if (Value)
        {
        retval = 1;
        }

    return retval;
}

// =========================================================================
// Convert integer into BOOL
// This function is to ensure that if the definition of TRUE/FALSE (i.e. 1/0)
// changes, the values stored within the config file can still be read/written
// correctly
BOOL itob(int Value)
{
    BOOL retval;

    retval = FALSE;
    if (Value == 1)
        {
        retval = TRUE;
        }

    return retval;
}


// =========================================================================
BOOL OptionsWrite(OPTIONS* Opts)
{
    BOOL retval;
    WCHAR* iniFilename;
    FILE* hFile;
    int idx;
    LNKLIST_DISABLED_DRIVER* currDisabledDriver;

    retval = TRUE;

    if (retval)
        {
        iniFilename = _AllocMainINIFullFilename();
        if (iniFilename == NULL)
            {
            retval = FALSE;
            }
        }


    if (!(Opts->SaveSettings))
        {
        DeleteFile(iniFilename);
        }
    else
        {
        hFile = _wfopen(iniFilename, TEXT("w"));
        if (hFile == NULL)
            {
            retval = FALSE;
            }

        if (retval)
            {
            // This is crude, but effective...
            fwprintf(hFile, TEXT("[%s]\n"), F4P_INI_SECTION);
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_CONFIGVERSION, F4P_INIDFLT_CONFIGVERSION);
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_SAVESETTINGS, btoi(Opts->SaveSettings));
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_MNTPNTUSEVOLFILEMAME, btoi(Opts->MntPntUseVolFilename));
            fwprintf(hFile, TEXT("%s=%s\n"), F4P_INI_MNTPNTDEFAULT, Opts->MntPntDefault);
            fwprintf(hFile, TEXT("%s=%s\n"), F4P_INI_EXPLORER, Opts->ExplorerExe);
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_EXPLOREONMOUNT, btoi(Opts->ExploreOnMount));
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_LARGEICONS, btoi(Opts->LargeIcons));
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_SOFTKEY_MENUS, btoi(Opts->SoftkeyMenus));
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_REVERTVOLTIMESTAMPS, btoi(Opts->RevertVolTimestamps));
            fwprintf(hFile, TEXT("%s=%s\n"), F4P_INI_LANGUAGECODE, Opts->LanguageCode);

            // Disabled hashes...
            fwprintf(hFile, TEXT("\n"));
            fwprintf(hFile, TEXT("[%s]\n"), F4P_INI_SECTION_HASHES);
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_DISABLEDCOUNT, CountDisabledDrivers(Opts->DisabledHashes));
            idx = 0;
            currDisabledDriver = Opts->DisabledHashes;
            while (currDisabledDriver != NULL)
                {
                idx++;
                fwprintf(hFile, TEXT("%d=%s\n"), idx, currDisabledDriver->Filename);
                currDisabledDriver = currDisabledDriver->Next;
                }

            // Disabled cyphers...
            fwprintf(hFile, TEXT("\n"));
            fwprintf(hFile, TEXT("[%s]\n"), F4P_INI_SECTION_CYPHERS);
            fwprintf(hFile, TEXT("%s=%d\n"), F4P_INI_DISABLEDCOUNT, CountDisabledDrivers(Opts->DisabledCyphers));
            idx = 0;
            currDisabledDriver = Opts->DisabledCyphers;
            while (currDisabledDriver != NULL)
                {
                idx++;
                fwprintf(hFile, TEXT("%d=%s\n"), idx, currDisabledDriver->Filename);
                currDisabledDriver = currDisabledDriver->Next;
                }

            }

        if (hFile != NULL)
            {
            fclose(hFile);
            }

        }

    _FreeMainINIFullFilename(iniFilename);

    return retval;
}


// =========================================================================

// =========================================================================
// =========================================================================

