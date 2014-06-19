// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFElib.h"
#include "wndMain.h"
#include "wndUserRNG.h"
#include "dlgAbout.h"
#include "resource.h"
#include "SDUGeneral.h"
#include "dlgMount.h"
#include "dlgMountLUKS.h"
#include "dlgNewVolWizard.h"
#include "DriverInterface.h"
#include "SDUi18n.h"

#include <windows.h>
#include <commctrl.h>
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib

#define IDC_MAIN_LISTBOX 100

// Portable mode related command line parameters...
//#define CMDLINE_PORTABLE       TEXT("portable")
//#define CMDLINE_START          TEXT("start")
//#define CMDLINE_ON             TEXT("on")
//#define CMDLINE_STOP           TEXT("stop")
//#define CMDLINE_OFF            TEXT("off")
// Mount related command line parameters...
#define CMDLINE_MOUNT          TEXT("mount")
//#define CMDLINE_FREEOTFE       TEXT("freeotfe")
//#define CMDLINE_LINUX          TEXT("linux")
#define CMDLINE_VOLUME         TEXT("volume")
#define CMDLINE_KEYFILE        TEXT("keyfile")
#define CMDLINE_PASSWORD       TEXT("password")
#define CMDLINE_READONLY       TEXT("readonly")
#define CMDLINE_OFFSET         TEXT("offset")
#define CMDLINE_NOCDBATOFFSET  TEXT("nocdbatoffset")
#define CMDLINE_KEYITERATIONS  TEXT("keyiterations")
#define CMDLINE_SALTLENGTH     TEXT("saltlength")
#define CMDLINE_MOUNTPOINT     TEXT("mountpoint")
#define CMDLINE_SILENT         TEXT("silent")
#define CMDLINE_FREEOTFE       TEXT("freeotfe")
#define CMDLINE_LINUX          TEXT("linux")
// Dismount related command line parameters...
#define CMDLINE_DISMOUNT       TEXT("dismount")
#define CMDLINE_FORCE          TEXT("force")
#define CMDLINE_ALL            TEXT("all")
// Other...
#define CMDLINE_CREATE         TEXT("create")
#define CMDLINE_NOEXIT         TEXT("noexit")


#define CMDLINE_SUCCESS                               0
#define CMDLINE_EXIT_INVALID_CMDLINE                100
//#define CMDLINE_EXIT_UNABLE_TO_CONNECT              101
#define CMDLINE_EXIT_UNABLE_TO_MOUNT                102
#define CMDLINE_EXIT_UNABLE_TO_DISMOUNT             103
//#define CMDLINE_EXIT_UNABLE_TO_START_PORTABLE_MODE  104
//#define CMDLINE_EXIT_UNABLE_TO_STOP_PORTABLE_MODE   105

// =========================================================================
BOOL InitApp(HINSTANCE hInstance)
{
    BOOL retval = TRUE;

    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Test number: %d\n"), 101));

    if (retval)
        {
        retval = WndMainRegister();
        }

    if (retval)
        {
        retval = WndUserRNGRegister();
        }

    return retval;
}


// =========================================================================
BOOL InitInstance(HINSTANCE hInstance, int iCmdShow)
{
    G_hWndMain = WndMainCreate();
    return (G_hWndMain != NULL);
}


// =========================================================================
void main_BadCommandLine()
{
    MsgError(NULL, _("Please check your command line parameters"));
}


// =========================================================================
LARGE_INTEGER main_CommandLineParameter_Name_AsNumber(
    WCHAR* Name,
    int Default
)
{
    LARGE_INTEGER retval;
    WCHAR* value;

    retval.QuadPart = Default;
    value = SDUCommandLineParameter_Name(Name);
    if (value != NULL)
        {
        retval.QuadPart = _wtoi64(value);
        SecZeroAndFreeWCHARMemory(value);
        }

    return retval;
}


// =========================================================================
// Returns -1 if there was no command line parameters, otherwise an exit value
int main_ProcessCmdLine()
{
    int retval;
    WCHAR* dismountMountpoint;
    BOOL forceDismount;
    BOOL dismountAll;
    WCHAR* mountVolume;
    BOOL readOnly;
    BOOL silent;
    BOOL linux;
    WCHAR* keyfile;
    WCHAR* password;
    WCHAR* volume;
    WCHAR* useMountpoint;
    BOOL noCDBAtOffset;
    LARGE_INTEGER keyiterations;
    LARGE_INTEGER offset;
    LARGE_INTEGER saltlength;
    LARGE_INTEGER sizelimit;

    retval = -1;

    // Check for create...
    if (SDUCommandLineSwitch(CMDLINE_CREATE))
        {
        retval = CMDLINE_SUCCESS;
        DisplayDlgNewVolWizard(NULL);
        }

    // Check for dismount...
    if (SDUCommandLineSwitch(CMDLINE_DISMOUNT))
        {
        retval = CMDLINE_EXIT_UNABLE_TO_DISMOUNT;

        dismountMountpoint = SDUCommandLineParameter_Name(CMDLINE_DISMOUNT);
        if (dismountMountpoint == NULL)
            {
            main_BadCommandLine();
            retval = CMDLINE_EXIT_INVALID_CMDLINE;
            }
        else
            {
            // Dismount...
            forceDismount = SDUCommandLineSwitch(CMDLINE_FORCE);
            dismountAll = (wcscmp(dismountMountpoint, CMDLINE_ALL) == 0);            

            if (dismountAll)
                {
                if (driver_DismountAll(forceDismount))
                    {
                    retval = CMDLINE_SUCCESS;
                    }
                }
            else
                {
                // Remove any leading "\" the user may have inserted
                useMountpoint = dismountMountpoint;
                if (useMountpoint == NULL)
                    {
                    retval = CMDLINE_EXIT_INVALID_CMDLINE;
                    }
                else
                    {
                    if (useMountpoint[0] == '\\')
                        {
                        useMountpoint = &(dismountMountpoint[1]);
                        }

                    if (driver_Dismount(useMountpoint, forceDismount))
                        {
                        retval = CMDLINE_SUCCESS;
                        }
                    }
                }

            SecZeroAndFreeWCHARMemory(dismountMountpoint);
            }

        }

    // Check for mount...
    if (SDUCommandLineSwitch(CMDLINE_MOUNT))
        {
        retval = CMDLINE_EXIT_UNABLE_TO_MOUNT;

        volume = SDUCommandLineParameter_Name(CMDLINE_VOLUME);
        if (volume == NULL)
            {
            main_BadCommandLine();
            retval = CMDLINE_EXIT_INVALID_CMDLINE;
            }
        else if (wcslen(volume) <= 0)
            {
            main_BadCommandLine();
            retval = CMDLINE_EXIT_INVALID_CMDLINE;
            }
        else
            {
            // Mount...        
            readOnly = SDUCommandLineSwitch(CMDLINE_READONLY);
            silent = SDUCommandLineSwitch(CMDLINE_SILENT);
            linux = SDUCommandLineSwitch(CMDLINE_LINUX);
            keyfile = SDUCommandLineParameter_Name(CMDLINE_KEYFILE);
            password = SDUCommandLineParameter_Name(CMDLINE_PASSWORD);

            noCDBAtOffset = SDUCommandLineSwitch(CMDLINE_NOCDBATOFFSET);
            offset = main_CommandLineParameter_Name_AsNumber(
                                                             CMDLINE_OFFSET,
                                                             DEFAULT_OFFSET
                                                            );
            keyiterations = main_CommandLineParameter_Name_AsNumber(
                                                             CMDLINE_KEYITERATIONS,
                                                             DEFAULT_KEYITERATIONS
                                                            );
            saltlength = main_CommandLineParameter_Name_AsNumber(
                                                             CMDLINE_SALTLENGTH,
                                                             DEFAULT_SALTLENGTH
                                                            );

            mountVolume = SDUCommandLineParameter_Name(CMDLINE_MOUNTPOINT);
            // Remove any leading "\" the user may have inserted
            useMountpoint = mountVolume;
            if (useMountpoint != NULL)
                {
                if (useMountpoint[0] == '\\')
                    {
                    useMountpoint = &(mountVolume[1]);
                    }
                }

            if (linux)
                {
                sizelimit.QuadPart = DEFAULT_SIZELIMIT;
                DisplayDlgMountLUKS_Params(
                                       NULL,
                                       useMountpoint,
                                       volume,
                                       password,
                                       readOnly,
                                       DEFAULT_LUKS_BASEICCYPHERONHASHLENGTH,
                                       sizelimit,
                                       silent
                                      );
                }
            else
                {
                DisplayDlgMount_Params(
                                       NULL,
                                       useMountpoint,
                                       volume,
                                       keyfile,
                                       offset,
                                       (!(noCDBAtOffset)),
                                       password,
                                       saltlength.LowPart,  // In *bits*
                                       keyiterations.LowPart,
                                       readOnly,
                                       silent
                                      );
                }

            // The user will either have cancelled, or it'll have mounted
            // successfully; either way the user will already know
            retval = CMDLINE_SUCCESS;

            SecZeroAndFreeWCHARMemory(mountVolume);
            SecZeroAndFreeWCHARMemory(keyfile);
            SecZeroAndFreeWCHARMemory(password);
            }

        }

    if (SDUCommandLineSwitch(CMDLINE_NOEXIT))
        {
        retval = -1;
        }

    return retval;
}


// =========================================================================
int WINAPI WinMain(
                   HINSTANCE hInstance, 
                   HINSTANCE hPrevInstance,
                   LPWSTR lpCmdLine, 
                   int nCmdShow
                  )
{
    MSG msg;
    HWND hWndExisting = NULL;
    int exitValue;

#if DBG
    FreeOTFEDebugLevel = 
                        DEBUGLEV_ERROR |
                        DEBUGLEV_WARN  |
                        DEBUGLEV_INFO  |
                        DEBUGLEV_ENTER |
                        DEBUGLEV_EXIT;
    //FreeOTFEDebugLevel = 0;
#endif
    
    G_hInstance = hInstance;

    G_Options = OptionsRead();
    G_driver_DisabledHashDrivers   = G_Options->DisabledHashes;
    G_driver_DisabledCypherDrivers = G_Options->DisabledCyphers;

    SDUi18n_Init();
    if (wcslen(G_Options->LanguageCode) > 0)
        {
        SDUi18n_Load_CodeW(G_Options->LanguageCode);
        }

    if (hPrevInstance == NULL)
        {
        if (!(InitApp(hInstance)))
            { 
            MsgError(NULL, TEXT("Unable to InitApp"));
            return FALSE; 
            }
        }

    // Check for command line options
    exitValue = main_ProcessCmdLine();

    if (exitValue == -1)
        {
        // If a copy of this application is already running, bring it to the top,
        // and exit
        hWndExisting = FindWindow(APP_TITLE, APP_TITLE);  
        if (hWndExisting != NULL) 
            {
            SetForegroundWindow(hWndExisting);    
            return TRUE;
            }

        if (!(InitInstance(hInstance, nCmdShow)))
            {
            MsgError(NULL, TEXT("Unable to InitInstance"));
            return FALSE;
            }

        // In case "/noexit" was used, and a previous was shown (e.g. mount, 
        // from command line parameters), bring the main window to the front
        SetForegroundWindow(G_hWndMain);             

        while (GetMessage(&msg, NULL, 0, 0))
            {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
            }

        exitValue = (int)(msg.wParam);
        }

    OptionsFree(G_Options);

    return exitValue;
}


// =========================================================================
void ExploreMountpoint(WCHAR* Mountpoint)
{
    WCHAR useMountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Explore mountpoint: %ls\n"), Mountpoint));
    
    // Prefix mountpoint with a "\"
    // Quoting the mountpoint appears to cause FEXPLORE (the default File
    // Explorer which comes with Windows Mobile) to fail and complain that
    // a bad URL has been used(?!)
    _snwprintf(
               useMountpoint, 
               (sizeof(useMountpoint) / sizeof(useMountpoint[0])),
               TEXT("\\%s"),
               Mountpoint
              );

    CreateProcess(
                  G_Options->ExplorerExe,
                  useMountpoint,

                  NULL,
                  NULL,
                  FALSE,
                  0,
                  NULL,
                  NULL,
                  NULL,
                  NULL
                 );

}


// =========================================================================
WCHAR* _FileFilterGenerate_Add(
    WCHAR* ExistingFilter, 
    const WCHAR* FilterType, 
    const WCHAR* Filter
)
{
    WCHAR* newFilter;
    int existingFilterWCharCnt;
    int newFilterWCharCnt;
    WCHAR* ptrIntoExisting;

    ptrIntoExisting = ExistingFilter;
    existingFilterWCharCnt = 0;
    if (ptrIntoExisting != NULL)
        {
        while (wcslen(ptrIntoExisting) > 0)
            {
            // Filter description...
            existingFilterWCharCnt = existingFilterWCharCnt + wcslen(ptrIntoExisting);
            ptrIntoExisting = &(ptrIntoExisting[(wcslen(ptrIntoExisting) + 1)]);
            // It's terminating NULL
            existingFilterWCharCnt = existingFilterWCharCnt + 1;
            // Filter description...
            existingFilterWCharCnt = existingFilterWCharCnt + wcslen(ptrIntoExisting);
            ptrIntoExisting = &(ptrIntoExisting[(wcslen(ptrIntoExisting) + 1)]);
            // It's terminating NULL
            existingFilterWCharCnt = existingFilterWCharCnt + 1;
            }

        }

    newFilterWCharCnt = existingFilterWCharCnt +
                        wcslen(FilterType) + 1 +
                        wcslen(Filter) + 1 + 
                        2; // The NULL-NULL terminator

    newFilter = calloc(newFilterWCharCnt, sizeof(*newFilter));
    // Sanity check...
    if (newFilter == NULL)
        {
        // Only sensible abort - return NULL.
        // Can't return what we were originally passed in as it'll be free'd by 
        // the caller
        return NULL;
        }
    
    // Zero memory to be used for new filter...
    memset(
           newFilter, 
           0, 
           (newFilterWCharCnt * sizeof(*newFilter))
          );

    // ...Copy existing filter over...
    if (ExistingFilter != NULL)
        {
        memcpy(
               newFilter, 
               ExistingFilter, 
               (existingFilterWCharCnt * sizeof(*ExistingFilter))
              );
        }

    // ...And add on new filter...
    wcscpy(&(newFilter[existingFilterWCharCnt]), FilterType);
    wcscpy(
           &(newFilter[(
                      existingFilterWCharCnt + 
                      wcslen(FilterType) +
                      1 // +1 here to cater for the terminating NULL on FilterType
                     )]),
           Filter
          );

    return newFilter;
}

// Note: This will *free* *off* the CurrentFilter, returning a new one - but only if needed
WCHAR* AppendFilterIfWanted(
    WCHAR* CurrentFilter,
    int WantedFilterTypes, 
    int CheckFilterType, 
    const WCHAR* FilterDesc, 
    const WCHAR* Filter
)
{
    WCHAR* retval;


    retval = CurrentFilter;

    if ((WantedFilterTypes & CheckFilterType) == CheckFilterType)
        {
        retval = _FileFilterGenerate_Add(
            CurrentFilter,
            FilterDesc,
            Filter
            );

        FileFilterFree(CurrentFilter);
        }

    return retval;
}

WCHAR* FileFilterGenerate(int Filter)
{
    WCHAR* retval;

    retval = NULL;

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_VOLANDKEYFILES, 
        FILE_FILTER_DESC_VOLANDKEYFILES,
        FILE_FILTER_EXTN_VOLANDKEYFILES
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_VOLUMES, 
        FILE_FILTER_DESC_VOLUMES,
        FILE_FILTER_EXTN_VOLUMES
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_KEYFILES, 
        FILE_FILTER_DESC_KEYFILES,
        FILE_FILTER_EXTN_KEYFILES
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_TEXT, 
        FILE_FILTER_DESC_TEXT,
        FILE_FILTER_EXTN_TEXT
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_CDBBACKUPS, 
        FILE_FILTER_DESC_CDBBACKUPS,
        FILE_FILTER_EXTN_CDBBACKUPS
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_EXECUTABLES, 
        FILE_FILTER_DESC_EXECUTABLES,
        FILE_FILTER_EXTN_EXECUTABLES
        );

    retval = AppendFilterIfWanted( 
        retval,
        Filter,
        FILE_FILTER_ALLFILES, 
        FILE_FILTER_DESC_ALLFILES,
        FILE_FILTER_EXTN_ALLFILES
        );

    return retval;
}

void FileFilterFree(WCHAR* FileFilter)
{
    SecZeroAndFreeWCHARMemory(FileFilter);
}


// =========================================================================
// =========================================================================

