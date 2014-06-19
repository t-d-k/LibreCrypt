// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgOptions.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceCommon.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"
#include "dlgTranslation.h"

#include "resource.h"

#include "SDUGeneral.h"

#include <stdlib.h>
#include <stdio.h> // Required for _snwprintf_s 
#include <winuser.h>  // Required for IDOK, etc
#include <Commdlg.h>  // Required from OPENFILENAME
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib


// =========================================================================
// Constants...

#define OPTION_TABS   4

#define PAGE_IDX_OPTIONS_GENERAL_ONE  0
#define PAGE_IDX_OPTIONS_GENERAL_TWO  1
#define PAGE_IDX_OPTIONS_HASHES       2
#define PAGE_IDX_OPTIONS_CYPHERS      3

// File association "Open" command
#define  FILE_ASSOC_OPEN   TEXT("\"%s\" /mount /volume \"%%1\" /freeotfe")

// If the user's selected more than this number of hash/cypher drivers, and they 
// have selected all available, warn user of performance impact when mounting
#define WARN_DRIVER_SELECTION_CNT   1

 // Buffer must be large enough to store *translated* msgs
#define TEMP_TRANSLATED_BUFFER_SIZE 4096

// Note: This is a literal string - it's translated programatically
const WCHAR LANG_CHANGE_RESTART_APP[] = TEXT("Please restart %1 to allow language changes to take effect.");
// A DUPLICATE OF THE ABOVE IS STORED HERE in order to allow the "dxgettext" tool
// to pick it for translation
#ifdef NEVER_DEFINED
#define JUNK _("Please restart %1 to allow language changes to take effect.")
#endif

// Note: This is a literal string - it's translated programatically
const WCHAR LANG_NOT_SAVING_SETTINGS[] = TEXT("You have not selected the option to save settings to a config file.\n\nAre you sure you don't want to make your settings persistant?");
// A DUPLICATE OF THE ABOVE IS STORED HERE in order to allow the "dxgettext" tool
// to pick it for translation
#ifdef NEVER_DEFINED
#define JUNK _("You have not selected the option to save settings to a config file.\n\nAre you sure you don't want to make your settings persistant?"),
#endif



// =========================================================================
// Local to this file...
HWND G_dlgOptions_MenuBar = NULL;
HWND G_dlgOptions_PropPage[OPTION_TABS];
BOOL G_dlgOptions_FileExtnAssociated = FALSE;
BOOL G_NewOptionsOK = FALSE;
OPTIONS* G_NewOptions = NULL;
BOOL G_NewFileExtnAssociated = FALSE;

int G_CountDriversHash;
int G_CountDriversCypher;
WCHAR** G_DriverFilenamesHash;
WCHAR** G_DriverFilenamesCypher;
HASH_DRIVER_INFO** G_DriverInfoHash;
CYPHER_DRIVER_INFO_v3** G_DriverInfoCypher;

LNKLIST_ITEM* G_TranslationsList = NULL;
WCHAR G_InitialLangCode[MAX_LANG_CODE];


// =========================================================================
// Forward declarations...

BOOL _dlgOptions_CheckFileAssociationPresent();

BOOL CALLBACK dlgOptions_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgOptions_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
int _dlgOptions_GetCurrPageIdx(HWND hDlg);
int _dlgOptions_GetPageIdxOfPage(HWND hDlg);
BOOL _dlgOptions_QuerySiblings(HWND hDlg);

BOOL dlgOptions_ValidateAllOptions(HWND hDlg);
BOOL dlgOptions_StoreOptions(HWND hDlg);

void _dlgOptions_FileAssociation(BOOL NewAssociateSelected);

// General #1
BOOL dlgOptions_PopulateDisplay_GeneralOne(HWND hDlg);
BOOL dlgOptions_GetUserInput_GeneralOne(HWND hDlg, OPTIONS* Opts);
BOOL dlgOptions_ValidateUserInput_GeneralOne(HWND hDlg, OPTIONS* Opts);
// General #2
BOOL dlgOptions_PopulateDisplay_GeneralTwo(HWND hDlg);
BOOL dlgOptions_GetUserInput_GeneralTwo(HWND hDlg, OPTIONS* Opts);
BOOL dlgOptions_ValidateUserInput_GeneralTwo(HWND hDlg, OPTIONS* Opts);
// Hashes
BOOL dlgOptions_PopulateDisplay_Hashes(HWND hDlg);
BOOL dlgOptions_GetUserInput_Hashes(HWND hDlg, OPTIONS* Opts);
BOOL dlgOptions_ValidateUserInput_Hashes(HWND hDlg, OPTIONS* Opts);
// Cyphers
BOOL dlgOptions_PopulateDisplay_Cyphers(HWND hDlg);
BOOL dlgOptions_GetUserInput_Cyphers(HWND hDlg, OPTIONS* Opts);
BOOL dlgOptions_ValidateUserInput_Cyphers(HWND hDlg, OPTIONS* Opts);

void dlgOptions_UpdateDriverSelectedCount(HWND hDlg);
void _dlgOptions_PopulateCombobox_Languages(HWND hDlg);

const WCHAR* dlgOptions_GetSelectedLangCode(
    HWND hDlg
);


// =========================================================================
void DisplayDlgOptions(HWND hWnd)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[OPTION_TABS];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgOptions\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying options dialog\n")));

    G_NewOptionsOK = FALSE;
    G_NewOptions = OptionsCopy(G_Options);

    // General #1 page...
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].lParam      = PAGE_IDX_OPTIONS_GENERAL_ONE;
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].pszTitle    = _("General #1");
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].hInstance   = G_hInstance;
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].pszTemplate = MAKEINTRESOURCE(IDD_OPTIONS_GENERAL_ONE);
    propPage[PAGE_IDX_OPTIONS_GENERAL_ONE].pfnDlgProc  = dlgOptions_PropSheetPageProc;
       
    // General #2 page...
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].lParam      = PAGE_IDX_OPTIONS_GENERAL_TWO;
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].pszTitle    = _("General #2");
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].hInstance   = G_hInstance;
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].pszTemplate = MAKEINTRESOURCE(IDD_OPTIONS_GENERAL_TWO);
    propPage[PAGE_IDX_OPTIONS_GENERAL_TWO].pfnDlgProc  = dlgOptions_PropSheetPageProc;
       
    // Hashes page...
    propPage[PAGE_IDX_OPTIONS_HASHES].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_OPTIONS_HASHES].lParam      = PAGE_IDX_OPTIONS_HASHES;
    propPage[PAGE_IDX_OPTIONS_HASHES].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_OPTIONS_HASHES].pszTitle    = _("Hashes");
    propPage[PAGE_IDX_OPTIONS_HASHES].hInstance   = G_hInstance;
    propPage[PAGE_IDX_OPTIONS_HASHES].pszTemplate = MAKEINTRESOURCE(IDD_OPTIONS_HASHES);
    propPage[PAGE_IDX_OPTIONS_HASHES].pfnDlgProc  = dlgOptions_PropSheetPageProc;
       
    // Cyphers page...
    propPage[PAGE_IDX_OPTIONS_CYPHERS].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_OPTIONS_CYPHERS].lParam      = PAGE_IDX_OPTIONS_CYPHERS;
    propPage[PAGE_IDX_OPTIONS_CYPHERS].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_OPTIONS_CYPHERS].pszTitle    = _("Cyphers");
    propPage[PAGE_IDX_OPTIONS_CYPHERS].hInstance   = G_hInstance;
    propPage[PAGE_IDX_OPTIONS_CYPHERS].pszTemplate = MAKEINTRESOURCE(IDD_OPTIONS_CYPHERS);
    propPage[PAGE_IDX_OPTIONS_CYPHERS].pfnDlgProc  = dlgOptions_PropSheetPageProc;
       
    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Options");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = OPTION_TABS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgOptions_PropSheetProc; // Needed to get WinCE 
                                                       // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);


	DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgOptions\n")));
}                


// =========================================================================
// Pass this the HWND for the *individual* *page*
void _dlgOptions_EnableDisableControls(HWND hDlg)
{
    BOOL specifyMntPnt;

    if (_dlgOptions_QuerySiblings(GetParent(hDlg)))
        {
        if (_dlgOptions_GetPageIdxOfPage(hDlg) == PAGE_IDX_OPTIONS_GENERAL_TWO)
            {
            specifyMntPnt = (IsDlgButtonChecked(
                                                hDlg,
                                                IDC_RADIO_MNTPNT_DEFAULT
                                               ) == BST_CHECKED);
	        SetControlEnabled(hDlg, IDC_STATIC_MOUNTPOINT, specifyMntPnt);
	        SetControlEnabled(hDlg, IDC_EDIT_MOUNTPOINT, specifyMntPnt);
            }
        }
}


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgOptions_GetCurrPageIdx(HWND hDlg)
{
    return _dlgOptions_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgOptions_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
HWND _dlgOptions_GetPageHWNDForPage(int idx)
{
    return G_dlgOptions_PropPage[idx];
}


// =========================================================================
BOOL CALLBACK dlgOptions_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("dlgOptions_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case PSN_SETACTIVE:
            {
            _dlgOptions_EnableDisableControls(hDlg);
            break;
            }

        case PSN_KILLACTIVE:
            {
            // (Validate if necessary here)
            // Set the DWL_MSGRESULT to PSNRET_NOERROR to receive PSN_APPLY
            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            break; 
            }

        case PSN_APPLY:
            {
            // This notification is received when the user clicks
            // "OK"/"Apply"
            // Because this is called once for each of the tabs, and we only
            // want to execute this functionality once for the whole dialog,
            // we only handle it for the "General" tab
            if (_dlgOptions_GetPageIdxOfPage(hDlg) != PAGE_IDX_OPTIONS_GENERAL_ONE)
                {
                // Not the genneral tab; set result as "OK"
                SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                }
            else
                {                 
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dialog OK'd by user\n")));

                if (dlgOptions_StoreOptions(hDlg))
                    {
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else
                    {
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_INVALID);
                    }
                }
                
            break; 
            }

        case PSN_RESET:
            {
            // This notification is received when the user clicks
            // "Close"/"Cancel"
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dialog cancelled by user\n")));
            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            break; 
            }

        }


    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("dlgOptions_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
BOOL dlgOptions_ValidateAllOptions(HWND hDlg)
{
    BOOL retval;
    int currPage;
    int newPage;

    retval = TRUE;
    newPage = 0;
    
    if (retval)
        {
        retval = dlgOptions_ValidateUserInput_GeneralOne(hDlg, G_NewOptions);
        newPage = PAGE_IDX_OPTIONS_GENERAL_ONE;
        }

    if (retval)
        {
        retval = dlgOptions_ValidateUserInput_GeneralTwo(hDlg, G_NewOptions);
        newPage = PAGE_IDX_OPTIONS_GENERAL_TWO;
        }

    if (retval)
        {
        retval = dlgOptions_ValidateUserInput_Hashes(hDlg, G_NewOptions);
        newPage = PAGE_IDX_OPTIONS_HASHES;
        }

    if (retval)
        {
        retval = dlgOptions_ValidateUserInput_Cyphers(hDlg, G_NewOptions);
        newPage = PAGE_IDX_OPTIONS_CYPHERS;
        }


    if (!(retval))
        {
        currPage = _dlgOptions_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg)));
        
        if (currPage != newPage)
            {
            PropSheet_SetCurSel(
                                GetParent(hDlg), 
                                NULL, 
                                newPage
                               );
            }
        }

    return retval;
}


// =========================================================================
BOOL dlgOptions_StoreOptions(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_StoreOptions\n")));

    OptionsFree(G_NewOptions);
    G_NewOptions = OptionsCopy(G_Options);
    G_NewOptionsOK = TRUE;

    if (_dlgOptions_QuerySiblings(GetParent(hDlg)))
        {
        if (dlgOptions_ValidateAllOptions(hDlg))
            {
            OptionsFree(G_Options);
            G_Options = OptionsCopy(G_NewOptions);

            if (!(OptionsWrite(G_Options)))
	            {
	            MsgError(hDlg, _("Options could not be saved."));
	            }

            // Sort out file associations...
            _dlgOptions_FileAssociation(G_NewFileExtnAssociated);

            // Update driver globals...
            G_driver_DisabledHashDrivers   = G_Options->DisabledHashes;
            G_driver_DisabledCypherDrivers = G_Options->DisabledCyphers;

            retval = TRUE;
            }
        }


    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_StoreOptions\n")));

    return retval;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgOptions_PropSheetProc(
  HWND hwndDlg, 
  UINT uMsg, 
  LPARAM lParam 
)
{
    int retval = 0;
    switch (uMsg)
        {
        case PSCB_GETVERSION:
            {
            retval = COMCTL32_VERSION;
            break; 
            }

        }

    return retval;
}


// =========================================================================
BOOL CALLBACK dlgOptions_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int currPageIdx;    

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_HandleMsg_WM_INITDIALOG\n")));


    // !!! WARNING !!!
    // This function gets called multiple times - once for each page!
    // See below for processing which should be only done *once* for the dialog



    // Don't recreate menu if it's already been done
    if (G_dlgOptions_MenuBar == NULL)
        {
        G_dlgOptions_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_DONECANCEL);
        }
    
    // This isn't actually needed for property sheet dialog...
    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgOptions_MenuBar);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    currPageIdx = ((LPPROPSHEETPAGE)lParam)->lParam;
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  currPageIdx
                 );
    G_dlgOptions_PropPage[currPageIdx] = hDlg;


    // THIS PROCESSING SHOULD ONLY BE DONE *ONCE* FOR THE DIALOG
    // **NOT** ONCE FOR 
    if (currPageIdx == PAGE_IDX_OPTIONS_GENERAL_ONE)
        {
        // Get details of all hash/cypher drivers for later
        if (!(driver_GetAllAlgorithmDriverDetails(
                                            &G_CountDriversHash,
                                            &G_DriverFilenamesHash,
                                            &G_DriverInfoHash,

                                            &G_CountDriversCypher,
                                            &G_DriverFilenamesCypher,
                                            &G_DriverInfoCypher,

                                            FALSE
                                            )))
            {
            G_CountDriversHash   = 0;
            G_CountDriversCypher = 0;
            }

        if (G_CountDriversHash == 0)
            {
            MsgWarn(hDlg, _("No hash drivers found."));
            }
        if (G_CountDriversCypher == 0)
            {
            MsgWarn(hDlg, _("No cypher drivers found."));
            }
        }

    // Populate display...
    // Default values for controls...
    if (currPageIdx == PAGE_IDX_OPTIONS_GENERAL_ONE)
        {
        dlgOptions_PopulateDisplay_GeneralOne(hDlg);
        }
    else if (currPageIdx == PAGE_IDX_OPTIONS_GENERAL_TWO)
        {
        dlgOptions_PopulateDisplay_GeneralTwo(hDlg);
        }
    else if (currPageIdx == PAGE_IDX_OPTIONS_HASHES)
        {
        dlgOptions_PopulateDisplay_Hashes(hDlg);
        }
    else if (currPageIdx == PAGE_IDX_OPTIONS_CYPHERS)
        {
        dlgOptions_PopulateDisplay_Cyphers(hDlg);
        }

    _dlgOptions_EnableDisableControls(hDlg);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgOptions_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_HandleMsg_WM_DESTROY\n")));
    if (G_dlgOptions_MenuBar != NULL)
        {
        DestroyWindow(G_dlgOptions_MenuBar);
        G_dlgOptions_MenuBar = NULL;
        }

    OptionsFree(G_NewOptions);

    driver_FreeAllAlgorithmDriverDetails(
                                        &G_CountDriversHash,
                                        &G_DriverFilenamesHash,
                                        &G_DriverInfoHash,

                                        &G_CountDriversCypher,
                                        &G_DriverFilenamesCypher,
                                        &G_DriverInfoCypher
                                        );


    // Language translations linked list...
    SDULLRemoveAllItems(&G_TranslationsList, TRUE);    

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgOptions_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgOptions_QuerySiblings\n")));

    G_NewOptionsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_NewOptionsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgOptions_QuerySiblings\n")));
    return retval;
}
    

// =========================================================================
BOOL
dlgOptions_PopulateDisplay_GeneralOne(HWND hDlg)
{
    if (G_Options->LargeIcons == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_LARGEICONS,
                       BST_CHECKED
                      );
        }    

    if (G_Options->RevertVolTimestamps == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_REVERTVOLTIMESTAMPS,
                       BST_CHECKED
                      );
        }

    // User can never use softkeys unless running WM2005 or later
    SetControlEnabled(
                      hDlg,
                      IDC_CHECK_SOFTKEYMENUS,
                      (SDUGetOSVersion_PDA() >= SDU_OS_VER_PDA_WM2005)
                     );
    if (G_Options->SoftkeyMenus == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_SOFTKEYMENUS,
                       BST_CHECKED
                      );
        }    

    _dlgOptions_PopulateCombobox_Languages(hDlg);

    if (G_Options->SaveSettings == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_SAVESETTINGS,
                       BST_CHECKED
                      );
        }

    SetDlgItemText_WithResize(
                   hDlg, 
                   IDC_CHECK_ASSOCIATE, 
                   SDUParamSubstituteW(
                             _("Associate with \".%1\" files"),
                             TEXT("%s"), VOL_FILE_EXTN,
                             NULL
                             )
                  );

    G_dlgOptions_FileExtnAssociated = _dlgOptions_CheckFileAssociationPresent();
    if (G_dlgOptions_FileExtnAssociated)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_ASSOCIATE,
                       BST_CHECKED
                      );
        }

    SDUTextSetBold(hDlg, IDC_CHECK_SAVESETTINGS, TRUE);
    // Putting text in bold will affect the width of the text; resize the control
    ResizeStaticToText_Child(hDlg, IDC_CHECK_SAVESETTINGS);

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_GetUserInput_GeneralOne(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    const WCHAR* useLangCode;

    retval = TRUE;


    G_NewFileExtnAssociated = (IsDlgButtonChecked(
                           hDlg,
                           IDC_CHECK_ASSOCIATE
                          ) == BST_CHECKED);


    Opts->LargeIcons = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_CHECK_LARGEICONS
                                       ) == BST_CHECKED);

    Opts->RevertVolTimestamps = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_CHECK_REVERTVOLTIMESTAMPS
                                       ) == BST_CHECKED);

    Opts->SoftkeyMenus = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_CHECK_SOFTKEYMENUS
                                       ) == BST_CHECKED);

    Opts->SaveSettings = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_CHECK_SAVESETTINGS
                                       ) == BST_CHECKED);

    useLangCode = dlgOptions_GetSelectedLangCode(hDlg);
    SecZeroAndFreeWCHARMemory(Opts->LanguageCode);
    Opts->LanguageCode = calloc(
                               (wcslen(useLangCode) + 1),
                               sizeof(*Opts->LanguageCode)
                              );
    if (Opts->LanguageCode == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to allocate memory for tmp opt string\n")));
        retval = FALSE;
        }
    else
        {
        wcscpy(Opts->LanguageCode, useLangCode);
        }

    return retval;
}

// =========================================================================
const WCHAR* dlgOptions_GetSelectedLangCode(
    HWND hDlg
)
{
    const WCHAR* retval;
    SDUI18N_LANGUAGEW* lang;

    retval = SDU_EMPTY_STRINGW;

    if (Combobox_GetItem(
                        hDlg,
                        IDC_COMBO_LANGUAGE,
                        &lang
                        ))
        {
        if (lang != NULL)
            {
            retval = lang->LanguageCode;
            }
        }

    return retval;
}


// =========================================================================
BOOL
dlgOptions_ValidateUserInput_GeneralOne(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    WCHAR translatedMsgText[TEMP_TRANSLATED_BUFFER_SIZE]; 

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_ValidateUserInput_GeneralOne\n")));

	retval = TRUE;

    if (!(Opts->SaveSettings))
        {
        // Warn user IN THEIR SELECTED LANGUAGE that they may want to *save* their settings
        // take effect...
        if (SDUi18n_GetText_ForCodeW(
               Opts->LanguageCode,
               LANG_NOT_SAVING_SETTINGS,
               translatedMsgText,
               (sizeof(translatedMsgText) / sizeof(translatedMsgText[0]))
               ))
            {
            retval = (MsgPromptWarn(
                        hDlg,
                        translatedMsgText,
                        MB_YESNO
                       ) == IDYES);
            }
        else
            {
            retval = (MsgPromptWarn(
                        hDlg,
                        LANG_NOT_SAVING_SETTINGS,
                        MB_YESNO
                       ) == IDYES);
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_ValidateUserInput_GeneralOne\n")));
	return retval;
}


// =========================================================================
BOOL
dlgOptions_PopulateDisplay_GeneralTwo(HWND hDlg)
{
    // Mountpoint radiobutton...
    if (G_Options->MntPntUseVolFilename == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_RADIO_MNTPNT_VOL,
                       BST_CHECKED
                      );
        }
    else
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_RADIO_MNTPNT_DEFAULT,
                       BST_CHECKED
                      );
        }
    SetDlgItemText(hDlg, IDC_EDIT_MOUNTPOINT, G_Options->MntPntDefault);

    SetDlgItemText(hDlg, IDC_EDIT_EXPLORER, G_Options->ExplorerExe);
    if (G_Options->ExploreOnMount == TRUE)
        {
        CheckDlgButton(
                       hDlg,    
                       IDC_CHECK_EXPLOREONMOUNT,
                       BST_CHECKED
                      );
        }

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_GetUserInput_GeneralTwo(HWND hDlg, OPTIONS* Opts)
{
	BOOL retval;
    WCHAR tmpStr[FREEOTFE_MAX_FILENAME_LENGTH];

    retval = TRUE;


    Opts->MntPntUseVolFilename = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_RADIO_MNTPNT_VOL
                                       ) == BST_CHECKED);

    SecZeroAndFreeWCHARMemory(Opts->MntPntDefault);
    GetDlgItemText( 
                   hDlg, 
                   IDC_EDIT_MOUNTPOINT,
                   tmpStr,
                   sizeof(tmpStr)
                 );
    // +1 to include terminating NULL
    Opts->MntPntDefault = calloc(
                                 (wcslen(tmpStr) + 1), 
                                 sizeof(*Opts->MntPntDefault)
                                );
    if (Opts->MntPntDefault == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to allocate memory for tmp opt string\n")));
        retval = FALSE;
        }
    else
        {
        wcscpy(Opts->MntPntDefault, tmpStr);
        }

    SecZeroAndFreeWCHARMemory(Opts->ExplorerExe);
    GetDlgItemText( 
                   hDlg, 
                   IDC_EDIT_EXPLORER,
                   tmpStr,
                   sizeof(tmpStr)
                 );
    // +1 to include terminating NULL
    Opts->ExplorerExe = calloc(
                               (wcslen(tmpStr) + 1),
                               sizeof(*Opts->ExplorerExe)
                              );
    if (Opts->ExplorerExe == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to allocate memory for tmp opt string\n")));
        retval = FALSE;
        }
    else
        {
        wcscpy(Opts->ExplorerExe, tmpStr);
        }
    Opts->ExploreOnMount = (IsDlgButtonChecked(
                                        hDlg,
                                        IDC_CHECK_EXPLOREONMOUNT
                                       ) == BST_CHECKED);

    return retval;
}

// =========================================================================
BOOL
dlgOptions_ValidateUserInput_GeneralTwo(HWND hDlg, OPTIONS* Opts)
{
	BOOL retval;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_ValidateUserInput_GeneralTwo\n")));

	retval = TRUE;

    if (retval)
        {
        retval = ValidateMountpoint(hDlg, Opts->MntPntDefault, TRUE, FALSE);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_ValidateUserInput_GeneralTwo\n")));
	return retval;
}


// =========================================================================
BOOL
dlgOptions_PopulateDisplay_Hashes(HWND hDlg)
{
    BOOL allOK = FALSE;
    WCHAR wcharDriverTitle[MAX_HASH_TITLE];
    int driverIdx;
    WCHAR* currDriverFilename;
    HASH_DRIVER_INFO* currDriverInfoHash;
    int lbIdx;
    BOOL driverEnabled;


    driverIdx = 0;
    while (G_DriverFilenamesHash[driverIdx] != NULL)
        {
        currDriverFilename = G_DriverFilenamesHash[driverIdx];
        currDriverInfoHash = G_DriverInfoHash[driverIdx];

        // If we got the DLL filename, but no driver details, skip this DLL file
        if (currDriverInfoHash == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping DLL due to lack of driver details\n")));
            driverIdx++;
            continue;
            }

        memset(wcharDriverTitle, 0, sizeof(wcharDriverTitle));
        mbstowcs(
                (WCHAR*)&wcharDriverTitle, 
                currDriverInfoHash->DriverTitle,
                strlen(currDriverInfoHash->DriverTitle)
                );

        lbIdx = Listbox_AddItem(
                        hDlg,
                        IDC_LIST_DRIVERS,    
                        wcharDriverTitle,                            
                        // currDriverFilename,
                        (void*)driverIdx
                        );

        driverEnabled = (!(IsDriverDisabled(
                                            G_NewOptions->DisabledHashes, 
                                            currDriverFilename
                                           )));
        SendMessage(
                    GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                    LB_SETSEL,
                    (WPARAM)driverEnabled,
                    (LPARAM)lbIdx
                   );

        driverIdx++;
        }

    dlgOptions_UpdateDriverSelectedCount(hDlg);

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_GetUserInput_Hashes(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    int totalCount;
    int i;
    int driverIdx;

    retval = TRUE;

    totalCount = SendMessage(
                            GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                            LB_GETCOUNT,
                            (WPARAM)0,
                            (LPARAM)0
                           );

    FreeAllDisabledDrivers(&(Opts->DisabledHashes));
    for (i = 0; i < totalCount; i++)
        {
        if (!(SendMessage(
                            GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                            LB_GETSEL,
                            (WPARAM)i,
                            (LPARAM)0
                           )))
            {
            driverIdx = SendMessage(
                                GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                                LB_GETITEMDATA,
                                (WPARAM)i,
                                (LPARAM)0
                               );
            AddDisabledDriver(
                              &(Opts->DisabledHashes), 
                              G_DriverFilenamesHash[driverIdx]
                             );
            }
        }

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_ValidateUserInput_Hashes(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    int enabledCount;
    
    retval = TRUE;

    enabledCount = G_CountDriversHash - CountDisabledDrivers(Opts->DisabledHashes);

    if (enabledCount <= 0)
        {
        retval = (MsgPromptWarn(
                                hDlg, 
                                _("You have not enabled any hash drivers\n\nYou will be unable to use FreeOTFE4PDA without enabling at least one hash driver\n\nAre you sure you wish to do this?"),
                                MB_YESNO
                               ) == IDYES);
        }
    else if (
             (enabledCount > WARN_DRIVER_SELECTION_CNT) &&
             (enabledCount >= G_CountDriversHash)
             )
        {
        retval = (MsgPromptWarn(
                                hDlg, 
                                _("You have enabled all hash drivers\n\nEnabling all hash drivers can significantly increase the time taken to mount volumes.\n\nAre you sure you wish to do this?"),
                                MB_YESNO
                               ) == IDYES);
        }

    return retval;
}


// =========================================================================
BOOL
dlgOptions_PopulateDisplay_Cyphers(HWND hDlg)
{
    BOOL allOK = FALSE;
    WCHAR wcharDriverTitle[MAX_CYPHER_TITLE];
    int driverIdx;
    WCHAR* currDriverFilename;
    CYPHER_DRIVER_INFO_v3* currDriverInfoCypher;
    int lbIdx;
    BOOL driverEnabled;


    driverIdx = 0;
    while (G_DriverFilenamesCypher[driverIdx] != NULL)
        {
        currDriverFilename = G_DriverFilenamesCypher[driverIdx];
        currDriverInfoCypher = G_DriverInfoCypher[driverIdx];

        // If we got the DLL filename, but no driver details, skip this DLL file
        if (currDriverInfoCypher == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_WARN, (TEXT("Skipping DLL due to lack of driver details\n")));
            driverIdx++;
            continue;
            }

        memset(wcharDriverTitle, 0, sizeof(wcharDriverTitle));
        mbstowcs(
                (WCHAR*)&wcharDriverTitle, 
                currDriverInfoCypher->DriverTitle,
                strlen(currDriverInfoCypher->DriverTitle)
                );

        lbIdx = Listbox_AddItem(
                        hDlg,
                        IDC_LIST_DRIVERS,    
                        wcharDriverTitle,                            
                        // currDriverFilename,
                        (void*)driverIdx
                        );

        driverEnabled = (!(IsDriverDisabled(
                                            G_NewOptions->DisabledCyphers, 
                                            currDriverFilename
                                           )));
        SendMessage(
                    GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                    LB_SETSEL,
                    (WPARAM)driverEnabled,
                    (LPARAM)lbIdx
                   );

        driverIdx++;
        }

    dlgOptions_UpdateDriverSelectedCount(hDlg);

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_GetUserInput_Cyphers(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    int totalCount;
    int i;
    int driverIdx;

    retval = TRUE;

    totalCount = SendMessage(
                            GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                            LB_GETCOUNT,
                            (WPARAM)0,
                            (LPARAM)0
                           );

    FreeAllDisabledDrivers(&(Opts->DisabledCyphers));
    for (i = 0; i < totalCount; i++)
        {
        if (!(SendMessage(
                            GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                            LB_GETSEL,
                            (WPARAM)i,
                            (LPARAM)0
                           )))
            {
            driverIdx = SendMessage(
                                GetDlgItem(hDlg, IDC_LIST_DRIVERS),
                                LB_GETITEMDATA,
                                (WPARAM)i,
                                (LPARAM)0
                               );
            AddDisabledDriver(
                              &(Opts->DisabledCyphers), 
                              G_DriverFilenamesCypher[driverIdx]
                             );
            }
        }

    return TRUE;
}

// =========================================================================
BOOL
dlgOptions_ValidateUserInput_Cyphers(HWND hDlg, OPTIONS* Opts)
{
    BOOL retval;
    int enabledCount;
    
    retval = TRUE;

    enabledCount = G_CountDriversCypher - CountDisabledDrivers(Opts->DisabledCyphers);

    if (enabledCount <= 0)
        {
        retval = (MsgPromptWarn(
                                hDlg, 
                                _("You have not enabled any cypher drivers\n\nYou will be unable to use FreeOTFE4PDA without enabling at least one cypher driver\n\nAre you sure you wish to do this?"),
                                MB_YESNO
                               ) == IDYES);
        }
    else if (
             (enabledCount > WARN_DRIVER_SELECTION_CNT) &&
             (enabledCount >= G_CountDriversCypher)
             )
        {
        retval = (MsgPromptWarn(
                                hDlg, 
                                _("You have enabled all cypher drivers\n\nEnabling all cypher drivers can significantly increase the time taken to mount volumes.\n\nAre you sure you wish to do this?"),
                                MB_YESNO
                               ) == IDYES);
        }

    return retval;
}


// =========================================================================
// Returns: TRUE if the file extension has an association, otherwise FALSE
BOOL
_dlgOptions_CheckFileAssociationPresent()
{
    BOOL retval;

    retval = RegIsFileExtensionAssociated(NULL, VOL_FILE_TYPE);

    return retval;
}


// =========================================================================
void
_dlgOptions_FileAssociation(BOOL NewAssociateSelected)
{
    WCHAR tmpFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR tmpOpenCommand[FREEOTFE_MAX_FILENAME_LENGTH];
    
    // Do nothing with the file extension, unless the user's changed the
    // setting

    if (NewAssociateSelected != G_dlgOptions_FileExtnAssociated)
        {
        if (NewAssociateSelected)
            {
            if (GetModuleFileName(
                                  NULL, 
                                  tmpFilename, 
                                  sizeof(tmpFilename)
                                 ) != 0)
                {
                _snwprintf(
                           tmpOpenCommand, 
                           (sizeof(tmpOpenCommand) / sizeof(tmpOpenCommand[0])),
                           FILE_ASSOC_OPEN,
                           tmpFilename
                          );

                RegAssociateFileExtension(
                                          VOL_FILE_EXTN,
                                          VOL_FILE_TYPE,
                                          (WCHAR*)VOL_FILE_DESC,
                                          tmpOpenCommand,
                                          tmpFilename,
                                          IDI_ICON_MAIN
                                         );
                }
            }
        else
            {
            RegDisassociateFileExtension(
                                         VOL_FILE_EXTN,
                                         VOL_FILE_TYPE
                                        );
            }
        }

}


// =========================================================================
BOOL CALLBACK dlgOptions_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;
    int msgID;
    WCHAR translatedMsgText[TEMP_TRANSLATED_BUFFER_SIZE]; 
    const WCHAR* useLangCode;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    ctrlID = LOWORD(wParam);
    msgID = HIWORD(wParam);

    switch(ctrlID)
        {
        case IDOK:
            {
            // IMPORTANT: EndDialog *not* called for property sheet dialog
            // We just forward the msg received by the *page* to the owning
            // parent; the property sheet dialog
            PropSheet_PressButton(GetParent(hDlg), PSBTN_OK);
            retval = TRUE;
            break;
            }

        case IDCANCEL:
            {
            // IMPORTANT: EndDialog *not* called for property sheet dialog
            // We just forward the msg received by the *page* to the owning
            // parent; the property sheet dialog
            PropSheet_PressButton(GetParent(hDlg), PSBTN_CANCEL);
            retval = TRUE;
            break;
            }

        case IDC_COMBO_LANGUAGE:
            {
            if (msgID == CBN_SELENDOK)
                {
                // Warn user to restart application for language changes to 
                // take effect...
                useLangCode = dlgOptions_GetSelectedLangCode(hDlg);
                if (SDUi18n_GetText_ForCodeW(
                       useLangCode,
                       LANG_CHANGE_RESTART_APP,
                       translatedMsgText,
                       (sizeof(translatedMsgText) / sizeof(translatedMsgText[0]))
                       ))
                    {
                    MsgInfo(
                            hDlg,
                            SDUParamSubstituteW(
                              translatedMsgText,
                              TEXT("%s"), APP_TITLE,
                              NULL
                              )
                            );
                    }
                retval = TRUE;
                }
            break;
            }

        case IDC_BUTTON_LANG_DETAILS:
            {
            useLangCode = dlgOptions_GetSelectedLangCode(hDlg);
            DisplayDlgTranslation(hDlg, useLangCode);
            break;
            }

        case IDC_BUTTON_BROWSEEXPLORER:
            {
            OPENFILENAME ofn;
            DWORD flags;
            WCHAR filename[FREEOTFE_MAX_FILENAME_LENGTH];
            WCHAR* useFilter;
			BOOL fileDlgOK;

            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Browse...\n")));

            memset(filename, 0, sizeof(filename));

            ofn.lStructSize       = sizeof(ofn);
            ofn.hwndOwner         = hDlg;
            ofn.hInstance         = NULL;
            ofn.lpstrCustomFilter = NULL;
            ofn.lpstrFile         = filename;
            ofn.nMaxFile          = (sizeof(filename) / sizeof(filename[0]));
            ofn.lpstrFileTitle    = NULL;
            ofn.nMaxFileTitle     = 0;
            ofn.lpstrInitialDir   = NULL;
            ofn.lpstrTitle        = NULL;
            flags = (
                     // OFN_HIDEREADONLY |
                     OFN_FILEMUSTEXIST | 
                     OFN_PATHMUSTEXIST
                    );
            ofn.Flags             = flags;
            ofn.nFileOffset       = 0;
            ofn.nFileExtension    = 0;
            ofn.lpstrDefExt       = NULL;
            ofn.lCustData         = 0;
            ofn.lpfnHook          = NULL;
            ofn.lpTemplateName    = NULL;

            useFilter = FileFilterGenerate(FILE_FILTER_FLT_EXECUTABLES);

            ofn.lpstrFilter       = useFilter;
            ofn.nMaxCustFilter    = FILE_FILTER_CNT_EXECUTABLES;
            ofn.nFilterIndex      = FILE_FILTER_DFLT_EXECUTABLES;

    		fileDlgOK = GetOpenFileName(&ofn);

			if (fileDlgOK)
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog OK'd...\n")));
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Filename: %ls\n"), filename));
                SetDlgItemText(hDlg, IDC_EDIT_EXPLORER, filename);
                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog cancelled\n")));
                }

            FileFilterFree(useFilter);

            break;
            }

        case IDC_RADIO_MNTPNT_VOL:
        case IDC_RADIO_MNTPNT_DEFAULT:
            {
            _dlgOptions_EnableDisableControls(hDlg);
            break;
            }

        case IDC_LIST_DRIVERS:
            {
            if (msgID == LBN_SELCHANGE)
                {
                dlgOptions_UpdateDriverSelectedCount(hDlg);
                }
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgOptions_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgOptions_HandleMsg_PSM_QUERYSIBLINGS\n")));
    
    if (_dlgOptions_GetPageIdxOfPage(hDlg) == PAGE_IDX_OPTIONS_GENERAL_ONE)
        {
        // Process for "General #1" dialog/tab
        dlgOptions_GetUserInput_GeneralOne(hDlg, G_NewOptions);
        }
    else if (_dlgOptions_GetPageIdxOfPage(hDlg) == PAGE_IDX_OPTIONS_GENERAL_TWO)
        {
        // Process for "General #2" dialog/tab
        dlgOptions_GetUserInput_GeneralTwo(hDlg, G_NewOptions);
        }
    else if (_dlgOptions_GetPageIdxOfPage(hDlg) == PAGE_IDX_OPTIONS_HASHES)
        {
        // Process for "Hashes" dialog/tab
        dlgOptions_GetUserInput_Hashes(hDlg, G_NewOptions);
        }
    else if (_dlgOptions_GetPageIdxOfPage(hDlg) == PAGE_IDX_OPTIONS_CYPHERS)
        {
        // Process for "Cyphers" dialog/tab
        dlgOptions_GetUserInput_Cyphers(hDlg, G_NewOptions);
        }

    G_NewOptionsOK = (G_NewOptionsOK && retval);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgOptions_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgOptions_PropSheetPageProc(
                           HWND hDlg, 
                           UINT msg, 
                           WPARAM wParam, 
                           LPARAM lParam
                          )
{
    BOOL retval = FALSE;

    switch(msg)
        {
        case WM_COMMAND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_COMMAND\n")));
            retval = dlgOptions_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgOptions_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgOptions_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgOptions_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
            break;
            }

        //case WM_ACTIVATE:
            //{
            //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ACTIVATE\n")));
            //SHHandleWMActivate(hDlg, wParam, lParam, &s_sai, 0);
            //break;
            //}

        //case WM_SETTINGCHANGE:
            //{
            //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SETTINGCHANGE\n")));
            //SHHandleWMSettingChange(hDlg, wParam, lParam, &s_sai);
            //break;
            //}

        case PSM_QUERYSIBLINGS:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: PSM_QUERYSIBLINGS\n")));
            retval = dlgOptions_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
void dlgOptions_UpdateDriverSelectedCount(HWND hDlg)
{
    int cnt;

    cnt = SendMessage(
                    GetDlgItem(hDlg, IDC_LIST_DRIVERS),      
                    (UINT)LB_GETSELCOUNT,
                    (WPARAM)0,
                    (LPARAM)0
                    );

    SetDlgItemText_WithResize(
                   hDlg, 
                   IDC_STATIC_CNT_SELECTED, 
                   SDUParamSubstituteW(
                             _("Drivers selected: %1"),
                             TEXT("%d"), cnt,
                             NULL
                             )
                  );

}


// =========================================================================
void _dlgOptions_PopulateCombobox_Languages(HWND hDlg)
{
    int i;
    int cbItemIdx;
    int cnt;
    SDUI18N_LANGUAGEW* lang;
    SDUI18N_LANGUAGEW* selectLang;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgOptions_PopulateCombobox_Languages\n")));


    // Store initial configured language code for later use
    wcscpy(G_InitialLangCode, G_Options->LanguageCode);

    G_TranslationsList = NULL;
    cnt = SDUi18n_GetTranslationsW(&G_TranslationsList);


    Combobox_AddItem(
                        hDlg,
                        IDC_COMBO_LANGUAGE,
                        (WCHAR*)_("(Default)"),
                        NULL                                    
                       );
    selectLang = NULL;
    for (i = 0; i < cnt; i++)
        {
        SDULLGetItem(G_TranslationsList, i, &lang);

        cbItemIdx = Combobox_AddItem(
                                hDlg,
                                IDC_COMBO_LANGUAGE,
                                lang->LanguageName,
                                (void*)lang                                    
                               );
        if (wcscmp(G_Options->LanguageCode, lang->LanguageCode) == 0)
            {
            selectLang = lang;
            }
        }
    // Set default (if possible)
    if (selectLang != NULL)
        {
        SetComboBoxSelected(hDlg, IDC_COMBO_LANGUAGE, selectLang->LanguageName);
        }
    else
        {
        SetComboBoxSelected(hDlg, IDC_COMBO_LANGUAGE, (WCHAR*)_("(Default)"));
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgOptions_PopulateCombobox_Languages\n")));
}


// =========================================================================
// =========================================================================
