// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include <stdlib.h>  // Required for _ultow(...)
#include <storemgr.h>  // Required for STORAGE_DEVICE_TYPE_PCCARD, etc
#include <Commdlg.h>  // Required from OPENFILENAME
#include <winuser.h>  // Required for IDOK, etc
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell")  // Link in aygshell.lib

#include "main.h"
#include "dlgDumpLUKSWizard.h"
#include "FreeOTFEDebug.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "resource.h"
#include "SDUGeneral.h"
#include "LUKS.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"


// =========================================================================
// Constants...

// Relativly arbitary marker const so we can identify tabs we created as they
// receive WM_INITDIALOG msg
#define SUBTAB_FLAG 0x0ABCDEF0

// The number of steps in the wizard
#define DUMP_LUKS_WIZARD_STEPS 3

// Password change/keyfile generation...
#define PAGE_IDX_SRCFILENAME        0
#define PAGE_IDX_SRCDETAILS         1
#define PAGE_IDX_DESTFILENAME       2

#define SRCDETAILSTAB_IDX_PASSWORD   0
#define SRCDETAILSTAB_IDX_ADVANCED   1

// =========================================================================
// Structures...

// Selected RNG
typedef struct _DUMPLUKS_PARAMETERS {
    BOOL ParamsOK;
    int ErrorPageIdx;

    WCHAR SrcFilename[FREEOTFE_MAX_FILENAME_LENGTH];

    unsigned char* SrcUserPassword;
    BOOL SrcBaseIVCypherOnHashLength;

    WCHAR DestFilename[FREEOTFE_MAX_FILENAME_LENGTH];

} DUMPLUKS_PARAMETERS, *PDUMPLUKS_PARAMETERS;


// =========================================================================
// Local to this file...

HWND G_dlgDumpLUKSWizard_MenuBar = NULL;
BOOL G_dlgDumpLUKSWizard_PageCompleted[DUMP_LUKS_WIZARD_STEPS];
DUMPLUKS_PARAMETERS G_dlgDumpLUKSWizard_UserParams;

BOOL G_dlgDumpLUKSWizard_AllowFinish = FALSE;

HWND hDumpLUKSSrcDetailsWndPassword = NULL;
HWND hDumpLUKSSrcDetailsWndAdvanced = NULL;

// The last page changes depending on RNG selected
int G_dlgDumpLUKSWizard_LastPageIdx = (DUMP_LUKS_WIZARD_STEPS - 1);

// =========================================================================
// Forward declarations...

void _dlgDumpLUKSWizard_EnableDisableControls(HWND hDlg);

BOOL CALLBACK dlgDumpLUKSWizard_Finish(HWND hDlg);
BOOL CALLBACK dlgDumpLUKSWizard_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgDumpLUKSWizard_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
void _dlgDumpLUKSWizard_SecZeroParams();
BOOL dlgDumpLUKSWizard_ValidatePage(HWND hDlg);
BOOL dlgDumpLUKSWizard_ValidateAll(HWND hDlg);

void _dlgDumpLUKSWizard_SizeWindowTab(
    HWND hTabWnd,
    HWND hWnd
);
void _dlgDumpLUKSWizard_SizeWindowTab_CtrlID(
    HWND hTabParentWnd,
    int tabCtrlID,
    HWND hWnd
);
BOOL _dlgDumpLUKSWizard_QuerySiblings(HWND hDlg);
int _dlgDumpLUKSWizard_GetCurrPageIdx(HWND hDlg);
int _dlgDumpLUKSWizard_GetPageIdxOfPage(HWND hDlg);


// =========================================================================
void DisplayDlgDumpLUKSWizard(HWND hWnd)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[DUMP_LUKS_WIZARD_STEPS];
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgDumpLUKSWizard\n")));

    for (
         i = 0;
         i < (sizeof(G_dlgDumpLUKSWizard_PageCompleted) / sizeof(G_dlgDumpLUKSWizard_PageCompleted[0]));
         i++
        )
        {
        G_dlgDumpLUKSWizard_PageCompleted[i] = FALSE;
        }

    memset(
           &G_dlgDumpLUKSWizard_UserParams, 
           0,
           sizeof(G_dlgDumpLUKSWizard_UserParams)
          );

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));


    // Src filename step...
    propPage[PAGE_IDX_SRCFILENAME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SRCFILENAME].lParam      = PAGE_IDX_SRCFILENAME;
    propPage[PAGE_IDX_SRCFILENAME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SRCFILENAME].pszTitle    = _("Source filename");
    propPage[PAGE_IDX_SRCFILENAME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SRCFILENAME].pszTemplate = MAKEINTRESOURCE(IDD_FILENAME);
    propPage[PAGE_IDX_SRCFILENAME].pfnDlgProc  = dlgDumpLUKSWizard_PropSheetPageProc;

    // Src details step...
    propPage[PAGE_IDX_SRCDETAILS].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SRCDETAILS].lParam      = PAGE_IDX_SRCDETAILS;
    propPage[PAGE_IDX_SRCDETAILS].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SRCDETAILS].pszTitle    = _("Source details");
    propPage[PAGE_IDX_SRCDETAILS].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SRCDETAILS].pszTemplate = MAKEINTRESOURCE(IDD_DIALOG_TAB);
    propPage[PAGE_IDX_SRCDETAILS].pfnDlgProc  = dlgDumpLUKSWizard_PropSheetPageProc;

    // Dest filename step...
    propPage[PAGE_IDX_DESTFILENAME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_DESTFILENAME].lParam      = PAGE_IDX_DESTFILENAME;
    propPage[PAGE_IDX_DESTFILENAME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_DESTFILENAME].pszTitle    = _("Destination filename");
    propPage[PAGE_IDX_DESTFILENAME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_DESTFILENAME].pszTemplate = MAKEINTRESOURCE(IDD_FILENAME);
    propPage[PAGE_IDX_DESTFILENAME].pfnDlgProc  = dlgDumpLUKSWizard_PropSheetPageProc;

    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Dump LUKS");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = DUMP_LUKS_WIZARD_STEPS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgDumpLUKSWizard_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgDumpLUKSWizard\n")));
}                


// =========================================================================
// Give the user the option of displaying the file now.
void _dlgDumpLUKSWizard_PromptDisplayDump(HWND hDlg)
{
    SHELLEXECUTEINFO seInfo;
    if (MsgPromptInfo(
                   hDlg,
                   _("LUKS dumped to file.\n\nDo you wish to view the human readable version now?"),
                   MB_YESNO
                  ) == IDYES)
        {
        // Launch viewer with stats report

        // We use ShellExecuteEx, rather than by using the executable's
        // filename and CreateProcess(...)
        seInfo.cbSize = sizeof(seInfo);
        seInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
        seInfo.hwnd = NULL;
        seInfo.lpVerb = TEXT("open");
        seInfo.lpFile = G_dlgDumpLUKSWizard_UserParams.DestFilename;
        seInfo.lpParameters = NULL;
        seInfo.lpDirectory = NULL;
        seInfo.nShow = SW_SHOWNORMAL;
        seInfo.hInstApp = G_hInstance;
        // Optional fields
        seInfo.lpIDList = 0;
        seInfo.lpClass = 0;
        seInfo.hkeyClass = NULL;
        seInfo.dwHotKey = 0;
        seInfo.hIcon = NULL;
        seInfo.hProcess = NULL;

        ShellExecuteEx(&seInfo);

        /*
        This will also do the same thing, but ties it to a specific
        executable:
        CreateProcess(
                      EXE_whatever_it_is,
                      G_dlgDumpLUKSWizard_UserParams.DestFilename,

                      NULL,
                      NULL,
                      FALSE,
                      0,
                      NULL,
                      NULL,
                      NULL,
                      NULL
                     );
        */
        }
}


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgDumpLUKSWizard_GetCurrPageIdx(HWND hDlg)
{
    return _dlgDumpLUKSWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgDumpLUKSWizard_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Returns 0 based index of the last page
int _dlgDumpLUKSWizard_GetPageIdxOfLastPage()
{
    int retval;

    retval = G_dlgDumpLUKSWizard_LastPageIdx;

    return retval;
}


// =========================================================================
void _dlgDumpLUKSWizard_SetWizardButtons(int PageIdx)
{
    UINT leftSoftkeyCmdID;

    CommandBar_ItemEnable(
                     G_dlgDumpLUKSWizard_MenuBar,
                     ID_WIZ_BACK,
                     (PageIdx > 0)
                    );

    CommandBar_ItemEnable(
                     G_dlgDumpLUKSWizard_MenuBar,
                     ID_WIZ_NEXT,
                     (PageIdx < _dlgDumpLUKSWizard_GetPageIdxOfLastPage())
                    );

    CommandBar_ItemEnable(
                     G_dlgDumpLUKSWizard_MenuBar,
                     IDOK,
                     // G_dlgDumpLUKSWizard_AllowFinish
                     (PageIdx == _dlgDumpLUKSWizard_GetPageIdxOfLastPage())
                    );

    // Default to left softkey being "Next >"
    leftSoftkeyCmdID = ID_WIZ_NEXT;
    // If user can finish with wizard, set left softkey to "Finish"
    if (
        // G_dlgDumpLUKSWizard_AllowFinish
        (PageIdx == _dlgDumpLUKSWizard_GetPageIdxOfLastPage())
       )
        {
        leftSoftkeyCmdID = IDOK;
        }
    ChangeLeftSoftkey(
                      G_dlgDumpLUKSWizard_MenuBar,
                      leftSoftkeyCmdID,
                      NULL
                     );

}


// =========================================================================
void _dlgDumpLUKSWizard_ShowDetailsTabPage(HWND hWnd, BOOL show)
{
    if (hWnd != NULL)
        {
        if (show)
            {
            ShowWindow(hWnd, SW_SHOW);
            }
        else
            {
            ShowWindow(hWnd, SW_HIDE);
            }
        }
}


// =========================================================================
void _dlgDumpLUKSWizard_DisplayDetailsTabPage(
    HWND hDlg,
    int CurrPage
)
{
    HWND tabHWND = NULL;
    HWND ctrlShow = NULL;
    int tabIdx;

    // Hide all.
    _dlgDumpLUKSWizard_ShowDetailsTabPage(
                                              hDumpLUKSSrcDetailsWndPassword, 
                                              FALSE
                                             );
    _dlgDumpLUKSWizard_ShowDetailsTabPage(
                                              hDumpLUKSSrcDetailsWndAdvanced, 
                                              FALSE
                                             );

    if (CurrPage == PAGE_IDX_SRCDETAILS)
        {
        tabHWND = GetDlgItem(hDlg, IDC_TAB1);
        if (tabHWND != NULL)
            {
            // Show the appropriate tab sheet...
            tabIdx = TabCtrl_GetCurSel(tabHWND);

            if (CurrPage == PAGE_IDX_SRCDETAILS)
                {
                if (tabIdx == SRCDETAILSTAB_IDX_PASSWORD)
                    {
                    ctrlShow = hDumpLUKSSrcDetailsWndPassword;
                    }
                else if (tabIdx == SRCDETAILSTAB_IDX_ADVANCED)
                    {
                    ctrlShow = hDumpLUKSSrcDetailsWndAdvanced;
                    }
                }

            _dlgDumpLUKSWizard_ShowDetailsTabPage(ctrlShow, TRUE);
            }
        }

}


// =========================================================================
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    int currPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case TCN_SELCHANGE:
            {
            currPage = _dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg);

            if (
                (currPage == PAGE_IDX_SRCDETAILS) &&
                (idCtrl == IDC_TAB1)
               )
                {
                _dlgDumpLUKSWizard_DisplayDetailsTabPage(hDlg, currPage);
                retval = TRUE;
                }
            
            break;
            }

        case PSN_SETACTIVE:
            {
            currPage = _dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg);

            _dlgDumpLUKSWizard_DisplayDetailsTabPage(hDlg, currPage);
            _dlgDumpLUKSWizard_SetWizardButtons(currPage);

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
            // This is called once for each of the tabs.
            // When called, we validate the tab it is called for (this
            // has the effect that the first invalid tab will be automatically
            // selected)
            // If the tab being called for is the last one (the summary),
            // we create the new volume.
            currPage = _dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg);
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PSN_APPLY for page idx: %d\n"), currPage));

            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            if (dlgDumpLUKSWizard_ValidatePage(hDlg))
                {
                // If it's the last page, only succeed if the volume can be
                // created
                if (currPage != _dlgDumpLUKSWizard_GetPageIdxOfLastPage())
                    {
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else
                    {
                    if (dlgDumpLUKSWizard_Finish(hDlg))
                        {
                        // Already prompted user to display file created
                        SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                        }
                    else
                        {
                        MsgError(hDlg, _("Unable to dump LUKS details; please check your password and other settings"));
                        SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_INVALID);
                        }
                    }
                }
            else
                {
                SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_INVALID);
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


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgDumpLUKSWizard_PropSheetProc(
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
            // We do *NOT* set the version here. The resize causes the page
            // to be displayed *over* the property sheet's tabs. Setting
            // this version prevents this from working correctly
            retval = COMCTL32_VERSION;
            break; 
            }

        }

    return retval;
}


// =========================================================================
HWND _dlgDumpLUKSWizard_CreateDialogWindow(
    HWND ParentHWND, 
    int ResourceID
)
{
    DLGTEMPLATE* dlgtempl;
    HRSRC hrsrc;
    HGLOBAL hglb;

    hrsrc = FindResource(
                         G_hInstance,
                         MAKEINTRESOURCE(ResourceID),
                         RT_DIALOG
                        );
    hglb = LoadResource(G_hInstance, hrsrc);
    dlgtempl = (DLGTEMPLATE*)LockResource(hglb); 
    return CreateDialogIndirectParam(
                            G_hInstance,
                            dlgtempl,
                            ParentHWND,
                            dlgDumpLUKSWizard_PropSheetPageProc,
                            SUBTAB_FLAG
                           );

}


// =========================================================================
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int i;
    UINT menuID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_INITDIALOG\n")));

    // Setup globals...

    // Don't recreate menu if it's already been done
    if (G_dlgDumpLUKSWizard_MenuBar == NULL)
        {
        menuID = IDR_MENU_WIZARD;
        // If softkeys are supported, we use a menu with separators
        if (G_Options->SoftkeyMenus == TRUE)
            {
            menuID = IDR_MENU_WIZARD_SOFTKEY;
            }

        G_dlgDumpLUKSWizard_MenuBar = SetupMenu(
                                               hDlg,
                                               menuID,
                                               ID_WIZ_NEXT,
                                               NULL
                                              );
        }

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgDumpLUKSWizard_MenuBar);

    for (i = 0; i < DUMP_LUKS_WIZARD_STEPS; i++)
        {
        G_dlgDumpLUKSWizard_PageCompleted[i] = FALSE;
        }
    memset(
           &G_dlgDumpLUKSWizard_UserParams, 
           0,
           sizeof(G_dlgDumpLUKSWizard_UserParams)
          );


//xxx - get rid of this?
    // This isn't actually needed for property sheet dialog...
    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    // Because this procedure is called to initialize both the propertysheet
    // pages *and* the dialogs (windows) created for the Password/Advanced tabs
    // on the src/dest details propertysheet page, we ensure this isn't set
    // for those windows.
    // This is done by passing a flag lParam to them when they're created,
    // and detecting it here
    // Note: You will get a "Data Abort" error here if lParam is dereferenced    
    //       here and it's NULL
    if (lParam != SUBTAB_FLAG)
        {
        SetWindowLong(
                      hDlg,
                      GWL_USERDATA,
                      ((LPPROPSHEETPAGE)lParam)->lParam
                     );
        }

    // Default values for controls...
    SetDlgItemText(hDlg, IDC_EDIT_FILENAME,   DEFAULT_FILENAME);
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORD,   DEFAULT_LUKS_PASSWORD);
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORDCONFIRM, DEFAULT_LUKS_PASSWORD);

    SetDlgItemInt(hDlg, IDC_EDIT_SIZELIMIT, DEFAULT_SIZELIMIT, TRUE);

	if (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
        {
        SetStaticText(hDlg, IDC_STATIC_FILENAME, _("Volume:"));

        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INST_DUMPLUKS_SRCFILE
                        );
        }
    else if (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCDETAILS)
        {
        HWND tmpHWND;
        TCITEM tci;

        // Setup the tab control to get the WinCE look and feel...
        tmpHWND = GetDlgItem(hDlg, IDC_TAB1);
        SendMessage(tmpHWND, CCM_SETVERSION, COMCTL32_VERSION, 0); 
        
        tci.mask = TCIF_TEXT;
        tci.pszText = (WCHAR*)_("Password");
        TabCtrl_InsertItem(tmpHWND, SRCDETAILSTAB_IDX_PASSWORD, &tci);
        tci.mask = TCIF_TEXT;
        tci.pszText = (WCHAR*)_("Advanced");
        TabCtrl_InsertItem(tmpHWND, SRCDETAILSTAB_IDX_ADVANCED, &tci);

        // Create the contents for the two tabs    
        hDumpLUKSSrcDetailsWndPassword = _dlgDumpLUKSWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_NEWVOLWIZARD_PASSWORD
                                       );
        hDumpLUKSSrcDetailsWndAdvanced = _dlgDumpLUKSWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_VOL_ADV_OPTS_LUKS
                                       );

        // Adjust controls for the context in which they will appear...
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_STATIC_MOUNTPOINT,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_EDIT_MOUNTPOINT,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndPassword,
                          IDC_STATIC_PASSWORDCONFIRM,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndPassword,
                          IDC_EDIT_PASSWORDCONFIRM,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_STATIC_SIZELIMIT,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_EDIT_SIZELIMIT,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_SPIN_SIZELIMIT,
                          FALSE
                         );
        SetControlVisible(
                          hDumpLUKSSrcDetailsWndAdvanced,
                          IDC_STATIC_SIZELIMITBYTES,
                          FALSE
                         );


        // Set defaults...
        // Password...
        SetDlgItemText(
                       hDumpLUKSSrcDetailsWndPassword,
                       IDC_EDIT_PASSWORD,
                       DEFAULT_PASSWORD
                      );

        SetupInstructions(
                         G_hInstance,
                         hDumpLUKSSrcDetailsWndPassword,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTWIZDUMPLUKSPASSWORD
                        );


        // Advanced...
        CheckDlgButton(
                       hDumpLUKSSrcDetailsWndAdvanced,    
                       IDC_CHECK_BASEIVCYPHERONHASHLENGTH,
                       BST_CHECKED
                      );

        }
    else if (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_DESTFILENAME)
        {
        SetStaticText(hDlg, IDC_STATIC_FILENAME, _("Dump filename:"));

        // Note: Only used when creating a new keyfile
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INST_DUMPLUKS_DESTFILE
                        );
        }

    _dlgDumpLUKSWizard_EnableDisableControls(hDlg);

    // Set menu buttons as on the first page
    _dlgDumpLUKSWizard_SetWizardButtons(0);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_DESTROY\n")));

    if (G_dlgDumpLUKSWizard_MenuBar != NULL)
        {
        DestroyWindow(G_dlgDumpLUKSWizard_MenuBar);
        G_dlgDumpLUKSWizard_MenuBar = NULL;
        }

    if (hDumpLUKSSrcDetailsWndPassword != NULL)
        {
        DestroyWindow(hDumpLUKSSrcDetailsWndPassword);
        hDumpLUKSSrcDetailsWndPassword = NULL;
        }
    if (hDumpLUKSSrcDetailsWndAdvanced != NULL)
        {
        DestroyWindow(hDumpLUKSSrcDetailsWndAdvanced);
        hDumpLUKSSrcDetailsWndAdvanced = NULL;
        }

    G_dlgDumpLUKSWizard_AllowFinish = FALSE;

    _dlgDumpLUKSWizard_SecZeroParams();

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// hTabWnd - The tab window
// hWnd - The window to be displayed on the tab
void _dlgDumpLUKSWizard_SizeWindowTab(
    HWND hTabWnd,
    HWND hWnd
)
{
    RECT rcTab;
    RECT rcAdjust;

    if (GetWindowRect(hTabWnd, &rcTab))
        {
        rcAdjust = rcTab;
        TabCtrl_AdjustRect(hTabWnd, FALSE, &rcAdjust);
        rcTab.bottom = rcAdjust.bottom;

        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Window: 0x%0.8x\n"), hWnd));
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  left  : %.3d\n"), rcTab.left));
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  top   : %.3d\n"), rcTab.top));
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  right : %.3d\n"), rcTab.right));
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  bottom: %.3d\n"), rcTab.bottom));

        SetWindowPos(
                     hWnd, 
                     NULL, 
                     rcTab.left, 
                     rcTab.top, 
                     (rcTab.right - rcTab.left),
                     (rcTab.bottom - rcTab.top), 
                     SWP_NOZORDER
                    );
        }

}


// =========================================================================
void _dlgDumpLUKSWizard_SizeWindowTab_CtrlID(
    HWND hTabParentWnd,
    int tabCtrlID,
    HWND hWnd
)
{
    HWND tmpHWND;

    if (
        (hTabParentWnd != NULL) &&
        (hWnd != NULL)
       )
        {
        tmpHWND = GetDlgItem(hTabParentWnd, tabCtrlID);
        if (tmpHWND != NULL)
            {
            _dlgDumpLUKSWizard_SizeWindowTab(tmpHWND, hWnd);
            }
        }
}


// =========================================================================
// Resize the property sheet page so that the tabs at the top are
// hidden by it; we don't want them shown for our wizard
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_WM_SIZE(HWND hDlg)
{
    RECT rcParent;
#if DBG
    // Nothing; tab header displayed under debug
#else
    RECT rcClient;
#endif
    static BOOL resizing = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_SIZE\n")));

    if (
        (hDlg == hDumpLUKSSrcDetailsWndPassword)  ||
        (hDlg == hDumpLUKSSrcDetailsWndAdvanced)
       )
        {
        // Do nothing; handed when the parent window (tabbed window) is
        // resized
        return TRUE;
        }

    // Prevent infinite recursion as we resize the dialog...
    if (!(resizing))
        {
        resizing = TRUE;

#if DBG
        // Nothing; tab header displayed under debug
#else
        // Get size of display area...
        GetClientRect(GetParent(hDlg), &rcClient);

        // Resize...
        SetWindowPos(
                     hDlg, 
                     NULL, 
                     0, 
                     0, 
                     rcClient.right,
                     rcClient.bottom,
                     SWP_NOZORDER
                    );
#endif

        // Resize specific property sheets...
        GetClientRect(hDlg, &rcParent);

        switch (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg))
            {
            case PAGE_IDX_SRCDETAILS:
                {
                // Resize src tab window...
                SizeControlToParent(hDlg, IDC_TAB1);

                // Resize the tabsheets within the src tab window...
                _dlgDumpLUKSWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hDumpLUKSSrcDetailsWndPassword
                                                            );
                _dlgDumpLUKSWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hDumpLUKSSrcDetailsWndAdvanced
                                                            );
                break;
                }

            }

        resizing = FALSE;
        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_SIZE\n")));
    return TRUE;
}


// =========================================================================
void _dlgDumpLUKSWizard_SecZeroParams()
{
    if (G_dlgDumpLUKSWizard_UserParams.SrcUserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgDumpLUKSWizard_UserParams.SrcUserPassword,
                    strlen(G_dlgDumpLUKSWizard_UserParams.SrcUserPassword)
                   );
        G_dlgDumpLUKSWizard_UserParams.SrcUserPassword = NULL;
        }

    SecZeroMemory(
                  &G_dlgDumpLUKSWizard_UserParams, 
                  sizeof(G_dlgDumpLUKSWizard_UserParams)
                 );
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgDumpLUKSWizard_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgDumpLUKSWizard_QuerySiblings\n")));

    G_dlgDumpLUKSWizard_UserParams.ParamsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_dlgDumpLUKSWizard_UserParams.ParamsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgDumpLUKSWizard_QuerySiblings\n")));
    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
void _dlgDumpLUKSWizard_UpdateLastPageGlobal(HWND hDlg)
{
    // Get the latest information entered by the user...
    _dlgDumpLUKSWizard_QuerySiblings(hDlg);

    G_dlgDumpLUKSWizard_LastPageIdx = PAGE_IDX_DESTFILENAME;

	// In case any of the navigation buttsons need changing...
    _dlgDumpLUKSWizard_SetWizardButtons(
            _dlgDumpLUKSWizard_GetCurrPageIdx(hDlg)
           );    
}


// =========================================================================
// Dialog OK'd; change password or create keyfile
BOOL CALLBACK dlgDumpLUKSWizard_Finish(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_Finish\n")));

    memset(
           &G_dlgDumpLUKSWizard_UserParams, 
           0,
           sizeof(G_dlgDumpLUKSWizard_UserParams)
          );
    G_dlgDumpLUKSWizard_UserParams.ParamsOK = TRUE;

    // Check the user's input is OK...
    if (!(dlgDumpLUKSWizard_ValidateAll(GetParent(hDlg))))
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Can't validate all of user input successfully\n")));
        // retval already set to FALSE, just switch to the appropriate tab
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            G_dlgDumpLUKSWizard_UserParams.ErrorPageIdx
                           );
        }
    else
        {
        HCURSOR csrHourglass;
        HCURSOR csrPrevious;

        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);

		retval = driver_DumpLUKS(
                G_dlgDumpLUKSWizard_UserParams.SrcFilename,
                G_dlgDumpLUKSWizard_UserParams.SrcUserPassword,
                G_dlgDumpLUKSWizard_UserParams.SrcBaseIVCypherOnHashLength,

                G_dlgDumpLUKSWizard_UserParams.DestFilename
               );

        SetCursor(csrPrevious);
        }

    if (retval)
        {
        _dlgDumpLUKSWizard_PromptDisplayDump(hDlg);                        
        }

    // Cleardown sensitive information
    _dlgDumpLUKSWizard_SecZeroParams();

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_Finish\n")));
    return retval;
}


// =========================================================================
void _dlgDumpLUKSWizard_EnableDisableControls(HWND hDlg)
{
    // Do nothing
}


// =========================================================================
// Navigate forwards/backwards in the wizard
// PageDirection - Set to +1 to go onto the next page; -1 to go back
void CALLBACK _dlgDumpLUKSWizard_PageChange(
    HWND hDlg, 
    int PageDirection
)
{
    int currPage;
    int newPage;
    BOOL identifiedNewPage;


    currPage = _dlgDumpLUKSWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg)));
    newPage = currPage;

    // Get the latest information entered by the user...
    _dlgDumpLUKSWizard_QuerySiblings(hDlg);

    identifiedNewPage = FALSE;
    while (!(identifiedNewPage))
        {
        if (PageDirection < 0) 
            {
            newPage = max((newPage - 1), 0);
            }
        else if (PageDirection > 0)
            {
            // Note: Any error message will have been displayed to user
            //       during validation
            if (dlgDumpLUKSWizard_ValidatePage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg))))
                {
                // -1 here as DUMP_LUKS_WIZARD_STEPS is the total number; not
                // the largest page indexe
                newPage = min((newPage + 1), (DUMP_LUKS_WIZARD_STEPS - 1));
                }
            }

        // Check if the new page is skipped
        identifiedNewPage = TRUE;
        }

    if (currPage != newPage)
        {
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            newPage
                           );
                
        }

    // It would make more sense for this to be called *after* all the
    // WM_INITDIALOGs
    _dlgDumpLUKSWizard_UpdateLastPageGlobal(GetParent(hDlg));
}


// =========================================================================
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));
            
    ctrlID = LOWORD(wParam);

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

        case ID_WIZ_BACK:
            {
            _dlgDumpLUKSWizard_PageChange(hDlg, -1);
            retval = TRUE;
            break;
            }

        case ID_WIZ_NEXT:
            {
            _dlgDumpLUKSWizard_PageChange(hDlg, +1);
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_BROWSEFILENAME:
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

            if (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_VOLUMES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_VOLUMES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_VOLUMES;
                }
            else
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_TEXTFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_TEXTFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_TEXTFILES;
                }

            ofn.lpstrFilter = useFilter;

            if (_dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
                {
                fileDlgOK = GetOpenFileName(&ofn);
                }
            else
                {
                fileDlgOK = GetSaveFileName(&ofn);
                }

            if (fileDlgOK)
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("File dialog OK'd...\n")));
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Filename: %ls\n"), filename));
                SetDlgItemText(hDlg, IDC_EDIT_FILENAME, filename);
                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("File dialog cancelled\n")));
                }

            FileFilterFree(useFilter);

            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgDumpLUKSWizard_ValidatePageIdx(HWND hDlg, int PageIdx)
{
    BOOL retval = FALSE;
    LARGE_INTEGER fileSize;
    int errorPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_ValidatePage\n")));

    errorPage = G_dlgDumpLUKSWizard_UserParams.ErrorPageIdx;

    if (!(_dlgDumpLUKSWizard_QuerySiblings(hDlg)))
        {
        // Do nothing; retval already set to FALSE
        }
    else
        {
        switch (PageIdx)
            {
            case PAGE_IDX_SRCFILENAME:
                {
                // Ensure that filename has been specified
                retval = (wcslen(G_dlgDumpLUKSWizard_UserParams.SrcFilename) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter the filename of an existing LUKS volume"));
                    errorPage = PAGE_IDX_SRCFILENAME;
                    break;
                    }
                else
                    {
                    // Ensure file exists
                    retval = SDUCheckFileExists(G_dlgDumpLUKSWizard_UserParams.SrcFilename);
                    if (!(retval))
                        {
                        MsgError(hDlg, _("The file specified does not exist. Please reenter"));
                        errorPage = PAGE_IDX_SRCFILENAME;
                        break;
                        }
                    else
                        {
                        retval = LUKS_IsLUKSVolume(G_dlgDumpLUKSWizard_UserParams.SrcFilename);
                        if (!(retval))
                            {
                            MsgError(hDlg, _("The file specified is not a LUKS encrypted volume."));
                            errorPage = PAGE_IDX_SRCFILENAME;
                            break;
                            }
                        }
                    }

                break;
                }

            case PAGE_IDX_SRCDETAILS:
                {
                // Password...

                // Ensure that something has been entered as a password
                if (G_dlgDumpLUKSWizard_UserParams.SrcUserPassword == NULL)
                    {
                    // <shrugs>
                    MsgError(hDlg, _("Error getting user password!"));                    
                    errorPage = PAGE_IDX_SRCDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                if (strlen(G_dlgDumpLUKSWizard_UserParams.SrcUserPassword) == 0)
                    {
                    MsgError(hDlg, _("Please enter a password"));
                    errorPage = PAGE_IDX_SRCDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                else
                    {
                    retval = TRUE;
                    }


                // Advanced...

                // Ensure offset specified is less than the size of the file
                retval = (wcslen(G_dlgDumpLUKSWizard_UserParams.SrcFilename) > 0);
                if (!(retval))
                    {
                    // Ignore error; handled by PAGE_IDX_SRCFILENAME
                    break;
                    }
                else
                    {
                    // Only carry out this check if the file already exists
                    if (SDUCheckFileExists(G_dlgDumpLUKSWizard_UserParams.SrcFilename))
                        {
                        // File exists...
                        if (!(GetFileSize_Filename(
                                  G_dlgDumpLUKSWizard_UserParams.SrcFilename,
                                  &fileSize
                                 )))
                            {
                            MsgError(hDlg, _("Unable to get filesize"));
                            errorPage = PAGE_IDX_SRCDETAILS;
                            break;
                            }
                        }
                    }
                
                break;
                }

            case PAGE_IDX_DESTFILENAME:
                {
                // Destination filename cannot already exist
                retval = (wcslen(G_dlgDumpLUKSWizard_UserParams.DestFilename) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please specify a filename for the LUKS dump file"));
                    errorPage = PAGE_IDX_DESTFILENAME;
                    break;
                    }
                else
                    {
                    retval = (!(SDUCheckFileExists(G_dlgDumpLUKSWizard_UserParams.DestFilename)));
                    if (!(retval))
                        {
                        MsgError(hDlg, _("A file with the filename specified already exists; please enter different filename"));
                        errorPage = PAGE_IDX_DESTFILENAME;
                        break;
                        }
                    }

                break;
                }

            default:
                {
                // This can't happen?!
                MsgError(hDlg, TEXT("Validation missed a page!"));
                MsgError(hDlg, _("Please report seeing this error!"));
                errorPage = PageIdx;
                retval = FALSE;
                }

            }
        }

    G_dlgDumpLUKSWizard_PageCompleted[PageIdx] = retval;
    if (!(retval))
        {
        G_dlgDumpLUKSWizard_UserParams.ErrorPageIdx = errorPage;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_ValidatePage\n")));
    return retval;
}


// =========================================================================
// Supply with the dialog to be validated
BOOL dlgDumpLUKSWizard_ValidatePage(HWND hDlg)
{
    BOOL retval;
    int pageIdx;

    pageIdx = _dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg);
    retval = dlgDumpLUKSWizard_ValidatePageIdx(GetParent(hDlg), pageIdx);

    return retval;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgDumpLUKSWizard_ValidateAll(HWND hDlg)
{
    BOOL retval = TRUE;
    int i;

    for (i = 0; i < DUMP_LUKS_WIZARD_STEPS; i++)
        {
        retval = retval && dlgDumpLUKSWizard_ValidatePageIdx(hDlg, i);
        }

    return retval;
}


// =========================================================================
// Note: This *purely* gets the data the user entered. No validation. No
//       corrections for values that can be autodetermined (validation does
//       that, where needed)
BOOL CALLBACK dlgDumpLUKSWizard_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    int pageIdx;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgDumpLUKSWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));

    pageIdx = _dlgDumpLUKSWizard_GetPageIdxOfPage(hDlg);

    switch (pageIdx)
        {
        case PAGE_IDX_SRCFILENAME:
            {
            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_FILENAME,
                           G_dlgDumpLUKSWizard_UserParams.SrcFilename,
                           sizeof(G_dlgDumpLUKSWizard_UserParams.SrcFilename)
                          );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_SRCDETAILS:
            {
            // Password...

            // Ditch any previously obtained password...
            if (G_dlgDumpLUKSWizard_UserParams.SrcUserPassword != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgDumpLUKSWizard_UserParams.SrcUserPassword,
                            strlen(G_dlgDumpLUKSWizard_UserParams.SrcUserPassword)
                           );
                G_dlgDumpLUKSWizard_UserParams.SrcUserPassword = NULL;
                }
            G_dlgDumpLUKSWizard_UserParams.SrcUserPassword = GetPassword(
                                                     hDumpLUKSSrcDetailsWndPassword,
                                                     IDC_EDIT_PASSWORD
                                                    );


            // Advanced...

            G_dlgDumpLUKSWizard_UserParams.SrcBaseIVCypherOnHashLength = (IsDlgButtonChecked(
                                              hDumpLUKSSrcDetailsWndAdvanced, 
                                              IDC_CHECK_BASEIVCYPHERONHASHLENGTH
                                             ) == BST_CHECKED);
        
            retval = TRUE;
            break;
            }

        case PAGE_IDX_DESTFILENAME:
            {
            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_FILENAME,
                           G_dlgDumpLUKSWizard_UserParams.DestFilename,
                           sizeof(G_dlgDumpLUKSWizard_UserParams.DestFilename)
                          );
            retval = TRUE;
            break;
            }

        default:
            {
            // This can't happen?!
            MsgError(hDlg, TEXT("Unknown page?!"));
            retval = FALSE;
            }

        }


    if (!(retval))
        {
        G_dlgDumpLUKSWizard_UserParams.ParamsOK = FALSE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgDumpLUKSWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgDumpLUKSWizard_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     )
{
    BOOL retval = FALSE;
    //static SHACTIVATEINFO s_sai = {0};

    switch(msg)
        {
        case WM_COMMAND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_COMMAND\n")));
            retval = dlgDumpLUKSWizard_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgDumpLUKSWizard_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgDumpLUKSWizard_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgDumpLUKSWizard_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
            break;
            }

        case WM_SIZE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SIZE\n")));
            retval = dlgDumpLUKSWizard_HandleMsg_WM_SIZE(hDlg);
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
            retval = dlgDumpLUKSWizard_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
