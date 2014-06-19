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
#include "dlgChpassKeyfileWizard.h"
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
#include "wndUserRNG.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"


// =========================================================================
// Constants...

// Relativly arbitary marker const so we can identify tabs we created as they
// receive WM_INITDIALOG msg
#define SUBTAB_FLAG 0x0ABCDEF0

// The number of steps in the wizard
#define CHPASS_KEYFILEWIZARD_STEPS 7

// Password change/keyfile generation...
#define PAGE_IDX_WELCOME            0
#define PAGE_IDX_SRCFILENAME        1
#define PAGE_IDX_SRCDETAILS         2
// Destination filename for keyfile generation only
#define PAGE_IDX_DESTFILENAME       3  
#define PAGE_IDX_DESTDETAILS        4
#define PAGE_IDX_RNGSELECT          5
#define PAGE_IDX_RNGMOUSEMOVEMENT   6


#define SRCDETAILSTAB_IDX_PASSWORD   0
#define SRCDETAILSTAB_IDX_ADVANCED   1

#define DESTDETAILSTAB_IDX_PASSWORD   0
#define DESTDETAILSTAB_IDX_ADVANCED   1

// =========================================================================
// Enums...

// Mount source
typedef enum _DLG_MODE {
    DLGMODE_CHPASS      =    1,
    DLGMODE_NEWKEYFILE  =    2
} DLG_MODE, *PDLG_MODE;


// =========================================================================
// Structures...

// Selected RNG
typedef struct _CHPASSKEYFILE_PARAMETERS {
    BOOL ParamsOK;
    int ErrorPageIdx;

    WCHAR SrcFilename[FREEOTFE_MAX_FILENAME_LENGTH];

    unsigned char* SrcUserPassword;
    LARGE_INTEGER SrcOffset;
    unsigned int SrcSaltLength;  // In *bits*
    unsigned int SrcKeyIterations;

    WCHAR DestFilename[FREEOTFE_MAX_FILENAME_LENGTH];

    unsigned char* DestUserPassword;
    unsigned char* DestUserPasswordConfirm;
    unsigned int DestSaltLength;  // In *bits*
    unsigned int DestKeyIterations;


    RNG_SYSTEM SelectedRNG;  // Combination of the RNGs

    // The following buffers store random data as collected...
    unsigned int RandomPoolFilled_UserInput;  // In *bits*
    FREEOTFEBYTE RandomPool_UserInput[(CRITICAL_DATA_LENGTH / 8)];
    unsigned int RandomPoolFilled_MSCryptoAPI;  // In *bits*
    FREEOTFEBYTE RandomPool_MSCryptoAPI[(CRITICAL_DATA_LENGTH / 8)];

} CHPASSKEYFILE_PARAMETERS, *PCHPASSKEYFILE_PARAMETERS;


// =========================================================================
// Local to this file...

HWND G_dlgChpassKeyfileWizard_MenuBar = NULL;
HWND G_dlgChpassKeyfileWizard_UserRNG = NULL;
BOOL G_dlgChpassKeyfileWizard_PageCompleted[CHPASS_KEYFILEWIZARD_STEPS];
CHPASSKEYFILE_PARAMETERS G_dlgChpassKeyfileWizard_ChpassKeyfileParams;

BOOL G_dlgChpassKeyfileWizard_AllowFinish = FALSE;

int G_dlgChpassKeyfileWizard_RandomUserInput_Filled;  // In *bits*
FREEOTFEBYTE G_dlgChpassKeyfileWizard_RandomUserInput_Data[(CRITICAL_DATA_LENGTH / 8)];

HWND hSrcDetailsWndPassword = NULL;
HWND hSrcDetailsWndAdvanced = NULL;
HWND hDestDetailsWndPassword = NULL;
HWND hDestDetailsWndAdvanced = NULL;

DLG_MODE G_dlgChpassKeyfileWizard_DlgMode;

// The last page changes depending on RNG selected
int G_dlgChpassKeyfileWizard_LastPageIdx = (CHPASS_KEYFILEWIZARD_STEPS - 1);

// =========================================================================
// Forward declarations...

void _dlgChpassKeyfileWizard_EnableDisableControls(HWND hDlg);

BOOL CALLBACK dlgChpassKeyfileWizard_ChpassKeyfile(HWND hDlg);
BOOL CALLBACK dlgChpassKeyfileWizard_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgChpassKeyfileWizard_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
void _dlgChpassKeyfileWizard_SecZeroParams();
BOOL dlgChpassKeyfileWizard_ValidatePage(HWND hDlg);
BOOL dlgChpassKeyfileWizard_ValidateAll(HWND hDlg);

void _dlgChpassKeyfileWizard_SizeWindowTab(
    HWND hTabWnd,
    HWND hWnd
);
void _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
    HWND hTabParentWnd,
    int tabCtrlID,
    HWND hWnd
);
BOOL _dlgChpassKeyfileWizard_QuerySiblings(HWND hDlg);
int _dlgChpassKeyfileWizard_GetCurrPageIdx(HWND hDlg);
int _dlgChpassKeyfileWizard_GetPageIdxOfPage(HWND hDlg);
void _DisplayDlgChpassKeyfileWizard(HWND hWnd);


// =========================================================================
void DisplayDlgChpassWizard(HWND hWnd)
{
    G_dlgChpassKeyfileWizard_DlgMode = DLGMODE_CHPASS;
    _DisplayDlgChpassKeyfileWizard(hWnd);
}


// =========================================================================
void DisplayDlgKeyfileWizard(HWND hWnd)
{
    G_dlgChpassKeyfileWizard_DlgMode = DLGMODE_NEWKEYFILE;
    _DisplayDlgChpassKeyfileWizard(hWnd);
}


// =========================================================================
void _DisplayDlgChpassKeyfileWizard(HWND hWnd)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[CHPASS_KEYFILEWIZARD_STEPS];
    int i;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgKeyfileWizard\n")));

    for (
         i = 0;
         i < (sizeof(G_dlgChpassKeyfileWizard_PageCompleted) / sizeof(G_dlgChpassKeyfileWizard_PageCompleted[0]));
         i++
        )
        {
        G_dlgChpassKeyfileWizard_PageCompleted[i] = FALSE;
        }

    memset(
           &G_dlgChpassKeyfileWizard_ChpassKeyfileParams, 
           0,
           sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams)
          );

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));


    // Welcome step...
    propPage[PAGE_IDX_WELCOME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_WELCOME].lParam      = PAGE_IDX_WELCOME;
    propPage[PAGE_IDX_WELCOME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_WELCOME].pszTitle    = _("Welcome");
    propPage[PAGE_IDX_WELCOME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_WELCOME].pszTemplate = MAKEINTRESOURCE(IDD_WIZARD_WELCOME);
    propPage[PAGE_IDX_WELCOME].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // Src filename step...
    propPage[PAGE_IDX_SRCFILENAME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SRCFILENAME].lParam      = PAGE_IDX_SRCFILENAME;
    propPage[PAGE_IDX_SRCFILENAME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SRCFILENAME].pszTitle    = _("Source filename");
    propPage[PAGE_IDX_SRCFILENAME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SRCFILENAME].pszTemplate = MAKEINTRESOURCE(IDD_FILENAME);
    propPage[PAGE_IDX_SRCFILENAME].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // Src details step...
    propPage[PAGE_IDX_SRCDETAILS].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_SRCDETAILS].lParam      = PAGE_IDX_SRCDETAILS;
    propPage[PAGE_IDX_SRCDETAILS].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_SRCDETAILS].pszTitle    = _("Source details");
    propPage[PAGE_IDX_SRCDETAILS].hInstance   = G_hInstance;
    propPage[PAGE_IDX_SRCDETAILS].pszTemplate = MAKEINTRESOURCE(IDD_DIALOG_TAB);
    propPage[PAGE_IDX_SRCDETAILS].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // Dest filename step...
    propPage[PAGE_IDX_DESTFILENAME].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_DESTFILENAME].lParam      = PAGE_IDX_DESTFILENAME;
    propPage[PAGE_IDX_DESTFILENAME].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_DESTFILENAME].pszTitle    = _("Destination filename");
    propPage[PAGE_IDX_DESTFILENAME].hInstance   = G_hInstance;
    propPage[PAGE_IDX_DESTFILENAME].pszTemplate = MAKEINTRESOURCE(IDD_FILENAME);
    propPage[PAGE_IDX_DESTFILENAME].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // Dest details step...
    propPage[PAGE_IDX_DESTDETAILS].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_DESTDETAILS].lParam      = PAGE_IDX_DESTDETAILS;
    propPage[PAGE_IDX_DESTDETAILS].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_DESTDETAILS].pszTitle    = _("Destination details");
    propPage[PAGE_IDX_DESTDETAILS].hInstance   = G_hInstance;
    propPage[PAGE_IDX_DESTDETAILS].pszTemplate = MAKEINTRESOURCE(IDD_DIALOG_TAB);
    propPage[PAGE_IDX_DESTDETAILS].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // RNG select step...
    propPage[PAGE_IDX_RNGSELECT].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_RNGSELECT].lParam      = PAGE_IDX_RNGSELECT;
    propPage[PAGE_IDX_RNGSELECT].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_RNGSELECT].pszTitle    = _("RNG select");
    propPage[PAGE_IDX_RNGSELECT].hInstance   = G_hInstance;
    propPage[PAGE_IDX_RNGSELECT].pszTemplate = MAKEINTRESOURCE(IDD_RNGSELECT);
    propPage[PAGE_IDX_RNGSELECT].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // RNG: Mouse movement step...
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].lParam      = PAGE_IDX_RNGMOUSEMOVEMENT;
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pszTitle    = _("RNG: Mouse movement");
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].hInstance   = G_hInstance;
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pszTemplate = MAKEINTRESOURCE(IDD_RNGMOUSEMOVEMENT);
    propPage[PAGE_IDX_RNGMOUSEMOVEMENT].pfnDlgProc  = dlgChpassKeyfileWizard_PropSheetPageProc;

    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_CHPASS)
        {
        propHeader.pszCaption = _("Change Password/Other Details");
        }
    else
        {
        propHeader.pszCaption = _("New keyfile");
        }
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = CHPASS_KEYFILEWIZARD_STEPS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgChpassKeyfileWizard_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgKeyfileWizard\n")));
}                


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgChpassKeyfileWizard_GetCurrPageIdx(HWND hDlg)
{
    return _dlgChpassKeyfileWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgChpassKeyfileWizard_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Returns 0 based index of the last page
int _dlgChpassKeyfileWizard_GetPageIdxOfLastPage()
{
    int retval;

    retval = G_dlgChpassKeyfileWizard_LastPageIdx;

    return retval;
}


// =========================================================================
void _dlgChpassKeyfileWizard_SetWizardButtons(int PageIdx)
{
    UINT leftSoftkeyCmdID;

    CommandBar_ItemEnable(
                     G_dlgChpassKeyfileWizard_MenuBar,
                     ID_WIZ_BACK,
                     (PageIdx > 0)
                    );

    CommandBar_ItemEnable(
                     G_dlgChpassKeyfileWizard_MenuBar,
                     ID_WIZ_NEXT,
                     (PageIdx < _dlgChpassKeyfileWizard_GetPageIdxOfLastPage())
                    );

    CommandBar_ItemEnable(
                     G_dlgChpassKeyfileWizard_MenuBar,
                     IDOK,
                     // G_dlgChpassKeyfileWizard_AllowFinish
                     (PageIdx == _dlgChpassKeyfileWizard_GetPageIdxOfLastPage())
                    );

    // Default to left softkey being "Next >"
    leftSoftkeyCmdID = ID_WIZ_NEXT;
    // If user can finish with wizard, set left softkey to "Finish"
    if (
        // G_dlgChpassKeyfileWizard_AllowFinish
        (PageIdx == _dlgChpassKeyfileWizard_GetPageIdxOfLastPage())
       )
        {
        leftSoftkeyCmdID = IDOK;
        }
    ChangeLeftSoftkey(
                      G_dlgChpassKeyfileWizard_MenuBar,
                      leftSoftkeyCmdID,
                      NULL
                     );

}


// =========================================================================
void _dlgChpassKeyfileWizard_ShowDetailsTabPage(HWND hWnd, BOOL show)
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
void _dlgChpassKeyfileWizard_DisplayDetailsTabPage(
    HWND hDlg,
    int CurrPage
)
{
    HWND tabHWND = NULL;
    HWND ctrlShow = NULL;
    int tabIdx;

    // Hide all.
    _dlgChpassKeyfileWizard_ShowDetailsTabPage(
                                              hSrcDetailsWndPassword, 
                                              FALSE
                                             );
    _dlgChpassKeyfileWizard_ShowDetailsTabPage(
                                              hSrcDetailsWndAdvanced, 
                                              FALSE
                                             );
    _dlgChpassKeyfileWizard_ShowDetailsTabPage(
                                              hDestDetailsWndPassword, 
                                              FALSE
                                             );
    _dlgChpassKeyfileWizard_ShowDetailsTabPage(
                                              hDestDetailsWndAdvanced, 
                                              FALSE
                                             );

    if (
        (CurrPage == PAGE_IDX_SRCDETAILS) ||
        (CurrPage == PAGE_IDX_DESTDETAILS)
       )
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
                    ctrlShow = hSrcDetailsWndPassword;
                    }
                else if (tabIdx == SRCDETAILSTAB_IDX_ADVANCED)
                    {
                    ctrlShow = hSrcDetailsWndAdvanced;
                    }
                }
            else if (CurrPage == PAGE_IDX_DESTDETAILS)
                {
                if (tabIdx == DESTDETAILSTAB_IDX_PASSWORD)
                    {
                    ctrlShow = hDestDetailsWndPassword;
                    }
                else if (tabIdx == DESTDETAILSTAB_IDX_ADVANCED)
                    {
                    ctrlShow = hDestDetailsWndAdvanced;
                    }
                }

            // If changing password on an existing volume/keyfile, set the
            // offset on the dest advanced tab to the src advanced tab
            if (
                (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_CHPASS) &&
                (ctrlShow == hDestDetailsWndAdvanced)
               )
                {
                SetDlgItemInt(
                              hDestDetailsWndAdvanced,
                              IDC_EDIT_OFFSET,
                              G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcOffset.LowPart,
                              TRUE
                             );
                }

            _dlgChpassKeyfileWizard_ShowDetailsTabPage(ctrlShow, TRUE);
            }
        }

}


// =========================================================================
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    NMUPDOWN* lpnmud;
    NMUSERRNG_BYTEGEN* lpnmRNG;
    int currPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case URNG_BYTEGEN:
            {
            if (idCtrl == IDCTRL_USERRNG)
                {
                lpnmRNG = (NMUSERRNG_BYTEGEN*)lParam;

                if ((G_dlgChpassKeyfileWizard_RandomUserInput_Filled / 8) < sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data))
                    {
                    // Store random byte...
                    G_dlgChpassKeyfileWizard_RandomUserInput_Data[G_dlgChpassKeyfileWizard_RandomUserInput_Filled / 8] = lpnmRNG->Byte;
                    G_dlgChpassKeyfileWizard_RandomUserInput_Filled += 8;

                    // Update display of bits generated...
                    SetDlgItemText_WithResize(
                                     hDlg, 
                                     IDC_STATIC_USERRNG_GENERATED_REQUIRED, 
                                     SDUParamSubstituteW(
                                             _("Random bits generated: %1/%2"),
                                             TEXT("%d"), G_dlgChpassKeyfileWizard_RandomUserInput_Filled,
                                             TEXT("%d"), (sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data) * 8),
                                             NULL
                                             )
                                    );

                    // Disable control if the userRNG pool is full...
                    if ((G_dlgChpassKeyfileWizard_RandomUserInput_Filled / 8) >= sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data))
                        {
                        SetControlEnabled(hDlg, IDCTRL_USERRNG, FALSE);
                        }

                    }
                
                }

            break;
            }

        case TCN_SELCHANGE:
            {
            currPage = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);

            if (
                (
                 (currPage == PAGE_IDX_SRCDETAILS) ||
                 (currPage == PAGE_IDX_DESTDETAILS) 
                ) &&
                (idCtrl == IDC_TAB1)
               )
                {
                _dlgChpassKeyfileWizard_DisplayDetailsTabPage(hDlg, currPage);
                retval = TRUE;
                }
            
            break;
            }

        // Spin edit up/down button
        case UDN_DELTAPOS:
            {
            switch(idCtrl)
                {
                case IDC_SPIN_OFFSET:
                    {
                    // Offset can only be specified for the src
                    if (hDlg == hSrcDetailsWndAdvanced)
                        {
                        lpnmud = (NMUPDOWN*)lParam;
                        AdjustSpinControl(
                                          hDlg, 
                                          IDC_EDIT_OFFSET, 
                                          0, 
                                          INT_MAX, 
                                          -(lpnmud->iDelta)
                                         );
                        }
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_SALTLENGTH:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_SALTLENGTH, 
                                      0, 
                                      INT_MAX, 
                                      (-(lpnmud->iDelta) * DELTA_SALT_LENGTH)
                                     );
                    retval = TRUE;
                    break;
                    }

                case IDC_SPIN_KEYITERATIONS:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_KEYITERATIONS, 
                                      MIN_KEYITERATIONS, 
                                      INT_MAX, 
                                      (-(lpnmud->iDelta) * DELTA_KEY_ITERATIONS)
                                     );
                    retval = TRUE;
                    break;
                    }

                }

            break;
            }

        case PSN_SETACTIVE:
            {
            currPage = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);

            _dlgChpassKeyfileWizard_DisplayDetailsTabPage(hDlg, currPage);
            _dlgChpassKeyfileWizard_SetWizardButtons(currPage);

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
            currPage = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PSN_APPLY for page idx: %d\n"), currPage));

            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
            if (dlgChpassKeyfileWizard_ValidatePage(hDlg))
                {
                // If it's the last page, only succeed if the volume can be
                // created
                if (currPage != _dlgChpassKeyfileWizard_GetPageIdxOfLastPage())
                    {
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else
                    {
                    if (dlgChpassKeyfileWizard_ChpassKeyfile(hDlg))
                        {
                        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
                            {
                            MsgInfo(hDlg, _("Keyfile created successfully."));
                            }
                        else
                            {
                            MsgInfo(hDlg, _("Volume/keyfile changed successfully."));
                            }

                        SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                        }
                    else
                        {
                        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
                            {
                            MsgError(hDlg, _("Unable to create keyfile; please check your password and other settings, and ensure you have enough storage space to hold a new keyfile"));
                            }
                        else
                            {
                            MsgError(hDlg, _("Unable to update the volume/keyfile as requested; please check your password and other settings"));
                            }

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


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgChpassKeyfileWizard_PropSheetProc(
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
HWND _dlgChpassKeyfileWizard_CreateDialogWindow(
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
                            dlgChpassKeyfileWizard_PropSheetPageProc,
                            SUBTAB_FLAG
                           );

}


// =========================================================================
// The first time the user RNG is setup, it must use the designtime 
// positioned placeholder. Subsequently, it can just resize it's width and
// depth itsself (having already got it's top border from the placeholder)
void _dlgChpassKeyfileWizard_ResizeUserRNG(
    HWND hDlg,
    BOOL usePlaceholder
)
{
    HWND hWndTmp;

    // We resize the userRNG such that it's as wide and deep as we
    // can make it, but with the top as being where the top of the
    // placeholder text was; this makes it easier to position on the
    // dialog at design time
    if (usePlaceholder)
        {
        SizeControlMaxWidth(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);
        SizeControlMaxDepth(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);

        hWndTmp = GetDlgItem(hDlg, IDC_STATIC_USERRNGPLACEHOLDER);
        SizeWindowToOverlapWindow(hWndTmp, G_dlgChpassKeyfileWizard_UserRNG);

        SetControlVisible(hDlg, IDC_STATIC_USERRNGPLACEHOLDER, FALSE);
        }
    else
        {
        SizeControlMaxWidth(hDlg, IDCTRL_USERRNG);
        SizeControlMaxDepth(hDlg, IDCTRL_USERRNG);
        }
}

// =========================================================================
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int i;
    UINT menuID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_INITDIALOG\n")));

    // Setup globals...

    G_dlgChpassKeyfileWizard_RandomUserInput_Filled = 0;
    memset(
        G_dlgChpassKeyfileWizard_RandomUserInput_Data, 
        0, 
        sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data)
       );

    // Don't recreate menu if it's already been done
    if (G_dlgChpassKeyfileWizard_MenuBar == NULL)
        {
        menuID = IDR_MENU_WIZARD;
        // If softkeys are supported, we use a menu with separators
        if (G_Options->SoftkeyMenus == TRUE)
            {
            menuID = IDR_MENU_WIZARD_SOFTKEY;
            }

        G_dlgChpassKeyfileWizard_MenuBar = SetupMenu(
                                               hDlg,
                                               menuID,
                                               ID_WIZ_NEXT,
                                               NULL
                                              );
        }

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgChpassKeyfileWizard_MenuBar);

    for (i = 0; i < CHPASS_KEYFILEWIZARD_STEPS; i++)
        {
        G_dlgChpassKeyfileWizard_PageCompleted[i] = FALSE;
        }
    memset(
           &G_dlgChpassKeyfileWizard_ChpassKeyfileParams, 
           0,
           sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams)
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
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORD,   DEFAULT_PASSWORD);
    SetDlgItemText(hDlg, IDC_EDIT_PASSWORDCONFIRM, DEFAULT_PASSWORD);
    SetDlgItemInt(hDlg, IDC_EDIT_OFFSET, DEFAULT_OFFSET, TRUE);

    // RNG select checkbox...
    if (DEFAULT_RND_USERINPUT)
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_RNGUSERINPUT,
                       BST_CHECKED
                      );
        }
    if (DEFAULT_RNG_MSCRYPTOAPI)
        {
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_RNGMSCRYPTOAPI,
                       BST_CHECKED
                      );
        }

    SetDlgItemInt(hDlg, IDC_EDIT_SALTLENGTH, DEFAULT_SALTLENGTH, TRUE);
    SetDlgItemInt(hDlg, IDC_EDIT_KEYITERATIONS, DEFAULT_KEYITERATIONS, TRUE);

    if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_WELCOME)
        {
        int titleString;
        int descString;

        SDUTextSetBold(hDlg, IDC_STATIC_WELCOME_TITLE, TRUE);

        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            titleString = IDS_STRING_WIZTITLE_NEWKEYFILE;
            descString = IDS_STRING_WIZDESC_NEWKEYFILE;
            }
        else
            {
            titleString = IDS_STRING_WIZTITLE_CHPASS;
            descString = IDS_STRING_WIZDESC_CHPASS;
            }

        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_TITLE,
                         titleString
                        );
        SetDlgItemString_WithResize(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_WELCOME_DESC,
                         descString
                        );
        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
        {
        SetStaticText(hDlg, IDC_STATIC_FILENAME, _("&Volume/keyfile:"));

        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            SetupInstructions(
                             G_hInstance,
                             hDlg,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZNEWKEYFILE_SRCFILE
                            );
            }
        else
            {
            SetupInstructions(
                             G_hInstance,
                             hDlg,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZCHPASS_SRCFILE
                            );
            }
        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCDETAILS)
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
        hSrcDetailsWndPassword = _dlgChpassKeyfileWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_NEWVOLWIZARD_PASSWORD
                                       );
        hSrcDetailsWndAdvanced = _dlgChpassKeyfileWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_VOL_ADV_OPTS
                                       );

        // Adjust controls for the context in which they will appear...
        SetControlVisible(
                          hSrcDetailsWndAdvanced,
                          IDC_STATIC_MOUNTPOINT,
                          FALSE
                         );
        SetControlVisible(
                          hSrcDetailsWndAdvanced,
                          IDC_EDIT_MOUNTPOINT,
                          FALSE
                         );
        SetControlVisible(
                          hSrcDetailsWndPassword,
                          IDC_STATIC_PASSWORDCONFIRM,
                          FALSE
                         );
        SetControlVisible(
                          hSrcDetailsWndPassword,
                          IDC_EDIT_PASSWORDCONFIRM,
                          FALSE
                         );


        // Set defaults...
        // Password...
        SetDlgItemText(
                       hSrcDetailsWndPassword,
                       IDC_EDIT_PASSWORD,
                       DEFAULT_PASSWORD
                      );

        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            SetupInstructions(
                             G_hInstance,
                             hSrcDetailsWndPassword,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZNEWKEYFILE_SRCPASSWORD
                            );
            }
        else
            {
            SetupInstructions(
                             G_hInstance,
                             hSrcDetailsWndPassword,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZCHPASS_SRCPASSWORD
                            );
            }


        // Advanced...
        SetDlgItemInt(
                      hSrcDetailsWndAdvanced, 
                      IDC_EDIT_OFFSET, 
                      DEFAULT_OFFSET,
                      TRUE
                     );
        CheckDlgButton(
                       hSrcDetailsWndAdvanced,    
                       IDC_CHECK_CDBATOFFSET,
                       BST_CHECKED
                      );
        SetControlEnabled(
                          hSrcDetailsWndAdvanced,
                          IDC_CHECK_CDBATOFFSET,
                          FALSE
                         );
        SetDlgItemInt(
                      hSrcDetailsWndAdvanced,
                      IDC_EDIT_SALTLENGTH,
                      DEFAULT_SALTLENGTH,
                      TRUE
                     );
        SetDlgItemInt(
                      hSrcDetailsWndAdvanced,
                      IDC_EDIT_KEYITERATIONS,
                      DEFAULT_KEYITERATIONS,
                      TRUE
                     );

        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_DESTFILENAME)
        {
        SetStaticText(hDlg, IDC_STATIC_FILENAME, _("New keyfile:"));

        // Note: Only used when creating a new keyfile
        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTWIZNEWKEYFILE_DESTFILE
                        );
        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_DESTDETAILS)
        {
        HWND tmpHWND;
        TCITEM tci;

        // Setup the tab control to get the WinCE look and feel...
        tmpHWND = GetDlgItem(hDlg, IDC_TAB1);
        SendMessage(tmpHWND, CCM_SETVERSION, COMCTL32_VERSION, 0); 
        
        tci.mask = TCIF_TEXT;
        tci.pszText = (WCHAR*)_("Password");
        TabCtrl_InsertItem(tmpHWND, DESTDETAILSTAB_IDX_PASSWORD, &tci);
        tci.mask = TCIF_TEXT;
        tci.pszText = (WCHAR*)_("Advanced");
        TabCtrl_InsertItem(tmpHWND, DESTDETAILSTAB_IDX_ADVANCED, &tci);

        // Create the contents for the two tabs        
        hDestDetailsWndPassword = _dlgChpassKeyfileWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_NEWVOLWIZARD_PASSWORD
                                       );
        hDestDetailsWndAdvanced = _dlgChpassKeyfileWizard_CreateDialogWindow(
                                        tmpHWND,
                                        IDD_VOL_ADV_OPTS
                                       );

        SetControlVisible(
                          hDestDetailsWndAdvanced,
                          IDC_STATIC_MOUNTPOINT,
                          FALSE
                         );
        SetControlVisible(
                          hDestDetailsWndAdvanced,
                          IDC_EDIT_MOUNTPOINT,
                          FALSE
                         );

        // Set defaults...
        // Password...
        SetDlgItemText(
                       hDestDetailsWndPassword, 
                       IDC_EDIT_PASSWORD, 
                       DEFAULT_PASSWORD
                      );
        SetDlgItemText(
                       hDestDetailsWndPassword,
                       IDC_EDIT_PASSWORDCONFIRM, 
                       DEFAULT_PASSWORD
                      );

        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            SetupInstructions(
                             G_hInstance,
                             hDestDetailsWndPassword,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZNEWKEYFILE_DESTPASSWORD
                            );
            }
        else
            {
            SetupInstructions(
                             G_hInstance,
                             hDestDetailsWndPassword,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZCHPASS_DESTPASSWORD
                            );
            }


        // Advanced...
        SetDlgItemInt(
                      hDestDetailsWndAdvanced, 
                      IDC_EDIT_OFFSET, 
                      DEFAULT_OFFSET,
                      TRUE
                     );
        SetControlReadonly(
                          hDestDetailsWndAdvanced,
                          IDC_EDIT_OFFSET,
                          TRUE
                         );
        CheckDlgButton(
                       hDestDetailsWndAdvanced,    
                       IDC_CHECK_CDBATOFFSET,
                       BST_CHECKED
                      );
        SetControlEnabled(
                          hDestDetailsWndAdvanced,
                          IDC_CHECK_CDBATOFFSET,
                          FALSE
                         );
        SetDlgItemInt(
                      hDestDetailsWndAdvanced,
                      IDC_EDIT_SALTLENGTH,
                      DEFAULT_SALTLENGTH,
                      TRUE
                     );
        SetDlgItemInt(
                      hDestDetailsWndAdvanced,
                      IDC_EDIT_KEYITERATIONS,
                      DEFAULT_KEYITERATIONS,
                      TRUE
                     );

        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_RNGSELECT)
        {
        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            SetupInstructions(
                             G_hInstance,
                             hDlg,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZNEWKEYFILE_RNGSELECT
                            );
            }
        else
            {
            SetupInstructions(
                             G_hInstance,
                             hDlg,
                             IDC_STATIC_INSTRUCTION,
                             IDS_STRING_INSTWIZCHPASS_RNGSELECT
                            );
            }

        }
    else if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_RNGMOUSEMOVEMENT)
        {
        G_dlgChpassKeyfileWizard_UserRNG = CreateWindow(
                              USERRNG_CONTROL,      // Class name
                              USERRNG_CONTROL,      // Window title       
                              (WS_VISIBLE | WS_BORDER),     // Style
                              CW_USEDEFAULT,  // X
                              CW_USEDEFAULT,  // Y
                              CW_USEDEFAULT,  // Width
                              CW_USEDEFAULT,  // Height
                              hDlg,           // Parent window
                              (HMENU)IDCTRL_USERRNG, // Menu/CtrlID
                              G_hInstance, 
                              NULL
                             );

        if (G_dlgChpassKeyfileWizard_UserRNG == NULL)      
            {
            MsgError(hDlg, _("Unable to create user RNG"));
            }
        else
            {
            _dlgChpassKeyfileWizard_ResizeUserRNG(hDlg, TRUE);
            }

        SetDlgItemText_WithResize(
                         hDlg, 
                         IDC_STATIC_USERRNG_GENERATED_REQUIRED, 
                         SDUParamSubstituteW(
                                 _("Random bits generated: %1/%2"),
                                 TEXT("%d"), G_dlgChpassKeyfileWizard_RandomUserInput_Filled,
                                 TEXT("%d"), (sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data) * 8),
                                 NULL
                                 )
                        );

        SetupInstructions(
                         G_hInstance,
                         hDlg,
                         IDC_STATIC_INSTRUCTION,
                         IDS_STRING_INSTR_RNGMOUSEMOVEMENT
                        );
        }

    _dlgChpassKeyfileWizard_EnableDisableControls(hDlg);

    // Set menu buttons as on the first page
    _dlgChpassKeyfileWizard_SetWizardButtons(0);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_DESTROY\n")));

    if (G_dlgChpassKeyfileWizard_MenuBar != NULL)
        {
        DestroyWindow(G_dlgChpassKeyfileWizard_MenuBar);
        G_dlgChpassKeyfileWizard_MenuBar = NULL;
        }

    if (G_dlgChpassKeyfileWizard_UserRNG != NULL)
        {
        DestroyWindow(G_dlgChpassKeyfileWizard_UserRNG);
        G_dlgChpassKeyfileWizard_UserRNG = NULL;
        }

    if (hSrcDetailsWndPassword != NULL)
        {
        DestroyWindow(hSrcDetailsWndPassword);
        hSrcDetailsWndPassword = NULL;
        }
    if (hSrcDetailsWndAdvanced != NULL)
        {
        DestroyWindow(hSrcDetailsWndAdvanced);
        hSrcDetailsWndAdvanced = NULL;
        }
    if (hDestDetailsWndPassword != NULL)
        {
        DestroyWindow(hDestDetailsWndPassword);
        hDestDetailsWndPassword = NULL;
        }
    if (hDestDetailsWndAdvanced != NULL)
        {
        DestroyWindow(hDestDetailsWndAdvanced);
        hDestDetailsWndAdvanced = NULL;
        }

    G_dlgChpassKeyfileWizard_RandomUserInput_Filled = 0;
    SecZeroMemory(
        G_dlgChpassKeyfileWizard_RandomUserInput_Data, 
        sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data)
       );

    G_dlgChpassKeyfileWizard_AllowFinish = FALSE;

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// hTabWnd - The tab window
// hWnd - The window to be displayed on the tab
void _dlgChpassKeyfileWizard_SizeWindowTab(
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
void _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
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
            _dlgChpassKeyfileWizard_SizeWindowTab(tmpHWND, hWnd);
            }
        }
}


// =========================================================================
// Resize the property sheet page so that the tabs at the top are
// hidden by it; we don't want them shown for our wizard
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_WM_SIZE(HWND hDlg)
{
    RECT rcParent;
#if DBG
    // Nothing; tab header displayed under debug
#else
    RECT rcClient;
#endif
    static BOOL resizing = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_SIZE\n")));

    if (
        (hDlg == hSrcDetailsWndPassword)  ||
        (hDlg == hSrcDetailsWndAdvanced)  ||
        (hDlg == hDestDetailsWndPassword) ||
        (hDlg == hDestDetailsWndAdvanced)
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

        switch (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg))
            {
            case PAGE_IDX_WELCOME:
                {
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_TITLE, 
                                          FREEOTFE_DLG_BORDER
                                         );
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_DESC, 
                                          FREEOTFE_DLG_BORDER
                                         );
                SizeControlMaxWidthBorder(
                                          hDlg, 
                                          IDC_STATIC_WELCOME_NEXT, 
                                          FREEOTFE_DLG_BORDER
                                         );

                break;
                }

            case PAGE_IDX_SRCDETAILS:
                {
                // Resize src tab window...
                SizeControlToParent(hDlg, IDC_TAB1);

                // Resize the tabsheets within the src tab window...
                _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hSrcDetailsWndPassword
                                                            );
                _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hSrcDetailsWndAdvanced
                                                            );
                break;
                }

            case PAGE_IDX_DESTDETAILS:
                {
                // Resize dest tab window...
                SizeControlToParent(hDlg, IDC_TAB1);

                // Resize the tabsheets within the dest tab window...
                _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hDestDetailsWndPassword
                                                            );
                _dlgChpassKeyfileWizard_SizeWindowTab_CtrlID(
                                                             hDlg, 
                                                             IDC_TAB1, 
                                                             hDestDetailsWndAdvanced
                                                            );
                break;
                }

            case PAGE_IDX_RNGMOUSEMOVEMENT:
                {
                _dlgChpassKeyfileWizard_ResizeUserRNG(hDlg, FALSE);
                break;
                }

            }

        resizing = FALSE;
        }


    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_SIZE\n")));
    return TRUE;
}


// =========================================================================
void _dlgChpassKeyfileWizard_SecZeroParams()
{
    if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword,
                    strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword)
                   );
        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword = NULL;
        }

    if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword,
                    strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword)
                   );
        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword = NULL;
        }

    if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm != NULL)
        {
        SecZeroAndFreeMemory(
                    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm,
                    strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm)
                   );
        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm = NULL;
        }

    SecZeroMemory(
                  &G_dlgChpassKeyfileWizard_ChpassKeyfileParams, 
                  sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams)
                 );
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgChpassKeyfileWizard_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgChpassKeyfileWizard_QuerySiblings\n")));

    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ParamsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ParamsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgChpassKeyfileWizard_QuerySiblings\n")));
    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
void _dlgChpassKeyfileWizard_UpdateLastPageGlobal(HWND hDlg)
{
    // Get the latest information entered by the user...
    _dlgChpassKeyfileWizard_QuerySiblings(hDlg);

    if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput)
        {
        G_dlgChpassKeyfileWizard_LastPageIdx = PAGE_IDX_RNGMOUSEMOVEMENT;
        }
    else
        {
        G_dlgChpassKeyfileWizard_LastPageIdx = PAGE_IDX_RNGSELECT;
        }

    // In case any of the navigation buttsons need changing...
    _dlgChpassKeyfileWizard_SetWizardButtons(
            _dlgChpassKeyfileWizard_GetCurrPageIdx(hDlg)
           );    
}


// =========================================================================
// Dialog OK'd; change password or create keyfile
BOOL CALLBACK dlgChpassKeyfileWizard_ChpassKeyfile(HWND hDlg)
{
    BOOL retval = FALSE;
    CDB_DETAILS CDBDetails;
    CDB_METADATA CDBMetaData;
    FREEOTFEBYTE randomPool[(CRITICAL_DATA_LENGTH / 8)];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_ChpassKeyfile\n")));

    memset(
           &G_dlgChpassKeyfileWizard_ChpassKeyfileParams, 
           0,
           sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams)
          );
    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ParamsOK = TRUE;

    // Check the user's input is OK...
    if (!(dlgChpassKeyfileWizard_ValidateAll(GetParent(hDlg))))
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Can't validate all of user input successfully\n")));
        // retval already set to FALSE, just switch to the appropriate tab
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ErrorPageIdx
                           );
        }
    else
        {
        HCURSOR csrHourglass;
        HCURSOR csrPrevious;

        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);

        // Combine random data buffers as configured by the user...
        memset(randomPool, 0, sizeof(randomPool));
        if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput)
            {
            XORBlock(
                     randomPool,
                     G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_UserInput,
                     randomPool,
                     (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_UserInput / 8)
                    );
            }

        if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI)
            {
            XORBlock(
                     randomPool,
                     G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_MSCryptoAPI,
                     randomPool,
                     (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_MSCryptoAPI / 8)
                    );
            }

        retval = FALSE;
        if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
            {
            retval = driver_CreateKeyfile(
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcOffset,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcSaltLength,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcKeyIterations,

                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestFilename,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestSaltLength,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestKeyIterations,

                randomPool
               );

            }
        else
            {
            retval = driver_ChangeDetails(
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcOffset,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcSaltLength,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcKeyIterations,

                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestSaltLength,
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestKeyIterations,

                randomPool
               );

            }

        SetCursor(csrPrevious);
        }

    // Cleardown sensitive information
    _dlgChpassKeyfileWizard_SecZeroParams();
    SecZeroMemory(&CDBDetails, sizeof(CDBDetails));
    SecZeroMemory(&CDBMetaData, sizeof(CDBMetaData));
    SecZeroMemory(randomPool, sizeof(randomPool));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_ChpassKeyfile\n")));
    return retval;
}


// =========================================================================
void _dlgChpassKeyfileWizard_EnableDisableControls(HWND hDlg)
{
    BOOL enableCtrl;

    enableCtrl = (IsDlgButtonChecked(
                                     hDlg, 
                                     IDC_RADIO_CDBINKEYFILE
                                    ) == BST_CHECKED);

    SetControlEnabled(hDlg, IDC_STATIC_KEYFILE, enableCtrl);
    SetControlEnabled(hDlg, IDC_EDIT_KEYFILE, enableCtrl);
    SetControlEnabled(hDlg, IDC_BUTTON_BROWSEKEYFILE, enableCtrl);

}


// =========================================================================
// Navigate forwards/backwards in the wizard
// PageDirection - Set to +1 to go onto the next page; -1 to go back
void CALLBACK _dlgChpassKeyfileWizard_PageChange(
    HWND hDlg, 
    int PageDirection
)
{
    int currPage;
    int newPage;
    BOOL identifiedNewPage;


    currPage = _dlgChpassKeyfileWizard_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg)));
    newPage = currPage;

    // Get the latest information entered by the user...
    _dlgChpassKeyfileWizard_QuerySiblings(hDlg);

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
            if (dlgChpassKeyfileWizard_ValidatePage(PropSheet_GetCurrentPageHwnd(GetParent(hDlg))))
                {
                // -1 here as NEWVOLWIZARD_STEPS is the total number; not
                // the largest page indexe
                newPage = min((newPage + 1), (CHPASS_KEYFILEWIZARD_STEPS - 1));
                }
            }

        // Check if the new page is skipped
        identifiedNewPage = TRUE;
        if (newPage == PAGE_IDX_RNGMOUSEMOVEMENT)
            {
            // Skip mouse RNG page if user doesn't want to use it
            identifiedNewPage = G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput;
            }
        else if (newPage == PAGE_IDX_DESTFILENAME)
            {
            // Skip destination filename if just changing password, otherwise
            // allow this page
            identifiedNewPage = (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE);
            }

        }

    if (currPage != newPage)
        {
        PropSheet_SetCurSel(
                            GetParent(hDlg), 
                            NULL, 
                            newPage
                           );
                
        if (newPage == PAGE_IDX_RNGMOUSEMOVEMENT)
            {
            // Identify if the user needs instructions...
            if (
                (G_dlgChpassKeyfileWizard_RandomUserInput_Filled / 8) <
                sizeof(G_dlgChpassKeyfileWizard_RandomUserInput_Data)
               )
                {
                MsgInfoString(
                              hDlg,
                              G_hInstance,
                              IDS_STRING_INSTR_RNGMOUSEMOVEMENT
                             );
                }
            }

        }

    // It would make more sense for this to be called *after* all the
    // WM_INITDIALOGs
    _dlgChpassKeyfileWizard_UpdateLastPageGlobal(GetParent(hDlg));
}


// =========================================================================
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_COMMAND\n")));
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
            _dlgChpassKeyfileWizard_PageChange(hDlg, -1);
            retval = TRUE;
            break;
            }

        case ID_WIZ_NEXT:
            {
            _dlgChpassKeyfileWizard_PageChange(hDlg, +1);
            retval = TRUE;
            break;
            }

        case IDC_CHECK_RNGMSCRYPTOAPI:
        case IDC_CHECK_RNGUSERINPUT:
            {
            int currPage;

            // If the user selected a different RNG, update the "<BACK"/"NEXT>"
            // navigation; the user may have other steps to carry out
            _dlgChpassKeyfileWizard_UpdateLastPageGlobal(GetParent(hDlg));
            currPage = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);
            _dlgChpassKeyfileWizard_SetWizardButtons(currPage);

            break;
            }

        case IDC_BUTTON_BROWSEFILENAME:
        case IDC_BUTTON_BROWSEKEYFILE:
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

            if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_VOLUMESANDKEYFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_VOLUMESANDKEYFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
                }
            else
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_KEYFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_KEYFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_KEYFILES;
                }

            ofn.lpstrFilter = useFilter;

            if (_dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg) == PAGE_IDX_SRCFILENAME)
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

        case IDC_RADIO_CDBINVOLUME:
        case IDC_RADIO_CDBINKEYFILE:
            {
            _dlgChpassKeyfileWizard_EnableDisableControls(hDlg);
            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgChpassKeyfileWizard_ValidatePageIdx(HWND hDlg, int PageIdx)
{
    BOOL retval = FALSE;
    LARGE_INTEGER fileSize;
    int errorPage;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_ValidatePage\n")));

    errorPage = G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ErrorPageIdx;

    if (!(_dlgChpassKeyfileWizard_QuerySiblings(hDlg)))
        {
        // Do nothing; retval already set to FALSE
        }
    else
        {
        switch (PageIdx)
            {
            case PAGE_IDX_WELCOME:
                {
                // Always completed
                retval = TRUE;
                break;
                }

            case PAGE_IDX_SRCFILENAME:
                {
                // Ensure that filename has been specified
                retval = (wcslen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename) > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter the filename of an existing volume (with CDB) or keyfile"));
                    errorPage = PAGE_IDX_SRCFILENAME;
                    break;
                    }
                else
                    {
                    // Ensure file exists
                    retval = SDUCheckFileExists(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename);
                    if (!(retval))
                        {
                        MsgError(hDlg, _("The file specified does not exist. Please reenter"));
                        errorPage = PAGE_IDX_SRCFILENAME;
                        break;
                        }
                    }

                break;
                }

            case PAGE_IDX_SRCDETAILS:
                {
                // Password...

                // Ensure that something has been entered as a password
                if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword == NULL)
                    {
                    // <shrugs>
                    MsgError(hDlg, _("Error getting user password!"));                    
                    errorPage = PAGE_IDX_SRCDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                if (strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword) == 0)
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
                retval = (wcslen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename) > 0);
                if (!(retval))
                    {
                    // Ignore error; handled by PAGE_IDX_SRCFILENAME
                    break;
                    }
                else
                    {
                    // Only carry out this check if the file already exists
                    if (SDUCheckFileExists(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename))
                        {
                        // File exists...
                        if (!(GetFileSize_Filename(
                                  G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename,
                                  &fileSize
                                 )))
                            {
                            MsgError(hDlg, _("Unable to get filesize"));
                            errorPage = PAGE_IDX_SRCDETAILS;
                            break;
                            }
                        else
                            {
                            retval = (fileSize.QuadPart > G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcOffset.QuadPart);
                            if (!(retval))
                                {
                                MsgError(hDlg, _("The offset specified is larger than the file selected!"));
                                errorPage = PAGE_IDX_SRCDETAILS;
                                break;
                                }
                            }
                        }
                    }
                
                // Ensure salt is a multiple of 8
                retval = ((G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcSaltLength % 8) == 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("The salt length must be a multiple of 8"));
                    errorPage = PAGE_IDX_SRCDETAILS;
                    break;
                    }

                retval = (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcKeyIterations > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter a number of key iterations greater than one"));
                    errorPage = PAGE_IDX_SRCDETAILS;
                    break;
                    }

                break;
                }

            case PAGE_IDX_DESTFILENAME:
                {
                // Destination filename for keyfile generation only!
                retval = TRUE;
                if (G_dlgChpassKeyfileWizard_DlgMode == DLGMODE_NEWKEYFILE)
                    {
                    retval = (wcslen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestFilename) > 0);
                    if (!(retval))
                        {
                        MsgError(hDlg, _("Please specify a filename for the new keyfile"));
                        errorPage = PAGE_IDX_DESTFILENAME;
                        break;
                        }
                    else
                        {
                        retval = (!(SDUCheckFileExists(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestFilename)));
                        if (!(retval))
                            {
                            MsgError(hDlg, _("A file with the filename specified already exists; please enter different filename"));
                            errorPage = PAGE_IDX_DESTFILENAME;
                            break;
                            }
                        }
                    }

                break;
                }

            case PAGE_IDX_DESTDETAILS:
                {
                // Password...

                // Ensure the password is confirmed, and something has been
                // entered as a password
                if (
                    (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword == NULL) ||
                    (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm == NULL)
                   )
                    {
                    // <shrugs>
                    MsgError(hDlg, _("Error getting user password!"));                    
                    errorPage = PAGE_IDX_DESTDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                if (strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword) == 0)
                    {
                    MsgError(hDlg, _("Please enter a password"));
                    errorPage = PAGE_IDX_DESTDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                else if (strcmp(
                                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword,
                                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm
                               ) != 0)
                    {
                    MsgError(hDlg, _("Passwords entered do not match"));
                    errorPage = PAGE_IDX_DESTDETAILS;
                    // retval already set to FALSE
                    break;
                    }
                else
                    {
                    retval = TRUE;
                    }


                // Advanced...

                // Ensure salt is a multiple of 8
                retval = ((G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestSaltLength % 8) == 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("The salt length must be a multiple of 8"));
                    errorPage = PAGE_IDX_DESTDETAILS;
                    break;
                    }

                retval = (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestKeyIterations > 0);
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please enter a number of key iterations greater than one"));
                    errorPage = PAGE_IDX_DESTDETAILS;
                    break;
                    }

                break;
                }

            case PAGE_IDX_RNGSELECT:
                {
                // Always completed (RNG defaults; this should never be FALSE)
                retval = (
                          G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput ||
                          G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI
                         );
                if (!(retval))
                    {
                    MsgError(hDlg, _("Please select an RNG"));
                    errorPage = PAGE_IDX_RNGSELECT;
                    break;
                    }

                if (retval)
                    {
                    // Check RNG functioned correctly
                    if (
                        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI &&
                        (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_MSCryptoAPI < CRITICAL_DATA_LENGTH)
                       )
                        {
                        MsgError(hDlg, _("Microsoft CryptoAPI failed to produce sufficient random data."));
                        errorPage = PAGE_IDX_RNGSELECT;
                        retval = FALSE;
                        break;
                        }
                    }

                break;
                }

            case PAGE_IDX_RNGMOUSEMOVEMENT:
                {
                retval = TRUE;
                if (
                    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput &&
                    (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_UserInput < CRITICAL_DATA_LENGTH)
                   )
                    {
                    MsgError(hDlg, _("User input has not yet generated sufficient random data."));
                    errorPage = PAGE_IDX_RNGMOUSEMOVEMENT;
                    retval = FALSE;
                    break;
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

    G_dlgChpassKeyfileWizard_PageCompleted[PageIdx] = retval;
    if (!(retval))
        {
        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ErrorPageIdx = errorPage;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_ValidatePage\n")));
    return retval;
}


// =========================================================================
// Supply with the dialog to be validated
BOOL dlgChpassKeyfileWizard_ValidatePage(HWND hDlg)
{
    BOOL retval;
    int pageIdx;

    pageIdx = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);
    retval = dlgChpassKeyfileWizard_ValidatePageIdx(GetParent(hDlg), pageIdx);

    return retval;
}


// =========================================================================
// Supply with the *main* dialog
BOOL dlgChpassKeyfileWizard_ValidateAll(HWND hDlg)
{
    BOOL retval = TRUE;
    int i;

    for (i = 0; i < CHPASS_KEYFILEWIZARD_STEPS; i++)
        {
        retval = retval && dlgChpassKeyfileWizard_ValidatePageIdx(hDlg, i);
        }

    return retval;
}


// =========================================================================
// Note: This *purely* gets the data the user entered. No validation. No
//       corrections for values that can be autodetermined (validation does
//       that, where needed)
BOOL CALLBACK dlgChpassKeyfileWizard_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    int pageIdx;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgChpassKeyfileWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));

    pageIdx = _dlgChpassKeyfileWizard_GetPageIdxOfPage(hDlg);

    switch (pageIdx)
        {
        case PAGE_IDX_WELCOME:
            {
            // Do nothing
            retval = TRUE;
            break;
            }

        case PAGE_IDX_SRCFILENAME:
            {
            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_FILENAME,
                           G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename,
                           sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcFilename)
                          );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_SRCDETAILS:
            {
            // Password...

            // Ditch any previously obtained password...
            if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword,
                            strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword)
                           );
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword = NULL;
                }
            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcUserPassword = GetPassword(
                                                     hSrcDetailsWndPassword,
                                                     IDC_EDIT_PASSWORD
                                                    );


            // Advanced...

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcOffset.QuadPart = GetDlgItemInt(
                                   hSrcDetailsWndAdvanced,
                                   IDC_EDIT_OFFSET,
                                   NULL,
                                   FALSE
                                  );

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcSaltLength = GetDlgItemInt(
                                   hSrcDetailsWndAdvanced,
                                   IDC_EDIT_SALTLENGTH,
                                   NULL,
                                   FALSE
                                  );

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SrcKeyIterations = GetDlgItemInt(
                                          hSrcDetailsWndAdvanced,
                                          IDC_EDIT_KEYITERATIONS,
                                          NULL,
                                          FALSE
                                         );

            retval = TRUE;
            break;
            }

        case PAGE_IDX_DESTFILENAME:
            {
            GetDlgItemText( 
                           hDlg, 
                           IDC_EDIT_FILENAME,
                           G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestFilename,
                           sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestFilename)
                          );
            retval = TRUE;
            break;
            }

        case PAGE_IDX_DESTDETAILS:
            {
            // Password...

            // Ditch any previously obtained password...
            if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword,
                            strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword)
                           );
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword = NULL;
                }

            // Ditch any previously obtained confirmation password...
            if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm != NULL)
                {
                SecZeroAndFreeMemory(
                            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm,
                            strlen(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm)
                           );
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm = NULL;
                }
            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword = GetPassword(
                                                     hDestDetailsWndPassword,
                                                     IDC_EDIT_PASSWORD
                                                    );

            if (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPassword == NULL)
                {
                // Do nothing; no point getting confirmation password
                }
            else
                {
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestUserPasswordConfirm = GetPassword(
                                                         hDestDetailsWndPassword,
                                                         IDC_EDIT_PASSWORDCONFIRM
                                                        );
                }


            // Advanced...

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestSaltLength = GetDlgItemInt(
                                   hDestDetailsWndAdvanced,
                                   IDC_EDIT_SALTLENGTH,
                                   NULL,
                                   FALSE
                                  );

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.DestKeyIterations = GetDlgItemInt(
                                          hSrcDetailsWndAdvanced,
                                          IDC_EDIT_KEYITERATIONS,
                                          NULL,
                                          FALSE
                                         );

            retval = TRUE;
            break;
            }

        case PAGE_IDX_RNGSELECT:
            {
            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput = FALSE;
            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI = FALSE;

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseUserInput =
                            (IsDlgButtonChecked(
                                                hDlg, 
                                                IDC_CHECK_RNGUSERINPUT
                                               ) == BST_CHECKED);

            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI = 
                            (IsDlgButtonChecked(
                                                hDlg, 
                                                IDC_CHECK_RNGMSCRYPTOAPI
                                               ) == BST_CHECKED);

            // Generate MS CryptoAPI random data, if needed
            if (
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.SelectedRNG.UseMSCryptoAPI &&
                (G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_MSCryptoAPI < CRITICAL_DATA_LENGTH)
               )
                {
                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_MSCryptoAPI = 0;
                if (GenerateRNGDataMSCryptoAPI(
                                sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_MSCryptoAPI),
                                G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_MSCryptoAPI
                                ))
                    {
                    G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_MSCryptoAPI  = (sizeof(G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_MSCryptoAPI) * 8);
                    }

                }

            retval = TRUE;
            break;
            }

        case PAGE_IDX_RNGMOUSEMOVEMENT:
            {
            G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPoolFilled_UserInput = G_dlgChpassKeyfileWizard_RandomUserInput_Filled; 
            memcpy(
                   G_dlgChpassKeyfileWizard_ChpassKeyfileParams.RandomPool_UserInput,
                   G_dlgChpassKeyfileWizard_RandomUserInput_Data,
                   (G_dlgChpassKeyfileWizard_RandomUserInput_Filled / 8)
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
        G_dlgChpassKeyfileWizard_ChpassKeyfileParams.ParamsOK = FALSE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgChpassKeyfileWizard_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgChpassKeyfileWizard_PropSheetPageProc(
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
            retval = dlgChpassKeyfileWizard_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgChpassKeyfileWizard_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgChpassKeyfileWizard_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgChpassKeyfileWizard_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
            break;
            }

        case WM_SIZE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SIZE\n")));
            retval = dlgChpassKeyfileWizard_HandleMsg_WM_SIZE(hDlg);
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
            retval = dlgChpassKeyfileWizard_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
