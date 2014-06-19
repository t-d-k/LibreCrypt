// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgMountLUKS.h"
#include "FreeOTFEDebug.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFEAPIConstsCommon.h"
#include "resource.h"
#include "SDUGeneral.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include <storemgr.h>  // Required for STORAGE_DEVICE_TYPE_PCCARD, etc
#include <Commdlg.h>  // Required from OPENFILENAME
#include <winuser.h>  // Required for IDOK, etc
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell")  // Link in aygshell.lib

// =========================================================================
// Constants...

// The number of tabs on the dialog 
#define MOUNTLUKS_TABS  2

#define PAGE_IDX_MOUNTLUKS_BASIC     0
#define PAGE_IDX_MOUNTLUKS_ADVANCED  1

// =========================================================================
// Structures...

typedef struct _USER_MOUNTLUKS_PARAMETERS {
    BOOL ParamsOK;

    WCHAR Mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];
    unsigned char* UserPassword;
    BOOL ReadOnly;
    BOOL BaseIVCypherOnHashLength;
    LARGE_INTEGER SizeLimit;

} USER_MOUNTLUKS_PARAMETERS, *PUSER_MOUNTLUKS_PARAMETERS;

// =========================================================================
// Local to this file...

HWND G_dlgMountLUKS_MenuBar = NULL;
USER_MOUNTLUKS_PARAMETERS G_UserMountLUKSParams;
HWND G_dlgMountLUKS_PropPage[MOUNTLUKS_TABS];
BOOL G_dlgMountLUKS_Silent = FALSE;

// =========================================================================
// Forward declarations...

BOOL CALLBACK dlgMountLUKS_Mount(HWND hDlg);
BOOL CALLBACK dlgMountLUKS_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgMountLUKS_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
void _dlgMountLUKS_EnableDisableControls(HWND hDlg);
int _dlgMountLUKS_GetCurrPageIdx(HWND hDlg);
int _dlgMountLUKS_GetPageIdxOfPage(HWND hDlg);
BOOL _dlgMountLUKS_QuerySiblings(HWND hDlg);
void _dlgMountLUKS_SetDlgMountpoint(WCHAR* Mountpoint);

// =========================================================================
void DisplayDlgMountLUKS(HWND hWnd)
{
    LARGE_INTEGER sizelimit;

    sizelimit.QuadPart = DEFAULT_SIZELIMIT;
    DisplayDlgMountLUKS_Params(
                           hWnd,
                           NULL,
                           DEFAULT_FILENAME,
                           DEFAULT_PASSWORD,
                           DEFAULT_READONLY,
                           DEFAULT_LUKS_BASEICCYPHERONHASHLENGTH,
                           sizelimit,
                           FALSE
                          );
}


// =========================================================================
void DisplayDlgMountLUKS_Params(
    HWND hWnd,
    WCHAR* Mountpoint,
    WCHAR* Filename,
    WCHAR* UserPassword,
    BOOL ReadOnly,
    BOOL BaseIVCypherOnHashLength,
    LARGE_INTEGER SizeLimit,
    BOOL Silent
)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[MOUNTLUKS_TABS];
    int userPasswordSize;
    int pwLen;
    char* userPassword;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplaydlgMountLUKS_Params\n")));

    memset(
           &G_UserMountLUKSParams, 
           0,
           sizeof(G_UserMountLUKSParams)
          );

    // Populate initial parameters to pass them through to the dialog   
    // initialization function...
    if (Mountpoint != NULL)
        {
        wcscpy(G_UserMountLUKSParams.Mountpoint, Mountpoint);
        }
    if (Filename != NULL)
        {
        wcscpy(G_UserMountLUKSParams.Filename, Filename);
        }
    G_UserMountLUKSParams.ReadOnly = ReadOnly;
    G_UserMountLUKSParams.BaseIVCypherOnHashLength = BaseIVCypherOnHashLength;
    G_UserMountLUKSParams.SizeLimit = SizeLimit;

    G_UserMountLUKSParams.UserPassword = NULL;
    if (UserPassword != NULL)
        {
        pwLen = wcslen(UserPassword);
        // +1 for terminating NULL
        userPasswordSize = (pwLen + 1) * sizeof(*userPassword);
        userPassword = malloc(userPasswordSize);
        if (userPassword != NULL) 
            {
            memset(userPassword, 0, userPasswordSize);
            wcstombs(userPassword, UserPassword, wcslen(UserPassword));
            G_UserMountLUKSParams.UserPassword = userPassword;
            }
        }

    G_dlgMountLUKS_Silent = Silent;

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));

    // Basic page...
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].lParam      = PAGE_IDX_MOUNTLUKS_BASIC;
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].pszTitle    = _("Basic");
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].hInstance   = G_hInstance;
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].pszTemplate = MAKEINTRESOURCE(IDD_MOUNTLUKS_BASIC);
    propPage[PAGE_IDX_MOUNTLUKS_BASIC].pfnDlgProc  = dlgMountLUKS_PropSheetPageProc;
       
    // Advanced page...
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].lParam      = PAGE_IDX_MOUNTLUKS_ADVANCED;
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].pszTitle    = _("Advanced");
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].hInstance   = G_hInstance;
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].pszTemplate = MAKEINTRESOURCE(IDD_VOL_ADV_OPTS_LUKS);
    propPage[PAGE_IDX_MOUNTLUKS_ADVANCED].pfnDlgProc  = dlgMountLUKS_PropSheetPageProc;
       
    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Mount LUKS volume");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = MOUNTLUKS_TABS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgMountLUKS_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplaydlgMountLUKS_Params\n")));
}                


// =========================================================================
// Pass this the HWND for the *individual* *page*
void _dlgMountLUKS_EnableDisableControls(HWND hDlg)
{
    // Nothing atm...
}


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgMountLUKS_GetCurrPageIdx(HWND hDlg)
{
    return _dlgMountLUKS_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgMountLUKS_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
HWND _dlgMountLUKS_GetPageHWNDForPage(int idx)
{
    return G_dlgMountLUKS_PropPage[idx];
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    NMUPDOWN* lpnmud;
    MOUNT_RESULT mountResult;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("dlgMountLUKS_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case PSN_SETACTIVE:
            {
            _dlgMountLUKS_EnableDisableControls(hDlg);
            break;
            }

        // Spin edit up/down button
        case UDN_DELTAPOS:
            {
            switch(idCtrl)
                {
                case IDC_SPIN_SIZELIMIT:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_SIZELIMIT, 
                                      0, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
                    retval = TRUE;
                    break;
                    }

                }
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
            // we only handle it for the "Basic" tab
            if (_dlgMountLUKS_GetPageIdxOfPage(hDlg) != PAGE_IDX_MOUNTLUKS_BASIC)
                {
                // Not the basic tab; set result as "OK"
                SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                }
            else
                {                 
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dialog OK'd by user\n")));

                mountResult = dlgMountLUKS_Mount(hDlg);
                if (mountResult == MR_MOUNTED_OK)
                    {
#if DBG
                    // Only bother feeding back success if we're debugging
                    MsgInfo(hDlg, _("Mounted OK"));
#endif

                    if (G_Options->ExploreOnMount)
                        {
                        ExploreMountpoint(G_UserMountLUKSParams.Mountpoint);
                        }

                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else if (G_dlgMountLUKS_Silent)
                    {
                    // If mounting silently, always set dialog return
                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else
                    {
                    if (mountResult == MR_ACTIVATEDEVICE_FAILED)
                        {
                        MsgError(hDlg, _("Mount failed; the virtual storage device could not be activated at this time\n\nPlease see documentation for further details"));
                        }
                    else if (mountResult == MR_NOT_A_LUKS_VOLUME)
                        {
                        MsgError(hDlg, _("The file specified is not a LUKS encrypted volume."));
                        }
                    else
                        {
                        MsgError(hDlg, _("Unable to mount volume; please ensure that you entered the correct details (password, etc)."));
                        }

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


    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("dlgMountLUKS_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgMountLUKS_PropSheetProc(
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
void dlgMountLUKS_HandleMsg_DefaultMountpoint(HWND hDlg)
{
    const TRYMOUNTPOINTS = 100;
    WCHAR initBaseMountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR testMountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR useMountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    int i;
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;

    // Identify default mountpoint to use...    
    memset(initBaseMountpoint, 0, sizeof(initBaseMountpoint));
    if (G_Options->MntPntUseVolFilename)
        {
        if (_dlgMountLUKS_QuerySiblings(GetParent(hDlg)))
            {
            SDUParsePath(
                         G_UserMountLUKSParams.Filename, 
                         &uncMachine,
                         &drive,
                         &path,
                         &filename,
                         &fileExtension
                        );
            if (fileExtension != NULL)
                {
                if (wcsicmp(fileExtension, VOL_FILE_EXTN) == 0)
                    {
                    // -1 because we want to replace the "."
                    *(fileExtension-1) = (WCHAR)NULL;
                    }
                }
            if (filename != NULL)
                {
                wcscpy(initBaseMountpoint, filename);
                }
            }
        }

    if (wcslen(initBaseMountpoint) == 0)
        {
        wcscpy(initBaseMountpoint, G_Options->MntPntDefault);
        }

    memset(useMountpoint, 0, sizeof(useMountpoint));
    for (i = 1; i < TRYMOUNTPOINTS; i++)
        {
        if (i == 1)
            {
            wcscpy(testMountpoint, initBaseMountpoint);
            }
        else
            {
            _snwprintf(
                       testMountpoint, 
                       (sizeof(testMountpoint) / sizeof(testMountpoint[0])),
                       TEXT("%s (%d)"),
                       initBaseMountpoint,
                       i
                      );
            }
        if (ValidateMountpoint(hDlg, testMountpoint, FALSE, TRUE))
            {
            wcscpy(useMountpoint, testMountpoint);
            break;
            }
        }

    _dlgMountLUKS_SetDlgMountpoint(useMountpoint);
}


// =========================================================================
void _dlgMountLUKS_SetDlgMountpoint(WCHAR* Mountpoint)
{
    SetDlgItemText(
                   _dlgMountLUKS_GetPageHWNDForPage(PAGE_IDX_MOUNTLUKS_ADVANCED),
                   IDC_EDIT_MOUNTPOINT, 
                   Mountpoint
                  );
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int currPageIdx;
    UINT checkBaseIVCypherOnHashLength;
    int userPasswordSize;
    int pwLen;
    WCHAR* userPassword;
    BOOL checkReadonly;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMountLUKS_HandleMsg_WM_INITDIALOG\n")));

    // Don't recreate menu if it's already been done
    if (G_dlgMountLUKS_MenuBar == NULL)
        {
        G_dlgMountLUKS_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_DONECANCEL);
        }
    else
        {
        // If processing the second tab, and "/silent" specified on
        // commandline, post "OK" click
        if (G_dlgMountLUKS_Silent) 
            {
            PropSheet_PressButton(GetParent(hDlg), PSBTN_OK);
            }
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
    SDUi18n_TranslateCommandBar(G_dlgMountLUKS_MenuBar);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    currPageIdx = ((LPPROPSHEETPAGE)lParam)->lParam;
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  currPageIdx
                 );
    G_dlgMountLUKS_PropPage[currPageIdx] = hDlg;


    // Default values for controls...
    if (currPageIdx == PAGE_IDX_MOUNTLUKS_BASIC)
        {
        // Basic page...
        if (G_UserMountLUKSParams.Filename != NULL)
            {
            SetDlgItemText(hDlg, IDC_EDIT_FILENAME, G_UserMountLUKSParams.Filename);
            SecZeroWCHARMemory(G_UserMountLUKSParams.Filename);
            }
        if (G_UserMountLUKSParams.UserPassword != NULL)
            {
            pwLen = strlen(G_UserMountLUKSParams.UserPassword);
            // +1 for terminating NULL
            userPasswordSize = (pwLen + 1) * sizeof(*userPassword);
            userPassword = malloc(userPasswordSize);
            if (userPassword != NULL) 
                {
                memset(userPassword, 0, userPasswordSize);
                mbstowcs(
                         userPassword, 
                         G_UserMountLUKSParams.UserPassword, 
                         strlen(G_UserMountLUKSParams.UserPassword)
                        );

                SetDlgItemText(hDlg, IDC_EDIT_PASSWORD, userPassword);
                SecZeroAndFreeWCHARMemory(userPassword);
                }

            SecZeroAndFreeMemory(G_UserMountLUKSParams.UserPassword, strlen(G_UserMountLUKSParams.UserPassword));
            G_UserMountLUKSParams.UserPassword = NULL;
            }

        // Readonly checkbox...
        checkReadonly = BST_UNCHECKED;
        if (G_UserMountLUKSParams.ReadOnly == TRUE)
            {
            checkReadonly = BST_CHECKED;
            }
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_READONLY,
                       checkReadonly
                      );
        }
    else if (currPageIdx == PAGE_IDX_MOUNTLUKS_ADVANCED)
        {
        // Advanced page...

        // Default mountpoint...
        dlgMountLUKS_HandleMsg_DefaultMountpoint(hDlg);
        if (G_UserMountLUKSParams.Mountpoint != NULL)
            {
            if (wcslen(G_UserMountLUKSParams.Mountpoint) > 0)
                {
                _dlgMountLUKS_SetDlgMountpoint(G_UserMountLUKSParams.Mountpoint);
                }
            }
        SecZeroWCHARMemory(G_UserMountLUKSParams.Mountpoint);

        // Base IV cypher on hash length checkbox...
        checkBaseIVCypherOnHashLength = BST_UNCHECKED;
        if (G_UserMountLUKSParams.BaseIVCypherOnHashLength == TRUE)
            {
            checkBaseIVCypherOnHashLength = BST_CHECKED;
            }
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_BASEIVCYPHERONHASHLENGTH,
                       checkBaseIVCypherOnHashLength
                      );

        SetDlgItemInt(
                      hDlg,
                      IDC_EDIT_SIZELIMIT,
                      G_UserMountLUKSParams.SizeLimit.LowPart,
                      TRUE
                     );
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMountLUKS_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
void _dlgMountLUKS_SecZeroParams()
{
    if (G_UserMountLUKSParams.UserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_UserMountLUKSParams.UserPassword,
                    strlen(G_UserMountLUKSParams.UserPassword)
                   );
        G_UserMountLUKSParams.UserPassword = NULL;
        }

    SecZeroMemory(&G_UserMountLUKSParams, sizeof(G_UserMountLUKSParams));
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMountLUKS_HandleMsg_WM_DESTROY\n")));
    if (G_dlgMountLUKS_MenuBar != NULL)
        {
        DestroyWindow(G_dlgMountLUKS_MenuBar);
        G_dlgMountLUKS_MenuBar = NULL;
        }

    _dlgMountLUKS_SecZeroParams();

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMountLUKS_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgMountLUKS_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgMountLUKS_QuerySiblings\n")));

    G_UserMountLUKSParams.ParamsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_UserMountLUKSParams.ParamsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgMountLUKS_QuerySiblings\n")));
    return retval;
}


// =========================================================================
// Dialog OK'd; mount volume
MOUNT_RESULT CALLBACK dlgMountLUKS_Mount(HWND hDlg)
{
    MOUNT_RESULT retval = MR_UNKNOWN;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMountLUKS_Mount\n")));

    memset(&G_UserMountLUKSParams, 0, sizeof(G_UserMountLUKSParams));
    G_UserMountLUKSParams.ParamsOK = TRUE;

    if (_dlgMountLUKS_QuerySiblings(GetParent(hDlg)))
        {
        HCURSOR csrHourglass;
        HCURSOR csrPrevious;

        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);
        retval = driver_MountLUKS(
                              G_UserMountLUKSParams.Mountpoint,
                              G_UserMountLUKSParams.Filename,
                              G_UserMountLUKSParams.UserPassword,
                              G_UserMountLUKSParams.ReadOnly,
                              G_UserMountLUKSParams.BaseIVCypherOnHashLength,
                              G_UserMountLUKSParams.SizeLimit,
                              G_Options->RevertVolTimestamps
                             );
        SetCursor(csrPrevious);
        }

    // Cleardown sensitive information
    if (G_UserMountLUKSParams.UserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                             G_UserMountLUKSParams.UserPassword,
                             strlen(G_UserMountLUKSParams.UserPassword)
                            );
        G_UserMountLUKSParams.UserPassword = NULL;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMountLUKS_Mount\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMountLUKS_HandleMsg_WM_COMMAND\n")));
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

        case IDC_BUTTON_BROWSEFILENAME:
        case IDC_BUTTON_BROWSEKEYFILE:
            {
            OPENFILENAME ofn;
            DWORD flags;
            WCHAR filename[FREEOTFE_MAX_FILENAME_LENGTH];
            WCHAR* useFilter;

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

            if (ctrlID == IDC_BUTTON_BROWSEFILENAME)
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_VOLUMES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_VOLUMES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_VOLUMES;
                }
            else
                {
                useFilter          = FileFilterGenerate(FILE_FILTER_FLT_KEYFILES);
                ofn.nMaxCustFilter = FILE_FILTER_CNT_KEYFILES;
                ofn.nFilterIndex   = FILE_FILTER_DFLT_KEYFILES;
                }

            ofn.lpstrFilter = useFilter;

            if (GetOpenFileName(&ofn))
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog OK'd...\n")));
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Filename: %ls\n"), filename));
                if (ctrlID == IDC_BUTTON_BROWSEFILENAME)
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_FILENAME, filename);
                    if (G_Options->MntPntUseVolFilename)
                        {
                        dlgMountLUKS_HandleMsg_DefaultMountpoint(hDlg);
                        }
                    }
                else
                    {
                    SetDlgItemText(hDlg, IDC_EDIT_KEYFILE, filename);
                    }

                }
            else
                {
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Open file dialog cancelled\n")));
                }

            FileFilterFree(useFilter);

            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMountLUKS_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMountLUKS_HandleMsg_PSM_QUERYSIBLINGS\n")));
    
    if (_dlgMountLUKS_GetPageIdxOfPage(hDlg) == PAGE_IDX_MOUNTLUKS_BASIC)
        {
        // Process for "Basic" dialog/tab

        GetDlgItemText( 
                       hDlg, 
                       IDC_EDIT_FILENAME,
                       G_UserMountLUKSParams.Filename,
                       sizeof(G_UserMountLUKSParams.Filename)
                     ); 

        // Ditch any previously obtained password...
        if (G_UserMountLUKSParams.UserPassword != NULL)
            {
            SecZeroAndFreeMemory(
                        G_UserMountLUKSParams.UserPassword,
                        strlen(G_UserMountLUKSParams.UserPassword)
                       );
            G_UserMountLUKSParams.UserPassword = NULL;
            }
        G_UserMountLUKSParams.UserPassword = GetPassword(hDlg, IDC_EDIT_PASSWORD);
        retval = (G_UserMountLUKSParams.UserPassword != NULL);

        G_UserMountLUKSParams.ReadOnly = (IsDlgButtonChecked(
                                                   hDlg, 
                                                   IDC_CHECK_READONLY
                                                  ) == BST_CHECKED);
        }
    else
        {
        // Process for "Advanced" dialog/tab

        G_UserMountLUKSParams.BaseIVCypherOnHashLength = (IsDlgButtonChecked(
                                          hDlg, 
                                          IDC_CHECK_BASEIVCYPHERONHASHLENGTH
                                         ) == BST_CHECKED);
        
        G_UserMountLUKSParams.SizeLimit.QuadPart = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_SIZELIMIT,
                                   NULL,
                                   FALSE
                                  );

        GetDlgItemText( 
                       hDlg, 
                       IDC_EDIT_MOUNTPOINT,
                       G_UserMountLUKSParams.Mountpoint,
                       sizeof(G_UserMountLUKSParams.Mountpoint)
                     ); 

        // Prohibit reserved mountpoints...
        retval = ValidateMountpoint(
                                    hDlg, 
                                    G_UserMountLUKSParams.Mountpoint,
                                    FALSE,
                                    FALSE
                                   );

        }

    G_UserMountLUKSParams.ParamsOK = (G_UserMountLUKSParams.ParamsOK && retval);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMountLUKS_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgMountLUKS_PropSheetPageProc(
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
            retval = dlgMountLUKS_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgMountLUKS_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgMountLUKS_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgMountLUKS_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
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
            retval = dlgMountLUKS_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
