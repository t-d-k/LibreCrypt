// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgMount.h"
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
#define MOUNT_TABS  2

#define PAGE_IDX_MOUNT_BASIC     0
#define PAGE_IDX_MOUNT_ADVANCED  1

// =========================================================================
// Structures...

typedef struct _USER_MOUNT_PARAMETERS {
    BOOL ParamsOK;

    WCHAR Mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR Filename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR Keyfile[FREEOTFE_MAX_FILENAME_LENGTH];
    LARGE_INTEGER OffsetWithinFile;
    BOOL CDBAtOffset;
    unsigned char* UserPassword;
    unsigned int SaltLength;  // In *bits*
    unsigned int KeyIterations;
    BOOL ReadOnly;

} USER_MOUNT_PARAMETERS, *PUSER_MOUNT_PARAMETERS;

// =========================================================================
// Local to this file...

HWND G_dlgMount_MenuBar = NULL;
USER_MOUNT_PARAMETERS G_UserMountParams;
HWND G_dlgMount_PropPage[MOUNT_TABS];
BOOL G_dlgMount_Silent = FALSE;

// =========================================================================
// Forward declarations...

BOOL CALLBACK dlgMount_Mount(HWND hDlg);
BOOL CALLBACK dlgMount_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgMount_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
void _dlgMount_EnableDisableControls(HWND hDlg);
int _dlgMount_GetCurrPageIdx(HWND hDlg);
int _dlgMount_GetPageIdxOfPage(HWND hDlg);
BOOL _dlgMount_QuerySiblings(HWND hDlg);
void _dlgMount_SetDlgMountpoint(WCHAR* Mountpoint);

// =========================================================================
void DisplayDlgMount(HWND hWnd)
{
    LARGE_INTEGER offset;

    offset.QuadPart = DEFAULT_OFFSET;
    DisplayDlgMount_Params(
                           hWnd,
                           NULL,
                           DEFAULT_FILENAME,
                           DEFAULT_KEYFILE,
                           offset,
                           DEFAULT_CDBATOFFSET,
                           DEFAULT_PASSWORD,
                           DEFAULT_SALTLENGTH,  // In *bits*
                           DEFAULT_KEYITERATIONS,
                           DEFAULT_READONLY,
                           FALSE
                          );
}


// =========================================================================
void DisplayDlgMount_Params(
    HWND hWnd,
    WCHAR* Mountpoint,
    WCHAR* Filename,
    WCHAR* Keyfile,
    LARGE_INTEGER OffsetWithinFile,
    BOOL CDBAtOffset,
    WCHAR* UserPassword,
    unsigned int SaltLength,  // In *bits*
    unsigned int KeyIterations,
    BOOL ReadOnly,
    BOOL Silent
)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[MOUNT_TABS];
    int userPasswordSize;
    int pwLen;
    char* userPassword;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgMount_Params\n")));

    memset(
           &G_UserMountParams, 
           0,
           sizeof(G_UserMountParams)
          );

    // Populate initial parameters to pass them through to the dialog   
    // initialization function...
    if (Mountpoint != NULL)
        {
        wcscpy(G_UserMountParams.Mountpoint, Mountpoint);
        }
    if (Filename != NULL)
        {
        wcscpy(G_UserMountParams.Filename, Filename);
        }
    if (Keyfile != NULL)
        {
        wcscpy(G_UserMountParams.Keyfile, Keyfile);
        }
    G_UserMountParams.OffsetWithinFile = OffsetWithinFile;
    G_UserMountParams.CDBAtOffset = CDBAtOffset;
    G_UserMountParams.SaltLength = SaltLength;  // In *bits*
    G_UserMountParams.KeyIterations = KeyIterations;
    G_UserMountParams.ReadOnly = ReadOnly;

    G_UserMountParams.UserPassword = NULL;
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
            G_UserMountParams.UserPassword = userPassword;
            }
        }

    G_dlgMount_Silent = Silent;

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));

    // Basic page...
    propPage[PAGE_IDX_MOUNT_BASIC].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_MOUNT_BASIC].lParam      = PAGE_IDX_MOUNT_BASIC;
    propPage[PAGE_IDX_MOUNT_BASIC].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_MOUNT_BASIC].pszTitle    = _("Basic");
    propPage[PAGE_IDX_MOUNT_BASIC].hInstance   = G_hInstance;
    propPage[PAGE_IDX_MOUNT_BASIC].pszTemplate = MAKEINTRESOURCE(IDD_MOUNT_BASIC);
    propPage[PAGE_IDX_MOUNT_BASIC].pfnDlgProc  = dlgMount_PropSheetPageProc;
       
    // Advanced page...
    propPage[PAGE_IDX_MOUNT_ADVANCED].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_MOUNT_ADVANCED].lParam      = PAGE_IDX_MOUNT_ADVANCED;
    propPage[PAGE_IDX_MOUNT_ADVANCED].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_MOUNT_ADVANCED].pszTitle    = _("Advanced");
    propPage[PAGE_IDX_MOUNT_ADVANCED].hInstance   = G_hInstance;
    propPage[PAGE_IDX_MOUNT_ADVANCED].pszTemplate = MAKEINTRESOURCE(IDD_VOL_ADV_OPTS);
    propPage[PAGE_IDX_MOUNT_ADVANCED].pfnDlgProc  = dlgMount_PropSheetPageProc;
       
    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Mount volume");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = MOUNT_TABS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgMount_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgMount_Params\n")));
}                


// =========================================================================
// Pass this the HWND for the *individual* *page*
void _dlgMount_EnableDisableControls(HWND hDlg)
{
    if (_dlgMount_QuerySiblings(GetParent(hDlg)))
        {
        if (_dlgMount_GetPageIdxOfPage(hDlg) == PAGE_IDX_MOUNT_ADVANCED)
            {
            // If no keyfile is specified, force IDC_CHECK_CDBATOFFSET to be checked
            // and disable it.
            if (wcslen(G_UserMountParams.Keyfile) == 0)
                {
                CheckDlgButton(
                               hDlg, 
                               IDC_CHECK_CDBATOFFSET,
                               BST_CHECKED
                              );

                SetControlEnabled(hDlg, IDC_CHECK_CDBATOFFSET, FALSE);
                }
            else
                {
                SetControlEnabled(hDlg, IDC_CHECK_CDBATOFFSET, TRUE);
                }

            }
        }
}


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgMount_GetCurrPageIdx(HWND hDlg)
{
    return _dlgMount_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgMount_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
HWND _dlgMount_GetPageHWNDForPage(int idx)
{
    return G_dlgMount_PropPage[idx];
}


// =========================================================================
BOOL CALLBACK dlgMount_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;
    NMUPDOWN* lpnmud;
    MOUNT_RESULT mountResult;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("dlgMount_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case PSN_SETACTIVE:
            {
            _dlgMount_EnableDisableControls(hDlg);
            break;
            }

        // Spin edit up/down button
        case UDN_DELTAPOS:
            {
            switch(idCtrl)
                {
                case IDC_SPIN_OFFSET:
                    {
                    lpnmud = (NMUPDOWN*)lParam;
                    AdjustSpinControl(
                                      hDlg, 
                                      IDC_EDIT_OFFSET, 
                                      0, 
                                      INT_MAX, 
                                      -(lpnmud->iDelta)
                                     );
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
            if (_dlgMount_GetPageIdxOfPage(hDlg) != PAGE_IDX_MOUNT_BASIC)
                {
                // Not the basic tab; set result as "OK"
                SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                }
            else
                {                 
                DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dialog OK'd by user\n")));

                mountResult = dlgMount_Mount(hDlg);
                if (mountResult == MR_MOUNTED_OK)
                    {
#if DBG
                    // Only bother feeding back success if we're debugging
                    MsgInfo(hDlg, _("Mounted OK"));
#endif

                    if (G_Options->ExploreOnMount)
                        {
                        ExploreMountpoint(G_UserMountParams.Mountpoint);
                        }

                    SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
                    }
                else if (G_dlgMount_Silent)
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
                    else
                        {
                        MsgError(hDlg, _("Unable to mount volume; please ensure that you entered the correct details (password, etc), and the appropriate hash/cypher drivers are enabled\n\nIf you are using a keyfile, please ensure that you check/uncheck the \"Data from offset includes CDB\" option, as appropriate for your volume."));
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


    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("dlgMount_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgMount_PropSheetProc(
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
void dlgMount_HandleMsg_DefaultMountpoint(HWND hDlg)
{
    WCHAR* useMountpoint;
    WCHAR* filename;

    filename = NULL;
    if (G_Options->MntPntUseVolFilename)
        {
        if (_dlgMount_QuerySiblings(GetParent(hDlg)))
            {
            filename = G_UserMountParams.Filename;
            }
        }

    useMountpoint = GenerateDefaultMountpoint(hDlg, filename);
    if (useMountpoint != NULL)
        {
        _dlgMount_SetDlgMountpoint(useMountpoint);
        SecZeroAndFreeWCHARMemory(useMountpoint);
        }

}


// =========================================================================
void _dlgMount_SetDlgMountpoint(WCHAR* Mountpoint)
{
    SetDlgItemText(
                   _dlgMount_GetPageHWNDForPage(PAGE_IDX_MOUNT_ADVANCED),
                   IDC_EDIT_MOUNTPOINT, 
                   Mountpoint
                  );
}


// =========================================================================
BOOL CALLBACK dlgMount_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int currPageIdx;
    UINT checkCDBAtOffset;
    int userPasswordSize;
    int pwLen;
    WCHAR* userPassword;
    BOOL checkReadonly;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMount_HandleMsg_WM_INITDIALOG\n")));

    // Don't recreate menu if it's already been done
    if (G_dlgMount_MenuBar == NULL)
        {
        G_dlgMount_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_DONECANCEL);
        }
    else
        {
        // If processing the second tab, and "/silent" specified on
        // commandline, post "OK" click
        if (G_dlgMount_Silent) 
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
    SDUi18n_TranslateCommandBar(G_dlgMount_MenuBar);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    currPageIdx = ((LPPROPSHEETPAGE)lParam)->lParam;
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  currPageIdx
                 );
    G_dlgMount_PropPage[currPageIdx] = hDlg;


    // Default values for controls...
    if (currPageIdx == PAGE_IDX_MOUNT_BASIC)
        {
        // Basic page...
        if (G_UserMountParams.Filename != NULL)
            {
            SetDlgItemText(hDlg, IDC_EDIT_FILENAME, G_UserMountParams.Filename);
            SecZeroWCHARMemory(G_UserMountParams.Filename);
            }
        if (G_UserMountParams.Keyfile != NULL)
            {
            SetDlgItemText(hDlg, IDC_EDIT_KEYFILE, G_UserMountParams.Keyfile);
            SecZeroWCHARMemory(G_UserMountParams.Keyfile);
            }
        if (G_UserMountParams.UserPassword != NULL)
            {
            pwLen = strlen(G_UserMountParams.UserPassword);
            // +1 for terminating NULL
            userPasswordSize = (pwLen + 1) * sizeof(*userPassword);
            userPassword = malloc(userPasswordSize);
            if (userPassword != NULL) 
                {
                memset(userPassword, 0, userPasswordSize);
                mbstowcs(
                         userPassword, 
                         G_UserMountParams.UserPassword, 
                         strlen(G_UserMountParams.UserPassword)
                        );

                SetDlgItemText(hDlg, IDC_EDIT_PASSWORD, userPassword);
                SecZeroAndFreeWCHARMemory(userPassword);
                }

            SecZeroAndFreeMemory(G_UserMountParams.UserPassword, strlen(G_UserMountParams.UserPassword));
            G_UserMountParams.UserPassword = NULL;
            }

        // Readonly checkbox...
        checkReadonly = BST_UNCHECKED;
        if (G_UserMountParams.ReadOnly == TRUE)
            {
            checkReadonly = BST_CHECKED;
            }
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_READONLY,
                       checkReadonly
                      );
        }
    else if (currPageIdx == PAGE_IDX_MOUNT_ADVANCED)
        {
        // Advanced page...

        // Default mountpoint...
        dlgMount_HandleMsg_DefaultMountpoint(hDlg);
        if (G_UserMountParams.Mountpoint != NULL)
            {
            if (wcslen(G_UserMountParams.Mountpoint) > 0)
                {
                _dlgMount_SetDlgMountpoint(G_UserMountParams.Mountpoint);
                }
            }
        SecZeroWCHARMemory(G_UserMountParams.Mountpoint);

        SetDlgItemInt(
                      hDlg, 
                      IDC_EDIT_OFFSET, 
                      G_UserMountParams.OffsetWithinFile.LowPart, 
                      TRUE
                     );

        // CDB at offset checkbox...
        checkCDBAtOffset = BST_UNCHECKED;
        if (G_UserMountParams.CDBAtOffset == TRUE)
            {
            checkCDBAtOffset = BST_CHECKED;
            }
        CheckDlgButton(
                       hDlg, 
                       IDC_CHECK_CDBATOFFSET,
                       checkCDBAtOffset
                      );

        SetDlgItemInt(
                      hDlg,
                      IDC_EDIT_SALTLENGTH,
                      G_UserMountParams.SaltLength,
                      TRUE
                     );
        SetDlgItemInt(
                      hDlg,
                      IDC_EDIT_KEYITERATIONS,
                      G_UserMountParams.KeyIterations,
                      TRUE
                     );
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMount_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
void _dlgMount_SecZeroParams()
{
    if (G_UserMountParams.UserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                    G_UserMountParams.UserPassword,
                    strlen(G_UserMountParams.UserPassword)
                   );
        G_UserMountParams.UserPassword = NULL;
        }

    SecZeroMemory(&G_UserMountParams, sizeof(G_UserMountParams));
}


// =========================================================================
BOOL CALLBACK dlgMount_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMount_HandleMsg_WM_DESTROY\n")));
    if (G_dlgMount_MenuBar != NULL)
        {
        DestroyWindow(G_dlgMount_MenuBar);
        G_dlgMount_MenuBar = NULL;
        }

    _dlgMount_SecZeroParams();

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMount_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
BOOL _dlgMount_QuerySiblings(HWND hDlg)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_dlgMount_QuerySiblings\n")));

    G_UserMountParams.ParamsOK = TRUE;

    if (PropSheet_QuerySiblings(hDlg, 0, 0) != 0)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("PropSheet_QuerySiblings returned non-zero; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    // Not clear why, but regardless of whether the dlgProc returns
    // TRUE/FALSE for the QuerySiblings call, the above always returns zero?!
    // Therefore, we use a kludge to get the *real* result...
    else if (G_UserMountParams.ParamsOK == FALSE)
        {
        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("ParamsOK set to FALSE; not proceeding with new volume\n")));
        // Do nothing; retval already set to FALSE
        }
    else
        {
        retval = TRUE;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_dlgMount_QuerySiblings\n")));
    return retval;
}


// =========================================================================
// Dialog OK'd; mount volume
MOUNT_RESULT CALLBACK dlgMount_Mount(HWND hDlg)
{
    MOUNT_RESULT retval = MR_UNKNOWN;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMount_Mount\n")));

    memset(&G_UserMountParams, 0, sizeof(G_UserMountParams));
    G_UserMountParams.ParamsOK = TRUE;

    if (_dlgMount_QuerySiblings(GetParent(hDlg)))
        {
        HCURSOR csrHourglass;
        HCURSOR csrPrevious;

        csrHourglass = LoadCursor(NULL, IDC_WAIT);
        csrPrevious = SetCursor(csrHourglass);
        retval = driver_Mount(
                              G_UserMountParams.Mountpoint,
                              G_UserMountParams.Filename,
                              G_UserMountParams.Keyfile,
                              G_UserMountParams.OffsetWithinFile,
                              G_UserMountParams.CDBAtOffset,
                              G_UserMountParams.UserPassword,
                              G_UserMountParams.SaltLength,
                              G_UserMountParams.KeyIterations,
                              G_UserMountParams.ReadOnly,
                              G_Options->RevertVolTimestamps
                             );
        SetCursor(csrPrevious);
        }

    // Cleardown sensitive information
    if (G_UserMountParams.UserPassword != NULL)
        {
        SecZeroAndFreeMemory(
                             G_UserMountParams.UserPassword,
                             strlen(G_UserMountParams.UserPassword)
                            );
        G_UserMountParams.UserPassword = NULL;
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMount_Mount\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgMount_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMount_HandleMsg_WM_COMMAND\n")));
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
                        dlgMount_HandleMsg_DefaultMountpoint(hDlg);
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

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMount_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgMount_HandleMsg_PSM_QUERYSIBLINGS(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = TRUE;
    
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgMount_HandleMsg_PSM_QUERYSIBLINGS\n")));
    
    if (_dlgMount_GetPageIdxOfPage(hDlg) == PAGE_IDX_MOUNT_BASIC)
        {
        // Process for "Basic" dialog/tab

        GetDlgItemText( 
                       hDlg, 
                       IDC_EDIT_FILENAME,
                       G_UserMountParams.Filename,
                       sizeof(G_UserMountParams.Filename)
                     ); 

        GetDlgItemText( 
                       hDlg, 
                       IDC_EDIT_KEYFILE,
                       G_UserMountParams.Keyfile,
                       sizeof(G_UserMountParams.Keyfile)
                     ); 

        // Ditch any previously obtained password...
        if (G_UserMountParams.UserPassword != NULL)
            {
            SecZeroAndFreeMemory(
                        G_UserMountParams.UserPassword,
                        strlen(G_UserMountParams.UserPassword)
                       );
            G_UserMountParams.UserPassword = NULL;
            }
        G_UserMountParams.UserPassword = GetPassword(hDlg, IDC_EDIT_PASSWORD);
        retval = (G_UserMountParams.UserPassword != NULL);

        G_UserMountParams.ReadOnly = (IsDlgButtonChecked(
                                                   hDlg, 
                                                   IDC_CHECK_READONLY
                                                  ) == BST_CHECKED);
        }
    else
        {
        // Process for "Advanced" dialog/tab

        G_UserMountParams.OffsetWithinFile.QuadPart = GetDlgItemInt(
                                                  hDlg,
                                                  IDC_EDIT_OFFSET,
                                                  NULL,
                                                  FALSE
                                                 );

        G_UserMountParams.CDBAtOffset = (IsDlgButtonChecked(
                                          hDlg, 
                                          IDC_CHECK_CDBATOFFSET
                                         ) == BST_CHECKED);
        
        G_UserMountParams.SaltLength = GetDlgItemInt(
                                   hDlg,
                                   IDC_EDIT_SALTLENGTH,
                                   NULL,
                                   FALSE
                                  );

        G_UserMountParams.KeyIterations = GetDlgItemInt(
                                      hDlg,
                                      IDC_EDIT_KEYITERATIONS,
                                      NULL,
                                      FALSE
                                     );
        GetDlgItemText( 
                       hDlg, 
                       IDC_EDIT_MOUNTPOINT,
                       G_UserMountParams.Mountpoint,
                       sizeof(G_UserMountParams.Mountpoint)
                     ); 

        // Prohibit reserved mountpoints...
        retval = ValidateMountpoint(
                                    hDlg, 
                                    G_UserMountParams.Mountpoint,
                                    FALSE,
                                    FALSE
                                   );

        }

    G_UserMountParams.ParamsOK = (G_UserMountParams.ParamsOK && retval);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgMount_HandleMsg_PSM_QUERYSIBLINGS\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgMount_PropSheetPageProc(
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
            retval = dlgMount_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgMount_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgMount_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgMount_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
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
            retval = dlgMount_HandleMsg_PSM_QUERYSIBLINGS(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
