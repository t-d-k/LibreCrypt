// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgHashInfo.h"
#include "FreeOTFEDebug.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceHash.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
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
#define HASHINFO_TABS  2

#define PAGE_IDX_HASHINFO_DRIVER          0
#define PAGE_IDX_HASHINFO_IMPLEMENTATION  1

// =========================================================================
// Structures...


// =========================================================================
// Local to this file...

HWND G_dlgHashInfo_MenuBar = NULL;
WCHAR G_dlgHashInfo_HashDriver[FREEOTFE_MAX_FILENAME_LENGTH];
GUID G_dlgHashInfo_HashGUID;
HWND G_dlgHashInfo_PropPage[HASHINFO_TABS];
int G_dlgHashInfo_ScrollFullHeight[HASHINFO_TABS];

// =========================================================================
// Forward declarations...

BOOL CALLBACK dlgHashInfo_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgHashInfo_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
int _dlgHashInfo_GetCurrPageIdx(HWND hDlg);
int _dlgHashInfo_GetPageIdxOfPage(HWND hDlg);


// =========================================================================
void DisplayDlgHashInfo(
    HWND hWnd,
    WCHAR* HashDriver,
    GUID HashGUID
)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[HASHINFO_TABS];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgHashInfo\n")));

    wcscpy(G_dlgHashInfo_HashDriver, HashDriver);
    G_dlgHashInfo_HashGUID = HashGUID;

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));

    // Basic page...
    propPage[PAGE_IDX_HASHINFO_DRIVER].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_HASHINFO_DRIVER].lParam      = PAGE_IDX_HASHINFO_DRIVER;
    propPage[PAGE_IDX_HASHINFO_DRIVER].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_HASHINFO_DRIVER].pszTitle    = _("Driver");
    propPage[PAGE_IDX_HASHINFO_DRIVER].hInstance   = G_hInstance;
    propPage[PAGE_IDX_HASHINFO_DRIVER].pszTemplate = MAKEINTRESOURCE(IDD_INFO_HASH_DRIVER);
    propPage[PAGE_IDX_HASHINFO_DRIVER].pfnDlgProc  = dlgHashInfo_PropSheetPageProc;
       
    // Advanced page...
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].lParam      = PAGE_IDX_HASHINFO_IMPLEMENTATION;
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].pszTitle    = _("Implementation");
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].hInstance   = G_hInstance;
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].pszTemplate = MAKEINTRESOURCE(IDD_INFO_HASH_IMPLEMENTATION);
    propPage[PAGE_IDX_HASHINFO_IMPLEMENTATION].pfnDlgProc  = dlgHashInfo_PropSheetPageProc;
       
    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Hash Details");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = HASHINFO_TABS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgHashInfo_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgHashInfo\n")));
}                


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgHashInfo_GetCurrPageIdx(HWND hDlg)
{
    return _dlgHashInfo_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgHashInfo_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
HWND _dlgHashInfo_GetPageHWNDForPage(int idx)
{
    return G_dlgHashInfo_PropPage[idx];
}


// =========================================================================
BOOL CALLBACK dlgHashInfo_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("dlgHashInfo_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
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
            // This is called once for each of the tabs
            SetWindowLong(hDlg, DWL_MSGRESULT, PSNRET_NOERROR);
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

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("dlgHashInfo_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgHashInfo_HandleMsg_WM_VSCROLL(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    SCROLLINFO si; 
    int yPos;
    RECT displaySize;
    int multiplier;
    int fullHeight;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgHashInfo_HandleMsg_WM_VSCROLL\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    // Get all the vertial scroll bar information
    si.cbSize = sizeof(si);
    si.fMask  = SIF_ALL;
    GetScrollInfo(hDlg, SB_VERT, &si);
    // Save the position for comparison later on
    yPos = si.nPos;
    switch (LOWORD(wParam))
        {
        // user clicked the HOME keyboard key
        case SB_TOP:
            {
            si.nPos = si.nMin;
            break;
            }

        // user clicked the END keyboard key
        case SB_BOTTOM:
            {
            si.nPos = si.nMax;
            break;
            }

        // user clicked the top arrow
        case SB_LINEUP:
            {
            si.nPos -= 1;
            break;
            }

        // user clicked the bottom arrow
        case SB_LINEDOWN:
            {
            si.nPos += 1;
            break;
            }

        // user clicked the scroll bar shaft above the scroll box
        case SB_PAGEUP:
            {
            si.nPos -= si.nPage;
            break;
            }

        // user clicked the scroll bar shaft below the scroll box
        case SB_PAGEDOWN:
            {
            si.nPos += si.nPage;
            break;
            }

        // user dragged the scroll box
        case SB_THUMBTRACK:
            {
            si.nPos = si.nTrackPos;
            break;
            }
        }

    // Set the position and then retrieve it.  Due to adjustments
    //   by Windows it may not be the same as the value set.
    si.fMask = SIF_POS;
    SetScrollInfo(hDlg, SB_VERT, &si, TRUE);

    GetClientRect(hDlg, &displaySize);
//    GetWindowRect(hDlg, &displaySize);

    fullHeight = G_dlgHashInfo_ScrollFullHeight[_dlgHashInfo_GetPageIdxOfPage(hDlg)];
    // +1 because this uses integer division
    multiplier = (fullHeight / (displaySize.bottom - displaySize.top)) + 1;

    // If the position has changed, scroll window and update it
    GetScrollInfo(hDlg, SB_VERT, &si);
    if (si.nPos != yPos)
        {                    
        ScrollWindowEx(
                        hDlg, 
                        0, 
                        ((yPos - si.nPos) * multiplier), 
                        NULL, 
                        NULL,
                        NULL,
                        NULL,
                        (SW_ERASE | SW_INVALIDATE | SW_SCROLLCHILDREN)
                        );
        UpdateWindow(hDlg);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgHashInfo_HandleMsg_WM_VSCROLL\n")));
    return retval;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgHashInfo_PropSheetProc(
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
BOOL CALLBACK dlgHashInfo_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int currPageIdx;
    HASH_DRIVER_INFO driverInfo;
    HASH implDetails;
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];
    WCHAR* tmpStringGIUD;
    WCHAR driverVersionStr[256]; // 256 pretty arbitary; just needs to be big
                                 // enough to store the formatted version
                                 // string
    RECT rcBounds;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgHashInfo_HandleMsg_WM_INITDIALOG\n")));

    // Don't recreate menu if it's already been done
    if (G_dlgHashInfo_MenuBar == NULL)
        {
        G_dlgHashInfo_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_NULL);
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
    SDUi18n_TranslateCommandBar(G_dlgHashInfo_MenuBar);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    currPageIdx = ((LPPROPSHEETPAGE)lParam)->lParam;
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  currPageIdx
                 );
    G_dlgHashInfo_PropPage[currPageIdx] = hDlg;


    // Default values for controls...
    if (currPageIdx == PAGE_IDX_HASHINFO_DRIVER)
        {
        // Driver page...

        // For some reason, the vertical scrollbar doesn't get shown when it's
        // set in the .rc file, so we force it here
        SDUSetWndStyle(hDlg, TRUE, WS_VSCROLL);

        // Determine the height of the dialog as specified in the resource editor
        // This is required for scrolling
        GetClientRect(hDlg, &rcBounds);
        G_dlgHashInfo_ScrollFullHeight[PAGE_IDX_HASHINFO_DRIVER] = (rcBounds.bottom - rcBounds.top);

        if (driver_HashGetDriverDetails(G_dlgHashInfo_HashDriver, &driverInfo))
            {
            SetDlgItemText(hDlg, IDC_EDIT_HASH_DRIVER_FILENAME, G_dlgHashInfo_HashDriver);

            GUIDToWCHAR(&driverInfo.DriverGUID, &tmpStringGIUD);
            SetDlgItemText(hDlg, IDC_EDIT_HASH_DRIVER_GUID, tmpStringGIUD);
            SecZeroAndFreeWCHARMemory(tmpStringGIUD);

            // Convert straight ASCII title to UNICODE
            memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
            mbstowcs(
                     tmpStrBuf, 
                     driverInfo.DriverTitle,
                     strlen(driverInfo.DriverTitle)
                    ); 
            SetDlgItemText(hDlg, IDC_EDIT_HASH_DRIVER_TITLE, tmpStrBuf);

            if (_snwprintf(
                           driverVersionStr, 
                           //(sizeof(driverVersionStr) * sizeof(driverVersionStr[0])),
                           (sizeof(driverVersionStr) / sizeof(driverVersionStr[0])),
                           TEXT("v%d.%0.2d.%0.4d"), 
                           (driverInfo.DriverVersionID & 0xFF000000) / 0x00FF0000,
                           (driverInfo.DriverVersionID & 0x00FF0000) / 0x0000FF00,
                           (driverInfo.DriverVersionID & 0x0000FFFF)
                          ) >= 0)
                {
                SetDlgItemText(hDlg, IDC_EDIT_HASH_DRIVER_VERSION, driverVersionStr);
                }

            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_HASH_DRIVER_HASHCOUNT, 
                          driverInfo.HashCount, 
                          TRUE
                         );
            }
        }
    else if (currPageIdx == PAGE_IDX_HASHINFO_IMPLEMENTATION)
        {
        // Implementation page...

        // For some reason, the vertical scrollbar doesn't get shown when it's
        // set in the .rc file, so we force it here
        SDUSetWndStyle(hDlg, TRUE, WS_VSCROLL);

        // Determine the height of the dialog as specified in the resource editor
        // This is required for scrolling
        GetClientRect(hDlg, &rcBounds);
        G_dlgHashInfo_ScrollFullHeight[PAGE_IDX_HASHINFO_IMPLEMENTATION] = (rcBounds.bottom - rcBounds.top);

        if (driver_HashGetImplDetails(
                                        G_dlgHashInfo_HashDriver, 
                                        &G_dlgHashInfo_HashGUID, 
                                        &implDetails
                                        ))
            {
            GUIDToWCHAR(&implDetails.HashGUID, &tmpStringGIUD);
            SetDlgItemText(hDlg, IDC_EDIT_HASH_IMPLEMENTATION_GUID, tmpStringGIUD);
            SecZeroAndFreeWCHARMemory(tmpStringGIUD);

            // Convert straight ASCII title to UNICODE
            memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
            mbstowcs(
                     tmpStrBuf, 
                     implDetails.Title, 
                     strlen(implDetails.Title)
                    ); 
            SetDlgItemText(hDlg, IDC_EDIT_HASH_IMPLEMENTATION_TITLE, tmpStrBuf);

            // -1 indicates any keysize
            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_HASH_IMPLEMENTATION_KEYSIZE, 
                          implDetails.Length, 
                          TRUE
                         );

            // -1 indicates any blocksize
            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_HASH_IMPLEMENTATION_BLOCKSIZE, 
                          implDetails.BlockSize, 
                          TRUE
                         );

            if (_snwprintf(
                           driverVersionStr, 
                           //(sizeof(driverVersionStr) * sizeof(driverVersionStr[0])),
                           (sizeof(driverVersionStr) / sizeof(driverVersionStr[0])),
                           TEXT("v%d.%0.2d.%0.4d"), 
                           (implDetails.VersionID & 0xFF000000) / 0x00FF0000,
                           (implDetails.VersionID & 0x00FF0000) / 0x0000FF00,
                           (implDetails.VersionID & 0x0000FFFF)
                          ) >= 0)
                {
                SetDlgItemText(hDlg, IDC_EDIT_HASH_IMPLEMENTATION_VERSION, driverVersionStr);
                }
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgHashInfo_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgHashInfo_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgHashInfo_HandleMsg_WM_DESTROY\n")));
    if (G_dlgHashInfo_MenuBar != NULL)
        {
        DestroyWindow(G_dlgHashInfo_MenuBar);
        G_dlgHashInfo_MenuBar = NULL;
        }

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgHashInfo_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgHashInfo_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgHashInfo_HandleMsg_WM_COMMAND\n")));
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

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgHashInfo_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgHashInfo_PropSheetPageProc(
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
            retval = dlgHashInfo_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_VSCROLL:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_VSCROLL\n")));
            retval = dlgHashInfo_HandleMsg_WM_VSCROLL(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgHashInfo_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgHashInfo_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgHashInfo_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
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
            retval = TRUE;
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
