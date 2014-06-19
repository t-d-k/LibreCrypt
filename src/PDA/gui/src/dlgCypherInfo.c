// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgCypherInfo.h"
#include "FreeOTFEDebug.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceCypher.h"
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
#define CYPHERINFO_TABS  2

#define PAGE_IDX_CYPHERINFO_DRIVER          0
#define PAGE_IDX_CYPHERINFO_IMPLEMENTATION  1

// =========================================================================
// Structures...


// =========================================================================
// Local to this file...

HWND G_dlgCypherInfo_MenuBar = NULL;
WCHAR G_dlgCypherInfo_CypherDriver[FREEOTFE_MAX_FILENAME_LENGTH];
GUID G_dlgCypherInfo_CypherGUID;
HWND G_dlgCypherInfo_PropPage[CYPHERINFO_TABS];
int G_dlgCypherInfo_ScrollFullHeight[CYPHERINFO_TABS];

// =========================================================================
// Forward declarations...

BOOL CALLBACK dlgCypherInfo_PropSheetProc(
                           HWND hDlg, 
                           UINT message, 
                           LPARAM lParam
                          );
BOOL CALLBACK dlgCypherInfo_PropSheetPageProc(
                      HWND hDlg, 
                      UINT msg, 
                      WPARAM wParam, 
                      LPARAM lParam
                     );
int _dlgCypherInfo_GetCurrPageIdx(HWND hDlg);
int _dlgCypherInfo_GetPageIdxOfPage(HWND hDlg);


// =========================================================================
void DisplayDlgCypherInfo(
    HWND hWnd,
    WCHAR* CypherDriver,
    GUID CypherGUID
)
{
    PROPSHEETHEADER propHeader;
    PROPSHEETPAGE propPage[CYPHERINFO_TABS];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgCypherInfo\n")));

    wcscpy(G_dlgCypherInfo_CypherDriver, CypherDriver);
    G_dlgCypherInfo_CypherGUID = CypherGUID;

    // Zero unused members...
    memset(propPage, 0, sizeof(propPage));

    // Basic page...
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].dwSize      = sizeof(propPage[0]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].lParam      = PAGE_IDX_CYPHERINFO_DRIVER;
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].pszTitle    = _("Driver");
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].hInstance   = G_hInstance;
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].pszTemplate = MAKEINTRESOURCE(IDD_INFO_CYPHER_DRIVER);
    propPage[PAGE_IDX_CYPHERINFO_DRIVER].pfnDlgProc  = dlgCypherInfo_PropSheetPageProc;
       
    // Advanced page...
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].dwSize      = sizeof(propPage[1]);
    // Create when property sheet created, and tab caption (title)
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].lParam      = PAGE_IDX_CYPHERINFO_IMPLEMENTATION;
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].dwFlags     = (PSP_PREMATURE | PSP_USETITLE);
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].pszTitle    = _("Implementation");
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].hInstance   = G_hInstance;
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].pszTemplate = MAKEINTRESOURCE(IDD_INFO_CYPHER_IMPLEMENTATION);
    propPage[PAGE_IDX_CYPHERINFO_IMPLEMENTATION].pfnDlgProc  = dlgCypherInfo_PropSheetPageProc;
       
    // Common...
    propHeader.dwSize = sizeof (PROPSHEETHEADER);
    propHeader.dwFlags     = (
                              PSH_NOAPPLYNOW | 
                              PSH_USECALLBACK |   // Needed to get WinCE
                                                  // look & feel control
                              PSH_PROPSHEETPAGE | // Use templates not handles
                              PSH_MAXIMIZE        // All WinCE dialogs are maximised
                             );
    propHeader.pszCaption  = _("Cypher Details");
    propHeader.hwndParent  = hWnd;
    propHeader.hInstance   = G_hInstance;
    propHeader.nPages      = CYPHERINFO_TABS;
    propHeader.nStartPage  = 0;
    propHeader.ppsp        = propPage;
    propHeader.pfnCallback = dlgCypherInfo_PropSheetProc; // Needed to get WinCE 
                                                     // look & feel control
       
    // Returns +ve value on success
    PropertySheet(&propHeader);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgCypherInfo\n")));
}                


// =========================================================================
// Returns 0 based index of current page specified
// Pass this the HWND for the *main* dialog
int _dlgCypherInfo_GetCurrPageIdx(HWND hDlg)
{
    return _dlgCypherInfo_GetPageIdxOfPage(PropSheet_GetCurrentPageHwnd(hDlg));
}


// =========================================================================
// Returns 0 based index of current page specified
int _dlgCypherInfo_GetPageIdxOfPage(HWND hDlg)
{
    int retval;

    retval = GetWindowLong(hDlg, GWL_USERDATA);

    return retval;
}


// =========================================================================
// Pass this the HWND for the *main* dialog
HWND _dlgCypherInfo_GetPageHWNDForPage(int idx)
{
    return G_dlgCypherInfo_PropPage[idx];
}


// =========================================================================
BOOL CALLBACK dlgCypherInfo_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("dlgCypherInfo_HandleMsg_WM_NOTIFY\n")));

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

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("dlgCypherInfo_HandleMsg_WM_NOTIFY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgCypherInfo_HandleMsg_WM_VSCROLL(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    SCROLLINFO si; 
    int yPos;
    RECT displaySize;
    int multiplier;
    int fullHeight;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgCypherInfo_HandleMsg_WM_VSCROLL\n")));
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

    fullHeight = G_dlgCypherInfo_ScrollFullHeight[_dlgCypherInfo_GetPageIdxOfPage(hDlg)];
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

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgCypherInfo_HandleMsg_WM_VSCROLL\n")));
    return retval;
}


// =========================================================================
// This function is required in order to get the WinCE look & feel to the
// tabbed dialog.
int CALLBACK dlgCypherInfo_PropSheetProc(
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
BOOL CALLBACK dlgCypherInfo_HandleMsg_WM_INITDIALOG(HWND hDlg, LPARAM lParam)
{
    SHINITDLGINFO shidi;
    int currPageIdx;
    CYPHER_DRIVER_INFO_v3 driverInfo;
    CYPHER_v3 implDetails;
    WCHAR tmpStrBuf[MAX_PRETTYPRINTED_TITLE];
    WCHAR* tmpStringGIUD;
    WCHAR driverVersionStr[256]; // 256 pretty arbitary; just needs to be big
                                 // enough to store the formatted version
                                 // string
    RECT rcBounds;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgCypherInfo_HandleMsg_WM_INITDIALOG\n")));

    // Don't recreate menu if it's already been done
    if (G_dlgCypherInfo_MenuBar == NULL)
        {
        G_dlgCypherInfo_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_NULL);
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
    SDUi18n_TranslateCommandBar(G_dlgCypherInfo_MenuBar);

    // Set userdata on the page being initialized; we use this to identify
    // the different pages
    currPageIdx = ((LPPROPSHEETPAGE)lParam)->lParam;
    SetWindowLong(
                  hDlg,
                  GWL_USERDATA,
                  currPageIdx
                 );
    G_dlgCypherInfo_PropPage[currPageIdx] = hDlg;


    // Default values for controls...
    if (currPageIdx == PAGE_IDX_CYPHERINFO_DRIVER)
        {
        // Driver page...

        // For some reason, the vertical scrollbar doesn't get shown when it's
        // set in the .rc file, so we force it here
        SDUSetWndStyle(hDlg, TRUE, WS_VSCROLL);

        // Determine the height of the dialog as specified in the resource editor
        // This is required for scrolling
        GetClientRect(hDlg, &rcBounds);
        G_dlgCypherInfo_ScrollFullHeight[PAGE_IDX_CYPHERINFO_DRIVER] = (rcBounds.bottom - rcBounds.top);

        if (driver_CypherGetDriverDetails(G_dlgCypherInfo_CypherDriver, &driverInfo))
            {
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_DRIVER_FILENAME, G_dlgCypherInfo_CypherDriver);

            GUIDToWCHAR(&driverInfo.DriverGUID, &tmpStringGIUD);
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_DRIVER_GUID, tmpStringGIUD);
            SecZeroAndFreeWCHARMemory(tmpStringGIUD);

            // Convert straight ASCII title to UNICODE
            memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
            mbstowcs(
                     tmpStrBuf, 
                     driverInfo.DriverTitle,
                     strlen(driverInfo.DriverTitle)
                    ); 
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_DRIVER_TITLE, tmpStrBuf);

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
                SetDlgItemText(hDlg, IDC_EDIT_CYPHER_DRIVER_VERSION, driverVersionStr);
                }

            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_CYPHER_DRIVER_CYPHERCOUNT, 
                          driverInfo.CypherCount, 
                          TRUE
                         );
            }
        }
    else if (currPageIdx == PAGE_IDX_CYPHERINFO_IMPLEMENTATION)
        {
        // Implementation page...

        // For some reason, the vertical scrollbar doesn't get shown when it's
        // set in the .rc file, so we force it here
        SDUSetWndStyle(hDlg, TRUE, WS_VSCROLL);

        // Determine the height of the dialog as specified in the resource editor
        // This is required for scrolling
        GetClientRect(hDlg, &rcBounds);
        G_dlgCypherInfo_ScrollFullHeight[PAGE_IDX_CYPHERINFO_IMPLEMENTATION] = (rcBounds.bottom - rcBounds.top);

        if (driver_CypherGetImplDetails(
                                        G_dlgCypherInfo_CypherDriver, 
                                        &G_dlgCypherInfo_CypherGUID, 
                                        &implDetails
                                        ))
            {
            GUIDToWCHAR(&implDetails.CypherGUID, &tmpStringGIUD);
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_IMPLEMENTATION_GUID, tmpStringGIUD);
            SecZeroAndFreeWCHARMemory(tmpStringGIUD);

            // Convert straight ASCII title to UNICODE
            memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
            mbstowcs(
                     tmpStrBuf, 
                     implDetails.Title, 
                     strlen(implDetails.Title)
                    ); 
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_IMPLEMENTATION_TITLE, tmpStrBuf);

            // Convert cypher mode to UNICODE string representation
            memset(tmpStrBuf, 0, sizeof(tmpStrBuf));
            driver_CypherPrettyprintAlgMode(
                                            implDetails.Mode,
                                            tmpStrBuf
                                           );
            SetDlgItemText(hDlg, IDC_EDIT_CYPHER_IMPLEMENTATION_MODE, tmpStrBuf);

            // -1 indicates any keysize
            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_CYPHER_IMPLEMENTATION_KEYSIZE, 
                          implDetails.KeySizeUnderlying, 
                          TRUE
                         );

            // -1 indicates any blocksize
            SetDlgItemInt(
                          hDlg, 
                          IDC_EDIT_CYPHER_IMPLEMENTATION_BLOCKSIZE, 
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
                SetDlgItemText(hDlg, IDC_EDIT_CYPHER_IMPLEMENTATION_VERSION, driverVersionStr);
                }
            }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgCypherInfo_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgCypherInfo_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgCypherInfo_HandleMsg_WM_DESTROY\n")));
    if (G_dlgCypherInfo_MenuBar != NULL)
        {
        DestroyWindow(G_dlgCypherInfo_MenuBar);
        G_dlgCypherInfo_MenuBar = NULL;
        }

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgCypherInfo_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgCypherInfo_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    int ctrlID;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgCypherInfo_HandleMsg_WM_COMMAND\n")));
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

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgCypherInfo_HandleMsg_WM_COMMAND\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgCypherInfo_PropSheetPageProc(
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
            retval = dlgCypherInfo_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_VSCROLL:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_VSCROLL\n")));
            retval = dlgCypherInfo_HandleMsg_WM_VSCROLL(hDlg, wParam);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgCypherInfo_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            //s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = dlgCypherInfo_HandleMsg_WM_INITDIALOG(hDlg, lParam);
            break;
            }

        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = dlgCypherInfo_HandleMsg_WM_NOTIFY(hDlg, wParam, lParam);
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
