// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFENULLGUID.h"
#include "main.h"
#include "dlgProperties.h"
#include "dlgHashInfo.h"
#include "dlgCypherInfo.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "DriverInterface.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "DriverInterfaceCommon.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include "resource.h"

#include "SDUGeneral.h"

#include <stdio.h> // Required for _snwprintf_s 
#include <winuser.h>  // Required for IDOK, etc
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib


// Local to this file...
HWND G_dlgProperties_MenuBar = NULL;
WCHAR* G_dlgProperties_Mountpoint = NULL;
DIOC_DISK_DEVICE_STATUS G_dlgProperties_volumeDetails;
int G_dlgProperties_ScrollFullHeight = 0;

// =========================================================================
// Forward declarations...
BOOL CALLBACK dlgProperties_Proc(
                           HWND hDlg, 
                           UINT message, 
                           WPARAM wParam, 
                           LPARAM lParam
                          );


// =========================================================================
void DisplayDlgProperties(HWND hWnd, WCHAR* mountpoint)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgProperties\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying \"Properties...\" dialog\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Mountpoint: %ls\n"), mountpoint));

    G_dlgProperties_Mountpoint = calloc(
                                        // +1 for terminating NULL
                                        (wcslen(mountpoint) + 1), 
                                        sizeof(*mountpoint)
                                       );
    if (G_dlgProperties_Mountpoint == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage to hold mountpoint\n")));
        MsgOutOfMemory(hWnd);
        }
    else
        {
        wcscpy(G_dlgProperties_Mountpoint, mountpoint);
        DialogBox(
                  G_hInstance, 
                  MAKEINTRESOURCE(IDD_PROPERTIES), 
                  hWnd, 
                  (DLGPROC)dlgProperties_Proc
                 );
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgProperties\n")));
}                


// =========================================================================
BOOL CALLBACK dlgProperties_HandleMsg_WM_INITDIALOG(HWND hDlg)
{
    SHINITDLGINFO shidi;
    HASH hashInfo;
    CYPHER_v3 cypherInfo;
    WCHAR strPrettyTitleHash[MAX_PRETTYPRINTED_TITLE];
    WCHAR strPrettyTitleCypher[MAX_PRETTYPRINTED_TITLE];
    WCHAR strPrettySectorIVGenMethod[MAX_PRETTYPRINTED_SECTORIVGENMETHOD];
    RECT rcBounds;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgProperties_HandleMsg_WM_INITDIALOG\n")));

    // Determine the height of the dialog as specified in the resource editor
    // This is required for scrolling
    GetClientRect(hDlg, &rcBounds);
    G_dlgProperties_ScrollFullHeight = (rcBounds.bottom - rcBounds.top);

    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    G_dlgProperties_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_NULL);

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgProperties_MenuBar);

    // Populate display...
    SetDlgItemText(hDlg, IDC_EDIT_PROP_MOUNTPOINT, G_dlgProperties_Mountpoint);

    if (!(driver_GetVolumeInfo_Mountpoint(
                                          G_dlgProperties_Mountpoint, 
                                          &G_dlgProperties_volumeDetails
                                         )))
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to get properties for mountpoint\n")));
        MsgError(hDlg, _("Unable to get volume properties."));
        }
    else
        {
        SetDlgItemText(hDlg, IDC_EDIT_PROP_VOLUME, G_dlgProperties_volumeDetails.Filename);

        if (G_dlgProperties_volumeDetails.ReadOnly)
            {
            SetDlgItemText(hDlg, IDC_EDIT_PROP_MOUNTEDFOR, _("Readonly"));
            }
        else
            {
            SetDlgItemText(hDlg, IDC_EDIT_PROP_MOUNTEDFOR, _("Read/write"));
            }

        // Sector IV generation method...
        driver_PrettyprintSectorIVGenMethod(
                            G_dlgProperties_volumeDetails.SectorIVGenMethod,
                            strPrettySectorIVGenMethod,
                            sizeof(strPrettySectorIVGenMethod)
                           );
        SetDlgItemText(
              hDlg, 
              IDC_EDIT_PROP_SECTOR_IVS, 
              strPrettySectorIVGenMethod
             );


        // IV hash...
        SetDlgItemText(hDlg, IDC_EDIT_PROP_HASH_IV, STR_UNKNOWN);
        if (
            (wcslen(G_dlgProperties_volumeDetails.IVHashDeviceName) == 0) &&
            (IsEqualGUID(&(G_dlgProperties_volumeDetails.IVHashGUID), &FREEOTFE_NULL_GUID))
           )
            {
            SetDlgItemText(hDlg, IDC_EDIT_PROP_HASH_IV, STR_NONE);
            }
        else if (driver_HashGetImplDetails(
                                       G_dlgProperties_volumeDetails.IVHashDeviceName,
                                       &(G_dlgProperties_volumeDetails.IVHashGUID),
                                       &hashInfo
                                      ))
            {
            memset(strPrettyTitleHash, 0, sizeof(strPrettyTitleHash));
            driver_HashPrettyprintAlgTechTitle(
                                           &hashInfo, 
                                           strPrettyTitleHash,
                                           sizeof(strPrettyTitleHash)
                                          );
            SetDlgItemText(
                           hDlg, 
                           IDC_EDIT_PROP_HASH_IV, 
                           strPrettyTitleHash
                          );
            }

        // IV cypher...
        SetDlgItemText(hDlg, IDC_EDIT_PROP_CYPHER_IV, STR_UNKNOWN);
        if (
            (wcslen(G_dlgProperties_volumeDetails.IVCypherDeviceName) == 0) &&
            (IsEqualGUID(&(G_dlgProperties_volumeDetails.IVCypherGUID), &FREEOTFE_NULL_GUID))
           )
            {
            SetDlgItemText(hDlg, IDC_EDIT_PROP_CYPHER_IV, STR_NONE);
            }
        else if (driver_CypherGetImplDetails(
                                       G_dlgProperties_volumeDetails.IVCypherDeviceName,
                                       &(G_dlgProperties_volumeDetails.IVCypherGUID),
                                       &cypherInfo
                                      ))
            {
            memset(strPrettyTitleCypher, 0, sizeof(strPrettyTitleCypher));
            driver_CypherPrettyprintAlgTitle(
                                           &cypherInfo, 
                                           strPrettyTitleCypher,
                                           sizeof(strPrettyTitleCypher)
                                          );
            SetDlgItemText(
                           hDlg, 
                           IDC_EDIT_PROP_CYPHER_IV, 
                           strPrettyTitleCypher
                          );
            }


        // Main cypher...
        SetDlgItemText(hDlg, IDC_EDIT_PROP_CYPHER_MAIN, STR_UNKNOWN);
        if (
            (wcslen(G_dlgProperties_volumeDetails.MainCypherDeviceName) == 0) &&
            (IsEqualGUID(&(G_dlgProperties_volumeDetails.MainCypherGUID), &FREEOTFE_NULL_GUID))
           )
            {
            SetDlgItemText(hDlg, IDC_EDIT_PROP_CYPHER_MAIN, STR_NONE);
            }
        else if (driver_CypherGetImplDetails(
                                       G_dlgProperties_volumeDetails.MainCypherDeviceName,
                                       &(G_dlgProperties_volumeDetails.MainCypherGUID),
                                       &cypherInfo
                                      ))
            {
            memset(strPrettyTitleCypher, 0, sizeof(strPrettyTitleCypher));
            driver_CypherPrettyprintAlgTitle(
                                           &cypherInfo, 
                                           strPrettyTitleCypher,
                                           sizeof(strPrettyTitleCypher)
                                          );
            SetDlgItemText(
                           hDlg, 
                           IDC_EDIT_PROP_CYPHER_MAIN, 
                           strPrettyTitleCypher
                          );
            }

        SetDlgItemText(hDlg, IDC_EDIT_PROP_DEVICENAME, G_dlgProperties_volumeDetails.DeviceName); 
        }

    SecZeroMemory(strPrettyTitleHash, sizeof(strPrettyTitleHash));
    SecZeroMemory(strPrettyTitleCypher, sizeof(strPrettyTitleCypher));
    SecZeroMemory(strPrettySectorIVGenMethod, sizeof(strPrettySectorIVGenMethod));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgProperties_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgProperties_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgProperties_HandleMsg_WM_DESTROY\n")));

    SecZeroMemory(&G_dlgProperties_volumeDetails, sizeof(G_dlgProperties_volumeDetails));
    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgProperties_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgProperties_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgProperties_HandleMsg_WM_CLOSE\n")));
    if (G_dlgProperties_MenuBar != NULL)
        {
        DestroyWindow(G_dlgProperties_MenuBar);
        G_dlgProperties_MenuBar = NULL;
        }
    SecZeroAndFreeWCHARMemory(G_dlgProperties_Mountpoint);
    G_dlgProperties_Mountpoint = NULL;
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgProperties_HandleMsg_WM_CLOSE\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgProperties_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgProperties_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    switch(LOWORD(wParam))
        {
        case IDOK:
            {
            EndDialog(hDlg, TRUE);
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_CYPHERINFO_MAIN:
            {
            DisplayDlgCypherInfo(
                                hDlg,
                                G_dlgProperties_volumeDetails.MainCypherDeviceName,
                                G_dlgProperties_volumeDetails.MainCypherGUID
                                );
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_HASHINFO_IV:
            {
            DisplayDlgHashInfo(
                                hDlg,
                                G_dlgProperties_volumeDetails.IVHashDeviceName,
                                G_dlgProperties_volumeDetails.IVHashGUID
                                );
            retval = TRUE;
            break;
            }

        case IDC_BUTTON_CYPHERINFO_IV:
            {
            DisplayDlgCypherInfo(
                                hDlg,
                                G_dlgProperties_volumeDetails.IVCypherDeviceName,
                                G_dlgProperties_volumeDetails.IVCypherGUID
                                );
            retval = TRUE;
            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgProperties_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgProperties_HandleMsg_WM_VSCROLL(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;
    SCROLLINFO si; 
    int yPos;
    RECT displaySize;
    int multiplier;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgProperties_HandleMsg_WM_VSCROLL\n")));
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

    // +1 because this uses integer division
    multiplier = (G_dlgProperties_ScrollFullHeight / (displaySize.bottom - displaySize.top)) + 1;

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

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgProperties_HandleMsg_WM_VSCROLL\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgProperties_Proc(
                           HWND hDlg, 
                           UINT msg, 
                           WPARAM wParam, 
                           LPARAM lParam
                          )
{
    BOOL retval = FALSE;

    switch(msg)
        {
        case WM_INITDIALOG:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_INITDIALOG\n")));
            retval = dlgProperties_HandleMsg_WM_INITDIALOG(hDlg);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgProperties_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = dlgProperties_HandleMsg_WM_CLOSE(hDlg);
            break;
            }

        case WM_COMMAND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_COMMAND\n")));
            retval = dlgProperties_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        case WM_VSCROLL:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_VSCROLL\n")));
            retval = dlgProperties_HandleMsg_WM_VSCROLL(hDlg, wParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
