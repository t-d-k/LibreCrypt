// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "main.h"
#include "dlgAbout.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "DriverInterface.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include "resource.h"

#include "SDUGeneral.h"

#include <stdio.h> // Required for _snwprintf_s 
#include <winuser.h>  // Required for IDOK, etc
#include <WindowsX.h>  // Required for WM_CTLCOLOR
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib


// Local to this file...
HWND G_dlgAbout_MenuBar = NULL;
HBRUSH G_URLBrush = NULL;

// =========================================================================
// Forward declarations...
BOOL CALLBACK dlgAbout_Proc(
                           HWND hDlg, 
                           WORD message, 
                           WPARAM wParam, 
                           LPARAM lParam
                          );
void _dlgAbout_AlignControls(HWND hDlg);


// =========================================================================
void DisplayDlgAbout(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgAbout\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying \"About...\" dialog\n")));

    DialogBox(
              G_hInstance, 
              MAKEINTRESOURCE(IDD_ABOUT), 
              hWnd, 
              (DLGPROC)dlgAbout_Proc
             );
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgAbout\n")));
}                


// =========================================================================
BOOL CALLBACK dlgAbout_HandleMsg_WM_INITDIALOG(HWND hDlg)
{
    LPWSTR strVersion;
    SHINITDLGINFO shidi;
    WCHAR betaNumber[5];  // Needs to be long enough to hold the beta number
    DWORD driverVersion;
    WCHAR driverVersionStr[256]; // 256 pretty arbitary; just needs to be big
                                 // enough to store the formatted version
                                 // string
    SDUI18N_LANGUAGEW translationDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgAbout_HandleMsg_WM_INITDIALOG\n")));

    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    G_dlgAbout_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_NULL);
    G_URLBrush = CreateSolidBrush(RGB(255, 0, 255));

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgAbout_MenuBar);

    // App title...
    SDUTextSetBold(hDlg, IDC_APP_TITLE, TRUE);
    SetStaticText(hDlg, IDC_APP_TITLE, APP_TITLE);

    // Meeeee!
    SetStaticText(hDlg, IDC_APP_AUTHORCREDIT, APP_AUTHOR_CREDIT);

    // Translator credit...
    SetControlVisible(hDlg, IDC_APP_TRANSLATORCREDIT, FALSE);
    if (!(SDUi18nIsLanguageCodeEnglishW(G_Options->LanguageCode)))
        {
        if (SDUi18n_GetTranslationDetailsW(
                                    G_Options->LanguageCode,
                                    &translationDetails
                                    ))
            {
            SetStaticText(
                          hDlg, 
                          IDC_APP_TRANSLATORCREDIT,
                          SDUParamSubstituteW(
                             _("%1 translation by %2"),
                             TEXT("%s"), translationDetails.LanguageName,
                             TEXT("%s"), translationDetails.TranslatorName,
                             NULL
                             )
                         );

            SetControlVisible(hDlg, IDC_APP_TRANSLATORCREDIT, TRUE);
            }
        }

    // App version...
    if (SDUGetVersionInfoShortFmtAlloc(
                                  NULL,
                                  &strVersion
                                 ))
        {
        SetStaticText(hDlg, IDC_APP_VERSION, strVersion);
        free(strVersion);
        }		

    if (APP_BETA_BUILD > 0)
        {
        _itow(APP_BETA_BUILD, betaNumber, 10);
        AppendStaticText_ResizeToText(hDlg, IDC_APP_VERSION, TEXT(" "));
        AppendStaticText_ResizeToText(hDlg, IDC_APP_VERSION, _("BETA"));
        AppendStaticText_ResizeToText(hDlg, IDC_APP_VERSION, TEXT(" "));
        AppendStaticText_ResizeToText(hDlg, IDC_APP_VERSION, betaNumber);
        }

    // Driver version...
    SetStaticText(
                  hDlg, 
                  IDC_DRIVER_VERSION, 
                  _("<unknown>")
                 );
    if (driver_VersionID(&driverVersion))
        {
        if (_snwprintf(
                       driverVersionStr, 
                       //(sizeof(driverVersionStr) * sizeof(driverVersionStr[0])),
                       (sizeof(driverVersionStr) / sizeof(driverVersionStr[0])),
                       TEXT("v%d.%0.2d.%0.4d"), 
                       (driverVersion & 0xFF000000) / 0x00FF0000,
                       (driverVersion & 0x00FF0000) / 0x0000FF00,
                       (driverVersion & 0x0000FFFF)
                      ) >= 0)
            {
            SetStaticText(hDlg, IDC_DRIVER_VERSION, driverVersionStr);
            }
        }

    _dlgAbout_AlignControls(hDlg);

    // Tappable URL...
    SetStaticText(hDlg, IDC_STATIC_URL, APP_URL);
    SDUTextSetUnderline(hDlg, IDC_STATIC_URL, TRUE);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgAbout_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgAbout_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgAbout_HandleMsg_WM_DESTROY\n")));

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgAbout_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
void _dlgAbout_AlignControls(HWND hDlg)
{
    int alignSetControls[4];

    // Center logo and application details as a group
    alignSetControls[0] = IDC_STATIC_ICON;
    alignSetControls[1] = IDC_APP_TITLE;
    alignSetControls[2] = IDC_APP_AUTHORCREDIT;
    alignSetControls[3] = IDC_APP_VERSION;
    AlignSetControlsToParent(
                             hDlg, 
                             4,
                             alignSetControls,
                             BS_CENTER,
                             BS_NONE
                            );

    // Center translator credit and URL
    CenterControlToParent(hDlg, IDC_STATIC_URL);
    CenterControlToParent(hDlg, IDC_APP_TRANSLATORCREDIT);

    // Center version label and version ID together
    alignSetControls[0] = IDC_STATIC_LABEL_DRIVER_VERSION;
    alignSetControls[1] = IDC_DRIVER_VERSION;
    AlignSetControlsToParent(
                             hDlg, 
                             2,
                             alignSetControls,
                             BS_CENTER,
                             BS_NONE
                            );
}


// =========================================================================
BOOL CALLBACK dlgAbout_HandleMsg_WM_SIZE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgAbout_HandleMsg_WM_SIZE\n")));

    _dlgAbout_AlignControls(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgAbout_HandleMsg_WM_SIZE\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgAbout_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgAbout_HandleMsg_WM_CLOSE\n")));
    if (G_dlgAbout_MenuBar != NULL)
        {
        DestroyWindow(G_dlgAbout_MenuBar);
        G_dlgAbout_MenuBar = NULL;
        }
    if (G_URLBrush != NULL)
        {
        DeleteObject(G_URLBrush);
        G_URLBrush = NULL;
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgAbout_HandleMsg_WM_CLOSE\n")));
    return TRUE;
}


// =========================================================================
HBRUSH _dlgAbout_HandleMsg_WM_CTLCOLORSTATIC(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    // Put URL in blue
    if ((HWND)lParam == GetDlgItem(hDlg, IDC_STATIC_URL))
        {
        SetTextColor((HDC)wParam, COLOR_URL);
        }

    return (GetStockObject(WHITE_BRUSH));
}


// =========================================================================
BOOL CALLBACK dlgAbout_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgAbout_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    switch(LOWORD(wParam))
        {
        case IDC_STATIC_URL:
            {
            DisplayURL(APP_URL);
            retval = TRUE;
            break;
            }

        case IDOK:
            {
            EndDialog(hDlg, TRUE);
            retval = TRUE;
            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgAbout_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgAbout_Proc(
                           HWND hDlg, 
                           WORD msg, 
                           WPARAM wParam, 
                           LPARAM lParam
                          )
{
    BOOL retval = FALSE;

    switch(msg)
        {
        case WM_INITDIALOG:
            {
            retval = dlgAbout_HandleMsg_WM_INITDIALOG(hDlg);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgAbout_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_SIZE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SIZE\n")));
            retval = dlgAbout_HandleMsg_WM_SIZE(hDlg);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = dlgAbout_HandleMsg_WM_CLOSE(hDlg);
            break;
            }

        case WM_COMMAND:
            {
            retval = dlgAbout_HandleMsg_WM_COMMAND(hDlg, wParam, lParam);
            break;
            }


        //case WM_CTLCOLOR:
        case WM_CTLCOLORSTATIC:
        //case WM_CTLCOLOREDIT:
        //case WM_CTLCOLORLISTBOX:
        //case WM_CTLCOLORBTN:
        //case WM_CTLCOLORDLG:
        //case WM_CTLCOLORSCROLLBAR:
        //case WM_CTLCOLORMSGBOX:
            {
            retval = (BOOL)_dlgAbout_HandleMsg_WM_CTLCOLORSTATIC(hDlg, wParam, lParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
