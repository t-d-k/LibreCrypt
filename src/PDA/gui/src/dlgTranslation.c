// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFENULLGUID.h"
#include "main.h"
#include "dlgTranslation.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include "resource.h"

#include "SDUGeneral.h"

#include <stdio.h> // Required for _snwprintf_s 
#include <winuser.h>  // Required for IDOK, etc
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib


// Local to this file...
HWND G_dlgTranslation_MenuBar = NULL;
WCHAR* G_dlgTranslation_LanguageCode = NULL;

// =========================================================================
// Forward declarations...
BOOL CALLBACK dlgTranslation_Proc(
                           HWND hDlg, 
                           UINT message, 
                           WPARAM wParam, 
                           LPARAM lParam
                          );


// =========================================================================
void DisplayDlgTranslation(HWND hWnd, const WCHAR* langCode)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("DisplayDlgTranslation\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying \"Translation...\" dialog\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Language code: %ls\n"), langCode));

    G_dlgTranslation_LanguageCode = calloc(
                                        // +1 for terminating NULL
                                        (wcslen(langCode) + 1), 
                                        sizeof(*langCode)
                                       );
    if (G_dlgTranslation_LanguageCode == NULL)
        {
        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage to hold language code\n")));
        MsgOutOfMemory(hWnd);
        }
    else
        {
        wcscpy(G_dlgTranslation_LanguageCode, langCode);
        DialogBox(
                  G_hInstance, 
                  MAKEINTRESOURCE(IDD_TRANSLATION), 
                  hWnd, 
                  (DLGPROC)dlgTranslation_Proc
                 );
        }
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("DisplayDlgTranslation\n")));
}                


// =========================================================================
BOOL CALLBACK dlgTranslation_HandleMsg_WM_INITDIALOG(HWND hDlg)
{
    SHINITDLGINFO shidi;
    SDUI18N_LANGUAGEW translationDetails;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgTranslation_HandleMsg_WM_INITDIALOG\n")));

    shidi.dwMask = SHIDIM_FLAGS;
    shidi.dwFlags = (
                     SHIDIF_DONEBUTTON | 
                     SHIDIF_SIPDOWN | 
                     SHIDIF_SIZEDLGFULLSCREEN
                    );
    shidi.hDlg = hDlg;
    SHInitDialog(&shidi);

    G_dlgTranslation_MenuBar = SetupMenu_Simple(hDlg, IDR_MENU_NULL);

    SDUi18n_TranslateWindow(hDlg);
    SDUi18n_TranslateCommandBar(G_dlgTranslation_MenuBar);

    // Populate display...

    if (SDUi18n_GetTranslationDetailsW(
                                        G_dlgTranslation_LanguageCode,
                                        &translationDetails
                                        ))
        {
        SetDlgItemText(hDlg, IDC_EDIT_TRANSLATE_LANG_NAME, translationDetails.LanguageName);
        SetDlgItemText(hDlg, IDC_EDIT_TRANSLATE_LANG_CODE, translationDetails.LanguageCode);
        SetDlgItemText(hDlg, IDC_EDIT_TRANSLATE_TRANSLATOR, translationDetails.Translator);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgTranslation_HandleMsg_WM_INITDIALOG\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgTranslation_HandleMsg_WM_DESTROY(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgTranslation_HandleMsg_WM_DESTROY\n")));

    SDUi18n_FreeOriginalStrings(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgTranslation_HandleMsg_WM_DESTROY\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgTranslation_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgTranslation_HandleMsg_WM_CLOSE\n")));
    if (G_dlgTranslation_MenuBar != NULL)
        {
        DestroyWindow(G_dlgTranslation_MenuBar);
        G_dlgTranslation_MenuBar = NULL;
        }
    SecZeroAndFreeWCHARMemory(G_dlgTranslation_LanguageCode);
    G_dlgTranslation_LanguageCode = NULL;
    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgTranslation_HandleMsg_WM_CLOSE\n")));
    return TRUE;
}


// =========================================================================
BOOL CALLBACK dlgTranslation_HandleMsg_WM_COMMAND(HWND hDlg, WPARAM wParam)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("dlgTranslation_HandleMsg_WM_COMMAND\n")));
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: %d\n"), LOWORD(wParam)));

    switch(LOWORD(wParam))
        {
        case IDOK:
            {
            EndDialog(hDlg, TRUE);
            retval = TRUE;
            break;
            }

        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("dlgTranslation_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK dlgTranslation_Proc(
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
            retval = dlgTranslation_HandleMsg_WM_INITDIALOG(hDlg);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = dlgTranslation_HandleMsg_WM_DESTROY(hDlg);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = dlgTranslation_HandleMsg_WM_CLOSE(hDlg);
            break;
            }

        case WM_COMMAND:
            {
            retval = dlgTranslation_HandleMsg_WM_COMMAND(hDlg, wParam);
            break;
            }

        }

    return retval;
}


// =========================================================================
// =========================================================================
