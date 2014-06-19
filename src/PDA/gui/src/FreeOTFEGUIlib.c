// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//



#include "FreeOTFEGUIlib.h"
#include "main.h"

#include "SDUGeneral.h"
#include "FreeOTFElib.h"

//#include <commctrl.h>
#include <Winbase.h>
#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "SDUi18n.h"

#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib

#define CONTROL_CLASS_STATIC "static"
#define CONTROL_CLASS_STATICW TEXT(CONTROL_CLASS_STATIC)
#define CONTROL_CLASS_BUTTON "button"
#define CONTROL_CLASS_BUTTONW TEXT(CONTROL_CLASS_BUTTON)
#define CONTROL_CLASS_EDIT "Edit"
#define CONTROL_CLASS_EDITW TEXT(CONTROL_CLASS_EDIT)

// =========================================================================
// Forward declarations...
WCHAR* _GetString(HINSTANCE hInstance, int stringID);


// =========================================================================
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/wcecoreos5/html/wce50lrfformatmessage.asp
void SDUDisplayLastError_Number()
{
    DWORD error;

    error = GetLastError();
    MsgErrorDWORD_AsHex(NULL, error);
}

// =========================================================================
void SDUDisplayLastError_Text()
{
    DWORD error;

    error = GetLastError();
    SDUDisplayLastError_TextForCode(error);
}


// =========================================================================
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/wcecoreos5/html/wce50lrfformatmessage.asp
void SDUDisplayLastError_TextForCode(DWORD error)
{
    LPVOID lpMsgBuf;

    FormatMessage( 
                  (
                   FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                   FORMAT_MESSAGE_FROM_SYSTEM | 
                   FORMAT_MESSAGE_IGNORE_INSERTS
                  ),
                  NULL,
                  error,
                  0, // Default language
                  (LPTSTR)&lpMsgBuf,
                  0,
                  NULL 
                 );
    MsgError(NULL, lpMsgBuf);

    LocalFree(lpMsgBuf);
}


// =========================================================================
void MsgOutOfMemory(HWND hWnd)
{
    MsgError(hWnd, _("Out of memory; please try again"));
}


// =========================================================================
// uType - Set to one of:
//           MB_ABORTRETRYIGNORE 
//           MB_OK 
//           MB_OKCANCEL 
//           MB_RETRYCANCEL 
//           MB_YESNO 
//           MB_YESNOCANCEL 
// Returns: One of:
//            IDABORT 
//            IDCANCEL 
//            IDIGNORE 
//            IDNO 
//            IDOK 
//            IDRETRY 
//            IDYES 
int MsgPrompt(HWND hWnd, LPCTSTR lpText, UINT uType)
{
    return MsgPromptQuestion(hWnd, lpText, uType);
}


// =========================================================================
// uType - Set to one of:
//           MB_ABORTRETRYIGNORE 
//           MB_OK 
//           MB_OKCANCEL 
//           MB_RETRYCANCEL 
//           MB_YESNO 
//           MB_YESNOCANCEL 
// Returns: One of:
//            IDABORT 
//            IDCANCEL 
//            IDIGNORE 
//            IDNO 
//            IDOK 
//            IDRETRY 
//            IDYES 
int MsgPromptQuestion(HWND hWnd, LPCTSTR lpText, UINT uType)
{
    return MessageBox(
                      hWnd,
                      lpText,
                      _("Confirm"),
                      (MB_ICONQUESTION | uType)
                     );
}


// =========================================================================
// uType - Set to one of:
//           MB_ABORTRETRYIGNORE 
//           MB_OK 
//           MB_OKCANCEL 
//           MB_RETRYCANCEL 
//           MB_YESNO 
//           MB_YESNOCANCEL 
// Returns: One of:
//            IDABORT 
//            IDCANCEL 
//            IDIGNORE 
//            IDNO 
//            IDOK 
//            IDRETRY 
//            IDYES 
int MsgPromptWarn(HWND hWnd, LPCTSTR lpText, UINT uType)
{
    return MessageBox(
                      hWnd,
                      lpText,
                      _("Warning"),
                      (MB_ICONWARNING | uType)
                     );
}


// =========================================================================
// uType - Set to one of:
//           MB_ABORTRETRYIGNORE 
//           MB_OK 
//           MB_OKCANCEL 
//           MB_RETRYCANCEL 
//           MB_YESNO 
//           MB_YESNOCANCEL 
// Returns: One of:
//            IDABORT 
//            IDCANCEL 
//            IDIGNORE 
//            IDNO 
//            IDOK 
//            IDRETRY 
//            IDYES 
int MsgPromptInfo(HWND hWnd, LPCTSTR lpText, UINT uType)
{
    return MessageBox(
                      hWnd,
                      lpText,
                      _("Information"),
                      (MB_ICONINFORMATION | uType)
                     );
}


// =========================================================================
void MsgInfo(HWND hWnd, LPCTSTR lpText)
{
    MessageBox(
               hWnd,
               lpText,
               _("Information"),
               (MB_ICONINFORMATION | MB_OK)
              );
}


// =========================================================================
void MsgWarn(HWND hWnd, LPCTSTR lpText)
{
    MessageBox(
               hWnd,
               lpText,
               _("Warning"),
               (MB_ICONWARNING | MB_OK)
              );
}


// =========================================================================
void MsgError(HWND hWnd, LPCTSTR lpText)
{
    MessageBox(
               hWnd,
               lpText,
               _("Error"),
               (MB_ICONERROR | MB_OK)
              );
}


// =========================================================================
void MsgInfoDWORD_AsHex(HWND hWnd, DWORD number)
{
    WCHAR msg[50]; // Must be large enough to store a DWORD as a hex string

    wsprintf(msg, TEXT("0x%0.8x"), number);
    MsgInfo(hWnd, msg);
}


// =========================================================================
void MsgWarnDWORD_AsHex(HWND hWnd, DWORD number)
{
    WCHAR msg[50]; // Must be large enough to store a DWORD as a hex string

    wsprintf(msg, TEXT("0x%0.8x"), number);
    MsgWarn(hWnd, msg);
}


// =========================================================================
void MsgErrorDWORD_AsHex(HWND hWnd, DWORD number)
{
    WCHAR msg[50]; // Must be large enough to store a DWORD as a hex string

    wsprintf(msg, TEXT("0x%0.8x"), number);
    MsgError(hWnd, msg);
}


// =========================================================================
void MsgInfoString(HWND hWnd, HINSTANCE hInstance, int stringID)
{
    WCHAR* str;

    str = GetString(hInstance, stringID);
    if (str != NULL)
        {
        MsgInfo(hWnd, str);
        free(str);
        }
}


// =========================================================================
void MsgWarnString(HWND hWnd, HINSTANCE hInstance, int stringID)
{
    WCHAR* str;

    str = GetString(hInstance, stringID);
    if (str != NULL)
        {
        MsgWarn(hWnd, str);
        free(str);
        }
}


// =========================================================================
void MsgErrorString(HWND hWnd, HINSTANCE hInstance, int stringID)
{
    WCHAR* str;

    str = GetString(hInstance, stringID);
    if (str != NULL)
        {
        MsgError(hWnd, str);
        free(str);
        }
}


// =========================================================================
// See: 
//   SetStaticText(...)
//   SetStaticText_HWND(...)
// Set static text, resizing the control as appropriate
void SetStaticText(HWND hWnd, int nIDDlgItem, LPCTSTR lpText)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    SetStaticText_HWND(hWndTmp, lpText);
}


// =========================================================================
// See: 
//   SetStaticText(...)
//   SetStaticText_HWND(...)
// Set static text, resizing the control as appropriate
void SetStaticText_HWND(HWND hWnd, LPCTSTR lpText)
{
    ResizeStaticToText_HWNDText(hWnd, lpText);

    // Set the text.
    SetWindowText(hWnd, lpText);
}

void ResizeStaticToText_ChildText(HWND hWnd, int nIDDlgItem, LPCTSTR lpText)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    ResizeStaticToText_HWNDText(hWndTmp, lpText);
}

void ResizeStaticToText_Child(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    ResizeStaticToText_HWND(hWndTmp);
}

void ResizeStaticToText_HWND(HWND hWnd)
{
    WCHAR* pwWS;
    int pwWSSize;
    int pwLen;

    pwLen = GetWindowTextLength(hWnd);
        
    // +1 for terminating NULL
    pwWSSize = (pwLen + 1) * sizeof(*pwWS);
    pwWS = malloc(pwWSSize);

    if (pwWS == NULL)
        {
        MsgOutOfMemory(hWnd);
        }
    else
        {
        memset(pwWS, 0, pwWSSize);
        GetWindowText( 
                       hWnd, 
                       pwWS,
                       pwWSSize
                     );

        ResizeStaticToText_HWNDText(hWnd, pwWS);
        }

    SecZeroAndFreeMemory(pwWS, pwWSSize);
}


// Notice: These values were determined by:
//
//   1) Uncommenting the second GetWindowRect(...) call
//   2) Placing a breakpoint on both the first and second GetWindowRect(...) 
//      calls
//   3) Running this function for a radiobutton/checkbox, etc which only has a 
//      single (short) text string
//   4) Adjusting these values until the value returned by the first
//      GetWindowRect(...) is the same as that returned by the second 
//      GetWindowRect(...)
//
// Note that we can't use GetSystemMetrics(...) with 
// SM_CXMENUCHECK/SM_CYMENUCHECK/etc under Windows Mobile to get this info
//
// Indent (e.g. text on a checkbox is indented by the checkbox)
#define CTRL_X_INDENT_RADIOBUTTON  24
#define CTRL_X_INDENT_CHECKBOX     24
// This one *really* shouldn't be needed, but it looks like it is?!
// The "Main driver version:" string on the about dialog gets messed up
// without this, or if it's set to 1
#define CTRL_X_INDENT_STATIC        2
// Minimum control height
#define CTRL_Y_MIN_RADIOBUTTON     16
#define CTRL_Y_MIN_CHECKBOX        16
void ResizeStaticToText_HWNDText(HWND hWnd, LPCTSTR lpText)
{
    HFONT hfont;
    HDC hdc;
//    SIZE size;
    UINT ctrlWordBreaks;
    DWORD wndStyle;
    RECT sizeRect;
    int ctrlXIndent;
    int ctrlYMin;

    // Identify the size of the text to be displayed...
    hfont = (HFONT)SendMessage(
                                      hWnd,
                                      WM_GETFONT,
                                      0,
                                      0
                                     );
    hdc = GetDC(hWnd);
    hfont = (HFONT)SelectObject(hdc, hfont);

    // Note: "SIMPLE" is win32 only.
    // Note: if nowrap OR simple then extend X out, otherwise extend Y out
    wndStyle = GetWindowLong(hWnd, GWL_STYLE);
   
    ctrlWordBreaks = 0;
    ctrlXIndent = 0;
    ctrlYMin = 0;
    // For static controls...
    if (IsWindow_Static(hWnd))
        {
        ctrlWordBreaks = DT_WORDBREAK;
        if ((wndStyle & SS_LEFTNOWORDWRAP) == SS_LEFTNOWORDWRAP)
            {
            ctrlWordBreaks = 0;
            }

        ctrlXIndent = CTRL_X_INDENT_STATIC;
        }
    // For checkbox controls...
    else if (IsWindow_Checkbox(hWnd))
        {
        ctrlWordBreaks = 0;
        if ((wndStyle & BS_MULTILINE) == BS_MULTILINE)
            {
            ctrlWordBreaks = DT_WORDBREAK;
            }

        ctrlXIndent = CTRL_X_INDENT_CHECKBOX;
        ctrlYMin = CTRL_Y_MIN_CHECKBOX;
        }
    // For radiobutton controls...
    else if (IsWindow_RadioButton(hWnd))
        {
        ctrlWordBreaks = 0;
        if ((wndStyle & BS_MULTILINE) == BS_MULTILINE)
            {
            ctrlWordBreaks = DT_WORDBREAK;
            }

        ctrlXIndent = CTRL_X_INDENT_RADIOBUTTON;
        ctrlYMin = CTRL_Y_MIN_RADIOBUTTON;
        }
    // For edit controls...
    else if (IsWindow_Edit(hWnd))
        {
        ctrlWordBreaks = 0;
        if ((wndStyle & ES_MULTILINE) == ES_MULTILINE)
            {
            ctrlWordBreaks = DT_WORDBREAK;
            }
        }

    GetWindowRect(hWnd, &sizeRect);
    sizeRect.right = (sizeRect.right - sizeRect.left);
    sizeRect.right -= ctrlXIndent;
    sizeRect.bottom = (sizeRect.bottom - sizeRect.top);
    sizeRect.bottom -= ctrlYMin;
    sizeRect.left = 0;
    sizeRect.top = 0;
    DrawText(hdc, lpText, -1, &sizeRect, (DT_CALCRECT | ctrlWordBreaks));
//    GetTextExtentPoint32(hdc, lpText, wcslen(lpText), &size);


    // Clean up...
    hfont = (HFONT)SelectObject(hdc, hfont);
    ReleaseDC(hWnd, hdc);

    // Resize the control...
    SetWindowPos(
                 hWnd, 
                 HWND_TOP, 
                 0,
                 0, 
//                 size.cx+5, 
//                 size.cy, 
                 (sizeRect.right + ctrlXIndent),
                 max(sizeRect.bottom, ctrlYMin),
                 (SWP_NOMOVE | SWP_NOZORDER)
                );
//    GetWindowRect(hWnd, &sizeRect);

}


// =========================================================================
void AppendStaticText(HWND hWnd, int ctrlID, LPCTSTR lpText)
{
    AppendStaticText_Simple(hWnd, ctrlID, lpText);
}

// =========================================================================
void AppendStaticText_Simple(HWND hWnd, int ctrlID, LPCTSTR lpText)
{
    AppendText(hWnd, ctrlID, lpText, FALSE);
}

// =========================================================================
void AppendStaticText_ResizeToText(HWND hWnd, int ctrlID, LPCTSTR lpText)
{
    AppendText(hWnd, ctrlID, lpText, TRUE);
}

// =========================================================================
void AppendText(HWND hWnd, int ctrlID, LPCTSTR lpText, BOOL resizeCtrl)
{
    WCHAR* pwWS;
    int pwWSSize;
    int pwLen;

    pwLen = GetWindowTextLength(GetDlgItem(hWnd, ctrlID));
        
    // +1 for terminating NULL
    pwWSSize = (pwLen + wcslen(lpText) + 1) * sizeof(*pwWS);
    pwWS = malloc(pwWSSize);

    if (pwWS == NULL)
        {
        MsgOutOfMemory(hWnd);
        }
    else
        {
        memset(pwWS, 0, pwWSSize);
        GetDlgItemText( 
                       hWnd, 
                       ctrlID,
                       pwWS,
                       pwWSSize
                     ); 

        wcscat(pwWS, lpText);

        // Set the text.
        if (resizeCtrl)
            {
            SetStaticText(hWnd, ctrlID, pwWS);
            }
        else
            {
            SetDlgItemText(hWnd, ctrlID, pwWS);
            }
        }

    SecZeroAndFreeMemory(pwWS, pwWSSize);
}


// =========================================================================
// Add button to menu
// Returns: TRUE/FALSE on success/failure
BOOL MenuButtonAdd(
                   HWND hWndMenu, 
                   UINT bitmapResource, 
                   UINT bitmapIdx, 
                   UINT cmdID
                  )
{
    BOOL retval = TRUE;
    TBBUTTON tbbButton;
    TBADDBITMAP tbabBitmap;

    // TB_BUTTONSTRUCTSIZE message, required for backward
    // compatibility(?)
    SendMessage(
                hWndMenu,
                TB_BUTTONSTRUCTSIZE,
                (WPARAM)sizeof(tbbButton),
                0
               );

    tbabBitmap.hInst = G_hInstance;
    tbabBitmap.nID = bitmapResource;
    SendMessage(hWndMenu, TB_ADDBITMAP, 0, (LPARAM)&tbabBitmap);

    ZeroMemory(&tbbButton, sizeof(tbbButton));

    // Add button...
    tbbButton.iBitmap = bitmapIdx; 
    tbbButton.fsState = TBSTATE_ENABLED;
    tbbButton.fsStyle = TBSTYLE_BUTTON;
    tbbButton.idCommand = cmdID;
    SendMessage(hWndMenu, TB_ADDBUTTONS, 1, (LPARAM)&tbbButton);

    return retval;
}


// =========================================================================
// Enable/disable a menu button
// Returns: TRUE/FALSE on success/failure
void MenuButtonEnable(
                   HWND hWndMenu, 
                   UINT cmdID,
                   BOOL enable
                  )
{
    SendMessage(
                hWndMenu, 
                TB_ENABLEBUTTON, 
                cmdID, 
                (LPARAM)enable
               );
}


// =========================================================================
// Enable/disable a menu item
// Returns: TRUE/FALSE on success/failure
void MenuItemEnableWnd(
                   HWND hWndMenu, 
                   UINT cmdID,
                   BOOL enable
                  )

{
    HMENU hMenu;

    hMenu = (HMENU)SendMessage(
                               hWndMenu, 
                               SHCMBM_GETMENU, 
                               (WPARAM)0,
                               (LPARAM)0
                              ); 

    MenuItemEnableMenu(hMenu, cmdID, enable);

    DrawMenuBar(hWndMenu);
}


// =========================================================================
// Enable/disable a menu item
// Returns: TRUE/FALSE on success/failure
void MenuItemEnableMenu(
                   HMENU hMenu, 
                   UINT cmdID,
                   BOOL enable
                  )
{
    UINT uEnable;

    uEnable = MF_BYCOMMAND;
    if (enable)
        {
        uEnable |= MF_ENABLED;
        }
    else
        {
        uEnable |= MF_GRAYED;
        }
    

    EnableMenuItem(hMenu, cmdID,uEnable); 
}


// =========================================================================
// Remove a menu item
// Returns: TRUE/FALSE on success/failure
void MenuItemRemoveWnd(
                   HWND hWndMenu, 
                   UINT cmdID
)
{
    HMENU hMenu;

    hMenu = (HMENU)SendMessage(
                               hWndMenu, 
                               SHCMBM_GETMENU, 
                               (WPARAM)0,
                               (LPARAM)0
                              ); 

    MenuItemRemoveMenu(hMenu, cmdID);

    DrawMenuBar(hWndMenu);
}


// =========================================================================
// Remove a menu item
// Returns: TRUE/FALSE on success/failure
void MenuItemRemoveMenu(
                   HMENU hMenu, 
                   UINT cmdID
)
{
    RemoveMenu(hMenu, cmdID, MF_BYCOMMAND); 
}


// =========================================================================
// Adjust spin control, checking bounds
void AdjustSpinControl(
                    HWND hDlg,
                    int nIDDlgItem,
                    int iMin,
                    int iMax,
                    int iDelta
                   )
{
    int iValue;

    iValue = GetDlgItemInt(hDlg, nIDDlgItem, NULL, TRUE);
    iValue = iValue + iDelta;
    if (iValue < iMin)
       {
       iValue = iMin;
       }
    else if (iValue > iMax)
       {
       iValue = iMax;
       }
    SetDlgItemInt(hDlg, nIDDlgItem, iValue, TRUE);

}


// =========================================================================
// Returns NULL on failure. Mallocs and returns pointer to char* password
// Note: It it is the *callers* responsibility to free of the returned string
char* GetPassword(
    HWND hDlg, 
    UINT ctrlID
)
{
    WCHAR* pwWS;
    int pwWSSize;
    int userPasswordSize;
    int pwLen;
    char* userPassword;
    HWND oldFocus;


    // Under Windows Mobile 6.5, Microsoft changed the way in which password 
    // edit controls behave.
    // When the user enters a character, it's shown as the character entered. 
    // Then a few seconds later (if the user switches to another control) it 
    // changes to a "*"
    // While it's shown as a normal character (i.e. before it changes to a
    // "*"), WE CAN'T GET THE FULL PASSWORD! Only those characters which have 
    // been changed to "*" will be returned!
    // To fix this, we switch the focus, and then set it back again to emulate
    // tabbing off to another control and back again, causing the last 
    // character entered to be changed to a "*" on the display.

    // Store current focus
    oldFocus = GetFocus();
    // Switch focus elsewhere...
    SetFocus(GetWindow(oldFocus, GW_HWNDNEXT));
    // Switch focus back to what it was.
    SetFocus(oldFocus);


    pwLen = GetWindowTextLength(GetDlgItem(hDlg, ctrlID));
        
    // +1 for terminating NULL
    pwWSSize = (pwLen + 1) * sizeof(*pwWS);
    userPasswordSize = (pwLen + 1) * sizeof(*userPassword);
    pwWS = malloc(pwWSSize);
    userPassword = malloc(userPasswordSize);

    if (
        (pwWS == NULL) ||
        (userPassword == NULL) 
       )
        {
        MsgOutOfMemory(hDlg);
        SecZeroAndFreeMemory(userPassword, userPasswordSize);
        userPassword = NULL;
        }
    else
        {
        memset(pwWS, 0, pwWSSize);
        memset(userPassword, 0, userPasswordSize);
        GetDlgItemText( 
                       hDlg, 
                       ctrlID,
                       pwWS,
                       pwWSSize
                     ); 
        wcstombs(userPassword, pwWS, wcslen(pwWS));
        }

    SecZeroAndFreeMemory(pwWS, pwWSSize);

    return userPassword;
}


// =========================================================================
void
SetDlgItemString(
    HINSTANCE hInstance,
    HWND hWnd,
    int ctrlID,
    int stringID
)
{
    WCHAR* str;

    str = GetString(hInstance, stringID);
    if (str != NULL)
        {
        SetDlgItemText(hWnd, ctrlID, str);
        free(str);
        }

}

void
SetDlgItemString_WithResize(
    HINSTANCE hInstance,
    HWND hWnd,
    int ctrlID,
    int stringID
)
{
    WCHAR* str;

    str = GetString(hInstance, stringID);
    if (str != NULL)
        {
        SetDlgItemText_WithResize(hWnd, ctrlID, str);
        free(str);
        }

}

// =========================================================================
// Obtain string resource
// Note: It is the *callers* responsibility to free off the string returned
// Returns NULL on failure
WCHAR* GetString(HINSTANCE hInstance, int stringID)
{
    WCHAR* retval;
    WCHAR* loadedString;
    const WCHAR* translated;


    loadedString = _GetString(hInstance, stringID);
    if (loadedString == NULL)
        {
        retval = loadedString;
        }
    else
        {
        translated = SDUi18n_GetTextW(loadedString);

        // Make copy of translated string, so caller can free it off
        // +1 to include terminating NULL
        retval = calloc((wcslen(translated) + 1), sizeof(*retval));
        if (retval != NULL)
            {
            wcscpy(retval, translated);
            }

        free(loadedString);
        }

    return retval;
}

// =========================================================================
WCHAR* _GetString(HINSTANCE hInstance, int stringID)
{
    WCHAR* retval;
    WCHAR* tmpStr;
    WORD strLen;

    retval = NULL;

    tmpStr = (WCHAR*)LoadString(hInstance, stringID, NULL, 0);
    if (tmpStr != NULL)
        {
        strLen = ((WORD*)tmpStr)[-1];

        tmpStr = calloc((strLen + 1), sizeof(*tmpStr));
        if (tmpStr != NULL)
            {
            if (LoadString(hInstance, stringID, tmpStr, (strLen + 1)) == strLen)
                {
                retval = tmpStr;
                }
            else
                {
                free(tmpStr);
                }
            }
        }

    return retval;
}

// As SetDlgItemText, but resize the control to match the text size
WINUSERAPI
BOOL
WINAPI
SetDlgItemText_WithResizeW(
    HWND hDlg,
    int nIDDlgItem,
    LPCWSTR lpString)
{
    BOOL retval;
    retval = SetDlgItemText(hDlg, nIDDlgItem, lpString);

    // Adding this condition truncates "Associate with ".vol" files" 
    // for some reason?!
    //if (retval)
        {
        ResizeStaticToText_Child(hDlg, nIDDlgItem);
        }

    return retval;
}


// =========================================================================
void
SetControlVisible(
    HWND ParentWindow,
    int CtrlID,
    BOOL Visible
)
{
    HWND tmpHWND;

    tmpHWND = GetDlgItem(ParentWindow, CtrlID);
    if (tmpHWND != NULL)
        {
        if (Visible)
            {
            ShowWindow(tmpHWND, SW_SHOW);
            }
        else
            {
            ShowWindow(tmpHWND, SW_HIDE);
            }
        }
}


// =========================================================================
BOOL
IsControlVisible(
    HWND ParentWindow,
    int CtrlID
)
{
    BOOL retval = FALSE;
    HWND tmpHWND;

    tmpHWND = GetDlgItem(ParentWindow, CtrlID);
    if (tmpHWND != NULL)
        {
        retval = IsWindowVisible(tmpHWND);
        }

    return retval;
}


// =========================================================================
void
SetControlEnabled(
    HWND ParentWindow,
    int CtrlID,
    BOOL Enabled
)
{
    HWND tmpHWND;

    tmpHWND = GetDlgItem(ParentWindow, CtrlID);
    if (tmpHWND != NULL)
        {
        EnableWindow(tmpHWND, Enabled);
        }
}


// =========================================================================
BOOL
IsControlEnabled(
    HWND ParentWindow,
    int CtrlID
)
{
    BOOL retval = FALSE;
    HWND tmpHWND;

    tmpHWND = GetDlgItem(ParentWindow, CtrlID);
    if (tmpHWND != NULL)
        {
        retval = IsWindowEnabled(tmpHWND);
        }

    return retval;
}


// =========================================================================
void
SetControlReadonly(
    HWND ParentWindow,
    int CtrlID,
    BOOL Readonly
)
{
    HWND tmpHWND;

    tmpHWND = GetDlgItem(ParentWindow, CtrlID);
    if (tmpHWND != NULL)
        {
        SendMessage(tmpHWND, EM_SETREADONLY, Readonly, 0);
        }
}


// =========================================================================
void SizeControlMaxDepth(
    HWND hDlg,
    int ctrlID
)
{
    RECT rcParent;
    RECT rcClient;
    HWND hWndTmp;
    POINT pnt;

    if (GetClientRect(hDlg, &rcParent))
        {
        hWndTmp = GetDlgItem(hDlg, ctrlID); 
        if (hWndTmp != NULL)
            {
            if (GetWindowRect(hWndTmp, &rcClient))
                {
                pnt.x = rcClient.left;
                pnt.y = rcClient.top;
                ScreenToClient(hDlg, &pnt);
                rcClient.left = pnt.x;
                rcClient.top = pnt.y;

                pnt.x = rcClient.right;
                pnt.y = rcClient.bottom;
                ScreenToClient(hDlg, &pnt);
                rcClient.right = pnt.x;
                rcClient.bottom = pnt.y;

                SetWindowPos(
                             hWndTmp, 
                             NULL, 
                             rcClient.left, 
                             rcClient.top, 
                             (rcClient.right - rcClient.left),
                             (rcParent.bottom - rcClient.top), 
                             SWP_NOZORDER
                            );
                }
            }
        }
}


// =========================================================================
void SizeControlMaxWidth(
    HWND hDlg,
    int ctrlID
)
{
    SizeControlMaxWidthBorder(hDlg, ctrlID, 0);
}


// =========================================================================
void SizeControlMaxWidthBorder(
    HWND hDlg,
    int ctrlID,
    int Border
)
{
    RECT rcParent;
    RECT rcClient;
    HWND hWndTmp;
    POINT pnt;

    if (GetClientRect(hDlg, &rcParent))
        {
        hWndTmp = GetDlgItem(hDlg, ctrlID); 
        if (hWndTmp != NULL)
            {
            if (GetWindowRect(hWndTmp, &rcClient))
                {
                pnt.x = rcClient.left;
                pnt.y = rcClient.top;
                ScreenToClient(hDlg, &pnt);
                rcClient.left = pnt.x;
                rcClient.top = pnt.y;

                pnt.x = rcClient.right;
                pnt.y = rcClient.bottom;
                ScreenToClient(hDlg, &pnt);
                rcClient.right = pnt.x;
                rcClient.bottom = pnt.y;

                SetWindowPos(
                             hWndTmp, 
                             NULL, 
                             Border, 
                             rcClient.top, 
                             (rcParent.right - (Border * 2)),
                             (rcClient.bottom - rcClient.top), 
                             SWP_NOZORDER
                            );
                }
            }
        }

}


// =========================================================================
void SizeChildWindowToParent(
    HWND hChildWnd
)
{
    HWND hParentWnd;
    
    if (hChildWnd != NULL)
        {
        hParentWnd = GetParent(hChildWnd); 
        SizeWindowToWindowClient(hParentWnd, hChildWnd);
        }

}


// =========================================================================
void SizeControlToParent(
    HWND hDlg,
    int ctrlID
)
{
    HWND hWndTmp;
    
    hWndTmp = GetDlgItem(hDlg, ctrlID); 
    if (hWndTmp != NULL)
        {
        SizeChildWindowToParent(hWndTmp);
        }
}


// =========================================================================
void SizeWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow
)
{
    RECT rcParent;
    
    if (
        (ResizeWindow != NULL) &&
        (BaseWindow != NULL)
       )
        {
        if (GetClientRect(BaseWindow, &rcParent))
            {
            SetWindowPos(
                         ResizeWindow, 
                         NULL, 
                         rcParent.left, 
                         rcParent.top, 
                         (rcParent.right - rcParent.left),
                         (rcParent.bottom - rcParent.top), 
                         SWP_NOZORDER
                        );
            }
        }

}


// =========================================================================
void SizeWindowToOverlapWindow(
    HWND BaseWindow,
    HWND ResizeWindow
)
{
    RECT rcBase;
    POINT pnt;

    if (GetWindowRect(BaseWindow, &rcBase))
        {
        pnt.x = rcBase.left;
        pnt.y = rcBase.top;
        ScreenToClient(ResizeWindow, &pnt);
        rcBase.left = pnt.x;
        rcBase.top = pnt.y;

        pnt.x = rcBase.right;
        pnt.y = rcBase.bottom;
        ScreenToClient(ResizeWindow, &pnt);
        rcBase.right = pnt.x;
        rcBase.bottom = pnt.y;

        SetWindowPos(
                     ResizeWindow, 
                     NULL, 
                     rcBase.left, 
                     rcBase.top, 
                     (rcBase.right - rcBase.left),
                     (rcBase.bottom - rcBase.top), 
                     SWP_NOZORDER
                    );
        }

}


// =========================================================================
void CenterWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow
)
{
    AlignWindowToWindowClient(
                              BaseWindow,
                              ResizeWindow,
                              BS_CENTER,
                              BS_NONE
                             );
}


// =========================================================================
void AlignSetWindowsToWindowClient(
    HWND BaseWindow,
    int ControlCount,
    HWND* ResizeWindow,  // Pointer to array
    int Horizontal,
    int Vertical
)
{
    BOOL allOK;
    RECT rcParentWindow;
    RECT rcChildWindow;
    int sizeX;
    int sizeY;
    POINT pnt;
    int screenXMin;
    int screenYMin;
    int screenXMax;
    int screenYMax;
    int indentX;  // Relative to the left edge of the parent
    int indentY;  // Relative to the top of the parent
    int nudgeX;
    int nudgeY;
    int i;


    allOK = TRUE;

    // Sanity...
    if (allOK)
        {
        if (
            (BaseWindow == NULL) ||
            (ResizeWindow == NULL)
           )
            {
            allOK = FALSE;
            }
        }


    // Get parent's details
    if (allOK)
        {
        if (!(GetWindowRect(BaseWindow, &rcParentWindow)))
            {
            allOK = FALSE;
            }
        else
            {
            screenXMin = rcParentWindow.right;
            screenYMin = rcParentWindow.bottom;
            screenXMax = rcParentWindow.left;
            screenYMax = rcParentWindow.top;
            }
        }

    // Get the extents of the set of controls
    if (allOK)
        {
        allOK = FALSE;
        for(i = 0; i < ControlCount; i++)
            {
            if (ResizeWindow[i] != NULL)
                {
                if (GetWindowRect(ResizeWindow[i], &rcChildWindow))
                    {
                    screenXMin = min(screenXMin, rcChildWindow.left);
                    screenYMin = min(screenYMin, rcChildWindow.top);
                    screenXMax = max(screenXMax, rcChildWindow.right);
                    screenYMax = max(screenYMax, rcChildWindow.bottom);

                    // Flag at least one was OK
                    allOK = TRUE;
                    }
                }
            }
        }
    

    // Identify how to move
    if (allOK)
        {
        indentX = 0;
        indentY = 0;

        sizeX = (screenXMax - screenXMin);
        sizeY = (screenYMax - screenYMin);

        if (Horizontal == BS_NONE)
            {
            indentX = (screenXMin - rcParentWindow.left);
            }
        else if (Horizontal == BS_RIGHT)
            {
            indentX = ((rcParentWindow.right - rcParentWindow.left) - sizeX);
            }
        else if (Horizontal == BS_LEFT)
            {
            indentX = 0;
            }
        else if (Horizontal == BS_CENTER)
            {
            indentX = (((rcParentWindow.right - rcParentWindow.left) - sizeX) / 2);
            }

        if (Vertical == BS_NONE)
            {
            indentY = (screenYMin - rcParentWindow.top);
            }
        else if (Vertical == BS_BOTTOM)
            {
            indentY = ((rcParentWindow.bottom - rcParentWindow.top) - sizeY);
            }
        else if (Vertical == BS_TOP)
            {
            indentY = 0;
            }
        else if (Vertical == BS_CENTER)
            {
            indentY = (((rcParentWindow.bottom - rcParentWindow.top) - sizeY) / 2);
            }

        }


    if (allOK)
        {
        allOK = FALSE;
        for(i = 0; i < ControlCount; i++)
            {
            if (ResizeWindow[i] != NULL)
                {
                if (GetWindowRect(ResizeWindow[i], &rcChildWindow))
                    {
                    nudgeX = ((rcParentWindow.left + indentX) - screenXMin);
                    nudgeY = ((rcParentWindow.top + indentY) - screenYMin);

                    pnt.x = rcChildWindow.left + nudgeX;
                    pnt.y = rcChildWindow.top + nudgeY;
                    ScreenToClient(BaseWindow, &pnt);

                    SetWindowPos(
                                 ResizeWindow[i], 
                                 NULL, 
                                 pnt.x, 
                                 pnt.y, 
                                 0,  // SWP_NOSIZE
                                 0,  // SWP_NOSIZE
                                 (SWP_NOZORDER | SWP_NOSIZE)
                                );
                    }
                }
            }
        }

}


// =========================================================================
// Note: All of the child windows *must* have the same parent
void AlignSetChildWindowsToParent(
    int ControlCount,
    HWND* hChildWnd,  // Pointer to array
    int Horizontal,
    int Vertical
)
{
    HWND hParentWnd;
    
    if (
        (hChildWnd != NULL) &&
        (ControlCount > 0)
       )
        {
        hParentWnd = GetParent(hChildWnd[0]); 
        AlignSetWindowsToWindowClient(
                                      hParentWnd, 
                                      ControlCount,
                                      hChildWnd,
                                      Horizontal,
                                      Vertical
                                     );
        }

}


// =========================================================================
// Note: All of the controls *must* have the same parent
void AlignSetControlsToParent(
    HWND hWndParent,
    int ControlCount,
    int* ctrlID,  // Pointer to array
    int Horizontal,
    int Vertical
)
{
    HWND* hWndTmp;
    int i;

    hWndTmp = calloc(ControlCount, sizeof(HWND));
    
    if (hWndTmp != NULL)
        {
        for(i = 0; i < ControlCount; i++)
            {
            hWndTmp[i] = GetDlgItem(hWndParent, ctrlID[i]); 
            }

        AlignSetChildWindowsToParent(
                                     ControlCount,
                                     hWndTmp, 
                                     Horizontal,
                                     Vertical
                                    );
        }
}


// =========================================================================
void AlignWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow,
    int Horizontal,
    int Vertical
)
{
    AlignSetWindowsToWindowClient(
                                  BaseWindow,
                                  1,
                                  &ResizeWindow, 
                                  Horizontal,
                                  Vertical
                                 );
}


// =========================================================================
void CenterChildWindowParent(
    HWND hChildWnd
)
{
    HWND hParentWnd;
    
    if (hChildWnd != NULL)
        {
        hParentWnd = GetParent(hChildWnd); 
        CenterWindowToWindowClient(hParentWnd, hChildWnd);
        }

}


// =========================================================================
void CenterControlToParent(
    HWND hDlg,
    int ctrlID
)
{
    HWND hWndTmp;
    
    hWndTmp = GetDlgItem(hDlg, ctrlID); 
    if (hWndTmp != NULL)
        {
        CenterChildWindowParent(hWndTmp);
        }
}


// =========================================================================
// Disable all instances of the specified menuitem from the command bar and
// all submenus
void CommandBar_ItemEnable(
    HWND hCmdBar,
    UINT cmdID,
    BOOL enabled
)
{
    TBBUTTON tbb;
    int idx;

    MenuItemEnableWnd(hCmdBar, cmdID, enabled);
    MenuButtonEnable(hCmdBar, cmdID, enabled);

    // If our commandbar was created using out procedure which sets up WM5
    // softkey menus, sweep through all commandbar button menus
    idx = 0;
    while (TRUE)
        {
        memset(&tbb, 0, sizeof(tbb));

        if (!(SendMessage(
                    hCmdBar,
                    TB_GETBUTTON,
                    (WPARAM)idx,
                    (LPARAM)(&tbb)
                   )))
            {
            break;
            }

        MenuItemEnableMenu((HMENU)tbb.dwData, cmdID, enabled);

        idx++;
        }

}


// =========================================================================
void DisplayURL(WCHAR* url)
{
    SHELLEXECUTEINFO seInfo;

    // Launch WWW browser

    // We use ShellExecuteEx, rather than by using the executable's
    // filename and CreateProcess(...)
    seInfo.cbSize = sizeof(seInfo);
    seInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
    seInfo.hwnd = NULL;
    seInfo.lpVerb = TEXT("open");
    seInfo.lpFile = url;
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
                  EXE_MSIE,
                  APP_URL,

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


// =========================================================================
// Select the item in a combobox with the index supplied
BOOL SetComboBoxSelectedIdx(HWND hDlg, int comboboxCtrlID, int selectItemIdx)
{
    BOOL retval;

    retval = (SendMessage(
                GetDlgItem(hDlg, comboboxCtrlID),      
                (UINT)CB_SETCURSEL,      
                selectItemIdx,
                0
               ) != CB_ERR);

    return retval;
}

// =========================================================================
// Select the item in a combobox with the same text as supplied
BOOL SetComboBoxSelected(HWND hDlg, const int comboboxCtrlID, const WCHAR* selectItem)
{   
    BOOL retval;
    WCHAR* buffer;
    int itemLength;
    int cntItems;
    int i;

    retval = FALSE;

    cntItems = SendMessage(
                        GetDlgItem(hDlg, comboboxCtrlID),      
                        (UINT)CB_GETCOUNT,      
                        0,
                        0
                       );
    for (i = 0; i < cntItems; i++)
        {
        itemLength = SendMessage(
                            GetDlgItem(hDlg, comboboxCtrlID),      
                            (UINT)CB_GETLBTEXTLEN,      
                            i,
                            0
                           );
        // +1 to include terminating NULL
        buffer = calloc((itemLength + 1), sizeof(*buffer));
        if (buffer != NULL)
            {
            itemLength = SendMessage(
                                GetDlgItem(hDlg, comboboxCtrlID),      
                                (UINT)CB_GETLBTEXT,      
                                i,
                                (LPARAM)buffer
                               );
            if (wcscmp(selectItem, buffer) == 0)
                {
                SetComboBoxSelectedIdx(hDlg, comboboxCtrlID, i);
                break;
                }
            SecZeroAndFreeWCHARMemory(buffer);
            }
        }

    return retval;
}


// =========================================================================
// Paste any text from the clipboard to the specified control
BOOL PasteTextFromClipboard(HWND hWnd, int nIDDlgItem)
{
    HANDLE hClipData;
    WCHAR *clipboardText = NULL;
    BOOL retval;

    retval = FALSE;

    if (IsClipboardFormatAvailable(CF_TEXT))
        {
        if (OpenClipboard(hWnd))
            {
            // CF_TEXT used for char*
            //hClipData = GetClipboardData(CF_TEXT);
            // CF_UNICODETEXT used for WCHAR*
            hClipData = GetClipboardData(CF_UNICODETEXT);
            if (hClipData)
                {
                clipboardText = GlobalLock(hClipData);
                SetDlgItemText(hWnd, nIDDlgItem, clipboardText);
                GlobalUnlock(hClipData);

                retval = TRUE;
                }

            CloseClipboard();
            }
        }

    return retval;
} 


// =========================================================================
// Returns the index of the item added, or -1 on error
int Combobox_AddItem(
    HWND hDlg,
    UINT ComboboxID,
    const WCHAR* ItemText,
    void* ItemUserData
)
{
    LRESULT itemIdx;

    itemIdx = SendMessage(
                        GetDlgItem(hDlg, ComboboxID),      
                        (UINT)CB_ADDSTRING,      
                        0,  // From API docs, must be zero
                        (LPARAM)ItemText
                       );

    if (
        (itemIdx == CB_ERR) ||
        (itemIdx == CB_ERRSPACE)
       )
        {
        itemIdx = -1;
        }
    else
        {
        if (SendMessage(
                            GetDlgItem(hDlg, ComboboxID),      
                            (UINT)CB_SETITEMDATA,      
                            itemIdx,
                            (LPARAM)ItemUserData
                           ) == CB_ERR)
            {
            // Oh well... Back out that one!
            SendMessage(
                            GetDlgItem(hDlg, ComboboxID),      
                            (UINT)CB_DELETESTRING,      
                            itemIdx,
                            0
                           );
            }
        }

    return itemIdx;
}

// =========================================================================
// Returns the index of the item added, or -1 on error
int Listbox_AddItem(
    HWND hDlg,
    UINT ListboxID,
    const WCHAR* ItemText,
    void* ItemUserData
)
{
    LRESULT itemIdx;

    itemIdx = SendMessage(
                        GetDlgItem(hDlg, ListboxID),      
                        (UINT)LB_ADDSTRING,      
                        0,  // From API docs, must be zero
                        (LPARAM)ItemText
                       );

    if (
        (itemIdx == LB_ERR) ||
        (itemIdx == LB_ERRSPACE)
       )
        {
        itemIdx = -1;
        }
    else
        {
        if (SendMessage(
                            GetDlgItem(hDlg, ListboxID),      
                            (UINT)LB_SETITEMDATA,      
                            itemIdx,
                            (LPARAM)ItemUserData
                           ) == LB_ERR)
            {
            // Oh well... Back out that one!
            SendMessage(
                            GetDlgItem(hDlg, ListboxID),      
                            (UINT)LB_DELETESTRING,      
                            itemIdx,
                            0
                           );
            }
        }

    return itemIdx;
}

// =========================================================================
// SelectedUserData - If not NULL, will be set to the data associated with 
//                    the currently selected item
// Returns TRUE/FALSE on success/failure
BOOL Combobox_GetItem(
    HWND hDlg,
    UINT ComboboxID,
    void** SelectedUserData
)
{
    BOOL retval = FALSE;
    LRESULT itemIdx;
    LRESULT lResult;
    
    itemIdx = SendMessage(
                        GetDlgItem(hDlg, ComboboxID),      
                        (UINT)CB_GETCURSEL,      
                        0,  // From API docs, must be zero
                        0
                       );

    if (itemIdx != CB_ERR)
        {
        if (SelectedUserData != NULL)
            {
            lResult = SendMessage(
                                GetDlgItem(hDlg, ComboboxID),      
                                (UINT)CB_GETITEMDATA,      
                                itemIdx,
                                0
                               );

            if (lResult != CB_ERR)
                {
                *SelectedUserData = (void*)lResult;
                retval = TRUE;
                }
            }

        }
    
    return retval;
}


// =========================================================================
// Return TRUE/FALSE, depending on whether identified control is a label
// (static)
BOOL IsControl_Static(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    return IsWindow_Static(hWndTmp);
}

BOOL IsWindow_Static(HWND hWnd)
{
    BOOL retval;
    DWORD wndStyle;
    WCHAR className[1024];

    retval = FALSE;

    if (GetClassName(hWnd, className, (sizeof(className) / sizeof(*className))) > 0)
        {
        if (wcsicmp(className, CONTROL_CLASS_STATICW) == 0)
            {
            wndStyle = GetWindowLong(hWnd, GWL_STYLE);
            if (
                (!((wndStyle & SS_BITMAP) == SS_BITMAP)) &&
                (!((wndStyle & SS_ICON) == SS_ICON))
               )
                {
                retval = TRUE;
                }
            }
        }

    return retval;
}

BOOL IsControl_Checkbox(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    return IsWindow_Checkbox(hWndTmp);
}

BOOL IsWindow_Checkbox(HWND hWnd)
{
    BOOL retval;
    DWORD wndStyle;
    WCHAR className[1024];

    retval = FALSE;

    if (GetClassName(hWnd, className, (sizeof(className) / sizeof(*className))) > 0)
        {
        if (wcsicmp(className, CONTROL_CLASS_BUTTONW) == 0)
            {
            wndStyle = GetWindowLong(hWnd, GWL_STYLE);
            if (
                ( 
                 ((wndStyle & BS_CHECKBOX) == BS_CHECKBOX) ||
                 ((wndStyle & BS_AUTOCHECKBOX) == BS_AUTOCHECKBOX)
                ) &&
                (!((wndStyle & BS_GROUPBOX) == BS_GROUPBOX))
               )
                {                
                retval = TRUE;
                }
            }
        }

    return retval;
}

BOOL IsControl_RadioButton(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    return IsWindow_Checkbox(hWndTmp);
}

BOOL IsWindow_RadioButton(HWND hWnd)
{
    BOOL retval;
    DWORD wndStyle;
    WCHAR className[1024];

    retval = FALSE;

    if (GetClassName(hWnd, className, (sizeof(className) / sizeof(*className))) > 0)
        {
        if (wcsicmp(className, CONTROL_CLASS_BUTTONW) == 0)
            {
            wndStyle = GetWindowLong(hWnd, GWL_STYLE);
            if (
                (
                 ((wndStyle & BS_RADIOBUTTON) == BS_RADIOBUTTON) ||
                 ((wndStyle & BS_AUTORADIOBUTTON) == BS_AUTORADIOBUTTON)
                ) &&
                (!((wndStyle & BS_GROUPBOX) == BS_GROUPBOX))
               )
                {                
                retval = TRUE;
                }
            }
        }

    return retval;
}

BOOL IsControl_Edit(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    return IsWindow_Edit(hWndTmp);
}

BOOL IsWindow_Edit(HWND hWnd)
{
    BOOL retval;
    WCHAR className[1024];

    retval = FALSE;

    if (GetClassName(hWnd, className, (sizeof(className) / sizeof(*className))) > 0)
        {
        if (wcsicmp(className, CONTROL_CLASS_EDITW) == 0)
            {
            retval = TRUE;
            }
        }

    return retval;
}


BOOL IsControl_Image(HWND hWnd, int nIDDlgItem)
{
    HWND hWndTmp;

    hWndTmp = GetDlgItem(hWnd, nIDDlgItem); 

    return IsWindow_Edit(hWndTmp);
}

BOOL IsWindow_Image(HWND hWnd)
{
    BOOL retval;
    DWORD wndStyle;
    WCHAR className[1024];

    retval = FALSE;

    if (GetClassName(hWnd, className, (sizeof(className) / sizeof(*className))) > 0)
        {
        if (wcsicmp(className, CONTROL_CLASS_STATICW) == 0)
            {
            wndStyle = GetWindowLong(hWnd, GWL_STYLE);
            if (
                ((wndStyle & SS_BITMAP) == SS_BITMAP) ||
                ((wndStyle & SS_ICON) == SS_ICON)
               )
                {
                retval = TRUE;
                }
            }
        }

    return retval;
}

// =========================================================================
