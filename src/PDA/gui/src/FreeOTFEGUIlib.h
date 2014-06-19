// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEGUIlib_H
#define _FreeOTFEGUIlib_H   1

#include <windows.h>

#define IDR_MENU_NULL  -1

#define FREEOTFE_DLG_BORDER  10


void SDUDisplayLastError_Number();
void SDUDisplayLastError_Text();
void SDUDisplayLastError_TextForCode(DWORD error);

WCHAR* GetString(HINSTANCE hInstance, int stringID);

void MsgOutOfMemory(HWND hWnd);
int  MsgPrompt(HWND hWnd, LPCTSTR lpText, UINT uType);
int  MsgPromptInfo(HWND hWnd, LPCTSTR lpText, UINT uType);
int  MsgPromptQuestion(HWND hWnd, LPCTSTR lpText, UINT uType);
int  MsgPromptWarn(HWND hWnd, LPCTSTR lpText, UINT uType);
void MsgInfo(HWND hWnd, LPCTSTR lpText);
void MsgWarn(HWND hWnd, LPCTSTR lpText);
void MsgError(HWND hWnd, LPCTSTR lpText);
void MsgInfoDWORD_AsHex(HWND hWnd, DWORD number);
void MsgWarnDWORD_AsHex(HWND hWnd, DWORD number);
void MsgErrorDWORD_AsHex(HWND hWnd, DWORD number);
void MsgInfoString(HWND hWnd, HINSTANCE hInstance, int stringID);
void MsgWarnString(HWND hWnd, HINSTANCE hInstance, int stringID);
void MsgErrorString(HWND hWnd, HINSTANCE hInstance, int stringID);

// Set static text, resizing the control as appropriate
void SetStaticText_HWND(HWND hWnd, LPCTSTR lpText);
void SetStaticText(HWND hWnd, int nIDDlgItem, LPCTSTR lpText);

// Append static text, resizing the control as appropriate
void AppendStaticText(HWND hWnd, int ctrlID, LPCTSTR lpText);
void AppendStaticText_Simple(HWND hWnd, int ctrlID, LPCTSTR lpText);
void AppendStaticText_ResizeToText(HWND hWnd, int ctrlID, LPCTSTR lpText);
void AppendText(HWND hWnd, int ctrlID, LPCTSTR lpText, BOOL resizeCtrl);

// Resize static control to accomodate text
void ResizeStaticToText_ChildText(HWND hWnd, int nIDDlgItem, LPCTSTR lpText);
void ResizeStaticToText_Child(HWND hWnd, int nIDDlgItem);
void ResizeStaticToText_HWNDText(HWND hWnd, LPCTSTR lpText);
void ResizeStaticToText_HWND(HWND hWnd);


void CopyMenu(HMENU hOrigMenu, HMENU hNewMenu);

BOOL MenuButtonAdd(
                   HWND hWndMenu, 
                   UINT bitmapResource, 
                   UINT bitmapIdx, 
                   UINT cmdID
                  );

void MenuButtonEnable(
                   HWND hWndMenu, 
                   UINT cmdID,
                   BOOL enable
                  );

void MenuItemEnableWnd(
                   HWND hWndMenu, 
                   UINT cmdID,
                   BOOL enable
                  );

void MenuItemEnableMenu(
                   HMENU hMenu, 
                   UINT cmdID,
                   BOOL enable
                  );

void MenuItemRemoveWnd(
                   HWND hWndMenu, 
                   UINT cmdID
                  );
void MenuItemRemoveMenu(
                   HMENU hMenu, 
                   UINT cmdID
                  );

void AdjustSpinControl(
                    HWND hDlg,
                    int nIDDlgItem,
                    int iMin,
                    int iMax,
                    int iDelta
                   );

char* GetPassword(
    HWND hDlg, 
    UINT ctrlID
);

void
SetDlgItemString(
    HINSTANCE hInstance,
    HWND hWnd,
    int ctrlID,
    int stringID    
);
void
SetDlgItemString_WithResize(
    HINSTANCE hInstance,
    HWND hWnd,
    int ctrlID,
    int stringID
);
void
SetControlVisible(
    HWND ParentWindow,
    int CtrlID,
    BOOL Visible
);
BOOL
IsControlVisible(
    HWND ParentWindow,
    int CtrlID
);
void
SetControlEnabled(
    HWND ParentWindow,
    int CtrlID,
    BOOL Enabled
);
BOOL
IsControlEnabled(
    HWND ParentWindow,
    int CtrlID
);
void
SetControlReadonly(
    HWND ParentWindow,
    int CtrlID,
    BOOL Readonly
);

void SizeControlMaxDepth(
    HWND hDlg,
    int ctrlID
);
void SizeControlMaxWidth(
    HWND hDlg,
    int ctrlID
);
void SizeControlMaxWidthBorder(
    HWND hDlg,
    int ctrlID,
    int Border
);
void SizeChildWindowToParent(
    HWND hChildWnd
);
void SizeControlToParent(
    HWND hDlg,
    int ctrlID
);
void SizeWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow
);
void SizeWindowToOverlapWindow(
    HWND BaseWindow,
    HWND ResizeWindow
);
void CenterWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow
);
void CenterChildWindowParent(
    HWND hChildWnd
);
void CenterControlToParent(
    HWND hDlg,
    int ctrlID
);
#define BS_NONE  0
void AlignWindowToWindowClient(
    HWND BaseWindow,
    HWND ResizeWindow,
    int Horizontal,
    int Vertical
);

void AlignSetWindowsToWindowClient(
    HWND BaseWindow,
    int ControlCount,
    HWND* ResizeWindow,  // Pointer to array
    int Horizontal,
    int Vertical
);

// Note: All of the child windows *must* have the same parent
void AlignSetChildWindowsToParent(
    int ControlCount,
    HWND* hChildWnd,  // Pointer to array
    int Horizontal,
    int Vertical
);

// Note: All of the controls *must* have the same parent
void AlignSetControlsToParent(
    HWND hWndParent,
    int ControlCount,
    int* ctrlID,  // Pointer to array
    int Horizontal,
    int Vertical
);

void CommandBar_ItemEnable(
    HWND hCmdBar,
    UINT cmdID,
    BOOL enabled
);

void DisplayURL(WCHAR* url);

int Combobox_AddItem(
    HWND hDlg,
    UINT ComboboxID,
    const WCHAR* ItemText,
    void* ItemUserData
);
BOOL SetComboBoxSelected(HWND hDlg, const int ctrlID, const WCHAR* selectItem);
BOOL SetComboBoxSelectedIdx(HWND hDlg, int comboboxCtrlID, int selectItemIdx);

int Listbox_AddItem(
    HWND hDlg,
    UINT ListboxID,
    const WCHAR* ItemText,
    void* ItemUserData
);

BOOL Combobox_GetItem(
    HWND hDlg,
    UINT ComboboxID,
    void** SelectedUserData
);

// Paste any text from the clipboard to the specified control
BOOL PasteTextFromClipboard(HWND hWnd, int nIDDlgItem);

// Return TRUE/FALSE, depending on whether identified control is a label
// (static)
BOOL IsControl_Static(HWND hWnd, int nIDDlgItem);
BOOL IsControl_Checkbox(HWND hWnd, int nIDDlgItem);
BOOL IsControl_RadioButton(HWND hWnd, int nIDDlgItem);
BOOL IsControl_Edit(HWND hWnd, int nIDDlgItem);
BOOL IsControl_Image(HWND hWnd, int nIDDlgItem);

BOOL IsWindow_Static(HWND hWnd);
BOOL IsWindow_Checkbox(HWND hWnd);
BOOL IsWindow_RadioButton(HWND hWnd);
BOOL IsWindow_Edit(HWND hWnd);
BOOL IsWindow_Image(HWND hWnd);

// As SetDlgItemText, but resize the control to match the text size
WINUSERAPI
BOOL
WINAPI
SetDlgItemText_WithResizeW(
    HWND hDlg,
    int nIDDlgItem,
    LPCWSTR lpString);
#ifdef UNICODE
#define SetDlgItemText_WithResize  SetDlgItemText_WithResizeW
#else
#define SetDlgItemText_WithResize  SetDlgItemText_WithResizeA
#endif // !UNICODE

// =========================================================================
// =========================================================================

#endif

