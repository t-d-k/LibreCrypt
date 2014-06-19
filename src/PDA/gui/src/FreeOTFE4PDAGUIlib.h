// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAGUIlib_H
#define _FreeOTFE4PDAGUIlib_H   1

#include <windef.h>  // Definition of BOOL

// This line should be replaced with #include of std header file with this definition
#define ID_SEPARATOR     0  

BOOL ValidateMountpoint(
    HWND hWnd,
    WCHAR* mountpoint,
    BOOL SkipExistsCheck,
    BOOL silent
);

// Note: It is the *CALLERS* responsibility to free off the string returned
WCHAR* GenerateDefaultMountpoint(HWND hDlg, const WCHAR* VolFilename);

void CopyMenu(HMENU hOrigMenu, HMENU hNewMenu);
HWND SetupMenu_Simple(
    HWND hWnd, 
    int MenuID
);
HWND SetupMenu(
    HWND hWnd, 
    int MenuID, 
    int leftMenuItemCtrlID, 
    WCHAR* rightMenuTitle
);
HWND SetupMenu_NonSoftkey(HWND hWnd, int ID);
HWND SetupMenu_Softkey(
    HWND hWnd, 
    int MenuID, 
    int leftMenuItemCtrlID, 
    WCHAR* rightMenuTitle
);

BOOL ChangeLeftSoftkey(
    HWND hCmdBar, 
    int newCmdID,
    WCHAR* rightMenuTitle
);

void SetupInstructions(
    HINSTANCE hInstance,
    HWND hDlg, 
    int ctrlID, 
    int textID
);


// =========================================================================
// =========================================================================

#endif

