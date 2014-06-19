// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//



#include "main.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFE4PDAAPI.h"
#include "SDUGeneral.h"
#include "SDUi18n.h"

#include "resource.h"

#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib

// Control IDs of the left/right WM5 menu buttons
#define TB_MENU_LEFT   101
#define TB_MENU_RIGHT  102

#define TB_BUTTON_IDX_LEFT   0  // 0 = lefthand button
#define TB_BUTTON_IDX_RIGHT  1  // 1 = righthand button


// Not clear where this is defined?!
#ifdef WINCE        
#define INVALID_FILE_ATTRIBUTES 0xFFFFFFFF
#endif


// =========================================================================
// Returns TRUE if the mountpoint specified is a valid mountpoint which may
// be used
BOOL ValidateMountpoint(
    HWND hWnd,
    WCHAR* mountpoint,
    BOOL SkipExistsCheck,
    BOOL silent
)
{
    BOOL retval = FALSE;

    if (wcslen(mountpoint) == 0)
        {
        if (!(silent))
            {
            MsgError(hWnd, _("Please specify a mountpoint"));
            }
        }
    else if (wcscmp(RESERVED_MOUNTPOINT, mountpoint) == 0)
        {
        if (!(silent))
            {
            MsgError(hWnd, _("The mountpoint specified is reserved; please enter a different mountpoint"));
            }
        }
    else if (wcspbrk(mountpoint, RESERVED_MOUNTPOINT_CHARS) != NULL)
        {
        if (!(silent))
            {
            MsgError(hWnd, _("The mountpoint specified includes a invalid character; please enter a different mountpoint"));
            }
        }
    else if (
             (!(SkipExistsCheck)) &&
             (GetFileAttributes(mountpoint) != INVALID_FILE_ATTRIBUTES)
            )
        {
        if (!(silent))
            {
            MsgError(hWnd, _("The mountpoint specified already exists; please enter a different mountpoint"));
            }
        }
    else
        {
        // Everything OK
        retval = TRUE;
        }

    return retval;
}


// =========================================================================
// Note: It is the *CALLERS* responsibility to free off the string returned
WCHAR* GenerateDefaultMountpoint(HWND hDlg, const WCHAR* VolFilename)
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
    WCHAR* retval;
    int filenameChars;

    retval = NULL;

    // Identify default mountpoint to use...    
    memset(initBaseMountpoint, 0, sizeof(initBaseMountpoint));
    if (
        (G_Options->MntPntUseVolFilename) &&
        (VolFilename != NULL)
       )
        {
        SDUParsePath(
                     VolFilename, 
                     &uncMachine,
                     &drive,
                     &path,
                     &filename,
                     &fileExtension
                    );
        if (filename != NULL)
            {
            wcscpy(initBaseMountpoint, filename);
            }
        if (fileExtension != NULL)
            {
            if (wcsicmp(fileExtension, VOL_FILE_EXTN) == 0)
                {
                // -1 because we want to replace the "."
                filenameChars = wcslen(filename) - wcslen(fileExtension) - 1;
                initBaseMountpoint[filenameChars] = (WCHAR)NULL;
                }
            }
        }

    // Fallback...
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
            // +1 for terminating NULL
            retval = FREEOTFE_MEMCALLOC(wcslen(testMountpoint) + 1, sizeof(WCHAR));
            if (retval != NULL)
                {
                wcscpy(retval, testMountpoint);
                }
            break;
            }
        }

    return retval;
}


// =========================================================================
// Because we can't load a main menu with separators, any menuitem with
// an ID of ID_SEPARATOR will be replaced with a separator
void CopyMenu(HMENU hOrigMenu, HMENU hNewMenu)
{
    MENUITEMINFO menuItemInfo;
    int i;
    WCHAR buffer[1024];  // Just a value large enough to store menu text
    UINT newID;
    UINT newItemFlags;
    HMENU hNewPopup;

    i = 0;
    while (TRUE)
        {
        ZeroMemory(&menuItemInfo, sizeof(menuItemInfo));
        menuItemInfo.cbSize = sizeof(menuItemInfo);
        menuItemInfo.fMask  = (
                      MIIM_CHECKMARKS |
                      MIIM_DATA |
                      MIIM_ID |
                      MIIM_STATE |
                      MIIM_SUBMENU |
                      MIIM_TYPE
                     );
        menuItemInfo.dwTypeData = buffer;
        menuItemInfo.cch = sizeof(buffer);

        if (!(GetMenuItemInfo(hOrigMenu, i, TRUE, &menuItemInfo)))
            {
            break;
            }

        // Recursively copy submenus:
        newItemFlags = MF_BYPOSITION;
        newID = menuItemInfo.wID;
        if (
            (menuItemInfo.fType && MFT_SEPARATOR) ||
            (newID == ID_SEPARATOR)
           )
            {
            newItemFlags |= MF_SEPARATOR;
            }

        if (menuItemInfo.hSubMenu)
            {
            newItemFlags |= MF_POPUP;
            hNewPopup = CreatePopupMenu();
            CopyMenu(menuItemInfo.hSubMenu, hNewPopup);
            menuItemInfo.hSubMenu = hNewPopup;
            newID = (int)hNewPopup;
            }

        /*
        MF_STRING
        MF_UNCHECKED 
        MF_MENUBREAK 
        MF_GRAYED 
        MF_ENABLED 
        MF_CHECKED
        MF_MENUBARBREAK 
        MF_SEPARATOR 
        */

        // Add the copy...
        InsertMenu(hNewMenu, i, newItemFlags, newID, buffer);

        // Setup this for enabled and checkmarks...
        SetMenuItemInfo(hNewMenu, i, TRUE, &menuItemInfo);

        i++;
        }

} 


// =========================================================================
HWND SetupMenu_Simple(
    HWND hWnd, 
    int MenuID
)
{
    return SetupMenu_NonSoftkey(hWnd, MenuID);
}


// =========================================================================
HWND SetupMenu(
    HWND hWnd, 
    int MenuID, 
    int leftMenuItemCtrlID, 
    WCHAR* rightMenuTitle
)
{
    HWND retval;

    if (G_Options->SoftkeyMenus)
        {
        retval = SetupMenu_Softkey(
                                   hWnd, 
                                   MenuID, 
                                   leftMenuItemCtrlID, 
                                   rightMenuTitle
                                  );
        }
    else
        {
        retval = SetupMenu_NonSoftkey(hWnd, MenuID);
        }

    return retval;
}


// =========================================================================
// This function *requires* dummy menu resource IDR_MENU_DUMMY_LEFTRIGHT with
// two buttons
HWND SetupMenu_Softkey(
    HWND hWnd, 
    int MenuID, 
    int leftMenuItemCtrlID, 
    WCHAR* rightMenuTitle
)
{
    HWND retval;
    MENUITEMINFO menuInfoLeft;
    HMENU hMenuRight;
    TBBUTTON tbb[2];
    HMENU hMenuLoaded;
    WCHAR buffer[1024];  // Just a value large enough to store menu text
    WCHAR defaultRightTitle[1024];  // Just a value large enough to store menu text
    WCHAR* useRightTitle;

    retval = SetupMenu_NonSoftkey(hWnd, IDR_MENU_DUMMY_LEFTRIGHT);    

    useRightTitle = rightMenuTitle;
    if (useRightTitle == NULL)
        {
        wcscpy(defaultRightTitle, _("Menu"));
        useRightTitle = defaultRightTitle;
        }

    if (retval != NULL) 
        {
        // Ditch the dummy buttons...
        SendMessage((HWND)retval, TB_DELETEBUTTON, 0, 0);
        SendMessage((HWND)retval, TB_DELETEBUTTON, 0, 0);

        hMenuLoaded = LoadMenu(G_hInstance, MAKEINTRESOURCE(MenuID)); 

        if (hMenuLoaded != NULL)
            {
            menuInfoLeft.cbSize = sizeof(menuInfoLeft);
            menuInfoLeft.fMask = (MIIM_TYPE | MIIM_ID);
            menuInfoLeft.cch = sizeof(buffer);
            menuInfoLeft.dwTypeData = buffer;

            // Identify menuitem for left button text
            if (GetMenuItemInfo(
                                  hMenuLoaded, 
                                  leftMenuItemCtrlID, 
                                  FALSE,
                                  &menuInfoLeft
                                 ))
                {
                // Lose any "..." following the command
                wcstok(buffer, TEXT("."));
                }
            else
                {
                wsprintf(buffer, TEXT("???"));
                }

            hMenuRight = CreatePopupMenu(); 
            CopyMenu(hMenuLoaded, hMenuRight);
            DestroyMenu(hMenuLoaded);

            memset(&tbb, 0, sizeof(tbb));

            // Setup left menubutton...
            tbb[0].idCommand = leftMenuItemCtrlID;
            tbb[0].iBitmap   = I_IMAGENONE;
            tbb[0].fsState   = TBSTATE_ENABLED;
            tbb[0].fsStyle   = (
                                TBSTYLE_BUTTON   |
                                TBSTYLE_AUTOSIZE |
                                0x80  // magic flag 
                               );
            tbb[0].dwData    = (DWORD)0;
            tbb[0].iString   = (int)buffer; 

            // Setup right menubutton...
            tbb[1].idCommand = TB_MENU_RIGHT;
            tbb[1].iBitmap   = I_IMAGENONE;
            tbb[1].fsState   = TBSTATE_ENABLED;
            tbb[1].fsStyle   = (
                                TBSTYLE_DROPDOWN |
                                TBSTYLE_BUTTON   |
                                TBSTYLE_AUTOSIZE |
                                0x80  // magic flag
                               );
            tbb[1].dwData    = (DWORD)hMenuRight;
            tbb[1].iString   = (int)useRightTitle; 

            SendMessage((HWND)retval, TB_ADDBUTTONSW, 2, (LPARAM)(&tbb));

            DrawMenuBar(retval); 
            }
        }

    return retval;

}


// =========================================================================
// Setup menu
// Returns: Menu HWND, or NULL on failure
HWND SetupMenu_NonSoftkey(HWND hWnd, int menuID)
{
    HWND retval = NULL;
    SHMENUBARINFO mbi;

    // Setup menu...
    memset(&mbi, 0, sizeof(mbi));

    mbi.cbSize     = sizeof(mbi);
    mbi.hwndParent = hWnd;

    if (menuID == IDR_MENU_NULL)
        {
        mbi.dwFlags = SHCMBF_EMPTYBAR;
        mbi.nToolBarId = 0;
        }
    else
        {
        // Needed if mbi.nToolBarId is a menu ID
        mbi.dwFlags = SHCMBF_HMENU;
        mbi.nToolBarId = menuID;
        }
                                       
    mbi.hInstRes   = G_hInstance;
    mbi.nBmpId     = 0;
    mbi.cBmpImages = 0; 

    if (!(SHCreateMenuBar(&mbi)))
        {
        MsgError(hWnd, TEXT("SHCreateMenuBar Failed"));
        }
    else
        {
        retval = mbi.hwndMB;        
        }

    return retval;
}


// =========================================================================
// Change the *left* softkey to the specified command/button text
// rightMenuTitle - If this is set to NULL, this function will try to use
//                  the text of newCmdID from the *right* softkey's menu
BOOL ChangeLeftSoftkey(
    HWND hCmdBar, 
    int newCmdID,
    WCHAR* rightMenuTitle
)
{
    BOOL allOK;
    TBBUTTONINFO buttonInfoLeft;
    TBBUTTON tbb;
    WCHAR existingMenuText[1024];  // Just a value large enough to store menu text
    WCHAR defaultRightTitle[1024];  // Just a value large enough to store menu text
    WCHAR* useRightTitle;
    UINT oldMenuCmdID;
    MENUITEMINFO existingMenuItemInfo;
    BOOL enableNewCtrl;

    // This function does nothing if we're not using softkey menus
    if (!(G_Options->SoftkeyMenus))
        {
        return TRUE;
        }

    allOK = TRUE;

    // Get the lefthand button
    if (allOK)
        {
        memset(&tbb, 0, sizeof(tbb));

        if (!(SendMessage(
                    hCmdBar,
                    TB_GETBUTTON,
                    (WPARAM)TB_BUTTON_IDX_LEFT,
                    (LPARAM)(&tbb)
                   )))
            {
            allOK = FALSE;
            }
        }


    // Locate the old button's command ID; needed to identify the button later
    if (allOK)
        {
        oldMenuCmdID = tbb.idCommand;

        memset(&buttonInfoLeft, 0, sizeof(buttonInfoLeft));

        buttonInfoLeft.cbSize = sizeof(buttonInfoLeft);
        buttonInfoLeft.dwMask = (
                               TBIF_COMMAND |
                               TBIF_STATE   | 
                               TBIF_LPARAM
                              );

        if (!(SendMessage(
                    hCmdBar,
                    TB_GETBUTTONINFO,
                    (WPARAM)oldMenuCmdID,
                    (LPARAM)(&buttonInfoLeft)
                   )))
            {
            allOK = FALSE;
            }
        }


    // Determine what text to give the replacement command item
    enableNewCtrl = TRUE;
    if (allOK)
        {
        // Use user supplied title...
        useRightTitle = rightMenuTitle;

        // Fallback to new command's title...
        if (useRightTitle == NULL)
            {
            // Attempt to get our righthand menu            
            memset(&tbb, 0, sizeof(tbb));
            if (SendMessage(
                        hCmdBar,
                        TB_GETBUTTON,
                        (WPARAM)TB_BUTTON_IDX_RIGHT,
                        (LPARAM)(&tbb)
                       ))
                {
                // Righthand menu obtained; get text of new menuitem
                existingMenuItemInfo.cbSize = sizeof(existingMenuItemInfo);
                existingMenuItemInfo.fMask = (
                                              MIIM_DATA    |
                                              MIIM_STATE   |
                                              MIIM_ID      |
                                              MIIM_SUBMENU |
                                              MIIM_TYPE
                                             );
                existingMenuItemInfo.dwTypeData = existingMenuText;
                existingMenuItemInfo.cch = sizeof(existingMenuText);
                
                if (GetMenuItemInfo(
                                    (HMENU)(tbb.dwData),
                                    newCmdID,
                                    FALSE,
                                    &existingMenuItemInfo
                                   ))
                    {
                    // Lose any "..." following the command
                    wcstok(existingMenuText, TEXT("."));

                    useRightTitle = existingMenuText;

                    if (existingMenuItemInfo.fState & MF_GRAYED)
                        {
                        enableNewCtrl = FALSE;
                        }
                    }

                }

            }

        // Fallback to "???"
        if (useRightTitle == NULL)
            {
            wcscpy(defaultRightTitle, TEXT("???"));
            useRightTitle = defaultRightTitle;
            }
        }


    // Update the button with the new text/command ID
    if (allOK) 
        {
        buttonInfoLeft.pszText = useRightTitle;
        buttonInfoLeft.cbSize = sizeof(buttonInfoLeft);
        buttonInfoLeft.idCommand = newCmdID;
        buttonInfoLeft.dwMask = (
                               TBIF_COMMAND |
                               TBIF_TEXT    |
                               TBIF_STATE   | 
                               TBIF_LPARAM
                              );

        if (!(SendMessage(
                          hCmdBar,
                          TB_SETBUTTONINFO,
                          (WPARAM)oldMenuCmdID,
                          (LPARAM)(&buttonInfoLeft)
                         )))
            {
            allOK = FALSE;
            }

        CommandBar_ItemEnable(hCmdBar, newCmdID, enableNewCtrl);
        }

    return allOK;
}


// =========================================================================
// This is a replacement window proc to filter out WM_CHAR to make an edit
// readonly, without making it readonly as this would result in the
// background being switched to a "disbled" background
LRESULT APIENTRY ReadonlyEditControlProc(
    HWND hwnd, 
    UINT uMsg, 
    WPARAM wParam, 
    LPARAM lParam
) 
{ 
    WNDPROC wpOrigEditProc;

    // Increment the key code for a WM_CHAR message
    if (uMsg == WM_CHAR) 
       {
       // Processed! (Honest!)
       return 0;
       }
/*
    if (uMsg == WM_UNICHAR) 
       {
       // Processed! (Honest!)
       return 0;
       }
*/

    wpOrigEditProc = (WNDPROC)GetWindowLong(hwnd, GWL_USERDATA);

    if (uMsg == WM_DESTROY) 
       {
       // Processed! (Honest!)
       (WNDPROC)SetWindowLong(hwnd, GWL_WNDPROC, (LONG)wpOrigEditProc);
       }

    if (wpOrigEditProc != NULL)
       {
       // Pass all messages on to the original wndproc
       return CallWindowProc(wpOrigEditProc, hwnd, uMsg, wParam, lParam); 
       }

    return 0;
} 

// Setup instructions control
void SetupInstructions(
    HINSTANCE hInstance,
    HWND hDlg, 
    int ctrlID, 
    int textID
)
{
    WCHAR msg[10240];  // lplp
    HWND hWndTmp;
    SCROLLINFO scrollInfo;
    WNDPROC wpOrigEditProc;

    hWndTmp = GetDlgItem(hDlg, ctrlID);
    if (hWndTmp == NULL)
        {
        return;
        }

    // Assume scrollbar needed; it that way when the text is set and the 
    // scrollbar's details read, they should be correct, bearing in mind the 
    // scrollbar will take up space that would otherwise be used for text
    SDUSetWndStyle(hWndTmp, TRUE, WS_VSCROLL);

    SDUSetWndStyle(hWndTmp, TRUE, ES_MULTILINE);
    SDUSetWndStyle(hWndTmp, TRUE, ES_AUTOVSCROLL);
    SDUSetWndStyle(hWndTmp, TRUE, ES_WANTRETURN);

    // Set the dialog to the string passed in, translating as needed
    SetDlgItemString(
                     G_hInstance,
                     hDlg,
                     ctrlID,
                     textID
                    );

    // Retrieve instructions text, and replace 0x10 chars with 0x10, 
    // 0x13 - required by EDIT (TEXTEDIT) controls
    GetDlgItemTextW(
        hDlg,
        ctrlID,
        msg,
        (sizeof(msg) / sizeof(*msg))
        );
    SDUStringReplaceW(msg, TEXT("\n"), TEXT("\r\n"));
    SetDlgItemTextW(
        hDlg,
        ctrlID,
        msg
        );

    
/*
    SDUSetWndStyle(hWndTmp, TRUE, WS_EX_STATICEDGE);

    //SDUSetWndStyle(hWndTmp, TRUE, WS_VSCROLL);
    scrollInfo.cbSize = sizeof(scrollInfo);
    scrollInfo.fMask = 0; // i.e. DO NOT SET SIF_DISABLENOSCROLL
    scrollInfo.fMask = SIF_DISABLENOSCROLL;
    SetScrollInfo(hWndTmp, SB_VERT, &scrollInfo, TRUE);
*/

    SizeControlMaxWidthBorder(
                              hDlg,
                              ctrlID,
                              FREEOTFE_DLG_BORDER
                             );
    SizeControlMaxDepth(hDlg, ctrlID);

    // Get all the vertial scroll bar information
    memset(&scrollInfo, 0, sizeof(scrollInfo));
    scrollInfo.cbSize = sizeof(scrollInfo);
    scrollInfo.fMask  = SIF_ALL;
    GetScrollInfo(hWndTmp, SB_VERT, &scrollInfo);
    // Save the position for comparison later on
    SizeControlMaxDepth(hDlg, ctrlID);
    SDUSetWndStyle(hWndTmp, (((UINT)scrollInfo.nMax) >= scrollInfo.nPage), WS_VSCROLL);

    // In case the scrollbar was turned on/off, 
    SetWindowPos(
                 hWndTmp, 
                 NULL, 
                 0, 
                 0, 
                 0, 
                 0, 
                 (
                  SWP_DRAWFRAME | 
                  SWP_FRAMECHANGED | 
                  SWP_NOZORDER | 
                  SWP_NOMOVE | 
                  SWP_NOSIZE
                 )
                );

    // Make readonly by intercepting WM_CHAR
    // Don't set the *control* to be readonly, as that results in wrong colour
    // background
    wpOrigEditProc = (WNDPROC)GetWindowLong(hWndTmp, GWL_WNDPROC);
    if (wpOrigEditProc != ReadonlyEditControlProc)
        {
        (WNDPROC)SetWindowLong(hWndTmp, GWL_WNDPROC, (LONG)ReadonlyEditControlProc);
        SetWindowLong(hWndTmp, GWL_USERDATA, (LONG)wpOrigEditProc);
        //lplp - SetWindowLong is WINDOWS MOBILE 5 AND LATER ONLY - NOT WM2003?
        }

}


// =========================================================================
// =========================================================================

