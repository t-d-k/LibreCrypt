// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// Note: List sorting not implemented; see parts marked with "[SORTRELATED]"


#include <windows.h>
#include <Windowsx.h>  // Needed for GET_X_LPARAM, GET_Y_LPARAM macros
#include <Shellapi.h>  // Needed for SHGetFileInfo(...)
#include <commctrl.h>
#include <stdlib.h> // Needed for itow(...)
#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib

#include "SDUGeneral.h"  // Required for SDUParsePath

#include "main.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFEGUIlib.h"
#include "FreeOTFE4PDAGUIlib.h"
#include "FreeOTFE4PDAlib.h"
#include "dlgAbout.h"
#include "dlgNewVolWizard.h"
#include "dlgChpassKeyfileWizard.h"
#include "dlgDumpCDBWizard.h"
#include "dlgDumpLUKSWizard.h"
#include "dlgBackupRestore.h"
#include "dlgMount.h"
#include "dlgMountLUKS.h"
#include "dlgProperties.h"
#include "dlgOptions.h"
#include "wndMain.h"
#include "DriverInterface.h"
#include "FreeOTFEStats.h"
#include "FreeOTFE4PDAOptions.h"
#include "SDUi18n.h"
#include "SDUi18n_GUI.h"

#include "resource.h"

#define WM_DEVICECHANGE 0x219

// Control ID of the main list view
#define IDC_MAIN_LISTVIEW 100


// Column indexes and widths
#define COL_IDX_MOUNTPOINT  0
#define COL_IDX_FILENAME    1
#define COL_WIDTH_MOUNTPOINT  100
#define COL_WIDTH_FILENAME    100

// When displaying "large icons", the indentation of the icon in pixels
// from the left
#define INDENT_ICON  2
// When displaying "large icons", the indentation of the text from the icon
#define INDENT_TEXT  5
// When displaying "large icons", the size of the icons used (fallback values
// if the size cannot be autodetermined)
#define ICON_FALLBACK_SIZE_X  32
#define ICON_FALLBACK_SIZE_Y  32

// Local to this file...
HMENU G_hMenuContext;
HWND G_hWndMenuBar;
HWND G_hWndListView;

// =========================================================================
// Forward declarations...
// [SORTRELATED]
int CALLBACK CompareFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort);  

void wndMain_RefreshList(HWND hWnd);
void wndMain_SetupMenuBar(HWND hWnd);
HWND wndMain_CreateListDisplay(HWND hWnd);
void wndMain_SetupIconSize();


// =========================================================================
// Resize the listview to the maximum it can be displayed
void wndMain_SizeListView()
{
    RECT rcClient;
        
    if (G_hWndListView != NULL)
        {
        // Get size of display area...
        GetClientRect(GetParent(G_hWndListView), &rcClient);

        // Resize...
        SetWindowPos(
                     G_hWndListView, 
                     NULL, 
                     0, 
                     0, 
                     rcClient.right,
                     rcClient.bottom, 
                     SWP_NOZORDER
                    );
        }

}


// =========================================================================
// Purge down any existing, create and size the listview
void wndMain_SetupListDisplay(HWND hWnd)
{
    if (G_hWndListView != NULL)
        {
        DestroyWindow(G_hWndListView);
        }

    G_hWndListView = wndMain_CreateListDisplay(hWnd);
    if(G_hWndListView == NULL)
        {
        MsgError(hWnd, TEXT("Could not create list display"));
        }
    else
        {
        wndMain_SizeListView();
        wndMain_SetupIconSize();
        wndMain_RefreshList(hWnd);
        }

}


// =========================================================================
// Create new listview
// Returns: The handle to the listview created
HWND wndMain_CreateListDisplay(HWND hWnd)
{
    HWND retval;
    INITCOMMONCONTROLSEX icex;
    DWORD dwStyle;
    
    // Ensure that the common control DLL is loaded. 
    icex.dwSize = sizeof(icex);
    icex.dwICC = ICC_LISTVIEW_CLASSES;
    InitCommonControlsEx(&icex); 
    
    dwStyle = 0;
    dwStyle = (
               // [SORTRELATED]
               // Note: These don't work with LVS_OWNERDRAW
               // Note: These two don't work with WinCE versions
               //       up to v3.0! See MS KB article Q191295 for
               //       known MS bug
               // LVS_SORTDESCENDING | 
               // LVS_SORTASCENDING |
               WS_TABSTOP | 
               WS_CHILD | 
               WS_BORDER | 
               WS_VISIBLE | 
               LVS_AUTOARRANGE | 
               LVS_SHAREIMAGELISTS | // Vital! Shags the system
                                     // imagelist if we omit this!
                                     // (When the imagelist is 
                                     // destroyed, it destroys the
                                     // system imagelist if this
                                     // isn't set - a *really*
                                     // silly thing to do as it
                                     // makes all the system icons                                                 
                                     // disappear!)
               LVS_REPORT |
               LVS_EX_FULLROWSELECT  | // Can only be used with
                                       // LVS_REPORT
               LVS_SINGLESEL | // Only allow one item to be
                               // selected at once
               LVS_ICON 
               // LVS_SMALLICON 
              );

    if (G_Options->LargeIcons)
        {
        dwStyle |= LVS_OWNERDRAWFIXED;
        dwStyle |= LVS_NOCOLUMNHEADER;
        }

    retval = CreateWindow(
                          WC_LISTVIEW, 
                          L"XYZ",  // Junk string required in order for the 
                                   // control to be displayed
                          dwStyle,
                          0, // Control will be repositioned later
                          0, // Control will be repositioned later
                          10, // Control will be resized later
                          10, // Control will be resized later
                          hWnd, 
                          (HMENU)IDC_MAIN_LISTVIEW, 
                          G_hInstance, 
                          NULL
                         ); 
    
    if (retval != NULL)
        {
        LVCOLUMN colDetails; 

        // Enable full row select
        ListView_SetExtendedListViewStyle(
                                          retval, 
                                          (
                                           LVS_EX_FULLROWSELECT | 
                                           LVS_ICON 
                                           // LVS_SMALLICON 
                                          )
                                         );

        colDetails.mask = (
                           LVCF_FMT | 
                           LVCF_TEXT | 
                           LVCF_WIDTH |
                           LVCF_SUBITEM
                          );
        colDetails.fmt = LVCFMT_LEFT;
        colDetails.cx = COL_WIDTH_MOUNTPOINT; // Column width; reset later
        colDetails.pszText = (WCHAR*)_("Mountpoint"); 
        colDetails.cchTextMax = wcslen(colDetails.pszText);
        colDetails.iSubItem = COL_IDX_MOUNTPOINT;
        ListView_InsertColumn(
                              retval,
                              COL_IDX_MOUNTPOINT,
                              &colDetails
                             );

        colDetails.mask = (
                           LVCF_FMT | 
                           LVCF_TEXT | 
                           LVCF_WIDTH |
                           LVCF_SUBITEM 
                          );
        colDetails.fmt = LVCFMT_LEFT;
        colDetails.cx = COL_WIDTH_FILENAME; // Column width; reset later
        colDetails.pszText = (WCHAR*)_("Volume"); 
        colDetails.cchTextMax = wcslen(colDetails.pszText);
        colDetails.iSubItem = COL_IDX_FILENAME; 
        ListView_InsertColumn(
                              retval,
                              COL_IDX_FILENAME,
                              &colDetails
                             );

        }

    return retval;
}


// =========================================================================
void wndMain_SetupMenuBar(HWND hWnd)
{
    if (G_hWndMenuBar != NULL)
        {
        DestroyWindow(G_hWndMenuBar);
        G_hWndMenuBar = NULL;
        }

#ifdef FREEOTFE_STATS
    G_hWndMenuBar = SetupMenu_Simple(hWnd, IDR_MENU_MAIN_DEBUG);
#else
#ifdef DEBUG
    G_hWndMenuBar = SetupMenu_Simple(hWnd, IDR_MENU_MAIN_DEBUG);
#else
    // If using softkey menus, the main menu has "Exit" on the "Menu" menu;
    // for the older menustyle (non-softkey menus) it's under "File"
    if (G_Options->SoftkeyMenus)
        {
        G_hWndMenuBar = SetupMenu(
                                  hWnd, 
                                  IDR_MENU_MAIN_SOFTKEY, 
                                  ID_FILE_MOUNT, 
                                  NULL
                                 );
        }
    else
        {
        G_hWndMenuBar = SetupMenu(
                                  hWnd, 
                                  IDR_MENU_MAIN,
                                  ID_FILE_MOUNT, 
                                  NULL
                                 );
        }

    // Add buttons with images
    if (!(G_Options->SoftkeyMenus))
        {
        // Button: New
        MenuButtonAdd(
                      G_hWndMenuBar, 
                      IDR_TOOLBAR_MAIN, 
                      TOOLBAR_BITMAP_NEW, 
                      ID_FILE_NEWVOL
                     );
        // Button: Mount
        MenuButtonAdd(
                      G_hWndMenuBar, 
                      IDR_TOOLBAR_MAIN, 
                      TOOLBAR_BITMAP_MOUNT, 
                      ID_FILE_MOUNT
                     );
        // Button: Dismount
        MenuButtonAdd(
                      G_hWndMenuBar, 
                      IDR_TOOLBAR_MAIN, 
                      TOOLBAR_BITMAP_DISMOUNT, 
                      ID_FILE_DISMOUNT
                     );
        // Button: Dismount all
        MenuButtonAdd(
                      G_hWndMenuBar, 
                      IDR_TOOLBAR_MAIN, 
                      TOOLBAR_BITMAP_DISMOUNT_ALL, 
                      ID_FILE_DISMOUNTALL
                     );
        }

#endif
#endif

    SDUi18n_TranslateCommandBar(G_hWndMenuBar);

    EnableDisableControls();
}


// =========================================================================
LRESULT CALLBACK wndMain_HandleMsg_WM_CREATE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_CREATE\n")));

    // Setup context menu...
    G_hMenuContext = LoadMenu(G_hInstance, MAKEINTRESOURCE(IDR_MENU_CONTEXT));
    
    // Setup menu/toolbar...
    G_hWndMenuBar = NULL;
    wndMain_SetupMenuBar(hWnd);

    // Setup listbox...  
    G_hWndListView = NULL;
    wndMain_SetupListDisplay(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_CREATE\n")));
    return 0;
}


// =========================================================================
// Setup listview (column widths, etc)
// Note that the icons to use are setup as part of the wndMain_RefreshList(...)
void wndMain_SetupIconSize()
{
    RECT rcClient;
    int colWidthMountpoint;
    int colWidthFilename;

    SDUSetWndStyle(
                   G_hWndListView, 
                   (G_Options->LargeIcons),
                   LVS_OWNERDRAWFIXED
                  );
    SDUSetWndStyle(
                   G_hWndListView, 
                   (G_Options->LargeIcons),
                   LVS_NOCOLUMNHEADER
                  );

    if (G_Options->LargeIcons)
        {
        // Get size of display area...
        GetClientRect(G_hWndListView, &rcClient);

        // Large icons view only displays a single "column", which we
        // draw. We set the width of the "filename" column to 0; if we
        // didn't, the draw area would increase by this amount

        // -2 to give a bit of a border on the right, and also to prevent the
        // horizontal scrollbar from appearing automatically(!) when the list
        // display is recreated after the Options dialog is displayed
        colWidthMountpoint = ((rcClient.right - rcClient.left) - 2);
        colWidthFilename = 0;
        }
    else
        {
        colWidthMountpoint = COL_WIDTH_MOUNTPOINT;
        colWidthFilename = COL_WIDTH_FILENAME;
        }

    ListView_SetColumnWidth(
                            G_hWndListView,
                            COL_IDX_MOUNTPOINT,
                            colWidthMountpoint
                           );
    ListView_SetColumnWidth(
                            G_hWndListView,
                            COL_IDX_FILENAME,
                            colWidthFilename
                           );

    UpdateWindow(G_hWndListView);
}

// =========================================================================
LRESULT CALLBACK wndMain_HandleMsg_WM_SIZE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_SIZE\n")));

    // Reset the list display size and repopulate in order to get the
    // column widths are sensible
    wndMain_SetupListDisplay(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_SIZE\n")));
    return 0;
}


// =========================================================================
LRESULT CALLBACK wndMain_HandleMsg_WM_CLOSE(HWND hWnd)
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_CLOSE\n")));
    if (G_hMenuContext != NULL)
        {
        DestroyMenu(G_hMenuContext);
        }

    if (G_hWndMenuBar != NULL)
        {
        DestroyWindow(G_hWndMenuBar);
        }
   
    DestroyWindow(hWnd);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_CLOSE\n")));
    return 0;
}


// =========================================================================
LRESULT CALLBACK wndMain_HandleMsg_WM_DESTROY()
{
    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_DESTROY\n")));

    // Quit the application...
    PostQuitMessage(0);

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_DESTROY\n")));
    return 0;
}

// =========================================================================
#ifdef DEBUG
BOOL TestOne(HWND hWnd)
{
    return TRUE;
}

BOOL TestTwo(HWND hWnd)
{
    return TRUE;
}


BOOL TestThree(HWND hWnd)
{
    return TRUE;
}
#endif

// =========================================================================
void _wndMain_DismountAll(HWND hWnd)
{
    BOOL allOK;
    HCURSOR csrHourglass;
    HCURSOR csrPrevious;

    csrHourglass = LoadCursor(NULL, IDC_WAIT);
    csrPrevious = SetCursor(csrHourglass);
    // Try normal (unforced) dismount...
    allOK = driver_DismountAll(FALSE);
    wndMain_RefreshList(hWnd);
    SetCursor(csrPrevious);

    if (allOK)
        {
        // Don't bother informing user; assume they know what they're doing
        //MsgInfo(hWnd, _("All volumes dismounted."));
        }
    else
        {
        MsgInfo(hWnd, _("One or more of volumes could not be dismounted."));
        }

}


// =========================================================================
void _wndMain_DismountSelected(HWND hWnd)
{
    UINT itemCount;
    UINT i;
    WCHAR mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];
    BOOL allOK;
    HCURSOR csrHourglass;
    HCURSOR csrPrevious;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_wndMain_DismountSelected\n")));

    itemCount = ListView_GetItemCount(G_hWndListView);

    for (i = 0; i < itemCount; i++)
	    {
	    if (ListView_GetItemState(
                                  G_hWndListView, 
                                  i,
                                  LVIS_SELECTED
                                 ) == LVIS_SELECTED)
		    {
            ListView_GetItemText( 
                                 G_hWndListView,
                                 i,
                                 COL_IDX_MOUNTPOINT,
                                 mountpoint, 
                                 sizeof(mountpoint)
                                );

            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Dismount mountpoint: %ls\n"), mountpoint));

            csrHourglass = LoadCursor(NULL, IDC_WAIT);
            csrPrevious = SetCursor(csrHourglass);
            allOK = driver_Dismount(mountpoint, FALSE);
            wndMain_RefreshList(hWnd);
            SetCursor(csrPrevious);


            // Try normal (unforced) dismount...
            if (allOK)
                {
                // Removed pointless message (assume user knows what they're doing!)
                // MsgInfo(hWnd, _("Volume dismounted."));
                }
            else
                {
                MsgInfo(hWnd, _("Dismount failed."));
                }

		    }
        }

    SecZeroMemory(mountpoint, sizeof(mountpoint));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_wndMain_DismountSelected\n")));
}


// =========================================================================
void _wndMain_PropertiesSelected(HWND hWnd)
{
    UINT itemCount;
    UINT i;
    WCHAR mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_wndMain_PropertiesSelected\n")));

    itemCount = ListView_GetItemCount(G_hWndListView);

    for (i = 0; i < itemCount; i++)
	    {
	    if (ListView_GetItemState(
                                  G_hWndListView, 
                                  i,
                                  LVIS_SELECTED
                                 ) == LVIS_SELECTED)
		    {
            ListView_GetItemText( 
                                 G_hWndListView,
                                 i,
                                 COL_IDX_MOUNTPOINT,
                                 mountpoint, 
                                 sizeof(mountpoint)
                                );

            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Properties of mountpoint: %ls\n"), mountpoint));
            DisplayDlgProperties(hWnd, mountpoint);
		    }
        }

    SecZeroMemory(mountpoint, sizeof(mountpoint));

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_wndMain_PropertiesSelected\n")));
}


// =========================================================================
void _wndMain_DisplayDocumentation(HWND hDlg)
{
    WCHAR* showURL;
    WCHAR testPathAndFilename[FREEOTFE_MAX_FILENAME_LENGTH];
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* filename;
    WCHAR* fileExtension;
    BOOL foundLocalCopy;

    showURL = NULL;

    foundLocalCopy = FALSE;


    // Check for a local copy of the documentation under the "CWD"  (the
    // directory in which this executable is located
    GetModuleFileName(
                      NULL, 
                      testPathAndFilename, 
                      (sizeof(testPathAndFilename) / sizeof(testPathAndFilename[0]))
                     ); 
    SDUParsePath(
                 testPathAndFilename, 
                 &uncMachine, 
                 &drive, 
                 &path, 
                 &filename, 
                 &fileExtension
                );

    if (!(foundLocalCopy))
        {
        // If ./docs/mobile_site/index.htm exists, display it
        wcscpy(filename, DOCUMENTATION_LOCALPATH_1);
        if (SDUCheckFileExists(testPathAndFilename))
            {
            foundLocalCopy = TRUE;
            }
        }

    if (!(foundLocalCopy))
        {
        // If ./docs/index.htm exists, display it
        wcscpy(filename, DOCUMENTATION_LOCALPATH_2);
        if (SDUCheckFileExists(testPathAndFilename))
            {
            foundLocalCopy = TRUE;
            }
        }

    // If there's a local copy of the documentation...
    if (foundLocalCopy)
        {
        showURL = testPathAndFilename;
        }
    // else fallback to launching WWW browser at documentation on WWW site
    // (DOCUMENTATION_URL)
    else
        {
        if (MsgPrompt(
                hDlg,
                _("Unable to locate a local copy of user guide; would you like to see the latest version on the Internet?"),
                MB_YESNO
               ) == IDYES)
            {
            showURL = DOCUMENTATION_URL;
            }

        }

    if (showURL != NULL)
        {
        DisplayURL(showURL);
        }
        
}


// =========================================================================
void _wndMain_ExploreSelected()
{
    UINT itemCount;
    UINT i;
    WCHAR mountpoint[FREEOTFE_MAX_FILENAME_LENGTH];

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("_wndMain_ExploreSelected\n")));

    itemCount = ListView_GetItemCount(G_hWndListView);

    for (i = 0; i < itemCount; i++)
	    {
	    if (ListView_GetItemState(
                                  G_hWndListView, 
                                  i,
                                  LVIS_SELECTED
                                 ) == LVIS_SELECTED)
		    {
            ListView_GetItemText( 
                                 G_hWndListView,
                                 i,
                                 COL_IDX_MOUNTPOINT,
                                 mountpoint, 
                                 sizeof(mountpoint)
                                );

            ExploreMountpoint(mountpoint);
		    }
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("_wndMain_ExploreSelected\n")));
}


// =========================================================================
BOOL CALLBACK wndMain_HandleMsg_WM_NOTIFY(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;
    int idCtrl;
    NMHDR* notifHdr;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("wndMain_HandleMsg_WM_NOTIFY\n")));

    idCtrl = (int)wParam;
    notifHdr = (NMHDR*)lParam;

    switch (notifHdr->code)
        {
        case NM_DBLCLK:
            {
            switch(idCtrl)
                {
                case IDC_MAIN_LISTVIEW:
                    {
                    _wndMain_ExploreSelected();
                    retval = TRUE;
                    break;
                    }
                }
            break;
            }

        // List view item changed (e.g. selection changed)
        case LVN_ITEMCHANGED:
            {
            switch(idCtrl)
                {
                case IDC_MAIN_LISTVIEW:
                    {
                    EnableDisableControls();
                    retval = TRUE;
                    break;
                    }
                }
            break;
            }

        }


    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("wndMain_HandleMsg_WM_NOTIFY\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK wndMain_HandleMsg_WM_DEVICECHANGE(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
    BOOL retval = FALSE;

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_ENTER, (TEXT("wndMain_HandleMsg_WM_DEVICECHANGE\n")));

    wndMain_RefreshList(hDlg);

    DEBUGOUTGUI(DEBUGLEV_VERBOSE_EXIT, (TEXT("wndMain_HandleMsg_WM_DEVICECHANGE\n")));
    return retval;
}


// =========================================================================
LRESULT CALLBACK wndMain_HandleMsg_WM_COMMAND(
                             HWND hWnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                            )
{
    LRESULT retval = 0;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_COMMAND\n")));
    switch(LOWORD(wParam))
        {
#ifdef FREEOTFE_STATS
        case ID_STATS_REPORT:
            {
            WCHAR ourFilename[FREEOTFE_MAX_FILENAME_LENGTH];
            WCHAR* uncMachine;
            WCHAR* drive;
            WCHAR* path;
            WCHAR* filename;
            WCHAR* fileExtension;
            HCURSOR csrHourglass;
            HCURSOR csrPrevious;
            SHELLEXECUTEINFO seInfo;

            MsgInfo(hWnd, _("About to generate stats report.\n\nPlease ensure PDA isn't running on battery power and tap \"OK\" to begin."));

            csrHourglass = LoadCursor(NULL, IDC_WAIT);
            csrPrevious = SetCursor(csrHourglass);

            // We dump our report into the "CWD"  (the directory in which this
            // executable is located
            GetModuleFileName(
                              NULL, 
                              ourFilename, 
                              (sizeof(ourFilename) / sizeof(ourFilename[0]))
                             ); 
            SDUParsePath(
                         ourFilename, 
                         &uncMachine, 
                         &drive, 
                         &path, 
                         &filename, 
                         &fileExtension
                        );
            /*
            wcsncpy_s(
                    filename, 
                    (
                     (sizeof(ourFilename) - (filename - path)) / 
                     sizeof(ourFilename[0])
                    ),
                    STATS_REPORT_FILE,
                    wcslen(STATS_REPORT_FILE)
                   );
            */
            wcsncpy(filename, STATS_REPORT_FILE);

            GenerateDriverStatsReport(ourFilename);

            SetCursor(csrPrevious);

            if (MsgPromptQuestion(
                           hWnd,
                           _("Stats report generated.\n\nDo you wish to view the report now?"),
                           MB_YESNO
                          ) == IDYES)
                {
                // Launch viewer with stats report

                // We use ShellExecuteEx, rather than by using the executable's
                // filename and CreateProcess(...)
                seInfo.cbSize = sizeof(seInfo);
                seInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
                seInfo.hwnd = NULL;
                seInfo.lpVerb = TEXT("open");
                seInfo.lpFile = ourFilename;
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
                              EXE_whatever_it_is,
                              ourFilename,

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

            break;
            }
#endif

#ifdef DEBUG
        case ID_TEST_TEST_XXX_1:
            {
            TestOne(hWnd);
            break;
            }

        case ID_TEST_TEST_XXX_2:
            {
            TestTwo(hWnd);
            break;
            }

        case ID_TEST_TEST_XXX_3:
            {
            TestThree(hWnd);
            break;
            }
#endif

        case ID_FILE_EXIT:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_EXIT\n")));
            PostMessage(hWnd, WM_CLOSE, 0, 0);
            break;
            }

        case ID_FILE_NEWVOL:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_NEWVOL\n")));
            MsgInfo(hWnd, _("Please note: If you wish to create a new volume for use on both PDAs and PCs, please do so using the PC version of FreeOTFE.\n\nSee documentation for further explanation."));
            DisplayDlgNewVolWizard(hWnd);
            break;
            }

        case ID_FILE_MOUNT:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_MOUNT\n")));
            DisplayDlgMount(hWnd);
            wndMain_RefreshList(hWnd);
            break;
            }

        case ID_FILE_MOUNT_LINUX:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_MOUNT_LINUX\n")));
            DisplayDlgMountLUKS(hWnd);
            wndMain_RefreshList(hWnd);
            break;
            }

        case ID_LINUX_LUKSDUMP:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_LINUX_LUKSDUMP\n")));
            DisplayDlgDumpLUKSWizard(hWnd);
            wndMain_RefreshList(hWnd);
            break;
            }            

        case ID_FILE_DISMOUNT:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_DISMOUNT\n")));
            _wndMain_DismountSelected(hWnd);
            break;
            }

        case ID_FILE_DISMOUNTALL:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_FILE_DISMOUNTALL\n")));
            _wndMain_DismountAll(hWnd);
            break;
            }

            
        case ID_VIEW_PROPERTIES:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_VIEW_PROPERTIES\n")));
            _wndMain_PropertiesSelected(hWnd);
            break;
            }

        case ID_VIEW_OPTIONS:
            {
            BOOL prevLargeIcons;
            BOOL prevSoftkeyMenus;

            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_VIEW_OPTIONS\n")));

            prevLargeIcons = G_Options->LargeIcons;
            prevSoftkeyMenus = G_Options->SoftkeyMenus;

            DisplayDlgOptions(hWnd);

            // If the user changed the menu options, setup the menu bar again
            if (prevSoftkeyMenus != G_Options->SoftkeyMenus)
                {
                wndMain_SetupMenuBar(hWnd);
                }

            // If the user changed the large/small icons option, setup
            // the list display again
            if (prevLargeIcons != G_Options->LargeIcons)
                {
                wndMain_SetupIconSize();
                wndMain_RefreshList(hWnd);
                }

            break;
            }

        case ID_VIEW_REFRESH:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_VIEW_REFRESH\n")));
            wndMain_RefreshList(hWnd);
            break;
            }

        case ID_TOOLS_CREATEKEYFILE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_TOOLS_CREATEKEYFILE\n")));
            DisplayDlgKeyfileWizard(hWnd);
            break;
            }

        case ID_TOOLS_CHANGEPASSWORD:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_TOOLS_CHANGEDETAILS\n")));
            DisplayDlgChpassWizard(hWnd);
            break;
            }

        case ID_TOOLS_CDBBACKUP:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_TOOLS_CDBBACKUP\n")));
            DisplayDlgBackup(hWnd);
            break;
            }

        case ID_TOOLS_CDBRESTORE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_TOOLS_CDBRESTORE\n")));
            DisplayDlgRestore(hWnd);
            break;
            }

        case ID_TOOLS_CDBDUMP:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_TOOLS_CDBDUMP\n")));
            DisplayDlgDumpCDBWizard(hWnd);
            break;
            }

        case ID_HELP_ABOUT:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_HELP_ABOUT\n")));
            DisplayDlgAbout(hWnd);
            break;
            }

        case ID_HELP_DOCUMENTATION:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_HELP_DOCUMENTATION\n")));
            _wndMain_DisplayDocumentation(hWnd);
            break;
            }

        case ID_CONTEXTMENU_EXPLORE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Command: ID_CONTEXTMENU_EXPLORE\n")));
            _wndMain_ExploreSelected();
            break;
            }

        default:
            {
            retval = DefWindowProc(hWnd, msg, wParam, lParam);
            }

        }  // switch(LOWORD(wParam))

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_COMMAND\n")));
    return retval;
}


// =========================================================================
BOOL CALLBACK wndMain_HandleMsg_WM_CONTEXTMENU(
                             HWND hWnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                             )
{
    BOOL retval;
    int x;
    int y;
    BOOL handled;
    HMENU popupMenu;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_HandleMsg_WM_CONTEXTMENU\n")));

    x = GET_X_LPARAM(lParam);
    y = GET_Y_LPARAM(lParam);
    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Context menu position: %d, %d\n"), x, y));

    handled = FALSE;
    if ((HWND)wParam != NULL)
        {
        if ((HWND)wParam == G_hWndListView)
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Displaying context menu...\n")));
            popupMenu = GetSubMenu(G_hMenuContext, 0);
            
            SDUi18n_TranslateMenu(popupMenu);

            TrackPopupMenu(
                           popupMenu,
                           (TPM_LEFTALIGN | TPM_TOPALIGN), 
                           x, 
                           y, 
                           0,
                           hWnd, 
                           NULL 
                         );

            handled = TRUE;
            retval = TRUE;
            }

        }

    if (!(handled))
        {
        retval = DefWindowProc(hWnd, msg, wParam, lParam);
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_HandleMsg_WM_CONTEXTMENU\n")));    
    return retval;
}

// =========================================================================
// [SORTRELATED]
// To sort the listview, call:
//SendMessage(
//            G_hWndListView,
//            LVM_SORTITEMS,
//            //LVM_SORTITEMSEX,  // NOT SUPPORTED under WinCE!
//            999,
//            (LPARAM)CompareFunc
//           );
// Which will call this function back with the lParam set on of each listitem
//
int CALLBACK CompareFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
    int retval;
    BOOL allOK;
    LV_ITEM listItem;
    WCHAR Text1[1024]; // [SORTRELATED] - replace with dynamicly allocated buffer
    WCHAR Text2[1024]; // [SORTRELATED] - replace with dynamicly allocated buffer

    allOK = TRUE;
    retval = 0; // Fallback if there's a problem

    // Get rid of PC-Lint warning
    memset(Text1, 0, sizeof(Text1));
    memset(Text2, 0, sizeof(Text2));

    if (allOK)
        {
        listItem.mask = LVIF_TEXT;
        listItem.iItem = lParam1;
        listItem.iSubItem = COL_IDX_MOUNTPOINT;
        listItem.pszText = Text1;
        listItem.cchTextMax = (sizeof(Text1) / sizeof(Text1[0]));
        allOK = ListView_GetItem(G_hWndListView, &listItem);
        }

    if (allOK)
        {
        listItem.mask = LVIF_TEXT;
        listItem.iItem = lParam2;
        listItem.iSubItem = COL_IDX_MOUNTPOINT;
        listItem.pszText = Text2;
        listItem.cchTextMax = (sizeof(Text2) / sizeof(Text2[0]));
        allOK = ListView_GetItem(G_hWndListView, &listItem);
        }

    if (allOK)
        {
        //retval = (wcscmp(Text1, Text2) * -1);
        retval = (wcscmp(Text1, Text2));
        }

    return retval;
}


// =========================================================================
LRESULT wndMain_HandleMsg_WM_DRAWITEM(
    HWND hWnd,
    UINT msg,
    WPARAM wParam,
    LPARAM lParam
)
{
    BOOL allOK;
    DRAWITEMSTRUCT* drawItem;
    int idx;
    int height;
    HIMAGELIST hil;
    int itemHilIdx;
    HBRUSH hBrush;
    LVITEM listItem;
    BOOL itemIsSelected;
    int iconDrawStyle;
    int iconX;
    int iconY;
    WCHAR* itemTextBuf;
    int itemTextBufSize;
    HFONT hFont;
    HFONT prevFont;
    LOGFONT lFont;
    RECT iconRect;
    int nudgeImageY;
    SIZE textSize;
    RECT textRect;
    int nudgeTextY;
    COLORREF prevTextColorFG;
    COLORREF prevTextColorBK;
    int maxText;
    int textLengthMountpoint;
    int textLengthFilename;

    allOK = FALSE;

    if (wParam == IDC_MAIN_LISTVIEW)
        {
        drawItem = (DRAWITEMSTRUCT*)lParam;
        idx = drawItem->itemID;
        if (idx < 0)
            {
            // Don't do anything - just exit
            allOK = TRUE;
            }
        else
            {
            listItem.mask = LVIF_IMAGE;
            listItem.iItem = idx;
            listItem.iSubItem = COL_IDX_MOUNTPOINT;

            allOK = ListView_GetItem(drawItem->hwndItem, &listItem);
            itemHilIdx = listItem.iImage;

            itemIsSelected = (drawItem->itemState & ODS_SELECTED);


            // Draw list item background...
            if (itemIsSelected)
                {
                // Item selected; draw highlighted background
                hBrush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));
                FillRect(drawItem->hDC, &(drawItem->rcItem), hBrush);
                DeleteObject(hBrush);
                }
            else // item not selected; draw black text on white background
                {
                // Item not selected; draw background colour
                // Is this actually needed??
                hBrush = GetStockObject(ListView_GetBkColor(drawItem->hwndItem));
                FillRect(drawItem->hDC, &(drawItem->rcItem), hBrush);
                // Note: Don't delete brush - it's a system object
                }

            // Display icon... 
            iconX = ICON_FALLBACK_SIZE_X;
            iconY = ICON_FALLBACK_SIZE_Y;
            hil = ListView_GetImageList(drawItem->hwndItem, LVSIL_NORMAL);
            if (hil != NULL)
                {
                if (!(ImageList_GetIconSize(
                                            hil,
                                            &iconX,
                                            &iconY
                                            )))
                    {
                    // Ensure iconX, iconY not overwritten...
                    iconX = ICON_FALLBACK_SIZE_X;
                    iconY = ICON_FALLBACK_SIZE_Y;
                    }

                iconRect = drawItem->rcItem;
                // Center icon within listview item display
                nudgeImageY = (((drawItem->rcItem.bottom - drawItem->rcItem.top) - iconX) / 2);
                // if ((nudgeImageY * 2) != ((drawItem->rcItem.bottom - drawItem->rcItem.top) - iconX))
                //     {
                //     nudgeImageY++;
                //     }
                iconRect.left += INDENT_ICON;
                iconRect.top += nudgeImageY;

                // Item spacing not used?
                //itemSpacing = ListView_GetItemSpacing(hWnd, TRUE);
                //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("itemSpacing: %d\n"), itemSpacing));
                //itemSpacing = ListView_GetItemSpacing(hWnd, FALSE);
                //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("itemSpacing: %d\n"), itemSpacing));
                //itemSpacing = ListView_GetItemSpacing(drawItem->hwndItem, TRUE);
                //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("itemSpacing: %d\n"), itemSpacing));
                //itemSpacing = ListView_GetItemSpacing(drawItem->hwndItem, FALSE);
                //DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("itemSpacing: %d\n"), itemSpacing));

                iconDrawStyle = (
                                 ILD_NORMAL |                      
                                 ILD_TRANSPARENT
                                );
                if (itemIsSelected)
                    {
                    iconDrawStyle |= ILD_SELECTED; // ILD_FOCUS
                    }

                ImageList_Draw(
                               hil, 
                               itemHilIdx,
                               drawItem->hDC,
                               iconRect.left,
                               iconRect.top,
                               iconDrawStyle
                              );
                }

            // Get ready to draw on the text...
            if (itemIsSelected)
                {
                prevTextColorFG = SetTextColor(
                                               drawItem->hDC, 
                                               GetSysColor(COLOR_HIGHLIGHTTEXT)
                                              );
                prevTextColorBK = SetBkColor(
                                             drawItem->hDC, 
                                             GetSysColor(COLOR_HIGHLIGHT)
                                            );
                }

            // Display mountpoint...
            prevFont = NULL;
            // Set mountpoint text to bold
            hFont = (HFONT)SendMessage(
                                       drawItem->hwndItem,
                                       WM_GETFONT,
                                       0,
                                       0
                                      );
            if (hFont != NULL)
                {
                if (GetObject(hFont, sizeof(lFont), &lFont) != 0)
                    {
                    lFont.lfWeight = FW_BOLD;
                    hFont = CreateFontIndirect(&lFont);
                    prevFont = SelectObject(drawItem->hDC, hFont);
                    }
                }

            // Get the length of the largest string
            textLengthMountpoint = SDUListView_GetItemTextLength(
                                                        drawItem->hwndItem,
                                                        idx,
                                                        COL_IDX_MOUNTPOINT
                                                       );
            textLengthFilename = SDUListView_GetItemTextLength(
                                                        drawItem->hwndItem,
                                                        idx,
                                                        COL_IDX_FILENAME
                                                       );
            maxText = max(textLengthMountpoint, textLengthFilename);
            // Increment to store NULL terminator
            maxText++;

            // +1 for terminating NULL
            itemTextBufSize = (maxText + 1) * sizeof(*itemTextBuf);
            itemTextBuf = malloc(itemTextBufSize);
            if (itemTextBuf == NULL)
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for max listitem item text\n")));
                }
            else
                {
                ListView_GetItemText(
                                     drawItem->hwndItem,
                                     idx,
                                     COL_IDX_MOUNTPOINT,  // iSubItem,
                                     itemTextBuf,
                                     maxText
                                    );

                // Center the two lines of text vertically within the item's display
                // Determine the height of a line, then sou
                GetTextExtentPoint32(
                                     drawItem->hDC, 
                                     itemTextBuf,
                                     wcslen(itemTextBuf),
                                     &textSize
                                    );

                textRect = drawItem->rcItem;
                textRect.left += INDENT_ICON + iconX + INDENT_TEXT;
                nudgeTextY = (
                              (
                               (drawItem->rcItem.bottom - drawItem->rcItem.top)
                                             - (textSize.cy * 2)
                              ) / 2);
                if (
                    (nudgeTextY * 2) != 
                      (
                       (drawItem->rcItem.bottom - drawItem->rcItem.top) 
                                     - (textSize.cy * 2)
                      )
                   )
                    {
                    nudgeTextY--;
                    }
                textRect.top += nudgeTextY;
                height = SDUDrawTextEllipses(
                                             drawItem->hDC,
                                             itemTextBuf,
                                             wcslen(itemTextBuf),
                                             &textRect,
                                             (
                                              DT_LEFT |
                                              DT_NOPREFIX |
                                              DT_SINGLELINE 
                                             )
                                            );


                // Restore non-bold font
                if (prevFont != NULL)
                    {
                    SelectObject(drawItem->hDC, prevFont);
                    }
                // Dump the bold font
                if (hFont != NULL)
                    {
                    DeleteObject(hFont);
                    }


                // Display filename...
                ListView_GetItemText(
                                     drawItem->hwndItem,
                                     idx,
                                     COL_IDX_FILENAME,  // iSubItem,
                                     itemTextBuf,
                                     maxText // (sizeof(itemText) / sizeof(itemText[0]))
                                    );

                textRect.top += height;
                height = SDUDrawTextEllipses(
                                             drawItem->hDC,
                                             itemTextBuf,
                                             wcslen(itemTextBuf),
                                             &textRect,
                                             (
                                              DT_LEFT |
                                              DT_NOPREFIX |
                                              DT_SINGLELINE 
                                             )
                                            );

                SecZeroAndFreeMemory(itemTextBuf, itemTextBufSize);
                }
             
            // Restore colours if changed...
            if (itemIsSelected)
                {
                SetTextColor(drawItem->hDC, prevTextColorFG);
                SetBkColor(drawItem->hDC, prevTextColorBK);
                }
            
            allOK = TRUE;
            }
        }

    return allOK;
}


// =========================================================================
LRESULT CALLBACK wndMain_Proc(
                             HWND hWnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                            )
{
    LRESULT retval = 0;
    static SHACTIVATEINFO s_sai = {0};

    switch(msg)
        {
        case WM_CREATE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CREATE\n")));
            s_sai.cbSize = sizeof(SHACTIVATEINFO);
            retval = wndMain_HandleMsg_WM_CREATE(hWnd);
            break;
            }

        case WM_SIZE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SIZE\n")));
            retval = wndMain_HandleMsg_WM_SIZE(hWnd);
            break;
            }

        case WM_ACTIVATE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_ACTIVATE\n")));
            SHHandleWMActivate(hWnd, wParam, lParam, &s_sai, 0);
            break;
            }

        case WM_SETTINGCHANGE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_SETTINGCHANGE\n")));
            SHHandleWMSettingChange(hWnd, wParam, lParam, &s_sai);
            break;
            }

        case WM_CLOSE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CLOSE\n")));
            retval = wndMain_HandleMsg_WM_CLOSE(hWnd);
            break;
            }

        case WM_DESTROY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DESTROY\n")));
            retval = wndMain_HandleMsg_WM_DESTROY();
            break;
            }
            
        case WM_NOTIFY:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_NOTIFY\n")));
            retval = wndMain_HandleMsg_WM_NOTIFY(hWnd, wParam, lParam);
            break;
            } 

        case WM_DEVICECHANGE:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DEVICECHANGE\n")));
            retval = wndMain_HandleMsg_WM_DEVICECHANGE(hWnd, wParam, lParam);
            break;
            } 

        case WM_COMMAND:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_COMMAND\n")));
            retval = wndMain_HandleMsg_WM_COMMAND(hWnd, msg, wParam, lParam);
            break;
            } 

        case WM_CONTEXTMENU:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_CONTEXTMENU\n")));
            retval = wndMain_HandleMsg_WM_CONTEXTMENU(hWnd, msg, wParam, lParam);
            break;
            }

        case WM_DRAWITEM:
            {
            DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("MSG: WM_DRAWITEM\n")));
            retval = wndMain_HandleMsg_WM_DRAWITEM(hWnd, msg, wParam, lParam);
            break;
            }

        default:
            {
            retval = DefWindowProc(hWnd, msg, wParam, lParam);
            }

        }  // switch(msg)

    return retval;
}


// =========================================================================
BOOL WndMainRegister()
{
    WNDCLASS wc;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("WndMainRegister\n")));

    InitCommonControls();

    wc.style         = (CS_HREDRAW | CS_VREDRAW) ;
    wc.lpfnWndProc   = (WNDPROC)wndMain_Proc;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hIcon         = LoadIcon(G_hInstance, MAKEINTRESOURCE(IDI_ICON_MAIN));
    wc.hInstance     = G_hInstance;
    wc.hCursor       = NULL; // No cursor for WinCE
    wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName  = NULL; // Must be set to NULL; WinCE doesn't support
                             // "MAKEINTRESOURCE(IDR_MAINMENU)" here
    wc.lpszClassName = APP_TITLE;

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("WndMainRegister\n")));
    return (RegisterClass(&wc) != 0);
}


// =========================================================================
HWND WndMainCreate()
{
    HWND retval = NULL;

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("WndMainCreate\n")));

    retval = CreateWindow(
                          APP_TITLE,      // Class name
                          APP_TITLE,      // Window title       
                          WS_VISIBLE,     // Style
                          CW_USEDEFAULT,  // X
                          CW_USEDEFAULT,  // Y
                          CW_USEDEFAULT,  // Width
                          CW_USEDEFAULT,  // Height
                          NULL,           // Parent window
                          NULL,           // Menu
                          G_hInstance, 
                          NULL
                         );

    if (retval == NULL)      
        {
        MsgError(NULL, _("Unable to create main window"));
        }
    else
        {
        RECT rc;

        // Resize the main window to the size of the work area.
        SHFullScreen(
                     retval, 
                     (
                      SHFS_SHOWTASKBAR | 
                      SHFS_SHOWSTARTICON | 
                      SHFS_SHOWSIPBUTTON
                     )
                    );
        SystemParametersInfo(SPI_GETWORKAREA, 0, &rc, FALSE);
        MoveWindow(
                   retval, 
                   rc.left, 
                   rc.top, 
                   (rc.right - rc.left),
                   (rc.bottom - rc.top),
                   TRUE
                  );
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("WndMainCreate\n")));
    return retval;
}


// =========================================================================
void EnableDisableControls()
{
    int itemCount;    
    int selectedCount;    

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("EnableDisableControls\n")));

    itemCount = ListView_GetItemCount(G_hWndListView);
    selectedCount = ListView_GetSelectedCount(G_hWndListView);

    // Context menu items...
    MenuItemEnableMenu(
                   G_hMenuContext, 
                   ID_CONTEXTMENU_EXPLORE, 
                   (selectedCount > 0) 
                  ); 
    MenuItemEnableMenu(
                   G_hMenuContext, 
                   ID_FILE_DISMOUNT, 
                   (selectedCount > 0) 
                  ); 
    MenuItemEnableMenu(
                   G_hMenuContext, 
                   ID_FILE_DISMOUNTALL, 
                   (itemCount > 0) 
                  ); 
    MenuItemEnableMenu(
                   G_hMenuContext, 
                   ID_VIEW_PROPERTIES, 
                   (selectedCount > 0) 
                  ); 

    // Commandbar items...
    CommandBar_ItemEnable(
                     G_hWndMenuBar,
                     ID_FILE_DISMOUNT,
                     (selectedCount > 0)
                    );
    CommandBar_ItemEnable(
                     G_hWndMenuBar,
                     ID_FILE_DISMOUNTALL,
                     (itemCount > 0)
                    );
    CommandBar_ItemEnable(
                     G_hWndMenuBar,
                     ID_VIEW_PROPERTIES,
                     (selectedCount > 0)
                    );

    if (selectedCount > 0)
        {
        ChangeLeftSoftkey(
                          G_hWndMenuBar,
                          ID_FILE_DISMOUNT,
                          NULL
                         );
        }
    else
        {
        ChangeLeftSoftkey(
                          G_hWndMenuBar,
                          ID_FILE_MOUNT,
                          NULL
                         );
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("EnableDisableControls\n")));
}


// =========================================================================
void wndMain_RefreshList(HWND hWnd)
{
    int cnt = 0;
    int i;
    REGDETAILS_ACTIVE* mountedDetails = NULL;
    BOOL allOK = TRUE;
    int detailsByteSize;
    int rowNum;
    LVITEM listItem;
    HCURSOR csrHourglass;
    HCURSOR csrPrevious;

    // Debug; disable the listview to speed up...
    // DISABLE_LIST_VIEW is defined in main.h if DEBUG is turned on
    if (DISABLE_LIST_VIEW)
        {
        return;
        }

    DEBUGOUTGUI(DEBUGLEV_ENTER, (TEXT("wndMain_RefreshList\n")));

    detailsByteSize = 0;

    csrHourglass = LoadCursor(NULL, IDC_WAIT);
    csrPrevious = SetCursor(csrHourglass);

    ListView_DeleteAllItems(G_hWndListView);

    allOK = driver_GetMountedCount(&cnt);
    if (allOK)
        {
        detailsByteSize = (cnt * sizeof(*mountedDetails)); 
        mountedDetails = malloc(detailsByteSize);
        if (mountedDetails == NULL)
            {
            DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to malloc memory for all volumes mounted\n")));
            allOK = FALSE;
            }
        else
            {
            allOK = driver_MountedVolumes(detailsByteSize, mountedDetails);
            if (!(allOK))
                {
                SecZeroAndFreeMemory(mountedDetails, detailsByteSize);
                mountedDetails = NULL;
                }
            }
        }

    if (allOK) 
        {
        for(i = 0; i < cnt; i++)
            {
            SHFILEINFO shfi;
            HIMAGELIST hil;
            DIOC_DISK_DEVICE_STATUS volumeDetails;

            allOK = driver_GetVolumeInfo_DeviceName(
                                         mountedDetails[i].Name,
                                         &volumeDetails
                                        );
            if (!(allOK))
                {
                DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to get volume details for: %ls\n"), mountedDetails[i].Mountpoint));
                // Bail out...
                break;
                }
            else
                {
                hil = (HIMAGELIST)SHGetFileInfo(
                                                mountedDetails[i].Mountpoint, 
                                                0,
                                                &shfi, 
                                                sizeof(shfi),
                                                (
                                                 SHGFI_SYSICONINDEX | 
                                                 SHGFI_LARGEICON
                                                )
                                               );
                // !! IMPORTANT  !!
                // LVS_SHAREIMAGELISTS must be specified for the listview - 
                // otherwise when the listview is destroyed, it'll try and 
                // destroy it's imagelist (the *SYSTEM* *IMAGELIST*) on it's
                // way out!
                // Destroying the system imagelist is a *really* silly thing
                // to do as it makes all the system icons disappear!)
                ListView_SetImageList(G_hWndListView, hil, LVSIL_NORMAL); 

                // To display large icons...
                if (G_Options->LargeIcons)
                    {
                    // We set the "small" icons to be the same as the normal
                    // sized icons here. The report view line height is taken
                    // from *this* (LVSIL_SMALL) imagelist
                    ListView_SetImageList(G_hWndListView, hil, LVSIL_SMALL); 
                    }
                else
                    {
                    // To display small icons...                
                    hil = (HIMAGELIST)SHGetFileInfo(
                                                    mountedDetails[i].Mountpoint, 
                                                    0,
                                                    &shfi, 
                                                    sizeof(shfi),
                                                    (
                                                     SHGFI_SYSICONINDEX | 
                                                     SHGFI_SMALLICON
                                                    )
                                                   );
                    // !! IMPORTANT  !!
                    // LVS_SHAREIMAGELISTS must be specified for the listview - 
                    // otherwise when the listview is destroyed, it'll try and 
                    // destroy it's imagelist (the *SYSTEM* *IMAGELIST*) on it's
                    // way out!
                    // Destroying the system imagelist is a *really* silly thing
                    // to do as it makes all the system icons disappear!)
                    ListView_SetImageList(G_hWndListView, hil, LVSIL_SMALL); 
                    }

                // We insert the new item, *then* set it's text.
                // This is because we *must* supply iSubItem set to 0 when
                // inserting - and gives us a little more freedom as to
                // whether we set COL_IDX_MOUNTPOINT to 0 and COL_IDX_FILENAME
                // to 1, or vice versa
                listItem.iItem = 0;  // Stick them onto the start of the list
                listItem.iSubItem = 0;
                listItem.mask = 0;

                listItem.mask |= LVIF_IMAGE;
                listItem.iImage = shfi.iIcon;

                // We *have* to set the text initially, or the next time
                // the list is refreshed, the ListView_InsertItem(...) will
                // cause a data abort for some unknown reason!
                // !! WARNING !!
                // [SORTRELATED]
                // atm, we just set this to an empty string. If
                // LVS_SORTASCENDING/LVS_SORTDESCENDING ever start working,
                // it may well be worth revisiting this, as the act of setting
                // the text may well change the insert position, getting the
                // subitem settext set on the wrong item in the list?
                listItem.mask |= LVIF_TEXT;
                listItem.pszText = TEXT("");
                listItem.cchTextMax = wcslen(listItem.pszText);

                rowNum = ListView_InsertItem(G_hWndListView, &listItem);
                if (rowNum < 0)
                    {
                    DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to insert listview item\n")));
                    allOK = FALSE;
                    }
                else
                    {
                    ListView_SetItemText(
                                         G_hWndListView,
                                         rowNum, 
                                         COL_IDX_MOUNTPOINT, 
                                         mountedDetails[i].Mountpoint
                                        );

                    ListView_SetItemText(
                                         G_hWndListView,
                                         rowNum, 
                                         COL_IDX_FILENAME, 
                                         volumeDetails.Filename
                                        );

                    /*
                    [SORTRELATED]
                    Intended for use with sorting the list, this isn't
                    implemented, as there's no easy way of doing it...
                    Note: rowNum will typically always return 0 as we insert
                          into the start of the list
                    listItem.mask = LVIF_PARAM;
                    listItem.iItem = rowNum;
                    listItem.iSubItem = COL_IDX_MOUNTPOINT;
                    listItem.lParam = 9999;
                    ListView_SetItem(
                                     G_hWndListView,
                                     &listItem
                                    );
                    */
                    }

                }
            }

        EnableDisableControls();
        }

    RegDetailsFreeAllActive(cnt, mountedDetails);
    SecZeroAndFreeMemory(mountedDetails, detailsByteSize);

    SetCursor(csrPrevious);

    if (!(allOK))
        {
        MsgError(hWnd, _("Unable to get details of all mounted volumes"));
        }

    DEBUGOUTGUI(DEBUGLEV_EXIT, (TEXT("wndMain_RefreshList\n")));
}


// =========================================================================
// =========================================================================
