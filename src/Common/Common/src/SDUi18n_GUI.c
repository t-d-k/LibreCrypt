
#include "SDUi18n_GUI.h"

#include <windows.h>
#include "SDULinkedList.h"
#include "SDUi18n.h"
#include "FreeOTFE4PDAGUIlib.h"  // Required for ID_SEPARATOR
#include "FreeOTFEGUIlib.h"

#include <aygshell.h>  // Required for SH... functions/definitions/etc
#pragma comment(lib, "aygshell") // Link in aygshell.lib

// =========================================================================

typedef struct _SDUTRANSLATION_ORIGINAL {
  HWND TranslatedWindow;
  TCHAR* OriginalText;
  struct _SDUTRANSLATION_ORIGINAL* Next;  
} SDUTRANSLATION_ORIGINAL, *PSDUTRANSLATION_ORIGINAL;

typedef struct _SDUTRANSLATION_ORIGINAL_FOR_WINDOW {
  HWND MainWindow;
  LNKLIST_ITEM* Head;
} SDUTRANSLATION_ORIGINAL_FOR_WINDOW, *PSDUTRANSLATION_ORIGINAL_FOR_WINDOW;


// =========================================================================

// This global stores all original text, pre-translation
// Required in case a window needs to be retranslated into a different language
//lplpjunk static SDUTRANSLATION_ORIGINAL* G_PretransatedStrings = NULL;
static LNKLIST_ITEM* G_PretransatedStrings = NULL;


// =========================================================================
// Forward declarations
void _SDUi18n_BuildWindowListWindow(HWND hwnd, HWND parenthWnd);
void _SDUi18n_TranslateWindowList(HWND hwnd);
void _SDUi18n_TranslateMenuForWindow(HWND hWndMenu);


// =========================================================================
SDUTRANSLATION_ORIGINAL_FOR_WINDOW* SDUi18n_SeekMainWindowList(HWND MainWindow)
{
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* retval;
  int mainWindowCnt;
  int i;
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow;

  retval = NULL;

  mainWindowCnt = SDULLCountItems(G_PretransatedStrings);
  for(i = 0; i < mainWindowCnt; i++)
      {
      if (SDULLGetItem(G_PretransatedStrings, i, &currMainWindow))
          {
          if (currMainWindow->MainWindow == MainWindow)
              {
              retval = currMainWindow;
              break;
              }
          }      
      }

  return retval;
}


// =========================================================================
SDUTRANSLATION_ORIGINAL* SDUi18n_SeekOriginalWindowText(HWND MainWindow, HWND ChildWindow)
{
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow;
  SDUTRANSLATION_ORIGINAL* retval;
  int childWindowCnt;
  int i;
  SDUTRANSLATION_ORIGINAL* currChildWindow;

  retval = NULL;

  currMainWindow = SDUi18n_SeekMainWindowList(MainWindow);
  if (currMainWindow != NULL)
      {
      childWindowCnt = SDULLCountItems(currMainWindow->Head);
      for(i = 0; i < childWindowCnt; i++)
          {
          if (SDULLGetItem(currMainWindow->Head, i, &currChildWindow))
              {
              if (currChildWindow->TranslatedWindow == ChildWindow)
                  {
                  retval = currChildWindow;
                  break;
                  }
              }      
          }
      }

  return retval;
}


// =========================================================================
void SDUi18n_AppendTranslatedString(
    HWND MainWindow, 
    SDUTRANSLATION_ORIGINAL* translatedToAdd
)
{
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow;
  
  currMainWindow = SDUi18n_SeekMainWindowList(MainWindow);
  if (currMainWindow == NULL)
      {
      currMainWindow = malloc(sizeof(*currMainWindow));
      if (currMainWindow != NULL)
          {
          currMainWindow->MainWindow = MainWindow;
          currMainWindow->Head = NULL;
          if (!(SDULLAddItem(&G_PretransatedStrings, currMainWindow)))
              {
              free(currMainWindow);
              currMainWindow = NULL;
              }
          }
      }

  if (currMainWindow != NULL)
      {
      SDULLAddItem(&currMainWindow->Head, translatedToAdd);
      }

}


// =========================================================================
void SDUFreeOriginalStrings(HWND hwnd)
{
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow;

  currMainWindow = SDUi18n_SeekMainWindowList(hwnd);
  if (currMainWindow != NULL)
      {
      SDULLRemoveItem(&G_PretransatedStrings, currMainWindow);
      SDULLRemoveAllItems(&(currMainWindow->Head), TRUE);
      free(currMainWindow);
      }

}


// =========================================================================
void SDUi18n_FreeOriginalStrings(HWND hwnd)
{
    int mainWindowCnt;
    SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow; 
    int i;

  mainWindowCnt = SDULLCountItems(G_PretransatedStrings);
  for(i = 0; i < mainWindowCnt; i++)
      {
      if (SDULLGetItem(G_PretransatedStrings, i, &currMainWindow))
          {
          currMainWindow->MainWindow = NULL;
          SDULLRemoveAllItems(&(currMainWindow->Head), TRUE);
          }      
      }

   SDULLRemoveAllItems(&G_PretransatedStrings, TRUE);

}


// =========================================================================
void SDUi18n_TranslateOriginalWindowText(PSDUTRANSLATION_ORIGINAL translation)
{
   const TCHAR* tmpBuffer;

#ifdef UNICODE
   tmpBuffer = SDUi18n_GetTextW(translation->OriginalText);
#else
   tmpBuffer = SDUi18n_GetText(translation->OriginalText);
#endif

   SetWindowText(
                 translation->TranslatedWindow, 
                 tmpBuffer
                );

   // Some controls may need to be resized to accomodate the translated text
   if (
       (IsWindow_Static(translation->TranslatedWindow)) ||
       (IsWindow_Checkbox(translation->TranslatedWindow)) ||
       (IsWindow_RadioButton(translation->TranslatedWindow))
      )
       {
       ResizeStaticToText_HWND(translation->TranslatedWindow);
       }

}


// =========================================================================
BOOL CALLBACK _SDUBiuldWindowListEnumWindowsProc(
    HWND hwnd,
    LPARAM lParam
)
{
  TCHAR* windowTextOld;
  int windowTextOldLength;
  SDUTRANSLATION_ORIGINAL* translation;

  translation = SDUi18n_SeekOriginalWindowText((HWND)lParam, hwnd);
  if (translation == NULL)
    {
    windowTextOldLength = GetWindowTextLength(hwnd);
    if (windowTextOldLength != 0)
      {
      // +1 to include terminating NULL
      windowTextOldLength = windowTextOldLength + 1;
      windowTextOld = (TCHAR*)calloc(windowTextOldLength, sizeof(*windowTextOld));
      if (windowTextOld != NULL) 
        {
        GetWindowText(hwnd, windowTextOld, windowTextOldLength);

        translation = (SDUTRANSLATION_ORIGINAL*)malloc(sizeof(*translation));
        translation->TranslatedWindow = hwnd;
        translation->OriginalText = windowTextOld;
        translation->Next = NULL;
        SDUi18n_AppendTranslatedString((HWND)lParam, translation);
        }
      }
  }

  return TRUE;
}


// =========================================================================
void SDUi18n_TranslateWindow(HWND hwnd)
{
    // Build up a list of windows to be translated (including the one passed in)
    _SDUi18n_BuildWindowListWindow(hwnd, hwnd);

    // Translate all windows in the list built
    _SDUi18n_TranslateWindowList(hwnd);
}


// =========================================================================
#ifdef WINCE
// Windows Mobile doesn't have EnumChildWindows(...)
void _SDUi18n_BuildWindowListWindow(HWND hwnd, HWND parenthWnd)
{
  HWND child;

  _SDUBiuldWindowListEnumWindowsProc(hwnd, (LPARAM)parenthWnd);

  // ...and all of its child windows
  child = GetWindow(hwnd, GW_CHILD);
  while (child != NULL) 
      {
      _SDUi18n_BuildWindowListWindow(child, parenthWnd);
      child = GetWindow(child, GW_HWNDNEXT);
      }

}
#else
lplp - this version hasn't been tested yet, but should work
void _SDUi18n_TranslateWindow(HWND hwnd, HWND parenthWnd)
{
  // Translate the window passed in...
  _SDUTranslateEnumWindowsProc(hwnd, (LPARAM)parenthWnd);

  /// ..and all of its child windows
  EnumChildWindows(
                   hwnd, 
                   _SDUBiuldWindowListEnumWindowsProc, 
                   (LPARAM)parenthWnd
                  );
}
#endif


// =========================================================================
void _SDUi18n_TranslateWindowList(HWND hwnd)
{
  int i;
  SDUTRANSLATION_ORIGINAL_FOR_WINDOW* currMainWindow;
  SDUTRANSLATION_ORIGINAL* currChildWindow;
  int childWindowCnt;

  currMainWindow = SDUi18n_SeekMainWindowList(hwnd);
  if (currMainWindow != NULL)
      {
      childWindowCnt = SDULLCountItems(currMainWindow->Head);
      for(i = 0; i < childWindowCnt; i++)
          {
          if (SDULLGetItem(currMainWindow->Head, i, &currChildWindow))
              {
              SDUi18n_TranslateOriginalWindowText(currChildWindow);
              }      
          }
      }
}


// =========================================================================
// Because we can't load a main menu with separators, any menuitem with
// an ID of ID_SEPARATOR will be replaced with a separator
void SDUi18n_TranslateMenu(HMENU hMenu)
{
    MENUITEMINFO menuItemInfo;
    int i;
    WCHAR buffer[1024];  // Just a value large enough to store menu text

    i = 0;
    while (TRUE)
        {
        ZeroMemory(&menuItemInfo, sizeof(menuItemInfo));
        menuItemInfo.cbSize = sizeof(menuItemInfo);
        menuItemInfo.fMask  = (
                      MIIM_DATA |
                      MIIM_ID |
                      MIIM_STATE |
                      MIIM_SUBMENU |
                      MIIM_TYPE
                     );
        menuItemInfo.fMask = (MIIM_TYPE | MIIM_SUBMENU);
        menuItemInfo.dwTypeData = buffer;
        menuItemInfo.cch = sizeof(buffer);

        if (!(GetMenuItemInfo(hMenu, i, TRUE, &menuItemInfo)))
            {
            break;
            }

        //if (!(
        //    (menuItemInfo.fType && MFT_SEPARATOR) ||
        //    (menuItemInfo.wID == ID_SEPARATOR)
        //      ))
        if (menuItemInfo.fType == MFT_STRING)
        //if (menuItemInfo.cch > 0)
            {
            // Translate it...
            wcscpy(buffer, SDUi18n_GetTextW(menuItemInfo.dwTypeData));
            
            menuItemInfo.cch = wcslen(menuItemInfo.dwTypeData);

            menuItemInfo.fMask = MIIM_TYPE;
            menuItemInfo.fType = MFT_STRING;

            if (!(SetMenuItemInfo(hMenu, i, TRUE, &menuItemInfo)))
                {
                SDUDisplayLastError_Text();
                break;
                }

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

        // Translate any submenus
        if (menuItemInfo.hSubMenu)
            {
            SDUi18n_TranslateMenu(menuItemInfo.hSubMenu);
            }

        i++;
        }

} 

void _SDUi18n_TranslateMenuForWindow(HWND hWndMenu)
{
    HMENU hMenu;

    hMenu = (HMENU)SendMessage(
                               hWndMenu, 
                               SHCMBM_GETMENU, 
                               (WPARAM)0,
                               (LPARAM)0
                              ); 

    SDUi18n_TranslateMenu(hMenu);

    DrawMenuBar(hWndMenu);
}

void SDUi18n_TranslateCommandBar(HWND hCmdBar)
{
    _SDUi18n_TranslateMenuForWindow(hCmdBar);
    SDUi18n_TranslateToolbar(hCmdBar);
}


void SDUi18n_TranslateToolbar(HWND hCmdBar)
{
    TBBUTTONINFO buttonInfo;
    TBBUTTON tbb;
    int i;
    WCHAR buffer[1024];  // Just a value large enough to store menu text

    i = 0;
    while (TRUE)
        {
        memset(&tbb, 0, sizeof(tbb));

        if (!(SendMessage(
                    hCmdBar,
                    TB_GETBUTTON,
                    (WPARAM)i,
                    (LPARAM)(&tbb)
                   )))
            {
            break;
            }

        // Don't translate if just image button
        if (tbb.fsStyle != TBSTYLE_BUTTON)
            {
            if ((tbb.fsStyle & TBSTYLE_DROPDOWN) == TBSTYLE_DROPDOWN)
                {
                SDUi18n_TranslateMenu((HMENU)tbb.dwData);
                }

            buttonInfo.cbSize = sizeof(buttonInfo);
            buttonInfo.dwMask = TBIF_TEXT;
            buttonInfo.pszText = buffer;
            buttonInfo.cchText = sizeof(buffer) / sizeof(WCHAR);
            if (!(SendMessage(
                        hCmdBar,
                        TB_GETBUTTONINFO,
                        (WPARAM)tbb.idCommand,
                        (LPARAM)(&buttonInfo)
                       )))
                {
                break;
                }

            // Translate...
            wcscpy(buffer, SDUi18n_GetTextW(buttonInfo.pszText));
                
            if (!(SendMessage(
                        hCmdBar,
                        TB_SETBUTTONINFO,
                        (WPARAM)tbb.idCommand,
                        (LPARAM)(&buttonInfo)
                       )))
                {
                break;
                }
            }

        i++;
        }
}

// =========================================================================
// =========================================================================
