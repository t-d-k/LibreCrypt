// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _SDUi18n_GUI_H
#define _SDUi18n_GUI_H   1

#include <windows.h>

void SDUi18n_TranslateWindow(HWND hwnd);
void SDUi18n_TranslateCommandBar(HWND hCmdBar);
void SDUi18n_TranslateMenu(HMENU hMenu);
void SDUi18n_TranslateToolbar(HWND hWndMenu);
void SDUi18n_FreeOriginalStrings(HWND hwnd);


// =========================================================================
// =========================================================================

#endif

