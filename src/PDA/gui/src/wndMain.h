// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _wndMain_H
#define _wndMain_H   1

// Toolbar bitmap indexes
#define TOOLBAR_BITMAP_NEW           0
#define TOOLBAR_BITMAP_MOUNT         1
#define TOOLBAR_BITMAP_DISMOUNT      2
#define TOOLBAR_BITMAP_DISMOUNT_ALL  3


BOOL WndMainRegister();
HWND WndMainCreate();

LRESULT CALLBACK wndMain_Proc(
                             HWND hwnd,
                             UINT msg,
                             WPARAM wParam,
                             LPARAM lParam
                            );

void RefreshList();
void EnableDisableControls();

// =========================================================================
// =========================================================================

#endif

