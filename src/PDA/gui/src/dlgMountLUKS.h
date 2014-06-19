// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _dlgMountLUKS_H
#define _dlgMountLUKS_H   1

#include <windows.h>

void DisplayDlgMountLUKS(HWND hWnd);
void DisplayDlgMountLUKS_Params(
    HWND hWnd,
    WCHAR* Mountpoint,
    WCHAR* Filename,
    WCHAR* UserPassword,
    BOOL ReadOnly,
    BOOL BaseIVCypherOnHashLength,
    LARGE_INTEGER SizeLimit,
    BOOL Silent
);


// =========================================================================
// =========================================================================

#endif

