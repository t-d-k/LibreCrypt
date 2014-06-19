// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _dlgMount_H
#define _dlgMount_H   1

#include <windows.h>

void DisplayDlgMount(HWND hWnd);
void DisplayDlgMount_Params(
    HWND hWnd,
    WCHAR* Mountpoint,
    WCHAR* Filename,
    WCHAR* Keyfile,
    LARGE_INTEGER OffsetWithinFile,
    BOOL CDBAtOffset,
    WCHAR* UserPassword,
    unsigned int SaltLength,  // In *bits*
    unsigned int KeyIterations,
    BOOL ReadOnly,
    BOOL Silent
);


// =========================================================================
// =========================================================================

#endif

