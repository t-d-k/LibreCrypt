// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDADriverList_H
#define _FreeOTFE4PDADriverList_H   1

#include <windows.h>


// =========================================================================
// Structures...

typedef struct _LNKLIST_DISABLED_DRIVER LNKLIST_DISABLED_DRIVER;
struct _LNKLIST_DISABLED_DRIVER {
    WCHAR* Filename;
    LNKLIST_DISABLED_DRIVER* Next;
};


// =========================================================================
// Functions...

BOOL AddDisabledDriver(LNKLIST_DISABLED_DRIVER** Head, WCHAR* driverName);
void RemoveDisabledDriver(LNKLIST_DISABLED_DRIVER** Head, WCHAR* driverName);
void FreeDisabledDriver(LNKLIST_DISABLED_DRIVER* item);
void FreeAllDisabledDrivers(LNKLIST_DISABLED_DRIVER** Head);
int  CountDisabledDrivers(LNKLIST_DISABLED_DRIVER* Head);
BOOL IsDriverDisabled(LNKLIST_DISABLED_DRIVER* Head, WCHAR* driverName);
LNKLIST_DISABLED_DRIVER* DeepCopyDisabledDrivers(LNKLIST_DISABLED_DRIVER* Head);

// =========================================================================
// =========================================================================

#endif

