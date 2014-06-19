// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAContextMgrOpen_H
#define _FreeOTFE4PDAContextMgrOpen_H   1

#include <windef.h>  // Required for DWORD
#include <winbase.h>  // Required for CRITICAL_SECTION

// =========================================================================
// Const definitions


// =========================================================================
// Structures

// Open handle list (linked list) item
typedef struct _OPEN_LIST_ITEM {
    DWORD hOpen;
    DWORD hDevice;
    LARGE_INTEGER Position;
    CRITICAL_SECTION CriticalSection;

    struct _OPEN_LIST_ITEM* NextListItem;
} OPEN_LIST_ITEM, *POPEN_LIST_ITEM;


// =========================================================================
// Global definitions

extern BOOL G_contextMgrOpenInitialized;
extern DWORD G_contextMgrOpenLastHandle;
extern CRITICAL_SECTION G_contextMgrOpenCriticalSection;
extern OPEN_LIST_ITEM* G_contextMgrOpenDeviceList;


// =========================================================================
// Function headers

BOOL contextMgrOpen_Init();
BOOL contextMgrOpen_Deinit();
OPEN_LIST_ITEM* contextMgrOpen_GetOpenContext(DWORD hOpen);
void contextMgrOpen_ReleaseOpenContext(OPEN_LIST_ITEM* listItem);
DWORD contextMgrOpen_AddOpenContext(DWORD hDevice);
void contextMgrOpen_DeleteOpenContext(DWORD hOpen);

DWORD contextMgrOpen_CountDeviceOpenContext(DWORD hDevice);
void contextMgrOpen_DeleteDeviceOpenContext(DWORD hDevice);


// =========================================================================
// =========================================================================

#endif

