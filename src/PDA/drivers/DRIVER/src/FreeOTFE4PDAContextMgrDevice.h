// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAContextMgrDevice_H
#define _FreeOTFE4PDAContextMgrDevice_H   1

#include "FreeOTFEContext.h"


// =========================================================================
// Const definitions


// =========================================================================
// Structures

// Device handle list (linked list) item
typedef struct _DEVICE_LIST_ITEM {
    DWORD hDevice;
    BOOL DismountPending;
    DEVICE_CONTEXT* DeviceContext;
    CRITICAL_SECTION CriticalSection;

    struct _DEVICE_LIST_ITEM* NextListItem;
} DEVICE_LIST_ITEM, *PDEVICE_LIST_ITEM;


// =========================================================================
// Global definitions

extern BOOL G_contextMgrDeviceInitialized;
extern DWORD G_contextMgrDeviceLastHandle;
extern CRITICAL_SECTION G_contextMgrDeviceCriticalSection;
extern DEVICE_LIST_ITEM* G_contextMgrDeviceDeviceList;


// =========================================================================
// Function headers

BOOL contextMgrDevice_Init();
BOOL contextMgrDevice_Deinit();
DEVICE_LIST_ITEM* contextMgrDevice_GetDevice(DWORD hDevice, DEVICE_CONTEXT** devContext);
void contextMgrDevice_ReleaseDevice(DEVICE_LIST_ITEM* listItem);
DWORD contextMgrDevice_AddDevice(DEVICE_CONTEXT* devContext);
DEVICE_CONTEXT* contextMgrDevice_DeleteDevice(DWORD hDevice);


// =========================================================================
// =========================================================================

#endif

