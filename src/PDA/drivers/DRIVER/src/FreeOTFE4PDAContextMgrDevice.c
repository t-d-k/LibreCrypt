// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

// Originally implemented as a dynamic array, changed to use a linked list
// in order to minimise memory usage; in particular with the open file
// handles, which could result in a large (mainly unused) array if a large
// number of file handles were opened on the device, followed by all except
// the *last* being closed again.


#include "FreeOTFE4PDA.h"
#include "FreeOTFE4PDAContextMgrDevice.h"
#include "FreeOTFE4PDAContextMgrOpen.h"
#include "FreeOTFEDebug.h"
#include "SDUGeneral.h"
#include "FreeOTFElib.h"


// =========================================================================
// Default globals...

BOOL              G_contextMgrDeviceInitialized;
DWORD             G_contextMgrDeviceLastHandle;
CRITICAL_SECTION  G_contextMgrDeviceCriticalSection;
DEVICE_LIST_ITEM* G_contextMgrDeviceDeviceList;


// =========================================================================
BOOL contextMgrDevice_Init()
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrDevice_Init\n")));

    if (!(G_contextMgrDeviceInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Initializing...\n")));
        G_contextMgrDeviceLastHandle = 1;

        G_contextMgrForceDismounts = FALSE;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to initialize critical section: 0x%0.8x\n"), &G_contextMgrDeviceCriticalSection));
        InitializeCriticalSection(&G_contextMgrDeviceCriticalSection);

        // Note that we defer creating the actual list until a device is added
        // to it; this reduces the load/initialization time for the DLL
        G_contextMgrDeviceDeviceList = NULL;
                
        G_contextMgrDeviceInitialized = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrDevice_Init\n")));
    return retval;
}


// =========================================================================
BOOL contextMgrDevice_Deinit()
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrDevice_Deinit\n")));

    if (G_contextMgrDeviceInitialized)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Deinitializing...\n")));
        G_contextMgrDeviceLastHandle = 1;

        G_contextMgrForceDismounts = FALSE;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to delete critical section: 0x%0.8x\n"), &G_contextMgrDeviceCriticalSection));
        DeleteCriticalSection(&G_contextMgrDeviceCriticalSection);

        G_contextMgrDeviceDeviceList = NULL;
                
        G_contextMgrDeviceInitialized = FALSE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrDevice_Deinit\n")));
    return retval;
}


// =========================================================================
// Returns the device context, or NULL if the volume is no longer valid
// (e.g. handle passed in relates to a dismounted volume/volume in the
// process of being dismounted)
DEVICE_LIST_ITEM* contextMgrDevice_GetDevice(DWORD hDevice, DEVICE_CONTEXT** devContext)
{
    DEVICE_LIST_ITEM* currListItem = NULL;
    DEVICE_LIST_ITEM* retval = NULL;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(DH: %d) contextMgrDevice_GetDevice\n"), hDevice));

    *devContext = NULL;

    if (!(G_contextMgrDeviceInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(DH: %d) Device mgr list not initialized\n"), hDevice));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL About to...\n"), hDevice));
        EnterCriticalSection(&G_contextMgrDeviceCriticalSection);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL ...Done.\n"), hDevice));
        currListItem = G_contextMgrDeviceDeviceList;
        while (currListItem != NULL)
            {
            if (currListItem->hDevice == hDevice)
                {
                if (
                    (!(currListItem->DismountPending)) &&
                    (currListItem->DeviceContext != NULL) 
                   )
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DI About to...\n"), hDevice));
                    EnterCriticalSection(&(currListItem->CriticalSection));
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DI ...Done.\n"), hDevice));
                    if (
                        (!(currListItem->DismountPending)) &&
                        (currListItem->DeviceContext != NULL) 
                       )
                        {
                        // Still valid...
                        *devContext = currListItem->DeviceContext;
                        retval = currListItem;
                        }
                    else
                        {
                        // Dismount attempted during entering criticalsection
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI About to...\n"), hDevice));
                        LeaveCriticalSection(&(currListItem->CriticalSection));                   
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI ...Done.\n"), hDevice));
                        }
                    }

                break;
                }

            currListItem = currListItem->NextListItem;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DL About to...\n"), hDevice));
        LeaveCriticalSection(&G_contextMgrDeviceCriticalSection);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DL ...Done.\n"), hDevice));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(DH: %d) contextMgrDevice_GetDevice\n"), hDevice));
    return retval;
}


// =========================================================================
// Note: Can handle being passed NULL as listItem
void contextMgrDevice_ReleaseDevice(DEVICE_LIST_ITEM* listItem)
{
    if (listItem != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(DH: %d) contextMgrDevice_ReleaseDevice\n"), listItem->hDevice));

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI About to...\n"), listItem->hDevice));
        LeaveCriticalSection(&(listItem->CriticalSection));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI ...Done.\n"), listItem->hDevice));

        DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(DH: %d) contextMgrDevice_ReleaseDevice\n"), listItem->hDevice));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(DH: <NULL PTR PASSED IN>)\n")));
        DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(DH: <NULL PTR PASSED IN>)\n")));
        }

}


// =========================================================================
// Note: Device list critical section should be taken before calling this
//       function
DWORD contextMgrDevice_GetNextHandleDevice()
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrDevice_GetNextHandleDevice\n")));

    G_contextMgrDeviceLastHandle++;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrDevice_GetNextHandleDevice\n")));
    return G_contextMgrDeviceLastHandle;
}


// =========================================================================
DWORD contextMgrDevice_AddDevice(DEVICE_CONTEXT* devContext)
{
    DWORD retval = 0;
    DEVICE_LIST_ITEM* newListItem;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(DH: +) contextMgrDevice_AddDevice\n")));

    if (!(G_contextMgrDeviceInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(DH: +) Device mgr list not initialized\n")));
        }
    else
        {
        newListItem = malloc(sizeof(*newListItem));
        if (newListItem == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) Unable to malloc new device list item\n")));
            }
        else
            {
            InitializeCriticalSection(&(newListItem->CriticalSection));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) Setting device context on new device list item to: 0x%0.8x\n"), devContext));
            newListItem->DeviceContext = devContext;
            newListItem->DismountPending = FALSE;

            // Setup device handle and insert new device item into list
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) +++DL About to...\n")));
            EnterCriticalSection(&G_contextMgrDeviceCriticalSection);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) +++DL ...Done.\n")));
            newListItem->hDevice = contextMgrDevice_GetNextHandleDevice();
            // Insert at head of list...
            newListItem->NextListItem = G_contextMgrDeviceDeviceList;
            G_contextMgrDeviceDeviceList = newListItem;

            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) Device context: %0.8x\n"), newListItem->DeviceContext));            
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) Device handle : %0.8x\n"), newListItem->hDevice));

            retval = newListItem->hDevice;

            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) ---DL About to...\n")));
            LeaveCriticalSection(&G_contextMgrDeviceCriticalSection);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: +) ---DL ...Done.\n")));
            }
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(DH: +) contextMgrDevice_AddDevice\n")));
    return retval;
}


// =========================================================================
// This function removes the device indicated from the list of devices, and
// returns the device context of the device for the *caller* to overwrite
// and destroy
DEVICE_CONTEXT* contextMgrDevice_DeleteDevice(DWORD hDevice)
{
    DEVICE_CONTEXT* retval = NULL;
    DEVICE_LIST_ITEM* currListItem = NULL;
    DEVICE_LIST_ITEM* prevListItem = NULL;
    BOOL gotListCriticalSection;
    BOOL gotListItemCriticalSection;
    DWORD openHandleCount;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(DH: %d) contextMgrDevice_DeleteDevice\n"), hDevice));

    if (!(G_contextMgrDeviceInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(DH: %d) Device mgr list not initialized\n"), hDevice));
        }
    else
        {
        if (G_contextMgrForceDismounts)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) EMERGENCY.\n"), hDevice));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) Not an emergency.\n"), hDevice));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL About to...\n"), hDevice));
        if (G_contextMgrForceDismounts)
            {
            gotListCriticalSection = TryEnterCriticalSection(&G_contextMgrDeviceCriticalSection);
            }
        else
            {
            EnterCriticalSection(&G_contextMgrDeviceCriticalSection);
            gotListCriticalSection = TRUE;
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL ...Done.\n"), hDevice));

        currListItem = G_contextMgrDeviceDeviceList;
        prevListItem = NULL;
        while (currListItem != NULL)
            {
            if (currListItem->hDevice == hDevice)
                {
                // Unlink from list
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) Unlinking from list...\n"), hDevice));
                if (prevListItem == NULL)
                    {
                    G_contextMgrDeviceDeviceList = currListItem->NextListItem;
                    }
                else
                    {
                    prevListItem->NextListItem = currListItem->NextListItem;
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) List item unlinked.\n"), hDevice));

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL About to...\n"), hDevice));
                if (G_contextMgrForceDismounts)
                    {
                    gotListItemCriticalSection = TryEnterCriticalSection(&(currListItem->CriticalSection));
                    }
                else
                    {
                    EnterCriticalSection(&(currListItem->CriticalSection));
                    gotListItemCriticalSection = TRUE;
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) +++DL ...Done.\n"), hDevice));

                openHandleCount = contextMgrOpen_CountDeviceOpenContext(hDevice);
                if (
                    (!(G_contextMgrForceDismounts)) &&
                    (openHandleCount > 0)
                   )
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) CANNOT DISMOUNT; %d open handles on device\n"), hDevice, openHandleCount));
                    }
                else
                    {
                    currListItem->DismountPending = TRUE;
                    currListItem->hDevice = 0;
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) Corresponding device context: 0x%0.8x\n"), hDevice, currListItem->DeviceContext));
                    retval = currListItem->DeviceContext;
                    currListItem->DeviceContext = NULL;

                    // Purge any open handles from the device...
                    contextMgrOpen_DeleteDeviceOpenContext(hDevice);

                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI About to...\n"), hDevice));
                    if (gotListItemCriticalSection)
                        {
                        LeaveCriticalSection(&(currListItem->CriticalSection));
                        }
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DI ...Done.\n"), hDevice));

                    // Free off list item
                    DeleteCriticalSection(&(currListItem->CriticalSection));
                    SecZeroAndFreeMemory(currListItem, sizeof(currListItem));
                    }

                break;
                }

            prevListItem = currListItem; 
            currListItem = currListItem->NextListItem;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DL About to...\n"), hDevice));
        if (gotListCriticalSection)
            {
            LeaveCriticalSection(&G_contextMgrDeviceCriticalSection);
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(DH: %d) ---DL ...Done.\n"), hDevice));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(DH: %d) contextMgrDevice_DeleteDevice\n"), hDevice));
    return retval;
}


// =========================================================================
// =========================================================================

