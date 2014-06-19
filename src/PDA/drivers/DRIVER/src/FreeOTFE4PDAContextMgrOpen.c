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
#include "FreeOTFE4PDAContextMgrOpen.h"
#include "FreeOTFEDebug.h"
#include "SDUGeneral.h"
#include "FreeOTFElib.h"


// =========================================================================
// Default globals...

BOOL              G_contextMgrOpenInitialized;
DWORD             G_contextMgrOpenLastHandle;
CRITICAL_SECTION  G_contextMgrOpenCriticalSection;
OPEN_LIST_ITEM*   G_contextMgrOpenDeviceList;


// =========================================================================
BOOL contextMgrOpen_Init()
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrOpen_Init\n")));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Initializing...\n")));
        G_contextMgrOpenLastHandle = 1;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to initialize critical section: 0x%0.8x\n"), &G_contextMgrOpenCriticalSection));
        InitializeCriticalSection(&G_contextMgrOpenCriticalSection);

        // Note that we defer creating the actual list until a device is added
        // to it; this reduces the load/initialization time for the DLL
        G_contextMgrOpenDeviceList = NULL;
                
        G_contextMgrOpenInitialized = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrOpen_Init\n")));
    return retval;
}


// =========================================================================
BOOL contextMgrOpen_Deinit()
{
    BOOL retval = TRUE;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrOpen_Deinit\n")));

    if (G_contextMgrOpenInitialized)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("Deinitializing...\n")));
        G_contextMgrOpenLastHandle = 1;

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("About to delete critical section: 0x%0.8x\n"), &G_contextMgrOpenCriticalSection));
        DeleteCriticalSection(&G_contextMgrOpenCriticalSection);

        G_contextMgrOpenDeviceList = NULL;
                
        G_contextMgrOpenInitialized = TRUE;
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrOpen_Deinit\n")));
    return retval;
}


// =========================================================================
// Returns the device context, or NULL if the volume is no longer valid
// (e.g. handle passed in relates to a dismounted volume/volume in the
// process of being dismounted)
OPEN_LIST_ITEM* contextMgrOpen_GetOpenContext(DWORD hOpen)
{
    OPEN_LIST_ITEM* retval = NULL;
    OPEN_LIST_ITEM* currListItem;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(OH: %d) contextMgrOpen_GetOpenContext\n"), hOpen));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(OH: %d) Device mgr list not initialized\n"), hOpen));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OL About to...\n"), hOpen));
        EnterCriticalSection(&G_contextMgrOpenCriticalSection);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OL ...Done.\n"), hOpen));
        currListItem = G_contextMgrOpenDeviceList;
        while (currListItem != NULL)
            {
            if (currListItem->hOpen == hOpen)
                {
                if (currListItem->hDevice != 0)
                    {
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OI About to...\n"), hOpen));
                    EnterCriticalSection(&(currListItem->CriticalSection));
                    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OI ...Done.\n"), hOpen));
                    if (currListItem->hDevice != 0)
                        {
                        // Still valid...
                        retval = currListItem;
                        }
                    else
                        {
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI About to...\n"), hOpen));
                        LeaveCriticalSection(&(currListItem->CriticalSection));                   
                        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI ...Done.\n"), hOpen));
                        // Dismount attempted during entering criticalsection
                        }
                    }

                break;
                }

            currListItem = currListItem->NextListItem;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OL About to...\n"), hOpen));
        LeaveCriticalSection(&G_contextMgrOpenCriticalSection);                   
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OL ...Done.\n"), hOpen));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(OH: %d) contextMgrOpen_GetOpenContext\n"), hOpen));
    return retval;
}


// =========================================================================
// Note: Can handle being passed NULL as listItem
void contextMgrOpen_ReleaseOpenContext(OPEN_LIST_ITEM* listItem)
{
    if (listItem != NULL)
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(OH: %d) contextMgrOpen_ReleaseOpenContext\n"), listItem->hOpen));

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI About to...\n"), listItem->hOpen));
        LeaveCriticalSection(&(listItem->CriticalSection));                   
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI ...Done.\n"), listItem->hOpen));

        DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(OH: %d) contextMgrOpen_ReleaseOpenContext\n"), listItem->hOpen));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(OH: <NULL PTR PASSED IN>)\n")));
        DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(OH: <NULL PTR PASSED IN>)\n")));
        }

}


// =========================================================================
// Note: Device list critical section should be taken before calling this
//       function
DWORD contextMgrOpen_GetNextHandleDevice()
{
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("contextMgrOpen_GetNextHandleDevice\n")));

    G_contextMgrOpenLastHandle++;

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("contextMgrOpen_GetNextHandleDevice\n")));
    return G_contextMgrOpenLastHandle;
}


// =========================================================================
DWORD contextMgrOpen_AddOpenContext(DWORD hDevice)
{
    DWORD retval = 0;
    OPEN_LIST_ITEM* newListItem;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(OH: +) contextMgrOpen_AddOpenContext\n"), hDevice));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(OH: +) Device mgr list not initialized\n"), hDevice));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) AQ-1\n"), hDevice));
        newListItem = malloc(sizeof(*newListItem));
        if (newListItem == NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) Unable to malloc new open list item\n"), hDevice));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) Setting up for new open list item\n"), hDevice));
            InitializeCriticalSection(&(newListItem->CriticalSection));
            newListItem->hDevice = hDevice;
            newListItem->Position.QuadPart = 0;

            // Setup device handle and insert new device item into list
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) +++OL About to...\n")));
            EnterCriticalSection(&G_contextMgrOpenCriticalSection);
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) +++OL ...Done.\n")));
            newListItem->hOpen = contextMgrOpen_GetNextHandleDevice();
            // Insert at head of list...
            newListItem->NextListItem = G_contextMgrOpenDeviceList;
            G_contextMgrOpenDeviceList = newListItem;

            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) Open handle       : %0.8x\n"), hDevice, newListItem->hOpen));
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) Open device handle: %0.8x\n"), hDevice, newListItem->hDevice));

            retval = newListItem->hOpen;
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) AQ-3\n"), hDevice));

            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) ---OL About to...\n")));
            LeaveCriticalSection(&G_contextMgrOpenCriticalSection);                   
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) ---OL ...Done.\n")));
            }
        }
    DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: +) AQ-4\n"), hDevice));

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(OH: +) contextMgrOpen_AddOpenContext\n"), hDevice));
    return retval;
}


// =========================================================================
// This function removes the device indicated from the list of devices, and
// returns the device context of the device for the *caller* to overwrite
// and destroy
void contextMgrOpen_DeleteOpenContext(DWORD hOpen)
{
    OPEN_LIST_ITEM* currListItem = NULL;
    OPEN_LIST_ITEM* prevListItem = NULL;
    BOOL gotListCriticalSection;
    BOOL gotListItemCriticalSection;
    
    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(OH: %d) contextMgrOpen_DeleteOpenContext\n"), hOpen));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(OH: %d) Device mgr list not initialized\n"), hOpen));
        }
    else
        {
        if (G_contextMgrForceDismounts)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) EMERGENCY.\n"), hOpen));
            }
        else
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) Not an emergency.\n"), hOpen));
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-1\n"), hOpen));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OL About to...\n"), hOpen));
        if (G_contextMgrForceDismounts)
            {
            gotListCriticalSection = TryEnterCriticalSection(&G_contextMgrOpenCriticalSection);
            }
        else
            {
            EnterCriticalSection(&G_contextMgrOpenCriticalSection);
            gotListCriticalSection = TRUE;
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OL ...Done..\n"), hOpen));
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-2\n"), hOpen));

        currListItem = G_contextMgrOpenDeviceList;
        prevListItem = NULL;
        while (currListItem != NULL)
            {
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-3.\n"), hOpen));
            if (currListItem->hOpen == hOpen)
                {
                // Unlink from list
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) Unlinking from list...\n"), hOpen));
                if (prevListItem == NULL)
                    {
                    G_contextMgrOpenDeviceList = currListItem->NextListItem;
                    }
                else
                    {
                    prevListItem->NextListItem = currListItem->NextListItem;
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) List item unlinked.\n"), hOpen));

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OI About to...\n"), hOpen));
                if (G_contextMgrForceDismounts)
                    {
                    gotListItemCriticalSection = TryEnterCriticalSection(&(currListItem->CriticalSection));
                    }
                else
                    {
                    EnterCriticalSection(&(currListItem->CriticalSection));
                    gotListItemCriticalSection = TRUE;
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) +++OI ...Done.\n"), hOpen));

                currListItem->hOpen = 0;
                currListItem->hDevice = 0;

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-3.3\n"), hOpen));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI About to...\n"), hOpen));
                if (gotListItemCriticalSection)
                    {
                    LeaveCriticalSection(&(currListItem->CriticalSection));
                    }
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OI ...Done.\n"), hOpen));
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-3.4\n"), hOpen));

                // Free off list item
                DeleteCriticalSection(&(currListItem->CriticalSection));
                SecZeroAndFreeMemory(currListItem, sizeof(currListItem));

                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-3.5\n"), hOpen));
                break;
                }
            DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-4.\n"), hOpen));

            prevListItem = currListItem; 
            currListItem = currListItem->NextListItem;
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) AZ-5.\n"), hOpen));

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OL About to...\n"), hOpen));
        if (gotListCriticalSection)
            {
            LeaveCriticalSection(&G_contextMgrOpenCriticalSection);
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(OH: %d) ---OL ...Done.\n"), hOpen));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(OH: %d) contextMgrOpen_DeleteOpenContext\n"), hOpen));
}

// =========================================================================
DWORD contextMgrOpen_CountDeviceOpenContext(DWORD hDevice)
{
    OPEN_LIST_ITEM* currListItem;
    DWORD retval = 0;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(ODH: %d) contextMgrOpen_CountDeviceOpenContext\n"), hDevice));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(ODH: %d) Device mgr list not initialized\n"), hDevice));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) +++OL About to...\n"), hDevice));
        EnterCriticalSection(&G_contextMgrOpenCriticalSection);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) +++OL ...Done.\n"), hDevice));
        currListItem = G_contextMgrOpenDeviceList;
        while (currListItem != NULL)
            {
            if (currListItem->hDevice == hDevice)
                {
                retval++;
                }

            currListItem = currListItem->NextListItem;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) ---OL About to...\n"), hDevice));
        LeaveCriticalSection(&G_contextMgrOpenCriticalSection);
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) ---OL ...Done.\n"), hDevice));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(ODH: %d) contextMgrOpen_CountDeviceOpenContext\n"), hDevice));
    return retval;
}



// =========================================================================
// Destroy all open handles for the specified device
void contextMgrOpen_DeleteDeviceOpenContext(DWORD hDevice)
{
    OPEN_LIST_ITEM* currListItem;
    OPEN_LIST_ITEM* nextListItem;
    BOOL gotListCriticalSection;

    DEBUGOUTMAINDRV(DEBUGLEV_ENTER, (TEXT("(ODH: %d) contextMgrOpen_DeleteDeviceOpenContext\n"), hDevice));

    if (!(G_contextMgrOpenInitialized))
        {
        DEBUGOUTMAINDRV(DEBUGLEV_ERROR, (TEXT("(ODH: %d) Device mgr list not initialized\n"), hDevice));
        }
    else
        {
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) +++OL About to...\n"), hDevice));
        if (G_contextMgrForceDismounts)
            {
            gotListCriticalSection = TryEnterCriticalSection(&G_contextMgrOpenCriticalSection);
            }
        else
            {
            EnterCriticalSection(&G_contextMgrOpenCriticalSection);
            gotListCriticalSection = TRUE;
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) +++OL ...Done.\n"), hDevice));
        currListItem = G_contextMgrOpenDeviceList;
        while (currListItem != NULL)
            {
            // Store the next one on the list; otherwise this information will
            // be overwritten if the current one is removed
            nextListItem = currListItem->NextListItem;

            if (currListItem->hDevice == hDevice)
                {
                // Note that even though 
                // contextMgrOpen_DeleteOpenContext(...) enters the list's
                // critical section, this won't block as it's still the same
                // thread as this function is called in
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) Calling contextMgrOpen_DeleteOpenContext...\n"), hDevice));
                contextMgrOpen_DeleteOpenContext(currListItem->hOpen);
                DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) Finished calling contextMgrOpen_DeleteOpenContext\n"), hDevice));
                }

            currListItem = nextListItem;
            }

        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) ---OL About to...\n"), hDevice));
        if (gotListCriticalSection)
            {
            LeaveCriticalSection(&G_contextMgrOpenCriticalSection);
            }
        DEBUGOUTMAINDRV(DEBUGLEV_INFO, (TEXT("(ODH: %d) ---OL ...Done.\n"), hDevice));
        }

    DEBUGOUTMAINDRV(DEBUGLEV_EXIT, (TEXT("(ODH: %d) contextMgrOpen_DeleteDeviceOpenContext\n"), hDevice));
}


// =========================================================================
// =========================================================================

