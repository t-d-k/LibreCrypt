// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEDebug.h"
#include "FreeOTFEPlatform.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDADriverList.h"
#include "SDUGeneral.h"


// =========================================================================
BOOL AddDisabledDriver(LNKLIST_DISABLED_DRIVER** Head, WCHAR* driverName)
{
    BOOL allOK;
    LNKLIST_DISABLED_DRIVER** last;
    LNKLIST_DISABLED_DRIVER* new;

    allOK = TRUE;

    if (allOK)  
        {
        new = malloc(sizeof(LNKLIST_DISABLED_DRIVER));
        allOK = (new != NULL);
        }

    if (allOK)
        {
        new->Next = NULL;
        new->Filename = _wcsdup(SDUFilenameFromPathAndFilename(driverName));
        allOK = (new->Filename != NULL);
        }


    if (allOK)
        {
        last = Head;
        while (*last != NULL)
            {
            last = &((*last)->Next);
            }

        *last = new;
        }

    return allOK;
}


// =========================================================================
void RemoveDisabledDriver(LNKLIST_DISABLED_DRIVER** Head, WCHAR* driverName)
{
    LNKLIST_DISABLED_DRIVER** last;
    LNKLIST_DISABLED_DRIVER* found;

    last = Head;
    while (*last != NULL)
        {
        // Case insensitive comparison
        if (wcsicmp ((*last)->Filename, driverName) == 0)
            {
            // Unhook from linked list, and free off
            found = *last;
            *last = found->Next;
            FreeDisabledDriver(found);
            break;
            }

        last = &((*last)->Next);
        }

}


// =========================================================================
void FreeDisabledDriver(LNKLIST_DISABLED_DRIVER* item)
{
    if (item != NULL)
        {
        SecZeroAndFreeWCHARMemory(item->Filename);
        SecZeroMemory(item, sizeof(*item));
        }
}


// =========================================================================
void FreeAllDisabledDrivers(LNKLIST_DISABLED_DRIVER** Head)
{
    LNKLIST_DISABLED_DRIVER* curr;

    if (Head != NULL)
        {
        curr = *Head;
        while (curr != NULL)
            {
            *Head = curr->Next;
            FreeDisabledDriver(curr);
            curr = *Head;
            }
        }
}

// =========================================================================
int CountDisabledDrivers(LNKLIST_DISABLED_DRIVER* Head)
{
    LNKLIST_DISABLED_DRIVER* curr;
    int retval;

    retval = 0;

    curr = Head;
    while (curr != NULL)
        {
        retval++;
        curr = curr->Next;
        }

    return retval;
}


// =========================================================================
// Returns: TRUE if the driver is disabled
BOOL IsDriverDisabled(LNKLIST_DISABLED_DRIVER* Head, WCHAR* driverName)
{
    BOOL retval;
    LNKLIST_DISABLED_DRIVER* curr;
    WCHAR* driverFilename;

    retval = FALSE;

    driverFilename = SDUFilenameFromPathAndFilename(driverName);

    curr = Head;
    while (curr != NULL)
        {
        // Case insensitive comparison
        if (wcsicmp(curr->Filename, driverFilename) == 0)
            {
            retval = TRUE;
            break;
            }

        curr = curr->Next;
        }

    return retval;
}


// =========================================================================
// Returns NULL on failure; caller should compare return value with value passed in
LNKLIST_DISABLED_DRIVER* DeepCopyDisabledDrivers(LNKLIST_DISABLED_DRIVER* Head)
{
    LNKLIST_DISABLED_DRIVER* curr;
    LNKLIST_DISABLED_DRIVER* newHead;

    newHead = NULL;

    curr = Head;
    while (curr != NULL)
        {
        if (!(AddDisabledDriver(&newHead, curr->Filename)))
            {
            FreeAllDisabledDrivers(&newHead);
            break;
            }

        curr = curr->Next;
        }

    return newHead;
}


// =========================================================================

// =========================================================================
// =========================================================================

