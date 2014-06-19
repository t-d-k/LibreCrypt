// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"
#include "SDULinkedList.h"
#include "FreeOTFElib.h"


// =========================================================================
// LNKLIST_ITEM typedef defined in .h file
struct _LNKLIST_ITEM {
    void* Item;
    LNKLIST_ITEM* Next;
};


// =========================================================================
// Forward declarations...
void _SDULLFreeListItem(LNKLIST_ITEM* Item, BOOL FreeItemsItem);


// =========================================================================
// Add item to linked list
BOOL SDULLAddItem(LNKLIST_ITEM** Head, void* Item)
{
    BOOL allOK;
    LNKLIST_ITEM** last;
    LNKLIST_ITEM* new;

    allOK = TRUE;

    if (allOK)  
        {
        new = malloc(sizeof(LNKLIST_ITEM));
        allOK = (new != NULL);
        }

    if (allOK)
        {
        new->Next = NULL;
        new->Item = Item;
        allOK = (new->Item != NULL);
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
// Get item by zero based index
// idx - Zero based index
BOOL SDULLGetItem(LNKLIST_ITEM* Head, unsigned int idx, void** Item)
{
    LNKLIST_ITEM* curr;
    BOOL retval;
    int currIdx;

    retval = FALSE;
    currIdx = 0;

    curr = Head;
    while (curr != NULL)
        {
        if (currIdx == idx)
            {
            *Item = curr->Item;
            retval = TRUE;
            break;
            }

        curr = curr->Next;
        currIdx++;
        }

    return retval;
}


// =========================================================================
// Set item by zero based index
// idx - Zero based index
BOOL SDULLSetItem(LNKLIST_ITEM* Head, unsigned int idx, void* Item)
{
    LNKLIST_ITEM* curr;
    BOOL retval;
    int currIdx;

    retval = FALSE;
    currIdx = 0;

    curr = Head;
    while (curr != NULL)
        {
        if (currIdx == idx)
            {
            curr->Item = Item;
            retval = TRUE;
            break;
            }

        curr = curr->Next;
        currIdx++;
        }

    return retval;
}


// =========================================================================
// Remove item from linked list
// Note: The item removed (i.e. passed in) will NOT be be free'd off
void SDULLRemoveItem(LNKLIST_ITEM** Head, void* Item)
{
    LNKLIST_ITEM** last;
    LNKLIST_ITEM* found;

    last = Head;
    while (*last != NULL)
        {
        // Case insensitive comparison
        if ((*last)->Item == Item)
            {
            // Unhook from linked list, and free off
            found = *last;
            *last = found->Next;
            _SDULLFreeListItem(found, FALSE);
            break;
            }

        last = &((*last)->Next);
        }

}


// =========================================================================
void _SDULLFreeListItem(LNKLIST_ITEM* Item, BOOL FreeItemsItem)
{
    if (Item != NULL)
        {
        if ( 
            (FreeItemsItem) &&
            (Item->Item != NULL)
           )
            {
            free(Item->Item);
            }

        SecZeroAndFreeMemory(Item, sizeof(*Item));
        }
}


// =========================================================================
// Remove all items from the linked list
// FreeItemsRemoved - If TRUE, free(...) will be called once on each of
//                    the items removed
void SDULLRemoveAllItems(LNKLIST_ITEM** Head, BOOL FreeItemsRemoved)
{
    LNKLIST_ITEM* curr;

    if (Head != NULL)
        {
        curr = *Head;
        while (curr != NULL)
            {
            *Head = curr->Next;
            _SDULLFreeListItem(curr, FreeItemsRemoved);
            curr = *Head;
            }
        }
}


// =========================================================================
// Count items in linked list
int SDULLCountItems(LNKLIST_ITEM* Head)
{
    LNKLIST_ITEM* curr;
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
// Copy a linked list. 
// Note: The item pointers in the copy will be the same as the original
// Returns NULL on failure
LNKLIST_ITEM* SDULLCopyLinkedList(LNKLIST_ITEM* Head)
{
    LNKLIST_ITEM* curr;
    LNKLIST_ITEM* newHead;

    newHead = NULL;

    curr = Head;
    while (curr != NULL)
        {
        if (!(SDULLAddItem(&newHead, curr->Item)))
            {
            SDULLRemoveAllItems(&newHead, FALSE);
            break;
            }

        curr = curr->Next;
        }

    return newHead;
}


// =========================================================================

// =========================================================================
// =========================================================================

