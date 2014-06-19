// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _SDULinkedList_H
#define _SDULinkedList_H1

#include <windows.h>


// =========================================================================
// Structures...

typedef struct _LNKLIST_ITEM LNKLIST_ITEM;
// _LNKLIST_ITEM struct defined in .c file


// =========================================================================
// Functions...

// Add item to linked list
BOOL SDULLAddItem(LNKLIST_ITEM** Head, void* Item);

// Get item by zero based index
BOOL SDULLGetItem(LNKLIST_ITEM* Head, unsigned int idx, void** Item);

// Set item by zero based index
// idx - Zero based index
BOOL SDULLSetItem(LNKLIST_ITEM* Head, unsigned int idx, void* Item);

// Remove item from linked list
// Note: Sets "Head" to NULL if removing the only item on the list
// Note: The item removed (i.e. passed in) will NOT be be free'd off
void SDULLRemoveItem(LNKLIST_ITEM** Head, void* Item);

// Remove all items from the linked list
// Note: Sets "Head" to NULL
// FreeItemsRemoved - If TRUE, free(...) will be called once on each of
//                    the items removed
void SDULLRemoveAllItems(LNKLIST_ITEM** Head, BOOL FreeItemsRemoved);

// Count items in linked list
int SDULLCountItems(LNKLIST_ITEM* Head);

// Copy a linked list. 
// Note: The item pointers in the copied list will be the same as the 
//       original list - to create a deep copy use:
//
//       LNKLIST_ITEM* newList;
//       void* origItem;
//       void* newItem;
//       int cnt;
//       int i;
//
//       newList = SDULLCopyLinkedList(myList);
//       cnt = SDULLCountItems(newList);
//       for(i = 0; i < cnt; i++)
//           {
//           SDULLGetItem(newList, i, &origItem);
//           newItem = <your routine to copy item>(origItem);
//           SDULLSetItem(newList, i, newItem);
//           }
//
// Returns NULL on failure
LNKLIST_ITEM* SDULLCopyLinkedList(LNKLIST_ITEM* Head);

// =========================================================================
// =========================================================================

#endif

