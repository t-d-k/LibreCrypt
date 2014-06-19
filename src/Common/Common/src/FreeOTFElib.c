// Description: Common FreeOTFE library functions
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEPlatform.h"

#include <stdio.h>
#ifdef FOTFE_PC_DLL
#include <stdlib.h> // Required for swprintf
#endif

#include "FreeOTFElib.h"
#include "FreeOTFEDebug.h"


// =========================================================================
// Convert the data passed in to it's ASCIIZ representation
// Note: outASCIIKey *must* be at least ((inKeyLength/4)+1) bytes long to store the ASCIIZ
//       representation of the key, including the terminating NULL
void ConvertDataToASCIIRep(
  unsigned int inKeyLength, // in *bits*
  FREEOTFEBYTE* inKeyData,
  char* outASCIIKey
)
{
    unsigned int i;
    unsigned int nibble;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("ConvertDataToASCIIRep\n")));

    for (i=0; i<(inKeyLength/8); i++)
        {
        nibble = (inKeyData[i] & 0xF0) >> 4;
        outASCIIKey[(i*2)] = _ConvertDataNibbleToASCIIChar(nibble);
        nibble = inKeyData[i] & 0x0F;
        outASCIIKey[((i*2)+1)] = _ConvertDataNibbleToASCIIChar(nibble);
        }

    // Add a terminating NULL
    outASCIIKey[(inKeyLength/4)]   = 0x00;

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("ConvertDataToASCIIRep\n")));
}


// =========================================================================
// Convert the specified nibble into a ASCII hex char
char _ConvertDataNibbleToASCIIChar(unsigned int nibble)
{
    char retval = 0x00;

    if (nibble<0x0A)
        {
        retval = (char)('0'+nibble);
        }
    else
        {
        retval = (char)('A'+nibble-0x0A);
        }

    return retval;
}


// =========================================================================
// Zero and free widestring memory
void SecZeroAndFreeWCHARMemory(
    IN WCHAR          *Memory
)
{
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("SecZeroAndFreeWCHARMemory\n")));

    if (Memory != NULL)
        {
        SecZeroAndFreeMemory(Memory, wcslen(Memory));
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("SecZeroAndFreeWCHARMemory\n")));
}


// =========================================================================
// Zero and free memory
void SecZeroAndFreeMemory(
    IN void          *Memory,
    IN unsigned int  Size
)
{
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("SecZeroAndFreeMemory\n")));

    if (Memory != NULL)
        {
        SecZeroMemory(Memory, Size);
        FREEOTFE_FREE(Memory);
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("SecZeroAndFreeMemory\n")));
}


// =========================================================================
// Overwrite and zero memory
void SecZeroWCHARMemory(
    IN void          *Memory
)
{
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("SecZeroWCHARMemory\n")));

    if (Memory != NULL)
        {
        SecZeroMemory(Memory, wcslen(Memory));
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("SecZeroWCHARMemory\n")));
}


// =========================================================================
// Overwrite and zero memory
void SecZerocharMemory(
    IN void          *Memory
)
{
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("SecZerocharMemory\n")));

    if (Memory != NULL)
        {
        SecZeroMemory(Memory, strlen(Memory));
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("SecZerocharMemory\n")));
}


// =========================================================================
// Overwrite and zero memory
void SecZeroMemory(
    IN void          *Memory,
    IN unsigned int  Size
)
{
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("SecZeroMemory\n")));

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Zeroing from: %lld\n"), Memory));
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Size        : %d\n"), Size));

    if (Memory != NULL)
        {
        memset(Memory, 0, Size);
        RtlZeroMemory(Memory, Size);
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("SecZeroMemory\n")));
}


// =========================================================================
// XOR block of memory
void XORBlock(
    IN   FREEOTFEBYTE*   InA,
    IN   FREEOTFEBYTE*   InB,
    OUT  FREEOTFEBYTE*   Out,
    IN   unsigned int    Size  // In Bytes
)
{
    unsigned int i;

    for (i=0; i<Size; i++)
        {
        Out[i] = InA[i] ^ InB[i];
        }

}


// =========================================================================
// Format a GUID as a WCHAR
// Note: *Caller* is responsible for freeing off "StringToGUID" after use
//#define GUID_FMT TEXT("{%0.8x-%0.4x-%0.4x-%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x}")
#define GUID_FMT L"{%0.8x-%0.4x-%0.4x-%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x%0.2x}"
BOOLEAN GUIDToWCHAR(
    IN   GUID* ptrGUID,
    OUT  WCHAR** StringGUID
)
{
    int stringSize;

    stringSize = (GUID_STR_LENGTH + 1) * sizeof(**StringGUID);
    *StringGUID = FREEOTFE_MEMALLOC(stringSize);
    if (*StringGUID != NULL)
        {
        _snwprintf(
                 *StringGUID, 
                 GUID_STR_LENGTH,
                 (WCHAR*)(GUID_FMT), 
                 ptrGUID->Data1,
                 ptrGUID->Data2,
                 ptrGUID->Data3,
                 ptrGUID->Data4[0],
                 ptrGUID->Data4[1],
                 ptrGUID->Data4[2],
                 ptrGUID->Data4[3],
                 ptrGUID->Data4[4],
                 ptrGUID->Data4[5],
                 ptrGUID->Data4[6],
                 ptrGUID->Data4[7]
                );
        }

    return (BOOLEAN)(*StringGUID != NULL);
}


// =========================================================================
// Function to return zero as a LARGE_INTEGER
LARGE_INTEGER ZeroLargeInteger()
{
    LARGE_INTEGER retval;
    retval.QuadPart = 0;
    return retval;
}


// =========================================================================
// =========================================================================


