// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFElib_H
#define _FreeOTFElib_H   1

#include "FreeOTFEPlatform.h"

#include <stdio.h>
#ifdef FOTFE_PDA
#include <windows.h>  // Required for WCHAR
#endif
#ifdef FOTFE_PC_DLL
#include <windows.h>  // Required for WCHAR
#endif
#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#include <ntdddisk.h>  // xxx -junk???
#endif


// =========================================================================
// Const definitions

// Length of the string representation of a GUID, in bytes
// The length of the string "{00000000-0000-0000-0000000000000000}"\0
//         i.e.              1234567890123456789012345678901234567  8
// WARNING: This omits any NULL terminator
#define GUID_STR_LENGTH  38
#define GUID_STRING_REP_CHAR_LENGTH   GUID_STR_LENGTH

// Length of the string representation of a GUID, in *bytes*
// WARNING: This omits any NULL terminator
#define GUID_STRING_REP_UNICODE_BYTE_LENGTH  (GUID_STR_LENGTH * sizeof(WCHAR))


// =========================================================================
// Function headers

// Convert the data passed in to it's ASCIIZ representation
// Note: outASCIIKey *must* be at least ((inKeyLength/4)+1) bytes long to store the ASCIIZ
//       representation of the key, including the terminating NULL
void ConvertDataToASCIIRep(
  unsigned int inKeyLength, // in Bits
  FREEOTFEBYTE* inKeyData,
  char* outASCIIKey
);


// Convert the specified nibble into a ASCII hex char
char _ConvertDataNibbleToASCIIChar(unsigned int nibble);


// Zero and free widestring memory
void SecZeroAndFreeWCHARMemory(
    IN WCHAR          *Memory
);

// Zero and free memory
void SecZeroAndFreeMemory(
    IN void          *Memory,
    IN unsigned int  Size
);

// Overwrite and zero memory
void SecZeroWCHARMemory(
    IN void          *Memory
);

// Overwrite and zero memory
void SecZerocharMemory(
    IN void          *Memory
);

// Overwrite and zero memory
void SecZeroMemory(
    IN void          *Memory,
    IN unsigned int  Size
);


// XOR block of memory
void XORBlock(
    IN   FREEOTFEBYTE*   InA,
    IN   FREEOTFEBYTE*   InB,
    OUT  FREEOTFEBYTE*   Out,
    IN   unsigned int    Size  // In Bytes
);

BOOLEAN GUIDToWCHAR(
    IN   GUID* ptrGUID,
    OUT  WCHAR** StringGUID
);

LARGE_INTEGER ZeroLargeInteger();


// =========================================================================
// =========================================================================

#endif

