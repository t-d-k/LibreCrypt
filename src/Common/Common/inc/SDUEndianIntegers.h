// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _SDUEndianIntegers_H
#define _SDUEndianIntegers_H   1

#include <windows.h>  // Required for DWORD

// =========================================================================
// Structs...

// 16 bit big endian numbers
typedef struct _SDU_BIG_ENDIAN_16 {
  BYTE Data[2];
} SDU_BIG_ENDIAN_16, *PSDU_BIG_ENDIAN_16;

// 32 bit big endian numbers
typedef struct _SDU_BIG_ENDIAN_32 {
  BYTE Data[4];
} SDU_BIG_ENDIAN_32, *PSDU_BIG_ENDIAN_32;

// 16 bit little endian numbers
typedef struct _SDU_LITTLE_ENDIAN_16 {
  BYTE Data[2];
} SDU_LITTLE_ENDIAN_16, *PSDU_LITTLE_ENDIAN_16;

// 32 bit little endian numbers
typedef struct _SDU_LITTLE_ENDIAN_32 {
  BYTE Data[4];
} SDU_LITTLE_ENDIAN_32, *PSDU_LITTLE_ENDIAN_32;



// =========================================================================
// Functions...

WORD SDUBigEndian16ToWORD(SDU_BIG_ENDIAN_16 bigEndian);
void SDUWORDToBigEndian16(WORD number, SDU_BIG_ENDIAN_16* bigEndian);
DWORD SDUBigEndian32ToDWORD(SDU_BIG_ENDIAN_32 bigEndian);
void SDUDWORDToBigEndian32(DWORD number, SDU_BIG_ENDIAN_32* bigEndian);

WORD SDULittleEndian16ToWORD(SDU_LITTLE_ENDIAN_16 littleEndian);
void SDUWORDToLittleEndian16(WORD number, SDU_LITTLE_ENDIAN_16* littleEndian);
DWORD SDULittleEndian32ToDWORD(SDU_LITTLE_ENDIAN_32 littleEndian);
void SDUDWORDToLittleEndian32(DWORD number, SDU_LITTLE_ENDIAN_32* littleEndian);


WORD SDULittleToBigEndianWORD(WORD littleEndian);
WORD SDUBigToLittleEndianWORD(WORD bigEndian);
DWORD SDULittleToBigEndianDWORD(DWORD littleEndian);
DWORD SDUBigToLittleEndianDWORD(DWORD bigEndian);

// =========================================================================
// =========================================================================

#endif

