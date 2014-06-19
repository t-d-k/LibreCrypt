// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#include "SDUEndianIntegers.h"


// =========================================================================
// Convert from big-endian to WORD
WORD SDUBigEndian16ToWORD(SDU_BIG_ENDIAN_16 bigEndian)
{
  return (
          (bigEndian.Data[0] * 0x0100) +
          (bigEndian.Data[1] * 0x0001)
         );
}

// =========================================================================
void SDUWORDToBigEndian16(WORD number, SDU_BIG_ENDIAN_16* bigEndian)
{
  bigEndian->Data[0] = ((number >>  8) & 0x00FF);
  bigEndian->Data[1] = (number & 0x00FF);
}

// =========================================================================
// Convert from big-endian to DWORD
DWORD SDUBigEndian32ToDWORD(SDU_BIG_ENDIAN_32 bigEndian)
{
  return (
          (bigEndian.Data[0] * 0x01000000) +
          (bigEndian.Data[1] * 0x00010000) +
          (bigEndian.Data[2] * 0x00000100) +
          (bigEndian.Data[3] * 0x00000001)
         );
}

// =========================================================================
void SDUDWORDToBigEndian32(DWORD number, SDU_BIG_ENDIAN_32* bigEndian)
{
  bigEndian->Data[0] = ((number >> 24) & 0x000000FF);
  bigEndian->Data[1] = ((number >> 16) & 0x000000FF);
  bigEndian->Data[2] = ((number >>  8) & 0x000000FF);
  bigEndian->Data[3] = (number & 0x000000FF);
}


// =========================================================================
// Convert from little-endian to WORD
WORD SDULittleEndian16ToWORD(SDU_LITTLE_ENDIAN_16 littleEndian)
{
  return (
          (littleEndian.Data[0] * 0x0100) +
          (littleEndian.Data[1] * 0x0001)
         );
}

// =========================================================================
void SDUWORDToLittleEndian16(WORD number, SDU_LITTLE_ENDIAN_16* littleEndian)
{
  littleEndian->Data[0] = ((number >>  8) & 0x00FF);
  littleEndian->Data[1] = (number & 0x00FF);
}

// =========================================================================
// Convert from little-endian to DWORD
DWORD SDULittleEndian32ToDWORD(SDU_LITTLE_ENDIAN_32 littleEndian)
{
  return (
          (littleEndian.Data[0] * 0x01000000) +
          (littleEndian.Data[1] * 0x00010000) +
          (littleEndian.Data[2] * 0x00000100) +
          (littleEndian.Data[3] * 0x00000001)
         );
}

// =========================================================================
void SDUDWORDToLittleEndian32(DWORD number, SDU_LITTLE_ENDIAN_32* littleEndian)
{
  littleEndian->Data[0] = ((number >> 24) & 0x000000FF);
  littleEndian->Data[1] = ((number >> 16) & 0x000000FF);
  littleEndian->Data[2] = ((number >>  8) & 0x000000FF);
  littleEndian->Data[3] = (number & 0x000000FF);
}

// =========================================================================
WORD SDULittleToBigEndianWORD(WORD littleEndian)
{
    SDU_LITTLE_ENDIAN_16 tmpEndianLittle;
    SDU_BIG_ENDIAN_16 tmpEndianBig;

    SDUWORDToLittleEndian16(littleEndian, &tmpEndianLittle);
    tmpEndianBig.Data[0] = tmpEndianLittle.Data[1];
    tmpEndianBig.Data[1] = tmpEndianLittle.Data[0];
    return SDUBigEndian16ToWORD(tmpEndianBig);
}

WORD SDUBigToLittleEndianWORD(WORD bigEndian)
{
    SDU_LITTLE_ENDIAN_16 tmpEndianLittle;
    SDU_BIG_ENDIAN_16 tmpEndianBig;

    SDUWORDToBigEndian16(bigEndian, &tmpEndianBig);
    tmpEndianLittle.Data[0] = tmpEndianBig.Data[1];
    tmpEndianLittle.Data[1] = tmpEndianBig.Data[0];
    return SDULittleEndian16ToWORD(tmpEndianLittle);
}

DWORD SDULittleToBigEndianDWORD(DWORD littleEndian)
{
    SDU_LITTLE_ENDIAN_32 tmpEndianLittle;
    SDU_BIG_ENDIAN_32 tmpEndianBig;

    SDUDWORDToLittleEndian32(littleEndian, &tmpEndianLittle);
    tmpEndianBig.Data[0] = tmpEndianLittle.Data[3];
    tmpEndianBig.Data[1] = tmpEndianLittle.Data[2];
    tmpEndianBig.Data[2] = tmpEndianLittle.Data[1];
    tmpEndianBig.Data[3] = tmpEndianLittle.Data[0];
    return SDUBigEndian32ToDWORD(tmpEndianBig);
}

DWORD SDUBigToLittleEndianDWORD(DWORD bigEndian)
{
    SDU_LITTLE_ENDIAN_32 tmpEndianLittle;
    SDU_BIG_ENDIAN_32 tmpEndianBig;

    SDUDWORDToBigEndian32(bigEndian, &tmpEndianBig);
    tmpEndianLittle.Data[0] = tmpEndianBig.Data[3];
    tmpEndianLittle.Data[1] = tmpEndianBig.Data[2];
    tmpEndianLittle.Data[2] = tmpEndianBig.Data[1];
    tmpEndianLittle.Data[3] = tmpEndianBig.Data[0];
    return SDULittleEndian32ToDWORD(tmpEndianLittle);
}

// =========================================================================
// =========================================================================
