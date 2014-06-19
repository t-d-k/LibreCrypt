unit OTFEBestCryptBytesToString_U;
// Description: Transforms bytes array to human readable form string
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// Basically, a port of Jetico's "byte2string.cpp" file - contact Jetico
// for further detail/a copy of this file, which at time of writing, does
// not appear in the BestCrypt BDK
//


interface

uses
  HashValue_U,
  HashAlg_U;

function  SE_EncodeString(var str: string; var written: integer; bf: THashArray; bfSize: integer): boolean;
function  SE_DecodeString(var buffer: THashArray; var written: integer; var str: string): boolean;

implementation

const
  // SE_bitsForDecoding by char from codeTable[ 1 << SE_bitsForDecoding ]
  SE_bitsForDecoding = 5;

  codeTable: array [0..32] of char = (
                                      '1','2','3','4','5','6','7','8','9','Q',
                                      'W','E','R','T','Y','P','A','S','D','F',
                                      'G','H','J','K','L','Z','X','C','V','B',
                                      'N','M',
                                      #0 // Add in an extra #0 as getCode(...)
                                         // uses this to tell if it's run off
                                         // the end of the array
                                     );


// Forward function/procedure definitions
procedure setBits(var buff: THashArray; bitStart: integer; value: integer; valueBitSize: integer); forward;
function  getBits(buff: THashArray; bitStart: integer; valueBitSize: integer): integer; forward;
function  extend(var extBuff: string; var written: integer; buff: THashArray; buffSize: integer): boolean; forward;
function  getCode(symbol: char): integer; forward;
function  compact(var buff: THashArray; var written: integer; const extBuff: string; extBuffSize: integer): boolean; forward;


// Function/procedure implementations


procedure setBits(var buff: THashArray; bitStart: integer; value: integer; valueBitSize: integer);
var
  leftPos, rightPos: integer;
  byteNum, bitNum: integer;
  mask: integer;
  tmp: integer;
begin
  assert(valueBitSize <= 8);

  byteNum := (bitStart div 8);
  bitNum := (bitStart mod 8);

  // left = buff + byteNum;
  leftPos := byteNum;
  // right = left + 1;
  rightPos := leftPos + 1;

  mask := $FF shr (8 - valueBitSize);

  value := value and mask;

  tmp := buff[leftPos] or ( buff[rightPos] shl 8 );

  tmp := tmp and  (not( mask shl bitNum ));
  tmp := tmp or (value shl bitNum);

  buff[leftPos] := byte(tmp);
  buff[rightPos] := byte(( tmp shr 8 ));
end;



function getBits(buff: THashArray; bitStart: integer; valueBitSize: integer): integer;
var
  byteNum, bitNum: integer;
  left, right: byte;
  mask: byte;
  value: integer;
begin
  assert(valueBitSize<=8);

  byteNum := (bitStart div 8);
  bitNum := (bitStart mod 8);

  left := buff[byteNum];
  right := buff[byteNum+1];

  mask := $FF shr (8 - valueBitSize);

  value := ( ( ( left or ( right shl 8) ) shr bitNum ) and mask );

  Result:= value;

end;


// extBuff - will be set to the coded version of the hash
// written - this will be set to the length of extBuff
// buff - set this to the hash digest to be coded
// buffSize - set this to the digest size in bytes
function extend(var extBuff: string; var written: integer; buff: THashArray; buffSize: integer): boolean;
var
  last: integer;
  bit: integer;
  value: integer;
begin
  last := ((buffSize * 8) div SE_bitsForDecoding) + 1;

  // increment last; required as the original code was written in C, which
  // indexes from 0. Delphi indexes from 1
  inc(last);

  if (last > written) then
    begin
    written := last;
    Result := FALSE;
    exit;
    end;

  dec(last);

  // The BestCrypt C version of this function sets #0 to be the end of the
  // coded hash.
  // We just initialize the buffer to contain the required number of chars
  //  extBuff[last] := #0; // EOF.
  extBuff := StringOfChar('.', last-1);

  bit := 0;
  while (bit<buffSize * 8) do
    begin
    value := getBits(buff, bit, SE_bitsForDecoding);
    if ( (value > sizeof(codeTable)) or
         (value < 0 ) ) then
      begin
      Result := FALSE;
      exit;
      end;

    dec(last);
    extBuff[last] := codeTable[value];

    inc(bit, SE_bitsForDecoding);
    end;

  // This loop terminates at 1, as Delphi indexes from 1, as opposed to 0,
  // which C indexes from
  while(last<>1) do
    begin
    dec(last);
    extBuff[last] := codeTable[0];
    end;

  Result:= TRUE;
end;


function getCode(symbol: char): integer;
var
  ch: char;
  i: integer;
begin
  i := 0;
  while (TRUE) do
    begin
    ch := codeTable[i];
    if (ch=#0) then // Not found...
      begin
      break;
      end;

    if (ch=symbol) then // Found...
      begin
      Result := i;
      exit;
      end;

    inc(i);
    end;

  Result := -1;

end;


function compact(var buff: THashArray; var written: integer; const extBuff: string; extBuffSize: integer): boolean;
var
  bit: integer;
  last: integer;
  value: integer;
  sizeRequired: integer;
begin
  sizeRequired := ( extBuffSize * SE_bitsForDecoding) div 8;

  if (sizeRequired > written) then
    begin
    written := sizeRequired;
    Result := FALSE;
    exit;
    end;

  last := extBuffSize;

  bit := 0;
  while (bit < extBuffSize*5) do
    begin
    dec(last);
    value := getCode(extBuff[last+1]);

    if ( value = -1 ) then
      begin
      Result := FALSE;
      exit;
      end;

    setBits(buff, bit, value, SE_bitsForDecoding);

    inc(bit, SE_bitsForDecoding);
    end;

  Result := TRUE;
end;



function SE_EncodeString(var str: string; var written: integer; bf: THashArray; bfSize: integer): boolean;
begin
  Result := extend(str, written, bf, bfSize);

end;

function SE_DecodeString(var buffer: THashArray; var written: integer; var str: string): boolean;
begin
  Result := compact(buffer, written, str, length(str));

end;


END.

