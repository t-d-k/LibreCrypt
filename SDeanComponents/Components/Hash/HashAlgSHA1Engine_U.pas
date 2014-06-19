unit HashAlgSHA1Engine_U;
// Description: SHA-1 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// Delphi translation of SHA-1, as defined in FIPS PUB 180-1 (17th April 1995)

// For details, see:
//   FIPS 180-1 - Secure Hash Standard
//   http://www.itl.nist.gov/fipspubs/fip180-1.htm

// The basic structure of this file, and the use of the object it describes, is
// based on the MD5 hash

// Note: The only difference between SHA and SHA1 is that SHA1 performs a
// ROTL(..., 1) to W[t], while SHA does not in SHA(1)Transform.

interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // SHA1 context.
  SHA1_CTX = packed record
    state: array [0..4] of DWORD;  // state (ABCD)
    count: array [0..1] of DWORD;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..63] of byte; // input buffer
  end;

type
  THashAlgSHA1Engine = class(THashAlgEngine)
  private
    function  GetK(t: integer): DWORD;
    function  f(B, C, D: DWORD; t: integer): DWORD;
    function  ROTL(x: DWORD; n: cardinal): DWORD;

    procedure SHA1Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
    procedure Decode(var output: array of DWORD; var input: array of byte; len: cardinal);

  public
    procedure SHA1Init(var context: SHA1_CTX);
    procedure SHA1Update(var context: SHA1_CTX; const input: array of byte; const inputLen: cardinal);
    procedure SHA1Final(digest: THashValue; var context: SHA1_CTX);
  end;

implementation

// Yes, the first value in this array is $80; the first bit after the data to
// be hashed (the padding) is "1"
const PADDING: array [0..63] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

// SHA1 initialization. Begins an SHA1 operation, writing a new context.
procedure THashAlgSHA1Engine.SHA1Init (var context: SHA1_CTX);
begin
  context.count[0] := 0;
  context.count[1] := 0;

  // Load magic initialization constants.
  context.state[0] := $67452301;
  context.state[1] := $EFCDAB89;
  context.state[2] := $98BADCFE;
  context.state[3] := $10325476;
  context.state[4] := $C3D2E1F0;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// SHA1 block update operation. Continues an SHA1 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgSHA1Engine.SHA1Update(var context: SHA1_CTX; const input: array of byte; const inputLen: cardinal);
var
  i, index, partLen: cardinal;
begin
  // Compute number of bytes mod 64
  index := ((context.count[0] shr 3) AND $3F);

  // Update number of bits
  context.count[0] := context.count[0] + (inputLen shl 3);

  if ((context.count[0]) < (inputLen shl 3)) then
    begin
    context.count[1] := context.count[1]+1;
    end;

  context.count[1] := context.count[1] + (inputLen shr 29);

  partLen := 64 - index;

  // Transform as many times as possible.
  if (inputLen >= partLen) then
    begin
    HE_memcpy(context.buffer, input, index, 0, partLen);
    SHA1Transform(context.state, context.buffer, 0);

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      SHA1Transform(context.state, input, i);
      inc(i, 64);
      end;

    index := 0;
    end
  else
    begin
    i := 0;
    end;

  // Buffer remaining input
  HE_memcpy(context.buffer, input, index, i, inputLen-i);

end;

// SHA1 finalization. Ends an SHA1 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgSHA1Engine.SHA1Final(digest: THashValue; var context: SHA1_CTX);
var
  bits: array [0..7] of byte;
  index: cardinal;
  padLen: cardinal;
  highByteFirst: array [0..1] of DWORD;
  tmpArr: THashArray;
begin
  // Unlike MD5, SHA-1 uses the high word first
  highByteFirst[0] := context.count[1];
  highByteFirst[1] := context.count[0];

  // Save number of bits
  Encode(bits, highByteFirst, 8);

  // Pad out to 56 mod 64.
  index := ((context.count[0] shr 3) AND $3f);
  if (index < 56) then
    begin
    padLen := (56 - index);
    end
  else
    begin
    padLen := (120 - index);
    end;

  SHA1Update(context, PADDING, padLen);

  // Append length (before padding)
  SHA1Update(context, bits, 8);

  // Store state in digest
  Encode(tmpArr, context.state, 20);
  digest.SetValueAsArray(tmpArr, (20 * 8));

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// SHA1 basic transformation. Transforms state based on block.
procedure THashAlgSHA1Engine.SHA1Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
var
  A, B, C, D, E: DWORD;
  W: array [0..79] of DWORD;
  TEMP: DWORD;
  i: integer;
  t: integer;
begin
  Decode(W, block[startOffsetInBlock], 64);

  for t:=16 to 79 do
    begin
    // Note: The only difference between SHA and SHA1 is that SHA1 performs a
    // ROTL(..., 1) to W[t], while SHA does not.
    W[t] := ROTL(W[t-3] XOR W[t-8] XOR W[t-14] XOR W[t-16], 1);
    end;

  A := state[0];
  B := state[1];
  C := state[2];
  D := state[3];
  E := state[4];


  for t:=0 to 79 do
    begin
    TEMP := ROTL(A, 5) + f(B,C,D, t) + E + W[t] + GetK(t);

    E := D;
    D := C;
    C := ROTL(B, 30);
    B := A;
    A := TEMP;
    end;

  state[0] := state[0] + A;
  state[1] := state[1] + B;
  state[2] := state[2] + C;
  state[3] := state[3] + D;
  state[4] := state[4] + E;

  // Zeroize sensitive information.
//  HE_memset(x, 0, 0, high(x)+1);
//xxx
  for i:=0 to 79 do
    begin
    W[i] := 0;
    end;

end;

// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 4.
procedure THashAlgSHA1Engine.Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    output[j+3] := (input[i] AND $ff);
    output[j+2] := ((input[i] shr 8) AND $ff);
    output[j+1] := ((input[i] shr 16) AND $ff);
    output[j+0] := ((input[i] shr 24) AND $ff);
    inc(i);
    inc(j, 4);
    end;

end;

// Decodes input (unsigned char) into output (UINT4). Assumes len is
// a multiple of 4.
procedure THashAlgSHA1Engine.Decode(var output: array of DWORD; var input: array of byte; len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    output[i] := (input[j+3]) OR ((input[j+2]) shl 8) OR ((input[j+1]) shl 16) OR ((input[j+0]) shl 24);
    inc(i);
    inc(j, 4);
    end;

end;


function THashAlgSHA1Engine.GetK(t: integer): DWORD;
begin
  if (0 <= t) AND (t <= 19) then
    begin
    Result := $5A827999;
    end
  else if (20 <= t) AND (t <= 39) then
    begin
    Result := $6ED9EBA1;
    end
  else if (40 <= t) AND (t <= 59) then
    begin
    Result := $8F1BBCDC;
    end
  else if (60 <= t) AND (t <= 79) then
    begin
    Result := $CA62C1D6;
    end
  else
    begin
    Result := 0;
    end;

end;

function THashAlgSHA1Engine.f(B, C, D: DWORD; t: integer): DWORD;
begin
  if (0 <= t) AND (t <= 19) then
    begin
    Result := (B AND C) OR ((NOT B) AND D);
    end
  else if (20 <= t) AND (t <= 39) then
    begin
    Result := B XOR C XOR D;
    end
  else if (40 <= t) AND (t <= 59) then
    begin
    Result := (B AND C) OR (B AND D) OR (C AND D);
    end
  else if (60 <= t) AND (t <= 79) then
    begin
    Result := B XOR C XOR D;
    end
  else
    begin
    Result := 0;
    end;

end;


// Rotate all bits, bits dropping off the end get wrapped round and reinserted
// onto the other end
function THashAlgSHA1Engine.ROTL(x: DWORD; n: cardinal): DWORD;
begin
  Result := (x shl n) OR (x shr (32-n));

end;

END.


