unit HashAlgMD4Engine_U;
// Description: MD4 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


//  Delphi translation of the MD4 program by RSA Data Security, Inc., as specified in RFC 1320


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // MD4 context.
  MD4_CTX = packed record
    state: array [0..3] of DWORD;  // state (ABCD)
    count: array [0..1] of DWORD;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..63] of byte; // input buffer
  end;

type
  THashAlgMD4Engine = class(THashAlgEngine)
  private
    procedure MD4Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
    procedure Decode(var output: array of DWORD; var input: array of byte; len: cardinal);

  public
    procedure MD4Init(var context: MD4_CTX);
    procedure MD4Update(var context: MD4_CTX; const input: array of byte; const inputLen: cardinal);
    procedure MD4Final(digest: THashValue; var context: MD4_CTX);
  end;

implementation


// Constants for MD4Transform routine.
const S11: cardinal = 3;
const S12: cardinal = 7;
const S13: cardinal = 11;
const S14: cardinal = 19;
const S21: cardinal = 3;
const S22: cardinal = 5;
const S23: cardinal = 9;
const S24: cardinal = 13;
const S31: cardinal = 3;
const S32: cardinal = 9;
const S33: cardinal = 11;
const S34: cardinal = 15;


const PADDING: array [0..63] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

// F, G, H and I are basic MD4 functions.
function F(x, y, z: cardinal): cardinal;
begin
  Result := (((x) AND (y)) OR ((not(x)) AND (z)));
end;

function G(x, y, z: cardinal): cardinal;
begin
  Result := (((x) AND (y)) OR ((x) AND (z)) OR ((y) AND (z)));
end;

function H(x, y, z: cardinal): cardinal;
begin
  Result := ((x) XOR (y) XOR (z));
end;

// ROTATE_LEFT rotates x left n bits.
function ROTATE_LEFT(const x, n: cardinal): cardinal;
begin
  Result := (((x) shl (n)) OR ((x) shr (32-(n))))
end;

// FF, GG, and HH transformations for rounds 1, 2, an 3.
// Rotation is separate from addition to prevent recomputation.
procedure FF(var a: cardinal; const b, c, d, x, s: cardinal);
begin
 a := a + (F ((b), (c), (d)) + (x));
 a := ROTATE_LEFT ((a), (s));
end;

procedure GG(var a: cardinal; const  b, c, d, x, s: cardinal);
begin
 a := a + (G ((b), (c), (d)) + (x) + ($5a827999));
 a := ROTATE_LEFT ((a), (s));
end;

procedure HH(var a: cardinal; const  b, c, d, x, s: cardinal);
begin
 a := a + (H ((b), (c), (d)) + (x) + ($6ed9eba1));
 a := ROTATE_LEFT ((a), (s));
end;

// MD4 initialization. Begins an MD4 operation, writing a new context.
procedure THashAlgMD4Engine.MD4Init (var context: MD4_CTX);
begin
  context.count[0] := 0;
  context.count[1] := 0;

  // Load magic initialization constants.
  context.state[0] := $67452301;
  context.state[1] := $efcdab89;
  context.state[2] := $98badcfe;
  context.state[3] := $10325476;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));
end;

// MD4 block update operation. Continues an MD4 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgMD4Engine.MD4Update(var context: MD4_CTX; const input: array of byte; const inputLen: cardinal);
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
    MD4Transform(context.state, context.buffer, 0);

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      MD4Transform(context.state, input, i);
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

// MD4 finalization. Ends an MD4 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgMD4Engine.MD4Final(digest: THashValue; var context: MD4_CTX);
var
  bits: array [0..7] of byte;
  index: cardinal;
  padLen: cardinal;
  tmpArr: THashArray;
begin
  // Save number of bits
  Encode(bits, context.count, 8);

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
  MD4Update(context, PADDING, padLen);

  // Append length (before padding)
  MD4Update(context, bits, 8);

  // Store state in digest
  Encode(tmpArr, context.state, 16);
  digest.SetValueAsArray(tmpArr, (16 * 8));

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// MD4 basic transformation. Transforms state based on block.
procedure THashAlgMD4Engine.MD4Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
var
  a, b, c, d: DWORD;
  x: array [0..15] of DWORD;
  i: integer;
begin
  a := state[0];
  b := state[1];
  c := state[2];
  d := state[3];

  Decode(x, block[startOffsetInBlock], 64);

  // Round 1
  FF (a, b, c, d, x[ 0], S11); // 1
  FF (d, a, b, c, x[ 1], S12); // 2
  FF (c, d, a, b, x[ 2], S13); // 3
  FF (b, c, d, a, x[ 3], S14); // 4
  FF (a, b, c, d, x[ 4], S11); // 5
  FF (d, a, b, c, x[ 5], S12); // 6
  FF (c, d, a, b, x[ 6], S13); // 7
  FF (b, c, d, a, x[ 7], S14); // 8
  FF (a, b, c, d, x[ 8], S11); // 9
  FF (d, a, b, c, x[ 9], S12); // 10
  FF (c, d, a, b, x[10], S13); // 11
  FF (b, c, d, a, x[11], S14); // 12
  FF (a, b, c, d, x[12], S11); // 13
  FF (d, a, b, c, x[13], S12); // 14
  FF (c, d, a, b, x[14], S13); // 15
  FF (b, c, d, a, x[15], S14); // 16

  // Round 2
  GG (a, b, c, d, x[ 0], S21); // 17
  GG (d, a, b, c, x[ 4], S22); // 18
  GG (c, d, a, b, x[ 8], S23); // 19
  GG (b, c, d, a, x[12], S24); // 20
  GG (a, b, c, d, x[ 1], S21); // 21
  GG (d, a, b, c, x[ 5], S22); // 22
  GG (c, d, a, b, x[ 9], S23); // 23
  GG (b, c, d, a, x[13], S24); // 24
  GG (a, b, c, d, x[ 2], S21); // 25
  GG (d, a, b, c, x[ 6], S22); // 26
  GG (c, d, a, b, x[10], S23); // 27
  GG (b, c, d, a, x[14], S24); // 28
  GG (a, b, c, d, x[ 3], S21); // 29
  GG (d, a, b, c, x[ 7], S22); // 30
  GG (c, d, a, b, x[11], S23); // 31
  GG (b, c, d, a, x[15], S24); // 32

  // Round 3
  HH (a, b, c, d, x[ 0], S31); // 33
  HH (d, a, b, c, x[ 8], S32); // 34
  HH (c, d, a, b, x[ 4], S33); // 35
  HH (b, c, d, a, x[12], S34); // 36
  HH (a, b, c, d, x[ 2], S31); // 37
  HH (d, a, b, c, x[10], S32); // 38
  HH (c, d, a, b, x[ 6], S33); // 39
  HH (b, c, d, a, x[14], S34); // 40
  HH (a, b, c, d, x[ 1], S31); // 41
  HH (d, a, b, c, x[ 9], S32); // 42
  HH (c, d, a, b, x[ 5], S33); // 43
  HH (b, c, d, a, x[13], S34); // 44
  HH (a, b, c, d, x[ 3], S31); // 45
  HH (d, a, b, c, x[11], S32); // 46
  HH (c, d, a, b, x[ 7], S33); // 47
  HH (b, c, d, a, x[15], S34); // 48

  state[0] := state[0] + a;
  state[1] := state[1] + b;
  state[2] := state[2] + c;
  state[3] := state[3] + d;

  // Zeroize sensitive information.
//  HE_memset(x, 0, 0, Length(x)+1);
//xxx
  for i:=0 to 15 do
    begin
    x[i] := 0;
    end;

end;

// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 4.
procedure THashAlgMD4Engine.Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    output[j]   := (input[i] AND $ff);
    output[j+1] := ((input[i] shr 8) AND $ff);
    output[j+2] := ((input[i] shr 16) AND $ff);
    output[j+3] := ((input[i] shr 24) AND $ff);
    inc(i);
    inc(j, 4);
    end;

end;

// Decodes input (unsigned char) into output (UINT4). Assumes len is
// a multiple of 4.
procedure THashAlgMD4Engine.Decode(var output: array of DWORD; var input: array of byte; len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    output[i] := (input[j]) OR ((input[j+1]) shl 8) OR ((input[j+2]) shl 16) OR ((input[j+3]) shl 24);
    inc(i);
    inc(j, 4);
    end;

end;


END.


