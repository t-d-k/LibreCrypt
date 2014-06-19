unit HashAlgMD5Engine_U;
// Description: MD5 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


//  Delphi translation of the MD5 program by RSA Data Security, Inc., as specified in RFC 1321


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // MD5 context.
  MD5_CTX = packed record
    state: array [0..3] of DWORD;  // state (ABCD)
    count: array [0..1] of DWORD;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..63] of byte; // input buffer
  end;

type
  THashAlgMD5Engine = class(THashAlgEngine)
  private
    procedure MD5Transform(var state: array of DWORD; const block: array of byte; const startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; const input: array of DWORD; const len: cardinal);
    procedure Decode(var output: array of DWORD; const input: array of byte; const len: cardinal);

  public
    procedure MD5Init(var context: MD5_CTX);
    procedure MD5Update(var context: MD5_CTX; const input: array of byte; const inputLen: cardinal);
    procedure MD5Final(digest: THashValue; var context: MD5_CTX);
  end;

implementation


// Constants for MD5Transform routine.
const S11: cardinal = 7;
const S12: cardinal = 12;
const S13: cardinal = 17;
const S14: cardinal = 22;
const S21: cardinal = 5;
const S22: cardinal = 9;
const S23: cardinal = 14;
const S24: cardinal = 20;
const S31: cardinal = 4;
const S32: cardinal = 11;
const S33: cardinal = 16;
const S34: cardinal = 23;
const S41: cardinal = 6;
const S42: cardinal = 10;
const S43: cardinal = 15;
const S44: cardinal = 21;


const PADDING: array [0..63] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

// F, G, H and I are basic MD5 functions.
function F(const x, y, z: cardinal): cardinal;
begin
  Result := (((x) AND (y)) OR ((not(x)) AND (z)));
end;

function G(const x, y, z: cardinal): cardinal;
begin
  Result := (((x) AND (z)) OR ((y) AND (not(z))));
end;

function H(const x, y, z: cardinal): cardinal;
begin
  Result := ((x) XOR (y) XOR (z));
end;

function I(const x, y, z: cardinal): cardinal;
begin
  Result := ((y) XOR ((x) OR (not(z))));
end;

// ROTATE_LEFT rotates x left n bits.
function ROTATE_LEFT(const x, n: cardinal): cardinal;
begin
  Result := (((x) shl (n)) OR ((x) shr (32-(n))))
end;

// FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
// Rotation is separate from addition to prevent recomputation.
procedure FF(var a: cardinal; const b, c, d, x, s: cardinal; ac: DWORD);
begin
 a := a + (F ((b), (c), (d)) + (x) + (ac));
 a := ROTATE_LEFT ((a), (s));
 a := a + (b);
end;

procedure GG(var a: cardinal; const b, c, d, x, s: cardinal; ac: DWORD);
begin
 a := a + (G ((b), (c), (d)) + (x) + (ac));
 a := ROTATE_LEFT ((a), (s));
 a := a + (b);
end;

procedure HH(var a: cardinal; const b, c, d, x, s: cardinal; ac: DWORD);
begin
 a := a + (H ((b), (c), (d)) + (x) + (ac));
 a := ROTATE_LEFT ((a), (s));
 a := a + (b);
end;

procedure II(var a: cardinal; const b, c, d, x, s: cardinal; ac: DWORD);
begin
 a := a + (I ((b), (c), (d)) + (x) + (ac));
 a := ROTATE_LEFT ((a), (s));
 a := a + (b);
end;

// MD5 initialization. Begins an MD5 operation, writing a new context.
procedure THashAlgMD5Engine.MD5Init (var context: MD5_CTX);
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

// MD5 block update operation. Continues an MD5 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgMD5Engine.MD5Update(var context: MD5_CTX; const input: array of byte; const inputLen: cardinal);
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
    MD5Transform(context.state, context.buffer, 0);

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      MD5Transform(context.state, input, i);
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

// MD5 finalization. Ends an MD5 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgMD5Engine.MD5Final(digest: THashValue; var context: MD5_CTX);
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
  MD5Update(context, PADDING, padLen);

  // Append length (before padding)
  MD5Update(context, bits, 8);

  // Store state in digest
  Encode(tmpArr, context.state, 16);
  digest.SetValueAsArray(tmpArr, (16 * 8));

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// MD5 basic transformation. Transforms state based on block.
procedure THashAlgMD5Engine.MD5Transform(var state: array of DWORD; const block: array of byte; const startOffsetInBlock: cardinal);
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
  FF (a, b, c, d, x[ 0], S11, $d76aa478); // 1
  FF (d, a, b, c, x[ 1], S12, $e8c7b756); // 2
  FF (c, d, a, b, x[ 2], S13, $242070db); // 3
  FF (b, c, d, a, x[ 3], S14, $c1bdceee); // 4
  FF (a, b, c, d, x[ 4], S11, $f57c0faf); // 5
  FF (d, a, b, c, x[ 5], S12, $4787c62a); // 6
  FF (c, d, a, b, x[ 6], S13, $a8304613); // 7
  FF (b, c, d, a, x[ 7], S14, $fd469501); // 8
  FF (a, b, c, d, x[ 8], S11, $698098d8); // 9
  FF (d, a, b, c, x[ 9], S12, $8b44f7af); // 10
  FF (c, d, a, b, x[10], S13, $ffff5bb1); // 11
  FF (b, c, d, a, x[11], S14, $895cd7be); // 12
  FF (a, b, c, d, x[12], S11, $6b901122); // 13
  FF (d, a, b, c, x[13], S12, $fd987193); // 14
  FF (c, d, a, b, x[14], S13, $a679438e); // 15
  FF (b, c, d, a, x[15], S14, $49b40821); // 16

  // Round 2
  GG (a, b, c, d, x[ 1], S21, $f61e2562); // 17
  GG (d, a, b, c, x[ 6], S22, $c040b340); // 18
  GG (c, d, a, b, x[11], S23, $265e5a51); // 19
  GG (b, c, d, a, x[ 0], S24, $e9b6c7aa); // 20
  GG (a, b, c, d, x[ 5], S21, $d62f105d); // 21
  GG (d, a, b, c, x[10], S22,  $2441453); // 22
  GG (c, d, a, b, x[15], S23, $d8a1e681); // 23
  GG (b, c, d, a, x[ 4], S24, $e7d3fbc8); // 24
  GG (a, b, c, d, x[ 9], S21, $21e1cde6); // 25
  GG (d, a, b, c, x[14], S22, $c33707d6); // 26
  GG (c, d, a, b, x[ 3], S23, $f4d50d87); // 27
  GG (b, c, d, a, x[ 8], S24, $455a14ed); // 28
  GG (a, b, c, d, x[13], S21, $a9e3e905); // 29
  GG (d, a, b, c, x[ 2], S22, $fcefa3f8); // 30
  GG (c, d, a, b, x[ 7], S23, $676f02d9); // 31
  GG (b, c, d, a, x[12], S24, $8d2a4c8a); // 32

  // Round 3
  HH (a, b, c, d, x[ 5], S31, $fffa3942); // 33
  HH (d, a, b, c, x[ 8], S32, $8771f681); // 34
  HH (c, d, a, b, x[11], S33, $6d9d6122); // 35
  HH (b, c, d, a, x[14], S34, $fde5380c); // 36
  HH (a, b, c, d, x[ 1], S31, $a4beea44); // 37
  HH (d, a, b, c, x[ 4], S32, $4bdecfa9); // 38
  HH (c, d, a, b, x[ 7], S33, $f6bb4b60); // 39
  HH (b, c, d, a, x[10], S34, $bebfbc70); // 40
  HH (a, b, c, d, x[13], S31, $289b7ec6); // 41
  HH (d, a, b, c, x[ 0], S32, $eaa127fa); // 42
  HH (c, d, a, b, x[ 3], S33, $d4ef3085); // 43
  HH (b, c, d, a, x[ 6], S34,  $4881d05); // 44
  HH (a, b, c, d, x[ 9], S31, $d9d4d039); // 45
  HH (d, a, b, c, x[12], S32, $e6db99e5); // 46
  HH (c, d, a, b, x[15], S33, $1fa27cf8); // 47
  HH (b, c, d, a, x[ 2], S34, $c4ac5665); // 48

  // Round 4
  II (a, b, c, d, x[ 0], S41, $f4292244); // 49
  II (d, a, b, c, x[ 7], S42, $432aff97); // 50
  II (c, d, a, b, x[14], S43, $ab9423a7); // 51
  II (b, c, d, a, x[ 5], S44, $fc93a039); // 52
  II (a, b, c, d, x[12], S41, $655b59c3); // 53
  II (d, a, b, c, x[ 3], S42, $8f0ccc92); // 54
  II (c, d, a, b, x[10], S43, $ffeff47d); // 55
  II (b, c, d, a, x[ 1], S44, $85845dd1); // 56
  II (a, b, c, d, x[ 8], S41, $6fa87e4f); // 57
  II (d, a, b, c, x[15], S42, $fe2ce6e0); // 58
  II (c, d, a, b, x[ 6], S43, $a3014314); // 59
  II (b, c, d, a, x[13], S44, $4e0811a1); // 60
  II (a, b, c, d, x[ 4], S41, $f7537e82); // 61
  II (d, a, b, c, x[11], S42, $bd3af235); // 62
  II (c, d, a, b, x[ 2], S43, $2ad7d2bb); // 63
  II (b, c, d, a, x[ 9], S44, $eb86d391); // 64

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
procedure THashAlgMD5Engine.Encode(var output: array of byte; const input: array of DWORD; const len: cardinal);
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
procedure THashAlgMD5Engine.Decode(var output: array of DWORD; const input: array of byte; const len: cardinal);
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


