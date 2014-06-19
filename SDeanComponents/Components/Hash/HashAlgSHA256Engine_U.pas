unit HashAlgSHA256Engine_U;
// Description: SHA-256 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// The basic structure of this file, and the use of the object it describes, is
// based on the MD5 hash


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // SHA256 context.
  SHA256_CTX = packed record
    state: array [0..7] of DWORD;  // state (ABCD)
    count: array [0..1] of DWORD;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..63] of byte; // input buffer
  end;

type
  THashAlgSHA256Engine = class(THashAlgEngine)
  private
    procedure SHA256Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
    procedure Decode(var output: array of DWORD; var input: array of byte; len: cardinal);

    // These are the six SHA functions
    function  Ch(x, y, z: DWORD): DWORD;
    function  Maj(x, y, z: DWORD): DWORD;
    function  sigmaE0(x: DWORD): DWORD;
    function  sigmaE1(x: DWORD): DWORD;
    function  sigmao0(x: DWORD): DWORD;
    function  sigmao1(x: DWORD): DWORD;

    function  ROTR(x: DWORD; n: cardinal): DWORD;
  public
    procedure SHA256Init(var context: SHA256_CTX);
    procedure SHA256Update(var context: SHA256_CTX; const input: array of byte; const inputLen: cardinal);
    procedure SHA256Final(digest: THashValue; var context: SHA256_CTX);
  end;

implementation

// Yes, the first value in this array is $80; the first bit after the data to
// be hashed (the padding) is "1"
const PADDING256: array [0..63] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

const SHA256_K: array [0..63] of cardinal = (
  $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
  $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
  $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
  $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
  $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
  $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
  $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
  $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
);

// SHA256 initialization. Begins an SHA256 operation, writing a new context.
procedure THashAlgSHA256Engine.SHA256Init (var context: SHA256_CTX);
begin
  context.count[0] := 0;
  context.count[1] := 0;

  // Load magic initialization constants.
  context.state[0] := $6a09e667;
  context.state[1] := $bb67ae85;
  context.state[2] := $3c6ef372;
  context.state[3] := $a54ff53a;
  context.state[4] := $510e527f;
  context.state[5] := $9b05688c;
  context.state[6] := $1f83d9ab;
  context.state[7] := $5be0cd19;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// SHA256 block update operation. Continues an SHA256 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgSHA256Engine.SHA256Update(var context: SHA256_CTX; const input: array of byte; const inputLen: cardinal);
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
    SHA256Transform(context.state, context.buffer, 0);

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      SHA256Transform(context.state, input, i);
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

// SHA256 finalization. Ends an SHA256 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgSHA256Engine.SHA256Final(digest: THashValue; var context: SHA256_CTX);
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
  // 448 mod 512 (bits) -> 56 mod 64 (bytes)
  index := ((context.count[0] shr 3) AND $3f);
  if (index < 56) then
    begin
    padLen := (56 - index);
    end
  else
    begin
    padLen := (120 - index);
    end;

  SHA256Update(context, PADDING256, padLen);

  // Append length (before padding)
  SHA256Update(context, bits, 8);

  // Store state in digest
  Encode(tmpArr, context.state, 32);
  digest.SetValueAsArray(tmpArr, (32 * 8));

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// SHA256 basic transformation. Transforms state based on block.
procedure THashAlgSHA256Engine.SHA256Transform(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
var
  A, B, C, D, E, F, G, H: DWORD;
  W: array [0..63] of DWORD;
  T1: DWORD;
  T2: DWORD;
  i: integer;
  t: integer;
begin
  Decode(W, block[startOffsetInBlock], 64);

  for t:=16 to 63 do
    begin
    W[t] := sigmao1(W[t-2]) + W[t-7] + sigmao0(W[t-15]) + W[t-16];
    end;

  A := state[0];
  B := state[1];
  C := state[2];
  D := state[3];
  E := state[4];
  F := state[5];
  G := state[6];
  H := state[7];


  for t:=0 to 63 do
    begin
    T1 := H + sigmaE1(E) + Ch(E, F, G) + SHA256_K[t] + W[t];
    T2 := sigmaE0(A) + Maj(A, B, C);

    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
    end;

  state[0] := state[0] + A;
  state[1] := state[1] + B;
  state[2] := state[2] + C;
  state[3] := state[3] + D;
  state[4] := state[4] + E;
  state[5] := state[5] + F;
  state[6] := state[6] + G;
  state[7] := state[7] + H;

  // Zeroize sensitive information.
//  HE_memset(x, 0, 0, Length(x)+1);
  for i:=low(W) to high(W) do
    begin
    W[i] := 0;
    end;

end;

// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 4.
procedure THashAlgSHA256Engine.Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
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
procedure THashAlgSHA256Engine.Decode(var output: array of DWORD; var input: array of byte; len: cardinal);
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


function THashAlgSHA256Engine.Ch(x, y, z: DWORD): DWORD;
begin
  Result := (x AND y) xor (not(x) AND z);
end;


function THashAlgSHA256Engine.Maj(x, y, z: DWORD): DWORD;
begin
  Result := (x AND y) xor (x AND z) xor (y AND z);
end;


function THashAlgSHA256Engine.sigmaE0(x: DWORD): DWORD;
begin
  Result := ROTR(x, 2) xor ROTR(x, 13) xor ROTR(x, 22);
end;


function THashAlgSHA256Engine.sigmaE1(x: DWORD): DWORD;
begin
  Result := ROTR(x, 6) xor ROTR(x, 11) xor ROTR(x, 25);
end;


function THashAlgSHA256Engine.sigmao0(x: DWORD): DWORD;
begin
  Result := ROTR(x, 7) xor ROTR(x, 18) xor (x shr 3);
end;


function THashAlgSHA256Engine.sigmao1(x: DWORD): DWORD;
begin
  Result := ROTR(x, 17) xor ROTR(x, 19) xor (x shr 10);
end;


// Rotate all bits, bits dropping off the end get wrapped round and reinserted
// onto the other end
function THashAlgSHA256Engine.ROTR(x: DWORD; n: cardinal): DWORD;
begin
  Result := (x shr n) OR (x shl (32-n));

end;


END.


