unit HashAlgSHA512Engine_U;
// Description: SHA-512 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// !! WARNING !!
// Because Delphi doesn't support an UNSIGNED int64 datatype, this software
// uses LONGLONGs - a SIGNED datatype.
// This means that this software can only be used to hash strings, etc which
// are up to $7FFFFFFFFFFFFFF bytes in length (576460752303423487 bytes; or
// 536870911 GB - which should be enough for most people!)
// See "[*1]" in code below


// The basic structure of this file, and the use of the object it describes, is
// based on the MD5 hash


interface

uses
  Windows, SDUGeneral,
  HashValue_U,
  HashAlgEngine_U;

type
  // SHA512 context.
  SHA512_CTX = packed record
    state: array [0..7] of ULONGLONG;  // state (ABCD)
    count: array [0..1] of ULONGLONG;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..127] of byte;  // input buffer
  end;

type
  THashAlgSHA512Engine = class(THashAlgEngine)
  private
    procedure SHA512Transform(var state: array of ULONGLONG; block: array of byte; startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; var input: array of ULONGLONG; len: cardinal);
    procedure Decode(var output: array of ULONGLONG; var input: array of byte; len: cardinal);

    // These are the six SHA functions
    function  Ch(x, y, z: ULONGLONG): ULONGLONG;
    function  Maj(x, y, z: ULONGLONG): ULONGLONG;
    function  sigmaE0(x: ULONGLONG): ULONGLONG;
    function  sigmaE1(x: ULONGLONG): ULONGLONG;
    function  sigmao0(x: ULONGLONG): ULONGLONG;
    function  sigmao1(x: ULONGLONG): ULONGLONG;

    function  ROTR(x: ULONGLONG; n: cardinal): ULONGLONG;
  public
    procedure SHA512Init(var context: SHA512_CTX);
    procedure SHA512Update(var context: SHA512_CTX; const input: array of byte; const inputLen: cardinal);
    procedure SHA512Final(digest: THashValue; var context: SHA512_CTX);
  end;

implementation

// Yes, the first value in this array is $80; the first bit after the data to
// be hashed (the padding) is "1"
const PADDING384512: array [0..127] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

const SHA384512_K: array [0..79] of ULONGLONG = (
  $428a2f98d728ae22, $7137449123ef65cd, $b5c0fbcfec4d3b2f, $e9b5dba58189dbbc,
  $3956c25bf348b538, $59f111f1b605d019, $923f82a4af194f9b, $ab1c5ed5da6d8118,
  $d807aa98a3030242, $12835b0145706fbe, $243185be4ee4b28c, $550c7dc3d5ffb4e2,
  $72be5d74f27b896f, $80deb1fe3b1696b1, $9bdc06a725c71235, $c19bf174cf692694,
  $e49b69c19ef14ad2, $efbe4786384f25e3, $0fc19dc68b8cd5b5, $240ca1cc77ac9c65,
  $2de92c6f592b0275, $4a7484aa6ea6e483, $5cb0a9dcbd41fbd4, $76f988da831153b5,
  $983e5152ee66dfab, $a831c66d2db43210, $b00327c898fb213f, $bf597fc7beef0ee4,
  $c6e00bf33da88fc2, $d5a79147930aa725, $06ca6351e003826f, $142929670a0e6e70,
  $27b70a8546d22ffc, $2e1b21385c26c926, $4d2c6dfc5ac42aed, $53380d139d95b3df,
  $650a73548baf63de, $766a0abb3c77b2a8, $81c2c92e47edaee6, $92722c851482353b,
  $a2bfe8a14cf10364, $a81a664bbc423001, $c24b8b70d0f89791, $c76c51a30654be30,
  $d192e819d6ef5218, $d69906245565a910, $f40e35855771202a, $106aa07032bbd1b8,
  $19a4c116b8d2d0c8, $1e376c085141ab53, $2748774cdf8eeb99, $34b0bcb5e19b48a8,
  $391c0cb3c5c95a63, $4ed8aa4ae3418acb, $5b9cca4f7763e373, $682e6ff3d6b2b8a3,
  $748f82ee5defb2fc, $78a5636f43172f60, $84c87814a1f0ab72, $8cc702081a6439ec,
  $90befffa23631e28, $a4506cebde82bde9, $bef9a3f7b2c67915, $c67178f2e372532b,
  $ca273eceea26619c, $d186b8c721c0c207, $eada7dd6cde0eb1e, $f57d4f7fee6ed178,
  $06f067aa72176fba, $0a637dc5a2c898a6, $113f9804bef90dae, $1b710b35131c471b,
  $28db77f523047d84, $32caab7b40c72493, $3c9ebe0a15c9bebc, $431d67c49c100d4c,
  $4cc5d4becb3e42b6, $597f299cfc657e2a, $5fcb6fab3ad6faec, $6c44198c4a475817
);


// SHA512 initialization. Begins an SHA512 operation, writing a new context.
procedure THashAlgSHA512Engine.SHA512Init (var context: SHA512_CTX);
begin
  context.count[0] := 0;
  context.count[1] := 0;

  // Load magic initialization constants.
  context.state[0] := $6a09e667f3bcc908;
  context.state[1] := $bb67ae8584caa73b;
  context.state[2] := $3c6ef372fe94f82b;
  context.state[3] := $a54ff53a5f1d36f1;
  context.state[4] := $510e527fade682d1;
  context.state[5] := $9b05688c2b3e6c1f;
  context.state[6] := $1f83d9abfb41bd6b;
  context.state[7] := $5be0cd19137e2179;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// SHA512 block update operation. Continues an SHA512 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgSHA512Engine.SHA512Update(var context: SHA512_CTX; const input: array of byte; const inputLen: cardinal);
var
  i, index, partLen: cardinal;
begin
  // Compute number of bytes mod 128
  index := ((context.count[0] shr 3) AND $7F);

  // Update number of bits
  context.count[0] := context.count[0] + (inputLen shl 3);

  // !! WARNING !!
  // THIS IS *WRONG* - BECAUSE WE'RE USING "LONGLONG" TYPES, WHICH ARE SIGNED,
  // THIS CAN DIP BELOW THE INPUT NUMBER WHEN THE MOST SIGNIFICANT BIT IS SET
  // [*1]
  if ((context.count[0]) < (inputLen shl 3)) then
    begin
    context.count[1] := context.count[1]+1;
    end;
  // We don't make any further changes to context.count[1], since "inputLen"
  // can't increment it as it's only 32 bits long


  partLen := 128 - index;

  // Transform as many times as possible.
  if (inputLen >= partLen) then
    begin
    HE_memcpy(context.buffer, input, index, 0, partLen);
    SHA512Transform(context.state, context.buffer, 0);

    i:=partLen;
    while ((i+127) < inputLen) do
      begin
      SHA512Transform(context.state, input, i);
      inc(i, 128);
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

// SHA512 finalization. Ends an SHA512 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgSHA512Engine.SHA512Final(digest: THashValue; var context: SHA512_CTX);
var
  bits: array [0..15] of byte;
  index: cardinal;
  padLen: cardinal;
  highByteFirst: array [0..1] of ULONGLONG;
  tmpArr: THashArray;
begin
  // Unlike MD5, SHA-1 uses the high word first
  highByteFirst[1] := context.count[0];
  highByteFirst[0] := context.count[1];

  // Save number of bits
  Encode(bits, highByteFirst, 16);

  // Pad out to 112 mod 128.
  // 896 mod 1024 (bits) -> 112 mod 128 (bytes)
  index := ((context.count[0] shr 3) AND $7f);
  if (index < 112) then
    begin
    padLen := (112 - index);
    end
  else
    begin
    padLen := (240 - index);
    end;

  SHA512Update(context, PADDING384512, padLen);

  // Append length (before padding)
  SHA512Update(context, bits, 16);

  // Store state in digest
  Encode(tmpArr, context.state, 64);
  digest.SetValueAsArray(tmpArr, (64 * 8));

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// SHA512 basic transformation. Transforms state based on block.
procedure THashAlgSHA512Engine.SHA512Transform(var state: array of ULONGLONG; block: array of byte; startOffsetInBlock: cardinal);
var
  A, B, C, D, E, F, G, H: ULONGLONG;
  W: array [0..79] of ULONGLONG;
  T1: ULONGLONG;
  T2: ULONGLONG;
  i: integer;
  t: integer;
begin
  Decode(W, block[startOffsetInBlock], 128);

  for t:=16 to 79 do
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


  for t:=0 to 79 do
    begin
    T1 := H + sigmaE1(E) + Ch(E, F, G) + SHA384512_K[t] + W[t];
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
//  HE_memset(x, 0, 0, high(x)+1);
  for i:=low(W) to high(W) do
    begin
    W[i] := 0;
    end;

end;

// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 8.
procedure THashAlgSHA512Engine.Encode(var output: array of byte; var input: array of ULONGLONG; len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    output[j+7] := (input[i] AND $ff);
    output[j+6] := ((input[i] shr 8) AND $ff);
    output[j+5] := ((input[i] shr 16) AND $ff);
    output[j+4] := ((input[i] shr 24) AND $ff);
    output[j+3] := ((input[i] shr 32) AND $ff);
    output[j+2] := ((input[i] shr 40) AND $ff);
    output[j+1] := ((input[i] shr 48) AND $ff);
    output[j+0] := ((input[i] shr 56) AND $ff);

    inc(i);
    inc(j, 8);
    end;

end;

// Decodes input (unsigned char) into output (UINT4). Assumes len is
// a multiple of 8.
procedure THashAlgSHA512Engine.Decode(var output: array of ULONGLONG; var input: array of byte; len: cardinal);
var
  i, j: cardinal;
  low32Bits: ULONGLONG;
  high32Bits: ULONGLONG;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    // This is the ideal:
    //    output[i] := (input[j+7]) OR ((input[j+6]) shl 8) OR ((input[j+5]) shl 16) OR ((input[j+4]) shl 24) OR ((input[j+3]) shl 32) OR ((input[j+2]) shl 40) OR ((input[j+1]) shl 48) OR ((input[j+0]) shl 56);
    // However, "shl" won't work when shifting 33 or more places
    // So, we remedy this by doing the calculation in 2 stages.
    low32Bits  := (input[j+7]) OR ((input[j+6]) shl 8) OR ((input[j+5]) shl 16) OR ((input[j+4]) shl 24);
    high32Bits := (input[j+3]) OR ((input[j+2]) shl 8) OR ((input[j+1]) shl 16) OR ((input[j+0]) shl 24);
    output[i] := low32Bits + (high32Bits * $0000000100000000);

    inc(i);
    inc(j, 8);
    end;

end;


function THashAlgSHA512Engine.Ch(x, y, z: ULONGLONG): ULONGLONG;
begin
  Result := (x AND y) xor (not(x) AND z);
end;


function THashAlgSHA512Engine.Maj(x, y, z: ULONGLONG): ULONGLONG;
begin
  Result := (x AND y) xor (x AND z) xor (y AND z);
end;


function THashAlgSHA512Engine.sigmaE0(x: ULONGLONG): ULONGLONG;
begin
  Result := ROTR(x, 28) xor ROTR(x, 34) xor ROTR(x, 39);
end;


function THashAlgSHA512Engine.sigmaE1(x: ULONGLONG): ULONGLONG;
begin
  Result := ROTR(x, 14) xor ROTR(x, 18) xor ROTR(x, 41);
end;


function THashAlgSHA512Engine.sigmao0(x: ULONGLONG): ULONGLONG;
begin
  Result := ROTR(x, 1) xor ROTR(x, 8) xor (x shr 7);
end;


function THashAlgSHA512Engine.sigmao1(x: ULONGLONG): ULONGLONG;
begin
  Result := ROTR(x, 19) xor ROTR(x, 61) xor (x shr 6);
end;


// Rotate all bits, bits dropping off the end get wrapped round and reinserted
// onto the other end
function THashAlgSHA512Engine.ROTR(x: ULONGLONG; n: cardinal): ULONGLONG;
begin
  Result := (x shr n) OR (x shl (64-n));

end;


END.


