unit HashAlgRIPEMDEngine_U;
// Description: RIPEMD Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // RIPEMD context.
  RIPEMD_CTX = packed record
    state: array [0..9] of DWORD;  // state (ABCDE)
    count: array [0..1] of DWORD;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..63] of byte; // input buffer
  end;

type
  THashAlgRIPEMDEngine = class(THashAlgEngine)
  private
    fBasicSize128: boolean;

    procedure RIPEMDTransform128(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
    procedure RIPEMDTransform160(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);

    procedure Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
    procedure Decode(var output: array of DWORD; var input: array of byte; len: cardinal);

  public
    OutputLength: integer;
    procedure RIPEMDInit(var context: RIPEMD_CTX);
    procedure RIPEMDUpdate(var context: RIPEMD_CTX; const input: array of byte; const inputLen: cardinal);
    procedure RIPEMDFinal(digest: THashValue; var context: RIPEMD_CTX);
  end;

implementation

// Constants for RIPEMDTransform routine.
const r: array [0..79] of integer = (
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
  7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
  3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
  1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
  4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13
  );

const rr: array [0..79] of integer = (
  5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
  6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
  15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
  8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
  12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11
  );

const s: array [0..79] of integer = (
  11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
  7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
  11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
  11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
  9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6
  );

const ss: array [0..79] of integer = (
  8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
  9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
  9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
  15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
  8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11
  );

const K: array [0..4] of DWORD = (
  $00000000,
  $5A827999,
  $6ED9EBA1,
  $8F1BBCDC,
  $A953FD4E
  );

const KK128: array [0..3] of DWORD = (
  $50A28BE6,
  $5C4DD124,
  $6D703EF3,
  $00000000
  );

const KK160: array [0..4] of DWORD = (
  $50A28BE6,
  $5C4DD124,
  $6D703EF3,
  $7A6D76E9,
  $00000000
  );


const PADDING: array [0..63] of byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);


function allfunc(t, x, y, z: DWORD): DWORD;
begin
//  if ((0 <= t) AND (t <= 15)) then
  if (t <= 15) then
    begin
    Result := x XOR y XOR z;
    end
  else if ((16 <= t) AND (t <= 31)) then
    begin
    Result := (x AND y) OR (NOT(x) AND z);
    end
  else if ((32 <= t) AND (t <= 47)) then
    begin
    Result := (x OR NOT(y)) XOR z;
    end
  else if ((48 <= t) AND (t <= 63)) then
    begin
    Result := (x AND z) OR (y AND NOT(z));
    end
  else if ((64 <= t) AND (t <= 79)) then
    begin
    Result := x XOR (y OR NOT(z));
    end
  else
    begin
    Result := 0;
    end;

end;

// ROL rotates x left n bits.
function ROL(x, n: cardinal): cardinal;
begin
  Result := (((x) shl (n)) OR ((x) shr (32-(n))))
end;

// RIPEMD initialization. Begins an RIPEMD operation, writing a new context.
procedure THashAlgRIPEMDEngine.RIPEMDInit (var context: RIPEMD_CTX);
begin
  fBasicSize128:= ((OutputLength mod 128)=0);

  context.count[0] := 0;
  context.count[1] := 0;
  // Load magic initialization constants.
  context.state[0] := $67452301;
  context.state[1] := $efcdab89;
  context.state[2] := $98badcfe;
  context.state[3] := $10325476;

  // Following used for generating > 160bit digests
  if fBasicSize128 then
    begin
    context.state[4] := $76543210;
    context.state[5] := $FEDCBA98;
    context.state[6] := $89ABCDEF;
    context.state[7] := $01234567;
    end
  else
    begin
    context.state[4] := $c3d2e1f0;

    // Following used for generating > 160bit digests
    context.state[5] := $76543210;
    context.state[6] := $FEDCBA98;
    context.state[7] := $89ABCDEF;
    context.state[8] := $01234567;
    context.state[9] := $3C2D1E0F;
    end;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// RIPEMD block update operation. Continues an RIPEMD message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgRIPEMDEngine.RIPEMDUpdate(var context: RIPEMD_CTX; const input: array of byte; const inputLen: cardinal);
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
    if fBasicSize128 then
      begin
      RIPEMDTransform128(context.state, context.buffer, 0);
      end
    else
      begin
      RIPEMDTransform160(context.state, context.buffer, 0);
      end;

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      if fBasicSize128 then
        begin
        RIPEMDTransform128(context.state, input, i);
        end
      else
        begin
        RIPEMDTransform160(context.state, input, i);
        end;
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

// RIPEMD finalization. Ends an RIPEMD message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgRIPEMDEngine.RIPEMDFinal(digest: THashValue; var context: RIPEMD_CTX);
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
  RIPEMDUpdate(context, PADDING, padLen);

  // Append length (before padding)
  RIPEMDUpdate(context, bits, 8);

  // Store state in digest
  Encode(tmpArr, context.state, (OutputLength div 8));
  digest.SetValueAsArray(tmpArr, OutputLength);

  // Zeroize sensitive information.
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.count,  0, low(context.count),  Length(context.count));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// RIPEMD basic transformation. Transforms state based on block.
procedure THashAlgRIPEMDEngine.RIPEMDTransform128(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
var
  aa, bb, cc, dd: DWORD;
  aaa, bbb, ccc, ddd: DWORD;
  x: array [0..15] of DWORD;
  j: integer;
  T: DWORD;
  doubleSize: boolean;
begin
  doubleSize := OutputLength>=256;

  aa := state[0];
  bb := state[1];
  cc := state[2];
  dd := state[3];

  if not(doubleSize) then
    begin
    aaa := state[0];
    bbb := state[1];
    ccc := state[2];
    ddd := state[3];
    end
  else
    begin
    aaa := state[4];
    bbb := state[5];
    ccc := state[6];
    ddd := state[7];
    end;

  Decode(x, block[startOffsetInBlock], 64);

  for j:=0 to 63 do
    begin
    T := aa + allfunc(j, bb, cc, dd) + x[r[j]] + K[j div 16];
    T := ROL(T, s[j]);
    aa := dd;
    dd := cc;
    cc := bb;
    bb := T;

    T := aaa + allfunc(63-j, bbb, ccc, ddd) + x[rr[j]] + KK128[j div 16];
    T := ROL(T, ss[j]);
    aaa := ddd;
    ddd := ccc;
    ccc := bbb;
    bbb := T;

    if doubleSize then
      begin
      if j=15 then
        begin
        T := aa;
        aa := aaa;
        aaa := T;
        end
      else if j=31 then
        begin
        T := bb;
        bb := bbb;
        bbb := T;
        end
      else if j=47 then
        begin
        T := cc;
        cc := ccc;
        ccc := T;
        end
      else if j=63 then
        begin
        T := dd;
        dd := ddd;
        ddd := T;
        end;

      end; // if doubleSize then

    end;


  // combine results
  if not(doubleSize) then
    begin
    ddd := state[1] + cc + ddd;               // final result for MDbuf[0]
    state[1] := state[2] + dd + aaa;
    state[2] := state[3] + aa + bbb;
    state[3] := state[0] + bb + ccc;
    state[0] := ddd;
    end
  else
    begin
    state[0] := state[0] + aa;
    state[1] := state[1] + bb;
    state[2] := state[2] + cc;
    state[3] := state[3] + dd;
    state[4] := state[4] + aaa;
    state[5] := state[5] + bbb;
    state[6] := state[6] + ccc;
    state[7] := state[7] + ddd;
    end;

  // Zeroize sensitive information.
//  HE_memset(x, 0, 0, Length(x)+1);
//xxx
{ TODO 3 -otdk -csecurity : check why not zeroed}
  for j:=0 to 15 do
    begin
    x[j] := 0;
    end;

end;

// RIPEMD basic transformation. Transforms state based on block.
procedure THashAlgRIPEMDEngine.RIPEMDTransform160(var state: array of DWORD; block: array of byte; startOffsetInBlock: cardinal);
var
  aa, bb, cc, dd, ee: DWORD;
  aaa, bbb, ccc, ddd, eee: DWORD;
  x: array [0..15] of DWORD;
  j: integer;
  T: DWORD;
  doubleSize: boolean;
begin
  doubleSize := OutputLength>=256;

  aa := state[0];
  bb := state[1];
  cc := state[2];
  dd := state[3];
  ee := state[4];

  if not(doubleSize) then
    begin
    aaa := state[0];
    bbb := state[1];
    ccc := state[2];
    ddd := state[3];
    eee := state[4];
    end
  else
    begin
    aaa := state[5];
    bbb := state[6];
    ccc := state[7];
    ddd := state[8];
    eee := state[9];
    end;

  Decode(x, block[startOffsetInBlock], 64);

  for j:=0 to 79 do
    begin
    T := aa + allfunc(j, bb, cc, dd) + x[r[j]] + K[j div 16];
    T := ROL(T, s[j]) + ee;
    aa := ee;
    ee := dd;
    dd := ROL(cc, 10);
    cc := bb;
    bb := T;

    T := aaa + allfunc(79-j, bbb, ccc, ddd) + x[rr[j]] + KK160[j div 16];
    T := ROL(T, ss[j]) + eee;
    aaa := eee;
    eee := ddd;
    ddd := ROL(ccc, 10);
    ccc := bbb;
    bbb := T;

    if doubleSize then
      begin
      if j=15 then
        begin
        T := bb;
        bb := bbb;
        bbb := T;
        end
      else if j=31 then
        begin
        T := dd;
        dd := ddd;
        ddd := T;
        end
      else if j=47 then
        begin
        T := aa;
        aa := aaa;
        aaa := T;
        end
      else if j=63 then
        begin
        T := cc;
        cc := ccc;
        ccc := T;
        end
      else if j=79 then
        begin
        T := ee;
        ee := eee;
        eee := T;
        end;

      end; // if doubleSize then

    end;


  // combine results
  if not(doubleSize) then
    begin
    ddd := state[1] + cc + ddd;               // final result for MDbuf[0]
    state[1] := state[2] + dd + eee;
    state[2] := state[3] + ee + aaa;
    state[3] := state[4] + aa + bbb;
    state[4] := state[0] + bb + ccc;
    state[0] := ddd;
    end
  else
    begin
    state[0] := state[0] + aa;
    state[1] := state[1] + bb;
    state[2] := state[2] + cc;
    state[3] := state[3] + dd;
    state[4] := state[4] + ee;
    state[5] := state[5] + aaa;
    state[6] := state[6] + bbb;
    state[7] := state[7] + ccc;
    state[8] := state[8] + ddd;
    state[9] := state[9] + eee;
    end;

  // Zeroize sensitive information.
//  HE_memset(x, 0, 0, Length(x)+1);
//xxx
{ TODO 3 -otdk -csecurity : check why not zeroed}
  for j:=0 to 15 do
    begin
    x[j] := 0;
    end;

end;

// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 4.
procedure THashAlgRIPEMDEngine.Encode(var output: array of byte; var input: array of DWORD; len: cardinal);
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
procedure THashAlgRIPEMDEngine.Decode(var output: array of DWORD; var input: array of byte; len: cardinal);
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


