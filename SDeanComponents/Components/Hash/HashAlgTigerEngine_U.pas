unit HashAlgTigerEngine_U;
// Description: Tiger Hashing Engine
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
  // Tiger context.
  Tiger_CTX = packed record
    state: array [0..2] of ULONGLONG;  // state (ABCD)
    count: ULONGLONG;  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..64] of byte;  // input buffer
  end;

type
  THashAlgTigerEngine = class(THashAlgEngine)
  private
    fPasses: integer;

    // These are the Tiger functions
    function  t1(idx: byte): ULONGLONG;
    function  t2(idx: byte): ULONGLONG;
    function  t3(idx: byte): ULONGLONG;
    function  t4(idx: byte): ULONGLONG;
    procedure save_abc(a, b, c: ULONGLONG; var aa, bb, cc: ULONGLONG);
    procedure pass(var a, b, c: ULONGLONG; mul: ULONGLONG; x0, x1, x2, x3, x4, x5, x6, x7: ULONGLONG);
    procedure round(var a, b, c, x: ULONGLONG; mul: ULONGLONG);
    procedure key_schedule(var x0, x1, x2, x3, x4, x5, x6, x7: ULONGLONG);
    procedure feedforward(var a, b, c: ULONGLONG; aa, bb, cc: ULONGLONG);
    procedure compress(var state: array of ULONGLONG; block: array of ULONGLONG);

    procedure Encode(var output: array of byte; const input: array of ULONGLONG; const len: cardinal);
    procedure Decode(var output: array of ULONGLONG; const input: array of byte; const len: cardinal);

  public
    property Passes: integer read fPasses write fPasses;

    constructor Create();
    destructor Destroy(); override;

    procedure TigerInit(var context: Tiger_CTX);
    procedure TigerUpdate(var context: Tiger_CTX; const input: array of byte; const inputLen: cardinal);
    procedure TigerFinal(digest: THashValue; var context: Tiger_CTX);
  end;

implementation

uses
  HashAlgTigerSBoxes_U;


// Yes, the first value in this array is $01; the first bit after the data to
// be hashed (the padding) is "1"
// ...and yes, this does differ from SHA-512's $80 - the byte ordering is
// different (see also Encode(...) and Decode(...))
const TIGER_PADDING_512: array [0..63] of byte = (
  $01,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);


constructor THashAlgTigerEngine.Create();
begin
  inherited;

  fPasses := 3;
end;

destructor THashAlgTigerEngine.Destroy();
begin
  inherited;
end;

// Tiger initialization. Begins an Tiger operation, writing a new context.
procedure THashAlgTigerEngine.TigerInit (var context: Tiger_CTX);
begin
  context.count := 0;

  // Load Tiger initialization constants.
  context.state[0] := $0123456789ABCDEF;
  context.state[1] := $FEDCBA9876543210;
  context.state[2] := $F096A5B4C3B2E187;

  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// Tiger block update operation. Continues a Tiger message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgTigerEngine.TigerUpdate(var context: Tiger_CTX; const input: array of byte; const inputLen: cardinal);
var
  i, index, partLen: cardinal;
  longlongBlock: array [0..7] of ULONGLONG;
begin
  // Compute number of bytes mod 64
  index := ((context.count shr 3) AND $3F);

  // Update number of bits
  context.count := context.count + (inputLen shl 3);


  partLen := 64 - index;

  // Transform as many times as possible.
  if (inputLen >= partLen) then
    begin
    HE_memcpy(context.buffer, input, index, 0, partLen);
    Decode(longlongBlock, context.buffer, (512 div 8));
    compress(context.state, longlongBlock);

    i:=partLen;
    while ((i+63) < inputLen) do
      begin
      Decode(longlongBlock, input[i], (512 div 8));
      compress(context.state, longlongBlock);
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


// Tiger finalization. Ends an Tiger message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgTigerEngine.TigerFinal(digest: THashValue; var context: Tiger_CTX);
var
  bits: array [0..7] of byte;
  index: cardinal;
  padLen: cardinal;
  highByteFirst: array [0..0] of ULONGLONG;
  tmpArr: THashArray;
begin
  // Unlike MD5, SHA-1 uses the high word first
  highByteFirst[0] := context.count;

  // Save number of bits
  // Unlike SHA-512, this is only a single 64 bit value
  Encode(bits, highByteFirst, 8);

  // Pad out to 56 mod 64.
  // 448 mod 512 (bits) -> 56 mod 64 (bytes)
//  index := ((context.count shr 3) AND $7f);
  index := ((context.count shr 3) AND $3f);
  if (index < 56) then
    begin
    padLen := (56 - index);
    end
  else
    begin
    padLen := (120 - index);
    end;

  TigerUpdate(context, TIGER_PADDING_512, padLen);

  // Append 64 bit length (before padding)
  TigerUpdate(context, bits, 8);  // 8 bytes = 64 bits

  // Store state in digest
  Encode(tmpArr, context.state, (192 div 8));
  digest.SetValueAsArray(tmpArr, 192);

  // Zeroize sensitive information.
  context.count := 0;
  HE_memset(context.state,  0, low(context.state),  Length(context.state));
  HE_memset(context.buffer, 0, low(context.buffer), Length(context.buffer));
  HE_memset(tmpArr,         0, low(tmpArr),         Length(tmpArr));

end;


// Encodes input (UINT4) into output (unsigned char). Assumes len is
// a multiple of 8.
// !! WARNING !!
// This converts using a DIFFERENT BYTE ORDER than that of SHA-xxx
procedure THashAlgTigerEngine.Encode(var output: array of byte; const input: array of ULONGLONG; const len: cardinal);
var
  i, j: cardinal;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    // !! WARNING !!
    // This converts using a DIFFERENT BYTE ORDER than that of SHA-xxx

    output[j+0] := ((input[i]       ) AND $ff);
    output[j+1] := ((input[i] shr  8) AND $ff);
    output[j+2] := ((input[i] shr 16) AND $ff);
    output[j+3] := ((input[i] shr 24) AND $ff);
    output[j+4] := ((input[i] shr 32) AND $ff);
    output[j+5] := ((input[i] shr 40) AND $ff);
    output[j+6] := ((input[i] shr 48) AND $ff);
    output[j+7] := ((input[i] shr 56) AND $ff);

    inc(i);
    inc(j, 8);
    end;

end;

// Decodes input (unsigned char) into output (UINT4). Assumes len is
// a multiple of 8.
// !! WARNING !!
// This converts using a DIFFERENT BYTE ORDER than that of SHA-xxx
procedure THashAlgTigerEngine.Decode(var output: array of ULONGLONG; const input: array of byte; const len: cardinal);
var
  i, j: cardinal;
  low32Bits: ULONGLONG;
  high32Bits: ULONGLONG;
begin
  i := 0;
  j := 0;

  while (j<len) do
    begin
    // !! WARNING !!
    // This converts using a DIFFERENT BYTE ORDER than that of SHA-xxx

    // This is the ideal:
    //    output[i] := (input[j+1]) OR ((input[j+2]) shl 8) OR ((input[j+3]) shl 16) OR ((input[j+4]) shl 24) OR ((input[j+5]) shl 32) OR ((input[j+6]) shl 40) OR ((input[j+7]) shl 48) OR ((input[j+0]) shl 56);
    // However, "shl" won't work when shifting 33 or more places
    // So, we remedy this by doing the calculation in 2 stages.
    low32Bits  := (input[j+0]) OR ((input[j+1]) shl 8) OR ((input[j+2]) shl 16) OR ((input[j+3]) shl 24);
    high32Bits := (input[j+4]) OR ((input[j+5]) shl 8) OR ((input[j+6]) shl 16) OR ((input[j+7]) shl 24);
    output[i] := low32Bits + (high32Bits * $0000000100000000);

    inc(i);
    inc(j, 8);
    end;

end;


function THashAlgTigerEngine.t1(idx: byte): ULONGLONG;
begin
  Result := tigerTable[idx];
end;

function THashAlgTigerEngine.t2(idx: byte): ULONGLONG;
begin
  Result := tigerTable[idx+256];
end;

function THashAlgTigerEngine.t3(idx: byte): ULONGLONG;
begin
  Result := tigerTable[idx+(256*2)];
end;

function THashAlgTigerEngine.t4(idx: byte): ULONGLONG;
begin
  Result := tigerTable[idx+(256*3)];
end;

procedure THashAlgTigerEngine.save_abc(a, b, c: ULONGLONG; var aa, bb, cc: ULONGLONG);
begin
  aa := a;
  bb := b;
  cc := c;
end;

procedure THashAlgTigerEngine.pass(var a, b, c: ULONGLONG; mul: ULONGLONG; x0, x1, x2, x3, x4, x5, x6, x7: ULONGLONG);
begin
  round(a, b, c, x0, mul);
  round(b, c, a, x1, mul);
  round(c, a, b, x2, mul);
  round(a, b, c, x3, mul);
  round(b, c, a, x4, mul);
  round(c, a, b, x5, mul);
  round(a, b, c, x6, mul);
  round(b, c, a, x7, mul);
end;

procedure THashAlgTigerEngine.round(var a, b, c, x: ULONGLONG; mul: ULONGLONG);
begin
  // c ^= x ;
  c := c xor x;

  // a -= t1[c_0] ^ t2[c_2] ^ t3[c_4] ^ t4[c_6] ;
  a := a - (
            t1((c       ) and $FF) xor
            t2((c shr 16) and $FF) xor
            t3((c shr 32) and $FF) xor
            t4((c shr 48) and $FF)
           );

  // b += t4[c_1] ^ t3[c_3] ^ t2[c_5] ^ t1[c_7] ;
  b := b + (
            t4((c shr  8) and $FF) xor
            t3((c shr 24) and $FF) xor
            t2((c shr 40) and $FF) xor
            t1((c shr 56) and $FF)
           );

  // b *= mul;
  b := b * mul;

end;

procedure THashAlgTigerEngine.key_schedule(var x0, x1, x2, x3, x4, x5, x6, x7: ULONGLONG);
begin
  x0 := x0 - (x7 xor $A5A5A5A5A5A5A5A5);
  x1 := x1 xor x0;
  x2 := x2 + x1;
  x3 := x3 - (x2 xor ((not(x1)) shl 19));
  x4 := x4 xor x3;
  x5 := x5 + x4;
  x6 := x6 - (x5 xor ((not(x4)) shr 23));
  x7 := x7 xor x6;
  x0 := x0 + x7;
  x1 := x1 - (x0 xor ((not(x7)) shl 19));
  x2 := x2 xor x1;
  x3 := x3 + x2;
  x4 := x4 - (x3 xor ((not(x2)) shr 23));
  x5 := x5 xor x4;
  x6 := x6 + x5;
  x7 := x7 - (x6 xor $0123456789ABCDEF);
end;

procedure THashAlgTigerEngine.feedforward(var a, b, c: ULONGLONG; aa, bb, cc: ULONGLONG);
begin
  a := a xor aa;
  b := b - bb;
  c := c + cc;
end;


// Tiger compression function.
procedure THashAlgTigerEngine.compress(var state: array of ULONGLONG; block: array of ULONGLONG);
var
  a, b, c: ULONGLONG;
  x0, x1, x2, x3, x4, x5, x6, x7: ULONGLONG;
  aa, bb, cc: ULONGLONG;
  pass_no: integer;
  tmpa: ULONGLONG;
begin
  a := state[0];
  b := state[1];
  c := state[2];

  x0 := block[0];
  x1 := block[1];
  x2 := block[2];
  x3 := block[3];
  x4 := block[4];
  x5 := block[5];
  x6 := block[6];
  x7 := block[7];

  save_abc(a, b, c, aa, bb, cc);
  pass(a, b, c, 5, x0, x1, x2, x3, x4, x5, x6, x7);
  key_schedule(x0, x1, x2, x3, x4, x5, x6, x7);
  pass(c, a, b, 7, x0, x1, x2, x3, x4, x5, x6, x7);
  key_schedule(x0, x1, x2, x3, x4, x5, x6, x7);
  pass(b, c, a, 9, x0, x1, x2, x3, x4, x5, x6, x7);

  // Yes, this for loop is correct (the -1 part)
  //  - this for loop should be skipped if Passes is anything less than 3; we've
  //    already done 3 passes just above
  for pass_no:=3 to (Passes-1) do
    begin
    key_schedule(x0, x1, x2, x3, x4, x5, x6, x7);
    pass(b, c, a, 9, x0, x1, x2, x3, x4, x5, x6, x7);
    tmpa := a;
    a := c;
    c := b;
    b := tmpa;
    end;

  feedforward(a, b, c, aa, bb, cc);

  state[0] := a;
  state[1] := b;
  state[2] := c;

end;


END.


