unit HashAlgMD2Engine_U;
// Description: MD2 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


//  Delphi translation of the MD2 program by RSA Data Security, Inc., as specified in RFC 1319


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

type
  // MD2 context.
  MD2_CTX = packed record
    state: array [0..15] of byte;   // state (ABCD)
    checksum: array [0..15] of byte; // checksum
    count: integer;                  // number of bits, modulo 2^64 (lsb first)
    buffer: array [0..15] of byte;   // input buffer
  end;

type
  THashAlgMD2Engine = class(THashAlgEngine)
  private
    procedure MD2Transform(var state: array of byte; var checksum: array of byte; block: array of byte; startOffsetInBlock: cardinal);

  public
    procedure MD2Init(var context: MD2_CTX);
    procedure MD2Update(var context: MD2_CTX; const input: array of byte; const inputLen: cardinal);
    procedure MD2Final(digest: THashValue; var context: MD2_CTX);
  end;

implementation


// Permutation of 0..255 constructed from the digits of pi. It gives a
// "random" nonlinear byte substitution operation.
const PI_SUBST: array [0..255] of byte = (
  41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
  19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
  76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
  138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
  245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
  148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
  39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
  181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
  150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
  112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
  96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
  85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
  234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
  129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
  8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
  203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
  166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
  31, 26, 219, 153, 141, 51, 159, 17, 131, 20
);

// The definition of PADDING in the original used octal, this uses decimal
const PADDING: array [0..16] of array [0..15] of byte = (
  ( 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 3,  3,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 4,  4,  4,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 5,  5,  5,  5,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 6,  6,  6,  6,  6,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 7,  7,  7,  7,  7,  7,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 8,  8,  8,  8,  8,  8,  8,  8,  0,  0,  0,  0,  0,  0,  0,  0),
  ( 9,  9,  9,  9,  9,  9,  9,  9,  9,  0,  0,  0,  0,  0,  0,  0),
  (10, 10, 10, 10, 10, 10, 10, 10, 10, 10,  0,  0,  0,  0,  0,  0),
  (11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,  0,  0,  0,  0,  0),
  (12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,  0,  0,  0,  0),
  (13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,  0,  0,  0),
  (14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,  0,  0),
  (15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0),
  (16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)
  );

// MD2 initialization. Begins an MD2 operation, writing a new context.
procedure THashAlgMD2Engine.MD2Init (var context: MD2_CTX);
begin
  context.count := 0;
  HE_memset(context.state, 0, 0, Length(context.state));
  HE_memset(context.checksum, 0, 0, Length(context.checksum));
  HE_memset(context.buffer, 0, 0, Length(context.buffer));

end;

// MD2 block update operation. Continues an MD2 message-digest
// operation, processing another message block, and updating the
// context.
procedure THashAlgMD2Engine.MD2Update(var context: MD2_CTX; const input: array of byte; const inputLen: cardinal);
var
  i, index, partLen: cardinal;
begin
  // Update number of bytes mod 16
  index := context.count;
  context.count := (index + inputLen) AND $f;

  partLen := 16 - index;

  // Transform as many times as possible.
  if (inputLen >= partLen) then
    begin
    HE_memcpy(context.buffer, input, index, 0, partLen);
    MD2Transform(context.state, context.checksum, context.buffer, 0);

    i:=partLen;
    while ((i+15)<inputLen) do
      begin
      MD2Transform(context.state, context.checksum, input, i);
      inc(i, 16);
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

// MD2 finalization. Ends an MD2 message-digest operation, writing the
// the message digest and zeroizing the context.
procedure THashAlgMD2Engine.MD2Final(digest: THashValue; var context: MD2_CTX);
var
  index: cardinal;
  padLen: cardinal;
  tmpArr: THashArray;
begin
  // Pad out to multiple of 16.
  index := context.count;
  padLen := 16 - index;
  MD2Update(context, PADDING[padLen], padLen);

  // Extend with checksum
  MD2Update (context, context.checksum, 16);

  // Store state in digest
  HE_memcpy(tmpArr, context.state, 0, 0, 16);
  digest.SetValueAsArray(tmpArr, (16 * 8));

  // Zeroize sensitive information.
  context.count := 0;
  HE_memset(context.state,    0, low(context.state),    Length(context.state));
  HE_memset(context.checksum, 0, low(context.checksum), Length(context.checksum));
  HE_memset(context.buffer,   0, low(context.buffer),   Length(context.buffer));
  HE_memset(tmpArr,           0, low(tmpArr),           Length(tmpArr));

end;


// MD2 basic transformation. Transforms state based on block.
procedure THashAlgMD2Engine.MD2Transform(var state: array of byte; var checksum: array of byte; block: array of byte; startOffsetInBlock: cardinal);
var
  i, j, t: cardinal;
  x: array [0..47] of byte;
begin
  // Form encryption block from state, block, state ^ block.
  HE_memcpy(x, state, 0, 0, 16);
  HE_memcpy(x, block, 16, startOffsetInBlock, 16);

  i:=0;
  while (i<16) do
    begin
    x[i+32] := state[i] XOR block[i+startOffsetInBlock];
    inc(i);
    end;

  // Encrypt block (18 rounds).
  t := 0;
  i:=0;
  while (i<18) do
    begin
    j:=0;
    while (j<48) do
      begin
      x[j] := x[j] XOR PI_SUBST[t];
      t := x[j];
      inc(j);
      end;
    t := (t + i) AND $ff;
    inc(i);
    end;


  // Save new state
  HE_memcpy(state, x, 0, 0, 16);

  // Update checksum.
  t := checksum[15];
  i:=0;
  while (i<16) do
    begin
    checksum[i] := checksum[i] XOR PI_SUBST[block[i+startOffsetInBlock] XOR t];
    t := checksum[i];
    inc(i);
    end;

  // Zeroize sensitive information.
  HE_memset (x, 0, 0, Length(x));

end;


END.


