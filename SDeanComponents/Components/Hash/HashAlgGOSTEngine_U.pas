unit HashAlgGOSTEngine_U;
// Description: GOST R 34.11-94 Hashing Engine
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


//  Delphi translation of the GOST hash program by SSH Communications Security, Finland


interface

uses
  Windows,
  HashValue_U,
  HashAlgEngine_U;

//type size_t = cardinal; // unsigned integer
type size_t = DWORD; // unsigned integer

type
  // GOST context.
  GostHashCtx = packed record
    sum: array [0..7] of DWORD;
    hash: array [0..7] of DWORD;
    len: array [0..7] of DWORD;
    partial: array [0..31] of byte;
    partial_bytes: size_t;
  end;

type
  THashAlgGOSTEngine = class(THashAlgEngine)
  private
    // lookup tables : each of these has two rotated 4-bit S-Boxes
    gost_sbox_1: array [0..255] of DWORD;
    gost_sbox_2: array [0..255] of DWORD;
    gost_sbox_3: array [0..255] of DWORD;
    gost_sbox_4: array [0..255] of DWORD;

  public
    procedure gosthash_reset(var ctx: GostHashCtx);
    procedure gosthash_init();
    procedure GOST_ENCRYPT_ROUND(var k1: DWORD; var k2: DWORD; var t: DWORD; var r: DWORD; var l: DWORD);
    procedure GOST_ENCRYPT(var key: array of DWORD; var t: DWORD; var r: DWORD; var l: DWORD);
    procedure gosthash_compress(var h: array of DWORD; var m: array of DWORD);
    procedure gosthash_bytes(var ctx: GostHashCtx; buf: array of byte; bits: size_t; start: integer);
    procedure gosthash_update(var ctx: GostHashCtx; const buf: array of byte; const len: size_t);
    procedure gosthash_final(var ctx: GostHashCtx; digest: THashValue);
  end;

implementation


// initialize the lookup tables
procedure THashAlgGOSTEngine.gosthash_init();
const
  // 4-bit S-Boxes
  sbox: array [0..7] of array [0..15] of DWORD = (
	(  4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3 ),
	( 14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9 ),
	(  5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11 ),
	(  7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3 ),
	(  6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2 ),
	(  4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14 ),
	( 13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12 ),
	(  1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12 )
    );
var
  a: integer;
  b: integer;
  i: integer;
  ax, bx, cx, dx: DWORD;
begin
  // s-box precomputation
  i:=0;
  a:=0;
  while (a<16) do
    begin
    ax := sbox[1][a] shl 15;
    bx := sbox[3][a] shl 23;
    cx := sbox[5][a];
    cx := (cx shr 1) OR (cx shl 31);
    dx := sbox[7][a] shl 7;

    b := 0;
    while (b<16) do
      begin
      gost_sbox_1[i] := ax OR (sbox[0][b] shl 11);
      gost_sbox_2[i] := bx OR (sbox[2][b] shl 19);
      gost_sbox_3[i] := cx OR (sbox[4][b] shl 27);
      gost_sbox_4[i] := dx OR (sbox[6][b] shl 3);
      inc(i);
      inc(b);
      end;

    inc(a);
    end;

end;

//  A macro that performs a full encryption round of GOST 28147-89.
//  Temporary variable t assumed and variables r and l for left and right
//  blocks
procedure THashAlgGOSTEngine.GOST_ENCRYPT_ROUND(var k1: DWORD; var k2: DWORD; var t: DWORD; var r: DWORD; var l: DWORD);
begin
  t := (k1) + r;
  l := l XOR (gost_sbox_1[t AND $ff] XOR gost_sbox_2[(t shr 8) AND $ff] XOR gost_sbox_3[(t shr 16) AND $ff] XOR gost_sbox_4[t shr 24]);
  t := (k2) + l;
  r := r XOR (gost_sbox_1[t AND $ff] XOR gost_sbox_2[(t shr 8) AND $ff] XOR gost_sbox_3[(t shr 16) AND $ff] XOR gost_sbox_4[t shr 24]);
end;

// encrypt a block with the given key
procedure THashAlgGOSTEngine.GOST_ENCRYPT(var key: array of DWORD; var t: DWORD; var r: DWORD; var l: DWORD);
begin
  GOST_ENCRYPT_ROUND(key[0], key[1], t, r, l);
  GOST_ENCRYPT_ROUND(key[2], key[3], t, r, l);
  GOST_ENCRYPT_ROUND(key[4], key[5], t, r, l);
  GOST_ENCRYPT_ROUND(key[6], key[7], t, r, l);
  GOST_ENCRYPT_ROUND(key[0], key[1], t, r, l);
  GOST_ENCRYPT_ROUND(key[2], key[3], t, r, l);
  GOST_ENCRYPT_ROUND(key[4], key[5], t, r, l);
  GOST_ENCRYPT_ROUND(key[6], key[7], t, r, l);
  GOST_ENCRYPT_ROUND(key[0], key[1], t, r, l);
  GOST_ENCRYPT_ROUND(key[2], key[3], t, r, l);
  GOST_ENCRYPT_ROUND(key[4], key[5], t, r, l);
  GOST_ENCRYPT_ROUND(key[6], key[7], t, r, l);
  GOST_ENCRYPT_ROUND(key[7], key[6], t, r, l);
  GOST_ENCRYPT_ROUND(key[5], key[4], t, r, l);
  GOST_ENCRYPT_ROUND(key[3], key[2], t, r, l);
  GOST_ENCRYPT_ROUND(key[1], key[0], t, r, l);
  t := r;
  r := l;
  l := t;

end;

//  "chi" compression function. the result is stored over h
procedure THashAlgGOSTEngine.gosthash_compress(var h: array of DWORD; var m: array of DWORD);
var
  i: integer;
  l, r, t: DWORD;
  key, u, v, w, s: array [0..7] of DWORD;
begin
  HE_memcpy(u, h, 0, 0, 8);
  HE_memcpy(v, m, 0, 0, 8);

  i:=0;
  while (i<8) do
    begin
    w[0] := u[0] XOR v[0];	       // w = u xor v
    w[1] := u[1] XOR v[1];
    w[2] := u[2] XOR v[2];
    w[3] := u[3] XOR v[3];
    w[4] := u[4] XOR v[4];
    w[5] := u[5] XOR v[5];
    w[6] := u[6] XOR v[6];
    w[7] := u[7] XOR v[7];

      // P-Transformation
    key[0] := (w[0]  AND $000000ff) OR ((w[2] AND $000000ff) shl 8) OR
              ((w[4] AND $000000ff) shl 16) OR ((w[6] AND $000000ff) shl 24);
    key[1] := ((w[0] AND $0000ff00) shr 8)  OR (w[2]  AND $0000ff00) OR
              ((w[4] AND $0000ff00) shl 8) OR ((w[6] AND $0000ff00) shl 16);
    key[2] := ((w[0] AND $00ff0000) shr 16) OR ((w[2] AND $00ff0000) shr 8) OR
              (w[4] AND $00ff0000) OR ((w[6] AND $00ff0000) shl 8);
    key[3] := ((w[0] AND $ff000000) shr 24) OR ((w[2] AND $ff000000) shr 16) OR
              ((w[4] AND $ff000000) shr 8) OR (w[6] AND $ff000000);
    key[4] := (w[1] AND $000000ff) OR ((w[3] AND $000000ff) shl 8) OR
              ((w[5] AND $000000ff) shl 16) OR ((w[7] AND $000000ff) shl 24);
    key[5] := ((w[1] AND $0000ff00) shr 8) OR (w[3]  AND $0000ff00) OR
              ((w[5] AND $0000ff00) shl 8) OR ((w[7] AND $0000ff00) shl 16);
    key[6] := ((w[1] AND $00ff0000) shr 16) OR ((w[3] AND $00ff0000) shr 8) OR
              (w[5] AND $00ff0000) OR ((w[7] AND $00ff0000) shl 8);
    key[7] := ((w[1] AND $ff000000) shr 24) OR ((w[3] AND $ff000000) shr 16) OR
              ((w[5] AND $ff000000) shr 8) OR (w[7] AND $ff000000);

    r := h[i];                   // encriphering transformation
    l := h[i + 1];
    GOST_ENCRYPT(key, t, r, l);

    s[i] := r;
    s[i + 1] := l;

    if (i = 6) then
      begin
      break;
      end;

    l := u[0] XOR u[2];         // U = A(U)
    r := u[1] XOR u[3];
    u[0] := u[2];
    u[1] := u[3];
    u[2] := u[4];
    u[3] := u[5];
    u[4] := u[6];
    u[5] := u[7];
    u[6] := l;
    u[7] := r;

    if (i = 2) then            // Constant C_3
      begin
      u[0] := u[0] XOR $ff00ff00;
      u[1] := u[1] XOR $ff00ff00;
      u[2] := u[2] XOR $00ff00ff;
      u[3] := u[3] XOR $00ff00ff;
      u[4] := u[4] XOR $00ffff00;
      u[5] := u[5] XOR $ff0000ff;
      u[6] := u[6] XOR $000000ff;
      u[7] := u[7] XOR $ff00ffff;
      end;

    l := v[0];// V = A(A(V))
    r := v[2];
    v[0] := v[4];
    v[2] := v[6];
    v[4] := l XOR r;
    v[6] := v[0] XOR r;
    l := v[1];
    r := v[3];
    v[1] := v[5];
    v[3] := v[7];
    v[5] := l XOR r;
    v[7] := v[1] XOR r;

    inc(i, 2);
    end;

  // 12 rounds of the LFSR (computed from a product matrix) and xor in M
  u[0] := m[0] XOR s[6];
  u[1] := m[1] XOR s[7];
  u[2] := m[2] XOR (s[0] shl 16) XOR (s[0] shr 16) XOR (s[0] AND $ffff) XOR
          (s[1] AND $ffff) XOR (s[1] shr 16) XOR (s[2] shl 16) XOR s[6] XOR (s[6] shl 16) XOR
          (s[7] AND $ffff0000) XOR (s[7] shr 16);
  u[3] := m[3] XOR (s[0] AND $ffff) XOR (s[0] shl 16) XOR (s[1] AND $ffff) XOR
          (s[1] shl 16) XOR (s[1] shr 16) XOR (s[2] shl 16) XOR (s[2] shr 16) XOR
          (s[3] shl 16) XOR s[6] XOR (s[6] shl 16) XOR (s[6] shr 16) XOR (s[7] AND $ffff) XOR
          (s[7] shl 16) XOR (s[7] shr 16);
  u[4] := m[4] XOR
          (s[0] AND $ffff0000) XOR (s[0] shl 16) XOR (s[0] shr 16) XOR
          (s[1] AND $ffff0000) XOR (s[1] shr 16) XOR (s[2] shl 16) XOR (s[2] shr 16) XOR
          (s[3] shl 16) XOR (s[3] shr 16) XOR (s[4] shl 16) XOR (s[6] shl 16) XOR
          (s[6] shr 16) XOR (s[7] AND $ffff) XOR (s[7] shl 16) XOR (s[7] shr 16);
  u[5] := m[5] XOR (s[0] shl 16) XOR (s[0] shr 16) XOR (s[0] AND $ffff0000) XOR
          (s[1] AND $ffff) XOR s[2] XOR (s[2] shr 16) XOR (s[3] shl 16) XOR (s[3] shr 16) XOR
          (s[4] shl 16) XOR (s[4] shr 16) XOR (s[5] shl 16) XOR  (s[6] shl 16) XOR
          (s[6] shr 16) XOR (s[7] AND $ffff0000) XOR (s[7] shl 16) XOR (s[7] shr 16);
  u[6] := m[6] XOR s[0] XOR (s[1] shr 16) XOR (s[2] shl 16) XOR s[3] XOR (s[3] shr 16) XOR
          (s[4] shl 16) XOR (s[4] shr 16) XOR (s[5] shl 16) XOR (s[5] shr 16) XOR s[6] XOR
          (s[6] shl 16) XOR (s[6] shr 16) XOR (s[7] shl 16);
  u[7] := m[7] XOR (s[0] AND $ffff0000) XOR (s[0] shl 16) XOR (s[1] AND $ffff) XOR
          (s[1] shl 16) XOR (s[2] shr 16) XOR (s[3] shl 16) XOR s[4] XOR (s[4] shr 16) XOR
          (s[5] shl 16) XOR (s[5] shr 16) XOR (s[6] shr 16) XOR (s[7] AND $ffff) XOR
          (s[7] shl 16) XOR (s[7] shr 16);

          
  // 16 * 1 round of the LFSR and xor in H
  v[0] := h[0] XOR (u[1] shl 16) XOR (u[0] shr 16);
  v[1] := h[1] XOR (u[2] shl 16) XOR (u[1] shr 16);
  v[2] := h[2] XOR (u[3] shl 16) XOR (u[2] shr 16);
  v[3] := h[3] XOR (u[4] shl 16) XOR (u[3] shr 16);
  v[4] := h[4] XOR (u[5] shl 16) XOR (u[4] shr 16);
  v[5] := h[5] XOR (u[6] shl 16) XOR (u[5] shr 16);
  v[6] := h[6] XOR (u[7] shl 16) XOR (u[6] shr 16);
  v[7] := h[7] XOR (u[0] AND $ffff0000) XOR (u[0] shl 16) XOR (u[7] shr 16) XOR
          (u[1] AND $ffff0000) XOR (u[1] shl 16) XOR (u[6] shl 16) XOR (u[7] AND $ffff0000);

          
  // 61 rounds of LFSR, mixing up h (computed from a product matrix)
  h[0] := (v[0] AND $ffff0000) XOR (v[0] shl 16) XOR (v[0] shr 16) XOR (v[1] shr 16) XOR
         (v[1] AND $ffff0000) XOR (v[2] shl 16) XOR (v[3] shr 16) XOR (v[4] shl 16) XOR
         (v[5] shr 16) XOR v[5] XOR (v[6] shr 16) XOR (v[7] shl 16) XOR (v[7] shr 16) XOR
         (v[7] AND $ffff);
  h[1] := (v[0] shl 16) XOR (v[0] shr 16) XOR (v[0] AND $ffff0000) XOR (v[1] AND $ffff) XOR
         v[2] XOR (v[2] shr 16) XOR (v[3] shl 16) XOR (v[4] shr 16) XOR (v[5] shl 16) XOR
         (v[6] shl 16) XOR v[6] XOR (v[7] AND $ffff0000) XOR (v[7] shr 16);
  h[2] := (v[0] AND $ffff) XOR (v[0] shl 16) XOR (v[1] shl 16) XOR (v[1] shr 16) XOR
         (v[1] AND $ffff0000) XOR (v[2] shl 16) XOR (v[3] shr 16) XOR v[3] XOR (v[4] shl 16) XOR
         (v[5] shr 16) XOR v[6] XOR (v[6] shr 16) XOR (v[7] AND $ffff) XOR (v[7] shl 16) XOR
         (v[7] shr 16);
  h[3] := (v[0] shl 16) XOR (v[0] shr 16) XOR (v[0] AND $ffff0000) XOR
         (v[1] AND $ffff0000) XOR (v[1] shr 16) XOR (v[2] shl 16) XOR (v[2] shr 16) XOR v[2] XOR
         (v[3] shl 16) XOR (v[4] shr 16) XOR v[4] XOR (v[5] shl 16) XOR (v[6] shl 16) XOR
         (v[7] AND $ffff) XOR (v[7] shr 16);
  h[4] := (v[0] shr 16) XOR (v[1] shl 16) XOR v[1] XOR (v[2] shr 16) XOR v[2] XOR
         (v[3] shl 16) XOR (v[3] shr 16) XOR v[3] XOR (v[4] shl 16) XOR (v[5] shr 16) XOR
         v[5] XOR (v[6] shl 16) XOR (v[6] shr 16) XOR (v[7] shl 16);
  h[5] := (v[0] shl 16) XOR (v[0] AND $ffff0000) XOR (v[1] shl 16) XOR (v[1] shr 16) XOR
         (v[1] AND $ffff0000) XOR (v[2] shl 16) XOR v[2] XOR (v[3] shr 16) XOR v[3] XOR
         (v[4] shl 16) XOR (v[4] shr 16) XOR v[4] XOR (v[5] shl 16) XOR (v[6] shl 16) XOR
         (v[6] shr 16) XOR v[6] XOR (v[7] shl 16) XOR (v[7] shr 16) XOR (v[7] AND $ffff0000);
  h[6] := v[0] XOR v[2] XOR (v[2] shr 16) XOR v[3] XOR (v[3] shl 16) XOR v[4] XOR
         (v[4] shr 16) XOR (v[5] shl 16) XOR (v[5] shr 16) XOR v[5] XOR (v[6] shl 16) XOR
         (v[6] shr 16) XOR v[6] XOR (v[7] shl 16) XOR v[7];
  h[7] := v[0] XOR (v[0] shr 16) XOR (v[1] shl 16) XOR (v[1] shr 16) XOR (v[2] shl 16) XOR
         (v[3] shr 16) XOR v[3] XOR (v[4] shl 16) XOR v[4] XOR (v[5] shr 16) XOR v[5] XOR
         (v[6] shl 16) XOR (v[6] shr 16) XOR (v[7] shl 16) XOR v[7];

end;

// Clear the state of the given context structure.
procedure THashAlgGOSTEngine.gosthash_reset(var ctx: GostHashCtx);
var
  i: integer;
begin
  HE_memset(ctx.sum,  0, 0, 8);
  HE_memset(ctx.hash, 0, 0, 8);
  HE_memset(ctx.len,  0, 0, 8);
  for i:=0 to (sizeof(ctx.partial)-1) do
    begin
    ctx.partial[i] := 0;
    end;
  ctx.partial_bytes := 0;

end;

// Mix in a 32-byte chunk ("stage 3")
procedure THashAlgGOSTEngine.gosthash_bytes(var ctx: GostHashCtx; buf: array of byte; bits: size_t; start: integer);
var
  i, j: integer;
  a, c: DWORD;
  m: array [0..7] of DWORD;
begin
  // convert bytes to a long words and compute the sum
  j := 0;
  c := 0;

  i:=0;
  while (i<8) do
    begin
    a := (buf[j+start]) OR
         ((buf[j + 1+start]) shl 8) OR
         ((buf[j + 2+start]) shl 16) OR
         ((buf[j + 3+start]) shl 24);
    inc(j, 4);
    m[i] := a;
    c := a + c + ctx.sum[i];
    ctx.sum[i] := c;
    if (c<a) then
      begin
      c := 1;
      end
    else
      begin
      c := 0;
      end;
    inc(i);
    end;

  // compress
   gosthash_compress(ctx.hash, m);

  // a 64-bit counter should be sufficient
  inc(ctx.len[0], bits);
  if (ctx.len[0] < bits) then
    begin
    inc(ctx.len[1]);
    end;

end;


// Mix in len bytes of data for the given buffer.
procedure THashAlgGOSTEngine.gosthash_update(var ctx: GostHashCtx; const buf: array of byte; const len: size_t);
var
  i, j: size_t;
begin
  i := ctx.partial_bytes;
  j := 0;
  while (i < 32) AND (j < len) do
    begin
    ctx.partial[i] := buf[j];
    inc(i);
    inc(j);
    end;

  if (i < 32) then
    begin
    ctx.partial_bytes := i;
    exit;
    end;

  gosthash_bytes(ctx, ctx.partial, 256, 0);

 while ((j + 32) < len) do
    begin
    gosthash_bytes(ctx, buf, 256, j);
    inc(j, 32);
    end;

  i := 0;
  while (j < len) do
    begin
    ctx.partial[i] := buf[j];
    inc(i);
    inc(j);
    end;
  ctx.partial_bytes := i;

end;


// Compute and save the 32-byte digest.
procedure THashAlgGOSTEngine.gosthash_final(var ctx: GostHashCtx; digest: THashValue);
var
  i, j: DWORD;
  a: DWORD;
  tmpArr: THashArray;
begin
  // Adjust and mix in the last chunk
  if (ctx.partial_bytes > 0) then
    begin
    for i:=1 to (32 - ctx.partial_bytes) do
      begin
      ctx.partial[ctx.partial_bytes+i-1] := 0;
      end;
    gosthash_bytes(ctx, ctx.partial, ctx.partial_bytes shl 3, 0);
    end;

  // Mix in the length and the sum
  gosthash_compress(ctx.hash, ctx.len);
  gosthash_compress(ctx.hash, ctx.sum);

  // Convert the output to bytes
  j := 0;
  i := 0;
  while (i<8) do
    begin
    a := ctx.hash[i];
    tmpArr[j] := a AND $FF;
    tmpArr[j + 1] := (a shr 8) AND $FF;
    tmpArr[j + 2] := (a shr 16) AND $FF;
    tmpArr[j + 3] := (a shr 24) AND $FF;
    inc(j, 4);
    inc(i);
    end;

  digest.SetValueAsArray(tmpArr, (32 * 8));

  // Zeroize sensitive information.
  HE_memset(tmpArr, 0, low(tmpArr), Length(tmpArr));

end;


END.


