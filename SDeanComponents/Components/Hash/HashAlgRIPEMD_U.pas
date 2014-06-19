unit HashAlgRIPEMD_U;
// Description: RIPEMD Hash (Wrapper for the RIPEMD Hashing Engine)
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U,
  HashValue_U,
  HashAlgRIPEMDEngine_U;

type
  THashAlgRIPEMD = class(THashAlg)
  private
    ripemdEngine: THashAlgRIPEMDEngine;
    context: RIPEMD_CTX;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Init(); override;
    procedure Update(const input: array of byte; const inputLen: cardinal); override;
    procedure Final(digest: THashValue); override;

    procedure SetDigestSize(size: integer);
  end;

  THashAlgRIPEMD128 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD160 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD256 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD320 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgRIPEMD]);
  RegisterComponents('Hash', [THashAlgRIPEMD128]);
  RegisterComponents('Hash', [THashAlgRIPEMD160]);
  RegisterComponents('Hash', [THashAlgRIPEMD256]);
  RegisterComponents('Hash', [THashAlgRIPEMD320]);
end;

constructor THashAlgRIPEMD128.Create(AOwner: TComponent);
begin
  inherited;

  fTitle := 'RIPEMD-128';
  fHashLength:= 128;
  fBlockLength := 512;

end;

constructor THashAlgRIPEMD160.Create(AOwner: TComponent);
begin
  inherited;

  fTitle := 'RIPEMD-160';
  fHashLength:= 160;
  fBlockLength := 512;

end;

constructor THashAlgRIPEMD256.Create(AOwner: TComponent);
begin
  inherited;

  fTitle := 'RIPEMD-256';
  fHashLength:= 256;
  fBlockLength := 512;

end;

constructor THashAlgRIPEMD320.Create(AOwner: TComponent);
begin
  inherited;

  fTitle := 'RIPEMD-320';
  fHashLength:= 320;
  fBlockLength := 512;

end;

constructor THashAlgRIPEMD.Create(AOwner: TComponent);
begin
  inherited;

  ripemdEngine:= THashAlgRIPEMDEngine.Create();

  fTitle := 'RIPEMD-xxx';
  fBlockLength := 512;

end;

destructor THashAlgRIPEMD.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  ripemdEngine.RIPEMDInit(context);

  ripemdEngine.Free();

  inherited;

end;

procedure THashAlgRIPEMD.Init();
begin
  ripemdEngine.OutputLength := HashLength;
  ripemdEngine.RIPEMDInit(context);
end;

procedure THashAlgRIPEMD.Update(const input: array of byte; const inputLen: cardinal);
begin
  ripemdEngine.RIPEMDUpdate(context, input, inputLen);
end;

procedure THashAlgRIPEMD.Final(digest: THashValue);
begin
  ripemdEngine.RIPEMDFinal(digest, context);
end;


procedure THashAlgRIPEMD.SetDigestSize(size: integer);
begin
  if (size<>128) AND
     (size<>160) AND
     (size<>256) AND
     (size<>320) then
    begin
    fHashLength := size;
    end;
    
end;

END.

