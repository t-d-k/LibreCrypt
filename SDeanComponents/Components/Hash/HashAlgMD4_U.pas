unit HashAlgMD4_U;
// Description: MD5 Hash (Wrapper for the MD5 Hashing Engine)
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
  HashAlgMD4Engine_U;

type
  THashAlgMD4 = class(THashAlg)
  private
    md4Engine: THashAlgMD4Engine;
    context: MD4_CTX;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Init(); override;
    procedure Update(const input: array of byte; const inputLen: cardinal); override;
    procedure Final(digest: THashValue); override;
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead

procedure Register;
begin
  RegisterComponents('Hash', [THashAlgMD4]);
end;

constructor THashAlgMD4.Create(AOwner: TComponent);
begin
  inherited;

  md4Engine:= THashAlgMD4Engine.Create();

  fTitle := 'MD4';
  fHashLength := 128;
  fBlockLength := (64 * 8);

end;

destructor THashAlgMD4.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  md4Engine.MD4Init(context);

  md4Engine.Free();

  inherited;

end;

procedure THashAlgMD4.Init();
begin
  md4Engine.MD4Init(context);
end;

procedure THashAlgMD4.Update(const input: array of byte; const inputLen: cardinal);
begin
  md4Engine.MD4Update(context, input, inputLen);
end;

procedure THashAlgMD4.Final(digest: THashValue);
begin
  md4Engine.MD4Final(digest, context);
end;


END.

