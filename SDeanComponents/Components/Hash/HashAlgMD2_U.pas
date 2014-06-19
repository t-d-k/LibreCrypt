unit HashAlgMD2_U;
// Description: MD2 Hash (Wrapper for the MD5 Hashing Engine)
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
  HashAlgMD2Engine_U;

type
  THashAlgMD2 = class(THashAlg)
  private
    md2Engine: THashAlgmd2Engine;
    context: MD2_CTX;
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
  RegisterComponents('Hash', [THashAlgMD2]);
end;

constructor THashAlgMD2.Create(AOwner: TComponent);
begin
  inherited;

  md2Engine:= THashAlgmd2Engine.Create();

  fTitle := 'MD2';
  fHashLength := 128;
  fBlockLength := 128;

end;

destructor THashAlgMD2.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  md2Engine.MD2Init(context);

  md2Engine.Free();

  inherited;

end;

procedure THashAlgMD2.Init();
begin
  md2Engine.MD2Init(context);
end;

procedure THashAlgMD2.Update(const input: array of byte; const inputLen: cardinal);
begin
  md2Engine.MD2Update(context, input, inputLen);
end;

procedure THashAlgMD2.Final(digest: THashValue);
begin
  md2Engine.MD2Final(digest, context);
end;


END.

