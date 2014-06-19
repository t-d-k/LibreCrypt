unit HashAlgMD5_U;
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
  HashAlgMD5Engine_U;

type
  THashAlgMD5 = class(THashAlg)
  private
    md5Engine: THashAlgMD5Engine;
    context: MD5_CTX;
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

procedure Register;
begin
  RegisterComponents('Hash', [THashAlgMD5]);

end;

constructor THashAlgMD5.Create(AOwner: TComponent);
begin
  inherited;
  
  md5Engine:= THashAlgMD5Engine.Create();
  
  fTitle := 'MD5';
  fHashLength := 128;
  fBlockLength := (64 * 8);

end;

destructor THashAlgMD5.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  md5Engine.MD5Init(context);
  
  md5Engine.Free();

  inherited;
end;

procedure THashAlgMD5.Init();
begin
  md5Engine.MD5Init(context);
end;

procedure THashAlgMD5.Update(const input: array of byte; const inputLen: cardinal);
begin
  md5Engine.MD5Update(context, input, inputLen);
end;

procedure THashAlgMD5.Final(digest: THashValue);
begin
  md5Engine.MD5Final(digest, context);
end;


END.

