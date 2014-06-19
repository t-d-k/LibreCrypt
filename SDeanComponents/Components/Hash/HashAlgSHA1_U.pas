unit HashAlgSHA1_U;
// Description: SHA-1 Hash (Wrapper for the SHA-1 Hashing Engine)
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
  HashAlgSHA1Engine_U;

type
  THashAlgSHA1 = class(THashAlg)
  private
    sha1Engine: THashAlgSHA1Engine;
    context: SHA1_CTX;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Init(); override;
    procedure Update(const input: array of byte; const inputLen: cardinal); override;
    procedure Final(digest: THashValue); override;

    function  PrettyPrintHashValue(const theHashValue: THashValue): string; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgSHA1]);

end;

constructor THashAlgSHA1.Create(AOwner: TComponent);
begin
  inherited;

  sha1Engine:= THashAlgSHA1Engine.Create();

  fTitle := 'SHA-1';
  fHashLength := 160;
  fBlockLength := 512;

end;

destructor THashAlgSHA1.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  sha1Engine.SHA1Init(context);

  sha1Engine.Free();

  inherited;

end;

procedure THashAlgSHA1.Init();
begin
  sha1Engine.SHA1Init(context);
end;

procedure THashAlgSHA1.Update(const input: array of byte; const inputLen: cardinal);
begin
  sha1Engine.SHA1Update(context, input, inputLen);
end;

procedure THashAlgSHA1.Final(digest: THashValue);
begin
  sha1Engine.SHA1Final(digest, context);
end;


function THashAlgSHA1.PrettyPrintHashValue(const theHashValue: THashValue): string;
var
  retVal: string;
begin
  retVal := inherited PrettyPrintHashValue(theHashValue);

  insert(' ', retVal, 33);
  insert(' ', retVal, 25);
  insert(' ', retVal, 17);
  insert(' ', retVal, 9);

  Result := retVal;

end;


END.

