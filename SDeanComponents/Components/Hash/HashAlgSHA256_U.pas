unit HashAlgSHA256_U;
// Description: SHA-256 Hash (Wrapper for the SHA-256 Hashing Engine)
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
  HashAlgSHA256Engine_U;

type
  THashAlgSHA256 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA256Engine;
    context: SHA256_CTX;
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
  RegisterComponents('Hash', [THashAlgSHA256]);

end;

constructor THashAlgSHA256.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA256Engine.Create();

  fTitle := 'SHA-256';
  fHashLength := 256;
  fBlockLength := 512;

end;

destructor THashAlgSHA256.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  shaXXXEngine.SHA256Init(context);

  shaXXXEngine.Free();

  inherited;

end;

procedure THashAlgSHA256.Init();
begin
  shaXXXEngine.SHA256Init(context);
end;

procedure THashAlgSHA256.Update(const input: array of byte; const inputLen: cardinal);
begin
  shaXXXEngine.SHA256Update(context, input, inputLen);
end;

procedure THashAlgSHA256.Final(digest: THashValue);
begin
  shaXXXEngine.SHA256Final(digest, context);
end;


function THashAlgSHA256.PrettyPrintHashValue(const theHashValue: THashValue): string;
var
  retVal: string;
begin
  retVal := inherited PrettyPrintHashValue(theHashValue);

  insert(' ', retVal, 57);
  insert(' ', retVal, 49);
  insert(' ', retVal, 41);
  insert(' ', retVal, 33);
  insert(' ', retVal, 25);
  insert(' ', retVal, 17);
  insert(' ', retVal, 9);

  Result := retVal;

end;


END.

