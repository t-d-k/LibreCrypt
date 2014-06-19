unit HashAlgSHA512_U;
// Description: SHA-512 Hash (Wrapper for the SHA-512 Hashing Engine)
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
  HashAlgSHA512Engine_U;

type
  THashAlgSHA512 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA512Engine;
    context: SHA512_CTX;
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
  RegisterComponents('Hash', [THashAlgSHA512]);

end;

constructor THashAlgSHA512.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA512Engine.Create();

  fTitle := 'SHA-512';
  fHashLength := 512;
  fBlockLength := 1024;

end;

destructor THashAlgSHA512.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  shaXXXEngine.SHA512Init(context);

  shaXXXEngine.Free();

  inherited;

end;

procedure THashAlgSHA512.Init();
begin
  shaXXXEngine.SHA512Init(context);
end;

procedure THashAlgSHA512.Update(const input: array of byte; const inputLen: cardinal);
begin
  shaXXXEngine.SHA512Update(context, input, inputLen);
end;

procedure THashAlgSHA512.Final(digest: THashValue);
begin
  shaXXXEngine.SHA512Final(digest, context);
end;


function THashAlgSHA512.PrettyPrintHashValue(const theHashValue: THashValue): string;
var
  retVal: string;
begin
  retVal := inherited PrettyPrintHashValue(theHashValue);

  insert(' ', retVal, 113);
  insert(' ', retVal, 97);
  insert(' ', retVal, 81);
  insert(' ', retVal, 65);
  insert(' ', retVal, 49);
  insert(' ', retVal, 33);
  insert(' ', retVal, 17);

  Result := retVal;

end;


END.

