unit HashAlgSHA384_U;
// Description: SHA-384 Hash (Wrapper for the SHA-384 Hashing Engine)
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
  HashAlgSHA384Engine_U;

type
  THashAlgSHA384 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA384Engine;
    context: SHA384_CTX;
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
  RegisterComponents('Hash', [THashAlgSHA384]);

end;

constructor THashAlgSHA384.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA384Engine.Create();

  fTitle := 'SHA-384';
  fHashLength := 384;
  fBlockLength := 1024;

end;

destructor THashAlgSHA384.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  shaXXXEngine.SHA384Init(context);

  shaXXXEngine.Free();

  inherited;

end;

procedure THashAlgSHA384.Init();
begin
  shaXXXEngine.SHA384Init(context);
end;

procedure THashAlgSHA384.Update(const input: array of byte; const inputLen: cardinal);
begin
  shaXXXEngine.SHA384Update(context, input, inputLen);
end;

procedure THashAlgSHA384.Final(digest: THashValue);
begin
  shaXXXEngine.SHA384Final(digest, context);
end;


function THashAlgSHA384.PrettyPrintHashValue(const theHashValue: THashValue): string;
var
  retVal: string;
begin
  retVal := inherited PrettyPrintHashValue(theHashValue);

  insert(' ', retVal, 81);
  insert(' ', retVal, 65);
  insert(' ', retVal, 49);
  insert(' ', retVal, 33);
  insert(' ', retVal, 17);

  Result := retVal;

end;


END.

