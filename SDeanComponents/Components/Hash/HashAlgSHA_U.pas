unit HashAlgSHA_U;
// Description: SHA Hash (Wrapper for the SHA Hashing Engine)
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
  HashAlgSHAEngine_U;

type
  THashAlgSHA = class(THashAlg)
  private
    shaEngine: THashAlgSHAEngine;
    context: SHA_CTX;
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
  RegisterComponents('Hash', [THashAlgSHA]);
end;

constructor THashAlgSHA.Create(AOwner: TComponent);
begin
  inherited;
  shaEngine:= THashAlgSHAEngine.Create();

  fTitle := 'SHA';
  fHashLength := 160;
  fBlockLength := 512;

end;

destructor THashAlgSHA.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  shaEngine.SHAInit(context);

  shaEngine.Free();

  inherited;

end;

procedure THashAlgSHA.Init();
begin
  shaEngine.SHAInit(context);
end;

procedure THashAlgSHA.Update(const input: array of byte; const inputLen: cardinal);
begin
  shaEngine.SHAUpdate(context, input, inputLen);
end;

procedure THashAlgSHA.Final(digest: THashValue);
begin
  shaEngine.SHAFinal(digest, context);
end;


function THashAlgSHA.PrettyPrintHashValue(const theHashValue: THashValue): string;
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

