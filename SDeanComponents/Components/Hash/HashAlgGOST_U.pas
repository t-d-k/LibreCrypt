unit HashAlgGOST_U;
// Description: GOST R 34.11-94 (Wrapper for the GOST Hashing Engine)
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
  HashAlgGOSTEngine_U;

type
  THashAlgGOST = class(THashAlg)
  private
    GOSTEngine: THashAlgGOSTEngine;
    context: GostHashCtx;
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


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgGOST]);

end;

constructor THashAlgGOST.Create(AOwner: TComponent);
begin
  inherited;

  GOSTEngine:= THashAlgGOSTEngine.Create();
  GOSTEngine.gosthash_init();

  fTitle := 'GOST R 34.11-94';
  fHashLength := 256;
  fBlockLength := 256;

end;

destructor THashAlgGOST.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  GOSTEngine.gosthash_reset(context);

  GOSTEngine.Free();

  inherited;

end;

procedure THashAlgGOST.Init();
begin
  GOSTEngine.gosthash_reset(context);
end;

procedure THashAlgGOST.Update(const input: array of byte; const inputLen: cardinal);
begin
  GOSTEngine.gosthash_update(context, input, inputLen);
end;

procedure THashAlgGOST.Final(digest: THashValue);
begin
  GOSTEngine.gosthash_final(context, digest);
end;


function THashAlgGOST.PrettyPrintHashValue(const theHashValue: THashValue): string;
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

