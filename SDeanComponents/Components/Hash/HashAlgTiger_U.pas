unit HashAlgTiger_U;
// Description: Tiger (Wrapper for the Tiger Hashing Engine)
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
  HashAlgTigerEngine_U;

type
  THashAlgTiger = class(THashAlg)
  private
    tigerEngine: THashAlgTigerEngine;
    context: Tiger_CTX;

    function  GetPasses(): integer;
    procedure SetPasses(passes: integer);
  protected
    { Protected declarations }
  public
    property passes: integer read GetPasses write SetPasses;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Init(); override;
    procedure Update(const input: array of byte; const inputLen: cardinal); override;
    procedure Final(digest: THashValue); override;

  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgTiger]);

end;

constructor THashAlgTiger.Create(AOwner: TComponent);
begin
  inherited;
  tigerEngine:= THashAlgTigerEngine.Create();

  fTitle := 'Tiger';
  fHashLength := 192;
  fBlockLength := 512;

  Passes := 3;

end;

destructor THashAlgTiger.Destroy();
begin
  // Nuke any existing context before freeing off the engine...
  tigerEngine.TigerInit(context);

  tigerEngine.Free();

  inherited;

end;

function  THashAlgTiger.GetPasses(): integer;
begin
  Result := tigerEngine.Passes;
end;

procedure THashAlgTiger.SetPasses(passes: integer);
begin
  tigerEngine.Passes := passes;
end;

procedure THashAlgTiger.Init();
begin
  tigerEngine.TigerInit(context);
end;

procedure THashAlgTiger.Update(const input: array of byte; const inputLen: cardinal);
begin
  tigerEngine.TigerUpdate(context, input, inputLen);
end;

procedure THashAlgTiger.Final(digest: THashValue);
begin
  tigerEngine.TigerFinal(digest, context);
end;


END.

