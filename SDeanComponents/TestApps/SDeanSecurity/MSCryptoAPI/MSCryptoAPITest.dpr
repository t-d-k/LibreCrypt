program MSCryptoAPITest;

uses
  Forms,
  frmMain_U in 'frmMain_U.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MS CryptoAPI Test';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
