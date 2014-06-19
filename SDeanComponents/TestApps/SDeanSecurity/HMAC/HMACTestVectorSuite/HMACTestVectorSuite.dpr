program HMACTestVectorSuite;

uses
  Forms,
  Form_Main in 'Form_Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HMAC: Test Vector Suite';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
