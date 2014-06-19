program HashTestVectorSuite;

uses
  Forms,
  Form_Main in 'Form_Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Hash Components: Test Vector Suite';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
