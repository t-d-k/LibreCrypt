program SDUWindows64TestApp;

uses
  Forms,
  formMain in 'formMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SDUWindows64TestApp';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
