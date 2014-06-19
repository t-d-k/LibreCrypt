program SDUURLLabelTestApp;

uses
  Forms,
  Main_U in 'Main_U.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SDUURLLabel Test Application';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
