program AFSplitterTestApp;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'AFSplitter Test Application';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
