program MouseRNGDialogTestApp;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TMouseRNGDialog Test App';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
