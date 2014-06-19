program PasswordRichEditTestApp;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TPasswordRichEdit Test Application';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
