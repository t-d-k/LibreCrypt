program SimpleHash;

uses
  Forms,
  Form_Main in 'Form_Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SimpleHash';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
