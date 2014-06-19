program SDUSimplePieChart;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SDUSimplePieChart Test Application';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

