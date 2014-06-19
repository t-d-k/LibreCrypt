program TigerHashTest;

uses
  Forms,
  frmTigerTest_U in 'frmTigerTest_U.pas' {frmTigerTest},
  NESSIESet3 in 'NESSIESet3.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Tiger Hash Test';
  Application.CreateForm(TfrmTigerTest, frmTigerTest);
  Application.Run;
end.
