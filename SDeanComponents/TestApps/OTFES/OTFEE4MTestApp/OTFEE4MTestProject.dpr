program OTFEE4MTestProject;

uses
  Forms,
  OTFEE4MTestApp_U in 'OTFEE4MTestApp_U.pas' {OTFEE4MTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEE4MTestApp_F, OTFEE4MTestApp_F);
  Application.Run;
end.
