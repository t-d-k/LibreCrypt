program OTFEUnifiedTestProject;

uses
  Forms,
  OTFEUnifiedTestApp_U in 'OTFEUnifiedTestApp_U.pas' {OTFEUnifiedTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEUnifiedTestApp_F, OTFEUnifiedTestApp_F);
  Application.Run;
end.
