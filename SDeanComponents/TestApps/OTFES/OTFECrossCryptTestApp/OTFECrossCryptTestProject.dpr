program OTFECrossCryptTestProject;

uses
  Forms,
  OTFECrossCryptTestApp_U in 'OTFECrossCryptTestApp_U.pas' {OTFECrossCryptTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFECrossCryptTestApp_F, OTFECrossCryptTestApp_F);
  Application.Run;
end.
