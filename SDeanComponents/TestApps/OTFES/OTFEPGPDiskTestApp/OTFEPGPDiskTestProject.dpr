program OTFEPGPDiskTestProject;

uses
  Forms,
  OTFEPGPDiskTestApp_U in 'OTFEPGPDiskTestApp_U.pas' {OTFEPGPDiskTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEPGPDiskTestApp_F, OTFEPGPDiskTestApp_F);
  Application.Run;
end.
