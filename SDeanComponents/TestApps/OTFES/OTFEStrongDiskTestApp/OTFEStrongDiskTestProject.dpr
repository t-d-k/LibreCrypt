program OTFEStrongDiskTestProject;

uses
  Forms,
  OTFEStrongDiskTestApp_U in 'OTFEStrongDiskTestApp_U.pas' {OTFEStrongDiskTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEStrongDiskTestApp_F, OTFEStrongDiskTestApp_F);
  Application.Run;
end.
