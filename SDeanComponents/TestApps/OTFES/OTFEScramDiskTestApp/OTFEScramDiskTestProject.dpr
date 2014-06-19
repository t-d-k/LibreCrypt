program OTFEScramDiskTestProject;

uses
  Forms,
  OTFEScramDiskTestApp_U in 'OTFEScramDiskTestApp_U.pas' {OTFEScramDiskTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEScramDiskTestApp_F, OTFEScramDiskTestApp_F);
  Application.Run;
end.
