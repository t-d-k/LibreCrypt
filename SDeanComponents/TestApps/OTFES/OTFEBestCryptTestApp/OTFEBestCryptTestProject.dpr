program OTFEBestCryptTestProject;

uses
  Forms,
  OTFEBestCryptTestApp_U in 'OTFEBestCryptTestApp_U.pas' {BestCryptTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBestCryptTestApp_F, BestCryptTestApp_F);
  Application.Run;
end.
