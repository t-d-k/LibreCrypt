program OTFETrueCryptTestProject;

uses
  Forms,
  OTFETrueCryptTestApp_U in 'OTFETrueCryptTestApp_U.pas' {OTFETrueCryptTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFETrueCryptTestApp_F, OTFETrueCryptTestApp_F);
  Application.Run;
end.
