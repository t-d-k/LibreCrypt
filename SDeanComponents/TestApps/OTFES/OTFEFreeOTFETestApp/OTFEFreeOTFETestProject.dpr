program OTFEFreeOTFETestProject;

uses
  Forms,
  OTFEFreeOTFETestApp_U in 'OTFEFreeOTFETestApp_U.pas' {OTFEFreeOTFETestApp_F},
  CriticalBlockTest_U in 'CriticalBlockTest_U.pas' {CriticalBlockTest_F},
  cryptlibAPITest_U in 'cryptlibAPITest_U.pas' {cryptlibAPITest},
  RNGTest_U in 'RNGTest_U.pas' {RNGTest};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEFreeOTFETestApp_F, OTFEFreeOTFETestApp_F);
  Application.Run;
end.
