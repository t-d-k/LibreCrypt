program OTFEFreeOTFETestProject;

uses
  Forms,
  OTFEFreeOTFETestApp_U in 'OTFEFreeOTFETestApp_U.pas' {OTFEFreeOTFETestApp_F},
  CriticalBlockTest_U in 'CriticalBlockTest_U.pas' {CriticalBlockTest_F},
  cryptlibAPITest_U in 'cryptlibAPITest_U.pas' {cryptlibAPITest},
  RNGTest_U in 'RNGTest_U.pas' {RNGTest},
  OTFE_U in 'P:\src\PC\gui\common\OTFE\OTFE\OTFE_U.pas',
  OTFEFreeOTFE_U in 'P:\src\PC\gui\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_U.pas',
  lcDebugLog in 'P:\src\PC\gui\common\lcDebugLog.pas',
  FreeOTFECypherDriverAPI in 'P:\src\PC\gui\common\OTFE\OTFEFreeOTFE\FreeOTFECypherDriverAPI.pas',
  FreeOTFEHashDriverAPI in 'P:\src\PC\gui\common\OTFE\OTFEFreeOTFE\FreeOTFEHashDriverAPI.pas',
  OTFEFreeOTFE_LUKSAPI in 'P:\src\PC\gui\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_LUKSAPI.pas',
  OTFEFreeOTFEBase_U in 'P:\src\PC\gui\common\OTFE\OTFEFreeOTFEBase_U.pas',
  pkcs11_library in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_library.pas',
  pkcs11_object in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_object.pas',
  pkcs11_session in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_session.pas',
  pkcs11_api in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_api.pas',
  pkcs11_slot in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_slot.pas',
  pkcs11_slot_event_thread in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_slot_event_thread.pas',
  pkcs11f in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11f.pas',
  PKCS11LibrarySelectDlg in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\PKCS11LibrarySelectDlg.pas' {PKCS11LibrarySelectDialog},
  pkcs11t in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11t.pas',
  pkcs11_token in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_token.pas',
  pkcs11_mechanism in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_mechanism.pas',
  pkcs11_attribute in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\pkcs11_attribute.pas',
  Shredder in 'P:\src\PC\gui\common\SDeanSecurity\Shredder\Shredder.pas',
  frmFileList in 'P:\src\PC\gui\common\SDeanSecurity\Shredder\frmFileList.pas' {frmFileList},
  PKCS11KnownLibs in 'P:\src\PC\gui\common\SDeanSecurity\PKCS#11\PKCS11KnownLibs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOTFEFreeOTFETestApp_F, OTFEFreeOTFETestApp_F);
  Application.Run;
end.
