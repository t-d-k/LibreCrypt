program TestPasswodFrameP;

uses
  Vcl.Forms,
  TestPasswordFrame in 'TestPasswordFrame.pas' {Form1},
  SDUFrames in 'SDUFrames.pas' {SDUFrame: TFrame},
  fmePassword in 'fmePassword.pas' {frmePassword: TFrame},
  MainSettings in '..\main\MainSettings.pas',
  SDUGeneral in 'SDeanUtils\SDUGeneral.pas',
  lcDialogs in 'SDeanUtils\lcDialogs.pas',
  Spin64 in 'P:\SDeanComponents\Components\SDeanUtils\Spin64.pas',
  SDUSpin64Units in 'P:\SDeanComponents\Components\SDeanUtils\SDUSpin64Units.pas' {SDUSpin64Unit: TFrame},
  dlgProgress in 'SDeanUtils\dlgProgress.pas' {dlgProgress};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  MainSettings.gSettings := TMainSettings.Create();
//  MainSettings.gSettings.OptShowPasswords := true;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
