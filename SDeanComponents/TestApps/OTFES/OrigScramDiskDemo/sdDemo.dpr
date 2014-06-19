program sdDemo;

uses
  Forms,
  sdMain in 'sdMain.pas' {MainForm},
  sdPassword in 'sdPassword.pas' {PasswordForm},
  sdGlobals in 'sdGlobals.pas',
  sdAbout in 'sdAbout.pas' {AboutForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TkrScramDisk Demonstration';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
