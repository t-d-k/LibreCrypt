program SDUSystemTrayIconMinCloseToSystemTray;

uses
  Forms,
  Windows,
  Main in 'Main.pas' {Form1},
  SecondForm in 'SecondForm.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;

{$IFDEF VER185}
  // This software works regardless of the following fix:
  
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$ENDIF}

  Application.Title := 'Min/Close to Tasktray';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
