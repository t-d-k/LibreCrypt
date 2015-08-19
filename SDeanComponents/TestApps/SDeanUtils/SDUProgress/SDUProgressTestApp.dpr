program SDUProgressTestApp;

uses
  Forms,
  Main in 'Main.pas' {Form2},
  SDUForms in 'P:\src\PC\gui\common\SDUForms.pas' {SDUForm},
  dlgProgress in 'P:\src\PC\gui\common\SDeanUtils\dlgProgress.pas' {dlgProgress},
  lcConsts in 'P:\src\PC\gui\common\lcConsts.pas',
  lcTypes in 'P:\src\PC\gui\common\lcTypes.pas',
  SDUFrames in 'P:\src\PC\gui\common\SDUFrames.pas' {SDUFrame: TFrame},
  SDUSpin64Units in '..\..\..\Components\SDeanUtils\SDUSpin64Units.pas' {SDUSpin64Unit: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SDUProgress Test application';
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
