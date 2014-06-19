program SDUDropFilesTestApp;

uses
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'SDUDropFiles Test Application';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
