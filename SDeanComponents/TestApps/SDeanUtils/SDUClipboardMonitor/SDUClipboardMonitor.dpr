program SDUClipboardMonitor;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clipboard Monitor';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
