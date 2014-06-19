program SDUSystemTrayIconBasicUsage;

uses
  Forms,
  Main_U in 'Main_U.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SDUSystemTrayIcon Test App';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
