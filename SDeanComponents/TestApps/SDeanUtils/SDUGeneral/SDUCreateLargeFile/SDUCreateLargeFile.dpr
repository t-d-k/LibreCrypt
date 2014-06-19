program SDUCreateLargeFile;

uses
  Forms,
  Main_U in 'Main_U.pas' {Main_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SDUCreateLargeFile Test Application';
  Application.CreateForm(TMain_F, Main_F);
  Application.Run;
end.
