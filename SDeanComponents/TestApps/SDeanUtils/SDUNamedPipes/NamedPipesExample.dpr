program NamedPipesExample;

uses
  Forms,
  Main_U in 'Main_U.pas' {Main_F},
  SDUNamedPipe_U;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain_F, Main_F);
  Application.Run;
end.
