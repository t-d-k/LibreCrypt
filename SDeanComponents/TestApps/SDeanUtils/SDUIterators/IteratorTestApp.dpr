program IteratorTestApp;

uses
  Forms,
  IteratorTestApp_U in 'IteratorTestApp_U.pas' {IteratorTestApp_F};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIteratorTestApp_F, IteratorTestApp_F);
  Application.Run;
end.
