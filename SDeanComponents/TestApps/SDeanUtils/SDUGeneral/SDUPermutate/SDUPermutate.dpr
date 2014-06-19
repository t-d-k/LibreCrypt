program SDUPermutate;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Permutations Test';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
