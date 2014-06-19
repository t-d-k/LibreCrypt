unit Main;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    pbPermutate: TButton;
    reReport: TRichEdit;
    edPool: TEdit;
    lblPermutations: TLabel;
    Label1: TLabel;
    pbSaveAs: TButton;
    SaveDialog1: TSDUSaveDialog;
    procedure pbPermutateClick(Sender: TObject);
    procedure edPoolChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbSaveAsClick(Sender: TObject);
  private
    { Private declarations }
  protected
    function PermutationCount(pool: string): string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  SDUGeneral;

procedure TfrmMain.pbPermutateClick(Sender: TObject);
const
  // The number of characters in the pool before the user gets warned that it
  // could take awhile...
  WARN_POOL_SIZE = 8;
var
  stlPermutations: TStringList;
  csrBegin: TCursor;
begin
  if (length(edPool.Text) >= WARN_POOL_SIZE) then
    begin
    if (MessageDlg(
                  'There are '+PermutationCount(edPool.text)+' permutations of "'+edPool.text+'".'+SDUCRLF+
                  SDUCRLF+
                  'It could take a while to generate them all.'+SDUCRLF+
                  SDUCRLF+
                  'Do you wish to proceed?', mtWarning, [mbYes, mbNo], 0) = mrNo) then
      begin
      exit;
      end;
    end;

  reReport.lines.clear();

  csrBegin := Screen.Cursor;
  stlPermutations:= TStringList.Create();
  try
    Screen.Cursor := crHourGlass;
    SDUPermutate(edPool.Text, stlPermutations);
    
    reReport.lines.Add('Permutated string: '+edPool.Text);
    reReport.lines.Add('Permutations: '+inttostr(stlPermutations.count));
    reReport.lines.AddStrings(stlPermutations);

    pbSaveAs.Enabled := TRUE;

  finally
    stlPermutations.Free();
    Screen.Cursor := csrBegin;
  end;

end;


function TfrmMain.PermutationCount(pool: string): string;
var
  li: LARGE_INTEGER;
  strPermCount: string;
begin
  try
    li := SDUFactorial(length(pool));
    strPermCount := inttostr(li.QuadPart);
  except
    // Just in case we overflowed...
    // Note: Project must be compiled with Project Options (compiler tab)
    //       overflow checking turned *on* for this exception to be raised
    on EIntOverflow do
      begin
      strPermCount := 'a large number(!) of'
      end;

  end;
  
  Result := strPermCount;
end;

procedure TfrmMain.edPoolChange(Sender: TObject);
var
  strPermCount: string;
begin
  strPermCount := PermutationCount(edPool.text);
  lblPermutations.caption := inttostr(length(edPool.text)) +' characters have '+ strPermCount +' permutations';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Update permutations count display...
  edPoolChange(nil);

  reReport.Clear();
  
  self.Caption := Application.Title;
  reReport.PlainText := TRUE;
  pbSaveAs.Enabled := FALSE;

end;

procedure TfrmMain.pbSaveAsClick(Sender: TObject);
begin
  if (SaveDialog1.Execute()) then
    begin
    reReport.Lines.SaveToFile(SaveDialog1.Filename);
    end;

end;

END.


