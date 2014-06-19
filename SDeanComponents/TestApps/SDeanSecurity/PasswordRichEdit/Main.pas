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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, PasswordRichedit;

type
  TfrmMain = class(TForm)
    edPasswordChar: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    edTest: TEdit;
    preTest: TPasswordRichEdit;
    pbClose: TButton;
    GroupBox2: TGroupBox;
    reReportTEdit: TRichEdit;
    reReportTPasswordRichEdit: TRichEdit;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ckHideClick(Sender: TObject);
    procedure edPasswordCharChange(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure edTestChange(Sender: TObject);
    procedure preTestChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetPasswordChar();
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  SDUGeneral;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Self.Caption := Application.Title;

  edPasswordChar.Text := '';

  preTest.WantReturns := TRUE;
  preTest.WordWrap := TRUE;

  preTest.Clear();
  preTest.Lines.Add('This is some sample text...');
  preTest.Lines.Add('...And some more.');

  edTest.Text := 'This is some sample text...';

  SetPasswordChar();

end;

procedure TfrmMain.ckHideClick(Sender: TObject);
begin
  SetPasswordChar();

end;


procedure TfrmMain.edPasswordCharChange(Sender: TObject);
begin
  SetPasswordChar();

end;

procedure TfrmMain.SetPasswordChar();
var
  passwordChar: char;
begin
  passwordChar := #0;
  if length(edPasswordChar.Text)>0 then
    begin
    passwordChar := edPasswordChar.Text[1];
    end;

  preTest.PasswordChar := passwordChar;
  edTest.PasswordChar := passwordChar;

end;


procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();

end;

procedure TfrmMain.edTestChange(Sender: TObject);
var
  prettyStrings: TStringList;
begin
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(edTest.Text, 0, length(edTest.Text), prettyStrings);
    reReportTEdit.Lines.Assign(prettyStrings);
  finally
    prettyStrings.Free();
  end;

end;

procedure TfrmMain.preTestChange(Sender: TObject);
var
  prettyStrings: TStringList;
begin
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(preTest.Text, 0, length(preTest.Text), prettyStrings);
    reReportTPasswordRichEdit.Lines.Assign(prettyStrings);
  finally
    prettyStrings.Free();
  end;

end;


END.


