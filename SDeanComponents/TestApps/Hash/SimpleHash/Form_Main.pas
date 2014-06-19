unit Form_Main;
// Description: Simple Hash
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// A simple demonstation application of how to use the hash components


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HashAlg_U, HashAlgMD5_U;

type
  TfrmMain = class(TForm)
    edText: TEdit;
    pbHashInput: TButton;
    edFilename: TEdit;
    pbBrowse: TButton;
    rbHashText: TRadioButton;
    rbHashFile: TRadioButton;
    mmoReport: TMemo;
    pbClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    hashAlg: THashAlgMD5;
    OpenDialog1: TOpenDialog;
    procedure pbCloseClick(Sender: TObject);
    procedure pbHashInputClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function HashText(userText: string): string;
    function HashFile(filename: string): string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  HashValue_U,
  SDUGeneral;

  
procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
end;

function  TfrmMain.HashText(userText: string): string;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    hashAlg.HashString(userText, hashValue);
    Result := hashAlg.PrettyPrintHashValue(hashValue);
  finally
    hashValue.Free();
  end;

end;

function  TfrmMain.HashFile(filename: string): string;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    hashAlg.HashFile(filename, hashValue);
    Result := hashAlg.PrettyPrintHashValue(hashValue);
  finally
    hashValue.Free();
  end;

end;


procedure TfrmMain.pbHashInputClick(Sender: TObject);
begin
  mmoReport.Lines.Add('The '+hashAlg.Title+' hash of:');
  if rbHashFile.checked then
    begin
    mmoReport.Lines.Add(edFilename.text);
    mmoReport.Lines.Add('is:');
    mmoReport.Lines.Add(HashFile(edFilename.text));
    end
  else
    begin
    mmoReport.Lines.Add(edText.text);
    mmoReport.Lines.Add('is:');
    mmoReport.Lines.Add(HashText(edText.text));
    end;

  mmoReport.Lines.Add('');

end;

procedure TfrmMain.pbBrowseClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(OpenDialog1, edFilename.Text);
  if OpenDialog1.Execute then
    begin
    edFilename.Text := OpenDialog1.Filename;
    rbHashFile.Checked := TRUE;
    end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.Caption := Application.Title;
  mmoReport.Lines.Clear();
  edText.Text := 'abc';
  edFilename.Text := '';

end;

END.


