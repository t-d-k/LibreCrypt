unit IteratorTestApp_U;
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
  StdCtrls,
  SDUFileIterator_U, SDUDirIterator_U, ComCtrls;

type
  TIteratorTestApp_F = class(TForm)
    edStartDir: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    pbCreateDirIterator: TButton;
    pbDirIterator: TButton;
    pbFileIterator: TButton;
    pbCreateFileIterator: TButton;
    ckReverseFormat: TCheckBox;
    cbRecurseSubDirs: TCheckBox;
    pbClear: TButton;
    pbExit: TButton;
    pbBrowse: TButton;
    pbCount: TButton;
    SDUDirIterator1: TSDUDirIterator;
    RichEdit1: TRichEdit;
    ckIncludeDirNames: TCheckBox;
    ckOmitStartDirPrefix: TCheckBox;
    SDUFileIterator1: TSDUFileIterator;
    procedure pbCreateDirIteratorClick(Sender: TObject);
    procedure pbDirIteratorClick(Sender: TObject);
    procedure pbCreateFileIteratorClick(Sender: TObject);
    procedure pbFileIteratorClick(Sender: TObject);
    procedure pbExitClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbCountClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IteratorTestApp_F: TIteratorTestApp_F;

implementation

{$R *.DFM}

{$IFDEF LINUX}
This software is only intended for use under MS Windows
{$ENDIF}
{$WARN UNIT_PLATFORM OFF}  // Useless warning about platform - we're already
                             // protecting against that!

uses FileCtrl;


procedure TIteratorTestApp_F.pbCreateDirIteratorClick(Sender: TObject);
begin
  SDUdiriterator1.Directory := edStartDir.text;
  SDUdiriterator1.ReverseFormat := ckReverseFormat.checked;
  SDUdiriterator1.Reset();

end;

procedure TIteratorTestApp_F.pbDirIteratorClick(Sender: TObject);
var
  dirname: string;
begin
  dirname := SDUdiriterator1.next();
  RichEdit1.lines.add(dirname);
  if (dirname = '') then
    begin
    showmessage('Finished.');
    end;

end;

procedure TIteratorTestApp_F.pbCreateFileIteratorClick(Sender: TObject);
begin
  SDUfileiterator1.Directory := edStartDir.text;
  SDUfileiterator1.RecurseSubDirs := cbRecurseSubDirs.checked;
  SDUfileiterator1.IncludeDirNames := ckIncludeDirNames.checked;
  SDUfileiterator1.OmitStartDirPrefix := ckOmitStartDirPrefix.checked;
  SDUfileiterator1.Reset();

end;

procedure TIteratorTestApp_F.pbFileIteratorClick(Sender: TObject);
var
  filename: string;
begin
  filename := SDUfileiterator1.next();
  RichEdit1.lines.add(filename);
  if (filename = '') then
    begin
    showmessage('Finished.');
    end;

end;

procedure TIteratorTestApp_F.pbExitClick(Sender: TObject);
begin
  Close();

end;

procedure TIteratorTestApp_F.pbBrowseClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select starting directory', '', dir) then
    begin
    edStartDir.text := dir;
    end;

end;

procedure TIteratorTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.clear();

end;

procedure TIteratorTestApp_F.pbCountClick(Sender: TObject);
begin
  RichEdit1.lines.add(inttostr(SDUfileiterator1.Count()));

end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

