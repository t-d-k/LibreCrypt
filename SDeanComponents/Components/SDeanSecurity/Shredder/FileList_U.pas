unit FileList_U;
// Description: Main Unit, Dialog
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, SDUForms;

type
  TFileList_F = class(TSDUForm)
    pbOK: TButton;
    lblTitle: TLabel;
    pnlPlaceholder: TPanel;
    lbFiles: TListBox;
    reReport: TRichEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileList_F: TFileList_F;

implementation

{$R *.DFM}

uses SDUGeneral;


procedure TFileList_F.FormShow(Sender: TObject);
var
  i: integer;
begin
  pnlPlaceholder.BevelOuter := bvNone;
  lbFiles.align := alClient;
  reReport.align := alClient;

  for i:=0 to (lbFiles.items.count-1) do
    begin
    lbFiles.items[i] := SDUConvertSFNToLFN(lbFiles.items[i]);
    end;

end;

END.

