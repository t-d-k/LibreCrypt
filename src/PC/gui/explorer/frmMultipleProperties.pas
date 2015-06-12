unit frmMultipleProperties;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms,
  frmProperties, Graphics, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmMultipleProperties = class (TfrmProperties)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    fMultipleItems: TStringList;
    fParentDir:     WideString;
  end;


implementation

{$R *.dfm}

uses
  SDFilesystem_FAT, SDUGeneral,
  SDUGraphics,
  SDUi18n;

procedure TfrmMultipleProperties.FormCreate(Sender: TObject);
begin
  inherited;
  fMultipleItems := TStringList.Create();
end;

procedure TfrmMultipleProperties.FormDestroy(Sender: TObject);
begin
  fMultipleItems.Free();

  inherited;
end;

procedure TfrmMultipleProperties.FormShow(Sender: TObject);
var
  totalSize:              ULONGLONG;
  totalDirCnt:            Integer;
  totalFileCnt:           Integer;
  tmpIcon:                TIcon;
  tmpSize:                ULONGLONG;
  tmpDirCnt:              Integer;
  tmpFileCnt:             Integer;
  i:                      Integer;
  allOK:                  Boolean;
  filenamesOnlyCommaText: String;
begin
  inherited;

  for i := 0 to (fMultipleItems.Count - 1) do begin
    if (filenamesOnlyCommaText <> '') then begin
      filenamesOnlyCommaText := filenamesOnlyCommaText + ', ';
    end;

    filenamesOnlyCommaText := filenamesOnlyCommaText + ExtractFilename(fMultipleItems[i]);
  end;
  self.Caption := Format(_('%s Properties'), [filenamesOnlyCommaText]);


  totalSize    := 0;
  totalDirCnt  := 0;
  totalFileCnt := 0;
  for i := 0 to (fMultipleItems.Count - 1) do begin
    allOK := fFilesystem.ItemSize(fMultipleItems[i], tmpSize, tmpDirCnt, tmpFileCnt);
    if not (allOK) then begin
      break;
    end else begin
      totalSize    := totalSize + tmpSize;
      totalDirCnt  := totalDirCnt + tmpDirCnt;
      totalFileCnt := totalFileCnt + tmpFileCnt;
    end;

  end;

  // Change borders on "filename"; user can *never* edit it, so make it look
  // more like a label
  edFilename.BevelOuter  := bvNone;
  edFilename.BevelInner  := bvNone;
  edFilename.BorderStyle := bsNone;
  edFilename.Text        := Format(_('%d Files, %d Folders'),
    [totalFileCnt, totalDirCnt]);
  edFileType.Caption     := _('Multiple types');
  edLocation.Caption     := Format(_('All in %s'), [fParentDir]);
  edSize.Caption         := Format(_('%s (%s bytes)'),
    [SDUFormatAsBytesUnits(totalSize, 2), SDUIntToStrThousands(totalSize)]);


  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, False, DLL_SHELL32_MULTIPLE_FILES, tmpIcon) then
    begin
      imgFileType.Picture.Assign(tmpIcon);
    end;
  finally
    tmpIcon.Free();
  end;

end;

end.
