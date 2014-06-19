unit FreeOTFEExplorerfrmPropertiesDlg_Multiple;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FreeOTFEExplorerfrmPropertiesDlg_Base, ExtCtrls, StdCtrls, ComCtrls;

type
  TfrmPropertiesDialog_Multiple = class(TfrmPropertiesDialog_Base)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    MultipleItems: TStringList;
    ParentDir: WideString;
  end;


implementation

{$R *.dfm}

uses
  SDUGeneral,
  SDUGraphics,
  SDUi18n,
  SDFilesystem_FAT;

procedure TfrmPropertiesDialog_Multiple.FormCreate(Sender: TObject);
begin
  inherited;
  MultipleItems := TStringList.Create();
end;

procedure TfrmPropertiesDialog_Multiple.FormDestroy(Sender: TObject);
begin
  MultipleItems.Free();
  
  inherited;
end;

procedure TfrmPropertiesDialog_Multiple.FormShow(Sender: TObject);
var
  totalSize: ULONGLONG;
  totalDirCnt: integer;
  totalFileCnt: integer;
  tmpIcon: TIcon;
  tmpSize: ULONGLONG;
  tmpDirCnt: integer;
  tmpFileCnt: integer;
  i: integer;
  allOK: boolean;
  filenamesOnlyCommaText: string;
begin
  inherited;

  for i:=0 to (MultipleItems.count - 1) do
    begin
    if (filenamesOnlyCommaText <> '') then
      begin
      filenamesOnlyCommaText := filenamesOnlyCommaText + ', ';
      end;

    filenamesOnlyCommaText := filenamesOnlyCommaText + ExtractFilename(MultipleItems[i]);
    end;
  self.Caption := SDUParamSubstitute(
                                     _('%1 Properties'),
                                     [filenamesOnlyCommaText]
                                    );


  totalSize    := 0;
  totalDirCnt  := 0;
  totalFileCnt := 0;
  for i:=0 to (MultipleItems.count - 1) do
    begin
    allOK := Filesystem.ItemSize(MultipleItems[i], tmpSize, tmpDirCnt, tmpFileCnt);
    if not(allOK) then
      begin
      break;
      end
    else
      begin
      totalSize := totalSize + tmpSize;
      totalDirCnt := totalDirCnt + tmpDirCnt;
      totalFileCnt := totalFileCnt + tmpFileCnt;
      end;
      
    end;

  // Change borders on "filename"; user can *never* edit it, so make it look
  // more like a label
  edFilename.BevelOuter := bvNone;
  edFilename.BevelInner := bvNone;
  edFilename.BorderStyle := bsNone;
  edFilename.Text := SDUParamSubstitute(
                                        _('%1 Files, %2 Folders'),
                                        [
                                         totalFileCnt,
                                         totalDirCnt
                                        ]
                                       );
  edFileType.Caption := _('Multiple types');
  edLocation.Caption := SDUParamSubstitute(_('All in %1'), [ParentDir]);
  edSize.Caption   := SDUParamSubstitute(
                                      _('%1 (%2 bytes)'),
                                      [
                                       SDUFormatAsBytesUnits(totalSize, 2),
                                       SDUIntToStrThousands(totalSize)
                                      ]
                                     );


  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(
                      DLL_SHELL32,
                      FALSE,
                      DLL_SHELL32_MULTIPLE_FILES,
                      tmpIcon
                     ) then
      begin
      imgFileType.Picture.Assign(tmpIcon)
      end;
  finally
    tmpIcon.Free();
  end;

end;

END.

