unit FreeOTFEExplorerfrmPropertiesDlg_Directory;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms,
  FreeOTFEExplorerfrmPropertiesDlg_Base, Graphics, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmPropertiesDialog_Directory = class (TfrmPropertiesDialog)
    edContains:         TLabel;
    Label1:             TLabel;
    edTimestampCreated: TLabel;
    Label4:             TLabel;
    Panel1:             TPanel;
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    PathAndFilename: WideString;
  end;


implementation

{$R *.dfm}

uses
  SDFilesystem_FAT, SDUGeneral,
  SDUGraphics,
  SDUi18n;

procedure TfrmPropertiesDialog_Directory.FormShow(Sender: TObject);
var
  totalSize:   ULONGLONG;
  dirCnt:      Integer;
  fileCnt:     Integer;
  item:        TSDDirItem_FAT;
  DLLFilename: String;
  DLLIconIdx:  Integer;
  tmpIcon:     TIcon;
begin
  inherited;

  self.Caption := SDUParamSubstitute(_('%1 Properties'), [ExtractFilename(PathAndFilename)]);

  SetupCtlSeparatorPanel(Panel1);

  if Filesystem.ItemSize(PathAndFilename, totalSize, dirCnt, fileCnt) then begin
    edFilename.Text    := ExtractFilename(PathAndFilename);
    edFileType.Caption := SDUGetFileType_Description(FILE_TYPE_DIRECTORY);
    edLocation.Caption := ExcludeTrailingPathDelimiter(ExtractFilePath(PathAndFilename));
    if (edLocation.Caption = '') then begin
      // Don't strip off trailing "\" if it's just "\"
      edLocation.Caption := ExtractFilePath(PathAndFilename);
    end;
    edSize.Caption := SDUParamSubstitute(_('%1 (%2 bytes)'),
      [SDUFormatAsBytesUnits(totalSize, 2), SDUIntToStrThousands(totalSize)]);

    edContains.Caption := SDUParamSubstitute(_('%1 Files, %2 Folders'), [fileCnt, dirCnt]);
  end;


  item := TSDDirItem_FAT.Create();
  try
    if Filesystem.GetItem_FAT(PathAndFilename, item) then begin
      edTimestampCreated.Caption := '';
      try
        edTimestampCreated.Caption := DateTimeToStr(TimeStampToDateTime(item.TimestampCreation));
      except
        // Swallow exception - if processing for the root dir, this will be set
        // to zero, raising an exception
      end;

      ckReadOnly.Checked := item.IsReadonly;
      ckHidden.Checked   := item.IsHidden;
      ckArchive.Checked  := item.IsArchive;
    end;
  finally
    item.Free();
  end;


  tmpIcon := TIcon.Create();
  try
    SDUGetFileType_Icon(FILE_TYPE_DIRECTORY, DLLFilename, DLLIconIdx);
    if (DLLFilename = '') then begin
      DLLFilename := DLL_SHELL32;
      DLLIconIdx  := DLL_SHELL32_FOLDER_CLOSED;
    end;
    if SDULoadDLLIcon(DLLFilename, False, DLLIconIdx, tmpIcon) then begin
      imgFileType.Picture.Assign(tmpIcon);
    end;
  finally
    tmpIcon.Free();
  end;

end;

end.
