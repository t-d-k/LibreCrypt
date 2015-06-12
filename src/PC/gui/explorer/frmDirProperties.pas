unit frmDirProperties;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms,
  frmProperties, Graphics, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmDirProperties = class (TfrmProperties)
    edContains:         TLabel;
    Label1:             TLabel;
    edTimestampCreated: TLabel;
    Label4:             TLabel;
    Panel1:             TPanel;
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    fPathAndFilename: WideString;
  end;


implementation

{$R *.dfm}

uses
  SDFilesystem_FAT, SDUGeneral,
  SDUGraphics,
  SDUi18n;

procedure TfrmDirProperties.FormShow(Sender: TObject);
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

  self.Caption := Format(_('%s Properties'), [ExtractFilename(fPathAndFilename)]);

  SetupCtlSeparatorPanel(Panel1);

  if fFilesystem.ItemSize(fPathAndFilename, totalSize, dirCnt, fileCnt) then begin
    edFilename.Text    := ExtractFilename(fPathAndFilename);
    edFileType.Caption := SDUGetFileType_Description(FILE_TYPE_DIRECTORY);
    edLocation.Caption := ExcludeTrailingPathDelimiter(ExtractFilePath(fPathAndFilename));
    if (edLocation.Caption = '') then begin
      // Don't strip off trailing "\" if it's just "\"
      edLocation.Caption := ExtractFilePath(fPathAndFilename);
    end;
    edSize.Caption := Format(_('%s (%s bytes)'),
      [SDUFormatAsBytesUnits(totalSize, 2), SDUIntToStrThousands(totalSize)]);

    edContains.Caption := Format(_('%d Files, %d Folders'), [fileCnt, dirCnt]);
  end;


  item := TSDDirItem_FAT.Create();
  try
    if fFilesystem.GetItem_FAT(fPathAndFilename, item) then begin
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
