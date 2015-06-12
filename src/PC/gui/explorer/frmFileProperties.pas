unit frmFileProperties;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms,
  frmProperties, Graphics, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmFileProperties = class (TfrmProperties)
    edOpensWith:         TLabel;
    Image1:              TImage;
    Label9:              TLabel;
    Panel1:              TPanel;
    edTimestampCreated:  TLabel;
    Label5:              TLabel;
    Label1:              TLabel;
    edTimestampModified: TLabel;
    Label11:             TLabel;
    Panel4:              TPanel;
    edTimestampAccessed: TLabel;
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
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

procedure TfrmFileProperties.FormShow(Sender: TObject);
var
  item:        TSDDirItem_FAT;
  DLLFilename: String;
  DLLIconIdx:  Integer;
  tmpIcon:     TIcon;
begin
  inherited;

  self.Caption := Format(_('%s Properties'), [ExtractFilename(
    fPathAndFilename)]);

  SetupCtlSeparatorPanel(Panel1);
  SetupCtlSeparatorPanel(Panel4);

  item := TSDDirItem_FAT.Create();
  try
    if fFilesystem.GetItem_FAT(fPathAndFilename, item) then begin
      edFilename.Text     := ExtractFilename(fPathAndFilename);
      edFileType.Caption  := SDUGetFileType_Description(fPathAndFilename);
      edOpensWith.Caption := '';
      edLocation.Caption  :=
        ExcludeTrailingPathDelimiter(ExtractFilePath(fPathAndFilename));
      if (edLocation.Caption = '') then begin
        // Don't strip off trailing "\" if it's just "\"
        edLocation.Caption := ExtractFilePath(fPathAndFilename);
      end;
      edSize.Caption :=
        SDUParamSubstitute(_('%1 (%2 bytes)'),
        [SDUFormatAsBytesUnits(item.Size, 2), SDUIntToStrThousands(
        item.Size)]);

      edTimestampCreated.Caption  := DateTimeToStr(TimeStampToDateTime(item.TimestampCreation));
      edTimestampModified.Caption :=
        DateTimeToStr(TimeStampToDateTime(item.TimestampLastModified));
      edTimestampAccessed.Caption := DateToStr(item.DatestampLastAccess);

      ckReadOnly.Checked := item.IsReadonly;
      ckHidden.Checked   := item.IsHidden;
      ckArchive.Checked  := item.IsArchive;
    end;

  finally
    item.Free();
  end;


  tmpIcon := TIcon.Create();
  try
    SDUGetFileType_Icon(fPathAndFilename, DLLFilename, DLLIconIdx);
    if (DLLFilename = '') then begin
      DLLFilename := DLL_SHELL32;
      DLLIconIdx  := DLL_SHELL32_DEFAULT_FILE;
    end;
    if SDULoadDLLIcon(DLLFilename, False, DLLIconIdx, tmpIcon) then begin
      imgFileType.Picture.Assign(tmpIcon);
    end;
  finally
    tmpIcon.Free();
  end;

end;

procedure TfrmFileProperties.pbOKClick(Sender: TObject);
begin
  inherited;

  ModalResult := mrOk;
end;


end.
