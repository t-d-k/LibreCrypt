unit FreeOTFEExplorerfrmPropertiesDlg_Volume;

interface

uses
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, Forms,
  FreeOTFEExplorerfrmPropertiesDlg_Base, Gauges, Graphics, Messages, Series,
  StdCtrls, SysUtils, TeEngine, TeeProcs, Variants, Windows;

type
  TfrmPropertiesDialog_Volume = class (TfrmPropertiesDialog_Base)
    lblFileSystem:           TLabel;
    edFileSystem:            TLabel;
    Panel1:                  TPanel;
    lblUsedSpace:            TLabel;
    edUsedSpace_Bytes:       TLabel;
    edUsedSpace_UsefulUnits: TLabel;
    lblFreeSpace:            TLabel;
    edFreeSpace_Bytes:       TLabel;
    edFreeSpace_UsefulUnits: TLabel;
    pnlColorFreeSpace:       TPanel;
    pnlColorUsedSpace:       TPanel;
    Panel4:                  TPanel;
    lblCapacity:             TLabel;
    edCapacity_Bytes:        TLabel;
    edCapacity_UsefulUnits:  TLabel;
    chtSpaceChart:           TGauge;
    tsTools:                 TTabSheet;
    gbErrorChecking:         TGroupBox;
    Label1:                  TLabel;
    imgScandisk:             TImage;
    pbErrorCheckNow:         TButton;
    procedure FormShow(Sender: TObject);
    procedure pbErrorCheckNowClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  FreeOTFEExplorerCheckFilesystem,
  FreeOTFEExplorerfrmMain,
  OTFEFreeOTFEDLL_PartitionImage,
  SDPartitionImage_File, SDUGeneral,
  SDUGraphics,
  SDUi18n;

procedure TfrmPropertiesDialog_Volume.FormShow(Sender: TObject);
const
  COLOR_FREE_SPACE = clFuchsia;
  COLOR_USED_SPACE = clBlue;
var
  tmpIcon:     TIcon;
  spaceTotal:  ULONGLONG;
  spaceFree:   ULONGLONG;
  spaceUsed:   ULONGLONG;
  volFilename: String;
begin
  inherited;

  SetupCtlSeparatorPanel(Panel1);
  SetupCtlSeparatorPanel(Panel4);

  volFilename := '';
  if (Filesystem.PartitionImage is TOTFEFreeOTFEDLL_PartitionImage) then begin
    volFilename := TOTFEFreeOTFEDLL_PartitionImage(Filesystem.PartitionImage).Filename;
  end else
  if (Filesystem.PartitionImage is TSDPartitionImage_File) then begin
    volFilename := TSDPartitionImage_File(Filesystem.PartitionImage).Filename;
  end;

  self.Caption := SDUParamSubstitute(_('%1 Properties'),
    [ExtractFilename(
    volFilename)]);


  // Sort out drive icon...
  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, False, DLL_SHELL32_HDD, tmpIcon) then begin
      imgFileType.Picture.Assign(tmpIcon);
    end;
  finally
    tmpIcon.Free();
  end;

  lblFileType.Caption := _('Type:');
  edFileType.Caption  := '';
  if (Filesystem <> nil) then begin
    edFileType.Caption := RS_ENCRYPTED_VOLUME;
    if (Filesystem.PartitionImage is TSDPartitionImage_File) then begin
      edFileType.Caption := RS_VOLUME_PLAINTEXT;
    end;
  end;

  edFileSystem.Caption := Filesystem.FilesystemTitle();

  // Get rid of unused controls
  lblLocation.Visible   := False;
  edLocation.Visible    := False;
  lblSize.Visible       := False;
  edSize.Visible        := False;
  lblSizeOnDisk.Visible := False;
  edSizeOnDisk.Visible  := False;
  lblAttributes.Visible := False;
  ckReadOnly.Visible    := False;
  ckHidden.Visible      := False;
  ckArchive.Visible     := False;


  spaceTotal := Filesystem.Size;
  spaceFree  := Filesystem.FreeSpace;
  spaceUsed  := (spaceTotal - spaceFree);

  edUsedSpace_Bytes.Caption := SDUFormatWithThousandsSeparator(spaceUsed) +
    ' ' + UNITS_STORAGE_BYTES;
  edFreeSpace_Bytes.Caption := SDUFormatWithThousandsSeparator(spaceFree) +
    ' ' + UNITS_STORAGE_BYTES;
  edCapacity_Bytes.Caption  := SDUFormatWithThousandsSeparator(spaceTotal) +
    ' ' + UNITS_STORAGE_BYTES;
  edUsedSpace_Bytes.left    := (chtSpaceChart.left + chtSpaceChart.Width) - edUsedSpace_Bytes.Width;
  edFreeSpace_Bytes.left    := (chtSpaceChart.left + chtSpaceChart.Width) - edFreeSpace_Bytes.Width;
  edCapacity_Bytes.left     := (chtSpaceChart.left + chtSpaceChart.Width) - edCapacity_Bytes.Width;

  edUsedSpace_UsefulUnits.Caption := SDUFormatAsBytesUnits(spaceUsed, 2);
  edFreeSpace_UsefulUnits.Caption := SDUFormatAsBytesUnits(spaceFree, 2);
  edCapacity_UsefulUnits.Caption  := SDUFormatAsBytesUnits(spaceTotal, 2);
  edUsedSpace_UsefulUnits.left    := (Panel1.left + Panel1.Width) - edUsedSpace_UsefulUnits.Width;
  edFreeSpace_UsefulUnits.left    := (Panel1.left + Panel1.Width) - edFreeSpace_UsefulUnits.Width;
  edCapacity_UsefulUnits.left     := (Panel1.left + Panel1.Width) - edCapacity_UsefulUnits.Width;

  pnlColorUsedSpace.Caption    := '';
  pnlColorUsedSpace.BevelInner := bvLowered;
  pnlColorUsedSpace.BevelOuter := bvNone;
  pnlColorUsedSpace.Color      := COLOR_USED_SPACE;

  pnlColorFreeSpace.Caption    := '';
  pnlColorFreeSpace.BevelInner := bvLowered;
  pnlColorFreeSpace.BevelOuter := bvNone;
  pnlColorFreeSpace.Color      := COLOR_FREE_SPACE;
  chtSpaceChart.BackColor      := COLOR_FREE_SPACE;
  chtSpaceChart.ForeColor      := COLOR_USED_SPACE;
  //  chtSpaceChart.Series[0].Add(spaceFree, _('Free space'), COLOR_FREE_SPACE);
  //  chtSpaceChart.Series[0].Add(spaceUsed, _('Used space'), COLOR_USED_SPACE);


  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, False,
      DLL_SHELL32_SCANDISK, tmpIcon) then
    begin
      imgScandisk.Picture.Assign(tmpIcon);
    end;
  finally
    tmpIcon.Free();
  end;

end;


procedure TfrmPropertiesDialog_Volume.pbErrorCheckNowClick(Sender: TObject);
begin
  inherited;
  CheckFilesystem(Filesystem);

end;

end.
