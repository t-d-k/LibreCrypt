unit FreeOTFEExplorerfrmPropertiesDlg_Volume;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FreeOTFEExplorerfrmPropertiesDlg_Base, ExtCtrls, StdCtrls, ComCtrls,
  TeEngine, Series, TeeProcs, Chart;

type
  TfrmPropertiesDialog_Volume = class(TfrmPropertiesDialog_Base)
    lblFileSystem: TLabel;
    edFileSystem: TLabel;
    Panel1: TPanel;
    lblUsedSpace: TLabel;
    edUsedSpace_Bytes: TLabel;
    edUsedSpace_UsefulUnits: TLabel;
    lblFreeSpace: TLabel;
    edFreeSpace_Bytes: TLabel;
    edFreeSpace_UsefulUnits: TLabel;
    pnlColorFreeSpace: TPanel;
    pnlColorUsedSpace: TPanel;
    Panel4: TPanel;
    lblCapacity: TLabel;
    edCapacity_Bytes: TLabel;
    edCapacity_UsefulUnits: TLabel;
    chtSpaceChart: TChart;
    Series1: TPieSeries;
    tsTools: TTabSheet;
    gbErrorChecking: TGroupBox;
    Label1: TLabel;
    imgScandisk: TImage;
    pbErrorCheckNow: TButton;
    procedure FormShow(Sender: TObject);
    procedure pbErrorCheckNowClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGraphics,
  SDUGeneral,
  FreeOTFEExplorerfrmMain,
  FreeOTFEExplorerCheckFilesystem,
  OTFEFreeOTFEDLL_PartitionImage,
  SDPartitionImage_File;


procedure TfrmPropertiesDialog_Volume.FormShow(Sender: TObject);
const
  COLOR_FREE_SPACE = clFuchsia;
  COLOR_USED_SPACE = clBlue;
var
  tmpIcon: TIcon;
  spaceTotal: ULONGLONG;
  spaceFree: ULONGLONG;
  spaceUsed: ULONGLONG;
  volFilename: string;
begin
  inherited;

  SetupCtlSeparatorPanel(Panel1);
  SetupCtlSeparatorPanel(Panel4);

  volFilename := '';
  if (Filesystem.PartitionImage is TOTFEFreeOTFEDLL_PartitionImage) then
    begin
    volFilename := TOTFEFreeOTFEDLL_PartitionImage(Filesystem.PartitionImage).Filename;
    end
  else if (Filesystem.PartitionImage is TSDPartitionImage_File) then
    begin
    volFilename := TSDPartitionImage_File(Filesystem.PartitionImage).Filename;
    end;

  self.Caption := SDUParamSubstitute(
                                     _('%1 Properties'),
                                     [ExtractFilename(volFilename)]
                                    );


  // Sort out drive icon...
  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, FALSE, DLL_SHELL32_HDD, tmpIcon) then
      begin
      imgFileType.Picture.Assign(tmpIcon)
      end;
  finally
    tmpIcon.Free();
  end;

  lblFileType.Caption := _('Type:');
  edFileType.Caption := '';
  if (Filesystem <> nil) then
    begin
    edFileType.Caption := RS_ENCRYPTED_VOLUME;
    if (Filesystem.PartitionImage is TSDPartitionImage_File) then
      begin
      edFileType.Caption := RS_VOLUME_PLAINTEXT;
      end;
    end;

  edFileSystem.Caption := Filesystem.FilesystemTitle();

  // Get rid of unused controls
  lblLocation.Visible := FALSE;
  edLocation.Visible := FALSE;
  lblSize.Visible := FALSE;
  edSize.Visible := FALSE;
  lblSizeOnDisk.Visible := FALSE;
  edSizeOnDisk.Visible := FALSE;
  lblAttributes.Visible := FALSE;
  ckReadOnly.Visible := FALSE;
  ckHidden.Visible := FALSE;
  ckArchive.Visible := FALSE;


  spaceTotal:= Filesystem.Size;
  spaceFree:= Filesystem.FreeSpace;
  spaceUsed:= (spaceTotal - spaceFree);

  edUsedSpace_Bytes.caption := SDUFormatWithThousandsSeparator(spaceUsed) + ' ' + UNITS_STORAGE_BYTES;
  edFreeSpace_Bytes.caption := SDUFormatWithThousandsSeparator(spaceFree) + ' ' + UNITS_STORAGE_BYTES;
  edCapacity_Bytes.caption  := SDUFormatWithThousandsSeparator(spaceTotal) + ' ' + UNITS_STORAGE_BYTES;
  edUsedSpace_Bytes.left := (chtSpaceChart.left + chtSpaceChart.width) - edUsedSpace_Bytes.width;
  edFreeSpace_Bytes.left := (chtSpaceChart.left + chtSpaceChart.width) - edFreeSpace_Bytes.width;
  edCapacity_Bytes.left  := (chtSpaceChart.left + chtSpaceChart.width) - edCapacity_Bytes.width;

  edUsedSpace_UsefulUnits.caption := SDUFormatAsBytesUnits(spaceUsed, 2);
  edFreeSpace_UsefulUnits.caption := SDUFormatAsBytesUnits(spaceFree, 2);
  edCapacity_UsefulUnits.caption  := SDUFormatAsBytesUnits(spaceTotal, 2);
  edUsedSpace_UsefulUnits.left := (Panel1.left + Panel1.width) - edUsedSpace_UsefulUnits.width;
  edFreeSpace_UsefulUnits.left := (Panel1.left + Panel1.width) - edFreeSpace_UsefulUnits.width;
  edCapacity_UsefulUnits.left  := (Panel1.left + Panel1.width) - edCapacity_UsefulUnits.width;

  pnlColorUsedSpace.Caption := '';
  pnlColorUsedSpace.BevelInner := bvLowered;
  pnlColorUsedSpace.BevelOuter := bvNone;
  pnlColorUsedSpace.Color := COLOR_USED_SPACE;

  pnlColorFreeSpace.Caption := '';
  pnlColorFreeSpace.BevelInner := bvLowered;
  pnlColorFreeSpace.BevelOuter := bvNone;
  pnlColorFreeSpace.Color := COLOR_FREE_SPACE;

  chtSpaceChart.Title.Visible := FALSE;
  chtSpaceChart.BevelInner := bvNone;
  chtSpaceChart.BevelOuter := bvNone;
  chtSpaceChart.Series[0].Add(spaceFree, _('Free space'), COLOR_FREE_SPACE);
  chtSpaceChart.Series[0].Add(spaceUsed, _('Used space'), COLOR_USED_SPACE);


  tmpIcon := TIcon.Create();
  try
    if SDULoadDLLIcon(
                      DLL_SHELL32,
                      FALSE,
                      DLL_SHELL32_SCANDISK,
                      tmpIcon
                     ) then
      begin
      imgScandisk.Picture.Assign(tmpIcon)
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

END.

