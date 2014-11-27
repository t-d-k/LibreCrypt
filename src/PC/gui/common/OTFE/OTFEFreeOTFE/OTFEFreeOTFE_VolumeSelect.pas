unit OTFEFreeOTFE_VolumeSelect;

interface

uses   Classes, Controls, Dialogs, Forms,
  Graphics, Messages, SDUDialogs, SDUFrames, StdCtrls, SysUtils, Variants, Windows,
  Vcl.Buttons,

  OTFEFreeOTFEBase_U;



type
    TOpenSave = (fndOpen, fndSave);

  TOTFEFreeOTFEVolumeSelect = class(TSDUFrame)
    bbBrowsePartition: TBitBtn;
    bbBrowseFile: TBitBtn;
    edFilename: TEdit;
    OpenDialog: TSDUOpenDialog;
    SaveDialog: TSDUSaveDialog;

    procedure bbBrowseFileClick(Sender: TObject);
    procedure bbBrowsePartitionClick(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
  private
    FFileButtonAdjust: integer;
    FSelectFor: TOpenSave;
    FFileSelectFilter: string;
    FFileSelectDefaultExt: string;
    FOnChange: TNotifyEvent;
    FFileGlyph: Vcl.Graphics.TBitmap;

    function  GetFilename(): string;
    procedure SetFilename(filename: string);
    function  GetAllowPartitionSelect(): boolean;
    procedure SetAllowPartitionSelect(allow: boolean);

  protected
    function  GetEnabled(): boolean; override;
    procedure SetEnabled(setValue: boolean); override;
  
  public
    OTFEFreeOTFE: TOTFEFreeOTFEBase;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Filename: string read GetFilename write SetFilename;
    property SelectFor: TOpenSave read FSelectFor write FSelectFor;
    property AllowPartitionSelect: boolean read GetAllowPartitionSelect write SetAllowPartitionSelect;

    property FileSelectFilter: string read FFileSelectFilter write FFileSelectFilter;
    property FileSelectDefaultExt: string read FFileSelectDefaultExt write FFileSelectDefaultExt;

  end;

procedure Register;

implementation

   {$R *.dfm}

uses
  SDUGeneral;

procedure Register;
begin
  RegisterComponents('FreeOTFE', [TOTFEFreeOTFEVolumeSelect]);
end;

procedure TOTFEFreeOTFEVolumeSelect.bbBrowseFileClick(Sender: TObject);
var
  dlg: TOpenDialog; // Note: Save dialog inherits from this. Don't use TSDUOpenDialog here
begin
  dlg := SaveDialog;
  if (SelectFor = fndOpen) then
    begin
    dlg := OpenDialog;
    end;

  dlg.Filter     := FileSelectFilter;
  dlg.DefaultExt := FileSelectDefaultExt;
  dlg.Options := dlg.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(dlg, edFilename.text);

  if dlg.Execute() then
    begin
    edFilename.text := dlg.Filename;
    end;

end;
procedure TOTFEFreeOTFEVolumeSelect.bbBrowsePartitionClick(Sender: TObject);
var
  selectedPartition: string;
begin
  if (OTFEFreeOTFE <> nil) then
    begin
    selectedPartition := OTFEFreeOTFE.SelectPartition();
    if (selectedPartition <> '') then
      begin
      edFilename.text := selectedPartition;
      end;
    end;
//
end;
//
constructor TOTFEFreeOTFEVolumeSelect.Create(AOwner: TComponent);
begin
  inherited;

  SetFilename('');

  self.height := edFilename.height;

  FFileGlyph := Vcl.Graphics.TBitmap.Create();
  FFileGlyph.Assign(bbBrowseFile.Glyph);

  // Pre-calculate difference in file browse button position/TEdit width when
  // switching enabling/disabling partition select
  FFileButtonAdjust := (
                        (bbBrowseFile.Left + bbBrowseFile.Width) - // Position of righthand edge of browse file button
                        (edFilename.Left + edFilename.Width) // Position of righthand edge of filename TEdit
                       );

end;

destructor  TOTFEFreeOTFEVolumeSelect.Destroy();
begin
  FFileGlyph.Free();

  inherited;
end;

function TOTFEFreeOTFEVolumeSelect.GetFilename(): string;
begin
  Result := Trim(edFilename.text);
end;

procedure TOTFEFreeOTFEVolumeSelect.SetFilename(filename: string);
begin
  edFilename.text := filename;
end;

function TOTFEFreeOTFEVolumeSelect.GetAllowPartitionSelect(): boolean;
begin
  Result := bbBrowsePartition.Enabled;
end;

procedure TOTFEFreeOTFEVolumeSelect.SetAllowPartitionSelect(allow: boolean);
begin
  if (bbBrowsePartition.Visible <> allow) then
    begin
    // Eliminate/allow partition button, remove/restore image from remaining
    // button; replace with "..." text if needed
    bbBrowsePartition.Visible := allow;

    if allow then
      begin
      bbBrowseFile.Caption := '';
      bbBrowseFile.Glyph := FFileGlyph;
      bbBrowseFile.Left := bbBrowseFile.Left - FFileButtonAdjust;
      edFilename.Width := edFilename.Width - FFileButtonAdjust;
      end
    else
      begin
      bbBrowseFile.Caption := '...';
      bbBrowseFile.Glyph := nil;
      bbBrowseFile.Left := bbBrowseFile.Left + FFileButtonAdjust;
      edFilename.Width := edFilename.Width + FFileButtonAdjust;
      end;
    end;

end;
//
procedure TOTFEFreeOTFEVolumeSelect.edFilenameChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    begin
    OnChange(self);
    end;
//
end;
//
function TOTFEFreeOTFEVolumeSelect.GetEnabled(): boolean;
begin
  Result := inherited GetEnabled();
end;

procedure TOTFEFreeOTFEVolumeSelect.SetEnabled(setValue: boolean);
begin
  SDUEnableControl(edFilename, setValue);
  inherited;

  SDUEnableControl(bbBrowseFile, setValue);
  SDUEnableControl(bbBrowsePartition, setValue);
end;

END.

