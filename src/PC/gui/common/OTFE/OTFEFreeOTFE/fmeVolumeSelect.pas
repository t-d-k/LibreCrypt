unit fmeVolumeSelect;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFEBase_U, SDUDialogs, SDUFrames, StdCtrls,
  SysUtils, Variants, Vcl.Buttons,
  Windows;

type
  TOpenSave = (fndOpen, fndSave);

  TfmeVolumeSelect = class (TSDUFrame)
    bbBrowsePartition: TBitBtn;
    bbBrowseFile:      TBitBtn;
    edFilename:        TEdit;
    OpenDialog:        TSDUOpenDialog;
    SaveDialog:        TSDUSaveDialog;

    procedure bbBrowseFileClick(Sender: TObject);
    procedure bbBrowsePartitionClick(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
  PRIVATE
    FFileButtonAdjust:     Integer;
    FSelectFor:            TOpenSave;
    FFileSelectFilter:     String;
    FFileSelectDefaultExt: String;
    FOnChange:             TNotifyEvent;
    FFileGlyph:            Vcl.Graphics.TBitmap;

    function GetFilename(): String;
    procedure SetFilename(filename: String);
    function GetAllowPartitionSelect(): Boolean;
    procedure SetAllowPartitionSelect(allow: Boolean);

  PROTECTED
    function GetEnabled(): Boolean; OVERRIDE;
    procedure SetEnabled(setValue: Boolean); OVERRIDE;

  PUBLIC

    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;

  PUBLISHED
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;

    property Filename: String Read GetFilename Write SetFilename;
    property SelectFor: TOpenSave Read FSelectFor Write FSelectFor;
    property AllowPartitionSelect: Boolean Read GetAllowPartitionSelect
      Write SetAllowPartitionSelect;

    property FileSelectFilter: String Read FFileSelectFilter Write FFileSelectFilter;
    property FileSelectDefaultExt: String Read FFileSelectDefaultExt Write FFileSelectDefaultExt;

  end;

// procedure Register;

implementation

   {$R *.dfm}

uses
  SDUGeneral;

// procedure Register;
// begin
//   RegisterComponents('FreeOTFE', [TfmeVolumeSelect]);
// end;

procedure TfmeVolumeSelect.bbBrowseFileClick(Sender: TObject);
var
  dlg: TOpenDialog; // Note: Save dialog inherits from this. Don't use TSDUOpenDialog here
begin
  dlg := SaveDialog;
  if (SelectFor = fndOpen) then begin
    dlg := OpenDialog;
  end;

  dlg.Filter     := FileSelectFilter;
  dlg.DefaultExt := FileSelectDefaultExt;
  dlg.Options    := dlg.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(dlg, edFilename.Text);

  if dlg.Execute() then begin
    edFilename.Text := dlg.Filename;
  end;

end;

procedure TfmeVolumeSelect.bbBrowsePartitionClick(Sender: TObject);
var
  selectedPartition: String;
begin
  if (GetFreeOTFEBase() <> nil) then begin
    selectedPartition := GetFreeOTFEBase().SelectPartition();
    if (selectedPartition <> '') then begin
      edFilename.Text := selectedPartition;
    end;
  end;
  //
end;
//
constructor TfmeVolumeSelect.Create(AOwner: TComponent);
begin
  inherited;

  SetFilename('');

  self.Height := edFilename.Height;

  FFileGlyph := Vcl.Graphics.TBitmap.Create();
  FFileGlyph.Assign(bbBrowseFile.Glyph);

  // Pre-calculate difference in file browse button position/TEdit width when
  // switching enabling/disabling partition select
  FFileButtonAdjust := ((bbBrowseFile.Left + bbBrowseFile.Width) -
    // Position of righthand edge of browse file button
    (edFilename.Left +
    edFilename.Width) // Position of righthand edge of filename TEdit
    );

end;

destructor TfmeVolumeSelect.Destroy();
begin
  FFileGlyph.Free();

  inherited;
end;

function TfmeVolumeSelect.GetFilename(): String;
begin
  Result := Trim(edFilename.Text);
end;

procedure TfmeVolumeSelect.SetFilename(filename: String);
begin
  edFilename.Text := filename;
end;

function TfmeVolumeSelect.GetAllowPartitionSelect(): Boolean;
begin
  Result := bbBrowsePartition.Enabled;
end;

procedure TfmeVolumeSelect.SetAllowPartitionSelect(allow: Boolean);
begin
  if (bbBrowsePartition.Visible <> allow) then begin
    // Eliminate/allow partition button, remove/restore image from remaining
    // button; replace with "..." text if needed
    bbBrowsePartition.Visible := allow;

    if allow then begin
      bbBrowseFile.Caption := '';
      bbBrowseFile.Glyph   := FFileGlyph;
      bbBrowseFile.Left    := bbBrowseFile.Left - FFileButtonAdjust;
      edFilename.Width     := edFilename.Width - FFileButtonAdjust;
    end else begin
      bbBrowseFile.Caption := '...';
      bbBrowseFile.Glyph   := nil;
      bbBrowseFile.Left    := bbBrowseFile.Left + FFileButtonAdjust;
      edFilename.Width     := edFilename.Width + FFileButtonAdjust;
    end;
  end;

end;
//
procedure TfmeVolumeSelect.edFilenameChange(Sender: TObject);
begin
  if Assigned(fOnChange) then begin
    fOnChange(self);
  end;
  //
end;
//
function TfmeVolumeSelect.GetEnabled(): Boolean;
begin
  Result := inherited GetEnabled();
end;

procedure TfmeVolumeSelect.SetEnabled(setValue: Boolean);
begin
  SDUEnableControl(edFilename, setValue);
  inherited;

  SDUEnableControl(bbBrowseFile, setValue);
  SDUEnableControl(bbBrowsePartition, setValue);
end;

end.
