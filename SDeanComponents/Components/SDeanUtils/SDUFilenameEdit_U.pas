unit SDUFilenameEdit_U;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SDUFrames, StdCtrls, SDUDialogs;

type
  TSDUFilenamEditType = (fetOpen, fetSave);

  TSDUFilenameEdit = class(TSDUFrame)
    edFilename: TEdit;
    pbBrowse: TButton;
    OpenDialog1: TSDUOpenDialog;
    SaveDialog1: TSDUSaveDialog;
    procedure pbBrowseClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FFilenameEditType: TSDUFilenamEditType;

    FInitialDir: string;
    FFilter: string;
    FFilterIndex: integer;
    FDefaultExt: string;
    FOnChange: TNotifyEvent;

    procedure TweakControlsLayout();

  protected
{
    function  GetInitialDir(): string;
    procedure SetInitialDir(value: string);
    function  GetFilter(): string;
    procedure SetFilter(value: string);
    function  GetFilterIndex(): integer;
    procedure SetFilterIndex(value: integer);
    function  GetDefaultExt(): string;
    procedure SetDefaultExt(value: string);
}

    function  GetFilename(): string;
    procedure SetFilename(value: string);

    procedure SetFilenameEditType(value: TSDUFilenamEditType);

    procedure SetEnabled(value: boolean); override;

    procedure Resizing(State: TWindowState); override;

    procedure DesigningSummary();

    procedure BrowseDialogOpen();
    procedure BrowseDialogSave();

    procedure DoOnChange();
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;
  published
    property TabStop default TRUE;  // Change default to TRUE
    
    property FilenameEditType: TSDUFilenamEditType read FFilenameEditType write SetFilenameEditType default fetOpen;

    property InitialDir: string read FInitialDir write FInitialDir;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: integer read FFilterIndex write FFilterIndex default 1;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
{
    property InitialDir: string read GetInitialDir write SetInitialDir;
    property Filter: string read GetFilter write SetFilter;
    property FilterIndex: integer read GetFilterIndex write SetFilterIndex;
    property DefaultExt: string read GetDefaultExt write SetDefaultExt;
}

    property Filename: string read GetFilename write SetFilename;

    property OpenDialog: TSDUOpenDialog read OpenDialog1;
    property SaveDialog: TSDUSaveDialog read SaveDialog1;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  SDUGeneral;

const
  // The space between the two controls
  CONTROL_MARGIN = 5;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUFilenameEdit]);
end;

constructor TSDUFilenameEdit.Create(AOwner: TComponent);
begin
  inherited;

  DesigningSummary();

  edFilename.Text := '';

  self.height := edFilename.height;

  TweakControlsLayout();

  edFilename.Anchors := [akLeft, akRight, akTop];
  pbBrowse.Anchors := [akRight, akTop];

  self.Constraints.MinHeight := edFilename.height;
  self.Constraints.MaxHeight := edFilename.height;
end;

destructor TSDUFilenameEdit.Destroy();
begin
  inherited;
end;

procedure TSDUFilenameEdit.TweakControlsLayout();
begin
  pbBrowse.height := edFilename.height;
  pbBrowse.width := pbBrowse.height;

  edFilename.Top := 0;
  edFilename.left := 0;
  edFilename.width := self.width - (pbBrowse.width + CONTROL_MARGIN);

  pbBrowse.Top := 0;
  pbBrowse.left := edFilename.width + CONTROL_MARGIN;
end;

{
function TSDUFilenameEdit.GetInitialDir(): string;
begin
  Result := OpenDialog1.InitialDir;
end;

procedure TSDUFilenameEdit.SetInitialDir(value: string);
begin
  OpenDialog1.InitialDir := value;
  SaveDialog1.InitialDir := value;
end;

function TSDUFilenameEdit.GetFilter(): string;
begin
  Result := OpenDialog1.Filter;
end;

procedure TSDUFilenameEdit.SetFilter(value: string);
begin
  OpenDialog1.Filter := value;
  SaveDialog1.Filter := value;
end;

function TSDUFilenameEdit.GetFilterIndex(): integer;
begin
  Result := OpenDialog1.FilterIndex;
end;

procedure TSDUFilenameEdit.SetFilterIndex(value: integer);
begin
  OpenDialog1.FilterIndex := value;
  SaveDialog1.FilterIndex := value;
end;

function TSDUFilenameEdit.GetDefaultExt(): string;
begin
  Result := OpenDialog1.DefaultExt;
end;

procedure TSDUFilenameEdit.SetDefaultExt(value: string);
begin
  OpenDialog1.DefaultExt := value;
  SaveDialog1.DefaultExt := value;
end;
}

procedure TSDUFilenameEdit.edFilenameChange(Sender: TObject);
begin
  inherited;
  DoOnChange();
end;

procedure TSDUFilenameEdit.DoOnChange();
begin
  if Assigned(FOnChange) then
    begin
    FOnChange(self);
    end;
end;

procedure TSDUFilenameEdit.FrameEnter(Sender: TObject);
begin
  inherited;
  // If the frame gets the focus, set it to the filename TEdit
  // This allows other controls (e.g. TLabel) to have their "FocusControl"
  // property to this component, and the right control will get the focus when
  // the user presses that control's accelerator key/gives it the focus

  // Need to check if the browse button has the focus first - otherwise, if the
  // user clicks on the button, the TEdit will get the focus!
  if not(pbBrowse.Focused) then
    begin
    edFilename.SetFocus();
    end;
end;

procedure TSDUFilenameEdit.pbBrowseClick(Sender: TObject);
begin
  if (FilenameEditType = fetSave) then
    begin
    BrowseDialogSave();
    end
  else
    begin
    BrowseDialogOpen();
    end;

end;

procedure TSDUFilenameEdit.BrowseDialogOpen();
var
  dlg: TSDUOpenDialog;
  cwd: string;
begin
  inherited;

  // Store and restore the CWD; the dialog changes it
  cwd := SDUGetCWD();
  try
    dlg:= OpenDialog;

    dlg.InitialDir  := InitialDir;
    dlg.Filter      := Filter;
    dlg.FilterIndex := FilterIndex;
    dlg.DefaultExt  := DefaultExt;

    SDUOpenSaveDialogSetup(dlg, Filename);
    if dlg.Execute() then
      begin
      Filename := dlg.Filename;
      end;

  finally
    SDUSetCWD(cwd);
  end;

end;

procedure TSDUFilenameEdit.BrowseDialogSave();
var
  dlg: TSDUSaveDialog;
  cwd: string;
begin
  inherited;

  // Store and restore the CWD; the dialog changes it
  cwd := SDUGetCWD();
  try
    dlg:= SaveDialog;

    dlg.InitialDir  := InitialDir;
    dlg.Filter      := Filter;
    dlg.FilterIndex := FilterIndex;
    dlg.DefaultExt  := DefaultExt;

    SDUOpenSaveDialogSetup(dlg, Filename);
    if dlg.Execute() then
      begin
      Filename := dlg.Filename;
      end;

  finally
    SDUSetCWD(cwd);
  end;

end;

function TSDUFilenameEdit.GetFilename(): string;
begin
  Result := edFilename.Text;
end;

procedure TSDUFilenameEdit.SetFilename(value: string);
begin
  edFilename.Text := value;
end;

procedure TSDUFilenameEdit.SetEnabled(value: boolean);
begin
  inherited;
  SDUEnableControl(edFilename, value);
  SDUEnableControl(pbBrowse, value);
end;

// Change the button's caption to reflect the open/save type - but only at
// design time.
// This makes it easier to see what type of filename edit it is. At runtime,
// the button's caption will revert to "..."
procedure TSDUFilenameEdit.DesigningSummary();
begin
  if (csDesigning in ComponentState) then
    begin
    pbBrowse.Font.Style := [fsBold];
    pbBrowse.Caption := 'O';
    if (FilenameEditType = fetSave) then
      begin
      pbBrowse.Caption := 'S';
      end;

    end;

end;

procedure TSDUFilenameEdit.Resizing(State: TWindowState);
begin
  inherited;
  DesigningSummary();
end;

procedure TSDUFilenameEdit.SetFilenameEditType(value: TSDUFilenamEditType);
begin
  FFilenameEditType := value;
  DesigningSummary();
end;

procedure TSDUFilenameEdit.FrameResize(Sender: TObject);
begin
  inherited;
  TweakControlsLayout();
end;

END.

