unit CommonfmeOptions_Autorun;

interface

uses
  Classes, ComCtrls,
  CommonfmeOptions_Base, CommonSettings, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, SDUDialogs, SDUStdCtrls,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeOptions_Autorun = class (TfmeOptions_Base)
    gbAutorun:            TGroupBox;
    Label33:              TLabel;
    Label34:              TLabel;
    Label35:              TLabel;
    edPostMountExe:       TEdit;
    pbPostMountBrowse:    TButton;
    pbPreDismountBrowse:  TButton;
    edPreDismountExe:     TEdit;
    pbPostDismountBrowse: TButton;
    edPostDismountExe:    TEdit;
    ckPrePostExeWarn:     TSDUCheckBox;
    OpenDialog:           TSDUOpenDialog;
    reInstructions:       TOTFEFreeOTFE_InstructionRichEdit;
    procedure pbPostMountBrowseClick(Sender: TObject);
    procedure pbPreDismountBrowseClick(Sender: TObject);
    procedure pbPostDismountBrowseClick(Sender: TObject);
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure ReadSettings(config: TSettings); OVERRIDE;
    procedure WriteSettings(config: TSettings); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  SDUGeneral, SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  FILTER_EXECUTABLE_FILES =
    'Program files (*.exe, *.com, *.bat)|*.exe; *.bat; *.com|All files|*.*';

procedure TfmeOptions_Autorun.pbPostMountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate post mount executable');
  OpenDialog.Filter  := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostMountExe.Text);
  if (OpenDialog.Execute) then begin
    // Strip drive letter
    edPostMountExe.Text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename) - 2);
  end;

end;

procedure TfmeOptions_Autorun.pbPreDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate pre dismount executable');
  OpenDialog.Filter  := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPreDismountExe.Text);
  if (OpenDialog.Execute) then begin
    // Strip drive letter
    edPreDismountExe.Text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename) - 2);
  end;

end;

procedure TfmeOptions_Autorun.pbPostDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate post dismount executable');
  OpenDialog.Filter  := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostDismountExe.Text);
  if (OpenDialog.Execute) then begin
    edPostDismountExe.Text := OpenDialog.Filename;
  end;

end;

procedure TfmeOptions_Autorun.ReadSettings(config: TSettings);
begin
  inherited;

  edPostMountExe.Text    := config.OptPostMountExe;
  edPreDismountExe.Text  := config.OptPreDismountExe;
  edPostDismountExe.Text := config.OptPostDismountExe;

  ckPrePostExeWarn.Checked := config.OptPrePostExeWarn;

end;

procedure TfmeOptions_Autorun.WriteSettings(config: TSettings);
begin
  inherited;

  config.OptPostMountExe    := edPostMountExe.Text;
  config.OptPreDismountExe  := edPreDismountExe.Text;
  config.OptPostDismountExe := edPostDismountExe.Text;

  config.OptPrePostExeWarn := ckPrePostExeWarn.Checked;

end;

procedure TfmeOptions_Autorun.Initialize();
begin
  inherited;

  SDUCenterControl(gbAutorun, ccHorizontal);
  SDUCenterControl(gbAutorun, ccVertical, 25);

  // Setup instructions text
  reInstructions.Text :=
    _('DoxBox can be configured here to automatically run programs after mounting, before dismounting, and after dismounting.'
    + SDUCRLF + SDUCRLF +
    'Any post-mount and pre-dismount executables specified must use a relative path to the executable within the mounted volume (i.e. absolute and UNC paths are not allowed for security reasons)' + SDUCRLF + SDUCRLF + 'Executables with spaces in their paths/names must be surrounded with double quotes.' + SDUCRLF + SDUCRLF + '"%DRIVE" will be substituted with the drive letter of the mounted drive letter');

end;

end.
