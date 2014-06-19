unit FreeOTFEfmeOptions_AutoRun;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FreeOTFESettings, StdCtrls, ComCtrls,
  OTFEFreeOTFE_InstructionRichEdit, SDUDialogs, SDUStdCtrls,
  CommonfmeOptions_Base,
  FreeOTFEfmeOptions_Base;

type
  TfmeOptions_Autorun = class(TfmeFreeOTFEOptions_Base)
    gbAutorun: TGroupBox;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    edPostMountExe: TEdit;
    pbPostMountBrowse: TButton;
    pbPreDismountBrowse: TButton;
    edPreDismountExe: TEdit;
    pbPostDismountBrowse: TButton;
    edPostDismountExe: TEdit;
    ckPrePostExeWarn: TSDUCheckBox;
    OpenDialog: TSDUOpenDialog;
    reInstructions: TOTFEFreeOTFE_InstructionRichEdit;
    procedure pbPostMountBrowseClick(Sender: TObject);
    procedure pbPreDismountBrowseClick(Sender: TObject);
    procedure pbPostDismountBrowseClick(Sender: TObject);
  protected
    procedure _ReadSettings(config: TFreeOTFESettings); override;
    procedure _WriteSettings(config: TFreeOTFESettings); override;
  public
    procedure Initialize(); override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  FILTER_EXECUTABLE_FILES = 'Program files (*.exe, *.com, *.bat)|*.exe; *.bat; *.com|All files|*.*';

procedure TfmeOptions_Autorun.pbPostMountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title := _('Locate post mount executable');
  OpenDialog.Filter := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostMountExe.text);
  if (OpenDialog.execute) then
    begin
    // Strip drive letter
    edPostMountExe.text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename)-2);
    end;

end;

procedure TfmeOptions_Autorun.pbPreDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title := _('Locate pre dismount executable');
  OpenDialog.Filter := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPreDismountExe.text);
  if (OpenDialog.execute) then
    begin
    // Strip drive letter
    edPreDismountExe.text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename)-2);
    end;

end;

procedure TfmeOptions_Autorun.pbPostDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title := _('Locate post dismount executable');
  OpenDialog.Filter := FILTER_EXECUTABLE_FILES;
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostDismountExe.text);
  if (OpenDialog.execute) then
    begin
    edPostDismountExe.text := OpenDialog.Filename;
    end;

end;

procedure TfmeOptions_Autorun._ReadSettings(config: TFreeOTFESettings);
begin
  edPostMountExe.text    := config.OptPostMountExe;
  edPreDismountExe.text  := config.OptPreDismountExe;
  edPostDismountExe.text := config.OptPostDismountExe;

  ckPrePostExeWarn.checked := config.OptPrePostExeWarn;

end;

procedure TfmeOptions_Autorun._WriteSettings(config: TFreeOTFESettings);
begin
  config.OptPostMountExe := edPostMountExe.text;
  config.OptPreDismountExe := edPreDismountExe.text;
  config.OptPostDismountExe := edPostDismountExe.text;

  config.OptPrePostExeWarn := ckPrePostExeWarn.checked;

end;

procedure TfmeOptions_Autorun.Initialize();
begin
  inherited;

  SDUCenterControl(gbAutorun, ccHorizontal);
  SDUCenterControl(gbAutorun, ccVertical, 25);

  // Setup instructions text
  reInstructions.Text :=
    _('FreeOTFE can be configured here to automatically run programs after mounting, before dismounting, and after dismounting.'+SDUCRLF+
      SDUCRLF+
      'Any post-mount and pre-dismount executables specified must use a relative path to the executable within the mounted volume (i.e. absolute and UNC paths are not allowed for security reasons)'+SDUCRLF+
      SDUCRLF+
      'Executables with spaces in their paths/names must be surrounded with double quotes.'+SDUCRLF+
      SDUCRLF+
      '"%DRIVE" will be substituted with the drive letter of the mounted drive letter');

end;

END.

