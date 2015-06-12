unit fmeExplorerWebDAVOptions;
{todo:this frame is only used in one place so should be inserted there instead}
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeCommonExplorerOptions,
  ExplorerSettings, Graphics, Messages, SDUFilenameEdit_U, SDUFrames,
  SDUStdCtrls, Shredder, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeExplorerWebDAVOptions = class (TfmeCommonExplorerOptions)
    gbWebDAV:                   TGroupBox;
    ckWebDAV:                   TSDUCheckBox;
    cbDrive:                    TComboBox;
    lblDefaultDriveLetter:      TLabel;
    gbWebDAVAdvanced:           TGroupBox;
    fedWebDAVLogDebug:          TSDUFilenameEdit;
    fedWebDAVLogAccess:         TSDUFilenameEdit;
    edWebDAVShareName:          TEdit;
    Label6:                     TLabel;
    ckExploreAfterMount:        TSDUCheckBox;
    ckPromptMountSuccessful:    TSDUCheckBox;
    ckOverwriteCacheOnDismount: TSDUCheckBox;
    ckWebDAVLogAccess:          TSDUCheckBox;
    ckWebDAVLogDebug:           TSDUCheckBox;
    procedure ControlChanged(Sender: TObject);
    procedure ckWebDAVClick(Sender: TObject);
    procedure ckWebDAVLogAccessClick(Sender: TObject);
    procedure ckWebDAVLogDebugClick(Sender: TObject);
  PRIVATE
    FWarnUserChangesRequireRemount: Boolean;

  PROTECTED
    procedure _ReadSettings(config: TExplorerSettings); OVERRIDE;
    procedure _WriteSettings(config: TExplorerSettings); OVERRIDE;

  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  frmCommonOptions,
  CommonSettings,
  lcConsts, OTFE_U,
  OTFEFreeOTFEBase_U,
  lcDialogs,
  SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

resourcestring
  USE_DEFAULT = 'Use default';

procedure TfmeExplorerWebDAVOptions.ckWebDAVLogAccessClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeExplorerWebDAVOptions.ckWebDAVClick(Sender: TObject);
begin
  inherited;

  if SDUOSVistaOrLater() then begin
    SDUMessageDlg(
      RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 +
      SDUCRLF + SDUParamSubstitute(
      _(
      'Mounted volumes will only be mapped to drive letters when %1 is run under Windows 2000/Windows XP'),
      [Application.Title]),
      mtInformation
      );
  end else
  if FWarnUserChangesRequireRemount then begin
    SDUMessageDlg(
      _('The changes you make here will only take effect when you next mount a container'),
      mtInformation
      );

    FWarnUserChangesRequireRemount := False;
  end;

  EnableDisableControls();
end;

procedure TfmeExplorerWebDAVOptions.ckWebDAVLogDebugClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeExplorerWebDAVOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeExplorerWebDAVOptions.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckExploreAfterMount, ckWebDAV.Checked);
  SDUEnableControl(ckPromptMountSuccessful, ckWebDAV.Checked);
  SDUEnableControl(cbDrive, ckWebDAV.Checked);

  SDUEnableControl(gbWebDAVAdvanced, ckWebDAV.Checked);

  SDUEnableControl(fedWebDAVLogAccess,
    (ckWebDAV.Checked and
    ckWebDAVLogAccess.Checked
    ));
  SDUEnableControl(fedWebDAVLogDebug, (ckWebDAV.Checked and
    ckWebDAVLogDebug.Checked
    ));

  if not (ckWebDAVLogAccess.Checked) then begin
    fedWebDAVLogAccess.Filename := '';
  end;
  if not (ckWebDAVLogDebug.Checked) then begin
    fedWebDAVLogDebug.Filename := '';
  end;

end;

procedure TfmeExplorerWebDAVOptions.Initialize();
var
  driveLetter: Char;
begin
  inherited;

  FWarnUserChangesRequireRemount := True;

  SDUCenterControl(gbWebDAV, ccHorizontal);
  SDUCenterControl(gbWebDAV, ccVertical, 25);

  cbDrive.Items.Clear();
  cbDrive.Items.Add(USE_DEFAULT);
  //  for driveLetter:='C' to 'Z' do
  for driveLetter := 'A' to 'Z' do begin
    cbDrive.Items.Add(driveLetter + ':');
  end;

end;

procedure TfmeExplorerWebDAVOptions._ReadSettings(config: TExplorerSettings);
var
  prevWarnUserChangesRequireRemount: Boolean;
begin
  // Temporarily set FWarnUserChangesRequireRemount to FALSE - otherwise we'll
  // get the warning message as the frame is loaded!
  prevWarnUserChangesRequireRemount := FWarnUserChangesRequireRemount;
  FWarnUserChangesRequireRemount    := False;
  ckWebDAV.Checked                  := config.OptWebDAVEnableServer;
  FWarnUserChangesRequireRemount    := prevWarnUserChangesRequireRemount;

  ckExploreAfterMount.Checked     := config.OptExploreAfterMount;
  ckPromptMountSuccessful.Checked := config.OptPromptMountSuccessful;

  // Default drive letter
  if (config.OptDefaultDriveLetter = #0) then begin
    cbDrive.ItemIndex := 0;
  end else begin
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.OptDefaultDriveLetter + ':');
  end;

  ckOverwriteCacheOnDismount.Checked := config.OptOverwriteWebDAVCacheOnDismount;
  edWebDAVShareName.Text             := config.OptWebDavShareName;
  fedWebDAVLogAccess.Filename        := config.OptWebDavLogAccess;
  fedWebDAVLogDebug.Filename         := config.OptWebDavLogDebug;

  ckWebDAVLogAccess.Checked := (trim(fedWebDAVLogAccess.Filename) <> '');
  ckWebDAVLogDebug.Checked  := (trim(fedWebDAVLogDebug.Filename) <> '');

end;


procedure TfmeExplorerWebDAVOptions._WriteSettings(config: TExplorerSettings);
begin
  config.OptWebDAVEnableServer := ckWebDAV.Checked;

  config.OptExploreAfterMount     := ckExploreAfterMount.Checked;
  config.OptPromptMountSuccessful := ckPromptMountSuccessful.Checked;

  // Default drive letter
  if (cbDrive.ItemIndex = 0) then begin
    config.OptDefaultDriveLetter := #0;
  end else begin
    config.OptDefaultDriveLetter := DriveLetterChar(cbDrive.Items[cbDrive.ItemIndex][1]);
  end;

  config.OptOverwriteWebDAVCacheOnDismount := ckOverwriteCacheOnDismount.Checked;
  config.OptWebDavShareName                := edWebDAVShareName.Text;
  config.OptWebDavLogDebug                 := fedWebDAVLogDebug.Filename;
  config.OptWebDavLogAccess                := fedWebDAVLogAccess.Filename;

end;

end.
