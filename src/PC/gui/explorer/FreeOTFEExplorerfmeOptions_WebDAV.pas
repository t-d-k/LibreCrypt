unit FreeOTFEExplorerfmeOptions_WebDAV;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin64,
  FreeOTFEExplorerSettings, SDUStdCtrls, CommonfmeOptions_Base,
  FreeOTFEExplorerfmeOptions_Base,
  Shredder, SDUFrames, SDUFilenameEdit_U;

type
  TfmeOptions_FreeOTFEExplorerWebDAV = class(TfmeFreeOTFEExplorerOptions_Base)
    gbWebDAV: TGroupBox;
    ckWebDAV: TSDUCheckBox;
    cbDrive: TComboBox;
    lblDefaultDriveLetter: TLabel;
    gbWebDAVAdvanced: TGroupBox;
    fedWebDAVLogDebug: TSDUFilenameEdit;
    fedWebDAVLogAccess: TSDUFilenameEdit;
    edWebDAVShareName: TEdit;
    Label6: TLabel;
    ckExploreAfterMount: TSDUCheckBox;
    ckPromptMountSuccessful: TSDUCheckBox;
    ckOverwriteCacheOnDismount: TSDUCheckBox;
    ckWebDAVLogAccess: TSDUCheckBox;
    ckWebDAVLogDebug: TSDUCheckBox;
    procedure ControlChanged(Sender: TObject);
    procedure ckWebDAVClick(Sender: TObject);
    procedure ckWebDAVLogAccessClick(Sender: TObject);
    procedure ckWebDAVLogDebugClick(Sender: TObject);
  private
    FWarnUserChangesRequireRemount: boolean;

  protected
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); override;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); override;

  public
    procedure Initialize(); override;
    procedure EnableDisableControls(); override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  CommonSettings,
  CommonfrmOptions,
  OTFE_U,
  OTFEFreeOTFEBase_U,
  FreeOTFEExplorerConsts;

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

procedure TfmeOptions_FreeOTFEExplorerWebDAV.ckWebDAVLogAccessClick(
  Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV.ckWebDAVClick(Sender: TObject);
begin
  inherited;

  if SDUOSVistaOrLater() then
    begin
    SDUMessageDlg(
                  RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7+
                  SDUCRLF+
                  SDUParamSubstitute(
                                     _('Mounted volumes will only be mapped to drive letters when %1 is run under Windows 2000/Windows XP'),
                                     [Application.Title]
                                    ),
                  mtInformation
                 );
    end
  else if FWarnUserChangesRequireRemount then
    begin
    SDUMessageDlg(
                  _('The changes you make here will only take effect when you next mount a volume'),
                  mtInformation
                 );

    FWarnUserChangesRequireRemount := FALSE;
    end;

  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV.ckWebDAVLogDebugClick(
  Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckExploreAfterMount, ckWebDAV.checked);
  SDUEnableControl(ckPromptMountSuccessful, ckWebDAV.checked);
  SDUEnableControl(cbDrive, ckWebDAV.checked);

  SDUEnableControl(gbWebDAVAdvanced, ckWebDAV.checked);

  SDUEnableControl(fedWebDAVLogAccess, (
                                        ckWebDAV.checked and
                                        ckWebDAVLogAccess.checked
                                       ));
  SDUEnableControl(fedWebDAVLogDebug, (
                                        ckWebDAV.checked and
                                        ckWebDAVLogDebug.checked
                                       ));

  if not(ckWebDAVLogAccess.checked) then
    begin
    fedWebDAVLogAccess.Filename := '';
    end;
  if not(ckWebDAVLogDebug.checked) then
    begin
    fedWebDAVLogDebug.Filename := '';
    end;

end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV.Initialize();
var
  driveLetter: char;
begin
  inherited;

  FWarnUserChangesRequireRemount := TRUE;

  SDUCenterControl(gbWebDAV, ccHorizontal);
  SDUCenterControl(gbWebDAV, ccVertical, 25);

  cbDrive.Items.clear();
  cbDrive.Items.Add(USE_DEFAULT);
//  for driveLetter:='C' to 'Z' do
  for driveLetter:='A' to 'Z' do
    begin
    cbDrive.Items.Add(driveLetter+':');
    end;

end;

procedure TfmeOptions_FreeOTFEExplorerWebDAV._ReadSettings(config: TFreeOTFEExplorerSettings);
var
  prevWarnUserChangesRequireRemount: boolean;
begin
  // Temporarily set FWarnUserChangesRequireRemount to FALSE - otherwise we'll
  // get the warning message as the frame is loaded!
  prevWarnUserChangesRequireRemount := FWarnUserChangesRequireRemount;
  FWarnUserChangesRequireRemount := FALSE;
  ckWebDAV.checked := config.OptWebDAVEnableServer;
  FWarnUserChangesRequireRemount := prevWarnUserChangesRequireRemount;

  ckExploreAfterMount.checked := config.OptExploreAfterMount;
  ckPromptMountSuccessful.checked := config.OptPromptMountSuccessful;

  // Default drive letter
  if (config.OptDefaultDriveLetter = #0) then
    begin
    cbDrive.ItemIndex := 0;
    end
  else
    begin
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.OptDefaultDriveLetter+':');
    end;

  ckOverwriteCacheOnDismount.checked := config.OptOverwriteWebDAVCacheOnDismount;
  edWebDAVShareName.text := config.OptWebDavShareName;
  fedWebDAVLogAccess.Filename := config.OptWebDavLogAccess;
  fedWebDAVLogDebug.Filename := config.OptWebDavLogDebug;

  ckWebDAVLogAccess.checked := (trim(fedWebDAVLogAccess.Filename) <> '');
  ckWebDAVLogDebug.checked := (trim(fedWebDAVLogDebug.Filename) <> '');

end;


procedure TfmeOptions_FreeOTFEExplorerWebDAV._WriteSettings(config: TFreeOTFEExplorerSettings);
begin
  config.OptWebDAVEnableServer := ckWebDAV.checked;

  config.OptExploreAfterMount := ckExploreAfterMount.checked;
  config.OptPromptMountSuccessful := ckPromptMountSuccessful.checked;

  // Default drive letter
  if (cbDrive.ItemIndex = 0) then
    begin
    config.OptDefaultDriveLetter := #0;
    end
  else
    begin
    config.OptDefaultDriveLetter := DriveLetterChar(cbDrive.Items[cbDrive.ItemIndex][1]);
    end;

  config.OptOverwriteWebDAVCacheOnDismount := ckOverwriteCacheOnDismount.checked;
  config.OptWebDavShareName := edWebDAVShareName.text;
  config.OptWebDavLogDebug := fedWebDAVLogDebug.Filename;
  config.OptWebDavLogAccess := fedWebDAVLogAccess.Filename;
  
end;

END.

