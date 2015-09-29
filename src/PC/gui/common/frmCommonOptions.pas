unit frmCommonOptions;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 { done 1 -otdk -crefactor : TfmePkcs11Options; frame is only used here - move into this dialog }

interface

uses
  Classes, ComCtrls,
  //  fmeBaseOptions,
  CommonSettings,
  Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Messages,
  StdCtrls, SysUtils, Windows,
  //sdu
  SDUFrames, SDUFilenameEdit_U, SDUForms, SDUStdCtrls,
  //librecrypt
  fmeVolumeSelect,
  OTFEFreeOTFEBase_U, Spin64, SDUDialogs;

type
  TLanguageTranslation = record
    Name:    String;
    Code:    String;
    Contact: String;
  end;
  PLanguageTranslation = ^TLanguageTranslation;

  TfrmCommonOptions = class (TSDUForm)
    pbOK:           TButton;
    pbCancel:       TButton;
    cbSettingsLocation: TComboBox;
    lblSettingsLocation: TLabel;
    ckAssociateFiles: TSDUCheckBox;
    pcOptions:      TPageControl;
    imgNoSaveWarning: TImage;
    tsPKCS11:       TTabSheet;
    gbPKCS11:       TGroupBox;
    lblLibrary:     TLabel;
    ckEnablePKCS11: TCheckBox;
    pbVerify:       TButton;
    gbPKCS11AutoActions: TGroupBox;
    lblAutoMountVolume: TLabel;
    ckPKCS11AutoDismount: TCheckBox;
    ckPKCS11AutoMount: TCheckBox;
    OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect;
    feLibFilename:  TSDUFilenameEdit;
    pbAutoDetect:   TButton;
    tsGeneral:      TTabSheet;
    gbGeneralMain:  TGroupBox;
    lblDefaultDriveLetter: TLabel;
    lblLanguage:    TLabel;
    lblChkUpdatesFreq: TLabel;
    ckDisplayToolbar: TSDUCheckBox;
    ckDisplayStatusbar: TSDUCheckBox;
    ckExploreAfterMount: TSDUCheckBox;
    cbDrive:        TComboBox;
    ckShowPasswords: TSDUCheckBox;
    cbLanguage:     TComboBox;
    pbLangDetails:  TButton;
    ckPromptMountSuccessful: TSDUCheckBox;
    ckDisplayToolbarLarge: TSDUCheckBox;
    ckDisplayToolbarCaptions: TSDUCheckBox;
    cbChkUpdatesFreq: TComboBox;
    ckStoreLayout:  TSDUCheckBox;
    tsAdvanced:     TTabSheet;
    ckRevertVolTimestamps: TSDUCheckBox;
    ckAllowNewlinesInPasswords: TSDUCheckBox;
    ckAllowTabsInPasswords: TSDUCheckBox;
    lblMRUMaxItemCount: TLabel;
    seMRUMaxItemCount: TSpinEdit64;
    lblMRUMaxItemCountInst: TLabel;
    ckAdvancedMountDlg: TSDUCheckBox;
    tsAutorun:      TTabSheet;
    gbAutorun:      TGroupBox;
    Label33:        TLabel;
    Label34:        TLabel;
    Label35:        TLabel;
    lblInstructions: TLabel;
    edPostMountExe: TEdit;
    pbPostMountBrowse: TButton;
    pbPreDismountBrowse: TButton;
    edPreDismountExe: TEdit;
    pbPostDismountBrowse: TButton;
    edPostDismountExe: TEdit;
    ckPrePostExeWarn: TSDUCheckBox;
    OpenDialog:     TSDUOpenDialog;

    procedure pbOKClick(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbAutoDetectClick(Sender: TObject);
    procedure pbVerifyClick(Sender: TObject);
    procedure pbLangDetailsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure ckStoreLayoutClick(Sender: TObject);
    procedure pbPostMountBrowseClick(Sender: TObject);
    procedure pbPreDismountBrowseClick(Sender: TObject);
    procedure pbPostDismountBrowseClick(Sender: TObject);
    procedure feLibFilenamepbBrowseClick(Sender: TObject);
  private
    FLanguages: array of TLanguageTranslation;
    FFlagClearLayoutOnSave: Boolean;

    procedure _InitializePkcs11Options;
    procedure _ReadPkcs11Settings(config: TCommonSettings);
    function GetLibDLLFilename: String;
    function _VerifyLibrary: Boolean;
    procedure _WritePkcs11Settings(config: TCommonSettings);
    function _CheckPkcs11Settings: Boolean;
    procedure _PopulateLanguages;
    function GetLanguage(idx: Integer): TLanguageTranslation;
    procedure SetLanguageSelection(langCode: String);
    function GetSelectedLanguage: TLanguageTranslation;
    procedure _ChangeLanguage(langCode: String);
    procedure _ReadAutoRunSettings(config: TCommonSettings);
    procedure _WriteAutoRunSettings(config: TCommonSettings);
  protected
    FOrigAssociateFiles: Boolean;

    function _SettingsLocationDisplay(loc: TSettingsSaveLocation): String;
    procedure _PopulateSaveLocation();
    procedure _EnableDisableControls(); virtual;
    function GetSettingsLocation(): TSettingsSaveLocation;

    function _DoOKClicked(): Boolean; virtual;
    procedure _InitAndReadSettings(config: TCommonSettings); virtual;
    procedure _WriteAllSettings(config: TCommonSettings); virtual;
    function _AreSettingsValid: Boolean; virtual;
  public


  end;

implementation

{$R *.DFM}

uses
           //delphi
  ShlObj,  // Required for CSIDL_PROGRAMS
{$IFDEF FREEOTFE_MAIN}
  MainSettings,
{$ENDIF}
{$IFDEF FREEOTFE_EXPLORER}
  ExplorerSettings,
{$ENDIF}


  //sdu, lcutils
  lcConsts, lcTypes,
  pkcs11_library,
  lcDialogs, SDUGeneral,
  SDUi18n, commonconsts,
  //librecrypt
  OTFEFreeOTFEDLL_U
  {$IFDEF _DXGETTEXT}
  , gnugettext
  {$ENDIF}
  ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  FILTER_LIBRARY_FILES = 'Library files (*.dll)|*.dll|All files|*.*';

const
  SHELL_VERB     = 'Open';
  CONTROL_MARGIN = 10;


function TfrmCommonOptions.GetSettingsLocation(): TSettingsSaveLocation;
var
  sl: TSettingsSaveLocation;
begin
  Result := GSettingsSaveLocation;
  for sl := low(sl) to high(sl) do begin
    if (_SettingsLocationDisplay(sl) = cbSettingsLocation.Items[cbSettingsLocation.ItemIndex]) then
    begin
      Result := sl;
      break;
    end;
  end;

end;

function TfrmCommonOptions._SettingsLocationDisplay(loc: TSettingsSaveLocation): String;
begin
  Result := '';

  case loc of
    slNone: Result     := SAVELOCATION_DO_NOT_SAVE;
    slExeDir: Result   := Format(SAVELOCATION_EXE_DIR, [Application.Title]);
    slProfile: Result  := SAVELOCATION_USER_PROFILE;
    slRegistry: Result := SAVELOCATION_REGISTRY;
    slCustom: Result   := SAVELOCATION_CUSTOMISED;
    else
      assert(False);
  end;

end;

procedure TfrmCommonOptions.pbOKClick(Sender: TObject);
begin
  if _DoOKClicked() then     ModalResult := mrOk;
end;

procedure TfrmCommonOptions.pbPostDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate post lock executable');
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostDismountExe.Text);
  if (OpenDialog.Execute) then begin
    edPostDismountExe.Text := OpenDialog.Filename;
  end;

end;

procedure TfrmCommonOptions.pbPostMountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate post open executable');
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPostMountExe.Text);
  if (OpenDialog.Execute) then begin
    // Strip drive letter
    edPostMountExe.Text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename) - 2);
  end;

end;

procedure TfrmCommonOptions.pbPreDismountBrowseClick(Sender: TObject);
begin
  inherited;
  OpenDialog.title   := _('Locate pre lock executable');
  OpenDialog.Options := OpenDialog.Options + [ofDontAddToRecent];
  SDUOpenSaveDialogSetup(OpenDialog, edPreDismountExe.Text);
  if (OpenDialog.Execute) then begin
    // Strip drive letter
    edPreDismountExe.Text := copy(OpenDialog.Filename, 3, length(OpenDialog.Filename) - 2);
  end;
end;



procedure TfrmCommonOptions._ReadAutoRunSettings(config: TCommonSettings);
begin
  inherited;

  edPostMountExe.Text    := config.PostMountExe;
  edPreDismountExe.Text  := config.PreDismountExe;
  edPostDismountExe.Text := config.PostDismountExe;

  ckPrePostExeWarn.Checked := config.WarnAutoRunExeErr;

end;

procedure TfrmCommonOptions._WriteAutoRunSettings(config: TCommonSettings);
begin
  inherited;

  config.PostMountExe    := edPostMountExe.Text;
  config.PreDismountExe  := edPreDismountExe.Text;
  config.PostDismountExe := edPostDismountExe.Text;

  config.WarnAutoRunExeErr := ckPrePostExeWarn.Checked;

end;


procedure TfrmCommonOptions.pbVerifyClick(Sender: TObject);
begin
  inherited;

  if _VerifyLibrary() then begin
    SDUMessageDlg(_('PKCS#11 library appears to be functional'), mtInformation);
  end;

end;



function TfrmCommonOptions._CheckPkcs11Settings(): Boolean;
begin
  Result := True;

  if Result then begin
    if ckEnablePKCS11.Checked then begin
      Result := _VerifyLibrary();
    end;
  end;

  if ckEnablePKCS11.Checked then begin
    if Result then begin
      if ckPKCS11AutoMount.Checked then begin
        if (OTFEFreeOTFEVolumeSelect1.Filename = '') then begin
          SDUMessageDlg(
            _(
            'If automount on PKCS#11 token insertion is enabled, the container to be mounted must be specified'),
            mtError
            );
          Result := False;
        end;
      end;

    end;
  end;

end;


function TfrmCommonOptions._DoOKClicked(): Boolean;
var
  oldSettingsLocation:   TSettingsSaveLocation;
  newSettingsLocation:   TSettingsSaveLocation;
  msgSegment:            String;
  vistaProgFilesDirWarn: Boolean;
  programFilesDir:       String;
  filename:              String;
  prevSDUDialogsStripSingleCRLF: Boolean;
  //  i:                     Integer;
  //  j:                     Integer;
  //  currTabSheet:          TTabSheet;
  deleteOldLocation:     Boolean;
begin
  Result := True;

  // Decode settings save location
  oldSettingsLocation := GSettingsSaveLocation;
  newSettingsLocation := GetSettingsLocation();

  if Result then begin
    // If user tried to save settings under C:\Program Files\... while
    // running under Vista, let the user know that Vista's security
    // system screws this up (it maps the save to somewhere in the user's
    // profile)
    vistaProgFilesDirWarn := False;
    if (SDUOSVistaOrLater() and (newSettingsLocation = slExeDir)) then begin
      programFilesDir       := SDUGetSpecialFolderPath(SDU_CSIDL_PROGRAM_FILES);
      filename              := GetSettings().GetSettingsFilename(newSettingsLocation);
      vistaProgFilesDirWarn := (Pos(uppercase(programFilesDir), uppercase(filename)) > 0);
    end;

    Result := not vistaProgFilesDirWarn;

    if not (Result) then begin
      prevSDUDialogsStripSingleCRLF := GSDUDialogsStripSingleCRLF;
      // Don't do special processing on this message
      GSDUDialogsStripSingleCRLF    := False;  // Don't do special processing on this message
      SDUMessageDlg(
        Format(_('Under Windows Vista and later, you cannot save your settings file anywhere under:' +
        SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF + 'due to Vista''s security/mapping system.'),
        [programFilesDir]) + SDUCRLF + SDUCRLF + _(
        'Please either select another location for storing your settings (e.g. your user profile), or move the LibreCrypt executable such that it is not stored underneath the directory shown above.'),
        mtWarning
        );
      GSDUDialogsStripSingleCRLF := prevSDUDialogsStripSingleCRLF;
      // Don't do special processing on this message
    end;

  end;

  if Result then begin
    Result := _AreSettingsValid;
    // For each tab, scan it's controls; if it's one of our frames, get it to
    // process...
   { for i := 0 to (pcOptions.PageCount - 1) do begin
      currTabSheet := pcOptions.Pages[i];
      for j := 0 to (currTabSheet.Controlcount - 1) do begin
        if (currTabSheet.Controls[j] is TfmeBaseOptions ) then begin
          if not (TfmeBaseOptions (currTabSheet.Controls[j]).CheckSettings()) then begin
            pcOptions.ActivePage := pcOptions.Pages[i];
            Result               := False;
            break;
          end;
        end;
      end;

      // Break out of loop early if problem detected
      if not (Result) then begin
        break;
      end;
    end;  }
  end;

  if Result then begin
    GSettingsSaveLocation := newSettingsLocation;

    _WriteAllSettings(GetSettings());

    if (GSettingsSaveLocation <> slNone) then begin
      Result := GetSettings().Save();
    end;

  end;

  if Result then begin
    // If settings aren't going to be saved, and they weren't saved previously,
    // warn user
    if ((oldSettingsLocation = newSettingsLocation) and (newSettingsLocation = slNone)) then begin
      Result := (SDUMessageDlg(_(
        'You have not specified a location where LibreCrypt should save its settings.') +
        SDUCRLF + SDUCRLF + _(
        'Although the settings entered will take effect, they will revert back to their defaults when LibreCrypt is exited.')
        + SDUCRLF + SDUCRLF + _(
        'Do you wish to select a location in order to make your settings persistant?'),
        mtWarning, [mbYes, mbNo], 0) = mrNo);
    end // If the save settings location has been changed, and we previously
        // saved settings to a file, ask the user if they want to delete their
        // old settings file
    else
    if ((oldSettingsLocation <> newSettingsLocation) and (oldSettingsLocation <> slNone)) then
    begin
      if (newSettingsLocation = slNone) then begin
        msgSegment := _('You have opted not to save your settings');
      end else begin
        msgSegment := _('You have changed the location where your settings will be saved');
      end;


      // If settings were previously stored in a file, prompt user if they want
      // to delete it
      deleteOldLocation := True;
      if ((oldSettingsLocation = slExeDir) or (oldSettingsLocation = slProfile) or
        (oldSettingsLocation = slCustom)) then begin
        deleteOldLocation := (SDUMessageDlg(msgSegment + SDUCRLF + SDUCRLF +
          _('Would you like LibreCrypt to delete your previous settings file?') +
          SDUCRLF + SDUCRLF + _(
          'Note: If you select "No" here, LibreCrypt may pick up your old settings the next time you start LibreCrypt'),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes);
      end;

      if deleteOldLocation then begin
        if not (GetSettings().DestroySettingsFile(oldSettingsLocation)) then begin
          if ((oldSettingsLocation = slExeDir) or (oldSettingsLocation = slProfile) or
            (oldSettingsLocation = slCustom)) then begin
            SDUMessageDlg(
              Format(_('Your previous settings file stored at:' + SDUCRLF +
              SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
              'could not be deleted. Please remove this file manually.'),
              [GetSettings().GetSettingsFilename(oldSettingsLocation)]),
              mtInformation
              );
          end else
          if (oldSettingsLocation = slRegistry) then begin
            SDUMessageDlg(
              Format(_(
              'Your previous settings file stored in the Windows registry under:' +
              SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
              'could not be deleted. Please delete this registry key manually.'),
              [GetSettings().RegistryKey()]),
              mtInformation
              );
          end;

        end;
      end else begin
        if ((oldSettingsLocation = slExeDir) or (oldSettingsLocation = slProfile) or
          (oldSettingsLocation = slCustom)) then begin
          SDUMessageDlg(
            Format(_('Your previous settings will remain stored at:' +
            SDUCRLF + SDUCRLF + '%s'),
            [GetSettings().GetSettingsFilename(oldSettingsLocation)]),
            mtInformation
            );
        end else
        if (oldSettingsLocation = slRegistry) then begin
          SDUMessageDlg(
            Format(_(
            'Your previous settings will remain stored in the Windows registry under:' +
            SDUCRLF + SDUCRLF + '%s'), [GetSettings().RegistryKey()]),
            mtInformation
            );
        end;
      end;
    end;
  end;

  if Result then begin
    if (ckAssociateFiles.Checked <> FOrigAssociateFiles) then begin
      if ckAssociateFiles.Checked then begin
        SDUFileExtnRegCmd(
          VOL_FILE_EXTN,
          SHELL_VERB,
          '"' + ParamStr(0) + '" /mount /volume "%1"'
          );
        SDUFileExtnRegIcon(
          VOL_FILE_EXTN,
          ParamStr(0),
          0
          );
      end else begin
        SDUFileExtnUnregIcon(VOL_FILE_EXTN);
        SDUFileExtnUnregCmd(
          VOL_FILE_EXTN,
          SHELL_VERB
          );
      end;

      FOrigAssociateFiles := ckAssociateFiles.Checked;
    end;

  end;

end;


procedure TfrmCommonOptions._PopulateSaveLocation();
var
  idx:    Integer;
  useIdx: Integer;
  sl:     TSettingsSaveLocation;
begin
  // Populate and set settings location dropdown
  cbSettingsLocation.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for sl := low(sl) to high(sl) do begin
    if ((sl = slCustom) and (GCustomSettingsLocation = '')) then begin
      // Skip this one; no custom location set
      continue;
    end;

    Inc(idx);
    cbSettingsLocation.Items.Add(_SettingsLocationDisplay(sl));
    if (GSettingsSaveLocation = sl) then begin
      useIdx := idx;
    end;
  end;
  cbSettingsLocation.ItemIndex := useIdx;

end;


procedure TfrmCommonOptions._InitializePkcs11Options();
begin
  SDUCenterControl(gbPKCS11, ccHorizontal);
  SDUCenterControl(gbPKCS11, ccVertical, 25);

  feLibFilename.DefaultExt         := 'dll';
  feLibFilename.OpenDialog.Options :=
    feLibFilename.OpenDialog.Options + [ofPathMustExist, ofFileMustExist, ofForceShowHidden];
  feLibFilename.OpenDialog.Options := feLibFilename.OpenDialog.Options + [ofDontAddToRecent];
  feLibFilename.Filter             := FILTER_LIBRARY_FILES;
  feLibFilename.FilterIndex        := 0;

  //  OTFEFreeOTFEVolumeSelect1.OTFEFreeOTFE         := OTFEFreeOTFE;
  OTFEFreeOTFEVolumeSelect1.FileSelectFilter     := FILE_FILTER_FLT_VOLUMES;
  OTFEFreeOTFEVolumeSelect1.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMES;

  // Disallow partition selection if running under DLL version
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := not (GetFreeOTFEBase() is TOTFEFreeOTFEDLL);

end;

procedure TfrmCommonOptions._ReadPkcs11Settings(config: TCommonSettings);
begin
  // General...
  ckEnablePKCS11.Checked := config.EnablePKCS11;
  feLibFilename.Filename := config.PKCS11LibraryPath;

  ckPKCS11AutoMount.Checked          := config.PKCS11AutoMount;
  OTFEFreeOTFEVolumeSelect1.Filename := config.MountPathOnPKCS11;
  ckPKCS11AutoDismount.Checked       := config.DismountOnPKCS11Remove;

end;

procedure TfrmCommonOptions._InitAndReadSettings(config: TCommonSettings);
var
  //  i:            Integer;
  //  j:            Integer;
  //  currTabSheet: TTabSheet;
  maxWidth: Integer;
  idx:      Integer;
  useIdx:   Integer;
  uf:       TUpdateFrequency;
begin
  FFlagClearLayoutOnSave := False;

  ckExploreAfterMount.Checked        := config.ExploreAfterMount;
  ckAdvancedMountDlg.Checked         := config.ShowAdvancedMountDialog;
  ckRevertVolTimestamps.Checked      := config.FreezeVolTimestamps;
  ckAllowNewlinesInPasswords.Checked := config.AllowNewlinesInPasswords;
  ckAllowTabsInPasswords.Checked     := config.AllowTabsInPasswords;
  ckPromptMountSuccessful.Checked    := config.InformIfMountedOK;
  ckShowPasswords.Checked            := config.ShowPasswords;
  ckStoreLayout.Checked              := config.StoreLayout;
    seMRUMaxItemCount.Value := config.MruList.MaxItems;

  _PopulateLanguages();

  // In case language code not found; reset to "(Default)" entry; at index 0
  SetLanguageSelection(config.LanguageCode);


  // Populate and set update frequency dropdown
  cbChkUpdatesFreq.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for uf := low(uf) to high(uf) do begin

    Inc(idx);
    cbChkUpdatesFreq.Items.Add(UpdateFrequencyTitle(uf));
    if (config.UpdateChkFreq = uf) then begin
      useIdx := idx;
    end;
  end;
  cbChkUpdatesFreq.ItemIndex := useIdx;

  _PopulateSaveLocation();

  _InitializePkcs11Options();
  _ReadPkcs11Settings(config);
  {
  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  for i := 0 to (pcOptions.PageCount - 1) do begin
    currTabSheet := pcOptions.Pages[i];
    for j := 0 to (currTabSheet.Controlcount - 1) do begin
      if (currTabSheet.Controls[j] is TfmeBaseOptions ) then begin
        TfmeBaseOptions (currTabSheet.Controls[j]).Align := alClient;
        TfmeBaseOptions (currTabSheet.Controls[j]).Initialize();
        TfmeBaseOptions (currTabSheet.Controls[j]).ReadSettings(config);
      end;
    end;
  end;   this now done by inheritence}

  // Center "assocate with .vol files" checkbox
  ckAssociateFiles.Caption := Format(_('Associate %s with ".vol" &files'), [Application.Title]);
  SDUCenterControl(ckAssociateFiles, ccHorizontal);

  // Reposition the "Settings location" controls, bearing in mind the label
  // can change width depending on the language used and whether it's in bold
  // or not

  // Setup for worst case scenario - the label is in bold (widest)
  lblSettingsLocation.font.style := [fsBold];

  // Determine full width of label, combobox and warning image
  maxWidth := (cbSettingsLocation.Width + CONTROL_MARGIN + lblSettingsLocation.Width +
    CONTROL_MARGIN + imgNoSaveWarning.Width);

  lblSettingsLocation.left := (self.Width - maxWidth) div 2;
  cbSettingsLocation.left  := (lblSettingsLocation.left + lblSettingsLocation.Width +
    CONTROL_MARGIN);
  imgNoSaveWarning.left    := (cbSettingsLocation.left + cbSettingsLocation.Width +
    CONTROL_MARGIN);


  _ReadAutoRunSettings(config);

  // Call enable/disable controls to put the label in bold/clear the bold
  _EnableDisableControls();

end;


procedure TfrmCommonOptions._WritePkcs11Settings(config: TCommonSettings);
begin
  // General...
  config.EnablePKCS11             := ckEnablePKCS11.Checked;
  config.PKCS11LibraryPath        := GetLibDLLFilename;
  config.PKCS11AutoMount          := ckPKCS11AutoMount.Checked;
  config.MountPathOnPKCS11 := OTFEFreeOTFEVolumeSelect1.Filename;
  config.DismountOnPKCS11Remove    := ckPKCS11AutoDismount.Checked;

end;

procedure TfrmCommonOptions._WriteAllSettings(config: TCommonSettings);
var
  useLang: TLanguageTranslation;
  //  i:            Integer;
  //  j:            Integer;
  //  currTabSheet: TTabSheet;
  uf:      TUpdateFrequency;


begin
  // General tab
  useLang                := GetSelectedLanguage();
  config.LanguageCode := useLang.Code;


  config.ShowPasswords        := ckShowPasswords.Checked;
  config.StoreLayout             := ckStoreLayout.Checked;
  config.ShowAdvancedMountDialog := ckAdvancedMountDlg.Checked;
  config.FreezeVolTimestamps  := ckRevertVolTimestamps.Checked;


  config.ShowToolbar                 := ckDisplayToolbar.Checked;
  config.ShowLargeToolbar         := ckDisplayToolbarLarge.Checked;
  config.ShowToolbarCaptions         := ckDisplayToolbarCaptions.Checked;
  config.ShowStatusbar         := ckDisplayStatusbar.Checked;
  config.InformIfMountedOK        := ckPromptMountSuccessful.Checked;
  config.AllowNewlinesInPasswords := ckAllowNewlinesInPasswords.Checked;
  config.AllowTabsInPasswords     := ckAllowTabsInPasswords.Checked;
  config.ExploreAfterMount           := ckExploreAfterMount.Checked;
    config.MruList.MaxItems := seMRUMaxItemCount.Value;


  // Decode update frequency
  config.UpdateChkFreq := ufNever;
  for uf := low(uf) to high(uf) do begin
    if (UpdateFrequencyTitle(uf) = cbChkUpdatesFreq.Items[cbChkUpdatesFreq.ItemIndex]) then begin
      config.UpdateChkFreq := uf;
      break;
    end;
  end;


  // Default drive letter
  if (cbDrive.ItemIndex = 0) then begin
    config.DefaultDriveChar := #0;
  end else begin
    config.DefaultDriveChar := DriveLetterChar(cbDrive.Items[cbDrive.ItemIndex][1]);
  end;

  _WritePkcs11Settings(config);
  _WriteAutoRunSettings(config);

  if FFlagClearLayoutOnSave then
    config.ClearLayout;


  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  {
  for i := 0 to (pcOptions.PageCount - 1) do begin
    currTabSheet := pcOptions.Pages[i];
    for j := 0 to (currTabSheet.Controlcount - 1) do begin
      if (currTabSheet.Controls[j] is TfmeBaseOptions ) then begin
        TfmeBaseOptions (currTabSheet.Controls[j]).WriteSettings(config);
      end;
    end;
  end; this now done by inheritence}
end;


function TfrmCommonOptions._AreSettingsValid: Boolean;
begin
  Result := _CheckPkcs11Settings;
  if not Result then
    pcOptions.ActivePage := tsPKCS11;
end;

procedure TfrmCommonOptions.FormCreate(Sender: TObject);
begin
  inherited;
  OTFEFreeOTFEVolumeSelect1.SelectFor            := fndOpen;
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := True;
    {$IFDEF _DXGETTEXT}
    // dont translate edits - includes paths
  gnugettext.TP_IgnoreClass(TEdit);
  {$ENDIF}
end;

procedure TfrmCommonOptions.FormShow(Sender: TObject);
begin
  // Special handling...
  _InitAndReadSettings(GetSettings());

  // Push the PKCS#11 tab to the far end; the "General" tab should be the
  // leftmost one
  tsPKCS11.PageIndex := (pcOptions.PageCount - 1);

  FOrigAssociateFiles      := SDUFileExtnIsRegCmd(VOL_FILE_EXTN, SHELL_VERB,
    ExtractFilename(ParamStr(0)));
  ckAssociateFiles.Checked := FOrigAssociateFiles;

  _EnableDisableControls();
  feLibFilename.TabStop     := False;
  feLibFilename.FilterIndex := 0;
  feLibFilename.OnChange    := ControlChanged;

end;

procedure TfrmCommonOptions._EnableDisableControls();
var
  //  i:            Integer;
  //  j:            Integer;
  //  currTabSheet: TTabSheet;
  pkcs11Enabled: Boolean;
begin

  pkcs11Enabled := ckEnablePKCS11.Checked;

  SDUEnableControl(feLibFilename, pkcs11Enabled);
  SDUEnableControl(lblLibrary, pkcs11Enabled);
  SDUEnableControl(pbAutoDetect, pkcs11Enabled);
  SDUEnableControl(pbVerify, (pkcs11Enabled and (GetLibDLLFilename <> '')));


  SDUEnableControl(gbPKCS11AutoActions, pkcs11Enabled);
  SDUEnableControl(OTFEFreeOTFEVolumeSelect1,
    (pkcs11Enabled and
    // Yes, this is needed; otherwise it'll enable/disable regardless
    ckPKCS11AutoMount.Checked));
  SDUEnableControl(lblAutoMountVolume, OTFEFreeOTFEVolumeSelect1.Enabled);
    {
  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  for i := 0 to (pcOptions.PageCount - 1) do begin
    currTabSheet := pcOptions.Pages[i];
    for j := 0 to (currTabSheet.Controlcount - 1) do begin
      if (currTabSheet.Controls[j] is TfmeBaseOptions ) then begin
        TfmeBaseOptions (currTabSheet.Controls[j]).EnableDisableControls();
      end;
    end;
  end;    this now done by inheritence}

  if (GetSettingsLocation() = slNone) then begin
    imgNoSaveWarning.Visible       := True;
    lblSettingsLocation.font.style := [fsBold];
  end else begin
    imgNoSaveWarning.Visible       := False;
    lblSettingsLocation.font.style := [];
  end;

  // Adjust "Save settings to:" label
  lblSettingsLocation.Left := (cbSettingsLocation.left - lblSettingsLocation.Width -
    CONTROL_MARGIN);

end;

procedure TfrmCommonOptions.pbAutoDetectClick(Sender: TObject);
var
  detectedLib: String;
begin
  inherited;

  detectedLib := PKCS11AutoDetectLibrary(self);
  if (detectedLib = PKCS11_USER_CANCELED) then begin
    if (detectedLib = '') then begin
      SDUMessageDlg(
        _('No PKCS#11 libraries detected.' + SDUCRLF + SDUCRLF +
        'Please manually enter the PKCS#11 library to be used.'),
        mtError
        );
    end else begin
      feLibFilename.Filename := detectedLib;
    end;
  end;// else  User canceled - do nothing
end;


procedure TfrmCommonOptions.pbCancelClick(Sender: TObject);
begin
  // Reset langugage used, in case it was changed by the user
  SDUSetLanguage(GetSettings().LanguageCode);

  ModalResult := mrCancel;

end;

procedure TfrmCommonOptions.pbLangDetailsClick(Sender: TObject);
var
  rcdLanguage: TLanguageTranslation;
begin
  inherited;

  rcdLanguage := GetSelectedLanguage();
  SDUMessageDlg(
    Format(_('Language name: %s'), [rcdLanguage.Name]) + SDUCRLF +
    Format(_('Language code: %s'), [rcdLanguage.Code]) + SDUCRLF +
    Format(_('Translator: %s'), [rcdLanguage.Contact])
    );
end;

procedure TfrmCommonOptions.cbLanguageChange(Sender: TObject);
var
  useLang: TLanguageTranslation;
  langIdx: Integer;
begin
  inherited;

  // Preserve selected language; the selected language gets change by the
  // PopulateLanguages() call
  langIdx := cbLanguage.ItemIndex;

  useLang := GetSelectedLanguage();
  _ChangeLanguage(useLang.Code);
  // Repopulate the languages list; translation would have translated them all
  _PopulateLanguages();

  // Restore selected
  cbLanguage.ItemIndex := langIdx;

  _EnableDisableControls();
end;

procedure TfrmCommonOptions.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;

procedure TfrmCommonOptions.feLibFilenamepbBrowseClick(Sender: TObject);
begin
  inherited;
  feLibFilename.pbBrowseClick(Sender);

end;

function TfrmCommonOptions.GetLibDLLFilename(): String;
begin
  Result := trim(feLibFilename.Filename);
end;

function TfrmCommonOptions._VerifyLibrary(): Boolean;
begin
  Result := True;
  if (GetLibDLLFilename = '') then begin
    SDUMessageDlg(_('Please specify the PKCS#11 library to be used'), mtError);
    Result := False;
  end;

  if Result then begin
    Result := PKCS11VerifyLibrary(GetLibDLLFilename);

    if not (Result) then begin
      SDUMessageDlg(_('The library specified does not appear to be a valid/working PKCS#11 library'),
        mtError);
    end;
  end;

end;


procedure TfrmCommonOptions._PopulateLanguages();
var
  i:            Integer;
  origLangCode: String;
  langCodes:    TStringList;
  sortedList:   TStringList;
begin
  // Store language information for later use...
  langCodes := TStringList.Create();
  try
    // Get all language codes...
    SDUGetLanguageCodes(langCodes);

    // +1 to include "Default"
    SetLength(FLanguages, langCodes.Count);

    // Spin though the languages, getting their corresponding human-readable
    // names
    origLangCode := SDUGetCurrentLanguageCode();
    try
      for i := 0 to (langCodes.Count - 1) do begin
        SDUSetLanguage(langCodes[i]);

        FLanguages[i].Code    := langCodes[i];
        FLanguages[i].Name    := _(CONST_LANGUAGE_ENGLISH);
        FLanguages[i].Contact := SDUGetTranslatorNameAndEmail();

        // Force set contact details for English version; the dxgettext software sets
        // this to some stupid default
        if (langCodes[i] = ISO639_ALPHA2_ENGLISH) then begin
          FLanguages[i].Contact := ENGLISH_TRANSLATION_CONTACT;
        end;

      end;
    finally
      // Flip back to original language...
      SDUSetLanguage(origLangCode);
    end;

  finally
    langCodes.Free();
  end;

  // Add "default" into the list
  SetLength(FLanguages, (length(FLanguages) + 1));
  FLanguages[length(FLanguages) - 1].Code    := '';
  FLanguages[length(FLanguages) - 1].Name    := _('(Default)');
  FLanguages[length(FLanguages) - 1].Contact := '';

  // Populate list
  sortedList := TStringList.Create();
  try
    for i := 0 to (length(FLanguages) - 1) do begin
      sortedList.AddObject(FLanguages[i].Name, @(FLanguages[i]));
    end;

    sortedList.Sorted := True;
    sortedList.Sorted := False;

    cbLanguage.Items.Assign(sortedList)
  finally
    sortedList.Free();
  end;

end;

function TfrmCommonOptions.GetLanguage(
  idx: Integer): TLanguageTranslation;
begin
  Result := (PLanguageTranslation(cbLanguage.Items.Objects[idx]))^;
end;


procedure TfrmCommonOptions.SetLanguageSelection(langCode: String);
var
  useIdx:   Integer;
  currLang: TLanguageTranslation;
  i:        Integer;
begin
  useIdx := 0;
  for i := 0 to (cbLanguage.items.Count - 1) do begin
    currLang := GetLanguage(i);
    if (currLang.Code = langCode) then begin
      useIdx := i;
      break;
    end;
  end;
  cbLanguage.ItemIndex := useIdx;
end;


procedure TfrmCommonOptions._ChangeLanguage(langCode: String);
var
  cur_lang  : string;

//  tmpConfig: TCommonSettings;
begin
// was saving and loading so edits and combos were updated
// cant do as singleton - prompt reboot instead
//  tmpConfig := TCommonSettings.Create();
//  try
//    tmpConfig.Assign(GetSettings());
//    _WriteAllSettings(tmpConfig);

{some meu items etc arent updated on a change only reload - it's rare to change this and complex to
 fix - so prompt to reboot instead
}
cur_lang := GetCurrentLanguage ;

    SDUSetLanguage(langCode);
    try

      SDURetranslateComponent(self);
      // not sure why - but these aren't retranslated otherwise
      SDURetranslateCombo(cbChkUpdatesFreq);
      SDURetranslateCombo(cbDrive);
      SDURetranslateCombo(cbLanguage);
    except
      on E: Exception do begin
        SDUTranslateComponent(self);
      end;
    end;
   if cur_lang <> GetCurrentLanguage then SDUMessageDlg(_('The language has changed. Some text will not update until the application is restarted'));

//    _InitAndReadSettings(tmpConfig);

//  finally
//    tmpConfig.Free();
//  end;

  // Call EnableDisableControls() as this re-jigs the "Save above settings to:"
  // label
  _EnableDisableControls();
end;

procedure TfrmCommonOptions.ckStoreLayoutClick(Sender: TObject);
begin
  inherited;

  if not (ckStoreLayout.Checked) then begin
    FFlagClearLayoutOnSave := SDUConfirmYN(Format(
      _('You have turned off the option to automatically save the window layout on exiting.') +
      SDUCRLF + SDUCRLF +
      _('Would you like to reset %s to its default window layout the next time it is started?'),
      [Application.Title]));
  end;

end;

function TfrmCommonOptions.GetSelectedLanguage: TLanguageTranslation;
begin
  Result := GetLanguage(cbLanguage.ItemIndex);
end;

end.
