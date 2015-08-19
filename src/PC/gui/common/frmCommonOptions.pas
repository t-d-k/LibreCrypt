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
   SDUFrames, SDUFilenameEdit_U , SDUForms, SDUStdCtrls,
   //librecrypt
   fmeVolumeSelect,
    OTFEFreeOTFEBase_U
  ;

type
  TfrmCommonOptions = class (TSDUForm)
    pbOK:      TButton;
    pbCancel:  TButton;
    cbSettingsLocation: TComboBox;
    lblSettingsLocation: TLabel;
    ckAssociateFiles: TSDUCheckBox;
    pcOptions: TPageControl;
    imgNoSaveWarning: TImage;
    tsPKCS11:  TTabSheet;
    gbPKCS11: TGroupBox;
    lblLibrary: TLabel;
    ckEnablePKCS11: TCheckBox;
    pbVerify: TButton;
    gbPKCS11AutoActions: TGroupBox;
    lblAutoMountVolume: TLabel;
    ckPKCS11AutoDismount: TCheckBox;
    ckPKCS11AutoMount: TCheckBox;
    OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect;
    feLibFilename: TSDUFilenameEdit;
    pbAutoDetect: TButton;

    procedure pbOKClick(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbAutoDetectClick(Sender: TObject);
    procedure pbVerifyClick(Sender: TObject);
  private
    procedure InitializePkcs11Options;
    procedure ReadPkcs11Settings(config: TCommonSettings);
    function LibraryDLL: String;
    function VerifyLibrary: Boolean;
    procedure WritePkcs11Settings(config: TCommonSettings);
    function CheckPkcs11Settings: Boolean;
  protected
    FOrigAssociateFiles: Boolean;

    function SettingsLocationDisplay(loc: TSettingsSaveLocation): String;
    procedure PopulateSaveLocation();
    procedure EnableDisableControls(); virtual;
    function GetSettingsLocation(): TSettingsSaveLocation;

    function DoOKClicked(): Boolean; virtual;

    procedure AllTabs_InitAndReadSettings(config: TCommonSettings); virtual;
    procedure AllTabs_WriteSettings(config: TCommonSettings);  virtual;
    function AreSettingsValid :Boolean  ; virtual;
  public

    procedure ChangeLanguage(langCode: String); virtual; abstract;
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
lcConsts,
  pkcs11_library,
  lcDialogs, SDUGeneral,
  SDUi18n,
  //librecrypt
  OTFEFreeOTFEDLL_U
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
  SHELL_VERB = 'Open';
  CONTROL_MARGIN = 10;


function TfrmCommonOptions.GetSettingsLocation(): TSettingsSaveLocation;
var
  sl: TSettingsSaveLocation;
begin
  Result := GetSettings().OptSaveSettings;
  for sl := low(sl) to high(sl) do begin
    if (SettingsLocationDisplay(sl) = cbSettingsLocation.Items[cbSettingsLocation.ItemIndex]) then
    begin
      Result := sl;
      break;
    end;
  end;

end;

function TfrmCommonOptions.SettingsLocationDisplay(loc: TSettingsSaveLocation): String;
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
  if DoOKClicked() then begin
    ModalResult := mrOk;
  end;

end;

procedure TfrmCommonOptions.pbVerifyClick(Sender: TObject);
begin
  inherited;

  if VerifyLibrary() then begin
    SDUMessageDlg(_('PKCS#11 library appears to be functional'), mtInformation);
  end;

end;



function TfrmCommonOptions.CheckPkcs11Settings(): Boolean;
begin
  Result := true;

  if Result then begin
    if ckEnablePKCS11.Checked then begin
      Result := VerifyLibrary();
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


function TfrmCommonOptions.DoOKClicked(): Boolean;
var
  oldSettingsLocation:   TSettingsSaveLocation;
  newSettingsLocation:   TSettingsSaveLocation;
  msgSegment:            String;
  vistaProgFilesDirWarn: Boolean;
  programFilesDir:       String;
  filename:              String;
  prevSDUDialogsStripSingleCRLF: Boolean;
  i:                     Integer;
  j:                     Integer;
  currTabSheet:          TTabSheet;
  deleteOldLocation:     Boolean;
begin
  Result := True;

  // Decode settings save location
  oldSettingsLocation := GetSettings().OptSaveSettings;
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

    Result := not (vistaProgFilesDirWarn);

    if not (Result) then begin
      prevSDUDialogsStripSingleCRLF := GSDUDialogsStripSingleCRLF;
      // Don't do special processing on this message
      GSDUDialogsStripSingleCRLF    := False;  // Don't do special processing on this message
      SDUMessageDlg(
        Format(_(
        'Under Windows Vista, you cannot save your settings file anywhere under:' +
        SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
        'due to Vista''s security/mapping system.'), [programFilesDir]) +
        SDUCRLF + SDUCRLF + _(
        'Please either select another location for storing your settings (e.g. your user profile), or move the LibreCrypt executable such that it is not stored underneath the directory shown above.'),
        mtWarning
        );
      GSDUDialogsStripSingleCRLF := prevSDUDialogsStripSingleCRLF;
      // Don't do special processing on this message
    end;

  end;

  if Result then begin
    Result               := AreSettingsValid;
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
    GetSettings().OptSaveSettings := newSettingsLocation;

    AllTabs_WriteSettings(GetSettings());

    if (GetSettings().OptSaveSettings <> slNone) then begin
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
              Format(_('Your previous settings file stored at:' +
              SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
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


procedure TfrmCommonOptions.PopulateSaveLocation();
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
    if ((sl = slCustom) and (GetSettings().CustomLocation = '')) then begin
      // Skip this one; no custom location set
      continue;
    end;

    Inc(idx);
    cbSettingsLocation.Items.Add(SettingsLocationDisplay(sl));
    if (GetSettings().OptSaveSettings = sl) then begin
      useIdx := idx;
    end;
  end;
  cbSettingsLocation.ItemIndex := useIdx;

end;


procedure TfrmCommonOptions.InitializePkcs11Options();
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

procedure TfrmCommonOptions.ReadPkcs11Settings(config: TCommonSettings);
begin
  // General...
  ckEnablePKCS11.Checked := config.OptPKCS11Enable;
  feLibFilename.Filename := config.OptPKCS11Library;

  ckPKCS11AutoMount.Checked          := config.OptPKCS11AutoMount;
  OTFEFreeOTFEVolumeSelect1.Filename := config.OptPKCS11AutoMountVolume;
  ckPKCS11AutoDismount.Checked       := config.OptPKCS11AutoDismount;

end;

procedure TfrmCommonOptions.AllTabs_InitAndReadSettings(config: TCommonSettings);
var
  i:            Integer;
  j:            Integer;
  currTabSheet: TTabSheet;
  maxWidth:     Integer;
begin
  PopulateSaveLocation();

  InitializePkcs11Options();
  ReadPkcs11Settings(config);
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
  ckAssociateFiles.Caption := Format(
    _('Associate %s with ".vol" &files'), [Application.Title]);
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
  // Call enable/disable controls to put the label in bold/clear the bold
  EnableDisableControls();

end;


procedure TfrmCommonOptions.WritePkcs11Settings(config: TCommonSettings);
begin
  // General...
  config.OptPKCS11Enable          := ckEnablePKCS11.Checked;
  config.OptPKCS11Library         := LibraryDLL;
  config.OptPKCS11AutoMount       := ckPKCS11AutoMount.Checked;
  config.OptPKCS11AutoMountVolume := OTFEFreeOTFEVolumeSelect1.Filename;
  config.OptPKCS11AutoDismount    := ckPKCS11AutoDismount.Checked;

end;

procedure TfrmCommonOptions.AllTabs_WriteSettings(config: TCommonSettings);
var
  i:            Integer;
  j:            Integer;
  currTabSheet: TTabSheet;
begin
  WritePkcs11Settings(config);
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


function TfrmCommonOptions.AreSettingsValid: Boolean;
begin
  result := CheckPkcs11Settings;
  if not result then pcOptions.ActivePage := pcOptions.Pages[0];
end;

procedure TfrmCommonOptions.FormCreate(Sender: TObject);
begin
  inherited;
  OTFEFreeOTFEVolumeSelect1.SelectFor            := fndOpen;
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := True;
end;

procedure TfrmCommonOptions.FormShow(Sender: TObject);
begin
  // Special handling...
  //  fmeOptions_PKCS11.OTFEFreeOTFE := OTFEFreeOTFEBase;

  AllTabs_InitAndReadSettings(GetSettings());

  // Push the PKCS#11 tab to the far end; the "General" tab should be the
  // leftmost one
  tsPKCS11.PageIndex := (pcOptions.PageCount - 1);

  FOrigAssociateFiles      := SDUFileExtnIsRegCmd(VOL_FILE_EXTN, SHELL_VERB,
    ExtractFilename(ParamStr(0)));
  ckAssociateFiles.Checked := FOrigAssociateFiles;

  EnableDisableControls();
     feLibFilename.        TabStop := False           ;
     feLibFilename.     FilterIndex := 0         ;
     feLibFilename.     OnChange := ControlChanged;

end;

procedure TfrmCommonOptions.EnableDisableControls();
var
  i:            Integer;
  j:            Integer;
  currTabSheet: TTabSheet;
  pkcs11Enabled: Boolean;
begin

   pkcs11Enabled := ckEnablePKCS11.Checked;

  SDUEnableControl(feLibFilename, pkcs11Enabled);
  SDUEnableControl(lblLibrary, pkcs11Enabled);
  SDUEnableControl(pbAutoDetect, pkcs11Enabled);
  SDUEnableControl(pbVerify, (pkcs11Enabled and (LibraryDLL <> '')));


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
    // User canceled - do nothing
  end else
  if (detectedLib = '') then begin
    SDUMessageDlg(
      _('No PKCS#11 libraries detected.' + SDUCRLF + SDUCRLF +
      'Please manually enter the PKCS#11 library to be used.'),
      mtError
      );
  end else begin
    feLibFilename.Filename := detectedLib;
  end;

end;


procedure TfrmCommonOptions.pbCancelClick(Sender: TObject);
begin
  // Reset langugage used, in case it was changed by the user
  SDUSetLanguage(GetSettings().OptLanguageCode);

  ModalResult := mrCancel;

end;

procedure TfrmCommonOptions.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();

end;




function TfrmCommonOptions.LibraryDLL(): String;
begin
  Result := trim(feLibFilename.Filename);
end;

function TfrmCommonOptions.VerifyLibrary(): Boolean;
begin
  Result := True;
  if (LibraryDLL = '') then begin
    SDUMessageDlg(_('Please specify the PKCS#11 library to be used'), mtError);
    Result := False;
  end;

  if Result then begin
    Result := PKCS11VerifyLibrary(LibraryDLL);

    if not (Result) then begin
      SDUMessageDlg(_('The library specified does not appear to be a valid/working PKCS#11 library'),
        mtError);
    end;
  end;

end;

end.
