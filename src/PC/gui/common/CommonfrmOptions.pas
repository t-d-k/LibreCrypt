unit CommonfrmOptions;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  CommonSettings, 
  OTFEFreeOTFEBase_U, ExtCtrls, SDUForms, SDUStdCtrls, CommonfmeOptions_Base,
  CommonfmeOptions_PKCS11;

type
  TfrmOptions = class(TSDUForm)
    pbOK: TButton;
    pbCancel: TButton;
    cbSettingsLocation: TComboBox;
    lblSettingsLocation: TLabel;
    ckAssociateFiles: TSDUCheckBox;
    pcOptions: TPageControl;
    imgNoSaveWarning: TImage;
    tsPKCS11: TTabSheet;
    fmeOptions_PKCS11: TfmeOptions_PKCS11;
    procedure pbOKClick(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    FOrigAssociateFiles: boolean;

    function  SettingsLocationDisplay(loc: TSettingsSaveLocation): string;
    procedure PopulateSaveLocation();
    procedure EnableDisableControls(); virtual;
    function  GetSettingsLocation(): TSettingsSaveLocation;

    function  DoOKClicked(): boolean; virtual;

    procedure AllTabs_InitAndReadSettings(config: TSettings); virtual;
    procedure AllTabs_WriteSettings(config: TSettings);
  public
    OTFEFreeOTFEBase: TOTFEFreeOTFEBase;

    procedure ChangeLanguage(langCode: string); virtual; abstract;
  end;

implementation

{$R *.DFM}

uses
  ShlObj,  // Required for CSIDL_PROGRAMS
{$IFDEF FREEOTFE_MAIN}
  FreeOTFESettings,
{$ENDIF}
{$IFDEF FREEOTFE_EXPLORER}
  FreeOTFEExplorerSettings,
{$ENDIF}
  SDUi18n,
  SDUGeneral,
  SDUDialogs;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}
  
const
  SHELL_VERB = 'Open';

  CONTROL_MARGIN = 10;


function TfrmOptions.GetSettingsLocation(): TSettingsSaveLocation;
var
  retval: TSettingsSaveLocation;
  sl: TSettingsSaveLocation;
begin
  retval := Settings.OptSaveSettings;
  for sl:=low(sl) to high(sl) do
    begin
    if (SettingsLocationDisplay(sl) = cbSettingsLocation.Items[cbSettingsLocation.ItemIndex]) then
      begin
      retval := sl;
      break;
      end;
    end;

  Result := retval;
end;

function TfrmOptions.SettingsLocationDisplay(loc: TSettingsSaveLocation): string;
var
  retval: string;
begin
  retval := '';

  case loc of
    slNone:     retval := SAVELOCATION_DO_NOT_SAVE;
    slExeDir:   retval := SDUParamSubstitute(
                                             SAVELOCATION_EXE_DIR,
                                             [Application.Title]
                                            );
    slProfile:  retval := SAVELOCATION_USER_PROFILE;
    slRegistry: retval := SAVELOCATION_REGISTRY;
    slCustom:   retval := SAVELOCATION_CUSTOMISED;
  end;

  Result := retval;
end;

procedure TfrmOptions.pbOKClick(Sender: TObject);
begin
  if DoOKClicked() then
    begin
    ModalResult := mrOK;
    end

end;

function TfrmOptions.DoOKClicked(): boolean;
var
  allOK: boolean;
  oldSettingsLocation: TSettingsSaveLocation;
  newSettingsLocation: TSettingsSaveLocation;
  msgSegment: string;
  vistaProgFilesDirWarn: boolean;
  programFilesDir: string;
  filename: string;
  prevSDUDialogsStripSingleCRLF: boolean;
  i: integer;
  j: integer;
  currTabSheet: TTabSheet;
  deleteOldLocation: boolean;
begin
  allOK := TRUE;

  // Decode settings save location
  oldSettingsLocation := Settings.OptSaveSettings;
  newSettingsLocation := GetSettingsLocation();

  if allOK then
    begin
    // If user tried to save settings under C:\Program Files\... while
    // running under Vista, let the user know that Vista's security
    // system screws this up (it maps the save to somewhere in the user's
    // profile)
    vistaProgFilesDirWarn:= FALSE;
    if (
        SDUOSVistaOrLater() and
        (newSettingsLocation = slExeDir)
       ) then
      begin
      programFilesDir := SDUGetSpecialFolderPath(SDU_CSIDL_PROGRAM_FILES);
      filename := Settings.GetSettingsFilename(newSettingsLocation);
      vistaProgFilesDirWarn := (Pos(
                                    uppercase(programFilesDir),
                                    uppercase(filename)
                                   ) > 0);
      end;

    allOK := not(vistaProgFilesDirWarn);

    if not(allOK) then
      begin
      prevSDUDialogsStripSingleCRLF := SDUDialogsStripSingleCRLF;  // Don't do special processing on this message
      SDUDialogsStripSingleCRLF := FALSE;  // Don't do special processing on this message
      SDUMessageDlg(
                    SDUParamSubstitute(_('Under Windows Vista, you cannot save your settings file anywhere under:'+SDUCRLF+
                            SDUCRLF+
                            '%1'+SDUCRLF+
                            SDUCRLF+
                            'due to Vista''s security/mapping system.'), [programFilesDir])+SDUCRLF+
                    SDUCRLF+
                    _('Please either select another location for storing your settings (e.g. your user profile), or move the FreeOTFE executable such that it is not stored underneath the directory shown above.'),
                    mtWarning
                   );
      SDUDialogsStripSingleCRLF := prevSDUDialogsStripSingleCRLF;  // Don't do special processing on this message
      end;

    end;

  if allOK then
    begin
    // For each tab, scan it's controls; if it's one of our frames, get it to
    // process...
    for i:=0 to (pcOptions.PageCount - 1) do
      begin
      currTabSheet := pcOptions.Pages[i];
      for j:=0 to (currTabSheet.Controlcount - 1) do
        begin
        if (currTabSheet.Controls[j] is TfmeOptions_Base) then
          begin
          if not(TfmeOptions_Base(currTabSheet.Controls[j]).CheckSettings()) then
            begin
            pcOptions.ActivePage := pcOptions.Pages[i];
            allOK := FALSE;
            break;
            end;
          end;
        end;

      // Break out of loop early if problem detected
      if not(allOK) then
        begin
        break;
        end;
      end;
    end;

  if allOK then
    begin
    Settings.OptSaveSettings := newSettingsLocation;

    AllTabs_WriteSettings(Settings);

    if (Settings.OptSaveSettings <> slNone) then
      begin
      allOK := Settings.Save();
      end;

    end;

  if allOK then
    begin
    // If settings aren't going to be saved, and they weren't saved previously,
    // warn user
    if (
        (oldSettingsLocation = newSettingsLocation) and
        (newSettingsLocation = slNone)
       ) then
      begin
      allOK := (SDUMessageDlg(
                              _('You have not specified a location where FreeOTFE should save its settings to.')+SDUCRLF+
                              SDUCRLF+
                              _('Although the settings entered will take effect, they will revert back to their defaults when FreeOTFE is exited.')+SDUCRLF+
                              SDUCRLF+
                              _('Do you wish to select a location in order to make your settings persistant?'),
                              mtWarning,
                              [mbYes, mbNo],
                              0
                             ) = mrNo);
      end
    // If the save settings location has been changed, and we previously
    // saved settings to a file, ask the user if they want to delete their
    // old settings file
    else if (
        (oldSettingsLocation <> newSettingsLocation) and
        (oldSettingsLocation <> slNone)
       ) then
      begin
      if (newSettingsLocation = slNone) then
        begin
        msgSegment := _('You have opted not to save your settings');
        end
      else
        begin
        msgSegment := _('You have changed the location where your settings will be saved');
        end;


      // If settings were previously stored in a file, prompt user if they want
      // to delete it
      deleteOldLocation := TRUE;
      if (
          (oldSettingsLocation = slExeDir) or
          (oldSettingsLocation = slProfile) or
          (oldSettingsLocation = slCustom)
         ) then
        begin
        deleteOldLocation := (SDUMessageDlg(
                                            msgSegment+SDUCRLF+
                                            SDUCRLF+
                                            _('Would you like FreeOTFE to delete your previous settings file?')+SDUCRLF+
                                            SDUCRLF+
                                            _('Note: If you select "No" here, FreeOTFE may pick up your old settings the next time you start FreeOTFE'),
                                            mtConfirmation,
                                            [mbYes, mbNo],
                                            0
                                           ) = mrYes);
        end;

      if deleteOldLocation then
        begin
        if not(Settings.DestroySettingsFile(oldSettingsLocation)) then
          begin
          if (
              (oldSettingsLocation = slExeDir) or
              (oldSettingsLocation = slProfile) or
              (oldSettingsLocation = slCustom)
             )then
            begin
            SDUMessageDlg(
                          SDUParamSubstitute(_('Your previous settings file stored at:'+SDUCRLF+
                                  SDUCRLF+
                                  '%1'+SDUCRLF+
                                  SDUCRLF+
                                  'could not be deleted. Please remove this file manually.'),
                                  [Settings.GetSettingsFilename(oldSettingsLocation)]),
                          mtInformation
                         );
            end
          else if (oldSettingsLocation = slRegistry) then
            begin
            SDUMessageDlg(
                          SDUParamSubstitute(_('Your previous settings file stored in the Windows registry under:'+SDUCRLF+
                                  SDUCRLF+
                                  '%1'+SDUCRLF+
                                  SDUCRLF+
                                  'could not be deleted. Please delete this registry key manually.'),
                                  [Settings.RegistryKey()]),
                          mtInformation
                         );
            end;

          end;
        end
      else
        begin
        if (
            (oldSettingsLocation = slExeDir) or
            (oldSettingsLocation = slProfile) or
            (oldSettingsLocation = slCustom)
           )then
          begin
          SDUMessageDlg(
                        SDUParamSubstitute(_('Your previous settings will remain stored at:'+SDUCRLF+
                                SDUCRLF+
                                '%1'),
                                [Settings.GetSettingsFilename(oldSettingsLocation)]),
                        mtInformation
                       );
          end
        else if (oldSettingsLocation = slRegistry) then
          begin
          SDUMessageDlg(
                        SDUParamSubstitute(_('Your previous settings will remain stored in the Windows registry under:'+SDUCRLF+
                                SDUCRLF+
                                '%1'),
                                [Settings.RegistryKey()]),
                        mtInformation
                       );
          end;
        end;
      end;
    end;

  if allOK then
    begin
    if (ckAssociateFiles.checked <> FOrigAssociateFiles) then
      begin
      if ckAssociateFiles.checked then
        begin
        SDUFileExtnRegCmd(
                          VOL_FILE_EXTN,
                          SHELL_VERB,
                          '"'+ParamStr(0)+'" /mount /volume "%1"'
                         );
        SDUFileExtnRegIcon(
                           VOL_FILE_EXTN,
                           ParamStr(0),
                           0
                          );
        end
      else
        begin
        SDUFileExtnUnregIcon(VOL_FILE_EXTN);
        SDUFileExtnUnregCmd(
                          VOL_FILE_EXTN,
                          SHELL_VERB
                         );
        end;

      FOrigAssociateFiles := ckAssociateFiles.checked;
      end;

    end;

  Result := allOK;
end;


procedure TfrmOptions.PopulateSaveLocation();
var
  idx: integer;
  useIdx: integer;
  sl: TSettingsSaveLocation;
begin
  // Populate and set settings location dropdown
  cbSettingsLocation.Items.Clear();
  idx := -1;
  useIdx := -1;
  for sl := low(sl) to high(sl) do
    begin
    if (
        (sl = slCustom) and 
        (Settings.CustomLocation = '')
       ) then
      begin
      // Skip this one; no custom location set
      continue;
      end;

    inc(idx);
    cbSettingsLocation.Items.Add(SettingsLocationDisplay(sl));
    if (Settings.OptSaveSettings = sl) then
      begin
      useIdx := idx;
      end;
    end;
  cbSettingsLocation.ItemIndex := useIdx;

end;

procedure TfrmOptions.AllTabs_InitAndReadSettings(config: TSettings);
var
  i: integer;
  j: integer;
  currTabSheet: TTabSheet;
  maxWidth: integer;
begin
  PopulateSaveLocation();

  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  for i:=0 to (pcOptions.PageCount - 1) do
    begin
    currTabSheet := pcOptions.Pages[i];
    for j:=0 to (currTabSheet.Controlcount - 1) do
      begin
      if (currTabSheet.Controls[j] is TfmeOptions_Base) then
        begin
        TfmeOptions_Base(currTabSheet.Controls[j]).Align := alClient;
        TfmeOptions_Base(currTabSheet.Controls[j]).Initialize();
        TfmeOptions_Base(currTabSheet.Controls[j]).ReadSettings(config);
        end;
      end;
    end;

  // Center "assocate with .vol files" checkbox
  ckAssociateFiles.Caption := SDUParamSubstitute(
                                                 _('Associate %1 with ".vol" &files'),
                                                 [Application.Title]
                                                );
  SDUCenterControl(ckAssociateFiles, ccHorizontal);

  // Reposition the "Settings location" controls, bearing in mind the label
  // can change width depending on the language used and whether it's in bold
  // or not

  // Setup for worst case scenario - the label is in bold (widest)
  lblSettingsLocation.font.style := [fsBold];

  // Determine full width of label, combobox and warning image
  maxWidth := (
               cbSettingsLocation.width +
               CONTROL_MARGIN +
               lblSettingsLocation.width +
               CONTROL_MARGIN +
               imgNoSaveWarning.width
              );

  lblSettingsLocation.left := (self.width - maxWidth) div 2;
  cbSettingsLocation.left := (
                              lblSettingsLocation.left+
                              lblSettingsLocation.width+
                              CONTROL_MARGIN
                             );
  imgNoSaveWarning.left := (
                            cbSettingsLocation.left+
                            cbSettingsLocation.width+
                            CONTROL_MARGIN
                           );
  // Call enable/disable controls to put the label in bold/clear the bold
  EnableDisableControls();

end;


procedure TfrmOptions.AllTabs_WriteSettings(config: TSettings);
var
  i: integer;
  j: integer;
  currTabSheet: TTabSheet;
begin
  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  for i:=0 to (pcOptions.PageCount - 1) do
    begin
    currTabSheet := pcOptions.Pages[i];
    for j:=0 to (currTabSheet.Controlcount - 1) do
      begin
      if (currTabSheet.Controls[j] is TfmeOptions_Base) then
        begin
        TfmeOptions_Base(currTabSheet.Controls[j]).WriteSettings(config);
        end;
      end;
    end;
end;


procedure TfrmOptions.FormShow(Sender: TObject);
begin
  // Special handling...
  fmeOptions_PKCS11.OTFEFreeOTFE := OTFEFreeOTFEBase;

  AllTabs_InitAndReadSettings(Settings);

  // Push the PKCS#11 tab to the far end; the "General" tab should be the
  // leftmost one
  tsPKCS11.PageIndex := (pcOptions.PageCount - 1);

  FOrigAssociateFiles := SDUFileExtnIsRegCmd(
                                          VOL_FILE_EXTN,
                                          SHELL_VERB,
                                          ExtractFilename(ParamStr(0))
                                         );
  ckAssociateFiles.checked := FOrigAssociateFiles;

  EnableDisableControls();

end;

procedure TfrmOptions.EnableDisableControls();
var
  i: integer;
  j: integer;
  currTabSheet: TTabSheet;
begin
  // For each tab, scan it's controls; if it's one of our frames, get it to
  // process...
  for i:=0 to (pcOptions.PageCount - 1) do
    begin
    currTabSheet := pcOptions.Pages[i];
    for j:=0 to (currTabSheet.Controlcount - 1) do
      begin
      if (currTabSheet.Controls[j] is TfmeOptions_Base) then
        begin
        TfmeOptions_Base(currTabSheet.Controls[j]).EnableDisableControls();
        end;
      end;
    end;

  if (GetSettingsLocation() = slNone) then
    begin
    imgNoSaveWarning.visible := TRUE;
    lblSettingsLocation.font.style := [fsBold];
    end
  else
    begin
    imgNoSaveWarning.visible := FALSE;
    lblSettingsLocation.font.style := [];
    end;

  // Adjust "Save settings to:" label
  lblSettingsLocation.Left := (
                               cbSettingsLocation.left -
                               lblSettingsLocation.Width -
                               CONTROL_MARGIN
                              );

end;

procedure TfrmOptions.pbCancelClick(Sender: TObject);
begin
  // Reset langugage used, in case it was changed by the user
  SDUSetLanguage(Settings.OptLanguageCode);

  ModalResult := mrCancel;
  
end;

procedure TfrmOptions.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
  
end;

END.


