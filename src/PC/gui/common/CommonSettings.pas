unit CommonSettings;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Dialogs,
  Classes, // Required for TShortCut
  Controls,  // Required for TDate
  INIFiles,
  SDUFilenameEdit_U,
  SDUMRUList;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  SECTION_GENERAL = 'General';

type
  TUpdateFrequency = (ufNever, ufDaily, ufWeekly, ufMonthly, ufAnnually);

resourcestring
  UPDATEFREQ_NEVER    = 'Never';
  UPDATEFREQ_DAILY    = 'Daily';
  UPDATEFREQ_WEEKLY   = 'Weekly';
  UPDATEFREQ_MONTHLY  = 'Monthly';
  UPDATEFREQ_ANNUALLY = 'Annually';

const
  UpdateFrequencyTitlePtr: array [TUpdateFrequency] of Pointer = (
                                  @UPDATEFREQ_NEVER,
                                  @UPDATEFREQ_DAILY,
                                  @UPDATEFREQ_WEEKLY,
                                  @UPDATEFREQ_MONTHLY,
                                  @UPDATEFREQ_ANNUALLY
                                 );
                                 
type
  TDragDropFileType = (ftPrompt, ftFreeOTFE, ftLinux);

resourcestring
  DRAGDROPFILETYPE_PROMPT   = '<prompt user>';
  DRAGDROPFILETYPE_FREEOTFE = 'FreeOTFE';
  DRAGDROPFILETYPE_LINUX    = 'Linux';

const
  DragDropFileTypeTitlePtr: array [TDragDropFileType] of Pointer = (
                                  @DRAGDROPFILETYPE_PROMPT,
                                  @DRAGDROPFILETYPE_FREEOTFE,
                                  @DRAGDROPFILETYPE_LINUX
                                 );

type
  TSettingsSaveLocation = (slNone, slExeDir, slProfile, slRegistry, slCustom);

resourcestring
  SAVELOCATION_DO_NOT_SAVE  = '<Do not save settings>';
  SAVELOCATION_EXE_DIR      = 'File in %1 directory';
  SAVELOCATION_USER_PROFILE = 'File in user profile';
  SAVELOCATION_REGISTRY     = 'Windows registry';
  SAVELOCATION_CUSTOMISED   = 'Customised location';

  UNKNOWN_SETTINGS_LOCATION = 'Unknown settings location?! Please report seeing this message!';

const
  SettingsSaveLocationSearchOrder: array [TSettingsSaveLocation] of TSettingsSaveLocation = (
                                  slCustom,  // Custom first; if the user's
                                             // specified a custom location,
                                             // use it before anything else
                                  slExeDir,
                                  slProfile,
                                  slRegistry,
                                  slNone
                                 );

type
{$M+}  // Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSettings = class
  private
    FCustomLocation: string;
  protected
    function IdentifyWhereSettingsStored(): TSettingsSaveLocation;

    procedure _Load(iniFile: TCustomINIFile); virtual;
    function  _Save(iniFile: TCustomINIFile): boolean; virtual;

    function  GetSettingsObj(): TCustomIniFile;
  public
    OptSettingsVersion: integer;
    OptSaveSettings: TSettingsSaveLocation;

    // General...
    OptAdvancedMountDlg: boolean;
    OptRevertVolTimestamps: boolean;
    OptShowPasswords: boolean;
    OptAllowNewlinesInPasswords: boolean;
    OptAllowTabsInPasswords: boolean;
    OptLanguageCode: string;
    OptDragDropFileType: TDragDropFileType;

    // Check for updates config...
    OptUpdateChkFrequency: TUpdateFrequency;
    OptUpdateChkLastChecked: TDate;
    OptUpdateChkSuppressNotifyVerMajor: integer;
    OptUpdateChkSuppressNotifyVerMinor: integer;

    // PKCS11...
    OptPKCS11Enable: boolean;
    OptPKCS11Library: string;
    OptPKCS11AutoMount: boolean;
    OptPKCS11AutoMountVolume: string;
    OptPKCS11AutoDismount: boolean;

    // MRU list...
    OptMRUList: TSDUMRUList;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Load(); virtual;
    function  Save(): boolean; virtual;

    procedure Assign(copyFrom: TSettings);

    function  DestroySettingsFile(loc: TSettingsSaveLocation): boolean;
    function  GetSettingsFilename(loc: TSettingsSaveLocation): string;

    function  PrettyPrintSettingsFile(loc: TSettingsSaveLocation): string;

    function  RegistryKey(): string; virtual; abstract;

  published
    property CustomLocation: string read FCustomLocation write FCustomLocation;
  end;

var
  // Global variable
  CommonSettingsObj: TSettings;

function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): string;
function DragDropFileTypeTitle(dragDropfileType: TDragDropFileType): string;

// These functions are duplicated in FreeOTFESettings.pas and FreeOTFEExplorerSettings.pas - can't move to CommonSettings.pas as they both use "Settings" object which is different for each of them
// Turn on/off option on open/save dialogs to add file selected under Start
// menu's "Documents", depending on whether MRU list is enabled or not
procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog); overload;
procedure FreeOTFEGUISetupOpenSaveDialog(fe: TSDUFilenameEdit); overload;

implementation

uses
  Windows,  // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Registry,
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  Menus,  // Required for ShortCutToText and TextToShortCut
  ShlObj;  // Required for CSIDL_PERSONAL

const
  SETTINGS_V1 = 1;
  SETTINGS_V2 = 2;

  // -- General section --
    OPT_SETTINGSVERSION                          = 'SettingsVersion';
      DFLT_OPT_SETTINGSVERSION                      = SETTINGS_V2;
    OPT_SAVESETTINGS                             = 'SaveSettings';
      DFLT_OPT_SAVESETTINGS                         = slProfile;
    OPT_ADVANCEDMOUNTDLG                         = 'AdvancedMountDlg';
      DFLT_OPT_ADVANCEDMOUNTDLG                     = FALSE;
    OPT_REVERTVOLTIMESTAMPS                      = 'RevertVolTimestamps';
      DFLT_OPT_REVERTVOLTIMESTAMPS                  = TRUE;
    OPT_SHOWPASSWORDS                            = 'ShowPasswords';
      DFLT_OPT_SHOWPASSWORDS                        = FALSE;
    OPT_LANGUAGECODE                             = 'LanguageCode';
      DFLT_OPT_LANGUAGECODE                         = '';
    OPT_DRAGDROP                                 = 'DragDropFileType';
      DFLT_OPT_DRAGDROP                             = ord(ftFreeOTFE);
    OPT_ALLOWNEWLINESINPASSWORDS                 = 'AllowNewlinesInPasswords';
      DFLT_OPT_ALLOWNEWLINESINPASSWORDS             = TRUE;
    OPT_ALLOWTABSINPASSWORDS                     = 'AllowTabsInPasswords';
      DFLT_OPT_ALLOWTABSINPASSWORDS                 = FALSE;

  // -- Check for updates section --
  SECTION_CHKUPDATE = 'Updates';
    OPT_CHKUPDATE_FREQ                           = 'CheckFrequency';
      DFLT_OPT_CHKUPDATE_FREQ                       = ufNever;
    OPT_CHKUPDATE_LASTCHECKED                    = 'LastChecked';
      DFLT_OPT_CHKUPDATE_LASTCHECKED                = '19000101';
    OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR         = 'SuppressNotifyVerMajor';
      DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR     = 0;
    OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR         = 'SuppressNotifyVerMinor';
      DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR     = 0;

  // -- PKCS#11 section --
  SECTION_PKCS11 = 'PKCS11';
    OPT_PKCS11ENABLE                             = 'Enabled';
      DFLT_OPT_PKCS11ENABLE                        = FALSE;
    OPT_PKCS11LIBRARY                            = 'Library';
      DFLT_OPT_PKCS11LIBRARY                       = '';
    OPT_PKCS11AUTOMOUNT                          = 'AutoMount';
      DFLT_OPT_PKCS11AUTOMOUNT                     = FALSE;
    OPT_PKCS11AUTOMOUNTVOLUME                    = 'AutoMountVolume';
      DFLT_OPT_PKCS11AUTOMOUNTVOLUME               = '';
    OPT_PKCS11AUTODISMOUNT                       = 'AutoDismount';
      DFLT_OPT_PKCS11AUTODISMOUNT                  = FALSE;

  // -- MRU list section --
  SECTION_MRULIST = 'MRUList';
    // (No name for this one)
      DFLT_OPT_MRUMAXITEMS                       = 0;


function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): string;
begin
  Result := LoadResString(UpdateFrequencyTitlePtr[updateFrequency]);
end;

function DragDropFileTypeTitle(dragDropfileType: TDragDropFileType): string;
begin
  Result := LoadResString(DragDropFileTypeTitlePtr[dragDropfileType]);
end;


procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog);
begin
  // Sanity check/short circuit
  if (dlg = nil) then
    begin
    exit;
    end;

  // Sanity check
  if (CommonSettingsObj = nil) then
    begin
    exit;
    end;

  if (CommonSettingsObj.OptMRUList.MaxItems <= 0) then
    begin
    if (dlg is TSaveDialog) then
      begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options + [ofDontAddToRecent];
      end
    else if (dlg is TOpenDialog) then
      begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options + [ofDontAddToRecent];
      end;
    end
  else
    begin
    if (dlg is TSaveDialog) then
      begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options - [ofDontAddToRecent];
      end
    else if (dlg is TOpenDialog) then
      begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options - [ofDontAddToRecent];
      end;
    end;
end;

procedure FreeOTFEGUISetupOpenSaveDialog(fe: TSDUFilenameEdit);
begin
  FreeOTFEGUISetupOpenSaveDialog(fe.OpenDialog);
  FreeOTFEGUISetupOpenSaveDialog(fe.SaveDialog);
end;

constructor TSettings.Create();
begin
  inherited;

  CustomLocation := '';
  OptMRUList:= TSDUMRUList.Create();

end;

destructor TSettings.Destroy();
begin
  OptMRUList.Free();

  inherited;
end;

procedure TSettings.Load();
var
  iniFile: TCustomINIFile;
begin
  OptSaveSettings:= IdentifyWhereSettingsStored();
  iniFile := GetSettingsObj();
  try
    _Load(iniFile);
  finally
    iniFile.Free();
  end;

end;

procedure TSettings._Load(iniFile: TCustomINIFile);
begin
  OptSettingsVersion := iniFile.ReadInteger(SECTION_GENERAL, OPT_SETTINGSVERSION, SETTINGS_V1);

  OptAdvancedMountDlg        := iniFile.ReadBool(SECTION_GENERAL,   OPT_ADVANCEDMOUNTDLG,       DFLT_OPT_ADVANCEDMOUNTDLG);
  OptRevertVolTimestamps     := iniFile.ReadBool(SECTION_GENERAL,   OPT_REVERTVOLTIMESTAMPS,    DFLT_OPT_REVERTVOLTIMESTAMPS);
  OptShowPasswords           := iniFile.ReadBool(SECTION_GENERAL,   OPT_SHOWPASSWORDS,    DFLT_OPT_SHOWPASSWORDS);
  OptAllowNewlinesInPasswords:= iniFile.ReadBool(SECTION_GENERAL,   OPT_ALLOWNEWLINESINPASSWORDS,    DFLT_OPT_ALLOWNEWLINESINPASSWORDS);
  OptAllowTabsInPasswords    := iniFile.ReadBool(SECTION_GENERAL,   OPT_ALLOWTABSINPASSWORDS,    DFLT_OPT_ALLOWTABSINPASSWORDS);
  OptLanguageCode            := iniFile.ReadString(SECTION_GENERAL, OPT_LANGUAGECODE,     DFLT_OPT_LANGUAGECODE);
  OptDragDropFileType        := TDragDropFileType(iniFile.ReadInteger(SECTION_GENERAL, OPT_DRAGDROP, DFLT_OPT_DRAGDROP));

  OptUpdateChkFrequency      := TUpdateFrequency(iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_FREQ,        ord(DFLT_OPT_CHKUPDATE_FREQ)));
  OptUpdateChkLastChecked    := SDUISO8601ToTDate(iniFile.ReadString(SECTION_CHKUPDATE,  OPT_CHKUPDATE_LASTCHECKED, DFLT_OPT_CHKUPDATE_LASTCHECKED));
  OptUpdateChkSuppressNotifyVerMajor := iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR, DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR);
  OptUpdateChkSuppressNotifyVerMinor := iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR, DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR);

  OptPKCS11Enable            := iniFile.ReadBool(SECTION_PKCS11,         OPT_PKCS11ENABLE,          DFLT_OPT_PKCS11ENABLE);
  OptPKCS11Library           := iniFile.ReadString(SECTION_PKCS11,       OPT_PKCS11LIBRARY,         DFLT_OPT_PKCS11LIBRARY);
  OptPKCS11AutoMount         := iniFile.ReadBool(SECTION_PKCS11,         OPT_PKCS11AUTOMOUNT,       DFLT_OPT_PKCS11AUTOMOUNT);
  OptPKCS11AutoMountVolume   := iniFile.ReadString(SECTION_PKCS11,       OPT_PKCS11AUTOMOUNTVOLUME, DFLT_OPT_PKCS11AUTOMOUNTVOLUME);
  OptPKCS11AutoDismount      := iniFile.ReadBool(SECTION_PKCS11,         OPT_PKCS11AUTODISMOUNT,    DFLT_OPT_PKCS11AUTODISMOUNT);

  OptMRUList.MaxItems := DFLT_OPT_MRUMAXITEMS;
  OptMRUList.Load(iniFile, SECTION_MRULIST);

end;

function TSettings.Save(): boolean;
var
  allOK: boolean;
  iniFile: TCustomINIFile;
begin
  allOK := FALSE;

  if (OptSaveSettings = slNone) then
    begin
    allOK := TRUE;
    end
  else
    begin
    iniFile := GetSettingsObj();
    if (iniFile <> nil) then
      begin
      try
        allOK := _Save(iniFile);

      finally
        iniFile.Free();
      end;
      end;
    end;

  if not(allOK) then
    begin
    allOK := FALSE;

    SDUMessageDlg(
               _('Your settings could not be saved.')+SDUCRLF+
                 SDUCRLF+
                 SDUParamSubstitute(_('Please ensure that you have suitable access rights in order to write to:'+SDUCRLF+
                       SDUCRLF+
                       '%1'),
                       [PrettyPrintSettingsFile(OptSaveSettings)]),
               mtError
              );
    end;

  Result := allOK;
end;

function TSettings._Save(iniFile: TCustomINIFile): boolean;
var
  allOK: boolean;
begin
  allOK := TRUE;

  try
    iniFile.WriteInteger(SECTION_GENERAL,     OPT_SETTINGSVERSION,           SETTINGS_V2);

    iniFile.WriteBool(SECTION_GENERAL,        OPT_ADVANCEDMOUNTDLG,          OptAdvancedMountDlg);
    iniFile.WriteBool(SECTION_GENERAL,        OPT_REVERTVOLTIMESTAMPS,       OptRevertVolTimestamps);
    iniFile.WriteBool(SECTION_GENERAL,        OPT_SHOWPASSWORDS,             OptShowPasswords);
    iniFile.WriteBool(SECTION_GENERAL,        OPT_ALLOWNEWLINESINPASSWORDS,  OptAllowNewlinesInPasswords);
    iniFile.WriteBool(SECTION_GENERAL,        OPT_ALLOWTABSINPASSWORDS,      OptAllowTabsInPasswords);
    iniFile.WriteString(SECTION_GENERAL,      OPT_LANGUAGECODE,              OptLanguageCode);
    iniFile.WriteInteger(SECTION_GENERAL,     OPT_DRAGDROP,                  ord(OptDragDropFileType));

    iniFile.WriteInteger(SECTION_CHKUPDATE,   OPT_CHKUPDATE_FREQ,            ord(OptUpdateChkFrequency));
    iniFile.WriteString(SECTION_CHKUPDATE,    OPT_CHKUPDATE_LASTCHECKED,     SDUTDateToISO8601(OptUpdateChkLastChecked));
    iniFile.WriteInteger(SECTION_CHKUPDATE,   OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR, OptUpdateChkSuppressNotifyVerMajor);
    iniFile.WriteInteger(SECTION_CHKUPDATE,   OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR, OptUpdateChkSuppressNotifyVerMinor);

    iniFile.WriteBool(SECTION_PKCS11,         OPT_PKCS11ENABLE,              OptPKCS11Enable);
    iniFile.WriteString(SECTION_PKCS11,       OPT_PKCS11LIBRARY,             OptPKCS11Library);
    iniFile.WriteBool(SECTION_PKCS11,         OPT_PKCS11AUTOMOUNT,           OptPKCS11AutoMount);
    iniFile.WriteString(SECTION_PKCS11,       OPT_PKCS11AUTOMOUNTVOLUME,     OptPKCS11AutoMountVolume);
    iniFile.WriteBool(SECTION_PKCS11,         OPT_PKCS11AUTODISMOUNT,        OptPKCS11AutoDismount);

    OptMRUList.Save(iniFile, SECTION_MRULIST);
    
  except
    on E:Exception do
      begin
      allOK := FALSE;
      end;
  end;

  Result := allOK;
end;

function TSettings.PrettyPrintSettingsFile(loc: TSettingsSaveLocation): string;
var
  retval: string;
begin
  retval := '';

  case loc of
    slNone:
      begin
      // Do nothing
      end;

    slExeDir,
    slProfile,
    slCustom:
      begin
      retval := GetSettingsFilename(loc);
      end;

    slRegistry:
      begin
      retval := RegistryKey();
      end;

    else
      begin
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);
      end;

    end;

  Result := retval;
end;


function TSettings.DestroySettingsFile(loc: TSettingsSaveLocation): boolean;
var
  filename: string;
  retval: boolean;
  registry: TRegistry;
begin
  retval := TRUE;

  case loc of
    slNone:
      begin
      // Do nothing; nothing to delete
      end;

    slExeDir,
    slProfile,
    slCustom:
      begin
      filename := GetSettingsFilename(loc);
      if (filename <> '') then
        begin
        retval := DeleteFile(filename);
        end;
      end;

    slRegistry:
      begin
      registry:= TRegistry.Create();
      try
        registry.RootKey := HKEY_CURRENT_USER;
        retval := registry.DeleteKey(RegistryKey());
      finally
        registry.Free();
      end;
      end;

    else
      begin
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);
      end;

    end;

  Result := retval;
end;

function TSettings.GetSettingsFilename(loc: TSettingsSaveLocation): string;
var
  iniFilenameOnly: string;
  filenameAndPath: string;
begin
  filenameAndPath := '';

  iniFilenameOnly := ChangeFileExt(ExtractFilename(ParamStr(0)), '.ini');

  case loc of
    slNone,
    slRegistry:
      begin
      // Do nothing; already set to empty string
      end;

    slExeDir:
      begin
      filenameAndPath := ExtractFilePath(ParamStr(0)) + iniFilenameOnly;
      end;

    slProfile:
      begin
      filenameAndPath := SDUGetSpecialFolderPath(CSIDL_PERSONAL)+
                         '\'+
                         iniFilenameOnly
      end;

    slCustom:
      begin
      filenameAndPath := CustomLocation;
      end;

    else
      begin
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);
      end;

    end;

  Result := filenameAndPath;
end;

// Identify where user settings are stored
function TSettings.IdentifyWhereSettingsStored(): TSettingsSaveLocation;
var
  sl: TSettingsSaveLocation;
  filename: string;
  retval: TSettingsSaveLocation;
  checkLocation: TSettingsSaveLocation;
  registry: TRegistry;
begin
  retval := slNone;

  if (CustomLocation <> '') then
    begin
    // If a custom location has been specified, we use it even if the file
    // doesn't exist
    retval := slCustom;
    end
  else
    begin
    for sl := low(SettingsSaveLocationSearchOrder) to high(SettingsSaveLocationSearchOrder) do
      begin
      checkLocation := SettingsSaveLocationSearchOrder[sl];

      if (
          (checkLocation = slExeDir) or
          (checkLocation = slProfile) or
          (checkLocation = slCustom)
         ) then
        begin
        filename := GetSettingsFilename(checkLocation);
        if (
            (filename <> '') and
            FileExists(filename)
           ) then
          begin
          retval := checkLocation;
          break;
          end;
        end
      else if (checkLocation = slRegistry) then
        begin
        // Test if registry key exists
        registry:= TRegistry.Create();
        try
          registry.RootKey := HKEY_CURRENT_USER;
          if registry.OpenKeyReadOnly(RegistryKey()) then
            begin
            retval := checkLocation;
            break;
            end;
        finally
          registry.Free();
        end;

        end;

      end;
    end;

  Result := retval;
end;

procedure TSettings.Assign(copyFrom: TSettings);
var
  iniFile: TINIFile;
begin
  // lplp - disabled for now.
  // lplp  - only used by options dialog, which should just save/restore internally; *shouldn't* need this function, but included for completeness
  exit;

  iniFile := TINIFile.Create('');
  try
    copyFrom._Save(iniFile);
    self._Load(iniFile);
  finally
    iniFile.Free();
  end;

end;

// Important: The *caller* is responsible for freeing off the object returned
// Retuns: A TCustomIniFile that can be used for saving/loading settings.
// This will return NIL if there's a problem (e.g. the .INI file can't be
// created)
function TSettings.GetSettingsObj(): TCustomIniFile;
var
  filename: string;
  retval: TCustomIniFile;
begin
  retval := nil;

  if (OptSaveSettings = slNone) then
    begin
    // Create "ini file" with blank filename; all calls to read from this will
    // fail; resulting in the default values being returned/used
    retval := TINIFile.Create('');
    end
  else if (
           (OptSaveSettings = slExeDir) or
           (OptSaveSettings = slProfile) or
           (OptSaveSettings = slCustom)
          ) then
    begin
    filename := GetSettingsFilename(OptSaveSettings);
    try
      retval := TINIFile.Create(filename);
    except
      on E:Exception do
        begin
        // Problem - e.g. can't create .INI file
        retval := nil;
        end;
    end;

    end
  else if (OptSaveSettings = slRegistry) then
    begin
      retval := TRegistryINIFile.Create(RegistryKey());
    end;

  Result := retval;
end;

END.


