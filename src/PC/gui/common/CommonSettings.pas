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
  Classes, Dialogs,
  // Required for TShortCut
  Controls,  // Required for TDate
  INIFiles,
  //sdu
  SDUFilenameEdit_U,
  SDUGeneral, SDUMRUList;

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
  // ufRandom = once per thousand startups, randomly - default
  // ufAlways = each time starts up
  TUpdateFrequency = (ufNever, ufAlways, ufWeekly, ufMonthly, ufAnnually, ufRandom);
{ TODO -otdk -crefactor : why resource strings? - array easier }
resourcestring
  UPDATEFREQ_NEVER    = 'Never';
  UPDATEFREQ_ALWAYS   = 'Every Start Up';
  UPDATEFREQ_WEEKLY   = 'Weekly';
  UPDATEFREQ_MONTHLY  = 'Monthly';
  UPDATEFREQ_ANNUALLY = 'Annually';
  UPDATEFREQ_RANDOM   = 'Randomly';

const
  UpdateFrequencyTitlePtr: array [TUpdateFrequency] of Pointer =
    (@UPDATEFREQ_NEVER, @UPDATEFREQ_ALWAYS, @UPDATEFREQ_WEEKLY, @UPDATEFREQ_MONTHLY,
    @UPDATEFREQ_ANNUALLY, @UPDATEFREQ_RANDOM
    );

type
  TDragDropFileType = (ftPrompt, ftFreeOTFE, ftLinux);

resourcestring
  DRAGDROPFILETYPE_PROMPT   = '<prompt user>';
  DRAGDROPFILETYPE_FREEOTFE = 'FreeOTFE';
  DRAGDROPFILETYPE_LINUX    = 'Linux';

const
  DragDropFileTypeTitlePtr: array [TDragDropFileType] of Pointer =
    (@DRAGDROPFILETYPE_PROMPT, @DRAGDROPFILETYPE_FREEOTFE, @DRAGDROPFILETYPE_LINUX
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

  // Section names which are common
  SECTION_CONFIRMATION = 'Confirmation';

type
{$M+}// Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"

  { TODO -otdk -crefactor : use rtti to read and write -much easier, just define properties + defaults }
  TCommonSettings = class
  private
    FCustomLocation: String;
  protected
    function IdentifyWhereSettingsStored(): TSettingsSaveLocation;

    procedure _Load(iniFile: TCustomINIFile); virtual;
    function _Save(iniFile: TCustomINIFile): Boolean; virtual;

    function GetSettingsObj(): TCustomIniFile;
  public
    OptSettingsVersion: Integer;
    OptSaveSettings:    TSettingsSaveLocation;

    // General...
    OptExploreAfterMount:    Boolean;
    OptAdvancedMountDlg:     Boolean;
    OptRevertVolTimestamps:  Boolean;
    OptShowPasswords:        Boolean;
    OptAllowNewlinesInPasswords: Boolean;
    OptAllowTabsInPasswords: Boolean;
    OptLanguageCode:         String;
    OptDragDropFileType:     TDragDropFileType;
    OptDefaultDriveLetter:   DriveLetterChar;

    // Prompts and messages
    OptPromptMountSuccessful: Boolean;  // If set, display an info msg after successful mount

    // Check for updates config...
    OptUpdateChkFrequency:   TUpdateFrequency;
    OptUpdateChkLastChecked: TDate;
    OptUpdateChkSuppressNotifyVerMajor: Integer;
    OptUpdateChkSuppressNotifyVerMinor: Integer;

    // PKCS11...
    OptPKCS11Enable:          Boolean;
    OptPKCS11Library:         String;
    OptPKCS11AutoMount:       Boolean;
    OptPKCS11AutoMountVolume: String;
    OptPKCS11AutoDismount:    Boolean;

    // MRU list...
    OptMRUList: TSDUMRUList;

    // Autorun...
    OptPostMountExe:    String;
    OptPreDismountExe:  String;
    OptPostDismountExe: String;
    OptPrePostExeWarn:  Boolean;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Load(); virtual;
    function Save(ShowWarningIfRO: Boolean = true): Boolean; virtual;

    procedure Assign(copyFrom: TCommonSettings);

    function DestroySettingsFile(loc: TSettingsSaveLocation): Boolean;
    function GetSettingsFilename(loc: TSettingsSaveLocation): String;

    function PrettyPrintSettingsFile(loc: TSettingsSaveLocation): String;

    function RegistryKey(): String; virtual; abstract;

    function IsRememberedDlgAnswer(msg:String;out answer:Integer) : Boolean;

  published
    property CustomLocation: String Read FCustomLocation Write FCustomLocation;
  end;

var
  // Global variable
  CommonSettingsObj: TCommonSettings;

function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): String;
function DragDropFileTypeTitle(dragDropfileType: TDragDropFileType): String;

// These functions are duplicated in MainSettings.pas and ExplorerSettings.pas - can't move to CommonSettings.pas as they both use "Settings" object which is different for each of them
 // Turn on/off option on open/save dialogs to add file selected under Start
 // menu's "Documents", depending on whether MRU list is enabled or not
procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog); overload;
procedure FreeOTFEGUISetupOpenSaveDialog(fe: TSDUFilenameEdit); overload;

implementation

uses
  Windows,   // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Menus, Registry,
           // Required for ShortCutToText and TextToShortCut
  ShlObj,  // Required for CSIDL_PERSONAL
           //sdu
  lcDialogs, SDUi18n;

const
  SETTINGS_V1 = 1;
  SETTINGS_V2 = 2;

  // -- General section --
  OPT_SETTINGSVERSION           = 'SettingsVersion';
  DFLT_OPT_SETTINGSVERSION      = SETTINGS_V2;
  OPT_SAVESETTINGS              = 'SaveSettings';
  DFLT_OPT_SAVESETTINGS         = slProfile;
  OPT_EXPLOREAFTERMOUNT         = 'ExploreAfterMount';
  DFLT_OPT_EXPLOREAFTERMOUNT    = False;
  OPT_ADVANCEDMOUNTDLG          = 'AdvancedMountDlg';
  DFLT_OPT_ADVANCEDMOUNTDLG     = False;
  OPT_REVERTVOLTIMESTAMPS       = 'RevertVolTimestamps';
  DFLT_OPT_REVERTVOLTIMESTAMPS  = True;
  OPT_SHOWPASSWORDS             = 'ShowPasswords';
  DFLT_OPT_SHOWPASSWORDS        = False;
  OPT_LANGUAGECODE              = 'LanguageCode';
  DFLT_OPT_LANGUAGECODE         = '';
  OPT_DRAGDROP                  = 'DragDropFileType';
  DFLT_OPT_DRAGDROP             = Ord(ftFreeOTFE);
  OPT_ALLOWNEWLINESINPASSWORDS  = 'AllowNewlinesInPasswords';
  DFLT_OPT_ALLOWNEWLINESINPASSWORDS = True;
  OPT_ALLOWTABSINPASSWORDS      = 'AllowTabsInPasswords';
  DFLT_OPT_ALLOWTABSINPASSWORDS = False;
  OPT_DEFAULTDRIVELETTER        = 'DefaultDriveLetter';
  DFLT_OPT_DEFAULTDRIVELETTER   = '#';

  // -- Prompts and messages --
  OPT_PROMPTMOUNTSUCCESSFUL      = 'OptPromptMountSuccessful';
  DFLT_OPT_PROMPTMOUNTSUCCESSFUL = True;

  // -- Check for updates section --
  SECTION_CHKUPDATE                    = 'Updates';
  OPT_CHKUPDATE_FREQ                   = 'CheckFrequency';
  DFLT_OPT_CHKUPDATE_FREQ              = ufRandom;
  OPT_CHKUPDATE_LASTCHECKED            = 'LastChecked';
  //this should be release date and updated with each release
  DFLT_OPT_CHKUPDATE_LASTCHECKED       = '20140820';
  { DONE 1 -otdk -ctest : auto update not tested }
  OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR = 'SuppressNotifyVerMajor';
  DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR = 0;
  OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR = 'SuppressNotifyVerMinor';
  DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR = 0;

  // -- PKCS#11 section --
  SECTION_PKCS11                 = 'PKCS11';
  OPT_PKCS11ENABLE               = 'Enabled';
  DFLT_OPT_PKCS11ENABLE          = False;
  OPT_PKCS11LIBRARY              = 'Library';
  DFLT_OPT_PKCS11LIBRARY         = '';
  OPT_PKCS11AUTOMOUNT            = 'AutoMount';
  DFLT_OPT_PKCS11AUTOMOUNT       = False;
  OPT_PKCS11AUTOMOUNTVOLUME      = 'AutoMountVolume';
  DFLT_OPT_PKCS11AUTOMOUNTVOLUME = '';
  OPT_PKCS11AUTODISMOUNT         = 'AutoDismount';
  DFLT_OPT_PKCS11AUTODISMOUNT    = False;

  // -- MRU list section --
  SECTION_MRULIST      = 'MRUList';
  // (No name for this one)
  DFLT_OPT_MRUMAXITEMS = 0;

  // -- Autorun section --
  SECTION_AUTORUN          = 'Autorun';
  OPT_POSTMOUNTEXE         = 'PostMountExe';
  DFLT_OPT_POSTMOUNTEXE    = '';
  OPT_PREDISMOUNTEXE       = 'PreDismountExe';
  DFLT_OPT_PREDISMOUNTEXE  = '';
  OPT_POSTDISMOUNTEXE      = 'PostDismountExe';
  DFLT_OPT_POSTDISMOUNTEXE = '';
  OPT_PREPOSTEXEWARN       = 'PrePostExeWarn';
  DFLT_OPT_PREPOSTEXEWARN  = True;

function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): String;
begin
  Result := LoadResString(UpdateFrequencyTitlePtr[updateFrequency]);
end;

function DragDropFileTypeTitle(dragDropfileType: TDragDropFileType): String;
begin
  Result := LoadResString(DragDropFileTypeTitlePtr[dragDropfileType]);
end;


procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog);
begin
  // Sanity check/short circuit
  if (dlg = nil) then begin
    exit;
  end;

  // Sanity check
  if (CommonSettingsObj = nil) then begin
    exit;
  end;

  if (CommonSettingsObj.OptMRUList.MaxItems <= 0) then begin
    if (dlg is TSaveDialog) then begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options + [ofDontAddToRecent];
    end else
    if (dlg is TOpenDialog) then begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options + [ofDontAddToRecent];
    end;
  end else begin
    if (dlg is TSaveDialog) then begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options - [ofDontAddToRecent];
    end else
    if (dlg is TOpenDialog) then begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options - [ofDontAddToRecent];
    end;
  end;
end;

procedure FreeOTFEGUISetupOpenSaveDialog(fe: TSDUFilenameEdit);
begin
  FreeOTFEGUISetupOpenSaveDialog(fe.OpenDialog);
  FreeOTFEGUISetupOpenSaveDialog(fe.SaveDialog);
end;

constructor TCommonSettings.Create();
begin
  inherited;

  CustomLocation := '';
  OptMRUList     := TSDUMRUList.Create();

end;

destructor TCommonSettings.Destroy();
begin
  OptMRUList.Free();

  inherited;
end;

procedure TCommonSettings.Load();
var
  iniFile: TCustomINIFile;
begin
  OptSaveSettings := IdentifyWhereSettingsStored();
  iniFile         := GetSettingsObj();
  try
    _Load(iniFile);
  finally
    iniFile.Free();
  end;

end;

procedure TCommonSettings._Load(iniFile: TCustomINIFile);
var
  useDefaultDriveLetter: DriveLetterString;
begin
  OptSettingsVersion := iniFile.ReadInteger(SECTION_GENERAL, OPT_SETTINGSVERSION, SETTINGS_V1);

  OptExploreAfterMount        := iniFile.ReadBool(SECTION_GENERAL,
    OPT_EXPLOREAFTERMOUNT, DFLT_OPT_EXPLOREAFTERMOUNT);
  OptAdvancedMountDlg         := iniFile.ReadBool(SECTION_GENERAL, OPT_ADVANCEDMOUNTDLG,
    DFLT_OPT_ADVANCEDMOUNTDLG);
  OptRevertVolTimestamps      := iniFile.ReadBool(SECTION_GENERAL,
    OPT_REVERTVOLTIMESTAMPS, DFLT_OPT_REVERTVOLTIMESTAMPS);
  OptShowPasswords            := iniFile.ReadBool(SECTION_GENERAL, OPT_SHOWPASSWORDS,
    DFLT_OPT_SHOWPASSWORDS);
  OptAllowNewlinesInPasswords := iniFile.ReadBool(SECTION_GENERAL,
    OPT_ALLOWNEWLINESINPASSWORDS, DFLT_OPT_ALLOWNEWLINESINPASSWORDS);
  OptAllowTabsInPasswords     := iniFile.ReadBool(SECTION_GENERAL,
    OPT_ALLOWTABSINPASSWORDS, DFLT_OPT_ALLOWTABSINPASSWORDS);
  OptLanguageCode             := iniFile.ReadString(SECTION_GENERAL, OPT_LANGUAGECODE,
    DFLT_OPT_LANGUAGECODE);
  OptDragDropFileType         :=
    TDragDropFileType(iniFile.ReadInteger(SECTION_GENERAL, OPT_DRAGDROP, DFLT_OPT_DRAGDROP));
  useDefaultDriveLetter       :=
    DriveLetterString(iniFile.ReadString(SECTION_GENERAL, OPT_DEFAULTDRIVELETTER,
    DFLT_OPT_DEFAULTDRIVELETTER));
  // #0 written as "#"
  OptDefaultDriveLetter       := useDefaultDriveLetter[1];
  if (OptDefaultDriveLetter = '#') then begin
    OptDefaultDriveLetter := #0;
  end;

  OptPromptMountSuccessful := iniFile.ReadBool(SECTION_CONFIRMATION,
    OPT_PROMPTMOUNTSUCCESSFUL, DFLT_OPT_PROMPTMOUNTSUCCESSFUL);

  OptUpdateChkFrequency              :=
    TUpdateFrequency(iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_FREQ,
    Ord(DFLT_OPT_CHKUPDATE_FREQ)));
  OptUpdateChkLastChecked            :=
    SDUISO8601ToTDate(iniFile.ReadString(SECTION_CHKUPDATE, OPT_CHKUPDATE_LASTCHECKED,
    DFLT_OPT_CHKUPDATE_LASTCHECKED));
  OptUpdateChkSuppressNotifyVerMajor :=
    iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR,
    DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR);
  OptUpdateChkSuppressNotifyVerMinor :=
    iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR,
    DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR);

  OptPKCS11Enable          := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11ENABLE,
    DFLT_OPT_PKCS11ENABLE);
  OptPKCS11Library         := iniFile.ReadString(SECTION_PKCS11, OPT_PKCS11LIBRARY,
    DFLT_OPT_PKCS11LIBRARY);
  OptPKCS11AutoMount       := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11AUTOMOUNT,
    DFLT_OPT_PKCS11AUTOMOUNT);
  OptPKCS11AutoMountVolume := iniFile.ReadString(SECTION_PKCS11,
    OPT_PKCS11AUTOMOUNTVOLUME, DFLT_OPT_PKCS11AUTOMOUNTVOLUME);
  OptPKCS11AutoDismount    := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11AUTODISMOUNT,
    DFLT_OPT_PKCS11AUTODISMOUNT);

  OptMRUList.MaxItems := DFLT_OPT_MRUMAXITEMS;
  OptMRUList.Load(iniFile, SECTION_MRULIST);

  OptPostMountExe    := iniFile.ReadString(SECTION_AUTORUN, OPT_POSTMOUNTEXE,
    DFLT_OPT_POSTMOUNTEXE);
  OptPreDismountExe  := iniFile.ReadString(SECTION_AUTORUN, OPT_PREDISMOUNTEXE,
    DFLT_OPT_PREDISMOUNTEXE);
  OptPostDismountExe := iniFile.ReadString(SECTION_AUTORUN, OPT_POSTDISMOUNTEXE,
    DFLT_OPT_POSTDISMOUNTEXE);
  OptPrePostExeWarn  := iniFile.ReadBool(SECTION_AUTORUN, OPT_PREPOSTEXEWARN,
    DFLT_OPT_PREPOSTEXEWARN);

end;

function TCommonSettings.Save(ShowWarningIfRO: Boolean ): Boolean;
var
  iniFile: TCustomINIFile;
begin
  Result := False;

  if (OptSaveSettings = slNone) then begin
    Result := True;
  end else begin
    iniFile := GetSettingsObj();
    if (iniFile <> nil) then begin
      try
        Result := _Save(iniFile);

      finally
        iniFile.Free();
      end;
    end;
  end;

  if (not Result) and ShowWarningIfRO then begin

    SDUMessageDlg(
      _('Your settings could not be saved.') + SDUCRLF + SDUCRLF + Format(
      _('Please ensure that you have suitable access rights in order to write to:' +
      SDUCRLF + SDUCRLF + '%s'), [PrettyPrintSettingsFile(OptSaveSettings)]),
      mtError
      );
  end;

end;

function TCommonSettings._Save(iniFile: TCustomINIFile): Boolean;
var
  useDefaultDriveLetter: DriveLetterChar;
begin
  Result := True;

  try
    iniFile.WriteInteger(SECTION_GENERAL, OPT_SETTINGSVERSION, SETTINGS_V2);

    iniFile.WriteBool(SECTION_GENERAL, OPT_EXPLOREAFTERMOUNT, OptExploreAfterMount);
    iniFile.WriteBool(SECTION_GENERAL, OPT_ADVANCEDMOUNTDLG, OptAdvancedMountDlg);
    iniFile.WriteBool(SECTION_GENERAL, OPT_REVERTVOLTIMESTAMPS,
      OptRevertVolTimestamps);
    iniFile.WriteBool(SECTION_GENERAL, OPT_SHOWPASSWORDS, OptShowPasswords);
    iniFile.WriteBool(SECTION_GENERAL, OPT_ALLOWNEWLINESINPASSWORDS,
      OptAllowNewlinesInPasswords);
    iniFile.WriteBool(SECTION_GENERAL, OPT_ALLOWTABSINPASSWORDS,
      OptAllowTabsInPasswords);
    iniFile.WriteString(SECTION_GENERAL, OPT_LANGUAGECODE, OptLanguageCode);
    iniFile.WriteInteger(SECTION_GENERAL, OPT_DRAGDROP,
      Ord(OptDragDropFileType));
    // #0 written as "#"
    useDefaultDriveLetter := OptDefaultDriveLetter;
    if (OptDefaultDriveLetter = #0) then begin
      useDefaultDriveLetter := '#';
    end;
    iniFile.WriteString(SECTION_GENERAL, OPT_DEFAULTDRIVELETTER,
      String(useDefaultDriveLetter));

    iniFile.WriteBool(SECTION_CONFIRMATION, OPT_PROMPTMOUNTSUCCESSFUL,
      OptPromptMountSuccessful);

    iniFile.WriteInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_FREQ,
      Ord(OptUpdateChkFrequency));
    iniFile.WriteString(SECTION_CHKUPDATE, OPT_CHKUPDATE_LASTCHECKED,
      SDUTDateToISO8601(OptUpdateChkLastChecked));
    iniFile.WriteInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR,
      OptUpdateChkSuppressNotifyVerMajor);
    iniFile.WriteInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR,
      OptUpdateChkSuppressNotifyVerMinor);

    iniFile.WriteBool(SECTION_PKCS11, OPT_PKCS11ENABLE, OptPKCS11Enable);
    iniFile.WriteString(SECTION_PKCS11, OPT_PKCS11LIBRARY, OptPKCS11Library);
    iniFile.WriteBool(SECTION_PKCS11, OPT_PKCS11AUTOMOUNT, OptPKCS11AutoMount);
    iniFile.WriteString(SECTION_PKCS11, OPT_PKCS11AUTOMOUNTVOLUME,
      OptPKCS11AutoMountVolume);
    iniFile.WriteBool(SECTION_PKCS11, OPT_PKCS11AUTODISMOUNT,
      OptPKCS11AutoDismount);

    OptMRUList.Save(iniFile, SECTION_MRULIST);

    iniFile.WriteString(SECTION_AUTORUN, OPT_POSTMOUNTEXE, OptPostMountExe);
    iniFile.WriteString(SECTION_AUTORUN, OPT_PREDISMOUNTEXE, OptPreDismountExe);
    iniFile.WriteString(SECTION_AUTORUN, OPT_POSTDISMOUNTEXE, OptPostDismountExe);
    iniFile.WriteBool(SECTION_AUTORUN, OPT_PREPOSTEXEWARN, OptPrePostExeWarn);

  except
    on E: Exception do begin
      Result := False;
    end;
  end;

end;

function TCommonSettings.PrettyPrintSettingsFile(loc: TSettingsSaveLocation): String;
begin
  Result := '';

  case loc of
    slNone:
    begin
      // Do nothing
    end;

    slExeDir,
    slProfile,
    slCustom:
    begin
      Result := GetSettingsFilename(loc);
    end;

    slRegistry:
    begin
      Result := RegistryKey();
    end;

    else
    begin
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);
    end;

  end;

end;


function TCommonSettings.DestroySettingsFile(loc: TSettingsSaveLocation): Boolean;
var
  filename: String;
  registry: TRegistry;
begin
  Result := True;

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
      if (filename <> '') then begin
        Result := DeleteFile(filename);
      end;
    end;

    slRegistry:
    begin
      registry := TRegistry.Create();
      try
        registry.RootKey := HKEY_CURRENT_USER;
        Result           := registry.DeleteKey(RegistryKey());
      finally
        registry.Free();
      end;
    end;

    else
    begin
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);
    end;

  end;

end;

function TCommonSettings.GetSettingsFilename(loc: TSettingsSaveLocation): String;
var
  iniFilenameOnly: String;
  filenameAndPath: String;
begin
  filenameAndPath := '';

  iniFilenameOnly := ChangeFileExt(ExtractFilename(ParamStr(0)), '.ini');

  case loc of
    slNone,
    slRegistry:
      // Do nothing; already set to empty string
      ;

    slExeDir:
      filenameAndPath := ExtractFilePath(ParamStr(0)) + iniFilenameOnly;

    slProfile:
      filenameAndPath := SDUGetSpecialFolderPath(CSIDL_APPDATA) + '\' + iniFilenameOnly;


    slCustom:
      filenameAndPath := CustomLocation;

    else
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);

  end;

  Result := filenameAndPath;
end;

// Identify where user settings are stored
function TCommonSettings.IdentifyWhereSettingsStored(): TSettingsSaveLocation;
var
  sl:            TSettingsSaveLocation;
  filename:      String;
  checkLocation: TSettingsSaveLocation;
  registry:      TRegistry;
begin
  Result := slNone;

  if (CustomLocation <> '') then begin
    // If a custom location has been specified, we use it even if the file
    // doesn't exist
    Result := slCustom;
  end else begin
    for sl := low(SettingsSaveLocationSearchOrder) to high(SettingsSaveLocationSearchOrder) do
    begin
      checkLocation := SettingsSaveLocationSearchOrder[sl];

      if ((checkLocation = slExeDir) or (checkLocation = slProfile) or
        (checkLocation = slCustom)) then begin
        filename := GetSettingsFilename(checkLocation);
        if ((filename <> '') and FileExists(filename)) then begin
          Result := checkLocation;
          break;
        end;
      end else
      if (checkLocation = slRegistry) then begin
        // Test if registry key exists
        registry := TRegistry.Create();
        try
          registry.RootKey := HKEY_CURRENT_USER;
          if registry.OpenKeyReadOnly(RegistryKey()) then begin
            Result := checkLocation;
            break;
          end;
        finally
          registry.Free();
        end;

      end;

    end;
  end;

end;

function TCommonSettings.IsRememberedDlgAnswer(msg: String;
  out answer: Integer): Boolean;
begin
  //todo: return remembered answer
  result:= false;
  answer := 0;
end;

procedure TCommonSettings.Assign(copyFrom: TCommonSettings);
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
function TCommonSettings.GetSettingsObj(): TCustomIniFile;
var
  filename: String;
begin
  Result := nil;

  if (OptSaveSettings = slNone) then begin
    // Create "ini file" with blank filename; all calls to read from this will
    // fail; resulting in the default values being returned/used
    Result := TINIFile.Create('');
  end else
  if ((OptSaveSettings = slExeDir) or (OptSaveSettings = slProfile) or
    (OptSaveSettings = slCustom)) then begin
      filename := GetSettingsFilename(OptSaveSettings);
      try
        Result := TINIFile.Create(filename);
      except
        on E: Exception do
          // Problem - e.g. can't create .INI file
          Result := nil;
      end;

  end else
    if (OptSaveSettings = slRegistry) then
        Result := TRegistryINIFile.Create(RegistryKey());


end;

end.
