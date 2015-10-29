unit CommonSettings;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
{
to add a setting:
  add a property with a default. will automatically be read/saved from file
  if cant add a default (eg string) then add to _SetDefaults fn.

settings need to be properties and published in order to be picked up by rtti and saved to ini file
}


interface

uses
  Classes, Dialogs,// Required for TShortCut
  vcl.Forms,
  Controls,  // Required for TDate
  INIFiles,
  //sdu
  lcTypes,
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
  //type to mount a file as if it has no header
  //  TDragDropFileType = (ftPrompt, ftFreeOTFE, ftPlainLinux);

  //type of volume - but can also be hidden if 1st two
  TVolumeType    = (vtUnknown, vtFreeOTFE, vtPlainLinux, vtLUKS);
  TVolumeTypeSet = set of TVolumeType;

const
  //these cant be detected
  sDragDropFileType: TVolumeTypeSet = [vtUnknown, vtFreeOTFE, vtPlainLinux];


resourcestring
  DRAGDROPFILETYPE_PROMPT   = '<prompt user>';
  DRAGDROPFILETYPE_FREEOTFE = 'FreeOTFE';
  DRAGDROPFILETYPE_LINUX    = 'dm-crypt';
  DRAGDROPFILETYPE_LUKS     = 'LUKS';

const
  DRAG_DROP_FILETYPE_TITLE_PTR: array [TVolumeType] of Pointer =
    (@DRAGDROPFILETYPE_PROMPT, @DRAGDROPFILETYPE_FREEOTFE, @DRAGDROPFILETYPE_LINUX,
    @DRAGDROPFILETYPE_LUKS
    );

type
  TSettingsSaveLocation = (slNone, slExeDir, slProfile, slRegistry, slCustom);

resourcestring
  SAVELOCATION_DO_NOT_SAVE  = '<Do not save settings>';
  SAVELOCATION_EXE_DIR      = 'File in %s directory';
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

  // is app running? used to detect crashes
  eAppRunning = (arDontKnow{no settings file}, arRunning{set in InitApp},
    arClosed{set in Form.close});

  { done -otdk -crefactor : use rtti to read and write -much easier, just define properties + defaults }


  TCommonSettings = class
  private
    //    fCustomLocation:  String;
    fPostMountExe: String;
    fisAppRunning:    eAppRunning;
    flanguageCode: String;
    fshowPasswords:       Boolean;
    fshowAdvancedMountDialog: Boolean;
    finformIfMountedOK:   Boolean;
    fsettingsVersion:     Integer;
    fPostDismountExe:      String;
    fallowTabsInPasswords: Boolean;
    ffreezeVolTimestamps:     Boolean;
    fdefaultDriveChar:        DriveLetterChar;
    fdefaultVolType:          TVolumeType;
    fmountPathOnPKCS11: String;
    fPreDismountExe: String;
    fshowStatusBar:     Boolean;
    fwarnAutoRunExeErr:  Boolean;
    fPKCS11AutoMount: Boolean;
    fenablePKCS11: Boolean;
    fPKCS11Library:         String;
    fallowNewlinesInPasswords: Boolean;
    fexploreAfterMount:     Boolean;
    fdismountOnPKCS11Remove:   Boolean;
    fMruList:          TSDUMRUList;
    fSavedDlgs:        TStringList;
    fMainWindowLayout: String;
    fstoreLayout:   Boolean;
    fupdateChkDontNotifyMinorVer: Integer;
    fupdateChkDontNotifyMajorVer: Integer;

    procedure _LoadNew(iniFile: TCustomINIFile);
    function _UpdateNew(iniFile: TCustomINIFile; saving: Boolean): Boolean;

  protected
    fshowToolbarLarge:     Boolean;
    fshowToolbar:          Boolean;
    FshowToolbarCaptions:  Boolean;
    fUpdateChkFreq:        TUpdateFrequency;
    flastCheckedForUpdate: TDate;

    function _IdentifyWhereSettingsStored(): TSettingsSaveLocation;
    procedure _LoadOld(iniFile: TCustomINIFile); virtual;
    procedure _Load(iniFile: TCustomINIFile);
    function _Save(iniFile: TCustomINIFile): Boolean; virtual;
    function _CreateSettingsFile(): TCustomIniFile;
    //set properties defaults that do not have a 'default' value (typ. strings and classes)
    procedure _SetDefaults; virtual;
  public

    // do NOT call directly
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Load(); virtual;
    function Save(ShowWarningIfRO: Boolean = True): Boolean; virtual;

    procedure Assign(copyFrom: TCommonSettings);

    function DestroySettingsFile(loc: TSettingsSaveLocation): Boolean;
    function GetSettingsFilename(loc: TSettingsSaveLocation): String;

    function PrettyPrintSettingsFile(loc: TSettingsSaveLocation): String;

    function RegistryKey(): String; virtual; abstract;

    function IsRememberedDlgAnswer(msg: String; out answer: Integer): Boolean;


    //set layout to default
    procedure ClearLayout; virtual;

    // MRU list...
    property MruList: TSDUMRUList Read fMruList Write fMruList;
  published

     (*
     property names are stored as keys in ini file where users can change so keep friendly and explanatory
     *)

    // version on disc
    property SettingsVersion: Integer Read fsettingsVersion Write fsettingsVersion;

    // General...
    //whether to store the layout on saving - is always loaded
    property StoreLayout: Boolean
      Read fstoreLayout Write fstoreLayout default True;

    property MainWindowLayout: String Read fMainWindowLayout
      Write fMainWindowLayout;//default '';

    property ShowToolbar: Boolean Read fshowToolbar
      Write fshowToolbar default True;
    property ShowToolbarCaptions: Boolean Read FshowToolbarCaptions
      Write FshowToolbarCaptions default True;

    property ExploreAfterMount: Boolean Read fexploreAfterMount
      Write fexploreAfterMount default False;
    property ShowAdvancedMountDialog: Boolean Read fshowAdvancedMountDialog
      Write fshowAdvancedMountDialog default False;
    property FreezeVolTimestamps: Boolean Read ffreezeVolTimestamps
      Write ffreezeVolTimestamps default True;
    property ShowPasswords: Boolean Read fshowPasswords
      Write fshowPasswords default False;
    property AllowNewlinesInPasswords: Boolean
      Read fallowNewlinesInPasswords Write fallowNewlinesInPasswords default True;
    property AllowTabsInPasswords: Boolean Read fallowTabsInPasswords
      Write fallowTabsInPasswords default False;
    property LanguageCode: String Read flanguageCode Write flanguageCode;
    // default '';
    // default type if cant be detected e.g. if file dropped on - is vtUnknown( ie prompt), vtFreeOTFE, vtPlainLinux
    property DefaultVolType: TVolumeType Read fdefaultVolType
      Write fdefaultVolType default vtUnknown;
    property DefaultDriveChar: DriveLetterChar
      Read fdefaultDriveChar Write fdefaultDriveChar default '#';
    property IsAppRunning: eAppRunning
      Read fisAppRunning Write fisAppRunning default arDontKnow;
    property ShowLargeToolbar: Boolean Read fshowToolbarLarge
      Write fshowToolbarLarge default True;
    property ShowStatusbar: Boolean Read fshowStatusBar
      Write fshowStatusBar default True;

    // Prompts and messages
    property InformIfMountedOK: Boolean Read finformIfMountedOK
      Write finformIfMountedOK default True;
    // If Set, display an info msg after successful mount

    // Check for updates config...
    property UpdateChkFreq: TUpdateFrequency
      Read fUpdateChkFreq Write fUpdateChkFreq default ufRandom;

    property LastCheckedForUpdate: TDate Read flastCheckedForUpdate
      Write flastCheckedForUpdate;//default to build date;

    property UpdateChkDontNotifyMajorVer: Integer
      Read fupdateChkDontNotifyMajorVer Write fupdateChkDontNotifyMajorVer default 0;
    property UpdateChkDontNotifyMinorVer: Integer
      Read fupdateChkDontNotifyMinorVer Write fupdateChkDontNotifyMinorVer default 0;

    // PKCS11...
    property EnablePKCS11: Boolean Read fenablePKCS11 Write fenablePKCS11 default False;
    property PKCS11LibraryPath: String Read fPKCS11Library Write fPKCS11Library;//default '';
    property PKCS11AutoMount: Boolean Read fPKCS11AutoMount
      Write fPKCS11AutoMount default False;
    property MountPathOnPKCS11: String Read fmountPathOnPKCS11
      Write fmountPathOnPKCS11;//default '';
    property DismountOnPKCS11Remove: Boolean Read fdismountOnPKCS11Remove
      Write fdismountOnPKCS11Remove default False;

    // Autorun...
    property PostMountExe: String Read fPostMountExe Write fPostMountExe;//default '';
    property PreDismountExe: String Read fPreDismountExe Write fPreDismountExe;
    // default '';
    property PostDismountExe: String Read fPostDismountExe Write fPostDismountExe;
    // default '';
    property WarnAutoRunExeErr: Boolean Read fwarnAutoRunExeErr
      Write fwarnAutoRunExeErr default True;

  end;

    {
TCommonSettings is a singleton class, only ever one instance - this is enforced by assertion in ctor
set up instance by calling SetSettingsType then get instance by calling GetSettings
}
  TSettingsClass = class of TCommonSettings;

{ factory fn creates an instance that is returned by GetSettings}
procedure SetSettingsType(typ: TSettingsClass);

{returns an instance of type set in SetSettingsType}
function GetSettings: TCommonSettings;

// allow to free early so can write and catch exceptions, but if not called will be freed in finalisation
// DO NOT do GetSettings.free
procedure FreeSettings;


 // some functions related to settings
 { TODO 1 -otdk -crefactor : move into separate unit }
function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): String;
function DragDropFileTypeTitle(dragDropfileType: TVolumeType): String;

 // Turn on/off option on open/save dialogs to add file selected under Start
 // menu's "Documents", depending on whether MRU list is enabled or not
procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog); overload;

// Get string representation of form's layout
function SDUGetFormLayout(form: TForm): String;
 // Set form's layout based on string representation of it
 // !! IMPORTANT !!
 // If there's a window layout stored, set ".Position" to poDefault before
 // calling this - otherwise it messes up the window if it was stored as
 // maximised.
 // Specifically, it shows the main window with maximised dimensions, with
 // the "Maximise" button in the top-right ready to "Normalise"
 // (non-maximise) the window, but WITH THE WINDOW SHOWN ABOUT 50 PIXELS
 // DOWN!)
procedure SDUSetFormLayout(form: TForm; layout: String);



var
  GCustomSettingsLocation: String;
  // this isnt a property because it is not saved in the file - its the location of file
  GSettingsSaveLocation:   TSettingsSaveLocation;//ditto - not saved to file

const
  SECTION_LAYOUT = 'Layout'; // used in explorer

implementation

uses
  Windows,   // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Menus, Registry, rtti, typinfo, strutils,
           // Required for ShortCutToText and TextToShortCut
  ShlObj,  // Required for CSIDL_PERSONAL
           //sdu
  SDUGeneral, lcCommandLine,
  lcDialogs, SDUi18n, lcConsts;

var
  //single instance of object, get by calling GetSettings. normally is of derived class
  _SettingsObj: TCommonSettings;

const
//  SETTINGS_V1 = 1;
//  SETTINGS_V2 = 2;

  CURR_SETTINGS_VER = 3;

  { TODO -otdk -cpending : delete all these once only new format used }
  // -- General section --
  OPT_DISPLAYTOOLBAR      = 'DisplayToolbar';
  DFLT_OPT_DISPLAYTOOLBAR = True;

  OPT_DISPLAYTOOLBARCAPTIONS      = 'DisplayToolbarCaptions';
  DFLT_OPT_DISPLAYTOOLBARCAPTIONS = True;

  OPT_SETTINGSVERSION           = 'SettingsVersion';
//  DFLT_OPT_SETTINGSVERSION      = SETTINGS_V2;
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
  DFLT_OPT_DRAGDROP             = Ord(vtFreeOTFE);
  OPT_ALLOWNEWLINESINPASSWORDS  = 'AllowNewlinesInPasswords';
  DFLT_OPT_ALLOWNEWLINESINPASSWORDS = True;
  OPT_ALLOWTABSINPASSWORDS      = 'AllowTabsInPasswords';
  DFLT_OPT_ALLOWTABSINPASSWORDS = False;
  OPT_DEFAULTDRIVELETTER        = 'DefaultDriveLetter';
  DFLT_OPT_DEFAULTDRIVELETTER   = '#';
  OPT_APPRUNNING                = 'AppRunning';
  DFLT_OPT_APPRUNNING           = arDontKnow;


  OPT_STORELAYOUT           = 'StoreLayout';
  DFLT_OPT_STORELAYOUT      = True;
  OPT_MAINWINDOWLAYOUT      = 'MainWindowLayout';
  DFLT_OPT_MAINWINDOWLAYOUT = '';

  OPT_DISPLAYTOOLBARLARGE      = 'DisplayToolbarLarge';
  DFLT_OPT_DISPLAYTOOLBARLARGE = True;

  OPT_DISPLAYSTATUSBAR      = 'DisplayStatusbar';
  DFLT_OPT_DISPLAYSTATUSBAR = True;

  // -- Prompts and messages --
  OPT_PROMPTMOUNTSUCCESSFUL      = 'OptPromptMountSuccessful';
  DFLT_OPT_PROMPTMOUNTSUCCESSFUL = True;

  // -- Check for updates section --
  SECTION_CHKUPDATE                    = 'Updates';
  OPT_CHKUPDATE_FREQ                   = 'CheckFrequency';
  DFLT_OPT_CHKUPDATE_FREQ              = ufRandom;
  OPT_CHKUPDATE_LASTCHECKED            = 'LastChecked';
  //this should be release date and updated with each release
  DFLT_OPT_CHKUPDATE_LASTCHECKED       = '20151001';
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
  SECTION_MRULIST = 'MRUList';

  SECTION_DLGS         = 'DIALOGS';
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

  WINDOW_LAYOUT_STATE  = 'STATE';
  WINDOW_LAYOUT_TOP    = 'TOP';
  WINDOW_LAYOUT_LEFT   = 'LEFT';
  WINDOW_LAYOUT_HEIGHT = 'HEIGHT';
  WINDOW_LAYOUT_WIDTH  = 'WIDTH';

 // type OnHideMsg
 // hide message box based on user settings?
function DoHideMsg(msg: String; out answer: Integer): Boolean;
begin
  Result := GetSettings().IsRememberedDlgAnswer(msg, answer);
end;

function UpdateFrequencyTitle(updateFrequency: TUpdateFrequency): String;
begin
  Result := LoadResString(UpdateFrequencyTitlePtr[updateFrequency]);
end;

function DragDropFileTypeTitle(dragDropfileType: TVolumeType): String;
begin
  Result := LoadResString(DRAG_DROP_FILETYPE_TITLE_PTR[dragDropfileType]);
end;


procedure FreeOTFEGUISetupOpenSaveDialog(dlg: TCommonDialog);
begin
  // Sanity check/short circuit
  if (dlg = nil) then begin
    exit;
  end;

  // Sanity check
  if (_SettingsObj = nil) then begin
    exit;
  end;

  if (Getsettings().fMruList.MaxItems <= 0) then begin
    if (dlg is TSaveDialog) then begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options + [ofDontAddToRecent];
    end else
    if (dlg is TOpenDialog) then begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options + [ofDontAddToRecent];
    end;
    //prevent form opening at last directory
    { TODO 2 -otdk -csecurity : windows still saves this dir. set guid? https://msdn.microsoft.com/en-us/library/windows/desktop/bb776913%28v=vs.85%29.aspx#state_persistence }
    //    TOpenDialog(dlg).InitialDir:=    ExtractFilePath(Application.ExeName);
  end else begin
    if (dlg is TSaveDialog) then begin
      TSaveDialog(dlg).Options := TSaveDialog(dlg).Options - [ofDontAddToRecent];
    end else
    if (dlg is TOpenDialog) then begin
      TOpenDialog(dlg).Options := TOpenDialog(dlg).Options - [ofDontAddToRecent];
    end;
  end;
end;


procedure TCommonSettings.ClearLayout;
begin
  fMainWindowLayout    := '';
  fShowStatusBar       := True;
  fshowToolbar         := True;
  fShowToolbarLarge    := True;
  FshowToolbarCaptions := True;
end;

constructor TCommonSettings.Create();
var
  settingsFilename: String;
begin
  inherited;

  assert(_SettingsObj = nil, 'Do not call TCommonSettings.Create directly - only call GetSettings');


  fMruList   := TSDUMRUList.Create();
  fSavedDlgs := TStringList.Create;

  //testing//
//  fSavedDlgs.Add('foo=bar');
//  fSavedDlgs.Add('splock=splunge');

  settingsFilename := GetCmdLine.settingsFileArg;
  if settingsFilename <> '' then
    GCustomSettingsLocation := SDURelativePathToAbsolute(settingsFilename);

  //if used before read
  _SetDefaults();
  Load();

  //set up to suppress messages if user chose
  lcDialogs.GOnHideMsg := DoHideMsg;
end;

destructor TCommonSettings.Destroy();
begin
  fMruList.Free();
  fSavedDlgs.Free();
  inherited;
end;

procedure TCommonSettings.Load();
var
  iniFile: TCustomINIFile;
begin
  GSettingsSaveLocation := _IdentifyWhereSettingsStored();
  iniFile               := _CreateSettingsFile();
  try
    _Load(iniFile);
  finally
    iniFile.Free();
  end;

end;


procedure TCommonSettings._LoadOld(iniFile: TCustomINIFile);
var
  useDefaultDriveLetter: DriveLetterString;
begin

  // Layout section...
  fstoreLayout :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_STORELAYOUT, DFLT_OPT_STORELAYOUT);

  fMainWindowLayout :=
    iniFile.ReadString(SECTION_LAYOUT, OPT_MAINWINDOWLAYOUT, DFLT_OPT_MAINWINDOWLAYOUT);

  // done -otdk : combine load and save, and/or use rtti
  fshowToolbar := iniFile.ReadBool(SECTION_GENERAL, OPT_DISPLAYTOOLBAR,
    DFLT_OPT_DISPLAYTOOLBAR);

  FshowToolbarCaptions := iniFile.ReadBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARCAPTIONS,
    DFLT_OPT_DISPLAYTOOLBARCAPTIONS);

//  fsettingsVersion := iniFile.ReadInteger(SECTION_GENERAL, OPT_SETTINGSVERSION, SETTINGS_V1);

  fexploreAfterMount     :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_EXPLOREAFTERMOUNT, DFLT_OPT_EXPLOREAFTERMOUNT);
  fshowAdvancedMountDialog      :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_ADVANCEDMOUNTDLG, DFLT_OPT_ADVANCEDMOUNTDLG);
  ffreezeVolTimestamps      :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_REVERTVOLTIMESTAMPS, DFLT_OPT_REVERTVOLTIMESTAMPS);
  fshowPasswords            :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_SHOWPASSWORDS, DFLT_OPT_SHOWPASSWORDS);
  fallowNewlinesInPasswords :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_ALLOWNEWLINESINPASSWORDS,
    DFLT_OPT_ALLOWNEWLINESINPASSWORDS);
  fallowTabsInPasswords  :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_ALLOWTABSINPASSWORDS, DFLT_OPT_ALLOWTABSINPASSWORDS);
  flanguageCode          :=
    iniFile.ReadString(SECTION_GENERAL, OPT_LANGUAGECODE, DFLT_OPT_LANGUAGECODE);
  fdefaultVolType           :=
    TVolumeType(iniFile.ReadInteger(SECTION_GENERAL, OPT_DRAGDROP, DFLT_OPT_DRAGDROP));
  useDefaultDriveLetter     :=
    DriveLetterString(iniFile.ReadString(SECTION_GENERAL, OPT_DEFAULTDRIVELETTER,
    DFLT_OPT_DEFAULTDRIVELETTER));
  // #0 written as "#"
  fdefaultDriveChar         := useDefaultDriveLetter[1];
  if (fdefaultDriveChar = '#') then begin
    fdefaultDriveChar := #0;
  end;

  fisAppRunning     :=
    eAppRunning(iniFile.ReadInteger(SECTION_GENERAL, OPT_APPRUNNING, Ord(DFLT_OPT_APPRUNNING)));
  fshowToolbarLarge := iniFile.ReadBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARLARGE,
    DFLT_OPT_DISPLAYTOOLBARLARGE);

  InformIfMountedOK := iniFile.ReadBool(SECTION_CONFIRMATION, OPT_PROMPTMOUNTSUCCESSFUL,
    DFLT_OPT_PROMPTMOUNTSUCCESSFUL);

  fshowStatusBar :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_DISPLAYSTATUSBAR, DFLT_OPT_DISPLAYSTATUSBAR);

  fUpdateChkFreq               :=
    TUpdateFrequency(iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_FREQ,
    Ord(DFLT_OPT_CHKUPDATE_FREQ)));
  fLastCheckedForUpdate        :=
    SDUISO8601ToTDate(iniFile.ReadString(SECTION_CHKUPDATE, OPT_CHKUPDATE_LASTCHECKED,
    DFLT_OPT_CHKUPDATE_LASTCHECKED));
  fUpdateChkDontNotifyMajorVer :=
    iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR,
    DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMAJOR);
  fUpdateChkDontNotifyMinorVer :=
    iniFile.ReadInteger(SECTION_CHKUPDATE, OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR,
    DFLT_OPT_CHKUPDATE_SUPPRESSNOTIFYVERMINOR);

  EnablePKCS11           := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11ENABLE,
    DFLT_OPT_PKCS11ENABLE);
  PKCS11LibraryPath      := iniFile.ReadString(SECTION_PKCS11, OPT_PKCS11LIBRARY,
    DFLT_OPT_PKCS11LIBRARY);
  fPKCS11AutoMount        := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11AUTOMOUNT,
    DFLT_OPT_PKCS11AUTOMOUNT);
  fMountPathOnPKCS11     := iniFile.ReadString(SECTION_PKCS11, OPT_PKCS11AUTOMOUNTVOLUME,
    DFLT_OPT_PKCS11AUTOMOUNTVOLUME);
  fDismountOnPKCS11Remove := iniFile.ReadBool(SECTION_PKCS11, OPT_PKCS11AUTODISMOUNT,
    DFLT_OPT_PKCS11AUTODISMOUNT);

  fMruList.MaxItems := DFLT_OPT_MRUMAXITEMS;
  fMruList.Load(iniFile, SECTION_MRULIST);

  PostMountExe      := iniFile.ReadString(SECTION_AUTORUN, OPT_POSTMOUNTEXE,
    DFLT_OPT_POSTMOUNTEXE);
  PreDismountExe    := iniFile.ReadString(SECTION_AUTORUN, OPT_PREDISMOUNTEXE,
    DFLT_OPT_PREDISMOUNTEXE);
  PostDismountExe   := iniFile.ReadString(SECTION_AUTORUN, OPT_POSTDISMOUNTEXE,
    DFLT_OPT_POSTDISMOUNTEXE);
  fWarnAutoRunExeErr := iniFile.ReadBool(SECTION_AUTORUN, OPT_PREPOSTEXEWARN,
    DFLT_OPT_PREPOSTEXEWARN);

end;


procedure TCommonSettings._Load(iniFile: TCustomINIFile);
begin
  //   fread_vers := iniFile.ReadInteger(SECTION_GENERAL, 'INI_VERSION',-1);
  fSettingsVersion := iniFile.ReadInteger(SECTION_GENERAL, OPT_SETTINGSVERSION, CURR_SETTINGS_VER);

  //if doesnt exist there is no error calling old
  if fSettingsVersion < 3 then
    _LoadOld(iniFile)
  else
    _LoadNew(iniFile);
end;

procedure TCommonSettings._LoadNew(iniFile: TCustomINIFile);
begin
  _SetDefaults();
  _UpdateNew(iniFile, False);
end;

// see http://stackoverflow.com/questions/10188459/how-to-loop-all-properties-in-a-class
function TCommonSettings._UpdateNew(iniFile: TCustomINIFile; saving: Boolean): Boolean;
var
  ctx:      TRttiContext;
  rt:       TRttiType;
  prop:     TRttiProperty;
  Value:    TValue;
  sval:     String;
  Name, nm: String;
  i:        Integer;

  function UpdateString(cur_val: String): String;
  begin
    if saving then begin
      //save as string for clarity
      iniFile.WriteString(SECTION_GENERAL, Name, cur_val);
      Result := cur_val;
    end else begin
      Result := iniFile.ReadString(SECTION_GENERAL, Name, cur_val);
    end;

  end;

begin
  Result := True;
  try
    ctx := TRttiContext.Create();
    try
      rt := ctx.GetType(self.ClassType);

      for prop in rt.GetProperties() do begin
        Name := Prop.Name;
        // todo: remove 'opt' from property names in class
        if AnsiStartsText('Opt', Name) then
          Delete(Name, 1, 3);
        if Name <> 'MruList' then begin

          assert(prop.IsWritable);
          assert(prop.IsReadable);

          // get current (default)
          Value := prop.GetValue(self);

          case prop.PropertyType.TypeKind of
            tkEnumeration:
            begin
              //save as string for clarity
              sval := UpdateString(Value.ToString);
              if not saving then begin
                i := GetEnumValue(Value.TypeInfo, sval);
                TValue.Make(i, Value.TypeInfo, Value);
                //           iniFile.ReadInteger(SECTION_GENERAL,Prop.Name,Value.AsOrdinal);
              end;
            end;
            tkInteger:
            begin
              if prop.PropertyType.QualifiedName = 'System.Classes.TShortCut' then begin
                sval := UpdateString(ShortCutToText(Value.AsInteger));
                if not saving then
                  Value := TextToShortCut(sval);
              end else begin
                if saving then
                  iniFile.WriteInteger(SECTION_GENERAL, Name, Value.AsInteger)
                else
                  Value := iniFile.ReadInteger(SECTION_GENERAL, Name, Value.AsInteger);
              end;
            end;
            tkUString:
            begin
              Value := UpdateString(Value.AsString);
            end;
            tkWChar:
            begin
              sval := UpdateString(Value.AsString);
              if not saving then
                if length(sval) > 0 then
                  Value := sval[1];
            end;
            tkFloat:
            begin
              if prop.PropertyType.QualifiedName = 'System.TDate' then begin
                sval := UpdateString(SDUTDateToISO8601(Value.AsExtended));
                if not saving then
                  Value := SDUISO8601ToTDate(sval);
              end else begin

                if saving then
                  iniFile.WriteFloat(SECTION_GENERAL, Name, Value.AsExtended)
                else
                  Value := iniFile.ReadFloat(SECTION_GENERAL, Name, Value.AsExtended);
              end;
            end else assert(False, 'update TCommonSettings._UpdateNew');
          end;
          if not saving then
            prop.SetValue(self, Value);
        end;
      end;

    finally
      ctx.Free();
    end;

    // mru list is special case - has own save/load fns
    if saving then begin
      fMruList.Save(iniFile, SECTION_MRULIST);
    end else begin
      fMruList.MaxItems := DFLT_OPT_MRUMAXITEMS;
      fMruList.Load(iniFile, SECTION_MRULIST);
    end;

    //ditto fSavedDlgs
    if saving then begin
      for i := 0 to (fSavedDlgs.Count - 1) do begin
        nm := fSavedDlgs.Names[i];
        iniFile.WriteString(SECTION_DLGS, nm, fSavedDlgs.Values[nm]);
      end;
      //      iniFile.Write(SECTION_DLGS,fSavedDlgs)
    end else begin
      iniFile.ReadSectionValues(SECTION_DLGS, fSavedDlgs);
    end;


  except
    on E: EIniFileException do begin
      SDUMessageDlg(IfThen(saving, 'Error writing settings file: ',
        'Error reading settings file: ') + E.Message, mtError, [mbOK], 0);
      Result := False;
    end;

  end;
end;

function TCommonSettings._Save(iniFile: TCustomINIFile): Boolean;
begin
  if fSettingsVersion < 3 then begin
    // was old file loaded - clear out old values
    DestroySettingsFile(GSettingsSaveLocation);
  end;
  fSettingsVersion := CURR_SETTINGS_VER; // saving so latest version
  //  save values
  Result           := _UpdateNew(iniFile, True);
end;

//set properties defaults that do not have a 'default' value (typ. strings and classes)
procedure TCommonSettings._SetDefaults;
begin
  flastCheckedForUpdate :=
    SDUISO8601ToTDate(DFLT_OPT_CHKUPDATE_LASTCHECKED);

  flanguageCode  := DFLT_OPT_LANGUAGECODE;
  fMainWindowLayout := '';
end;

function TCommonSettings.Save(ShowWarningIfRO: Boolean): Boolean;
var
  iniFile: TCustomINIFile;
begin
  Result := False;

  if (GSettingsSaveLocation = slNone) then begin
    Result := True;
  end else begin
    iniFile := _CreateSettingsFile();
    try
      if (iniFile <> nil) then
        Result := _Save(iniFile);

    finally
      iniFile.Free();
    end;
  end;

  if (not Result) and ShowWarningIfRO then begin

    SDUMessageDlg(
      _('Your settings could not be saved.') + SDUCRLF + SDUCRLF + Format(
      _('Please ensure that you have suitable access rights in order to write to:' +
      SDUCRLF + SDUCRLF + '%s'), [PrettyPrintSettingsFile(GSettingsSaveLocation)]),
      mtError
      );
  end;

end;


(* always save new version
function TCommonSettings._Save(iniFile: TCustomINIFile): Boolean;
var
  useDefaultDriveLetter: DriveLetterChar;
begin
  Result := True;

  try
    iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBAR, OptDisplayToolbar);

    iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARCAPTIONS,
      OptDisplayToolbarCaptions);

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

    iniFile.WriteInteger(SECTION_GENERAL, OPT_APPRUNNING,
      Ord(feAppRunning));
    iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARLARGE,
      OptDisplayToolbarLarge);

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
    iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYSTATUSBAR,
      OptDisplayStatusbar);

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
*)

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
begin
  Result := '';

  iniFilenameOnly := ChangeFileExt(ExtractFilename(ParamStr(0)), '.ini');

  case loc of
    slNone,
    slRegistry:
      // Do nothing; already set to empty string
      ;

    slExeDir:
      Result := ExtractFilePath(ParamStr(0)) + iniFilenameOnly;

    slProfile:
      Result := SDUGetSpecialFolderPath(CSIDL_APPDATA) + '\' + iniFilenameOnly;


    slCustom:
      Result := GCustomSettingsLocation;

    else
      SDUMessageDlg(UNKNOWN_SETTINGS_LOCATION, mtError);

  end;

end;

// Identify where user settings are stored
function TCommonSettings._IdentifyWhereSettingsStored(): TSettingsSaveLocation;
var
  sl:            TSettingsSaveLocation;
  filename:      String;
  checkLocation: TSettingsSaveLocation;
  registry:      TRegistry;
begin
  Result := slNone;

  if (GCustomSettingsLocation <> '') then begin
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
  Result := False;
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
function TCommonSettings._CreateSettingsFile(): TCustomIniFile;
var
  filename: String;
begin
  //  Result := nil;
  case GSettingsSaveLocation of
    slNone:
    begin
      // Create "ini file" with blank filename; all calls to read from this will
      // fail; resulting in the default values being returned/used
      Result := TINIFile.Create('');
    end;
    slExeDir, slProfile, slCustom:
    begin
      filename := GetSettingsFilename(GSettingsSaveLocation);
      try
        Result := TINIFile.Create(filename);
      except
        on E: Exception do
          // Problem - e.g. can't create .INI file
          Result := nil;
      end;
    end;
    slRegistry: Result := TRegistryINIFile.Create(RegistryKey());
    else Result := nil;
  end;
end;


{ factory fn creates an instance that is returned by GetSettings}
procedure SetSettingsType(typ: TSettingsClass);
begin
  assert(_SettingsObj = nil,
    'call SetSettingsType only once, and do not call TCommonSettings.create');
  _SettingsObj := typ.Create;
end;

{returns an instance of type set in SetSettingsType}
function GetSettings: TCommonSettings;
begin
  assert(_SettingsObj <> nil, 'call SetSettingsType before GetSettings/GetSettingsBase');
  Result := _SettingsObj;
end;

// allow to freee early so can write and catch exceptions, but if not called will be freed in finalisation
// DO NOT do GetSettings.free
procedure FreeSettings;
begin
  FreeAndNil(_SettingsObj);
end;

// Get string representation of form's layout
function SDUGetFormLayout(form: TForm): String;
var
  stlLayout: TStringList;
  placement: TWindowPlacement;
begin
  // If the window's maximised, the forms top, left, width and height are set
  // to the maximised values - use this to get the normalised size and position
  placement.Length := sizeof(placement);
  GetWindowPlacement(form.Handle, @placement);

  stlLayout := TStringList.Create();
  try
    if IsIconic(form.Handle) then begin
      stlLayout.Values[WINDOW_LAYOUT_STATE] := IntToStr(Ord(wsMinimized));
    end else begin
      stlLayout.Values[WINDOW_LAYOUT_STATE] := IntToStr(Ord(form.WindowState));
    end;

    stlLayout.Values[WINDOW_LAYOUT_TOP]    := IntToStr(Ord(placement.rcNormalPosition.Top));
    stlLayout.Values[WINDOW_LAYOUT_LEFT]   := IntToStr(Ord(placement.rcNormalPosition.Left));
    stlLayout.Values[WINDOW_LAYOUT_HEIGHT] :=
      IntToStr(Ord(placement.rcNormalPosition.Bottom - placement.rcNormalPosition.Top));
    stlLayout.Values[WINDOW_LAYOUT_WIDTH]  :=
      IntToStr(Ord(placement.rcNormalPosition.Right - placement.rcNormalPosition.Left));

    stlLayout.Delimiter := ',';
    stlLayout.QuoteChar := '"';
    Result              := stlLayout.DelimitedText;
  finally
    stlLayout.Free();
  end;

end;


 // Set form's layout based on string representation of it
 // !! IMPORTANT !!
 // If there's a window layout stored, set ".Position" to poDefault before
 // calling this - otherwise it messes up the window if it was stored as
 // maximised.
 // Specifically, it shows the main window with maximised dimensions, with
 // the "Maximise" button in the top-right ready to "Normalise"
 // (non-maximise) the window, but WITH THE WINDOW SHOWN ABOUT 50 PIXELS
 // DOWN!)
procedure SDUSetFormLayout(form: TForm; layout: String);
var
  frmState:  TWindowState;
  stlLayout: TStringList;
  placement: TWindowPlacement;
  tmpHeight: Integer;
  tmpWidth:  Integer;
begin
  // Sanity...
  if (trim(layout) = '') then begin
    exit;
  end;

  stlLayout := TStringList.Create();
  try
    stlLayout.Delimiter     := ',';
    stlLayout.QuoteChar     := '"';
    stlLayout.DelimitedText := layout;

    // If the window's maximised, the forms top, left, width and height are set
    // to the maximised values - use this to get the normalised size and position
    placement.Length := sizeof(placement);
    GetWindowPlacement(form.Handle, @placement);

    placement.rcNormalPosition.Top  :=
      SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_TOP], placement.rcNormalPosition.Top);
    placement.rcNormalPosition.Left :=
      SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_LEFT],
      placement.rcNormalPosition.Left);

    tmpHeight := SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_HEIGHT],
      (placement.rcNormalPosition.Bottom - placement.rcNormalPosition.Top));
    tmpWidth  := SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_WIDTH],
      (placement.rcNormalPosition.Right - placement.rcNormalPosition.Left));

    placement.rcNormalPosition.Bottom := placement.rcNormalPosition.Top + tmpHeight;
    placement.rcNormalPosition.Right  := placement.rcNormalPosition.Left + tmpWidth;

    // this doesnt work if on second monitor - possible to do but more complex
    // http://stackoverflow.com/questions/7077572/get-current-native-screen-resolution-of-all-monitors-in-delphi-directx

    // Sanity check - we're only set the window position if it doesn't go
    // off screen (partially off-screen should be OK enough though - as long
    // as the title bar's visible, the user can always drag it back)
    if ((placement.rcNormalPosition.Top >= 0) and
      (placement.rcNormalPosition.Top < Screen.Height) and
      (placement.rcNormalPosition.Left >= 0) and (placement.rcNormalPosition.Left <
      Screen.Width)) then begin
      SetWindowPlacement(form.Handle, @placement);
    end;

    frmState := TWindowState(SDUTryStrToIntDflt(
      stlLayout.Values[WINDOW_LAYOUT_STATE], Ord(form.WindowState)));
    // Special case - if minimised, call Application.Minimise(...) - otherwise
    // the window looks like it's been "minimised to the desktop" - like an
    // MDI child window with the desktop being the main window
    if (frmState = wsMinimized) then begin
      form.Visible := True;
      Application.Minimize();
    end else begin
      form.WindowState := frmState;
    end;

  finally
    stlLayout.Free();
  end;

end;


initialization
  _SettingsObj := nil; //create by calling SetSettingsType

finalization
  _SettingsObj.Free;

end.
