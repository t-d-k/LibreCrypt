unit FreeOTFESettings;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes, // Required for TShortCut
  IniFiles,
  CommonSettings,
  OTFEFreeOTFEBase_U;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}
  
const
  FREEOTFE_REGISTRY_SETTINGS_LOCATION = '\Software\FreeOTFE';
  
resourcestring
  RS_PROMPT_USER = 'Prompt user';
  RS_DO_NOTHING = 'Do nothing';


type
  TOnExitWhenMounted = (oewmDismount, oewmPromptUser, oewmLeaveMounted);

resourcestring
  ONEXITWHENMOUNTED_DISMOUNT = 'Dismount all volumes';
  ONEXITWHENMOUNTED_LEAVEMOUNTED = 'Leave volumes mounted';

const
  OnExitWhenMountedTitlePtr: array [TOnExitWhenMounted] of Pointer = (
                                  @ONEXITWHENMOUNTED_DISMOUNT,
                                  @RS_PROMPT_USER,
                                  @ONEXITWHENMOUNTED_LEAVEMOUNTED
                                 );

type
  TOnExitWhenPortableMode = (owepPortableOff, oewpPromptUser, oewpDoNothing);

resourcestring
  ONEXITWHENPORTABLEMODE_PORTABLEOFF = 'Turn off portable mode';

const
  OnExitWhenPortableModeTitlePtr: array [TOnExitWhenPortableMode] of Pointer = (
                                  @ONEXITWHENPORTABLEMODE_PORTABLEOFF,
                                  @RS_PROMPT_USER,
                                  @RS_DO_NOTHING
                                 );


type
  TOnNormalDismountFail = (ondfForceDismount, ondfPromptUser, ondfCancelDismount);

resourcestring
  ONNORMALDISMOUNTFAIL_FORCEDISMOUNT = 'Force dismount';
  ONNORMALDISMOUNTFAIL_CANCELDISMOUNT = 'Cancel dismount';

const
  OnNormalDismountFailTitlePtr: array [TOnNormalDismountFail] of Pointer = (
                                  @ONNORMALDISMOUNTFAIL_FORCEDISMOUNT,
                                  @RS_PROMPT_USER,
                                  @ONNORMALDISMOUNTFAIL_CANCELDISMOUNT
                                 );


type
  TSystemTrayClickAction = (
                            stcaDoNothing,
                            stcaDisplayConsole,
                            stcaDisplayHideConsoleToggle,
                            stcaMountFile,
                            stcaMountPartition,
                            stcaMountLinuxFile,
                            stcaMountLinuxPartition,
                            stcaDismountAll
                           );

resourcestring
  SYSTEMTRAYCLICKACTION_DONOTHING                = 'Do nothing';
  SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE           = 'Display console';
  SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE = 'Display/hide console toggle';
  SYSTEMTRAYCLICKACTION_MOUNTFILE                = 'Mount file...';
  SYSTEMTRAYCLICKACTION_MOUNTPARTITION           = 'Mount partition...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE           = 'Mount file (Linux)...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION      = 'Mount partition (Linux)...';
  SYSTEMTRAYCLICKACTION_DISMOUNTALL              = 'Dismount all';

const
  SystemTrayClickActionTitlePtr: array [TSystemTrayClickAction] of Pointer = (
                                  @SYSTEMTRAYCLICKACTION_DONOTHING,
                                  @SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE,
                                  @SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE,
                                  @SYSTEMTRAYCLICKACTION_MOUNTFILE,
                                  @SYSTEMTRAYCLICKACTION_MOUNTPARTITION,
                                  @SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE,
                                  @SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION,
                                  @SYSTEMTRAYCLICKACTION_DISMOUNTALL
                                 );


type
  TFreeOTFESettings = class(TSettings)
  protected
    procedure _Load(iniFile: TCustomINIFile); override;
    function  _Save(iniFile: TCustomINIFile): boolean; override;

  public
    // General...
    OptDisplayToolbar: boolean;
    OptDisplayToolbarLarge: boolean;
    OptDisplayToolbarCaptions: boolean;
    OptDisplayStatusbar: boolean;
    OptExploreAfterMount: boolean;
    OptAllowMultipleInstances: boolean;
    OptAutoStartPortable: boolean;
    OptDefaultDriveLetter: char;
    OptDefaultMountAs: TFreeOTFEMountAs;

    // Prompts and messages
    OptPromptMountSuccessful: boolean;  // If set, display an info msg after successful mount
    OptWarnBeforeForcedDismount: boolean;
    OptOnExitWhenMounted: TOnExitWhenMounted;
    OptOnExitWhenPortableMode: TOnExitWhenPortableMode;
    OptOnNormalDismountFail: TOnNormalDismountFail;

    // System tray icon...
    OptSystemTrayIconDisplay: boolean;
    OptSystemTrayIconMinTo: boolean;
    OptSystemTrayIconCloseTo: boolean;
    OptSystemTrayIconActionSingleClick: TSystemTrayClickAction;
    OptSystemTrayIconActionDoubleClick: TSystemTrayClickAction;

    // Hotkey...
    OptHKeyEnableDismount: boolean;
    OptHKeyKeyDismount: TShortCut;
    OptHKeyEnableDismountEmerg: boolean;
    OptHKeyKeyDismountEmerg: TShortCut;

    // Autorun...
    OptPostMountExe: string;
    OptPreDismountExe: string;
    OptPostDismountExe: string;
    OptPrePostExeWarn: boolean;

    constructor Create(); override;
    destructor Destroy(); override;

    function  RegistryKey(): string; override;

  end;


var
  // Global variable
  Settings: TFreeOTFESettings;

function OnExitWhenMountedTitle(value: TOnExitWhenMounted): string;
function OnExitWhenPortableModeTitle(value: TOnExitWhenPortableMode): string;
function OnNormalDismountFailTitle(value: TOnNormalDismountFail): string;
function SystemTrayClickActionTitle(value: TSystemTrayClickAction): string;

implementation

uses
  Windows,  // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Registry,
  Dialogs,
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  Menus,  // Required for ShortCutToText and TextToShortCut
  ShlObj;  // Required for CSIDL_PERSONAL

const
  SETTINGS_V1 = 1;
  SETTINGS_V2 = 2;

  // -- General section --
    // Note: "Advanced" options are *also* stored within the "General" section
    //       This is done because some "General" settings were moved to become "Advanced" settings
    OPT_DISPLAYTOOLBAR                           = 'DisplayToolbar';
      DFLT_OPT_DISPLAYTOOLBAR                       = TRUE;
    OPT_DISPLAYTOOLBARLARGE                      = 'DisplayToolbarLarge';
      DFLT_OPT_DISPLAYTOOLBARLARGE                  = TRUE;
    OPT_DISPLAYTOOLBARCAPTIONS                   = 'DisplayToolbarCaptions';
      DFLT_OPT_DISPLAYTOOLBARCAPTIONS               = TRUE;
    OPT_DISPLAYSTATUSBAR                         = 'DisplayStatusbar';
      DFLT_OPT_DISPLAYSTATUSBAR                     = TRUE;
    OPT_EXPLOREAFTERMOUNT                        = 'ExploreAfterMount';
      DFLT_OPT_EXPLOREAFTERMOUNT                    = FALSE;
    OPT_ALLOWMULTIPLEINSTANCES                   = 'AllowMultipleInstances';
      DFLT_OPT_ALLOWMULTIPLEINSTANCES               = FALSE;
    OPT_AUTOSTARTPORTABLE                        = 'AutoStartPortable';
      DFLT_OPT_AUTOSTARTPORTABLE                    = FALSE;
    OPT_DEFAULTDRIVELETTER                       = 'DefaultDriveLetter';
      DFLT_OPT_DEFAULTDRIVELETTER                   = '#';
    OPT_DEFAULTMOUNTAS                           = 'DefaultMountAs';
      DFLT_OPT_DEFAULTMOUNTAS                       = fomaFixedDisk;

  // -- Prompts and messages --
  SECTION_CONFIRMATION = 'Confirmation';
    OPT_PROMPTMOUNTSUCCESSFUL                    = 'OptPromptMountSuccessful';
      DFLT_OPT_PROMPTMOUNTSUCCESSFUL                = TRUE;
    OPT_WARNBEFOREFORCEDISMOUNT                  = 'WarnBeforeForcedDismount';
      DFLT_OPT_WARNBEFOREFORCEDISMOUNT              = TRUE;
    OPT_ONEXITWHENMOUNTED                        = 'OnExitWhenMounted';
      DFLT_OPT_ONEXITWHENMOUNTED                    = oewmPromptUser;
    OPT_ONEXITWHENPORTABLEMODE                   = 'OnExitWhenPortableMode';
      DFLT_OPT_ONEXITWHENPORTABLEMODE               = oewpPromptUser;
    OPT_ONNORMALDISMOUNTFAIL                     = 'OnNormalDismountFail';
      DFLT_OPT_ONNORMALDISMOUNTFAIL                 = oewpPromptUser;

  // -- System tray icon section --
  SECTION_SYSTEMTRAYICON = 'SystemTrayIcon';
    OPT_SYSTEMTRAYICONDISPLAY                    = 'Display';
      DFLT_OPT_SYSTEMTRAYICONDISPLAY                = TRUE;
    OPT_SYSTEMTRAYICONMINTO                      = 'MinTo';
      DFLT_OPT_SYSTEMTRAYICONMINTO                  = FALSE;
    OPT_SYSTEMTRAYICONCLOSETO                    = 'CloseTo';
      DFLT_OPT_SYSTEMTRAYICONCLOSETO                = FALSE;
    OPT_SYSTEMTRAYICONACTIONSINGLECLICK          = 'ActionSingleClick';
      DFLT_OPT_SYSTEMTRAYICONACTIONSINGLECLICK      = stcaDoNothing;
    OPT_SYSTEMTRAYICONACTIONDOUBLECLICK          = 'ActionDoubleClick';
      DFLT_OPT_SYSTEMTRAYICONACTIONDOUBLECLICK      = stcaDisplayConsole;

  // -- Hotkey section --
  SECTION_HOTKEY = 'Hotkeys';
    OPT_HOTKEYENABLEDISMOUNT                     = 'EnableDismount';
      DFLT_OPT_HOTKEYENABLEDISMOUNT                 = FALSE;
    OPT_HOTKEYKEYDISMOUNT                        = 'KeyDismount';
      DFLT_OPT_HOTKEYKEYDISMOUNT                    = 'Shift+Ctrl+D';
    OPT_HOTKEYENABLEDISMOUNTEMERG                = 'EnableDismountEmerg';
      DFLT_OPT_HOTKEYENABLEDISMOUNTEMERG            = FALSE;
    OPT_HOTKEYKEYDISMOUNTEMERG                   = 'KeyDismountEmerg';
      DFLT_OPT_HOTKEYKEYDISMOUNTEMERG               = 'Shift+Ctrl+Alt+D';

  // -- Autorun section --
  SECTION_AUTORUN = 'Autorun';
    OPT_POSTMOUNTEXE                             = 'PostMountExe';
      DFLT_OPT_POSTMOUNTEXE                         = '';
    OPT_PREDISMOUNTEXE                           = 'PreDismountExe';
      DFLT_OPT_PREDISMOUNTEXE                       = '';
    OPT_POSTDISMOUNTEXE                          = 'PostDismountExe';
      DFLT_OPT_POSTDISMOUNTEXE                      = '';
    OPT_PREPOSTEXEWARN                           = 'PrePostExeWarn';
      DFLT_OPT_PREPOSTEXEWARN                       = TRUE;


function OnExitWhenMountedTitle(value: TOnExitWhenMounted): string;
begin
  Result := LoadResString(OnExitWhenMountedTitlePtr[value]);
end;

function OnExitWhenPortableModeTitle(value: TOnExitWhenPortableMode): string;
begin
  Result := LoadResString(OnExitWhenPortableModeTitlePtr[value]);
end;

function OnNormalDismountFailTitle(value: TOnNormalDismountFail): string;
begin
  Result := LoadResString(OnNormalDismountFailTitlePtr[value]);
end;

function SystemTrayClickActionTitle(value: TSystemTrayClickAction): string;
begin
  Result := LoadResString(SystemTrayClickActionTitlePtr[value]);
end;


constructor TFreeOTFESettings.Create();
begin
  inherited;

  Load();

end;

destructor TFreeOTFESettings.Destroy();
begin

  inherited;
end;

procedure TFreeOTFESettings._Load(iniFile: TCustomINIFile);
var
  useDefaultDriveLetter: string;
begin
  inherited _Load(iniFile);

  OptDisplayToolbar          := iniFile.ReadBool(SECTION_GENERAL,   OPT_DISPLAYTOOLBAR,         DFLT_OPT_DISPLAYTOOLBAR);
  OptDisplayToolbarLarge     := iniFile.ReadBool(SECTION_GENERAL,   OPT_DISPLAYTOOLBARLARGE,    DFLT_OPT_DISPLAYTOOLBARLARGE);
  OptDisplayToolbarCaptions  := iniFile.ReadBool(SECTION_GENERAL,   OPT_DISPLAYTOOLBARCAPTIONS, DFLT_OPT_DISPLAYTOOLBARCAPTIONS);
  OptDisplayStatusbar        := iniFile.ReadBool(SECTION_GENERAL,   OPT_DISPLAYSTATUSBAR,       DFLT_OPT_DISPLAYSTATUSBAR);
  OptExploreAfterMount       := iniFile.ReadBool(SECTION_GENERAL,   OPT_EXPLOREAFTERMOUNT,      DFLT_OPT_EXPLOREAFTERMOUNT);
  OptAllowMultipleInstances  := iniFile.ReadBool(SECTION_GENERAL,   OPT_ALLOWMULTIPLEINSTANCES, DFLT_OPT_ALLOWMULTIPLEINSTANCES);
  OptAutoStartPortable       := iniFile.ReadBool(SECTION_GENERAL,   OPT_AUTOSTARTPORTABLE,      DFLT_OPT_AUTOSTARTPORTABLE);
  useDefaultDriveLetter      := iniFile.ReadString(SECTION_GENERAL, OPT_DEFAULTDRIVELETTER,     DFLT_OPT_DEFAULTDRIVELETTER);
  // #0 written as "#"
  OptDefaultDriveLetter := useDefaultDriveLetter[1];
  if (OptDefaultDriveLetter = '#') then
    begin
    OptDefaultDriveLetter := #0;
    end;
  OptDefaultMountAs        := TFreeOTFEMountAs(iniFile.ReadInteger(SECTION_GENERAL, OPT_DEFAULTMOUNTAS, ord(DFLT_OPT_DEFAULTMOUNTAS)));

  OptPromptMountSuccessful    := iniFile.ReadBool(SECTION_CONFIRMATION, OPT_PROMPTMOUNTSUCCESSFUL,       DFLT_OPT_PROMPTMOUNTSUCCESSFUL);
  OptWarnBeforeForcedDismount := iniFile.ReadBool(SECTION_CONFIRMATION, OPT_WARNBEFOREFORCEDISMOUNT,     DFLT_OPT_WARNBEFOREFORCEDISMOUNT);
  OptOnExitWhenMounted        := TOnExitWhenMounted(iniFile.ReadInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED, ord(DFLT_OPT_ONEXITWHENMOUNTED)));
  OptOnExitWhenPortableMode   := TOnExitWhenPortableMode(iniFile.ReadInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENPORTABLEMODE, ord(DFLT_OPT_ONEXITWHENPORTABLEMODE)));
  OptOnNormalDismountFail     := TOnNormalDismountFail(iniFile.ReadInteger(SECTION_CONFIRMATION, OPT_ONNORMALDISMOUNTFAIL, ord(DFLT_OPT_ONNORMALDISMOUNTFAIL)));

  OptSystemTrayIconDisplay   := iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY, DFLT_OPT_SYSTEMTRAYICONDISPLAY);
  OptSystemTrayIconMinTo     := iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,   DFLT_OPT_SYSTEMTRAYICONMINTO);
  OptSystemTrayIconCloseTo   := iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO, DFLT_OPT_SYSTEMTRAYICONCLOSETO);
  OptSystemTrayIconActionSingleClick := TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONSINGLECLICK, ord(DFLT_OPT_SYSTEMTRAYICONACTIONSINGLECLICK)));
  OptSystemTrayIconActionDoubleClick := TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONDOUBLECLICK, ord(DFLT_OPT_SYSTEMTRAYICONACTIONDOUBLECLICK)));

  OptHKeyEnableDismount      := iniFile.ReadBool(SECTION_HOTKEY,   OPT_HOTKEYENABLEDISMOUNT,      DFLT_OPT_HOTKEYENABLEDISMOUNT);
  OptHKeyKeyDismount         := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNT,         DFLT_OPT_HOTKEYKEYDISMOUNT));
  OptHKeyEnableDismountEmerg := iniFile.ReadBool(SECTION_HOTKEY,   OPT_HOTKEYENABLEDISMOUNTEMERG, DFLT_OPT_HOTKEYENABLEDISMOUNTEMERG);
  OptHKeyKeyDismountEmerg    := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNTEMERG,    DFLT_OPT_HOTKEYKEYDISMOUNTEMERG));

  OptPostMountExe            := iniFile.ReadString(SECTION_AUTORUN,      OPT_POSTMOUNTEXE,          DFLT_OPT_POSTMOUNTEXE);
  OptPreDismountExe          := iniFile.ReadString(SECTION_AUTORUN,      OPT_PREDISMOUNTEXE,        DFLT_OPT_PREDISMOUNTEXE);
  OptPostDismountExe         := iniFile.ReadString(SECTION_AUTORUN,      OPT_POSTDISMOUNTEXE,       DFLT_OPT_POSTDISMOUNTEXE);
  OptPrePostExeWarn          := iniFile.ReadBool(SECTION_AUTORUN,        OPT_PREPOSTEXEWARN,        DFLT_OPT_PREPOSTEXEWARN);

end;


function TFreeOTFESettings._Save(iniFile: TCustomINIFile): boolean;
var
  allOK: boolean;
  useDefaultDriveLetter: char;
begin
  allOK := inherited _Save(iniFile);
  if allOK then
    begin
    try
      iniFile.WriteBool(SECTION_GENERAL,        OPT_DISPLAYTOOLBAR,            OptDisplayToolbar);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_DISPLAYTOOLBARLARGE,       OptDisplayToolbarLarge);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_DISPLAYTOOLBARCAPTIONS,    OptDisplayToolbarCaptions);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_DISPLAYSTATUSBAR,          OptDisplayStatusbar);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_EXPLOREAFTERMOUNT,         OptExploreAfterMount);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_ALLOWMULTIPLEINSTANCES,    OptAllowMultipleInstances);
      iniFile.WriteBool(SECTION_GENERAL,        OPT_AUTOSTARTPORTABLE,         OptAutoStartPortable);
      // #0 written as "#"
      useDefaultDriveLetter := OptDefaultDriveLetter;
      if (OptDefaultDriveLetter = #0) then
        begin
        useDefaultDriveLetter := '#';
        end;
      iniFile.WriteString(SECTION_GENERAL,      OPT_DEFAULTDRIVELETTER,        useDefaultDriveLetter);
      iniFile.WriteInteger(SECTION_GENERAL,     OPT_DEFAULTMOUNTAS,            ord(OptDefaultMountAs));

      iniFile.WriteBool(SECTION_CONFIRMATION,    OPT_PROMPTMOUNTSUCCESSFUL,    OptPromptMountSuccessful);
      iniFile.WriteBool(SECTION_CONFIRMATION,    OPT_WARNBEFOREFORCEDISMOUNT,  OptWarnBeforeForcedDismount);
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED,        ord(OptOnExitWhenMounted));
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENPORTABLEMODE,   ord(OptOnExitWhenPortableMode));
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONNORMALDISMOUNTFAIL,     ord(OptOnNormalDismountFail));

      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY,     OptSystemTrayIconDisplay);
      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,       OptSystemTrayIconMinTo);
      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO,     OptSystemTrayIconCloseTo);
      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONSINGLECLICK,     ord(OptSystemTrayIconActionSingleClick));
      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONDOUBLECLICK,     ord(OptSystemTrayIconActionDoubleClick));

      iniFile.WriteBool(SECTION_HOTKEY,         OPT_HOTKEYENABLEDISMOUNT,      OptHKeyEnableDismount);
      iniFile.WriteString(SECTION_HOTKEY,       OPT_HOTKEYKEYDISMOUNT,         ShortCutToText(OptHKeyKeyDismount));
      iniFile.WriteBool(SECTION_HOTKEY,         OPT_HOTKEYENABLEDISMOUNTEMERG, OptHKeyEnableDismountEmerg);
      iniFile.WriteString(SECTION_HOTKEY,       OPT_HOTKEYKEYDISMOUNTEMERG,    ShortCutToText(OptHKeyKeyDismountEmerg));

      iniFile.WriteString(SECTION_AUTORUN,      OPT_POSTMOUNTEXE,              OptPostMountExe);
      iniFile.WriteString(SECTION_AUTORUN,      OPT_PREDISMOUNTEXE,            OptPreDismountExe);
      iniFile.WriteString(SECTION_AUTORUN,      OPT_POSTDISMOUNTEXE,           OptPostDismountExe);
      iniFile.WriteBool(SECTION_AUTORUN,        OPT_PREPOSTEXEWARN,            OptPrePostExeWarn);

    except
      on E:Exception do
        begin
        allOK := FALSE;
        end;
    end;
    end;

  Result := allOK;
end;

function TFreeOTFESettings.RegistryKey(): string; 
begin
  Result := FREEOTFE_REGISTRY_SETTINGS_LOCATION;
end;

END.


