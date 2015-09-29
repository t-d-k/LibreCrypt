unit MainSettings;
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
  //  OTFEFreeOTFEBase_U,
  //sdu
  lcTypes,
  CommonSettings;

const
{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF

  SDUCRLF = ''#13#10;
{$ENDIF}

  FREEOTFE_REGISTRY_SETTINGS_LOCATION = '\Software\LibreCrypt';



resourcestring
  RS_PROMPT_USER = 'Prompt user';
  RS_DO_NOTHING  = 'Do nothing';


type
  eOnExitWhenMounted = (oewmDismount, oewmPromptUser, oewmLeaveMounted);

resourcestring
  ONEXITWHENMOUNTED_DISMOUNT     = 'Lock all containers';
  ONEXITWHENMOUNTED_LEAVEMOUNTED = 'Leave containers open';

const
  ON_EXIT_WHEN_MOUNTED_TITLE_PTR: array [eOnExitWhenMounted] of Pointer =
    (@ONEXITWHENMOUNTED_DISMOUNT, @RS_PROMPT_USER, @ONEXITWHENMOUNTED_LEAVEMOUNTED
    );

type
  eOnExitWhenPortableMode = (owepPortableOff, oewpPromptUser, oewpDoNothing);

resourcestring
  ONEXITWHENPORTABLEMODE_PORTABLEOFF = 'Turn off portable mode';

const
  ON_EXIT_WHEN_PORTABLE_MODE_TITLE_PTR: array [eOnExitWhenPortableMode] of Pointer =
    (@ONEXITWHENPORTABLEMODE_PORTABLEOFF, @RS_PROMPT_USER, @RS_DO_NOTHING
    );


type
  eOnNormalDismountFail = (ondfForceDismount, ondfPromptUser, ondfCancelDismount);



resourcestring
  ONNORMALDISMOUNTFAIL_FORCEDISMOUNT  = 'Force lock';
  ONNORMALDISMOUNTFAIL_CANCELDISMOUNT = 'Cancel lock';

const
  OnNormalDismountFailTitlePtr: array [eOnNormalDismountFail] of Pointer =
    (@ONNORMALDISMOUNTFAIL_FORCEDISMOUNT, @RS_PROMPT_USER, @ONNORMALDISMOUNTFAIL_CANCELDISMOUNT
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
  SYSTEMTRAYCLICKACTION_DONOTHING           = 'Do nothing';
  SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE      = 'Show window';
  SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE = 'Show/hide window toggle';
  SYSTEMTRAYCLICKACTION_MOUNTFILE           = 'Open LibreCrypt file ...';
  SYSTEMTRAYCLICKACTION_MOUNTPARTITION      = 'Open LibreCrypt partition ...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE      = 'Open (Linux) ...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION = 'Open partition (Linux) ...';
  SYSTEMTRAYCLICKACTION_DISMOUNTALL         = 'Lock all';

const
  SYSTEM_TRAY_CLICK_ACTION_TITLE_PTR: array [TSystemTrayClickAction] of Pointer =
    (@SYSTEMTRAYCLICKACTION_DONOTHING, @SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE,
    @SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE, @SYSTEMTRAYCLICKACTION_MOUNTFILE,
    @SYSTEMTRAYCLICKACTION_MOUNTPARTITION, @SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE,
    @SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION, @SYSTEMTRAYCLICKACTION_DISMOUNTALL
    );


type
  TMainSettings = class (TCommonSettings)
  private
    femergencyDismountHotKeyEnabled: Boolean;
    fsysTrayDoubleClickAction: TSystemTrayClickAction;
    fcloseToSysTray:           Boolean;
    fexitWhenPortableModeAction: eOnExitWhenPortableMode;
    fdismountHotKey:       TShortCut;
    fOptAutoStartPortable:     Boolean;
    femergencyDismountHotKey:  TShortCut;
    fshowSysTrayIcon:          Boolean;
    fOptOnExitWhenMounted:     eOnExitWhenMounted;
    fdefaultMountDiskType:     TMountDiskType;
    fOptAllowMultipleInstances: Boolean;
    fsysTraySingleClickAction: TSystemTrayClickAction;
    fminimiseToSysTray:        Boolean;
    fOptWarnBeforeForcedDismount: Boolean;
    fdismountHotKeyEnabled:    Boolean;
    fOptOnNormalDismountFail:  eOnNormalDismountFail;

  protected
    procedure _LoadOld(iniFile: TCustomINIFile); override;
    //    function _Save(iniFile: TCustomINIFile): Boolean; OVERRIDE;
    procedure _SetDefaults; override;

  public



    function RegistryKey(): String; override;

  published
    (*
     property names are stored as keys in ini file where users can change so keep friendly and explanatory
     *)

    // settings need to be properties and published in order to be picked up by rtti and saved to ini file

    // General...
    property AllowMultipleInstances: Boolean Read FOptAllowMultipleInstances
      Write FOptAllowMultipleInstances default False;
    property AutoStartPortableMode: Boolean Read FOptAutoStartPortable
      Write FOptAutoStartPortable default False;
    //    OptInstalled:              Boolean;// has installer been run?

    property DefaultMountDiskType: TMountDiskType
      Read fdefaultMountDiskType Write fdefaultMountDiskType default fomaRemovableDisk;

    // Prompts and messages
    property WarnBeforeForceDismount: Boolean Read FOptWarnBeforeForcedDismount
      Write fOptWarnBeforeForcedDismount default True;
    property ExitWhenMountedAction: eOnExitWhenMounted
      Read FOptOnExitWhenMounted Write fOptOnExitWhenMounted default oewmPromptUser;
    property ExitWhenPortableModeAction: eOnExitWhenPortableMode
      Read fexitWhenPortableModeAction Write fexitWhenPortableModeAction default oewpPromptUser;
    property NormalDismountFailAction: eOnNormalDismountFail
      Read FOptOnNormalDismountFail Write fOptOnNormalDismountFail default ondfPromptUser;

    // System tray icon...
    property ShowSysTrayIcon: Boolean Read fshowSysTrayIcon
      Write fshowSysTrayIcon default True;
    property MinimiseToSysTray: Boolean Read fminimiseToSysTray
      Write fminimiseToSysTray default False;
    property CloseToSysTray: Boolean
      Read fcloseToSysTray Write fcloseToSysTray default True;
    property SysTraySingleClickAction: TSystemTrayClickAction
      Read fsysTraySingleClickAction Write fsysTraySingleClickAction default stcaDisplayConsole;
    property SysTrayDoubleClickAction: TSystemTrayClickAction
      Read fsysTrayDoubleClickAction Write fsysTrayDoubleClickAction default stcaDisplayConsole;

    // Hotkey...
    property DismountHotKeyEnabled: Boolean Read fdismountHotKeyEnabled
      Write fdismountHotKeyEnabled default False;
     // default 'Shift+Ctrl+D';
    property DismountHotKey: TShortCut Read fdismountHotKey Write fdismountHotKey;

    property EmergencyDismountHotKeyEnabled: Boolean
      Read femergencyDismountHotKeyEnabled Write femergencyDismountHotKeyEnabled default False;
      // default 'Shift+Ctrl+Alt+D';
    property EmergencyDismountHotKey: TShortCut Read femergencyDismountHotKey
      Write femergencyDismountHotKey;

  end;


 //var
 // Global variable
 //  gSettings: TMainSettings;
 {returns an instance of the only object, must be type TMainSettings. call SetSettingsType first}
function GetMainSettings: TMainSettings;

function OnExitWhenMountedTitle(Value: eOnExitWhenMounted): String;
function OnExitWhenPortableModeTitle(Value: eOnExitWhenPortableMode): String;
function OnNormalDismountFailTitle(Value: eOnNormalDismountFail): String;
function SystemTrayClickActionTitle(Value: TSystemTrayClickAction): String;

implementation

uses
  Windows,   // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Dialogs,
  Menus, Registry,
  //sdu
  SDUDialogs,
  SDUi18n,
           // Required for ShortCutToText and TextToShortCut
  ShlObj;  // Required for CSIDL_PERSONAL

const
  SETTINGS_V1 = 1;
  SETTINGS_V2 = 2;

  // -- General section --
  // Note: "Advanced" options are *also* stored within the "General" section
  //       This is done because some "General" settings were moved to become "Advanced" settings


  OPT_ALLOWMULTIPLEINSTANCES      = 'AllowMultipleInstances';
  DFLT_OPT_ALLOWMULTIPLEINSTANCES = False;
  OPT_AUTOSTARTPORTABLE           = 'AutoStartPortable';
  DFLT_OPT_AUTOSTARTPORTABLE      = False;
  OPT_DEFAULTMOUNTAS              = 'DefaultMountAs';
  DFLT_OPT_DEFAULTMOUNTAS         = fomaRemovableDisk;
  //  OPT_OPTINSTALLED                = 'Installed';
  //  DFLT_OPT_OPTINSTALLED           = False;


  // -- Prompts and messages --
  // Section name defined in parent class's unit
  OPT_WARNBEFOREFORCEDISMOUNT      = 'WarnBeforeForcedDismount';
  DFLT_OPT_WARNBEFOREFORCEDISMOUNT = True;
  OPT_ONEXITWHENMOUNTED            = 'OnExitWhenMounted';
  DFLT_OPT_ONEXITWHENMOUNTED       = oewmPromptUser;
  OPT_ONEXITWHENPORTABLEMODE       = 'OnExitWhenPortableMode';
  DFLT_OPT_ONEXITWHENPORTABLEMODE  = oewpPromptUser;
  OPT_ONNORMALDISMOUNTFAIL         = 'OnNormalDismountFail';
  DFLT_OPT_ONNORMALDISMOUNTFAIL    = oewpPromptUser;

  // -- System tray icon section --
  SECTION_SYSTEMTRAYICON                   = 'SystemTrayIcon';
  OPT_SYSTEMTRAYICONDISPLAY                = 'Display';
  DFLT_OPT_SYSTEMTRAYICONDISPLAY           = True;
  OPT_SYSTEMTRAYICONMINTO                  = 'MinTo';
  DFLT_OPT_SYSTEMTRAYICONMINTO             = False;
  OPT_SYSTEMTRAYICONCLOSETO                = 'CloseTo';
  DFLT_OPT_SYSTEMTRAYICONCLOSETO           = True;
  OPT_SYSTEMTRAYICONACTIONSINGLECLICK      = 'ActionSingleClick';
  DFLT_OPT_SYSTEMTRAYICONACTIONSINGLECLICK = stcaDisplayConsole;
  OPT_SYSTEMTRAYICONACTIONDOUBLECLICK      = 'ActionDoubleClick';
  DFLT_OPT_SYSTEMTRAYICONACTIONDOUBLECLICK = stcaDisplayConsole;

  // -- Hotkey section --
  SECTION_HOTKEY                     = 'Hotkeys';
  OPT_HOTKEYENABLEDISMOUNT           = 'EnableDismount';
  DFLT_OPT_HOTKEYENABLEDISMOUNT      = False;
  OPT_HOTKEYKEYDISMOUNT              = 'KeyDismount';
  DFLT_OPT_HOTKEYKEYDISMOUNT         = 'Shift+Ctrl+D';
  OPT_HOTKEYENABLEDISMOUNTEMERG      = 'EnableDismountEmerg';
  DFLT_OPT_HOTKEYENABLEDISMOUNTEMERG = False;
  OPT_HOTKEYKEYDISMOUNTEMERG         = 'KeyDismountEmerg';
  DFLT_OPT_HOTKEYKEYDISMOUNTEMERG    = 'Shift+Ctrl+Alt+D';


function OnExitWhenMountedTitle(Value: eOnExitWhenMounted): String;
begin
  Result := LoadResString(ON_EXIT_WHEN_MOUNTED_TITLE_PTR[Value]);
end;

function OnExitWhenPortableModeTitle(Value: eOnExitWhenPortableMode): String;
begin
  Result := LoadResString(ON_EXIT_WHEN_PORTABLE_MODE_TITLE_PTR[Value]);
end;

function OnNormalDismountFailTitle(Value: eOnNormalDismountFail): String;
begin
  Result := LoadResString(OnNormalDismountFailTitlePtr[Value]);
end;

function SystemTrayClickActionTitle(Value: TSystemTrayClickAction): String;
begin
  Result := LoadResString(SYSTEM_TRAY_CLICK_ACTION_TITLE_PTR[Value]);
end;


procedure TMainSettings._LoadOld(iniFile: TCustomINIFile);
begin
  inherited _LoadOld(iniFile);


  AllowMultipleInstances := iniFile.ReadBool(SECTION_GENERAL, OPT_ALLOWMULTIPLEINSTANCES,
    DFLT_OPT_ALLOWMULTIPLEINSTANCES);
  AutoStartPortableMode  := iniFile.ReadBool(SECTION_GENERAL, OPT_AUTOSTARTPORTABLE,
    DFLT_OPT_AUTOSTARTPORTABLE);
  DefaultMountDiskType   :=
    TMountDiskType(iniFile.ReadInteger(SECTION_GENERAL, OPT_DEFAULTMOUNTAS,
    Ord(DFLT_OPT_DEFAULTMOUNTAS)));


  WarnBeforeForceDismount    := iniFile.ReadBool(SECTION_CONFIRMATION,
    OPT_WARNBEFOREFORCEDISMOUNT, DFLT_OPT_WARNBEFOREFORCEDISMOUNT);
  ExitWhenMountedAction      :=
    eOnExitWhenMounted(iniFile.ReadInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED,
    Ord(DFLT_OPT_ONEXITWHENMOUNTED)));
  ExitWhenPortableModeAction :=
    eOnExitWhenPortableMode(iniFile.ReadInteger(SECTION_CONFIRMATION,
    OPT_ONEXITWHENPORTABLEMODE, Ord(DFLT_OPT_ONEXITWHENPORTABLEMODE)));
  NormalDismountFailAction   :=
    eOnNormalDismountFail(iniFile.ReadInteger(SECTION_CONFIRMATION,
    OPT_ONNORMALDISMOUNTFAIL, Ord(DFLT_OPT_ONNORMALDISMOUNTFAIL)));

  fShowSysTrayIcon          :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY,
    DFLT_OPT_SYSTEMTRAYICONDISPLAY);
  fMinimiseToSysTray        :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,
    DFLT_OPT_SYSTEMTRAYICONMINTO);
  fCloseToSysTray           :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO,
    DFLT_OPT_SYSTEMTRAYICONCLOSETO);
  fSysTraySingleClickAction :=
    TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON,
    OPT_SYSTEMTRAYICONACTIONSINGLECLICK, Ord(DFLT_OPT_SYSTEMTRAYICONACTIONSINGLECLICK)));
  fSysTrayDoubleClickAction :=
    TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON,
    OPT_SYSTEMTRAYICONACTIONDOUBLECLICK, Ord(DFLT_OPT_SYSTEMTRAYICONACTIONDOUBLECLICK)));

  fDismountHotKeyEnabled         := iniFile.ReadBool(SECTION_HOTKEY,
    OPT_HOTKEYENABLEDISMOUNT, DFLT_OPT_HOTKEYENABLEDISMOUNT);
  DismountHotKey                 := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY,
    OPT_HOTKEYKEYDISMOUNT, DFLT_OPT_HOTKEYKEYDISMOUNT));
  EmergencyDismountHotKeyEnabled :=
    iniFile.ReadBool(SECTION_HOTKEY, OPT_HOTKEYENABLEDISMOUNTEMERG,
    DFLT_OPT_HOTKEYENABLEDISMOUNTEMERG);
  EmergencyDismountHotKey        := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY,
    OPT_HOTKEYKEYDISMOUNTEMERG, DFLT_OPT_HOTKEYKEYDISMOUNTEMERG));

end;

procedure TMainSettings._SetDefaults;
begin
  inherited;
  fdismountHotKey      := TextToShortCut(DFLT_OPT_HOTKEYKEYDISMOUNT);
  femergencyDismountHotKey := TextToShortCut(DFLT_OPT_HOTKEYKEYDISMOUNTEMERG);
end;

 //
 //function TMainSettings._Save(iniFile: TCustomINIFile): Boolean;
 //begin
 //  Result := inherited _Save(iniFile);
 //  if Result then begin
 //    try
 //
 //
 //      iniFile.WriteBool(SECTION_GENERAL, OPT_ALLOWMULTIPLEINSTANCES,
 //        OptAllowMultipleInstances);
 //      iniFile.WriteBool(SECTION_GENERAL, OPT_AUTOSTARTPORTABLE,
 //        OptAutoStartPortable);
 //      iniFile.WriteInteger(SECTION_GENERAL, OPT_DEFAULTMOUNTAS,
 //        Ord(OptDefaultMountAs));
 //
 //
 //      iniFile.WriteBool(SECTION_CONFIRMATION, OPT_WARNBEFOREFORCEDISMOUNT,
 //        OptWarnBeforeForcedDismount);
 //      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED,
 //        Ord(OptOnExitWhenMounted));
 //      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENPORTABLEMODE,
 //        Ord(OptOnExitWhenPortableMode));
 //      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONNORMALDISMOUNTFAIL,
 //        Ord(OptOnNormalDismountFail));
 //
 //
 //
 //      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY,
 //        OptSystemTrayIconDisplay);
 //      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,
 //        OptSystemTrayIconMinTo);
 //      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO,
 //        OptSystemTrayIconCloseTo);
 //      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONSINGLECLICK,
 //        Ord(OptSystemTrayIconActionSingleClick));
 //      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONDOUBLECLICK,
 //        Ord(OptSystemTrayIconActionDoubleClick));
 //
 //      iniFile.WriteBool(SECTION_HOTKEY, OPT_HOTKEYENABLEDISMOUNT,
 //        OptHKeyEnableDismount);
 //      iniFile.WriteString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNT,
 //        ShortCutToText(OptHKeyKeyDismount));
 //      iniFile.WriteBool(SECTION_HOTKEY, OPT_HOTKEYENABLEDISMOUNTEMERG,
 //        OptHKeyEnableDismountEmerg);
 //      iniFile.WriteString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNTEMERG,
 //        ShortCutToText(OptHKeyKeyDismountEmerg));
 //
 //    except
 //      on E: Exception do begin
 //        Result := False;
 //      end;
 //    end;
 //  end;
 //
 //end;

function TMainSettings.RegistryKey(): String;
begin
  Result := FREEOTFE_REGISTRY_SETTINGS_LOCATION;
end;

{returns an instance of the only object, must be type TMainSettings. call SetSettingsType first}
function GetMainSettings: TMainSettings;
begin
  assert(GetSettings is TMainSettings, 'call SetSettingsType with correct type');
  Result := GetSettings as TMainSettings;
end;

end.
