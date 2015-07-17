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
  CommonSettings,
  IniFiles,
  OTFEFreeOTFEBase_U,
  //sdu
  sdugeneral;
const
{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF

  SDUCRLF = ''#13#10;
{$ENDIF}

  FREEOTFE_REGISTRY_SETTINGS_LOCATION = '\Software\LibreCrypt';

  // Command line parameter handled in the .dpr
  CMDLINE_MINIMIZE = 'minimize';

resourcestring
  RS_PROMPT_USER = 'Prompt user';
  RS_DO_NOTHING  = 'Do nothing';


type
  eOnExitWhenMounted = (oewmDismount, oewmPromptUser, oewmLeaveMounted);

resourcestring
  ONEXITWHENMOUNTED_DISMOUNT     = 'Lock all containers';
  ONEXITWHENMOUNTED_LEAVEMOUNTED = 'Leave containers open';

const
  OnExitWhenMountedTitlePtr: array [eOnExitWhenMounted] of Pointer =
    (@ONEXITWHENMOUNTED_DISMOUNT, @RS_PROMPT_USER, @ONEXITWHENMOUNTED_LEAVEMOUNTED
    );

type
  eOnExitWhenPortableMode = (owepPortableOff, oewpPromptUser, oewpDoNothing);

resourcestring
  ONEXITWHENPORTABLEMODE_PORTABLEOFF = 'Turn off portable mode';

const
  OnExitWhenPortableModeTitlePtr: array [eOnExitWhenPortableMode] of Pointer =
    (@ONEXITWHENPORTABLEMODE_PORTABLEOFF, @RS_PROMPT_USER, @RS_DO_NOTHING
    );


type
  eOnNormalDismountFail = (ondfForceDismount, ondfPromptUser, ondfCancelDismount);

  // is app running? used to detect crashes
  eAppRunning = (arDontKnow{no settings file}, arRunning{set in InitApp}, arClosed{set in Form.close});

resourcestring
  ONNORMALDISMOUNTFAIL_FORCEDISMOUNT  = 'Force dismount';
  ONNORMALDISMOUNTFAIL_CANCELDISMOUNT = 'Cancel dismount';

const
  OnNormalDismountFailTitlePtr: array [eOnNormalDismountFail] of Pointer =
    (@ONNORMALDISMOUNTFAIL_FORCEDISMOUNT, @RS_PROMPT_USER,
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
  SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE           = 'Display window';
  SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE = 'Display/hide window toggle';
  SYSTEMTRAYCLICKACTION_MOUNTFILE                = 'Open LibreCrypt file ...';
  SYSTEMTRAYCLICKACTION_MOUNTPARTITION           = 'Open LibreCrypt partition...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE           = 'Open (Linux) ...';
  SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION      = 'Open partition (Linux) ...';
  SYSTEMTRAYCLICKACTION_DISMOUNTALL              = 'Lock all';

const
  SystemTrayClickActionTitlePtr: array [TSystemTrayClickAction] of Pointer =
    (@SYSTEMTRAYCLICKACTION_DONOTHING, @SYSTEMTRAYCLICKACTION_DISPLAYCONSOLE,
    @SYSTEMTRAYCLICKACTION_DISPLAYHIDECONSOLETOGGLE, @SYSTEMTRAYCLICKACTION_MOUNTFILE,
    @SYSTEMTRAYCLICKACTION_MOUNTPARTITION, @SYSTEMTRAYCLICKACTION_MOUNTLINUXFILE,
    @SYSTEMTRAYCLICKACTION_MOUNTLINUXPARTITION, @SYSTEMTRAYCLICKACTION_DISMOUNTALL
    );


type
  TMainSettings = class (TCommonSettings)
  PROTECTED
    procedure _Load(iniFile: TCustomINIFile); OVERRIDE;
    function _Save(iniFile: TCustomINIFile): Boolean; OVERRIDE;

  PUBLIC
    // General...
    OptDisplayToolbar:         Boolean;
    OptDisplayToolbarLarge:    Boolean;
    OptDisplayToolbarCaptions: Boolean;
    OptDisplayStatusbar:       Boolean;
    OptAllowMultipleInstances: Boolean;
    OptAutoStartPortable:      Boolean;
//    OptInstalled:              Boolean;// has installer been run?
    OptDefaultDriveLetter:     DriveLetterChar;
    OptDefaultMountAs:         TFreeOTFEMountAs;
    feAppRunning        :         eAppRunning;
    // Prompts and messages
    OptWarnBeforeForcedDismount: Boolean;
    OptOnExitWhenMounted:        eOnExitWhenMounted;
    OptOnExitWhenPortableMode:   eOnExitWhenPortableMode;
    OptOnNormalDismountFail:     eOnNormalDismountFail;

    // System tray icon...
    OptSystemTrayIconDisplay:           Boolean;
    OptSystemTrayIconMinTo:             Boolean;
    OptSystemTrayIconCloseTo:           Boolean;
    OptSystemTrayIconActionSingleClick: TSystemTrayClickAction;
    OptSystemTrayIconActionDoubleClick: TSystemTrayClickAction;

    // Hotkey...
    OptHKeyEnableDismount:      Boolean;
    OptHKeyKeyDismount:         TShortCut;
    OptHKeyEnableDismountEmerg: Boolean;
    OptHKeyKeyDismountEmerg:    TShortCut;

    constructor Create(); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    function RegistryKey(): String; OVERRIDE;

  end;


var
  // Global variable
  gSettings: TMainSettings;

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
  OPT_DISPLAYTOOLBAR              = 'DisplayToolbar';
  DFLT_OPT_DISPLAYTOOLBAR         = True;
  OPT_DISPLAYTOOLBARLARGE         = 'DisplayToolbarLarge';
  DFLT_OPT_DISPLAYTOOLBARLARGE    = True;
  OPT_DISPLAYTOOLBARCAPTIONS      = 'DisplayToolbarCaptions';
  DFLT_OPT_DISPLAYTOOLBARCAPTIONS = True;
  OPT_DISPLAYSTATUSBAR            = 'DisplayStatusbar';
  DFLT_OPT_DISPLAYSTATUSBAR       = True;
  OPT_ALLOWMULTIPLEINSTANCES      = 'AllowMultipleInstances';
  DFLT_OPT_ALLOWMULTIPLEINSTANCES = False;
  OPT_AUTOSTARTPORTABLE           = 'AutoStartPortable';
  DFLT_OPT_AUTOSTARTPORTABLE      = False;
  OPT_DEFAULTMOUNTAS              = 'DefaultMountAs';
  DFLT_OPT_DEFAULTMOUNTAS         = fomaRemovableDisk;
//  OPT_OPTINSTALLED                = 'Installed';
//  DFLT_OPT_OPTINSTALLED           = False;
  OPT_APPRUNNING                 = 'AppRunning';
  DFLT_OPT_APPRUNNING            = arDontKnow;

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
  Result := LoadResString(OnExitWhenMountedTitlePtr[Value]);
end;

function OnExitWhenPortableModeTitle(Value: eOnExitWhenPortableMode): String;
begin
  Result := LoadResString(OnExitWhenPortableModeTitlePtr[Value]);
end;

function OnNormalDismountFailTitle(Value: eOnNormalDismountFail): String;
begin
  Result := LoadResString(OnNormalDismountFailTitlePtr[Value]);
end;

function SystemTrayClickActionTitle(Value: TSystemTrayClickAction): String;
begin
  Result := LoadResString(SystemTrayClickActionTitlePtr[Value]);
end;


constructor TMainSettings.Create();
begin
  inherited;

  Load();

end;

destructor TMainSettings.Destroy();
begin

  inherited;
end;

procedure TMainSettings._Load(iniFile: TCustomINIFile);
begin
  inherited _Load(iniFile);
  // todo -otdk : combine load and save, and/or use rtti
  OptDisplayToolbar         := iniFile.ReadBool(SECTION_GENERAL, OPT_DISPLAYTOOLBAR,
    DFLT_OPT_DISPLAYTOOLBAR);
  OptDisplayToolbarLarge    := iniFile.ReadBool(SECTION_GENERAL,
    OPT_DISPLAYTOOLBARLARGE, DFLT_OPT_DISPLAYTOOLBARLARGE);
  OptDisplayToolbarCaptions := iniFile.ReadBool(SECTION_GENERAL,
    OPT_DISPLAYTOOLBARCAPTIONS, DFLT_OPT_DISPLAYTOOLBARCAPTIONS);
  OptDisplayStatusbar       := iniFile.ReadBool(SECTION_GENERAL,
    OPT_DISPLAYSTATUSBAR, DFLT_OPT_DISPLAYSTATUSBAR);
  OptAllowMultipleInstances := iniFile.ReadBool(SECTION_GENERAL,
    OPT_ALLOWMULTIPLEINSTANCES, DFLT_OPT_ALLOWMULTIPLEINSTANCES);
  OptAutoStartPortable      := iniFile.ReadBool(SECTION_GENERAL, OPT_AUTOSTARTPORTABLE,
    DFLT_OPT_AUTOSTARTPORTABLE);
  OptDefaultMountAs         := TFreeOTFEMountAs(iniFile.ReadInteger(SECTION_GENERAL,
    OPT_DEFAULTMOUNTAS, Ord(DFLT_OPT_DEFAULTMOUNTAS)));
  feAppRunning      :=
    eAppRunning(iniFile.ReadInteger(SECTION_GENERAL,
    OPT_APPRUNNING, Ord(DFLT_OPT_APPRUNNING)));

  OptWarnBeforeForcedDismount := iniFile.ReadBool(SECTION_CONFIRMATION,
    OPT_WARNBEFOREFORCEDISMOUNT, DFLT_OPT_WARNBEFOREFORCEDISMOUNT);
  OptOnExitWhenMounted        :=
    eOnExitWhenMounted(iniFile.ReadInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED,
    Ord(DFLT_OPT_ONEXITWHENMOUNTED)));
  OptOnExitWhenPortableMode   :=
    eOnExitWhenPortableMode(iniFile.ReadInteger(SECTION_CONFIRMATION,
    OPT_ONEXITWHENPORTABLEMODE, Ord(DFLT_OPT_ONEXITWHENPORTABLEMODE)));
  OptOnNormalDismountFail     :=
    eOnNormalDismountFail(iniFile.ReadInteger(SECTION_CONFIRMATION,
    OPT_ONNORMALDISMOUNTFAIL, Ord(DFLT_OPT_ONNORMALDISMOUNTFAIL)));

  OptSystemTrayIconDisplay           :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY,
    DFLT_OPT_SYSTEMTRAYICONDISPLAY);
  OptSystemTrayIconMinTo             :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,
    DFLT_OPT_SYSTEMTRAYICONMINTO);
  OptSystemTrayIconCloseTo           :=
    iniFile.ReadBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO,
    DFLT_OPT_SYSTEMTRAYICONCLOSETO);
  OptSystemTrayIconActionSingleClick :=
    TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON,
    OPT_SYSTEMTRAYICONACTIONSINGLECLICK, Ord(DFLT_OPT_SYSTEMTRAYICONACTIONSINGLECLICK)));
  OptSystemTrayIconActionDoubleClick :=
    TSystemTrayClickAction(iniFile.ReadInteger(SECTION_SYSTEMTRAYICON,
    OPT_SYSTEMTRAYICONACTIONDOUBLECLICK, Ord(DFLT_OPT_SYSTEMTRAYICONACTIONDOUBLECLICK)));

  OptHKeyEnableDismount      := iniFile.ReadBool(SECTION_HOTKEY,
    OPT_HOTKEYENABLEDISMOUNT, DFLT_OPT_HOTKEYENABLEDISMOUNT);
  OptHKeyKeyDismount         := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY,
    OPT_HOTKEYKEYDISMOUNT, DFLT_OPT_HOTKEYKEYDISMOUNT));
  OptHKeyEnableDismountEmerg := iniFile.ReadBool(SECTION_HOTKEY,
    OPT_HOTKEYENABLEDISMOUNTEMERG, DFLT_OPT_HOTKEYENABLEDISMOUNTEMERG);
  OptHKeyKeyDismountEmerg    := TextToShortCut(iniFile.ReadString(SECTION_HOTKEY,
    OPT_HOTKEYKEYDISMOUNTEMERG, DFLT_OPT_HOTKEYKEYDISMOUNTEMERG));

end;


function TMainSettings._Save(iniFile: TCustomINIFile): Boolean;
begin
  Result := inherited _Save(iniFile);
  if Result then begin
    try
      iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBAR, OptDisplayToolbar);
      iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARLARGE,
        OptDisplayToolbarLarge);
      iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYTOOLBARCAPTIONS,
        OptDisplayToolbarCaptions);
      iniFile.WriteBool(SECTION_GENERAL, OPT_DISPLAYSTATUSBAR,
        OptDisplayStatusbar);
      iniFile.WriteBool(SECTION_GENERAL, OPT_ALLOWMULTIPLEINSTANCES,
        OptAllowMultipleInstances);
      iniFile.WriteBool(SECTION_GENERAL, OPT_AUTOSTARTPORTABLE,
        OptAutoStartPortable);
      iniFile.WriteInteger(SECTION_GENERAL, OPT_DEFAULTMOUNTAS,
        Ord(OptDefaultMountAs));
     iniFile.WriteInteger(SECTION_GENERAL, OPT_APPRUNNING,
        Ord(feAppRunning));

      iniFile.WriteBool(SECTION_CONFIRMATION, OPT_WARNBEFOREFORCEDISMOUNT,
        OptWarnBeforeForcedDismount);
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENMOUNTED,
        Ord(OptOnExitWhenMounted));
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONEXITWHENPORTABLEMODE,
        Ord(OptOnExitWhenPortableMode));
      iniFile.WriteInteger(SECTION_CONFIRMATION, OPT_ONNORMALDISMOUNTFAIL,
        Ord(OptOnNormalDismountFail));




      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONDISPLAY,
        OptSystemTrayIconDisplay);
      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONMINTO,
        OptSystemTrayIconMinTo);
      iniFile.WriteBool(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONCLOSETO,
        OptSystemTrayIconCloseTo);
      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONSINGLECLICK,
        Ord(OptSystemTrayIconActionSingleClick));
      iniFile.WriteInteger(SECTION_SYSTEMTRAYICON, OPT_SYSTEMTRAYICONACTIONDOUBLECLICK,
        Ord(OptSystemTrayIconActionDoubleClick));

      iniFile.WriteBool(SECTION_HOTKEY, OPT_HOTKEYENABLEDISMOUNT,
        OptHKeyEnableDismount);
      iniFile.WriteString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNT,
        ShortCutToText(OptHKeyKeyDismount));
      iniFile.WriteBool(SECTION_HOTKEY, OPT_HOTKEYENABLEDISMOUNTEMERG,
        OptHKeyEnableDismountEmerg);
      iniFile.WriteString(SECTION_HOTKEY, OPT_HOTKEYKEYDISMOUNTEMERG,
        ShortCutToText(OptHKeyKeyDismountEmerg));

    except
      on E: Exception do begin
        Result := False;
      end;
    end;
  end;

end;

function TMainSettings.RegistryKey(): String;
begin
  Result := FREEOTFE_REGISTRY_SETTINGS_LOCATION;
end;

end.

