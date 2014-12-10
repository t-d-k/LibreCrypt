unit FreeOTFEExplorerSettings;
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
  ComCtrls,
  CommonSettings,
  IniFiles,
  Shredder;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  FREEOTFE_REGISTRY_SETTINGS_LOCATION = '\Software\FreeOTFEExplorer';

type
  TDefaultStoreOp = (dsoPrompt, dsoCopy, dsoMove);

resourcestring
  DEFAULT_STORE_OP_PROMPT = 'Prompt user';
  DEFAULT_STORE_OP_COPY   = 'Copy';
  DEFAULT_STORE_OP_MOVE   = 'Move';

const
  DefaultStoreOpTitlePtr: array [TDefaultStoreOp] of Pointer =
    (@DEFAULT_STORE_OP_PROMPT, @DEFAULT_STORE_OP_COPY, @DEFAULT_STORE_OP_MOVE
    );

type
  TMoveDeletionMethod = (mdmPrompt, mdmDelete, mdmOverwrite);

resourcestring
  MOVE_DELETION_METHOD_PROMPT    = 'Prompt user';
  MOVE_DELETION_METHOD_DELETE    = 'Delete original';
  MOVE_DELETION_METHOD_OVERWRITE = 'Overwrite original';

const
  MoveDeletionMethodTitlePtr: array [TMoveDeletionMethod] of Pointer =
    (@MOVE_DELETION_METHOD_PROMPT, @MOVE_DELETION_METHOD_DELETE,
    @MOVE_DELETION_METHOD_OVERWRITE
    );

type
  TExplorerBarType = (ebNone, ebFolders);

  TFreeOTFEExplorerSettings = class (TSettings)
  PROTECTED
    procedure _Load(iniFile: TCustomINIFile); OVERRIDE;
    function _Save(iniFile: TCustomINIFile): Boolean; OVERRIDE;

  PUBLIC
    // General...
    OptShowHiddenItems:                  Boolean;
    OptHideKnownFileExtns:               Boolean;
    OptDefaultStoreOp:                   TDefaultStoreOp;
    OptMoveDeletionMethod:               TMoveDeletionMethod;
    OptOverwriteMethod:                  TShredMethod;
    OptOverwritePasses:                  Integer;
    OptPreserveTimestampsOnStoreExtract: Boolean;

                                         // Layout...
                                         // i.e. All items *not* configured via the "Options" dialog, but by
                                         //      controls on the main window
    FlagClearLayoutOnSave:      Boolean; // If set, and not storing layout, any
                                         // existing layout will be deleted -
                                         // resulting in a reset to default after
                                         // restarting
    OptStoreLayout:             Boolean;
    OptShowToolbarVolume:       Boolean;
    OptShowToolbarExplorer:     Boolean;
    OptToolbarVolumeLarge:      Boolean;
    OptToolbarVolumeCaptions:   Boolean;
    OptToolbarExplorerCaptions: Boolean;
    OptToolbarExplorerLarge:    Boolean;
    OptShowAddressBar:          Boolean;
    OptShowExplorerBar:         TExplorerBarType;
    OptShowStatusBar:           Boolean;
    OptMainWindowLayout:        String;
    OptExplorerBarWidth:        Integer;  // Set to 0 to indicate hidden
    OptListViewLayout:          String;

    // WebDAV related...
    OptWebDAVEnableServer:             Boolean;
    OptOverwriteWebDAVCacheOnDismount: Boolean;
    OptWebDAVPort:                     Integer;
    OptWebDavShareName:                String;
    OptWebDavLogDebug:                 String;
    OptWebDavLogAccess:                String;

    constructor Create(); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    function RegistryKey(): String; OVERRIDE;

  end;

function DefaultStoreOpTitle(defaultStoreOp: TDefaultStoreOp): String;
function MoveDeletionMethodTitle(moveDeletionMethod: TMoveDeletionMethod): String;

var
  // Global variable
  Settings: TFreeOTFEExplorerSettings;

implementation

uses
  Windows,   // Required to get rid of compiler hint re DeleteFile
  SysUtils,  // Required for ChangeFileExt, DeleteFile
  Dialogs,
  Menus, Registry,
  SDUDialogs,
  SDUGeneral,
  SDUi18n,
           // Required for ShortCutToText and TextToShortCut
  ShlObj;  // Required for CSIDL_PERSONAL

const
  SETTINGS_V1 = 1;
  SETTINGS_V2 = 2;

  // -- General section --
  OPT_SHOWHIDDENITEMS                       = 'ShowHiddenItems';
  DFLT_OPT_SHOWHIDDENITEMS                  = False;
  // Default as per MS Windows Explorer default
  OPT_HIDEKNOWNFILEEXTNS                    = 'HideKnownFileExtns';
  DFLT_OPT_HIDEKNOWNFILEEXTNS               = True;
  // Default as per MS Windows Explorer default
  OPT_DEFAULTSTOREOP                        = 'DefaultStoreOp';
  DFLT_OPT_DEFAULTSTOREOP                   = dsoPrompt;
  OPT_MOVEDELETIONMETHOD                    = 'MoveDeletionMethod';
  DFLT_OPT_MOVEDELETIONMETHOD               = mdmDelete;
  OPT_OVERWRITEMETHOD                       = 'OverwriteMethod';
  DFLT_OPT_OVERWRITEMETHOD                  = smPseudorandom;
  OPT_OVERWRITEPASSES                       = 'OverwritePasses';
  DFLT_OPT_OVERWRITEPASSES                  = 1;
  OPT_PRESERVETIMESTAMPSONSTOREEXTRACT      = 'PreserveTimestampsOnStoreExtract';
  DFLT_OPT_PRESERVETIMESTAMPSONSTOREEXTRACT = True;

  // -- Layout section --
  SECTION_LAYOUT                   = 'Layout';
  OPT_STORELAYOUT                  = 'StoreLayout';
  DFLT_OPT_STORELAYOUT             = True;
  OPT_MAINWINDOWLAYOUT             = 'MainWindowLayout';
  DFLT_OPT_MAINWINDOWLAYOUT        = '';
  OPT_EXPLORERBARWIDTH             = 'ExplorerBarWidth';
  DFLT_OPT_EXPLORERBARWIDTH        = 215; // Set to 0 to hide
  OPT_SHOWTOOLBARVOLUME            = 'ShowToolbarVolume';
  DFLT_OPT_SHOWTOOLBARVOLUME       = True;
  OPT_SHOWTOOLBAREXPLORER          = 'ShowToolbarExplorer';
  DFLT_OPT_SHOWTOOLBAREXPLORER     = True;
  OPT_TOOLBARVOLUMELARGE           = 'ToolbarVolumeLarge';
  DFLT_OPT_TOOLBARVOLUMELARGE      = True;
  OPT_TOOLBARVOLUMECAPTIONS        = 'ToolbarVolumeCaptions';
  DFLT_OPT_TOOLBARVOLUMECAPTIONS   = True;
  OPT_TOOLBAREXPLORERLARGE         = 'ToolbarExplorerLarge';
  DFLT_OPT_TOOLBAREXPLORERLARGE    = True;
  OPT_TOOLBAREXPLORERCAPTIONS      = 'ToolbarExplorerCaptions';
  DFLT_OPT_TOOLBAREXPLORERCAPTIONS = True;
  // Not the same as MS Windows Explorer, but makes
  // the store/extract operations more obvious
  OPT_SHOWADDRESSBAR               = 'ShowAddressBar';
  DFLT_OPT_SHOWADDRESSBAR          = True;
  OPT_SHOWEXPLORERBAR              = 'ShowExplorerBar';
  DFLT_OPT_SHOWEXPLORERBAR         = ebFolders;
  OPT_SHOWSTATUSBAR                = 'ShowStatusBar';
  DFLT_OPT_SHOWSTATUSBAR           = True;
  OPT_LISTVIEWLAYOUT               = 'ListViewLayout';
  DFLT_OPT_LISTVIEWLAYOUT          = '';

  // -- WebDAV section --
  SECTION_WEBDAV                    = 'WedDAV';
  OPT_ENABLESERVER                  = 'EnableServer';
  DFLT_OPT_ENABLESERVER             = False;
  OPT_OVERWRITECACHEONDISMOUNT      = 'OverwriteCacheOnDismount';
  DFLT_OPT_OVERWRITECACHEONDISMOUNT = True;
  OPT_PORT                          = 'Port';
  DFLT_OPT_PORT                     = 80;
  OPT_SHARENAME                     = 'ShareName';
  DFLT_OPT_SHARENAME                = 'FEXPL';
  OPT_LOGDEBUG                      = 'LogDebug';
  DFLT_OPT_LOGDEBUG                 = '';
  OPT_LOGACCESS                     = 'LogAccess';
  DFLT_OPT_LOGACCESS                = '';


function DefaultStoreOpTitle(defaultStoreOp: TDefaultStoreOp): String;
begin
  Result := LoadResString(DefaultStoreOpTitlePtr[defaultStoreOp]);
end;

function MoveDeletionMethodTitle(moveDeletionMethod: TMoveDeletionMethod): String;
begin
  Result := LoadResString(MoveDeletionMethodTitlePtr[moveDeletionMethod]);
end;

constructor TFreeOTFEExplorerSettings.Create();
begin
  inherited;

  FlagClearLayoutOnSave := False;

  Load();

end;

destructor TFreeOTFEExplorerSettings.Destroy();
begin

  inherited;
end;

procedure TFreeOTFEExplorerSettings._Load(iniFile: TCustomINIFile);
begin
  inherited _Load(iniFile);

  // General section...
  OptShowHiddenItems                  :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_SHOWHIDDENITEMS, DFLT_OPT_SHOWHIDDENITEMS);
  OptHideKnownFileExtns               :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_HIDEKNOWNFILEEXTNS, DFLT_OPT_HIDEKNOWNFILEEXTNS);
  OptDefaultStoreOp                   :=
    TDefaultStoreOp(iniFile.ReadInteger(SECTION_GENERAL, OPT_DEFAULTSTOREOP,
    Ord(DFLT_OPT_DEFAULTSTOREOP)));
  OptMoveDeletionMethod               :=
    TMoveDeletionMethod(iniFile.ReadInteger(SECTION_GENERAL, OPT_MOVEDELETIONMETHOD,
    Ord(DFLT_OPT_MOVEDELETIONMETHOD)));
  OptOverwriteMethod                  :=
    TShredMethod(iniFile.ReadInteger(SECTION_GENERAL, OPT_OVERWRITEMETHOD,
    Ord(DFLT_OPT_OVERWRITEMETHOD)));
  OptOverwritePasses                  :=
    iniFile.ReadInteger(SECTION_GENERAL, OPT_OVERWRITEPASSES, DFLT_OPT_OVERWRITEPASSES);
  OptPreserveTimestampsOnStoreExtract :=
    iniFile.ReadBool(SECTION_GENERAL, OPT_PRESERVETIMESTAMPSONSTOREEXTRACT,
    DFLT_OPT_PRESERVETIMESTAMPSONSTOREEXTRACT);

  // Layout section...
  OptStoreLayout             :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_STORELAYOUT, DFLT_OPT_STORELAYOUT);
  OptMainWindowLayout        :=
    iniFile.ReadString(SECTION_LAYOUT, OPT_MAINWINDOWLAYOUT, DFLT_OPT_MAINWINDOWLAYOUT);
  OptShowToolbarVolume       :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_SHOWTOOLBARVOLUME, DFLT_OPT_SHOWTOOLBARVOLUME);
  OptShowToolbarExplorer     :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_SHOWTOOLBAREXPLORER, DFLT_OPT_SHOWTOOLBAREXPLORER);
  OptToolbarVolumeLarge      :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_TOOLBARVOLUMELARGE, DFLT_OPT_TOOLBARVOLUMELARGE);
  OptToolbarVolumeCaptions   :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_TOOLBARVOLUMECAPTIONS, DFLT_OPT_TOOLBARVOLUMECAPTIONS);
  OptToolbarExplorerLarge    :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_TOOLBAREXPLORERLARGE, DFLT_OPT_TOOLBAREXPLORERLARGE);
  OptToolbarExplorerCaptions :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_TOOLBAREXPLORERCAPTIONS,
    DFLT_OPT_TOOLBAREXPLORERCAPTIONS);
  OptExplorerBarWidth        :=
    iniFile.ReadInteger(SECTION_LAYOUT, OPT_EXPLORERBARWIDTH, DFLT_OPT_EXPLORERBARWIDTH);
  OptShowAddressBar          :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_SHOWADDRESSBAR, DFLT_OPT_SHOWADDRESSBAR);
  OptShowExplorerBar         :=
    TExplorerBarType(iniFile.ReadInteger(SECTION_LAYOUT, OPT_SHOWEXPLORERBAR,
    Ord(DFLT_OPT_SHOWEXPLORERBAR)));
  OptShowStatusBar           :=
    iniFile.ReadBool(SECTION_LAYOUT, OPT_SHOWSTATUSBAR, DFLT_OPT_SHOWSTATUSBAR);
  OptListViewLayout          :=
    iniFile.ReadString(SECTION_LAYOUT, OPT_LISTVIEWLAYOUT, DFLT_OPT_LISTVIEWLAYOUT);

  // WebDAV section...
  OptWebDAVEnableServer             :=
    iniFile.ReadBool(SECTION_WEBDAV, OPT_ENABLESERVER, DFLT_OPT_ENABLESERVER);
  OptOverwriteWebDAVCacheOnDismount :=
    iniFile.ReadBool(SECTION_WEBDAV, OPT_OVERWRITECACHEONDISMOUNT,
    DFLT_OPT_OVERWRITECACHEONDISMOUNT);
  OptWebDAVPort                     :=
    iniFile.ReadInteger(SECTION_WEBDAV, OPT_PORT, DFLT_OPT_PORT);
  OptWebDavShareName                :=
    iniFile.ReadString(SECTION_WEBDAV, OPT_SHARENAME, DFLT_OPT_SHARENAME);
  OptWebDavLogDebug                 :=
    iniFile.ReadString(SECTION_WEBDAV, OPT_LOGDEBUG, DFLT_OPT_LOGDEBUG);
  OptWebDavLogAccess                :=
    iniFile.ReadString(SECTION_WEBDAV, OPT_LOGACCESS, DFLT_OPT_LOGACCESS);

end;


function TFreeOTFEExplorerSettings._Save(iniFile: TCustomINIFile): Boolean;
var
  allOK: Boolean;
begin
  allOK := inherited _Save(iniFile);

  if allOK then begin
    try
      // General section...
      iniFile.WriteBool(SECTION_GENERAL, OPT_SHOWHIDDENITEMS,
        OptShowHiddenItems);
      iniFile.WriteBool(SECTION_GENERAL, OPT_HIDEKNOWNFILEEXTNS,
        OptHideKnownFileExtns);
      iniFile.WriteInteger(SECTION_GENERAL, OPT_DEFAULTSTOREOP,
        Ord(OptDefaultStoreOp));
      iniFile.WriteInteger(SECTION_GENERAL, OPT_MOVEDELETIONMETHOD,
        Ord(OptMoveDeletionMethod));
      iniFile.WriteInteger(SECTION_GENERAL, OPT_OVERWRITEMETHOD,
        Ord(OptOverwriteMethod));
      iniFile.WriteInteger(SECTION_GENERAL, OPT_OVERWRITEPASSES,
        OptOverwritePasses);
      iniFile.WriteBool(SECTION_GENERAL, OPT_PRESERVETIMESTAMPSONSTOREEXTRACT,
        OptPreserveTimestampsOnStoreExtract);

      // Layout section...
      iniFile.WriteBool(SECTION_LAYOUT, OPT_STORELAYOUT,
        OptStoreLayout);
      if OptStoreLayout then begin
        iniFile.WriteString(SECTION_LAYOUT, OPT_MAINWINDOWLAYOUT,
          OptMainWindowLayout);
        iniFile.WriteInteger(SECTION_LAYOUT, OPT_EXPLORERBARWIDTH,
          OptExplorerBarWidth);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_SHOWTOOLBARVOLUME,
          OptShowToolbarVolume);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_SHOWTOOLBAREXPLORER,
          OptShowToolbarExplorer);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_TOOLBARVOLUMELARGE,
          OptToolbarVolumeLarge);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_TOOLBARVOLUMECAPTIONS,
          OptToolbarVolumeCaptions);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_TOOLBAREXPLORERLARGE,
          OptToolbarExplorerLarge);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_TOOLBAREXPLORERCAPTIONS,
          OptToolbarExplorerCaptions);
        iniFile.WriteBool(SECTION_LAYOUT, OPT_SHOWADDRESSBAR,
          OptShowAddressBar);
        iniFile.WriteInteger(SECTION_LAYOUT, OPT_SHOWEXPLORERBAR,
          Ord(OptShowExplorerBar));
        iniFile.WriteBool(SECTION_LAYOUT, OPT_SHOWSTATUSBAR,
          OptShowStatusBar);
        iniFile.WriteString(SECTION_LAYOUT, OPT_LISTVIEWLAYOUT,
          OptListViewLayout);
      end else
      if FlagClearLayoutOnSave then begin
        // Purge all layout related - next time the application is started,
        // it'll just assume the defaults
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_MAINWINDOWLAYOUT);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_EXPLORERBARWIDTH);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_SHOWTOOLBARVOLUME);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_SHOWTOOLBAREXPLORER);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_TOOLBARVOLUMELARGE);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_TOOLBARVOLUMECAPTIONS);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_TOOLBAREXPLORERLARGE);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_TOOLBAREXPLORERCAPTIONS);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_SHOWADDRESSBAR);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_SHOWEXPLORERBAR);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_SHOWSTATUSBAR);
        iniFile.DeleteKey(SECTION_LAYOUT, OPT_LISTVIEWLAYOUT);
      end;

      // WebDAV section...
      iniFile.WriteBool(SECTION_WEBDAV, OPT_ENABLESERVER, OptWebDAVEnableServer);
      iniFile.WriteBool(SECTION_WEBDAV, OPT_OVERWRITECACHEONDISMOUNT,
        OptOverwriteWebDAVCacheOnDismount);
      iniFile.WriteInteger(SECTION_WEBDAV, OPT_PORT, OptWebDAVPort);
      iniFile.WriteString(SECTION_WEBDAV, OPT_SHARENAME, OptWebDavShareName);
      iniFile.WriteString(SECTION_WEBDAV, OPT_LOGDEBUG, OptWebDavLogDebug);
      iniFile.WriteString(SECTION_WEBDAV, OPT_LOGACCESS, OptWebDavLogAccess);

    except
      on E: Exception do begin
        allOK := False;
      end;
    end;

  end;

  Result := allOK;
end;

function TFreeOTFEExplorerSettings.RegistryKey(): String;
begin
  Result := FREEOTFE_REGISTRY_SETTINGS_LOCATION;
end;

end.
