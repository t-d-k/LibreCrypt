unit FreeOTFEfrmMain;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  ActnList,
  Buttons, Classes, ComCtrls, CommonfrmMain, CommonSettings, Controls, Dialogs,
  ExtCtrls, Forms, FreeOTFESettings, Graphics, Grids, ImgList, Menus, Messages,
  //sdu
  SDUSystemTrayIcon, Shredder, Spin64, StdCtrls, SysUtils, ToolWin, Windows, XPMan,

  //doxbox/freeotfe
  OTFE_U,
  OTFEFreeOTFE_DriverControl,
  OTFEFreeOTFE_U, OTFEFreeOTFEBase_U,
  pkcs11_library, SDUDialogs, SDUForms, SDUGeneral, SDUMRUList, SDUMultimediaKeys;

type
  TPortableModeAction = (pmaStart, pmaStop, pmaToggle);

  TfrmFreeOTFEMain = class (TfrmMain)
    ToolBar1:                  TToolBar;
    pnlTopSpacing:             TPanel;
    ilDriveIcons:              TImageList;
    lvDrives:                  TListView;
    pmDrives:                  TPopupMenu;
    miPopupDismount:           TMenuItem;
    N2:                        TMenuItem;
    miPopupProperties:         TMenuItem;
    N3:                        TMenuItem;
    miProperties:              TMenuItem;
    N1:                        TMenuItem;
    miDismountAllMain:         TMenuItem;
    miPopupDismountAll:        TMenuItem;
    miFreeOTFEDrivers:         TMenuItem;
    miOverwriteFreeSpace:      TMenuItem;
    miFormat:                  TMenuItem;
    N6:                        TMenuItem;
    miPopupFormat:             TMenuItem;
    miPopupOverwriteFreeSpace: TMenuItem;

    N7:                         TMenuItem;
    miPortableModeDrivers:      TMenuItem;
    N8:                         TMenuItem;
    actDismountAll:             TAction;
    actProperties:              TAction;
    pmSystemTray:               TPopupMenu;
    open1:                      TMenuItem;
    open2:                      TMenuItem;
    mountfile1:                 TMenuItem;
    mountpartition1:            TMenuItem;
    dismountall1:               TMenuItem;
    N10:                        TMenuItem;
    miOptions:                  TMenuItem;
    actConsoleDisplay:          TAction;
    N11:                        TMenuItem;
    N12:                        TMenuItem;
    SDUSystemTrayIcon1:         TSDUSystemTrayIcon;
    tbbNew:                     TToolButton;
    tbbMountFile:               TToolButton;
    tbbMountPartition:          TToolButton;
    tbbDismount:                TToolButton;
    tbbDismountAll:             TToolButton;
    actTogglePortableMode:      TAction;
    actFormat:                  TAction;
    actOverwriteFreeSpace:      TAction;
    tbbTogglePortableMode:      TToolButton;
    actDrivers:                 TAction;
    ilDriveIconOverlay:         TImageList;
    actOverwriteEntireDrive:    TAction;
    Overwriteentiredrive1:      TMenuItem;
    Overwriteentiredrive2:      TMenuItem;
    N17:                        TMenuItem;
    mmRecentlyMounted:          TMenuItem;
    actFreeOTFEMountPartition:  TAction;
    actLinuxMountPartition:     TAction;
    miFreeOTFEMountPartition:   TMenuItem;
    miLinuxMountPartition:      TMenuItem;
    actConsoleHide:             TAction;
    actInstall:                 TAction;
    InstallDoxBox1:             TMenuItem;
    actTestModeOn:              TAction;
    SetTestMode1:               TMenuItem;
    actTestModeOff:             TAction;
    DisallowTestsigneddrivers1: TMenuItem;
    tbbSettings:                TToolButton;

    procedure actDriversExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure lvDrivesResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvDrivesClick(Sender: TObject);
    procedure lvDrivesDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actDismountExecute(Sender: TObject);
    procedure actDismountAllExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actConsoleDisplayExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actTogglePortableModeExecute(Sender: TObject);
    procedure lvDrivesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure actFormatExecute(Sender: TObject);
    procedure actOverwriteFreeSpaceExecute(Sender: TObject);
    procedure tbbTogglePortableModeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actOverwriteEntireDriveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actFreeOTFEMountPartitionExecute(Sender: TObject);
    procedure actLinuxMountPartitionExecute(Sender: TObject);
    procedure actFreeOTFENewExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure SDUSystemTrayIcon1DblClick(Sender: TObject);
    procedure SDUSystemTrayIcon1Click(Sender: TObject);
    procedure actConsoleHideExecute(Sender: TObject);
    procedure actInstallExecute(Sender: TObject);
    procedure actTestModeOnExecute(Sender: TObject);
    procedure actTestModeOffExecute(Sender: TObject);
    procedure miToolsClick(Sender: TObject);
  PRIVATE
    function IsTestModeOn: Boolean;

  PROTECTED
    ftempCypherDriver:     Ansistring;
    ftempCypherGUID:       TGUID;
    ftempCypherDetails:    TFreeOTFECypher_v3;
    ftempCypherKey:        TSDUBytes;
    ftempCypherEncBlockNo: Int64;

    fIconMounted:   TIcon;
    fIconUnmounted: TIcon;

    // We cache this information as it take a *relativly* long time to get it
    // from the OTFE component.
    fcountPortableDrivers: Integer;

    fallowUACEsclation: Boolean;

    function HandleCommandLineOpts_Portable(): eCmdLine_Exit;
    function HandleCommandLineOpts_DriverControl(): eCmdLine_Exit;
    function HandleCommandLineOpts_DriverControl_GUI(): eCmdLine_Exit;
    function HandleCommandLineOpts_DriverControl_Install(driverControlObj:
      TOTFEFreeOTFEDriverControl): eCmdLine_Exit;
    function HandleCommandLineOpts_DriverControl_Uninstall(driverControlObj:
      TOTFEFreeOTFEDriverControl): eCmdLine_Exit;
    function HandleCommandLineOpts_DriverControl_Count(driverControlObj:
      TOTFEFreeOTFEDriverControl): Integer;
    function HandleCommandLineOpts_Count(): eCmdLine_Exit;
    function HandleCommandLineOpts_Create(): eCmdLine_Exit; OVERRIDE;
    function HandleCommandLineOpts_Dismount(): eCmdLine_Exit;
    function HandleCommandLineOpts_SetTestMode(): eCmdLine_Exit;
    function HandleCommandLineOpts_SetInstalled(): eCmdLine_Exit;

    //true = set OK
    function SetTestMode(silent, SetOn: Boolean): Boolean;
    //sets 'installed' flag in ini file - returns false if fails (ini file must exist or be set as custom)
    function SetInstalled(): Boolean;
    function InstallAllDrivers(driverControlObj: TOTFEFreeOTFEDriverControl;
      silent: Boolean): eCmdLine_Exit;

    procedure ReloadSettings(); OVERRIDE;

    procedure SetupOTFEComponent(); OVERRIDE;
    function ActivateFreeOTFEComponent(suppressMsgs: Boolean): Boolean; OVERRIDE;
    procedure DeactivateFreeOTFEComponent(); OVERRIDE;
    procedure ShowOldDriverWarnings(); OVERRIDE;

    procedure PKCS11TokenRemoved(SlotID: Integer); OVERRIDE;

    procedure RefreshMRUList(); OVERRIDE;
    procedure MRUListItemClicked(mruList: TSDUMRUList; idx: Integer); OVERRIDE;

    procedure InitializeDrivesDisplay();
    procedure RefreshDrives();
    function DetermineDriveOverlay(volumeInfo: TOTFEFreeOTFEVolumeInfo): Integer;
    function AddIconForDrive(driveLetter: DriveLetterChar; overlayIdx: Integer): Integer;

    procedure SetStatusBarTextNormal();
    procedure EnableDisableControls(); OVERRIDE;
    function GetDriveLetterFromLVItem(listItem: TListItem): DriveLetterChar;

    procedure ResizeWindow();

    function GetSelectedDrives(): DriveLetterString;
    procedure OverwriteDrives(drives: DriveLetterString; overwriteEntireDrive: Boolean);

    procedure MountFiles(mountAsSystem: TDragDropFileType; filenames: TStringList;
      ReadOnly, forceHidden: Boolean); OVERLOAD; OVERRIDE;

    procedure DriveProperties();

    function DismountSelected(): Boolean;
    function DismountAll(isEmergency: Boolean = False): Boolean;
    function DismountDrives(dismountDrives: DriveLetterString; isEmergency: Boolean): Boolean;
    procedure ReportDrivesNotDismounted(drivesRemaining: DriveLetterString; isEmergency: Boolean);

    procedure GetAllDriversUnderCWD(driverFilenames: TStringList);

    function PortableModeSet(setTo: TPortableModeAction; suppressMsgs: Boolean): Boolean;
    function _PortableModeStart(suppressMsgs: Boolean): Boolean;
    function _PortableModeStop(suppressMsgs: Boolean): Boolean;
    function _PortableModeToggle(suppressMsgs: Boolean): Boolean;

    // System tray icon related...
    procedure SystemTrayIconDismount(Sender: TObject);
    procedure DestroySysTrayIconMenuitems();

    // Hotkey related...
    procedure SetupHotKeys();
    procedure EnableHotkey(hotKey: TShortCut; hotKeyIdent: Integer);
    procedure DisableHotkey(hotKeyIdent: Integer);

    procedure RecaptionToolbarAndMenuIcons(); OVERRIDE;
    procedure SetIconListsAndIndexes(); OVERRIDE;
    procedure SetupToolbarFromSettings(); OVERRIDE;

    procedure DoSystemTrayAction(Sender: TObject; doAction: TSystemTrayClickAction);

    procedure UACEscalateForDriverInstallation();
    procedure UACEscalateForPortableMode(portableAction: TPortableModeAction;
      suppressMsgs: Boolean);
    procedure UACEscalate(cmdLineParams: String; suppressMsgs: Boolean);

    function DoTests: Boolean; OVERRIDE;

  PUBLIC
    // This next line will generate a compiler warning - this is harmless.
    // (We want to use OTFEFreeOTFE as the descendant class in this unit)
    //    function OTFEFreeOTFE(): TOTFEFreeOTFE; OVERLOAD;

    procedure WMDropFiles(var Msg: TWMDropFiles); MESSAGE WM_DROPFILES;
    procedure WMDeviceChange(var Msg: TMessage); MESSAGE WM_DEVICECHANGE;
    procedure WMQueryEndSession(var msg: TWMQueryEndSession); MESSAGE WM_QUERYENDSESSION;
    procedure WMEndSession(var msg: TWMEndSession); MESSAGE WM_ENDSESSION;

    procedure GenerateOverwriteData(Sender: TObject; passNumber: Integer;
      bytesRequired: Cardinal; var generatedOK: Boolean; var outputBlock: TShredBlock);

    procedure InitApp(); OVERRIDE;

    function MessageHook(var msg: TMessage): Boolean;

    procedure DisplayDriverControlDlg();

    // Handle any command line options; returns TRUE if command line options
    // were passed through
    function HandleCommandLineOpts(out cmdExitCode: eCmdLine_Exit): Boolean; OVERRIDE;

  end;



const
  // Command line parameter handled in the .dpr
  CMDLINE_MINIMIZE = 'minimize';

var
  frmFreeOTFEMain:                TfrmFreeOTFEMain;
  GLOBAL_VAR_WM_FREEOTFE_RESTORE: Cardinal;
  GLOBAL_VAR_WM_FREEOTFE_REFRESH: Cardinal;

implementation

{$R *.DFM}

{ TODO -otdk -crefactor : editing dcr files is awkward - use imagelist instead }
{$R FreeOTFESystemTrayIcons.dcr}

uses
             // delphi units
  ShellApi,  // Required for SHGetFileInfo
  Commctrl,  // Required for ImageList_GetIcon
  ComObj,    // Required for StringToGUID
  Math,      // Required for min
  strutils,
  //sdu units
  pkcs11_slot,
  SDUFileIterator_U,
  SDUi18n,//for format drive
  SDUGraphics,

  //DoxBox units
  MouseRNGDialog_U,

  //  SDUWinSvc,
  CommonfrmAbout,
  CommonfrmCDBBackupRestore, FreeOTFEConsts,
  FreeOTFEfrmOptions,
  FreeOTFEfrmSelectOverwriteMethod,
  FreeOTFEfrmVolProperties,
  OTFEConsts_U,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_frmWizardCreateVolume, OTFEFreeOTFE_PKCS11;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  AUTORUN_POST_MOUNT    = 'post mount';
  AUTORUN_PRE_MOUNT     = 'pre dismount';
  AUTORUN_POST_DISMOUNT = 'post dismount';

const
  AUTORUN_TITLE: array [TAutorunType] of String = (
    AUTORUN_POST_MOUNT,
    AUTORUN_PRE_MOUNT,
    AUTORUN_POST_DISMOUNT
    );

resourcestring
  FREEOTFE_DESCRIPTION = 'DoxBox: Open-Source Transparent Encryption';

  TEXT_NEED_ADMIN = 'You need administrator privileges in order to carry out this operation.';

  // Toolbar captions...
  RS_TOOLBAR_CAPTION_MOUNTPARTITION = 'Open partition Box';
  RS_TOOLBAR_CAPTION_DISMOUNTALL    = 'Lock all';
  RS_TOOLBAR_CAPTION_PORTABLEMODE   = 'Portable mode';
  // Toolbar hints...
  RS_TOOLBAR_HINT_MOUNTPARTITION    = 'Open a partition based Box';
  RS_TOOLBAR_HINT_DISMOUNTALL       = 'Lock all open Boxes';
  RS_TOOLBAR_HINT_PORTABLEMODE      = 'Toggle portable mode on/off';

  POPUP_DISMOUNT = 'Dismount %1: %2';

const

  R_ICON_MOUNTED   = 'MOUNTED';
  R_ICON_UNMOUNTED = 'UNMOUNTED';

  TAG_SYSTRAYICON_POPUPMENUITEMS           = $5000; // Note: LSB will be the drive letter
  TAG_SYSTRAYICON_POPUPMENUITEMS_DRIVEMASK = $FF;   // LSB mask for drive letter

  HOTKEY_TEXT_NONE           = '(None)';
  HOTKEY_IDENT_DISMOUNT      = 1;
  HOTKEY_IDENT_DISMOUNTEMERG = 2;

  // Overlay icons; these MUST be kept in sync with the icons in
  // ilDriveIconOverlay
  OVERLAY_IMGIDX_NONE        = -1;
  OVERLAY_IMGIDX_PKCS11TOKEN = 0;
  OVERLAY_IMGIDX_LINUX       = 1;

  AUTORUN_SUBSTITUTE_DRIVE = '%DRIVE';

  // Command line parameters. case insensitive. generally only one action per invocation
  //swithces only
  CMDLINE_NOUACESCALATE = 'noUACescalate';
  CMDLINE_GUI           = 'gui';
  CMDLINE_FORCE         = 'force';
  CMDLINE_SET_INSTALLED = 'SetInstalled';
  //sets 'installed' flag in ini file, creating if nec. - usually used with CMDLINE_SETTINGSFILE

  // cmds with params
  CMDLINE_DRIVERCONTROL = 'driverControl';
  CMDLINE_PORTABLE      = 'portable';
  CMDLINE_TOGGLE        = 'toggle';
  CMDLINE_DISMOUNT      = 'dismount';


  //  CMDLINE_MOUNTED          = 'mounted'; unused
  CMDLINE_SET_TESTMODE = 'SetTestMode';

  // args to Command line parameters...
  CMDLINE_COUNT            = 'count';
  CMDLINE_START            = 'start';
  CMDLINE_ON               = 'on';
  CMDLINE_STOP             = 'stop';
  CMDLINE_OFF              = 'off';
  CMDLINE_ALL              = 'all';
  CMDLINE_INSTALL          = 'install';
  CMDLINE_UNINSTALL        = 'uninstall';
  CMDLINE_DRIVERNAME       = 'drivername';
  CMDLINE_TOTAL            = 'total';
  CMDLINE_DRIVERSPORTABLE  = 'portable';
  CMDLINE_DRIVERSINSTALLED = 'installed';

  // Online user manual URL...
  { DONE -otdk -cenhancement : set project homepage }
  URL_USERGUIDE_MAIN = 'http://DoxBox.eu/docs/Main';
  // PAD file URL...
  URL_PADFILE_MAIN   = 'https://raw.githubusercontent.com/t-d-k/doxbox/master/PAD.xml';
  // Download URL...
  URL_DOWNLOAD_MAIN  = 'http://DoxBox.eu/download.html';


 // This procedure is called after OnCreate, and immediately before the
 // Application.Run() is called
procedure TfrmFreeOTFEMain.InitApp();
var
  goForStartPortable: Boolean;
begin
  inherited;

  // Hook Windows messages first; if we're running under Vista and the user UAC
  // escalates to start portable mode (below), we don't want to miss any
  // refresh messages the escalated process may send us!
  Application.HookMainWindow(MessageHook);

  InitializeDrivesDisplay();
  // We call EnableDisableControls(...) at this point, to display the
  // toolbar/statusbar as needed
  EnableDisableControls();

  // EnableDisableControls(...) calls SetupToolbarFromSettings(...) which can
  // change the window's width if large icons are being used
  // We recenter the window here so it looks right
  //
  // !!! WARNING !!!
  // DON'T USE self.Position HERE!
  // This causes the Window to be destroyed and recreated - which messes up
  // the system tray icon code as it's monitoring the window's messages
  //  self.Position := poScreenCenter;
  // Instead, position window manually...
  self.Top  := ((Screen.Height - self.Height) div 2);
  self.Left := ((Screen.Width - self.Width) div 2);

  if not (ActivateFreeOTFEComponent(True)) then begin
    goForStartPortable := Settings.OptAutoStartPortable;
    if not (goForStartPortable) then begin
      // if 'installed' then install drivers and prompt reboot else prompt portable as below
      if not Settings.OptInstalled then
        goForStartPortable := SDUConfirmYN(
          _('The main DoxBox driver does not appear to be installed and running on this computer') +
          SDUCRLF + SDUCRLF + _('Would you like to start DoxBox in portable mode?'));
    end;

    if goForStartPortable then begin
      PortableModeSet(pmaStart, False);
    end else begin
      if Settings.OptInstalled then begin
        actInstallExecute(nil);
      end else begin
        SDUMessageDlg(
          _(
          'Please see the "installation" section of the accompanying documentation for instructions on how to install the DoxBox drivers.'),
          mtInformation,
          [mbOK],
          0
          );
      end;
    end;

  end;

  RefreshDrives();
  EnableDisableControls();
  SetupHotKeys();
  SetupPKCS11(False);

end;


procedure TfrmFreeOTFEMain.RefreshDrives();
var
  ListItem:     TListItem;
  driveIconNum: Integer;
  i:            Integer;
  volumeInfo:   TOTFEFreeOTFEVolumeInfo;
  mounted:      DriveLetterString;
  miTmp:        TMenuItem;
  strVolID:     String;
  overlayIdx:   Integer;
begin
  // Flush any caches in case a separate instance of FreeOTFE running from the
  // commandline, with escalated UAC changed the drivers installed/running
  fOtfeFreeOtfeBase.CachesFlush();

  // Cleardown all...

  // ...Main FreeOTFE window list...
  lvDrives.Items.Clear();
  ilDriveIcons.Clear();

  // ...System tray icon drives list...
  DestroySysTrayIconMenuitems();

  // In case the drivers have been stopped/started recently, we bounce the
  // FreeOTFE component up and down (silently!)
  DeactivateFreeOTFEComponent();
  ActivateFreeOTFEComponent(True);

  /// ...and repopulate
  if fOtfeFreeOtfeBase.Active then begin
    mounted := fOtfeFreeOtfeBase.DrivesMounted();

    for i := 1 to length(mounted) do begin
      // ...Main FreeOTFE window list...
      if fOtfeFreeOtfeBase.GetVolumeInfo(mounted[i], volumeInfo) then begin
        ListItem         := lvDrives.Items.Add;
        ListItem.Caption := '     ' + mounted[i] + ':';
        ListItem.SubItems.Add(volumeInfo.Filename);
        overlayIdx   := DetermineDriveOverlay(volumeInfo);
        driveIconNum := AddIconForDrive(mounted[i], overlayIdx);
        if (driveIconNum >= 0) then begin
          ListItem.ImageIndex := driveIconNum;
        end;
      end else begin
        // Strange... The drive probably got dismounted between getting the
        // list of mounted drives, and querying them for more information...
      end;


      // The volume ID/label
      // Cast to prevent compiler error
      strVolID := SDUVolumeID(DriveLetterChar(mounted[i]));
      if strVolID <> '' then begin
        strVolID := '[' + strVolID + ']';
      end;

      // ...System tray icon popup menu...
      miTmp             := TMenuItem.Create(nil);
      miTmp.Tag         := TAG_SYSTRAYICON_POPUPMENUITEMS or Ord(mounted[i]);
      // We tag our menuitems
      // in order that we can
      // know which ones to
      // remove later
      miTmp.OnClick     := SystemTrayIconDismount;
      miTmp.AutoHotkeys := maManual;  // Otherwise the hotkeys added
                                      // automatically can interfere with
                                      // examing the caption to get the drive
                                      // letter when clicked on
      miTmp.Caption     := SDUParamSubstitute(POPUP_DISMOUNT, [mounted[i], strVolID]);
      pmSystemTray.Items.Insert((i - 1), miTmp);
    end;

    // Insert a divider into the system tray icon's popup menu after the
    // drives are listed
    if (length(mounted) > 0) then begin
      miTmp         := TMenuItem.Create(nil);
      miTmp.Caption := '-';
      pmSystemTray.Items.Insert(length(mounted), miTmp);
    end;

  end;


  fcountPortableDrivers := (fOtfeFreeOtfeBase as TOTFEFreeOTFE).DriversInPortableMode();
  EnableDisableControls();

end;


// Tear down system tray icon popup menu menuitems previously added
procedure TfrmFreeOTFEMain.DestroySysTrayIconMenuitems();
var
  i:     Integer;
  miTmp: TMenuItem;
begin
  for i := (pmSystemTray.Items.Count - 1) downto 0 do begin
    if ((pmSystemTray.Items[i].Tag and TAG_SYSTRAYICON_POPUPMENUITEMS) =
      TAG_SYSTRAYICON_POPUPMENUITEMS) then begin
      miTmp := pmSystemTray.Items[i];
      pmSystemTray.Items.Delete(i);
      miTmp.Free();
    end;
  end;
end;


procedure TfrmFreeOTFEMain.SystemTrayIconDismount(Sender: TObject);
var
  miDismount: TMenuItem;
  drive:      DriveLetterChar;
begin
  miDismount := TMenuItem(Sender);

  drive := DriveLetterChar(miDismount.Tag and TAG_SYSTRAYICON_POPUPMENUITEMS_DRIVEMASK);

  DismountDrives(drive, False);
end;


function TfrmFreeOTFEMain.DetermineDriveOverlay(volumeInfo: TOTFEFreeOTFEVolumeInfo): Integer;
begin
  Result := OVERLAY_IMGIDX_NONE;

  if volumeInfo.MetaDataStructValid then begin
    if (volumeInfo.MetaDataStruct.PKCS11SlotID <> PKCS11_NO_SLOT_ID) then begin
      Result := OVERLAY_IMGIDX_PKCS11TOKEN;
    end else
    if volumeInfo.MetaDataStruct.LinuxVolume then begin
      Result := OVERLAY_IMGIDX_LINUX;
    end;
  end;

end;

 { TODO : use diff icons for eg freeotfe/luks/dmcrypt , instead of overlays }
 // Add an icon to represent the specified drive
 // overlayIdx - Set to an image index within ilDriveIconOverlay, or -1 for no
 //              overlay
 // Returns: The icon's index in ilDriveIcons, or -1 on error
function TfrmFreeOTFEMain.AddIconForDrive(driveLetter: DriveLetterChar;
  overlayIdx: Integer): Integer;
var
  iconIdx:       Integer;
  anIcon:        TIcon;
  shfi:          SHFileInfo;
  imgListHandle: THandle;
  iconHandle:    HICON;
  tmpDrivePath:  String;
  overlayIcon:   TIcon;
  listBkColor:   TColor;
begin
  iconIdx := -1;

  // Create a suitable icon to represent the drive
  tmpDrivePath  := driveLetter + ':\';
  imgListHandle := SHGetFileInfo(PChar(tmpDrivePath), 0, shfi, sizeof(shfi),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  //                                 SHGFI_DISPLAYNAME

  if imgListHandle <> 0 then begin
    iconHandle := ImageList_GetIcon(imgListHandle, shfi.iIcon, ILD_NORMAL);
    if (iconHandle <> 0) then begin
      anIcon := TIcon.Create();
      try
        anIcon.ReleaseHandle();

        anIcon.handle := iconHandle;

        if (ilDriveIcons.Width < anIcon.Width) then begin
          ilDriveIcons.Width := anIcon.Width;
        end;
        if (ilDriveIcons.Height < anIcon.Height) then begin
          ilDriveIcons.Height := anIcon.Height;
        end;

        if (overlayIdx >= 0) then begin
          overlayIcon := TIcon.Create();
          try
            ilDriveIconOverlay.GetIcon(overlayIdx, overlayIcon);
            SDUOverlayIcon(anIcon, overlayIcon);
          finally
            overlayIcon.Free();
          end;
        end;

        // We switch the background colour so that (Under Windows XP, unthemed,
        // at least) the icons don't appear to have a black border around them
        listBkColor          := ilDriveIcons.BkColor;
        ilDriveIcons.BkColor := lvDrives.Color;
        iconIdx              := ilDriveIcons.addicon(anIcon);
        ilDriveIcons.BkColor := listBkColor;


        anIcon.ReleaseHandle();
        DestroyIcon(iconHandle);
      finally
        anIcon.Free();
      end;

    end;
  end;


  Result := iconIdx;

end;

procedure TfrmFreeOTFEMain.InitializeDrivesDisplay();
var
  tmpColumn: TListColumn;
begin
  // Listview setup...
  lvDrives.SmallImages             := ilDriveIcons;
  lvDrives.RowSelect               := True;
  lvDrives.MultiSelect             := True;
  lvDrives.showcolumnheaders       := True;
  lvDrives.ReadOnly                := True;
  lvDrives.ViewStyle               := vsReport;
  //    lvDrives.ViewStyle := vsIcon;
  //    lvDrives.ViewStyle := vsList;
  lvDrives.IconOptions.Arrangement := TIconArrangement.iaTop;


  // Listview columns...
  tmpColumn         := lvDrives.Columns.Add;
  tmpColumn.Caption := _('Drive');
  tmpColumn.Width   := 100;

  //    tmpColumn.minwidth := lvDrives.width;
  tmpColumn         := lvDrives.Columns.Add;
  tmpColumn.Caption := _('Box');
  tmpColumn.Width   := lvDrives.clientwidth - lvDrives.columns[0].Width - ilDriveIcons.Width;


  // xxx - shouldn't this work?     NewColumn.autosize := TRUE;
  // xxx - shouldn't this work?     NewColumn.minwidth := lvDrives.clientwidth - lvDrives.columns[0].width - ilDriveIcons.width;


  // xxx - use destroyimage or whatever it was to cleardown the **imagelist**

end;

procedure TfrmFreeOTFEMain.RecaptionToolbarAndMenuIcons();
begin
  inherited;

  // Change toolbar captions to shorter captions
  tbbNew.Caption                := RS_TOOLBAR_CAPTION_NEW;
  tbbMountFile.Caption          := RS_TOOLBAR_CAPTION_MOUNTFILE;
  tbbMountPartition.Caption     := RS_TOOLBAR_CAPTION_MOUNTPARTITION;
  tbbDismount.Caption           := RS_TOOLBAR_CAPTION_DISMOUNT;
  tbbDismountAll.Caption        := RS_TOOLBAR_CAPTION_DISMOUNTALL;
  tbbTogglePortableMode.Caption := RS_TOOLBAR_CAPTION_PORTABLEMODE;

  // Setup toolbar hints
  tbbNew.Hint                := RS_TOOLBAR_HINT_NEW;
  tbbMountFile.Hint          := RS_TOOLBAR_HINT_MOUNTFILE;
  tbbMountPartition.Hint     := RS_TOOLBAR_HINT_MOUNTPARTITION;
  tbbDismount.Hint           := RS_TOOLBAR_HINT_DISMOUNT;
  tbbDismountAll.Hint        := RS_TOOLBAR_HINT_DISMOUNTALL;
  tbbTogglePortableMode.Hint := RS_TOOLBAR_HINT_PORTABLEMODE;

end;

procedure TfrmFreeOTFEMain.SDUSystemTrayIcon1Click(Sender: TObject);
begin
  inherited;
  DoSystemTrayAction(Sender, Settings.OptSystemTrayIconActionSingleClick);
end;

procedure TfrmFreeOTFEMain.SDUSystemTrayIcon1DblClick(Sender: TObject);
begin
  inherited;
  DoSystemTrayAction(Sender, Settings.OptSystemTrayIconActionDoubleClick);
end;

procedure TfrmFreeOTFEMain.DoSystemTrayAction(Sender: TObject; doAction: TSystemTrayClickAction);
begin
  case doAction of

    stcaDoNothing:
    begin
      // Do nothing...
    end;

    stcaDisplayConsole:
    begin
      actConsoleDisplay.Execute();
    end;

    stcaDisplayHideConsoleToggle:
    begin
      if self.Visible then begin
        actConsoleHide.Execute();
      end else begin
        actConsoleDisplay.Execute();
      end;
    end;

    stcaMountFile:
    begin
      actFreeOTFEMountFile.Execute();
    end;

    stcaMountPartition:
    begin
      actFreeOTFEMountPartition.Execute();
    end;

    stcaMountLinuxFile:
    begin
      actLinuxMountFile.Execute();
    end;

    stcaMountLinuxPartition:
    begin
      actLinuxMountPartition.Execute();
    end;

    stcaDismountAll:
    begin
      actDismountAll.Execute();
    end;

  end;

end;

{ tests
  system tests
  opens volumes created with old versions and checks contents as expected
  regression testing only
  see testing.jot for more details of volumes

  to test
  LUKS keyfile
  LUKS at offset (ever used?)
  creating vol
  creating keyfile
  creating .les file
  partition operations
  overwrite ops
  pkcs11 ops
  cdb at offset
  salt <> default value
}
function TfrmFreeOTFEMain.DoTests: Boolean;
var
  mountList: TStringList;
  mountedAs: DriveLetterString;
  prettyMountedAs, vol_path, key_file, les_file: String;
  vl: Integer;
const

  TEST_VOLS: array[0..12] of String =
    ('a.box', 'b.box', 'c.box', 'd.box', 'e.box', 'e.box', 'f.box', 'luks.box',
    'luks_essiv.box', 'a.box', 'b.box', 'dmcrypt_dx.box', 'dmcrypt_dx.box');
  PASSWORDS: array[0..12] of String =
    ('password', 'password', '!"£$%^&*()', 'password', 'password', '5ekr1t',
    'password', 'password', 'password', 'secret', 'secret', 'password', '5ekr1t');
  ITERATIONS: array[0..12] of Integer =
    (2048, 2048, 2048, 2048, 10240, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048);
  OFFSET: array[0..12] of Integer =
    (0, 0, 0, 0, 0, 2097152, 0, 0, 0, 0, 0, 0, 0);
  KEY_FILES: array[0..12] of String =
    ('', '', '', '', '', '', '', '', '', 'a.cdb', 'b.cdb', '', '');
  LES_FILES: array[0..12] of String =
    ('', '', '', '', '', '', '', '', '', '', '', 'dmcrypt_dx.les', 'dmcrypt_hid.les');

  procedure CheckMountedOK;
  begin
    RefreshDrives();
    // Mount successful
    //        prettyMountedAs := prettyPrintDriveLetters(mountedAs);
    if (CountValidDrives(mountedAs) <> 1) then begin
      SDUMessageDlg(Format('The Box %s has NOT been opened as drive: %s',
        [TEST_VOLS[vl], mountedAs]));
      Result := False;
    end else begin
      //done: test opened OK & file exists
      if not FileExists(mountedAs + ':\README.txt') then begin
        SDUMessageDlg(Format('File: %s:\README.txt not found', [mountedAs]));
        Result := False;
      end;
    end;
  end;

  procedure UnMountAndCheck;
  begin
    Application.ProcessMessages;
    DismountAll(True);
    Application.ProcessMessages;
    RefreshDrives();
    Application.ProcessMessages;
    if CountValidDrives(fOtfeFreeOtfeBase.DrivesMounted) > 0 then begin
      SDUMessageDlg(Format('Drive(s) %s not unmounted', [fOtfeFreeOtfeBase.DrivesMounted]));
      Result := False;
    end;
  end;

begin
  inherited;

  Result    := True;

  assert('1234' = SDUBytesToString(SDUStringToSDUBytes('1234')));

  mountList := TStringList.Create();
  try
    //for loop is optimised into reverse order , but want to process forwards
    vl       := 0;
    vol_path := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\test_vols\');
    while vl <= high(TEST_VOLS) do begin

      //test one at a time as this is normal use
      mountList.Clear;
      mountList.Add(vol_path + TEST_VOLS[vl]);
      mountedAs := '';
      key_file  := '';
      if KEY_FILES[vl] <> '' then
        key_file := vol_path + KEY_FILES[vl];
      if LES_FILES[vl] <> '' then
        les_file := vol_path + LES_FILES[vl];

      if fOtfeFreeOtfeBase.IsLUKSVolume(vol_path + TEST_VOLS[vl]) or (LES_FILES[vl] <> '') then
      begin
        if not fOtfeFreeOtfeBase.MountLinux(mountList, mountedAs, True, les_file,SDUStringToSDUBytes(PASSWORDS[vl]),
          key_file, False, nlLF, 0, True) then
          Result := False;
      end else begin
        //call silently
        if not fOtfeFreeOtfeBase.MountFreeOTFE(mountList, mountedAs, True,
          key_file,SDUStringToSDUBytes(PASSWORDS[vl]), OFFSET[vl], False, True, 256, ITERATIONS[vl]) then
          Result := False;
      end;
      if not Result then begin
        SDUMessageDlg(
          _('Unable to open ') + TEST_VOLS[vl] + '.', mtError);
      end else begin
        CheckMountedOK;
        UnMountAndCheck;
      end;
      Inc(vl);
    end;

  {test changing password
    backup freeotfe header ('cdb')
    change password
    open with new password
    restore header
    open with old password -tested on next test run
    this tests most of code for creating volume as well
  }
    //don't fail if doesnt exist
    SysUtils.DeleteFile(vol_path + 'a_test.cdbBackup');
    if Result then
      BackupRestore(opBackup, vol_path + TEST_VOLS[0], vol_path + 'a_test.cdbBackup', True);
    Result := Result and fOtfeFreeOtfeBase.WizardChangePassword(vol_path +
      TEST_VOLS[0], PASSWORDS[0], 'secret4', True);

    if Result then begin
      mountList.Clear;
      mountList.Add(vol_path + TEST_VOLS[0]);
      if not fOtfeFreeOtfeBase.MountFreeOTFE(mountList, mountedAs, True, '',
        SDUStringToSDUBytes('secret4'), 0, False, True, 256, 2048) then
        Result := False;
    end;
    if Result then
      CheckMountedOK;
    UnmountAndCheck;
    if Result then
      BackupRestore(opRestore, vol_path + TEST_VOLS[0], vol_path + 'a_test.cdbBackup', True);
    { TODO -otdk -cenhance : use copied file to check is same as orig file }
    // no need to check mount again as will be checked with next test run
    Result := Result and SysUtils.DeleteFile(vol_path + 'a_test.cdbBackup');
  finally
    mountList.Free();
  end;

  if Result then
    SDUMessageDlg('All functional tests passed')
  else
    SDUMessageDlg('At least one functional test failed');
end;

procedure TfrmFreeOTFEMain.SetIconListsAndIndexes();
begin
  inherited;

  if Settings.OptDisplayToolbarLarge then begin
    ToolBar1.Images := ilToolbarIcons_Large;
     (*
    tbbNew.ImageIndex                := FIconIdx_Large_New;
    tbbMountFile.ImageIndex          := FIconIdx_Large_MountFile;
    tbbMountPartition.ImageIndex     := FIconIdx_Large_MountPartition;
    tbbDismount.ImageIndex           := FIconIdx_Large_Dismount;
    tbbDismountAll.ImageIndex        := FIconIdx_Large_DismountAll;
    tbbTogglePortableMode.ImageIndex := FIconIdx_Large_PortableMode;
    *)
  end else begin
    ToolBar1.Images := ilToolbarIcons_Small;
                    (*
    tbbNew.ImageIndex                := FIconIdx_Small_New;
    tbbMountFile.ImageIndex          := FIconIdx_Small_MountFile;
    tbbMountPartition.ImageIndex     := FIconIdx_Small_MountPartition;
    tbbDismount.ImageIndex           := FIconIdx_Small_Dismount;
    tbbDismountAll.ImageIndex        := FIconIdx_Small_DismountAll;
    tbbTogglePortableMode.ImageIndex := FIconIdx_Small_PortableMode;
    *)
  end;
     (*
  actDismountAll.ImageIndex            := FIconIdx_Small_DismountAll;
  actFreeOTFEMountPartition.ImageIndex := FIconIdx_Small_MountPartition;
  actTogglePortableMode.ImageIndex     := FIconIdx_Small_PortableMode;
  actProperties.ImageIndex             := FIconIdx_Small_Properties;
          *)
  // Set icons on menuitems
  if (FIconIdx_Small_VistaUACShield >= 0) then begin
    miFreeOTFEDrivers.ImageIndex     := FIconIdx_Small_VistaUACShield;
    miPortableModeDrivers.ImageIndex := FIconIdx_Small_VistaUACShield;
    actFormat.ImageIndex             := FIconIdx_Small_VistaUACShield;

    // Splat small Vista UAC shield icon onto large *portable mode* icon
    //  lplp - todo - Note: 22x22 "World" icon may be too small to have UAC icon on top of it?
  end;

end;

procedure TfrmFreeOTFEMain.FormCreate(Sender: TObject);
begin
  // Prevent endless loops of UAC escalation...
  fallowUACEsclation := False;

  fOtfeFreeOtfeBase := TOTFEFreeOTFE.Create(nil);

  inherited;

  fendSessionFlag   := False;
  fshuttingDownFlag := False;

  fIconMounted          := TIcon.Create();
  fIconMounted.Handle   := LoadImage(hInstance, PChar(R_ICON_MOUNTED), IMAGE_ICON,
    16, 16, LR_DEFAULTCOLOR);
  fIconUnmounted        := TIcon.Create();
  fIconUnmounted.Handle := LoadImage(hInstance, PChar(R_ICON_UNMOUNTED),
    IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);

  fcountPortableDrivers := -1;

  StatusBar_Status.SimplePanel := True;

  // We set these to invisible so that if they're disabled by the user
  // settings, it doens't flicker on them off
  ToolBar1.Visible := False;

  // Give the user a clue
  ToolBar1.ShowHint := True;

  ToolBar1.Indent := 5;

end;

procedure TfrmFreeOTFEMain.FormDestroy(Sender: TObject);
begin
  inherited;

  // Note: If Settings is *not* set, this will be free'd off
  //       in FreeOTFE.dpr
  if Application.ShowMainForm then begin
    Settings.Free();
  end;

  fIconMounted.Free();
  fIconUnmounted.Free();

  fOtfeFreeOtfeBase.Free();
end;

procedure TfrmFreeOTFEMain.actRefreshExecute(Sender: TObject);
begin
  RefreshDrives();

end;


procedure TfrmFreeOTFEMain.lvDrivesResize(Sender: TObject);
begin
  ResizeWindow();

end;

procedure TfrmFreeOTFEMain.FormResize(Sender: TObject);
begin
  // Don't call ResizeWindow() here - it causes Windows 7 to give an error
  // ("System Error. Code 87") on startup if both FormResize(...) and
  // lvDrivesResize(...) call this, which prevents the application from
  // initializing properly (e.g. it won't accept drag dropped files on the GUI,
  // because it silently aborts before initializing that functionality)
  //  ResizeWindow();

end;


// Resize the columns in the drives list so the 2nd column fills out...
procedure TfrmFreeOTFEMain.ResizeWindow();
var
  tmpColumn: TListColumn;
begin
  if lvDrives.columns.Count > 1 then begin
    tmpColumn       := lvDrives.columns[1];
    tmpColumn.Width := lvDrives.clientwidth - lvDrives.columns[0].Width - ilDriveIcons.Width;
  end;

end;

procedure TfrmFreeOTFEMain.SetupOTFEComponent();
begin
  inherited;

  (fOtfeFreeOtfeBase as TOTFEFreeOTFE).DefaultDriveLetter := Settings.OptDefaultDriveLetter;
  (fOtfeFreeOtfeBase as TOTFEFreeOTFE).DefaultMountAs     := Settings.OptDefaultMountAs;

end;

// Reload settings, setting up any components as needed
procedure TfrmFreeOTFEMain.ReloadSettings();
begin
  inherited;

  SDUSystemTrayIcon1.Tip := Application.Title;

  SDUClearPanel(pnlTopSpacing);

  SDUSystemTrayIcon1.Active         := Settings.OptSystemTrayIconDisplay;
  SDUSystemTrayIcon1.MinimizeToIcon := Settings.OptSystemTrayIconMinTo;

  SetupHotKeys();

  RefreshMRUList();

end;

procedure TfrmFreeOTFEMain.MountFiles(mountAsSystem: TDragDropFileType;
  filenames: TStringList; ReadOnly, forceHidden: Boolean);
var
  i:               Integer;
  mountedAs:       DriveLetterString;
  msg:             String;
  mountedOK:       Boolean;
  prettyMountedAs: String;
begin
  // Why was this in here?!
  //  ReloadSettings();

  AddToMRUList(filenames);

  if (mountAsSystem = ftFreeOTFE) then begin
    mountedOK := fOtfeFreeOtfeBase.MountFreeOTFE(filenames, mountedAs, ReadOnly);
  end else
  if (mountAsSystem = ftLinux) then begin
    mountedOK := fOtfeFreeOtfeBase.MountLinux(filenames, mountedAs, ReadOnly,
      '',nil, '', False, LINUX_KEYFILE_DEFAULT_NEWLINE, 0, False, forceHidden);
  end else begin
    mountedOK := fOtfeFreeOtfeBase.Mount(filenames, mountedAs, ReadOnly);
  end;

  if not (mountedOK) then begin
    if (fOtfeFreeOtfeBase.LastErrorCode <> OTFE_ERR_USER_CANCEL) then begin
      SDUMessageDlg(
        _('Unable to open Box.') + SDUCRLF + SDUCRLF +
        _('Please check your keyphrase and settings, and try again.'),
        mtError
        );
    end;
  end else begin
    // Mount successful
    prettyMountedAs := PrettyPrintDriveLetters(mountedAs);
    if (CountValidDrives(mountedAs) = 1) then begin
      msg := SDUParamSubstitute(_('Your DoxBox has been opened as drive: %1'), [prettyMountedAs]);
    end else begin
      msg := SDUParamSubstitute(_('Your DoxBoxes have been opened as drives: %1'),
        [prettyMountedAs]);
    end;

    RefreshDrives();

    if Settings.OptPromptMountSuccessful then begin
      SDUMessageDlg(msg, mtInformation);
    end;

    for i := 1 to length(mountedAs) do begin
      if (mountedAs[i] <> #0) then begin
        if Settings.OptExploreAfterMount then begin
          ExploreDrive(mountedAs[i]);
        end;

        AutoRunExecute(arPostMount, mountedAs[i], False);
      end;
    end;

  end;

end;


procedure TfrmFreeOTFEMain.RefreshMRUList();
begin
  inherited;

  // Refresh menuitems...
  Settings.OptMRUList.RemoveMenuItems(mmMain);
  Settings.OptMRUList.InsertAfter(miFreeOTFEDrivers);

  Settings.OptMRUList.RemoveMenuItems(pmSystemTray);
  Settings.OptMRUList.InsertUnder(mmRecentlyMounted);

  mmRecentlyMounted.Visible := ((Settings.OptMRUList.MaxItems > 0) and
    (Settings.OptMRUList.Items.Count > 0));

end;

procedure TfrmFreeOTFEMain.MRUListItemClicked(mruList: TSDUMRUList; idx: Integer);
begin
  MountFilesDetectLUKS(mruList.Items[idx], False, Settings.OptDragDropFileType);
end;

 // Handle files being dropped onto the form from (for example) Windows Explorer.
 // Files dropped onto the form should be treated as though the user is trying
 // to mount them as volume files
procedure TfrmFreeOTFEMain.WMDropFiles(var Msg: TWMDropFiles);
var
  buffer:       array[0..MAX_PATH] of Char;
  numFiles:     Longint;
  i:            Integer;
  filesToMount: TStringList;
begin
  try
    // Bring our window to the foreground
    SetForeGroundWindow(self.handle);

    // Handle the dropped files
    numFiles     := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    filesToMount := TStringList.Create();
    try
      for i := 0 to (numFiles - 1) do begin
        DragQueryFile(msg.Drop,
          i, @buffer,
          sizeof(buffer));
        filesToMount.Add(buffer);
      end;

      MountFilesDetectLUKS(filesToMount, False, Settings.OptDragDropFileType);

      Msg.Result := 0;
    finally
      filesToMount.Free();
    end;
  finally
    DragFinish(Msg.Drop);
  end;

end;


procedure TfrmFreeOTFEMain.WMDeviceChange(var Msg: TMessage);
begin
  RefreshDrives();

end;


// Function required for close to system tray icon
procedure TfrmFreeOTFEMain.WMQueryEndSession(var msg: TWMQueryEndSession);
begin
  // Default to allowing shutdown
  msg.Result      := 1;
  fendSessionFlag := (msg.Result = 1);
  inherited;
  fendSessionFlag := (msg.Result = 1);

end;


// Function required for close to system tray icon
procedure TfrmFreeOTFEMain.WMEndSession(var msg: TWMEndSession);
begin
  fendSessionFlag := msg.EndSession;

end;


procedure TfrmFreeOTFEMain.DriveProperties();
var
  propertiesDlg: TfrmFreeOTFEVolProperties;
  i:             Integer;
  selDrives:     DriveLetterString;
begin
  selDrives := GetSelectedDrives();
  for i := 1 to length(selDrives) do begin
    propertiesDlg := TfrmFreeOTFEVolProperties.Create(self);
    try
      propertiesDlg.DriveLetter  := selDrives[i];
      propertiesDlg.OTFEFreeOTFE := (fOtfeFreeOtfeBase as TOTFEFreeOTFE);
      propertiesDlg.ShowModal();
    finally
      propertiesDlg.Free();
    end;

  end;

end;


 // If "isEmergency" is TRUE, the user will *not* be informed of any drives
 // which couldn't be dismounted
function TfrmFreeOTFEMain.DismountAll(isEmergency: Boolean = False): Boolean;
var
  drivesRemaining: DriveLetterString;
  initialDrives:   DriveLetterString;
  i:               Integer;
begin
  // Change CWD to anywhere other than a mounted drive
  ChangeCWDToSafeDir();

  // Launch any pre-dismount executable
  initialDrives := fOtfeFreeOtfeBase.DrivesMounted;
  for i := 1 to length(initialDrives) do begin
    AutoRunExecute(arPreDismount, initialDrives[i], isEmergency);
  end;

  fOtfeFreeOtfeBase.DismountAll(isEmergency);

  // If it wasn't an emergency dismount, and drives are still mounted, report.
  drivesRemaining := fOtfeFreeOtfeBase.DrivesMounted();

  for i := 1 to length(initialDrives) do begin
    // If the drive is no longer mounted, run any post-dismount executable
    if (Pos(initialDrives[i], drivesRemaining) <= 0) then begin
      AutoRunExecute(arPostDismount, initialDrives[i], isEmergency);
    end;
  end;

  if (drivesRemaining <> '') then begin
    if not (isEmergency) then begin
      RefreshDrives();
      ReportDrivesNotDismounted(drivesRemaining, False);
    end else begin
      // Just give it another go... Best we can do.
      fOtfeFreeOtfeBase.DismountAll(isEmergency);
    end;
  end;

  RefreshDrives();

  Result := (fOtfeFreeOtfeBase.CountDrivesMounted <= 0);
end;


function TfrmFreeOTFEMain.DismountSelected(): Boolean;
var
  toDismount: DriveLetterString;
begin
  // First we build up a list of drives to dismount, then we dismount them.
  // This is done since we can't just run through the lvDrives looking for
  // selected items, as they're getting removed while we walk though the list!
  toDismount := GetSelectedDrives();

  Result := DismountDrives(toDismount, False);
end;


 // Dismount the drives specified
 // This procedure *will* report drives which couldn't be mounted - regardless
 // of "isEmergency"
function TfrmFreeOTFEMain.DismountDrives(dismountDrives: DriveLetterString;
  isEmergency: Boolean): Boolean;
var
  i:               Integer;
  j:               Integer;
  subVols:         DriveLetterString;
  tmpDrv:          DriveLetterChar;
  drivesRemaining: DriveLetterString;
begin
  Result          := True;
  drivesRemaining := '';

  // Change CWD to anywhere other than a mounted drive
  ChangeCWDToSafeDir();

  // Skip processing if no drives to dismount - otherwise it'll just flicker
  // the display
  if (dismountDrives <> '') then begin
    // Ensure that none of the drives to be dismounted contain one of the other
    // mounted drives
    // Although FreeOTFE wouldn't be able to dismount such a drive (it would
    // the mounted volume stored on it would have an open filehandles, so
    // preventing the dismount), it's clearly better to warn the user in a more
    // meaningful way than just "can't do it - files open"
    for i := 1 to length(dismountDrives) do begin
      tmpDrv := dismountDrives[i];

      subVols := fOtfeFreeOtfeBase.VolsMountedOnDrive(tmpDrv);
      for j := 1 to length(subVols) do begin
        if (Pos(subVols[j], dismountDrives) = 0) then begin
          // At least one of the currently mounted drives is stored on one of
          // the drives to be dismounted - and we're not dismounting that drive
          SDUMessageDlg(
            SDUParamSubstitute(_(
            'The Box currently opened as %1: must be locked before %2: can be locked'),
            [subVols[j], tmpDrv]),
            mtError
            );
          Result := False;
        end;
      end;
    end;

    if Result then begin
      // Sort out dismount order, in case one drive nested within another
      dismountDrives := fOtfeFreeOtfeBase.DismountOrder(dismountDrives);


      for i := 1 to length(dismountDrives) do begin
        tmpDrv := dismountDrives[i];

        // Pre-dismount...
        AutoRunExecute(arPreDismount, tmpDrv, isEmergency);

        if fOtfeFreeOtfeBase.Dismount(tmpDrv, isEmergency) then begin
          // Dismount OK, execute post-dismount...
          AutoRunExecute(arPostDismount, tmpDrv, isEmergency);
        end else begin
          drivesRemaining := drivesRemaining + tmpDrv;
          Result          := False;
        end;

      end;

      if (drivesRemaining <> '') then begin
        RefreshDrives();
        ReportDrivesNotDismounted(drivesRemaining, isEmergency);
      end;

      RefreshDrives();
    end;
  end;
end;


 // Warn the user that some drives remain mounted, and if the dismount attempted
 // wasn't an emergency dismount, then prompt the user if they want to attempt
 // an emergency dismount
procedure TfrmFreeOTFEMain.ReportDrivesNotDismounted(drivesRemaining: DriveLetterString;
  isEmergency: Boolean);
var
  msg:       String;
  warningOK: Boolean;
  forceOK:   Boolean;
begin
  msg := SDUPluralMsg(length(drivesRemaining), SDUParamSubstitute(
    _('Unable to dismount drive: %1'), [PrettyPrintDriveLetters(drivesRemaining)]),
    SDUParamSubstitute(_('Unable to dismount drives: %1'),
    [PrettyPrintDriveLetters(drivesRemaining)]));

  // If the dismount attempted was a non-emergency dismount; prompt user if
  // they want to attempt an emergency dismount
  if not (isEmergency) then begin
    forceOK := False;
    if (Settings.OptOnNormalDismountFail = ondfPromptUser) then begin
      msg     := msg + SDUCRLF + SDUCRLF + SDUPluralMsg(
        length(drivesRemaining), _('Do you wish to force a dismount on this drive?'),
        _('Do you wish to force a dismount on these drives?'));
      forceOK := SDUConfirmYN(msg);
    end else
    if (Settings.OptOnNormalDismountFail = ondfForceDismount) then begin
      forceOK := True;
    end else  // ondfCancelDismount
    begin
      // Do nothing - forceOK already set to FALSE
    end;

    if forceOK then begin
      warningOK := True;
      if settings.OptWarnBeforeForcedDismount then begin
        warningOK := SDUWarnYN(_('Warning: Emergency dismounts are not recommended') +
          SDUCRLF + SDUCRLF + _('Are you sure you wish to proceed?'));
      end;

      if warningOK then begin
        DismountDrives(drivesRemaining, True);
      end;

    end;
  end else begin
    // Dismount reporting on *was* an emergency dismount; just report remaining
    // drives still mounted
    SDUMessageDlg(msg, mtInformation, [mbOK], 0);
  end;

end;



procedure TfrmFreeOTFEMain.lvDrivesClick(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmFreeOTFEMain.EnableDisableControls();
var
  drivesSelected: Boolean;
  drivesMounted:  Boolean;
begin
  inherited;

  // The FreeOTFE object may not be active...
  actFreeOTFEMountPartition.Enabled :=
    (fOtfeFreeOtfeBase.Active and fOtfeFreeOtfeBase.CanMountDevice());
  // Linux menuitem completely disabled if not active...
  actLinuxMountPartition.Enabled    :=
    (miLinuxVolume.Enabled and fOtfeFreeOtfeBase.CanMountDevice());

  // Flags used later
  drivesSelected := (lvDrives.selcount > 0);
  drivesMounted  := (lvDrives.Items.Count > 0);

  // Actions & menuitems to be enabled/disabled, depending on whether a drive
  // is selected
  actDismount.Enabled   := drivesSelected;
  actProperties.Enabled := drivesSelected;

  miFormat.Enabled                := drivesSelected;
  miPopupFormat.Enabled           := drivesSelected;
  actOverwriteFreeSpace.Enabled   := drivesSelected;  // Note: Implies FreeOTFE drivers are running
  actOverwriteEntireDrive.Enabled := drivesSelected;  // Note: Implies FreeOTFE drivers are running

  // Action item to be enabled/disabled, as long as one or more drives are
  // mounted
  actDismountAll.Enabled := drivesMounted;

  // Driver handling...
  actTogglePortableMode.Checked := (fcountPortableDrivers > 0);
  actTogglePortableMode.Enabled :=
    ((fOtfeFreeOtfeBase as TOTFEFreeOTFE).CanUserManageDrivers() or
    // If we're on Vista, always allow the
    // toggle portable mode option; the
    // user can escalate UAC if needed
    SDUOSVistaOrLater());
  // If the portable mode control is enabled, but we don't know how many
  // drivers there are in portable mode, presumably we're running under
  // Vista without admin access - in which case this control just toggles the
  // portable mode status - and DOESN'T turn it on/off
  if (actTogglePortableMode.Enabled and (fcountPortableDrivers < 0)) then begin
    actTogglePortableMode.Caption := _('Toggle portable mode drivers');
  end else begin
    actTogglePortableMode.Caption := _('Use portable mode drivers');

    // Ensure NO ICON FOR PORTABLE MODE DRIVERS if the user is admin; it shows
    // a checkbox instead, depending on whether the drivers are running or not
    miPortableModeDrivers.ImageIndex := -1;
  end;


  // Misc display related...
  if drivesMounted then begin
    SDUSystemTrayIcon1.Icon := fIconMounted;
  end else begin
    SDUSystemTrayIcon1.Icon := fIconUnmounted;
  end;

  StatusBar_Status.Visible := Settings.OptDisplayStatusbar;
  StatusBar_Hint.Visible   := False;

  SetStatusBarTextNormal();

  SetupToolbarFromSettings();

end;

procedure TfrmFreeOTFEMain.SetStatusBarTextNormal();
var
  drvLetter:  DriveLetterChar;
  freeSpace:  Int64;
  totalSpace: Int64;
  statusText: String;
  selDrives:  DriveLetterString;
begin
  statusText := '';

  selDrives := GetSelectedDrives();

  // Update status bar text, if visible
  if (StatusBar_Status.Visible or StatusBar_Hint.Visible) then begin
    statusText := '';
    if not (fOtfeFreeOtfeBase.Active) then begin
      statusText := _('FreeOTFE main driver not connected.');
    end else
    if (length(selDrives) = 0) then begin
      statusText := SDUPluralMsg(lvDrives.Items.Count,
        SDUParamSubstitute(_('%1 drive mounted.'), [lvDrives.Items.Count]),
        SDUParamSubstitute(_('%1 drives mounted.'), [lvDrives.Items.Count]));
    end else
    if (length(selDrives) > 0) then begin
      if (length(selDrives) = 1) then begin
        drvLetter  := selDrives[1];
        freeSpace  := DiskFree(Ord(drvLetter) - 64);
        totalSpace := DiskSize(Ord(drvLetter) - 64);

        if ((freeSpace > -1) and (totalSpace > -1)) then begin
          statusText := SDUParamSubstitute(_('%1: Free Space: %2; Total size: %3'),
            [drvLetter, SDUFormatUnits(freeSpace, SDUUnitsStorageToTextArr(),
            UNITS_BYTES_MULTIPLIER, 1), SDUFormatUnits(totalSpace,
            SDUUnitsStorageToTextArr(), UNITS_BYTES_MULTIPLIER, 1)]);
        end;
      end;

      if (statusText = '') then begin
        statusText := SDUPluralMsg(lvDrives.SelCount,
          SDUParamSubstitute(_('%1 drive selected.'), [length(selDrives)]),
          SDUParamSubstitute(_('%1 drives selected.'), [length(selDrives)]));
      end;

    end;

  end;

  SetStatusBarText(statusText);
end;

// Set the toolbar size
procedure TfrmFreeOTFEMain.SetupToolbarFromSettings();
var
  toolbarWidth: Integer;
  i:            Integer;
begin
  SetIconListsAndIndexes();

  ToolBar1.Visible := Settings.OptDisplayToolbar;

  // Toolbar captions...
  if Settings.OptDisplayToolbarLarge then begin
    ToolBar1.ShowCaptions := Settings.OptDisplayToolbarCaptions;
  end else begin
    ToolBar1.ShowCaptions := False;
  end;

  ToolBar1.Height       := ToolBar1.Images.Height + TOOLBAR_ICON_BORDER;
  ToolBar1.ButtonHeight := ToolBar1.Images.Height;
  ToolBar1.ButtonWidth  := ToolBar1.Images.Width;

  if Settings.OptDisplayToolbar then begin
    // Turning captions on can cause the buttons to wrap if the window isn't big
    // enough.
    // This looks pretty poor, so resize the window if it's too small
    toolbarWidth := 0;
    for i := 0 to (Toolbar1.ButtonCount - 1) do begin
      toolbarWidth := toolbarWidth + Toolbar1.Buttons[i].Width;
    end;
    if (toolbarWidth > self.Width) then begin
      self.Width := toolbarWidth;
    end;
    // Adjusting the width to the sum of the toolbar buttons doens't make it wide
    // enough to prevent wrapping (presumably due to window borders); nudge the
    // size until it does
    // (Crude, but effective)
    while (Toolbar1.RowCount > 1) do begin
      self.Width := self.Width + 10;
    end;
  end;

end;

function TfrmFreeOTFEMain.GetDriveLetterFromLVItem(listItem: TListItem): DriveLetterChar;
var
  tmpDrv: String;
begin
  // Trim the padding whitespace off the drive letter + colon
  tmpDrv := listItem.Caption;
  tmpDrv := TrimLeft(tmpDrv);

  // The first letter of the item's caption is the drive letter
  Result := DriveLetterChar(tmpDrv[1]);

end;

procedure TfrmFreeOTFEMain.lvDrivesDblClick(Sender: TObject);
var
  driveLetter: DriveLetterChar;
begin
  if (lvDrives.selcount > 0) then begin
    driveLetter := GetDriveLetterFromLVItem(lvDrives.selected);
    ExploreDrive(driveLetter);
  end;

end;


procedure TfrmFreeOTFEMain.DisplayDriverControlDlg();
var
  UACEscalateAttempted: Boolean;
begin
  UACEscalateAttempted := False;

  DeactivateFreeOTFEComponent();
  try
    try
      // This is surrounded by a try...finally as it may throw an exception if
      // the user doesn't have sufficient privs to do this
      (fOtfeFreeOtfeBase as TOTFEFreeOTFE).ShowDriverControlDlg();

      // Send out a message to any other running instances of FreeOTFE to
      // refresh; the drivers may have changed.
      // Note that if we had to UAC escalate (see exception below), the UAC
      // escalated process will send out this windows message
{$IF CompilerVersion >= 18.5}
      //SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
      PostMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ELSE}
      SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$IFEND}
    except
      on EFreeOTFENeedAdminPrivs do begin
        UACEscalateForDriverInstallation();
        UACEscalateAttempted := True;
      end;
    end;

  finally
    // Note: We supress any messages that may be the result of failing to
    //       activate the fOtfeFreeOtfeBase component activating the if we had to
    //       UAC escalate.
    //       In that situation, the UAC escalated process will sent out a
    //       refresh message when it's done. 
    ActivateFreeOTFEComponent(UACEscalateAttempted);

    EnableDisableControls();
  end;

end;

procedure TfrmFreeOTFEMain.actDriversExecute(Sender: TObject);
begin
  DisplayDriverControlDlg();
end;



function TfrmFreeOTFEMain.ActivateFreeOTFEComponent(suppressMsgs: Boolean): Boolean;
begin
  inherited ActivateFreeOTFEComponent(suppressMsgs);

  // Let Windows know whether we accept dropped files or not
  DragAcceptFiles(self.Handle, fOtfeFreeOtfeBase.Active);

  Result := fOtfeFreeOtfeBase.Active;
end;

procedure TfrmFreeOTFEMain.ShowOldDriverWarnings();
begin
  { TODO 1 -otdk -cclean : dont support old drivers }
  // No warnings to be shown in base class
  if (fOtfeFreeOtfeBase.Version() < FREEOTFE_ID_v03_00_0000) then begin
    SDUMessageDlg(
      SDUParamSubstitute(_(
      'The main DoxBox driver installed on this computer dates back to FreeOTFE %1'),
      [fOtfeFreeOtfeBase.VersionStr()]) + SDUCRLF + SDUCRLF + _(
      'It is highly recommended that you upgrade your DoxBox drivers to v3.00 or later as soon as possible, in order to allow the use of LRW and XTS based volumes.') + SDUCRLF + SDUCRLF + _('See documentation (installation section) for instructions on how to do this.'),
      mtWarning
      );
  end else
  if (fOtfeFreeOtfeBase.Version() < FREEOTFE_ID_v04_30_0000) then begin
    SDUMessageDlg(
      SDUParamSubstitute(_(
      'The main DoxBox driver installed on this computer dates back to FreeOTFE %1'),
      [fOtfeFreeOtfeBase.VersionStr()]) + SDUCRLF + SDUCRLF + _(
      'It is highly recommended that you upgrade your DoxBox drivers to those included in v4.30 or later as soon as possible, due to improvements in the main driver.') + SDUCRLF + SDUCRLF + _('See documentation (installation section) for instructions on how to do this.'),
      mtWarning
      );
  end else begin
    inherited;
  end;

end;

procedure TfrmFreeOTFEMain.DeactivateFreeOTFEComponent();
begin
  inherited;

  // Let Windows know we accept dropped files or not
  DragAcceptFiles(self.Handle, fOtfeFreeOtfeBase.Active);

end;


// The array passed in is zero-indexed; populate elements zero to "bytesRequired"
procedure TfrmFreeOTFEMain.GenerateOverwriteData(Sender: TObject; passNumber: Integer;
  bytesRequired: Cardinal; var generatedOK: Boolean; var outputBlock: TShredBlock);
var
  i:              Integer;
  tempArraySize:  Cardinal;
  blocksizeBytes: Cardinal;
  plaintext:      Ansistring;
  cyphertext:     Ansistring;
  IV:             Ansistring;
  localIV:        Int64;
  sectorID:       LARGE_INTEGER;
begin
  // Generate an array of random data containing "bytesRequired" bytes of data,
  // plus additional random data to pad out to the nearest multiple of the
  // cypher's blocksize bits
  // Cater for if the blocksize was -ve or zero
  if (ftempCypherDetails.BlockSize < 1) then begin
    blocksizeBytes := 1;
  end else begin
    blocksizeBytes := (ftempCypherDetails.BlockSize div 8);
  end;
  tempArraySize := bytesRequired + (blocksizeBytes - (bytesRequired mod blocksizeBytes));

  // SDUInitAndZeroBuffer(tempArraySize, plaintext);
  plaintext := '';
  for i := 1 to tempArraySize do begin
    plaintext := plaintext + Ansichar(random(256));
    { DONE 2 -otdk -csecurity : This is not secure PRNG - but key is random, so result is CSPRNG -see faq }
  end;


  Inc(ftempCypherEncBlockNo);

  // Adjust the IV so that this block of encrypted pseudorandom data should be
  // reasonably unique
  IV := '';
  // SDUInitAndZeroBuffer(0, IV);
  if (ftempCypherDetails.BlockSize > 0) then begin
    // SDUInitAndZeroBuffer((ftempCypherDetails.BlockSize div 8), IV);
    IV := StringOfChar(AnsiChar(#0), (ftempCypherDetails.BlockSize div 8));

    localIV := ftempCypherEncBlockNo;

    for i := 1 to min(sizeof(localIV), length(IV)) do begin
      IV[i]   := Ansichar((localIV and $FF));
      localIV := localIV shr 8;
    end;

  end;

  // Adjust the sectorID so that this block of encrypted pseudorandom data
  // should be reasonably unique
  sectorID.QuadPart := ftempCypherEncBlockNo;

  // Encrypt the pseudorandom data generated
  if not (fOtfeFreeOtfeBase.EncryptSectorData(ftempCypherDriver, ftempCypherGUID,
    sectorID, FREEOTFE_v1_DUMMY_SECTOR_SIZE, ftempCypherKey, IV, plaintext, cyphertext))
  then begin
    SDUMessageDlg(
      _('Unable to encrypt pseudorandom data before using for overwrite buffer?!') +
      SDUCRLF + SDUCRLF + SDUParamSubstitute(_('Error #: %1'), [fOtfeFreeOtfeBase.LastErrorCode]),
      mtError
      );

    generatedOK := False;
  end else begin
    // Copy the encrypted data into the outputBlock
    for i := 0 to (bytesRequired - 1) do begin
      outputBlock[i] := Byte(cyphertext[i + 1]);
    end;

    generatedOK := True;
  end;

end;


function TfrmFreeOTFEMain.PortableModeSet(setTo: TPortableModeAction;
  suppressMsgs: Boolean): Boolean;
begin
  Result := False;

  if not ((fOtfeFreeOtfeBase as TOTFEFreeOTFE).CanUserManageDrivers()) then begin
    // On Vista, escalate UAC
    if SDUOSVistaOrLater() then begin
      UACEscalateForPortableMode(setTo, suppressMsgs);
    end else begin
      // On pre-Vista, just let user know they can't do it
      if not (suppressMsgs) then begin
        SDUMessageDlg(
          TEXT_NEED_ADMIN,
          mtWarning,
          [mbOK],
          0
          );
      end;
    end;

  end else begin
    case setTo of
      pmaStart:
      begin
        Result := _PortableModeStart(suppressMsgs);
      end;

      pmaStop:
      begin
        Result := _PortableModeStop(suppressMsgs);
      end;

      pmaToggle:
      begin
        Result := _PortableModeToggle(suppressMsgs);
      end;

    end;

    // In case we UAC escalated (i.e. ran a separate process to handle the
    // drivers), we send out a message to any other running instances of
    // FreeOTFE to refresh
{$IF CompilerVersion >= 18.5}
    //SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
    PostMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ELSE}
    SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$IFEND}
  end;


end;

procedure TfrmFreeOTFEMain.GetAllDriversUnderCWD(driverFilenames: TStringList);
const
  // Subdirs which should be searched for drivers
  DRIVERS_SUBDIR_32_BIT = '\x86';
  DRIVERS_SUBDIR_64_BIT = '\amd64';
var
  fileIterator: TSDUFileIterator;
  filename:     String;
  driversDir:   String;
begin
  // Compile a list of drivers in the CWD
  fileIterator := TSDUFileIterator.Create(nil);
  try
    driversDir := ExtractFilePath(Application.ExeName);
    if SDUOS64bit() then begin
      driversDir := driversDir + DRIVERS_SUBDIR_64_BIT;
    end else begin
      driversDir := driversDir + DRIVERS_SUBDIR_32_BIT;
    end;

    fileIterator.Directory          := driversDir;
    fileIterator.FileMask           := '*.sys';
    fileIterator.RecurseSubDirs     := False;
    fileIterator.OmitStartDirPrefix := False;
    fileIterator.IncludeDirNames    := False;

    fileIterator.Reset();
    filename := fileIterator.Next();
    while (filename <> '') do begin
      driverFilenames.Add(filename);
      filename := fileIterator.Next();
    end;

  finally
    fileIterator.Free();
  end;
end;

function TfrmFreeOTFEMain._PortableModeStart(suppressMsgs: Boolean): Boolean;
var
  driverFilenames: TStringList;
begin
  Result := False;

  driverFilenames := TStringList.Create();
  try
    GetAllDriversUnderCWD(driverFilenames);

    if (driverFilenames.Count < 1) then begin
      if not (suppressMsgs) then begin
        SDUMessageDlg(
          _('Unable to locate any portable DoxBox drivers.') + SDUCRLF +
          SDUCRLF + _(
          'Please ensure that the a copy of the DoxBox drivers (".sys" files) you wish to use are located in the correct directory.'),
          mtWarning
          );
      end;
    end else begin
      DeactivateFreeOTFEComponent();
      if (fOtfeFreeOtfeBase as TOTFEFreeOTFE).PortableStart(driverFilenames, not (suppressMsgs))
      then begin
        // Message commented out - it's pointless as user will know anyway because
        // either:
        //   a) They started it, or
        //   b) It's started automatically on FreeOTFE startup - in which case, it's
        //      just annoying
        //        if not(suppressMsgs) then
        //          begin
        //          SDUMessageDlg('Portable mode drivers installed and started.', mtInformation, [mbOK], 0);
        //          end;
        Result := True;
      end else begin
        if not (suppressMsgs) then begin
          SDUMessageDlg(
            _('One or more of your portable DoxBox drivers could not be installed/started.') +
            SDUCRLF + SDUCRLF + TEXT_NEED_ADMIN + SDUCRLF + SDUCRLF + _(
            'Please select "File | Drivers..." to check which drivers are currently operating.'),
            mtWarning
            );
        end;
      end;

    end;

  finally
    driverFilenames.Free();
    ActivateFreeOTFEComponent(suppressMsgs);
  end;
end;

function TfrmFreeOTFEMain._PortableModeStop(suppressMsgs: Boolean): Boolean;
var
  stopOK: Boolean;
begin
  Result := False;

  // Note that if the component was *not* active, then we can shutdown all
  // portable mode drivers; if we aren't active, this implies the main driver
  // isn't installed/running - in which case we don't need the user to confirm
  // as no drives can be mounted
  stopOK := True;
  if fOtfeFreeOtfeBase.Active then begin
    if (fOtfeFreeOtfeBase.CountDrivesMounted() > 0) then begin
      if suppressMsgs then begin
        stopOK := False;
      end else begin
        stopOK := (SDUMessageDlg(_('You have one or more volumes mounted.') +
          SDUCRLF + SDUCRLF + _(
          'If any of the currently mounted volumes makes use of any of the DoxBox drivers which are currently in portable mode, stopping portable mode is not advisable.') + SDUCRLF + SDUCRLF + _('It is recommended that you dismount all volumes before stopping portable mode.') + SDUCRLF + SDUCRLF + _('Do you wish to continue stopping portable mode?'), mtWarning, [mbYes, mbNo], 0) = mrYes);
      end;
    end;
  end;


  if stopOK then begin
    DeactivateFreeOTFEComponent();
    if (fOtfeFreeOtfeBase as TOTFEFreeOTFE).PortableStop() then begin
      // No point in informing user; they would have been prompted if they
      // wanted to do this, and they can tell they've stopped as the
      // application will just exit; only popping up a messagebox if there's a
      // problem
      if not (fshuttingDownFlag) then begin
        if not (suppressMsgs) then begin
          SDUMessageDlg(_('Portable mode drivers stopped and uninstalled.'),
            mtInformation, [mbOK], 0);
        end;
      end;

      Result := True;
    end else begin
      if not (suppressMsgs) then begin
        SDUMessageDlg(
          _('One or more of your portable DoxBox drivers could not be stopped/uninstalled.') +
          SDUCRLF + SDUCRLF + TEXT_NEED_ADMIN + SDUCRLF + SDUCRLF + _(
          'Please select "File | Drivers..." to check which drivers are currently operating.'),
          mtWarning
          );
      end;
    end;

    ActivateFreeOTFEComponent(suppressMsgs);
  end;
end;

function TfrmFreeOTFEMain._PortableModeToggle(suppressMsgs: Boolean): Boolean;
var
  cntPortable: Integer;
begin
  Result := False;

  cntPortable := (fOtfeFreeOtfeBase as TOTFEFreeOTFE).DriversInPortableMode();

  if (cntPortable < 0) then begin
    // We *should* be authorised to do this by the time we get here...
    // Do nothing; Result already set to FALSE
  end else
  if (cntPortable > 0) then begin
    Result := PortableModeSet(pmaStop, suppressMsgs);
  end else begin
    Result := PortableModeSet(pmaStart, suppressMsgs);
  end;
end;

procedure TfrmFreeOTFEMain.actDismountExecute(Sender: TObject);
begin
  DismountSelected();
end;

procedure TfrmFreeOTFEMain.actAboutExecute(Sender: TObject);
var
  dlg: TfrmAbout;
begin
  dlg := TfrmAbout.Create(self);
  try
    dlg.FreeOTFEObj := fOtfeFreeOtfeBase;
    dlg.BetaNumber  := APP_BETA_BUILD;
    dlg.Description := FREEOTFE_DESCRIPTION;
    dlg.ShowModal();
  finally
    dlg.Free();
  end;
end;

procedure TfrmFreeOTFEMain.actConsoleHideExecute(Sender: TObject);
begin
  inherited;
  SDUSystemTrayIcon1.DoMinimizeToIcon();
end;

procedure TfrmFreeOTFEMain.actDismountAllExecute(Sender: TObject);
begin
  DismountAll();
end;

procedure TfrmFreeOTFEMain.actPropertiesExecute(Sender: TObject);
begin
  DriveProperties();
end;

// Function required for both close and minimize to system tray icon
procedure TfrmFreeOTFEMain.actExitExecute(Sender: TObject);
begin
  // Either:
  //   Application.Terminate();
  // Or:
  fendSessionFlag := True;
  Close();
end;


// Function required for both close and minimize to system tray icon
procedure TfrmFreeOTFEMain.actConsoleDisplayExecute(Sender: TObject);
begin
  SDUSystemTrayIcon1.DoRestore();

end;


procedure TfrmFreeOTFEMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  userConfirm:          Word;
  oldActive:            Boolean;
  closingDrivesMounted: Integer;
begin
  CanClose := True;

  closingDrivesMounted := 0;

  // This code segment is required to handle the case when Close() is called
  // programatically
  // Only carry out this action if closing to SystemTrayIcon
  // Note the MainForm test; if it was not the main form, setting Action would
  // in FormClose would do the minimise
  if (not (fendSessionFlag) and Settings.OptSystemTrayIconDisplay and
    Settings.OptSystemTrayIconCloseTo and
{$IF CompilerVersion >= 18.5}
    (
    // If Application.MainFormOnTaskbar is set, use the form name,
    // otherwise check exactly
    (Application.MainFormOnTaskbar and (application.mainform <> nil) and
    (application.mainform.Name = self.Name)) or (not (Application.MainFormOnTaskbar) and
    //        (application.mainform = self)
    (application.mainform <> nil) and (application.mainform.Name = self.Name)))
{$ELSE}
    (Application.MainForm = self)
{$IFEND}
    ) then begin
    CanClose := False;
    actConsoleHide.Execute();
  end;

  if (CanClose) then begin
    oldActive := fOtfeFreeOtfeBase.Active;
    // We (quite reasonably) assume that if the FreeOTFE component is *not*
    // active, and we can't activate it, then no FreeOTFE volumes are mounted
    if not (fOtfeFreeOtfeBase.Active) then begin
      // Prevent any warnings...
      fshuttingDownFlag := True;
      ActivateFreeOTFEComponent(False);
      // Reset flag
      fshuttingDownFlag := False;
    end;

    if (fOtfeFreeOtfeBase.Active) then begin
      if (fOtfeFreeOtfeBase.CountDrivesMounted() > 0) then begin
        userConfirm := mrNo;
        if (Settings.OptOnExitWhenMounted = oewmPromptUser) then begin
          userConfirm := SDUMessageDlg(_('One or more volumes are still mounted.') +
            SDUCRLF + SDUCRLF + _(
            'Do you wish to dismount all volumes before exiting?'),
            mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        end else
        if (Settings.OptOnExitWhenMounted = oewmDismount) then begin
          userConfirm := mrYes;
        end else // oewmLeaveMounted
        begin
          // Do nothing - userConfirm already set to mrNo
        end;

        if (userConfirm = mrCancel) then begin
          CanClose := False;
        end else
        if (userConfirm = mrYes) then begin
          CanClose := DismountAll();
        end else begin
          // CanClose already set to TRUE; do nothing
        end;

      end;

      closingDrivesMounted := fOtfeFreeOtfeBase.CountDrivesMounted();
    end;  // if (fOtfeFreeOtfeBase.Active) then

    if not (oldActive) then begin
      // Prevent any warnings...
      fshuttingDownFlag := True;
      DeactivateFreeOTFEComponent();
      // Reset flag
      fshuttingDownFlag := False;
    end;

  end;


  if (CanClose) then begin
    // If there's no drives mounted, and running in portable mode, prompt for
    // portable mode shutdown
    if ((closingDrivesMounted <= 0) and
      ((fOtfeFreeOtfeBase as TOTFEFreeOTFE).DriversInPortableMode() > 0)) then begin
      userConfirm := mrNo;
      if (Settings.OptOnExitWhenPortableMode = oewpPromptUser) then begin
        userConfirm := SDUMessageDlg(
          _('One or more of the DoxBox drivers are running in portable mode.') +
          SDUCRLF + SDUCRLF + _('Do you wish to shutdown portable mode before exiting?'),
          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      end else
      if (Settings.OptOnExitWhenPortableMode = owepPortableOff) then begin
        userConfirm := mrYes;
      end else // oewpDoNothing
      begin
        // Do nothing - userConfirm already set to mrNo
      end;

      if (userConfirm = mrCancel) then begin
        CanClose := False;
      end else
      if (userConfirm = mrYes) then begin
        fshuttingDownFlag := True;
        CanClose          := PortableModeSet(pmaStop, False);
        fshuttingDownFlag := CanClose;
      end else begin
        CanClose := True;
      end;

    end;

  end;  // if (CanClose) then

  if CanClose then begin
    ShutdownPKCS11();
    SDUSystemTrayIcon1.Active := False;
    DestroySysTrayIconMenuitems();
    Application.ProcessMessages();
    Application.UnhookMainWindow(MessageHook);
    fOtfeFreeOtfeBase.Active := False;
  end else begin
    // Reset flag in case user clicked "Cancel" on tany of the above prompts...
    fendSessionFlag := False;
  end;

end;


 // Function required for close to system tray icon
 // Note: This is *only* required in your application is you intend to call
 //       Close() to hide/close the window. Calling Hide() instead is preferable
procedure TfrmFreeOTFEMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // This code segment is required to handle the case when Close() is called
  // programatically
  // Only carry out this action if closing to SystemTrayIcon
  // Note the MainForm test; if it was the main form, setting Action would have
  // no effect
  if (not (fendSessionFlag) and Settings.OptSystemTrayIconDisplay and
    Settings.OptSystemTrayIconCloseTo and
{$IF CompilerVersion >= 18.5}
    (
    // If Application.MainFormOnTaskbar is set, use the form name,
    // otherwise check exactly
    (Application.MainFormOnTaskbar and (application.mainform <> nil) and
    (application.mainform.Name = self.Name)) or (not (Application.MainFormOnTaskbar) and
    //        (application.mainform <> self)
    (application.mainform <> nil) and (application.mainform.Name = self.Name)))
{$ELSE}
    (Application.MainForm <> self)
{$IFEND}
    ) then begin
    Action := caMinimize;
  end;

end;


 // This is used to ensure that only one copy of FreeOTFE is running at any
 // given time
function TfrmFreeOTFEMain.MessageHook(var msg: TMessage): Boolean;
var
  hotKeyMsg: TWMHotKey;
begin
  result := False;

  if (msg.Msg = GLOBAL_VAR_WM_FREEOTFE_RESTORE) then begin
    // Restore application
    actConsoleDisplay.Execute();

    msg.Result := 0;
    result     := True;
  end else
  if (msg.Msg = GLOBAL_VAR_WM_FREEOTFE_REFRESH) then begin
    actRefreshExecute(nil);
    msg.Result := 0;
    result     := True;
  end else
  if (msg.Msg = WM_HOTKEY) then begin
    // Hotkey window message handler
    hotKeyMsg := TWMHotKey(msg);

    if fOtfeFreeOtfeBase.Active then begin
      if (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNT) then begin
        // We'll allow errors in dismount to be reported to the user; it's just
        // a normal dismount
        DismountAll(False);
      end else
      if (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNTEMERG) then begin
        // Panic! The user pressed the emergency dismount hotkey! Better not
        // report any errors to the user
        DismountAll(True);
      end;
    end;

    if ((hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNT) or (hotKeyMsg.HotKey =
      HOTKEY_IDENT_DISMOUNTEMERG)) then begin
      msg.Result := 0;
      result     := True;
    end;

  end;
end;

procedure TfrmFreeOTFEMain.miToolsClick(Sender: TObject);
begin
  inherited;

end;

// Cleardown and setup hotkeys
procedure TfrmFreeOTFEMain.SetupHotKeys();
begin
  // Don't attempt to setup hotkeys until *after* InitApp(...) is called - this
  // prevents any attempt at registering the hotkeys until FreeOTFE is going to
  // continue running (i.e. it's not just been started with commandline
  // options, and will exit shortly once they've been processed)
  if finitAppCalled then begin
    // Cleardown any existing hotkeys...
    DisableHotkey(HOTKEY_IDENT_DISMOUNT);
    DisableHotkey(HOTKEY_IDENT_DISMOUNTEMERG);

    // ...And setup hotkeys again
    if Settings.OptHKeyEnableDismount then begin
      EnableHotkey(
        Settings.OptHKeyKeyDismount,
        HOTKEY_IDENT_DISMOUNT
        );
    end;

    if Settings.OptHKeyEnableDismountEmerg then begin
      EnableHotkey(
        Settings.OptHKeyKeyDismountEmerg,
        HOTKEY_IDENT_DISMOUNTEMERG
        );
    end;
  end;

end;

 // Enable the specified shortcut as a hotkey, which will callback using the
 // given identifier
procedure TfrmFreeOTFEMain.EnableHotkey(hotKey: TShortCut; hotKeyIdent: Integer);
var
  modifiers:                    Integer;
  vk:                           Word;
  shiftState:                   TShiftState;
  toEarlyToDetectOtherInstance: Boolean;
  msg:                          String;
  msgType:                      TMsgDlgType;
begin
  if (hotkey = TextToShortCut(HOTKEY_TEXT_NONE)) then begin
    DisableHotkey(hotkeyIdent);
  end else begin
    // Obtain the virtual keycode and shiftState
    ShortCutToKey(hotkey, vk, shiftState);

    // Convet TShiftState into a modifier value for RegisterHotKey
    modifiers := 0;
    if (ssCtrl in shiftState) then begin
      modifiers := modifiers or MOD_CONTROL;
    end;
    if (ssAlt in shiftState) then begin
      modifiers := modifiers or MOD_ALT;
    end;
    if (ssShift in shiftState) then begin
      modifiers := modifiers or MOD_SHIFT;
    end;

    DisableHotkey(hotkeyIdent);
    if not (RegisterHotkey(Application.Handle, hotkeyIdent, modifiers, vk)) then begin
      // If FreeOTFE's already running, assume that the other instance has
      // already registered (taken) the hotkey

{$IF CompilerVersion >= 18.5}
      // See SDUGeneral._SDUDetectExistWindowDetails_ThisClassHandle(...)
      //  - if this is set, then SDUDetectExistingApp(...) don't be able to
      // detect any other running instance yet

      // Vista fix for Delphi 2007 and later
      toEarlyToDetectOtherInstance := False;
      if ((Application.Mainform = nil) and // Yes, "=" is correct here
        Application.MainFormOnTaskbar) then begin
        toEarlyToDetectOtherInstance := True;
      end;
{$ELSE}
      toEarlyToDetectOtherInstance := False;
{$IFEND}

      if not (toEarlyToDetectOtherInstance) then begin
        msg     := SDUParamSubstitute(_('Error: Unable to assign hotkey %1'),
          [ShortCutToText(hotkey)]);
        msgType := mtError;

        // It's not too early to detect other running instances, and there's
        // no other instances detected - so warn user the hotkey couldn't be
        // registered
        //
        // NOTE: THIS IS DEBATABLE - If *this* instance can't registered the
        //       hotkey due to another instance having already registered it,
        //       then the first instance is exited - the user has no hotkey
        //       support. On that basis, the user *should* be warned here -
        //       and the other instance detection should be removed.
        //
        // Because of this, we add a message to the warning if another instance
        // was running
        if (SDUDetectExistingApp() > 0) then begin
          msg     := msg + SDUCRLF + SDUCRLF +
            _('Another instance of DoxBox is running - the hotkey may already be assigned to that instance.');
          msgType := mtWarning;
        end;

        SDUMessageDlg(msg, msgType);
      end;
    end;

  end;

end;

// Disable the hotkey with the specifid identifier
procedure TfrmFreeOTFEMain.DisableHotkey(hotKeyIdent: Integer);
begin
  UnRegisterHotkey(Application.Handle, hotkeyIdent);
end;

function TfrmFreeOTFEMain.IsTestModeOn: Boolean;
var
  s: TStringList;
  r: Cardinal;
begin
  Result := False;
  s      := TStringList.Create();
  try
    r := SDUWinExecAndWaitOutput(SDUGetWindowsDirectory() + '\sysnative\bcdedit.exe',
      s, SDUGetWindowsDirectory());
    if r <> $FFFFFFFF then begin
      Result := s.IndexOf('testsigning             Yes') <> -1;
    end else begin
      SDUMessageDlg(_('Getting Test Mode status failed: ') + SysErrorMessage(GetLastError),
        mtError, [mbOK], 0);
    end;

  finally
    s.Free;
  end;

end;

function TfrmFreeOTFEMain.SetTestMode(silent, setOn: Boolean): Boolean;
begin
  inherited;
{
  run bcdedit.exe /set TESTSIGNING ON
  see https://stackoverflow.com/questions/16827229/file-not-found-error-launching-system32-winsat-exe-using-process-start for 'sysnative'
  this will change if built as 64 bit exe  "Alternatively, if you build your program as x64, you can leave the path as c:\windows\system32"
}
  // could also use SDUWow64DisableWow64FsRedirection
  Result := SDUWinExecAndWait32(SDUGetWindowsDirectory() +
    '\sysnative\bcdedit.exe /set TESTSIGNING ' + IfThen(setOn, 'ON', 'OFF'),
    SW_SHOWNORMAL) <> $FFFFFFFF;
  if not silent then begin
    if not Result then begin
      SDUMessageDlg(_('Set Test Mode failed: ') + SysErrorMessage(GetLastError),
        mtError, [mbOK], 0);
    end else begin
      if setOn then
        SDUMessageDlg(_('Please reboot. After rebooting the DoxBox drivers may be loaded'),
          mtInformation, [mbOK], 0)
      else
        SDUMessageDlg(_('After rebooting Test Mode will be off'), mtInformation, [mbOK], 0);
    end;
  end;
end;

function TfrmFreeOTFEMain.SetInstalled(): Boolean;
begin
  inherited;
  //needs to be place to save to
  if Settings.OptSaveSettings = slNone then
    Settings.OptSaveSettings := slProfile;//exe not supported in win >NT

  Settings.OptInstalled := True;
  Result                := Settings.Save();//todo:needed?
end;

procedure TfrmFreeOTFEMain.actTestModeOffExecute(Sender: TObject);
var
  SigningOS: Boolean;
begin
  inherited;
  SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
  if SigningOS then
    SetTestMode(False, False)
  else
    SDUMessageDlg(_('This version of Windows does not support test mode'), mtError, [mbOK], 0);
end;

procedure TfrmFreeOTFEMain.actTestModeOnExecute(Sender: TObject);
var
  SigningOS: Boolean;
begin
  inherited;
  SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
  if SigningOS then
    SetTestMode(False, True)
  else
    SDUMessageDlg(_('There is no need to set test mode on this version of Windows'),
      mtError, [mbOK], 0);
end;

procedure TfrmFreeOTFEMain.actTogglePortableModeExecute(Sender: TObject);
begin
  if (actTogglePortableMode.Enabled and (fcountPortableDrivers < 0)) then begin
    PortableModeSet(pmaToggle, False);
  end else
  if actTogglePortableMode.Checked then begin
    PortableModeSet(pmaStop, False);
  end else begin
    PortableModeSet(pmaStart, False);
  end;

  fcountPortableDrivers := (fOtfeFreeOtfeBase as TOTFEFreeOTFE).DriversInPortableMode();
  EnableDisableControls();

end;


procedure TfrmFreeOTFEMain.lvDrivesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  EnableDisableControls();

end;

procedure TfrmFreeOTFEMain.actFormatExecute(Sender: TObject);
var
  selDrives: DriveLetterString;
begin
  selDrives := GetSelectedDrives();
  Format_drive(selDrives, self);

  RefreshDrives();

end;



procedure TfrmFreeOTFEMain.actOptionsExecute(Sender: TObject);
var
  dlg: TfrmOptions_FreeOTFE;
begin
  inherited;

  dlg := TfrmOptions_FreeOTFE.Create(self);
  try
    dlg.OTFEFreeOTFEBase := fOtfeFreeOtfeBase;
    if (dlg.ShowModal() = mrOk) then begin
      ReloadSettings();
      EnableDisableControls();
    end;

  finally
    dlg.Free();
  end;

end;

procedure TfrmFreeOTFEMain.actOverwriteEntireDriveExecute(Sender: TObject);
var
  selectedDrive: DriveLetterString;
  selDrives:     DriveLetterString;
begin
  selDrives := GetSelectedDrives();
  if (length(selDrives) > 1) then begin
    SDUMessageDlg(
      _('Due to the destructive nature of overwriting entire drives, only one drive may be selected when this option is chosen.') + SDUCRLF + SDUCRLF + _('Please ensure that only one mounted drive is selected, and retry.')
      );
  end else begin
    selectedDrive := selDrives[1];

    if SDUWarnYN(_('WARNING!') + SDUCRLF + SDUCRLF + SDUParamSubstitute(
      _('You are attempting to overwrite drive: %1:'), [selectedDrive]) +
      SDUCRLF + SDUCRLF + SDUParamSubstitute(
      _('THIS WILL DESTROY ALL INFORMATION STORED ON DRIVE %1:, and require it to be reformatted'),
      [selectedDrive]) + SDUCRLF + SDUCRLF + _(
      'Are you ABSOLUTELY SURE you want to do this?'))
    then begin
      OverwriteDrives(selectedDrive, True);
    end;
  end;
end;


procedure TfrmFreeOTFEMain.actOverwriteFreeSpaceExecute(Sender: TObject);
var
  selDrives: DriveLetterString;
begin
  selDrives := GetSelectedDrives();
  OverwriteDrives(selDrives, False);
end;

 // overwriteEntireDrive - Set to TRUE to overwrite the entire drive; this will
 //                        destroy all data on the drive, and requiring it to be
 //                        reformatted. Set to FALSE to simply overwrite the
 //                        free space on the drive
procedure TfrmFreeOTFEMain.OverwriteDrives(drives: DriveLetterString;
  overwriteEntireDrive: Boolean);
var
  shredder:                   TShredder;
  i:                          Integer;
  currDrive:                  DriveLetterChar;
  frmOverWriteMethod:         TfrmFreeOTFESelectOverwriteMethod;
  overwriteWithEncryptedData: Boolean;
  allOK:                      Boolean;
  getRandomBits:              Integer;
  randomBuffer:               array of Byte;
  overwriteOK:                TShredResult;
  currDriveDevice:            String;
  failMsg:                    String;
  MouseRNGDialog1:            TMouseRNGDialog;
begin
  overwriteOK                := srSuccess;
  allOK                      := False;
  overwriteWithEncryptedData := False;

  frmOverWriteMethod := TfrmFreeOTFESelectOverwriteMethod.Create(self);
  try
    frmOverWriteMethod.OTFEFreeOTFEObj := (fOtfeFreeOtfeBase as TOTFEFreeOTFE);
    if (frmOverWriteMethod.ShowModal() = mrOk) then begin
      allOK := True;

      overwriteWithEncryptedData := frmOverWriteMethod.OverwriteWithEncryptedData;
      if (overwriteWithEncryptedData) then begin
        ftempCypherDriver := frmOverWriteMethod.CypherDriver;
        ftempCypherGUID   := frmOverWriteMethod.CypherGUID;

        fOtfeFreeOtfeBase.GetSpecificCypherDetails(ftempCypherDriver, ftempCypherGUID,
          ftempCypherDetails);

        // Initilize zeroed IV for encryption
        ftempCypherEncBlockNo := 0;

        // Get *real* random data for encryption key
        getRandomBits := 0;
        if (ftempCypherDetails.KeySizeRequired < 0) then begin
          // -ve keysize = arbitary keysize supported - just use 512 bits
          getRandomBits := 512;
        end else
        if (ftempCypherDetails.KeySizeRequired > 0) then begin
          // +ve keysize = specific keysize - get sufficient bits
          getRandomBits := ftempCypherDetails.KeySizeRequired;
        end;

        MouseRNGDialog1 := TMouseRNGDialog.Create();
        try


          { TODO 1 -otdk -cenhance : use cryptoapi etc if avail }
          if (getRandomBits > 0) then begin
            MouseRNGDialog1.RequiredBits := getRandomBits;
            allOK                        := MouseRNGDialog1.Execute();
          end;

          if (allOK) then begin
            SetLength(randomBuffer, (getRandomBits div 8));
            MouseRNGDialog1.RandomData(getRandomBits, randomBuffer);

            for i := low(randomBuffer) to high(randomBuffer) do begin
              SDUAddByte(ftempCypherKey, randomBuffer[i]);
              // Overwrite the temp buffer...
              randomBuffer[i] := random(256);
            end;
            SetLength(randomBuffer, 0);
          end;

        finally
          MouseRNGDialog1.Free;
        end;
      end;

    end;

  finally
    frmOverWriteMethod.Free();
  end;



  if (allOK) then begin
    if SDUConfirmYN(_('Overwriting on a large volume may take awhile.') +
      SDUCRLF + SDUCRLF + _('Are you sure you wish to proceed?')) then begin
      shredder := TShredder.Create(nil);
      try
        shredder.IntMethod := smPseudorandom;
        shredder.IntPasses := 1;

        if overwriteWithEncryptedData then begin
          // Note: Setting this event overrides shredder.IntMethod
          shredder.OnOverwriteDataReq := GenerateOverwriteData;
        end else begin
          shredder.OnOverwriteDataReq := nil;
        end;

        for i := 1 to length(drives) do begin
          currDrive := drives[i];

          if overwriteEntireDrive then begin
            currDriveDevice := '\\.\' + currDrive + ':';
            shredder.DestroyDevice(currDriveDevice, False, False);
            overwriteOK := shredder.LastIntShredResult;
          end else begin
            overwriteOK := shredder.OverwriteDriveFreeSpace(currDrive, False);
          end;

        end;

        if (overwriteOK = srSuccess) then begin
          SDUMessageDlg(_('Overwrite operation complete.'), mtInformation);
          if overwriteEntireDrive then begin
            SDUMessageDlg(_(
              'Please reformat the drive just overwritten before attempting to use it.'),
              mtInformation);
          end;
        end else
        if (overwriteOK = srError) then begin
          failMsg := _('Overwrite operation FAILED.');
          if not (overwriteEntireDrive) then begin
            failMsg := failMsg + SDUCRLF + SDUCRLF +
              _('Did you remember to format this drive first?');
          end;

          SDUMessageDlg(failMsg, mtError);
        end else
        if (overwriteOK = srUserCancel) then begin
          SDUMessageDlg(_('Overwrite operation cancelled by user.'), mtInformation);
        end else begin
          SDUMessageDlg(_('No drives selected to overwrite?'), mtInformation);
        end;


      finally
        shredder.Free();
      end;

    end;  // if (SDUMessageDlg(
  end;  // if (allOK) then

  ftempCypherDriver := '';
  ftempCypherGUID   := StringToGUID('{00000000-0000-0000-0000-000000000000}');
  SDUZeroBuffer(ftempCypherKey);
  //  ftempCypherKey        := '';
  ftempCypherEncBlockNo := 0;

end;

procedure TfrmFreeOTFEMain.tbbTogglePortableModeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // This is ugly, but required since if the button is clicked on, and the
  // operation cancelled, the button pops up - even though it shouldn't!
  tbbTogglePortableMode.down := (fcountPortableDrivers > 0);
end;


 // Handle "/portable" command line
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Portable(): eCmdLine_Exit;
var
  paramValue: String;
begin
  Result := ceSUCCESS;
  if SDUCommandLineParameter(CMDLINE_PORTABLE, paramValue) then begin
    Result := ceINVALID_CMDLINE;
    if (uppercase(paramValue) = uppercase(CMDLINE_TOGGLE)) then begin
      if PortableModeSet(pmaToggle, SDUCommandLineSwitch(CMDLINE_SILENT)) then begin
        Result := ceSUCCESS;
      end else begin
        Result := ceUNABLE_TO_START_PORTABLE_MODE;
      end;
    end else
    if ((uppercase(paramValue) = uppercase(CMDLINE_START)) or
      (uppercase(paramValue) = uppercase(CMDLINE_ON)) or (paramValue = '1')) then begin
      if PortableModeSet(pmaStart, SDUCommandLineSwitch(CMDLINE_SILENT)) then begin
        Result := ceSUCCESS;
      end else begin
        Result := ceUNABLE_TO_START_PORTABLE_MODE;
      end;
    end else
    if ((uppercase(paramValue) = uppercase(CMDLINE_STOP)) or
      (uppercase(paramValue) = uppercase(CMDLINE_OFF)) or (paramValue = '0')) then begin
      if PortableModeSet(pmaStop, SDUCommandLineSwitch(CMDLINE_SILENT)) then begin
        Result := ceSUCCESS;
      end else begin
        Result := ceUNABLE_TO_STOP_PORTABLE_MODE;
      end;
    end;
  end;
end;

 // install all drivers
 //
 // !! IMPORTANT !!
 // NOTICE: THIS DOES NOT CARRY OUT ANY UAC ESCALATION!
 //         User should use "runas DoxBox.exe ..." if UAC escalation
 //         required
 // !! IMPORTANT !!
 //
 // Returns: exit code
function TfrmFreeOTFEMain.InstallAllDrivers(driverControlObj: TOTFEFreeOTFEDriverControl;
  silent: Boolean): eCmdLine_Exit;
var
  driverFilenames: TStringList;
  //  vista64Bit: boolean;
begin

  Result := ceUNKNOWN_ERROR;

  if not IsTestModeOn then begin
    SDUMessageDlg(
      _('The DoxBox drivers canot be installed as ''Test Mode'' is not enabled, please see the documentation on manually enabling Windows Test Mode.'),
      mtError
      );
  end else begin

    driverFilenames := TStringList.Create();
    try
      GetAllDriversUnderCWD(driverFilenames);

      if (driverFilenames.Count <= 0) then begin
        Result := ceFILE_NOT_FOUND;
      end else begin
        Result := ceSUCCESS;
       {
          from delphi 7 project:"If under Vista x64, don't autostart the drivers after
          installing - this could cause a bunch of *bloody* *stupid*
          warning messages about "unsigned drivers" to be displayed by
          the OS"

          apparently on vista is not necesary to set test mode - but need to start drivers on os start.
          test mode is set anyway for all OS's so start drivers now whatever

      //          vista64Bit := (SDUOSVistaOrLater() and SDUOS64bit());
       }
        if driverControlObj.InstallMultipleDrivers(driverFilenames, False,
          not (silent), True// not(vista64Bit)
          ) then begin
            { from delphi 7 project:"If Vista x64, we tell the user to reboot. That way, the drivers
             will be started up on boot - and the user won't actually see
             any stupid warning messages about "unsigned drivers" "
            }
          if (
            // vista64Bit and
            not (silent)) then begin
            SDUMessageDlg(
              _('DoxBox drivers have been installed successfully.'),
              mtInformation
              );
          end;
        end else begin
          Result := ceUNKNOWN_ERROR;
        end;
      end;

    finally
      driverFilenames.Free();
    end;
  end;
end;

 // Handle "/install" command line
 //
 // !! IMPORTANT !!
 // NOTICE: THIS DOES NOT CARRY OUT ANY UAC ESCALATION!
 //         User should use "runas DoxBox.exe ..." if UAC escalation
 //         required
 // !! IMPORTANT !!
 //
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Install(
  driverControlObj: TOTFEFreeOTFEDriverControl): eCmdLine_Exit;
var
  paramValue:            String;
  driverPathAndFilename: String;
  silent:                Boolean;
  //  vista64Bit: boolean;
begin
  Result := ceINVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_FILENAME, paramValue) then begin
    silent := SDUCommandLineSwitch(CMDLINE_SILENT);

    if (uppercase(trim(paramValue)) = uppercase(CMDLINE_ALL)) then begin
      Result := InstallAllDrivers(driverControlObj, silent);
    end else begin
      // Convert any relative path to absolute
      driverPathAndFilename := SDURelativePathToAbsolute(paramValue);

      // Ensure driver file actually exists(!)
      if not (fileexists(driverPathAndFilename)) then begin
        Result := ceFILE_NOT_FOUND;
      end else begin
        Result := ceUNKNOWN_ERROR;
        if driverControlObj.InstallSetAutoStartAndStartDriver(paramValue) then begin
          Result := ceSUCCESS;
        end;
      end;
    end;
  end;

end;


 // Handle "/uninstall" command line
 //
 // !! IMPORTANT !!
 // NOTICE: THIS DOES NOT CARRY OUT ANY UAC ESCALATION!
 //         User should use "runas DoxBox.exe ..." if UAC escalation
 //         required
 // !! IMPORTANT !!
 //
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Uninstall(
  driverControlObj: TOTFEFreeOTFEDriverControl): eCmdLine_Exit;
var
  paramValue:           String;
  driveUninstallResult: DWORD;
  //    vista64Bit: boolean;
begin
  Result := ceINVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_DRIVERNAME, paramValue) then begin
    Result := ceUNKNOWN_ERROR;
    if (uppercase(trim(paramValue)) = uppercase(CMDLINE_ALL)) then begin
      Result := ceSUCCESS;
      //      vista64Bit := (SDUOSVistaOrLater() and SDUOS64bit());
      // set test mode off
      // if vista64Bit then if not SetTestMode(true,false) then cmdExitCode := ceUNKNOWN_ERROR;

      if not driverControlObj.UninstallAllDrivers(False) then begin
        Result := ceUNKNOWN_ERROR;
      end;
    end else begin
      driveUninstallResult := driverControlObj.UninstallDriver(paramValue);

      if ((driveUninstallResult and DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS) then begin
        Result := ceSUCCESS;
      end;
    end;

  end;

end;


// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_GUI(): eCmdLine_Exit;
begin
  actDriversExecute(nil);
  Result := ceSUCCESS;

end;

function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl(): eCmdLine_Exit;
var
  paramValue:       String;
  driverControlObj: TOTFEFreeOTFEDriverControl;
begin
  Result := ceSUCCESS;
  try
    if SDUCommandLineParameter(CMDLINE_DRIVERCONTROL, paramValue) then begin
      Result := ceINVALID_CMDLINE;
      // Special case; this one can UAC escalate
      if (uppercase(paramValue) = uppercase(CMDLINE_GUI)) then begin
        Result := HandleCommandLineOpts_DriverControl_GUI();
      end else begin
        // Creating this object needs admin privs; if the user doesn't have
        // these, it'll raise an exception
        driverControlObj := TOTFEFreeOTFEDriverControl.Create();
        try
          driverControlObj.Silent := SDUCommandLineSwitch(CMDLINE_SILENT);

          // This first test is redundant
          if (uppercase(paramValue) = uppercase(CMDLINE_GUI)) then begin
            Result := HandleCommandLineOpts_DriverControl_GUI();
          end else
          if (uppercase(paramValue) = uppercase(CMDLINE_COUNT)) then begin
            Result := eCmdLine_Exit(HandleCommandLineOpts_DriverControl_Count(driverControlObj));
          end else
          if (uppercase(paramValue) = uppercase(CMDLINE_INSTALL)) then begin
            Result := HandleCommandLineOpts_DriverControl_Install(driverControlObj);
          end else
          if (uppercase(paramValue) = uppercase(CMDLINE_UNINSTALL)) then begin
            Result := HandleCommandLineOpts_DriverControl_Uninstall(driverControlObj);
          end;

        finally
          driverControlObj.Free();
        end;
      end;

    end;

  except
    on E: EFreeOTFENeedAdminPrivs do begin
      Result := ceADMIN_PRIVS_NEEDED;
    end;
  end;

end;


 // Handle "/install" command line
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Count(
  driverControlObj: TOTFEFreeOTFEDriverControl): Integer;
var
  paramValue: String;
begin
  Result := Integer(ceINVALID_CMDLINE);

  // If "/type" not specified, default to the total
  if not (SDUCommandLineParameter(CMDLINE_TYPE, paramValue)) then begin
    paramValue := CMDLINE_TOTAL;
  end;

  if (uppercase(paramValue) = uppercase(CMDLINE_TOTAL)) then begin
    Result := driverControlObj.CountDrivers();
  end else
  if (uppercase(paramValue) = uppercase(CMDLINE_DRIVERSPORTABLE)) then begin
    Result := driverControlObj.CountDrivers(True);
  end else
  if (uppercase(paramValue) = uppercase(CMDLINE_DRIVERSINSTALLED)) then begin
    Result := driverControlObj.CountDrivers(False);
  end;

end;


 // Handle "/count" command line
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Count(): eCmdLine_Exit;
begin
  Result := ceSUCCESS;

  if SDUCommandLineSwitch(CMDLINE_COUNT) then begin
    if not (EnsureOTFEComponentActive) then
      Result := ceUNABLE_TO_CONNECT
    else
      Result := eCmdLine_Exit(fOtfeFreeOtfeBase.CountDrivesMounted());
  end;
end;

{ Handle "/settestmode" command line
 // Returns: Exit code
 // call if  CMDLINE_SET_TESTMODE is set or not - does check itself
 }
function TfrmFreeOTFEMain.HandleCommandLineOpts_SetTestMode(): eCmdLine_Exit;
var
  setOn:             Boolean;
  SigningOS, silent: Boolean;
  paramValue:        String;
begin
  Result := ceSUCCESS;
  if SDUCommandLineParameter(CMDLINE_SET_TESTMODE, paramValue) then begin

    setOn := False;
    if (uppercase(paramValue) = uppercase(CMDLINE_ON)) then
      setOn := True
    else
    if (uppercase(paramValue) <> uppercase(CMDLINE_OFF)) then
      Result := ceINVALID_CMDLINE;

    if Result = ceSUCCESS then begin
      SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
      silent    := SDUCommandLineSwitch(CMDLINE_SILENT);
      if SigningOS then begin
        if not SetTestMode(silent, setOn) then
          Result := ceUNABLE_TO_SET_TESTMODE;
      end else begin
        if not silent then
          SDUMessageDlg(_('This version of Windows does not support test mode'),
            mtError, [mbOK], 0);
      end;
    end;
  end;
end;


{ Handle "/SetInstalled" command
 // Returns: Exit code

 }
function TfrmFreeOTFEMain.HandleCommandLineOpts_SetInstalled(): eCmdLine_Exit;
begin
  Result := ceSUCCESS;

  if SDUCommandLineSwitch(CMDLINE_SET_INSTALLED) then begin

    if not SetInstalled() then
      Result := ceUNABLE_TO_SET_INSTALLED;
  end;
end;

 // Handle "/create" command line
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Create(): eCmdLine_Exit;
begin
  Result := ceSUCCESS;

  if SDUCommandLineSwitch(CMDLINE_CREATE) then begin
    if not (EnsureOTFEComponentActive) then
      Result := ceUNABLE_TO_CONNECT
    else
    if fOtfeFreeOtfeBase.CreateFreeOTFEVolumeWizard() then
      Result := ceSUCCESS
    else
      Result := ceUNKNOWN_ERROR;

  end;
end;


 // Handle "/unmount" command line
 // Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Dismount(): eCmdLine_Exit;
var
  paramValue: String;
  force:      Boolean;
  drive:      Char;
  dismountOK: Boolean;
begin
  Result := ceSUCCESS;

  if SDUCommandLineParameter(CMDLINE_DISMOUNT, paramValue) then begin
    if not (EnsureOTFEComponentActive) then
      Result := ceUNABLE_TO_CONNECT
    else begin
      force := SDUCommandLineSwitch(CMDLINE_FORCE);

      if (uppercase(paramValue) = uppercase(CMDLINE_ALL)) then begin
        dismountOK := (fOtfeFreeOtfeBase.DismountAll(force) = '');
      end else begin
        // Only use the 1st char...
        drive := paramValue[1];

        dismountOK := fOtfeFreeOtfeBase.Dismount(drive, force);
      end;

      if dismountOK then begin
        Result := ceSUCCESS;
      end else begin
        Result := ceUNABLE_TO_DISMOUNT;
      end;
    end;
  end;

end;

 // Handle any command line options; returns TRUE if command line options
 // were passed through, and "/noexit" wasn't specified as a command line
 // parameter
function TfrmFreeOTFEMain.HandleCommandLineOpts(out cmdExitCode: eCmdLine_Exit): Boolean;
var
  ignoreParams: Integer;
  settingsFile: String;
begin
  cmdExitCode := ceSUCCESS;

  fallowUACEsclation := not (SDUCommandLineSwitch(CMDLINE_NOUACESCALATE));
  // todo: a lot of repetition here bc HandleCommandLineOpts_ fns also call  SDUCommandLineParameter
  //todo -otdk: 'else' statements mean one cmd per call - allow compatible ones at same time

  //these cmds at least  are independent of others

  cmdExitCode := HandleCommandLineOpts_SetTestMode();

  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_EnableDevMenu();

  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_SetInstalled();

  // Driver control dialog
  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_DriverControl();

  // Portable mode on/off...
  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Portable();

  // All command line options below require FreeOTFE to be active before they
  // can be used

  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Count();
  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Create();
  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Mount();
  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Dismount();

  if fOtfeFreeOtfeBase.Active then
    DeactivateFreeOTFEComponent();


  // Notice: CMDLINE_MINIMIZE handled in the .dpr

  // Return TRUE if there were no parameters specified on the command line
  // and "/noexit" wasn't specified
  // Note: Disregard any CMDLINE_SETTINGSFILE and parameter
  // Note: Also disregard any CMDLINE_MINIMIZE and parameter
  Result := False;
  if not (SDUCommandLineSwitch(CMDLINE_NOEXIT)) then begin
    ignoreParams := 0;

    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFile) then begin
      Inc(ignoreParams);
      if (settingsFile <> '') then
        Inc(ignoreParams);
    end;

    if SDUCommandLineSwitch(CMDLINE_MINIMIZE) then
      Inc(ignoreParams);

    Result := (ParamCount > ignoreParams);
  end;

end;


procedure TfrmFreeOTFEMain.UACEscalateForDriverInstallation();
begin
  UACEscalate(CMDLINE_SWITCH_IND + CMDLINE_DRIVERCONTROL + ' ' + CMDLINE_GUI, False);
end;

procedure TfrmFreeOTFEMain.UACEscalateForPortableMode(portableAction: TPortableModeAction;
  suppressMsgs: Boolean);
var
  cmdLinePortableSwitch: String;
begin
  case portableAction of
    pmaStart:
    begin
      cmdLinePortableSwitch := CMDLINE_START;
    end;

    pmaStop:
    begin
      cmdLinePortableSwitch := CMDLINE_STOP;
    end;

    pmaToggle:
    begin
      cmdLinePortableSwitch := CMDLINE_TOGGLE;
    end;

  end;

  UACEscalate(
    CMDLINE_SWITCH_IND + CMDLINE_PORTABLE + ' ' + cmdLinePortableSwitch,
    suppressMsgs
    );

end;

procedure TfrmFreeOTFEMain.UACEscalate(cmdLineParams: String; suppressMsgs: Boolean);
const
  VERB_RUNAS = 'runas';
var
  filenameAndPath: String;
  cwd:             String;
begin
  // Only applicable in Windows Vista; simply warn user they can't continue on
  // earlier OSs
  if (not (SDUOSVistaOrLater()) or not (fallowUACEsclation)) then begin
    if not (suppressMsgs) then begin
      SDUMessageDlg(TEXT_NEED_ADMIN, mtError, [mbOK], 0);
    end;
  end else begin
    // Prevent endless loops of UAC escalation...
    cmdLineParams := CMDLINE_SWITCH_IND + CMDLINE_NOUACESCALATE + ' ' + cmdLineParams;

    // Message supression
    if suppressMsgs then begin
      cmdLineParams := CMDLINE_SWITCH_IND + CMDLINE_SILENT + ' ' + cmdLineParams;
    end;

    filenameAndPath := ParamStr(0);
    cwd             := ExtractFilePath(filenameAndPath);
    ShellExecute(
      self.Handle,
      VERB_RUNAS,
      PChar(filenameAndPath),
      PChar(cmdLineParams),
      PChar(cwd),
      SW_SHOW
      );

    // The UAC escalation/reexecution of FreeOTFE as UAC may have changed the
    // drivers installed/running; flush any caches in the FreeOTFE object
    fOtfeFreeOtfeBase.CachesFlush();
  end;

end;


procedure TfrmFreeOTFEMain.PKCS11TokenRemoved(SlotID: Integer);
var
  i:                Integer;
  volumeInfo:       TOTFEFreeOTFEVolumeInfo;
  drivesToDismount: DriveLetterString;
  mounted:          DriveLetterString;
begin
  SetStatusBarText(SDUParamSubstitute(_('Detected removal of token from slot ID: %1'), [SlotID]));
  if Settings.OptPKCS11AutoDismount then begin
    drivesToDismount := '';
    mounted          := fOtfeFreeOtfeBase.DrivesMounted();
    for i := 1 to length(mounted) do begin
      // ...Main FreeOTFE window list...
      if fOtfeFreeOtfeBase.GetVolumeInfo(mounted[i], volumeInfo) then begin
        if volumeInfo.MetaDataStructValid then begin
          if (volumeInfo.MetaDataStruct.PKCS11SlotID = SlotID) then begin
            drivesToDismount := drivesToDismount + mounted[i];
          end;
        end;
      end;
    end;

    SetStatusBarText(SDUParamSubstitute(_('Autodismounting drives: %1'),
      [PrettyPrintDriveLetters(drivesToDismount)]));
    DismountDrives(drivesToDismount, False);
  end;

end;


function TfrmFreeOTFEMain.GetSelectedDrives(): DriveLetterString;
var
  i:      Integer;
begin
  result := '';

  for i := 0 to (lvDrives.items.Count - 1) do begin
    if lvDrives.Items[i].Selected then begin
      result := result + GetDriveLetterFromLVItem(lvDrives.Items[i]);
    end;
  end;
end;

 //function TfrmFreeOTFEMain.OTFEFreeOTFE(): TOTFEFreeOTFE;
 //begin
 //  Result := TOTFEFreeOTFE(fOtfeFreeOtfeBase);
 //end;

procedure TfrmFreeOTFEMain.actFreeOTFEMountPartitionExecute(Sender: TObject);
var
  selectedPartition: String;
begin
  if fOtfeFreeOtfeBase.WarnIfNoHashOrCypherDrivers() then begin
    selectedPartition := fOtfeFreeOtfeBase.SelectPartition();

    if (selectedPartition <> '') then begin
      MountFilesDetectLUKS(
        selectedPartition,
        False,
        ftFreeOTFE
        );
    end;
  end;

end;

procedure TfrmFreeOTFEMain.actFreeOTFENewExecute(Sender: TObject);
var
  prevMounted:      DriveLetterString;
  newMounted:       DriveLetterString;
  createdMountedAs: DriveLetterString;
  msg:              WideString;
  i:                Integer;
begin
  inherited;

  prevMounted := fOtfeFreeOtfeBase.DrivesMounted;
  if not (fOtfeFreeOtfeBase.CreateFreeOTFEVolumeWizard()) then begin
    if (fOtfeFreeOtfeBase.LastErrorCode <> OTFE_ERR_USER_CANCEL) then begin
      SDUMessageDlg(_('DoxBox could not be created'), mtError);
    end;
  end else begin
    newMounted := fOtfeFreeOtfeBase.DrivesMounted;

    // If the "drive letters" mounted have changed, the new volume was
    // automatically mounted; setup for this
    if (newMounted = prevMounted) then begin
      msg := _('DoxBox created successfully.') + SDUCRLF + SDUCRLF +
        _('Please mount, and format this volume''s free space before use.');
    end else begin
      // Get new drive on display...
      RefreshDrives();

      createdMountedAs := '';
      for i := 1 to length(newMounted) do begin
        if (Pos(newMounted[i], prevMounted) = 0) then begin
          if (createdMountedAs <> '') then begin
            // If the user mounted another volume during volume creation, we
            // can't tell from here which was the new volume
            createdMountedAs := createdMountedAs + ' / ';
          end;

          createdMountedAs := createdMountedAs + newMounted[i] + ':';
        end;
      end;

      msg := SDUParamSubstitute(_('DoxBox created successfully and opened as: %1.'),
        [createdMountedAs]);
    end;

    SDUMessageDlg(msg, mtInformation);
  end;

end;

procedure TfrmFreeOTFEMain.actInstallExecute(Sender: TObject);
var
  driverControlObj: TOTFEFreeOTFEDriverControl;

begin
  inherited;
  driverControlObj := TOTFEFreeOTFEDriverControl.Create();
  try
    driverControlObj.Silent := SDUCommandLineSwitch(CMDLINE_SILENT);

    InstallAllDrivers(driverControlObj, False);

  finally
    driverControlObj.Free();
  end;

end;

procedure TfrmFreeOTFEMain.actLinuxMountPartitionExecute(Sender: TObject);
var
  selectedPartition: String;
  mountList:         TStringList;
begin
  if fOtfeFreeOtfeBase.WarnIfNoHashOrCypherDrivers() then begin
    selectedPartition := fOtfeFreeOtfeBase.SelectPartition();

    if (selectedPartition <> '') then begin
      mountList := TStringList.Create();
      try
        mountList.Add(selectedPartition);
        MountFiles(ftLinux, mountList, False, False);
      finally
        mountList.Free();
      end;

    end;
  end;

end;

end.
