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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin64, StdCtrls, ComCtrls, ImgList, ExtCtrls, Grids, Menus, OTFE_U,
  OTFEFreeOTFE_U, Shredder, MouseRNGDialog_U, ActnList,
  FreeOTFESettings, SDUSystemTrayIcon, ToolWin, Buttons, XPMan,
  OTFEFreeOTFE_DriverControl,
  pkcs11_library, SDUMRUList, SDUForms, SDUDialogs, OTFEFreeOTFEBase_U,
  CommonfrmMain, CommonSettings, SDUMultimediaKeys;


type
  TPortableModeAction = (pmaStart, pmaStop, pmaToggle);
  TAutorunType = (arPostMount, arPreDismount, arPostDismount);

  TfrmFreeOTFEMain = class(TfrmMain)
    ToolBar1: TToolBar;
    pnlTopSpacing: TPanel;
    ilDriveIcons: TImageList;
    lvDrives: TListView;
    pmDrives: TPopupMenu;
    miPopupDismount: TMenuItem;
    N2: TMenuItem;
    miPopupProperties: TMenuItem;
    N3: TMenuItem;
    miProperties: TMenuItem;
    N1: TMenuItem;
    miDismountAllMain: TMenuItem;
    miPopupDismountAll: TMenuItem;
    miFreeOTFEDrivers: TMenuItem;
    miOverwriteFreeSpace: TMenuItem;
    miFormat: TMenuItem;
    N6: TMenuItem;
    miPopupFormat: TMenuItem;
    miPopupOverwriteFreeSpace: TMenuItem;
    MouseRNGDialog1: TMouseRNGDialog;
    N7: TMenuItem;
    miPortableModeDrivers: TMenuItem;
    N8: TMenuItem;
    actDismountAll: TAction;
    actProperties: TAction;
    pmSystemTray: TPopupMenu;
    open1: TMenuItem;
    open2: TMenuItem;
    mountfile1: TMenuItem;
    mountpartition1: TMenuItem;
    dismountall1: TMenuItem;
    N10: TMenuItem;
    miOptions: TMenuItem;
    actConsoleDisplay: TAction;
    N11: TMenuItem;
    N12: TMenuItem;
    SDUSystemTrayIcon1: TSDUSystemTrayIcon;
    tbbNew: TToolButton;
    tbbMountFile: TToolButton;
    tbbMountPartition: TToolButton;
    tbbDismount: TToolButton;
    tbbDismountAll: TToolButton;
    actTogglePortableMode: TAction;
    actFormat: TAction;
    actOverwriteFreeSpace: TAction;
    tbbTogglePortableMode: TToolButton;
    actDrivers: TAction;
    ilDriveIconOverlay: TImageList;
    actOverwriteEntireDrive: TAction;
    Overwriteentiredrive1: TMenuItem;
    Overwriteentiredrive2: TMenuItem;
    N17: TMenuItem;
    mmRecentlyMounted: TMenuItem;
    actFreeOTFEMountPartition: TAction;
    actLinuxMountPartition: TAction;
    miFreeOTFEMountPartition: TMenuItem;
    miLinuxMountPartition: TMenuItem;
    actConsoleHide: TAction;
    actInstall: TAction;
    InstallDoxBox1: TMenuItem;
    actTestModeOn: TAction;
    SetTestMode1: TMenuItem;
    actTestModeOff: TAction;
    DisallowTestsigneddrivers1: TMenuItem;

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
    procedure lvDrivesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
    procedure miCreateKeyfileClick(Sender: TObject);
    procedure actInstallExecute(Sender: TObject);
    procedure actTestModeOnExecute(Sender: TObject);
    procedure actTestModeOffExecute(Sender: TObject);
  private


  protected
    TempCypherDriver: string;
    TempCypherGUID: TGUID;
    TempCypherDetails: TFreeOTFECypher_v3;
    TempCypherKey: Ansistring;
    TempCypherEncBlockNo: int64;

    IconMounted: TIcon;
    IconUnmounted: TIcon;

    // We cache this information as it take a *relativly* long time to get it
    // from the OTFE component.
    CountPortableDrivers: integer;

    AllowUACEsclation: boolean;

    function HandleCommandLineOpts_Portable(): integer;
    function HandleCommandLineOpts_DriverControl(): integer;
    function HandleCommandLineOpts_DriverControl_GUI(): integer;
    function HandleCommandLineOpts_DriverControl_Install(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
    function HandleCommandLineOpts_DriverControl_Uninstall(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
    function HandleCommandLineOpts_DriverControl_Count(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
    function HandleCommandLineOpts_Count(): integer;
    function HandleCommandLineOpts_Create(): integer; override;
    function HandleCommandLineOpts_Dismount(): integer;
    function HandleCommandLineOpts_SetTestMode(): integer;
    function HandleCommandLineOpts_SetInstalled(): integer;

    //true = set OK
    function SetTestMode(silent,SetOn:Boolean):Boolean;
    //sets 'installed' flag in ini file - returns false if fails (ini file must exist or be set as custom)
    function SetInstalled():Boolean;
    function InstallAllDrivers(driverControlObj: TOTFEFreeOTFEDriverControl;silent: boolean): integer;

    procedure ReloadSettings(); override;

    procedure SetupOTFEComponent(); override;
    function  ActivateFreeOTFEComponent(suppressMsgs: boolean): boolean; override;
    procedure DeactivateFreeOTFEComponent(); override;
    procedure ShowOldDriverWarnings(); override;

    procedure PKCS11TokenRemoved(SlotID: integer); override;

    procedure RefreshMRUList(); override;
    procedure MRUListItemClicked(mruList: TSDUMRUList; idx: integer); override;

    procedure InitializeDrivesDisplay();
    procedure RefreshDrives();
    function  DetermineDriveOverlay(volumeInfo: TOTFEFreeOTFEVolumeInfo): integer;
    function  AddIconForDrive(driveLetter: ansichar; overlayIdx: integer): integer;

    procedure SetStatusBarTextNormal();
    procedure EnableDisableControls(); override;
    function  GetDriveLetterFromLVItem(listItem: TListItem): ansichar;

    procedure ResizeWindow();

    function  GetSelectedDrives(): ansistring;
    procedure OverwriteDrives(drives: ansistring; overwriteEntireDrive: boolean);

    procedure ExploreDrive(driveLetter: Ansichar);

    procedure MountFiles(mountAsSystem: TDragDropFileType; filenames: TStringList; readOnly, forceHidden : Boolean); overload; override;

    procedure DriveProperties();

    function  DismountSelected(): boolean;
    function  DismountAll(isEmergency: boolean = FALSE): boolean;
    function  DismountDrives(dismountDrives: ansistring; isEmergency: boolean): boolean;
    procedure ReportDrivesNotDismounted(drivesRemaining: ansistring; isEmergency: boolean);
    
    procedure GetAllDriversUnderCWD(driverFilenames: TStringList);

    function  PortableModeSet(setTo: TPortableModeAction; suppressMsgs: boolean): boolean;
    function  _PortableModeStart(suppressMsgs: boolean): boolean;
    function  _PortableModeStop(suppressMsgs: boolean): boolean;
    function  _PortableModeToggle(suppressMsgs: boolean): boolean;

    // System tray icon related...
    procedure SystemTrayIconDismount(Sender: TObject);
    procedure DestroySysTrayIconMenuitems();

    // Hotkey related...
    procedure SetupHotKeys();
    procedure EnableHotkey(hotKey: TShortCut; hotKeyIdent: integer);
    procedure DisableHotkey(hotKeyIdent: integer);

    procedure RecaptionToolbarAndMenuIcons(); override;
    procedure SetIconListsAndIndexes(); override;
    procedure SetupToolbarFromSettings(); override;

    procedure DoSystemTrayAction(Sender: TObject; doAction: TSystemTrayClickAction);

    procedure UACEscalateForDriverInstallation();
    procedure UACEscalateForPortableMode(portableAction: TPortableModeAction; suppressMsgs: boolean);
    procedure UACEscalate(cmdLineParams: string; suppressMsgs: boolean);

    procedure AutoRunExecute(autorun: TAutorunType; driveLetter: ansichar; isEmergency: boolean);

  public
    // This next line will generate a compiler warning - this is harmless.
    // (We want to use OTFEFreeOTFE as the descendant class in this unit)
    function  OTFEFreeOTFE(): TOTFEFreeOTFE; overload;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMDeviceChange(var Msg : TMessage); message WM_DEVICECHANGE;
    procedure WMQueryEndSession(var msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var msg: TWMEndSession); message WM_ENDSESSION;

    procedure GenerateOverwriteData(
                                    Sender: TObject;
                                    passNumber: integer;
                                    bytesRequired: cardinal;
                                    var generatedOK: boolean;
                                    var outputBlock: TShredBlock
                                   );

    procedure InitApp(); override;

    function MessageHook(var msg: TMessage): boolean;

    procedure DisplayDriverControlDlg();

    // Handle any command line options; returns TRUE if command line options
    // were passed through
    function HandleCommandLineOpts(out cmdExitCode: integer): boolean; override;

  end;


const
  // Command line return values...
  CMDLINE_EXIT_UNABLE_TO_CONNECT             = 101;
  CMDLINE_EXIT_UNABLE_TO_DISMOUNT            = 103;
  CMDLINE_EXIT_UNABLE_TO_START_PORTABLE_MODE = 104;
  CMDLINE_EXIT_UNABLE_TO_STOP_PORTABLE_MODE  = 105;
  CMDLINE_EXIT_ADMIN_PRIVS_NEEDED            = 106;
  CMDLINE_EXIT_UNABLE_TO_SET_TESTMODE        = 107;
  CMDLINE_EXIT_UNABLE_TO_SET_INSTALLED       = 108;
  CMDLINE_EXIT_UNKNOWN_ERROR                 = 999;

  // Command line parameter handled in the .dpr
  CMDLINE_MINIMIZE         = 'minimize';

var
  frmFreeOTFEMain: TfrmFreeOTFEMain;
  GLOBAL_VAR_WM_FREEOTFE_RESTORE: cardinal;
  GLOBAL_VAR_WM_FREEOTFE_REFRESH: cardinal;

implementation

{$R *.DFM}
{ TODO -otdk -crefactor : editing dcr files is awkward - use imagelist instead }
{$R FreeOTFESystemTrayIcons.dcr}

uses
// delphi units
  ShellApi,  // Required for SHGetFileInfo
  Commctrl,  // Required for ImageList_GetIcon
  ComObj,  // Required for StringToGUID
  Math,  // Required for min
  strutils,
  //DoxBox units
  SDUGeneral,
  SDUGraphics,
  CommonfrmAbout,
  SDUi18n,
  FreeOTFEConsts,
  FreeOTFEfrmVolProperties,
  FreeOTFEfrmSelectOverwriteMethod,
  FreeOTFEfrmOptions,
  OTFEConsts_U,
  OTFEFreeOTFE_DriverAPI,
  SDUFileIterator_U,
  pkcs11_slot,
  OTFEFreeOTFE_PKCS11;

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
  AUTORUN_TITLE : array [TAutorunType] of string = (
                                                    AUTORUN_POST_MOUNT,
                                                    AUTORUN_PRE_MOUNT,
                                                    AUTORUN_POST_DISMOUNT
                                                   );

resourcestring
  FREEOTFE_DESCRIPTION = 'DoxBox: Open-Source On-The-Fly Encryption';

  TEXT_NEED_ADMIN = 'You need administrator privileges in order to carry out this operation.';

  // Toolbar captions...
  RS_TOOLBAR_CAPTION_MOUNTPARTITION  = 'Open partition Box';
  RS_TOOLBAR_CAPTION_DISMOUNTALL     = 'Lock all';
  RS_TOOLBAR_CAPTION_PORTABLEMODE    = 'Portable mode';
  // Toolbar hints...
  RS_TOOLBAR_HINT_MOUNTPARTITION     = 'Open a partition based Box';
  RS_TOOLBAR_HINT_DISMOUNTALL        = 'Lock all open Boxes';
  RS_TOOLBAR_HINT_PORTABLEMODE       = 'Toggle portable mode on/off';

  POPUP_DISMOUNT = 'Dismount %1: %2';

const
  // Format disk related functions ripped from the Unofficial Delphi FAQ (UDF)
  SHFMT_ID_DEFAULT        = $FFFF;
  // Formating options
  SHFMT_OPT_QUICKFORMAT   = $0000;
  SHFMT_OPT_FULL          = $0001;
  SHFMT_OPT_SYSONLY       = $0002;
  // Error codes
  SHFMT_ERROR             = $FFFFFFFF;
  SHFMT_CANCEL            = $FFFFFFFE;
  SHFMT_NOFORMAT          = $FFFFFFFD;

  R_ICON_MOUNTED   = 'MOUNTED';
  R_ICON_UNMOUNTED = 'UNMOUNTED';

  TAG_SYSTRAYICON_POPUPMENUITEMS           = $5000; // Note: LSB will be the drive letter
  TAG_SYSTRAYICON_POPUPMENUITEMS_DRIVEMASK =   $FF; // LSB mask for drive letter

  HOTKEY_TEXT_NONE = '(None)';
  HOTKEY_IDENT_DISMOUNT      = 1;
  HOTKEY_IDENT_DISMOUNTEMERG = 2;

  // Overlay icons; these MUST be kept in sync with the icons in
  // ilDriveIconOverlay
  OVERLAY_IMGIDX_NONE        = -1;
  OVERLAY_IMGIDX_PKCS11TOKEN =  0;
  OVERLAY_IMGIDX_LINUX       =  1;

  AUTORUN_SUBSTITUTE_DRIVE = '%DRIVE';

  // Command line parameters. case insensitive. generally only one action per invocation
  //swithces only 
  CMDLINE_NOUACESCALATE    = 'noUACescalate';
  CMDLINE_GUI              = 'gui';
  CMDLINE_FORCE            = 'force';
  CMDLINE_SET_INSTALLED    = 'SetInstalled';//sets 'installed' flag in ini file, creating if nec. - usually used with CMDLINE_SETTINGSFILE
  
  // cmds with params   
  CMDLINE_DRIVERCONTROL    = 'driverControl';
  CMDLINE_PORTABLE         = 'portable';
  CMDLINE_TOGGLE           = 'toggle';
  CMDLINE_DISMOUNT         = 'dismount';


//  CMDLINE_MOUNTED          = 'mounted'; unused    
  CMDLINE_SET_TESTMODE       ='SetTestMode';
  
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
  { TODO -otdk -cenhancement : set project homepage }
  URL_USERGUIDE_MAIN = 'http://DoxBox.eu/docs/Main';
  // PAD file URL...
  URL_PADFILE_MAIN = 'https://raw.githubusercontent.com/t-d-k/doxbox/master/PAD.xml';
  // Download URL...
  URL_DOWNLOAD_MAIN = 'http://DoxBox.eu/download.html';


// External function in shell32.dll
function SHFormatDrive(Handle: HWND; Drive, ID, Options: Word): LongInt;
            stdcall; external 'shell32.dll' name 'SHFormatDrive'


// This procedure is called after OnCreate, and immediatly before the
// Application.Run() is called
procedure TfrmFreeOTFEMain.InitApp();
var
  goForStartPortable: boolean;
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
  self.Top := ((Screen.Height - self.Height) div 2);
  self.Left := ((Screen.Width - self.Width) div 2);

  if not(ActivateFreeOTFEComponent(TRUE)) then
    begin
    goForStartPortable := Settings.OptAutoStartPortable;
    if not(goForStartPortable) then
      begin
      // if 'installed' then install drivers and prompt reboot else prompt portable as below
      if not Settings.OptInstalled then 
        goForStartPortable := SDUConfirmYN(
               _('The main DoxBox driver does not appear to be installed and running on this computer')+SDUCRLF+
               SDUCRLF+
               _('Would you like to start DoxBox in portable mode?')
              );
      end;

    if goForStartPortable then
      begin
      PortableModeSet(pmaStart, FALSE);
      end
    else
      begin
        if Settings.OptInstalled then 
        begin
          actInstallExecute(nil);
        end 
        else
          begin
            SDUMessageDlg(
               _('Please see the "installation" section of the accompanying documentation for instructions on how to install the DoxBox drivers.'),
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
  SetupPKCS11(FALSE);

end;


procedure TfrmFreeOTFEMain.RefreshDrives();
var
  ListItem: TListItem;
  driveIconNum: integer;
  i: integer;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  mounted: Ansistring;
  miTmp: TMenuItem;
  strVolID: string;
  overlayIdx: integer;
begin
  // Flush any caches in case a separate instance of FreeOTFE running from the
  // commandline, with escalated UAC changed the drivers installed/running
  OTFEFreeOTFE.CachesFlush();

  // Cleardown all...

  // ...Main FreeOTFE window list...
  lvDrives.Items.Clear();
  ilDriveIcons.Clear();

  // ...System tray icon drives list...
  DestroySysTrayIconMenuitems();

  // In case the drivers have been stopped/started recently, we bounce the
  // FreeOTFE component up and down (silently!)
  DeactivateFreeOTFEComponent();
  ActivateFreeOTFEComponent(TRUE);

  /// ...and repopulate
  if OTFEFreeOTFE.Active then
    begin
    mounted := OTFEFreeOTFE.DrivesMounted();

    for i:=1 to length(mounted) do
      begin
      // ...Main FreeOTFE window list...
      if OTFEFreeOTFE.GetVolumeInfo(mounted[i], volumeInfo) then
        begin
        ListItem := lvDrives.Items.Add;
        ListItem.Caption := '     '+mounted[i]+':';
        ListItem.SubItems.Add(volumeInfo.Filename);
        overlayIdx := DetermineDriveOverlay(volumeInfo);
        driveIconNum := AddIconForDrive(mounted[i], overlayIdx);
        if (driveIconNum>=0) then
          begin
          ListItem.ImageIndex := driveIconNum;
          end;
        end
      else
        begin
        // Strange... The drive probably got dismounted between getting the
        // list of mounted drives, and querying them for more information...
        end;


        // The volume ID/label
        strVolID := SDUVolumeID(mounted[i]);
        if strVolID<>'' then
          begin
          strVolID := '['+strVolID+']';
          end;

        // ...System tray icon popup menu...
        miTmp := TMenuItem.Create(nil);
        miTmp.Tag := TAG_SYSTRAYICON_POPUPMENUITEMS or ord(mounted[i]);
                                                      // We tag our menuitems
                                                      // in order that we can
                                                      // know which ones to
                                                      // remove later
        miTmp.OnClick := SystemTrayIconDismount;
        miTmp.AutoHotkeys := maManual;  // Otherwise the hotkeys added
                                        // automatically can interfere with
                                        // examing the caption to get the drive
                                        // letter when clicked on
        miTmp.Caption := SDUParamSubstitute(POPUP_DISMOUNT, [mounted[i], strVolID]);
        pmSystemTray.Items.Insert((i-1), miTmp);
      end;

      // Insert a divider into the system tray icon's popup menu after the
      // drives are listed
      if (length(mounted) > 0) then
        begin
        miTmp := TMenuItem.Create(nil);
        miTmp.Caption := '-';
        pmSystemTray.Items.Insert(length(mounted), miTmp);
        end;

    end;


  CountPortableDrivers := OTFEFreeOTFE.DriversInPortableMode();
  EnableDisableControls();

end;


// Tear down system tray icon popup menu menuitems previously added
procedure TfrmFreeOTFEMain.DestroySysTrayIconMenuitems();
var
  i: integer;
  miTmp: TMenuItem;
begin
  for i:=(pmSystemTray.Items.count-1) downto 0 do
    begin
    if ((pmSystemTray.Items[i].Tag and TAG_SYSTRAYICON_POPUPMENUITEMS) = TAG_SYSTRAYICON_POPUPMENUITEMS) then
      begin
      miTmp := pmSystemTray.Items[i];
      pmSystemTray.Items.Delete(i);
      miTmp.Free();
      end;
    end;
end;


procedure TfrmFreeOTFEMain.SystemTrayIconDismount(Sender: TObject);
var
  miDismount: TMenuItem;
  drive: ansichar;
begin
  miDismount := TMenuItem(Sender);

  drive := ansichar(miDismount.Tag and TAG_SYSTRAYICON_POPUPMENUITEMS_DRIVEMASK);

  DismountDrives(drive, FALSE);
end;


function TfrmFreeOTFEMain.DetermineDriveOverlay(volumeInfo: TOTFEFreeOTFEVolumeInfo): integer;
var
  retval: integer;
begin
  retval := OVERLAY_IMGIDX_NONE;

  if volumeInfo.MetaDataStructValid then
    begin
    if (volumeInfo.MetaDataStruct.PKCS11SlotID <> PKCS11_NO_SLOT_ID) then
      begin
      retval := OVERLAY_IMGIDX_PKCS11TOKEN;
      end
    else if volumeInfo.MetaDataStruct.LinuxVolume then
      begin
      retval := OVERLAY_IMGIDX_LINUX;
      end;
    end;

  Result := retval;
end;


// Add an icon to represent the specified drive
// overlayIdx - Set to an image index within ilDriveIconOverlay, or -1 for no
//              overlay
// Returns: The icon's index in ilDriveIcons, or -1 on error
function TfrmFreeOTFEMain.AddIconForDrive(driveLetter: ansichar; overlayIdx: integer): integer;
var
  iconIdx: integer;
  anIcon: TIcon;
  shfi: SHFileInfo;
  imgListHandle: THandle;
  iconHandle: HICON;
  tmpDrivePath: string;
  overlayIcon: TIcon;
  listBkColor: TColor;
begin
  iconIdx := -1;
  
  // Create a suitable icon to represent the drive
  tmpDrivePath := driveLetter+':\';
  imgListHandle := SHGetFileInfo(
                                 PChar(tmpDrivePath),
                                 0,
                                 shfi,
                                 sizeof(shfi),
                                 SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
//                                 SHGFI_DISPLAYNAME

  if imgListHandle<>0 then
    begin
    iconHandle := ImageList_GetIcon(imgListHandle, shfi.iIcon, ILD_NORMAL);
    if (iconHandle<>0) then
      begin
      anIcon:= TIcon.Create();
      try
        anIcon.ReleaseHandle();

        anIcon.handle := iconHandle;

        if (ilDriveIcons.Width<anIcon.Width) then
          begin
          ilDriveIcons.Width := anIcon.Width;
          end;
        if (ilDriveIcons.Height<anIcon.Height) then
          begin
          ilDriveIcons.Height := anIcon.Height;
          end;

        if (overlayIdx >= 0) then
          begin
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
        listBkColor := ilDriveIcons.BkColor;
        ilDriveIcons.BkColor := lvDrives.Color;
        iconIdx := ilDriveIcons.addicon(anIcon);
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
  lvDrives.SmallImages := ilDriveIcons;
  lvDrives.RowSelect:= TRUE;
  lvDrives.MultiSelect:= TRUE;
  lvDrives.showcolumnheaders := TRUE;
  lvDrives.ReadOnly := TRUE;
  lvDrives.ViewStyle := vsReport;
//    lvDrives.ViewStyle := vsIcon;
//    lvDrives.ViewStyle := vsList;
  lvDrives.IconOptions.Arrangement := iaTop;


  // Listview columns...
  tmpColumn := lvDrives.Columns.Add;
  tmpColumn.Caption := _('Drive');
  tmpColumn.width := 100;

//    tmpColumn.minwidth := lvDrives.width;
  tmpColumn := lvDrives.Columns.Add;
  tmpColumn.Caption := _('Box');
  tmpColumn.width := lvDrives.clientwidth - lvDrives.columns[0].width - ilDriveIcons.width;


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
      if self.Visible then
        begin
        actConsoleHide.Execute();
        end
      else
        begin
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

procedure TfrmFreeOTFEMain.SetIconListsAndIndexes();
begin
  inherited;
  
  if Settings.OptDisplayToolbarLarge then
    begin
    ToolBar1.Images := ilToolbarIcons_Large;

    tbbNew.ImageIndex                := FIconIdx_Large_New;
    tbbMountFile.ImageIndex          := FIconIdx_Large_MountFile;
    tbbMountPartition.ImageIndex     := FIconIdx_Large_MountPartition;
    tbbDismount.ImageIndex           := FIconIdx_Large_Dismount;
    tbbDismountAll.ImageIndex        := FIconIdx_Large_DismountAll;
    tbbTogglePortableMode.ImageIndex := FIconIdx_Large_PortableMode;
    end
  else
    begin
    ToolBar1.Images := ilToolbarIcons_Small;

    tbbNew.ImageIndex                := FIconIdx_Small_New;
    tbbMountFile.ImageIndex          := FIconIdx_Small_MountFile;
    tbbMountPartition.ImageIndex     := FIconIdx_Small_MountPartition;
    tbbDismount.ImageIndex           := FIconIdx_Small_Dismount;
    tbbDismountAll.ImageIndex        := FIconIdx_Small_DismountAll;
    tbbTogglePortableMode.ImageIndex := FIconIdx_Small_PortableMode;
    end;

  actDismountAll.ImageIndex            := FIconIdx_Small_DismountAll;
  actFreeOTFEMountPartition.ImageIndex := FIconIdx_Small_MountPartition;
  actTogglePortableMode.ImageIndex     := FIconIdx_Small_PortableMode;
  actProperties.ImageIndex             := FIconIdx_Small_Properties;

  // Set icons on menuitems
  if (FIconIdx_Small_VistaUACShield >= 0) then
    begin
    miFreeOTFEDrivers.ImageIndex := FIconIdx_Small_VistaUACShield;
    miPortableModeDrivers.ImageIndex := FIconIdx_Small_VistaUACShield;
    actFormat.ImageIndex := FIconIdx_Small_VistaUACShield;

    // Splat small Vista UAC shield icon onto large *portable mode* icon
//  lplp - todo - Note: 22x22 "World" icon may be too small to have UAC icon on top of it?
    end;

end;

procedure TfrmFreeOTFEMain.FormCreate(Sender: TObject);
begin
  // Prevent endless loops of UAC escalation...
  AllowUACEsclation := FALSE;

  OTFEFreeOTFEBase := TOTFEFreeOTFE.Create(nil);

  inherited;

  EndSessionFlag := FALSE;
  ShuttingDownFlag := FALSE;

  IconMounted:= TIcon.Create();
  IconMounted.Handle := LoadImage(hInstance, PChar(R_ICON_MOUNTED), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
  IconUnmounted:= TIcon.Create();
  IconUnmounted.Handle := LoadImage(hInstance, PChar(R_ICON_UNMOUNTED), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);

  CountPortableDrivers := -1;

  StatusBar_Status.SimplePanel := TRUE;

  // We set these to invisible so that if they're disabled by the user
  // settings, it doens't flicker on them off
  ToolBar1.Visible := FALSE;

  // Give the user a clue
  ToolBar1.ShowHint := TRUE;

  ToolBar1.Indent := 5;

end;

procedure TfrmFreeOTFEMain.FormDestroy(Sender: TObject);
begin
  inherited;

  // Note: If Settings is *not* set, this will be free'd off
  //       in FreeOTFE.dpr
  if Application.ShowMainForm then
    begin
    Settings.Free();
    end;

  IconMounted.Free();
  IconUnmounted.Free();

  OTFEFreeOTFE.Free();
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
  if lvDrives.columns.count>1 then
    begin
    tmpColumn := lvDrives.columns[1];
    tmpColumn.width := lvDrives.clientwidth - lvDrives.columns[0].width - ilDriveIcons.width;
    end;

end;

procedure TfrmFreeOTFEMain.SetupOTFEComponent();
begin
  inherited;

  OTFEFreeOTFE.DefaultDriveLetter := Settings.OptDefaultDriveLetter;
  OTFEFreeOTFE.DefaultMountAs := Settings.OptDefaultMountAs;

end;

// Reload settings, setting up any components as needed
procedure TfrmFreeOTFEMain.ReloadSettings();
begin
  inherited;

  SDUSystemTrayIcon1.Tip := Application.Title;

  SDUClearPanel(pnlTopSpacing);

  SDUSystemTrayIcon1.Active := Settings.OptSystemTrayIconDisplay;
  SDUSystemTrayIcon1.MinimizeToIcon := Settings.OptSystemTrayIconMinTo;

  SetupHotKeys();

  RefreshMRUList();

end;

procedure TfrmFreeOTFEMain.MountFiles(mountAsSystem: TDragDropFileType; filenames: TStringList; readOnly,forceHidden: boolean);
var
  i: integer;
  mountedAs: Ansistring;
  msg: string;
  mountedOK: boolean;
  prettyMountedAs: string;
begin
// Why was this in here?!
//  ReloadSettings();

  AddToMRUList(filenames);

  if (mountAsSystem = ftFreeOTFE) then
    begin
    mountedOK:= OTFEFreeOTFE.MountFreeOTFE(filenames, mountedAs, readOnly);
    end
  else if (mountAsSystem = ftLinux) then
    begin
    mountedOK:= OTFEFreeOTFE.MountLinux(filenames, mountedAs, readOnly,'','','',false,LINUX_KEYFILE_DEFAULT_NEWLINE,0,false,forceHidden);
    end
  else
    begin
    mountedOK:= OTFEFreeOTFE.Mount(filenames, mountedAs, readOnly);
    end;

  if not(mountedOK) then
    begin
    if (OTFEFreeOTFE.LastErrorCode <> OTFE_ERR_USER_CANCEL) then
      begin
      SDUMessageDlg(
                    _('Unable to open Box.')+SDUCRLF+
                    SDUCRLF+
                    _('Please check your keyphrase and settings, and try again.'),
                    mtError
                   );
      end;
    end
  else
    begin
    // Mount successful
    prettyMountedAs := PrettyPrintDriveLetters(mountedAs);
    if (CountValidDrives(mountedAs) = 1) then
      begin
      msg := SDUParamSubstitute(_('Your DoxBox has been opened as drive: %1'), [prettyMountedAs]);
      end
    else
      begin
      msg := SDUParamSubstitute(_('Your DoxBoxes have been opened as drives: %1'), [prettyMountedAs]);
      end;

    RefreshDrives();

    if Settings.OptPromptMountSuccessful then
      begin
      SDUMessageDlg(msg, mtInformation, [mbOK], 0);
      end;

    for i:=1 to length(mountedAs) do
      begin
      if Settings.OptExploreAfterMount then
        begin
        ExploreDrive(mountedAs[i]);
        end;

      AutoRunExecute(arPostMount, mountedAs[i], FALSE);
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

  mmRecentlyMounted.visible := (
                                (Settings.OptMRUList.MaxItems > 0) and
                                (Settings.OptMRUList.Items.Count > 0)
                               );

end;

procedure TfrmFreeOTFEMain.MRUListItemClicked(mruList: TSDUMRUList; idx: integer);
begin
  MountFilesDetectLUKS(mruList.Items[idx], FALSE, Settings.OptDragDropFileType);
end;

// Handle files being dropped onto the form from (for example) Windows Explorer.
// Files dropped onto the form should be treated as though the user is trying
// to mount them as volume files
procedure TfrmFreeOTFEMain.WMDropFiles(var Msg: TWMDropFiles);
var
  buffer: array[0..MAX_PATH] of Char;
  numFiles: longint;
  i: integer;
  filesToMount: TStringList;
begin
  try
    // Bring our window to the foreground
    SetForeGroundWindow(self.handle);

    // Handle the dropped files
    numFiles := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    filesToMount := TStringList.Create();
    try
      for i:=0 to (numFiles-1) do
        begin
        DragQueryFile(msg.Drop,
                      i,
                      @buffer,
                      sizeof(buffer));
        filesToMount.Add(buffer);
        end;

      MountFilesDetectLUKS(filesToMount, FALSE, Settings.OptDragDropFileType);

      Msg.Result := 0;
    finally
      filesToMount.Free();
    end;
  finally
    DragFinish(Msg.Drop);
  end;

end;


procedure TfrmFreeOTFEMain.WMDeviceChange(var Msg : TMessage);
begin
  RefreshDrives();

end;


// Function required for close to system tray icon
procedure TfrmFreeOTFEMain.WMQueryEndSession(var msg: TWMQueryEndSession);
begin
  // Default to allowing shutdown
  msg.Result := 1;
  EndSessionFlag := (msg.Result = 1);
  inherited;
  EndSessionFlag := (msg.Result = 1);
  
end;


// Function required for close to system tray icon
procedure TfrmFreeOTFEMain.WMEndSession(var msg: TWMEndSession);
begin
  EndSessionFlag := msg.EndSession;

end;


procedure TfrmFreeOTFEMain.DriveProperties();
var
  propertiesDlg: TfrmFreeOTFEVolProperties;
  i: integer;
  selDrives: ansistring;
begin
  selDrives := GetSelectedDrives();
  for i:=1 to length(selDrives) do
    begin
    propertiesDlg:= TfrmFreeOTFEVolProperties.Create(self);
    try
      propertiesDlg.DriveLetter := selDrives[i];
      propertiesDlg.OTFEFreeOTFE := OTFEFreeOTFE;
      propertiesDlg.ShowModal();
    finally
      propertiesDlg.Free();
    end;

    end;

end;


// If "isEmergency" is TRUE, the user will *not* be informed of any drives
// which couldn't be dismounted
function TfrmFreeOTFEMain.DismountAll(isEmergency: boolean = FALSE): boolean;
var
  drivesRemaining: ansistring;
  initialDrives: ansistring;
  i: integer;
begin
  // Change CWD to anywhere other than a mounted drive
  ChangeCWDToSafeDir();

  // Launch any pre-dismount executable
  initialDrives := OTFEFreeOTFE.DrivesMounted;
  for i:=1 to length(initialDrives) do
    begin
    AutoRunExecute(arPreDismount, initialDrives[i], isEmergency);
    end;

  OTFEFreeOTFE.DismountAll(isEmergency);

  // If it wasn't an emergency dismount, and drives are still mounted, report.
  drivesRemaining := OTFEFreeOTFE.DrivesMounted();

  for i:=1 to length(initialDrives) do
    begin
    // If the drive is no longer mounted, run any post-dismount executable
    if (Pos(initialDrives[i], drivesRemaining) <= 0) then
      begin
      AutoRunExecute(arPostDismount, initialDrives[i], isEmergency);
      end;
    end;

  if (drivesRemaining<>'') then
    begin
    if not(isEmergency) then
      begin
      RefreshDrives();
      ReportDrivesNotDismounted(drivesRemaining, FALSE);
      end
    else
      begin
      // Just give it another go... Best we can do.
      OTFEFreeOTFE.DismountAll(isEmergency);
      end;
    end;

  RefreshDrives();

  Result := (OTFEFreeOTFE.CountDrivesMounted <= 0);
end;


function TfrmFreeOTFEMain.DismountSelected(): boolean;
var
  toDismount: ansistring;
  allOK: boolean;
begin
  // First we build up a list of drives to dismount, then we dismount them.
  // This is done since we can't just run through the lvDrives looking for
  // selected items, as they're getting removed while we walk though the list!
  toDismount := GetSelectedDrives();

  allOK := DismountDrives(toDismount, FALSE);

  Result := allOK;
end;


// Dismount the drives specified
// This procedure *will* report drives which couldn't be mounted - regardless
// of "isEmergency"
function TfrmFreeOTFEMain.DismountDrives(dismountDrives: ansistring; isEmergency: boolean): boolean;
var
  i: integer;
  j: integer;
  subVols: ansistring;
  tmpDrv: ansichar;
  drivesRemaining: ansistring;
  allOK: boolean;
begin
  allOK := TRUE;
  drivesRemaining := '';

  // Change CWD to anywhere other than a mounted drive
  ChangeCWDToSafeDir();

  // Skip processing if no drives to dismount - otherwise it'll just flicker
  // the display
  if (dismountDrives <> '') then
    begin
    // Ensure that none of the drives to be dismounted contain one of the other
    // mounted drives
    // Although FreeOTFE wouldn't be able to dismount such a drive (it would
    // the mounted volume stored on it would have an open filehandles, so
    // preventing the dismount), it's clearly better to warn the user in a more
    // meaningful way than just "can't do it - files open"
    for i:=1 to length(dismountDrives) do
      begin
      tmpDrv := dismountDrives[i];

      subVols := OTFEFreeOTFE.VolsMountedOnDrive(tmpDrv);
      for j:=1 to length(subVols) do
        begin
        if (Pos(subVols[j], dismountDrives) = 0) then
          begin
          // At least one of the currently mounted drives is stored on one of
          // the drives to be dismounted - and we're not dismounting that drive
          SDUMessageDlg(
                        SDUParamSubstitute(_('The Box currently opened as %1: must be locked before %2: can be locked'), [subVols[j], tmpDrv]),
                        mtError
                       );
          allOK := FALSE;
          end;
        end;
      end;

    if allOK then
      begin
      // Sort out dismount order, in case one drive nested within another
      dismountDrives := OTFEFreeOTFE.DismountOrder(dismountDrives);


      for i:=1 to length(dismountDrives) do
        begin
        tmpDrv := dismountDrives[i];

        // Pre-dismount...
        AutoRunExecute(arPreDismount, tmpDrv, isEmergency);

        if OTFEFreeOTFE.Dismount(tmpDrv, isEmergency) then
          begin
          // Dismount OK, execute post-dismount...
          AutoRunExecute(arPostDismount, tmpDrv, isEmergency);
          end
        else
          begin
          drivesRemaining := drivesRemaining + tmpDrv;
          allOK := FALSE;
          end;

        end;

      if (drivesRemaining<>'') then
        begin
        RefreshDrives();
        ReportDrivesNotDismounted(drivesRemaining, isEmergency);
        end;

      RefreshDrives();
      end;
    end;

  Result := allOK;
end;


// Warn the user that some drives remain mounted, and if the dismount attempted
// wasn't an emergency dismount, then prompt the user if they want to attempt
// an emergency dismount
procedure TfrmFreeOTFEMain.ReportDrivesNotDismounted(drivesRemaining: ansistring; isEmergency: boolean);
var
  msg: string;
  warningOK: boolean;
  forceOK: boolean;
begin
  msg := SDUPluralMsg(
                      length(drivesRemaining),
                      SDUParamSubstitute(_('Unable to dismount drive: %1'), [PrettyPrintDriveLetters(drivesRemaining)]),
                      SDUParamSubstitute(_('Unable to dismount drives: %1'), [PrettyPrintDriveLetters(drivesRemaining)])
                     );

  // If the dismount attempted was a non-emergency dismount; prompt user if
  // they want to attempt an emergency dismount
  if not(isEmergency) then
    begin
    forceOK := FALSE;
    if (Settings.OptOnNormalDismountFail = ondfPromptUser) then
      begin
      msg := msg+SDUCRLF+
             SDUCRLF+
             SDUPluralMsg(
                          length(drivesRemaining),
                          _('Do you wish to force a dismount on this drive?'),
                          _('Do you wish to force a dismount on these drives?')
                         );
      forceOK := SDUConfirmYN(msg);
      end
    else if (Settings.OptOnNormalDismountFail = ondfForceDismount) then
      begin
      forceOK := TRUE;
      end
    else  // ondfCancelDismount
      begin
      // Do nothing - forceOK already set to FALSE
      end;

    if forceOK then
      begin
      warningOK := TRUE;
      if settings.OptWarnBeforeForcedDismount then
        begin
        warningOK := SDUWarnYN(
                       _('Warning: Emergency dismounts are not recommended')+SDUCRLF+
                       SDUCRLF+
                       _('Are you sure you wish to proceed?')
                      );
        end;

      if warningOK then
        begin
        DismountDrives(drivesRemaining, TRUE);
        end;

      end;
    end
  else
    begin
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
  drivesSelected: boolean;
  drivesMounted: boolean;
begin
  inherited;

  // The FreeOTFE object may not be active...
  actFreeOTFEMountPartition.Enabled := (OTFEFreeOTFE.Active and OTFEFreeOTFE.CanMountDevice());
  // Linux menuitem completely disabled if not active...
  actLinuxMountPartition.Enabled    := (miLinuxVolume.Enabled and OTFEFreeOTFE.CanMountDevice());

  // Flags used later
  drivesSelected := (lvDrives.selcount > 0);
  drivesMounted  := (lvDrives.Items.count > 0);

  // Actions & menuitems to be enabled/disabled, depending on whether a drive
  // is selected
  actDismount.enabled   := drivesSelected;
  actProperties.enabled := drivesSelected;

  miFormat.enabled                  := drivesSelected;
  miPopupFormat.enabled             := drivesSelected;
  actOverwriteFreeSpace.enabled     := drivesSelected;  // Note: Implies FreeOTFE drivers are running
  actOverwriteEntireDrive.enabled   := drivesSelected;  // Note: Implies FreeOTFE drivers are running

  // Action item to be enabled/disabled, as long as one or more drives are
  // mounted
  actDismountAll.enabled  := drivesMounted;

  // Driver handling...
  actTogglePortableMode.Checked := (CountPortableDrivers > 0);
  actTogglePortableMode.Enabled := (
                                    OTFEFreeOTFE.CanUserManageDrivers() or
                                    // If we're on Vista, always allow the
                                    // toggle portable mode option; the
                                    // user can escalate UAC if needed
                                    SDUOSVistaOrLater()
                                   );
  // If the portable mode control is enabled, but we don't know how many
  // drivers there are in portable mode, presumably we're running under
  // Vista without admin access - in which case this control just toggles the
  // portable mode status - and DOESN'T turn it on/off
  if (
      actTogglePortableMode.Enabled and
      (CountPortableDrivers < 0)
     ) then
    begin
    actTogglePortableMode.Caption := _('Toggle portable mode drivers');
    end
  else
    begin
    actTogglePortableMode.Caption := _('Use portable mode drivers');

    // Ensure NO ICON FOR PORTABLE MODE DRIVERS if the user is admin; it shows
    // a checkbox instead, depending on whether the drivers are running or not
    miPortableModeDrivers.ImageIndex := -1;
    end;


  // Misc display related...
  if drivesMounted then
    begin
    SDUSystemTrayIcon1.Icon := IconMounted;
    end
  else
    begin
    SDUSystemTrayIcon1.Icon := IconUnmounted;
    end;

  StatusBar_Status.Visible := Settings.OptDisplayStatusbar;
  StatusBar_Hint.Visible := FALSE;

  SetStatusBarTextNormal();

  SetupToolbarFromSettings();

end;

procedure TfrmFreeOTFEMain.SetStatusBarTextNormal();
var
  drvLetter: char;
  freeSpace: int64;
  totalSpace: int64;
  statusText: string;
  selDrives: string;
begin
  statusText := '';

  selDrives := GetSelectedDrives();

  // Update status bar text, if visible
  if (StatusBar_Status.Visible or StatusBar_Hint.Visible) then
    begin
    statusText := '';
    if not(OTFEFreeOTFE.Active) then
      begin
      statusText := _('FreeOTFE main driver not connected.');
      end
    else if (length(selDrives) = 0) then
      begin
      statusText := SDUPluralMsg(
                                 lvDrives.Items.Count,
                                 SDUParamSubstitute(_('%1 drive mounted.'), [lvDrives.Items.Count]),
                                 SDUParamSubstitute(_('%1 drives mounted.'), [lvDrives.Items.Count])
                                );
      end
    else if (length(selDrives) > 0) then
      begin
      if (length(selDrives) = 1) then
        begin
        drvLetter := selDrives[1];
        freeSpace := DiskFree(Ord(drvLetter) - 64);
        totalSpace := DiskSize(Ord(drvLetter) - 64);

        if (
            (freeSpace > -1) and
            (totalSpace > -1)
           ) then
          begin
          statusText := SDUParamSubstitute(
                               _('%1: Free Space: %2; Total size: %3'),
                               [
                                drvLetter,
                                SDUFormatUnits(
                                               freeSpace,
                                               SDUUnitsStorageToTextArr(),
                                               UNITS_BYTES_MULTIPLIER,
                                               1
                                              ),
                                SDUFormatUnits(
                                               totalSpace,
                                               SDUUnitsStorageToTextArr(),
                                               UNITS_BYTES_MULTIPLIER,
                                               1
                                              )
                                ]
                               );
          end;
        end;

      if (statusText = '') then
        begin
        statusText := SDUPluralMsg(
                                   lvDrives.SelCount,
                                   SDUParamSubstitute(_('%1 drive selected.'), [length(selDrives)]),
                                   SDUParamSubstitute(_('%1 drives selected.'), [length(selDrives)])
                                  );
        end;

      end;

    end;

  SetStatusBarText(statusText);
end;

// Set the toolbar size
procedure TfrmFreeOTFEMain.SetupToolbarFromSettings();
var
  toolbarWidth: integer;
  i: integer;
begin
  SetIconListsAndIndexes();

  ToolBar1.Visible := Settings.OptDisplayToolbar;

  // Toolbar captions...
  if Settings.OptDisplayToolbarLarge then
    begin
    ToolBar1.ShowCaptions := Settings.OptDisplayToolbarCaptions;
    end
  else
    begin
    ToolBar1.ShowCaptions :=  FALSE;
    end;

  ToolBar1.Height       := ToolBar1.Images.Height + TOOLBAR_ICON_BORDER;
  ToolBar1.ButtonHeight := ToolBar1.Images.Height;
  ToolBar1.ButtonWidth  := ToolBar1.Images.Width;

  if Settings.OptDisplayToolbar then
    begin
    // Turning captions on can cause the buttons to wrap if the window isn't big
    // enough.
    // This looks pretty poor, so resize the window if it's too small
    toolbarWidth := 0;
    for i:=0 to (Toolbar1.ButtonCount - 1) do
      begin
      toolbarWidth := toolbarWidth + Toolbar1.Buttons[i].Width;
      end;
    if (toolbarWidth > self.Width) then
      begin
      self.Width := toolbarWidth;
      end;
    // Adjusting the width to the sum of the toolbar buttons doens't make it wide
    // enough to prevent wrapping (presumably due to window borders); nudge the
    // size until it does
    // (Crude, but effective)
    while (Toolbar1.RowCount > 1) do
      begin
      self.Width := self.Width + 10;
      end;
    end;

end;

function TfrmFreeOTFEMain.GetDriveLetterFromLVItem(listItem: TListItem): ansichar;
var
  tmpDrv: string;
begin
  // Trim the padding whitespace off the drive letter + colon
  tmpDrv := listItem.Caption;
  tmpDrv := TrimLeft(tmpDrv);

  // The first letter of the item's caption is the drive letter
  Result :=ansichar( tmpDrv[1]);

end;

procedure TfrmFreeOTFEMain.lvDrivesDblClick(Sender: TObject);
var
  driveLetter: ansichar;
begin
  if (lvDrives.selcount > 0) then
    begin
    driveLetter := GetDriveLetterFromLVItem(lvDrives.selected);
    ExploreDrive(driveLetter);
    end;

end;

// Launch autorun executable on specified drive
procedure TfrmFreeOTFEMain.AutoRunExecute(autorun: TAutorunType; driveLetter: ansichar; isEmergency: boolean);
var
  exeFullCmdLine: ansistring;
  launchOK: boolean;
  splitCmdLine: TStringList;
  exeOnly: string;
begin
  // Sanity...
  if (driveLetter = #0) then
    begin
    exit;
    end;

  exeFullCmdLine := '';
  
  case autorun of

    arPostMount:
      begin
      exeFullCmdLine := Settings.OptPostMountExe;
      end;

    arPreDismount:
      begin
      // We don't bother with predismount in case of emergency
      if not(isEmergency) then
        begin
        exeFullCmdLine := Settings.OptPreDismountExe;
        end;
      end;

    arPostDismount:
      begin
      exeFullCmdLine := Settings.OptPostDismountExe;
      end;

    else
      begin
      if not(isEmergency) then
        begin
        SDUMessageDlg(_('Unknown autorun type?!'), mtError);
        end;
      end;

    end;

  if (exeFullCmdLine <> '') then
    begin
    // Split up, in case user is using a commandline with spaces in the
    // executable path
    splitCmdLine:= TStringList.Create();
    try
      splitCmdLine.QuoteChar := '"';
      splitCmdLine.Delimiter := ' ';
      splitCmdLine.DelimitedText := trim(exeFullCmdLine);

      // Relative path with mounted drive letter
      if (
          (autorun = arPostMount) or
          (autorun = arPreDismount)
         ) then
        begin
        splitCmdLine[0] := driveLetter + ':' + splitCmdLine[0];
        end;

      exeOnly := splitCmdLine[0];
      
      // Recombine to produce new commandline
      exeFullCmdLine := splitCmdLine.DelimitedText;

    finally
      splitCmdLine.Free();
    end;

    // Perform substitution, if needed
    exeFullCmdLine := StringReplace(
                                    exeFullCmdLine,
                                    AUTORUN_SUBSTITUTE_DRIVE,
                                    driveLetter,
                                    [rfReplaceAll]
                                   );

    // NOTE: THIS MUST BE UPDATED IF CMDLINE IS TO SUPPORT COMMAND LINE
    //       PARAMETERS!
    if not(FileExists(exeOnly)) then
      begin
      if (
          not(isEmergency) and
          Settings.OptPrePostExeWarn
         ) then
        begin
        SDUMessageDlg(
                      SDUParamSubstitute(_('Unable to locate %1 executable:'), [AUTORUN_TITLE[autorun]])+SDUCRLF+
                      SDUCRLF+
                      exeOnly,
                      mtWarning
                     );
        end;
      end
    else
      begin
      if (autorun = arPreDismount) then
        begin
        // Launch and block until terminated...
        launchOK := (SDUWinExecAndWait32(exeFullCmdLine, SW_SHOW, driveLetter+':\') <> $FFFFFFFF);
        end
      else
        begin
        // Fire and forget...
        launchOK := (WinExec(PAnsiChar(exeFullCmdLine), SW_RESTORE) >= 31)
        end;

      if not(launchOK) then
        begin
        if not(isEmergency) then
          begin
          SDUMessageDlg(
                        SDUParamSubstitute(_('Error running %1 executable:'), [AUTORUN_TITLE[autorun]])+SDUCRLF+
                        SDUCRLF+
                        exeFullCmdLine,
                        mtError
                       );
          end;
        end;
      end;
    end;

end;


procedure TfrmFreeOTFEMain.ExploreDrive(driveLetter: Ansichar);
var
  explorerCommandLine: Ansistring;
begin
  if (driveLetter <> #0) then
    begin
    explorerCommandLine := Ansistring('explorer ')+driveLetter+Ansistring(':\');

    if (WinExec(PAnsiChar(explorerCommandLine), SW_RESTORE))<31 then
      begin
      SDUMessageDlg(_('Error running Explorer'), mtError, [mbOK], 0);
      end;
    end;

end;

procedure TfrmFreeOTFEMain.DisplayDriverControlDlg();
var
  UACEscalateAttempted: boolean;
begin
  UACEscalateAttempted := FALSE;

  DeactivateFreeOTFEComponent();
  try
    try
      // This is surrounded by a try...finally as it may throw an exception if
      // the user doesn't have sufficient privs to do this
      OTFEFreeOTFE.ShowDriverControlDlg();

      // Send out a message to any other running instances of FreeOTFE to
      // refresh; the drivers may have changed.
      // Note that if we had to UAC escalate (see exception below), the UAC
      // escalated process will send out this windows message
{$IFDEF VER185}
      //SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
      PostMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ELSE}
      SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ENDIF}
    except
      on EFreeOTFENeedAdminPrivs do
        begin
        UACEscalateForDriverInstallation();
        UACEscalateAttempted := TRUE;
        end;
    end;

  finally
    // Note: We supress any messages that may be the result of failing to
    //       activate the OTFEFreeOTFE component activating the if we had to
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

function TfrmFreeOTFEMain.ActivateFreeOTFEComponent(suppressMsgs: boolean): boolean;
begin
  inherited ActivateFreeOTFEComponent(suppressMsgs);

  // Let Windows know whether we accept dropped files or not
  DragAcceptFiles(self.Handle, OTFEFreeOTFE.Active);

  Result := OTFEFreeOTFE.Active;
end;

procedure TfrmFreeOTFEMain.ShowOldDriverWarnings();
begin
{ TODO 1 -otdk -cclean : dont support old drivers }
  // No warnings to be shown in base class
  if (OTFEFreeOTFE.Version() < FREEOTFE_ID_v03_00_0000) then
    begin
    SDUMessageDlg(
               SDUParamSubstitute(_('The main DoxBox driver installed on this computer dates back to FreeOTFE %1'), [OTFEFreeOTFE.VersionStr()])+SDUCRLF+
               SDUCRLF+
               _('It is highly recommended that you upgrade your DoxBox drivers to v3.00 or later as soon as possible, in order to allow the use of LRW and XTS based volumes.')+SDUCRLF+
               SDUCRLF+
               _('See documentation (installation section) for instructions on how to do this.'),
               mtWarning
              );
    end
  else if (OTFEFreeOTFE.Version() < FREEOTFE_ID_v04_30_0000) then
    begin
    SDUMessageDlg(
               SDUParamSubstitute(_('The main DoxBox driver installed on this computer dates back to FreeOTFE %1'), [OTFEFreeOTFE.VersionStr()])+SDUCRLF+
               SDUCRLF+
               _('It is highly recommended that you upgrade your DoxBox drivers to those included in v4.30 or later as soon as possible, due to improvements in the main driver.')+SDUCRLF+
               SDUCRLF+
               _('See documentation (installation section) for instructions on how to do this.'),
               mtWarning
              );
    end
  else
    begin
    inherited;
    end;

end;

procedure TfrmFreeOTFEMain.DeactivateFreeOTFEComponent();
begin
  inherited;

  // Let Windows know we accept dropped files or not
  DragAcceptFiles(self.Handle, OTFEFreeOTFE.Active);

end;


// The array passed in is zero-indexed; populate elements zero to "bytesRequired"
procedure TfrmFreeOTFEMain.GenerateOverwriteData(
                                    Sender: TObject;
                                    passNumber: integer;
                                    bytesRequired: cardinal;
                                    var generatedOK: boolean;
                                    var outputBlock: TShredBlock
                                   );
var
  i: integer;
  tempArraySize: cardinal;
  blocksizeBytes: cardinal;
  plaintext: Ansistring;
  cyphertext: Ansistring;
  IV: Ansistring;
  localIV: int64;
  sectorID: LARGE_INTEGER;
begin
  // Generate an array of random data containing "bytesRequired" bytes of data,
  // plus additional random data to pad out to the nearest multiple of the
  // cypher's blocksize bits
  // Cater for if the blocksize was -ve or zero
  if (TempCypherDetails.BlockSize < 1) then
    begin
    blocksizeBytes := 1;
    end
  else
    begin
    blocksizeBytes := (TempCypherDetails.BlockSize div 8);
    end;
  tempArraySize := bytesRequired + (blocksizeBytes - (bytesRequired mod blocksizeBytes));

  plaintext := '';
  for i:=1 to tempArraySize do
    begin
    plaintext := plaintext + Ansichar(random(256));
    { TODO 2 -otdk -csecurity : This is not secure PRNG - check }
    end;


  inc(TempCypherEncBlockNo);

  // Adjust the IV so that this block of encrypted pseudorandom data should be
  // reasonably unique
  IV := '';
  if (TempCypherDetails.BlockSize > 0) then
    begin
    IV := StringOfChar(AnsiChar(#0), (TempCypherDetails.BlockSize div 8));

    localIV := TempCypherEncBlockNo;

    for i:=1 to min(sizeof(localIV), length(IV)) do
      begin
      IV[i] := Ansichar((localIV AND $FF));
      localIV := localIV shr 8;
      end;

    end;

  // Adjust the sectorID so that this block of encrypted pseudorandom data
  // should be reasonably unique
  sectorID.QuadPart := TempCypherEncBlockNo;

  // Encrypt the pseudorandom data generated
  if not(OTFEFreeOTFE.EncryptSectorData(
                                  TempCypherDriver,
                                  TempCypherGUID,
                                  sectorID,
                                  FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                  TempCypherKey,
                                  IV,
                                  plaintext,
                                  cyphertext
                                 )) then
    begin
    SDUMessageDlg(
               _('Unable to encrypt pseudorandom data before using for overwrite buffer?!')+SDUCRLF+
               SDUCRLF+
               SDUParamSubstitute(_('Error #: %1'), [OTFEFreeOTFE.LastErrorCode]),
               mtError
              );

    generatedOK := FALSE;
    end
  else
    begin
    // Copy the encrypted data into the outputBlock
    for i:=0 to (bytesRequired-1) do
      begin
      outputBlock[i] := byte(cyphertext[i+1]);
      end;

    generatedOK := TRUE;
    end;

end;


function TfrmFreeOTFEMain.PortableModeSet(setTo: TPortableModeAction; suppressMsgs: boolean): boolean;
var
  retval: boolean;
begin
  retval := FALSE;

  if not(OTFEFreeOTFE.CanUserManageDrivers()) then
    begin
    // On Vista, escalate UAC
    if SDUOSVistaOrLater() then
      begin
      UACEscalateForPortableMode(setTo, suppressMsgs);
      end
    else
      begin
      // On pre-Vista, just let user know they can't do it
      if not(suppressMsgs) then
        begin
        SDUMessageDlg(
                      TEXT_NEED_ADMIN,
                      mtWarning,
                      [mbOK],
                      0
                     );
        end;
      end;

    end
  else
    begin
    case setTo of
      pmaStart:
        begin
        retval := _PortableModeStart(suppressMsgs);
        end;

      pmaStop:
        begin
        retval := _PortableModeStop(suppressMsgs);
        end;

      pmaToggle:
        begin
        retval := _PortableModeToggle(suppressMsgs);
        end;

    end;

    // In case we UAC escalated (i.e. ran a separate process to handle the
    // drivers), we send out a message to any other running instances of
    // FreeOTFE to refresh
{$IFDEF VER185}
      //SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
      PostMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ELSE}
      SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_REFRESH, 0, 0);
{$ENDIF}
    end;

  Result := retval;
end;

procedure TfrmFreeOTFEMain.GetAllDriversUnderCWD(driverFilenames: TStringList);
const
  // Subdirs which should be searched for drivers
  DRIVERS_SUBDIR_32_BIT = '\x86';
  DRIVERS_SUBDIR_64_BIT = '\amd64';
var
  fileIterator: TSDUFileIterator;
  filename: string;
  driversDir: string;
begin
  // Compile a list of drivers in the CWD
  fileIterator:= TSDUFileIterator.Create(nil);
  try
    driversDir := ExtractFilePath(Application.ExeName);
    if SDUOS64bit() then
      begin
      driversDir := driversDir + DRIVERS_SUBDIR_64_BIT;
      end
    else
      begin
      driversDir := driversDir + DRIVERS_SUBDIR_32_BIT;
      end;

    fileIterator.Directory := driversDir;
    fileIterator.FileMask := '*.sys';
    fileIterator.RecurseSubDirs := FALSE;
    fileIterator.OmitStartDirPrefix := FALSE;
    fileIterator.IncludeDirNames := FALSE;

    fileIterator.Reset();
    filename := fileIterator.Next();
    while (filename<>'') do
      begin
      driverFilenames.Add(filename);
      filename := fileIterator.Next();
      end;

  finally
    fileIterator.Free();
  end;
end;

function TfrmFreeOTFEMain._PortableModeStart(suppressMsgs: boolean): boolean;
var
  driverFilenames: TStringList;
  allOK: boolean;
begin
  allOK:= FALSE;

  driverFilenames:= TStringList.Create();
  try
    GetAllDriversUnderCWD(driverFilenames);

    if (driverFilenames.count<1) then
      begin
      if not(suppressMsgs) then
        begin
        SDUMessageDlg(
                   _('Unable to locate any portable DoxBox drivers.')+SDUCRLF+
                   SDUCRLF+
                   _('Please ensure that the a copy of the DoxBox drivers (".sys" files) you wish to use are located in the correct directory.'),
                   mtWarning
                  );
        end;
      end
    else
      begin
      DeactivateFreeOTFEComponent();
      if OTFEFreeOTFE.PortableStart(driverFilenames, not(suppressMsgs)) then
        begin
// Message commented out - it's pointless as user will know anyway because
// either:
//   a) They started it, or
//   b) It's started automatically on FreeOTFE startup - in which case, it's
//      just annoying
//        if not(suppressMsgs) then
//          begin
//          SDUMessageDlg('Portable mode drivers installed and started.', mtInformation, [mbOK], 0);
//          end;
        allOK:= TRUE;
        end
      else
        begin
        if not(suppressMsgs) then
          begin
          SDUMessageDlg(
                     _('One or more of your portable DoxBox drivers could not be installed/started.')+SDUCRLF+
                     SDUCRLF+
                     TEXT_NEED_ADMIN+SDUCRLF+
                     SDUCRLF+
                     _('Please select "File | Drivers..." to check which drivers are currently operating.'),
                     mtWarning
                    );
          end;
        end;

      end;

  finally
    driverFilenames.Free();
    ActivateFreeOTFEComponent(suppressMsgs);
  end;

  Result := allOK;
end;

function TfrmFreeOTFEMain._PortableModeStop(suppressMsgs: boolean): boolean;
var
  allOK: boolean;
  stopOK: boolean;
begin
  allOK := FALSE;

  // Note that if the component was *not* active, then we can shutdown all
  // portable mode drivers; if we aren't active, this implies the main driver
  // isn't installed/running - in which case we don't need the user to confirm
  // as no drives can be mounted
  stopOK := TRUE;
  if OTFEFreeOTFE.Active then
    begin
    if (OTFEFreeOTFE.CountDrivesMounted() > 0) then
      begin
      if suppressMsgs then
        begin
        stopOK := FALSE;
        end
      else
        begin
        stopOK := (SDUMessageDlg(
                               _('You have one or more volumes mounted.')+SDUCRLF+
                               SDUCRLF+
                               _('If any of the currently mounted volumes makes use of any of the DoxBox drivers which are currently in portable mode, stopping portable mode is not advisable.')+SDUCRLF+
                               SDUCRLF+
                               _('It is recommended that you dismount all volumes before stopping portable mode.')+SDUCRLF+
                               SDUCRLF+
                               _('Do you wish to continue stopping portable mode?'),
                               mtWarning,
                               [mbYes, mbNo],
                               0
                              ) = mrYes);
        end;
      end;
    end;
    

  if stopOK then
    begin
    DeactivateFreeOTFEComponent();
    if OTFEFreeOTFE.PortableStop() then
      begin
      // No point in informing user; they would have been prompted if they
      // wanted to do this, and they can tell they've stopped as the
      // application will just exit; only popping up a messagebox if there's a
      // problem
      if not(ShuttingDownFlag) then
        begin
        if not(suppressMsgs) then
          begin
          SDUMessageDlg(_('Portable mode drivers stopped and uninstalled.'), mtInformation, [mbOK], 0);
          end;
        end;

      allOK := TRUE;
      end
    else
      begin
      if not(suppressMsgs) then
        begin
        SDUMessageDlg(
                   _('One or more of your portable DoxBox drivers could not be stopped/uninstalled.')+SDUCRLF+
                   SDUCRLF+
                   TEXT_NEED_ADMIN+SDUCRLF+
                   SDUCRLF+
                   _('Please select "File | Drivers..." to check which drivers are currently operating.'),
                   mtWarning
                  );
        end;
      end;

    ActivateFreeOTFEComponent(suppressMsgs);
    end;

  Result := allOK;
end;

function TfrmFreeOTFEMain._PortableModeToggle(suppressMsgs: boolean): boolean;
var
  retval: boolean;
  cntPortable: integer;
begin
  retval := FALSE;

  cntPortable := OTFEFreeOTFE.DriversInPortableMode();

  if (cntPortable < 0) then
    begin
    // We *should* be authorised to do this by the time we get here...
    // Do nothing; retval already set to FALSE
    end
  else if (cntPortable > 0) then
    begin
    retval := PortableModeSet(pmaStop, suppressMsgs);
    end
  else
    begin
    retval := PortableModeSet(pmaStart, suppressMsgs);
    end;

  Result := retval;
end;


procedure TfrmFreeOTFEMain.actDismountExecute(Sender: TObject);
begin
  DismountSelected();

end;

procedure TfrmFreeOTFEMain.actAboutExecute(Sender: TObject);
var
  dlg: TfrmAbout;
begin
  dlg:= TfrmAbout.Create(self);
  try
    dlg.FreeOTFEObj := OTFEFreeOTFE;
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
  EndSessionFlag := TRUE;
  Close();
end;


// Function required for both close and minimize to system tray icon
procedure TfrmFreeOTFEMain.actConsoleDisplayExecute(Sender: TObject);
begin
  SDUSystemTrayIcon1.DoRestore();

end;


procedure TfrmFreeOTFEMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  userConfirm: WORD;
  oldActive: boolean;
  closingDrivesMounted: integer;
begin
  CanClose := TRUE;

  closingDrivesMounted := 0;

  // This code segment is required to handle the case when Close() is called
  // programatically
  // Only carry out this action if closing to SystemTrayIcon
  // Note the MainForm test; if it was not the main form, setting Action would
  // in FormClose would do the minimise
  if (
      not(EndSessionFlag) and
      Settings.OptSystemTrayIconDisplay and
      Settings.OptSystemTrayIconCloseTo and
{$IFDEF VER185}
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
//        (application.mainform = self)
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       )
      )
{$ELSE}
      (Application.MainForm = self)
{$ENDIF}
     ) then
    begin
    CanClose := FALSE;
    actConsoleHide.Execute();
    end;

  if (CanClose) then
    begin
    oldActive := OTFEFreeOTFE.Active;
    // We (quite reasonably) assume that if the FreeOTFE component is *not*
    // active, and we can't activate it, then no FreeOTFE volumes are mounted
    if not(OTFEFreeOTFE.Active) then
      begin
      // Prevent any warnings...
      ShuttingDownFlag := TRUE;
      ActivateFreeOTFEComponent(FALSE);
      // Reset flag
      ShuttingDownFlag := FALSE;
      end;

    if (OTFEFreeOTFE.Active) then
      begin
      if (OTFEFreeOTFE.CountDrivesMounted() > 0) then
        begin
        userConfirm := mrNo;
        if (Settings.OptOnExitWhenMounted = oewmPromptUser) then
          begin
          userConfirm := SDUMessageDlg(
                                    _('One or more volumes are still mounted.')+SDUCRLF+
                                    SDUCRLF+
                                    _('Do you wish to dismount all volumes before exiting?'),
                                    mtConfirmation,
                                    [mbYes, mbNo, mbCancel],
                                    0
                                   );
          end
        else if (Settings.OptOnExitWhenMounted = oewmDismount) then
          begin
          userConfirm := mrYes;
          end
        else // oewmLeaveMounted
          begin
          // Do nothing - userConfirm already set to mrNo
          end;

        if (userConfirm = mrCancel) then
          begin
          CanClose := FALSE;
          end
        else if (userConfirm = mrYes) then
          begin
          CanClose := DismountAll();
          end
        else
          begin
          // CanClose already set to TRUE; do nothing
          end;

        end;

      closingDrivesMounted := OTFEFreeOTFE.CountDrivesMounted();
      end;  // if (OTFEFreeOTFE.Active) then

    if not(oldActive) then
      begin
      // Prevent any warnings...
      ShuttingDownFlag := TRUE;
      DeactivateFreeOTFEComponent();
      // Reset flag
      ShuttingDownFlag := FALSE;
      end;

    end;


  if (CanClose) then
    begin
    // If there's no drives mounted, and running in portable mode, prompt for
    // portable mode shutdown
    if (
        (closingDrivesMounted <= 0) and
        (OTFEFreeOTFE.DriversInPortableMode() > 0)
       ) then
      begin
      userConfirm := mrNo;
      if (Settings.OptOnExitWhenPortableMode = oewpPromptUser) then
        begin
        userConfirm := SDUMessageDlg(
                                   _('One or more of the DoxBox drivers are running in portable mode.')+SDUCRLF+
                                   SDUCRLF+
                                   _('Do you wish to shutdown portable mode before exiting?'),
                                   mtConfirmation,
                                   [mbYes, mbNo, mbCancel],
                                   0
                                   );
        end
      else if (Settings.OptOnExitWhenPortableMode = owepPortableOff) then
        begin
        userConfirm := mrYes;
        end
      else // oewpDoNothing
        begin
        // Do nothing - userConfirm already set to mrNo
        end;

      if (userConfirm = mrCancel) then
        begin
        CanClose := FALSE;
        end
      else if (userConfirm = mrYes) then
        begin
        ShuttingDownFlag := TRUE;
        CanClose := PortableModeSet(pmaStop, FALSE);
        ShuttingDownFlag := CanClose;
        end
      else
        begin
        CanClose := TRUE;
        end;

      end;

    end;  // if (CanClose) then

  if CanClose then
    begin
    ShutdownPKCS11();
    SDUSystemTrayIcon1.Active := FALSE;
    DestroySysTrayIconMenuitems();
    Application.ProcessMessages();
    Application.UnhookMainWindow(MessageHook);
    OTFEFreeOTFE.Active := FALSE;
    end
  else
    begin
    // Reset flag in case user clicked "Cancel" on tany of the above prompts...
    EndSessionFlag := FALSE;
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
  if (
      not(EndSessionFlag) and
      Settings.OptSystemTrayIconDisplay and
      Settings.OptSystemTrayIconCloseTo and
{$IFDEF VER185}
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
//        (application.mainform <> self)
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       )
      )
{$ELSE}
      (Application.MainForm <> self)
{$ENDIF}
     ) then
    begin
    Action := caMinimize;
    end;

end;


// This is used to ensure that only one copy of FreeOTFE is running at any
// given time
function TfrmFreeOTFEMain.MessageHook(var msg: TMessage): boolean;
var
  retVal: boolean;
  hotKeyMsg: TWMHotKey;
begin
  retVal := FALSE;

  if (msg.Msg = GLOBAL_VAR_WM_FREEOTFE_RESTORE) then
    begin
    // Restore application
    actConsoleDisplay.Execute();

    msg.Result := 0;
    retVal := TRUE;
    end
  else if (msg.Msg = GLOBAL_VAR_WM_FREEOTFE_REFRESH) then
    begin
    actRefreshExecute(nil);
    msg.Result := 0;
    retVal := TRUE;
    end
  else if (msg.Msg = WM_HOTKEY) then
    begin
    // Hotkey window message handler
    hotKeyMsg := TWMHotKey(msg);

    if OTFEFreeOTFE.Active then
      begin
      if (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNT) then
        begin
        // We'll allow errors in dismount to be reported to the user; it's just
        // a normal dismount
        DismountAll(FALSE);
        end
      else if (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNTEMERG) then
        begin
        // Panic! The user pressed the emergency dismount hotkey! Better not
        // report any errors to the user
        DismountAll(TRUE);
        end;
      end;

      if (
          (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNT) or
          (hotKeyMsg.HotKey = HOTKEY_IDENT_DISMOUNTEMERG)
         ) then
        begin
        msg.Result := 0;
        retVal := TRUE;
        end;

    end;

  Result := retVal;
end;

procedure TfrmFreeOTFEMain.miCreateKeyfileClick(Sender: TObject);
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
  if FInitAppCalled then
    begin
    // Cleardown any existing hotkeys...
    DisableHotkey(HOTKEY_IDENT_DISMOUNT);
    DisableHotkey(HOTKEY_IDENT_DISMOUNTEMERG);

    // ...And setup hotkeys again
    if Settings.OptHKeyEnableDismount then
      begin
      EnableHotkey(
                   Settings.OptHKeyKeyDismount,
                   HOTKEY_IDENT_DISMOUNT
                  );
      end;

    if Settings.OptHKeyEnableDismountEmerg then
      begin
      EnableHotkey(
                   Settings.OptHKeyKeyDismountEmerg,
                   HOTKEY_IDENT_DISMOUNTEMERG
                  );
      end;
    end;

end;

// Enable the specified shortcut as a hotkey, which will callback using the
// given identifier
procedure TfrmFreeOTFEMain.EnableHotkey(hotKey: TShortCut; hotKeyIdent: integer);
var
  modifiers : integer;
  vk  : word;
  shiftState : TShiftState;
  toEarlyToDetectOtherInstance: boolean;
  msg: string;
  msgType: TMsgDlgType;
begin
  if (hotkey = TextToShortCut(HOTKEY_TEXT_NONE)) then
    begin
    DisableHotkey(hotkeyIdent);
    end
  else
    begin
    // Obtain the virtual keycode and shiftState
    ShortCutToKey(hotkey, vk, shiftState);

    // Convet TShiftState into a modifier value for RegisterHotKey
    modifiers := 0;
    if (ssCtrl in shiftState) then
      begin
      modifiers := modifiers OR MOD_CONTROL;
      end;
    if (ssAlt in shiftState) then
      begin
      modifiers := modifiers OR MOD_ALT;
      end;
    if (ssShift in shiftState) then
      begin
      modifiers := modifiers OR MOD_SHIFT;
      end;

    DisableHotkey(hotkeyIdent);
    if not(RegisterHotkey(Application.Handle, hotkeyIdent, modifiers, vk)) then
      begin
      // If FreeOTFE's already running, assume that the other instance has
      // already registered (taken) the hotkey

{$IFDEF VER185}
      // See SDUGeneral._SDUDetectExistWindowDetails_ThisClassHandle(...)
      //  - if this is set, then SDUDetectExistingApp(...) don't be able to
      // detect any other running instance yet

      // Vista fix for Delphi 2007 and later
      toEarlyToDetectOtherInstance := FALSE;
      if (
          (Application.Mainform = nil) and // Yes, "=" is correct here
          Application.MainFormOnTaskbar
         ) then
        begin
        toEarlyToDetectOtherInstance := TRUE;
        end;
{$ELSE}
      toEarlyToDetectOtherInstance := FALSE;
{$ENDIF}

      if not(toEarlyToDetectOtherInstance) then
        begin
        msg := SDUParamSubstitute(_('Error: Unable to assign hotkey %1'), [ShortCutToText(hotkey)]);
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
        if (SDUDetectExistingApp() > 0) then
          begin
          msg := msg + SDUCRLF +
                 SDUCRLF+
                 _('Another instance of DoxBox is running - the hotkey may already be assigned to that instance.');
          msgType := mtWarning;
          end;

        SDUMessageDlg(msg, msgType);
        end;
      end;

    end;

end;

// Disable the hotkey with the specifid identifier
procedure TfrmFreeOTFEMain.DisableHotkey(hotKeyIdent: integer);
begin
  UnRegisterHotkey(Application.Handle, hotkeyIdent);
end;


function TfrmFreeOTFEMain.SetTestMode(silent,setOn:Boolean):Boolean;
var
    Error  :cardinal;
begin
  inherited;
    {   Error :=  WinExec(PAnsiChar('C:\Windows\System32\bcdedit.exe /set TESTSIGNING ON'), SW_SHOWNORMAL);
      if Error<31 then
      begin
      SDUMessageDlg(_('Set Test Mode failed:')+Inttostr(Error), mtError, [mbOK], 0);
      end;  }

{
  run bcdedit.exe /set TESTSIGNING ON

  see https://stackoverflow.com/questions/16827229/file-not-found-error-launching-system32-winsat-exe-using-process-start for 'sysnative'
  this will change if built as 64 bit exe  "Alternatively, if you build your program as x64, you can leave the path as c:\windows\system32"
}
// could also use SDUWow64DisableWow64FsRedirection
  result := SDUWinExecAndWait32(SDUGetWindowsDirectory()+'\sysnative\bcdedit.exe /set TESTSIGNING '+IfThen(setOn,'ON','OFF'), SW_SHOWNORMAL) <>  $FFFFFFFF;
  if not silent then
    begin
    if not result then
      begin
      SDUMessageDlg(_('Set Test Mode failed: ')+SysErrorMessage(GetLastError), mtError, [mbOK], 0);
      end
    else
      begin
      if setOn then    SDUMessageDlg(_('Please reboot. After rebooting the DoxBox drivers may be loaded'), mtInformation, [mbOK], 0)
      else             SDUMessageDlg(_('After rebooting Test Mode will be off'), mtInformation, [mbOK], 0) ;
      end
    end;
end;

function TfrmFreeOTFEMain.SetInstalled():Boolean;
begin
  inherited;    
  //needs to be place to save to
  if Settings.OptSaveSettings = slNone then
     Settings.OptSaveSettings := slProfile;//exe not supported in win >NT
  if result then
    begin
      Settings.OptInstalled := true;
      result := Settings.Save();//todo:needed?
    end;
end;

procedure TfrmFreeOTFEMain.actTestModeOffExecute(Sender: TObject);
var
  SigningOS :boolean;
begin
  inherited;
  SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
  if SigningOS then SetTestMode(false,false)
  else  SDUMessageDlg(_('This version of Windows does not support test mode'), mtError, [mbOK], 0);
end;

procedure TfrmFreeOTFEMain.actTestModeOnExecute(Sender: TObject);
var
  SigningOS :boolean;
begin
  inherited;
  SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
  if SigningOS then SetTestMode(false,true)
  else  SDUMessageDlg(_('There is no need to set test mode on this version of Windows'), mtError, [mbOK], 0);
end;

procedure TfrmFreeOTFEMain.actTogglePortableModeExecute(Sender: TObject);
begin
  if (
      actTogglePortableMode.Enabled and
      (CountPortableDrivers < 0)
     ) then
    begin
    PortableModeSet(pmaToggle, FALSE);
    end
  else if actTogglePortableMode.checked then
    begin
    PortableModeSet(pmaStop, FALSE);
    end
  else
    begin
    PortableModeSet(pmaStart, FALSE);
    end;

  CountPortableDrivers := OTFEFreeOTFE.DriversInPortableMode();
  EnableDisableControls();

end;


procedure TfrmFreeOTFEMain.lvDrivesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  EnableDisableControls();

end;

procedure TfrmFreeOTFEMain.actFormatExecute(Sender: TObject);
resourcestring
  FORMAT_CAPTION = 'Format';
var
  i: integer;
  currDrive: char;
  driveNum: word;
  selDrives: string;
begin
  selDrives := GetSelectedDrives();
  for i:=1 to length(selDrives) do
    begin
    currDrive := selDrives[i];
    driveNum := ord(currDrive) - ord('A');
    SHFormatDrive(Handle, driveNum, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
    end;

  // This is *BIZARRE*.
  // If you are running under Windows Vista, and you mount a volume for
  // yourself only, and then try to format it, the format will fail - even if
  // you UAC escalate to admin
  // That's one thing - HOWEVER! After it fails, for form's title is set to:
  //   Format <current volume label> <drive letter>:
  // FREAKY!
  // OTOH, it does means we can detect and inform the user that the format
  // just failed...
  if (Pos(uppercase(FORMAT_CAPTION), uppercase(self.Caption)) > 0) then
    begin
    self.Caption := Application.Title;
    SDUMessageDlg(
                  _('Your encrypted drive could not be formatted at this time.')+SDUCRLF+
                  SDUCRLF+
                  _('Please lock this Box and re-open it with the "Mount for all users" option checked, before trying again.'),
                  mtError
                 );
    end;

  RefreshDrives();

end;

procedure TfrmFreeOTFEMain.actOptionsExecute(Sender: TObject);
var
  dlg: TfrmOptions_FreeOTFE;
begin
  inherited;

  dlg := TfrmOptions_FreeOTFE.Create(self);
  try
    dlg.OTFEFreeOTFEBase := OTFEFreeOTFE;
    if (dlg.ShowModal() = mrOK) then
      begin
      ReloadSettings();
      EnableDisableControls();
      end;

  finally
    dlg.Free();
  end;

end;

procedure TfrmFreeOTFEMain.actOverwriteEntireDriveExecute(Sender: TObject);
var
  selectedDrive: ansistring;
  selDrives: ansistring;
begin
  selDrives := GetSelectedDrives();
  if (length(selDrives) > 1) then
    begin
    SDUMessageDlg(
                  _('Due to the destructive nature of overwriting entire drives, only one drive may be selected when this option is chosen.')+SDUCRLF+
                  SDUCRLF+
                  _('Please ensure that only one mounted drive is selected, and retry.')
                 );
    end
  else
    begin
    selectedDrive := selDrives[1];

    if SDUWarnYN(
                    _('WARNING!')+SDUCRLF+
                    SDUCRLF+
                    SDUParamSubstitute(_('You are attempting to overwrite drive: %1:'), [selectedDrive])+SDUCRLF+
                    SDUCRLF+
                    SDUParamSubstitute(_('THIS WILL DESTROY ALL INFORMATION STORED ON DRIVE %1:, and require it to be reformatted'), [selectedDrive])+SDUCRLF+
                    SDUCRLF+
                    _('Are you ABSOLUTELY SURE you want to do this?')
                   ) then
      begin
      OverwriteDrives(selectedDrive, TRUE);
      end;
    end;
end;


procedure TfrmFreeOTFEMain.actOverwriteFreeSpaceExecute(Sender: TObject);
var
  selDrives: ansistring;
begin
  selDrives := GetSelectedDrives();
  OverwriteDrives(selDrives, FALSE);
end;

// overwriteEntireDrive - Set to TRUE to overwrite the entire drive; this will
//                        destroy all data on the drive, and requiring it to be
//                        reformatted. Set to FALSE to simply overwrite the
//                        free space on the drive
procedure TfrmFreeOTFEMain.OverwriteDrives(drives: ansistring; overwriteEntireDrive: boolean);
var
  shredder: TShredder;
  i: integer;
  currDrive: ansichar;
  frmOverWriteMethod: TfrmFreeOTFESelectOverwriteMethod;
  overwriteWithEncryptedData: boolean;
  allOK: boolean;
  getRandomBits: integer;
  randomBuffer: array of byte;
  overwriteOK: TShredResult;
  currDriveDevice: string;
  failMsg: string;
begin
  overwriteOK := srSuccess;
  allOK := FALSE;
  overwriteWithEncryptedData := FALSE;

  frmOverWriteMethod:= TfrmFreeOTFESelectOverwriteMethod.Create(self);
  try
    frmOverWriteMethod.OTFEFreeOTFEObj := OTFEFreeOTFE;
    if (frmOverWriteMethod.ShowModal() = mrOK) then
      begin
      allOK := TRUE;

      overwriteWithEncryptedData := frmOverWriteMethod.OverwriteWithEncryptedData;
      if (overwriteWithEncryptedData) then
        begin
        TempCypherDriver:= frmOverWriteMethod.CypherDriver;
        TempCypherGUID:= frmOverWriteMethod.CypherGUID;

        OTFEFreeOTFE.GetSpecificCypherDetails(TempCypherDriver, TempCypherGUID, TempCypherDetails);

        // Initilize zeroed IV for encryption
        TempCypherEncBlockNo := 0;

        // Get *real* random data for encryption key
        getRandomBits := 0;
        if (TempCypherDetails.KeySizeRequired < 0) then
          begin
          // -ve keysize = arbitary keysize supported - just use 512 bits
          getRandomBits := 512;
          end
        else if (TempCypherDetails.KeySizeRequired > 0) then
          begin
          // +ve keysize = specific keysize - get sufficient bits
          getRandomBits := TempCypherDetails.KeySizeRequired;
          end;

        if (getRandomBits>0) then
          begin
          MouseRNGDialog1.RequiredBits := getRandomBits;
          allOK := MouseRNGDialog1.Execute();
          end;

        if (allOK) then
          begin
          SetLength(randomBuffer, (getRandomBits div 8));
          MouseRNGDialog1.RandomData(getRandomBits, randomBuffer);

          for i:=low(randomBuffer) to high(randomBuffer) do
            begin
            TempCypherKey := TempCypherKey + Ansichar(randomBuffer[i]);
            // Overwrite the temp buffer...
            randomBuffer[i] := random(256);
            end;
          SetLength(randomBuffer, 0);
          end;

        end;

      end;

  finally
    frmOverWriteMethod.Free();
  end;



  if (allOK) then
    begin
    if SDUConfirmYN(
                   _('Overwriting on a large volume may take awhile.')+SDUCRLF+
                   SDUCRLF+
                   _('Are you sure you wish to proceed?')
                  ) then
      begin
      shredder:= TShredder.Create(nil);
      try
        shredder.IntMethod := smPseudorandom;
        shredder.IntPasses := 1;

        if overwriteWithEncryptedData then
          begin
          // Note: Setting this event overrides shredder.IntMethod
          shredder.OnOverwriteDataReq := GenerateOverwriteData;
          end
        else
          begin
          shredder.OnOverwriteDataReq := nil;
          end;

        for i:=1 to length(drives) do
          begin
          currDrive := drives[i];

          if overwriteEntireDrive then
            begin
            currDriveDevice := '\\.\'+currDrive+':';
            shredder.DestroyDevice(currDriveDevice, FALSE, FALSE);
            overwriteOK := shredder.LastIntShredResult;
            end
          else
            begin
            overwriteOK := shredder.OverwriteDriveFreeSpace(currDrive, FALSE);
            end;

          end;

        if (overwriteOK = srSuccess) then
          begin
          SDUMessageDlg(_('Overwrite operation complete.'), mtInformation);
          if overwriteEntireDrive then
            begin
            SDUMessageDlg(_('Please reformat the drive just overwritten before attempting to use it.'), mtInformation);
            end;
          end
        else if (overwriteOK = srError) then
          begin
          failMsg := _('Overwrite operation FAILED.');
          if not(overwriteEntireDrive) then
            begin
            failMsg := failMsg + SDUCRLF+
                       SDUCRLF+
                       _('Did you remember to format this drive first?');
            end;

          SDUMessageDlg(failMsg, mtError);
          end
        else if (overwriteOK = srUserCancel) then
          begin
          SDUMessageDlg(_('Overwrite operation cancelled by user.'), mtInformation);
          end
        else
          begin
          SDUMessageDlg(_('No drives selected to overwrite?'), mtInformation);
          end;


      finally
        shredder.Free();
      end;

    end;  // if (SDUMessageDlg(
  end;  // if (allOK) then

  TempCypherDriver:= '';
  TempCypherGUID:= StringToGUID('{00000000-0000-0000-0000-000000000000}');
  TempCypherKey := '';
  TempCypherEncBlockNo := 0;

end;

procedure TfrmFreeOTFEMain.tbbTogglePortableModeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // This is ugly, but required since if the button is clicked on, and the
  // operation cancelled, the button pops up - even though it shouldn't!
  tbbTogglePortableMode.down  := (CountPortableDrivers > 0);

end;


// Handle "/portable" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Portable(): integer;
var
  cmdExitCode: integer;
  paramValue: string;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;
  if SDUCommandLineParameter(CMDLINE_PORTABLE, paramValue) then
    begin
    if (uppercase(paramValue) = uppercase(CMDLINE_TOGGLE)) then
      begin
      if PortableModeSet(
                         pmaToggle,
                         SDUCommandLineSwitch(CMDLINE_SILENT)
                        ) then
        begin
        cmdExitCode := CMDLINE_SUCCESS;
        end
      else
        begin
        cmdExitCode := CMDLINE_EXIT_UNABLE_TO_START_PORTABLE_MODE;
        end;
      end
    else if (
             (uppercase(paramValue) = uppercase(CMDLINE_START)) or
             (uppercase(paramValue) = uppercase(CMDLINE_ON)) or
             (paramValue = '1')
            ) then
      begin
      if PortableModeSet(
                         pmaStart,
                         SDUCommandLineSwitch(CMDLINE_SILENT)
                        ) then
        begin
        cmdExitCode := CMDLINE_SUCCESS;
        end
      else
        begin
        cmdExitCode := CMDLINE_EXIT_UNABLE_TO_START_PORTABLE_MODE;
        end;
      end
    else if (
             (uppercase(paramValue) = uppercase(CMDLINE_STOP)) or
             (uppercase(paramValue) = uppercase(CMDLINE_OFF)) or
             (paramValue = '0')
            ) then
      begin
      if PortableModeSet(
                         pmaStop,
                         SDUCommandLineSwitch(CMDLINE_SILENT)
                        ) then
        begin
        cmdExitCode := CMDLINE_SUCCESS;
        end
      else
        begin
        cmdExitCode := CMDLINE_EXIT_UNABLE_TO_STOP_PORTABLE_MODE;
        end;
      end
    end;

  Result := cmdExitCode;
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
function TfrmFreeOTFEMain.InstallAllDrivers(driverControlObj: TOTFEFreeOTFEDriverControl;silent: boolean): integer;
var

  paramValue: string;
  driverPathAndFilename: string;
  driverFilenames: TStringList;
//  vista64Bit: boolean;
begin



             result := CMDLINE_EXIT_UNKNOWN_ERROR;

      driverFilenames:= TStringList.Create();
      try
        GetAllDriversUnderCWD(driverFilenames);

        if (driverFilenames.count <= 0) then
          begin
          result := CMDLINE_EXIT_FILE_NOT_FOUND;
          end
        else
          begin
          result := CMDLINE_SUCCESS;

          // If under Vista x64, don't autostart the drivers after
          // installing - this could cause a bunch of *bloody* *stupid*
          // warning messages about "unsigned drivers" to be displayed by
          // the OS
//          vista64Bit := (SDUOSVistaOrLater() and SDUOS64bit());
          // set test mode
         // if vista64Bit then
          //  if not SetTestMode(true,true) then cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;

          if driverControlObj.InstallMultipleDrivers(
                                                     driverFilenames,
                                                     FALSE,
                                                     not(silent),
                                                     true// not(vista64Bit)
                                                    ) then
            begin

            { If Vista x64, we tell the user to reboot. That way, the drivers
            // will be started up on boot - and the user won't actually see
            // any stupid warning messages about "unsigned drivers"
            apparently on vista is not necesary to set test mode - but need to start drivers on os start
            test mode is set anyway for all OS's so start drivers now whatever
            }
            if (
               // vista64Bit and
                not(silent)
               ) then
              begin
              SDUMessageDlg(
                            _('DoxBox drivers have been installed successfully.'),
                            mtInformation
                           );
              end;
            end else
            begin
              result := CMDLINE_EXIT_UNKNOWN_ERROR;
            end;
          end;

      finally
        driverFilenames.Free();
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
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Install(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
var
  cmdExitCode: integer;
  paramValue: string;
  driverPathAndFilename: string;
  driverFilenames: TStringList;
  silent: boolean;
//  vista64Bit: boolean;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_FILENAME, paramValue) then
    begin
    silent := SDUCommandLineSwitch(CMDLINE_SILENT);

    if (uppercase(trim(paramValue)) = uppercase(CMDLINE_ALL)) then
      begin
        cmdExitCode := InstallAllDrivers(driverControlObj,silent);
      end
    else
      begin
      // Convert any relative path to absolute
      driverPathAndFilename := SDURelativePathToAbsolute(paramValue);

      // Ensure driver file actually exists(!)
      if not(fileexists(driverPathAndFilename)) then
        begin
        cmdExitCode := CMDLINE_EXIT_FILE_NOT_FOUND;
        end
      else
        begin
        cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;
        if driverControlObj.InstallSetAutoStartAndStartDriver(paramValue) then
          begin
          cmdExitCode := CMDLINE_SUCCESS;
          end;
        end;
      end;
    end;

  Result := cmdExitCode;
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
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Uninstall(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
var
  cmdExitCode: integer;
  paramValue: string;
  driveUninstallResult: DWORD;
//    vista64Bit: boolean;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_DRIVERNAME, paramValue) then
    begin
    cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;
    if (uppercase(trim(paramValue)) = uppercase(CMDLINE_ALL)) then
      begin
      cmdExitCode := CMDLINE_SUCCESS;
//      vista64Bit := (SDUOSVistaOrLater() and SDUOS64bit());
      // set test mode off
      // if vista64Bit then if not SetTestMode(true,false) then cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;

      if not driverControlObj.UninstallAllDrivers(FALSE) then
        begin
          cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;
        end;
      end
    else
      begin
      driveUninstallResult := driverControlObj.UninstallDriver(paramValue);

      if ((driveUninstallResult AND DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS) then
        begin
        cmdExitCode := CMDLINE_SUCCESS;
        end;
      end;

    end;

  Result := cmdExitCode;
end;


// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_GUI(): integer;
var
  cmdExitCode: integer;
begin
  actDriversExecute(nil);
  cmdExitCode := CMDLINE_SUCCESS;

  Result := cmdExitCode;
end;

function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl(): integer;
var
  cmdExitCode: integer;
  paramValue: string;
  driverControlObj: TOTFEFreeOTFEDriverControl;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;
  try
    if SDUCommandLineParameter(CMDLINE_DRIVERCONTROL, paramValue) then
      begin
      // Special case; this one can UAC escalate
      if (uppercase(paramValue) = uppercase(CMDLINE_GUI)) then
        begin
        cmdExitCode := HandleCommandLineOpts_DriverControl_GUI();
        end
      else
        begin
        // Creating this object needs admin privs; if the user doesn't have
        // these, it'll raise an exception
        driverControlObj := TOTFEFreeOTFEDriverControl.Create();
        try
          driverControlObj.Silent := SDUCommandLineSwitch(CMDLINE_SILENT);

          // This first test is redundant
          if (uppercase(paramValue) = uppercase(CMDLINE_GUI)) then
            begin
            cmdExitCode := HandleCommandLineOpts_DriverControl_GUI();
            end
          else if (uppercase(paramValue) = uppercase(CMDLINE_COUNT)) then
            begin
            cmdExitCode := HandleCommandLineOpts_DriverControl_Count(driverControlObj);
            end
          else if (uppercase(paramValue) = uppercase(CMDLINE_INSTALL)) then
            begin
            cmdExitCode := HandleCommandLineOpts_DriverControl_Install(driverControlObj);
            end
          else if (uppercase(paramValue) = uppercase(CMDLINE_UNINSTALL)) then
            begin
            cmdExitCode := HandleCommandLineOpts_DriverControl_Uninstall(driverControlObj);
            end;

          finally
            driverControlObj.Free();
          end;
        end;

      end;

    except
      on E:EFreeOTFENeedAdminPrivs do
        begin
        cmdExitCode := CMDLINE_EXIT_ADMIN_PRIVS_NEEDED;
        end;
    end;
        
  Result := cmdExitCode;
end;


// Handle "/install" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_DriverControl_Count(driverControlObj: TOTFEFreeOTFEDriverControl): integer;
var
  cmdExitCode: integer;
  paramValue: string;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  // If "/type" not specified, default to the total
  if not(SDUCommandLineParameter(CMDLINE_TYPE, paramValue)) then
  begin
    paramValue := CMDLINE_TOTAL;
  end;

  if (uppercase(paramValue) = uppercase(CMDLINE_TOTAL)) then
    begin
    cmdExitCode := driverControlObj.CountDrivers();
    end
  else if (uppercase(paramValue) = uppercase(CMDLINE_DRIVERSPORTABLE)) then
    begin
    cmdExitCode := driverControlObj.CountDrivers(TRUE);
    end
  else if (uppercase(paramValue) = uppercase(CMDLINE_DRIVERSINSTALLED)) then
    begin
    cmdExitCode := driverControlObj.CountDrivers(FALSE);
    end;

  Result := cmdExitCode;
end;


// Handle "/count" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Count(): integer;
var
  cmdExitCode: integer;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineSwitch(CMDLINE_COUNT) then
    begin
    cmdExitCode := OTFEFreeOTFE.CountDrivesMounted();
    end;

  Result := cmdExitCode;
end;

// Handle "/settestmode" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_SetTestMode(): integer;
var
  paramValue: string;
  setOn: boolean;
  SigningOS,silent :boolean;
begin
  Result := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_SET_TESTMODE, paramValue) then
    begin
    Result := CMDLINE_SUCCESS;

    setOn := false;
    if (uppercase(paramValue) = uppercase(CMDLINE_ON)) then         setOn := true
    else if (uppercase(paramValue) <> uppercase(CMDLINE_OFF)) then  Result := CMDLINE_EXIT_INVALID_CMDLINE;

    if Result = CMDLINE_SUCCESS then
      begin
      SigningOS := (SDUOSVistaOrLater() and SDUOS64bit());
      silent  := SDUCommandLineSwitch(CMDLINE_SILENT);
      if SigningOS then
        begin
        if not SetTestMode(silent,setOn) then Result := CMDLINE_EXIT_UNABLE_TO_SET_TESTMODE;
        end
      else
        begin
        if not silent then SDUMessageDlg(_('This version of Windows does not support test mode'), mtError, [mbOK], 0);
        end;
      end;                
  end
end;


// Handle "/SetInstalled" command 
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_SetInstalled(): integer;
var
  paramValue: string;
  setOn: boolean;
begin
  Result := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineSwitch(CMDLINE_SET_INSTALLED) then
    begin
    Result := CMDLINE_SUCCESS;  
    
    if not SetInstalled() then Result := CMDLINE_EXIT_UNABLE_TO_SET_INSTALLED;
    end;
end;

// Handle "/create" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Create(): integer;
var
  cmdExitCode: integer;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineSwitch(CMDLINE_CREATE) then
    begin
    if OTFEFreeOTFE.CreateFreeOTFEVolumeWizard() then
      begin
      cmdExitCode := CMDLINE_SUCCESS;
      end
    else
      begin
      cmdExitCode := CMDLINE_EXIT_UNKNOWN_ERROR;
      end;

    end;

  Result := cmdExitCode;
end;


// Handle "/unmount" command line
// Returns: Exit code
function TfrmFreeOTFEMain.HandleCommandLineOpts_Dismount(): integer;
var
  cmdExitCode: integer;
  paramValue: string;
  force: boolean;
  drive: char;
  dismountOK: boolean;
begin
  cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE;

  if SDUCommandLineParameter(CMDLINE_DISMOUNT, paramValue) then
    begin
    force := SDUCommandLineSwitch(CMDLINE_FORCE);
    
    if (uppercase(paramValue) = uppercase(CMDLINE_ALL)) then
      begin
      dismountOK := (OTFEFreeOTFE.DismountAll(force) = '');
      end
    else
      begin
      // Only use the 1st char...
      drive := paramValue[1];

      dismountOK := OTFEFreeOTFE.Dismount(drive, force);
      end;

    if dismountOK then
      begin
      cmdExitCode := CMDLINE_SUCCESS;
      end
    else
      begin
      cmdExitCode := CMDLINE_EXIT_UNABLE_TO_DISMOUNT;
      end;
    end;

  Result := cmdExitCode;
end;

// Handle any command line options; returns TRUE if command line options
// were passed through, and "/noexit" wasn't specified as a command line
// parameter
function TfrmFreeOTFEMain.HandleCommandLineOpts(out cmdExitCode: integer): boolean;
var
  paramValue: string;
  retval: boolean;
  ignoreParams: integer;
  settingsFile: string;
begin
  cmdExitCode := CMDLINE_SUCCESS;

  AllowUACEsclation := not(SDUCommandLineSwitch(CMDLINE_NOUACESCALATE));
// todo: a lot of repirition here bc HandleCommandLineOpts_ fns also call  SDUCommandLineParameter
//todo: 'else' statements mean one cmd per call - allow compatible ones at same time

//these cmds at least  are independent of others
  if SDUCommandLineParameter(CMDLINE_SET_TESTMODE, paramValue) then
    begin
    cmdExitCode := HandleCommandLineOpts_SetTestMode();
    end;
  if cmdExitCode = CMDLINE_SUCCESS then
    begin
    if SDUCommandLineSwitch(CMDLINE_SET_INSTALLED) then
      begin
        cmdExitCode := HandleCommandLineOpts_SetInstalled();
      end
    end ;
    
    
if cmdExitCode = CMDLINE_SUCCESS then 
  begin
      
     cmdExitCode := CMDLINE_EXIT_INVALID_CMDLINE; 
    // Driver control dialog
    if SDUCommandLineSwitch(CMDLINE_DRIVERCONTROL) then
      begin
      cmdExitCode := HandleCommandLineOpts_DriverControl();
      end
    // Portable mode on/off...
    else if SDUCommandLineParameter(CMDLINE_PORTABLE, paramValue) then
      begin
      cmdExitCode := HandleCommandLineOpts_Portable();
      end
    else if SDUCommandLineParameter(CMDLINE_DRIVERCONTROL, paramValue) then
      begin
      cmdExitCode := HandleCommandLineOpts_DriverControl();
      end  
    else
      begin
      // All command line options below require FreeOTFE to be active before they
      // can be used
      if not(ActivateFreeOTFEComponent(TRUE)) then
        begin
        cmdExitCode := CMDLINE_EXIT_UNABLE_TO_CONNECT;
        end
      else
        begin
        if SDUCommandLineSwitch(CMDLINE_COUNT) then
          begin
          cmdExitCode := HandleCommandLineOpts_Count();
          end
        else if SDUCommandLineSwitch(CMDLINE_CREATE) then
          begin
          cmdExitCode := HandleCommandLineOpts_Create();
          end
        else if SDUCommandLineSwitch(CMDLINE_MOUNT) then
          begin
          cmdExitCode := HandleCommandLineOpts_Mount();
          end
        else if SDUCommandLineParameter(CMDLINE_DISMOUNT, paramValue) then
          begin
          cmdExitCode := HandleCommandLineOpts_Dismount();
          end;
  
        DeactivateFreeOTFEComponent();
        end;
      end;
    end;
  // Notice: CMDLINE_MINIMIZE handled in the .dpr

  // Return TRUE if there were no parameters specified on the command line
  // and "/noexit" wasn't specified
  // Note: Disregard any CMDLINE_SETTINGSFILE and parameter
  // Note: Also disregard any CMDLINE_MINIMIZE and parameter
  retval := FALSE;
  if not(SDUCommandLineSwitch(CMDLINE_NOEXIT)) then
    begin
    ignoreParams := 0;

    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFile) then
      begin
      inc(ignoreParams);
      if (settingsFile <> '') then
        begin
        inc(ignoreParams);
        end;
      end;

    if SDUCommandLineSwitch(CMDLINE_MINIMIZE) then
      begin
      inc(ignoreParams);
      end;

      retval := (ParamCount > ignoreParams);
    end;

  Result := retval;
end;


procedure TfrmFreeOTFEMain.UACEscalateForDriverInstallation();
begin
  UACEscalate(CMDLINE_SWITCH_IND + CMDLINE_DRIVERCONTROL + ' ' +CMDLINE_GUI, FALSE);
end;

procedure TfrmFreeOTFEMain.UACEscalateForPortableMode(portableAction: TPortableModeAction; suppressMsgs: boolean);
var
  cmdLinePortableSwitch: string;
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
              CMDLINE_SWITCH_IND + CMDLINE_PORTABLE+ ' ' +cmdLinePortableSwitch,
              suppressMsgs
             );

end;

procedure TfrmFreeOTFEMain.UACEscalate(cmdLineParams: string; suppressMsgs: boolean);
const
  VERB_RUNAS = 'runas';
var
  filenameAndPath: string;
  cwd: string;
begin
  // Only applicable in Windows Vista; simply warn user they can't continue on
  // earlier OSs
  if (
      not(SDUOSVistaOrLater()) or
      not(AllowUACEsclation)
     ) then
    begin
    if not(suppressMsgs) then
      begin
      SDUMessageDlg(TEXT_NEED_ADMIN, mtError, [mbOK], 0);
      end;
    end
  else
    begin
    // Prevent endless loops of UAC escalation...
    cmdLineParams := CMDLINE_SWITCH_IND + CMDLINE_NOUACESCALATE +
                     ' ' + cmdLineParams;

    // Message supression
    if suppressMsgs then
      begin
      cmdLineParams := CMDLINE_SWITCH_IND + CMDLINE_SILENT +
                       ' ' + cmdLineParams;
      end;

    filenameAndPath := ParamStr(0);
    cwd := ExtractFilePath(filenameAndPath);
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
    OTFEFreeOTFE.CachesFlush();
    end;

end;


procedure TfrmFreeOTFEMain.PKCS11TokenRemoved(SlotID: integer);
var
  i: integer;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  drivesToDismount: ansistring;
  mounted: ansistring;
begin
  SetStatusBarText(SDUParamSubstitute(_('Detected removal of token from slot ID: %1'), [SlotID]));
  if Settings.OptPKCS11AutoDismount then
    begin
    drivesToDismount := '';
    mounted := OTFEFreeOTFE.DrivesMounted();
    for i:=1 to length(mounted) do
      begin
      // ...Main FreeOTFE window list...
      if OTFEFreeOTFE.GetVolumeInfo(mounted[i], volumeInfo) then
        begin
        if volumeInfo.MetaDataStructValid then
          begin
          if (volumeInfo.MetaDataStruct.PKCS11SlotID = SlotID) then
            begin
            drivesToDismount := drivesToDismount + mounted[i];
            end;
          end;
        end;
      end;

    SetStatusBarText(SDUParamSubstitute(_('Autodismounting drives: %1'), [PrettyPrintDriveLetters(drivesToDismount)]));
    DismountDrives(drivesToDismount, FALSE);
    end;

end;


function TfrmFreeOTFEMain.GetSelectedDrives(): ansistring;
var
  i: integer;
  retval: ansistring;
begin
  retval := '';

  for i:=0 to (lvDrives.items.count-1) do
    begin
    if lvDrives.Items[i].Selected then
      begin
      retval := retval + GetDriveLetterFromLVItem(lvDrives.Items[i]);
      end;
    end;

  Result := retval;
end;

function TfrmFreeOTFEMain.OTFEFreeOTFE(): TOTFEFreeOTFE;
begin
  Result := TOTFEFreeOTFE(OTFEFreeOTFEBase);
end;

procedure TfrmFreeOTFEMain.actFreeOTFEMountPartitionExecute(
  Sender: TObject);
var
  selectedPartition: string;
begin
  if OTFEFreeOTFE.WarnIfNoHashOrCypherDrivers() then
    begin
    selectedPartition := OTFEFreeOTFE.SelectPartition();

    if (selectedPartition <> '') then
      begin
      MountFilesDetectLUKS(
                           selectedPartition,
                           FALSE,
                           ftFreeOTFE
                          );
      end;
    end;

end;

procedure TfrmFreeOTFEMain.actFreeOTFENewExecute(Sender: TObject);
var
  prevMounted: string;
  newMounted: string;
  createdMountedAs: string;
  msg: WideString;
  i: integer;
begin
  inherited;

  prevMounted := OTFEFreeOTFE.DrivesMounted;
  if not(OTFEFreeOTFE.CreateFreeOTFEVolumeWizard()) then
    begin
    if (OTFEFreeOTFE.LastErrorCode <> OTFE_ERR_USER_CANCEL) then
      begin
      SDUMessageDlg(_('DoxBox could not be created'), mtError);
      end;
    end
  else
    begin
    newMounted := OTFEFreeOTFE.DrivesMounted;

    // If the "drive letters" mounted have changed, the new volume was
    // automatically mounted; setup for this
    if (newMounted = prevMounted) then
      begin
      msg := _('DoxBox created successfully.')+SDUCRLF+
             SDUCRLF+
             _('Please mount, format and overwrite this volume''s free space before use.');
      end
    else
      begin
      // Get new drive on display...
      RefreshDrives();

      createdMountedAs := '';
      for i:=1 to length(newMounted) do
        begin
        if (Pos(newMounted[i], prevMounted) = 0) then
          begin
          if (createdMountedAs <> '') then
            begin
            // If the user mounted another volume during volume creation, we
            // can't tell from here which was the new volume
            createdMountedAs := createdMountedAs + ' / ';
            end;

          createdMountedAs := createdMountedAs + newMounted[i] + ':';
          end;
        end;

      msg := SDUParamSubstitute(
                                _('DoxBox created successfully and opened as: %1.'),
                                [createdMountedAs]
                               )+SDUCRLF+
             SDUCRLF+
             _('Please format and overwrite this drives''s free space before use.');
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

      InstallAllDrivers(driverControlObj,false);

          finally
            driverControlObj.Free();
          end;


end;

procedure TfrmFreeOTFEMain.actLinuxMountPartitionExecute(Sender: TObject);
var
  selectedPartition: string;
  mountList: TStringList;
begin
  if OTFEFreeOTFE.WarnIfNoHashOrCypherDrivers() then
    begin
    selectedPartition := OTFEFreeOTFE.SelectPartition();

    if (selectedPartition <> '') then
      begin
      mountList:= TStringList.Create();
      try
        mountList.Add(selectedPartition);
        MountFiles(ftLinux, mountList, FALSE,FALSE);
      finally
        mountList.Free();
      end;

      end;
    end;

end;

END.


