unit frmCommonMain;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  // delphi
  ActnList, Buttons, Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Grids, ImgList, Menus, Messages,
  SDUSystemTrayIcon, Shredder, Spin64, StdCtrls, SysUtils, ToolWin,
  Windows, XPMan,
  //sdu, lcutils
 pkcs11_library ,
  lcTypes, SDUMRUList, SDUMultimediaKeys, SDUDialogs, lcDialogs, CommonSettings,
  MouseRNGDialog_U, OTFE_U,
  DriverControl,
  OTFEFreeOTFE_U, OTFEFreeOTFEBase_U ,    sduGeneral,
  // librecrypt forms
  SDUForms, frmHdrBackupRestore;



// Command line return values. not all common, but put here so can use enum

type
  eCmdLine_Exit = (
    // common
    ceSUCCESS = 0,
    ceINVALID_CMDLINE = 100,
    ceUNABLE_TO_MOUNT = 102,
    ceFILE_NOT_FOUND = 106,

    // main specific
    ceUNABLE_TO_CONNECT = 101,
    ceUNABLE_TO_DISMOUNT = 103,
    ceUNABLE_TO_START_PORTABLE_MODE = 104,
    ceUNABLE_TO_STOP_PORTABLE_MODE = 105,
    ceADMIN_PRIVS_NEEDED = 106,
    ceUNABLE_TO_SET_TESTMODE = 107,
    ceUNABLE_TO_SET_INSTALLED = 108,
    ceUNKNOWN_ERROR = 999
    );

const

  WM_USER_POST_SHOW = WM_USER + 10;

  DEVICE_PREFIX = '\Device';


type
  TAutorunType = (arPostMount, arPreDismount, arPostDismount);

  TfrmCommonMain = class (TSDUForm)
    mmMain:       TMainMenu;
    File1:        TMenuItem;
    miFreeOTFEMountFile: TMenuItem;
    miDismountMain: TMenuItem;
    miExit:       TMenuItem;
    miFreeOTFENew: TMenuItem;
    miFreeOTFENewHidden : TMenuItem;
    miFreeOTFEMountHidden : TMenuItem;
    View1:        TMenuItem;
    miRefresh:    TMenuItem;
    OpenDialog:   TSDUOpenDialog;
    miHelp:       TMenuItem;
    About1:       TMenuItem;
    N4:           TMenuItem;
    miLinuxNew:   TMenuItem;
    miLinuxMountFile: TMenuItem;
    miTools:      TMenuItem;
    miCDBBackup:  TMenuItem;
    miCDBRestore: TMenuItem;
    miCreateKeyfile: TMenuItem;
    miChangePassword: TMenuItem;
    miCDBPlaintextDump: TMenuItem;
    miCDB:        TMenuItem;
    miLUKSDump:   TMenuItem;
    ActionList1:  TActionList;
    actFreeOTFENewNotHidden: TAction;
    actFreeOTFENewHidden: TAction;
    actFreeOTFEMountFileNotHidden: TAction;
    actFreeOTFEMountFileHidden: TAction;
    actNewDmCryptNotHidden: TAction;
    actNewDmCryptHidden: TAction;
    actMountDmcryptNotHidden: TAction;
    actMountDmcryptHidden: TAction;
        actDismount:  TAction;
    actExit:      TAction;
    actCDBBackup: TAction;
    actCDBRestore: TAction;
    actCDBPlaintextDump: TAction;
    actLUKSDump:  TAction;
    actRefresh:   TAction;
    actListCyphers: TAction;
    actPKCS11TokenManagement: TAction;
    ilToolbarIcons_Small: TImageList;
    StatusBar_Status: TStatusBar;
    XPManifest1:  TXPManifest;
    Listcyphers1: TMenuItem;
    actListHashes: TAction;
    Listhashes1:  TMenuItem;
    N13:          TMenuItem;
    PKCS11management1: TMenuItem;
    N14:          TMenuItem;
    actUserGuide: TAction;
    Userguide1:   TMenuItem;
    N15:          TMenuItem;
    actInstallOnUSBDrive: TAction;
    N16:          TMenuItem;
    CopyFreeOTFEtoUSBdrive1: TMenuItem;
    ilToolbarIcons_Large: TImageList;
    ilWindowsStd_24x24: TImageList;
    ilWindowsStd_16x16: TImageList;
    actAbout:     TAction;
    actOptions:   TAction;
    SDUMultimediaKeys1: TSDUMultimediaKeys;
    actCheckForUpdates: TAction;
    Checkforupdates1: TMenuItem;
    N100:         TMenuItem;
    StatusBar_Hint: TStatusBar;
    Mountfilehidden1: TMenuItem;
    Label1:       TLabel;
    miDev:        TMenuItem;
    actFullTest:  TAction;
    DoTests1:     TMenuItem;
    aLuksTest:    TAction;
    aMountLUKS: TAction;
    N5: TMenuItem;
    N9: TMenuItem;
    N19: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure miCreateKeyfileClick(Sender: TObject);
    procedure miChangePasswordClick(Sender: TObject);
    procedure actCDBPlaintextDumpExecute(Sender: TObject);
    procedure actFreeOTFENewNotHiddenExecute(Sender: TObject);
    procedure actFreeOTFENewHiddenExecute(Sender: TObject);
    procedure actFreeOTFEMountFileNotHiddenExecute(Sender: TObject);
    procedure actFreeOTFEMountFileHiddenExecute(Sender: TObject);

    // not hidden
    procedure actNewDmcryptNotHiddenExecute(Sender: TObject);
    procedure actNewHiddenDmcryptExecute(Sender: TObject);
    procedure actMountDmcryptNotHiddenExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actListCyphersExecute(Sender: TObject);
    procedure actListHashesExecute(Sender: TObject);
    procedure actPKCS11TokenManagementExecute(Sender: TObject);
    procedure actUserGuideExecute(Sender: TObject);
    procedure actInstallOnUSBDriveExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actCheckForUpdatesExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actCDBBackupExecute(Sender: TObject);
    procedure actCDBRestoreExecute(Sender: TObject);
    procedure actLUKSDumpExecute(Sender: TObject);
    procedure actMountDmcryptHiddenExecute(Sender: TObject);
    procedure actFullTestExecute(Sender: TObject);
    procedure aLuksTestExecute(Sender: TObject);
    procedure aMountLUKSExecute(Sender: TObject);

  private



  protected

    ficonIdx_Small_VistaUACShield: Integer;

    ficonIdx_Large_VistaUACShield: Integer;

    // This is set to TRUE at the very start of InitApp(...)
    finitAppCalled: Boolean;
    // This is set to TRUE at the end of calling WMUserPostShow(...) is called
    fwmUserPostShowCalledAlready: Boolean;


    // Flag to indicate that the login session is about to end
    fendSessionFlag:    Boolean;
    // Flag to indicate that the executable is about to exit
    fIsAppShuttingDown: Boolean;
    // was there an error on last run? (detected with 'AppRunning' setting)
    fWasLastRunError:   Boolean;

    fPkcs11Library: TPKCS11Library;

    fFuncTestPending: Boolean;
    // if functional test was selected in cmd line but not run yet

    procedure DoAppIdle(Sender: TObject; var Done: Boolean); // TIdleEvent

    function HandleCommandLineOpts_Create(): eCmdLine_Exit; virtual; abstract;
    function HandleCommandLineOpts_Mount(): eCmdLine_Exit; virtual;
    function HandleCommandLineOpts_EnableDevMenu(): eCmdLine_Exit; virtual;
    function HandleCommandLineOpts_DumpLUKSHdr: eCmdLine_Exit; virtual;

    procedure ReloadSettings(); virtual;

    function ActivateFreeOTFEComponent(suppressMsgs: Boolean): Boolean; virtual;
    procedure DeactivateFreeOTFEComponent(); virtual;
    procedure ShowOldDriverWarnings(); virtual;

    procedure SetupPKCS11(suppressMsgs: Boolean);
    procedure ShutdownPKCS11();
    procedure PKCS11SlotEvent(Sender: TObject; SlotID: Integer);
    procedure PKCS11TokenRemoved(SlotID: Integer); virtual; abstract;
    procedure PKCS11TokenInserted(SlotID: Integer); virtual;

    procedure AddToMRUList(filenames: TStringList); overload;
    procedure AddToMRUList(filename: String); overload;
    procedure RefreshMRUList(); virtual;
    procedure MRUListItemClicked(mruList: TSDUMRUList; idx: Integer);
      virtual; abstract;

    procedure SetStatusBarText(statusText: String);
    procedure EnableDisableControls(); virtual;

    procedure _DumpDetailsToFile(LUKSDump: Boolean);

    procedure _ExploreDrive(driveLetter: DriveLetterChar);
    procedure _AutoRunExecute(autorun: TAutorunType; driveLetter: DriveLetterChar;
      isEmergency: Boolean);

    procedure _MountFilesDetectLUKS(fileToMount: String; ReadOnly: Boolean;
      defaultType: TVolumeType); overload;
    // procedure MountFilesDetectLUKS(fileToMount: String; ReadOnly: Boolean;
    // defaultType: TDragDropFileType); OVERLOAD;
    // procedure MountFiles(mountAsSystem: TDragDropFileType; filename: String;
    // ReadOnly, forceHidden: Boolean); OVERLOAD;

    procedure _MountFile(mountAsSystem: TVolumeType; filename: String;
      ReadOnly, isHidden: Boolean; createVol: Boolean = False); overload; virtual; abstract;

      // mount dm-crypt
    procedure _PlainLinuxMountFile(isHidden: Boolean);

    {$IFDEF VER180}
    {$IFNDEF VER185}
    // Vista fix for Delphi 2006 only
    procedure WMSyscommand(var Message: TWmSysCommand); message WM_SYSCOMMAND;
    {$ENDIF}
    {$ENDIF}
    procedure AddStdIcons();
    procedure AddUACShieldIcons();
//    procedure SetupToolbarAndMenuIcons(); virtual;
//    procedure RecaptionToolbarAndMenuIcons(); virtual;
//    procedure SetIconListsAndIndexes(); virtual;
//    procedure SetupToolbarFromSettings(); virtual; abstract;

    procedure StartupUpdateCheck();

    procedure BackupRestore(dlgType: TCDBOperationType;
      volPath: TFilename = ''; hdrPath: TFilename = '';
      silent: Boolean = False);

    procedure SetStatusBarToHint(Sender: TObject);
    function EnsureOTFEComponentActive: Boolean;

    function DoFullTests: Boolean; virtual; abstract;
    function DoLuksTests: Boolean; virtual; abstract;

  public
    constructor Create(AOwner: TComponent); override;

    procedure WMUserPostShow(var msg: TWMEndSession); message WM_USER_POST_SHOW;

    procedure InitApp(); virtual;

    // Handle any command line options; returns TRUE if command line options
    // were passed through
    function HandleCommandLineOpts( ):eCmdLine_Exit;
      virtual; abstract;

    {$IFDEF VER180}
    {$IFNDEF VER185}
    // Vista fix for Delphi 2006 only
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    {$ENDIF}
  end;

resourcestring
  // Captions...
  RS_TOOLBAR_CAPTION_NEW       = 'New';
  RS_TOOLBAR_CAPTION_MOUNTFILE = 'Open container';
  RS_TOOLBAR_CAPTION_DISMOUNT  = 'Lock';
  // Hints...
  RS_TOOLBAR_HINT_NEW          = 'Create a new container';
  RS_TOOLBAR_HINT_MOUNTFILE    = 'Open a file based container';
  RS_TOOLBAR_HINT_DISMOUNT     = 'Lock an open container';

const
  USERGUIDE_LOCAL = '.\docs\index.html';

 

  // Toolbar button height is set to the image size + this value
  TOOLBAR_ICON_BORDER = 4;


implementation

{$R *.DFM}


uses
  //delphi
  DateUtils,
  ShellApi, // Required for SHGetFileInfo
  Commctrl, // Required for ImageList_GetIcon
  ComObj,   // Required for StringToGUID
  Math,     // Required for min
  {$IFDEF UNICODE}
  AnsiStrings,
  {$ENDIF}
  //lc utils/sdu

    lcConsts,
   {$IFDEF FREEOTFE_EXPLORER}
  ExplorerSettings,
  {$ENDIF}
    {$IFDEF FREEOTFE_MAIN}
  MainSettings,
  {$ENDIF}

  OTFEConsts_U,
  DriverAPI,
  PKCS11Lib, VolumeFileAPI,
  pkcs11_slot,
  SDUFileIterator_U,
  SDUGraphics,
  SDUi18n, LUKSTools,
    lcCommandLine,
  // lc forms
   frmAbout,
  frmHdrDump,
  frmFreeOTFEHdrDump,
  frmLUKSHdrDump,
  frmCypherReport,
  frmHashReport,
  frmInstallOnUSBDrive,
  frmVersionCheck, frmKeyEntryLinux,frmKeyEntryFreeOTFE, frmSelectVolumeType,frmPKCS11Management,
  frmWizardChangePasswordCreateKeyfile;

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

  AUTORUN_SUBSTITUTE_DRIVE = '%DRIVE';

constructor TfrmCommonMain.Create(AOwner: TComponent);
begin
  finitAppCalled               := False;
  fwmUserPostShowCalledAlready := False;
  inherited;
end;



// Add standard Windows icons
procedure TfrmCommonMain.AddStdIcons();

  procedure LoadSystemImagesIntoImageList(imageSet: Integer;
    ImgList: TImageList);
  var
    HToolBar: HWND;
  begin
    ImgList.Clear();
    HToolBar := CreateWindowEx(0, TOOLBARCLASSNAME, nil, WS_CHILD, 0, 0, 0,
      0, Handle, 0, HInstance, nil);
    SendMessage(HToolBar, TB_SETIMAGELIST, 0, ImgList.Handle);
    SendMessage(
      HToolBar,
      TB_LOADIMAGES,
      imageSet,
      LPARAM(HINST_COMMCTRL)
      );
    DestroyWindow(HToolBar);
  end;

  procedure AddStdIconToToolbarIcons(stdIconIdx: Integer;
  var fotfeSmallIdx: Integer;
  var fotfeLargeIdx: Integer);
  var
    tmpIdx: Integer;
  begin
    tmpIdx := ilToolbarIcons_Small.AddImage(ilWindowsStd_16x16, stdIconIdx);
    if (tmpIdx >= 0) then begin
      fotfeSmallIdx := tmpIdx;
    end;

    tmpIdx := ilToolbarIcons_Large.AddImage(ilWindowsStd_24x24, stdIconIdx);
    if (tmpIdx >= 0) then begin
      fotfeLargeIdx := tmpIdx;
    end;

  end;

begin
  // Windows standard icons: Small (16x16)
  ilWindowsStd_16x16.Width  := 16;
  ilWindowsStd_16x16.Height := 16;
  LoadSystemImagesIntoImageList(IDB_STD_SMALL_COLOR, ilWindowsStd_16x16);

  // Windows standard icons: Large (24x24)
  ilWindowsStd_24x24.Width  := 24;
  ilWindowsStd_24x24.Height := 24;
  LoadSystemImagesIntoImageList(IDB_STD_LARGE_COLOR, ilWindowsStd_24x24);

  // Copy certain icons over to the imagelists we use for menuitems/toolbar
  // buttons, and set the stored indexes
  (*
    AddStdIconToToolbarIcons(
    STD_FILENEW,
    FIconIdx_Small_New,
    FIconIdx_Large_New
    );
    AddStdIconToToolbarIcons(
    STD_FILEOPEN,
    FIconIdx_Small_MountFile,
    FIconIdx_Large_MountFile
    );
    AddStdIconToToolbarIcons(
    STD_PROPERTIES,
    FIconIdx_Small_Properties,
    FIconIdx_Large_Properties
    );
  *)
end;

// Add an standard Windows Vista UAC shield icon
procedure TfrmCommonMain.AddUACShieldIcons();
var
  anIcon:     TIcon;
  iconHandle: HICON;
begin
  // Load the Windows Vista UAC shield icon

  // --- SMALL (16x16) ICON ----------
  iconHandle := LoadImage(0, PChar(UAC_IDI_SHIELD), IMAGE_ICON,
    ilToolbarIcons_Small.Width, ilToolbarIcons_Small.Height,
    (LR_CREATEDIBSECTION or LR_SHARED));
  if (iconHandle <> 0) then begin
    anIcon := TIcon.Create();
    try
      anIcon.ReleaseHandle();

      anIcon.Handle                 := iconHandle;
      {
        if (ilToolbarIcons_Small.Width < anIcon.Width) then
        begin
        ilToolbarIcons_Small.Width := anIcon.Width;
        end;
        if (ilToolbarIcons_Small.Height < anIcon.Height) then
        begin
        ilToolbarIcons_Small.Height := anIcon.Height;
        end;
      }
      ficonIdx_Small_VistaUACShield := ilToolbarIcons_Small.AddIcon(anIcon);

      anIcon.ReleaseHandle();
      DestroyIcon(iconHandle);
    finally
      anIcon.Free();
    end;

  end;


  // --- LARGE (32x32) ICON ----------
  iconHandle := LoadImage(0, PChar(UAC_IDI_SHIELD), IMAGE_ICON,
    ilToolbarIcons_Large.Width, ilToolbarIcons_Large.Height,
    (LR_CREATEDIBSECTION or LR_SHARED));
  if (iconHandle <> 0) then begin
    anIcon := TIcon.Create();
    try
      anIcon.ReleaseHandle();

      anIcon.Handle                 := iconHandle;
      {
        if (ilToolbarIcons_Large.Width < anIcon.Width) then
        begin
        ilToolbarIcons_Large.Width := anIcon.Width;
        end;
        if (ilToolbarIcons_Large.Height < anIcon.Height) then
        begin
        ilToolbarIcons_Large.Height := anIcon.Height;
        end;
      }
      ficonIdx_Large_VistaUACShield := ilToolbarIcons_Large.AddIcon(anIcon);

      anIcon.ReleaseHandle();
      DestroyIcon(iconHandle);
    finally
      anIcon.Free();
    end;

  end;

end;


procedure TfrmCommonMain.FormCreate(Sender: TObject);
begin

  {$IFDEF VER180}
  {$IFNDEF VER185}
  if SDUOSVistaOrLater() then
  begin
    // Vista fix for Delphi 2006 only
    ShowWindow(Application.Handle, SW_HIDE);
    SetWindowLong(
      Application.Handle,
      GWL_EXSTYLE,
      (
      GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW
      )
      );
    ShowWindow(Application.Handle, SW_SHOW);
  end;
  {$ENDIF}
  {$ENDIF}

  ficonIdx_Small_VistaUACShield := -1;
  ficonIdx_Large_VistaUACShield := -1;
  // Load system icons...
  if SDUOSVistaOrLater() then
    // Mark as appropriate with UAC "shield" icons
    AddUACShieldIcons();


  // Reload settings to ensure any any components are setup, etc
  ReloadSettings();

  // We set these to invisible so that if they're disabled by the user
  // settings, it doens't flicker on them off
  StatusBar_Hint.Visible   := False;
  StatusBar_Status.Visible := False;
  Application.OnHint       := SetStatusBarToHint;
  Application.OnIdle       := DoAppIdle;

   // Redo captions, etc which otherwise get reverted when translation
  // is carried out
  self.Caption := Application.Title;

  actAbout.Caption := Format(_('About %s...'), [Application.Title]);

  actInstallOnUSBDrive.Caption :=
    Format(_('&Copy %s to USB drive...'), [Application.Title]);

end;


procedure TfrmCommonMain.FormShow(Sender: TObject);
begin
  // Post a message back to *this* form to allow any automatic updates check
  // to be carried out *after* the main form is displayed (i.e. post-show)
  // This way, there's no risk of an updates dialog getting "lost", and not
  // appearing on the taskbar if there is a newer version available
  PostMessage(self.Handle, WM_USER_POST_SHOW, 0, 0);
end;


// Reload settings, setting up any components as needed
procedure TfrmCommonMain.ReloadSettings();
begin
  SDUSetLanguage(Getsettings().OptLanguageCode);
  try
    SDURetranslateComponent(self);
  except
    on E: Exception do begin
      SDUTranslateComponent(self);
    end;
  end;

  SetupPKCS11(False);

  RefreshMRUList();
end;

 // procedure TfrmCommonMain.MountFiles(mountAsSystem: TDragDropFileType; filename: String;
 // ReadOnly, forceHidden: Boolean);
 // var
 // tmpFilenames: TStringList;
 // begin
 // tmpFilenames := TStringList.Create();
 // try
 // tmpFilenames.Add(filename);
 // MountFiles(mountAsSystem, tmpFilenames, ReadOnly, forceHidden);
 // finally
 // tmpFilenames.Free();
 // end;
 // end;


// If called with "nil", just refresh the menuitems shown
procedure TfrmCommonMain.AddToMRUList(filenames: TStringList);
begin
  if (filenames <> nil) then begin
    GetSettings().OptMRUList.Add(filenames);

    if (GetSettings().OptSaveSettings <> slNone) then begin
      GetSettings().Save();
    end;
  end;

  RefreshMRUList();

end;

procedure TfrmCommonMain.AddToMRUList(filename: String);
begin

  GetSettings().OptMRUList.Add(filename);

  if (GetSettings().OptSaveSettings <> slNone) then begin
    GetSettings().Save();
  end;


  RefreshMRUList();

end;


procedure TfrmCommonMain.RefreshMRUList();
begin
  GetSettings().OptMRUList.OnClick := MRUListItemClicked;
end;

 // procedure TfrmCommonMain.MountFilesDetectLUKS(fileToMount: String; ReadOnly: Boolean;
 // defaultType: TDragDropFileType);
 // var
 // tmpFilenames: TStringList;
 // begin
 // tmpFilenames := TStringList.Create();
 // try
 // tmpFilenames.Add(fileToMount);
 // MountFilesDetectLUKS(
 // tmpFilenames,
 // ReadOnly,
 // defaultType
 // );
 // finally
 // tmpFilenames.Free();
 // end;
 // end;


procedure TfrmCommonMain._MountFilesDetectLUKS(fileToMount: String;
  ReadOnly: Boolean;
  defaultType: TVolumeType);
var
  // i:         Integer;
  mountType: TVolumeType;
begin
  // If he volume select is a LUKS volume, mount as LUKS.
  // Otherwise mount the type passed into this function
  // (The chances of a FreeOTFE volume starting with the LUKS signature are
  // pretty low, so this is included to make life easier for LUKS users)
  mountType := vtLUKS;
  // for i := 0 to (filesToMount.Count - 1) do begin
  if not (LUksTools.IsLUKSVolume(fileToMount)) then begin
    mountType := defaultType;
    // break;
  end;
  // end;

  _MountFile(mountType, fileToMount, ReadOnly, False);
end;

procedure TfrmCommonMain.EnableDisableControls();

begin
  actPKCS11TokenManagement.Enabled := GetSettings().OptPKCS11Enable;

  // The FreeOTFE object may not be active...
  actFreeOTFENewNotHidden.Enabled       := GetFreeOTFEBase().Active;
  actFreeOTFENewHidden.Enabled       :=  actFreeOTFENewNotHidden.Enabled ;
  actFreeOTFEMountFileNotHidden.Enabled := GetFreeOTFEBase().Active;
  actFreeOTFEMountFileHidden.Enabled := GetFreeOTFEBase().Active;

  // Linux menuitem completely disabled if not active...
  miLUKSDump.Enabled        := GetFreeOTFEBase().Active;
  actNewDmCryptNotHidden.Enabled          := miLUKSDump.Enabled;
  actNewDmCryptHidden.Enabled    := miLUKSDump.Enabled;
  actMountDmcryptNotHidden.Enabled    := miLUKSDump.Enabled;
  actMountDmcryptHidden.Enabled    := miLUKSDump.Enabled;

  miChangePassword.Enabled := GetFreeOTFEBase().Active;
  miCreateKeyfile.Enabled  := GetFreeOTFEBase().Active;
  miCDB.Enabled            := GetFreeOTFEBase().Active;


  // Because the copy system "dumbly" copies the dir *this* executable is in,
  // plus anything below it, we disable the "Copy FreeOTFE to..." menuitem if
  // *this* application is running from a root dir.
  // >3 because the root'll be "X:\"; 3 chars long
  actInstallOnUSBDrive.Enabled := (length(ExtractFilePath(ParamStr(0))) > 3);
end;


procedure TfrmCommonMain.SetStatusBarText(statusText: String);
begin
  StatusBar_Status.SimpleText := statusText;
end;


function TfrmCommonMain.ActivateFreeOTFEComponent(suppressMsgs: Boolean): Boolean;
var
  obsoleteDriver: Boolean;
begin
  obsoleteDriver := False;
  try
    GetFreeOTFEBase().Active := True;
  except
    on EFreeOTFEObsoleteDriver do begin
      obsoleteDriver := True;
    end;

    else
      // We should only get to here if the FreeOTFE driver isn't both
      // installed & started

  end;

  if (not (suppressMsgs) and not (GetFreeOTFEBase().Active) and not
    fIsAppShuttingDown // Don't complain to user as we're about to exit!
    ) then begin
    if (obsoleteDriver) then begin
      SDUMessageDlg(
        Format(_(
        'In order to use this software, you must first upgrade your %s drivers to a later version.'),
        [Application.Title]) + SDUCRLF + SDUCRLF + Format(
        _('If you have just upgraded your copy of %s from a previous version, please ensure that'
        + ' you have followed the upgrade instructions that came with it, and have rebooted if necessary.'), [Application.Title]),
        mtError
        );
    end else begin
      SDUMessageDlg(
        Format(_('Unable to connect to the main %s driver.'), [Application.Title]) +
        SDUCRLF + SDUCRLF + Format(
        _('Please ensure that the main "%s" driver is installed and started.'),
        [Application.Title]) + SDUCRLF + SDUCRLF +
        Format(_('If you have only just installed %s, please reboot and try again.'),
        [Application.Title]),
        mtError
        );
    end;

    // Note that we don't terminate here; the user may still use the driver
    // control functionality to install and/or start the FreeOTFE driver
  end;

  // Old driver warnings
  if GetFreeOTFEBase().Active then
    ShowOldDriverWarnings();

  Result := GetFreeOTFEBase().Active;
end;

procedure TfrmCommonMain.ShowOldDriverWarnings();
begin
  // No warnings to be shown in base class
end;

procedure TfrmCommonMain.DeactivateFreeOTFEComponent();
begin
  GetFreeOTFEBase().Active := False;
end;

procedure TfrmCommonMain.DoAppIdle(Sender: TObject; var Done: Boolean);
begin
  if fFuncTestPending then begin
    fFuncTestPending := False;
    // actCheckForUpdatesExecute(self);
    // actTestExecute(self);
    // Close;

  end;
  Done := True;
end;

procedure TfrmCommonMain.actCDBBackupExecute(Sender: TObject);
begin
  BackupRestore(opBackup);
end;

procedure TfrmCommonMain.actCDBRestoreExecute(Sender: TObject);
begin
  BackupRestore(opRestore);
end;

procedure TfrmCommonMain.BackupRestore(dlgType: TCDBOperationType;
  volPath: TFilename = ''; hdrPath: TFilename = ''; silent: Boolean = False);
var
  dlg: TfrmHdrBackupRestore;
begin
  dlg := TfrmHdrBackupRestore.Create(self);

  try
    dlg.silent := silent;
    if dlgType = opBackup then begin
      dlg.SrcFilename  := volPath;
      dlg.DestFilename := hdrPath;
    end else begin
      dlg.SrcFilename  := hdrPath;
      dlg.DestFilename := volPath;
    end;

    dlg.OpType := dlgType;
    dlg.ShowModal();

  finally
    dlg.Free();
  end;
end;

procedure TfrmCommonMain.miCreateKeyfileClick(Sender: TObject);
begin
  frmWizardChangePasswordCreateKeyfile.WizardCreateKeyfile();
end;

procedure TfrmCommonMain.miChangePasswordClick(Sender: TObject);
begin
  frmWizardChangePasswordCreateKeyfile.WizardChangePassword();
end;

procedure TfrmCommonMain.actCDBPlaintextDumpExecute(Sender: TObject);
begin
  _DumpDetailsToFile(False);
end;

procedure TfrmCommonMain.actLUKSDumpExecute(Sender: TObject);
begin
  _DumpDetailsToFile(True);
end;

procedure TfrmCommonMain._DumpDetailsToFile(LUKSDump: Boolean)  ;
var
  dlg: TfrmHdrDump;
begin
  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    if LUKSDump then
      dlg := TfrmLUKSHdrDump.Create(self)
    else
      dlg := TfrmFreeOTFEHdrDump.Create(self);
    try
//      dlg.silent := useSilent;

//
//      dlg.volumeFileName := volume;
//      dlg.password :=password ;
//      dlg.DumpFilename := dump_file;
      dlg.ShowModal();
    finally
      dlg.Free();
    end;
  end;
end;


procedure TfrmCommonMain._ExploreDrive(driveLetter: DriveLetterChar);
begin
  if (driveLetter <> #0) then begin
    // explorerCommandLine := 'explorer.exe ' + driveLetter + ':\';
    // ShellExecute(Application.Handle, 'open', PWideChar('Explorer'),PWideChar(driveLetter + ':\') , nil, cmdShow) ;

    if not (SDUWinExecNoWait32('Explorer', driveLetter + ':\', SW_RESTORE)) then
      SDUMessageDlg(_('Error running Explorer'), mtError, [mbOK], 0);
  end;

end;


// Launch autorun executable on specified drive
procedure TfrmCommonMain._AutoRunExecute(autorun: TAutorunType;
  driveLetter: DriveLetterChar;
  isEmergency: Boolean);
var
  exeParams, exeFullCmdLine: String;
  launchOK:                  Boolean;
  splitCmdLine:              TStringList;
  exeOnly:                   String;
begin
  // Sanity...
  if (driveLetter = #0) then begin
    exit;
  end;

  exeFullCmdLine := '';

  case autorun of

    arPostMount:
    begin
      exeFullCmdLine := GetSettings().OptPostMountExe;
    end;

    arPreDismount:
    begin
      // We don't bother with predismount in case of emergency
      if not (isEmergency) then begin
        exeFullCmdLine := GetSettings().OptPreDismountExe;
      end;
    end;

    arPostDismount:
    begin
      exeFullCmdLine := GetSettings().OptPostDismountExe;
    end;

    else
    begin
      if not (isEmergency) then begin
        SDUMessageDlg(_('Unknown autorun type?!'), mtError);
      end;
    end;

  end;

  if (exeFullCmdLine <> '') then begin
    // Split up, in case user is using a commandline with spaces in the
    // executable path
    splitCmdLine := TStringList.Create();
    try
      splitCmdLine.QuoteChar     := '"';
      splitCmdLine.Delimiter     := ' ';
      splitCmdLine.DelimitedText := trim(exeFullCmdLine);

      // Relative path with mounted drive letter
      if ((autorun = arPostMount) or (autorun = arPreDismount)) then begin
        splitCmdLine[0] := driveLetter + ':' + splitCmdLine[0];
      end;

      exeOnly := splitCmdLine[0];
      splitCmdLine.Delete(0);
      // Recombine to produce new commandline
      exeParams := splitCmdLine.DelimitedText;

    finally
      splitCmdLine.Free();
    end;

    // Perform substitution, if needed
    exeParams := StringReplace(exeParams, AUTORUN_SUBSTITUTE_DRIVE,
      // Cast to prevent compiler warning
      Char(driveLetter), [rfReplaceAll]);


    // NOTE: THIS MUST BE UPDATED IF CMDLINE IS TO SUPPORT COMMAND LINE
    // PARAMETERS!
    if not (FileExists(exeOnly)) then begin
      if (not (isEmergency) and GetSettings().OptPrePostExeWarn) then begin
        SDUMessageDlg(
          Format(_('Unable to locate %s executable:'), [AUTORUN_TITLE[autorun]]) +
          SDUCRLF + SDUCRLF + exeOnly,
          mtWarning
          );
      end;
    end else begin
      if (autorun = arPreDismount) then begin
        // Launch and block until terminated...
        launchOK := (SDUWinExecAndWait32(exeFullCmdLine, SW_SHOW,
          ExtractFilePath(exeOnly)) <> EXEC_FAIL_CODE);
      end else begin
        // Fire and forget...
        launchOK := SDUWinExecNoWait32(exeOnly, exeParams, SW_RESTORE);
      end;

      if not (launchOK) then begin
        if not (isEmergency) then begin
          SDUMessageDlg(
            Format(_('Error running %s executable:'), [AUTORUN_TITLE[autorun]]) +
            SDUCRLF + SDUCRLF + exeFullCmdLine,
            mtError
            );
        end;
      end;
    end;
  end;
end;


procedure TfrmCommonMain.actFreeOTFENewNotHiddenExecute(Sender: TObject);
begin
  // Do nothing; method still required here in order that the actionItem isn't
  // automatically disabled because it does nothing
end;

procedure TfrmCommonMain.actFreeOTFENewHiddenExecute(Sender: TObject);
begin
  // Do nothing; method still required here in order that the actionItem isn't
  // automatically disabled because it does nothing
end;

procedure TfrmCommonMain.actInstallOnUSBDriveExecute(Sender: TObject);
var
  dlg: TfrmInstallOnUSBDrive;
begin
  dlg := TfrmInstallOnUSBDrive.Create(nil);
  try
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;

{ DONE 1 -otdk -ccomplete : add similar for hidden volumes }
procedure TfrmCommonMain.actFreeOTFEMountFileNotHiddenExecute(Sender: TObject);
//var
//buf:array[0..99] of Widechar;
begin
// GetCurrentDirectory(100,buf);
  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
//    ForceCurrentDirectory:= true;

    SDUOpenSaveDialogSetup(OpenDialog, '');
    FreeOTFEGUISetupOpenSaveDialog(OpenDialog);
    assert(OpenDialog <> nil);
    OpenDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
    OpenDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
    OpenDialog.Options    := OpenDialog.Options - [ofAllowMultiSelect];

    if OpenDialog.Execute() then begin
    //warn if luks?
       _MountFile(vtFreeOTFE, OpenDialog.filename, ((ofReadOnly in OpenDialog.Options) or FileIsReadOnly(
        OpenDialog.filename)
        ), False);

    end;
  end;
end;

procedure TfrmCommonMain.actFreeOTFEMountFileHiddenExecute(Sender: TObject);
begin
  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    SDUOpenSaveDialogSetup(OpenDialog, '');
    FreeOTFEGUISetupOpenSaveDialog(OpenDialog);
    assert(OpenDialog <> nil);
    OpenDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
    OpenDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
    OpenDialog.Options    := OpenDialog.Options - [ofAllowMultiSelect];

    if OpenDialog.Execute() then begin
    //warn if luks?
       _MountFile(vtFreeOTFE, OpenDialog.filename, ((ofReadOnly in OpenDialog.Options) or FileIsReadOnly(
        OpenDialog.filename)
        ), true);

    end;
  end;
end;

// creates plain dmcrypt vol
procedure TfrmCommonMain.actNewDmcryptNotHiddenExecute(Sender: TObject);
begin   // create enc vol
    _MountFile(vtPlainLinux, '', False, false, True);
end;

// creates hidden dmcrypt vol
procedure TfrmCommonMain.actNewHiddenDmcryptExecute(Sender: TObject);
//var
//  filename: String;
begin
  // create empty file
//  if frmKeyEntryLinux.CreatePlainLinuxVolumeWizard(filename) then begin

//    SDUMessageDlg(
//      _('Linux container created successfully.') + SDUCRLF + SDUCRLF +
//      _('Now this container will be mounted and formated before use.'),
//      mtInformation);
    { TODO 1 -otdk -cenhance : wipe volume first }
    // create enc vol
    _MountFile(vtPlainLinux, '', False, true, True);
//  end else begin
//    if (GetFreeOTFEBase().LastErrorCode <> OTFE_ERR_USER_CANCEL) then begin
//      SDUMessageDlg(_('Linux container could not be created'), mtError);
//    end;
//  end;
end;

procedure TfrmCommonMain.actListHashesExecute(Sender: TObject);
var
  dlg: TfrmHashReport;
begin
  dlg := TfrmHashReport.Create(self);
  try
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;

procedure TfrmCommonMain.actListCyphersExecute(Sender: TObject);
var
  dlg: TfrmCypherReport;
begin
  dlg := TfrmCypherReport.Create(self);
  try
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;

{ DONE 1 -otdk -ccomplete : add similar for luks }
procedure TfrmCommonMain._PlainLinuxMountFile(isHidden: Boolean);
begin
  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    SDUOpenSaveDialogSetup(OpenDialog, '');
    FreeOTFEGUISetupOpenSaveDialog(OpenDialog);
    { TODO 1 -otdk -crefactor : move these properties into designer }
    OpenDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
    OpenDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
    OpenDialog.Options    := OpenDialog.Options - [ofAllowMultiSelect];
    if OpenDialog.Execute() then begin
      _MountFile(
        vtPlainLinux,
        OpenDialog.filename,
        ((ofReadOnly in OpenDialog.Options) or FileIsReadOnly(
        OpenDialog.filename) // Only bother checking the first files
        ), isHidden
        );
    end;
  end;

end;

procedure TfrmCommonMain.aMountLUKSExecute(Sender: TObject);
begin
  inherited;
  _MountFile(vtLUKS,'',false,false,false);
end;



procedure TfrmCommonMain.actMountDmcryptNotHiddenExecute(Sender: TObject);
begin
  _PlainLinuxMountFile(False);
end;

procedure TfrmCommonMain.actMountDmcryptHiddenExecute(Sender: TObject);
begin
  _PlainLinuxMountFile(True);
end;

procedure TfrmCommonMain.actPKCS11TokenManagementExecute(Sender: TObject);
begin
  frmPKCS11Management.ShowPKCS11ManagementDlg();
end;

procedure TfrmCommonMain.actRefreshExecute(Sender: TObject);
begin
  // Do nothing in back class - dummy method required to create event for use
  // with multimedia key component to call
end;

procedure TfrmCommonMain.actFullTestExecute(Sender: TObject);
begin
  inherited;
  DoFullTests(); // abstract
end;

procedure TfrmCommonMain.aLuksTestExecute(Sender: TObject);
begin
  inherited;
  DoLuksTests(); // abstract
end;


procedure TfrmCommonMain.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TfrmCommonMain.actUserGuideExecute(Sender: TObject);
var
  cwd:       String;
  userGuide: String;
begin
  // Determine full path to local userguide
  cwd := ParamStr(0);
  cwd := ExtractFilePath(cwd);

  {$IFDEF DEBUG}
  // browse in dev directory, from bin\PC\Debug\Win32\ to root
  cwd       := cwd + '..\..\..\..';
  {$ENDIF}
  userGuide := cwd + '\' + USERGUIDE_LOCAL;

  // If local userguide doens't exist, fallback to online version...
  if not (FileExists(userGuide)) then begin
    userGuide := URL_USERGUIDE;

    if not (SDUConfirmYN(_('This requires a connection to the internet. Continue?'))) then
      exit;

  end;

  // Open HTML userguide
  ShellExecute(
    self.Handle,
    PChar('open'),
    PChar(userGuide),
    PChar(''),
    PChar(''),
    SW_SHOW
    );

end;


{$IFDEF VER180}
{$IFNDEF VER185}

// Vista fix for Delphi 2006 only
procedure TfrmCommonMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if SDUOSVistaOrLater() then
  begin
    Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or
      WS_EX_APPWINDOW;
  end;

end;
{$ENDIF}
{$ENDIF}

{$IFDEF VER180}
{$IFNDEF VER185}

// Vista fix for Delphi 2006 only
procedure TfrmCommonMain.WMSyscommand(var Message: TWmSysCommand);
begin
  if not(SDUOSVistaOrLater()) then begin
    inherited;
  end else begin
    case (message.CmdType and $FFF0) of
      SC_MINIMIZE:
        begin
          ShowWindow(Handle, SW_MINIMIZE);
          message.Result := 0;
        end;

      SC_RESTORE:
        begin
          ShowWindow(Handle, SW_RESTORE);
          message.Result := 0;
        end;

    else
      begin
        inherited;
      end;

    end;
  end;

end;
{$ENDIF}
{$ENDIF}


procedure TfrmCommonMain.SetupPKCS11(suppressMsgs: Boolean);
var
  tmpPKCS11Lib: TPKCS11Library;
begin
  ShutdownPKCS11();

  tmpPKCS11Lib := nil;
  if (GetSettings().OptPKCS11Enable and (GetSettings().OptPKCS11Library <> '')) then begin
    try
      tmpPKCS11Lib             := TPKCS11Library.Create(GetSettings().OptPKCS11Library);
      tmpPKCS11Lib.OnSlotEvent := PKCS11SlotEvent;
      tmpPKCS11Lib.Initialize();
      fPkcs11Library                  := tmpPKCS11Lib;
      GPKCS11Library := fPkcs11Library;
    except
      on E: Exception do begin
        if not (suppressMsgs) then begin
          SDUMessageDlg(
            _('Unable to initialize PKCS#11 support:') + SDUCRLF + SDUCRLF + E.Message,
            mtError
            );
        end;

        if (tmpPKCS11Lib <> nil) then begin
          tmpPKCS11Lib.Free();
        end;
      end;
    end;

  end;

end;

procedure TfrmCommonMain.ShutdownPKCS11();
begin
  // Sanity; strip from FreeOTFE copmponent
  GPKCS11Library := nil;

  if (fPkcs11Library <> nil) then begin
    try
      fPkcs11Library.Finalize();
      fPkcs11Library.Free();
    except
      on E: Exception do
        // Just swallow it - if there's a problem, there's nothing we can do
        // about it
    end;
  end;

  fPkcs11Library := nil;
end;


procedure TfrmCommonMain.PKCS11TokenInserted(SlotID: Integer);
var
  // tmpFilename:   TStringList;
  mountNameOnly: String;
begin
  SetStatusBarText(Format(_('Detected insertion of token into slot ID: %u'), [SlotID]));
  if GetSettings().OptPKCS11AutoMount then begin
    // Attempt automount...
    // tmpFilename := TStringList.Create;
    // try
    // tmpFilename.Add(GetSettings().OptPKCS11AutoMountVolume);
    mountNameOnly := ExtractFilename(GetSettings().OptPKCS11AutoMountVolume);
    SetStatusBarText(Format(_('Automounting: %s'), [mountNameOnly]));
    _MountFilesDetectLUKS(
      GetSettings().OptPKCS11AutoMountVolume,
      False,
      {$IFDEF FREEOTFE_MAIN}
      GetSettings().OptDragDropFileType
      {$ENDIF}
      {$IFDEF FREEOTFE_EXPLORER}
      vtUnknown // lplp
      {$ENDIF}
      );
    // finally
    // tmpFilename.Free();
    // end;

  end;

end;

procedure TfrmCommonMain.PKCS11SlotEvent(Sender: TObject; SlotID: Integer);
var
  lib:  TPKCS11Library;
  slot: TPKCS11Slot;
begin
  // Sanity check; we can't do anything if FreeOTFE component's not available
  if GetFreeOTFEBase().Active then begin
    SetStatusBarText(Format(_('Slot event on slot ID: %d'), [SlotID]));

    lib  := TPKCS11Library(Sender);
    slot := lib.SlotByID[SlotID];
    if (slot = nil) then begin
      // Assume token removed, and the PKCS#11 driver dynamically removes the
      // slot when that happens
      PKCS11TokenRemoved(SlotID);
    end else begin
      if slot.TokenPresent then begin
        PKCS11TokenInserted(SlotID);
      end else begin
        PKCS11TokenRemoved(SlotID);
      end;
    end;

  end;
end;

procedure TfrmCommonMain.StartupUpdateCheck();
var
  nextCheck: TDate;
  doCheck:   Boolean;
begin
  doCheck   := False; // fix warning
  nextCheck := 0;     // fix warning
  case GetSettings().OptUpdateChkFrequency of
    ufNever:
      doCheck := False;
    // for ufRandom - check once per 1000 calls
    ufRandom:
      doCheck := Random(1000) = 123;
    ufAlways:
      doCheck := True;

    ufWeekly:
      // Simply add on 7 days
      nextCheck := GetSettings().OptUpdateChkLastChecked + 7;

    ufMonthly:
      // Add on the number of days in the month it was last checked
      nextCheck := GetSettings().OptUpdateChkLastChecked +
        DaysInMonth(GetSettings().OptUpdateChkLastChecked);

    ufAnnually:
      nextCheck := GetSettings().OptUpdateChkLastChecked +
        DaysInYear(GetSettings().OptUpdateChkLastChecked);
    else
    begin
      assert(False, 'UPDATE TfrmCommonMain.StartupUpdateCheck');
      doCheck := True;
    end;
  end;

  if GetSettings().OptUpdateChkFrequency in [ufMonthly, ufAnnually, ufWeekly] then
    doCheck := nextCheck <= Now();

  if doCheck then begin
    CheckForUpdates_AutoCheck(
      URL_PADFILE,
      GetSettings().OptUpdateChkFrequency,
      GetSettings().OptUpdateChkLastChecked,
      GetSettings().OptUpdateChkSuppressNotifyVerMajor,
      GetSettings().OptUpdateChkSuppressNotifyVerMinor
      );
    // Save any changes to the auto check for updates settings (eg check date)...
    GetSettings().Save();
  end;
end;

procedure TfrmCommonMain.actCheckForUpdatesExecute(Sender: TObject);
begin
  CheckForUpdates_UserCheck(URL_PADFILE);
end;

procedure TfrmCommonMain.WMUserPostShow(var msg: TWMEndSession);
begin
  if not fwmUserPostShowCalledAlready then
    StartupUpdateCheck();

  fwmUserPostShowCalledAlready := True;
end;

procedure TfrmCommonMain.InitApp();
begin
  finitAppCalled := True;
end;

function TfrmCommonMain.EnsureOTFEComponentActive(): Boolean;
begin
  Result := True;
  if not GetFreeOTFEBase().Active then
    Result := ActivateFreeOTFEComponent(True);
end;


function TfrmCommonMain.HandleCommandLineOpts_EnableDevMenu(): eCmdLine_Exit;
begin
  Result           := ceSUCCESS;
  fFuncTestPending := False;
  if GetCmdLine.IsEnableDevMenu then begin
    miDev.Visible    := True;
    fFuncTestPending := True;
  end;
end;

 // Handle "/dump" command line
 // Returns: Exit code
 function TfrmCommonMain.HandleCommandLineOpts_DumpLUKSHdr(): eCmdLine_Exit;
// var
//  volume:                VolumeFilenameString;
//  password:        String;
//dump_file :        String;
//      useSilent:             Boolean;
 begin

    Result := ceSUCCESS;

  if GetCmdLine.isDump then begin
        if GetCmdLine.VolumeArg<> '' then begin
        if GetCmdLine.filenameArg<>'' then begin
      if not (EnsureOTFEComponentActive) then begin
        Result := ceUNABLE_TO_CONNECT;
      end else begin
         _DumpDetailsToFile(true);
        end;

      end;
        end;
  end;

 end;


 // Handle "/mount" command line
 // Returns: Exit code
function TfrmCommonMain.HandleCommandLineOpts_Mount(): eCmdLine_Exit;
var
  volume:                VolumeFilenameString;
  ReadOnly:              Boolean;
  mountAs:               DriveLetterChar;
  fileOK:                Boolean;
  {$IFDEF FREEOTFE_MAIN}
  useDriveLetter:        string;
  {$ENDIF}
  useKeyfile:            KeyFilenameString;
//  useKeyfileIsASCII:     Boolean;
  useKeyfileNewlineType: TSDUNewline;
  useLESFile:            FilenameString;
  usePassword:           TSDUBytes;
//  strUsePassword:        String;
//  useSilent:             Boolean;
  strTemp:               String;
  useOffset:             ULONGLONG;
  useNoCDBAtOffset:      Boolean;
  useSaltLength:         Integer;
  useKeyIterations:      Integer;
  tmpNewlineType:        TSDUNewline;
  currParamOK:           Boolean;
  useType:TVolumeType;
begin
  Result := ceSUCCESS;

  if GetCmdLine.isMount then begin
    Result := ceINVALID_CMDLINE;

    volume  :=  GetCmdLine.VolumeArg;
    if volume <> '' then begin
      if not (EnsureOTFEComponentActive) then begin
        Result := ceUNABLE_TO_CONNECT;
      end else begin
        Result := ceINVALID_CMDLINE;
        fileOK := True;

        ReadOnly := getCmdLine.isReadonly;
        { TODO 1 -otdk -cbug : warn user if not ansi password }
//        if not (SDUCommandLineParameter(CMDLINE_PASSWORD, strUsePassword)) then
//          strUsePassword := '';

        usePassword := SDUStringToSDUBytes(GetCmdLine.PasswordArg);
//        useSilent   := SDUCommandLineSwitch(CMDLINE_SILENT);

        useOffset := 0;
        strTemp:= GetCmdLine.OffsetArg;
        if strTemp<>'' then
          fileOK := SDUParseUnitsAsBytesUnits(strTemp, useOffset);

        useNoCDBAtOffset := GetCmdLine.isNocdbatoffset;

        useSaltLength := DEFAULT_SALT_LENGTH;
        strTemp:= GetCmdLine.SaltlengthArg;
        if strTemp<>'' then
          fileOK := TryStrToInt(strTemp, useSaltLength);

        useKeyIterations := DEFAULT_KEY_ITERATIONS;
        strTemp:= GetCmdLine.KeyiterationsArg;
        if strTemp<>'' then
          fileOK := TryStrToInt(strTemp, useKeyIterations);


        {$IFDEF FREEOTFE_MAIN}
         useDriveLetter := GetCmdLine.DriveArg;
        if useDriveLetter<>'' then begin
          if (length(useDriveLetter) = 0) then
            fileOK                           := False
          else
            GetSettings().OptDefaultDriveLetter := (uppercase(useDriveLetter))[1];

        end;
        {$ENDIF}
        useKeyfile := GetCmdLine.KeyfileArg;
        if useKeyfile<>'' then begin
          if (length(useKeyfile) = 0) then begin
            fileOK := False;
          end else begin
            useKeyfile := SDURelativePathToAbsolute(useKeyfile);
            if not (FileExists(useKeyfile)) then begin
              Result := ceFILE_NOT_FOUND;
              fileOK := False;
            end;
          end;
        end;

        // Flag for Linux keyfiles containing ASCII passwords
//        useKeyfileIsASCII := SDUCommandLineSwitch(CMDLINE_KEYFILEISASCII);

        // Setting for newlines where Linux keyfiles contain ASCII passwords
        useKeyfileNewlineType := LINUX_KEYFILE_DEFAULT_NEWLINE;
        strTemp := GetCmdLine.KeyfilenewlineArg;
        if strTemp<>'' then begin
          currParamOK := False;
          for tmpNewlineType := low(tmpNewlineType) to high(tmpNewlineType) do begin
            if (strTemp = uppercase(SDUNEWLINE_TITLE[tmpNewlineType]))
            then begin
              useKeyfileNewlineType := tmpNewlineType;
              currParamOK           := True;
              break;
            end;
          end;

          if not (currParamOK) then begin
            Result := ceINVALID_CMDLINE;
            fileOK := False;
          end;
        end;


        useLESFile := GetCmdLine.LesfileArg;
        if useLESFile<>'' then begin
          if (length(useLESFile) = 0) then begin
            fileOK := False;
          end else begin
            useLESFile := SDURelativePathToAbsolute(useLESFile);
            if not (FileExists(useLESFile)) then begin
              Result := ceFILE_NOT_FOUND;
              fileOK := False;
            end;
          end;
        end;

        // Convert any relative path to absolute, based on CWD - but not if a
        // device passed through!
        if (Pos(uppercase(DEVICE_PREFIX), volume) <= 0) then begin
          volume := SDURelativePathToAbsolute(volume);
          if not (FileExists(volume)) then begin
            Result := ceFILE_NOT_FOUND;
            fileOK := False;
          end;
        end;

        if fileOK then begin
           //determine type to open as
           useType := vtUnknown;


           // use user cmd line as top priority, do in inverse order of safety in case user specified > 1
           if GetCmdLine.isDmcrypt or (useLESFile <> '') then  useType := vtPlainLinux;
           if GetCmdLine.isFreeotfe then  useType := vtFreeOTFE;
           // if still unknown test for luks, else dont as use reads from disk
           if useType = vtUnknown then
             if IsLUKSVolume(volume) then  useType := vtLUKS;

           // keyfiles can be for luks or freeotfe
           //  LUKS volume should be found above
           if (useType = vtUnknown) and (useKeyfile <> '') then useType := vtFreeOTFE;

           // if still unknown, use drag drop default
           if useType = vtUnknown then
              useType := GetSettings().OptDragDropFileType;

           case useType of
              vtPlainLinux:
                frmKeyEntryLinux.MountPlainLinux(volume, mountAs, ReadOnly,
                  useLESFile,
                  usePassword, useOffset);
              vtLUKS:
                MountLUKS(volume,mountAs, ReadOnly,
               usePassword,useKeyfile ,GetCmdLine.isKeyfileisascii,useKeyfileNewlineType,
              GetCmdLine.IsSilent);
              vtFreeOTFE:
                   mountAs := MountFreeOTFE(volume, ReadOnly,
              useKeyfile, usePassword, useOffset, useNoCDBAtOffset,
              GetCmdLine.IsSilent, useSaltLength, useKeyIterations);
              else
                //still unknown - prompt
                mountAs := frmSelectVolumeType.Mount(volume, ReadOnly);
           end;



          Result := ceSUCCESS;
          if (mountAs = #0) then
            Result := ceUNABLE_TO_MOUNT;

        end;
      end;
    end;
  end;
end;

procedure TfrmCommonMain.SetStatusBarToHint(Sender: TObject);
var
  showHint:       String;
  statusBarShown: Boolean;
begin
  statusBarShown := (StatusBar_Hint.Visible or StatusBar_Status.Visible);

  // Note: GetLongHint(...) returns '' if there's no hint
  showHint := GetLongHint(Application.Hint);

  if (showHint <> '') then begin
    // If a hint is to be shown, display statusbar with single panel that has
    // no bevel
    // Note: To so this, we have to use a second statusbar, to prevent messing
    // with the "standard" statusbar's panels
    // We can't use SimpleText for this, as that has a bevel on the
    // single panel
    StatusBar_Status.Visible      := False;
    StatusBar_Hint.Visible        := statusBarShown;
    StatusBar_Hint.panels[0].Text := showHint;
  end else begin
    // If no hint is to be shown, display "standard" statusbar with multiple panels
    StatusBar_Status.Visible      := statusBarShown;
    StatusBar_Hint.Visible        := False;
    StatusBar_Hint.panels[0].Text := '';
  end;

end;


end.
