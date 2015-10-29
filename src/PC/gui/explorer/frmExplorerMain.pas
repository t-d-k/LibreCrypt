unit frmExplorerMain;

interface

uses
  // delphi & libs
  ActnList,
  Buttons, Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, Forms, StdCtrls, SysUtils, ToolWin, Variants,
  Windows, XPMan,
  Graphics, ImgList, Menus, Messages,
  // sdu & LibreCrypt utils
  SDUWinHttp_API,
  OTFE_U, OTFEConsts_U,
  OTFEFreeOTFEBase_U, PartitionImageDLL,
  OTFEFreeOTFEDLL_U, SDFilesystem,
  SDFilesystem_FAT,
  SDFilesystemCtrls,
  SDPartitionImage,
  SDPartitionImage_File,
  SDUComCtrls, lcDialogs, SDUDropFiles, SDUFileIterator_U,
  SDUForms,
  SDUGeneral,
  SDUMRUList,
  SDUMultimediaKeys,
  dlgProgress,
  Shredder, SDUDialogs,
  lcTypes,
  CommonSettings,
  ExplorerSettings,
  ExplorerWebDAV,
  // LibreCrypt forms
  frmCommonMain;

const
  WM_FREEOTFE_EXPLORER_REFRESH = WM_USER + 1;

type
  TLastFocussed   = (lfTreeView, lfListView, lfTreePopup);
  TFExplOperation = (cmCopy, cmMove, cmDelete);

resourcestring
  RS_STORE = 'store';
  RS_EXTRACT = 'extract';
  RS_MOVE = 'move';
  RS_COPY = 'copy';
  RS_DELETE = 'delete';
  RS_STORING = 'storing';
  RS_EXTRACTING = 'extracting';
  RS_MOVING = 'moving';
  RS_COPYING = 'copying';
  RS_DELETING = 'deleting';
  RS_ENCRYPTED_VOLUME = 'Locked Container';
  RS_VOLUME_PLAINTEXT = 'Open Container';
  RS_DETERMINING_SIZE_MSG = 'Determining size of files/folders....';

const
  OperationTitlePtr: array [TFExplOperation] of Pointer     = (@RS_COPY, @RS_MOVE, @RS_DELETE);
  OperationVerbTitlePtr: array [TFExplOperation] of Pointer = (@RS_COPYING, @RS_MOVING, @RS_DELETING);

type
  TfrmExplorerMain = class(TfrmCommonMain)
    pnlTopSpacing: TPanel;
    pmTreeView: TPopupMenu;
    pmListView: TPopupMenu;
    mnuTreeViewExpand: TMenuItem;
    mnuExploreView: TMenuItem;
    mnuListViewExtract: TMenuItem;
    SDUOpenDialog_MountPlaintextImage: TSDUOpenDialog;
    ImageList_StatusBar: TImageList;
    mnuListViewStore: TMenuItem;
    mnuListViewStoreFile: TMenuItem;
    mnuListViewStoreDir: TMenuItem;
    mnuViewDelete: TMenuItem;
    mnuViewProperties: TMenuItem;
    mnuViewCreateSubDir: TMenuItem;
    mnuTreeViewCollapse: TMenuItem;
    SDUOpenDialog_Store: TSDUOpenDialog;
    SDUSaveDialog_Extract: TSDUSaveDialog;
    SDUDropFilesTreeView: TSDUDropFiles;
    SDUDropFilesListView: TSDUDropFiles;
    actPlaintextMountFile: TAction;
    Mountplaintextimage1: TMenuItem;
    Edit2: TMenuItem;
    actSelectAll: TAction;
    Selectall1: TMenuItem;
    actShowBootSector: TAction;
    actCheckFilesystem: TAction;
    actListStyleSmallIcons: TAction;
    actListStyleIcons: TAction;
    actListStyleList: TAction;
    actListStyleDetails: TAction;
    N1: TMenuItem;
    Icons1: TMenuItem;
    SmallIcons1: TMenuItem;
    List1: TMenuItem;
    Details1: TMenuItem;
    actCheckStatusBar: TAction;
    actCheckAddressBar: TAction;
    oolbars1: TMenuItem;
    Statusbar2: TMenuItem;
    Addressbar1: TMenuItem;
    N6: TMenuItem;
    Showbootsectordetails1: TMenuItem;
    Checkfilesystem1: TMenuItem;
    pmToolbarBack: TPopupMenu;
    pmToolbarViews: TPopupMenu;
    pmToolbarForward: TPopupMenu;
    pnlExplorer: TPanel;
    SDFilesystemListView1: TSDFilesystemListView;
    Splitter1: TSplitter;
    SDFilesystemTreeView1: TSDFilesystemTreeView;
    pnlAddressBar: TPanel;
    lblFolder: TLabel;
    edPath: TEdit;
    pbGo: TBitBtn;
    ToolbarExplorer: TToolBar;
    tbbNavigateBack: TToolButton;
    tbbNavigateForward: TToolButton;
    tbbUp: TToolButton;
    ToolButton3: TToolButton;
    tbbMoveTo: TToolButton;
    tbbCopyTo: TToolButton;
    ToolButton1: TToolButton;
    tbbDelete: TToolButton;
    ToolButton4: TToolButton;
    tbbViews: TToolButton;
    ToolBarVolume: TToolBar;
    tbbNew: TToolButton;
    ToolButton2: TToolButton;
    tbbMountFile: TToolButton;
    ToolButton5: TToolButton;
    tbbDismount: TToolButton;
    actCheckToolbarVolume: TAction;
    actCheckToolbarExplorer: TAction;
    StandardButtons1: TMenuItem;
    Volume1: TMenuItem;
    tbbExtract: TToolButton;
    tbbStore: TToolButton;
    ToolButton6: TToolButton;
    Icons2: TMenuItem;
    SmallIcons2: TMenuItem;
    List2: TMenuItem;
    Details2: TMenuItem;
    actDelete: TAction;
    actStoreFile: TAction;
    actExtract: TAction;
    pmToolbarStore: TPopupMenu;
    actStoreDir: TAction;
    mnuToolbarStoreFile: TMenuItem;
    mnuToolbarStoreDir: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    actCreateSubDir: TAction;
    tbbItemProperties: TToolButton;
    ToolButton8: TToolButton;
    actItemProperties: TAction;
    N10: TMenuItem;
    mnuTreeViewCreateSubDir: TMenuItem;
    mnuTreeViewDelete: TMenuItem;
    N11: TMenuItem;
    mnuTreeViewExtract: TMenuItem;
    mnuTreeViewStore: TMenuItem;
    mnuTreeViewStoreFile: TMenuItem;
    mnuTreeViewStoreDir: TMenuItem;
    N12: TMenuItem;
    mnuTreeViewItemProperties: TMenuItem;
    actUpDir: TAction;
    actNavigateBack: TAction;
    actNavigateForward: TAction;
    Options1: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    mnuMainExtract: TMenuItem;
    mnuMainStore: TMenuItem;
    File2: TMenuItem;
    Folder1: TMenuItem;
    Plaintextimage1: TMenuItem;
    miPlaintextDismount: TMenuItem;
    actPlaintextNew: TAction;
    New1: TMenuItem;
    actCheckExplorerBarFolders: TAction;
    ExplorerBar1: TMenuItem;
    Folders1: TMenuItem;
    tbbExplorerBarFolders: TToolButton;
    ToolButton9: TToolButton;
    actRename: TAction;
    mnuTreeViewRename: TMenuItem;
    mnuViewRename: TMenuItem;
    actMoveTo: TAction;
    actCopyTo: TAction;
    CopyToFolder1: TMenuItem;
    MoveToFolder1: TMenuItem;
    actInvertSelection: TAction;
    InvertSelection1: TMenuItem;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    N20: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N21: TMenuItem;
    mnuTreeViewCut: TMenuItem;
    mnuTreeViewCopy: TMenuItem;
    mnuTreeViewPaste: TMenuItem;
    actcut1: TMenuItem;
    Copy3: TMenuItem;
    Paste3: TMenuItem;
    N22: TMenuItem;
    pmToolbarMenu: TPopupMenu;
    FreeOTFEButtons1: TMenuItem;
    StandardButtons2: TMenuItem;
    AddressBar2: TMenuItem;
    actOverwriteFile: TAction;
    actOverwriteDir: TAction;
    N23: TMenuItem;
    Overwrite1: TMenuItem;
    Overwritefile1: TMenuItem;
    Overwritefolder1: TMenuItem;
    SDUOpenDialog_Overwrite: TSDUOpenDialog;
    actWebDAVStatus: TAction;
    Networkservicestatus1: TMenuItem;
    actChooseDetails: TAction;
    ChooseDetails1: TMenuItem;
    actMapNetworkDrive: TAction;
    actDisconnectNetworkDrive: TAction;
    tbbMapNetworkDrive: TToolButton;
    tbbDisconnectNetworkDrive: TToolButton;
    tbbSettings: TToolButton;
    OpenLUKScontainer1: TMenuItem;
    pmNew: TPopupMenu;
    Newhiddendmcryptfilecontainer1: TMenuItem;
    Newdmcryptfilecontainer1: TMenuItem;
    Newfile1: TMenuItem;
    New2: TMenuItem;
    NewHidden1: TMenuItem;
    NewLUKS1: TMenuItem;
    pmOpen: TPopupMenu;
    miMountfile: TMenuItem;
    miOpenLUKSBox: TMenuItem;
    Openhiddendmcryptfile1: TMenuItem;
    miOpenBoxforcedmcrypt: TMenuItem;
    OpenLUKScontainer2: TMenuItem;
    procedure pbGoClick(Sender: TObject);
    procedure edPathKeyPress(Sender: TObject; var Key: Char);
    procedure mnuTreeViewExpandClick(Sender: TObject);
    procedure mnuExploreViewClick(Sender: TObject);
    procedure pmViewPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SDFilesystemListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure mnuTreeViewCollapseClick(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actPlaintextMountFileExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actDismountExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actShowBootSectorExecute(Sender: TObject);
    procedure actCheckFilesystemExecute(Sender: TObject);
    procedure actListStyleExecute(Sender: TObject);
    procedure actCheckStatusBarExecute(Sender: TObject);
    procedure actCheckAddressBarExecute(Sender: TObject);
    procedure actCheckToolbarVolumeExecute(Sender: TObject);
    procedure actCheckToolbarExplorerExecute(Sender: TObject);
    procedure actStoreFileExecute(Sender: TObject);
    procedure actStoreDirExecute(Sender: TObject);
    procedure actCreateSubDirExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actItemPropertiesExecute(Sender: TObject);
    procedure actExtractExecute(Sender: TObject);
    procedure SDFilesystemTreeView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SDFilesystemListView1Enter(Sender: TObject);
    procedure SDFilesystemTreeView1Enter(Sender: TObject);
    procedure SDFilesystemListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure actUpDirExecute(Sender: TObject);
    procedure actNavigateBackExecute(Sender: TObject);
    procedure actNavigateForwardExecute(Sender: TObject);
    procedure tbbWithDropDownMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pmToolbarBackPopup(Sender: TObject);
    procedure pmToolbarForwardPopup(Sender: TObject);

    procedure actOptionsExecute(Sender: TObject);
    procedure edPathChange(Sender: TObject);
    procedure SDUDropFilesTreeViewItemsDrop(Sender: TObject; DropItems: TStringList; DropPoint: TPoint);
    procedure SDUDropFilesListViewItemsDrop(Sender: TObject; DropItems: TStringList; DropPoint: TPoint);
    procedure actPlaintextNewExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCheckExplorerBarFoldersExecute(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure SDFilesystemListView1Edited(Sender: TObject; Item: TListItem; var S: string);
    procedure SDFilesystemTreeView1Edited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure actInvertSelectionExecute(Sender: TObject);
    procedure actMoveToExecute(Sender: TObject);
    procedure actCopyToExecute(Sender: TObject);
    procedure SDFilesystemTreeView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure SDFilesystemListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure SDFilesystemBothViewStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure SDFilesystemBothViewEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actOverwriteFileExecute(Sender: TObject);
    procedure actOverwriteDirExecute(Sender: TObject);
    procedure SDFilesystemListView1DblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actWebDAVStatusExecute(Sender: TObject);
    procedure actChooseDetailsExecute(Sender: TObject);
    procedure actMapNetworkDriveExecute(Sender: TObject);
    procedure actDisconnectNetworkDriveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFreeOTFENewHiddenExecute(Sender: TObject);
    procedure actFreeOTFENewNotHiddenExecute(Sender: TObject);

  private
    fPartitionImage: TSDPartitionImage;
    fFilesystem:     TSDFilesystem_FAT;

    finFormShow:     Boolean;
    finRefreshing:   Boolean;
    procedure _PromptCreateFreeOTFEVolume(isHidden: Boolean);

  protected
    fLastFocussed:    TLastFocussed;

    fNavigateHistory: TStringList;
    fnavigateIdx:     Integer;

    fShredderObj:     TShredder;
    fOpProgressDlg:   TSDUWindowsProgressDialog;

    fWebDAVObj:       TExplorerWebDAV;
    fmappedDrive:     DriveLetterChar;

    function _ProcessCommandLine_Create(): eCmdLine_Exit; override;
    function _ProcessCommandLine_Mount(): eCmdLine_Exit; override;

    procedure _ReloadSettings(); override;

    procedure _RefreshMRUList(); override;
    procedure MRUListItemClicked(mruList: TSDUMRUList; idx: Integer); override;

    procedure _MountFile(mountAsSystem: TVolumeType; filename: string; ReadOnly, isHidden: Boolean;
      createVol: Boolean = False); overload; override;

    procedure _PostMountGUISetup(driveLetter: DriveLetterChar);

    procedure _WebDAVStartup();
    procedure _WebDAVShutdown();
    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: DriveLetterChar): DriveLetterChar;
    function MapNetworkDrive(const displayErrors: Boolean): Boolean;
    procedure DisconnectNetworkDrive();
    function CheckNetServiceStatusAndPrompt(): Boolean; overload;
    function CheckNetServiceStatusAndPrompt(const serviceName: string): Boolean; overload;

    // procedure RecaptionToolbarAndMenuIcons(); override;
    // procedure SetIconListsAndIndexes(); override;
    // procedure SetupToolbarFromSettings(); override;

    procedure MountPlaintextImage(filename: string; mountReadonly: Boolean);

    function ClipboardHasFiles(): Boolean;
    function ClipboardHasFExplFiles(): Boolean;
    procedure ClipboardClear();
    procedure ClipboardSetToSelected(Sender: TObject; opType: TFExplOperation);
    procedure ClipboardSetItems(opType: TFExplOperation; stlItems: TStringList);
    function ClipboardGetItems(out srcIsMountedFSNotLocalFS: Boolean; out opType: TFExplOperation;
      stlItems: TStringList): Boolean;

    // These functions carry out the extract/store operations...
    function CreateSubDir(path: WideString; newDirName: WideString): Boolean;
    function _StoreFile(path: WideString; fileToStore: string): Boolean;

    procedure ExpandRootTreeNode();

    function Mounted(): Boolean;

    procedure SetTitleCaption();

    procedure SetStatusMsg(); overload;
    procedure SetStatusMsg(msg: string); overload;
    function DefaultStatusMsg(): string;
    function SelectedItemsStatusMsg(): string;
    procedure SizeStatusBar();

    procedure SetDragCursor();

    procedure Dismount(); overload;
    procedure Dismount(driveLetter: DriveLetterChar); overload;

    procedure PromptForAndImportFile(importToPath: WideString);
    procedure PromptForAndImportDir(importToPath: WideString);

    function IsSenderTreeviewContextMenu(Sender: TObject): Boolean;

    procedure GetSelectedItems(Sender: TObject; targets: TStringList);
    function GetSelectedPath(Sender: TObject): string;

    procedure OnNavigationMenuItemClick(Sender: TObject);
    procedure NavigateToHistoryIdx(idx: Integer);

    function IsFilenameValid(filename: WideString): Boolean;

    procedure PostRefresh();
    procedure OnFreeOTFEExplorerRefreshMsg(var msg: TMessage); message WM_FREEOTFE_EXPLORER_REFRESH;

    function CheckDestinationIsntSource(opType: TFExplOperation; srcPathAndFilename: string; destPath: string): Boolean;
    function _DetermineTotalSize(isMountedFSNotLocalFS: Boolean; items: TStrings): ULONGLONG; overload;
    function _DetermineTotalSize(isMountedFSNotLocalFS: Boolean; Item: string): ULONGLONG;   overload;
    function _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS: Boolean; srcItem: string): ULONGLONG;
    function FSLoadContentsFromDisk(fromMountedFSNotLocalFS: Boolean; srcItem: string;
      srcDirContents: TSDDirItemList): Boolean;
    function GetLocalFileDetails(pathAndFilename: string; var Item: TSDDirItem_FAT): Boolean;

    // Move from mounted volume to mounted volume
    procedure MoveTo(items: TStringList); overload;
    procedure MoveTo(items: TStringList; destDir: string); overload;
    // Copy from mounted volume to mounted volume
    procedure CopyTo(items: TStringList); overload;
    procedure CopyTo(items: TStringList; destDir: string); overload;
    // Copy from HDD to mounted volume
    procedure Store(items: TStrings); overload;
    procedure Store(items: TStrings; destDir: string); overload;
    // Copy from mounted volume to HDD
    procedure Extract(items: TStrings); overload;
    procedure Extract(items: TStrings; destDir: string; destFilename: string); overload;
    function PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItems: TStrings;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string
      // When moving/copying a single file, use this as the destination filename
      // Set to '' to use the source filename's filename
      ): Boolean; overload;
    function ConfirmPerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItems: TStrings;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string): Boolean;

    procedure ProgressDlgSetup(opType: TFExplOperation);
    procedure ProgressDlgStart();
    procedure ProgressDlgSetTotal(total: ULONGLONG);
    procedure ProgressDlgTimerReset();
    procedure ProgressDlgSetLineOne(line: string);
    procedure ProgressDlgSetLineTwo(line: string);
    procedure ProgressDlgIncrement(increment: ULONGLONG);
    function ProgressDlgHasUserCancelled(): Boolean;
    procedure ProgressDlgStop();

    function _PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItem: string;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
      // When moving/copying a single file, use this as the destination filename
      // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean; var promptOverwriteDirs: Boolean)
      : Boolean; overload;
    function _PerformOperation_File(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItem: string;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
      // When moving/copying a single file, use this as the destination filename
      // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
      var promptOverwriteDirs: Boolean): Boolean;
    function _PerformOperation_File_Actual(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItem: string;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destItem: string;
      moveDeletionMethod: TMoveDeletionMethod): Boolean;
    function _PerformOperation_Dir(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItem: string;
      destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
      // When moving/copying a single file, use this as the destination filename
      // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
      var promptOverwriteDirs: Boolean): Boolean;
    function DeleteLocalFSItemUsingMethod(moveDeletionMethod: TMoveDeletionMethod; Item: string): Boolean;
    procedure OverwritePassStarted(Sender: TObject; itemName: string; passNumber: Integer; totalPasses: Integer);
    procedure OverwriteCheckForUserCancel(Sender: TObject; var userCancelled: Boolean);


    // Returns one of: TRUE / FALSE
    function PromptToReplace(filename: string; existingSize: ULONGLONG; existingDateTime: TDateTime; newSize: ULONGLONG;
      newDateTime: TDateTime): Boolean;
    // Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
    function PromptToReplaceYYANC(filename: string; existingSize: ULONGLONG; existingDateTime: TDateTime;
      newSize: ULONGLONG; newDateTime: TDateTime): Integer; overload;
    function PromptToReplaceYYANC(srcIsMountedFSNotLocalFS: Boolean; srcPathAndFilename: string;
      destIsMountedFSNotLocalFS: Boolean; destPathAndFilename: string): Integer; overload;


    function IsContolKeyDown(): Boolean;
    function ConfirmOperation(opType: TFExplOperation; srcItems: TStrings; destDir: string): Boolean;

    procedure OverwriteAllWebDAVCachedFiles();
    procedure _DoFullTests; override;
        procedure _EnableDisableControls(); override;

  public

    procedure WMUserPostShow(var msg: TWMEndSession); override;

    procedure InitApp(); override;

    procedure SetupControls();


    // Handle any command line options; returns exit code
    function ProcessCommandLineOpts(): eCmdLine_Exit;

  end;

  // e.g. Copy, Move, Delete
function OperationTitle(op: TFExplOperation): string;
// e.g. Copying, Moving, Deleting
function OperationVerbTitle(op: TFExplOperation): string;

var
  GfrmExplorerMain: TfrmExplorerMain;

implementation

{$R *.dfm}

uses
  // delphi & libs
  // Turn off useless hints about FileCtrl.pas being platform specific
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  {$WARN SYMBOL_PLATFORM ON}
  ActiveX, // Required for DROPEFFECT_COPY/DROPEFFECT_MOVE
  ClipBrd,
  IdException, Math,
  // Indy components exceptions
  IdSocketHandle,
  {$IFDEF UNICODE}
  AnsiStrings,
  {$ENDIF}
  DriverControl,
  // Just required for DEFAULT_HTTP_PORT
  WinSvc,
  // sdu & LibreCrypt utils
  lcConsts,
  SDUAboutDlg,
  SDUClipBrd,
  SDUGraphics,
  SDUHTTPServer, SDUi18n,
  SDUSysUtils,
  SDUWebDav,
  CheckFilesystem,
  SDFATBootSectorPropertiesDlg, LUKSTools, lcCommandLine,
  // LibreCrypt forms

  frmAbout,
  frmNewDirDlg,
  frmExplorerOptions,
  frmOverwritePrompt,
  frmDirProperties,
  frmFileProperties,
  frmMultipleProperties,
  frmVolumeProperties,
  frmSelectCopyOrMove,
  frmSelectDirectory,
  frmNewVolumeSize,
  frmCreateFreeOTFEVolume,
  frmKeyEntryFreeOTFE
  // frmWebDAVStatus { TODO 1 -otdk -cenhance : implement webdav }
    , frmKeyEntryPlainLinux, frmSelectVolumeType,frmVersionCheck;

{$IFDEF _NEVER_DEFINED}

// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
  {$ENDIF}

  // Exactly one of these *must* be defined
  // {$DEFINE WEBDAV_OVERWRITE_TSHREDDER 1}  // Use TShredder when overwriting local WebDAV cache
  {$DEFINE WEBDAV_OVERWRITE_SIMPLE 1}
  // Just overwrite local WebDAV cache files with zeros, then truncate to 0 bytes

resourcestring
  // Toolbar captions
  RS_TOOLBAR_CAPTION_BACK = 'Back';
  RS_TOOLBAR_CAPTION_FORWARD = 'Forward';
  RS_TOOLBAR_CAPTION_UP = 'Up';
  RS_TOOLBAR_CAPTION_MOVETO = 'Move To';
  RS_TOOLBAR_CAPTION_COPYTO = 'Copy To';
  RS_TOOLBAR_CAPTION_DELETE = 'Delete';
  RS_TOOLBAR_CAPTION_VIEWS = 'Views';
  RS_TOOLBAR_CAPTION_EXTRACT = 'Extract';
  RS_TOOLBAR_CAPTION_STORE = 'Store';
  RS_TOOLBAR_CAPTION_ITEMPROPERTIES = 'Properties';
  RS_TOOLBAR_CAPTION_FOLDERS = 'Folders';
  RS_TOOLBAR_CAPTION_MAP_DRIVE = 'Map Drive';
  RS_TOOLBAR_CAPTION_DISCONNECT = 'Disconnect';
  RS_TOOLBAR_MNU_CAPTION_STORE_FILE = 'Store file...';
  RS_TOOLBAR_MNU_CAPTION_STORE_DIR = 'Store folder...';

  // Toolbar hints
  RS_TOOLBAR_HINT_BACK = 'Back';
  RS_TOOLBAR_HINT_FORWARD = 'Forward';
  RS_TOOLBAR_HINT_UP = 'Up';
  RS_TOOLBAR_HINT_MOVETO = 'Move To';
  RS_TOOLBAR_HINT_COPYTO = 'Copy To';
  RS_TOOLBAR_HINT_DELETE = 'Delete';
  RS_TOOLBAR_HINT_VIEWS = 'Views';
  RS_TOOLBAR_HINT_EXTRACT = 'Extract selected files/folders from the open container';
  RS_TOOLBAR_HINT_STORE = 'Store a file/folder in the open container';
  RS_TOOLBAR_HINT_ITEMPROPERTIES = 'Properties';
  RS_TOOLBAR_HINT_EXPLORERBARFOLDERS = 'Shows the Folders bar.';
  RS_TOOLBAR_HINT_MAP_DRIVE = 'Map Drive';
  RS_TOOLBAR_HINT_DISCONNECT = 'Disconnect';

  RS_TOOLBAR_MNU_HINT_STORE_FILE = 'Store file';
  RS_TOOLBAR_MNU_HINT_STORE_DIR = 'Store folder';

  RS_COULD_MOUNT_BUT_NOT_PARTITION = 'Container could be mounted, but not mounted as a partition image?!';
  RS_FILESYSTEM_NOT_FAT121632 =
    'The filesystem used by this container could not be recognised as either FAT12, FAT16 or FAT32.' + SDUCRLF + SDUCRLF +
    'Although LibreCrypt supports all filesystems, LibreCrypt Explorer only supports those listed above.';

  RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE = 'Cannot %s %s: Cannot find the specified file.' + SDUCRLF + SDUCRLF +
    'Make sure you specify the correct path and file name.';

  RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION = 'Cannot %s %s: Cannot find the specified destination.' + SDUCRLF +
    SDUCRLF + 'Make sure you specify the correct path and file name.';

  RS_UNABLE_TO_CREATE_FOLDER = 'Unable to create folder ''%s''';
  RS_UNABLE_TO_DELETE_EXISTING_FILE = 'Unable to delete file ''%s''';

  RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE = 'Please ensure that there is enough free space available';

  // Open/Save file filters...
  FILE_FILTER_FLT_PLAINTEXTIMAGES = 'Image files (*.img)|*.img|All files|*.*';
  FILE_FILTER_DFLT_PLAINTEXTIMAGES = 'img';

const
  STATUSBAR_PANEL_STATUS         = 0;
  STATUSBAR_PANEL_SELECTEDITEMS  = 1;
  STATUSBAR_PANEL_LOCATION       = 2;

  // These icon indexes must match those in ImageList_StatusBar
  IMGLIST_IDX_FREEOTFE           = 0;

  // Max number of menu items to show in the Back/Forward button dropdown menus
  MAX_BACKFORWARD_NAV_MENUITEMS  = 10;
  // Max number of items in the history
  MAX_BACKFORWARD_NAV_TOTALITEMS = 50;


  // Drag 'n' drop cursors...
  // (Custom cursors are used to get the "+" on the arrow cursor when copying
  USER_CURSORS_START                     = 1;
  crFreeOTFEDragMove                     = TCursor(USER_CURSORS_START);
  crFreeOTFEDragCopy                     = TCursor(USER_CURSORS_START + 1);

  // These *must* match the resource names in FreeOTFEExplorerCursors.res
  CURSOR_DRAG_COPY                       = 'CURSOR_DRAG_COPY';
  CURSOR_DRAG_MOVE                       = 'CURSOR_DRAG_MOVE';

  // String for this instance of FreeOTFE Explorer's clipboard
  CFSTR_FEXPL_SESSION_BASE               = 'LibreCryptExplorer:%s';

  ERROR_WORKSTATION_DRIVER_NOT_INSTALLED = $00000836;
  // "The workstation driver is not installed."


var
  CF_FEXPL_SESSION_DATA:    Word;
  CFSTR_FEXPL_SESSION_DATA: string;


function OperationTitle(op: TFExplOperation): string;
begin
  Result := LoadResString(OperationTitlePtr[op]);
end;

function OperationVerbTitle(op: TFExplOperation): string;
begin
  Result := LoadResString(OperationVerbTitlePtr[op]);
end;


// This procedure is called after OnCreate, and immediatly before the
// Application.Run() is called
procedure TfrmExplorerMain.InitApp();
begin
  inherited;

  // We call _EnableDisableControls(...) at this point, to display the
  // toolbar/statusbar as needed
  _EnableDisableControls();

  _ReloadSettings();
  // SetupToolbarFromSettings(...) can change the window's width if large
  // icons are being used
  // We recenter the window here so it looks right
  // But! Only if no main window layout has been stored! If it has, we set it
  // to default
  // SEE CALL TO SDUSetFormLayout(...) FOR DETAILS
  // if (GetExplorerSettings().OptMainWindowLayout = '') then begin
  // self.Position := poScreenCenter;
  // end else begin
  // self.Position := poDefault;
  // end;

  if not(_ActivateFreeOTFEComponent(True)) then begin
    SDUMessageDlg(_('The LibreCrypt DLL (FreeOTFE.dll) could not be found on this computer' + SDUCRLF + SDUCRLF +
      'Please check your installation'), mtError);
  end;

  _EnableDisableControls();
  _SetupPKCS11(False);

  SDUMultimediaKeys1.Active                               := True;

  // Setup for status bar icon...
  StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Style := psOwnerDraw;
  // Needs to be at least 20 pixels high to allow the icon to be shown properly
  StatusBar_Status.Height                                 := max(StatusBar_Status.Height, 20);
  StatusBar_Hint.Height                                   := StatusBar_Status.Height;
  Application.ProcessMessages;

end;

// Reload settings, setting up any components as needed
procedure TfrmExplorerMain._ReloadSettings();

  procedure SortToolbar(processToolbar: TToolBar; displayToolbar: Boolean);
  // var
  // toolbarWidth: Integer;
  // i:            Integer;
  begin
    processToolbar.Height       := processToolbar.Images.Height + TOOLBAR_ICON_BORDER;
    processToolbar.ButtonHeight := processToolbar.Images.Height;
    processToolbar.ButtonWidth  := processToolbar.Images.Width;

    // if displayToolbar then begin
    // // Turning captions on can cause the buttons to wrap if the window isn't big
    // // enough.
    // // This looks pretty poor, so resize the window if it's too small
    // toolbarWidth := 0;
    // for i := 0 to (processToolbar.ButtonCount - 1) do begin
    // toolbarWidth := toolbarWidth + processToolbar.Buttons[i].Width;
    // end;
    // if (toolbarWidth > self.Width) then begin
    // self.Width           := toolbarWidth;
    // processToolbar.Width := self.Width;
    // end;
    // // Adjusting the width to the sum of the toolbar buttons doens't make it wide
    // // enough to prevent wrapping (presumably due to window borders); nudge the
    // // size until it does
    // // (Crude, but effective)
    // while (processToolbar.RowCount > 1) do begin
    // self.Width           := self.Width + 10;
    // processToolbar.Width := self.Width;
    // end;
    // end;

  end;

var
  tmpToolBarVolumeVisible:   Boolean;
  tmpToolBarExplorerVisible: Boolean;

begin
  inherited;

  SDUClearPanel(pnlTopSpacing);
  SDUClearPanel(pnlAddressBar);
  SDUClearPanel(pnlExplorer);

  SetTitleCaption();

  // Off man out...
  ToolBarVolume.Visible                    := GetExplorerSettings().ShowToolbar;
  ToolbarExplorer.Visible                  := GetExplorerSettings().ShowExplorerToolBar;

  // Treeview...
  SDFilesystemTreeView1.ShowHiddenItems    := GetExplorerSettings().ShowHiddenItems;
  // Listview...
  SDFilesystemListView1.ShowHiddenItems    := GetExplorerSettings().ShowHiddenItems;
  SDFilesystemListView1.HideKnownFileExtns := GetExplorerSettings().hideKnownFileExtns;

  fShredderObj.FileDirUseInt               := True;
  fShredderObj.OnStartingFileOverwritePass := OverwritePassStarted;
  fShredderObj.OnCheckForUserCancel        := OverwriteCheckForUserCancel;
  fShredderObj.IntMethod                   := GetExplorerSettings().OverwriteMethod;
  fShredderObj.IntPasses                   := GetExplorerSettings().overwritePasses;

  if (fFilesystem <> nil) then
    if (fFilesystem is TSDFilesystem_FAT) then
      fFilesystem.PreserveTimeDateStamps := GetExplorerSettings().keepTimestampsOnStoreExtract;


  if GetExplorerSettings().ShowLargeToolbar then begin
    ToolBarVolume.Images := ilToolbarIcons_Large;

  end else begin
    ToolBarVolume.Images := ilToolbarIcons_Small;

  end;

  if GetExplorerSettings().ShowLargerExplorerToolbar then
    ToolbarExplorer.Images := ilToolbarIcons_Large
  else
    ToolbarExplorer.Images := ilToolbarIcons_Small;

  // Visible/invisible are set in _EnableDisableControls()

  // Toolbar captions...
  if GetExplorerSettings().ShowLargeToolbar then begin
    ToolBarVolume.ShowCaptions := GetExplorerSettings().ShowToolbarCaptions;
  end else begin
    ToolBarVolume.ShowCaptions := False;
  end;

  // Toolbar captions...
  if GetExplorerSettings().ShowLargerExplorerToolbar then begin
    ToolbarExplorer.ShowCaptions := GetExplorerSettings().ShowExplorerToolbarCaptions;
  end else begin
    ToolbarExplorer.ShowCaptions := False;
  end;


  tmpToolBarExplorerVisible := ToolbarExplorer.Visible;
  ToolbarExplorer.Visible   := False;
  SortToolbar(ToolBarVolume, tmpToolBarExplorerVisible);

  tmpToolBarVolumeVisible   := ToolBarVolume.Visible;
  ToolBarVolume.Visible     := False;
  SortToolbar(ToolbarExplorer, tmpToolBarVolumeVisible);

  ToolBarVolume.Visible     := tmpToolBarVolumeVisible;
  ToolbarExplorer.Visible   := tmpToolBarExplorerVisible;


  // We DO NOT restart WebDAV functionality here
  //
  // If we did, then it would:
  //
  // *) Overwrite any WebDAV cache before shutting it down (potentially a time consuming task)
  // *) Remount the network drive, possibly under a different drive letter
  // *) Prompt the user the user with the (potentially new) drive letter
  //
  // Because this is all pretty ugly - for the time being, we'll just inform
  // the user that their changes require their volume to be remounted (done by
  // the options dialog)

  // Restart WebDAV functionality
  // WARNING! If a transfer was in mid-progress, this may well probably
  // nobble it, though the volume contents should be consistent.
  _WebDAVShutdown();
  // WebDAVStartup();

end;

procedure TfrmExplorerMain._MountFile(mountAsSystem: TVolumeType; filename: string; ReadOnly, isHidden: Boolean;
  createVol: Boolean = False);
var
  mountedAs: DriveLetterChar;
  mountedOK: TMountResult;
begin
  // Sanity check
  if (filename = '') then begin
    SDUMessageDlg(_('Please specify a container to be opened'), mtError);
    exit;
  end;

  Dismount();

  AddToMRUList(filename);


  if (mountAsSystem = vtFreeOTFE) then begin
    mountedOK   := MountFreeOTFE(filename, mountedAs, readonly);
  end else begin
    if (mountAsSystem = vtPlainLinux) then begin
      mountedOK := frmKeyEntryPlainLinux.MountPlainLinux(filename, mountedAs, readonly, '', nil, 0, createVol, isHidden);
    end else begin
      if (mountAsSystem = vtLUKS) then begin
        assert(not isHidden);
        if not IsLUKSVolume(filename) then begin
          SDUMessageDlg(Format(_('%s is not a LUKS container.'), [filename]), mtError);
          mountedOK := morFail;
        end else begin
          mountedOK := LUKSTools.MountLUKS(filename, mountedAs, readonly, nil, '', False,
            LINUX_KEYFILE_DEFAULT_NEWLINE);
        end;
      end else begin
        mountedOK := frmSelectVolumeType.Mount(filename, mountedAs, readonly);
      end;
    end;
  end;


  if mountedOK <> morOK then begin
    if mountedOK = morFail then
      SDUMessageDlg(_('Unable to open Container.') + SDUCRLF + SDUCRLF +
        _('Please check your keyphrase and settings, and try again.'), mtError);
  end else begin
    _PostMountGUISetup(mountedAs);
    // WebDAVStartup();
  end;
end;

procedure TfrmExplorerMain._RefreshMRUList();
begin
  inherited;
  // Refresh menuitems...
  GetSettings().mruList.RemoveMenuItems(mmMain);
  GetSettings().mruList.InsertAfter(miDismountMain);
end;

procedure TfrmExplorerMain.MRUListItemClicked(mruList: TSDUMRUList; idx: Integer);
begin
  // lplp - this isnt' right; should cater for plaintext images, and linux volumes being in the MRU list
  // MountFilesDetectLUKS(mruList.Items[idx], FALSE, Settings.OptDragDropFileType);
  _MountFilesDetectLUKS(mruList.items[idx], False, vtUnknown); // ftPrompt ?
end;

// Configure up the GUI, assuming the filename supplied has been mounted under
// the "drive letter" supplied
procedure TfrmExplorerMain._PostMountGUISetup(driveLetter: DriveLetterChar);
var
  volFilename: string;
begin
  volFilename                                   := GetFreeOTFEDLL().GetVolFileForDrive(driveLetter);

  fPartitionImage                               := TPartitionImageDLL.Create();
  TPartitionImageDLL(fPartitionImage).filename  := volFilename;
  TPartitionImageDLL(fPartitionImage).mountedAs := driveLetter;
  fPartitionImage.Mounted                       := True;

  if not(fPartitionImage.Mounted) then begin
    fPartitionImage.Free();
    fPartitionImage := nil;
    SDUMessageDlg(RS_COULD_MOUNT_BUT_NOT_PARTITION, mtError);
  end;

  if (fPartitionImage <> nil) then begin
    fFilesystem                        := TSDFilesystem_FAT.Create();
    fFilesystem.PreserveTimeDateStamps := GetExplorerSettings().keepTimestampsOnStoreExtract;

    fFilesystem.PartitionImage         := fPartitionImage;
    try
      fFilesystem.Mounted              := True;
    except
      on E: EFileSystemNotRecognised do begin
        SDUMessageDlg(RS_FILESYSTEM_NOT_FAT121632, mtError);
        Dismount();
      end;
    end;
  end;

  if (fFilesystem <> nil) then begin
    if fFilesystem.Mounted then begin
      ExpandRootTreeNode();
      SetupControls();
      SetTitleCaption();
    end;
  end;

  _EnableDisableControls();
end;

{ TODO 1 -otdk -cenhance : implement webdav }

procedure TfrmExplorerMain._WebDAVStartup();
const
  // Local range is 127.0.0.1/8 (i.e. 127.[0-255].[0-255].[0-255]) - but we
  // only bother trying the first 255 addresses; there's not much point in
  // checking more than that.
  LOCALHOST_RANGE_FIRST_255 = '127.0.0.%d';
var
  serverPrettyID: string;
  boundSktHandle: TIdSocketHandle;
  ipAddrStr:      string;
  i:              Integer;
  portMsg:        string;
begin
  // Shutdown any running...
  _WebDAVShutdown();

  if (GetExplorerSettings().EnableWebDAVServer and (fFilesystem <> nil)) then begin

    if { SDUOSVistaOrLater() } False then begin
      SDUMessageDlg(RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7, mtInformation);
    end else begin
      fWebDAVObj.fLogAccess.filename := GetExplorerSettings().WebDavLogAccessFile;
      if (fWebDAVObj.fLogAccess.filename <> '') then begin
        SDUMessageDlg(Format(_('Volume access logging is ENABLED.' + SDUCRLF + SDUCRLF +
          'All accesses to the network server will be written to:' + SDUCRLF + SDUCRLF + '%s'),
          [fWebDAVObj.fLogAccess.filename]), mtWarning);
      end;

      fWebDAVObj.fLogDebug.filename := GetExplorerSettings().WebDavLogDebugFile;
      if (fWebDAVObj.fLogDebug.filename <> '') then begin
        SDUMessageDlg(Format(_('Volume access DEBUG logging is ENABLED.' + SDUCRLF + SDUCRLF +
          'All accesses to the network server will be written to:' + SDUCRLF + SDUCRLF + '%s'),
          [fWebDAVObj.fLogDebug.filename]), mtWarning);
      end;

      // Pretty server ID for inclusion in any HTTP headers returned...
      serverPrettyID             := Application.Title + '/' + SDUGetVersionInfoString(ParamStr(0));
      if (APP_BETA_BUILD > 0) then
        serverPrettyID           := serverPrettyID + '_BETA_' + IntToStr(APP_BETA_BUILD);

      fWebDAVObj.fServerSoftware := serverPrettyID;
      // Port our server runs on...
      fWebDAVObj.fDefaultPort    := GetExplorerSettings().WebDAVPort;

      // We generate a random share name in order to prevent caching from "trying
      // it on"
      fWebDAVObj.ShareName     := SDURandomHexString(4);
      if (GetExplorerSettings().WebDavShareName <> '') then begin
        fWebDAVObj.ShareName   := GetExplorerSettings().WebDavShareName + '_';
      end;
      fWebDAVObj.ShareName     := fWebDAVObj.ShareName + SDURandomHexString(4);

      fWebDAVObj.fFilesystem   := fFilesystem;

      // In case previously used...
      fWebDAVObj.ReturnGarbage := False;
      fWebDAVObj.ClearDownRequestedItemsList();


      // Loop through to try and get a IP address we can listen on...
      // Note: Start from 1, not 0 and run through to 255 and *not* 256 here!
      for i         := 1 to 255 do begin
        try
          ipAddrStr := Format(LOCALHOST_RANGE_FIRST_255, [i]);

          // Ditch any previous bindings...
          fWebDAVObj.Bindings.Clear();
          // Only bind to the localhost IP; don't allow connections from other PCs...
          boundSktHandle              := fWebDAVObj.Bindings.Add();
          // boundSktHandle.IP := IP_ADDR_ALL_IP_ADDRESSES;
          boundSktHandle.IP           := ipAddrStr;
          boundSktHandle.Port         := fWebDAVObj.fDefaultPort;

          // Don't allow connections from remote...
          fWebDAVObj.fOnlyPermitLocal := True;
          // For security, only allow connections from localhost

          fWebDAVObj.Activate;

          // Found one! Ditch the loop...
          break;

        except
          // Swallow error
          on EIdCouldNotBindSocket do begin
          end;
        end;

      end;

      if not(fWebDAVObj.IsActive) then begin
        if (fWebDAVObj.fDefaultPort = DEFAULT_HTTP_PORT) then begin
          portMsg := Format(_('This is most likely caused by a WWW server (on port %d) running on this computer.'),
            [fWebDAVObj.fDefaultPort]);
        end else begin
          portMsg := Format(_('You probably have some other service running on the required port (port %d).'),
            [fWebDAVObj.fDefaultPort]);
        end;

        SDUMessageDlg(_('Unable to start drive mapping server.') + SDUCRLF + SDUCRLF + portMsg, mtWarning);
      end else begin
        // Success starting WebDAV server - try to map to local drive...
        if not(MapNetworkDrive(False)) then begin
          // Check and start services, if needed
          CheckNetServiceStatusAndPrompt();

          // Try again...
          if not(MapNetworkDrive(True)) then
            // Um... Oh well! Shut it all down.
            _WebDAVShutdown();
        end;
      end;

    end;
  end;
  _EnableDisableControls();

end;

procedure TfrmExplorerMain._WebDAVShutdown();
begin
  DisconnectNetworkDrive();

  fWebDAVObj.DeActivate;
  // Sanity - this shouldn't be needed
  fWebDAVObj.fFilesystem := nil;

  _EnableDisableControls();
end;

// ----------------------------------------------------------------------------
// !! IMPORTANT !!
// !! IMPORTANT !!
// !! IMPORTANT !!
// If this function is changed, make sure:
// TOTFEFreeOTFE.GetNextDriveLetter(...)
// TfrmExplorerMain.GetNextDriveLetter(...)
// are kept in sync
//
// Identify the next drive letter to mount a volume as
// i.e. If userDriveLetter is not set to #0, then return either that letter, or
// the next free drive letter.
// If userDriveLetter is set to #0, then use requiredDriveLetter instead
// of userDriveLetter
// userDriveLetter - The drive letter the user has specifically requested
// requiredDriveLetter - The drive letter the system would normally use
// Returns: Drive letter to mount as, or #0 on error
function TfrmExplorerMain.GetNextDriveLetter(userDriveLetter, requiredDriveLetter: DriveLetterChar): DriveLetterChar;
var
  freeDriveLetters:  DriveLetterString;
  searchDriveLetter: DriveLetterChar;
begin
  Result              := #0;

  searchDriveLetter   := userDriveLetter;
  if (searchDriveLetter = #0) then begin
    searchDriveLetter := requiredDriveLetter;
  end;

  // If still #0, just get the next one after C:
  if (searchDriveLetter = #0) then begin
    searchDriveLetter := 'C';
  end;


  freeDriveLetters  := DriveLetterString(uppercase(SDUGetUnusedDriveLetters()));
  searchDriveLetter := upcase(searchDriveLetter);

  // Delete drive letters from the free drive letters, until we hit one which
  // appears after the one we've been requested - or we run out of free drive
  // letters
  while (freeDriveLetters <> '') and (freeDriveLetters[1] < searchDriveLetter) do begin
    Delete(freeDriveLetters, 1, 1);
  end;

  if (freeDriveLetters <> '') then begin
    Result := freeDriveLetters[1];
  end;

end;


function TfrmExplorerMain.MapNetworkDrive(const displayErrors: Boolean): Boolean;
var
  networkShare:   string;
  useDriveLetter: DriveLetterChar;
  msg:            string;
  lastErrorNo:    DWORD;
  lastErrorMsg:   string;
  comment:        string;
begin
  Result         := False;

  useDriveLetter := GetNextDriveLetter(#0, GetSettings().DefaultDriveChar);

  if (useDriveLetter = #0) then begin
    if displayErrors then begin
      SDUMessageDlg(_('Unable to assign a new drive letter; please confirm you have drive letters free!'), mtError);
    end;

    Result := False;
    exit;
  end;


  // networkShare := '\\'+fWebDAVObj.Bindings[0].IP+'\'+fWebDAVObj.ShareName;
  networkShare := 'http://' + fWebDAVObj.Bindings[0].IP + '/' + fWebDAVObj.ShareName + '/';

  // Cast to prevent compiler error
  if SDUMapNetworkDrive(networkShare, Char(useDriveLetter)) then begin
    fmappedDrive := useDriveLetter;
    Result       := True;

    if GetSettings().InformIfMountedOK then begin
      msg        := Format(_('Your container has been mounted as drive: %s'), [useDriveLetter + ':']);
      SDUMessageDlg(msg, mtInformation);
    end;

    if GetSettings().ExploreAfterMount then begin
      _ExploreDrive(useDriveLetter);
    end;

    _AutoRunExecute(arPostMount, useDriveLetter, False);
  end else begin
    if displayErrors then begin
      lastErrorNo  := GetLastError();
      lastErrorMsg := SysErrorMessage(lastErrorNo) + ' (0x' + SDUIntToHex(lastErrorNo, 8) + ')';
      comment      := '';
      if ((lastErrorNo = ERROR_NO_NET_OR_BAD_PATH) or (lastErrorNo = ERROR_WORKSTATION_DRIVER_NOT_INSTALLED)) then begin
        comment    := _('Please ensure that the WebClient service is running');
      end;

      SDUMessageDlg(Format(_('Unable to map container to %s:' + SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF + '%s'),
        [useDriveLetter, lastErrorMsg, comment]), mtWarning);
    end;
  end;

end;

// Returns TRUE if the service was running, or successfully started.
// Returns FALSE if the service wasn't running, or it's status couldn't be
// determined

function TfrmExplorerMain.CheckNetServiceStatusAndPrompt(): Boolean;
var
  servicesOK: Boolean;
begin
  servicesOK   := CheckNetServiceStatusAndPrompt(SERVICE_MRXDAV);

  if servicesOK then begin
    servicesOK := CheckNetServiceStatusAndPrompt(SERVICE_WEBCLIENT);
  end;

  Result       := servicesOK;
end;


// Returns TRUE if the service was running, or successfully started.
// Returns FALSE if the service wasn't running, or it's status couldn't be
// determined
function TfrmExplorerMain.CheckNetServiceStatusAndPrompt(const serviceName: string): Boolean;
const
  SERVICE_START_PAUSE = 3000; // 3 seconds
var
  sc:           TSDUServiceControl;
  serviceState: Cardinal;
begin
  Result := False;

  try
    sc   := TSDUServiceControl.Create();
  except
    // Problem getting service control manager - ignore the error; Result
    // already set to FALSE
    sc := nil;
  end;

  if (sc <> nil) then begin
    try
      // We'll process all errors...
      sc.Silent := True;

      if sc.GetServiceState(serviceName, serviceState) then begin
        if (serviceState = SERVICE_RUNNING) then begin
          Result := True;
        end else begin
          if (SDUMessageDlg(Format(_('The "%s" service does not appear to be running.' + SDUCRLF + SDUCRLF +
            'This service is required in order to map the mounted container to a drive letter.' + SDUCRLF + SDUCRLF +
            'Do you wish to start this service now?'), [serviceName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            if sc.StartStopService(serviceName, True) then begin
              // Delay to allow service to start up.
              // Not sure if strictly needed, but if a drive is mapped
              // immediatly afterwards, the drive mapping can fail (e.g. with
              // "No network provider accepted the given path"), although it'll
              // succeed if the user subsequently tries to mount again
              SDUPause(SERVICE_START_PAUSE);

              Result := True;
            end else begin
              SDUMessageDlg(Format(_('Unable to start the "%s" service.'), [serviceName]), mtError);
            end;

          end;
        end;
      end;

    finally
      sc.Free;
    end;

  end;

end;


procedure TfrmExplorerMain.DisconnectNetworkDrive();
begin
  if (fmappedDrive <> #0) then begin
    _AutoRunExecute(arPreDismount, fmappedDrive, False);

    if GetExplorerSettings().OverwriteWebDAVCacheOnDismount then begin
      OverwriteAllWebDAVCachedFiles();
    end;

    // SDUDisconnectNetworkDrive(Char(FMappedDrive), Force);
    // SDUDisconnectNetworkDrive(Char(FMappedDrive), TRUE);

    _AutoRunExecute(arPostDismount, fmappedDrive, False);

    fmappedDrive := #0;
  end;

end;

procedure TfrmExplorerMain.SetupControls();
var
  mountedWritable: Boolean;
begin
  fNavigateHistory.Clear();
  fnavigateIdx                     := -1;

  SDFilesystemListView1.Filesystem := fFilesystem;
  SDFilesystemTreeView1.Filesystem := fFilesystem;

  // Allow user to rename files...
  mountedWritable                  := (Mounted() and not(fFilesystem.ReadOnly));

  SDFilesystemTreeView1.ReadOnly   := not(mountedWritable);
  SDFilesystemListView1.ReadOnly   := not(mountedWritable);

  SDFilesystemListView1.Initialize();
  SDFilesystemTreeView1.Initialize();

  SDFilesystemTreeView1.RefreshNodes();

  // Expand out the root node
  if (SDFilesystemTreeView1.GoToPath(PATH_SEPARATOR, False) <> nil) then
    SDFilesystemTreeView1.Selected.Expand(False);

  SDFilesystemTreeView1.SetFocus();

  SetStatusMsg();

  _EnableDisableControls();
end;

procedure TfrmExplorerMain.pbGoClick(Sender: TObject);
begin
  edPath.Text := trim(edPath.Text);
  if (SDFilesystemTreeView1.GoToPath(edPath.Text, False) = nil) then begin
    SDUMessageDlg(Format(_('Path not found:' + SDUCRLF + SDUCRLF + '%s'), [edPath.Text]), mtError);
  end;

end;


procedure TfrmExplorerMain.edPathChange(Sender: TObject);
begin
  inherited;
  pbGo.Hint     := Format(_('Go to ''%s'''), [trim(edPath.Text)]);
  pbGo.ShowHint := True;
end;

procedure TfrmExplorerMain.edPathKeyPress(Sender: TObject; var Key: Char);
begin
  // <ENTER> is the same as pressing the "Go" button
  if (Key = #13) then begin
    pbGoClick(nil);
  end;

end;

procedure TfrmExplorerMain.mnuTreeViewCollapseClick(Sender: TObject);
var
  popupNode: TTreeNode;
begin
  // pmTreeView.Tag set to the node rightclicked on by
  // SDFilesystemTreeView1ContextPopup(...)
  popupNode := TTreeNode(pmTreeView.Tag);
  popupNode.Collapse(False);

  // DO NOT USE THIS - THE USER MIGHT HAVE RIGHTCLICKED ON A DIFFERENT NODE TO
  // THE ONE SELECTED
  // SDFilesystemTreeView1.Selected.Collapse(FALSE);
end;

procedure TfrmExplorerMain.mnuTreeViewExpandClick(Sender: TObject);
var
  popupNode: TTreeNode;
begin
  // pmTreeView.Tag set to the node rightclicked on by
  // SDFilesystemTreeView1ContextPopup(...)
  popupNode := TTreeNode(pmTreeView.Tag);
  popupNode.Expand(False);

  // DO NOT USE THIS - THE USER MIGHT HAVE RIGHTCLICKED ON A DIFFERENT NODE TO
  // THE ONE SELECTED
  // SDFilesystemTreeView1.Selected.Expand(FALSE);
end;

procedure TfrmExplorerMain.mnuExploreViewClick(Sender: TObject);
begin
  if (SDFilesystemListView1.SelCount > 0) then begin
    SDFilesystemTreeView1.GoToPath(IncludeTrailingPathDelimiter(SDFilesystemListView1.path) +
      SDFilesystemListView1.DirItem[SDFilesystemListView1.SelectedIdx].filename, True);
  end;
end;

procedure TfrmExplorerMain.pmToolbarBackPopup(Sender: TObject);
var
  tmpMenuItem: TMenuItem;
  i:           Integer;
begin
  pmToolbarBack.items.Clear();
  for i                 := (fnavigateIdx - 1) downto max(0, (fnavigateIdx - MAX_BACKFORWARD_NAV_MENUITEMS)) do begin
    tmpMenuItem         := TMenuItem.Create(nil);
    tmpMenuItem.Caption := fNavigateHistory[i];
    tmpMenuItem.OnClick := OnNavigationMenuItemClick;
    tmpMenuItem.Tag     := i;
    pmToolbarBack.items.Add(tmpMenuItem);
  end;

end;

procedure TfrmExplorerMain.pmToolbarForwardPopup(Sender: TObject);
var
  tmpMenuItem: TMenuItem;
  i:           Integer;
begin
  pmToolbarForward.items.Clear();
  for i := (fnavigateIdx + 1) to min((fNavigateHistory.Count - 1), (fnavigateIdx + MAX_BACKFORWARD_NAV_MENUITEMS)) do
  begin
    tmpMenuItem := TMenuItem.Create(nil);
    tmpMenuItem.Caption := fNavigateHistory[i];
    tmpMenuItem.OnClick := OnNavigationMenuItemClick;
    tmpMenuItem.Tag     := i;
    pmToolbarForward.items.Add(tmpMenuItem);
  end;

end;

procedure TfrmExplorerMain.OnNavigationMenuItemClick(Sender: TObject);
var
  targetIdx: Integer;
begin
  targetIdx := TMenuItem(Sender).Tag;

  // Sanity check
  if ((fNavigateHistory.Count > 0) and (targetIdx >= 0) and (targetIdx <= (fNavigateHistory.Count - 1))) then begin
    NavigateToHistoryIdx(targetIdx);
  end;

end;

procedure TfrmExplorerMain.pmViewPopup(Sender: TObject);
begin
  _EnableDisableControls();
end;

procedure TfrmExplorerMain._EnableDisableControls();
var
  mountedWritable: Boolean;
  selectedTargets: TStringList;
begin
  inherited;

  // Inherited controls...
  actDismount.Enabled        := Mounted();

  mountedWritable            := False;
  if (fFilesystem <> nil) then
    mountedWritable          := (Mounted() and not(fFilesystem.ReadOnly));

  // Controls only enabled when mounted...
  actNavigateBack.Enabled    := Mounted() and (fnavigateIdx > 0);
  actNavigateForward.Enabled := (Mounted() and (fnavigateIdx < (fNavigateHistory.Count - 1)));
  SDUEnableControl(edPath, Mounted());
  SDUEnableControl(pbGo, (Mounted() and (trim(edPath.Text) <> '')));

  // Don't disable the ListView - then the user can still play with the column
  // layout/sizes
  // SDUEnableControl(SDFilesystemListView1, Mounted());
  // Don't disable the TreeView - that'll prevent the splitter from
  // resizing(?!!!!)
  // SDUEnableControl(SDFilesystemTreeView1, Mounted());

  actSelectAll.Enabled       := Mounted();
  actInvertSelection.Enabled := Mounted();
  actShowBootSector.Enabled  := Mounted();
  actCheckFilesystem.Enabled := Mounted();
  actUpDir.Enabled           := (Mounted() and (SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected) <>
    PATH_SEPARATOR));

  // Controls only enabled when mounted for read/write...
  SDUEnableControl(tbbStore, mountedWritable);
  mnuMainStore.Enabled        := mountedWritable;
  actStoreFile.Enabled        := mountedWritable;
  actStoreDir.Enabled         := mountedWritable;
  actCut.Enabled              := mountedWritable;
  actCopy.Enabled             := mountedWritable;
  actPaste.Enabled            := mountedWritable and ClipboardHasFiles();

  actCreateSubDir.Enabled     := mountedWritable;
  actDelete.Enabled           := mountedWritable;

  // Only allow drag and drop to store files/dirs if mounted for read/write...
  SDUDropFilesTreeView.Active := mountedWritable;
  SDUDropFilesListView.Active := mountedWritable;


  mnuTreeViewExpand.Visible   := ((SDFilesystemTreeView1.SelectionCount = 1) and
    not(SDFilesystemTreeView1.Selected.Expanded));
  mnuTreeViewCollapse.Visible := ((SDFilesystemTreeView1.SelectionCount = 1) and
    SDFilesystemTreeView1.Selected.Expanded);

  mnuExploreView.Enabled      := ((SDFilesystemListView1.SelCount = 1) and SDFilesystemListView1.DirItem
    [SDFilesystemListView1.SelectedIdx].IsDirectory);

  mnuViewRename.Enabled       := (not(SDFilesystemListView1.ReadOnly) and (SDFilesystemListView1.SelCount = 1) and
    mountedWritable);

  mnuTreeViewRename.Enabled := (not(SDFilesystemTreeView1.ReadOnly) and (SDFilesystemTreeView1.SelectionCount = 1) and
    mountedWritable);


  selectedTargets := TStringList.Create();
  try
    GetSelectedItems(nil, selectedTargets);

    actExtract.Enabled        := (Mounted() and (selectedTargets.Count > 0));

    actMoveTo.Enabled         := (Mounted() and (selectedTargets.Count > 0));

    actCopyTo.Enabled         := (Mounted() and (selectedTargets.Count > 0));

    actItemProperties.Enabled := actExtract.Enabled;
  finally
    selectedTargets.Free();
  end;


  // Remove buttons which are only really used for debug purposes...
  actMapNetworkDrive.Visible        := False;
  actDisconnectNetworkDrive.Visible := False;

  actMapNetworkDrive.Enabled := (Mounted() and (fmappedDrive = #0) and GetExplorerSettings().EnableWebDAVServer);

  actDisconnectNetworkDrive.Enabled := (Mounted() and (fmappedDrive <> #0) and GetExplorerSettings()
    .EnableWebDAVServer);

  // IMPORTANT!
  // Because the treeview's menuitems have their actionitem set to nil in
  // FormCreate(...), these must be enabled/disabled to match the appropriate
  // actionitem
  // SDUEnableControl(mnuTreeViewExtract, actExtract.enabled);  // Don't mirror this one - if we're displaying the context menu, we can always extract
  mnuTreeViewStoreFile.Enabled      := actStoreFile.Enabled;
  mnuTreeViewStoreDir.Enabled       := actStoreDir.Enabled;
  mnuTreeViewExtract.Enabled        := actExtract.Enabled;
  mnuTreeViewCut.Enabled            := actCut.Enabled;
  mnuTreeViewCopy.Enabled           := actCopy.Enabled;
  mnuTreeViewPaste.Enabled          := actPaste.Enabled;
  mnuTreeViewCreateSubDir.Enabled   := actCreateSubDir.Enabled;
  mnuTreeViewDelete.Enabled         := actDelete.Enabled;
  mnuTreeViewRename.Enabled         := actRename.Enabled;
  mnuTreeViewItemProperties.Enabled := actItemProperties.Enabled;


  // Note: Toolbars setup by inherited method, apart from visible/invisible;
  // setup on startup
  // ToolBarVolume.Visible := Settings.OptShowToolbarVolume;
  // ToolBarExplorer.Visible := Settings.OptShowToolbarExplorer;

  pnlTopSpacing.Visible := not(pnlAddressBar.Visible);

  // Set .Top on aligned "alTop" components so they are shown in the correct order
  // We call LockWindowUpdate(...) to prevent *real* bad flicker!
  LockWindowUpdate(self.handle);
  try
    pnlTopSpacing.Top   := 0;
    ToolbarExplorer.Top := 0;
    ToolBarVolume.Top   := 0;
  finally
    LockWindowUpdate(0);
  end;

  // Update layout controls to reflect their state...
  actCheckToolbarVolume.Checked      := ToolBarVolume.Visible;
  actCheckToolbarExplorer.Checked    := ToolbarExplorer.Visible;
  actCheckAddressBar.Checked         := pnlAddressBar.Visible;
  actCheckExplorerBarFolders.Checked := SDFilesystemTreeView1.Visible;
  actCheckStatusBar.Checked          := (StatusBar_Status.Visible or StatusBar_Hint.Visible);

  // Set menu checkmarks...
  case SDFilesystemListView1.ViewStyle of
    vsReport: begin
        actListStyleDetails.Checked := True;
      end;

    vsList: begin
        actListStyleList.Checked := True;
      end;

    vsIcon: begin
        actListStyleIcons.Checked := True;
      end;

    vsSmallIcon: begin
        actListStyleSmallIcons.Checked := True;
      end;
  end;

end;

procedure TfrmExplorerMain.SetTitleCaption();
var
  volFilename: string;
begin
  volFilename     := '';
  if Mounted() then begin
    if (fPartitionImage is TPartitionImageDLL) then begin
      volFilename := TPartitionImageDLL(fPartitionImage).filename;
    end
    else if (fPartitionImage is TSDPartitionImage_File) then begin
      volFilename := TSDPartitionImage_File(fPartitionImage).filename;
    end;
  end;

  if (volFilename = '') then begin
    self.Caption := Application.Title;
  end else begin
    self.Caption := Application.Title + ' - ' + ExtractFilename(volFilename);
  end;

end;

procedure TfrmExplorerMain.SetStatusMsg();
begin
  SetStatusMsg('');
end;

procedure TfrmExplorerMain.SetStatusMsg(msg: string);
begin
  if (msg = '') then begin
    msg := DefaultStatusMsg();

    // If setting to default status message, update the selected items panel
    if (fFilesystem = nil) then begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Text := '';
    end else begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Text := SelectedItemsStatusMsg();
    end;

    StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text        := '';
    if (fFilesystem <> nil) then begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text      := RS_ENCRYPTED_VOLUME;
      if (fPartitionImage is TSDPartitionImage_File) then begin
        StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text    := RS_VOLUME_PLAINTEXT;
      end;
    end;
  end;

  SizeStatusBar();
  StatusBar_Status.Panels[STATUSBAR_PANEL_STATUS].Text := msg;

  if not(finFormShow) then begin
    Application.ProcessMessages(); // Required in order to get display to
    // update when called in a loop
    // (e.g. when processing multiple files, etc)
  end;

end;


procedure TfrmExplorerMain.SizeStatusBar();
begin
  // These values approximate the sizes of the panels Windows Explorer uses
  // (under Windows XP, with no themes, at any rate)
  StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Width := 80;
  StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Width      := 165;

  StatusBar_Status.Panels[STATUSBAR_PANEL_STATUS].Width        := self.Width -
    (StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Width + StatusBar_Status.Panels
    [STATUSBAR_PANEL_LOCATION].Width);
end;

procedure TfrmExplorerMain.actAboutExecute(Sender: TObject);
var
  dlg: TfrmAbout;
begin
  dlg := TfrmAbout.Create(self);
  try
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;


procedure TfrmExplorerMain.actRefreshExecute(Sender: TObject);
var
  lastPath: string;
begin
  inherited;

  if Mounted() then begin
    SetStatusMsg(_('Refreshing...'));

    lastPath      := SDFilesystemListView1.path;
    if (lastPath = '') then begin
      lastPath    := PATH_SEPARATOR;
    end;

    finRefreshing := True;
    try
      SDFilesystemTreeView1.RefreshNodes();
    finally
      finRefreshing := False;
    end;

    // Always expand root node to make sure if user's just created the first
    // subdir, or stored the first subdir, it shows up in the treeview
    ExpandRootTreeNode();

    SDFilesystemTreeView1.GoToPath(lastPath, True);

    SetStatusMsg('');
  end;

  _EnableDisableControls();
end;

procedure TfrmExplorerMain.ExpandRootTreeNode();
var
  rootNode: TTreeNode;
begin
  if Mounted() then begin
    // Make sure the root node is always expanded; otherwise if the user
    // creates a new container, them immediatly creates a subnode, the subnode
    // doesn't get shown in the treeview
    rootNode            := SDFilesystemTreeView1.items.GetFirstNode();
    if (rootNode <> nil) then begin
      rootNode.Expanded := True;
    end;

  end;

end;

procedure TfrmExplorerMain.actRenameExecute(Sender: TObject);
var
  popupNode:          TTreeNode;
  useContextMenuNode: Boolean;
begin
  useContextMenuNode := IsSenderTreeviewContextMenu(Sender);

  if useContextMenuNode then begin
    popupNode        := TTreeNode(pmTreeView.Tag);
    popupNode.EditText;
  end
  else if (fLastFocussed = lfTreeView) then begin
    SDFilesystemTreeView1.Selected.EditText;
  end
  else if (fLastFocussed = lfListView) then begin
    if (SDFilesystemListView1.SelCount = 1) then begin
      SDFilesystemListView1.Selected.EditCaption;
    end;
  end;

end;


procedure TfrmExplorerMain.actSelectAllExecute(Sender: TObject);
begin
  inherited;
  SDFilesystemListView1.SelectAll();
  SDFilesystemListView1.SetFocus();
end;

procedure TfrmExplorerMain.actInvertSelectionExecute(Sender: TObject);
begin
  inherited;
  SDFilesystemListView1.InvertSelection();
  SDFilesystemListView1.SetFocus();
end;

procedure TfrmExplorerMain.actShowBootSectorExecute(Sender: TObject);
var
  dlg: TSDFATBootSectorPropertiesDialog;
begin
  dlg              := TSDFATBootSectorPropertiesDialog.Create(self);
  try
    dlg.Filesystem := TSDFilesystem_FAT(fFilesystem);
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

end;

procedure TfrmExplorerMain.actStoreDirExecute(Sender: TObject);
begin
  inherited;
  PromptForAndImportDir(GetSelectedPath(Sender));

end;

procedure TfrmExplorerMain.actStoreFileExecute(Sender: TObject);
begin
  inherited;
  PromptForAndImportFile(GetSelectedPath(Sender));

end;

procedure TfrmExplorerMain.actCheckStatusBarExecute(Sender: TObject);
begin
  inherited;
  StatusBar_Status.Visible := actCheckStatusBar.Checked;
  StatusBar_Hint.Visible   := False;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckAddressBarExecute(Sender: TObject);
begin
  inherited;
  pnlAddressBar.Visible := actCheckAddressBar.Checked;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckExplorerBarFoldersExecute(Sender: TObject);
begin
  inherited;
  // Important - set splitter bar visible *first* - otherwise, it goes to the
  // *left* of the treeview, where it doesn't do much good!
  Splitter1.Visible             := actCheckExplorerBarFolders.Checked;
  SDFilesystemTreeView1.Visible := actCheckExplorerBarFolders.Checked;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckToolbarExplorerExecute(Sender: TObject);
begin
  inherited;
  ToolbarExplorer.Visible := actCheckToolbarExplorer.Checked;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckToolbarVolumeExecute(Sender: TObject);
begin
  inherited;
  ToolBarVolume.Visible := actCheckToolbarVolume.Checked;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.actChooseDetailsExecute(Sender: TObject);
begin
  inherited;
  SDFilesystemListView1.ChooseColumns();

end;

procedure TfrmExplorerMain.actCreateSubDirExecute(Sender: TObject);
var
  dlg:        TfrmNewDirDlg;
  prevCursor: TCursor;
  ok:         Boolean;
begin
  inherited;

  dlg                 := TfrmNewDirDlg.Create(self);
  try
    if (dlg.ShowModal() = mrOk) then begin
      prevCursor      := Screen.Cursor;
      Screen.Cursor   := crHourglass;
      try
        ok            := CreateSubDir(GetSelectedPath(Sender), dlg.DirName);
      finally
        Screen.Cursor := prevCursor;
      end;

      if ok then begin
        // Don't bother telling user - they should know what they're doing!
        // SDUMessageDlg(_('Folder created successfully'), mtInformation);
      end else begin
        SDUMessageDlg(Format(RS_UNABLE_TO_CREATE_FOLDER + SDUCRLF + SDUCRLF + RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE,
          [dlg.DirName]), mtError);
      end;

      actRefreshExecute(Sender);
    end;

  finally
    dlg.Free();
  end;

end;

procedure TfrmExplorerMain.Dismount();
begin
  Dismount(#0);
end;

// Specify #0 as driveLetter to dismount all
procedure TfrmExplorerMain.Dismount(driveLetter: DriveLetterChar);
begin
  _WebDAVShutdown();

  SDFilesystemListView1.Filesystem := nil;
  SDFilesystemTreeView1.Filesystem := nil;

  if (fFilesystem <> nil) then begin
    fFilesystem.Mounted            := False;
    fFilesystem.Free();
    fFilesystem                    := nil;
  end;

  if (fPartitionImage <> nil) then begin
    fPartitionImage.Mounted := False;
    fPartitionImage.Free();
    fPartitionImage         := nil;
  end;

  if (driveLetter = #0) then begin
    GetFreeOTFEDLL().DismountAll();
  end else begin
    GetFreeOTFEDLL().Dismount(driveLetter);
  end;

  // If *we* put anything on the clipboard, clear it
  if ClipboardHasFExplFiles() then begin
    ClipboardClear();
  end;

  // Blank top control...
  edPath.Text := '';

  SetTitleCaption();
  SetStatusMsg('');
  _EnableDisableControls();

end;

procedure TfrmExplorerMain.MountPlaintextImage(filename: string; mountReadonly: Boolean);
begin
  Dismount();

  fPartitionImage                                  := TSDPartitionImage_File.Create();
  TSDPartitionImage_File(fPartitionImage).filename := filename;
  fPartitionImage.Mounted                          := True;

  if not(fPartitionImage.Mounted) then begin
    fPartitionImage.Free();
    fPartitionImage := nil;
  end;

  if (fPartitionImage <> nil) then begin
    fFilesystem                        := TSDFilesystem_FAT.Create();
    fFilesystem.PreserveTimeDateStamps := GetExplorerSettings().keepTimestampsOnStoreExtract;

    fFilesystem.PartitionImage         := fPartitionImage;
    try
      fFilesystem.Mounted              := True;
    except
      on E: EFileSystemNotRecognised do begin
        SDUMessageDlg(RS_FILESYSTEM_NOT_FAT121632, mtError);
        Dismount();
      end;
    end;
  end;

  if (fFilesystem <> nil) then begin
    if fFilesystem.Mounted then begin
      fFilesystem.ReadOnly := mountReadonly;

      SetupControls();
      SetTitleCaption();

      // WebDAVStartup();
    end;
  end;

end;

procedure TfrmExplorerMain.actCheckFilesystemExecute(Sender: TObject);
begin
  CheckFATFilesystem(fFilesystem);
end;

procedure TfrmExplorerMain.actDeleteExecute(Sender: TObject);
var
  userConfirmed: Boolean;
  promptMsg:     WideString;
  Item:          TSDDirItem_FAT;
  i:             Integer;
  selItems:      TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);

    // Sanity check...
    for i := 0 to (selItems.Count - 1) do begin
      if (selItems[i] = PATH_SEPARATOR) then begin
        // WHAT?!!!
        // The user wants to delete the *root* *directory*?!
        // I don't *think* so!!
        exit; // Bail out
      end;
    end;

    userConfirmed := False;

    if (selItems.Count = 0) then begin
      // Do nothing; userConfirmed already set to FALSE
    end else begin
      if (selItems.Count = 1) then begin
        Item := TSDDirItem_FAT.Create();
        try
          fFilesystem.GetItem(selItems[0], Item);

          if Item.IsFile then begin
            promptMsg := Format(_('Are you sure you want to delete ''%s''?'), [Item.filename]);
          end else begin
            promptMsg := Format(_('Are you sure you want to remove the folder ''%s'' and all its contents?'),
              [Item.filename]);
          end;
        finally
          Item.Free();
        end;

      end else begin
        promptMsg   := Format(_('Are you sure you want to delete these %d items?'), [selItems.Count]);
      end;

      userConfirmed := SDUConfirmYN(promptMsg);
    end;

    if userConfirmed then begin
      PerformOperation(cmDelete, True, selItems, True, '', '');

      // MS Windows Explorer doesn't display confirmation prompt, so neither
      // do we
      // if allOK then
      // begin
      // SDUMessageDlg(_('Deleted successfully'), mtInformation);
      // end;

      // Refresh anyway, in case *some* but not all files were deleted
      actRefreshExecute(Sender);
    end;

  finally
    selItems.Free()
  end;

end;

procedure TfrmExplorerMain.actDismountExecute(Sender: TObject);
begin
  inherited;
  Dismount();
end;

procedure TfrmExplorerMain.actExtractExecute(Sender: TObject);
var
  selItems: TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);
    Extract(selItems);

  finally
    selItems.Free();
  end;

end;

procedure TfrmExplorerMain._PromptCreateFreeOTFEVolume(isHidden: Boolean);
var
  prevMounted: DriveLetterString;
  newMounted:  DriveLetterString;
  i:           Integer;
  res : TMountResult;
begin
  inherited;

  prevMounted := GetFreeOTFEDLL().DrivesMounted;
  res := CreateFreeOTFEVolume(isHidden);
  if res<>morOK then begin
    if res = morFail then begin
      SDUMessageDlg(_('LibreCrypt container could not be created'), mtError);
    end;
  end else begin
    newMounted := GetFreeOTFEDLL().DrivesMounted;

    // If the "drive letters" mounted have changed, the new volume was
    // automatically mounted; setup for this
    if (newMounted <> prevMounted) then begin
      for i := 1 to length(prevMounted) do
        Dismount(prevMounted[i]);

      if (newMounted <> '') then begin
        _PostMountGUISetup(newMounted[1]);
        // WebDAVStartup();
      end;

      _EnableDisableControls();
    end;

    SDUMessageDlg(_('LibreCrypt container created successfully.'), mtInformation);
  end;

end;

procedure TfrmExplorerMain.actFreeOTFENewHiddenExecute(Sender: TObject);
begin
  inherited;
  _PromptCreateFreeOTFEVolume(True);
end;

procedure TfrmExplorerMain.actFreeOTFENewNotHiddenExecute(Sender: TObject);
begin
  inherited;
  _PromptCreateFreeOTFEVolume(False);
end;

procedure TfrmExplorerMain.actListStyleExecute(Sender: TObject);
begin
  inherited;
  SDFilesystemListView1.ViewStyle := TViewStyle(TMenuItem(Sender).Tag);
  _EnableDisableControls();

end;

procedure TfrmExplorerMain.actPlaintextMountFileExecute(Sender: TObject);
begin
  inherited;

  FreeOTFEGUISetupOpenSaveDialog(SDUOpenDialog_MountPlaintextImage);

  SDUOpenDialog_MountPlaintextImage.Filter     := FILE_FILTER_FLT_PLAINTEXTIMAGES;
  SDUOpenDialog_MountPlaintextImage.DefaultExt := FILE_FILTER_DFLT_PLAINTEXTIMAGES;

  SDUOpenDialog_MountPlaintextImage.Options    := SDUOpenDialog_MountPlaintextImage.Options - [ofHideReadOnly];

  if SDUOpenDialog_MountPlaintextImage.Execute() then begin
    MountPlaintextImage(SDUOpenDialog_MountPlaintextImage.filename,
      (ofReadOnly in SDUOpenDialog_MountPlaintextImage.Options));
  end;
end;

procedure TfrmExplorerMain.actPlaintextNewExecute(Sender: TObject);
var
  volSizeDlg:        TfrmNewVolumeSize;
  mr:                Integer;
  allOK:             Boolean;
  userCancel:        Boolean;
  filename:          string;
  tmpPartitionImage: TSDPartitionImage_File;
  tmpFilesystem:     TSDFilesystem_FAT;
begin
  allOK   := True;

  if allOK then begin
    allOK := SDUWarnYN(_('This function will create an UNENCRYPTED disk image.' + SDUCRLF + SDUCRLF +
      'If you would like to create an ENCRYPTED disk image, please click "No" below, and then select "File | New..."' +
      SDUCRLF + SDUCRLF + 'Are you sure you with to create an UNENCRYPTED disk image?'));
  end;

  // Create the plaintext image file
  if allOK then begin
    volSizeDlg              := TfrmNewVolumeSize.Create(nil);
    try
      volSizeDlg.Filter     := FILE_FILTER_FLT_PLAINTEXTIMAGES;
      volSizeDlg.DefaultExt := FILE_FILTER_DFLT_PLAINTEXTIMAGES;

      mr                    := volSizeDlg.ShowModal;
      if (mr <> mrOk) then begin
        allOK               := False;
      end else begin
        filename            := volSizeDlg.filename;
        if not(SDUCreateLargeFile(filename, volSizeDlg.VolumeSize, True, userCancel)) then begin
          if not(userCancel) then begin
            SDUMessageDlg(_('An error occured while trying to create your LibreCrypt file'), mtError, [mbOK], 0);
          end;

          allOK := False;
        end;
      end;

    finally
      volSizeDlg.Free();
    end;

  end;

  // Format the plaintext image file
  if allOK then begin
    tmpPartitionImage            := TSDPartitionImage_File.Create();
    try
      tmpPartitionImage.filename := filename;
      tmpPartitionImage.Mounted  := True;

      if not(tmpPartitionImage.Mounted) then begin
        SDUMessageDlg(RS_COULD_MOUNT_BUT_NOT_PARTITION, mtError);
        allOK                          := False;
      end else begin
        tmpFilesystem                  := TSDFilesystem_FAT.Create();
        try
          tmpFilesystem.PartitionImage := tmpPartitionImage;
          tmpFilesystem.FormatFS();
        finally
          tmpFilesystem.Free();
        end;

        tmpPartitionImage.Mounted := False;
      end;

    finally
      tmpPartitionImage.Free();
    end;

  end;

  // Mount the plaintext image file
  if allOK then begin
    MountPlaintextImage(filename, False);
  end;

end;


function TfrmExplorerMain.ConfirmOperation(opType: TFExplOperation; srcItems: TStrings; destDir: string): Boolean;
var
  confirmMsg: WideString;
begin
  if (srcItems.Count = 1) then begin
    confirmMsg := Format(_('Are you sure you wish to %s ''%s'' to:' + SDUCRLF + SDUCRLF + '  %s'),
      [OperationTitle(opType), ExtractFilename(srcItems[0]), destDir]);
  end else begin
    confirmMsg := Format(_('Are you sure you wish to %s the %n items dropped to:' + SDUCRLF + SDUCRLF + '%s'),
      [OperationTitle(opType), srcItems.Count, destDir]);
  end;

  Result := SDUConfirmYN(confirmMsg);

end;


procedure TfrmExplorerMain.CopyTo(items: TStringList);
var
  destDir: string;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtCopy, items);
  if (destDir <> '') then begin
    CopyTo(items, destDir);
  end;

end;

procedure TfrmExplorerMain.CopyTo(items: TStringList; destDir: string);
begin
  PerformOperation(cmCopy, True, items, True, destDir, '');
end;

procedure TfrmExplorerMain.actCopyToExecute(Sender: TObject);
var
  selItems: TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);
    CopyTo(selItems);
  finally
    selItems.Free();
  end;

end;

procedure TfrmExplorerMain.MoveTo(items: TStringList);
var
  destDir: string;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtMove, items);
  if (destDir <> '') then begin
    MoveTo(items, destDir);
  end;

end;

procedure TfrmExplorerMain.MoveTo(items: TStringList; destDir: string);
begin
  PerformOperation(cmMove, True, items, True, destDir, '');
end;

procedure TfrmExplorerMain.actMapNetworkDriveExecute(Sender: TObject);
begin
  inherited;
  // WebDAVStartup();
end;

procedure TfrmExplorerMain.actDisconnectNetworkDriveExecute(Sender: TObject);
begin
  inherited;
  _WebDAVShutdown();
end;

procedure TfrmExplorerMain.actMoveToExecute(Sender: TObject);
var
  selItems: TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);
    MoveTo(selItems);
  finally
    selItems.Free();
  end;

end;

procedure TfrmExplorerMain.actItemPropertiesExecute(Sender: TObject);
var
  dlgVolume:    TfrmVolumeProperties;
  dlgFile:      TfrmFileProperties;
  dlgDirectory: TfrmDirProperties;
  dlgMultiple:  TfrmMultipleProperties;
  selItems:     TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);

    if (selItems.Count = 0) then begin
      // Do nothing
    end
    else if (selItems.Count > 1) then begin
      // Show properties as single directory...
      dlgMultiple               := TfrmMultipleProperties.Create(self);
      try
        dlgMultiple.fFilesystem := fFilesystem;
        dlgMultiple.fMultipleItems.Assign(selItems);
        dlgMultiple.fParentDir  := GetSelectedPath(Sender);
        dlgMultiple.ShowModal();
      finally
        dlgMultiple.Free();
      end;

    end
    else if (selItems.Count = 1) then begin
      if (selItems[0] = PATH_SEPARATOR) then begin
        // Show properties as single file...
        dlgVolume               := TfrmVolumeProperties.Create(self);
        try
          dlgVolume.fFilesystem := fFilesystem;
          dlgVolume.ShowModal();
        finally
          dlgVolume.Free();
        end;

      end
      else if fFilesystem.FileExists(selItems[0]) then begin
        // Show properties as single file...
        dlgFile                    := TfrmFileProperties.Create(self);
        try
          dlgFile.fFilesystem      := fFilesystem;
          dlgFile.fPathAndFilename := selItems[0];
          dlgFile.ShowModal();
        finally
          dlgFile.Free();
        end;

      end else begin
        // Show properties as single directory...
        dlgDirectory                    := TfrmDirProperties.Create(self);
        try
          dlgDirectory.fFilesystem      := fFilesystem;
          dlgDirectory.fPathAndFilename := selItems[0];
          dlgDirectory.ShowModal();
        finally
          dlgDirectory.Free();
        end;

      end;
    end;

  finally
    selItems.Free();
  end;

end;

procedure TfrmExplorerMain.actUpDirExecute(Sender: TObject);
begin
  inherited;

  SDFilesystemTreeView1.Selected := SDFilesystemTreeView1.Selected.Parent;
end;

procedure TfrmExplorerMain.actWebDAVStatusExecute(Sender: TObject);
// var
// dlg: TfrmWebDAVStatus;
begin
  inherited;
  {
    dlg:= TfrmWebDAVStatus.Create(self);
    try
    dlg.fWebDAVObj := fWebDAVObj;
    dlg.MappedDrive := FMappedDrive;

    dlg.ShowModal();
    finally
    dlg.Free();
    end;
  }
end;

procedure TfrmExplorerMain.FormShow(Sender: TObject);
begin
  // Setup so *we* draw this panel, as we want the little image displayed
  // StatusBar.Panels[STATUSBAR_PANEL_LOCATION].Style := psOwnerDraw;

  inherited;

  finFormShow := True;
  try
    SetStatusMsg();

    SDUCenterControl(lblFolder, ccVertical);
    SDUCenterControl(edPath, ccVertical);
    SDUCenterControl(pbGo, ccVertical);

    SDFilesystemTreeView1.Constraints.MinWidth := 60;
    // Like MS Windows Explorer


    ToolBarVolume.Visible         := GetExplorerSettings().ShowToolbar;
    ToolbarExplorer.Visible       := GetExplorerSettings().ShowExplorerToolBar;
    pnlAddressBar.Visible         := GetExplorerSettings().ShowAddressBar;
    // Important: Must turn these next two on/off in this order!
    Splitter1.Visible             := GetExplorerSettings().ShowExplorerFolders ;
    SDFilesystemTreeView1.Visible := GetExplorerSettings().ShowExplorerFolders ;
    StatusBar_Status.Visible      := GetSettings().ShowStatusbar;
    StatusBar_Hint.Visible        := False;

    if (GetExplorerSettings().TreeViewWidth > 0) then
      SDFilesystemTreeView1.Width := GetExplorerSettings().TreeViewWidth;

    SDFilesystemListView1.Layout  := GetExplorerSettings().ListViewLayout;

    _EnableDisableControls();
  finally
    finFormShow := False;
  end;

//  _DoFullTests;
end;

function TfrmExplorerMain.DefaultStatusMsg(): string;
var
  fsFreeSpace:     ULONGLONG;
  // fsSize: ULONGLONG;
  dispMsg:         string;
  selectedItem:    TSDDirItem;
  size:            string;
  fileType:        string;
  date:            string;
  sizeKB:          Integer;
  itemsInSelected: Integer;
begin
  // Fallback msg; anything other than ''
  dispMsg             := _('Ready.');

  if not(Mounted()) then begin
    dispMsg           := _('Ready to open Container...');
  end else begin
    if (SDFilesystemListView1.SelCount <= 0) then begin
      fsFreeSpace     := fFilesystem.FreeSpace;
      // fsSize := fFilesystem.Size;

      itemsInSelected := SDFilesystemListView1.items.Count;
      if SDFilesystemListView1.ShowParentDir then begin
        Dec(itemsInSelected);
      end;

      dispMsg := Format(_('%d objects (Disk free space: %s)'), [itemsInSelected,
        // SDUFormatAsBytesUnits(fsFreeSpace),
        // MS Explorer only displays to 1 decimal place
        SDUFormatUnits(fsFreeSpace, SDUUnitsStorageToTextArr(), UNITS_BYTES_MULTIPLIER, 1)]);


      {
        dispMsg := Format(
        _('Free Space: %1; Total size: %2'),
        [
        //                                  SDUFormatAsBytesUnits(fsFreeSpace),
        // MS Explorer only displays to 1 decimal place
        SDUFormatUnits(
        fsFreeSpace,
        SDUUnitsStorageToTextArr(),
        UNITS_BYTES_MULTIPLIER,
        1
        ),
        //                                  SDUFormatAsBytesUnits(fsSize)
        // MS Explorer only displays to 1 decimal place
        SDUFormatUnits(
        fsSize,
        SDUUnitsStorageToTextArr(),
        UNITS_BYTES_MULTIPLIER,
        1
        )
        ]
        );
      }
    end
    else if (SDFilesystemListView1.SelCount = 1) then begin
      selectedItem := SDFilesystemListView1.DirItem[SDFilesystemListView1.SelectedIdx];
      if selectedItem.IsFile then begin
        // Always display size in KB
        sizeKB     := (selectedItem.size div BYTES_IN_KILOBYTE);
        // Round up, like MS Explorer does...
        if ((selectedItem.size mod BYTES_IN_KILOBYTE) > 0) then begin
          sizeKB   := sizeKB + 1;
        end;
        // Format with thousands separator and KB units
        size       := SDUIntToStrThousands(sizeKB) + ' ' + UNITS_STORAGE_KB;

        fileType   := SDUGetFileType_Description(selectedItem.filename);
        date       := DateTimeToStr(TimeStampToDateTime(selectedItem.TimestampLastModified));

        dispMsg    := Format(_('Type: %s Date Modified: %s Size: %s'), [fileType, date, size]);
      end else begin
        dispMsg    := Format(_('%d object selected'), [SDFilesystemListView1.SelCount]);
      end;

    end else begin
      dispMsg := SDUPluralMsg(SDFilesystemListView1.SelCount,
        [Format(_('%d object selected'), [SDFilesystemListView1.SelCount]), Format(_('%d objects selected'),
        [SDFilesystemListView1.SelCount])]);
    end;
  end;

  Result := dispMsg;
end;

function TfrmExplorerMain.SelectedItemsStatusMsg(): string;
var
  currItem:         TSDDirItem;
  totalSize:        ULONGLONG;
  sumAllItems:      Boolean;
  i:                Integer;
  onlyDirsSelected: Boolean;
begin
  Result := '';

  // If the only things which are *selected* are directories, return blank
  // If one or more items are selected, return their sum total size
  // If nothing is selected, return the sum total size of all items

  sumAllItems          := (SDFilesystemListView1.SelCount <= 0);

  totalSize            := 0;
  onlyDirsSelected     := True;
  for i                := 0 to (SDFilesystemListView1.items.Count - 1) do begin
    currItem           := SDFilesystemListView1.DirItem[i];

    if (SDFilesystemListView1.items[i].Selected and currItem.IsFile) then begin
      onlyDirsSelected := False;
    end;

    if ((SDFilesystemListView1.items[i].Selected or sumAllItems) and currItem.IsFile) then begin
      totalSize := totalSize + currItem.size;
    end;

  end;

  if ((SDFilesystemListView1.SelCount > 0) and onlyDirsSelected) then begin
    // Do nothing - already set to ''
  end else begin
    Result := SDUFormatUnits(totalSize, SDUUnitsStorageToTextArr(), UNITS_BYTES_MULTIPLIER, 2);
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  SetStatusMsg();
end;

procedure TfrmExplorerMain.SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
begin
  if finRefreshing then begin
    exit;
  end;

  edPath.Text := SDFilesystemTreeView1.PathToNode(Node);

  if (SDFilesystemTreeView1.Selected <> nil) then begin
    // Delete all after the current point
    for i := (fNavigateHistory.Count - 1) downto (fnavigateIdx + 1) do begin
      fNavigateHistory.Delete(i);
    end;

    fNavigateHistory.Append(SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected));

    while (fNavigateHistory.Count > MAX_BACKFORWARD_NAV_TOTALITEMS) do begin
      fNavigateHistory.Delete(0);
    end;

    fnavigateIdx := (fNavigateHistory.Count - 1);
  end;

  // This is a bit klunky!
  // Because the popup context menu has shortcut keys, when the treeview is
  // selected and one of these shortcut keys is pressed, the relevant context
  // menuitem's event will fire - and it'll look like the context menuitem was
  // selected by the user.
  // As a result, pmTreeView.Tag won't have been setup to reflect the selected
  // treeview node.
  // Part of the solution to prevent this from happening is set the .Tag of the
  // context menu everytime a different node is selected
  // The other part of the solution is to detect when the treeview's context
  // menu closes, and reset pmTreeView.Tag back to the currently selected node
  //
  // !!!!!!!!!!!!!
  // WARNING:
  // This still has a bug in it; if the user displays the context menu for a
  // node *other* than the one currently selected, cancels the popup menu by
  // pressing <ESCAPE>, and then uses a shortcut key to do something - the
  // node used for the popup context menu will be acted on - not the currently
  // selected node!
  // xxx - lplp - TODO: FIX THIS!
  // !!!!!!!!!!!!!
  pmTreeView.Tag := Integer(SDFilesystemTreeView1.Selected);

  _EnableDisableControls();
  SetStatusMsg();
end;

procedure TfrmExplorerMain.actNavigateBackExecute(Sender: TObject);
begin
  if (fnavigateIdx > 0) then begin
    NavigateToHistoryIdx(fnavigateIdx - 1);
  end;

end;

procedure TfrmExplorerMain.actNavigateForwardExecute(Sender: TObject);
begin
  if (fnavigateIdx < (fNavigateHistory.Count - 1)) then begin
    NavigateToHistoryIdx(fnavigateIdx + 1);
  end;

end;

procedure TfrmExplorerMain.actOptionsExecute(Sender: TObject);
var
  dlg: TfrmExplorerOptions;
begin
  inherited;

  // Odd man out...
  GetExplorerSettings().ShowToolbar   := ToolBarVolume.Visible;
  GetExplorerSettings().ShowExplorerToolBar := ToolbarExplorer.Visible;

  dlg                                          := TfrmExplorerOptions.Create(self);
  try
    if (dlg.ShowModal() = mrOk) then begin
      _ReloadSettings();
      actRefreshExecute(nil);
      // Note: actRefreshExecute calls _EnableDisableControls(...) anyway
      // _EnableDisableControls();
    end;

  finally
    dlg.Free();
  end;

end;

procedure TfrmExplorerMain.actOverwriteFileExecute(Sender: TObject);
var
  overwriteItem: string;
begin
  inherited;

  SDUOpenDialog_Overwrite.Options := SDUOpenDialog_Overwrite.Options + [ofHideReadOnly,
  // i.e. Set to TRUE to remove the readonly checkbox
  ofPathMustExist, ofFileMustExist
  // ofAllowMultiSelect
    ];
  SDUOpenDialog_Overwrite.Options := SDUOpenDialog_Overwrite.Options + [ofDontAddToRecent];
  if SDUOpenDialog_Overwrite.Execute then begin
    overwriteItem                 := SDUOpenDialog_Overwrite.filename;

    if SDUWarnYN(Format(_('This will wipe:' + SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
      'Are you sure you want to do this?'), [overwriteItem])) then begin
      if (fShredderObj.DestroyFileOrDir(overwriteItem, False, False, False) = srError) then begin
        SDUMessageDlg(_('Wipe failed'), mtError);
      end;
    end;
  end;

end;

procedure TfrmExplorerMain.actOverwriteDirExecute(Sender: TObject);
var
  overwriteItem: string;
begin
  inherited;

  if SDUSelectDirectory(self.handle, _('Folder to wipe:'), '', overwriteItem) then begin
    if SDUWarnYN(Format(_('This will wipe:' + SDUCRLF + SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
      'and everything contained within it.' + SDUCRLF + SDUCRLF + 'Are you sure you want to do this?'), [overwriteItem]))
    then begin
      if (fShredderObj.DestroyFileOrDir(overwriteItem, False, False, False) = srError) then begin
        SDUMessageDlg(_('Wipe failed'), mtError);
      end;
    end;
  end;

end;

procedure TfrmExplorerMain.NavigateToHistoryIdx(idx: Integer);
var
  prev: TStringList;
begin
  prev := TStringList.Create();
  try
    prev.Assign(fNavigateHistory);

    SDFilesystemTreeView1.GoToPath(fNavigateHistory[idx], False);

    fNavigateHistory.Assign(prev);
    fnavigateIdx := idx;

  finally
    prev.Free();
  end;

  // _EnableDisableControls(...) call required here as FNavigateHistory
  // was updated
  _EnableDisableControls();

end;

procedure TfrmExplorerMain.SDFilesystemTreeView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  hitTestTreeNode: TTreeNode;
begin
  inherited;

  hitTestTreeNode               := TTreeView(Sender).GetNodeAt(MousePos.X, MousePos.Y);

  if (hitTestTreeNode <> nil) then begin
    mnuTreeViewExpand.Visible   := ((SDFilesystemTreeView1.SelectionCount = 1) and
      not(SDFilesystemTreeView1.Selected.Expanded));
    mnuTreeViewCollapse.Visible := ((SDFilesystemTreeView1.SelectionCount = 1) and
      SDFilesystemTreeView1.Selected.Expanded);

    pmTreeView.Tag              := Integer(hitTestTreeNode);

    // This *should* enable/disable controls taking into account the node the
    // context menu is being displayed for.
    // But since all nodes are currently treated the same, we can get away
    // with just calling _EnableDisableControls(...) for now
    // xxx - lplp - fix this
    // _EnableDisableControls(FHitContextTreeNode);
    _EnableDisableControls();

  end else begin
    // No node under mouse - do not display popup
    Handled := True;
  end;

end;


procedure TfrmExplorerMain.SDFilesystemListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  inherited;
  _EnableDisableControls();

end;

procedure TfrmExplorerMain.SDFilesystemBothViewStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  inherited;

  SetDragCursor();

end;

procedure TfrmExplorerMain.SetDragCursor();
{
  var
  selItems: TStringList;
}
begin
  {
    selItems:= TStringList.Create();
    try
    GetSelectedItems(Sender, selItems);

    if (selItems.count > 1) then
    begin
    SDFilesystemTreeView1.DragCursor := crMultiDrag;
    SDFilesystemListView1.DragCursor := crMultiDrag;
    end
    else
    begin
    SDFilesystemTreeView1.DragCursor := crDrag;
    SDFilesystemListView1.DragCursor := crDrag;
    end;

    finally
    selItems.Free();
    end;
  }

  if IsContolKeyDown() then begin
    SDFilesystemTreeView1.DragCursor := crFreeOTFEDragCopy;
    SDFilesystemListView1.DragCursor := crFreeOTFEDragCopy;
  end else begin
    SDFilesystemTreeView1.DragCursor := crFreeOTFEDragMove;
    SDFilesystemListView1.DragCursor := crFreeOTFEDragMove;
  end;

end;

procedure TfrmExplorerMain.SDFilesystemTreeView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  droppedOnNode: TTreeNode;
begin
  inherited;

  Accept := False;

  if ((Source = SDFilesystemTreeView1) or (Source = SDFilesystemListView1)) then begin
    // Only accept inter-window drag'n'drop onto the treeview if the drop in on
    // top of a node
    // NOTE: This is slightly different from drag'n'drop from MS Windows
    // Explorer
    droppedOnNode := SDFilesystemTreeView1.GetNodeAt(X, Y);
    Accept        := (droppedOnNode <> nil);
  end;

  SetDragCursor();

end;

procedure TfrmExplorerMain.SDFilesystemListView1DblClick(Sender: TObject);
begin
  inherited;

  if ((SDFilesystemListView1.SelCount = 1) and SDFilesystemListView1.DirItemSelected.IsFile) then begin
    actExtractExecute(Sender);
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;

  Accept := False;

  if (
    // Don't support dragging from treeview -> listview - trying to do this causes
    // the treeview to change which node is selected
    // (Source = SDFilesystemTreeView1) or
    (Source = SDFilesystemListView1)) then begin
    Accept := True;
  end;

  SetDragCursor();

end;

function TfrmExplorerMain.IsContolKeyDown(): Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_CONTROL] and 128) <> 0);
end;

// This is called when a Delphi drag/drop operation ends by dropping either of
// the treeview or listview
procedure TfrmExplorerMain.SDFilesystemBothViewEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  droppedOnNode: TTreeNode;
  droppedOnItem: TListItem;
  selItems:      TStringList;
  droppedOnPath: string;
  op:            TFExplOperation;
begin
  inherited;

  if ((Target = SDFilesystemTreeView1) or (Target = SDFilesystemListView1)) then begin
    // Determine path dropped on
    droppedOnPath     := '';
    if (Target = SDFilesystemTreeView1) then begin
      droppedOnNode   := SDFilesystemTreeView1.GetNodeAt(X, Y);
      if (droppedOnNode <> nil) then begin
        droppedOnPath := SDFilesystemTreeView1.PathToNode(droppedOnNode);
      end;

    end else begin
      if (Target = SDFilesystemListView1) then begin
        droppedOnPath     := SDFilesystemListView1.path;

        droppedOnItem     := SDFilesystemListView1.GetItemAt(X, Y);
        if (droppedOnItem <> nil) then begin
          if SDFilesystemListView1.DirItem[droppedOnItem.Index].IsDirectory then begin
            droppedOnPath := IncludeTrailingPathDelimiter(SDFilesystemListView1.path) + SDFilesystemListView1.DirItem
              [droppedOnItem.Index].filename;
          end;
        end;

      end;
    end;

    if (droppedOnPath <> '') then begin
      selItems := TStringList.Create();
      try
        GetSelectedItems(Sender, selItems);

        if IsContolKeyDown() then begin
          op := cmCopy;
        end else begin
          op := cmMove;
        end;

        // We confirm the action as it's pretty easy for the user to accidently
        // drag/drop between the treeview and listview controls
        if ConfirmOperation(op, selItems, droppedOnPath) then begin
          PerformOperation(op, True, selItems, True, droppedOnPath, '');
        end;

      finally
        selItems.Free();
      end;

    end;
  end;

end;


function TfrmExplorerMain.IsFilenameValid(filename: WideString): Boolean;
var
  invalidCharsForDisplay: string;
  filenameNoDots:         WideString;
  i:                      Integer;
begin
  Result           := fFilesystem.IsValidFilename(filename);

  if not(Result) then begin
    filenameNoDots := trim(StringReplace(filename, '.', '', [rfReplaceAll]));

    if (trim(filename) = '') then begin
      // MS Windows doesn't bother telling user, so we don't either
    end
    else if (length(filenameNoDots) <= 0) then begin
      // MS Windows doesn't bother telling user, so we don't either
    end else begin
      invalidCharsForDisplay   := '';
      for i                    := 1 to length(FAT_INVALID_FILENAME_CHARS) do begin
        // Cast to prevent compiler warning
        invalidCharsForDisplay := invalidCharsForDisplay + ' ' + Char(FAT_INVALID_FILENAME_CHARS[i]);
      end;

      SDUMessageDlg(Format(_('A file name cannot contain any of the following characters:' + SDUCRLF + SDUCRLF + '%s'),
        [invalidCharsForDisplay]), mtError);
    end;
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1Edited(Sender: TObject; Item: TListItem; var S: string);
var
  renamedOK: Boolean;
begin
  inherited;

  renamedOK := False;

  // Ditch whitespace...
  S         := trim(S);

  if (S = '') then begin
    // Do nothing - as MS Windows Explorer does
  end else begin
    // Tack on previous filename extension, if file extensions was hidden
    if SDFilesystemListView1.HideKnownFileExtns then begin
      if (SDFilesystemListView1.DisplayedName[Item.Index] <> SDFilesystemListView1.DirItem[Item.Index].filename) then
      begin
        S := S + ExtractFileExt(SDFilesystemListView1.DirItem[Item.Index].filename);
      end;
    end;

    if (SDFilesystemListView1.DirItem[Item.Index].filename = S) then begin
      // Do nothing - renaming file back to what it originally was
    end else begin
      if IsFilenameValid(S) then begin
        // Carry out the rename operation...
        renamedOK := fFilesystem.MoveFileOrDir(IncludeTrailingPathDelimiter(SDFilesystemListView1.path) +
          SDFilesystemListView1.DirItem[Item.Index].filename,
          IncludeTrailingPathDelimiter(SDFilesystemListView1.path) + S);
      end;
    end;
  end;

  if renamedOK then begin
    PostRefresh();
  end else begin
    // Revert to displayed name (not filename; may be hiding file extensions)
    S := SDFilesystemListView1.DisplayedName[Item.Index];
  end;

end;

procedure TfrmExplorerMain.SDFilesystemTreeView1Edited(Sender: TObject; Node: TTreeNode; var S: string);
var
  renamedOK: Boolean;
begin
  inherited;

  renamedOK := False;

  // Ditch whitespace...
  S         := trim(S);

  if (ExtractFilename(SDFilesystemTreeView1.PathToNode(Node)) = S) then begin
    // Do nothing - renaming file back to what it originally was
  end
  else if (S = '') then begin
    // Do nothing - leave as it was
  end else begin
    if IsFilenameValid(S) then begin
      // Carry out the rename operation...
      renamedOK := fFilesystem.MoveFileOrDir(SDFilesystemTreeView1.PathToNode(Node),
        IncludeTrailingPathDelimiter(ExtractFilePath(SDFilesystemTreeView1.PathToNode(Node))) + S);
    end;
  end;

  if renamedOK then begin
    PostRefresh();
  end else begin
    // Revert to displayed name (not filename; may be hiding file extensions)
    S := ExtractFilename(SDFilesystemTreeView1.PathToNode(Node));
  end;

end;

procedure TfrmExplorerMain.PostRefresh();
begin
  PostMessage(self.handle, WM_FREEOTFE_EXPLORER_REFRESH, 0, 0);
end;

procedure TfrmExplorerMain.SDFilesystemListView1Enter(Sender: TObject);
begin
  inherited;
  fLastFocussed := lfListView;
  _EnableDisableControls();
end;

procedure TfrmExplorerMain.SDFilesystemTreeView1Enter(Sender: TObject);
begin
  inherited;
  fLastFocussed := lfTreeView;
  _EnableDisableControls();
end;

// This is called when a MS Windows Explorer drag/drop operation ends by
// dropping on the treeview
procedure TfrmExplorerMain.SDUDropFilesTreeViewItemsDrop(Sender: TObject; DropItems: TStringList; DropPoint: TPoint);
var
  droppedOnNode: TTreeNode;
  importToPath:  WideString;
  // op: TFExplOperation;
begin
  inherited;

  // If the user dropped on a tree node, import to that path. Otherwise,
  // import to the currently selected node
  droppedOnNode   := SDFilesystemTreeView1.GetNodeAt(DropPoint.X, DropPoint.Y);
  if (droppedOnNode = nil) then begin
    droppedOnNode := SDFilesystemTreeView1.Selected;
  end;

  if (droppedOnNode <> nil) then begin
    importToPath := SDFilesystemTreeView1.PathToNode(droppedOnNode);

    {
      At present, moving via MS Windows Explorer isn't supported - pressing
      <SHIFT> doesn't change the cursor during dragging
      - for this reason, we prompt the user
      if IsContolKeyDown() then
      begin
      op:= cmCopy;
      end
      else
      begin
      op:= cmMove;
      end;

      PerformOperation(op, FALSE, DropItems, TRUE, importToPath);
    }

    // As a result - we use this to get the user to confirm copy/move
    Store(DropItems, importToPath);
  end;

end;

// This is called when a MS Windows Explorer drag/drop operation ends by
// dropping on the listview
procedure TfrmExplorerMain.SDUDropFilesListViewItemsDrop(Sender: TObject; DropItems: TStringList; DropPoint: TPoint);
var
  droppedOnItem: TListItem;
  importToPath:  WideString;
  // op: TFExplOperation;
begin
  inherited;

  droppedOnItem    := SDFilesystemListView1.GetItemAt(DropPoint.X, DropPoint.Y);

  importToPath     := SDFilesystemListView1.path;
  if (droppedOnItem <> nil) then begin
    if SDFilesystemListView1.DirItem[droppedOnItem.Index].IsDirectory then begin
      importToPath := IncludeTrailingPathDelimiter(SDFilesystemListView1.path) + SDFilesystemListView1.DirItem
        [droppedOnItem.Index].filename;
    end;
  end;

  if (importToPath <> '') then begin
    {
      At present, moving via MS Windows Explorer isn't supported - pressing
      <SHIFT> doesn't change the cursor during dragging
      - for this reason, we prompt the user
      if IsContolKeyDown() then
      begin
      op:= cmCopy;
      end
      else
      begin
      op:= cmMove;
      end;

      PerformOperation(op, FALSE, DropItems, TRUE, importToPath);
    }

    // As a result - we use this to get the user to confirm copy/move
    Store(DropItems, importToPath);
  end;

end;


procedure TfrmExplorerMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;

  // We save settings here, in case the user changed any of the toolbar
  // visible/invisible, etc settings
  // Note that if the user hasn't configured a location to save their settings,
  // this save will have no effect

  GetExplorerSettings().TreeViewWidth    := SDFilesystemTreeView1.Width;
  GetExplorerSettings().ListViewLayout      := SDFilesystemListView1.Layout;
  GetExplorerSettings().ShowToolbar   := ToolBarVolume.Visible;
  GetExplorerSettings().ShowExplorerToolBar := ToolbarExplorer.Visible;
  GetExplorerSettings().ShowAddressBar      := pnlAddressBar.Visible;
//  GetExplorerSettings().ShowExplorerFolders     := false;
  GetExplorerSettings().ShowExplorerFolders   := SDFilesystemTreeView1.Visible ;

     //this can be changed directly from UI
  GetSettings().ShowStatusbar       := (StatusBar_Status.Visible or StatusBar_Hint.Visible);
  GetSettings().Save();
end;

procedure TfrmExplorerMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  Dismount();
end;

procedure TfrmExplorerMain.FormCreate(Sender: TObject);
var
  goIcon:   Graphics.TIcon;
  goBitmap: Graphics.TBitmap;
  // sysMagGlassIcon: TIcon;
  // settingsFilename: String;
begin
  // GSettings := TExplorerSettings.Create();
  SetFreeOTFEType(TOTFEFreeOTFEDLL);
  SetSettingsType(TExplorerSettings);

  finFormShow   := False;
  finRefreshing := False;


  fShredderObj  := TShredder.Create();
  fWebDAVObj    := TExplorerWebDAV.Create(nil);
  fmappedDrive  := #0;


  inherited;

  fPartitionImage            := nil;
  fFilesystem                := nil;

  fNavigateHistory           := TStringList.Create();
  fnavigateIdx               := -1;

  actListStyleIcons.Tag      := Ord(vsIcon);
  actListStyleSmallIcons.Tag := Ord(vsSmallIcon);
  actListStyleList.Tag       := Ord(vsList);
  actListStyleDetails.Tag    := Ord(vsReport);

  // Set the actions on the TreeView's *context* *menuitems* to nil.
  // The act... actionitem event will still be called, but because of this, the
  // sender will be the popup's TMenuItem instead of the action item
  // This tells us to use the TTreeNode stored in the TreeView context menu's
  // .tag property when determining what dir to carry out the action on.
  //
  // !!!!!!!!!!!!!!
  // WARNING
  // !!!!!!!!!!!!!!
  // As a consequence of this, _EnableDisableControls(...) must *explicitly* set
  // enabled/disabled on these menuitems - as well as the actionitems they
  // would otherwise use!
  mnuTreeViewStoreFile.Action      := nil;
  mnuTreeViewStoreDir.Action       := nil;
  mnuTreeViewExtract.Action        := nil;
  mnuTreeViewCut.Action            := nil;
  mnuTreeViewCopy.Action           := nil;
  mnuTreeViewPaste.Action          := nil;
  mnuTreeViewCreateSubDir.Action   := nil;
  mnuTreeViewDelete.Action         := nil;
  mnuTreeViewRename.Action         := nil;
  mnuTreeViewItemProperties.Action := nil;

  // Add standard Windows "Go" icon to "Go" button
  goIcon                           := TIcon.Create();
  goBitmap                         := Graphics.TBitmap.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, True, DLL_SHELL32_GO_ICON, goIcon) then begin
      // Must specify 16x16 explicitly as the icon loaded is always 32x32
      SDUConvertIconToBitmap(goIcon, 16, 16, goBitmap, nil);
      pbGo.Glyph.Assign(goBitmap);
    end;

  finally
    goBitmap.Free();
    goIcon.Free();
  end;

  {
    // Overlay system standard magnifying glass icon on top of main icon, if
    // possible
    sysMagGlassIcon := TIcon.Create();
    try
    if SDULoadDLLIcon(
    DLL_SHELL32,
    FALSE,
    DLL_SHELL32_MAGNIFYING_GLASS,
    sysMagGlassIcon
    ) then
    begin
    SDUImageFlip(sysMagGlassIcon, ifHorizontal);
    SDUOverlayIcon(Application.Icon, sysMagGlassIcon);
    end;

    finally
    sysMagGlassIcon.Free();
    end;
  }


  // Get drag and drop cursors from resources
  // i.e. Cursor with "+" to indicate when copying
  Screen.Cursors[crFreeOTFEDragMove] := LoadCursor(HInstance, CURSOR_DRAG_MOVE);
  Screen.Cursors[crFreeOTFEDragCopy] := LoadCursor(HInstance, CURSOR_DRAG_COPY);


  // We set these to invisible so that if they're disabled by the user
  // settings, it doens't flicker on then off
  pnlAddressBar.Visible         := False;
  ToolBarVolume.Visible         := False;
  ToolbarExplorer.Visible       := False;
  Splitter1.Visible             := False;
  SDFilesystemTreeView1.Visible := False;
  StatusBar_Status.Visible      := False;
  StatusBar_Hint.Visible        := False;

  // Give the user a clue
  ToolBarVolume.ShowHint        := True;
  ToolbarExplorer.ShowHint      := True;

  ToolBarVolume.Indent          := 5;
  ToolbarExplorer.Indent        := 5;

  // Cosmetic tweak - when the user rightclicks on a node, this will make it
  // appear selected while the context menu is displayed - BUT WITHOUT FIRING
  // A TREEVIEW OnChange EVENT. After the context menu disappears, the
  // previously selected node appears selected
  SDFilesystemTreeView1.RightClickSelect := True;


  { TODO 1 -otdk -crefactor : whats the point of this instead of doing in designer? investigate and fix }
  // Change toolbar captions to shorter captions
  tbbNew.Caption                    := RS_TOOLBAR_CAPTION_NEW;
  tbbMountFile.Caption              := RS_TOOLBAR_CAPTION_MOUNTFILE;
  tbbDismount.Caption               := RS_TOOLBAR_CAPTION_DISMOUNT;

  tbbNavigateBack.Caption           := RS_TOOLBAR_CAPTION_BACK;
  tbbNavigateForward.Caption        := RS_TOOLBAR_CAPTION_FORWARD;
  tbbUp.Caption                     := RS_TOOLBAR_CAPTION_UP;
  tbbMoveTo.Caption                 := RS_TOOLBAR_CAPTION_MOVETO;
  tbbCopyTo.Caption                 := RS_TOOLBAR_CAPTION_COPYTO;
  tbbDelete.Caption                 := RS_TOOLBAR_CAPTION_DELETE;
  tbbViews.Caption                  := RS_TOOLBAR_CAPTION_VIEWS;
  tbbExtract.Caption                := RS_TOOLBAR_CAPTION_EXTRACT;
  tbbStore.Caption                  := RS_TOOLBAR_CAPTION_STORE;
  tbbItemProperties.Caption         := RS_TOOLBAR_CAPTION_ITEMPROPERTIES;
  tbbExplorerBarFolders.Caption     := RS_TOOLBAR_CAPTION_FOLDERS;
  tbbMapNetworkDrive.Caption        := RS_TOOLBAR_CAPTION_MAP_DRIVE;
  tbbDisconnectNetworkDrive.Caption := RS_TOOLBAR_CAPTION_DISCONNECT;

  mnuToolbarStoreFile.Caption       := RS_TOOLBAR_MNU_CAPTION_STORE_FILE;
  mnuToolbarStoreDir.Caption        := RS_TOOLBAR_MNU_CAPTION_STORE_DIR;

  // Toolbar hints...
  tbbNew.Hint                       := RS_TOOLBAR_HINT_NEW;
  tbbMountFile.Hint                 := RS_TOOLBAR_HINT_MOUNTFILE;
  tbbDismount.Hint                  := RS_TOOLBAR_HINT_DISMOUNT;

  tbbNavigateBack.Hint              := RS_TOOLBAR_HINT_BACK;
  tbbNavigateForward.Hint           := RS_TOOLBAR_HINT_FORWARD;
  tbbUp.Hint                        := RS_TOOLBAR_HINT_UP;
  tbbMoveTo.Hint                    := RS_TOOLBAR_HINT_MOVETO;
  tbbCopyTo.Hint                    := RS_TOOLBAR_HINT_COPYTO;
  tbbDelete.Hint                    := RS_TOOLBAR_HINT_DELETE;
  tbbViews.Hint                     := RS_TOOLBAR_HINT_VIEWS;
  tbbExtract.Hint                   := RS_TOOLBAR_HINT_EXTRACT;
  tbbStore.Hint                     := RS_TOOLBAR_HINT_STORE;
  tbbItemProperties.Hint            := RS_TOOLBAR_HINT_ITEMPROPERTIES;
  tbbExplorerBarFolders.Hint        := RS_TOOLBAR_HINT_EXPLORERBARFOLDERS;
  tbbMapNetworkDrive.Hint           := RS_TOOLBAR_HINT_MAP_DRIVE;
  tbbDisconnectNetworkDrive.Hint    := RS_TOOLBAR_HINT_DISCONNECT;

  mnuToolbarStoreFile.Hint          := RS_TOOLBAR_MNU_HINT_STORE_FILE;
  mnuToolbarStoreDir.Hint           := RS_TOOLBAR_MNU_HINT_STORE_DIR;

end;

procedure TfrmExplorerMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(fShredderObj);
  _WebDAVShutdown();
  FreeAndNil(fWebDAVObj);
  fNavigateHistory.Free();
  // FreeAndNil(GetSettings());
end;

procedure TfrmExplorerMain.FormResize(Sender: TObject);
begin
  SizeStatusBar();
end;

procedure TfrmExplorerMain.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var
  textHeight: Integer;
begin
  if ((Panel.Index <> STATUSBAR_PANEL_LOCATION) or (Panel.Text = '')) then begin
    inherited;
  end else begin
    // +2 here to "center" it a bit more
    ImageList_StatusBar.Draw(StatusBar.Canvas, (Rect.Left + 2), Rect.Top, IMGLIST_IDX_FREEOTFE);

    // +5 here to give a bit of a gap between the image and text
    textHeight := StatusBar.Canvas.textHeight(Panel.Text);
    StatusBar.Canvas.TextOut((Rect.Left + ImageList_StatusBar.Width + 5),
      Rect.Top + (((Rect.Bottom - Rect.Top) - textHeight) div 2), Panel.Text);
    {
      StatusBar.Canvas.TextRect(
      Rect,
      (Rect.Left + ImageList_StatusBar.width + 5),
      (Rect.Top + (((Rect.Bottom - Rect.Top) - textHeight) div 2)),
      Panel.Text
      );
    }
  end;
end;

procedure TfrmExplorerMain.PromptForAndImportFile(importToPath: WideString);
begin
  SDUOpenDialog_Store.Options := SDUOpenDialog_Store.Options + [ofHideReadOnly,
  // i.e. Set to TRUE to remove the readonly checkbox
  ofPathMustExist, ofFileMustExist, ofAllowMultiSelect];
  SDUOpenDialog_Store.Options := SDUOpenDialog_Store.Options + [ofDontAddToRecent];
  if SDUOpenDialog_Store.Execute then begin
    Store(SDUOpenDialog_Store.Files, importToPath);
  end;

end;


// Returns: TRUE if the user wants to replace the existing file, FALSE
// otherwise
function TfrmExplorerMain.PromptToReplace(filename: string; existingSize: ULONGLONG; existingDateTime: TDateTime;
  newSize: ULONGLONG; newDateTime: TDateTime): Boolean;
begin
  Result := SDUConfirmYN(Format(_('This folder already contains a file named ''%s''.' + SDUCRLF + SDUCRLF +
    'Would you like to replace the existing file' + SDUCRLF + SDUCRLF + '  %s' + SDUCRLF + '  modified: %s' + SDUCRLF +
    SDUCRLF + 'with this one?' + SDUCRLF + SDUCRLF + '  %s' + SDUCRLF + '  modified: %s'),
    [ExtractFilename(filename), SDUFormatAsBytesUnits(existingSize), DateTimeToStr(existingDateTime),
    SDUFormatAsBytesUnits(newSize), DateTimeToStr(newDateTime)]));

end;

// Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
function TfrmExplorerMain.PromptToReplaceYYANC(filename: string; existingSize: ULONGLONG; existingDateTime: TDateTime;
  newSize: ULONGLONG; newDateTime: TDateTime): Integer;
begin
  Result := SDUMessageDlg(Format(_('This folder already contains a file named ''%s''.' + SDUCRLF + SDUCRLF +
    'Would you like to replace the existing file' + SDUCRLF + SDUCRLF + '  %s' + SDUCRLF + '  modified: %s' + SDUCRLF +
    SDUCRLF + 'with this one?' + SDUCRLF + SDUCRLF + '  %s' + SDUCRLF + '  modified: %s'),
    [ExtractFilename(filename), SDUFormatAsBytesUnits(existingSize), DateTimeToStr(existingDateTime),
    SDUFormatAsBytesUnits(newSize), DateTimeToStr(newDateTime)]), mtConfirmation,
    [mbYes, mbNo, mbCancel, mbYesToAll], 0);

end;

// Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
function TfrmExplorerMain.PromptToReplaceYYANC(srcIsMountedFSNotLocalFS: Boolean; srcPathAndFilename: string;
  destIsMountedFSNotLocalFS: Boolean; destPathAndFilename: string): Integer;

  procedure GetFileDetails(isMountedFSNotLocalFS: Boolean; pathAndFilename: string; var size: ULONGLONG;
    var DateTime: TDateTime);
  var
    Item:           TSDDirItem_FAT;
    CreationTime:   TFileTime;
    LastAccessTime: TFileTime;
    LastWriteTime:  TFileTime;
  begin
    if isMountedFSNotLocalFS then begin
      Item         := TSDDirItem_FAT.Create();
      try
        if fFilesystem.GetItem_FAT(pathAndFilename, Item) then begin
          size     := Item.size;
          DateTime := TimeStampToDateTime(Item.TimestampLastModified);
        end;
      finally
        Item.Free();
      end;

    end else begin
      size       := SDUGetFileSize(pathAndFilename);

      DateTime   := Now();
      if SDUFileTimestamps(pathAndFilename, CreationTime, LastAccessTime, LastWriteTime) then begin
        DateTime := SDUFileTimeToDateTime(LastWriteTime);
      end;

    end;

  end;

var


  destSize:     ULONGLONG;
  destDateTime: TDateTime;
  srcSize:      ULONGLONG;
  srcDateTime:  TDateTime;
begin
  GetFileDetails(srcIsMountedFSNotLocalFS, srcPathAndFilename, srcSize, srcDateTime);
  GetFileDetails(destIsMountedFSNotLocalFS, destPathAndFilename, destSize, destDateTime);

  Result := PromptToReplaceYYANC(ExtractFilename(srcPathAndFilename), destSize, destDateTime, srcSize, srcDateTime);

end;

function TfrmExplorerMain._StoreFile(path: WideString; fileToStore: string): Boolean;
var
  Item:       TSDDirItem_FAT;
  fileStream: TFileStream;
begin
  Item       := TSDDirItem_FAT.Create();
  fileStream := TFileStream.Create(fileToStore, (fmOpenRead or fmShareDenyNone));
  try
    GetLocalFileDetails(fileToStore, Item);

    Item.FilenameDOS := '';   // Set to '' to autogenerate DOS 8.3 filename
    Item.IsFile      := True; // *VERY* IMPORTANT!!!

    Result           := fFilesystem.StoreFileOrDir(path, Item, fileStream);
  finally
    fileStream.Free();
    Item.Free();
  end;

end;

procedure TfrmExplorerMain.tbbWithDropDownMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  TToolButton(Sender).CheckMenuDropdown();

end;

procedure TfrmExplorerMain.PromptForAndImportDir(importToPath: WideString);
var
  srcPathStr: string;
  stlPath:    TStringList;
begin
  if SDUSelectDirectory(self.handle, _('Folder to store:'), '', srcPathStr) then begin
    stlPath := TStringList.Create();
    try
      stlPath.Add(srcPathStr);
      Store(stlPath, importToPath);
    finally
      stlPath.Free();
    end;
  end;
end;

function TfrmExplorerMain.CreateSubDir(path: WideString; newDirName: WideString): Boolean;
begin
  Result := fFilesystem.CreateDir(path, newDirName);
end;

function TfrmExplorerMain.Mounted(): Boolean;
begin
  Result   := False;

  if (fFilesystem <> nil) then begin
    Result := fFilesystem.Mounted;
  end;

end;


function TfrmExplorerMain.IsSenderTreeviewContextMenu(Sender: TObject): Boolean;
var
  currMenuItem: TMenuItem;
begin
  Result           := False;

  if (Sender is TMenuItem) then begin
    currMenuItem   := TMenuItem(Sender);
    while (currMenuItem <> nil) do begin
      if (currMenuItem.Owner = pmTreeView) then begin
        Result     := True;
        break;
      end;
      currMenuItem := currMenuItem.Parent;
    end;
  end;

end;

// Populate the supplied TStringList with the full path and filenames of the
// most recently touched/selected items
procedure TfrmExplorerMain.GetSelectedItems(Sender: TObject; targets: TStringList);
var
  popupNode:          TTreeNode;
  i:                  Integer;
  useContextMenuNode: Boolean;
begin
  useContextMenuNode := IsSenderTreeviewContextMenu(Sender);

  if useContextMenuNode then begin
    popupNode        := TTreeNode(pmTreeView.Tag);
    targets.Add(SDFilesystemTreeView1.PathToNode(popupNode));
  end
  else if (fLastFocussed = lfTreeView) then begin
    targets.Add(SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected));
  end
  else if (fLastFocussed = lfListView) then begin
    for i := 0 to (SDFilesystemListView1.items.Count - 1) do begin
      if SDFilesystemListView1.items[i].Selected then begin
        targets.Add(IncludeTrailingPathDelimiter(SDFilesystemListView1.path) + SDFilesystemListView1.DirItem[i]
          .filename);
      end;
    end;
  end;

end;


// Return the full path of the most recently touched/selected item
// e.g. In the case of the treeview popup menu, the path of the item the
// context menu was invoked for, otherwise either the treeview or listview's
// path
function TfrmExplorerMain.GetSelectedPath(Sender: TObject): string;
var
  popupNode:          TTreeNode;
  currMenuItem:       TMenuItem;
  useContextMenuNode: Boolean;
begin
  useContextMenuNode       := False;
  if (Sender is TMenuItem) then begin
    currMenuItem           := TMenuItem(Sender);
    while (currMenuItem <> nil) do begin
      if (currMenuItem.Owner = pmTreeView) then begin
        useContextMenuNode := True;
        break;
      end;
      currMenuItem         := currMenuItem.Parent;
    end;
  end;

  if useContextMenuNode then begin
    popupNode := TTreeNode(pmTreeView.Tag);
    Result    := SDFilesystemTreeView1.PathToNode(popupNode);
  end
  else if (fLastFocussed = lfTreeView) then begin
    Result    := SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected);
  end
  else if (fLastFocussed = lfListView) then begin
    Result    := SDFilesystemListView1.path;
  end;

end;

function TfrmExplorerMain._ProcessCommandLine_Mount(): eCmdLine_Exit;
var
  preMounted:  DriveLetterString;
  postMounted: DriveLetterString;
begin
  Result        := ceSUCCESS;
  if GetCmdLine.isMount then begin

    preMounted  := GetFreeOTFEDLL().DrivesMounted;

    Result      := inherited _ProcessCommandLine_Mount();

    postMounted := GetFreeOTFEDLL().DrivesMounted;

    if ((Result = ceSUCCESS) and (preMounted <> postMounted) and
      // Sanity check
      (postMounted <> '') // Sanity check
      ) then begin
      _PostMountGUISetup(postMounted[1]);
      // WebDAVStartup();
      _EnableDisableControls();
    end;
  end;
end;


// Handle "/create" command line
// Returns: Exit code
function TfrmExplorerMain._ProcessCommandLine_Create(): eCmdLine_Exit;
begin
  Result := ceSUCCESS;
  if GetCmdLine.isCreate then
    _PromptCreateFreeOTFEVolume(False);
end;


procedure TfrmExplorerMain.WMUserPostShow(var msg: TWMEndSession);
begin
  inherited;
  ProcessCommandLineOpts();
end;

// Handle any command line options; returns TRUE if command line options
// were passed through, and "/noexit" wasn't specified as a command line
// parameter
function TfrmExplorerMain.ProcessCommandLineOpts(): eCmdLine_Exit;
begin
  Result := _ProcessCommonCommandLine;

end;

procedure TfrmExplorerMain.OnFreeOTFEExplorerRefreshMsg(var msg: TMessage);
begin
  actRefreshExecute(nil);
end;


// Returns: TRUE if no problems, otherwise FALSE
function TfrmExplorerMain.CheckDestinationIsntSource(opType: TFExplOperation; srcPathAndFilename: string;
  destPath: string): Boolean;
var
  ucSrcPath:  string;
  ucDestPath: string;
begin
  Result := True;

  // Note: We use IncludeTrailingPathDelimiter(...) here and not
  // ExcludeTrailingPathDelimiter(...) - otherwise "/" will return ""
  ucSrcPath  := uppercase(IncludeTrailingPathDelimiter(ExtractFilePath(srcPathAndFilename)));
  ucDestPath := uppercase(IncludeTrailingPathDelimiter(destPath));

  // Sanity check; not trying to move to itself?
  if (ucSrcPath = ucDestPath) then begin
    SDUMessageDlg(Format(_('Cannot %s %s: The destination folder is the same as the source folder.'),
      [OperationTitle(opType), ExtractFilename(srcPathAndFilename)]), mtError);
    Result := False;
  end // Sanity check; not trying to move to a subdir?
  else if fFilesystem.DirectoryExists(srcPathAndFilename) then begin
    if (Pos(uppercase(srcPathAndFilename), ucDestPath) = 1) then begin
      SDUMessageDlg(Format(_('Cannot %s %s: The destination folder is a subfolder of the source folder'),
        [OperationTitle(opType), ExtractFilename(srcPathAndFilename)]), mtError);
      Result := False;
    end;
  end;

end;


procedure TfrmExplorerMain.actCutExecute(Sender: TObject);
begin
  inherited;
  ClipboardSetToSelected(Sender, cmMove);
end;

procedure TfrmExplorerMain.actCopyExecute(Sender: TObject);
begin
  inherited;
  ClipboardSetToSelected(Sender, cmCopy);
end;

procedure TfrmExplorerMain.ClipboardClear();
begin
  SDUClearClipboard();
end;

procedure TfrmExplorerMain.ClipboardSetToSelected(Sender: TObject; opType: TFExplOperation);
var
  selItems: TStringList;
begin
  inherited;

  ClipboardClear();

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);

    if (selItems.Count > 0) then begin
      ClipboardSetItems(opType, selItems);
    end;

  finally
    selItems.Free();
  end;

end;

function TfrmExplorerMain.ClipboardHasFiles(): Boolean;
var
  srcIsMountedFSNotLocalFS: Boolean;
  opType:                   TFExplOperation;
  stlItems:                 TStringList;
begin
  stlItems := TStringList.Create();
  try
    Result := ClipboardGetItems(srcIsMountedFSNotLocalFS, opType, stlItems);
  finally
    stlItems.Free();
  end;

end;

function TfrmExplorerMain.ClipboardHasFExplFiles(): Boolean;
begin
  Result := Clipboard.HasFormat(CF_FEXPL_SESSION_DATA);
end;

procedure TfrmExplorerMain.ClipboardSetItems(opType: TFExplOperation; stlItems: TStringList);
var
  dropType: Word;
begin
  ClipboardClear();

  dropType   := DROPEFFECT_COPY;
  if (opType = cmMove) then begin
    dropType := DROPEFFECT_MOVE;
  end;
  SDUSetPreferredDropEffectOnClipboard(dropType);

  SDUSetDropFilesOnClipboard(stlItems);

  SDUSetDWORDOnClipboard(CF_FEXPL_SESSION_DATA, 1);

end;

function TfrmExplorerMain.ClipboardGetItems(out srcIsMountedFSNotLocalFS: Boolean; out opType: TFExplOperation;
  stlItems: TStringList): Boolean;

  function GetDropEffect(): TFExplOperation;
  var
    dropEffect: DWORD;
  begin
    if not(SDUGetPreferredDropEffectFromClipboard(dropEffect)) then begin
      dropEffect := DROPEFFECT_COPY;
    end;

    Result       := cmCopy;
    if ((dropEffect and DROPEFFECT_MOVE) > 0) then begin
      Result     := cmMove;
    end;
  end;

begin
  Result := False;

  stlItems.Clear();

  // Check for FreeOTFE Explorer clipboard type...
  if Clipboard.HasFormat(CF_FEXPL_SESSION_DATA) then begin
    srcIsMountedFSNotLocalFS := True;
    opType                   := GetDropEffect();

    Result                   := SDUGetDropFilesFromClipboard(stlItems);
  end // Check for MS Windows Explorer cut/copied files...
  else if SDUGetDropFilesFromClipboard(stlItems) then begin
    srcIsMountedFSNotLocalFS := False;
    opType                   := GetDropEffect();

    // stlItems already setup, so that's it!

    Result := True;
  end;

end;

procedure TfrmExplorerMain.actPasteExecute(Sender: TObject);
var
  destDir:                  string;
  srcIsMountedFSNotLocalFS: Boolean;
  opType:                   TFExplOperation;
  stlItems:                 TStringList;
begin
  inherited;

  stlItems    := TStringList.Create();
  try
    if ClipboardGetItems(srcIsMountedFSNotLocalFS, opType, stlItems) then begin
      destDir := GetSelectedPath(Sender);

      if PerformOperation(opType, srcIsMountedFSNotLocalFS, stlItems, True, destDir, '') then begin
        if (opType = cmMove) then begin
          ClipboardClear();
        end;
      end;

    end;

  finally
    stlItems.Free();
  end;

end;

procedure TfrmExplorerMain.Extract(items: TStrings);
var
  destDir:      string;
  srcFilename:  string;
  allOK:        Boolean;
  newFilename:  string;
  destFilename: string;
begin
  allOK        := True;
  destFilename := '';

  // If there's only one item to be extracted, and it's a file - let the user
  // specify a path and filename to extract to
  if (items.Count = 1) then begin
    srcFilename                      := items[0];
    if fFilesystem.FileExists(srcFilename) then begin
      SDUSaveDialog_Extract.Options  := SDUSaveDialog_Extract.Options + [ofDontAddToRecent];
      SDUSaveDialog_Extract.filename := ExtractFilename(srcFilename);
      allOK                          := SDUSaveDialog_Extract.Execute();
      if allOK then begin
        newFilename                  := SDUSaveDialog_Extract.filename;

        destDir                      := ExcludeTrailingPathDelimiter(ExtractFilePath(newFilename));
        destFilename                 := ExtractFilename(newFilename);
      end;
    end;

  end;

  // If everything's OK, but there's no destination dir, let the user select
  // one
  // Extracted filenames are the same as the source filenames
  if allOK then begin
    if (destDir = '') then begin
      allOK        := SDUSelectDirectory(self.handle, _('Select location to extract to:'), '', destDir, BIF_USENEWUI);

      destFilename := ''; // Sanity
    end;
  end;


  if allOK then begin
    Extract(items, destDir, destFilename);
  end;

end;

procedure TfrmExplorerMain.Extract(items: TStrings; destDir: string; destFilename: string);
begin
  PerformOperation(cmCopy, True, items, False, destDir, destFilename);
end;

procedure TfrmExplorerMain.Store(items: TStrings);
var
  destDir: string;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtStore, items);
  if (destDir <> '') then begin
    Store(items, destDir);
  end;

end;

procedure TfrmExplorerMain.Store(items: TStrings; destDir: string);
var
  selectOpTypeDlg: TfrmSelectCopyOrMove;
  opType:          TFExplOperation;
  allOK:           Boolean;
begin
  allOK             := True;

  if (GetExplorerSettings().DefaultStoreOp = dsoCopy) then begin
    opType          := cmCopy;
  end
  else if (GetExplorerSettings().DefaultStoreOp = dsoMove) then begin
    opType          := cmMove;
  end else begin
    opType          := cmCopy; // Get rid of compiler warning

    selectOpTypeDlg := TfrmSelectCopyOrMove.Create(self);
    try
      if (selectOpTypeDlg.ShowModal = mrOk) then begin
        opType      := selectOpTypeDlg.opType;
      end else begin
        allOK       := False;
      end;
    finally
      selectOpTypeDlg.Free();
    end;

  end;

  if allOK then begin
    PerformOperation(opType, False, items, True, destDir, '');
  end;

end;

function TfrmExplorerMain.ConfirmPerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
  srcItems: TStrings; destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string): Boolean;
begin
  Result   := False;

  if ConfirmOperation(opType, srcItems, destDir) then begin
    Result := PerformOperation(opType, srcIsMountedFSNotLocalFS, srcItems, destIsMountedFSNotLocalFS, destDir,
      destFilename);
  end;

end;

function TfrmExplorerMain._DetermineTotalSize(isMountedFSNotLocalFS: Boolean; items: TStrings): ULONGLONG;
var
  i: Integer;
begin
  Result   := 0;
  for i    := 0 to (items.Count - 1) do begin
    Result := Result + _DetermineTotalSize(isMountedFSNotLocalFS, items[i]);

    if ProgressDlgHasUserCancelled() then begin
      break;
    end;

  end;

end;

function TfrmExplorerMain._DetermineTotalSize(isMountedFSNotLocalFS: Boolean; Item: string): ULONGLONG;
var
  tmpFileItem: TSDDirItem_FAT;
begin
  Result := 0;

  if isMountedFSNotLocalFS then begin
    if fFilesystem.FileExists(Item) then begin
      tmpFileItem := TSDDirItem_FAT.Create();
      try
        fFilesystem.GetItem_FAT(Item, tmpFileItem);
        Result    := tmpFileItem.size;
      finally
        tmpFileItem.Free();
      end;

    end
    else if fFilesystem.DirectoryExists(Item) then begin
      Result := _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS, Item);
    end;
  end else begin
    if FileExists(Item) then begin
      Result := SDUGetFileSize(Item);
    end
    else if SysUtils.DirectoryExists(Item) then begin
      Result := _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS, Item);
    end;
  end;

end;


function TfrmExplorerMain._DetermineTotalSize_MountedDir(isMountedFSNotLocalFS: Boolean; srcItem: string): ULONGLONG;
var
  srcDirContents:            TSDDirItemList;
  i:                         Integer;
  currSrcSubItem:            TSDDirItem;
  currSrcSubPathAndFilename: string;
begin
  Result                          := 0;

  srcDirContents                  := TSDDirItemList.Create();
  try
    if FSLoadContentsFromDisk(isMountedFSNotLocalFS, srcItem, srcDirContents) then begin
      for i                       := 0 to (srcDirContents.Count - 1) do begin
        currSrcSubItem            := srcDirContents[i];
        currSrcSubPathAndFilename := IncludeTrailingPathDelimiter(srcItem) + currSrcSubItem.filename;

        // Skip volume labels, devices, and "."
        // Also skip "..", unless ShowParentDir is TRUE
        if currSrcSubItem.IsFile then begin
          Result := Result + currSrcSubItem.size;
        end
        else if currSrcSubItem.IsDirectory then begin
          if ((currSrcSubItem.filename <> DIR_CURRENT_DIR) and (currSrcSubItem.filename <> DIR_PARENT_DIR)) then begin
            Result := Result + _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS, currSrcSubPathAndFilename);
          end;
        end;

        if ProgressDlgHasUserCancelled() then begin
          break;
        end;

      end;
    end;

  finally
    srcDirContents.Free();
  end;

end;

function TfrmExplorerMain.PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
  srcItems: TStrings; destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  ): Boolean;
var
  i:                     Integer;
  abortAllRemaining:     Boolean;
  promptOverwriteFiles:  Boolean;
  promptOverwriteDirs:   Boolean;
  totalSize:             ULONGLONG;
  userCancelled:         Boolean;
  dlgOverwritePrompt:    TfrmOverwritePrompt;
  useMoveDeletionMethod: TMoveDeletionMethod;
begin
  abortAllRemaining := False;
  Result            := False;

  // Upfront sanity check ON ALL ITEMS - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
    for i                 := 0 to (srcItems.Count - 1) do begin
      if not(CheckDestinationIsntSource(opType, srcItems[i], destDir)) then begin
        // Error message already shown...
        Result            := False;
        abortAllRemaining := True;
      end;
    end;
  end;

  // Bail out, if needed...
  if abortAllRemaining then begin

    exit;
  end;


  useMoveDeletionMethod         := GetExplorerSettings().MoveDeletionMethod;
  if ((opType = cmMove) and not(srcIsMountedFSNotLocalFS)) then begin
    if (useMoveDeletionMethod = mdmPrompt) then begin
      dlgOverwritePrompt        := TfrmOverwritePrompt.Create(self);
      try
        if (dlgOverwritePrompt.ShowModal() = mrOk) then begin
          useMoveDeletionMethod := dlgOverwritePrompt.moveDeletionMethod;
        end;
      finally
        dlgOverwritePrompt.Free();
      end;

    end;
  end;

  // Bail out, if needed...
  if abortAllRemaining then begin

    exit;
  end;


  ProgressDlgSetup(opType);
  ProgressDlgStart();

  ProgressDlgSetLineOne(RS_DETERMINING_SIZE_MSG);
  ProgressDlgSetLineTwo('');
  SetStatusMsg(RS_DETERMINING_SIZE_MSG);

  // Determine the volume of data to be processed, in order to setup the
  // progress dialog correctly
  totalSize := _DetermineTotalSize(srcIsMountedFSNotLocalFS, srcItems);
  ProgressDlgSetTotal(totalSize);

  // Determining the total size of all files could have taken awhile; reset the
  // progress dialog's timer
  ProgressDlgTimerReset();

  if ProgressDlgHasUserCancelled() then begin
    abortAllRemaining := True;
  end;

  if not(abortAllRemaining) then begin
    promptOverwriteFiles := True;
    promptOverwriteDirs  := True;

    abortAllRemaining    := False;
    for i                := 0 to (srcItems.Count - 1) do begin
      abortAllRemaining  := not(_PerformOperation(opType, srcIsMountedFSNotLocalFS, srcItems[i],
        destIsMountedFSNotLocalFS, destDir, destFilename, useMoveDeletionMethod, promptOverwriteFiles,
        promptOverwriteDirs));
      if abortAllRemaining then begin
        break;
      end;

    end;
  end;

  userCancelled := ProgressDlgHasUserCancelled();

  SetStatusMsg('');
  ProgressDlgStop();

  if abortAllRemaining then begin
    if not(userCancelled) then begin
      SDUMessageDlg(Format(_('%s failed.'), [SDUInitialCapital(OperationTitle(opType))]), mtError);
    end;
  end else begin
    // SDUMessageDlg(
    // Format(
    // _('%s completed successfully.'),
    // [SDUInitialCapital(OperationTitle(opType))]
    // ),
    // mtInformation
    // );
  end;

  // Refresh anyway, in case *some* but not all items were processed
  // NOT NEEDED FOR EXTRACTION! i.e. If copying and the destination is the HDD
  if not((opType = cmCopy) and not(destIsMountedFSNotLocalFS)) then begin
    actRefreshExecute(nil);
  end;

  Result := not(abortAllRemaining);

end;

procedure TfrmExplorerMain.ProgressDlgSetup(opType: TFExplOperation);
begin
  // Get rid of any existing...
  ProgressDlgStop();

  fOpProgressDlg       := TSDUWindowsProgressDialog.Create();

  fOpProgressDlg.Title := SDUInitialCapital(OperationVerbTitle(opType)) + '...';

  case opType of
    cmCopy: begin
        fOpProgressDlg.CommonAVI := aviCopyFile;
      end;

    cmMove: begin
        // Although called "aviCopyFiles" in the Delphi enum, this actually shows
        // multiple files being moved
        fOpProgressDlg.CommonAVI := aviCopyFiles;
      end;

    cmDelete: begin
        fOpProgressDlg.CommonAVI := aviDeleteFile;
      end;

  end;

  fOpProgressDlg.CancelMsg      := 'Please wait while the current operation is cleaned up...';

  fOpProgressDlg.LineCompact[1] := True;
  fOpProgressDlg.LineCompact[2] := True;

  // We turn off the "time remaining" display as it can be misleading - we only
  // update the progressbar after each *file* is processed. Although the
  // progressbar is updated with the size of each file as it's processed, this
  // can give "spurts" which can disrupt the "time remaining" display
  fOpProgressDlg.ShowTime := False;

end;

procedure TfrmExplorerMain.ProgressDlgStart();
begin
  fOpProgressDlg.StartProgressDialog();
end;

procedure TfrmExplorerMain.ProgressDlgSetTotal(total: ULONGLONG);
begin
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.Total64 := total;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgTimerReset();
begin
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.Timer(PDTIMER_RESET);
  end;

end;

procedure TfrmExplorerMain.ProgressDlgSetLineOne(line: string);
begin
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.LineText[1] := line;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgSetLineTwo(line: string);
begin
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.LineText[2] := line;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgIncrement(increment: ULONGLONG);
begin
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.Progress64 := fOpProgressDlg.Progress64 + increment;
  end;

end;

function TfrmExplorerMain.ProgressDlgHasUserCancelled(): Boolean;
begin
  Result   := False;
  if (fOpProgressDlg <> nil) then begin
    Result := fOpProgressDlg.HasUserCancelled();
  end;

end;

procedure TfrmExplorerMain.ProgressDlgStop();
begin
  // Stop and free off any existing...
  if (fOpProgressDlg <> nil) then begin
    fOpProgressDlg.StopProgressDialog();
    fOpProgressDlg.Free();
    fOpProgressDlg := nil;
  end;

end;


// destDir - the desination dir (path) where the src item should be placed *under*
// Returns: TRUE if successful, FALSE when any remaining Extract operations
// should be ABORTED
function TfrmExplorerMain._PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean; srcItem: string;
  destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean; var promptOverwriteDirs: Boolean)
  : Boolean;
var
  abortAllRemaining: Boolean;
  destOK:            Boolean;
  srcIsFile:         Boolean;
  srcIsDir:          Boolean;
begin
  abortAllRemaining := False;

  // Sanity check - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
    if not(CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
      // Error message already shown...
      abortAllRemaining := True;
    end;
  end;

  // Sanity check...
  if not(abortAllRemaining) then begin
    if (opType = cmDelete) then begin
      // Destination irrelevant; deleting source
      destOK := True;
    end
    else if destIsMountedFSNotLocalFS then begin
      destOK := fFilesystem.DirectoryExists(destDir);
    end else begin
      destOK := SysUtils.DirectoryExists(destDir);
    end;
    if not(destOK) then begin
      SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION, [OperationTitle(opType),
        ExtractFilename(destDir)]), mtError);

      abortAllRemaining := True;
    end;
  end;

  if not(abortAllRemaining) then begin
    // Check if source is a file...
    if srcIsMountedFSNotLocalFS then begin
      srcIsFile := fFilesystem.FileExists(srcItem);
    end else begin
      srcIsFile := SysUtils.FileExists(srcItem);
    end;

    // If source a file, process as such...
    if srcIsFile then begin
      abortAllRemaining := not(_PerformOperation_File(opType, srcIsMountedFSNotLocalFS, srcItem,
        destIsMountedFSNotLocalFS, destDir, destFilename, moveDeletionMethod, promptOverwriteFiles,
        promptOverwriteDirs));
    end else begin
      // Check if source is a dir...
      if srcIsMountedFSNotLocalFS then begin
        srcIsDir := fFilesystem.DirectoryExists(srcItem);
      end else begin
        srcIsDir := SysUtils.DirectoryExists(srcItem);
      end;

      // If it's a dir, process as such...
      if srcIsDir then begin
        abortAllRemaining := not(_PerformOperation_Dir(opType, srcIsMountedFSNotLocalFS, srcItem,
          destIsMountedFSNotLocalFS, destDir, destFilename, moveDeletionMethod, promptOverwriteFiles,
          promptOverwriteDirs));
      end else begin
        // Source is neither a file nor dir - abort.
        SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE, [OperationTitle(opType), ExtractFilename(srcItem)]
          ), mtError);

        abortAllRemaining := True;
      end;
    end;
  end;

  Result := not(abortAllRemaining);
end;

function TfrmExplorerMain._PerformOperation_File(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
  srcItem: string; destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean; var promptOverwriteDirs: Boolean)
  : Boolean;
var
  abortAllRemaining: Boolean;
  goodToDoOp:        Boolean;
  destItem:          string;
  confirmResult:     Word;
  srcOK:             Boolean;
  destOK:            Boolean;
  destItemIsFile:    Boolean;
  destItemIsDir:     Boolean;
  increment:         ULONGLONG;
  tmpItem:           TSDDirItem_FAT;
begin
  goodToDoOp        := True;
  abortAllRemaining := False;

  // Sanity check - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
    if not(CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
      // Error message already shown...
      abortAllRemaining := True;
    end;
  end;

  // Sanity check...
  if srcIsMountedFSNotLocalFS then begin
    srcOK := fFilesystem.FileExists(srcItem);
  end else begin
    srcOK := SysUtils.FileExists(srcItem);
  end;
  if not(srcOK) then begin
    SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE, [OperationTitle(opType), ExtractFilename(srcItem)]
      ), mtError);

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;


  // Sanity check...
  if (opType = cmDelete) then begin
    // Destination irrelevant; deleting source
    destOK := True;
  end
  else if destIsMountedFSNotLocalFS then begin
    destOK := fFilesystem.DirectoryExists(destDir);
  end else begin
    destOK := SysUtils.DirectoryExists(destDir);
  end;
  if not(destOK) then begin
    SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION, [OperationTitle(opType),
      ExtractFilename(destDir)]), mtError);

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;

  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if (destFilename <> '') then begin
        destItem := IncludeTrailingPathDelimiter(destDir) + destFilename;
      end else begin
        destItem := IncludeTrailingPathDelimiter(destDir) + ExtractFilename(srcItem);
      end;
    end else begin
      destItem   := '';
    end;
  end;

  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemIsDir := fFilesystem.DirectoryExists(destItem);
      end else begin
        destItemIsDir := SysUtils.DirectoryExists(destItem);
      end;
      if destItemIsDir then begin
        SDUMessageDlg
          (Format(_(
          'Cannot create or replace %s: There is already a file with the same name as the folder name you specified. Specify a different name.'),
          [ExtractFilename(srcItem)]), mtError);

        abortAllRemaining := True;
        goodToDoOp        := False;
      end else begin
        if destIsMountedFSNotLocalFS then begin
          destItemIsFile  := fFilesystem.FileExists(destItem);
        end else begin
          destItemIsFile  := SysUtils.FileExists(destItem);
        end;

        if destItemIsFile then begin
          if promptOverwriteFiles then begin
            confirmResult := PromptToReplaceYYANC(srcIsMountedFSNotLocalFS, srcItem, destIsMountedFSNotLocalFS,
              destItem);

            if (confirmResult = mrYes) then begin
              // Do nothing - goodToDoOp already set to TRUE
            end
            else if (confirmResult = mrYesToAll) then begin
              // goodToDoOp already set to TRUE
              promptOverwriteFiles := False;
              // This is what MS Windows explorer appears to do.
            end
            else if (confirmResult = mrNo) then begin
              goodToDoOp        := False;
            end
            else if (confirmResult = mrCancel) then begin
              abortAllRemaining := True;
              goodToDoOp        := False;
            end;
          end else begin
            // Do nothing - goodToDoOp already set to TRUE
          end;

        end;
      end;
    end;
  end;


  // Delete any existing destination item - *if* it's a file (directories get
  // merged)
  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        if fFilesystem.FileExists(destItem) then begin
          goodToDoOp := fFilesystem.DeleteFileOrDir(destItem);
        end;
      end else begin
        if SysUtils.FileExists(destItem) then begin
          // lplp - shouldn't this be the standard overwrite? Maybe...
          goodToDoOp := SysUtils.DeleteFile(destItem);
        end;
      end;

      if not(goodToDoOp) then begin
        SDUMessageDlg(Format(RS_UNABLE_TO_DELETE_EXISTING_FILE, [ExtractFilename(destItem)]), mtError);
        abortAllRemaining := True;
      end;
    end;
  end;

  // Process the file...
  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    // Get the size of the file before copy/move/delete
    if srcIsMountedFSNotLocalFS then begin
      tmpItem     := TSDDirItem_FAT.Create();
      try
        fFilesystem.GetItem_FAT(srcItem, tmpItem);
        increment := tmpItem.size;
      finally
        tmpItem.Free();
      end;

    end else begin
      increment       := SDUGetFileSize(srcItem);
    end;

    abortAllRemaining := not(_PerformOperation_File_Actual(opType, srcIsMountedFSNotLocalFS, srcItem,
      destIsMountedFSNotLocalFS, destDir, destItem, moveDeletionMethod));

    if abortAllRemaining then begin
      if (opType = cmDelete) then begin
        SDUMessageDlg(Format(_('Unable to delete ''%s''.'), [ExtractFilename(srcItem)]), mtError);
      end else begin
        SDUMessageDlg(Format(_('Unable to create or replace %s.'), [ExtractFilename(srcItem)]), mtError);
      end;
    end else begin
      ProgressDlgIncrement(increment);
    end;
  end;

  Result := not(abortAllRemaining);
end;

// All checks carried out - do operation
// IMPORTANT: This function should **ONLY** be called from _PerformOperation_File(...)
function TfrmExplorerMain._PerformOperation_File_Actual(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
  srcItem: string; destIsMountedFSNotLocalFS: Boolean; destDir: string; destItem: string;
  moveDeletionMethod: TMoveDeletionMethod): Boolean;

// Return the last path segment of the path passed in
  function GetLastSegmentFrom(path: string): string;
  begin
    Result   := ExcludeTrailingPathDelimiter(path);
    Result   := ExtractFilename(Result);
    if (Result = '') then begin
      Result := PATH_SEPARATOR;
    end;

  end;

var
  prevCursor: TCursor;
begin
  Result        := False;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    ProgressDlgSetLineOne(ExtractFilename(srcItem));
    if (opType = cmDelete) then begin
      ProgressDlgSetLineTwo(Format(_('From ''%s'''), [GetLastSegmentFrom(ExtractFilePath(srcItem))]));
    end else begin
      ProgressDlgSetLineTwo(Format(_('From ''%s'' to ''%s'''), [GetLastSegmentFrom(ExtractFilePath(srcItem)),
        GetLastSegmentFrom(destDir)]));
    end;

    // opType    srcIsMountedFSNotLocalFS   destIsMountedFSNotLocalFS   Description
    // cmCopy           TRUE                        TRUE               Copy from mounted volume to mounted volume
    // cmCopy           TRUE                        FALSE              Copy from mounted volume to HDD ("Extract")
    // cmCopy           FALSE                       TRUE               Copy from HDD to mounted volume ("Store")
    // cmCopy           FALSE                       FALSE              Copy from HDD to HDD
    // cmMove           TRUE                        TRUE               Move from mounted volume to mounted volume
    // cmMove           TRUE                        FALSE              Move from mounted volume to HDD ("Extract, delete mounted version")
    // cmMove           FALSE                       TRUE               Move from HDD to mounted volume ("Store, delete HDD version")
    // cmMove           FALSE                       FALSE              Move from HDD to HDD
    // cmDelete         TRUE                        n/a                Delete from mounted volume
    // cmDelete         FALSE                       n/a                Delete from HDD

    // COPY OPERATIONS...
    if (opType = cmCopy) then begin
      if (srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(Format(_('Copying: %s'), [srcItem]));
        Result := fFilesystem.CopyFile(srcItem, destItem);
      end
      else if (srcIsMountedFSNotLocalFS and not(destIsMountedFSNotLocalFS)) then begin
        SetStatusMsg(Format(_('Extracting: %s'), [srcItem]));
        Result := fFilesystem.ExtractFile(srcItem, destItem);
      end
      else if (not(srcIsMountedFSNotLocalFS) and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(Format(_('Storing: %s'), [srcItem]));
        Result := _StoreFile(destDir, srcItem);
      end
      else if (not(srcIsMountedFSNotLocalFS) and not(destIsMountedFSNotLocalFS)) then begin
        Result := CopyFile(PChar(destItem), PChar(srcItem), False);
      end;
    end // MOVE OPERATIONS...
    else if (opType = cmMove) then begin
      if (srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(Format(_('Moving: %s'), [srcItem]));
        Result := fFilesystem.MoveFileOrDir(srcItem, destItem);
      end
      else if (srcIsMountedFSNotLocalFS and not(destIsMountedFSNotLocalFS)) then begin
        SetStatusMsg(Format(_('Extracting: %s'), [srcItem]));
        Result := fFilesystem.ExtractFile(srcItem, destItem);
        if Result then begin
          fFilesystem.DeleteFileOrDir(srcItem);
        end;
      end
      else if (not(srcIsMountedFSNotLocalFS) and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(Format(_('Storing: %s'), [srcItem]));
        Result   := _StoreFile(destDir, srcItem);
        if Result then begin
          Result := DeleteLocalFSItemUsingMethod(moveDeletionMethod, srcItem);
        end;
      end
      else if (not(srcIsMountedFSNotLocalFS) and not(destIsMountedFSNotLocalFS)) then begin
        Result := MoveFile(PChar(destItem), PChar(srcItem));
      end;
    end // DELETE OPERATIONS...
    else if (opType = cmDelete) then begin
      SetStatusMsg(Format(_('Deleting: %s'), [srcItem]));
      if srcIsMountedFSNotLocalFS then begin
        Result := fFilesystem.DeleteFileOrDir(srcItem);
      end else begin
        Result := DeleteLocalFSItemUsingMethod(mdmDelete, srcItem);
      end;

    end;

  finally
    Screen.Cursor := prevCursor;
  end;

end;

function TfrmExplorerMain.DeleteLocalFSItemUsingMethod(moveDeletionMethod: TMoveDeletionMethod; Item: string): Boolean;
var
  userPrompt: Integer;
  promptMsg:  string;
begin
  Result := True;

  if (moveDeletionMethod = mdmDelete) then begin
    // Simple delete...
    // Don't bother to set progress strings - MS Windows Explorer doesn't for
    // *it's* simple move/deletes
    // ProgressDlgSetLineTwo(_('Deleting original...'));
    // SetStatusMsg(Format(_('Deleting: %s'), [item]));

    if FileExists(Item) then begin
      Result := SysUtils.DeleteFile(Item);
    end
    else if SysUtils.DirectoryExists(Item) then begin
      try
        RmDir(Item);
      except
        // Swallow exception; just set return value
        Result := False;
      end;
    end;
  end
  else if (moveDeletionMethod = mdmOverwrite) then begin
    // Overwrite then delete...
    ProgressDlgSetLineTwo(_('Overwriting original...'));
    SetStatusMsg(Format(_('Overwriting: %s'), [Item]));

    Result := (fShredderObj.DestroyFileOrDir(Item, False, True) = srSuccess);
  end;

  // If there was a problem, allow the user to abort/retry/ignore
  if not(Result) then begin
    if (moveDeletionMethod = mdmDelete) then begin
      promptMsg := _('Deletion of %s failed');
    end
    else if (moveDeletionMethod = mdmOverwrite) then begin
      promptMsg := _('Wipe of %s failed');
    end;

    userPrompt := SDUMessageDlg(Format(promptMsg, [ExtractFilename(Item)]), mtWarning, [mbAbort, mbRetry, mbIgnore], 0);

    if (userPrompt = mrAbort) then begin
      // Do nothing - Result already set to FALSE
    end
    else if (userPrompt = mrRetry) then begin
      // Go again...
      Result := DeleteLocalFSItemUsingMethod(moveDeletionMethod, Item);
    end
    else if (userPrompt = mrIgnore) then begin
      // Assume that it's OK...
      Result := True;
    end;

  end;

end;

procedure TfrmExplorerMain.OverwritePassStarted(Sender: TObject; itemName: string; passNumber: Integer;
  totalPasses: Integer);
begin
  ProgressDlgSetLineTwo(Format(_('Overwriting original (Pass: %d / %d)...'), [passNumber, totalPasses]));
end;

procedure TfrmExplorerMain.OverwriteCheckForUserCancel(Sender: TObject; var userCancelled: Boolean);
begin
  userCancelled := ProgressDlgHasUserCancelled();
end;

function TfrmExplorerMain._PerformOperation_Dir(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
  srcItem: string; destIsMountedFSNotLocalFS: Boolean; destDir: string; destFilename: string;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean; var promptOverwriteDirs: Boolean)
  : Boolean;
var
  abortAllRemaining:         Boolean;
  goodToDoOp:                Boolean;
  destItem:                  string;
  srcDirContents:            TSDDirItemList;
  junkFALSE:                 Boolean;
  confirmResult:             Word;
  i:                         Integer;
  currSrcSubItem:            TSDDirItem;
  currSrcSubPathAndFilename: string;
  srcOK:                     Boolean;
  destOK:                    Boolean;
  destItemExistsFile:        Boolean;
  destItemExistsDir:         Boolean;
begin
  goodToDoOp        := True;
  abortAllRemaining := False;

  // Sanity check - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
    if not(CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
      // Error message already shown...
      abortAllRemaining := True;
    end;
  end;

  // Sanity check...
  if srcIsMountedFSNotLocalFS then begin
    srcOK := fFilesystem.DirectoryExists(srcItem);
  end else begin
    srcOK := SysUtils.DirectoryExists(srcItem);
  end;
  if not(srcOK) then begin
    SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE, [OperationTitle(opType), ExtractFilename(srcItem)]
      ), mtError);

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;

  // Sanity check...
  if (opType = cmDelete) then begin
    // Destination irrelevant; deleting source
    destOK := True;
  end
  else if destIsMountedFSNotLocalFS then begin
    destOK := fFilesystem.DirectoryExists(destDir);
  end else begin
    destOK := SysUtils.DirectoryExists(destDir);
  end;
  if not(destOK) then begin
    SDUMessageDlg(Format(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION, [OperationTitle(opType),
      ExtractFilename(destDir)]), mtError);

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;


  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      destItem := IncludeTrailingPathDelimiter(destDir) + ExtractFilename(srcItem);
    end else begin
      // destItem := '';
    end;
  end;


  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemExistsFile := fFilesystem.FileExists(destItem);
      end else begin
        destItemExistsFile := SysUtils.FileExists(destItem);
      end;
      if destItemExistsFile then begin
        SDUMessageDlg
          (Format(_(
          'Cannot create or replace %s: There is already a file with the same name as the folder name you specified. Specify a different name.'),
          [ExtractFilename(srcItem)]), mtError);

        abortAllRemaining   := True;
        goodToDoOp          := False;
      end else begin
        if destIsMountedFSNotLocalFS then begin
          destItemExistsDir := fFilesystem.DirectoryExists(destItem);
        end else begin
          destItemExistsDir := SysUtils.DirectoryExists(destItem);
        end;
        if destItemExistsDir then begin
          if promptOverwriteDirs then begin
            confirmResult :=
              SDUMessageDlg(Format(_('This folder already contains a folder named ''%s''.' + SDUCRLF + SDUCRLF +
              'If the files in the existing folder have the same name as files in the folder you are moving or copying,'
              + ' they will be replaced. Do you still want to move or copy the folder?'), [ExtractFilename(srcItem)]),
              mtWarning, [mbYes, mbNo, mbCancel, mbYesToAll], 0);

            if (confirmResult = mrYes) then begin
              // Do nothing - goodToDoOp already set to TRUE
            end
            else if (confirmResult = mrYesToAll) then begin
              // goodToDoOp already set to TRUE
              promptOverwriteDirs  := False;
              promptOverwriteFiles := False;
              // This is what MS Windows explorer appears to do.
            end
            else if (confirmResult = mrNo) then begin
              goodToDoOp := False;
              // *Don't* set abortAllRemaining here - MS Windows Explorer just
              // continues, skipping this one
            end
            else if (confirmResult = mrCancel) then begin
              abortAllRemaining := True;
              goodToDoOp        := False;
            end;
          end else begin
            // Do nothing - goodToDoOp already set to TRUE
          end;

        end;
      end;
    end;
  end;

  // Create destination dir if it doesn't already exist...
  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemExistsDir := fFilesystem.DirectoryExists(destItem);
      end else begin
        destItemExistsDir := SysUtils.DirectoryExists(destItem);
      end;

      if not(destItemExistsDir) then begin
        if destIsMountedFSNotLocalFS then begin
          goodToDoOp := fFilesystem.CreateDir(destDir, ExtractFilename(destItem));
        end else begin
          goodToDoOp := SysUtils.CreateDir(IncludeTrailingPathDelimiter(destDir) + ExtractFilename(destItem));
        end;

        if not(goodToDoOp) then begin
          SDUMessageDlg(Format(RS_UNABLE_TO_CREATE_FOLDER + SDUCRLF + SDUCRLF + RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE,
            [ExtractFilename(destItem)]), mtError);
          abortAllRemaining := True;
        end;
      end;
    end;
  end;

  // Process the contents of the directory...
  if (goodToDoOp and not(abortAllRemaining) // (Included as sanity check)
    ) then begin
    srcDirContents := TSDDirItemList.Create();
    try
      if FSLoadContentsFromDisk(srcIsMountedFSNotLocalFS, srcItem, srcDirContents) then begin
        // These pass FALSE through always
        junkFALSE           := False;

        for i               := 0 to (srcDirContents.Count - 1) do begin
          abortAllRemaining := ProgressDlgHasUserCancelled();
          if abortAllRemaining then begin
            break;
          end;

          currSrcSubItem            := srcDirContents[i];
          currSrcSubPathAndFilename := IncludeTrailingPathDelimiter(srcItem) + currSrcSubItem.filename;

          // Skip volume labels, devices, and "."
          // Also skip "..", unless ShowParentDir is TRUE
          if currSrcSubItem.IsFile then begin
            abortAllRemaining := not(_PerformOperation_File(opType, srcIsMountedFSNotLocalFS, currSrcSubPathAndFilename,
              destIsMountedFSNotLocalFS, destItem, destFilename, moveDeletionMethod, junkFALSE, promptOverwriteDirs));
          end
          else if currSrcSubItem.IsDirectory then begin
            if ((currSrcSubItem.filename <> DIR_CURRENT_DIR) and (currSrcSubItem.filename <> DIR_PARENT_DIR)) then begin
              abortAllRemaining := not(_PerformOperation_Dir(opType, srcIsMountedFSNotLocalFS,
                currSrcSubPathAndFilename, destIsMountedFSNotLocalFS, destItem, destFilename, moveDeletionMethod,
                junkFALSE, promptOverwriteDirs));
            end;
          end;

          if abortAllRemaining then begin
            break;
          end;

        end;
      end;

    finally
      srcDirContents.Free();
    end;

  end;

  if not(abortAllRemaining) then begin
    if ((opType = cmMove) or (opType = cmDelete)) then begin
      if srcIsMountedFSNotLocalFS then begin
        fFilesystem.DeleteFileOrDir(srcItem);
      end else begin
        abortAllRemaining := DeleteLocalFSItemUsingMethod(moveDeletionMethod, srcItem);
      end;
    end;
  end;

  Result := not(abortAllRemaining);
end;

function TfrmExplorerMain.FSLoadContentsFromDisk(fromMountedFSNotLocalFS: Boolean; srcItem: string;
  srcDirContents: TSDDirItemList): Boolean;
var
  iterator: TSDUFileIterator;
  entry:    string;
  currItem: TSDDirItem_FAT;
begin
  if fromMountedFSNotLocalFS then begin
    Result                        := fFilesystem.LoadContentsFromDisk(srcItem, srcDirContents);
  end else begin
    iterator                      := TSDUFileIterator.Create(nil);
    try
      iterator.Directory          := srcItem;
      iterator.FileMask           := '*';
      iterator.RecurseSubDirs     := False;
      iterator.OmitStartDirPrefix := False;
      iterator.IncludeDirNames    := True;
      iterator.Reset();
      entry                       := iterator.Next();
      while (entry <> '') do begin
        currItem                  := TSDDirItem_FAT.Create();

        GetLocalFileDetails(entry, currItem);

        srcDirContents.Add(currItem);

        entry := iterator.Next();
      end;

    finally
      iterator.Free();
    end;

    Result := True;
  end;

end;

function TfrmExplorerMain.GetLocalFileDetails(pathAndFilename: string; var Item: TSDDirItem_FAT): Boolean;
var
  CreationTime:   TFileTime;
  LastAccessTime: TFileTime;
  LastWriteTime:  TFileTime;
  fileAttrs:      Integer;
begin
  Item.filename                := ExtractFilename(pathAndFilename);
  Item.FilenameDOS             := ''; // Set to '' to autogenerate DOS 8.3 filename

  Item.IsFile                  := SysUtils.FileExists(pathAndFilename);

  Item.size                    := SDUGetFileSize(pathAndFilename);

  if SDUFileTimestamps(pathAndFilename, CreationTime, LastAccessTime, LastWriteTime) then begin
    Item.TimestampCreation     := SDUFileTimeToTimeStamp(CreationTime);
    Item.DatestampLastAccess   := SDUFileTimeToDateTime(LastAccessTime);
    Item.TimestampLastModified := SDUFileTimeToTimeStamp(LastWriteTime);
  end else begin
    Item.TimestampCreation     := DateTimeToTimeStamp(Now);
    Item.DatestampLastAccess   := Now;
    Item.TimestampLastModified := DateTimeToTimeStamp(Now);
  end;

  // Turn off useless hints about FileCtrl.pas being platform specific
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  fileAttrs         := FileGetAttr(pathAndFilename);
  if (fileAttrs <> -1) then begin
    Item.IsReadonly := (fileAttrs and faReadOnly) = faReadOnly;
    Item.IsArchive  := (fileAttrs and faArchive) = faArchive;
    Item.isHidden   := (fileAttrs and faHidden) = faHidden;
    Item.IsSystem   := (fileAttrs and faSysFile) = faSysFile;
  end;
  {$WARN UNIT_PLATFORM ON}
  {$WARN SYMBOL_PLATFORM ON}
  Result := True;
end;

procedure TfrmExplorerMain._DoFullTests;
var
  mountFile:                              string;
  mountedAs:                              DriveLetterChar;
  vol_path, key_file, les_file, projPath: string;
  vl:                                     Integer;
  res:                                    Boolean;
const

  TEST_VOLS: array [0 .. 12] of string = ('a.box', 'b.box', 'c.box', 'd.box', 'e.box', 'e.box', 'f.box', 'luks.box',
    'luks_essiv.box', 'a.box', 'b.box', 'dmcrypt_dx.box', 'dmcrypt_dx.box');
  PASSWORDS: array [0 .. 12] of Ansistring = ('password', 'password', '!"£$%^&*()', 'password', 'password', '5ekr1t',
    'password', 'password', 'password', 'secret', 'secret', 'password', '5ekr1t');
  ITERATIONS: array [0 .. 12] of Integer = (2048, 2048, 2048, 2048, 10240, 2048, 2048, 2048, 2048, 2048, 2048,
    2048, 2048);
  OFFSET: array [0 .. 12] of Integer     = (0, 0, 0, 0, 0, 2097152, 0, 0, 0, 0, 0, 0, 0);
  KEY_FILES: array [0 .. 12] of string   = ('', '', '', '', '', '', '', '', '', 'a.cdb', 'b.cdb', '', '');
  LES_FILES: array [0 .. 12] of string   = ('', '', '', '', '', '', '', '', '', '', '', 'dmcrypt_dx.les',
    'dmcrypt_hid.les');

  {
    TEST_VOLS: array[0..0] of String =
    ( 'luks.box');
    PASSWORDS: array[0..0] of Ansistring =
    ( 'password');
    ITERATIONS: array[0..0] of Integer =
    ( 2048);
    OFFSET: array[0..0] of Integer =
    ( 0);
    KEY_FILES: array[0..0] of String =
    ( '');
    LES_FILES: array[0..0] of String =
    ( '');
  }

  procedure CheckMountedOK;
  var
    S: string;
  begin
    // RefreshDrives();
    // Mount successful
    // prettyMountedAs := prettyPrintDriveLetters(mountedAs);
    _PostMountGUISetup(mountedAs);
    S := SDFilesystemListView1.DirItem[0].filename;
    // ToNode(SDFilesystemListView1.Items[0]);
    // s:= GetSelectedPath(nil);
    if LowerCase(S) <> 'readme.txt' then begin
      SDUMessageDlg('File: readme.txt not found');
      res := False;
    end;
    {
      if (CountValidDrives(mountedAs) <> 1) then begin
      SDUMessageDlg(Format('The Container %s has NOT been opened as drive: %s',
      [TEST_VOLS[vl], mountedAs]));
      res := False;
      end else begin
      //done: test opened OK & file exists
      if not FileExists(mountedAs + ':\README.txt') then begin
      SDUMessageDlg(Format('File: %s:\README.txt not found', [mountedAs]));
      res := False;
      end;
      end;
    }
  end;

  procedure UnMountAndCheck;
  begin
    Application.ProcessMessages;
    Dismount();
    Application.ProcessMessages;
    // RefreshDrives();
    Application.ProcessMessages;
    if CountValidDrives(GetFreeOTFEDLL().DrivesMounted) > 0 then begin
      SDUMessageDlg(Format('Drive(s) %s not unmounted', [GetFreeOTFEDLL().DrivesMounted]));
      res := False;
    end;
  end;

begin
  inherited;

  res                 := True;
  GetCmdLine.isSilent := True;
  if not GetFreeOTFEBase().Active then begin
    ShowMessage('component not active - can''t run tests');
    exit;
  end;

  // for loop is optimised into reverse order , but want to process forwards
  vl       := 0;
  // debug ver is in subdir
  {$IFDEF DEBUG}
  projPath := 'P:\';
  {$ELSE}
  projPath := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\');
  {$ENDIF}
  vol_path := projPath + 'test_vols\';
  while vl <= high(TEST_VOLS) do begin

    // test one at a time as this is normal use
    // mountList.Clear;
    mountFile  := vol_path + TEST_VOLS[vl];
    mountedAs  := #0;
    key_file   := '';
    if KEY_FILES[vl] <> '' then
      key_file := vol_path + KEY_FILES[vl];
    if LES_FILES[vl] <> '' then
      les_file := vol_path + LES_FILES[vl];

    if IsLUKSVolume(mountFile) then begin
      if LUKSTools.MountLUKS(mountFile, mountedAs, True, SDUStringToSDUBytes(PASSWORDS[vl]), key_file, False, nlLF) <> morOK  then
        res := False;
    end else begin
      if (LES_FILES[vl] <> '') then begin
        if frmKeyEntryPlainLinux.MountPlainLinux(mountFile, mountedAs, True, les_file,
          SDUStringToSDUBytes(PASSWORDS[vl]), OFFSET[vl], False, OFFSET[vl] <> 0) <> morOK  then
          res := False;
      end else begin
        // call silently
        if  MountFreeOTFE(mountFile, mountedAs, True, key_file, SDUStringToSDUBytes(PASSWORDS[vl]), OFFSET[vl],
          False, 256, ITERATIONS[vl]) <> morOK then
          res := False;
      end;
    end;
    if not res then begin
      SDUMessageDlg(_('Unable to open ') + TEST_VOLS[vl] + '.', mtError);
    end else begin
      CheckMountedOK;
      UnMountAndCheck;
    end;
    Inc(vl);
  end;


  if res then
    SDUMessageDlg('All functional tests passed')
  else
    SDUMessageDlg('At least one functional test failed');
end;


procedure TfrmExplorerMain.OverwriteAllWebDAVCachedFiles();
{$IFNDEF WEBDAV_OVERWRITE_TSHREDDER}
{$IFNDEF WEBDAV_OVERWRITE_SIMPLE}
{$ERROR
  If the Delphi compiler throws an error at this point, then neither
"WEBDAV_OVERWRITE_TSHREDDER" nor "WEBDAV_OVERWRITE_SIMPLE" were defined.

See notes above for what this should be set to. }
{$ENDIF}
{$ENDIF}
var
  i:          Integer;
  filename:   string;
  pItem:      PRequestedItem;
  prevCursor: TCursor;
  {$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
  Shredder:   TShredder;
  {$ENDIF}
  {$IFDEF WEBDAV_OVERWRITE_SIMPLE}
  data:       TSDUBytes;
  {$ENDIF}
begin
  SetStatusMsg(_('Overwriting cached files...'));

  ProgressDlgSetup(cmDelete);
  fOpProgressDlg.Title := _('Overwriting cached files...');
  ProgressDlgStart();
  ProgressDlgSetTotal(fWebDAVObj.RequestedItems.Count);
  ProgressDlgTimerReset();
  prevCursor    := Screen.Cursor;
  Screen.Cursor := crAppStart; // Hourglass with mouse pointer
  {$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
shreddercomponentversion:
  Shredder      := TShredder.Create(nil);
  {$ENDIF}
  try
    {$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
    Shredder.IntMethod             := smZeros; // Probably faster than smPseudorandom
    Shredder.IntPasses             := 1;
    Shredder.IntRenameBeforeDelete := False;
    {$ENDIF}
    // Setup WebDAV server to ignore writes, and return garbage if files are
    // requested...
    fWebDAVObj.ReturnGarbage := True;

    // Overwrite all files which have been requested during the mounted
    // session...
    // We *WRITE* to the file, as this is the only way of *immediatly* forcing
    // the local copy to be overwritten. Just reading the file again only causes
    // it to be loaded from the cache, until the cached copy times out (typically
    // after 60 seconds)
    for i      := 0 to (fWebDAVObj.RequestedItems.Count - 1) do begin
      ProgressDlgSetLineOne(fWebDAVObj.RequestedItems[i]);
      filename := fmappedDrive + ':' + fWebDAVObj.RequestedItems[i];

      pItem    := PRequestedItem(fWebDAVObj.RequestedItems.Objects[i]);

      // Skip directories...
      if not(pItem.IsFile) then begin
        ProgressDlgIncrement(1);
        continue;
      end;

      {$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
      Shredder.DestroyFileOrDir(filename, False, True, False);
      {$ENDIF}
      {$IFDEF WEBDAV_OVERWRITE_SIMPLE}
      // Cause local cached copy to be overwritten...
      SDUInitAndZeroBuffer(pItem.size, data);
      SDUSetFileContent(filename, data);
      // Cause local cached copy to have zero filesize...
      SDUInitAndZeroBuffer(0, data);
      SDUSetFileContent(filename, data);
      {$ENDIF}
      ProgressDlgIncrement(1);
      if ProgressDlgHasUserCancelled() then begin
        break;
      end;

    end;

  finally
    {$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
    Shredder.Free();
    {$ENDIF}
    ProgressDlgStop();
    Screen.Cursor := prevCursor;
  end;

  fWebDAVObj.ClearDownRequestedItemsList();
  DefaultStatusMsg();

end;

initialization

// A "session ID" is used when registering the FreeOTFE Explorer clipboard
// format to prevent confusion when running multiple instances; atm it isn't
// possible to copy from one instance and paste into another.
// When cut/copy from FreeOTFE Explorer to MS Windows Explorer is
// implemented, this restriction can be removed.
// The current date/time should be unique enough (unless the user mounts
// multiple volumes in the same second!)
CFSTR_FEXPL_SESSION_DATA := Format(CFSTR_FEXPL_SESSION_BASE, [SDUTDateTimeToISO8601(Now)]);

CF_FEXPL_SESSION_DATA    := RegisterClipboardFormat(PChar(CFSTR_FEXPL_SESSION_DATA));

finalization

// There's no means of deregistering the clipboard type?!

end.
