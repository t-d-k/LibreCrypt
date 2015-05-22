unit frmExplorerMain;

interface

uses
  //delphi
  ActnList,
  Buttons, Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, Forms, StdCtrls, SysUtils, ToolWin, Variants, Windows, XPMan,
  Graphics, ImgList, Menus, Messages,
  //sdu libs

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
  SDUProgressDlg,
  Shredder, SDUDialogs,

  //LibreCrypt
  frmCommonMain,
  CommonSettings,
  ExplorerSettings,
  FreeOTFEExplorerWebDAV;

const
  WM_FREEOTFE_EXPLORER_REFRESH = WM_USER + 1;

type
  TLastFocussed = (lfTreeView, lfListView, lfTreePopup);
  TFExplOperation = (cmCopy, cmMove, cmDelete);

resourcestring
  RS_STORE      = 'store';
  RS_EXTRACT    = 'extract';
  RS_MOVE       = 'move';
  RS_COPY       = 'copy';
  RS_DELETE     = 'delete';
  RS_STORING    = 'storing';
  RS_EXTRACTING = 'extracting';
  RS_MOVING     = 'moving';
  RS_COPYING    = 'copying';
  RS_DELETING   = 'deleting';

  RS_ENCRYPTED_VOLUME = 'Locked Container';
  RS_VOLUME_PLAINTEXT = 'Open Container';

  RS_DETERMINING_SIZE_MSG = 'Determining size of files/folders....';

const
  OperationTitlePtr: array [TFExplOperation] of Pointer =
    (@RS_COPY, @RS_MOVE, @RS_DELETE
    );
  OperationVerbTitlePtr: array [TFExplOperation] of Pointer =
    (@RS_COPYING, @RS_MOVING, @RS_DELETING
    );

type
  TfrmExplorerMain = class (TfrmCommonMain)
    pnlTopSpacing: TPanel;
    pmTreeView:   TPopupMenu;
    pmListView:   TPopupMenu;
    mnuTreeViewExpand: TMenuItem;
    mnuExploreView: TMenuItem;
    mnuListViewExtract: TMenuItem;
    SDUOpenDialog_MountPlaintextImage: TSDUOpenDialog;
    ImageList_StatusBar: TImageList;
    mnuListViewStore: TMenuItem;
    mnuListViewStoreFile: TMenuItem;
    mnuListViewStoreDir: TMenuItem;
    mnuViewDelete: TMenuItem;
    N3:           TMenuItem;
    mnuViewProperties: TMenuItem;
    mnuViewCreateSubDir: TMenuItem;
    mnuTreeViewCollapse: TMenuItem;
    SDUOpenDialog_Store: TSDUOpenDialog;
    SDUSaveDialog_Extract: TSDUSaveDialog;
    SDUDropFilesTreeView: TSDUDropFiles;
    SDUDropFilesListView: TSDUDropFiles;
    actPlaintextMountFile: TAction;
    Mountplaintextimage1: TMenuItem;
    Edit2:        TMenuItem;
    actSelectAll: TAction;
    Selectall1:   TMenuItem;
    actShowBootSector: TAction;
    actCheckFilesystem: TAction;
    actListStyleSmallIcons: TAction;
    actListStyleIcons: TAction;
    actListStyleList: TAction;
    actListStyleDetails: TAction;
    N1:           TMenuItem;
    Icons1:       TMenuItem;
    SmallIcons1:  TMenuItem;
    List1:        TMenuItem;
    Details1:     TMenuItem;
    actCheckStatusBar: TAction;
    actCheckAddressBar: TAction;
    oolbars1:     TMenuItem;
    Statusbar2:   TMenuItem;
    Addressbar1:  TMenuItem;
    N6:           TMenuItem;
    Showbootsectordetails1: TMenuItem;
    Checkfilesystem1: TMenuItem;
    pmToolbarBack: TPopupMenu;
    pmToolbarViews: TPopupMenu;
    pmToolbarForward: TPopupMenu;
    pnlExplorer:  TPanel;
    SDFilesystemListView1: TSDFilesystemListView;
    Splitter1:    TSplitter;
    SDFilesystemTreeView1: TSDFilesystemTreeView;
    pnlAddressBar: TPanel;
    lblFolder:    TLabel;
    edPath:       TEdit;
    pbGo:         TBitBtn;
    ToolbarExplorer: TToolBar;
    tbbNavigateBack: TToolButton;
    tbbNavigateForward: TToolButton;
    tbbUp:        TToolButton;
    ToolButton3:  TToolButton;
    tbbMoveTo:    TToolButton;
    tbbCopyTo:    TToolButton;
    ToolButton1:  TToolButton;
    tbbDelete:    TToolButton;
    ToolButton4:  TToolButton;
    tbbViews:     TToolButton;
    ToolBarVolume: TToolBar;
    tbbNew:       TToolButton;
    ToolButton2:  TToolButton;
    tbbMountFile: TToolButton;
    ToolButton5:  TToolButton;
    tbbDismount:  TToolButton;
    actCheckToolbarVolume: TAction;
    actCheckToolbarExplorer: TAction;
    StandardButtons1: TMenuItem;
    Volume1:      TMenuItem;
    tbbExtract:   TToolButton;
    tbbStore:     TToolButton;
    ToolButton6:  TToolButton;
    Icons2:       TMenuItem;
    SmallIcons2:  TMenuItem;
    List2:        TMenuItem;
    Details2:     TMenuItem;
    actDelete:    TAction;
    actStoreFile: TAction;
    actExtract:   TAction;
    pmToolbarStore: TPopupMenu;
    actStoreDir:  TAction;
    mnuToolbarStoreFile: TMenuItem;
    mnuToolbarStoreDir: TMenuItem;
    N7:           TMenuItem;
    N8:           TMenuItem;
    actCreateSubDir: TAction;
    tbbItemProperties: TToolButton;
    ToolButton8:  TToolButton;
    actItemProperties: TAction;
    N10:          TMenuItem;
    mnuTreeViewCreateSubDir: TMenuItem;
    mnuTreeViewDelete: TMenuItem;
    N11:          TMenuItem;
    mnuTreeViewExtract: TMenuItem;
    mnuTreeViewStore: TMenuItem;
    mnuTreeViewStoreFile: TMenuItem;
    mnuTreeViewStoreDir: TMenuItem;
    N12:          TMenuItem;
    mnuTreeViewItemProperties: TMenuItem;
    actUpDir:     TAction;
    actNavigateBack: TAction;
    actNavigateForward: TAction;
    Options1:     TMenuItem;
    N17:          TMenuItem;
    N18:          TMenuItem;
    mnuMainExtract: TMenuItem;
    mnuMainStore: TMenuItem;
    File2:        TMenuItem;
    Folder1:      TMenuItem;
    Plaintextimage1: TMenuItem;
    miPlaintextDismount: TMenuItem;
    actPlaintextNew: TAction;
    New1:         TMenuItem;
    actCheckExplorerBarFolders: TAction;
    ExplorerBar1: TMenuItem;
    Folders1:     TMenuItem;
    tbbExplorerBarFolders: TToolButton;
    ToolButton9:  TToolButton;
    actRename:    TAction;
    mnuTreeViewRename: TMenuItem;
    mnuViewRename: TMenuItem;
    actMoveTo:    TAction;
    actCopyTo:    TAction;
    N19:          TMenuItem;
    CopyToFolder1: TMenuItem;
    MoveToFolder1: TMenuItem;
    actInvertSelection: TAction;
    InvertSelection1: TMenuItem;
    actCut:       TAction;
    actCopy:      TAction;
    actPaste:     TAction;
    N20:          TMenuItem;
    Cut1:         TMenuItem;
    Copy1:        TMenuItem;
    Paste1:       TMenuItem;
    N21:          TMenuItem;
    mnuTreeViewCut: TMenuItem;
    mnuTreeViewCopy: TMenuItem;
    mnuTreeViewPaste: TMenuItem;
    actcut1:      TMenuItem;
    Copy3:        TMenuItem;
    Paste3:       TMenuItem;
    N22:          TMenuItem;
    pmToolbarMenu: TPopupMenu;
    FreeOTFEButtons1: TMenuItem;
    StandardButtons2: TMenuItem;
    AddressBar2:  TMenuItem;
    actOverwriteFile: TAction;
    actOverwriteDir: TAction;
    N23:          TMenuItem;
    Overwrite1:   TMenuItem;
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
    tbbSettings:  TToolButton;
    procedure pbGoClick(Sender: TObject);
    procedure edPathKeyPress(Sender: TObject; var Key: Char);
    procedure mnuTreeViewExpandClick(Sender: TObject);
    procedure mnuExploreViewClick(Sender: TObject);
    procedure pmViewPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SDFilesystemListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
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
    procedure SDFilesystemTreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure SDFilesystemListView1Enter(Sender: TObject);
    procedure SDFilesystemTreeView1Enter(Sender: TObject);
    procedure SDFilesystemListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure actUpDirExecute(Sender: TObject);
    procedure actNavigateBackExecute(Sender: TObject);
    procedure actNavigateForwardExecute(Sender: TObject);
    procedure tbbWithDropDownMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pmToolbarBackPopup(Sender: TObject);
    procedure pmToolbarForwardPopup(Sender: TObject);
    procedure actFreeOTFENewExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure edPathChange(Sender: TObject);
    procedure SDUDropFilesTreeViewItemsDrop(Sender: TObject; DropItems: TStringList;
      DropPoint: TPoint);
    procedure SDUDropFilesListViewItemsDrop(Sender: TObject; DropItems: TStringList;
      DropPoint: TPoint);
    procedure actPlaintextNewExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCheckExplorerBarFoldersExecute(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure SDFilesystemListView1Edited(Sender: TObject; Item: TListItem; var S: String);
    procedure SDFilesystemTreeView1Edited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure actInvertSelectionExecute(Sender: TObject);
    procedure actMoveToExecute(Sender: TObject);
    procedure actCopyToExecute(Sender: TObject);
    procedure SDFilesystemTreeView1DragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SDFilesystemListView1DragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
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
  private
    fPartitionImage: TSDPartitionImage;
    fFilesystem:     TSDFilesystem_FAT;

    fInFormShow:   Boolean;
    fInRefreshing: Boolean;
  protected
    fLastFocussed: TLastFocussed;

    fNavigateHistory: TStringList;
    fNavigateIdx:     Integer;

    fShredderObj:   TShredder;
    fOpProgressDlg: TSDUWindowsProgressDialog;

    fWebDAVObj:   TFreeOTFEExplorerWebDAV;
    fMappedDrive: DriveLetterChar;

    function HandleCommandLineOpts_Create(): eCmdLine_Exit; override;
    function HandleCommandLineOpts_Mount(): eCmdLine_Exit; override;

    procedure ReloadSettings(); override;

    procedure RefreshMRUList(); override;
    procedure MRUListItemClicked(mruList: TSDUMRUList; idx: Integer); override;

    procedure MountFiles(mountAsSystem: TDragDropFileType; filenames: TStringList;
      ReadOnly, forceHidden: Boolean); overload; override;

    procedure PostMountGUISetup(driveLetter: DriveLetterChar);
          {
    procedure WebDAVStartup();
    procedure WebDAVShutdown();   }
    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: DriveLetterChar):
      DriveLetterChar;
    function MapNetworkDrive(const displayErrors: Boolean): Boolean;
    //    procedure DisconnectNetworkDrive();
    //    function  CheckNetServiceStatusAndPrompt(): boolean; overload;
    //    function  CheckNetServiceStatusAndPrompt(const serviceName: string): boolean; overload;

    procedure RecaptionToolbarAndMenuIcons(); override;
    procedure SetIconListsAndIndexes(); override;
    procedure SetupToolbarFromSettings(); override;

    procedure MountPlaintextImage(filename: String; mountReadonly: Boolean);

    function ClipboardHasFiles(): Boolean;
    function ClipboardHasFExplFiles(): Boolean;
    procedure ClipboardClear();
    procedure ClipboardSetToSelected(Sender: TObject; opType: TFExplOperation);
    procedure ClipboardSetItems(opType: TFExplOperation; stlItems: TStringList);
    function ClipboardGetItems(out srcIsMountedFSNotLocalFS: Boolean;
      out opType: TFExplOperation; stlItems: TStringList): Boolean;

    // These functions carry out the extract/store operations...
    function CreateSubDir(path: WideString; newDirName: WideString): Boolean;
    function _StoreFile(path: WideString; fileToStore: String): Boolean;

    procedure ExpandRootTreeNode();

    function Mounted(): Boolean;

    procedure SetTitleCaption();

    procedure SetStatusMsg(); overload;
    procedure SetStatusMsg(msg: String); overload;
    function DefaultStatusMsg(): String;
    function SelectedItemsStatusMsg(): String;
    procedure SizeStatusBar();

    procedure SetDragCursor();

    procedure Dismount(); overload;
    procedure Dismount(driveLetter: DriveLetterChar); overload;

    procedure PromptForAndImportFile(importToPath: WideString);
    procedure PromptForAndImportDir(importToPath: WideString);

    function IsSenderTreeviewContextMenu(Sender: TObject): Boolean;

    procedure GetSelectedItems(Sender: TObject; targets: TStringList);
    function GetSelectedPath(Sender: TObject): String;

    procedure OnNavigationMenuItemClick(Sender: TObject);
    procedure NavigateToHistoryIdx(idx: Integer);

    function IsFilenameValid(filename: WideString): Boolean;

    procedure PostRefresh();
    procedure OnFreeOTFEExplorerRefreshMsg(var Msg: TMessage);
      message WM_FREEOTFE_EXPLORER_REFRESH;

    function CheckDestinationIsntSource(opType: TFExplOperation;
      srcPathAndFilename: String; destPath: String): Boolean;
    function DetermineTotalSize(isMountedFSNotLocalFS: Boolean; items: TStrings): ULONGLONG;
    function _DetermineTotalSize(isMountedFSNotLocalFS: Boolean; item: String): ULONGLONG;
    function _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS: Boolean;
      srcItem: String): ULONGLONG;
    function FSLoadContentsFromDisk(fromMountedFSNotLocalFS: Boolean;
      srcItem: String; srcDirContents: TSDDirItemList): Boolean;
    function GetLocalFileDetails(pathAndFilename: String; var item: TSDDirItem_FAT): Boolean;

    // Move from mounted volume to mounted volume
    procedure MoveTo(items: TStringList); overload;
    procedure MoveTo(items: TStringList; destDir: String); overload;
    // Copy from mounted volume to mounted volume
    procedure CopyTo(items: TStringList); overload;
    procedure CopyTo(items: TStringList; destDir: String); overload;
    // Copy from HDD to mounted volume
    procedure Store(items: TStrings); overload;
    procedure Store(items: TStrings; destDir: String); overload;
    // Copy from mounted volume to HDD
    procedure Extract(items: TStrings); overload;
    procedure Extract(items: TStrings; destDir: String; destFilename: String); overload;
    function PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
      srcItems: TStrings; destIsMountedFSNotLocalFS: Boolean; destDir: String;
      destFilename: String
    // When moving/copying a single file, use this as the destination filename
    // Set to '' to use the source filename's filename
      ): Boolean; overload;
    function ConfirmPerformOperation(opType: TFExplOperation;
      srcIsMountedFSNotLocalFS: Boolean; srcItems: TStrings;
      destIsMountedFSNotLocalFS: Boolean; destDir: String; destFilename: String): Boolean;

    procedure ProgressDlgSetup(opType: TFExplOperation);
    procedure ProgressDlgStart();
    procedure ProgressDlgSetTotal(total: ULONGLONG);
    procedure ProgressDlgTimerReset();
    procedure ProgressDlgSetLineOne(line: String);
    procedure ProgressDlgSetLineTwo(line: String);
    procedure ProgressDlgIncrement(increment: ULONGLONG);
    function ProgressDlgHasUserCancelled(): Boolean;
    procedure ProgressDlgStop();

    function _PerformOperation(opType: TFExplOperation; srcIsMountedFSNotLocalFS: Boolean;
      srcItem: String; destIsMountedFSNotLocalFS: Boolean; destDir: String; destFilename: String;
    // When moving/copying a single file, use this as the destination filename
    // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
      var promptOverwriteDirs: Boolean): Boolean; overload;
    function _PerformOperation_File(opType: TFExplOperation;
      srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
      destDir: String; destFilename: String;
    // When moving/copying a single file, use this as the destination filename
    // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
      var promptOverwriteDirs: Boolean): Boolean;
    function _PerformOperation_File_Actual(opType: TFExplOperation;
      srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
      destDir: String; destItem: String; moveDeletionMethod: TMoveDeletionMethod): Boolean;
    function _PerformOperation_Dir(opType: TFExplOperation;
      srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
      destDir: String; destFilename: String;
    // When moving/copying a single file, use this as the destination filename
    // Set to '' to use the source filename's filename
      moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
      var promptOverwriteDirs: Boolean): Boolean;
    function DeleteLocalFSItemUsingMethod(moveDeletionMethod: TMoveDeletionMethod;
      item: String): Boolean;
    procedure OverwritePassStarted(Sender: TObject; itemName: String;
      passNumber: Integer; totalPasses: Integer);
    procedure OverwriteCheckForUserCancel(Sender: TObject; var userCancelled: Boolean);


    // Returns one of: TRUE / FALSE
    function PromptToReplace(filename: String; existingSize: ULONGLONG;
      existingDateTime: TDateTime; newSize: ULONGLONG; newDateTime: TDateTime): Boolean;
    // Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
    function PromptToReplaceYYANC(filename: String; existingSize: ULONGLONG;
      existingDateTime: TDateTime; newSize: ULONGLONG; newDateTime: TDateTime): Integer;
      overload;
    function PromptToReplaceYYANC(srcIsMountedFSNotLocalFS: Boolean;
      srcPathAndFilename: String; destIsMountedFSNotLocalFS: Boolean;
      destPathAndFilename: String): Integer; overload;


    function IsContolKeyDown(): Boolean;
    function ConfirmOperation(opType: TFExplOperation; srcItems: TStrings;
      destDir: String): Boolean;

    //    procedure OverwriteAllWebDAVCachedFiles();
    function DoTests: Boolean; override;

  public

    procedure WMUserPostShow(var msg: TWMEndSession); override;

    procedure InitApp(); override;

    procedure SetupControls();
    procedure EnableDisableControls(); override;

    // Handle any command line options; returns TRUE if command line options
    // were passed through
    function HandleCommandLineOpts(out cmdExitCode: eCmdLine_Exit): Boolean; override;

  end;

// e.g. Copy, Move, Delete
function OperationTitle(op: TFExplOperation): String;
// e.g. Copying, Moving, Deleting
function OperationVerbTitle(op: TFExplOperation): String;

var
  GfrmExplorerMain: TfrmExplorerMain;

implementation


{$R *.dfm}

uses
  // Turn off useless hints about FileCtrl.pas being platform specific
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
  FileCtrl,
{$WARN UNIT_PLATFORM ON}
{$WARN SYMBOL_PLATFORM ON}
  ActiveX,  // Required for DROPEFFECT_COPY/DROPEFFECT_MOVE
  ClipBrd,
  IdException, Math,
  // Indy components exceptions
  IdSocketHandle,
{$IFDEF UNICODE}
  AnsiStrings,
{$ENDIF}
  frmAbout,
  lcConsts,
  SDUAboutDlg,
  SDUClipBrd,
  SDUGraphics,
  SDUHTTPServer, SDUi18n,
  SDUSysUtils,
  // Just required for DEFAULT_HTTP_PORT
  WinSvc,
  //   SDUWinSvc,
  CheckFilesystem,
  FreeOTFEExplorerfrmNewDirDlg,
  FreeOTFEExplorerfrmOptions,
  frmOverwritePrompt,
  FreeOTFEExplorerfrmPropertiesDlg_Directory,
  FreeOTFEExplorerfrmPropertiesDlg_File,
  FreeOTFEExplorerfrmPropertiesDlg_Multiple,
  FreeOTFEExplorerfrmPropertiesDlg_Volume,
  frmSelectCopyOrMove,
  FreeOTFEExplorerfrmSelectDirectoryDlg,
  frmNewVolumeSize,
  SDFATBootSectorPropertiesDlg
  //  FreeOTFEExplorerfrmWebDAVStatus { TODO 1 -otdk -cenhance : implement webdav }
  ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

 // Exactly one of these *must* be defined
 //{$DEFINE WEBDAV_OVERWRITE_TSHREDDER 1}  // Use TShredder when overwriting local WebDAV cache
{$DEFINE WEBDAV_OVERWRITE_SIMPLE 1}
// Just overwrite local WebDAV cache files with zeros, then truncate to 0 bytes

resourcestring


  // Toolbar captions
  RS_TOOLBAR_CAPTION_BACK           = 'Back';
  RS_TOOLBAR_CAPTION_FORWARD        = 'Forward';
  RS_TOOLBAR_CAPTION_UP             = 'Up';
  RS_TOOLBAR_CAPTION_MOVETO         = 'Move To';
  RS_TOOLBAR_CAPTION_COPYTO         = 'Copy To';
  RS_TOOLBAR_CAPTION_DELETE         = 'Delete';
  RS_TOOLBAR_CAPTION_VIEWS          = 'Views';
  RS_TOOLBAR_CAPTION_EXTRACT        = 'Extract';
  RS_TOOLBAR_CAPTION_STORE          = 'Store';
  RS_TOOLBAR_CAPTION_ITEMPROPERTIES = 'Properties';
  RS_TOOLBAR_CAPTION_FOLDERS        = 'Folders';
  RS_TOOLBAR_CAPTION_MAP_DRIVE      = 'Map Drive';
  RS_TOOLBAR_CAPTION_DISCONNECT     = 'Disconnect';

  RS_TOOLBAR_MNU_CAPTION_STORE_FILE = 'Store file...';
  RS_TOOLBAR_MNU_CAPTION_STORE_DIR  = 'Store folder...';

  // Toolbar hints
  RS_TOOLBAR_HINT_BACK               = 'Back';
  RS_TOOLBAR_HINT_FORWARD            = 'Forward';
  RS_TOOLBAR_HINT_UP                 = 'Up';
  RS_TOOLBAR_HINT_MOVETO             = 'Move To';
  RS_TOOLBAR_HINT_COPYTO             = 'Copy To';
  RS_TOOLBAR_HINT_DELETE             = 'Delete';
  RS_TOOLBAR_HINT_VIEWS              = 'Views';
  RS_TOOLBAR_HINT_EXTRACT            = 'Extract selected files/folders from the open box';
  RS_TOOLBAR_HINT_STORE              = 'Store a file/folder in the open box';
  RS_TOOLBAR_HINT_ITEMPROPERTIES     = 'Properties';
  RS_TOOLBAR_HINT_EXPLORERBARFOLDERS = 'Shows the Folders bar.';
  RS_TOOLBAR_HINT_MAP_DRIVE          = 'Map Drive';
  RS_TOOLBAR_HINT_DISCONNECT         = 'Disconnect';

  RS_TOOLBAR_MNU_HINT_STORE_FILE = 'Store file';
  RS_TOOLBAR_MNU_HINT_STORE_DIR  = 'Store folder';

  RS_COULD_MOUNT_BUT_NOT_PARTITION =
    'Container could be mounted, but not mounted as a partition image?!';
  RS_FILESYSTEM_NOT_FAT121632      =
    'The filesystem used by this box could not be recognised as either FAT12, FAT16 or FAT32.' +
    SDUCRLF + SDUCRLF +
    'Although LibreCrypt supports all filesystems, LibreCrypt Explorer only supports those listed above.';

  RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE =
    'Cannot %1 %2: Cannot find the specified file.' + SDUCRLF + SDUCRLF +
    'Make sure you specify the correct path and file name.';

  RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION =
    'Cannot %1 %2: Cannot find the specified destination.' + SDUCRLF + SDUCRLF +
    'Make sure you specify the correct path and file name.';

  RS_UNABLE_TO_CREATE_FOLDER        = 'Unable to create folder ''%1''';
  RS_UNABLE_TO_DELETE_EXISTING_FILE = 'Unable to delete file ''%1''';

  RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE = 'Please ensure that there is enough free space available';

  // Open/Save file filters...
  FILE_FILTER_FLT_PLAINTEXTIMAGES  = 'Image files (*.img)|*.img|All files|*.*';
  FILE_FILTER_DFLT_PLAINTEXTIMAGES = 'img';

const
  STATUSBAR_PANEL_STATUS        = 0;
  STATUSBAR_PANEL_SELECTEDITEMS = 1;
  STATUSBAR_PANEL_LOCATION      = 2;

  // These icon indexes must match those in ImageList_StatusBar
  IMGLIST_IDX_FREEOTFE = 0;

  // Max number of menu items to show in the Back/Forward button dropdown menus
  MAX_BACKFORWARD_NAV_MENUITEMS  = 10;
  // Max number of items in the history
  MAX_BACKFORWARD_NAV_TOTALITEMS = 50;


  // Drag 'n' drop cursors...
  // (Custom cursors are used to get the "+" on the arrow cursor when copying
  USER_CURSORS_START = 1;
  crFreeOTFEDragMove = TCursor(USER_CURSORS_START);
  crFreeOTFEDragCopy = TCursor(USER_CURSORS_START + 1);

  // These *must* match the resource names in FreeOTFEExplorerCursors.res
  CURSOR_DRAG_COPY = 'CURSOR_DRAG_COPY';
  CURSOR_DRAG_MOVE = 'CURSOR_DRAG_MOVE';

  // String for this instance of FreeOTFE Explorer's clipboard
  CFSTR_FEXPL_SESSION_BASE = 'LibreCryptExplorer:%s';

  ERROR_WORKSTATION_DRIVER_NOT_INSTALLED = $00000836;
// "The workstation driver is not installed."


var
  CF_FEXPL_SESSION_DATA:    Word;
  CFSTR_FEXPL_SESSION_DATA: String;


function OperationTitle(op: TFExplOperation): String;
begin
  Result := LoadResString(OperationTitlePtr[op]);
end;

function OperationVerbTitle(op: TFExplOperation): String;
begin
  Result := LoadResString(OperationVerbTitlePtr[op]);
end;


 // This procedure is called after OnCreate, and immediatly before the
 // Application.Run() is called
procedure TfrmExplorerMain.InitApp();
begin
  inherited;

  // We call EnableDisableControls(...) at this point, to display the
  // toolbar/statusbar as needed
  EnableDisableControls();

  SetupToolbarFromSettings();
  // SetupToolbarFromSettings(...) can change the window's width if large
  // icons are being used
  // We recenter the window here so it looks right
  // But! Only if no main window layout has been stored! If it has, we set it
  // to default
  // SEE CALL TO SDUSetFormLayout(...) FOR DETAILS
  if (gSettings.OptMainWindowLayout = '') then begin
    self.Position := poScreenCenter;
  end else begin
    self.Position := poDefault;
  end;

  if not (ActivateFreeOTFEComponent(True)) then begin
    SDUMessageDlg(
      _('The LibreCrypt DLL (FreeOTFE.dll) could not be found on this computer' +
      SDUCRLF + SDUCRLF + 'Please check your installation'),
      mtError
      );
  end;

  EnableDisableControls();
  SetupPKCS11(False);

  SDUMultimediaKeys1.Active := True;

  // Setup for status bar icon...
  StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Style := psOwnerDraw;
  // Needs to be at least 20 pixels high to allow the icon to be shown properly
  StatusBar_Status.Height := max(StatusBar_Status.Height, 20);
  StatusBar_Hint.Height   := StatusBar_Status.Height;

end;

// Reload settings, setting up any components as needed
procedure TfrmExplorerMain.ReloadSettings();
begin
  inherited;

  SDUClearPanel(pnlTopSpacing);
  SDUClearPanel(pnlAddressBar);
  SDUClearPanel(pnlExplorer);

  SetupToolbarFromSettings();

  SetTitleCaption();

  // Off man out...
  ToolBarVolume.Visible   := gSettings.OptShowToolbarVolume;
  ToolBarExplorer.Visible := gSettings.OptShowToolbarExplorer;

  // Treeview...
  SDFilesystemTreeView1.ShowHiddenItems    := gSettings.OptShowHiddenItems;
  // Listview...
  SDFilesystemListView1.ShowHiddenItems    := gSettings.OptShowHiddenItems;
  SDFilesystemListView1.HideKnownFileExtns := gSettings.OptHideKnownFileExtns;

  fShredderObj.FileDirUseInt               := True;
  fShredderObj.OnStartingFileOverwritePass := OverwritePassStarted;
  fShredderObj.OnCheckForUserCancel        := OverwriteCheckForUserCancel;
  fShredderObj.IntMethod                   := gSettings.OptOverwriteMethod;
  fShredderObj.IntPasses                   := gSettings.OptOverwritePasses;

  if (fFilesystem <> nil) then begin
    if (fFilesystem is TSDFilesystem_FAT) then begin
      fFilesystem.PreserveTimeDateStamps := gSettings.OptPreserveTimestampsOnStoreExtract;
    end;
  end;

  // We DO NOT restart WebDAV functionality here
  //
  // If we did, then it would:
  //
  //   *) Overwrite any WebDAV cache before shutting it down (potentially a time consuming task)
  //   *) Remount the network drive, possibly under a different drive letter
  //   *) Prompt the user the user with the (potentially new) drive letter
  //
  // Because this is all pretty ugly - for the time being, we'll just inform
  // the user that their changes require their volume to be remounted (done by
  // the options dialog)
{
  // Restart WebDAV functionality
  // WARNING! If a transfer was in mid-progress, this may well probably
  // nobble it, though the volume contents should be consistent.
  WebDAVShutdown();
  WebDAVStartup();
}

end;

procedure TfrmExplorerMain.MountFiles(mountAsSystem: TDragDropFileType;
  filenames: TStringList; ReadOnly, forceHidden: Boolean);
var
  mountedAs: DriveLetterString;
  mountedOK: Boolean;
begin
  // Sanity check
  if (filenames.Count <> 1) then begin
    SDUMessageDlg(_('Please specify a single box to be opened'), mtError);
    exit;
  end;

  Dismount();

  AddToMRUList(filenames);

  if (mountAsSystem = ftFreeOTFE) then begin
    mountedOK := GetFreeOTFEDLL().MountFreeOTFE(filenames, mountedAs, ReadOnly);
  end else
  if (mountAsSystem = ftLinux) then begin
    mountedOK := GetFreeOTFEDLL().MountLinux(filenames, mountedAs, ReadOnly);
  end else begin
    mountedOK := GetFreeOTFEDLL().Mount(filenames, mountedAs, ReadOnly);
  end;

  if not (mountedOK) then begin
    if (GetFreeOTFEDLL().LastErrorCode <> OTFE_ERR_USER_CANCEL) then begin
      SDUMessageDlg(
        _('Unable to open Container.') + SDUCRLF + SDUCRLF +
        _('Please check your keyphrase and settings, and try again.'),
        mtError
        );
    end;
  end else begin
    PostMountGUISetup(mountedAs[1]);
    //    WebDAVStartup();
  end;

end;

procedure TfrmExplorerMain.RefreshMRUList();
begin
  inherited;

  // Refresh menuitems...
  gSettings.OptMRUList.RemoveMenuItems(mmMain);
  gSettings.OptMRUList.InsertAfter(miLinuxVolume);

end;

procedure TfrmExplorerMain.MRUListItemClicked(mruList: TSDUMRUList; idx: Integer);
begin
  //lplp - this isnt' right; should cater for plaintext images, and linux volumes being in the MRU list
  //  MountFilesDetectLUKS(mruList.Items[idx], FALSE, Settings.OptDragDropFileType);
  MountFilesDetectLUKS(mruList.Items[idx], False, ftFreeOTFE);  // ftPrompt ?
end;

 // Configure up the GUI, assuming the filename supplied has been mounted under
 // the "drive letter" supplied
procedure TfrmExplorerMain.PostMountGUISetup(driveLetter: DriveLetterChar);
var
  volFilename: String;
begin
  volFilename := GetFreeOTFEDLL().GetVolFileForDrive(driveLetter);

  fPartitionImage         := TPartitionImageDLL.Create();
  TPartitionImageDLL(fPartitionImage).FreeOTFEObj :=
    GetFreeOTFEDLL() as TOTFEFreeOTFEDLL;
  TPartitionImageDLL(fPartitionImage).Filename := volFilename;
  TPartitionImageDLL(fPartitionImage).MountedAs := driveLetter;
  fPartitionImage.Mounted := True;

  if not (fPartitionImage.Mounted) then begin
    fPartitionImage.Free();
    fPartitionImage := nil;
    SDUMessageDlg(RS_COULD_MOUNT_BUT_NOT_PARTITION, mtError);
  end;

  if (fPartitionImage <> nil) then begin
    fFilesystem := TSDFilesystem_FAT.Create();
    fFilesystem.PreserveTimeDateStamps := gSettings.OptPreserveTimestampsOnStoreExtract;

    fFilesystem.PartitionImage := fPartitionImage;
    try
      fFilesystem.Mounted := True;
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

  EnableDisableControls();
end;

{ TODO 1 -otdk -cenhance : implement webdav }
{
procedure TfrmExplorerMain.WebDAVStartup();
const
  // Local range is 127.0.0.1/8 (i.e. 127.[0-255].[0-255].[0-255]) - but we
  // only bother trying the first 255 addresses; there's not much point in
  // checking more than that.
  LOCALHOST_RANGE_FIRST_255 = '127.0.0.%d';
var
  serverPrettyID: string;
  boundSktHandle: TIdSocketHandle;
  ipAddrStr: string;
  i: integer;
  portMsg: string;
begin
  // Shutdown any running...
  WebDAVShutdown();

  if SDUOSVistaOrLater() then
    begin
    SDUMessageDlg(
                  RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7,
                  mtInformation
                 );
    end
  else if (
           Settings.OptWebDAVEnableServer and
           (fFilesystem <> nil)
          ) then
    begin
    WebDAVObj.LogAccess.Filename := Settings.OptWebDAVLogAccess;
    if (WebDAVObj.LogAccess.Filename <> '') then
      begin
      SDUMessageDlg(
                    SDUParamSubstitute(
                                       _('Volume access logging is ENABLED.'+SDUCRLF+
                                       SDUCRLF+
                                       'All accesses to the network server will be written to:'+SDUCRLF+
                                       SDUCRLF+
                                       '%1'),
                                       [WebDAVObj.LogAccess.Filename]
                                      ),
                    mtWarning
                   );
      end;

    WebDAVObj.LogDebug.Filename := Settings.OptWebDAVLogDebug;
    if (WebDAVObj.LogDebug.Filename <> '') then
      begin
      SDUMessageDlg(
                    SDUParamSubstitute(
                                       _('Volume access DEBUG logging is ENABLED.'+SDUCRLF+
                                       SDUCRLF+
                                       'All accesses to the network server will be written to:'+SDUCRLF+
                                       SDUCRLF+
                                       '%1'),
                                       [WebDAVObj.LogDebug.Filename]
                                      ),
                    mtWarning
                   );
      end;

    // Pretty server ID for inclusion in any HTTP headers returned...
    serverPrettyID := Application.Title+'/'+SDUGetVersionInfoString(ParamStr(0));
    if (APP_BETA_BUILD > 0) then
      begin
      serverPrettyID := serverPrettyID + '_BETA_'+inttostr(APP_BETA_BUILD);
      end;
    WebDAVObj.ServerSoftware := serverPrettyID;
    // Port our server runs on...
    WebDAVObj.DefaultPort := Settings.OptWebDAVPort;

    // We generate a random share name in order to prevent caching from "trying
    // it on"
    WebDAVObj.ShareName := SDURandomHexString(4);
    if (Settings.OptWebDAVShareName <> '') then
      begin
      WebDAVObj.ShareName := Settings.OptWebDAVShareName+'_';
      end;
    WebDAVObj.ShareName := WebDAVObj.ShareName + SDURandomHexString(4);
    
    WebDAVObj.fFilesystem := fFilesystem;

    // In case previously used...
    WebDAVObj.ReturnGarbage := FALSE;
    WebDAVObj.ClearDownRequestedItemsList();


    // Loop through to try and get a IP address we can listen on...
    // Note: Start from 1, not 0 and run through to 255 and *not* 256 here!
    for i:=1 to 255 do
      begin
      try
        ipAddrStr := Format(LOCALHOST_RANGE_FIRST_255, [i]);

        // Ditch any previous bindings...
        WebDAVObj.Bindings.Clear();
        // Only bind to the localhost IP; don't allow connections from other PCs...
        boundSktHandle:= WebDAVObj.Bindings.Add();
        //boundSktHandle.IP := IP_ADDR_ALL_IP_ADDRESSES;
        boundSktHandle.IP := ipAddrStr;
        boundSktHandle.Port := WebDAVObj.DefaultPort;

        // Don't allow connections from remote...
        WebDAVObj.OnlyPermitLocal := TRUE;  // For security, only allow connections from localhost

        WebDAVObj.Active := TRUE;

        // Found one! Ditch the loop...
        break;

      except
        // Swallow error
        on EIdCouldNotBindSocket do
          begin
          end;
      end;

      end;

    if not(WebDAVObj.Active) then
      begin
      if (WebDAVObj.DefaultPort = DEFAULT_HTTP_PORT) then
        begin
        portMsg := SDUParamSubstitute(
                                      _('This is most likely caused by a WWW server (on port %1) running on this computer.'),
                                      [WebDAVObj.DefaultPort]
                                     );
        end
      else
        begin
        portMsg := SDUParamSubstitute(
                                      _('You probably have some other service running on the required port (port %1).'),
                                      [WebDAVObj.DefaultPort]
                                     );
        end;

      SDUMessageDlg(
                    _('Unable to start drive mapping server.')+SDUCRLF+
                    SDUCRLF+
                    portMsg,
                    mtWarning
                   );
      end
    else
      begin
      // Success starting WebDAV server - try to map to local drive...
      if not(MapNetworkDrive(FALSE)) then
        begin
        // Check and start services, if needed
        CheckNetServiceStatusAndPrompt();

        // Try again...
        if not(MapNetworkDrive(TRUE)) then
          begin
          // Um... Oh well! Shut it all down.
          WebDAVShutdown();
          end;
        end;
      end;

    end;

  EnableDisableControls();

end;

procedure TfrmExplorerMain.WebDAVShutdown();
begin
  DisconnectNetworkDrive();

  WebDAVObj.Active := FALSE;
  // Sanity - this shouldn't be needed
  WebDAVObj.fFilesystem := nil;

  EnableDisableControls();
end;
}

 // ----------------------------------------------------------------------------
 // !! IMPORTANT !!
 // !! IMPORTANT !!
 // !! IMPORTANT !!
 // If this function is changed, make sure:
 //   TOTFEFreeOTFE.GetNextDriveLetter(...)
 //   TfrmExplorerMain.GetNextDriveLetter(...)
 // are kept in sync
 //
 // Identify the next drive letter to mount a volume as
 // i.e. If userDriveLetter is not set to #0, then return either that letter, or
 //      the next free drive letter.
 //      If userDriveLetter is set to #0, then use requiredDriveLetter instead
 //      of userDriveLetter
 // userDriveLetter - The drive letter the user has specifically requested
 // requiredDriveLetter - The drive letter the system would normally use
 // Returns: Drive letter to mount as, or #0 on error
function TfrmExplorerMain.GetNextDriveLetter(userDriveLetter,
  requiredDriveLetter: DriveLetterChar): DriveLetterChar;
var
  freeDriveLetters:  DriveLetterString;
  searchDriveLetter: DriveLetterChar;
begin
  Result := #0;

  searchDriveLetter := userDriveLetter;
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
  networkShare:   String;
  useDriveLetter: DriveLetterChar;
  msg:            String;
  lastErrorNo:    DWORD;
  lastErrorMsg:   String;
  comment:        String;
begin
  Result := False;

  useDriveLetter := GetNextDriveLetter(#0, gSettings.OptDefaultDriveLetter);

  if (useDriveLetter = #0) then begin
    if displayErrors then begin
      SDUMessageDlg(_('Unable to assign a new drive letter; please confirm you have drive letters free!'),
        mtError);
    end;

    Result := False;
    exit;
  end;


  //  networkShare := '\\'+WebDAVObj.Bindings[0].IP+'\'+WebDAVObj.ShareName;
  networkShare := 'http://' + fWebDAVObj.Bindings[0].IP + '/' + fWebDAVObj.ShareName + '/';

  // Cast to prevent compiler error
  if SDUMapNetworkDrive(networkShare, Char(useDriveLetter)) then begin
    FMappedDrive := useDriveLetter;
    Result       := True;

    if gSettings.OptPromptMountSuccessful then begin
      msg := SDUParamSubstitute(_('Your FreeOTFE container has been mounted as drive: %1'),
        [useDriveLetter + ':']);
      SDUMessageDlg(msg, mtInformation);
    end;

    if gSettings.OptExploreAfterMount then begin
      ExploreDrive(useDriveLetter);
    end;

    AutoRunExecute(arPostMount, useDriveLetter, False);
  end else begin
    if displayErrors then begin
      lastErrorNo  := GetLastError();
      lastErrorMsg := SysErrorMessage(lastErrorNo) + ' (0x' + SDUIntToHex(lastErrorNo, 8) + ')';
      comment      := '';
      if ((lastErrorNo = ERROR_NO_NET_OR_BAD_PATH) or (lastErrorNo =
        ERROR_WORKSTATION_DRIVER_NOT_INSTALLED)) then begin
        comment := _('Please ensure that the WebClient service is running');
      end;

      SDUMessageDlg(
        SDUParamSubstitute(_('Unable to map container to %1:' + SDUCRLF +
        SDUCRLF + '%2' + SDUCRLF + SDUCRLF + '%3'), [useDriveLetter,
        lastErrorMsg, comment]),
        mtWarning
        );
    end;
  end;

end;

 // Returns TRUE if the service was running, or successfully started.
 // Returns FALSE if the service wasn't running, or it's status couldn't be
 // determined
{
function TfrmExplorerMain.CheckNetServiceStatusAndPrompt(): boolean;
var
  servicesOK: boolean;
begin
  servicesOK := CheckNetServiceStatusAndPrompt(SERVICE_MRXDAV);

  if servicesOK then
    begin
    servicesOK := CheckNetServiceStatusAndPrompt(SERVICE_WEBCLIENT);
    end;

  Result := servicesOK;
end;


// Returns TRUE if the service was running, or successfully started.
// Returns FALSE if the service wasn't running, or it's status couldn't be
// determined
function TfrmExplorerMain.CheckNetServiceStatusAndPrompt(const serviceName: string): boolean;
const
  SERVICE_START_PAUSE = 3000;  // 3 seconds
var
  sc: TSDUServiceControl;
  serviceState: cardinal;
  Result: boolean;
begin
  Result := FALSE;

  try
    sc:= TSDUServiceControl.Create();
  except
    // Problem getting service control manager - ignore the error; Result
    // already set to FALSE
    sc := nil;
  end;

  if (sc <> nil) then
  begin
    try
      // We'll process all errors...
      sc.Silent := TRUE;

      if sc.GetServiceState(serviceName, serviceState) then
        begin
        if (serviceState = SERVICE_RUNNING) then
          begin
          Result := TRUE;
          end
        else
          begin
          if (SDUMessageDlg(
                            SDUParamSubstitute(
                                _('The "%1" service does not appear to be running.'+SDUCRLF+
                                SDUCRLF+
                                'This service is required in order to map the mounted container to a drive letter.'+SDUCRLF+
                                SDUCRLF+
                                'Do you wish to start this service now?'),
                                [serviceName]
                                ),
                            mtConfirmation,
                            [mbYes, mbNo],
                            0
                           ) = mrYes) then
            begin
            if sc.StartStopService(serviceName, TRUE) then
              begin
              // Delay to allow service to start up.
              // Not sure if strictly needed, but if a drive is mapped
              // immediatly afterwards, the drive mapping can fail (e.g. with
              // "No network provider accepted the given path"), although it'll
              // succeed if the user subsequently tries to mount again
              SDUPause(SERVICE_START_PAUSE);
              
              Result := TRUE;
              end
            else
              begin
              SDUMessageDlg(
                            SDUParamSubstitute(
                                              _('Unable to start the "%1" service.'),
                                              [serviceName]
                                              ),
                            mtError
                           );
              end;

            end;
          end;
        end;

    finally
      sc.free;
    end;

  end;



end;


procedure TfrmExplorerMain.DisconnectNetworkDrive();
begin
  if (FMappedDrive <> #0) then
    begin
    AutoRunExecute(arPreDismount, FMappedDrive, FALSE);

    if Settings.OptOverwriteWebDAVCacheOnDismount then
      begin
      OverwriteAllWebDAVCachedFiles();
      end;

//    SDUDisconnectNetworkDrive(Char(FMappedDrive), Force);
    SDUDisconnectNetworkDrive(Char(FMappedDrive), TRUE);

    AutoRunExecute(arPostDismount, FMappedDrive, FALSE);

    FMappedDrive := #0;
    end;

end;   }

procedure TfrmExplorerMain.SetupControls();
var
  mountedWritable: Boolean;
begin
  FNavigateHistory.Clear();
  FNavigateIdx := -1;

  SDFilesystemListView1.Filesystem := fFilesystem;
  SDFilesystemTreeView1.Filesystem := fFilesystem;

  // Allow user to rename files...
  mountedWritable := (Mounted() and not (fFilesystem.ReadOnly));

  SDFilesystemTreeView1.ReadOnly := not (mountedWritable);
  SDFilesystemListView1.ReadOnly := not (mountedWritable);

  SDFilesystemListView1.Initialize();
  SDFilesystemTreeView1.Initialize();

  SDFilesystemTreeView1.RefreshNodes();

  // Expand out the root node
  if (SDFilesystemTreeView1.GoToPath(PATH_SEPARATOR, False) <> nil) then begin
    SDFilesystemTreeView1.Selected.Expand(False);
  end;

  SDFilesystemTreeView1.SetFocus();

  SetStatusMsg();

  EnableDisableControls();
end;

procedure TfrmExplorerMain.pbGoClick(Sender: TObject);
begin
  edPath.Text := trim(edPath.Text);
  if (SDFilesystemTreeView1.GoToPath(edPath.Text, False) = nil) then begin
    SDUMessageDlg(SDUParamSubstitute(_('Path not found:' + SDUCRLF + SDUCRLF + '%1'),
      [edPath.Text]), mtError);
  end;

end;


procedure TfrmExplorerMain.edPathChange(Sender: TObject);
begin
  inherited;
  pbGo.Hint     := SDUParamSubstitute(_('Go to ''%1'''), [trim(edPath.Text)]);
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
  //  SDFilesystemTreeView1.Selected.Collapse(FALSE);
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
  //  SDFilesystemTreeView1.Selected.Expand(FALSE);
end;

procedure TfrmExplorerMain.mnuExploreViewClick(Sender: TObject);
begin
  if (SDFilesystemListView1.SelCount > 0) then begin
    SDFilesystemTreeView1.GoToPath(
      IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) +
      SDFilesystemListView1.DirItem[SDFilesystemListView1.SelectedIdx].Filename,
      True
      );
  end;
end;

procedure TfrmExplorerMain.pmToolbarBackPopup(Sender: TObject);
var
  tmpMenuItem: TMenuItem;
  i:           Integer;
begin
  pmToolbarBack.Items.Clear();
  for i := (FNavigateIdx - 1) downto max(0, (FNavigateIdx - MAX_BACKFORWARD_NAV_MENUITEMS))
    do begin
    tmpMenuItem         := TMenuItem.Create(nil);
    tmpMenuItem.Caption := FNavigateHistory[i];
    tmpMenuItem.OnClick := OnNavigationMenuItemClick;
    tmpMenuItem.Tag     := i;
    pmToolbarBack.Items.Add(tmpMenuItem);
  end;

end;

procedure TfrmExplorerMain.pmToolbarForwardPopup(Sender: TObject);
var
  tmpMenuItem: TMenuItem;
  i:           Integer;
begin
  pmToolbarForward.Items.Clear();
  for i := (FNavigateIdx + 1) to min((FNavigateHistory.Count - 1),
      (FNavigateIdx + MAX_BACKFORWARD_NAV_MENUITEMS)) do begin
    tmpMenuItem         := TMenuItem.Create(nil);
    tmpMenuItem.Caption := FNavigateHistory[i];
    tmpMenuItem.OnClick := OnNavigationMenuItemClick;
    tmpMenuItem.Tag     := i;
    pmToolbarForward.Items.Add(tmpMenuItem);
  end;

end;

procedure TfrmExplorerMain.OnNavigationMenuItemClick(Sender: TObject);
var
  targetIdx: Integer;
begin
  targetIdx := TMenuItem(Sender).Tag;

  // Sanity check
  if ((FNavigateHistory.Count > 0) and (targetIdx >= 0) and (targetIdx <=
    (FNavigateHistory.Count - 1))) then begin
    NavigateToHistoryIdx(targetIdx);
  end;

end;

procedure TfrmExplorerMain.pmViewPopup(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmExplorerMain.EnableDisableControls();
var
  mountedWritable: Boolean;
  selectedTargets: TStringList;
begin
  inherited;

  // Inherited controls...
  actDismount.Enabled := Mounted();

  mountedWritable := False;
  if (fFilesystem <> nil) then
    mountedWritable := (Mounted() and not (fFilesystem.ReadOnly));

  // Controls only enabled when mounted...
  actNavigateBack.Enabled    := Mounted() and (FNavigateIdx > 0);
  actNavigateForward.Enabled := (Mounted() and (FNavigateIdx < (FNavigateHistory.Count - 1)));
  SDUEnableControl(edPath, Mounted());
  SDUEnableControl(pbGo, (Mounted() and (trim(edPath.Text) <> '')));

  // Don't disable the ListView - then the user can still play with the column
  // layout/sizes
  //  SDUEnableControl(SDFilesystemListView1, Mounted());
  // Don't disable the TreeView - that'll prevent the splitter from
  // resizing(?!!!!)
  //  SDUEnableControl(SDFilesystemTreeView1, Mounted());

  actSelectAll.Enabled       := Mounted();
  actInvertSelection.Enabled := Mounted();
  actShowBootSector.Enabled  := Mounted();
  actCheckFilesystem.Enabled := Mounted();
  actUpDir.Enabled           := (Mounted() and (SDFilesystemTreeView1.PathToNode(
    SDFilesystemTreeView1.Selected) <> PATH_SEPARATOR));

  // Controls only enabled when mounted for read/write...
  SDUEnableControl(tbbStore, mountedWritable);
  mnuMainStore.Enabled := mountedWritable;
  actStoreFile.Enabled := mountedWritable;
  actStoreDir.Enabled  := mountedWritable;
  actCut.Enabled       := mountedWritable;
  actCopy.Enabled      := mountedWritable;
  actPaste.Enabled     := mountedWritable and ClipboardHasFiles();

  actCreateSubDir.Enabled := mountedWritable;
  actDelete.Enabled       := mountedWritable;

  // Only allow drag and drop to store files/dirs if mounted for read/write...
  SDUDropFilesTreeView.Active := mountedWritable;
  SDUDropFilesListView.Active := mountedWritable;


  mnuTreeViewExpand.Visible   :=
    ((SDFilesystemTreeView1.SelectionCount = 1) and not
    (SDFilesystemTreeView1.Selected.Expanded));
  mnuTreeViewCollapse.Visible :=
    ((SDFilesystemTreeView1.SelectionCount = 1) and SDFilesystemTreeView1.Selected.Expanded);

  mnuExploreView.Enabled :=
    ((SDFilesystemListView1.SelCount = 1) and
    SDFilesystemListView1.DirItem[SDFilesystemListView1.SelectedIdx].IsDirectory);

  mnuViewRename.Enabled :=
    (not (SDFilesystemListView1.ReadOnly) and (SDFilesystemListView1.SelCount = 1) and
    mountedWritable);

  mnuTreeViewRename.Enabled :=
    (not (SDFilesystemTreeView1.ReadOnly) and (SDFilesystemTreeView1.SelectionCount = 1) and
    mountedWritable);


  selectedTargets := TStringList.Create();
  try
    GetSelectedItems(nil, selectedTargets);

    actExtract.Enabled := (Mounted() and (selectedTargets.Count > 0));

    actMoveTo.Enabled := (Mounted() and (selectedTargets.Count > 0));

    actCopyTo.Enabled := (Mounted() and (selectedTargets.Count > 0));

    actItemProperties.Enabled := actExtract.Enabled;
  finally
    selectedTargets.Free();
  end;


  // Remove buttons which are only really used for debug purposes...
  actMapNetworkDrive.Visible        := False;
  actDisconnectNetworkDrive.Visible := False;

  actMapNetworkDrive.Enabled :=
    (Mounted() and (FMappedDrive = #0) and gSettings.OptWebDAVEnableServer);

  actDisconnectNetworkDrive.Enabled :=
    (Mounted() and (FMappedDrive <> #0) and gSettings.OptWebDAVEnableServer);

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
  //       setup on startup
  //  ToolBarVolume.Visible := Settings.OptShowToolbarVolume;
  //  ToolBarExplorer.Visible := Settings.OptShowToolbarExplorer;

  pnlTopSpacing.Visible := not (pnlAddressBar.Visible);

  // Set .Top on aligned "alTop" components so they are shown in the correct order
  // We call LockWindowUpdate(...) to prevent *real* bad flicker!
  LockWindowUpdate(self.handle);
  try
    pnlTopSpacing.Top   := 0;
    ToolBarExplorer.Top := 0;
    ToolBarVolume.Top   := 0;
  finally
    LockWindowUpdate(0);
  end;

  // Update layout controls to reflect their state...
  actCheckToolbarVolume.Checked      := ToolBarVolume.Visible;
  actCheckToolbarExplorer.Checked    := ToolBarExplorer.Visible;
  actCheckAddressBar.Checked         := pnlAddressBar.Visible;
  actCheckExplorerBarFolders.Checked := SDFilesystemTreeView1.Visible;
  actCheckStatusBar.Checked          := (StatusBar_Status.Visible or StatusBar_Hint.Visible);

  // Set menu checkmarks...
  case SDFilesystemListView1.ViewStyle of
    vsReport:
    begin
      actListStyleDetails.Checked := True;
    end;

    vsList:
    begin
      actListStyleList.Checked := True;
    end;

    vsIcon:
    begin
      actListStyleIcons.Checked := True;
    end;

    vsSmallIcon:
    begin
      actListStyleSmallIcons.Checked := True;
    end;
  end;

end;

procedure TfrmExplorerMain.SetTitleCaption();
var
  volFilename: String;
begin
  volFilename := '';
  if Mounted() then begin
    if (fPartitionImage is TPartitionImageDLL) then begin
      volFilename := TPartitionImageDLL(fPartitionImage).Filename;
    end else
    if (fPartitionImage is TSDPartitionImage_File) then begin
      volFilename := TSDPartitionImage_File(fPartitionImage).Filename;
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

procedure TfrmExplorerMain.SetStatusMsg(msg: String);
begin
  if (msg = '') then begin
    msg := DefaultStatusMsg();

    // If setting to default status message, update the selected items panel
    if (fFilesystem = nil) then begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Text := '';
    end else begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Text := SelectedItemsStatusMsg();
    end;

    StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text := '';
    if (fFilesystem <> nil) then begin
      StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text := RS_ENCRYPTED_VOLUME;
      if (fPartitionImage is TSDPartitionImage_File) then begin
        StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Text := RS_VOLUME_PLAINTEXT;
      end;
    end;
  end;

  SizeStatusBar();
  StatusBar_Status.Panels[STATUSBAR_PANEL_STATUS].Text := msg;

  if not (FInFormShow) then begin
    Application.ProcessMessages();  // Required in order to get display to
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

  StatusBar_Status.Panels[STATUSBAR_PANEL_STATUS].Width :=
    self.Width - (StatusBar_Status.Panels[STATUSBAR_PANEL_SELECTEDITEMS].Width +
    StatusBar_Status.Panels[STATUSBAR_PANEL_LOCATION].Width);
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
  lastPath: String;
begin
  inherited;

  if Mounted() then begin
    SetStatusMsg(_('Refreshing...'));

    lastPath := SDFilesystemListView1.Path;
    if (lastPath = '') then begin
      lastPath := PATH_SEPARATOR;
    end;

    FInRefreshing := True;
    try
      SDFilesystemTreeView1.RefreshNodes();
    finally
      FInRefreshing := False;
    end;

    // Always expand root node to make sure if user's just created the first
    // subdir, or stored the first subdir, it shows up in the treeview
    ExpandRootTreeNode();

    SDFilesystemTreeView1.GoToPath(lastPath, True);

    SetStatusMsg('');
  end;

  EnableDisableControls();
end;

procedure TfrmExplorerMain.ExpandRootTreeNode();
var
  rootNode: TTreeNode;
begin
  if Mounted() then begin
    // Make sure the root node is always expanded; otherwise if the user
    // creates a new container, them immediatly creates a subnode, the subnode
    // doesn't get shown in the treeview
    rootNode := SDFilesystemTreeView1.Items.GetFirstNode();
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
    popupNode := TTreeNode(pmTreeView.Tag);
    popupNode.EditText;
  end else
  if (fLastFocussed = lfTreeView) then begin
    SDFilesystemTreeView1.Selected.EditText;
  end else
  if (fLastFocussed = lfListView) then begin
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
  dlg := TSDFATBootSectorPropertiesDialog.Create(self);
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
  EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckAddressBarExecute(Sender: TObject);
begin
  inherited;
  pnlAddressBar.Visible := actCheckAddressBar.Checked;
  EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckExplorerBarFoldersExecute(Sender: TObject);
begin
  inherited;
  // Important - set splitter bar visible *first* - otherwise, it goes to the
  //             *left* of the treeview, where it doesn't do much good!
  Splitter1.Visible             := actCheckExplorerBarFolders.Checked;
  SDFilesystemTreeView1.Visible := actCheckExplorerBarFolders.Checked;
  EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckToolbarExplorerExecute(Sender: TObject);
begin
  inherited;
  ToolBarExplorer.Visible := actCheckToolbarExplorer.Checked;
  EnableDisableControls();
end;

procedure TfrmExplorerMain.actCheckToolbarVolumeExecute(Sender: TObject);
begin
  inherited;
  ToolBarVolume.Visible := actCheckToolbarVolume.Checked;
  EnableDisableControls();
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

  dlg := TfrmNewDirDlg.Create(self);
  try
    if (dlg.ShowModal() = mrOk) then begin
      prevCursor    := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        ok := CreateSubDir(GetSelectedPath(Sender), dlg.DirName);
      finally
        Screen.Cursor := prevCursor;
      end;

      if ok then begin
        // Don't bother telling user - they should know what they're doing!
        //SDUMessageDlg(_('Folder created successfully'), mtInformation);
      end else begin
        SDUMessageDlg(
          SDUParamSubstitute(RS_UNABLE_TO_CREATE_FOLDER + SDUCRLF + SDUCRLF +
          RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE, [dlg.DirName]),
          mtError
          );
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
  //  WebDAVShutdown();

  SDFilesystemListView1.Filesystem := nil;
  SDFilesystemTreeView1.Filesystem := nil;

  if (fFilesystem <> nil) then begin
    fFilesystem.Mounted := False;
    fFilesystem.Free();
    fFilesystem := nil;
  end;

  if (fPartitionImage <> nil) then begin
    fPartitionImage.Mounted := False;
    fPartitionImage.Free();
    fPartitionImage := nil;
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
  EnableDisableControls();

end;

procedure TfrmExplorerMain.MountPlaintextImage(filename: String; mountReadonly: Boolean);
begin
  Dismount();

  fPartitionImage         := TSDPartitionImage_File.Create();
  TSDPartitionImage_File(fPartitionImage).Filename := filename;
  fPartitionImage.Mounted := True;

  if not (fPartitionImage.Mounted) then begin
    fPartitionImage.Free();
    fPartitionImage := nil;
  end;

  if (fPartitionImage <> nil) then begin
    fFilesystem := TSDFilesystem_FAT.Create();
    fFilesystem.PreserveTimeDateStamps := gSettings.OptPreserveTimestampsOnStoreExtract;

    fFilesystem.PartitionImage := fPartitionImage;
    try
      fFilesystem.Mounted := True;
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

      //      WebDAVStartup();
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
  item:          TSDDirItem_FAT;
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
        item := TSDDirItem_FAT.Create();
        try
          fFilesystem.GetItem(selItems[0], item);

          if item.IsFile then begin
            promptMsg := SDUParamSubstitute(
              _('Are you sure you want to delete ''%1''?'), [item.Filename]);
          end else begin
            promptMsg := SDUParamSubstitute(_(
              'Are you sure you want to remove the folder ''%1'' and all its contents?'),
              [item.Filename]);
          end;
        finally
          item.Free();
        end;

      end else begin
        promptMsg := SDUParamSubstitute(
          _('Are you sure you want to delete these %1 items?'), [selItems.Count]);
      end;

      userConfirmed := SDUConfirmYN(promptMsg);
    end;

    if userConfirmed then begin
      PerformOperation(cmDelete, True, selItems, True, '', '');

      // MS Windows Explorer doesn't display confirmation prompt, so neither
      // do we
      //if allOK then
      //  begin
      //  SDUMessageDlg(_('Deleted successfully'), mtInformation);
      //  end;

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

procedure TfrmExplorerMain.actFreeOTFENewExecute(Sender: TObject);
var
  prevMounted: DriveLetterString;
  newMounted:  DriveLetterString;
  i:           Integer;
begin
  inherited;

  prevMounted := GetFreeOTFEDLL().DrivesMounted;

  if not (GetFreeOTFEDLL().CreateFreeOTFEVolumeWizard()) then begin
    if (GetFreeOTFEDLL().LastErrorCode <> OTFE_ERR_USER_CANCEL) then begin
      SDUMessageDlg(_('LibreCrypt could not be created'), mtError);
    end;
  end else begin
    newMounted := GetFreeOTFEDLL().DrivesMounted;

    // If the "drive letters" mounted have changed, the new volume was
    // automatically mounted; setup for this
    if (newMounted <> prevMounted) then begin
      for i := 1 to length(prevMounted) do begin
        Dismount(prevmounted[i]);
      end;

      if (newMounted <> '') then begin
        PostMountGUISetup(newMounted[1]);
        //        WebDAVStartup();
      end;

      EnableDisableControls();
    end;

    SDUMessageDlg(
      _('LibreCrypt created successfully.'),
      mtInformation
      );
  end;

end;

procedure TfrmExplorerMain.actListStyleExecute(Sender: TObject);
begin
  inherited;
  SDFilesystemListView1.ViewStyle := TViewStyle(TMenuItem(Sender).Tag);
  EnableDisableControls();

end;

procedure TfrmExplorerMain.actPlaintextMountFileExecute(Sender: TObject);
begin
  inherited;

  FreeOTFEGUISetupOpenSaveDialog(SDUOpenDialog_MountPlaintextImage);

  SDUOpenDialog_MountPlaintextImage.Filter     := FILE_FILTER_FLT_PLAINTEXTIMAGES;
  SDUOpenDialog_MountPlaintextImage.DefaultExt := FILE_FILTER_DFLT_PLAINTEXTIMAGES;

  SDUOpenDialog_MountPlaintextImage.Options :=
    SDUOpenDialog_MountPlaintextImage.Options - [ofHideReadOnly];

  if SDUOpenDialog_MountPlaintextImage.Execute() then begin
    MountPlaintextImage(
      SDUOpenDialog_MountPlaintextImage.Filename,
      (ofReadOnly in SDUOpenDialog_MountPlaintextImage.Options)
      );
  end;
end;

procedure TfrmExplorerMain.actPlaintextNewExecute(Sender: TObject);
var
  volSizeDlg:        TfrmNewVolumeSize;
  mr:                Integer;
  allOK:             Boolean;
  userCancel:        Boolean;
  filename:          String;
  tmpPartitionImage: TSDPartitionImage_File;
  tmpFilesystem:     TSDFilesystem_FAT;
begin
  allOK := True;

  if allOK then begin
    allOK := SDUWarnYN(_('This function will create an UNENCRYPTED disk image.' +
      SDUCRLF + SDUCRLF +
      'If you would like to create an ENCRYPTED disk image, please click "No" below, and then select "File | New..."'
      + SDUCRLF + SDUCRLF + 'Are you sure you with to create an UNENCRYPTED disk image?'));
  end;

  // Create the plaintext image file
  if allOK then begin
    volSizeDlg := TfrmNewVolumeSize.Create(nil);
    try
      volSizeDlg.Filter     := FILE_FILTER_FLT_PLAINTEXTIMAGES;
      volSizeDlg.DefaultExt := FILE_FILTER_DFLT_PLAINTEXTIMAGES;

      mr := volSizeDlg.ShowModal;
      if (mr <> mrOk) then begin
        allOK := False;
      end else begin
        filename := volSizeDlg.Filename;
        if not (SDUCreateLargeFile(filename, volSizeDlg.VolumeSize, True, userCancel)) then begin
          if not (userCancel) then begin
            SDUMessageDlg(_('An error occured while trying to create your LibreCrypt file'),
              mtError, [mbOK], 0);
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
    tmpPartitionImage := TSDPartitionImage_File.Create();
    try
      tmpPartitionImage.Filename := filename;
      tmpPartitionImage.Mounted  := True;

      if not (tmpPartitionImage.Mounted) then begin
        SDUMessageDlg(RS_COULD_MOUNT_BUT_NOT_PARTITION, mtError);
        allOK := False;
      end else begin
        tmpFilesystem := TSDFilesystem_FAT.Create();
        try
          tmpFilesystem.PartitionImage := tmpPartitionImage;
          tmpFilesystem.Format();
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


function TfrmExplorerMain.ConfirmOperation(opType: TFExplOperation;
  srcItems: TStrings; destDir: String): Boolean;
var
  confirmMsg: WideString;
begin
  if (srcItems.Count = 1) then begin
    confirmMsg := SDUParamSubstitute(_('Are you sure you wish to %1 ''%2'' to:' +
      SDUCRLF + SDUCRLF + '  %3'), [OperationTitle(opType),
      ExtractFilename(srcItems[0]), destDir]);
  end else begin
    confirmMsg := SDUParamSubstitute(_(
      'Are you sure you wish to %1 the %2 items dropped to:' + SDUCRLF + SDUCRLF + '%3'),
      [OperationTitle(opType), srcItems.Count, destDir]);
  end;

  Result := SDUConfirmYN(confirmMsg);

end;


procedure TfrmExplorerMain.CopyTo(items: TStringList);
var
  destDir: String;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtCopy, items);
  if (destDir <> '') then begin
    CopyTo(items, destDir);
  end;

end;

procedure TfrmExplorerMain.CopyTo(items: TStringList; destDir: String);
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
  destDir: String;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtMove, items);
  if (destDir <> '') then begin
    MoveTo(items, destDir);
  end;

end;

procedure TfrmExplorerMain.MoveTo(items: TStringList; destDir: String);
begin
  PerformOperation(cmMove, True, items, True, destDir, '');
end;

procedure TfrmExplorerMain.actMapNetworkDriveExecute(Sender: TObject);
begin
  inherited;
  //  WebDAVStartup();
end;

procedure TfrmExplorerMain.actDisconnectNetworkDriveExecute(Sender: TObject);
begin
  inherited;
  //  WebDAVShutdown();
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
  dlgVolume:    TfrmPropertiesDialog_Volume;
  dlgFile:      TfrmFilePropertiesDialog;
  dlgDirectory: TfrmPropertiesDialog_Directory;
  dlgMultiple:  TfrmPropertiesDialog_Multiple;
  selItems:     TStringList;
begin
  inherited;

  selItems := TStringList.Create();
  try
    GetSelectedItems(Sender, selItems);

    if (selItems.Count = 0) then begin
      // Do nothing
    end else
    if (selItems.Count > 1) then begin
      // Show properties as single directory...
      dlgMultiple := TfrmPropertiesDialog_Multiple.Create(self);
      try
        dlgMultiple.Filesystem := fFilesystem;
        dlgMultiple.MultipleItems.Assign(selItems);
        dlgMultiple.ParentDir := GetSelectedPath(Sender);
        dlgMultiple.ShowModal();
      finally
        dlgMultiple.Free();
      end;

    end else
    if (selItems.Count = 1) then begin
      if (selItems[0] = PATH_SEPARATOR) then begin
        // Show properties as single file...
        dlgVolume := TfrmPropertiesDialog_Volume.Create(self);
        try
          dlgVolume.Filesystem := fFilesystem;
          dlgVolume.ShowModal();
        finally
          dlgVolume.Free();
        end;

      end else
      if fFilesystem.FileExists(selItems[0]) then begin
        // Show properties as single file...
        dlgFile := TfrmFilePropertiesDialog.Create(self);
        try
          dlgFile.Filesystem      := fFilesystem;
          dlgFile.PathAndFilename := selItems[0];
          dlgFile.ShowModal();
        finally
          dlgFile.Free();
        end;

      end else begin
        // Show properties as single directory...
        dlgDirectory := TfrmPropertiesDialog_Directory.Create(self);
        try
          dlgDirectory.Filesystem      := fFilesystem;
          dlgDirectory.PathAndFilename := selItems[0];
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
 //var
 //  dlg: TfrmWebDAVStatus;
begin
  inherited;
{
  dlg:= TfrmWebDAVStatus.Create(self);
  try
    dlg.WebDAVObj := WebDAVObj;
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
  //  StatusBar.Panels[STATUSBAR_PANEL_LOCATION].Style := psOwnerDraw;

  inherited;

  FInFormShow := True;
  try
    SetStatusMsg();

    SDUCenterControl(lblFolder, ccVertical);
    SDUCenterControl(edPath, ccVertical);
    SDUCenterControl(pbGo, ccVertical);

    SDFilesystemTreeView1.Constraints.MinWidth := 60; // Like MS Windows Explorer

    // Note: If Settings.OptStoreLayout is TRUE, then the layout would have
    //       been deleted from the settings the last time settings were
    //       saved - so this should be using the defaults
    SDUSetFormLayout(self, gSettings.OptMainWindowLayout);

    ToolBarVolume.Visible         := gSettings.OptShowToolbarVolume;
    ToolBarExplorer.Visible       := gSettings.OptShowToolbarExplorer;
    pnlAddressBar.Visible         := gSettings.OptShowAddressBar;
    // Important: Must turn these next two on/off in this order!
    Splitter1.Visible             := (gSettings.OptShowExplorerBar <> ebNone);
    SDFilesystemTreeView1.Visible := (gSettings.OptShowExplorerBar = ebFolders);
    StatusBar_Status.Visible      := gSettings.OptShowStatusbar;
    StatusBar_Hint.Visible        := False;

    if (gSettings.OptExplorerBarWidth > 0) then
      SDFilesystemTreeView1.Width := gSettings.OptExplorerBarWidth;

    SDFilesystemListView1.Layout := gSettings.OptListViewLayout;

    EnableDisableControls();
  finally
    FInFormShow := False;
  end;

end;

function TfrmExplorerMain.DefaultStatusMsg(): String;
var
  fsFreeSpace:     ULONGLONG;
  //  fsSize: ULONGLONG;
  dispMsg:         String;
  selectedItem:    TSDDirItem;
  size:            String;
  fileType:        String;
  date:            String;
  sizeKB:          Integer;
  itemsInSelected: Integer;
begin
  // Fallback msg; anything other than ''
  dispMsg := _('Ready.');

  if not (Mounted()) then begin
    dispMsg := _('Ready to open Container...');
  end else begin
    if (SDFilesystemListView1.SelCount <= 0) then begin
      fsFreeSpace := fFilesystem.FreeSpace;
      //    fsSize := fFilesystem.Size;

      itemsInSelected := SDFilesystemListView1.Items.Count;
      if SDFilesystemListView1.ShowParentDir then begin
        Dec(itemsInSelected);
      end;

      dispMsg := SDUParamSubstitute(_('%1 objects (Disk free space: %2)'), [itemsInSelected,
        //                                   SDUFormatAsBytesUnits(fsFreeSpace),
        // MS Explorer only displays to 1 decimal place
        SDUFormatUnits(fsFreeSpace, SDUUnitsStorageToTextArr(),
        UNITS_BYTES_MULTIPLIER, 1)]);


  {
      dispMsg := SDUParamSubstitute(
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
    end else
    if (SDFilesystemListView1.SelCount = 1) then begin
      selectedItem := SDFilesystemListView1.DirItem[SDFilesystemListView1.SelectedIdx];
      if selectedItem.IsFile then begin
        // Always display size in KB
        sizeKB := (selectedItem.Size div BYTES_IN_KILOBYTE);
        // Round up, like MS Explorer does...
        if ((selectedItem.Size mod BYTES_IN_KILOBYTE) > 0) then begin
          sizeKB := sizeKB + 1;
        end;
        // Format with thousands separator and KB units
        size := SDUIntToStrThousands(sizeKB) + ' ' + UNITS_STORAGE_KB;

        fileType := SDUGetFileType_Description(selectedItem.Filename);
        date     := DateTimeToStr(TimeStampToDateTime(selectedItem.TimestampLastModified));

        dispMsg := SDUParamSubstitute(_('Type: %1 Date Modified: %2 Size: %3'),
          [fileType, date, size]);
      end else begin
        dispMsg := SDUParamSubstitute(_('%1 object selected'),
          [SDFilesystemListView1.SelCount]);
      end;

    end else begin
      dispMsg := SDUPluralMsg(SDFilesystemListView1.SelCount,
        [SDUParamSubstitute(_('%1 object selected'), [SDFilesystemListView1.SelCount]),
        SDUParamSubstitute(_('%1 objects selected'), [SDFilesystemListView1.SelCount])]);
    end;
  end;

  Result := dispMsg;
end;

function TfrmExplorerMain.SelectedItemsStatusMsg(): String;
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

  sumAllItems := (SDFilesystemListView1.SelCount <= 0);

  totalSize        := 0;
  onlyDirsSelected := True;
  for i := 0 to (SDFilesystemListView1.Items.Count - 1) do begin
    currItem := SDFilesystemListView1.DirItem[i];

    if (SDFilesystemListView1.Items[i].Selected and currItem.IsFile) then begin
      onlyDirsSelected := False;
    end;

    if ((SDFilesystemListView1.Items[i].Selected or sumAllItems) and currItem.IsFile) then begin
      totalSize := totalSize + currItem.Size;
    end;

  end;

  if ((SDFilesystemListView1.SelCount > 0) and onlyDirsSelected) then begin
    // Do nothing - already set to ''
  end else begin
    Result := SDUFormatUnits(totalSize, SDUUnitsStorageToTextArr(),
      UNITS_BYTES_MULTIPLIER, 2);
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  SetStatusMsg();
end;

procedure TfrmExplorerMain.SDFilesystemTreeView1Change(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
begin
  if FInRefreshing then begin
    exit;
  end;

  edPath.Text := SDFilesystemTreeView1.PathToNode(Node);

  if (SDFilesystemTreeView1.Selected <> nil) then begin
    // Delete all after the current point
    for i := (FNavigateHistory.Count - 1) downto (FNavigateIdx + 1) do begin
      FNavigateHistory.Delete(i);
    end;

    FNavigateHistory.Append(SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected));

    while (FNavigateHistory.Count > MAX_BACKFORWARD_NAV_TOTALITEMS) do begin
      FNavigateHistory.Delete(0);
    end;

    FNavigateIdx := (FNavigateHistory.Count - 1);
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

  EnableDisableControls();
  SetStatusMsg();
end;

procedure TfrmExplorerMain.actNavigateBackExecute(Sender: TObject);
begin
  if (FNavigateIdx > 0) then begin
    NavigateToHistoryIdx(FNavigateIdx - 1);
  end;

end;

procedure TfrmExplorerMain.actNavigateForwardExecute(Sender: TObject);
begin
  if (FNavigateIdx < (FNavigateHistory.Count - 1)) then begin
    NavigateToHistoryIdx(FNavigateIdx + 1);
  end;

end;

procedure TfrmExplorerMain.actOptionsExecute(Sender: TObject);
var
  dlg: TfrmExplorerOptions;
begin
  inherited;

  // Odd man out...
  gSettings.OptShowToolbarVolume   := ToolBarVolume.Visible;
  gSettings.OptShowToolbarExplorer := ToolBarExplorer.Visible;

  dlg := TfrmExplorerOptions.Create(self);
  try
    //    dlg.OTFEFreeOTFEBase := fOTFEFreeOTFEBase;
    if (dlg.ShowModal() = mrOk) then begin
      ReloadSettings();
      actRefreshExecute(nil);
      // Note: actRefreshExecute calls EnableDisableControls(...) anyway
      //EnableDisableControls();
    end;

  finally
    dlg.Free();
  end;

end;

procedure TfrmExplorerMain.actOverwriteFileExecute(Sender: TObject);
var
  overwriteItem: String;
begin
  inherited;

  SDUOpenDialog_Overwrite.Options :=
    SDUOpenDialog_Overwrite.Options + [ofHideReadOnly,
    // i.e. Set to TRUE to remove the readonly checkbox
    ofPathMustExist, ofFileMustExist
    // ofAllowMultiSelect
    ];
  SDUOpenDialog_Overwrite.Options := SDUOpenDialog_Overwrite.Options + [ofDontAddToRecent];
  if SDUOpenDialog_Overwrite.Execute then begin
    overwriteItem := SDUOpenDialog_Overwrite.Filename;

    if SDUWarnYN(SDUParamSubstitute(_('This will overwrite:' + SDUCRLF +
      SDUCRLF + '%1' + SDUCRLF + SDUCRLF + 'Are you sure you want to do this?'),
      [overwriteItem])) then begin
      if (fShredderObj.DestroyFileOrDir(overwriteItem, False, False, False) = srError) then begin
        SDUMessageDlg(_('Overwrite failed'), mtError);
      end;
    end;
  end;

end;

procedure TfrmExplorerMain.actOverwriteDirExecute(Sender: TObject);
var
  overwriteItem: String;
begin
  inherited;

  if SDUSelectDirectory(self.Handle, _('Folder to overwrite:'), '', overwriteItem) then begin
    if SDUWarnYN(SDUParamSubstitute(_('This will overwrite:' + SDUCRLF +
      SDUCRLF + '%1' + SDUCRLF + SDUCRLF + 'and everything contained within it.' +
      SDUCRLF + SDUCRLF + 'Are you sure you want to do this?'), [overwriteItem])) then begin
      if (fShredderObj.DestroyFileOrDir(overwriteItem, False, False, False) = srError) then begin
        SDUMessageDlg(_('Overwrite failed'), mtError);
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
    prev.Assign(FNavigateHistory);

    SDFilesystemTreeView1.GoToPath(FNavigateHistory[idx], False);

    FNavigateHistory.Assign(prev);
    FNavigateIdx := idx;

  finally
    prev.Free();
  end;

  // EnableDisableControls(...) call required here as FNavigateHistory
  // was updated
  EnableDisableControls();

end;

procedure TfrmExplorerMain.SDFilesystemTreeView1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  hitTestTreeNode: TTreeNode;
begin
  inherited;

  hitTestTreeNode := TTreeView(Sender).GetNodeAt(MousePos.X, MousePos.Y);

  if (hitTestTreeNode <> nil) then begin
    mnuTreeViewExpand.Visible   :=
      ((SDFilesystemTreeView1.SelectionCount = 1) and not
      (SDFilesystemTreeView1.Selected.Expanded));
    mnuTreeViewCollapse.Visible :=
      ((SDFilesystemTreeView1.SelectionCount = 1) and
      SDFilesystemTreeView1.Selected.Expanded);

    pmTreeView.Tag := Integer(hitTestTreeNode);

    // This *should* enable/disable controls taking into account the node the
    // context menu is being displayed for.
    // But since all nodes are currently treated the same, we can get away
    // with just calling EnableDisableControls(...) for now
    // xxx - lplp - fix this
    //EnableDisableControls(FHitContextTreeNode);
    EnableDisableControls();

  end else begin
    // No node under mouse - do not display popup
    Handled := True;
  end;

end;


procedure TfrmExplorerMain.SDFilesystemListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  inherited;
  EnableDisableControls();

end;

procedure TfrmExplorerMain.SDFilesystemBothViewStartDrag(Sender: TObject;
  var DragObject: TDragObject);
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

procedure TfrmExplorerMain.SDFilesystemTreeView1DragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  droppedOnNode: TTreeNode;
begin
  inherited;

  Accept := False;

  if ((Source = SDFilesystemTreeView1) or (Source = SDFilesystemListView1)) then begin
    // Only accept inter-window drag'n'drop onto the treeview if the drop in on
    // top of a node
    // NOTE: This is slightly different from drag'n'drop from MS Windows
    //       Explorer
    droppedOnNode := SDFilesystemTreeView1.GetNodeAt(X, Y);
    Accept        := (droppedOnNode <> nil);
  end;

  SetDragCursor();

end;

procedure TfrmExplorerMain.SDFilesystemListView1DblClick(Sender: TObject);
begin
  inherited;

  if ((SDFilesystemListView1.SelCount = 1) and
    SDFilesystemListView1.DirItemSelected.IsFile) then
  begin
    actExtractExecute(Sender);
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1DragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;

  Accept := False;

  if (
    // Don't support dragging from treeview -> listview - trying to do this causes
    // the treeview to change which node is selected
    //      (Source = SDFilesystemTreeView1) or
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
procedure TfrmExplorerMain.SDFilesystemBothViewEndDrag(Sender, Target: TObject;
  X, Y: Integer);
var
  droppedOnNode: TTreeNode;
  droppedOnItem: TListItem;
  selItems:      TStringList;
  droppedOnPath: String;
  op:            TFExplOperation;
begin
  inherited;

  if ((Target = SDFilesystemTreeView1) or (Target = SDFilesystemListView1)) then begin
    // Determine path dropped on
    droppedOnPath := '';
    if (Target = SDFilesystemTreeView1) then begin
      droppedOnNode := SDFilesystemTreeView1.GetNodeAt(X, Y);
      if (droppedOnNode <> nil) then begin
        droppedOnPath := SDFilesystemTreeView1.PathToNode(droppedOnNode);
      end;

    end else begin
      if (Target = SDFilesystemListView1) then begin
        droppedOnPath := SDFilesystemListView1.Path;

        droppedOnItem := SDFilesystemListView1.GetItemAt(X, Y);
        if (droppedOnItem <> nil) then begin
          if SDFilesystemListView1.DirItem[droppedOnItem.Index].IsDirectory then begin
            droppedOnPath := IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) +
              SDFilesystemListView1.DirItem[droppedOnItem.Index].Filename;
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
  invalidCharsForDisplay: String;
  filenameNoDots: WideString;
  i: Integer;
begin
  Result := fFilesystem.IsValidFilename(filename);

  if not (Result) then begin
    filenameNoDots := trim(StringReplace(filename, '.', '', [rfReplaceAll]));

    if (trim(filename) = '') then begin
      // MS Windows doesn't bother telling user, so we don't either
    end else
    if (length(filenameNoDots) <= 0) then begin
      // MS Windows doesn't bother telling user, so we don't either
    end else begin
      invalidCharsForDisplay := '';
      for i := 1 to length(FAT_INVALID_FILENAME_CHARS) do begin
        // Cast to prevent compiler warning
        invalidCharsForDisplay := invalidCharsForDisplay + ' ' +
          Char(FAT_INVALID_FILENAME_CHARS[i]);
      end;

      SDUMessageDlg(SDUParamSubstitute(
        _('A file name cannot contain any of the following characters:' +
        SDUCRLF + SDUCRLF + '%1'), [invalidCharsForDisplay]), mtError);
    end;
  end;

end;

procedure TfrmExplorerMain.SDFilesystemListView1Edited(Sender: TObject;
  Item: TListItem; var S: String);
var
  renamedOK: Boolean;
begin
  inherited;

  renamedOK := False;

  // Ditch whitespace...
  S := trim(S);

  if (S = '') then begin
    // Do nothing - as MS Windows Explorer does
  end else begin
    // Tack on previous filename extension, if file extensions was hidden
    if SDFilesystemListView1.HideKnownFileExtns then begin
      if (SDFilesystemListView1.DisplayedName[Item.Index] <>
        SDFilesystemListView1.DirItem[Item.Index].Filename) then begin
        S := S + ExtractFileExt(SDFilesystemListView1.DirItem[Item.Index].Filename);
      end;
    end;

    if (SDFilesystemListView1.DirItem[Item.Index].Filename = S) then begin
      // Do nothing - renaming file back to what it originally was
    end else begin
      if IsFilenameValid(S) then begin
        // Carry out the rename operation...
        renamedOK := fFilesystem.MoveFileOrDir(
          IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) +
          SDFilesystemListView1.DirItem[Item.Index].Filename,
          IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) + S);
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

procedure TfrmExplorerMain.SDFilesystemTreeView1Edited(Sender: TObject;
  Node: TTreeNode; var S: String);
var
  renamedOK: Boolean;
begin
  inherited;

  renamedOK := False;

  // Ditch whitespace...
  S := trim(S);

  if (ExtractFilename(SDFilesystemTreeView1.PathToNode(Node)) = S) then begin
    // Do nothing - renaming file back to what it originally was
  end else
  if (S = '') then begin
    // Do nothing - leave as it was
  end else begin
    if IsFilenameValid(S) then begin
      // Carry out the rename operation...
      renamedOK := fFilesystem.MoveFileOrDir(SDFilesystemTreeView1.PathToNode(Node),
        IncludeTrailingPathDelimiter(
        ExtractFilePath(SDFilesystemTreeView1.PathToNode(Node))) + S);
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
  PostMessage(self.Handle, WM_FREEOTFE_EXPLORER_REFRESH, 0, 0);
end;

procedure TfrmExplorerMain.SDFilesystemListView1Enter(Sender: TObject);
begin
  inherited;
  fLastFocussed := lfListView;
  EnableDisableControls();
end;

procedure TfrmExplorerMain.SDFilesystemTreeView1Enter(Sender: TObject);
begin
  inherited;
  fLastFocussed := lfTreeView;
  EnableDisableControls();
end;

 // This is called when a MS Windows Explorer drag/drop operation ends by
 // dropping on the treeview
procedure TfrmExplorerMain.SDUDropFilesTreeViewItemsDrop(Sender: TObject;
  DropItems: TStringList; DropPoint: TPoint);
var
  droppedOnNode: TTreeNode;
  importToPath:  WideString;
  //  op: TFExplOperation;
begin
  inherited;

  // If the user dropped on a tree node, import to that path. Otherwise,
  // import to the currently selected node
  droppedOnNode := SDFilesystemTreeView1.GetNodeAt(DropPoint.X, DropPoint.Y);
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
procedure TfrmExplorerMain.SDUDropFilesListViewItemsDrop(Sender: TObject;
  DropItems: TStringList; DropPoint: TPoint);
var
  droppedOnItem: TListItem;
  importToPath:  WideString;
  //  op: TFExplOperation;
begin
  inherited;

  droppedOnItem := SDFilesystemListView1.GetItemAt(DropPoint.X, DropPoint.Y);

  importToPath := SDFilesystemListView1.Path;
  if (droppedOnItem <> nil) then begin
    if SDFilesystemListView1.DirItem[droppedOnItem.Index].IsDirectory then begin
      importToPath := IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) +
        SDFilesystemListView1.DirItem[droppedOnItem.Index].Filename;
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


procedure TfrmExplorerMain.RecaptionToolbarAndMenuIcons();
begin
  inherited;

  // Change toolbar captions to shorter captions
  tbbNew.Caption       := RS_TOOLBAR_CAPTION_NEW;
  tbbMountFile.Caption := RS_TOOLBAR_CAPTION_MOUNTFILE;
  tbbDismount.Caption  := RS_TOOLBAR_CAPTION_DISMOUNT;

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

  mnuToolbarStoreFile.Caption := RS_TOOLBAR_MNU_CAPTION_STORE_FILE;
  mnuToolbarStoreDir.Caption  := RS_TOOLBAR_MNU_CAPTION_STORE_DIR;

  // Toolbar hints...
  tbbNew.Hint       := RS_TOOLBAR_HINT_NEW;
  tbbMountFile.Hint := RS_TOOLBAR_HINT_MOUNTFILE;
  tbbDismount.Hint  := RS_TOOLBAR_HINT_DISMOUNT;

  tbbNavigateBack.Hint           := RS_TOOLBAR_HINT_BACK;
  tbbNavigateForward.Hint        := RS_TOOLBAR_HINT_FORWARD;
  tbbUp.Hint                     := RS_TOOLBAR_HINT_UP;
  tbbMoveTo.Hint                 := RS_TOOLBAR_HINT_MOVETO;
  tbbCopyTo.Hint                 := RS_TOOLBAR_HINT_COPYTO;
  tbbDelete.Hint                 := RS_TOOLBAR_HINT_DELETE;
  tbbViews.Hint                  := RS_TOOLBAR_HINT_VIEWS;
  tbbExtract.Hint                := RS_TOOLBAR_HINT_EXTRACT;
  tbbStore.Hint                  := RS_TOOLBAR_HINT_STORE;
  tbbItemProperties.Hint         := RS_TOOLBAR_HINT_ITEMPROPERTIES;
  tbbExplorerBarFolders.Hint     := RS_TOOLBAR_HINT_EXPLORERBARFOLDERS;
  tbbMapNetworkDrive.Hint        := RS_TOOLBAR_HINT_MAP_DRIVE;
  tbbDisconnectNetworkDrive.Hint := RS_TOOLBAR_HINT_DISCONNECT;

  mnuToolbarStoreFile.Hint := RS_TOOLBAR_MNU_HINT_STORE_FILE;
  mnuToolbarStoreDir.Hint  := RS_TOOLBAR_MNU_HINT_STORE_DIR;

end;

procedure TfrmExplorerMain.SetIconListsAndIndexes();
begin
  inherited;

  if gSettings.OptToolbarVolumeLarge then begin
    ToolbarVolume.Images := ilToolbarIcons_Large;
       {
    tbbNew.ImageIndex       := FIconIdx_Large_New;
    tbbMountFile.ImageIndex := FIconIdx_Large_MountFile;
    tbbDismount.ImageIndex  := FIconIdx_Large_Dismount;
    }
  end else begin
    ToolbarVolume.Images := ilToolbarIcons_Small;
     {
    tbbNew.ImageIndex       := FIconIdx_Small_New;
    tbbMountFile.ImageIndex := FIconIdx_Small_MountFile;
    tbbDismount.ImageIndex  := FIconIdx_Small_Dismount;
    }
  end;

  if gSettings.OptToolbarExplorerLarge then begin
    ToolbarExplorer.Images := ilToolbarIcons_Large;
    (*
    tbbNavigateBack.ImageIndex           := FIconIdx_Large_Back;
    tbbNavigateForward.ImageIndex        := FIconIdx_Large_Forward;
    tbbUp.ImageIndex                     := FIconIdx_Large_Up;
    tbbMoveTo.ImageIndex                 := FIconIdx_Large_MoveTo;
    tbbCopyTo.ImageIndex                 := FIconIdx_Large_CopyTo;
    tbbDelete.ImageIndex                 := FIconIdx_Large_Delete;
    tbbViews.ImageIndex                  := FIconIdx_Large_Views;
    tbbExtract.ImageIndex                := FIconIdx_Large_Extract;
    tbbStore.ImageIndex                  := FIconIdx_Large_Store;
    tbbItemProperties.ImageIndex         := FIconIdx_Large_ItemProperties;
    tbbExplorerBarFolders.ImageIndex     := FIconIdx_Large_Folders;
    tbbMapNetworkDrive.ImageIndex        := FIconIdx_Large_MapNetworkDrive;
    tbbDisconnectNetworkDrive.ImageIndex := FIconIdx_Large_DisconnectNetworkDrive;
    *)
  end else begin
    ToolbarExplorer.Images := ilToolbarIcons_Small;
            (*
    tbbNavigateBack.ImageIndex           := FIconIdx_Small_Back;
    tbbNavigateForward.ImageIndex        := FIconIdx_Small_Forward;
    tbbUp.ImageIndex                     := FIconIdx_Small_Up;
    tbbMoveTo.ImageIndex                 := FIconIdx_Small_MoveTo;
    tbbCopyTo.ImageIndex                 := FIconIdx_Small_CopyTo;
    tbbDelete.ImageIndex                 := FIconIdx_Small_Delete;
    tbbViews.ImageIndex                  := FIconIdx_Small_Views;
    tbbExtract.ImageIndex                := FIconIdx_Small_Extract;
    tbbStore.ImageIndex                  := FIconIdx_Small_Store;
    tbbItemProperties.ImageIndex         := FIconIdx_Small_ItemProperties;
    tbbExplorerBarFolders.ImageIndex     := FIconIdx_Small_Folders;
    tbbMapNetworkDrive.ImageIndex        := FIconIdx_Small_MapNetworkDrive;
    tbbDisconnectNetworkDrive.ImageIndex := FIconIdx_Small_DisconnectNetworkDrive;
    *)
  end;

end;

procedure TfrmExplorerMain.SetupToolbarFromSettings();

  procedure SortToolbar(processToolbar: TToolbar; displayToolbar: Boolean);
  var
    toolbarWidth: Integer;
    i:            Integer;
  begin
    processToolbar.Height       := processToolbar.Images.Height + TOOLBAR_ICON_BORDER;
    processToolbar.ButtonHeight := processToolbar.Images.Height;
    processToolbar.ButtonWidth  := processToolbar.Images.Width;

//    if displayToolbar then begin
//      // Turning captions on can cause the buttons to wrap if the window isn't big
//      // enough.
//      // This looks pretty poor, so resize the window if it's too small
//      toolbarWidth := 0;
//      for i := 0 to (processToolbar.ButtonCount - 1) do begin
//        toolbarWidth := toolbarWidth + processToolbar.Buttons[i].Width;
//      end;
//      if (toolbarWidth > self.Width) then begin
//        self.Width           := toolbarWidth;
//        processToolbar.Width := self.Width;
//      end;
//      // Adjusting the width to the sum of the toolbar buttons doens't make it wide
//      // enough to prevent wrapping (presumably due to window borders); nudge the
//      // size until it does
//      // (Crude, but effective)
//      while (processToolbar.RowCount > 1) do begin
//        self.Width           := self.Width + 10;
//        processToolbar.Width := self.Width;
//      end;
//    end;

  end;

var
  tmpToolBarVolumeVisible:   Boolean;
  tmpToolBarExplorerVisible: Boolean;
begin
  SetIconListsAndIndexes();

  // Visible/invisible are set in EnableDisableControls()

  // Toolbar captions...
  if gSettings.OptToolbarVolumeLarge then begin
    ToolBarVolume.ShowCaptions := gSettings.OptToolbarVolumeCaptions;
  end else begin
    ToolBarVolume.ShowCaptions := False;
  end;

  // Toolbar captions...
  if gSettings.OptToolbarExplorerLarge then begin
    ToolBarExplorer.ShowCaptions := gSettings.OptToolbarExplorerCaptions;
  end else begin
    ToolBarExplorer.ShowCaptions := False;
  end;


  tmpToolBarExplorerVisible := ToolBarExplorer.Visible;
  ToolBarExplorer.Visible   := False;
  SortToolbar(ToolBarVolume, tmpToolBarExplorerVisible);

  tmpToolBarVolumeVisible := ToolBarVolume.Visible;
  ToolBarVolume.Visible   := False;
  SortToolbar(ToolBarExplorer, tmpToolBarVolumeVisible);

  ToolBarVolume.Visible   := tmpToolBarVolumeVisible;
  ToolBarExplorer.Visible := tmpToolBarExplorerVisible;

end;


procedure TfrmExplorerMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;

  // We save settings here, in case the user changed any of the toolbar
  // visible/invisible, etc settings
  // Note that if the user hasn't configured a location to save their settings,
  // this save will have no effect
  gSettings.OptMainWindowLayout    := SDUGetFormLayout(self);
  gSettings.OptExplorerBarWidth    := SDFilesystemTreeView1.Width;
  gSettings.OptListViewLayout      := SDFilesystemListView1.Layout;
  gSettings.OptShowToolbarVolume   := ToolBarVolume.Visible;
  gSettings.OptShowToolbarExplorer := ToolBarExplorer.Visible;
  gSettings.OptShowAddressBar      := pnlAddressBar.Visible;
  gSettings.OptShowExplorerBar     := ebNone;
  if SDFilesystemTreeView1.Visible then begin
    gSettings.OptShowExplorerBar := ebFolders;
  end;
  gSettings.OptShowStatusbar := (StatusBar_Status.Visible or StatusBar_Hint.Visible);

  gSettings.Save();


  //  fOTFEFreeOTFEBase.Free();

end;

procedure TfrmExplorerMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  Dismount();
end;

procedure TfrmExplorerMain.FormCreate(Sender: TObject);
var
  goIcon:           Graphics.TIcon;
  goBitmap:         Graphics.TBitmap;
  //  sysMagGlassIcon: TIcon;
  settingsFilename: String;
begin
  gSettings := TExplorerSettings.Create();
  SetFreeOTFEType(TOTFEFreeOTFEDLL);
  //  fOTFEFreeOTFEBase := TOTFEFreeOTFEDLL.Create();


  FInFormShow   := False;
  FInRefreshing := False;



  fShredderObj := TShredder.Create(nil);
  fWebDAVObj   := TFreeOTFEExplorerWebDAV.Create(nil);
  FMappedDrive := #0;


  CommonSettingsObj := gSettings;
  if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFilename) then begin
    settingsFilename         := SDURelativePathToAbsolute(settingsFilename);
    gSettings.CustomLocation := settingsFilename;
  end;
  gSettings.Load();

  inherited;

  fPartitionImage := nil;
  fFilesystem     := nil;

  FNavigateHistory := TStringList.Create();
  FNavigateIdx     := -1;

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
  // As a consequence of this, EnableDisableControls(...) must *explicitly* set
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
  goIcon   := TIcon.Create();
  goBitmap := Graphics.TBitmap.Create();
  try
    if SDULoadDLLIcon(DLL_SHELL32, True, DLL_SHELL32_GO_ICON, goIcon) then begin
      // Must specify 16x16 explicitly as the icon loaded is always 32x32
      SDUConvertIconToBitmap(goIcon, 16, 16, goBitmap, nil);
      pbGO.Glyph.Assign(goBitmap);
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
  ToolBarExplorer.Visible       := False;
  Splitter1.Visible             := False;
  SDFilesystemTreeView1.Visible := False;
  StatusBar_Status.Visible      := False;
  StatusBar_Hint.Visible        := False;

  // Give the user a clue
  ToolBarVolume.ShowHint   := True;
  ToolBarExplorer.ShowHint := True;

  ToolBarVolume.Indent   := 5;
  ToolBarExplorer.Indent := 5;

  // Cosmetic tweak - when the user rightclicks on a node, this will make it
  // appear selected while the context menu is displayed - BUT WITHOUT FIRING
  // A TREEVIEW OnChange EVENT. After the context menu disappears, the
  // previously selected node appears selected
  SDFilesystemTreeView1.RightClickSelect := True;

end;

procedure TfrmExplorerMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(gSettings);
  //   FreeAndNil(fOtfeFreeOtfeBase);



  FreeAndNil(fShredderObj);
  FreeAndNil(fWebDAVObj);
  //  WebDAVShutdown();


  FNavigateHistory.Free();

end;

procedure TfrmExplorerMain.FormResize(Sender: TObject);
begin
  SizeStatusBar();
end;

procedure TfrmExplorerMain.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  textHeight: Integer;
begin
  if ((Panel.Index <> STATUSBAR_PANEL_LOCATION) or (Panel.Text = '')) then begin
    inherited;
  end else begin
    // +2 here to "center" it a bit more
    ImageList_StatusBar.Draw(StatusBar.Canvas, (Rect.Left + 2), Rect.Top, IMGLIST_IDX_FREEOTFE);

    // +5 here to give a bit of a gap between the image and text
    textHeight := StatusBar.Canvas.TextHeight(Panel.Text);
    StatusBar.Canvas.TextOut(
      (Rect.Left + ImageList_StatusBar.Width + 5),
      Rect.Top + (((Rect.Bottom - Rect.Top) - textHeight) div 2),
      Panel.Text
      );
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
 //          otherwise
function TfrmExplorerMain.PromptToReplace(filename: String;
  existingSize: ULONGLONG; existingDateTime: TDateTime; newSize: ULONGLONG;
  newDateTime: TDateTime): Boolean;
begin
  Result := SDUConfirmYN(SDUParamSubstitute(
    _('This folder already contains a file named ''%1''.' + SDUCRLF + SDUCRLF +
    'Would you like to replace the existing file' + SDUCRLF + SDUCRLF + '  %2' +
    SDUCRLF + '  modified: %3' + SDUCRLF + SDUCRLF + 'with this one?' + SDUCRLF +
    SDUCRLF + '  %4' + SDUCRLF + '  modified: %5'), [ExtractFilename(filename),
    SDUFormatAsBytesUnits(existingSize), DateTimeToStr(existingDateTime),
    SDUFormatAsBytesUnits(newSize), newDateTime]));

end;

// Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
function TfrmExplorerMain.PromptToReplaceYYANC(filename: String;
  existingSize: ULONGLONG; existingDateTime: TDateTime; newSize: ULONGLONG;
  newDateTime: TDateTime): Integer;
begin
  Result := SDUMessageDlg(SDUParamSubstitute(
    _('This folder already contains a file named ''%1''.' + SDUCRLF + SDUCRLF +
    'Would you like to replace the existing file' + SDUCRLF + SDUCRLF + '  %2' +
    SDUCRLF + '  modified: %3' + SDUCRLF + SDUCRLF + 'with this one?' + SDUCRLF +
    SDUCRLF + '  %4' + SDUCRLF + '  modified: %5'), [ExtractFilename(filename),
    SDUFormatAsBytesUnits(existingSize), DateTimeToStr(existingDateTime),
    SDUFormatAsBytesUnits(newSize), newDateTime]), mtConfirmation,
    [mbYes, mbNo, mbCancel, mbYesToAll], 0);

end;

// Returns one of: mrYes, mrYesToAll, mrNo, mrCancel
function TfrmExplorerMain.PromptToReplaceYYANC(srcIsMountedFSNotLocalFS: Boolean;
  srcPathAndFilename: String; destIsMountedFSNotLocalFS: Boolean;
  destPathAndFilename: String): Integer;

  procedure GetFileDetails(isMountedFSNotLocalFS: Boolean; pathAndFilename: String;
  var Size: ULONGLONG; var DateTime: TDateTime);
  var
    item:           TSDDirItem_FAT;
    CreationTime:   TFileTime;
    LastAccessTime: TFileTime;
    LastWriteTime:  TFileTime;
  begin
    if isMountedFSNotLocalFS then begin
      item := TSDDirItem_FAT.Create();
      try
        if fFilesystem.GetItem_FAT(pathAndFilename, item) then begin
          Size     := item.Size;
          DateTime := TimeStampToDateTime(item.TimestampLastModified);
        end;
      finally
        item.Free();
      end;

    end else begin
      Size := SDUGetFileSize(pathAndFilename);

      DateTime := Now();
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
  GetFileDetails(
    srcIsMountedFSNotLocalFS,
    srcPathAndFilename,
    srcSize,
    srcDateTime
    );
  GetFileDetails(
    destIsMountedFSNotLocalFS,
    destPathAndFilename,
    destSize,
    destDateTime
    );

  Result := PromptToReplaceYYANC(ExtractFilename(srcPathAndFilename), destSize,
    destDateTime, srcSize, srcDateTime);

end;

function TfrmExplorerMain._StoreFile(path: WideString; fileToStore: String): Boolean;
var
  item:       TSDDirItem_FAT;
  fileStream: TFileStream;
begin
  item       := TSDDirItem_FAT.Create();
  fileStream := TFileStream.Create(fileToStore, (fmOpenRead or fmShareDenyNone));
  try
    GetLocalFileDetails(fileToStore, item);

    item.FilenameDOS := '';    // Set to '' to autogenerate DOS 8.3 filename
    item.IsFile      := True;  // *VERY* IMPORTANT!!!

    Result := fFilesystem.StoreFileOrDir(path, item, fileStream);
  finally
    fileStream.Free();
    item.Free();
  end;

end;

procedure TfrmExplorerMain.tbbWithDropDownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TToolButton(Sender).CheckMenuDropdown();

end;

procedure TfrmExplorerMain.PromptForAndImportDir(importToPath: WideString);
var
  srcPathStr: String;
  stlPath:    TStringList;
begin
  if SDUSelectDirectory(self.Handle, _('Folder to store:'), '', srcPathStr) then begin
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

 //function TfrmExplorerMain.OTFEFreeOTFE(): TOTFEFreeOTFEDLL;
 //begin
 //  Result := TOTFEFreeOTFEDLL(fOTFEFreeOTFEBase);
 //end;

function TfrmExplorerMain.Mounted(): Boolean;
begin
  Result := False;

  if (fFilesystem <> nil) then begin
    Result := fFilesystem.Mounted;
  end;

end;


function TfrmExplorerMain.IsSenderTreeviewContextMenu(Sender: TObject): Boolean;
var
  currMenuItem: TMenuItem;
begin
  Result := False;

  if (Sender is TMenuItem) then begin
    currMenuItem := TMenuItem(Sender);
    while (currMenuItem <> nil) do begin
      if (currMenuItem.Owner = pmTreeView) then begin
        Result := True;
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
    popupNode := TTreeNode(pmTreeView.Tag);
    targets.Add(SDFilesystemTreeView1.PathToNode(popupNode));
  end else
  if (fLastFocussed = lfTreeView) then begin
    targets.Add(SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected));
  end else
  if (fLastFocussed = lfListView) then begin
    for i := 0 to (SDFilesystemListView1.Items.Count - 1) do begin
      if SDFilesystemListView1.Items[i].Selected then begin
        targets.Add(IncludeTrailingPathDelimiter(SDFilesystemListView1.Path) +
          SDFilesystemListView1.DirItem[i].Filename);
      end;
    end;
  end;

end;


 // Return the full path of the most recently touched/selected item
 // e.g. In the case of the treeview popup menu, the path of the item the
 // context menu was invoked for, otherwise either the treeview or listview's
 // path 
function TfrmExplorerMain.GetSelectedPath(Sender: TObject): String;
var
  popupNode:          TTreeNode;
  currMenuItem:       TMenuItem;
  useContextMenuNode: Boolean;
begin
  useContextMenuNode := False;
  if (Sender is TMenuItem) then begin
    currMenuItem := TMenuItem(Sender);
    while (currMenuItem <> nil) do begin
      if (currMenuItem.Owner = pmTreeView) then begin
        useContextMenuNode := True;
        break;
      end;
      currMenuItem := currMenuItem.Parent;
    end;
  end;

  if useContextMenuNode then begin
    popupNode := TTreeNode(pmTreeView.Tag);
    Result    := SDFilesystemTreeView1.PathToNode(popupNode);
  end else
  if (fLastFocussed = lfTreeView) then begin
    Result := SDFilesystemTreeView1.PathToNode(SDFilesystemTreeView1.Selected);
  end else
  if (fLastFocussed = lfListView) then begin
    Result := SDFilesystemListView1.Path;
  end;

end;

function TfrmExplorerMain.HandleCommandLineOpts_Mount(): eCmdLine_Exit;
var
  preMounted:  DriveLetterString;
  postMounted: DriveLetterString;
begin
  Result := ceSUCCESS;
  if SDUCommandLineSwitch(CMDLINE_MOUNT) then begin

    preMounted := GetFreeOTFEDLL().DrivesMounted;

    Result := inherited HandleCommandLineOpts_Mount();

    postMounted := GetFreeOTFEDLL().DrivesMounted;

    if ((Result = ceSUCCESS) and (preMounted <> postMounted) and
      // Sanity check
      (postMounted <> '')  // Sanity check
      ) then begin
      PostMountGUISetup(postMounted[1]);
      //    WebDAVStartup();
      EnableDisableControls();
    end;
  end;
end;


 // Handle "/create" command line
 // Returns: Exit code
function TfrmExplorerMain.HandleCommandLineOpts_Create(): eCmdLine_Exit;
begin
  Result := ceSUCCESS;
  if SDUCommandLineSwitch(CMDLINE_CREATE) then
    actFreeOTFENewExecute(nil);
end;


procedure TfrmExplorerMain.WMUserPostShow(var msg: TWMEndSession);
var
  junk: eCmdLine_Exit;
begin
  inherited;
  HandleCommandLineOpts(junk);

end;

 // Handle any command line options; returns TRUE if command line options
 // were passed through, and "/noexit" wasn't specified as a command line
 // parameter
function TfrmExplorerMain.HandleCommandLineOpts(out cmdExitCode: eCmdLine_Exit): Boolean;
var
  ignoreParams: Integer;
  settingsFile: String;
begin
  cmdExitCode := ceSUCCESS;

  // All command line options below require FreeOTFE to be active before they
  // can be used
  cmdExitCode := HandleCommandLineOpts_Create();

  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_EnableDevMenu();

  if cmdExitCode = ceSUCCESS then
    cmdExitCode := HandleCommandLineOpts_Mount();

  // Return TRUE if there were no parameters specified on the command line
  // and "/noexit" wasn't specified
  // Note: Disregard any CMDLINE_SETTINGSFILE and parameter
  Result := False;
  if not (SDUCommandLineSwitch(CMDLINE_NOEXIT)) then begin
    ignoreParams := 0;

    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFile) then begin
      Inc(ignoreParams);
      if (settingsFile <> '') then begin
        Inc(ignoreParams);
      end;
    end;

    Result := (ParamCount > ignoreParams);
  end;
end;

procedure TfrmExplorerMain.OnFreeOTFEExplorerRefreshMsg(var Msg: TMessage);
begin
  actRefreshExecute(nil);
end;


// Returns: TRUE if no problems, otherwise FALSE
function TfrmExplorerMain.CheckDestinationIsntSource(opType: TFExplOperation;
  srcPathAndFilename: String; destPath: String): Boolean;
var
  ucSrcPath:  String;
  ucDestPath: String;
begin
  Result := True;

  // Note: We use IncludeTrailingPathDelimiter(...) here and not
  //       ExcludeTrailingPathDelimiter(...) - otherwise "/" will return ""
  ucSrcPath  := uppercase(IncludeTrailingPathDelimiter(ExtractFilePath(srcPathAndFilename)));
  ucDestPath := uppercase(IncludeTrailingPathDelimiter(destPath));

  // Sanity check; not trying to move to itself?
  if (ucSrcPath = ucDestPath) then begin
    SDUMessageDlg(SDUParamSubstitute(_(
      'Cannot %1 %2: The destination folder is the same as the source folder.'),
      [OperationTitle(opType), ExtractFilename(srcPathAndFilename)]), mtError);
    Result := False;
  end // Sanity check; not trying to move to a subdir?
  else
  if fFilesystem.DirectoryExists(srcPathAndFilename) then begin
    if (Pos(uppercase(srcPathAndFilename), ucDestPath) = 1) then begin
      SDUMessageDlg(SDUParamSubstitute(
        _('Cannot %1 %2: The destination folder is a subfolder of the source folder'),
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

procedure TfrmExplorerMain.ClipboardSetToSelected(Sender: TObject;
  opType: TFExplOperation);
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

procedure TfrmExplorerMain.ClipboardSetItems(opType: TFExplOperation;
  stlItems: TStringList);
var
  dropType: Word;
begin
  ClipboardClear();

  dropType := DROPEFFECT_COPY;
  if (opType = cmMove) then begin
    dropType := DROPEFFECT_MOVE;
  end;
  SDUSetPreferredDropEffectOnClipboard(dropType);

  SDUSetDropFilesOnClipboard(stlItems);

  SDUSetDWORDOnClipboard(CF_FEXPL_SESSION_DATA, 1);

end;

function TfrmExplorerMain.ClipboardGetItems(out srcIsMountedFSNotLocalFS: Boolean;
  out opType: TFExplOperation; stlItems: TStringList): Boolean;

  function GetDropEffect(): TFExplOperation;
  var
    dropEffect: DWORD;
  begin
    if not (SDUGetPreferredDropEffectFromClipboard(dropEffect)) then begin
      dropEffect := DROPEFFECT_COPY;
    end;

    Result := cmCopy;
    if ((dropEffect and DROPEFFECT_MOVE) > 0) then begin
      Result := cmMove;
    end;
  end;

begin
  Result := False;

  stlItems.Clear();

  // Check for FreeOTFE Explorer clipboard type...
  if Clipboard.HasFormat(CF_FEXPL_SESSION_DATA) then begin
    srcIsMountedFSNotLocalFS := True;
    opType                   := GetDropEffect();

    Result := SDUGetDropFilesFromClipboard(stlItems);
  end // Check for MS Windows Explorer cut/copied files...
  else
  if SDUGetDropFilesFromClipboard(stlItems) then begin
    srcIsMountedFSNotLocalFS := False;
    opType                   := GetDropEffect();

    // stlItems already setup, so that's it!

    Result := True;
  end;

end;

procedure TfrmExplorerMain.actPasteExecute(Sender: TObject);
var
  destDir:                  String;
  srcIsMountedFSNotLocalFS: Boolean;
  opType:                   TFExplOperation;
  stlItems:                 TStringList;
begin
  inherited;

  stlItems := TStringList.Create();
  try
    if ClipboardGetItems(srcIsMountedFSNotLocalFS, opType, stlItems) then begin
      destDir := GetSelectedPath(Sender);

      if PerformOperation(opType, srcIsMountedFSNotLocalFS, stlItems, True,
        destDir, '') then
      begin
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
  destDir:      String;
  srcFilename:  String;
  allOK:        Boolean;
  newFilename:  String;
  destFilename: String;
begin
  allOK        := True;
  destFilename := '';

  // If there's only one item to be extracted, and it's a file - let the user
  // specify a path and filename to extract to
  if (items.Count = 1) then begin
    srcFilename := items[0];
    if fFilesystem.FileExists(srcFilename) then begin
      SDUSaveDialog_Extract.Options := SDUSaveDialog_Extract.Options + [ofDontAddToRecent];
      SDUSaveDialog_Extract.Filename := ExtractFilename(srcFilename);
      allOK := SDUSaveDialog_Extract.Execute();
      if allOK then begin
        newFilename := SDUSaveDialog_Extract.Filename;

        destDir      := ExcludeTrailingPathDelimiter(ExtractFilePath(newFilename));
        destFilename := ExtractFilename(newFilename);
      end;
    end;

  end;

  // If everything's OK, but there's no destination dir, let the user select
  // one
  // Extracted filenames are the same as the source filenames
  if allOK then begin
    if (destDir = '') then begin
      allOK := SDUSelectDirectory(self.Handle, _('Select location to extract to:'),
        '', destDir, BIF_USENEWUI);

      destFilename := ''; // Sanity
    end;
  end;


  if allOK then begin
    Extract(items, destDir, destFilename);
  end;

end;

procedure TfrmExplorerMain.Extract(items: TStrings; destDir: String; destFilename: String);
begin
  PerformOperation(cmCopy, True, items, False, destDir, destFilename);
end;

procedure TfrmExplorerMain.Store(items: TStrings);
var
  destDir: String;
begin
  destDir := SelectFilesystemDirectory(self, fFilesystem, sdtStore, items);
  if (destDir <> '') then begin
    Store(items, destDir);
  end;

end;

procedure TfrmExplorerMain.Store(items: TStrings; destDir: String);
var
  selectOpTypeDlg: TfrmSelectCopyOrMove;
  opType:          TFExplOperation;
  allOK:           Boolean;
begin
  allOK := True;

  if (gSettings.OptDefaultStoreOp = dsoCopy) then begin
    opType := cmCopy;
  end else
  if (gSettings.OptDefaultStoreOp = dsoMove) then begin
    opType := cmMove;
  end else begin
    opType := cmCopy; // Get rid of compiler warning

    selectOpTypeDlg := TfrmSelectCopyOrMove.Create(self);
    try
      if (selectOpTypeDlg.ShowModal = mrOk) then begin
        opType := selectOpTypeDlg.OpType;
      end else begin
        allOK := False;
      end;
    finally
      selectOpTypeDlg.Free();
    end;

  end;

  if allOK then begin
    PerformOperation(opType, False, items, True, destDir, '');
  end;

end;

function TfrmExplorerMain.ConfirmPerformOperation(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItems: TStrings; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destFilename: String): Boolean;
begin
  Result := False;

  if ConfirmOperation(opType, srcItems, destDir) then begin
    Result := PerformOperation(opType, srcIsMountedFSNotLocalFS, srcItems,
      destIsMountedFSNotLocalFS, destDir, destFilename);
  end;

end;

function TfrmExplorerMain.DetermineTotalSize(isMountedFSNotLocalFS: Boolean;
  items: TStrings): ULONGLONG;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to (items.Count - 1) do begin
    Result := Result + _DetermineTotalSize(isMountedFSNotLocalFS, items[i]);

    if ProgressDlgHasUserCancelled() then begin
      break;
    end;

  end;

end;

function TfrmExplorerMain._DetermineTotalSize(isMountedFSNotLocalFS: Boolean;
  item: String): ULONGLONG;
var
  tmpFileItem: TSDDirItem_FAT;
begin
  Result := 0;

  if isMountedFSNotLocalFS then begin
    if fFilesystem.FileExists(item) then begin
      tmpFileItem := TSDDirItem_FAT.Create();
      try
        fFilesystem.GetItem_FAT(item, tmpFileItem);
        Result := tmpFileItem.Size;
      finally
        tmpFileItem.Free();
      end;

    end else
    if fFilesystem.DirectoryExists(item) then begin
      Result := _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS, item);
    end;
  end else begin
    if FileExists(item) then begin
      Result := SDUGetFileSize(item);
    end else
    if SysUtils.DirectoryExists(item) then begin
      Result := _DetermineTotalSize_MountedDir(isMountedFSNotLocalFS, item);
    end;
  end;

end;


function TfrmExplorerMain._DetermineTotalSize_MountedDir(isMountedFSNotLocalFS: Boolean;
  srcItem: String): ULONGLONG;
var
  srcDirContents: TSDDirItemList;
  i:              Integer;
  currSrcSubItem: TSDDirItem;
  currSrcSubPathAndFilename: String;
begin
  Result := 0;

  srcDirContents := TSDDirItemList.Create();
  try
    if FSLoadContentsFromDisk(isMountedFSNotLocalFS, srcItem, srcDirContents) then begin
      for i := 0 to (srcDirContents.Count - 1) do begin
        currSrcSubItem            := srcDirContents[i];
        currSrcSubPathAndFilename :=
          IncludeTrailingPathDelimiter(srcItem) + currSrcSubItem.Filename;

        // Skip volume labels, devices, and "."
        // Also skip "..", unless ShowParentDir is TRUE
        if currSrcSubItem.IsFile then begin
          Result := Result + currSrcSubItem.Size;
        end else
        if currSrcSubItem.IsDirectory then begin
          if ((currSrcSubItem.Filename <> DIR_CURRENT_DIR) and
            (currSrcSubItem.Filename <> DIR_PARENT_DIR)) then begin
            Result := Result + _DetermineTotalSize_MountedDir(
              isMountedFSNotLocalFS, currSrcSubPathAndFilename);
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

function TfrmExplorerMain.PerformOperation(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItems: TStrings; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destFilename: String
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
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and
    destIsMountedFSNotLocalFS) then begin
    for i := 0 to (srcItems.Count - 1) do begin
      if not (CheckDestinationIsntSource(opType, srcItems[i], destDir)) then begin
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


  useMoveDeletionMethod := gSettings.OptMoveDeletionMethod;
  if ((opType = cmMove) and not (srcIsMountedFSNotLocalFS)) then begin
    if (useMoveDeletionMethod = mdmPrompt) then begin
      dlgOverwritePrompt := TfrmOverwritePrompt.Create(self);
      try
        if (dlgOverwritePrompt.ShowModal() = mrOk) then begin
          useMoveDeletionMethod := dlgOverwritePrompt.MoveDeletionMethod;
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
  totalSize := DetermineTotalSize(srcIsMountedFSNotLocalFS, srcItems);
  ProgressDlgSetTotal(totalSize);

  // Determining the total size of all files could have taken awhile; reset the
  // progress dialog's timer
  ProgressDlgTimerReset();

  if ProgressDlgHasUserCancelled() then begin
    abortAllRemaining := True;
  end;

  if not (abortAllRemaining) then begin
    promptOverwriteFiles := True;
    promptOverwriteDirs  := True;

    abortAllRemaining := False;
    for i := 0 to (srcItems.Count - 1) do begin
      abortAllRemaining := not (_PerformOperation(
        opType, srcIsMountedFSNotLocalFS, srcItems[i], destIsMountedFSNotLocalFS,
        destDir, destFilename, useMoveDeletionMethod, promptOverwriteFiles, promptOverwriteDirs));
      if abortAllRemaining then begin
        break;
      end;

    end;
  end;

  userCancelled := ProgressDlgHasUserCancelled();

  SetStatusMsg('');
  ProgressDlgStop();

  if abortAllRemaining then begin
    if not (userCancelled) then begin
      SDUMessageDlg(
        SDUParamSubstitute(_('%1 failed.'), [SDUInitialCapital(OperationTitle(opType))]),
        mtError
        );
    end;
  end else begin
    //    SDUMessageDlg(
    //                  SDUParamSubstitute(
    //                                     _('%1 completed successfully.'),
    //                                     [SDUInitialCapital(OperationTitle(opType))]
    //                                    ),
    //                  mtInformation
    //                 );
  end;

  // Refresh anyway, in case *some* but not all items were processed
  // NOT NEEDED FOR EXTRACTION! i.e. If copying and the destination is the HDD
  if not ((opType = cmCopy) and not (destIsMountedFSNotLocalFS)) then begin
    actRefreshExecute(nil);
  end;

  Result := not (abortAllRemaining);

end;

procedure TfrmExplorerMain.ProgressDlgSetup(opType: TFExplOperation);
begin
  // Get rid of any existing...
  ProgressDlgStop();

  FOpProgressDlg := TSDUWindowsProgressDialog.Create();

  FOpProgressDlg.Title := SDUInitialCapital(OperationVerbTitle(opType)) + '...';

  case opType of
    cmCopy:
    begin
      FOpProgressDlg.CommonAVI := aviCopyFile;
    end;

    cmMove:
    begin
      // Although called "aviCopyFiles" in the Delphi enum, this actually shows
      // multiple files being moved
      FOpProgressDlg.CommonAVI := aviCopyFiles;
    end;

    cmDelete:
    begin
      FOpProgressDlg.CommonAVI := aviDeleteFile;
    end;

  end;

  FOpProgressDlg.CancelMsg := 'Please wait while the current operation is cleaned up...';

  FOpProgressDlg.LineCompact[1] := True;
  FOpProgressDlg.LineCompact[2] := True;

  // We turn off the "time remaining" display as it can be misleading - we only
  // update the progressbar after each *file* is processed. Although the
  // progressbar is updated with the size of each file as it's processed, this
  // can give "spurts" which can disrupt the "time remaining" display
  FOpProgressDlg.ShowTime := False;

end;

procedure TfrmExplorerMain.ProgressDlgStart();
begin
  FOpProgressDlg.StartProgressDialog();
end;

procedure TfrmExplorerMain.ProgressDlgSetTotal(total: ULONGLONG);
begin
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.Total64 := total;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgTimerReset();
begin
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.Timer(PDTIMER_RESET);
  end;

end;

procedure TfrmExplorerMain.ProgressDlgSetLineOne(line: String);
begin
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.LineText[1] := line;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgSetLineTwo(line: String);
begin
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.LineText[2] := line;
  end;

end;

procedure TfrmExplorerMain.ProgressDlgIncrement(increment: ULONGLONG);
begin
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.Progress64 := FOpProgressDlg.Progress64 + increment;
  end;

end;

function TfrmExplorerMain.ProgressDlgHasUserCancelled(): Boolean;
begin
  Result := False;
  if (FOpProgressDlg <> nil) then begin
    Result := FOpProgressDlg.HasUserCancelled();
  end;

end;

procedure TfrmExplorerMain.ProgressDlgStop();
begin
  // Stop and free off any existing...
  if (FOpProgressDlg <> nil) then begin
    FOpProgressDlg.StopProgressDialog();
    FOpProgressDlg.Free();
    FOpProgressDlg := nil;
  end;

end;


 // destDir - the desination dir (path) where the src item should be placed *under*
 // Returns: TRUE if successful, FALSE when any remaining Extract operations
 //          should be ABORTED
function TfrmExplorerMain._PerformOperation(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destFilename: String;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
  var promptOverwriteDirs: Boolean): Boolean;
var
  abortAllRemaining: Boolean;
  destOK:            Boolean;
  srcIsFile:         Boolean;
  srcIsDir:          Boolean;
begin
  abortAllRemaining := False;

  // Sanity check - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and
    destIsMountedFSNotLocalFS) then begin
    if not (CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
      // Error message already shown...
      abortAllRemaining := True;
    end;
  end;

  // Sanity check...
  if not (abortAllRemaining) then begin
    if (opType = cmDelete) then begin
      // Destination irrelevant; deleting source
      destOK := True;
    end else
    if destIsMountedFSNotLocalFS then begin
      destOK := fFilesystem.DirectoryExists(destDir);
    end else begin
      destOK := SysUtils.DirectoryExists(destDir);
    end;
    if not (destOK) then begin
      SDUMessageDlg(
        SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION,
        [OperationTitle(opType), ExtractFilename(destDir)]),
        mtError
        );

      abortAllRemaining := True;
    end;
  end;

  if not (abortAllRemaining) then begin
    // Check if source is a file...
    if srcIsMountedFSNotLocalFS then begin
      srcIsFile := fFilesystem.FileExists(srcItem);
    end else begin
      srcIsFile := SysUtils.FileExists(srcItem);
    end;

    // If source a file, process as such...
    if srcIsFile then begin
      abortAllRemaining := not (_PerformOperation_File(
        opType, srcIsMountedFSNotLocalFS, srcItem, destIsMountedFSNotLocalFS,
        destDir, destFilename, moveDeletionMethod, promptOverwriteFiles,
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
        abortAllRemaining := not (_PerformOperation_Dir(opType,
          srcIsMountedFSNotLocalFS, srcItem, destIsMountedFSNotLocalFS,
          destDir, destFilename, moveDeletionMethod, promptOverwriteFiles,
          promptOverwriteDirs));
      end else begin
        // Source is neither a file nor dir - abort.
        SDUMessageDlg(
          SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE,
          [OperationTitle(opType), ExtractFilename(srcItem)]),
          mtError
          );

        abortAllRemaining := True;
      end;
    end;
  end;

  Result := not (abortAllRemaining);
end;

function TfrmExplorerMain._PerformOperation_File(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destFilename: String;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
  var promptOverwriteDirs: Boolean): Boolean;
var
  abortAllRemaining: Boolean;
  goodToDoOp:        Boolean;
  destItem:          String;
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
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and
    destIsMountedFSNotLocalFS) then begin
    if not (CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
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
  if not (srcOK) then begin
    SDUMessageDlg(
      SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE,
      [OperationTitle(opType), ExtractFilename(srcItem)]),
      mtError
      );

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;


  // Sanity check...
  if (opType = cmDelete) then begin
    // Destination irrelevant; deleting source
    destOK := True;
  end else
  if destIsMountedFSNotLocalFS then begin
    destOK := fFilesystem.DirectoryExists(destDir);
  end else begin
    destOK := SysUtils.DirectoryExists(destDir);
  end;
  if not (destOK) then begin
    SDUMessageDlg(
      SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION,
      [OperationTitle(opType), ExtractFilename(destDir)]),
      mtError
      );

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;

  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if (destFilename <> '') then begin
        destItem := IncludeTrailingPathDelimiter(destDir) + destFilename;
      end else begin
        destItem := IncludeTrailingPathDelimiter(destDir) + ExtractFilename(srcItem);
      end;
    end else begin
      destItem := '';
    end;
  end;

  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemIsDir := fFilesystem.DirectoryExists(destItem);
      end else begin
        destItemIsDir := SysUtils.DirectoryExists(destItem);
      end;
      if destItemIsDir then begin
        SDUMessageDlg(SDUParamSubstitute(
          _(
          'Cannot create or replace %1: There is already a file with the same name as the folder name you specified. Specify a different name.'),
          [ExtractFilename(srcItem)]),
          mtError
          );

        abortAllRemaining := True;
        goodToDoOp        := False;
      end else begin
        if destIsMountedFSNotLocalFS then begin
          destItemIsFile := fFilesystem.FileExists(destItem);
        end else begin
          destItemIsFile := SysUtils.FileExists(destItem);
        end;

        if destItemIsFile then begin
          if promptOverwriteFiles then begin
            confirmResult := PromptToReplaceYYANC(
              srcIsMountedFSNotLocalFS, srcItem, destIsMountedFSNotLocalFS, destItem);

            if (confirmResult = mrYes) then begin
              // Do nothing - goodToDoOp already set to TRUE
            end else
            if (confirmResult = mrYesToAll) then begin
              // goodToDoOp already set to TRUE
              promptOverwriteFiles := False; // This is what MS Windows explorer appears to do.
            end else
            if (confirmResult = mrNo) then begin
              goodToDoOp := False;
            end else
            if (confirmResult = mrCancel) then begin
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
  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
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

      if not (goodToDoOp) then begin
        SDUMessageDlg(
          SDUParamSubstitute(RS_UNABLE_TO_DELETE_EXISTING_FILE,
          [ExtractFilename(destItem)]),
          mtError
          );
        abortAllRemaining := True;
      end;
    end;
  end;

  // Process the file...
  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    // Get the size of the file before copy/move/delete
    if srcIsMountedFSNotLocalFS then begin
      tmpItem := TSDDirItem_FAT.Create();
      try
        fFilesystem.GetItem_FAT(srcItem, tmpItem);
        increment := tmpItem.Size;
      finally
        tmpItem.Free();
      end;

    end else begin
      increment := SDUGetFileSize(srcItem);
    end;

    abortAllRemaining := not (_PerformOperation_File_Actual(
      opType, srcIsMountedFSNotLocalFS, srcItem, destIsMountedFSNotLocalFS,
      destDir, destItem, moveDeletionMethod));

    if abortAllRemaining then begin
      if (opType = cmDelete) then begin
        SDUMessageDlg(
          SDUParamSubstitute(_('Unable to delete ''%1''.'), [ExtractFilename(srcItem)]),
          mtError
          );
      end else begin
        SDUMessageDlg(
          SDUParamSubstitute(_('Unable to create or replace %1.'),
          [ExtractFilename(srcItem)]),
          mtError
          );
      end;
    end else begin
      ProgressDlgIncrement(increment);
    end;
  end;

  Result := not (abortAllRemaining);
end;

 // All checks carried out - do operation
 // IMPORTANT: This function should **ONLY** be called from _PerformOperation_File(...)
function TfrmExplorerMain._PerformOperation_File_Actual(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destItem: String; moveDeletionMethod: TMoveDeletionMethod): Boolean;

  // Return the last path segment of the path passed in
  function GetLastSegmentFrom(path: String): String;
  begin
    Result := ExcludeTrailingPathDelimiter(path);
    Result := ExtractFilename(Result);
    if (Result = '') then begin
      Result := PATH_SEPARATOR;
    end;

  end;

var
  prevCursor: TCursor;
begin
  Result := False;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    ProgressDlgSetLineOne(ExtractFilename(srcItem));
    if (opType = cmDelete) then begin
      ProgressDlgSetLineTwo(
        SDUParamSubstitute(_('From ''%1'''), [GetLastSegmentFrom(
        ExtractFilePath(srcItem))])
        );
    end else begin
      ProgressDlgSetLineTwo(
        SDUParamSubstitute(_('From ''%1'' to ''%2'''),
        [GetLastSegmentFrom(ExtractFilePath(srcItem)), GetLastSegmentFrom(destDir)])
        );
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
        SetStatusMsg(SDUParamSubstitute(_('Copying: %1'), [srcItem]));
        Result := fFilesystem.CopyFile(srcItem, destItem);
      end else
      if (srcIsMountedFSNotLocalFS and not (destIsMountedFSNotLocalFS)) then begin
        SetStatusMsg(SDUParamSubstitute(_('Extracting: %1'), [srcItem]));
        Result := fFilesystem.ExtractFile(srcItem, destItem);
      end else
      if (not (srcIsMountedFSNotLocalFS) and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(SDUParamSubstitute(_('Storing: %1'), [srcItem]));
        Result := _StoreFile(destDir, srcItem);
      end else
      if (not (srcIsMountedFSNotLocalFS) and not (destIsMountedFSNotLocalFS)) then begin
        Result := CopyFile(PChar(destItem), PChar(srcItem), False);
      end;
    end // MOVE OPERATIONS...
    else
    if (opType = cmMove) then begin
      if (srcIsMountedFSNotLocalFS and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(SDUParamSubstitute(_('Moving: %1'), [srcItem]));
        Result := fFilesystem.MoveFileOrDir(srcItem, destItem);
      end else
      if (srcIsMountedFSNotLocalFS and not (destIsMountedFSNotLocalFS)) then begin
        SetStatusMsg(SDUParamSubstitute(_('Extracting: %1'), [srcItem]));
        Result := fFilesystem.ExtractFile(srcItem, destItem);
        if Result then begin
          fFilesystem.DeleteFileOrDir(srcItem);
        end;
      end else
      if (not (srcIsMountedFSNotLocalFS) and destIsMountedFSNotLocalFS) then begin
        SetStatusMsg(SDUParamSubstitute(_('Storing: %1'), [srcItem]));
        Result := _StoreFile(destDir, srcItem);
        if Result then begin
          Result := DeleteLocalFSItemUsingMethod(moveDeletionMethod, srcItem);
        end;
      end else
      if (not (srcIsMountedFSNotLocalFS) and not (destIsMountedFSNotLocalFS)) then begin
        Result := MoveFile(PChar(destItem), PChar(srcItem));
      end;
    end // DELETE OPERATIONS...
    else
    if (opType = cmDelete) then begin
      SetStatusMsg(SDUParamSubstitute(_('Deleting: %1'), [srcItem]));
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

function TfrmExplorerMain.DeleteLocalFSItemUsingMethod(moveDeletionMethod:
  TMoveDeletionMethod; item: String): Boolean;
var
  userPrompt: Integer;
  promptMsg:  String;
begin
  Result := True;

  if (moveDeletionMethod = mdmDelete) then begin
    // Simple delete...
    // Don't bother to set progress strings - MS Windows Explorer doesn't for
    // *it's* simple move/deletes
    //    ProgressDlgSetLineTwo(_('Deleting original...'));
    //    SetStatusMsg(SDUParamSubstitute(_('Deleting: %1'), [item]));

    if FileExists(item) then begin
      Result := SysUtils.DeleteFile(item);
    end else
    if SysUtils.DirectoryExists(item) then begin
      try
        RmDir(item);
      except
        // Swallow exception; just set return value
        Result := False;
      end;
    end;
  end else
  if (moveDeletionMethod = mdmOverwrite) then begin
    // Overwrite then delete...
    ProgressDlgSetLineTwo(_('Overwriting original...'));
    SetStatusMsg(SDUParamSubstitute(_('Overwriting: %1'), [item]));

    Result := (fShredderObj.DestroyFileOrDir(item, False, True) = srSuccess);
  end;

  // If there was a problem, allow the user to abort/retry/ignore
  if not (Result) then begin
    if (moveDeletionMethod = mdmDelete) then begin
      promptMsg := _('Deletion of %1 failed');
    end else
    if (moveDeletionMethod = mdmOverwrite) then begin
      promptMsg := _('Overwrite of %1 failed');
    end;

    userPrompt := SDUMessageDlg(SDUParamSubstitute(promptMsg, [ExtractFilename(item)]),
      mtWarning, [mbAbort, mbRetry, mbIgnore], 0);

    if (userPrompt = mrAbort) then begin
      // Do nothing - Result already set to FALSE
    end else
    if (userPrompt = mrRetry) then begin
      // Go again...
      Result := DeleteLocalFSItemUsingMethod(moveDeletionMethod, item);
    end else
    if (userPrompt = mrIgnore) then begin
      // Assume that it's OK...
      Result := True;
    end;

  end;

end;

procedure TfrmExplorerMain.OverwritePassStarted(Sender: TObject;
  itemName: String; passNumber: Integer; totalPasses: Integer);
begin
  ProgressDlgSetLineTwo(
    SDUParamSubstitute(_('Overwriting original (Pass: %1 / %2)...'),
    [passNumber, totalPasses]));
end;

procedure TfrmExplorerMain.OverwriteCheckForUserCancel(Sender: TObject;
  var userCancelled: Boolean);
begin
  userCancelled := ProgressDlgHasUserCancelled();
end;

function TfrmExplorerMain._PerformOperation_Dir(opType: TFExplOperation;
  srcIsMountedFSNotLocalFS: Boolean; srcItem: String; destIsMountedFSNotLocalFS: Boolean;
  destDir: String; destFilename: String;
  // When moving/copying a single file, use this as the destination filename
  // Set to '' to use the source filename's filename
  moveDeletionMethod: TMoveDeletionMethod; var promptOverwriteFiles: Boolean;
  var promptOverwriteDirs: Boolean): Boolean;
var
  abortAllRemaining:  Boolean;
  goodToDoOp:         Boolean;
  destItem:           String;
  srcDirContents:     TSDDirItemList;
  junkFALSE:          Boolean;
  confirmResult:      Word;
  i:                  Integer;
  currSrcSubItem:     TSDDirItem;
  currSrcSubPathAndFilename: String;
  srcOK:              Boolean;
  destOK:             Boolean;
  destItemExistsFile: Boolean;
  destItemExistsDir:  Boolean;
begin
  goodToDoOp        := True;
  abortAllRemaining := False;

  // Sanity check - can't copy/move from/to the same
  if (((opType = cmCopy) or (opType = cmMove)) and srcIsMountedFSNotLocalFS and
    destIsMountedFSNotLocalFS) then begin
    if not (CheckDestinationIsntSource(opType, srcItem, destDir)) then begin
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
  if not (srcOK) then begin
    SDUMessageDlg(
      SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_FILE,
      [OperationTitle(opType), ExtractFilename(srcItem)]),
      mtError
      );

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;

  // Sanity check...
  if (opType = cmDelete) then begin
    // Destination irrelevant; deleting source
    destOK := True;
  end else
  if destIsMountedFSNotLocalFS then begin
    destOK := fFilesystem.DirectoryExists(destDir);
  end else begin
    destOK := SysUtils.DirectoryExists(destDir);
  end;
  if not (destOK) then begin
    SDUMessageDlg(
      SDUParamSubstitute(RS_CANNOT_X_CANNOT_FIND_SPECIFIED_DESTINATION,
      [OperationTitle(opType), ExtractFilename(destDir)]),
      mtError
      );

    abortAllRemaining := True;
    goodToDoOp        := False;
  end;


  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      destItem := IncludeTrailingPathDelimiter(destDir) + ExtractFilename(srcItem);
    end else begin
      //destItem := '';
    end;
  end;


  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemExistsFile := fFilesystem.FileExists(destItem);
      end else begin
        destItemExistsFile := SysUtils.FileExists(destItem);
      end;
      if destItemExistsFile then begin
        SDUMessageDlg(SDUParamSubstitute(
          _(
          'Cannot create or replace %1: There is already a file with the same name as the folder name you specified. Specify a different name.'),
          [ExtractFilename(srcItem)]),
          mtError
          );

        abortAllRemaining := True;
        goodToDoOp        := False;
      end else begin
        if destIsMountedFSNotLocalFS then begin
          destItemExistsDir := fFilesystem.DirectoryExists(destItem);
        end else begin
          destItemExistsDir := SysUtils.DirectoryExists(destItem);
        end;
        if destItemExistsDir then begin
          if promptOverwriteDirs then begin
            confirmResult := SDUMessageDlg(SDUParamSubstitute(
              _('This folder already contains a folder named ''%1''.' +
              SDUCRLF + SDUCRLF +
              'If the files in the existing folder have the same name as files in the folder you are moving or copying, they will be replaced. Do you still want to move or copy the folder?'), [ExtractFilename(srcItem)]), mtWarning, [mbYes, mbNo, mbCancel, mbYesToAll], 0);

            if (confirmResult = mrYes) then begin
              // Do nothing - goodToDoOp already set to TRUE
            end else
            if (confirmResult = mrYesToAll) then begin
              // goodToDoOp already set to TRUE
              promptOverwriteDirs  := False;
              promptOverwriteFiles := False; // This is what MS Windows explorer appears to do.
            end else
            if (confirmResult = mrNo) then begin
              goodToDoOp := False;
              // *Don't* set abortAllRemaining here - MS Windows Explorer just
              // continues, skipping this one
            end else
            if (confirmResult = mrCancel) then begin
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
  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    if ((opType = cmCopy) or (opType = cmMove)) then begin
      if destIsMountedFSNotLocalFS then begin
        destItemExistsDir := fFilesystem.DirectoryExists(destItem);
      end else begin
        destItemExistsDir := SysUtils.DirectoryExists(destItem);
      end;

      if not (destItemExistsDir) then begin
        if destIsMountedFSNotLocalFS then begin
          goodToDoOp := fFilesystem.CreateDir(destDir, ExtractFilename(destItem));
        end else begin
          goodToDoOp := SysUtils.CreateDir(IncludeTrailingPathDelimiter(destDir) +
            ExtractFilename(destItem));
        end;

        if not (goodToDoOp) then begin
          SDUMessageDlg(
            SDUParamSubstitute(RS_UNABLE_TO_CREATE_FOLDER + SDUCRLF +
            SDUCRLF + RS_PLEASE_ENSURE_ENOUGH_FREE_SPACE, [ExtractFilename(destItem)]),
            mtError
            );
          abortAllRemaining := True;
        end;
      end;
    end;
  end;

  // Process the contents of the directory...
  if (goodToDoOp and not (abortAllRemaining)  // (Included as sanity check)
    ) then begin
    srcDirContents := TSDDirItemList.Create();
    try
      if FSLoadContentsFromDisk(srcIsMountedFSNotLocalFS, srcItem, srcDirContents) then begin
        // These pass FALSE through always
        junkFALSE := False;

        for i := 0 to (srcDirContents.Count - 1) do begin
          abortAllRemaining := ProgressDlgHasUserCancelled();
          if abortAllRemaining then begin
            break;
          end;

          currSrcSubItem            := srcDirContents[i];
          currSrcSubPathAndFilename :=
            IncludeTrailingPathDelimiter(srcItem) + currSrcSubItem.Filename;

          // Skip volume labels, devices, and "."
          // Also skip "..", unless ShowParentDir is TRUE
          if currSrcSubItem.IsFile then begin
            abortAllRemaining := not (_PerformOperation_File(
              opType, srcIsMountedFSNotLocalFS, currSrcSubPathAndFilename,
              destIsMountedFSNotLocalFS, destItem, destFilename, moveDeletionMethod,
              junkFALSE, promptOverwriteDirs));
          end else
          if currSrcSubItem.IsDirectory then begin
            if ((currSrcSubItem.Filename <> DIR_CURRENT_DIR) and
              (currSrcSubItem.Filename <> DIR_PARENT_DIR)) then begin
              abortAllRemaining :=
                not (_PerformOperation_Dir(opType, srcIsMountedFSNotLocalFS,
                currSrcSubPathAndFilename, destIsMountedFSNotLocalFS,
                destItem, destFilename, moveDeletionMethod, junkFALSE,
                promptOverwriteDirs));
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

  if not (abortAllRemaining) then begin
    if ((opType = cmMove) or (opType = cmDelete)) then begin
      if srcIsMountedFSNotLocalFS then begin
        fFilesystem.DeleteFileOrDir(srcItem);
      end else begin
        abortAllRemaining := DeleteLocalFSItemUsingMethod(moveDeletionMethod, srcItem);
      end;
    end;
  end;

  Result := not (abortAllRemaining);
end;

function TfrmExplorerMain.FSLoadContentsFromDisk(fromMountedFSNotLocalFS: Boolean;
  srcItem: String; srcDirContents: TSDDirItemList): Boolean;
var
  iterator: TSDUFileIterator;
  entry:    String;
  currItem: TSDDirItem_FAT;
begin
  if fromMountedFSNotLocalFS then begin
    Result := fFilesystem.LoadContentsFromDisk(srcItem, srcDirContents);
  end else begin
    iterator := TSDUFileIterator.Create(nil);
    try
      iterator.Directory          := srcItem;
      iterator.FileMask           := '*';
      iterator.RecurseSubDirs     := False;
      iterator.OmitStartDirPrefix := False;
      iterator.IncludeDirNames    := True;
      iterator.Reset();
      entry := iterator.Next();
      while (entry <> '') do begin
        currItem := TSDDirItem_FAT.Create();

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

function TfrmExplorerMain.GetLocalFileDetails(pathAndFilename: String;
  var item: TSDDirItem_FAT): Boolean;
var
  CreationTime:   TFileTime;
  LastAccessTime: TFileTime;
  LastWriteTime:  TFileTime;
  fileAttrs:      Integer;
begin
  item.Filename    := ExtractFilename(pathAndFilename);
  item.FilenameDOS := '';  // Set to '' to autogenerate DOS 8.3 filename

  item.IsFile := SysUtils.FileExists(pathAndFilename);

  item.Size := SDUGetFileSize(pathAndFilename);

  if SDUFileTimestamps(pathAndFilename, CreationTime, LastAccessTime, LastWriteTime) then begin
    item.TimestampCreation     := SDUFileTimeToTimeStamp(CreationTime);
    item.DatestampLastAccess   := SDUFileTimeToDateTime(LastAccessTime);
    item.TimestampLastModified := SDUFileTimeToTimeStamp(LastWriteTime);
  end else begin
    item.TimestampCreation     := DateTimeToTimeStamp(now);
    item.DatestampLastAccess   := Now;
    item.TimestampLastModified := DateTimeToTimeStamp(now);
  end;

  // Turn off useless hints about FileCtrl.pas being platform specific
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
  fileAttrs := FileGetAttr(pathAndFilename);
  if (fileAttrs <> -1) then begin
    item.IsReadonly := (fileAttrs and faReadOnly) = faReadOnly;
    item.IsArchive  := (fileAttrs and faArchive) = faArchive;
    item.IsHidden   := (fileAttrs and faHidden) = faHidden;
    item.IsSystem   := (fileAttrs and faSysFile) = faSysFile;
  end;
{$WARN UNIT_PLATFORM ON}
{$WARN SYMBOL_PLATFORM ON}

  Result := True;
end;

function TfrmExplorerMain.DoTests: Boolean;
var
  mountList: TStringList;
  mountedAs: DriveLetterString;
  vol_path, key_file, les_file, projPath: String;
  vl:        Integer;
const
  TEST_VOLS: array[0..12] of String =
    ('a.box', 'b.box', 'c.box', 'd.box', 'e.box',
    'e.box', 'f.box', 'luks.box', 'luks_essiv.box', 'a.box',
    'b.box', 'dmcrypt_dx.box', 'dmcrypt_dx.box');
  PASSWORDS: array[0..12] of Ansistring =
    ('password', 'password', '!"£$%^&*()', 'password', 'password',
    '5ekr1t', 'password', 'password', 'password', 'secret',
    'secret', 'password', '5ekr1t');
  ITERATIONS: array[0..12] of Integer =
    (2048, 2048, 2048, 2048, 10240,
    2048, 2048, 2048, 2048, 2048,
    2048, 2048, 2048);
  OFFSET: array[0..12] of Integer =
    (0, 0, 0, 0, 0,
    2097152, 0, 0, 0, 0,
    0, 0, 0);
  KEY_FILES: array[0..12] of String =
    ('', '', '', '', '',
    '', '', '', '', 'a.cdb',
    'b.cdb', '', '');
  LES_FILES: array[0..12] of String =
    ('', '', '', '', '',
    '', '', '', '', '',
    '', 'dmcrypt_dx.les', 'dmcrypt_hid.les');

  procedure CheckMountedOK;
  var
    s: String;
  begin
    //    RefreshDrives();
    // Mount successful
    //        prettyMountedAs := prettyPrintDriveLetters(mountedAs);
    PostMountGUISetup(mountedAs[1]);
    s := SDFilesystemListView1.DirItem[0].Filename;//ToNode(SDFilesystemListView1.Items[0]);
    //    s:= GetSelectedPath(nil);
    if LowerCase(s) <> 'readme.txt' then begin
      SDUMessageDlg('File: readme.txt not found');
      Result := False;
    end;
    {
    if (CountValidDrives(mountedAs) <> 1) then begin
      SDUMessageDlg(Format('The Container %s has NOT been opened as drive: %s',
        [TEST_VOLS[vl], mountedAs]));
      Result := False;
    end else begin
      //done: test opened OK & file exists
      if not FileExists(mountedAs + ':\README.txt') then begin
        SDUMessageDlg(Format('File: %s:\README.txt not found', [mountedAs]));
        Result := False;
      end;
    end;
    }
  end;

  procedure UnMountAndCheck;
  begin
    Application.ProcessMessages;
    Dismount();
    Application.ProcessMessages;
    //    RefreshDrives();
    Application.ProcessMessages;
    if CountValidDrives(GetFreeOTFEDLL().DrivesMounted) > 0 then begin
      SDUMessageDlg(Format('Drive(s) %s not unmounted', [GetFreeOTFEDLL().DrivesMounted]));
      Result := False;
    end;
  end;

begin
  inherited;

  Result := True;
  if not GetFreeOTFEBase().Active then begin
    ShowMessage('component not active - can''t run tests');
    exit;
  end;

  mountList := TStringList.Create();
  try
    //for loop is optimised into reverse order , but want to process forwards
    vl       := 0;
    // debug ver is in subdir
    {$IFDEF DEBUG}
      projPath := 'P:\';
    {$ELSE}
    projPath := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\');
    {$ENDIF}
    vol_path := projPath + 'test_vols\';
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

      if GetFreeOTFEDLL().IsLUKSVolume(vol_path + TEST_VOLS[vl]) or (LES_FILES[vl] <> '') then
      begin
        if not GetFreeOTFEDLL().MountLinux(mountList, mountedAs, True,
          les_file, SDUStringToSDUBytes(PASSWORDS[vl]), key_file, False, nlLF, 0, True) then
          Result := False;
      end else begin
        //call silently
        if not GetFreeOTFEDLL().MountFreeOTFE(mountList, mountedAs, True,
          key_file, SDUStringToSDUBytes(PASSWORDS[vl]), OFFSET[vl], False, True,
          256, ITERATIONS[vl]) then
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


  finally
    mountList.Free();
  end;

  if Result then
    SDUMessageDlg('All functional tests passed')
  else
    SDUMessageDlg('At least one functional test failed');
end;

  (*
procedure TfrmExplorerMain.OverwriteAllWebDAVCachedFiles();
{$IFNDEF WEBDAV_OVERWRITE_TSHREDDER}
{$IFNDEF WEBDAV_OVERWRITE_SIMPLE}

  If the Delphi compiler throws an error at this point, then neither
  "WEBDAV_OVERWRITE_TSHREDDER" nor "WEBDAV_OVERWRITE_SIMPLE" were defined.

  See notes above for what this should be set to.

{$ENDIF}
{$ENDIF}
var
  i: integer;
  filename: string;
  pItem: PRequestedItem;
  prevCursor: TCursor;
{$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
  shredder: TShredder;
{$ENDIF}
{$IFDEF WEBDAV_OVERWRITE_SIMPLE}
  data: TSDUBytes;
{$ENDIF}
begin
  SetStatusMsg(_('Overwriting cached files...'));

  ProgressDlgSetup(cmDelete);
  FOpProgressDlg.Title := _('Overwriting cached files...');
  ProgressDlgStart();
  ProgressDlgSetTotal(WebDAVObj.RequestedItems.count);
  ProgressDlgTimerReset();
  prevCursor := Screen.Cursor;
  Screen.Cursor := crAppStart;  // Hourglass with mouse pointer
{$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
  shreddercomponentversion:  shredder:= TShredder.Create(nil);
{$ENDIF}
  try
{$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
    shredder.IntMethod := smZeros; // Probably faster than smPseudorandom
    shredder.IntPasses := 1;
    shredder.IntRenameBeforeDelete := FALSE;
{$ENDIF}

    // Setup WebDAV server to ignore writes, and return garbage if files are
    // requested...
    WebDAVObj.ReturnGarbage := TRUE;
    
    // Overwrite all files which have been requested during the mounted
    // session...
    // We *WRITE* to the file, as this is the only way of *immediatly* forcing
    // the local copy to be overwritten. Just reading the file again only causes
    // it to be loaded from the cache, until the cached copy times out (typically
    // after 60 seconds)
    for i:=0 to (WebDAVObj.RequestedItems.count-1) do
      begin
      ProgressDlgSetLineOne(WebDAVObj.RequestedItems[i]);
      filename := FMappedDrive+':'+WebDAVObj.RequestedItems[i];

      pItem := PRequestedItem(WebDAVObj.RequestedItems.Objects[i]);

      // Skip directories...
      if not(pItem.IsFile) then
        begin
        ProgressDlgIncrement(1);
        continue;
        end;

{$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
      shredder.DestroyFileOrDir(filename, FALSE, TRUE, FALSE);
{$ENDIF}

{$IFDEF WEBDAV_OVERWRITE_SIMPLE}
      // Cause local cached copy to be overwritten...
      SDUInitAndZeroBuffer(pItem.Size, data);
      SDUSetFileContent(filename, data);
      // Cause local cached copy to have zero filesize...
      SDUInitAndZeroBuffer(0, data);
      SDUSetFileContent(filename, data);
{$ENDIF}

      ProgressDlgIncrement(1);
      if ProgressDlgHasUserCancelled() then
        begin
        break;
        end;

      end;

  finally
{$IFDEF WEBDAV_OVERWRITE_TSHREDDER}
    shredder.Free();
{$ENDIF}
    ProgressDlgStop();
    Screen.Cursor := prevCursor;
  end;

  WebDAVObj.ClearDownRequestedItemsList();
  DefaultStatusMsg();

end;
      *)
initialization
  // A "session ID" is used when registering the FreeOTFE Explorer clipboard
  // format to prevent confusion when running multiple instances; atm it isn't
  // possible to copy from one instance and paste into another.
  // When cut/copy from FreeOTFE Explorer to MS Windows Explorer is
  // implemented, this restriction can be removed.
  // The current date/time should be unique enough (unless the user mounts
  // multiple volumes in the same second!)
  CFSTR_FEXPL_SESSION_DATA := Format(CFSTR_FEXPL_SESSION_BASE,
    [SDUTDateTimeToISO8601(Now)]);

  CF_FEXPL_SESSION_DATA := RegisterClipboardFormat(PChar(CFSTR_FEXPL_SESSION_DATA));

finalization
  // There's no means of deregistering the clipboard type?!

end.
