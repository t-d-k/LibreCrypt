unit SDFilesystemCtrls;

interface

uses
  ComCtrls, Classes, Contnrs, Controls, Windows,
  Menus,
  SysUtils,
  SDUGeneral,
  SDUComCtrls,
  SDFilesystem;

type
{$M+}  // Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"

  TFilesystemListViewColumn = (
                               flvcFilename,
                               flvcSize,
                               flvcFiletype,
                               flvcTimestampModified,
                               flvcTimestampCreated,
                               flvcAttributes,
                               flvcTimestampAccessed
                              );
  TFilesystemListViewColumn_Layout = record
    Visible: boolean;
    Position: integer;
    Width: integer;
  end;

resourcestring
  FILESYSTEMLISTVIEWCOL_FILENAME           = 'Name';
  FILESYSTEMLISTVIEWCOL_SIZE               = 'Size';
  FILESYSTEMLISTVIEWCOL_FILETYPE           = 'Type';
  FILESYSTEMLISTVIEWCOL_TIMESTAMP_MODIFIED = 'Date Modified';
  FILESYSTEMLISTVIEWCOL_TIMESTAMP_CREATED  = 'Date Created';
  FILESYSTEMLISTVIEWCOL_ATTRIBUTES         = 'Attributes';
  FILESYSTEMLISTVIEWCOL_TIMESTAMP_ACCESSED = 'Date Accessed';


const
  FilesystemListViewColumnTitlePtr: array [TFilesystemListViewColumn] of Pointer = (
                                              @FILESYSTEMLISTVIEWCOL_FILENAME,
                                              @FILESYSTEMLISTVIEWCOL_SIZE,
                                              @FILESYSTEMLISTVIEWCOL_FILETYPE,
                                              @FILESYSTEMLISTVIEWCOL_TIMESTAMP_MODIFIED,
                                              @FILESYSTEMLISTVIEWCOL_TIMESTAMP_CREATED,
                                              @FILESYSTEMLISTVIEWCOL_ATTRIBUTES,
                                              @FILESYSTEMLISTVIEWCOL_TIMESTAMP_ACCESSED
                                             );
  // Internal names used when saving/restoring layout
  FilesystemListViewColumnIntName: array [TFilesystemListViewColumn] of string = (
                                              'NAME',
                                              'SIZE',
                                              'FILETYPE',
                                              'TS_MODIFIED',
                                              'TS_CREATED',
                                              'ATTRIBUTES',
                                              'TS_ACCESSED'
                                             );

  FilesystemListViewColumnAlignment: array [TFilesystemListViewColumn] of TAlignment = (
                                              taLeftJustify,
                                              taRightJustify,
                                              taLeftJustify,
                                              taLeftJustify,
                                              taLeftJustify,
                                              taLeftJustify,
                                              taLeftJustify
                                             );

type
  TFilesystemListView_Layout = array [TFilesystemListViewColumn] of TFilesystemListViewColumn_Layout;
  TFilesystemListView_ColOrder = array of TFilesystemListViewColumn;

const
  AUTOCALC_COL_WIDTH = -42;
  COL_ID_NONE = -1;

  FILESYSTEMLISTVIEWSTYLE_DEFAULT = vsReport;
  FILESYSTEMLISTVIEWCOLUMN_DEFAULTS: TFilesystemListView_Layout = (
                                          (Visible: TRUE;  Position: 0; Width: AUTOCALC_COL_WIDTH),
                                          (Visible: TRUE;  Position: 1; Width: 75),
                                          (Visible: TRUE;  Position: 2; Width: 150),
                                          (Visible: TRUE;  Position: 3; Width: 120),
                                          (Visible: FALSE; Position: 4; Width: 120),
                                          (Visible: TRUE;  Position: 5; Width: 100),
                                          (Visible: FALSE; Position: 6; Width: 120)
                                         );

type
  TNodeRec = record
    PlaceHolder: boolean;

    Name: string;

    ContentsLoaded: boolean;
    Contents: TSDDirItemList;

  end;
  PNodeRec = ^TNodeRec;

  // Forward declarations
  TSDLoadDirThread = class;
  TSDCustomFilesystemListView = class;


  TSDLoadDirThreadCallback = procedure (thread: TSDLoadDirThread) of object;

  TSDLoadDirThread = class(TThread)
  private
    FFilesystem: TSDCustomFilesystem;
    Paths: TStringList;
    procedure SyncMethod();
  protected
    procedure Execute(); override;
  public
    LoadedPath: string;
    LoadedContents: TSDDirItemList;
    Callback: TSDLoadDirThreadCallback;

    destructor Destroy(); override;
    procedure  AfterConstruction(); override;

    procedure AddPath(pathToLoad: string);

  published
    property Filesystem: TSDCustomFilesystem read FFilesystem write FFilesystem;

  end;
  

  TSDCustomFilesystemTreeView = class(TTreeView)
  private
    FFilesystem: TSDCustomFilesystem;
    FFilesystemListView: TSDCustomFilesystemListView;
    FNodeImgIdxClosed: integer;
    FNodeImgIdxOpen: integer;
    FNodeImgIdxDrive: integer;
    FNodeImages: TImageList;

    FShowHiddenItems: boolean;

    FCursorStack: TList;

    procedure AddRootNode();

    procedure AddPlaceHolderNode(Node: TTreeNode);
    procedure DeletePlaceHolderNodesFrom(Node: TTreeNode);
    function  NodeHasPlaceHolderNode(Node: TTreeNode): boolean;

    procedure CreateChildNodes(Node: TTreeNode);
    procedure DeleteAllNodes();

    procedure LoadContents(Node: TTreeNode);

    function  GetNodeForPath(path: string; farAsPossible: boolean): TTreeNode;

    procedure AddExpandedLeafNodes(Node: TTreeNode; expandedLeafNodes: TStringList);
  protected
    function  CanExpand(Node: TTreeNode): boolean; override;
    procedure Delete(Node: TTreeNode); override;
    procedure Change(Node: TTreeNode); override;

    procedure ThreadLoadedContents(thread: TSDLoadDirThread);

    procedure SetFilesystem(newFilesystem: TSDCustomFilesystem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure Initialize();

    function  PathToNode(Node: TTreeNode): string;
    function  GoToPath(path: string; farAsPossible: boolean): TTreeNode;

    procedure RefreshNodes();

    procedure BeginUpdate();
    procedure EndUpdate();

  published
    property Filesystem: TSDCustomFilesystem read FFilesystem write SetFilesystem;
    property FilesystemListView: TSDCustomFilesystemListView read FFilesystemListView write FFilesystemListView;

    property ShowHiddenItems: boolean read FShowHiddenItems write FShowHiddenItems default FALSE;

  end;

  TSDFilesystemTreeView = class(TSDCustomFilesystemTreeView)
  end;


  TSDCustomFilesystemListView = class(TSDListView)
  private
    FFilesystem: TSDCustomFilesystem;
    FFilesystemTreeView: TSDCustomFilesystemTreeView;
    FPath: string;
    FContents: TSDDirItemList;
    FShowParentDir: boolean;
    FShowHiddenItems: boolean;
    FHideKnownFileExtns: boolean;
    FSuppressPopupMenu: boolean;
    FColumnHeaderPopup: TPopupMenu;

    FDirectoriesAlwaysSortFirst: boolean;
    
    FFileExtnToIconMap_Small: TStringList;
    FFileExtnToIconMap_Large: TStringList;
    FIconsLarge: TImageList;
    FIconsSmall: TImageList;
    FColumnIDs: array [TFilesystemListViewColumn] of integer;

    FCursorStack: TList;

    FInternalColOrder: TFilesystemListView_ColOrder;
    FInternalLayout: TFilesystemListView_Layout;

    procedure RecreateAllPosibleColHeaders();
    // Update FInternalColOrder based on FInternalLayout
    procedure UpdateInternalColOrder();

  protected
    procedure DblClick(); override;

    function  LoadContents(path: string): boolean;
    procedure RepopulateDisplay();
    procedure SyncIcons();

    procedure GetIconsForFile(
                              filename: string;
                              subduedIcon: boolean;
                              out smallIconIdx: integer;
                              out largeIconIdx: integer
                             );
    procedure MakeIconSubdued(imgList: TImageList; idx: integer; treatAs32x32Icon: boolean);

    function  GetDisplayName(item: TSDDirItem): string;

    function  GetDirItemByListIdx(idx: integer): TSDDirItem;
    function  GetDirItemSelected(): TSDDirItem;
    function  GetDisplayedNameByListIdx(idx: integer): string;

    procedure SetFilesystem(newFilesystem: TSDCustomFilesystem);

    function  GetColumn(colType: TFilesystemListViewColumn): TListColumn;

    procedure DefaultInternalLayout();
    procedure SyncInternalLayoutToDisplayed();
    procedure SyncDisplayedToInternalLayout();
    function  GetLayout(): string;
    procedure SetLayout(newLayout: string);

    procedure InitPopup();
    function  GetPopupMenu: TPopupMenu; override;
    procedure ColRightClick(Column: TListColumn; Point: TPoint); override;
    procedure ColumnHeaderPopupClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    // Note: This is *not* a published property
    property Layout: string read GetLayout write SetLayout;

    procedure Initialize();

    procedure SetPath(path: string);

    procedure _ColumnSortCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer); override;

    property DirItem[idx: integer]: TSDDirItem read GetDirItemByListIdx;
    property DisplayedName[idx: integer]: string read GetDisplayedNameByListIdx;

    procedure BeginUpdate();
    procedure EndUpdate();
    procedure ChooseColumns();

  published
    property Filesystem: TSDCustomFilesystem read FFilesystem write SetFilesystem;
    property FilesystemTreeView: TSDCustomFilesystemTreeView read FFilesystemTreeView write FFilesystemTreeView;
    property Path: string read FPath write SetPath;
    property ShowParentDir: boolean read FShowParentDir write FShowParentDir default FALSE;
    property ShowHiddenItems: boolean read FShowHiddenItems write FShowHiddenItems default FALSE;
    property HideKnownFileExtns: boolean read FHideKnownFileExtns write FHideKnownFileExtns default TRUE;
    // If DirectoriesAlwaysSortFirst is set to TRUE, dirs will always appear
    // first when sorting by column
    // Otherwise, it'll follow MS Windows Explorer style, dirs all appear first
    // if ascending, last if descending
    property DirectoriesAlwaysSortFirst: boolean read FDirectoriesAlwaysSortFirst write FDirectoriesAlwaysSortFirst default FALSE;

    property DirItemSelected: TSDDirItem read GetDirItemSelected;

  end;

  TSDFilesystemListView = class(TSDCustomFilesystemListView)
  end;

procedure Register;

function GetLayoutColOrder(layout: TFilesystemListView_Layout; includeNonVisible: boolean): TFilesystemListView_ColOrder;
function FilesystemListViewColumnTitle(col: TFilesystemListViewColumn): string;


implementation

uses
  Messages, Graphics, Math,
  SDFilesystem_FAT,
  SDUi18n,
  SDUGraphics,
  SDFilesystemCtrls_ColDetails;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  PLACEHOLDER = '<placeholder node>';  // DO NOT TRANSLATE! Just a placeholder marker
  PATH_SEPARATOR = '\';

  // Marker tag
  COLHEADERPOPUPMENUITEM_MORE = 9999;  

procedure Register;
begin
  RegisterComponents('SDeanFilesystem', [TSDFilesystemTreeView]);
  RegisterComponents('SDeanFilesystem', [TSDFilesystemListView]);
end;


function FilesystemListViewColumnTitle(col: TFilesystemListViewColumn): string;
begin
  Result := LoadResString(FilesystemListViewColumnTitlePtr[col]);
end;


function GetLayoutColOrder(layout: TFilesystemListView_Layout; includeNonVisible: boolean): TFilesystemListView_ColOrder;
var
  retval: TFilesystemListView_ColOrder;
  colType: TFilesystemListViewColumn;
  order: TStringList;
  i: integer;
begin
  order:= TStringList.Create();
  try
    for colType:=low(colType) to high(colType) do
      begin
      if (
          layout[colType].Visible or
          includeNonVisible
         ) then
        begin
        // Use IntToHex so that we have padding "0"'s, and the TStringList sort
        // sorts the items in the correct order
        order.AddObject(IntToHex(layout[colType].Position, 4), TObject(colType));
        end;
      end;

    order.Sorted := TRUE;

    SetLength(retval, order.Count);
    for i:=0 to (order.Count - 1) do
      begin
      retval[i] := TFilesystemListViewColumn(order.Objects[i]);
      end;

  finally
    order.Free();
  end;

  Result := retval;
end;

// farAsPossible - If the path isn't available, go as far along it as possible
function TSDCustomFilesystemTreeView.GoToPath(path: string; farAsPossible: boolean): TTreeNode;
var
  targetNode: TTreeNode;
  tmpNode: TTreeNode;
begin
  BeginUpdate();
  try
    targetNode := GetNodeForPath(path, farAsPossible);
    if (targetNode <> nil) then
      begin
      tmpNode := targetNode.Parent;
      while (tmpNode <> nil) do
        begin
        tmpNode.Expand(FALSE);
        tmpNode := tmpNode.Parent;
        end;

      targetNode.Selected := TRUE;
      targetNode.Focused := TRUE;
      end;

  finally
    EndUpdate();
  end;

  Result := targetNode;
end;

procedure TSDCustomFilesystemTreeView.BeginUpdate();
begin
  FCursorStack.Add(Pointer(self.Cursor));
  self.Cursor := crHourglass;
  Perform(WM_SETCURSOR, Handle, HTCLIENT);  // Force cursor display to update
  Items.BeginUpdate();
end;

procedure TSDCustomFilesystemTreeView.EndUpdate();
begin
  if (FCursorStack.Count > 0) then
    begin
    self.Cursor := TCursor(FCursorStack[FCursorStack.Count-1]);
    Perform(WM_SETCURSOR, Handle, HTCLIENT);  // Force cursor display to update
    FCursorStack.Delete(FCursorStack.Count - 1);
    end;
    
  Items.EndUpdate();
end;

// farAsPossible - If the path isn't available, go as far along it as possible
function TSDCustomFilesystemTreeView.GetNodeForPath(path: string; farAsPossible: boolean): TTreeNode;
var
  retval: TTreeNode;
  pathComponents: TStringList;
  i: integer;
  j: integer;
  lastNode: TTreeNode;
  checkNode: TTreeNode;
  checkNodeRec: PNodeRec;
  allOK: boolean;
  pathPartNode: TTreeNode;
  stlTmp: TStringList;
  pathFirst: string;
  pathRest: string;
  currNodeName: string;
begin
  retval := nil;
  allOK := TRUE;

  // Normalise path
  path := Trim(path);
  path := StringReplace(path, '/', PATH_SEPARATOR, [rfReplaceAll]);
  path := StringReplace(path, '\', PATH_SEPARATOR, [rfReplaceAll]);

  if not(Filesystem.CaseSensitive) then
    begin
    path := uppercase(path);
    end;

  pathComponents := TStringList.Create();
  try
    // Sanity check; a path was passed in, right?
    if allOK then
      begin
      if (length(path) <= 0) then
        begin
        allOK := FALSE;
        end;
      end;

    // Sanity check; the path started with a PATH_SEPARATOR, right?
    if allOK then
      begin
      if (path[1] <> PATH_SEPARATOR) then
        begin
        allOK := FALSE;
        end;
      end;

    // Split path into TStringList
    if allOK then
      begin
      // Remove leading PATH_SEPARATOR; otherwise this would cause the first
      // item taken off with SDUSplitString(...) to be a blank
      System.Delete(path, 1, 1);

      while SDUSplitString(path, pathFirst, pathRest, PATH_SEPARATOR) do
        begin
        pathComponents.Add(pathFirst);
        path := pathRest;
        end;

      // Reinsert the leading "/" we previously took off
      pathComponents.Insert(0, PATH_SEPARATOR);
      end;


    // Handle any "." and ".." in path
    if allOK then
      begin
      stlTmp := TStringList.Create();
      try
        for i:=0 to (pathComponents.Count - 1) do
          begin
          if (pathComponents[i] = DIR_CURRENT_DIR) then
            begin
            // Do nothing - skip
            end
          else if (pathComponents[i] = DIR_PARENT_DIR) then
            begin
            if (stlTmp.Count > 0) then
              begin
              stlTmp.Delete(stlTmp.count-1);
              end
            else
              begin
              allOK := FALSE;
              break;
              end;
            end
          else
            begin
            stlTmp.Add(pathComponents[i]);
            end;

          end;

        pathComponents.Assign(stlTmp);
      finally
        stlTmp.Free();
      end;
      
      end;


    // Seek node...
    if allOK then
      begin
      // Root node is always the root node
      lastNode := self.Items.GetFirstNode();
      for i:=1 to (pathComponents.Count - 1) do
        begin
        // Create nodes as we go, if required...
        if NodeHasPlaceHolderNode(lastNode) then
          begin
          CreateChildNodes(lastNode);
          end;

        // Check all of the current node's children to try and find the next
        // path element
        pathPartNode := nil;
        for j:=0 to (lastNode.Count - 1) do
          begin
          checkNode := lastNode.Item[j];
          checkNodeRec := checkNode.Data;

          // (We're case insensitive (and below))
          currNodeName := checkNodeRec.Name;
          if not(Filesystem.CaseSensitive) then
            begin
            currNodeName := uppercase(currNodeName);
            end;

          if (currNodeName = pathComponents[i]) then
            begin
            pathPartNode := checkNode;
            break;
            end;
            
          end;

        if (pathPartNode = nil) then
          begin
          allOK := FALSE;
          break;
          end;

        lastNode := pathPartNode;
        end;

      retval := lastNode;
      end;

    // If there was a problem, make sure we return NIL
    if (
        not(farAsPossible) and
        not(allOK)
       ) then
      begin
      retval := nil;
      end;

  finally
    pathComponents.Free();
  end;

  Result := retval;
end;


constructor TSDCustomFilesystemTreeView.Create(AOwner: TComponent);
begin
  inherited;

  FShowHiddenItems := FALSE;

  FCursorStack := TList.Create();

  // Prevent user from changing node captions
  self.Readonly := TRUE;

  // Leave visual indicator of selected node when control not focussed
  self.HideSelection := FALSE;

  // No line to the root node
  self.ShowRoot := FALSE;

  FNodeImages:= TImageList.Create(nil);
  FNodeImages.BlendColor := self.Color;
  FNodeImages.BkColor := self.Color;

  // Get icon file and icon index within icon file
  FNodeImgIdxClosed := SDULoadDLLIconToList(
                                              DLL_SHELL32,
                                              TRUE,
                                              DLL_SHELL32_FOLDER_CLOSED,
                                              FNodeImages
                                             );
  FNodeImgIdxOpen   := SDULoadDLLIconToList(
                                              DLL_SHELL32,
                                              TRUE,
                                              DLL_SHELL32_FOLDER_OPEN,
                                              FNodeImages
                                             );
  FNodeImgIdxDrive := SDULoadDLLIconToList(
                                              DLL_SHELL32,
                                              TRUE,
                                              DLL_SHELL32_HDD,
                                              FNodeImages
                                             );

end;

destructor  TSDCustomFilesystemTreeView.Destroy();
begin
  FNodeImages.Free();
  FCursorStack.Free();
  inherited;
end;

procedure TSDCustomFilesystemTreeView.Initialize();
begin
  self.Images := FNodeImages;
  DeleteAllNodes();
  AddRootNode();
end;

procedure TSDCustomFilesystemTreeView.SetFilesystem(newFilesystem: TSDCustomFilesystem);
begin
  FFilesystem := newFilesystem;
  if (Filesystem = nil) then
    begin
    DeleteAllNodes();
    end;
end;

procedure TSDCustomFilesystemTreeView.AddRootNode();
var
  newNode: TTreeNode;
  newNodeRec: PNodeRec;
begin
  // Create initial root node
  newNodeRec := new(PNodeRec);
  newNodeRec.PlaceHolder := FALSE;
  newNodeRec.Name := PATH_SEPARATOR;
  newNodeRec.ContentsLoaded := FALSE;
  newNodeRec.Contents := nil;
  newNode:= self.Items.AddFirst(nil, newNodeRec.Name);
  newNode.Data := newNodeRec;
  newNode.ImageIndex := FNodeImgIdxDrive;
  newNode.SelectedIndex := FNodeImgIdxDrive;

  AddPlaceHolderNode(newNode);

  newNode.Selected := TRUE;
  newNode.Focused := TRUE;
end;

procedure TSDCustomFilesystemTreeView.AddPlaceHolderNode(Node: TTreeNode);
var
  newNode: TTreeNode;
  newNodeRec: PNodeRec;
begin
  newNodeRec := new(PNodeRec);
  newNodeRec.PlaceHolder := TRUE;
  newNodeRec.Name := PLACEHOLDER;
  newNodeRec.ContentsLoaded := FALSE;
  newNode := self.Items.AddChild(Node, newNodeRec.Name);
  newNode.Data := newNodeRec;
end;

// Delete all place holder nodes from the specified node
procedure TSDCustomFilesystemTreeView.DeletePlaceHolderNodesFrom(Node: TTreeNode);
var
  currNode: TTreeNode;
  currNodeRec: PNodeRec;
  i: integer;
begin
  for i:=(Node.Count - 1) downto 0 do
    begin
    currNode := Node.Item[i];
    currNodeRec := currNode.Data;
    if currNodeRec.PlaceHolder then
      begin
      Node.Item[i].Delete();
      end;

    end;

end;

function TSDCustomFilesystemTreeView.NodeHasPlaceHolderNode(Node: TTreeNode): boolean;
var
  currNode: TTreeNode;
  currNodeRec: PNodeRec;
  i: integer;
  retval: boolean;
begin
  retval := FALSE;

  for i:=(Node.Count - 1) downto 0 do
    begin
    currNode := Node.Item[i];
    currNodeRec := currNode.Data;
    if currNodeRec.PlaceHolder then
      begin
      retval := TRUE;
      break
      end;

    end;

  Result := retval;
end;

procedure TSDCustomFilesystemTreeView.DeleteAllNodes();
begin
  self.Items.Clear();
end;

procedure TSDCustomFilesystemTreeView.CreateChildNodes(Node: TTreeNode);
var
  newNode: TTreeNode;
  nodeRec: PNodeRec;
  newNodeRec: PNodeRec;
  i: integer;
  nodePath: string;
  currContent: TSDDirItem;
  showItemInTree: boolean;
begin
  nodeRec := Node.Data;

  LoadContents(Node);
  if nodeRec.ContentsLoaded then
    begin
    DeletePlaceHolderNodesFrom(Node);

    nodePath := PathToNode(Node);

    for i:=0 to (nodeRec.Contents.Count - 1) do
      begin
      currContent := nodeRec.Contents[i];

      showItemInTree := (
                         currContent.IsDirectory and
                         (currContent.Filename <> DIR_CURRENT_DIR) and
                         (currContent.Filename <> DIR_PARENT_DIR)
                        );

      // Skip hidden items, unless this control is set to show them
      if (
          currContent.IsHidden and
          not(ShowHiddenItems)
         ) then
        begin
        showItemInTree := FALSE;
        end;

      if showItemInTree then
        begin
        newNodeRec := new(PNodeRec);
        newNodeRec.PlaceHolder := FALSE;
        newNodeRec.Name := currContent.Filename;
        newNodeRec.ContentsLoaded := FALSE;
        newNodeRec.Contents := nil;
        newNode:= self.Items.AddChild(Node, newNodeRec.Name);
        newNode.Data := newNodeRec;
        newNode.ImageIndex := FNodeImgIdxClosed;
        newNode.SelectedIndex:= FNodeImgIdxOpen;

        AddPlaceHolderNode(newNode);
        end;

      end;

    // Get the nodes just added into alphabetical order
    Node.AlphaSort(FALSE);
    end;

end;

procedure TSDCustomFilesystemTreeView.ThreadLoadedContents(thread: TSDLoadDirThread);
var
  node: TTreeNode;
  nodeRec: PNodeRec;
begin
  node := GetNodeForPath(thread.LoadedPath, FALSE);
  if (node <> nil) then
    begin
    nodeRec := Node.Data;

    if not(nodeRec.ContentsLoaded) then
      begin
      nodeRec.Contents := TSDDirItemList.Create();
      nodeRec.Contents.Assign(thread.LoadedContents);
      nodeRec.ContentsLoaded := TRUE;

      if NodeHasPlaceHolderNode(node) then
        begin
        CreateChildNodes(node);
        end;

      end;

    end;

end;

procedure TSDCustomFilesystemTreeView.LoadContents(Node: TTreeNode);
var
  nodeRec: PNodeRec;
  tmpDirItemList: TSDDirItemList;
begin
  noderec := Node.Data;
  if not(nodeRec.ContentsLoaded) then
    begin
    tmpDirItemList := TSDDirItemList.Create();
    try
      nodeRec.ContentsLoaded := FFileSystem.LoadContentsFromDisk(PathToNode(Node), tmpDirItemList);

      if nodeRec.ContentsLoaded then
        begin
        nodeRec.Contents := tmpDirItemList;
        end
      else
        begin
        // If there was a problem, free off dir list object created
        tmpDirItemList.Free();
        end;
        
    except
      on E:Exception do
        begin
        tmpDirItemList.Free();
        end;
    end;

    end;

end;

function TSDCustomFilesystemTreeView.PathToNode(Node: TTreeNode): string;
var
  retval: string;
  currNode: TTreeNode;
  currNodeRec: PNodeRec;
begin
  retval := '';

  currNode := Node;
  while (currNode <> nil) do
    begin
    currNodeRec := currNode.Data;

    if (retval = '') then
      begin
      retval := currNodeRec.Name;
      end
    else
      begin
      if (currNode.Parent = nil) then
        begin
        retval := PATH_SEPARATOR + retval;
        end
      else
        begin
        retval := currNodeRec.Name + PATH_SEPARATOR + retval;
        end;
      end;
      
    currNode := currNode.Parent;             
    end;

  Result := retval;
end;

procedure TSDCustomFilesystemTreeView.AddExpandedLeafNodes(Node: TTreeNode; expandedLeafNodes: TStringList);
var
  i: integer;
  preChildCnt: integer;
begin
  // Add all expanded leaf nodes to expandedLeafNodes (place holder nodes not
  // considered leaf nodes)

  if Node.Expanded then
    begin
    preChildCnt := expandedLeafNodes.Count;

    for i:=0 to (Node.Count - 1) do
      begin
      AddExpandedLeafNodes(Node.Item[i], expandedLeafNodes);
      end;

    // If none of the child nodes where expanded, add this node on
    if (preChildCnt = expandedLeafNodes.Count) then
      begin
      expandedLeafNodes.Add(PathToNode(Node));
      end;
    end;

end;

function TSDCustomFilesystemTreeView.CanExpand(Node: TTreeNode): boolean;
var
  i: integer;
  currNode: TTreeNode;
  currThread: TSDLoadDirThread;
  retval: boolean;
begin
  retval := inherited CanExpand(Node);

  if retval then
    begin
    BeginUpdate();
    try
      if NodeHasPlaceHolderNode(Node) then
        begin
        CreateChildNodes(Node);
        end;

      retval := Node.HasChildren;

      currThread := TSDLoadDirThread.Create(TRUE);
      for i:=0 to (Node.Count - 1) do
        begin
        currNode := Node.Item[i];
        currThread.AddPath(PathToNode(currNode));
        end;
      currThread.Filesystem := self.Filesystem;
      currThread.Callback := ThreadLoadedContents;
      currThread.FreeOnTerminate := TRUE;
      // Put the priority of the thread down; the application gets sluggish
      // otherwise
      currThread.Priority := tpLower;
      currThread.Resume();
    finally
      EndUpdate();
    end;

    end;

  Result := retval;
end;

procedure TSDCustomFilesystemTreeView.Delete(Node: TTreeNode);
var
  nodeRec: PNodeRec;
begin
  inherited;

  nodeRec := Node.Data;

  if not(nodeRec.PlaceHolder) then
    begin
    if nodeRec.ContentsLoaded then
      begin
      nodeRec.ContentsLoaded := FALSE;
      end;

    if (nodeRec.Contents <> nil) then
      begin
      nodeRec.Contents.Free();
      nodeRec.Contents := nil;
      end;

    end;

  Dispose(nodeRec);

end;

procedure TSDCustomFilesystemTreeView.Change(Node: TTreeNode);
begin
  LoadContents(Node);

  if NodeHasPlaceHolderNode(Node) then
    begin
    CreateChildNodes(Node);
    end;

  if Assigned(FFilesystemListView) then
    begin
    FFilesystemListView.SetPath(PathToNode(Node));
    end;

  inherited;

end;

procedure TSDCustomFilesystemTreeView.RefreshNodes();
var
  expandedLeafNodes: TStringList;
  i: integer;
  currNode: TTreeNode;
  lastPath: string;
  lastExpanded: boolean;
begin
  BeginUpdate();
  try
    lastPath := PathToNode(self.Selected);
    lastExpanded := FALSE;
    if (self.Selected <> nil) then
      begin
      lastExpanded := self.Selected.Expanded;
      end;

    expandedLeafNodes:= TStringList.Create();
    try
      // Note: We only ever process the one root node
      AddExpandedLeafNodes(self.Items.GetFirstNode(), expandedLeafNodes);

      DeleteAllNodes();
      AddRootNode();

      for i:=0 to (expandedLeafNodes.count - 1) do
        begin
        currNode := GoToPath(expandedLeafNodes[i], TRUE);
        currNode.Expanded := TRUE;
        end;

    finally
      expandedLeafNodes.Free();
    end;

    currNode := GoToPath(lastPath, TRUE);
    if (lastPath = PathToNode(currNode)) then
      begin
      currNode.Expanded := lastExpanded;
      end
    else
      begin
      currNode.Expanded := TRUE;
      end;

  finally
    EndUpdate();
  end;

end;

procedure TSDCustomFilesystemListView.SetFilesystem(newFilesystem: TSDCustomFilesystem);
begin
  FFilesystem := newFilesystem;
  if (Filesystem = nil) then
    begin
    self.Clear();
    end;
end;

function TSDCustomFilesystemListView.GetColumn(colType: TFilesystemListViewColumn): TListColumn;
var
  retval: TListColumn;
begin
  retval := nil;

  if (FColumnIDs[colType] <> COL_ID_NONE) then
    begin
    retval := self.GetColumnByID(FColumnIDs[colType]);
    end;

  Result := retval;
end;


procedure TSDCustomFilesystemListView.UpdateInternalColOrder();
begin
  FInternalColOrder := GetLayoutColOrder(FInternalLayout, FALSE);
end;

procedure TSDCustomFilesystemListView.RecreateAllPosibleColHeaders();
var
  currColType: TFilesystemListViewColumn;
  newCol: TListColumn;
begin
  // Clear existing columns...
  self.Columns.Clear();

  // Create list column for all possible columns
  for currColType:=low(currColType) to high(currColType) do
    begin
    newCol := self.AddCol(FilesystemListViewColumnTitle(currColType));
    newCol.Alignment := FilesystemListViewColumnAlignment[currColType];
    FColumnIDs[currColType] := newCol.ID;
    end;

end;


procedure TSDCustomFilesystemListView.DefaultInternalLayout();
begin
  FInternalLayout := FILESYSTEMLISTVIEWCOLUMN_DEFAULTS;

  UpdateInternalColOrder();

end;


function TSDCustomFilesystemListView.GetLayout(): string;
var
  retval: string;
  currColType: TFilesystemListViewColumn;
  stlColLayout: TStringList;
  colTypeName: string;
  colLayout: TFilesystemListViewColumn_Layout;
begin
  retval := '';

  SyncInternalLayoutToDisplayed();

  stlColLayout:= TStringList.Create();
  try
    stlColLayout.Values['VIEWSTYLE'] := inttostr(ord(self.ViewStyle));

    for currColType:=low(currColType) to high(currColType) do
      begin
      colTypeName := FilesystemListViewColumnIntName[currColType];
      colLayout := FInternalLayout[currColType];

      stlColLayout.Values[colTypeName+'_VISIBLE'] := SDUBoolToStr(colLayout.Visible);
      stlColLayout.Values[colTypeName+'_WIDTH'] := inttostr(colLayout.Width);
      stlColLayout.Values[colTypeName+'_POSITION'] := inttostr(colLayout.Position);
      end;

    stlColLayout.Delimiter := ',';
    stlColLayout.QuoteChar := '"';
    retval := stlColLayout.DelimitedText;
  finally
    stlColLayout.Free();
  end;

  Result := retval;
end;


procedure TSDCustomFilesystemListView.SetLayout(newLayout: string);

  function GetStlValueOrDefault(stl: TStringList; itemName: string; defaultValue: string): string;
  var
    idx: integer;
    retval: string;
  begin
    idx := stl.IndexOfName(itemName);

    retval := defaultValue;
    if (idx >= 0) then
      begin
      retval := stl.Values[itemName];
      end;

    Result := retval;
  end;

var
  stlColLayout: TStringList;
  currColType: TFilesystemListViewColumn;
  colTypeName: string;
  colLayout: TFilesystemListViewColumn_Layout;
begin
  // Set column defaults in case they're not mentioned in the layout provided
  DefaultInternalLayout();

  stlColLayout:= TStringList.Create();
  try
    stlColLayout.Delimiter := ',';
    stlColLayout.QuoteChar := '"';
    stlColLayout.DelimitedText := newLayout;

    self.ViewStyle := TViewStyle(StrToInt(GetStlValueOrDefault(
                                                       stlColLayout,
                                                       'VIEWSTYLE',
                                                       inttostr(ord(FILESYSTEMLISTVIEWSTYLE_DEFAULT))
                                                      )));

    for currColType:=low(currColType) to high(currColType) do
      begin
      // Get pre-set defaults
      colLayout := FInternalLayout[currColType];

      colTypeName := FilesystemListViewColumnIntName[currColType];

      colLayout.Visible := SDUStrToBool(GetStlValueOrDefault(
                                                              stlColLayout,
                                                              colTypeName+'_VISIBLE',
                                                              SDUBoolToStr(colLayout.Visible)
                                                             ));
      colLayout.Width := StrToInt(GetStlValueOrDefault(
                                                       stlColLayout,
                                                       colTypeName+'_WIDTH',
                                                       inttostr(colLayout.Width)
                                                      ));
      colLayout.Position := StrToInt(GetStlValueOrDefault(
                                                       stlColLayout,
                                                       colTypeName+'_POSITION',
                                                       inttostr(colLayout.Position)
                                                      ));

      FInternalLayout[currColType] := colLayout;
      end;

  finally
    stlColLayout.Free();
  end;

  UpdateInternalColOrder();

  RepopulateDisplay();
end;


procedure TSDCustomFilesystemListView.SyncInternalLayoutToDisplayed();
var
  retval: string;
  currColType: TFilesystemListViewColumn;
  listCol: TListColumn;
  colTypeName: string;
  colLayout: TFilesystemListViewColumn_Layout;
begin
  retval := '';

  for currColType:=low(currColType) to high(currColType) do
    begin
    colTypeName := FilesystemListViewColumnIntName[currColType];

    listCol := GetColumn(currColType);

    if (listCol = nil) then
      begin
      colLayout.Visible := FALSE;
      colLayout.Width := FILESYSTEMLISTVIEWCOLUMN_DEFAULTS[currColType].Width;
      colLayout.Position := FILESYSTEMLISTVIEWCOLUMN_DEFAULTS[currColType].Position;
      end
    else
      begin
      colLayout.Visible := TRUE;
      colLayout.Position := listCol.Index;
      colLayout.Width := RealColWidth[listCol];
      end;

    // MS Windows Explorer doens't allow zero width columns - it sets then back
    // to the default width
    if (colLayout.Width <= 0) then
      begin
      colLayout.Width := FILESYSTEMLISTVIEWCOLUMN_DEFAULTS[currColType].Width;
      end;

    FInternalLayout[currColType] := colLayout;
    end;

  UpdateInternalColOrder();

end;

// Note: This is called automatically as part of RepopulateDisplay(...)
procedure TSDCustomFilesystemListView.SyncDisplayedToInternalLayout();
var
  currColType: TFilesystemListViewColumn;
  listCol: TListColumn;
  currColLayout: TFilesystemListViewColumn_Layout;
  i: integer;
  totalWidthUsed: integer;
  autoSizeCol: TListColumn;
  newColIdx: integer;
begin
  UpdateInternalColOrder();

  // Delete any columns which aren't visible
  for currColType:=low(currColType) to high(currColType) do
    begin
    if (currColType = flvcFilename) then
      begin
      // "Filename" column *always* visible
      continue;
      end;

    currColLayout := FInternalLayout[currColType];

    if not(currColLayout.Visible) then
      begin
      listCol := GetColumn(currColType);

      if (listCol <> nil) then
        begin
        // Delete all subitems for that column
        for i:=0 to (self.Items.count - 1) do
          begin
          self.Items[i].SubItems.Delete(listCol.Index - 1);
          end;

        Self.Columns.Delete(listCol.Index);
        end;

      end;

    end;

  // Put columns into layout order...
  newColIdx := 0;
  for i:=low(FInternalColOrder) to high(FInternalColOrder) do
    begin
    currColType := FInternalColOrder[i];
    currColLayout := FInternalLayout[currColType];
    listCol := GetColumn(currColType);

    if (listCol <> nil) then
      begin
      // Note: DON'T USE currColLayout.Position HERE!
      // It *might* be wrong if a column was just removed...
      listCol.Index := newColIdx;
      inc(newColIdx);
      end;

    end;

  // Resize columns...
  // NOTE: This is done in a SEPARATE LOOP than the column ordering to prevent
  //       problems with Delphi 2007 and earlier misreporting/setting column
  //       widths
  totalWidthUsed := 0;
  autoSizeCol := nil;
  for i:=low(FInternalColOrder) to high(FInternalColOrder) do
    begin
    currColType := FInternalColOrder[i];
    currColLayout := FInternalLayout[currColType];
    listCol := GetColumn(currColType);

    if (listCol <> nil) then
      begin
      if (currColLayout.Width = AUTOCALC_COL_WIDTH) then
        begin
        autoSizeCol := listCol;
        end
      else
        begin
        RealColWidth[listCol] := currColLayout.Width;
        totalWidthUsed:= totalWidthUsed + listCol.Width;
        end;
      end;

    end;

  // If any of the columns were set to autosize, we do that now.
  if (autoSizeCol <> nil) then
    begin
    // -50 so the scrollbar doesn't show when first shown
    RealColWidth[autoSizeCol] := max(
                             100,
                             (self.Width - totalWidthUsed - 50)
                            );
    end;
    
end;


procedure TSDCustomFilesystemListView.BeginUpdate();
begin
  FCursorStack.Add(Pointer(self.Cursor));
  self.Cursor := crHourglass;
  Perform(WM_SETCURSOR, Handle, HTCLIENT);  // Force cursor display to update
  Items.BeginUpdate();
end;

procedure TSDCustomFilesystemListView.EndUpdate();
begin
  if (FCursorStack.Count > 0) then
    begin
    self.Cursor := TCursor(FCursorStack[FCursorStack.Count-1]);
    Perform(WM_SETCURSOR, Handle, HTCLIENT);  // Force cursor display to update
    FCursorStack.Delete(FCursorStack.Count - 1);
    end;
    
  Items.EndUpdate();
end;

procedure TSDCustomFilesystemListView.SetPath(path: string);
begin
  BeginUpdate();
  try
    self.Clear();  // Clear the display; otherwise when self.Clear() is called
                   // in RepopulateDisplay(...) (below), it can fire off the
                   // OnChanged event - at which point the display will be out
                   // of sync with FContents - and if the OnChange event
                   // accesses FContents via DirItem[...] based on self.Count,
                   // it can crash the DirItem[...] caller
    try
      if LoadContents(path) then         begin
        FPath := path;
        end;
    finally
      SyncInternalLayoutToDisplayed();
      RepopulateDisplay();
    end;

  finally
    EndUpdate();
  end;
  
end;

// Update icons for display
// Needed as large icon indexes may not match small icon indexes
procedure TSDCustomFilesystemListView.SyncIcons();
var
  i: integer;
  dirItem: TSDDirItem;
  iconIdxSmall: integer;
  iconIdxLarge: integer;
begin
  for i:=0 to (Self.Items.Count - 1) do
    begin
    dirItem := GetDirItemByListIdx(i);
    if dirItem.IsDirectory then
      begin
      GetIconsForFile(
                      FILE_TYPE_DIRECTORY,
                      dirItem.IsHidden,
                      iconIdxSmall,
                      iconIdxLarge
                     );
      end
    else
      begin
      GetIconsForFile(
                      dirItem.Filename,
                      dirItem.IsHidden,
                      iconIdxSmall,
                      iconIdxLarge
                     );
      end;

    if (self.ViewStyle = vsIcon) then
      begin
      self.Items[i].ImageIndex := iconIdxLarge;
      end
    else
      begin
      self.Items[i].ImageIndex := iconIdxSmall;
      end;
    end;

end;

// Clear and repopulate display with loaded dir content
procedure TSDCustomFilesystemListView.RepopulateDisplay();

  procedure ValuesToEnumOrder(
                              Filename: string;
                              Size: string;
                              FileType: string;
                              DateModified: string;
                              DateCreated: string;
                              Attributes: string;
                              DateAccessed: string;

                              stlUseColValues: TStringList
                              );
  var
    currColType: TFilesystemListViewColumn;
  begin
    stlUseColValues.Clear();

    for currColType:=low(currColType) to high(currColType) do
      begin
      case currColType of
        flvcFilename:
          begin
          stlUseColValues.Add(Filename);
          end;

        flvcSize:
          begin
          stlUseColValues.Add(Size);
          end;

        flvcFiletype:
          begin
          stlUseColValues.Add(FileType);
          end;

        flvcTimestampModified:
          begin
          stlUseColValues.Add(DateModified);
          end;

        flvcTimestampCreated:
          begin
          stlUseColValues.Add(DateCreated);
          end;

        flvcAttributes:
          begin
          stlUseColValues.Add(Attributes);
          end;

        flvcTimestampAccessed:
          begin
          stlUseColValues.Add(DateAccessed);
          end;

      end;
    end;

  end;

var
  i: integer;
  size: string;
  fileType: string;
  sizeKB: integer;
  dateModified: string;
  dateCreated: string;
  dateAccessed: string;
  attributes: string;
  currItem: TSDDirItem;
  currItemFAT: TSDDirItem_FAT;
  stlUseColValues: TStringList;
  dispFilename: string;
  knownFiletype: boolean;
begin
  BeginUpdate();
  try
    stlUseColValues := TStringList.Create();
    try
      self.Clear();

      RecreateAllPosibleColHeaders();

      if (FContents <> nil) then         begin
        for i:=0 to (FContents.Count - 1) do           begin
          currItem := FContents[i];

          currItemFAT := nil;
          if (currItem is TSDDirItem_FAT) then
            begin
            currItemFAT := TSDDirItem_FAT(currItem);
            end;

          // Skip volume labels, devices, and "."
          // Also skip "..", unless ShowParentDir is TRUE
          if not(
                 currItem.IsFile or
                 (
                  // It's a directory, but not the current directory
                  currItem.IsDirectory and
                  (currItem.Filename <> DIR_CURRENT_DIR) and
                  (
                   (currItem.Filename <> DIR_PARENT_DIR) or
                   (
                    (currItem.Filename = DIR_PARENT_DIR) and
                    ShowParentDir 
                   )
                  )
                 )
                ) then
            begin
            continue;
            end;

        // Skip hidden items, unless this control is set to show them
        if (
            currItem.IsHidden and
            not(ShowHiddenItems)
           ) then
          begin
          continue;
          end;

          size := '';
          knownFiletype := FALSE;
          if currItem.IsDirectory then
            begin
            fileType := SDUGetFileType_Description(
                                                   FILE_TYPE_DIRECTORY,
                                                   knownFiletype
                                                  );
            end
          else
            begin
            // Always display size in KB
            sizeKB := (currItem.Size div BYTES_IN_KILOBYTE);
            // Round up, like MS Explorer does...
            if ((currItem.Size mod BYTES_IN_KILOBYTE) > 0) then
              begin
              sizeKB := sizeKB + 1;
              end;
            // Format with thousands separator and KB units
            size := SDUIntToStrThousands(sizeKB) + ' ' + UNITS_STORAGE_KB;

            fileType := SDUGetFileType_Description(
                                                   currItem.Filename,
                                                   knownFiletype
                                                  );
            end;

          dateModified := '';
          if (currItem.TimestampLastModified.Date <> 0) then
            begin
            dateModified := DateTimeToStr(TimeStampToDateTime(currItem.TimestampLastModified));
            end;

          dateCreated := '';
          dateAccessed := '';
          if (currItem is TSDDirItem_FAT) then
            begin
            if (currItemFAT.TimestampCreation.Date <> 0) then
              begin
              dateCreated := DateTimeToStr(TimeStampToDateTime(currItemFAT.TimestampCreation));
              end;

            if (currItemFAT.DatestampLastAccess <> 0) then
              begin
              dateAccessed := DateToStr(currItemFAT.DatestampLastAccess);
              end;
            end;

          attributes := '';
          if currItem.IsReadonly then
            begin
            attributes := attributes + _('R');
            end;
          if (currItem is TSDDirItem_FAT) then
            begin
            if currItemFAT.IsHidden then
              begin
              attributes := attributes + _('H');
              end;
            if currItemFAT.IsSystem then
              begin
              attributes := attributes + _('S');
              end;
            if currItemFAT.IsArchive then
              begin
              attributes := attributes + _('A');
              end;
            end;

          dispFilename := currItem.Filename;
          if (
              HideKnownFileExtns and
              knownFiletype and
              not(currItem.IsDirectory)
             ) then
            begin
            dispFilename := ChangeFileExt(currItem.Filename, '');
            end;

          // Order the values to the same order they appear in the enum - this
          // is the order of the column headers after they've all been recreated
          ValuesToEnumOrder(
                             dispFilename,
                             size,
                             fileType,
                             dateModified,
                             dateCreated,
                             attributes,
                             dateAccessed,

                             stlUseColValues
                            );

          Self.AppendRow(stlUseColValues, Pointer(i));
          end;

        SyncIcons();
      end;

    finally
      stlUseColValues.Free();
    end;

    // Sort the items...
  // These two methods of sorting don't work?!
  //  self.SortType := stText;
  //  self.AlphaSort();
    self.Tag := 1;  // Ensure that sort is on filename
    self.ColClick(GetColumn(flvcFilename));

    SyncDisplayedToInternalLayout();

  finally
    EndUpdate();
  end;

end;

procedure TSDCustomFilesystemListView.DblClick();
var
  contentsIdx: integer;
  target: string;
begin
  inherited;

  if (self.SelCount = 1) then
    begin
    contentsIdx := integer(self.Selected.Data);
    target := IncludeTrailingPathDelimiter(Path) + FContents[contentsIdx].Filename;

    if Assigned(FilesystemTreeView) then
      begin
      FilesystemTreeView.GoToPath(target, TRUE);
      end
    else
      begin
      self.SetPath(target);
      end;

    end;

end;

function TSDCustomFilesystemListView.LoadContents(path: string): boolean;
var
  tmpDirItemList: TSDDirItemList;
begin
  Result := FALSE;

  tmpDirItemList := TSDDirItemList.Create();
  try
    Result := FFileSystem.LoadContentsFromDisk(path, tmpDirItemList);
    if Result then      begin
      if (FContents <> nil) then        begin
        FContents.Free();
     end;

      FContents := tmpDirItemList;
     end    else      begin
      // If there was a problem, free off dir list object created
      tmpDirItemList.Free();
      end;

  except
    on E:Exception do
      tmpDirItemList.Free();
  end;

end;

constructor TSDCustomFilesystemListView.Create(AOwner: TComponent);
var
  currColType: TFilesystemListViewColumn;
begin
  FDirectoriesAlwaysSortFirst := FALSE;

  inherited;

  // Allow the user to reorder the columns by dragging them around
  self.FullDrag := TRUE;
  
  // Prevent user from changing filenames
  self.Readonly := TRUE;

  FCursorStack := TList.Create();

  FShowParentDir := FALSE;

  for currColType:=low(currColType) to high(currColType) do
    begin
    FColumnIDs[currColType] := COL_ID_NONE;
    end;

  // Default the layout
  DefaultInternalLayout();

  FContents := nil;

  FFileExtnToIconMap_Large:= TStringList.Create();
  FIconsLarge:= TImageList.Create(nil);
  FIconsLarge.Width  := 32;
  FIconsLarge.Height := 32;
  FIconsLarge.BkColor := self.Color;

  FFileExtnToIconMap_Small:= TStringList.Create();
  FIconsSmall:= TImageList.Create(nil);
  FIconsSmall.Width  := 16;
  FIconsSmall.Height := 16;
  FIconsSmall.BkColor := self.Color;

  FSuppressPopupMenu := FALSE;
  FColumnHeaderPopup:= TPopupMenu.Create(self);
  InitPopup();

end;

procedure TSDCustomFilesystemListView.InitPopup();
var
  newPopupMnuItem: TMenuItem;
  currColType: TFilesystemListViewColumn;
begin
  for currColType:=low(currColType) to high(currColType) do
    begin
    newPopupMnuItem := TMenuItem.Create(FColumnHeaderPopup);
    FColumnHeaderPopup.Items.Add(newPopupMnuItem);
    newPopupMnuItem.Caption := FilesystemListViewColumnTitle(currColType);
    newPopupMnuItem.Tag := ord(currColType);
    newPopupMnuItem.OnClick := ColumnHeaderPopupClick;
    end;

  // Add separator...
  newPopupMnuItem := TMenuItem.Create(FColumnHeaderPopup);
  FColumnHeaderPopup.Items.Add(newPopupMnuItem);
  newPopupMnuItem.Caption := '-';

  // Add "More..." menuitem
  newPopupMnuItem := TMenuItem.Create(FColumnHeaderPopup);
  FColumnHeaderPopup.Items.Add(newPopupMnuItem);
  newPopupMnuItem.Caption := _('More...');
  newPopupMnuItem.Tag := COLHEADERPOPUPMENUITEM_MORE;
  newPopupMnuItem.OnClick := ColumnHeaderPopupClick;

end;

// This is a bit of a kludge - because TControl.WMContextMenu(...) is called
// after ColRightClick(...), it pops up any developer configured popupmenu!
// i.e. The user sees the column header selection popup menu, then the
// controls popup menu!
// To get around this, we set FSuppressPopupMenu if our column header context
// menu is displayed, then return nil here when TControl.WMContextMenu(...)
// calls *this* function to get the developer configured popup menu
function TSDCustomFilesystemListView.GetPopupMenu(): TPopupMenu;
var
  retval: TPopupMenu;
begin
  retval := inherited GetPopupMenu();

  if FSuppressPopupMenu then
    begin
    retval := nil;
    FSuppressPopupMenu:= FALSE;
    end;

  Result := retval;

end;

procedure TSDCustomFilesystemListView.ChooseColumns;
begin
{ TODO 1 -otdk -cfix : implement }
end;

procedure TSDCustomFilesystemListView.ColRightClick(Column: TListColumn; Point: TPoint);
var
  i: integer;
  currColType: TFilesystemListViewColumn;
  currMenuItem: TMenuItem;
begin
  if Assigned(OnColumnRightClick) then
    begin
    inherited;
    end
  else
    begin
    SyncInternalLayoutToDisplayed();

    // Put checkmarks against all visible columns on the popup menu, clear
    // checkmarks against non-visible columns
    for currColType:=low(currColType) to high(currColType) do
      begin
      for i:=0 to (FColumnHeaderPopup.Items.count - 1) do
        begin
        currMenuItem := FColumnHeaderPopup.Items[i];

        if (currColType = flvcFilename) then
          begin
          // "Filename" column must always be visible
          currMenuItem.Enabled := FALSE;
          end;

        if (TFilesystemListViewColumn(currMenuItem.Tag) = currColType) then
          begin
          currMenuItem.Checked := FInternalLayout[currColType].Visible;
          break;
          end;
        end;
      end;

    Point := self.ClientToScreen(Point);
    FColumnHeaderPopup.Popup(Point.X, Point.Y);

    FSuppressPopupMenu := TRUE;
    end;

end;

procedure TSDCustomFilesystemListView.ColumnHeaderPopupClick(Sender: TObject);
var
  clickedColType: TFilesystemListViewColumn;
  mnuItemClicked: TMenuItem;
  colsChanged: boolean;
  dlg: TSDFilesystemListView_ColDetails;
begin
  colsChanged := FALSE;
  SyncInternalLayoutToDisplayed();

  mnuItemClicked := TMenuItem(Sender);

  if (mnuItemClicked.Tag = COLHEADERPOPUPMENUITEM_MORE) then
    begin
    // "More..." menuitem clicked
    dlg:= TSDFilesystemListView_ColDetails.Create(nil);
    try
      dlg.Layout := FInternalLayout;
      dlg.ShowModal();
      if (dlg.ModalResult = mrOK) then
        begin
        FInternalLayout := dlg.Layout;
        colsChanged := TRUE;
        end;
    finally
      dlg.Free();
    end;
    end
  else
    begin
    clickedColType := TFilesystemListViewColumn(mnuItemClicked.Tag);
    FInternalLayout[clickedColType].Visible := not(mnuItemClicked.Checked);
    colsChanged := TRUE;
    end;

  // Repopulate the display - if a column was added, it's data won't be a
  // subitem
  if colsChanged then
    begin
    RepopulateDisplay();
    end;

end;

destructor TSDCustomFilesystemListView.Destroy();
begin
  FColumnHeaderPopup.Free();

  FFileExtnToIconMap_Large.Free();
  FIconsLarge.Free();
  FFileExtnToIconMap_Small.Free();
  FIconsSmall.Free();

  FCursorStack.Free();

  Freeandnil(FContents);
  inherited;
end;

procedure TSDCustomFilesystemListView.Initialize();
begin
  self.Items.Clear();
  self.RowSelect := TRUE;

  Self.LargeImages := FIconsLarge;
  Self.SmallImages := FIconsSmall;

  RecreateAllPosibleColHeaders();
  SyncDisplayedToInternalLayout();
end;

procedure TSDCustomFilesystemListView._ColumnSortCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  sortDirection: TColumnSortDirection;
  sortCol: TListColumn;
  dirItem1: TSDDirItem;
  dirItem2: TSDDirItem;
begin
  Compare := 0;

  sortCol := self.GetColumnByID(FColumnToSort);
  sortDirection := TColumnSortDirection(sortCol.Tag);

  dirItem1:= FContents[integer(Item1.Data)];
  dirItem2:= FContents[integer(Item2.Data)];

  // Parent dir comes first, regardless of anything else...
  if dirItem1.Filename = DIR_PARENT_DIR then
    begin
    Compare := -1;
    end
  else if dirItem2.Filename = DIR_PARENT_DIR then
    begin
    Compare := 1;
    end;

  // Directories come before files...
  if (Compare = 0) then
    begin
    if (dirItem1.IsFile and dirItem2.IsDirectory) then
      begin
      if DirectoriesAlwaysSortFirst then
        begin
        Compare := 1;
        end
      else
        begin
        if (sortDirection = csdAscending) then
          begin
          Compare := 1;
          end
        else
          begin
          Compare := -1;
          end;

        end;
      end
    else if (dirItem1.IsDirectory and dirItem2.IsFile) then
      begin
      if DirectoriesAlwaysSortFirst then
        begin
        Compare := -1;
        end
      else
        begin
        if (sortDirection = csdAscending) then
          begin
          Compare := -1;
          end
        else
          begin
          Compare := 1;
          end;

        end;
      end
    end;

  // If the two items haven't got a mandatory sort order, fallback to the
  // default sort order...
  if (Compare = 0) then
    begin
    inherited;
    end;
end;

procedure TSDCustomFilesystemListView.GetIconsForFile(
  filename: string;
  subduedIcon: boolean;
  out smallIconIdx: integer;
  out largeIconIdx: integer
);
var
  DLLFilename: string;
  DLLIconIdx: integer;
  ucExtn: string;
begin
  // "Special" icons
  if (filename = FILE_TYPE_DIRECTORY) then
    begin
    // Lowercase intentional; this way it won't get confused with the
    // uppercase (*real*) extensions added to the TStringList later
    ucExtn := lowercase(filename);
    end
  else
    begin
    ucExtn := uppercase(ExtractFileExt(filename));
    end;

  if subduedIcon then
    begin
    ucExtn := '***SUBDUED***'+ucExtn;
    end;

  smallIconIdx := FFileExtnToIconMap_Small.IndexOf(ucExtn);
  largeIconIdx := FFileExtnToIconMap_Large.IndexOf(ucExtn);

  // If neither icon already cached, get new icons
  if (
      (smallIconIdx < 0) and
      (largeIconIdx < 0)
     ) then
    begin
    // Get icon file and icon index within icon file
    SDUGetFileType_Icon(filename, DLLFilename, DLLIconIdx);

    if (DLLFilename = '') then
      begin
      DLLFilename := DLL_SHELL32;
      DLLIconIdx := DLL_SHELL32_DEFAULT_FILE;
      end;

    if (SDULoadDLLIconToList(DLLFilename, TRUE, DLLIconIdx, FIconsSmall) >= 0) then
      begin
      FFileExtnToIconMap_Small.Add(ucExtn);
      end
    else
      begin
      if (SDULoadDLLIconToList(DLL_SHELL32, TRUE, DLL_SHELL32_DEFAULT_FILE, FIconsSmall) >= 0) then
        begin
        FFileExtnToIconMap_Small.Add(ucExtn);
        end;
      end;

    if (SDULoadDLLIconToList(DLLFilename, FALSE, DLLIconIdx, FIconsLarge) >= 0) then
      begin
      FFileExtnToIconMap_Large.Add(ucExtn);
      end
    else
      begin
      if (SDULoadDLLIconToList(DLL_SHELL32, FALSE, DLL_SHELL32_DEFAULT_FILE, FIconsLarge) >= 0) then
        begin
        FFileExtnToIconMap_Large.Add(ucExtn);
        end;
      end;

    smallIconIdx := FFileExtnToIconMap_Small.IndexOf(ucExtn);
    largeIconIdx := FFileExtnToIconMap_Large.IndexOf(ucExtn);

    if subduedIcon then
      begin
      if (smallIconIdx >= 0) then
        begin
        MakeIconSubdued(FIconsSmall, smallIconIdx, FALSE);
        end;

      if (largeIconIdx >= 0) then
        begin
        MakeIconSubdued(FIconsLarge, largeIconIdx, TRUE);
        end;
      end;

    end;

end;

procedure TSDCustomFilesystemListView.MakeIconSubdued(imgList: TImageList; idx: integer; treatAs32x32Icon: boolean);
var
  srcIcon: TIcon;
  destIcon: TIcon;
begin
  srcIcon:= TIcon.Create();
  destIcon:= TIcon.Create();
  try
    imgList.GetIcon(idx, srcIcon);
    SDUMakeIconGhosted(srcIcon, destIcon, treatAs32x32Icon);
    imgList.ReplaceIcon(idx, destIcon);
  finally
    destIcon.Free();
    srcIcon.Free();
  end;

end;

function TSDCustomFilesystemListView.GetDirItemByListIdx(idx: integer): TSDDirItem;
begin
  Result := FContents[integer(self.Items[idx].Data)];
end;

function  TSDCustomFilesystemListView.GetDisplayName(item: TSDDirItem): string;
var
  retval: string;
  knownFiletype: boolean;
begin
  retval := '';

  if (item <> nil) then
    begin
    retval := item.Filename;

    if (
        HideKnownFileExtns and
        not(item.IsDirectory)
       ) then
      begin
      SDUGetFileType_Description(
                                 item.Filename,
                                 knownFiletype
                                );
      if knownFiletype then
        begin
        retval := ChangeFileExt(item.Filename, '');
        end;
      end;

    end;

  Result := retval;
end;

function  TSDCustomFilesystemListView.GetDisplayedNameByListIdx(idx: integer): string;
var
  item: TSDDirItem;
  retval: string;
begin
  item := DirItem[idx];
  if (item <> nil) then
    begin
    retval := GetDisplayName(item);
    end;

  Result := retval;
end;

function TSDCustomFilesystemListView.GetDirItemSelected(): TSDDirItem;
var
  retval: TSDDirItem;
  idx: integer;
begin
  retval := nil;

  idx := self.SelectedIdx;
  if (idx >= 0) then
    begin
    retval := GetDirItemByListIdx(idx);
    end;

  Result := retval;
end;

procedure TSDLoadDirThread.AfterConstruction();
begin
  inherited;
  Paths := TStringList.Create();
  LoadedContents := TSDDirItemList.Create();
end;

destructor TSDLoadDirThread.Destroy();
begin
  LoadedContents.Free();
  Paths.Free();
  inherited;
end;

procedure TSDLoadDirThread.Execute();
var
  i: integer;
begin
  for i:=0 to (Paths.count - 1) do
    begin
    LoadedContents.Clear();
    if Filesystem.LoadContentsFromDisk(Paths[i], LoadedContents) then
      begin
      LoadedPath := Paths[i];
      Synchronize(SyncMethod);
      end;

    end;
    
end;

procedure TSDLoadDirThread.SyncMethod();
begin
  Callback(self);
end;

procedure TSDLoadDirThread.AddPath(pathToLoad: string);
begin
  Paths.Add(pathToLoad);
end;


END.


