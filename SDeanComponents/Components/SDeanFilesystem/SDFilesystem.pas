unit SDFilesystem;

interface

uses
  Classes, Windows, SysUtils, Contnrs, SyncObjs,
  SDPartitionImage,
  SDUGeneral;

const
  PATH_SEPARATOR: WideChar = '\';

  DIR_CURRENT_DIR: WideString = '.';
  DIR_PARENT_DIR : WideString = '..';

type
  EFileSystemError = class(Exception);
  EFileSystemNotMounted = class(EFileSystemError);
  EFileSystemNoPartition = class(EFileSystemError);
  EFileSystemNotRecognised = class(EFileSystemError);

{$M+}  // Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDDirItem = class
  private
    FIsFile: boolean;
    FIsDirectory: boolean;
    FIsReadonly: boolean;
    FIsHidden: boolean;
  protected
    function  GetIsFile(): boolean; virtual;
    procedure SetIsFile(value: boolean); virtual;
    function  GetIsDirectory(): boolean; virtual;
    procedure SetIsDirectory(value: boolean); virtual;
    function  GetIsReadonly(): boolean; virtual;
    procedure SetIsReadonly(value: boolean); virtual;
    function  GetIsHidden(): boolean; virtual;
    procedure SetIsHidden(value: boolean); virtual;
  public
    // IMPORTANT: IF THESE ARE ADDED TO; ADD THEM TO Assign(...)
    // implementation as well!
    FFilename: string;
    FSize: ULONGLONG;
    FTimestampLastModified: TTimeStamp;

    procedure Assign(srcItem: TSDDirItem); overload; virtual;
  published
    // IMPORTANT: IF THESE ARE ADDED TO; ADD THEM TO Assign(...)
    // implementation as well!
    property Filename: string read FFilename write FFilename;
    property Size: ULONGLONG read FSize write FSize;
    property TimestampLastModified: TTimeStamp read FTimestampLastModified write FTimestampLastModified;
    // Flags
    property IsFile: boolean read GetIsFile write SetIsFile;
    property IsDirectory: boolean read GetIsDirectory write SetIsDirectory;
    property IsReadonly: boolean read GetIsReadonly write SetIsReadonly;
    property IsHidden: boolean read GetIsHidden write SetIsHidden;

  end;

  TSDDirItemList = class(TObjectList)
  protected
    function  GetItem(Index: Integer): TSDDirItem;
    procedure SetItem(Index: Integer; AObject: TSDDirItem);
  public
    property  Items[Index: Integer]: TSDDirItem read GetItem write SetItem; default;

    procedure AddAsCopy(src: TSDDirItem);
    procedure Assign(list: TSDDirItemList); overload;
  end;


  TSDCustomFilesystem = class
  protected
    FMounted: boolean;
    FReadOnly: boolean;

    FSerializeCS: TCriticalSection; // Protect by serializing read/write
                                    // operations in multithreaded operations

    procedure AssertMounted();

    procedure SetMounted(newMounted: boolean);

    function  DoMount(): boolean; virtual; abstract;
    procedure DoDismount(); virtual; abstract;

    // Returns TRUE/FALSE, depending on whether we treat filenames as case
    // sensitive or not
    // This is filesytem dependant
    function  GetCaseSensitive(): boolean; virtual; abstract;

    function  GetFreeSpace(): ULONGLONG; virtual; abstract;
    function  GetSize(): ULONGLONG; virtual; abstract;

    procedure LastErrorClear(); virtual;
    procedure LastErrorSet(errMsg: string); virtual;

    function  CheckWritable(): boolean;

  public
    LastError: string;

    constructor Create(); virtual;
    destructor  Destroy(); override;

    function  FilesystemTitle(): string; virtual; abstract;

    function  Format(): boolean; virtual; abstract;

    function  CheckFilesystem(): boolean; virtual;

    function  LoadContentsFromDisk(path: string; items: TSDDirItemList): boolean; virtual; abstract;
    function  ExtractFile(srcPath: WideString; extractToFilename: string): boolean; virtual; abstract;

    function  GetItem(path: WideString; item: TSDDirItem): boolean; virtual; abstract;
    function  ItemSize(Path: WideString; out TotalSize: ULONGLONG; out DirectoryCount: integer; out FilesCount: integer): boolean;

    function  FileExists(fullPathToItem: WideString): boolean;
    function  DirectoryExists(fullPathToItem: WideString): boolean;
    function  DirectoryOrFileExists(fullPathToItem: WideString): boolean;

  published
    property Mounted: boolean read FMounted write SetMounted default FALSE;
    property CaseSensitive: boolean read GetCaseSensitive;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property FreeSpace: ULONGLONG read GetFreeSpace;
    property Size: ULONGLONG read GetSize;
  end;

  TSDCustomFilesystemPartitionBased = class(TSDCustomFilesystem)
  protected
    FPartitionImage: TSDPartitionImage;

    function  DoMount(): boolean; override;
    procedure DoDismount(); override;

  public
    constructor Create(); override;
    destructor Destroy(); override;

  published
    property PartitionImage: TSDPartitionImage read FPartitionImage write FPartitionImage default nil;
  end;

implementation

uses
  SDUClasses,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}
  
function TSDDirItem.GetIsFile(): boolean;
begin
  Result := FIsFile;
end;

procedure TSDDirItem.SetIsFile(value: boolean);
begin
  FIsFile := value;
end;

function TSDDirItem.GetIsDirectory(): boolean;
begin
  Result := FIsDirectory;
end;

procedure TSDDirItem.SetIsDirectory(value: boolean);
begin
  FIsDirectory := value;
end;

function TSDDirItem.GetIsReadonly(): boolean;
begin
  Result := FIsReadonly;
end;

procedure TSDDirItem.SetIsReadonly(value: boolean);
begin
  FIsReadonly := value;
end;

function TSDDirItem.GetIsHidden(): boolean;
begin
  Result := FIsHidden;
end;

procedure TSDDirItem.SetIsHidden(value: boolean);
begin
  FIsHidden := value;
end;

function TSDDirItemList.GetItem(Index: Integer): TSDDirItem;
begin
  Result := TSDDirItem(inherited Items[Index]);
end;

procedure TSDDirItemList.SetItem(Index: Integer; AObject: TSDDirItem);
begin
  inherited Items[Index] := AObject;
end;

procedure TSDDirItemList.AddAsCopy(src: TSDDirItem);
var
  tmpItem: TSDDirItem;
begin
  tmpItem := TSDDirItem.Create();
  tmpItem.Assign(src);
  self.Add(tmpItem);

end;

procedure TSDDirItemList.Assign(list: TSDDirItemList);
var
  i: integer;
begin
  self.Clear();
  for i:=0 to (list.count - 1) do
    begin
    AddAsCopy(list.Items[i]);
    end;

end;

procedure TSDDirItem.Assign(srcItem: TSDDirItem);
begin
  self.Filename    := srcItem.Filename;
  self.Size        := srcItem.Size;
  self.IsFile      := srcItem.IsFile;
  self.IsDirectory := srcItem.IsDirectory;
  self.IsReadonly  := srcItem.IsReadonly;
  self.IsHidden    := srcItem.IsHidden;
  self.TimestampLastModified := srcItem.TimestampLastModified;
end;

// ----------------------------------------------------------------------------

constructor TSDCustomFilesystem.Create();
begin
  inherited;
  LastError := '';
  FMounted := FALSE;
  FSerializeCS := TCriticalSection.Create();
end;

destructor TSDCustomFilesystem.Destroy();
begin
  Mounted := FALSE;
  FSerializeCS.Free();
  inherited;
end;

function TSDCustomFilesystem.CheckFilesystem(): boolean;
begin
  Result := TRUE;
end;


function TSDCustomFilesystem.ItemSize(Path: WideString; out TotalSize: ULONGLONG; out DirectoryCount: integer; out FilesCount: integer): boolean;
var
  dirItems: TSDDirItemList;
  allOK: boolean;
  item: TSDDirItem;
  subdirsSize: ULONGLONG;
  subdirsDirsCnt: integer;
  subditsFileCnt: integer;
  i: integer;
begin
  TotalSize := 0;
  DirectoryCount := 0;
  FilesCount := 0;

  // Short circuit...
  if (
      (ExtractFilename(Path) = DIR_CURRENT_DIR) or
      (ExtractFilename(Path) = DIR_PARENT_DIR)
     ) then
    begin
    Result:= TRUE;
    exit;
    end;

  item:= TSDDirItem.Create();
  try
    allOK := GetItem(Path, item);
    if allOK then
      begin
      if item.IsFile then
        begin
        TotalSize := TotalSize + item.Size;
        inc(FilesCount);
        end
      else if item.IsDirectory then
        begin
        inc(DirectoryCount);

        dirItems:= TSDDirItemList.Create();
        try
          allOK := LoadContentsFromDisk(Path, dirItems);
          if allOK then
            begin
            for i:=0 to (dirItems.Count - 1) do
              begin
              if (
                  dirItems[i].IsDirectory or
                  dirItems[i].IsFile
                 ) then
                 begin
                allOK := ItemSize(
                                  IncludeTrailingPathDelimiter(Path)+dirItems[i].Filename,
                                  subdirsSize,
                                  subdirsDirsCnt,
                                  subditsFileCnt
                                 );
                if allOK then
                  begin
                  TotalSize := TotalSize + subdirsSize;
                  DirectoryCount := DirectoryCount + subdirsDirsCnt;
                  FilesCount := FilesCount + subditsFileCnt;
                  end
                else
                  begin
                  // Bail out...
                  break;
                  end;
                end;
              end;
            end;

        finally
          dirItems.Free();
        end;

        end;
      end;

  finally
    item.Free();
  end;

  Result := allOK;
end;


function TSDCustomFilesystem.FileExists(fullPathToItem: WideString): boolean;
var
  retval: boolean;
  item: TSDDirItem;
begin
  retval := FALSE;

  item:= TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then
      begin
      retval := item.IsFile;
      end;

  finally
    item.Free();
  end;

  Result := retval;
end;

function TSDCustomFilesystem.DirectoryExists(fullPathToItem: WideString): boolean;
var
  retval: boolean;
  item: TSDDirItem;
begin
  retval := FALSE;

  item:= TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then
      begin
      retval := item.IsDirectory;
      end;

  finally
    item.Free();
  end;

  Result := retval;
end;

function TSDCustomFilesystem.DirectoryOrFileExists(fullPathToItem: WideString): boolean;
begin
  Result := (
             FileExists(fullPathToItem) or
             DirectoryExists(fullPathToItem)
            );
end;

procedure TSDCustomFilesystem.LastErrorClear();
begin
  LastErrorSet('');
end;

procedure TSDCustomFilesystem.LastErrorSet(errMsg: string);
begin
  LastError := errMsg;
end;

function TSDCustomFilesystem.CheckWritable(): boolean;
var
  retval: boolean;
begin
  retval := TRUE;
  if self.ReadOnly then
    begin
    LastErrorSet(_('Filesystem mounted readonly'));
    retval := FALSE;
    end;

  Result := retval;
end;

procedure TSDCustomFilesystem.SetMounted(newMounted: boolean);
var
  oldMounted: boolean;
begin

  inherited;
  if (newMounted <> Mounted) then
    begin
    oldMounted := Mounted;

    try
      if newMounted then
        begin
        FMounted := DoMount();
        end
      else
        begin
        DoDismount();
        FMounted := FALSE;
        end;

    except
      on E:Exception do
        begin
        FMounted := oldMounted;
        raise;
        end;
    end;

    end;
end;

procedure TSDCustomFilesystem.AssertMounted();
begin
  inherited;

  if not(Mounted) then
    begin
    raise EFileSystemNotMounted.Create('Filesystem not mounted');
    end;
end;

constructor TSDCustomFilesystemPartitionBased.Create();
begin
  inherited;
  FPartitionImage := nil;
end;

// ----------------------------------------------------------------------------

destructor TSDCustomFilesystemPartitionBased.Destroy();
begin
  inherited;
end;

function TSDCustomFilesystemPartitionBased.DoMount(): boolean;
begin
  if (PartitionImage = nil) then
    begin
    raise EFileSystemNoPartition.Create('Partition not specified');
    end;

  Result := inherited DoMount();
end;

procedure TSDCustomFilesystemPartitionBased.DoDismount();
begin
  inherited;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

END.


