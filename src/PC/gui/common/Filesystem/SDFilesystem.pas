unit SDFilesystem;

interface

uses
  Classes, Contnrs, SDPartitionImage,
  SDUGeneral, SyncObjs,
  SysUtils, Windows;

const
  PATH_SEPARATOR: Widechar = '\';

  DIR_CURRENT_DIR: WideString = '.';
  DIR_PARENT_DIR: WideString  = '..';

type
  EFileSystemError = class (Exception);
  EFileSystemNotMounted = class (EFileSystemError);
  EFileSystemNoPartition = class (EFileSystemError);
  EFileSystemNotRecognised = class (EFileSystemError);

{$M+}// Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDDirItem = class
  private
    FIsFile:      Boolean;
    FIsDirectory: Boolean;
    FIsReadonly:  Boolean;
    FIsHidden:    Boolean;
  protected
    function GetIsFile(): Boolean; virtual;
    procedure SetIsFile(Value: Boolean); virtual;
    function GetIsDirectory(): Boolean; virtual;
    procedure SetIsDirectory(Value: Boolean); virtual;
    function GetIsReadonly(): Boolean; virtual;
    procedure SetIsReadonly(Value: Boolean); virtual;
    function GetIsHidden(): Boolean; virtual;
    procedure SetIsHidden(Value: Boolean); virtual;
  public
    // IMPORTANT: IF THESE ARE ADDED TO; ADD THEM TO Assign(...)
    // implementation as well!
    FFilename: String;
    FSize:     ULONGLONG;
    FTimestampLastModified: TTimeStamp;

    procedure Assign(srcItem: TSDDirItem); overload; virtual;
  published
    // IMPORTANT: IF THESE ARE ADDED TO; ADD THEM TO Assign(...)
    // implementation as well!
    property Filename: String Read FFilename Write FFilename;
    property Size: ULONGLONG Read FSize Write FSize;
    property TimestampLastModified: TTimeStamp Read FTimestampLastModified
      Write FTimestampLastModified;
    // Flags
    property IsFile: Boolean Read GetIsFile Write SetIsFile;
    property IsDirectory: Boolean Read GetIsDirectory Write SetIsDirectory;
    property IsReadonly: Boolean Read GetIsReadonly Write SetIsReadonly;
    property IsHidden: Boolean Read GetIsHidden Write SetIsHidden;

  end;

  TSDDirItemList = class (TObjectList)
  protected
    function GetItem(Index: Integer): TSDDirItem;
    procedure SetItem(Index: Integer; AObject: TSDDirItem);
  public
    property Items[Index: Integer]: TSDDirItem Read GetItem Write SetItem; default;

    procedure AddAsCopy(src: TSDDirItem);
    procedure Assign(list: TSDDirItemList); overload;
  end;


  TSDCustomFilesystem = class
  protected
    FMounted:  Boolean;
    FReadOnly: Boolean;

    FSerializeCS: TCriticalSection; // Protect by serializing read/write
    // operations in multithreaded operations

    procedure AssertMounted();

    procedure SetMounted(newMounted: Boolean);

    function DoMount(): Boolean; virtual; abstract;
    procedure DoDismount(); virtual; abstract;

    // Returns TRUE/FALSE, depending on whether we treat filenames as case
    // sensitive or not
    // This is filesytem dependant
    function GetCaseSensitive(): Boolean; virtual; abstract;

    function GetFreeSpace(): ULONGLONG; virtual; abstract;
    function GetSize(): ULONGLONG; virtual; abstract;

    procedure LastErrorClear(); virtual;
    procedure LastErrorSet(errMsg: String); virtual;

    function CheckWritable(): Boolean;

  public
    LastError: String;

    constructor Create(); virtual;
    destructor Destroy(); override;

    function FilesystemTitle(): String; virtual; abstract;

    function Format(): Boolean; virtual; abstract;

    function CheckFilesystem(): Boolean; virtual;

    function LoadContentsFromDisk(path: String; items: TSDDirItemList): Boolean;
      virtual; abstract;
    function ExtractFile(srcPath: WideString; extractToFilename: String): Boolean;
      virtual; abstract;

    function GetItem(path: WideString; item: TSDDirItem): Boolean; virtual; abstract;
    function ItemSize(Path: WideString; out TotalSize: ULONGLONG;
      out DirectoryCount: Integer; out FilesCount: Integer): Boolean;

    function FileExists(fullPathToItem: WideString): Boolean;
    function DirectoryExists(fullPathToItem: WideString): Boolean;
    function DirectoryOrFileExists(fullPathToItem: WideString): Boolean;

  published
    property Mounted: Boolean Read FMounted Write SetMounted default False;
    property CaseSensitive: Boolean Read GetCaseSensitive;
    property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
    property FreeSpace: ULONGLONG Read GetFreeSpace;
    property Size: ULONGLONG Read GetSize;
  end;

  TSDCustomFilesystemPartitionBased = class (TSDCustomFilesystem)
  protected
    FPartitionImage: TSDPartitionImage;

    function DoMount(): Boolean; override;
    procedure DoDismount(); override;

  public
    constructor Create(); override;
    destructor Destroy(); override;

  published
    property PartitionImage: TSDPartitionImage
      Read FPartitionImage Write FPartitionImage default nil;
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

function TSDDirItem.GetIsFile(): Boolean;
begin
  Result := FIsFile;
end;

procedure TSDDirItem.SetIsFile(Value: Boolean);
begin
  FIsFile := Value;
end;

function TSDDirItem.GetIsDirectory(): Boolean;
begin
  Result := FIsDirectory;
end;

procedure TSDDirItem.SetIsDirectory(Value: Boolean);
begin
  FIsDirectory := Value;
end;

function TSDDirItem.GetIsReadonly(): Boolean;
begin
  Result := FIsReadonly;
end;

procedure TSDDirItem.SetIsReadonly(Value: Boolean);
begin
  FIsReadonly := Value;
end;

function TSDDirItem.GetIsHidden(): Boolean;
begin
  Result := FIsHidden;
end;

procedure TSDDirItem.SetIsHidden(Value: Boolean);
begin
  FIsHidden := Value;
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
  i: Integer;
begin
  self.Clear();
  for i := 0 to (list.Count - 1) do begin
    AddAsCopy(list.Items[i]);
  end;

end;

procedure TSDDirItem.Assign(srcItem: TSDDirItem);
begin
  self.Filename              := srcItem.Filename;
  self.Size                  := srcItem.Size;
  self.IsFile                := srcItem.IsFile;
  self.IsDirectory           := srcItem.IsDirectory;
  self.IsReadonly            := srcItem.IsReadonly;
  self.IsHidden              := srcItem.IsHidden;
  self.TimestampLastModified := srcItem.TimestampLastModified;
end;

// ----------------------------------------------------------------------------

constructor TSDCustomFilesystem.Create();
begin
  inherited;
  LastError    := '';
  FMounted     := False;
  FSerializeCS := TCriticalSection.Create();
end;

destructor TSDCustomFilesystem.Destroy();
begin
  Mounted := False;
  FSerializeCS.Free();
  inherited;
end;

function TSDCustomFilesystem.CheckFilesystem(): Boolean;
begin
  Result := True;
end;


function TSDCustomFilesystem.ItemSize(Path: WideString; out TotalSize: ULONGLONG;
  out DirectoryCount: Integer; out FilesCount: Integer): Boolean;
var
  dirItems:       TSDDirItemList;
  item:           TSDDirItem;
  subdirsSize:    ULONGLONG;
  subdirsDirsCnt: Integer;
  subditsFileCnt: Integer;
  i:              Integer;
begin
  TotalSize      := 0;
  DirectoryCount := 0;
  FilesCount     := 0;

  // Short circuit...
  if ((ExtractFilename(Path) = DIR_CURRENT_DIR) or (ExtractFilename(Path) = DIR_PARENT_DIR))
  then begin
    Result := True;
    exit;
  end;

  item := TSDDirItem.Create();
  try
    Result := GetItem(Path, item);
    if Result then begin
      if item.IsFile then begin
        TotalSize := TotalSize + item.Size;
        Inc(FilesCount);
      end else
      if item.IsDirectory then begin
        Inc(DirectoryCount);

        dirItems := TSDDirItemList.Create();
        try
          Result := LoadContentsFromDisk(Path, dirItems);
          if Result then begin
            for i := 0 to (dirItems.Count - 1) do begin
              if (dirItems[i].IsDirectory or dirItems[i].IsFile) then begin
                Result := ItemSize(IncludeTrailingPathDelimiter(Path) +
                  dirItems[i].Filename, subdirsSize, subdirsDirsCnt,
                  subditsFileCnt);
                if Result then begin
                  TotalSize      := TotalSize + subdirsSize;
                  DirectoryCount := DirectoryCount + subdirsDirsCnt;
                  FilesCount     := FilesCount + subditsFileCnt;
                end else begin
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

end;


function TSDCustomFilesystem.FileExists(fullPathToItem: WideString): Boolean;
var
  item: TSDDirItem;
begin
  Result := False;

  item := TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then begin
      Result := item.IsFile;
    end;

  finally
    item.Free();
  end;

end;

function TSDCustomFilesystem.DirectoryExists(fullPathToItem: WideString): Boolean;
var
  item: TSDDirItem;
begin
  Result := False;

  item := TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then begin
      Result := item.IsDirectory;
    end;

  finally
    item.Free();
  end;

end;

function TSDCustomFilesystem.DirectoryOrFileExists(fullPathToItem: WideString): Boolean;
begin
  Result := (FileExists(fullPathToItem) or DirectoryExists(fullPathToItem));
end;

procedure TSDCustomFilesystem.LastErrorClear();
begin
  LastErrorSet('');
end;

procedure TSDCustomFilesystem.LastErrorSet(errMsg: String);
begin
  LastError := errMsg;
end;

function TSDCustomFilesystem.CheckWritable(): Boolean;
begin
  Result := True;
  if self.ReadOnly then begin
    LastErrorSet(_('Filesystem mounted readonly'));
    Result := False;
  end;

end;

procedure TSDCustomFilesystem.SetMounted(newMounted: Boolean);
var
  oldMounted: Boolean;
begin

  inherited;
  if (newMounted <> Mounted) then begin
    oldMounted := Mounted;

    try
      if newMounted then begin
        FMounted := DoMount();
      end else begin
        DoDismount();
        FMounted := False;
      end;

    except
      on E: Exception do begin
        FMounted := oldMounted;
        raise;
      end;
    end;

  end;
end;

procedure TSDCustomFilesystem.AssertMounted();
begin
  inherited;

  if not (Mounted) then begin
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

function TSDCustomFilesystemPartitionBased.DoMount(): Boolean;
begin
  if (PartitionImage = nil) then begin
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

end.
