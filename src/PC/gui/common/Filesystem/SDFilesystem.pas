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
  PRIVATE
    FIsFile:      Boolean;
    FIsDirectory: Boolean;
    FIsReadonly:  Boolean;
    FIsHidden:    Boolean;
  PROTECTED
    function GetIsFile(): Boolean; VIRTUAL;
    procedure SetIsFile(Value: Boolean); VIRTUAL;
    function GetIsDirectory(): Boolean; VIRTUAL;
    procedure SetIsDirectory(Value: Boolean); VIRTUAL;
    function GetIsReadonly(): Boolean; VIRTUAL;
    procedure SetIsReadonly(Value: Boolean); VIRTUAL;
    function GetIsHidden(): Boolean; VIRTUAL;
    procedure SetIsHidden(Value: Boolean); VIRTUAL;
  PUBLIC
    // IMPORTANT: IF THESE ARE ADDED TO; ADD THEM TO Assign(...)
    // implementation as well!
    FFilename:              String;
    FSize:                  ULONGLONG;
    FTimestampLastModified: TTimeStamp;

    procedure Assign(srcItem: TSDDirItem); OVERLOAD; VIRTUAL;
  PUBLISHED
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
  PROTECTED
    function GetItem(Index: Integer): TSDDirItem;
    procedure SetItem(Index: Integer; AObject: TSDDirItem);
  PUBLIC
    property Items[Index: Integer]: TSDDirItem Read GetItem Write SetItem; DEFAULT;

    procedure AddAsCopy(src: TSDDirItem);
    procedure Assign(list: TSDDirItemList); OVERLOAD;
  end;


  TSDCustomFilesystem = class
  PROTECTED
    FMounted:  Boolean;
    FReadOnly: Boolean;

    FSerializeCS: TCriticalSection; // Protect by serializing read/write
                                    // operations in multithreaded operations

    procedure AssertMounted();

    procedure SetMounted(newMounted: Boolean);

    function DoMount(): Boolean; VIRTUAL; ABSTRACT;
    procedure DoDismount(); VIRTUAL; ABSTRACT;

    // Returns TRUE/FALSE, depending on whether we treat filenames as case
    // sensitive or not
    // This is filesytem dependant
    function GetCaseSensitive(): Boolean; VIRTUAL; ABSTRACT;

    function GetFreeSpace(): ULONGLONG; VIRTUAL; ABSTRACT;
    function GetSize(): ULONGLONG; VIRTUAL; ABSTRACT;

    procedure LastErrorClear(); VIRTUAL;
    procedure LastErrorSet(errMsg: String); VIRTUAL;

    function CheckWritable(): Boolean;

  PUBLIC
    LastError: String;

    constructor Create(); VIRTUAL;
    destructor Destroy(); OVERRIDE;

    function FilesystemTitle(): String; VIRTUAL; ABSTRACT;

    function Format(): Boolean; VIRTUAL; ABSTRACT;

    function CheckFilesystem(): Boolean; VIRTUAL;

    function LoadContentsFromDisk(path: String; items: TSDDirItemList): Boolean;
      VIRTUAL; ABSTRACT;
    function ExtractFile(srcPath: WideString; extractToFilename: String): Boolean;
      VIRTUAL; ABSTRACT;

    function GetItem(path: WideString; item: TSDDirItem): Boolean; VIRTUAL; ABSTRACT;
    function ItemSize(Path: WideString; out TotalSize: ULONGLONG;
      out DirectoryCount: Integer; out FilesCount: Integer): Boolean;

    function FileExists(fullPathToItem: WideString): Boolean;
    function DirectoryExists(fullPathToItem: WideString): Boolean;
    function DirectoryOrFileExists(fullPathToItem: WideString): Boolean;

  PUBLISHED
    property Mounted: Boolean Read FMounted Write SetMounted DEFAULT False;
    property CaseSensitive: Boolean Read GetCaseSensitive;
    property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
    property FreeSpace: ULONGLONG Read GetFreeSpace;
    property Size: ULONGLONG Read GetSize;
  end;

  TSDCustomFilesystemPartitionBased = class (TSDCustomFilesystem)
  PROTECTED
    FPartitionImage: TSDPartitionImage;

    function DoMount(): Boolean; OVERRIDE;
    procedure DoDismount(); OVERRIDE;

  PUBLIC
    constructor Create(); OVERRIDE;
    destructor Destroy(); OVERRIDE;

  PUBLISHED
    property PartitionImage: TSDPartitionImage
      Read FPartitionImage Write FPartitionImage DEFAULT nil;
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
  allOK:          Boolean;
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
    allOK := GetItem(Path, item);
    if allOK then begin
      if item.IsFile then begin
        TotalSize := TotalSize + item.Size;
        Inc(FilesCount);
      end else
      if item.IsDirectory then begin
        Inc(DirectoryCount);

        dirItems := TSDDirItemList.Create();
        try
          allOK := LoadContentsFromDisk(Path, dirItems);
          if allOK then begin
            for i := 0 to (dirItems.Count - 1) do begin
              if (dirItems[i].IsDirectory or dirItems[i].IsFile) then begin
                allOK := ItemSize(IncludeTrailingPathDelimiter(Path) +
                  dirItems[i].Filename, subdirsSize, subdirsDirsCnt,
                  subditsFileCnt);
                if allOK then begin
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

  Result := allOK;
end;


function TSDCustomFilesystem.FileExists(fullPathToItem: WideString): Boolean;
var
  retval: Boolean;
  item:   TSDDirItem;
begin
  retval := False;

  item := TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then begin
      retval := item.IsFile;
    end;

  finally
    item.Free();
  end;

  Result := retval;
end;

function TSDCustomFilesystem.DirectoryExists(fullPathToItem: WideString): Boolean;
var
  retval: Boolean;
  item:   TSDDirItem;
begin
  retval := False;

  item := TSDDirItem.Create();
  try
    if GetItem(fullPathToItem, item) then begin
      retval := item.IsDirectory;
    end;

  finally
    item.Free();
  end;

  Result := retval;
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
var
  retval: Boolean;
begin
  retval := True;
  if self.ReadOnly then begin
    LastErrorSet(_('Filesystem mounted readonly'));
    retval := False;
  end;

  Result := retval;
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
