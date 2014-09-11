unit SDFilesystem_Local;

// To use:
//  Filesystem := TSDFilesystem_Local.Create();
//  TSDFilesystem_Local(Filesystem).DriveLetter := 'C';
//  Filesystem.Mounted := TRUE;


interface

uses
  Windows,
  SDUGeneral,
  SDUSysUtils,
  SDFilesystem;

type
  TSDFilesystem_Local = class(TSDCustomFilesystem)
  private
    FDriveLetter: char;

  protected
    function  DoMount(): boolean; override;
    procedure DoDismount(); override;

    function  GetCaseSensitive(): boolean; override;

    function  GetFreeSpace(): ULONGLONG; override;
    function  GetSize(): ULONGLONG; override;

  public
    constructor Create(); override;
    destructor  Destroy(); override;

    function  FilesystemTitle(): string; override;

    function  Format(): boolean; override;

    function  LoadContentsFromDisk(path: string; items: TSDDirItemList): boolean; override;
    function  ExtractFile(srcPath: WideString; extractToFilename: string): boolean; override;

    function  GetItem(path: WideString; item: TSDDirItem): boolean; override;
    
  published
    property  DriveLetter: char read FDriveLetter write FDriveLetter default #0;

  end;

implementation

uses
  SysUtils,
  SDUFileIterator_U;

constructor TSDFilesystem_Local.Create();
begin
  inherited;
end;

destructor  TSDFilesystem_Local.Destroy();
begin
  inherited;
end;

function TSDFilesystem_Local.GetCaseSensitive(): boolean; 
begin
  Result := FALSE;
end;

function TSDFilesystem_Local.FilesystemTitle(): string;
begin
  Result := 'Local filesystem';
end;

function TSDFilesystem_Local.DoMount(): boolean;
begin
  Assert(
         (DriveLetter <> #0),
         'Drive letter of drive to mount not specified'
        );

  Result := TRUE;
end;

procedure TSDFilesystem_Local.DoDismount();
begin
  inherited;
end;


function TSDFilesystem_Local.LoadContentsFromDisk(path: string; items: TSDDirItemList): boolean;

  procedure AddItem(filename: string);
  var
    currItem: TSDDirItem;
    fullFilename: WideString;
  begin
    currItem := TSDDirItem.Create();

    fullFilename := IncludeTrailingPathDelimiter(DriveLetter+':'+path)+filename;
    GetItem(fullFilename, currItem);

    items.Add(currItem);
  end;

var
  dc: TSDUFileIterator;
  currName: string;
begin
  AssertMounted();

  dc := TSDUFileIterator.Create(nil);
  try
    dc.Directory := IncludeTrailingPathDelimiter(DriveLetter+':'+path);
    dc.OmitStartDirPrefix := TRUE;
    dc.IncludeDirNames := TRUE;

    items.clear();
    dc.Reset();
    currName := dc.Next();
    while (currName <> '') do
      begin
      AddItem(currName);
      currName := dc.Next();
      end;

    // Add in current and parent dirs
    AddItem(DIR_CURRENT_DIR);
    AddItem(DIR_PARENT_DIR);

  finally
    dc.Free();
  end;

  Result := TRUE;
end;

function TSDFilesystem_Local.ExtractFile(srcPath: WideString; extractToFilename: string): boolean;
var
  tmpFullSrc: string;
begin
//  Result := SDUCopyFile(DriveLetter+':'+srcPath, extractToFilename);
  tmpFullSrc := DriveLetter+':'+srcPath;
  Result := CopyFile(PChar(tmpFullSrc), PChar(extractToFilename), FALSE);
end;

function TSDFilesystem_Local.GetFreeSpace(): ULONGLONG;
begin
  Result := DiskFree(Ord(DriveLetter) - 64);
end;

function TSDFilesystem_Local.GetSize(): ULONGLONG;
begin
  Result := DiskSize(Ord(DriveLetter) - 64);
end;

function TSDFilesystem_Local.Format(): boolean;
begin
  Result := FALSE;
end;

function TSDFilesystem_Local.GetItem(path: WideString; item: TSDDirItem): boolean;
var
  retval: boolean;
  tmpDateTime: TDateTime;
  tmpTimeStamp: TTimeStamp;
//  attr: integer;
  attr: DWORD;
  pathAsString: string;
begin
  retval := FALSE;

  if (
      SysUtils.FileExists(path) or
      SysUtils.DirectoryExists(path)
     ) then
    begin
    retval := TRUE;

    item.Filename := ExtractFilename(path);

    item.IsFile := SysUtils.FileExists(path);
    item.IsDirectory := SysUtils.DirectoryExists(path);

    item.Size := 0;
    if item.IsFile then
      begin
      item.Size := SDUGetFileSize(path);

      if SDUFileAge(path, tmpDateTime) then
        begin
        item.TimestampLastModified := DateTimeToTimeStamp(tmpDateTime);
        end
      else
        begin
        tmpTimeStamp.Time := 0;
        tmpTimeStamp.Date := 0;
        item.TimestampLastModified := tmpTimeStamp;
        end;

// This directive is ineffective; so we use the below, which isn't as obvious
// what it's doing, but reduces the number of compiler warnings
// {$WARN UNIT_PLATFORM OFF}
//      attr := FileGetAttr(path);
//      item.IsReadOnly := ((attr and faReadonly) = faReadonly);
//      item.IsHidden := ((attr and faHidden) = faHidden);
//{$WARN UNIT_PLATFORM ON}
      pathAsString := path;
      attr := GetFileAttributes(PChar(pathAsString));
//      item.IsReadOnly := FileIsReadOnly(path);
      item.IsReadOnly := ((attr and FILE_ATTRIBUTE_READONLY) <> 0);
      item.IsHidden   := ((attr and FILE_ATTRIBUTE_HIDDEN) <> 0);
      end;

    end;

  Result := retval;
end;

END.

