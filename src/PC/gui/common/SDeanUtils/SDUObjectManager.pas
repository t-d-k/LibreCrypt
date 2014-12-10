unit SDUObjectManager;

interface

uses
  Classes,
  MSntdll;

type
  TObjectType = (otDirectory, otDevice, otSymlink, otOther);

  TObjMgrEntry = record
    Name:     WideString;
    FullName: WideString;
    ObjType:  TObjectType;

    SymLinkTo: WideString;
  end;
  PObjMgrEntry = ^TObjMgrEntry;


  TSDUObjManager = class
  PRIVATE
    // Stringlist containing full path and name of objects found
    // The objects in this TStringList are PObjMgrEntry
    FAllObjects: TStringList;

    procedure ProcessSymLink(objEntry: PObjMgrEntry);
    procedure ProcessDirectory(objEntry: PObjMgrEntry);
    procedure ProcessDevice(objEntry: PObjMgrEntry);

    function FullPathAndName(StartDir: WideString;
      objDirInfo: POBJECT_DIRECTORY_INFORMATION
      ): WideString;

    procedure Print(line: String); OVERLOAD;
    procedure Print(line: WideString); OVERLOAD;

    procedure ClearScannedObjects();

    procedure Scan();
    procedure WalkDir(StartDir: WideString);

  PUBLIC
    PrintOut: TStringList;

    constructor Create();
    destructor Destroy(); OVERRIDE;

    // Rescan object manager objects...
    procedure Rescan();

    // Return the underlying device for the specified name
    // e.g.:
    //   \Device\Harddisk0\Partition1 => \Device\HarddiskVolume1
    function UnderlyingDeviceForName(startName: String): String;
    // Return the underlying device for the specified drive
    // e.g.:
    //   C: => \Device\HarddiskVolume1
    function UnderlyingDeviceForDrive(driveLetter: Char): String;
  end;

implementation

uses
  SysUtils, Windows;

const
  //  GLOBAL_DRIVE_NAME = '\\.\%s:';
  GLOBAL_DRIVE_NAME = '\GLOBAL??\%s:';

constructor TSDUObjManager.Create();
begin
  inherited;
  PrintOut := TStringList.Create();

  FAllObjects        := TStringList.Create();
  FAllObjects.Sorted := True;

  Scan();
end;

destructor TSDUObjManager.Destroy();
begin
  ClearScannedObjects();

  FAllObjects.Free();
  PrintOut.Free();
  inherited;
end;

procedure TSDUObjManager.ClearScannedObjects();
var
  i:        Integer;
  objEntry: PObjMgrEntry;
begin
  Print('-------------------------------');
  Print('Clearing all scanned objects...');
  for i := 0 to (FAllObjects.Count - 1) do begin
    objEntry := PObjMgrEntry(FAllObjects.Objects[i]);
    if (objEntry <> nil) then begin
      Dispose(objEntry);
    end;
  end;

  FAllObjects.Clear();
end;

procedure TSDUObjManager.Scan();
begin
  ClearScannedObjects();
  Print('-------------------------------');
  Print('Scanning all objects...');
  WalkDir('\');
end;

procedure TSDUObjManager.ProcessSymLink(objEntry: PObjMgrEntry);
const
  BUFFER_LINK_TARGET_NAME = 1024;
var
  USPathAndName: TUNICODE_STRING;
  objAttrs:      TOBJECT_ATTRIBUTES;
  status:        NTSTATUS;
  hSymLinkObj:   THandle;
  linkTarget:    TUNICODE_STRING;
  tgtLen:        ULONG;
  tgtBuffer:     array [0..BUFFER_LINK_TARGET_NAME] of Widechar;
begin
  objEntry.ObjType := otSymlink;

  // Open symlink object
  AllocRtlInitUnicodeString(@USPathAndName, objEntry.FullName);
  InitializeObjectAttributes(@objAttrs,
    @USPathAndName,
    OBJ_CASE_INSENSITIVE,
    0,
    nil
    );
  status := NtOpenSymbolicLinkObject(@hSymLinkObj,
    SYMBOLIC_LINK_QUERY,
    @objAttrs);
  if NT_SUCCESS(status) then begin
    // Buffer for link target
    linkTarget.Length        := sizeof(tgtBuffer) - sizeof(tgtBuffer[0]);
    linkTarget.MaximumLength := sizeof(tgtBuffer);
    linkTarget.Buffer        := @(tgtBuffer[0]);
    status                   := NtQuerySymbolicLinkObject(hSymLinkObj, @linkTarget, @tgtLen);

    if NT_SUCCESS(status) then begin
      objEntry.SymLinkTo := UNICODE_STRINGToWideString(linkTarget);
      Print('--> "' + objEntry.SymLinkTo + '"');
    end else begin
      Print('NtQuerySymbolicLinkObject = 0x' + inttohex(status, 8));
    end;

    NtClose(hSymLinkObj);
  end else begin
    Print('NtOpenSymboliclinkObject = 0x' + inttohex(status, 8));
  end;

  FreeRtlInitUnicodeString(@USPathAndName);
end;

procedure TSDUObjManager.ProcessDirectory(objEntry: PObjMgrEntry);
begin
  objEntry.ObjType := otDirectory;

  // Recurse...
  WalkDir(objEntry.FullName);
end;


procedure TSDUObjManager.ProcessDevice(objEntry: PObjMgrEntry);
begin
  objEntry.ObjType := otDevice;
end;

function TSDUObjManager.FullPathAndName(StartDir: WideString;
  objDirInfo: POBJECT_DIRECTORY_INFORMATION
  ): WideString;
var
  pathAndName: WideString;
begin
  // StartDir "\" Name
  pathAndName := StartDir;
  // Don't double up "\" if already present
  if ((length(pathAndName) = 0) or (pathAndName[length(pathAndName)] <> '\')) then
  begin
    pathAndName := pathAndName + '\';
  end;
  pathAndName := pathAndName + UNICODE_STRINGToWideString(objDirInfo.ObjectName);

  Result := pathAndName;
end;

procedure TSDUObjManager.WalkDir(StartDir: WideString);
const
  OBJDIR_BUFFERSIZE = 1024 * 2;
var
  hDirObj:          THandle;
  status:           NTSTATUS;
  ObjectAttributes: TOBJECT_ATTRIBUTES;
  objDirInfo:       POBJECT_DIRECTORY_INFORMATION;
  UNStartDir:       TUNICODE_STRING;
  szIdentBuf:       String;
  context:          ULONG;
  dirLen:           ULONG;
  objEntry:         PObjMgrEntry;
begin
  szIdentBuf := '';

  // Open directory
  AllocRtlInitUnicodeString(@UNStartDir, StartDir);
  InitializeObjectAttributes(@ObjectAttributes,
    @UNStartDir,
    OBJ_CASE_INSENSITIVE,
    0,
    nil
    );
  status := NtOpenDirectoryObject(@hDirObj,
    (STANDARD_RIGHTS_READ or DIRECTORY_QUERY),
    @ObjectAttributes);
  if NT_SUCCESS(status) then begin
    // Spin through all directory entries...
    dirLen := 1;
    repeat
      objDirInfo := AllocMem(OBJDIR_BUFFERSIZE);
      zeromemory(objDirInfo, OBJDIR_BUFFERSIZE);
      status := NtQueryDirectoryObject(hDirObj,
        objDirInfo,
        OBJDIR_BUFFERSIZE,
        True, False,
        @context,
        @dirLen);

      if NT_SUCCESS(status) then begin
        Print(szIdentBuf + UNICODE_STRINGToWideString(objDirInfo.ObjectTypeName) +
          ' "' + UNICODE_STRINGToWideString(objDirInfo.ObjectName) + '" ');
        objEntry          := new(PObjMgrEntry);
        objEntry.Name     := UNICODE_STRINGToWideString(objDirInfo.ObjectName);
        objEntry.FullName := FullPathAndName(StartDir, objDirInfo);
        objEntry.ObjType  := otOther;
        FAllObjects.AddObject(objEntry.FullName, TObject(objEntry));
        Print('+++' + objEntry.FullName);

        if (UNICODE_STRINGToWideString(objDirInfo.ObjectTypeName) = 'Device') then begin
          ProcessDevice(objEntry);
        end;

        if (UNICODE_STRINGToWideString(objDirInfo.ObjectTypeName) = 'SymbolicLink') then begin
          ProcessSymLink(objEntry);
        end;

        if (UNICODE_STRINGToWideString(objDirInfo.ObjectTypeName) = 'Directory') then begin
          ProcessDirectory(objEntry);
        end;

      end else
      if not (NT_SUCCESS(status)) then begin
        Print('NtQueryDirectoryObject = 0x' + inttohex(status, 8) + ' (' + StartDir + ')');
      end;

      FreeMem(objDirInfo);
    until not (NT_SUCCESS(status));

    NtClose(hDirObj);
  end;

  FreeRtlInitUnicodeString(@UNStartDir);
end;


procedure TSDUObjManager.Print(line: String);
begin
  PrintOut.Add(line);
end;

procedure TSDUObjManager.Print(line: WideString);
var
  x: String;
begin
  x := line;
  PrintOut.Add(x);
end;

function TSDUObjManager.UnderlyingDeviceForName(startName: String): String;
var
  retval: String;
  obj:    PObjMgrEntry;
  idx:    Integer;
begin
  retval := '';

  idx := FAllObjects.IndexOf(startName);
  if (idx >= 0) then begin
    obj := PObjMgrEntry(FAllObjects.Objects[idx]);
    if (obj.ObjType = otSymlink) then begin
      retval := UnderlyingDeviceForName(obj.SymLinkTo);
    end else begin
      retval := obj.FullName;
    end;
  end;

  Result := retval;
end;

function TSDUObjManager.UnderlyingDeviceForDrive(driveLetter: Char): String;
var
  driveDevice: String;
begin
  driveDevice := Format(GLOBAL_DRIVE_NAME, [driveLetter]);
  Result      := UnderlyingDeviceForName(driveDevice);
end;

procedure TSDUObjManager.Rescan();
begin
  Scan();
end;

end.
