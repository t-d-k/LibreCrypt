unit OTFEPGPDisk_U;
// Description: Delphi PGPDisk Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes, SysUtils, Windows,
  OTFE_U,
  OTFEPGPDiskStructures_U;

type
  TOTFEPGPDisk = class(TOTFE)
  protected
    hPGPDiskVxD: THandle;
    procedure SetActive(AValue : Boolean); override;
    function  Connect(): boolean;
    function  Disconnect(): boolean;
    function  GetMountedDrives(volumeFilenames: TStringList; var drivesMounted: string): boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    // TOTFE functions...
    function  Title(): string; overload; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  DrivesMounted(): string; override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; overload; override;
    function  MountDevices(): string; override;
    function  CanMountDevice(): boolean; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetMainExe(): string; override;

    function  GetVolInfo(driveLetter: char): boolean;
    function  GetPGPDiskInfo(): boolean;
    function  HasDriveOpenFiles(driveLetter: char): boolean;
  end;

procedure Register;

implementation

uses ShellAPI,
     Dialogs,
     Registry, RegStr,
     SDUGeneral,
     OTFEConsts_U,
     OTFEPGPDiskMounting_U;


{$IFDEF LINUX}
This software is only intended for use under MS Windows
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}  // Useless warning about platform - we're already
                             // protecting against that!


procedure Register;
begin
  RegisterComponents('OTFE', [TOTFEPGPDisk]);
end;


constructor TOTFEPGPDisk.Create(AOwner : TComponent);
begin
  inherited create(AOwner);
  FActive := False;
end;

destructor TOTFEPGPDisk.Destroy;
begin
  if FActive then
    CloseHandle(hPGPDiskVxD);
  inherited Destroy;
end;

function TOTFEPGPDisk.Connect(): boolean;
begin
  Result := Active;
  if not(Active) then
    begin
    hPGPDiskVxD := CreateFile(PGPDISK_DRIVER_NAME,
                                GENERIC_READ OR GENERIC_WRITE,
                                0,
                                nil,
                                OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL,
                                0);
    if hPGPDiskVxD = INVALID_HANDLE_VALUE then
      begin
      raise EPGPDiskVxdNotFound.Create('PGPDisk device driver not present'#13#10 +
        'If you have just installed PGPDisk without restarting'+#13#10 +
        'the computer, you need to restart it now if you wish to'+#13#10 +
        'use PGPDisk');
      end;

    Result := TRUE;

    end;
end;


function TOTFEPGPDisk.Title(): string;
begin
  Result := 'PGPDisk';
end;


function TOTFEPGPDisk.Version(): cardinal;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_QueryVersion;
  replyVersion: PGPDISK_Rec_ADPacketHeader;
  buffer : array [0..1000] of char;
  drvVers: PGPUint32;
begin
  CheckActive();

  replyVersion.magic	:= PGPDISK_ADPacketMagic;
  replyVersion.code	:= PGPDISK_AD_QueryVersion;
  replyVersion.pDerr	:= @buffer;

  query.header := replyVersion;

  query.appVersion:= PGPDISK_APP_VERSION;  // application version
  query.pDriverVersion:= @drvVers;  // driver version (PGPUInt32 *)

  DeviceIoControl(hPGPDiskVxD,
                  IOCTL_PGPDISK_SENDPACKET,
                  @query,
                  sizeof(query),
                  nil,
                  0,
                  dwBytesReturned,
                  nil);

  Result := drvVers;
end;


function TOTFEPGPDisk.Disconnect(): boolean;
begin
  if Active then
    begin
    CloseHandle(hPGPDiskVxD);
    end;
  Result := TRUE;

end;



procedure TOTFEPGPDisk.SetActive(AValue : Boolean);
var
  allOK: boolean;
begin
  allOK := FALSE;
  if AValue <> Active then
    begin
      if AValue then
        begin
        allOK := Connect();
        end
      else
        begin
        allOK := Disconnect();
        end;
    end;

  if allOK then
    begin
    inherited;
    end;

end;


// xxx - shouldn't this be the other way round?
//       i.e. Mount(volumeFilename: string;...) calls
//            Mount(volumeFilenames: TStringList;...) ???
function TOTFEPGPDisk.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  appName: string;
  parameters: string;
  cmdLine: string;

  zAppName:array[0..512] of char;
//  zCurDir:array[0..255] of char;
//  WorkDir:String;
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  plsWaitDlg: TOTFEPGPDiskMounting_F;
begin
  CheckActive();

  // This next bit is a bit of a cheat, using PGPDisk.exe, but that's how the
  // PGPDisk shell extension does it, so...
  // xxx - todo - I'll rewrite it later, so that it talks directly to the driver

  // Execute the command line.
  appName := GetMainExe();

  if appName='' then
    begin
    Result := #0;
    exit;
    end;

  parameters := 'mount "'+volumeFilename+'"';
  cmdLine := appName + ' ' + parameters;

// We can't use this because it takes 23 seconds to return after PGPDisk
// appears to close
//  SDUWinExecAndWait32(cmdLine, SW_SHOWNORMAL);

  StrPCopy(zAppName, cmdLine);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := cmdShow;

  plsWaitDlg:= TOTFEPGPDiskMounting_F.Create(nil);
  try
    if CreateProcess(PChar(appName),
                     PChar(cmdLine),
                     nil,
                     nil,
                     false,
                     CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                     nil,
                     nil,
                     StartupInfo,
                     ProcessInfo) then
      begin
      // Because PGPDisk.exe typically takes around 23 seconds to terminate if
      // we were to monitor it using code like:
      //
      //   WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
      //   GetExitCodeProcess(ProcessInfo.hProcess, retVal);
      //   CloseHandle(ProcessInfo.hProcess);
      //   CloseHandle(ProcessInfo.hThread);
      //
      // (presumably due to some kind of timeout on the broadcast message
      // stating that the drive's mounted) we use a *seriously* bizarre way of
      // finding out when PGPDisk.exe terminates.
      //
      // In a nutshell, we create a modal form that calls GetExitCodeProcess,
      // and checks if the PGPDisk.exe has terminated. If it has, it closes the
      // form. Because it's a modal form, it's idle, except when the periodic
      // check is carried out, and so doesn't freeze up or anything.
      //
      // Sheesh! Anyone think of a better way of doing this?
      //
      plsWaitDlg.MonitorProcess := ProcessInfo.hProcess;
      plsWaitDlg.ShowModal();

      // We don't need this junk anymore. Close it.
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      end;
  finally
    plsWaitDlg.Free();
  end;

  Result := GetDriveForVolFile(volumeFilename);
  if Result=#0 then
    begin
    FLastErrCode:= OTFE_ERR_USER_CANCEL;
    end;

end;

function TOTFEPGPDisk.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
var
  i: integer;
  tmpDrv: char;
begin
  Result := FALSE;
  mountedAs := '';

  for i:=0 to (volumeFilenames.count-1) do
    begin
    tmpDrv:=Mount(volumeFilenames[i], readonly);
    mountedAs := mountedAs + tmpDrv;

    if tmpDrv<>#0 then
      begin
      Result:= TRUE;
      end;
      
    end;

end;


function TOTFEPGPDisk.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
var
  driveLetter: char;
begin
  CheckActive();

  Result := FALSE;

  driveLetter := GetDriveForVolFile(volumeFilename);
  if driveLetter<>#0 then
    begin
    Result := Dismount(driveLetter, emergency);
    end;

end;

function TOTFEPGPDisk.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_Unmount;
  packetHeader: PGPDISK_Rec_ADPacketHeader;

  buffer : array [0..1000] of char;

  driveNum: integer;
  drivesMtd: string;
begin
  CheckActive();

  Result := FALSE;

  drivesMtd := DrivesMounted();
  if Pos(driveLetter, drivesMtd)>0 then
    begin
    driveLetter := upcase(driveLetter);
    driveNum := ord(driveLetter)-ord('A');

    packetHeader.magic := PGPDISK_ADPacketMagic;
    packetHeader.code  := PGPDISK_AD_Unmount;
    packetHeader.pDerr := @buffer;
    //  packetHeader.pDerr := nil;

    query.header := packetHeader;

    query.drive := driveNum;
    if emergency then
      begin
      query.isThisEmergency := PGPDISK_BOOL_TRUE;
      end
    else
      begin
      query.isThisEmergency := PGPDISK_BOOL_FALSE;
      end;

    DeviceIoControl(hPGPDiskVxD,
                    IOCTL_PGPDISK_SENDPACKET,
                    @query,
                    sizeof(query),
                    nil,
                    0,
                    dwBytesReturned,
                    nil);

    drivesMtd := DrivesMounted();
    Result := (Pos(driveLetter, drivesMtd)<1);
    end;

end;


// This function is not particularly useful...
function TOTFEPGPDisk.GetPGPDiskInfo(): boolean;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_GetPGPDiskInfo;
  packetHeader: PGPDISK_Rec_ADPacketHeader;

  buffer : array [0..1000] of char;

  driveInfo: array [1..PGPDISK_MAX_DRIVES] of PGPDISK_Rec_PGPdiskInfo;
begin
  CheckActive();

  packetHeader.magic := PGPDISK_ADPacketMagic;
  packetHeader.code  := PGPDISK_AD_GeTOTFEPGPDiskInfo;
  packetHeader.pDerr := @buffer;

  query.header := packetHeader;

  query.arrayElems:= PGPDISK_MAX_DRIVES;
  query.pPDIArray := @driveInfo;

  DeviceIoControl(hPGPDiskVxD,
                  IOCTL_PGPDISK_SENDPACKET,
                  @query,
                  sizeof(query),
                  nil,
                  0,
                  dwBytesReturned,
                  nil);

  Result := TRUE;
end;


// This function is not particularly useful...
function TOTFEPGPDisk.GetVolInfo(driveLetter: char): boolean;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_QueryVolInfo;
  packetHeader: PGPDISK_Rec_ADPacketHeader;

  buffer : array [0..1000] of char;

  blockSize: PGPUInt16;
  totalBlocks: PGPUInt32;
begin
  CheckActive();

  packetHeader.magic := PGPDISK_ADPacketMagic;
  packetHeader.code  := PGPDISK_AD_QueryVolInfo;
  packetHeader.pDerr := @buffer;

  query.header := packetHeader;
  query.drive := ord(upcase(driveLetter))-ord('A');

  blockSize := 42; // junk number, set to anything (not needed, just set to assist bugfixing)
  totalBlocks := 41; // junk number, set to anything (not needed, just set to assist bugfixing)

  query.pBlockSize := @blockSize;
  query.pTotalBlocks := @totalBlocks;

  DeviceIoControl(hPGPDiskVxD,
                  IOCTL_PGPDISK_SENDPACKET,
                  @query,
                  sizeof(query),
                  nil,
                  0,
                  dwBytesReturned,
                  nil);

{
  pBlockSize: ^PGPUInt16;  // return blocksize here (PGPUInt16 *)
  pTotalBlocks: ^PGPUInt64;  // return total blocks here (PGPUInt64 *)
}


  Result := TRUE;
end;


function TOTFEPGPDisk.HasDriveOpenFiles(driveLetter: char): boolean;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_QueryOpenFiles;
  packetHeader: PGPDISK_Rec_ADPacketHeader;

  buffer : array [0..1000] of char;

  answer: PGPBoolean;
begin
  CheckActive();

  packetHeader.magic := PGPDISK_ADPacketMagic;
  packetHeader.code  := PGPDISK_AD_QueryOpenFiles;
  packetHeader.pDerr := @buffer;

  query.header := packetHeader;
  query.drive := ord(upcase(driveLetter))-ord('A');

  query.pHasOpenFiles:= @answer;  // pointer to answer (PGPBoolean *)

  DeviceIoControl(hPGPDiskVxD,
                  IOCTL_PGPDISK_SENDPACKET,
                  @query,
                  sizeof(query),
                  nil,
                  0,
                  dwBytesReturned,
                  nil);

  Result := (answer=PGPDISK_BOOL_TRUE);
end;

function TOTFEPGPDisk.IsEncryptedVolFile(volumeFilename: string): boolean;
var
  testFile: TFileStream;
  readIn: integer;
  readInHeader: PGPDISK_Rec_PGPdiskFileHeaderInfo;
begin
  try
    testFile := TFileStream.Create(volumeFilename, fmOpenRead OR fmShareDenyNone);
  except
    on E:EFOpenError do
      begin
      Result := FALSE;
      exit;
      end;
  end;

  readIn := testFile.Read(readInHeader, sizeof(readInHeader));
  testFile.Free();

  Result := (readIn=sizeof(readInHeader)) AND (readInHeader.headerMagic = PGPDISK_HeaderMagic);

end;

function TOTFEPGPDisk.GetDriveForVolFile(volumeFilename: string): char;
var
  mtdVolFiles: TStringList;
  mtdDrives: string;
begin
  CheckActive();

  Result := #0;

  mtdVolFiles:= TStringList.Create();
  try
    if GetMountedDrives(mtdVolFiles, mtdDrives) then
      begin
      volumeFilename := SDUConvertSFNToLFN(volumeFilename);
      if mtdVolFiles.IndexOf(volumeFilename)>-1 then
        begin
        Result := mtdDrives[mtdVolFiles.IndexOf(volumeFilename)+1];
        end;
      end;
  finally
    mtdVolFiles.Free();
  end;

end;


function TOTFEPGPDisk.GetVolFileForDrive(driveLetter: char): string;
var
  mtdVolFiles: TStringList;
  mtdDrives: string;
begin
  CheckActive();

  Result := '';

  mtdVolFiles:= TStringList.Create();
  try
    if GetMountedDrives(mtdVolFiles, mtdDrives) then
      begin
      driveLetter := upcase(driveLetter);
      if Pos(driveLetter, mtdDrives)>0 then
        begin
        Result := mtdVolFiles[Pos(driveLetter, mtdDrives)-1];
        end;
      end;
  finally
    mtdVolFiles.Free();
  end;

end;


function TOTFEPGPDisk.DrivesMounted(): string;
var
  drivesMounted: string;
begin
  GetMountedDrives(nil, drivesMounted);
  Result := SortString(drivesMounted);

end;

function TOTFEPGPDisk.GetMainExe(): string;
var
  registry: TRegistry;
  keyName: string;
  appName: string;
begin
  Result := '';
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  appName := PGPDISK_APP_NAME;
  appName := appName + '.exe';

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := REGSTR_PATH_APPPATHS + '\' + appName;
    if registry.OpenKeyReadOnly(keyName) then
      begin
      Result := registry.ReadString('');
      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;

  if Result<>'' then
    begin
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

end;


function TOTFEPGPDisk.VersionStr(): string;
begin
  Result := 'v0x'+inttohex(Version(), 8);

end;


// Set volumeFilenames to nil if you only want drivesMounted
// !! WARNING !!
// You may find that neither volumeFienames nor drivesMounted are in any
// particular order, although volumeFilenames[x] is the file for drives
// drivesMounted[x]
// (Having said that, drivesMounted should be in alphabetical order, and
// therefore volumeFilenames will reflect this order)
function TOTFEPGPDisk.GetMountedDrives(volumeFilenames: TStringList; var drivesMounted: string): boolean;
var
  dwBytesReturned : DWORD;
  query: PGPDISK_Rec_AD_GetPGPDiskInfo;
  packetHeader: PGPDISK_Rec_ADPacketHeader;

  buffer: array [0..1000] of char;

  currDriveLetter: string;
  i: integer;
  driveInfo: array [1..PGPDISK_MAX_DRIVES] of PGPDISK_Rec_PGPdiskInfo;
  currDriveFilename: string;
begin
  CheckActive();

  packetHeader.magic := PGPDISK_ADPacketMagic;
  packetHeader.code  := PGPDISK_AD_GeTOTFEPGPDiskInfo;
  packetHeader.pDerr := @buffer;

  query.header := packetHeader;

  query.arrayElems:= PGPDISK_MAX_DRIVES;
  query.pPDIArray := @driveInfo;

  DeviceIoControl(hPGPDiskVxD,
                  IOCTL_PGPDISK_SENDPACKET,
                  @query,
                  sizeof(query),
                  nil,
                  0,
                  dwBytesReturned,
                  nil);

  if volumeFilenames<>nil then
    begin
    volumeFilenames.Clear();
    end;

  for i:=1 to query.arrayElems do
    begin
    if driveInfo[i].drive<>PGPDISK_INVALID_DRIVE then
      begin
      currDriveLetter := chr(driveInfo[i].drive+ord('A'));
      drivesMounted := drivesMounted + currDriveLetter[1];

      currDriveFilename := copy(driveInfo[i].path, 1, pos(#0, driveInfo[i].path)-1);
      currDriveFilename := SDUConvertSFNToLFN(currDriveFilename);
      if volumeFilenames<>nil then
        begin
        volumeFilenames.Add(currDriveFilename);
        end;
      end;
    end;

  Result := TRUE;

end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEPGPDisk.MountDevices(): string;
begin
  // Not supported...
  Result := #0;
end;

// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFEPGPDisk.CanMountDevice(): boolean;
begin
  // Not supported...
  Result := FALSE;
end;

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN SYMBOL_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

