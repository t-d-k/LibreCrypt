unit OTFETrueCrypt_U;
// Description: Delphi TrueCrypt Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// TRUE if integer<>0
// FALSE if integer=0


interface

uses
  Classes, SysUtils, Windows, forms, controls,
  OTFE_U, OTFETrueCryptStructures_U;

type
  TOTFETrueCrypt = class(TOTFE)
  private
    hTrueCryptVxD : THandle;
    FPasswordPrompt: string;

    FMountDeviceDlg9xStyle: boolean;
    FTrueCryptVersionHint: TTrueCryptVersionHint;

    FCachePasswordsInDriver: boolean;
    FOpenExplorerWindowAfterMount: boolean;
    FCloseExplorerWindowsOnDismount: boolean;
    FSaveMountedVolumesHistory: boolean;
    FWipePasswordCacheOnExit: boolean;
    FLastSelectedDrive: char;
    FLastMountedVolume: TStringList;

    function  ld(driveNum: integer; mode: integer): boolean;
    function  CloseSlot(driveNum: integer; brutal: boolean): boolean;

    function  locklogdrive(drivenum: integer; mode: integer): boolean;
    function  ioctllock(nDrive: cardinal; permissions: integer; func: integer): integer;
    function  DoDeviceClose(driveNum: integer): boolean;
    function  EjectStop(driveLetter: char; func: boolean): boolean;

    function  GetNextUnusedDrvLtr(afterDrv: char): char;
    function  GetUnusedDriveLetters(): string;

    function  UseVersionHint(): TTrueCryptVersionHint;

  protected
    TrueCryptDriverName: string;
    FTrueCryptDriverVersion: cardinal;
    CurrentOS: integer;

    function  Connect(): boolean;
    function  Disconnect(): boolean;
    procedure SetActive(AValue : Boolean); override;

    function  GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;
    function  GetDisksMounted_PRE30(var mountedDrives: string; volumeFilenames: TStrings): boolean;
    function  GetDisksMounted_30(var mountedDrives: string; volumeFilenames: TStrings): boolean;

    function  Mount_PRE30(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
    function  Mount_30(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;

    function  GetWordFromHeader(buffer: array of byte; var posInBuffer: integer): cardinal;
    function  GetCipherNames(cyphers: array of TrueCrypt_CIPHER_TYPE): string;
    function  GetVolumeInfo(volumeFilename: string; info: pTOTFETrueCryptVolumeInfo): boolean;

    function  GetAvailableRemovables(dispNames: TStringList; deviceNames: TStringList): integer;
    function  GetAvailableFixedDisks(dispNames: TStringList; deviceNames: TStringList): integer;
    function  OpenDevice(device: string): boolean;

    procedure ShortArrayToString(theArray: array of WCHAR; var theString: string);
    procedure StringToShortArray(theString: string; var theArray: array of WCHAR);


    function  IdentifyVolumeFileType(volumeFilename: string): TrueCrypt_VOLUME_FILE_TYPE;

    function  GetCyphersForTrueCryptCypherID(cypherID: integer; var cyphers: array of TrueCrypt_CIPHER_TYPE; var cypherMode: TrueCrypt_CIPHER_MODE): boolean;

    procedure LoadSettings();
    procedure SaveSettings();

    // Returns TRUE if "volumeFilename" is a partition, not a file
    function  IsFilePartition(volumeFilename: string): boolean;

    function  Dismount_PRE30(driveLetter: char; emergency: boolean = FALSE): boolean;
    function  Dismount_30(driveLetter: char; emergency: boolean = FALSE): boolean; 

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    // TOTFE functions...
    function  Title(): string; overload; override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean; override;
    function  MountDevices(): string; override;
    function  CanMountDevice(): boolean; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    // !! WARNING !!
    // Due to limitations of the TrueCrypt driver, only up to the first 64
    // chars of the volumeFilename are significant!
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  DrivesMounted(): string; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    // !! WARNING !!
    // Due to limitations of the TrueCrypt driver, only up to the first 64
    // chars of the volumeFilename are significant!
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetMainExe(): string; override;


    function  GetDriveInfo(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
    function  GetDriveInfo_PRE30(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
    function  GetDriveInfo_30(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
    function  GetDriveInfo_31a(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;

    function  ClearPasswords(driveLetter: char): boolean;
    function  GetAvailableRawDevices(dispNames: TStringList; deviceNames: TStringList): boolean;

    // "idx" should be between 1 and TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME
    // Returns '' on error
    // Also returns '' if there is no filename in that position
    function  GetLastMountedVolume(idx: integer): string;
    function  SetLastMountedVolume(idx: integer; filename: string): boolean;
    // Note: Calling AddLastMountedVolume will push the oldest filename on the MRU
    // list off
    procedure AddLastMountedVolume(volumeFilename: string);

  published
    property PasswordPrompt: string read FPasswordPrompt write FPasswordPrompt;

    property CachePasswordsInDriver: boolean read FCachePasswordsInDriver write FCachePasswordsInDriver default FALSE;
    property OpenExplorerWindowAfterMount: boolean read FOpenExplorerWindowAfterMount write FOpenExplorerWindowAfterMount default FALSE;
    property CloseExplorerWindowsOnDismount: boolean read FCloseExplorerWindowsOnDismount write FCloseExplorerWindowsOnDismount default FALSE;
    property SaveMountedVolumesHistory: boolean read FSaveMountedVolumesHistory write FSaveMountedVolumesHistory default FALSE;
    property WipePasswordCacheOnExit: boolean read FWipePasswordCacheOnExit write FWipePasswordCacheOnExit default FALSE;
    property LastSelectedDrive: char read FLastSelectedDrive write FLastSelectedDrive default #0;

    property MountDeviceDlg9xStyle: boolean read FMountDeviceDlg9xStyle write FMountDeviceDlg9xStyle default FALSE;
    property VersionHint: TTrueCryptVersionHint read FTrueCryptVersionHint write FTrueCryptVersionHint default tcvAuto;
  end;

procedure Register;


implementation

uses
  Messages,  // Required for "WM_DEVICECHANGE"
  ShellAPI,
  dialogs,
  Registry, RegStr,
  INIFiles,
  Math,
  OTFEConsts_U,
  OTFETrueCryptPasswordEntry_U,
  OTFETrueCryptMountDevice_U,
  OTFETrueCryptMountDevice9x_U,
  OTFETrueCryptMountDeviceNT_U,
  SDUGeneral,
  ShlObj;  // Required for SHChangeNotify

procedure Register;
begin
  RegisterComponents('OTFE', [TOTFETrueCrypt]);
end;



constructor TOTFETrueCrypt.Create(AOwner : TComponent);
var
  os: OSVERSIONINFO;
  i: integer;
begin
  inherited create(AOwner);

  // Pull down the windows version
  os.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
  if (GetVersionEx(os) = FALSE) then
    begin
    raise ETrueCryptError.Create('Unable to determine OS');
    end
  else if (os.dwPlatformId = VER_PLATFORM_WIN32_NT) then
    begin
    CurrentOS := TrueCrypt_OS_WIN_NT;
    end
  else if ((os.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and (os.dwMajorVersion = 4) and (os.dwMinorVersion = 0)) then
    begin
    CurrentOS := TrueCrypt_OS_WIN_95;
    end
  else if ((os.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and (os.dwMajorVersion = 4) and (os.dwMinorVersion >= 10)) then
    begin
    // Note: This is ">= 10" in order to catch both Win98 and WinMe
    CurrentOS := TrueCrypt_OS_WIN_98;
    end
  else
    begin
    // Fallback to NT/2K/XP on the basis that only TrueCrypt v1.0 supported
    // Windows 9x/Me
    CurrentOS := TrueCrypt_OS_WIN_NT;
    end;


  FActive := False;
  FTrueCryptDriverVersion := $FFFFFFFF;
  
  FLastMountedVolume:= TStringList.Create();
  // Initialise with empty strings
  for i:=1 to TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME do
    begin
    FLastMountedVolume.Add('');
    end;



  FPasswordPrompt := 'Enter password for %s';

  LoadSettings();

end;


destructor TOTFETrueCrypt.Destroy;
begin
  SaveSettings();

  FLastMountedVolume.Free();

  if FActive then
    begin
    CloseHandle(hTrueCryptVxD);
    end;

  inherited Destroy;
end;


function TOTFETrueCrypt.Connect(): boolean;
var
  OSVersion: TOSVersionInfo;
  dwResult: DWORD;
begin
  Result := Active;
  if not(Active) then
    begin
    if CurrentOS = TrueCrypt_OS_WIN_NT then
      begin
      TrueCryptDriverName := TrueCrypt_WIN32_ROOT_PREFIX;
      end
    else
      begin
      TrueCryptDriverName := TrueCrypt_WIN9X_DRIVER_NAME;
      end;

    hTrueCryptVxD := CreateFile(
                                PChar(TrueCryptDriverName),
                                0,
                                0,
                                nil,
                                OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL,
                                0
                               );

    if hTrueCryptVxD = INVALID_HANDLE_VALUE then
      begin
      raise ETrueCryptVxdNotFound.Create('TrueCrypt device driver not found');
      end
    else
      begin
      OSVersion.dwOSVersionInfoSize := SizeOf(OSVersion);
      GetVersionEx(OSVersion);
      FActive := TRUE;
      // This *ONLY* applies to Windows *98*
      if (CurrentOS = TrueCrypt_OS_WIN_98) then
        begin
        // We're runninng TrueCrypt v2.0.1 under w98
        DeviceIoControl(hTrueCryptVxD,
                        TrueCrypt_IOCTL_ALLOW_FAST_SHUTDOWN,
                        nil,
                        0,
                        nil,
                        0,
                        dwResult,
                        nil);
        end;

      Result := TRUE;
      end;

    end;

end;

function TOTFETrueCrypt.Disconnect(): boolean;
begin
  if Active then
    begin
    CloseHandle(hTrueCryptVxD);
    end;
  Result := TRUE;

end;


procedure TOTFETrueCrypt.SetActive(AValue : Boolean);
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
    if Active then
      begin
      FTrueCryptDriverVersion := Version();
      end;
    end;

end;

function TOTFETrueCrypt.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
begin
  if (Version() < $300) then
    begin
    Result := Dismount_PRE30(driveLetter, emergency);
    end
  else
    begin
    Result := Dismount_30(driveLetter, emergency);
    end;

end;

// Note that the emergency parameter is IGNORED
function TOTFETrueCrypt.Dismount_PRE30(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  inbuf: array [1..80] of char;
  outbuf: array [1..80] of char;
  bytesRead: DWORD;
  serviceCmd: string;
  i: integer;
  nDosDriveNo: integer;
begin
  CheckActive();

  Result := FALSE;

  driveLetter := upcase(driveLetter);

  if (pos(driveLetter, DrivesMounted()) < 1) then
    begin
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    exit;
    end;

  nDosDriveNo := ord(upcase(driveLetter))-ord('A');

  if (CurrentOS = TrueCrypt_OS_WIN_NT) then
    begin
    // Unmount the volume using the TrueCryptService, this is done to allow
    // non-administrators to unmount volumes
    // Doing it with the DeviceIOControl DISMOUNT is a bad idea as we would need
    // to lock the volume first, which non-administrators can't do
    serviceCmd := 'unmount '+inttostr(nDosDriveNo);
    for i:=1 to length(serviceCmd) do
      begin
      outbuf[i] := serviceCmd[i];
      end;
    outbuf[length(serviceCmd)+1] := #0;

    if CallNamedPipe(
                     TrueCrypt_PIPE_SERVICE,
                     @outbuf,
                     sizeof(outbuf),
                     @inbuf,
                     sizeof(inbuf),
                     bytesRead,
                     NMPWAIT_WAIT_FOREVER
                    ) then
      begin
      Result := (inbuf[1]<>'-');
      if not(Result) then
        begin
        if pos(inttostr(TrueCrypt_ERR_FILES_OPEN_LOCK), inbuf)>0 then
          begin
          FLastErrCode := OTFE_ERR_FILES_OPEN;
          end;
        end;
      end;
    end // if CurrentOS = TrueCrypt_OS_WIN_NT then
  else
    begin
    Result := CloseSlot(nDosDriveNo, emergency);
    end; // if not(if CurrentOS = TrueCrypt_OS_WIN_NT) then

end;


function TOTFETrueCrypt.Dismount_30(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  nDosDriveNo: integer;
  query: TOTFETrueCrypt_UNMOUNT_STRUCT_30;
  dwResult: DWORD;
  allOK: boolean;
  i: integer;
begin
  CheckActive();

  allOK := FALSE;
  driveLetter := upcase(driveLetter);

  if (pos(driveLetter, DrivesMounted()) < 1) then
    begin
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    Result := FALSE;
    exit;
    end;

  BroadcastDriveChangeMessage(DBT_DEVICEREMOVEPENDING, driveLetter);

  nDosDriveNo := ord(driveLetter)-ord('A');

  for i:=low(query.junkPadding) to high(query.junkPadding) do
    begin
    query.junkPadding[i] := 0;
    end;

  query.nDosDriveNo := nDosDriveNo;
  query.ignoreOpenFiles := emergency;
  query.nReturnCode := 0;

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_UNMOUNT,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwResult,
                     nil) then
    begin
    allOK := (query.nReturnCode = 0);
    end;


  if allOK then
    begin
    BroadcastDriveChangeMessage(FALSE, driveLetter);
    end
  else
    begin
    // Assume files open...
    FLastErrCode := OTFE_ERR_FILES_OPEN;
    end;

  Result := allOK;
end;


function TOTFETrueCrypt.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  // Don't need to convert volumeFilename to SFN/LFN, this is done in
  // GetDriveForVolFile(...)
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


function TOTFETrueCrypt.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  stlVolumes: TStringList;
  mountedAs: AnsiString;
begin
  CheckActive();

  Result := #0;

  stlVolumes:= TStringList.Create();
  try
    stlVolumes.Add(volumeFilename);
    if Mount(stlVolumes, mountedAs, readonly) then
      begin
      Result := mountedAs[1];
      end;
  finally
    stlVolumes.Free();
  end;

end;


function TOTFETrueCrypt.Mount(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
begin
  if (Version() < $300) then
    begin
    Result := Mount_PRE30(volumeFilenames, mountedAs, readonly);
    end
  else
    begin
    Result := Mount_30(volumeFilenames, mountedAs, readonly);
    end;

end;


function TOTFETrueCrypt.Mount_PRE30(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
var
  query: TOTFETrueCrypt_MOUNT_STRUCT_PRE30;
  dwBytesReturned: DWORD;
  volumeLoop: integer;
  i: integer;
  driveLetter: char;
  thePassword: string;
  oneOK: boolean;
  passwordDlg: TOTFETrueCryptPasswordEntry_F;

  currAllOK: boolean;
  mountedDrvLetter: char;
  drvLtrsFree: string;
  pwEntryDlgTitle: string;
  currVolValid: boolean;

  serviceCmd: string;
  inbuf: array [1..80] of char;
  outbuf: array [1..80] of char;
  bytesRead: DWORD;

  tmpFilename: string;
begin
  CheckActive();
  Result := FALSE;

  oneOK := FALSE;
  mountedAs := '';

  pwEntryDlgTitle := Format(FPasswordPrompt, [volumeFilenames[0]]);

  // Get the password/starting drive
  passwordDlg:= TOTFETrueCryptPasswordEntry_F.Create(nil);
  try
    passwordDlg.DriverCachePassword := FCachePasswordsInDriver;

    passwordDlg.UserCanMountReadonly  := FALSE;
    passwordDlg.UserCanMountRemovable := FALSE;
    passwordDlg.UserCanForceMount    := FALSE;


    // The user should be prompted for the drive letter, and offered the
    // default
    drvLtrsFree := GetUnusedDriveLetters();
    driveLetter := LastSelectedDrive;  // Last selected drive letter (if any) stored in registry

    if (driveLetter=#0) then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      // If we're on A or B, skip this drive
      if (driveLetter='A') or (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      // If we're on B, skip this drive
      if (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      end
    else if (pos(driveLetter, drvLtrsFree) < 1) then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      end;
        
    passwordDlg.DrivesAllowed := drvLtrsFree;
    passwordDlg.Drive := driveLetter;

    passwordDlg.DriverCachePassword := CachePasswordsInDriver; // looked up from the registry

    passwordDlg.caption := pwEntryDlgTitle;

    if passwordDlg.ShowModal()=mrCancel then
      begin
      passwordDlg.ClearEnteredPassword();
      // Result already = FALSE, so just set the error and exit
      FLastErrCode := OTFE_ERR_USER_CANCEL;
      exit;
      end;

    CachePasswordsInDriver := passwordDlg.DriverCachePassword;
    LastSelectedDrive := passwordDlg.Drive;

    thePassword := passwordDlg.mePassword.text;
    passwordDlg.ClearEnteredPassword();

  finally
    passwordDlg.Free();
  end;


  driveLetter := LastSelectedDrive;
  if (driveLetter=#0) then
    begin
    // Result already = FALSE, so just set the error and exit
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    Result := FALSE;
    exit;
    end;

  for i:=1 to length(thePassword) do
    begin
    query.szPassword[i-1] := thePassword[i];
    // Overwrite password chars once used...
    thePassword[i] := char(Random(256));
    end;
  // No +1 here; szPassword indexes from zero.
  query.szPassword[length(thePassword)] := #0;

  query.nPasswordLen := length(thePassword);

  query.bCache := CachePasswordsInDriver;

  for i:=low(query.junkPadding) to high(query.junkPadding) do
    begin
    query.junkPadding[i] := 0;
    end;


  for volumeLoop:=0 to (volumeFilenames.count-1) do
    begin
    mountedDrvLetter := #0;

    if (driveLetter<>#0) then
      begin
      query.nDosDriveNo := ord(upcase(driveLetter))-ord('A');

      // If it's not a partition, check that the file exists...
      currVolValid := TRUE;
      if not(IsFilePartition(volumeFilenames[volumeLoop])) then
        begin
        currVolValid := FileExists(volumeFilenames[volumeLoop]);
        if not(currVolValid) then
          begin
          FLastErrCode:= OTFE_ERR_VOLUME_FILE_NOT_FOUND;
          end;
          
        end;

      AddLastMountedVolume(volumeFilenames[volumeLoop]);

      if currVolValid then
        begin
        query.time := DateTimeToFileDate(now);
        query.nReturnCode := 0;

        // Convert filename to UNICODE, if needed
        StringToShortArray(volumeFilenames[volumeLoop], query.wszVolume);

        currAllOK := DeviceIoControl(hTrueCryptVxD,
                                     TrueCrypt_IOCTL_MOUNT,
                                     @query,
                                     sizeof(query),
                                     @query,
                                     sizeof(query),
                                     dwBytesReturned,
                                     nil);
        currAllOK := currAllOK AND (query.nReturnCode=0);

        if not(currAllOK) then
          begin
          FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
          end
        else
          begin
          if (CurrentOS = TrueCrypt_OS_WIN_NT) then
            begin
            serviceCmd := 'mount '+inttostr(query.nDosDriveNo);
            for i:=1 to length(serviceCmd) do
              begin
              outbuf[i] := serviceCmd[i];
              end;
            outbuf[length(serviceCmd)+1] := #0;

            if CallNamedPipe(
                             TrueCrypt_PIPE_SERVICE,
                             @outbuf,
                             sizeof(outbuf),
                             @inbuf,
                             sizeof(inbuf),
                             bytesRead,
                             NMPWAIT_WAIT_FOREVER
                            ) then
              begin
              // If the return value comes back with "-ERR", then there was a
              // problem
              if (inbuf[1] = '-') then
                begin
                FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
                end
              else
                begin
                // Set oneOK to TRUE if at least *one* volume mounted OK
                oneOK := TRUE;
                mountedDrvLetter := driveLetter;
                end;

              end;

            end
          else  // if (CurrentOS = TrueCrypt_OS_WIN_NT) then
            begin
            tmpFilename := volumeFilenames[volumeLoop];
            EjectStop(upcase(tmpFilename[1]), TRUE);

            // Set oneOK to TRUE if at least *one* volume mounted OK
            oneOK := TRUE;
            mountedDrvLetter := driveLetter;
            end;

          end;

        end; // if IsEncryptedVolFile(currVolFilename) then


      if volumeLoop<(volumeFilenames.count-1) then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        if driveLetter=#0 then
          begin
          FLastErrCode := OTFE_ERR_INVALID_DRIVE;
          break;
          end;
        end;

      end;  // if (driveLetter<>#0) then

    mountedAs := mountedAs + mountedDrvLetter;
    end;

  // Pad out the string with #0, if needed
  while length(mountedAs)<volumeFilenames.count do
    begin
    mountedAs := mountedAs + #0;
    end;

  Result := oneOK;

end;

function TOTFETrueCrypt.Mount_30(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
var
  query: TOTFETrueCrypt_MOUNT_STRUCT_30;
  dwBytesReturned: DWORD;
  volumeLoop: integer;
  i: integer;
  driveLetter: char;
  thePassword: string;
  oneOK: boolean;
  passwordDlg: TOTFETrueCryptPasswordEntry_F;

  currAllOK: boolean;
  mountedDrvLetter: char;
  drvLtrsFree: string;
  pwEntryDlgTitle: string;
  currVolValid: boolean;

  forceMount: boolean;
  removable: boolean;

  osVersionInfo: TOSVERSIONINFO;
begin
  CheckActive();
  Result := FALSE;

  oneOK := FALSE;
  mountedAs := '';

  pwEntryDlgTitle := Format(FPasswordPrompt, [volumeFilenames[0]]);

  // Get the password/starting drive
  passwordDlg:= TOTFETrueCryptPasswordEntry_F.Create(nil);
  try
    passwordDlg.DriverCachePassword := FCachePasswordsInDriver;

    passwordDlg.UserCanMountReadonly  := TRUE;
    passwordDlg.UserCanMountRemovable := TRUE;
    passwordDlg.UserCanForceMount     := TRUE;

    passwordDlg.MountReadonly := readonly;

    // The user should be prompted for the drive letter, and offered the
    // default
    drvLtrsFree := GetUnusedDriveLetters();
    driveLetter := LastSelectedDrive;  // Last selected drive letter (if any) stored in registry

    if (driveLetter=#0) then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      // If we're on A or B, skip this drive
      if (driveLetter='A') or (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      // If we're on B, skip this drive
      if (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      end
    else if (pos(driveLetter, drvLtrsFree) < 1) then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      end;
        
    passwordDlg.DrivesAllowed := drvLtrsFree;
    passwordDlg.Drive := driveLetter;

    passwordDlg.DriverCachePassword := CachePasswordsInDriver; // looked up from the registry

    passwordDlg.caption := pwEntryDlgTitle;

    if passwordDlg.ShowModal()=mrCancel then
      begin
      passwordDlg.ClearEnteredPassword();
      // Result already = FALSE, so just set the error and exit
      FLastErrCode := OTFE_ERR_USER_CANCEL;
      exit;
      end;

    CachePasswordsInDriver := passwordDlg.DriverCachePassword;
    LastSelectedDrive := passwordDlg.Drive;

    forceMount := passwordDlg.ForceMount;
    readonly   := passwordDlg.MountReadonly;
    removable  := passwordDlg.MountAsRemovable;

    thePassword := passwordDlg.mePassword.text;
    passwordDlg.ClearEnteredPassword();

  finally
    passwordDlg.Free();
  end;


  driveLetter := LastSelectedDrive;
  if (driveLetter=#0) then
    begin
    // Result already = FALSE, so just set the error and exit
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    Result := FALSE;
    exit;
    end;

  for i:=1 to length(thePassword) do
    begin
    query.szPassword[i-1] := thePassword[i];
    // Overwrite password chars once used...
    thePassword[i] := char(Random(256));
    end;
  // No +1 here; szPassword indexes from zero.
  query.szPassword[length(thePassword)] := #0;

  query.nPasswordLen := length(thePassword);

  query.bCache := CachePasswordsInDriver;

  for i:=low(query.junkPadding1) to high(query.junkPadding1) do
    begin
    query.junkPadding1[i] := 0;
    end;
  for i:=low(query.junkPadding2) to high(query.junkPadding2) do
    begin
    query.junkPadding2[i] := 0;
    end;
  for i:=low(query.junkPadding3) to high(query.junkPadding3) do
    begin
    query.junkPadding3[i] := 0;
    end;
  for i:=low(query.junkPadding4) to high(query.junkPadding4) do
    begin
    query.junkPadding4[i] := 0;
    end;
  for i:=low(query.junkPadding5) to high(query.junkPadding5) do
    begin
    query.junkPadding5[i] := 0;
    end;


  for volumeLoop:=0 to (volumeFilenames.count-1) do
    begin
    mountedDrvLetter := #0;

    if (driveLetter<>#0) then
      begin
      query.nDosDriveNo := ord(upcase(driveLetter))-ord('A');

      // If it's not a partition, check that the file exists...
      currVolValid := TRUE;
      if not(IsFilePartition(volumeFilenames[volumeLoop])) then
        begin
        currVolValid := FileExists(volumeFilenames[volumeLoop]);
        if not(currVolValid) then
          begin
          FLastErrCode:= OTFE_ERR_VOLUME_FILE_NOT_FOUND;
          end;
          
        end;

      AddLastMountedVolume(volumeFilenames[volumeLoop]);

      if currVolValid then
        begin
        query.bMountReadOnly := readOnly;  // Mount volume in read-only mode
        query.bMountRemovable := removable;  // Mount volume as removable media

        query.bExclusiveAccess := forceMount;  // Open host file/device in exclusive access mode
                                               //  - i.e. "Force mount" user
                                               //    option; TrueCrypt v3.0a's
                                               //    GUI only allows this to be
                                               //    set to FALSE, but the user
                                               //    can override setting it to
                                               //    TRUE if a volume is mounted
                                               //    via the TrueCrypt command
                                               //    line

        // The following logic taken from TrueCrypt v3.0a's "DLGCODE.C"...
        query.bMountManager := TRUE;  // Announce volume to mount manager
        osVersionInfo.dwOSVersionInfoSize := sizeof(osVersionInfo);
        GetVersionEx(osVersionInfo);
        // Windows 2000 mount manager causes problems with remounted volumes
        if ((osVersionInfo.dwMajorVersion = 5) and (osVersionInfo.dwMinorVersion = 0)) then
          begin
          query.bMountManager := FALSE;  // Announce volume to mount manager
          end;


        query.time := DateTimeToFileDate(now);
        query.nReturnCode := 0;

        // Convert filename to UNICODE, if needed
        StringToShortArray(volumeFilenames[volumeLoop], query.wszVolume);

        currAllOK := DeviceIoControl(hTrueCryptVxD,
                                     TrueCrypt_IOCTL_MOUNT,
                                     @query,
                                     sizeof(query),
                                     @query,
                                     sizeof(query),
                                     dwBytesReturned,
                                     nil);
        currAllOK := currAllOK AND (query.nReturnCode=0);

        if not(currAllOK) then
          begin
          FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
          end
        else
          begin
          // Set oneOK to TRUE if at least *one* volume mounted OK
          oneOK := TRUE;
          mountedDrvLetter := driveLetter;

          BroadcastDriveChangeMessage(TRUE, driveLetter);
          end;

        end; // if IsEncryptedVolFile(currVolFilename) then


      if volumeLoop<(volumeFilenames.count-1) then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        if driveLetter=#0 then
          begin
          FLastErrCode := OTFE_ERR_INVALID_DRIVE;
          break;
          end;
        end;

      end;  // if (driveLetter<>#0) then

    mountedAs := mountedAs + mountedDrvLetter;
    end;

  // Pad out the string with #0, if needed
  while length(mountedAs)<volumeFilenames.count do
    begin
    mountedAs := mountedAs + #0;
    end;

  Result := oneOK;

end;


function TOTFETrueCrypt.DrivesMounted(): string;
var
  output: string;
begin
  GetDisksMounted(output, nil);
  Result := SortString(output);

end;

// !! WARNING !!
// Under NT, the TrueCrypt driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFETrueCrypt.GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;
begin
  if (Version() < $300) then
    begin
    Result := GetDisksMounted_PRE30(mountedDrives, volumeFilenames);
    end
  else
    begin
    Result := GetDisksMounted_30(mountedDrives, volumeFilenames);
    end;

end;


// !! WARNING !!
// Under NT, the TrueCrypt driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFETrueCrypt.GetDisksMounted_PRE30(var mountedDrives: string; volumeFilenames: TStrings): boolean;
var
  dwBytesReturned : DWORD;
  query: TOTFETrueCrypt_MOUNT_LIST_STRUCT_PRE30;
  i: integer;
  currBit: cardinal;
  currVolFilename: string;
  retVal: boolean;
begin
  retVal := FALSE;
  CheckActive();

  // Cleared query.ulMountedDrives now so we can can use either later...
  query.ulMountedDrives := 0;

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_MOUNT_LIST,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwBytesReturned,
                     nil) then
    begin
    retVal := TRUE;

    mountedDrives := '';
    if volumeFilenames<>nil then
      begin
      volumeFilenames.Clear();
      end;
    currBit := 1;
    for i:=low(query.wszVolume) to high(query.wszVolume) do
      begin
      // We cleared the query.ulMountedDrives earlier, so we can do this
      if ((query.ulMountedDrives AND currBit)>0) then
        begin
        mountedDrives := mountedDrives + chr(ord('A')+i);
        if volumeFilenames<>nil then
          begin
          currVolFilename := '';
          ShortArrayToString(query.wszVolume[i], currVolFilename);

          // If we're running under NT, and it's a file that's mounted, we need
          // to strip off the "/??/" at the start of the filename returned
          if (pos('\??\', currVolFilename)=1) then
            begin
            delete(currVolFilename, 1, 4);
            end;

          if IsFilePartition(currVolFilename) then
            begin
            volumeFilenames.Add(currVolFilename);
            end
          else
            begin
            // The file may not exist if we only have the first 60 chars of the
            // filename followed by "..."
            if FileExists(currVolFilename) then
              begin
              volumeFilenames.Add(SDUConvertSFNToLFN(currVolFilename));
              end
            else
              begin
              volumeFilenames.Add(currVolFilename);
              end;
            end;

          end;
        end;
      currBit := currBit * 2;
      end;

    end;

  Result := retVal;

end;

// !! WARNING !!
// Under NT, the TrueCrypt driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFETrueCrypt.GetDisksMounted_30(var mountedDrives: string; volumeFilenames: TStrings): boolean;
var
  dwBytesReturned : DWORD;
  query: TOTFETrueCrypt_MOUNT_LIST_STRUCT_30;
  i: integer;
  currBit: cardinal;
  currVolFilename: string;
  retVal: boolean;
begin
  retVal := FALSE;
  CheckActive();

  // Cleared query.ulMountedDrives now so we can can use either later...
  query.ulMountedDrives := 0;

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_MOUNT_LIST,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwBytesReturned,
                     nil) then
    begin
    retVal := TRUE;
    mountedDrives := '';
    if volumeFilenames<>nil then
      begin
      volumeFilenames.Clear();
      end;
    currBit := 1;
    for i:=low(query.wszVolume) to high(query.wszVolume) do
      begin
      // We cleared the query.ulMountedDrives earlier, so we can do this
      if ((query.ulMountedDrives AND currBit)>0) then
        begin
        mountedDrives := mountedDrives + chr(ord('A')+i);
        if volumeFilenames<>nil then
          begin
          currVolFilename := '';
          ShortArrayToString(query.wszVolume[i], currVolFilename);

          // If we're running under NT, and it's a file that's mounted, we need
          // to strip off the "/??/" at the start of the filename returned
          if (pos('\??\', currVolFilename)=1) then
            begin
            delete(currVolFilename, 1, 4);
            end;

          if IsFilePartition(currVolFilename) then
            begin
            volumeFilenames.Add(currVolFilename);
            end
          else
            begin
            // The file may not exist if we only have the first 60 chars of the
            // filename followed by "..."
            if FileExists(currVolFilename) then
              begin
              volumeFilenames.Add(SDUConvertSFNToLFN(currVolFilename));
              end
            else
              begin
              volumeFilenames.Add(currVolFilename);
              end;
            end;

          end;
        end;
      currBit := currBit * 2;
      end;

    end;

  Result := retVal;

end;

// !! WARNING !!
// Under NT, the TrueCrypt driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFETrueCrypt.GetVolFileForDrive(driveLetter: char): string;
var
  mountedFilenames: TStringList;
  mountedDrives: string;
begin
  Result := '';

  driveLetter := upcase(driveLetter);

  // Use GetDisksMounted(...) to get the filenames
  mountedFilenames:= TStringList.Create();
  try
    GetDisksMounted(mountedDrives, mountedFilenames);

    if Pos(driveLetter, mountedDrives)>0 then
      begin
      Result := mountedFilenames[Pos(driveLetter, mountedDrives)-1];
      end;

  finally
    mountedFilenames.Free();
  end;

end;


// !! WARNING !!
// If running under NT, then if the lanegth of "volumeFilename" is >64, this
// function may FAIL as the TrueCrypt driver only gives us the first 60 chars
// followed by "..." to work with
function TOTFETrueCrypt.GetDriveForVolFile(volumeFilename: string): char;
var
  mountedFilenames: TStringList;
  mountedDrives: string;
  numChars: integer;
begin
  Result := #0;

  if not(IsFilePartition(volumeFilename)) then
    begin
    volumeFilename := SDUConvertSFNToLFN(volumeFilename);
    end;

  if (CurrentOS=TrueCrypt_OS_WIN_NT) AND
     (length(volumeFilename)>63) then
    begin
    numChars := 60;
    if not(IsFilePartition(volumeFilename)) then
      begin
      dec(numChars, 4);
      end;
    volumeFilename := Copy(volumeFilename, 1, numChars);
    volumeFilename := volumeFilename + '...';
    end;

  mountedFilenames:= TStringList.Create();
  try
    GetDisksMounted(mountedDrives, mountedFilenames);

    if mountedFilenames.IndexOf(volumeFilename)>-1 then
      begin
      Result := mountedDrives[mountedFilenames.IndexOf(volumeFilename)+1];
      end;

  finally
    mountedFilenames.Free();
  end;
  
end;



function TOTFETrueCrypt.GetDriveInfo(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
begin
  if (Version() < $300) then
    begin
    Result := GetDriveInfo_PRE30(driveLetter, info);
    end
  else if (Version() >= $300) and (Version() < $31A) then
    begin
    Result := GetDriveInfo_30(driveLetter, info);
    end
  else
    begin
    Result := GetDriveInfo_31a(driveLetter, info);
    end;

end;


// Function to be used for TrueCrypt versions prior to v3.0
function TOTFETrueCrypt.GetDriveInfo_PRE30(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
var
  dwBytesReturned : DWORD;
  query: TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_PRE30;
  retVal: boolean;
  sysTime: SYSTEMTIME;
  hi: TrueCrypt_PKCS5_TYPE;
begin
  retVal := FALSE;

  CheckActive();
  driveLetter := upcase(driveLetter);

  query.driveNo := ord(driveLetter)-ord('A');

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_VOLUME_PROPERTIES,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwBytesReturned,
                     nil) then
    begin
    ShortArrayToString(query.wszVolume, info.volumeFilename);

    // If we're running under NT, and it's a file that's mounted, we need
    // to strip off the "/??/" at the start of the filename returned
    if (pos('\??\', info.volumeFilename)=1) then
      begin
      delete(info.volumeFilename, 1, 4);
      end;

    info.mountedAs:= driveLetter;
    info.diskLength:= query.diskLength;

    // Note that we start from the end and work backwards; that way if the
    // cypher is unsupported, it gets reported as "Unknown"
    GetCyphersForTrueCryptCypherID(query.cipher, info.ciphers, info.cipherMode);
    info.cipherNames := GetCipherNames(info.ciphers);
    info.cipherModeName := TrueCrypt_CIPHER_MODE_NAMES[info.cipherMode];

    info.pkcs5Type := pkcs5Unknown;
    for hi:=low(TrueCrypt_PKCS5_IDS) to high(TrueCrypt_PKCS5_IDS) do
      begin
      if TrueCrypt_PKCS5_IDS[hi] = query.pkcs5 then
        begin
        info.pkcs5Type := hi;
        break;
        end;

      end;
    info.pkcs5Iterations:= query.pkcs5Iterations;
    info.pkcs5TypeName := TrueCrypt_PKCS5_NAMES[info.pkcs5Type];


    info.volumeLocation:= IdentifyVolumeFileType(info.volumeFilename);
    info.volumeLocationName := TrueCrypt_VOLUME_FILE_TYPE_NAMES[info.volumeLocation];


    FileTimeToSystemTime(FILETIME(query.volumeCreationTime), sysTime);
    info.volumeCreated := SystemTimeToDateTime(sysTime);

    FileTimeToSystemTime(FILETIME(query.headerCreationTime), sysTime);
    info.passwordChanged := SystemTimeToDateTime(sysTime);

    info.readOnly:= IsDriveReadonly(driveLetter);

    info.hidden := FALSE;

    retVal := TRUE;
    end;


  Result := retVal;

end;


// Function to be used for TrueCrypt versions v3.0-v3.1
function TOTFETrueCrypt.GetDriveInfo_30(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
var
  dwBytesReturned : DWORD;
  query: TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_30;
  retVal: boolean;
  sysTime: SYSTEMTIME;
  hi: TrueCrypt_PKCS5_TYPE;
begin
  retVal := FALSE;

  CheckActive();
  driveLetter := upcase(driveLetter);

  query.driveNo := ord(driveLetter)-ord('A');

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_VOLUME_PROPERTIES,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwBytesReturned,
                     nil) then
    begin
    ShortArrayToString(query.wszVolume, info.volumeFilename);

    // If we're running under NT, and it's a file that's mounted, we need
    // to strip off the "/??/" at the start of the filename returned
    if (pos('\??\', info.volumeFilename)=1) then
      begin
      delete(info.volumeFilename, 1, 4);
      end;

    info.mountedAs:= driveLetter;
    info.diskLength:= query.diskLength;

    // Note that we start from the end and work backwards; that way if the
    // cypher is unsupported, it gets reported as "Unknown"
    GetCyphersForTrueCryptCypherID(query.ea, info.ciphers, info.cipherMode);
    info.cipherNames := GetCipherNames(info.ciphers);
    info.cipherModeName := TrueCrypt_CIPHER_MODE_NAMES[info.cipherMode];

    // Not sure why this is needed, but that's what TrueCrypt appears to have...
    if (
         (info.ciphers[0] = cphrTripleDES) and (info.ciphers[0] = cphrNone) and
         (info.cipherMode = cphrmodeCBC)
       ) then
      begin
      info.cipherMode := cphrmodeOuterCBC;
      end;

    info.pkcs5Type := pkcs5Unknown;
    for hi:=low(TrueCrypt_PKCS5_IDS) to high(TrueCrypt_PKCS5_IDS) do
      begin
      if TrueCrypt_PKCS5_IDS[hi] = query.pkcs5 then
        begin
        info.pkcs5Type := hi;
        break;
        end;

      end;
    info.pkcs5Iterations:= query.pkcs5Iterations;
    info.pkcs5TypeName := TrueCrypt_PKCS5_NAMES[info.pkcs5Type];


    info.volumeLocation:= IdentifyVolumeFileType(info.volumeFilename);
    info.volumeLocationName := TrueCrypt_VOLUME_FILE_TYPE_NAMES[info.volumeLocation];


    FileTimeToSystemTime(FILETIME(query.volumeCreationTime), sysTime);
    info.volumeCreated := SystemTimeToDateTime(sysTime);

    FileTimeToSystemTime(FILETIME(query.headerCreationTime), sysTime);
    info.passwordChanged := SystemTimeToDateTime(sysTime);

    info.readOnly:= IsDriveReadonly(driveLetter);

    info.hidden := query.hiddenVolume;

    retVal := TRUE;
    end;


  Result := retVal;

end;


// Function to be used for TrueCrypt versions v3.1a and later
function TOTFETrueCrypt.GetDriveInfo_31a(driveLetter: char; info: pTOTFETrueCryptVolumeInfo): boolean;
var
  dwBytesReturned : DWORD;
  query: TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_31a;
  retVal: boolean;
  sysTime: SYSTEMTIME;
  hi: TrueCrypt_PKCS5_TYPE;
begin
  retVal := FALSE;

  CheckActive();
  driveLetter := upcase(driveLetter);

  query.driveNo := ord(driveLetter)-ord('A');

  if DeviceIoControl(hTrueCryptVxD,
                     TrueCrypt_IOCTL_VOLUME_PROPERTIES,
                     @query,
                     sizeof(query),
                     @query,
                     sizeof(query),
                     dwBytesReturned,
                     nil) then
    begin
    ShortArrayToString(query.wszVolume, info.volumeFilename);

    // If we're running under NT, and it's a file that's mounted, we need
    // to strip off the "/??/" at the start of the filename returned
    if (pos('\??\', info.volumeFilename)=1) then
      begin
      delete(info.volumeFilename, 1, 4);
      end;

    info.mountedAs:= driveLetter;
    info.diskLength:= query.diskLength;

    // Note that we start from the end and work backwards; that way if the
    // cypher is unsupported, it gets reported as "Unknown"
    GetCyphersForTrueCryptCypherID(query.ea, info.ciphers, info.cipherMode);
    info.cipherNames := GetCipherNames(info.ciphers);
    info.cipherModeName := TrueCrypt_CIPHER_MODE_NAMES[info.cipherMode];

    // Not sure why this is needed, but that's what TrueCrypt appears to have...
    if (
         (info.ciphers[0] = cphrTripleDES) and (info.ciphers[0] = cphrNone) and
         (info.cipherMode = cphrmodeCBC)
       ) then
      begin
      info.cipherMode := cphrmodeOuterCBC;
      end;

    info.pkcs5Type := pkcs5Unknown;
    for hi:=low(TrueCrypt_PKCS5_IDS) to high(TrueCrypt_PKCS5_IDS) do
      begin
      if TrueCrypt_PKCS5_IDS[hi] = query.pkcs5 then
        begin
        info.pkcs5Type := hi;
        break;
        end;

      end;
    info.pkcs5Iterations:= query.pkcs5Iterations;
    info.pkcs5TypeName := TrueCrypt_PKCS5_NAMES[info.pkcs5Type];


    info.volumeLocation:= IdentifyVolumeFileType(info.volumeFilename);
    info.volumeLocationName := TrueCrypt_VOLUME_FILE_TYPE_NAMES[info.volumeLocation];


    FileTimeToSystemTime(FILETIME(query.volumeCreationTime), sysTime);
    info.volumeCreated := SystemTimeToDateTime(sysTime);

    FileTimeToSystemTime(FILETIME(query.headerCreationTime), sysTime);
    info.passwordChanged := SystemTimeToDateTime(sysTime);

    info.readOnly:= query.readOnly;

    info.hidden := query.hiddenVolume;

    retVal := TRUE;
    end;


  Result := retVal;

end;



function TOTFETrueCrypt.GetVolumeInfo(volumeFilename: string; info: pTOTFETrueCryptVolumeInfo): boolean;
var
  retVal: boolean;
  tgtVolumeFilename: string;
  mountedDrives: string;
  mountedVolumeFilenames: TStringList;
  i: integer;
begin
  retVal := FALSE;

  // Uppercase volumeFilename, and convert to LFN if it's a file
  if IsFilePartition(volumeFilename) then
    begin
    tgtVolumeFilename := uppercase(volumeFilename);
    end
  else
    begin
    tgtVolumeFilename := uppercase(SDUConvertSFNToLFN(volumeFilename));
    if (tgtVolumeFilename = '') then
      begin
      tgtVolumeFilename := uppercase(volumeFilename);
      end;
    end;


  mountedVolumeFilenames := TStringList.Create();
  try
    // Get the filenames for all mounted volumes, and scan through until we
    // get the targetted volume - the just call GetVolumeInfo(...) with the
    // appropriate drive letter
    if GetDisksMounted(mountedDrives, mountedVolumeFilenames) then
      begin
      for i:=0 to (mountedVolumeFilenames.count-1) do
        begin
        if (uppercase(mountedVolumeFilenames[i]) = tgtVolumeFilename) then
          begin
          retVal := GetDriveInfo(mountedDrives[i+1], info);
          break;
          end;

        end;

      end;

  finally
    mountedVolumeFilenames.Free();
  end;


  Result := retVal;
end;


function TOTFETrueCrypt.IdentifyVolumeFileType(volumeFilename: string): TrueCrypt_VOLUME_FILE_TYPE;
var
  retVal: TrueCrypt_VOLUME_FILE_TYPE;
begin
  if IsFilePartition(volumeFilename) then
    begin
    retVal := vtidPartition;
    end
  else
    begin
    retVal := vtidTrueCryptFile;
    end;

  Result := retVal;
end;



function TOTFETrueCrypt.GetWordFromHeader(buffer: array of byte; var posInBuffer: integer): cardinal;
begin
  Result := (buffer[posInBuffer-1] * 256) + buffer[posInBuffer];
  inc(posInBuffer, 2);

end;


// Convert an array of cypher names into a human readable string of cypher names
function TOTFETrueCrypt.GetCipherNames(cyphers: array of TrueCrypt_CIPHER_TYPE): string;
var
  i: integer;
  retVal: string;
  allNone: boolean;
  allUnknown: boolean;
begin
  retVal := '';

  // Check to see if *all* the cyphers are still cphrNone/cphrUnknown
  allNone := TRUE;
  allUnknown := TRUE;
  for i:=low(cyphers) to high(cyphers) do
    begin
    if (cyphers[i] <> cphrNone) then
      begin
      // At least *one* of the cyphers was not cphrNone
      allNone := FALSE;
      end;

    if (cyphers[i] <> cphrUnknown) then
      begin
      // At least *one* of the cyphers was not cphrUnknown
      allUnknown := FALSE;
      end;

    end;


  if allNone then
    begin
    // Special case: All cyphers are unused
    retVal := TrueCrypt_CIPHER_NAMES[cphrNone];
    end
  else if allUnknown then
    begin
    // Special case: All cyphers are unknown
    retVal := TrueCrypt_CIPHER_NAMES[cphrUnknown];
    end
  else
    begin
    // Spin through the list of cyphers, converting to human readable names
    // Now *THIS* is counter-intuitive. TrueCrypt's structures store the
    // cyphers in the *REVERSE* order to that which they should be displayed
    // in?! Yet more TrueCrypt freakyness...
    for i:=high(cyphers) downto low(cyphers) do
      begin
      // Note: Skip unused cypher elements
      if (cyphers[i] <> cphrNone) then
        begin
        if (retVal <> '') then
          begin
          retVal := retVal + '-';
          end;

        retVal := retVal + TrueCrypt_CIPHER_NAMES[cyphers[i]];
        end;
        
      end;

    end;

  Result := retVal;
end;


function TOTFETrueCrypt.ClearPasswords(driveLetter: char): boolean;
var
  dwResult: DWORD;
begin
  Result := DeviceIoControl(hTrueCryptVxD,
                            TrueCrypt_IOCTL_WIPE_CACHE,
                            nil,
                            0,
                            nil,
                            0,
                            dwResult,
                            nil);
end;


function TOTFETrueCrypt.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  CheckActive();

  Result := TRUE;

end;


function TOTFETrueCrypt.Title(): string;
begin
  Result := 'TrueCrypt';
end;


function TOTFETrueCrypt.Version(): cardinal;
var
  driverVers: TOTFETrueCrypt_VERSION;
  dwResult: DWORD;
begin
  CheckActive();

  if FTrueCryptDriverVersion=$FFFFFFFF then
    begin
    if DeviceIoControl(hTrueCryptVxD,
                       TrueCrypt_IOCTL_DRIVER_VERSION,
                       @driverVers,
                       sizeof(driverVers),
                       @driverVers,
                       sizeof(driverVers),
                       dwResult,
                       nil) then
      begin
      FTrueCryptDriverVersion := driverVers.version;
      end
    else
      begin
      FTrueCryptDriverVersion:=$FFFFFFFF;
      end;

    end;

  Result := FTrueCryptDriverVersion;

end;

function TOTFETrueCrypt.CloseSlot(driveNum: integer; brutal: boolean): boolean;
begin
  if (ld(driveNum, 1)) then
    begin
    Result := FALSE;
    FLastErrCode := OTFE_ERR_FILES_OPEN;
    end
  else
    begin
    Result := DoDeviceClose(driveNum);
    ld(driveNum, 0);
    end;

end;


function TOTFETrueCrypt.ld(driveNum: integer; mode: integer): boolean;
var
  a: boolean;
  drivelett: integer;
begin
  a:= TRUE;

  drivelett := driveNum + 1;

  if ((drivelett > 1) AND (drivelett<27)) then
    begin
    a := (locklogdrive(drivelett, mode));
    end;

  Result := a;

end;

// Returns TRUE if error
function TOTFETrueCrypt.locklogdrive(drivenum: integer; mode: integer): boolean;
var
  a: integer;
begin
  if (mode<>0) then
    begin
    ioctllock(drivenum, 0, 1);
    a := ioctllock(drivenum, 4, 1);
    end
  else
    begin
    a := 0;
    ioctllock(drivenum, 0, 0);
    ioctllock(drivenum, 0, 0);
    end;

  Result := (a<>0);

end;


function TOTFETrueCrypt.ioctllock(nDrive: cardinal; permissions: integer; func: integer): integer;
var
  hDevice: THANDLE;
  reg: TDeviceIOControlRegisters;
  cb: DWORD;
  lockfunc: integer;
begin
  if (func<>0) then
    begin
    lockfunc := $4a;
    end
  else
    begin
    lockfunc := $6a;
    end;

  hDevice := CreateFile('\\.\vwin32',
                        0, 0, nil, 0, FILE_FLAG_DELETE_ON_CLOSE, 0);

  reg.reg_EAX := $440D;
  reg.reg_EBX := nDrive;
  reg.reg_ECX := $0800 OR lockfunc;
  reg.reg_EDX := permissions;
  reg.reg_Flags := $0001;

  DeviceIoControl(hDevice,
                  VWIN32_DIOC_DOS_IOCTL,
                  @reg, sizeof(reg),
                  @reg, sizeof(reg),
                  cb, nil);

  CloseHandle (hDevice);

  Result := (reg.reg_Flags AND 1); // error if carry flag is set

end;


function TOTFETrueCrypt.DoDeviceClose(driveNum: integer): boolean;
var
  mount_list: TOTFETrueCrypt_MOUNT_LIST_N_STRUCT;
  unmount: TOTFETrueCrypt_UNMOUNT_STRUCT_PRE30;
  tries: integer;
  c: integer;
  dwBytes: DWORD;
  volFilename: string;
begin
  mount_list.nDosDriveNo := driveNum;
  if (DeviceIoControl(hTrueCryptVxD, TrueCrypt_IOCTL_MOUNT_LIST_N, @mount_list, sizeof(mount_list), nil, 0, dwBytes, nil) = FALSE) then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    Result := FALSE;
    exit;
    end
  else
    begin
    if (mount_list.nReturnCode<>0) then
      begin
      FLastErrCode := OTFE_ERR_SUCCESS;
      Result := TRUE;
      exit;
      end;
    end;

  if (mount_list.mountfilehandle<>0) then
    begin
    ShortArrayToString(mount_list.wszVolume, volFilename);
    EjectStop(upcase(volFilename[1]), FALSE);
    end;

  unmount.nDosDriveNo := driveNum;
  if (DeviceIoControl (hTrueCryptVxD, TrueCrypt_IOCTL_UNMOUNT_PENDING, @unmount, sizeof(unmount), nil, 0, dwBytes, nil) = FALSE) then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    Result := FALSE;
    exit;
    end
  else
    begin
    if (mount_list.nReturnCode<>0) then
      begin
      FLastErrCode := OTFE_ERR_SUCCESS;
      Result := TRUE;
      exit;
      end;
    end;

  for c:=0 to 19 do
    begin
    DeviceIoControl(hTrueCryptVxD, TrueCrypt_IOCTL_RELEASE_TIME_SLICE, nil, 0, nil, 0, dwBytes, nil);
    end;

  for tries:=0 to 31 do
    begin
    DeviceIoControl (hTrueCryptVxd, TrueCrypt_IOCTL_RELEASE_TIME_SLICE, nil, 0, nil, 0, dwBytes, nil);
    if (DeviceIoControl (hTrueCryptVxd, TrueCrypt_IOCTL_UNMOUNT, @unmount, sizeof(UNMOUNT), nil, 0, dwBytes, nil) = FALSE) then
      begin
      FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
      Result := FALSE;
      exit;
      end
    else
      begin
      if (mount_list.nReturnCode=0) then
        begin
        FLastErrCode := OTFE_ERR_SUCCESS;
        Result := TRUE;
        exit;
        end;

      end;
    end;

  FLastErrCode := OTFE_ERR_SUCCESS;
  Result := TRUE;

end;

function TOTFETrueCrypt.EjectStop(driveLetter: char; func: boolean): boolean;
var
  hDevice: THANDLE;
  reg: TDeviceIOControlRegisters;
  cb: DWORD;
  lockfunc: integer;
  p: TPARAMBLOCK;
  driveNum: integer;
begin
  if (driveletter = #0) then
    begin
    Result := FALSE;
    exit;
    end;

  driveNum := ord('A') - ord(driveLetter) + 1;

  lockfunc := $48;

  if (func = TRUE) then
    begin
    p.Operation := 0; // lock
    end
  else
    begin
    p.Operation := 1;
    end;

  hDevice := CreateFile('\\.\vwin32', 0, 0, nil, 0,
                        FILE_FLAG_DELETE_ON_CLOSE, 0);

  reg.reg_EAX := $440D;
  reg.reg_EBX := driveNum;
  reg.reg_ECX := $0800 OR lockfunc;
  reg.reg_EDX := Cardinal(@p); // xxx - Not sure if this will work OK?
  reg.reg_Flags := $0001;

  DeviceIoControl(hDevice,
                  VWIN32_DIOC_DOS_IOCTL,
                  @reg, sizeof(reg),
                  @reg, sizeof(reg),
                  cb,
                  nil);

  CloseHandle (hDevice);

  Result := (reg.reg_Flags AND 1)>0; // error if carry flag is set */

end;



{
procedure TOTFETrueCrypt.AddVolumeToHistory(volumeFilename: string);
var
  i: integer;
  key: string;
  iniFile: TIniFile;
  lastVol: string;
  historyMRU: TStringList;
begin
  if SaveHistory then
    begin
    iniFile := TIniFile.Create(TrueCrypt_INI_FILE);
    try
      historyMRU:= TStringList.Create();
      try
        for i:=1 to TrueCrypt_SIZEOF_MRU_LIST do
          begin
          key := TrueCrypt_INI_KEY_LASTVOLUMEn + inttostr(i);
          historyMRU.Add(iniFile.ReadString(TrueCrypt_INI_SECTION_LASTRUN, key, ''));
          end;

        // if volumeFilename does not exist in the list, add it. If it does
        // exist, move it to the head of the list.
        if historyMRU.IndexOf(volumeFilename)<0 then
          begin
          historyMRU.Add(lastVol);
          end
        else
          begin
          historyMRU.Delete(historyMRU.IndexOf(volumeFilename));
          historyMRU.Insert(0, volumeFilename);
          end;

        for i:=1 to TrueCrypt_SIZEOF_MRU_LIST do
          begin
          key := TrueCrypt_INI_KEY_LASTVOLUMEn + inttostr(i);
          iniFile.WriteString(TrueCrypt_INI_SECTION_LASTRUN, key, historyMRU[i-1]);
          end;

      finally
        historyMRU.Free();
      end;

    finally
      iniFile.Free();
    end;

    end;

end;
}


function TOTFETrueCrypt.GetNextUnusedDrvLtr(afterDrv: char): char;
var
  finished: boolean;
  unusedDrvs: string;
begin
  Result := #0;
  finished := FALSE;

  unusedDrvs := GetUnusedDriveLetters();
  while not(finished) do
    begin
    afterDrv := chr(ord(afterDrv)+1);
    if ord(afterDrv)>ord('Z') then
      begin
      finished := TRUE;
      end
    else
      begin
      if pos(afterDrv, unusedDrvs)>0 then
        begin
        Result := afterDrv;
        finished := TRUE;
        end;
      end;

    end;

end;

// Returns a string containing the drive letters of "unallocated" drives
function TOTFETrueCrypt.GetUnusedDriveLetters(): string;
var
  driveLetters: string;
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  driveLetters := '';

  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
    begin
    if not(DriveNum in DriveBits) then
      begin
      driveLetters := driveLetters + Char(DriveNum + Ord('A'));
      end;
    end;

  Result := driveLetters;

end;

function TOTFETrueCrypt.VersionStr(): string;
var
  verNo: cardinal;
  majorVer : integer;
  minorVer1: integer;
  minorVer2: integer;
  retVal: string;
begin
  verNo:= Version();
  majorVer  := (verNo AND $FF00) div $FF;
  minorVer1 := (verNo AND $F0) div $F;
  minorVer2 := (verNo AND $F);


  retVal := Format('v%d.%d', [majorVer, minorVer1]);

  // Check for version v2.1 and v2.1a - THEY HAVE DIFFERENT CYPHERS, BUT THE
  // DRIVER REPORTS $0210 IN BOTH CASES
  if (verNo = $0210) then
    begin
    // This is where it gets crappy... We've no idea if we're dealing with
    // v2.1 or v2.1a! And... THE CYPHER, ETC IDS CHANGED BETWEEN THESE TWO VERSIONS!!!
    if (UseVersionHint() = tcv21) then
      begin
      // No change to retVal - it's just straight v2.1
      end
    else if (UseVersionHint() = tcv21a) then
      begin
      retVal := retVal + 'a';
      end
    else
      begin
      // We know it's either v2.1 or v2.1a, but can't tell which.
      // Indicate this.
      retVal := retVal + '(???)';
      end;

    end
  else
    begin
    if (minorVer2 > 0) then
      begin
      // We subtract $0A because v. 3.1a was $031A
      retVal := retVal + char((minorVer2 - $0A) + ord('a'));
      end;

    end;

  Result := retVal;
end;


function TOTFETrueCrypt.GetMainExe(): string;
var
  registry: TRegistry;
  keyName: string;
  appPath: string;
  exeLocation: string;
  wholeString: string;
  firstItem: string;
  theRest: string;
begin
  Result := '';
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  registry := TRegistry.create();
  try
    registry.RootKey := TrueCrypt_REGISTRY_EXE_ROOT;
    keyName := TrueCrypt_REGISTRY_EXE_PATH;
    if registry.OpenKeyReadOnly(keyName) then
      begin
      appPath := registry.ReadString('');
      exeLocation := ExtractFilePath(appPath) + TrueCrypt_APP_EXE_NAME;
      if FileExists(exeLocation) then
        begin
        Result := exeLocation
        end
      else
        begin
        // The registry key may well be of the form:
        //   "C:\Program Files\TrueCrypt\TrueCrypt.exe" /v "%1"
        wholeString := appPath;

        if SDUSplitString(wholeString, firstItem, theRest, '"') then
          begin
          // The first item is just a dumn quote - ignore as "firstItem" will
          // be an empty string
          wholeString := theRest;
          if SDUSplitString(wholeString, firstItem, theRest, '"') then
            begin
            exeLocation := firstItem;
            if FileExists(exeLocation) then
              begin
              Result := exeLocation
              end;
              
            end;

          end;
        
        end;

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

function TOTFETrueCrypt.GetAvailableRemovables(dispNames: TStringList; deviceNames: TStringList): integer;
var
  cnt: integer;
  szTmp: array [0..TrueCrypt_TC_MAX_PATH-1] of char;
begin
  cnt := 0;

  if CurrentOS=TrueCrypt_OS_WIN_NT then
    begin
    if (QueryDosDevice('A:', @szTmp[0], sizeof(szTmp))<>0) then
      begin
      dispNames.Add('Floppy (A:)');
      deviceNames.Add('\Device\Floppy0');
      inc(cnt);
      end;

    if (QueryDosDevice('B:', @szTmp[0], sizeof(szTmp))<>0) then
      begin
      dispNames.Add('Floppy (B:)');
      deviceNames.Add('\Device\Floppy1');
      inc(cnt);
      end;

    end;

  Result := cnt;

end;


function TOTFETrueCrypt.GetAvailableFixedDisks(dispNames: TStringList; deviceNames: TStringList): integer;
var
  i: integer;
  n: integer;
  cnt: integer;
  strTmp: string;
begin
  cnt := 0;

  i := 0;
  // This "64" is hardcoded in the TrueCrypt application (not a const!)
  while (i<64) do
    begin
    // This "32" is hardcoded in the TrueCrypt application (not a const!)
    for n:=1 to 32 do
      begin
      strTmp := Format(TrueCrypt_HDD_PARTITION_DEVICE_NAME_FORMAT, [i, n]);

      if OpenDevice(strTmp) then
        begin
        dispNames.Add(strTmp);
        deviceNames.Add(strTmp);
        inc(cnt);
        end
      else
        begin
        if n=1 then
          begin
          i := 64;
          break;
          end;
        end;
      end;

    inc(i);
    end;

  Result := cnt;
end;


function TOTFETrueCrypt.OpenDevice(device: string): boolean;
var
  query: TOTFETrueCrypt_OPEN_TEST;
  dwResult: DWORD;
  currAllOK: boolean;
begin
  CheckActive();

  StringToShortArray(device, query.wszFileName);

  currAllOK := DeviceIoControl(hTrueCryptVxD,
                               TrueCrypt_IOCTL_OPEN_TEST,
                               @query,
                               sizeof(query),
                               @query,
                               sizeof(query),
                               dwResult,
                               nil);

  if not(currAllOK) then
    begin
    dwResult := GetLastError();
    Result := (dwResult = ERROR_SHARING_VIOLATION);
    end
  else
    begin
    if CurrentOS=TrueCrypt_OS_WIN_NT then
      begin
      Result := TRUE;
      end
    else if (query.nReturnCode = 0) then
      begin
      Result := TRUE;
      end
    else
      begin
      FLastErrCode := OTFE_ERR_VOLUME_FILE_NOT_FOUND;
      Result := FALSE;
      end;
    end;

end;


function TOTFETrueCrypt.GetAvailableRawDevices(dispNames: TStringList; deviceNames: TStringList): boolean;
var
  stlTempDisp: TStringList;
  stlTempDevice: TStringList;
begin
  CheckActive();

  dispNames.Clear();
  deviceNames.Clear();
  GetAvailableRemovables(dispNames, deviceNames);

  stlTempDisp:= TStringList.Create();
  try
    stlTempDevice:= TStringList.Create();
    try
      GetAvailableFixedDisks(stlTempDisp, stlTempDevice);
      dispNames.AddStrings(stlTempDisp);
      deviceNames.AddStrings(stlTempDevice);
    finally
      stlTempDevice.Free();
    end;
  finally
    stlTempDisp.Free();
  end;

  Result := TRUE;

end;


// This function is required under NT/2K/XP unicode is stored. Under 9x, it's
// just straight 8 bit chars 
//   Under NT to store WCHARs
//   Under 95/98 to store chars
// as a result of this, we need to extract data from these arrays with in the
// correct way, depending on the OS being used...
// i.e. This function will take an TrueCrypt array of short and convert it back into
//      a string
procedure TOTFETrueCrypt.ShortArrayToString(theArray: array of WCHAR; var theString: string);
var
  i: integer;
  hiNotLo: boolean;
  shortChar: short;
begin
  theString := '';

  if CurrentOS<>TrueCrypt_OS_WIN_NT then
    begin
    i:=0;
    hiNotLo:= TRUE;
    shortChar := (short(theArray[0]) AND $FF);
    while shortChar<>0 do
      begin
      theString := theString + chr(shortChar);

      hiNotLo := not(hiNotLo);

      shortChar := short(theArray[i]);
      if hiNotLo then
        begin
        shortChar := shortChar AND $FF;
        end
      else
        begin
        shortChar := (shortChar AND $FF00) shr 8;
        inc(i);
        end;

      end;
    end
  else
    begin
    i:=0;
    shortChar := short(theArray[0]);
    while shortChar<>0 do
      begin
      theString := theString + char(shortChar);
      inc(i);
      shortChar := short(theArray[i]);
      end;
    end;

end;


// This function is required as v2.0.1's short arrays are used:
//   Under NT to store WCHARs
//   Under 95/98 to store chars
// as a result of this, we need to fill in these arrays with correctly packed
// data, depending on the OS being used
// i.e. This function will take a string and put it into an TrueCrypt array of short
procedure TOTFETrueCrypt.StringToShortArray(theString: string; var theArray: array of WCHAR);
var
  i: integer;
  hiNotLo: boolean;
  arrPos: integer;
begin
  theString := theString + #0;

  if CurrentOS<>TrueCrypt_OS_WIN_NT then
    begin
    hiNotLo:= TRUE;
    arrPos := 0;
    theArray[arrPos] := #0;
    for i:=1 to length(theString) do
      begin
      if hiNotLo then
        begin
        theArray[arrPos] := WCHAR(theString[i]);
        end
      else
        begin
        theArray[arrPos] := WCHAR(cardinal(theArray[arrPos]) OR (ord(theString[i]) shl 8));
        inc(arrPos);
        theArray[arrPos] := #0;
        end;

      hiNotLo := not(hiNotLo);
      end;
    end
  else
    begin
    for i:=1 to length(theString) do
      begin
      theArray[i-1] := WCHAR(theString[i]);
      end;
    end;

end;


// Returns TRUE if "volumeFilename" is a partition, not a file
function TOTFETrueCrypt.IsFilePartition(volumeFilename: string): boolean;
begin
  Result := (pos('\DEVICE\', uppercase(volumeFilename))=1);

end;

procedure TOTFETrueCrypt.LoadSettings();
var
  registry: TRegistry;
  keyName: string;
  tmpDriveLetter: string;
  i: integer;
  tmpLastMountedVolume: string;
begin
  FLastErrCode:= OTFE_ERR_UNKNOWN_ERROR;

  registry := TRegistry.create();
  try
    registry.RootKey := TrueCrypt_REGISTRY_SETTINGS_ROOT;
    keyName := TrueCrypt_REGISTRY_SETTINGS_PATH;
    if registry.OpenKeyReadOnly(keyName) then
      begin
      // Options
      FCachePasswordsInDriver := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CACHEPASSWORDSINDRIVER;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_CACHEPASSWORDSINDRIVER) then
        begin
        try
          FCachePasswordsInDriver := registry.ReadBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_CACHEPASSWORDSINDRIVER);
        except
          FCachePasswordsInDriver := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CACHEPASSWORDSINDRIVER;
        end;

        end;

      FOpenExplorerWindowAfterMount := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_OPENEXPLORERWINDOWAFTERMOUNT;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_OPENEXPLORERWINDOWAFTERMOUNT) then
        begin
        try
          FOpenExplorerWindowAfterMount := registry.ReadBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_OPENEXPLORERWINDOWAFTERMOUNT);
        except
          FOpenExplorerWindowAfterMount := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_OPENEXPLORERWINDOWAFTERMOUNT;
        end;

        end;

      FCloseExplorerWindowsOnDismount := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CLOSEEXPLORERWINDOWSONDISMOUNT;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_CLOSEEXPLORERWINDOWSONDISMOUNT) then
        begin
        try
          FCloseExplorerWindowsOnDismount := registry.ReadBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_CLOSEEXPLORERWINDOWSONDISMOUNT);
        except
          FCloseExplorerWindowsOnDismount := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CLOSEEXPLORERWINDOWSONDISMOUNT;
        end;

        end;

      FSaveMountedVolumesHistory := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_SAVEMOUNTEVOLUMESHISTORY;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_SAVEMOUNTEVOLUMESHISTORY) then
        begin
        try
          FSaveMountedVolumesHistory := registry.ReadBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_SAVEMOUNTEVOLUMESHISTORY);
        except
          FSaveMountedVolumesHistory := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_SAVEMOUNTEVOLUMESHISTORY;
        end;

        end;

      FWipePasswordCacheOnExit := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_WIPEPASSWORDCACHEONEXIT;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_WIPEPASSWORDCACHEONEXIT) then
        begin
        try
          FWipePasswordCacheOnExit := registry.ReadBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_WIPEPASSWORDCACHEONEXIT);
        except
          FWipePasswordCacheOnExit := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_WIPEPASSWORDCACHEONEXIT;
        end;

        end;



      // Drive Letter
      tmpDriveLetter := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTSELECTEDDRIVE;
      if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_LASTSELECTEDDRIVE) then
        begin
        try
          tmpDriveLetter := registry.ReadString(TrueCrypt_REGISTRY_SETTINGS_PARAM_LASTSELECTEDDRIVE);
        except
          tmpDriveLetter := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTSELECTEDDRIVE;
        end;

        if (tmpDriveLetter <> '') then
          begin
          FLastSelectedDrive := tmpDriveLetter[1];
          end
        else
          begin
          FLastSelectedDrive := #0;
          end;

        end;


      // History
      for i:=0 to (TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME-1) do
        begin
        tmpLastMountedVolume := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTMOUNTEVOLUME;
        if registry.ValueExists(TrueCrypt_REGISTRY_SETTINGS_PARAM_PREFIX_LASTMOUNTEVOLUME+inttostr(i)) then
          begin
          try
            tmpLastMountedVolume := registry.ReadString(TrueCrypt_REGISTRY_SETTINGS_PARAM_PREFIX_LASTMOUNTEVOLUME+inttostr(i));
          except
            tmpLastMountedVolume := TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTMOUNTEVOLUME;
          end;

          SetLastMountedVolume(i+1, tmpLastMountedVolume);
          end;
        end;


      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;


  FLastErrCode:= OTFE_ERR_SUCCESS;

end;


procedure TOTFETrueCrypt.SaveSettings();
var
  registry: TRegistry;
  keyName: string;
  i: integer;
  tmpLastMountedVolume: string;
begin
  FLastErrCode:= OTFE_ERR_UNKNOWN_ERROR;

  registry := TRegistry.create();
  try
    registry.RootKey := TrueCrypt_REGISTRY_SETTINGS_ROOT;
    keyName := TrueCrypt_REGISTRY_SETTINGS_PATH;
    if registry.OpenKey(keyName, TRUE) then
      begin
      // Options
      registry.WriteBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_CACHEPASSWORDSINDRIVER,         FCachePasswordsInDriver);
      registry.WriteBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_OPENEXPLORERWINDOWAFTERMOUNT,   FOpenExplorerWindowAfterMount);
      registry.WriteBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_CLOSEEXPLORERWINDOWSONDISMOUNT, FCloseExplorerWindowsOnDismount);
      registry.WriteBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_SAVEMOUNTEVOLUMESHISTORY,       FSaveMountedVolumesHistory);
      registry.WriteBool(TrueCrypt_REGISTRY_SETTINGS_PARAM_WIPEPASSWORDCACHEONEXIT,        FWipePasswordCacheOnExit);


      // Drive Letter
      if ((FLastSelectedDrive = #0) OR (not(SaveMountedVolumesHistory))) then
        begin
        registry.WriteString(TrueCrypt_REGISTRY_SETTINGS_PARAM_LASTSELECTEDDRIVE, '');
        end
      else
        begin
        registry.WriteString(TrueCrypt_REGISTRY_SETTINGS_PARAM_LASTSELECTEDDRIVE, FLastSelectedDrive+':');
        end;


      // History
      for i:=0 to (TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME-1) do
        begin
        tmpLastMountedVolume := GetLastMountedVolume(i+1);
        registry.WriteString(TrueCrypt_REGISTRY_SETTINGS_PARAM_PREFIX_LASTMOUNTEVOLUME+inttostr(i), tmpLastMountedVolume);
        end;


      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;


  FLastErrCode:= OTFE_ERR_SUCCESS;

end;


function  TOTFETrueCrypt.SetLastMountedVolume(idx: integer; filename: string): boolean;
begin
  Result := FALSE;

  if ((idx>=1) and (idx<TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME)) then
    begin
    FLastMountedVolume[idx-1] := filename;
    Result := TRUE;
    end;

end;


// "idx" should be between 1 and TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME
// Returns '' on error
// Also returns '' if there is no filename in that position
function TOTFETrueCrypt.GetLastMountedVolume(idx: integer): string;
begin
  Result := '';

  if ((idx>=1) and (idx<TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME)) then
    begin
    Result := FLastMountedVolume[idx-1];
    end;

end;


// Note: Calling AddVolumeToHistory will push the oldest filename on the MRU
// list off
procedure TOTFETrueCrypt.AddLastMountedVolume(volumeFilename: string);
var
  i: integer;
begin
  for i:=1 to TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME do
    begin
    if (GetLastMountedVolume(i)<>'') then
      begin
      SetLastMountedVolume(i, volumeFilename);
      end;

    end;

end;


// Function to convert TrueCrypt cypher ID into the Delphi cypher type/types
// This function is required because TrueCrypt is *FOUL* and different versions
// of the TrueCrypt driver use different IDs - and sometimes the IDs change,
// but the driver version doens't?!!! (e.g. v2.1->v2.1a)
// YUCK! DISGUSTING! :(
// Returns: TRUE/FALSE, depending on whether the cypher ID was recognised
//          If TRUE, the FIRST THREE ELEMENTS OF "cyphers" will be populated
//          with the relevant cyphers; unused elements will be populated
//          with cphrNone
function TOTFETrueCrypt.GetCyphersForTrueCryptCypherID(cypherID: integer; var cyphers: array of TrueCrypt_CIPHER_TYPE; var cypherMode: TrueCrypt_CIPHER_MODE): boolean;
var
  i: integer;
  allOK: boolean;
  allUnknown: boolean;
  verNo: cardinal;
begin
  allOK := FALSE;
  for i:=low(cyphers) to high(cyphers) do
    begin
    cyphers[i] := cphrUnknown;
    end;

  verNo := Version();

  // OK, now *THIS* is *puke* - *every* different TrueCrypt versions use
  // different IDs, so we're stuck with doing this :(
  // Couldn't they just use nice enums in their code, as opposed to #defining
  // everything?!! Ewww!
  // WHY ON EARTH does the "cypher chaining system" use PROSCRIBED CYPHER
  // COMBINATIONS?!! This is TRUELY IDIOTIC! Let the ***USER*** decide what
  // combination they want to use - much more flexible!

  if (verNo <= $0100) then
    begin
    // v1.0 driver...
    cypherMode := cphrmodeCBC;

    case cypherID of
      0:
        begin
        cyphers[0] := cphrNone;
        end;

      1:
        begin
        cyphers[0] := cphrBlowfish;
        end;

      2:
        begin
        cyphers[0] := cphrCAST;
        end;

      3:
        begin
        cyphers[0] := cphrIDEA;
        end;

      4:
        begin
        cyphers[0] := cphrTRIPLEDES;
        end;

      100:
        begin
        cyphers[0] := cphrDES56;
        end;
    end;  // case cypherID of

    end
  else if ( (verNo > $0100) and (verNo <= $0200) ) then
    begin
    // v2.0 driver...
    cypherMode := cphrmodeCBC;

    case cypherID of
      0:
        begin
        cyphers[0] := cphrNone;
        end;

      1:
        begin
        cyphers[0] := cphrBlowfish;
        end;

      2:
        begin
        cyphers[0] := cphrAES;
        end;

      3:
        begin
        cyphers[0] := cphrCAST;
        end;

      4:
        begin
        cyphers[0] := cphrIDEA;
        end;

      5:
        begin
        cyphers[0] := cphrTRIPLEDES;
        end;

      100:
        begin
        cyphers[0] := cphrDES56;
        end;
    end;  // case cypherID of

    end
  else if ( (verNo > $0200) and (verNo <= $0210) ) then
    begin
  // v2.1/v2.1a driver...
    cypherMode := cphrmodeCBC;

    // This is where it gets crappy... We've no idea if we're dealing with
    // v2.1 or v2.1a! And... THE IDS CHANGED BETWEEN THESE TWO VERSIONS!!!
    if (UseVersionHint() = tcv21) then
      begin
      // v2.1 driver...
      case cypherID of
        0:
          begin
          cyphers[0] := cphrNone;
          end;

        1:
          begin
          cyphers[0] := cphrBlowfish;
          end;

        2:
          begin
          cyphers[0] := cphrAES;
          end;

        3:
          begin
          cyphers[0] := cphrCAST;
          end;

        4:
          begin
          cyphers[0] := cphrIDEA;
          end;

        5:
          begin
          cyphers[0] := cphrTRIPLEDES;
          end;

        100:
          begin
          cyphers[0] := cphrDES56;
          end;
      end;  // case cypherID of
      end  // if statement
    else
      begin
      // v2.1a driver...
      case cypherID of
        0:
          begin
          cyphers[0] := cphrNone;
          end;

        1:
          begin
          cyphers[0] := cphrBlowfish;
          end;

        2:
          begin
          cyphers[0] := cphrAES;
          end;

        3:
          begin
          cyphers[0] := cphrCAST;
          end;

        4:
          begin
          cyphers[0] := cphrTRIPLEDES;
          end;

        100:
          begin
          cyphers[0] := cphrDES56;
          end;
      end;  // case cypherID of
      end;  // ELSE PART
    end
  else if (verNo > $0210) then  // e.g. 030A for v3.0a
    begin
  // v3.0 driver...
    case cypherID of
      0:
        begin
        cyphers[0] := cphrNone;
        cypherMode := cphrmodeCBC;
        end;

      1:
        begin
        cyphers[0] := cphrAES;
        cypherMode := cphrmodeCBC;
        end;

      2:
        begin
        cyphers[0] := cphrBlowfish;
        cypherMode := cphrmodeCBC;
        end;

      3:
        begin
        cyphers[0] := cphrCAST;
        cypherMode := cphrmodeCBC;
        end;

      4:
        begin
        cyphers[0] := cphrSerpent;
        cypherMode := cphrmodeCBC;
        end;

      5:
        begin
        cyphers[0] := cphrTRIPLEDES;
        cypherMode := cphrmodeCBC;
        end;

      6:
        begin
        cyphers[0] := cphrTwofish;
        cypherMode := cphrmodeCBC;
        end;

      7:
        begin
        cyphers[0] := cphrBlowfish;
        cyphers[1] := cphrAES;
        cypherMode := cphrmodeInnerCBC;
        end;

      8:
        begin
        cyphers[0] := cphrSerpent;
        cyphers[1] := cphrBlowfish;
        cyphers[2] := cphrAES;
        cypherMode := cphrmodeInnerCBC;
        end;

      9:
        begin
        cyphers[0] := cphrTwofish;
        cyphers[1] := cphrAES;
        cypherMode := cphrmodeOuterCBC;
        end;

      10:
        begin
        cyphers[0] := cphrSerpent;
        cyphers[1] := cphrTwofish;
        cyphers[2] := cphrAES;
        cypherMode := cphrmodeOuterCBC;
        end;

      11:
        begin
        cyphers[0] := cphrAES;
        cyphers[1] := cphrSerpent;
        cypherMode := cphrmodeOuterCBC;
        end;

      12:
        begin
        cyphers[0] := cphrAES;
        cyphers[1] := cphrTwofish;
        cyphers[2] := cphrSerpent;
        cypherMode := cphrmodeOuterCBC;
        end;

      13:
        begin
        cyphers[0] := cphrTwofish;
        cyphers[1] := cphrSerpent;
        cypherMode := cphrmodeOuterCBC;
        end;
    end;  // case cypherID of

    end;


  // Check to see if *all* the cyphers are still cphrUnknown
  allUnknown := TRUE;
  for i:=low(cyphers) to high(cyphers) do
    begin
    if (cyphers[i] <> cphrUnknown) then
      begin
      // At least *one* of the cyphers was not cphrUnknown
      allUnknown := FALSE;
      break;
      end;

    end;


  // Change unused cyphers from cphrUnknown to cphrNone
  if not(allUnknown) then
    begin
    allOK := TRUE;

    for i:=low(cyphers) to high(cyphers) do
      begin
      if (cyphers[i] = cphrUnknown) then
        begin
        cyphers[i] := cphrNone;
        end;

      end;

    end;


  Result := allOK;
end;


// This is a last ditch attempt to identify the version of TrueCrypt
// installed: Use the programmed hint, or failing that, check the file size of
// the TrueCrypt executable.
// Horrible hack, but it's all we can do because the driver doesn't report
// its's version ID correctly
function TOTFETrueCrypt.UseVersionHint(): TTrueCryptVersionHint;
var
  exeFilename: string;
  f: file of Byte;
  size: integer;
  i: TTrueCryptVersionHint;
  retVal: TTrueCryptVersionHint;
begin
  retVal := VersionHint;

  if (VersionHint = tcvAuto) then
    begin
    // There is no user defined hint... Time for a last ditch attempt...
    exeFilename := GetMainExe();

    if (exeFilename <> '') then
      begin
      AssignFile(f, exeFilename);
      FileMode := 0; // Reset is in readonly mode
      Reset(f);
      size := FileSize(f);
      CloseFile(f);

      // Check filesize against known executable sizes...
      for i:=low(TrueCrypt_EXE_FILESIZES) to high(TrueCrypt_EXE_FILESIZES) do
        begin
        if (TrueCrypt_EXE_FILESIZES[i] = size) then
          begin
          retVal := i;
          break;
          end;
        end;  // for i:=low(TrueCrypt_EXE_FILESIZES) to high(TrueCrypt_EXE_FILESIZES) do

      end;  // if (exeFilename <> '') then

    end;  // if (VersionHint = tcvAuto) then

  Result:= retVal;
end;



// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFETrueCrypt.MountDevices(): string;
var
  mntDeviceDlg: TOTFETrueCryptMountDevice_F;
  finished: boolean;
begin
  Result := '';

  if MountDeviceDlg9xStyle then
    begin
    mntDeviceDlg := TOTFETrueCryptMountDevice9x_F.Create(nil);
    end
  else
    begin
    mntDeviceDlg := TOTFETrueCryptMountDeviceNT_F.Create(nil);
    end;
  try
    mntDeviceDlg.TrueCryptComponent := self;
    if mntDeviceDlg.ShowModal()=mrOK then
      begin
      finished:=FALSE;
      while not(finished) do
        begin
        Result := Mount(mntDeviceDlg.PartitionDevice, FALSE);
        finished := (Result<>#0);
        if not(finished) then
          begin
          finished := (LastErrorCode<>OTFE_ERR_WRONG_PASSWORD);
          if not(finished) then
            begin
            MessageDlg('Wrong passsword entered; try again', mtError, [mbOK], 0);
            end;
          end;
        end;
      end;
  finally
    mntDeviceDlg.Free();
  end;

end;


// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFETrueCrypt.CanMountDevice(): boolean;
begin
  // Supported...
  Result := TRUE;
end;


// -----------------------------------------------------------------------------


END.


