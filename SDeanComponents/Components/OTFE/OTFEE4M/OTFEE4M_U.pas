unit OTFEE4M_U;
// Description: Delphi E4M Component
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
  OTFE_U, OTFEE4MStructures_U;

type
  TOTFEE4M = class(TOTFE)
  private
    hE4MVxD : THandle;
    FPasswordPrompt: string;
    FSupportScramDisk: boolean;
    FHidePasswordsEnteredCk: boolean;

    FMountDeviceDlg9xStyle: boolean;
    
    mtdVolInfoLst: TList;
    procedure AddVolumeToHistory(volumeFilename: string);

    procedure UpdateVolInfoList();
    function  GetHashType(hashID: integer): E4M_HASH_TYPE;
    function  GetCipherType(cipherID: integer): E4M_CIPHER_TYPE;
    function  ld(driveNum: integer; mode: integer): boolean;
    function  CloseSlot(driveNum: integer; brutal: boolean): boolean;

    function  locklogdrive(drivenum: integer; mode: integer): boolean;
    function  ioctllock(nDrive: cardinal; permissions: integer; func: integer): integer;
    function  DoDeviceClose(driveNum: integer): boolean;
    function  EjectStop(driveLetter: char; func: boolean): boolean;

    function  GetNextUnusedDrvLtr(afterDrv: char): char;
    function  GetUnusedDriveLetters(): string;
  protected
    E4MDriverName: string;
    E4MDriverVersion: cardinal;
    CurrentOS: integer;

    function  Connect(): boolean;
    function  Disconnect(): boolean;
    procedure SetActive(AValue : Boolean); override;
    function  GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;
    function  GetWordFromHeader(buffer: array of byte; var posInBuffer: integer): cardinal;
    function  GetCipherName(cipherID: integer): string;
    function  GetVolumeInfo(volumeFilename: string; info: pTOTFEE4MVolumeInfo): boolean;

    // Property settings (read/written to INI file)
    function  GetCachePasswordInDriver(): boolean;
    procedure SetCachePasswordInDriver(cache: boolean);
    function  GetSaveHistory(): boolean;
    procedure SetSaveHistory(save: boolean);
    function  GetMountWarning(): boolean;
    procedure SetMountWarning(mountWarning: boolean);
    function  GetDefaultDrive(): char;
    procedure SetDefaultDrive(dfltDrive: char);
    procedure SetSupportScramDisk(support: boolean);

    function  IsE4MorSFSEncryptedVolFile(volumeFilename: string): boolean; overload;
    function  IsE4MorSFSEncryptedVolFile(volumeFilename: string; var cancelled: boolean): boolean; overload;
    function  GetScramDiskPasswords(dlgTitle: string; var passwordDigest: string; var driveLetter: char): boolean;

    function  GetAvailableRemovables(dispNames: TStringList; deviceNames: TStringList): integer;
    function  GetAvailableFixedDisks(dispNames: TStringList; deviceNames: TStringList): integer;
    function  OpenDevice(device: string): boolean;

    procedure ShortArrayToString(theArray: array of short; var theString: string);
    procedure StringToShortArray(theString: string; var theArray: array of short);

    function  IsFilePartition(volumeFilename: string): boolean;
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
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  DrivesMounted(): string; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): Ansichar; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetMainExe(): string; override;


    function  GetDriveInfo(driveLetter: char): TOTFEE4MVolumeInfo;
    function  VersionCanSupportScramDiskVols(): boolean;
    function  ClearPasswords(driveLetter: char): boolean;
    function  GetAvailableRawDevices(dispNames: TStringList; deviceNames: TStringList): boolean;

  published
    property  SupportScramDisk: Boolean read FSupportScramDisk write SetSupportScramDisk default FALSE;
    property  PasswordPrompt: string read FPasswordPrompt write FPasswordPrompt;
    property  SaveHistory: boolean read GetSaveHistory write SetSaveHistory default TRUE;
    property  CachePasswordInDriver: boolean read GetCachePasswordInDriver write SetCachePasswordInDriver default FALSE;
    property  MountWarning: boolean read GetMountWarning write SetMountWarning default FALSE;
    property  DefaultDrive: char read GetDefaultDrive write SetDefaultDrive;
    property  HidePasswordsEnteredCk: boolean read FHidePasswordsEnteredCk write FHidePasswordsEnteredCk default TRUE;

    property  MountDeviceDlg9xStyle: boolean read FMountDeviceDlg9xStyle write FMountDeviceDlg9xStyle default FALSE;
  end;

procedure Register;


implementation

uses
  ShellAPI,
  dialogs,
  Registry, RegStr,
  INIFiles,
  Math,
  OTFEConsts_U,
  OTFEE4MPasswordEntry_U,
  SDUGeneral,
  HashValue_U,
  HashAlg_U, // required for the SHA1 function
  HashAlgSHA1_U, // required for the SHA1 function
  OTFEE4MScramDiskPasswordEntry_U, // Part of the ScramDisk component; the ScramDisk password entry dialog
  OTFEE4MMountDevice_U,
  OTFEE4MMountDevice9x_U,
  OTFEE4MMountDeviceNT_U;


procedure Register;
begin
  RegisterComponents('OTFE', [TOTFEE4M]);
end;



constructor TOTFEE4M.Create(AOwner : TComponent);
var
  os: OSVERSIONINFO;
begin
  inherited create(AOwner);

  // Pull down the windows version
  os.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
  if (GetVersionEx(os) = FALSE) then
    begin
    raise EE4MError.Create('Unable to determine OS');
    end
  else if (os.dwPlatformId = VER_PLATFORM_WIN32_NT) then
    begin
    CurrentOS := E4M_OS_WIN_NT;
    end
  else if ((os.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and (os.dwMajorVersion = 4) and (os.dwMinorVersion = 0)) then
    begin
    CurrentOS := E4M_OS_WIN_95;
    end
  else if ((os.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and (os.dwMajorVersion = 4) and (os.dwMinorVersion = 10)) then
    begin
    CurrentOS := E4M_OS_WIN_98;
    end
  else
    begin
    // raise EE4MError('Unable to determine OS');
    CurrentOS := E4M_OS_WIN_98;
    end;


  FActive := False;
  E4MDriverVersion := $FFFFFFFF;

  mtdVolInfoLst := TList.Create();

  FPasswordPrompt := 'Enter password for %s';

end;


destructor TOTFEE4M.Destroy;
var
  i: integer;
begin
  for i:=0 to (mtdVolInfoLst.count-1) do
    begin
    dispose(mtdVolInfoLst.items[i]);
    end;
  mtdVolInfoLst.Free();

  if FActive then
    CloseHandle(hE4MVxD);
  inherited Destroy;
end;


function TOTFEE4M.Connect(): boolean;
var
  OSVersion: TOSVersionInfo;
  dwResult: DWORD;
begin
  Result := Active;
  if not(Active) then
    begin
    hE4MVxD := INVALID_HANDLE_VALUE;
    if CurrentOS = E4M_OS_WIN_NT then
      begin
      // See if v2.10 is installed
      E4MDriverName := E4M_WIN32_ROOT_PREFIX;
      hE4MVxD := CreateFile(PChar(E4MDriverName),
                                  0,
                                  0,
                                  nil,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL,
                                  0);
      if hE4MVxD=INVALID_HANDLE_VALUE then
        begin
        // v2.10 isn't installed, fallback to v2.00
        E4MDriverName := E4M_WIN32_ROOT_PREFIX_v200;
        end;
      end
    else
      begin
      E4MDriverName := E4M_WIN9X_DRIVER_NAME;
      end;

    if hE4MVxD=INVALID_HANDLE_VALUE then
      begin
      hE4MVxD := CreateFile(PChar(E4MDriverName),
                                  0,
                                  0,
                                  nil,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL,
                                  0);
      end;

    if hE4MVxD = INVALID_HANDLE_VALUE then
      begin
      raise EE4MVxdNotFound.Create('E4M device driver not found');
      end
    else
      begin
      OSVersion.dwOSVersionInfoSize := SizeOf(OSVersion);
      GetVersionEx(OSVersion);
      FActive := TRUE;
      if (Version()>=$201) AND
         (CurrentOS=E4M_OS_WIN_98) then
        begin
        // We're runninng E4M v2.0.1 under w98
        DeviceIoControl(hE4MVxD,
                        ALLOW_FAST_SHUTDOWN,
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

function TOTFEE4M.Disconnect(): boolean;
begin
  if Active then
    begin
    CloseHandle(hE4MVxD);
    end;
  Result := TRUE;

end;


procedure TOTFEE4M.SetActive(AValue : Boolean);
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
      E4MDriverVersion := Version();
      end;
    end;

end;

// Note that the emergency parameter is IGNORED
function TOTFEE4M.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  query: TOTFEE4M_UNMOUNT;
  allOK: boolean;
  inbuf: array [1..80] of char;
  outbuf: array [1..80] of char;
  bytesRead: DWORD;
  serviceCmd: string;
  i: integer;
begin
  CheckActive();

  Result := FALSE;

  if pos(driveLetter, DrivesMounted())<1 then
    begin
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    exit;
    end;

  query.nDosDriveNo := ord(upcase(driveLetter))-ord('A');
  query.nReturnCode := 0;

  if CurrentOS = E4M_OS_WIN_NT then
    begin
    // Unmount the volume using the e4mservice, this is done to allow
    // non-administrators to unmount volumes
    // Doing it with the DeviceIOControl DISMOUNT is a bad idea as we would need
    // to lock the volume first, which non-administrators can't do
    serviceCmd := 'unmount '+inttostr(ord(upcase(driveLetter))-ord('A'));
    for i:=1 to length(serviceCmd) do
      begin
      outbuf[i] := serviceCmd[i];
      end;
    outbuf[length(serviceCmd)+1] := #0;

    allOK := CallNamedPipe(E4M_PIPE_SERVICE,
                            @outbuf,
                            sizeof(outbuf),
                            @inbuf,
                            sizeof(inbuf),
                            bytesRead,
                            NMPWAIT_WAIT_FOREVER);
    if allOK then
      begin
      Result := (inbuf[1]<>'-');
      if not(Result) then
        begin
        if pos(inttostr(E4M_ERR_FILES_OPEN_LOCK), inbuf)>0 then
          begin
          FLastErrCode := OTFE_ERR_FILES_OPEN;
          end;
        end;
      end;
    end // if CurrentOS = E4M_OS_WIN_NT then
  else
    begin
    Result := CloseSlot(query.nDosDriveNo, emergency);
    end; // if not(if CurrentOS = E4M_OS_WIN_NT) then

end;

function TOTFEE4M.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  // Don't need to convert volumeFilename to SFN/LFN, this is done in
  // GetDriveForVolFile(...)
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


function TOTFEE4M.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
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


function TOTFEE4M.Mount(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
var
  query200: TOTFEE4M_200_MOUNT;
  query201: TOTFEE4M_201_MOUNT;
  dwBytesReturned: DWORD;
  volumeLoop: integer;
  i: integer;
  driveLetter: ansichar;
  thePassword: AnsiString;
  oneOK: boolean;
  targetPath: string;
  dosName: string;
  volInfo: pTOTFEE4MVolumeInfo;
  passwordDlg: TOTFEE4MPasswordEntry_F;

  currAllOK: boolean;
  mountedDrvLetter: char;
  currVolFilename: string;
  drvLtrsFree: string;
  pwEntryDlgTitle: string;
  currFileValid: boolean;
  isPartition: boolean;
begin
  CheckActive();
  Result := FALSE;

  oneOK := FALSE;
  mountedAs := '';

  pwEntryDlgTitle := Format(FPasswordPrompt, [volumeFilenames[0]]);

  // Get the password/starting drive
  if IsE4MorSFSEncryptedVolFile(volumeFilenames[0]) then
    begin
    query200.bSD := FALSE; // v2.00 only

    passwordDlg:= TOTFEE4MPasswordEntry_F.Create(nil);
    try
      passwordDlg.ckHidePassword.visible := not(FHidePasswordsEnteredCk);

      // The user should be prompted for the drive letter, and offered the
      // default
      drvLtrsFree := GetUnusedDriveLetters();
      driveLetter := DefaultDrive;

      if driveLetter=#0 then
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
      else if pos(driveLetter, drvLtrsFree)<1 then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
        
      passwordDlg.DrivesAllowed := drvLtrsFree;
      passwordDlg.Drive := driveLetter;

      passwordDlg.caption := pwEntryDlgTitle;

      if passwordDlg.ShowModal()=mrCancel then
        begin
        passwordDlg.ClearEnteredPassword();
        // Result already = FALSE, so just set the error and exit
        FLastErrCode := OTFE_ERR_USER_CANCEL;
        exit;
        end;
      thePassword := passwordDlg.mePassword.text;
      passwordDlg.ClearEnteredPassword();
      driveLetter := passwordDlg.Drive;
    finally
      passwordDlg.Free();
    end;
    end
  else
    begin
    // It's a ScramDisk volume
    if (SupportScramDisk) AND
       (VersionCanSupportScramDiskVols()) then
      begin
      query200.bSD := TRUE; // v2.00 only
      // Get the 4 passwords SHA1'd
      GetScramDiskPasswords(pwEntryDlgTitle, thePassword, driveLetter);
      end
    else
      begin
      // driveLetter already = FALSE, so just set the error and exit
      FLastErrCode:= OTFE_ERR_FILE_NOT_ENCRYPTED_VOLUME;
      Result := FALSE;
      exit;
      end;
    end;

  if driveLetter=#0 then
    begin
    // Result already = FALSE, so just set the error and exit
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    Result := FALSE;
    exit;
    end;

  DefaultDrive := driveLetter;

  for i:=1 to length(thePassword) do
    begin
    query200.szPassword[i] := thePassword[i];
    query201.szPassword[i] := thePassword[i];
    thePassword[i] := char(Random(256));
    end;
  query200.szPassword[length(thePassword)+1] := #0;
  query201.szPassword[length(thePassword)+1] := #0;

  query200.nPasswordLen := length(thePassword);
  query201.nPasswordLen := length(thePassword);

  query200.bCache := CachePasswordInDriver; // looked up from the INI file
  query201.bCache := CachePasswordInDriver; // looked up from the INI file

  // NOTE: THESE VALUES SHOULD *NOT* NEED TO BE SET TO ANYTHING
  // xxx - is this really needed?
  query200.dummy1[1] := $00;
  query200.dummy1[2] := $8a;
  query200.dummy1[3] := $01;

  query200.dummy1[1] := $00;
  query200.dummy1[2] := $00;
  query200.dummy1[3] := $00;
// v2.0.1 doesn't seem to need these...
//  query201.dummy1[1] := $00;
//  query201.dummy1[2] := $8a;
//  query201.dummy1[3] := $01;

  query200.dummy2[1] := $00;
  query200.dummy2[2] := $00;
  query200.dummy2[3] := $00;
  query201.dummy2[1] := $00;
  query201.dummy2[2] := $00;
  query201.dummy2[3] := $00;

  query200.dummy3[1] := $00; // v2.00 only
  query200.dummy3[2] := $00; // v2.00 only
  query200.dummy3[3] := $00; // v2.00 only

  for volumeLoop:=0 to (volumeFilenames.count-1) do
    begin
    mountedDrvLetter := #0;

    query200.nDosDriveNo := ord(upcase(driveLetter))-ord('A');
    query201.nDosDriveNo := ord(upcase(driveLetter))-ord('A');

    currVolFilename := volumeFilenames[volumeLoop];

    isPartition := IsFilePartition(currVolFilename);
    currFileValid := isPartition;
    if not(isPartition) then
      begin
      currVolFilename := SDUConvertSFNToLFN(volumeFilenames[volumeLoop]);
      currFileValid := IsEncryptedVolFile(currVolFilename);
      end;

    AddVolumeToHistory(currVolFilename);

    if currFileValid OR isPartition then
      begin
      // We must get info on the volume *before* we mount it, just in case it
      // mounts correctly and we need to store this information
      new(volInfo);
      GetVolumeInfo(currVolFilename, volInfo);

      for i:=1 to length(currVolFilename) do
        begin
        query200.wszVolume[i] := WCHAR(currVolFilename[i]);
        end;
      query200.wszVolume[length(currVolFilename)+1] := #0;

      StringToShortArray(currVolFilename, query201.wszVolume);

      query201.time := DateTimeToFileDate(now);
      query201.time := DateTimeToFileDate(now);

      query200.nReturnCode := 0;
      query201.nReturnCode := 0;

      if E4MDriverVersion=$200 then
        begin
        currAllOK := DeviceIoControl(hE4MVxD,
                                     E4M_MOUNT,
                                     @query200,
                                     sizeof(query200),
                                     @query200,
                                     sizeof(query200),
                                     dwBytesReturned,
                                     nil);

        currAllOK := currAllOK AND (query200.nReturnCode=0);
        end
      else
        begin
        currAllOK := DeviceIoControl(hE4MVxD,
                                     E4M_MOUNT,
                                     @query201,
                                     sizeof(query201),
                                     @query201,
                                     sizeof(query201),
                                     dwBytesReturned,
                                     nil);

        currAllOK := currAllOK AND (query201.nReturnCode=0);
        end;

      if not(currAllOK) then
        begin
        FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
        end;

      if currAllOK AND (CurrentOS = E4M_OS_WIN_NT) then
        begin
        dosName := driveLetter + ':';
        if E4MDriverVersion=$200 then
          begin
          targetPath := E4M_NT_MOUNT_PREFIX_v200;
          end
        else
          begin
           targetPath := E4M_NT_MOUNT_PREFIX;
          end;

        targetPath := targetPath+driveLetter;
        currAllOK := DefineDosDevice(DDD_RAW_TARGET_PATH, PChar(dosName), PChar(targetPath));
        end;

      if currAllOK then
        begin
        volInfo^.mountedAs := driveLetter;
        volInfo^.readonly := IsDriveReadonly(driveLetter);
        mtdVolInfoLst.Add(volInfo);
        mountedDrvLetter := driveLetter;
        oneOK := TRUE;
        end
      else
        begin
        dispose(volInfo);
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

    mountedAs := mountedAs + mountedDrvLetter;
    end;

  // Pad out the string with #0, if needed
  while length(mountedAs)<volumeFilenames.count do
    begin
    mountedAs := mountedAs + #0;
    end;

  Result := oneOK;

end;


function TOTFEE4M.DrivesMounted(): string;
var
  output: string;
begin
  GetDisksMounted(output, nil);
  Result := SortString(output);

end;

// !! WARNING !!
// Under NT, the E4M driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFEE4M.GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;
var
  dwBytesReturned : DWORD;
  query200: TOTFEE4M_200_MOUNT_LIST;
  query201: TOTFEE4M_201_MOUNT_LIST;
  i: integer;
  j: integer;
  currBit: cardinal;
  currVolFilename: string;
begin
  CheckActive();

  // Cleared query.ulMountedDrives now so we can can use either later...
  query200.ulMountedDrives := 0;
  query201.ulMountedDrives := 0;

  if E4MDriverVersion=$200 then
    begin
    DeviceIoControl(hE4MVxD,
                    E4M_MOUNT_LIST,
                    @query200,
                    sizeof(query200),
                    @query200,
                    sizeof(query200),
                    dwBytesReturned,
                    nil);
    end
  else
    begin
    DeviceIoControl(hE4MVxD,
                    E4M_MOUNT_LIST,
                    @query201,
                    sizeof(query201),
                    @query201,
                    sizeof(query201),
                    dwBytesReturned,
                    nil);
    end;

  mountedDrives := '';
  if volumeFilenames<>nil then
    begin
    volumeFilenames.Clear();
    end;
  currBit := 1;
  for i:=1 to 26 do
    begin
    // We cleared the query.ulMountedDrives earlier, so we can do this
    if ((query200.ulMountedDrives AND currBit)>0) OR
       ((query201.ulMountedDrives AND currBit)>0) then
      begin
      mountedDrives := mountedDrives + chr(ord('A')+i-1);
      if volumeFilenames<>nil then
        begin
        currVolFilename := '';
        if E4MDriverVersion=$200 then
          begin
          j := 1;
          while query200.wszVolume[i][j]<>#0 do
            begin
            currVolFilename := currVolFilename + query200.wszVolume[i][j];
            inc(j);
            end;
          end
        else
          begin
          ShortArrayToString(query201.wszVolume[i], currVolFilename);
          end;

        if CurrentOS=E4M_OS_WIN_NT then
          begin
          // If we're running under NT, and it's a file that's mounted, we need
          // to strip off the "/??/" at the start of the filename returned
          if not(IsFilePartition(currVolFilename)) then
            begin
            delete(currVolFilename, 1, 4);
            end;
          end
        else
          begin
          // If we're running under 95/98, we need to do this to prevent problems
          // with E4M returning only the first 60 chars followed by "..." if
          // the volume filename length is >64
          currVolFilename := GetVolFileForDrive(chr(ord('A')+i-1));
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


  Result := TRUE;

end;

// !! WARNING !!
// Under NT, the E4M driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFEE4M.GetVolFileForDrive(driveLetter: char): string;
var
  mountedFilenames: TStringList;
  mountedDrives: string;
  mountListN98: TOTFEE4M_9x_MOUNT_LIST_N_STRUCT;
  dwBytes: DWORD;
  volFilename: string;
begin
  Result := '';

  driveLetter := upcase(driveLetter);

  if CurrentOS = E4M_OS_WIN_NT then
    begin
      // We're running under NT, so use GetDisksMounted(...) to get the
      // filenames
      mountedFilenames:= TStringList.Create();
      try

// xxx -       hang on... GetDisksMounted will call us if it's under w98...
//             That's not a problem, but it's not particularly nice, is it?

        GetDisksMounted(mountedDrives, mountedFilenames);

        if Pos(driveLetter, mountedDrives)>0 then
          begin
          Result := mountedFilenames[Pos(driveLetter, mountedDrives)-1];
          end;

      finally
        mountedFilenames.Free();
      end;
    end
  else
    begin
    // Running under w98, we need to use MOUNT_LIST_N because otherwise if we
    // used MOUNT_LIST, if the length of a volume filename is >64 chars, E4M
    // would return the first 60 chars of the filename, followed by "..."
    //  - Arrrrgh!!
    mountListN98.nDosDriveNo := ord(upcase(driveLetter))-ord('A');
    if DeviceIoControl(hE4MVxD, E4M_9x_MOUNT_LIST_N, @mountListN98, sizeof(mountListN98), nil, 0, dwBytes, nil) then
      begin
      if mountListN98.nReturnCode=0 then
        begin
        volFilename := '';
        ShortArrayToString(mountListN98.wszVolume, volFilename);
        if IsFilePartition(volFilename) then
          begin
          Result := volFilename;
          end
        else
          begin
          Result := SDUConvertSFNToLFN(volFilename);
          end;

        end; // if mountListN98.nReturnCode=0 then

      end;
    end;
end;


// !! WARNING !!
// If running under NT, then if the lanegth of "volumeFilename" is >64, this
// function may FAIL as the E4M driver only gives us the first 60 chars
// followed by "..." to work with
function TOTFEE4M.GetDriveForVolFile(volumeFilename: string): Ansichar;
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

  if (CurrentOS=E4M_OS_WIN_NT) AND
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


function TOTFEE4M.GetVolumeInfo(volumeFilename: string; info: pTOTFEE4MVolumeInfo): boolean;
var
  buffer: array [1..(22 * 512)] of byte;
  posInBuffer: integer;
  tmpFile: TFileStream;
  vtid: E4M_VOLUME_FILE_TYPE;
  j: integer;
  matchFound: boolean;
  packetID: integer;
  packetLength: integer;
  k: integer;
  remainingInPkt: integer;
  nKeyLen: integer;
begin
  Result := FALSE;

  info^.volumeFilename := volumeFilename;

  info^.cipherID   := cphrUNKNOWN;
  info^.cipherName := E4M_CIPHER_NAMES[info^.cipherID];

  info^.hashID   := hashUnknown;
  info^.hashName := E4M_HASH_NAMES[info^.hashID];

  info^.volumeTypeID   := vtidUnknown;
  info^.volumeTypeName := E4M_VOLUME_FILE_TYPE_NAMES[info^.volumeTypeID];

  // We can't return any information on partitions...
  if IsFilePartition(volumeFilename) then
    begin
    info^.volumeTypeID   := vtidE4MSDSDevice;
    info^.volumeTypeName := E4M_VOLUME_FILE_TYPE_NAMES[info^.volumeTypeID];
    end
  else
    begin
    volumeFilename := SDUConvertSFNToLFN(volumeFilename);

    try
      tmpFile := TFileStream.Create(volumeFilename, fmOpenRead OR fmShareDenyNone);
      try
        tmpFile.Seek(0, soFromBeginning);
        tmpFile.Read(buffer[1], sizeof(buffer));
      finally
        tmpFile.Free();
      end;
    except
      on E:EFOpenError do
        begin
        exit;
        end;
    end;

    posInBuffer := 1;

    if FSupportScramDisk AND VersionCanSupportScramDiskVols() then
      begin
      info^.volumeTypeID := vtidScramDisk; // Default to ScramDisk
      end;

    for vtid:=low(E4M_VOLUME_FILE_TYPE) to high(E4M_VOLUME_FILE_TYPE) do
      begin
      matchFound := TRUE;
      for j:=1 to length(E4M_VOLUME_SIGNATURES[vtid]) do
        begin
        matchFound := matchFound AND ((E4M_VOLUME_SIGNATURES[vtid])[j]=char(buffer[posInBuffer+j-1]));
        end;
      if matchFound then
        begin
        info^.volumeTypeID := vtid;
        end;
      end;

    info^.volumeTypeName := E4M_VOLUME_FILE_TYPE_NAMES[info^.volumeTypeID];

    inc(posInBuffer, 4);

    if (info^.volumeTypeID=vtidSFS)    OR
       (info^.volumeTypeID=vtidOldE4M) OR
       (info^.volumeTypeID=vtidE4M)    then
      begin
      packetID := GetWordFromHeader(buffer, posInBuffer);
      packetLength := GetWordFromHeader(buffer, posInBuffer);
      while (posInBuffer<high(buffer)) AND (packetID<>0) do
        begin
        case packetID of
        1:
           begin
           // Nothing atm, just reposition the buffer pointer
           inc(posInBuffer, packetLength);
           end;
        2:
           begin
           remainingInPkt := packetLength;

           info^.cipherID := GetCipherType(GetWordFromHeader(buffer, posInBuffer));
           dec(remainingInPkt, 2);

           GetWordFromHeader(buffer, posInBuffer); // The key setup iteration count
           dec(remainingInPkt, 2);

           for k:=1 to 20 do // that 20 is hardcoded in voltest; according to the SFS docs, it should be the blocksize of the encryption algorithm (20 for SFS)
             begin
             GetWordFromHeader(buffer, posInBuffer); // The disk key IV (salt)
             dec(remainingInPkt, 2);
             end;

           if (info^.volumeTypeID=vtidE4M) then
             begin
             nKeyLen := E4M_DISKKEY_SIZE;
             end
           else
             begin
             nKeyLen := SFS_DISKKEY_SIZE;
             end;

           dec(remainingInPkt, nKeyLen);

           for k:=1 to (nKeyLen div 2) do
             begin
             GetWordFromHeader(buffer, posInBuffer); // The encrypted disk key
             end;

           GetWordFromHeader(buffer, posInBuffer); // Key check value
           dec(remainingInPkt, 2);

           if (info^.volumeTypeID=vtidE4M) then
             begin
             info^.hashID := GetHashType(GetWordFromHeader(buffer, posInBuffer));
             dec(remainingInPkt, 2);
             end;

           // Reposition the position in the buffer to skip anything else in this
           // packet
           inc(posInBuffer, remainingInPkt);
           end;

        end; // case packetID of

        packetID := GetWordFromHeader(buffer, posInBuffer);
        packetLength := GetWordFromHeader(buffer, posInBuffer);

        end; // while (posInBuffer<high(buffer)) AND (packetID<>0) do

      end; // if info^.volumeType<>VOLUME_FILE_TYPE[high(VOLUME_FILE_TYPE)] then

    info^.hashName := E4M_HASH_NAMES[info.hashID];
    info^.cipherName := E4M_CIPHER_NAMES[info^.cipherID];

    info^.volumeFilename := volumeFilename;
    end;

  Result := TRUE;

end;


function TOTFEE4M.GetWordFromHeader(buffer: array of byte; var posInBuffer: integer): cardinal;
begin
  Result := (buffer[posInBuffer-1] * 256) + buffer[posInBuffer];
  inc(posInBuffer, 2);

end;

function TOTFEE4M.GetCipherName(cipherID: integer): string;
var
  i: E4M_CIPHER_TYPE;
begin
  Result := '';

  for i:=low(E4M_CIPHER_IDS) to high(E4M_CIPHER_IDS) do
    begin
    if E4M_CIPHER_IDS[i]=cipherID then
      begin
      Result := E4M_CIPHER_NAMES[i];
      end;
    end;

end;


function TOTFEE4M.GetDriveInfo(driveLetter: char): TOTFEE4MVolumeInfo;
var
  i: integer;
  currInfo: pTOTFEE4MVolumeInfo;
begin
  driveLetter := upcase(driveLetter);

  UpdateVolInfoList();

  for i:=0 to (mtdVolInfoLst.count-1) do
    begin
    currInfo := mtdVolInfoLst.Items[i];
    if currInfo^.mountedAs = driveLetter then
      begin
      Result := currInfo^;
      end;

    end;

end;

function TOTFEE4M.ClearPasswords(driveLetter: char): boolean;
var
  dwResult: DWORD;
begin
  Result := DeviceIoControl(hE4MVxD,
                            E4M_WIPE_CACHE,
                            nil,
                            0,
                            nil,
                            0,
                            dwResult,
                            nil);
end;


function TOTFEE4M.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  CheckActive();

  Result := FSupportScramDisk AND VersionCanSupportScramDiskVols();

  if not(Result) then
    begin
    Result := IsE4MorSFSEncryptedVolFile(volumeFilename);
    end;

end;

// Return TRUE if the volume file specified is a E4M or SFS volume, otherwise
// FALSE
function TOTFEE4M.IsE4MorSFSEncryptedVolFile(volumeFilename: string): boolean;
var
  junk: boolean;
begin
  Result := IsE4MorSFSEncryptedVolFile(volumeFilename, junk);
end;

function TOTFEE4M.IsE4MorSFSEncryptedVolFile(volumeFilename: string; var cancelled: boolean): boolean;
var
  info: pTOTFEE4MVolumeInfo;
begin
  cancelled := FALSE;

  if IsFilePartition(volumeFilename) then
    begin
    Result := TRUE;
    end
  else
    begin
    volumeFilename := SDUConvertSFNToLFN(volumeFilename);
    new(info);
    if GetVolumeInfo(volumeFilename, info) then
      begin
      Result := not((info^.volumeTypeID = vtidUnknown) OR (info^.volumeTypeID = vtidScramDisk));
      end
    else
      begin
      Result := (GetDriveForVolFile(volumeFilename)<>#0);
      end;
    dispose(info);
    end;

end;

// Remove any drive entries that are no longer mounted, and add records for any
// drives that have been mounted, but don't yet have a record
procedure TOTFEE4M.UpdateVolInfoList();
var
  i: integer;
  mtdDrives: string;
  currInfo: pTOTFEE4MVolumeInfo;
  volFilenames: TStringList;
  posInStr: integer;
begin
  volFilenames:= TStringList.Create();
  try
    GetDisksMounted(mtdDrives, volFilenames);

    for i:=(mtdVolInfoLst.count-1) downto 0 do
      begin
      currInfo := mtdVolInfoLst.Items[i];
      posInStr := pos(currInfo^.mountedAs, mtdDrives);
      if posInStr=0 then
        begin
        mtdVolInfoLst.Delete(i);
        dispose(currInfo);
        end
      else
        begin
        delete(mtdDrives, posInStr, 1);
        volFilenames.Delete(posInStr-1);
        end;
      end;

    for i:=1 to length(mtdDrives) do
      begin
      new(currInfo);

      currInfo^.volumeFilename := volFilenames[i-1];
      currInfo^.mountedAs := mtdDrives[i];

      currInfo^.cipherID := cphrUnknown;
      currInfo^.cipherName := E4M_CIPHER_NAMES[currInfo^.cipherID];

      currInfo^.hashID := hashUnknown;
      currInfo^.hashName := E4M_HASH_NAMES[currInfo^.hashID];

      if IsFilePartition(currInfo^.volumeFilename) then
        begin
        currInfo^.volumeTypeID   := vtidE4MSDSDevice;
        currInfo^.volumeTypeName := E4M_VOLUME_FILE_TYPE_NAMES[currInfo^.volumeTypeID];
        end
      else
        begin
        currInfo^.volumeTypeID := vtidUnknown;
        currInfo^.volumeTypeName := E4M_VOLUME_FILE_TYPE_NAMES[currInfo^.volumeTypeID];
        end;

      currInfo^.readonly := IsDriveReadonly(mtdDrives[i]);

      mtdVolInfoLst.Add(currInfo);
      end;

  finally
    volFilenames.Free();
  end;

end;

function TOTFEE4M.GetHashType(hashID: integer): E4M_HASH_TYPE;
var
  hashLoop: E4M_HASH_TYPE;
begin
  Result := high(E4M_HASH_IDS);
  for hashLoop:=low(E4M_HASH_IDS) to high(E4M_HASH_IDS) do
    begin
    if E4M_HASH_IDS[hashLoop]=hashID then
      begin
      Result := hashLoop;
      end;
    end;

end;

function TOTFEE4M.GetCipherType(cipherID: integer): E4M_CIPHER_TYPE;
var
  cipherLoop: E4M_CIPHER_TYPE;
begin
  Result := high(E4M_CIPHER_IDS);
  for cipherLoop:=low(E4M_CIPHER_IDS) to high(E4M_CIPHER_IDS) do
    begin
    if E4M_CIPHER_IDS[cipherLoop]=cipherID then
      begin
      Result := cipherLoop;
      end;
    end;

end;


function TOTFEE4M.Title(): string;
begin
  Result := 'E4M';
end;


function TOTFEE4M.Version(): cardinal;
var
  driverVers: TOTFEE4M_VERSION;
  dwResult: DWORD;
begin
  CheckActive();

  if E4MDriverVersion=$FFFFFFFF then
    begin
    if DeviceIoControl(hE4MVxD,
                       E4M_DRIVER_VERSION,
                       @driverVers,
                       sizeof(driverVers),
                       @driverVers,
                       sizeof(driverVers),
                       dwResult,
                       nil) then
      begin
      E4MDriverVersion := driverVers.version;
      end
    else
      begin
      // v2.00 didn't support getting the driver version, so it must be that
      // version
      if E4MDriverVersion=$FFFFFFFF then
        begin
        E4MDriverVersion := $200;
        end;
      end;

    end;

  Result := E4MDriverVersion;

end;

function TOTFEE4M.CloseSlot(driveNum: integer; brutal: boolean): boolean;
begin
  if (ld(driveNum, 1)) then
    begin
    Result := FALSE;
    // xxx - Setup error code here
    // *err = ERR_FILES_OPEN_LOCK;
    end
  else
    begin
    Result := DoDeviceClose(driveNum);
    ld(driveNum, 0);
    end;

end;


function TOTFEE4M.ld(driveNum: integer; mode: integer): boolean;
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
function TOTFEE4M.locklogdrive(drivenum: integer; mode: integer): boolean;
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


function TOTFEE4M.ioctllock(nDrive: cardinal; permissions: integer; func: integer): integer;
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


function TOTFEE4M.DoDeviceClose(driveNum: integer): boolean;
var
  mount_list: TOTFEE4M_9x_MOUNT_LIST_N_STRUCT;
  unmount: TOTFEE4M_UNMOUNT;
  tries: integer;
  c: integer;
  dwBytes: DWORD;
  volFilename: string;
begin
  mount_list.nDosDriveNo := driveNum;
  if (DeviceIoControl(hE4MVxD, E4M_9x_MOUNT_LIST_N, @mount_list, sizeof(mount_list), nil, 0, dwBytes, nil) = FALSE) then
    begin
    // xxx - Setup error code here
    // *err = ERR_OS_ERROR;
    Result := FALSE;
    exit;
    end
  else
    begin
    if (mount_list.nReturnCode<>0) then
      begin
      // xxx - Setup error code here
      // *err = mount_list.nReturnCode;
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
  if (DeviceIoControl (hE4MVxD, E4M_UNMOUNT_PENDING, @unmount, sizeof(unmount), nil, 0, dwBytes, nil) = FALSE) then
    begin
    // xxx - Setup error code here
    //*err = ERR_OS_ERROR;
    Result := FALSE;
    exit;
    end
  else
    begin
    if (mount_list.nReturnCode<>0) then
      begin
      // xxx - Setup error code here
      //*err = mount_list.nReturnCode;
      Result := TRUE;
      exit;
      end;
    end;

  for c:=0 to 19 do
    begin
    DeviceIoControl(hE4MVxD, RELEASE_TIME_SLICE, nil, 0, nil, 0, dwBytes, nil);
    end;

  for tries:=0 to 31 do
    begin
    DeviceIoControl (hE4MVxd, RELEASE_TIME_SLICE, nil, 0, nil, 0, dwBytes, nil);
    if (DeviceIoControl (hE4MVxd, E4M_UNMOUNT, @unmount, sizeof(UNMOUNT), nil, 0, dwBytes, nil) = FALSE) then
      begin
      // xxx - Setup error code here
      // *err = ERR_OS_ERROR;
      Result := FALSE;
      exit;
      end
    else
      begin
      if (mount_list.nReturnCode=0) then
        begin
        // xxx - Setup error code here
        // *err = 0;
        Result := TRUE;
        exit;
        end;

      end;
    end;

  // xxx - Setup error code here
  // *err = ERR_FILES_OPEN_LOCK;
  Result := TRUE;

end;

function TOTFEE4M.EjectStop(driveLetter: char; func: boolean): boolean;
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


function TOTFEE4M.GetCachePasswordInDriver(): boolean;
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    Result := iniFile.ReadBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_CACHEINDRIVER, FALSE);

  finally
    iniFile.Free();
  end;

end;

procedure TOTFEE4M.SetCachePasswordInDriver(cache: boolean);
var
  iniFile: TIniFile;
begin
  if SaveHistory then
    begin
    iniFile := TIniFile.Create(E4M_INI_FILE);
    try
      iniFile.WriteBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_CACHEINDRIVER, cache);

    finally
      iniFile.Free();
    end;
  end;

end;

function TOTFEE4M.GetMountWarning(): boolean;
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    Result := iniFile.ReadBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_NOMOUNTWARNING, FALSE);

  finally
    iniFile.Free();
  end;

end;

procedure TOTFEE4M.SetMountWarning(mountWarning: boolean);
var
  iniFile: TIniFile;
begin
  // This setting is not affected by the no history switch
  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    iniFile.WriteBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_NOMOUNTWARNING, mountWarning);

  finally
    iniFile.Free();
  end;

end;

function TOTFEE4M.GetDefaultDrive(): char;
var
  iniFile: TIniFile;
  dfltDrvColon: string;
begin
  Result := #0;

  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    dfltDrvColon := iniFile.ReadString(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_DRIVELETTER, '');
    if dfltDrvColon<>'' then
      begin
      Result := dfltDrvColon[1];
      end;

  finally
    iniFile.Free();
  end;

end;


procedure TOTFEE4M.SetDefaultDrive(dfltDrive: char);
var
  iniFile: TIniFile;
begin
  if SaveHistory then
    begin
    iniFile := TIniFile.Create(E4M_INI_FILE);
    try
      iniFile.WriteString(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_DRIVELETTER, upcase(dfltDrive)+':');

    finally
      iniFile.Free();
    end;
  end;

end;


function TOTFEE4M.GetSaveHistory(): boolean;
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    Result := not(iniFile.ReadBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_NEVERSAVEHISTORY, TRUE));

  finally
    iniFile.Free();
  end;

end;

procedure TOTFEE4M.SetSaveHistory(save: boolean);
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(E4M_INI_FILE);
  try
    iniFile.WriteBool(E4M_INI_SECTION_LASTRUN, E4M_INI_KEY_NEVERSAVEHISTORY, save);

  finally
    iniFile.Free();
  end;

end;

procedure TOTFEE4M.AddVolumeToHistory(volumeFilename: string);
var
  i: integer;
  key: string;
  iniFile: TIniFile;
  lastVol: string;
  historyMRU: TStringList;
begin
  if SaveHistory then
    begin
    iniFile := TIniFile.Create(E4M_INI_FILE);
    try
      historyMRU:= TStringList.Create();
      try
        for i:=1 to E4M_SIZEOF_MRU_LIST do
          begin
          key := E4M_INI_KEY_LASTVOLUMEn + inttostr(i);
          historyMRU.Add(iniFile.ReadString(E4M_INI_SECTION_LASTRUN, key, ''));
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

        for i:=1 to E4M_SIZEOF_MRU_LIST do
          begin
          key := E4M_INI_KEY_LASTVOLUMEn + inttostr(i);
          iniFile.WriteString(E4M_INI_SECTION_LASTRUN, key, historyMRU[i-1]);
          end;

      finally
        historyMRU.Free();
      end;

    finally
      iniFile.Free();
    end;

    end;

end;


function TOTFEE4M.GetNextUnusedDrvLtr(afterDrv: char): char;
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
function TOTFEE4M.GetUnusedDriveLetters(): string;
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

function TOTFEE4M.VersionStr(): string;
var
  verNo: cardinal;
  majorVer : integer;
  minorVer1: integer;
  minorVer2: integer;
begin
  verNo:= Version();
  majorVer  := (verNo AND $FF00) div $FF;
  minorVer1 := (verNo AND $F0) div $F;
  minorVer2 := (verNo AND $F);

  Result := Format('v%d.%d.%d', [majorVer, minorVer1, minorVer2]);

end;

// For ScramDisk volumes, get the 4 passwords and SHA1 them together
function TOTFEE4M.GetScramDiskPasswords(dlgTitle: string; var passwordDigest: string; var driveLetter: char): boolean;
var
  hashObj: THashAlgSHA1;
  tempPasswordDigest: THashArray;
  tempPasswordString: string;
  passwordEntryDlg: TOTFEE4MScramDiskPasswordEntry_F;
  i: integer;
  j: integer;
  drvLtrsFree: string;
  tempDriveLetter: char;
begin
  Result := FALSE;

  passwordEntryDlg := TOTFEE4MScramDiskPasswordEntry_F.create(nil);
  try
    passwordEntryDlg.ckHidePasswords.visible := not(FHidePasswordsEnteredCk);

    // The user should be prompted for the drive letter, and offered the
    // default
    drvLtrsFree := GetUnusedDriveLetters();
    tempDriveLetter := DefaultDrive;

    if pos(tempDriveLetter, drvLtrsFree)<1 then
      begin
      tempDriveLetter := GetNextUnusedDrvLtr(driveLetter);
      end;
    passwordEntryDlg.DrivesAllowed := drvLtrsFree;
    passwordEntryDlg.Drive := tempDriveLetter;

    passwordEntryDlg.caption := dlgTitle;

    if (passwordEntryDlg.showmodal=mrOK) then
      begin
      tempPasswordString := Format('%160s', ['x']);
      tempPasswordString := passwordEntryDlg.mePassword1.text +
                            Format('%'+inttostr(40-length(passwordEntryDlg.mePassword1.text))+'s', [#0]) +
                            passwordEntryDlg.mePassword2.text +
                            Format('%'+inttostr(40-length(passwordEntryDlg.mePassword2.text))+'s', [#0]) +
                            passwordEntryDlg.mePassword3.text +
                            Format('%'+inttostr(40-length(passwordEntryDlg.mePassword3.text))+'s', [#0]) +
                            passwordEntryDlg.mePassword4.text +
                            Format('%'+inttostr(40-length(passwordEntryDlg.mePassword4.text))+'s', [#0]);

      // Clear down the dialog's passwords
      passwordEntryDlg.ClearEnteredPasswords();

      hashObj := THashAlgSHA1.Create(nil);
      try
        tempPasswordDigest := hashObj.HashString(tempPasswordString);

        // Destroy copy of the passwords
        for i:=1 to 160 do
          begin
          tempPasswordString[i] := char(Random(256));
          end;
        tempPasswordString := '';

        // OK, because the driver effectively expects the hashed passwords to
        // be passed to it as a set of 4 DWORDs, we need to setup the digest
        // in the format:
        // byte# 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12, 19, 18, 17, 16
        passwordDigest := '';
        for i:=0 to 4 do
          begin
          for j:=0 to 3 do
            begin
            passwordDigest := passwordDigest + char(tempPasswordDigest[(i*4)+(3-j)]);
            tempPasswordDigest[(i*4)+(3-j)] := Random(256);
            end;
          end;

      finally
        hashObj.Free();
      end;

      driveLetter := passwordEntryDlg.Drive;

      Result := TRUE;

      end // if (passwordEntryDlg.showmodal=mrOK) then
    else
      begin
      passwordEntryDlg.ClearEnteredPasswords();
      FLastErrCode := OTFE_ERR_USER_CANCEL;
      end;

  finally
    passwordEntryDlg.free;
  end;

end;


procedure TOTFEE4M.SetSupportScramDisk(support: boolean);
begin
  if not(support) then
    begin
    FSupportScramDisk := FALSE;
    end
  else
    begin
    CheckActive();

    if VersionCanSupportScramDiskVols() then
      begin
      FSupportScramDisk := support;
      end
    else
      begin
      FSupportScramDisk := FALSE;
      end;

    end;

end;

function TOTFEE4M.VersionCanSupportScramDiskVols(): boolean;
begin
  Result := (Version() = $200);

end;

function TOTFEE4M.GetMainExe(): string;
var
  registry: TRegistry;
  keyName: string;
  appPath: string;
  exeLocation: string;
begin
  Result := '';
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    keyName := E4M_REGISTRY_ENTRY_PATH;
    if registry.OpenKeyReadOnly(keyName) then
      begin
      appPath := registry.ReadString('');
      exeLocation := ExtractFilePath(appPath) + E4M_APP_EXE_NAME;
      if FileExists(exeLocation) then
        begin
        Result := exeLocation
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

function TOTFEE4M.GetAvailableRemovables(dispNames: TStringList; deviceNames: TStringList): integer;
var
  cnt: integer;
  szTmp: array [0..E4M_MAX_PATH-1] of char;
begin
  cnt := 0;

  if CurrentOS=E4M_OS_WIN_NT then
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


function TOTFEE4M.GetAvailableFixedDisks(dispNames: TStringList; deviceNames: TStringList): integer;
var
  i: integer;
  n: integer;
  cnt: integer;
  strTmp: string;
begin
  cnt := 0;

  i := 0;
  while (i<64) do
    begin
    for n:=1 to E4M_HDD_MAX_PARTITIONS do
      begin
      strTmp := Format(E4M_HDD_PARTITION_DEVICE_NAME_FORMAT, [i, n]);

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


function TOTFEE4M.OpenDevice(device: string): boolean;
var
  driver200: TOTFEE4M_200_OPEN_TEST;
  driver201: TOTFEE4M_201_OPEN_TEST;
  dwResult: DWORD;
  currAllOK: boolean;
  i: integer;
begin
  CheckActive();

  for i:=1 to length(device) do
    begin
    driver200.wszFileName[i] := WCHAR(device[i]);
    end;
  driver200.wszFileName[length(device)+1] := #0;

  StringToShortArray(device, driver201.wszFileName);

{
// xxx - is this needed?
  if CurrentOS=E4M_OS_WIN_NT then
    begin
    ToUNICODE ((char *) &driver->wszFileName[0]);
    end;
}

  if E4MDriverVersion=$200 then
    begin
    currAllOK := DeviceIoControl(hE4MVxD,
                                 E4M_OPEN_TEST,
                                 @driver200,
                                 sizeof(driver200),
                                 @driver200,
                                 sizeof(driver200),
                                 dwResult,
                                 nil);

    Result := currAllOK;
    if not(currAllOK) then
      begin
      dwResult := GetLastError ();
      Result := (dwResult = ERROR_SHARING_VIOLATION);
      end;
    end
  else
    begin
    currAllOK := DeviceIoControl(hE4MVxD,
                                 E4M_OPEN_TEST,
                                 @driver201,
                                 sizeof(driver201),
                                 @driver201,
                                 sizeof(driver201),
                                 dwResult,
                                 nil);

    if not(currAllOK) then
      begin
      dwResult := GetLastError();
      Result := (dwResult = ERROR_SHARING_VIOLATION);
      end
    else
      begin
      if CurrentOS=E4M_OS_WIN_NT then
        begin
        Result := TRUE;
        end
      else if (driver201.nReturnCode = 0) then
        begin
        Result := TRUE;
        end
      else
        begin
        // xxx - Setup error code here
        // SetLastError(ERROR_FILE_NOT_FOUND);
        Result := FALSE;
        end;
      end;
    end;

end;


function TOTFEE4M.GetAvailableRawDevices(dispNames: TStringList; deviceNames: TStringList): boolean;
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


// This function is required as v2.0.1's short arrays are used:
//   Under NT to store WCHARs
//   Under 95/98 to store chars
// as a result of this, we need to extract data from these arrays with in the
// correct way, depending on the OS being used...
// i.e. This function will take an E4M array of short and convert it back into
//      a string
procedure TOTFEE4M.ShortArrayToString(theArray: array of short; var theString: string);
var
  i: integer;
  hiNotLo: boolean;
  shortChar: short;
begin
  theString := '';

  if CurrentOS<>E4M_OS_WIN_NT then
    begin
    i:=0;
    hiNotLo:= TRUE;
    shortChar := (theArray[0] AND $FF);
    while shortChar<>0 do
      begin
      theString := theString + chr(shortChar);

      hiNotLo := not(hiNotLo);

      shortChar := theArray[i];
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
    shortChar := theArray[0];
    while shortChar<>0 do
      begin
      theString := theString + char(shortChar);
      inc(i);
      shortChar := theArray[i];;
      end;
    end;

end;


// This function is required as v2.0.1's short arrays are used:
//   Under NT to store WCHARs
//   Under 95/98 to store chars
// as a result of this, we need to fill in these arrays with correctly packed
// data, depending on the OS being used
// i.e. This function will take a string and put it into an E4M array of short
procedure TOTFEE4M.StringToShortArray(theString: string; var theArray: array of short);
var
  i: integer;
  hiNotLo: boolean;
  arrPos: integer;
begin
  theString := theString + #0;

  if CurrentOS<>E4M_OS_WIN_NT then
    begin
    hiNotLo:= TRUE;
    arrPos := 0;
    theArray[arrPos] := 0;
    for i:=1 to length(theString) do
      begin
      if hiNotLo then
        begin
        theArray[arrPos] := ord(theString[i]);
        end
      else
        begin
        theArray[arrPos] := theArray[arrPos] OR ord(theString[i]) shl 8;
        inc(arrPos);
        theArray[arrPos] := 0;
        end;

      hiNotLo := not(hiNotLo);
      end;
    end
  else
    begin
    for i:=1 to length(theString) do
      begin
      theArray[i-1] := short(WCHAR(theString[i]));
      end;
    end;

end;

function TOTFEE4M.IsFilePartition(volumeFilename: string): boolean;
begin
  Result := (pos('\DEVICE\', uppercase(volumeFilename))=1);

end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEE4M.MountDevices(): string;
var
  mntDeviceDlg: TOTFEE4MMountDevice_F;
  finished: boolean;
begin
  Result := '';

  if MountDeviceDlg9xStyle then
    begin
    mntDeviceDlg := TOTFEE4MMountDevice9x_F.Create(nil);
    end
  else
    begin
    mntDeviceDlg := TOTFEE4MMountDeviceNT_F.Create(nil);
    end;
  try
    mntDeviceDlg.E4MComponent := self;
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
function TOTFEE4M.CanMountDevice(): boolean;
begin
  // Supported...
  Result := TRUE;
end;

// -----------------------------------------------------------------------------


END.


