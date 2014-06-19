unit OTFEScramDisk_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface


//xxx - TODO LIST:
// xxx - implement BACKUP HEADER DATA MOUNTING (v3.xx only)
// xxx - verify skf files work OK
// xxx - grep for "xxx"
// xxx - ensure that   FLastErrCode:= OTFE_ERR_SUCCESS; or whatever when applicable
// xxx - FLastErrCode - mount
// xxx - FLastErrCode - dismount
// xxx - FLastErrCode - getinfo



uses
  Classes, SysUtils, Windows,
  WinSvc,
  SdStructures_U,
  WindowsMissing_U,
  OTFE_U, OTFEConsts_U;

const
  // ---
  // NT ONLY - PUT IN NT DISMOUNT FILE??
  // ---
  SDCHECKOPENFILES     = 0;
  SDDISMOUNTDISK       = 1;
  SDSENDUNMOUNDPENDING = 2;
  // ---


  CRLF = #10+#13;

  SYS_FILE = 'SD.SYS';
  SYS_NAME = 'SD';
  DRIVER_RAM_SIZE = 32768;

  NT_SEND_SD_DEVICE  = $73000;
  PD_INQUIRE_DEVICES  = 2;
  REQMON_INQUIRE_DEVICES   = 4;
  INQUIRE_CRYPT_DEVICES  = 4;
  SUPPLY_PASSWORD  = 5;
  CLOSE_CRYPTED_DEVICE1  = 6;
  CLOSE_CRYPTED_DEVICE2  = 7;
  CLOSE_CRYPTED_DEVICE3  = 8;
  CLOSE_CRYPTED_DEVICE4  = 9;
  PUTCVS  = 10;
  PD_GETCVS  = 1;
  DISKIO  = 11;
  GETDCBINFO  = 12;
  FINDDRIVELETTER  = 13;
  DISMOUNTDRIVE  = 14;
  MOUNTFILE      = 15;
  GETSHIFTCONST  = 16;
  CLEARPASSWORDS  = 17;
  MOUNTNEWDISK    = 18;
  BORDER  = 19;
  VALIDATEPASSWORD  = 20;
  APPQUERY  = 21;
  VOLQUERY  = 22;
  GETDRIVERVERSION  = 23;
  SETTIMEOUT  = 24;
  RELEASETIMESLICE  = 25;
  READINFOBLOCK  = 26;
  WRITEINFOBLOCK  = 27;
  R0FILEIO   = 28;
  RING0CALLBACK  = 29;
  UNINSTALLSCRAMDISK  = 30;
  SETKEYFILEPASSWORD  = 31;
  GETKEYFILEPASSWORD  = 32;
  GETPASSWORDBUFFER   = 33;
  GETINKEY  = 34;
  SUPPLYDATEANDTIME  = 35;
  SUPPLYCLICKEDFILENAME  = 36;
  SENDFORCEQUIT  = 37;
  REVOKESKFDATA  = 38;
  RAWDEVICEDATA  = 39;
  CHANGEPASSWORDS  = 40;
  CLOSE_CRYPTED_DEVICE_N  = 41;
  APP_REQUEST_REFRESH  = 42;
  INQUIRE_SLOT  = 43;
  ENABLE_CONTAINER_DELETE  = 44;
  ALLOW_FAST_SHUTDOWN  = 45;
  DISABLE_AUTORUN  = 46;

  SET_SECTOR_GET  = 47;

  GET_FILE_SECTORS_HANDLE  = 48;

  GET_FILE_SECTORS  = 49;
  REGISTERAPPWINDOW  = 50;
  DEREGISTERAPPWINDOW  = 51;
  SETALTCRITDATA  = 52;
  PASSWORDCACHESTATUS  = 53;
  TRAVELLERMODE  = 54;
  GETTESTVAR  = 55;
  CLR_PASSWORD_RESPONSE  = 56;

//used on NT only....
  INQUIRE_TIMEOUT  = 57;
  CHECK_NT_EVENTS  = 58;
  DISCONNECT_DOSDRIVE  = 59;
  READBOOTSECTOR  = 60;
// CHECKOPENFILESORDISMOUNTDISK = 61;
  SDISK_NT_IOCTL  = 61;
  SDISK_NT_EJECT_STOP  = 62;

  NTFILEOPEN  = 63;
  NTFILEACCESS  = 64;
  NTFILECLOSE   = 65;
  NTAPPQUERY  = 66;


type

  TOTFEScramDisk = class(TOTFE)
  private

    hScramDiskVxD: THandle;
    cachedDriverVersion: cardinal;
    pThePasswords: Pointer;

    FUseRedScreen: boolean;
    FRedScreenCaption : string;
    FHidePasswordsEnteredCk: boolean;
    FDefaultPromptDir: string;
    volumeMadeReadonly : array [0..MAX_SCRAMDISK_SLOTS] of boolean;

    FStartDrive: integer;

    RunningOnNT: boolean;


    DriverRam: array [0..DRIVER_RAM_SIZE-1] of char;

    // Aman used VisibleDrives in ScramDisk v3 because of problems
    // reported when he used GetLogicalDrives()... So we do to.
    VisibleDrives: cardinal;
    NTSDPartitionNames: TStringList; // Holds a list of ScramDisk partition device names - only used with ScramDisk NT



    // DetermineMountType for ScramDisk v2.02 and above
    function DetermineMountType(slotNo: integer): TMountType; overload;
    // DetermineMountType for ScramDisk v3
    function DetermineMountType(cv: TCryptVol): TMountType; overload;


    function  SetReadonlyAttrib(filename: string; setNotUnset: boolean) : boolean;


    function  NTLoadLocalDriver(): THandle;
    function  NTUnloadDeviceDriver(Name: string): boolean;
    function  NTLoadDeviceDriver(Name: string; Path: string; var lphDevice: THandle; var Error: DWORD): boolean;
    function  ResetSDRegistry(): boolean;
    function  OpenDevice(DriverName: string; var lphDevice: THandle): boolean;
    function  StopDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;
    function  RemoveDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;
    function  InstallDriver(SchSCManager: SC_HANDLE; DriverName: string; ServiceExe: string): boolean;
    function  StartDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;

    
{$IFDEF _DEBUG}
    procedure DEBUG_REPORTERROR(e: integer);
{$ENDIF}


    function SDXDeviceIoControl(hDevice: THandle;       // handle to device of interest
                                dwIoControlCode: DWORD; // control code of operation to perform
                                lpInBuffer: Pointer;    // pointer to buffer to supply input data
                                nInBufferSize: DWORD;   // size of input buffer
                                lpOutBuffer: Pointer;   // pointer to buffer to receive output data
                                nOutBufferSize: DWORD;  // size of output buffer
                                lpBytesReturned: DWORD; // pointer to variable to receive output byte count
                                thelpOverlapped: POverlapped  // pointer to overlapped structure for asynchronous operation
                                ): boolean;
    procedure NTMountSDPartitions(crit: PChar);
    procedure MountNTVol(drive: char);
    function  openread(filename: string): THandle;

    function  NTCheckStreaming(slotNo: integer): boolean;
    function  NTDoDeviceCloseNT(slotNo: integer; brutal: integer): boolean;
    procedure ResetTimeStamp(filename: string);
    procedure Refresh_MyComp();
    procedure UniCpy(var output: string; pwidestring: LPWSTR);
    function  RecursedContainer(drv: char): boolean;
    function  NTDismountVolume(brutal: integer; drv: integer; slot: integer): boolean;
    procedure NTReadAllPartitionsNT();
    function  NTReadNTPartition(physdrive: integer; var driveLayout: TDRIVE_LAYOUT_INFORMATION): boolean;
    procedure SetPreferredDrive(drive: char);
    function  GetPreferredDrive(): char;



  protected
    MyCVS : array [0..MAX_SCRAMDISK_SLOTS] of TCryptVol;

    procedure ClearAllPasswords(skipActiveCheck: boolean); overload; virtual;
    procedure SetPasswords(skipActiveCheck: boolean; Password1, Password2, Password3, Password4 : string); overload; virtual;
    function  EjectStop(DriveLetter: string; EjectFunction : boolean): boolean; virtual;
    procedure NTEjectStopNT(DriveLetter: char; EjectFunction: boolean);
    function  Connect(): boolean;
    function  Disconnect(): boolean;
    procedure SetActive(AValue : Boolean); override;
    procedure UpdateDeviceDriverTime;virtual;
    procedure GetPasswordBufferInt;
    procedure PassPasswords;virtual;
    function  CheckWav(Filename: string; var wavInfo: TWavInfo): Integer; virtual;
    function  FindWavData(fs : TFileStream; var wavInfo: TWavInfo) : Integer;
    function  IOCtlLock(DriveNo: Integer; Permissions: Integer; FunctionNo: Integer): Integer; virtual;
    function  LockLogDrive(DriveNo: Integer; Mode :Integer): Integer; virtual;
    function  LockDrive(Drive: string; Mode: Integer): Boolean; virtual;
    procedure BrutalDismountSlot(SlotNo: Integer);virtual;
    function  PerformMountContainer(Filename: string; clearPasswordsAfterwards: boolean): boolean;

    function  GetWordFromStream(ts: TFileStream): WORD;
    function  GetDWordFromStream(ts: TFileStream): DWORD;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    procedure DEBUG_GetDriveInfo(physdrive: integer; var driveLayout: TDRIVE_LAYOUT_INFORMATION);

    // "SniffPassword" - sounds horrible, but is actually used to demonstrate
    // a security weakness in ScramDisk
    procedure SniffPassword(var password1 : string;
                            var password2 : string;
                            var password3 : string;
                            var password4 : string);

    // Enable/disable the ScramDisk driver's ".SVL" file protection
    function  SetEnableVolumeDeletion(enable: boolean): boolean;

    procedure ClearAllPasswords(); overload; virtual;
    function  GetSlotInfo(SlotNo : Integer) : TSlotInfo; virtual;
    procedure UpdateSlotInfo;virtual;
    procedure RefreshApplication;virtual;
    procedure MountPartitions; virtual;
    function  SetPasswordsRedScreen(ScreenTitle : string) : Boolean;virtual;
    procedure SetPasswords(Password1, Password2, Password3, Password4 : string); overload; virtual;
    procedure MountAll;virtual;
    function  ValidScramDiskPartitions(partitionNames: TStringList): boolean;

    // TOTFE functions...
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; overload; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  DrivesMounted(): string; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  GetMainExe(): string; override;

    function  GetSlotForDrive(driveLetter: char): integer;
    function  GetVolumeInfo(driveLetter: char; var info: TSlotInfo): boolean;


    function  IsDriveReadonly(driveLetter: char): boolean; override;

    // Compatability with TEnhKrScramDisk
    // !! WARNING TO DEVELOPERS !!!
    // These functions are only included for backward compatability with
    // TEnhkrScramDisk and *may* or *may not* be dropped in a later release.
    function  MountPrompted(var drive: string): boolean; overload;
    function  MountPrompted(volumeFilenames: TStrings; readonly: boolean) : boolean; overload;
    function  MountPrompted(volumeFilename: string; readonly: boolean): boolean; overload;
    // Modified MountContainer to mount container file as readonly
    function  MountContainer(filename: string; readonly: boolean; clearPasswordsAfterwards: boolean): boolean; overload;
    function  MaxSlots() : integer;
    function  NumberMounted() : integer;
    // Modified Dismounts to undo any readonly changes made
    procedure Dismount(SlotNo : Integer; Brutal : Boolean = TRUE); overload;
    // This next Dismount has been dropped (replaced with TOTFE Dismount
//    procedure Dismount(Drive : string; Brutal : Boolean = TRUE); overload;
    function  GetDriveLetterForVolFile(volumeFilename: string): string;
    function  EnterPasswordsDialog(): boolean;
    function  MountPartitionsPrompted(): string;
    function  DismountPrompted(): boolean;

  published
    // Use RedScreen has no effect under NT/2000
    property  UseRedScreen: boolean read FUseRedScreen write FUseRedScreen default FALSE;
    // Use RedScreenCaption has no effect under NT/2000
    property  RedScreenCaption: string read FRedScreenCaption write FRedScreenCaption;
    property  HidePasswordsEnteredCk: boolean read FHidePasswordsEnteredCk write FHidePasswordsEnteredCk default TRUE;
    property  DefaultPromptDir: string read FDefaultPromptDir write FDefaultPromptDir;

    // PreferredDrive was introduced with ScramDisk v3 
    property  PreferredDrive: char read GetPreferredDrive write SetPreferredDrive;
  end;

type
  TkrScramDisk = TOTFEScramDisk;
  TEnhKrScramDisk = TOTFEScramDisk;

procedure Register;

implementation

uses
  OTFEScramDiskPasswordEntry_U,
  Controls, // required for definition of mrOK
  SDUGeneral,
  dialogs, // required for TOpenDialog
  OTFEScramDiskDismountVolumes_U, // required for the Dismount Volumes dialog
  HashAlg_U, // required for SHA-1 hashing
  HashAlgSHA1_U, // required for SHA-1 hashing
  shlobj, // required for NT/2K IShellFolder, IMalloc, SHGetDesktopFolder, SHGetSpecialFolderLocation, etc
  messages; // required for NT/2K for refresh

procedure Register;
begin
  // TkrScramDisk is no longer registered
  // TEnhKrScramDisk is no longer registered
  RegisterComponents('OTFE', [TOTFEScramDisk]);
end;

(******************************************************************************
                         TOTFEScramDisk DEFINITIONS
 ******************************************************************************)

{ --- Standard component and connection routines --- }

constructor TOTFEScramDisk.Create(AOwner : TComponent);
var
  i : integer;
  OSversion: DWORD;
begin
  inherited create(AOwner);

  OSversion := GetVersion();
  RunningOnNT := (OSversion<$80000000);

  FActive := False;
  RedScreenCaption := 'Enter Passwords';

  // Initialise slot file attrib readonly flags
  for i:=0 to MAX_SCRAMDISK_SLOTS do
    begin
    volumeMadeReadonly[i] := FALSE;
    end;

  PreferredDrive:= 'S'; // Default to S:
  NTSDPartitionNames:= TStringList.Create();

  cachedDriverVersion := 0;

end;

destructor TOTFEScramDisk.Destroy;
begin
  if FActive then
    CloseHandle(hScramDiskVxD);

  NTSDPartitionNames.Free();

  inherited Destroy;
end;

function TOTFEScramDisk.Connect(): boolean;
var
  pchDriver : PChar;
  iDriverVersion : ^Integer;
  dwBytesReturned : DWORD;
  cmd: string;
  t: integer;
begin
  Result := Active;
  FLastErrCode:= OTFE_ERR_SUCCESS;
  dwBytesReturned := 0; // Required to get rid of compiler warning

  if not(Active) then
    begin
    if not(RunningOnNT) then
      begin
      hScramDiskVxD := CreateFile('\\.\sd', 0, 0, nil, 0, FILE_FLAG_DELETE_ON_CLOSE, 0);
      if hScramDiskVxD=INVALID_HANDLE_VALUE then
        begin
        // Try traveller mode stuff for wMe/w98

        // This is *weird* - in the v3 ScramDisk source, if ScramDisk can't
        // find the ScramDisk driver at this stage, it messes around with the
        // path to the ScramDisk executable - AND THEN OVERWRITES IT?!!
        cmd := '\\.\';
        cmd := cmd + 'SD.vxd';

        hScramDiskVxD := CreateFile(PChar(cmd), 0, 0, nil, CREATE_NEW, 0, 0);

        if (hScramDiskVxD<>INVALID_HANDLE_VALUE) then
          begin
          SDXDeviceIoControl(hScramDiskVxD, TRAVELLERMODE, @t, 0, nil, 0, dwBytesReturned, nil);
          t := 1;
          end;

        end;
      end
    else
      begin
      hScramDiskVxD := CreateFile('\\.\SD', 0, (FILE_SHARE_READ or FILE_SHARE_WRITE), nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if hScramDiskVxD=INVALID_HANDLE_VALUE then
        begin
        hScramDiskVxD := NTLoadLocalDriver();
        end;
      end;


    if hScramDiskVxD=INVALID_HANDLE_VALUE then
      begin
      FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
      raise ESDVxdNotFound.Create('Scramdisk device driver not present'+CRLF+CRLF+
                                  'Please ensure that ScramDisk is installed correctly.'+CRLF+CRLF+
                                  'If you have just installed Scramdisk without'+CRLF+
                                  'restarting the computer you may need to restart it now'+CRLF+
                                  'if you wish to use Scramdisk');
      end;

    SDXDeviceIoControl(hScramDiskVxD,
                       GETDRIVERVERSION,
                       @pchDriver,
                       0,
                       @iDriverVersion,
                       0,
                       dwBytesReturned,
                       nil);
    if iDriverVersion^ < $202 then
      begin
      raise ESDWrongVersion.Create('The ScramDisk driver installed is out of date'+CRLF+
                                   'Please install a later version of Scramdisk');
      FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
      end;

    Result := TRUE;
    GetPasswordBufferInt;
    end;

end;

function TOTFEScramDisk.Disconnect(): boolean;
begin
  FLastErrCode:= OTFE_ERR_SUCCESS;
  if Active then
    begin
    CloseHandle(hScramDiskVxD);
    end;

  NTSDPartitionNames.Clear();
  
  Result := TRUE;

end;




function TOTFEScramDisk.GetSlotInfo(SlotNo : Integer) : TSlotInfo;
var
  dwBytesReturned: DWORD;
  infoStruct: TInfoStruct;
//  Dummy : Char;
  lastMountTimeDate: TSystemTime;
  writeProtect: DWORD;
  mountType: TMountType;
  mountedAs: char;
  fileAttr: integer;
begin
  CheckActive();

  // Ensure that MyCVS is up to date
  UpdateSlotInfo();


  if (Version()>=$300) then
    begin
  mountType := DetermineMountType(MyCVS[slotNo]);
    end
  else
    begin
    mountType := DetermineMountType(slotNo);
    end;

  Result.SlotNo := SlotNo;
  Result.MountType := mountType;

  if mountType=mtNotMounted then
    begin
    exit;
    end;

  // Work out the host drive letter, where appropriate
  Result.HostDriveLetter := #0;
  if (mountType=mtSVLFile) or (mountType=mtWAVFile) then
    begin
    Result.HostDriveLetter := MyCVS[SlotNo].mounted_file_name[0];
    end;

  // Work out the number of bits used if WAV file steganography
  Result.wavFileBits := 0;
  if (mountType=mtWAVFile) then
    begin
    if ((MyCVS[slotNo].wavoffset and $80000000)<>0) then
      begin
      Result.wavFileBits := 4;
      end
    else
      begin
      Result.wavFileBits := 8;
      end;
    end;


  if (Version()>=$300) then
    begin
    // Determine if partition is removable or not
    if (mountType=mtPartition) then
      begin
      Result.partitionRemovable := (MyCVS[slotNo].devicetype=2);

      if ((MyCVS[slotNo].devicetype and 65536)>0) then
        begin
        Result.partitionRemovable := FALSE;
        end;
      end;

    end;

  mountedAs := Chr(MyCVS[SlotNo].drive + 65);
  Result.DriveMountedAs := mountedAs;

  Result.CipherType := CipherType[MyCVS[SlotNo].cipher];
  if mountType=mtPartition then
    begin
    Result.FileName := '';
    if RunningOnNT then
      begin
      Result.Filename := MyCVS[SlotNo].mounted_file_name;
      end
    else
      begin
      Result.Filename := copy(MyCVS[SlotNo].physdevDCB.DCB_vendor_id, 0, sizeof(MyCVS[SlotNo].physdevDCB.DCB_vendor_id));
      Result.Filename := Result.Filename + copy(MyCVS[SlotNo].physdevDCB.DCB_product_id, 0, sizeof(MyCVS[SlotNo].physdevDCB.DCB_product_id));
      end;
    end
  else
    begin
    Result.FileName := MyCVS[SlotNo].mounted_file_name;
    end;


  if ((Version>=$300) and not(RunningOnNT)) then
    begin
    Result.bfs := ((MyCVS[SlotNo].flags and $40000000)>0);
    end
  else
    begin
    Result.bfs := FALSE;
    end;

  // Find out readonly status...
  if Version()<$300 then
    begin
    Result.softReadOnly := FALSE; // ScramDisk v2.02h doesn't support "softreadonly"
    Result.mediaReadOnly := FALSE;
    if ((mountType=mtSVLFile) or (mountType=mtWAVFile)) then
      begin
      fileAttr := FileGetAttr(MyCVS[SlotNo].mounted_file_name);
      Result.mediaReadOnly := ((fileAttr AND faReadOnly)<>0);
      end;

    end
  else
    begin
    // "writeProtect" only gives an indication of whether or not the volume
    // was mounted in "readonly mode". It doesn't tell us if the volume is
    // readonly due to the volume file having it's readonly attrib set, or
    // being stored on readonly media, so we have to work things our
    // ourselves...
    Result.mediaReadOnly := not((MyCVS[SlotNo].flags AND 1)<>0);
    Result.softReadOnly  := ((MyCVS[SlotNo].flags AND 2)<>0);

    end; // if Version()<$300 then

  Result.readonly := Result.softReadOnly or Result.mediaReadOnly;


  infoStruct.SlotNo := SlotNo;
  writeProtect := 1;
  dwBytesReturned := 0; // Required to get rid of compiler warning
  // "writeProtect" was "dummy" (ignored) in ScramDisk v2.02h
  SDXDeviceIoControl(hScramDiskVxD, READINFOBLOCK, @infoStruct, 0, @writeProtect, 0, dwBytesReturned, nil);


  Result.Comments := infoStruct.infotext;
  Result.PreferredDrive := Char(65 + infoStruct.preferredletter);
  Result.NoAccesses := infoStruct.accessed;

  // Determine if mounted via an SKF file
  if (Version()<$300) then
    begin
    Result.viaSKFFile := Pos('.SKF', uppercase(MyCVS[SlotNo].mounted_file_name))=length(MyCVS[SlotNo].mounted_file_name)-4;
    end
  else
    begin
    // Determine if mounted via SKF file
//    Result.viaSKFFile := ((MyCVS[slotNo].devicetype and 65536)>0);
    Result.viaSKFFile := (infoStruct.owner = #0);
    end;

  if (Result.viaSKFFile) then
    begin
    Result.Comments := '<Not available when mounted via an SKF file>';
    end;


  // Next, sort out the date stuff...
  if (infoStruct.TimeValid2<>$67293c87) then
    begin
    lastMountTimeDate.wYear         := 9999;
    lastMountTimeDate.wMonth        := 1;
    lastMountTimeDate.wDay          := 1;
    lastMountTimeDate.wHour         := 0;
    lastMountTimeDate.wMinute       := 0;
    lastMountTimeDate.wSecond       := 0;
    lastMountTimeDate.wMilliseconds := 0;
    end
  else
    begin
    lastMountTimeDate := infoStruct.oldtime;
    end;

  try
    Result.LastAccess := EncodeDate(lastMountTimeDate.wYear,
                                    lastMountTimeDate.wMonth,
                                    lastMountTimeDate.wDay) +
                         EncodeTime(lastMountTimeDate.wHour,
                                    lastMountTimeDate.wMinute,
                                    lastMountTimeDate.wSecond,
                                    lastMountTimeDate.wMilliseconds);
  except
    ;
  end;

end;



procedure TOTFEScramDisk.UpdateSlotInfo;
var
  dwBytesReturned : DWORD;
  c: char;
  dbit: integer;
begin
  CheckActive();

  dwBytesReturned := 0; // Required to get rid of compiler warning
  // sizeof(MyCVS) was hardcoded 55 in ScramDisk v3.xx!
  SDXDeviceIoControl(hScramDiskVxD, PUTCVS, @MyCVS, sizeof(MyCVS), @MyCVS, sizeof(MyCVS), dwBytesReturned, nil);

  VisibleDrives := 0;
  for c:='A' to 'Z' do
    begin
    if GetDriveType(PChar(c + ':\'))<>1 then
      begin
      dbit := (ord(c)-ord('A'));
      VisibleDrives := VisibleDrives OR (1 shl dbit);
      end;
    end;

end;

{ --- Mount routines --- }

function TOTFEScramDisk.PerformMountContainer(Filename: string; clearPasswordsAfterwards: boolean): boolean;
var
  fs : TMountFileStruct;
  mf : ^TMountFileStruct;
  dwBytesReturned : DWORD;
  wavInfo : TWavInfo;
begin
  CheckActive();

  FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;

  Filename := SDUConvertSFNToLFN(Filename);

  mf := @fs;
  mf.delphiUnion.dcb := nil;
  mf.WavOffset := 0;
  mf.fname := PChar(Filename);
  mf.Critical := nil; // SKF mount only

  if CheckWav(Filename, wavInfo)=1 then  //  is it a 16 bit .WAV ?
    begin
    mf.WavOffset:=wavInfo.FileOffset;
    end;

  UpdateDeviceDriverTime(); // MAKE SURE DRIVER KNOWS THE TIME...
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd, MOUNTFILE, mf, 0, mf, 0, dwBytesReturned, nil);

  if mf.mountresult<>3 then // not mounted...
    begin
    if mf.wavoffset<>0 then
      begin
      mf.wavoffset:=mf.wavoffset and $80000000;

      UpdateDeviceDriverTime();  // MAKE SURE DRIVER KNOWS THE TIME...
      SDXDeviceIoControl(hScramDiskVxd, MOUNTFILE, mf, 0, mf, 0, dwBytesReturned, nil);
      end;
    end;

  Result := (mf.MountResult = 3);
  if Result then
    begin
    EjectStop(mf.fname[0], TRUE); // StopEject if file on removable media
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

  if clearPasswordsAfterwards then
    begin
    ClearAllPasswords(TRUE);
    end;

  RefreshApplication; // Refresh the scramdisk application if it is running.
                      // No longer needed in v3, but harmless, so...

end;


procedure TOTFEScramDisk.MountAll;
begin
  MountPartitions();

end;

// !! WARNING !!
// This procedure does *not* call ClearAllPartions - that's for the software
// calling it to do
procedure TOTFEScramDisk.MountPartitions;
var
  dwBytesReturned : DWORD;
begin
  CheckActive();

  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd, MOUNTNEWDISK, nil, 0, nil, 0, dwBytesReturned, nil);
  RefreshApplication;
end;

{ --- Dismount routines --- }

// "brutal" is "emergency" dismount
function  TOTFEScramDisk.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
//procedure TOTFEScramDisk.Dismount(Drive : string; Brutal : Boolean);
var
  SlotNo : Integer;
  ExamineSlot : Integer;
  slotInfo: TSlotInfo;
  NTBrutal: integer;
begin

  if RunningOnNT then
    begin
    GetVolumeInfo(driveLetter, slotInfo);

    NTBrutal := 0;
    if emergency then
      begin
      NTBrutal := 1;
      end;
      
    Result := NTDoDeviceCloseNT(slotInfo.SlotNo, NTBrutal);
    exit;
    end;

  CheckActive();
  FLastErrCode:= OTFE_ERR_SUCCESS;

  Result := FALSE;

  driveLetter := upcase(driveLetter);

  if not(emergency) then
    begin
    if not LockDrive(driveLetter, 1) then
      begin
//      raise ESDDriveInUse.Create(E_DRIVE_IN_USE);
      // Result is already FALSE, so just exit
      exit;
      end;
    end;

  // Find the slot number of the drive
  SlotNo := -1;
  UpdateSlotInfo;
  for ExamineSlot := 0 to MAX_SCRAMDISK_SLOTS do
    begin
    slotInfo := GetSlotInfo(ExamineSlot);
    if (slotInfo.DriveMountedAs = driveLetter) AND
       (slotInfo.MountType<>mtNotMounted) then
      begin
      SlotNo := ExamineSlot;
      break;
      end;
    end;

  BrutalDismountSlot(SlotNo);

  if volumeMadeReadonly[SlotNo] then
    begin
    SetReadonlyAttrib(GetSlotInfo(SlotNo).Filename, FALSE);
    volumeMadeReadonly[SlotNo] := FALSE;
    end;

  Result := (Pos(driveLetter, DrivesMounted())<1);

  if SlotNo=-1 then
    begin
    FLastErrCode:= OTFE_ERR_INVALID_DRIVE;
    end
  else if not(Result) then
    begin
    FLastErrCode:= OTFE_ERR_DISMOUNT_FAILURE;
    end;

end;

procedure TOTFEScramDisk.Dismount(SlotNo : Integer; Brutal : Boolean);
begin
  CheckActive();

  UpdateSlotInfo();
  if (GetSlotInfo(SlotNo).MountType<>mtNotMounted) then
    begin
    Dismount(GetSlotInfo(SlotNo).DriveMountedAs[1], Brutal);
    end;

end;

procedure TOTFEScramDisk.BrutalDismountSlot(SlotNo : Integer);
var
  dwBytesReturned : DWORD;
  CloseMode : Integer;
  LocalSlotNo : Integer; //local copy of SlotNo for passing to SD.VXD
begin
  CheckActive();

  CloseMode := 0; // preclose
  LocalSlotNo := SlotNo;
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd,
                  CLOSE_CRYPTED_DEVICE_N,
                  @CloseMode,
                  0,
                  @LocalSlotNo,
                  0,
                  dwBytesReturned,
                  nil);

  if (CloseMode <> 1) then
    begin
    CloseMode := 1;
    LocalSlotNo := SlotNo;
    SDXDeviceIoControl(hScramDiskVxd,
                    CLOSE_CRYPTED_DEVICE_N,
                    @CloseMode,
                    0,
                    @LocalSlotNo,
                    0,
                    dwBytesReturned,
                    nil);
    end;

  RefreshApplication;

end;

{ --- WAV mounting functions --- }

function TOTFEScramDisk.FindWavData(fs : TFileStream; var wavInfo: TWavInfo) : Integer;
const
  STR_RIFF = 'RIFF';
  STR_WAVEFMT = 'WAVEfmt ';
  STR_DATA = 'data';
var
  c2 : DWORD;
  loop: integer;
  readIn: array [1..10] of char;
  dataFound: boolean;
begin
  wavInfo.WavType := $FFFF;
  wavInfo.BitsPerSample := $FFFF;
  wavInfo.NumChannels := $FFFF;

  fs.Read(readIn, 8);
  for loop:=1 to length(STR_RIFF) do
    begin
    if readIn[loop]<>STR_RIFF[loop] then
      begin
      Result := -1;
      exit;
      end;
    end;

  fs.Read(readIn, 8);
  for loop:=1 to length(STR_WAVEFMT) do
    begin
    if readIn[loop]<>STR_WAVEFMT[loop] then
      begin
      Result := -1;
      exit;
      end;
    end;

  c2 := 16;

  GetDWordFromStream(fs); // Length Of FORMAT Chunk (Binary, always 0x10) - ignored
  wavInfo.WavType := GetWordFromStream(fs); // Always 0x01
  wavInfo.numchannels := GetWordFromStream(fs); // Channel Numbers (Always 0x01=Mono, 0x02=Stereo)
  GetDWordFromStream(fs); // Sample Rate (Binary, in Hz)
  GetDWordFromStream(fs); // Bytes Per Second
  GetWordFromStream(fs); // Bytes Per Sample: 1=8 bit Mono, 2=8 bit Stereo or 16 bit Mono, 4=16 bit Stereo
  wavInfo.bitspersample := GetWordFromStream(fs); // Bits Per Sample

  fs.Seek(c2, soFromBeginning);
  c2 := c2 + 4;
  c2 := c2 + GetDWordFromStream(fs);

  dataFound := FALSE;
  while not(dataFound) do
    begin
    if (c2>64000) then
      begin
      Result := -1;
      exit;
      end;
    fs.Seek(c2, soFromBeginning);
    fs.Read(readIn, length(STR_DATA));

    dataFound := TRUE;
    for loop:=1 to length(STR_DATA) do
      begin
      dataFound := dataFound AND (readIn[loop]=STR_DATA[loop]);
      end;

    if not(dataFound) then
      begin
      fs.Seek((c2 + 4), soFromBeginning);
      c2 := c2 + GetDWordFromStream(fs);
      c2 := c2 + 8;
      end;
    end;


  c2 := c2 + 4;
  fs.Seek(c2, soFromBeginning);
  wavInfo.length := GetDWordFromStream(fs);
  c2 := c2 + 4;

  // DataPointer is never actually used, so we won't bother filling it in
  // wav.DataPointer := (char * ) c2;
  wavInfo.fileoffset := c2;
  Result := 0;

end;

function TOTFEScramDisk.CheckWav(Filename: string; var wavInfo: TWavInfo) : Integer;
var
  valid : Integer;
  TestFile : TFileStream;
  ig: string;
begin
  Result := 0;
  ig := Copy(FileName, Length(FileName) - 3, 4);
  if Copy(FileName, Length(FileName) - 3, 4) = '.wav' then
    begin
    TestFile := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      valid := FindWavData(TestFile, wavInfo);
    finally
      TestFile.Free;
    end;

    if (valid=0) AND
       (wavInfo.BitsPerSample = 16) and (wavInfo.WavType = 1) and (wavInfo.Length > (2048 * 1024)) then
      begin
      Result := 1
      end
    else
      begin
      Result := -1;
      end;

    end; //if Copy(FileName, Length(FileName) - 3, 3) = '.wav' then

end;

{ --- Password routines - to be called by the client application --- }

function TOTFEScramDisk.SetPasswordsRedScreen(ScreenTitle : string) : Boolean;
var
  dwBytesReturned : DWORD;
  messagestr : PChar;
  accepted : LongInt;
begin
  CheckActive();

  messagestr := PChar(ScreenTitle);
  accepted := 256; //Pre-condition to ensure a result
  GetPasswordBufferInt;
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxD,
                     GETINKEY,
                     messagestr,
                     0,
                     @accepted,
                     0,
                     dwBytesReturned,
                     nil);

  if (accepted=1) then
    begin
    PassPasswords;
    end;

  Result := (accepted=1);

end;


procedure TOTFEScramDisk.SetPasswords(Password1, Password2, Password3, Password4 : string);
begin
  SetPasswords(FALSE, Password1, Password2, Password3, Password4);
end;


procedure TOTFEScramDisk.SetPasswords(skipActiveCheck: boolean; Password1, Password2, Password3, Password4 : string);
var
  Passwords : TMemoryStream;
  Blank : Char;
  n : Integer;
begin
  if not(skipActiveCheck) then
    begin
    CheckActive();
    end;

  Passwords := TMemoryStream.Create;
  try
    Blank := #0;
    for n := 0 to 159 do
      begin
      Passwords.Write(Blank, 1);
      end;
    Passwords.Position := 0;
    Passwords.Write(Password1[1], length(Password1));
    Passwords.Position := 40;
    Passwords.Write(Password2[1], length(Password2));
    Passwords.Position := 80;
    Passwords.Write(Password3[1], length(Password3));
    Passwords.Position := 120;
    Passwords.Write(Password4[1], length(Password4));
    GetPasswordBufferInt;
    CopyMemory(pThePasswords, Passwords.Memory, 160);
    PassPasswords;
  finally
    Passwords.Free;
  end;
end;

// Clear down the cached passwords
procedure TOTFEScramDisk.ClearAllPasswords();
begin
  ClearAllPasswords(FALSE);

end;

// Clear down the cached passwords
procedure TOTFEScramDisk.ClearAllPasswords(skipActiveCheck: boolean);
var
  junkPassword : array [1..4] of string;
  i : integer;
  j : integer;
  dwBytesReturned: DWORD;
begin
  if not(skipActiveCheck) then
    begin
    CheckActive();
    end;

  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd, CLEARPASSWORDS, nil, 0, nil, 0, dwBytesReturned, nil);

  // 4 passwords, each of 40 chars long = complete password; 160 chars
  for i:=1 to 4 do
    begin
    junkPassword[i] := '';
    randomize;
    for j:=0 to 39 do
      begin
      junkPassword[i] := junkPassword[i] + chr(random(255));
      end;
    end;

  SetPasswords(TRUE,
               junkPassword[1],
               junkPassword[2],
               junkPassword[3],
               junkPassword[4]);
  SetPasswords(TRUE, '', '', '', '');

  RefreshApplication;
  inherited;

end;
{ --- Internal password routines - not to be called by the client application --- }

procedure TOTFEScramDisk.GetPasswordBufferInt;
var
  dwBytesReturned : DWORD;
begin
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxD,
                     GETPASSWORDBUFFER,
                     @pThePasswords,
                     0,
                     @pThePasswords,
                     0,
                     dwBytesReturned,
                     nil);
end;

procedure TOTFEScramDisk.PassPasswords;
var
  dwBytesReturned : DWORD;
  hashObj: THashAlgSHA1;
  PasswordDigest: THashArray;
  tempPasswordDigest: THashArray;
  i: integer;
  j: integer;
begin
  //Called internally...no need to check FActive
  hashObj := THashAlgSHA1.Create(nil);
  try
    tempPasswordDigest := hashObj.HashMemory(pThePasswords, 160);

    // OK, because the driver effectively expects the hashed passwords to
    // be passed to it as a set of 4 DWORDs, we need to setup the digest
    // in the format:
    // byte# 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12, 19, 18, 17, 16
    for i:=0 to 4 do
      begin
      for j:=0 to 3 do
        begin
        passwordDigest[(i*4)+j] := tempPasswordDigest[(i*4)+(3-j)];
        tempPasswordDigest[(i*4)+(3-j)] := Random(256);
        end;
      end;

    UpdateDeviceDriverTime();

    // send both the password and SHA1 digest to the device driver
    dwBytesReturned := 0; // Required to get rid of compiler warning
    SDXDeviceIoControl(hScramDiskVxd, SUPPLY_PASSWORD, pThePasswords, 160, @passwordDigest, 20,
      dwBytesReturned, nil);

  finally
    hashObj.Free();
  end;

  for i:=low(passwordDigest) to high(passwordDigest) do
    begin
    passwordDigest[i] := byte(Random(256));
    end;

  RefreshApplication;
end;

{ --- General routines --- }

procedure TOTFEScramDisk.RefreshApplication;
var
  dwBytesReturned : DWORD;
begin
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd, APP_REQUEST_REFRESH, nil, 0, nil, 0, dwBytesReturned, nil);
end;


{ --- Internal routines - not to be called by client applications --- }

function TOTFEScramDisk.EjectStop(DriveLetter : string; EjectFunction : Boolean) : Boolean;
var
  hVWin32VxD : THandle;
  Registers : TDeviceIOControlRegisters;
  EjectResult : Boolean;
  dwBytesReturned : DWORD;
  LockFunction : Integer;
  DriveNumber : Integer;
  ParamBlock : TParamBlock;
  DriveChar : Char;
begin
  DriveLetter := Uppercase(DriveLetter);
  DriveChar := DriveLetter[1];
  DriveNumber := (Ord(DriveChar) - 64);
  LockFunction := $48;

  if DriveLetter=#0 then
    begin
    Result := FALSE;
    exit;
    end;

  if (RunningOnNT) then
    begin
    NTEjectStopNT(Driveletter[1], EjectFunction);
    end;

  if EjectFunction = TRUE then
    ParamBlock.Operation := 0 //lock
  else
    ParamBlock.Operation := 1; //unlock

  hVWin32VxD := CreateFile('\\.\vwin32', 0, 0, nil, 0, FILE_FLAG_DELETE_ON_CLOSE, 0);
  Registers.EAX := $440D;
  Registers.EBX := DriveNumber;
  Registers.ECX := $0800 or LockFunction;
  Registers.EDX := DWORD(@ParamBlock);
  Registers.Flags := $0001;
  dwBytesReturned := 0; // Required to get rid of compiler warning
  EjectResult := SDXDeviceIoControl(hVWin32Vxd,
                                    VWIN32_DIOC_DOS_IOCTL,
                                    @Registers,
                                    SizeOf(Registers),
                                    @Registers,
                                    SizeOf(Registers),
                                    dwBytesReturned,
                                    nil);
  if not EjectResult or ((Registers.Flags and $0001) <> 0) then
    CloseHandle(hVWin32Vxd);
  Result := (Registers.Flags and $1) = 0;
end;


procedure TOTFEScramDisk.NTEjectStopNT(DriveLetter: char; EjectFunction: boolean);
var
  volEjectName: string;
  func: integer;
begin
  if EjectFunction then
    begin
    func := 0;
    end
  else
    begin
    func := 1;
    end;

  volEjectName := '\??\'+DriveLetter+':';

  // 512 passed as the length of volEjectName? Youch! That sounds dangerous,
  // but that's what it was in the original ScramDisk code, so...
  SDXDeviceIoControl(hScramDiskVxD,
                     SDISK_NT_EJECT_STOP,
                     PChar(volEjectName),
                     512,
                     @func,
                     4,
                     0,
                     nil);
                     
end;


procedure TOTFEScramDisk.UpdateDeviceDriverTime;
var
  s : TSystemTime;
  dwBytesReturned : DWORD;
begin
  GetLocalTime(s);
  dwBytesReturned := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hScramDiskVxd, SUPPLYDATEANDTIME, @s, 0, @s, 0, dwBytesReturned, nil);
  
end;

procedure TOTFEScramDisk.SetActive(AValue : Boolean);
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

    if Active and RunningOnNT then
      begin
      NTReadAllPartitionsNT();
      end;
    end;

end;

function TOTFEScramDisk.IOCtlLock(DriveNo : Integer; Permissions : Integer; FunctionNo : Integer) : Integer;
var
  hDevice : THandle;
  Reg : TDeviceIOControlRegisters;
  cb : DWORD;
  lockfunc : Integer;
begin

  if FunctionNo <> 0 then
    lockfunc := $4A
  else
    lockfunc := $6A;

  hDevice := CreateFile('\\.\vwin32', 0, 0, nil, 0, FILE_FLAG_DELETE_ON_CLOSE, 0);

  reg.EAX := $440D;
  reg.EBX := DriveNo;
  reg.ECX := $0800 or lockfunc;
  reg.EDX := permissions;
  reg.Flags := $0001;


  cb := 0; // Required to get rid of compiler warning
  SDXDeviceIoControl(hDevice, VWIN32_DIOC_DOS_IOCTL, @reg, sizeof(reg),
    @reg, sizeof(reg), cb, nil);

  CloseHandle(hDevice);

  Result := reg.Flags and 1;
end;


// DriveNo is indexed from 1
// i.e. A: = 1, B: = 2, etc...
function TOTFEScramDisk.LockLogDrive(DriveNo, Mode: Integer) : Integer;
var
  a : Integer;
begin
  if (mode > 0) then
    begin
      IOCtlLock(DriveNo, 0, 1);
      a := IOCtlLock(DriveNo, 4, 1);
    end
  else
    begin
      a := 0;
      IOCtlLock(DriveNo, 0, 0);
      IOCtlLock(DriveNo, 0, 0);
    end;

  Result := a;
end;

function TOTFEScramDisk.LockDrive(Drive : string; Mode : Integer) : Boolean;
var
  DriveLett : Integer;
  a : Integer;
  UpCaseDrive : string;
begin
  UpCaseDrive := Uppercase(Drive);
  DriveLett := Ord(UpCaseDrive[1]);
  a := 1;
  DriveLett := DriveLett - ord('A');
  Inc(Drivelett);

  if ((drivelett > 1) and (drivelett < 27)) then
    a := LockLogDrive(drivelett, mode);
  Result := (a <> 1);
end;

function TOTFEScramDisk.GetWordFromStream(ts: TFileStream): WORD;
var
  word: array [1..2] of byte;
begin
  ts.Read(word, 2);
  Result := (word[2]*$FF)+word[1];
end;

function TOTFEScramDisk.GetDWordFromStream(ts: TFileStream): DWORD;
var
  a, b: WORD;
begin
  a := GetWordFromStream(ts);
  b := GetWordFromStream(ts);
  Result := (b*$FFFF) + a;
end;

// Prompt the user for the passwords, and the mount the specified volume
// [IN] volumeFilename - the filename of the volume to be mounted
// [IN] readonly - TRUE to mount as readonly, otherwise set to FALSE
// Returns TRUE on success, otherwise FALSE
function  TOTFEScramDisk.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  stlTemp: TStringList;
  mountedAs: string;
begin
  stlTemp := TStringList.Create();
  try
    stlTemp.Add(volumeFilename);
    if Mount(stlTemp, mountedAs, readonly) then
      begin
      Result := mountedAs[1];
      end
    else
      begin
      Result := #0;
      end;
  finally
    stlTemp.Free();
  end;

end;

function  TOTFEScramDisk.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
var
  passwordsEntered: boolean;
  i: integer;
  tmpDrv: char;
  mountedOK: boolean;
begin
  Result := FALSE;

  if (FUseRedScreen and (not(RunningOnNT)))then
    begin
    passwordsEntered := SetPasswordsRedScreen(FRedScreenCaption);
    end
  else
    begin
    passwordsEntered := EnterPasswordsDialog();
    end;

  if passwordsEntered=TRUE then
    begin
    mountedAs := '';
    for i:=0 to (volumeFilenames.count-1) do
      begin
      mountedOK := MountContainer(volumeFilenames[i], readonly, FALSE);
      Result := Result OR mountedOK;

      tmpDrv := GetDriveForVolFile(volumeFilenames[i]);
      if tmpDrv=#0 then
        begin
        FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
        end;
      mountedAs := mountedAs + tmpDrv;
      end;
    end
  else
    begin
    FLastErrCode:= OTFE_ERR_USER_CANCEL;
    end;

  ClearAllPasswords(TRUE);

end;

// Maintaining compatability with TEnhKrScramDisk
function TOTFEScramDisk.MountPrompted(volumeFilenames: TStrings; readonly: boolean) : boolean;
var
  mountedAs: string;
begin
  Result := Mount(TStringList(volumeFilenames), mountedAs, readonly);

end;

function TOTFEScramDisk.MountPrompted(volumeFilename: string; readonly: boolean): boolean;
begin
  Result := (Mount(volumeFilename, readonly)<>#0);

end;

// Display dialog to get passwords from the user
function TOTFEScramDisk.EnterPasswordsDialog() : boolean;
const
  ANTI_SWAP = 512*1024;
var
  passwordEntryDlg : TOTFEScramDiskPasswordEntry_F;
  memoryBlock: Pointer;
begin
  CheckActive();
  Result := FALSE;

  // Grab and release (say) 512K memory and then release it to try and reduce
  // risk of swapping memory while the form collects the passwords
  memoryBlock := AllocMem(ANTI_SWAP);
  FreeMem(memoryBlock, ANTI_SWAP);

  passwordEntryDlg := TOTFEScramDiskPasswordEntry_F.create(nil);
  try
    passwordEntryDlg.ckHidePasswords.visible := not(FHidePasswordsEnteredCk);
    if (passwordEntryDlg.showmodal=mrOK) then
      begin
      SetPasswords(TRUE,
                   passwordEntryDlg.mePassword1.text,
                   passwordEntryDlg.mePassword2.text,
                   passwordEntryDlg.mePassword3.text,
                   passwordEntryDlg.mePassword4.text);
      Result := TRUE;
      end
    else
      begin
      FLastErrCode:= OTFE_ERR_USER_CANCEL;
      end;

    passwordEntryDlg.ClearEnteredPasswords();
  finally
    passwordEntryDlg.free;
  end;

end;

// Enhanced MountContainer, allows mounting of a container file as
// readonly
// Returns: TRUE on success, otherwise FALSE
function TOTFEScramDisk.MountContainer(filename: string; readonly: boolean; clearPasswordsAfterwards: boolean): boolean;
var
  fileAttributes : integer;
  setOK : boolean;
  readonlyToggled : boolean;
  i : integer;
  slotInfo : TSlotInfo;
begin
  CheckActive();

  Result := TRUE;
  readonlyToggled := FALSE;

  fileAttributes := FileGetAttr(filename);

  if (((fileAttributes AND faReadOnly)=0) AND readonly) then
    begin
    // No error checking when setting the file attribute - if the file
    // is already readonly, we expect an error (e.g. if the media the
    // file is stored on is readonly like a CDROM).
    // This is pretty naff, but it'll do for now
    setOK := SetReadonlyAttrib(filename, TRUE);
    if setOK then
      begin
      readonlyToggled := TRUE;
      end
    else
      begin
      Result := FALSE;
      end;

    end;

  // Mount the container if all OK up to here
  if Result=TRUE then
    begin
    Result := PerformMountContainer(filename, clearPasswordsAfterwards);

    // Finally, set a flag if the readonly attrib was changed to readonly,
    // or clear it if there was a problem in mounting the container
    if (readonlyToggled=TRUE) then
      begin
      if (Result=TRUE) then
        begin
        UpdateSlotInfo();
        for i:=0 to MAX_SCRAMDISK_SLOTS do
          begin
          slotInfo := GetSlotInfo(i);
          if slotInfo.Filename=filename then
            begin
            volumeMadeReadonly[i] := TRUE;
            end;
          end;
        end
      else
        begin
        // If we get to here, there was a problem mounting the container
        // and we toggled the readonly attrib - so change it back
        SetReadonlyAttrib(filename, FALSE);
        end;
      end; // Result was = 0
    end;
end;

// Set/clear the readonly attribute on a file
// filename    - file for which attribute is to be changed
// setNotUnset - TRUE to set readonly, otherwise FALSE to clear
function TOTFEScramDisk.SetReadonlyAttrib(filename: string; setNotUnset: boolean) : boolean;
var
  fileAttributes : integer;
begin
  fileAttributes := FileGetAttr(filename);
  if setNotUnset then
    begin
    fileAttributes := (fileAttributes or faReadOnly)
    end
  else
    begin
    fileAttributes := fileAttributes AND not(faReadOnly);
    end;

  Result := (FileSetAttr(filename, fileAttributes) = 0);
end;

// Return the maximum number of slots possible
function TOTFEScramDisk.MaxSlots() : integer;
begin
  Result := MAX_SCRAMDISK_SLOTS + 1;
end;

function TOTFEScramDisk.NumberMounted() : integer;
begin
  Result := CountDrivesMounted();
end;

function  TOTFEScramDisk.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;

function  TOTFEScramDisk.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  // Always TRUE, unless the file doesn't exist - ScramDisk volume files don't
  // have any signature
  Result := FileExists(volumeFilename);

end;

function  TOTFEScramDisk.DrivesMounted(): string;
var
  slotInfo : TSlotInfo;
  i : integer;
  mtdDrvs: string;
begin
  CheckActive();

  UpdateSlotInfo();
  RefreshApplication();

  mtdDrvs := '';
  for i:=0 to MAX_SCRAMDISK_SLOTS do
    begin
    slotInfo := GetSlotInfo(i);
    if slotInfo.MountType<>mtNotMounted then
      begin
      mtdDrvs := mtdDrvs + slotInfo.DriveMountedAs[1];
      end;
    end;

  Result := SortString(mtdDrvs);

end;


// Get the drive letter for a mounted volume file; or '' if the volume file is
// not mounted
// [IN] volumeFilename - the filename of the volume file to be checked for
// Returns the drive letter of the mounted volume file, or '' if the file is
// not mounted
function TOTFEScramDisk.GetDriveLetterForVolFile(volumeFilename: string): string;
begin
  Result := GetDriveForVolFile(volumeFilename);

end;

function  TOTFEScramDisk.GetDriveForVolFile(volumeFilename: string): char;
var
  i: integer;
  currSlotInfo: TSlotInfo;
begin
  Result := #0;

  volumeFilename := SDUConvertSFNToLFN(volumeFilename);
  UpdateSlotInfo();
  for i:=0 to MAX_SCRAMDISK_SLOTS do
    begin
    currSlotInfo := GetSlotInfo(i);
    if (currSlotInfo.MountType<>mtNotMounted) then
      begin
      if (SDUConvertSFNToLFN(currSlotInfo.Filename) = volumeFilename) then
        begin
        Result := (currSlotInfo.DriveMountedAs)[1];
        end;
      end;
    end;
end;

function  TOTFEScramDisk.GetVolFileForDrive(driveLetter: char): string;
var
  i: integer;
  slotInfo: TSlotInfo;
begin
  Result := '';

  driveLetter := upcase(driveLetter);
  UpdateSlotInfo();
  for i:=0 to MAX_SCRAMDISK_SLOTS do
    begin
    slotInfo := GetSlotInfo(i);
    if (slotInfo.DriveMountedAs = driveLetter) AND
       (slotInfo.MountType<>mtNotMounted) then
      begin
      if slotInfo.MountType<>mtPartition then
        begin
        Result := SDUConvertSFNToLFN(slotInfo.Filename);
        end
      else
        begin
        Result := slotInfo.FileName;
        end;
      end;
    end;

end;

function TOTFEScramDisk.Version(): cardinal;
var
  pchDriver : PChar;
  iDriverVersion : ^Integer;
  dwBytesReturned : DWORD;
begin
  if cachedDriverVersion=0 then
    begin
    CheckActive();

    dwBytesReturned := 0; // Required to get rid of compiler warning
    SDXDeviceIoControl(hScramDiskVxD,
                       GETDRIVERVERSION,
                       @pchDriver,
                       0,
                       @iDriverVersion,
                       0,
                       dwBytesReturned,
                       nil);

    cachedDriverVersion := iDriverVersion^;
    end;

  Result := cachedDriverVersion;

end;



// Returns -1 on failure
function TOTFEScramDisk.GetSlotForDrive(driveLetter: char): integer;
var
  i: integer;
  slotInfo: TSlotInfo;
begin
  Result := -1;

  driveLetter := upcase(driveLetter);
  UpdateSlotInfo();
  for i:=0 to MAX_SCRAMDISK_SLOTS do
    begin
    slotInfo := GetSlotInfo(i);
    if (slotInfo.DriveMountedAs = driveLetter) AND
       (slotInfo.MountType<>mtNotMounted) then
      begin
      Result := i;
      break;
      end;
    end;

end;

function TOTFEScramDisk.GetVolumeInfo(driveLetter: char; var info: TSlotInfo): boolean;
var
  slot: integer;
begin
  Result := FALSE;

  slot := GetSlotForDrive(driveLetter);
  if slot>-1 then
    begin
    info := GetSlotInfo(slot);
    end;

end;

// Display dialog to ask user for a volume file to mount, then prompt
// for password.
// [OUT] drive - set to the drive letter of the volume just mounted
//               (should be a list of driver letters, but still...)
// Returns TRUE on success, otherwise FALSE
function TOTFEScramDisk.MountPrompted(var drive: string) : boolean;
var
  mountFilenameDlg : TOpenDialog;
begin
  CheckActive();

  Result := FALSE;
  drive := '';

  mountFilenameDlg := TOpenDialog.create(nil);
  try
    mountFilenameDlg.Options := [ofFileMustExist, ofPathMustExist, ofAllowMultiSelect];
    mountFilenameDlg.InitialDir := '\';
    if FDefaultPromptDir<>'' then
      begin
      mountFilenameDlg.InitialDir := FDefaultPromptDir;
      end;
    mountFilenameDlg.filter := 'SVL files (*.svl)|*.svl|Wave files (*.wav)|*.wav|All files|*.*';
    mountFilenameDlg.Title := 'Mount Volume';
    if (mountFilenameDlg.execute) then
      begin
      Result := MountPrompted(mountFilenameDlg.files,
                              (ofReadOnly in mountFilenameDlg.Options));
      if Result then
        begin
        drive := GetDriveLetterForVolFile(mountFilenameDlg.filename);
        end;
      end;
  finally
    mountFilenameDlg.free;
    ClearAllPasswords(TRUE);
  end;

end;


// Display dialog to ask user for passwords, and then mount all partitions
// Returns string containing the mounted drives, or empty string on failure
function TOTFEScramDisk.MountPartitionsPrompted(): string;
var
  passwordsEntered : boolean;
  beforeDrvs: string;
  afterDrvs: string;
  i: integer;
begin
  CheckActive();

  Result := '';

  // Get passwords...
  if FUseRedScreen then
    begin
    passwordsEntered := SetPasswordsRedScreen(FRedScreenCaption);
    end
  else
    begin
    passwordsEntered := EnterPasswordsDialog();
    end;

  // ...and mount!
  if passwordsEntered=TRUE then
    begin
    beforeDrvs := DrivesMounted();
    // We need a slight pause (say, 500ms) in here to let the
    // passwords entered take effect before mounting the partitions
    SDUPause(500);

    MountPartitions();
    ClearAllPasswords(TRUE);

    afterDrvs := DrivesMounted();

    for i:=1 to length(afterDrvs) do
      begin
      if pos(afterDrvs[i], beforeDrvs)<1 then
        begin
        Result := Result + afterDrvs[i];
        end;
      end;

    if Result='' then
      begin
      FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
      end;
    end
  else
    begin
    FLastErrCode:= OTFE_ERR_USER_CANCEL;
    end;

end;

// Display a dialog offering the user the ability to dismount any drives
function TOTFEScramDisk.DismountPrompted() : boolean;
var
  dismountDlg: TOTFEScramDiskDismountVolumes_F;
  i: integer;
begin
  CheckActive();

  Result := FALSE;

  if NumberMounted()<1 then
    begin
    showmessage('No ScramDisk volumes mounted');
    end
  else
    begin
    dismountDlg:= TOTFEScramDiskDismountVolumes_F.Create(nil);
    try
      dismountDlg.ScramDiskComponent := self;
      if dismountDlg.showmodal()=mrOK then
        begin
        for i:=0 to (dismountDlg.SelectedDrives.count-1) do
          begin
          Dismount(dismountDlg.SelectedDrives[i], dismountDlg.DismountBrutal);
          end;
        end;
    finally
      dismountDlg.Free();
    end;
  end;

end;

function TOTFEScramDisk.VersionStr(): string;
var
  verNo: cardinal;
  majorVer: integer;
  minorVer: integer;
  minorVerStr: string;
begin
  verNo := Version();
  majorVer := (verNo AND $FF00) div $FF;
  minorVer := (verNo AND $FF);
  minorVerStr := inttostr(minorVer);

  Result := Format('v%d.%.2d', [majorVer, minorVer]);

end;

function TOTFEScramDisk.GetMainExe(): string;
begin
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;
  Result := '';

end;

function TOTFEScramDisk.NTLoadLocalDriver(): THandle;
var
  hdriver: THandle;
  error: DWORD;
  sysDir: string;
  exeDir: string;
  sysDirLen: integer;
begin
//  GetSystemDirectory(path, MAX_PATH);
//  GetSystemDirectory(@sysDir, MAX_PATH);
  SetLength(sysDir, MAX_PATH);
  sysDirLen := GetSystemDirectory(PChar(sysDir), MAX_PATH);
  SetLength(sysDir, sysDirLen);
  sysDir := sysDir + '\Drivers\Sd.Sys';

  if (NTLoadDeviceDriver(SYS_NAME, sysDir, hdriver, error)) then
    begin
    ResetSDRegistry();
    Result := hdriver;
    exit;
    end;


  exeDir := ParamStr(0);
  exeDir := ExtractFilePath(exeDir);
  exeDir := exeDir + '\Sd.Sys';


  if (NTLoadDeviceDriver(SYS_NAME, exeDir, hdriver, error)) then
    begin
    RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD\Security');
    RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD\Enum');
    RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD');
    Result := hdriver;
    end
  else
    begin
    Result := INVALID_HANDLE_VALUE;
    end;

end;



function TOTFEScramDisk.NTUnloadDeviceDriver(Name: string): boolean;
var
  schSCManager: SC_HANDLE;
begin

  schSCManager := OpenSCManager(nil, // machine (NULL == local)
                                nil, // database (NULL == default)
                                SC_MANAGER_ALL_ACCESS // access required
                                );

  StopDriver(schSCManager, Name);
  RemoveDriver(schSCManager, Name);

  CloseServiceHandle(schSCManager);

  Result:= TRUE;

end;


function TOTFEScramDisk.NTLoadDeviceDriver(Name: string; Path: string; var lphDevice: THandle; var Error: DWORD): boolean;
var
  schSCManager: SC_HANDLE;
  okay: boolean;
begin
  schSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);

  if (schSCManager=0) then
    begin
    FLastErrCode:= OTFE_ERR_INSUFFICENT_RIGHTS;
    Result:= FALSE;
    exit;
    end;


  RemoveDriver(schSCManager, Name);


  InstallDriver(schSCManager, Name, Path);


  StartDriver(schSCManager, Name);


  okay := OpenDevice(Name, lphDevice);
  Error := GetLastError();
  CloseServiceHandle(schSCManager);
  Result := okay;

end;




function TOTFEScramDisk.ResetSDRegistry(): boolean;
const
  pridisk: string =' PrimaryDisk';
  sysroot: string = '\SystemRoot\System32\drivers\SD.sys';
var
  dwErrorCode: DWORD;
  theHKey: HKEY;
  dwDisposition: DWORD;
  i: DWORD;
begin

  RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD\Security');
  RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD\Enum');
  RegDeleteKey(HKEY_LOCAL_MACHINE,'System\CurrentControlSet\Services\SD');

  dwErrorCode := RegCreateKeyEx(HKEY_LOCAL_MACHINE,
                                'System\CurrentControlSet\Services\SD',
                                0,
                                '',
                                REG_OPTION_NON_VOLATILE,
                                KEY_ALL_ACCESS,
                                nil, //Security
                                theHKey,
                                @dwDisposition );

  if (dwErrorCode<>ERROR_SUCCESS) then
    begin
    Result := FALSE;
    exit;
    end;

  i:=1;
  RegSetValueEx(theHKey, 'Type', 0, REG_DWORD, @i, 4);
  i:=2;
  RegSetValueEx(theHKey, 'Start', 0, REG_DWORD, @i,4);
  i:=1;
  RegSetValueEx(theHKey, 'ErrorControl', 0, REG_DWORD, @i, 4);
  RegSetValueEx(theHKey, 'Group', 0, REG_SZ, PChar(pridisk), length(pridisk));
  RegSetValueEx(theHKey, 'ImagePath', 0, REG_EXPAND_SZ, PChar(sysroot), length(sysroot));

  RegCloseKey(theHKey);

  Result := TRUE;

end;


function TOTFEScramDisk.StopDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;
var
  schService: SC_HANDLE;
  ret: boolean;
  serviceStatus: SERVICE_STATUS;
begin
  schService := OpenService( SchSCManager, PChar(DriverName), SERVICE_ALL_ACCESS);
  if (schService=0) then
    begin
    Result := FALSE;
    exit;
    end;

  ret := ControlService( schService, SERVICE_CONTROL_STOP, serviceStatus);

  CloseServiceHandle( schService );

  Result := ret;

end;

function TOTFEScramDisk.RemoveDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;
var
  schService: SC_HANDLE;
  ret: boolean;
begin
  schService := OpenService(SchSCManager, PChar(DriverName), SERVICE_ALL_ACCESS);
  if (schService=0) then
    begin
    Result := FALSE;
    exit;
    end;

  ret := DeleteService(schService);

  CloseServiceHandle(schService);

  Result := ret;
end;

function TOTFEScramDisk.InstallDriver(SchSCManager: SC_HANDLE; DriverName: string; ServiceExe: string): boolean;
var
  schService: SC_HANDLE;
begin
  schService := CreateService(SchSCManager,          // SCManager database
                              PChar(DriverName),     // name of service
                              PChar(DriverName),     // name to display
                              SERVICE_ALL_ACCESS,    // desired access
                              SERVICE_KERNEL_DRIVER, // service type
                              SERVICE_DEMAND_START,  // start type
                              SERVICE_ERROR_NORMAL,  // error control type
                              PChar(ServiceExe),     // service's binary
                              nil,                   // no load ordering group
                              nil,                   // no tag identifier
                              nil,                   // no dependencies
                              nil,                   // LocalSystem account
                              nil                    // no password
                              );

  CloseServiceHandle(schService);

  Result := TRUE;

end;



function TOTFEScramDisk.StartDriver(SchSCManager: SC_HANDLE; DriverName: string): boolean;
var
  schService: SC_HANDLE;
  ret: boolean;
  lpServiceArgVectors: PChar;
begin
  schService := OpenService(SchSCManager,
                            PChar(DriverName),
                            SERVICE_ALL_ACCESS
                            );
  if (schService=0) then
    begin
    Result := FALSE;
    exit;
    end;

  lpServiceArgVectors := nil;
  ret := StartService(schService, 0, lpServiceArgVectors);

  ret := ret or
         (GetLastError() = ERROR_SERVICE_ALREADY_RUNNING) or
         (GetLastError() = ERROR_SERVICE_DISABLED);

  CloseServiceHandle(schService);

  Result := ret;

end;


function TOTFEScramDisk.OpenDevice(DriverName: string; var lphDevice: THandle): boolean;
var
  completeDeviceName: string; // TCHAR [64]
  hDevice: THandle;
begin
  if ((GetVersion() AND $FF)>=5) then
    begin
    completeDeviceName := '\\.\Global\'+DriverName;
    end
  else
    begin
    completeDeviceName := '\\.\'+DriverName;
    end;

  hDevice := CreateFile(PChar(completeDeviceName),
                          GENERIC_READ or GENERIC_WRITE,
                          0,
                          nil,
                          OPEN_EXISTING,
                          FILE_ATTRIBUTE_NORMAL,
                          0
                          );
  if (hDevice=INVALID_HANDLE_VALUE) then
    begin
    Result:= FALSE;
    exit;
    end;

  if (lphDevice<>0) then
    begin
    lphDevice := hDevice;
    end
  else
    begin
    CloseHandle(hDevice);
    end;

  Result := TRUE;

end;

// DEBUG: Display a message with the string version of errormessage "e"
{$IFDEF _DEBUG}
procedure TOTFEScramDisk.DEBUG_REPORTERROR(e: integer);
begin
  case e of
    ERROR_ACCESS_DENIED             : showmessage('ERROR_ACCESS_DENIED');
    ERROR_CIRCULAR_DEPENDENCY       : showmessage('ERROR_CIRCULAR_DEPENDENCY');
    ERROR_DUP_NAME                  : showmessage('ERROR_DUP_NAME');
    ERROR_INVALID_HANDLE            : showmessage('ERROR_INVALID_HANDLE');
    ERROR_INVALID_NAME              : showmessage('ERROR_INVALID_NAME');
    ERROR_INVALID_PARAMETER         : showmessage('ERROR_INVALID_PARAMETER');
    ERROR_INVALID_SERVICE_ACCOUNT   : showmessage('ERROR_INVALID_SERVICE_ACCOUNT');
    ERROR_SERVICE_EXISTS            : showmessage('ERROR_SERVICE_EXISTS');
    ERROR_PATH_NOT_FOUND            : showmessage('ERROR_PATH_NOT_FOUND');
    ERROR_SERVICE_ALREADY_RUNNING   : showmessage('ERROR_SERVICE_ALREADY_RUNNING');
    ERROR_SERVICE_DATABASE_LOCKED   : showmessage('ERROR_SERVICE_DATABASE_LOCKED');
    ERROR_SERVICE_DEPENDENCY_DELETED: showmessage('ERROR_SERVICE_DEPENDENCY_DELETED');
    ERROR_SERVICE_DEPENDENCY_FAIL   : showmessage('ERROR_SERVICE_DEPENDENCY_FAIL');
    ERROR_SERVICE_DISABLED          : showmessage('ERROR_SERVICE_DISABLED');
    ERROR_SERVICE_LOGON_FAILED      : showmessage('ERROR_SERVICE_LOGON_FAILED');
    ERROR_SERVICE_MARKED_FOR_DELETE : showmessage('ERROR_SERVICE_MARKED_FOR_DELETE');
    ERROR_SERVICE_NO_THREAD         : showmessage('ERROR_SERVICE_NO_THREAD');
    ERROR_SERVICE_REQUEST_TIMEOUT   : showmessage('ERROR_SERVICE_REQUEST_TIMEOUT');
  end;

end;
{$ENDIF}



// modified deviceIO for winNT
function TOTFEScramDisk.SDXDeviceIoControl(hDevice: THandle;       // handle to device of interest
                                           dwIoControlCode: DWORD; // control code of operation to perform
                                           lpInBuffer: Pointer;    // pointer to buffer to supply input data
                                           nInBufferSize: DWORD;   // size of input buffer
                                           lpOutBuffer: Pointer;   // pointer to buffer to receive output data
                                           nOutBufferSize: DWORD;  // size of output buffer
                                           lpBytesReturned: DWORD; // pointer to variable to receive output byte count
                                           thelpOverlapped: POverlapped  // pointer to overlapped structure for asynchronous operation
                                           ): boolean;
var
  d: TDIOCParams;
  bytes: longword;
  r: boolean;
  strHex: string;
  h: THandle;
  mp: PTMountFileStruct;
  spc, bps, nfc, tc: DWORD;
  strDosName: string;
  strSzDevice: string;
  strDr: string;
begin
  if (RunningOnNT) then
    begin
    d.lpvInBuffer:=lpInBuffer;
    d.cbInBuffer:=nInBufferSize;
    d.lpvOutBuffer:=lpOutBuffer;
    d.cbOutBuffer:=nOutBufferSize;
    d.dwIoControlCode:=dwIoControlCode;
    d.pUserSpace:=@DriverRam; // &DriverRam
    if (d.lpvInBuffer<>nil) then
      begin
      mp:= PTMountFileStruct(d.lpvInBuffer);
      end
    else
      begin
      mp := nil;
      end;

    if (dwIoControlCode=MOUNTFILE) then
      begin
      // WARNING!! SDXDeviceIOControl should never be called with
      // d.lpvInBuffer=nil and dwIoControlCode=MOUNTFILE
      mp^.DrvObjAbandoned:=0;
      strDr := mp^.fname[0]+':\';

      // Aman used VisibleDrives in the ScramDisk v3 system because of problems
      // reported when he used GetLogicalDrives()... So we do to.
      UpdateSlotInfo(); // Ensure that VisibleDrives is up to date.
      mp^.LogicalDrives:=VisibleDrives;


//        test(m->LogicalDrives);


      //FindDriveLetter(3,m->LogicalDrives);


      mp^.mountdrive:=FStartDrive;  // choose preferred


      if ((mp^.LogicalDrives and $3ffffff)=$3ffffff) then
        begin
        FLastErrCode := OTFE_ERR_NO_FREE_DRIVE_LETTERS;
        mp^.mountresult:=3+128;
        Result := FALSE;
        exit;
        end;

      if (strDr[1]<>'\') then
        begin
        GetDiskFreeSpace(PChar(strDr), spc, bps, nfc, tc);
        mp^.bytespersector:=bps;
        end
      else
        begin
        mp^.bytespersector:=512;
        end;

      end;

    if (dwIoControlCode=MOUNTNEWDISK) then
      begin
      NTMountSDPartitions(d.lpvInBuffer);
      Result := TRUE;
      exit;
      end;

    r:= (DeviceIoControl(hDevice, NT_SEND_SD_DEVICE, @d, sizeof(d), @d, sizeof(d), bytes, nil));

    if (dwIoControlCode=MOUNTFILE) then
      begin
      if (mp^.mountresult=3) then
        begin
        strDosName := char(ord('A') + mp^.mountdrive);
        strDosName := strDosName + ':' + #0;
        strSzDevice := '\Device\SDMOUNT'+strDosName[1];

        strHex := lowercase(inttohex(mp^.DrvObjAbandoned, 1));
        strSzDevice := strSzDevice + strHex;
        // Tut, tut! We don't actually check the return value of the DosDevice call!
        // Well, Aman doens't, so why should we?
        DefineDosDevice(DDD_RAW_TARGET_PATH, PChar(strDosName), PChar(strSzDevice));

        MountNTVol(char(ord('A') + mp^.mountdrive));

        h:=openread(strDosName);
        if (h<>INVALID_HANDLE_VALUE) then
          begin
          CloseHandle(h);
          end;

        end
      else
        begin
        if (mp^.delphiUnion.NtRealError=$8007) then
          begin
          mp^.mountresult:=3+128; //{breaks out of loop}
          FLastErrCode:= OTFE_ERR_UNABLE_MOUNT_COMPRESSED;
          end;
        end;
      end;

    Result := r;
    exit;

    end;

  // Otherwise, we're not running under NT, so just call the standard
  // DeviceIOControl
  Result := DeviceIoControl(hDevice,
                            dwIoControlCode,
                            lpInBuffer,
                            nInBufferSize,
                            lpOutBuffer,
                            nOutBufferSize,
                            lpBytesReturned,
                            thelpOverlapped);

end;




procedure TOTFEScramDisk.NTMountSDPartitions(crit: PChar);
var
  i: integer;
  mf: TMountFileStruct;
begin

  for i:=0 to (NTSDPartitionNames.count-1) do
    begin
    mf.fname := PChar(NTSDPartitionNames[i]);
    mf.wavoffset:=0;
    mf.critical := crit;

    SDXDeviceIoControl(hScramDiskVxD, MOUNTFILE, @mf, 0, @mf, 0, 0, nil);
    end;

end;


// xxx - this should be in a different file? (NT specific)
// To get the OS to mount the disk just needs the read of the boot sector!!
procedure TOTFEScramDisk.MountNTVol(drive: char);
var
  buffer: array [0..511] of char;
  strVolMountName: string;
  i: integer;
begin
  for i:=low(buffer) to high(buffer) do
    begin
    buffer[i] := #0;
    end;
  strVolMountName:= '\??\'+drive+':'+#0;
  // 55 is a MAGIC NUMBER!!! Yell at Aman - he had 55 hardcoded
  // In practice, this is irrelevant since ScramDisk doesn't so sanity checking
  // on things passed in
  SDXDeviceIoControl(hScramDiskVxD, READBOOTSECTOR, PChar(strVolMountName), 55, @buffer[0], 55, 0, nil);

end;


// xxx - this should probably be in a different file (stegansound)
function TOTFEScramDisk.openread(filename: string): THandle;
var
  h: THandle;
begin
  h:=CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  Result := h;
end;



function TOTFEScramDisk.IsDriveReadonly(driveLetter: char): boolean;
var
  slotNo: integer;
  slotInfo: TSlotInfo;
begin
  slotNo := GetSlotForDrive(driveLetter);
  slotInfo:= GetSlotInfo(slotNo);
  Result := slotInfo.readonly;

end;


// DetermineMountType for ScramDisk v2.02
function TOTFEScramDisk.DetermineMountType(slotNo: integer): TMountType;
var
  infoslot : array [0..8] of char; // Yes, 8 is a magic number. Basically, this
                                   // needs to be big enough to hold the data
                                   // returned from the driver
  dwBytesReturned: DWORD;
begin
  Result := mtNotMounted;

  UpdateSlotInfo();

  if (Version()>=$300) then
    begin
    Result := DetermineMountType(MyCVS[slotNo]);
    exit;
    end
  else
    begin
    infoslot[0] := char(slotNo);
    dwBytesReturned := 0; // Required to get rid of compiler warning
    SDXDeviceIoControl(hScramDiskVxD, INQUIRE_SLOT, @infoslot, 0, nil, 0, dwBytesReturned, nil);

    if (Ord(InfoSlot[1]) <= 3) and
       (Ord(InfoSlot[1]) >= 1) then // 1-3 = the 3 different types in
                                    // ScramDisk v2.02h
      begin
      case Ord(InfoSlot[1]) of
        1: Result := mtPartition;
        2: Result := mtSVLFile;
        3: Result := mtWAVFile;
      end;
      end;

    end;

end;

// DetermineMountType for ScramDisk v3+
function TOTFEScramDisk.DetermineMountType(cv: TCryptVol): TMountType;
begin
  Result := mtNotMounted;

  if (Version()<$300) then
    begin
    raise EOTFEException.Create('TOTFEScramDisk.DetermineMountType may only be called with a TCryptVol with ScramDisk v3 or later');
    end
  else
    begin
    if (cv.booted=0) then
    // Alternative:
    // if cv.drive=0
      begin
      exit;
      end;

    Result := mtUnknown;

    // Note that we can't use ((cv.devicetype and 65536)<>0) to determine if
    // it's a partition that's mounted - this fails under WinME (and probably
    // Win9x as well)
    if ((cv.devicetype=1) or (cv.devicetype=2)) then
      begin
      // HDD/removable partition
      Result := mtPartition;
      end
    else if ((cv.devicetype and 256)<>0) then
      begin
      // SVL File
      Result := mtSVLFile;
      end;

    // WAV file overrides
    if (cv.wavoffset<>0) then
      begin
      Result := mtWAVFile;
      end;

    if (cv.timeoutcount=$ffffffff) then
      begin
      Result := mtTimedOut;
      end;

    end;

end;


// xxx - what happens if volume mounted as readonly attrib set by component - what happens when this attrib is cleared?
procedure TOTFEScramDisk.ResetTimeStamp(filename: string);
var
  hFile: THandle;

  CT: FILETIME;
  AT: FILETIME;
  WT: FILETIME;
begin

  hFile:=CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

  if (hFile=INVALID_HANDLE_VALUE) then
    begin
    exit;
    end;

  GetFileTime(hFile,  // identifies the file
              @CT,    // address of creation time
              @AT,    // address of last access time
              @WT     // address of last write time
              );

  CloseHandle(hFile);

  WT.dwLowDateTime  := CT.dwLowDateTime;
  WT.dwHighDateTime := CT.dwHighDateTime;
  AT.dwLowDateTime  := CT.dwLowDateTime;
  AT.dwHighDateTime := CT.dwHighDateTime;

  hFile:=CreateFile(PChar(filename), GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);


  SetFileTime(hFile,  // identifies the file
              @CT,    // time the file was created
              @AT,    // time the file was last accessed
              @WT     // time the file was last written
              );

  CloseHandle(hFile);

end;



// xxx - put NT dismount stuff in a different file?
function TOTFEScramDisk.NTCheckStreaming(slotNo: integer): boolean;
var
  slotnum: integer;
  closemode: integer;
  i: integer;
begin
  slotnum := slotNo;
  closemode:=0;

  SDXDeviceIoControl(hScramDiskVxD, CLOSE_CRYPTED_DEVICE_N, @closemode, 0, @slotnum, 0, 0, nil);

  if (closemode<>0) then
    begin

    // Attempt operation 10 times, sleeping for 200ms between attempts
    for i:=0 to 9 do
      begin
      sleep(200);
      closemode:=0;
      SDXDeviceIoControl(hScramDiskVxD, CLOSE_CRYPTED_DEVICE_N, @closemode, 0, @slotnum, 0, 0, nil);
      if (closemode=0) then
        begin
        Result := FALSE;
        exit;
        end;
      end;


    FLastErrCode := OTFE_ERR_STREAMING_DATA;

    Result := TRUE;
    exit;
    end;

  Result := FALSE;
  
end;


// xxx - put NT dismount stuff in a different file?
// Call with brutal = 1 to dismount brutal
// Call with brutal = 0 to dismount normal
// THIS FUNCTION SHOULD ONLY BE CALLED BY "NTDoDeviceCloseNT"
function TOTFEScramDisk.NTDismountVolume(brutal: integer; drv: integer; slot: integer): boolean;
var
  bResult: integer; // yes, integer, not DWORD
  strChkopName: string;
  prevErrorMode: cardinal;
  finished: boolean;
begin
  Result := FALSE;

  strChkopName := '\DosDevices\'+char(drv+ord('A'))+':';

  bResult:=SDCHECKOPENFILES;
  //	SDXDeviceIoControl(hScramDiskVxD, CHECKOPENFILESORDISMOUNTDISK, &ChkopName, 0, &bResult, 0, nil, nil);
  SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(strChkopName), slot, @bResult, 0, 0, nil);

  if (bResult>=0) then
    begin
    bResult:=1;
    end
  else
    begin
    bResult:=0;
    end;

  if ((bResult=0) and (brutal=0))  then
    begin
    // Result already = FALSE; just exit
    exit;
    end;

  finished := FALSE;


  // When dismounting under NT (ScramDisk v3), everything seems fine the first
  // time a volume is dismounted, but the second time an error dialog pops up
  // titled "<application name> - Wrong Volume" stating:
  //
  //   The wrong volume is in the drive. Please insert volume into drive
  //   \\Device\SDMOUNTYb4d20e82
  //
  // Pressing either of the Abort/Retry/Ignore buttons has the same effect -
  // the dialog goes away, and everything seems OK.
  //
  // The same problem happens with the original ScramDisk v3 application, but
  // Aman added in a call to SetErrorMode to prevent this dialog from popping
  // up.
  // So we do the same thing here with SetErrorMode in a try...finally section...
  prevErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  try
    if ( (brutal=0) or (brutal=2) ) then
      begin
      //Win2k

      if (brutal=2) then
        begin
        sleep(1000);
        end;

      if not(NTCheckStreaming(slot)) then
        begin
        bResult:=SDDISMOUNTDISK;
        SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(strChkopName), 0, @bResult, 0, 0, nil);

        // The original ScramDisk v3 software had a SetFocus(NULL) here, so we do to :)
        SetFocus(0);
        sleep(100);

        bResult:=SDSENDUNMOUNDPENDING;
        SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(strChkopName), 0, @bResult, 0, 0, nil);

        sleep(100);

        Result := TRUE;
        finished := TRUE;
        end;

      end;

    if (not(finished) and (((brutal=1) and (bResult<>0)))) then
      begin
      bResult:=SDDISMOUNTDISK;
      SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(strChkopName), 0, @bResult, 0, 0, nil);

      // The original ScramDisk v3 software had a SetFocus(NULL) here, so we do to :)
      SetFocus(0);
      sleep(100);

      bResult:=SDSENDUNMOUNDPENDING;
      SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(strChkopName), 0, @bResult, 0, 0, nil);

      sleep(100);

      Result := TRUE;
      end;

  finally
    // Undo the change we made...
    SetErrorMode(prevErrorMode);
  end;

  // Result already = FALSE; just exit

end;



// xxx - put NT dismount stuff in a different file?
// Check to see if any ScramDisk volumes are mounted from the specified drive
function TOTFEScramDisk.RecursedContainer(drv: char): boolean;
begin
  Result := (length(VolsMountedOnDrive(drv))>0);

end;


// xxx - put NT dismount stuff in a different file?
procedure TOTFEScramDisk.Refresh_MyComp();
var
  sf: IShellFolder;
  p: PItemIDList;
  stt: STRRET;
  hMc: HWND;
  mcomstr: string;
begin

  SHGetDesktopFolder(sf);
  SHGetSpecialFolderLocation(0, CSIDL_DRIVES, p);
  sf.GetDisplayNameOf(p, 0, stt);

  if (stt.uType=STRRET_CSTR) then
    begin
    // strcpy(mcomstr, stt.cStr);
    mcomstr := stt.cStr;
    end
  else
    begin
    UniCpy(mcomstr, stt.pOleStr);
    end;

  // calling release causes an exception the next time SHGetDesktopFolder(sf) is called
  //  sf._Release();

  hMc:=FindWindow('CabinetWClass', PChar(mcomstr));

  if (hMc<>0) then
    begin
    SendMessage(hMc, WM_COMMAND, ($10000 or 41504), 0);
    end;

end;


procedure TOTFEScramDisk.UniCpy(var output: string; pwidestring: LPWSTR);
var
  s: DWORD;
  i: cardinal;
  wStr: widestring;
begin

  output := '';
  wStr := pwidestring;
  i:= 1;
  while (TRUE) do
    begin
    s := DWORD(wStr[i]);
    if (s=0) then
      begin
      break;
      end;
    output:= output + char(s and 255);
    inc(i);
    end;

end;



// xxx - put NT dismount stuff in a different file?
function TOTFEScramDisk.NTDoDeviceCloseNT(slotNo: integer; brutal: integer): boolean;
var
  hdr: TDEV_BROADCAST_VOLUME;
  closemode: integer;
  slotnum: integer;
  del: byte;
begin
  Result := FALSE;

  UpdateSlotInfo();
  if (MyCVS[slotNo].booted=0) then
    begin
    exit;
    end;

  if (RecursedContainer(char(MyCVS[slotNo].drive+ord('A')))) then
    begin
    FLastErrCode:= OTFE_ERR_DISMOUNT_RECURSIVE;
    // Result already = FALSE
    exit;
    end;

  closemode := 1;
  slotnum := slotNo;

  del:=byte(MyCVS[slotNo].drive);
  hdr.dbcv_size:= sizeof(TDEV_BROADCAST_VOLUME);

  hdr.dbcv_devicetype:= DBT_DEVTYP_VOLUME;
  hdr.dbcv_reserved:= 0;
  hdr.dbcv_unitmask:= 1 shl del;
  hdr.dbcv_flags:= DBTF_MEDIA;

  if not((NTDismountVolume(brutal, del, slotNo))) then
    begin
    // closes any relevant windows....
    BroadcastSystemMessage((BSF_FORCEIFHUNG or BSF_IGNORECURRENTTASK), nil, WM_DEVICECHANGE, DBT_DEVICEREMOVECOMPLETE, integer(@hdr));

    sleep(3000);
    if (brutal=1) then
      begin
      brutal:=2;
      end;

    if not(NTDismountVolume(brutal, del, slotNo)) then
      begin
      FLastErrCode := OTFE_ERR_FILES_OPEN;
      // Result already set to FALSE, so just exit
      exit;
      end;
    end;

  EjectStop(MyCVS[slotNo].mounted_file_name[0], FALSE);

  //	SDXDeviceIoControl(hScramDiskVxD, CLOSE_CRYPTED_DEVICE_N, &closemode, 0, &slotnum, 0, nil, nil);

  SDXDeviceIoControl(hScramDiskVxD, DISCONNECT_DOSDRIVE, @del, 0, @del, 0, 0, nil);

  SDXDeviceIoControl(hScramDiskVxD, CLOSE_CRYPTED_DEVICE_N, @closemode, 0, @slotnum, 0, 0, nil);

  BroadcastSystemMessage(BSF_FORCEIFHUNG, nil, WM_DEVICECHANGE, DBT_DEVICEREMOVECOMPLETE, integer(@hdr));


  ResetTimeStamp(MyCVS[slotNo].mounted_file_name);
  sleep(200);
  Refresh_MyComp();

  Result := TRUE;

end;



procedure TOTFEScramDisk.NTReadAllPartitionsNT();
var
  devstr: string;
  parstr: string;
  pc: integer;
  ic: integer;
  testdev: string;
  li: TDRIVE_LAYOUT_INFORMATION;
  pinfo: TPARTITION_INFORMATION;
begin
  CheckActive();

  devstr:='\Device\Harddisk';
  parstr:='\Partition';

  NTSDPartitionNames.Clear;

  for ic:=0 to 8 do
    begin
    if (NTReadNTPartition(ic, li)) then
      begin
      for pc:=0 to (li.PartitionCount-1) do
        begin
        pinfo:=li.PartitionEntry[pc];
        if (pinfo.PartitionType = $74) then
          begin
          testdev := devstr + char(ord('0')+ic) + parstr + char(ord('0')+pinfo.PartitionNumber);
          NTSDPartitionNames.Add(testdev);
          end;

        end;

      end;

    end;



end;


// Return TRUE if the physical drive has some partitions, otherwise FALSE
function TOTFEScramDisk.NTReadNTPartition(physdrive: integer; var driveLayout: TDRIVE_LAYOUT_INFORMATION): boolean;
var
  Ipartstr: string;
  li: TDRIVE_LAYOUT_INFORMATION;
  Pdevname: string;
  pli: PTDRIVE_LAYOUT_INFORMATION;
begin
  if not(RunningOnNT) then
    begin
    FLastErrCode := OTFE_ERR_NOT_W9X;
    raise EOTFEException.Create('OTFE_EXCPT_NOT_W9X');
    exit;
    end;


  Ipartstr := '\DosDevices\PHYSICALDRIVE';
  pli := @li;

  Pdevname := Ipartstr + char(physdrive+ord('0'));

  li.PartitionCount:=0;


  SDXDeviceIoControl(hScramDiskVxD, SDISK_NT_IOCTL, PChar(Pdevname), 0, @pli, 0, 0, nil);

  driveLayout := li;

  Result := ((li.PartitionCount)>0);

end;




// Debugging: Returns information about the specified drive
procedure TOTFEScramDisk.DEBUG_GetDriveInfo(physdrive: integer; var driveLayout: TDRIVE_LAYOUT_INFORMATION);
begin
  CheckActive();
  NTReadNTPartition(physdrive, driveLayout);
end;


// Sets "partitionNames" to be a list of valid ScramDisk partitions.
// Returns TRUE if there are any ScramDisk partitions, otherwise FALSE
// ONLY VALID WHEN RUNNING UNDER WINDOWS NT with ScramDisk v3
function TOTFEScramDisk.ValidScramDiskPartitions(partitionNames: TStringList): boolean;
begin
  CheckActive();

  if not(RunningOnNT) then
    begin
    FLastErrCode := OTFE_ERR_NOT_WNT;
    raise EOTFEException.Create(OTFE_EXCPT_NOT_WNT);
    Result := FALSE;
    exit;
    end;

  partitionNames.Assign(NTSDPartitionNames);
  Result := (partitionNames.count>0);

end;

procedure TOTFEScramDisk.SetPreferredDrive(drive: char);
begin
  FStartDrive:=(ord(upcase(drive))-ord('A'));

end;

function TOTFEScramDisk.GetPreferredDrive(): char;
begin
  Result := char(FStartDrive + ord('A'));

end;

// "SniffPassword" - sounds horrible, but is actually used to demonstrate
// a security weakness in ScramDisk
// Basically it just returns whatever passwords appears to be cached in the
// ScramDisk driver
procedure TOTFEScramDisk.SniffPassword(var password1 : string;
                                       var password2 : string;
                                       var password3 : string;
                                       var password4 : string);
var
  Blank : Char;
  Passwords : TMemoryStream;
  n : integer;
  ccc : char;
  i : integer;
begin

  GetPasswordBufferInt;

  Passwords := TMemoryStream.Create;
  try
    Blank := #0;
    for n := 0 to 159 do
      begin
      Passwords.Write(Blank, 1);
      end;
    CopyMemory(Passwords.Memory, pThePasswords, 160);

    Passwords.Position := 0;
    for i:=0 to 40 do
      begin
      Passwords.Read(ccc, 1);
      Password1 := Password1 + ccc;
      end;
    Passwords.Position := 40;
    for i:=0 to 40 do
      begin
      Passwords.Read(ccc, 1);
      Password2 := Password2 + ccc;
      end;
    Passwords.Position := 80;
    for i:=0 to 40 do
      begin
      Passwords.Read(ccc, 1);
      Password3 := Password3 + ccc;
      end;
    Passwords.Position := 120;
    for i:=0 to 40 do
      begin
      Passwords.Read(ccc, 1);
      Password4 := Password4 + ccc;
      end;
  finally
    Passwords.Free;
  end;

end;

// Enable/disable the ScramDisk driver's ".SVL" file protection
function TOTFEScramDisk.SetEnableVolumeDeletion(enable: boolean): boolean;
var
  en: integer;
  dwBytesReturned: DWORD;
begin
  CheckActive();

  dwBytesReturned := 0; // Required to get rid of compiler warning

  if (enable) then
    begin
    en := 1;
    end
  else
    begin
    en := 0;
    end;

	Result := SDXDeviceIoControl(hScramDiskVxD, ENABLE_CONTAINER_DELETE, @en, 0, @en, 0, dwBytesReturned, nil);

end;


END.


