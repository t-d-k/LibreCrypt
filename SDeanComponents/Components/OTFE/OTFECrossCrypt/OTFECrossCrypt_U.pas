unit OTFECrossCrypt_U;
// Description: Delphi CrossCrypt Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OTFE_U,
  OTFEConsts_U,
  OTFECrossCrypt_DriverAPI,
  WinSVC, // needed for OpenSCManager, etc

  
  dialogs; // xxx - get rid of this

//xxx - WASTE all showmessages left in this file.


type
  // Exceptions...
  ECrossCryptError = Exception;
  ECrossCryptCannotSetActive = ECrossCryptError;


type
  TOTFECrossCryptVolumeInfo = packed record
    DriveLetter: char;
    FileName: string;
    UserModeDeviceName: string;
    KernelModeDeviceName: string;
    ReadOnly: boolean;
  end;



  TOTFECrossCrypt = class(TOTFE)
  private
    { private declarations here}
  protected
    // Set the component active/inactive
    procedure SetActive(status: Boolean); override;

    function  IsServiceRunning(): boolean;


    procedure AddKey(ka: POPEN_FILE_INFORMATION; cipher: TCROSSCRYPT_CIPHER_TYPE; var key: string);

    function  FileDiskMount(
                            mountDeviceName: string;
                            OpenFileInformation: POPEN_FILE_INFORMATION;
                            DriveLetter: char;
                            CdImage: boolean
                           ): boolean;

    // Note: These are the symlinks to be used in USER MODE - *NOT* THE DEVICE
    //       DRIVER'S ACTUAL NAME
    function  GetAllUserDeviceNames(deviceNames: TStringList): boolean;
    // As GetAllUserDeviceNames, but for CD devices only
    function  GetCDUserDeviceNames(deviceNames: TStringList): boolean;
    // As GetAllUserDeviceNames, but for disk devices only
    function  GetDiskUserDeviceNames(deviceNames: TStringList): boolean;

    function  GetNextFreeDev(getCDDevice: boolean): string;

    function  GetDeviceInfo(deviceName: string; var volumeInfo: TOTFECrossCryptVolumeInfo): boolean;

    // Get a list of all CrossCrypt devices which are currently mounted
    function  GetMountedDevices(driveLetters: TStringList; deviceNames: TStringList): boolean;

    function  DoesLogicalDriveExist(driveLetter: char): boolean;

    function  UserToKernelModeDeviceName(deviceName: string): string;

    function  DoMount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE; volumeSizeInBytes: int64 = -1): boolean;
  public
    // Standard TOTFE methods
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy(); override;
    function  Title(): string; overload; override;
    function  IsDriverInstalled(): boolean; overload; override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; overload; override;
    function  MountDevices(): string; override;
    function  CanMountDevice(): boolean; override;
    // Note: CrossCrypt doesn't have the concept of an "emergency dismount"
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    // Note: CrossCrypt doesn't have the concept of an "emergency dismount"
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    // Note: You can only get the version ID with CrossCrypt, if there's one or
    //       more volumes mounted.
    //       *Stupid*, but true - a problem with the CrossCrypt driver!
    function  Version(): cardinal; overload; override;
    function  VersionStr(): string; overload; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  DrivesMounted(): string; overload; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  GetMainExe(): string; override;


    // Extensions to the standard TOTFE class...
    
    function  GetVolumeInfo(driveLetter: char; var volumeInfo: TOTFECrossCryptVolumeInfo): boolean;

    // Attempt to create and mount a volume file of the specified size
    // Returns the drive letter of the volume file, or #0 on error
    function  CreateVolume(volumeFilename: string; sizeInBytes: int64): char;

    // Returns the number of devices for each of the types disk/CDROM
    function  GetNumberOfDevices(): integer;

    // Set the number of devices for each of the types disk/CDROM
    function  SetNumberOfDevices(deviceCount: integer): boolean;

  end;

procedure Register;

implementation


uses
  OTFECrossCrypt_PasswordEntry,
  HashValue_U,
  HashAlg_U,
  HashAlgRIPEMD_U,
  HashAlgSHA256_U,
  HashAlgSHA384_U,
  HashAlgSHA512_U,
  Registry;


const
  CRLF = #13+#10;

  MIN_LINUX_PASSWORD_LENGTH = 20;

procedure Register;
begin
  RegisterComponents('OTFE', [TOTFECrossCrypt]);
end;


constructor TOTFECrossCrypt.Create(AOwner : TComponent);
begin
  inherited;

end;

destructor TOTFECrossCrypt.Destroy();
begin
  inherited;

end;


procedure TOTFECrossCrypt.SetActive(status: boolean);
var
  allOK: boolean;
begin
  LastErrorCode := OTFE_ERR_NOT_ACTIVE;

  allOK := TRUE;
  if (Active = FALSE) and (status = TRUE) then
    begin
    allOK := IsDriverInstalled() AND IsServiceRunning();
    if not(allOK) then
      begin
      raise ECrossCryptCannotSetActive.Create('CrossCrypt driver not installed/service not running');
      end;

    end;

  if allOK then
    begin
    LastErrorCode := OTFE_ERR_SUCCESS;
    inherited;
    end;

end;




function TOTFECrossCrypt.IsServiceRunning(): boolean;
var
  scmHandle: SC_HANDLE;
  servHandle: SC_HANDLE;
  serviceStatus: SERVICE_STATUS;
  isRunning: boolean;
begin
  isRunning := FALSE;

//showmessage('OpenSCManager');
  scmHandle:= OpenSCManager(nil, nil, GENERIC_READ);
  if (scmHandle <> 0) then
    begin
//showmessage('OpenService');
    servHandle := OpenService(scmHandle, 'FileDisk', GENERIC_READ);
    if (servHandle <> 0) then
      begin
//showmessage('QueryServiceStatus');
      if QueryServiceStatus(servHandle, serviceStatus) then
        begin
        isRunning := (serviceStatus.dwCurrentState = SERVICE_RUNNING);

//showmessage('xxx');
//  xxx - DOES THIS WORK FOR NON ADMIN?
//  xxx - DOES THIS WORK FOR w9x?
        end;

      end;

    end;

//showmessage('ENDING FUNCTION');

  Result := isRunning;

end;


function TOTFECrossCrypt.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  retVal: char;
  filenames: TStringList;
  mountedAs: string;
begin
  retVal := #0;
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  filenames:= TStringList.Create();
  try
    filenames.Add(volumeFilename);
    if Mount(filenames, mountedAs, readonly) then
      begin
      retVal := mountedAs[1];
      end;
  finally
    filenames.Text := StringOfChar('X', length(filenames.text));
    filenames.Free();
  end;


  Result := retVal;
end;


function TOTFECrossCrypt.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
begin
  CheckActive();

  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  Result := DoMount(volumeFilenames, mountedAs, readonly, -1);

end;


// Do an actual mount
// If "volumeSizeInBytes" is >0 then it will attempt to create the volume if
// the file doesn't already exist, otherwise it'll fail the mount if the volume
// file doesn't exist 
function TOTFECrossCrypt.DoMount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE; volumeSizeInBytes: int64 = -1): boolean;
var
  ka: TOPEN_FILE_INFORMATION;
  retVal: boolean;
  passwordDlg: TOTFECrossCrypt_PasswordEntry_F;
  tmpPw: string;
  tmpStr: string;
  multiPasswords: TStringList;
  i: integer;
  mountDeviceName: string;
  x: integer;
  currVolumeFilename: string;
  currVolumeDrive: char;
  fileCheck: boolean;
begin
  retVal := FALSE;

  CheckActive();

  mountedAs := '';

  // This is purely a sanity check to see if *all* CrossCrypt devices are in
  // use
  mountDeviceName := GetNextFreeDev(FALSE);
  if (mountDeviceName='') then
    begin
    mountDeviceName := GetNextFreeDev(TRUE);
    if (mountDeviceName='') then
      begin
      LastErrorCode := OTFE_ERR_NO_FREE_DEVICES;
      Result := retVal;
      exit;
      end;
    end;


  passwordDlg := TOTFECrossCrypt_PasswordEntry_F.Create(nil);
  try
    // If we're creating a new volume file, then we'd better get the user
    // to confirm the passwords they'll be using
    passwordDlg.Confirm := (volumeSizeInBytes>0);
    passwordDlg.OnlyReadWrite := (volumeSizeInBytes>0);
    passwordDlg.MinPasswordLength := 0;
    if (volumeSizeInBytes>0) then
      begin
      passwordDlg.MinPasswordLength := MIN_LINUX_PASSWORD_LENGTH;
      end;

    // Default password entry options
    passwordDlg.MountReadOnly:= readonly;

    if not(passwordDlg.ShowModal() = mrOK) then
      begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
      end
    else
      begin
      // --- FROM HERE ON IN WE JUST TRY TO MOUNT USING THE DETAILS SUPPLIED ---
      for x:=0 to (volumeFilenames.count-1) do
        begin
        currVolumeFilename := volumeFilenames[x];
        currVolumeDrive:= #0;

        // Quick sanity check: Does the file exist? Should it?
        fileCheck := FileExists(currVolumeFilename);
        if (volumeSizeInBytes>0) then
          begin
          // If the file already exists, but we're trying to create it; the
          // filecheck is inverted
          fileCheck := not(fileCheck);
          end;

        if fileCheck then
          begin
          // Attempt to get a CrossCrypt device name to use.
          mountDeviceName := GetNextFreeDev(passwordDlg.MountAsCD);
    // showmessage('mountdeives: '+mountDeviceName);
          // Now try to mount the volume on the free CrossCrypt device just
          // found... (If there *was* a free CrossCrypt device found...)
          if (mountDeviceName='') then
            begin
            LastErrorCode := OTFE_ERR_NO_FREE_DEVICES;
            end
          else
            begin
            ka.FileSize.QuadPart := volumeSizeInBytes;

            ka.DriveLetter := passwordDlg.DriveLetter;
            ka.ReadOnly := passwordDlg.MountReadOnly;
            ka.KeyType := CROSSCRYPT_CIPHER_IDS[passwordDlg.Cipher];


            ka.KeyNum := 0;
            if (passwordDlg.Cipher <> cphrNone) then
              begin
              if not(passwordDlg.MultipleKey) then
                begin
                // Not multikey mode: We want a single key
                passwordDlg.GetSinglePassword(tmpPw);
                AddKey(@ka, passwordDlg.Cipher, tmpPw);
                tmpPw := StringOfChar('X', length(tmpPw));
                end
              else
                begin
                // Multikey mode: We want multiple keys
                multiPasswords := TStringList.Create();
                try
                  passwordDlg.GetMultiplePasswords(multiPasswords);
                  for i:=0 to (multiPasswords.count-1) do
                    begin
                    // The password is passed by var to ensure that we don't make a
                    // duplicate which is never overwritten...
                    tmpPw := multiPasswords[i];
                    AddKey(@ka, passwordDlg.Cipher, tmpPw);
                    tmpPw := StringOfChar('X', length(tmpPw));
                    end;

                finally
                  multiPasswords.Text := StringOfChar('X', length(multiPasswords.text));
                  multiPasswords.Free();
                end;

                end;  // if not(passwordDlg.MultipleKey) then

              end;  // if (passwordDlg.Cipher <> cphrNone) then


              // Sort out ka.Filename and ka.FilenameLength

              // The TOPEN_FILE_INFORMATION stores the filename, and the length of the file
              //  - the length of the file is stored as a WORD (USHORT; unsigned number 2
              //    bytes long - max 65535); therefore we cannot handle filenames which
              //    exceed this is length
              //    (Less 10, as we may add in UNC info to the front)
              if (
                  (length(currVolumeFilename) <= (65535 - 10)) AND
                  (length(currVolumeFilename) <= (MAX_PATH - 10))
                 ) then
                begin
    // showmessage('vol filename OK lengthwise');
                if Pos('\', currVolumeFilename)=1 then
                  begin
                  if Pos('\\', currVolumeFilename)=1 then
                    begin
                    // \\server\share\path\filedisk.img
                    Delete(currVolumeFilename, 0, 1);
                    tmpStr := '\??\UNC'+currVolumeFilename;
                    ka.FileNameLength := Length(tmpStr);
                    StrPCopy(ka.Filename, tmpStr);
                    end
                  else
                    begin
                    // \Device\Harddisk0\Partition1\path\filedisk.img
                    tmpStr := currVolumeFilename;
                    ka.FileNameLength := Length(tmpStr);
                    StrPCopy(ka.Filename, tmpStr);
                    end;
                  end
                else
                  begin
                  // c:\path\filedisk.img
                  tmpStr := '\??\'+currVolumeFilename;
                  ka.FileNameLength := Length(tmpStr);
                  StrPCopy(ka.Filename, tmpStr);
                  end;


                if (FileDiskMount(mountDeviceName, @ka, passwordDlg.DriveLetter, passwordDlg.MountAsCD)) then
                  begin
                  currVolumeDrive := passwordDlg.DriveLetter;
                  end;

                end;

            end;  // if (mountDeviceName=='') then

          end;  // if fileCheck then

        mountedAs := mountedAs + currVolumeDrive;
        end;  // for x:=0 to (volumeFilenames.count-1) do

      end;  // if not(passwordDlg.ShowModal() = mrOK) then

  finally
    passwordDlg.Wipe();
    passwordDlg.Free();
  end;


  // If any drives could be mounted; set the return value to TRUE
  for i:=1 to length(mountedAs) do
    begin
    if (mountedAs[i]<>#0) then
      begin
      retVal := TRUE;
      LastErrorCode := OTFE_ERR_SUCCESS;
      end;
    end;


  Result := retVal;

end;



procedure TOTFECrossCrypt.AddKey(ka: POPEN_FILE_INFORMATION; cipher: TCROSSCRYPT_CIPHER_TYPE; var key: string);
var
  hashAlg: THashAlg;
  hashValue: THashArray;
  i: integer;
  x, y: integer;
begin
  if (ka.KeyNum >= MAX_KEYS) then
    begin
    exit;
    end;

  case (cipher) of

    cphrNone:
      begin
      // Do nothing
      end;

    cphrTwoFish:
      begin
      ka.KeyLength := 20;

      hashAlg:= THashAlgRIPEMD160.Create(nil);
      try
        hashValue := hashAlg.HashString(key);
        x := low(ka.Key);
        y := low(hashValue);
        for i:=1 to ka.KeyLength do
          begin
          ka.Key[ka.KeyNum][x] := hashValue[y];
          inc(x);
          inc(y);
          end;
        hashAlg.ClearHash(hashValue);

      finally
        hashAlg.Free();
      end;

      inc(ka.KeyNum);
      end;


    cphrAES256:
      begin
      ka.KeyLength := 32;

      hashAlg:= THashAlgSHA512.Create(nil);
      try
        hashValue := hashAlg.HashString(key);
        x := low(ka.Key);
        y := low(hashValue);
        for i:=1 to ka.KeyLength do
          begin
          ka.Key[ka.KeyNum][x] := hashValue[y];
          inc(x);
          inc(y);
          end;
        hashAlg.ClearHash(hashValue);

      finally
        hashAlg.Free();
      end;

      inc(ka.KeyNum);
      end;


    cphrAES128:
      begin
      ka.KeyLength := 16;

      hashAlg:= THashAlgSHA256.Create(nil);
      try
        hashValue := hashAlg.HashString(key);
        x := low(ka.Key);
        y := low(hashValue);
        for i:=1 to ka.KeyLength do
          begin
          ka.Key[ka.KeyNum][x] := hashValue[y];
          inc(x);
          inc(y);
          end;
        hashAlg.ClearHash(hashValue);

      finally
        hashAlg.Free();
      end;

      inc(ka.KeyNum);
      end;


    cphrAES192:
      begin
      ka.KeyLength := 24;

      hashAlg:= THashAlgSHA384.Create(nil);
      try
        hashValue := hashAlg.HashString(key);
        x := low(ka.Key);
        y := low(hashValue);
        for i:=1 to ka.KeyLength do
          begin
          ka.Key[ka.KeyNum][x] := hashValue[y];
          inc(x);
          inc(y);
          end;
        hashAlg.ClearHash(hashValue);

      finally
        hashAlg.Free();
      end;

      inc(ka.KeyNum);
      end;


    cphrUnknown:
      begin
      // <shrug>
      // Do nothing
      end;

  end;


end;


function TOTFECrossCrypt.FileDiskMount(
                                       mountDeviceName: string;
                                       OpenFileInformation: POPEN_FILE_INFORMATION;
                                       DriveLetter: char;
                                       CdImage: boolean
                                      ): boolean;
var
  VolumeName: string;
  BytesReturned: DWORD;
  retVal: boolean;
  Device: THandle;
  driveColon: string;
  kernelModeMountDeviceName: string;
begin
  retVal := FALSE;

  CheckActive();

  driveLetter := upcase(driveLetter);

  VolumeName := '\\.\'+driveLetter+':';
  driveColon := driveLetter+':';


  kernelModeMountDeviceName := UserToKernelModeDeviceName(mountDeviceName);

  Device := CreateFile(
                       PChar(VolumeName),
                       GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_READ or FILE_SHARE_WRITE,
                       nil,
                       OPEN_EXISTING,
                       FILE_FLAG_NO_BUFFERING,
                       0
                      );

  // We *want* this to *FAIL*; it indicates that the drive isn't already in use
  if (Device <> INVALID_HANDLE_VALUE) then
    begin
    LastErrorCode := OTFE_ERR_INVALID_DRIVE;
    CloseHandle(Device);
    end
  else
    begin
// showmessage('about to define dosdevice: '+driveColon+'  --  '+kernelModeMountDeviceName);
    if not(DefineDosDevice(
                           DDD_RAW_TARGET_PATH,
                           PChar(driveColon),
                           PChar(kernelModeMountDeviceName)
                          )) then
      begin
      // Unable to create drive
      LastErrorCode := OTFE_ERR_MOUNT_FAILURE;
// showmessage('FAILED DEFINEDOSDEVICE');
      end
    else
      begin
// showmessage('about to createfile on new DOSDEVICE on: '+VolumeName);
      Device := CreateFile(
                           PChar(VolumeName),
                           GENERIC_READ or GENERIC_WRITE,
                           FILE_SHARE_READ or FILE_SHARE_WRITE,
                           nil,
                           OPEN_EXISTING,
                           FILE_FLAG_NO_BUFFERING,
                           0
                          );
      // We want this to SUCCEED; it indicates that the drive is connected to
      // the device
      if (Device = INVALID_HANDLE_VALUE) then
        begin
        LastErrorCode := OTFE_ERR_MOUNT_FAILURE;
        DefineDosDevice(DDD_REMOVE_DEFINITION, PChar(driveColon), nil);
        end
      else
        begin
// showmessage('about to IOCTL_FILE_DISK_OPEN_FILE');
        if (DeviceIoControl(
                            Device,
                            IOCTL_FILE_DISK_OPEN_FILE,
                            OpenFileInformation,
                            sizeof(TOPEN_FILE_INFORMATION) + OpenFileInformation.FileNameLength - 1,
                            nil,
                            0,
                            BytesReturned,
                            nil
                           )) then
          begin
// showmessage('OKOKOK');
          retVal := TRUE;
          end
        else
          begin
// showmessage('BARFOUT - BARFOUT - BARFOUT - removign the DosDevice');
          LastErrorCode := OTFE_ERR_MOUNT_FAILURE;
          DefineDosDevice(DDD_REMOVE_DEFINITION, PChar(driveColon), nil);
          end;

        CloseHandle(Device);
        end;

      end;

    end;

  Result := retVal;
end;


function TOTFECrossCrypt.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  LastErrorCode := OTFE_ERR_DISMOUNT_FAILURE;

  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


function TOTFECrossCrypt.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  VolumeName: string;
  driveDevice: THandle;
  BytesReturned: DWORD;
  retVal: boolean;
  deviceName: string;
begin
  CheckActive();

  LastErrorCode := OTFE_ERR_DISMOUNT_FAILURE;
  retVal := FALSE;

  driveLetter := upcase(driveLetter);

  VolumeName := '\\.\'+driveLetter+':';
  deviceName := driveLetter+':';

//showmessage('About to createfile... ('+VolumeName+')');
  driveDevice := CreateFile(
                            PChar(VolumeName),
                            GENERIC_READ or GENERIC_WRITE,
                            FILE_SHARE_READ or FILE_SHARE_WRITE,
                            nil,
                            OPEN_EXISTING,
                            FILE_FLAG_NO_BUFFERING,
                            0
                           );

  if (driveDevice <> INVALID_HANDLE_VALUE) then
    begin
//showmessage('Got handle...');
    if (DeviceIoControl(
                        driveDevice,
                        FSCTL_LOCK_VOLUME,
                        nil,
                        0,
                        nil,
                        0,
                        BytesReturned,
                        nil
                        )) then
      begin
//showmessage('Locked...');
      if (DeviceIoControl(
                          driveDevice,
                          IOCTL_FILE_DISK_CLOSE_FILE,
                          nil,
                          0,
                          nil,
                          0,
                          BytesReturned,
                          nil
                          )) then
        begin
//showmessage('CrossCrypt closed disk...');
        if (DeviceIoControl(
                            driveDevice,
                            FSCTL_DISMOUNT_VOLUME,
                            nil,
                            0,
                            nil,
                            0,
                            BytesReturned,
                            nil
                            )) then
          begin
//showmessage('Dismounted...');
          if (DeviceIoControl(
                              driveDevice,
                              FSCTL_UNLOCK_VOLUME,
                              nil,
                              0,
                              nil,
                              0,
                              BytesReturned,
                              nil
                              )) then
            begin
//showmessage('Unlocked...');
            CloseHandle(driveDevice);
            driveDevice := 0;

            retVal := DefineDosDevice(
                                      DDD_REMOVE_DEFINITION,
                                      PChar(deviceName),
                                      nil
                                     );
            if (retVal) then
              begin
//showmessage('Undefined DosDevice OK!');
              LastErrorCode:= OTFE_ERR_SUCCESS;
              end;

            end;  // DIOC for FSCTL_UNLOCK_VOLUME
          end;  // DIOC for FSCTL_DISMOUNT_VOLUME
        end;  // DIOC for IOCTL_FILE_DISK_CLOSE_FILE
      end;  // DIOC for FSCTL_LOCK_VOLUME


    // Only close the device if it hasn't already been closed
    if (driveDevice<>0) then
      begin
//showmessage('Closing handle...');
      CloseHandle(driveDevice);
//showmessage('Closed.');
      end;

    end;  // if (driveDevice <> INVALID_HANDLE_VALUE) then

//showmessage('Exiting dismount.');

  Result := retVal;
end;


function TOTFECrossCrypt.Title(): string;
begin
  Result := 'CrossCrypt';
end;


function TOTFECrossCrypt.IsDriverInstalled(): boolean;
var
  registry: TRegistry;
  installed: boolean;
begin
  installed := FALSE;


  // The check to see if the driver is installed consists of just checking to
  // see if the relevant registry entries exist for the driver...


  registry := TRegistry.create();
  try
    registry.RootKey := CROSSCRYPT_REGISTRY_ROOT;
    if registry.OpenKeyReadOnly(CROSSCRYPT_REGISTRY_KEY_MAIN) then
      begin
      // Options
      if registry.ValueExists(CROSSCRYPT_REGGISTRY_MAIN_NAME_START) then
        begin
        installed := TRUE;
        end;

      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;

  FLastErrCode:= OTFE_ERR_SUCCESS;

  Result := installed;
end;



function TOTFECrossCrypt.Version(): cardinal;
var
  BytesReturned: DWORD;
  retVal: cardinal;
  deviceHandle: THandle;
  OpenFileInformation: TOPEN_FILE_INFORMATION;
  deviceNames: TStringList;
  i: integer;
begin
  retVal := $FFFFFFFF;

  CheckActive();

  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  deviceNames := TStringList.Create();
  try
    if GetAllUserDeviceNames(deviceNames) then
      begin
      for i:=0 to (deviceNames.count-1) do
        begin
        deviceHandle := CreateFile(
                                   PChar(deviceNames[i]),
                                   GENERIC_READ,
                                   FILE_SHARE_READ OR FILE_SHARE_WRITE,
                                   nil,
                                   OPEN_EXISTING,
                                   FILE_FLAG_NO_BUFFERING,
                                   0
                                  );

        if (deviceHandle <> INVALID_HANDLE_VALUE) then
          begin
          if (DeviceIoControl(
                              deviceHandle,
                              IOCTL_FILE_DISK_QUERY_FILE,
                              nil,
                              0,
                              @OpenFileInformation,
                              sizeof(OpenFileInformation), // Note: We don't add MAX_PATH due to the fixed buffer we used for this struct
                              BytesReturned,
                              nil
                              )) then
            begin
            retVal := OpenFileInformation.Version;
            LastErrorCode := OTFE_ERR_SUCCESS;
            break;
            end;

          CloseHandle(deviceHandle);
          end;

        end;  // for i:=0 to (deviceNames.count-1) do

      end;  // if GetAllDeviceNames(deviceNames) then

  finally
    deviceNames.Free();
  end;


  Result := retVal;

end;


function TOTFECrossCrypt.VersionStr(): string;
var
  verNo: cardinal;
  majorVer: integer;
  minorVer: integer;
  retval: string;
begin
  retval := '';

  CheckActive();
  
  verNo := Version();
  
  if (verNo <> $FFFFFFFF) then
    begin
    majorVer := (verNo AND $FF00) div $FF;
    minorVer := (verNo AND $FF);
    retval := Format('v%d.%.d', [majorVer, minorVer]);
    end;

  Result := retval;

end;


function TOTFECrossCrypt.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  CheckActive();

  LastErrorCode:= OTFE_ERR_SUCCESS;

  // We can't tell, so always return TRUE
  Result := TRUE;

end;


function TOTFECrossCrypt.DrivesMounted(): string;
var
  retVal: string;
  i: integer;
  driveLetters: TStringList;
  deviceNames: TStringList;
begin
  retVal := '';
  LastErrorCode:= OTFE_ERR_SUCCESS;

  CheckActive();

  driveLetters:= TStringList.Create();
  try
    deviceNames:= TStringList.Create();
    try
      if GetMountedDevices(driveLetters, deviceNames) then
        begin
        for i:=0 to (driveLetters.count-1) do
          begin
          if (driveLetters[i]<>'') then
            begin
            retVal := retVal + driveLetters[i];
            end;

          end;

        retVal := SortString(retVal);
        end;

    finally
      deviceNames.Free();
    end;
  finally
    driveLetters.Free();
  end;


  Result := retVal;
end;


function TOTFECrossCrypt.GetVolFileForDrive(driveLetter: char): string;
var
  retVal: string;
  volumeInfo: TOTFECrossCryptVolumeInfo;
begin
  retVal := '';
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  if (GetVolumeInfo(driveLetter, volumeInfo)) then
    begin
    retVal := volumeInfo.Filename;
    LastErrorCode := OTFE_ERR_SUCCESS;
    end;

  Result := retVal;

end;


function TOTFECrossCrypt.GetDriveForVolFile(volumeFilename: string): char;
var
  mounted: string;
  volumeInfo: TOTFECrossCryptVolumeInfo;
  retVal: char;
  i: integer;
begin
  retVal := #0;
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  mounted := DrivesMounted();
  for i:=1 to length(mounted) do
    begin
    if GetVolumeInfo(mounted[i], volumeInfo) then
      begin
      if (uppercase(volumeInfo.Filename) = uppercase(volumeFilename)) then
        begin
        retVal := volumeInfo.DriveLetter;
        LastErrorCode := OTFE_ERR_SUCCESS;
        break;
        end;
      
      end;
    end;

  Result := retVal;

end;


function TOTFECrossCrypt.GetMainExe(): string;
begin
  // This is not meaningful for CrossCrypt
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;
  Result := '';

end;


// Get a list of all CrossCrypt devices which are currently mounted
// Drive letters will be added to "driveLetters", and the corresponding device
// in "deviceNames".
// Returns TRUE/FALSE on success/failure
function TOTFECrossCrypt.GetMountedDevices(driveLetters: TStringList; deviceNames: TStringList): boolean;
var
  allOK: boolean;
  i: integer;
  volumeInfo: TOTFECrossCryptVolumeInfo;
begin
  allOK := TRUE;

  CheckActive();

  deviceNames:= TStringList.Create();
  try
    GetAllUserDeviceNames(deviceNames);

    // Extract details for each of the devices in turn...
    for i:=0 to (deviceNames.count-1) do
      begin
      if GetDeviceInfo(deviceNames[i], volumeInfo) then
        begin
        driveLetters.Add(volumeInfo.DriveLetter);
        deviceNames.Add(volumeInfo.UserModeDeviceName);
        end;  // if GetDeviceInfo(deviceNames[i], open_file_information) then

      end;  // for i:=0 to (deviceNames.count-1) do

  finally
    deviceNames.Free();
  end;


  Result := allOK;

end;


function TOTFECrossCrypt.GetVolumeInfo(driveLetter: char; var volumeInfo: TOTFECrossCryptVolumeInfo): boolean;
var
  found: boolean;
  deviceNames: TStringList;
  i: integer;
begin
  found := FALSE;
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  driveLetter := upcase(driveLetter);

  deviceNames:= TStringList.Create();
  try
    GetAllUserDeviceNames(deviceNames);

    // Extract details for each of the devices in turn...
    for i:=0 to (deviceNames.count-1) do
      begin
      if GetDeviceInfo(deviceNames[i], volumeInfo) then
        begin
        if (upcase(volumeInfo.DriveLetter) = driveLetter) then
          begin
          found := TRUE;
          LastErrorCode := OTFE_ERR_SUCCESS;
          break;
          end;  // if (uppercase(open_file_information.DriveLetter) = driveLetter) then

        end;  // if GetDeviceInfo(deviceNames[i], open_file_information) then

      end;  // for i:=0 to (deviceNames.count-1) do

  finally
    deviceNames.Free();
  end;


  Result := found;

end;


// Set deviceNames to be a list of *all* CrossCrypt devices
// Note: These are the symlinks to be used in USER MODE - *NOT* THE DEVICE
//       DRIVER'S ACTUAL NAME
function TOTFECrossCrypt.GetAllUserDeviceNames(deviceNames: TStringList): boolean;
var
  allOK: boolean;
  tmpDeviceNames: TStringList;
begin
  allOK := TRUE;

  // Generate a list of all CrossCrypt device names
  deviceNames.Clear();

  tmpDeviceNames:= TStringList.Create();
  try
    tmpDeviceNames.Clear();
    allOK := allOK AND GetDiskUserDeviceNames(tmpDeviceNames);
    deviceNames.AddStrings(tmpDeviceNames);

    tmpDeviceNames.Clear();
    allOK := allOK AND GetCDUserDeviceNames(tmpDeviceNames);
    deviceNames.AddStrings(tmpDeviceNames);
  finally
    tmpDeviceNames.Free();
  end;

  Result := allOK;

end;


// Set deviceNames to be a list of all CrossCrypt disk devices
function TOTFECrossCrypt.GetCDUserDeviceNames(deviceNames: TStringList): boolean;
var
  i: integer;
  deviceCount: integer;
begin
  deviceCount := GetNumberOfDevices();

  // Generate a list of all CrossCrypt device names
  deviceNames.Clear();
  for i:=0 to (deviceCount-1) do
    begin
    deviceNames.Add(USER_MODE_DEVICE_NAME_PREFIX+'Cd'+inttostr(i));
    end;

  Result := TRUE;

end;


// Set deviceNames to be a list of all CrossCrypt CD devices
function TOTFECrossCrypt.GetDiskUserDeviceNames(deviceNames: TStringList): boolean;
var
  i: integer;
  deviceCount: integer;
begin
  deviceCount := GetNumberOfDevices();

  // Generate a list of all CrossCrypt device names
  deviceNames.Clear();
  for i:=0 to (deviceCount-1) do
    begin
    deviceNames.Add(USER_MODE_DEVICE_NAME_PREFIX+inttostr(i));
    end;

  Result := TRUE;

end;


// Get the next free CrossCrypt device
// Returns '' if there are none, and on error
function TOTFECrossCrypt.GetNextFreeDev(getCDDevice: boolean): string;
var
  deviceNames: TStringList;
  retVal: string;
  volumeInfo: TOTFECrossCryptVolumeInfo;
  i: integer;
begin
  retVal := '';

  deviceNames := TStringList.Create();
  try
    if (getCDDevice) then
      begin
      GetCDUserDeviceNames(deviceNames);
      end
    else
      begin
      GetDiskUserDeviceNames(deviceNames);
      end;


    // Scan through all the device names, attempting to find one which isn't
    // in use
    for i:=0 to (deviceNames.count-1) do
      begin
      if not(GetDeviceInfo(deviceNames[i], volumeInfo)) then
        begin
        // We couldn't get any information - the device must be unused
        retVal := deviceNames[i];
        break;
        end;

      end;

  finally
    deviceNames.Free();
  end;

  Result := retVal;
end;


function TOTFECrossCrypt.GetDeviceInfo(deviceName: string; var volumeInfo: TOTFECrossCryptVolumeInfo): boolean;
var
  BytesReturned: DWORD;
  allOK: boolean;
  deviceHandle: THandle;
  open_file_information: TOPEN_FILE_INFORMATION;
begin
  allOK := FALSE;

  CheckActive();


  deviceHandle := CreateFile(
                             PChar(deviceName),
                             GENERIC_READ or GENERIC_WRITE,
                             FILE_SHARE_READ or FILE_SHARE_WRITE,
                             nil,
                             OPEN_EXISTING,
                             FILE_FLAG_NO_BUFFERING,
                             0
                            );
  if (deviceHandle <> INVALID_HANDLE_VALUE) then
    begin
    if (DeviceIoControl(
                        deviceHandle,
                        IOCTL_FILE_DISK_QUERY_FILE,
                        nil,
                        0,
                        @open_file_information,
                        sizeof(open_file_information), // Note: We don't add MAX_PATH due to the fixed buffer we used for this struct
                        BytesReturned,
                        nil
                        )) then
      begin
      volumeInfo.UserModeDeviceName:= deviceName;
      volumeInfo.KernelModeDeviceName := UserToKernelModeDeviceName(deviceName);


      volumeInfo.DriveLetter := upcase(open_file_information.DriveLetter);
      volumeInfo.ReadOnly:= open_file_information.ReadOnly;

      volumeInfo.FileName := copy(open_file_information.FileName, 0, open_file_information.FileNameLength);

      // Clean up the filename...
      if Pos('\??\UNC', volumeInfo.FileName)=1 then
        begin
        // \??\UNC\server\share\path\filedisk.img
        // should display as:
        // \\server\share\path\filedisk.img
        delete(volumeInfo.FileName, 1, length('\??\UNC'));
        volumeInfo.FileName := '\'+volumeInfo.FileName;
        end
      else if Pos('\??\', volumeInfo.FileName)=1 then
        begin
        // \??\c:\path\filedisk.img
        // should display as:
        // c:\path\filedisk.img
        delete(volumeInfo.FileName, 1, length('\??\'));
        end
      else
        begin
        // \Device\Harddisk0\Partition1\path\filedisk.img
        // should display as:
        // \Device\Harddisk0\Partition1\path\filedisk.img

        // i.e. No change...
        end;

      allOK := TRUE;
      end;

    CloseHandle(deviceHandle);
    end;


  Result := allOK;

end;


// Returns the number of devices for each of the types disk/CDROM
// Returns -1 on error
function TOTFECrossCrypt.GetNumberOfDevices(): integer;
var
  registry: TRegistry;
  deviceCount: integer;
begin
  LastErrorCode:= OTFE_ERR_UNKNOWN_ERROR;

  deviceCount := CROSSCRYPT_DFLT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES;

  registry := TRegistry.create();
  try
    registry.RootKey := CROSSCRYPT_REGISTRY_ROOT;
    if registry.OpenKeyReadOnly(CROSSCRYPT_REGISTRY_KEY_PARAM) then
      begin
      // Options
      if registry.ValueExists(CROSSCRYPT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES) then
        begin
        try
          deviceCount := registry.ReadInteger(CROSSCRYPT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES);
        except
          deviceCount := CROSSCRYPT_DFLT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES;
        end;

        end;


      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;


  LastErrorCode:= OTFE_ERR_SUCCESS;

  Result := deviceCount;
end;


// Set the number of devices for each of the types disk/CDROM
function TOTFECrossCrypt.SetNumberOfDevices(deviceCount: integer): boolean;
var
  registry: TRegistry;
  allOK: boolean;
begin
  LastErrorCode:= OTFE_ERR_UNKNOWN_ERROR;

  allOK := FALSE;
  
  registry := TRegistry.create();
  try
    registry.RootKey := CROSSCRYPT_REGISTRY_ROOT;
    if registry.OpenKey(CROSSCRYPT_REGISTRY_KEY_PARAM, FALSE) then
      begin
      registry.WriteInteger(CROSSCRYPT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES, deviceCount);

      registry.CloseKey;
      allOK := TRUE;
      end;
  finally
    registry.Free();
  end;


  LastErrorCode:= OTFE_ERR_SUCCESS;

  Result := allOK;
end;



// Checks to see if the specified logical drive letter currently exists
// Returns TRUE if it does, otherwise FALSE
function TOTFECrossCrypt.DoesLogicalDriveExist(driveLetter: char): boolean;
var
  driveNum: Integer;
  driveBits: set of 0..25;
  retVal: boolean;
begin
  retVal:= FALSE;

  driveLetter := upcase(driveLetter);

  Integer(driveBits) := GetLogicalDrives();
  driveNum := ord(driveLetter) - ord('A');

  if (driveNum in DriveBits) then
    begin
    retVal := TRUE;
    end;

  Result := retVal;

end;


// Convert a user-mode device naem (a symlink) into the corresponding *actual*
// device name
function TOTFECrossCrypt.UserToKernelModeDeviceName(deviceName: string): string;
begin
  // Strip off the USER_MODE_DEVICE_NAME_PREFIX and replace with the
  // DEVICE_NAME_PREFIX
  delete(deviceName, 1, Length(USER_MODE_DEVICE_NAME_PREFIX));
  deviceName := DEVICE_NAME_PREFIX + deviceName;

  Result := deviceName;
end;


function TOTFECrossCrypt.CreateVolume(volumeFilename: string; sizeInBytes: int64): char;
var
  retVal: char;
  filenames: TStringList;
  mountedAs: string;
begin
  retVal := #0;

  CheckActive();

  LastErrorCode:= OTFE_ERR_UNKNOWN_ERROR;

  filenames:= TStringList.Create();
  try
    filenames.Add(volumeFilename);
    if DoMount(filenames, mountedAs, FALSE, sizeInBytes) then
      begin
      retVal := mountedAs[1];
      LastErrorCode:= OTFE_ERR_SUCCESS;
      end;
  finally
    filenames.Text := StringOfChar('X', length(filenames.text));
    filenames.Free();
  end;


  Result := retVal;
end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFECrossCrypt.MountDevices(): string;
begin
  // Not supported...
  Result := #0;
end;

// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFECrossCrypt.CanMountDevice(): boolean;
begin
  // Not supported...
  Result := FALSE;
end;

// -----------------------------------------------------------------------------


END.


