unit OTFEBestCrypt_U;
// Description: Delphi BestCrypt Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes, SysUtils, Windows, forms, controls,
  OTFE_U, OTFEConsts_U,
  OTFEBestCryptStructures_U;

type
  TOTFEBestCrypt = class(TOTFE)
  private
    function  RegistryOnlyVersion(): boolean;

    // INI file versions - don't call these! Used by the general routines
    function  INIGetAutomountContainerNum(volumeFilename: string): integer;
    function  INIGetDefaultDrive(volumeFilename: string): char;
    function  INISetDefaultDriveAutomount(volumeFilename: string; driveLetter: char; automount: boolean = FALSE): boolean;
    function  INIDeleteDefaultDrive(volumeFilename: string): boolean;

    // Registry versions - don't call these! Used by the general routines
    function  RegistryVolumeSetDefaultDrive(volumeFilename: string; driveLetter: char): boolean;
    function  RegistryVolumeSetAutomounted(volumeFilename: string; automount: boolean): boolean;
    function  GetFilenameASCIIHash(volumeFilename: string): string;
  protected
    hBestCryptVxD : THandle;
    FPasswordPrompt: string;
    kBestCryptINIFileAutoOpenKey: string;
    function  LoadDLL(DLLFilename: string): THandle;
    function  Connect(): boolean;
    function  ConnectAttempt(driverName: string; var handle: THandle): boolean;

    function  Disconnect(): boolean;
    procedure SetActive(AValue : Boolean); override;

    function  GetDriveInfoOLD(driveLetter: char; var volInfo: TDISKINFO_BUFFER_driverOLD): boolean;
    function  GetDriveInfoNEW(driveLetter: char; var volInfo: TDISKINFO_BUFFER_driverNEW): boolean;

    function  MountDriverOLD(volumeFilename: string; readonly: boolean; driveLetter: char; diskSizeLow, diskSizeHigh: DWORD; readInHeader: THiddenSector): char;
    function  MountDriverNEW(volumeFilename: string; readonly: boolean; driveLetter: char; diskSizeLow, diskSizeHigh: DWORD; readInHeader: THiddenSector): char;

    // This function is ported from the code segment (function) received by
    // email from Sergey Frolov (Jetico)
    function  GetCurrentUserID(): DWORD;
  public
    function  GetBestCryptFileHeader(volumeFilename: string; var header: THiddenSector): boolean;


    function  VolumeIsAutomounted(volumeFilename: string): boolean;

    function  GetDefaultDrive(volumeFilename: string): char;
    function  SetDefaultDriveAutomount(volumeFilename: string; driveLetter: char; automount: boolean = FALSE): boolean;
    function  DeleteDefaultDrive(volumeFilename: string): boolean;


    function  CreateKeyHandle(volumeFilename: string; var keyHandle: KEY_HANDLE): boolean;

    procedure GetAllAlgorithmIDs(algIDs: TStrings);
    function  GetAlgorithmName(algorithmID: DWORD): string; // just calls GetAlgorithmRegDetails
    function  GetAlgorithmDriverName(algorithmID: DWORD): string; // just calls GetAlgorithmRegDetails
    function  GetAlgorithmKeyLength(algorithmID: DWORD): integer; // just calls GetAlgorithmRegDetails
    function  GetAlgorithmVersion(algorithmID: DWORD): cardinal;
    function  GetAlgorithmRegDetails(algorithmID: DWORD; var dispName: string; var driverName: string; var keyLen: integer): boolean;

    procedure GetAllKeyGenIDs(keyGenIDs: TStrings);
    function  GetKeyGenName(keyGenID: DWORD): string; // just calls GetKeyGenRegDetails
    function  GetKeyGenDLLName(keyGenID: DWORD): string; // just calls GetKeyGenRegDetails
    function  GetKeyGenVersion(keyGenID: DWORD): cardinal;
    function  GetKeyGenRegDetails(keyGenID: DWORD; var dispName: string; var DLLName: string): boolean;

    property  PasswordPrompt: string read FPasswordPrompt write FPasswordPrompt;

    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    // Determine if the specified volume was previously mounted, and then only
    // "half dismounted" (i.e. a dismount was forced, even though the drive was
    // in use at the time)
    function  VolumeInternallyMounted(volumeFilename: string): char;
    // Return a list of drive letters for all drives the BestCrypt driver
    // *claims* are mounted; i.e. including drives that have been
    // "half dismounted" (i.e. a dismount was forced, even though the drive was
    // in use at the time)
    function  InternalDrivesMounted(): string;


    // TOTFE functions...
    function  Title(): string; overload; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; overload; override;
    function  MountDevices(): string; override;
    function  CanMountDevice(): boolean; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  DrivesMounted(): string; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetMainExe(): string; override;


    function  GetVolumeInfo(volumeFilename: string; var info: TBCDiskInfo): boolean; overload;
    function  GetVolumeInfo(driveLetter: char; var info: TBCDiskInfo): boolean; overload;

  end;

procedure Register;

type
  // Function declarations for DLL functions
  TBCGetNameAndVersion = function(Name: PChar;     // module name
                                  nameSize: UINT;  // size of  Name string in bytes
                                  Major: pDword;   // module’s major version number
                                  Minor: pDword    // module’s minor version number
                                  ): boolean; stdcall;


  TBCCreateKeyHandle = function(AlgServiceName: PChar;  // what Encryption Algorithm will use the encryption key
                                AlgId: DWORD;           // Encryption Algorithm Identificator
                                AlgKeyLength: DWORD;    // encryption key length for the Encryption Algorithm
                                Text: PChar;            // Any information text (optional parameter)
                                Caption: PChar;         // Caption of the "get password" dialog window (optional parameter)
                                CreateFlag: DWORD;      // whether to create new key or open existing
                                vDataBlock: ppByte;     // block of data that is stored inside file-container in encrypted form
                                DataSize: pDWORD;       // size of the block of data
                                KeyHandle: pDWORD;      // returned Key Handle
                                ErrorCode: pDWORD;      // if the function returns FALSE, look at the Error code
                                hWndParent: HWND        // parent Window for any dialogs drawn by the DLL (optional parameter, may be NULL))
                                ): boolean; stdcall;

  TBCFreeDataBlock = function(vDataBlock: ppByte): DWORD; stdcall;

  TBCFreeKeyHandle = function(AlgServiceName: PChar;
                              KeyHandle: DWORD
                              ): DWORD; stdcall;


implementation

uses
  ShellAPI,
  Messages,
  dialogs,
  Registry, RegStr,
  OTFEBestCryptGetDriveLetter_U,
  INIFiles,
  SDUGeneral,
  HashValue_U, HashAlg_U, HashAlgSHA1_U,
  OTFEBestCryptBytesToString_U,
  math; // required for "power(...)" function


const
  CRLF = #10+#13;

procedure Register;
begin
  RegisterComponents('OTFE', [TOTFEBestCrypt]);
end;


constructor TOTFEBestCrypt.Create(AOwner : TComponent);
const
  MAX_USERNAME_LENGTH = 256;
var
 usernameLength: cardinal;
 arrUserName: array [1..MAX_USERNAME_LENGTH] of char;
begin
  inherited create(AOwner);

  FPasswordPrompt := 'Enter password';

  kBestCryptINIFileAutoOpenKey := 'AutoOpen';
  usernameLength := MAX_USERNAME_LENGTH;
  GetUserName(@arrUserName, usernameLength);

  if usernameLength>0 then
    begin
    kBestCryptINIFileAutoOpenKey := kBestCryptINIFileAutoOpenKey + ' ' + copy(arrUserName, 1, usernameLength)
    end;

end;

destructor TOTFEBestCrypt.Destroy;
begin
  if FActive then
    CloseHandle(hBestCryptVxD);
  inherited Destroy;
end;


function TOTFEBestCrypt.Connect(): boolean;
var
  attemptOK: boolean;
begin
  Result := Active;
  FLastErrCode:= OTFE_ERR_SUCCESS;

  if not(Active) then
    begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      begin
      // Windows NT/2000/XP driver
      if Win32MajorVersion>=5 then
        begin
        // Windows 2000/XP driver
        attemptOK := ConnectAttempt(BESTCRYPT_2KXP_DRIVER, hBestCryptVxD);

        // Previous versions don't have a specific name for Windows 2000/XP
        //  - in which case, fall back to the NT name
        if (not(attemptOK)) then
          begin
          attemptOK := ConnectAttempt(BESTCRYPT_NT_DRIVER, hBestCryptVxD);
          end;

        end
      else
        begin
        // Windows NT driver
        attemptOK := ConnectAttempt(BESTCRYPT_NT_DRIVER, hBestCryptVxD);
        end;

      end
    else
      begin
      // Windows 95/98
      attemptOK := ConnectAttempt(BESTCRYPT_9598_DRIVER, hBestCryptVxD);
      end;

    if not(attemptOK) then
      begin
      FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
      raise EBestCryptVxdNotFound.Create('BestCrypt device driver not found');
      end;

    Result := attemptOK;
    end;

end;


// driverName - the name of the driver to attempt a connectoin to
// handle - if successful, this will be set to the handle
// Returns TRUE/FALSE on success/failure
function TOTFEBestCrypt.ConnectAttempt(driverName: string; var handle: THandle): boolean;
begin
  handle:= INVALID_HANDLE_VALUE;

  handle := CreateFile(PChar(driverName),
                       GENERIC_READ OR GENERIC_WRITE,
                       0,
                       nil,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0);

  Result := (handle <> INVALID_HANDLE_VALUE);
end;


function TOTFEBestCrypt.Title(): string;
begin
  Result := 'BestCrypt';
end;


function TOTFEBestCrypt.Version(): cardinal;
var
  dwBytesReturned : DWORD;
  query: TVERSION_BUFFER;
begin
  CheckActive();

  Result := $FFFFFFFF;

  query.Signature := INPUT_SIGNATURE;

  DeviceIoControl(hBestCryptVxD,
                  IOCTL_PRIVATE_GET_VERSION_INFO,
                  @query,
                  sizeof(query),
                  @query,
                  sizeof(query),
                  dwBytesReturned,
                  nil);

  LastErrorCode := query.status;
  if query.signature<>OUTPUT_SIGNATURE then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end;

  if query.status=0 then
    begin
    Result := (query.Major_version * $100) + query.Minor_version;
    end;

end;

function TOTFEBestCrypt.DrivesMounted(): string;
var
  retVal: string;
  i: integer;
  logicalDrives: DWORD;
  internalMountedDrvs: string;
  currDrv: integer;
begin
  retVal := '';

  internalMountedDrvs:= InternalDrivesMounted();

  // logicalDrivesAvailable is required as BestCrypt will quite happily claim
  // that volumes that have been half unmounted (e.g. forced dismount while
  // the volume was still in use) are still mounted.
  logicalDrives := GetLogicalDrives();
  for i:=1 to length(internalMountedDrvs) do
    begin
    currDrv := (ord(internalMountedDrvs[i]))-(ord('A'));
    if ((logicalDrives AND (trunc(power(2, currDrv))))>0) then
      begin
      retVal:= retVal + internalMountedDrvs[i];
      end;
    end;


  Result := retVal;

end;

function TOTFEBestCrypt.InternalDrivesMounted(): string;
var
  dwBytesReturned : DWORD;
  queryOLD: TGETMASK_BUFFER_driverOLD;
  queryNEW: TGETMASK_BUFFER_driverNEW;
  retVal: string;
  i: integer;
  mask: DWORD;
  maskFromQuery: DWORD;
  signatureFromQuery: string;
  statusFromQuery: UCHAR;
begin
  CheckActive();

  if (Version() < DRIVER_VERSION_GETMASK_NEW) then
    begin
    // HANDLE FOR v2 DRIVERS...

    queryOLD.Signature := INPUT_SIGNATURE;
    queryOLD.status := 0;
    queryOLD.Mask := 0;

    DeviceIoControl(hBestCryptVxD,
                    IOCTL_PRIVATE_GET_BCDISK_MASK,
                    @queryOLD,
                    sizeof(queryOLD),
                    @queryOLD,
                    sizeof(queryOLD),
                    dwBytesReturned,
                    nil);

    maskFromQuery:= queryOLD.mask;
    signatureFromQuery:= queryOLD.signature;
    statusFromQuery:= queryOLD.status;
    end
  else
    begin
    // HANDLE FOR v3 DRIVERS...

    queryNEW.Signature := INPUT_SIGNATURE;
    queryNEW.status := 0;
    queryNEW.Mask := 0;

    DeviceIoControl(hBestCryptVxD,
                    IOCTL_PRIVATE_GET_BCDISK_MASK,
                    @queryNEW,
                    sizeof(queryNEW),
                    @queryNEW,
                    sizeof(queryNEW),
                    dwBytesReturned,
                    nil);

    maskFromQuery:= queryNEW.mask;
    signatureFromQuery:= queryNEW.signature;
    statusFromQuery:= queryNEW.status;
    end;

  LastErrorCode := statusFromQuery;
  if signatureFromQuery<>OUTPUT_SIGNATURE then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end;

  retVal := '';
  mask := 1;
  for i:=0 to 31 do
    begin
    if ((maskFromQuery AND mask)>0) then
      begin
      retVal := retVal + chr(ord('A')+i);
      end;
    mask := mask * 2;
    end;

  // We don't need to sort retVal into alphabetical order, as it will be anyway
  // as it's already sorted
  Result := retVal;

end;

function TOTFEBestCrypt.Disconnect(): boolean;
begin
  FLastErrCode:= OTFE_ERR_SUCCESS;
  if Active then
    begin
    CloseHandle(hBestCryptVxD);
    end;
  Result := TRUE;

end;


procedure TOTFEBestCrypt.SetActive(AValue : Boolean);
var
  allOK: boolean;
begin
  FLastErrCode:= OTFE_ERR_SUCCESS;
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


function TOTFEBestCrypt.GetDriveInfoOLD(driveLetter: char; var volInfo: TDISKINFO_BUFFER_driverOLD): boolean;
var
  dwBytesReturned : DWORD;
  query: TDISKINFO_BUFFER_driverOLD;
begin
  CheckActive();

  query.Signature := INPUT_SIGNATURE;
  query.diskNumber := ord(upcase(driveLetter))-ord('A');

  query.process := GetCurrentProcessID();
  query.thread := GetCurrentThreadID();

  DeviceIoControl(hBestCryptVxD,
                  IOCTL_PRIVATE_GET_BCDISK_INFO,
                  @query,
                  sizeof(query),
                  @query,
                  sizeof(query),
                  dwBytesReturned,
                  nil);

  if (query.signature<>OUTPUT_SIGNATURE) then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end
  else if (query.status<>BCSTATUS_SUCCESS) then
    begin
    LastErrorCode := OTFE_ERR_INVALID_DRIVE;
    end
  else
    begin
    LastErrorCode := OTFE_ERR_SUCCESS;
    end;

  volInfo := query;

  Result := (LastErrorCode = OTFE_ERR_SUCCESS);

end;



function TOTFEBestCrypt.GetDriveInfoNEW(driveLetter: char; var volInfo: TDISKINFO_BUFFER_driverNEW): boolean;
var
  dwBytesReturned : DWORD;
  query: TDISKINFO_BUFFER_driverNEW;
begin
  CheckActive();

  query.Signature := INPUT_SIGNATURE;
  query.diskNumber := ord(upcase(driveLetter))-ord('A');

  query.process := GetCurrentProcessID();
  query.thread := GetCurrentThreadID();

  DeviceIoControl(hBestCryptVxD,
                  IOCTL_PRIVATE_GET_BCDISK_INFO,
                  @query,
                  sizeof(query),
                  @query,
                  sizeof(query),
                  dwBytesReturned,
                  nil);

  if (query.signature<>OUTPUT_SIGNATURE) then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end
  else if (query.status<>BCSTATUS_SUCCESS) then
    begin
    LastErrorCode := OTFE_ERR_INVALID_DRIVE;
    end
  else
    begin
    LastErrorCode := OTFE_ERR_SUCCESS;
    end;

  volInfo := query;

  Result := (LastErrorCode = OTFE_ERR_SUCCESS);

end;


function TOTFEBestCrypt.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  dwBytesReturned : DWORD;
  query: TDISCONNECT_BUFFER;
  NTDriveHandle: THandle;
  NTDriveStr: string;
  NTOKToDismount: boolean;
begin
  CheckActive();

  driveLetter := upcase(driveLetter);

  query.Signature := INPUT_SIGNATURE;
  query.diskNumber := ord(driveLetter)-ord('A');
  query.fulldismounted := TRUE; // set to 1

  query.status := BCSTATUS_SUCCESS;

  query.dummy[1] := $00;
  query.dummy[2] := $00;
  query.dummy[3] := $00;

  query.dummy2[1] := $00;
  query.dummy2[2] := $00;
  query.dummy2[3] := $00;

//  query.dummy3[1] := $00;
//  query.dummy3[2] := $00;
//  query.dummy3[3] := $00;

  // If running under NT
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin

    // try and lock the drive
    NTDriveStr := '\\.\'+upcase(driveLetter)+':';
    NTDriveHandle := CreateFile(PChar(NTDriveStr),
                                (GENERIC_READ or GENERIC_WRITE),
                                (FILE_SHARE_READ or FILE_SHARE_WRITE),
                                nil,
                                OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL,
                                0);
    try
      FlushFileBuffers(NTDriveHandle);

      NTOKToDismount := DeviceIoControl(NTDriveHandle,
                                        FSCTL_LOCK_VOLUME,
                                        nil,
                                        0,
                                        nil,
                                        0,
                                        dwBytesReturned,
                                        nil);
      try
        DeviceIoControl(NTDriveHandle,
                        FSCTL_DISMOUNT_VOLUME,
                        nil,
                        0,
                        nil,
                        0,
                        dwBytesReturned,
                        nil);
                      
      finally
        DeviceIoControl(NTDriveHandle,
                        FSCTL_UNLOCK_VOLUME,
                        nil,
                        0,
                        nil,
                        0,
                        dwBytesReturned,
                        nil);
      end;

    finally
      CloseHandle(NTDriveHandle);
    end;

    if (NTOKToDismount) then
      begin
      // We can do a straight dismount
      query.fulldismounted := TRUE; // set to 1
      end
    else if (not(emergency)) then
      begin
      // Files open on the volume, and no emergency; abort dismount
      Result := FALSE;
      exit;
      end
    else
      begin
      // Files open on the volume, but it's an emergency; perform dismount,
      // but not full dismount
      query.fulldismounted := FALSE; // set to 1
      end;


    end
  else
    begin
    // Under 9x/Me, we just do a full dismount
    query.fulldismounted := TRUE; // set to 1
    end;

  DeviceIoControl(hBestCryptVxD,
                  IOCTL_PRIVATE_DISCONNECT_BCDISK,
                  @query,
                  sizeof(query),
                  @query,
                  sizeof(query),
                  dwBytesReturned,
                  nil);

  LastErrorCode := query.status;
  if query.signature<>OUTPUT_SIGNATURE then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end;

  SDUPause(500);

  Result := (query.status=BCSTATUS_SUCCESS);

  if not(Result) then
    begin
    FLastErrCode:= OTFE_ERR_DISMOUNT_FAILURE;
    end
  else
    begin
    BroadcastDriveChangeMessage(FALSE, driveLetter);
    end;

end;

function TOTFEBestCrypt.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  CheckActive();

  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


function TOTFEBestCrypt.IsEncryptedVolFile(volumeFilename: string): boolean;
var
  readInHeader: THiddenSector;
begin
  CheckActive();

  Result := FALSE;

  if GetBestCryptFileHeader(volumeFilename, readInHeader) then
    begin
    // Check the OEMid in the Bootrecord to see if this is a BestCrypt volume
    // file.
    // Note that this probably won't work if the user has change the boot record
    // OEM ID on their BestCrypt drive!
    Result := (readInHeader.BootRecord.OEMid = BESTCRYPT_VOLUME_DESCRIPTOR);
    end;

end;

function TOTFEBestCrypt.GetBestCryptFileHeader(volumeFilename: string; var header: THiddenSector): boolean;
var
  testFile: TFileStream;
begin
  Result := FALSE;

  try
    testFile := TFileStream.Create(volumeFilename, fmOpenRead OR fmShareDenyNone);
    try
      testFile.Read(header, sizeof(header));
      Result := TRUE;
    finally
      testFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;

function TOTFEBestCrypt.GetVolFileForDrive(driveLetter: char): string;
var
  queryOLD: TDISKINFO_BUFFER_driverOLD;
  queryNEW: TDISKINFO_BUFFER_driverNEW;
  filename: string;
begin
  CheckActive();

  filename := '';
  if (Version() < DRIVER_VERSION_DISKINFO_NEW) then
    begin
    if GetDriveInfoOLD(driveLetter, queryOLD) then
      begin
      // Start copying from the 2nd char to avoid the extra "\" at the start
      // Only copy up to the #0, i.e. only the number of chars 2 less than the
      // #0's pos
      filename := SDUConvertSFNtoLFN(copy(queryOLD.filename, 2, (pos(#0, queryOLD.filename)-2)));
      end;

    end
  else
    begin
    if GetDriveInfoNEW(driveLetter, queryNEW) then
      begin
      // Start copying from the 2nd char to avoid the extra "\" at the start
      // Only copy up to the #0, i.e. only the number of chars 2 less than the
      // #0's pos
      filename := SDUConvertSFNtoLFN(copy(queryNEW.filename, 2, (pos(#0, queryNEW.filename)-2)));
      end;

    end;

  Result := filename;
end;

function TOTFEBestCrypt.GetDriveForVolFile(volumeFilename: string): char;
var
  i: integer;
  mountedDrives: string;
  queryOLD: TDISKINFO_BUFFER_driverOLD;
  queryNEW: TDISKINFO_BUFFER_driverNEW;
begin
  mountedDrives := DrivesMounted();

  Result := #0;

  volumeFilename := uppercase(volumeFilename);

  for i:=1 to length(mountedDrives) do
    begin
    if uppercase(GetVolFileForDrive(mountedDrives[i]))=volumeFilename then
      begin
      if (Version() < DRIVER_VERSION_DISKINFO_NEW) then
        begin
        // HANDLE FOR v2 DRIVERS...
        if GetDriveInfoOLD(mountedDrives[i], queryOLD) then
          begin
          Result := chr(ord('A')+queryOLD.diskNumber);
          break;
          end;

        end
      else
        begin
        // HANDLE FOR v3 DRIVERS...
        if GetDriveInfoNEW(mountedDrives[i], queryNEW) then
          begin
          Result := chr(ord('A')+queryNEW.diskNumber);
          break;
          end;

        end;

      end;
    end;

end;


function TOTFEBestCrypt.GetAlgorithmName(algorithmID: DWORD): string;
var
  dispName: string;
  driverName: string;
  keyLen: integer;
begin
  Result := '';
  if GetAlgorithmRegDetails(algorithmID, dispName, driverName, keyLen) then
    begin
    Result := dispName;
    end;

end;


function TOTFEBestCrypt.GetAlgorithmDriverName(algorithmID: DWORD): string;
var
  dispName: string;
  driverName: string;
  keyLen: integer;
begin
  Result := '';
  if GetAlgorithmRegDetails(algorithmID, dispName, driverName, keyLen) then
    begin
    Result := driverName;
    end;

end;

function TOTFEBestCrypt.GetAlgorithmKeyLength(algorithmID: DWORD): integer;
var
  dispName: string;
  driverNAme: string;
  keyLen: integer;
begin
  Result := -1;
  if GetAlgorithmRegDetails(algorithmID, dispName, driverName, keyLen) then
    begin
    Result := keyLen;
    end;

end;


function TOTFEBestCrypt.GetKeyGenDLLName(keyGenID: DWORD): string;
var
  dispName: string;
  DLLName: string;
begin
  Result := '';
  if GetKeyGenRegDetails(keyGenID, dispName, DLLName) then
    begin
    Result := DLLName;
    end;

end;

function TOTFEBestCrypt.GetKeyGenName(keyGenID: DWORD): string;
var
  dispName: string;
  DLLName: string;
begin
  Result := '';
  if GetKeyGenRegDetails(keyGenID, dispName, DLLName) then
    begin
    Result := dispName;
    end;

end;


// Returns $FFFFFFFF on error
function TOTFEBestCrypt.GetKeyGenVersion(keyGenID: DWORD): cardinal;
var
  keyGenDLL: THandle;
  keyGenFilename: string;
  GetNameAndVersion: TBCGetNameAndVersion;
  majorVersion: DWORD;
  minorVersion: DWORD;
  keyGenName: string;
  nameBuffer: array [1..MAX_KEYGEN_NAME_LENGTH] of char;
begin
  FLastErrCode:= OTFE_ERR_SUCCESS;
  Result := $FFFFFFFF;

  keyGenFilename := GetKeyGenDLLName(keyGenID);

  if keyGenFilename = '' then
    begin
    // Just quit; Result already set to error value
    exit;
    end;

  keyGenDLL := LoadDLL(keyGenFilename);
  try
    @GetNameAndVersion := GetProcAddress(keyGenDLL, 'GetNameAndVersion');
    if @GetNameAndVersion=nil then
      begin
      raise EBestCryptBadDLL.Create(E_BESTCRYPT_BAD_DLL+
                                    ' ('+
                                    keyGenFilename+
                                    '; GetNameAndVersion)');
      end
    else
      begin
      if not(GetNameAndVersion(@nameBuffer, MAX_KEYGEN_NAME_LENGTH, @majorVersion, @minorVersion)) then
        begin
        raise EBestCryptDLLFailure.Create(E_BESTCRYPT_DLL_FAILURE+
                                      ' ('+
                                      keyGenFilename+
                                      '; GetNameAndVersion)');
        end
      else
        begin
        Result := (majorVersion * $100) + minorVersion;
        end;
      end;
  finally
    FreeLibrary(keyGenDLL);
  end;

  // The DLL's name as it is returned from the DLL. It would be nice to sanity
  // check here and verify this against the information held in the registry,
  // but it doesn't really matter that much
  keyGenName := copy(nameBuffer, 1, pos(#0, nameBuffer));

  if Result=$FFFFFFFF then
    begin
    FLastErrCode:= OTFE_ERR_UNKNOWN_ALGORITHM;
    end;

end;


function TOTFEBestCrypt.CreateKeyHandle(volumeFilename: string; var keyHandle: KEY_HANDLE): boolean;
var
  keyGenDLLFilename: string;
  keyGenDLL: THandle;
  CreateKeyHandle: TBCCreateKeyHandle;
  hiSe: THiddenSector;
  algorithmName: string;
  algorithmKeyLength: integer;
  keyReturned: KEY_HANDLE;
  errorCode: DWORD;
  pDataBlock: Pointer;
  dataBlockSize: DWORD;
  tmpFile: TFileStream;
  memBlock: Pointer;
begin
  FLastErrCode:= OTFE_ERR_SUCCESS;
  Result := FALSE;

  GetBestCryptFileHeader(volumeFilename, hiSe);

  algorithmKeyLength := GetAlgorithmKeyLength(hiSe.algorithmId);
  algorithmName := GetAlgorithmDriverName(hiSe.algorithmId);
  if (algorithmKeyLength=-1) OR (algorithmName='') then
    begin
    // Error code set in relevant function call above
    exit;
    end;

  dataBlockSize:= hiSe.dwKeySize;

  // Read the keyblock in from the volume file
  GetMem(memBlock, dataBlockSize);
  try
    tmpFile := TFileStream.Create(volumeFilename, fmOpenRead OR fmShareDenyNone);
    try
      tmpFile.Seek(HIDDEN_SECTOR_SIZE, soFromBeginning);
      tmpFile.Read(memBlock^, dataBlockSize);
    finally
      tmpFile.Free();
    end;

    pDataBlock:= memBlock;

    keyGenDLLFilename := GetKeyGenDLLName(hiSe.keyGenId);

    keyGenDLL := LoadDLL(keyGenDLLFilename);
    try
      @CreateKeyHandle := GetProcAddress(keyGenDLL, 'CreateKeyHandle');
      if @CreateKeyHandle=nil then
        begin
        MessageDlg(
                   'Unable to locate CreateKeyHandle within BestCrypt Key '+#13+#10+'generator DLL "'+keyGenDLLFilename+'"',
                   mtError,
                   [mbOK],
                   0
                  );
        FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
        end
      else
        begin
        if CreateKeyHandle(PChar(algorithmName),
                           hiSe.algorithmId,
                           DWORD(algorithmKeyLength),
                           PChar(volumeFilename),
                           PChar(FPasswordPrompt),
                           CFLAG_VERIFY_AND_LOAD_KEY,
                           @pDataBlock,
                           @dataBlockSize,
                           @keyReturned,
                           @errorCode,
                           HWND(nil)
                          ) then
          begin
          keyHandle := keyReturned;
          Result := TRUE;
          end
        else
          begin
          if errorCode<>BCKEYGEN_ERROR_NO then
            begin
            Result := FALSE;
            FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
            if errorCode=BCKEYGEN_ERROR_INCORRECT_PASSWORD then
              begin
              FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
              end
            else if errorCode=BCKEYGEN_ERROR_CANCELLED_BY_USER then
              begin
              FLastErrCode:= OTFE_ERR_USER_CANCEL;
              end
            end;
          end;

        end;
    finally
      FreeLibrary(keyGenDLL);
    end;

  finally
    FreeMem(memBlock);
  end;

end;


function  TOTFEBestCrypt.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
var
  i: integer;
  tmpDrv: char;
begin
  mountedAs := '';

  Result := FALSE;

  for i:=0 to (volumeFilenames.count-1) do
    begin
    // Don't ask, just don't ask...
    // (kludge because for loop doesn't always terminate when it should,
    // causing a "List out of bounds" error when trying to get a filename
    // that doesn't exist - first noticed after installing BestCrypt v6.07.2)
    // xxx - this should be sorted out properly...
    if (i>(volumeFilenames.count-1)) then
      begin
      exit;
      end;

    tmpDrv := Mount(volumeFilenames[i], readonly);
    mountedAs := mountedAs + tmpDrv;

    if tmpDrv<>#0 then
      begin
      Result := TRUE;
      end;

    end;

end;


function TOTFEBestCrypt.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  readInHeader: THiddenSector;
  volFileHandle: THandle;
  previousSizeLow: DWORD;
  driveDlg: TGetDriveLetter_F;
  defDrv: char;
  driveLetter: char;
  diskSizeLow, diskSizeHigh: DWORD;
begin
  CheckActive();
  FLastErrCode:= OTFE_ERR_UNKNOWN_ERROR;

  Result := #0;

  driveLetter := #0;

  // If we were not supplied with a drive letter and either we are not using
  // the default volume drive letter assignment, or such an assignment doesn't
  // exist, prompt the user for the drive letter for the volume file to be
  // mounted on
  driveDlg := TGetDriveLetter_F.Create(nil);
  try
    driveDlg.Filename := volumeFilename;
    defDrv := GetDefaultDrive(volumeFilename);

    driveDlg.DefaultDrive := defDrv;
    driveDlg.cbAlwaysThisDrive.checked := (defDrv<>#0);

    driveDlg.ForceDriveLetter := VolumeInternallyMounted(volumeFilename);
    driveDlg.ProhibitedDriveLetters := InternalDrivesMounted();

    driveDlg.AutoMountOptionAlwaysEnabled := RegistryOnlyVersion();
    driveDlg.cbAutomount.checked := VolumeIsAutomounted(volumeFilename);

    if driveDlg.ShowModal()=mrOK then
      begin
      driveLetter := driveDlg.cbDriveLetter.text[1];
      if driveDlg.cbAlwaysThisDrive.checked then
        begin
        SetDefaultDriveAutomount(volumeFilename, driveLetter, driveDlg.cbAutomount.checked);
        end
      else
        begin
        SetDefaultDriveAutomount(volumeFilename, #0, driveDlg.cbAutomount.checked);
        end;
      end
    else
      begin
      FLastErrCode:= OTFE_ERR_USER_CANCEL;
      end;
  finally
    driveDlg.Free();
  end;

  if driveLetter=#0 then
    begin
    // Result already set to #0, so just exit
    exit;
    end;

  GetBestCryptFileHeader(volumeFilename, readInHeader);

  volFileHandle := CreateFile(PChar(volumeFilename),
                              GENERIC_READ,
                              0,
                              nil,
                              OPEN_EXISTING,
                              FILE_ATTRIBUTE_NORMAL,
                              0);
  if (volFileHandle <> INVALID_HANDLE_VALUE) then
    begin
    diskSizeLow := GetFileSize(volFileHandle, @diskSizeHigh);
    CloseHandle(volFileHandle);

    // Now subtract the start of the data from the file's size
    // This needs to be done as follows to ensure that it is done correctly;
    // if the start of the encrypted disk data is > the low DWORD of the file size
    previousSizeLow := diskSizeLow;
    diskSizeLow := diskSizeLow-readInHeader.dwDataOffset;
    if previousSizeLow<=diskSizeLow then
      begin
      dec(diskSizeHigh)
      end;


    if (Version() < DRIVER_VERSION_DISKINFO_NEW) then
      begin
      // HANDLE FOR v2 DRIVERS...
      Result := MountDriverOLD(volumeFilename, readonly, driveLetter, diskSizeLow, diskSizeHigh, readInHeader);
      end
    else
      begin
      // HANDLE FOR v3 DRIVERS...
      Result := MountDriverNEW(volumeFilename, readonly, driveLetter, diskSizeLow, diskSizeHigh, readInHeader);
      end;

    end;

end;

function TOTFEBestCrypt.MountDriverOLD(volumeFilename: string; readonly: boolean; driveLetter: char; diskSizeLow, diskSizeHigh: DWORD; readInHeader: THiddenSector): char;
var
  dwBytesReturned : DWORD;
  queryOLD: TDISKINFO_BUFFER_driverOLD;
  i: integer;
  algorithmName: string;
  keyHandle: KEY_HANDLE;
  volumeOnDrive: string;
  bytesPerSector: DWORD;
  junk1: DWORD;
  junk2: DWORD;
  junk3: DWORD;
  mountedDriveLetter: char;
begin
  CheckActive();

  Result := #0;

  queryOLD.Signature := INPUT_SIGNATURE;

  if not(CreateKeyHandle(volumeFilename, keyHandle)) then
    begin
    // FLastErrCode set in the above function call, if needed
    end
  else
    begin
    queryOLD.keyHandle := keyHandle;

    queryOLD.diskSizeLow := diskSizeLow;
    queryOLD.diskSizeHigh := diskSizeHigh;

    queryOLD.dummy[1] := $5c;
    queryOLD.dummy[2] := $3f;
    queryOLD.dummy[3] := $3f;

    volumeOnDrive := copy(volumeFilename, 1, 3);
    volumeFilename := '\' + volumeFilename;
    for i:=1 to length(volumeFilename) do
      begin
      queryOLD.filename[i] := volumeFilename[i];
      end;
    queryOLD.filename[length(volumeFilename)+1] := #0;

    queryOLD.diskNumber := ord(upcase(driveLetter))-ord('A');

    algorithmName := '\Device\' + GetAlgorithmDriverName(readInHeader.algorithmId);

    for i:=1 to length(algorithmName) do
      begin
      queryOLD.algDevice[i] := algorithmName[i];
      end;
    queryOLD.algDevice[length(algorithmName)+1] := #0;

    queryOLD.algorithmID := readInHeader.algorithmId;

    if readonly then
      begin
      queryOLD.readOnly := BESTCRYPT_DWORD_TRUE;
      end
    else
      begin
      queryOLD.readOnly := BESTCRYPT_DWORD_FALSE;
      end;

    queryOLD.process := 0; // BestCrypt v6.06 control panel sets this to 0 when it mounts volumes
    queryOLD.thread := 0; // BestCrypt v6.06 control panel sets this to 0 when it mounts volumes


//    query.sectorSize := readInHeader.bootRecord.bpb.sectSize;
// that last query.sectorSize is wrong; it should be set to the sectorsize of
// the *host* drive
    if GetDiskFreeSpace(PChar(volumeOnDrive), junk1, bytesPerSector, junk2, junk3) then
      begin
      queryOLD.sectorSize := bytesPerSector;
      end
    else
      begin
      // xxx - set error code here
      MessageDlg(
                 'Unable to obtain sectorsize for drive: '+volumeOnDrive+CRLF+
                 CRLF+
                 'Assuming 512 bytes/sector.',
                 mtWarning,
                 [mbOK],
                 0
                );

      // <shrugs> Well, no harm in giving it a go, I guess...
      queryOLD.sectorSize := 512;
      end;

    queryOLD.fullDismounted := TRUE; // set to 1

    queryOLD.dataOffset := readInHeader.dwDataOffset;

    DeviceIoControl(hBestCryptVxD,
                    IOCTL_PRIVATE_MAP_BCDISK,
                    @queryOLD,
                    sizeof(queryOLD),
                    @queryOLD,
                    sizeof(queryOLD),
                    dwBytesReturned,
                    nil);

    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    if (queryOLD.signature<>OUTPUT_SIGNATURE) then
      begin
      raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
      end
    else if (queryOLD.status=BCSTATUS_SUCCESS) then
      begin
      mountedDriveLetter := chr(queryOLD.diskNumber+ord('A'));
      BroadcastDriveChangeMessage(TRUE, mountedDriveLetter);
      FLastErrCode:= OTFE_ERR_SUCCESS;
      Result := mountedDriveLetter;
      end;

    end;

end;


function TOTFEBestCrypt.MountDriverNEW(volumeFilename: string; readonly: boolean; driveLetter: char; diskSizeLow, diskSizeHigh: DWORD; readInHeader: THiddenSector): char;
var
  dwBytesReturned : DWORD;
  queryNEW: TDISKINFO_BUFFER_driverNEW;
  i: integer;
  algorithmName: string;
  keyHandle: KEY_HANDLE;
  volumeOnDrive: string;
  bytesPerSector: DWORD;
  junk1: DWORD;
  junk2: DWORD;
  junk3: DWORD;
  mountedDriveLetter: char;
begin
  CheckActive();

  Result := #0;

  queryNEW.Signature := INPUT_SIGNATURE;

  if not(CreateKeyHandle(volumeFilename, keyHandle)) then
    begin
    // FLastErrCode set in the above function call, if needed
    end
  else
    begin
    queryNEW.keyHandle := keyHandle;

    queryNEW.diskSizeLow := diskSizeLow;
    queryNEW.diskSizeHigh := diskSizeHigh;

    queryNEW.dummy[1] := $5c;
    queryNEW.dummy[2] := $3f;
    queryNEW.dummy[3] := $3f;

    volumeOnDrive := copy(volumeFilename, 1, 3);
    volumeFilename := '\' + volumeFilename;
    for i:=1 to length(volumeFilename) do
      begin
      queryNEW.filename[i] := volumeFilename[i];
      end;
    queryNEW.filename[length(volumeFilename)+1] := #0;

    queryNEW.diskNumber := ord(upcase(driveLetter))-ord('A');

    algorithmName := '\Device\' + GetAlgorithmDriverName(readInHeader.algorithmId);

    for i:=1 to length(algorithmName) do
      begin
      queryNEW.algDevice[i] := algorithmName[i];
      end;
    queryNEW.algDevice[length(algorithmName)+1] := #0;

    queryNEW.algorithmID := readInHeader.algorithmId;

    if readonly then
      begin
      queryNEW.readOnly := BESTCRYPT_DWORD_TRUE;
      end
    else
      begin
      queryNEW.readOnly := BESTCRYPT_DWORD_FALSE;
      end;

    queryNEW.process := 0; // BestCrypt v6.06 control panel sets this to 0 when it mounts volumes
    queryNEW.thread := 0; // BestCrypt v6.06 control panel sets this to 0 when it mounts volumes


//    query.sectorSize := readInHeader.bootRecord.bpb.sectSize;
// that last query.sectorSize is wrong; it should be set to the sectorsize of
// the *host* drive
    if GetDiskFreeSpace(PChar(volumeOnDrive), junk1, bytesPerSector, junk2, junk3) then
      begin
      queryNEW.sectorSize := bytesPerSector;
      end
    else
      begin
      // xxx - set error code here
      MessageDlg(
                 'Unable to obtain sectorsize for drive: '+volumeOnDrive+CRLF+
                 CRLF+
                 'Assuming 512 bytes/sector.',
                 mtWarning,
                 [mbOK],
                 0
                );

      // <shrugs> Well, no harm in giving it a go, I guess...
      queryNEW.sectorSize := 512;
      end;

    queryNEW.fullDismounted := TRUE; // set to 1

    queryNEW.dataOffset := readInHeader.dwDataOffset;


    // New items for v3 drivers and later...
    queryNEW.UserId := GetCurrentUserID();
    queryNEW.HarddiskVolumeNumber := 0; // xxx - ???


    DeviceIoControl(hBestCryptVxD,
                    IOCTL_PRIVATE_MAP_BCDISK,
                    @queryNEW,
                    sizeof(queryNEW),
                    @queryNEW,
                    sizeof(queryNEW),
                    dwBytesReturned,
                    nil);

    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    if (queryNEW.signature<>OUTPUT_SIGNATURE) then
      begin
      raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
      end
    else if (queryNEW.status=BCSTATUS_SUCCESS) then
      begin
      mountedDriveLetter := chr(queryNEW.diskNumber+ord('A'));
      BroadcastDriveChangeMessage(TRUE, mountedDriveLetter);
      FLastErrCode:= OTFE_ERR_SUCCESS;
      Result := mountedDriveLetter;
      end;

    end;

end;


// This is ported from the function received in email from Sergey Frolov
function TOTFEBestCrypt.GetCurrentUserID(): DWORD;
const
  // This algorithm only uses up to a max of 32 chars
  MAX_USERNAME_CHARS_USED = 32;
var
  currentUserId: DWORD;
  position: DWORD;
  i: integer;
  usernameLength: DWORD;
  arrUserName: array [1..MAX_USERNAME_CHARS_USED] of char;
begin
  currentUserId := USER_ID_SUITABLE_FOR_ALL_USERS;

  usernameLength := MAX_USERNAME_CHARS_USED;
  if GetUserName(@arrUserName, usernameLength) then
    begin
    position := 1;
    for i:=0 to 9 do
      begin
      // calculate user Id
      if (arrUserName[i+1] = #0) then
        begin
        break;
        end;

      currentUserId := currentUserId + ord(arrUserName[i+1]) + position;

      position := position * 10;
      end;  // for i:=0 to 9 do

    end;  // if GetUserName(@arrUserName, usernameLength) then

  Result := currentUserId;
end;



function TOTFEBestCrypt.LoadDLL(DLLFilename: string): THandle;
var
  theDLL: THandle;
  loadFilename: string;
begin
  loadFilename := DLLFilename;
  theDLL := LoadLibrary(PChar(loadFilename));
  if theDLL=0 then
    begin
    // DLL couldn't be loaded; maybe it isn't on the path? Try again, looking
    // in the BestCrypt directory
    loadFilename := ExtractFilePath(GetMainExe())+DLLFilename;
    theDLL := LoadLibrary(PChar(loadFilename));
    if theDLL=0 then
      begin
      FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
      raise EBestCryptDLLNotFound.Create(E_BESTCRYPT_DLL_NOT_FOUND+
                                         ' ('+
                                         DLLFilename+
                                         ')');
      end;
    end;

  Result := theDLL;

end;

// Returns default drive letter volume gets mounted as
// If the letter is in UPPERCASE, it's automounted
// If the letter is in LOWERCASE, it's NOT automounted
// Returns #0 if there is no default drive
// WARNING: If were running with a version of BestCrypt that stores all of
//          it's settings in the registry, and nothing in an INI file
//          (BestCrypt later than v6.07.2), this function will call
//          VolumeIsAutomounted(...)
function TOTFEBestCrypt.GetDefaultDrive(volumeFilename: string): char;
var
  registry: TRegistry;
  keyName: string;
  tmpDriveString: string;
  driveLetter: char;
  filenameASCIIHash: string;
begin
  if RegistryOnlyVersion() then
    begin
    filenameASCIIHash := GetFilenameASCIIHash(volumeFilename);

    driveLetter := #0;

    registry := TRegistry.create();
    try
      registry.RootKey := HKEY_CURRENT_USER;
      keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\MountPoint';
      if registry.OpenKeyReadOnly(keyName) then
        begin
        tmpDriveString := registry.ReadString(filenameASCIIHash);
        registry.CloseKey();
        if (tmpDriveString<>'') then
          begin
          // Ensure that it's always lowercase (no automount)
          tmpDriveString := lowercase(tmpDriveString);
          driveLetter := tmpDriveString[1];
          end;
        end;
    finally
      registry.Free();
    end;


    // Default is lowercase (see above); uppercase the driveletter is
    // AutoMounted
    if VolumeIsAutomounted(volumeFilename) then
      begin
      driveLetter := upcase(driveLetter);
      end;

    Result := driveLetter;
    end
  else
    begin
    Result := INIGetDefaultDrive(volumeFilename);
    end;

end;


// For old versions of BestCrypt, returns default drive letter volume gets
// mounted as
// Returns #0 if there is no default drive
// NOTE: If the letter is in UPPERCASE, it's automounted
//       If the letter is in LOWERCASE, it's NOT automounted
// WARNING: If were running with a version of BestCrypt that stores some of
//          it's settings in an INI file (BestCrypt prior to v6.07.2), this
//          function will be called by VolumeIsAutomounted(...)

function TOTFEBestCrypt.INIGetDefaultDrive(volumeFilename: string): char;
var
  iniFile: TINIFile;
  strTemp: string;
  containerNum: integer;
begin
  Result := #0;

  iniFile := TINIFile.Create(BESTCRYPT_INI_FILENAME);
  try
    containerNum := INIGetAutomountContainerNum(volumeFilename);

    if containerNum<>-1 then
      begin
      strTemp := iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), '');
      Result := strTemp[1];
      end;

  finally
    iniFile.Free();
  end;

end;

// Setup wether the specified volume filename is automounted, and it's default
// driver (#0 for no default drive)
function TOTFEBestCrypt.SetDefaultDriveAutomount(volumeFilename: string; driveLetter: char; automount: boolean): boolean;
begin
  if RegistryOnlyVersion() then
    begin
    Result := TRUE;

    if not(RegistryVolumeSetDefaultDrive(volumeFilename, driveLetter)) then
      begin
      MessageDlg(
                 'Unable to set default drive for volume "'+volumeFilename+'"',
                 mtError,
                 [mbOK],
                 0
                );
      Result := FALSE;
      end;

    if not(RegistryVolumeSetAutomounted(volumeFilename, automount)) then
      begin
      MessageDlg(
                 'Unable to set volume to be automounted for volume "'+volumeFilename+'"',
                 mtError,
                 [mbOK],
                 0
                );
      Result := FALSE;
      end;

    end
  else
    begin
    // If there's no default driveletter, delete default drive (automount
    // irrelevant)
    if (driveLetter=#0) then
      begin
      Result := DeleteDefaultDrive(volumeFilename);
      end
    else
      begin
      Result := INISetDefaultDriveAutomount(volumeFilename, driveLetter, automount);
      end;

    end;

end;


function TOTFEBestCrypt.INISetDefaultDriveAutomount(volumeFilename: string; driveLetter: char; automount: boolean): boolean;
var
  iniFile: TINIFile;
  containerNum: integer;
  keyValue: string;
begin
  Result := TRUE;

  if automount then
    begin
    driveLetter := upcase(driveLetter);
    end
  else
    begin
    driveLetter := (lowercase(driveLetter))[1];
    end;

  iniFile := TINIFile.Create(BESTCRYPT_INI_FILENAME);
  try
    containerNum := INIGetAutomountContainerNum(volumeFilename);

    if containerNum<>-1 then
      begin
      iniFile.WriteString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), driveLetter+volumeFilename);
      end
    else
      begin
      // Create a new key

      containerNum := 0;
      keyValue := iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), '');

      while keyValue<>'' do
        begin
        inc(containerNum);
        keyValue := iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), '');
        end;

      iniFile.WriteString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), driveLetter+volumeFilename);
      end;

  finally
    iniFile.Free();
  end;

end;



function TOTFEBestCrypt.INIGetAutomountContainerNum(volumeFilename: string): integer;
var
  sectionKeys: TStringList;
  i: integer;
  iniFile: TINIFile;
  strTemp: string;
begin
  Result := -1;

  volumeFilename := uppercase(volumeFilename);

  sectionKeys := TStringList.Create();
  try
    iniFile := TINIFile.Create(BESTCRYPT_INI_FILENAME);
    try
      // This would be considerably neater if it just used
      // TINIFile.ReadSectionValues - but for some strange reason, using this
      // causes it to choke later on... Weird...

      // iniFile.ReadSectionValues(kBestCryptINIFileAutoOpenKey, sectionKeys);

      i:=0;
      while i<>-1 do
        begin
        strTemp := uppercase(iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(i), ''));
        if strTemp='' then
          begin
          i:=-1
          end
        else
          begin
          sectionKeys.add(strTemp);
          inc(i);
          end;
        end;
    finally
      iniFile.Free();
    end;

    for i:=0 to (sectionKeys.count-1) do
      begin
      if pos(volumeFilename, sectionKeys[i])=2 then
        begin
        Result := i;
        end;
      end;

  finally
    sectionKeys.Free();
  end;

end;

function TOTFEBestCrypt.DeleteDefaultDrive(volumeFilename: string): boolean;
begin
  Result := INIDeleteDefaultDrive(volumeFilename);
end;


function TOTFEBestCrypt.INIDeleteDefaultDrive(volumeFilename: string): boolean;
var
  iniFile: TINIFile;
  containerNum: integer;
  keyValue: string;
begin
  Result := TRUE;

  iniFile := TINIFile.Create(BESTCRYPT_INI_FILENAME);
  try
    containerNum := INIGetAutomountContainerNum(volumeFilename);

    keyValue := iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum+1), '');

    while keyValue<>'' do
      begin
      iniFile.WriteString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum), keyValue);
      inc(containerNum);
      keyValue := iniFile.ReadString(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum+1), '');
      end;

    iniFile.DeleteKey(kBestCryptINIFileAutoOpenKey, 'Container'+inttostr(containerNum));

  finally
    iniFile.Free();
  end;

end;

procedure TOTFEBestCrypt.GetAllAlgorithmIDs(algIDs: TStrings);
var
  registry: TRegistry;
  subKeys: TStringList;
  i: integer;
  currID: DWORD;
  keyName: string;
begin
  CheckActive();

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\Algorithms';
    if registry.OpenKeyReadOnly(keyName) then
      begin
      subKeys := TStringList.Create();
      try
        registry.GetKeyNames(subKeys);
        registry.CloseKey();
        for i:=0 to (subKeys.count-1) do
          begin
          registry.OpenKeyReadOnly(keyName + '\' + subKeys[i]);
          currID := registry.ReadInteger('AlgorithmID');
          algIDs.add(inttostr(currID));
          registry.CloseKey();
          end;
      finally
        subKeys.Free();
      end;
      end;
  finally
    registry.Free();
  end;
end;


procedure TOTFEBestCrypt.GetAllKeyGenIDs(keyGenIDs: TStrings);
var
  registry: TRegistry;
  subKeys: TStringList;
  i: integer;
  currID: DWORD;
  keyName: string;
begin
  CheckActive();

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\KeyGenerators';
    if registry.OpenKeyReadOnly(keyName) then
      begin
      subKeys := TStringList.Create();
      try
        registry.GetKeyNames(subKeys);
        registry.CloseKey();
        for i:=0 to (subKeys.count-1) do
          begin
          registry.OpenKeyReadOnly(keyName + '\' + subKeys[i]);
          currID := registry.ReadInteger('KeyGenID');
          keyGenIDs.add(inttostr(currID));
          registry.CloseKey();
          end;
      finally
        subKeys.Free();
      end;
      end;
  finally
    registry.Free();
  end;
end;

function TOTFEBestCrypt.GetAlgorithmVersion(algorithmID: DWORD): cardinal;
var
  dwBytesReturned : DWORD;
  query: TVERSION_BUFFER;
  algHandle: THandle;
begin
  CheckActive();

  algHandle := CreateFile(PChar('\\.\'+GetAlgorithmDriverName(algorithmID)),
                              GENERIC_READ OR GENERIC_WRITE,
                              0,
                              nil,
                              OPEN_EXISTING,
                              FILE_ATTRIBUTE_NORMAL,
                              0);
  if algHandle = INVALID_HANDLE_VALUE then
    begin
    FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptVxdNotFound.Create('BestCrypt algorithm device driver not found');
    end;

  Result := $FFFFFFFF;

  query.Signature := INPUT_SIGNATURE;

  DeviceIoControl(algHandle,
                  IOCTL_PRIVATE_GET_VERSION_INFO,
                  @query,
                  sizeof(query),
                  @query,
                  sizeof(query),
                  dwBytesReturned,
                  nil);

  LastErrorCode := query.status;
  if query.signature<>OUTPUT_SIGNATURE then
    begin
    FLastErrCode := OTFE_ERR_DRIVER_FAILURE;
    raise EBestCryptDLLBadSignature.Create(E_BESTCRYPT_DLL_SIGNATURE_FAILURE);
    end;

  FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
  if query.status=0 then
    begin
    FLastErrCode:= OTFE_ERR_SUCCESS;
    Result := (query.Major_version * $100) + query.Minor_version;
    end;

end;

function TOTFEBestCrypt.GetVolumeInfo(volumeFilename: string; var info: TBCDiskInfo): boolean;
var
  hi: THiddenSector;
  diOLD: TDISKINFO_BUFFER_driverOLD;
  diNEW: TDISKINFO_BUFFER_driverNEW;
  mtdAs: char;
  tmpFile: TFileStream;
begin
  volumeFilename := SDUConvertSFNToLFN(volumeFilename);
  info.volumeFilename := volumeFilename;

  mtdAs := GetDriveForVolFile(volumeFilename);
  info.mountedAs := mtdAs;
  info.readOnly := FALSE;
  if mtdAs<>#0 then
    begin
    if (Version() < DRIVER_VERSION_DISKINFO_NEW) then
      begin
      // HANDLE FOR v2 DRIVERS...
      GetDriveInfoOLD(mtdAs, diOLD);
      info.readOnly := (diOLD.readOnly<>0);
      end
    else
      begin
      // HANDLE FOR v3 DRIVERS...
      GetDriveInfoNEW(mtdAs, diNEW);
      info.readOnly := (diNEW.readOnly<>0);
      end;

    end;

  tmpFile := TFileStream.Create(volumeFilename, fmOpenRead OR fmShareDenyNone);
  try
    tmpFile.Seek(BC_FILE_ERRORS_IN_MOUNTING_OFFSET, soFromBeginning);
    tmpFile.Read(info.errsInMounting, 1);
  finally
    tmpFile.Free();
  end;

  GetBestCryptFileHeader(volumeFilename, hi);

  info.algorithmID     := hi.algorithmId;
  info.algorithmName   := GetAlgorithmName(hi.algorithmId);
  info.algorithmKeyLen := GetAlgorithmKeyLength(hi.algorithmId);
  info.keyGenID        := hi.keyGenId;
  info.keyGenName      := GetKeyGenName(hi.keyGenId);
  info.description     := hi.description;
  info.extent          := hi.extent;
  info.version         := hi.version;
  info.fileSystemID    := hi.fileSystemId;

  Result := TRUE;
end;

function TOTFEBestCrypt.GetVolumeInfo(driveLetter: char; var info: TBCDiskInfo): boolean;
var
  volumeFilename: string;
begin
  Result := FALSE;
  volumeFilename := GetVolFileForDrive(driveLetter);
  if (volumeFilename<>'') then
    begin
    Result := GetVolumeInfo(volumeFilename, info);
    end;

end;

function TOTFEBestCrypt.VersionStr(): string;
var
  verNo: cardinal;
  majorVer: integer;
  minorVer: integer;
begin
  verNo := Version();
  majorVer := (verNo AND $FF00) div $FF;
  minorVer := (verNo AND $FF);

  Result := Format('v%d.%d', [majorVer, minorVer]);

end;


function TOTFEBestCrypt.GetAlgorithmRegDetails(algorithmID: DWORD; var dispName: string; var driverName: string; var keyLen: integer): boolean;
var
  registry: TRegistry;
  subKeys: TStringList;
  i: integer;
  currID: DWORD;
  keyName: string;
begin
  Result := FALSE;
  FLastErrCode:= OTFE_ERR_UNKNOWN_ALGORITHM;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\Algorithms';
    if registry.OpenKeyReadOnly(keyName) then
      begin
      subKeys := TStringList.Create();
      try
        registry.GetKeyNames(subKeys);
        registry.CloseKey();
        for i:=0 to (subKeys.count-1) do
          begin
          registry.OpenKeyReadOnly(keyName + '\' + subKeys[i]);
          currID := registry.ReadInteger('AlgorithmID');
          if currID = algorithmID then
            begin
            dispName := registry.ReadString('DisplayName');
            driverName := subKeys[i];
            keyLen := registry.ReadInteger('KeyLength');
            Result := TRUE
            end;
          registry.CloseKey();
          end;
      finally
        subKeys.Free();
      end;
      end;
  finally
    registry.Free();
  end;

  if Result then
    begin
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

end;


function TOTFEBestCrypt.GetKeyGenRegDetails(keyGenID: DWORD; var dispName: string; var DLLName: string): boolean;
var
  registry: TRegistry;
  subKeys: TStringList;
  i: integer;
  currID: DWORD;
  keyName: string;
begin
  Result := FALSE;
  FLastErrCode:= OTFE_ERR_UNKNOWN_KEYGEN;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\KeyGenerators';
    if registry.OpenKeyReadOnly(keyName) then
      begin
      subKeys := TStringList.Create();
      try
        registry.GetKeyNames(subKeys);
        registry.CloseKey();
        for i:=0 to (subKeys.count-1) do
          begin
          registry.OpenKeyReadOnly(keyName + '\' + subKeys[i]);
          currID := registry.ReadInteger('KeyGenID');
          if currID = keyGenID then
            begin
            dispName := registry.ReadString('DisplayName');
            DLLName  := registry.ReadString('ImagePath');
            Result := TRUE;
            end;
          registry.CloseKey();
          end;
      finally
        subKeys.Free();
      end;
      end;
  finally
    registry.Free();
  end;

  if Result then
    begin
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

end;

function TOTFEBestCrypt.GetMainExe(): string;
var
  registry: TRegistry;
  keyName: string;
  appName: string;
begin
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  Result := '';
  appName := BESTCRYPT_APP_EXE_NAME;

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

// 17-November-2000 release, BestCrypt Control Panel v.6.07.2, Windows 95/98
// driver v2.40, Windows NT driver v2.18 - from Jetico WWW site
function TOTFEBestCrypt.RegistryOnlyVersion(): boolean;
var
  ver: cardinal;
begin
  ver := Version();
  Result := ( ((Win32Platform = VER_PLATFORM_WIN32_NT) and (ver>=$212)) or
              ((Win32Platform <> VER_PLATFORM_WIN32_NT) and (ver>=$229)) );
end;


// Return TRUE if the volume specified is automounted, otherwise FALSE
// WARNING: If were running with a version of BestCrypt that stores some of
//          it's settings in an INI file (BestCrypt prior to v6.07.2), this
//          function will call INIGetDefaultDrive
function  TOTFEBestCrypt.VolumeIsAutomounted(volumeFilename: string): boolean;
var
  defDrv: char;
  registry: TRegistry;
  subKeys: TStringList;
  i: integer;
  keyName: string;
begin
  Result := FALSE;

  if not(RegistryOnlyVersion()) then
    begin
    defDrv := INIGetDefaultDrive(volumeFilename);
    if defDrv<>#0 then
      begin
      Result := (defDrv=uppercase(defDrv));
      end;

    end
  else
    begin
    volumeFilename := uppercase(volumeFilename);

    registry := TRegistry.create();
    try
      registry.RootKey := HKEY_CURRENT_USER;
      keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\AutoMount';
      if registry.OpenKeyReadOnly(keyName) then
        begin
        subKeys := TStringList.Create();
        try
          registry.GetValueNames(subKeys);
          registry.CloseKey();
          for i:=0 to (subKeys.count-1) do
            begin
            if (uppercase(subKeys[i]) = volumeFilename) then
              begin
              Result := TRUE;
              exit;
              end;
            end;
        finally
          subKeys.Free();
        end;
        end;
    finally
      registry.Free();
    end;
    end;

end;


// Set the default drive for the specified volume (later, registry-only versions
// of BestCrypt; with no INI file)
// If driveLetter is #0, delete the default drive
function TOTFEBestCrypt.RegistryVolumeSetDefaultDrive(volumeFilename: string; driveLetter: char): boolean;
var
  registry: TRegistry;
  keyName: string;
  filenameASCIIHash: string;
  driveString: string;
begin
  Result := FALSE;

  filenameASCIIHash := GetFilenameASCIIHash(volumeFilename);

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_CURRENT_USER;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\MountPoint';
    if registry.OpenKey(keyName, TRUE) then
      begin

      if (driveLetter=#0) then
        begin
        registry.deletevalue(filenameASCIIHash);
        end
      else
        begin
        driveString := uppercase(driveLetter)+':\';
        registry.WriteString(filenameASCIIHash, driveString);
        end;

      registry.CloseKey();
      Result := TRUE;
      end;
  finally
    registry.Free();
  end;

end;


// Set the specified volume to be automounted (later, registry-only versions
// of BestCrypt; with no INI file)
function TOTFEBestCrypt.RegistryVolumeSetAutomounted(volumeFilename: string; automount: boolean): boolean;
var
  registry: TRegistry;
  keyName: string;
  nothingBuffer: string;
begin
  Result := FALSE;

  if (VolumeIsAutomounted(volumeFilename) = automount) then
    begin
    // Nothing to do - all dome already!
    Result := TRUE;
    exit;
    end;

  volumeFilename := SDUConvertSFNToLFN(volumeFilename);
  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_CURRENT_USER;
    keyName := BESTCRYPT_REGKEY_BESTCRYPT + '\AutoMount';
    if registry.OpenKey(keyName, TRUE) then
      begin
      if (automount) then
        begin
        nothingBuffer := '';
        registry.WriteBinaryData(volumeFilename, nothingBuffer, 0);
        end
      else
        begin
        registry.DeleteValue(volumeFilename);
        end;
      registry.CloseKey();
      Result := TRUE;
      end;
  finally
    registry.Free();
  end;

end;

// Return the SHA-1 hash of the specified filename, for use with the registry
// settings introduced in BestCrypt v6.07.2
function  TOTFEBestCrypt.GetFilenameASCIIHash(volumeFilename: string): string;
var
  hashBuffSize: integer;
  written: integer;
  codedHash: string;
  digest: THashArray;
  hashSHA1: THashAlgSHA1;
begin
  // BestCrypt registry settings use LFNs
  volumeFilename := SDUConvertSFNtoLFN(volumeFilename);

  hashSHA1:= THashAlgSHA1.Create(nil);
  try
    digest := hashSHA1.HashString(volumeFilename);

    hashBuffSize := ( (hashSHA1.HashLength div 8) // BestCrypt hashSize in bytes
                      * 2 + 1 );

    written := hashBuffSize;

    if not(SE_EncodeString(codedHash, written, digest, (hashSHA1.HashLength div 8))) then
      begin
      raise EOTFEException.Create(E_BESTCRYPT_UNABLE_TO_CREATE_FILENAME_HASH);
      end;

    Result := codedHash;

  finally
    hashSHA1.Free();
  end;

end;


function TOTFEBestCrypt.VolumeInternallyMounted(volumeFilename: string): char;
var
  diOLD: TDISKINFO_BUFFER_driverOLD;
  diNEW: TDISKINFO_BUFFER_driverNEW;
  drv: char;
begin
  CheckActive();

  volumeFilename := SDUConvertSFNtoLFN(volumeFilename);

  Result := #0;

  for drv:='A' to 'Z' do
    begin
    if (Version() < DRIVER_VERSION_DISKINFO_NEW) then
      begin
      // HANDLE FOR v2 DRIVERS...
      if GetDriveInfoOLD(drv, diOLD) then
        begin
        // Start copying from the 2nd char to avoid the extra "\" at the start
        // Only copy up to the #0, i.e. only the number of chars 2 less than the
        // #0's pos
        if (volumeFilename=SDUConvertSFNtoLFN(copy(diOLD.filename, 2, (pos(#0, diOLD.filename)-2)))) then
          begin
          Result := drv;
          break;
          end;

        end;
      end
    else
      begin
      // HANDLE FOR v3 DRIVERS...
      if GetDriveInfoNEW(drv, diNEW) then
        begin
        // Start copying from the 2nd char to avoid the extra "\" at the start
        // Only copy up to the #0, i.e. only the number of chars 2 less than the
        // #0's pos
        if (volumeFilename=SDUConvertSFNtoLFN(copy(diNEW.filename, 2, (pos(#0, diNEW.filename)-2)))) then
          begin
          Result := drv;
          break;
          end;

        end;
        
      end;

    end;

end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEBestCrypt.MountDevices(): string;
begin
  // Not supported...
  Result := #0;
end;

// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFEBestCrypt.CanMountDevice(): boolean;
begin
  // Not supported...
  Result := FALSE;
end;

// -----------------------------------------------------------------------------


END.



