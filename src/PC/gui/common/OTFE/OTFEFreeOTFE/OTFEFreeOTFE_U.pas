unit OTFEFreeOTFE_U;
 // Description: Delphi FreeOTFE Component
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //
    {
TOTFEFreeOTFE is a wrapper round the driver and also does all drive operations,
enc data, create new vols, etc
is a singleton class, only ever one instance - this is enforced by assertion in
base class ctor
set up instance by calling OTFEFreeOTFEBase_U.SetFreeOTFEType then get instance
by calling OTFEFreeOTFEBase_U.GetFreeOTFE or  TOTFEFreeOTFE.GetFreeOTFE
}


interface

uses
//delphi
  Classes, Controls, dialogs, Graphics, Messages,  SysUtils,
  Windows,
  DriverAPI,
  Forms,
  //sdu , lc utils

     lcDebugLog,    OTFE_U,
  OTFEConsts_U,     lcTypes,
     FreeOTFEDriverConsts,
  OTFEFreeOTFE_DriverCypherAPI,
  OTFEFreeOTFE_DriverHashAPI,
  OTFEFreeOTFE_LUKSAPI,
  OTFEFreeOTFEBase_U,
  pkcs11_library,
  pkcs11_object,
  pkcs11_session,
  PKCS11Lib,
   VolumeFileAPI
  //librecrypt forms
;

type
  TOTFEFreeOTFE = class (TOTFEFreeOTFEBase)
  protected
    fdriver_handle:        THandle;


    // Connect/disconnect to the main FreeOTFE device driver
    function Connect(): Boolean; override;
    procedure Disconnect(); override;


    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations
    // deviceType - Set to FILE_DEVICE_DISK, etc

    function CreateDiskDevice(deviceType: DWORD = FILE_DEVICE_DISK): String;
    function DestroyDiskDevice(deviceName: Ansistring): Boolean;

    function MountDiskDevice(
      deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      volumeIV: TSDUBytes;
      ReadOnly: Boolean;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      metaData: TOTFEFreeOTFEVolumeMetaData;
      offset: Int64 = 0;
      size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType =
      mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; override;


    function DismountDiskDevice(
      deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      emergency: Boolean
      ): Boolean; override;

    function LDREUDriver(
      driveLetter: Char;
      deviceName: Ansistring;
      emergency: Boolean
      ): Boolean;
    function LDREUUserApp(
      driveLetter: Char;
      deviceName: Ansistring;
      emergency: Boolean;
      var unableToOpenDrive: Boolean
      ): Boolean;

    // ---------
    // Status functions

    // Get the number of disk devices the driver currently has (including both
    // mounted and unmounted disks)
    function GetDiskDeviceCount(): Cardinal;

    // Get the kernel mode device name of all disk devices
    function GetDiskDeviceList(deviceNames: TStringList): Boolean;


    // Get a list of all FreeOTFE devices which are currently mounted
    // Note: Unmounted devices will have '' in the "driveLetters" item
    //       corresponding to the unmounted device listed in the same item
    //       position in "deviceNames"
    function GetMountedDevices(driveLetters: TStringList; deviceNames: TStringList): Boolean;

    // Given the specified drive letter, return the FreeOTFE device for that
    // mounted drive
    function GetDriveDeviceName(driveLetter: Char): String;
    // Given the specified device name, return the drive mounted on that device
    function GetDriveDeviceLetter(deviceName: String): Char;


    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    { TODO 2 -otdk -csecurity :
driver allows raw device access w.out admin privs - pretty big security flaw
instead use windows standard file fns (as in base class) - will need admin privs }

    // This executes a simple call to the driver to read dataLength bytes from
    // offsetWithinFile
    // Note: Will probably not work when reading directly from partitions;
    //       they typically need read/writes carried out in sector sized blocks
    //        - see ReadRawVolumeDataBounded for this.
    function ReadRawVolumeDataSimple(
      filename: String;
      offsetWithinFile: Int64;
      dataLength: DWORD;  // In bytes
      var Data: Ansistring
      ): Boolean; override;

    function WriteRawVolumeDataSimple(
      filename: String;
      offsetWithinFile: Int64;
      data: Ansistring
      ): Boolean; override;

    // ---------
    // Misc functions

    // Call QueryDosDevice, and return it's output
    function DoQueryDosDevice(deviceName: Ansistring; queryResults: TStringList): Boolean;
    // Given an MSDOS device name, return the device name of that device
    // (Uses DoQueryDosDevice)
    function GetDeviceName(MSDOSKernelModeDeviceName: String): Ansistring;

    // Create/delete DosDevices symlink; effectivly wraps DefineDosDevice(...) with
    // additional support required for Vista changes
    function DosDeviceSymlink(
      CreateNotDelete: Boolean;
      DeviceName: Ansistring;
      DriveLetter: DriveLetterChar;
      Global: Boolean
      ): Boolean;

    // Delete both local and global DosDevices symlinks, as possible
    function DeleteDosDeviceSymlink(DeviceName: String; DriveLetter: Char): Boolean;

    // Connect to the specified user mode named device
    function ConnectDevice(devicename: String): THandle;
    // Disconnect to the specified device
    function DisconnectDevice(deviceHandle: THandle): Boolean;

    // Convert kernel mode device name to user mode device name
    function GetHashDeviceUserModeDeviceName(hashKernelModeDeviceName: String): String;
    // Convert kernel mode device name to user mode device name
    function GetCypherDeviceUserModeDeviceName(cypherKernelModeDeviceName: String): String;

    function DriverType(): String; override;

    // ---------
    // Convert critical data area to/from a string representation

    // Convert a user-space volume filename to a format the kernel mode driver
    // can understand
    function GetKernelModeVolumeFilename(userModeFilename: String): String;
    // Convert a kernel mode driver volume filename to format user-space
    // filename
    function GetUserModeVolumeFilename(kernelModeFilename: Ansistring): Ansistring;

    // Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
    // filename refers to a partition/file
    function IsPartition_KernelModeName(kernelModeFilename: String): Boolean;

    // expectedLength - Set to the amount of metadata to retrieve
    function GetVolumeMetaData(driveLetter: Char; expectedLength: Integer;
      var metaData: Ansistring): Boolean;

    // v3 Cypher API functions
    // Note: This shouldn't be called directly - only via wrapper functions!
    function _GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; override;

  public
    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Char): Char; override;
    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(); override;
    destructor Destroy(); override;
    function Dismount(driveLetter: DriveLetterChar; emergency: Boolean = False): Boolean;
      overload; override;
    function Version(): Cardinal; overload; override;
    function DrivesMounted(): String; overload; override;

    // -----------------------------------------------------------------------
    // TOTFEFreeOTFEBase standard API

    function GetVolumeInfo(driveLetter: Char; var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
      override;

    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean; override;
    function GetHashDriverHashes(hashDriver: string;
      var hashDriverDetails: TFreeOTFEHashDriver): Boolean; override;

    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean; override;

    function _EncryptDecryptData(
      encryptFlag: Boolean;
      cypherDriver: Ansistring;
      cypherGUID: TGUID;
      var key: TSDUBytes;
      var IV: Ansistring;
      var inData: Ansistring;
      var outData: Ansistring
      ): Boolean; override;

    function EncryptDecryptSectorData(
      encryptFlag: Boolean;
      cypherDriver: Ansistring;
      cypherGUID: TGUID;
      SectorID: LARGE_INTEGER;
      SectorSize: Integer;
      var key: TSDUBytes;
      var IV: Ansistring;
      var inData: Ansistring;
      var outData: Ansistring
      ): Boolean; override;

    function HashData(
      hashDriver: Ansistring;
      hashGUID: TGUID;
      const data: TSDUBytes;
      out hashOut: TSDUBytes
      ): Boolean; override;

    function MACData(
      macAlgorithm: TFreeOTFEMACAlgorithm;
      HashDriver: Ansistring;
      HashGUID: TGUID;
      CypherDriver: Ansistring;
      CypherGUID: TGUID;
      var key: PasswordString;
      var data: Ansistring;
      var MACOut: Ansistring;
      tBits: Integer = -1
      ): Boolean; override;

    function DeriveKey(
      kdfAlgorithm: TFreeOTFEKDFAlgorithm;
      HashDriver: Ansistring;
      HashGUID: TGUID;
      CypherDriver: Ansistring;
      CypherGUID: TGUID;
      Password: TSDUBytes;
      Salt: array of Byte;
      Iterations: Integer;
      dkLenBits: Integer;  // In *bits*
      out DK: TSDUBytes
      ): Boolean; override;

 function CreateMountDiskDevice(
      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      volumeIV: TSDUBytes;
      ReadOnly: Boolean;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      DriveLetter: Char;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      offset: Int64 = 0;
      size: Int64 = 0;
      MetaData_LinuxVolume: Boolean = False;  // Linux volume
      MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
      MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;
    // PC kernel drivers *only* - ignored otherwise
      mountForAllUsers: Boolean =
      True  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; override;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE specific functions

    // ---------
    // FreeOTFE Driver handling...


    // Install and start FreeOTFE drivers in portable mode
    function PortableStart(driverFilenames: TStringList; showProgress: Boolean): Boolean;

    // Stop and uninstall any FreeOTFE drivers started in portable mode
    function PortableStop(): Boolean;

    // Identify how many drivers are currently running in portable mode
    function DriversInPortableMode(): Integer;

    function CanUserManageDrivers(): Boolean;




    function ReadWritePlaintextToVolume(
      readNotWrite: Boolean;

      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      mountMountAs: TFreeOTFEMountAs;

      dataOffset: Int64;
    // Offset from within mounted volume from where to read/write data
      dataLength: Integer;  // Length of data to read/write. In bytes
      var data: TSDUBytes;  // Data to read/write

      offset: Int64 = 0;
      size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
      ): Boolean; override;

  published

  end;


{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFE: TOTFEFreeOTFE;


implementation

uses
 // delphi
    Math,     // Required for min
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for GUIDToString
  WinSvc,   // Required for SERVICE_ACTIVE


 // 3rd party
{$IFDEF LINUX_DETECT}
  DbugIntf,  // GExperts
{$ENDIF}
{$IFDEF FREEOTFE_DEBUG}
  DbugIntf,  // GExperts
{$ENDIF}

//sdu / lclibs
 SDUi18n, SDUDialogs,
  SDUEndianIntegers,
     CommonSettings,
  sduGeneral,
  dlgProgress,
     DriverControl
  //LibreCrypt forms
  ;




{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

type
  EFreeOTFEReadCDBFailure  = EFreeOTFEError;  // Internal error - should never
  // be propagated beyond this unit
  EFreeOTFEWriteCDBFailure = EFreeOTFEError;  // Internal error - should never
  // be propagated beyond this unit
  PHashIdentify            = ^THashIdentify;

  THashIdentify = record
    KernelModeDriverName: String;
    GUID:                 String;
    Details:              TFreeOTFEHash;
  end;

  PCypherIdentify = ^TCypherIdentify;

  TCypherIdentify = record
    KernelModeDriverName: String;
    GUID:                 String;
    Details:              TFreeOTFECypher_v3;
  end;

const
  VERSION_ID_NOT_CACHED = VERSION_ID_FAILURE;

  // These are artifical maximums; they must be enough to store the results of
  // the DIOC calls when the amount of data output is variable
  MAX_DERIVED_KEY_LENGTH = 1024 * 1024;  // In *bits*
  MAX_MAC_LENGTH         = 1024 * 1024;  // In *bits*

  // Version ID for arbitary metadata passed to, and returned from, driver
  METADATA_VERSION = 1;


 // ----------------------------------------------------------------------------
 //procedure Register;
 //begin
 //  RegisterComponents('OTFE', [TOTFEFreeOTFE]);
 //end;


// ----------------------------------------------------------------------------
constructor TOTFEFreeOTFE.Create();
begin
  inherited;
end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFE.Destroy();
begin
  inherited;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.Connect(): Boolean;
begin
  Result := False;

  fdriver_handle := ConnectDevice(DEVICE_SYMLINK_MAIN_NAME);

  if (fdriver_handle <> 0) then begin
    Result := True;
  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
  end;

end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFE.Disconnect();
begin
  DisconnectDevice(fdriver_handle);
end;


 // ----------------------------------------------------------------------------
 // Attempt to create a new device to be used
function TOTFEFreeOTFE.CreateDiskDevice(deviceType: DWORD): String;
var
  DIOCBufferIn:  TDIOC_DISK_DEVICE_CREATE;
  DIOCBufferOut: TDIOC_DEVICE_NAME;
  bytesReturned: DWORD;
begin
  Result := '';

  CheckActive();

  // !!! WARNING !!!
  // We use FILE_DEVICE_DISK as FILE_DEVICE_VIRTUAL_DISK can
  // make NTFS-only Windows XP systems unstable?
  // See MS Knowledgebase article: Q257405 for further details
  // http://support.microsoft.com/default.aspx?scid=kb;en-us;257405

  DIOCBufferIn.DeviceType := deviceType;

  if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_CREATE, @DIOCBufferIn,
    sizeof(DIOCBufferIn), @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
    Result := copy(DIOCBufferOut.DeviceName, 1, StrLen(DIOCBufferOut.DeviceName));

    DebugMsg('Disk device created: ' + Result);

  end;

end;


 // ----------------------------------------------------------------------------
 // Attempt to mount on an existing device
function TOTFEFreeOTFE.MountDiskDevice(
  deviceName: String;
  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  volumeIV: TSDUBytes;
  ReadOnly: Boolean;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  metaData: TOTFEFreeOTFEVolumeMetaData;
  offset: Int64 = 0;
  size: Int64 = 0;
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
  ): Boolean;
var
  ptrDIOCBuffer:     PDIOC_MOUNT_PC_DRIVER;
  bytesReturned:     DWORD;
  bufferSize:        Integer;
  mainCypherDetails: TFreeOTFECypher_v3;
  kmFilename:        String;
  useVolumeFlags:    Integer;
  strMetaData:       Ansistring;
  volumeKeyStr:      Ansistring;
begin
  Result := False;

  DebugMsg('In MountDiskDevice');

  VolumeMetadataToString(metaData, strMetaData);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MountDiskDevice called with: ');
DebugMsg('  deviceName: '+deviceName);
DebugMsg('  filename: '+volFilename);
DebugMsg('  volumeKey: ');
DebugMsg(volumeKey);
DebugMsg('  IVHashDriver: '+IVHashDriver);
DebugMsg('  IVHashGUID: '+GUIDToString(IVHashGUID));
DebugMsg('  IVCypherDriver: '+IVCypherDriver);
DebugMsg('  IVCypherGUID: '+GUIDToString(IVCypherGUID));
DebugMsg('  mainCypherDriver: '+mainCypherDriver);
DebugMsg('  mainCypherGUID: '+GUIDToString(mainCypherGUID));
DebugMsg('  sectorIVGenMethod: '+FreeOTFESectorIVGenMethodTitle[sectorIVGenMethod]);
DebugMsg('  VolumeFlags: '+inttostr(VolumeFlags));
DebugMsg('  metaData: <not shown>');
DebugMsg('  strMetaData:');
DebugMsgBinary(strMetaData);
DebugMsg('  offset: '+inttostr(offset));
DebugMsg('  size: '+inttostr(size));
DebugMsg('  ReadOnly: '+BoolToStr(ReadOnly));
{$ENDIF}

  CheckActive();


  if not (GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then begin
    //
    exit;
  end;

  // Ensure the volumeKey's length matches that of the cypher's keysize, if
  // the cypher's keysize is >= 0
  if (mainCypherDetails.KeySizeRequired >= 0) then begin
    // THIS IS CORRECT; caters for both volumeKey>keysize and
    // volumeKey<keysize
    // Copy as much of the hash value as possible to match the key length
    volumeKeyStr := SDUBytesToString(volumeKey);
    volumeKeyStr := Copy(volumeKeyStr, 1, min((mainCypherDetails.KeySizeRequired div 8),
      length(volumeKeyStr)));
    // If the hash wasn't big enough, pad out with zeros

    volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0),
      ((mainCypherDetails.KeySizeRequired div 8) - Length(volumeKeyStr)));
    SafeSetLength(volumeKey, mainCypherDetails.KeySizeRequired div 8);
    assert(volumeKeyStr = SDUBytesToString(volumeKey));// passed
  end;


  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then begin
    IVHashDriver := '';
    IVHashGUID   := StringToGUID(NULL_GUID);
  end;

  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then begin
    IVCypherDriver := '';
    IVCypherGUID   := StringToGUID(NULL_GUID);
  end;


  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize    := sizeof(ptrDIOCBuffer^) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(volumeKey)) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(volumeIV)) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(strMetaData));
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize    := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StrPCopy(ptrDIOCBuffer.DiskDeviceName, deviceName);

    // Determine the filename as the kernel sees it
    kmFilename := GetKernelModeVolumeFilename(volFilename);

    StrPCopy(ptrDIOCBuffer.Filename, kmFilename);
    ptrDIOCBuffer.DataStart := offset;

    ptrDIOCBuffer.DataEnd := 0;
    if (size > 0) then begin
      ptrDIOCBuffer.DataEnd := offset + size;
    end;

    StrPCopy(ptrDIOCBuffer.IVHashDeviceName, IVHashDriver);
    ptrDIOCBuffer.IVHashGUID := IVHashGUID;
    StrPCopy(ptrDIOCBuffer.IVCypherDeviceName, IVCypherDriver);
    ptrDIOCBuffer.IVCypherGUID := IVCypherGUID;

    StrPCopy(ptrDIOCBuffer.MainCypherDeviceName, mainCypherDriver);
    ptrDIOCBuffer.MainCypherGUID := mainCypherGUID;

    ptrDIOCBuffer.ReadOnly    := ReadOnly;
    ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsFile];
    if IsPartition_KernelModeName(kmFilename) then begin
      ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsPartition];
    end;

    ptrDIOCBuffer.StorageMediaType := FreeOTFEStorageMediaTypeID[storageMediaType];


    useVolumeFlags := VolumeFlags;
    // Yes, this timestamp reverting is the right way around; if the bit
    // *isn't* set, the timestamps get reverted
    if GetSettings().OptRevertVolTimestamps then
      // Strip off bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags and not (VOL_FLAGS_NORMAL_TIMESTAMPS)
    else
      // Set bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags or VOL_FLAGS_NORMAL_TIMESTAMPS;

    ptrDIOCBuffer.VolumeFlags       := useVolumeFlags;
    ptrDIOCBuffer.SectorIVGenMethod := FreeOTFESectorIVGenMethodID[sectorIVGenMethod];

    ptrDIOCBuffer.MasterKeyLength := Length(volumeKey) * 8;
    ptrDIOCBuffer.VolumeIVLength  := Length(volumeIV) * 8;
    ptrDIOCBuffer.MetaDataLength  := Length(strMetaData);
    // This may seem a little weird, but we do this because the VolumeIV and metaData are
    // immediatly after the master key
    StrMove(@ptrDIOCBuffer.MasterKey, PAnsiChar(volumeKey), Length(volumeKey));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey)) + length(volumeKey)),
      PAnsiChar(volumeIV), Length(volumeIV));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey)) + length(volumeKey) + length(volumeIV)),
      PAnsiChar(strMetaData), Length(strMetaData));

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('+++ ABOUT TO DIOC MOUNT WITH STRUCT:');
DebugMsg('+++ struct size: '+inttostr(bufferSize));
DebugMsg('+++ ---begin struct---');
DebugMsgBinaryPtr(PAnsiChar(ptrDIOCBuffer), bufferSize);
DebugMsg('+++ ---end struct---');
{$ENDIF}
DebugFlush();
    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_MOUNT, ptrDIOCBuffer,
      bufferSize, nil, 0, bytesReturned, nil)) then begin

      DebugMsg('Mounted OK!');

      Result := True;
    end;

    DebugMsg('+++ DIOC STRUCT COMPLETE');


  finally
    FreeMem(ptrDIOCBuffer);
  end;


  DebugMsg('Exiting MountDiskDevice');

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.CreateMountDiskDevice(
  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  volumeIV: TSDUBytes;
  ReadOnly: Boolean;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  DriveLetter: Char;
  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
  offset: Int64 = 0;
  size: Int64 = 0;
  MetaData_LinuxVolume: Boolean = False;               // Linux volume
  MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
  MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;
  // PC kernel drivers *only* - ignored otherwise
  mountForAllUsers: Boolean =
  True  // PC kernel drivers *only* - ignored otherwise
  ): Boolean;
var
  deviceName:    String;
  mountMetadata: TOTFEFreeOTFEVolumeMetaData;
begin
  Result := False;

  // Attempt to create a new device to be used
  deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[MountMountAs]);
  if (deviceName <> '') then begin
    PopulateVolumeMetadataStruct(
      MetaData_LinuxVolume,
      MetaData_PKCS11SlotID,
      mountMetadata
      );

    // Attempt to mount the device
    if MountDiskDevice(deviceName, volFilename, volumeKey, sectorIVGenMethod,
      volumeIV, ReadOnly, IVHashDriver, IVHashGUID, IVCypherDriver, IVCypherGUID,
      mainCypherDriver, mainCypherGUID, VolumeFlags, mountMetadata, offset,
      size, FreeOTFEMountAsStorageMediaType[MountMountAs]) then begin
      if DosDeviceSymlink(True, deviceName, DriveLetter, mountForAllUsers) then begin
        Result := True;
      end else begin
        // Cleardown
        DismountDiskDevice(deviceName, True);
        DestroyDiskDevice(deviceName);
      end;

    end else begin
      // Cleardown
      DestroyDiskDevice(deviceName);
    end;

  end;

  //
end;


 // ----------------------------------------------------------------------------
 // Attempt to dismount a device
function TOTFEFreeOTFE.DismountDiskDevice(deviceName: String; emergency: Boolean): Boolean;
var
  DIOCBuffer:    TDIOC_DISMOUNT;
  bytesReturned: DWORD;
begin
  Result := False;

  CheckActive();

  StrPCopy(DIOCBuffer.DiskDeviceName, deviceName);
  DIOCBuffer.Emergency := emergency;

  if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_DISMOUNT, @DIOCBuffer,
    sizeof(DIOCBuffer), nil, 0, bytesReturned, nil)) then begin
    Result := True;
  end;

end;


 // ----------------------------------------------------------------------------
 // This dismount routine is based on the MS "media eject" routine for NT/2k/XP
 // at:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q165/7/21.asp&NoWebContent=1
function TOTFEFreeOTFE.LDREUUserApp(
  driveLetter: Char;
  deviceName: Ansistring;
  emergency: Boolean;
  var unableToOpenDrive: Boolean
  ): Boolean;
var
  driveDevice:      THandle;
  BytesReturned:    DWORD;
  driveLetterColon: String;
  driveFile:        WideString;
  pmr:              TPREVENT_MEDIA_REMOVAL;
  volumeInfo:       TOTFEFreeOTFEVolumeInfo;
begin
  CheckActive();

  driveLetterColon := upcase(driveLetter) + ':';
  driveFile        := '\\.\' + driveLetterColon;

  Result := True;

  if Result then begin

    DebugMsg('getVolumeInfo');

    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if (not Result) then begin

      DebugMsg('getVolumeInfo NOT Result');

      emergency := False;
    end;
  end;



  // We attempt to open the drive, and then carry out various operations to
  // shut the drive down.
  // If were operating in an emergency, then we don't care if some of these
  // operations fail - it's an EMERGENCY! JUST GET RID OF THE %$!# DRIVE!


  // Initialize to flag that we haven't opened it
  driveDevice := INVALID_HANDLE_VALUE;
  if Result then begin

    DebugMsg('createfile: ' + driveFile);

    driveDevice := CreateFile(PChar(driveFile),
      //GENERIC_READ or GENERIC_WRITE,
      GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
      FILE_FLAG_NO_BUFFERING, 0);
    Result      := (driveDevice <> INVALID_HANDLE_VALUE);

    DebugMsg('driveDevice: ' + IntToStr(driveDevice) + ' (INVALID_HANDLE_VALUE = ' +
      IntToStr(INVALID_HANDLE_VALUE));

  end;

  unableToOpenDrive := (driveDevice = INVALID_HANDLE_VALUE);

  if (Result or (emergency and (driveDevice <> INVALID_HANDLE_VALUE))) then begin

    DebugMsg('lockvolume');

    Result := DeviceIoControl(driveDevice, FSCTL_LOCK_VOLUME, nil, 0, nil,
      0, bytesReturned, nil);
  end;


  if (Result or (emergency and (driveDevice <> INVALID_HANDLE_VALUE))) then begin

    DebugMsg('dismountvolume');

    Result := DeviceIoControl(driveDevice, FSCTL_DISMOUNT_VOLUME, nil, 0,
      nil, 0, bytesReturned, nil);
  end;


  if (Result or (emergency and (driveDevice <> INVALID_HANDLE_VALUE))) then begin
    pmr.PreventMediaRemoval := False;

    DebugMsg('mediaremoval');

    Result := DeviceIoControl(driveDevice, IOCTL_STORAGE_MEDIA_REMOVAL, @pmr,
      sizeof(pmr), nil, 0, bytesReturned, nil);
  end;


  if (Result or (emergency and (driveDevice <> INVALID_HANDLE_VALUE))) then begin
    pmr.PreventMediaRemoval := False;

    DebugMsg('ejectmedia');

    Result := DeviceIoControl(driveDevice, IOCTL_STORAGE_EJECT_MEDIA, nil,
      0, nil, 0, bytesReturned, nil);
  end;


  // An explicit call to IOCTL_FREEOTFE_DISMOUNT should be redundant; the
  // IOCTL_STORAGE_EJECT_MEDIA should have already done this
  if (Result or emergency) then begin

    DebugMsg('dismountdevice');

    Result := DismountDiskDevice(volumeInfo.deviceName, emergency);
  end;


  if (Result or (emergency and (driveDevice <> INVALID_HANDLE_VALUE))) then begin

    DebugMsg('unlock');

    Result := DeviceIoControl(driveDevice, FSCTL_UNLOCK_VOLUME, nil, 0, nil,
      0, bytesReturned, nil);
  end;


  if (driveDevice <> INVALID_HANDLE_VALUE) then begin

    DebugMsg('closehandle');

    // Note that we can't do:
    //   Result := Result and CloseHandle(driveDevice);"
    // here because lazy evaluation will prevent CloseHandle(...) from being
    // called if Result is FALSE - and we *want* CloseHandle(...) called
    // regardless, otherwise we end up with FreeOTFE never closing this file
    // handle, and "leaking" this handle
    if not (CloseHandle(driveDevice)) then begin
      Result := False;
    end;
  end;

end;


 // Send LDREU to get the driver to lock, dismount, remove eject and unlock
 // driveFile - Must be of the format "\??\Z:"
 // deviceName - Must be of the formst \Device\FreeOTFE\Disks\Disk1
function TOTFEFreeOTFE.LDREUDriver(
  driveLetter: Char;
  deviceName: Ansistring;
  emergency: Boolean
  ): Boolean;
var
  DIOCBuffer:      TDIOC_LDREU;
  bytesReturned:   DWORD;
  kernelDriveFile: Ansistring;
begin
  Result := False;

  CheckActive();

  // This functionality only present in v2.00 o f the FreeOTFE driver; previous
  // versions could only carry this out within the userspace app
  if CheckVersionAtLeast(FREEOTFE_ID_v02_00_0000) then begin
    kernelDriveFile := '\??\' + uppercase(AnsiChar(driveLetter)) + ':';

    StrPCopy(DIOCBuffer.DriveFile, kernelDriveFile);
    StrPCopy(DIOCBuffer.DiskDeviceName, deviceName);
    DIOCBuffer.Emergency := emergency;
    DebugMsg('LDREUDriver: "' + DIOCBuffer.DriveFile + '" : "' +
      DIOCBuffer.DiskDeviceName + '" ' + Booltostr(emergency));

    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_LDREU, @DIOCBuffer,
      sizeof(DIOCBuffer), nil, 0, bytesReturned, nil)) then begin
      Result := True;
    end;
  end;

end;


 // ----------------------------------------------------------------------------
 // Attempt to destroy a device
function TOTFEFreeOTFE.DestroyDiskDevice(deviceName: Ansistring): Boolean;
var
  DIOCBuffer:    TDIOC_DEVICE_NAME;
  bytesReturned: DWORD;
begin
  Result := False;

  CheckActive();

  StrPCopy(DIOCBuffer.DeviceName, deviceName);

  if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_DESTROY, @DIOCBuffer,
    sizeof(DIOCBuffer), nil, 0, bytesReturned, nil)) then begin
    Result := True;
  end;

end;


 // ----------------------------------------------------------------------------
 // This dismount routine is based on the MS "media eject" routine for NT/2k/XP
 // at:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q165/7/21.asp&NoWebContent=1
function TOTFEFreeOTFE.Dismount(driveLetter: DriveLetterChar; emergency: Boolean = False): Boolean;
var
  volumeInfo:        TOTFEFreeOTFEVolumeInfo;
  unableToOpenDrive: Boolean;
begin
  CheckActive();

  Result := True;

  if Result then begin

    DebugMsg('getVolumeInfo');

    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if (not Result) then begin

      DebugMsg('getVolumeInfo NOT Result');

      emergency := False;
    end;
  end;



  // We attempt to open the drive, and then carry out various operations to
  // shut the drive down.
  // If were operating in an emergency, then we don't care if some of these
  // operations fail - it's an EMERGENCY! JUST GET RID OF THE %$!# DRIVE!

  if Result then begin
    unableToOpenDrive := False;

    DebugMsg('LDREUUserApp: "' + driveLetter + '" : "' + volumeInfo.deviceName +
      '" ' + Booltostr(emergency) + Booltostr(unableToOpenDrive));

    Result := LDREUUserApp(driveLetter, volumeInfo.deviceName, emergency, unableToOpenDrive);
  end;


  // If we were unable to open the drive to carry out the dismount, we're
  // probably on Vista, without having escalated UAC before trying to dismount.
  // Fallback to getting the driver handle the privileged part of the
  // dismount (i.e. opening the volume) and carry out the LDREU
  if (not Result and unableToOpenDrive) then begin

    DebugMsg(Format('LDREUDriver "%s" "%s" %s', [driveLetter, volumeInfo.deviceName,
      Booltostr(emergency)]));


    Result := LDREUDriver(driveLetter, volumeInfo.deviceName, emergency);
  end;

  if (Result or emergency) then begin

    DebugMsg('remove definition');

    Result := DeleteDosDeviceSymlink(volumeInfo.deviceName, driveLetter);
  end;


  // And finally... Destroy the device
  if (Result or emergency) then begin

    DebugMsg('destroy device');

    Result := DestroyDiskDevice(volumeInfo.deviceName);
  end;
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.Version(): Cardinal;
var
  BytesReturned: DWORD;
  DIOCBuffer:    TDIOC_VERSION;
begin
  Result := VERSION_ID_FAILURE;

  CheckActive();

  // If we have the version ID cached, use the cached information
  if (fCachedVersionID <> VERSION_ID_NOT_CACHED) then begin
    Result := fCachedVersionID;
  end else begin
    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_VERSION, nil, 0,
      @DIOCBuffer, sizeof(DIOCBuffer), BytesReturned, nil)) then begin
      if BytesReturned = sizeof(DIOCBuffer) then begin
        Result           := DIOCBuffer.VersionID;
        fCachedVersionID := Result;
      end;

    end;

  end;

end;


 // ----------------------------------------------------------------------------
 // Get the number of disk devices the driver currently has
 // Returns $FFFFFFFF on error
function TOTFEFreeOTFE.GetDiskDeviceCount(): Cardinal;
var
  BytesReturned: DWORD;
  DIOCBuffer:    TDIOC_DISK_DEVICE_COUNT;
begin
  Result := $FFFFFFFF;

  CheckActive();

  if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GET_DISK_DEVICE_COUNT, nil,
    0, @DIOCBuffer, sizeof(DIOCBuffer), BytesReturned, nil)) then begin
    if BytesReturned = sizeof(DIOCBuffer) then begin
      Result := DIOCBuffer.Count;
    end;

  end;

end;


 // ----------------------------------------------------------------------------
 // Get the kernel mode device name of all disk devices
 // WARNING: If devices are mounted while this function is called, this function
 //          may fail if the number of devices goes up between the call to
 //          GetDiskDevicesCount(), and the DIOC call (the buffer won't be big
 //          enough to store all device names)
function TOTFEFreeOTFE.GetDiskDeviceList(deviceNames: TStringList): Boolean;
var
  BytesReturned:         DWORD;
  ptrBuffer:             Pointer;
  ptrBufferOffset:       Pointer;
  bufferSize:            Cardinal;
  i:                     Integer;
  devName:               Ansistring;
  deviceCount:           Integer;
  ptrDIOCDeviceNameList: PDIOC_DEVICE_NAME_LIST;
begin
  Result := False;

  CheckActive();

  deviceCount := GetDiskDeviceCount();


  bufferSize := sizeof(ptrDIOCDeviceNameList^) - FREEOTFE_MAX_FILENAME_LENGTH +
    (FREEOTFE_MAX_FILENAME_LENGTH * deviceCount);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrBuffer  := allocmem(bufferSize);

  if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GET_DISK_DEVICE_LIST, nil,
    0, ptrBuffer, bufferSize, BytesReturned, nil)) then begin
    if BytesReturned <= bufferSize then begin
      // In case devices were unmounted between the call to
      // GetDiskDeviceCount() and the DIOC call...
      ptrDIOCDeviceNameList := ptrBuffer;
      deviceCount           := ptrDIOCDeviceNameList.DeviceCount;

      ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) + sizeof(ptrDIOCDeviceNameList^) -
        FREEOTFE_MAX_FILENAME_LENGTH);
      for i := 1 to deviceCount do begin
        devName := Copy(PAnsiChar(ptrBufferOffset), 1, StrLen(PAnsiChar(ptrBufferOffset)));

        deviceNames.Add(devName);

        // Setup for the next one...
        ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + FREEOTFE_MAX_FILENAME_LENGTH);
      end;

      Result := True;
    end;

  end else begin

    DebugMsg('devicelist DIOC 2 FAIL');

  end;

  FreeMem(ptrBuffer);

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DrivesMounted(): String;
var
  i:            Integer;
  driveLetters: TStringList;
  deviceNames:  TStringList;
begin
  Result := '';

  CheckActive();

  driveLetters := TStringList.Create();
  try
    deviceNames := TStringList.Create();
    try
      if GetMountedDevices(driveLetters, deviceNames) then begin
        for i := 0 to (driveLetters.Count - 1) do begin
          if (driveLetters[i] <> '') then begin
            Result := Result + driveLetters[i];
          end;

        end;

        Result := SortString(Result);
      end;

    finally
      deviceNames.Free();
    end;
  finally
    driveLetters.Free();
  end;

end;


 // ----------------------------------------------------------------------------
 // Get a list of all FreeOTFE devices which are currently mounted
 // Drive letters will be added to "driveLetters", and the corresponding device
 // in "deviceNames".
 // If the FreeOTFE driver reports that a device is mounted, but there is no
 // corresponding drive letter, then the relevant entry in "driveLetter" will
 // be an empty string, instead of a drive letter.
 // NOTE: THIS FUNCTION *CANNOT* CALL "GetVolumeInfo(...)"!!
 // Returns TRUE/FALSE on success/failure
function TOTFEFreeOTFE.GetMountedDevices(driveLetters: TStringList;
  deviceNames: TStringList): Boolean;
const
  // 5 is the arbitary number of times we'll try this until we just give up on
  // it...
  MAX_ATTEMPTS = 5;
var
  i:                Integer;
  driveLetter:      ansichar;
  driveColon:       String;
  foundDriveLetter: Boolean;
  attempt:          Integer;
  flagTryAgain:     Boolean;
  currDeviceName:   String;
  queryResults:     TStringList;
begin
  Result := False;

  CheckActive();

  // flagTryAgain indicates whether the devices have changed during this
  // operation; if they have, it tries it again
  flagTryAgain := True;
  // attempt counts the number of times the this loop was attempted
  attempt      := 1;
  while ((flagTryAgain) and (attempt <= MAX_ATTEMPTS)) do begin
    flagTryAgain := False;
    driveLetters.Clear();
    deviceNames.Clear();

    if not (GetDiskDeviceList(deviceNames)) then begin
      Inc(attempt);
      flagTryAgain := True;
    end;

  end;  // while



  if (attempt <= MAX_ATTEMPTS) then begin
    for i := 0 to (deviceNames.Count - 1) do begin
      currDeviceName := deviceNames[i];

      foundDriveLetter := False;

      for driveLetter := 'A' to 'Z' do begin
        driveColon := driveLetter + ':';

        queryResults := TStringList.Create();
        try
          if (DoQueryDosDevice(driveColon, queryResults)) then begin
            if (queryResults.Count >= 1) then begin
              if (queryResults[0] = currDeviceName) then begin
                foundDriveLetter := True;
                driveLetters.Add(driveLetter);
                break;
              end;
            end;
          end;


        finally
          queryResults.Free();
        end;

      end;

      if (not (foundDriveLetter)) then begin
        driveLetters.Add('');
      end;

    end;

    Result := True;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetVolumeInfo(driveLetter: Char;
  var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
var
  deviceName:           String;
  BytesReturned:        DWORD;
  inBuffer:             TDIOC_DEVICE_NAME;
  outBuffer:            TDIOC_DISK_DEVICE_STATUS_PC_DRIVER;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := False;

  CheckActive();

  driveLetter := upcase(driveLetter);

  deviceName := GetDriveDeviceName(driveLetter);

  DebugMsg('deviceName: ' + deviceName);

  if (deviceName <> '') then begin
    StrPCopy(inBuffer.DeviceName, deviceName);

    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS,
      @inBuffer, sizeof(inBuffer), @outBuffer, sizeof(outBuffer), BytesReturned, nil))
    then begin
      if (BytesReturned <= sizeof(outBuffer)) then begin
        volumeInfo.Filename         := Copy(outBuffer.Filename, 1, StrLen(outBuffer.Filename));
        volumeInfo.DeviceName       :=
          Copy(outBuffer.DiskDeviceName, 1, StrLen(outBuffer.DiskDeviceName));
        volumeInfo.IVHashDevice     :=
          Copy(outBuffer.IVHashDeviceName, 1, StrLen(outBuffer.IVHashDeviceName));
        volumeInfo.IVHashGUID       := outBuffer.IVHashGUID;
        volumeInfo.IVCypherDevice   :=
          Copy(outBuffer.IVCypherDeviceName, 1, StrLen(outBuffer.IVCypherDeviceName));
        volumeInfo.IVCypherGUID     := outBuffer.IVCypherGUID;
        volumeInfo.MainCypherDevice :=
          Copy(outBuffer.MainCypherDeviceName, 1, StrLen(outBuffer.MainCypherDeviceName));
        volumeInfo.MainCypherGUID   := outBuffer.MainCypherGUID;
        volumeInfo.Mounted          := outBuffer.Mounted;
        volumeInfo.ReadOnly         := outBuffer.ReadOnly;
        volumeInfo.VolumeFlags      := outBuffer.VolumeFlags;

        volumeInfo.SectorIVGenMethod := foivgUnknown;
        for tmpSectorIVGenMethod :=
          low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do begin
          if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = outBuffer.SectorIVGenMethod) then
          begin
            volumeInfo.SectorIVGenMethod := tmpSectorIVGenMethod;
          end;
        end;

        volumeInfo.DriveLetter := AnsiChar(driveLetter);

        volumeInfo.Filename := GetUserModeVolumeFilename(volumeInfo.Filename);

        Result := GetVolumeMetaData(driveLetter, outBuffer.MetaDataLength, volumeInfo.MetaData);

        volumeInfo.MetaDataStructValid :=
          ParseVolumeMetadata(volumeInfo.MetaData, volumeInfo.MetaDataStruct);

      end;

    end;

  end;

end;

 // ----------------------------------------------------------------------------
 // expectedLength - Set to the amount of metadata to retrieve
function TOTFEFreeOTFE.GetVolumeMetaData(driveLetter: Char; expectedLength: Integer;
  var metaData: Ansistring): Boolean;
var
  deviceName:    Ansistring;
  bufferSize:    DWORD;
  BytesReturned: DWORD;
  inBuffer:      TDIOC_DEVICE_NAME;
  ptrDIOCBuffer: PDIOC_DISK_DEVICE_METADATA;
begin
  Result := False;

  CheckActive();

  driveLetter := upcase(driveLetter);


  DebugMsg('expectedLength: ' + IntToStr(expectedLength));

  deviceName := GetDriveDeviceName(driveLetter);
  if (deviceName <> '') then begin
    StrPCopy(inBuffer.DeviceName, deviceName);

    bufferSize    := sizeof(ptrDIOCBuffer^) - sizeof(Ansichar) +
      (sizeof(Ansichar) * expectedLength);
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSize    := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
    ptrDIOCBuffer := allocmem(bufferSize);
    try
      StrPCopy(inBuffer.DeviceName, deviceName);

      if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA,
        @inBuffer, sizeof(inBuffer), ptrDIOCBuffer, bufferSize, BytesReturned, nil))
      then begin
        if (BytesReturned <= bufferSize) then begin
          // Set metaData so that it has enough characters which can be
          // overwritten with StrMove
          metaData := StringOfChar(Ansichar(#0), ptrDIOCBuffer.MetaDataLength);
          StrMove(PAnsiChar(metaData), @ptrDIOCBuffer.MetaData, ptrDIOCBuffer.MetaDataLength);

          DebugMsg('Metadata length: ' + IntToStr(ptrDIOCBuffer.MetaDataLength));
          DebugMsgBinary(metaData);


          Result := True;
        end;

      end;

    finally
      FreeMem(ptrDIOCBuffer);
    end;

  end;

end;


 // ----------------------------------------------------------------------------
 // Determine the FreeOTFE device for the specified drive letter, this may then
 // be passed back to the driver
 // NOTE: THIS FUNCTION *CANNOT* CALL "GetVolumeInfo(...)"!!
 // Returns '' on failure
function TOTFEFreeOTFE.GetDriveDeviceName(driveLetter: Char): String;
var
  mountedDriveLetters: TStringList;
  deviceNames:         TStringList;
  i:                   Integer;
begin
  Result := '';

  CheckActive();

  mountedDriveLetters := TStringList.Create();
  try
    deviceNames := TStringList.Create();
    try
      if GetMountedDevices(mountedDriveLetters, deviceNames) then begin
        for i := 0 to (mountedDriveLetters.Count - 1) do begin
          if (mountedDriveLetters[i] = driveLetter) then begin
            Result := deviceNames[i];
            break;
          end;

        end;

      end;

    finally
      deviceNames.Free();
    end;
  finally
    mountedDriveLetters.Free();
  end;

end;


 // ----------------------------------------------------------------------------
 // Given the specified device name, return the drive mounted on that device
 // NOTE: THIS FUNCTION *CANNOT* CALL "GetVolumeInfo(...)"!!
 // Returns #0 on failure
function TOTFEFreeOTFE.GetDriveDeviceLetter(deviceName: String): Char;
var
  mountedDriveLetters: TStringList;
  deviceNames:         TStringList;
  i:                   Integer;
begin
  Result := #0;

  CheckActive();

  mountedDriveLetters := TStringList.Create();
  try
    deviceNames := TStringList.Create();
    try
      if GetMountedDevices(mountedDriveLetters, deviceNames) then begin
        for i := 0 to (mountedDriveLetters.Count - 1) do begin
          if (deviceNames[i] = deviceName) then begin
            Result := mountedDriveLetters[i][1];
            break;
          end;

        end;

      end;

    finally
      deviceNames.Free();
    end;
  finally
    mountedDriveLetters.Free();
  end;

end;


 // ----------------------------------------------------------------------------
 // Connects to the named device, returns a device handle, or 0 on error
function TOTFEFreeOTFE.ConnectDevice(devicename: String): THandle;
var
  deviceHandle: THandle;
begin
  deviceHandle := CreateFile(PChar(devicename), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

  if (deviceHandle = INVALID_HANDLE_VALUE) then begin
    deviceHandle := 0;
  end;

  Result := deviceHandle;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DisconnectDevice(deviceHandle: THandle): Boolean;
begin
  CloseHandle(deviceHandle);
  Result := True;
end;


 // ----------------------------------------------------------------------------
 // Create/delete DosDevices symlink; effectivly wraps DefineDosDevice(...) with
 // additional support required for Vista changes
 // createNotDelete - Set to TRUE to create, FALSE to delete
function TOTFEFreeOTFE.DosDeviceSymlink(
  CreateNotDelete: Boolean;
  DeviceName: Ansistring;
  DriveLetter: DriveLetterChar;
  Global: Boolean
  ): Boolean;
var
  DIOCBuffer:       TDIOC_DOS_MOUNTPOINT;
  bytesReturned:    DWORD;
  mountpoint:       Ansistring;
  DIOCCode:         DWORD;
  driveLetterColon: WideString;
begin
  Result := False;

  CheckActive();

  DriveLetter := upcase(DriveLetter);

  // v2.00 of the FreeOTFE driver use DIOC call to create global/local version
  if not (CheckVersionAtLeast(FREEOTFE_ID_v02_00_0000)) then begin
    driveLetterColon := DriveLetter + ':';

    if CreateNotDelete then begin
      Result := DefineDosDevice(DDD_RAW_TARGET_PATH, PWideChar(driveLetterColon),
        PWideChar(deviceName));
    end else begin
      Result := DefineDosDevice(DDD_REMOVE_DEFINITION, PWideChar(driveLetterColon), nil);
    end;

  end else begin
    mountpoint := DriveLetter + ':';

    StrPCopy(DIOCBuffer.DiskDeviceName, DeviceName);
    DIOCBuffer.Global := Global;
    StrPCopy(DIOCBuffer.Mountpoint, mountpoint);

    if CreateNotDelete then begin
      DIOCCode := IOCTL_FREEOTFE_CREATE_DOS_MOUNTPOINT;
    end else begin
      DIOCCode := IOCTL_FREEOTFE_DELETE_DOS_MOUNTPOINT;
    end;

    if (DeviceIoControl(fdriver_handle, DIOCCode, @DIOCBuffer, sizeof(DIOCBuffer),
      nil, 0, bytesReturned, nil)) then begin
      Result := True;

      BroadcastDriveChangeMessage(CreateNotDelete, DriveLetter);
    end;
  end;

end;


 // ----------------------------------------------------------------------------
 // Delete both local and global DosDevices symlinks, as possible
function TOTFEFreeOTFE.DeleteDosDeviceSymlink(DeviceName: String; DriveLetter: Char): Boolean;
begin
  Result := False;

  // Note: We do ***NOT*** use
  //         Result := (
  //                    DosDeviceSymlink(..., TRUE) or
  //                    DosDeviceSymlink(..., FALSE)
  //                   );
  //       here, as lazy evaluation will prevent one or the other from being
  //       evaluated - we want *both* to always be executed!
  if DosDeviceSymlink(False, DeviceName, DriveLetter, True) then begin
    Result := True;
  end;

  if DosDeviceSymlink(False, DeviceName, DriveLetter, False) then begin
    Result := True;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DoQueryDosDevice(deviceName: Ansistring;
  queryResults: TStringList): Boolean;
const
  buffSizeIncrement: Integer = 1024;
var
  ptrBuffer:       Pointer;
  ptrBufferOffset: Pointer;
  dev:             String;
  offset:          Integer;
  z:               Integer;
  pcharDeviceName: PAnsiChar;
  bufferSize:      Integer;
  bufferUsed:      Integer;
  finished:        Boolean;
begin
  Result := False;

  bufferUsed := 0;

  pcharDeviceName := PAnsiChar(deviceName);
  if (deviceName = '') then begin
    pcharDeviceName := nil;
  end;

  finished   := False;
  bufferSize := buffSizeIncrement;
  ptrBuffer  := AllocMem(bufferSize);
  FillChar(ptrBuffer^, bufferSize, 'Z');
  try
    while (not (finished)) do begin
      // Cleardown any previous error
      SetLastError($FFFFFFFF);

      // Here, we dereference ptrBuffer to zero the buffer's contents
      bufferUsed := QueryDosDeviceA(pcharDeviceName, ptrBuffer, bufferSize);

      // We need this "if" statement because Windows 2000 *will* return a partial
      // buffer (set bufferUsed > 0) if the buffer is large enough to store some,
      // but not all, of the information
      // Windows XP will only return bufferUsed > 0 if *all* information could
      // be returned (it seems)
      if (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then begin
        FreeMem(ptrBuffer);
        bufferSize := bufferSize + 1024; // Increment buffer size by 10K
        ptrBuffer  := AllocMem(bufferSize);
        continue;
      end else begin
        finished := True;
      end;

    end;


    if (bufferUsed > 0) then begin
      queryResults.Clear();

      offset := 0;
      while (offset <= bufferUsed) do begin
        ptrBufferOffset := PAnsiChar(ptrBuffer) + offset;

        z := StrLen(PAnsiChar(ptrBufferOffset));

        // From the help file on QueryDosDevice:
        //   "The final null-terminated string is followed by an additional NULL."
        // i.e. if z=0, then we've done the last one; quit the loop
        if (z = 0) then begin
          break;
        end;

        dev := Copy(PAnsiChar(ptrBufferOffset), 1, z);
        queryResults.add(dev);

        offset := offset + length(dev) + 1;
      end;

      Result := True;
    end;

  finally
    FreeMem(ptrBuffer);
  end;

end;


 // ----------------------------------------------------------------------------
 // Given an MSDOS device name, return the device name of that device
 // Returns '' on error
function TOTFEFreeOTFE.GetDeviceName(MSDOSKernelModeDeviceName: String): Ansistring;
var
  deviceNames:       TStringList;
  deviceNameMapping: TStringList;
  i, j:              Integer;
begin
  Result := '';

  deviceNames := TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then begin
      for i := 0 to (deviceNames.Count - 1) do begin
        deviceNameMapping := TStringList.Create();
        try

          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then begin
            for j := 0 to (deviceNameMapping.Count - 1) do begin
              if (deviceNameMapping[j] = MSDOSKernelModeDeviceName) then begin
                Result := deviceNames[i];
                break;
              end;
            end;

            // If we've found the name, exit the outer loop as well
            if (Result <> '') then begin
              break;
            end;

          end;

        finally
          deviceNameMapping.Free();
        end;

      end;

    end;

  finally
    deviceNames.Free();
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean;
var
  deviceNames:       TStringList;
  deviceNameMapping: TStringList;
  i, j:              Integer;
begin
  Result := True;

  SetLength(cypherDrivers, 0);

  deviceNames := TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then begin
      for i := 0 to (deviceNames.Count - 1) do begin
        DebugMsg('Device name: ' + deviceNames[i],0);
        deviceNameMapping := TStringList.Create();
        try
          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then begin
            for j := 0 to (deviceNameMapping.Count - 1) do begin
              if pos(DEVICE_CYPHER_DIR_NAME, deviceNameMapping[j]) > 0 then begin
                // Get the details for the device
                SetLength(cypherDrivers, (Length(cypherDrivers) + 1));
                Result := GetCypherDriverCyphers(deviceNameMapping[j],
                  cypherDrivers[high(cypherDrivers)]);

                // If we got the details, bang on to the next device
                // Note: If we couldn't get the details, this will have the
                //       effect of allowing the loop to go round again, but
                //       this time Result will still be FALSE; if the loop
                //       eventually exits, then Result will *still* be false,
                //       signalling an error
                // Note: This "break" condition differs between the DLL and kernel
                //       driver versions
                if Result then begin
                   DebugMsg('Cypher Driver found : ' + IntToStr(j)+' ' + deviceNameMapping[j],0);
                  break;
                end;
              end;

            end;  // for j:=1 to (deviceNameMapping.count-1) do

          end;

        finally
          deviceNameMapping.Free();
        end;

        // Propogate breakout, if Result is FALSE
        if (not (Result)) then begin
          break;
        end;

      end;  // for i:=1 to (deviceNames.count-1) do

    end;

  finally
    deviceNames.Free();
  end;
  DebugMsg('Exit GetCypherDrivers',0);
end;

(*
 // ----------------------------------------------------------------------------
 // Get cypher details - using FreeOTFE v3 and later API
 // Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFE._GetCypherDriverCyphers_v1(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  cypherHandle:      THandle;
  BytesReturned:     DWORD;
  DIOCBuffer:        TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer:         Pointer;
  ptrBufferOffset:   Pointer;
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v1;  // for debugging only

  bufferSize:         Cardinal;
  cypherDetails:      PCYPHER_v1;
  i:                  Integer;
  currCypherMode:     TFreeOTFECypherMode;
  arrIdx:             Integer;
  deviceUserModeName: Ansistring;
begin
  Result := False;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then begin
    Result := True;
  end else begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);


    DebugMsg('Connecting to: ' + deviceUserModeName);

    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then begin

      DebugMsg('Couldn''t connect to: ' + deviceUserModeName);

    end else begin
      if (DeviceIoControl(cypherHandle, IOCTL_FREEOTFECYPHER_IDENTIFYDRIVER,
        nil, 0, @DIOCBuffer, sizeof(DIOCBuffer), BytesReturned, nil)) then begin

        DebugMsg('_1_ supplied: ' + IntToStr(sizeof(DIOCBuffer)) + ' used: ' +
          IntToStr(BytesReturned));

        if BytesReturned = sizeof(DIOCBuffer) then begin
          cypherDriverDetails.DriverGUID           := DIOCBuffer.DriverGUID;
          cypherDriverDetails.DeviceName           := GetDeviceName(cypherDriver);
          cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
          cypherDriverDetails.Title                :=
            Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          cypherDriverDetails.VersionID            := DIOCBuffer.VersionID;
          cypherDriverDetails.CypherCount          := DIOCBuffer.CypherCount;


          bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) -
            sizeof(cypherDetails^) + (sizeof(cypherDetails^) *
            cypherDriverDetails.CypherCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer  := allocmem(bufferSize);

          SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

          if (DeviceIoControl(cypherHandle, IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v1,
            nil, 0, ptrBuffer, bufferSize, BytesReturned, nil)) then begin
            DebugMsg('_2_ supplied: ' + IntToStr(bufferSize) + ' used: ' +
              IntToStr(BytesReturned));

            if BytesReturned <= bufferSize then begin
              DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v1(ptrBuffer));
              DebugMsg('cypher details count: ' + IntToStr(DIOCBufferCyphers.BufCount));

              ptrBufferOffset :=
                Pointer(PAnsiChar(ptrBuffer) + sizeof(
                TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) - sizeof(cypherDetails^));
              arrIdx          := low(cypherDriverDetails.Cyphers);
              for i := 1 to cypherDriverDetails.CypherCount do begin
                cypherDetails := (PCYPHER_v1(ptrBufferOffset));
                cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
                cypherDriverDetails.Cyphers[arrIdx].Title :=
                  Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

                // Failsafe to "unknown"
                cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
                for currCypherMode :=
                  low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do begin
                  if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then begin
                    cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
                  end;

                end;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying :=
                  cypherDetails.KeySizeUnderlying;
                cypherDriverDetails.Cyphers[arrIdx].BlockSize         := cypherDetails.BlockSize;
                cypherDriverDetails.Cyphers[arrIdx].VersionID         := cypherDetails.VersionID;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired :=
                  cypherDetails.KeySizeUnderlying;

                // Setup for the next one...
                ptrBufferOffset :=
                  Pointer(PAnsiChar(ptrBufferOffset) + sizeof(cypherDetails^));
                Inc(arrIdx);
              end;

              // Cache the information retrieved
              CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

              Result := True;
            end;

          end else begin

            DebugMsg('getcypherdrivers DIOC 2 FAIL');

          end;

          FreeMem(ptrBuffer);
        end;

      end;

      DisconnectDevice(cypherHandle);
    end;

  end;  // if CachesGetCypherDriver(cypherKernelModeDeviceName, cypherDriverDetails) then



  DebugMsg('Exiting function...');
end;   *)

 // ----------------------------------------------------------------------------
 // Get cypher details - using FreeOTFE v3 and later API
 // Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFE._GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  cypherHandle:       THandle;
  BytesReturned:      DWORD;
  DIOCBuffer:         TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer:          Pointer;
  ptrBufferOffset:    Pointer;
  DIOCBufferCyphers:  PDIOC_CYPHER_IDENTIFYSUPPORTED_v3;// for debugging only
  bufferSize:         Cardinal;
  cypherDetails:      PCYPHER_v3;
  i:                  Integer;
  currCypherMode:     TFreeOTFECypherMode;
  arrIdx:             Integer;
  deviceUserModeName: Ansistring;
begin
  Result := False;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then begin
    Result := True;
  end else begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);


    DebugMsg('Connecting to: ' + deviceUserModeName);

    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then begin

      DebugMsg('Couldn''t connect to: ' + deviceUserModeName);

    end else begin
      if (DeviceIoControl(cypherHandle, IOCTL_FREEOTFECYPHER_IDENTIFYDRIVER,
        nil, 0, @DIOCBuffer, sizeof(DIOCBuffer), BytesReturned, nil)) then begin

  DebugMsg('_1_ supplied: '+inttostr(sizeof(DIOCBuffer))+' used: '+inttostr(BytesReturned));

        if BytesReturned = sizeof(DIOCBuffer) then begin
          cypherDriverDetails.DriverGUID           := DIOCBuffer.DriverGUID;
          cypherDriverDetails.DeviceName           := GetDeviceName(cypherDriver);
          cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
          cypherDriverDetails.Title                :=
            Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          cypherDriverDetails.VersionID            := DIOCBuffer.VersionID;
          cypherDriverDetails.CypherCount          := DIOCBuffer.CypherCount;


          bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
            sizeof(cypherDetails^) + (sizeof(cypherDetails^) *
            cypherDriverDetails.CypherCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer  := allocmem(bufferSize);

          SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

          if (DeviceIoControl(cypherHandle, IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v3,
            nil, 0, ptrBuffer, bufferSize, BytesReturned, nil)) then begin

            DebugMsg('_2_ supplied: ' + IntToStr(bufferSize) + ' used: ' +
              IntToStr(BytesReturned));

            if BytesReturned <= bufferSize then begin

              DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v3(ptrBuffer));
              DebugMsg('cypher details count: ' + IntToStr(DIOCBufferCyphers.BufCount));

              ptrBufferOffset :=
                Pointer(PAnsiChar(ptrBuffer) + sizeof(
                TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) - sizeof(cypherDetails^));
              arrIdx          := low(cypherDriverDetails.Cyphers);
              for i := 1 to cypherDriverDetails.CypherCount do begin
                cypherDetails := (PCYPHER_v3(ptrBufferOffset));
                cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
                cypherDriverDetails.Cyphers[arrIdx].Title :=
                  Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

                // Failsafe to "unknown"
                cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
                for currCypherMode :=
                  low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do begin
                  if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then begin
                    cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
                  end;

                end;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired :=
                  cypherDetails.KeySizeRequired;
                cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying :=
                  cypherDetails.KeySizeUnderlying;
                cypherDriverDetails.Cyphers[arrIdx].BlockSize       := cypherDetails.BlockSize;
                cypherDriverDetails.Cyphers[arrIdx].VersionID       := cypherDetails.VersionID;

                // Setup for the next one...
                ptrBufferOffset :=
                  Pointer(PAnsiChar(ptrBufferOffset) + sizeof(cypherDetails^));
                Inc(arrIdx);
              end;

              // Cache the information retrieved
              CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

              Result := True;
            end;

          end else begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('getcypherdrivers DIOC 2 FAIL');
  {$ENDIF}
          end;

          FreeMem(ptrBuffer);
        end;

      end;

      DisconnectDevice(cypherHandle);
    end;

  end;  // if CachesGetCypherDriver(cypherKernelModeDeviceName, cypherDriverDetails) then


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting _GetCypherDriverCyphers_v3');
{$ENDIF}

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean;
var
  deviceNames:       TStringList;
  deviceNameMapping: TStringList;
  i, j:              Integer;
begin
  Result := True;

  SetLength(hashDrivers, 0);

  deviceNames := TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then begin
      for i := 0 to (deviceNames.Count - 1) do begin
        deviceNameMapping := TStringList.Create();
        try
          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then begin
            for j := 0 to (deviceNameMapping.Count - 1) do begin
              if pos(DEVICE_HASH_DIR_NAME, deviceNameMapping[j]) > 0 then begin
                // Get the details for the device
                SetLength(hashDrivers, (Length(hashDrivers) + 1));
                Result := GetHashDriverHashes(deviceNameMapping[j],
                  hashDrivers[high(hashDrivers)]);

                // If we got the details, bang on to the next device
                // Note: If we couldn't get the details, this will have the
                //       effect of allowing the loop to go round again, but
                //       this time Result will still be FALSE; if the loop
                //       eventually exits, then Result will *still* be false,
                //       signalling an error
                // Note: This "break" condition differs between the DLL and kernel
                //       driver versions
                if Result then begin
                  break;
                end;
              end;

            end;  // for j:=1 to (deviceNameMapping.count-1) do

          end;

        finally
          deviceNameMapping.Free();
        end;

        // Propogate breakout, if Result is FALSE
        if (not (Result)) then begin
          break;
        end;

      end;  // for i:=1 to (deviceNames.count-1) do

    end;

  finally
    deviceNames.Free();
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetHashDriverHashes(hashDriver: string;
  var hashDriverDetails: TFreeOTFEHashDriver): Boolean;
var
  hashHandle:         THandle;
  BytesReturned:      DWORD;
  DIOCBuffer:         TDIOC_HASH_IDENTIFYDRIVER;
  ptrBuffer:          Pointer;
  ptrBufferOffset:    Pointer;
  DIOCBufferHashes: PDIOC_HASH_IDENTIFYSUPPORTED;

  bufferSize:         Cardinal;
  hashDetails:        PHASH;
  i:                  Integer;
  arrIdx:             Integer;
  deviceUserModeName: Ansistring;
begin
  Result := False;

  if CachesGetHashDriver(hashDriver, hashDriverDetails) then begin
    Result := True;
  end else begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetHashDeviceUserModeDeviceName(hashDriver);


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+deviceUserModeName);
{$ENDIF}
    hashHandle := ConnectDevice(deviceUserModeName);
    if (hashHandle = 0) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
    end else begin

      if (DeviceIoControl(hashHandle, IOCTL_FREEOTFEHASH_IDENTIFYDRIVER,
        nil, 0, @DIOCBuffer, sizeof(DIOCBuffer), BytesReturned, nil)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_3_ supplied: '+inttostr(sizeof(DIOCBuffer))+' used: '+inttostr(BytesReturned));
{$ENDIF}
        if BytesReturned = sizeof(DIOCBuffer) then begin
          hashDriverDetails.DriverGUID           := DIOCBuffer.DriverGUID;
          hashDriverDetails.DeviceName           := GetDeviceName(hashDriver);
          hashDriverDetails.LibFNOrDevKnlMdeName := hashDriver;
          hashDriverDetails.DeviceUserModeName   := DeviceUserModeName;
          hashDriverDetails.Title                :=
            Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          hashDriverDetails.VersionID            := DIOCBuffer.VersionID;
          hashDriverDetails.HashCount            := DIOCBuffer.HashCount;


          bufferSize := sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
            sizeof(hashDetails^) + (sizeof(hashDetails^) * hashDriverDetails.HashCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer  := allocmem(bufferSize);

          SetLength(hashDriverDetails.Hashes, hashDriverDetails.HashCount);

          if (DeviceIoControl(hashHandle, IOCTL_FREEOTFEHASH_IDENTIFYSUPPORTED,
            nil, 0, ptrBuffer, bufferSize, BytesReturned, nil)) then begin

DebugMsg('_4_ supplied: '+inttostr(bufferSize)+' used: '+inttostr(BytesReturned));

            if (BytesReturned <= bufferSize) then begin

DIOCBufferHashes := (PDIOC_HASH_IDENTIFYSUPPORTED(ptrBuffer));
DebugMsg('hash details count: '+inttostr(DIOCBufferHashes.BufCount));

              ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
                sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) - sizeof(hashDetails^));
              arrIdx          := low(hashDriverDetails.Hashes);
              for i := 1 to hashDriverDetails.HashCount do begin
                hashDetails := (PHASH(ptrBufferOffset));
                hashDriverDetails.Hashes[arrIdx].HashGUID := hashDetails.HashGUID;
                hashDriverDetails.Hashes[arrIdx].Title :=
                  Copy(hashDetails.Title, 1, StrLen(hashDetails.Title));
                hashDriverDetails.Hashes[arrIdx].VersionID := hashDetails.VersionID;
                hashDriverDetails.Hashes[arrIdx].Length := hashDetails.Length;
                hashDriverDetails.Hashes[arrIdx].BlockSize := hashDetails.BlockSize;

                // Setup for the next one...
                ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + sizeof(hashDetails^));
                Inc(arrIdx);
              end;

              // Cache the information retrieved
              CachesAddHashDriver(hashDriver, hashDriverDetails);

              Result := True;
            end;

          end else begin

DebugMsg('gethashdrivers DIOC 2 FAIL');

          end;

          FreeMem(ptrBuffer);
        end;

      end;

      DisconnectDevice(hashHandle);
    end;

  end;  // if CachesGetHashDriver(driver, hashDriverDetails) then

end;


 // ----------------------------------------------------------------------------
 // Convert kernel mode device name to user mode device name
function TOTFEFreeOTFE.GetHashDeviceUserModeDeviceName(hashKernelModeDeviceName: String): String;
var
  deviceUserModeName: String;
begin
  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := hashKernelModeDeviceName;
  // Lose the prefix
  Delete(deviceUserModeName, 1, length(DEVICE_HASH_DIR_NAME));
  // Trim off the leading "\"
  Delete(deviceUserModeName, 1, 1);
  // Tack on the dosdevice root
  deviceUserModeName := DEVICE_HASH_SYMLINK_PREFIX + deviceUserModeName;

  Result := deviceUserModeName;
end;


 // ----------------------------------------------------------------------------
 // Convert kernel mode device name to user mode device name
function TOTFEFreeOTFE.GetCypherDeviceUserModeDeviceName(cypherKernelModeDeviceName:
  String): String;
var
  deviceUserModeName: String;
begin
  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := cypherKernelModeDeviceName;
  // Lose the prefix
  Delete(deviceUserModeName, 1, length(DEVICE_CYPHER_DIR_NAME));
  // Trim off the leading "\"
  Delete(deviceUserModeName, 1, 1);
  // Tack on the dosdevice root
  deviceUserModeName := DEVICE_CYPHER_SYMLINK_PREFIX + deviceUserModeName;

  Result := deviceUserModeName;
end;


 // ----------------------------------------------------------------------------
 // Hash the specified data with the supplied hash device/hash GUID
function TOTFEFreeOTFE.HashData(
  hashDriver: Ansistring;
  hashGUID: TGUID;
  const data: TSDUBytes;
  out hashOut: TSDUBytes
  ): Boolean;
var
  hashHandle:           THandle;
  BytesReturned:        DWORD;
  ptrDIOCBufferIn:      PDIOC_HASH_DATA_IN;
  bufferSizeIn:         DWORD;
  ptrDIOCBufferOut:     PDIOC_HASH_DATA_OUT;
  bufferSizeOut:        DWORD;
  hashDetails:          TFreeOTFEHash;
  deviceUserModeName:   String;
  hashByteCount:        Integer;
  expectedHashSizeBits: Integer;  // In *bits*
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetHashDeviceUserModeDeviceName(hashDriver);

  if GetSpecificHashDetails(hashDriver, hashGUID, hashDetails) then begin
    hashHandle := ConnectDevice(deviceUserModeName);
    if (hashHandle = 0) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
    end else begin
      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(Byte) + Length(Data);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        expectedHashSizeBits := hashDetails.Length;
        if (hashDetails.Length = -1) then begin
          expectedHashSizeBits := (Length(Data) * 8);
        end;
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(Byte) +
          (expectedHashSizeBits div 8);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.HashGUID := hashGUID;

          ptrDIOCBufferIn.DataLength := Length(Data) * 8;
          StrMove(@ptrDIOCBufferIn.Data, PAnsiChar(Data), Length(Data));

          if (DeviceIoControl(hashHandle, IOCTL_FREEOTFEHASH_HASHDATA,
            ptrDIOCBufferIn, bufferSizeIn, ptrDIOCBufferOut, bufferSizeOut,
            BytesReturned, nil)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_5_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then begin
              hashByteCount := (ptrDIOCBufferOut.HashLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hashByteCount: '+inttostr(hashByteCount));
{$ENDIF}

              // Set hashOut so that it has enough characters which can be
              // overwritten with StrMove
              SDUInitAndZeroBuffer(hashByteCount, hashOut);
              //              hashOut := StringOfChar(#0, hashByteCount);
              StrMove(PAnsiChar(hashOut), @ptrDIOCBufferOut.Hash, hashByteCount);

              Result := True;
            end else begin
              LastErrorCode := OTFE_ERR_HASH_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
            end;

          end else begin
            LastErrorCode := OTFE_ERR_HASH_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hash DIOC 2 FAIL');
{$ENDIF}
          end;

        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;

      DisconnectDevice(hashHandle);
    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to GetSpecificHashDetails');
{$ENDIF}
  end;  // ELSE PART - if GetSpecificHashDetails(hashKernelModeDeviceName, hashGUID, hashDetails) then

end;


 // ----------------------------------------------------------------------------
 // Generate MAC of data using hash driver/encryption driver identified
 // Note: "tBits" is in *bits* not *bytes*
 // tBits - Set the the number of bits to return; set to -ve value for no
 //         truncation/padding
 //         If tBits is larger than the number of bits the MAC would normally
 //         be, then the MAC will be right-padded with NULLs
 //         If tBits is set to a -ve number, then this function will return
 //         an MAC of variable length
function TOTFEFreeOTFE.MACData(
  macAlgorithm: TFreeOTFEMACAlgorithm;
  HashDriver: Ansistring;
  HashGUID: TGUID;
  CypherDriver: Ansistring;
  CypherGUID: TGUID;
  var key: PasswordString;
  var data: Ansistring;
  var MACOut: Ansistring;
  tBits: Integer = -1
  ): Boolean;
var
  BytesReturned:    DWORD;
  ptrDIOCBufferIn:  PDIOC_GENERATE_MAC_IN_PC_DRIVER;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_GENERATE_MAC_OUT;
  bufferSizeOut:    DWORD;
  outputByteCount:  Integer;
  tmpSizeBytes:     Integer;
  minBufferSizeOut: Integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Key) +
    Length(key) - sizeof(ptrDIOCBufferIn^.Data) + Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full DK length depends on
    // the algorithm, hash, etc
    tmpSizeBytes := (max(MAX_MAC_LENGTH, tBits) div 8);  // Convert to bytes


    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.MAC);
    bufferSizeOut    := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut    := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.MACAlgorithm := FreeOTFEMACID[macAlgorithm];

      StrPCopy(ptrDIOCBufferIn.HashDeviceName, HashDriver);
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StrPCopy(ptrDIOCBufferIn.CypherDeviceName, CypherDriver);
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.LengthWanted := tBits;

      ptrDIOCBufferIn.KeyLength  := Length(key) * 8;
      ptrDIOCBufferIn.DataLength := Length(data) * 8;
      // This may seem a little weird, but we do this because the data is
      // immediatly after the key
      StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key), Length(key));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)), PAnsiChar(data), Length(data));


      if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GENERATE_MAC,
        ptrDIOCBufferIn, bufferSizeIn, ptrDIOCBufferOut, bufferSizeOut, BytesReturned, nil))
      then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC out buffer supplied: '+inttostr(bufferSizeOut)+'; DIOC call used: '+inttostr(BytesReturned)+'; min: '+inttostr(minBufferSizeOut));
{$ENDIF}
        if ((BytesReturned >= DWORD(minBufferSizeOut)) and
          (BytesReturned <= DWORD(bufferSizeOut))) then begin
          outputByteCount := (ptrDIOCBufferOut.MACLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

          // Set MACOut so that it has enough characters which can be
          // overwritten with StrMove
          MACOut := StringOfChar(#0, outputByteCount);
          StrMove(PAnsiChar(MACOut), @ptrDIOCBufferOut.MAC, outputByteCount);

          Result := True;
        end else begin
          LastErrorCode := OTFE_ERR_MAC_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
        end;

      end else begin
        LastErrorCode := OTFE_ERR_MAC_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC DIOC 2 FAIL');
{$ENDIF}
      end;

    finally
      FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
      FreeMem(ptrDIOCBufferOut);
    end;

  finally
    FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
    FreeMem(ptrDIOCBufferIn);
  end;

end;


 // ----------------------------------------------------------------------------
 // dkLenBits - This must *always* be >= 0
function TOTFEFreeOTFE.DeriveKey(
  kdfAlgorithm: TFreeOTFEKDFAlgorithm;
  HashDriver: Ansistring;
  HashGUID: TGUID;
  CypherDriver: Ansistring;
  CypherGUID: TGUID;
  Password: TSDUBytes;
  Salt: array of Byte;
  Iterations: Integer;
  dkLenBits: Integer;  // In *bits*
  out DK: TSDUBytes
  ): Boolean;
var
  BytesReturned:    DWORD;
  ptrDIOCBufferIn:  PDIOC_DERIVE_KEY_IN_PC_DRIVER;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_DERIVE_KEY_OUT;
  bufferSizeOut:    DWORD;
  outputByteCount:  Integer;
  tmpSizeBytes:     Integer;
  minBufferSizeOut: Integer;
begin
  DebugMsg('DeriveKey:');
  DebugMsg(IntToStr(Ord(kdfAlgorithm)));
  DebugMsg(HashDriver);
  DebugMsg(HashGUID);
  DebugMsg(CypherDriver);
  DebugMsg(CypherGUID);
  DebugMsg(Password);
  DebugMsg(Format('Password len %d',[length(Password)]));
  DebugMsg(Salt);
  DebugMsg(Iterations);
  DebugMsg(dkLenBits);
  assert(Iterations<= 1000000);// debugging
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();


  bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Password) +
    Length(Password) - sizeof(ptrDIOCBufferIn^.Salt) + Length(Salt);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    tmpSizeBytes := (max(MAX_DERIVED_KEY_LENGTH, dkLenBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.DerivedKey);
    bufferSizeOut    := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut    := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.KDFAlgorithm := FreeOTFEKDFID[kdfAlgorithm];

      StrPCopy(ptrDIOCBufferIn.HashDeviceName, HashDriver);
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StrPCopy(ptrDIOCBufferIn.CypherDeviceName, CypherDriver);
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.Iterations   := Iterations;
      ptrDIOCBufferIn.LengthWanted := dkLenBits;

      ptrDIOCBufferIn.PasswordLength := (Length(Password) * 8);
      ptrDIOCBufferIn.SaltLength     := (Length(Salt) * 8);
      // This may seem a little weird, but we do this because the salt is
      // immediatly after the password
      StrMove(@ptrDIOCBufferIn.Password, PAnsiChar(Password), Length(Password));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Password)) + length(Password)),
        PAnsiChar(@Salt[0]), Length(Salt));

       DebugFlush();
      if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_DERIVE_KEY, ptrDIOCBufferIn,
        bufferSizeIn, ptrDIOCBufferOut, bufferSizeOut, BytesReturned, nil)) then begin

        DebugMsg('MAC out buffer supplied: ' + IntToStr(bufferSizeOut) +
          '; DIOC call used: ' + IntToStr(BytesReturned) + '; min: ' + IntToStr(minBufferSizeOut));

        if ((BytesReturned >= DWORD(minBufferSizeOut)) and
          (BytesReturned <= DWORD(bufferSizeOut))) then begin
          outputByteCount := (ptrDIOCBufferOut.DerivedKeyLength div 8);

          DebugMsg('outputByteCount: ' + IntToStr(outputByteCount));
          // Set DK so that it has enough characters which can be
          // overwritten with StrMove
          SDUInitAndZeroBuffer(outputByteCount, DK);
          //          DK := StringOfChar(#0, outputByteCount);
          StrMove(PAnsiChar(DK), @ptrDIOCBufferOut.DerivedKey, outputByteCount);

          Result := True;
        end else begin
          LastErrorCode := OTFE_ERR_KDF_FAILURE;

          DebugMsg('incorrect bytecount returned');

        end;

      end else begin
        LastErrorCode := OTFE_ERR_KDF_FAILURE;

        DebugMsg('MAC DIOC 2 FAIL');

      end;

    finally
      FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
      FreeMem(ptrDIOCBufferOut);
    end;

  finally
    FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
    FreeMem(ptrDIOCBufferIn);
  end;
  DebugMsg(DK);
end;


 // ----------------------------------------------------------------------------
 // encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFE._EncryptDecryptData(
  encryptFlag: Boolean;
  cypherDriver: Ansistring;
  cypherGUID: TGUID;
  var key: TSDUBytes;
  var IV: Ansistring;
  var inData: Ansistring;
  var outData: Ansistring
  ): Boolean;
var
  cypherHandle:       THandle;
  BytesReturned:      DWORD;
  ptrDIOCBufferIn:    PDIOC_CYPHER_DATA_IN;
  bufferSizeIn:       DWORD;
  ptrDIOCBufferOut:   PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut:      DWORD;
  cypherDetails:      TFreeOTFECypher_v3;
  deviceUserModeName: String;
  dwIoControlCode:    DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then begin
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;

      DebugMsg('couldn''t connect to: ' + deviceUserModeName);

    end else begin
      dwIoControlCode := IOCTL_FREEOTFECYPHER_DECRYPT;
      if encryptFlag then begin
        dwIoControlCode := IOCTL_FREEOTFECYPHER_ENCRYPT;
      end;


      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Key) +
        Length(key) - sizeof(ptrDIOCBufferIn^.IV) + Length(IV) -
        sizeof(ptrDIOCBufferIn^.Data) + Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) +
          Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key),
            Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)),
            PAnsiChar(IV), Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key) + length(IV)),
            PAnsiChar(inData), Length(inData));

          if (DeviceIoControl(cypherHandle, dwIoControlCode, ptrDIOCBufferIn,
            bufferSizeIn, ptrDIOCBufferOut, bufferSizeOut, BytesReturned, nil)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_6_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then begin
              // Set outData so that it has enough characters which can be
              // overwritten with StrMove
              outData := StringOfChar(#0, BytesReturned);

              StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, BytesReturned);

              Result := True;
            end else begin
              LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
            end;

          end else begin
            LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('cypher DIOC 2 FAIL');
{$ENDIF}
          end;


        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;


      DisconnectDevice(cypherHandle);
    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
  end;

end;


 // ----------------------------------------------------------------------------
 // encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFE.EncryptDecryptSectorData(
  encryptFlag: Boolean;
  cypherDriver: Ansistring;
  cypherGUID: TGUID;
  SectorID: LARGE_INTEGER;
  SectorSize: Integer;
  var key: TSDUBytes;
  var IV: Ansistring;
  var inData: Ansistring;
  var outData: Ansistring
  ): Boolean;
var
  cypherHandle:       THandle;
  BytesReturned:      DWORD;
  ptrDIOCBufferIn:    PDIOC_CYPHER_SECTOR_DATA_IN;
  bufferSizeIn:       DWORD;
  ptrDIOCBufferOut:   PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut:      DWORD;
  cypherDetails:      TFreeOTFECypher_v3;
  deviceUserModeName: String;
  dwIoControlCode:    DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then begin
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
    end else begin
      dwIoControlCode := IOCTL_FREEOTFECYPHER_DECRYPTSECTOR;
      if encryptFlag then begin
        dwIoControlCode := IOCTL_FREEOTFECYPHER_ENCRYPTSECTOR;
      end;


      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Key) +
        Length(key) - sizeof(ptrDIOCBufferIn^.IV) + Length(IV) -
        sizeof(ptrDIOCBufferIn^.Data) + Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) +
          Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.SectorID   := SectorID;
          ptrDIOCBufferIn.SectorSize := SectorSize;
          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediately after the key
          StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key),
            Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)),
            PAnsiChar(IV), Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key) + length(IV)),
            PAnsiChar(inData), Length(inData));

          if (DeviceIoControl(cypherHandle, dwIoControlCode, ptrDIOCBufferIn,
            bufferSizeIn, ptrDIOCBufferOut, bufferSizeOut, BytesReturned, nil)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_6_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then begin
              // Set outData so that it has enough characters which can be
              // overwritten with StrMove
              outData := StringOfChar(#0, BytesReturned);

              StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, BytesReturned);

              Result := True;
            end else begin
              LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
            end;

          end else begin
            LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('cypher DIOC 2 FAIL');
{$ENDIF}
          end;


        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;


      DisconnectDevice(cypherHandle);
    end;

    // If there was a problem, fallback to using v1 cypher API
    if not (Result) then begin
      Result := _EncryptDecryptData(encryptFlag, cypherDriver, cypherGUID,
        key, IV, inData, outData);

    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
  end;

end;


 // ----------------------------------------------------------------------------
 // Identify the next drive letter to mount a volume as
 // i.e. If userDriveLetter is not set to #0, then return either that letter, or
 //      the next free drive letter.
 //      If userDriveLetter is set to #0, then use requiredDriveLetter instead
 //      of userDriveLetter
 // userDriveLetter - The drive letter the user has specifically requested
 // requiredDriveLetter - The drive letter the system would normally use
 // Returns: Drive letter to mount as, or #0 on error
function TOTFEFreeOTFE.GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Char): Char;
var
  freeDriveLetters:  String;
  searchDriveLetter: Char;
begin
  Result := #0;

  searchDriveLetter := userDriveLetter;
  if (searchDriveLetter = #0) then begin
    searchDriveLetter := requiredDriveLetter;
  end;

  // If still #0, just get the next one after C:
  if (searchDriveLetter = #0) then begin
    searchDriveLetter := 'C';
  end;


  freeDriveLetters  := uppercase(SDUGetUnusedDriveLetters());
  searchDriveLetter := upcase(searchDriveLetter);

  // Delete drive letters from the free drive letters, until we hit one which
  // appears after the one we've been requested - or we run out of free drive
  // letters
  while (freeDriveLetters <> '') and (freeDriveLetters[1] < searchDriveLetter) do begin
    Delete(freeDriveLetters, 1, 1);
  end;

  if (freeDriveLetters <> '') then begin
    Result := freeDriveLetters[1];
  end;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DriverType(): String;
begin
  Result := _('Kernel driver');
end;



 // ----------------------------------------------------------------------------
 //PortableDriverFilenames
function TOTFEFreeOTFE.PortableStart(driverFilenames: TStringList;
  showProgress: Boolean): Boolean;
var
  DriverControlObj: TDriverControl;
begin
  DriverControlObj := TDriverControl.Create();
  try
    Result := DriverControlObj.InstallMultipleDrivers(driverFilenames, True,
      showProgress, True);
  finally
    DriverControlObj.Free();
  end;

  // In case drivers were added/removed...
  CachesFlush();

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.PortableStop(): Boolean;
var
  DriverControlObj: TDriverControl;
begin
  DriverControlObj := TDriverControl.Create();
  try
    Result := DriverControlObj.UninstallAllDrivers(True)
  finally
    DriverControlObj.Free();
  end;

  // In case drivers were added/removed...
  CachesFlush();

end;


 // -----------------------------------------------------------------------------
 // Identify how many drivers are currently running in portable mode
 // Returns -1 on failure
function TOTFEFreeOTFE.DriversInPortableMode(): Integer;
var
  DriverControlObj:     TDriverControl;
  allDrivers:           TStringList;
  wasInstalledPortable: Boolean;
  i:                    Integer;
begin
  Result := -1;

  try
    DriverControlObj := TDriverControl.Create();
    try
      allDrivers := TStringList.Create();
      try
        if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then begin
          Result := 0;
          for i := 0 to (allDrivers.Count - 1) do begin
            // If the driver was installed in portable mode, stop and uninstall it
            if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then
            begin
              if wasInstalledPortable then begin
                Inc(Result);
              end;

            end;
            // if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then

          end;  // for i:=0 to (allDrivers.count-1) do

        end;  // if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then

      finally
        allDrivers.Free();
      end;

    finally
      DriverControlObj.Free();
    end;

  except
    on EFreeOTFENeedAdminPrivs do begin
      // Do nothing - Result already set to -1
    end;

  end;

end;


 // ----------------------------------------------------------------------------
 // Convert a kernel mode volume filename to a user mode volume filename
function TOTFEFreeOTFE.GetUserModeVolumeFilename(kernelModeFilename: Ansistring): Ansistring;
begin
  Result := kernelModeFilename;


  // Kernel mode drivers take volume filenames of the format:
  //  \??\UNC<unc path>
  //  \??\<filename>
  //  <devicename>
  // e.g.:
  //   \??\UNC\myOtherComputer\shareName\dirname\file.dat
  //   \??\C:\file.dat
  //   \Device\Harddisk0\Partition1\path\filedisk.img
  //   \Device\Harddisk0\Partition1


  // Strip out the filename prefix
  if Pos('\??\UNC\', Result) = 1 then begin
    // \\server\share\path\filedisk.img
    Delete(Result, 1, length('\??\UNC\'));
    Result := '\\' + Result;
  end else
  if Pos('\??\', Result) = 1 then begin
    // C:\path\filedisk.img
    Delete(Result, 1, length('\??\'));
  end else begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    // Do nothing.
  end;

end;


 // ----------------------------------------------------------------------------
 // Convert a user mode volume filename to a kernel mode volume filename
function TOTFEFreeOTFE.GetKernelModeVolumeFilename(userModeFilename: String): String;
begin
  Result := '';


  // Kernel mode drivers take volume filenames of the format:
  //  \??\UNC<unc path>
  //  \??\<filename>
  //  <devicename>
  // e.g.:
  //   \??\UNC\myOtherComputer\shareName\dirname\file.dat
  //   \??\C:\file.dat
  //   \Device\Harddisk0\Partition1\path\filedisk.img
  //   \Device\Harddisk0\Partition1


  if (Pos('\\', userModeFilename) = 1) then begin
    // \\server\share\path\filedisk.img
    // Remove first "\"
    Delete(userModeFilename, 1, 1);
    Result := '\??\UNC' + userModeFilename;
//         Result := userModeFilename
  end else
  if ((Pos(':\', userModeFilename) = 2) or (Pos(':/', userModeFilename) = 2)) then begin
    // C:\path\filedisk.img
    Result := '\??\' + userModeFilename;
  end else begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    Result := userModeFilename;
  end;

end;


 // ----------------------------------------------------------------------------
 // Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
 // filename refers to a partition/file
function TOTFEFreeOTFE.IsPartition_KernelModeName(kernelModeFilename: String): Boolean;
begin
  // In kernel mode, partitions start with just "\", not "\??\"
  Result := (Pos('\??\', kernelModeFilename) <> 1);

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.ReadRawVolumeDataSimple(
  filename: String;
  offsetWithinFile: Int64;
  dataLength: DWORD;  // In bytes
  var Data: Ansistring
  ): Boolean;
var
  BytesReturned:    DWORD;
  kmFilename:       String;
  DIOCBufferIn:     TDIOC_GET_RAW_DATA_IN;
  ptrDIOCBufferOut: PDIOC_GET_RAW_DATA_OUT;
begin
  Result := False;

  CheckActive();

  // +1 because StrPCopy(...) copies a terminating NULL
  ptrDIOCBufferOut := allocmem(dataLength + 1);
  try
    // Determine the filename as the kernel sees it
    kmFilename := GetKernelModeVolumeFilename(filename);
//      kmFilename := '\\.\PHYSICALDRIVE1';

    StrPCopy(DIOCBufferIn.Filename, kmFilename);
    DIOCBufferIn.Offset     := offsetWithinFile;
    DIOCBufferIn.DataLength := dataLength;


    DebugMsg('Read raw data from: ' + filename);
    BytesReturned := 0;
    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_GET_RAW, @DIOCBufferIn,
      sizeof(DIOCBufferIn), ptrDIOCBufferOut, dataLength, BytesReturned, nil)) then begin
      DebugMsg('DIOC OK (bytes: ' + IntToStr(BytesReturned) + ')');

      if (BytesReturned = dataLength) then begin

        DebugMsg('Bytes OK');

        // Set data so that it has enough characters which can be
        // overwritten with StrMove
        data := StringOfChar(AnsiChar(#0), dataLength);
        StrMove(PAnsiChar(data), @ptrDIOCBufferOut.Data, dataLength);

        Result := True;
      end;

    end;
  finally
    FillChar(ptrDIOCBufferOut^, dataLength, 0);  // Overwrite buffer before
    // freeing off
    FreeMem(ptrDIOCBufferOut);
  end;

end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to write a critical data block from the named file
 // starting from the specified offset
 // criticalData - This should be set to the string representation of a raw
 //                (encrypted) critical data block
function TOTFEFreeOTFE.WriteRawVolumeDataSimple(
  filename: String;
  offsetWithinFile: Int64;
  data: Ansistring
  ): Boolean;
var
  BytesReturned: DWORD;
  kmFilename:    String;
  ptrDIOCBuffer: PDIOC_SET_RAW_DATA;
  bufferSize:    Integer;
begin
  Result := False;

  CheckActive();

  // Determine the volume filename as the kernel sees it
  kmFilename := GetKernelModeVolumeFilename(filename);

  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize    := sizeof(ptrDIOCBuffer^) - sizeof(ptrDIOCBuffer^.Data) + Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize    := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StrPCopy(ptrDIOCBuffer.Filename, kmFilename);
    ptrDIOCBuffer.Offset     := offsetWithinFile;
    ptrDIOCBuffer.DataLength := Length(data);

    StrMove(@ptrDIOCBuffer.Data, PAnsiChar(data), Length(data));


    DebugMsg('Write raw data from: '+filename);
    if (DeviceIoControl(fdriver_handle, IOCTL_FREEOTFE_SET_RAW, ptrDIOCBuffer,
      bufferSize, nil, 0, BytesReturned, nil)) then begin
      DebugMsg('Written OK');
      Result := True;
    end;

  finally
    FillChar(ptrDIOCBuffer^, bufferSize, 0);
    FreeMem(ptrDIOCBuffer);
  end;

end;


 // ----------------------------------------------------------------------------
 // Use the FreeOTFE device driver to mount the specified volume, and either:
 //   *) Read in and decrypt specified data, returning the decrypted version
 //   *) Encrypt specified data, and write to volume
 // readNotWrite - Set to TRUE to read, FALSE to write
function TOTFEFreeOTFE.ReadWritePlaintextToVolume(
  readNotWrite: Boolean;

  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  mountMountAs: TFreeOTFEMountAs;

  dataOffset: Int64;    // Offset from within mounted volume from where to read/write data
  dataLength: Integer;  // Length of data to read/write. In bytes
  var data: TSDUBytes;  // Data to read/write

  offset: Int64 = 0;  // Offset within volume where encrypted data starts
  size: Int64 = 0;    // Size of volume
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia): Boolean;
var
  deviceName:    String;
  dummyMetadata: TOTFEFreeOTFEVolumeMetaData;
  // emptyIV:TSDUBytes;
  strData:       Ansistring; { TODO 1 -otdk -crefactor : store data in arrays not strings }
begin
  LastErrorCode := OTFE_ERR_SUCCESS;

  CheckActive();

  DebugMsg('BEGIN  ReadWritePlaintextToVolume');
  DebugMsg(readNotWrite);
  DebugMsg(volFilename);
  DebugMsg(volumeKey);
  DebugMsg(Ord(sectorIVGenMethod));
  DebugMsg(IVHashDriver);
  DebugMsg(IVHashGUID);
  DebugMsg(IVCypherDriver);
  DebugMsg(IVCypherGUID);
  DebugMsg(mainCypherDriver);
  DebugMsg(mainCypherGUID);
  DebugMsg(VolumeFlags);
  DebugMsg(Ord(mountMountAs));
  DebugMsg(dataOffset);
  DebugMsg(dataLength);
  DebugMsg(offset);
  DebugMsg(size);
  DebugMsg(Ord(storageMediaType));
  DebugMsg('END  ReadWritePlaintextToVolume');

  // Attempt to create a new device to be used
  deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[mountMountAs]);
  Result     := (deviceName <> '');
  if Result then begin
    try
      PopulateVolumeMetadataStruct(
        False,              // Linux volume
        PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
        dummyMetadata
        );
      // SDUInitAndZeroBuffer(0,emptyIV);
      // Attempt to mount the device
      DebugFlush();
      Result := MountDiskDevice(deviceName, volFilename, volumeKey,
        sectorIVGenMethod, nil, readNotWrite, IVHashDriver, IVHashGUID, IVCypherDriver,
        // IV cypher
        IVCypherGUID,                // IV cypher
        mainCypherDriver,            // Main cypher
        mainCypherGUID,              // Main cypher
        VolumeFlags, dummyMetadata, offset, size,
        FreeOTFEMountAsStorageMediaType[mountMountAs]);
      if Result then begin
        try
          // Read decrypted data from mounted device
          if (readNotWrite) then begin
            Result := ReadRawVolumeData(deviceName, dataOffset, dataLength, strData);
            data   := SDUStringToSDUBytes(strData);

          end else begin
            Result := WriteRawVolumeData(deviceName, dataOffset,
              SDUBytesToString(data));
          end;

        finally
          // Unmount
          DismountDiskDevice(deviceName, True);
        end;

      end;  // if (Result) then

    finally
      // Destroy device
      DestroyDiskDevice(deviceName);
    end;

  end;  // if (Result) then

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.CanUserManageDrivers(): Boolean;
var
  testControlObj: TDriverControl;
begin
  Result := False;

  try
    // If we can't create a drive control object (i.e. we don't have admin
    // access to open the SCManager), we'll get an exception here.
    testControlObj := TDriverControl.Create();
    testControlObj.Free();
    Result := True;
  except
    on EFreeOTFENeedAdminPrivs do begin
      // Do nothing - just swallow the exception
    end;

  end;

end;


{returns an instance of the only object, must be type TOTFEFreeOTFE. call SetFreeOTFEType first}
function GetFreeOTFE: TOTFEFreeOTFE;
begin
  assert(GetFreeOTFEBase is TOTFEFreeOTFE, 'call SetFreeOTFEType with correct type');
  Result := GetFreeOTFEBase as TOTFEFreeOTFE;
end;


end.
