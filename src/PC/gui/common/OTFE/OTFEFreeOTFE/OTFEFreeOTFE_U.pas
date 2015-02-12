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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OTFE_U,
  dialogs,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_DriverCommon,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_DriverHashAPI,
  OTFEFreeOTFE_DriverCypherAPI,
  OTFEConsts_U,
  OTFEFreeOTFE_VolumeFileAPI,
  OTFEFreeOTFE_LUKSAPI,
  pkcs11_session,
  pkcs11_object,
  pkcs11_library,
  OTFEFreeOTFE_PKCS11,
  SDUGeneral;


type
  TOTFEFreeOTFE = class(TOTFEFreeOTFEBase)
  protected
    DriverHandle: THandle;
    fDefaultDriveLetter: ansichar;
    fDefaultMountAs: TFreeOTFEMountAs;

    // Connect/disconnect to the main FreeOTFE device driver
    function  Connect(): boolean; override;
    function  Disconnect(): boolean; override;


    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations
    // deviceType - Set to FILE_DEVICE_DISK, etc

    function  CreateDiskDevice(deviceType: DWORD = FILE_DEVICE_DISK): string;
    function  DestroyDiskDevice(deviceName: Ansistring): boolean;

    function  MountDiskDevice(
                              deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: Ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: Ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: Ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              metaData: TOTFEFreeOTFEVolumeMetaData;
                              offset: int64 = 0;
                              size: int64 = 0;
                              storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; override;

    function  CreateMountDiskDevice(
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: Ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: Ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: Ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              DriveLetter: Ansichar;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              offset: int64 = 0;
                              size: int64 = 0;
                              MetaData_LinuxVolume: boolean = FALSE;  // Linux volume
                              MetaData_PKCS11SlotID: integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
                              MountMountAs: TFreeOTFEMountAs = fomaFixedDisk;  // PC kernel drivers *only* - ignored otherwise
                              mountForAllUsers: boolean = TRUE  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; override;

    function  DismountDiskDevice(
                                 deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                                 emergency: boolean
                                ): boolean; override;

    function  LDREUDriver(
                          driveLetter: ansichar;
                          deviceName: Ansistring;
                          emergency: boolean
                         ): boolean;
    function  LDREUUserApp(
                          driveLetter: ansichar;
                          deviceName: Ansistring;
                          emergency: boolean;
                          var unableToOpenDrive: boolean
                         ): boolean;

    // ---------
    // Status functions

    // Get the number of disk devices the driver currently has (including both
    // mounted and unmounted disks)
    function  GetDiskDeviceCount(): cardinal;

    // Get the kernel mode device name of all disk devices
    function  GetDiskDeviceList(deviceNames: TStringList): boolean;


    // Get a list of all FreeOTFE devices which are currently mounted
    // Note: Unmounted devices will have '' in the "driveLetters" item
    //       corresponding to the unmounted device listed in the same item
    //       position in "deviceNames"
    function GetMountedDevices(driveLetters: TStringList; deviceNames: TStringList): boolean;

    // Given the specified drive letter, return the FreeOTFE device for that
    // mounted drive
    function GetDriveDeviceName(driveLetter: ansichar): string;
    // Given the specified device name, return the drive mounted on that device
    function GetDriveDeviceLetter(deviceName: string): char;


    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    // This executes a sipmle call to the driver to read dataLength bytes from
    // offsetWithinFile
    // Note: Will probably not work when reading directly from partitions;
    //       they typically need read/writes carried out in sector sized blocks
    //        - see ReadRawVolumeDataBounded for this.
    function ReadRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: Ansistring
                        ): boolean; override;

    function WriteRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         data: Ansistring
                        ): boolean; override;

    function ReadWritePlaintextToVolume(
                    readNotWrite: boolean;

                    volFilename: string;
                    volumeKey: TSDUBytes;
                    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                    IVHashDriver: Ansistring;
                    IVHashGUID: TGUID;
                    IVCypherDriver: Ansistring;
                    IVCypherGUID: TGUID;
                    mainCypherDriver: Ansistring;
                    mainCypherGUID: TGUID;
                    VolumeFlags: integer;
                    mountMountAs: TFreeOTFEMountAs;

                    dataOffset: int64;  // Offset from within mounted volume from where to read/write data
                    dataLength: integer;  // Length of data to read/write. In bytes
                    var data: ansistring;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean; override;

    // ---------
    // Misc functions

    // Call QueryDosDevice, and return it's output
    function DoQueryDosDevice(deviceName: Ansistring; queryResults: TStringList): boolean;
    // Given an MSDOS device name, return the device name of that device
    // (Uses DoQueryDosDevice)
    function GetDeviceName(MSDOSKernelModeDeviceName: string): ansistring;

    // Create/delete DosDevices symlink; effectivly wraps DefineDosDevice(...) with
    // additional support required for Vista changes
    function DosDeviceSymlink(
                              CreateNotDelete: boolean;
                              DeviceName: ansistring;
                              DriveLetter: ansichar;
                              Global: boolean
                             ): boolean;

    // Delete both local and global DosDevices symlinks, as possible
    function DeleteDosDeviceSymlink(DeviceName: string; DriveLetter: ansichar): boolean;

    // Connect to the specified user mode named device
    function ConnectDevice(devicename: string): THandle;
    // Disconnect to the specified device
    function DisconnectDevice(deviceHandle: THandle): boolean;

    // Convert kernel mode device name to user mode device name
    function  GetHashDeviceUserModeDeviceName(hashKernelModeDeviceName: string): string;
    // Convert kernel mode device name to user mode device name
    function  GetCypherDeviceUserModeDeviceName(cypherKernelModeDeviceName: string): string;


    function  GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Ansichar): Ansichar; override;

    function  DriverType(): string; override;

    // ---------
    // Convert critical data area to/from a string representation

    // Convert a user-space volume filename to a format the kernel mode driver
    // can understand
    function GetKernelModeVolumeFilename(userModeFilename: string): string;
    // Convert a kernel mode driver volume filename to format user-space
    // filename
    function GetUserModeVolumeFilename(kernelModeFilename: Ansistring): Ansistring;

    // Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
    // filename refers to a partition/file
    function IsPartition_KernelModeName(kernelModeFilename: string): boolean;

    // expectedLength - Set to the amount of metadata to retrieve
    function GetVolumeMetaData(driveLetter: ansichar; expectedLength: integer; var metaData: Ansistring): boolean;

    // v1 / v3 Cypher API functions
    // Note: This shouldn't be called directly - only via wrapper functions!
    function _GetCypherDriverCyphers_v1(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; override;
    function _GetCypherDriverCyphers_v3(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; override;

  public
    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(); override;
    destructor  Destroy(); override;
    function  Dismount(driveLetter: ansichar; emergency: boolean = FALSE): boolean; overload; override;
    function  Version(): cardinal; overload; override;
    function  DrivesMounted(): ansistring; overload; override;


    // -----------------------------------------------------------------------
    // TOTFEFreeOTFEBase standard API

    function  GetVolumeInfo(driveLetter: ansichar; var volumeInfo: TOTFEFreeOTFEVolumeInfo): boolean; override;

    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): boolean; override;
    function GetHashDriverHashes(hashDriver: ansistring; var hashDriverDetails: TFreeOTFEHashDriver): boolean; override;

    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): boolean; override;


    function _EncryptDecryptData(
                                encryptFlag: boolean;
                                cypherDriver: Ansistring;
                                cypherGUID: TGUID;
                                var key: TSDUBytes;
                                var IV: Ansistring;
                                var inData: Ansistring;
                                var outData: Ansistring
                               ): boolean; override;

    function EncryptDecryptSectorData(
                                encryptFlag: boolean;
                                cypherDriver: Ansistring;
                                cypherGUID: TGUID;
                                SectorID: LARGE_INTEGER;
                                SectorSize: integer;
                                var key: TSDUBytes;
                                var IV: Ansistring;
                                var inData: Ansistring;
                                var outData: Ansistring
                               ): boolean; override;

    function HashData(
                      hashDriver: Ansistring;
                      hashGUID: TGUID;
                      const data: TSDUBytes;
                      out hashOut: TSDUBytes
                     ): boolean; override;

    function MACData(
                      macAlgorithm: TFreeOTFEMACAlgorithm;
                      HashDriver: Ansistring;
                      HashGUID: TGUID;
                      CypherDriver: Ansistring;
                      CypherGUID: TGUID;
                      var key: PasswordString;
                      var data: Ansistring;
                      var MACOut: Ansistring;
                      tBits: integer = -1
                     ): boolean; override;

    function DeriveKey(
                    kdfAlgorithm: TFreeOTFEKDFAlgorithm;
                    HashDriver: Ansistring;
                    HashGUID: TGUID;
                    CypherDriver: Ansistring;
                    CypherGUID: TGUID;
                    Password: TSDUBytes;
                    Salt: TSDUBytes;
                    Iterations: integer;
                    dkLenBits: integer;  // In *bits*
                    out DK: TSDUBytes
                   ): boolean; override;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE specific functions
                   
    // ---------
    // FreeOTFE Driver handling...

    // Display control for controlling underlying drivers
    procedure ShowDriverControlDlg();

    // Install and start FreeOTFE drivers in portable mode
    function  PortableStart(driverFilenames: TStringList; showProgress: boolean): boolean;

    // Stop and uninstall any FreeOTFE drivers started in portable mode
    function  PortableStop(): boolean;

    // Identify how many drivers are currently running in portable mode
    function  DriversInPortableMode(): integer;

    function  CanUserManageDrivers(): boolean;


    // ---------
    // See function comment in body of function
{$IFDEF LINUX_DETECT}
    function  DetectLinux(
      volumeFilename: string;
      userKey: string;
      keyProcSeed: string;
      keyProcIterations: integer;
      fileOptOffset: int64;
      fileOptSize: int64;
      mountDriveLetter: char;
      mountMountAs: TFreeOTFEMountAs;
      testFilename: string
      ): string;
{$ENDIF}

  published
    property DefaultDriveLetter: ansichar read fDefaultDriveLetter write fDefaultDriveLetter default #0;
    property DefaultMountAs: TFreeOTFEMountAs read fDefaultMountAs write fDefaultMountAs default fomaFixedDisk;
  end;


  {returns an instance of the only object. call SetFreeOTFEType first}
  function GetFreeOTFE :  TOTFEFreeOTFE;


implementation

uses
  SDUi18n,
{$IFDEF LINUX_DETECT}
  DbugIntf,  // GExperts
{$ENDIF}
{$IFDEF FREEOTFE_DEBUG}
  DbugIntf,  // GExperts
{$ENDIF}
  SDUProgressDlg,
  SDUDialogs,
  SDUEndianIntegers,
  OTFEFreeOTFE_frmVolumeType,
  OTFEFreeOTFE_frmKeyEntryFreeOTFE,
  OTFEFreeOTFE_frmKeyEntryLinux,
  OTFEFreeOTFE_frmKeyEntryLUKS,
  OTFEFreeOTFE_frmHashInfo,
  OTFEFreeOTFE_frmCypherInfo,
//  OTFEFreeOTFE_frmWizardCreateVolume,
  OTFEFreeOTFE_frmSelectHashCypher,
//  OTFEFreeOTFE_frmNewVolumeSize,
  OTFEFreeOTFE_DriverControl,
  OTFEFreeOTFE_frmDriverControl,
//  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile,
  OTFEFreeOTFE_frmSelectPartition,
  OTFEFreeOTFE_frmPKCS11Session,
  OTFEFreeOTFE_frmPKCS11Management,
  Math,  // Required for min
  ActiveX,  // Required for IsEqualGUID
  ComObj,  // Required for GUIDToString
  WinSvc;  // Required for SERVICE_ACTIVE



{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

type
  EFreeOTFEReadCDBFailure = EFreeOTFEError;  // Internal error - should never
                                             // be propagated beyond this unit
  EFreeOTFEWriteCDBFailure = EFreeOTFEError;  // Internal error - should never
                                              // be propagated beyond this unit
  PHashIdentify = ^THashIdentify;
  THashIdentify = record
    KernelModeDriverName: string;
    GUID: string;
    Details: TFreeOTFEHash;
  end;

  PCypherIdentify = ^TCypherIdentify;
  TCypherIdentify = record
    KernelModeDriverName: string;
    GUID: string;
    Details: TFreeOTFECypher_v3;
  end;

const
  VERSION_ID_NOT_CACHED = VERSION_ID_FAILURE;

  // These are artifical maximums; they must be enough to store the results of
  // the DIOC calls when the amount of data output is variable
  MAX_DERIVED_KEY_LENGTH = 1024*1024;  // In *bits*
  MAX_MAC_LENGTH         = 1024*1024;  // In *bits*

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
function TOTFEFreeOTFE.Connect(): boolean;
begin
  Result := FALSE;

  DriverHandle := ConnectDevice(DEVICE_SYMLINK_MAIN_NAME);

  if (DriverHandle <> 0) then
    begin
    Result := TRUE;
    end
  else
    begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.Disconnect(): boolean;
begin
  DisconnectDevice(DriverHandle);
  Result:= TRUE;
end;


// ----------------------------------------------------------------------------
{$IFDEF LINUX_DETECT}
// This function was included to allow the detection of Linux encrypted
// volume's settings
// This function is *not* normally built, and it's inclusion is only for
// development/diagnostic purposes; not for general use
// It operates by being given the volume's password, and the filename of a file
// stored on the encrypted volume.
// This function cycles through all possible combinations of Linux settings for
// that password, attempting to mount the volume using the given password.
// It does not terminate on a successful mount, as there may be more than one
// set of settings which mount correctly
// IMPORTANT: The encrypted volume *must* be formatted with a filesystem
//            MS Windows understands (e.g. FAT)
// WARNING: This procedure can take some time to complete(!)
// WARNING: Some parts of this function may be commented out in order to
//          increase it's speed, but at a cost of not checking all combinations
// NOTE: Makes use of GExpert's (3rd party) debugging tool to pump out details
//       of successful mounts before this function returns
function TOTFEFreeOTFE.DetectLinux(
  volumeFilename: string;
  userKey: string;
  keyProcSeed: string;
  keyProcIterations: integer;
  fileOptOffset: int64;
  fileOptSize: int64;
  mountDriveLetter: char;
  mountMountAs: TFreeOTFEMountAs;
  testFilename: string
  ): string;
var
  deviceName: string;
  i: integer;
  volumeKey: string;

  keyProcHashDriver: string;
  keyProcHashGUID: TGUID;
  keyProcHashWithAs: boolean;
  keyProcCypherDriver: string;
  keyProcCypherGUID: TGUID;
  mainCypherDriver: string;
  mainCypherGUID: TGUID;
  mainIVHashDriver: string;
  mainIVHashGUID: TGUID;
  mainIVCypherDriver: string;
  mainIVCypherGUID: TGUID;
  currDriveLetter: Ansichar;
  startOfVolFile: boolean;
  startOfEndData: boolean;
  VolumeFlags: DWORD;
  mr: integer;
  mainCypherDetails: TFreeOTFECypher;

  hashDrivers: array of TFreeOTFEHashDriver;
  cypherDrivers: array of TFreeOTFECypherDriver;

  kph_i, kph_j: integer;
  kpc_i, kpc_j: integer;
  mc_i, mc_j: integer;
  hashWithAsInt: integer;
  ivh_i, ivh_j: integer;
  ivc_i, ivc_j: integer;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  ivStartSectorInt: integer;

  kph_currDriver: TFreeOTFEHashDriver;
  kph_currImpl: TFreeOTFEHash;
  kpc_currDriver: TFreeOTFECypherDriver;
  kpc_currImpl: TFreeOTFECypher;
  mc_currDriver: TFreeOTFECypherDriver;
  mc_currImpl: TFreeOTFECypher;
  ivh_currDriver: TFreeOTFEHashDriver;
  ivh_currImpl: TFreeOTFEHash;
  ivc_currDriver: TFreeOTFECypherDriver;
  ivc_currImpl: TFreeOTFECypher;

begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := '';

  CheckActive();

  // Sanity checking
  // Is the user attempting to mount Linux LUKS volumes?
  // Mount as LUKS if they are...
  if (IsLUKSVolume(volumeFilename)) then
    begin
    Result := Result + 'LUKS volume; no need to do exhaustive search';
    exit;
    end;


  if (Result = '') then
    begin
    // Obtain details of all hashes...
    SetLength(hashDrivers, 0);
    if not(GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers))) then
      begin
      Result := Result + 'Unable to get list of hash drivers.';
      end;
    end;

  if (Result = '') then
    begin
    // Obtain details of all cyphers...
    SetLength(cypherDrivers, 0);
    if not(GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers))) then
      begin
      Result := Result + 'Unable to get list of cypher drivers.';
      end;
    end;


  if (Result = '') then
    begin
    currDriveLetter := GetNextDriveLetter(mountDriveLetter, #0);
    if (currDriveLetter = #0) then
      begin
      // No more drive letters following the user's specified drive
      // letter - don't mount further drives
      Result := Result + 'Out of drive letters.';
      end;
    end;


  if (Result = '') then
    begin
    // KEY PROCESSING
    // FOR ALL HASH DRIVERS...
    SendDateTime('START', now);
    for kph_i:=low(hashDrivers) to high(hashDrivers) do
      begin
      SendDebug('-kph_i: '+inttostr(kph_i)+'/'+inttostr(high(hashDrivers) - low(hashDrivers)));
      kph_currDriver := hashDrivers[kph_i];
      // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
      for kph_j:=low(kph_currDriver.Hashes) to high(kph_currDriver.Hashes) do
        begin
        SendDebug('--kph_j: '+inttostr(kph_j)+'/'+inttostr(high(kph_currDriver.Hashes) - low(kph_currDriver.Hashes)));
        kph_currImpl := kph_currDriver.Hashes[kph_j];

        keyProcHashDriver := kph_currDriver.DeviceKernelModeName;
        keyProcHashGUID := kph_currImpl.HashGUID;

        // KEY PROCESSING
        // FOR ALL CYPHER DRIVERS...
        for kpc_i:=low(cypherDrivers) to high(cypherDrivers) do
          begin
          SendDebug('---kpc_i: '+inttostr(kpc_i)+'/'+inttostr(high(cypherDrivers) - low(cypherDrivers)));
          kpc_currDriver := cypherDrivers[kpc_i];
          // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
          for kpc_j:=low(kpc_currDriver.Cyphers) to high(kpc_currDriver.Cyphers) do
            begin
            SendDebug('----kpc_j: '+inttostr(kpc_j)+'/'+inttostr(high(kpc_currDriver.Cyphers) - low(kpc_currDriver.Cyphers)));
            kpc_currImpl := kpc_currDriver.Cyphers[kpc_j];

            keyProcCypherDriver := kpc_currDriver.DeviceKernelModeName;
            keyProcCypherGUID := kpc_currImpl.CypherGUID;

//            // MAIN CYPHER
//            // FOR ALL CYPHER DRIVERS...
//            for mc_i:=low(cypherDrivers) to high(cypherDrivers) do
//              begin
//              mc_currDriver := cypherDrivers[mc_i];
//              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
//              for mc_j:=low(mc_currDriver.Cyphers) to high(mc_currDriver.Cyphers) do
//                begin
//                mc_currImpl := kpc_currDriver.Cyphers[mc_j];

// xxx
// xxx
mc_currDriver := kpc_currDriver;
mc_currImpl := kpc_currImpl;

                mainCypherDriver := mc_currDriver.DeviceKernelModeName;
                mainCypherGUID := mc_currImpl.CypherGUID;
                mainCypherDetails := mc_currImpl;


                for hashWithAsInt:=0 to 1 do
                  begin
                  SendDebug('-----hashWithAsInt: '+inttostr(hashWithAsInt)+'/(0-1)');

                  // Generate volume key for encryption/decryption
                  if not(GenerateLinuxVolumeKey(
                                                keyProcHashDriver,
                                                keyProcHashGUID,
                                                userKey,
                                                keyProcSeed,
                                                (hashWithAsInt = 1),
                                                mainCypherDetails.KeySize,
                                                volumeKey
                                               )) then
                    begin
                    Result := Result + 'HASH FAILURE: '+keyProcHashDriver+':'+GUIDToString(keyProcHashGUID);
                    continue;
                    end;


                  // IV GENERATION
                  // FOR ALL HASH DRIVERS...
                  for ivh_i:=low(hashDrivers) to high(hashDrivers) do
                    begin
                    SendDebug('------ivh_i: '+inttostr(ivh_i)+'/'+inttostr(high(hashDrivers) - low(hashDrivers)));
                    ivh_currDriver := hashDrivers[ivh_i];
                    // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
                    for ivh_j:=low(ivh_currDriver.Hashes) to high(ivh_currDriver.Hashes) do
                      begin
                      SendDebug('-------ivh_j: '+inttostr(ivh_j)+'/'+inttostr(high(ivh_currDriver.Hashes) - low(ivh_currDriver.Hashes)));
                      ivh_currImpl := ivh_currDriver.Hashes[ivh_j];

                      mainIVHashDriver := ivh_currDriver.DeviceKernelModeName;
                      mainIVHashGUID := ivh_currImpl.HashGUID;

//                      // IV GENERATION
//                      // FOR ALL CYPHER DRIVERS...
//                      for ivc_i:=low(cypherDrivers) to high(cypherDrivers) do
//                        begin
//                        ivc_currDriver := cypherDrivers[ivc_i];
//                        // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
//                        for ivc_j:=low(ivc_currDriver.Cyphers) to high(ivc_currDriver.Cyphers) do
//                          begin
//                          ivc_currImpl := ivc_currDriver.Cyphers[ivc_j];

// xxx
ivc_currDriver := kpc_currDriver;
ivc_currImpl := kpc_currImpl;

                          mainIVCypherDriver := ivc_currDriver.DeviceKernelModeName;
                          mainIVCypherGUID := ivc_currImpl.CypherGUID;

                          // IV GENERATION
//                          for sectorIVGenMethod := low(sectorIVGenMethod) to high(sectorIVGenMethod) do
sectorIVGenMethod := foivg32BitSectorID;
                            begin
//                            SendDebug('--------sectorIVGenMethod: '+inttostr(ord(sectorIVGenMethod))+'/'+inttostr(ord(high(sectorIVGenMethod)) - loword((sectorIVGenMethod))));

//                            for ivStartSectorInt:=0 to 1 do
                              begin
ivStartSectorInt := 1;
//                              SendDebug('---------ivStartSectorInt: '+inttostr(ivStartSectorInt)+'/(0-1)');

                              startOfVolFile := (ivStartSectorInt = 0);
                              startOfEndData := (ivStartSectorInt = 1);

                              // Encode IV generation method to flags...
                              VolumeFlags := 0;
                              if (startOfVolFile) then
                                begin
                                VolumeFlags := VolumeFlags OR VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART;
                                end;


                              // Attempt to create a new device to be used
                              deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[mountMountAs]);
                              if (deviceName <> '') then
                                begin
                                // Attempt to mount the device
                                if MountDiskDevice(
                                                   deviceName,
                                                   volumeFilename,
                                                   volumeKey,
                                                   sectorIVGenMethod,
                                                   '',  // Linux volumes don't have per-volume IVs
                                                   TRUE,
                                                   mainIVHashDriver,
                                                   mainIVHashGUID,
                                                   mainIVCypherDriver,
                                                   mainIVCypherGUID,
                                                   mainCypherDriver,
                                                   mainCypherGUID,
                                                   VolumeFlags,
                                                   '',
                                                   fileOptOffset,
                                                   fileOptSize,
                                                   FreeOTFEMountAsStorageMediaType[mountMountAs]
                                                  ) then
                                  begin
                                  if DosDeviceSymlink(
                                                      TRUE,
                                                      deviceName,
                                                      currDriveLetter,
                                                      TRUE
                                                     ) then
                                    begin
                                    // Test if mounted OK
                                    if FileExists(currDriveLetter+':'+testFilename) then
                                      begin
                                      Result := Result + '---------------------------------------------------------------';
                                      Result := Result + 'OK!';
                                      Result := Result + 'mountedAs: '+currDriveLetter;
                                      Result := Result + 'kph: '+keyProcHashDriver +':'+GUIDToString(keyProcHashGUID );
                                      Result := Result + 'kpc: '+keyProcCypherDriver +':'+GUIDToString(keyProcCypherGUID );
                                      Result := Result + 'mc: '+mainCypherDriver +':'+GUIDToString(mainCypherGUID);
                                      Result := Result + 'hashWithAsInt: '+inttostr(hashWithAsInt);
                                      Result := Result + 'ivh: '+mainIVHashDriver +':'+GUIDToString(mainIVHashGUID );
                                      Result := Result + 'ivc: '+mainIVCypherDriver +':'+GUIDToString(mainIVCypherGUID );
                                      Result := Result + 'sectorIVGenMethod: '+inttostr(ord(sectorIVGenMethod));
                                      Result := Result + 'ivStartSectorInt: '+inttostr(ivStartSectorInt);
                                      Result := Result + '---------------------------------------------------------------';
SendDateTime('HIT', now);
SendDebug('Result: '+Result);
                                      end;

                                    Dismount(currDriveLetter);
                                    end
                                  else
                                    begin
                                    // Cleardown
                                    DismountDiskDevice(deviceName, TRUE);
                                    DestroyDiskDevice(deviceName);
                                    end;  // ELSE PART - if DefineDosDevice(

                                  end
                                else
                                  begin
                                  // Cleardown
                                  DestroyDiskDevice(deviceName);
                                  end;  // ELSE PART - if MountDiskDevice(

                                end;  // ELSE PART - if (deviceName <> '') then


                              end;  // for ivStartSectorInt:=0 to 1 do
                            end;  // for sectorIVGenMethod := low(sectorIVGenMethod) to high(sectorIVGenMethod) do
//                          end;  // for ivc_j:=low(ivc_currDriver.Cyphers) to high(ivc_currDriver.Cyphers) do
//                        end;  // for ivc_i:=low(cypherDrivers) to high(cypherDrivers) do
                      end;  // for ivh_j:=low(ivh_currDriver.Hashes) to high(ivh_currDriver.Hashes) do
                    end;  // for ivh_i:=low(hashDrivers) to high(hashDrivers) do
                  end;  // for hashWithAsInt:=0 to 1 do
//                end;  // for mc_j:=low(mc_currDriver.Cyphers) to high(mc_currDriver.Cyphers) do
//              end;  // for mc_i:=low(cypherDrivers) to high(cypherDrivers) do
            end;  // for kpc_j:=low(kpc_currDriver.Cyphers) to high(kpc_currDriver.Cyphers) do
          end;  // for kpc_i:=low(cypherDrivers) to high(cypherDrivers) do
        end;  // for kph_j:=low(kph_currDriver.Hashes) to high(kph_currDriver.Hashes) do
      end;  // for kph_i:=low(hashDrivers) to high(hashDrivers) do
    end;  //  if (Result = '') then

SendDateTime('FINISHED', now);


end;
{$ENDIF}


// ----------------------------------------------------------------------------
// Attempt to create a new device to be used
function TOTFEFreeOTFE.CreateDiskDevice(deviceType: DWORD): string;
var
  DIOCBufferIn: TDIOC_DISK_DEVICE_CREATE;
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

  if (DeviceIoControl(
                      DriverHandle,
                      IOCTL_FREEOTFE_CREATE,
                      @DIOCBufferIn,
                      sizeof(DIOCBufferIn),
                      @DIOCBufferOut,
                      sizeof(DIOCBufferOut),
                      bytesReturned,
                      nil
                     )) then
    begin
    Result := copy(DIOCBufferOut.DeviceName, 1, StrLen(DIOCBufferOut.DeviceName));
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Disk device created: '+Result);
{$ENDIF}
    end;



end;


// ----------------------------------------------------------------------------
// Attempt to mount on an existing device
function TOTFEFreeOTFE.MountDiskDevice(
                                   deviceName: string;
                                   volFilename: string;
                                   volumeKey: TSDUBytes;
                                   sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                                   volumeIV: TSDUBytes;
                                   readonly: boolean;
                                   IVHashDriver: Ansistring;
                                   IVHashGUID: TGUID;
                                   IVCypherDriver: Ansistring;
                                   IVCypherGUID: TGUID;
                                   mainCypherDriver: Ansistring;
                                   mainCypherGUID: TGUID;
                                   VolumeFlags: integer;
                                   metaData: TOTFEFreeOTFEVolumeMetaData;
                                   offset: int64 = 0;
                                   size: int64 = 0;
                                   storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                                  ): boolean;
var
  ptrDIOCBuffer: PDIOC_MOUNT_PC_DRIVER;
  bytesReturned: DWORD;
  bufferSize: integer;
  mainCypherDetails: TFreeOTFECypher_v3;
  kmFilename: string;
  useVolumeFlags: integer;
  strMetaData: Ansistring;
  volumeKeyStr: Ansistring;
begin
  result := FALSE;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In MountDiskDevice');
{$ENDIF}

  VolumeMetadataToString(metaData, strMetaData);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MountDiskDevice called with: ');
DebugMsg('  deviceName: '+deviceName);
DebugMsg('  filename: '+volFilename);
DebugMsg('  volumeKey: ');
DebugMsgBinary(volumeKey);
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
{$ENDIF}

  CheckActive();


  if not(GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then
    begin
//
    exit;
    end;

  // Ensure the volumeKey's length matches that of the cypher's keysize, if
  // the cypher's keysize is >= 0
  if (mainCypherDetails.KeySizeRequired >= 0) then
    begin
    // THIS IS CORRECT; caters for both volumeKey>keysize and
    // volumeKey<keysize
    // Copy as much of the hash value as possible to match the key length
    volumeKeyStr := SDUBytesToString(volumeKey);
    volumeKeyStr := Copy(volumeKeyStr, 1, min((mainCypherDetails.KeySizeRequired div 8), length(volumeKeyStr)));
    // If the hash wasn't big enough, pad out with zeros

     volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0), ((mainCypherDetails.KeySizeRequired div 8) - Length(volumeKeyStr)));
      SafeSetLength(volumeKey,mainCypherDetails.KeySizeRequired div 8);
       assert(volumeKeyStr=SDUBytesToString(volumeKey) );// passed
    end;


  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not(SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then
    begin
    IVHashDriver := '';
    IVHashGUID := StringToGUID(NULL_GUID);
    end;

  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not(SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then
    begin
    IVCypherDriver := '';
    IVCypherGUID := StringToGUID(NULL_GUID);
    end;


  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize := sizeof(ptrDIOCBuffer^)
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(volumeKey))
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(volumeIV))
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(strMetaData));
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StrPCopy(ptrDIOCBuffer.DiskDeviceName, deviceName);

    // Determine the filename as the kernel sees it
    kmFilename:= GetKernelModeVolumeFilename(volFilename);

    StrPCopy(ptrDIOCBuffer.Filename, kmFilename);
    ptrDIOCBuffer.DataStart := offset;

    ptrDIOCBuffer.DataEnd := 0;
    if (size > 0) then
      begin
      ptrDIOCBuffer.DataEnd := offset + size;
      end;

    StrPCopy(ptrDIOCBuffer.IVHashDeviceName, IVHashDriver);
    ptrDIOCBuffer.IVHashGUID := IVHashGUID;
    StrPCopy(ptrDIOCBuffer.IVCypherDeviceName, IVCypherDriver);
    ptrDIOCBuffer.IVCypherGUID := IVCypherGUID;

    StrPCopy(ptrDIOCBuffer.MainCypherDeviceName, mainCypherDriver);
    ptrDIOCBuffer.MainCypherGUID := mainCypherGUID;

    ptrDIOCBuffer.ReadOnly := readonly;
    ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsFile];
    if IsPartition_KernelModeName(kmFilename) then
      begin
      ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsPartition];
      end;

    ptrDIOCBuffer.StorageMediaType := FreeOTFEStorageMediaTypeID[storageMediaType];


    useVolumeFlags := VolumeFlags;
    // Yes, this timestamp reverting is the right way around; if the bit
    // *isn't* set, the timestamps get reverted
    if RevertVolTimestamps then
      begin
      // Strip off bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags and not(VOL_FLAGS_NORMAL_TIMESTAMPS);
      end
    else
      begin
      // Set bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags or VOL_FLAGS_NORMAL_TIMESTAMPS
      end;
    ptrDIOCBuffer.VolumeFlags := useVolumeFlags;
    ptrDIOCBuffer.SectorIVGenMethod := FreeOTFESectorIVGenMethodID[sectorIVGenMethod];

    ptrDIOCBuffer.MasterKeyLength := Length(volumeKey) * 8;
    ptrDIOCBuffer.VolumeIVLength := Length(volumeIV) * 8;
    ptrDIOCBuffer.MetaDataLength := Length(strMetaData);
    // This may seem a little weird, but we do this because the VolumeIV and metaData are
    // immediatly after the master key
    StrMove(@ptrDIOCBuffer.MasterKey, PAnsiChar(volumeKey), Length(volumeKey));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey))+length(volumeKey)), PAnsiChar(volumeIV), Length(volumeIV));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey))+length(volumeKey)+length(volumeIV)), PAnsiChar(strMetaData), Length(strMetaData));

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('+++ ABOUT TO DIOC MOUNT WITH STRUCT:');
DebugMsg('+++ struct size: '+inttostr(bufferSize));
DebugMsg('+++ ---begin struct---');
DebugMsgBinaryPtr(PAnsiChar(ptrDIOCBuffer), bufferSize);
DebugMsg('+++ ---end struct---');
{$ENDIF}
    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_MOUNT,
                        ptrDIOCBuffer,
                        bufferSize,
                        nil,
                        0,
                        bytesReturned,
                        nil
                       )) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Mounted OK!');
{$ENDIF}
      result := TRUE;
      end;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('+++ DIOC STRUCT COMPLETE');
{$ENDIF}

  finally
    FreeMem(ptrDIOCBuffer);
  end;


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting MountDiskDevice');
{$ENDIF}

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.CreateMountDiskDevice(
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: Ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: Ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: Ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              DriveLetter: Ansichar;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              offset: int64 = 0;
                              size: int64 = 0;
                              MetaData_LinuxVolume: boolean = FALSE;  // Linux volume
                              MetaData_PKCS11SlotID: integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
                              MountMountAs: TFreeOTFEMountAs = fomaFixedDisk;  // PC kernel drivers *only* - ignored otherwise
                              mountForAllUsers: boolean = TRUE  // PC kernel drivers *only* - ignored otherwise
                             ): boolean;
var
  deviceName: string;
  mountMetadata: TOTFEFreeOTFEVolumeMetaData;
begin
  result:= FALSE;

  // Attempt to create a new device to be used
  deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[MountMountAs]);
  if (deviceName <> '') then
    begin
    PopulateVolumeMetadataStruct(
                                 MetaData_LinuxVolume,
                                 MetaData_PKCS11SlotID,
                                 mountMetadata
                                );

    // Attempt to mount the device
    if MountDiskDevice(
                       deviceName,
                       volFilename,
                       volumeKey,
                       sectorIVGenMethod,
                       volumeIV,
                       readonly,
                       IVHashDriver,
                       IVHashGUID,
                       IVCypherDriver,
                       IVCypherGUID,
                       mainCypherDriver,
                       mainCypherGUID,
                       VolumeFlags,
                       mountMetadata,
                       offset,
                       size,
                       FreeOTFEMountAsStorageMediaType[MountMountAs]
                      ) then
      begin
      if DosDeviceSymlink(
                          TRUE,
                          deviceName,
                          DriveLetter,
                          mountForAllUsers
                         ) then
        begin
        result := TRUE;
        end
      else
        begin
        // Cleardown
        DismountDiskDevice(deviceName, TRUE);
        DestroyDiskDevice(deviceName);
        end;

      end
    else
      begin
      // Cleardown
      DestroyDiskDevice(deviceName);
      end;

    end; 

//
end;


// ----------------------------------------------------------------------------
// Attempt to dismount a device
function TOTFEFreeOTFE.DismountDiskDevice(deviceName: string; emergency: boolean): boolean;
var
  DIOCBuffer: TDIOC_DISMOUNT;
  bytesReturned: DWORD;
begin
  Result := FALSE;

  CheckActive();

  StrPCopy(DIOCBuffer.DiskDeviceName, deviceName);
  DIOCBuffer.Emergency := emergency;

  if (DeviceIoControl(
                      DriverHandle,
                      IOCTL_FREEOTFE_DISMOUNT,
                      @DIOCBuffer,
                      sizeof(DIOCBuffer),
                      nil,
                      0,
                      bytesReturned,
                      nil
                     )) then
    begin
    Result := TRUE;
    end;


end;


// ----------------------------------------------------------------------------
// This dismount routine is based on the MS "media eject" routine for NT/2k/XP
// at:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q165/7/21.asp&NoWebContent=1
function TOTFEFreeOTFE.LDREUUserApp(
                          driveLetter: ansichar;
                          deviceName: Ansistring;
                          emergency: boolean;
                          var unableToOpenDrive: boolean
                         ): boolean;
var
  driveDevice: THandle;
  BytesReturned: DWORD;
  driveLetterColon: string;
  driveFile: Widestring;
  pmr: TPREVENT_MEDIA_REMOVAL;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  CheckActive();

  driveLetterColon := upcase(driveLetter)+':';
  driveFile := '\\.\'+driveLetterColon;

  Result := TRUE;

  if Result then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo');
{$ENDIF}
    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if (not Result) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo NOT Result');
{$ENDIF}
      emergency := FALSE;
      end;
    end;



  // We attempt to open the drive, and then carry out various operations to
  // shut the drive down.
  // If were operating in an emergency, then we don't care if some of these
  // operations fail - it's an EMERGENCY! JUST GET RID OF THE %$!# DRIVE!


  // Initialize to flag that we haven't opened it
  driveDevice := INVALID_HANDLE_VALUE;
  if Result then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('createfile: '+driveFile);
{$ENDIF}
    driveDevice := CreateFile(
                              PChar(driveFile),
                              //GENERIC_READ or GENERIC_WRITE,
                              GENERIC_READ,
                              FILE_SHARE_READ or FILE_SHARE_WRITE,
                              nil,
                              OPEN_EXISTING,
                              FILE_FLAG_NO_BUFFERING,
                              0
                             );
    Result := (driveDevice <> INVALID_HANDLE_VALUE);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('driveDevice: '+inttostr(driveDevice)+' (INVALID_HANDLE_VALUE = '+inttostr(INVALID_HANDLE_VALUE));
{$ENDIF}
    end;
    
  unableToOpenDrive := (driveDevice = INVALID_HANDLE_VALUE);

  if ( Result or (emergency and (driveDevice<>INVALID_HANDLE_VALUE)) )then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('lockvolume');
{$ENDIF}
    Result := DeviceIoControl(
                             driveDevice,
                             FSCTL_LOCK_VOLUME,
                             nil,
                             0,
                             nil,
                             0,
                             bytesReturned,
                             nil
                            );
    end;


  if ( Result or (emergency and (driveDevice<>INVALID_HANDLE_VALUE)) )then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('dismountvolume');
{$ENDIF}
    Result := DeviceIoControl(
                             driveDevice,
                             FSCTL_DISMOUNT_VOLUME,
                             nil,
                             0,
                             nil,
                             0,
                             bytesReturned,
                             nil
                            );
    end;


  if ( Result or (emergency and (driveDevice<>INVALID_HANDLE_VALUE)) )then
    begin
    pmr.PreventMediaRemoval := FALSE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('mediaremoval');
{$ENDIF}
    Result := DeviceIoControl(
                             driveDevice,
                             IOCTL_STORAGE_MEDIA_REMOVAL,
                             @pmr,
                             sizeof(pmr),
                             nil,
                             0,
                             bytesReturned,
                             nil
                            );
    end;


  if ( Result or (emergency and (driveDevice<>INVALID_HANDLE_VALUE)) )then
    begin
    pmr.PreventMediaRemoval := FALSE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ejectmedia');
{$ENDIF}
    Result := DeviceIoControl(
                             driveDevice,
                             IOCTL_STORAGE_EJECT_MEDIA,
                             nil,
                             0,
                             nil,
                             0,
                             bytesReturned,
                             nil
                            );
    end;


  // An explicit call to IOCTL_FREEOTFE_DISMOUNT should be redundant; the
  // IOCTL_STORAGE_EJECT_MEDIA should have already done this
  if (Result or emergency) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('dismountdevice');
{$ENDIF}
    Result := DismountDiskDevice(volumeInfo.deviceName, emergency);
    end;


  if ( Result or (emergency and (driveDevice<>INVALID_HANDLE_VALUE)) )then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('unlock');
{$ENDIF}
    Result := DeviceIoControl(
                             driveDevice,
                             FSCTL_UNLOCK_VOLUME,
                             nil,
                             0,
                             nil,
                             0,
                             bytesReturned,
                             nil
                            );
    end;


  if (driveDevice<>INVALID_HANDLE_VALUE) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('closehandle');
{$ENDIF}
    // Note that we can't do:
    //   Result := Result and CloseHandle(driveDevice);"
    // here because lazy evaluation will prevent CloseHandle(...) from being
    // called if Result is FALSE - and we *want* CloseHandle(...) called
    // regardless, otherwise we end up with FreeOTFE never closing this file
    // handle, and "leaking" this handle
    if not(CloseHandle(driveDevice)) then
      begin
      Result := FALSE;
      end;
    end;




end;


// ----------------------------------------------------------------------------// ----------------------------------------------------------------------------
// Send LDREU to get the driver to lock, dismount, remove eject and unlock
// driveFile - Must be of the format "\??\Z:"
// deviceName - Must be of the formst \Device\FreeOTFE\Disks\Disk1
function TOTFEFreeOTFE.LDREUDriver(
                                   driveLetter: ansichar;
                                   deviceName: Ansistring;
                                   emergency: boolean
                                 ): boolean;
var
  DIOCBuffer: TDIOC_LDREU;
  bytesReturned: DWORD;
  kernelDriveFile: string;
begin
  Result := FALSE;

  CheckActive();

  // This functionality only present in v2.00 of the FreeOTFE driver; previous
  // versions could only carry this out within the userspace app
  if CheckVersionAtLeast(FREEOTFE_ID_v02_00_0000) then
    begin
    kernelDriveFile := '\??\'+uppercase(driveLetter)+':';

    StrPCopy(DIOCBuffer.DriveFile, kernelDriveFile);
    StrPCopy(DIOCBuffer.DiskDeviceName, deviceName);
    DIOCBuffer.Emergency := emergency;

    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_LDREU,
                        @DIOCBuffer,
                        sizeof(DIOCBuffer),
                        nil,
                        0,
                        bytesReturned,
                        nil
                       )) then
      begin
      Result := TRUE;
      end;
    end;



end;


// ----------------------------------------------------------------------------
// Attempt to destroy a device
function TOTFEFreeOTFE.DestroyDiskDevice(deviceName: Ansistring): boolean;
var
  DIOCBuffer: TDIOC_DEVICE_NAME;
  bytesReturned: DWORD;
begin
  Result := FALSE;

  CheckActive();

  StrPCopy(DIOCBuffer.DeviceName, deviceName);

  if (DeviceIoControl(
                      DriverHandle,
                      IOCTL_FREEOTFE_DESTROY,
                      @DIOCBuffer,
                      sizeof(DIOCBuffer),
                      nil,
                      0,
                      bytesReturned,
                      nil
                     )) then
    begin
    Result := TRUE;
    end;



end;


// ----------------------------------------------------------------------------
// This dismount routine is based on the MS "media eject" routine for NT/2k/XP
// at:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q165/7/21.asp&NoWebContent=1
function TOTFEFreeOTFE.Dismount(driveLetter: ansichar; emergency: boolean = FALSE): boolean;
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  unableToOpenDrive: boolean;
begin
  CheckActive();

  Result := TRUE;

  if Result then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo');
{$ENDIF}
    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if (not Result) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo NOT Result');
{$ENDIF}
      emergency := FALSE;
      end;
    end;



  // We attempt to open the drive, and then carry out various operations to
  // shut the drive down.
  // If were operating in an emergency, then we don't care if some of these
  // operations fail - it's an EMERGENCY! JUST GET RID OF THE %$!# DRIVE!

  if Result then
    begin
    unableToOpenDrive := FALSE;
    Result := LDREUUserApp(
                          driveLetter,
                          volumeInfo.deviceName,
                          emergency,
                          unableToOpenDrive
                         );
    end;


  // If we were unable to open the drive to carry out the dismount, we're
  // probably on Vista, without having escalated UAC before trying to dismount.
  // Fallback to getting the driver handle the privileged part of the
  // dismount (i.e. opening the volume) and carry out the LDREU
  if (
      not Result and
      unableToOpenDrive
     ) then
    begin
    Result := LDREUDriver(driveLetter, volumeInfo.deviceName, emergency);
    end;

  if (Result or emergency) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('remove definition');
{$ENDIF}
    Result := DeleteDosDeviceSymlink(volumeInfo.deviceName, driveLetter);
    end;


  // And finally... Destroy the device
  if (Result or emergency) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('destroy device');
{$ENDIF}
    Result := DestroyDiskDevice(volumeInfo.deviceName);
    end;




end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.Version(): cardinal;
var
  BytesReturned: DWORD;
  DIOCBuffer: TDIOC_VERSION;
begin
  Result := VERSION_ID_FAILURE;

  CheckActive();

  // If we have the version ID cached, use the cached information
  if (fCachedVersionID <> VERSION_ID_NOT_CACHED) then
    begin
    Result := fCachedVersionID;
    end
  else
    begin
    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_VERSION,
                        nil,
                        0,
                        @DIOCBuffer,
                        sizeof(DIOCBuffer),
                        BytesReturned,
                        nil
                        )) then
      begin
      if BytesReturned=sizeof(DIOCBuffer) then
        begin
        Result := DIOCBuffer.VersionID;
        fCachedVersionID := Result;
        end;

      end;

    end;




end;


// ----------------------------------------------------------------------------
// Get the number of disk devices the driver currently has
// Returns $FFFFFFFF on error
function TOTFEFreeOTFE.GetDiskDeviceCount(): cardinal;
var
  BytesReturned: DWORD;
  DIOCBuffer: TDIOC_DISK_DEVICE_COUNT;
begin
  Result := $FFFFFFFF;

  CheckActive();

  if (DeviceIoControl(
                      DriverHandle,
                      IOCTL_FREEOTFE_GET_DISK_DEVICE_COUNT,
                      nil,
                      0,
                      @DIOCBuffer,
                      sizeof(DIOCBuffer),
                      BytesReturned,
                      nil
                      )) then
    begin
    if BytesReturned=sizeof(DIOCBuffer) then
      begin
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
function TOTFEFreeOTFE.GetDiskDeviceList(deviceNames: TStringList): boolean;
var
  BytesReturned: DWORD;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
  bufferSize: cardinal;
  i: integer;
  devName: Ansistring;
  deviceCount: integer;
  ptrDIOCDeviceNameList: PDIOC_DEVICE_NAME_LIST;
begin
  Result := FALSE;

  CheckActive();

  deviceCount := GetDiskDeviceCount();


  bufferSize := sizeof(ptrDIOCDeviceNameList^) -
                FREEOTFE_MAX_FILENAME_LENGTH +
                (FREEOTFE_MAX_FILENAME_LENGTH * deviceCount);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrBuffer := allocmem(bufferSize);

  if (DeviceIoControl(
                      DriverHandle,
                      IOCTL_FREEOTFE_GET_DISK_DEVICE_LIST,
                      nil,
                      0,
                      ptrBuffer,
                      bufferSize,
                      BytesReturned,
                      nil
                     )) then
    begin
    if BytesReturned<=bufferSize then
      begin
      // In case devices were unmounted between the call to
      // GetDiskDeviceCount() and the DIOC call...
      ptrDIOCDeviceNameList := ptrBuffer;
      deviceCount := ptrDIOCDeviceNameList.DeviceCount;

      ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
                         sizeof(ptrDIOCDeviceNameList^) -
                         FREEOTFE_MAX_FILENAME_LENGTH);
      for i:=1 to deviceCount do
        begin
        devName := Copy(PAnsiChar(ptrBufferOffset), 1, StrLen(PAnsiChar(ptrBufferOffset)));

        deviceNames.Add(devName);

        // Setup for the next one...
        ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + FREEOTFE_MAX_FILENAME_LENGTH);
        end;

      Result := TRUE;
      end;

    end
  else
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('devicelist DIOC 2 FAIL');
{$ENDIF}
    end;

  FreeMem(ptrBuffer);

  


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DrivesMounted(): ansistring;
var
  i: integer;
  driveLetters: TStringList;
  deviceNames: TStringList;
begin
  Result := '';

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
function TOTFEFreeOTFE.GetMountedDevices(driveLetters: TStringList; deviceNames: TStringList): boolean;
const
  // 5 is the arbitary number of times we'll try this until we just give up on
  // it...
  MAX_ATTEMPTS = 5;
var
  i: integer;
  driveLetter: ansichar;
  driveColon: string;
  foundDriveLetter: boolean;
  attempt: integer;
  flagTryAgain: boolean;
  currDeviceName: string;
  queryResults: TStringList;
begin
  Result := FALSE;

  CheckActive();

  // flagTryAgain indicates whether the devices have changed during this
  // operation; if they have, it tries it again
  flagTryAgain := TRUE;
  // attempt counts the number of times the this loop was attempted
  attempt := 1;
  while ((flagTryAgain) and (attempt<=MAX_ATTEMPTS)) do
    begin
    flagTryAgain := FALSE;
    driveLetters.Clear();
    deviceNames.Clear();

    if not(GetDiskDeviceList(deviceNames)) then
      begin
      inc(attempt);
      flagTryAgain := TRUE;
      end;

    end;  // while

    

  if (attempt <= MAX_ATTEMPTS) then
    begin
    for i:=0 to (deviceNames.count-1) do
      begin
      currDeviceName := deviceNames[i];

      foundDriveLetter:= FALSE;

      for driveLetter:='A' to 'Z' do
        begin
        driveColon := driveLetter+':';

        queryResults:= TStringList.Create();
        try
          if (DoQueryDosDevice(driveColon, queryResults)) then
            begin
            if (queryResults.count>=1) then
              begin
              if (queryResults[0] = currDeviceName) then
                begin
                foundDriveLetter := TRUE;
                driveLetters.Add(driveLetter);
                break;
                end;
              end;
            end;


        finally
          queryResults.Free();
        end;

        end;

      if (not(foundDriveLetter)) then
        begin
        driveLetters.Add('');
        end;

      end;

    Result := TRUE;
    end;



end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetVolumeInfo(driveLetter: ansichar; var volumeInfo: TOTFEFreeOTFEVolumeInfo): boolean;
var
  deviceName: string;
  BytesReturned: DWORD;
  inBuffer: TDIOC_DEVICE_NAME;
  outBuffer: TDIOC_DISK_DEVICE_STATUS_PC_DRIVER;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := FALSE;

  CheckActive();

  driveLetter := upcase(driveLetter);

  deviceName := GetDriveDeviceName(driveLetter);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('deviceName: '+deviceName);
{$ENDIF}
  if (deviceName <> '') then
    begin
    StrPCopy(inBuffer.DeviceName, deviceName);

    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS,
                        @inBuffer,
                        sizeof(inBuffer),
                        @outBuffer,
                        sizeof(outBuffer),
                        BytesReturned,
                        nil
                        )) then
      begin
      if (BytesReturned <= sizeof(outBuffer)) then
        begin
        volumeInfo.Filename         := Copy(outBuffer.Filename, 1, StrLen(outBuffer.Filename));
        volumeInfo.DeviceName       := Copy(outBuffer.DiskDeviceName, 1, StrLen(outBuffer.DiskDeviceName));
        volumeInfo.IVHashDevice     := Copy(outBuffer.IVHashDeviceName, 1, StrLen(outBuffer.IVHashDeviceName));
        volumeInfo.IVHashGUID       := outBuffer.IVHashGUID;
        volumeInfo.IVCypherDevice   := Copy(outBuffer.IVCypherDeviceName, 1, StrLen(outBuffer.IVCypherDeviceName));
        volumeInfo.IVCypherGUID     := outBuffer.IVCypherGUID;
        volumeInfo.MainCypherDevice := Copy(outBuffer.MainCypherDeviceName, 1, StrLen(outBuffer.MainCypherDeviceName));
        volumeInfo.MainCypherGUID   := outBuffer.MainCypherGUID;
        volumeInfo.Mounted          := outBuffer.Mounted;
        volumeInfo.ReadOnly         := outBuffer.ReadOnly;
        volumeInfo.VolumeFlags      := outBuffer.VolumeFlags;

        volumeInfo.SectorIVGenMethod:= foivgUnknown;
        for tmpSectorIVGenMethod := low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do
          begin
          if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = outBuffer.SectorIVGenMethod) then
            begin
            volumeInfo.SectorIVGenMethod:= tmpSectorIVGenMethod;
            end;
          end;

        volumeInfo.DriveLetter :=AnsiChar(driveLetter);

        volumeInfo.Filename := GetUserModeVolumeFilename(volumeInfo.Filename);

        Result := GetVolumeMetaData(driveLetter, outBuffer.MetaDataLength, volumeInfo.MetaData);

        volumeInfo.MetaDataStructValid := ParseVolumeMetadata(
                                                              volumeInfo.MetaData,
                                                              volumeInfo.MetaDataStruct
                                                             );


        end;

      end;

    end;



end;

// ----------------------------------------------------------------------------
// expectedLength - Set to the amount of metadata to retrieve
function TOTFEFreeOTFE.GetVolumeMetaData(driveLetter: ansichar; expectedLength: integer; var metaData: Ansistring): boolean;
var
  deviceName: Ansistring;
  bufferSize: DWORD;
  BytesReturned: DWORD;
  inBuffer: TDIOC_DEVICE_NAME;
  ptrDIOCBuffer: PDIOC_DISK_DEVICE_METADATA;
begin
  Result := FALSE;

  CheckActive();

  driveLetter := upcase(driveLetter);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('expectedLength: '+inttostr(expectedLength));
{$ENDIF}

  deviceName := GetDriveDeviceName(driveLetter);
  if (deviceName <> '') then
    begin
    StrPCopy(inBuffer.DeviceName, deviceName);

    bufferSize := sizeof(ptrDIOCBuffer^) -
                  sizeof(Ansichar) + (sizeof(Ansichar)*expectedLength);
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
    ptrDIOCBuffer := allocmem(bufferSize);
    try
      StrPCopy(inBuffer.DeviceName, deviceName);

      if (DeviceIoControl(
                          DriverHandle,
                          IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA,
                          @inBuffer,
                          sizeof(inBuffer),
                          ptrDIOCBuffer,
                          bufferSize,
                          BytesReturned,
                          nil
                          )) then
        begin
        if (BytesReturned <= bufferSize) then
          begin
          // Set metaData so that it has enough characters which can be
          // overwritten with StrMove
          metaData := StringOfChar(Ansichar(#0), ptrDIOCBuffer.MetaDataLength);
          StrMove(PAnsiChar(metaData), @ptrDIOCBuffer.MetaData, ptrDIOCBuffer.MetaDataLength);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Metadata length: '+inttostr(ptrDIOCBuffer.MetaDataLength));
DebugMsgBinary(metaData);
{$ENDIF}

          Result := TRUE;
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
function TOTFEFreeOTFE.GetDriveDeviceName(driveLetter: ansichar): string;
var
  mountedDriveLetters: TStringList;
  deviceNames: TStringList;
  i: integer;
begin
  Result := '';

  CheckActive();

  mountedDriveLetters:= TStringList.Create();
  try
    deviceNames:= TStringList.Create();
    try
      if GetMountedDevices(mountedDriveLetters, deviceNames) then
        begin
        for i:=0 to (mountedDriveLetters.count-1) do
          begin
          if (mountedDriveLetters[i] = driveLetter) then
            begin
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
function TOTFEFreeOTFE.GetDriveDeviceLetter(deviceName: string): char;
var
  mountedDriveLetters: TStringList;
  deviceNames: TStringList;
  i: integer;
begin
  Result := #0;

  CheckActive();

  mountedDriveLetters:= TStringList.Create();
  try
    deviceNames:= TStringList.Create();
    try
      if GetMountedDevices(mountedDriveLetters, deviceNames) then
        begin
        for i:=0 to (mountedDriveLetters.count-1) do
          begin
          if (deviceNames[i] = deviceName) then
            begin
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
function TOTFEFreeOTFE.ConnectDevice(devicename: string): THandle;
var
  deviceHandle: THandle;
begin
  deviceHandle := CreateFile(
                             PChar(devicename),
                             GENERIC_READ or GENERIC_WRITE,
                             FILE_SHARE_READ or FILE_SHARE_WRITE,
                             nil,
                             OPEN_EXISTING,
                             FILE_FLAG_NO_BUFFERING,
                             0
                            );

  if (deviceHandle = INVALID_HANDLE_VALUE) then
    begin
    deviceHandle := 0;
    end;

  Result := deviceHandle;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DisconnectDevice(deviceHandle: THandle): boolean;
begin
  CloseHandle(deviceHandle);
  Result:= TRUE;
end;


// ----------------------------------------------------------------------------
// Create/delete DosDevices symlink; effectivly wraps DefineDosDevice(...) with
// additional support required for Vista changes
// createNotDelete - Set to TRUE to create, FALSE to delete
function TOTFEFreeOTFE.DosDeviceSymlink(
                                        CreateNotDelete: boolean;
                                        DeviceName: ansistring;
                                        DriveLetter: ansichar;
                                        Global: boolean
                                       ): boolean;
var
  DIOCBuffer: TDIOC_DOS_MOUNTPOINT;
  bytesReturned: DWORD;
  mountpoint: ansistring;
  DIOCCode: DWORD;
  driveLetterColon: Widestring;
begin
  Result := FALSE;

  CheckActive();

  DriveLetter := upcase(DriveLetter);

  // v2.00 of the FreeOTFE driver use DIOC call to create global/local version
  if not(CheckVersionAtLeast(FREEOTFE_ID_v02_00_0000)) then
    begin
    driveLetterColon := DriveLetter+':';

    if CreateNotDelete then
      begin
      Result := DefineDosDevice(
                                DDD_RAW_TARGET_PATH,
                                PWideChar(driveLetterColon),
                                PWideChar(deviceName)
                               );
      end
    else
      begin
      Result := DefineDosDevice(
                                DDD_REMOVE_DEFINITION,
                                PWideChar(driveLetterColon),
                                nil
                               );
      end;

    end
  else
    begin
    mountpoint := DriveLetter + ':';

    StrPCopy(DIOCBuffer.DiskDeviceName, DeviceName);
    DIOCBuffer.Global := Global;
    StrPCopy(DIOCBuffer.Mountpoint, mountpoint);

    if CreateNotDelete then
      begin
      DIOCCode := IOCTL_FREEOTFE_CREATE_DOS_MOUNTPOINT;
      end
    else
      begin
      DIOCCode := IOCTL_FREEOTFE_DELETE_DOS_MOUNTPOINT;
      end;

    if (DeviceIoControl(
                        DriverHandle,
                        DIOCCode,
                        @DIOCBuffer,
                        sizeof(DIOCBuffer),
                        nil,
                        0,
                        bytesReturned,
                        nil
                       )) then
      begin
      Result := TRUE;

      BroadcastDriveChangeMessage(CreateNotDelete, DriveLetter);
      end;
    end;



end;


// ----------------------------------------------------------------------------
// Delete both local and global DosDevices symlinks, as possible
function TOTFEFreeOTFE.DeleteDosDeviceSymlink(DeviceName: string; DriveLetter: ansichar): boolean;
begin
  Result := FALSE;

  // Note: We do ***NOT*** use
  //         Result := (
  //                    DosDeviceSymlink(..., TRUE) or
  //                    DosDeviceSymlink(..., FALSE)
  //                   );
  //       here, as lazy evaluation will prevent one or the other from being
  //       evaluated - we want *both* to always be executed!
  if DosDeviceSymlink(
                      FALSE,
                      DeviceName,
                      DriveLetter,
                      TRUE
                     ) then
    begin
    Result := TRUE;
    end;

  if DosDeviceSymlink(
                      FALSE,
                      DeviceName,
                      DriveLetter,
                      FALSE
                     ) then
    begin
    Result := TRUE;
    end;



end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DoQueryDosDevice(deviceName: Ansistring; queryResults: TStringList): boolean;
const
  buffSizeIncrement: integer = 1024;
var
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
  dev: string;
  offset: integer;
  z: integer;
  pcharDeviceName: PAnsiChar;
  bufferSize: integer;
  bufferUsed: integer;
  finished: boolean;
begin
  Result := FALSE;

  bufferUsed := 0;

  pcharDeviceName := PAnsiChar(deviceName);
  if (deviceName = '') then
    begin
    pcharDeviceName := nil;
    end;

  finished := FALSE;
  bufferSize := buffSizeIncrement;
  ptrBuffer := AllocMem(bufferSize);
  FillChar(ptrBuffer^, bufferSize, 'Z');
  try
    while (not(finished)) do
      begin
      // Cleardown any previous error
      SetLastError($FFFFFFFF);

      // Here, we dereference ptrBuffer to zero the buffer's contents
      bufferUsed := QueryDosDeviceA(pcharDeviceName, ptrBuffer, bufferSize);

      // We need this "if" statement because Windows 2000 *will* return a partial
      // buffer (set bufferUsed > 0) if the buffer is large enough to store some,
      // but not all, of the information
      // Windows XP will only return bufferUsed > 0 if *all* information could
      // be returned (it seems)
      if (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
        begin
        FreeMem(ptrBuffer);
        bufferSize := bufferSize + 1024; // Increment buffer size by 10K
        ptrBuffer := AllocMem(bufferSize);
        continue;
        end
      else
        begin
        finished := TRUE;
        end;

      end;


    if (bufferUsed > 0) then
      begin
      queryResults.Clear();

      offset := 0;
      while (offset<=bufferUsed) do
        begin
        ptrBufferOffset := PAnsiChar(ptrBuffer) + offset;

        z := StrLen(PAnsiChar(ptrBufferOffset));

        // From the help file on QueryDosDevice:
        //   "The final null-terminated string is followed by an additional NULL."
        // i.e. if z=0, then we've done the last one; quit the loop
        if (z = 0) then
          begin
          break;
          end;

        dev := Copy(PAnsiChar(ptrBufferOffset), 1, z);
        queryResults.add(dev);

        offset := offset + length(dev) + 1;
        end;

      Result := TRUE;
      end;

  finally
    FreeMem(ptrBuffer);
  end;



end;


// ----------------------------------------------------------------------------
// Given an MSDOS device name, return the device name of that device
// Returns '' on error
function TOTFEFreeOTFE.GetDeviceName(MSDOSKernelModeDeviceName: string): ansistring;
var
  deviceNames: TStringList;
  deviceNameMapping: TStringList;
  i, j: integer;
begin
  Result := '';

  deviceNames:= TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then
      begin
      for i:=0 to (deviceNames.count-1) do
        begin
        deviceNameMapping:= TStringList.Create();
        try

          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then
            begin
            for j:=0 to (deviceNameMapping.count-1) do
              begin
              if (deviceNameMapping[j] = MSDOSKernelModeDeviceName) then
                begin
                Result := deviceNames[i];
                break;
                end;
              end;
              
            // If we've found the name, exit the outer loop as well
            if (Result<>'') then
              begin
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
function TOTFEFreeOTFE.GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): boolean;
var
  deviceNames: TStringList;
  deviceNameMapping: TStringList;
  i, j: integer;
begin
  Result := TRUE;

  SetLength(cypherDrivers, 0);

  deviceNames:= TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then
      begin
      for i:=0 to (deviceNames.count-1) do
        begin
        deviceNameMapping:= TStringList.Create();
        try
          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then
            begin
            for j:=0 to (deviceNameMapping.count-1) do
              begin
              if pos(DEVICE_CYPHER_DIR_NAME, deviceNameMapping[j])>0 then
                begin
                // Get the details for the device
                SetLength(cypherDrivers, (Length(cypherDrivers)+1));
                Result := GetCypherDriverCyphers(deviceNameMapping[j], cypherDrivers[high(cypherDrivers)]);

                // If we got the details, bang on to the next device
                // Note: If we couldn't get the details, this will have the
                //       effect of allowing the loop to go round again, but
                //       this time Result will still be FALSE; if the loop
                //       eventually exits, then Result will *still* be false,
                //       signalling an error
                // Note: This "break" condition differs between the DLL and kernel
                //       driver versions
                if Result then
                  begin
                  break;
                  end;
                end;

              end;  // for j:=1 to (deviceNameMapping.count-1) do

            end;

        finally
          deviceNameMapping.Free();
        end;

        // Propogate breakout, if Result is FALSE
        if (not(Result)) then
          begin
          break;
          end;

        end;  // for i:=1 to (deviceNames.count-1) do

      end;

  finally
    deviceNames.Free();
  end;


end;


// ----------------------------------------------------------------------------
// Get cypher details - using FreeOTFE v3 and later API
// Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFE._GetCypherDriverCyphers_v1(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  cypherHandle: THandle;
  BytesReturned: DWORD;
  DIOCBuffer: TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v1;
{$ENDIF}
  bufferSize: cardinal;
  cypherDetails: PCYPHER_v1;
  i: integer;
  currCypherMode: TFreeOTFECypherMode;
  arrIdx: integer;
  deviceUserModeName: ansistring;
begin
  Result := FALSE;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then
    begin
    Result := TRUE;
    end
  else
    begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+deviceUserModeName);
{$ENDIF}
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      end
    else
      begin
      if (DeviceIoControl(
                          cypherHandle,
                          IOCTL_FREEOTFECYPHER_IDENTIFYDRIVER,
                          nil,
                          0,
                          @DIOCBuffer,
                          sizeof(DIOCBuffer),
                          BytesReturned,
                          nil
                         )) then
        begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('_1_ supplied: '+inttostr(sizeof(DIOCBuffer))+' used: '+inttostr(BytesReturned));
  {$ENDIF}
        if BytesReturned=sizeof(DIOCBuffer) then
          begin
          cypherDriverDetails.DriverGUID  := DIOCBuffer.DriverGUID;
          cypherDriverDetails.DeviceName  := GetDeviceName(cypherDriver);
          cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
          cypherDriverDetails.Title       := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          cypherDriverDetails.VersionID   := DIOCBuffer.VersionID;
          cypherDriverDetails.CypherCount := DIOCBuffer.CypherCount;


          bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) -
                        sizeof(cypherDetails^) +
                        (sizeof(cypherDetails^) * cypherDriverDetails.CypherCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer := allocmem(bufferSize);

          SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

          if (DeviceIoControl(
                              cypherHandle,
                              IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v1,
                              nil,
                              0,
                              ptrBuffer,
                              bufferSize,
                              BytesReturned,
                              nil
                             )) then
            begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('_2_ supplied: '+inttostr(bufferSize)+' used: '+inttostr(BytesReturned));
  {$ENDIF}
            if BytesReturned<=bufferSize then
              begin
  {$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v1(ptrBuffer));
  DebugMsg('cypher details count: '+inttostr(DIOCBufferCyphers.BufCount));
  {$ENDIF}
              ptrBufferOffset := Pointer(
                                         PAnsiChar(ptrBuffer) +
                                         sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) -
                                         sizeof(cypherDetails^)
                                        );
              arrIdx := low(cypherDriverDetails.Cyphers);
              for i:=1 to cypherDriverDetails.CypherCount do
                begin
                cypherDetails := (PCYPHER_v1(ptrBufferOffset));
                cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
                cypherDriverDetails.Cyphers[arrIdx].Title := Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

                // Failsafe to "unknown"
                cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
                for currCypherMode:=low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do
                  begin
                  if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then
                    begin
                    cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
                    end;

                  end;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying := cypherDetails.KeySizeUnderlying;
                cypherDriverDetails.Cyphers[arrIdx].BlockSize := cypherDetails.BlockSize;
                cypherDriverDetails.Cyphers[arrIdx].VersionID := cypherDetails.VersionID;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired := cypherDetails.KeySizeUnderlying;

                // Setup for the next one...
                ptrBufferOffset := Pointer(
                                           PAnsiChar(ptrBufferOffset) +
                                           sizeof(cypherDetails^)
                                          );
                inc(arrIdx);
                end;

              // Cache the information retrieved
              CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

              Result := TRUE;
              end;

            end
          else
            begin
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
DebugMsg('Exiting function...');
{$ENDIF}


end;

// ----------------------------------------------------------------------------
// Get cypher details - using FreeOTFE v3 and later API
// Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFE._GetCypherDriverCyphers_v3(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  cypherHandle: THandle;
  BytesReturned: DWORD;
  DIOCBuffer: TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v3;
{$ENDIF}
  bufferSize: cardinal;
  cypherDetails: PCYPHER_v3;
  i: integer;
  currCypherMode: TFreeOTFECypherMode;
  arrIdx: integer;
  deviceUserModeName: ansistring;
begin
  Result := FALSE;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then
    begin
    Result := TRUE;
    end
  else
    begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+deviceUserModeName);
{$ENDIF}
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      end
    else
      begin
      if (DeviceIoControl(
                          cypherHandle,
                          IOCTL_FREEOTFECYPHER_IDENTIFYDRIVER,
                          nil,
                          0,
                          @DIOCBuffer,
                          sizeof(DIOCBuffer),
                          BytesReturned,
                          nil
                         )) then
        begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('_1_ supplied: '+inttostr(sizeof(DIOCBuffer))+' used: '+inttostr(BytesReturned));
  {$ENDIF}
        if BytesReturned=sizeof(DIOCBuffer) then
          begin
          cypherDriverDetails.DriverGUID  := DIOCBuffer.DriverGUID;
          cypherDriverDetails.DeviceName  := GetDeviceName(cypherDriver);
          cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
          cypherDriverDetails.Title       := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          cypherDriverDetails.VersionID   := DIOCBuffer.VersionID;
          cypherDriverDetails.CypherCount := DIOCBuffer.CypherCount;


          bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
                        sizeof(cypherDetails^) +
                        (sizeof(cypherDetails^) * cypherDriverDetails.CypherCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer := allocmem(bufferSize);

          SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

          if (DeviceIoControl(
                              cypherHandle,
                              IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v3,
                              nil,
                              0,
                              ptrBuffer,
                              bufferSize,
                              BytesReturned,
                              nil
                             )) then
            begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('_2_ supplied: '+inttostr(bufferSize)+' used: '+inttostr(BytesReturned));
  {$ENDIF}
            if BytesReturned<=bufferSize then
              begin
  {$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v3(ptrBuffer));
  DebugMsg('cypher details count: '+inttostr(DIOCBufferCyphers.BufCount));
  {$ENDIF}
              ptrBufferOffset := Pointer(
                                         PAnsiChar(ptrBuffer) +
                                         sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
                                         sizeof(cypherDetails^)
                                        );
              arrIdx := low(cypherDriverDetails.Cyphers);
              for i:=1 to cypherDriverDetails.CypherCount do
                begin
                cypherDetails := (PCYPHER_v3(ptrBufferOffset));
                cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
                cypherDriverDetails.Cyphers[arrIdx].Title := Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

                // Failsafe to "unknown"
                cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
                for currCypherMode:=low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do
                  begin
                  if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then
                    begin
                    cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
                    end;

                  end;

                cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired := cypherDetails.KeySizeRequired;
                cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying := cypherDetails.KeySizeUnderlying;
                cypherDriverDetails.Cyphers[arrIdx].BlockSize := cypherDetails.BlockSize;
                cypherDriverDetails.Cyphers[arrIdx].VersionID := cypherDetails.VersionID;

                // Setup for the next one...
                ptrBufferOffset := Pointer(
                                           PAnsiChar(ptrBufferOffset) +
                                           sizeof(cypherDetails^)
                                          );
                inc(arrIdx);
                end;

              // Cache the information retrieved
              CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

              Result := TRUE;
              end;

            end
          else
            begin
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
DebugMsg('Exiting function...');
{$ENDIF}


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): boolean;
var
  deviceNames: TStringList;
  deviceNameMapping: TStringList;
  i, j: integer;
begin
  Result := TRUE;

  SetLength(hashDrivers, 0);

  deviceNames:= TStringList.Create();
  try
    if DoQueryDosDevice('', deviceNames) then
      begin
      for i:=0 to (deviceNames.count-1) do
        begin
        deviceNameMapping:= TStringList.Create();
        try
          if DoQueryDosDevice(deviceNames[i], deviceNameMapping) then
            begin
            for j:=0 to (deviceNameMapping.count-1) do
              begin
              if pos(DEVICE_HASH_DIR_NAME, deviceNameMapping[j])>0 then
                begin
                // Get the details for the device
                SetLength(hashDrivers, (Length(hashDrivers)+1));
                Result := GetHashDriverHashes(deviceNameMapping[j], hashDrivers[high(hashDrivers)]);

                // If we got the details, bang on to the next device
                // Note: If we couldn't get the details, this will have the
                //       effect of allowing the loop to go round again, but
                //       this time Result will still be FALSE; if the loop
                //       eventually exits, then Result will *still* be false,
                //       signalling an error
                // Note: This "break" condition differs between the DLL and kernel
                //       driver versions
                if Result then
                  begin
                  break;
                  end;
                end;

              end;  // for j:=1 to (deviceNameMapping.count-1) do

            end;

        finally
          deviceNameMapping.Free();
        end;

        // Propogate breakout, if Result is FALSE
        if (not(Result)) then
          begin
          break;
          end;

        end;  // for i:=1 to (deviceNames.count-1) do

      end;

  finally
    deviceNames.Free();
  end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.GetHashDriverHashes(hashDriver: ansistring; var hashDriverDetails: TFreeOTFEHashDriver): boolean;
var
  hashHandle: THandle;
  BytesReturned: DWORD;
  DIOCBuffer: TDIOC_HASH_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferHashes: PDIOC_HASH_IDENTIFYSUPPORTED;
{$ENDIF}
  bufferSize: cardinal;
  hashDetails: PHASH;
  i: integer;
  arrIdx: integer;
  deviceUserModeName: ansistring;
begin
  Result := FALSE;

  if CachesGetHashDriver(hashDriver, hashDriverDetails) then
    begin
    Result := TRUE;
    end
  else
    begin
    // Determine the user mode MSDOS device name from the kernel mode device name
    deviceUserModeName := GetHashDeviceUserModeDeviceName(hashDriver);


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+deviceUserModeName);
{$ENDIF}
    hashHandle := ConnectDevice(deviceUserModeName);
    if (hashHandle = 0) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      end
    else
      begin

      if (DeviceIoControl(
                          hashHandle,
                          IOCTL_FREEOTFEHASH_IDENTIFYDRIVER,
                          nil,
                          0,
                          @DIOCBuffer,
                          sizeof(DIOCBuffer),
                          BytesReturned,
                          nil
                         )) then
        begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_3_ supplied: '+inttostr(sizeof(DIOCBuffer))+' used: '+inttostr(BytesReturned));
{$ENDIF}
        if BytesReturned=sizeof(DIOCBuffer) then
          begin
          hashDriverDetails.DriverGUID := DIOCBuffer.DriverGUID;
          hashDriverDetails.DeviceName := GetDeviceName(hashDriver);
          hashDriverDetails.LibFNOrDevKnlMdeName := hashDriver;
          hashDriverDetails.DeviceUserModeName := DeviceUserModeName;
          hashDriverDetails.Title := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
          hashDriverDetails.VersionID := DIOCBuffer.VersionID;
          hashDriverDetails.HashCount := DIOCBuffer.HashCount;


          bufferSize := sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
                        sizeof(hashDetails^) + (sizeof(hashDetails^) * hashDriverDetails.HashCount);
          // Round up to next "DIOC boundry". Although this always results in an
          // oversized buffer, it's guaranteed to be big enough.
          bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
          ptrBuffer := allocmem(bufferSize);

          SetLength(hashDriverDetails.Hashes, hashDriverDetails.HashCount);

          if (DeviceIoControl(
                              hashHandle,
                              IOCTL_FREEOTFEHASH_IDENTIFYSUPPORTED,
                              nil,
                              0,
                              ptrBuffer,
                              bufferSize,
                              BytesReturned,
                              nil
                             )) then
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_4_ supplied: '+inttostr(bufferSize)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned<=bufferSize) then
              begin
{$IFDEF FREEOTFE_DEBUG}
DIOCBufferHashes := (PDIOC_HASH_IDENTIFYSUPPORTED(ptrBuffer));
DebugMsg('hash details count: '+inttostr(DIOCBufferHashes.BufCount));
{$ENDIF}
              ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
                                 sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
                                 sizeof(hashDetails^));
              arrIdx := low(hashDriverDetails.Hashes);
              for i:=1 to hashDriverDetails.HashCount do
                begin
                hashDetails := (PHASH(ptrBufferOffset));
                hashDriverDetails.Hashes[arrIdx].HashGUID  := hashDetails.HashGUID;
                hashDriverDetails.Hashes[arrIdx].Title     := Copy(hashDetails.Title, 1, StrLen(hashDetails.Title));
                hashDriverDetails.Hashes[arrIdx].VersionID := hashDetails.VersionID;
                hashDriverDetails.Hashes[arrIdx].Length    := hashDetails.Length;
                hashDriverDetails.Hashes[arrIdx].BlockSize := hashDetails.BlockSize;

                // Setup for the next one...
                ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + sizeof(hashDetails^));
                inc(arrIdx);
                end;

              // Cache the information retrieved
              CachesAddHashDriver(hashDriver, hashDriverDetails);
              
              Result := TRUE;
              end;

            end
          else
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('gethashdrivers DIOC 2 FAIL');
{$ENDIF}
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
function TOTFEFreeOTFE.GetHashDeviceUserModeDeviceName(hashKernelModeDeviceName: string): string;
var
  deviceUserModeName: string;
begin
  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := hashKernelModeDeviceName;
  // Lose the prefix
  delete(deviceUserModeName, 1, length(DEVICE_HASH_DIR_NAME));
  // Trim off the leading "\"
  delete(deviceUserModeName, 1, 1);
  // Tack on the dosdevice root
  deviceUserModeName := DEVICE_HASH_SYMLINK_PREFIX + deviceUserModeName;

  Result := deviceUserModeName;
end;


// ----------------------------------------------------------------------------
// Convert kernel mode device name to user mode device name
function TOTFEFreeOTFE.GetCypherDeviceUserModeDeviceName(cypherKernelModeDeviceName: string): string;
var
  deviceUserModeName: string;
begin
  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := cypherKernelModeDeviceName;
  // Lose the prefix
  delete(deviceUserModeName, 1, length(DEVICE_CYPHER_DIR_NAME));
  // Trim off the leading "\"
  delete(deviceUserModeName, 1, 1);
  // Tack on the dosdevice root
  deviceUserModeName := DEVICE_CYPHER_SYMLINK_PREFIX + deviceUserModeName;

  Result := deviceUserModeName;
end;


// ----------------------------------------------------------------------------
// Hash the specified data with the supplied hash device/hash GUID
function TOTFEFreeOTFE.HashData(
                      hashDriver: ansistring;
                      hashGUID: TGUID;
                      const data: TSDUBytes;
                      out hashOut: TSDUBytes
                     ): boolean;
var
  hashHandle: THandle;
  BytesReturned: DWORD;
  ptrDIOCBufferIn: PDIOC_HASH_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_HASH_DATA_OUT;
  bufferSizeOut: DWORD;
  hashDetails: TFreeOTFEHash;
  deviceUserModeName: string;
  hashByteCount: integer;
  expectedHashSizeBits: integer;  // In *bits*
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetHashDeviceUserModeDeviceName(hashDriver);

  if GetSpecificHashDetails(hashDriver, hashGUID, hashDetails) then
    begin
    hashHandle := ConnectDevice(deviceUserModeName);
    if (hashHandle = 0) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      end
    else
      begin
      bufferSizeIn := sizeof(ptrDIOCBufferIn^) - sizeof(byte) + Length(Data);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        expectedHashSizeBits := hashDetails.Length;
        if (hashDetails.Length = -1) then
          begin
          expectedHashSizeBits := (Length(Data) * 8);
          end;
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(byte) + (expectedHashSizeBits div 8);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.HashGUID := hashGUID;

          ptrDIOCBufferIn.DataLength := Length(Data) * 8;
          StrMove(@ptrDIOCBufferIn.Data, PAnsiChar(Data), Length(Data));

          if (DeviceIoControl(
                              hashHandle,
                              IOCTL_FREEOTFEHASH_HASHDATA,
                              ptrDIOCBufferIn,
                              bufferSizeIn,
                              ptrDIOCBufferOut,
                              bufferSizeOut,
                              BytesReturned,
                              nil
                             )) then
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_5_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then
              begin
              hashByteCount := (ptrDIOCBufferOut.HashLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hashByteCount: '+inttostr(hashByteCount));
{$ENDIF}

              // Set hashOut so that it has enough characters which can be
              // overwritten with StrMove
               SDUInitAndZeroBuffer(hashByteCount,hashOut);
//              hashOut := StringOfChar(#0, hashByteCount);
              StrMove(PAnsiChar(hashOut), @ptrDIOCBufferOut.Hash, hashByteCount);

              Result := TRUE;
              end
            else
              begin
              LastErrorCode := OTFE_ERR_HASH_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
              end;

            end
          else
            begin
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

    end
  else
    begin
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
                      tBits: integer = -1
                     ): boolean;
var
  BytesReturned: DWORD;
  ptrDIOCBufferIn: PDIOC_GENERATE_MAC_IN_PC_DRIVER;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_GENERATE_MAC_OUT;
  bufferSizeOut: DWORD;
  outputByteCount: integer;
  tmpSizeBytes: integer;
  minBufferSizeOut: integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                  sizeof(ptrDIOCBufferIn^.Key) +
                  Length(key) -
                  sizeof(ptrDIOCBufferIn^.Data) +
                  Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full DK length depends on
    // the algorithm, hash, etc
    tmpSizeBytes := (max(MAX_MAC_LENGTH, tBits) div 8);  // Convert to bytes


    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.MAC);
    bufferSizeOut := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.MACAlgorithm := FreeOTFEMACID[macAlgorithm];

      StrPCopy(ptrDIOCBufferIn.HashDeviceName, HashDriver);
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StrPCopy(ptrDIOCBufferIn.CypherDeviceName, CypherDriver);
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.LengthWanted := tBits;

      ptrDIOCBufferIn.KeyLength := Length(key) * 8;
      ptrDIOCBufferIn.DataLength := Length(data) * 8;
      // This may seem a little weird, but we do this because the data is
      // immediatly after the key
      StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key), Length(key));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)), PAnsiChar(data), Length(data));


      if (DeviceIoControl(
                          DriverHandle,
                          IOCTL_FREEOTFE_GENERATE_MAC,
                          ptrDIOCBufferIn,
                          bufferSizeIn,
                          ptrDIOCBufferOut,
                          bufferSizeOut,
                          BytesReturned,
                          nil
                         )) then
        begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC out buffer supplied: '+inttostr(bufferSizeOut)+'; DIOC call used: '+inttostr(BytesReturned)+'; min: '+inttostr(minBufferSizeOut));
{$ENDIF}
        if (
            (BytesReturned >= DWORD(minBufferSizeOut)) AND
            (BytesReturned <= DWORD(bufferSizeOut))
           ) then
          begin
          outputByteCount := (ptrDIOCBufferOut.MACLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

          // Set MACOut so that it has enough characters which can be
          // overwritten with StrMove
          MACOut := StringOfChar(#0, outputByteCount);
          StrMove(PAnsiChar(MACOut), @ptrDIOCBufferOut.MAC, outputByteCount);

          Result := TRUE;
          end
        else
          begin
          LastErrorCode := OTFE_ERR_MAC_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
          end;

        end
      else
        begin
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
                    Salt: TSDUBytes;
                    Iterations: integer;
                    dkLenBits: integer;  // In *bits*
                    out DK: TSDUBytes
                   ): boolean;
var
  BytesReturned: DWORD;
  ptrDIOCBufferIn: PDIOC_DERIVE_KEY_IN_PC_DRIVER;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_DERIVE_KEY_OUT;
  bufferSizeOut: DWORD;
  outputByteCount: integer;
  tmpSizeBytes: integer;
  minBufferSizeOut: integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();


  bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                  sizeof(ptrDIOCBufferIn^.Password) +
                  Length(Password) -
                  sizeof(ptrDIOCBufferIn^.Salt) +
                  Length(Salt);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    tmpSizeBytes := (max(MAX_DERIVED_KEY_LENGTH, dkLenBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.DerivedKey);
    bufferSizeOut := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
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
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Password))+length(Password)), PAnsiChar(Salt), Length(Salt));


      if (DeviceIoControl(
                          DriverHandle,
                          IOCTL_FREEOTFE_DERIVE_KEY,
                          ptrDIOCBufferIn,
                          bufferSizeIn,
                          ptrDIOCBufferOut,
                          bufferSizeOut,
                          BytesReturned,
                          nil
                         )) then
        begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC out buffer supplied: '+inttostr(bufferSizeOut)+'; DIOC call used: '+inttostr(BytesReturned)+'; min: '+inttostr(minBufferSizeOut));
{$ENDIF}
        if (
            (BytesReturned >= DWORD(minBufferSizeOut)) AND
            (BytesReturned <= DWORD(bufferSizeOut))
           ) then
          begin
          outputByteCount := (ptrDIOCBufferOut.DerivedKeyLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

          // Set DK so that it has enough characters which can be
          // overwritten with StrMove
           SDUInitAndZeroBuffer(outputByteCount,DK);
//          DK := StringOfChar(#0, outputByteCount);
          StrMove(PAnsiChar(DK), @ptrDIOCBufferOut.DerivedKey, outputByteCount);

          Result := TRUE;
          end
        else
          begin
          LastErrorCode := OTFE_ERR_KDF_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
          end;

        end
      else
        begin
        LastErrorCode := OTFE_ERR_KDF_FAILURE;
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
// encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFE._EncryptDecryptData(
                                   encryptFlag: boolean;
                                   cypherDriver: Ansistring;
                                   cypherGUID: TGUID;
                                   var key: TSDUBytes;
                                   var IV: Ansistring;
                                   var inData: Ansistring;
                                   var outData: Ansistring
                                  ): boolean;
var
  cypherHandle: THandle;
  BytesReturned: DWORD;
  ptrDIOCBufferIn: PDIOC_CYPHER_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut: DWORD;
  cypherDetails: TFreeOTFECypher_v3;
  deviceUserModeName: string;
  dwIoControlCode: DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then
    begin
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      end
    else
      begin
      dwIoControlCode := IOCTL_FREEOTFECYPHER_DECRYPT;
      if encryptFlag then
        begin
        dwIoControlCode := IOCTL_FREEOTFECYPHER_ENCRYPT;
        end;


      bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                      sizeof(ptrDIOCBufferIn^.Key) +
                      Length(key) -
                      sizeof(ptrDIOCBufferIn^.IV) +
                      Length(IV) -
                      sizeof(ptrDIOCBufferIn^.Data) +
                      Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) + Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key,                                   PAnsiChar(key),    Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)),            PAnsiChar(IV),     Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)+length(IV)), PAnsiChar(inData), Length(inData));

          if (DeviceIoControl(
                              cypherHandle,
                              dwIoControlCode,
                              ptrDIOCBufferIn,
                              bufferSizeIn,
                              ptrDIOCBufferOut,
                              bufferSizeOut,
                              BytesReturned,
                              nil
                             )) then
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_6_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then
              begin
              // Set outData so that it has enough characters which can be
              // overwritten with StrMove
              outData := StringOfChar(#0, BytesReturned);

              StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, BytesReturned);

              Result := TRUE;
              end
            else
              begin
              LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
              end;

            end
          else
            begin
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


    end
  else
    begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
    end;



end;


// ----------------------------------------------------------------------------
// encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFE.EncryptDecryptSectorData(
                                   encryptFlag: boolean;
                                   cypherDriver: Ansistring;
                                   cypherGUID: TGUID;
                                   SectorID: LARGE_INTEGER;
                                   SectorSize: integer;
                                   var key: TSDUBytes;
                                   var IV: Ansistring;
                                   var inData: Ansistring;
                                   var outData: Ansistring
                                  ): boolean;
var
  cypherHandle: THandle;
  BytesReturned: DWORD;
  ptrDIOCBufferIn: PDIOC_CYPHER_SECTOR_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut: DWORD;
  cypherDetails: TFreeOTFECypher_v3;
  deviceUserModeName: string;
  dwIoControlCode: DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  // Determine the user mode MSDOS device name from the kernel mode device name
  deviceUserModeName := GetCypherDeviceUserModeDeviceName(cypherDriver);

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then
    begin
    cypherHandle := ConnectDevice(deviceUserModeName);
    if (cypherHandle = 0) then
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+deviceUserModeName);
{$ENDIF}
      end
    else
      begin
      dwIoControlCode := IOCTL_FREEOTFECYPHER_DECRYPTSECTOR;
      if encryptFlag then
        begin
        dwIoControlCode := IOCTL_FREEOTFECYPHER_ENCRYPTSECTOR;
        end;


      bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                      sizeof(ptrDIOCBufferIn^.Key) +
                      Length(key) -
                      sizeof(ptrDIOCBufferIn^.IV) +
                      Length(IV) -
                      sizeof(ptrDIOCBufferIn^.Data) +
                      Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) + Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.SectorID   := SectorID;
          ptrDIOCBufferIn.SectorSize := SectorSize;
          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key,                                   PAnsiChar(key),    Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)),            PAnsiChar(IV),     Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)+length(IV)), PAnsiChar(inData), Length(inData));

          if (DeviceIoControl(
                              cypherHandle,
                              dwIoControlCode,
                              ptrDIOCBufferIn,
                              bufferSizeIn,
                              ptrDIOCBufferOut,
                              bufferSizeOut,
                              BytesReturned,
                              nil
                             )) then
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('_6_ supplied: '+inttostr(bufferSizeOut)+' used: '+inttostr(BytesReturned));
{$ENDIF}
            if (BytesReturned <= bufferSizeOut) then
              begin
              // Set outData so that it has enough characters which can be
              // overwritten with StrMove
              outData := StringOfChar(#0, BytesReturned);

              StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, BytesReturned);

              Result := TRUE;
              end
            else
              begin
              LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('incorrect bytecount returned');
{$ENDIF}
              end;

            end
          else
            begin
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
      if not(Result) then
        begin
        Result := _EncryptDecryptData(
                                encryptFlag,
                                cypherDriver,
                                cypherGUID,
                                key,
                                IV,
                                inData,
                                outData
                               );

        end;


    end
  else
    begin
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
function TOTFEFreeOTFE.GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Ansichar): Ansichar;
var
  freeDriveLetters: Ansistring;
  searchDriveLetter: Ansichar;
begin
  Result:= #0;

  searchDriveLetter := userDriveLetter;
  if (searchDriveLetter = #0) then
    begin
    searchDriveLetter := requiredDriveLetter;
    end;

  // If still #0, just get the next one after C:
  if (searchDriveLetter = #0) then
    begin
    searchDriveLetter := 'C';
    end;


  freeDriveLetters := uppercase(SDUGetUnusedDriveLetters());
  searchDriveLetter := upcase(searchDriveLetter);

  // Delete drive letters from the free drive letters, until we hit one which
  // appears after the one we've been requested - or we run out of free drive
  // letters
  while (freeDriveLetters <> '') and (freeDriveLetters[1]<searchDriveLetter) do
    begin
    Delete(freeDriveLetters, 1, 1);
    end;

  if (freeDriveLetters <> '') then
    begin
    Result := freeDriveLetters[1];
    end;



end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.DriverType(): string;
begin
  Result := _('Kernel driver');
end;


// ----------------------------------------------------------------------------
// Display control for controlling underlying drivers
// You *shouldn't* call this while the component is active, as the user may
// try to do something dumn like uninstall the main FreeOTFE driver while we're
// connected to it!
procedure TOTFEFreeOTFE.ShowDriverControlDlg();
var
  dlg: TfrmDriverControl;
begin
  // TfrmDriverControl.Create(nil) will raise an exception if the user doens't
  // have the required privs to access driver control
  dlg:= TfrmDriverControl.Create(nil);
  try
    dlg.ShowModal();
  finally
    dlg.Free();
  end;

  // In case drivers were added/removed...
  CachesFlush();
end;


// ----------------------------------------------------------------------------
//PortableDriverFilenames
function TOTFEFreeOTFE.PortableStart(driverFilenames: TStringList; showProgress: boolean): boolean; 
var
  DriverControlObj: TOTFEFreeOTFEDriverControl;
begin
  DriverControlObj := TOTFEFreeOTFEDriverControl.Create();
  try
    Result := DriverControlObj.InstallMultipleDrivers(
                                                      driverFilenames,
                                                      TRUE,
                                                      showProgress,
                                                      TRUE
                                                     );
  finally
    DriverControlObj.Free();
  end;

  // In case drivers were added/removed...
  CachesFlush();


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.PortableStop(): boolean;
var
  DriverControlObj: TOTFEFreeOTFEDriverControl;
begin
  DriverControlObj := TOTFEFreeOTFEDriverControl.Create();
  try
    Result := DriverControlObj.UninstallAllDrivers(TRUE)
  finally
    DriverControlObj.Free();
  end;

  // In case drivers were added/removed...
  CachesFlush();


end;


// -----------------------------------------------------------------------------
// Identify how many drivers are currently running in portable mode
// Returns -1 on failure
function TOTFEFreeOTFE.DriversInPortableMode(): integer;
var
  DriverControlObj: TOTFEFreeOTFEDriverControl;
  allDrivers: TStringList;
  wasInstalledPortable: boolean;
  i: integer;
begin
  Result := -1;

  try
    DriverControlObj := TOTFEFreeOTFEDriverControl.Create();
    try
      allDrivers:= TStringList.Create();
      try
        if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then
          begin
          Result := 0;
          for i:=0 to (allDrivers.count-1) do
            begin
            // If the driver was installed in portable mode, stop and uninstall it
            if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then
              begin
              if wasInstalledPortable then
                begin
                inc(Result);
                end;

              end;  // if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then

            end;  // for i:=0 to (allDrivers.count-1) do

          end;  // if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then

      finally
        allDrivers.Free();
      end;

    finally
      DriverControlObj.Free();
    end;

  except
    on EFreeOTFENeedAdminPrivs do
      begin
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
  if Pos('\??\UNC\', Result) = 1 then
    begin
    // \\server\share\path\filedisk.img
    delete(Result, 1, length('\??\UNC\'));
    Result := '\\' + Result;
    end
  else if Pos('\??\', Result) = 1 then
    begin
    // C:\path\filedisk.img
    delete(Result, 1, length('\??\'));
    end
  else
    begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    // Do nothing.
    end;


end;


// ----------------------------------------------------------------------------
// Convert a user mode volume filename to a kernel mode volume filename
function TOTFEFreeOTFE.GetKernelModeVolumeFilename(userModeFilename: string): string;
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


  if (Pos('\\', userModeFilename) = 1) then
    begin
    // \\server\share\path\filedisk.img
    // Remove first "\"
    Delete(userModeFilename, 1, 1);
    Result := '\??\UNC'+userModeFilename;
    end
  else if (
           (Pos(':\', userModeFilename) = 2) OR
           (Pos(':/', userModeFilename) = 2)
          ) then
    begin
    // C:\path\filedisk.img
    Result := '\??\'+userModeFilename;
    end
  else
    begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    Result := userModeFilename;
    end;


end;


// ----------------------------------------------------------------------------
// Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
// filename refers to a partition/file
function TOTFEFreeOTFE.IsPartition_KernelModeName(kernelModeFilename: string): boolean;
begin
  // In kernel mode, partitions start with just "\", not "\??\"
  Result := (Pos('\??\', kernelModeFilename) <> 1);

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.ReadRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: Ansistring
                        ): boolean;
var
  BytesReturned: DWORD;
  kmFilename: string;
  DIOCBufferIn: TDIOC_GET_RAW_DATA_IN;
  ptrDIOCBufferOut: PDIOC_GET_RAW_DATA_OUT;
begin
  Result := FALSE;

  CheckActive();

  // +1 because StrPCopy(...) copies a terminating NULL
  ptrDIOCBufferOut := allocmem(dataLength + 1);
  try
    // Determine the filename as the kernel sees it
    kmFilename:= GetKernelModeVolumeFilename(filename);

    StrPCopy(DIOCBufferIn.Filename, kmFilename);
    DIOCBufferIn.Offset := offsetWithinFile;
    DIOCBufferIn.DataLength := dataLength;

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Read raw data from: '+filename);
  {$ENDIF}
    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_GET_RAW,
                        @DIOCBufferIn,
                        sizeof(DIOCBufferIn),
                        ptrDIOCBufferOut,
                        dataLength,
                        BytesReturned,
                        nil
                        )) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('DIOC OK (bytes: '+inttostr(BytesReturned)+')');
  {$ENDIF}
      if (BytesReturned = dataLength) then
        begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Bytes OK');
  {$ENDIF}
        // Set data so that it has enough characters which can be
        // overwritten with StrMove
        data := StringOfChar(AnsiChar(#0), dataLength);
        StrMove(PAnsiChar(data), @ptrDIOCBufferOut.Data, dataLength);

        Result := TRUE;
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
                         filename: string;
                         offsetWithinFile: int64;
                         data: Ansistring
                        ): boolean;
var
  BytesReturned: DWORD;
  kmFilename: string;
  ptrDIOCBuffer: PDIOC_SET_RAW_DATA;
  bufferSize: integer;
begin
  Result := FALSE;

  CheckActive();

  // Determine the volume filename as the kernel sees it
  kmFilename:= GetKernelModeVolumeFilename(filename);

  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize := sizeof(ptrDIOCBuffer^) -
                sizeof(ptrDIOCBuffer^.Data) + Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StrPCopy(ptrDIOCBuffer.Filename, kmFilename);
    ptrDIOCBuffer.Offset := offsetWithinFile;
    ptrDIOCBuffer.DataLength := Length(data);

    StrMove(@ptrDIOCBuffer.Data, PAnsiChar(data), Length(data));

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Write raw data from: '+filename);
  {$ENDIF}
    if (DeviceIoControl(
                        DriverHandle,
                        IOCTL_FREEOTFE_SET_RAW,
                        ptrDIOCBuffer,
                        bufferSize,
                        nil,
                        0,
                        BytesReturned,
                        nil
                        )) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Written OK');
  {$ENDIF}
      Result := TRUE;
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
  readNotWrite: boolean;

  volFilename: string;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: integer;
  mountMountAs: TFreeOTFEMountAs;

  dataOffset: int64;  // Offset from within mounted volume from where to read/write data
  dataLength: integer;  // Length of data to read/write. In bytes
  var data: ansistring;  // Data to read/write

  offset: int64 = 0;  // Offset within volume where encrypted data starts
  size: int64 = 0;  // Size of volume
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
): boolean;
var
  deviceName: string;
  dummyMetadata: TOTFEFreeOTFEVolumeMetaData;
  // emptyIV:TSDUBytes;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;

  CheckActive();

  // Attempt to create a new device to be used
  deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[mountMountAs]);
  Result := (deviceName <> '');
  if (Result) then
    begin
    try
      PopulateVolumeMetadataStruct(
                                   FALSE,  // Linux volume
                                   PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
                                   dummyMetadata
                                  );
       // SDUInitAndZeroBuffer(0,emptyIV);
      // Attempt to mount the device
      Result := MountDiskDevice(
                         deviceName,
                         volFilename,
                         volumeKey,
                         sectorIVGenMethod,
                         nil,
                         FALSE,
                         IVHashDriver,
                         IVHashGUID,
                         IVCypherDriver,  // IV cypher
                         IVCypherGUID,  // IV cypher
                         mainCypherDriver,  // Main cypher
                         mainCypherGUID,  // Main cypher
                         VolumeFlags,
                         dummyMetadata,
                         offset,
                         size,
                         FreeOTFEMountAsStorageMediaType[mountMountAs]
                        );
      if (Result) then
        begin
        try
          // Read decrypted data from mounted device
          if (readNotWrite) then
            begin
            Result := ReadRawVolumeData(
                                      deviceName,
                                      dataOffset,
                                      dataLength,
                                      data
                                     );
            end
          else
            begin
            Result := WriteRawVolumeData(
                                      deviceName,
                                      dataOffset,
                                      data
                                     );
            end;

        finally
          // Unmount
          DismountDiskDevice(deviceName, TRUE);
        end;

        end;  // if (Result) then

    finally
      // Destroy device
      DestroyDiskDevice(deviceName);
    end;

    end;  // if (Result) then


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFE.CanUserManageDrivers(): boolean;
var
  testControlObj: TOTFEFreeOTFEDriverControl;
begin
  Result := FALSE;

  try
    // If we can't create a drive control object (i.e. we don't have admin
    // access to open the SCManager), we'll get an exception here.
    testControlObj:= TOTFEFreeOTFEDriverControl.Create();
    testControlObj.Free();
    Result := TRUE;
  except
    on EFreeOTFENeedAdminPrivs do
      begin
      // Do nothing - just swallow the exception
      end;

  end;


end;



{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFE :  TOTFEFreeOTFE;
begin
  assert (GetFreeOTFEBase is TOTFEFreeOTFE,'call SetFreeOTFEType with correct type');
  result :=GetFreeOTFEBase as TOTFEFreeOTFE;
end;


END.


