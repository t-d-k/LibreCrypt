unit OTFEFreeOTFEBase_U;
// Description: Delphi FreeOTFE Component
// Description:
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
  dialogs,
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


const
  MIN_PDA_VOLUME_SIZE = 2;
  BYTES_IN_MEGABYTE = 1024 * 1024;

  VERSION_ID_NOT_CACHED = VERSION_ID_FAILURE;

  // These are artifical maximums; they must be enough to store the results of
  // the DIOC calls when the amount of data output is variable
  MAX_DERIVED_KEY_LENGTH = 1024*1024;  // In *bits*
  MAX_MAC_LENGTH         = 1024*1024;  // In *bits*


  // Fairly arbitary number of CDROMs/HDDs/Partitions per HDD to check for
  // This is only used in the *old* partition selection dialog; it refers to
  // the partition *number* - unlike the drive layout struct, which is the
  // number of partition table records (which each indicate a partition number)
  MAX_CDROMS     = 8;
  MAX_HDDS       = 16;
  MAX_PARTITIONS = 16;

  VOL_FILE_EXTN                        = 'vol';

  // This const is used in rounding up DIOC buffers to the nearest DWORD
  DIOC_BOUNDRY = 4;

  FREEOTFE_URL = 'http://www.FreeOTFE.org/';

  LINUX_KEYFILE_DEFAULT_IS_ASCII = FALSE;
  LINUX_KEYFILE_DEFAULT_NEWLINE = nlLF;



resourcestring
  // Open/Save file filters...
  FILE_FILTER_FLT_VOLUMES              = 'Volume files (*.vol)|*.vol|All files|*.*';
  FILE_FILTER_DFLT_VOLUMES             = 'vol';

  FILE_FILTER_FLT_KEYFILES             = 'Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_KEYFILES            = 'cdb';

  FILE_FILTER_FLT_VOLUMESANDKEYFILES   = 'Volume files and keyfiles (*.vol;*.cdb)|*.vol;*.cdb|Volume files (*.vol)|*.vol|Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_VOLUMESANDKEYFILES  = '';

  FILE_FILTER_FLT_TEXTFILES            = 'Text files (*.txt)|*.txt|All files|*.*';
  FILE_FILTER_DFLT_TEXTFILES           = 'txt';

  FILE_FILTER_FLT_CDBBACKUPS           = 'CDB backups (*.cdbBackup)|*.cdbBackup|All files|*.*';
  FILE_FILTER_DFLT_CDBBACKUPS          = 'cdbBackup';

  FILE_FILTER_FLT_EXECUTABLES          = 'Executable files (*.exe)|*.exe|All files|*.*';
  FILE_FILTER_DFLT_EXECUTABLES         = 'exe';

  FILE_FILTER_DFLT_LINUX_SETTINGS      = 'les';
  FILE_FILTER_FLT_LINUX_SETTINGS       = 'Linux encryption settings (*.les)|*.les|All files|*.*';

  COUNT_BITS  = '%1 bits';

const
  // xxx - This shouldn't really be used; we should be getting the actual
  // host disk's sector size and using that, instead of just *assuming* 512
  // bytes, as we do here
  // xxx - change this to get the host disk's geometry
  ASSUMED_HOST_SECTOR_SIZE = 512;

const
  // Only used by the GUI
  DEFAULT_SALT_LENGTH = 256;  // In bits

  DEFAULT_KEY_ITERATIONS = 2048;
  DEFAULT_KEY_ITERATIONS_INCREMENT = 512;

  // Default key length - only used if user's selected encryption algorithm
  // doesn't have a set keylength
  DEFAULT_CYPHER_KEY_LENGTH = 512;  // In bits

  // Default hash length - only used if user's selected hash algorithm
  // doesn't have a set hash length
  DEFAULT_HASH_LENGTH_LENGTH = 512;  // In bits

  // Default volume size as 25MB
  DEFAULT_VOLUME_SIZE: ULONGLONG = 25 * BYTES_IN_MEGABYTE;


type
  // Exceptions...
  EFreeOTFEError = class(Exception);
  EFreeOTFEInvalidParameter = class(EFreeOTFEError);
  EFreeOTFENeedAdminPrivs = class(EFreeOTFEError);
  EFreeOTFEConnectFailure = class(EFreeOTFEError);
  EFreeOTFEObsoleteDriver = class(EFreeOTFEError);
  EFreeOTFEInvalidDerivedKeyLength = class(EFreeOTFEError);
  EFreeOTFEInternalError = class(EFreeOTFEError);  // Critical failure within this software
  EFreeOTFEDriverError = class(EFreeOTFEError);  // Critical failure within the driver


type
  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  TFreeOTFECypherMode = (focmNone, focmECB, focmCBC, focmLRW, focmXTS, focmUnknown);

  TFreeOTFEMountAs = (fomaFixedDisk, fomaRemovableDisk, fomaCD, fomaDVD, fomaUnknown);

const
  // These consts are used to supply a dummy sector ID/size to the sector
  // encrypt/decrypt routines, where there is none/it's inconseqeuential
  FREEOTFE_v1_DUMMY_SECTOR_ID: LARGE_INTEGER = (QuadPart: 0);
  FREEOTFE_v1_DUMMY_SECTOR_SIZE = 512;

  DEFAULT_MOUNTAS = fomaFixedDisk;

resourcestring
  MOUNT_AS_FIXED_DISK      = 'Fixed disk';
  MOUNT_AS_REMOVABLE_DISK  = 'Removable disk';
  MOUNT_AS_CD              = 'CD';
  MOUNT_AS_DVD             = 'DVD';

  // Cypher mode display strings
  CYPHER_MODE_TITLE_NONE = 'None';
  CYPHER_MODE_TITLE_ECB  = 'ECB';
  CYPHER_MODE_TITLE_CBC  = 'CBC';
  CYPHER_MODE_TITLE_LRW  = 'LRW';
  CYPHER_MODE_TITLE_XTS  = 'XTS';

  // MAC display strings
  MAC_TITLE_HASH = 'Hash';
  MAC_TITLE_HMAC = 'HMAC';

  KDF_HASH_WITH_SALT = 'Hash with salt';
  KDF_PKCS5PBKDF2    = 'PKCS #5 PBKDF2 (HMAC)';

  // This is a DUPLICATE of the resourcestring found in SDUGeneral.pas
  // Required here in order to get rid of:
  //   [DCC Error] E2201 Need imported data reference ($G) to access 'RS_UNKNOWN' from unit 'OTFEFreeOTFE_U'
  // compiler errors
  RS_UNKNOWN = '<Unknown>';

const
  FreeOTFEMountAsTitlePtr : array [TFreeOTFEMountAs] of Pointer = (
                                                               @MOUNT_AS_FIXED_DISK,
                                                               @MOUNT_AS_REMOVABLE_DISK,
                                                               @MOUNT_AS_CD,
                                                               @MOUNT_AS_DVD,
                                                               @RS_UNKNOWN
                                                              );

  // Indicate which of the above emulated devices are writable
  FreeOTFEMountAsCanWrite : array [TFreeOTFEMountAs] of boolean = (
                                                               TRUE,
                                                               TRUE,
                                                               FALSE,
                                                               FALSE,
                                                               FALSE
                                                              );

  // Decode the above into device types/media types
  FreeOTFEMountAsDeviceType : array [TFreeOTFEMountAs] of DWORD = (
                                                              FILE_DEVICE_DISK,
                                                              FILE_DEVICE_DISK,
                                                              FILE_DEVICE_CD_ROM,
                                                              FILE_DEVICE_DVD,
                                                              FILE_DEVICE_UNKNOWN
                                                              );

  // Decode the above into device types/media types
  FreeOTFEMountAsStorageMediaType : array [TFreeOTFEMountAs] of TFreeOTFEStorageMediaType = (
                                                              mtFixedMedia,
                                                              mtRemovableMedia,
                                                              mtCD_ROM,
                                                              mtDVD_ROM,
                                                              mtUnknown
                                                              );



  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFECypherModeTitlePtr : array [TFreeOTFECypherMode] of Pointer = (
                                                                @CYPHER_MODE_TITLE_NONE,
                                                                @CYPHER_MODE_TITLE_ECB,
                                                                @CYPHER_MODE_TITLE_CBC,
                                                                @CYPHER_MODE_TITLE_LRW,
                                                                @CYPHER_MODE_TITLE_XTS,
                                                                @RS_UNKNOWN
                                                                    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFECypherModeID : array [TFreeOTFECypherMode] of integer = (
                                                       0,  // CYPHER_MODE_NONE
                                                       1,  // CYPHER_MODE_ECB
                                                       2,  // CYPHER_MODE_CBC
                                                       3,  // CYPHER_MODE_LRW
                                                       4,  // CYPHER_MODE_XTS
                                                    9999   // CYPHER_MODE_UNKNOWN
                                                                  );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEMACTitlePtr : array [TFreeOTFEMACAlgorithm] of Pointer = (
                                                                @MAC_TITLE_HASH,
                                                                @MAC_TITLE_HMAC,
                                                                @RS_UNKNOWN
                                                               );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFEMACID : array [TFreeOTFEMACAlgorithm] of integer = (
                                                       1,  // MAC_HASH
                                                       2,  // MAC_HMAC
                                                    9999   // MAC_UNKNOWN
                                                             );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEKDFTitlePtr : array [TFreeOTFEKDFAlgorithm] of Pointer = (
                                                                @KDF_HASH_WITH_SALT,
                                                                @KDF_PKCS5PBKDF2,
                                                                @RS_UNKNOWN
                                                               );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFEKDFID : array [TFreeOTFEKDFAlgorithm] of integer = (
                                                       1,  // KDF_HASH
                                                       2,  // KDF_PBKDF2
                                                    9999   // KDF_UNKNOWN
                                                             );


type

  // ------------------------
  // DELPHI FACING STRUCTURES
  // ------------------------

  // !! WARNING !!
  // When editing this structure:
  //   *) Add new elements to the end, to allow for backward compatibility
  //   *) Only used fixed-width fields; e.g. no "string" types
  POTFEFreeOTFEVolumeMetaData = ^TOTFEFreeOTFEVolumeMetaData;
  TOTFEFreeOTFEVolumeMetaData = packed record
    Signature: array [0..10] of char;
    Version: integer;

    LinuxVolume: boolean;
    PKCS11SlotID: integer;
  end;

  TOTFEFreeOTFEVolumeInfo = packed record
    DriveLetter: char;
    Filename: string;
    DeviceName: string;

    IVHashDevice: string;
    IVHashGUID: TGUID;
    IVCypherDevice: string;
    IVCypherGUID: TGUID;

    MainCypherDevice: string;
    MainCypherGUID: TGUID;

    Mounted: boolean;
    ReadOnly: boolean;

    VolumeFlags: DWORD;
    SectorIVGenMethod: TFreeOTFESectorIVGenMethod;

    MetaData: string;
    MetaDataStructValid: boolean;
    MetaDataStruct: TOTFEFreeOTFEVolumeMetaData;
  end;



type
  TFreeOTFECypher_v1 = packed record
    CypherGUID: TGUID;

    Title: string;
    Mode: TFreeOTFECypherMode;
    KeySizeUnderlying: integer;  // In bits
    BlockSize: integer;  // In bits
    VersionID: DWORD;
  end;

  TFreeOTFECypher_v3 = packed record
    CypherGUID: TGUID;

    Title: string;
    Mode: TFreeOTFECypherMode;
    KeySizeUnderlying: integer;  // In bits
    BlockSize: integer;  // In bits
    VersionID: DWORD;
    KeySizeRequired: integer;  // In bits
  end;


  PFreeOTFECypherDriver = ^TFreeOTFECypherDriver;
  TFreeOTFECypherDriver = packed record
    DriverGUID: TGUID;

    // PC DRIVER - The MSDOS device name (to be passed to the FreeOTFE driver when mounting)
    // PC DLL    - The library filename
    LibFNOrDevKnlMdeName: string;

    // --- KERNEL DRIVER ONLY ---
    // The device name
    DeviceName: string;
    // The MSDOS device name (to be used when connecting directly from userspace)
    DeviceUserModeName: string;
    // ------

    // The name of the driver, as should be displayed to the user
    Title: string;
    VersionID: DWORD;
    CypherCount: integer;
    Cyphers: array of TFreeOTFECypher_v3;
  end;


  TFreeOTFEHash = packed record
    HashGUID: TGUID;

    Title: string;
    VersionID: DWORD;
    Length: integer;  // In bits
    BlockSize: integer;  // In bits
  end;


  PFreeOTFEHashDriver = ^TFreeOTFEHashDriver;
  TFreeOTFEHashDriver = packed record
    DriverGUID: TGUID;

    // PC DRIVER - The MSDOS device name (to be passed to the FreeOTFE driver when mounting)
    // PC DLL    - The library filename
    LibFNOrDevKnlMdeName: string;

    // --- KERNEL DRIVER ONLY ---
    // The device name
    DeviceName: string;
    // The MSDOS device name (to be used when connecting directly from userspace)
    DeviceUserModeName: string;
    // ------

    // The name of the driver, as should be displayed to the user
    Title: string;
    VersionID: DWORD;
    HashCount: integer;
    Hashes: array of TFreeOTFEHash;
  end;


  // This definition is needed because we cannot pass an "array of Txyz" to a
  // function/procedure a "var" parameter, and the use SetLength(...) on that
  // parameter within the function/parameter
  TFreeOTFECypherDriverArray = array of TFreeOTFECypherDriver;
  TFreeOTFEHashDriverArray = array of TFreeOTFEHashDriver;


  TOTFEFreeOTFEBase = class(TOTFE)
  private
    // This is a bit hacky, but the Dump functionality does things that you
    // wouldn't normally do (i.e. it's a high level function that wants to
    // know about the inner workings of lower-level functionality; CDB
    // processing)
    DumpFlag: boolean;
    DumpCriticalDataKey: string;
    DumpCheckMAC: string;
    DumpPlaintextEncryptedBlock: string;
    DumpVolumeDetailsBlock: string;
  protected
    fPasswordChar: char;
    fAllowNewlinesInPasswords: boolean;
    fAllowTabsInPasswords: boolean;
    fCachedVersionID: DWORD;

    fPKCS11Library: TPKCS11Library;

    // Cached information: The strings in the list are kernel mode device
    // names, the objects are TFreeOTFEHashDriver/TFreeOTFECypherDriver
    fCachedHashDriverDetails: TStringList;
    fCachedCypherDriverDetails: TStringList;

    // Set the component active/inactive
    procedure SetActive(status: Boolean); override;

    // Connect/disconnect to the main FreeOTFE device driver
    function  Connect(): boolean; virtual; abstract;
    function  Disconnect(): boolean; virtual; abstract;


    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations

    // volFilename - This is the filename of the file/partition as seen by the
    //               user mode software
    function  MountDiskDevice(
                              deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              volFilename: string;
                              volumeKey: string;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: string;
                              readonly: boolean;
                              IVHashDriver: string;
                              IVHashGUID: TGUID;
                              IVCypherDriver: string;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: string;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              metaData: TOTFEFreeOTFEVolumeMetaData;
                              offset: int64 = 0;
                              size: int64 = 0;
                              storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; virtual; abstract;

    function  CreateMountDiskDevice(
                              volFilename: string;
                              volumeKey: string;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: string;
                              readonly: boolean;
                              IVHashDriver: string;
                              IVHashGUID: TGUID;
                              IVCypherDriver: string;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: string;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              DriveLetter: char;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              offset: int64 = 0;
                              size: int64 = 0;
                              MetaData_LinuxVolume: boolean = FALSE;  // Linux volume
                              MetaData_PKCS11SlotID: integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
                              MountMountAs: TFreeOTFEMountAs = fomaFixedDisk;  // PC kernel drivers *only* - ignored otherwise
                              mountForAllUsers: boolean = TRUE  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; virtual; abstract;

    function  DismountDiskDevice(
                                 deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                                 emergency: boolean
                                ): boolean; virtual; abstract;


    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    function ReadRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;

    // This executes a simple call to the driver to read dataLength bytes from
    // offsetWithinFile
    // Note: Will probably not work when reading directly from partitions;
    //       they typically need read/writes carried out in sector sized blocks
    //        - see ReadRawVolumeDataBounded for this.
    function ReadRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean; virtual;

    // This function is the same as ReadRawVolumeDataSimple(...), but will round
    // the offsetWithinFile down to the nearest sector offset, read in complete
    // sectors, and *only* *return "datalength" *bytes*
    // This function is included as reading from partitions must be carried out
    // in sector size blocks
    function ReadRawVolumeDataBounded(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;

    function WriteRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean; 

    function WriteRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean; virtual;

    function WriteRawVolumeDataBounded(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;

    function ReadWritePlaintextToVolume(
                    readNotWrite: boolean;

                    volFilename: string;
                    volumeKey: string;
                    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                    IVHashDriver: string;
                    IVHashGUID: TGUID;
                    IVCypherDriver: string;
                    IVCypherGUID: TGUID;
                    mainCypherDriver: string;
                    mainCypherGUID: TGUID;
                    VolumeFlags: integer;
                    mountMountAs: TFreeOTFEMountAs;

                    dataOffset: int64;  // Offset from within mounted volume from where to read/write data
                    dataLength: integer;  // Length of data to read/write. In bytes
                    var data: string;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean; virtual; abstract;


    // ---------
    // Misc functions

    // Generate a Linux volume's volume key
    function GenerateLinuxVolumeKey(
                                    hashDriver: string;
                                    hashGUID: TGUID;
                                    userKey: string;
                                    seed: string;
                                    hashWithAs: boolean;
                                    targetKeylengthBits: integer;
                                    var volumeKey: string
                                   ): boolean;

    function  GetNextDriveLetter(): char; overload;
    function  GetNextDriveLetter(userDriveLetter, requiredDriveLetter: char): char; overload; virtual; abstract;

    // Returns the type of driver used; either "DLL driver" or "Kernel driver"
    function  DriverType(): string; virtual; abstract;

    // ---------
    // Convert critical data area to/from a string representation

    // Note: This function does *not* append random padding data
    function  BuildVolumeDetailsBlock(volumeDetailsBlock: TVolumeDetailsBlock; var stringRep: string): boolean;
    // Note: Ignores any random padding data
    function  ParseVolumeDetailsBlock(stringRep: string; var volumeDetailsBlock: TVolumeDetailsBlock): boolean;


    // Return a nicely formatted string, decoding one bit of a bitmapped value
    function DumpCriticalDataToFileBitmap(
                                   value: DWORD;
                                   bit: integer;
                                   unsetMeaning: string;
                                   setMeaning: string
                                  ): string;

    // Convert a user-space volume filename to a format the kernel mode driver
    // can understand
    function GetKernelModeVolumeFilename(userModeFilename: string): string;
    // Convert a kernel mode driver volume filename to format user-space
    // filename
    function GetUserModeVolumeFilename(kernelModeFilename: string): string;

    function MetadataSig(): string;

    function PopulateVolumeMetadataStruct(
      LinuxVolume: boolean;
      PKCS11SlotID: integer;
      var metadata: TOTFEFreeOTFEVolumeMetaData
    ): boolean;

    function ParseVolumeMetadata(
      metadataAsString: string;
      var metadata: TOTFEFreeOTFEVolumeMetaData
    ): boolean;

    procedure VolumeMetadataToString(
      metadata: TOTFEFreeOTFEVolumeMetaData;
      var metadataAsString: string
    );

    // ---------
    // Internal caching functions
    procedure CachesCreate(); virtual;
    procedure CachesDestroy(); virtual;
    // Add a hash to the cache...
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    procedure CachesAddHashDriver(hashDriver: string; hashDriverDetails: TFreeOTFEHashDriver);
    // Get a hash from the cache...
    // Note: A cache MISS will cause this to return FALSE
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    function  CachesGetHashDriver(hashDriver: string; var hashDriverDetails: TFreeOTFEHashDriver): boolean;
    // Add a cypher to the cache...
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    procedure CachesAddCypherDriver(cypherDriver: string; cypherDriverDetails: TFreeOTFECypherDriver);
    // Get a cypher from the cache...
    // Note: A cache MISS will cause this to return FALSE
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    function  CachesGetCypherDriver(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;

    // v1 / v3 Cypher API functions
    // Note: This shouldn't be called directly - only via wrapper functions!
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    function _GetCypherDriverCyphers_v1(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; virtual; abstract;
    function _GetCypherDriverCyphers_v3(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; virtual; abstract;

  public
    AdvancedMountDlg: boolean;
    RevertVolTimestamps: boolean;

{$IFDEF FREEOTFE_DEBUG}
DebugStrings: TStrings;
DebugShowMessage: boolean;
{$IFNDEF _GEXPERTS}
DebugLogfile: string;
DebugLogfileCount: integer;
{$ENDIF}
procedure DebugClear();
procedure DebugMsg(msg: string);
procedure DebugMsgBinary(msg: string);
procedure DebugMsgBinaryPtr(msg: PChar; len:integer);
{$ENDIF}

{$INCLUDE OTFEFreeOTFE_mntLUKS_Intf.pas}

    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy(); override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; overload; override;
    function  CanMountDevice(): boolean; override;
    function  MountDevices(): string; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    // Although this Dismount(...) is introduced as virtual/abstractin TOTFE,
    // it needs to be shown here as well in order to prevent compiler error
    // ("Ambiguous overloaded call to 'Dismount'") in the above Dismount(...)
    // implementation
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override; abstract;
    function  Title(): string; overload; override;
    function  VersionStr(): string; overload; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  GetMainExe(): string; override;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE specific functions

    // Mount file(s) assuming specific volume type
    // Note: The volumeFilename(s) passed in can EITHER by files on the local
    //       filesystem (e.g. "C:\myfile.dat"), OR partitions
    //       (e.g. "\Device\Harddisk1\Partition3")
    // Note: "keyfile" is only supported for LUKS volumes atm
    // Note: If "keyfile" is supplied, the contents of this file will be used
    //       and "password" will be ignored
    function  MountLinux(
                         volumeFilename: string;
                         readonly: boolean = FALSE;
                         lesFile: string = '';
                         password: string = '';
                         keyfile: string = '';
                         keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
                         keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
                         offset: ULONGLONG = 0;
                         silent: boolean = FALSE;
                         forceHidden : boolean = FALSE
                        ): char; overload;
    function  MountLinux(
                         volumeFilenames: TStringList;
                         var mountedAs: string;
                         readonly: boolean = FALSE;
                         lesFile: string = '';
                         password: string = '';
                         keyfile: string = '';
                         keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
                         keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
                         offset: ULONGLONG = 0;
                         silent: boolean = FALSE;
                         forceHidden: boolean = FALSE
                        ): boolean; overload;
    // Note: The MountLUKS(...) functions will be called automatically if
    //       MountLinux(...) is called with a LUKS volume
    function  MountLUKS(
                        volumeFilename: string;
                        readonly: boolean = FALSE;
                        password: string = '';
                        keyfile: string = '';
                        keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
                        keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
                        silent: boolean = FALSE
                       ): char; overload;
    function  MountLUKS(
                        volumeFilenames: TStringList;
                        var mountedAs: string;
                        readonly: boolean = FALSE;
                        password: string = '';
                        keyfile: string = '';
                        keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
                        keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
                        silent: boolean = FALSE
                       ): boolean; overload;
    function  MountFreeOTFE(
                            volumeFilename: string;
                            readonly: boolean = FALSE;
                            keyfile: string = '';
                            password: string = '';
                            offset: ULONGLONG = 0;
                            noCDBAtOffset: boolean = FALSE;
                            silent: boolean = FALSE;
                            saltLength: integer = DEFAULT_SALT_LENGTH;
                            keyIterations: integer = DEFAULT_KEY_ITERATIONS
                           ): char; overload;
    function  MountFreeOTFE(
                            volumeFilenames: TStringList;
                            var mountedAs: string;
                            readonly: boolean = FALSE;
                            keyfile: string = '';
                            password: string = '';
                            offset: ULONGLONG = 0;
                            noCDBAtOffset: boolean = FALSE;
                            silent: boolean = FALSE;
                            saltLength: integer = DEFAULT_SALT_LENGTH;
                            keyIterations: integer = DEFAULT_KEY_ITERATIONS
                           ): boolean; overload;
    function  MountFreeOTFE(
                            volumeFilenames: TStringList;
                            UserKey: string;
                            Keyfile: string;
                            CDB: string; // CDB data (e.g. already read in PKCS#11 token)
                                         // If CDB is specified, "Keyfile" will be ignored
                            SlotID: integer;  // Set to PKCS#11 slot ID if PKCS#11 slot was *actually* used for mounting
                            PKCS11Session: TPKCS11Session;  // Set to nil if not needed
                            PKCS11SecretKeyRecord: PPKCS11SecretKey;  // Set to nil if not needed
                            KeyIterations: integer;
                            UserDriveLetter: char;  // PC kernel drivers *only* - ignored otherwise
                            MountReadonly: boolean;
                            MountMountAs: TFreeOTFEMountAs;  // PC kernel drivers *only* - ignored otherwise
                            Offset: int64;
                            OffsetPointsToCDB: boolean;
                            SaltLength: integer;  // In *bits*
                            MountForAllUsers: boolean;  // PC kernel drivers *only* - ignored otherwise
                            var mountedAs: string
                          ): boolean; overload; 

    // Get information on a mounted volume
    function  GetVolumeInfo(driveLetter: char; var volumeInfo: TOTFEFreeOTFEVolumeInfo): boolean; virtual; abstract;

    // Get details of all hash drivers and the hashes they support
    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): boolean; virtual; abstract;
    // Get details of a single hash driver, and all it's hashes
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    function GetHashDriverHashes(hashDriver: string; var hashDriverDetails: TFreeOTFEHashDriver): boolean; virtual; abstract;

    // Get details of all cypher drivers and the cyphers they support
    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): boolean; virtual; abstract;
    // Get details of a single cypher driver, and all it's cyphers
    // cypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
    function GetCypherDriverCyphers(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; virtual;

    // These two additional "helper" functions will *clear* and repopulate the TStringLists supplied with:
    //   A displayable title for each hash/cypher
    //   The kernel mode driver name for each hash/cypher
    //   The GUID for each hash/cypher
    function GetHashList(var hashDispTitles, hashKernelModeDriverNames, hashGUIDs: TStringList): boolean;
    function GetCypherList(var cypherDispTitles, cypherKernelModeDriverNames, cypherGUIDs: TStringList): boolean;

    // Two additional "helper" functions...
    // Generate a prettyprinted title for a specific cypher
    // Returns '' on error
    function GetCypherDisplayTitle(cypher: TFreeOTFECypher_v3): string;
    // (More technical version)
    function GetCypherDisplayTechTitle(cypher: TFreeOTFECypher_v3): string;
    // Generate a prettyprinted title for a specific hash
    // Returns '' on error
    function GetHashDisplayTitle(hash: TFreeOTFEHash): string;
    // (More technical version)
    function GetHashDisplayTechTitle(hash: TFreeOTFEHash): string;


    // Display a standard dialog with details of the specific hash identified
    procedure ShowHashDetailsDlg(driverName: string; hashGUID: TGUID);
    // Display a standard dialog with details of the specific cypher identified
    procedure ShowCypherDetailsDlg(driverName: string; cypherGUID: TGUID);


    function  GetSpecificHashDetails(hashKernelModeDeviceName: string; hashGUID: TGUID; var hashDetails: TFreeOTFEHash): boolean;
    function  GetSpecificCypherDetails(cypherKernelModeDeviceName: string; cypherGUID: TGUID; var cypherDetails: TFreeOTFECypher_v3): boolean;


    // Display dialog to allow user to select a partition
    // Returns '' if the user cancels/on error, otherwise returns a partition
    // identifier
    function  SelectPartition(): string;
    function  HDDNumbersList(var DiskNumbers: TSDUArrayInteger): boolean;
    function  HDDDeviceList(hddDevices: TStringList; title: TStringList): boolean; overload;
    function  HDDDeviceList(diskNo: integer; hddDevices: TStringList; title: TStringList): boolean; overload;
    function  CDROMDeviceList(cdromDevices: TStringList; title: TStringList): boolean;


    // Check to see if there's at least one cypher and one hash algorithm
    // available for use.
    // Warn user and return FALSE if not.
    function  WarnIfNoHashOrCypherDrivers(): boolean;

    // Convert a version ID into a prettyprinted version string
    function  VersionIDToStr(versionID: DWORD): string;

    // Handy function!
    procedure AddStdDumpHeader(content: TStringList; title: string);
    procedure AddStdDumpSection(content: TStringList; sectionTitle: string);

    // ---------
    // Algorithm driver functions

    // Encrypt data using the cypher/cypher driver identified
    // Encrypted data returned in "cyphertext"
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _EncryptData(
                         cypherKernelModeDeviceName: string;
                         cypherGUID: TGUID;
                         var key: string;
                         var IV: string;
                         var plaintext: string;
                         var cyphertext: string
                        ): boolean;
    // Decrypt data using the cypher/cypher driver identified
    // Decrypted data returned in "plaintext"
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _DecryptData(
                         cypherKernelModeDeviceName: string;
                         cypherGUID: TGUID;
                         var key: string;
                         var IV: string;
                         var cyphertext: string;
                         var plaintext: string
                        ): boolean;

    // Encrypt "sector" data using the cypher/cypher driver identified
    // Encrypted data returned in "cyphertext"
    function EncryptSectorData(
                         cypherKernelModeDeviceName: string;
                         cypherGUID: TGUID;
                         SectorID: LARGE_INTEGER;
                         SectorSize: integer;
                         var key: string;
                         var IV: string;
                         var plaintext: string;
                         var cyphertext: string
                        ): boolean;
    // Decrypt "sector" data using the cypher/cypher driver identified
    // Decrypted data returned in "plaintext"
    function DecryptSectorData(
                         cypherKernelModeDeviceName: string;
                         cypherGUID: TGUID;
                         SectorID: LARGE_INTEGER;
                         SectorSize: integer;
                         var key: string;
                         var IV: string;
                         var cyphertext: string;
                         var plaintext: string
                        ): boolean;

    // Combined...
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _EncryptDecryptData(
                                encryptFlag: boolean;
                                // driver - Kernel drivers: cypherKernelModeDeviceName
                                //          DLL drivers:    cypherLibFilename
                                cypherDriver: string;
                                cypherGUID: TGUID;
                                var key: string;
                                var IV: string;
                                var inData: string;
                                var outData: string
                               ): boolean; virtual; abstract;

    function EncryptDecryptSectorData(
                                encryptFlag: boolean;
                                // driver - Kernel drivers: cypherKernelModeDeviceName
                                //          DLL drivers:    cypherLibFilename
                                cypherDriver: string;
                                cypherGUID: TGUID;
                                SectorID: LARGE_INTEGER;
                                SectorSize: integer;
                                var key: string;
                                var IV: string;
                                var inData: string;
                                var outData: string
                               ): boolean; virtual; abstract;

    // Hash data using the hash driver identified
    // Result returned in "hashOut"
    // driver - Kernel drivers: hashKernelModeDeviceName
    //          DLL drivers:    hashLibFilename
    function HashData(
                      hashDriver: string;
                      hashGUID: TGUID;
                      var data: string;
                      var hashOut: string
                     ): boolean; virtual; abstract;

    // Generate MAC of data using hash driver/encryption driver identified
    // Note: "tBits" is in *bits* not *bytes*
    // tBits - Set the the number of bits to return; set to -ve value for no
    //         truncation/padding
    //         If tBits is larger than the number of bits the MAC would normally
    //         be, then the MAC will be right-padded with NULLs
    //         If tBits is set to a -ve number, then this function will return
    //         an MAC of variable length
    function MACData(
                      macAlgorithm: TFreeOTFEMACAlgorithm;
                      // HashDriver - Kernel drivers: hashKernelModeDeviceName
                      //              DLL drivers:    hashLibFilename
                      HashDriver: string;
                      HashGUID: TGUID;
                      // CypherDriver - Kernel drivers: cypherKernelModeDeviceName
                      //                DLL drivers:    cypherLibFilename
                      CypherDriver: string;
                      CypherGUID: TGUID;
                      var key: string;
                      var data: string;
                      var MACOut: string;
                      tBits: integer = -1
                     ): boolean; virtual; abstract;


    // Key derivation function
    function DeriveKey(
                    kdfAlgorithm: TFreeOTFEKDFAlgorithm;
                    // HashDriver - Kernel drivers: hashKernelModeDeviceName
                    //              DLL drivers:    hashLibFilename
                    HashDriver: string;
                    HashGUID: TGUID;
                    // CypherDriver - Kernel drivers: cypherKernelModeDeviceName
                    //                DLL drivers:    cypherLibFilename
                    CypherDriver: string;
                    CypherGUID: TGUID;
                    Password: string;
                    Salt: string;
                    Iterations: integer;
                    dkLenBits: integer;  // In *bits*
                    var DK: string
                   ): boolean; virtual; abstract;


    // ---------
    // Volume creation
    function CreateFreeOTFEVolumeWizard(): boolean;
    function CreateLinuxVolumeWizard(): boolean;
    function CreateLinuxGPGKeyfile(): boolean;

    function WizardChangePassword(): boolean;
    function WizardCreateKeyfile(): boolean;


    // ---------
    // Critical data block handling
    function  ReadVolumeCriticalData(
                                     filename: string;
                                     offsetWithinFile: int64;
                                     userPassword: string;
                                     saltLength: integer;  // In bits
                                     keyIterations: integer;

                                     var volumeDetails: TVolumeDetailsBlock;
                                     var CDBMetaData: TCDBMetaData
                                    ): boolean;

    function  ReadVolumeCriticalData_CDB(
                                     CDB: string;
                                     userPassword: string;
                                     saltLength: integer;  // In bits
                                     keyIterations: integer;

                                     var volumeDetails: TVolumeDetailsBlock;
                                     var CDBMetaData: TCDBMetaData
                                    ): boolean; overload;

    function  ReadVolumeCriticalData_CDB_v1(
                                     CDB: string;
                                     userPassword: string;
                                     saltLength: integer;  // In bits

                                     var volumeDetails: TVolumeDetailsBlock;
                                     var CDBMetaData: TCDBMetaData
                                    ): boolean; overload;

    function  ReadVolumeCriticalData_CDB_v2(
                                     CDB: string;
                                     userPassword: string;
                                     saltLength: integer;  // In bits
                                     keyIterations: integer;

                                     var volumeDetails: TVolumeDetailsBlock;
                                     var CDBMetaData: TCDBMetaData
                                    ): boolean; overload;

    function ChooseFromValid(
                             validVolumeDetails: TVolumeDetailsBlockArray;
                             validCDBMetaDatas: TCDBMetaDataArray;
                             var volumeDetails: TVolumeDetailsBlock;
                             var CDBMetaData: TCDBMetaData
                            ): boolean;

    // Write out a critical data block
    // If the file specified doesn't already exist, it will be created
    // ALL members of these two "volumeDetails" and "CDBMetaData" structs MUST
    // be populated
    // *Except* for volumeDetails.CDBFormatID - this is force set to the latest value
    function  WriteVolumeCriticalData(
                                      filename: string;
                                      offsetWithinFile: int64;
                                      userPassword: string;
                                      salt: string;
                                      keyIterations: integer;

                                      volumeDetails: TVolumeDetailsBlock;
                                      CDBMetaData: TCDBMetaData;

                                      randomPadData: string // Note: If insufficient is supplied, the write will
                                                            //       *fail*
                                                            //       The maximum that could possibly be required
                                                            //       is CRITICAL_DATA_LENGTH bits.
                                     ): boolean;  overload;

    function  BackupVolumeCriticalData(
                                       srcFilename: string;
                                       srcOffsetWithinFile: int64;
                                       destFilename: string
                                      ): boolean;

    function  RestoreVolumeCriticalData(
                                        srcFilename: string;
                                        destFilename: string;
                                        destOffsetWithinFile: int64
                                       ): boolean;

    function  DumpCriticalDataToFile(
                                   volFilename: string;
                                   offsetWithinFile: int64;
                                   userPassword: string;
                                   saltLength: integer;  // In bits
                                   keyIterations: integer;
                                   dumpFilename: string
                                  ): boolean;

    function  ChangeVolumePassword(
                                   filename: string;
                                   offsetWithinFile: int64;
                                   oldUserPassword: string;
                                   oldSaltLength: integer;  // In bits
                                   oldKeyIterations: integer;
                                   newUserPassword: string;
                                   newSaltBytes: string;
                                   newKeyIterations: integer;
                                   newRequestedDriveLetter: char;
                                   newRandomPadData: string  // Note: If insufficient is supplied, the write will *fail*
                                                             //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
                                  ): boolean;

    function  CreateKeyfile(
                               srcFilename: string;
                               srcOffsetWithinFile: int64;
                               srcUserPassword: string;
                               srcSaltLength: integer;  // In bits
                               srcKeyIterations: integer;
                               keyFilename: string;
                               keyfileUserPassword: string;
                               keyfileSaltBytes: string;
                               keyfileKeyIterations: integer;
                               keyfileRequestedDrive: char;
                               keyfileRandomPadData: string  // Note: If insufficient is supplied, the write will *fail*
                                                             //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
                              ): boolean;

    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    // Get the FreeOTFE driver to read a critical data block from the named file
    // starting from the specified offset
    function  ReadRawVolumeCriticalData(
                             filename: string;
                             offsetWithinFile: int64;
                             var criticalData: string
                            ): boolean; overload;

    // Get the FreeOTFE driver to write a critical data block from the named file
    // starting from the specified offset
    function  WriteRawVolumeCriticalData(
                             filename: string;
                             offsetWithinFile: int64;
                             criticalData: string
                            ): boolean; overload;

    // ---------
    // PKCS#11 related...

    procedure ShowPKCS11ManagementDlg();


    // ---------
    // FreeOTFE Driver handling...

    procedure CachesFlush();

    // ---------
    // Debug functions - expose private members in debug builds for testing...

{$IFDEF FREEOTFE_DEBUG}
    // Expose ReadRawVolumeData(...) for debug
    function DEBUGReadRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;

    // Expose WriteRawVolumeData(...) for debug
    function DEBUGWriteRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;

    // Expose ReadWritePlaintextToVolume(...) for debug
    function DEBUGReadWritePlaintextToVolume(
                    readNotWrite: boolean;

                    volFilename: string;
                    volumeKey: string;
                    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                    IVHashDriver: string;
                    IVHashGUID: TGUID;
                    IVCypherDriver: string;
                    IVCypherGUID: TGUID;
                    mainCypherDriver: string;
                    mainCypherGUID: TGUID;
                    VolumeFlags: integer;
                    mountMountAs: TFreeOTFEMountAs;

                    dataOffset: int64;  // Offset from within mounted volume from where to read/write data
                    dataLength: integer;  // Length of data to read/write. In bytes
                    var data: string;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean;
{$ENDIF}

    // Return TRUE/FALSE, depending on whether the specified USER MODE volume
    // filename refers to a partition/file
    function IsPartition_UserModeName(userModeFilename: string): boolean;

  published
    property PasswordChar: char read fPasswordChar write fPasswordChar;
    property AllowNewlinesInPasswords: boolean read fAllowNewlinesInPasswords write fAllowNewlinesInPasswords default TRUE;
    property AllowTabsInPasswords: boolean read fAllowTabsInPasswords write fAllowTabsInPasswords default FALSE;
    property PKCS11Library: TPKCS11Library read fPKCS11Library write fPKCS11Library;
  end;


function FreeOTFEMountAsTitle(mountAs: TFreeOTFEMountAs): string;
function FreeOTFECypherModeTitle(cypherMode: TFreeOTFECypherMode): string;
function FreeOTFEMACTitle(MACAlgorithm: TFreeOTFEMACAlgorithm): string;
function FreeOTFEKDFTitle(KDFAlgorithm: TFreeOTFEKDFAlgorithm): string;


implementation

uses
  SDUi18n,
{$IFDEF FREEOTFE_DEBUG}
{$IFDEF _GEXPERTS}
  DbugIntf,  // GExperts
{$ENDIF}
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
  OTFEFreeOTFE_frmWizardCreateVolume,
  OTFEFreeOTFE_frmSelectHashCypher,
  OTFEFreeOTFE_frmNewVolumeSize,
  OTFEFreeOTFE_DriverControl,
  OTFEFreeOTFE_frmDriverControl,
  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile,
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
  // Version ID for arbitary metadata passed to, and returned from, driver
  METADATA_VERSION = 1;

{$INCLUDE OTFEFreeOTFE_mntLUKS_Impl.pas}

// ----------------------------------------------------------------------------
function FreeOTFEMountAsTitle(mountAs: TFreeOTFEMountAs): string;
begin
  Result := LoadResString(FreeOTFEMountAsTitlePtr[mountAs]);
end;

function FreeOTFECypherModeTitle(cypherMode: TFreeOTFECypherMode): string;
begin
  Result := LoadResString(FreeOTFECypherModeTitlePtr[cypherMode]);
end;

function FreeOTFEMACTitle(MACAlgorithm: TFreeOTFEMACAlgorithm): string;
begin
  Result := LoadResString(FreeOTFEMACTitlePtr[MACAlgorithm]);
end;

function FreeOTFEKDFTitle(KDFAlgorithm: TFreeOTFEKDFAlgorithm): string;
begin
  Result := LoadResString(FreeOTFEKDFTitlePtr[KDFAlgorithm]);
end;

// ----------------------------------------------------------------------------
constructor TOTFEFreeOTFEBase.Create(AOwner : TComponent);
{$IFDEF FREEOTFE_DEBUG}
{$IFNDEF _GEXPERTS}
var
  dlg: TSaveDialog;
{$ENDIF}
{$ENDIF}
begin
  inherited;

  fAllowNewlinesInPasswords := TRUE;
  fAllowTabsInPasswords     := FALSE;

{$IFDEF FREEOTFE_DEBUG}
  DebugStrings := TStringList.Create();
  DebugShowMessage := FALSE;

{$IFNDEF _GEXPERTS}
  DebugLogFile := '';
  dlg:= TSaveDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [ofDontAddToRecent];
    if dlg.Execute() then
      begin
      DebugLogFile := dlg.Filename;
      end;
  finally
    dlg.Free();
  end;
{$ENDIF}

{$ENDIF}

  DumpFlag := FALSE;

  CachesCreate();

end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFEBase.Destroy();
begin
  CachesDestroy();

{$IFDEF FREEOTFE_DEBUG}
  DebugStrings.Free();
{$ENDIF}

  inherited;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.SetActive(status: boolean);
var
  allOK: boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  allOK := TRUE;

{$IFDEF FREEOTFE_DEBUG}
{$IFNDEF _GEXPERTS}
  // If logging debug to a file, clear to logfile
  if (DebugLogFile <> '') then
    begin
    DebugClear();
    end;
{$ENDIF}
{$ENDIF}

  // Flush caches
  CachesFlush();

{$IFDEF FREEOTFE_DEBUG}
if (status) then
  begin
  DebugMsg(
           '-------------------------------------------------------'+SDUCRLF+
           '-------------------------------------------------------'+SDUCRLF+
           'DEBUG BUILD OF TOTFEFreeOTFE component being set ACTIVE'+SDUCRLF+
           '-------------------------------------------------------'+SDUCRLF+
           '-------------------------------------------------------'
          );
  end;
{$ENDIF}

  if (status <> Active) then
    begin
    if status then
      begin
      allOK := Connect();
      if not(allOK) then
        begin
        raise EFreeOTFEConnectFailure.Create('FreeOTFE driver not installed/not running');
        end;

      end
    else
      begin
      allOK := Disconnect();
      if not(allOK) then
        begin
        raise EFreeOTFEConnectFailure.Create('Could not disconnect from FreeOTFE driver?!');
        end;

      end;

    end;


  // Note: LastErrorCode will have been set by Connect(...)/Dicconnect(...) on
  //       failure.
  if allOK then
    begin
    inherited;
    end;


  if (Active) then
    begin
    // Previous versions which are no longer supported due to changes in the driver API
    if (Version() < FREEOTFE_ID_v00_58_0000) then
      begin
      Active := FALSE;
      raise EFreeOTFEObsoleteDriver.Create(
                     'An old version of the FreeOTFE driver was detected. Please upgrade before using this software'
                     );
      end;

    end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountFreeOTFE(
  volumeFilename: string;
  readonly: boolean = FALSE;
  keyfile: string = '';
  password: string = '';
  offset: ULONGLONG = 0;
  noCDBAtOffset: boolean = FALSE;
  silent: boolean = FALSE;
  saltLength: integer = DEFAULT_SALT_LENGTH;
  keyIterations: integer = DEFAULT_KEY_ITERATIONS
): char;
var
  tmpStringList: TStringList;
  mountedAs: string;
  retVal: char;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := #0;

  tmpStringList:= TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountFreeOTFE(
                     tmpStringList,
                     mountedAs,
                     readonly,
                     keyfile,
                     password,
                     offset,
                     noCDBAtOffset,
                     silent,
                     saltLength,
                     keyIterations
                    ) then
      begin
      retVal := mountedAs[1];
      end;

  finally
    tmpStringList.Free();
  end;

  Result :=  retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountFreeOTFE(
  volumeFilenames: TStringList;
  var mountedAs: string;
  readonly: boolean = FALSE;
  keyfile: string = '';
  password: string = '';
  offset: ULONGLONG = 0;
  noCDBAtOffset: boolean = FALSE;
  silent: boolean = FALSE;
  saltLength: integer = DEFAULT_SALT_LENGTH;
  keyIterations: integer = DEFAULT_KEY_ITERATIONS
): boolean;
var
  retVal: boolean;
  keyEntryDlg: TfrmKeyEntryFreeOTFE;
  mr: integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := FALSE;
  mountedAs := StringOfChar(#0, volumeFilenames.count);

  CheckActive();

  if (LastErrorCode = OTFE_ERR_SUCCESS) then
    begin
    keyEntryDlg := TfrmKeyEntryFreeOTFE.Create(nil);
    try
      keyEntryDlg.FreeOTFEObj := self;
      keyEntryDlg.Silent := silent;
      keyEntryDlg.SetPassword(password);
      keyEntryDlg.SetReadonly(readonly);
      keyEntryDlg.SetOffset(offset);
      keyEntryDlg.SetCDBAtOffset(not(noCDBAtOffset));
      keyEntryDlg.SetSaltLength(saltLength);
      keyEntryDlg.SetKeyIterations(keyIterations);
      keyEntryDlg.SetKeyfile(keyfile);
      // .FreeOTFEObj must be set before the advanced options can be displayed;
      // required in case PKCS#11 support is enabled (needed to populate
      // PKCS#11 comboboxes)
      keyEntryDlg.DisplayAdvanced(AdvancedMountDlg);
      keyEntryDlg.VolumeFiles := volumeFilenames;

      mr := keyEntryDlg.ShowModal();
      if (mr = mrCancel) then
        begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
        end
      else
        begin
        mountedAs := keyEntryDlg.MountedDrives;
        retVal := TRUE;
        end;

    finally
      keyEntryDlg.Free()
    end;
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Important: To use CDB from keyfile/volume, "CDB" *must* be set to an empty
//            string
function TOTFEFreeOTFEBase.MountFreeOTFE(
                            volumeFilenames: TStringList;
                            UserKey: string;
                            Keyfile: string;
                            CDB: string; // CDB data (e.g. already read in PKCS#11 token)
                                         // If CDB is specified, "Keyfile" will be ignored
                            SlotID: integer;  // Set to PKCS#11 slot ID if PKCS#11 slot was *actually* used for mounting
                            PKCS11Session: TPKCS11Session;  // Set to nil if not needed
                            PKCS11SecretKeyRecord: PPKCS11SecretKey;  // Set to nil if not needed
                            KeyIterations: integer;
                            UserDriveLetter: char;
                            MountReadonly: boolean;
                            MountMountAs: TFreeOTFEMountAs;
                            Offset: int64;
                            OffsetPointsToCDB: boolean;
                            SaltLength: integer;  // In *bits*
                            MountForAllUsers: boolean;
                            var mountedAs: string
                          ): boolean;
var
  retVal: boolean;
  i: integer;
  currMountFilename: string;
  useFileOffset: int64;
  mountDriveLetter: char;

  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;

  useCDB: string;
  decryptedCDB: string;
  commonCDB: string;
  errMsg: string;

  prevCursor: TCursor;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := TRUE;

  CheckActive();

  commonCDB := '';
  if (CDB <> '') then
    begin
    commonCDB := CDB;
    end
  else if (Keyfile <> '') then
    begin
    // Attempt to obtain the current file's critical data
    retVal := ReadRawVolumeCriticalData(
                                      Keyfile,
                                      0,
                                      commonCDB
                                     );
    if not(retVal) then
      begin
      LastErrorCode := OTFE_ERR_KEYFILE_NOT_FOUND;
      end;
    end;

  if retVal then
    begin
    prevCursor:= Screen.Cursor;
    Screen.Cursor := crHourglass;
    try 
      // For each volume
      for i:=0 to (volumeFilenames.count-1) do
        begin
        useCDB := commonCDB;
        currMountFilename := volumeFilenames[i];

        if (useCDB = '') then
          begin
          // Attempt to obtain the current file's critical data
          if not(ReadRawVolumeCriticalData(
                                            volumeFilenames[i],
                                            Offset,
                                            useCDB
                                           )) then
            begin
            // Bad file...
            LastErrorCode := OTFE_ERR_VOLUME_FILE_NOT_FOUND;
            mountedAs := mountedAs + #0;
            retVal := FALSE;
            continue;
            end;
          end;

        // If CDB encrypted by PKCS#11 secret key, decrypt it now...
        decryptedCDB := useCDB;
        if (
            (PKCS11Session <> nil) and
            (PKCS11SecretKeyRecord <> nil)
           ) then
          begin
          if not(PKCS11DecryptCDBWithSecretKey(
                                               PKCS11Session,
                                               PKCS11SecretKeyRecord,
                                               useCDB,
                                               decryptedCDB,
                                               errMsg
                                              )) then
            begin
            LastErrorCode := OTFE_ERR_PKCS11_SECRET_KEY_DECRYPT_FAILURE;
            mountedAs := mountedAs + #0;
            retVal := FALSE;
            // Bail out; can't continue as problem with token
            break;
            end;
          end;

        // Process CDB passed in into CDBMetaData
        if not(ReadVolumeCriticalData_CDB(
                                      decryptedCDB,
                                      UserKey,
                                      SaltLength,  // In *bits*
                                      KeyIterations,
                                      volumeDetails,
                                      CDBMetaData
                                     )) then
          begin
          // Wrong password, bad volume file, correct hash/cypher driver not installed
          LastErrorCode := OTFE_ERR_WRONG_PASSWORD;
          mountedAs := mountedAs + #0;
          retVal := FALSE;
          continue;
          end;

        // Mount the volume
        if (volumeDetails.RequestedDriveLetter = #0) then
          begin
          // Nudge on to prevent getting drive A: or B:
          volumeDetails.RequestedDriveLetter := 'C';
          end;
        mountDriveLetter := GetNextDriveLetter(UserDriveLetter, volumeDetails.RequestedDriveLetter);
        if (mountDriveLetter = #0) then
          begin
          // Skip onto next volume file...
          LastErrorCode := OTFE_ERR_NO_FREE_DRIVE_LETTERS;
          mountedAs := mountedAs + #0;
          retVal := FALSE;
          continue;
          end;

        // Locate where the actual encrypted partition starts within the
        // volume file
        useFileOffset := Offset;
        if OffsetPointsToCDB then
          begin
          useFileOffset := useFileOffset + int64((CRITICAL_DATA_LENGTH div 8));
          end;

        if CreateMountDiskDevice(
                             currMountFilename,
                             volumeDetails.MasterKey,
                             volumeDetails.SectorIVGenMethod,
                             volumeDetails.VolumeIV,
                             MountReadonly,
                             CDBMetaData.HashDriver,
                             CDBMetaData.HashGUID,
                             CDBMetaData.CypherDriver,  // IV cypher
                             CDBMetaData.CypherGUID,  // IV cypher
                             CDBMetaData.CypherDriver,  // Main cypher
                             CDBMetaData.CypherGUID,  // Main cypher
                             volumeDetails.VolumeFlags,
                             mountDriveLetter,
                             useFileOffset,
                             volumeDetails.PartitionLen,
                             FALSE,
                             SlotID,
                             MountMountAs,
                             MountForAllUsers
                             ) then
          begin
          mountedAs := mountedAs + mountDriveLetter;
          end
        else
          begin
          mountedAs := mountedAs + #0;
          // LastErrorCode set by MountDiskDevice (called by CreateMountDiskDevice)????
          LastErrorCode := OTFE_ERR_MOUNT_FAILURE;
          retVal := FALSE;
          end;

        end;  // for i:=0 to (volumeFilenames.count) do

    finally
      Screen.Cursor :=prevCursor;
    end;

    end;


  // Pad out unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  for i:=(length(mountedAs)+1) to (volumeFilenames.count) do
    begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := FALSE;
    end;


  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Generate the metadata signature to be used
// Returns: Appropriatly sized signature
function TOTFEFreeOTFEBase.MetadataSig(): string;
var
  tmpSig: string;
  retval: string;
  tmpStruct: TOTFEFreeOTFEVolumeMetaData;
begin
  // Dummy assignment to get rid of compiler warning; this variable is only
  // used to get the sizeof(...) the signature
  tmpStruct.Version := 1;

  tmpSig := Title();
  tmpSig := tmpSig + StringOfChar(' ', length(tmpStruct.Signature));

  // Copy(...) indexes from 1
  retval := Copy(tmpSig, 1, length(tmpStruct.Signature));

  Result := retval;
end;

// ----------------------------------------------------------------------------
// Populate metadata struct with information
function TOTFEFreeOTFEBase.PopulateVolumeMetadataStruct(
  LinuxVolume: boolean;
  PKCS11SlotID: integer;
  var metadata: TOTFEFreeOTFEVolumeMetaData
): boolean;
var
  i: integer;
  tmpSig: string;
begin
  // Standard...
  tmpSig := MetadataSig();
  for i:=low(metadata.Signature) to high(metadata.Signature) do
    begin
    // +1 because the string indexes from 1, while low(metadata.Signature) is
    // zero
    metadata.Signature[i] := tmpSig[i+1];
    end;
  metadata.Version := METADATA_VERSION;

  metadata.PKCS11SlotID := PKCS11SlotID;
  metadata.LinuxVolume := LinuxVolume;

  Result := TRUE;
end;


// ----------------------------------------------------------------------------
// Convert string to metadata struct
function TOTFEFreeOTFEBase.ParseVolumeMetadata(
  metadataAsString: string;
  var metadata: TOTFEFreeOTFEVolumeMetaData
): boolean;
var
  retval: boolean;
  tmpStructPtr: POTFEFreeOTFEVolumeMetaData;
begin
  retval := (Pos(MetadataSig(), metadataAsString) = 0);

  if retval then
    begin
    retval := (length(metadataAsString) = sizeof(metadata));
    if retval then
      begin
      tmpStructPtr := POTFEFreeOTFEVolumeMetaData(PChar(metadataAsString));
      metadata := tmpStructPtr^;
      end;

    end;

  Result := retval;
end;

// ----------------------------------------------------------------------------
// Convert metadata struct to string
procedure TOTFEFreeOTFEBase.VolumeMetadataToString(
  metadata: TOTFEFreeOTFEVolumeMetaData;
  var metadataAsString: string
);
begin
  metadataAsString := StringOfChar(#0, sizeof(metadata));
  StrMove(PChar(metadataAsString), @metadata, sizeof(metadata));
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLinux(
  volumeFilename: string;
  readonly: boolean = FALSE;
  lesFile: string = '';
  password: string = '';
  keyfile: string = '';
  keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  offset: ULONGLONG = 0;
  silent: boolean = FALSE;
  forceHidden: boolean = FALSE
): char;
var
  tmpStringList: TStringList;
  mountedAs: string;
  retVal: char;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := #0;

  tmpStringList:= TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountLinux(
                  tmpStringList,
                  mountedAs,
                  readonly,
                  lesFile,
                  password,
                  keyfile,
                  keyfileIsASCII,
                  keyfileNewlineType,
                  offset,
                  silent,
                  forceHidden
                 ) then
      begin
      retVal := mountedAs[1];
      end;

  finally
    tmpStringList.Free();
  end;

  Result :=  retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLinux(
  volumeFilenames: TStringList;
  var mountedAs: string;
  readonly: boolean = FALSE;
  lesFile: string = '';
  password: string = '';
  keyfile: string = '';
  keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  offset: ULONGLONG = 0;
  silent: boolean = FALSE;
  forceHidden: boolean = FALSE
): boolean;
var
  retVal: boolean;
  keyEntryDlg: TfrmKeyEntryLinux;
  i: integer;
  volumeKey: string;

  userKey: string;
  keyProcSeed: string;
  keyProcHashDriver: string;
  keyProcHashGUID: TGUID;
  keyProcHashWithAs: boolean;
  keyProcCypherDriver: string;
  keyProcCypherGUID: TGUID;
  keyProcIterations: integer;
  fileOptOffset: int64;
  fileOptSize: int64;
  mountDriveLetter: char;
  mountReadonly: boolean;
  mainCypherDriver: string;
  mainCypherGUID: TGUID;
  mainIVHashDriver: string;
  mainIVHashGUID: TGUID;
  mainIVCypherDriver: string;
  mainIVCypherGUID: TGUID;
  currDriveLetter: char;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  startOfVolFile: boolean;
  startOfEndData: boolean;
  mountMountAs: TFreeOTFEMountAs;
  VolumeFlags: DWORD;
  mr: integer;
  mainCypherDetails: TFreeOTFECypher_v3;
  mountForAllUsers: boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := TRUE;

  CheckActive();

  // Sanity checking
  if (volumeFilenames.count = 0) then
    begin
    mountedAs := '';
    Result := TRUE;
    exit;
    end
  else
    begin
    // Is the user attempting to mount Linux LUKS volumes?
    // Mount as LUKS if they are...
  {  TDK change - test user option to force non-luks (for hidden vols) }
    if (IsLUKSVolume(volumeFilenames[0])) and not forceHidden then
      begin
      Result := MountLUKS(
                          volumeFilenames,
                          mountedAs,
                          readonly,
                          password,
                          keyfile,
                          silent
                         );
      exit;
      end;
    end;


  keyEntryDlg := TfrmKeyEntryLinux.Create(nil);
  try
    keyEntryDlg.FreeOTFEObj := self;
    keyEntryDlg.Initialize();
    if (lesFile <> '') then
      begin
      keyEntryDlg.LoadSettings(lesFile);
      end;
    keyEntryDlg.SetReadonly(readonly);
    keyEntryDlg.SetKey(password);
    keyEntryDlg.SetOffset(offset);

    if silent then
      begin
      mr := mrOK;
      end
    else
      begin
      mr := keyEntryDlg.ShowModal();
      end;
    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then
      begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
      end
    else if (mr = mrOK) then
      begin
      // Retrieve all data from the mount dialog...
      if (
          // Key...
          (keyEntryDlg.GetKey(userKey)) AND

          // Key processing...
          (keyEntryDlg.GetKeyProcSeed(keyProcSeed)) AND
          (keyEntryDlg.GetKeyProcHashKernelDeviceName(keyProcHashDriver)) AND
          (keyEntryDlg.GetKeyProcHashGUID(keyProcHashGUID)) AND
          (keyEntryDlg.GetKeyProcHashWithAs(keyProcHashWithAs)) AND
          (keyEntryDlg.GetKeyProcCypherKernelDeviceName(keyProcCypherDriver)) AND
          (keyEntryDlg.GetKeyProcCypherGUID(keyProcCypherGUID)) AND
          (keyEntryDlg.GetKeyProcCypherIterationCount(keyProcIterations)) AND

          // File options...
          (keyEntryDlg.GetOffset(fileOptOffset)) AND
          (keyEntryDlg.GetSizeLimit(fileOptSize)) AND

          // Mount options...
          (keyEntryDlg.GetDriveLetter(mountDriveLetter)) AND
          (keyEntryDlg.GetReadonly(mountReadonly)) AND
          (keyEntryDlg.GetMountAs(mountMountAs)) AND

          // Encryption options...
          (keyEntryDlg.GetMainCypherKernelDeviceName(mainCypherDriver)) AND
          (keyEntryDlg.GetMainCypherGUID(mainCypherGUID)) AND
          (keyEntryDlg.GetMainSectorIVGenMethod(sectorIVGenMethod)) AND
          (keyEntryDlg.GetMainIVSectorZeroPos(startOfVolFile, startOfEndData)) AND
          (keyEntryDlg.GetMainIVHashKernelDeviceName(mainIVHashDriver)) AND
          (keyEntryDlg.GetMainIVHashGUID(mainIVHashGUID)) AND
          (keyEntryDlg.GetMainIVCypherKernelDeviceName(mainIVCypherDriver)) AND
          (keyEntryDlg.GetMainIVCypherGUID(mainIVCypherGUID))
         ) then
        begin
        mountForAllUsers := keyEntryDlg.GetMountForAllUsers();

        // Before we can generate the volume key for encryption/decryption, we
        // need to know the keysize of the cypher
        if not(GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then
          begin
          retVal := FALSE;
          end;


        // Generate volume key for encryption/decryption
        if retVal then
          begin
          if not(GenerateLinuxVolumeKey(
                                        keyProcHashDriver,
                                        keyProcHashGUID,
                                        userKey,
                                        keyProcSeed,
                                        keyProcHashWithAs,
                                        mainCypherDetails.KeySizeRequired,
                                        volumeKey
                                       )) then
            begin
            LastErrorCode := OTFE_ERR_HASH_FAILURE;
            retVal := FALSE;
            end;
          end;


        // Encode IV generation method to flags...
        VolumeFlags := 0;
        if (startOfVolFile) then
          begin
          VolumeFlags := VolumeFlags OR VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART;
          end;


        if retVal then
          begin
          for i:=0 to (volumeFilenames.count-1) do
            begin
            currDriveLetter := GetNextDriveLetter(mountDriveLetter, #0);
            if (currDriveLetter = #0) then
              begin
              // No more drive letters following the user's specified drive
              // letter - don't mount further drives
              retVal := FALSE;
              // Bail out...
              break;
              end;

            if CreateMountDiskDevice(
                                 volumeFilenames[i],
                                 volumeKey,
                                 sectorIVGenMethod,
                                 '',  // Linux volumes don't have per-volume IVs
                                 mountReadonly,
                                 mainIVHashDriver,
                                 mainIVHashGUID,
                                 mainIVCypherDriver,
                                 mainIVCypherGUID,
                                 mainCypherDriver,
                                 mainCypherGUID,
                                 VolumeFlags,
                                 currDriveLetter,
                                 fileOptOffset,
                                 fileOptSize,
                                 TRUE,  // Linux volume
                                 PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
                                 mountMountAs,
                                 mountForAllUsers
                                ) then
              begin
              mountedAs := mountedAs + currDriveLetter;
              end
            else
              begin
              mountedAs := mountedAs + #0;
              retVal := FALSE;
              end;

            end;  // for i:=0 to (volumeFilenames.count) do

          end;  // if retVal then
        end;

      end;


  finally
    keyEntryDlg.Free()
  end;


  // Unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  for i:=(length(mountedAs)+1) to (volumeFilenames.count) do
    begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := FALSE;
    end;


  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Generate a Linux volume's volume key
// hashWithAs - If set to FALSE, the user's key will be seeded and hashed once
//              the result will be right padded with 0x00 chars/truncated as
//              appropriate
//              If set to TRUE, then:
//                If "targetKeylengthBits" is > -1, the user's key will be
//                seeded and hashed once
//                Otherwise, the volume key will be created by concatenating
//                  Hashing the user's seeded key
//                  Hashing 'A' + the user's seeded key
//                  Hashing 'AA' + the user's seeded key
//                  Hashing 'AAA' + the user's seeded key
//                etc - until the required number of bits is reached
function TOTFEFreeOTFEBase.GenerateLinuxVolumeKey(
                                              hashDriver: string;
                                              hashGUID: TGUID;
                                              userKey: string;
                                              seed: string;
                                              hashWithAs: boolean;
                                              targetKeylengthBits: integer;
                                              var volumeKey: string
                                             ): boolean;
var
  seededKey: string;
  hashOutput: string;
  allOK: boolean;
begin
  allOK:= TRUE;

  // Salt the user's password
  seededKey := userKey + seed;


  // If the target key length is any size key, or we don't hash with A's, then
  // we simply hash the user's key once with the specified hash
  if (
      (targetKeylengthBits = -1) or
      not(hashWithAs)
     ) then
    begin
    allOK := HashData(hashDriver, hashGUID, seededKey, volumeKey);
    end
  else
    begin
    // We're been asked to supply a volume key with a fixed, defined length
    while (
           allOK and
           (Length(volumeKey) < (targetKeylengthBits div 8))
          ) do
      begin
      // Note: We'll truncate later - don't worry if volumeKey is set to too
      //       many bits at this stage...
      allOK := allOK and HashData(hashDriver, hashGUID, seededKey, hashOutput);
      volumeKey := volumeKey + hashOutput;
      // Prepare for next...
      seededKey := 'A'+seededKey;
      end;

    end;


  // If the target key length is > -1, truncate or we right-pad with 0x00
  // as appropriate
  if (allOK) then
    begin
    if (targetKeylengthBits > -1) then
      begin
      // Copy as much of the hash value as possible to match the key length
      volumeKey := Copy(volumeKey, 1, min((targetKeylengthBits div 8), length(volumeKey)));
      // Right-pad if needed
      volumeKey := volumeKey + StringOfChar(#0, ((targetKeylengthBits div 8) - Length(volumeKey)));
      end;

    end;


  if not(allOK) then
    begin
    LastErrorCode := OTFE_ERR_HASH_FAILURE;
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetNextDriveLetter(): char; 
begin
  Result:= GetNextDriveLetter(#0, #0);
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLUKS(
  volumeFilename: string;
  readonly: boolean = FALSE;
  password: string = '';
  keyfile: string = '';
  keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: boolean = FALSE
): char;
var
  tmpStringList: TStringList;
  mountedAs: string;
  retVal: char;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := #0;

  tmpStringList:= TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountLUKS(
                 tmpStringList,
                 mountedAs,
                 readonly,
                 password,
                 keyfile,
                 keyfileIsASCII,
                 keyfileNewlineType,
                 silent
                ) then
      begin
      retVal := mountedAs[1];
      end;

  finally
    tmpStringList.Free();
  end;

  Result :=  retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLUKS(
  volumeFilenames: TStringList;
  var mountedAs: string;
  readonly: boolean = FALSE;
  password: string = '';
  keyfile: string = '';
  keyfileIsASCII: boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: boolean = FALSE
): boolean;
var
  retVal: boolean;
  keyEntryDlg: TfrmKeyEntryLUKS;
  i: integer;
  volumeKey: string;

  userKey: string;
  fileOptSize: int64;
  mountDriveLetter: char;
  mountReadonly: boolean;
  currDriveLetter: char;
  mountMountAs: TFreeOTFEMountAs;
  mr: integer;
  LUKSHeader: TLUKSHeader;
  keySlot: integer;
  baseIVCypherOnHashLength: boolean;
  mountForAllUsers: boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal := TRUE;

  CheckActive();

  keyEntryDlg := TfrmKeyEntryLUKS.Create(nil);
  try
    keyEntryDlg.FreeOTFEObj := self;
    keyEntryDlg.Initialize();
    keyEntryDlg.SetReadonly(readonly);
    keyEntryDlg.SetKey(password);
    keyEntryDlg.SetKeyfile(keyfile);
    keyEntryDlg.SetKeyfileIsASCII(keyfileIsASCII);
    keyEntryDlg.SetKeyfileNewlineType(keyfileNewlineType);

    if silent then
      begin
      mr := mrOK;
      end
    else
      begin
      mr := keyEntryDlg.ShowModal();
      end;
    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then
      begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
      end
    else if (mr = mrOK) then
      begin
      // Retrieve all data from the mount dialog...
      if (
          // Key...
          (keyEntryDlg.GetKey(userKey)) AND
          (keyEntryDlg.GetIVCypherBase(baseIVCypherOnHashLength)) AND

          // File options...
          (keyEntryDlg.GetSizeLimit(fileOptSize)) AND

          // Mount options...
          (keyEntryDlg.GetDriveLetter(mountDriveLetter)) AND
          (keyEntryDlg.GetReadonly(mountReadonly)) AND
          (keyEntryDlg.GetMountAs(mountMountAs))
         ) then
        begin
        mountForAllUsers := keyEntryDlg.GetMountForAllUsers();

        for i:=0 to (volumeFilenames.count-1) do
          begin
          currDriveLetter := GetNextDriveLetter(mountDriveLetter, #0);
          if (currDriveLetter = #0) then
            begin
            // No more drive letters following the user's specified drive
            // letter - don't mount further drives
            retVal := FALSE;
            // Bail out...
            break;
            end;

          if not(RecoverMasterKey(
                              volumeFilenames[i],
                              userKey,
                              baseIVCypherOnHashLength,
                              LUKSHeader,
                              keySlot,
                              volumeKey
                             )) then
            begin
            retVal := FALSE;
            // Bail out...
            break;
            end;

{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Recovered key OK');
  DebugMsg('key slot '+inttostr(keySlot)+' unlocked.');
  DebugMsg('Master key: '+PrettyHex(
                                    PChar(volumeKey),
                                    length(volumeKey)
                                   ));
{$ENDIF}

          if CreateMountDiskDevice(
                               volumeFilenames[i],
                               volumeKey,
                               LUKSHeader.sectorIVGenMethod,
                               '',  // Linux volumes don't have per-volume IVs
                               mountReadonly,
                               LUKSHeader.IVHashKernelModeDeviceName,
                               LUKSHeader.IVHashGUID,
                               LUKSHeader.IVCypherKernelModeDeviceName,
                               LUKSHeader.IVCypherGUID,
                               LUKSHeader.cypherKernelModeDeviceName,
                               LUKSHeader.cypherGUID,
                               0,  // Volume flags
                               currDriveLetter,
                               (LUKSHeader.payload_offset * LUKS_SECTOR_SIZE),
                               fileOptSize,
                               TRUE,  // Linux volume
                               PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
                               mountMountAs,
                               mountForAllUsers
                              ) then
            begin
            mountedAs := mountedAs + currDriveLetter;
            end
          else
            begin
            mountedAs := mountedAs + #0;
            retVal := FALSE;
            end;

          end;  // for i:=0 to (volumeFilenames.count) do

        end;

      end;


  finally
    keyEntryDlg.Free()
  end;


  // Unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  for i:=(length(mountedAs)+1) to (volumeFilenames.count) do
    begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := FALSE;
    end;


  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  tmpStringList: TStringList;
  mountedAs: string;
  retVal: char;
begin
  retVal := #0;

  tmpStringList:= TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if Mount(tmpStringList, mountedAs, readonly) then
      begin
      retVal := mountedAs[1];
      end;

  finally
    tmpStringList.Free();
  end;

  Result :=  retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
var
  frmSelectVolumeType: TfrmSelectVolumeType;
  retVal: boolean;
  mr: integer;
begin
  retVal := FALSE;

  CheckActive();

  frmSelectVolumeType:= TfrmSelectVolumeType.Create(nil);
  try
    mr := frmSelectVolumeType.ShowModal();
    if (mr = mrCancel) then
      begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
      end
    else if (mr = mrOK) then
      begin
      if frmSelectVolumeType.FreeOTFEVolume then
        begin
        retVal := MountFreeOTFE(volumeFilenames, mountedAs, readonly);
        end
      else
        begin
        retVal := MountLinux(volumeFilenames, mountedAs, readonly);
        end;

      end;

  finally
    frmSelectVolumeType.Free();;
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Title(): string;
begin
  Result := 'FreeOTFE';
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.VersionStr(): string;
var
  verNo: cardinal;
  retVal: string;
begin
  retval := '';

  CheckActive();

  verNo := Version();
  retVal := VersionIDToStr(verNo);

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Convert a version ID into a prettyprinted version string
function TOTFEFreeOTFEBase.VersionIDToStr(versionID: DWORD): string;
var
  majorVer: integer;
  minorVer: integer;
  buildVer: integer;
  retval: string;
begin
  retval := '';

  if (versionID <> VERSION_ID_FAILURE) then
    begin
    majorVer := (versionID AND $FF000000) div $00FF0000;
    minorVer := (versionID AND $00FF0000) div $0000FF00;
    buildVer := (versionID AND $0000FFFF);
    retval := Format('v%d.%.2d.%.4d', [majorVer, minorVer, buildVer]);
    end;

  Result := retval;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  CheckActive();

  // We can't tell! Return TRUE...
  Result := TRUE;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetVolFileForDrive(driveLetter: char): string;
var
  retVal: string;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  retVal := '';

  CheckActive();

  if (GetVolumeInfo(upcase(driveLetter), volumeInfo)) then
    begin
    retVal := volumeInfo.Filename;
    end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetDriveForVolFile(volumeFilename: string): char;
var
  mounted: string;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  retVal: char;
  i: integer;
begin
  retVal := #0;

  CheckActive();

  mounted := DrivesMounted();
  for i:=1 to length(mounted) do
    begin
    if GetVolumeInfo(mounted[i], volumeInfo) then
      begin
      if (uppercase(volumeInfo.Filename) = uppercase(volumeFilename)) then
        begin
        retVal := volumeInfo.DriveLetter;
        break;
        end;

      
      end;
    end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetMainExe(): string;
begin
  // This is not meaningful for FreeOTFE
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;
  Result := '';

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetCypherDriverCyphers(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  retval: boolean;
begin
  retval := _GetCypherDriverCyphers_v3(cypherDriver, cypherDriverDetails);
  if not(retval) then
    begin
    retval := _GetCypherDriverCyphers_v1(cypherDriver, cypherDriverDetails);
    end;

  Result:= retval;
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.ShowHashDetailsDlg(driverName: string; hashGUID: TGUID);
var
  detailsDlg: TfrmHashInfo;
begin
  detailsDlg:= TfrmHashInfo.Create(nil);
  try
    detailsDlg.OTFEFreeOTFEObj := self;
    detailsDlg.ShowDriverName:= driverName;
    detailsDlg.ShowGUID:= hashGUID;

    detailsDlg.ShowModal();
  finally
    detailsDlg.Free();
  end;

end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.ShowCypherDetailsDlg(driverName: string; cypherGUID: TGUID);
var
  detailsDlg: TfrmCypherInfo;
begin
  detailsDlg:= TfrmCypherInfo.Create(nil);
  try
    detailsDlg.OTFEFreeOTFEObj := self;
    detailsDlg.ShowDriverName:= driverName;
    detailsDlg.ShowGUID:= cypherGUID;

    detailsDlg.ShowModal();
  finally
    detailsDlg.Free();
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificHashDetails(hashKernelModeDeviceName: string; hashGUID: TGUID; var hashDetails: TFreeOTFEHash): boolean;
var
  i: integer;
  retval: boolean;
  hashDriverDetails: TFreeOTFEHashDriver;
begin
  retval := FALSE;

  if GetHashDriverHashes(hashKernelModeDeviceName, hashDriverDetails) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('low hashDriverDetails.Hashes: '+inttostr(low(hashDriverDetails.Hashes)));
DebugMsg('high hashDriverDetails.Hashes: '+inttostr(high(hashDriverDetails.Hashes)));
{$ENDIF}
    for i:=low(hashDriverDetails.Hashes) to high(hashDriverDetails.Hashes) do
      begin
      if (IsEqualGUID(hashDriverDetails.Hashes[i].HashGUID, hashGUID)) then
        begin
        hashDetails := hashDriverDetails.Hashes[i];
        retval := TRUE;
        break;
        end;

      end;

    end;

  Result := retval;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificCypherDetails(
  cypherKernelModeDeviceName: string;
  cypherGUID: TGUID;
  var cypherDetails: TFreeOTFECypher_v3
): boolean;
var
  i: integer;
  retval: boolean;
  cypherDriverDetails: TFreeOTFECypherDriver;
begin
  retval := FALSE;

  if GetCypherDriverCyphers(cypherKernelModeDeviceName, cypherDriverDetails) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('low cypherDriverDetails.Cyphers: '+inttostr(low(cypherDriverDetails.Cyphers)));
DebugMsg('high cypherDriverDetails.Cyphers: '+inttostr(high(cypherDriverDetails.Cyphers)));
{$ENDIF}
    for i:=low(cypherDriverDetails.Cyphers) to high(cypherDriverDetails.Cyphers) do
      begin
      if (IsEqualGUID(cypherDriverDetails.Cyphers[i].CypherGUID, cypherGUID)) then
        begin
        cypherDetails := cypherDriverDetails.Cyphers[i];
        retval := TRUE;
        break;
        end;

      end;

    end;

  Result := retval;

end;


// ----------------------------------------------------------------------------
// Encrypt data using the cypher/cypher driver identified
// Encrypted data returned in "plaintext"
function TOTFEFreeOTFEBase._EncryptData(
                     cypherKernelModeDeviceName: string;
                     cypherGUID: TGUID;
                     var key: string;
                     var IV: string;
                     var plaintext: string;
                     var cyphertext: string
                    ): boolean;
begin
  // Call generic function...
  Result := _EncryptDecryptData(
                               TRUE,
                               cypherKernelModeDeviceName,
                               cypherGUID,
                               key,
                               IV,
                               plaintext,
                               cyphertext
                              );
end;


// ----------------------------------------------------------------------------
// Decrypt data using the cypher/cypher driver identified
// Decrypted data returned in "plaintext"
function TOTFEFreeOTFEBase._DecryptData(
                     cypherKernelModeDeviceName: string;
                     cypherGUID: TGUID;
                     var key: string;
                     var IV: string;
                     var cyphertext: string;
                     var plaintext: string
                    ): boolean;
begin
  // Call generic function...
  Result := _EncryptDecryptData(
                               FALSE,
                               cypherKernelModeDeviceName,
                               cypherGUID,
                               key,
                               IV,
                               cyphertext,
                               plaintext
                              );
end;


// ----------------------------------------------------------------------------
// Encrypt data using the cypher/cypher driver identified
// Encrypted data returned in "plaintext"
function TOTFEFreeOTFEBase.EncryptSectorData(
                     cypherKernelModeDeviceName: string;
                     cypherGUID: TGUID;
                     SectorID: LARGE_INTEGER;
                     SectorSize: integer;
                     var key: string;
                     var IV: string;
                     var plaintext: string;
                     var cyphertext: string
                    ): boolean;
begin
  // Call generic function...
  Result := EncryptDecryptSectorData(
                               TRUE,
                               cypherKernelModeDeviceName,
                               cypherGUID,
                               SectorID,
                               SectorSize,
                               key,
                               IV,
                               plaintext,
                               cyphertext
                              );
end;


// ----------------------------------------------------------------------------
// Decrypt data using the cypher/cypher driver identified
// Decrypted data returned in "plaintext"
function TOTFEFreeOTFEBase.DecryptSectorData(
                     cypherKernelModeDeviceName: string;
                     cypherGUID: TGUID;
                     SectorID: LARGE_INTEGER;
                     SectorSize: integer;
                     var key: string;
                     var IV: string;
                     var cyphertext: string;
                     var plaintext: string
                    ): boolean;
begin
  // Call generic function...
  Result := EncryptDecryptSectorData(
                               FALSE,
                               cypherKernelModeDeviceName,
                               cypherGUID,
                               SectorID,
                               SectorSize,
                               key,
                               IV,
                               cyphertext,
                               plaintext
                              );
end;


// ----------------------------------------------------------------------------
// Note: Ignores any random padding data
function TOTFEFreeOTFEBase.ParseVolumeDetailsBlock(stringRep: string; var volumeDetailsBlock: TVolumeDetailsBlock): boolean;
const
  // CDB format 1 and 2 volume flags were used to identify sector IV generation
  // method
  VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV        = 1;  // Bit 0
  VOL_FLAGS_PRE_CDB_3__SECTOR_ID_ZERO_VOLSTART    = 2;  // Bit 1
  // Bit 2 unused
  VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE  = 8;  // Bit 3
var
  tmpByte: byte;
  tmpInt64: int64;
  tmpDWORD: DWORD;
  allOK: boolean;
  i: DWORD;
  idx: DWORD;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  allOK := TRUE;

  // The first byte in the string is at index 1
  idx := 1;

  // 8 bits: CDB Format ID...
  volumeDetailsBlock.CDBFormatID:= ord(stringRep[idx]);
  inc(idx, 1);


  // 32 bits: Volume flags...
  tmpDWORD := 0;
  for i:=1 to 4 do
    begin
    tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
    tmpByte := ord(stringRep[idx]);
    inc(idx);
    tmpDWORD := tmpDWORD + tmpByte;
    end;
  // CDBFormatID 1 CDBs (FreeOTFE v00.00.01 and v00.00.02) had a ***BUG*** that
  // caused the wrong value to be read back as the VolumeFlags
  if (volumeDetailsBlock.CDBFormatID < 2) then
    begin
    volumeDetailsBlock.VolumeFlags:= ord(stringRep[10]);
    end
  else
    begin
    volumeDetailsBlock.VolumeFlags:= tmpDWORD;
    end;


  // For CDB Format IDs less than 2, determine the sector IV generation method
  // based on the volume flags, and switch off the volume flags used
  if (volumeDetailsBlock.CDBFormatID < 3) then
    begin
    // VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV and VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE -> foivgHash64BitSectorID
    // 0                                        and VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE -> foivgUnknown (can't hash sector ID if it's not being used!)
    // VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV and 0                                              -> foivg64BitSectorID
    // 0                                        and 0                                              -> foivgNone

    volumeDetailsBlock.SectorIVGenMethod:= foivgUnknown;
    if (
         (
            volumeDetailsBlock.VolumeFlags AND
            (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV OR VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)
         ) =
            (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV OR VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)
       ) then
      begin
      volumeDetailsBlock.SectorIVGenMethod:= foivgHash64BitSectorID;
      end
    else if (
         (
            volumeDetailsBlock.VolumeFlags AND
            (0                                        OR VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)
         ) =
            (0                                        OR VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)
       ) then
      begin
      volumeDetailsBlock.SectorIVGenMethod:= foivgUnknown;
      end
    else if (
         (
            volumeDetailsBlock.VolumeFlags AND
            (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV OR 0                                             )
         ) =
            (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV OR 0                                             )
       ) then
      begin
      volumeDetailsBlock.SectorIVGenMethod:= foivg64BitSectorID;
      end
    else if (
         (
            volumeDetailsBlock.VolumeFlags AND
            (0                                        OR 0                                             )
         ) =
            (0                                        OR 0                                             )
       ) then
      begin
      volumeDetailsBlock.SectorIVGenMethod:= foivgNone;
      end;

    volumeDetailsBlock.VolumeFlags := volumeDetailsBlock.VolumeFlags AND
                                      not (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV OR VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE);
    end;


  // 64 bits: Partition length...
  tmpInt64 := 0;
  for i:=1 to 8 do
    begin
    tmpInt64 := tmpInt64 shl 8;  // Shift 8 bits to the *left*
    tmpByte := ord(stringRep[idx]);
    inc(idx);
    tmpInt64 := tmpInt64 + tmpByte;
    end;
  volumeDetailsBlock.PartitionLen := tmpInt64;


  // 32 bits: Master key length...
  tmpDWORD := 0;
  for i:=1 to 4 do
    begin
    tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
    tmpByte := ord(stringRep[idx]);
    inc(idx);
    tmpDWORD := tmpDWORD + tmpByte;
    end;
  volumeDetailsBlock.MasterKeyLength := tmpDWORD;
  // Note: If decrypted badly, can't rely on: criticalData.MasterKeyLen to be
  //       valid
  //       We check if the volume details block's master key length implies
  //       would put idx beyond the amount of data we have
  if ((idx + (volumeDetailsBlock.MasterKeyLength div 8)) > DWORD(length(stringRep))) then
    begin
    allOK := FALSE;
    end;

  // Variable bits: Master key...
  if (allOK) then
    begin
    volumeDetailsBlock.MasterKey:= Copy(stringRep, idx, (volumeDetailsBlock.MasterKeyLength div 8));
    inc(idx, (volumeDetailsBlock.MasterKeyLength div 8));
    end;


  // 8 bits: Requested drive letter...
  if (allOK) then
    begin
    volumeDetailsBlock.RequestedDriveLetter := stringRep[idx];
    inc(idx);
    end;


  // If the CDB Format ID is 2 or more, the CDB has a volume IV block
  // For CDB Format IDs less than 2, use a NULL volume IV
  if (volumeDetailsBlock.CDBFormatID < 2) then
    begin
    volumeDetailsBlock.VolumeIVLength := 0;
    volumeDetailsBlock.VolumeIV := '';
    end
  else
    begin
    // 32 bits: Volume IV length...
    tmpDWORD := 0;
    for i:=1 to 4 do
      begin
      tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
      tmpByte := ord(stringRep[idx]);
      inc(idx);
      tmpDWORD := tmpDWORD + tmpByte;
      end;
    volumeDetailsBlock.VolumeIVLength := tmpDWORD;
    // Note: If decrypted badly, can't rely on: criticalData.MasterKeyLen to be
    //       valid
    //       We check if the volume details block's master key length implies
    //       would put idx beyond the amount of data we have
    if ((idx + (volumeDetailsBlock.VolumeIVLength div 8)) > DWORD(length(stringRep))) then
      begin
      allOK := FALSE;
      end;

    // Variable bits: Master key...
    if (allOK) then
      begin
      volumeDetailsBlock.VolumeIV:= Copy(stringRep, idx, (volumeDetailsBlock.VolumeIVLength div 8));
      inc(idx, (volumeDetailsBlock.VolumeIVLength div 8));
      end;

    end;


  // For CDB Format IDs less than 2, determine the sector IV generation method
  // based on the volume flags, and reset the volume flags
  if (volumeDetailsBlock.CDBFormatID < 3) then
    begin
    // volumeDetailsBlock.SectorIVGenMethod already set above when by processing VolumeFlags
    end
  else
    begin
    // 8 bits: Sector IV generation method...
    tmpByte:= ord(stringRep[idx]);
// NEXT LINE COMMENTED OUT TO PREVENT COMPILER WARNING
//    inc(idx, 1);
    volumeDetailsBlock.SectorIVGenMethod := foivgUnknown;
    for tmpSectorIVGenMethod := low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do
      begin
      if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = tmpByte) then
        begin
        volumeDetailsBlock.SectorIVGenMethod:= tmpSectorIVGenMethod;
        end;
      end;

    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Note: This function does *not* append random padding data
// Note that volumeDetails.CDBFormatID is ignored - it is force set to the latest value
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
// IF YOU UPDATE THIS, YOU SHOULD PROBABLY ALSO CHANGE THE DEFINITION OF:
//   function BuildVolumeDetailsBlock(...)
//   function ParseVolumeDetailsBlock(...)
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
function TOTFEFreeOTFEBase.BuildVolumeDetailsBlock(volumeDetailsBlock: TVolumeDetailsBlock; var stringRep: string): boolean;
var
  tmpDWORD: DWORD;
  tmpInt64: int64;
  tmpByte: byte;
  i: integer;
  allOK: boolean;
  tmpString: string;
begin
  allOK := TRUE;

  volumeDetailsBlock.CDBFormatID := CDB_FORMAT_ID;

  stringRep := '';

  // 8 bits: CDB Format ID...
  stringRep := stringRep + char(volumeDetailsBlock.CDBFormatID);


  // 32bits: Volume flags...
  tmpDWORD := volumeDetailsBlock.VolumeFlags;
  tmpString:= '';
  for i:=1 to 4 do
    begin
    tmpByte := (tmpDWORD AND $FF);
    tmpDWORD := tmpDWORD shr 8;  // Shift 8 bits to the *right*
    tmpString := char(tmpByte) + tmpString;
    end;
  stringRep := stringRep + tmpString;


  // 64 bits: Partition length...
  tmpInt64 := volumeDetailsBlock.PartitionLen;
  tmpString:= '';
  for i:=1 to 8 do
    begin
    tmpByte := (tmpInt64 AND $FF);
    tmpInt64 := tmpInt64 shr 8;  // Shift 8 bits to the *right*
    tmpString := char(tmpByte) + tmpString;
    end;
  stringRep := stringRep + tmpString;


  // 32 bits: Master key length...
  tmpDWORD := volumeDetailsBlock.MasterKeyLength;
  tmpString:= '';
  for i:=1 to 4 do
    begin
    tmpByte := (tmpDWORD AND $FF);
    tmpDWORD := tmpDWORD shr 8;  // Shift 8 bits to the *right*
    tmpString := char(tmpByte) + tmpString;
    end;
  stringRep := stringRep + tmpString;


  // Variable bits: Master key...
  stringRep := stringRep + Copy(volumeDetailsBlock.MasterKey, 1, (volumeDetailsBlock.MasterKeyLength div 8));


  // Requested drive letter...
  stringRep := stringRep + volumeDetailsBlock.RequestedDriveLetter;


  if (volumeDetailsBlock.CDBFormatID >= 2) then
    begin
    // 32 bits: Volume IV length...
    tmpDWORD := volumeDetailsBlock.VolumeIVLength;
    tmpString:= '';
    for i:=1 to 4 do
      begin
      tmpByte := (tmpDWORD AND $FF);
      tmpDWORD := tmpDWORD shr 8;  // Shift 8 bits to the *right*
      tmpString := char(tmpByte) + tmpString;
      end;
    stringRep := stringRep + tmpString;


    // Variable bits: Volume IV ...
    stringRep := stringRep + Copy(volumeDetailsBlock.VolumeIV, 1, (volumeDetailsBlock.VolumeIVLength div 8));

    
    if (volumeDetailsBlock.CDBFormatID >= 3) then
      begin
      // 8 bits: Sector IV generation method...
      stringRep := stringRep + char(FreeOTFESectorIVGenMethodID[volumeDetailsBlock.SectorIVGenMethod]);
      end;

    end;


  Result := allOK;
end;


// ----------------------------------------------------------------------------
// !!! WARNING !!!
// THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
//   Cypher keysizes (in bits)
//   Cypher blocksizes (in bits)
//   Hash lengths (in bits)
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
// IF YOU UPDATE THIS, YOU SHOULD PROBABLY ALSO CHANGE THE DEFINITION OF:
//   function BuildVolumeDetailsBlock(...)
//   function ParseVolumeDetailsBlock(...)
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
function TOTFEFreeOTFEBase.ReadVolumeCriticalData(
    filename: string;
    offsetWithinFile: int64;
    userPassword: string;
    saltLength: integer;  // In bits
    keyIterations: integer;
    
    var volumeDetails: TVolumeDetailsBlock;
    var CDBMetaData: TCDBMetaData
    ): boolean;
var
  allOK: boolean;
  CDB: string;
  prevCursor: TCursor;
begin
  prevCursor:= Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    allOK := ReadRawVolumeCriticalData(
                                      filename,
                                      offsetWithinFile,
                                      CDB
                                     );

    if allOK then
      begin
      allOK := ReadVolumeCriticalData_CDB(
                                        CDB,
                                        userPassword,
                                        saltLength,
                                        keyIterations,
                                        volumeDetails,
                                        CDBMetaData
                                        );
      end;
  finally
    Screen.Cursor := prevCursor;
  end;

  Result:= allOK;
end;


// ----------------------------------------------------------------------------
// !!! WARNING !!!
// THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
//   Cypher keysizes (in bits)
//   Cypher blocksizes (in bits)
//   Hash lengths (in bits)
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
// IF YOU UPDATE THIS, YOU SHOULD PROBABLY ALSO CHANGE THE DEFINITION OF:
//   function BuildVolumeDetailsBlock(...)
//   function ParseVolumeDetailsBlock(...)
// !!!!!!!!!!!!!!!!
// !!!  WARNING !!!
// !!!!!!!!!!!!!!!!
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB(
    CDB: string;
    userPassword: string;
    saltLength: integer;  // In bits
    keyIterations: integer;
    
    var volumeDetails: TVolumeDetailsBlock;
    var CDBMetaData: TCDBMetaData
    ): boolean;
var
  allOK: boolean;
begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Attempting v2 format CDB decode...');
{$ENDIF}
  allOK := ReadVolumeCriticalData_CDB_v2(
                                    CDB,
                                    userPassword,
                                    saltLength,
                                    keyIterations,
                                    volumeDetails,
                                    CDBMetaData
                                    );

  if not(allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('v2 format CDB decode failed - attempting v1...');
{$ENDIF}
    allOK := ReadVolumeCriticalData_CDB_v1(
                                    CDB,
                                    userPassword,
                                    saltLength,
                                    volumeDetails,
                                    CDBMetaData
                                    );
    end;


  if (allOK) then
    begin
    // Warn user if the volume appears to be a later version that this software
    // supports
    if (volumeDetails.CDBFormatID > CDB_FORMAT_ID) then
      begin
      SDUMessageDlg(
                 _('WARNING: This volume appears to have been created with a later version of FreeOTFE than this software supports.'+SDUCRLF+
                 SDUCRLF+
                 'You may continue, however:'+SDUCRLF+
                 '1) Your data may be read incorrectly due to changes in the decryption process, and'+SDUCRLF+
                 '2) It is not recommended that you write to this volume using this software.'),
                 mtWarning);
      end;

    end;

  Result:= allOK;
end;


// ----------------------------------------------------------------------------
// Version 1 volumes:
//   *) Used the hash of the user's password + salt as the CDB
//      encryption/decryption key
//   *) Used the hash of the volume details block as the check data
// !!! WARNING !!!
// THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
//   Cypher keysizes (in bits)
//   Cypher blocksizes (in bits)
//   Hash lengths (in bits)
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v1(
    CDB: string;
    userPassword: string;
    saltLength: integer;  // In bits

    var volumeDetails: TVolumeDetailsBlock;
    var CDBMetaData: TCDBMetaData
    ): boolean;
var
  validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaData: TCDBMetaDataArray;

  encryptedBlockLen: integer;  // In *bits*
  encryptedBlock: string;
  decryptedBlock: string;
  strVolumeDetailsBlock: string;
  hashDrivers: array of TFreeOTFEHashDriver;
  hi, hj: integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHashImpl: TFreeOTFEHash;
  cypherDrivers: array of TFreeOTFECypherDriver;
  ci, cj: integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypherImpl: TFreeOTFECypher_v3;
  allOK: boolean;
  salt: string;
  saltedUserPassword: string;
  derivedKey: string;  // The key to be used in decrypting the CDB, after
                       // salting, hashing, HMACing, etc - but before any
                       // truncating/padding as a result of the cypher's keysize
  hk: integer;  // Hash value length, in *bits*
  ks: integer;  // Cypher keysize, in *bits*
  bs: integer;  // Cypher blocksize, in *bits*
  criticalDataKey: string;
  IV: string;
  checkDataDecrypted: string;
  checkDataGenerated: string;
  strDecryptedVolDetailsBlk: string;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData: TCDBMetaData;

  i: integer;
  tgtVolDetailsBlockLen: integer;  // In *bits*
  prevCursor: TCursor;
begin
  SetLength(validVolumeDetails, 0);
  SetLength(validCDBMetaData, 0);


  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // NOTE: "allOK" indicates a *fatal* error in processing; not that an invalid
    //       password was entered
    allOK := TRUE;


    // Append the salt onto the user's password
    if (allOK) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Determining user''s salted password...');
  {$ENDIF}
      salt := Copy(CDB, 1, (saltLength div 8));
      saltedUserPassword := userPassword + salt;
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('User''s salted key:');
  DebugMsg('-- begin --');
  DebugMsgBinary(saltedUserPassword);
  DebugMsg('-- end --');
  {$ENDIF}
      end;

    // Obtain details of all hashes...
    if (allOK) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting hash drivers...');
  {$ENDIF}
      SetLength(hashDrivers, 0);
      allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
      end;

    // Obtain details of all cyphers...
    if (allOK) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting cypher drivers...');
  {$ENDIF}
      SetLength(cypherDrivers, 0);
      allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
      end;


    if (allOK) then
      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Starting main hashes loop...');
  {$ENDIF}
      // FOR ALL HASH DRIVERS...
      for hi:=low(hashDrivers) to high(hashDrivers) do
        begin
        currHashDriver := hashDrivers[hi];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using hash driver: '+currHashDriver.Title+' ('+GUIDToString(currHashDriver.DriverGUID)+')');
  {$ENDIF}
        // FOR ALL HASH ALGORITHMS SUPPORTED BY THE CURRENT DRIVER...
        for hj:=low(hashDrivers[hi].Hashes) to high(hashDrivers[hi].Hashes) do
          begin
          currHashImpl := currHashDriver.Hashes[hj];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using hash impl: '+GetHashDisplayTechTitle(currHashImpl));
  {$ENDIF}
          hk := currHashImpl.Length;
          if (hk < 0) then
            begin
            // Assume 512 bits for the hash length, if the hash supports arbitary
            // lengths
            hk := 512;
            end;


  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('hk: '+inttostr(hk));
  DebugMsg('About to derive key from user''s password and salt');
  {$ENDIF}

          // HASH THE USER's PASSWORD+SALT...
          if not(HashData(currHashDriver.LibFNOrDevKnlMdeName, currHashImpl.HashGUID, saltedUserPassword, derivedKey)) then
            begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Hashing FAILED.');
  {$ENDIF}
            allOK := FALSE;
            end;


          if (allOK) then
            begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Hashed data OK.');
  DebugMsg('Processed user''s password and salt to give:');
  DebugMsgBinary(derivedKey);
  {$ENDIF}
            // FOR ALL CYPHER DRIVERS...
            for ci:=low(cypherDrivers) to high(cypherDrivers) do
              begin
              currCypherDriver := cypherDrivers[ci];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
  {$ENDIF}
              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
              for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
                begin
                currCypherImpl := currCypherDriver.Cyphers[cj];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
  {$ENDIF}

                // Determine keysize...
                ks := currCypherImpl.KeySizeRequired;  // In *bits*
                // Determine blocksize...
                bs := currCypherImpl.BlockSize;
                if (bs <= 0) then
                  begin
                  // Assume 8 bits for the blocksize, if the cypher supports arbitary
                  // block sizes
                  bs := 8;
                  end;

                // Extract the encrypted block from the critical data block
                encryptedBlockLen := (((CRITICAL_DATA_LENGTH - saltLength) div bs) * bs);
                // +1 because we index from 1, not zero
                encryptedBlock := Copy(CDB, ((saltLength div 8)+1), (encryptedBlockLen div 8));



                // Determine the criticalDataKey...
                // Fallback - assign no key (e.g. if the cypher's keysize = 0 bits)
                criticalDataKey := '';
                if (ks < 0) then
                  begin
                  // If the cypher's keysize is -1, then the critical data block
                  // en/decryption key is the hash value returned
                  criticalDataKey := derivedKey;
                  end
                else
                  begin
                  if ((Length(derivedKey) * 8) < ks) then
                    begin
                    // The hash value isn't long enough; right pad with zero bytes
                    criticalDataKey := derivedKey + StringOfChar(#0, ((ks div 8) - Length(derivedKey)));
                    end
                  else
                    begin
                    // The hash value is too long; truncate
                    criticalDataKey := Copy(derivedKey, 1, (ks div 8));
                    end;

                  end;  // if (ks > 0) then



                // Zero the IV for decryption
                IV := StringOfChar(#0, (bs div 8));

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('About to decrypt data...');
  {$ENDIF}
                if not(DecryptSectorData(
                                   currCypherDriver.LibFNOrDevKnlMdeName,
                                   currCypherImpl.CypherGUID,
                                   FREEOTFE_v1_DUMMY_SECTOR_ID,
                                   FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                   criticalDataKey,
                                   IV,
                                   encryptedBlock,
                                   decryptedBlock
                                  )) then
                  begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Decryption FAILED.');
  {$ENDIF}
                  allOK := FALSE;
                  end
                else
                  begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Key:');
  DebugMsgBinary(criticalDataKey);
  DebugMsg('Decoded to:');
  DebugMsgBinary(decryptedBlock);
  {$ENDIF}
                  // Extract the decrypted check hash and volume details block
                  // 1 and +1 because we index from 1
                  checkDataDecrypted    := Copy(decryptedBlock, 1, (hk div 8));
                  tgtVolDetailsBlockLen := (((4096 - saltLength) div bs) * bs) - hk;
                  strVolumeDetailsBlock := Copy(decryptedBlock, ((hk div 8)+1), (tgtVolDetailsBlockLen div 8));
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('About to generate hash/HMAC using decoded data...');
  {$ENDIF}


                  // HASH THE USER DECODED DATA...
                  if not(HashData(currHashDriver.LibFNOrDevKnlMdeName, currHashImpl.HashGUID, strVolumeDetailsBlock, checkDataGenerated)) then
                    begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Hash of decrypted data FAILED.');
  {$ENDIF}
                    allOK := FALSE;
                    end;


                  if (allOK) then
                    begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Decrypted data hashed OK.');
  {$ENDIF}
                    // If the checkdata newly generated is less than hk bits, right pad it with zero bits
                    if ((Length(checkDataGenerated) * 8) < hk) then
                      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash less than hk bits; padding...');
  {$ENDIF}
                      checkDataGenerated := checkDataGenerated + StringOfChar(#0, (((Length(checkDataGenerated) * 8) - hk) div 8));
                      end;
                    // If the new checkdata is greater than hk bits, truncate it after the first
                    // hk bits
                    if ((Length(checkDataGenerated) * 8) > hk) then
                      begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash more than hk bits; padding...');
  {$ENDIF}
                      // +1 because we index from 1, not zero
                      Delete(checkDataGenerated, (hk div 8)+1, (Length(checkDataGenerated) - (hk div 8)));
                      end;


                    if (checkDataDecrypted = checkDataGenerated) then
                      begin
                      // We have found the hash and cypher to successfully decrypt!

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('SUCCESS! Found valid hash/cypher combination!');
  DebugMsg('Volume details block is length: '+inttostr(length(strVolumeDetailsBlock)));
  {$ENDIF}
                      if not(ParseVolumeDetailsBlock(strVolumeDetailsBlock, currVolumeDetails)) then
                        begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: FAILED to parse plaintext volume details block to structure');
  {$ENDIF}
                        // At this point, there has been some failure (e.g.
                        // critical data block version ID or block structure
                        // incorrect)
                        // Note that we *don't* set allOK to FALSE; this failure
                        // is valid and only indicates that the current
                        // cypher/hash combination are incorrect for the volume;
                        // not that something has gone wrong with the mount
                        // operation
                        end
                      else
                        begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('OK so far...');
  {$ENDIF}
                        // Sanity check...
                        if
                           (
                            (ks >= 0) AND
                            (currVolumeDetails.MasterKeyLength <> DWORD(ks))
                           ) then
                          begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Sanity check failed(!)');
  {$ENDIF}
                          // Skip to next loop iteration...
                          Continue;
                          end;

                        // Add the decrypted critical data area onto an array
                        // containing other successfully decrypted copies,
                        // together with the hash/cypher combination successfully used



                        // v1 volumes only use hashed for MAC and hash of
                        // user's salted password as KDF
                        currCDBMetaData.MACAlgorithm := fomacHash;
                        currCDBMetaData.KDFAlgorithm := fokdfHashWithSalt;

                        currCDBMetaData.CypherDriver := cypherDrivers[ci].LibFNOrDevKnlMdeName;
                        currCDBMetaData.CypherGUID   := cypherDrivers[ci].Cyphers[cj].CypherGUID;
                        currCDBMetaData.HashDriver   := hashDrivers[hi].LibFNOrDevKnlMdeName;
                        currCDBMetaData.HashGUID     := hashDrivers[hi].Hashes[hj].HashGUID;
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Got valid decryption cypher/hash combination');
  {$ENDIF}
                        SetLength(validVolumeDetails, (Length(validVolumeDetails)+1));
                        SetLength(validCDBMetaData, (Length(validCDBMetaData)+1));
                        validVolumeDetails[Length(validVolumeDetails)-1] := currVolumeDetails;
                        validCDBMetaData[Length(validCDBMetaData)-1] := currCDBMetaData;
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('OK.');
  {$ENDIF}

                        // Additional: Store internal information, if dumping
                        if DumpFlag then
                          begin
                          DumpCriticalDataKey        := criticalDataKey;
                          DumpCheckMAC               := checkDataDecrypted;
                          DumpPlaintextEncryptedBlock:= decryptedBlock;
                          DumpVolumeDetailsBlock     := strVolumeDetailsBlock;
                          end;

                        end;  // ELSE PART - if not(ParseVolumeDetailsBlock(strVolumeDetailsBlock, currVolumeDetails)) then

                      end
                    else
                      begin
                      // Do nothing - just loop around for the next one...
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash and generated hash do not match.');
  {$ENDIF}
                      end;  // ELSE PART - if (checkDataDecrypted = checkDataGenerated) then

                    end;  // ELSE PART - if not(HashData(...

                  end;  // ELSE PART - if not(DecryptData(...

                end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
              end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
            end;  // ELSE PART - if (HashData(...

          end;  // for hj:=low(hashDrivers[hi].Hashes) to high(hashDrivers[hi].Hashes) do

        end;  // for hi:=low(hashDrivers) to high(hashDrivers) do

      end;  // if (allOK) then


  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Completed checking for all hash/cypher combinations');
  {$ENDIF}


  finally
    Screen.Cursor := prevCursor;
  end;


  // Depending on how many hash/cypher combinations were found, select the
  // appropriate one to use
  if (allOK) then
    begin
    allOK := ChooseFromValid(
                             validVolumeDetails,
                             validCDBMetaData,
                             volumeDetails,
                             CDBMetaData
                            );
    end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Finished function.');
{$ENDIF}

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Version 2 volumes:
//   *) Used the PKCS#5 PBKDF2 key derivation function with the user's password
//      and salt to generate thethe CDB encryption/decryption key
//   *) Used the HMAC of the (derived key, volume details block) as the check data
// !!! WARNING !!!
// THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
//   Cypher keysizes (in bits)
//   Cypher blocksizes (in bits)
//   Hash lengths (in bits)
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v2(
    CDB: string;
    userPassword: string;
    saltLength: integer;  // In bits
    keyIterations: integer;

    var volumeDetails: TVolumeDetailsBlock;
    var CDBMetaData: TCDBMetaData
    ): boolean;
var
  validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaData: TCDBMetaDataArray;

  hashDrivers: array of TFreeOTFEHashDriver;
  hi, hj: integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHashImpl: TFreeOTFEHash;

  cypherDrivers: array of TFreeOTFECypherDriver;
  ci, cj: integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypherImpl: TFreeOTFECypher_v3;

  allOK: boolean;
  salt: string;
  derivedKey: string;  // The key to be used in decrypting the CDB, after
                       // salting, hashing, HMACing, etc - but before any
                       // truncating/padding as a result of the cypher's keysize
  cdkSize_bits: integer;  // Size of the "critical data key", in *bits*
  maxCDKSize_bits: integer;  // Max size of the "critical data key", in *bits*
  IV: string;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData: TCDBMetaData;

  i: integer;
  leb_bits: integer;  // In *bits*
  criticalDataKey: string;
  maxCriticalDataKey: string;

  paddedEncryptedBlock: string;
  encryptedBlock: string;
  plaintextEncryptedBlock: string;

  paddedCheckMAC: string;
  checkMAC: string;
  generatedMAC: string;

  volumeDetailsBlock: string;

  MACAlgorithm: TFreeOTFEMACAlgorithm;
  KDFAlgorithm: TFreeOTFEKDFAlgorithm;
  prevCursor: TCursor;
begin
  SetLength(validVolumeDetails, 0);
  SetLength(validCDBMetaData, 0);


  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    MACAlgorithm:= fomacHMAC;
    KDFAlgorithm:= fokdfPBKDF2;

    // NOTE: "allOK" indicates whether or not a *fatal* error in processing;
    //       NOT that an invalid password was entered
    allOK := TRUE;


    // Obtain details of all hashes...
    if (allOK) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Getting hash drivers...');
{$ENDIF}
      SetLength(hashDrivers, 0);
      allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
      end;


    // Obtain details of all cyphers...
    if (allOK) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Getting cypher drivers...');
{$ENDIF}
      SetLength(cypherDrivers, 0);
      allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
      end;


    // Strip off salt
    if (allOK) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Extracting salt...');
{$ENDIF}
      salt := Copy(CDB, 1, (saltLength div 8));
      paddedEncryptedBlock := Copy(
                                   CDB,
                                   ((saltLength div 8)+1),
                                   (Length(CDB) - (saltLength div 8))
                                  );
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Salt follows:');
DebugMsgBinary(salt);
{$ENDIF}
      end;



    // !!! OPTIMISATION !!!
    // Determine the longest cypher key
    if (allOK) then
      begin
      maxCDKSize_bits := 0;

      // FOR ALL CYPHER DRIVERS...
      for ci:=low(cypherDrivers) to high(cypherDrivers) do
        begin
        currCypherDriver := cypherDrivers[ci];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
{$ENDIF}
        // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
        for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
          begin
          currCypherImpl := currCypherDriver.Cyphers[cj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
{$ENDIF}

          // Determine size of "critical data key"...
          cdkSize_bits := currCypherImpl.KeySizeRequired;  // In *bits*
          if (cdkSize_bits < 0) then
            begin
            cdkSize_bits := 512;
            end;

          maxCDKSize_bits := max(maxCDKSize_bits, cdkSize_bits);

          end;
        end;
      end;



    if (allOK) then
      begin
      try
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Starting main hashes loop...');
{$ENDIF}
        // FOR ALL HASH DRIVERS...
        for hi:=low(hashDrivers) to high(hashDrivers) do
          begin
          currHashDriver := hashDrivers[hi];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using hash driver: '+currHashDriver.Title+' ('+GUIDToString(currHashDriver.DriverGUID)+')');
{$ENDIF}
          // FOR ALL HASH ALGORITHMS SUPPORTED BY THE CURRENT DRIVER...
          for hj:=low(currHashDriver.Hashes) to high(currHashDriver.Hashes) do
            begin
            currHashImpl := currHashDriver.Hashes[hj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using hash impl: '+GetHashDisplayTechTitle(currHashImpl));
{$ENDIF}


            // v2 volumes won't work with hash algorithms with <= 0 length, or
            // blocksize, as HMAC/PBKDF2 require these to be fixed
            //  - skip these, and move onto the next hash algorithm (if any)
            if (
                (currHashImpl.Length <= 0) OR
                (currHashImpl.BlockSize <= 0)
               ) then
              begin
              Continue;
              end;

            // !!! OPTIMISATION !!!
            // Because we only allow PBKDF2, which:
            //   a) Doesn't need a cypher
            //   b) Short keys are identical to truncated longer keys
            // We derive the key *here*, outside the cypher loop
            if (KDFAlgorithm <> fokdfPBKDF2) then
              begin
              raise EFreeOTFEInternalError.Create('PBKDF2 assumed - optimisation failed');
              end;

            allOK := DeriveKey(
                            KDFAlgorithm,
                            currHashDriver.LibFNOrDevKnlMdeName,
                            currHashImpl.HashGUID,
                            '',
                            StringToGUID(NULL_GUID),
                            userPassword,
                            salt,
                            keyIterations,
                            maxCDKSize_bits,  // cdkSizeBits
                            maxCriticalDataKey
                           );
            if not(allOK) then
              begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg(
     'FAILED TO DERIVE CRITICAL DATA KEY:'+SDUCRLF+
     'Hash:'+SDUCRLF+
     currHashImpl.Title+' ('+GUIDToString(currHashImpl.HashGUID)+')'+SDUCRLF+
     'Cypher:'+SDUCRLF+
     currCypherImpl.Title+' ('+GUIDToString(currCypherImpl.CypherGUID)+')'
    );
{$ENDIF}
              SDUMessageDlg(
                         _('Failed to derive critical data key.')+SDUCRLF+
                         SDUCRLF+
                         _('Hash:')+SDUCRLF+
                         '  '+currHashDriver.LibFNOrDevKnlMdeName+SDUCRLF+
                         '  '+GUIDToString(currHashImpl.HashGUID),
                         mtError
                        );
              // Bail out...
              raise EFreeOTFEReadCDBFailure.Create('Failed to derive key');
              end;


            // FOR ALL CYPHER DRIVERS...
            for ci:=low(cypherDrivers) to high(cypherDrivers) do
              begin
              currCypherDriver := cypherDrivers[ci];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
{$ENDIF}
              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
              for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
                begin
                currCypherImpl := currCypherDriver.Cyphers[cj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
{$ENDIF}

                // Determine size of "critical data key"...
                cdkSize_bits := currCypherImpl.KeySizeRequired;  // In *bits*
                if (cdkSize_bits < 0) then
                  begin
                  cdkSize_bits := 512;
                  end;


                // !!! OPTIMISATION !!!
                criticalDataKey := Copy(maxCriticalDataKey, 1, (cdkSize_bits div 8));


                // Calculate "leb"
                if (currCypherImpl.BlockSize > 8) then
                  begin
                  leb_bits := ((CRITICAL_DATA_LENGTH - saltLength) div currCypherImpl.BlockSize) * currCypherImpl.BlockSize;
                  end
                else
                  begin
                  leb_bits := (CRITICAL_DATA_LENGTH - saltLength);
                  end;

                // Strip off the padding after the encrypted block
                encryptedBlock := Copy(paddedEncryptedBlock, 1, (leb_bits div 8));

                // Zero the IV for decryption
                IV := '';
                if (currCypherImpl.BlockSize > 0) then
                  begin
                  IV := StringOfChar(#0, (currCypherImpl.BlockSize div 8));
                  end;

                allOK := DecryptSectorData(
                                     currCypherDriver.LibFNOrDevKnlMdeName,
                                     currCypherImpl.CypherGUID,
                                     FREEOTFE_v1_DUMMY_SECTOR_ID,
                                     FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                     criticalDataKey,
                                     IV,
                                     encryptedBlock,
                                     plaintextEncryptedBlock
                                    );
                if not(allOK) then
                  begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg(
         'FAILED TO DECRYPT ENCRYPTED BLOCK:'+SDUCRLF+
         'Hash:'+SDUCRLF+
         currHashImpl.Title+' ('+GUIDToString(currHashImpl.HashGUID)+')'+SDUCRLF+
         'Cypher:'+SDUCRLF+
         currCypherImpl.Title+' ('+GUIDToString(currCypherImpl.CypherGUID)+')'
        );
{$ENDIF}
                  SDUMessageDlg(
                             _('Failed to decrypt encrypted block.')+SDUCRLF+
                             SDUCRLF+
                             _('Cypher:')+SDUCRLF+
                             '  '+currCypherDriver.LibFNOrDevKnlMdeName+SDUCRLF+
                             '  '+GUIDToString(currCypherImpl.CypherGUID),
                             mtError
                            );
                  // Bail out...
                  raise EFreeOTFEReadCDBFailure.Create('Failed to decrypt encrypted block');
                  end;


                paddedCheckMAC := Copy(plaintextEncryptedBlock, 1, (CDB_MAX_MAC_LENGTH div 8));
                volumeDetailsBlock := Copy(
                                           plaintextEncryptedBlock,
                                           ((CDB_MAX_MAC_LENGTH div 8)+1),
                                           (Length(plaintextEncryptedBlock) - (CDB_MAX_MAC_LENGTH div 8))
                                          );

                allOK := MACData(
                                  MACAlgorithm,
                                  currHashDriver.LibFNOrDevKnlMdeName,
                                  currHashImpl.HashGUID,
                                  currCypherDriver.LibFNOrDevKnlMdeName,
                                  currCypherImpl.CypherGUID,
                                  criticalDataKey,
                                  volumeDetailsBlock,
                                  generatedMAC,
                                  -1
                                 );
                if not(allOK) then
                  begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg(
         'FAILED TO GENERATE MAC:'+SDUCRLF+
         'Hash:'+SDUCRLF+
         currHashImpl.Title+' ('+GUIDToString(currHashImpl.HashGUID)+')'+SDUCRLF+
         'Cypher:'+SDUCRLF+
         currCypherImpl.Title+' ('+GUIDToString(currCypherImpl.CypherGUID)+')'
        );
{$ENDIF}
                  SDUMessageDlg(
                             _('Failed to generate check MAC.')+SDUCRLF+
                             _('Hash:')+SDUCRLF+
                             '  '+currHashDriver.LibFNOrDevKnlMdeName+SDUCRLF+
                             '  '+GUIDToString(currHashImpl.HashGUID)+SDUCRLF+
                             _('Cypher:')+SDUCRLF+
                             '  '+currCypherDriver.LibFNOrDevKnlMdeName+SDUCRLF+
                             '  '+GUIDToString(currCypherImpl.CypherGUID)+SDUCRLF,
                             mtError
                            );
                  // Bail out...
                  raise EFreeOTFEReadCDBFailure.Create('Failed to generate MAC');
                  end;


                // Truncate generated MAC to CDB_MAX_MAC_LENGTH bits, if it's too
                // long
                // Note that we can't do this by specifying a MAC length to
                // MACData(...) as this would right-pad the MAC with 0x00 if it
                // wasn't long enough
                if (length(generatedMAC) > (CDB_MAX_MAC_LENGTH * 8)) then
                  begin
                  // +1 because we index from 1, not zero
                  Delete(
                         generatedMAC,
                         ((CDB_MAX_MAC_LENGTH * 8)+1),
                         (length(generatedMAC) - (CDB_MAX_MAC_LENGTH * 8))
                        );
                  end;

                checkMAC := Copy(paddedCheckMAC, 1, Length(generatedMAC));

                // Check if we've found a valid hash/cypher combination
                if (checkMAC = generatedMAC) then
                  begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('SUCCESS! Found valid hash/cypher combination!');
DebugMsg('Volume details block is length: '+inttostr(length(volumeDetailsBlock)));
{$ENDIF}

                  if not(ParseVolumeDetailsBlock(volumeDetailsBlock, currVolumeDetails)) then
                    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: FAILED to parse plaintext volume details block to structure');
{$ENDIF}
                    // At this point, there has been some failure (e.g.
                    // critical data block version ID or block structure
                    // incorrect)
                    // Note that we *don't* set allOK to FALSE; this failure
                    // is valid and only indicates that the current
                    // cypher/hash combination are incorrect for the volume;
                    // not that something has gone wrong with the mount
                    // operation
                    end
                  else
                    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('OK so far...');
{$ENDIF}
                    // Sanity check...
                    if
                       (
                        (currCypherImpl.KeySizeRequired >= 0) AND
                        (currVolumeDetails.MasterKeyLength <> DWORD(currCypherImpl.KeySizeRequired))
                       ) then
                      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: Sanity check failed(!)');
{$ENDIF}
                      // Skip to next loop iteration...
                      Continue;
                      end;


                    // Add the decrypted critical data area onto an array
                    // containing other successfully decrypted copies,
                    // together with the hash/cypher combination successfully used



                    // v2 volumes only use HMAC for MAC and PBKDF2 of
                    // user's salted password as KDF
                    currCDBMetaData.MACAlgorithm := MACAlgorithm;
                    currCDBMetaData.KDFAlgorithm := KDFAlgorithm;

                    currCDBMetaData.CypherDriver := cypherDrivers[ci].LibFNOrDevKnlMdeName;
                    currCDBMetaData.CypherGUID   := cypherDrivers[ci].Cyphers[cj].CypherGUID;
                    currCDBMetaData.HashDriver   := hashDrivers[hi].LibFNOrDevKnlMdeName;
                    currCDBMetaData.HashGUID     := hashDrivers[hi].Hashes[hj].HashGUID;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Got valid decryption cypher/hash combination');
{$ENDIF}
                    SetLength(validVolumeDetails, (Length(validVolumeDetails)+1));
                    SetLength(validCDBMetaData, (Length(validCDBMetaData)+1));
                    validVolumeDetails[Length(validVolumeDetails)-1] := currVolumeDetails;
                    validCDBMetaData[Length(validCDBMetaData)-1] := currCDBMetaData;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('OK.');
{$ENDIF}

                    // Additional: Store internal information, if dumping
                    if DumpFlag then
                      begin
                      DumpCriticalDataKey        := criticalDataKey;
                      DumpCheckMAC               := checkMAC;
                      DumpPlaintextEncryptedBlock:= plaintextEncryptedBlock;
                      DumpVolumeDetailsBlock     := volumeDetailsBlock;
                      end;

                    end;  // ELSE PART - if not(ParseVolumeDetailsBlock(volumeDetailsBlock, currVolumeDetails)) then

                  end
                else
                  begin
                  // Do nothing - just loop around for the next one...
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Check hash and generated hash do not match.');
{$ENDIF}
                  end;  // ELSE PART - if (checkMAC = generatedMAC) then

                end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do

              end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do

            end; // for hj:=low(hashDrivers[hi].Hashes) to high(hashDrivers[hi].Hashes) do

          end;  // for hi:=low(hashDrivers) to high(hashDrivers) do

      except
        on EFreeOTFEReadCDBFailure do
          begin
          allOK := FALSE;
          end;

      end;

      end;  // if (allOK) then


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Completed checking for all hash/cypher combinations');
{$ENDIF}


  finally
    Screen.Cursor := prevCursor;
  end;


  // Depending on how many hash/cypher combinations were found, select the
  // appropriate one to use
  if (allOK) then
    begin
    allOK := ChooseFromValid(
                             validVolumeDetails,
                             validCDBMetaData,
                             volumeDetails,
                             CDBMetaData
                            );
    end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Finished function.');
{$ENDIF}

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ChooseFromValid(
                             validVolumeDetails: TVolumeDetailsBlockArray;
                             validCDBMetaDatas: TCDBMetaDataArray;
                             var volumeDetails: TVolumeDetailsBlock;
                             var CDBMetaData: TCDBMetaData
                            ): boolean;
var
  allOK: boolean;
  hashCypherSelectDlg: TfrmSelectHashCypher;
  i: integer;
  prevCursor: TCursor;
begin
  allOK := TRUE;

  if (Length(validCDBMetaDatas) = 0) then
    begin
    // No valid combination of hash/cypher could be found
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('No valid hash/cypher combination found');
{$ENDIF}
    allOK := FALSE;
    end
  else if (Length(validCDBMetaDatas) = 1) then
    begin
    // There is only one valid hash/cypher combination that can be used; use it
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exactly one hash/cypher combination found successfully');
{$ENDIF}
    volumeDetails := validVolumeDetails[low(validVolumeDetails)];
    CDBMetaData := validCDBMetaDatas[low(validCDBMetaDatas)];
    end
  else
    begin
    // More than one valid hash/cypher combination; offer the user the choice
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('More than one hash/cypher combination found');
{$ENDIF}
    hashCypherSelectDlg:= TfrmSelectHashCypher.Create(nil);
    prevCursor:= Screen.Cursor;
    Screen.Cursor := crDefault;
    try
      hashCypherSelectDlg.FreeOTFEObj := self;
      for i:=low(validCDBMetaDatas) to high(validCDBMetaDatas) do
        begin
        hashCypherSelectDlg.AddCombination(
                                           validCDBMetaDatas[i].HashDriver,
                                           validCDBMetaDatas[i].HashGUID,
                                           validCDBMetaDatas[i].CypherDriver,
                                           validCDBMetaDatas[i].CypherGUID
                                          );
        end;

      if (hashCypherSelectDlg.ShowModal() <> mrOK) then
        begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('User cancelled');
{$ENDIF}
        allOK := FALSE;
        end
      else
        begin
        // Locate the selected hash/cypher
        for i:=low(validCDBMetaDatas) to high(validCDBMetaDatas) do
          begin
          if (
              (validCDBMetaDatas[i].HashDriver = hashCypherSelectDlg.SelectedHashDriverKernelModeName()) and
              IsEqualGUID(validCDBMetaDatas[i].HashGUID, hashCypherSelectDlg.SelectedHashGUID()) and
              (validCDBMetaDatas[i].CypherDriver = hashCypherSelectDlg.SelectedCypherDriverKernelModeName()) and
              IsEqualGUID(validCDBMetaDatas[i].CypherGUID, hashCypherSelectDlg.SelectedCypherGUID())
             ) then
            begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('User''s selected hash/cypher combination found.');
{$ENDIF}
            volumeDetails := validVolumeDetails[i];
            CDBMetaData := validCDBMetaDatas[i];
            break;
            end;

          end;

        end;

    finally
      Screen.Cursor := prevCursor;
      hashCypherSelectDlg.Free();
    end;

    end;


  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Write out a critical data block
// If the file specified doesn't already exist, it will be created
// ALL members of these two "volumeDetails" and "CDBMetaData" structs MUST
// be populated
// *EXCEPT* for:
//    volumeDetails.CDBFormatID - this is force set to the latest value
//    CDBMetaData.MACAlgorithm - set to appropriate value
//    CDBMetaData.KDFAlgorithm - set to appropriate value
function TOTFEFreeOTFEBase.WriteVolumeCriticalData(
    filename: string;
    offsetWithinFile: int64;
    userPassword: string;
    salt: string;
    keyIterations: integer;

    volumeDetails: TVolumeDetailsBlock;
    CDBMetaData: TCDBMetaData;

    randomPadData: string // Note: If insufficient is supplied, the write will
                          //       *fail*
                          //       The maximum that could possibly be required
                          //       is CRITICAL_DATA_LENGTH bits.
    ): boolean;
var
  encryptedBlock: string;
  allOK: boolean;
  sl_bits: integer;  // Salt value length, in *bits*
  criticalDataKey: string;
  IV: string;
  cypherDetails: TFreeOTFECypher_v3;
  hashDetails: TFreeOTFEHash;
  volumeDetailsBlockNoPadding: string;
  volumeDetailsBlock: string;
  criticalDataBlock: string;
  cdkSize_bits: integer;  // In *bits*
  randomPaddingTwoLength_bits: integer;  // In *bits*
  leb_bits: integer;  // In *bits*
  volumeDetailsBlockLength_bits: integer;  // In *bits*
  volumeDetailsBlockNoPaddingLength_bits: integer;  // In *bits*
  checkMAC: string;
  paddedCheckMAC: string;
  randomPaddingThreeLength_bits: integer;  // In *bits*
  plaintextEncryptedBlock: string;
  randomPaddingOneLength_bits: integer;  // In *bits*
begin
  allOK := TRUE;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In WriteVolumeCriticalData');
{$ENDIF}

  // Force set; this function only supports this one CDB format
  CDBMetaData.MACAlgorithm:= fomacHMAC;
  CDBMetaData.KDFAlgorithm:= fokdfPBKDF2;


  // Set to 0 to get rid of compiler warning
  randomPaddingOneLength_bits := 0;

  sl_bits := Length(salt) * 8;

  // Note: No need to populate CDBMetaData.MACAlgorithm or
  //       CDBMetaData.KDFAlgorithm as the function which writes the CDB out
  //       populates them automatically


  
  // Get details of the cypher so we know the key and blocksize...
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to get cypher details...');
{$ENDIF}
    if GetSpecificCypherDetails(CDBMetaData.CypherDriver, CDBMetaData.cypherGUID, cypherDetails) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Cypher driver: '+CDBMetaData.CypherDriver);
DebugMsg('Cypher impl: '+cypherDetails.Title+' ('+GUIDToString(CDBMetaData.CypherGUID)+')');
{$ENDIF}
      //
      end
    else
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      allOK := FALSE;
      end;
    end;


  // Get details of the hash so we know the hash length...
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to get hash details...');
{$ENDIF}
    if GetSpecificHashDetails(CDBMetaData.HashDriver, CDBMetaData.hashGUID, hashDetails) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Hash driver: '+CDBMetaData.HashDriver);
DebugMsg('Hash impl: '+hashDetails.Title+' ('+GUIDToString(CDBMetaData.HashGUID)+')');
{$ENDIF}
      end
    else
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      allOK := FALSE;
      end;
    end;



  // Sanity checks
  if (
      (cypherDetails.BlockSize <= 0) AND
      (volumeDetails.SectorIVGenMethod <> foivgNone)
     ) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Sector IVs cannot be used if cypher blocksize <= 0');
    end;
  if (
      (cypherDetails.BlockSize <= 0) AND
      (
       (volumeDetails.VolumeIVLength > 0) OR
       (Length(volumeDetails.VolumeIV) > 0)
      )
     ) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Volume IV cannot be used if cypher blocksize <= 0');
    end;
  if (
      (cypherDetails.BlockSize > 0) AND
      ((sl_bits mod cypherDetails.BlockSize) <> 0)
     ) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Salt length must be a multiple of the cypher blocksize');
    end;
  if (hashDetails.Length = 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Hash selected cannot be used; hash length must be greater than zero due to PBKDF2');
    end;
  if (hashDetails.Length < 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Hash selected cannot be used; hash length must fixed due to PBKDF2');
    end;
  if (hashDetails.BlockSize = 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Hash selected cannot be used; zero blocksize means useless check MAC may will be generated (zero bytes) long with HMAC)');
    end;
  if (hashDetails.BlockSize < 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Hash selected cannot be used; hash blocksize must be fixed due to HMAC algorithm');
    end;
  if ((volumeDetails.MasterKeyLength mod 8) <> 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Master key length must be multiple of 8');
    end;
  if (
      (cypherDetails.KeySizeRequired >= 0) AND
      (volumeDetails.MasterKeyLength <> DWORD(cypherDetails.KeySizeRequired))
     ) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Master key length must be the same as the cypher''s required keysize for cyphers with a fixed keysize');
    end;
  if ((volumeDetails.VolumeIVLength mod 8) <> 0) then
    begin
    raise EFreeOTFEInternalError.Create('Invalid: Volume IV length must be multiple of 8');
    end;



  // Determine size of "critical data key"...
  cdkSize_bits := cypherDetails.KeySizeRequired;  // In *bits*
  if (cdkSize_bits < 0) then
    begin
    cdkSize_bits := 512;
    end;


  // Derive the "critical data key"
  if (allOK) then
    begin
    allOK := DeriveKey(
                    CDBMetaData.KDFAlgorithm,
                    CDBMetaData.HashDriver,
                    CDBMetaData.HashGUID,
                    CDBMetaData.CypherDriver,
                    CDBMetaData.CypherGUID,
                    userPassword,
                    salt,
                    keyIterations,
                    cdkSize_bits,
                    criticalDataKey
                   );
    if not(allOK) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg(
         'FAILED TO DERIVE CRITICAL DATA KEY:'+SDUCRLF+
         'Hash:'+SDUCRLF+
         CDBMetaData.HashDriver+' ('+GUIDToString(CDBMetaData.hashGUID)+')'+SDUCRLF+
         'Cypher:'+SDUCRLF+
         CDBMetaData.CypherDriver+' ('+GUIDToString(CDBMetaData.cypherGUID)+')'
        );
{$ENDIF}
      LastErrorCode := OTFE_ERR_KDF_FAILURE;
      allOK := FALSE;
      end;
    end;


  // Create plaintext version of the "Volume details block" in-memory
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Converting volume details block structure to string rep...');
{$ENDIF}
    // Note: The string returned by BuildVolumeDetailsBlock(...) does *not*
    //       have any random data appended to it
    if not(BuildVolumeDetailsBlock(volumeDetails, volumeDetailsBlockNoPadding)) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: FAILED to convert critical data structure to string rep.');
{$ENDIF}
      LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
      allOK := FALSE;
      end
    else
      begin
      if (cypherDetails.BlockSize > 8) then
        begin
        leb_bits := (((CRITICAL_DATA_LENGTH - sl_bits) div cypherDetails.BlockSize) * cypherDetails.BlockSize);
        end
      else
        begin
        leb_bits := (CRITICAL_DATA_LENGTH - sl_bits);
        end;

      randomPaddingOneLength_bits := (CRITICAL_DATA_LENGTH - sl_bits) - leb_bits;

      // Note: This *includes* the length of the "Random padding #2"
      volumeDetailsBlockLength_bits := leb_bits - CDB_MAX_MAC_LENGTH;

      // Note: This *excludes* the length of the "Random padding #2"
      volumeDetailsBlockNoPaddingLength_bits := (Length(volumeDetailsBlockNoPadding) * 8);

      randomPaddingTwoLength_bits := volumeDetailsBlockLength_bits -
                                     volumeDetailsBlockNoPaddingLength_bits;


      // Pad out with "random data #2"...
      if (randomPaddingTwoLength_bits > 0) then
        begin
        volumeDetailsBlock := volumeDetailsBlockNoPadding +
                              Copy(
                                   randomPadData,
                                   1,
                                   (randomPaddingTwoLength_bits div 8)
                                  );
        // 1 because we index from 1, not zero
        Delete(randomPadData, 1, (randomPaddingTwoLength_bits div 8));
        end;

      end;

    end;


  // Generate the check MAC of the "volume details block"
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to HMAC critical data to create check data...');
{$ENDIF}
    allOK := MACData(
                      CDBMetaData.MACAlgorithm,
                      CDBMetaData.HashDriver,
                      CDBMetaData.HashGUID,
                      CDBMetaData.CypherDriver,
                      CDBMetaData.CypherGUID,
                      criticalDataKey,
                      volumeDetailsBlock,
                      checkMAC,
                      -1
                     );
    if not(allOK) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg(
         'FAILED TO GENERATE MAC:'+SDUCRLF+
         'Hash:'+SDUCRLF+
         hashDetails.Title+' ('+GUIDToString(hashDetails.HashGUID)+')'+SDUCRLF+
         'Cypher:'+SDUCRLF+
         cypherDetails.Title+' ('+GUIDToString(cypherDetails.CypherGUID)+')'
        );
{$ENDIF}
      LastErrorCode := OTFE_ERR_MAC_FAILURE;
      allOK := FALSE;
      end;

    end;


  // Truncate/pad check data as appropriate
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC check data generated OK.');
{$ENDIF}
    // If the checkdata newly generated is less than CDB_MAX_MAC_LENGTH bits,
    // right pad it with random data bits to CDB_MAX_MAC_LENGTH
    // If the checkdata newly generated is greater than CDB_MAX_MAC_LENGTH
    // bits, truncate it to the first CDB_MAX_MAC_LENGTH bits.
    if ((Length(checkMAC) * 8) < CDB_MAX_MAC_LENGTH) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Check data generated hash less than CDB_MAX_MAC_LENGTH bits; padding...');
{$ENDIF}
      randomPaddingThreeLength_bits := CDB_MAX_MAC_LENGTH - (Length(checkMAC) * 8);
      paddedCheckMAC := checkMAC + Copy(
                                        randomPadData,
                                        1,
                                        (randomPaddingThreeLength_bits div 8)
                                       );
      // 1 because we index from 1, not zero
      Delete(randomPadData, 1, (randomPaddingThreeLength_bits div 8));
      end
    else if ((Length(checkMAC) * 8) > CDB_MAX_MAC_LENGTH) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Check data generated hash greater than CDB_MAX_MAC_LENGTH bits; truncating...');
{$ENDIF}
      paddedCheckMAC := checkMAC;
      // +1 because we index from 1, not zero
      Delete(
             paddedCheckMAC,
             ((CDB_MAX_MAC_LENGTH div 8)+1),
             (Length(paddedCheckMAC) - (CDB_MAX_MAC_LENGTH div 8))
            );
      end
    else
      begin
      // No padding/truncation needed
      paddedCheckMAC := checkMAC;
      end;

    end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Padded check MAC is: ');
DebugMsg('-- begin --');
DebugMsgBinary(paddedCheckMAC);
DebugMsg('-- end --');
{$ENDIF}


  if (allOK) then
    begin
    // Prepend the check MAC (padded if necessary) to the volume details block
    // to form a plaintext version of the "Encrypted block"
    plaintextEncryptedBlock := paddedCheckMAC + volumeDetailsBlock;
    end;


  // Encrypt the plaintextEncryptedBlock, using
  //  - Zero'd IV
  //  - Critical data key
  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Creating zeroed IV');
{$ENDIF}
    // Zero the IV for decryption
    IV := '';
    if (cypherDetails.BlockSize > 0) then
      begin
      IV := StringOfChar(#0, (cypherDetails.BlockSize div 8));
      end;


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('CriticalData.cypherDriverKernelModeName: '+CDBMetaData.CypherDriver);
DebugMsg('CriticalData.cypherGUID: '+GUIDToString(CDBMetaData.CypherGUID));
DebugMsg('CriticalDataKey len: '+inttostr(length(criticalDataKey)));
DebugMsg('IV len: '+inttostr(length(IV)));

DebugMsg('Critical data key:');
DebugMsgBinary(criticalDataKey);
DebugMsg('before encryption, decrypted block is:');
DebugMsgBinary(plaintextEncryptedBlock);
{$ENDIF}
    if not(EncryptSectorData(
                       CDBMetaData.CypherDriver,
                       CDBMetaData.CypherGUID,
                       FREEOTFE_v1_DUMMY_SECTOR_ID,
                       FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                       criticalDataKey,
                       IV,
                       plaintextEncryptedBlock,
                       encryptedBlock
                      )) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: FAILED to encrypt data block');
{$ENDIF}
      LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
      allOK := FALSE;
      end;
      
    end;



  // Form the critical datablock using the salt bytes, the encrypted data block,
  // and random padding data #1
  if (allOK) then
    begin
    criticalDataBlock := salt +
                         encryptedBlock;
                         Copy(
                              randomPadData,
                              1,
                              (randomPaddingOneLength_bits div 8)
                             );
    // 1 because we index from 1, not zero
    Delete(randomPadData, 1, (randomPaddingOneLength_bits div 8));
    end;


  // Finally, write the critical data block out to the volume file, starting
  // from the appropriate offset
  if (allOK) then
    begin
    allOK := WriteRawVolumeCriticalData(
                                        filename,
                                        offsetWithinFile,
                                        criticalDataBlock
                                       );
    end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting function');
{$ENDIF}


  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetHashList(var hashDispTitles, hashKernelModeDriverNames, hashGUIDs: TStringList): boolean;
var
  hashDrivers: array of TFreeOTFEHashDriver;
  i, j, k: integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHash: TFreeOTFEHash;
  allOK: boolean;
  tmpPrettyTitle: string;
  inserted: boolean;
begin
  allOK := FALSE;

  SetLength(hashDrivers, 0);
  if (GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers))) then
    begin
    hashDispTitles.Clear();
    hashKernelModeDriverNames.Clear();
    hashGUIDs.Clear();

    for i:=low(hashDrivers) to high(hashDrivers) do
      begin
      currHashDriver := hashDrivers[i];
      for j:=low(hashDrivers[i].Hashes) to high(hashDrivers[i].Hashes) do
        begin
        currHash := hashDrivers[i].Hashes[j];

        tmpPrettyTitle := GetHashDisplayTitle(currHash);

        // Attempt to insert in alphabetic  order...
        inserted := FALSE;
        for k:=0 to (hashDispTitles.count-1) do
          begin
          if (tmpPrettyTitle < hashDispTitles[k]) then
            begin
            hashDispTitles.Insert(k, tmpPrettyTitle);
            hashKernelModeDriverNames.Insert(k, currHashDriver.LibFNOrDevKnlMdeName);
            hashGUIDs.Insert(k, GUIDToString(currHash.HashGUID));
            inserted := TRUE;
            break;
            end;

          end;

        if not(inserted) then
          begin
          hashDispTitles.Add(tmpPrettyTitle);
          hashKernelModeDriverNames.Add(currHashDriver.LibFNOrDevKnlMdeName);
          hashGUIDs.Add(GUIDToString(currHash.HashGUID));
          end;
          
        end;

      end;

    allOK := TRUE;
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetCypherList(var cypherDispTitles, cypherKernelModeDriverNames, cypherGUIDs: TStringList): boolean;
var
  cypherDrivers: array of TFreeOTFECypherDriver;
  i, j, k: integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypher: TFreeOTFECypher_v3;
  allOK: boolean;
  tmpPrettyTitle: string;
  inserted: boolean;
begin
  allOK:= FALSE;
  
  SetLength(cypherDrivers, 0);
  if (GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers))) then
    begin
    for i:=low(cypherDrivers) to high(cypherDrivers) do
      begin
      currCypherDriver := cypherDrivers[i];
      for j:=low(cypherDrivers[i].Cyphers) to high(cypherDrivers[i].Cyphers) do
        begin
        currCypher := cypherDrivers[i].Cyphers[j];

        tmpPrettyTitle := GetCypherDisplayTitle(currCypher);

        // Attempt to insert in alphabetic  order...
        inserted := FALSE;
        for k:=0 to (cypherDispTitles.count-1) do
          begin
          if (tmpPrettyTitle < cypherDispTitles[k]) then
            begin
            cypherDispTitles.Insert(k, tmpPrettyTitle);
            cypherKernelModeDriverNames.Insert(k, currCypherDriver.LibFNOrDevKnlMdeName);
            cypherGUIDs.Insert(k, GUIDToString(currCypher.CypherGUID));
            inserted := TRUE;
            break;
            end;

          end;

        if not(inserted) then
          begin
          cypherDispTitles.Add(tmpPrettyTitle);
          cypherKernelModeDriverNames.Add(currCypherDriver.LibFNOrDevKnlMdeName);
          cypherGUIDs.Add(GUIDToString(currCypher.CypherGUID));
          end;
          
        end;

      end;

    allOK := TRUE;
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Generate a prettyprinted title for a specific hash
// Returns '' on error
function TOTFEFreeOTFEBase.GetHashDisplayTitle(hash: TFreeOTFEHash): string;
begin
  Result := hash.Title;
end;

// ----------------------------------------------------------------------------
// Generate a prettyprinted title for a specific hash
// Returns '' on error
function TOTFEFreeOTFEBase.GetHashDisplayTechTitle(hash: TFreeOTFEHash): string;
begin
  Result := hash.Title + ' (' + inttostr(hash.Length) + '/' + inttostr(hash.BlockSize) + ')';
end;


// ----------------------------------------------------------------------------
// Generate a prettyprinted title for a specific cypher
// Returns '' on error
function TOTFEFreeOTFEBase.GetCypherDisplayTitle(cypher: TFreeOTFECypher_v3): string;
var
  strCypherMode: string;
begin
  // This part prettifies the cypher details and populates the combobox
  // as appropriate
  strCypherMode := '';
  if (cypher.Mode = focmUnknown) then
    begin
    strCypherMode := _('Mode unknown');
    end
  else if (cypher.Mode <> focmNone) then
    begin
    strCypherMode := FreeOTFECypherModeTitle(cypher.Mode);
    end;

  Result := cypher.Title + ' (' + inttostr(cypher.KeySizeUnderlying) + ' bit ' + strCypherMode + ')';

end;

// ----------------------------------------------------------------------------
// Generate a prettyprinted title for a specific cypher
// Returns '' on error
function TOTFEFreeOTFEBase.GetCypherDisplayTechTitle(cypher: TFreeOTFECypher_v3): string;
var
  strCypherMode: string;
  strCypherSizes: string;
begin
  // This part prettifies the cypher details and populates the combobox
  // as appropriate
  strCypherMode := '';
  if (cypher.Mode = focmUnknown) then
    begin
    // Note the semicolon-space on the end
    strCypherMode := _('Mode unknown; ');
    end
  else if (cypher.Mode <> focmNone) then
    begin
    // Note the semicolon-space on the end
    strCypherMode := FreeOTFECypherModeTitle(cypher.Mode)+'; ';
    end;

  strCypherSizes := inttostr(cypher.KeySizeUnderlying) + '/' + inttostr(cypher.BlockSize);

  Result := cypher.Title + ' (' + strCypherMode + strCypherSizes + ')';

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateFreeOTFEVolumeWizard(): boolean;
var
  frmWizard: TfrmWizardCreateVolume;
  allOK: boolean;
  mr: integer;
begin
  allOK:= FALSE;
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  if WarnIfNoHashOrCypherDrivers() then
    begin
    frmWizard:= TfrmWizardCreateVolume.Create(nil);
    try
      frmWizard.FreeOTFEObj := self;

      mr := frmWizard.ShowModal();
      if (mr = mrOK) then
        begin
        LastErrorCode := OTFE_ERR_SUCCESS;
        allOK:= TRUE;
        end
      else if (mr = mrCancel) then
        begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
        end;

    finally
      frmWizard.Free();
    end;

    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateLinuxVolumeWizard(): boolean;
var
  volSizeDlg: TfrmNewVolumeSize;
  mr: integer;
  allOK: boolean;
  userCancel: boolean;
begin
  allOK := FALSE;

  volSizeDlg:= TfrmNewVolumeSize.Create(nil);
  try
    mr := volSizeDlg.ShowModal;
    if (mr = mrCancel) then
      begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
      end
    else if (mr = mrOK) then
      begin
      if not(SDUCreateLargeFile(volSizeDlg.Filename, volSizeDlg.VolumeSize, TRUE, userCancel)) then
        begin
        if not(userCancel) then
          begin
          SDUMessageDlg(_('An error occured while trying to create your volume file'), mtError, [mbOK], 0);
          end;

        end
      else
        begin
        allOK := TRUE;
        end;
      end;

  finally
    volSizeDlg.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateLinuxGPGKeyfile(): boolean;
var
  allOK: boolean;
begin
  allOK:= FALSE;

// xxx - implement GPG integration

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Display control for managing PKCS#11 tokens
procedure TOTFEFreeOTFEBase.ShowPKCS11ManagementDlg();
var
  pkcs11session: TPKCS11Session;
  dlgPKCS11Session: TfrmPKCS11Session;
  dlgPKCS11Management: TfrmPKCS11Management;
begin
  // Setup PKCS11 session, as appropriate
  pkcs11session := nil;
  if PKCS11LibraryReady(PKCS11Library) then
    begin
    dlgPKCS11Session := TfrmPKCS11Session.Create(nil);
    try
      dlgPKCS11Session.PKCS11LibObj := PKCS11Library;
      dlgPKCS11Session.AllowSkip := FALSE;
      if (dlgPKCS11Session.ShowModal = mrCancel) then
        begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
        end
      else
        begin
        pkcs11session := dlgPKCS11Session.Session;
        end;

    finally
      dlgPKCS11Session.Free();
    end;
    end;

  if (pkcs11session <> nil) then
    begin
    dlgPKCS11Management:= TfrmPKCS11Management.Create(nil);
    try
      dlgPKCS11Management.FreeOTFEObj := self;
      dlgPKCS11Management.PKCS11Session := pkcs11session;
      dlgPKCS11Management.ShowModal();
    finally
      dlgPKCS11Management.Free();
    end;

    pkcs11session.Logout();
    pkcs11session.CloseSession();
    pkcs11session.Free();
    end;

end;


// ----------------------------------------------------------------------------
{$IFDEF FREEOTFE_DEBUG}
procedure TOTFEFreeOTFEBase.DebugMsg(msg: string);
begin
  DebugStrings.Add(msg);
{$IFDEF _GEXPERTS}
  SendDebug(msg);
{$ENDIF}
  if DebugShowMessage then
    begin
    showmessage(msg);
    end;
end;
{$ENDIF}

{$IFDEF FREEOTFE_DEBUG}
procedure TOTFEFreeOTFEBase.DebugClear();
begin
{$IFNDEF _GEXPERTS}
  if (DebugLogfile <> '') then
    begin
    inc(DebugLogfileCount);
    DebugStrings.SaveToFile(DebugLogfile+'.'+inttostr(DebugLogfileCount)+'.log');
    end;
{$ENDIF}

  DebugStrings.Clear();
end;
{$ENDIF}

{$IFDEF FREEOTFE_DEBUG}
procedure TOTFEFreeOTFEBase.DebugMsgBinary(msg: string);
var
  prettyStrings: TStringList;
  i: integer;
begin
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(msg, 0, Length(msg), TStringList(prettyStrings), 16);
    for i := 0 to (prettyStrings.Count - 1) do
      begin
{$IFDEF _GEXPERTS}
      SendDebug(prettyStrings[i]);
{$ENDIF}
      end;
    DebugStrings.AddStrings(prettyStrings);
    if DebugShowMessage then
      begin
      showmessage(prettyStrings.Text);
      end;
  finally
    prettyStrings.Free();
  end;

end;
{$ENDIF}

{$IFDEF FREEOTFE_DEBUG}
procedure TOTFEFreeOTFEBase.DebugMsgBinaryPtr(msg: PChar; len:integer);
var
  prettyStrings: TStringList;
  i: integer;
begin
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(Pointer(msg), 0, len, TStringList(prettyStrings), 16);
    for i := 0 to (prettyStrings.Count - 1) do
      begin
{$IFDEF _GEXPERTS}
      SendDebug(prettyStrings[i]);
{$ENDIF}
      end;
    DebugStrings.AddStrings(prettyStrings);
    if DebugShowMessage then
      begin
      showmessage(prettyStrings.Text);
      end;
  finally
    prettyStrings.Free();
  end;

end;
{$ENDIF}
 

// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFEFreeOTFEBase.CanMountDevice(): boolean;
begin
  // Supported
  Result := TRUE;
end;


// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEFreeOTFEBase.MountDevices(): string;
var
  selectedPartition: string;
  mountedAs: string;
  frmSelectVolumeType: TfrmSelectVolumeType;
  retVal: string;
  mr: integer;
begin
  mountedAs := '';

  CheckActive();

  if CanMountDevice() then
    begin
    // Ask the user if they want to mount their partitions as FreeOTFE or Linux
    // partitions
    frmSelectVolumeType:= TfrmSelectVolumeType.Create(nil);
    try
      mr := frmSelectVolumeType.ShowModal();
      if (mr = mrCancel) then
        begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
        end
      else if (mr = mrOK) then
        begin
        // Ask the user which partition they want to mount
        selectedPartition:= SelectPartition();
        if (selectedPartition = '') then
          begin
          LastErrorCode := OTFE_ERR_USER_CANCEL;
          end
        else
          begin
          // Mount the partition as the appropriate type
          if frmSelectVolumeType.FreeOTFEVolume then
            begin
            retVal := MountFreeOTFE(selectedPartition, FALSE);
            end
          else
            begin
            retVal := MountLinux(selectedPartition, FALSE);
            end;
          end;

        end;

    finally
      frmSelectVolumeType.Free();;
    end;

    end;

  Result := retVal;

end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.BackupVolumeCriticalData(
                                   srcFilename: string;
                                   srcOffsetWithinFile: int64;
                                   destFilename: string
                                  ): boolean;
var
  criticalData: string;
  destFileStream: TFileStream;
  allOK: boolean;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  allOK := ReadRawVolumeCriticalData(
                             srcFilename,
                             srcOffsetWithinFile,
                             criticalData
                            );

  if (allOK) then
    begin
    // Because we're just writing to a straight file, we don't need the
    // FreeOTFE driver to do this.
    // Also, because the file shouldn't already exist, the FreeOTFE driver
    // *won't* do this - it expects the target to already exist
    destFileStream := TFileStream.Create(destFilename, fmCreate OR fmShareDenyNone);
    try
      if length(criticalData)>0 then
        begin
        allOK := (destFileStream.Write(criticalData[1], length(criticalData)) = length(criticalData));
        end;
    finally
      destFileStream.Free();
    end;

    end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.RestoreVolumeCriticalData(
                                    srcFilename: string;
                                    destFilename: string;
                                    destOffsetWithinFile: int64
                                   ): boolean;
var
  criticalData: string;
  allOK: boolean;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  allOK := ReadRawVolumeCriticalData(
                             srcFilename,
                             0,
                             criticalData
                            );

  if (allOK) then
    begin
    // Get the FreeOTFE driver to write the critical data block to the named file
    // starting from the specified offset
    allOK := WriteRawVolumeCriticalData(
                             destFilename,
                             destOffsetWithinFile,
                             criticalData
                            );
    end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateKeyfile(
                               srcFilename: string;
                               srcOffsetWithinFile: int64;
                               srcUserPassword: string;
                               srcSaltLength: integer;  // In bits
                               srcKeyIterations: integer;
                               keyFilename: string;
                               keyfileUserPassword: string;
                               keyfileSaltBytes: string;
                               keyfileKeyIterations: integer;
                               keyfileRequestedDrive: char;
                               keyfileRandomPadData: string  // Note: If insufficient is supplied, the write will *fail*
                                                             //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
                              ): boolean;
var
  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;
  allOK: boolean;
  userCancel: boolean;
begin
  allOK := FALSE;

  if ReadVolumeCriticalData(
                           srcFilename,
                           srcOffsetWithinFile,
                           srcUserPassword,
                           srcSaltLength,  // In bits
                           srcKeyIterations,
                           volumeDetails,
                           CDBMetaData
                          ) then
    begin
    volumeDetails.RequestedDriveLetter := keyfileRequestedDrive;

    // Just create a dummy file which will be populated with the keyfile's
    // contents (the driver doesn't create files, it just writes data to them)
    if (not(FileExists(keyFilename))) then
      begin
      SDUCreateLargeFile(keyFilename, (CRITICAL_DATA_LENGTH div 8), FALSE, userCancel);
      end;

    allOK := WriteVolumeCriticalData(
                                     keyFilename,
                                     0,
                                     keyfileUserPassword,
                                     keyfileSaltBytes,
                                     keyfileKeyIterations,
                                     volumeDetails,
                                     CDBMetaData,
                                     keyfileRandomPadData
                                    );
    end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ChangeVolumePassword(
                               filename: string;
                               offsetWithinFile: int64;
                               oldUserPassword: string;
                               oldSaltLength: integer;  // In bits
                               oldKeyIterations: integer;
                               newUserPassword: string;
                               newSaltBytes: string;
                               newKeyIterations: integer;
                               newRequestedDriveLetter: char;
                               newRandomPadData: string  // Note: If insufficient is supplied, the write will *fail*
                                                         //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
                              ): boolean;
var
  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;
  allOK: boolean;
begin
  allOK := FALSE;

  if ReadVolumeCriticalData(
                           filename,
                           offsetWithinFile,
                           oldUserPassword,
                           oldSaltLength,  // In bits
                           oldKeyIterations,
                           volumeDetails,
                           CDBMetaData
                          ) then
    begin
    volumeDetails.RequestedDriveLetter := newRequestedDriveLetter;

    allOK := WriteVolumeCriticalData(
                                     filename,
                                     offsetWithinFile,
                                     newUserPassword,
                                     newSaltBytes,
                                     newKeyIterations,
                                     volumeDetails,
                                     CDBMetaData,
                                     newRandomPadData
                                    );
    end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.DumpCriticalDataToFile(
                               volFilename: string;
                               offsetWithinFile: int64;
                               userPassword: string;
                               saltLength: integer;  // In bits
                               keyIterations: integer;
                               dumpFilename: string
                              ): boolean;
var
  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;
  allOK: boolean;
  dumpReport: TStringList;
  prettyPrintData: TStringList;
  criticalDataBuffer: string;
  hashTitle: string;
  cypherTitle: string;
  hashDetails: TFreeOTFEHash;
  cypherDetails: TFreeOTFECypher_v3;
  readTimeStart: TDateTime;
  readTimeStop: TDateTime;
  readTimeDiff: TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
  allOK := FALSE;

  CheckActive();

  prettyPrintData := TStringList.Create();
  try
    DumpFlag := TRUE;

    readTimeStart:= Now();

    // Read in the raw CDB to display the encrypted version, and then read it in
    // again to get the decrypt it
    if (
        ReadRawVolumeCriticalData(
                               volFilename,
                               offsetWithinFile,
                               criticalDataBuffer
                              )

          AND

        ReadVolumeCriticalData(
                               volFilename,
                               offsetWithinFile,
                               userPassword,
                               saltLength,  // In bits
                               keyIterations,
                               volumeDetails,
                               CDBMetaData
                              )
      ) then
      begin
      readTimeStop:= Now();

      // Generate the report
      dumpReport := TStringList.Create();
      try
        AddStdDumpHeader(dumpReport, _('Critical Data Block Dump'));

        AddStdDumpSection(dumpReport, _('User Supplied Information'));
        dumpReport.Add(SDUParamSubstitute(_('Filename (user mode)  : %1'), [volFilename]));
        dumpReport.Add(SDUParamSubstitute(_('Filename (kernel mode): %1'), [GetKernelModeVolumeFilename(volFilename)]));
        dumpReport.Add(_('Password              : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length: %1 bits'), [(Length(userPassword) * 8)]));
        dumpReport.Add('  '+_('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          userPassword,
                          0,
                          Length(userPassword),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(SDUParamSubstitute(_('Offset                : %1 bytes'), [offsetWithinFile]));
        dumpReport.Add(SDUParamSubstitute(_('Salt length           : %1 bits'), [saltLength]));
        dumpReport.Add(SDUParamSubstitute(_('Key iterations        : %1'), [keyIterations]));

        AddStdDumpSection(dumpReport, _('Plaintext Information'));
        dumpReport.Add(_('Salt data             :'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          criticalDataBuffer,
                          0,
                          (saltLength div 8),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);


        AddStdDumpSection(dumpReport, _('Dump Performance'));
        readTimeDiff := (readTimeStop - readTimeStart);
        DecodeTime(readTimeDiff, Hour, Min, Sec, MSec);
        dumpReport.Add(SDUParamSubstitute(_('Time to process CDB   : %1 hours, %2 mins, %3.%4 secs'), [Hour, Min, Sec, MSec]));


        AddStdDumpSection(dumpReport, _('Autodetermined Information'));

        hashTitle := _('ERROR: Unable to determine hash title?!');
        if GetSpecificHashDetails(
                                  CDBMetaData.HashDriver,
                                  CDBMetaData.HashGUID,
                                  hashDetails
                                 ) then
          begin
          hashTitle := GetHashDisplayTechTitle(hashDetails);
          end;

        cypherTitle := _('ERROR: Unable to determine cypher title?!');
        if GetSpecificCypherDetails(
                                    CDBMetaData.CypherDriver,
                                    CDBMetaData.CypherGUID,
                                    cypherDetails
                                   ) then
          begin
          cypherTitle := GetCypherDisplayTechTitle(cypherDetails);
          end;

        dumpReport.Add(SDUParamSubstitute(_('Hash pretty title        : %1'), [hashTitle]));
        dumpReport.Add(SDUParamSubstitute(_('Hash driver lib/KM name  : %1'), [CDBMetaData.HashDriver]));
        dumpReport.Add(SDUParamSubstitute(_('Hash GUID                : %1'), [GUIDToString(CDBMetaData.HashGUID)]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher pretty title      : %1'), [cypherTitle]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher driver lib/KM name: %1'), [CDBMetaData.CypherDriver]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher GUID              : %1'), [GUIDToString(CDBMetaData.CypherGUID)]));
        dumpReport.Add(_('Critical data key     : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('KDF   : %1'), [FreeOTFEKDFTitle(CDBMetaData.KDFAlgorithm)]));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length: %1 bits'), [(Length(DumpCriticalDataKey) * 8)]));
        dumpReport.Add('  '+_('Key   : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          DumpCriticalDataKey,
                          0,
                          Length(DumpCriticalDataKey),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Plaintext encrypted block: '));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length: %1 bits'), [(Length(DumpPlaintextEncryptedBlock) * 8)]));
        dumpReport.Add('  '+_('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          DumpPlaintextEncryptedBlock,
                          0,
                          Length(DumpPlaintextEncryptedBlock),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Check MAC             : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('MAC algorithm: %1'), [FreeOTFEMACTitle(CDBMetaData.MACAlgorithm)]));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length       : %1 bits'), [(Length(DumpCheckMAC) * 8)]));
        dumpReport.Add('  '+_('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          DumpCheckMAC,
                          0,
                          Length(DumpCheckMAC),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Volume details block  : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length       : %1 bits'), [(Length(DumpVolumeDetailsBlock) * 8)]));
        dumpReport.Add('  '+_('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          DumpVolumeDetailsBlock,
                          0,
                          Length(DumpVolumeDetailsBlock),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);


        AddStdDumpSection(dumpReport, _('Volume Details Block'));
        dumpReport.Add(SDUParamSubstitute(_('CDB format ID         : %1'), [volumeDetails.CDBFormatID]));

        // Decode the VolumeFlags to human-readable format
        dumpReport.Add(SDUParamSubstitute(_('Volume flags          : %1'), [volumeDetails.VolumeFlags]));
        dumpReport.Add(_('                        ')+
                       DumpCriticalDataToFileBitmap(
                                 volumeDetails.VolumeFlags,
                                 VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART,
                                 _('Sector ID zero is at the start of the encrypted data'),
                                 _('Sector ID zero is at the start of the host file/partition')
                                ));

        dumpReport.Add(SDUParamSubstitute(_('Partition length      : %1 bytes'), [volumeDetails.PartitionLen]));

        dumpReport.Add(_('Master key            : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length: %1 bits'), [volumeDetails.MasterKeyLength]));
        dumpReport.Add('  '+_('Key   :'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          volumeDetails.MasterKey,
                          0,
                          (volumeDetails.MasterKeyLength div 8),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(SDUParamSubstitute(_('Sector IV generation  : %1'), [FreeOTFESectorIVGenMethodTitle[volumeDetails.SectorIVGenMethod]]));
        dumpReport.Add(_('Volume IV             : '));
        dumpReport.Add('  '+SDUParamSubstitute(_('Length : %1 bits'), [volumeDetails.VolumeIVLength]));
        dumpReport.Add('  '+_('IV data:'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          volumeDetails.VolumeIV,
                          0,
                          (volumeDetails.VolumeIVLength div 8),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);

        if (volumeDetails.RequestedDriveLetter = #0) then
          begin
          dumpReport.Add(_('Requested drive letter: None; use default'));
          end
        else
          begin
          dumpReport.Add(SDUParamSubstitute(_('Requested drive letter: %1'), [volumeDetails.RequestedDriveLetter+':']));
          end;

        AddStdDumpSection(dumpReport, _('Encrypted CDB'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
                          criticalDataBuffer,
                          0,
                          (CRITICAL_DATA_LENGTH div 8),
                          prettyPrintData
                         );
        dumpReport.AddStrings(prettyPrintData);


        // Save the report out to disk...
        dumpReport.SaveToFile(dumpFilename);
        allOK := TRUE;
      finally
        DumpFlag := FALSE;
        DumpCriticalDataKey        := '';
        DumpCheckMAC               := '';
        DumpPlaintextEncryptedBlock:= '';
        DumpVolumeDetailsBlock     := '';

        dumpReport.Clear();
        dumpReport.Free();
      end;

      end;

  finally
    prettyPrintData.Free();
  end;

  if (allOK) then
    begin
    LastErrorCode := OTFE_ERR_SUCCESS;
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Return a nicely formatted string, decoding one bit of a bitmapped value
// bit - The zero-offset bit (i.e. the LSB is bit 0)
function TOTFEFreeOTFEBase.DumpCriticalDataToFileBitmap(
                               value: DWORD;
                               bit: integer;
                               unsetMeaning: string;
                               setMeaning: string
                              ): string;
var
  x: DWORD;
  i: integer;
  retVal: string;
  oneZero: integer;
  useMeaning: string;
begin
  retVal := '';

  x := 1;
  for i:=1 to bit do
    begin
    x := x * 2;
    end;

  oneZero := 0;
  useMeaning := unsetMeaning;
  if ((value AND x) = x) then
    begin
    oneZero := 1;
    useMeaning := setMeaning;
    end;

  retVal := SDUParamSubstitute(_('Bit %1: %2 (%3)'), [bit, oneZero, useMeaning]);

  Result := retval;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WizardChangePassword(): boolean;
var
  dlg: TfrmWizardChangePasswordCreateKeyfile;
  allOK: boolean;
begin
  allOK := FALSE;

  if WarnIfNoHashOrCypherDrivers() then
    begin
    dlg:= TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.ChangePasswordCreateKeyfile := opChangePassword;
      dlg.FreeOTFEObj := self;
      allOK := (dlg.ShowModal() = mrOK);
    finally
      dlg.Free();
    end;

    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WizardCreateKeyfile(): boolean;
var
  dlg: TfrmWizardChangePasswordCreateKeyfile;
  allOK: boolean;
begin
  allOK := FALSE;

  if WarnIfNoHashOrCypherDrivers() then
    begin
    dlg:= TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.ChangePasswordCreateKeyfile := opCreateKeyfile;
      dlg.FreeOTFEObj := self;
      allOK := (dlg.ShowModal() = mrOK);
    finally
      dlg.Free();
    end;
    
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Convert a kernel mode volume filename to a user mode volume filename
function TOTFEFreeOTFEBase.GetUserModeVolumeFilename(kernelModeFilename: string): string;
var
  retVal: string;
begin
  retVal := kernelModeFilename;


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
  if Pos('\??\UNC\', retVal) = 1 then
    begin
    // \\server\share\path\filedisk.img
    delete(retVal, 1, length('\??\UNC\'));
    retVal := '\\' + retVal;
    end
  else if Pos('\??\', retVal) = 1 then
    begin
    // C:\path\filedisk.img
    delete(retVal, 1, length('\??\'));
    end
  else
    begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    // Do nothing.
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Convert a user mode volume filename to a kernel mode volume filename
function TOTFEFreeOTFEBase.GetKernelModeVolumeFilename(userModeFilename: string): string;
var
  retVal: string;
begin
  retVal := '';


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
    retVal := '\??\UNC'+userModeFilename;
    end
  else if (
           (Pos(':\', userModeFilename) = 2) OR
           (Pos(':/', userModeFilename) = 2)
          ) then
    begin
    // C:\path\filedisk.img
    retVal := '\??\'+userModeFilename;
    end
  else
    begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    retVal := userModeFilename;
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
// filename refers to a partition/file
function TOTFEFreeOTFEBase.IsPartition_UserModeName(userModeFilename: string): boolean;
begin
  // In user mode, partitions start with "\\.\"
  Result := (Pos('\\.\', userModeFilename) = 1);

end;


// ----------------------------------------------------------------------------
// Get the FreeOTFE driver to read a critical data block from the named file
// starting from the specified offset
// criticalData - This will be set to the string representation of the raw
//                (encrypted) critical data block
function TOTFEFreeOTFEBase.ReadRawVolumeCriticalData(
                         filename: string;
                         offsetWithinFile: int64;
                         var criticalData: string
                        ): boolean;
begin
  Result := ReadRawVolumeData(
                             filename,
                             offsetWithinFile,
                             (CRITICAL_DATA_LENGTH div 8),
                             criticalData
                            );

end;


// ----------------------------------------------------------------------------
// Get the FreeOTFE driver to read data from the named file
// starting from the specified offset
// data - This will be set to the string representation of the raw data read
function TOTFEFreeOTFEBase.ReadRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;
var
  retVal: boolean;
begin
  retVal := ReadRawVolumeDataSimple(
                                    filename,
                                    offsetWithinFile,
                                    dataLength,
                                    data
                                   );
  if not(retVal) then
    begin
    retVal := ReadRawVolumeDataBounded(
                                       filename,
                                       offsetWithinFile,
                                       dataLength,
                                       data
                                      );
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadRawVolumeDataBounded(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;
var
  preData: DWORD;
  postData: DWORD;
  tmpData: string;
  retVal: boolean;
begin
  preData := (offsetWithinFile mod ASSUMED_HOST_SECTOR_SIZE);
  // Yes, this is correct.
  // We use preData to avoid the need to add offsetWithinFile (a 64 bit value)
  // and then do the mod
  postData := ASSUMED_HOST_SECTOR_SIZE - ((preData + dataLength) mod ASSUMED_HOST_SECTOR_SIZE);

  retVal := ReadRawVolumeDataSimple(
                                    filename,
                                    (offsetWithinFile - preData),
                                    (preData + dataLength + postData),
                                    tmpData
                                   );
  if retVal then
    begin
    // Trim off the pre/post data garbage
    data := Copy(tmpData, preData, dataLength);
    
    // Overwrite...
    tmpData := StringOfChar(#0, (preData + dataLength + postData));
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var Data: string
                        ): boolean;
var
  fStream: TFileStream;
  retVal: boolean;
begin
  retVal := FALSE;

  CheckActive();

  // Set data so that it has enough characters which can be
  // overwritten with StrMove
  data := StringOfChar(#0, dataLength);

  try
    fStream:= TFileStream.Create(filename, fmOpenRead);
    try
      fStream.Position := offsetWithinFile;
      fStream.Read(data[1], dataLength);
      retval := TRUE;
    finally
      fStream.Free();
    end;
  except
    // Just swallow exception; retval already set to FALSE
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
{$IFDEF FREEOTFE_DEBUG}
// Expose ReadRawVolumeData(...) for debug
function TOTFEFreeOTFEBase.DEBUGReadRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         dataLength: DWORD;  // In bytes
                         var data: string
                        ): boolean;
begin
  Result := ReadRawVolumeData(
                         filename,
                         offsetWithinFile,
                         dataLength,
                         data
                        );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
{$IFDEF FREEOTFE_DEBUG}
// Expose WriteRawVolumeData(...) for debug
function TOTFEFreeOTFEBase.DEBUGWriteRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;
begin
  Result := WriteRawVolumeData(
                         filename,
                         offsetWithinFile,
                         data
                        );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
{$IFDEF FREEOTFE_DEBUG}
// Expose ReadWritePlaintextToVolume(...) for debug
function TOTFEFreeOTFEBase.DEBUGReadWritePlaintextToVolume(
                    readNotWrite: boolean;

                    volFilename: string;
                    volumeKey: string;
                    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                    IVHashDriver: string;
                    IVHashGUID: TGUID;
                    IVCypherDriver: string;
                    IVCypherGUID: TGUID;
                    mainCypherDriver: string;
                    mainCypherGUID: TGUID;
                    VolumeFlags: integer;
                    mountMountAs: TFreeOTFEMountAs;

                    dataOffset: int64;  // Offset from within mounted volume from where to read/write data
                    dataLength: integer;  // Length of data to read/write. In bytes
                    var data: string;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean;
begin
  Result := ReadWritePlaintextToVolume(
                    readNotWrite,

                    volFilename,
                    volumeKey,
                    sectorIVGenMethod,
                    IVHashDriver,
                    IVHashGUID,
                    IVCypherDriver,
                    IVCypherGUID,
                    mainCypherDriver,
                    mainCypherGUID,
                    VolumeFlags,
                    mountMountAs,

                    dataOffset,
                    dataLength,
                    data,

                    offset,
                    size,
                    storageMediaType
                  );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
// Get the FreeOTFE driver to write a critical data block from the named file
// starting from the specified offset
// criticalData - This should be set to the string representation of a raw
//                (encrypted) critical data block
function TOTFEFreeOTFEBase.WriteRawVolumeCriticalData(
                         filename: string;
                         offsetWithinFile: int64;
                         criticalData: string
                        ): boolean;
begin
  if (length(criticalData) <> (CRITICAL_DATA_LENGTH div 8)) then
    begin
    EFreeOTFEInvalidParameter.Create('Invalid CDB passed in for raw writing');
    end;

  Result := WriteRawVolumeData(
                               filename,
                               offsetWithinFile,
                               criticalData
                              );
end;


// ----------------------------------------------------------------------------
// Get the FreeOTFE driver to write data from the named file
// starting from the specified offset
// data - This will be set to the string representation of the raw data read
function TOTFEFreeOTFEBase.WriteRawVolumeData(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;
var
  retVal: boolean;
begin
  retVal := WriteRawVolumeDataSimple(
                                    filename,
                                    offsetWithinFile,
                                    data
                                   );
  if not(retVal) then
    begin
    retVal := WriteRawVolumeDataBounded(
                                       filename,
                                       offsetWithinFile,
                                       data
                                      );
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Get the FreeOTFE driver to write a critical data block from the named file
// starting from the specified offset
// criticalData - This should be set to the string representation of a raw
//                (encrypted) critical data block
function TOTFEFreeOTFEBase.WriteRawVolumeDataSimple(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;
var
  fStream: TFileStream;
  retVal: boolean;
  bytesWritten: integer;
begin
  retVal := FALSE;

  CheckActive();

  try
    fStream:= TFileStream.Create(filename, fmOpenReadWrite);
    try
      fStream.Position := offsetWithinFile;
      bytesWritten := fStream.Write(data[1], length(data));
      retval := (bytesWritten = length(data));
    finally
      fStream.Free();
    end;
  except
    // Just swallow exception; retval already set to FALSE
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WriteRawVolumeDataBounded(
                         filename: string;
                         offsetWithinFile: int64;
                         data: string
                        ): boolean;
begin
  // Bounded asjust not yet implemented - MORE COMPLEX THAN FIRST LOOKS
  //  - NEED TO ***READ*** THE PRE/POST DATA, THEN REWRITE IT BACK AGAIN 
  Result := WriteRawVolumeDataSimple(filename, offsetWithinFile, data);
end;


// ----------------------------------------------------------------------------
// Display dialog to allow user to select a partition
// Returns '' if the user cancels/on error, otherwise returns a partition
// identifier
function TOTFEFreeOTFEBase.SelectPartition(): string;
var
  dlg: TfrmSelectPartition;
  retVal: string;
begin
  retVal := '';

  dlg:= TfrmSelectPartition.Create(nil);
  try
    dlg.OTFEFreeOTFE := self;
    if (dlg.ShowModal() = mrOK) then
      begin
      retVal := dlg.Partition;
      end;

  finally
    dlg.Free();
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Generate a list of HDDs and partitions
function TOTFEFreeOTFEBase.HDDNumbersList(var DiskNumbers: TSDUArrayInteger): boolean;
var
  disk: integer;
  junk: string;
  currDevice: string;
  retVal: boolean;
  prevCursor: TCursor;
begin
  retVal:= TRUE;

  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    for disk:=0 to MAX_HDDS do
      begin
      // Check to see if the entire disk exists, before checking for partitions
      // 0 - Entire disk
      currDevice := SDUDeviceNameForPartition(disk, 0);
      if ReadRawVolumeCriticalData(
                                currDevice,
                                0,
                                junk
                               ) then
        begin
        SetLength(DiskNumbers, (length(DiskNumbers)+1));
        DiskNumbers[(length(DiskNumbers)-1)] := disk;
        end;
      end;

  finally
    Screen.Cursor := prevCursor;
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Generate a list of HDDs and partitions
// Note: The .Objects of hddDevices are set to the partition numbers
function TOTFEFreeOTFEBase.HDDDeviceList(hddDevices: TStringList; title: TStringList): boolean;
var
  diskNo: integer;
  retVal: boolean;
  prevCursor: TCursor;
begin
  retVal:= TRUE;

  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for diskNo:=0 to MAX_HDDS do
      begin
      HDDDeviceList(diskNo, hddDevices, title);
      end;

  finally
    Screen.Cursor := prevCursor;
  end;

  Result := retVal;
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.HDDDeviceList(diskNo: integer; hddDevices: TStringList; title: TStringList): boolean;
var
  partition: integer;
  junk: string;
  currDevice: string;
  retVal: boolean;
  prevCursor: TCursor;
begin
  retVal:= FALSE;

  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    // Check to see if the entire disk exists, before checking for partitions
    currDevice := SDUDeviceNameForPartition(diskNo, 0);
    if ReadRawVolumeCriticalData(
                              currDevice,
                              0,
                              junk
                             ) then
      begin
      retVal := TRUE;
      
      // The entire disk
      hddDevices.AddObject(currDevice, TObject(0));
      title.Add(SDUParamSubstitute(_('Hard disk #%1 (entire disk)'), [diskNo]));

      // Note: We start from 1; partition 0 indicates the entire disk
      for partition:=1 to MAX_PARTITIONS do
        begin
        currDevice := SDUDeviceNameForPartition(diskNo, partition);
        if ReadRawVolumeCriticalData(
                                  currDevice,
                                  0,
                                  junk
                                 ) then
          begin
          hddDevices.AddObject(currDevice, TObject(partition));
          title.Add(SDUParamSubstitute(_('Hard disk #%1, partition #%2'), [diskNo, partition]));
          end;
          
        end;

      end;


  finally
    Screen.Cursor := prevCursor;
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// Generate a list of HDDs and partitions
function TOTFEFreeOTFEBase.CDROMDeviceList(cdromDevices: TStringList; title: TStringList): boolean;
var
  cdrom: integer;
  junk: string;
  currDevice: string;
  retVal: boolean;
  prevCursor: TCursor;
begin
  retVal:= TRUE;

  prevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for cdrom:=0 to MAX_CDROMS do
      begin
      // Check to see if the entire disk exists, before checking for partitions
      currDevice := SDUDeviceNameForCDROM(cdrom);
      if ReadRawVolumeCriticalData(
                                currDevice,
                                0,
                                junk
                               ) then
        begin
        cdromDevices.Add(currDevice);
        title.Add(SDUParamSubstitute(_('CDROM drive #%1'), [cdrom]));
        end;

      end;


  finally
    Screen.Cursor := prevCursor;
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesCreate();
begin
  fCachedVersionID := VERSION_ID_NOT_CACHED;

  fCachedHashDriverDetails := TStringList.Create();
  fCachedCypherDriverDetails := TStringList.Create();
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesDestroy();
var
  i: integer;
begin
  fCachedVersionID := VERSION_ID_NOT_CACHED;

  for i:=0 to (fCachedHashDriverDetails.Count-1) do
    begin
    if (fCachedHashDriverDetails.Objects[i] <> nil) then
      begin
      Dispose(PFreeOTFEHashDriver(fCachedHashDriverDetails.Objects[i]));
      end;
    end;
  fCachedHashDriverDetails.Free();

  for i:=0 to (fCachedCypherDriverDetails.Count-1) do
    begin
    if (fCachedCypherDriverDetails.Objects[i] <> nil) then
      begin
      Dispose(PFreeOTFECypherDriver(fCachedCypherDriverDetails.Objects[i]));
      end;
    end;
  fCachedCypherDriverDetails.Free();

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesFlush();
begin
  CachesDestroy();
  CachesCreate();
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesAddHashDriver(hashDriver: string; hashDriverDetails: TFreeOTFEHashDriver);
var
  ptrRec: PFreeOTFEHashDriver;
begin
  new(ptrRec);
  ptrRec^ := hashDriverDetails;
  fCachedHashDriverDetails.AddObject(hashDriver, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CachesGetHashDriver(hashDriver: string; var hashDriverDetails: TFreeOTFEHashDriver): boolean;
var
  ptrRec: PFreeOTFEHashDriver;
  idx: integer;
  cacheHit: boolean;
begin
  cacheHit := FALSE;

  idx := fCachedHashDriverDetails.IndexOf(hashDriver);
  if (idx >= 0) then
    begin
    ptrRec := PFreeOTFEHashDriver(fCachedHashDriverDetails.Objects[idx]);
    hashDriverDetails := ptrRec^;
    cacheHit := TRUE;
    end;

  Result := cacheHit;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesAddCypherDriver(cypherDriver: string; cypherDriverDetails: TFreeOTFECypherDriver);
var
  ptrRec: PFreeOTFECypherDriver;
begin
  new(ptrRec);
  ptrRec^ := cypherDriverDetails;
  fCachedCypherDriverDetails.AddObject(cypherDriver, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CachesGetCypherDriver(cypherDriver: string; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  ptrRec: PFreeOTFECypherDriver;
  idx: integer;
  cacheHit: boolean;
begin
  cacheHit := FALSE;

  idx := fCachedCypherDriverDetails.IndexOf(cypherDriver);
  if (idx >= 0) then
    begin
    ptrRec := PFreeOTFECypherDriver(fCachedCypherDriverDetails.Objects[idx]);
    cypherDriverDetails := ptrRec^;
    cacheHit := TRUE;
    end;

  Result := cacheHit;
end;


// ----------------------------------------------------------------------------
// Check to see if there's at least one cypher and one hash algorithm
// available for use.
// Warn user and return FALSE if not.
// Returns TRUE if no problems
function TOTFEFreeOTFEBase.WarnIfNoHashOrCypherDrivers(): boolean;
var
  displayTitles: TStringList;
  kernelModeDriverNames: TStringList;
  hashGUIDs: TStringList;
  hashCount: integer;
  cypherCount: integer;
begin
  hashCount := 0;
  cypherCount := 0;

  displayTitles:= TStringList.Create();
  try
    kernelModeDriverNames:= TStringList.Create();
    try
      hashGUIDs:= TStringList.Create();
      try

        if (GetHashList(displayTitles, kernelModeDriverNames, hashGUIDs)) then
          begin
          hashCount := displayTitles.count;
          end;

        if (hashCount = 0) then
          begin
          SDUMessageDlg(
                        _('You do not appear to have any FreeOTFE hash drivers installed and started.')+SDUCRLF+
                        SDUCRLF+
                        _('If you have only just installed FreeOTFE, you may need to restart your computer.'),
                        mtError
                       );
          end;


        if (GetCypherList(displayTitles, kernelModeDriverNames, hashGUIDs)) then
          begin
          cypherCount := displayTitles.count;
          end;

        if (cypherCount = 0) then
          begin
          SDUMessageDlg(
                        _('You do not appear to have any FreeOTFE cypher drivers installed and started.')+SDUCRLF+
                        SDUCRLF+
                        _('If you have only just installed FreeOTFE, you may need to restart your computer.'),
                        mtError
                       );
          end;

      finally
        hashGUIDs.Free();
      end;
    finally
      kernelModeDriverNames.Free();
    end;
  finally
    displayTitles.Free();
  end;

  Result := (hashCount > 0) AND (cypherCount > 0);
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.AddStdDumpHeader(content: TStringList; title: string);
var
  driverVersion: string;
  platformID: string;
  envARCHITECTURE: string;
  envARCH_W3264: string;
  useEnvARCHITECTURE: string;
  useEnvARCH_W3264: string;
begin
  // This function may be called when the driver isn't running
  try
    driverVersion := VersionStr();
  except
    driverVersion := RS_UNKNOWN;
  end;

  content.Add(title);
  content.Add(StringOfChar('=', length(title)));
  // content.Add(''); - Newlines not needed; AddStdDumpSection(..) adds newlines
  AddStdDumpSection(content, _('Dump Created By'));
  useEnvARCHITECTURE := '---';
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCHITECTURE, envARCHITECTURE) then
  begin
    useEnvARCHITECTURE := envARCHITECTURE;
  end;
  useEnvARCH_W3264 := '---';
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCH_W3264, envARCH_W3264) then
  begin
    useEnvARCH_W3264 := envARCH_W3264;
  end;
  platformID := 'PC '+DriverType()+' ('+
                INSTALLED_OS_TITLE[SDUInstalledOS()]+'; '+
                inttostr(SDUOSCPUSize())+' bit ['+useEnvARCHITECTURE+'/'+useEnvARCH_W3264+']'+
                ')';
  content.Add(SDUParamSubstitute(_('Platform              : %1'), [platformID]));
  content.Add(SDUParamSubstitute(_('Application version   : %1'), ['v'+SDUGetVersionInfoString(ParamStr(0))]));
  content.Add(SDUParamSubstitute(_('Driver ID             : %1'), [driverVersion]));
//  content.Add(''); // Newlines not needed - added by following section header
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.AddStdDumpSection(content: TStringList; sectionTitle: string);
begin
  content.Add('');
  content.Add('');
  content.Add(sectionTitle);
  content.Add(StringOfChar('-', length(sectionTitle)));
end;


// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

END.


