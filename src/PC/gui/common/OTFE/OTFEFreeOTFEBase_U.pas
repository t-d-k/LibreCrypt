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
  Classes, Controls, dialogs,
  Forms,
  Graphics, Messages, OTFE_U,
  OTFEConsts_U,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_DriverCommon,
  OTFEFreeOTFE_DriverCypherAPI,
  OTFEFreeOTFE_DriverHashAPI,
  OTFEFreeOTFE_LUKSAPI,
  OTFEFreeOTFE_PKCS11,
  OTFEFreeOTFE_VolumeFileAPI,
  pkcs11_library,
  pkcs11_object,
  pkcs11_session,
  SDUGeneral, SysUtils, Windows;

const
  MIN_PDA_VOLUME_SIZE = 2;
  BYTES_IN_MEGABYTE   = 1024 * 1024;

  VERSION_ID_NOT_CACHED = VERSION_ID_FAILURE;

                                         // These are artifical maximums; they must be enough to store the results of
                                         // the DIOC calls when the amount of data output is variable
  MAX_DERIVED_KEY_LENGTH = 1024 * 1024;  // In *bits*
  MAX_MAC_LENGTH         = 1024 * 1024;  // In *bits*


  // Fairly arbitary number of CDROMs/HDDs/Partitions per HDD to check for
  // This is only used in the *old* partition selection dialog; it refers to
  // the partition *number* - unlike the drive layout struct, which is the
  // number of partition table records (which each indicate a partition number)
  MAX_CDROMS     = 8;
  MAX_HDDS       = 16;
  MAX_PARTITIONS = 16;

  VOL_FILE_EXTN = 'box';

  // This const is used in rounding up DIOC buffers to the nearest DWORD
  DIOC_BOUNDRY = 4;

  FREEOTFE_URL = 'http://DoxBox.eu/';

  LINUX_KEYFILE_DEFAULT_IS_ASCII = False;
  LINUX_KEYFILE_DEFAULT_NEWLINE  = nlLF;



resourcestring
  // Open/Save file filters...
  FILE_FILTER_FLT_VOLUMES  = 'Box files (*.box)|*.box|All files|*.*';
  FILE_FILTER_DFLT_VOLUMES = 'box';

  FILE_FILTER_FLT_KEYFILES  = 'Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_KEYFILES = 'cdb';

  FILE_FILTER_FLT_VOLUMESANDKEYFILES  =
    'Box files and keyfiles (*.box;*.cdb)|*.box;*.cdb|Box files (*.box)|*.box|Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_VOLUMESANDKEYFILES = '';

  FILE_FILTER_FLT_TEXTFILES  = 'Text files (*.txt)|*.txt|All files|*.*';
  FILE_FILTER_DFLT_TEXTFILES = 'txt';

  FILE_FILTER_FLT_CDBBACKUPS  = 'CDB backups (*.cdbBackup)|*.cdbBackup|All files|*.*';
  FILE_FILTER_DFLT_CDBBACKUPS = 'cdbBackup';

  FILE_FILTER_FLT_EXECUTABLES  = 'Executable files (*.exe)|*.exe|All files|*.*';
  FILE_FILTER_DFLT_EXECUTABLES = 'exe';

  FILE_FILTER_DFLT_LINUX_SETTINGS = 'les';
  FILE_FILTER_FLT_LINUX_SETTINGS  = 'Linux encryption settings (*.les)|*.les|All files|*.*';

  COUNT_BITS = '%1 bits';

const
  // xxx - This shouldn't really be used; we should be getting the actual
  // host disk's sector size and using that, instead of just *assuming* 512
  // bytes, as we do here
  // xxx - change this to get the host disk's geometry
  ASSUMED_HOST_SECTOR_SIZE = 512;

const
                              // Only used by the GUI
  DEFAULT_SALT_LENGTH = 256;  // In bits

  DEFAULT_KEY_ITERATIONS           = 2048;
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
  EFreeOTFEError = class (Exception);
  EFreeOTFEInvalidParameter = class (EFreeOTFEError);
  EFreeOTFENeedAdminPrivs = class (EFreeOTFEError);
  EFreeOTFEConnectFailure = class (EFreeOTFEError);
  EFreeOTFEObsoleteDriver = class (EFreeOTFEError);
  EFreeOTFEInvalidDerivedKeyLength = class (EFreeOTFEError);
  EFreeOTFEInternalError = class (EFreeOTFEError);  // Critical failure within this software
  EFreeOTFEDriverError = class (EFreeOTFEError);    // Critical failure within the driver


type
  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  TFreeOTFECypherMode = (focmNone, focmECB, focmCBC, focmLRW, focmXTS, focmUnknown);

  TFreeOTFEMountAs = (fomaFixedDisk, fomaRemovableDisk, fomaCD, fomaDVD, fomaUnknown);

const
  // These consts are used to supply a dummy sector ID/size to the sector
  // encrypt/decrypt routines, where there is none/it's inconseqeuential
  FREEOTFE_v1_DUMMY_SECTOR_ID: LARGE_INTEGER = (QuadPart: 0);
  FREEOTFE_v1_DUMMY_SECTOR_SIZE              = 512;

  DEFAULT_MOUNTAS = fomaFixedDisk;

resourcestring
  MOUNT_AS_FIXED_DISK     = 'Fixed disk';
  MOUNT_AS_REMOVABLE_DISK = 'Removable disk';
  MOUNT_AS_CD             = 'CD';
  MOUNT_AS_DVD            = 'DVD';

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
  FreeOTFEMountAsTitlePtr: array [TFreeOTFEMountAs] of Pointer =
    (@MOUNT_AS_FIXED_DISK, @MOUNT_AS_REMOVABLE_DISK, @MOUNT_AS_CD, @MOUNT_AS_DVD, @RS_UNKNOWN
    );

  // Indicate which of the above emulated devices are writable
  FreeOTFEMountAsCanWrite: array [TFreeOTFEMountAs] of Boolean = (
    True,
    True,
    False,
    False,
    False
    );

  // Decode the above into device types/media types
  FreeOTFEMountAsDeviceType: array [TFreeOTFEMountAs] of DWORD = (
    FILE_DEVICE_DISK,
    FILE_DEVICE_DISK,
    FILE_DEVICE_CD_ROM,
    FILE_DEVICE_DVD,
    FILE_DEVICE_UNKNOWN
    );

  // Decode the above into device types/media types
  FreeOTFEMountAsStorageMediaType: array [TFreeOTFEMountAs] of TFreeOTFEStorageMediaType = (
    mtFixedMedia,
    mtRemovableMedia,
    mtCD_ROM,
    mtDVD_ROM,
    mtUnknown
    );



  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFECypherModeTitlePtr: array [TFreeOTFECypherMode] of Pointer =
    (@CYPHER_MODE_TITLE_NONE, @CYPHER_MODE_TITLE_ECB, @CYPHER_MODE_TITLE_CBC,
    @CYPHER_MODE_TITLE_LRW, @CYPHER_MODE_TITLE_XTS, @RS_UNKNOWN
    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFECypherModeID: array [TFreeOTFECypherMode] of Integer = (
    0,     // CYPHER_MODE_NONE
    1,     // CYPHER_MODE_ECB
    2,     // CYPHER_MODE_CBC
    3,     // CYPHER_MODE_LRW
    4,     // CYPHER_MODE_XTS
    9999   // CYPHER_MODE_UNKNOWN
    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEMACTitlePtr: array [TFreeOTFEMACAlgorithm] of Pointer =
    (@MAC_TITLE_HASH, @MAC_TITLE_HMAC, @RS_UNKNOWN
    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFEMACID: array [TFreeOTFEMACAlgorithm] of Integer = (
    1,     // MAC_HASH
    2,     // MAC_HMAC
    9999   // MAC_UNKNOWN
    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEKDFTitlePtr: array [TFreeOTFEKDFAlgorithm] of Pointer =
    (@KDF_HASH_WITH_SALT, @KDF_PKCS5PBKDF2, @RS_UNKNOWN
    );

  // This definition is stored here, and not in OTFEFreeOTFE_DriverCypherAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFEKDFID: array [TFreeOTFEKDFAlgorithm] of Integer = (
    1,     // KDF_HASH
    2,     // KDF_PBKDF2
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
    Signature: array [0..10] of Ansichar;
    Version:   Integer;

    LinuxVolume:  Boolean;
    PKCS11SlotID: Integer;
  end;

  TOTFEFreeOTFEVolumeInfo = packed record
    DriveLetter: Ansichar;
    Filename:    Ansistring;
    DeviceName:  Ansistring;

    IVHashDevice:   Ansistring;
    IVHashGUID:     TGUID;
    IVCypherDevice: Ansistring;
    IVCypherGUID:   TGUID;

    MainCypherDevice: Ansistring;
    MainCypherGUID:   TGUID;

    Mounted:  Boolean;
    ReadOnly: Boolean;

    VolumeFlags:       DWORD;
    SectorIVGenMethod: TFreeOTFESectorIVGenMethod;

    MetaData:            Ansistring;
    MetaDataStructValid: Boolean;
    MetaDataStruct:      TOTFEFreeOTFEVolumeMetaData;
  end;



type
  TFreeOTFECypher_v1 = packed record
    CypherGUID: TGUID;

    Title:             Ansistring;
    Mode:              TFreeOTFECypherMode;
    KeySizeUnderlying: Integer;  // In bits
    BlockSize:         Integer;  // In bits
    VersionID:         DWORD;
  end;

  TFreeOTFECypher_v3 = packed record
    CypherGUID: TGUID;

    Title:             Ansistring;
    Mode:              TFreeOTFECypherMode;
    KeySizeUnderlying: Integer;  // In bits
    BlockSize:         Integer;  // In bits
    VersionID:         DWORD;
    KeySizeRequired:   Integer;  // In bits
  end;


  PFreeOTFECypherDriver = ^TFreeOTFECypherDriver;

  TFreeOTFECypherDriver = packed record
    DriverGUID: TGUID;

    // PC DRIVER - The MSDOS device name (to be passed to the FreeOTFE driver when mounting)
    // PC DLL    - The library filename
    LibFNOrDevKnlMdeName: Ansistring;

    // --- KERNEL DRIVER ONLY ---
    // The device name
    DeviceName:         Ansistring;
    // The MSDOS device name (to be used when connecting directly from userspace)
    DeviceUserModeName: Ansistring;
    // ------

    // The name of the driver, as should be displayed to the user
    Title:       Ansistring;
    VersionID:   DWORD;
    CypherCount: Integer;
    Cyphers:     array of TFreeOTFECypher_v3;
  end;


  TFreeOTFEHash = packed record
    HashGUID: TGUID;

    Title:     Ansistring;
    VersionID: DWORD;
    Length:    Integer;  // In bits
    BlockSize: Integer;  // In bits
  end;


  PFreeOTFEHashDriver = ^TFreeOTFEHashDriver;

  TFreeOTFEHashDriver = packed record
    DriverGUID: TGUID;

    // PC DRIVER - The MSDOS device name (to be passed to the FreeOTFE driver when mounting)
    // PC DLL    - The library filename
    LibFNOrDevKnlMdeName: Ansistring;

    // --- KERNEL DRIVER ONLY ---
    // The device name
    DeviceName:         Ansistring;
    // The MSDOS device name (to be used when connecting directly from userspace)
    DeviceUserModeName: Ansistring;
    // ------

    // The name of the driver, as should be displayed to the user
    Title:     Ansistring;
    VersionID: DWORD;
    HashCount: Integer;
    Hashes:    array of TFreeOTFEHash;
  end;


  // This definition is needed because we cannot pass an "array of Txyz" to a
  // function/procedure a "var" parameter, and the use SetLength(...) on that
  // parameter within the function/parameter
  TFreeOTFECypherDriverArray = array of TFreeOTFECypherDriver;
  TFreeOTFEHashDriverArray   = array of TFreeOTFEHashDriver;


  TOTFEFreeOTFEBase = class (TOTFE)
  PRIVATE
    // This is a bit hacky, but the Dump functionality does things that you
    // wouldn't normally do (i.e. it's a high level function that wants to
    // know about the inner workings of lower-level functionality; CDB
    // processing)
    fdumpFlag:                    Boolean;
    fdumpCriticalDataKey:         Ansistring;
    fdumpCheckMAC:                String;
    fdumpPlaintextEncryptedBlock: String;
    fdumpVolumeDetailsBlock:      String;
  PROTECTED
    fpasswordChar:             Char;
    fallowNewlinesInPasswords: Boolean;
    fallowTabsInPasswords:     Boolean;
    fcachedVersionID:          DWORD;

    fPKCS11Library: TPKCS11Library;

    // Cached information: The strings in the list are kernel mode device
    // names, the objects are TFreeOTFEHashDriver/TFreeOTFECypherDriver
    fCachedHashDriverDetails:   TStringList;
    fCachedCypherDriverDetails: TStringList;

    // Set the component active/inactive
    procedure SetActive(status: Boolean); OVERRIDE;

    // Connect/disconnect to the main FreeOTFE device driver
    function Connect(): Boolean; VIRTUAL; ABSTRACT;
    function Disconnect(): Boolean; VIRTUAL; ABSTRACT;


    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations

    // volFilename - This is the filename of the file/partition as seen by the
    //               user mode software
    function MountDiskDevice(deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      volFilename: String; volumeKey: Ansistring;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod; volumeIV: Ansistring;
      ReadOnly: Boolean; IVHashDriver: Ansistring; IVHashGUID: TGUID;
      IVCypherDriver: Ansistring; IVCypherGUID: TGUID; mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID; VolumeFlags: Integer; metaData: TOTFEFreeOTFEVolumeMetaData;
      offset: Int64 = 0; size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType =
      mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; VIRTUAL; ABSTRACT;



    function CreateMountDiskDevice(volFilename: String; volumeKey: Ansistring;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod; volumeIV: Ansistring;
      ReadOnly: Boolean; IVHashDriver: Ansistring; IVHashGUID: TGUID;
      IVCypherDriver: Ansistring; IVCypherGUID: TGUID; mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID; VolumeFlags: Integer; DriveLetter: Ansichar;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      offset: Int64 = 0; size: Int64 = 0; MetaData_LinuxVolume: Boolean = False;
    // Linux volume
      MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
      MountMountAs: TFreeOTFEMountAs = fomaFixedDisk;
                                        // PC kernel drivers *only* - ignored otherwise
      mountForAllUsers: Boolean = True  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; VIRTUAL; ABSTRACT;

    function DismountDiskDevice(deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      emergency: Boolean): Boolean;
      VIRTUAL; ABSTRACT;


    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    function ReadRawVolumeData(filename: String; offsetWithinFile: Int64; dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean;

    // This executes a simple call to the driver to read dataLength bytes from
    // offsetWithinFile
    // Note: Will probably not work when reading directly from partitions;
    //       they typically need read/writes carried out in sector sized blocks
    //        - see ReadRawVolumeDataBounded for this.
    function ReadRawVolumeDataSimple(filename: String; offsetWithinFile: Int64;
      dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean; VIRTUAL;

    // This function is the same as ReadRawVolumeDataSimple(...), but will round
    // the offsetWithinFile down to the nearest sector offset, read in complete
    // sectors, and *only* *return "datalength" *bytes*
    // This function is included as reading from partitions must be carried out
    // in sector size blocks
    function ReadRawVolumeDataBounded(filename: String; offsetWithinFile: Int64;
      dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean;

    function WriteRawVolumeData(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean;

    function WriteRawVolumeDataSimple(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean; VIRTUAL;

    function WriteRawVolumeDataBounded(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean;

    function ReadWritePlaintextToVolume(readNotWrite: Boolean; volFilename: String;
      volumeKey: Ansistring; sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      IVHashDriver: Ansistring; IVHashGUID: TGUID; IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID; mainCypherDriver: Ansistring; mainCypherGUID: TGUID;
      VolumeFlags: Integer; mountMountAs: TFreeOTFEMountAs; dataOffset: Int64;
                             // Offset from within mounted volume from where to read/write data
      dataLength: Integer;   // Length of data to read/write. In bytes
      var data: Ansistring;  // Data to read/write

      offset: Int64 = 0; size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia): Boolean; VIRTUAL; ABSTRACT;


    // ---------
    // Misc functions

    // Generate a Linux volume's volume key
    function GenerateLinuxVolumeKey(hashDriver: Ansistring; hashGUID: TGUID;
      userKey: Ansistring; seed: Ansistring; hashWithAs: Boolean;
      targetKeylengthBits: Integer; var volumeKey: TSDUBytes): Boolean;

    function GetNextDriveLetter(): Ansichar; OVERLOAD;
    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Ansichar): Ansichar;
      OVERLOAD; VIRTUAL; ABSTRACT;

    // Returns the type of driver used; either "DLL driver" or "Kernel driver"
    function DriverType(): String; VIRTUAL; ABSTRACT;

    // ---------
    // Convert critical data area to/from a string representation

    // Note: This function does *not* append random padding data
    function BuildVolumeDetailsBlock(volumeDetailsBlock: TVolumeDetailsBlock;
      var stringRep: Ansistring): Boolean;
    // Note: Ignores any random padding data
    function ParseVolumeDetailsBlock(stringRep: Ansistring;
      var volumeDetailsBlock: TVolumeDetailsBlock): Boolean;


    // Return a nicely formatted string, decoding one bit of a bitmapped value
    function DumpCriticalDataToFileBitmap(Value: DWORD; bit: Integer;
      unsetMeaning: String; setMeaning: String): String;

    // Convert a user-space volume filename to a format the kernel mode driver
    // can understand
    function GetKernelModeVolumeFilename(userModeFilename: String): String;
    // Convert a kernel mode driver volume filename to format user-space
    // filename
    function GetUserModeVolumeFilename(kernelModeFilename: String): String;

    function MetadataSig(): Ansistring;

    function PopulateVolumeMetadataStruct(LinuxVolume: Boolean; PKCS11SlotID: Integer;
      var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;

    function ParseVolumeMetadata(metadataAsString: String;
      var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;

    procedure VolumeMetadataToString(metadata: TOTFEFreeOTFEVolumeMetaData;
      var metadataAsString: Ansistring);

    // ---------
    // Internal caching functions
    procedure CachesCreate(); VIRTUAL;
    procedure CachesDestroy(); VIRTUAL;
    // Add a hash to the cache...
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    procedure CachesAddHashDriver(hashDriver: String; hashDriverDetails: TFreeOTFEHashDriver);
    // Get a hash from the cache...
    // Note: A cache MISS will cause this to return FALSE
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    function CachesGetHashDriver(hashDriver: String;
      var hashDriverDetails: TFreeOTFEHashDriver): Boolean;
    // Add a cypher to the cache...
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    procedure CachesAddCypherDriver(cypherDriver: Ansistring;
      cypherDriverDetails: TFreeOTFECypherDriver);
    // Get a cypher from the cache...
    // Note: A cache MISS will cause this to return FALSE
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    function CachesGetCypherDriver(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;

    // v1 / v3 Cypher API functions
    // Note: This shouldn't be called directly - only via wrapper functions!
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    function _GetCypherDriverCyphers_v1(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; VIRTUAL; ABSTRACT;
    function _GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; VIRTUAL; ABSTRACT;

  PUBLIC
    AdvancedMountDlg:    Boolean;
    RevertVolTimestamps: Boolean;

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
procedure DebugMsgBinaryPtr(msg: PAnsiChar; len:integer);
{$ENDIF}

    //////////////////////////////////////////////
    // was in OTFEFreeOTFE_mntLUKS_AFS_Intf.pas
    //////////////////////////////////////////////
    // ------------------------
    // "Private"
    function GetRandom(len: Integer): String;

    function RepeatedHash(hashKernelModeDeviceName: Ansistring; hashGUID: TGUID;
      input: Ansistring; var output: TSDUBytes): Boolean;
    function Process(stripeData: Ansistring; totalBlockCount: Integer;
      blockLength: Integer; hashKernelModeDeviceName: String; hashGUID: TGUID;
      out output: TSDUBytes): Boolean;

    function GetBlock(stripeData: Ansistring; totalBlockCount: Integer;
      blockLength: Integer; blockNumber: Integer; var output: Ansistring): Boolean;


    // ------------------------
    // "Public"

    function AFSplit(input: Ansistring; n: Integer; hashKernelModeDeviceName: String;
      hashGUID: TGUID; var output: Ansistring): Boolean;

    function AFMerge(input: Ansistring; n: Integer; hashKernelModeDeviceName: String;
      hashGUID: TGUID; out output: TSDUBytes): Boolean;


    // ------------------------


    //////////////////////////////////////////////
    // was in OTFEFreeOTFE_mntLUKS_Intf.pas
    //////////////////////////////////////////////

    // ------------------------
    // "Private"

    function ParseLUKSHeader(rawData: Ansistring; var LUKSheader: TLUKSHeader): Boolean;
    function IdentifyHash(hashSpec: String; var hashKernelModeDeviceName: String;
      var hashGUID: TGUID): Boolean;
    function IdentifyCypher(cypherName: String; keySizeBits: Integer;
      cypherMode: String; baseIVCypherOnHashLength: Boolean;
      var cypherKernelModeDeviceName: String; var cypherGUID: TGUID;
      var sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      var IVHashKernelModeDeviceName: String; var IVHashGUID: TGUID;
      var IVCypherKernelModeDeviceName: String; var IVCypherGUID: TGUID): Boolean;
    function IdentifyCypher_SearchCyphers(cypherDrivers: array of TFreeOTFECypherDriver;
      cypherName: String; keySizeBits: Integer; useCypherMode: TFreeOTFECypherMode;
      var cypherKernelModeDeviceName: String; var cypherGUID: TGUID): Boolean;
    function PrettyHex(data: Pointer; bytesCount: Integer): Ansistring;
    function StringHasNumbers(checkString: String): Boolean;

    // ------------------------
    // "Public"

    // Determine if the specified volume is a Linux LUKS volume
    function IsLUKSVolume(volumeFilename: String): Boolean;

    // Read a LUKS key in from a file
    function ReadLUKSKeyFromFile(filename: String; treatAsASCII: Boolean;
      ASCIINewline: TSDUNewline; out key: PasswordString): Boolean;


    // If keyfile is set, ignore userKey and read the key from the file
    function DumpLUKSDataToFile(filename: String; userKey: Ansistring;
      keyfile: String; keyfileIsASCII: Boolean; keyfileNewlineType: TSDUNewline;
      baseIVCypherOnHashLength: Boolean; dumpFilename: String): Boolean;

    // Note: This will only populate the LUKS members of the header; it will not
    //       populate the FreeOTFE driver details
    function ReadLUKSHeader(filename: String; var LUKSheader: TLUKSHeader): Boolean;

    // If the hash and cypher members of the LUKS header passed in are populated,
    // this function will populate struct with the the FreeOTFE driver details
    // (e.g. driver name/GUID) that correspond to that hash/cypher
    // Note: Must be called *after* ReadLUKSHeader(...)
    function MapToFreeOTFE(baseIVCypherOnHashLength: Boolean;
      var LUKSheader: TLUKSHeader): Boolean;

    function RecoverMasterKey(filename: String; userPassword: Ansistring;
      baseIVCypherOnHashLength: Boolean; var LUKSHeader: TLUKSHeader;
      var keySlot: Integer; var keyMaterial: TSDUBytes): Boolean;

    //////////////////////////////////////////////
    // was in OTFEFreeOTFE_mntLUKS_Impl.pas
    //////////////////////////////////////////////

    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;
    function Mount(volumeFilename: Ansistring; ReadOnly: Boolean = False): Ansichar;
      OVERLOAD; OVERRIDE;
    function Mount(volumeFilenames: TStringList; var mountedAs: Ansistring;
      ReadOnly: Boolean = False): Boolean; OVERLOAD; OVERRIDE;
    function CanMountDevice(): Boolean; OVERRIDE;
    function MountDevices(): Ansistring; OVERRIDE;
    function Dismount(volumeFilename: String; emergency: Boolean = False): Boolean;
      OVERLOAD; OVERRIDE;
    // Although this Dismount(...) is introduced as virtual/abstractin TOTFE,
    // it needs to be shown here as well in order to prevent compiler error
    // ("Ambiguous overloaded call to 'Dismount'") in the above Dismount(...)
    // implementation
    function Dismount(driveLetter: Ansichar; emergency: Boolean = False): Boolean;
      OVERLOAD; OVERRIDE; ABSTRACT;
    function Title(): String; OVERLOAD; OVERRIDE;
    function VersionStr(): String; OVERLOAD; OVERRIDE;
    function IsEncryptedVolFile(volumeFilename: String): Boolean; OVERRIDE;
    function GetVolFileForDrive(driveLetter: Ansichar): String; OVERRIDE;
    function GetDriveForVolFile(volumeFilename: String): Ansichar; OVERRIDE;
    function GetMainExe(): String; OVERRIDE;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE specific functions

    // Mount file(s) assuming specific volume type
    // Note: The volumeFilename(s) passed in can EITHER by files on the local
    //       filesystem (e.g. "C:\myfile.dat"), OR partitions
    //       (e.g. "\Device\Harddisk1\Partition3")
    // Note: "keyfile" is only supported for LUKS volumes atm
    // Note: If "keyfile" is supplied, the contents of this file will be used
    //       and "password" will be ignored
    function MountLinux(volumeFilename: String; ReadOnly: Boolean = False;
      lesFile: String = ''; password: Ansistring = ''; keyfile: String = '';
      keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
      offset: ULONGLONG = 0; silent: Boolean = False; forceHidden: Boolean = False): Ansichar;
      OVERLOAD;
    function MountLinux(volumeFilenames: TStringList; var mountedAs: Ansistring;
      ReadOnly: Boolean = False; lesFile: String = ''; password: Ansistring = '';
      keyfile: String = ''; keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
      offset: ULONGLONG = 0; silent: Boolean = False; forceHidden: Boolean = False): Boolean;
      OVERLOAD;
    // Note: The MountLUKS(...) functions will be called automatically if
    //       MountLinux(...) is called with a LUKS volume
    function MountLUKS(volumeFilename: String; ReadOnly: Boolean = False;
      password: Ansistring = ''; keyfile: String = '';
      keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
      silent: Boolean = False): Ansichar; OVERLOAD;
    function MountLUKS(volumeFilenames: TStringList; var mountedAs: Ansistring;
      ReadOnly: Boolean = False; password: Ansistring = ''; keyfile: String = '';
      keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
      silent: Boolean = False): Boolean; OVERLOAD;
    function MountFreeOTFE(volumeFilename: String; ReadOnly: Boolean = False;
      keyfile: String = ''; password: Ansistring = ''; offset: ULONGLONG = 0;
      noCDBAtOffset: Boolean = False; silent: Boolean = False;
      saltLength: Integer = DEFAULT_SALT_LENGTH;
      keyIterations: Integer = DEFAULT_KEY_ITERATIONS): Ansichar; OVERLOAD;
    function MountFreeOTFE(volumeFilenames: TStringList; var mountedAs: Ansistring;
      ReadOnly: Boolean = False; keyfile: String = ''; password: Ansistring = '';
      offset: ULONGLONG = 0; noCDBAtOffset: Boolean = False; silent: Boolean = False;
      saltLength: Integer = DEFAULT_SALT_LENGTH;
      keyIterations: Integer = DEFAULT_KEY_ITERATIONS): Boolean; OVERLOAD;
    function MountFreeOTFE(volumeFilenames: TStringList; UserKey: Ansistring;
      Keyfile: String; CDB: Ansistring; // CDB data (e.g. already read in PKCS#11 token)
                                        // If CDB is specified, "Keyfile" will be ignored
      SlotID: Integer;
                                      // Set to PKCS#11 slot ID if PKCS#11 slot was *actually* used for mounting
      PKCS11Session: TPKCS11Session;  // Set to nil if not needed
      PKCS11SecretKeyRecord: PPKCS11SecretKey;  // Set to nil if not needed
      KeyIterations: Integer; UserDriveLetter: ansichar;
    // PC kernel drivers *only* - ignored otherwise
      MountReadonly: Boolean; MountMountAs: TFreeOTFEMountAs;
    // PC kernel drivers *only* - ignored otherwise
      Offset: Int64; OffsetPointsToCDB: Boolean; SaltLength: Integer;  // In *bits*
      MountForAllUsers: Boolean;
    // PC kernel drivers *only* - ignored otherwise
      var mountedAs: Ansistring): Boolean;
      OVERLOAD;

    // Get information on a mounted volume
    function GetVolumeInfo(driveLetter: Ansichar;
      var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
      VIRTUAL; ABSTRACT;

    // Get details of all hash drivers and the hashes they support
    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean; VIRTUAL; ABSTRACT;
    // Get details of a single hash driver, and all it's hashes
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    function GetHashDriverHashes(hashDriver: Ansistring;
      var hashDriverDetails: TFreeOTFEHashDriver): Boolean; VIRTUAL; ABSTRACT;

    // Get details of all cypher drivers and the cyphers they support
    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean;
      VIRTUAL; ABSTRACT;
    // Get details of a single cypher driver, and all it's cyphers
    // cypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
    function GetCypherDriverCyphers(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; VIRTUAL;

    // These two additional "helper" functions will *clear* and repopulate the TStringLists supplied with:
    //   A displayable title for each hash/cypher
    //   The kernel mode driver name for each hash/cypher
    //   The GUID for each hash/cypher
    function GetHashList(var hashDispTitles, hashKernelModeDriverNames, hashGUIDs:
      TStringList): Boolean;
    function GetCypherList(var cypherDispTitles, cypherKernelModeDriverNames,
      cypherGUIDs: TStringList): Boolean;

    // Two additional "helper" functions...
    // Generate a prettyprinted title for a specific cypher
    // Returns '' on error
    function GetCypherDisplayTitle(cypher: TFreeOTFECypher_v3): String;
    // (More technical version)
    function GetCypherDisplayTechTitle(cypher: TFreeOTFECypher_v3): String;
    // Generate a prettyprinted title for a specific hash
    // Returns '' on error
    function GetHashDisplayTitle(hash: TFreeOTFEHash): String;
    // (More technical version)
    function GetHashDisplayTechTitle(hash: TFreeOTFEHash): String;


    // Display a standard dialog with details of the specific hash identified
    procedure ShowHashDetailsDlg(driverName: String; hashGUID: TGUID);
    // Display a standard dialog with details of the specific cypher identified
    procedure ShowCypherDetailsDlg(driverName: String; cypherGUID: TGUID);


    function GetSpecificHashDetails(hashKernelModeDeviceName: Ansistring;
      hashGUID: TGUID; var hashDetails: TFreeOTFEHash): Boolean;
    function GetSpecificCypherDetails(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; var cypherDetails: TFreeOTFECypher_v3): Boolean;


    // Display dialog to allow user to select a partition
    // Returns '' if the user cancels/on error, otherwise returns a partition
    // identifier
    function SelectPartition(): String;
    function HDDNumbersList(var DiskNumbers: TSDUArrayInteger): Boolean;
    function HDDDeviceList(hddDevices: TStringList; title: TStringList): Boolean; OVERLOAD;
    function HDDDeviceList(diskNo: Integer; hddDevices: TStringList; title: TStringList): Boolean;
      OVERLOAD;
    function CDROMDeviceList(cdromDevices: TStringList; title: TStringList): Boolean;


    // Check to see if there's at least one cypher and one hash algorithm
    // available for use.
    // Warn user and return FALSE if not.
    function WarnIfNoHashOrCypherDrivers(): Boolean;

    // Convert a version ID into a prettyprinted version string
    function VersionIDToStr(versionID: DWORD): String;

    // Handy function!
    procedure AddStdDumpHeader(content: TStringList; title: String);
    procedure AddStdDumpSection(content: TStringList; sectionTitle: String);

    // ---------
    // Algorithm driver functions

    // Encrypt data using the cypher/cypher driver identified
    // Encrypted data returned in "cyphertext"
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _EncryptData(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; var key: TSDUBytes; var IV: Ansistring;
      var plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
    // Decrypt data using the cypher/cypher driver identified
    // Decrypted data returned in "plaintext"
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _DecryptData(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; var key: TSDUBytes; var IV: Ansistring;
      var cyphertext: Ansistring; var plaintext: Ansistring): Boolean;

    // Encrypt "sector" data using the cypher/cypher driver identified
    // Encrypted data returned in "cyphertext"
    function EncryptSectorData(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; SectorID: LARGE_INTEGER; SectorSize: Integer;
      var key: TSDUBytes; var IV: Ansistring; var plaintext: Ansistring;
      var cyphertext: Ansistring): Boolean;
    // Decrypt "sector" data using the cypher/cypher driver identified
    // Decrypted data returned in "plaintext"
    function DecryptSectorData(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; SectorID: LARGE_INTEGER; SectorSize: Integer;
      var key: TSDUBytes; var IV: Ansistring; var cyphertext: Ansistring;
      var plaintext: Ansistring): Boolean;

    // Combined...
    // Note: This is pretty much redundant with EncryptSectorData(...)/DecryptSectorData(...)
    function _EncryptDecryptData(encryptFlag: Boolean;
    // driver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
      cypherDriver: Ansistring; cypherGUID: TGUID; var key: TSDUBytes;
      var IV: Ansistring; var inData: Ansistring; var outData: Ansistring): Boolean;
      VIRTUAL; ABSTRACT;

    function EncryptDecryptSectorData(encryptFlag: Boolean;
    // driver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
      cypherDriver: Ansistring; cypherGUID: TGUID; SectorID: LARGE_INTEGER;
      SectorSize: Integer; var key: TSDUBytes; var IV: Ansistring;
      var inData: Ansistring; var outData: Ansistring): Boolean;
      VIRTUAL; ABSTRACT;

    // Hash data using the hash driver identified
    // Result returned in "hashOut"
    // driver - Kernel drivers: hashKernelModeDeviceName
    //          DLL drivers:    hashLibFilename
    function HashData(hashDriver: Ansistring; hashGUID: TGUID; const data: TSDUBytes;
      out hashOut: TSDUBytes): Boolean; VIRTUAL; ABSTRACT;

    // Generate MAC of data using hash driver/encryption driver identified
    // Note: "tBits" is in *bits* not *bytes*
    // tBits - Set the the number of bits to return; set to -ve value for no
    //         truncation/padding
    //         If tBits is larger than the number of bits the MAC would normally
    //         be, then the MAC will be right-padded with NULLs
    //         If tBits is set to a -ve number, then this function will return
    //         an MAC of variable length
    function MACData(macAlgorithm: TFreeOTFEMACAlgorithm;
    // HashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
      HashDriver: Ansistring; HashGUID: TGUID;
    // CypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //                DLL drivers:    cypherLibFilename
      CypherDriver: Ansistring; CypherGUID: TGUID; var key: PasswordString;
      var data: Ansistring; var MACOut: Ansistring; tBits: Integer = -1): Boolean;
      VIRTUAL; ABSTRACT;


    // Key derivation function
    function DeriveKey(kdfAlgorithm: TFreeOTFEKDFAlgorithm;
    // HashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
      HashDriver: Ansistring; HashGUID: TGUID;
    // CypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //                DLL drivers:    cypherLibFilename
      CypherDriver: Ansistring; CypherGUID: TGUID; Password: PasswordString;
      Salt: TSDUBytes; Iterations: Integer; dkLenBits: Integer;  // In *bits*
      out DK: TSDUBytes): Boolean; VIRTUAL; ABSTRACT;



    // ---------
    // Volume creation
    function CreateFreeOTFEVolumeWizard(): Boolean;
    function CreateLinuxVolumeWizard(): Boolean;
    function CreateLinuxGPGKeyfile(): Boolean;

    function WizardChangePassword(): Boolean;
    function WizardCreateKeyfile(): Boolean;


    // ---------
    // Critical data block handling
    function ReadVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      userPassword: Ansistring; saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean;

    function ReadVolumeCriticalData_CDB(CDB: Ansistring; userPassword: Ansistring;
      saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean; OVERLOAD;

    function ReadVolumeCriticalData_CDB_v1(CDB: Ansistring; userPassword: Ansistring;
      saltLength: Integer;  // In bits

      var volumeDetails: TVolumeDetailsBlock; var CDBMetaData: TCDBMetaData): Boolean;
      OVERLOAD;

    function ReadVolumeCriticalData_CDB_v2(CDB: Ansistring; userPassword: Ansistring;
      saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean; OVERLOAD;

    function ChooseFromValid(validVolumeDetails: TVolumeDetailsBlockArray;
      validCDBMetaDatas: TCDBMetaDataArray; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean;

    // Write out a critical data block
    // If the file specified doesn't already exist, it will be created
    // ALL members of these two "volumeDetails" and "CDBMetaData" structs MUST
    // be populated
    // *Except* for volumeDetails.CDBFormatID - this is force set to the latest value
    function WriteVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      userPassword: PasswordString; salt: TSDUBytes; keyIterations: Integer;
      volumeDetails: TVolumeDetailsBlock; CDBMetaData: TCDBMetaData
    // Note: If insufficient is supplied, the write will
    //       *fail*
    //       The maximum that could possibly be required
    //       is CRITICAL_DATA_LENGTH bits.
      ): Boolean; OVERLOAD;

    function BackupVolumeCriticalData(srcFilename: String; srcOffsetWithinFile: Int64;
      destFilename: String): Boolean;

    function RestoreVolumeCriticalData(srcFilename: String; destFilename: String;
      destOffsetWithinFile: Int64): Boolean;

    function DumpCriticalDataToFile(volFilename: String; offsetWithinFile: Int64;
      userPassword: Ansistring; saltLength: Integer;  // In bits
      keyIterations: Integer; dumpFilename: String): Boolean;

    function ChangeVolumePassword(filename: String; offsetWithinFile: Int64;
      oldUserPassword: PasswordString; oldSaltLength: Integer;  // In bits
      oldKeyIterations: Integer; newUserPassword: PasswordString;
      newSaltBytes: TSDUBytes; newKeyIterations: Integer;
      newRequestedDriveLetter: ansichar): Boolean;

    function CreateKeyfile(srcFilename: String; srcOffsetWithinFile: Int64;
      srcUserPassword: PasswordString; srcSaltLength: Integer;  // In bits
      srcKeyIterations: Integer; keyFilename: String; keyfileUserPassword: PasswordString;
      keyfileSaltBytes: TSDUBytes; keyfileKeyIterations: Integer;
      keyfileRequestedDrive: ansichar): Boolean;

    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    // Get the FreeOTFE driver to read a critical data block from the named file
    // starting from the specified offset
    function ReadRawVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      var criticalData: Ansistring): Boolean;
      OVERLOAD;

    // Get the FreeOTFE driver to write a critical data block from the named file
    // starting from the specified offset
    function WriteRawVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      criticalData: Ansistring): Boolean;
      OVERLOAD;

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
                         var AnsiData: Ansistring
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
                    volumeKey: TSDUBytes;
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
                    var data: ansistring;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean;

{$ENDIF}

    // Return TRUE/FALSE, depending on whether the specified USER MODE volume
    // filename refers to a partition/file
    function IsPartition_UserModeName(userModeFilename: String): Boolean;

  PUBLISHED
    property PasswordChar: Char Read fPasswordChar Write fPasswordChar;
    property AllowNewlinesInPasswords: Boolean Read fAllowNewlinesInPasswords
      Write fAllowNewlinesInPasswords DEFAULT True;
    property AllowTabsInPasswords: Boolean Read fAllowTabsInPasswords
      Write fAllowTabsInPasswords DEFAULT False;
    property PKCS11Library: TPKCS11Library Read fPKCS11Library Write fPKCS11Library;
  end;


function FreeOTFEMountAsTitle(mountAs: TFreeOTFEMountAs): String;
function FreeOTFECypherModeTitle(cypherMode: TFreeOTFECypherMode): String;
function FreeOTFEMACTitle(MACAlgorithm: TFreeOTFEMACAlgorithm): String;
function FreeOTFEKDFTitle(KDFAlgorithm: TFreeOTFEKDFAlgorithm): String;


implementation

uses
  SDUi18n,
{$IFDEF FREEOTFE_DEBUG}
{$IFDEF _GEXPERTS}
  DbugIntf,  // GExperts
{$ENDIF}
{$ENDIF}
  //SDU
  SDUDialogs,
  SDUEndianIntegers,
  SDUProgressDlg,
  SDURandPool,
  //DoxBox
  Math, OTFEFreeOTFE_DriverControl,
  OTFEFreeOTFE_frmCypherInfo,
  OTFEFreeOTFE_frmDriverControl,
  OTFEFreeOTFE_frmHashInfo,
  OTFEFreeOTFE_frmKeyEntryFreeOTFE,
  OTFEFreeOTFE_frmKeyEntryLinux,
  OTFEFreeOTFE_frmKeyEntryLUKS,
  OTFEFreeOTFE_frmNewVolumeSize,
  OTFEFreeOTFE_frmPKCS11Management,
  OTFEFreeOTFE_frmPKCS11Session,
  OTFEFreeOTFE_frmSelectHashCypher,
  OTFEFreeOTFE_frmSelectPartition,
  OTFEFreeOTFE_frmVolumeType,
  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile,
  OTFEFreeOTFE_frmWizardCreateVolume,
            // Required for min
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for GUIDToString
  WinSvc;   // Required for SERVICE_ACTIVE



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
  // Version ID for arbitary metadata passed to, and returned from, driver
  METADATA_VERSION = 1;

 //////////////////////////////////////////////
 // was in OTFEFreeOTFE_mntLUKS_AFS_Impl.pas
 //////////////////////////////////////////////



// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetRandom(len: Integer): String;
var
  retVal: String;
  i:      Integer;
begin
  for i := 1 to len do begin
    retVal := retVal + Char(random(256));
    { TODO 3 -otdk -csecurity : only used in AFSKit - checks works OK }
    //    retVal := retVal + char(0);
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Hash input repeatedly until a string with the same length is produced
function TOTFEFreeOTFEBase.RepeatedHash(hashKernelModeDeviceName: Ansistring;
  hashGUID: TGUID; input: Ansistring; var output: TSDUBytes): Boolean;
var
  hashOutput:          TSDUBytes;
  digestSizeBytes:     Integer;
  inputDivDigestBytes: Integer;
  inputModDigestBytes: Integer;
  i, j:                Integer;
  tmpString:           TSDUBytes;
  hashDetails:         TFreeOTFEHash;
begin
  SDUInitAndZeroBuffer(0, output);
  //  output := '';

  digestSizeBytes     := 0;
  inputDivDigestBytes := 0;
  inputModDigestBytes := 0;

  Result := GetSpecificHashDetails(hashKernelModeDeviceName, hashGUID, hashDetails);

  if (Result) then begin
    // Hash output
    digestSizeBytes := (hashDetails.Length div 8);

    inputDivDigestBytes := (length(input) div digestSizeBytes);
    inputModDigestBytes := (length(input) mod digestSizeBytes);

    for i := 0 to (inputDivDigestBytes - 1) do begin
      tmpString := SDUStringToSDUBytes(SDUBigEndian32ToString(SDUDWORDToBigEndian32(i)));
      for j := 0 to (digestSizeBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(tmpString, Ord(input[((i * digestSizeBytes) + j + 1)]));
        //        tmpString := tmpString + input[((i * digestSizeBytes) + j + 1)];
      end;

      Result := HashData(hashKernelModeDeviceName, hashGUID, tmpString, hashOutput);

      if not (Result) then begin
        break;
      end;
      SDUAddArrays(output, hashOutput);

      //      output := output + hashOutput;
    end;  // for i:=0 to (inputDivDigestBytes-1) do

  end;

  if (Result) then begin
    if (inputModDigestBytes > 0) then begin
      tmpString := SDUStringToSDUBytes(
        SDUBigEndian32ToString(SDUDWORDToBigEndian32(inputDivDigestBytes)));
      for j := 0 to (inputModDigestBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(tmpString, Ord(input[((inputDivDigestBytes * digestSizeBytes) + j + 1)]));
        //        tmpString := tmpString + input[((inputDivDigestBytes * digestSizeBytes) + j + 1)];
      end;

      Result := HashData(hashKernelModeDeviceName, hashGUID, tmpString, hashOutput);

      for j := 0 to (inputModDigestBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(output, hashOutput[j]);
        //        output := output + hashOutput[j + 1];
      end;
    end;

  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.AFSplit(input: Ansistring; n: Integer;
  hashKernelModeDeviceName: String; hashGUID: TGUID; var output: Ansistring): Boolean;
var
  i, j:             Integer;
  randomStripeData: Ansistring;
  calcStripe:       Ansistring;
  processedStripes: TSDUBytes;
  blockLength:      Integer;
begin
  Result := True;

  blockLength := length(input);

  randomStripeData := '';
  for i := 1 to (n - 1) do begin
    // Get random block "i"
    randomStripeData := randomStripeData + GetRandom(blockLength);
  end;

  Process(
    randomStripeData,
    n,
    blockLength,
    hashKernelModeDeviceName,
    hashGUID,
    processedStripes
    );

  // XOR last hash output with input
  calcStripe := '';
  for j := 1 to blockLength do begin
    calcStripe := calcStripe + Ansichar((Ord(processedStripes[j]) xor Ord(input[j])));
  end;

  // Store result as random block "n" as output
  output := randomStripeData + calcStripe;
end;


 // ----------------------------------------------------------------------------
 // Process blocks 1 - (n-1)
function TOTFEFreeOTFEBase.Process(stripeData: Ansistring; totalBlockCount: Integer;
  blockLength: Integer; hashKernelModeDeviceName: String; hashGUID: TGUID;
  out output: TSDUBytes): Boolean;
var
  prev:        TSDUBytes;
  i, j:        Integer;
  tmpData:     Ansistring;
  hashedBlock: TSDUBytes;
  blockN:      Ansistring;
{$IFDEF FREEOTFE_DEBUG}
  blkNum: integer;
{$ENDIF}
begin
  Result := True;
  SDUInitAndZeroBuffer(blockLength, prev);

  //  prev := StringOfChar(#0, blockLength);
  // -1 because we don't process the last block
{$IFDEF FREEOTFE_DEBUG}
  blkNum := 0;
{$ENDIF}
  for i := 1 to (totalBlockCount - 1) do begin
{$IFDEF FREEOTFE_DEBUG}
inc(blkNum);
{$ENDIF}
    // Get block "i"
    GetBlock(stripeData, totalBlockCount, blockLength, i, blockN);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Before XOR ('+inttostr(blkNum)+'):');
DebugMsgBinary(SDUBytesToString( prev));
{$ENDIF}

    // XOR previous block output with current stripe
    tmpData := '';
    for j := 1 to length(prev) do begin
      tmpData := tmpData + Ansichar((prev[j - 1] xor Ord(blockN[j])));
    end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In between XOR and diffuse ('+inttostr(blkNum)+'):');
DebugMsgBinary(tmpData);
{$ENDIF}
    assert('1234' = SDUBytesToString(SDUStringToSDUBytes('1234')));
    // Hash output
    RepeatedHash(
      hashKernelModeDeviceName,
      hashGUID,
      tmpData,
      hashedBlock
      );

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('After process ('+inttostr(blkNum)+'):');
DebugMsgBinary(SDUBytesToString( hashedBlock));
{$ENDIF}

    // Setup for next iteration
    prev := hashedBlock;
  end;

  // Store last hashing result as output
  output := prev;

end;


 // ----------------------------------------------------------------------------
 // Get the "blockNumber" block out of "totalBlockCount"
function TOTFEFreeOTFEBase.GetBlock(stripeData: Ansistring; totalBlockCount: Integer;
  blockLength: Integer; blockNumber: Integer; var output: Ansistring): Boolean;
var
  allOK: Boolean;
  i:     Integer;
begin
  allOK := True;

  // xxx - sanity checking

  // Get block "i"
  output := '';
  for i := 0 to (blockLength - 1) do begin
    // -1 because block numbers start from 1
    // +1 because strings start from 1
    output := output + stripeData[(((blockNumber - 1) * blockLength) + i + 1)];
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.AFMerge(input: Ansistring; n: Integer;
  hashKernelModeDeviceName: String; hashGUID: TGUID; out output: TSDUBytes): Boolean;
var
  i:                Integer;
  processedStripes: TSDUBytes;
  blockLength:      Integer;
  lastBlock:        Ansistring;
begin
  Result := True;

  blockLength := (length(input) div n);

  Process(
    input,
    n,
    blockLength,
    hashKernelModeDeviceName,
    hashGUID,
    processedStripes
    );

  // Get block "i"
  GetBlock(input, n, blockLength, n, lastBlock);

  // XOR last hash output with last block to obtain original data
  SDUInitAndZeroBuffer(0, output);
  for i := 1 to blockLength do begin
    SDUAddByte(output, processedStripes[i - 1] xor Ord(lastBlock[i]));
  end;

end;

// ----------------------------------------------------------------------------

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------



 //////////////////////////////////////////////
 // end of OTFEFreeOTFE_mntLUKS_AFS_Impl.pas
 //////////////////////////////////////////////


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.StringHasNumbers(checkString: String): Boolean;
var
  i:      Integer;
  retval: Boolean;
begin
  retval := False;

  for i := 1 to length(checkString) do begin
    if ((checkString[i] >= '0') and (checkString[i] <= '9')) then begin
      retval := True;
      break;
    end;
  end;

  Result := retval;
end;


 // ----------------------------------------------------------------------------
 // Determine if the specified volume is a Linux LUKS volume
function TOTFEFreeOTFEBase.IsLUKSVolume(volumeFilename: String): Boolean;
var
  rawData: Ansistring;
  retVal:  Boolean;
begin
  retVal := False;

  // Read in what should be the volume's signature
  if ReadRawVolumeData(volumeFilename, 0,
    // LUKS volumes always start from the beginning
    length(LUKS_MAGIC), rawData) then begin
    // Compare with LUKS volume signature
    retVal := (rawData = LUKS_MAGIC);
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // LUKS Header (v1)
 // ================
 //
 // LUKS PHDR Layout
 // ----------------
 // Offset  Field name      Length  Data type  Description
 //     0   magic              6    bytes[]    magic for LUKS partition header
 //     6   version            2    uint16_t   LUKS version
 //     8   cipher-name       32    char[]     cipher name specification
 //    40   cipher-mode       32    char[]     cipher mode specification
 //    72   hash-spec         32    char[]     hash specification
 //   104   payload-offset     4    uint32_t   start offset of the bulk data (in sectors)
 //   108   key-bytes          4    uint32_t   number of key bytes
 //   112   mk-digest         20    byte[]     master key checksum from PBKDF2
 //   132   mk-digest-salt    32    byte[]     salt parameter for master key PBKDF2
 //   164   mk-digest-iter     4    uint32_t   iteraions parameter for master key PBKDF2
 //   168   uuid              40    char[]     UUID of the partition
 //   208   key-slot-1        48    key_slot   key slot 1
 //   256   key-slot-2        48    key_slot   key slot 2
 //   ...   ...               ..    ...        ...
 //   554   key-slot-8        48    key_slot   key slot 8
 //  =====  ===============
 //   592   total phdr size
 //  =====  ===============
 //
 // NOTE THAT THE DOCUMENTATION HAS AN ERROR - The length of hash-spec is 32
 // bytes long, though the offsets detailed in the specifications are correct.
 //
 //
 // LUKS Key Slot Layout
 // --------------------
 // Offset  Field name           Length  Data type  Description
 //     0   active                  4    uint32_t   state of keyslot, enabled/disabled
 //     4   iterations              4    uint32_t   iteration parameter for PBKDF2
 //     8   salt                   32    byte[]     salt paramter for PBKDF2
 //    40   key-material-offset     4    uint32_t   start sector of key material
 //    44   stripes                 4    uint32_t   number of anti-forensic stripes
 //  =====  ==================
 //    48   total keyslot size
 //  =====  ==================
function TOTFEFreeOTFEBase.ParseLUKSHeader(rawData: Ansistring;
  var LUKSheader: TLUKSHeader): Boolean;
var
  rawLUKSheader: TLUKSHeaderRaw;
  allOK:         Boolean;
  strGUID:       Ansistring;
  i:             Integer;
  faultFlag:     Boolean;
  keySlotActive: DWORD;
begin
  allOK := False;

  if (length(rawData) = sizeof(rawLUKSheader)) then begin
    StrMove(@rawLUKSheader, PAnsiChar(rawData), Length(rawData));

    // OK, this loop is crude - but it works
    LUKSheader.magic := '';
    for i := low(rawLUKSheader.magic) to high(rawLUKSheader.magic) do begin
      LUKSheader.magic := LUKSheader.magic + Ansichar(rawLUKSheader.magic[i]);
    end;

    if (LUKSheader.magic = LUKS_MAGIC) then begin
      LUKSheader.version := rawLUKSheader.version[1] + (rawLUKSheader.version[0] shl 8);
      // As per the LUKS specifications: If the LUKS version is of a
      // later version than we support - we make no attempt to interpret
      // it, and simply return an error
      if (LUKSheader.version <= LUKS_VER_SUPPORTED) then begin
        LUKSheader.cipher_name    :=
          Copy(rawLUKSheader.cipher_name, 1, StrLen(rawLUKSheader.cipher_name));
        LUKSheader.cipher_mode    :=
          Copy(rawLUKSheader.cipher_mode, 1, StrLen(rawLUKSheader.cipher_mode));
        LUKSheader.hash_spec      :=
          Copy(rawLUKSheader.hash_spec, 1, StrLen(rawLUKSheader.hash_spec));
        LUKSheader.payload_offset := SDUBigEndian32ToDWORD(rawLUKSheader.payload_offset);
        LUKSheader.key_bytes      := SDUBigEndian32ToDWORD(rawLUKSheader.key_bytes);
        CopyMemory(@LUKSheader.mk_digest, @rawLUKSheader.mk_digest,
          sizeof(rawLUKSheader.mk_digest)
          );
        CopyMemory(@LUKSheader.mk_digest_salt, @rawLUKSheader.mk_digest_salt,
          sizeof(rawLUKSheader.mk_digest_salt)
          );
        LUKSheader.mk_digest_iter := SDUBigEndian32ToDWORD(rawLUKSheader.mk_digest_iter);
        strGUID                   := Copy(rawLUKSheader.uuid, 1, StrLen(rawLUKSheader.uuid));
        //todo: is this index right?
        strGUID                   := '{' + strGUID + '}';
        LUKSheader.uuid           := StringToGUID(strGUID);

        faultFlag := False;
        for i := 0 to (LUKS_NUMKEYS - 1) do begin
          keySlotActive := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].active);

          // Sanity check
          if ((keySlotActive <> LUKS_KEY_ENABLED) and (keySlotActive <>
            LUKS_KEY_DISABLED)) then begin
            faultFlag := True;
            break;
          end;

          LUKSheader.keySlot[i].active := (keySlotActive = LUKS_KEY_ENABLED);
          if (LUKSheader.keySlot[i].active) then begin
            LUKSheader.keySlot[i].iterations :=
              SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].iterations);

            CopyMemory(@LUKSheader.keySlot[i].salt, @rawLUKSheader.keySlot[i].salt,
              sizeof(rawLUKSheader.keySlot[i].salt)
              );

            LUKSheader.keySlot[i].key_material_offset :=
              SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].key_material_offset);
            LUKSheader.keySlot[i].stripes             :=
              SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].stripes);
          end;

        end;

        allOK := not (faultFlag);
      end;

    end;

  end;  // if (length(rawData) = sizeof(rawLUKSheader)) then


  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadLUKSHeader(filename: String; var LUKSheader: TLUKSHeader): Boolean;
var
  allOK:   Boolean;
  rawData: Ansistring;
begin
  allOK := False;

  if ReadRawVolumeData(filename, 0,
    // LUKS volumes always start from the beginning
    sizeof(TLUKSHeaderRaw), rawData) then begin
    allOK := ParseLUKSHeader(rawData, LUKSheader);
  end;

  Result := allOK;
end;


 // ----------------------------------------------------------------------------
 // If the hash and cypher members of the LUKS header passed in are populated,
 // this function will populate struct with the the FreeOTFE driver details
 // (e.g. driver name/GUID) that correspond to that hash/cypher
 // Note: Must be called *after* ReadLUKSHeader(...)
function TOTFEFreeOTFEBase.MapToFreeOTFE(baseIVCypherOnHashLength: Boolean;
  var LUKSheader: TLUKSHeader): Boolean;
var
  allOK: Boolean;
begin
  allOK := True;

  // Identify the FreeOTFE hash driver to be used
  if (allOK) then begin
    allOK := IdentifyHash(LUKSHeader.hash_spec, LUKSHeader.hashKernelModeDeviceName,
      LUKSHeader.hashGUID);
  end;

  // Identify the FreeOTFE cypher driver to be used
  if (allOK) then begin
    allOK := IdentifyCypher(LUKSHeader.cipher_name, (LUKSHeader.key_bytes * 8),
      LUKSHeader.cipher_mode, baseIVCypherOnHashLength, LUKSHeader.cypherKernelModeDeviceName,
      LUKSHeader.cypherGUID, LUKSHeader.sectorIVGenMethod, LUKSHeader.IVHashKernelModeDeviceName,
      LUKSHeader.IVHashGUID, LUKSHeader.IVCypherKernelModeDeviceName,
      LUKSHeader.IVCypherGUID);

  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.DumpLUKSDataToFile(filename: String; userKey: Ansistring;
  keyfile: String; keyfileIsASCII: Boolean; keyfileNewlineType: TSDUNewline;
  baseIVCypherOnHashLength: Boolean; dumpFilename: String): Boolean;
var
  allOK:            Boolean;
  LUKSHeader:       TLUKSHeader;
  i:                Integer;
  strFormattedGUID: String;
  hashTitle:        String;
  cypherTitle:      String;
  hashDetails:      TFreeOTFEHash;
  cypherDetails:    TFreeOTFECypher_v3;
  IVCypherDetails:  TFreeOTFECypher_v3;
  keySlot:          Integer;
  keyMaterial:      TSDUBytes;
  prettyPrintData:  TStringList;
  dumpReport:       TStringList;
  IVHashTitle:      String;
  IVCypherTitle:    String;
  userKeyOK:        Boolean;
begin
  CheckActive();

  dumpReport := TStringList.Create();
  try
    dumpReport.Add('LUKS Dump');
    dumpReport.Add('=========');
    dumpReport.Add('');
    dumpReport.Add('Dump Created By');
    dumpReport.Add('---------------');
    dumpReport.Add('Platform              : PC');
    dumpReport.Add('Application version   : v' + SDUGetVersionInfoString(ParamStr(0)));
    dumpReport.Add('Driver ID             : ' + VersionStr());

    // Get the LUKS header from the volume...
    if not (ReadLUKSHeader(filename, LUKSHeader)) then begin
      if IsLUKSVolume(filename) then begin
        dumpReport.Add('ERROR: Unable to read LUKS header?!');
      end else begin
        dumpReport.Add('Unable to read LUKS header; this does not appear to be a LUKS volume.');
      end;

    end else begin
      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('cryptsetup Style Dump');
      dumpReport.Add('---------------------');
      dumpReport.Add('LUKS header information for ' + filename);
      dumpReport.Add('');
      dumpReport.Add('Version:        ' + IntToStr(LUKSheader.version));
      dumpReport.Add('Cipher name:    ' + LUKSheader.cipher_name);
      dumpReport.Add('Cipher mode:    ' + LUKSheader.cipher_mode);
      dumpReport.Add('Hash spec:      ' + LUKSheader.hash_spec);
      dumpReport.Add('Payload offset: ' + IntToStr(LUKSheader.payload_offset));
      dumpReport.Add('MK bits:        ' + IntToStr(LUKSheader.key_bytes * 8));
      // Convert from bytes to bits
      dumpReport.Add('MK digest:      ' + PrettyHex(
        @(LUKSheader.mk_digest), LUKS_DIGESTSIZE));
      dumpReport.Add('MK salt:        ' + PrettyHex(
        @(LUKSheader.mk_digest_salt[0]), (LUKS_SALTSIZE div 2)));
      dumpReport.Add('                ' + PrettyHex(
        @(LUKSheader.mk_digest_salt[(LUKS_SALTSIZE div 2)]), (LUKS_SALTSIZE div 2)));
      dumpReport.Add('MK iterations:  ' + IntToStr(LUKSheader.mk_digest_iter));
      strFormattedGUID := GUIDToString(LUKSheader.uuid);
      // Remove leading and trailing "{" and "}"
      Delete(strFormattedGUID, 1, 1);
      Delete(strFormattedGUID, length(strFormattedGUID), 1);
      strFormattedGUID := lowercase(strFormattedGUID);
      dumpReport.Add('UUID:           ' + strFormattedGUID);
      dumpReport.Add('');

      for i := low(LUKSheader.keySlot) to high(LUKSheader.keySlot) do begin
        if (LUKSheader.keySlot[i].active = False) then begin
          dumpReport.Add('Key Slot ' + IntToStr(i) + ': DISABLED');
        end else begin
          dumpReport.Add('Key Slot ' + IntToStr(i) + ': ENABLED');
          dumpReport.Add('        Iterations:             ' + IntToStr(
            LUKSheader.keySlot[i].iterations));
          dumpReport.Add('        Salt:                   ' + PrettyHex(
            @(LUKSheader.keySlot[i].salt[0]), (LUKS_SALTSIZE div 2)));
          dumpReport.Add('                                ' + PrettyHex(
            @(LUKSheader.keySlot[i].salt[(LUKS_SALTSIZE div 2)]),
            (LUKS_SALTSIZE div 2)));
          dumpReport.Add('        Key material offset:    ' + IntToStr(
            LUKSheader.keySlot[i].key_material_offset));
          dumpReport.Add('        AF stripes:             ' + IntToStr(
            LUKSheader.keySlot[i].stripes));
        end;
      end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Mapped FreeOTFE Drivers');
      dumpReport.Add('-----------------------');
      if not (MapToFreeOTFE(baseIVCypherOnHashLength, LUKSheader)) then begin
        dumpReport.Add('One or more of the following:');
        dumpReport.Add('');
        dumpReport.Add('  *) The hash algorithm');
        dumpReport.Add('  *) The cypher');
        dumpReport.Add('  *) The IV generation method');
        dumpReport.Add('');
        dumpReport.Add('specified in the LUKS header could not be mapped to a FreeOTFE equivalent.');
        dumpReport.Add('This may be because the required FreeOTFE driver hasn''t been installed and');
        dumpReport.Add('started, or because the required LUKS option is not supported in this');
        dumpReport.Add('version of FreeOTFE');
      end else begin
        hashTitle := 'ERROR: Unable to determine hash title?!';
        if GetSpecificHashDetails(LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, hashDetails) then begin
          hashTitle := GetHashDisplayTechTitle(hashDetails);
        end;

        cypherTitle := 'ERROR: Unable to determine cypher title?!';
        if GetSpecificCypherDetails(LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, cypherDetails) then begin
          cypherTitle := GetCypherDisplayTechTitle(cypherDetails);
        end;

        if (LUKSheader.sectorIVGenMethod <> foivgESSIV) then begin
          IVHashTitle   := 'IV hash n/a';
          IVCypherTitle := 'IV cypher n/a';
        end else begin
          IVHashTitle := 'ERROR: Unable to determine IV hash title?!';
          if GetSpecificHashDetails(LUKSheader.IVHashKernelModeDeviceName,
            LUKSheader.IVHashGUID, hashDetails) then begin
            IVHashTitle := GetHashDisplayTechTitle(hashDetails);
          end;

          IVCypherTitle := 'ERROR: Unable to determine IV cypher title?!';
          if GetSpecificCypherDetails(LUKSheader.IVCypherKernelModeDeviceName,
            LUKSheader.IVCypherGUID, IVCypherDetails) then begin
            IVCypherTitle := GetCypherDisplayTechTitle(IVCypherDetails);
          end;
        end;

        dumpReport.Add('Hash pretty title         : ' + hashTitle);
        dumpReport.Add('Hash driver KM name       : ' + LUKSheader.hashKernelModeDeviceName);
        dumpReport.Add('Hash GUID                 : ' + GUIDToString(LUKSheader.hashGUID));
        dumpReport.Add('Cypher pretty title       : ' + cypherTitle);
        dumpReport.Add('Cypher driver KM name     : ' + LUKSheader.cypherKernelModeDeviceName);
        dumpReport.Add('Cypher GUID               : ' + GUIDToString(LUKSheader.cypherGUID));
        dumpReport.Add('Sector IV generation      : ' +
          FreeOTFESectorIVGenMethodTitle[LUKSheader.sectorIVGenMethod]);
        if (LUKSheader.sectorIVGenMethod = foivgESSIV) then begin
          dumpReport.Add('  IV hash pretty title    : ' + IVHashTitle);
          dumpReport.Add('  IV hash driver KM name  : ' + LUKSheader.IVHashKernelModeDeviceName);
          dumpReport.Add('  IV hash GUID            : ' + GUIDToString(LUKSheader.IVHashGUID));

          if baseIVCypherOnHashLength then begin
            dumpReport.Add('  IV cypher               : <based on IV hash length>');
          end else begin
            dumpReport.Add('  IV cypher               : <same as main cypher>');
          end;

          dumpReport.Add('  IV cypher pretty title  : ' + IVCypherTitle);
          dumpReport.Add('  IV cypher driver KM name: ' + LUKSheader.IVCypherKernelModeDeviceName);
          dumpReport.Add('  IV cypher GUID          : ' + GUIDToString(LUKSheader.IVCypherGUID));
        end;
      end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Master Key');
      dumpReport.Add('----------');
      if (keyfile = '') then begin
        userKeyOK := True;
        dumpReport.Add('User supplied password   : ' + userKey);
      end else begin
        dumpReport.Add('Keyfile                  : ' + keyfile);

        if keyfileIsASCII then begin
          dumpReport.Add('Treat keyfile as         : ASCII');
          dumpReport.Add('Newlines are             : ' + SDUNEWLINE_TITLE[keyfileNewlineType]);
        end else begin
          dumpReport.Add('Treat keyfile as         : Binary');
        end;

        userKeyOK := ReadLUKSKeyFromFile(keyfile, keyfileIsASCII,
          keyfileNewlineType, userKey);

        if not (userKeyOK) then begin
          dumpReport.Add('Unable to read password from keyfile.');
        end else begin
          if keyfileIsASCII then begin
            dumpReport.Add('Password from keyfile    : ' + userKey);
          end;

          dumpReport.Add('Password as binary       : ');
          prettyPrintData := TStringList.Create();
          try
            prettyPrintData.Clear();
            SDUPrettyPrintHex(
              userKey,
              0,
              length(userKey),
              prettyPrintData
              );
            dumpReport.AddStrings(prettyPrintData);
          finally
            prettyPrintData.Free();
          end;

        end;

      end;

      if userKeyOK then begin
        if not (RecoverMasterKey(filename, userKey, baseIVCypherOnHashLength,
          LUKSHeader, keySlot, keyMaterial)) then begin
          dumpReport.Add('No master key could be recovered with the specified password.');
        end else begin
          prettyPrintData := TStringList.Create();
          try
            dumpReport.Add('Password unlocks key slot: ' + IntToStr(keySlot));
            dumpReport.Add('Recovered master key     :');
            prettyPrintData.Clear();
            SDUPrettyPrintHex(
              keyMaterial,
              0,
              length(keyMaterial),
              prettyPrintData
              );
            dumpReport.AddStrings(prettyPrintData);
          finally
            prettyPrintData.Free();
          end;

        end;
      end;

    end;

    // Save the report out to disk...
    dumpReport.SaveToFile(dumpFilename);
    allOK := True;

  finally
    dumpReport.Free();
  end;


  if (allOK) then begin
    LastErrorCode := OTFE_ERR_SUCCESS;
  end;

  Result := allOK;
end;


 // ----------------------------------------------------------------------------
 // This function is similar to SDUPrettyPrintHex from the SDUGeneral unit, but
 // formats slightly differently
 // This special function is required to generate the same format output as
 // cryptsetup produces, in order to make diffing the output easier.
function TOTFEFreeOTFEBase.PrettyHex(data: Pointer; bytesCount: Integer): Ansistring;
var
  i:      Integer;
  x:      Ansichar;
  retVal: Ansistring;
begin
  retVal := '';

  for i := 0 to (bytesCount - 1) do begin
    x      := (PAnsiChar(data))[i];
    // Yeah, I know - this isn't too nice. There's always going to be an extra
    // space at the end of the line. Don't blame me, this is how
    // cryptosetup-luks does things - I'm just replicating it here so that the
    // output can be diffed easily
    retVal := retVal + inttohex(Ord(x), 2) + ' ';
  end;

  Result := lowercase(retVal);
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.RecoverMasterKey(filename: String; userPassword: PasswordString;
  baseIVCypherOnHashLength: Boolean; var LUKSHeader: TLUKSHeader; var keySlot: Integer;
  var keyMaterial: TSDUBytes): Boolean;
var
  pwd_PBKDF2ed:          TSDUBytes;
  foundValid:            Boolean;
  i, j:                  Integer;
  keySlotSaltAsString:   TSDUBytes;
  masterkeySaltAsString: Ansistring;
  generatedCheck:        TSDUBytes;
  masterKey:             TSDUBytes;
  splitDataLength:       Integer;
  splitData:             Ansistring;
  tmpVolumeFlags:        Integer;
begin
  Result := True;

  keySlot := -1;


  // Get the LUKS header from the volume...
  if (Result) then
    Result := ReadLUKSHeader(filename, LUKSHeader);


  // Identify the FreeOTFE options to be used...
  if (Result) then begin
    Result := MapToFreeOTFE(baseIVCypherOnHashLength, LUKSHeader);
  end;



  if (Result) then begin
    masterkeySaltAsString := '';
    for j := low(LUKSHeader.mk_digest_salt) to high(LUKSHeader.mk_digest_salt) do begin
      masterkeySaltAsString := masterkeySaltAsString + Ansichar(LUKSHeader.mk_digest_salt[j]);
    end;
  end;


  if (Result) then begin
    // For each keyslot...
    for i := low(LUKSHeader.keySlot) to high(LUKSHeader.keySlot) do begin
      if (LUKSHeader.keySlot[i].Active) then begin
        SDUZeroBuffer(keySlotSaltAsString);
        for j := low(LUKSHeader.keySlot[i].salt) to high(LUKSHeader.keySlot[i].salt) do begin
          SDUAddByte(keySlotSaltAsString, LUKSHeader.keySlot[i].salt[j]);
        end;

        // PBKDF2 the user's password with the salt
        if not (DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, userPassword, keySlotSaltAsString,
          LUKSHeader.keySlot[i].iterations, (LUKSHeader.key_bytes * 8),  // In *bits*
          pwd_PBKDF2ed)) then begin
          Result := False;
          break;
        end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS derived key follows:');
  DebugMsgBinary(SDUBytesToString( pwd_PBKDF2ed));
{$ENDIF}

        splitDataLength := (LUKSheader.key_bytes * LUKSheader.keyslot[i].stripes);
        tmpVolumeFlags  := 0;


        if not (ReadWritePlaintextToVolume(True,
          // Read, NOT write

          filename, SDUBytesToString(pwd_PBKDF2ed), LUKSheader.sectorIVGenMethod,
          LUKSheader.IVHashKernelModeDeviceName, LUKSheader.IVHashGUID,
          LUKSheader.IVCypherKernelModeDeviceName, LUKSheader.IVCypherGUID,
          LUKSheader.cypherKernelModeDeviceName, LUKSheader.cypherGUID,
          tmpVolumeFlags, fomaFixedDisk, 0,
                            // Offset from within mounted volume from where to read/write data
          splitDataLength,  // Length of data to read/write. In bytes
          splitData,        // Data to read/write

          (LUKS_SECTOR_SIZE * LUKSheader.KeySlot[i].key_material_offset)
          // Offset within volume where encrypted data starts
          )) then begin
          Result := False;
          break;
        end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS decrypted split data follows:');
  DebugMsgBinary(splitData);
{$ENDIF}


        // Merge the split data
        if not (AFMerge(splitData, LUKSheader.keySlot[i].stripes,
          LUKSheader.hashKernelModeDeviceName, LUKSheader.hashGUID, masterKey)) then begin
          Result := False;
          break;
        end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS AF merged data follows:');
  DebugMsgBinary(SDUBytesToString( masterKey));
{$ENDIF}


        // Generate PBKDF2 of the merged data
        if not (DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, SDUBytesToString(masterKey), SDUStringToSDUBytes(
          masterkeySaltAsString), LUKSHeader.mk_digest_iter, (sizeof(LUKSHeader.mk_digest) * 8),
          generatedCheck)) then begin
          Result := False;
          break;
        end;

{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS generated pbkdf2 checksum of recovered key follows:');
  DebugMsgBinary(SDUBytesToString(generatedCheck));
{$ENDIF}

        // Check if the PBKDF2 of the merged data matches that of the MK digest
        foundValid := True;
        for j := 1 to length(generatedCheck) do begin
          if (generatedCheck[j - 1] <> LUKSHeader.mk_digest[j - 1]) then begin
            foundValid := False;
            break;
          end;

        end;

        if (foundValid) then begin
          // Valid keyslot found; break out of loop
          keySlot     := i;
          keyMaterial := masterKey;
          break;
        end;

      end;

    end;

  end;
  Result := Result and (keySlot <> -1);
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyHash(hashSpec: String; var hashKernelModeDeviceName: String;
  var hashGUID: TGUID): Boolean;
const
  // The Tiger hash is called "tgr" under Linux (why?!!)
  FREEOTFE_TIGER     = 'TIGER';
  LUKS_TIGER         = 'TGR';
  // The Whirlpool hash is called "wp" under Linux (why?!!)
  FREEOTFE_WHIRLPOOL = 'WHIRLPOOL';
  LUKS_WHIRLPOOL     = 'WP';
var
  allOK:                    Boolean;
  hashDrivers:              array of TFreeOTFEHashDriver;
  hi, hj:                   Integer;
  currDriver:               TFreeOTFEHashDriver;
  currImpl:                 TFreeOTFEHash;
  idx:                      Integer;
  validKernelDeviceName:    TStringList;
  validGUID:                TStringList;
  selectDlg:                TfrmSelectHashCypher;
  i:                        Integer;
  normalisedTitle:          String;
  stillStripping:           Boolean;
  normalisedTitleLUKSified: String;
begin
  allOK := True;

  // Standardise to uppercase; removes any potential problems with case
  // sensitivity
  hashSpec := uppercase(hashSpec);

  // Obtain details of all hashes...
  if (allOK) then begin
    SetLength(hashDrivers, 0);
    allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
  end;


  validKernelDeviceName := TStringList.Create();
  try
    validGUID := TStringList.Create();
    try
      // Locate the specific FreeOTFE hash to use...
      if (allOK) then begin
        // FOR ALL HASH DRIVERS...
        for hi := low(hashDrivers) to high(hashDrivers) do begin
          currDriver := hashDrivers[hi];
          // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
          for hj := low(currDriver.Hashes) to high(currDriver.Hashes) do begin
            currImpl := currDriver.Hashes[hj];


            // Strip out any "-" or whitespace in the hash's title; LUKS hashes
            // don't have these characters in their names
            normalisedTitle := currImpl.Title;
            stillStripping  := True;
            while (stillStripping) do begin
              stillStripping := False;

              idx := Pos(' ', normalisedTitle);
              if (idx <= 0) then begin
                idx := Pos('-', normalisedTitle);
              end;

              if (idx > 0) then begin
                Delete(normalisedTitle, idx, 1);
                stillStripping := True;
              end;

            end;

            normalisedTitle := uppercase(normalisedTitle);

            // The Tiger hash is called "tgr" under Linux (why?!!)
            // Because of this weirdness, we have to carry out an additional
            // check for LUKS volumes encrypted with "tgr" (Tiger) cypher
            // Note: The tiger hash isn't covered by the LUKS specification; it's
            //       support here (and in the Linux implementation) is an
            //       *optional* extension to the standard.
            // Note: Tiger isn't included in the v2.6.11.7 Linux kernel, but is
            //       included in the v2.6.19 kernel
            //
            // Examples of Tiger hash specs:
            //   tgr128
            //   tgr160
            //   tgr190
            normalisedTitleLUKSified := normalisedTitle;

            if (pos(FREEOTFE_TIGER, normalisedTitleLUKSified) > 0) then begin
              // If there's no numbers in our FreeOTFE's hash driver's title, add
              // on the hash length in bits; see the example above of this hash's
              // hashspec under Linux
              if not (StringHasNumbers(normalisedTitleLUKSified)) then begin
                normalisedTitleLUKSified := normalisedTitleLUKSified + IntToStr(currImpl.Length);
              end;

              normalisedTitleLUKSified :=
                StringReplace(normalisedTitleLUKSified, uppercase(FREEOTFE_TIGER),
                uppercase(LUKS_TIGER), []);
            end;

            // A similar thing happens with Whirlpool...
            // Examples of Whirlpool hash specs:
            //   wp256
            //   wp384
            //   wp512
            if (pos(FREEOTFE_WHIRLPOOL, normalisedTitleLUKSified) > 0) then begin
              // If there's no numbers in our FreeOTFE's hash driver's title, add
              // on the hash length in bits; see the example above of this hash's
              // hashspec under Linux
              if not (StringHasNumbers(normalisedTitleLUKSified)) then begin
                normalisedTitleLUKSified := normalisedTitleLUKSified + IntToStr(currImpl.Length);
              end;

              normalisedTitleLUKSified :=
                StringReplace(normalisedTitleLUKSified,
                uppercase(FREEOTFE_WHIRLPOOL), uppercase(LUKS_WHIRLPOOL), []);
            end;

            // Check current hash implementation details against volume's
            if ((normalisedTitle = hashSpec) or (normalisedTitleLUKSified = hashSpec))
            then begin
              // Valid cypher found; add to list
              validGUID.Add(GUIDToString(currImpl.HashGUID));
              validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then begin
        allOK := False;
      end else
      if (validGUID.Count = 1) then begin
        hashKernelModeDeviceName := validKernelDeviceName[0];
        hashGUID                 := StringToGUID(validGUID[0]);
      end else begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          selectDlg.FreeOTFEObj := self;
          for i := 0 to (validGUID.Count - 1) do begin
            selectDlg.AddCombination(
              validKernelDeviceName[i],
              StringToGUID(validGUID[i]),
              '',
              StringToGUID(NULL_GUID)
              );
          end;

          allOK := (selectDlg.ShowModal = mrOk);
          if (allOK) then begin
            hashKernelModeDeviceName := selectDlg.SelectedHashDriverKernelModeName();
            hashGUID                 := selectDlg.SelectedHashGUID();
          end;

        finally
          selectDlg.Free();
        end;

      end;

    finally
      validGUID.Free();
    end;

  finally
    validKernelDeviceName.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyCypher_SearchCyphers(
  cypherDrivers: array of TFreeOTFECypherDriver; cypherName: String;
  keySizeBits: Integer; useCypherMode: TFreeOTFECypherMode;
  var cypherKernelModeDeviceName: String; var cypherGUID: TGUID): Boolean;
var
  allOK:                 Boolean;
  validKernelDeviceName: TStringList;
  validGUID:             TStringList;
  selectDlg:             TfrmSelectHashCypher;
  ci, cj:                Integer;
  currDriver:            TFreeOTFECypherDriver;
  currImpl:              TFreeOTFECypher_v3;
  i:                     Integer;
begin
  allOK := True;

  validKernelDeviceName := TStringList.Create();
  try
    validGUID := TStringList.Create();
    try
      // Locate the specific FreeOTFE cypher to use...
      if (allOK) then begin
        // FOR ALL CYPHER DRIVERS...
        for ci := low(cypherDrivers) to high(cypherDrivers) do begin
          currDriver := cypherDrivers[ci];
          // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
          for cj := low(currDriver.Cyphers) to high(currDriver.Cyphers) do begin
            currImpl := currDriver.Cyphers[cj];

            // Check current cypher implementation details against volume's
            if ((uppercase(currImpl.Title) = cypherName) and
              (currImpl.KeySizeRequired = keySizeBits) and
              (currImpl.Mode = useCypherMode)) then begin
              // Valid cypher found; add to list
              validGUID.Add(GUIDToString(currImpl.CypherGUID));
              validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then begin
        allOK := False;
      end else
      if (validGUID.Count = 1) then begin
        cypherKernelModeDeviceName := validKernelDeviceName[0];
        cypherGUID                 := StringToGUID(validGUID[0]);
      end else begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          selectDlg.FreeOTFEObj := self;
          for i := 0 to (validGUID.Count - 1) do begin
            selectDlg.AddCombination(
              '',
              StringToGUID(NULL_GUID),
              validKernelDeviceName[i],
              StringToGUID(validGUID[i])
              );
          end;

          allOK := (selectDlg.ShowModal = mrOk);
          if (allOK) then begin
            cypherKernelModeDeviceName := selectDlg.SelectedCypherDriverKernelModeName();
            cypherGUID                 := selectDlg.SelectedCypherGUID();
          end;

        finally
          selectDlg.Free();
        end;

      end;

    finally
      validGUID.Free();
    end;

  finally
    validKernelDeviceName.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyCypher(cypherName: String; keySizeBits: Integer;
  cypherMode: String; baseIVCypherOnHashLength: Boolean; var cypherKernelModeDeviceName: String;
  var cypherGUID: TGUID; var sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  var IVHashKernelModeDeviceName: String; var IVHashGUID: TGUID;
  var IVCypherKernelModeDeviceName: String; var IVCypherGUID: TGUID): Boolean;
const
  PLAIN_IV = 'plain';
  ESSIV_IV = 'essiv';  // Not defined in the LUKS spec, but supported by LUKS anyway
  BENBI_IV = 'benbi';  // Not defined in the LUKS spec, but supported by LUKS anyway
var
  allOK:          Boolean;
  cypherDrivers:  array of TFreeOTFECypherDriver;
  idx:            Integer;
  currCypherMode: TFreeOTFECypherMode;
  useCypherMode:  TFreeOTFECypherMode;
  IVGenMethod:    String;
  IVHash:         String;
  tmpStr:         String;
  IVHashDetails:  TFreeOTFEHash;
  //  tmpSplitOK: boolean;
begin
  allOK := True;

  // Standardise to uppercase; removes any potential problems with case
  // sensitivity
  cypherMode := uppercase(cypherMode);
  cypherName := uppercase(cypherName);


  // Detect IV generation method
  // Examples: ecb
  //           cbc-plain
  //           lrw-plain
  //           cbc-essiv:sha256
  // The stuff before the "-" is the cypher mode; the stuff after is the IV
  // generation
  // Note: ESSIV is NOT specified in the LUKS specification, but LUKS implements
  //       this anyway as "-essiv:<hash>"
  if (allOK) then begin
    // Fallback to none... (e.g. ECB)
    sectorIVGenMethod          := foivgNone;
    IVHashKernelModeDeviceName := '';
    IVHashGUID                 := StringToGUID(NULL_GUID);

    if SDUSplitString(cypherMode, tmpStr, IVGenMethod, '-') then begin
      cypherMode := tmpStr;

      // Search for "-plain" and strip off cypher mode
      idx := pos(uppercase(PLAIN_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivg32BitSectorID;
      end;

      // Search for "-essiv" and strip off cypher mode
      idx := pos(uppercase(ESSIV_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivgESSIV;

        // Get the hash
        allOK := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        if (allOK) then begin
          allOK := IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
        end;

      end;

      // Search for "-benbi" and strip off cypher mode
      idx := pos(uppercase(BENBI_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivgNone;

        // Get the hash
        // Optional/not needed for "benbi"?
        //tmpSplitOK := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        //if (tmpSplitOK) then
        //  begin
        //  allOK := IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
        //  end;

      end;

    end;
  end;

  // Determine cypher mode
  useCypherMode := focmUnknown;
  if (allOK) then begin
    for currCypherMode := low(TFreeOTFECypherMode) to high(TFreeOTFECypherMode) do begin
      if (uppercase(FreeOTFECypherModeTitle(currCypherMode)) = cypherMode) then begin
        useCypherMode := currCypherMode;
        break;
      end;

    end;

    allOK := (useCypherMode <> focmUnknown);
  end;

  // Obtain details of all cyphers...
  if (allOK) then begin
    SetLength(cypherDrivers, 0);
    allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
  end;

  // Given the information we've determined, identify the FreeOTFE driver to
  // map the details to
  if (allOK) then begin
    allOK := IdentifyCypher_SearchCyphers(cypherDrivers, cypherName, keySizeBits,
      useCypherMode, cypherKernelModeDeviceName, cypherGUID);
  end;

  // Sort out the IV cypher driver
  if (allOK) then begin
    IVCypherKernelModeDeviceName := cypherKernelModeDeviceName;
    IVCypherGUID                 := cypherGUID;

    // Because of a bug in dm-crypt, the cypher used to generate ESSIV IVs may
    // ***NOT*** be the same cypher as used for the bulk encryption of data
    // This is because it passes the IV hash's length to the cypher when it
    // creates the ESSIV cypher - as a result, the ESSIV cypher may have a
    // different keylength to the bulk encryption cypher
    if (baseIVCypherOnHashLength and (sectorIVGenMethod = foivgESSIV)) then begin
      allOK := GetSpecificHashDetails(IVHashKernelModeDeviceName, IVHashGUID,
        IVHashDetails);

      if (allOK) then begin
        allOK := IdentifyCypher_SearchCyphers(cypherDrivers, cypherName,
          IVHashDetails.Length, useCypherMode, IVCypherKernelModeDeviceName, IVCypherGUID);
      end;
    end;

  end;


  Result := allOK;
end;

 // ----------------------------------------------------------------------------
 // Read a LUKS key in from a file
function TOTFEFreeOTFEBase.ReadLUKSKeyFromFile(filename: String; treatAsASCII: Boolean;
  ASCIINewline: TSDUNewline; out key: PasswordString): Boolean;
var
  newlinePos: Integer;
begin
  Result := SDUGetFileContent(filename, key);

  if Result then begin
    // If the input file was non-binary, only read in to the first newline
    if treatAsASCII then begin
      newlinePos := Pos(SDUNEWLINE_STRING[ASCIINewline], key);
      if (newlinePos > 0) then begin
        // -1 to actually *exclude* the newline
        key := Copy(key, 1, (newlinePos - 1));
      end;
    end;
  end;

end;


// ----------------------------------------------------------------------------

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
function FreeOTFEMountAsTitle(mountAs: TFreeOTFEMountAs): String;
begin
  Result := LoadResString(FreeOTFEMountAsTitlePtr[mountAs]);
end;

function FreeOTFECypherModeTitle(cypherMode: TFreeOTFECypherMode): String;
begin
  Result := LoadResString(FreeOTFECypherModeTitlePtr[cypherMode]);
end;

function FreeOTFEMACTitle(MACAlgorithm: TFreeOTFEMACAlgorithm): String;
begin
  Result := LoadResString(FreeOTFEMACTitlePtr[MACAlgorithm]);
end;

function FreeOTFEKDFTitle(KDFAlgorithm: TFreeOTFEKDFAlgorithm): String;
begin
  Result := LoadResString(FreeOTFEKDFTitlePtr[KDFAlgorithm]);
end;

// ----------------------------------------------------------------------------
constructor TOTFEFreeOTFEBase.Create(AOwner: TComponent);
{$IFDEF FREEOTFE_DEBUG}
{$IFNDEF _GEXPERTS}
var
  dlg: TSaveDialog;
{$ENDIF}
{$ENDIF}
begin
  inherited;

  fAllowNewlinesInPasswords := True;
  fAllowTabsInPasswords     := False;

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

  fdumpFlag := False;

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
procedure TOTFEFreeOTFEBase.SetActive(status: Boolean);
var
  allOK: Boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  allOK         := True;

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

  if (status <> Active) then begin
    if status then begin
      allOK := Connect();
      if not (allOK) then begin
        raise EFreeOTFEConnectFailure.Create('FreeOTFE driver not installed/not running');
      end;

    end else begin
      allOK := Disconnect();
      if not (allOK) then begin
        raise EFreeOTFEConnectFailure.Create('Could not disconnect from FreeOTFE driver?!');
      end;

    end;

  end;


  // Note: LastErrorCode will have been set by Connect(...)/Dicconnect(...) on
  //       failure.
  if allOK then begin
    inherited;
  end;


  if (Active) then begin
    // Previous versions which are no longer supported due to changes in the driver API
    if (Version() < FREEOTFE_ID_v00_58_0000) then begin
      Active := False;
      raise EFreeOTFEObsoleteDriver.Create(
        'An old version of the FreeOTFE driver was detected. Please upgrade before using this software'
        );
    end;

  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountFreeOTFE(volumeFilename: String;
  ReadOnly: Boolean = False; keyfile: String = ''; password: Ansistring = '';
  offset: ULONGLONG = 0; noCDBAtOffset: Boolean = False; silent: Boolean = False;
  saltLength: Integer = DEFAULT_SALT_LENGTH;
  keyIterations: Integer = DEFAULT_KEY_ITERATIONS): Ansichar;
var
  tmpStringList: TStringList;
  mountedAs:     Ansistring;
  retVal:        Ansichar;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := #0;

  tmpStringList := TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountFreeOTFE(tmpStringList, mountedAs, ReadOnly, keyfile, password,
      offset, noCDBAtOffset, silent, saltLength, keyIterations) then begin
      retVal := mountedAs[1];
    end;

  finally
    tmpStringList.Free();
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountFreeOTFE(volumeFilenames: TStringList;
  var mountedAs: Ansistring; ReadOnly: Boolean = False; keyfile: String = '';
  password: Ansistring = ''; offset: ULONGLONG = 0; noCDBAtOffset: Boolean = False;
  silent: Boolean = False; saltLength: Integer = DEFAULT_SALT_LENGTH;
  keyIterations: Integer = DEFAULT_KEY_ITERATIONS): Boolean;
var
  retVal:      Boolean;
  keyEntryDlg: TfrmKeyEntryFreeOTFE;
  mr:          Integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := False;
  mountedAs     := StringOfChar(AnsiChar(#0), volumeFilenames.Count);

  CheckActive();

  if (LastErrorCode = OTFE_ERR_SUCCESS) then begin
    keyEntryDlg := TfrmKeyEntryFreeOTFE.Create(nil);
    try
      keyEntryDlg.fFreeOTFEObj := self;
      keyEntryDlg.fSilent      := silent;
      keyEntryDlg.SetPassword(password);
      keyEntryDlg.SetReadonly(ReadOnly);
      keyEntryDlg.SetOffset(offset);
      keyEntryDlg.SetCDBAtOffset(not (noCDBAtOffset));
      keyEntryDlg.SetSaltLength(saltLength);
      keyEntryDlg.SetKeyIterations(keyIterations);
      keyEntryDlg.SetKeyfile(keyfile);
      // .FreeOTFEObj must be set before the advanced options can be displayed;
      // required in case PKCS#11 support is enabled (needed to populate
      // PKCS#11 comboboxes)
      keyEntryDlg.DisplayAdvanced(AdvancedMountDlg);
      keyEntryDlg.fVolumeFiles := volumeFilenames;

      mr := keyEntryDlg.ShowModal();
      if (mr = mrCancel) then begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
      end else begin
        mountedAs := keyEntryDlg.fMountedDrives;
        retVal    := True;
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
function TOTFEFreeOTFEBase.MountFreeOTFE(volumeFilenames: TStringList;
  UserKey: Ansistring; Keyfile: String; CDB: Ansistring;
  // CDB data (e.g. already read in PKCS#11 token)
  // If CDB is specified, "Keyfile" will be ignored
  SlotID: Integer;
                                  // Set to PKCS#11 slot ID if PKCS#11 slot was *actually* used for mounting
  PKCS11Session: TPKCS11Session;  // Set to nil if not needed
  PKCS11SecretKeyRecord: PPKCS11SecretKey;  // Set to nil if not needed
  KeyIterations: Integer; UserDriveLetter: Ansichar; MountReadonly: Boolean;
  MountMountAs: TFreeOTFEMountAs; Offset: Int64; OffsetPointsToCDB: Boolean;
  SaltLength: Integer;  // In *bits*
  MountForAllUsers: Boolean; var mountedAs: Ansistring): Boolean;
var
  retVal:            Boolean;
  i:                 Integer;
  currMountFilename: String;
  useFileOffset:     Int64;
  mountDriveLetter:  ansichar;

  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData:   TCDBMetaData;

  useCDB:       Ansistring;
  decryptedCDB: Ansistring;
  commonCDB:    Ansistring;
  errMsg:       String;

  prevCursor: TCursor;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := True;

  CheckActive();

  commonCDB := '';
  if (CDB <> '') then begin
    commonCDB := CDB;
  end else
  if (Keyfile <> '') then begin
    // Attempt to obtain the current file's critical data
    retVal := ReadRawVolumeCriticalData(Keyfile, 0, commonCDB);
    if not (retVal) then begin
      LastErrorCode := OTFE_ERR_KEYFILE_NOT_FOUND;
    end;
  end;

  if retVal then begin
    prevCursor    := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      // For each volume
      for i := 0 to (volumeFilenames.Count - 1) do begin
        useCDB            := commonCDB;
        currMountFilename := volumeFilenames[i];

        if (useCDB = '') then begin
          // Attempt to obtain the current file's critical data
          if not (ReadRawVolumeCriticalData(volumeFilenames[i], Offset, useCDB)) then
          begin
            // Bad file...
            LastErrorCode := OTFE_ERR_VOLUME_FILE_NOT_FOUND;
            mountedAs     := mountedAs + #0;
            retVal        := False;
            continue;
          end;
        end;

        // If CDB encrypted by PKCS#11 secret key, decrypt it now...
        decryptedCDB := useCDB;
        if ((PKCS11Session <> nil) and (PKCS11SecretKeyRecord <> nil)) then begin
          if not (PKCS11DecryptCDBWithSecretKey(PKCS11Session,
            PKCS11SecretKeyRecord, useCDB, decryptedCDB, errMsg)) then begin
            LastErrorCode := OTFE_ERR_PKCS11_SECRET_KEY_DECRYPT_FAILURE;
            mountedAs     := mountedAs + #0;
            retVal        := False;
            // Bail out; can't continue as problem with token
            break;
          end;
        end;

        // Process CDB passed in into CDBMetaData
        if not (ReadVolumeCriticalData_CDB(decryptedCDB, UserKey, SaltLength,
          // In *bits*
          KeyIterations, volumeDetails, CDBMetaData)) then begin
          // Wrong password, bad volume file, correct hash/cypher driver not installed
          LastErrorCode := OTFE_ERR_WRONG_PASSWORD;
          mountedAs     := mountedAs + #0;
          retVal        := False;
          continue;
        end;

        // Mount the volume
        if (volumeDetails.RequestedDriveLetter = #0) then begin
          // Nudge on to prevent getting drive A: or B:
          volumeDetails.RequestedDriveLetter := 'C';
        end;
        mountDriveLetter := GetNextDriveLetter(UserDriveLetter,
          volumeDetails.RequestedDriveLetter);
        if (mountDriveLetter = #0) then begin
          // Skip onto next volume file...
          LastErrorCode := OTFE_ERR_NO_FREE_DRIVE_LETTERS;
          mountedAs     := mountedAs + #0;
          retVal        := False;
          continue;
        end;

        // Locate where the actual encrypted partition starts within the
        // volume file
        useFileOffset := Offset;
        if OffsetPointsToCDB then begin
          useFileOffset := useFileOffset + Int64((CRITICAL_DATA_LENGTH div 8));
        end;

        if CreateMountDiskDevice(currMountFilename, volumeDetails.MasterKey,
          volumeDetails.SectorIVGenMethod, volumeDetails.VolumeIV, MountReadonly,
          CDBMetaData.HashDriver, CDBMetaData.HashGUID, CDBMetaData.CypherDriver,  // IV cypher
          CDBMetaData.CypherGUID,    // IV cypher
          CDBMetaData.CypherDriver,  // Main cypher
          CDBMetaData.CypherGUID,    // Main cypher
          volumeDetails.VolumeFlags, mountDriveLetter, useFileOffset,
          volumeDetails.PartitionLen, False, SlotID, MountMountAs, MountForAllUsers) then
        begin
          mountedAs := mountedAs + mountDriveLetter;
        end else begin
          mountedAs     := mountedAs + #0;
          // LastErrorCode set by MountDiskDevice (called by CreateMountDiskDevice)????
          LastErrorCode := OTFE_ERR_MOUNT_FAILURE;
          retVal        := False;
        end;

      end;  // for i:=0 to (volumeFilenames.count) do

    finally
      Screen.Cursor := prevCursor;
    end;

  end;


  // Pad out unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  for i := (length(mountedAs) + 1) to (volumeFilenames.Count) do begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := False;
  end;


  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Generate the metadata signature to be used
 // Returns: Appropriatly sized signature
function TOTFEFreeOTFEBase.MetadataSig(): Ansistring;
var
  tmpSig:    Ansistring;
  retval:    Ansistring;
  tmpStruct: TOTFEFreeOTFEVolumeMetaData;
begin
  // Dummy assignment to get rid of compiler warning; this variable is only
  // used to get the sizeof(...) the signature
  tmpStruct.Version := 1;

  tmpSig := Title();
  tmpSig := tmpSig + StringOfChar(AnsiChar(' '), length(tmpStruct.Signature));

  // Copy(...) indexes from 1
  retval := Copy(tmpSig, 1, length(tmpStruct.Signature));

  Result := retval;
end;

 // ----------------------------------------------------------------------------
 // Populate metadata struct with information
function TOTFEFreeOTFEBase.PopulateVolumeMetadataStruct(LinuxVolume: Boolean;
  PKCS11SlotID: Integer; var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;
var
  i:      Integer;
  tmpSig: Ansistring;
begin
  // Standard...
  tmpSig := MetadataSig();
  for i := low(metadata.Signature) to high(metadata.Signature) do begin
    // +1 because the string indexes from 1, while low(metadata.Signature) is
    // zero
    metadata.Signature[i] := tmpSig[i + 1];
  end;
  metadata.Version := METADATA_VERSION;

  metadata.PKCS11SlotID := PKCS11SlotID;
  metadata.LinuxVolume  := LinuxVolume;

  Result := True;
end;


 // ----------------------------------------------------------------------------
 // Convert string to metadata struct
function TOTFEFreeOTFEBase.ParseVolumeMetadata(metadataAsString: String;
  var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;
var
  retval:       Boolean;
  tmpStructPtr: POTFEFreeOTFEVolumeMetaData;
begin
  retval := (Pos(MetadataSig(), metadataAsString) = 0);

  if retval then begin
    retval := (length(metadataAsString) = sizeof(metadata));
    if retval then begin
      tmpStructPtr := POTFEFreeOTFEVolumeMetaData(PChar(metadataAsString));
      metadata     := tmpStructPtr^;
    end;

  end;

  Result := retval;
end;

 // ----------------------------------------------------------------------------
 // Convert metadata struct to string
procedure TOTFEFreeOTFEBase.VolumeMetadataToString(metadata: TOTFEFreeOTFEVolumeMetaData;
  var metadataAsString: Ansistring);
begin
  metadataAsString := StringOfChar(AnsiChar(#0), sizeof(metadata));
  StrMove(PAnsiChar(metadataAsString), @metadata, sizeof(metadata));
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLinux(volumeFilename: String; ReadOnly: Boolean = False;
  lesFile: String = ''; password: Ansistring = ''; keyfile: String = '';
  keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE; offset: ULONGLONG = 0;
  silent: Boolean = False; forceHidden: Boolean = False): Ansichar;
var
  tmpStringList: TStringList;
  mountedAs:     Ansistring;
  retVal:        Ansichar;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := #0;

  tmpStringList := TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountLinux(tmpStringList, mountedAs, ReadOnly, lesFile, password,
      keyfile, keyfileIsASCII, keyfileNewlineType, offset, silent, forceHidden) then begin
      retVal := mountedAs[1];
    end;

  finally
    tmpStringList.Free();
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLinux(volumeFilenames: TStringList;
  var mountedAs: Ansistring; ReadOnly: Boolean = False; lesFile: String = '';
  password: Ansistring = ''; keyfile: String = '';
  keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE; offset: ULONGLONG = 0;
  silent: Boolean = False; forceHidden: Boolean = False): Boolean;
var
  retVal:      Boolean;
  keyEntryDlg: TfrmKeyEntryLinux;
  i:           Integer;
  volumeKey:   TSDUBytes;

  userKey:             Ansistring;
  keyProcSeed:         Ansistring;
  keyProcHashDriver:   String;
  keyProcHashGUID:     TGUID;
  keyProcHashWithAs:   Boolean;
  keyProcCypherDriver: String;
  keyProcCypherGUID:   TGUID;
  keyProcIterations:   Integer;
  fileOptOffset:       Int64;
  fileOptSize:         Int64;
  mountDriveLetter:    ansichar;
  mountReadonly:       Boolean;
  mainCypherDriver:    Ansistring;
  mainCypherGUID:      TGUID;
  mainIVHashDriver:    Ansistring;
  mainIVHashGUID:      TGUID;
  mainIVCypherDriver:  Ansistring;
  mainIVCypherGUID:    TGUID;
  currDriveLetter:     ansichar;
  sectorIVGenMethod:   TFreeOTFESectorIVGenMethod;
  startOfVolFile:      Boolean;
  startOfEndData:      Boolean;
  mountMountAs:        TFreeOTFEMountAs;
  VolumeFlags:         DWORD;
  mr:                  Integer;
  mainCypherDetails:   TFreeOTFECypher_v3;
  mountForAllUsers:    Boolean;
  // emptyIV :TSDUBytes;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := True;

  CheckActive();

  // Sanity checking
  if (volumeFilenames.Count = 0) then begin
    mountedAs := '';
    Result    := True;
    exit;
  end else begin
    // Is the user attempting to mount Linux LUKS volumes?
    // Mount as LUKS if they are...
    {  TDK change - test user option to force non-luks (for hidden vols) }
    if (IsLUKSVolume(volumeFilenames[0])) and not forceHidden then begin
      Result := MountLUKS(volumeFilenames, mountedAs, ReadOnly, password, keyfile, silent);
      exit;
    end;
  end;


  keyEntryDlg := TfrmKeyEntryLinux.Create(nil);
  try
    keyEntryDlg.fFreeOTFEObj := self;
    keyEntryDlg.Initialize();
    if (lesFile <> '') then begin
      keyEntryDlg.LoadSettings(lesFile);
    end;
    keyEntryDlg.SetReadonly(ReadOnly);
    keyEntryDlg.SetKey(password);
    keyEntryDlg.SetOffset(offset);

    if silent then begin
      mr := mrOk;
    end else begin
      mr := keyEntryDlg.ShowModal();
    end;
    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
    end else
    if (mr = mrOk) then begin
      // Retrieve all data from the mount dialog...
      if (
        // Key...
        (keyEntryDlg.GetKey(userKey)) and

        // Key processing...
        (keyEntryDlg.GetKeyProcSeed(keyProcSeed)) and
        (keyEntryDlg.GetKeyProcHashKernelDeviceName(keyProcHashDriver)) and
        (keyEntryDlg.GetKeyProcHashGUID(keyProcHashGUID)) and
        (keyEntryDlg.GetKeyProcHashWithAs(keyProcHashWithAs)) and
        (keyEntryDlg.GetKeyProcCypherKernelDeviceName(keyProcCypherDriver)) and
        (keyEntryDlg.GetKeyProcCypherGUID(keyProcCypherGUID)) and
        (keyEntryDlg.GetKeyProcCypherIterationCount(keyProcIterations)) and

        // File options...
        (keyEntryDlg.GetOffset(fileOptOffset)) and
        (keyEntryDlg.GetSizeLimit(fileOptSize)) and

        // Mount options...
        (keyEntryDlg.GetDriveLetter(mountDriveLetter)) and
        (keyEntryDlg.GetReadonly(mountReadonly)) and
        (keyEntryDlg.GetMountAs(mountMountAs)) and

        // Encryption options...
        (keyEntryDlg.GetMainCypherKernelDeviceName(mainCypherDriver)) and
        (keyEntryDlg.GetMainCypherGUID(mainCypherGUID)) and
        (keyEntryDlg.GetMainSectorIVGenMethod(sectorIVGenMethod)) and
        (keyEntryDlg.GetMainIVSectorZeroPos(startOfVolFile, startOfEndData)) and
        (keyEntryDlg.GetMainIVHashKernelDeviceName(mainIVHashDriver)) and
        (keyEntryDlg.GetMainIVHashGUID(mainIVHashGUID)) and
        (keyEntryDlg.GetMainIVCypherKernelDeviceName(mainIVCypherDriver)) and
        (keyEntryDlg.GetMainIVCypherGUID(mainIVCypherGUID))) then begin
        mountForAllUsers := keyEntryDlg.GetMountForAllUsers();

        // Before we can generate the volume key for encryption/decryption, we
        // need to know the keysize of the cypher
        if not (GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then
        begin
          retVal := False;
        end;


        // Generate volume key for encryption/decryption
        if retVal then begin
          if not (GenerateLinuxVolumeKey(keyProcHashDriver, keyProcHashGUID,
            userKey, keyProcSeed, keyProcHashWithAs, mainCypherDetails.KeySizeRequired,
            volumeKey)) then begin
            LastErrorCode := OTFE_ERR_HASH_FAILURE;
            retVal        := False;
          end;
        end;


        // Encode IV generation method to flags...
        VolumeFlags := 0;
        if (startOfVolFile) then begin
          VolumeFlags := VolumeFlags or VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART;
        end;


        if retVal then begin
          for i := 0 to (volumeFilenames.Count - 1) do begin
            currDriveLetter := GetNextDriveLetter(mountDriveLetter, AnsiChar(#0));
            if (currDriveLetter = #0) then begin
              // No more drive letters following the user's specified drive
              // letter - don't mount further drives
              retVal := False;
              // Bail out...
              break;
            end;
            // SDUInitAndZeroBuffer(0, emptyIV);
            if CreateMountDiskDevice(volumeFilenames[i], SDUBytesToString(
              volumeKey), sectorIVGenMethod, '',  // Linux volumes don't have per-volume IVs
              mountReadonly, mainIVHashDriver, mainIVHashGUID,
              mainIVCypherDriver, mainIVCypherGUID, mainCypherDriver,
              mainCypherGUID, VolumeFlags, currDriveLetter, fileOptOffset, fileOptSize, True,
                                  // Linux volume
              PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
              mountMountAs, mountForAllUsers) then begin
              mountedAs := mountedAs + currDriveLetter;
            end else begin
              mountedAs := mountedAs + #0;
              retVal    := False;
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
  for i := (length(mountedAs) + 1) to (volumeFilenames.Count) do begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := False;
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
function TOTFEFreeOTFEBase.GenerateLinuxVolumeKey(hashDriver: Ansistring;
  hashGUID: TGUID; userKey: Ansistring; seed: Ansistring; hashWithAs: Boolean;
  targetKeylengthBits: Integer; var volumeKey: TSDUBytes): Boolean;
var
  seededKey:    Ansistring;
  hashOutput:   TSDUBytes;
  allOK:        Boolean;
  len:          Integer;
  volumeKeyStr: Ansistring;
begin
  allOK := True;

  // Salt the user's password
  seededKey := userKey + seed;


  // If the target key length is any size key, or we don't hash with A's, then
  // we simply hash the user's key once with the specified hash
  if ((targetKeylengthBits = -1) or not (hashWithAs)) then begin
    Result := HashData(hashDriver, hashGUID, SDUStringToSDUBytes(seededKey), volumeKey);
  end else begin
    // We're been asked to supply a volume key with a fixed, defined length
    while (Result and (Length(volumeKey) < (targetKeylengthBits div 8))) do begin
      // Note: We'll truncate later - don't worry if volumeKey is set to too
      //       many bits at this stage...
      Result := Result and HashData(hashDriver, hashGUID, SDUStringToSDUBytes(seededKey),
        hashOutput);
      //      volumeKey := volumeKey +  hashOutput;
      len    := length(volumeKey) + length(hashOutput);
      SDUAddArrays(volumeKey, hashOutput);
      assert(length(volumeKey) = len);
      // Prepare for next...         SDUBytesToString(
      seededKey := 'A' + seededKey;
    end;

  end;


  // If the target key length is > -1, truncate or we right-pad with 0x00
  // as appropriate
  if (Result) then begin
    if (targetKeylengthBits > -1) then begin
      // Copy as much of the hash value as possible to match the key length

      // Right-pad if needed
      volumeKeyStr := SDUBytesToString(volumeKey);
      volumeKeyStr := Copy(volumeKeyStr, 1, min((targetKeylengthBits div 8),
        length(volumeKeyStr)));
      volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0),
        ((targetKeylengthBits div 8) - Length(volumeKeyStr)));

      SDUResetLength(volumeKey, targetKeylengthBits div 8);
      assert(volumeKeyStr = SDUBytesToString(volumeKey));
    end;

  end;


  if not (allOK) then begin
    LastErrorCode := OTFE_ERR_HASH_FAILURE;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetNextDriveLetter(): Ansichar;
begin
  Result := GetNextDriveLetter(#0, #0);
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLUKS(volumeFilename: String; ReadOnly: Boolean = False;
  password: Ansistring = ''; keyfile: String = '';
  keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: Boolean = False): ansichar;
var
  tmpStringList: TStringList;
  mountedAs:     Ansistring;
  retVal:        ansichar;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := #0;

  tmpStringList := TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if MountLUKS(tmpStringList, mountedAs, ReadOnly, password, keyfile,
      keyfileIsASCII, keyfileNewlineType, silent) then begin
      retVal := mountedAs[1];
    end;

  finally
    tmpStringList.Free();
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.MountLUKS(volumeFilenames: TStringList;
  var mountedAs: Ansistring; ReadOnly: Boolean = False; password: PasswordString = '';
  keyfile: String = ''; keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: Boolean = False): Boolean;
var
  retVal:      Boolean;
  keyEntryDlg: TfrmKeyEntryLUKS;
  i:           Integer;
  volumeKey:   TSDUBytes;

  userKey:                  Ansistring;
  fileOptSize:              Int64;
  mountDriveLetter:         ansichar;
  mountReadonly:            Boolean;
  currDriveLetter:          Ansichar;
  mountMountAs:             TFreeOTFEMountAs;
  mr:                       Integer;
  LUKSHeader:               TLUKSHeader;
  keySlot:                  Integer;
  baseIVCypherOnHashLength: Boolean;
  mountForAllUsers:         Boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  retVal        := True;

  CheckActive();

  keyEntryDlg := TfrmKeyEntryLUKS.Create(nil);
  try
    keyEntryDlg.fFreeOTFEObj := self;
    keyEntryDlg.Initialize();
    keyEntryDlg.SetReadonly(ReadOnly);
    keyEntryDlg.SetKey(password);
    keyEntryDlg.SetKeyfile(keyfile);
    keyEntryDlg.SetKeyfileIsASCII(keyfileIsASCII);
    keyEntryDlg.SetKeyfileNewlineType(keyfileNewlineType);

    if silent then begin
      mr := mrOk;
    end else begin
      mr := keyEntryDlg.ShowModal();
    end;
    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
    end else
    if (mr = mrOk) then begin
      // Retrieve all data from the mount dialog...
      if (
        // Key...
        (keyEntryDlg.GetKey(userKey)) and
        (keyEntryDlg.GetIVCypherBase(baseIVCypherOnHashLength)) and

        // File options...
        (keyEntryDlg.GetSizeLimit(fileOptSize)) and

        // Mount options...
        (keyEntryDlg.GetDriveLetter(mountDriveLetter)) and
        (keyEntryDlg.GetReadonly(mountReadonly)) and (keyEntryDlg.GetMountAs(mountMountAs)))
      then begin
        mountForAllUsers := keyEntryDlg.GetMountForAllUsers();

        for i := 0 to (volumeFilenames.Count - 1) do begin
          currDriveLetter := GetNextDriveLetter(mountDriveLetter, #0);
          if (currDriveLetter = #0) then begin
            // No more drive letters following the user's specified drive
            // letter - don't mount further drives
            retVal := False;
            // Bail out...
            break;
          end;

          if not (RecoverMasterKey(volumeFilenames[i], userKey,
            baseIVCypherOnHashLength, LUKSHeader, keySlot, volumeKey)) then begin
            retVal := False;
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

          if CreateMountDiskDevice(volumeFilenames[i], SDUBytesToString(volumeKey),
            LUKSHeader.sectorIVGenMethod, '',
            // Linux volumes don't have per-volume IVs
            mountReadonly, LUKSHeader.IVHashKernelModeDeviceName,
            LUKSHeader.IVHashGUID, LUKSHeader.IVCypherKernelModeDeviceName,
            LUKSHeader.IVCypherGUID, LUKSHeader.cypherKernelModeDeviceName,
            LUKSHeader.cypherGUID, 0,
            // Volume flags
            currDriveLetter, (LUKSHeader.payload_offset * LUKS_SECTOR_SIZE),
            fileOptSize, True,  // Linux volume
            PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
            mountMountAs, mountForAllUsers) then begin
            mountedAs := mountedAs + currDriveLetter;
          end else begin
            mountedAs := mountedAs + #0;
            retVal    := False;
          end;

        end;  // for i:=0 to (volumeFilenames.count) do

      end;

    end;


  finally
    keyEntryDlg.Free()
  end;


  // Unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  for i := (length(mountedAs) + 1) to (volumeFilenames.Count) do begin
    mountedAs := mountedAs + #0;

    // This should not be needed; included to ensure sanity...
    retVal := False;
  end;


  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Mount(volumeFilename: Ansistring; ReadOnly: Boolean = False): Ansichar;
var
  tmpStringList: TStringList;
  mountedAs:     Ansistring;
  retVal:        Ansichar;
begin
  retVal := #0;

  tmpStringList := TStringList.Create();
  try
    tmpStringList.Add(volumeFilename);
    if Mount(tmpStringList, mountedAs, ReadOnly) then begin
      retVal := mountedAs[1];
    end;

  finally
    tmpStringList.Free();
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Mount(volumeFilenames: TStringList; var mountedAs: Ansistring;
  ReadOnly: Boolean = False): Boolean;
var
  frmSelectVolumeType: TfrmSelectVolumeType;
  retVal:              Boolean;
  mr:                  Integer;
begin
  retVal := False;

  CheckActive();

  frmSelectVolumeType := TfrmSelectVolumeType.Create(nil);
  try
    mr := frmSelectVolumeType.ShowModal();
    if (mr = mrCancel) then begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
    end else
    if (mr = mrOk) then begin
      if frmSelectVolumeType.FreeOTFEVolume then begin
        retVal := MountFreeOTFE(volumeFilenames, mountedAs, ReadOnly);
      end else begin
        retVal := MountLinux(volumeFilenames, mountedAs, ReadOnly);
      end;

    end;

  finally
    frmSelectVolumeType.Free();
    ;
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Dismount(volumeFilename: String; emergency: Boolean = False): Boolean;
begin
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.Title(): String;
begin
  Result := 'FreeOTFE';
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.VersionStr(): String;
var
  verNo:  Cardinal;
  retVal: String;
begin
  retval := '';

  CheckActive();

  verNo  := Version();
  retVal := VersionIDToStr(verNo);

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Convert a version ID into a prettyprinted version string
function TOTFEFreeOTFEBase.VersionIDToStr(versionID: DWORD): String;
var
  majorVer: Integer;
  minorVer: Integer;
  buildVer: Integer;
  retval:   String;
begin
  retval := '';

  if (versionID <> VERSION_ID_FAILURE) then begin
    majorVer := (versionID and $FF000000) div $00FF0000;
    minorVer := (versionID and $00FF0000) div $0000FF00;
    buildVer := (versionID and $0000FFFF);
    retval   := Format('v%d.%.2d.%.4d', [majorVer, minorVer, buildVer]);
  end;

  Result := retval;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IsEncryptedVolFile(volumeFilename: String): Boolean;
begin
  CheckActive();

  // We can't tell! Return TRUE...
  Result := True;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetVolFileForDrive(driveLetter: Ansichar): String;
var
  retVal:     String;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  retVal := '';

  CheckActive();

  if (GetVolumeInfo(upcase(driveLetter), volumeInfo)) then begin
    retVal := volumeInfo.Filename;
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetDriveForVolFile(volumeFilename: String): Ansichar;
var
  mounted:    Ansistring;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  retVal:     Ansichar;
  i:          Integer;
begin
  retVal := #0;

  CheckActive();

  mounted := DrivesMounted();
  for i := 1 to length(mounted) do begin
    if GetVolumeInfo(mounted[i], volumeInfo) then begin
      if (uppercase(volumeInfo.Filename) = uppercase(volumeFilename)) then begin
        retVal := volumeInfo.DriveLetter;
        break;
      end;

    end;
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetMainExe(): String;
begin
  // This is not meaningful for FreeOTFE
  FLastErrCode := OTFE_ERR_UNABLE_TO_LOCATE_FILE;
  Result       := '';

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetCypherDriverCyphers(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  retval: Boolean;
begin
  retval := _GetCypherDriverCyphers_v3(cypherDriver, cypherDriverDetails);
  if not (retval) then begin
    retval := _GetCypherDriverCyphers_v1(cypherDriver, cypherDriverDetails);
  end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.ShowHashDetailsDlg(driverName: String; hashGUID: TGUID);
var
  detailsDlg: TfrmHashInfo;
begin
  detailsDlg := TfrmHashInfo.Create(nil);
  try
    detailsDlg.OTFEFreeOTFEObj := self;
    detailsDlg.ShowDriverName  := driverName;
    detailsDlg.ShowGUID        := hashGUID;

    detailsDlg.ShowModal();
  finally
    detailsDlg.Free();
  end;

end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.ShowCypherDetailsDlg(driverName: String; cypherGUID: TGUID);
var
  detailsDlg: TfrmCypherInfo;
begin
  detailsDlg := TfrmCypherInfo.Create(nil);
  try
    detailsDlg.OTFEFreeOTFEObj := self;
    detailsDlg.ShowDriverName  := driverName;
    detailsDlg.ShowGUID        := cypherGUID;

    detailsDlg.ShowModal();
  finally
    detailsDlg.Free();
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificHashDetails(hashKernelModeDeviceName: Ansistring;
  hashGUID: TGUID; var hashDetails: TFreeOTFEHash): Boolean;
var
  i:                 Integer;
  retval:            Boolean;
  hashDriverDetails: TFreeOTFEHashDriver;
begin
  retval := False;

  if GetHashDriverHashes(hashKernelModeDeviceName, hashDriverDetails) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('low hashDriverDetails.Hashes: '+inttostr(low(hashDriverDetails.Hashes)));
DebugMsg('high hashDriverDetails.Hashes: '+inttostr(high(hashDriverDetails.Hashes)));
{$ENDIF}
    for i := low(hashDriverDetails.Hashes) to high(hashDriverDetails.Hashes) do begin
      if (IsEqualGUID(hashDriverDetails.Hashes[i].HashGUID, hashGUID)) then begin
        hashDetails := hashDriverDetails.Hashes[i];
        retval      := True;
        break;
      end;

    end;

  end;

  Result := retval;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificCypherDetails(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; var cypherDetails: TFreeOTFECypher_v3): Boolean;
var
  i:                   Integer;
  retval:              Boolean;
  cypherDriverDetails: TFreeOTFECypherDriver;
begin
  retval := False;

  if GetCypherDriverCyphers(cypherKernelModeDeviceName, cypherDriverDetails) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('low cypherDriverDetails.Cyphers: '+inttostr(low(cypherDriverDetails.Cyphers)));
DebugMsg('high cypherDriverDetails.Cyphers: '+inttostr(high(cypherDriverDetails.Cyphers)));
{$ENDIF}
    for i := low(cypherDriverDetails.Cyphers) to high(cypherDriverDetails.Cyphers) do begin
      if (IsEqualGUID(cypherDriverDetails.Cyphers[i].CypherGUID, cypherGUID)) then begin
        cypherDetails := cypherDriverDetails.Cyphers[i];
        retval        := True;
        break;
      end;

    end;

  end;

  Result := retval;

end;


 // ----------------------------------------------------------------------------
 // Encrypt data using the cypher/cypher driver identified
 // Encrypted data returned in "plaintext"
function TOTFEFreeOTFEBase._EncryptData(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; var key: TSDUBytes; var IV: Ansistring; var plaintext: Ansistring;
  var cyphertext: Ansistring): Boolean;
begin
  // Call generic function...
  Result := _EncryptDecryptData(True, cypherKernelModeDeviceName, cypherGUID,
    key, IV, plaintext, cyphertext);
end;


 // ----------------------------------------------------------------------------
 // Decrypt data using the cypher/cypher driver identified
 // Decrypted data returned in "plaintext"
function TOTFEFreeOTFEBase._DecryptData(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; var key: TSDUBytes; var IV: Ansistring; var cyphertext: Ansistring;
  var plaintext: Ansistring): Boolean;
begin
  // Call generic function...
  Result := _EncryptDecryptData(False, cypherKernelModeDeviceName, cypherGUID,
    key, IV, cyphertext, plaintext);
end;


 // ----------------------------------------------------------------------------
 // Encrypt data using the cypher/cypher driver identified
 // Encrypted data returned in "plaintext"
function TOTFEFreeOTFEBase.EncryptSectorData(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; SectorID: LARGE_INTEGER; SectorSize: Integer; var key: TSDUBytes;
  var IV: Ansistring; var plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
begin
  // Call generic function...
  Result := EncryptDecryptSectorData(True, cypherKernelModeDeviceName,
    cypherGUID, SectorID, SectorSize, key, IV, plaintext, cyphertext);
end;


 // ----------------------------------------------------------------------------
 // Decrypt data using the cypher/cypher driver identified
 // Decrypted data returned in "plaintext"
function TOTFEFreeOTFEBase.DecryptSectorData(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; SectorID: LARGE_INTEGER; SectorSize: Integer; var key: TSDUBytes;
  var IV: Ansistring; var cyphertext: Ansistring; var plaintext: Ansistring): Boolean;
begin
  // Call generic function...
  Result := EncryptDecryptSectorData(False, cypherKernelModeDeviceName,
    cypherGUID, SectorID, SectorSize, key, IV, cyphertext, plaintext);
end;


 // ----------------------------------------------------------------------------
 // Note: Ignores any random padding data
function TOTFEFreeOTFEBase.ParseVolumeDetailsBlock(stringRep: Ansistring;
  var volumeDetailsBlock: TVolumeDetailsBlock): Boolean;
const
  // CDB format 1 and 2 volume flags were used to identify sector IV generation
  // method
  VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV       = 1;  // Bit 0
  VOL_FLAGS_PRE_CDB_3__SECTOR_ID_ZERO_VOLSTART   = 2;  // Bit 1
  // Bit 2 unused
  VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE = 8;  // Bit 3
var
  tmpByte:              Byte;
  tmpInt64:             Int64;
  tmpDWORD:             DWORD;
  allOK:                Boolean;
  i:                    DWORD;
  idx:                  DWORD;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  allOK := True;

  // The first byte in the string is at index 1
  idx := 1;

  // 8 bits: CDB Format ID...
  volumeDetailsBlock.CDBFormatID := Ord(stringRep[idx]);
  Inc(idx, 1);


  // 32 bits: Volume flags...
  tmpDWORD := 0;
  for i := 1 to 4 do begin
    tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
    tmpByte  := Ord(stringRep[idx]);
    Inc(idx);
    tmpDWORD := tmpDWORD + tmpByte;
  end;
  // CDBFormatID 1 CDBs (FreeOTFE v00.00.01 and v00.00.02) had a ***BUG*** that
  // caused the wrong value to be read back as the VolumeFlags
  if (volumeDetailsBlock.CDBFormatID < 2) then begin
    volumeDetailsBlock.VolumeFlags := Ord(stringRep[10]);
  end else begin
    volumeDetailsBlock.VolumeFlags := tmpDWORD;
  end;


  // For CDB Format IDs less than 2, determine the sector IV generation method
  // based on the volume flags, and switch off the volume flags used
  if (volumeDetailsBlock.CDBFormatID < 3) then begin
    // VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV and VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE -> foivgHash64BitSectorID
    // 0                                        and VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE -> foivgUnknown (can't hash sector ID if it's not being used!)
    // VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV and 0                                              -> foivg64BitSectorID
    // 0                                        and 0                                              -> foivgNone

    volumeDetailsBlock.SectorIVGenMethod := foivgUnknown;
    if ((volumeDetailsBlock.VolumeFlags and (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV or
      VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)) =
      (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV or
      VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)) then begin
      volumeDetailsBlock.SectorIVGenMethod := foivgHash64BitSectorID;
    end else
    if ((volumeDetailsBlock.VolumeFlags and
      (0 or VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)) =
      (0 or VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE)) then begin
      volumeDetailsBlock.SectorIVGenMethod := foivgUnknown;
    end else
    if ((volumeDetailsBlock.VolumeFlags and (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV or 0)) =
      (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV or 0)) then begin
      volumeDetailsBlock.SectorIVGenMethod := foivg64BitSectorID;
    end else
    if ((volumeDetailsBlock.VolumeFlags and (0 or 0)) = (0 or 0)) then begin
      volumeDetailsBlock.SectorIVGenMethod := foivgNone;
    end;

    volumeDetailsBlock.VolumeFlags :=
      volumeDetailsBlock.VolumeFlags and not (VOL_FLAGS_PRE_CDB_3__USE_SECTOR_ID_AS_IV or
      VOL_FLAGS_PRE_CDB_3__HASH_SECTOR_ID_BEFORE_USE);
  end;


  // 64 bits: Partition length...
  tmpInt64 := 0;
  for i := 1 to 8 do begin
    tmpInt64 := tmpInt64 shl 8;  // Shift 8 bits to the *left*
    tmpByte  := Ord(stringRep[idx]);
    Inc(idx);
    tmpInt64 := tmpInt64 + tmpByte;
  end;
  volumeDetailsBlock.PartitionLen := tmpInt64;


  // 32 bits: Master key length...
  tmpDWORD := 0;
  for i := 1 to 4 do begin
    tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
    tmpByte  := Ord(stringRep[idx]);
    Inc(idx);
    tmpDWORD := tmpDWORD + tmpByte;
  end;
  volumeDetailsBlock.MasterKeyLength := tmpDWORD;
  // Note: If decrypted badly, can't rely on: criticalData.MasterKeyLen to be
  //       valid
  //       We check if the volume details block's master key length implies
  //       would put idx beyond the amount of data we have
  if ((idx + (volumeDetailsBlock.MasterKeyLength div 8)) > DWORD(length(stringRep))) then begin
    allOK := False;
  end;

  // Variable bits: Master key...
  if (allOK) then begin
    volumeDetailsBlock.MasterKey :=
      Copy(stringRep, idx, (volumeDetailsBlock.MasterKeyLength div 8));
    Inc(idx, (volumeDetailsBlock.MasterKeyLength div 8));
  end;


  // 8 bits: Requested drive letter...
  if (allOK) then begin
    volumeDetailsBlock.RequestedDriveLetter := stringRep[idx];
    Inc(idx);
  end;


  // If the CDB Format ID is 2 or more, the CDB has a volume IV block
  // For CDB Format IDs less than 2, use a NULL volume IV
  if (volumeDetailsBlock.CDBFormatID < 2) then begin
    volumeDetailsBlock.VolumeIVLength := 0;
    volumeDetailsBlock.VolumeIV       := '';
  end else begin
    // 32 bits: Volume IV length...
    tmpDWORD := 0;
    for i := 1 to 4 do begin
      tmpDWORD := tmpDWORD shl 8;  // Shift 8 bits to the *left*
      tmpByte  := Ord(stringRep[idx]);
      Inc(idx);
      tmpDWORD := tmpDWORD + tmpByte;
    end;
    volumeDetailsBlock.VolumeIVLength := tmpDWORD;
    // Note: If decrypted badly, can't rely on: criticalData.MasterKeyLen to be
    //       valid
    //       We check if the volume details block's master key length implies
    //       would put idx beyond the amount of data we have
    if ((idx + (volumeDetailsBlock.VolumeIVLength div 8)) > DWORD(length(stringRep))) then begin
      allOK := False;
    end;

    // Variable bits: Master key...
    if (allOK) then begin
      volumeDetailsBlock.VolumeIV :=
        Copy(stringRep, idx, (volumeDetailsBlock.VolumeIVLength div 8));
      Inc(idx, (volumeDetailsBlock.VolumeIVLength div 8));
    end;

  end;


  // For CDB Format IDs less than 2, determine the sector IV generation method
  // based on the volume flags, and reset the volume flags
  if (volumeDetailsBlock.CDBFormatID < 3) then begin
    // volumeDetailsBlock.SectorIVGenMethod already set above when by processing VolumeFlags
  end else begin
    // 8 bits: Sector IV generation method...
    tmpByte                              := Ord(stringRep[idx]);
    // NEXT LINE COMMENTED OUT TO PREVENT COMPILER WARNING
    //    inc(idx, 1);
    volumeDetailsBlock.SectorIVGenMethod := foivgUnknown;
    for tmpSectorIVGenMethod := low(TFreeOTFESectorIVGenMethod)
      to high(TFreeOTFESectorIVGenMethod) do begin
      if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = tmpByte) then begin
        volumeDetailsBlock.SectorIVGenMethod := tmpSectorIVGenMethod;
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
function TOTFEFreeOTFEBase.BuildVolumeDetailsBlock(volumeDetailsBlock: TVolumeDetailsBlock;
  var stringRep: Ansistring): Boolean;
var
  tmpDWORD:  DWORD;
  tmpInt64:  Int64;
  tmpByte:   Byte;
  i:         Integer;
  allOK:     Boolean;
  tmpString: Ansistring;
begin
  allOK := True;

  volumeDetailsBlock.CDBFormatID := CDB_FORMAT_ID;

  stringRep := '';

  // 8 bits: CDB Format ID...
  stringRep := stringRep + ansichar(volumeDetailsBlock.CDBFormatID);


  // 32bits: Volume flags...
  tmpDWORD  := volumeDetailsBlock.VolumeFlags;
  tmpString := '';
  for i := 1 to 4 do begin
    tmpByte   := (tmpDWORD and $FF);
    tmpDWORD  := tmpDWORD shr 8;  // Shift 8 bits to the *right*
    tmpString := ansichar(tmpByte) + tmpString;
  end;
  stringRep := stringRep + tmpString;


  // 64 bits: Partition length...
  tmpInt64  := volumeDetailsBlock.PartitionLen;
  tmpString := '';
  for i := 1 to 8 do begin
    tmpByte   := (tmpInt64 and $FF);
    tmpInt64  := tmpInt64 shr 8;  // Shift 8 bits to the *right*
    tmpString := ansichar(tmpByte) + tmpString;
  end;
  stringRep := stringRep + tmpString;


  // 32 bits: Master key length...
  tmpDWORD  := volumeDetailsBlock.MasterKeyLength;
  tmpString := '';
  for i := 1 to 4 do begin
    tmpByte   := (tmpDWORD and $FF);
    tmpDWORD  := tmpDWORD shr 8;  // Shift 8 bits to the *right*
    tmpString := ansichar(tmpByte) + tmpString;
  end;
  stringRep := stringRep + tmpString;


  // Variable bits: Master key...
  stringRep := stringRep + Copy(volumeDetailsBlock.MasterKey, 1,
    (volumeDetailsBlock.MasterKeyLength div 8));


  // Requested drive letter...
  stringRep := stringRep + volumeDetailsBlock.RequestedDriveLetter;


  if (volumeDetailsBlock.CDBFormatID >= 2) then begin
    // 32 bits: Volume IV length...
    tmpDWORD  := volumeDetailsBlock.VolumeIVLength;
    tmpString := '';
    for i := 1 to 4 do begin
      tmpByte   := (tmpDWORD and $FF);
      tmpDWORD  := tmpDWORD shr 8;  // Shift 8 bits to the *right*
      tmpString := ansichar(tmpByte) + tmpString;
    end;
    stringRep := stringRep + tmpString;


    // Variable bits: Volume IV ...
    stringRep := stringRep + Copy(volumeDetailsBlock.VolumeIV, 1,
      (volumeDetailsBlock.VolumeIVLength div 8));


    if (volumeDetailsBlock.CDBFormatID >= 3) then begin
      // 8 bits: Sector IV generation method...
      stringRep := stringRep + ansichar(
        FreeOTFESectorIVGenMethodID[volumeDetailsBlock.SectorIVGenMethod]);
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
function TOTFEFreeOTFEBase.ReadVolumeCriticalData(filename: String;
  offsetWithinFile: Int64; userPassword: Ansistring; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  allOK:      Boolean;
  CDB:        Ansistring;
  prevCursor: TCursor;
begin
  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    allOK := ReadRawVolumeCriticalData(filename, offsetWithinFile, CDB);

    if allOK then begin
      allOK := ReadVolumeCriticalData_CDB(CDB, userPassword, saltLength,
        keyIterations, volumeDetails, CDBMetaData);
    end;
  finally
    Screen.Cursor := prevCursor;
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
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB(CDB: Ansistring;
  userPassword: Ansistring; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  allOK: Boolean;
begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Attempting v2 format CDB decode...');
{$ENDIF}
  allOK := ReadVolumeCriticalData_CDB_v2(CDB, userPassword, saltLength,
    keyIterations, volumeDetails, CDBMetaData);

  if not (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('v2 format CDB decode failed - attempting v1...');
{$ENDIF}
    allOK := ReadVolumeCriticalData_CDB_v1(CDB, userPassword, saltLength,
      volumeDetails, CDBMetaData);
  end;


  if (allOK) then begin
    // Warn user if the volume appears to be a later version that this software
    // supports
    if (volumeDetails.CDBFormatID > CDB_FORMAT_ID) then begin
      SDUMessageDlg(
        _('WARNING: This Box appears to have been created with a later version of DoxBox than this software supports.'
        + SDUCRLF + SDUCRLF + 'You may continue, however:' + SDUCRLF +
        '1) Your data may be read incorrectly due to changes in the decryption process, and'
        +
        SDUCRLF + '2) It is not recommended that you write to this box using this software.'),
        mtWarning);
    end;

  end;

  Result := allOK;
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
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v1(CDB: Ansistring;
  userPassword: Ansistring; saltLength: Integer;  // In bits

  var volumeDetails: TVolumeDetailsBlock; var CDBMetaData: TCDBMetaData): Boolean;
var
  validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaData:   TCDBMetaDataArray;

  encryptedBlockLen:         Integer;  // In *bits*
  encryptedBlock:            Ansistring;
  decryptedBlock:            Ansistring;
  strVolumeDetailsBlock:     Ansistring;
  hashDrivers:               array of TFreeOTFEHashDriver;
  hi, hj:                    Integer;
  currHashDriver:            TFreeOTFEHashDriver;
  currHashImpl:              TFreeOTFEHash;
  cypherDrivers:             array of TFreeOTFECypherDriver;
  ci, cj:                    Integer;
  currCypherDriver:          TFreeOTFECypherDriver;
  currCypherImpl:            TFreeOTFECypher_v3;
  allOK:                     Boolean;
  salt:                      Ansistring;
  saltedUserPassword:        Ansistring;
  derivedKey:                TSDUBytes;   // The key to be used in decrypting the CDB, after
                                          // salting, hashing, HMACing, etc - but before any
                                          // truncating/padding as a result of the cypher's keysize
  hk:                        Integer;     // Hash value length, in *bits*
  ks:                        Integer;     // Cypher keysize, in *bits*
  bs:                        Integer;     // Cypher blocksize, in *bits*
  criticalDataKey:           TSDUBytes;
  IV:                        Ansistring;
  checkDataDecrypted:        String;
  checkDataGenerated:        TSDUBytes;
  strDecryptedVolDetailsBlk: String;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData:   TCDBMetaData;

  i:                     Integer;
  tgtVolDetailsBlockLen: Integer;  // In *bits*
  prevCursor:            TCursor;
  derivedKeyStr, criticalDataKeyStr, checkDataGeneratedStr: Ansistring;
begin
  SetLength(validVolumeDetails, 0);
  SetLength(validCDBMetaData, 0);


  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // NOTE: "allOK" indicates a *fatal* error in processing; not that an invalid
    //       password was entered
    allOK := True;


    // Append the salt onto the user's password
    if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Determining user''s salted password...');
  {$ENDIF}
      salt               := Copy(CDB, 1, (saltLength div 8));
      saltedUserPassword := userPassword + salt;
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('User''s salted key:');
  DebugMsg('-- begin --');
  DebugMsgBinary(saltedUserPassword);
  DebugMsg('-- end --');
  {$ENDIF}
    end;

    // Obtain details of all hashes...
    if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting hash drivers...');
  {$ENDIF}
      SetLength(hashDrivers, 0);
      allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
    end;

    // Obtain details of all cyphers...
    if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting cypher drivers...');
  {$ENDIF}
      SetLength(cypherDrivers, 0);
      allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
    end;


    if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Starting main hashes loop...');
  {$ENDIF}
      // FOR ALL HASH DRIVERS...
      for hi := low(hashDrivers) to high(hashDrivers) do begin
        currHashDriver := hashDrivers[hi];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using hash driver: '+currHashDriver.Title+' ('+GUIDToString(currHashDriver.DriverGUID)+')');
  {$ENDIF}
        // FOR ALL HASH ALGORITHMS SUPPORTED BY THE CURRENT DRIVER...
        for hj := low(hashDrivers[hi].Hashes) to high(hashDrivers[hi].Hashes) do begin
          currHashImpl := currHashDriver.Hashes[hj];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using hash impl: '+GetHashDisplayTechTitle(currHashImpl));
  {$ENDIF}
          hk           := currHashImpl.Length;
          if (hk < 0) then begin
            // Assume 512 bits for the hash length, if the hash supports arbitary
            // lengths
            hk := 512;
          end;


  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('hk: '+inttostr(hk));
  DebugMsg('About to derive key from user''s password and salt');
  {$ENDIF}

          // HASH THE USER's PASSWORD+SALT...
          if not (HashData(currHashDriver.LibFNOrDevKnlMdeName, currHashImpl.HashGUID,
            SDUStringToSDUBytes(saltedUserPassword), derivedKey)) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Hashing FAILED.');
  {$ENDIF}
            allOK := False;
          end;


          if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Hashed data OK.');
  DebugMsg('Processed user''s password and salt to give:');
  DebugMsgBinary(SDUBytesToString( derivedKey));
  {$ENDIF}
            // FOR ALL CYPHER DRIVERS...
            for ci := low(cypherDrivers) to high(cypherDrivers) do begin
              currCypherDriver := cypherDrivers[ci];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
  {$ENDIF}
              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
              for cj := low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do begin
                currCypherImpl := currCypherDriver.Cyphers[cj];
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
  {$ENDIF}

                                                       // Determine keysize...
                ks := currCypherImpl.KeySizeRequired;  // In *bits*
                                                       // Determine blocksize...
                bs := currCypherImpl.BlockSize;
                if (bs <= 0) then begin
                  // Assume 8 bits for the blocksize, if the cypher supports arbitary
                  // block sizes
                  bs := 8;
                end;

                // Extract the encrypted block from the critical data block
                encryptedBlockLen := (((CRITICAL_DATA_LENGTH - saltLength) div bs) * bs);
                // +1 because we index from 1, not zero
                encryptedBlock    :=
                  Copy(CDB, ((saltLength div 8) + 1), (encryptedBlockLen div 8));



                // Determine the criticalDataKey...
                // Fallback - assign no key (e.g. if the cypher's keysize = 0 bits)
                criticalDataKey := SDUStringToSDUBytes('');
                if (ks < 0) then begin
                  // If the cypher's keysize is -1, then the critical data block
                  // en/decryption key is the hash value returned
                  criticalDataKey := derivedKey;
                end else begin
                  derivedKeyStr := SDUBytesToString(derivedKey);
                  if ((Length(derivedKeyStr) * 8) < ks) then begin
                    // The hash value isn't long enough; right pad with zero bytes
                    criticalDataKeyStr :=
                      derivedKeyStr + StringOfChar(AnsiChar(#0), ((ks div 8) - Length(derivedKeyStr)));
                  end else begin
                    // The hash value is too long; truncate
                    criticalDataKeyStr := Copy(derivedKeyStr, 1, (ks div 8));
                  end;
                  // copy derivedKey, and pad if no long enough
                  SDUAddLimit(criticalDataKey, derivedKey, ks div 8);
                  SDUResetLength(criticalDataKey, ks div 8);
                  assert(criticalDataKeyStr = SDUBytesToString(criticalDataKey));
                end;  // if (ks > 0) then



                // Zero the IV for decryption
                IV := StringOfChar(AnsiChar(#0), (bs div 8));

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('About to decrypt data...');
  {$ENDIF}
                if not (DecryptSectorData(currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, FREEOTFE_v1_DUMMY_SECTOR_ID,
                  FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKey, IV,
                  encryptedBlock, decryptedBlock)) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Decryption FAILED.');
  {$ENDIF}
                  allOK := False;
                end else begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Key:');
  DebugMsgBinary(SDUBytesToString(criticalDataKey));
  DebugMsg('Decoded to:');
  DebugMsgBinary(decryptedBlock);
  {$ENDIF}
                  // Extract the decrypted check hash and volume details block
                  // 1 and +1 because we index from 1
                  checkDataDecrypted    := Copy(decryptedBlock, 1, (hk div 8));
                  tgtVolDetailsBlockLen := (((4096 - saltLength) div bs) * bs) - hk;
                  strVolumeDetailsBlock :=
                    Copy(decryptedBlock, ((hk div 8) + 1), (tgtVolDetailsBlockLen div 8));
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('About to generate hash/HMAC using decoded data...');
  {$ENDIF}


                  // HASH THE USER DECODED DATA...
                  if not (HashData(currHashDriver.LibFNOrDevKnlMdeName,
                    currHashImpl.HashGUID, SDUStringToSDUBytes(strVolumeDetailsBlock),
                    checkDataGenerated)) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Hash of decrypted data FAILED.');
  {$ENDIF}
                    allOK := False;
                  end;


                  if (allOK) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Decrypted data hashed OK.');
  {$ENDIF}
                    // If the checkdata newly generated is less than hk bits, right pad it with zero bits
                    if ((Length(checkDataGenerated) * 8) < hk) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash less than hk bits; padding...');
  {$ENDIF}
                      checkDataGeneratedStr := SDUBytesToString(checkDataGenerated);
                      checkDataGeneratedStr :=
                        checkDataGeneratedStr + StringOfChar(AnsiChar(#0),
                        (((Length(checkDataGeneratedStr) * 8) - hk) div 8));

                      SDUResetLength(checkDataGenerated, hk);
                      assert(checkDataGeneratedStr = SDUBytesToString(checkDataGenerated));
                    end;

                    // If the new checkdata is greater than hk bits, truncate it after the first
                    // hk bits
                    if ((Length(checkDataGeneratedStr) * 8) > hk) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash more than hk bits; padding...');
  {$ENDIF}
                      // +1 because we index from 1, not zero
                      Delete(checkDataGeneratedStr, (hk div 8) + 1,
                        (Length(checkDataGeneratedStr) - (hk div 8)));
                    end;

                    if ((Length(checkDataGenerated) * 8) > hk) then begin
                      // +1 because we index from 1, not zero
                      SDUResetLength(checkDataGenerated, hk);
                    end;
                    assert(checkDataGeneratedStr = SDUBytesToString(checkDataGenerated));


                    if (checkDataDecrypted = SDUBytesToString(checkDataGenerated)) then begin
                      // We have found the hash and cypher to successfully decrypt!

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('SUCCESS! Found valid hash/cypher combination!');
  DebugMsg('Volume details block is length: '+inttostr(length(strVolumeDetailsBlock)));
  {$ENDIF}
                      if not (ParseVolumeDetailsBlock(strVolumeDetailsBlock, currVolumeDetails))
                      then begin
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
                      end else begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('OK so far...');
  {$ENDIF}
                        // Sanity check...
                        if ((ks >= 0) and
                          (currVolumeDetails.MasterKeyLength <>
                          DWORD(ks)))
                        then begin
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
                        SetLength(validVolumeDetails, (Length(validVolumeDetails) + 1));
                        SetLength(validCDBMetaData, (Length(validCDBMetaData) + 1));
                        validVolumeDetails[Length(validVolumeDetails) - 1] := currVolumeDetails;
                        validCDBMetaData[Length(validCDBMetaData) - 1]     := currCDBMetaData;
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('OK.');
  {$ENDIF}

                        // Additional: Store internal information, if dumping
                        if fdumpFlag then begin
                          fDumpCriticalDataKey         := SDUBytesToString(criticalDataKey);
                          fDumpCheckMAC                := checkDataDecrypted;
                          fDumpPlaintextEncryptedBlock := decryptedBlock;
                          fDumpVolumeDetailsBlock      := strVolumeDetailsBlock;
                        end;

                      end;
                      // ELSE PART - if not(ParseVolumeDetailsBlock(strVolumeDetailsBlock, currVolumeDetails)) then

                    end else begin
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
  if (allOK) then begin
    allOK := ChooseFromValid(validVolumeDetails, validCDBMetaData, volumeDetails,
      CDBMetaData);
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
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v2(CDB: Ansistring;
  userPassword: Ansistring; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaData:   TCDBMetaDataArray;

  hashDrivers:    array of TFreeOTFEHashDriver;
  hi, hj:         Integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHashImpl:   TFreeOTFEHash;

  cypherDrivers:    array of TFreeOTFECypherDriver;
  ci, cj:           Integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypherImpl:   TFreeOTFECypher_v3;

  allOK:           Boolean;
  salt:            Ansistring;
  derivedKey:      Ansistring;  // The key to be used in decrypting the CDB, after
                                // salting, hashing, HMACing, etc - but before any
                                // truncating/padding as a result of the cypher's keysize
  cdkSize_bits:    Integer;     // Size of the "critical data key", in *bits*
  maxCDKSize_bits: Integer;     // Max size of the "critical data key", in *bits*
  IV:              Ansistring;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData:   TCDBMetaData;

  i:                  Integer;
  leb_bits:           Integer;  // In *bits*
  criticalDataKey:    Ansistring;
  maxCriticalDataKey: TSDUBytes;

  paddedEncryptedBlock:    Ansistring;
  encryptedBlock:          Ansistring;
  plaintextEncryptedBlock: Ansistring;

  paddedCheckMAC: Ansistring;
  checkMAC:       Ansistring;
  generatedMAC:   Ansistring;

  volumeDetailsBlock: Ansistring;

  MACAlgorithm:         TFreeOTFEMACAlgorithm;
  KDFAlgorithm:         TFreeOTFEKDFAlgorithm;
  prevCursor:           TCursor;
  criticalDataKeyBytes: TSDUBytes;
begin
  SetLength(validVolumeDetails, 0);
  SetLength(validCDBMetaData, 0);


  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    MACAlgorithm := fomacHMAC;
    KDFAlgorithm := fokdfPBKDF2;

    // NOTE: "allOK" indicates whether or not a *fatal* error in processing;
    //       NOT that an invalid password was entered
    allOK := True;


    // Obtain details of all hashes...
    if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Getting hash drivers...');
{$ENDIF}
      SetLength(hashDrivers, 0);
      allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
    end;


    // Obtain details of all cyphers...
    if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Getting cypher drivers...');
{$ENDIF}
      SetLength(cypherDrivers, 0);
      allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
    end;


    // Strip off salt
    if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Extracting salt...');
{$ENDIF}
      salt                 := Copy(CDB, 1, (saltLength div 8));
      paddedEncryptedBlock := Copy(CDB, ((saltLength div 8) + 1),
        (Length(CDB) - (saltLength div 8)));
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Salt follows:');
DebugMsgBinary(salt);
{$ENDIF}
    end;



    // !!! OPTIMISATION !!!
    // Determine the longest cypher key
    if (allOK) then begin
      maxCDKSize_bits := 0;

      // FOR ALL CYPHER DRIVERS...
      for ci := low(cypherDrivers) to high(cypherDrivers) do begin
        currCypherDriver := cypherDrivers[ci];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
{$ENDIF}
        // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
        for cj := low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do begin
          currCypherImpl := currCypherDriver.Cyphers[cj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
{$ENDIF}

          // Determine size of "critical data key"...
          cdkSize_bits := currCypherImpl.KeySizeRequired;  // In *bits*
          if (cdkSize_bits < 0) then begin
            cdkSize_bits := 512;
          end;

          maxCDKSize_bits := max(maxCDKSize_bits, cdkSize_bits);

        end;
      end;
    end;



    if (allOK) then begin
      try
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Starting main hashes loop...');
{$ENDIF}
        // FOR ALL HASH DRIVERS...
        for hi := low(hashDrivers) to high(hashDrivers) do begin
          currHashDriver := hashDrivers[hi];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using hash driver: '+currHashDriver.Title+' ('+GUIDToString(currHashDriver.DriverGUID)+')');
{$ENDIF}
          // FOR ALL HASH ALGORITHMS SUPPORTED BY THE CURRENT DRIVER...
          for hj := low(currHashDriver.Hashes) to high(currHashDriver.Hashes) do begin
            currHashImpl := currHashDriver.Hashes[hj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using hash impl: '+GetHashDisplayTechTitle(currHashImpl));
{$ENDIF}


            // v2 volumes won't work with hash algorithms with <= 0 length, or
            // blocksize, as HMAC/PBKDF2 require these to be fixed
            //  - skip these, and move onto the next hash algorithm (if any)
            if ((currHashImpl.Length <= 0) or (currHashImpl.BlockSize <= 0)) then begin
              Continue;
            end;

            // !!! OPTIMISATION !!!
            // Because we only allow PBKDF2, which:
            //   a) Doesn't need a cypher
            //   b) Short keys are identical to truncated longer keys
            // We derive the key *here*, outside the cypher loop
            if (KDFAlgorithm <> fokdfPBKDF2) then begin
              raise EFreeOTFEInternalError.Create('PBKDF2 assumed - optimisation failed');
            end;

            allOK := DeriveKey(KDFAlgorithm, currHashDriver.LibFNOrDevKnlMdeName,
              currHashImpl.HashGUID, '', StringToGUID(NULL_GUID),
              userPassword, SDUStringToSDUBytes(salt), keyIterations,
              maxCDKSize_bits,  // cdkSizeBits
              maxCriticalDataKey);
            if not (allOK) then begin
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
                _('Failed to derive critical data key.') + SDUCRLF +
                SDUCRLF + _('Hash:') + SDUCRLF + '  ' +
                currHashDriver.LibFNOrDevKnlMdeName + SDUCRLF + '  ' +
                GUIDToString(currHashImpl.HashGUID),
                mtError
                );
              // Bail out...
              raise EFreeOTFEReadCDBFailure.Create('Failed to derive key');
            end;


            // FOR ALL CYPHER DRIVERS...
            for ci := low(cypherDrivers) to high(cypherDrivers) do begin
              currCypherDriver := cypherDrivers[ci];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher driver: '+currCypherDriver.Title+' ('+GUIDToString(currCypherDriver.DriverGUID)+')');
{$ENDIF}
              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
              for cj := low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do begin
                currCypherImpl := currCypherDriver.Cyphers[cj];
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Checking using cypher impl: '+GetCypherDisplayTechTitle(currCypherImpl));
{$ENDIF}

                // Determine size of "critical data key"...
                cdkSize_bits := currCypherImpl.KeySizeRequired;  // In *bits*
                if (cdkSize_bits < 0) then begin
                  cdkSize_bits := 512;
                end;


                // !!! OPTIMISATION !!!
                criticalDataKey :=
                  Copy(SDUBytesToString(maxCriticalDataKey), 1, (cdkSize_bits div 8));


                // Calculate "leb"
                if (currCypherImpl.BlockSize > 8) then begin
                  leb_bits := ((CRITICAL_DATA_LENGTH - saltLength) div currCypherImpl.BlockSize) *
                    currCypherImpl.BlockSize;
                end else begin
                  leb_bits := (CRITICAL_DATA_LENGTH - saltLength);
                end;

                // Strip off the padding after the encrypted block
                encryptedBlock := Copy(paddedEncryptedBlock, 1, (leb_bits div 8));

                // Zero the IV for decryption
                IV := '';
                if (currCypherImpl.BlockSize > 0) then begin
                  IV := StringOfChar(AnsiChar(#0), (currCypherImpl.BlockSize div 8));
                end;
                criticalDataKeyBytes := SDUStringToSDUBytes(criticalDataKey);
                allOK                := DecryptSectorData(
                  currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, FREEOTFE_v1_DUMMY_SECTOR_ID,
                  FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKeyBytes, IV,
                  encryptedBlock, plaintextEncryptedBlock);
                criticalDataKey      := SDUBytesToString(criticalDataKeyBytes);
                if not (allOK) then begin
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
                    _('Failed to decrypt encrypted block.') + SDUCRLF +
                    SDUCRLF + _('Cypher:') + SDUCRLF + '  ' +
                    currCypherDriver.LibFNOrDevKnlMdeName + SDUCRLF + '  ' +
                    GUIDToString(currCypherImpl.CypherGUID),
                    mtError
                    );
                  // Bail out...
                  raise EFreeOTFEReadCDBFailure.Create('Failed to decrypt encrypted block');
                end;


                paddedCheckMAC     := Copy(plaintextEncryptedBlock, 1, (CDB_MAX_MAC_LENGTH div 8));
                volumeDetailsBlock :=
                  Copy(plaintextEncryptedBlock, ((CDB_MAX_MAC_LENGTH div 8) + 1),
                  (Length(plaintextEncryptedBlock) - (CDB_MAX_MAC_LENGTH div 8)));

                allOK := MACData(MACAlgorithm,
                  currHashDriver.LibFNOrDevKnlMdeName, currHashImpl.HashGUID,
                  currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, criticalDataKey,
                  volumeDetailsBlock, generatedMAC, -1);
                if not (allOK) then begin
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
                    _('Failed to generate check MAC.') + SDUCRLF +
                    _('Hash:') + SDUCRLF + '  ' +
                    currHashDriver.LibFNOrDevKnlMdeName + SDUCRLF + '  ' +
                    GUIDToString(currHashImpl.HashGUID) + SDUCRLF + _('Cypher:') +
                    SDUCRLF + '  ' + currCypherDriver.LibFNOrDevKnlMdeName +
                    SDUCRLF + '  ' + GUIDToString(currCypherImpl.CypherGUID) + SDUCRLF,
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
                if (length(generatedMAC) > (CDB_MAX_MAC_LENGTH * 8)) then begin
                  // +1 because we index from 1, not zero
                  Delete(
                    generatedMAC,
                    ((CDB_MAX_MAC_LENGTH * 8) + 1),
                    (length(generatedMAC) - (CDB_MAX_MAC_LENGTH * 8))
                    );
                end;

                checkMAC := Copy(paddedCheckMAC, 1, Length(generatedMAC));

                // Check if we've found a valid hash/cypher combination
                if (checkMAC = generatedMAC) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('SUCCESS! Found valid hash/cypher combination!');
DebugMsg('Volume details block is length: '+inttostr(length(volumeDetailsBlock)));
{$ENDIF}

                  if not (ParseVolumeDetailsBlock(volumeDetailsBlock, currVolumeDetails)) then
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
                  end else begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('OK so far...');
{$ENDIF}
                    // Sanity check...
                    if ((currCypherImpl.KeySizeRequired >= 0) and
                      (currVolumeDetails.MasterKeyLength <>
                      DWORD(currCypherImpl.KeySizeRequired))) then begin
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
                    SetLength(validVolumeDetails, (Length(validVolumeDetails) + 1));
                    SetLength(validCDBMetaData, (Length(validCDBMetaData) + 1));
                    validVolumeDetails[Length(validVolumeDetails) - 1] := currVolumeDetails;
                    validCDBMetaData[Length(validCDBMetaData) - 1]     := currCDBMetaData;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('OK.');
{$ENDIF}

                    // Additional: Store internal information, if dumping
                    if fDumpFlag then begin
                      fdumpCriticalDataKey         := criticalDataKey;
                      fdumpCheckMAC                := checkMAC;
                      fdumpPlaintextEncryptedBlock := plaintextEncryptedBlock;
                      fdumpVolumeDetailsBlock      := volumeDetailsBlock;
                    end;

                  end;
                  // ELSE PART - if not(ParseVolumeDetailsBlock(volumeDetailsBlock, currVolumeDetails)) then

                end else begin
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
        on EFreeOTFEReadCDBFailure do begin
          allOK := False;
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
  if (allOK) then begin
    allOK := ChooseFromValid(validVolumeDetails, validCDBMetaData, volumeDetails,
      CDBMetaData);
  end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Finished function.');
{$ENDIF}

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ChooseFromValid(validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaDatas: TCDBMetaDataArray; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  allOK:               Boolean;
  hashCypherSelectDlg: TfrmSelectHashCypher;
  i:                   Integer;
  prevCursor:          TCursor;
begin
  allOK := True;

  if (Length(validCDBMetaDatas) = 0) then begin
    // No valid combination of hash/cypher could be found
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('No valid hash/cypher combination found');
{$ENDIF}
    allOK := False;
  end else
  if (Length(validCDBMetaDatas) = 1) then begin
    // There is only one valid hash/cypher combination that can be used; use it
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exactly one hash/cypher combination found successfully');
{$ENDIF}
    volumeDetails := validVolumeDetails[low(validVolumeDetails)];
    CDBMetaData   := validCDBMetaDatas[low(validCDBMetaDatas)];
  end else begin
    // More than one valid hash/cypher combination; offer the user the choice
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('More than one hash/cypher combination found');
{$ENDIF}
    hashCypherSelectDlg := TfrmSelectHashCypher.Create(nil);
    prevCursor          := Screen.Cursor;
    Screen.Cursor       := crDefault;
    try
      hashCypherSelectDlg.FreeOTFEObj := self;
      for i := low(validCDBMetaDatas) to high(validCDBMetaDatas) do begin
        hashCypherSelectDlg.AddCombination(
          validCDBMetaDatas[i].HashDriver,
          validCDBMetaDatas[i].HashGUID,
          validCDBMetaDatas[i].CypherDriver,
          validCDBMetaDatas[i].CypherGUID
          );
      end;

      if (hashCypherSelectDlg.ShowModal() <> mrOk) then begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('User cancelled');
{$ENDIF}
        allOK         := False;
      end else begin
        // Locate the selected hash/cypher
        for i := low(validCDBMetaDatas) to high(validCDBMetaDatas) do begin
          if ((validCDBMetaDatas[i].HashDriver =
            hashCypherSelectDlg.SelectedHashDriverKernelModeName()) and
            IsEqualGUID(validCDBMetaDatas[i].HashGUID,
            hashCypherSelectDlg.SelectedHashGUID()) and
            (validCDBMetaDatas[i].CypherDriver =
            hashCypherSelectDlg.SelectedCypherDriverKernelModeName()) and
            IsEqualGUID(validCDBMetaDatas[i].CypherGUID,
            hashCypherSelectDlg.SelectedCypherGUID())) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('User''s selected hash/cypher combination found.');
{$ENDIF}
            volumeDetails := validVolumeDetails[i];
            CDBMetaData   := validCDBMetaDatas[i];
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
function TOTFEFreeOTFEBase.WriteVolumeCriticalData(filename: String;
  offsetWithinFile: Int64; userPassword: PasswordString; salt: TSDUBytes;
  keyIterations: Integer; volumeDetails: TVolumeDetailsBlock; CDBMetaData: TCDBMetaData
  // Note: If insufficient is supplied, the write will
  //       *fail*
  //       The maximum that could possibly be required
  //       is CRITICAL_DATA_LENGTH bits.
  ): Boolean;
var
  encryptedBlock:                         Ansistring;
  allOK:                                  Boolean;
  sl_bits:                                Integer;  // Salt value length, in *bits*
  criticalDataKey:                        TSDUBytes;
  criticalDataKeyStr:                     Ansistring;
  IV:                                     Ansistring;
  cypherDetails:                          TFreeOTFECypher_v3;
  hashDetails:                            TFreeOTFEHash;
  volumeDetailsBlockNoPadding:            Ansistring;
  volumeDetailsBlock:                     Ansistring;
  criticalDataBlock:                      Ansistring;
  cdkSize_bits:                           Integer;  // In *bits*
  randomPaddingTwoLength_bits:            Integer;  // In *bits*
  leb_bits:                               Integer;  // In *bits*
  volumeDetailsBlockLength_bits:          Integer;  // In *bits*
  volumeDetailsBlockNoPaddingLength_bits: Integer;  // In *bits*
  checkMAC:                               Ansistring;
  paddedCheckMAC:                         Ansistring;
  randomPaddingThreeLength_bits:          Integer;  // In *bits*
  plaintextEncryptedBlock:                Ansistring;
  randomPaddingOneLength_bits:            Integer;  // In *bits*

  randomPadData: TSDUBytes;
begin
  allOK := True;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In WriteVolumeCriticalData');
{$ENDIF}

  // Force set; this function only supports this one CDB format
  CDBMetaData.MACAlgorithm := fomacHMAC;
  CDBMetaData.KDFAlgorithm := fokdfPBKDF2;


  // Set to 0 to get rid of compiler warning
  randomPaddingOneLength_bits := 0;

  sl_bits := Length(salt) * 8;

  // Note: No need to populate CDBMetaData.MACAlgorithm or
  //       CDBMetaData.KDFAlgorithm as the function which writes the CDB out
  //       populates them automatically



  // Get details of the cypher so we know the key and blocksize...
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to get cypher details...');
{$ENDIF}
    if GetSpecificCypherDetails(CDBMetaData.CypherDriver, CDBMetaData.cypherGUID,
      cypherDetails) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Cypher driver: '+CDBMetaData.CypherDriver);
DebugMsg('Cypher impl: '+cypherDetails.Title+' ('+GUIDToString(CDBMetaData.CypherGUID)+')');
{$ENDIF}
      //
    end else begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      allOK         := False;
    end;
  end;


  // Get details of the hash so we know the hash length...
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to get hash details...');
{$ENDIF}
    if GetSpecificHashDetails(CDBMetaData.HashDriver, CDBMetaData.hashGUID, hashDetails) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Hash driver: '+CDBMetaData.HashDriver);
DebugMsg('Hash impl: '+hashDetails.Title+' ('+GUIDToString(CDBMetaData.HashGUID)+')');
{$ENDIF}
    end else begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      allOK         := False;
    end;
  end;



  // Sanity checks
  if ((cypherDetails.BlockSize <= 0) and (volumeDetails.SectorIVGenMethod <> foivgNone)) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Sector IVs cannot be used if cypher blocksize <= 0');
  end;
  if ((cypherDetails.BlockSize <= 0) and ((volumeDetails.VolumeIVLength > 0) or
    (Length(volumeDetails.VolumeIV) > 0))) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Volume IV cannot be used if cypher blocksize <= 0');
  end;
  if ((cypherDetails.BlockSize > 0) and ((sl_bits mod cypherDetails.BlockSize) <> 0)) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Salt length must be a multiple of the cypher blocksize');
  end;
  if (hashDetails.Length = 0) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Hash selected cannot be used; hash length must be greater than zero due to PBKDF2');
  end;
  if (hashDetails.Length < 0) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Hash selected cannot be used; hash length must fixed due to PBKDF2');
  end;
  if (hashDetails.BlockSize = 0) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Hash selected cannot be used; zero blocksize means useless check MAC may will be generated (zero bytes) long with HMAC)');
  end;
  if (hashDetails.BlockSize < 0) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Hash selected cannot be used; hash blocksize must be fixed due to HMAC algorithm');
  end;
  if ((volumeDetails.MasterKeyLength mod 8) <> 0) then begin
    raise EFreeOTFEInternalError.Create('Invalid: Master key length must be multiple of 8');
  end;
  if ((cypherDetails.KeySizeRequired >= 0) and (volumeDetails.MasterKeyLength <>
    DWORD(cypherDetails.KeySizeRequired))) then begin
    raise EFreeOTFEInternalError.Create(
      'Invalid: Master key length must be the same as the cypher''s required keysize for cyphers with a fixed keysize');
  end;
  if ((volumeDetails.VolumeIVLength mod 8) <> 0) then begin
    raise EFreeOTFEInternalError.Create('Invalid: Volume IV length must be multiple of 8');
  end;



  // Determine size of "critical data key"...
  cdkSize_bits := cypherDetails.KeySizeRequired;  // In *bits*
  if (cdkSize_bits < 0) then begin
    cdkSize_bits := 512;
  end;


  // Derive the "critical data key"
  if (allOK) then begin
    allOK := DeriveKey(CDBMetaData.KDFAlgorithm, CDBMetaData.HashDriver,
      CDBMetaData.HashGUID, CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      userPassword, salt, keyIterations, cdkSize_bits, criticalDataKey);
    if not (allOK) then begin
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
      allOK         := False;
    end;
  end;


  // Create plaintext version of the "Volume details block" in-memory
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Converting volume details block structure to string rep...');
{$ENDIF}
    // Note: The string returned by BuildVolumeDetailsBlock(...) does *not*
    //       have any random data appended to it
    if not (BuildVolumeDetailsBlock(volumeDetails, volumeDetailsBlockNoPadding)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: FAILED to convert critical data structure to string rep.');
{$ENDIF}
      LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
      allOK         := False;
    end else begin
      if (cypherDetails.BlockSize > 8) then begin
        leb_bits := (((CRITICAL_DATA_LENGTH - sl_bits) div cypherDetails.BlockSize) *
          cypherDetails.BlockSize);
      end else begin
        leb_bits := (CRITICAL_DATA_LENGTH - sl_bits);
      end;

      randomPaddingOneLength_bits := (CRITICAL_DATA_LENGTH - sl_bits) - leb_bits;

      // Note: This *includes* the length of the "Random padding #2"
      volumeDetailsBlockLength_bits := leb_bits - CDB_MAX_MAC_LENGTH;

      // Note: This *excludes* the length of the "Random padding #2"
      volumeDetailsBlockNoPaddingLength_bits := (Length(volumeDetailsBlockNoPadding) * 8);

      randomPaddingTwoLength_bits :=
        volumeDetailsBlockLength_bits - volumeDetailsBlockNoPaddingLength_bits;


      // Pad out with "random data #2"...
      if (randomPaddingTwoLength_bits > 0) then begin
        GetRandPool().GetRandomData(randomPaddingTwoLength_bits div 8, randomPadData);
        volumeDetailsBlock := volumeDetailsBlockNoPadding + SDUBytesToString(randomPadData);
        SDUZeroBuffer(randomPadData);
{
        volumeDetailsBlock := volumeDetailsBlockNoPadding + Copy(
          randomPadData, 1, (randomPaddingTwoLength_bits div 8));
        // 1 because we index from 1, not zero
        Delete(randomPadData, 1, (randomPaddingTwoLength_bits div 8));
}
      end;

    end;

  end;


  // Generate the check MAC of the "volume details block"
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('About to HMAC critical data to create check data...');
{$ENDIF}
    criticalDataKeyStr := SDUBytesToString(criticalDataKey);
    allOK              := MACData(CDBMetaData.MACAlgorithm, CDBMetaData.HashDriver,
      CDBMetaData.HashGUID, CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      criticalDataKeyStr, volumeDetailsBlock, checkMAC, -1);
    criticalDataKey    := SDUStringToSDUBytes(criticalDataKeyStr);
    if not (allOK) then begin
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
      allOK         := False;
    end;

  end;


  // Truncate/pad check data as appropriate
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC check data generated OK.');
{$ENDIF}
    // If the checkdata newly generated is less than CDB_MAX_MAC_LENGTH bits,
    // right pad it with random data bits to CDB_MAX_MAC_LENGTH
    // If the checkdata newly generated is greater than CDB_MAX_MAC_LENGTH
    // bits, truncate it to the first CDB_MAX_MAC_LENGTH bits.
    if ((Length(checkMAC) * 8) < CDB_MAX_MAC_LENGTH) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Check data generated hash less than CDB_MAX_MAC_LENGTH bits; padding...');
{$ENDIF}
      randomPaddingThreeLength_bits := CDB_MAX_MAC_LENGTH - (Length(checkMAC) * 8);
{      paddedCheckMAC                :=
        checkMAC + Copy(randomPadData, 1, (randomPaddingThreeLength_bits div 8));
      // 1 because we index from 1, not zero
      Delete(randomPadData, 1, (randomPaddingThreeLength_bits div 8));  }


      GetRandPool().GetRandomData(randomPaddingThreeLength_bits div 8, randomPadData);
      paddedCheckMAC := checkMAC + SDUBytesToString(randomPadData);
      SDUZeroBuffer(randomPadData);

    end else
    if ((Length(checkMAC) * 8) > CDB_MAX_MAC_LENGTH) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Check data generated hash greater than CDB_MAX_MAC_LENGTH bits; truncating...');
{$ENDIF}
      paddedCheckMAC := checkMAC;
      // +1 because we index from 1, not zero
      Delete(
        paddedCheckMAC,
        ((CDB_MAX_MAC_LENGTH div 8) + 1),
        (Length(paddedCheckMAC) - (CDB_MAX_MAC_LENGTH div 8))
        );
    end else begin
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


  if (allOK) then begin
    // Prepend the check MAC (padded if necessary) to the volume details block
    // to form a plaintext version of the "Encrypted block"
    plaintextEncryptedBlock := paddedCheckMAC + volumeDetailsBlock;
  end;


  // Encrypt the plaintextEncryptedBlock, using
  //  - Zero'd IV
  //  - Critical data key
  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Creating zeroed IV');
{$ENDIF}
    // Zero the IV for decryption
    IV := '';
    if (cypherDetails.BlockSize > 0) then begin
      IV := StringOfChar(AnsiChar(#0), (cypherDetails.BlockSize div 8));
    end;


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('CriticalData.cypherDriverKernelModeName: '+CDBMetaData.CypherDriver);
DebugMsg('CriticalData.cypherGUID: '+GUIDToString(CDBMetaData.CypherGUID));
DebugMsg('CriticalDataKey len: '+inttostr(length(criticalDataKey)));
DebugMsg('IV len: '+inttostr(length(IV)));

DebugMsg('Critical data key:');
DebugMsgBinary(SDUBytesToString(criticalDataKey));
DebugMsg('before encryption, decrypted block is:');
DebugMsgBinary(plaintextEncryptedBlock);
{$ENDIF}
    if not (EncryptSectorData(CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      FREEOTFE_v1_DUMMY_SECTOR_ID, FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKey,
      IV, plaintextEncryptedBlock, encryptedBlock)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('ERROR: FAILED to encrypt data block');
{$ENDIF}
      LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
      allOK         := False;
    end;

  end;



  // Form the critical datablock using the salt bytes, the encrypted data block,
  // and random padding data #1
  if (allOK) then begin
    {criticalDataBlock := SDUBytesToString(salt) + encryptedBlock;
    Copy(
      randomPadData,
      1,
      (randomPaddingOneLength_bits div 8)
      );
    // 1 because we index from 1, not zero
    Delete(randomPadData, 1, (randomPaddingOneLength_bits div 8));  }

    GetRandPool().GetRandomData(randomPaddingOneLength_bits div 8, randomPadData);
    criticalDataBlock := SDUBytesToString(salt) + encryptedBlock +
      SDUBytesToString(randomPadData);
    SDUZeroBuffer(randomPadData);

  end;


  // Finally, write the critical data block out to the volume file, starting
  // from the appropriate offset
  if (allOK) then begin
    allOK := WriteRawVolumeCriticalData(filename, offsetWithinFile, criticalDataBlock);
  end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting function');
{$ENDIF}


  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetHashList(
  var hashDispTitles, hashKernelModeDriverNames, hashGUIDs: TStringList): Boolean;
var
  hashDrivers:    array of TFreeOTFEHashDriver;
  i, j, k:        Integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHash:       TFreeOTFEHash;
  allOK:          Boolean;
  tmpPrettyTitle: String;
  inserted:       Boolean;
begin
  allOK := False;

  SetLength(hashDrivers, 0);
  if (GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers))) then begin
    hashDispTitles.Clear();
    hashKernelModeDriverNames.Clear();
    hashGUIDs.Clear();

    for i := low(hashDrivers) to high(hashDrivers) do begin
      currHashDriver := hashDrivers[i];
      for j := low(hashDrivers[i].Hashes) to high(hashDrivers[i].Hashes) do begin
        currHash := hashDrivers[i].Hashes[j];

        tmpPrettyTitle := GetHashDisplayTitle(currHash);

        // Attempt to insert in alphabetic  order...
        inserted := False;
        for k := 0 to (hashDispTitles.Count - 1) do begin
          if (tmpPrettyTitle < hashDispTitles[k]) then begin
            hashDispTitles.Insert(k, tmpPrettyTitle);
            hashKernelModeDriverNames.Insert(k, currHashDriver.LibFNOrDevKnlMdeName);
            hashGUIDs.Insert(k, GUIDToString(currHash.HashGUID));
            inserted := True;
            break;
          end;

        end;

        if not (inserted) then begin
          hashDispTitles.Add(tmpPrettyTitle);
          hashKernelModeDriverNames.Add(currHashDriver.LibFNOrDevKnlMdeName);
          hashGUIDs.Add(GUIDToString(currHash.HashGUID));
        end;

      end;

    end;

    allOK := True;
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetCypherList(
  var cypherDispTitles, cypherKernelModeDriverNames, cypherGUIDs: TStringList): Boolean;
var
  cypherDrivers:    array of TFreeOTFECypherDriver;
  i, j, k:          Integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypher:       TFreeOTFECypher_v3;
  allOK:            Boolean;
  tmpPrettyTitle:   String;
  inserted:         Boolean;
begin
  allOK := False;

  SetLength(cypherDrivers, 0);
  if (GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers))) then begin
    for i := low(cypherDrivers) to high(cypherDrivers) do begin
      currCypherDriver := cypherDrivers[i];
      for j := low(cypherDrivers[i].Cyphers) to high(cypherDrivers[i].Cyphers) do begin
        currCypher := cypherDrivers[i].Cyphers[j];

        tmpPrettyTitle := GetCypherDisplayTitle(currCypher);

        // Attempt to insert in alphabetic  order...
        inserted := False;
        for k := 0 to (cypherDispTitles.Count - 1) do begin
          if (tmpPrettyTitle < cypherDispTitles[k]) then begin
            cypherDispTitles.Insert(k, tmpPrettyTitle);
            cypherKernelModeDriverNames.Insert(k, currCypherDriver.LibFNOrDevKnlMdeName);
            cypherGUIDs.Insert(k, GUIDToString(currCypher.CypherGUID));
            inserted := True;
            break;
          end;

        end;

        if not (inserted) then begin
          cypherDispTitles.Add(tmpPrettyTitle);
          cypherKernelModeDriverNames.Add(currCypherDriver.LibFNOrDevKnlMdeName);
          cypherGUIDs.Add(GUIDToString(currCypher.CypherGUID));
        end;

      end;

    end;

    allOK := True;
  end;

  Result := allOK;
end;


 // ----------------------------------------------------------------------------
 // Generate a prettyprinted title for a specific hash
 // Returns '' on error
function TOTFEFreeOTFEBase.GetHashDisplayTitle(hash: TFreeOTFEHash): String;
begin
  Result := hash.Title;
end;

 // ----------------------------------------------------------------------------
 // Generate a prettyprinted title for a specific hash
 // Returns '' on error
function TOTFEFreeOTFEBase.GetHashDisplayTechTitle(hash: TFreeOTFEHash): String;
begin
  Result := hash.Title + ' (' + IntToStr(hash.Length) + '/' + IntToStr(hash.BlockSize) + ')';
end;


 // ----------------------------------------------------------------------------
 // Generate a prettyprinted title for a specific cypher
 // Returns '' on error
function TOTFEFreeOTFEBase.GetCypherDisplayTitle(cypher: TFreeOTFECypher_v3): String;
var
  strCypherMode: String;
begin
  // This part prettifies the cypher details and populates the combobox
  // as appropriate
  strCypherMode := '';
  if (cypher.Mode = focmUnknown) then begin
    strCypherMode := _('Mode unknown');
  end else
  if (cypher.Mode <> focmNone) then begin
    strCypherMode := FreeOTFECypherModeTitle(cypher.Mode);
  end;

  Result := cypher.Title + ' (' + IntToStr(cypher.KeySizeUnderlying) + ' bit ' +
    strCypherMode + ')';

end;

 // ----------------------------------------------------------------------------
 // Generate a prettyprinted title for a specific cypher
 // Returns '' on error
function TOTFEFreeOTFEBase.GetCypherDisplayTechTitle(cypher: TFreeOTFECypher_v3): String;
var
  strCypherMode:  String;
  strCypherSizes: String;
begin
  // This part prettifies the cypher details and populates the combobox
  // as appropriate
  strCypherMode := '';
  if (cypher.Mode = focmUnknown) then begin
    // Note the semicolon-space on the end
    strCypherMode := _('Mode unknown; ');
  end else
  if (cypher.Mode <> focmNone) then begin
    // Note the semicolon-space on the end
    strCypherMode := FreeOTFECypherModeTitle(cypher.Mode) + '; ';
  end;

  strCypherSizes := IntToStr(cypher.KeySizeUnderlying) + '/' + IntToStr(cypher.BlockSize);

  Result := cypher.Title + ' (' + strCypherMode + strCypherSizes + ')';

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateFreeOTFEVolumeWizard(): Boolean;
var
  frmWizard: TfrmWizardCreateVolume;
  allOK:     Boolean;
  mr:        Integer;
begin
  allOK         := False;
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  CheckActive();

  if WarnIfNoHashOrCypherDrivers() then begin
    frmWizard := TfrmWizardCreateVolume.Create(nil);
    try
      frmWizard.fFreeOTFEObj := self;

      mr := frmWizard.ShowModal();
      if (mr = mrOk) then begin
        LastErrorCode := OTFE_ERR_SUCCESS;
        allOK         := True;
      end else
      if (mr = mrCancel) then begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
      end;

    finally
      frmWizard.Free();
    end;

  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateLinuxVolumeWizard(): Boolean;
var
  volSizeDlg: TfrmNewVolumeSize;
  mr:         Integer;
  allOK:      Boolean;
  userCancel: Boolean;
begin
  allOK := False;

  volSizeDlg := TfrmNewVolumeSize.Create(nil);
  try
    mr := volSizeDlg.ShowModal;
    if (mr = mrCancel) then begin
      LastErrorCode := OTFE_ERR_USER_CANCEL;
    end else
    if (mr = mrOk) then begin
      if not (SDUCreateLargeFile(volSizeDlg.Filename, volSizeDlg.VolumeSize, True, userCancel))
      then begin
        if not (userCancel) then begin
          SDUMessageDlg(_('An error occured while trying to create your DoxBox'),
            mtError, [mbOK], 0);
        end;

      end else begin
        allOK := True;
      end;
    end;

  finally
    volSizeDlg.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateLinuxGPGKeyfile(): Boolean;
var
  allOK: Boolean;
begin
  allOK := False;

  // xxx - implement GPG integration

  Result := allOK;
end;


 // ----------------------------------------------------------------------------
 // Display control for managing PKCS#11 tokens
procedure TOTFEFreeOTFEBase.ShowPKCS11ManagementDlg();
var
  pkcs11session:       TPKCS11Session;
  dlgPKCS11Session:    TfrmPKCS11Session;
  dlgPKCS11Management: TfrmPKCS11Management;
begin
  // Setup PKCS11 session, as appropriate
  pkcs11session := nil;
  if PKCS11LibraryReady(PKCS11Library) then begin
    dlgPKCS11Session := TfrmPKCS11Session.Create(nil);
    try
      dlgPKCS11Session.PKCS11LibObj := PKCS11Library;
      dlgPKCS11Session.AllowSkip    := False;
      if (dlgPKCS11Session.ShowModal = mrCancel) then begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
      end else begin
        pkcs11session := dlgPKCS11Session.Session;
      end;

    finally
      dlgPKCS11Session.Free();
    end;
  end;

  if (pkcs11session <> nil) then begin
    dlgPKCS11Management := TfrmPKCS11Management.Create(nil);
    try
      dlgPKCS11Management.FreeOTFEObj   := self;
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
    if DebugStrings.Count> 0 then begin
       inc(DebugLogfileCount);
       DebugStrings.SaveToFile(DebugLogfile+'.'+inttostr(DebugLogfileCount)+'.log');
    end;
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
procedure TOTFEFreeOTFEBase.DebugMsgBinaryPtr(msg: PAnsiChar; len:integer);
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
function TOTFEFreeOTFEBase.CanMountDevice(): Boolean;
begin
  // Supported
  Result := True;
end;


 // -----------------------------------------------------------------------------
 // Prompt the user for a device (if appropriate) and password (and drive
 // letter if necessary), then mount the device selected
 // Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEFreeOTFEBase.MountDevices(): Ansistring;
var
  selectedPartition:   String;
  mountedAs:           Ansistring;
  frmSelectVolumeType: TfrmSelectVolumeType;
  retVal:              Ansistring;
  mr:                  Integer;
begin
  mountedAs := '';

  CheckActive();

  if CanMountDevice() then begin
    // Ask the user if they want to mount their partitions as FreeOTFE or Linux
    // partitions
    frmSelectVolumeType := TfrmSelectVolumeType.Create(nil);
    try
      mr := frmSelectVolumeType.ShowModal();
      if (mr = mrCancel) then begin
        LastErrorCode := OTFE_ERR_USER_CANCEL;
      end else
      if (mr = mrOk) then begin
        // Ask the user which partition they want to mount
        selectedPartition := SelectPartition();
        if (selectedPartition = '') then begin
          LastErrorCode := OTFE_ERR_USER_CANCEL;
        end else begin
          // Mount the partition as the appropriate type
          if frmSelectVolumeType.FreeOTFEVolume then begin
            retVal := MountFreeOTFE(selectedPartition, False);
          end else begin
            retVal := MountLinux(selectedPartition, False);
          end;
        end;

      end;

    finally
      frmSelectVolumeType.Free();
      ;
    end;

  end;

  Result := retVal;

end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.BackupVolumeCriticalData(srcFilename: String;
  srcOffsetWithinFile: Int64; destFilename: String): Boolean;
var
  criticalData:   Ansistring;
  destFileStream: TFileStream;
  allOK:          Boolean;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  allOK := ReadRawVolumeCriticalData(srcFilename, srcOffsetWithinFile, criticalData);

  if (allOK) then begin
    // Because we're just writing to a straight file, we don't need the
    // FreeOTFE driver to do this.
    // Also, because the file shouldn't already exist, the FreeOTFE driver
    // *won't* do this - it expects the target to already exist
    destFileStream := TFileStream.Create(destFilename, fmCreate or fmShareDenyNone);
    try
      if length(criticalData) > 0 then begin
        allOK := (destFileStream.Write(criticalData[1], length(criticalData)) =
          length(criticalData));
      end;
    finally
      destFileStream.Free();
    end;

  end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.RestoreVolumeCriticalData(srcFilename: String;
  destFilename: String; destOffsetWithinFile: Int64): Boolean;
var
  criticalData: Ansistring;
  allOK:        Boolean;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  allOK := ReadRawVolumeCriticalData(srcFilename, 0, criticalData);

  if (allOK) then begin
    // Get the FreeOTFE driver to write the critical data block to the named file
    // starting from the specified offset
    allOK := WriteRawVolumeCriticalData(destFilename, destOffsetWithinFile, criticalData);
  end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CreateKeyfile(srcFilename: String; srcOffsetWithinFile: Int64;
  srcUserPassword: PasswordString; srcSaltLength: Integer;  // In bits
  srcKeyIterations: Integer; keyFilename: String; keyfileUserPassword: Ansistring;
  keyfileSaltBytes: TSDUBytes; keyfileKeyIterations: Integer;
  keyfileRequestedDrive: ansichar): Boolean;
var
  volumeDetails:        TVolumeDetailsBlock;
  CDBMetaData:          TCDBMetaData;
  allOK:                Boolean;
  userCancel:           Boolean;
  keyfileRandomPadData: Ansistring;  // Note: If insufficient is supplied, the write will *fail*
                                     //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
begin
  allOK := False;
  GetRandPool.GenerateRandomData(CRITICAL_DATA_LENGTH div 8, keyfileRandomPadData);


  if ReadVolumeCriticalData(srcFilename, srcOffsetWithinFile, srcUserPassword,
    srcSaltLength,  // In bits
    srcKeyIterations, volumeDetails, CDBMetaData) then begin
    volumeDetails.RequestedDriveLetter := keyfileRequestedDrive;

    // Just create a dummy file which will be populated with the keyfile's
    // contents (the driver doesn't create files, it just writes data to them)
    if (not (FileExists(keyFilename))) then begin
      SDUCreateLargeFile(keyFilename, (CRITICAL_DATA_LENGTH div 8), False, userCancel);
    end;

    allOK := WriteVolumeCriticalData(keyFilename, 0, keyfileUserPassword,
      keyfileSaltBytes, keyfileKeyIterations, volumeDetails,
      CDBMetaData{, keyfileRandomPadData});
  end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ChangeVolumePassword(filename: String; offsetWithinFile: Int64;
  oldUserPassword: Ansistring; oldSaltLength: Integer;  // In bits
  oldKeyIterations: Integer; newUserPassword: Ansistring; newSaltBytes: TSDUBytes;
  newKeyIterations: Integer; newRequestedDriveLetter: ansichar

  //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
  ): Boolean;
var
  volumeDetails:    TVolumeDetailsBlock;
  CDBMetaData:      TCDBMetaData;
  allOK:            Boolean;
  newRandomPadData: Ansistring; // Note: If insufficient is supplied, the write will *fail*
                                //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
begin
  //todo: better to get random data in WriteVolumeCriticalData
  allOK := GetRandPool.GenerateRandomData(CRITICAL_DATA_LENGTH, newRandomPadData);

  if ReadVolumeCriticalData(filename, offsetWithinFile, oldUserPassword,
    oldSaltLength,  // In bits
    oldKeyIterations, volumeDetails, CDBMetaData) then begin
    volumeDetails.RequestedDriveLetter := newRequestedDriveLetter;

    allOK := WriteVolumeCriticalData(filename, offsetWithinFile, newUserPassword,
      newSaltBytes, newKeyIterations, volumeDetails, CDBMetaData{, newRandomPadData});
  end;

  Result := allOK;
end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.DumpCriticalDataToFile(volFilename: String;
  offsetWithinFile: Int64; userPassword: Ansistring; saltLength: Integer;  // In bits
  keyIterations: Integer; dumpFilename: String): Boolean;
var
  volumeDetails:        TVolumeDetailsBlock;
  CDBMetaData:          TCDBMetaData;
  allOK:                Boolean;
  dumpReport:           TStringList;
  prettyPrintData:      TStringList;
  criticalDataBuffer:   Ansistring;
  hashTitle:            String;
  cypherTitle:          String;
  hashDetails:          TFreeOTFEHash;
  cypherDetails:        TFreeOTFECypher_v3;
  readTimeStart:        TDateTime;
  readTimeStop:         TDateTime;
  readTimeDiff:         TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
  allOK         := False;

  CheckActive();

  prettyPrintData := TStringList.Create();
  try
    fdumpFlag := True;

    readTimeStart := Now();

    // Read in the raw CDB to display the encrypted version, and then read it in
    // again to get the decrypt it
    if (ReadRawVolumeCriticalData(volFilename, offsetWithinFile, criticalDataBuffer) and
      ReadVolumeCriticalData(volFilename, offsetWithinFile, userPassword,
      saltLength,  // In bits
      keyIterations, volumeDetails, CDBMetaData)) then begin
      readTimeStop := Now();

      // Generate the report
      dumpReport := TStringList.Create();
      try
        AddStdDumpHeader(dumpReport, _('Critical Data Block Dump'));

        AddStdDumpSection(dumpReport, _('User Supplied Information'));
        dumpReport.Add(SDUParamSubstitute(_('Filename (user mode)  : %1'), [volFilename]));
        dumpReport.Add(SDUParamSubstitute(_('Filename (kernel mode): %1'),
          [GetKernelModeVolumeFilename(volFilename)]));
        dumpReport.Add(_('Password              : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length: %1 bits'),
          [(Length(userPassword) * 8)]));
        dumpReport.Add('  ' + _('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          userPassword,
          0,
          Length(userPassword),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(SDUParamSubstitute(_('Offset                : %1 bytes'),
          [offsetWithinFile]));
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
        dumpReport.Add(SDUParamSubstitute(
          _('Time to process CDB   : %1 hours, %2 mins, %3.%4 secs'), [Hour, Min, Sec, MSec]));


        AddStdDumpSection(dumpReport, _('Autodetermined Information'));

        hashTitle := _('ERROR: Unable to determine hash title?!');
        if GetSpecificHashDetails(CDBMetaData.HashDriver, CDBMetaData.HashGUID,
          hashDetails) then begin
          hashTitle := GetHashDisplayTechTitle(hashDetails);
        end;

        cypherTitle := _('ERROR: Unable to determine cypher title?!');
        if GetSpecificCypherDetails(CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
          cypherDetails) then begin
          cypherTitle := GetCypherDisplayTechTitle(cypherDetails);
        end;

        dumpReport.Add(SDUParamSubstitute(_('Hash pretty title        : %1'), [hashTitle]));
        dumpReport.Add(SDUParamSubstitute(_('Hash driver lib/KM name  : %1'),
          [CDBMetaData.HashDriver]));
        dumpReport.Add(SDUParamSubstitute(_('Hash GUID                : %1'),
          [GUIDToString(CDBMetaData.HashGUID)]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher pretty title      : %1'), [cypherTitle]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher driver lib/KM name: %1'),
          [CDBMetaData.CypherDriver]));
        dumpReport.Add(SDUParamSubstitute(_('Cypher GUID              : %1'),
          [GUIDToString(CDBMetaData.CypherGUID)]));
        dumpReport.Add(_('Critical data key     : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('KDF   : %1'),
          [FreeOTFEKDFTitle(CDBMetaData.KDFAlgorithm)]));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length: %1 bits'),
          [(Length(fdumpCriticalDataKey) * 8)]));
        dumpReport.Add('  ' + _('Key   : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          fdumpCriticalDataKey,
          0,
          Length(fdumpCriticalDataKey),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Plaintext encrypted block: '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length: %1 bits'),
          [(Length(fdumpPlaintextEncryptedBlock) * 8)]));
        dumpReport.Add('  ' + _('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          fdumpPlaintextEncryptedBlock,
          0,
          Length(fdumpPlaintextEncryptedBlock),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Check MAC             : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('MAC algorithm: %1'),
          [FreeOTFEMACTitle(CDBMetaData.MACAlgorithm)]));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length       : %1 bits'),
          [(Length(fdumpCheckMAC) * 8)]));
        dumpReport.Add('  ' + _('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          fdumpCheckMAC,
          0,
          Length(fdumpCheckMAC),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Volume details block  : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length       : %1 bits'),
          [(Length(fdumpVolumeDetailsBlock) * 8)]));
        dumpReport.Add('  ' + _('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          fdumpVolumeDetailsBlock,
          0,
          Length(fdumpVolumeDetailsBlock),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);


        AddStdDumpSection(dumpReport, _('Volume Details Block'));
        dumpReport.Add(SDUParamSubstitute(_('CDB format ID         : %1'),
          [volumeDetails.CDBFormatID]));

        // Decode the VolumeFlags to human-readable format
        dumpReport.Add(SDUParamSubstitute(_('Volume flags          : %1'),
          [volumeDetails.VolumeFlags]));
        dumpReport.Add(_('                        ') + DumpCriticalDataToFileBitmap(
          volumeDetails.VolumeFlags, VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART,
          _('Sector ID zero is at the start of the encrypted data'), _(
          'Sector ID zero is at the start of the host file/partition')));

        dumpReport.Add(SDUParamSubstitute(_('Partition length      : %1 bytes'),
          [volumeDetails.PartitionLen]));

        dumpReport.Add(_('Master key            : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length: %1 bits'),
          [volumeDetails.MasterKeyLength]));
        dumpReport.Add('  ' + _('Key   :'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          volumeDetails.MasterKey,
          0,
          (volumeDetails.MasterKeyLength div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(SDUParamSubstitute(_('Sector IV generation  : %1'),
          [FreeOTFESectorIVGenMethodTitle[volumeDetails.SectorIVGenMethod]]));
        dumpReport.Add(_('Volume IV             : '));
        dumpReport.Add('  ' + SDUParamSubstitute(_('Length : %1 bits'),
          [volumeDetails.VolumeIVLength]));
        dumpReport.Add('  ' + _('IV data:'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          volumeDetails.VolumeIV,
          0,
          (volumeDetails.VolumeIVLength div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);

        if (volumeDetails.RequestedDriveLetter = #0) then begin
          dumpReport.Add(_('Requested drive letter: None; use default'));
        end else begin
          dumpReport.Add(SDUParamSubstitute(_('Requested drive letter: %1'),
            [volumeDetails.RequestedDriveLetter + ':']));
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
        allOK := True;
      finally
        fdumpFlag                    := False;
        fdumpCriticalDataKey         := '';
        fdumpCheckMAC                := '';
        fdumpPlaintextEncryptedBlock := '';
        fdumpVolumeDetailsBlock      := '';

        dumpReport.Clear();
        dumpReport.Free();
      end;

    end;

  finally
    prettyPrintData.Free();
  end;

  if (allOK) then begin
    LastErrorCode := OTFE_ERR_SUCCESS;
  end;

  Result := allOK;
end;


 // ----------------------------------------------------------------------------
 // Return a nicely formatted string, decoding one bit of a bitmapped value
 // bit - The zero-offset bit (i.e. the LSB is bit 0)
function TOTFEFreeOTFEBase.DumpCriticalDataToFileBitmap(Value: DWORD;
  bit: Integer; unsetMeaning: String; setMeaning: String): String;
var
  x:          DWORD;
  i:          Integer;
  retVal:     String;
  oneZero:    Integer;
  useMeaning: String;
begin
  retVal := '';

  x := 1;
  for i := 1 to bit do begin
    x := x * 2;
  end;

  oneZero    := 0;
  useMeaning := unsetMeaning;
  if ((Value and x) = x) then begin
    oneZero    := 1;
    useMeaning := setMeaning;
  end;

  retVal := SDUParamSubstitute(_('Bit %1: %2 (%3)'), [bit, oneZero, useMeaning]);

  Result := retval;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WizardChangePassword(): Boolean;
var
  dlg:   TfrmWizardChangePasswordCreateKeyfile;
  allOK: Boolean;
begin
  allOK := False;

  if WarnIfNoHashOrCypherDrivers() then begin
    dlg := TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.ChangePasswordCreateKeyfile := opChangePassword;
      dlg.fFreeOTFEObj                := self;
      allOK                           := (dlg.ShowModal() = mrOk);
    finally
      dlg.Free();
    end;

  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WizardCreateKeyfile(): Boolean;
var
  dlg: TfrmWizardChangePasswordCreateKeyfile;
begin
  Result := False;

  if WarnIfNoHashOrCypherDrivers() then begin
    dlg := TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.ChangePasswordCreateKeyfile := opCreateKeyfile;
      dlg.fFreeOTFEObj                := self;
      Result                          := (dlg.ShowModal() = mrOk);
    finally
      dlg.Free();
    end;

  end;
end;


 // ----------------------------------------------------------------------------
 // Convert a kernel mode volume filename to a user mode volume filename
function TOTFEFreeOTFEBase.GetUserModeVolumeFilename(kernelModeFilename: String): String;
var
  retVal: String;
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
  if Pos('\??\UNC\', retVal) = 1 then begin
    // \\server\share\path\filedisk.img
    Delete(retVal, 1, length('\??\UNC\'));
    retVal := '\\' + retVal;
  end else
  if Pos('\??\', retVal) = 1 then begin
    // C:\path\filedisk.img
    Delete(retVal, 1, length('\??\'));
  end else begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    // Do nothing.
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Convert a user mode volume filename to a kernel mode volume filename
function TOTFEFreeOTFEBase.GetKernelModeVolumeFilename(userModeFilename: String): String;
var
  retVal: String;
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


  if (Pos('\\', userModeFilename) = 1) then begin
    // \\server\share\path\filedisk.img
    // Remove first "\"
    Delete(userModeFilename, 1, 1);
    retVal := '\??\UNC' + userModeFilename;
  end else
  if ((Pos(':\', userModeFilename) = 2) or (Pos(':/', userModeFilename) = 2)) then begin
    // C:\path\filedisk.img
    retVal := '\??\' + userModeFilename;
  end else begin
    // \Device\Harddisk0\Partition1\path\filedisk.img
    retVal := userModeFilename;
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Return TRUE/FALSE, depending on whether the specified KERNEL MODE volume
 // filename refers to a partition/file
function TOTFEFreeOTFEBase.IsPartition_UserModeName(userModeFilename: String): Boolean;
begin
  // In user mode, partitions start with "\\.\"
  Result := (Pos('\\.\', userModeFilename) = 1);

end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to read a critical data block from the named file
 // starting from the specified offset
 // criticalData - This will be set to the string representation of the raw
 //                (encrypted) critical data block
function TOTFEFreeOTFEBase.ReadRawVolumeCriticalData(filename: String;
  offsetWithinFile: Int64; var criticalData: Ansistring): Boolean;
begin
  Result := ReadRawVolumeData(filename, offsetWithinFile, (CRITICAL_DATA_LENGTH div 8),
    criticalData);

end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to read data from the named file
 // starting from the specified offset
 // data - This will be set to the string representation of the raw data read
function TOTFEFreeOTFEBase.ReadRawVolumeData(filename: String; offsetWithinFile: Int64;
  dataLength: DWORD;
  // In bytes
  var Data: Ansistring): Boolean;
var
  retVal: Boolean;
begin
  retVal := ReadRawVolumeDataSimple(filename, offsetWithinFile, dataLength, data);
  if not (retVal) then begin
    retVal := ReadRawVolumeDataBounded(filename, offsetWithinFile, dataLength, data);
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadRawVolumeDataBounded(filename: String;
  offsetWithinFile: Int64; dataLength: DWORD;
  // In bytes
  var Data: Ansistring): Boolean;
var
  preData:  DWORD;
  postData: DWORD;
  tmpData:  Ansistring;
  retVal:   Boolean;
begin
  preData  := (offsetWithinFile mod ASSUMED_HOST_SECTOR_SIZE);
  // Yes, this is correct.
  // We use preData to avoid the need to add offsetWithinFile (a 64 bit value)
  // and then do the mod
  postData := ASSUMED_HOST_SECTOR_SIZE - ((preData + dataLength) mod ASSUMED_HOST_SECTOR_SIZE);

  retVal := ReadRawVolumeDataSimple(filename, (offsetWithinFile - preData),
    (preData + dataLength + postData), tmpData);
  if retVal then begin
    // Trim off the pre/post data garbage
    data := Copy(tmpData, preData, dataLength);

    // Overwrite...
    tmpData := StringOfChar(AnsiChar(#0), (preData + dataLength + postData));
  end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadRawVolumeDataSimple(filename: String;
  offsetWithinFile: Int64; dataLength: DWORD;
  // In bytes
  var Data: Ansistring): Boolean;
var
  fStream: TFileStream;
  retVal:  Boolean;
begin
  retVal := False;

  CheckActive();

  // Set data so that it has enough characters which can be
  // overwritten with StrMove
  data := StringOfChar(AnsiChar(#0), dataLength);

  try
    fStream := TFileStream.Create(filename, fmOpenRead);
    try
      fStream.Position := offsetWithinFile;
      fStream.Read(data[1], dataLength);
      retval := True;
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
                         var ansidata: ansistring
                        ): boolean;
begin
  Result := ReadRawVolumeData(
                         filename,
                         offsetWithinFile,
                         dataLength,
                         ansidata
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
                    volumeKey: TSDUBytes;
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
                    var data: ansistring;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean;
begin
  Result := ReadWritePlaintextToVolume(
                    readNotWrite,

                    volFilename,
                   SDUBytesToString( volumeKey),
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
function TOTFEFreeOTFEBase.WriteRawVolumeCriticalData(filename: String;
  offsetWithinFile: Int64; criticalData: Ansistring): Boolean;
begin
  if (length(criticalData) <> (CRITICAL_DATA_LENGTH div 8)) then begin
    EFreeOTFEInvalidParameter.Create('Invalid CDB passed in for raw writing');
  end;

  Result := WriteRawVolumeData(filename, offsetWithinFile, criticalData);
end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to write data from the named file
 // starting from the specified offset
 // data - This will be set to the string representation of the raw data read
function TOTFEFreeOTFEBase.WriteRawVolumeData(filename: String; offsetWithinFile: Int64;
  data: Ansistring): Boolean;
var
  retVal: Boolean;
begin
  retVal := WriteRawVolumeDataSimple(filename, offsetWithinFile, data);
  if not (retVal) then begin
    retVal := WriteRawVolumeDataBounded(filename, offsetWithinFile, data);
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to write a critical data block from the named file
 // starting from the specified offset
 // criticalData - This should be set to the string representation of a raw
 //                (encrypted) critical data block
function TOTFEFreeOTFEBase.WriteRawVolumeDataSimple(filename: String;
  offsetWithinFile: Int64; data: Ansistring): Boolean;
var
  fStream:      TFileStream;
  retVal:       Boolean;
  bytesWritten: Integer;
begin
  retVal := False;

  CheckActive();

  try
    fStream := TFileStream.Create(filename, fmOpenReadWrite);
    try
      fStream.Position := offsetWithinFile;
      bytesWritten     := fStream.Write(data[1], length(data));
      retval           := (bytesWritten = length(data));
    finally
      fStream.Free();
    end;
  except
    // Just swallow exception; retval already set to FALSE
  end;

  Result := retVal;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.WriteRawVolumeDataBounded(filename: String;
  offsetWithinFile: Int64; data: Ansistring): Boolean;
begin
  // Bounded asjust not yet implemented - MORE COMPLEX THAN FIRST LOOKS
  //  - NEED TO ***READ*** THE PRE/POST DATA, THEN REWRITE IT BACK AGAIN 
  Result := WriteRawVolumeDataSimple(filename, offsetWithinFile, data);
end;


 // ----------------------------------------------------------------------------
 // Display dialog to allow user to select a partition
 // Returns '' if the user cancels/on error, otherwise returns a partition
 // identifier
function TOTFEFreeOTFEBase.SelectPartition(): String;
var
  dlg:    TfrmSelectPartition;
  retVal: String;
begin
  retVal := '';

  dlg := TfrmSelectPartition.Create(nil);
  try
    dlg.OTFEFreeOTFE := self;
    if (dlg.ShowModal() = mrOk) then begin
      retVal := dlg.Partition;
    end;

  finally
    dlg.Free();
  end;

  Result := retVal;
end;


 // ----------------------------------------------------------------------------
 // Generate a list of HDDs and partitions
function TOTFEFreeOTFEBase.HDDNumbersList(var DiskNumbers: TSDUArrayInteger): Boolean;
var
  disk:       Integer;
  junk:       Ansistring;
  currDevice: String;
  retVal:     Boolean;
  prevCursor: TCursor;
begin
  retVal := True;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    for disk := 0 to MAX_HDDS do begin
      // Check to see if the entire disk exists, before checking for partitions
      // 0 - Entire disk
      currDevice := SDUDeviceNameForPartition(disk, 0);
      if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
        SetLength(DiskNumbers, (length(DiskNumbers) + 1));
        DiskNumbers[(length(DiskNumbers) - 1)] := disk;
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
function TOTFEFreeOTFEBase.HDDDeviceList(hddDevices: TStringList; title: TStringList): Boolean;
var
  diskNo:     Integer;
  retVal:     Boolean;
  prevCursor: TCursor;
begin
  retVal := True;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for diskNo := 0 to MAX_HDDS do begin
      HDDDeviceList(diskNo, hddDevices, title);
    end;

  finally
    Screen.Cursor := prevCursor;
  end;

  Result := retVal;
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.HDDDeviceList(diskNo: Integer; hddDevices: TStringList;
  title: TStringList): Boolean;
var
  partition:  Integer;
  junk:       Ansistring;
  currDevice: String;
  retVal:     Boolean;
  prevCursor: TCursor;
begin
  retVal := False;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    // Check to see if the entire disk exists, before checking for partitions
    currDevice := SDUDeviceNameForPartition(diskNo, 0);
    if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
      retVal := True;

      // The entire disk
      hddDevices.AddObject(currDevice, TObject(0));
      title.Add(SDUParamSubstitute(_('Hard disk #%1 (entire disk)'), [diskNo]));

      // Note: We start from 1; partition 0 indicates the entire disk
      for partition := 1 to MAX_PARTITIONS do begin
        currDevice := SDUDeviceNameForPartition(diskNo, partition);
        if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
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
function TOTFEFreeOTFEBase.CDROMDeviceList(cdromDevices: TStringList; title: TStringList): Boolean;
var
  cdrom:      Integer;
  junk:       Ansistring;
  currDevice: String;
  retVal:     Boolean;
  prevCursor: TCursor;
begin
  retVal := True;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for cdrom := 0 to MAX_CDROMS do begin
      // Check to see if the entire disk exists, before checking for partitions
      currDevice := SDUDeviceNameForCDROM(cdrom);
      if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
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

  fCachedHashDriverDetails   := TStringList.Create();
  fCachedCypherDriverDetails := TStringList.Create();
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesDestroy();
var
  i: Integer;
begin
  fCachedVersionID := VERSION_ID_NOT_CACHED;

  for i := 0 to (fCachedHashDriverDetails.Count - 1) do begin
    if (fCachedHashDriverDetails.Objects[i] <> nil) then begin
      Dispose(PFreeOTFEHashDriver(fCachedHashDriverDetails.Objects[i]));
    end;
  end;
  fCachedHashDriverDetails.Free();

  for i := 0 to (fCachedCypherDriverDetails.Count - 1) do begin
    if (fCachedCypherDriverDetails.Objects[i] <> nil) then begin
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
procedure TOTFEFreeOTFEBase.CachesAddHashDriver(hashDriver: String;
  hashDriverDetails: TFreeOTFEHashDriver);
var
  ptrRec: PFreeOTFEHashDriver;
begin
  new(ptrRec);
  ptrRec^ := hashDriverDetails;
  fCachedHashDriverDetails.AddObject(hashDriver, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CachesGetHashDriver(hashDriver: String;
  var hashDriverDetails: TFreeOTFEHashDriver): Boolean;
var
  ptrRec:   PFreeOTFEHashDriver;
  idx:      Integer;
  cacheHit: Boolean;
begin
  cacheHit := False;

  idx := fCachedHashDriverDetails.IndexOf(hashDriver);
  if (idx >= 0) then begin
    ptrRec            := PFreeOTFEHashDriver(fCachedHashDriverDetails.Objects[idx]);
    hashDriverDetails := ptrRec^;
    cacheHit          := True;
  end;

  Result := cacheHit;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.CachesAddCypherDriver(cypherDriver: Ansistring;
  cypherDriverDetails: TFreeOTFECypherDriver);
var
  ptrRec: PFreeOTFECypherDriver;
begin
  new(ptrRec);
  ptrRec^ := cypherDriverDetails;
  fCachedCypherDriverDetails.AddObject(cypherDriver, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.CachesGetCypherDriver(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  ptrRec:   PFreeOTFECypherDriver;
  idx:      Integer;
  cacheHit: Boolean;
begin
  cacheHit := False;

  idx := fCachedCypherDriverDetails.IndexOf(cypherDriver);
  if (idx >= 0) then begin
    ptrRec              := PFreeOTFECypherDriver(fCachedCypherDriverDetails.Objects[idx]);
    cypherDriverDetails := ptrRec^;
    cacheHit            := True;
  end;

  Result := cacheHit;
end;


 // ----------------------------------------------------------------------------
 // Check to see if there's at least one cypher and one hash algorithm
 // available for use.
 // Warn user and return FALSE if not.
 // Returns TRUE if no problems
function TOTFEFreeOTFEBase.WarnIfNoHashOrCypherDrivers(): Boolean;
var
  displayTitles:         TStringList;
  kernelModeDriverNames: TStringList;
  hashGUIDs:             TStringList;
  hashCount:             Integer;
  cypherCount:           Integer;
begin
  hashCount   := 0;
  cypherCount := 0;

  displayTitles := TStringList.Create();
  try
    kernelModeDriverNames := TStringList.Create();
    try
      hashGUIDs := TStringList.Create();
      try

        if (GetHashList(displayTitles, kernelModeDriverNames, hashGUIDs)) then begin
          hashCount := displayTitles.Count;
        end;

        if (hashCount = 0) then begin
          SDUMessageDlg(
            _('You do not appear to have any FreeOTFE hash drivers installed and started.') +
            SDUCRLF + SDUCRLF + _(
            'If you have only just installed FreeOTFE, you may need to restart your computer.'),
            mtError
            );
        end;


        if (GetCypherList(displayTitles, kernelModeDriverNames, hashGUIDs)) then begin
          cypherCount := displayTitles.Count;
        end;

        if (cypherCount = 0) then begin
          SDUMessageDlg(
            _('You do not appear to have any FreeOTFE cypher drivers installed and started.') +
            SDUCRLF + SDUCRLF + _(
            'If you have only just installed FreeOTFE, you may need to restart your computer.'),
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

  Result := (hashCount > 0) and (cypherCount > 0);
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.AddStdDumpHeader(content: TStringList; title: String);
var
  driverVersion:      String;
  platformID:         String;
  envARCHITECTURE:    String;
  envARCH_W3264:      String;
  useEnvARCHITECTURE: String;
  useEnvARCH_W3264:   String;
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
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCHITECTURE, envARCHITECTURE) then begin
    useEnvARCHITECTURE := envARCHITECTURE;
  end;
  useEnvARCH_W3264 := '---';
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCH_W3264, envARCH_W3264) then begin
    useEnvARCH_W3264 := envARCH_W3264;
  end;
  platformID := 'PC ' + DriverType() + ' (' + INSTALLED_OS_TITLE[SDUInstalledOS()] +
    '; ' + IntToStr(SDUOSCPUSize()) + ' bit [' + useEnvARCHITECTURE + '/' +
    useEnvARCH_W3264 + ']' + ')';
  content.Add(SDUParamSubstitute(_('Platform              : %1'), [platformID]));
  content.Add(SDUParamSubstitute(_('Application version   : %1'),
    ['v' + SDUGetVersionInfoString(ParamStr(0))]));
  content.Add(SDUParamSubstitute(_('Driver ID             : %1'), [driverVersion]));
  //  content.Add(''); // Newlines not needed - added by following section header
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.AddStdDumpSection(content: TStringList; sectionTitle: String);
begin
  content.Add('');
  content.Add('');
  content.Add(sectionTitle);
  content.Add(StringOfChar('-', length(sectionTitle)));
end;


// ----------------------------------------------------------------------------

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------

end.
