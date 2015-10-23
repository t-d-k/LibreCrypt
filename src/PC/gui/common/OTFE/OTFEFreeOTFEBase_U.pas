unit OTFEFreeOTFEBase_U;
 // Description: Delphi FreeOTFE Component
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //
{
TOTFEFreeOTFEBase is a wrapper round the driver and also does all drive operations,
enc data, create new vols, etc
TOTFEFreeOTFEBase is a singleton class
set up instance by calling SetFreeOTFEType then get instance by calling GetFreeOTFEBase

}

interface

uses
 //delphi / libs
   Classes, Controls, Dialogs,
  Forms,
  Graphics, Messages,
  SysUtils, Windows,
  //sdu & LibreCrypt utils
      pkcs11_library,
  pkcs11_object,
  pkcs11_session,  lcTypes, OTFE_U,
  OTFEConsts_U,
  DriverAPI,
  FreeOTFEDriverConsts,
  FreeOTFECypherDriverAPI,
  FreeOTFEHashDriverAPI,
  OTFEFreeOTFE_LUKSAPI,
  PKCS11Lib,
  VolumeFileAPI
   // LibreCrypt forms
 ;

const

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

  VOL_FILE_EXTN = 'vol';

  // This const is used in rounding up DIOC buffers to the nearest DWORD
  DIOC_BOUNDRY = 4;

  FREEOTFE_URL = 'http://LibreCrypt.eu/';

  LINUX_KEYFILE_DEFAULT_IS_ASCII = False;
  LINUX_KEYFILE_DEFAULT_NEWLINE  = nlLF;

resourcestring
  // Open/Save file filters...
  FILE_FILTER_FLT_VOLUMES  = 'Container files (*.vol;*.box)|*.vol;*.box|All files|*.*';
  FILE_FILTER_DFLT_VOLUMES = 'vol';

  FILE_FILTER_FLT_KEYFILES  = 'Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_KEYFILES = 'cdb';

  FILE_FILTER_FLT_VOLUMESANDKEYFILES  =
    'Container files and keyfiles (*.vol;*.cdb;*.box)|*.vol;*.box;*.cdb|Container files (*.vol;*.box)|*.vol;*.box|Keyfiles (*.cdb)|*.cdb|All files|*.*';
  FILE_FILTER_DFLT_VOLUMESANDKEYFILES = '';

  FILE_FILTER_FLT_TEXTFILES  = 'Text files (*.txt)|*.txt|All files|*.*';
  FILE_FILTER_DFLT_TEXTFILES = 'txt';

  FILE_FILTER_FLT_CDBBACKUPS  = 'Header backups (*.cdbBackup)|*.cdbBackup|All files|*.*';
  FILE_FILTER_DFLT_CDBBACKUPS = 'cdbBackup';

  FILE_FILTER_FLT_EXECUTABLES  = 'Executable files (*.exe)|*.exe|All files|*.*';
  FILE_FILTER_DFLT_EXECUTABLES = 'exe';

  FILE_FILTER_DFLT_LINUX_SETTINGS = 'les';
  FILE_FILTER_FLT_LINUX_SETTINGS  = 'Linux encryption settings (*.les)|*.les|All files|*.*';

  COUNT_BITS = '%d bits';

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
  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
  // that only this unit need be "used" by user applications
  TFreeOTFECypherMode = (focmNone, focmECB, focmCBC, focmLRW, focmXTS, focmUnknown);



const
  // These consts are used to supply a dummy sector ID/size to the sector
  // encrypt/decrypt routines, where there is none/it's inconseqeuential
  FREEOTFE_v1_DUMMY_SECTOR_ID: LARGE_INTEGER = (QuadPart: 0);
  FREEOTFE_v1_DUMMY_SECTOR_SIZE = 512;

  DEFAULT_MOUNTAS = fomaRemovableDisk;

resourcestring
  MOUNT_AS_FIXED_DISK = 'Fixed disk';
  MOUNT_AS_REMOVABLE_DISK = 'Removable disk';
  MOUNT_AS_CD  = 'CD';
  MOUNT_AS_DVD = 'DVD';

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
  FreeOTFEMountAsTitlePtr: array [TMountDiskType] of Pointer =
    (@MOUNT_AS_FIXED_DISK, @MOUNT_AS_REMOVABLE_DISK, @MOUNT_AS_CD, @MOUNT_AS_DVD, @RS_UNKNOWN
    );

  // Indicate which of the above emulated devices are writable
  FreeOTFEMountAsCanWrite: array [TMountDiskType] of Boolean = (
    True,
    True,
    False,
    False,
    False
    );

  // Decode the above into device types/media types
  FreeOTFEMountAsDeviceType: array [TMountDiskType] of DWORD = (
    FILE_DEVICE_DISK,
    FILE_DEVICE_DISK,
    FILE_DEVICE_CD_ROM,
    FILE_DEVICE_DVD,
    FILE_DEVICE_UNKNOWN
    );

  // Decode the above into device types/media types
  FreeOTFEMountAsStorageMediaType: array [TMountDiskType] of TFreeOTFEStorageMediaType = (
    mtFixedMedia,
    mtRemovableMedia,
    mtCD_ROM,
    mtDVD_ROM,
    mtUnknown
    );

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFECypherModeTitlePtr: array [TFreeOTFECypherMode] of Pointer =
    (@CYPHER_MODE_TITLE_NONE, @CYPHER_MODE_TITLE_ECB, @CYPHER_MODE_TITLE_CBC,
    @CYPHER_MODE_TITLE_LRW, @CYPHER_MODE_TITLE_XTS, @RS_UNKNOWN
    );

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
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

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEMACTitlePtr: array [TFreeOTFEMACAlgorithm] of Pointer =
    (@MAC_TITLE_HASH, @MAC_TITLE_HMAC, @RS_UNKNOWN
    );

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
  // that only this unit need be "used" by user applications
  // These are the numerical IDs that are to be passed to/from the FreeOTFE
  // encryption drivers
  FreeOTFEMACID: array [TFreeOTFEMACAlgorithm] of Integer = (
    1,     // MAC_HASH
    2,     // MAC_HMAC
    9999   // MAC_UNKNOWN
    );

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
  // that only this unit need be "used" by user applications
  FreeOTFEKDFTitlePtr: array [TFreeOTFEKDFAlgorithm] of Pointer =
    (@KDF_HASH_WITH_SALT, @KDF_PKCS5PBKDF2, @RS_UNKNOWN
    );

  // This definition is stored here, and not in FreeOTFECypherDriverAPI, so
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
  { TODO -otdk -crefactor : does this need to be packed - is ever passed to drivers? }
  { TODO -otdk -crefactor : change to unicode }
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

    MetaData:       Ansistring;
    MetaDataStructValid: Boolean;
    MetaDataStruct: TOTFEFreeOTFEVolumeMetaData;
  end;

type
  //  TFreeOTFECypher_v1 = packed record
  //    CypherGUID: TGUID;
  //
  //    Title:     Ansistring;
  //    Mode:      TFreeOTFECypherMode;
  //    KeySizeUnderlying: Integer;  // In bits
  //    BlockSize: Integer;          // In bits
  //    VersionID: DWORD;
  //  end;

  TFreeOTFECypher_v3 = packed record
    CypherGUID: TGUID;

    Title:     Ansistring;
    Mode:      TFreeOTFECypherMode;
    KeySizeUnderlying: Integer;  // In bits
    BlockSize: Integer;          // In bits
    VersionID: DWORD;
    KeySizeRequired: Integer;  // In bits
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

  {
TOTFEFreeOTFEBase is a wrapper round the driver and also does all drive operations,
enc data, create new vols, etc
TOTFEFreeOTFEBase is a singleton class
set up instance by calling SetFreeOTFEType then get instance by calling GetFreeOTFEBase
}
  TOTFEFreeOTFEBase = class (TOTFE)
  private


  protected

    fcachedVersionID:      DWORD;

    fPKCS11Library: TPKCS11Library;

    // Cached information: The strings in the list are kernel mode device
    // names, the objects are TFreeOTFEHashDriver/TFreeOTFECypherDriver
    fCachedHashDriverDetails:   TStringList;
    fCachedCypherDriverDetails: TStringList;


//    frevertVolTimestamps: Boolean;


    // Set the component active/inactive
    procedure SetActive(status: Boolean); override;

    // Connect/disconnect to the main FreeOTFE device driver
    function _Connect(): Boolean; virtual; abstract;
    procedure _Disconnect(); virtual; abstract;

    function GetNextDriveLetter(): Char; overload;

    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations

    // volFilename - This is the filename of the file/partition as seen by the
    //               user mode software
    function _MountDiskDevice(deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      volFilename: String; volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod; volumeIV: TSDUBytes;
      ReadOnly: Boolean; IVHashDriver: Ansistring; IVHashGUID: TGUID;
      IVCypherDriver: Ansistring; IVCypherGUID: TGUID; mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID; VolumeFlags: Integer; metaData: TOTFEFreeOTFEVolumeMetaData;
      offset: Int64 = 0; size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType =
      mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; virtual; abstract;



    function _DismountDiskDevice(
    deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      emergency: Boolean): Boolean;
      virtual; abstract;


    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place



    // This executes a simple call to the driver to read dataLength bytes from
    // offsetWithinFile
    // Note: Will probably not work when reading directly from partitions;
    //       they typically need read/writes carried out in sector sized blocks
    //        - see ReadRawVolumeDataBounded for this.
    function ReadRawVolumeDataSimple(filename: String; offsetWithinFile: Int64;
      dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean; virtual;

    // This function is the same as ReadRawVolumeDataSimple(...), but will round
    // the offsetWithinFile down to the nearest sector offset, read in complete
    // sectors, and *only* *return "datalength" *bytes*
    // This function is included as reading from partitions must be carried out
    // in sector size blocks
    function ReadRawVolumeDataBounded(filename: String; offsetWithinFile: Int64;
      dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean;

    function WriteRawVolumeDataSimple(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean; virtual;

    function WriteRawVolumeDataBounded(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean;



    // ---------
    // Misc functions

    // ---------
    // Convert critical data area to/from a string representation

    // Note: This function does *not* append random padding data
    function BuildVolumeDetailsBlock(volumeDetailsBlock: TVolumeDetailsBlock;
      var stringRep: Ansistring): Boolean;
    // Note: Ignores any random padding data
    function ParseVolumeDetailsBlock(stringRep: Ansistring;
      var volumeDetailsBlock: TVolumeDetailsBlock): Boolean;

    // Convert a user-space volume filename to a format the kernel mode driver
    // can understand
    function GetKernelModeVolumeFilename(userModeFilename: String): String;
    // Convert a kernel mode driver volume filename to format user-space
    // filename
    function GetUserModeVolumeFilename(kernelModeFilename: String): String;

    function MetadataSig(): Ansistring;

    function PopulateVolumeMetadataStruct(LinuxVolume: Boolean; PKCS11SlotID: Integer;
      var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;

    function ParseVolumeMetadata(metadataAsString: Ansistring;
      var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;

    procedure VolumeMetadataToString(metadata: TOTFEFreeOTFEVolumeMetaData;
      var metadataAsString: Ansistring);

    // ---------
    // Internal caching functions
    procedure CachesCreate(); virtual;
    procedure CachesDestroy(); virtual;

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
    function _CachesGetCypherDriver(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;

    // v1 / v3 Cypher API functions
    // Note: This shouldn't be called directly - only via wrapper functions!
    // cypherDriver - Kernel drivers: hashKernelModeDeviceName
    //                DLL drivers:    hashLibFilename
    //    function _GetCypherDriverCyphers_v1(cypherDriver: Ansistring;
    //      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; virtual; abstract;
    function _GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; virtual; abstract;



  public
        { TODO 2 -otdk -crefactor : make into paramters to function }
          // This is a hacky, but the Dump functionality does things that you
    // wouldn't normally do (i.e. it's a high level function that wants to
    // know about the inner workings of lower-level functionality; CDB
    // processing)
    fdumpFlag:     Boolean;
    fdumpCriticalDataKey: TSDUBytes;
    fdumpCheckMAC: String;
    fdumpPlaintextEncryptedBlock: String;
    fdumpVolumeDetailsBlock: String;


    // Returns the type of driver used; either "DLL driver" or "Kernel driver"
    function GetDriverType(): String; virtual; abstract;

    function ReadRawVolumeData(filename: String; offsetWithinFile: Int64; dataLength: DWORD;
    // In bytes
      var Data: Ansistring): Boolean;

    function WriteRawVolumeData(filename: String; offsetWithinFile: Int64;
      data: Ansistring): Boolean;

    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(); override;
    destructor Destroy(); override;

    function CanMountDevice(): Boolean; override;

    function Dismount(volumeFilename: String; emergency: Boolean = False): Boolean;
      overload; override;
    // Although this Dismount(...) is introduced as virtual/abstractin TOTFE,
    // it needs to be shown here as well in order to prevent compiler error
    // ("Ambiguous overloaded call to 'Dismount'") in the above Dismount(...)
    // implementation
    function Dismount(driveLetter: DriveLetterChar; emergency: Boolean = False): Boolean;
      overload; override; abstract;
    function Title(): String; overload; override;
    function VersionStr(): String; overload; override;
    //    function IsEncryptedVolFile(volumeFilename: String): Boolean; override;
    function GetVolFileForDrive(driveLetter: Char): String; override;
    function GetDriveForVolFile(volumeFilename: String): DriveLetterChar; override;
    function GetMainExe(): String; override;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE specific functions

    // Get information on a mounted volume
    function GetVolumeInfo(driveLetter: DriveLetterChar;
      var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
      virtual; abstract;

    // Get details of all hash drivers and the hashes they support
    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean; virtual; abstract;
    // Get details of a single hash driver, and all it's hashes
    // hashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
    function GetHashDriverHashes(hashDriver: String;
      var hashDriverDetails: TFreeOTFEHashDriver): Boolean; virtual; abstract;

    // Get details of all cypher drivers and the cyphers they support
    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean;
      virtual; abstract;
    // Get details of a single cypher driver, and all it's cyphers
    // cypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
    function GetCypherDriverCyphers(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; virtual;

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


    function GetSpecificHashDetails(hashKernelModeDeviceName: String;
      hashGUID: TGUID; var hashDetails: TFreeOTFEHash): Boolean;
    function GetSpecificCypherDetails(cypherKernelModeDeviceName: Ansistring;
      cypherGUID: TGUID; var cypherDetails: TFreeOTFECypher_v3): Boolean;

    function HDDNumbersList(var DiskNumbers: TSDUArrayInteger): Boolean;
    function HDDDeviceList(hddDevices: TStringList; title: TStringList): Boolean; overload;
    function HDDDeviceList(diskNo: Integer; hddDevices: TStringList; title: TStringList): Boolean;
      overload;
    function CDROMDeviceList(cdromDevices: TStringList; title: TStringList): Boolean;


    // Check to see if there's at least one cypher and one hash algorithm
    // available for use.
    // Warn user and return FALSE if not.
    function WarnIfNoHashOrCypherDrivers(): Boolean;

    // Convert a version ID into a prettyprinted version string
    function VersionIDToStr(versionID: DWORD): String;

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
      virtual; abstract;

    function EncryptDecryptSectorData(encryptFlag: Boolean;
    // driver - Kernel drivers: cypherKernelModeDeviceName
    //          DLL drivers:    cypherLibFilename
      cypherDriver: Ansistring; cypherGUID: TGUID; SectorID: LARGE_INTEGER;
      SectorSize: Integer; var key: TSDUBytes; var IV: Ansistring;
      var inData: Ansistring; var outData: Ansistring): Boolean;
      virtual; abstract;

    // Hash data using the hash driver identified
    // Result returned in "hashOut"
    // driver - Kernel drivers: hashKernelModeDeviceName
    //          DLL drivers:    hashLibFilename
    function HashData(hashDriver: Ansistring; hashGUID: TGUID; const data: TSDUBytes;
      out hashOut: TSDUBytes): Boolean; virtual; abstract;

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
      virtual; abstract;


    // Key derivation function
    function DeriveKey(kdfAlgorithm: TFreeOTFEKDFAlgorithm;
    // HashDriver - Kernel drivers: hashKernelModeDeviceName
    //              DLL drivers:    hashLibFilename
      HashDriver: Ansistring; HashGUID: TGUID;
    // CypherDriver - Kernel drivers: cypherKernelModeDeviceName
    //                DLL drivers:    cypherLibFilename
      CypherDriver: Ansistring; CypherGUID: TGUID; Password: TSDUBytes;
      Salt: array of Byte; Iterations: Integer; dkLenBits: Integer;  // In *bits*
      out DK: TSDUBytes): Boolean; virtual; abstract;

    // ---------
    // Volume creation
    function CreateLinuxGPGKeyfile(): Boolean;


    // ---------
    // Critical data block handling
    function ReadVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      userPassword: TSDUBytes; saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean;

    function ReadVolumeCriticalData_CDB(CDB: Ansistring; userPassword: TSDUBytes;
      saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean; overload;
    (*
    function ReadVolumeCriticalData_CDB_v1(CDB: Ansistring; userPassword: TSDUBytes;
      saltLength: Integer;  // In bits
      var volumeDetails: TVolumeDetailsBlock; var CDBMetaData: TCDBMetaData): Boolean;
      overload;
     *)
    function ReadVolumeCriticalData_CDB_v2(CDB: Ansistring; userPassword: TSDUBytes;
      saltLength: Integer;  // In bits
      keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData
        ): Boolean; overload;


   // when multiple valid cphers or hashes are found (eg alternate drivers), prompt user to choose one
   // calls  TfrmSelectHashCypher
    function ChooseFromValid(validVolumeDetails: TVolumeDetailsBlockArray;
      validCDBMetaDatas: TCDBMetaDataArray; var volumeDetails: TVolumeDetailsBlock;
      var CDBMetaData: TCDBMetaData): Boolean;

    // Write out a critical data block
    // If the file specified doesn't already exist, it will be created
    // ALL members of these two "volumeDetails" and "CDBMetaData" structs MUST
    // be populated
    // *Except* for volumeDetails.CDBFormatID - this is force set to the latest value
    function WriteVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      userPassword: TSDUBytes; salt: TSDUBytes; keyIterations: Integer;
      volumeDetails: TVolumeDetailsBlock; CDBMetaData: TCDBMetaData
    // Note: If insufficient is supplied, the write will
    //       *fail*
    //       The maximum that could possibly be required
    //       is CRITICAL_DATA_LENGTH bits.
      ): Boolean; overload;

    function BackupVolumeCriticalData(srcFilename: String; srcOffsetWithinFile: Int64;
      destFilename: String): Boolean;

    function RestoreVolumeCriticalData(srcFilename: String; destFilename: String;
      destOffsetWithinFile: Int64): Boolean;



    function ChangeVolumePassword(filename: String; offsetWithinFile: Int64;
      oldUserPassword: TSDUBytes; oldSaltLength: Integer;  // In bits
      oldKeyIterations: Integer; newUserPassword: TSDUBytes; newSaltBytes: TSDUBytes;
      newKeyIterations: Integer; newRequestedDriveLetter: DriveLetterChar): Boolean;

    function CreateKeyfile(srcFilename: String; srcOffsetWithinFile: Int64;
      srcUserPassword: TSDUBytes; srcSaltLength: Integer;  // In bits
      srcKeyIterations: Integer; keyFilename: String; keyfileUserPassword: TSDUBytes;
      keyfileSaltBytes: TSDUBytes; keyfileKeyIterations: Integer;
      keyfileRequestedDrive: DriveLetterChar): Boolean;

    // ---------
    // Raw volume access functions
    // Note: These all transfer RAW data to/from the volume - no
    //       encryption/decryption takes place

    // Get the FreeOTFE driver to read a critical data block from the named file
    // starting from the specified offset
    function ReadRawVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      var criticalData: Ansistring): Boolean;
      overload;

    // Get the FreeOTFE driver to write a critical data block from the named file
    // starting from the specified offset
    function WriteRawVolumeCriticalData(filename: String; offsetWithinFile: Int64;
      criticalData: Ansistring): Boolean;
      overload;


    // ---------
    // FreeOTFE Driver handling...

    procedure CachesFlush();

    // ---------
    // Debug functions - expose private members in debug builds for testing...


    // Expose ReadRawVolumeData(...) for debug
    function DEBUGReadRawVolumeData(
      filename: String;
      offsetWithinFile: Int64;
      dataLength: DWORD;  // In bytes
      var AnsiData: Ansistring
      ): Boolean;

    // Expose WriteRawVolumeData(...) for debug
    function DEBUGWriteRawVolumeData(
      filename: String;
      offsetWithinFile: Int64;
      data: String
      ): Boolean;

    // Expose ReadWritePlaintextToVolume(...) for debug
    function DEBUGReadWritePlaintextToVolume(
      readNotWrite: Boolean;

      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      IVHashDriver: String;
      IVHashGUID: TGUID;
      IVCypherDriver: String;
      IVCypherGUID: TGUID;
      mainCypherDriver: String;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      mountMountAs: TMountDiskType;

      dataOffset: Int64;
    // Offset from within mounted volume from where to read/write data
      dataLength: Integer;  // Length of data to read/write. In bytes
      var data: TSDUBytes;  // Data to read/write

      offset: Int64 = 0;
      size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
      ): Boolean;

    // Return TRUE/FALSE, depending on whether the specified USER MODE volume
    // filename refers to a partition/file
    function IsPartitionUserModeName(userModeFilename: String): Boolean;


    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Char): Char;
      overload; virtual; abstract;

    function CreateMountDiskDevice(volFilename: String; volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod; volumeIV: TSDUBytes;
      ReadOnly: Boolean; IVHashDriver: Ansistring; IVHashGUID: TGUID;
      IVCypherDriver: Ansistring; IVCypherGUID: TGUID; mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID; VolumeFlags: Integer; DriveLetter: Char;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      offset: Int64 = 0; size: Int64 = 0; MetaData_LinuxVolume: Boolean = False;
    // Linux volume
      MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
      MountMountAs: TMountDiskType = fomaRemovableDisk;
    // PC kernel drivers *only* - ignored otherwise
      mountForAllUsers: Boolean = True  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; virtual; abstract;

    function ReadWritePlaintextToVolume(readNotWrite: Boolean; volFilename: String;
      volumeKey: TSDUBytes; sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      IVHashDriver: Ansistring; IVHashGUID: TGUID; IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID; mainCypherDriver: Ansistring; mainCypherGUID: TGUID;
      VolumeFlags: Integer; mountMountAs: TMountDiskType; dataOffset: Int64;
    // Offset from within mounted volume from where to read/write data
      dataLength: Integer;   // Length of data to read/write. In bytes
      var data: TSDUBytes;  // Data to read/write

      offset: Int64 = 0; size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia): Boolean; virtual; abstract;


  published
//    property PasswordChar: Char Read fPasswordChar Write fPasswordChar;
//    property AllowNewlinesInPasswords: Boolean Read fAllowNewlinesInPasswords
//      Write fAllowNewlinesInPasswords default True;
//    property AllowTabsInPasswords: Boolean Read fAllowTabsInPasswords
//      Write fAllowTabsInPasswords default False;
  { TODO -otdk -crefactor : move PKCS11Library into globals unit }
//property PKCS11Library: TPKCS11Library Read fPKCS11Library Write fPKCS11Library;
  end;


    {
TOTFEFreeOTFEBase is a wrapper round the driver/DLL and also does all drive operations,
enc data, create new vols, etc
TOTFEFreeOTFEBase is a singleton class, only ever one instance - this is enforced by assertion in ctor
set up instance by calling SetFreeOTFEType then get instance by calling GetFreeOTFEBase
}
  TOTFEFreeOTFEBaseClass = class of TOTFEFreeOTFEBase;

{ factory fn creates an instance that is returned by GetFreeOTFEBase}
procedure SetFreeOTFEType(typ: TOTFEFreeOTFEBaseClass);
{returns an instance of type set in SetFreeOTFEType}
function GetFreeOTFEBase: TOTFEFreeOTFEBase;

function FreeOTFEMountAsTitle(mountAs: TMountDiskType): String;
function FreeOTFECypherModeTitle(cypherMode: TFreeOTFECypherMode): String;
function FreeOTFEMACTitle(MACAlgorithm: TFreeOTFEMACAlgorithm): String;
function FreeOTFEKDFTitle(KDFAlgorithm: TFreeOTFEKDFAlgorithm): String;


implementation

uses
 //delphi / libs
  {$IFDEF FREEOTFE_DEBUG}
{$IFDEF _GEXPERTS}
  DbugIntf,  // GExperts
{$ENDIF}
{$ENDIF}

            // Required for min
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for GUIDToString
  WinSvc,   // Required for SERVICE_ACTIVE
  Math,
  //sdu & LibreCrypt utils
    sduGeneral,
  SDUi18n,
  lcDialogs,
  dlgProgress,
  SDURandPool,
  lcDebugLog,
  DriverControl,
  AFSplitMerge,
  SDUEndianIntegers,
    PartitionTools,
lcConsts,
  MainSettings,
  LUKSTools,
   // LibreCrypt forms
  frmSelectHashCypher//used by ChooseFromValid
  { TODO 1 -otdk -crefactor : remove calls to frmSelectHashCypher by ChooseFromValid- perhaps use callback? }
  ;



{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

var
  //single instance of object, get by calling GetFreeOTFEBase. normally is of derived class
  _FreeOTFEObj: TOTFEFreeOTFEBase;

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



 //////////////////////////////////////////////
 // end of OTFEFreeOTFE_mntLUKS_AFS_Impl.pas
 //////////////////////////////////////////////


// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
function FreeOTFEMountAsTitle(mountAs: TMountDiskType): String;
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
constructor TOTFEFreeOTFEBase.Create();
begin
  inherited;
  assert(_FreeOTFEObj = nil, 'Do not call TOTFEFreeOTFEBase.Create directly - only call GetOTFEBase');

  fdumpFlag := False;
  CachesCreate();
end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFEBase.Destroy();
begin
  CachesDestroy();



  inherited;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEBase.SetActive(status: Boolean);
var
  allOK: Boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  allOK         := True;

  // If logging debug to a file, clear to logfile
  DebugFlush();

  // Flush caches
  CachesFlush();

  if (status) then begin
    DebugMsg(
      '-------------------------------------------------------' + SDUCRLF +
      '-------------------------------------------------------' + SDUCRLF +
      'DEBUG BUILD OF TOTFEFreeOTFE component being set ACTIVE' + SDUCRLF +
      '-------------------------------------------------------' + SDUCRLF +
      '-------------------------------------------------------'
      );
  end;

  if (status <> Active) then begin
    if status then begin
      allOK := _Connect();
      if not (allOK) then begin
        raise EFreeOTFEConnectFailure.Create('FreeOTFE driver not installed/not running');
      end;

    end else begin
      _Disconnect();
  //    if not (allOK) then begin
//        raise EFreeOTFEConnectFailure.Create('Could not disconnect from FreeOTFE driver?!');
//      end;

    end;

  end;


  // Note: LastErrorCode will have been set by Connect(...)/Dicconnect(...) on
  //       failure.
  if allOK then begin
    inherited;
  end;


  if (Active) then begin
    // Previous versions which are no longer supported due to changes in the driver API
    if (Version() < FREEOTFE_ID_v02_00_0000) then begin
      Active := False;
      raise EFreeOTFEObsoleteDriver.Create(
        'An old version of the FreeOTFE driver was detected. Please upgrade before using this software'
        );
    end;
  end;
end;




 // ----------------------------------------------------------------------------
 // Generate the metadata signature to be used
 // Returns: Appropriatly sized signature
function TOTFEFreeOTFEBase.MetadataSig(): Ansistring;
var
  tmpSig:    Ansistring;
  tmpStruct: TOTFEFreeOTFEVolumeMetaData;
begin
  // Dummy assignment to get rid of compiler warning; this variable is only
  // used to get the sizeof(...) the signature
  tmpStruct.Version := 1;

  tmpSig := Title();
  tmpSig := tmpSig + StringOfChar(AnsiChar(' '), length(tmpStruct.Signature));

  // Copy(...) indexes from 1
  Result := Copy(tmpSig, 1, length(tmpStruct.Signature));
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
function TOTFEFreeOTFEBase.ParseVolumeMetadata(metadataAsString: Ansistring;
  var metadata: TOTFEFreeOTFEVolumeMetaData): Boolean;
var
  tmpStructPtr: POTFEFreeOTFEVolumeMetaData;
begin
  Result := (Pos(MetadataSig(), metadataAsString) = 1);

  if Result then begin
    Result := (length(metadataAsString) = sizeof(metadata));
    if Result then begin
      tmpStructPtr := POTFEFreeOTFEVolumeMetaData(PAnsiChar(metadataAsString));
      metadata     := tmpStructPtr^;
    end;

  end;
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
function TOTFEFreeOTFEBase.GetNextDriveLetter(): Char;
begin
  Result := GetNextDriveLetter(#0, #0);
end;



// ----------------------------------------------------------------------------





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
  verNo: Cardinal;
begin
  Result := '';

  CheckActive();

  verNo  := Version();
  Result := VersionIDToStr(verNo);
end;


 // ----------------------------------------------------------------------------
 // Convert a version ID into a prettyprinted version string
function TOTFEFreeOTFEBase.VersionIDToStr(versionID: DWORD): String;
var
  majorVer: Integer;
  minorVer: Integer;
  buildVer: Integer;
begin
  Result := '';

  if (versionID <> VERSION_ID_FAILURE) then begin
    majorVer := (versionID and $FF000000) div $00FF0000;
    minorVer := (versionID and $00FF0000) div $0000FF00;
    buildVer := (versionID and $0000FFFF);
    Result   := Format('v%d.%.2d.%.4d', [majorVer, minorVer, buildVer]);
  end;

end;


 //function TOTFEFreeOTFEBase.IsEncryptedVolFile(volumeFilename: String): Boolean;
 //begin
 //  CheckActive();
 //  // We can't tell! Return TRUE...
 //  Result := True;
 //end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetVolFileForDrive(driveLetter: Char): String;
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  Result := '';

  CheckActive();

  if (GetVolumeInfo(upcase(driveLetter), volumeInfo)) then begin
    Result := volumeInfo.Filename;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetDriveForVolFile(volumeFilename: String): DriveLetterChar;
var
  mounted:    String;
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  i:          Integer;
begin
  Result := #0;

  CheckActive();

  mounted := DrivesMounted();
  for i := 1 to length(mounted) do begin
    if GetVolumeInfo(mounted[i], volumeInfo) then begin
      if (uppercase(volumeInfo.Filename) = uppercase(volumeFilename)) then begin
        Result := Char(volumeInfo.DriveLetter);
        break;
      end;

    end;
  end;

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
begin
  Result := _GetCypherDriverCyphers_v3(cypherDriver, cypherDriverDetails);
  //  if not (Result) then begin
  //    Result := _GetCypherDriverCyphers_v1(cypherDriver, cypherDriverDetails);
  //  end;

end;








// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificHashDetails(hashKernelModeDeviceName: String;
  hashGUID: TGUID; var hashDetails: TFreeOTFEHash): Boolean;
var
  i:                 Integer;
  hashDriverDetails: TFreeOTFEHashDriver;
begin
  Result := False;

  if GetHashDriverHashes(hashKernelModeDeviceName, hashDriverDetails) then begin

    DebugMsg('low hashDriverDetails.Hashes: ' + IntToStr(low(hashDriverDetails.Hashes)));
    DebugMsg('high hashDriverDetails.Hashes: ' + IntToStr(high(hashDriverDetails.Hashes)));

    for i := low(hashDriverDetails.Hashes) to high(hashDriverDetails.Hashes) do begin
      if (IsEqualGUID(hashDriverDetails.Hashes[i].HashGUID, hashGUID)) then begin
        hashDetails := hashDriverDetails.Hashes[i];
        Result      := True;
        break;
      end;

    end;

  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetSpecificCypherDetails(cypherKernelModeDeviceName: Ansistring;
  cypherGUID: TGUID; var cypherDetails: TFreeOTFECypher_v3): Boolean;
var
  i:                   Integer;
  cypherDriverDetails: TFreeOTFECypherDriver;
begin
  Result := False;

  if GetCypherDriverCyphers(cypherKernelModeDeviceName, cypherDriverDetails) then begin

    DebugMsg('low cypherDriverDetails.Cyphers: ' + IntToStr(low(cypherDriverDetails.Cyphers)));
    DebugMsg('high cypherDriverDetails.Cyphers: ' + IntToStr(high(cypherDriverDetails.Cyphers)));

    for i := low(cypherDriverDetails.Cyphers) to high(cypherDriverDetails.Cyphers) do begin
      if (IsEqualGUID(cypherDriverDetails.Cyphers[i].CypherGUID, cypherGUID)) then begin
        cypherDetails := cypherDriverDetails.Cyphers[i];
        Result        := True;
        break;
      end;

    end;

  end;

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
  i:                    DWORD;
  idx:                  DWORD;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := True;

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
    Result := False;
  end;

  // Variable bits: Master key...
  if Result then begin
    //    volumeDetailsBlock.MasterKey := Copy(stringRep, idx, (volumeDetailsBlock.MasterKeyLength div 8));
    volumeDetailsBlock.MasterKey :=
      SDUStringToSDUBytes(Copy(stringRep, idx, (volumeDetailsBlock.MasterKeyLength div 8)));
    Inc(idx, (volumeDetailsBlock.MasterKeyLength div 8));
  end;


  // 8 bits: Requested drive letter...
  if Result then begin
    volumeDetailsBlock.RequestedDriveLetter := Char(stringRep[idx]);
    Inc(idx);
  end;


  // If the CDB Format ID is 2 or more, the CDB has a volume IV block
  // For CDB Format IDs less than 2, use a NULL volume IV
  if (volumeDetailsBlock.CDBFormatID < 2) then begin
    volumeDetailsBlock.VolumeIVLength := 0;
    //    volumeDetailsBlock.VolumeIV       := '';
    SDUZeroBuffer(volumeDetailsBlock.VolumeIV);
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
      Result := False;
    end;

    // Variable bits: Master key...
    if Result then begin
      volumeDetailsBlock.VolumeIV :=
        SDUStringToSDUBytes(Copy(stringRep, idx, (volumeDetailsBlock.VolumeIVLength div 8)));
      Inc(idx, (volumeDetailsBlock.VolumeIVLength div 8));
    end;

  end;


  // For CDB Format IDs less than 2, determine the sector IV generation method
  // based on the volume flags, and reset the volume flags
  // volumeDetailsBlock.SectorIVGenMethod already set above when by processing VolumeFlags
  if (volumeDetailsBlock.CDBFormatID >= 3) then begin
    // 8 bits: Sector IV generation method...
    tmpByte := Ord(stringRep[idx]);
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
  tmpString: Ansistring;
begin
  Result := True;

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
  stringRep := stringRep + Copy(SDUBytesToString(volumeDetailsBlock.MasterKey),
    1, (volumeDetailsBlock.MasterKeyLength div 8));
  //   Copy(volumeDetailsBlock.MasterKey, 1,    (volumeDetailsBlock.MasterKeyLength div 8));

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
    stringRep := stringRep + Copy(SDUBytesToString(volumeDetailsBlock.VolumeIV),
      1, (volumeDetailsBlock.VolumeIVLength div 8));
    //     Copy(volumeDetailsBlock.VolumeIV, 1,      (volumeDetailsBlock.VolumeIVLength div 8));

    if (volumeDetailsBlock.CDBFormatID >= 3) then begin
      // 8 bits: Sector IV generation method...
      stringRep := stringRep + ansichar(
        FreeOTFESectorIVGenMethodID[volumeDetailsBlock.SectorIVGenMethod]);
    end;

  end;

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
  offsetWithinFile: Int64; userPassword: TSDUBytes; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  CDB:        Ansistring;
  prevCursor: TCursor;
begin
  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Result := ReadRawVolumeCriticalData(filename, offsetWithinFile, CDB);

    if Result then begin
      Result := ReadVolumeCriticalData_CDB(CDB, userPassword, saltLength,
        keyIterations, volumeDetails, CDBMetaData);
    end;
  finally
    Screen.Cursor := prevCursor;
  end;

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
  userPassword: TSDUBytes; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
begin

  DebugMsg('Attempting v2 format FreOTFE Header decode...');

  Result := ReadVolumeCriticalData_CDB_v2(CDB, userPassword, saltLength,
    keyIterations, volumeDetails, CDBMetaData);
  (*
  if not Result then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('v2 format Header decode failed - attempting v1...');
{$ENDIF}
    Result := ReadVolumeCriticalData_CDB_v1(CDB, userPassword, saltLength,
      volumeDetails, CDBMetaData);
  end;
  *)

  if Result then begin
    // Warn user if the volume appears to be a later version that this software
    // supports
    if (volumeDetails.CDBFormatID > CDB_FORMAT_ID) then begin
      SDUMessageDlg(
        _('WARNING: This container appears to have been created with a later version of LibreCrypt than this software supports.'
        + SDUCRLF + SDUCRLF + 'You may continue, however:' + SDUCRLF +
        '1) Your data may be read incorrectly due to changes in the decryption process, and'
        +
        SDUCRLF + '2) It is not recommended that you write to this container using this software.'),
        mtWarning);
    end;

  end;
end;

(*
 // ----------------------------------------------------------------------------
 // Version 1 volumes:
 //   * Used the hash of the user's password + salt as the CDB
 //      encryption/decryption key
 //   * Used the hash of the volume details block as the check data
 // !!! WARNING !!!
 // THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
 //   Cypher keysizes (in bits)
 //   Cypher blocksizes (in bits)
 //   Hash lengths (in bits)
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v1(CDB: Ansistring;
  userPassword: TSDUBytes; saltLength: Integer;  // In bits

  var volumeDetails: TVolumeDetailsBlock; var CDBMetaData: TCDBMetaData): Boolean;
var
  validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaData:   TCDBMetaDataArray;

  encryptedBlockLen:     Integer;  // In *bits*
  encryptedBlock:        Ansistring;
  decryptedBlock:        Ansistring;
  strVolumeDetailsBlock: Ansistring;
  hashDrivers:           array of TFreeOTFEHashDriver;
  hi, hj:                Integer;
  currHashDriver:        TFreeOTFEHashDriver;
  currHashImpl:          TFreeOTFEHash;
  cypherDrivers:         array of TFreeOTFECypherDriver;
  ci, cj:                Integer;
  currCypherDriver:      TFreeOTFECypherDriver;
  currCypherImpl:        TFreeOTFECypher_v3;
  salt:                  TSDUBytes;
  saltedUserPassword:    TSDUBytes;
  derivedKey:            TSDUBytes;   // The key to be used in decrypting the CDB, after
  // salting, hashing, HMACing, etc - but before any
  // truncating/padding as a result of the cypher's keysize
  hk:                    Integer;     // Hash value length, in *bits*
  ks:                    Integer;     // Cypher keysize, in *bits*
  bs:                    Integer;     // Cypher blocksize, in *bits*
  criticalDataKey:       TSDUBytes;
  IV:                    Ansistring;
  checkDataDecrypted:    String;
  checkDataGenerated:    TSDUBytes;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData:   TCDBMetaData;

  tgtVolDetailsBlockLen: Integer;  // In *bits*
  prevCursor:            TCursor;
  derivedKeyStr, criticalDataKeyStr, checkDataGeneratedStr: Ansistring;
begin
  SetLength(validVolumeDetails, 0);
  SetLength(validCDBMetaData, 0);


  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // NOTE: "Result" indicates a *fatal* error in processing; not that an invalid
    //       password was entered
    Result := True;


    // Append the salt onto the user's password
    if Result then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Determining user''s salted password...');
  {$ENDIF}
      salt               := SDUStringToSDUBytes(Copy(CDB, 1, (saltLength div 8)));
      saltedUserPassword := userPassword;
      SDUAddArrays(saltedUserPassword, salt);

  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('User''s salted key:');
  DebugMsg('-- begin --');
  DebugMsg(saltedUserPassword);
  DebugMsg('-- end --');
  {$ENDIF}
    end;

    // Obtain details of all hashes...
    if Result then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting hash drivers...');
  {$ENDIF}
      SetLength(hashDrivers, 0);
      Result := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
    end;

    // Obtain details of all cyphers...
    if Result then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Getting cypher drivers...');
  {$ENDIF}
      SetLength(cypherDrivers, 0);
      Result := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
    end;


    if Result then begin
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
            saltedUserPassword, derivedKey)) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('ERROR: Hashing FAILED.');
  {$ENDIF}
            Result := False;
          end;


          if Result then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Hashed data OK.');
  DebugMsg('Processed user''s password and salt to give:');
  DebugMsg( derivedKey);
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
                      derivedKeyStr + StringOfChar(AnsiChar(#0),
                      ((ks div 8) - Length(derivedKeyStr)));
                  end else begin
                    // The hash value is too long; truncate
                    criticalDataKeyStr := Copy(derivedKeyStr, 1, (ks div 8));
                  end;
                  // copy derivedKey, and pad if no long enough
                  SDUAddLimit(criticalDataKey, derivedKey, ks div 8);
                  SafeSetLength(criticalDataKey, ks div 8);
                  assert(criticalDataKeyStr = SDUBytesToString(criticalDataKey));
                end;  // if (ks > 0) then



                // Zero the IV for decryption
                IV := StringOfChar(AnsiChar(#0), (bs div 8));


                DebugMsg('About to decrypt data...');

                if not (DecryptSectorData(currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, FREEOTFE_v1_DUMMY_SECTOR_ID,
                  FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKey, IV,
                  encryptedBlock, decryptedBlock)) then begin

                  DebugMsg('ERROR: Decryption FAILED.');

                  Result := False;
                end else begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Key:');
  DebugMsg(criticalDataKey);
  DebugMsg('Decoded to:');
  DebugMsgBinary(decryptedBlock);
  {$ENDIF}
                  // Extract the decrypted check hash and volume details block
                  // 1 and +1 because we index from 1
                  checkDataDecrypted    := Copy(decryptedBlock, 1, (hk div 8));
                  tgtVolDetailsBlockLen := (((4096 - saltLength) div bs) * bs) - hk;
                  strVolumeDetailsBlock :=
                    Copy(decryptedBlock, ((hk div 8) + 1), (tgtVolDetailsBlockLen div 8));

                  DebugMsg('About to generate hash/HMAC using decoded data...');



                  // HASH THE USER DECODED DATA...
                  if not (HashData(currHashDriver.LibFNOrDevKnlMdeName,
                    currHashImpl.HashGUID, SDUStringToSDUBytes(strVolumeDetailsBlock),
                    checkDataGenerated)) then begin

                    DebugMsg('ERROR: Hash of decrypted data FAILED.');

                    Result := False;
                  end;


                  if Result then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Decrypted data hashed OK.');
  {$ENDIF}
                    checkDataGeneratedStr := SDUBytesToString(checkDataGenerated);
                    // If the checkdata newly generated is less than hk bits, right pad it with zero bits
                    if ((Length(checkDataGenerated) * 8) < hk) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash less than hk bits; padding...');
  {$ENDIF}
                      checkDataGeneratedStr :=
                        checkDataGeneratedStr + StringOfChar(AnsiChar(#0),
                        (((Length(checkDataGeneratedStr) * 8) - hk) div 8));

                      SafeSetLength(checkDataGenerated, hk);
                      assert(checkDataGeneratedStr = SDUBytesToString(checkDataGenerated));
                    end;

                    // If the new checkdata is greater than hk bits, truncate it after the first
                    // hk bits
                    if ((Length(checkDataGeneratedStr) * 8) > hk) then begin

                      // +1 because we index from 1, not zero
                      Delete(checkDataGeneratedStr, (hk div 8) + 1,
                        (Length(checkDataGeneratedStr) - (hk div 8)));
                    end;

                    if ((Length(checkDataGenerated) * 8) > hk) then begin
  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Check hash generated hash more than hk bits; padding...');
  {$ENDIF}
                      SafeSetLength(checkDataGenerated, hk div 8);
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

                        DebugMsg('ERROR: FAILED to parse plaintext volume details block to structure');

                        // At this point, there has been some failure (e.g.
                        // critical data block version ID or block structure
                        // incorrect)
                        // Note that we *don't* set Result to FALSE; this failure
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
                          SDUCopy(fDumpCriticalDataKey, criticalDataKey);
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

    end;  // if Result  then


  {$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Completed checking for all hash/cypher combinations');
  {$ENDIF}
  finally
    Screen.Cursor := prevCursor;
  end;
  // Depending on how many hash/cypher combinations were found, select the
  // appropriate one to use
  if Result then begin
    Result := ChooseFromValid(validVolumeDetails, validCDBMetaData, volumeDetails,
      CDBMetaData);
  end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Finished function.');
{$ENDIF}

end;
*)

 // ----------------------------------------------------------------------------
 // Version 2 volumes:
 //   * Used the PKCS#5 PBKDF2 key derivation function with the user's password
 //      and salt to generate thethe CDB encryption/decryption key
 //   * Used the HMAC of the (derived key, volume details block) as the check data
 // !!! WARNING !!!
 // THIS SOFTWARE ASSUMES THAT THE FOLLOWING ARE ALL MULTIPLES OF 8:
 //   Cypher keysizes (in bits)
 //   Cypher blocksizes (in bits)
 //   Hash lengths (in bits)
function TOTFEFreeOTFEBase.ReadVolumeCriticalData_CDB_v2(CDB: Ansistring;
  userPassword: TSDUBytes; saltLength: Integer;  // In bits
  keyIterations: Integer; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData

  ): Boolean;
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

  salt:            TSDUBytes;
  saltStr:         Ansistring;
  cdkSize_bits:    Integer;     // Size of the "critical data key", in *bits*
  maxCDKSize_bits: Integer;     // Max size of the "critical data key", in *bits*
  IV:              Ansistring;

  currVolumeDetails: TVolumeDetailsBlock;
  currCDBMetaData:   TCDBMetaData;

  leb_bits:           Integer;  // In *bits*
  criticalDataKey:    TSDUBytes;  // The key to be used in decrypting the CDB, after
  // salting, hashing, HMACing, etc - but before any
  // truncating/padding as a result of the cypher's keysize
  criticalDataKeyStr: Ansistring;
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

    // NOTE: "Result" indicates whether or not a *fatal* error in processing;
    //       NOT that an invalid password was entered
    Result := True;


    // Obtain details of all hashes...
    if Result then begin
      DebugMsg('Getting hash drivers...');

      SetLength(hashDrivers, 0);
      Result := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
    end;


    // Obtain details of all cyphers...
    if Result then begin

      DebugMsg('Getting cypher drivers...');

      SetLength(cypherDrivers, 0);
      Result := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
    end;


    // Strip off salt
    if Result then begin

      DebugMsg('Extracting salt...');

      saltStr := Copy(CDB, 1, (saltLength div 8));
      SDUCopyLimit(salt, SDUStringToSDUBytes(CDB), saltLength div 8);
      assert(SDUBytestoString(salt) = saltStr);  //passed
      paddedEncryptedBlock := Copy(CDB, ((saltLength div 8) + 1),
        (Length(CDB) - (saltLength div 8)));

      DebugMsg('Salt follows:');
      DebugMsg(salt);

    end;

    maxCDKSize_bits := 0;// avoids warning

    // !!! OPTIMISATION !!!
    // Determine the longest cypher key
    if Result then begin
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



    if Result then begin
      try

        DebugMsg('Starting main hashes loop...');

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

            Result := DeriveKey(KDFAlgorithm, currHashDriver.LibFNOrDevKnlMdeName,
              currHashImpl.HashGUID, '', StringToGUID(NULL_GUID),
              userPassword, salt, keyIterations, maxCDKSize_bits,  // cdkSizeBits
              maxCriticalDataKey);
            if not (Result) then begin
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
                criticalDataKeyStr :=
                  Copy(SDUBytesToString(maxCriticalDataKey), 1, (cdkSize_bits div 8));
                SDUCopyLimit(criticalDataKey, maxCriticalDataKey, cdkSize_bits div 8);
                assert(criticalDataKeyStr = SDUBytesToString(criticalDataKey));//passed
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
                SDUCopy(criticalDataKeyBytes, criticalDataKey);
                //                criticalDataKeyBytes := SDUStringToSDUBytes(criticalDataKey);
                Result :=
                  DecryptSectorData(currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, FREEOTFE_v1_DUMMY_SECTOR_ID,
                  FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKeyBytes, IV,
                  encryptedBlock, plaintextEncryptedBlock);
                SDUCopy(criticalDataKey, criticalDataKeyBytes);
                //                criticalDataKey      := SDUBytesToString(criticalDataKeyBytes);
                if not (Result) then begin
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
                criticalDataKeyStr := SDUBytesToString(criticalDataKey);
                Result             :=
                  MACData(MACAlgorithm, currHashDriver.LibFNOrDevKnlMdeName,
                  currHashImpl.HashGUID, currCypherDriver.LibFNOrDevKnlMdeName,
                  currCypherImpl.CypherGUID, criticalDataKeyStr,
                  volumeDetailsBlock, generatedMAC, -1);
                criticalDataKey    := SDUStringToSDUBytes(criticalDataKeyStr);

                if not (Result) then begin
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
                    DebugMsg('ERROR: FAILED to parse plaintext volume details block to structure');

                    // At this point, there has been some failure (e.g.
                    // critical data block version ID or block structure
                    // incorrect)
                    // Note that we *don't* set Result to FALSE; this failure
                    // is valid and only indicates that the current
                    // cypher/hash combination are incorrect for the volume;
                    // not that something has gone wrong with the mount
                    // operation
                  end else begin

                    DebugMsg('OK so far...');

                    // Sanity check...
                    if ((currCypherImpl.KeySizeRequired >= 0) and
                      (currVolumeDetails.MasterKeyLength <>
                      DWORD(currCypherImpl.KeySizeRequired))) then begin

                      DebugMsg('ERROR: Sanity check failed(!)');

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

                    DebugMsg('Got valid decryption cypher/hash combination');

                    SetLength(validVolumeDetails, (Length(validVolumeDetails) + 1));
                    SetLength(validCDBMetaData, (Length(validCDBMetaData) + 1));
                    validVolumeDetails[Length(validVolumeDetails) - 1] := currVolumeDetails;
                    validCDBMetaData[Length(validCDBMetaData) - 1]     := currCDBMetaData;

                    DebugMsg('OK.');


                    // Additional: Store internal information, if dumping
                    if fdumpFlag then begin
                      SDUCopy(fDumpCriticalDataKey, criticalDataKey);
                      fdumpCheckMAC                := checkMAC;
                      fdumpPlaintextEncryptedBlock := plaintextEncryptedBlock;
                      fdumpVolumeDetailsBlock      := volumeDetailsBlock;
                    end;

                  end;
                  // ELSE PART - if not(ParseVolumeDetailsBlock(volumeDetailsBlock, currVolumeDetails)) then

                end else begin
                  // Do nothing - just loop around for the next one...
                  DebugMsg('Check hash and generated hash do not match.');

                end;  // ELSE PART - if (checkMAC = generatedMAC) then

              end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do

            end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do

          end; // for hj:=low(hashDrivers[hi].Hashes) to high(hashDrivers[hi].Hashes) do

        end;  // for hi:=low(hashDrivers) to high(hashDrivers) do

      except
        on EFreeOTFEReadCDBFailure do begin
          Result := False;
        end;

      end;

    end;  // if Result  then

    DebugMsg('Completed checking for all hash/cypher combinations');

  finally
    Screen.Cursor := prevCursor;
  end;


  // Depending on how many hash/cypher combinations were found, select the
  // appropriate one to use
  if Result then begin
    Result := ChooseFromValid(validVolumeDetails, validCDBMetaData, volumeDetails,
      CDBMetaData);
  end;

  DebugMsg('Finished function.');

end;

function TOTFEFreeOTFEBase.ChooseFromValid(validVolumeDetails: TVolumeDetailsBlockArray;
  validCDBMetaDatas: TCDBMetaDataArray; var volumeDetails: TVolumeDetailsBlock;
  var CDBMetaData: TCDBMetaData): Boolean;
var
  hashCypherSelectDlg: TfrmSelectHashCypher;
  i:                   Integer;
  prevCursor:          TCursor;
begin
  Result := True;

  if (Length(validCDBMetaDatas) = 0) then begin
    // No valid combination of hash/cypher could be found
    DebugMsg('No valid hash/cypher combination found');

    Result := False;
  end else
  if (Length(validCDBMetaDatas) = 1) then begin
    // There is only one valid hash/cypher combination that can be used; use it
    DebugMsg('Exactly one hash/cypher combination found successfully');

    volumeDetails := validVolumeDetails[low(validVolumeDetails)];
    CDBMetaData   := validCDBMetaDatas[low(validCDBMetaDatas)];
  end else begin
    // More than one valid hash/cypher combination; offer the user the choice
    DebugMsg('More than one hash/cypher combination found');

    hashCypherSelectDlg := TfrmSelectHashCypher.Create(nil);
    prevCursor          := Screen.Cursor;
    Screen.Cursor       := crDefault;
    try
      //      hashCypherSelectDlg.FreeOTFEObj := self;
      for i := low(validCDBMetaDatas) to high(validCDBMetaDatas) do begin
        hashCypherSelectDlg.AddCombination(
          validCDBMetaDatas[i].HashDriver,
          validCDBMetaDatas[i].HashGUID,
          validCDBMetaDatas[i].CypherDriver,
          validCDBMetaDatas[i].CypherGUID
          );
      end;

      if (hashCypherSelectDlg.ShowModal() <> mrOk) then begin
//        LastErrorCode := OTFE_ERR_USER_CANCEL;
        DebugMsg('User cancelled');

        Result := False;
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
            DebugMsg('User''s selected hash/cypher combination found.');

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
  offsetWithinFile: Int64; userPassword: TSDUBytes; salt: TSDUBytes;
  keyIterations: Integer; volumeDetails: TVolumeDetailsBlock; CDBMetaData: TCDBMetaData
  // Note: If insufficient is supplied, the write will
  //       *fail*
  //       The maximum that could possibly be required
  //       is CRITICAL_DATA_LENGTH bits.
  ): Boolean;
var
  encryptedBlock:     Ansistring;
  sl_bits:            Integer;  // Salt value length, in *bits*
  criticalDataKey:    TSDUBytes;
  criticalDataKeyStr: Ansistring;
  IV:                 Ansistring;
  cypherDetails:      TFreeOTFECypher_v3;
  hashDetails:        TFreeOTFEHash;
  volumeDetailsBlockNoPadding: Ansistring;
  volumeDetailsBlock: Ansistring;
  criticalDataBlock:  Ansistring;
  cdkSize_bits:       Integer;                      // In *bits*
  randomPaddingTwoLength_bits: Integer;             // In *bits*
  leb_bits:           Integer;                      // In *bits*
  volumeDetailsBlockLength_bits: Integer;           // In *bits*
  volumeDetailsBlockNoPaddingLength_bits: Integer;  // In *bits*
  checkMAC:           Ansistring;
  paddedCheckMAC:     Ansistring;
  randomPaddingThreeLength_bits: Integer;  // In *bits*
  plaintextEncryptedBlock: Ansistring;
  randomPaddingOneLength_bits: Integer;  // In *bits*

  randomPadData: TSDUBytes;
begin
  Result := True;

  DebugMsg('In WriteVolumeCriticalData');


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
  if Result then begin
    DebugMsg('About to get cypher details...');

    if GetSpecificCypherDetails(CDBMetaData.CypherDriver, CDBMetaData.cypherGUID,
      cypherDetails) then begin
      DebugMsg('Cypher driver: ' + CDBMetaData.CypherDriver);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Cypher impl: '+cypherDetails.Title+' ('+GUIDToString(CDBMetaData.CypherGUID)+')');
{$ENDIF}
      //
    end else begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      Result        := False;
    end;
  end;


  // Get details of the hash so we know the hash length...
  if Result then begin
    DebugMsg('About to get hash details...');

    if GetSpecificHashDetails(CDBMetaData.HashDriver, CDBMetaData.hashGUID, hashDetails) then begin
      DebugMsg('Hash driver: ' + CDBMetaData.HashDriver);


DebugMsg('Hash impl: '+hashDetails.Title+' ('+GUIDToString(CDBMetaData.HashGUID)+')');

    end else begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      Result        := False;
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
      'Invalid: Container IV cannot be used if cypher blocksize <= 0');
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
    raise EFreeOTFEInternalError.Create('Invalid: Container IV length must be multiple of 8');
  end;



  // Determine size of "critical data key"...
  cdkSize_bits := cypherDetails.KeySizeRequired;  // In *bits*
  if (cdkSize_bits < 0) then begin
    cdkSize_bits := 512;
  end;


  // Derive the "critical data key"
  if Result then begin
    Result := DeriveKey(CDBMetaData.KDFAlgorithm, CDBMetaData.HashDriver,
      CDBMetaData.HashGUID, CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      userPassword, salt, keyIterations, cdkSize_bits, criticalDataKey);
    if not (Result) then begin
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
      Result        := False;
    end;
  end;


  // Create plaintext version of the "Volume details block" in-memory
  if Result then begin
    DebugMsg('Converting volume details block structure to string rep...');

    // Note: The string returned by BuildVolumeDetailsBlock(...) does *not*
    //       have any random data appended to it
    if not (BuildVolumeDetailsBlock(volumeDetails, volumeDetailsBlockNoPadding)) then begin
      DebugMsg('ERROR: FAILED to convert critical data structure to string rep.');
      LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
      Result        := False;
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
  if Result then begin
    DebugMsg('About to HMAC critical data to create check data...');

    criticalDataKeyStr := SDUBytesToString(criticalDataKey);
    Result             := MACData(CDBMetaData.MACAlgorithm, CDBMetaData.HashDriver,
      CDBMetaData.HashGUID, CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      criticalDataKeyStr, volumeDetailsBlock, checkMAC, -1);
    criticalDataKey    := SDUStringToSDUBytes(criticalDataKeyStr);
    if not (Result) then begin
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
      Result        := False;
    end;

  end;


  // Truncate/pad check data as appropriate
  if Result then begin
    DebugMsg('MAC check data generated OK.');

    // If the checkdata newly generated is less than CDB_MAX_MAC_LENGTH bits,
    // right pad it with random data bits to CDB_MAX_MAC_LENGTH
    // If the checkdata newly generated is greater than CDB_MAX_MAC_LENGTH
    // bits, truncate it to the first CDB_MAX_MAC_LENGTH bits.
    if ((Length(checkMAC) * 8) < CDB_MAX_MAC_LENGTH) then begin
      DebugMsg('Check data generated hash less than CDB_MAX_MAC_LENGTH bits; padding...');

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
      DebugMsg('Check data generated hash greater than CDB_MAX_MAC_LENGTH bits; truncating...');

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

  DebugMsg('Padded check MAC is: ');
  DebugMsg('-- begin --');
  DebugMsgBinary(paddedCheckMAC);
  DebugMsg('-- end --');

  if Result then begin
    // Prepend the check MAC (padded if necessary) to the volume details block
    // to form a plaintext version of the "Encrypted block"
    plaintextEncryptedBlock := paddedCheckMAC + volumeDetailsBlock;
  end;


  // Encrypt the plaintextEncryptedBlock, using
  //  - Zero'd IV
  //  - Critical data key
  if Result then begin
    DebugMsg('Creating zeroed IV');

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
DebugMsgBinary(criticalDataKey);
DebugMsg('before encryption, decrypted block is:');
DebugMsgBinary(plaintextEncryptedBlock);
{$ENDIF}
    if not (EncryptSectorData(CDBMetaData.CypherDriver, CDBMetaData.CypherGUID,
      FREEOTFE_v1_DUMMY_SECTOR_ID, FREEOTFE_v1_DUMMY_SECTOR_SIZE, criticalDataKey,
      IV, plaintextEncryptedBlock, encryptedBlock)) then begin
      DebugMsg('ERROR: FAILED to encrypt data block');

      LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
      Result        := False;
    end;

  end;



  // Form the critical datablock using the salt bytes, the encrypted data block,
  // and random padding data #1
  if Result then begin
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
  if Result then begin
    Result := WriteRawVolumeCriticalData(filename, offsetWithinFile, criticalDataBlock);
  end;

  DebugMsg('Exiting function');

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetHashList(
  var hashDispTitles, hashKernelModeDriverNames, hashGUIDs: TStringList): Boolean;
var
  hashDrivers:    array of TFreeOTFEHashDriver;
  i, j, k:        Integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHash:       TFreeOTFEHash;
  tmpPrettyTitle: String;
  inserted:       Boolean;
begin
  Result := False;

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

    Result := True;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetCypherList(
  var cypherDispTitles, cypherKernelModeDriverNames, cypherGUIDs: TStringList): Boolean;
var
  cypherDrivers:    array of TFreeOTFECypherDriver;
  i, j, k:          Integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypher:       TFreeOTFECypher_v3;
  tmpPrettyTitle:   String;
  inserted:         Boolean;
begin
  Result := False;

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

    Result := True;
  end;

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
function TOTFEFreeOTFEBase.CreateLinuxGPGKeyfile(): Boolean;
begin
  Result := False;

  // xxx - implement GPG integration

end;





 // -----------------------------------------------------------------------------
 // Determine if OTFE component can mount devices.
 // Returns TRUE if it can, otherwise FALSE
function TOTFEFreeOTFEBase.CanMountDevice(): Boolean;
begin
  // Supported
  Result := True;
end;




// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.BackupVolumeCriticalData(srcFilename: String;
  srcOffsetWithinFile: Int64; destFilename: String): Boolean;
var
  criticalData:   Ansistring;
  destFileStream: TFileStream;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  Result := ReadRawVolumeCriticalData(srcFilename, srcOffsetWithinFile, criticalData);

  if Result then begin
    // Because we're just writing to a straight file, we don't need the
    // FreeOTFE driver to do this.
    // Also, because the file shouldn't already exist, the FreeOTFE driver
    // *won't* do this - it expects the target to already exist
    destFileStream := TFileStream.Create(destFilename, fmCreate or fmShareDenyNone);
    try
      if length(criticalData) > 0 then begin
        Result := (destFileStream.Write(criticalData[1], length(criticalData)) =
          length(criticalData));
      end;
    finally
      destFileStream.Free();
    end;

  end;

end;


// -----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.RestoreVolumeCriticalData(srcFilename: String;
  destFilename: String; destOffsetWithinFile: Int64): Boolean;
var
  criticalData: Ansistring;
begin
  // Get the FreeOTFE driver to read a critical data block from the named file
  // starting from the specified offset
  Result := ReadRawVolumeCriticalData(srcFilename, 0, criticalData);

  if Result then begin
    // Get the FreeOTFE driver to write the critical data block to the named file
    // starting from the specified offset
    Result := WriteRawVolumeCriticalData(destFilename, destOffsetWithinFile, criticalData);
  end;

end;


function TOTFEFreeOTFEBase.CreateKeyfile(srcFilename: String; srcOffsetWithinFile: Int64;
  srcUserPassword: TSDUBytes; srcSaltLength: Integer;  // In bits
  srcKeyIterations: Integer; keyFilename: String; keyfileUserPassword: TSDUBytes;
  keyfileSaltBytes: TSDUBytes; keyfileKeyIterations: Integer;
  keyfileRequestedDrive: DriveLetterChar): Boolean;
var
  volumeDetails:        TVolumeDetailsBlock;
  CDBMetaData:          TCDBMetaData;
  userCancel:           Boolean;
  keyfileRandomPadData: Ansistring;  // Note: If insufficient is supplied, the write will *fail*
  //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
begin
  Result := False;
  GetRandPool.GenerateRandomData(CRITICAL_DATA_LENGTH div 8, keyfileRandomPadData);


  if ReadVolumeCriticalData(srcFilename, srcOffsetWithinFile, srcUserPassword,
    srcSaltLength,  // In bits
    srcKeyIterations, volumeDetails, CDBMetaData) then begin
    volumeDetails.RequestedDriveLetter := keyfileRequestedDrive;

    // Just create a dummy file which will be populated with the keyfile's
    // contents (the driver doesn't create files, it just writes data to them)
    if (not (FileExists(keyFilename))) then
      SDUCreateLargeFile(keyFilename, (CRITICAL_DATA_LENGTH div 8), False, userCancel);

    Result := WriteVolumeCriticalData(keyFilename, 0, keyfileUserPassword,
      keyfileSaltBytes, keyfileKeyIterations, volumeDetails,
      CDBMetaData{, keyfileRandomPadData});
  end;

end;


function TOTFEFreeOTFEBase.ChangeVolumePassword(filename: String; offsetWithinFile: Int64;
  oldUserPassword: TSDUBytes; oldSaltLength: Integer;  // In bits
  oldKeyIterations: Integer; newUserPassword: TSDUBytes; newSaltBytes: TSDUBytes;
  newKeyIterations: Integer; newRequestedDriveLetter: DriveLetterChar

  //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
  ): Boolean;
var
  volumeDetails:    TVolumeDetailsBlock;
  CDBMetaData:      TCDBMetaData;
  newRandomPadData: Ansistring; // Note: If insufficient is supplied, the write will *fail*
  //       The maximum that could possibly be required is CRITICAL_DATA_LENGTH bits.
begin
  //todo: better to get random data in WriteVolumeCriticalData
  GetRandPool.GenerateRandomData(CRITICAL_DATA_LENGTH, newRandomPadData);
  Result := False;
  if ReadVolumeCriticalData(filename, offsetWithinFile, oldUserPassword,
    oldSaltLength,  // In bits
    oldKeyIterations, volumeDetails, CDBMetaData) then begin
    volumeDetails.RequestedDriveLetter := newRequestedDriveLetter;

    Result := WriteVolumeCriticalData(filename, offsetWithinFile, newUserPassword,
      newSaltBytes, newKeyIterations, volumeDetails, CDBMetaData{, newRandomPadData});
  end;
end;

 // Convert a kernel mode volume filename to a user mode volume filename
function TOTFEFreeOTFEBase.GetUserModeVolumeFilename(kernelModeFilename: String): String;
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
    //else ...
    // \Device\Harddisk0\Partition1\path\filedisk.img
    // Do nothing.
  end;

end;


 // ----------------------------------------------------------------------------
 // Convert a user mode volume filename to a kernel mode volume filename
function TOTFEFreeOTFEBase.GetKernelModeVolumeFilename(userModeFilename: String): String;
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
function TOTFEFreeOTFEBase.IsPartitionUserModeName(userModeFilename: String): Boolean;
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
begin
  Result := ReadRawVolumeDataSimple(filename, offsetWithinFile, dataLength, data);
  if not (Result) then begin
    Result := ReadRawVolumeDataBounded(filename, offsetWithinFile, dataLength, data);
  end;

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
begin
  preData  := (offsetWithinFile mod ASSUMED_HOST_SECTOR_SIZE);
  // Yes, this is correct.
  // We use preData to avoid the need to add offsetWithinFile (a 64 bit value)
  // and then do the mod
  postData := ASSUMED_HOST_SECTOR_SIZE - ((preData + dataLength) mod ASSUMED_HOST_SECTOR_SIZE);

  Result := ReadRawVolumeDataSimple(filename, (offsetWithinFile - preData),
    (preData + dataLength + postData), tmpData);
  if Result then begin
    // Trim off the pre/post data garbage
    data := Copy(tmpData, preData, dataLength);

    // Overwrite...
    tmpData := StringOfChar(AnsiChar(#0), (preData + dataLength + postData));
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadRawVolumeDataSimple(filename: String;
  offsetWithinFile: Int64; dataLength: DWORD;
  // In bytes
  var Data: Ansistring): Boolean;
var
  fStream: TFileStream;
begin
  Result := False;

  CheckActive();

  // Set data so that it has enough characters which can be
  // overwritten with StrMove
  data := StringOfChar(AnsiChar(#0), dataLength);

  try
    fStream := TFileStream.Create(filename, fmOpenRead);
    try
      fStream.Position := offsetWithinFile;
      fStream.Read(data[1], dataLength);
      Result := True;
    finally
      fStream.Free();
    end;
  except
    // Just swallow exception; Result already set to FALSE
  end;

end;


 // ----------------------------------------------------------------------------
 // Expose ReadRawVolumeData(...) for debug
function TOTFEFreeOTFEBase.DEBUGReadRawVolumeData(
  filename: String;
  offsetWithinFile: Int64;
  dataLength: DWORD;  // In bytes
  var ansidata: Ansistring
  ): Boolean;
begin
  Result := ReadRawVolumeData(filename, offsetWithinFile, dataLength, ansidata);
end;

 // ----------------------------------------------------------------------------
 // Expose WriteRawVolumeData(...) for debug
function TOTFEFreeOTFEBase.DEBUGWriteRawVolumeData(
  filename: String;
  offsetWithinFile: Int64;
  data: String
  ): Boolean;
begin
  Result := WriteRawVolumeData(filename, offsetWithinFile, data);
end;

 // ----------------------------------------------------------------------------
 // Expose ReadWritePlaintextToVolume(...) for debug
function TOTFEFreeOTFEBase.DEBUGReadWritePlaintextToVolume(
  readNotWrite: Boolean;

  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  IVHashDriver: String;
  IVHashGUID: TGUID;
  IVCypherDriver: String;
  IVCypherGUID: TGUID;
  mainCypherDriver: String;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  mountMountAs: TMountDiskType;

  dataOffset: Int64;
  // Offset from within mounted volume from where to read/write data
  dataLength: Integer;  // Length of data to read/write. In bytes
  var data: TSDUBytes;  // Data to read/write

  offset: Int64 = 0;
  size: Int64 = 0;
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
  ): Boolean;
begin
  Result := ReadWritePlaintextToVolume(readNotWrite, volFilename, volumeKey,
    sectorIVGenMethod, IVHashDriver, IVHashGUID, IVCypherDriver, IVCypherGUID,
    mainCypherDriver, mainCypherGUID, VolumeFlags, mountMountAs, dataOffset,
    dataLength, data, offset, size, storageMediaType);
end;

 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to write a critical data block from the named file
 // starting from the specified offset
 // criticalData - This should be set to the string representation of a raw
 //                (encrypted) critical data block
function TOTFEFreeOTFEBase.WriteRawVolumeCriticalData(filename: String;
  offsetWithinFile: Int64; criticalData: Ansistring): Boolean;
begin
  if (length(criticalData) <> (CRITICAL_DATA_LENGTH div 8)) then
    EFreeOTFEInvalidParameter.Create('Invalid FreeOTFE header passed in for raw writing');

  Result := WriteRawVolumeData(filename, offsetWithinFile, criticalData);
end;


 // ----------------------------------------------------------------------------
 // Get the FreeOTFE driver to write data from the named file
 // starting from the specified offset
 // data - This will be set to the string representation of the raw data read
function TOTFEFreeOTFEBase.WriteRawVolumeData(filename: String; offsetWithinFile: Int64;
  data: Ansistring): Boolean;
begin
  Result := WriteRawVolumeDataSimple(filename, offsetWithinFile, data);
  if not (Result) then begin
    Result := WriteRawVolumeDataBounded(filename, offsetWithinFile, data);
  end;

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
  bytesWritten: Integer;
begin
  Result := False;

  CheckActive();

  try
    fStream := TFileStream.Create(filename, fmOpenReadWrite);
    try
      fStream.Position := offsetWithinFile;
      bytesWritten     := fStream.Write(data[1], length(data));
      Result           := (bytesWritten = length(data));
    finally
      fStream.Free();
    end;
  except
    // Just swallow exception; Result already set to FALSE
  end;

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
 // Generate a list of HDDs and partitions
function TOTFEFreeOTFEBase.HDDNumbersList(var DiskNumbers: TSDUArrayInteger): Boolean;
var
  disk:       Integer;
  junk:       Ansistring;
  currDevice: String;
  prevCursor: TCursor;
begin
  Result := True;

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

end;


 // ----------------------------------------------------------------------------
 // Generate a list of HDDs and partitions
 // Note: The .Objects of hddDevices are set to the partition numbers
function TOTFEFreeOTFEBase.HDDDeviceList(hddDevices: TStringList; title: TStringList): Boolean;
var
  diskNo:     Integer;
  prevCursor: TCursor;
begin
  Result := True;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for diskNo := 0 to MAX_HDDS do begin
      HDDDeviceList(diskNo, hddDevices, title);
    end;

  finally
    Screen.Cursor := prevCursor;
  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.HDDDeviceList(diskNo: Integer; hddDevices: TStringList;
  title: TStringList): Boolean;
var
  partition:  Integer;
  junk:       Ansistring;
  currDevice: String;
  prevCursor: TCursor;
begin
  Result := False;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    // Check to see if the entire disk exists, before checking for partitions
    currDevice := SDUDeviceNameForPartition(diskNo, 0);
    if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
      Result := True;

      // The entire disk
      hddDevices.AddObject(currDevice, TObject(0));
      title.Add(Format(_('Hard disk #%d (entire disk)'), [diskNo]));

      // Note: We start from 1; partition 0 indicates the entire disk
      for partition := 1 to MAX_PARTITIONS do begin
        currDevice := SDUDeviceNameForPartition(diskNo, partition);
        if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
          hddDevices.AddObject(currDevice, TObject(partition));
          title.Add(Format(_('Hard disk #%d, partition #%d'), [diskNo, partition]));
        end;

      end;

    end;


  finally
    Screen.Cursor := prevCursor;
  end;

end;


 // ----------------------------------------------------------------------------
 // Generate a list of HDDs and partitions
function TOTFEFreeOTFEBase.CDROMDeviceList(cdromDevices: TStringList; title: TStringList): Boolean;
var
  cdrom:      Integer;
  junk:       Ansistring;
  currDevice: String;
  prevCursor: TCursor;
begin
  Result := True;

  prevCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try

    for cdrom := 0 to MAX_CDROMS do begin
      // Check to see if the entire disk exists, before checking for partitions
      currDevice := SDUDeviceNameForCDROM(cdrom);
      if ReadRawVolumeCriticalData(currDevice, 0, junk) then begin
        cdromDevices.Add(currDevice);
        title.Add(Format(_('CDROM drive #%d'), [cdrom]));
      end;
    end;

  finally
    Screen.Cursor := prevCursor;
  end;

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
begin
  Result := False;

  idx := fCachedHashDriverDetails.IndexOf(hashDriver);
  if (idx >= 0) then begin
    ptrRec            := PFreeOTFEHashDriver(fCachedHashDriverDetails.Objects[idx]);
    hashDriverDetails := ptrRec^;
    Result          := True;
  end;

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
function TOTFEFreeOTFEBase._CachesGetCypherDriver(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  ptrRec:   PFreeOTFECypherDriver;
  idx:      Integer;
begin
  Result := False;

  idx := fCachedCypherDriverDetails.IndexOf(cypherDriver);
  if (idx >= 0) then begin
    ptrRec              := PFreeOTFECypherDriver(fCachedCypherDriverDetails.Objects[idx]);
    cypherDriverDetails := ptrRec^;
    Result            := True;
  end;


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


{ factory fn creates an instance that is returned by GetFreeOTFEBase}
procedure SetFreeOTFEType(typ: TOTFEFreeOTFEBaseClass);
begin
  assert(_FreeOTFEObj = nil,
    'call SetFreeOTFEType only once, and do not call TOTFEFreeOTFEBase.create');
  _FreeOTFEObj := typ.Create;
end;

{returns an instance of type set in SetFreeOTFEType}
function GetFreeOTFEBase: TOTFEFreeOTFEBase;
begin
  assert(_FreeOTFEObj <> nil, 'call SetFreeOTFEType before GetFreeOTFE/GetFreeOTFEBase');
  Result := _FreeOTFEObj;
end;


initialization
  _FreeOTFEObj := nil; //create by calling SetFreeOTFEType

finalization
  _FreeOTFEObj.Free;

end.
