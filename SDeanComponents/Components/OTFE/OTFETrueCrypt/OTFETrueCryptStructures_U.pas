unit OTFETrueCryptStructures_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes, SysUtils, Windows;


type
  ETrueCryptError = Exception;
  ETrueCryptNotConnected = ETrueCryptError;
  ETrueCryptExeNotFound = ETrueCryptError;
  ETrueCryptVxdNotFound = ETrueCryptError;


  TDeviceIOControlRegisters = packed record
    reg_EBX : DWORD;
    reg_EDX : DWORD;
    reg_ECX : DWORD;
    reg_EAX : DWORD;
    reg_EDI : DWORD;
    reg_ESI : DWORD;
    reg_Flags : DWORD;
  end;

  TParamBlock = packed record
    Operation : Integer;
    NumLocks : Integer;
  end;

const
  VWIN32_DIOC_DOS_IOCTL = 1;
  


type
  TrueCrypt_VOLUME_FILE_TYPE = (vtidUnknown, vtidPartition, vtidTrueCryptFile);

  // This is ALL the different cypher types, in ALL versions of TrueCrypt
  // Unsupported cyphers are flagged by checking the relevant
  // TrueCrypt_CIPHER_IDS_xxx const for the value $FFFF
  TrueCrypt_CIPHER_TYPE = (
                           cphrNone,
                           cphrBlowfish,
                           cphrAES,
                           cphrCAST,
                           cphrIDEA,
                           cphrTRIPLEDES,
                           cphrDES56,
                           cphrSerpent,
                           cphrTwofish,
                           cphrUnknown
                          );


  TrueCrypt_PKCS5_TYPE = (pkcs5HMAC_SHA_1, pkcs5HMAC_RIPEMD_160, pkcs5Unknown);

  TrueCrypt_CIPHER_MODE = (cphrmodeCBC, cphrmodeOuterCBC, cphrmodeInnerCBC, cphrmodeUnknown);


const
  TrueCrypt_OS_WIN_95 = $a;
  TrueCrypt_OS_WIN_98 = $b;
  TrueCrypt_OS_WIN_NT = $c;


  TrueCrypt_VOLUME_FILE_TYPE_NAMES: array [TrueCrypt_VOLUME_FILE_TYPE] of string = ('Unknown', 'Partition', 'File');


type
  // OK, now *THIS* is *puke* - *every* different TrueCrypt versions use
  // different IDs, so we're stuck with doing this :(
  // Couldn't they just use nice enums in their code, as opposed to #defining
  // everything?!! Ewww!

  // Because of this, we allow the users of this component to "hint" which
  // version of TrueCrypt is installed.
  // If this software is ever in any doubt, this value will be referred to
  TTrueCryptVersionHint = (tcvAuto, tcv21, tcv21a);

const
  // Filesize, in bytes, of different versions of TrueCrypt
  TrueCrypt_EXE_FILESIZES: array [TTrueCryptVersionHint] of integer = (
                                                                0,  // tcvAuto
                                                           360448,  // tcv21
                                                           356352   // tcv21a
                                                                      );


  // This is ALL the different cypher types, in ALL versions of TrueCrypt
  TrueCrypt_CIPHER_NAMES: array [TrueCrypt_CIPHER_TYPE] of string = (
                                                         'None',
                                                         'Blowfish',
                                                         'AES',
                                                         'CAST',
                                                         'IDEA',
                                                         'Triple-DES',
                                                         'DES',
                                                         'Serpent',
                                                         'Twofish',
                                                         '<Unknown>'
                                                        );


  // These are the internal TrueCrypt IDs of the various items
  TrueCrypt_PKCS5_IDS: array [TrueCrypt_PKCS5_TYPE] of integer = (1, 2, $ffff);
  TrueCrypt_PKCS5_NAMES: array [TrueCrypt_PKCS5_TYPE] of string = ('HMAC-SHA-1', 'HMAC-RIPEMD-160', '<Unknown>');

  TrueCrypt_CIPHER_MODE_IDS: array [TrueCrypt_CIPHER_MODE] of integer = (1, 2, 3, $ffff);
  TrueCrypt_CIPHER_MODE_NAMES: array [TrueCrypt_CIPHER_MODE] of string = ('CBC', 'Outer-CBC', 'Inner-CBC', '<Unknown>');


const TrueCrypt_DISKKEY_SIZE = 256;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

TrueCrypt_APP_NAME     = 'TrueCrypt';
TrueCrypt_APP_EXE_NAME = 'TrueCrypt.exe';
//kTrueCryptDriverName // setup when the component is created; depends on the OS
TrueCrypt_SERVICE_NAME = 'TrueCrypt';


TrueCrypt_REGISTRY_EXE_ROOT = HKEY_CLASSES_ROOT;
TrueCrypt_REGISTRY_EXE_PATH = '\TrueCryptVolume\Shell\Open\command';

// Registry keys
TrueCrypt_REGISTRY_SETTINGS_ROOT = HKEY_CURRENT_USER;
TrueCrypt_REGISTRY_SETTINGS_PATH = '\Software\TrueCrypt';

// Options
TrueCrypt_REGISTRY_SETTINGS_PARAM_CACHEPASSWORDSINDRIVER = 'CachePasswordsInDriver';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CACHEPASSWORDSINDRIVER = FALSE;

TrueCrypt_REGISTRY_SETTINGS_PARAM_OPENEXPLORERWINDOWAFTERMOUNT = 'OpenExplorerWindowAfterMount';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_OPENEXPLORERWINDOWAFTERMOUNT = FALSE;

TrueCrypt_REGISTRY_SETTINGS_PARAM_CLOSEEXPLORERWINDOWSONDISMOUNT = 'CloseExplorerWindowsOnDismount';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_CLOSEEXPLORERWINDOWSONDISMOUNT = TRUE;

TrueCrypt_REGISTRY_SETTINGS_PARAM_SAVEMOUNTEVOLUMESHISTORY = 'SaveMountedVolumesHistory';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_SAVEMOUNTEVOLUMESHISTORY = FALSE;

TrueCrypt_REGISTRY_SETTINGS_PARAM_WIPEPASSWORDCACHEONEXIT = 'WipePasswordCacheOnExit';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_WIPEPASSWORDCACHEONEXIT = FALSE;

// Drive Letter
TrueCrypt_REGISTRY_SETTINGS_PARAM_LASTSELECTEDDRIVE = 'LastSelectedDrive';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTSELECTEDDRIVE = '';

// History
TrueCrypt_REGISTRY_SETTINGS_COUNT_LASTMOUNTEVOLUME = 8; // The TrueCrypt application stores the last 8 volume files
TrueCrypt_REGISTRY_SETTINGS_PARAM_PREFIX_LASTMOUNTEVOLUME = 'LastMountedVolume';
TrueCrypt_REGISTRY_SETTINGS_PARAM_DFLT_LASTMOUNTEVOLUME = '';




// TrueCrypt driver name (9x only)
const TrueCrypt_WIN9X_DRIVER_NAME = '\\.\TRUCRYPT';


const TrueCrypt_TC_UNIQUE_ID_PREFIX  = 'TrueCrypt';
const TrueCrypt_TC_MOUNT_PREFIX = '\Device\TrueCryptVolume';

// xxx - under NT these should be "WIDE"?? - see "DRIVER_STR" #define thing
const TrueCrypt_NT_MOUNT_PREFIX  = '\Device\TrueCryptVolume';
const TrueCrypt_NT_ROOT_PREFIX = '\Device\TrueCrypt';
const TrueCrypt_DOS_MOUNT_PREFIX = '\DosDevices\';
const TrueCrypt_DOS_ROOT_PREFIX = '\DosDevices\TrueCrypt';
const TrueCrypt_WIN32_ROOT_PREFIX = '\\.\TrueCrypt';




// TrueCrypt service
// YUCK! In the TrueCrypt source, this is a hardcoded value!
const TrueCrypt_PIPE_SERVICE = '\\.\pipe\truecryptservice';

const TrueCrypt_HDD_PARTITION_DEVICE_NAME_FORMAT = '\Device\Harddisk%d\Partition%d';
const TrueCrypt_HDD_MAX_PHYSICAL_DRIVES = 9; // E4M can only OPEN_TEST a drive with a
                                             // single digit number (0-9)
const TrueCrypt_HDD_MAX_PARTITIONS = 9;  // E4M can only OPEN_TEST a partition
                                         // with a single digit number (1-9)


const TrueCrypt_SIZEOF_MRU_LIST = 8; // 0-7




const TrueCrypt_DWORD_TRUE = 1;
const TrueCrypt_DWORD_FALSE = 0;


// TrueCrypt DeviceIOControl values
const TrueCrypt_IOCTL_MOUNT                = 466944;   // Mount a volume or partition
const TrueCrypt_IOCTL_MOUNT_LIST           = 466948;   // Return list of mounted volumes
const TrueCrypt_IOCTL_OPEN_TEST            = 466952;   // Open a file at ring0
const TrueCrypt_IOCTL_UNMOUNT              = 466956;   // Unmount a volume
const TrueCrypt_IOCTL_WIPE_CACHE           = 466960;   // Wipe the driver password cache
const TrueCrypt_IOCTL_HALT_SYSTEM          = 466964;   // Halt system; (only NT when compiled with debug)
const TrueCrypt_IOCTL_DRIVER_VERSION       = 466968;   // Current driver version
const TrueCrypt_IOCTL_RELEASE_TIME_SLICE   = 466972;   // Release time slice on apps behalf (only Win9x)
const TrueCrypt_IOCTL_MOUNT_LIST_N         = 466976;   // Get volume info from the device (only Win9x)
const TrueCrypt_IOCTL_DISKIO               = 466980;   // Read/Write at ring0 for win32 gui (only Win9x)
const TrueCrypt_IOCTL_ALLOW_FAST_SHUTDOWN  = 466984;   // Fast shutdown under win98 (only Win9x)
const TrueCrypt_IOCTL_CACHE_STATUS         = 466988;   // Get password cache status
const TrueCrypt_IOCTL_VOLUME_PROPERTIES    = 466992;   // Get mounted volume properties

const TrueCrypt_IOCTL_UNMOUNT_PENDING      = 475112;   // Flush the device with this api before sending UNMOUNT


const TrueCrypt_ERR_OS_ERROR                    =  1;
const TrueCrypt_ERR_OUTOFMEMORY                 =  2;
const TrueCrypt_ERR_PASSWORD_WRONG              =  3;
const TrueCrypt_ERR_VOL_FORMAT_BAD              =  4;
const TrueCrypt_ERR_BAD_DRIVE_LETTER            =  5;
const TrueCrypt_ERR_DRIVE_NOT_FOUND             =  6;
const TrueCrypt_ERR_FILES_OPEN                  =  7;
const TrueCrypt_ERR_VOL_SIZE_WRONG              =  8;
const TrueCrypt_ERR_COMPRESSION_NOT_SUPPORTED   =  9;
const TrueCrypt_ERR_PASSWORD_CHANGE_VOL_TYPE    = 10;
const TrueCrypt_ERR_PASSWORD_CHANGE_VOL_VERSION = 11;
const TrueCrypt_ERR_VOL_SEEKING                 = 12;
const TrueCrypt_ERR_VOL_WRITING                 = 13;
const TrueCrypt_ERR_FILES_OPEN_LOCK             = 14;
const TrueCrypt_ERR_VOL_READING                 = 15;
const TrueCrypt_ERR_DRIVER_VERSION		= 16;
const TrueCrypt_ERR_NEW_VERSION_REQUIRED	= 17;

const TrueCrypt_ERR_VOL_ALREADY_MOUNTED         = 32;
const TrueCrypt_ERR_NO_FREE_SLOTS               = 33;
const TrueCrypt_ERR_NO_FREE_DRIVES              = 34;
const TrueCrypt_ERR_FILE_OPEN_FAILED            = 35;
const TrueCrypt_ERR_VOL_MOUNT_FAILED            = 36;
const TrueCrypt_ERR_INVALID_DEVICE              = 37;
const TrueCrypt_ERR_ACCESS_DENIED               = 38;


const TrueCrypt_TC_MAX_PATH  = 260;	// Includes the null terminator

const TrueCrypt_MIN_PASSWORD = 12;
const TrueCrypt_MAX_PASSWORD = 64;


type
  // This is only used by Delphi applications; not the TrueCrypt driver
  TOTFETrueCryptVolumeInfo = record // This is not part of the TrueCrypt source
    volumeFilename: string; // !! WARNING !!
                            // The driver will only ever return up to 64
                            // chars for a volume filename (*YUCK!*).
                            // This is due to a limitation of the TrueCrypt
                            // driver

    mountedAs: char;

    diskLength: int64;

    ciphers: array [0..2] of TrueCrypt_CIPHER_TYPE; // Unused elements will be populated with cphrNone
    cipherNames: string;
    cipherMode: TrueCrypt_CIPHER_MODE;
    cipherModeName: string;

    pkcs5Type: TrueCrypt_PKCS5_TYPE;
    pkcs5TypeName: string;
    pkcs5Iterations: integer;

    volumeLocation: TrueCrypt_VOLUME_FILE_TYPE;
    volumeLocationName: string;

    hidden: boolean;

    volumeCreated: TDateTime;
    passwordChanged: TDateTime;

    readOnly: boolean;
  end;
  pTOTFETrueCryptVolumeInfo = ^TOTFETrueCryptVolumeInfo;


type
  // The following structures are used by the TrueCrypt driver


  // Note: TrueCrypt v2.0 itself uses an $25D sized buffer (checked with
  //       debugger on the running TrueCrypt executable)
  TOTFETrueCrypt_MOUNT_STRUCT_PRE30 = packed record
    nReturnCode: integer;  // Return code back from driver
    wszVolume: array [0..(TrueCrypt_TC_MAX_PATH-1)] of WCHAR;  // Volume to be mounted - -1 since we index from 0
    szPassword: array [0..((TrueCrypt_MAX_PASSWORD+1)-1)] of char;  // User password - -1 since we index from 0
    nPasswordLen: integer;  //  User password length
    bCache: boolean;  // Cache passwords in driver
    junkPadding: array [0..2] of byte;  // junk padding to align with word boundry
    nDosDriveNo: integer;  // Drive number to mount
    time: DWORD; // Time when this volume was mounted
  end;


  // Note: TrueCrypt v3.0a itself uses an $26D sized buffer (checked with
  //       debugger on the running TrueCrypt executable)
  TOTFETrueCrypt_MOUNT_STRUCT_30 = packed record
    nReturnCode: integer;  // Return code back from driver
    wszVolume: array [0..(TrueCrypt_TC_MAX_PATH-1)] of WCHAR;  // Volume to be mounted - -1 since we index from 0
    szPassword: array [0..((TrueCrypt_MAX_PASSWORD+1)-1)] of char;  // User password - -1 since we index from 0
    nPasswordLen: integer;  //  User password length
    bCache: boolean;  // Cache passwords in driver
    junkPadding1: array [0..2] of byte;  // junk padding to align with word boundry
    nDosDriveNo: integer;  // Drive number to mount
    bMountReadOnly: boolean;  // Mount volume in read-only mode
    junkPadding2: array [0..2] of byte;  // junk padding to align with word boundry
    bMountRemovable: boolean;  // Mount volume as removable media
    junkPadding3: array [0..2] of byte;  // junk padding to align with word boundry
    bExclusiveAccess: boolean;  // Open host file/device in exclusive access mode
    junkPadding4: array [0..2] of byte;  // junk padding to align with word boundry
    bMountManager: boolean;  // Announce volume to mount manager
    junkPadding5: array [0..2] of byte;  // junk padding to align with word boundry
    time: DWORD; // Time when this volume was mounted
  end;


  // Structure valid for pre-v3.0 versions of TrueCrypt
  // Note: The TrueCrypt GUI NEVER USES THIS (Though the command line
  //       interface might...). This Delphi component calls the TrueCrypt
  //       service instead via a named pipe, like the TrueCrypt GUI.
  TOTFETrueCrypt_UNMOUNT_STRUCT_PRE30 = packed record
    nReturnCode: integer;  // Return code back from driver
    nDosDriveNo: integer;  // Drive letter to unmount
  end;

  // Structure valid for v3.0 and later versions of TrueCrypt
  // v3.0a *does* use this...
  // Note: TrueCrypt v3.0a itself uses an $C sized buffer (checked with
  //       debugger on the running TrueCrypt executable)
  TOTFETrueCrypt_UNMOUNT_STRUCT_30 = packed record
    nDosDriveNo: integer;  // Drive letter to unmount
    ignoreOpenFiles: boolean;
    junkPadding: array [0..2] of byte;  // junk padding to align with word boundry
    nReturnCode: integer;  // Return code back from driver
  end;

  // Note: TrueCrypt v2.0 itself uses an $E3C sized buffer (checked with
  //       debugger on the running TrueCrypt executable)
  TOTFETrueCrypt_MOUNT_LIST_STRUCT_PRE30 = packed record
    ulMountedDrives: cardinal; // Bitfield of all mounted drive letters
    // Yes, it is 64 in the next line; it was hardcoded in the TrueCrypt source
    // (I would have *expected* it to be "TrueCrypt_TC_MAX_PATH", not 64! Looks
    // like a throwback from E4M - the same was hardcoded there to...
    wszVolume: array [0..26-1] of array [0..64-1] of WCHAR;	 // Volume names of mounted volumes
    diskLength: array [0..26-1] of int64;
    cipher: array [0..26-1] of integer; // -1 because we index from 0, giving 26 array elements
  end;


  // Note: TrueCrypt v3.0a itself uses an $EA4 sized buffer (checked with
  //       debugger on the running TrueCrypt executable)
  TOTFETrueCrypt_MOUNT_LIST_STRUCT_30 = packed record
    ulMountedDrives: cardinal; // Bitfield of all mounted drive letters
    // Yes, it is 64 in the next line; it was hardcoded in the TrueCrypt source
    // (I would have *expected* it to be "TrueCrypt_TC_MAX_PATH", not 64! Looks
    // like a throwback from E4M - the same was hardcoded there to...
    wszVolume: array [0..26-1] of array [0..64-1] of WCHAR;	 // Volume names of mounted volumes
    diskLength: array [0..26-1] of int64;
    ea: array [0..26-1] of integer; // -1 because we index from 0, giving 26 array elements
    hiddenVol: array [0..26-1] of integer; // -1 because we index from 0, giving 26 array elements
                                           // We use integer for this as the
                                           // BOOLs TrueCrypt uses appear to be
                                           // 4 bytes each; probably one byte
                                           // plus 3 padding bytes
  end;

  // Win9x only
  TOTFETrueCrypt_MOUNT_LIST_N_STRUCT = record
    nReturnCode: integer;  // Return code back from driver
    nDosDriveNo: integer;  /// Drive letter to get info on
    wszVolume: array [0..TrueCrypt_TC_MAX_PATH-1] of WCHAR;   // Volume names of mounted volumes
    mountfilehandle: DWORD;  // Device file handle or 0
  end;

  // YES, THIS ONE IS A **PACKED** RECORD
  // If it's not packed, then the diskLength member grows the record by an
  // extra 8 bytes (totalling 176 bytes) - though if you don't pack the record,
  // and you change diskLength to an array of 8 bytes, it works (168 bytes)?!!
  // Structure valid for pre-v3.0 versions of TrueCrypt
  TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_PRE30 = packed record
    driveNo: integer;
    // Yes, it is 64 in the next line; it was hardcoded in the TrueCrypt source
    // (I would have *expected* it to be "TrueCrypt_TC_MAX_PATH", not 64! Looks
    // like a throwback from E4M...
    wszVolume: array [0..64-1] of WCHAR;
//    bs: array [1..8] of byte;
    diskLength: int64;
    cipher: integer;
    pkcs5: integer;
    pkcs5Iterations: integer;
    volumeCreationTime: int64;
    headerCreationTime: int64;
    //int readOnly;  // Commented out in TrueCrypt source for some reason?!
  end;


  // YES, THIS ONE IS A **PACKED** RECORD
  // If it's not packed, then the diskLength member grows the record by an
  // extra 8 bytes (totalling 176 bytes) - though if you don't pack the record,
  // and you change diskLength to an array of 8 bytes, it works (168 bytes)?!!
  // Structure valid for v3.0-v3.1 versions of TrueCrypt
  TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_30 = packed record
    driveNo: integer;
    // Yes, it is 64 in the next line; it was hardcoded in the TrueCrypt source
    // (I would have *expected* it to be "TrueCrypt_TC_MAX_PATH", not 64! Looks
    // like a throwback from E4M...
    wszVolume: array [0..64-1] of WCHAR;
    diskLength: int64;
    ea: integer;
    pkcs5: integer;
    pkcs5Iterations: integer;
    hiddenVolume: boolean;
    junkPadding: array [0..2] of byte;  // junk padding to align with word boundry
    volumeCreationTime: int64;
    headerCreationTime: int64;
    //int readOnly;  // Commented out in TrueCrypt source for some reason?!
  end;


  // YES, THIS ONE IS A **PACKED** RECORD
  // If it's not packed, then the diskLength member grows the record by an
  // extra 8 bytes (totalling 176 bytes) - though if you don't pack the record,
  // and you change diskLength to an array of 8 bytes, it works (168 bytes)?!!
  // Structure valid for v3.1a and later versions of TrueCrypt
  TOTFETrueCrypt_VOLUME_PROPERTIES_STRUCT_31a = packed record
    driveNo: integer;
    // Yes, it is 64 in the next line; it was hardcoded in the TrueCrypt source
    // (I would have *expected* it to be "TrueCrypt_TC_MAX_PATH", not 64! Looks
    // like a throwback from E4M...
    wszVolume: array [0..64-1] of WCHAR;
    diskLength: int64;
    ea: integer;
    pkcs5: integer;
    pkcs5Iterations: integer;
    hiddenVolume: boolean;
    junkPadding1: array [0..2] of byte;  // junk padding to align with word boundry
    readOnly: boolean;
    junkPadding2: array [0..2] of byte;  // junk padding to align with word boundry
    volumeCreationTime: int64;
    headerCreationTime: int64;
    //int readOnly;  // Commented out in TrueCrypt source for some reason?!
  end;



  TOTFETrueCrypt_OPEN_TEST = record
    wszFileName: array [1..TrueCrypt_TC_MAX_PATH] of WCHAR; // Volume to be "open tested"
    nReturnCode: integer;  //Win9x only
    secstart: cardinal;  // Win9x only
    seclast: cardinal;  // Win9x only
    device: cardinal;  // Win9x only
  end;



  TOTFETrueCrypt_VERSION = record // This is not part of the TrueCrypt source; it's just a
                                  // DWORD, but is included for completeness
    version: DWORD;
  end;


implementation


END.


