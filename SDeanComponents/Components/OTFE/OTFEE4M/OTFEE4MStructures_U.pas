unit OTFEE4MStructures_U;
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
  EE4MError = Exception;
  EE4MNotConnected = EE4MError;
  EE4MExeNotFound = EE4MError;
  EE4MVxdNotFound = EE4MError;
  EE4MVxdBadStatus = EE4MError;
  EE4MDLLNotFound = EE4MError;
  EE4MBadDLL = EE4MError;
  EE4MDLLFailure = EE4MError;
  EE4MDLLBadSignature = EE4MDLLFailure;


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
  
  E_NOT_CONNECTED = 'E4M not connected - set Active to TRUE first';
  E_BAD_STATUS = 'E4M driver returned bad status';
  E_DRIVE_IN_USE = 'Close all windows and applications used by this drive first';
  E_E4M_EXE_NOT_FOUND = 'The E4M executable could not be found';
  E_E4M_DLL_NOT_FOUND = 'E4M DLL not found';
  E_E4M_BAD_DLL = 'E4M DLL did not have correct facilities!';
  E_E4M_DLL_FAILURE = 'E4M DLL returned unexpected value';
  E_E4M_DLL_SIGNATURE_FAILURE = 'E4M DLL returned bad signature';

  // SFS Volume File Packet IDs
  SFS_PACKET_NONE           = 0; // Null packet
  SFS_PACKET_VOLUMEINFO     = 1; // Volume information
  SFS_PACKET_ENCRINFO       = 2; // Encryption information
  SFS_PACKET_DISK_BPB       = 3; // Filesystem information
  SFS_PACKET_MULTIUSER      = 4; // Multiuser access information
  SFS_PACKET_FASTACCESS     = 5; // Direct disk access information
  SFS_PACKET_UNMOUNT        = 6; // Volume unmount information
  SFS_PACKET_SMARTCARD      = 7; // Smart card information

type
  E4M_VOLUME_FILE_TYPE = (vtidUnknown, vtidSFS, vtidOldE4M, vtidE4M, vtidE4MSDSDevice, vtidScramDisk);

  E4M_CIPHER_TYPE = (cphrNone,
                     cphrMDCSHA,
                     cphrSQUARE,
                     cphrTEA16,
                     cphrTEA32,
                     cphrMISTY1,
                     cphrTRIPLEDES,
                     cphrIDEA,
                     cphrBLOWFISH,
                     cphrDES56,
                     cphrCAST,
                     cphrUnknown);

  E4M_HASH_TYPE = (hashSHA1, hashMD5, hashUnknown);

const

  E4M_OS_WIN_95 = $a;
  E4M_OS_WIN_98 = $b;
  E4M_OS_WIN_NT = $c;


  E4M_VOLUME_FILE_TYPE_NAMES: array [E4M_VOLUME_FILE_TYPE] of Ansistring = ('Unknown', 'SFS', 'OLD E4M', 'E4M', 'E4M/SFS partition', 'ScramDisk'); // ScramDisk is last

  E4M_VOLUME_SIGNATURES: array [E4M_VOLUME_FILE_TYPE] of string = ('<Unknown has no signature>', 'SFS1', 'CAV', 'E4M', '<E4M/SFS partition has no signature>', '<ScramDisk has no signature>');


  // The encryption algorithm IDs
  E4M_CIPHER_IDS: array [E4M_CIPHER_TYPE] of integer = (0,
                                                        1,
                                                        32,
                                                        33,
                                                        34,
                                                        35,
                                                        36,
                                                        37,
                                                        38,
                                                        39,
                                                        40,
                                                        $ffff);

  E4M_CIPHER_NAMES: array [E4M_CIPHER_TYPE] of Ansistring = ('None',
                                                         'MDCSHA',
                                                         'SQUARE',
                                                         'TEA16',
                                                         'TEA32',
                                                         'MISTY1',
                                                         '3DES',
                                                         'IDEA',
                                                         'BLOWFISH',
                                                         'DES',
                                                         'CAST',
                                                         'Unknown');


  E4M_HASH_IDS: array [E4M_HASH_TYPE] of integer = (0, 1, $ffff);
  E4M_HASH_NAMES: array [E4M_HASH_TYPE] of Ansistring = ('SHA1', 'MD5', 'Unknown');

const SFS_DISKKEY_SIZE = 128;
const E4M_DISKKEY_SIZE = 288;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

E4M_APP_NAME     = 'E4M';
E4M_APP_EXE_NAME = 'volmount.exe';
//kE4MDriverName // setup when the component is created; depends on the OS
E4M_SERVICE_NAME = 'E4M';

E4M_REGISTRY_ENTRY_PATH = '\e4m_volume\Shell\Open\command';


// E4M v2.00 driver name (NT only; 9x never supported)
const E4M_NT_MOUNT_PREFIX_v200   = '\Device\E4MV200';
const E4M_NT_ROOT_PREFIX_v200    = '\Device\E4MRootV200';
const E4M_DOS_MOUNT_PREFIX_v200  = '\DosDevices\';
const E4M_DOS_ROOT_PREFIX_v200   = '\DosDevices\E4MRootV200';
const E4M_WIN32_ROOT_PREFIX_v200 = '\\.\E4MRootV200';

// E4M v2.01 driver name (NT only)
const E4M_NT_MOUNT_PREFIX   = '\Device\E4M';
const E4M_NT_ROOT_PREFIX    = '\Device\E4MRoot';
const E4M_DOS_MOUNT_PREFIX  = '\DosDevices\';
const E4M_DOS_ROOT_PREFIX   = '\DosDevices\E4MRoot';
const E4M_WIN32_ROOT_PREFIX = '\\.\E4MRoot';

// E4M v2.01 driver name (9x only)
const E4M_WIN9X_DRIVER_NAME = '\\.\E4M';

// E4M service
const E4M_PIPE_SERVICE = '\\.\pipe\e4mservice';

const E4M_HDD_PARTITION_DEVICE_NAME_FORMAT = '\Device\Harddisk%d\Partition%d';
const MAX_HDD_MAX_PHYSICAL_DRIVES = 9; // E4M can only OPEN_TEST a drive with a
                                       // single digit number (0-9)
const E4M_HDD_MAX_PARTITIONS = 9;      // E4M can only OPEN_TEST a partition
                                       // with a single digit number (1-9)


const E4M_SIZEOF_MRU_LIST = 8; // 0-7

const E4M_INI_FILE = 'e4m.ini';
const E4M_INI_SECTION_LASTRUN = 'LastRun';
const E4M_INI_KEY_NOMOUNTWARNING   = 'no_mount_warning';
const E4M_INI_KEY_NEVERSAVEHISTORY = 'never_save_history';
const E4M_INI_KEY_CACHEINDRIVER    = 'cache_in_driver';
const E4M_INI_KEY_DRIVELETTER      = 'drive_letter';
const E4M_INI_KEY_LASTVOLUMEn      = 'last_volume';


/////////////////////////////////////////////////////////////
// Application to driver (DeviceIoControl) packet definitions
/////////////////////////////////////////////////////////////

const FILE_DEVICE_DISK  = $00000007;
const FILE_DEVICE_FILE_SYSTEM = $00000009;
const IOCTL_DISK_BASE   = FILE_DEVICE_DISK;
const METHOD_BUFFERED   = 0;
const METHOD_IN_DIRECT  = 1;
const FILE_ANY_ACCESS   = 0;

const E4M_DWORD_TRUE = 1;
const E4M_DWORD_FALSE = 0;

//  #const CTL_CODE( DeviceType, Function, Method, Access ) (                 \
//      ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \

// Standard Windows values
const FSCTL_LOCK_VOLUME       =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((6) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 6, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_UNLOCK_VOLUME     =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((7) * $4) OR (METHOD_BUFFERED));

//          = CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 7, METHOD_BUFFERED, FILE_ANY_ACCESS)
const FSCTL_DISMOUNT_VOLUME   =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((8) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 8, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_MOUNT_DBLS_VOLUME =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((13) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(FILE_DEVICE_FILE_SYSTEM,13, METHOD_BUFFERED, FILE_ANY_ACCESS)


// E4M values
const E4M_MOUNT =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($800) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x800, METHOD_BUFFERED, FILE_ANY_ACCESS)

const E4M_MOUNT_LIST =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($801) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x801, METHOD_BUFFERED, FILE_ANY_ACCESS)

// E4M_9x_MOUNT_LIST_N introduced for E4M v2.01
const E4M_9x_MOUNT_LIST_N = 466976; // Get volume info from the device (only Win9x)



const E4M_OPEN_TEST =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($802) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x802, METHOD_BUFFERED, FILE_ANY_ACCESS)

const E4M_UNMOUNT =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($803) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x803, METHOD_BUFFERED, FILE_ANY_ACCESS)

const E4M_WIPE_CACHE =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($804) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x804, METHOD_BUFFERED, FILE_ANY_ACCESS)

{$IFDEF Debug}
const E4M_HALT_SYSTEM =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($805) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x805, METHOD_BUFFERED, FILE_ANY_ACCESS)
{$ENDIF}

const E4M_UNMOUNT_PENDING =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($ffa) * $4) OR (METHOD_BUFFERED));
//          = CTL_CODE(IOCTL_DISK_BASE, 0xffa, METHOD_BUFFERED,	FILE_ANY_ACCESS)

const RELEASE_TIME_SLICE = 466972; // Release time slice on apps behalf
const E4M_DRIVER_VERSION = 466968; // Current driver version; introduced in
                                   // E4M v2.01
const ALLOW_FAST_SHUTDOWN = 466984; // Fast shutdown under win98 (only Win9x);
                                    // introduced in E4M v2.01


const E4M_MAX_PATH = 260; // Includes the null terminator

const E4M_MAX_PASSWORD = 100;


const E4M_ERR_OS_ERROR                    = $7000;
const E4M_ERR_OUTOFMEMOR                  = $700e;
const E4M_ERR_PASSWORD_WRONG              = $8001;
const E4M_ERR_VOLUME_FORMAT_BAD           = $8002;
const E4M_ERR_BAD_DRIVE_LETTER            = $8003;
const E4M_ERR_DRIVE_NOT_FOUND             = $8004;
const E4M_ERR_FILES_OPEN                  = $8005;
const E4M_ERR_VOLUME_SIZE_WRONG           = $8006;
const E4M_ERR_COMPRESSION_NOT_SUPPORTED   = $8007;
const E4M_ERR_PASSWORD_CHANGE_VOL_TYPE    = $8008;
const E4M_ERR_PASSWORD_CHANGE_VOL_VERSION = $8009;
const E4M_ERR_VOL_SEEKING                 = $800a;
const E4M_ERR_VOL_WRITING                 = $800b;
const E4M_ERR_FILES_OPEN_LOCK             = $800c;


type
  TOTFEE4M_200_MOUNT = packed record
    nReturnCode: integer; // Return code back from driver
    wszVolume: array [1..E4M_MAX_PATH] of WCHAR; // Volume to be mounted
    szPassword: array [1..(E4M_MAX_PASSWORD + 1)] of ansichar; // User password or SHA1 hash
    dummy1: array [1..3] of byte; // Needed to get the next item on a word boundry
    nPasswordLen: integer; // User password length
    bCache: boolean;  // Cache passwords in driver
    dummy2: array [1..3] of byte; // Needed to get the next item on a word boundry
    bSD: boolean; // Set to TRUE for ScramDisk volumes, otherwise FALSE
    dummy3: array [1..3] of byte; // Needed to get the next item on a word boundry
    nDosDriveNo: integer; // Drive number to mount
    time: longint; // Time when this volume was mounted
  end;

  TOTFEE4M_201_MOUNT = packed record
    nReturnCode: integer; // Return code back from driver
    wszVolume: array [1..E4M_MAX_PATH] of short; // Volume to be mounted
    szPassword: array [1..(E4M_MAX_PASSWORD + 1)] of ansichar; // User password or SHA1 hash
// it seems that we no longer need dummy1
//  dummy1: array [1..3] of byte; // Needed to get the next item on a word boundry
    nPasswordLen: integer; // User password length
    bCache: boolean;  // Cache passwords in driver
    dummy2: array [1..3] of byte; // Needed to get the next item on a word boundry
    nDosDriveNo: integer; // Drive number to mount
    time: longint; // Time when this volume was mounted
  end;

  TOTFEE4M_UNMOUNT = packed record
    nReturnCode: integer;
    nDosDriveNo: integer; // Drive letter to unmount
  end;

  TOTFEE4M_200_MOUNT_LIST = packed record
    ulMountedDrives: cardinal; // Bitfield of all mounted drive letters
    // Yes, it is 64 in the next line; it was hardcoded in the E4M source
    wszVolume: array [1..26] of array [1..64] of WCHAR; // Volume names of mounted volumes
  end;

  TOTFEE4M_201_MOUNT_LIST = packed record
    ulMountedDrives: cardinal; // Bitfield of all mounted drive letters
    // Yes, it is 64 in the next line; it was hardcoded in the E4M source
    wszVolume: array [1..26] of array [1..64] of short; // Volume names of mounted volumes
  end;

  TOTFEE4M_200_OPEN_TEST = packed record
    wszFileName: array [1..E4M_MAX_PATH] of WCHAR; // Volume to be "open tested"
  end;

  TOTFEE4M_201_OPEN_TEST = packed record
    wszFileName: array [1..E4M_MAX_PATH] of short; // Volume to be "open tested"
    nReturnCode: integer;  // Win9x only
    secstart   : cardinal; // Win9x only
    seclast    : cardinal; // Win9x only
    device     : cardinal; // Win9x only
  end;

  // Win9x only
  TOTFEE4M_9x_MOUNT_LIST_N_STRUCT = packed record
    nReturnCode: integer; // Return code back from driver
    nDosDriveNo: integer; // Drive letter to get info on

    wszVolume: array [1..E4M_MAX_PATH] of short; // Volume names of mounted volumes
    mountfilehandle: cardinal; // Device file handle or 0
  end;

  // This one was introduced in E4M v2.01
  TOTFEE4M_VERSION = packed record // This is not part of the E4M source; it's just a
                               // DWORD, but is included for completeness
    version: DWORD;
  end;

  TOTFEE4MVolumeInfo = packed record // This is not part of the E4M source
    volumeFilename: AnsiString;
    mountedAs: ansichar;
    cipherID: E4M_CIPHER_TYPE;
    cipherName: AnsiString;
    hashID: E4M_HASH_TYPE;
    hashName: AnsiString;
    volumeTypeID: E4M_VOLUME_FILE_TYPE;
    volumeTypeName: AnsiString;
    readonly: boolean;
  end;

  pTOTFEE4MVolumeInfo = ^TOTFEE4MVolumeInfo;

implementation


END.


