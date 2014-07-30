unit OTFEBestCryptStructures_U;
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
  EBestCryptError = Exception;
  EBestCryptNotConnected = EBestCryptError;
  EBestCryptExeNotFound = EBestCryptError;
  EBestCryptVxdNotFound = EBestCryptError;
  EBestCryptVxdBadStatus = EBestCryptError;
  EBestCryptDLLNotFound = EBestCryptError;
  EBestCryptBadDLL = EBestCryptError;
  EBestCryptDLLFailure = EBestCryptError;
  EBestCryptDLLBadSignature = EBestCryptDLLFailure;

const
  E_NOT_CONNECTED = 'BestCrypt not connected - set Active to TRUE first';
  E_BAD_STATUS = 'BestCrypt driver returned bad status';
  E_DRIVE_IN_USE = 'Close all windows and applications used by this drive first';
  E_BESTCRYPT_EXE_NOT_FOUND = 'The BestCrypt executable could not be found';
  E_BESTCRYPT_DLL_NOT_FOUND = 'BestCrypt DLL not found';
  E_BESTCRYPT_BAD_DLL = 'BestCrypt DLL did not have correct facilities!';
  E_BESTCRYPT_DLL_FAILURE = 'BestCrypt DLL returned unexpected value';
  E_BESTCRYPT_DLL_SIGNATURE_FAILURE = 'BestCrypt DLL returned bad signature';
  E_BESTCRYPT_UNABLE_TO_CREATE_FILENAME_HASH = 'Internal error: Unable to create ASCII coded SHA-1 hash of box filename';


  // BestCrypt Status codes
  BCSTATUS_SUCCESS                 = $00;
  BCSTATUS_ERR_NOSYSTEM_RESOURCES  = $01;
  BCSTATUS_ERR_ALREADYMOUNTED      = $02;
  BCSTATUS_ERR_CONNECTALGORITHM    = $03;
  BCSTATUS_ERR_CREATEDEVICELINK    = $04;
  BCSTATUS_ERR_INVALIDDISKNUMBER   = $05;
  BCSTATUS_ERR_DISK_IS_NOT_MOUNTED = $06;
  BCSTATUS_ERR_INVALIDALGORITHM    = $07;
  BCSTATUS_ERR_CANTOPENCONTAINER   = $08;
  BCSTATUS_ERR_INVALID_KEY         = $10;
  BCSTATUS_ERR_INVALID_KEY_LENGTH  = $11;

  // Key generator error codes
  BCKEYGEN_ERROR_NO                         = $00000000;
  BCKEYGEN_ERROR_INTERNAL_PROBLEM           = $00000001;
  BCKEYGEN_ERROR_INVALID_ALGORITHM          = $00000002;
  BCKEYGEN_ERROR_INCORRECT_CREATE_FLAG      = $00000003;
  BCKEYGEN_ERROR_INCORRECT_FORMAT_VERSION   = $00000004;
  BCKEYGEN_ERROR_INCORRECT_BLOCK_SIGNATURE  = $00000005;
  BCKEYGEN_ERROR_DAMAGED_DATA_BLOCK         = $00000006;
  BCKEYGEN_ERROR_CANCELLED_BY_USER          = $00000007;
  BCKEYGEN_ERROR_USER_PASSWORDS_MISMATCH    = $00000008;
  BCKEYGEN_ERROR_INCORRECT_PASSWORD         = $00000009;
  BCKEYGEN_ERROR_NOT_ENOUGH_INIT_KEYS       = $0000000a;
  BCKEYGEN_ERROR_INCORRECT_ENCR_ALGORITHM   = $0000000b;
  BCKEYGEN_ERROR_INCORRECT_HASH_ALGORITHM   = $0000000c;
  BCKEYGEN_ERROR_INCORRECT_DATA_BLOCK_SIZE  = $0000000d;
  BCKEYGEN_ERROR_NOT_ENOUGH_SPACE_FOR_KEY   = $0000000e;
  BCKEYGEN_ERROR_INCORRECT_POOL             = $0000000f;

  // This comes from email from Sergey Frolov
  USER_ID_SUITABLE_FOR_ALL_USERS = $FFFFFFFF;


  // Minimum driver version for using TGETMASK_BUFFER_driverNEW; drivers older
  // than this use TGETMASK_BUFFER_driverOLD
  // Note:
  DRIVER_VERSION_GETMASK_NEW = $212;  // (Known v2.41 uses new version; v2.14
                                      // used the old version)

  // Minimum driver version for using TDISKINFO_BUFFER_driverNEW; drivers older
  // than this use TDISKINFO_BUFFER_driverOLD
  // Note:
  DRIVER_VERSION_DISKINFO_NEW = $22A;  // (Known v2.42 uses new version; v2.41
                                       // used the old version)



// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

BESTCRYPT_APP_NAME     = 'BestCrypt';
BESTCRYPT_APP_EXE_NAME = 'BestCrypt.exe';
BESTCRYPT_SERVICE_NAME = 'BestCrypt';
BESTCRYPT_INI_FILENAME = 'BESTCRYPT.INI';

BESTCRYPT_NT_DRIVER   = '\\.\BCDISK';
BESTCRYPT_9598_DRIVER = '\\.\BCRYPT.VXD';
BESTCRYPT_2KXP_DRIVER = '\\.\bcbus';       // Only v3.00 drivers onwards(?);
                                           // earlier versions use
                                           // BESTCRYPT_9598_DRIVER under
                                           // Windows 2000/XP

BESTCRYPT_REGKEY_BESTCRYPT = 'SOFTWARE\Jetico\BestCrypt';

const BESTCRYPT_UNUSED_ALGORITHM_ID = $ff;

// MAX_KEY_LENGTH value in bites
const BESTCRYPT_MAX_KEY_LENGTH      = 512;

// Pool of random bytes
const BESTCRYPT_POOL_SIZE_BYTES     = 512;


/////////////////////////////////////////////////////////////
// Application to driver (DeviceIoControl) packet definitions
/////////////////////////////////////////////////////////////

const FILE_DEVICE_DISK  = $00000007;
const IOCTL_DISK_BASE   = FILE_DEVICE_DISK;
const METHOD_BUFFERED   = 0;
const METHOD_IN_DIRECT  = 1;
const FILE_ANY_ACCESS   = 0;

const BESTCRYPT_DWORD_TRUE = 1;
const BESTCRYPT_DWORD_FALSE = 0;

const FILE_DEVICE_FILE_SYSTEM = 9;

const DISKINFO_BUFFER_filename_SIZE = 256;


//  #define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
//      ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \


// STANDARD WINDOWS DeviceIOControl parameters
const FSCTL_LOCK_VOLUME =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($6) * $4) OR (FILE_ANY_ACCESS));
// #define FSCTL_LOCK_VOLUME               CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  6, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_UNLOCK_VOLUME =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($7) * $4) OR (FILE_ANY_ACCESS));
// #define FSCTL_UNLOCK_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  7, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_DISMOUNT_VOLUME =
         (((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($8) * $4) OR (FILE_ANY_ACCESS));
// #define FSCTL_DISMOUNT_VOLUME           CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  8, METHOD_BUFFERED, FILE_ANY_ACCESS)


// BESTCRYPT DeviceIOControl parameters
const IOCTL_PRIVATE_GET_VERSION_INFO =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($800) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x800, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_GET_BCDISK_MASK =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($801) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x801, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_GET_BCDISK_INFO =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($802) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x802, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_MAP_BCDISK =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($803) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x803, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_DISCONNECT_BCDISK =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($804) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x804, METHOD_IN_DIRECT, FILE_ANY_ACCESS)




// BestCrypt's BDK documentation is wrong - This is not actually supported by
// the driver!
const IOCTL_PRIVATE_DISCONNECT_ALL =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($805) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x805, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_CHANGE_SYMBOLIC_LINK =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($807) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x807, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_CHANGE_NT_SECURITY =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($808) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x808, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_CREATE_KEY_HANDLE =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($810) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x810, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_TEST_KEY_HANDLE =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($811) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x811, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_FREE_KEY_HANDLE =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($812) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x812, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_ENCRYPT =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($814) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x814, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_DECRYPT =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($815) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x815, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_GETFUNCTIONS =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($816) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x816, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_PRIVATE_GET_AND_RESET_FLAG =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($855) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x855, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

const IOCTL_FSH_REFRESH_HOOK =
         (((IOCTL_DISK_BASE) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($820) * $4) OR (METHOD_IN_DIRECT));
//          = CTL_CODE(IOCTL_DISK_BASE, 0x820, METHOD_IN_DIRECT, FILE_ANY_ACCESS)

type

  KEY_HANDLE = DWORD;

  TVERSION_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    Major_version: UCHAR;
    Minor_version: UCHAR;
  end;

  TGETMASK_BUFFER_driverOLD = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    Mask: DWORD;
  end;

  TGETMASK_BUFFER_driverNEW = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    Mask: DWORD;
    //  Mask of the drives available for transparent ecryption.
    //  To set transparent ecryption we will use same command as for usual mount
    //  but for bcrmpart.vxd (or .sys) "BestCrypt Removable Partitions"
    hostsMask: DWORD;
  end;

  TDISKINFO_BUFFER_driverOLD = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    filename: array [1..DISKINFO_BUFFER_filename_SIZE] of char;
    diskNumber: DWORD;
    diskSizeLow: DWORD;
    diskSizeHigh: DWORD;
    keyHandle: DWORD;
    algDevice: array [1..128] of char;
    algorithmID: DWORD;
    readOnly: DWORD;
    process: THandle;
    thread: THandle;
    sectorSize: DWORD;
    fullDismounted: boolean;
    dummy2: array [1..3] of UCHAR;   // word boundry padding
    dataOffset: DWORD;     //in bytes
  end;


  TDISKINFO_BUFFER_driverNEW = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    filename: array [1..DISKINFO_BUFFER_filename_SIZE] of char;
    diskNumber: DWORD;
    diskSizeLow: DWORD;
    diskSizeHigh: DWORD;
    keyHandle: DWORD;
    algDevice: array [1..128] of char;
    algorithmID: DWORD;
    readOnly: DWORD;
    process: THandle;
    thread: THandle;
    sectorSize: DWORD;
    fullDismounted: boolean;
    dummy2: array [1..3] of UCHAR;   // word boundry padding
    dataOffset: DWORD;     //in bytes
    UserId: DWORD;                // Ser added 25-Apr-2003
    HarddiskVolumeNumber: DWORD;  // Ser added 25-Apr-2003
  end;


  TDISCONNECT_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;   // word boundry padding
    DiskNumber: DWORD;

//    dummy2: UCHAR;  // word boundry padding                    \
//    FullDismounted: boolean;                            //      > The C "BOOL" type is 4 bytes long
//    dummy3: array [1..2] of UCHAR; // word boundry padding     /

    FullDismounted: boolean;                            //     \ The C "BOOL" type is 4 bytes long
    dummy2: array [1..3] of UCHAR; // word boundry padding     /

//    the above was changed - previously "fulldismounted" was prefixed with one of the UCHARs now in in dummy2
    
  end;

  TTESTKEY_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    IsValid: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    KeyHandle: KEY_HANDLE;
  end;

  TENCRYPT_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    KeyHandle: KEY_HANDLE;
    IVector: array [1..8] of UCHAR;
    Length: DWORD;  // in bytes
  end;

  // This one is for NT only...
  TGETFUNC_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    FuncTestKey: Pointer;
    FuncFreeKey: Pointer;
    FuncEncrypt: Pointer;
    FuncDecrypt: Pointer;
  end;

  TFLAG_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    Flag: DWORD;
  end;

  TREFRESH_BUFFER = packed record
    Signature: array [1..8] of char; // input: "LOCOS97 "; output: "LS06CXX "
    Status: UCHAR;
    dummy: array [1..3] of UCHAR;  // word boundry padding
    HookMask: ULONG;
  end;


// Container file format types

const HIDDEN_SECTOR_SIZE = 512;
const DESCRIPTION_SIZE   = 66;
const CHECKSUM_SIZE      = 8;

const BESTCRYPT_VOLUME_DESCRIPTOR = 'LOCOS94 ';

type
  TBPB = packed record
    sectSize: WORD;
    sectPerCluster: byte;
    reservedSectors: WORD;
    NumberOfFat: byte;
    maxRootDirEntry: WORD;
    totalSectors: WORD;
    mediaDesc: BYTE;
    sectorsPerFat: WORD;
    sectorsPerTrack: WORD;
    numberOfHeads: WORD;
    hiddenSectors: DWORD;

    totalSectorsLong: DWORD;
  end; // The same as the DOS Bios Parameter Block

  TBootRecord = packed record
    jmpCode: array [1..3] of byte;
    OEMid: array [1..8] of char;
    bpb: TBPB;
    driveNo: byte;
    reserved: byte;
    extBootSign: byte;
    serialNumber: DWORD;
    volumeLabel: array [1..11] of char;
    FatType: array [1..8] of char;
  end; // The same as DOS Boot Record structure for FAT12 and FAT16


  THiddenSector = packed record
    bootRecord: TBootRecord;
    description: array [1..DESCRIPTION_SIZE] of char; // Description of the file-container

    extent: WORD;                          // 0 (reserved for future)
    version: WORD;                         // 0 (reserved for future)
    reserved: array [1.. (HIDDEN_SECTOR_SIZE  -
                          sizeof(TBootRecord) -
                          DESCRIPTION_SIZE -     // sizeof(description)
                          sizeof(WORD)   -     // sizeof(extent)
                          sizeof(WORD)   -     // sizeof(version)
                          sizeof(DWORD)  -     // sizeof(dwKeySize)

                          sizeof(DWORD)  -     // sizeof(dwDataOffset)
                          sizeof(DWORD)  -     // sizeof(fileSystemId)
                          sizeof(DWORD)  -     // sizeof(algorithmId)
                          sizeof(DWORD)  -     // sizeof(keyGenId)
                          CHECKSUM_SIZE)] of byte;

    dwKeySize: DWORD;    // Key Data Block size.
    dwDataOffset: DWORD; // Encrypted Data offset from the beginning of file in bytes
    fileSystemId: DWORD; // Driver will mark container during formating

    algorithmId: DWORD;  // Encryption Algorithm identifier
    keyGenId: DWORD;     // Key Generation identifier
    CheckSum: array [1..CHECKSUM_SIZE] of char; // Not used in version 6 of BestCrypt
  end;

  pDWORD = ^DWORD;
  pbyte = ^byte;
  ppByte = ^pByte;

const CFLAG_CREATE_WITH_SINGLE_PASSWORD = $00000001;
const CFLAG_VERIFY_AND_LOAD_KEY         = $00000002;
const CFLAG_CHANGE_PASSWORD             = $00000003;

const MAX_ALGORITHM_DEVICE_NAME_LENGTH = 128; // Max length of the algorithm's name
const MAX_VOLUME_FILENAME_LENGTH = 256; // Max length of a volume's filename
const MAX_KEYBLOCK_SIZE = 5012; // Max size of the keyblock held in a container
const MAX_KEYGEN_NAME_LENGTH = 128; // Max length of the algorithm's name

const INPUT_SIGNATURE  = 'LOCOS97 ';
const OUTPUT_SIGNATURE = 'LS06CXX ';

const BC_FILE_ERRORS_IN_MOUNTING_OFFSET = $88;

type
  TBCDiskInfo = class(TObject) // This is not part of the BestCrypt source
    volumeFilename: AnsiString;
    mountedAs: Ansichar;
    algorithmID: integer;
    algorithmName: AnsiString;
    algorithmKeyLen: integer;
    keyGenID: integer;
    keyGenName: AnsiString;
    readOnly: boolean;
    description: AnsiString;
    errsInMounting: integer;
    extent: cardinal;
    version: cardinal;
    fileSystemID: cardinal;
  end;

implementation


END.


