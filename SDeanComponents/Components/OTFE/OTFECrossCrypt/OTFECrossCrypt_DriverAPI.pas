unit OTFECrossCrypt_DriverAPI;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows;


type
  // ------------------------
  // DELPHI FACING STRUCTURES
  // ------------------------

  // !! DANGER !!
  // DO NOT UPDATE WITHOUT ALSO CHANGING BOTH "CROSSCRYPT_CIPHER_IDS" AND
  // "CROSSCRYPT_CIPHER_NAMES"
  TCROSSCRYPT_CIPHER_TYPE = (
                             cphrNone,
                             cphrTwoFish,
                             cphrAES256,
                             cphrAES128,
                             cphrAES192,
                             cphrUnknown
                            );

const
  CROSSCRYPT_CIPHER_NAMES: array [TCROSSCRYPT_CIPHER_TYPE] of string = (
                                                                 'None',
                                                                 'TwoFish',
                                                                 'AES256',
                                                                 'AES128',
                                                                 'AES192',
                                                                 'Unknown'
                                                                );
                                                                
  // The encryption algorithm IDs which must be passed to the driver
  CROSSCRYPT_CIPHER_IDS: array [TCROSSCRYPT_CIPHER_TYPE] of word = (
                                                                    0,
                                                                    1,
                                                                    2,
                                                                    3,
                                                                    4,
                                                                    $ffff
                                                                   );


// #define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
//     ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
// )


// From winioctl.h (part of the MS DDK)
const
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  FILE_DEVICE_UNKNOWN             = $00000022;
  FILE_DEVICE_MASS_STORAGE        = $0000002d;


  FILE_ANY_ACCESS                 = 0;
  FILE_READ_ACCESS                = $0001;    // file & pipe
  FILE_WRITE_ACCESS               = $0002;    // file & pipe

  METHOD_BUFFERED                 = 0;


// Extracted from CrossCrypt's filedisk.c:
// The number of passwords required for a multikey password:
MULTIKEY_PASSWORD_REQUIREMENT = 64;

// From winioctl.h:

const IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;

const
  CROSSCRYPT_REGISTRY_ROOT = HKEY_LOCAL_MACHINE;
  CROSSCRYPT_REGISTRY_KEY_MAIN = 'SYSTEM\CurrentControlSet\Services\FileDisk';
  CROSSCRYPT_REGGISTRY_MAIN_NAME_START = 'Start';


  CROSSCRYPT_REGISTRY_KEY_PARAM = CROSSCRYPT_REGISTRY_KEY_MAIN + '\Parameters';
  CROSSCRYPT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES = 'NumberOfDevices';
    CROSSCRYPT_DFLT_REGGISTRY_PARAM_NAME_NUMEROFDEVICES = 4;



const IOCTL_STORAGE_MEDIA_REMOVAL =
     ((IOCTL_STORAGE_BASE) * $10000) OR ((FILE_READ_ACCESS) * $4000) OR (($0201) * $4) OR (METHOD_BUFFERED);
// #define IOCTL_STORAGE_MEDIA_REMOVAL           CTL_CODE(IOCTL_STORAGE_BASE, 0x0201, METHOD_BUFFERED, FILE_READ_ACCESS)

const IOCTL_STORAGE_EJECT_MEDIA =
     ((IOCTL_STORAGE_BASE) * $10000) OR ((FILE_READ_ACCESS) * $4000) OR (($0202) * $4) OR (METHOD_BUFFERED);
// #define IOCTL_STORAGE_EJECT_MEDIA             CTL_CODE(IOCTL_STORAGE_BASE, 0x0202, METHOD_BUFFERED, FILE_READ_ACCESS)

const FSCTL_LOCK_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((6) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_LOCK_VOLUME               CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  6, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_UNLOCK_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((7) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_UNLOCK_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  7, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_DISMOUNT_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((8) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_DISMOUNT_VOLUME           CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  8, METHOD_BUFFERED, FILE_ANY_ACCESS)



const

DEVICE_BASE_NAME    = '\FileDisk';
DEVICE_DIR_NAME     = '\Device'+DEVICE_BASE_NAME;
DEVICE_NAME_PREFIX  = DEVICE_DIR_NAME+DEVICE_BASE_NAME;

FILE_DEVICE_FILE_DISK       = $8000;


USER_MODE_DEVICE_NAME_PREFIX = '\\.\FileDisk';

MAX_KEYS		= 64;
MAX_KEYHASH		= 32;


// This next one's from CrossCrypt's filedisk.c
MAX_KEY_LENGTH = 500;


// We have these as magic numbers as I was getting an "Overflow in conversion
// or arithmetic operation" when they were expressed in the same way as the
// "FSCTL_..." declarations above
const IOCTL_FILE_DISK_OPEN_FILE = $8000e000;
//#define IOCTL_FILE_DISK_OPEN_FILE   CTL_CODE(FILE_DEVICE_FILE_DISK, 0x800, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)


const IOCTL_FILE_DISK_CLOSE_FILE = $8000e004;
//#define IOCTL_FILE_DISK_CLOSE_FILE  CTL_CODE(FILE_DEVICE_FILE_DISK, 0x801, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)


const IOCTL_FILE_DISK_QUERY_FILE = $80006008;
//#define IOCTL_FILE_DISK_QUERY_FILE  CTL_CODE(FILE_DEVICE_FILE_DISK, 0x802, METHOD_BUFFERED, FILE_READ_ACCESS)




type

  // !! WARNING !!
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  POPEN_FILE_INFORMATION = ^TOPEN_FILE_INFORMATION;
  TOPEN_FILE_INFORMATION = record
    version: word;
    FileSize: LARGE_INTEGER;
    ReadOnly: boolean;
    KeyType: word;  //0 None 1 2Fish 2 AES256
    KeyNum: word;
    KeyLength: word;
    FileNameLength: word;
    Key: array [0..MAX_KEYS-1] of array [0..MAX_KEYHASH-1] of byte;
    DriveLetter: char;
    // This next one uses an artificially long buffer; DON'T USE
    // siezeof(TOPEN_FILE_INFORMATION) when passing one of these structs to the
    // device driver, as the fixed length of this buffer will screw things up
    FileName: array [0..MAX_PATH] of char;
  end;



implementation

END.


