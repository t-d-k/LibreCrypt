unit OTFEPGPDiskStructures_U;
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
  // base types
  PGPUInt8 = Byte;
  PGPUInt16 = Word;
  PGPUInt32 = Cardinal;
  PGPUInt64 = array [1..2] of cardinal; // KLUDGE - xxx
  PGPBoolean = Byte; {= can be TRUE(1) or FALSE(0) }

  CRC32 = PGPUInt32;

  EPGPDiskError = Exception;
  EPGPDiskNotConnected = EPGPDiskError;
  EPGPDiskExeNotFound = EPGPDiskError;
  EPGPDiskVxdNotFound = EPGPDiskError;


  TDeviceIOControlRegisters = packed record
    EBX : DWORD;
    EDX : DWORD;
    ECX : DWORD;
    EAX : DWORD;
    EDI : DWORD;
    ESI : DWORD;
    Flags : DWORD;
  end;


  TParamBlock = packed record
    Operation : Integer;
    NumLocks : Integer;
  end;


const
  VWIN32_DIOC_DOS_IOCTL = 1;

  E_NOT_CONNECTED = 'PGPDisk not connected - set Active to TRUE first';
  E_PGPDISK_EXE_NOT_FOUND = 'The PGPDisk executable could not be found';
  E_DRIVE_IN_USE = 'Close all windows and applications used by this drive first';

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

PGPDISK_APP_NAME     = 'PGPdisk';
PGPDISK_DRIVER_NAME  = '\\.\PGPdisk';

// We are emulating v6.0.2, so use this version number
PGPDISK_APP_VERSION  = $60000001;


PGPDISK_MAX_DRIVES            = 26; // (PGPUInt32)
PGPDISK_INVALID_DRIVE         = 255; // invalid drive # (PGPUInt8)
PGPDISK_MAX_STRING_SIZE       = 300; // > MAX_PATH (PGPUInt32)


PGPDISK_BOOL_FALSE = 0;
PGPDISK_BOOL_TRUE  = 1;

/////////////////////////////////////////////////////////////
// Application to driver (DeviceIoControl) packet definitions
/////////////////////////////////////////////////////////////

const METHOD_BUFFERED   = 0;
const FILE_ANY_ACCESS   = 0;
const FILE_DEVICE_DISK  = 7;


// The PGPdisk app is allowed to pass two values to the driver - a control
// code and a pointer.

// The above line is the expanded out version using:
//#define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
//    ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
//)
// ... and ...
//const PGPUInt32 IOCTL_PGPDISK_SENDPACKET = CTL_CODE(FILE_DEVICE_DISK, 0x800,
//      METHOD_BUFFERED, FILE_ANY_ACCESS) | 0x40000000;
const IOCTL_PGPDISK_SENDPACKET : PGPUInt32 =
    ((FILE_DEVICE_DISK * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR (($800) * $4) OR (METHOD_BUFFERED))
    OR $40000000;

const PGPDISK_ADPacketMagic : PGPUInt32 = $820F7353;

PGPDISK_HeaderMagic       = 'PGPd';
PGPDISK_PrimaryHeaderType = 'MAIN';

// Functions codes for packets.
const PGPDISK_AD_Mount            = $0001;
const PGPDISK_AD_Unmount          = $0002;
const PGPDISK_AD_QueryVersion     = $0003;
const PGPDISK_AD_QueryMounted     = $0004;
const PGPDISK_AD_QueryOpenFiles   = $0005;
const PGPDISK_AD_ChangePrefs      = $0006;
const PGPDISK_AD_LockUnlockMem    = $0007;
const PGPDISK_AD_GeTOTFEPGPDiskInfo   = $0008;
const PGPDISK_AD_LockUnlockVol    = $0009;
const PGPDISK_AD_ReadWriteVol     = $000A;
const PGPDISK_AD_QueryVolInfo     = $000B;
const PGPDISK_AD_NotifyUserLogoff = $000C;

type
  PGPDISK_Rec_ADPacketHeader = packed record
    magic: PGPUInt32;  // must be PGPDISK_ADPacketMagic
    code: PGPUInt16;   // packet code
  //  DualErr *pDerr;    // error (DualErr *)
    pDerr: Pointer;
  end;

  PGPDISK_Rec_AD_QueryMounted = packed record
    header: PGPDISK_Rec_ADPacketHeader;     // standard header

    trueIfUsePath: PGPboolean;  // TRUE to use path, FALSE to use drive

    path: PChar;                // file to enquire about
    sizePath: PGPUInt32;        // size of path
    drive: PGPUInt8;            // drive to enquire about

    pIsPGPdisk: ^PGPboolean;    // is a mounted PGPdisk? (PGPBoolean *)
  end;


  PGPDISK_Rec_AD_QueryVersion = packed record
    header : PGPDISK_Rec_ADPacketHeader;     // standard header

    appVersion: PGPUInt32;       // application version
    pDriverVersion: ^PGPUInt32;  // driver version (PGPUInt32 *)
  end;

  {
  PGPDISK_Rec_AD_Mount = packed record
    header: ADPacketHeader;    // standard header

    path: PChar;               // path to the PGPdisk
    sizePath: PGPUInt32;       // size of path

    drive: PGPUInt8;           // preferred drive letter
    readOnly: PGPBoolean;      // mount as read-only?

  CipherContext is a C++ object that (AFAIK) handles all the encryption/decryption
    pContext: ^CipherContext;  // the cipher context
    pDrive: ^PGPUInt8;         // drive mounted on (PGPUInt8 *)
  end;
  }

  PGPDISK_Rec_AD_Unmount = packed record
    header: PGPDISK_Rec_ADPacketHeader;       // standard header

    drive: PGPUInt8;              // drive to unmount
    isThisEmergency: PGPBoolean;  // emergency unmount?
  end;

  PGPDISK_Rec_PGPdiskInfo = packed record
    path: array [1..PGPDISK_MAX_STRING_SIZE] of char;  // path to the PGPdisk

    drive: PGPUInt8;                                   // drive # of a mounted PGPdisk
    sessionId: PGPUInt64;                              // unique session Id
  end;

  PGPDISK_Rec_AD_GetPGPDiskInfo = packed record
    header: PGPDISK_Rec_ADPacketHeader;   // standard header

    arrayElems: PGPUInt32;    // elems in below array
    pPDIArray: ^PGPDISK_Rec_PGPdiskInfo;  // array of kMaxDrives # of structures.
  end;

  PGPDISK_Rec_AD_QueryVolInfo = packed record
    header: PGPDISK_Rec_ADPacketHeader;    // standard header

    drive: PGPUInt8;           // drive number
    pBlockSize: ^PGPUInt16;    // return blocksize here (PGPUInt16 *)
    pTotalBlocks: ^PGPUInt64;  // return total blocks here (PGPUInt64 *)
  end;

  PGPDISK_Rec_AD_QueryOpenFiles = packed record
    header: PGPDISK_Rec_ADPacketHeader;      // standard header

    drive: PGPUInt8;             // drive to enumerate open files on
    pHasOpenFiles: ^PGPBoolean;  // pointer to answer (PGPBoolean *)
  end;

  PGPDISK_Rec_PGPDiskFileHeaderInfo = packed record
    headerMagic: array [1..4] of char;  // Magic field. Always PGPDISK_HeaderMagic
    headerType: array [1..4] of char;   // PGPDISK_PrimaryHeaderType + ???
    headerSize: PGPUInt32;              // Total size of this header, in bytes
    headerCRC: CRC32;                   // CRC of this header
    nextHeaderOffset: PGPUInt64;        // Offset to next header from file start
                                        // 0 = no additional headers
    reserved: array [1..2] of PGPUInt32;
  end;


implementation

END.

