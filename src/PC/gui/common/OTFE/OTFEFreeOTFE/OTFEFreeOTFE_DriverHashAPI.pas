unit OTFEFreeOTFE_DriverHashAPI;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows,  // Required for DWORD
  FreeOTFEDriverConsts,
  DriverAPI;  // Required for DEVICE_FREEOTFE_ROOT



// HASH DRIVER RELATED
const
  MAX_HASH_NAME = 256;


type
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PHASH = ^THASH;
  THASH = record
    HashGUID: TGUID;

    Title: array [0..MAX_HASH_NAME-1] of Ansichar;
    VersionID: DWORD;
    Length: DWORD;  // In bits
    BlockSize: DWORD;  // In bits
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_HASH_IDENTIFYDRIVER = ^TDIOC_HASH_IDENTIFYDRIVER;
  TDIOC_HASH_IDENTIFYDRIVER = record
    DriverGUID: TGUID;

    Title: array [0..MAX_HASH_NAME-1] of Ansichar;
    VersionID: DWORD;
    HashCount: DWORD;
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_HASH_IDENTIFYSUPPORTED = ^TDIOC_HASH_IDENTIFYSUPPORTED;
  TDIOC_HASH_IDENTIFYSUPPORTED = record
    BufCount: DWORD;
    Hashes: array [0..0] of THASH; // Variable length array of them
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_HASH_DATA_IN = ^TDIOC_HASH_DATA_IN;
  TDIOC_HASH_DATA_IN = record
    HashGUID: TGUID;
    DataLength: DWORD;  // In bits
    Data: array [0..0] of byte;  // Variable length
  end;

  
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_HASH_DATA_OUT = ^TDIOC_HASH_DATA_OUT;
  TDIOC_HASH_DATA_OUT = record
    HashLength: DWORD;  // In bits
    Hash: array [0..0] of byte;  // Variable length
  end;



const IOCTL_FREEOTFEHASH_IDENTIFYDRIVER =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($800) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFEHASH_IDENTIFYSUPPORTED =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($801) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFEHASH_HASHDATA =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($802) * $4) OR (METHOD_BUFFERED));



const
DEVICE_HASH_BASE_NAME          = '\FreeOTFEHash';

DEVICE_HASH_DIR_NAME           = DEVICE_FREEOTFE_ROOT + '\Hash';
DEVICE_HASH_SYMLINK_DIR_NAME   = DEVICE_SYMLINK_FREEOTFE_ROOT;
DEVICE_HASH_SYMLINK_PREFIX     = DEVICE_HASH_SYMLINK_DIR_NAME + DEVICE_HASH_BASE_NAME;



implementation

END.


