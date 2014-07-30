unit OTFEFreeOTFE_DriverCypherAPI;
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
  OTFEFreeOTFE_DriverCommon,
  OTFEFreeOTFE_DriverAPI,  // Required for DEVICE_FREEOTFE_ROOT
  OTFEFreeOTFE_DriverHashAPI;  // Required for MAX_HASH_LENGTH




// ENCRYPTION DRIVER RELATED
const
  MAX_CYPHER_NAME = 256;


type
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PCYPHER_v1 = ^TCYPHER_v1;
  TCYPHER_v1 = record
    CypherGUID: TGUID;

    Title: array [0..MAX_CYPHER_NAME-1] of Ansichar;
    Mode: integer;  // This must be one of the values stored in FreeOTFECypherModeID
    KeySizeUnderlying: DWORD;  // In bits
    BlockSize: DWORD;  // In bits
    VersionID: DWORD;
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PCYPHER_v3 = ^TCYPHER_v3;
  TCYPHER_v3 = record
    CypherGUID: TGUID;

    Title: array [0..MAX_CYPHER_NAME-1] of Ansichar;
    Mode: integer;  // This must be one of the values stored in FreeOTFECypherModeID
    KeySizeUnderlying: DWORD;  // In bits
    BlockSize: DWORD;  // In bits
    VersionID: DWORD;
    KeySizeRequired: DWORD;  // In bits
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_CYPHER_IDENTIFYDRIVER = ^TDIOC_CYPHER_IDENTIFYDRIVER;
  TDIOC_CYPHER_IDENTIFYDRIVER = record
    DriverGUID: TGUID;

    Title: array [0..MAX_CYPHER_NAME-1] of Ansichar;
    VersionID: DWORD;
    CypherCount: DWORD;
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_CYPHER_IDENTIFYSUPPORTED_v1 = ^TDIOC_CYPHER_IDENTIFYSUPPORTED_v1;
  TDIOC_CYPHER_IDENTIFYSUPPORTED_v1 = record
    BufCount: DWORD;
    Cyphers: array [0..0] of TCYPHER_v1;  // Variable length array of them
  end;
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_CYPHER_IDENTIFYSUPPORTED_v3 = ^TDIOC_CYPHER_IDENTIFYSUPPORTED_v3;
  TDIOC_CYPHER_IDENTIFYSUPPORTED_v3 = record
    BufCount: DWORD;
    Cyphers: array [0..0] of TCYPHER_v3;  // Variable length array of them
  end;


  
  PDIOC_CYPHER_DATA_IN = ^TDIOC_CYPHER_DATA_IN;
  TDIOC_CYPHER_DATA_IN = record
    CypherGUID: TGUID;
    KeyLength: integer;  // In *bits*
    IVLength: integer;  // In *bits*
                   // Note: Not all cyphers use an IV (e.g. in EBC mode)
                   //       If specified though, IVLength, and IV, should
                   //       be equal to the blocksize of the cypher being used
    DataLength: integer;  // In *bytes*
    Key: array [0..0] of Ansichar;  // Variable length
    IV: array [0..0] of Ansichar;  // Variable length
    Data: array [0..0] of Ansichar;  // Variable length
  end;

  PDIOC_CYPHER_SECTOR_DATA_IN = ^TDIOC_CYPHER_SECTOR_DATA_IN;
  TDIOC_CYPHER_SECTOR_DATA_IN = record
    CypherGUID: TGUID;
    SectorID: LARGE_INTEGER;
    SectorSize: integer;
    KeyLength: integer;  // In *bits*
    IVLength: integer;  // In *bits*
                   // Note: Not all cyphers use an IV (e.g. in EBC mode)
                   //       If specified though, IVLength, and IV, should
                   //       be equal to the blocksize of the cypher being used
    DataLength: integer;  // In *bytes*
    Key: array [0..0] of Ansichar;  // Variable length
    IV: array [0..0] of Ansichar;  // Variable length
    Data: array [0..0] of Ansichar;  // Variable length
  end;


  // When used, this buffer must be the same size as the "Data" member
  // of DIOC_CYPHER_DATA_IN
  PDIOC_CYPHER_DATA_OUT = ^TDIOC_CYPHER_DATA_OUT;
  TDIOC_CYPHER_DATA_OUT = record
    Data: array [0..0] of Ansichar;  // Variable length
  end;



const IOCTL_FREEOTFECYPHER_IDENTIFYDRIVER =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($800) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v1 =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($801) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_INTLDETAILS_v1 =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($802) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_ENCRYPT =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($803) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_DECRYPT =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($804) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_ENCRYPTSECTOR =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($805) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_DECRYPTSECTOR =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($806) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_IDENTIFYSUPPORTED_v3 =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($807) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFECYPHER_INTLDETAILS_v3 =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($808) * $4) OR (METHOD_BUFFERED));


const
DEVICE_CYPHER_BASE_NAME          = '\FreeOTFECypher';

DEVICE_CYPHER_DIR_NAME           = DEVICE_FREEOTFE_ROOT + '\Cypher';
DEVICE_CYPHER_SYMLINK_DIR_NAME   = DEVICE_SYMLINK_FREEOTFE_ROOT;
DEVICE_CYPHER_SYMLINK_PREFIX     = DEVICE_CYPHER_SYMLINK_DIR_NAME + DEVICE_CYPHER_BASE_NAME;


                 
implementation

END.


