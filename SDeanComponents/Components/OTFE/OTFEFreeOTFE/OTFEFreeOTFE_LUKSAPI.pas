unit OTFEFreeOTFE_LUKSAPI;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  windows,  // Required for DWORD
  SDUEndianIntegers,
  OTFEFreeOTFE_DriverAPI;

const
  // The LUKS version we support.
  LUKS_VER_SUPPORTED = 1;


  // Size (in bytes) of a LUKS header, as defined by the LUKS specification
  LUKS_HEADER_BYTES = 592;


  LUKS_MAGIC        = 'LUKS'+char($BA)+char($BE);
  LUKS_DIGESTSIZE   = 20;
  LUKS_SALTSIZE     = 32;
  LUKS_NUMKEYS      = 8;
  LUKS_MKD_ITER     = 10;
  LUKS_KEY_DISABLED = $0000DEAD;
  LUKS_KEY_ENABLED  = $00AC71F3;
  LUKS_STRIPES      = 4000;


  LUKS_MAGIC_L       = 6;
  LUKS_CIPHERNAME_L  = 32;
  LUKS_CIPHERMODE_L  = 32;
  LUKS_HASHSPEC_L    = 32;
  UUID_STRING_L      = 40;

  LUKS_SECTOR_SIZE   = 512;  // In *bytes*

type

  // Delphi-facing struct; cleaned up version of the "raw" version
  TLUKSKeySlot = record
    active: boolean;
    iterations: DWORD;
    salt: array [0..(LUKS_SALTSIZE-1)] of byte;
    key_material_offset: DWORD;
    stripes: DWORD;
  end;


  // Raw structure, as it appears within volume files
  TLUKSKeySlotRaw = record
    active             : TSDUBigEndian32;
    iterations         : TSDUBigEndian32;
    salt               : array [0..(LUKS_SALTSIZE-1)] of byte;
    key_material_offset: TSDUBigEndian32;
    stripes            : TSDUBigEndian32;
  end;


  // Delphi-facing struct; cleaned up version of the "raw" version
  TLUKSHeader = record
    magic: string;
    version: WORD;
    cipher_name: string;
    cipher_mode: string;
    hash_spec: string;
    payload_offset: DWORD;
    key_bytes: DWORD;  // In *bytes*
    mk_digest: array [0..(LUKS_DIGESTSIZE-1)] of byte;
    mk_digest_salt: array [0..(LUKS_SALTSIZE-1)] of byte;
    mk_digest_iter: DWORD;
    uuid: TGUID;
    keySlot: array [0..(LUKS_NUMKEYS-1)] of TLUKSKeySlot;  // Note: Indexes from 1

    cypherKernelModeDeviceName: string;
    cypherGUID: TGUID;
    hashKernelModeDeviceName: string;
    hashGUID: TGUID;
    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
    IVHashKernelModeDeviceName: string;
    IVHashGUID: TGUID;
    IVCypherKernelModeDeviceName: string;
    IVCypherGUID: TGUID;
  end;


  // Raw structure, as it appears within volume files
  TLUKSHeaderRaw = record
    magic         : array [0..(LUKS_MAGIC_L-1)] of byte;
    version       : array [0..1] of byte;  // Stored as big endian
    cipher_name   : array [0..(LUKS_CIPHERNAME_L-1)] of char;
    cipher_mode   : array [0..(LUKS_CIPHERMODE_L-1)] of char;
    hash_spec     : array [0..(LUKS_HASHSPEC_L-1)] of char;
    payload_offset: TSDUBigEndian32;
    key_bytes     : TSDUBigEndian32;
    mk_digest     : array [0..(LUKS_DIGESTSIZE-1)] of byte;
    mk_digest_salt: array [0..(LUKS_SALTSIZE-1)] of byte;
    mk_digest_iter: TSDUBigEndian32;
    uuid          : array [0..(UUID_STRING_L-1)] of char;
    keySlot       : array [0..(LUKS_NUMKEYS-1)] of TLUKSKeySlotRaw;  // Note: Indexes from 0
  end;


implementation

END.


