// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _LUKSAPI_H
#define _LUKSAPI_H   1

#include "FreeOTFEAPITypes.h"

// =========================================================================
// Consts...

// The LUKS version we support.
#define  LUKS_VER_SUPPORTED  1


// Size (in bytes) of a LUKS header, as defined by the LUKS specification
#define LUKS_HEADER_BYTES  592


static const BYTE LUKS_MAGIC[6] =  {'L', 'U', 'K', 'S', 0xBA, 0xBE};
#define LUKS_MAGICSIZE     6
#define LUKS_DIGESTSIZE    20
#define LUKS_SALTSIZE      32
#define LUKS_NUMKEYS       8
#define LUKS_MKD_ITER      10
#define LUKS_KEY_DISABLED  0x0000DEAD
#define LUKS_KEY_ENABLED   0x00AC71F3
#define LUKS_STRIPES       4000


#define LUKS_MAGIC_L       6
#define LUKS_CIPHERNAME_L  32
#define LUKS_CIPHERMODE_L  32
#define LUKS_HASHSPEC_L    32
#define UUID_STRING_L      40

// In *bytes*
#define LUKS_SECTOR_SIZE  512


// =========================================================================
// Structs...

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

typedef struct _LUKS_KEY_SLOT {
    DWORD active;
    DWORD iterations;
    BYTE salt[LUKS_SALTSIZE];
    DWORD key_material_offset;
    DWORD stripes;
} LUKS_KEY_SLOT, *PLUKS_KEY_SLOT;

typedef struct _LUKS_HEADER {
    BYTE magic[LUKS_MAGIC_L];
    WORD version;
    char cipher_name[LUKS_CIPHERNAME_L];
    char cipher_mode[LUKS_CIPHERMODE_L];
    char hash_spec[LUKS_HASHSPEC_L];
    DWORD payload_offset;
    DWORD key_bytes;
    BYTE mk_digest[LUKS_DIGESTSIZE];
    BYTE mk_digest_salt[LUKS_SALTSIZE];
    DWORD mk_digest_iter;
    char uuid[UUID_STRING_L];
    LUKS_KEY_SLOT key_slot[LUKS_NUMKEYS];
} LUKS_HEADER, *PLUKS_HEADER;


// Delphi-facing struct; cleaned up version of the "raw" version
typedef struct _LUKS_HEADER_EXT {
    LUKS_HEADER rawHeader;

    WCHAR cypherKernelModeDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID cypherGUID;
    WCHAR hashKernelModeDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID hashGUID;
    SECTOR_IV_GEN_METHOD sectorIVGenMethod;
    WCHAR IVHashKernelModeDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID IVHashGUID;
    WCHAR IVCypherKernelModeDeviceName[FREEOTFE_MAX_FILENAME_LENGTH];
    GUID IVCypherGUID;
} LUKS_HEADER_EXT, *PLUKS_HEADER_EXT;


// =========================================================================
// =========================================================================

#endif

