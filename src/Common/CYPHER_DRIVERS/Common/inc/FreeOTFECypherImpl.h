// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


// This file details the functions that the encryption algorithm MUST implement

#ifndef _FreeOTFECypherImpl_H
#define _FreeOTFECypherImpl_H   1

#include "FreeOTFEPlatform.h"

#ifndef FOTFE_PC_DLL  
#include <ntstatus.h>  // Required for NTSTATUS
#endif
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#include <ntverp.h>  // Needed for VER_PRODUCTBUILD
#endif
//xxx - junk - #include "FreeOTFECypherAPI.h"
//xxx - junk - #include "FreeOTFECypherDriver.h"

#if (VER_PRODUCTBUILD < 2600)
#ifndef STATUS_CRYPTO_SYSTEM_INVALID
// Windows XP and later only had STATUS_CRYPTO_SYSTEM_INVALID
// If building for a previous OS, assume it's just an invalid parameter
#define STATUS_CRYPTO_SYSTEM_INVALID STATUS_INVALID_PARAMETER
#endif
#endif


// Block size (128 bit) block index. Hardcoded to 15 here, as that's all LTC
// supports
typedef unsigned char INTEGER_128[15];


// Cypher driver init function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members initialized
NTSTATUS
ImpCypherDriverExtDetailsInit_v1(
    IN OUT  CYPHER_DRIVER_INFO_v1* devExt
);

// Cypher driver init function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members initialized
NTSTATUS
ImpCypherDriverExtDetailsInit_v3(
    IN OUT  CYPHER_DRIVER_INFO_v3* devExt
);

// Cypher driver cleardown function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members cleared down
NTSTATUS
ImpCypherDriverExtDetailsCleardown_v1(
    IN OUT  CYPHER_DRIVER_INFO_v1* devExt
);

// Cypher driver cleardown function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members cleared down
NTSTATUS
ImpCypherDriverExtDetailsCleardown_v3(
    IN OUT  CYPHER_DRIVER_INFO_v3* devExt
);


// Encryption function
// Note: CyphertextLength must be set to the size of the CyphertextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherEncryptData(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
);

// Decryption function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherDecryptData(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
);

// Encryption function
// Note: CyphertextLength must be set to the size of the CyphertextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherEncryptSectorData(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,  // Indexed from zero
    IN      int SectorSize, // In bytes
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
);


// Decryption function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherDecryptSectorData(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize, // In bytes
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
);

// LRW uses big endian
void
LARGE_INTEGER__To__INTEGER_128_BigEndian(
    IN      LARGE_INTEGER i64Value,
    OUT     INTEGER_128 blockIdx
);

// XTS uses little endian
void
LARGE_INTEGER__To__INTEGER_128_LittleEndian(
    IN      LARGE_INTEGER i64Value,
    OUT     INTEGER_128 blockIdx
);


// =========================================================================
// =========================================================================

#endif

