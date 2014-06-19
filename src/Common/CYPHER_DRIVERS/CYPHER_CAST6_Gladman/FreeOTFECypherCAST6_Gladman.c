// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#endif

#include "FreeOTFECypherCAST6_Gladman.h"

#include "FreeOTFECypherImpl.h"
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFECypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#else
#include "FreeOTFE4PDACypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include Gladman library...
#include <std_defs.h>


// =========================================================================
// Cypher driver init function
// driverInfo - The structure to be initialized
NTSTATUS
ImpCypherDriverExtDetailsInit_v3(
    IN OUT CYPHER_DRIVER_INFO_v3* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;
    int idx = -1;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDriverExtDetailsInit_v3\n")));


    // -- POPULATE DRIVER IDENTIFICATION --
    FREEOTFE_MEMZERO(driverInfo->DriverTitle, sizeof(driverInfo->DriverTitle));
    FREEOTFE_MEMCPY(
                  driverInfo->DriverTitle,
                  DRIVER_TITLE,
                  strlen(DRIVER_TITLE)
                 );

    driverInfo->DriverGUID = DRIVER_GUID;
    driverInfo->DriverVersionID = DRIVER_CYPHER_VERSION;


    // -- POPULATE CYPHERS SUPPORTED --
    driverInfo->CypherCount = CYPHERS_SUPPORTED;
    driverInfo->CypherDetails = FREEOTFE_MEMCALLOC(
                                                   sizeof(CYPHER_v3),
                                                   driverInfo->CypherCount
                                                  );


    // -- CAST6-256 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_CAST6_256_CBC,
                  strlen(DRIVER_CIPHER_TITLE_CAST6_256_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 256;
    driverInfo->CypherDetails[idx].BlockSize         = CAST6_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_CAST6_256_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- CAST6-224 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_CAST6_224_CBC,
                  strlen(DRIVER_CIPHER_TITLE_CAST6_224_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 224;
    driverInfo->CypherDetails[idx].BlockSize         = CAST6_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_CAST6_224_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- CAST6-192 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_CAST6_192_CBC,
                  strlen(DRIVER_CIPHER_TITLE_CAST6_192_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 192;
    driverInfo->CypherDetails[idx].BlockSize         = CAST6_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_CAST6_192_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- CAST6-160 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_CAST6_160_CBC,
                  strlen(DRIVER_CIPHER_TITLE_CAST6_160_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 160;
    driverInfo->CypherDetails[idx].BlockSize         = CAST6_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_CAST6_160_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- CAST6-128 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_CAST6_128_CBC,
                  strlen(DRIVER_CIPHER_TITLE_CAST6_128_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 128;
    driverInfo->CypherDetails[idx].BlockSize         = CAST6_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_CAST6_128_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;


    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDriverExtDetailsInit_v3\n")));

    return status;
}


// =========================================================================
// Cypher driver cleardown function
// driverInfo - The structure to be cleared down
NTSTATUS
ImpCypherDriverExtDetailsCleardown_v3(    
    IN OUT CYPHER_DRIVER_INFO_v3* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDriverExtDetailsCleardown_v3\n")));

    if (driverInfo->CypherDetails != NULL)
        {
        FREEOTFE_FREE(driverInfo->CypherDetails);
        }

    driverInfo->CypherDetails = NULL;
    driverInfo->CypherCount = 0;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDriverExtDetailsCleardown_v3\n")));

    return status;
}


// =========================================================================
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
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
)
{
    NTSTATUS status = STATUS_SUCCESS;
    // Null IV in case we're not given an IV
    char zeroIV[FREEOTFE_MAX_CYPHER_BLOCKSIZE];
    char tmpBuffer[(CAST6_BLOCK_SIZE / 8)];
    int i;
    CONTEXT context;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherEncryptData\n")));

    if (
          (IsEqualGUID(&CIPHER_GUID_CAST6_256_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_224_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_192_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_160_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_128_CBC, CypherGUID)) 
         )
        {
        // Handle situation where a NULL/zero length IV is passed in
        if ( (IVLength == 0) || (IV == NULL) )
            {
            FREEOTFE_MEMZERO(&zeroIV, sizeof(zeroIV));
            IV = (char*)&zeroIV;
            }


        // Process data using CBC mode
        //
        // CBC Mode ENCRYPT:
        // *) Setup key
        // *) For each block...
        //     *) XOR plaintext with IV
        //     *) Encrypt XOR'd plaintext with key
        //     *) IV = The just encrypted data

        // Note: "enc" specified; we're ENCRYPTING
        // Note: We use a temp buffer (tmpBuffer) because we can't write to
        //       PlaintextData
        set_key(&context, (u4byte *)Key, KeyLength);
        for (i=0; i < PlaintextLength; i += (CAST6_BLOCK_SIZE / 8))
            {
            XORBlock(PlaintextData+i, IV, tmpBuffer, (CAST6_BLOCK_SIZE / 8));
            encrypt(&context, (u4byte *)tmpBuffer, (u4byte *)(CyphertextData+i));
            IV = CyphertextData+i;
            }

        SecZeroMemory(tmpBuffer, sizeof(tmpBuffer));
        }
    else
        {
        DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unsupported cipher GUID passed in.\n")));
        status = STATUS_INVALID_PARAMETER;
        }

    SecZeroMemory(&context, sizeof(context));


    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherEncryptData\n")));

    return status;
}


// =========================================================================
// Decryption function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherDecryptSectorData(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,  // Indexed from zero
    IN      int SectorSize, // In bytes
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
)
{
    NTSTATUS status = STATUS_SUCCESS;
    // Null IV in case we're not given an IV
    char zeroIV[FREEOTFE_MAX_CYPHER_BLOCKSIZE];
    int i;
    CONTEXT context;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDecryptData\n")));

    if (
          (IsEqualGUID(&CIPHER_GUID_CAST6_256_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_224_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_192_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_160_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_CAST6_128_CBC, CypherGUID)) 
         )
        {
        // Handle situation where a NULL/zero length IV is passed in
        if ( (IVLength == 0) || (IV == NULL) )
            {
            FREEOTFE_MEMZERO(&zeroIV, sizeof(zeroIV));
            IV = (char*)&zeroIV;
            }


        // Process data using CBC mode
        //
        // CBC Mode DECRYPT:
        // *) Setup key
        // *) For each block...
        //     *) Decrypt cyphertext with key
        //     *) XOR output plaintext with IV
        //     *) IV = Cyphertext data

        // Note: "dec" specified; we're DECRYPTING
        set_key(&context, (u4byte *)Key, KeyLength);
        for (i=0; i < CyphertextLength; i += (CAST6_BLOCK_SIZE / 8))
            {
            decrypt(&context, (u4byte *)(CyphertextData+i), (u4byte *)(PlaintextData+i));
            XORBlock(PlaintextData+i, IV, PlaintextData+i, (CAST6_BLOCK_SIZE / 8));
            IV = CyphertextData+i;
            }

        }
    else
        {
        DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unsupported cipher GUID passed in.\n")));
        status = STATUS_INVALID_PARAMETER;
        }

    SecZeroMemory(&context, sizeof(context));


    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDecryptData\n")));

    return status;
}


// =========================================================================
// =========================================================================

