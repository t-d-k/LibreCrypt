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

#include "FreeOTFECypherMARS_Gladman.h"

#include "FreeOTFECypherImpl.h"
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFECypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#else
#include "FreeOTFE4PDACypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include modified libtomcrypt library which allows the use of Gladman 
// cyphers in XTS mode
#include "ltc_gladman_xts.h"


// Include Gladman library...
#include <aes_defs.h>
#include <mars.h>


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


    // -- MARS-256 XTS --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_256_XTS,
                  strlen(DRIVER_CIPHER_TITLE_MARS_256_XTS)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_XTS;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 256;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_256_XTS;
    driverInfo->CypherDetails[idx].KeySizeRequired   = (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);

    // -- MARS-192 XTS --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_192_XTS,
                  strlen(DRIVER_CIPHER_TITLE_MARS_192_XTS)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_XTS;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 192;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_192_XTS;
    driverInfo->CypherDetails[idx].KeySizeRequired   = (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);

    // -- MARS-128 XTS --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_128_XTS,
                  strlen(DRIVER_CIPHER_TITLE_MARS_128_XTS)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_XTS;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 128;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_128_XTS;
    driverInfo->CypherDetails[idx].KeySizeRequired   = (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);


    // -- MARS-256 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_256_CBC,
                  strlen(DRIVER_CIPHER_TITLE_MARS_256_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 256;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_256_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- MARS-192 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_192_CBC,
                  strlen(DRIVER_CIPHER_TITLE_MARS_192_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 192;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_192_CBC;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- MARS-128 CBC --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_MARS_128_CBC,
                  strlen(DRIVER_CIPHER_TITLE_MARS_128_CBC)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 128;
    driverInfo->CypherDetails[idx].BlockSize         = MARS_BLOCK_SIZE;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_MARS_128_CBC;
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
    char tmpBuffer[(MARS_BLOCK_SIZE / 8)];
    int i;
    INTEGER_128 blockID128;
    int errnum;
    MARS(CONTEXT) context;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherEncryptData\n")));

    if (
          (IsEqualGUID(&CIPHER_GUID_MARS_256_XTS, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_192_XTS, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_128_XTS, CypherGUID)) 
         )
        {
        // Generate index in correct format
        // XTS uses:
        //   *) The sector index (i.e. the number of N-bit sectors)
        //   *) The first sector is sector 0
        //   *) Littleendian format
        LARGE_INTEGER__To__INTEGER_128_LittleEndian(
                                            SectorID,
                                            blockID128
                                           );

        if ((errnum = xts_encrypt(
                                  &context,
                                  &(MARS(set_key)),
                                  &(MARS(encrypt)),
                                  ((KeyLength/2)),
                                  Key, 
                                  ((KeyLength/2)),
                                  &(Key[(KeyLength/2)/8]),
                                  PlaintextData, 
                                  PlaintextLength, 
                                  CyphertextData, 
                                  blockID128
                                 )) != CRYPT_OK)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to encrypt block (errnum: %d)\n"), errnum));
            status = STATUS_UNSUCCESSFUL;
            } 

        }
    else if (
          (IsEqualGUID(&CIPHER_GUID_MARS_256_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_192_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_128_CBC, CypherGUID)) 
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
        MARS(set_key)(&context, Key, KeyLength, enc);
        for (i=0; i < PlaintextLength; i += (MARS_BLOCK_SIZE / 8))
            {
            XORBlock(PlaintextData+i, IV, tmpBuffer, (MARS_BLOCK_SIZE / 8));
            MARS(encrypt)(&context, tmpBuffer, CyphertextData+i);
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
    INTEGER_128 blockID128;
    int errnum;
    MARS(CONTEXT) context;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDecryptData\n")));

    if (
          (IsEqualGUID(&CIPHER_GUID_MARS_256_XTS, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_192_XTS, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_128_XTS, CypherGUID)) 
         )
        {
        // Generate index in correct format
        // XTS uses:
        //   *) The sector index (i.e. the number of N-bit sectors)
        //   *) The first sector is sector 0
        //   *) Littleendian format
        LARGE_INTEGER__To__INTEGER_128_LittleEndian(
                                            SectorID,
                                            blockID128
                                           );

        if ((errnum = xts_decrypt(
                              &context,
                              &(MARS(set_key)),
                              &(MARS(encrypt)),
                              &(MARS(decrypt)),
                              ((KeyLength/2)),
                              Key, 
                              ((KeyLength/2)),
                              &(Key[(KeyLength/2)/8]),
                              CyphertextData, 
                              CyphertextLength, 
                              PlaintextData, 
                              blockID128
                             )) != CRYPT_OK)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to decrypt block (errnum: %d)\n"), errnum));
            status = STATUS_UNSUCCESSFUL;
            } 

        }
    else if (
          (IsEqualGUID(&CIPHER_GUID_MARS_256_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_192_CBC, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_MARS_128_CBC, CypherGUID)) 
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
        MARS(set_key)(&context, Key, KeyLength, dec);
        for (i=0; i < CyphertextLength; i += (MARS_BLOCK_SIZE / 8))
            {
            MARS(decrypt)(&context, CyphertextData+i, PlaintextData+i);
            XORBlock(PlaintextData+i, IV, PlaintextData+i, (MARS_BLOCK_SIZE / 8));
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

