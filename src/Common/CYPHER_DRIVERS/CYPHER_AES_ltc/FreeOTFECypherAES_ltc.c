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

#include "FreeOTFECypherAES_ltc.h"

#include "FreeOTFECypherImpl.h"
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFECypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#else
#include "FreeOTFE4PDACypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include libtomcrypt library...
#include <tomcrypt.h>
#include <tomcrypt_custom.h>


// =========================================================================
// Forward declarations...

NTSTATUS
DetermineCypherDetails(
    IN    GUID* CypherGUID,
    OUT   int* keySizeUnderlying,
    OUT   CYPHER_MODE* CypherMode
);

NTSTATUS
ImpCypherCryptData(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,  // Indexed from zero
    IN      int SectorSize,  // In bytes
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      BOOLEAN encryptNotDecrypt,  // TRUE = encrypt; FALSE = decrypt
    IN      int InLength,  // In bytes
    IN      FREEOTFEBYTE* InData,
    OUT     FREEOTFEBYTE* OutData
);


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
    int blockSizeBits;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDriverExtDetailsInit_v3\n")));

    blockSizeBits = (aes_desc.block_length * 8);

    // -- POPULATE DRIVER IDENTIFICATION --
    FREEOTFE_MEMZERO(driverInfo->DriverTitle, sizeof(driverInfo->DriverTitle));
    FREEOTFE_MEMCPY(
                  driverInfo->DriverTitle,
                  DRIVER_TITLE,
                  strlen(DRIVER_TITLE)
                 );

    driverInfo->DriverGUID = DRIVER_GUID;
    driverInfo->DriverVersionID = DRIVER_CYPHER_VERSION;


    // -- POPULATE cyphers SUPPORTED --
    driverInfo->CypherCount = CYPHERS_SUPPORTED;
    driverInfo->CypherDetails = FREEOTFE_MEMCALLOC(
                                                   sizeof(CYPHER_v3),
                                                   driverInfo->CypherCount
                                                  );


    // -- AES-256 CBC --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_256_CBC,
                  strlen(DRIVER_CIPHER_TITLE_AES_256_CBC)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_256_CBC;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- AES-256 XTS --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_256_XTS,
                  strlen(DRIVER_CIPHER_TITLE_AES_256_XTS)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_256_XTS;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);

    // -- AES-256 LRW --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_256_LRW,
                  strlen(DRIVER_CIPHER_TITLE_AES_256_LRW)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_256_LRW;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying + 
                            driverInfo->CypherDetails[idx].BlockSize;

    // -- AES-192 CBC --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_192_CBC,
                  strlen(DRIVER_CIPHER_TITLE_AES_192_CBC)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_192_CBC;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- AES-192 XTS --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_192_XTS,
                  strlen(DRIVER_CIPHER_TITLE_AES_192_XTS)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_192_XTS;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);

    // -- AES-192 LRW --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_192_LRW,
                  strlen(DRIVER_CIPHER_TITLE_AES_192_LRW)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_192_LRW;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying + 
                            driverInfo->CypherDetails[idx].BlockSize;

    // -- AES-128 CBC --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_128_CBC,
                  strlen(DRIVER_CIPHER_TITLE_AES_128_CBC)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_128_CBC;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- AES-128 XTS --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_128_XTS,
                  strlen(DRIVER_CIPHER_TITLE_AES_128_XTS)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_128_XTS;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            (2 * driverInfo->CypherDetails[idx].KeySizeUnderlying);

    // -- AES-128 LRW --
    idx++;
    FREEOTFE_MEMZERO(
                     driverInfo->CypherDetails[idx].Title,
                     sizeof(driverInfo->CypherDetails[idx].Title)
                    );
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_AES_128_LRW,
                  strlen(DRIVER_CIPHER_TITLE_AES_128_LRW)
                 );

    driverInfo->CypherDetails[idx].BlockSize  = blockSizeBits;
    driverInfo->CypherDetails[idx].VersionID  = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID = CIPHER_GUID_AES_128_LRW;
    DetermineCypherDetails(
                           &driverInfo->CypherDetails[idx].CypherGUID,
                           &driverInfo->CypherDetails[idx].KeySizeUnderlying,
                           &driverInfo->CypherDetails[idx].Mode
                          );
    driverInfo->CypherDetails[idx].KeySizeRequired = 
                            driverInfo->CypherDetails[idx].KeySizeUnderlying + 
                            driverInfo->CypherDetails[idx].BlockSize;


    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDriverExtDetailsInit_v3\n")));

    return status;
}


// =========================================================================
NTSTATUS
DetermineCypherDetails(
    IN    GUID* CypherGUID,
    OUT   int* KeySizeUnderlying,
    OUT   CYPHER_MODE* CypherMode
)
{
    NTSTATUS status = STATUS_SUCCESS;

    if (IsEqualGUID(&CIPHER_GUID_AES_256_CBC, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_CBC;
        *KeySizeUnderlying = 256;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_256_XTS, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_XTS;
        *KeySizeUnderlying = 256;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_256_LRW, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_LRW;
        *KeySizeUnderlying = 256;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_192_CBC, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_CBC;
        *KeySizeUnderlying = 192;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_192_XTS, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_XTS;
        *KeySizeUnderlying = 192;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_192_LRW, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_LRW;
        *KeySizeUnderlying = 192;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_128_CBC, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_CBC;
        *KeySizeUnderlying = 128;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_128_XTS, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_XTS;
        *KeySizeUnderlying = 128;
        }
    else if (IsEqualGUID(&CIPHER_GUID_AES_128_LRW, CypherGUID))
        {
        *CypherMode = CYPHER_MODE_LRW;
        *KeySizeUnderlying = 128;
        }
    else
        {
		DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unsupported cipher GUID passed in.\n")));
        status = STATUS_INVALID_PARAMETER;
        }

    return status;
}


// =========================================================================
// Cypher driver cleardown function
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
    return ImpCypherCryptData(
                              CypherGUID,
                              SectorID,
                              SectorSize,
                              KeyLength,
                              Key,
                              KeyASCII,
                              IVLength,
                              IV,
                              TRUE,
                              PlaintextLength,
                              PlaintextData,
                              CyphertextData
                             );
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
    return ImpCypherCryptData(
                              CypherGUID,
                              SectorID,
                              SectorSize,
                              KeyLength,
                              Key,
                              KeyASCII,
                              IVLength,
                              IV,
                              FALSE,
                              CyphertextLength,
                              CyphertextData,
                              PlaintextData
                             );
}


// =========================================================================
void
SectorIDToBlockIdx_64Bit(
    IN      LARGE_INTEGER SectorID,  // Indexed from zero
    IN      int SectorSize,  // In bytes
    OUT     LARGE_INTEGER* blockID
)
{
    int blockSizeBytes;

    blockSizeBytes = aes_desc.block_length;

    // +1 because LRW starts block IDs from 1, not 0
    blockID->QuadPart = (
                         (
                          SectorID.QuadPart * (
                                               SectorSize / blockSizeBytes
                                              )
                         ) + 1
                        );
}


// =========================================================================
// Encrypt/Decrypt function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
NTSTATUS
ImpCypherCryptData(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize, // In bytes
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      BOOLEAN encryptNotDecrypt,  // TRUE = encrypt; FALSE = decrypt
    IN      int InLength,  // In bytes
    IN      FREEOTFEBYTE* InData,
    OUT     FREEOTFEBYTE* OutData
)
{
    NTSTATUS status = STATUS_SUCCESS;
    // libtomcrypt can't handle NULL IVs in CBC mode - it ASSERTs that IV != NULL
    char ltcNullIV[FREEOTFE_MAX_CYPHER_BLOCKSIZE];
    int cipher;
    symmetric_CBC *cbc;
    symmetric_LRW *lrw;
    symmetric_xts *xts;
    int errnum;
    CYPHER_MODE mode;
    int keySizeUnderlying;
    LARGE_INTEGER blockID64;
    INTEGER_128 blockID128;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDecryptData\n")));

    status = DetermineCypherDetails(
                                    CypherGUID,
                                    &keySizeUnderlying,
                                    &mode
                                   );

    // libtomcrypt can't handle NULL IVs in CBC mode - it ASSERTs that IV != NULL
    if ( (IVLength == 0) || (IV == NULL) )
        {
        FREEOTFE_MEMZERO(&ltcNullIV, sizeof(ltcNullIV));
        IV = (char*)&ltcNullIV;
        }

    // Sanity check on key supplied
    if NT_SUCCESS(status) 
        {
        switch (mode)
            {
            case CYPHER_MODE_CBC:
                {
                if (KeyLength != keySizeUnderlying)
                    {
                    status = STATUS_INVALID_PARAMETER;
                    }

                break;
                }

            case CYPHER_MODE_XTS:
                {
                if (KeyLength != (2 * keySizeUnderlying))
                    {
                    status = STATUS_INVALID_PARAMETER;
                    }

                break;
                }

            case CYPHER_MODE_LRW:
                {
                if (KeyLength != (keySizeUnderlying + (aes_desc.block_length * 8)))
                    {
                    status = STATUS_INVALID_PARAMETER;
                    }

                break;
                }

            }
        }

    if NT_SUCCESS(status) 
        {
        status = InitLTCCypher(&cipher);
        }

    if NT_SUCCESS(status)
        {
        switch (mode)
            {
            case CYPHER_MODE_CBC:
                {
                cbc = FREEOTFE_MEMALLOC(sizeof(symmetric_CBC));    
                FREEOTFE_MEMZERO(cbc, sizeof(symmetric_CBC));

                // Start a CBC session
                if ((errnum = cbc_start(
                                        cipher, 
                                        IV, 
                                        Key, 
                                        (keySizeUnderlying/8), 
                                        0, 
                                        cbc
                                       )) != CRYPT_OK)
                    {
                    status = STATUS_UNSUCCESSFUL;
                    DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to start CBC session (errnum: %d)\n"), errnum));
                    }
                else
                    {
                    if (encryptNotDecrypt)
                        {
                        if ((errnum = cbc_encrypt(
                                                  InData, 
                                                  OutData, 
                                                  InLength, 
                                                  cbc
                                                 )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to encrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }
                    else
                        {
                        if ((errnum = cbc_decrypt(
                                              InData, 
                                              OutData, 
                                              InLength, 
                                              cbc
                                             )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to decrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }

                    cbc_done(cbc);
                    }

                SecZeroMemory(cbc, sizeof(symmetric_CBC));
                FREEOTFE_FREE(cbc);

                break;
                }

            case CYPHER_MODE_LRW:
                {
                lrw = FREEOTFE_MEMALLOC(sizeof(symmetric_LRW));    
                FREEOTFE_MEMZERO(lrw, sizeof(symmetric_LRW));

                // Generate index in correct format
                // LRW uses:
                //   *) The block index (i.e. the number of 128 bit blocks)
                //   *) The first block has block index 1 - not 0!
                //   *) Bigendian format
                // Note: LTC increments this itself as it processes each block
                SectorIDToBlockIdx_64Bit(SectorID, SectorSize, &blockID64);
                LARGE_INTEGER__To__INTEGER_128_BigEndian(
                                                    blockID64,
                                                    blockID128
                                                   );
                IV = blockID128;

                // Start a LRW session
                if ((errnum = lrw_start(
                                        cipher, 
                                        IV, 
                                        Key, 
                                        (keySizeUnderlying/8), 
                                        // 128 bits tweak key begins after the
                                        // cypher key
                                        (Key + (keySizeUnderlying/8)),
                                        0, 
                                        lrw
                                       )) != CRYPT_OK)
                    {
                    status = STATUS_UNSUCCESSFUL;
                    DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to start LRW session (errnum: %d)\n"), errnum));
                    }
                else
                    {
                    if (encryptNotDecrypt)
                        {
                        if ((errnum = lrw_encrypt(
                                                  InData, 
                                                  OutData, 
                                                  InLength, 
                                                  lrw
                                                 )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to encrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }
                    else 
                        {
                            if ((errnum = lrw_decrypt(
                                                  InData, 
                                                  OutData, 
                                                  InLength, 
                                                  lrw
                                                 )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to decrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }

                    lrw_done(lrw);
                    }

                SecZeroMemory(lrw, sizeof(symmetric_LRW));
                FREEOTFE_FREE(lrw);

                break;
                }

            case CYPHER_MODE_XTS:
                {
                xts = FREEOTFE_MEMALLOC(sizeof(symmetric_xts));    
                FREEOTFE_MEMZERO(xts, sizeof(symmetric_xts));

                // Generate index in correct format
                // XTS uses:
                //   *) The sector index (i.e. the number of N-bit sectors)
                //   *) The first sector is sector 0
                //   *) Littleendian format
                LARGE_INTEGER__To__INTEGER_128_LittleEndian(
                                                    SectorID,
                                                    blockID128
                                                   );

                // Start an XTS session
                if ((errnum = xts_start(
                                        cipher, 
                                        Key, 
                                        &(Key[keySizeUnderlying/8]),
                                        (keySizeUnderlying/8), 
                                        0, 
                                        xts
                                       )) != CRYPT_OK)
                    {
                    status = STATUS_UNSUCCESSFUL;
                    DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to start XTS session (errnum: %d)\n"), errnum));
                    }
                else
                    {
                    if (encryptNotDecrypt)
                        {
                        if ((errnum = xts_encrypt(
                                                  InData, 
                                                  InLength, 
                                                  OutData, 
                                                  blockID128,
                                                  xts
                                                 )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to encrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }
                    else 
                        {
                        if ((errnum = xts_decrypt(
                                              InData, 
                                              InLength, 
                                              OutData, 
                                              blockID128,
                                              xts
                                             )) != CRYPT_OK)
                            {
                            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unable to decrypt block (errnum: %d)\n"), errnum));
                            status = STATUS_UNSUCCESSFUL;
                            } 
                        }

                    xts_done(xts);
                    }

                SecZeroMemory(xts, sizeof(symmetric_xts));
                FREEOTFE_FREE(xts);

                break;
                }

            }

        }

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDecryptData\n")));

    return status;
}


// =========================================================================
// Initialize libtomcrypt cypher
NTSTATUS
InitLTCCypher(
    OUT  int *cipher
)
{
    NTSTATUS status = STATUS_CRYPTO_SYSTEM_INVALID;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("InitLTCCypher\n")));

    // Initialize cipher
    *cipher = register_cipher(&aes_desc);
    if (*cipher == -1)
        {
        DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not register cipher\n")));
        }
    else
        {    
        *cipher = find_cipher("aes");
        if (*cipher == -1)
            {
      	    DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not find cipher\n")));
            }
        else
            {
            status = STATUS_SUCCESS;
            }
        }

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("InitLTCCypher\n")));

    return status;
}


// =========================================================================
// =========================================================================

