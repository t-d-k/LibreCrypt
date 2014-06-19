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

#include "FreeOTFECypherTwofish_HifnCS.h"

#include "FreeOTFECypherImpl.h"
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFECypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#else
#include "FreeOTFE4PDACypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include Hi/fn and Counterpane Systems Twofish library...
#include "AES.H"


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


    // -- Twofish-256 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_TWOFISH_256,
                  strlen(DRIVER_CIPHER_TITLE_TWOFISH_256)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 256;
    driverInfo->CypherDetails[idx].BlockSize         = BLOCK_SIZE;  // From TWOFISH.C
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_TWOFISH_256;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- Twofish-192 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_TWOFISH_192,
                  strlen(DRIVER_CIPHER_TITLE_TWOFISH_192)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 192;
    driverInfo->CypherDetails[idx].BlockSize         = BLOCK_SIZE;  // From TWOFISH.C
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_TWOFISH_192;
    driverInfo->CypherDetails[idx].KeySizeRequired   = driverInfo->CypherDetails[idx].KeySizeUnderlying;

    // -- Twofish-128 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_TWOFISH_128,
                  strlen(DRIVER_CIPHER_TITLE_TWOFISH_128)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_CBC;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = 128;
    driverInfo->CypherDetails[idx].BlockSize         = BLOCK_SIZE;  // From TWOFISH.C
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_TWOFISH_128;
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
    cipherInstance cipher;
    // NOTE: We can't do "keyInstance key;" as this causes a __chkstk error on
    //       building. For that reason, we have to manually allocate the memory
    //       outselves
    keyInstance* key;
    int countBitsProcessed;
    char* asciizIV;
    unsigned int asciizIVLen;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherEncryptData\n")));

    if (!(
		  (IsEqualGUID(&CIPHER_GUID_TWOFISH_128, CypherGUID)) || 
          (IsEqualGUID(&CIPHER_GUID_TWOFISH_192, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_TWOFISH_256, CypherGUID))
		 ))
        {
		DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unsupported cipher GUID passed in.\n")));
        status = STATUS_INVALID_PARAMETER;
        }

    key = FREEOTFE_MEMALLOC(sizeof(keyInstance));    
    FREEOTFE_MEMZERO(key, sizeof(keyInstance));

    FREEOTFE_MEMZERO(&cipher, sizeof(cipherInstance));

    if NT_SUCCESS(status)
        {
        if ( (IV == NULL) && (IVLength != 0) )
            {
            // Sanity check failed...
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Inconsistant: NULL passed as %d bit long IV\n"), IVLength));
            status = STATUS_INVALID_PARAMETER;
            }
        else if ( (IV == NULL) || (IVLength == 0) )
            {
            // Note: The IV is NULL; it's zero's by the FREEOTFE_MEMZERO above
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Using NULL IV\n")));
            asciizIV = NULL;
            }
        else
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Using specific IV\n")));

            // Convert the IV to it's ASCIIZ representation
            // Horrific, but that's the cypher AES API for you...
            asciizIVLen = (IVLength / 4) + 1; // Convert bits into nibbles,
                                              // and add a NULL terminator

            asciizIV = FREEOTFE_MEMALLOC(asciizIVLen);
            ConvertDataToASCIIRep(IVLength, IV, asciizIV);
            }

        }

    if NT_SUCCESS(status)
        {
        if (cipherInit(&cipher, MODE_CBC, asciizIV) != TRUE)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not cipherInit.\n")));
            status = STATUS_INTERNAL_ERROR;
            }
        }


    if NT_SUCCESS(status)
        {
        if (makeKey(key, DIR_DECRYPT, KeyLength, KeyASCII) != TRUE)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not makeKey.\n")));
            status = STATUS_INTERNAL_ERROR;
            }
        }

    if NT_SUCCESS(status)
        {  
        countBitsProcessed = blockEncrypt(&cipher, key, (BYTE*)PlaintextData, (PlaintextLength * 8), (BYTE*)CyphertextData);
        if (countBitsProcessed != (PlaintextLength * 8))
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not encrypt/decrypt all; expected: %d; got: %d\n"), (PlaintextLength * 8), countBitsProcessed));
            status = STATUS_INTERNAL_ERROR;
            }
        }


    // Nuke the key and cipher instance...
    SecZeroMemory(&cipher, sizeof(cipherInstance));

    if (asciizIV != NULL)
        {
        SecZeroMemory(asciizIV, sizeof(asciizIVLen));
        FREEOTFE_FREE(asciizIV);
        }

    SecZeroMemory(key, sizeof(keyInstance));
    FREEOTFE_FREE(key);

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
    cipherInstance cipher;
    // NOTE: We can't do "keyInstance key;" as this causes a __chkstk error on
    //       building. For that reason, we have to manually allocate the memory
    //       outselves
    keyInstance* key;
    int countBitsProcessed;
    char* asciizIV;
    unsigned int asciizIVLen;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDecryptData\n")));

    if (!(
		  (IsEqualGUID(&CIPHER_GUID_TWOFISH_128, CypherGUID)) || 
          (IsEqualGUID(&CIPHER_GUID_TWOFISH_192, CypherGUID)) ||
          (IsEqualGUID(&CIPHER_GUID_TWOFISH_256, CypherGUID))
		 ))
		{
		DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Unsupported cipher GUID passed in.\n")));
        status = STATUS_INVALID_PARAMETER;
        }

    key = FREEOTFE_MEMALLOC(sizeof(keyInstance));    
    FREEOTFE_MEMZERO(key, sizeof(keyInstance));

    FREEOTFE_MEMZERO(&cipher, sizeof(cipherInstance));

    if NT_SUCCESS(status)
        {
        if ( (IV == NULL) && (IVLength != 0) )
            {
            // Sanity check failed...
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Inconsistant: NULL passed as %d bit long IV\n"), IVLength));
            status = STATUS_INVALID_PARAMETER;
            }
        else if ( (IV == NULL) || (IVLength == 0) )
            {
            // Note: The IV is NULL; it's zero's by the FREEOTFE_MEMZERO above
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Using NULL IV\n")));
            asciizIV = NULL;
            }
        else
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Using specific IV\n")));

            // Convert the IV to it's ASCIIZ representation
            // Horrific, but that's the cypher AES API for you...
            asciizIVLen = (IVLength / 4) + 1; // Convert bits into nibbles,
                                              // and add a NULL terminator

            asciizIV = FREEOTFE_MEMALLOC(asciizIVLen);
            ConvertDataToASCIIRep(IVLength, IV, asciizIV);
            }

        }

    if NT_SUCCESS(status)
        {
        if (cipherInit(&cipher, MODE_CBC, asciizIV) != TRUE)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not cipherInit.\n")));
            status = STATUS_INTERNAL_ERROR;
            }
        }


    if NT_SUCCESS(status)
        {
        if (makeKey(key, DIR_DECRYPT, KeyLength, KeyASCII) != TRUE)
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not makeKey.\n")));
            status = STATUS_INTERNAL_ERROR;
            }
        }

    if NT_SUCCESS(status)
        {  
        countBitsProcessed = blockDecrypt(&cipher, key, (BYTE*)CyphertextData, (CyphertextLength * 8), (BYTE*)PlaintextData);
        if (countBitsProcessed != (CyphertextLength * 8))
            {
            DEBUGOUTCYPHERIMPL(DEBUGLEV_ERROR, (TEXT("Could not encrypt/decrypt all; expected: %d; got: %d\n"), (CyphertextLength * 8), countBitsProcessed));
            status = STATUS_INTERNAL_ERROR;
            }
        }


    // Nuke the key and cipher instance...
    SecZeroMemory(&cipher, sizeof(cipherInstance));

    if (asciizIV != NULL)
        {
        SecZeroMemory(asciizIV, sizeof(asciizIVLen));
        FREEOTFE_FREE(asciizIV);
        }

    SecZeroMemory(key, sizeof(keyInstance));
    FREEOTFE_FREE(key);

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDecryptData\n")));

    return status;
}


// =========================================================================
// =========================================================================

