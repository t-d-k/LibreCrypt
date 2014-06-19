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

#include "FreeOTFECypherXOR.h"

#include "FreeOTFECypherImpl.h"
#include "FreeOTFECypherAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFECypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#else
#include "FreeOTFE4PDACypherDriver.h"  // Required for DRIVER_CYPHER_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


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


    // -- XOR --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->CypherDetails[idx].Title, MAX_CYPHER_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->CypherDetails[idx].Title,
                  DRIVER_CIPHER_TITLE_XOR,
                  strlen(DRIVER_CIPHER_TITLE_XOR)
                 );

    driverInfo->CypherDetails[idx].Mode              = CYPHER_MODE_NONE;
    driverInfo->CypherDetails[idx].KeySizeUnderlying = -1;
    driverInfo->CypherDetails[idx].BlockSize         = -1;
    driverInfo->CypherDetails[idx].VersionID         = DRIVER_CYPHER_IMPL_VERSION;
    driverInfo->CypherDetails[idx].CypherGUID        = CIPHER_GUID_XOR;
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
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
)
{
    NTSTATUS status = STATUS_SUCCESS;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherEncryptData\n")));

    if (!(IsEqualGUID(&CIPHER_GUID_XOR, CypherGUID)))
        {
        status = STATUS_INVALID_PARAMETER;
        }
    else
        {
        if ( (IV == NULL) && (IVLength != 0) )
            {
            // Sanity check failed...
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Inconsistant: NULL passed as %d bit long IV\n"), IVLength));
            status = STATUS_INVALID_PARAMETER;
            }
        else
            {
            _xor_data(KeyLength, Key, PlaintextLength, PlaintextData, CyphertextData);
            }
        }

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
    IN      char *KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
)
{
    NTSTATUS status = STATUS_SUCCESS;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("ImpCypherDecryptData\n")));
    DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("key length = %d; data length = %d\n"), KeyLength, CyphertextLength));

    if (!(IsEqualGUID(&CIPHER_GUID_XOR, CypherGUID)))
        {
        status = STATUS_INVALID_PARAMETER;
        }
    else
        {
        if ( (IV == NULL) && (IVLength != 0) )
            {
            // Sanity check failed...
            DEBUGOUTCYPHERIMPL(DEBUGLEV_INFO, (TEXT("Inconsistant: NULL passed as %d bit long IV\n"), IVLength));
            status = STATUS_INVALID_PARAMETER;
            }
        else
            {
            _xor_data(KeyLength, Key, CyphertextLength, CyphertextData, PlaintextData);
            }
        }

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("ImpCypherDecryptData\n")));

    return status;
}


// =========================================================================
// Internal function to carry out actual XOR encryption
NTSTATUS
_xor_data(
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      int DataLength,  // In bytes
    IN      FREEOTFEBYTE* InData,
    OUT     FREEOTFEBYTE* OutData
)
{
    NTSTATUS status = STATUS_SUCCESS;
    int i;
    int keyOffset;
    int keyLenBytes;

    DEBUGOUTCYPHERIMPL(DEBUGLEV_ENTER, (TEXT("_xor_data\n")));

    keyLenBytes = (KeyLength / 8);
    // Sanity check, just in case we were passed an empty password!
    if (keyLenBytes != 0)
        {
        keyOffset = 0;
        for(i = 0; i < DataLength; i++, keyOffset++)
            {
            if (keyOffset >= keyLenBytes)
                {
                keyOffset = 0;
                }

            OutData[i] = InData[i] ^ Key[keyOffset];
            }
        }
    else
        {
        // No password? Just copy the data from the input buffer to the output
        FREEOTFE_MEMCPY(
                      OutData,
                      InData,
                      DataLength
                     );
        }

    DEBUGOUTCYPHERIMPL(DEBUGLEV_EXIT, (TEXT("_xor_data\n")));

    return status;
}


// =========================================================================
// =========================================================================

