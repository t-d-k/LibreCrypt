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

#include "FreeOTFEHashSHA.h"

#include "FreeOTFEHashImpl.h"
#include "FreeOTFEHashAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFEHashDriver.h"  // Required for DRIVER_HASH_VERSION
#else
#include "FreeOTFE4PDAHashDriver.h"  // Required for DRIVER_HASH_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include libtomcrypt library...
#include <tomcrypt.h>
#include <tomcrypt_custom.h>


// =========================================================================
// Hash driver init function
// driverInfo - The structure to be initialized
NTSTATUS
ImpHashDriverExtDetailsInit(
    IN OUT HASH_DRIVER_INFO* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;
    int idx = -1;

    DEBUGOUTHASHIMPL(DEBUGLEV_ENTER, (TEXT("ImpHashDriverExtDetailsInit\n")));


    // -- POPULATE DRIVER IDENTIFICATION --
    FREEOTFE_MEMZERO(driverInfo->DriverTitle, sizeof(driverInfo->DriverTitle));
    FREEOTFE_MEMCPY(
                  driverInfo->DriverTitle,
                  DRIVER_TITLE,
                  strlen(DRIVER_TITLE)
                 );

    driverInfo->DriverGUID = DRIVER_GUID;
    driverInfo->DriverVersionID = DRIVER_HASH_VERSION;


    // -- POPULATE HASHES SUPPORTED --
    driverInfo->HashCount = HASHES_SUPPORTED;
    driverInfo->HashDetails = FREEOTFE_MEMCALLOC(
                                                 sizeof(HASH),
                                                 driverInfo->HashCount
                                                );


    // -- SHA-512 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_SHA512,
                  strlen(DRIVER_HASH_TITLE_SHA512)
                 );

    driverInfo->HashDetails[idx].Length = (sha512_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (sha512_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_SHA512;

    // -- SHA-384 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_SHA384,
                  strlen(DRIVER_HASH_TITLE_SHA384)
                 );

    driverInfo->HashDetails[idx].Length = (sha384_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (sha384_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_SHA384;

    // -- SHA-256 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_SHA256,
                  strlen(DRIVER_HASH_TITLE_SHA256)
                 );

    driverInfo->HashDetails[idx].Length = (sha256_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (sha256_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_SHA256;

    // -- SHA-224 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_SHA224,
                  strlen(DRIVER_HASH_TITLE_SHA224)
                 );

    driverInfo->HashDetails[idx].Length = (sha224_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (sha224_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_SHA224;

    // -- SHA-1 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_SHA1,
                  strlen(DRIVER_HASH_TITLE_SHA1)
                 );

    driverInfo->HashDetails[idx].Length = (sha1_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (sha1_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_SHA1;


    DEBUGOUTHASHIMPL(DEBUGLEV_EXIT, (TEXT("ImpHashDriverExtDetailsInit\n")));
    return status;
}


// =========================================================================
// Hash driver cleardown function
// driverInfo - The structure to be cleared down
NTSTATUS
ImpHashDriverExtDetailsCleardown(    
    IN OUT HASH_DRIVER_INFO* driverInfo
)
{
    NTSTATUS status = STATUS_SUCCESS;

    DEBUGOUTHASHIMPL(DEBUGLEV_ENTER, (TEXT("ImpHashDriverExtDetailsCleardown\n")));

    if (driverInfo->HashDetails != NULL)
        {
        FREEOTFE_FREE(driverInfo->HashDetails);
        }

    driverInfo->HashDetails = NULL;
    driverInfo->HashCount = 0;

    DEBUGOUTHASHIMPL(DEBUGLEV_EXIT, (TEXT("ImpHashDriverExtDetailsCleardown\n")));
    return status;
}


// =========================================================================
// Hash function
NTSTATUS
ImpHashHashData(
    IN      GUID* HashGUID,
    IN      unsigned int DataLength,  // In bits
    IN      FREEOTFEBYTE* Data,
    IN OUT  unsigned int* HashLength,  // In bits
    OUT     FREEOTFEBYTE* Hash
)
{
    NTSTATUS status = STATUS_SUCCESS;
    hash_state md;
    WCHAR* tmpGUIDStr;

    DEBUGOUTHASHIMPL(DEBUGLEV_ENTER, (TEXT("ImpHashHashData\n")));
    if (IsEqualGUID(&HASH_GUID_SHA512, HashGUID))
        {
        if (*HashLength < (sha512_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (sha512_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            sha512_desc.init(&md);
            sha512_desc.process(&md, Data, (DataLength / 8));
            sha512_desc.done(&md, Hash);
        
            *HashLength = (sha512_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_SHA384, HashGUID))
        {
        if (*HashLength < (sha384_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (sha384_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            sha384_desc.init(&md);
            sha384_desc.process(&md, Data, (DataLength / 8));
            sha384_desc.done(&md, Hash);
        
            *HashLength = (sha384_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_SHA256, HashGUID))
        {
        if (*HashLength < (sha256_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (sha256_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            sha256_desc.init(&md);
            sha256_desc.process(&md, Data, (DataLength / 8));
            sha256_desc.done(&md, Hash);
        
            *HashLength = (sha256_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_SHA224, HashGUID))
        {
        if (*HashLength < (sha224_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (sha224_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            sha224_desc.init(&md);
            sha224_desc.process(&md, Data, (DataLength / 8));
            sha224_desc.done(&md, Hash);
        
            *HashLength = (sha224_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_SHA1, HashGUID))
        {
        if (*HashLength < (sha1_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (sha1_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            sha1_desc.init(&md);
            sha1_desc.process(&md, Data, (DataLength / 8));
            sha1_desc.done(&md, Hash);
        
            *HashLength = (sha1_desc.hashsize * 8);
            }
            
        }
    else
        {
        DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("Driver doesn't recognise GUID\n")));
        GUIDToWCHAR(HashGUID, &tmpGUIDStr);
        DEBUGOUTHASHIMPL(DEBUGLEV_INFO, (TEXT("Hash passed in: %ls\n"), tmpGUIDStr));
        SecZeroAndFreeWCHARMemory(tmpGUIDStr);
        status = STATUS_INVALID_PARAMETER;
        }
        
    DEBUGOUTHASHIMPL(DEBUGLEV_EXIT, (TEXT("ImpHashHashData\n")));
      
    return status;
}


// =========================================================================
// =========================================================================

