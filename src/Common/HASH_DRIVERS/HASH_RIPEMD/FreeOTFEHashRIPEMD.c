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

#include "FreeOTFEHashRIPEMD.h"

#include "FreeOTFEHashImpl.h"
#include "FreeOTFEHashAPICommon.h"

#ifdef FOTFE_PC_DRIVER
#include "FreeOTFEHashDriver.h"  // Required for DRIVER_HASH_VERSION
#else
#include "FreeOTFE4PDAHashDriver.h"  // Required for DRIVER_HASH_VERSION
#endif

#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"


// Include libtomcrypt libraries...
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


    // -- RIPEMD-320 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_RMD320,
                  strlen(DRIVER_HASH_TITLE_RMD320)
                 );

    driverInfo->HashDetails[idx].Length = (rmd320_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (rmd320_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_RMD320;

    // -- RIPEMD-256 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_RMD256,
                  strlen(DRIVER_HASH_TITLE_RMD256)
                 );

    driverInfo->HashDetails[idx].Length = (rmd256_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (rmd256_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_RMD256;

    // -- RIPEMD-160 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_RMD160,
                  strlen(DRIVER_HASH_TITLE_RMD160)
                 );

    driverInfo->HashDetails[idx].Length = (rmd160_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (rmd160_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_RMD160;

    // -- RIPEMD-160, twice with A; Linux --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_RMD160_2xA_LINUX,
                  strlen(DRIVER_HASH_TITLE_RMD160_2xA_LINUX)
                 );

    driverInfo->HashDetails[idx].Length = (rmd160_desc.hashsize * 8) * 2;
    driverInfo->HashDetails[idx].BlockSize = (rmd160_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_RMD160_2xA_LINUX;

    // -- RIPEMD-128 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_RMD128,
                  strlen(DRIVER_HASH_TITLE_RMD128)
                 );

    driverInfo->HashDetails[idx].Length = (rmd128_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (rmd128_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_RMD128;


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
    unsigned int ile;
    unsigned char pwdCopy[130];
    WCHAR* tmpGUIDStr;

    DEBUGOUTHASHIMPL(DEBUGLEV_ENTER, (TEXT("ImpHashHashData\n")));
    if (IsEqualGUID(&HASH_GUID_RMD320, HashGUID))
        {
        if (*HashLength < (rmd320_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (rmd320_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            rmd320_desc.init(&md);
            rmd320_desc.process(&md, Data, (DataLength / 8));
            rmd320_desc.done(&md, Hash);
        
            *HashLength = (rmd320_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_RMD256, HashGUID))
        {
        if (*HashLength < (rmd256_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (rmd256_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            rmd256_desc.init(&md);
            rmd256_desc.process(&md, Data, (DataLength / 8));
            rmd256_desc.done(&md, Hash);
        
            *HashLength = (rmd256_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_RMD160, HashGUID))
        {
        if (*HashLength < (rmd160_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (rmd160_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            rmd160_desc.init(&md);
            rmd160_desc.process(&md, Data, (DataLength / 8));
            rmd160_desc.done(&md, Hash);
        
            *HashLength = (rmd160_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_RMD128, HashGUID))
        {
        if (*HashLength < (rmd128_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (rmd128_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            rmd128_desc.init(&md);
            rmd128_desc.process(&md, Data, (DataLength / 8));
            rmd128_desc.done(&md, Hash);
        
            *HashLength = (rmd128_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_RMD160_2xA_LINUX, HashGUID))
        {
        if (*HashLength < ((rmd160_desc.hashsize * 8) * 2))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                ((rmd160_desc.hashsize * 8) * 2)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            rmd160_desc.init(&md);
            rmd160_desc.process(&md, Data, (DataLength / 8));
            rmd160_desc.done(&md, Hash);

            pwdCopy[0] = 'A';

            ile = (DataLength / 8);
            if (ile > sizeof(pwdCopy) - 1) 
                {
                ile = sizeof(pwdCopy) - 1;
                }

            FREEOTFE_MEMCPY(
                          pwdCopy + 1,
                          Data,
                          ile
                         );

            rmd160_desc.init(&md);
            rmd160_desc.process(&md, pwdCopy, (ile + 1));
            rmd160_desc.done(&md, &Hash[20]);

            *HashLength = ((rmd160_desc.hashsize * 8) * 2);
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

