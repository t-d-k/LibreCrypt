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

#include "FreeOTFEHashMD.h"

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


    // -- MD5 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_MD5,
                  strlen(DRIVER_HASH_TITLE_MD5)
                 );

    driverInfo->HashDetails[idx].Length = (md5_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (md5_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_MD5;

    // -- MD4 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_MD4,
                  strlen(DRIVER_HASH_TITLE_MD4)
                 );

    driverInfo->HashDetails[idx].Length = (md4_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (md4_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_MD4;

    // -- MD2 --
    idx++;
    FREEOTFE_MEMZERO(driverInfo->HashDetails[idx].Title, MAX_HASH_TITLE);
    FREEOTFE_MEMCPY(
                  driverInfo->HashDetails[idx].Title,
                  DRIVER_HASH_TITLE_MD2,
                  strlen(DRIVER_HASH_TITLE_MD2)
                 );

    driverInfo->HashDetails[idx].Length = (md2_desc.hashsize * 8);
    driverInfo->HashDetails[idx].BlockSize = (md2_desc.blocksize * 8);
    driverInfo->HashDetails[idx].VersionID = DRIVER_HASH_IMPL_VERSION;
    driverInfo->HashDetails[idx].HashGUID = HASH_GUID_MD2;


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
    if (IsEqualGUID(&HASH_GUID_MD5, HashGUID))
        {
        if (*HashLength < (md5_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (md5_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            md5_desc.init(&md);
            md5_desc.process(&md, Data, (DataLength / 8));
            md5_desc.done(&md, Hash);
        
            *HashLength = (md5_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_MD4, HashGUID))
        {
        if (*HashLength < (md4_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (md4_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            md4_desc.init(&md);
            md4_desc.process(&md, Data, (DataLength / 8));
            md4_desc.done(&md, Hash);
        
            *HashLength = (md4_desc.hashsize * 8);
            }
            
        }
    else if (IsEqualGUID(&HASH_GUID_MD2, HashGUID))
        {
        if (*HashLength < (md2_desc.hashsize * 8))
            {
            DEBUGOUTHASHIMPL(DEBUGLEV_ERROR, (TEXT("output hash length buffer too small (got: %d; need: %d)\n"),
                *HashLength,
                (md2_desc.hashsize * 8)
                ));
            status = STATUS_BUFFER_TOO_SMALL;
            }
        else
            {
            md2_desc.init(&md);
            md2_desc.process(&md, Data, (DataLength / 8));
            md2_desc.done(&md, Hash);
        
            *HashLength = (md2_desc.hashsize * 8);
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

