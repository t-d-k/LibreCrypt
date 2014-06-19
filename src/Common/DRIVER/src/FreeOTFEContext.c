// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"
#include "FreeOTFEContext.h"
#include "FreeOTFECypherAPICommon.h"


// =========================================================================
// Move contents of CYPHER_v3 struct to CYPHER struct
// ! WARNING !
// This only does a *simple* copy - it doesn't malloc any memory, etc; just
// copies the data over
// *Don't* free off the v1 structure after using this - it'll clobber the v3
// version!
void
MoveMODULE_DETAILS_CYPHER_v1ToMODULE_DETAILS_CYPHER_v3 (
    IN     MODULE_DETAILS_CYPHER_v1*  Source,
    OUT    MODULE_DETAILS_CYPHER_v3*  Dest
)
{
    Dest->DeviceName = Source->DeviceName;
    Dest->CypherGUID = Source->CypherGUID;

    // IV cypher device handle
#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
    Dest->Lib = Source->Lib;
#else
    Dest->FileObject = Source->FileObject;
    Dest->DeviceObject = Source->DeviceObject;
#endif

    CopyCYPHER_v1ToCYPHER_v3(&(Source->Details), &(Dest->Details));
    Dest->FnEncrypt = Source->FnEncrypt;
    Dest->FnDecrypt = Source->FnDecrypt;
    // ! WARNING !
    // The next ones aren't populated for older (pre v3) cypher drivers
    Dest->FnEncryptSector = NULL;
    Dest->FnDecryptSector = NULL;

}


// =========================================================================
// =========================================================================
