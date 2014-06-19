// Description: FreeOTFE IV Generation
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

#include "FreeOTFECypherAPICommon.h"


// =========================================================================
// Copy contents of CYPHER_v3 struct to CYPHER_v1 struct
void
CopyCYPHER_v3ToCYPHER_v1(
    IN     CYPHER_v3*  Source,
    OUT    CYPHER_v1*  Dest
)
{
    FREEOTFE_MEMZERO(Dest->Title, sizeof(Dest->Title));
    FREEOTFE_MEMCPY(
                    Dest->Title,
                    Source->Title,
                    strlen(Source->Title)
                   );

    Dest->BlockSize         = Source->BlockSize;
    Dest->VersionID         = Source->VersionID;
    Dest->CypherGUID        = Source->CypherGUID;
    Dest->KeySizeUnderlying = Source->KeySizeRequired;
    Dest->Mode              = Source->Mode;
}

// =========================================================================
// Copy contents of CYPHER_v1 struct to CYPHER_v3 struct
void
CopyCYPHER_v1ToCYPHER_v3(
    IN     CYPHER_v1*  Source,
    OUT    CYPHER_v3*  Dest
)
{
    FREEOTFE_MEMZERO(Dest->Title, sizeof(Dest->Title));
    FREEOTFE_MEMCPY(
                    Dest->Title,
                    Source->Title,
                    strlen(Source->Title)
                   );

    Dest->BlockSize         = Source->BlockSize;
    Dest->VersionID         = Source->VersionID;
    Dest->CypherGUID        = Source->CypherGUID;
    Dest->KeySizeUnderlying = Source->KeySizeUnderlying;
    Dest->Mode              = Source->Mode;

    // Values which don't exist in the earlier struct...
    Dest->KeySizeRequired   = Dest->KeySizeUnderlying;
}


// =========================================================================
// Copy contents of CYPHER_DRIVER_INFO_v3 struct to CYPHER_DRIVER_INFO_v1
// struct
// ! WARNING !
void
CopyCYPHER_DRIVER_INFO_v1ToCYPHER_DRIVER_INFO_v3(
    IN     CYPHER_DRIVER_INFO_v1*  Source,
    OUT    CYPHER_DRIVER_INFO_v3*  Dest
)
{
    unsigned int i;

    Dest->DriverGUID = Source->DriverGUID;

    FREEOTFE_MEMZERO(Dest->DriverTitle, sizeof(Dest->DriverTitle));
    FREEOTFE_MEMCPY(
                    Dest->DriverTitle,
                    Source->DriverTitle,
                    strlen(Source->DriverTitle)
                   );

    Dest->DriverVersionID = Source->DriverVersionID;
    Dest->CypherCount = Source->CypherCount;

    Dest->CypherDetails = FREEOTFE_MEMCALLOC( 
                                             Source->CypherCount,
                                             sizeof(Dest->CypherDetails[0])
                                            );
    for (i = 0; i < Source->CypherCount; i++)
        {
        CopyCYPHER_v1ToCYPHER_v3(
                                 &(Source->CypherDetails[i]),
                                 &(Dest->CypherDetails[i])
                                );
        }
}

// =========================================================================
// =========================================================================
