// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEBlockCalc_H
#define _FreeOTFEBlockCalc_H   1

#include "FreeOTFEContext.h"

//#include <winnt.h>  // Required for LARGE_INTEGER


// =========================================================================

// Determine block ID, given encryption block size, first sector location, etc
LARGE_INTEGER
GetBlockID(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER fileOffset  // The offset within the *volume file* where
                                       // the data was obtained from
);

// =========================================================================
// =========================================================================

#endif
