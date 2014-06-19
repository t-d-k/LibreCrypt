// Description: FreeOTFE IV Generation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEGenerateBlockIV_H
#define _FreeOTFEGenerateBlockIV_H   1

#include "FreeOTFEContext.h"

//#include <winnt.h>  // Required for LARGE_INTEGER


// =========================================================================

// Generate block IV
NTSTATUS
GenerateBlockIV(
    IN      DEVICE_CONTEXT* devContext,
    IN      LARGE_INTEGER fileOffset,  // The offset within the *volume file* where
                                       // the data was obtained from
    IN      unsigned int blockIVLength,  // The size of the required IV (and length of
	                                 // "sectorIV"), in bits
    OUT     unsigned char* blockIV
);


// =========================================================================
// =========================================================================

#endif
