// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEPlatform.h"

// IMPORTANT! initguid must only be included *once* in the application
#ifdef FOTFE_PDA
// INITGUID defined, as per comment in objbase.h
//#define INITGUID 1
// Yes, this next line is needed, even though it's in the .h file.
// Required to be included before initguid.h
#include <objbase.h>
#endif
#include <initguid.h>  // Required for DEFINE_GUID macro

#include "FreeOTFENULLGUID.h"


// =========================================================================
// =========================================================================
