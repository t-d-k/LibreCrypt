// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFENULLGUID_H
#define _FreeOTFENULLGUID_H   1

#include "FreeOTFEPlatform.h"

#ifdef FOTFE_PDA
#include <objbase.h>
#endif
#ifdef FOTFE_PC_DLL
#include <objbase.h>
#endif
#ifdef FOTFE_PC_DRIVER
#include <ntddk.h>
#include <stdio.h>
// Required for std disk device driver I/O control (IOCTL) codes
#include <ntdddisk.h> // xxx -junk
//xxx - junk #include <objbase.h>
#endif

// NULL_GUID defined in std library
DEFINE_GUID(FREEOTFE_NULL_GUID, 0x00000000L, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00);


// =========================================================================
// =========================================================================

#endif

