// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEDriverlib_H
#define _FreeOTFEDriverlib_H   1


#include <ntddk.h>

// Required for std disk device driver I/O control (IOCTL) codes
#include <ntdddisk.h>


// =========================================================================
// Function headers

// Create device dir
NTSTATUS
CreateDeviceDir(
    IN   PWCHAR DirName,
    OUT  PHANDLE DirHandle
);


NTSTATUS
AppendGUIDToUnicodeString(
    IN      GUID  useGUID,
    IN OUT  PUNICODE_STRING unicodeString
);


// =========================================================================
// =========================================================================

#endif

