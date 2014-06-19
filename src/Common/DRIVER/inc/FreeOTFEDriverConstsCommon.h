// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEDriverConstsCommon_H
#define _FreeOTFEDriverConstsCommon_H   1


// =========================================================================
// Const definitions

// Encrypt in 512 byte "sectors"
// Note: This is SEPARATE to the number of bytes per sector the emulated
//       CD/HDD has
//       e.g. A HDD has HDD_BYTES_PER_SECTOR, a CDROM CD_BYTES_PER_SECTOR  
#define ENCRYPTION_BLOCK_SIZE 512

// Maximum MAC length
// This should be set to the MAXIMUM MAC length that the any MAC may generate.
// Note: This value is in *bits*
#define FREEOTFE_MAX_MAC_LENGTH 1024



// =========================================================================
// =========================================================================

#endif

