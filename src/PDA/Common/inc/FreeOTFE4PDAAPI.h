// Description: FreeOTFE Device Driver API
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDAAPI_H
#define _FreeOTFE4PDAAPI_H   1


// Required for definition of ULONG
#include <windef.h>

#include "FreeOTFEAPITypes.h"


// =========================================================================
// Const definitions

#define FREEOTFE_BYTES_PER_SECTOR 512

// Name which may not be used as the mountpoint
// (From help IOCTL_DISK_GETNAME other language systems may have a
// different reserved name)
#define RESERVED_MOUNTPOINT           TEXT("Storage Card")
// Characters that may not appear in a mountpoint
#define RESERVED_MOUNTPOINT_CHARS     TEXT("\\/*?\"<>|")

#define FAILED_GETFILEATTRIBUTES  0xFFFFFFFF
#define FAILED_SETFILEPOINTER     0xFFFFFFFF

// Min volume file size; in MB
#define MIN_VOLUME_SIZE           2
// Max is arbitary
#define MAX_VOLUME_SIZE           9999  

// INI filenames
#define FREEOTFE_INI                 TEXT("FreeOTFE4PDA.ini")

// DLL filenames
#define DRIVER_SUBDIR                TEXT("dll\\")
#define FREEOTFE_DLL_MAIN            DRIVER_SUBDIR TEXT("FreeOTFE4PDA.dll")
#define FREEOTFE_DLL_PATTERN_CYPHER  DRIVER_SUBDIR TEXT("FreeOTFE4PDACypher*.dll")
#define FREEOTFE_DLL_PATTERN_HASH    DRIVER_SUBDIR TEXT("FreeOTFE4PDAHash*.dll")


// =========================================================================
// Exported functions...

// --------------------------------------
// Standard stream interface functions...

#define DLLEXPORT_MAIN_DSK_INIT TEXT("DSK_Init")
typedef DWORD (* PMainDLLFnDSK_Init)(
    LPCTSTR pContext,
    LPCVOID lpvBusContext
);
DWORD DSK_Init(
    LPCTSTR pContext,
    LPCVOID lpvBusContext
);

#define DLLEXPORT_MAIN_DSK_DEINIT TEXT("DSK_Deinit")
typedef BOOL (* PMainDLLFnDSK_Deinit)(
    DWORD hDevice
);
BOOL DSK_Deinit(
    DWORD hDevice
);

#define DLLEXPORT_MAIN_DSK_OPEN TEXT("DSK_Open")
typedef DWORD (* PMainDLLFnDSK_Open)(
    DWORD hDevice,
    DWORD AccessCode,
    DWORD ShareMode 
);
DWORD DSK_Open(
    DWORD hDevice,
    DWORD AccessCode,
    DWORD ShareMode 
); 

#define DLLEXPORT_MAIN_DSK_CLOSE TEXT("DSK_Close")
typedef BOOL (* PMainDLLFnDSK_Close)(
    DWORD hOpen 
);
BOOL DSK_Close(
    DWORD hOpen 
);

#define DLLEXPORT_MAIN_DSK_IOCONTROL TEXT("DSK_IOControl")
typedef BOOL (* PMainDLLFnDSK_IOControl)(
    DWORD hOpen,
    DWORD dwCode,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);
BOOL DSK_IOControl(
    DWORD hOpen,
    DWORD dwCode,
    PBYTE pBufIn,
    DWORD dwLenIn,
    PBYTE pBufOut,
    DWORD dwLenOut,
    PDWORD pdwActualOut 
);

#define DLLEXPORT_MAIN_DSK_SEEK TEXT("DSK_Seek")
typedef DWORD (* PMainDLLFnDSK_Seek)(
    DWORD hOpen,
    long Amount,
    WORD Type 
);
DWORD DSK_Seek(
    DWORD hOpen,
    long Amount,
    WORD Type 
); 

#define DLLEXPORT_MAIN_DSK_READ TEXT("DSK_Read")
typedef DWORD (* PMainDLLFnDSK_Read)(
    DWORD hOpen,
    LPVOID pBuffer,
    DWORD Count 
);
DWORD DSK_Read(
    DWORD hOpen,
    LPVOID pBuffer,
    DWORD Count 
); 

#define DLLEXPORT_MAIN_DSK_WRITE TEXT("DSK_Write")
typedef DWORD (* PMainDLLFnDSK_Write)(
    DWORD hOpen,
    LPCVOID pBuffer,
    DWORD Count 
);
DWORD DSK_Write(
    DWORD hOpen,
    LPCVOID pBuffer,
    DWORD Count 
);

#define DLLEXPORT_MAIN_DSK_POWERDOWN TEXT("DSK_PowerDown")
typedef void (* PMainDLLFnDSK_PowerDown)(
    DWORD hDevice 
);
void DSK_PowerDown(
    DWORD hDevice 
); 

#define DLLEXPORT_MAIN_DSK_POWERUP TEXT("DSK_PowerUp")
typedef void (* PMainDLLFnDSK_PowerUp)(
    DWORD hDevice 
);
void DSK_PowerUp(
    DWORD hDevice 
); 



#define DLLEXPORT_MAIN_DERIVEKEY TEXT("MainDeriveKey")
typedef DWORD (* PMainDLLFnDeriveKey)(
    IN  DWORD SizeBufferIn,
    IN  DIOC_DERIVE_KEY_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_DERIVE_KEY_OUT*  DIOCBufferOut
);
DWORD
MainDeriveKey(
    IN  DWORD SizeBufferIn,
    IN  DIOC_DERIVE_KEY_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_DERIVE_KEY_OUT*  DIOCBufferOut
);


#define DLLEXPORT_MAIN_MACDATA TEXT("MainMACData")
typedef DWORD (* PMainDLLFnMACData)(
    IN  DWORD SizeBufferIn,
    IN  DIOC_GENERATE_MAC_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_GENERATE_MAC_OUT*  DIOCBufferOut
);
DWORD
MainMACData(
    IN  DWORD SizeBufferIn,
    IN  DIOC_GENERATE_MAC_IN*   DIOCBufferIn,
    IN  DWORD SizeBufferOut,
    OUT DIOC_GENERATE_MAC_OUT*  DIOCBufferOut
);


// =========================================================================
// =========================================================================

#endif

