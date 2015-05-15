unit FreeOTFEDLLMainAPI;

 // API ported from:
 //   FreeOTFE4PDAAPIConsts.h
 //   FreeOTFEAPITypes.h
 //   FreeOTFE4PDA.h
 //   FreeOTFE4PDAAPI.h

interface

uses
  DriverAPI,
  FreeOTFEDriverConsts, Windows;

const
  // Taken from diskio.h:
  // Older values; replaced by IOCTL_... versions
  DISK_IOCTL_GETINFO      = 1;
  DISK_IOCTL_SETINFO      = 5;
  DISK_IOCTL_READ         = 2;
  DISK_IOCTL_WRITE        = 3;
  DISK_IOCTL_INITIALIZED  = 4;
  DISK_IOCTL_FORMAT_MEDIA = 6;
  DISK_IOCTL_GETNAME      = 9;

const
  IOCTL_DISK_BASE = FILE_DEVICE_DISK;

const
  IOCTL_DISK_GETINFO =
    (((IOCTL_DISK_BASE) * $10000) or ((FILE_ANY_ACCESS) * $4000) or
    (($700) * $4) or (METHOD_BUFFERED));

const
  IOCTL_DISK_READ =
    (((IOCTL_DISK_BASE) * $10000) or ((FILE_READ_ACCESS) * $4000) or
    (($702) * $4) or (METHOD_BUFFERED));

const
  IOCTL_DISK_WRITE =
    (((IOCTL_DISK_BASE) * $10000) or ((FILE_WRITE_ACCESS) * $4000) or
    (($703) * $4) or (METHOD_BUFFERED));


type
  // From:
  //   http://msdn.microsoft.com/en-us/library/aa930126.aspx
  //typedef struct _SG_BUF {
  //  PUCHAR sb_buf;
  //  DWORD sb_len;
  //} SG_BUF, *PSG_BUF;
  PSG_BUF = ^TSG_BUF;

  TSG_BUF = record
    sb_buf: PByte;
    sb_len: DWORD;
  end;

  // Declared upfront, as used by PFN_REQDONE, but uses TSG_REQ
  PSG_REQ = ^TSG_REQ;

  // From:
  //   http://4pda.ru/forum/index.php?showtopic=41047
  //typedef DWORD (* PFN_REQDONE)(struct _SG_REQ *);
  PFN_REQDONE = function(sg_reg: PSG_REQ): DWORD; CDECL;

  // From:
  //   http://msdn.microsoft.com/en-us/library/ms920817.aspx
  //typedef struct _SG_REQ {
  //  DWORD sr_start;
  //  DWORD sr_num_sec;
  //  DWORD sr_num_sg;
  //  DWORD sr_status;
  //  PFN_REQDONE sr_callback;
  //  SG_BUF sr_sglist[1];
  //} SG_REQ, *PSG_REQ;
  TSG_REQ = record
    sr_start:    DWORD;
    sr_num_sec:  DWORD;
    sr_num_sg:   DWORD;
    sr_status:   DWORD;
    sr_callback: PFN_REQDONE;
    sr_sglist:   array [1..1] of TSG_BUF;
  end;


const
  IOCTL_FREEOTFE_USER_DEV_HANDLE_SET =
    (((FILE_DEVICE_UNKNOWN) * $10000) or ((FILE_READ_DATA) * $4000) or
    (($815) * $4) or (METHOD_BUFFERED));

const
  IOCTL_FREEOTFE_USER_DEV_HANDLE_GET =
    (((FILE_DEVICE_UNKNOWN) * $10000) or ((FILE_READ_DATA) * $4000) or
    (($816) * $4) or (METHOD_BUFFERED));

const
  IOCTL_FREEOTFE_SET_FORCE_DISMOUNT =
    (((FILE_DEVICE_UNKNOWN) * $10000) or ((FILE_READ_DATA) * $4000) or
    (($817) * $4) or (METHOD_BUFFERED));


type
  // Commented out as not used in the PC DLL version
  //  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //  //   - i.e. it's not a "packed record"
  //  PDIOC_USER_DEVICE_HANDLE = ^TDIOC_USER_DEVICE_HANDLE;
  //  TDIOC_USER_DEVICE_HANDLE = record
  //    UserSpaceDeviceHandle: THandle;
  //  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_FORCE_DISMOUNTS = ^TDIOC_FORCE_DISMOUNTS;

  TDIOC_FORCE_DISMOUNTS = record
    ForceDismounts: Boolean;
  end;


 //DWORD DSK_Init(
 //    LPCTSTR pContext,
 //    LPCVOID lpvBusContext
 //);
const
  DLLEXPORT_MAIN_DSK_Init = 'DSK_Init';

type
  PDSK_Init = function(pContext: LPCTSTR; lpvBusContext: Pointer): DWORD; CDECL;

 //BOOL DSK_Deinit(
 //    DWORD hDevice
 //);
const
  DLLEXPORT_MAIN_DSK_Deinit = 'DSK_Deinit';

type
  PDSK_Deinit = function(hDevice: DWORD): Boolean; CDECL;

 //DWORD DSK_Open(
 //    DWORD hDevice,
 //    DWORD AccessCode,
 //    DWORD ShareMode
 //);
const
  DLLEXPORT_MAIN_DSK_Open = 'DSK_Open';

type
  PDSK_Open = function(hDevice: DWORD; AccessCode: DWORD; ShareMode: DWORD): DWORD; CDECL;

 //BOOL DSK_Close(
 //    DWORD hOpen
 //);
const
  DLLEXPORT_MAIN_DSK_Close = 'DSK_Close';

type
  PDSK_Close = function(hOpen: DWORD): Boolean; CDECL;

 //BOOL DSK_IOControl(
 //    DWORD hOpen,
 //    DWORD dwCode,
 //    PBYTE pBufIn,
 //    DWORD dwLenIn,
 //    PBYTE pBufOut,
 //    DWORD dwLenOut,
 //    PDWORD pdwActualOut
 //);
const
  DLLEXPORT_MAIN_DSK_IOControl = 'DSK_IOControl';

type
  PDSK_IOControl = function(hOpen: DWORD; dwCode: DWORD; pBufIn: PBYTE;
    dwLenIn: DWORD; pBufOut: PBYTE; dwLenOut: DWORD; pdwActualOut: PDWORD): Boolean; CDECL;

 //DWORD DSK_Seek(
 //    DWORD hOpen,
 //    long Amount,
 //    WORD Type
 //);
const
  DLLEXPORT_MAIN_DSK_Seek = 'DSK_Seek';

type
  PDSK_Seek = function(hOpen: DWORD; Amount: Integer; xType: Word): DWORD; CDECL;

 //DWORD DSK_Read(
 //    DWORD hOpen,
 //    LPVOID pBuffer,
 //    DWORD Count
 //);
const
  DLLEXPORT_MAIN_DSK_Read = 'DSK_Read';

type
  PDSK_Read = function(hOpen: DWORD; pBuffer: PByte; Count: DWORD): DWORD; CDECL;

 //DWORD DSK_Write(
 //    DWORD hOpen,
 //    LPCVOID pBuffer,
 //    DWORD Count
 //);
const
  DLLEXPORT_MAIN_DSK_Write = 'DSK_Write';

type
  PDSK_Write = function(hOpen: DWORD; pBuffer: PByte; Count: DWORD): DWORD; CDECL;

 //void DSK_PowerDown(
 //    DWORD hDevice
 //);
const
  DLLEXPORT_MAIN_DSK_PowerDown = 'DSK_PowerDown';

type
  PDSK_PowerDown = function(hDevice: DWORD): DWORD; CDECL;

 //void DSK_PowerUp(
 //    DWORD hDevice
 //);
const
  DLLEXPORT_MAIN_DSK_PowerUp = 'DSK_PowerUp';

type
  PDSK_PowerUp = function(hDevice: DWORD): DWORD; CDECL;



 //#define DLLEXPORT_MAIN_DERIVEKEY TEXT("MainDeriveKey")
 //typedef DWORD (* PMainDLLFnDeriveKey)(
 //    IN  DWORD SizeBufferIn,
 //    IN  DIOC_DERIVE_KEY_IN*   DIOCBufferIn,
 //    IN  DWORD SizeBufferOut,
 //    OUT DIOC_DERIVE_KEY_OUT*  DIOCBufferOut
 //);
const
  DLLEXPORT_MAIN_DERIVEKEY = 'MainDeriveKey';

type
  PMainDLLFnDeriveKey = function(SizeBufferIn: DWORD; DIOCBufferIn: PDIOC_DERIVE_KEY_IN_PC_DLL;
    SizeBufferOut: DWORD; DIOCBufferOut: PDIOC_DERIVE_KEY_OUT): DWORD; CDECL;


 //#define DLLEXPORT_MAIN_MACDATA TEXT("MainMACData")
 //typedef DWORD (* PMainDLLFnMACData)(
 //    IN  DWORD SizeBufferIn,
 //    IN  DIOC_GENERATE_MAC_IN*   DIOCBufferIn,
 //    IN  DWORD SizeBufferOut,
 //    OUT DIOC_GENERATE_MAC_OUT*  DIOCBufferOut
 //);
const
  DLLEXPORT_MAIN_MACDATA = 'MainMACData';

type
  PMainDLLFnMACData = function(SizeBufferIn: DWORD; DIOCBufferIn: PDIOC_GENERATE_MAC_IN_PC_DLL;
    SizeBufferOut: DWORD; DIOCBufferOut: PDIOC_GENERATE_MAC_OUT): DWORD; CDECL;

implementation

end.
