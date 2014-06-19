unit WindowsMissing_U;
// Description: Standard Windows type definitions that are missing from Delphi
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  windows;

const
  WM_DEVICECHANGE = $0219;

  DBT_DEVTYP_VOLUME = $00000002;
  DBTF_MEDIA = $0001; // Change affects media in drive
  DBTF_NET = $0002; // Volume is a network drive

  DBT_DEVICEREMOVECOMPLETE = $8004; // Device has been removed

type
  (*
  typedef union _LARGE_INTEGER {
      struct {
          DWORD LowPart;
          LONG  HighPart;
      };
      LONGLONG QuadPart;
  } LARGE_INTEGER;
  *)
  TLARGE_INTEGER = packed record
    LowPart: DWORD;
    HighPart: longint;
  end;


  TPARTITION_INFORMATION = packed record
    StartingOffset: TLARGE_INTEGER;
    PartitionLength: TLARGE_INTEGER;
    HiddenSectors: DWORD;
    PartitionNumber: DWORD;
    PartitionType: WORD; // byte;
    BootIndicator: WORD; // boolean;
    RecognizedPartition: WORD; //boolean;
    RewritePartition: WORD; //boolean;
  end;
  PTPARTITION_INFORMATION = ^TPARTITION_INFORMATION;


  TDRIVE_LAYOUT_INFORMATION = packed record
    PartitionCount: DWORD;
    Signature: DWORD;
    // This shouldn't actually be a 0..255 array, but a variable array instead
    PartitionEntry: array [0..255] of TPARTITION_INFORMATION;
  end;
  PTDRIVE_LAYOUT_INFORMATION = ^TDRIVE_LAYOUT_INFORMATION;


  TDEV_BROADCAST_VOLUME = packed record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: Word;
    junk: Word; // Just some padding to bring the struct size up to 20 bytes
  end;
  PTDEV_BROADCAST_VOLUME = ^TDEV_BROADCAST_VOLUME;


implementation

END.

