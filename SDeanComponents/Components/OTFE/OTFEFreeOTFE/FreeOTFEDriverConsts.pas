unit FreeOTFEDriverConsts;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

// From winioctl.h (part of the MS DDK)
const
  FILE_DEVICE_CD_ROM       = $00000002;
  FILE_DEVICE_DISK         = $00000007;
  FILE_DEVICE_FILE_SYSTEM  = $00000009;
  FILE_DEVICE_UNKNOWN      = $00000022;
  FILE_DEVICE_MASS_STORAGE = $0000002d;
  FILE_DEVICE_DVD          = $00000033;

  FILE_ANY_ACCESS                 = 0;
  FILE_READ_ACCESS                = $0001;    // file & pipe
  FILE_WRITE_ACCESS               = $0002;    // file & pipe

  FILE_READ_DATA  = FILE_READ_ACCESS;

  METHOD_BUFFERED                 = 0;


// From winioctl.h:

const IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;


const IOCTL_STORAGE_MEDIA_REMOVAL =
     ((IOCTL_STORAGE_BASE) * $10000) OR ((FILE_READ_ACCESS) * $4000) OR (($0201) * $4) OR (METHOD_BUFFERED);
// #define IOCTL_STORAGE_MEDIA_REMOVAL           CTL_CODE(IOCTL_STORAGE_BASE, 0x0201, METHOD_BUFFERED, FILE_READ_ACCESS)

const IOCTL_STORAGE_EJECT_MEDIA =
     ((IOCTL_STORAGE_BASE) * $10000) OR ((FILE_READ_ACCESS) * $4000) OR (($0202) * $4) OR (METHOD_BUFFERED);
// #define IOCTL_STORAGE_EJECT_MEDIA             CTL_CODE(IOCTL_STORAGE_BASE, 0x0202, METHOD_BUFFERED, FILE_READ_ACCESS)

const FSCTL_LOCK_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((6) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_LOCK_VOLUME               CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  6, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_UNLOCK_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((7) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_UNLOCK_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  7, METHOD_BUFFERED, FILE_ANY_ACCESS)

const FSCTL_DISMOUNT_VOLUME =
     ((FILE_DEVICE_FILE_SYSTEM) * $10000) OR ((FILE_ANY_ACCESS) * $4000) OR ((8) * $4) OR (METHOD_BUFFERED);
// #define FSCTL_DISMOUNT_VOLUME           CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  8, METHOD_BUFFERED, FILE_ANY_ACCESS)


type
  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PPREVENT_MEDIA_REMOVAL = ^TPREVENT_MEDIA_REMOVAL;
  TPREVENT_MEDIA_REMOVAL = record
    PreventMediaRemoval: boolean;
  end;


implementation

END.


