unit SdStructures_U;

interface

uses
  Classes, SysUtils, Windows;

const
  CipherType : array[0..8] of string = ('Summer v1', 'Blowfish 256 bit', 'TEA (16 round) 128 bit',
    'TEA (32 round) 128 bit', 'IDEA - AscomTech AG 128 bit', 'DES 56 bit', 'Square 128 bit',
    'Mitsubishi Misty1 128 bit', 'Triple DES 168 bit');

const
  MAX_SCRAMDISK_SLOTS = 7;

type
  EScramDiskError = Exception;
  ESDWrongVersion = EScramDiskError;
  ESDVxdNotFound = EScramDiskError;
  ESDVxdNotConnected = EScramDiskError;
  ESDDriveInUse = EScramDiskError;


// ==========
// -- begin -- Introduced for ScramDisk v3.xx
// ==========

  TDIOCParams = packed record
    dwIoControlCode: DWORD;
    lpvInBuffer:     Pointer; // xxx - should be DWORD or Pointer?
    cbInBuffer:      DWORD;
    lpvOutBuffer:    Pointer; // xxx - should be DWORD or Pointer?
    cbOutBuffer:     DWORD;
    pUserSpace:      Pointer;  // NT pointer to context ram used by driver
  end;

  // Fields common to physical and logical disk DCB's
  TDCB_COMMON = packed record
    DCB_physical_dcb: longword; // DCB for physical device
    DCB_expansion_length: longword;   // total length of IOP extension filled
                                      // in by IOS (excludes IOP size)
	//
	// Link fields follow
        //

    DCB_ptr_cd: Pointer;                // pointer to calldown list
    DCB_next_dcb: longword;             // link to next DCB
    DCB_next_logical_dcb: longword;     // pointer to next logical dcb
                                        // for physical device

    DCB_drive_lttr_equiv: byte;    // drive number (A: = 0, etc.)
                                    // set up by iosserv during logical
                                    // device associate processing.

    DCB_unit_number: byte;    // either physical drive number
                            // (sequential drive number or'd
                            // with 80h) or unit number within
                            // tsd. set up by iosbid for disk
                            // physical dcb's. set up by tsdpart
                            // for disk logical dcb's. set up by
                            // tsdaer for cdrom physical dcb's.

    DCB_TSD_Flags: word;    // Flags for TSD

	//
	// Volume Tracking fields follow
        //

    DCB_vrp_ptr: longword;            // pointer to VRP for this DCB

    DCB_dmd_flags: longword;            // demand bits of the topmost layer

    DCB_device_flags: longword;        // was BDD_Flags
    DCB_device_flags2: longword;        // second set of general purpose flags

    DCB_Partition_Start: longword;        // partition start sector

    DCB_track_table_ptr: longword;    // pointer for the track table buffer
                                        // for ioctls

    DCB_bds_ptr: longword;         // DOS BDS corresp. to this DCB
                                   // (logical DCB's only)
    DCB_Reserved1: longword;       // reserved - MBZ
    DCB_Reserved2: longword;       // reserved - MBZ

    DCB_apparent_blk_shift: byte;  // log of apparent_blk_size
    DCB_partition_type: byte;      // partition type
    DCB_sig: word;                 // padding and signature
    DCB_device_type: byte;         // Device Type
    DCB_Exclusive_VM: longword;    // handle for exclusive access to this device
    DCB_disk_bpb_flags: byte;      // bpb flags see defines below
    DCB_cAssoc: byte;              // count of logical drives
                                   // associated with this logical DCB
    DCB_Sstor_Host: byte;          // This field indicates a sstor host volume

    DCB_user_drvlet: word;         // contains the userdriveletter settings else ff
    DCB_Reserved3: word;           // reserved - MBZ
    DCB_Reserved4: longword;       // reserved - MBZ
  end;

  // blockdev compatible fields in physical disk DCB's
  TDCB_BLOCKDEV = packed record
    // The initial set of fields below should not be re-ordered or modified in
    // anyway as the offset and the size of fileds have to match those
    // in BLOCKDEV.H for COMPATIBITLITY!!!

    DCB_BDD_Next: longword;
    DCB_BDD_BD_Major_Version: byte;    // INIT <BDD_BD_Major_Ver>
    DCB_BDD_BD_Minor_Version: byte;    // INIT <BDD_BD_Minor_Ver>
    DCB_BDD_Device_SubType: byte;
    DCB_BDD_Int_13h_Number: byte;
    DCB_BDD_flags: longword;    // was BDD_Flags
    DCB_BDD_Name_Ptr: longword;

    DCB_apparent_sector_cnt: array [0..1] of longword; // num. of secs as seen by tsd and above
    DCB_apparent_blk_size: longword; // blk size of dev. as seen by tsd and above
    DCB_apparent_head_cnt: longword; // num. of heads as seen by tsd and above
    DCB_apparent_cyl_cnt: longword; // num. of cyls as seen by tsd and above
    DCB_apparent_spt: longword; // num. of secs/trk as seen by tsd and above
    DCB_BDD_Sync_Cmd_Proc: longword;
    DCB_BDD_Command_Proc: longword;
    DCB_BDD_Hw_Int_Proc: longword; // INIT <0>
    DCB_BDP_Cmd_Queue_Ascending: longword;
    DCB_BDP_Cmd_Queue_Descending: longword;
    DCB_BDP_Current_Flags: longword;
    DCB_BDP_Int13_Param_Ptr: longword;
    DCB_BDP_Current_Command: longword;
    DCB_BDP_Current_Position: array [0..1] of longword;
    DCB_BDP_Reserved: array [0..4] of longword;
    DCB_fastdisk_bdd: longword;  // set for DCBs created when a fastdisk regs
                                 // with the blockdev BDD for it else 0

    // End BlockDev compatibility
  end;


  // define the device control block (dcb) for physical devices (i.e., disks)
  TMYDCB = packed record
    DCB_cmn: TDCB_COMMON;

    DCB_max_xfer_len: longword;    // maximum transfer length

	//
	//	Actual geometry data follows
        //

    DCB_actual_sector_cnt: array [0..1] of longword;    // number of sectors as seen below
                                        // the tsd.

    DCB_actual_blk_size: longword;        // actual block size of the device
                                        // as seen below the tsd.
    DCB_actual_head_cnt: longword;        // number of heads as seen below
                                        // the tsd.
    DCB_actual_cyl_cnt: longword;        // number of cylinders as seen
                                        // below the tsd.
    DCB_actual_spt: longword;	      // number of sectors per track as
                                        // seen below the tsd.

    DCB_next_ddb_dcb: Pointer;    // link to next DCB on DDB chain

    DCB_dev_node: Pointer;    // pointer to dev node for this device

    DCB_bus_type: byte;    // Type of BUS, see below

    DCB_bus_number: byte;    // channel (cable) within adapter
    DCB_queue_freeze: byte;    // queue freeze depth counter

    DCB_max_sg_elements: byte;    // max # s/g elements. set initially
                                // by port, but may be MORE RESTRICTIVELY
                                // updated by other layers
    DCB_io_pend_count: byte;    // indicates number of requests pending
                                // for this DCB (VOLUME TRACKING LAYER USE ONLY)
    DCB_lock_count: byte;    // depth counter for LOCK MEDIA commands
                            // (VOLUME TRACKING LAYER USE ONLY)
	//
	// SCSI fields follow
        //

    DCB_SCSI_VSD_FLAGS: word;    // Flags for SRB builder

    DCB_scsi_target_id: byte;    // SCSI target ID
    DCB_scsi_lun: byte;        // SCSI logical unit number
    DCB_scsi_hba: byte;    // adapter number relative to port driver
    DCB_max_sense_data_len: byte;    // Maximum sense Length
    DCB_srb_ext_size: word;    // miniport srb extension length

    DCB_inquiry_flags: array [0..7] of byte;    // Device Inquiry Flags

    DCB_vendor_id: array [0..7] of char;    // Vendor ID string

    DCB_product_id: array [0..15] of char;    // Product ID string

    DCB_rev_level: array [0..3] of byte;    // Product revision level

    DCB_port_name: array [0..7] of byte;

    DCB_current_unit: byte;    // used to emulate multiple logical devices
                                // with a single physical device
    DCB_blocked_iop: longword;    // pointer to requests for an inactive
                                    // volume (VOLUME TRACKING LAYER USE ONLY)
    DCB_vol_unlock_timer: longword;    // unlock timer handle
    DCB_access_timer: byte;    // used to measure time between accesses
    DCB_Vol_Flags: byte;    // Flags for Volume Tracking
                            // volume tracking use only
    DCB_q_algo: byte;    // queuing algorithm index - see
                        // values below.

    DCB_unit_on_ctl: byte;        // relative device number on ctlr (0-based)
    DCB_Port_Specific: longword;    // bytes for PORT DRIVER use

    DCB_spindown_timer: longword;    // timer for drive spin down

    DCB_bdd: TDCB_BLOCKDEV;
  end;
  PTMYDCB = ^TMYDCB;


// ==========
// -- end -- Introduced for ScramDisk v3.xx
// ==========

  TDeviceInfo = packed record
    dr_lett1 : char;
    dr_lett1status : char;
    dr_lett2 : char;
    dr_lett2status : char;
    dr_lett3 : char;
    dr_lett3status : char;
    dr_lett4 : char;
    dr_lett4status : char;
    dr_lett5 : char;
    dr_lett5status : char;
    dr_lett6 : char;
    dr_lett6status : char;
    dr_lett7 : char;
    dr_lett7status : char;
    dr_lett8 : char;
    dr_lett8status : char;
  end;

  TMountType = (mtNotMounted, mtPartition, mtSVLFile, mtWAVFile, mtTimedOut, mtUnknown);

  // "VolumeName" has been removed from TSlotInfo - call GetVolumeInformation if
  // you need this information
  TSlotInfo = packed record
    SlotNo: Integer;
    MountType: TMountType;
    HostDriveLetter: string;
    DriveMountedAs: string;
    CipherType: string;
    FileName: string; // Will be set to partition ID if a partition is mounted.
                      // Under Win9x/Me, set to the drive ID if a partition is
                      // mounted.
    Comments: string;
    PreferredDrive: string;
    NoAccesses: Integer;
    LastAccess: TDateTime;
    readonly: boolean;
    softReadOnly: boolean; // mounted as readonly because header information
                           // set to readonly
    mediaReadOnly: boolean; // mounted as readonly because the media on which
                            // the volume resides is readonly/the volume file's
                            // file attributes are readonly
    wavFileBits: integer; // Gets set to either 4 (of 16 bits) or 8 (of 16 bits)
    partitionRemovable: boolean; // If mountType=mtPartition, this will be
                                 // TRUE if the partition is removable,
                                 // otherwise FALSE
    viaSKFFile: boolean; // Set to TRUE if mounted via an SKF file
    bfs: boolean; // Bypassed File System! ScramDisk v3 (and above) under Windows 9x/Me only
  end;

  TDeviceIOControlRegisters = packed record
    EBX : DWORD;
    EDX : DWORD;
    ECX : DWORD;
    EAX : DWORD;
    EDI : DWORD;
    ESI : DWORD;
    Flags : DWORD;
  end;

  TParamBlock = packed record
    Operation : Integer;
    NumLocks : Integer;
  end;

{
// XXX - delete if no problems with mounting under v2.02 and v3 under Win95/98/ME
// xxx - previously called - DELETE IF NO PROBS -
  TMountFileStruct = packed record
    DCB: Integer;
    Reserved1 : Integer; //reserved....
    Reserved2 : Integer; //reserved....
    WavOffset : Integer; // this has the offset to the WAV data if WAV (|0x80000000 if 4 bit wav)
    // Win32 needs to calculate wavoffset. See Scramdisk source code...
    FileName : PChar; // string of file to mount.
    MountDrive : Integer;
    MountSlot : Integer;
    MountResult : Integer; // set to 3 if file mounts ok Bit 8 set if WAV and buffer cannot allocate...
    // Set to 0x83 (bit 7 set) if file ALREADY mounted.....
    // Set to 0 if no slots free
    // 1 Not mounted
    Critical : PChar; // Points at 2k critical data, for SKF mount or nil for normal
  end;
}


  // xxx - previously called  - DELETE THIS LINE IF NO PROBS - TNewNTMountFileStruct
  TMountFileStruct = packed record
    //union
    //{
    //    MYPDCB dcb;
    //    int NtRealError;
    //};

    delphiUnion: record
      case Byte of
        0: ( dcb: PTMYDCB );
        1: ( NtRealError: integer );
      end;

    DrvObjAbandoned: cardinal; // flags1;    //inclusive
    bytespersector: cardinal; // flagsMagic;   //inclusive
    wavoffset: cardinal; // attrib
    fname: PChar; // previously "FileName"
    mountdrive: cardinal;
    LogicalDrives: cardinal; // Previously "mountslot"
    mountresult: cardinal; // Set to 3 if file mounts OK - bit 8 set if WAV and buffer cannot allocate
                           // Set to 0x83 (bit 7 set) if file ALREADY mounted
                           // Set to 0 if no slots free
                           // 1 Not mounted
    critical: PChar;
  end;
  // xxx - previously called - DELETE THIS LINE IF NO PROBS - PTNewNTMountFileStruct = ^TNewNTMountFileStruct;
  PTMountFileStruct = ^TMountFileStruct;


  TInfoStruct = packed record
    // slotNo is added, and infotext is reduced by 4 bytes to compensate
    // in comparison to the original ScramDisk source code, which just passed
    // slotNo as the first 4 bytes of infotext
    slotNo: integer;                  // slotNo and infotext - "infotext" in
    infotext: array [0..255] of char; // the ScramDisk source is 256 bytes
                                      // long, and slotNo doesn't exist in the
                                      // struct. However, due to the werido way
                                      // ScramDisk does things (passing slotNo
                                      // in as the first 4 bytes of infotext),
                                      // this is what the Delphi record should
                                      // look like.
                                      // YUCK!
                                      // i.e. It may look wrong here, but it
                                      // works, so just don't ask, OK?

    accessed: integer; // any user...   4
    preferredletter: integer; // any user	  4
    magiclow: integer; // any user	  4
    magichigh: integer; // any user	  4

    newtime: TSystemTime; //any user    16 bytes
    TimeValid1: integer; // any user	  4 bytes
    oldtime: TSystemTime; //any user	  16 bytes
    TimeValid2: integer; //any user	  4
    newmode: integer; //any user	  4
    sumLOW: cardinal;
    sumHIGH: cardinal;
    sum3: cardinal; //dummy
    sum4: cardinal; //dummy
    invalidsum: char;
    owner: char;
    spare2: char;
    spare3: char;
    writeEnabled: integer;
    dummy: array [0..171] of char; // Ugg... ScramDisk shouldn't never
                                   // actually use this - it's not part of
                                   // the struct. However, ScramDisk uses a
                                   // 516 byte buffer for when calling
                                   // DevioceIOControl with READINFOBLOCK,
                                   // and during this call, data is written
                                   // past the end of the struct.
                                   // We therefore have to add in this array
                                   // to compensate
                                   // DOUBLE YUCK IN ONE STRUCT!
  end;
  PInfoStruct = ^TInfoStruct;


  TWavInfo = packed record
    DataPointer : PChar;
    FileOffset : Cardinal;
    NumChannels : Word;
    WavType : Word;
    BitsPerSample : Word;
    Length : Cardinal;
  end;


  TDDB = packed record
    DDB_phys_addr : Cardinal;
    DDB_Next_DDB : Cardinal;
    DDB_Next_DDB_init : Cardinal;
    DDB_dcb_ptr : Cardinal;
    DDB_number_buses : Byte;
    DDB_ios_flags : Byte;
    DDB_sig : Word;
    DDB_dvt : Pointer;
    DDB_devnode_ptr : Pointer;
    DDB_reserved : Pointer;
  end;

  TSlotInfoEx = packed record
    DriveMountedFrom : string;
    DriveMountedAs : string;
    VolumeName : string;
  end;

// bpb introduced in ScramDisk v3
Tbpb = packed record
  bytespersector: word;
  sectorspercluster: byte;
  sectorsreserved: word;
  numfats: byte;
  numdirentries: word;
  smalldisksectors: word;
  mediadescripter: byte;
  sectorsperfat: word;
  sectorspertrack: word;
  numheads: word;
  hiddensectorcount: longword;
  bigdisksectors: longword;
  drivenum: byte;
  bytereserved: byte;
  extendbootsig: byte;
  serialnumber: longword;
  volname: array [0..10] of char;

  padding: array [0..4] of char; // Additional field required to get it to a
                                 // DWORD boundry

  voltype: array [0..7] of char;
end;

  TCryptVol = packed record
    cipher : Integer;
    index : Integer;
    wavoffset : Integer;
    ldcb: PTMYDCB;
    linkediop : Integer;
    booted : Integer;
    driveinuse : Integer;
    drive : Integer;

    cryptsectorfirst : Cardinal;
    cryptsectorlast : Cardinal;
    physdevDCB : PTMYDCB;
    mainkeys : array[0..2047] of Char;
    mountfilehandle : Integer;
    devicetype : Integer;

    SKFCreateTime : Integer;
    flags: integer; // Previously "digestindex : Integer;" with ScramDisk v2.02h
    timeoutcount: DWORD;
    spare3 : Integer;

    volname : array[0..15] of Char;
    digest : array[0..19] of Char;
    bpb: Tbpb;
    // spare is an array of char with dimensions [0..((20*7)-sizeof(bpb))-1];
    // bpb is 56 bytes long
    spare: array[0..83] of char;

    addb : TDDB;

    logicaldcb: TMYDCB;
    mounted_file_name : array[0..511] of Char;
  end;
  PTCryptVol = ^TCryptVol;

const
  VWIN32_DIOC_DOS_IOCTL = 1;
  PD_GETCVS = 1;
  PD_INQUIRE_DEVICES = 2;
  NUMBER_MOUNT_TYPES = 3;
  REQMON_INQUIRE_DEVICES = 4;
  INQUIRE_CRYPT_DEVICES = 4;
  SUPPLY_PASSWORD = 5;
  CLOSE_CRYPTED_DEVICE1 = 6;
  CLOSE_CRYPTED_DEVICE2 = 7;
  CLOSE_CRYPTED_DEVICE3 = 8;
  CLOSE_CRYPTED_DEVICE4 = 9;
  PUTCVS = 10;
  DISKIO = 11;
  GETDCBINFO = 12;
  FINDDRIVELETTER = 13;
  DISMOUNTDRIVE = 14;
  MOUNTFILE = 15;
  GETSHIFTCONST = 16;
  CLEARPASSWORDS = 17;
  MOUNTNEWDISK = 18;
  BORDER = 19;
  VALIDATEPASSWORD = 20;
  APPQUERY = 21;
  VOLQUERY = 22;
  GETDRIVERVERSION = 23;
  SETTIMEOUT = 24;
  RELEASETIMESLICE = 25;
  READINFOBLOCK = 26;
  WRITEINFOBLOCK = 27;
  R0FILEIO = 28;
  RING0CALLBACK = 29;
  UNINSTALLSCRAMDISK = 30;
  SETKEYFILEPASSWORD = 31;
  GETKEYFILEPASSWORD = 32;
  GETPASSWORDBUFFER = 33;
  GETINKEY = 34;
  SUPPLYDATEANDTIME = 35;
  SUPPLYCLICKEDFILENAME = 36;
  SENDFORCEQUIT = 37;
  REVOKESKFDATA = 38;
  RAWDEVICEDATA = 39;
  CHANGEPASSWORDS = 40;
  CLOSE_CRYPTED_DEVICE_N = 41;
  APP_REQUEST_REFRESH = 42;
  INQUIRE_SLOT = 43;
  E_NOT_CONNECTED = 'ScramDisk not connected - set Active to TRUE first';
  E_DRIVE_IN_USE = 'Close all windows and applications used by this drive first';

implementation

end.

