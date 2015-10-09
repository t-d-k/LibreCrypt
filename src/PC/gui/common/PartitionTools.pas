unit PartitionTools;
{
functions for handling partitions and disks

moved from sduGeneral
(c) tdk
}

interface
uses
//DELPHI
Winapi.Windows,
//sdu, lcutils
lcTypes;

const
  // A reasonable upper limit to the number of partitions that we'll support on
  // a disk
  // Note: During testing, a Windows XP x64 with one disk, which only had a
  //       about 8 partitions on it clocked up 24 partition table entries
  //       getting returned from drive layout DIOC.
  //       This *should* be dynamically calculated, but for now we just put in
  //       a reasonably high value.
  //       Since this is only used to define the size of a struct used, we can
  //       make it reasonably large without consequence
  SDU_MAX_PARTITIONS = 75;

  PARTITION_STYLE_MBR = 0;
  PARTITION_STYLE_GPT = 1;
  PARTITION_STYLE_RAW = 2;


TYPE
  // Note: DON'T USE A PACKED RECORD HERE!
  TSDUPartitionInfo = record
    StartingOffset:      ULONGLONG;  // LARGE_INTEGER;
    PartitionLength:     ULONGLONG;  // In *bytes*. LARGE_INTEGER;
    HiddenSectors:       DWORD;
    PartitionNumber:     DWORD;
    PartitionType:       Byte;
    BootIndicator:       Boolean;
    RecognizedPartition: Boolean;
    RewritePartition:    Boolean;
    //    junk: array[0..255] of byte;  // Not needed - just padding; should be removed
  end;
  PSDUPartitionInfo = ^TSDUPartitionInfo;

    TSDUDiskGeometry = packed record
    Cylinders: LARGE_INTEGER;
    MediaType: DWORD;  // A TSDUMediaType, but must be a DWORD to pad out
    // correctly
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    junk: array[0..255] of Byte;  // Not needed - just padding; should be removed
  end;
  PSDUDiskGeometry = ^TSDUDiskGeometry;

   // Note: DON'T USE A PACKED RECORD HERE!
  TSDUDriveLayoutInformation = record
    PartitionCount: DWORD;
    Signature:      DWORD;
    PartitionEntry: array [0..(SDU_MAX_PARTITIONS - 1)] of TSDUPartitionInfo;
  end;
  PSDUDriveLayoutInformation = ^TSDUDriveLayoutInformation;

    TDriveLayoutInformationMbr = record
    Signature: DWORD;
  end;

  TDriveLayoutInformationGpt = record
    DiskId:       TGuid;
    StartingUsableOffset: Int64;
    UsableLength: Int64;
    MaxPartitionCount: DWORD;
  end;

  TPartitionInformationMbr = record
    PartitionType:       Byte;
    BootIndicator:       Boolean;
    RecognizedPartition: Boolean;
    HiddenSectors:       DWORD;
  end;

  TPartitionInformationGpt = record
    PartitionType: TGuid;
    PartitionId:   TGuid;
    Attributes:    Int64;
    Name:          array [0..35] of Widechar;
  end;

  TPartitionInformationEx = record
    PartitionStyle:   Integer;
    StartingOffset:   Int64;
    PartitionLength:  Int64;
    PartitionNumber:  DWORD;
    RewritePartition: Boolean;
    case Integer of
      0: (Mbr: TPartitionInformationMbr);
      1: (Gpt: TPartitionInformationGpt);
  end;


  TSDUDriveLayoutInformationEx = record
    PartitionStyle: DWORD;
    PartitionCount: DWORD;
    DriveLayoutInformation: record
           case Integer of
        0: (Mbr: TDriveLayoutInformationMbr);
        1: (Gpt: TDriveLayoutInformationGpt);
    end;
    PartitionEntry: array [0..(SDU_MAX_PARTITIONS - 1)] of TPartitionInformationEx;
  end;


// Get device name for CDROM/DVD
function SDUDeviceNameForCDROM(CDROMNumber: Cardinal): String;
// Get device name for disk
function SDUDeviceNameForDisk(DiskNumber: Cardinal): String;
// Get device name for drive
function SDUDeviceNameForDrive(driveLetter: ansichar): String;
// Get device name for partition
function SDUDeviceNameForPartition(DiskNo: Integer; PartitionNo: Integer): String;

function SDUPartitionDiscFromDeviceName(out DiskNo, PartitionNo: Integer;device_name  : string)   : Boolean;

{ TODO -otdk -crefactor : move TSDUPartitionInfo and fns into separate unit }
 // Get size of partition
 // Returns file size, or -1 on error
function SDUGetPartitionSize(driveletter: ansichar): ULONGLONG;
function SDUGetPartitionSize_Device(driveDevice: String): ULONGLONG;
 // Get partition information
 // Returns TRUE/FALSE on success/failure
function SDUGetPartitionInfo(driveletter: ansichar; var partInfo: TSDUPartitionInfo): Boolean;
  overload;
function SDUGetPartitionInfo(driveDevice: String; var partInfo: TSDUPartitionInfo): Boolean;
  overload;
 // Get disk geometry
 // Returns TRUE/FALSE on success/failure
function SDUGetDiskGeometry(driveletter: ansichar; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
function SDUGetDiskGeometry(DiskNumber: Integer; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
function SDUGetDiskGeometry(driveDevice: String; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;

  // Return text representation of partition type
function SDUMbrPartitionType(PartitionTypeID: Byte; LongDesc: Boolean): String;
// Return text representation of partition type
function SDUGptPartitionType(PartitionType: TGuid; LongDesc: Boolean): String;

// does it  start with '\device\...'?
function IsPartitionPath(path  : string                               ) : Boolean;

const
                                            //\Device\Harddisk1\Partition0
                                            // \\.\PHYSICALDRIVE
  FMT_DEVICENAME_HDD_PHYSICAL_DISK = '\\.\PHYSICALDRIVE%d';
  FMT_DEVICENAME_PARTITION_DEVICE  = '\Device\Harddisk%d\Partition%d';
  FMT_DEVICENAME_HDD_DEVICE        = '\Device\Harddisk%d';
  FMT_DEVICENAME_CDROM_DEVICE      = '\Device\CdRom%d';
  FMT_DEVICENAME_DRIVE_DEVICE      = '\\.\%s:';

implementation

 uses
 //DELPHI
 System.SysUtils,  System.StrUtils,
 //sdu, lcutils
 lcCOnsts;
const
  SDU_FILE_ANY_ACCESS   = 0;
  SDU_FILE_READ_ACCESS  = $0001;    // file & pipe
  SDU_FILE_WRITE_ACCESS = $0002;    // file & pipe


  SDU_METHOD_BUFFERED = 0;

  // Constants from winioctl.h (from the MS DDK)
  SDU_FILE_READ_DATA     = SDU_FILE_READ_ACCESS;
  SDU_FILE_DEVICE_CD_ROM = $00000002;
  SDU_FILE_DEVICE_DISK   = $00000007;
  SDU_FILE_DEVICE_DVD    = $00000033;
  SDU_IOCTL_DISK_BASE = SDU_FILE_DEVICE_DISK;


 // From winioctl.h
  SDU_IOCTL_DISK_GET_DRIVE_GEOMETRY =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_ANY_ACCESS) * $4000) or
    (($0000) * $4) or (SDU_METHOD_BUFFERED));

  // From winioctl.h
  SDU_IOCTL_DISK_GET_PARTITION_INFO =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_READ_ACCESS) * $4000) or
    (($0001) * $4) or (SDU_METHOD_BUFFERED));

  SDU_IOCTL_DISK_GET_DRIVE_LAYOUT    =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_READ_ACCESS) * $4000) or
    (($0003) * $4) or (SDU_METHOD_BUFFERED));
  // #define IOCTL_DISK_GET_DRIVE_LAYOUT     CTL_CODE(IOCTL_DISK_BASE, 0x0003, METHOD_BUFFERED, FILE_READ_ACCESS)
  SDU_IOCTL_DISK_GET_DRIVE_LAYOUT_EX =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_READ_ACCESS) * $4000) or
    (($0014) * $4) or (SDU_METHOD_BUFFERED));
  //  CTL_CODE(IOCTL_DISK_BASE, 0x0014, METHOD_BUFFERED, FILE_ANY_ACCESS)

// ----------------------------------------------------------------------------
function SDUDeviceNameForCDROM(CDROMNumber: Cardinal): String;
begin
  Result := Format(FMT_DEVICENAME_CDROM_DEVICE, [CDROMNumber]);
end;
// ----------------------------------------------------------------------------
function SDUDeviceNameForDisk(DiskNumber: Cardinal): String;
begin
  Result := Format(FMT_DEVICENAME_HDD_PHYSICAL_DISK, [DiskNumber]);
end;

// ----------------------------------------------------------------------------
function SDUDeviceNameForDrive(driveLetter: ansichar): String;
begin
  Result := Format(FMT_DEVICENAME_DRIVE_DEVICE, [upcase(driveLetter)]);
end;


// PartitionNo - Set to zero for entire disk, otherwise partition number
function SDUDeviceNameForPartition(DiskNo: Integer; PartitionNo: Integer): String;
begin
  Result := Format(FMT_DEVICENAME_PARTITION_DEVICE, [DiskNo, PartitionNo]);
end;

function SDUPartitionDiscFromDeviceName(out DiskNo, PartitionNo: Integer;device_name  : string)   : Boolean;
var
  psn   : Integer;
begin
  result := false;
  if AnsiStartsStr('\Device\Harddisk', device_name) and (length(device_name)>= length('\Device\Harddisk0\Partition0'))  then begin
    delete(device_name,1, 16);
    psn  := Pos('\',device_name);
    if psn > 1 then begin
      if TryStrToInt(AnsiMidStr(device_name,1,psn-1) ,DiskNo) then begin
        delete(device_name,1, psn);
        if AnsiStartsStr('Partition', device_name) and (length(device_name)>= length('Partition0'))  then begin
          delete(device_name,1, 9);
          result := TryStrToInt(device_name,PartitionNo);
        end;
      end;
    end;
  end;
end;

// ----------------------------------------------------------------------------
function SDUMbrPartitionType(PartitionTypeID: Byte; LongDesc: Boolean): String;
begin
  Result := RS_UNKNOWN;

  if LongDesc then begin
    {Partition types taken (31st May //2008) from:
      http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
     see also https://msdn.microsoft.com/en-us/library/windows/desktop/aa363990%28v=vs.85%29.aspx
     for windows specific ones}

    case PartitionTypeID of
      $00: Result := 'Empty';
      $01: Result := 'DOS 12-bit FAT';
      $02: Result := 'XENIX root';
      $03: Result := 'XENIX /usr';
      $04: Result := 'DOS 3.0+ 16-bit FAT (up to 32M)';
      $05: Result := 'DOS 3.3+ Extended Partition';
      $06: Result := 'DOS 3.31+ 16-bit FAT (over 32M)';
      $07: Result := 'Windows NT NTFS';  // Most likely of the below
      {
      $07: Result := 'OS/2 IFS (e.g., HPFS)';
      $07: Result := 'Windows NT NTFS';
      $07: Result := 'Advanced Unix';
      $07: Result := 'QNX2.x pre-1988 (see below under IDs 4d-4f)';
      }
      $08: Result := 'OS/2/AIX boot/SplitDrive/Commodore DOS/DELL multispan partition/QNX';
      {
      $08: Result := 'OS/2 (v1.0-1.3 only)';
      $08: Result := 'AIX boot partition';
      $08: Result := 'SplitDrive';
      $08: Result := 'Commodore DOS';
      $08: Result := 'DELL partition spanning multiple drives';
      $08: Result := 'QNX 1.x and 2.x ("qny")';
      }
      $09: Result := 'AIX data/Coherent filesystem/QNX';
      {
      $09: Result := 'AIX data partition';
      $09: Result := 'Coherent filesystem';
      $09: Result := 'QNX 1.x and 2.x ("qnz")';
      }
      $0a: Result := 'OS/2 Boot Manager/Coherent swap/OPUS';
      {
      $0a: Result := 'OS/2 Boot Manager';
      $0a: Result := 'Coherent swap partition';
      $0a: Result := 'OPUS';
      }
      $0b: Result := 'WIN95 OSR2 FAT32';
      $0c: Result := 'WIN95 OSR2 FAT32, LBA-mapped';
      $0e: Result := 'WIN95: DOS 16-bit FAT, LBA-mapped';
      $0f: Result := 'WIN95: Extended partition, LBA-mapped';
      $10: Result := 'OPUS (?)';
      $11: Result := 'Hidden FAT'; // Most useful of the below
      {
      $11: Result := 'Hidden DOS 12-bit FAT';
      $11: Result := 'Leading Edge DOS 3.x logically sectored FAT';
      }
      $12: Result := 'Configuration/diagnostics partition';
      $14: Result := 'Hidden FAT'; // Most useful of the below
      {
      $14: Result := 'Hidden DOS 16-bit FAT <32M';
      $14: Result := 'AST DOS with logically sectored FAT';
      }
      $16: Result := 'Hidden DOS 16-bit FAT >=32M';
      $17: Result := 'Hidden IFS (e.g., HPFS)';
      $18: Result := 'AST SmartSleep Partition';
      $19: Result := 'Unused';
      $1b: Result := 'Hidden WIN95 OSR2 FAT32';
      $1c: Result := 'Hidden WIN95 OSR2 FAT32, LBA-mapped';
      $1e: Result := 'Hidden WIN95 16-bit FAT, LBA-mapped';
      $20: Result := 'Unused';
      $21: Result := 'Reserved/Unused';
      {
      $21: Result := 'Reserved';
      $21: Result := 'Unused';
      }
      $22: Result := 'Unused';
      $23: Result := 'Reserved';
      $24: Result := 'NEC DOS 3.x';
      $26: Result := 'Reserved';
      $27: Result := 'PQservice/Windows RE hidden/MirOS/RouterBOOT';
      {
      $27: Result := 'PQservice';
      $27: Result := 'Windows RE hidden partition';
      $27: Result := 'MirOS partition';
      $27: Result := 'RouterBOOT kernel partition';
      }
      $2a: Result := 'AtheOS File System (AFS)';
      $2b: Result := 'SyllableSecure (SylStor)';
      $31: Result := 'Reserved';
      $32: Result := 'NOS';
      $33: Result := 'Reserved';
      $34: Result := 'Reserved';
      $35: Result := 'JFS on OS/2 or eCS';
      $36: Result := 'Reserved';
      $38: Result := 'THEOS ver 3.2 2gb partition';
      $39: Result := 'Plan 9 partition/THEOS ver 4 spanned';
      {
      $39: Result := 'Plan 9 partition';
      $39: Result := 'THEOS ver 4 spanned partition';
      }
      $3a: Result := 'THEOS ver 4 4gb partition';
      $3b: Result := 'THEOS ver 4 extended partition';
      $3c: Result := 'PartitionMagic recovery partition';
      $3d: Result := 'Hidden NetWare';
      $40: Result := 'Venix 80286';
      $41: Result := 'Linux/MINIX'; // Most likely of the below
      {
      $41: Result := 'Linux/MINIX (sharing disk with DRDOS)';
      $41: Result := 'Personal RISC Boot';
      $41: Result := 'PPC PReP (Power PC Reference Platform) Boot';
      }
      $42: Result := 'Linux swap/logical disk manager'; // Most likely of the below
      {
      $42: Result := 'Linux swap (sharing disk with DRDOS)';
      $42: Result := 'SFS (Secure Filesystem)';
      $42: Result := 'Windows 2000 dynamic extended partition marker';
      }
      $43: Result := 'Linux native (sharing disk with DRDOS)';
      $44: Result := 'GoBack partition';
      $45: Result := 'Boot-US boot manager/Priam/EUMEL/Elan';
      {
      $45: Result := 'Boot-US boot manager';
      $45: Result := 'Priam';
      $45: Result := 'EUMEL/Elan';
      }
      $46: Result := 'EUMEL/Elan';
      $47: Result := 'EUMEL/Elan';
      $48: Result := 'EUMEL/Elan';
      $4a: Result := 'Mark Aitchison''s ALFS/THIN';  // AdaOS (Withdrawn)
      {
      $4a: Result := 'Mark Aitchison's ALFS/THIN lightweight filesystem for DOS';
      $4a: Result := 'AdaOS Aquila (Withdrawn)';
      }
      $4c: Result := 'Oberon partition';
      $4d: Result := 'QNX4.x';
      $4e: Result := 'QNX4.x 2nd part';
      $4f: Result := 'QNX4.x 3rd part/Oberon';
      {
      $4f: Result := 'QNX4.x 3rd part';
      $4f: Result := 'Oberon partition';
      }
      $50: Result := 'OnTrack Disk Manager/Lynx RTOS/Native Oberon';
      {
      $50: Result := 'OnTrack Disk Manager (older versions) RO';
      $50: Result := 'Lynx RTOS';
      $50: Result := 'Native Oberon (alt)';
      }
      $51: Result := 'OnTrack Disk Manager RW (DM6 Aux1)/Novell';
      {
      $51: Result := 'OnTrack Disk Manager RW (DM6 Aux1)';
      $51: Result := 'Novell';
      }
      $52: Result := 'CP/M / Microport SysV/AT';
      {
      $52: Result := 'CP/M';
      $52: Result := 'Microport SysV/AT';
      }
      $53: Result := 'Disk Manager 6.0 Aux3';
      $54: Result := 'Disk Manager 6.0 Dynamic Drive Overlay (DDO)';
      $55: Result := 'EZ-Drive';
      $56: Result := 'Golden Bow VFeature/DM converted to EZ-BIOS/AT&T MS-DOS';
      {
      $56: Result := 'Golden Bow VFeature Partitioned Volume.';
      $56: Result := 'DM converted to EZ-BIOS';
      $56: Result := 'AT&T MS-DOS 3.x';
      }
      $57: Result := 'DrivePro/VNDI';
      {
      $57: Result := 'DrivePro';
      $57: Result := 'VNDI Partition';
      }
      $5c: Result := 'Priam EDisk';
      $61: Result := 'SpeedStor';
      $63: Result := 'Unix System V (SCO, ISC Unix, UnixWare, ...), Mach, GNU Hurd';
      $64: Result := 'PC-ARMOUR protected/Novell Netware 286, 2.xx';
      {
      $64: Result := 'PC-ARMOUR protected partition';
      $64: Result := 'Novell Netware 286, 2.xx';
      }
      $65: Result := 'Novell Netware 386, 3.xx or 4.xx';
      $66: Result := 'Novell Netware SMS Partition';
      $67: Result := 'Novell';
      $68: Result := 'Novell';
      $69: Result := 'Novell Netware 5+, Novell Netware NSS Partition';
      $6e: Result := '??';
      $70: Result := 'DiskSecure Multi-Boot';
      $71: Result := 'Reserved';
      $72: Result := 'V7/x86';
      $73: Result := 'Reserved';
      $74: Result := 'Reserved/Scramdisk';
      {
      $74: Result := 'Reserved';
      $74: Result := 'Scramdisk partition';
      }
      $75: Result := 'IBM PC/IX';
      $76: Result := 'Reserved';
      $77: Result := 'M2FS/M2CS / VNDI';
      {
      $77: Result := 'M2FS/M2CS partition';
      $77: Result := 'VNDI Partition';
      }
      $78: Result := 'XOSL FS';
      $7e: Result := 'Unused';
      $7f: Result := 'Unused';
      $80: Result := 'MINIX until 1.4a';
      $81: Result := 'MINIX since 1.4b, early Linux/Mitac disk manager';
      {
      $81: Result := 'MINIX since 1.4b, early Linux';
      $81: Result := 'Mitac disk manager';
      }
      $82: Result := 'Prime/Solaris x86/Linux swap';
      {
      $82: Result := 'Prime';
      $82: Result := 'Solaris x86';
      $82: Result := 'Linux swap';
      }
      $83: Result := 'Linux native partition';
      $84: Result := 'OS/2 hidden C: drive/Hibernation partition';
      {
      $84: Result := 'OS/2 hidden C: drive';
      $84: Result := 'Hibernation partition';
      }
      $85: Result := 'Linux extended partition';
      $86: Result := 'Old Linux RAID superblock/FAT16 volume set';
      {
      $86: Result := 'Old Linux RAID partition superblock';
      $86: Result := 'FAT16 volume set';
      }
      $87: Result := 'NTFS volume set';
      $88: Result := 'Linux plaintext partition table';
      $8a: Result := 'Linux Kernel Partition (used by AiR-BOOT)';
      $8b: Result := 'Legacy Fault Tolerant FAT32 volume';
      $8c: Result := 'Legacy Fault Tolerant FAT32 volume using BIOS extd INT 13h';
      $8d: Result := 'Free FDISK hidden Primary DOS FAT12 partitition';
      $8e: Result := 'Linux Logical Volume Manager partition';
      $90: Result := 'Free FDISK hidden Primary DOS FAT16 partitition';
      $91: Result := 'Free FDISK hidden DOS extended partitition';
      $92: Result := 'Free FDISK hidden Primary DOS large FAT16 partitition';
      $93: Result := 'Hidden Linux native/Amoeba';
      {
      $93: Result := 'Hidden Linux native partition';
      $93: Result := 'Amoeba';
      }
      $94: Result := 'Amoeba bad block table';
      $95: Result := 'MIT EXOPC native partitions';
      $97: Result := 'Free FDISK hidden Primary DOS FAT32 partitition';
      $98: Result := 'Free FDISK hidden Primary DOS FAT32 (LBA)/Datalight ROM-DOS Super-Boot';
      {
      $98: Result := 'Free FDISK hidden Primary DOS FAT32 partitition (LBA)';
      $98: Result := 'Datalight ROM-DOS Super-Boot Partition';
      }
      $99: Result := 'DCE376 logical drive';
      $9a: Result := 'Free FDISK hidden Primary DOS FAT16 partitition (LBA)';
      $9b: Result := 'Free FDISK hidden DOS extended partitition (LBA)';
      $9f: Result := 'BSD/OS';
      $a0: Result := 'Laptop hibernation partition';
      $a1: Result := 'Laptop hibernation partition/HP Volume Expansion (SpeedStor variant)';
      {
      $a1: Result := 'Laptop hibernation partition';
      $a1: Result := 'HP Volume Expansion (SpeedStor variant)';
      }
      $a3: Result := 'HP Volume Expansion (SpeedStor variant)';
      $a4: Result := 'HP Volume Expansion (SpeedStor variant)';
      $a5: Result := 'BSD/386, 386BSD, NetBSD, FreeBSD';
      $a6: Result := 'OpenBSD/HP Volume Expansion (SpeedStor variant)';
      {
      $a6: Result := 'OpenBSD';
      $a6: Result := 'HP Volume Expansion (SpeedStor variant)';
      }
      $a7: Result := 'NeXTStep';
      $a8: Result := 'Mac OS-X';
      $a9: Result := 'NetBSD';
      $aa: Result := 'Olivetti Fat 12 1.44MB Service Partition';
      $ab: Result := 'Mac OS-X Boot partition/GO! partition';
      {
      $ab: Result := 'Mac OS-X Boot partition';
      $ab: Result := 'GO! partition';
      }
      $ae: Result := 'ShagOS filesystem';
      $af: Result := 'ShagOS swap partition/MacOS X HFS';
      {
      $af: Result := 'ShagOS swap partition';
      $af: Result := 'MacOS X HFS';
      }
      $b0: Result := 'BootStar Dummy';
      $b1: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b3: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b4: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b6: Result :=
          'HP Volume Expansion (SpeedStor variant)/Corrupted Windows NT mirror set (master), FAT16 file system';
      {
      $b6: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b6: Result := 'Corrupted Windows NT mirror set (master), FAT16 file system';
      }
      $b7: Result :=
          'Corrupted Windows NT mirror set (master), NTFS file system/BSDI BSD/386 filesystem';
      {
      $b7: Result := 'Corrupted Windows NT mirror set (master), NTFS file system';
      $b7: Result := 'BSDI BSD/386 filesystem';
      }
      $b8: Result := 'BSDI BSD/386 swap partition';
      $bb: Result := 'Boot Wizard hidden';
      $bc: Result := 'Acronis backup partition';
      $be: Result := 'Solaris 8 boot partition';
      $bf: Result := 'New Solaris x86 partition';
      $c0: Result := 'CTOS/REAL/32 secure/NTFT/DR-DOS/Novell DOS secured'; //The high bit of a partition type code indicates that a partition is part of an NTFT mirror or striped array.
      {
      $c0: Result := 'CTOS';
      $c0: Result := 'REAL/32 secure small partition';
      $c0: Result := 'NTFT Partition';
      $c0: Result := 'DR-DOS/Novell DOS secured partition';
      }
      $c1: Result := 'DRDOS/secured (FAT-12)';
      $c2: Result := 'Unused/Hidden Linux';
      {
      $c2: Result := 'Unused';
      $c2: Result := 'Hidden Linux';
      }
      $c3: Result := 'Hidden Linux swap';
      $c4: Result := 'DRDOS/secured (FAT-16, < 32M)';
      $c5: Result := 'DRDOS/secured (extended)';
      $c6: Result := 'DRDOS/secured (FAT-16, >= 32M)/Windows NT corrupted FAT16 volume/stripe set';
      {
      $c6: Result := 'DRDOS/secured (FAT-16, >= 32M)';
      $c6: Result := 'Windows NT corrupted FAT16 volume/stripe set';
      }
      $c7: Result := 'Windows NT corrupted NTFS volume/stripe set / Syrinx boot';
      {
      $c7: Result := 'Windows NT corrupted NTFS volume/stripe set';
      $c7: Result := 'Syrinx boot';
      }
      $c8: Result := 'Reserved for DR-DOS 8.0+';
      $c9: Result := 'Reserved for DR-DOS 8.0+';
      $ca: Result := 'Reserved for DR-DOS 8.0+';
      $cb: Result := 'DR-DOS 7.04+ secured FAT32 (CHS)/';
      $cc: Result := 'DR-DOS 7.04+ secured FAT32 (LBA)/';
      $cd: Result := 'CTOS Memdump?';
      $ce: Result := 'DR-DOS 7.04+ FAT16X (LBA)/';
      $cf: Result := 'DR-DOS 7.04+ secured EXT DOS (LBA)/';
      $d0: Result := 'REAL/32 secure big/Multiuser DOS secured';
      {
      $d0: Result := 'REAL/32 secure big partition';
      $d0: Result := 'Multiuser DOS secured partition';
      }
      $d1: Result := 'Old Multiuser DOS secured FAT12';
      $d4: Result := 'Old Multiuser DOS secured FAT16 <32M';
      $d5: Result := 'Old Multiuser DOS secured extended partition';
      $d6: Result := 'Old Multiuser DOS secured FAT16 >=32M';
      $d8: Result := 'CP/M-86';
      $da: Result := 'Non-FS Data/Powercopy Backup';
      {
      $da: Result := 'Non-FS Data';
      $da: Result := 'Powercopy Backup';
      }
      $db: Result :=
          'Digital Research CP/M, Concurrent CP/M, Concurrent DOS / CTOS / KDG Telemetry SCPU boot';
      {
      $db: Result := 'Digital Research CP/M, Concurrent CP/M, Concurrent DOS';
      $db: Result := 'CTOS (Convergent Technologies OS -Unisys)';
      $db: Result := 'KDG Telemetry SCPU boot';
      }
      $dd: Result := 'Hidden CTOS Memdump?';
      $de: Result := 'Dell PowerEdge Server utilities (FAT fs)';
      $df: Result := 'DG/UX virtual disk manager partition / BootIt EMBRM';
      {
      $df: Result := 'DG/UX virtual disk manager partition';
      $df: Result := 'BootIt EMBRM';
      }
      $e0: Result := 'Reserved by STMicroelectronics for a filesystem called ST AVFS';
      $e1: Result := 'DOS access or SpeedStor 12-bit FAT extended partition';
      $e3: Result := 'DOS R/O or SpeedStor';
      $e4: Result := 'SpeedStor 16-bit FAT extended partition < 1024 cyl.';
      $e5: Result := 'Tandy MSDOS with logically sectored FAT';
      $e6: Result := 'Storage Dimensions SpeedStor';
      $e8: Result := 'LUKS';
      $eb: Result := 'BeOS BFS';
      $ec: Result := 'SkyOS SkyFS';
      $ed: Result := 'Unused';
      $ee: Result := 'Indication that this legacy MBR is followed by an EFI header';
      $ef: Result := 'Partition that contains an EFI file system';
      $f0: Result := 'Linux/PA-RISC boot loader';
      $f1: Result := 'Storage Dimensions SpeedStor';
      $f2: Result := 'DOS 3.3+ secondary partition';
      $f3: Result := 'Reserved';
      $f4: Result := 'SpeedStor large partition/Prologue single-volume partition';
      {
      $f4: Result := 'SpeedStor large partition';
      $f4: Result := 'Prologue single-volume partition';
      }
      $f5: Result := 'Prologue multi-volume partition';
      $f6: Result := 'Storage Dimensions SpeedStor';
      $f7: Result := 'Unused';
      $f9: Result := 'pCache';
      $fa: Result := 'Bochs';
      $fb: Result := 'VMware File System partition';
      $fc: Result := 'VMware Swap partition';
      $fd: Result := 'Linux raid partition with autodetect using persistent superblock';
      $fe: Result :=
          'SpeedStor > 1024 cyl./LANstep/IBM PS/2 IML/Windows NT Disk Administrator hidden partition/Linux Logical Volume Manager partition (old)';
      {
      $fe: Result := 'SpeedStor > 1024 cyl.';
      $fe: Result := 'LANstep';
      $fe: Result := 'IBM PS/2 IML (Initial Microcode Load) partition,';
      $fe: Result := 'Windows NT Disk Administrator hidden partition';
      $fe: Result := 'Linux Logical Volume Manager partition (old)';
      }
      $ff: Result := 'Xenix Bad Block Table';
    end;
  end else begin
    // Partition types taken (31st May 2008) from:
    //   Linux fdisk v1.0
    case PartitionTypeID of
      $00: Result := 'Empty';
      $01: Result := 'FAT12';
      $02: Result := 'XENIX root';
      $03: Result := 'XENIX usr';
      $04: Result := 'Small FAT16';
      $05: Result := 'Extended';
      $06: Result := 'FAT16';
      $07: Result := 'HPFS/NTFS';
      $08: Result := 'AIX';
      $09: Result := 'AIX bootable';
      $0a: Result := 'OS/2 boot mgr';
      $0b: Result := 'FAT32';
      $0c: Result := 'FAT32 LBA';
      $0e: Result := 'FAT16 LBA';
      $0f: Result := 'Extended LBA';
      $10: Result := 'OPUS';
      $11: Result := 'Hidden FAT12';
      $12: Result := 'Compaq diag';
      $14: Result := 'Hidd Sm FAT16';
      $16: Result := 'Hidd FAT16';
      $17: Result := 'Hidd HPFS/NTFS';
      $18: Result := 'AST SmartSleep';
      $1b: Result := 'Hidd FAT32';
      $1c: Result := 'Hidd FAT32 LBA';
      $1e: Result := 'Hidd FAT16 LBA';
      $24: Result := 'NEC DOS';
      $39: Result := 'Plan 9';
      $3c: Result := 'PMagic recovery';
      $40: Result := 'Venix 80286';
      $41: Result := 'PPC PReP Boot';
      $42: Result := 'SFS';
      $4d: Result := 'QNX4.x';
      $4e: Result := 'QNX4.x 2nd part';
      $4f: Result := 'QNX4.x 3rd part';
      $50: Result := 'OnTrack DM';
      $51: Result := 'OnTrackDM6 Aux1';
      $52: Result := 'CP/M';
      $53: Result := 'OnTrackDM6 Aux3';
      $54: Result := 'OnTrack DM6';
      $55: Result := 'EZ Drive';
      $56: Result := 'Golden Bow';
      $5c: Result := 'Priam Edisk';
      $61: Result := 'SpeedStor';
      $63: Result := 'GNU HURD/SysV';
      $64: Result := 'Netware 286';
      $65: Result := 'Netware 386';
      $70: Result := 'DiskSec MltBoot';
      $75: Result := 'PC/IX';
      $80: Result := 'Minix <1.4a';
      $81: Result := 'Minix >1.4b';
      $82: Result := 'Linux swap';
      $83: Result := 'Linux';
      $84: Result := 'OS/2 hidden C:';
      $85: Result := 'Linux extended';
      $86: Result := 'NTFS volume set';
      $87: Result := 'NTFS volume set';
      $88: Result := 'Linux plaintext';
      $8e: Result := 'Linux LVM';
      $93: Result := 'Amoeba';
      $94: Result := 'Amoeba BBT';
      $9f: Result := 'BSD/OS';
      $a0: Result := 'Thinkpad hib';
      $a5: Result := 'FreeBSD';
      $a6: Result := 'OpenBSD';
      $a7: Result := 'NeXTSTEP';
      $a8: Result := 'Darwin UFS';
      $a9: Result := 'NetBSD';
      $ab: Result := 'Darwin boot';
      $b7: Result := 'BSDI fs';
      $b8: Result := 'BSDI swap';
      $bb: Result := 'Boot Wizard Hid';
      $be: Result := 'Solaris boot';
      $bf: Result := 'Solaris';
      $c1: Result := 'DRDOS/2 FAT12';
      $c4: Result := 'DRDOS/2 smFAT16';
      $c6: Result := 'DRDOS/2 FAT16';
      $c7: Result := 'Syrinx';
      $da: Result := 'Non-FS data';
      $db: Result := 'CP/M / CTOS';
      $de: Result := 'Dell Utility';
      $df: Result := 'BootIt';
      $e1: Result := 'DOS access';
      $e3: Result := 'DOS R/O';
      $e4: Result := 'SpeedStor';
      $eb: Result := 'BeOS fs';
      $ee: Result := 'EFI GPT';
      $ef: Result := 'EFI FAT';
      $f0: Result := 'Lnx/PA-RISC bt';
      $f1: Result := 'SpeedStor';
      $f2: Result := 'DOS secondary';
      $f4: Result := 'SpeedStor';
      $fd: Result := 'Lnx RAID auto';
      $fe: Result := 'LANstep';
      $ff: Result := 'XENIX BBT';
    end;
  end;

end;

function SDUGptPartitionType(PartitionType: TGuid; LongDesc: Boolean): String;
const
  // must be uppercase for comparison
  GPT_GUIDS: array [0..6] of String = (
    '{EBD0A0A2-B9E5-4433-87C0-68B6B72699C7}'
    , '{00000000-0000-0000-0000-000000000000}'
    , '{C12A7328-F81F-11D2-BA4B-00A0C93EC93B}'
    , '{E3C9E316-0B5C-4DB8-817D-F92DF00215AE}'
    , '{5808C8AA-7E8F-42E0-85D2-E1E90434CFB3}'
    , '{AF9B60A0-1431-4F62-BC68-3311714A69AD}'
    , '{DE94BBA4-06D1-4D40-A16A-BFD50179D6AC}'
    );
  GPT_GUID_DESCS: array [0..6] of String = (
    'Windows data partition'
    , 'No partition'
    , 'EFI system'
    , 'Microsoft reserved'
    , 'Logical Disk Manager (LDM) metadata'
    , 'LDM data partition'
    , 'Microsoft recovery'
    );
var
  i: Integer;
begin
  { done -otdk -cenhance : gpt desc }
  // see  https://msdn.microsoft.com/en-us/library/windows/desktop/aa365449%28v=vs.85%29.aspx - or use guid
  Result := GUIDToString(PartitionType);
  assert(length(GPT_GUIDS) = length(GPT_GUID_DESCS));
  for i := low(GPT_GUIDS) to High(GPT_GUIDS) do
    if GPT_GUIDS[i] = Result then begin
      Result := GPT_GUID_DESCS[i];
      break;
    end;
end;


 // ----------------------------------------------------------------------------
 // Get size of partition
 // Returns partition size, or -1 on error
function SDUGetPartitionSize(driveletter: ansichar): ULONGLONG;
begin
  Result := SDUGetPartitionSize_Device(SDUDeviceNameForDrive(driveLetter));
end;


// ----------------------------------------------------------------------------
function SDUGetPartitionSize_Device(driveDevice: String): ULONGLONG;
var
  partInfo: TSDUPartitionInfo;
begin
  Result := 0;

  if SDUGetPartitionInfo(driveDevice, partInfo) then begin
    Result := partInfo.PartitionLength;
  end;

end;


// ----------------------------------------------------------------------------
function SDUGetDiskGeometry(driveletter: ansichar; var diskGeometry: TSDUDiskGeometry): Boolean;
begin
  Result := SDUGetDiskGeometry(SDUDeviceNameForDrive(driveLetter), diskGeometry);
end;

// ----------------------------------------------------------------------------
function SDUGetDiskGeometry(DiskNumber: Integer; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
begin
  Result := SDUGetDiskGeometry(SDUDeviceNameForDisk(DiskNumber), diskGeometry);
end;

 // ----------------------------------------------------------------------------
 // This can be used with (for example):
 //   \\.\C:
 //   \\.\PHYSICALDRIVE2   (i.e. Format(HDD_DISK_DEVICE_NAME_FORMAT, [<diskNo>]);
function SDUGetDiskGeometry(driveDevice: String; var diskGeometry: TSDUDiskGeometry): Boolean;
var
  fileHandle:    THandle;
  DIOCBufferOut: TSDUDiskGeometry;
  bytesReturned: DWORD;
begin
  Result := False;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(driveDevice),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    if (DeviceIoControl(fileHandle, SDU_IOCTL_DISK_GET_DRIVE_GEOMETRY, nil,
      0, @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
      diskGeometry := DIOCBufferOut;
      Result       := True;
    end;

    CloseHandle(fileHandle);
  end;

end;

// ----------------------------------------------------------------------------
function SDUGetPartitionInfo(driveletter: ansichar; var partInfo: TSDUPartitionInfo): Boolean;
begin
  Result := SDUGetPartitionInfo(SDUDeviceNameForDrive(driveLetter), partInfo);
end;


// ----------------------------------------------------------------------------
function SDUGetPartitionInfo(driveDevice: String; var partInfo: TSDUPartitionInfo): Boolean;
var
  fileHandle:    THandle;
  DIOCBufferOut: TSDUPartitionInfo;
  bytesReturned: DWORD;
begin
  Result := False;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(driveDevice),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    if (DeviceIoControl(fileHandle, SDU_IOCTL_DISK_GET_PARTITION_INFO, nil,
      0, @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
      partInfo := DIOCBufferOut;
      Result   := True;
    end;

    CloseHandle(fileHandle);
  end;

end;

// does it  start with '\device\...'?
function IsPartitionPath(path  : string                               ) : Boolean;
begin
  result := (AnsiPos ('\Device\',path) = 1) or (AnsiPos('\\.\PHYSICALDRIVE',path)= 1);
end;

end.
