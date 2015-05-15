unit DriverAPI;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows,  // Required for DWORD
  FreeOTFEDriverConsts;



// #define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
//     ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
// )


type
  TFreeOTFEMountSource = (fomsFile, fomsPartition, fomsUnknown);
  
  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //          *must* all be kept in sync with each other at all times
  TFreeOTFESectorIVGenMethod = (
                          foivgNone,
                          foivg32BitSectorID,
                          foivg64BitSectorID,
                          foivgHash32BitSectorID,
                          foivgHash64BitSectorID,
                          foivgESSIV,
                          foivgUnknown
                         );


  // This is taken from "ntdddisk.h" from the MS Windows DDK
  TFreeOTFEStorageMediaType = (
    mtUnknown,                // Format is unknown
    mtF5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    mtF3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    mtF3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    mtF3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    mtF3_720_512,             // 3.5",  720KB,  512 bytes/sector
    mtF5_360_512,             // 5.25", 360KB,  512 bytes/sector
    mtF5_320_512,             // 5.25", 320KB,  512 bytes/sector
    mtF5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    mtF5_180_512,             // 5.25", 180KB,  512 bytes/sector
    mtF5_160_512,             // 5.25", 160KB,  512 bytes/sector
    mtRemovableMedia,         // Removable media other than floppy
    mtFixedMedia,             // Fixed hard disk media
    mtF3_120M_512,            // 3.5", 120M Floppy
    mtF3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
    mtF5_640_512,             // 5.25",  640KB,  512 bytes/sector
    mtF5_720_512,             // 5.25",  720KB,  512 bytes/sector
    mtF3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
    mtF3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
    mtF5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
    mtF3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
    mtF3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
    mtF8_256_128,             // 8",     256KB,  128 bytes/sector
    mtF3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
    mtF3_240M_512,            // 3.5",   240Mb Floppy (HiFD)
    mtF3_32M_512,             // 3.5",   32Mb Floppy

    mtDDS_4mm,                   // Tape - DAT DDS1,2,... (all vendors)
    mtMiniQic,                   // Tape - miniQIC Tape
    mtTravan,                    // Tape - Travan TR-1,2,3,...
    mtQIC,                       // Tape - QIC
    mtMP_8mm,                    // Tape - 8mm Exabyte Metal Particle
    mtAME_8mm,                   // Tape - 8mm Exabyte Advanced Metal Evap
    mtAIT1_8mm,                  // Tape - 8mm Sony AIT
    mtDLT,                       // Tape - DLT Compact IIIxt, IV
    mtNCTP,                      // Tape - Philips NCTP
    mtIBM_3480,                  // Tape - IBM 3480
    mtIBM_3490E,                 // Tape - IBM 3490E
    mtIBM_Magstar_3590,          // Tape - IBM Magstar 3590
    mtIBM_Magstar_MP,            // Tape - IBM Magstar MP
    mtSTK_DATA_D3,               // Tape - STK Data D3
    mtSONY_DTF,                  // Tape - Sony DTF
    mtDV_6mm,                    // Tape - 6mm Digital Video
    mtDMI,                       // Tape - Exabyte DMI and compatibles
    mtSONY_D2,                   // Tape - Sony D2S and D2L
    mtCLEANER_CARTRIDGE,         // Cleaner - All Drive types that support Drive Cleaners
    mtCD_ROM,                    // Opt_Disk - CD
    mtCD_R,                      // Opt_Disk - CD-Recordable (Write Once)
    mtCD_RW,                     // Opt_Disk - CD-Rewriteable
    mtDVD_ROM,                   // Opt_Disk - DVD-ROM
    mtDVD_R,                     // Opt_Disk - DVD-Recordable (Write Once)
    mtDVD_RW,                    // Opt_Disk - DVD-Rewriteable
    mtMO_3_RW,                   // Opt_Disk - 3.5" Rewriteable MO Disk
    mtMO_5_WO,                   // Opt_Disk - MO 5.25" Write Once
    mtMO_5_RW,                   // Opt_Disk - MO 5.25" Rewriteable (not LIMDOW)
    mtMO_5_LIMDOW,               // Opt_Disk - MO 5.25" Rewriteable (LIMDOW)
    mtPC_5_WO,                   // Opt_Disk - Phase Change 5.25" Write Once Optical
    mtPC_5_RW,                   // Opt_Disk - Phase Change 5.25" Rewriteable
    mtPD_5_RW,                   // Opt_Disk - PhaseChange Dual Rewriteable
    mtABL_5_WO,                  // Opt_Disk - Ablative 5.25" Write Once Optical
    mtPINNACLE_APEX_5_RW,        // Opt_Disk - Pinnacle Apex 4.6GB Rewriteable Optical
    mtSONY_12_WO,                // Opt_Disk - Sony 12" Write Once
    mtPHILIPS_12_WO,             // Opt_Disk - Philips/LMS 12" Write Once
    mtHITACHI_12_WO,             // Opt_Disk - Hitachi 12" Write Once
    mtCYGNET_12_WO,              // Opt_Disk - Cygnet/ATG 12" Write Once
    mtKODAK_14_WO,               // Opt_Disk - Kodak 14" Write Once
    mtMO_NFR_525,                // Opt_Disk - Near Field Recording (Terastor)
    mtNIKON_12_RW,               // Opt_Disk - Nikon 12" Rewriteable
    mtIOMEGA_ZIP,                // Mag_Disk - Iomega Zip
    mtIOMEGA_JAZ,                // Mag_Disk - Iomega Jaz
    mtSYQUEST_EZ135,             // Mag_Disk - Syquest EZ135
    mtSYQUEST_EZFLYER,           // Mag_Disk - Syquest EzFlyer
    mtSYQUEST_SYJET,             // Mag_Disk - Syquest SyJet
    mtAVATAR_F2,                 // Mag_Disk - 2.5" Floppy
    mtMP2_8mm,                   // Tape - 8mm Hitachi
    mtDST_S,                     // Ampex DST Small Tapes
    mtDST_M,                     // Ampex DST Medium Tapes
    mtDST_L,                     // Ampex DST Large Tapes
    mtVXATape_1,                 // Ecrix 8mm Tape
    mtVXATape_2,                 // Ecrix 8mm Tape
    mtSTK_9840,                  // STK 9840
    mtLTO_Ultrium,               // IBM, HP, Seagate LTO Ultrium
    mtLTO_Accelis,               // IBM, HP, Seagate LTO Accelis
    mtDVD_RAM,                   // Opt_Disk - DVD-RAM
    mtAIT_8mm,                   // AIT2 or higher
    mtADR_1,                     // OnStream ADR Mediatypes
    mtADR_2
    );



resourcestring
  RS_MOUNTSOURCE_FILE      = 'File';
  RS_MOUNTSOURCE_PARTITION = 'Partition';
  RS_MOUNTSOURCE_UNKNOWN   = 'Unknown';
const
  FreeOTFEMountSourceTitle : array [TFreeOTFEMountSource] of string = (
                                                                     RS_MOUNTSOURCE_FILE,
                                                                     RS_MOUNTSOURCE_PARTITION,
                                                                     RS_MOUNTSOURCE_UNKNOWN
                                                                    );

  FreeOTFEMountSourceID : array [TFreeOTFEMountSource] of integer = (
                                                       1,  // MNTSRC_FILE
                                                       2,  // MNTSRC_PARTITION
                                                    9999   // MNTSRC_FILE_UNKNOWN
                                                                    );

resourcestring
  RS_SECTORIVGENMETHOD_NULL_IV             = 'Null IV';
  RS_SECTORIVGENMETHOD_SECTOR_ID_32        = '32 bit sector ID';
  RS_SECTORIVGENMETHOD_SECTOR_ID_64        = '64 bit sector ID';
  RS_SECTORIVGENMETHOD_HASHED_SECTOR_ID_32 = 'Hashed 32 bit sector ID';
  RS_SECTORIVGENMETHOD_HASHED_SECTOR_ID_64 = 'Hashed 64 bit sector ID';
  RS_SECTORIVGENMETHOD_ESSIV               = 'ESSIV';
  RS_SECTORIVGENMETHOD_UNKNOWN             = 'Unknown';
const
  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //          *must* all be kept in sync with each other at all times
  FreeOTFESectorIVGenMethodTitle : array [TFreeOTFESectorIVGenMethod] of string = (
                                                                     RS_SECTORIVGENMETHOD_NULL_IV,
                                                                     RS_SECTORIVGENMETHOD_SECTOR_ID_32,
                                                                     RS_SECTORIVGENMETHOD_SECTOR_ID_64,
                                                                     RS_SECTORIVGENMETHOD_HASHED_SECTOR_ID_32,
                                                                     RS_SECTORIVGENMETHOD_HASHED_SECTOR_ID_64,
                                                                     RS_SECTORIVGENMETHOD_ESSIV,
                                                                     RS_SECTORIVGENMETHOD_UNKNOWN
                                                                    );

  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //          *must* all be kept in sync with each other at all times
  FreeOTFESectorIVGenMethodID : array [TFreeOTFESectorIVGenMethod] of byte = (
                                                       0,  // IVGEN_NONE
                                                       1,  // IVGEN_32BIT_SECTOR_ID
                                                       2,  // IVGEN_64BIT_SECTOR_ID
                                                       3,  // IVGEN_HASH_32BIT_SECTOR_ID
                                                       4,  // IVGEN_HASH_64BIT_SECTOR_ID
                                                       5,  // IVGEN_ESSIV
                                                     255   // IVGEN_UNKNOWN
                                                                    );


  FreeOTFEStorageMediaTypeTitle : array [TFreeOTFEStorageMediaType] of string = (
                                          'Format is unknown',
                                          '5.25", 1.2MB,  512 bytes/sector',
                                          '3.5",  1.44MB, 512 bytes/sector',
                                          '3.5",  2.88MB, 512 bytes/sector',
                                          '3.5",  20.8MB, 512 bytes/sector',
                                          '3.5",  720KB,  512 bytes/sector',
                                          '5.25", 360KB,  512 bytes/sector',
                                          '5.25", 320KB,  512 bytes/sector',
                                          '5.25", 320KB,  1024 bytes/sector',
                                          '5.25", 180KB,  512 bytes/sector',
                                          '5.25", 160KB,  512 bytes/sector',
                                          'Removable media other than floppy',
                                          'Fixed hard disk media',
                                          '3.5", 120M Floppy',
                                          '3.5" ,  640KB,  512 bytes/sector',
                                          '5.25",  640KB,  512 bytes/sector',
                                          '5.25",  720KB,  512 bytes/sector',
                                          '3.5" ,  1.2Mb,  512 bytes/sector',
                                          '3.5" ,  1.23Mb, 1024 bytes/sector',
                                          '5.25",  1.23MB, 1024 bytes/sector',
                                          '3.5" MO 128Mb   512 bytes/sector',
                                          '3.5" MO 230Mb   512 bytes/sector',
                                          '8",     256KB,  128 bytes/sector',
                                          '3.5",   200M Floppy (HiFD)',
                                          '3.5",   240Mb Floppy (HiFD)',
                                          '3.5",   32Mb Floppy',

                                          'Tape - DAT DDS1,2,... (all vendors)',
                                          'Tape - miniQIC Tape',
                                          'Tape - Travan TR-1,2,3,...',
                                          'Tape - QIC',
                                          'Tape - 8mm Exabyte Metal Particle',
                                          'Tape - 8mm Exabyte Advanced Metal Evap',
                                          'Tape - 8mm Sony AIT',
                                          'Tape - DLT Compact IIIxt, IV',
                                          'Tape - Philips NCTP',
                                          'Tape - IBM 3480',
                                          'Tape - IBM 3490E',
                                          'Tape - IBM Magstar 3590',
                                          'Tape - IBM Magstar MP',
                                          'Tape - STK Data D3',
                                          'Tape - Sony DTF',
                                          'Tape - 6mm Digital Video',
                                          'Tape - Exabyte DMI and compatibles',
                                          'Tape - Sony D2S and D2L',
                                          'Cleaner - All Drive types that support Drive Cleaners',
                                          'Opt_Disk - CD',
                                          'Opt_Disk - CD-Recordable (Write Once)',
                                          'Opt_Disk - CD-Rewriteable',
                                          'Opt_Disk - DVD-ROM',
                                          'Opt_Disk - DVD-Recordable (Write Once)',
                                          'Opt_Disk - DVD-Rewriteable',
                                          'Opt_Disk - 3.5" Rewriteable MO Disk',
                                          'Opt_Disk - MO 5.25" Write Once',
                                          'Opt_Disk - MO 5.25" Rewriteable (not LIMDOW)',
                                          'Opt_Disk - MO 5.25" Rewriteable (LIMDOW)',
                                          'Opt_Disk - Phase Change 5.25" Write Once Optical',
                                          'Opt_Disk - Phase Change 5.25" Rewriteable',
                                          'Opt_Disk - PhaseChange Dual Rewriteable',
                                          'Opt_Disk - Ablative 5.25" Write Once Optical',
                                          'Opt_Disk - Pinnacle Apex 4.6GB Rewriteable Optical',
                                          'Opt_Disk - Sony 12" Write Once',
                                          'Opt_Disk - Philips/LMS 12" Write Once',
                                          'Opt_Disk - Hitachi 12" Write Once',
                                          'Opt_Disk - Cygnet/ATG 12" Write Once',
                                          'Opt_Disk - Kodak 14" Write Once',
                                          'Opt_Disk - Near Field Recording (Terastor)',
                                          'Opt_Disk - Nikon 12" Rewriteable',
                                          'Mag_Disk - Iomega Zip',
                                          'Mag_Disk - Iomega Jaz',
                                          'Mag_Disk - Syquest EZ135',
                                          'Mag_Disk - Syquest EzFlyer',
                                          'Mag_Disk - Syquest SyJet',
                                          'Mag_Disk - 2.5" Floppy',
                                          'Tape - 8mm Hitachi',
                                          'Ampex DST Small Tapes',
                                          'Ampex DST Medium Tapes',
                                          'Ampex DST Large Tapes',
                                          'Ecrix 8mm Tape',
                                          'Ecrix 8mm Tape',
                                          'STK 9840',
                                          'IBM, HP, Seagate LTO Ultrium',
                                          'IBM, HP, Seagate LTO Accelis',
                                          'Opt_Disk - DVD-RAM',
                                          'AIT2 or higher',
                                          'OnStream ADR Mediatypes (1)',
                                          'OnStream ADR Mediatypes (2)'
                                                                    );

  FreeOTFEStorageMediaTypeID : array [TFreeOTFEStorageMediaType] of integer = (
                                                                      0,
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      4,
                                                                      5,
                                                                      6,
                                                                      7,
                                                                      8,
                                                                      9,
                                                                     10,
                                                                     11,
                                                                     12,
                                                                     13,
                                                                     14,
                                                                     15,
                                                                     16,
                                                                     17,
                                                                     18,
                                                                     19,
                                                                     20,
                                                                     21,
                                                                     22,
                                                                     23,
                                                                     24,
                                                                     25,

                                                                     32, // 0x20
                                                                     33,
                                                                     34,
                                                                     35,
                                                                     36,
                                                                     37,
                                                                     38,
                                                                     39,
                                                                     40,
                                                                     41,
                                                                     42,
                                                                     43,
                                                                     44,
                                                                     45,
                                                                     46,
                                                                     47,
                                                                     48,
                                                                     49,
                                                                     50,
                                                                     51,
                                                                     52,
                                                                     53,
                                                                     54,
                                                                     55,
                                                                     56,
                                                                     57,
                                                                     58,
                                                                     59,
                                                                     60,
                                                                     61,
                                                                     62,
                                                                     63,
                                                                     64,
                                                                     65,
                                                                     66,
                                                                     67,
                                                                     68,
                                                                     69,
                                                                     70,
                                                                     71,
                                                                     72,
                                                                     73,
                                                                     74,
                                                                     75,
                                                                     76,
                                                                     77,
                                                                     78,
                                                                     79,
                                                                     80,
                                                                     81,
                                                                     82,
                                                                     83,
                                                                     84,
                                                                     85,
                                                                     86,
                                                                     87,
                                                                     88,
                                                                     89,
                                                                     90,
                                                                     91
                                                                );


type
  // This definition is stored here, although it would be better if it was
  // stored in OTFEFreeOTFE_U, so only that unit need be "used" by user
  // applications
  TFreeOTFEMACAlgorithm = (fomacHash, fomacHMAC, fomacUnknown);

  // This definition is stored here, although it would be better if it was
  // stored in OTFEFreeOTFE_U, so only that unit need be "used" by user
  // applications
  TFreeOTFEKDFAlgorithm = (fokdfHashWithSalt, fokdfPBKDF2, fokdfUnknown);


const


// Various version IDs returned by the main FreeOTFE driver
// Unused consts are commented out
//FREEOTFE_ID_v00_50_0000 = (00 * $01000000) + (50 * $00010000) + (0000 * $00000001);
//FREEOTFE_ID_v00_53_0000 = (00 * $01000000) + (53 * $00010000) + (0000 * $00000001);
//FREEOTFE_ID_v00_54_0000 = (00 * $01000000) + (54 * $00010000) + (0000 * $00000001);
//FREEOTFE_ID_v00_56_0000 = (00 * $01000000) + (56 * $00010000) + (0000 * $00000001);
//FREEOTFE_ID_v00_57_0000 = (00 * $01000000) + (57 * $00010000) + (0000 * $00000001);
FREEOTFE_ID_v00_58_0000 = (00 * $01000000) + (58 * $00010000) + (0000 * $00000001);
FREEOTFE_ID_v01_60_0000 = (01 * $01000000) + (60 * $00010000) + (0000 * $00000001);
FREEOTFE_ID_v02_00_0000 = (02 * $01000000) + (00 * $00010000) + (0000 * $00000001);
FREEOTFE_ID_v03_00_0000 = (03 * $01000000) + (00 * $00010000) + (0000 * $00000001);
FREEOTFE_ID_v04_30_0000 = (04 * $01000000) + (30 * $00010000) + (0000 * $00000001);

// If the version ID couldn't be determined
FREEOTFE_ID_FAILURE = $FFFFFFFF;

// From FreeOTFEAPI.h

// Length in characters (not bytes)
FREEOTFE_MAX_FILENAME_LENGTH = 1024;


MAIN_DEVICE_NAME              = 'FreeOTFE';
DEVICE_BASE_NAME              = '\' + MAIN_DEVICE_NAME;
DEVICE_FREEOTFE_ROOT          = '\Device' + DEVICE_BASE_NAME;
//DEVICE_SYMLINK_FREEOTFE_ROOT  = '\DosDevices';
// Note: THERE IS NO TRAILING SLASH ON THIS ONE - THE NAMES WHICH GET APPENDED
//       ONTO THE END ALL START WITH THAT "\"
DEVICE_SYMLINK_FREEOTFE_ROOT  = '\\.';


// Main device
DEVICE_MAIN_NAME              = DEVICE_FREEOTFE_ROOT + DEVICE_BASE_NAME;
DEVICE_SYMLINK_MAIN_NAME      = DEVICE_SYMLINK_FREEOTFE_ROOT + DEVICE_BASE_NAME;

// Disk devices
DEVICE_DISK_DIR_NAME          = DEVICE_FREEOTFE_ROOT + '\Disks';
DEVICE_DISK_PREFIX            = DEVICE_DISK_DIR_NAME + '\Disk';
// The following is just padding to represent the number tacked onto the
// end of DEVICE_DISK_PREFIX
DEVICE_DISK_NUMBER            = 'zzzzz';



  NULL_GUID = '{00000000-0000-0000-0000-000000000000}';



  // The length of the critical data section, INCLUDING ALL PADDING
  // If this is changed, the following functions will also probably have to be changed:
  //   BackupVolumeCriticalData(...)
  //   RestoreVolumeCriticalData(...)
  //   ChangeVolumePassword(...)
  CRITICAL_DATA_LENGTH = 4096;  // In *bits*

  // In bits
  CDB_MAX_MAC_LENGTH = 512;



  // Volume flags
  // Bit 0 unused (1)
  VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART =  2;  // Bit 1
  // Bit 2 unused (4)
  // Bit 3 unused (8)
  VOL_FLAGS_NORMAL_TIMESTAMPS       = 16;  // Bit 4


const IOCTL_FREEOTFE_VERSION =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($800) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_CREATE =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($805) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFE_SET_RAW =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($809) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFE_GET_RAW =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($80A) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFE_MOUNT =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($801) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_DISMOUNT =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($802) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_DESTROY =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($806) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_GET_DISK_DEVICE_COUNT =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($807) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_GET_DISK_DEVICE_LIST =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($808) * $4) OR (METHOD_BUFFERED));


const IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($803) * $4) OR (METHOD_BUFFERED));

const IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA =
         (((FILE_DEVICE_DISK) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($804) * $4) OR (METHOD_BUFFERED));

// Derive a key
const IOCTL_FREEOTFE_DERIVE_KEY =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($810) * $4) OR (METHOD_BUFFERED));

// Generate a MAC
const IOCTL_FREEOTFE_GENERATE_MAC =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($811) * $4) OR (METHOD_BUFFERED));

// Encrypt a block
const IOCTL_FREEOTFE_ENCRYPT_BLOCK =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($812) * $4) OR (METHOD_BUFFERED));

// Decrypt a block
const IOCTL_FREEOTFE_DECRYPT_BLOCK =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($813) * $4) OR (METHOD_BUFFERED));

// Lock, dismount, remove, eject and unlock
// Originally added for Vista, where the user can't do this if UAC is
// operational, and the userspace software isn't running with escalated privs
const IOCTL_FREEOTFE_LDREU =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($814) * $4) OR (METHOD_BUFFERED));

// Create DOS symlink for device
const IOCTL_FREEOTFE_CREATE_DOS_MOUNTPOINT =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($815) * $4) OR (METHOD_BUFFERED));

// Delete DOS symlink for device
const IOCTL_FREEOTFE_DELETE_DOS_MOUNTPOINT =
         (((FILE_DEVICE_UNKNOWN) * $10000) OR ((FILE_READ_DATA) * $4000) OR (($816) * $4) OR (METHOD_BUFFERED));


type

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_VERSION = ^TDIOC_VERSION;
  TDIOC_VERSION = record
    VersionID: DWORD;
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DISK_DEVICE_CREATE = ^TDIOC_DISK_DEVICE_CREATE;
  TDIOC_DISK_DEVICE_CREATE = record
    DeviceType: DWORD;  // Set to one of the system defined FILE_DEVICE_XXX
                        // constants
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DEVICE_NAME = ^TDIOC_DEVICE_NAME;
  TDIOC_DEVICE_NAME = record
    DeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of aNSIchar;
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DEVICE_NAME_LIST = ^TDIOC_DEVICE_NAME_LIST;
  TDIOC_DEVICE_NAME_LIST = record
    DeviceCount: DWORD;
    DeviceName: array [0..0] of array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of aNSIchar;  // Variable length
  end;



  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_MOUNT_PC_DRIVER = ^TDIOC_MOUNT_PC_DRIVER;
  TDIOC_MOUNT_PC_DRIVER = record
    DiskDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of aNSIchar;

    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    // Start and end of encrypted data within the file
    // If DataEnd is zero, it will be set to the last byte in the file
    DataStart: int64;
    DataEnd: int64;

    IVHashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    IVHashGUID: TGUID;
    IVCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    IVCypherGUID: TGUID;

    MainCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    MainCypherGUID: TGUID;

    ReadOnly: boolean;
    MountSource: integer;  //  An ID taken from FreeOTFEMountSourceID
    StorageMediaType: integer;  //  An ID taken from FreeOTFEMStoragemediaTypeID

    VolumeFlags: DWORD;
    SectorIVGenMethod: integer;

    MasterKeyLength: cardinal;  // In bits
    VolumeIVLength: cardinal;  // In bits
    MetaDataLength: cardinal;  // In *bytes*
    MasterKey: array [0..0] of byte;  // Variable length array
    VolumeIV: array [0..0] of byte;  // Variable length array
    MetaData: array [0..0] of byte;  // Variable length array
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_MOUNT_PC_DLL = ^TDIOC_MOUNT_PC_DLL;
  TDIOC_MOUNT_PC_DLL = record
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    // Start and end of encrypted data within the file
    // If DataEnd is zero, it will be set to the last byte in the file
    DataStart: int64;
    DataEnd: int64;

    IVHashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    IVHashGUID: TGUID;
    IVCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    IVCypherGUID: TGUID;

    MainCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    MainCypherGUID: TGUID;

    ReadOnly: boolean;
    MountSource: integer;  //  An ID taken from FreeOTFEMountSourceID

    VolumeFlags: DWORD;
    SectorIVGenMethod: integer;

    MasterKeyLength: cardinal;  // In bits
    VolumeIVLength: cardinal;  // In bits
    MetaDataLength: cardinal;  // In *bytes*
    MasterKey: array [0..0] of byte;  // Variable length array
    VolumeIV: array [0..0] of byte;  // Variable length array
    MetaData: array [0..0] of byte;  // Variable length array
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DISMOUNT = ^TDIOC_DISMOUNT;
  TDIOC_DISMOUNT = record
    DiskDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Emergency: boolean;
  end;


  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DISK_DEVICE_COUNT = ^TDIOC_DISK_DEVICE_COUNT;
  TDIOC_DISK_DEVICE_COUNT = record
    Count: cardinal;
  end;


  PDIOC_DISK_DEVICE_STATUS_PC_DRIVER = ^TDIOC_DISK_DEVICE_STATUS_PC_DRIVER;
  TDIOC_DISK_DEVICE_STATUS_PC_DRIVER = packed record
    DiskDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Mounted: boolean;

    IVHashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    junk_padding1: array [1..3] of byte;  // Weird, but needed?!
    IVHashGUID: TGUID;

    IVCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    IVCypherGUID: TGUID;

    MainCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    MainCypherGUID: TGUID;

    DismountPending: boolean;
    ReadOnly: boolean;
    junk_padding3: array [1..2] of byte;

    VolumeFlags: DWORD;
    SectorIVGenMethod: integer;

    MetaDataLength: cardinal;  // In *bytes*
  end;

  PDIOC_DISK_DEVICE_STATUS_PC_DLL = ^TDIOC_DISK_DEVICE_STATUS_PC_DLL;
  TDIOC_DISK_DEVICE_STATUS_PC_DLL = packed record
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;

    IVHashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    IVHashGUID: TGUID;

    IVCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    IVCypherGUID: TGUID;

    MainCypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    MainCypherGUID: TGUID;

    DismountPending: boolean;
    ReadOnly: boolean;
    junk_padding3: array [1..2] of byte;

    VolumeFlags: DWORD;
    SectorIVGenMethod: integer;

    MetaDataLength: cardinal;  // In *bytes*
  end;


  PDIOC_DISK_DEVICE_METADATA = ^TDIOC_DISK_DEVICE_METADATA;
  TDIOC_DISK_DEVICE_METADATA = record
    MetaDataLength: cardinal;  // In *bytes*
    MetaData: array [0..0] of byte;  // Variable length array
  end;



  PDIOC_FILENAME = ^TDIOC_FILENAME;
  TDIOC_FILENAME = record
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
  end;

  PDIOC_SET_RAW_DATA = ^TDIOC_SET_RAW_DATA;
  TDIOC_SET_RAW_DATA = record
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Offset: int64;
    DataLength: integer;
    Data: array [0..0] of byte;
  end;

  PDIOC_GET_RAW_DATA_IN = ^TDIOC_GET_RAW_DATA_IN;
  TDIOC_GET_RAW_DATA_IN = record
    Filename: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Offset: int64;
    DataLength: integer;
  end;

  PDIOC_GET_RAW_DATA_OUT = ^TDIOC_GET_RAW_DATA_OUT;
  TDIOC_GET_RAW_DATA_OUT = record
    Data: array [0..0] of byte;
  end;



  PDIOC_DERIVE_KEY_IN_PC_DRIVER = ^TDIOC_DERIVE_KEY_IN_PC_DRIVER;
  TDIOC_DERIVE_KEY_IN_PC_DRIVER = record
    KDFAlgorithm: integer;

    HashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    HashGUID: TGUID;

    CypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    CypherGUID: TGUID;

    Iterations: integer;
    LengthWanted: integer;  // In bits - the length of the output key wanted

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "Password", "SaltLength", "Salt", then we could not access
    //       "SaltLength" directly, and would have to take PasswordLength into account before
    //       accessing it.
    PasswordLength: integer;  // In bits
    SaltLength: integer;  // In bits

    Password: array [0..0] of AnsiChar;  // Variable length array
    Salt: array [0..0] of AnsiChar;  // Variable length array
  end;

  PDIOC_DERIVE_KEY_IN_PC_DLL = ^TDIOC_DERIVE_KEY_IN_PC_DLL;
  TDIOC_DERIVE_KEY_IN_PC_DLL = record
    KDFAlgorithm: integer;

    HashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    HashGUID: TGUID;

    CypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    CypherGUID: TGUID;

    Iterations: integer;
    LengthWanted: integer;  // In bits - the length of the output key wanted

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "Password", "SaltLength", "Salt", then we could not access
    //       "SaltLength" directly, and would have to take PasswordLength into account before
    //       accessing it.
    PasswordLength: integer;  // In bits
    SaltLength: integer;  // In bits

    Password: array [0..0] of byte;  // Variable length array
    Salt: array [0..0] of byte;  // Variable length array
  end;

  PDIOC_DERIVE_KEY_OUT = ^TDIOC_DERIVE_KEY_OUT;
  TDIOC_DERIVE_KEY_OUT = record
    DerivedKeyLength: integer;  // In bits
    DerivedKey: array [0..0] of byte;  // Variable length array
  end;

  

  PDIOC_GENERATE_MAC_IN_PC_DRIVER = ^TDIOC_GENERATE_MAC_IN_PC_DRIVER;
  TDIOC_GENERATE_MAC_IN_PC_DRIVER = record
    MACAlgorithm: integer;

    HashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of aNSIchar;
    HashGUID: TGUID;

    CypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of aNSIchar;
    CypherGUID: TGUID;

    LengthWanted: integer;  // In bits - the length of the output key wanted

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "Data", "SaltLength", "Salt", then we could not access
    //       "SaltLength" directly, and would have to take DataLength into account before
    //       accessing it.
    KeyLength: integer;  // In bits
    DataLength: integer;  // In bits

    Key: array [0..0] of byte;  // Variable length array
    Data: array [0..0] of byte;  // Variable length array
  end;

  PDIOC_GENERATE_MAC_IN_PC_DLL = ^TDIOC_GENERATE_MAC_IN_PC_DLL;
  TDIOC_GENERATE_MAC_IN_PC_DLL = record
    MACAlgorithm: integer;

    HashDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    HashGUID: TGUID;

    CypherDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of WChar;
    CypherGUID: TGUID;

    LengthWanted: integer;  // In bits - the length of the output key wanted

    // NOTE: THE ORDER OF THE FOLLOWING ITEMS IS SIGNIFICANT
    //       We have the lengths first, *then* the data as this makes checking the size of the
    //       struct easier, since these are both known-width members and can therefore
    //       be accessed directly using "variable->SaltLength".
    //       If the order was: "Data", "SaltLength", "Salt", then we could not access
    //       "SaltLength" directly, and would have to take DataLength into account before
    //       accessing it.
    KeyLength: integer;  // In bits
    DataLength: integer;  // In bits

    Key: array [0..0] of byte;  // Variable length array
    Data: array [0..0] of byte;  // Variable length array
  end;

  PDIOC_GENERATE_MAC_OUT = ^TDIOC_GENERATE_MAC_OUT;
  TDIOC_GENERATE_MAC_OUT = record
    MACLength: integer;  // In bits
    MAC: array [0..0] of byte;  // Variable length array
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_LDREU = ^TDIOC_LDREU;
  TDIOC_LDREU = record
    DriveFile: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    DiskDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Emergency: boolean;
  end;

  // THIS DEPENDS ON DELPHI ALIGNING VALUES IN A RECORD ON WORD BOUNDRIES!!!
  //   - i.e. it's not a "packed record"
  PDIOC_DOS_MOUNTPOINT = ^TDIOC_DOS_MOUNTPOINT;
  TDIOC_DOS_MOUNTPOINT = record
    DiskDeviceName: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
    Global: boolean;
    Mountpoint: array [0..FREEOTFE_MAX_FILENAME_LENGTH-1] of AnsiChar;
  end;


implementation

END.


