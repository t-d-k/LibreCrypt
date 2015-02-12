unit SDFilesystem_FAT;

interface

uses
  Classes, Controls, Dialogs, SDFilesystem, SDPartitionImage,
  SDUClasses,
  SDUDialogs,
  SDUGeneral,
  SyncObjs,
  SysUtils, Windows;

const
  FAT_INVALID_FILENAME_CHARS = '\/:*?"<>|';

  VFAT_ATTRIB_FLAG_READONLY = $01;
  VFAT_ATTRIB_FLAG_HIDDEN   = $02;
  VFAT_ATTRIB_FLAG_SYSTEM   = $04;
  VFAT_ATTRIB_FLAG_VOLLABEL = $08;
  VFAT_ATTRIB_FLAG_SUBDIR   = $10;
  VFAT_ATTRIB_FLAG_ARCHIVE  = $20;
  VFAT_ATTRIB_FLAG_DEVICE   = $40;
  VFAT_ATTRIB_FLAG_UNUSED   = $80;
  VFAT_ATTRIB_VFAT_ENTRY    = VFAT_ATTRIB_FLAG_VOLLABEL or VFAT_ATTRIB_FLAG_SYSTEM or
    VFAT_ATTRIB_FLAG_HIDDEN or VFAT_ATTRIB_FLAG_READONLY;

resourcestring
  FAT_TITLE_FAT12 = 'FAT12';
  FAT_TITLE_FAT16 = 'FAT16';
  FAT_TITLE_FAT32 = 'FAT32';

type
  TSDDirItem_FAT = class (TSDDirItem)
  PRIVATE
    FAttributes: Byte;
  PROTECTED
    function CheckAttr(attr: Byte): Boolean;
    procedure SetAttr(attr: Byte; Value: Boolean);

    function GetIsFile(): Boolean; OVERRIDE;
    procedure SetIsFile(Value: Boolean); OVERRIDE;
    function GetIsDirectory(): Boolean; OVERRIDE;
    procedure SetIsDirectory(Value: Boolean); OVERRIDE;

    function GetIsReadonly(): Boolean; OVERRIDE;
    procedure SetIsReadonly(Value: Boolean); OVERRIDE;
    function GetIsHidden(): Boolean; OVERRIDE;
    procedure SetIsHidden(Value: Boolean); OVERRIDE;

    function GetIsArchive(): Boolean;
    procedure SetIsArchive(Value: Boolean);
    function GetIsSystem(): Boolean;
    procedure SetIsSystem(Value: Boolean);

    function GetAttributes(): Byte;
    procedure SetAttributes(Value: Byte);

    function GetIsVolumeLabel(): Boolean;
    procedure SetIsVolumeLabel(Value: Boolean);
  PUBLIC
    FilenameDOS: Ansistring;

    TimestampCreation:   TTimeStamp;
    DatestampLastAccess: TDate;
    FirstCluster:        DWORD;

    procedure Assign(srcItem: TSDDirItem_FAT); OVERLOAD;

  PUBLISHED
    property IsVolumeLabel: Boolean Read GetIsVolumeLabel Write SetIsVolumeLabel;
    property IsArchive: Boolean Read GetIsArchive Write SetIsArchive;
    property IsSystem: Boolean Read GetIsSystem Write SetIsSystem;

    property Attributes: Byte Read GetAttributes Write SetAttributes;

  end;

  TFATType = (ftFAT12, ftFAT16, ftFAT32);

const
  FATTypeTitlePtr: array [TFATType] of Pointer =
    (@FAT_TITLE_FAT12, @FAT_TITLE_FAT16, @FAT_TITLE_FAT32
    );

type
  TSDFATClusterChain = array of DWORD;

  // Boot sector information
  TSDBootSector_FAT = record
    FATType: TFATType;

    JMP:                 array [1..3] of Byte;
    OEMName:             Ansistring;
    BytesPerSector:      Word;
    SectorsPerCluster:   Byte;
    ReservedSectorCount: Word;
    FATCount:            Byte;
    MaxRootEntries:      Word;
    TotalSectors:        DWORD;
    MediaDescriptor:     Byte;
    SectorsPerFAT:       DWORD;
    SectorsPerTrack:     Word;
    NumberOfHeads:       Word;
    HiddenSectors:       DWORD;

    PhysicalDriveNo:   Byte;
    ExtendedBootSig:   Byte;
    FATFilesystemType: Ansistring;
    SerialNumber:      DWORD;
    VolumeLabel:       Ansistring;
    BootSectorSig:     Word;

    // FAT32 only
    FATFlags:               Word;
    Version:                Word;
    SectorNoFSInfoSector:   Word;
    SectorNoBootSectorCopy: Word;

    RootDirFirstCluster: DWORD;
  end;

  TSDFilesystem_FAT = class (TSDCustomFilesystemPartitionBased)
  PRIVATE
    FPreserveTimeDateStamps: Boolean;

    function GetFATEntry_FAT12(clusterID: DWORD): DWORD;
    function GetFATEntry_FAT1632(clusterID: DWORD): DWORD;
    function SetFATEntry_FAT12(clusterID: DWORD; Value: DWORD): Boolean;
    function SetFATEntry_FAT1632(clusterID: DWORD; Value: DWORD): Boolean;

    // The next two shouldn't be called directly; internal use...
    function _ReadWriteClusterData(readNotWrite: Boolean; clusterID: DWORD;
      data: TStream; maxSize: Integer): Boolean;
    function _ReadWriteClusterChainData(readNotWrite: Boolean; chain: TSDFATClusterChain;
      data: TStream; maxSize: Int64 = -1): Boolean;

  PROTECTED
    FBootSectorSummary: TSDBootSector_FAT;

    FFAT:               TSDUMemoryStream;
                                      // IMPORTANT: Access to this must be protected by FSerializeCS
    FFATEntrySize:      DWORD;        // Size of each FAT entry
    FFATEntryMask:      DWORD;
    FFATEntryFree:      DWORD;        // Free (unused) FAT entry
    FFATEntryUsedStart: DWORD;        // Very first valid cluster number
    FFATEntryUsedEnd:   DWORD;        // Very lsat valid cluster number
    FFATEntryBadSector: DWORD;        // Cluster contains bad sector
    FFATEntryEOCStart:  DWORD;        // End of cluster chain
    FFATEntryEOCEnd:    DWORD;        // End of cluster chain
    procedure SetupFFATEntryValues(SetupAsFATType: TFATType);

    function ExtractFAT1216RootDir(data: TStream): Boolean;
    function StoreFAT1216RootDir(data: TStream): Boolean;
    function ReadWriteFAT1216RootDir(readNotWrite: Boolean; data: TStream): Boolean;

    // This function will delete DOSFilename from dirData
    function DeleteEntryFromDir(DOSFilename: String; dirData: TSDUMemoryStream): Boolean;
    // This function will add a directory entry for newItem to dirData
    function AddEntryToDir(itemToAdd: TSDDirItem_FAT; var dirChain: TSDFATClusterChain;
      dirData: TSDUMemoryStream; var clusterReserved: DWORD;
      flagDirDataIsRootDirData: Boolean): Boolean;

    function MaxClusterID(): DWORD;
    function ClusterSize(): Int64;
    function SectorsInDataArea(): DWORD;

    procedure AssertSufficientData(data: TStream; maxSize: Int64 = -1);

    function GetCaseSensitive(): Boolean; OVERRIDE;

    function DoMount(): Boolean; OVERRIDE;
    procedure DoDismount(); OVERRIDE;

    procedure FreeCachedFAT();

    // Boot sector information
    function ReadBootSector(): Boolean;
    function WriteBootSector(newBootSector: TSDBootSector_FAT): Boolean;

    function DetermineFATType(stmBootSector: TSDUMemoryStream): TFATType;

    function GetRootDirItem(item: TSDDirItem_FAT): Boolean;

    function GetFATEntry(clusterID: DWORD): DWORD;
    // !! NOTICE !!
    // THIS ONLY OPERATES ON IN-MEMORY FAT COPY - USE WriteFAT(...) TO WRITE
    // ANY CHANGES OUT TO THE UNDERLYING PARTITION
    function SetFATEntry(clusterID: DWORD; Value: DWORD): Boolean;
    // Get the next empty FAT entry (cluster ID).
    // Note: This *doesn't* write to the FAT, it only determines the next free
    //       (unused) cluster
    // afterClusterID - If nonzero, get the next free cluster ID after the
    //                  specified cluster ID
    // Returns ERROR_DWORD on failure
    function GetNextEmptyFATEntry(afterClusterID: DWORD = 0): DWORD;
    // Mark the next empty FAT entry as reservced, returning it's cluster ID
    // Returns ERROR_DWORD on failure
    function ReserveFATEntry(afterClusterID: DWORD = 0): DWORD;
    // Undo a call to ReserveFATEntry(...)
    procedure UnreserveFATEntry(clusterID: DWORD);

    // Count the number of empty FAT entries (i.e. free clusters)
    function CountEmptyFATEntries(): DWORD;
    function WriteDirEntry(item: TSDDirItem_FAT; stream: TSDUMemoryStream): Integer;
    procedure WriteDirEntry_83(item: TSDDirItem_FAT; stream: TSDUMemoryStream);
    function SeekBlockUnusedDirEntries(cntNeeded: Integer; dirData: TSDUMemoryStream): Boolean;
    function Seek83FileDirNameInDirData(filename: Ansistring; dirData: TSDUMemoryStream): Boolean;

    // Returns TRUE/FALSE, depending on whether clusterID appers in chain or not
    function IsClusterInChain(clusterID: DWORD; chain: TSDFATClusterChain): Boolean;
    // Add the specified cluster to the given chain
    procedure AddClusterToChain(clusterID: DWORD; var chain: TSDFATClusterChain);

    // Extend the specified stream by a cluster filled with zeros
    procedure ExtendByEmptyCluster(stream: TSDUMemoryStream);

    function _TraverseClusterChain(clusterID: DWORD; var chain: TSDFATClusterChain): DWORD;

    // Note: TTimeStamp includes a datestamp
    function WORDToTTimeStamp(dateBitmask: Word; timeBitmask: Word; msec: Byte): TTimeStamp;
    function WORDToTDate(dateBitmask: Word): TDate;
    procedure TTimeStampToWORD(timeStamp: TTimeStamp; var dateBitmask: Word;
      var timeBitmask: Word; var msec: Byte);
    function TDateToWORD(date: TDate): Word;

    function DOSFilenameTo11Chars(DOSFilename: Ansistring): Ansistring;
    function DOSFilenameCheckSum(DOSFilename: Ansistring): Byte;

    function _LoadContentsFromDisk(dirStartCluster: DWORD; items: TSDDirItemList): Boolean;

    // -- READ/WRITE CLUSTER RELATED --
    // Set maxSize to -1 to write all remaining data
    function ReadWriteClusterData(readNotWrite: Boolean; clusterID: DWORD;
      data: TStream; maxSize: Integer): Boolean;
    // Set maxSize to -1 to write all remaining data
    function ReadWriteClusterChainData(readNotWrite: Boolean; chain: TSDFATClusterChain;
      data: TStream; maxSize: Int64 = -1): Boolean;


    // -- EXTRACT CLUSTER RELATED --
    // Extract the cluster chain, starting from the specified cluster ID
    // clusterID - The cluster ID of the starting cluster. If set to 0, this
    //             will use the cluster ID of the root directory instead
    function ExtractClusterChain(clusterID: DWORD): TSDFATClusterChain;
    // Extract the data associated with the ***SINGLE*** cluster specified
    // maxSize - Extract up to this number of bytes; specify -1 to extract all
    //           data for the cluster
    function ExtractClusterData(clusterID: DWORD; data: TStream; maxSize: Integer = -1): Boolean;
    // Extract the data associated with the cluster chain starting from the
    // specified cluster ID
    // maxSize - Extract up to this number of bytes; specify -1 to extract all
    //           data for the cluster chain
    function ExtractClusterChainData(clusterID: DWORD; data: TStream;
      maxSize: Int64 = -1): Boolean; OVERLOAD;
    function ExtractClusterChainData(clusterID: DWORD; filename: String;
      maxSize: Int64 = -1): Boolean; OVERLOAD;
    function ExtractClusterChainData(chain: TSDFATClusterChain; data: TStream;
      maxSize: Int64 = -1): Boolean; OVERLOAD;


    // -- FREE CLUSTER RELATED --
    //
    // !! NOTICE !!
    // THESE ONLY UPDATE THE IN-MEMORY FAT COPY - USE WriteFAT(...) TO WRITE
    // THE CHANGES OUT TO THE UNDERLYING PARTITION
    //
    // Free up the specified cluster
    // !! WARNING !!
    // If this functions return FALSE, the FAT may be in an inconsistent state!
    function FreeCluster(clusterID: DWORD): Boolean;
    // Free up a cluster chain, starting from the specified cluster ID
    // !! WARNING !!
    // If this function returns FALSE, the FAT may be in an inconsistent state!
    function FreeClusterChain(clusterID: DWORD): Boolean; OVERLOAD;
    // Free up all clusters in the specifid cluster chain
    // !! WARNING !!
    // If this function returns FALSE, the FAT may be in an inconsistent state!
    function FreeClusterChain(clusterChain: TSDFATClusterChain): Boolean; OVERLOAD;


    // -- STORE CLUSTER RELATED --
    // Store a cluster chain
    function StoreClusterChain(chain: TSDFATClusterChain): Boolean;
    // Store the data specified to the specified cluster
    // maxSize - Store up to this number of bytes; specify -1 to store all
    //           data for the cluster
    function StoreClusterData(clusterID: DWORD; data: TStream; maxSize: Integer = -1): Boolean;
    // Store the data specified in the given cluster chain
    // maxSize - Write up to this number of bytes; specify -1 to store all
    //           the remaining data in "data", or as much as "chain" can hold
    // Note: This *ONLY* updates the *data* stored in the chain, it DOESN'T WRITE THE CHAIN TO THE FAT
    function StoreClusterChainData(chain: TSDFATClusterChain; data: TStream;
      maxSize: Int64 = -1): Boolean;
    // Determine the number of clusters needed to store the specified data
    function DetermineClustersNeeded(data: TStream; maxSize: Int64 = -1): DWORD;
    // Note: This doesn't update the FAT, it just gets a chain
    function AllocateChainForData(var chain: TSDFATClusterChain;
      var unusedChain: TSDFATClusterChain; data: TStream; maxSize: Int64 = -1): Boolean;
    //    function StoreClusterChainData(clusterID: DWORD; filename: string; maxSize: int64 = -1): boolean; overload;

    // Copy all data from one cluster to another
    function CopyClusterChainData(srcChain: TSDFATClusterChain;
      destChain: TSDFATClusterChain): Boolean;

    function ParseDirectory(data: TSDUMemoryStream; var dirContent: TSDDirItemList): Boolean;

    // Get item's content (e.g. unparsed directory content, file content to
    // rounded up cluster)
    function GetItemContent(path: WideString; content: TStream): Boolean;
    // As GetItemContent, but only extracts to the file's size
    function GetFileContent(path: WideString; fileContent: TStream): Boolean;
    function GetStartingClusterForItem(path: WideString): DWORD;
    function SectorIDForCluster(clusterID: DWORD): DWORD;

    // FAT numbers must be >= 1
    function ReadFAT(fatNo: DWORD): Boolean; OVERLOAD;
    function ReadFAT(fatNo: DWORD; stmFAT: TSDUMemoryStream): Boolean; OVERLOAD;
    function WriteFAT(fatNo: DWORD): Boolean; OVERLOAD;
    function WriteFAT(fatNo: DWORD; stmFAT: TSDUMemoryStream): Boolean; OVERLOAD;
    function WriteFATToAllCopies(): Boolean;

    function GetFreeSpace(): ULONGLONG; OVERRIDE;
    function GetSize(): ULONGLONG; OVERRIDE;

    function PathParent(path: WideString): WideString;

    function DeleteItem(fullPathToItem: WideString): Boolean;


    // Returns a new, unique, 8.3 DOS filename (without the path) that can be
    // used as for the DOS filename for the LFN supplied
    // Note: lfnFilename must include the path *and* filename; the path will
    //       be checked during the generation process to ensure the 8.3
    //      filename generated doesn't already exist
    function GenerateNew83Filename(lfnFilename: WideString): String;

    // Filesystem checking...
    function CheckFilesystem_ConsistentFATs(): Boolean;
    function CheckFilesystem_Crosslinks(): Boolean;

  PUBLIC
    // If set, preserve time/datestamps when storing/extracting files
    property PreserveTimeDateStamps: Boolean Read FPreserveTimeDateStamps
      Write FPreserveTimeDateStamps;

    // Boot sector information
    property OEMName: Ansistring Read FBootSectorSummary.OEMName;
    property BytesPerSector: Word Read FBootSectorSummary.BytesPerSector;
    property SectorsPerCluster: Byte Read FBootSectorSummary.SectorsPerCluster;
    property ReservedSectorCount: Word Read FBootSectorSummary.ReservedSectorCount;
    property FATCount: Byte Read FBootSectorSummary.FATCount;
    property MaxRootEntries: Word Read FBootSectorSummary.MaxRootEntries;
    property TotalSectors: DWORD Read FBootSectorSummary.TotalSectors;
    property MediaDescriptor: Byte Read FBootSectorSummary.MediaDescriptor;
    property SectorsPerFAT: DWORD Read FBootSectorSummary.SectorsPerFAT;
    property SectorsPerTrack: Word Read FBootSectorSummary.SectorsPerTrack;
    property NumberOfHeads: Word Read FBootSectorSummary.NumberOfHeads;
    property HiddenSectors: DWORD Read FBootSectorSummary.HiddenSectors;

    property FATType: TFATType Read FBootSectorSummary.FATType;
    property RootDirFirstCluster: DWORD Read FBootSectorSummary.RootDirFirstCluster;

    constructor Create(); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    function FilesystemTitle(): String; OVERRIDE;

    function Format(): Boolean; OVERRIDE;
    function _Format(fmtType: TFATType): Boolean;

    function CheckFilesystem(): Boolean; OVERRIDE;

    function LoadContentsFromDisk(path: String; items: TSDDirItemList): Boolean;
      OVERLOAD; OVERRIDE;
    function ExtractFile(srcPath: WideString; extractToFilename: String): Boolean; OVERRIDE;

    // If parentDir is not nil, it will have the dirToStoreIn's details
    // *assigned* to it
    function StoreFileOrDir(dirToStoreIn: WideString;
    // The dir in which item/data is to be stored
      item: TSDDirItem_FAT; data: TStream; parentDir: TSDDirItem_FAT = nil): Boolean;

    function MoveFileOrDir(srcItemPath: WideString;
                                // The path and filename of the file/dir to be moved
      destItemPath: WideString  // The new path and filename
      ): Boolean;

    function CopyFile(srcItemPath: WideString;
                                // The path and filename of the file to be copied
      destItemPath: WideString  // The path and filename of the copy
      ): Boolean;

    function CreateDir(dirToStoreIn: WideString;
    // The dir in which item/data is to be stored
      newDirname: WideString; templateDirAttrs: TSDDirItem_FAT = nil): Boolean;

    function DeleteFile(fullPathToItem: WideString): Boolean;
    function DeleteDir(fullPathToItem: WideString): Boolean;
    function DeleteFileOrDir(fullPathToItem: WideString): Boolean;

    function GetItem(path: WideString; item: TSDDirItem): Boolean; OVERRIDE;
    function GetItem_FAT(path: WideString; item: TSDDirItem_FAT): Boolean;

    function IsValidFilename(filename: String): Boolean;
  end;

function FATTypeTitle(fatType: TFATType): String;


implementation

uses
  DateUtils, Math,
  SDUi18n,
  SDUSysUtils;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  ERROR_DWORD = $FFFFFFFF;

  DEFAULT_FAT = 1;

  CLUSTER_ZERO               = 0;
  CLUSTER_FIRST_DATA_CLUSTER = 2;  // First usable cluster

  FAT_BOOTSECTORSIG = $AA55; // Note: In little-endian format ($55AA in big-endian)

  BOOTSECTOR_OFFSET_JMP                 = $00;
  BOOTSECTOR_LENGTH_JMP                 = 3;
  BOOTSECTOR_OFFSET_OEMNAME             = $03;
  BOOTSECTOR_LENGTH_OEMNAME             = 8;
  BOOTSECTOR_OFFSET_BYTESPERSECTOR      = $0b;
  BOOTSECTOR_OFFSET_SECTORSPERCLUSTER   = $0d;
  BOOTSECTOR_OFFSET_RESERVEDSECTORCOUNT = $0e;
  BOOTSECTOR_OFFSET_FATCOUNT            = $10;
  BOOTSECTOR_OFFSET_MAXROOTENTRIES      = $11;
  BOOTSECTOR_OFFSET_TOTALSECTORS_SMALL  = $13;
  BOOTSECTOR_OFFSET_TOTALSECTORS_LARGE  = $20;
  BOOTSECTOR_OFFSET_MEDIADESCRIPTOR     = $15;
  BOOTSECTOR_OFFSET_SECTORSPERFAT       = $16;
  BOOTSECTOR_OFFSET_SECTORSPERTRACK     = $18;
  BOOTSECTOR_OFFSET_NUMBEROFHEADS       = $1a;
  BOOTSECTOR_OFFSET_HIDDENSECTORS       = $1c;
  BOOTSECTOR_OFFSET_BOOTSECTORSIG       = $1fe;

  // Extended BIOS parameter block: FAT12/FAT16
  BOOTSECTOR_OFFSET_FAT1216_PHYSICALDRIVENO = $24;
  BOOTSECTOR_OFFSET_FAT1216_RESERVED        = $25;
  BOOTSECTOR_OFFSET_FAT1216_EXTENDEDBOOTSIG = $26;
  BOOTSECTOR_OFFSET_FAT1216_SERIALNUMBER    = $27;
  BOOTSECTOR_OFFSET_FAT1216_VOLUMELABEL     = $2b;
  BOOTSECTOR_LENGTH_FAT1216_VOLUMELABEL     = 11;
  BOOTSECTOR_OFFSET_FAT1216_FATFSTYPE       = $36;
  BOOTSECTOR_LENGTH_FAT1216_FATFSTYPE       = 8;

  // Extended BIOS parameter block: FAT32
  BOOTSECTOR_OFFSET_FAT32_SECTORSPERFAT          = $24;
  BOOTSECTOR_OFFSET_FAT32_FATFLAGS               = $28;
  BOOTSECTOR_OFFSET_FAT32_VERSION                = $2a;
  BOOTSECTOR_OFFSET_FAT32_CLUSTERNOROOTDIRSTART  = $2c;
  BOOTSECTOR_OFFSET_FAT32_SECTORNOFSINFOSECTOR   = $30;
  BOOTSECTOR_OFFSET_FAT32_SECTORNOBOOTSECTORCOPY = $32;
  BOOTSECTOR_OFFSET_FAT32_RESERVED_1             = $34;
  BOOTSECTOR_OFFSET_FAT32_PHYSICALDRIVENO        = $40;
  BOOTSECTOR_OFFSET_FAT32_RESERVED_2             = $41;
  BOOTSECTOR_OFFSET_FAT32_EXTENDEDBOOTSIG        = $42;
  BOOTSECTOR_OFFSET_FAT32_SERIALNUMBER           = $43;
  BOOTSECTOR_OFFSET_FAT32_VOLUMELABEL            = $47;
  BOOTSECTOR_LENGTH_FAT32_VOLUMELABEL            = 11;
  BOOTSECTOR_OFFSET_FAT32_FATFSTYPE              = $52;
  BOOTSECTOR_LENGTH_FAT32_FATFSTYPE              = 8;
  BOOTSECTOR_OFFSET_FAT32_OSBOOTCODE             = $5a;

  SIGNATURE_FAT12 = 'FAT12   ';
  SIGNATURE_FAT16 = 'FAT16   ';
  SIGNATURE_FAT32 = 'FAT32   ';

  // Root dir first cluster always cluster 2 for FAT12/FAT16
  // Zero here in order to be consistent with FAT entries relating to root dir
  FAT1216_ROOT_DIR_FIRST_CLUSTER = 0;

                                // FAT12 FAT entries...
  FAT12_ENTRY_SIZE       = 1.5; // Bytes per FAT entry
  FAT12_ENTRY_MASK       = $FFF;
  FAT12_ENTRY_FREE       = $000;
  FAT12_ENTRY_USED_START = $002;
  FAT12_ENTRY_USED_END   = $FEF;
  FAT12_ENTRY_BAD_SECTOR = $FF7;
  FAT12_ENTRY_EOC_START  = $FF8;
  FAT12_ENTRY_EOC_END    = $FFF;
                              // FAT16 FAT entries...
  FAT16_ENTRY_SIZE       = 2; // Bytes per FAT entry
  FAT16_ENTRY_MASK       = $FFFF;
  FAT16_ENTRY_FREE       = $0000;
  FAT16_ENTRY_USED_START = $0002;
  FAT16_ENTRY_USED_END   = $FFEF;
  FAT16_ENTRY_BAD_SECTOR = $FFF7;
  FAT16_ENTRY_EOC_START  = $FFF8;
  FAT16_ENTRY_EOC_END    = $FFFF;
                              // Note: Only use the 7 byte LSB for FAT32; the highest *nibble* is RESERVED.
  FAT32_ENTRY_SIZE       = 4; // Bytes per FAT entry
  FAT32_ENTRY_MASK       = $0FFFFFFF;
  FAT32_ENTRY_FREE       = $0000000;
  FAT32_ENTRY_USED_START = $0000002;
  FAT32_ENTRY_USED_END   = $FFFFFEF;
  FAT32_ENTRY_BAD_SECTOR = $FFFFFF7;
  FAT32_ENTRY_EOC_START  = $FFFFFF8;
  FAT32_ENTRY_EOC_END    = $FFFFFFF;


  // Normal DOS 8.3 directory entry:
  //
  //   00000000000000001111111111111111
  //   0123456789ABCDEF0123456789ABCDEF
  //   ^^^^^^^^                          Filename (8 chars; 8 bytes - padded with spaces)
  //                                     Special first characters:
  //                                       0x00  Entry unused; all subsequent entries are also unused
  //                                       0x05  Initial character is actually 0xE5
  //                                       0xE5  Entry deleted
  //           ^^^                       Extension (3 chars; 3 bytes - padded with spaces)
  //              ^                      Attributes (as VFAT_ATTRIB_FLAG_... consts)
  //               ^                     Case information
  //                ^                    Create time (fine; 0-199; 10ms units)
  //                 ^^                  Create time (bit encoded to 2 second accuracy)
  //                   ^^                Create date (bit encoded)
  //                     ^^              Last access (bit encoded)
  //                       ^^            EA-Index; highest 2 bytes of first cluster number in FAT32
  //                         ^^          Last modified time (bit encoded to 2 second accuracy)
  //                          ^^         Last modified date (bit encoded)
  //                             ^^      First cluster; lowest 2 bytes of first cluster number in FAT32. Set to 0 for root directory, empty files and volume labels
  //                               ^^^^  File size. Set to 0 for volume labels and directories
  //
  DIR_ENTRY_SIZE: Int64           = $20;
  DIR_ENTRY_OFFSET_DOSFILENAME    = $00;
  DIR_ENTRY_LENGTH_DOSFILENAME    = 8;
  DIR_ENTRY_OFFSET_DOSEXTENSION   = $08;
  DIR_ENTRY_OFFSET_FILEATTRS      = $0b;
  DIR_ENTRY_OFFSET_RESERVED       = $0c;  // Actually used to store case information
  DIR_ENTRY_OFFSET_CREATETIMEFINE = $0d;
  DIR_ENTRY_OFFSET_CREATETIME     = $0e;
  DIR_ENTRY_OFFSET_CREATEDATE     = $10;
  DIR_ENTRY_OFFSET_LASTACCESSDATE = $12;
  DIR_ENTRY_OFFSET_EAINDEX        = $14;
  DIR_ENTRY_OFFSET_LASTMODTIME    = $16;
  DIR_ENTRY_OFFSET_LASTMODDATE    = $18;
  DIR_ENTRY_OFFSET_FIRSTCLUSTERLO = $1a;
  DIR_ENTRY_OFFSET_FILESIZE       = $1c;

  DIR_ENTRY_UNUSED  = $00;
  DIR_ENTRY_DELETED = $E5;

  DIR_ENTRY_83CASE_BASENAME = $08;
  DIR_ENTRY_83CASE_EXTN     = $10;

  // VFAT entries..
  // LFN directory entry entry:
  //
  //   00000000000000001111111111111111
  //   0123456789ABCDEF0123456789ABCDEF
  //   ^                                 Sequence number
  //    ^^^^^^^^^^                       Name (5 chars; 10 bytes)
  //              ^                      0x0F (Attributes)
  //               ^                     0x00 (Reserved)
  //                ^                    Checksum
  //                 ^^^^^^^^^^^^        Name (6 chars; 12 bytes)
  //                             ^^      0x0000 (First cluster)
  //                               ^^^^  Name (2 chars; 4 bytes)
  DIR_ENTRY_OFFSET_VFAT_SEQ_NO       = $00;
  DIR_ENTRY_OFFSET_VFAT_NAME_PART_1  = $01;
  DIR_ENTRY_OFFSET_VFAT_ATTRIBUTES   = $0b;
  DIR_ENTRY_OFFSET_VFAT_RESERVED     = $0c;
  DIR_ENTRY_OFFSET_VFAT_CHECKSUM     = $0d;
  DIR_ENTRY_OFFSET_VFAT_NAME_PART_2  = $0e;
  DIR_ENTRY_OFFSET_VFAT_FIRSTCLUSTER = $1a;
  DIR_ENTRY_OFFSET_VFAT_NAME_PART_3  = $1c;

  DIR_ENTRY_VFAT_LAST_LONG_ENTRY = $40;


type
  TClusterSizeBreakdown = record
    MaxPartitionSize:       ULONGLONG;
    ClusterSize_FAT12FAT16: DWORD;
    ClusterSize_FAT32:      DWORD;
  end;

const
  // From:
  //   http://support.microsoft.com/kb/140365/EN-US/
  // The following table describes the default FAT cluster sizes for Windows
  // Server 2003 file system volumes:
  //
  // Volume size     |         Cluster size
  //                 | FAT16         | FAT32
  // ----------------+---------------+---------------
  // 7 MB–16 MB      | 2 KB          | Not supported
  // 17 MB–32 MB     | 512 bytes     | Not supported
  // 33 MB–64 MB     | 1 KB          | 512 bytes
  // 65 MB–128 MB    | 2 KB          | 1 KB
  // 129 MB–256 MB   | 4 KB          | 2 KB
  // 257 MB–512 MB   | 8 KB          | 4 KB
  // 513 MB–1,024 MB | 16 KB         | 4 KB
  // 1,025 MB–2 GB   | 32 KB         | 4 KB
  // 2 GB–4 GB       | 64 KB         | 4 KB
  // 4 GB–8 GB       | Not supported | 4 KB
  // 8 GB–16 GB      | Not supported | 8 KB
  // 16 GB–32 GB     | Not supported | 16 KB
  // 32 GB–2 TB      | Not supported | Not supported [*]
  //
  // [*] - For this one, we use 16 KB clusters
  //
  // A later version of this is at:
  //   http://support.microsoft.com/kb/314878
  // which gives details for FAT16, but not FAT32 - so we use the earlier Windows
  // Server 2003 system
  CLUSTERSIZE_BREAKDOWN: array [1..13] of TClusterSizeBreakdown =
    (
    (
    MaxPartitionSize: (16 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: (2 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: 0  // Not supported
    ),
    (
    MaxPartitionSize: (32 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: 512;
    ClusterSize_FAT32: 0  // Not supported
    ),
    (
    MaxPartitionSize: (64 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: (1 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: 512
    ),
    (
    MaxPartitionSize: (128 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: (2 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (1 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (256 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: (4 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (2 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (512 * BYTES_IN_MEGABYTE);
    ClusterSize_FAT12FAT16: (8 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (4 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (1 * BYTES_IN_GIGABYTE);
    ClusterSize_FAT12FAT16: (16 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (4 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (2 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: (32 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (4 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (4 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: (64 * BYTES_IN_KILOBYTE);
    ClusterSize_FAT32: (4 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (8 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: 0;  // Not supported
    ClusterSize_FAT32: (4 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (16 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: 0;  // Not supported
    ClusterSize_FAT32: (8 * BYTES_IN_KILOBYTE)
    ),
    (
    MaxPartitionSize: (32 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: 0;  // Not supported
    ClusterSize_FAT32: (16 * BYTES_IN_KILOBYTE)
    ),
    (
    // Not MS "standard", but needed to support filesystems over 32GB
    // If you need more than this, you probably shouldn't be using FAT!
    MaxPartitionSize: (99999 * ULONGLONG(BYTES_IN_GIGABYTE));
    ClusterSize_FAT12FAT16: 0;  // Not supported
    ClusterSize_FAT32: (16 * BYTES_IN_KILOBYTE)
    )
    );


function FATTypeTitle(fatType: TFATType): String;
begin
  Result := LoadResString(FATTypeTitlePtr[fatType]);
end;


procedure TSDDirItem_FAT.Assign(srcItem: TSDDirItem_FAT);
begin
  inherited Assign(srcItem);

  self.FilenameDOS         := srcItem.FilenameDOS;
  self.Attributes          := srcItem.Attributes;
  self.TimestampCreation   := srcItem.TimestampCreation;
  self.DatestampLastAccess := srcItem.DatestampLastAccess;
  self.FirstCluster        := srcItem.FirstCluster;
end;

function TSDDirItem_FAT.CheckAttr(attr: Byte): Boolean;
begin
  Result := ((FAttributes and attr) = attr);

end;

procedure TSDDirItem_FAT.SetAttr(attr: Byte; Value: Boolean);
begin
  if Value then begin
    FAttributes := (FAttributes or attr);
  end else begin
    FAttributes := (FAttributes and not (attr));
  end;

end;

function TSDDirItem_FAT.GetIsFile(): Boolean;
begin
  Result := not (((Attributes and VFAT_ATTRIB_FLAG_SUBDIR) = VFAT_ATTRIB_FLAG_SUBDIR) or
    ((Attributes and VFAT_ATTRIB_FLAG_VOLLABEL) = VFAT_ATTRIB_FLAG_VOLLABEL) or
    ((Attributes and VFAT_ATTRIB_FLAG_DEVICE) = VFAT_ATTRIB_FLAG_DEVICE));
end;

procedure TSDDirItem_FAT.SetIsFile(Value: Boolean);
begin
  if Value then begin
    Attributes := (Attributes and not (VFAT_ATTRIB_FLAG_SUBDIR or
      VFAT_ATTRIB_FLAG_VOLLABEL or VFAT_ATTRIB_FLAG_DEVICE));
  end else begin
    IsDirectory := True;
  end;
end;

function TSDDirItem_FAT.GetIsDirectory(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_SUBDIR);
end;

procedure TSDDirItem_FAT.SetIsDirectory(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_SUBDIR, Value);
end;

function TSDDirItem_FAT.GetIsReadonly(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_READONLY);
end;

procedure TSDDirItem_FAT.SetIsReadonly(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_READONLY, Value);
end;

function TSDDirItem_FAT.GetIsArchive(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_ARCHIVE);
end;

procedure TSDDirItem_FAT.SetIsArchive(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_ARCHIVE, Value);
end;

function TSDDirItem_FAT.GetIsHidden(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_HIDDEN);
end;

procedure TSDDirItem_FAT.SetIsHidden(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_HIDDEN, Value);
end;

function TSDDirItem_FAT.GetIsSystem(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_SYSTEM);
end;

procedure TSDDirItem_FAT.SetIsSystem(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_SYSTEM, Value);
end;

function TSDDirItem_FAT.GetIsVolumeLabel(): Boolean;
begin
  Result := CheckAttr(VFAT_ATTRIB_FLAG_VOLLABEL);
end;

function TSDDirItem_FAT.GetAttributes(): Byte;
begin
  Result := FAttributes;
end;

procedure TSDDirItem_FAT.SetAttributes(Value: Byte);
begin
  FAttributes := Value;
end;

procedure TSDDirItem_FAT.SetIsVolumeLabel(Value: Boolean);
begin
  SetAttr(VFAT_ATTRIB_FLAG_VOLLABEL, Value);
end;

// ----------------------------------------------------------------------------

constructor TSDFilesystem_FAT.Create();
begin
  inherited;
  FPreserveTimeDateStamps := True;
end;

destructor TSDFilesystem_FAT.Destroy();
begin
  inherited;
end;

function TSDFilesystem_FAT.GetCaseSensitive(): Boolean;
begin
  Result := False;
end;

function TSDFilesystem_FAT.DoMount(): Boolean;
begin
  Result := False;

  // Assume mounting is successful...
  FMounted := True;
  try
    if not (ReadBootSector()) then begin
      raise EFileSystemNotRecognised.Create('Not a FAT12/FAT16/FAT32 filesystem');
    end;

    SetupFFATEntryValues(FATType);

    Result := ReadFAT(DEFAULT_FAT);
  finally
    FMounted := False;
  end;


end;

procedure TSDFilesystem_FAT.DoDismount();
begin
  FreeCachedFAT();

  inherited;
end;

procedure TSDFilesystem_FAT.FreeCachedFAT();
begin
  if (FFAT <> nil) then begin
    FFAT.Free();
  end;

end;

function TSDFilesystem_FAT.ReadBootSector(): Boolean;
var
  stmBootSector: TSDUMemoryStream;
  i:             Integer;
begin
  AssertMounted();

  Result := True;

  FSerializeCS.Acquire();
  try
    stmBootSector := TSDUMemoryStream.Create();
    try
      try
        PartitionImage.ReadSector(0, stmBootSector);
      except
        // Problem...
        on E: Exception do begin
          Result := False;
        end;
      end;

      if Result then begin
        // Sanity check - can we find the Boot sector signature (0x55 0xAA)?
        if (stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_BOOTSECTORSIG) <> FAT_BOOTSECTORSIG) then
        begin
          Result := False;
        end;
      end;

      if Result then begin
        // Attempt to identify filesystem type; FAT12/FAT16/FAT32...
        // Default to FAT16
        FBootSectorSummary.FATType := DetermineFATType(stmBootSector);

        // ------------
        // Parse the FAT12/FAT16/FAT32 common boot sector...

        stmBootSector.Position := BOOTSECTOR_OFFSET_JMP;
        for i := low(FBootSectorSummary.JMP) to high(FBootSectorSummary.JMP) do begin
          FBootSectorSummary.JMP[i] := stmBootSector.ReadByte();
        end;

        FBootSectorSummary.OEMName             :=
          stmBootSector.ReadString(BOOTSECTOR_LENGTH_OEMNAME, BOOTSECTOR_OFFSET_OEMNAME);
        FBootSectorSummary.BytesPerSector      :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_BYTESPERSECTOR);
        FBootSectorSummary.SectorsPerCluster   :=
          stmBootSector.ReadByte(BOOTSECTOR_OFFSET_SECTORSPERCLUSTER);
        FBootSectorSummary.ReservedSectorCount :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_RESERVEDSECTORCOUNT);
        FBootSectorSummary.FATCount            :=
          stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FATCOUNT);
        FBootSectorSummary.MaxRootEntries      :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_MAXROOTENTRIES);

        FBootSectorSummary.TotalSectors :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_TOTALSECTORS_SMALL);
        if (TotalSectors = 0) then begin
          FBootSectorSummary.TotalSectors :=
            stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_TOTALSECTORS_LARGE);
        end;

        FBootSectorSummary.MediaDescriptor :=
          stmBootSector.ReadByte(BOOTSECTOR_OFFSET_MEDIADESCRIPTOR);

        // Note: This will be overwritten with value in the FAT32 extended BIOS parameter block if FAT32...
        FBootSectorSummary.SectorsPerFAT :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_SECTORSPERFAT);

        FBootSectorSummary.SectorsPerTrack :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_SECTORSPERTRACK);
        FBootSectorSummary.NumberOfHeads   :=
          stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_NUMBEROFHEADS);
        FBootSectorSummary.HiddenSectors   :=
          stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_HIDDENSECTORS);


        // ------------
        // Parse the FAT12/FAT16 extended BIOS parameter block, if FAT12/FAT16...
        if ((FATType = ftFAT12) or (FATType = ftFAT16)) then begin
          FBootSectorSummary.PhysicalDriveNo :=
            stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FAT1216_PHYSICALDRIVENO);
          FBootSectorSummary.ExtendedBootSig :=
            stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FAT1216_EXTENDEDBOOTSIG);
          FBootSectorSummary.SerialNumber    :=
            stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_FAT1216_SERIALNUMBER);
          FBootSectorSummary.VolumeLabel     :=
            stmBootSector.ReadString(BOOTSECTOR_LENGTH_FAT1216_VOLUMELABEL,
            BOOTSECTOR_OFFSET_FAT1216_VOLUMELABEL);
          // .FilesystemType read above
          FBootSectorSummary.BootSectorSig   :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_BOOTSECTORSIG);

          FBootSectorSummary.RootDirFirstCluster := FAT1216_ROOT_DIR_FIRST_CLUSTER;
        end;

        // ------------
        // Parse the FAT32 extended BIOS parameter block, if FAT32...
        if (FATType = ftFAT32) then begin
          FBootSectorSummary.SectorsPerFAT          :=
            stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_FAT32_SECTORSPERFAT);
          FBootSectorSummary.FATFlags               :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_FAT32_FATFLAGS);
          FBootSectorSummary.Version                :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_FAT32_VERSION);
          FBootSectorSummary.RootDirFirstCluster    :=
            stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_FAT32_CLUSTERNOROOTDIRSTART);
          FBootSectorSummary.SectorNoFSInfoSector   :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_FAT32_SECTORNOFSINFOSECTOR);
          FBootSectorSummary.SectorNoBootSectorCopy :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_FAT32_SECTORNOBOOTSECTORCOPY);
          FBootSectorSummary.PhysicalDriveNo        :=
            stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FAT32_PHYSICALDRIVENO);
          FBootSectorSummary.ExtendedBootSig        :=
            stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FAT32_EXTENDEDBOOTSIG);
          FBootSectorSummary.SerialNumber           :=
            stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_FAT32_SERIALNUMBER);
          FBootSectorSummary.VolumeLabel            :=
            stmBootSector.ReadString(BOOTSECTOR_LENGTH_FAT32_VOLUMELABEL,
            BOOTSECTOR_OFFSET_FAT32_VOLUMELABEL);
          // .FilesystemType read above
          FBootSectorSummary.BootSectorSig          :=
            stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_BOOTSECTORSIG);
        end;

      end;

    finally
      stmBootSector.Free();
    end;

  finally
    FSerializeCS.Release();
  end;

  if Result then begin
    // Sanity checks...
    if ((FBootSectorSummary.FATCount < 1) or
      // Make sure there's at least *one* FAT
      (trim(FBootSectorSummary.OEMName) = 'NTFS')  // System ID must not be NTFS
      ) then begin
      Result := False;
    end;
  end;


end;

function TSDFilesystem_FAT.WriteBootSector(newBootSector: TSDBootSector_FAT): Boolean;
var
  stmBootSector: TSDUMemoryStream;
  i:             Integer;
begin
  // Note: Filesystem *shouldn't* be mounted to do this (e.g. When formatting
  //       for the first time)
  // AssertMounted();

  Result := True;

  FSerializeCS.Acquire();
  try
    stmBootSector := TSDUMemoryStream.Create();
    try
      // Initialize with all zero bytes...
      stmBootSector.WriteByte(0, 0, newBootSector.BytesPerSector);
{
      try
        // We read in original boot sector, modify it, then write it back out
        stmBootSector.Position := 0;
        PartitionImage.ReadSector(0, stmBootSector);
      except
        // Problem...
        on E:Exception do
          begin
          Result := FALSE;
          end;
      end;
}
      if Result then begin
        // ------------
        // Write the FAT12/FAT16/FAT32 common boot sector...

{
        // We leave the JMP instruction alone; our format rountine doesn't
        // populate this information, so leave any existing as-is
        //stmBootSector.Position := BOOTSECTOR_OFFSET_JMP;
        //for i:=low(newBootSector.JMP) to high(newBootSector.JMP) do
        //  begin
        //  stmBootSector.WriteByte(newBootSector.JMP[i]);
        //  end;
}
        stmBootSector.Position := BOOTSECTOR_OFFSET_JMP;
        for i := low(newBootSector.JMP) to high(newBootSector.JMP) do begin
          stmBootSector.WriteByte(newBootSector.JMP[i]);
        end;

        // 8 char OEM name
        stmBootSector.WriteString(
          newBootSector.OEMName,
          BOOTSECTOR_LENGTH_OEMNAME,
          BOOTSECTOR_OFFSET_OEMNAME
          );

        stmBootSector.WriteWORD_LE(newBootSector.BytesPerSector, BOOTSECTOR_OFFSET_BYTESPERSECTOR);

        stmBootSector.WriteByte(newBootSector.SectorsPerCluster,
          BOOTSECTOR_OFFSET_SECTORSPERCLUSTER);
        stmBootSector.WriteWORD_LE(newBootSector.ReservedSectorCount,
          BOOTSECTOR_OFFSET_RESERVEDSECTORCOUNT);
        stmBootSector.WriteByte(newBootSector.FATCount, BOOTSECTOR_OFFSET_FATCOUNT);
        stmBootSector.WriteWORD_LE(newBootSector.MaxRootEntries, BOOTSECTOR_OFFSET_MAXROOTENTRIES);

        if (newBootSector.TotalSectors <= SDUMaxWORD) then begin
          stmBootSector.WriteWORD_LE(newBootSector.TotalSectors,
            BOOTSECTOR_OFFSET_TOTALSECTORS_SMALL);
          stmBootSector.WriteDWORD_LE(0, BOOTSECTOR_OFFSET_TOTALSECTORS_LARGE);
        end else begin
          stmBootSector.WriteWORD_LE(0, BOOTSECTOR_OFFSET_TOTALSECTORS_SMALL);
          stmBootSector.WriteDWORD_LE(newBootSector.TotalSectors,
            BOOTSECTOR_OFFSET_TOTALSECTORS_LARGE);
        end;

        stmBootSector.WriteByte(newBootSector.MediaDescriptor, BOOTSECTOR_OFFSET_MEDIADESCRIPTOR);

        stmBootSector.WriteWORD_LE(newBootSector.SectorsPerTrack,
          BOOTSECTOR_OFFSET_SECTORSPERTRACK);
        stmBootSector.WriteWORD_LE(newBootSector.NumberOfHeads, BOOTSECTOR_OFFSET_NUMBEROFHEADS);
        stmBootSector.WriteDWORD_LE(newBootSector.HiddenSectors, BOOTSECTOR_OFFSET_HIDDENSECTORS);


        // ------------
        // Parse the FAT12/FAT16 extended BIOS parameter block, if FAT12/FAT16...
        if ((newBootSector.FATType = ftFAT12) or (newBootSector.FATType = ftFAT16)) then begin
          // Note: This will be overwritten with value in the FAT32 extended BIOS parameter block if FAT32...
          stmBootSector.WriteWORD_LE(newBootSector.SectorsPerFAT, BOOTSECTOR_OFFSET_SECTORSPERFAT);

          stmBootSector.WriteByte(newBootSector.PhysicalDriveNo,
            BOOTSECTOR_OFFSET_FAT1216_PHYSICALDRIVENO);
          stmBootSector.WriteByte(newBootSector.ExtendedBootSig,
            BOOTSECTOR_OFFSET_FAT1216_EXTENDEDBOOTSIG);
          stmBootSector.WriteDWORD_LE(newBootSector.SerialNumber,
            BOOTSECTOR_OFFSET_FAT1216_SERIALNUMBER);
          stmBootSector.WriteString(newBootSector.VolumeLabel,
            BOOTSECTOR_LENGTH_FAT1216_VOLUMELABEL, BOOTSECTOR_OFFSET_FAT1216_VOLUMELABEL);
          stmBootSector.WriteString(newBootSector.FATFilesystemType,
            BOOTSECTOR_LENGTH_FAT1216_FATFSTYPE, BOOTSECTOR_OFFSET_FAT1216_FATFSTYPE);
        end;

        // ------------
        // Parse the FAT32 extended BIOS parameter block, if FAT32...
        if (newBootSector.FATType = ftFAT32) then begin
          // SectorsPerFAT stored in different location for FAT32
          stmBootSector.WriteWORD_LE(0, BOOTSECTOR_OFFSET_SECTORSPERFAT);
          stmBootSector.WriteDWORD_LE(newBootSector.SectorsPerFAT,
            BOOTSECTOR_OFFSET_FAT32_SECTORSPERFAT);

          stmBootSector.WriteWORD_LE(newBootSector.FATFlags, BOOTSECTOR_OFFSET_FAT32_FATFLAGS);
          stmBootSector.WriteWORD_LE(newBootSector.Version, BOOTSECTOR_OFFSET_FAT32_VERSION);
          stmBootSector.WriteDWORD_LE(newBootSector.RootDirFirstCluster,
            BOOTSECTOR_OFFSET_FAT32_CLUSTERNOROOTDIRSTART);
          stmBootSector.WriteWORD_LE(newBootSector.SectorNoFSInfoSector,
            BOOTSECTOR_OFFSET_FAT32_SECTORNOFSINFOSECTOR);
          stmBootSector.WriteWORD_LE(newBootSector.SectorNoBootSectorCopy,
            BOOTSECTOR_OFFSET_FAT32_SECTORNOBOOTSECTORCOPY);
          stmBootSector.WriteByte(newBootSector.PhysicalDriveNo,
            BOOTSECTOR_OFFSET_FAT32_PHYSICALDRIVENO);
          stmBootSector.WriteByte(newBootSector.ExtendedBootSig,
            BOOTSECTOR_OFFSET_FAT32_EXTENDEDBOOTSIG);
          stmBootSector.WriteDWORD_LE(newBootSector.SerialNumber,
            BOOTSECTOR_OFFSET_FAT32_SERIALNUMBER);
          stmBootSector.WriteString(newBootSector.VolumeLabel,
            BOOTSECTOR_LENGTH_FAT32_VOLUMELABEL, BOOTSECTOR_OFFSET_FAT32_VOLUMELABEL);
          stmBootSector.WriteString(newBootSector.FATFilesystemType,
            BOOTSECTOR_LENGTH_FAT32_FATFSTYPE, BOOTSECTOR_OFFSET_FAT32_FATFSTYPE);
        end;

        stmBootSector.WriteWORD_LE(newBootSector.BootSectorSig, BOOTSECTOR_OFFSET_BOOTSECTORSIG);
      end;

      if Result then begin
        try
          stmBootSector.Position := 0;
          PartitionImage.WriteSector(0, stmBootSector);
        except
          // Problem...
          on E: Exception do begin
            Result := False;
          end;
        end;
      end;

    finally
      stmBootSector.Free();
    end;

  finally
    FSerializeCS.Release();
  end;


end;


function TSDFilesystem_FAT.ReadFAT(fatNo: DWORD): Boolean;
begin
  FreeCachedFAT();
  FFAT := TSDUMemoryStream.Create();

  Result := ReadFAT(fatNo, FFAT);

  if not (Result) then begin
    FreeCachedFAT();
  end;


end;

function TSDFilesystem_FAT.WriteFAT(fatNo: DWORD): Boolean;
begin
  if ReadOnly then begin
    Result := False;
  end else begin
    FFAT.Position := 0;
    Result        := WriteFAT(fatNo, FFAT);
  end;


end;

function TSDFilesystem_FAT.WriteFATToAllCopies(): Boolean;
var
  i:      Integer;
begin
  Result := True;

  for i := 1 to FATCount do begin
    if not (WriteFAT(i)) then begin
      Result := False;
    end;
  end;


end;

function TSDFilesystem_FAT.ReadFAT(fatNo: DWORD; stmFAT: TSDUMemoryStream): Boolean;
var
  FATStartSectorID: DWORD;
  i:                DWORD;
  allOK:            Boolean;
begin
  // Sanity checking
  Assert(
    (stmFAT <> nil),
    'No stream passed into ReadFAT'
    );
  Assert(
    ((fatNo >= 1) and (fatNo <= FATCount)),
    'FAT number must be 1 <= x <= ' + IntToStr(FATCount) + ' when reading FAT'
    );

  allOK := True;

  FATStartSectorID := ReservedSectorCount + ((fatNo - 1) * SectorsPerFAT);
  for i := 0 to (SectorsPerFAT - 1) do begin
    allOK := PartitionImage.ReadSector((FATStartSectorID + i), stmFAT);

    if not (allOK) then begin
      break;
    end;
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.WriteFAT(fatNo: DWORD; stmFAT: TSDUMemoryStream): Boolean;
var
  FATStartSectorID: DWORD;
  allOK:            Boolean;
begin
  // Sanity checking
  Assert(
    (stmFAT <> nil),
    'No stream passed into WriteFAT'
    );
  Assert(
    ((fatNo >= 1) and (fatNo <= FATCount)),
    'FAT number must be 1 <= x <= ' + IntToStr(FATCount) + ' when writing FAT'
    );

  FATStartSectorID := ReservedSectorCount + ((fatNo - 1) * SectorsPerFAT);
  allOK            := PartitionImage.WriteConsecutiveSectors(FATStartSectorID,
    stmFAT, (SectorsPerFAT * BytesPerSector));

  Result := allOK;
end;

function TSDFilesystem_FAT.ExtractClusterChain(clusterID: DWORD): TSDFATClusterChain;
var
  chainLength: DWORD;
begin
  // Determine chain length
  SetLength(Result, 0);
  chainLength := _TraverseClusterChain(clusterID, Result);

  // Get chain
  if (chainLength > 0) then begin
    SetLength(Result, chainLength);
    _TraverseClusterChain(clusterID, Result);
  end;


end;


function TSDFilesystem_FAT.StoreClusterChain(chain: TSDFATClusterChain): Boolean;
var
  allOK:       Boolean;
  i:           Integer;
  nextCluster: DWORD;
begin
  allOK := True;

  // Special case - cluster zero on a FAT12/FAT16 is the root dir.
  // In this case, just return a single "cluster" with that cluster ID
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and IsClusterInChain(CLUSTER_ZERO, chain))
  then begin
    // Do nothing - root dir doesn't have a cluster chain, it's outside the
    // data area
  end else begin
    // Replace any references of clusterID 0 to cluster ID root dir cluster ID
    for i := low(chain) to high(chain) do begin
      if (chain[i] = CLUSTER_ZERO) then begin
        chain[i] := RootDirFirstCluster;
      end;
    end;

    // Update the FAT to store the chain
    for i := low(chain) to high(chain) do begin
      if (i < high(chain)) then begin
        nextCluster := chain[i + 1];
      end else begin
        nextCluster := FFATEntryEOCEnd;
      end;

      allOK := SetFATEntry(chain[i], nextCluster);
      if not (allOK) then begin
        break;
      end;

    end;
  end;

  Result := allOK;
end;


// Copy all data from one cluster chain to another
function TSDFilesystem_FAT.CopyClusterChainData(srcChain: TSDFATClusterChain;
  destChain: TSDFATClusterChain): Boolean;
var
  i:       Integer;
  allOK:   Boolean;
  tmpData: TSDUMemoryStream;
begin
  allOK := True;

  // Sanity check both cluster chains are the same length
  if allOK then begin
    allOK := (length(srcChain) = length(destChain));
  end;

  if allOK then begin
    tmpData := TSDUMemoryStream.Create();
    try
      // For each cluster in the chain...
      for i := low(srcChain) to high(srcChain) do begin
        // Read in the src cluster...
        tmpData.Position := 0;
        allOK            := ExtractClusterData(srcChain[i], tmpData);

        if allOK then begin
          // ...and write it out to the dest cluster
          tmpData.Position := 0;
          allOK            := StoreClusterData(destChain[i], tmpData);
        end;

        if not (allOK) then begin
          break;
        end;
      end;

    finally
      tmpData.Free();
    end;

  end;

  Result := allOK;
end;


function TSDFilesystem_FAT.GetFATEntry(clusterID: DWORD): DWORD;
begin
  FSerializeCS.Acquire();
  try
    if (FATType = ftFAT12) then begin
      Result := GetFATEntry_FAT12(clusterID);
    end else begin
      Result := GetFATEntry_FAT1632(clusterID);
    end;
  finally
    FSerializeCS.Release();
  end;

end;

function TSDFilesystem_FAT.GetFATEntry_FAT12(clusterID: DWORD): DWORD;
var
  tmpDouble: Double;
begin
  tmpDouble     := clusterID * FAT12_ENTRY_SIZE;
  FFAT.Position := trunc(tmpDouble);
  Result        := FFAT.ReadWORD_LE();
  if SDUIsOddNumber(clusterID) then begin
    Result := Result shr 4;
  end else begin
    Result := (Result and FFATEntryMask);
  end;


end;

function TSDFilesystem_FAT.GetFATEntry_FAT1632(clusterID: DWORD): DWORD;
var
  multiplier: DWORD;
  i:          DWORD;
  tmpByte:    Byte;
begin
  Result := 0;

  // FAT entries are stored LSB first
  // Note: Length of FAT entries VARIES with FAT12/FAT16/FAT32!
  multiplier    := 1;
  FFAT.Position := (clusterID * FFATEntrySize);
  for i := 1 to FFATEntrySize do begin
    tmpByte    := FFAT.ReadByte;
    Result     := Result + (tmpByte * multiplier);
    multiplier := multiplier * $100;
  end;


end;

function TSDFilesystem_FAT.MaxClusterID(): DWORD;
var
  FATSizeInBytes:   DWORD;
  maxClustersInFAT: DWORD;
begin
  // Implemented as failsafe - not sure if FAT size or physical partition 
  // size should drive this?!

  // Calculated max number of entries in the FAT
  FATSizeInBytes := SectorsPerFAT * BytesPerSector;

  maxClustersInFAT := 0;
  case FATType of
    ftFAT12:
    begin
      maxClustersInFAT := trunc(FATSizeInBytes / FAT12_ENTRY_SIZE);
    end;

    ftFAT16:
    begin
      maxClustersInFAT := (FATSizeInBytes div FAT16_ENTRY_SIZE);
    end;

    ftFAT32:
    begin
      maxClustersInFAT := (FATSizeInBytes div FAT32_ENTRY_SIZE);
    end;
  end;

  // First two FAT entries are reserved
  maxClustersInFAT := (maxClustersInFAT - CLUSTER_FIRST_DATA_CLUSTER);

  Result := min(
    // Calculated max number of clusters in the data area
    (SectorsInDataArea() div SectorsPerCluster), maxClustersInFAT);

end;

function TSDFilesystem_FAT.SectorsInDataArea(): DWORD;
var
  sectorsBeforeDataArea: DWORD;
begin
  if ((FATType = ftFAT12) or (FATType = ftFAT16)) then begin
    sectorsBeforeDataArea := ReservedSectorCount +         // Reserved sectors
      (FATCount * SectorsPerFAT) +                         // FATs
      ((MaxRootEntries * DIR_ENTRY_SIZE) div BytesPerSector);
    // Root directory
  end else begin
    sectorsBeforeDataArea := ReservedSectorCount +        // Reserved sectors
      (FATCount * SectorsPerFAT);                         // FATs

  end;

  Result := (TotalSectors - sectorsBeforeDataArea); // The number of sectors in the data area

end;

function TSDFilesystem_FAT.ClusterSize(): Int64;
var
  tmpInt64_A: Int64;
  tmpInt64_B: Int64;
begin
  tmpInt64_A := SectorsPerCluster;
  tmpInt64_B := BytesPerSector;
  Result     := (tmpInt64_A * tmpInt64_B);
end;

 // Get the next empty FAT entry (cluster ID).
 // afterClusterID - If nonzero, get the next free cluster ID after the
 //                  specified cluster ID
 // Returns ERROR_DWORD on failure/if there are no free FAT entries
function TSDFilesystem_FAT.GetNextEmptyFATEntry(afterClusterID: DWORD = 0): DWORD;
var
  maskedFATEntry: DWORD;
  i:              DWORD;
begin
  Result := ERROR_DWORD;

  if (afterClusterID < FFATEntryUsedStart) then begin
    afterClusterID := FFATEntryUsedStart;
  end;

  for i := (afterClusterID + 1) to MaxClusterID() do begin
    maskedFATEntry := GetFATEntry(i) and FFATEntryMask;
    if (maskedFATEntry = FFATEntryFree) then begin
      Result := i;
      break;
    end;
  end;


end;

 // Mark the next empty FAT entry as reservced/mark it as empty
 // Returns
function TSDFilesystem_FAT.ReserveFATEntry(afterClusterID: DWORD = 0): DWORD;
var
  Result: DWORD;
begin
  Result := GetNextEmptyFATEntry(afterClusterID);
  if (Result <> ERROR_DWORD) then begin
    SetFATEntry(Result, FFATEntryEOCEnd);
  end;

end;

procedure TSDFilesystem_FAT.UnreserveFATEntry(clusterID: DWORD);
begin
  if (clusterID <> ERROR_DWORD) then begin
    SetFATEntry(clusterID, FFATEntryFree);
  end;

end;

function TSDFilesystem_FAT.CountEmptyFATEntries(): DWORD;
var
  Result:         DWORD;
  maskedFATEntry: DWORD;
  i:              DWORD;
begin
  Result := 0;

  for i := FFATEntryUsedStart to MaxClusterID() do begin
    maskedFATEntry := GetFATEntry(i) and FFATEntryMask;
    if (maskedFATEntry = FFATEntryFree) then begin
      Inc(Result);
    end;
  end;


end;

function TSDFilesystem_FAT.SetFATEntry(clusterID: DWORD; Value: DWORD): Boolean;
begin
  FSerializeCS.Acquire();
  try
    if (FATType = ftFAT12) then begin
      Result := SetFATEntry_FAT12(clusterID, Value);
    end else begin
      Result := SetFATEntry_FAT1632(clusterID, Value);
    end;

  finally
    FSerializeCS.Release();
  end;

end;

function TSDFilesystem_FAT.SetFATEntry_FAT12(clusterID: DWORD; Value: DWORD): Boolean;
var
  Result:    Boolean;
  tmpDouble: Double;
  prevWord:  Word;
  newWord:   Word;
begin
  Result := True;

  tmpDouble     := clusterID * FAT12_ENTRY_SIZE;
  FFAT.Position := trunc(tmpDouble);
  prevWord      := FFAT.ReadWORD_LE();

  if SDUIsOddNumber(clusterID) then begin
    newWord := ((Value and FFATEntryMask) shl 4) + (prevWord and $0F);
  end else begin
    newWord := (Value and FFATEntryMask) + (prevWord and $F000);
  end;

  FFAT.Position := trunc(tmpDouble);
  FFAT.WriteWORD_LE(newWord);


end;

function TSDFilesystem_FAT.SetFATEntry_FAT1632(clusterID: DWORD; Value: DWORD): Boolean;
var
  Result: Boolean;
  i:      DWORD;
begin
  Result := True;

  // FAT entries are stored LSB first
  // Note: Length of FAT entries VARIES with FAT12/FAT16/FAT32!
  FFAT.Position := (clusterID * FFATEntrySize);
  for i := 0 to (FFATEntrySize - 1) do begin
    FFAT.WriteByte(Value and $FF);
    Value := Value shr 8;
  end;


end;


 // Traverse cluster chain, populating *pre-sized* chain
 // If the length of "chain" is zero, it won't be populated
 // Returns: The number of clusters in the chain
function TSDFilesystem_FAT._TraverseClusterChain(clusterID: DWORD;
  var chain: TSDFATClusterChain): DWORD;
var
  maskedFATEntry: DWORD;
  currClusterID:  DWORD;
  Result:         DWORD;
  finished:       Boolean;
  writeChain:     Boolean;
begin
  Result := 0;

  writeChain := (length(chain) > 0);

  // Special case - cluster zero on a FAT12/FAT16 is the root dir.
  // In this case, just return a single "cluster" with that cluster ID
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and (clusterID = CLUSTER_ZERO)) then begin
    if writeChain then begin
      chain[0] := CLUSTER_ZERO;
    end;

    Result := 1; // Single "cluster"
  end else begin
    // NOTE: If FAT32, we don't translate cluster 0 to the root cluster here;
    //       that's catered for in the read/write cluster functions

    finished      := False;
    currClusterID := clusterID;
    while not (finished) do begin
      maskedFATEntry := GetFATEntry(currClusterID) and FFATEntryMask;

      if ((maskedFATEntry >= FFATEntryUsedStart) and (maskedFATEntry <= FFATEntryUsedEnd))
      then begin
        // Include this one
        if writeChain then begin
          chain[Result] := currClusterID;
        end;

        Inc(Result);

        currClusterID := maskedFATEntry;
      end else
      if ((maskedFATEntry >= FFATEntryEOCStart) and (maskedFATEntry <= FFATEntryEOCEnd))
      then begin
        // Include this one
        if writeChain then begin
          chain[Result] := currClusterID;
        end;

        Inc(Result);

        // Finished.
        finished := True;
      end else
      if (maskedFATEntry = FFATEntryFree) then begin
        // This shouldn't happen - shouldn't normally be calling this routine
        // with free FAT cluster IDs
        finished := True;
      end else
      if (maskedFATEntry = FFATEntryBadSector) then begin
        // Uh oh!
        // Effectivly just truncate at this point...
        finished := True;
      end;

    end;
  end;


end;

// Extract the data for a single, specified, cluster
function TSDFilesystem_FAT.ExtractClusterData(clusterID: DWORD; data: TStream;
  maxSize: Integer = -1): Boolean;
begin
  Result := ReadWriteClusterData(True, clusterID, data, maxSize);
end;

// NOTE: This writes starting from current position in "data"
function TSDFilesystem_FAT.StoreClusterData(clusterID: DWORD; data: TStream;
  maxSize: Integer = -1): Boolean;
var
  maxDataSize: Int64;
begin
  // Sanity check...
  AssertSufficientData(data, maxSize);

  if (maxSize < 0) then begin
    maxDataSize := (data.Size - data.Position);

    if (((FATType = ftFAT12) or (FATType = ftFAT16)) and (clusterID = CLUSTER_ZERO)) then begin
      // Don't worry about max size - checked at point it writes the root dir
    end else begin
      maxSize := min(ClusterSize(), maxDataSize);
    end;

  end;

  Result := ReadWriteClusterData(False, clusterID, data, maxSize);
end;

function TSDFilesystem_FAT.ReadWriteClusterData(readNotWrite: Boolean;
  clusterID: DWORD; data: TStream; maxSize: Integer): Boolean;
var
  Result: Boolean;
begin
  // Special case - FAT12/FAT16 root directory
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and (clusterID = CLUSTER_ZERO)) then begin
    Result := ReadWriteFAT1216RootDir(readNotWrite, data);
  end else begin
    Result := _ReadWriteClusterData(readNotWrite, clusterID, data, maxSize);
  end;


end;

function TSDFilesystem_FAT._ReadWriteClusterData(readNotWrite: Boolean;
  clusterID: DWORD; data: TStream; maxSize: Integer): Boolean;
var
  startSectorID:  DWORD;
  allOK:          Boolean;
  bytesRemaining: Integer;
  sectorMax:      Integer;
  tmpInt64:       Int64;
begin
  // Sanity check...
  if not (readNotWrite) then begin
    AssertSufficientData(data, maxSize);
  end;

  bytesRemaining := maxSize;
  if (bytesRemaining < 0) then begin
    if readNotWrite then begin
      // Set the number of bytes remaining to the total number of bytes in the
      // cluster (we process the entire cluster)
      bytesRemaining := SectorsPerCluster * BytesPerSector;
    end else begin
      tmpInt64       := (data.Size - data.Position);
      bytesRemaining := min((SectorsPerCluster * BytesPerSector), tmpInt64);
    end;
  end;

  startSectorID := SectorIDForCluster(clusterID);
  sectorMax     := min(bytesRemaining, (BytesPerSector * SectorsPerCluster));

  if readNotWrite then begin
    allOK := PartitionImage.ReadConsecutiveSectors(startSectorID, data, sectorMax);
  end else begin
    allOK := PartitionImage.WriteConsecutiveSectors(startSectorID, data, sectorMax);
  end;

  bytesRemaining := bytesRemaining - sectorMax;

  if allOK then begin
    allOK := (bytesRemaining = 0);
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.ExtractClusterChainData(clusterID: DWORD; data: TStream;
  maxSize: Int64 = -1): Boolean;
var
  chain: TSDFATClusterChain;
begin
  chain  := ExtractClusterChain(clusterID);
  Result := ExtractClusterChainData(chain, data, maxSize);
end;

function TSDFilesystem_FAT.ExtractClusterChainData(chain: TSDFATClusterChain;
  data: TStream; maxSize: Int64 = -1): Boolean;
var
  bytesToProcess: Int64;
begin
  bytesToProcess := maxSize;

  // Special case - FAT12/FAT16 root directory
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and IsClusterInChain(CLUSTER_ZERO, chain))
  then begin
    // bytesToProcess already set to maxSize
  end else begin
    if (bytesToProcess < 0) then begin
      bytesToProcess := ClusterSize() * Int64(length(chain));
    end;
  end;

  Result := ReadWriteClusterChainData(True, chain, data, bytesToProcess);
end;


function TSDFilesystem_FAT.StoreClusterChainData(chain: TSDFATClusterChain;
  data: TStream; maxSize: Int64 = -1): Boolean;
var
  bytesToProcess: Int64;
  maxDataSize:    Int64;
  maxChainSize:   Int64;
begin
  // Sanity check...
  AssertSufficientData(data, maxSize);

  bytesToProcess := maxSize;
  maxDataSize    := (data.Size - data.Position);

  // Special case - FAT12/FAT16 root directory
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and IsClusterInChain(CLUSTER_ZERO, chain))
  then begin
    // Don't worry about max maxChainSize - checked at point it writes the root dir
  end else begin
    maxChainSize := ClusterSize() * Int64(length(chain));

    if (bytesToProcess < 0) then begin
      bytesToProcess := min(maxDataSize, maxChainSize);
    end;

    Assert(
      (bytesToProcess <= maxChainSize),
      'StoreClusterChainData(...) call attempts to store more data than the cluster chain supplied can hold'
      );

  end;

  // Sanity check...
  Assert(
    (bytesToProcess <= maxDataSize),
    'StoreClusterChainData(...) call attempts to store more data than supplied'
    );

  Result := ReadWriteClusterChainData(False, chain, data, bytesToProcess);
end;

 // Calculate based on size of "maxSize". If that's set to a -ve value, fallback
 // to calculating based on the size of "data"
function TSDFilesystem_FAT.DetermineClustersNeeded(data: TStream; maxSize: Int64 = -1): DWORD;
var
  Result:      DWORD;
  maxDataSize: Int64;
  tmpInt64:    Int64;  // Used to prevent Delphi casting incorrectly
  useSize:     Int64;
begin
  maxDataSize := 0;
  if (data <> nil) then begin
    maxDataSize := (data.Size - data.Position);
  end;

  useSize := maxDataSize;
  if (maxSize >= 0) then begin
    useSize := maxSize;
  end;

  tmpInt64 := (useSize div ClusterSize());
  Result   := tmpInt64;

  tmpInt64 := (useSize mod ClusterSize());
  if (tmpInt64 <> 0) then begin
    Inc(Result);
  end;


end;

 // Truncate/extend chain as appropriate such that it can store the data
 // specified
 // unusedChain - If "chain" passed in is truncated, this will be set to the
 //               unused clusters in the chain
function TSDFilesystem_FAT.AllocateChainForData(var chain: TSDFATClusterChain;
  var unusedChain: TSDFATClusterChain; data: TStream; maxSize: Int64 = -1): Boolean;
var
  clustersNeeded:   DWORD;
  origChainLength:  DWORD;
  newChain:         TSDFATClusterChain;
  lastAllocCluster: DWORD;
  nextFreeCluster:  DWORD;
  Result:           Boolean;
  i:                DWORD;
begin
  Result := True;

  origChainLength := length(chain);
  clustersNeeded  := DetermineClustersNeeded(data, maxSize);

  // Setup new chain and unused portion of chain...
  SetLength(newChain, clustersNeeded);
  SetLength(unusedChain, 0);

  // Copy from original chain as must as we can, and need...
  for i := 1 to min(clustersNeeded, origChainLength) do begin
    newChain[i - 1] := chain[i - 1];
  end;

  // Store any unused portion of the original chain...
  if (origChainLength > clustersNeeded) then begin
    SetLength(unusedChain, (origChainLength - clustersNeeded));
    for i := 1 to (origChainLength - clustersNeeded) do begin
      unusedChain[i - 1] := chain[clustersNeeded + i - 1];
    end;
  end // Extend chain passed in...
  else
  if (clustersNeeded > origChainLength) then begin
    // Extend chain, if needed
    lastAllocCluster := 0;
    for i := 1 to (clustersNeeded - origChainLength) do begin
      nextFreeCluster := GetNextEmptyFATEntry(lastAllocCluster);
      if (nextFreeCluster = ERROR_DWORD) then begin
        // Unable to extend...
        Result := False;
        break;
      end;

      newChain[origChainLength + i - 1] := nextFreeCluster;
      lastAllocCluster                  := nextFreeCluster;
    end;
  end;

  if Result then begin
    chain := newChain;
  end;


end;

function TSDFilesystem_FAT.ReadWriteClusterChainData(readNotWrite: Boolean;
  chain: TSDFATClusterChain; data: TStream; maxSize: Int64 = -1): Boolean;
var
  Result: Boolean;
begin
  // Special case - FAT12/FAT16 root directory
  if (((FATType = ftFAT12) or (FATType = ftFAT16)) and IsClusterInChain(CLUSTER_ZERO, chain))
  then begin
    Result := ReadWriteFAT1216RootDir(readNotWrite, data);
  end else begin
    Result := _ReadWriteClusterChainData(readNotWrite, chain, data, maxSize);
  end;


end;

function TSDFilesystem_FAT._ReadWriteClusterChainData(readNotWrite: Boolean;
  chain: TSDFATClusterChain; data: TStream; maxSize: Int64 = -1): Boolean;
var
  Result:         Boolean;
  i:              Integer;
  useClusterSize: Int64;
  bytesRemaining: Int64;
begin
  // Sanity check...
  if not (readNotWrite) then begin
    AssertSufficientData(data, maxSize);
  end;

  Result := True;

  bytesRemaining := maxSize;
  if (bytesRemaining < 0) then begin
    if readNotWrite then begin
      bytesRemaining := SectorsPerCluster;
      bytesRemaining := bytesRemaining * BytesPerSector;
      bytesRemaining := bytesRemaining * length(Chain);
    end else begin
      bytesRemaining := (data.Size - data.Position);
    end;
  end;

  for i := low(chain) to high(chain) do begin
    useClusterSize := min(bytesRemaining, ClusterSize());

    if readNotWrite then begin
      Result := ExtractClusterData(chain[i], data, useClusterSize);
    end else begin
      Result := StoreClusterData(chain[i], data, useClusterSize);
    end;

    if not (Result) then begin
      break;
    end;

    bytesRemaining := bytesRemaining - useClusterSize;

    // If we can exit early...
    if (bytesRemaining <= 0) then begin
      break;
    end;

  end;

  if Result then begin
    Result := (bytesRemaining = 0);
  end;


end;

function TSDFilesystem_FAT.ExtractClusterChainData(clusterID: DWORD; filename: String;
  maxSize: Int64 = -1): Boolean;
var
  allOK:      Boolean;
  fileStream: TFileStream;
begin
  fileStream := TFileStream.Create(filename, fmCreate);
  try
    allOK := ExtractClusterChainData(clusterID, fileStream, maxSize);
  finally
    fileStream.Free();
  end;

  Result := allOK;
end;


// Note: TTimeStamp includes a datestamp
function TSDFilesystem_FAT.WORDToTTimeStamp(dateBitmask: Word; timeBitmask: Word;
  msec: Byte): TTimeStamp;
var
  dd:     Integer;
  mm:     Integer;
  yyyy:   Integer;
  hh:     Integer;
  mi:     Integer;
  ss:     Integer;
  Result: TTimeStamp;
begin
  dd   := (dateBitmask and $1F);
  mm   := ((dateBitmask and $1E0) shr 5);
  yyyy := 1980 + ((dateBitmask and $FE00) shr 9);
  ss   := ((timeBitmask and $1F) * 2);
  mi   := ((timeBitmask and $7E0) shr 5);
  hh   := ((timeBitmask and $F800) shr 11);

  try
    Result := DateTimeToTimeStamp(EncodeDateTime(yyyy, mm, dd, hh, mi, ss, msec));
  except
    // Dud date/timestamp
    Result.Date := 0;
    Result.Time := 0;
  end;


end;


procedure TSDFilesystem_FAT.TTimeStampToWORD(timeStamp: TTimeStamp;
  var dateBitmask: Word; var timeBitmask: Word; var msec: Byte);
var
  dd:      Word;
  mm:      Word;
  yyyy:    Word;
  hh:      Word;
  mi:      Word;
  ss:      Word;
  tmpMsec: Word;
begin
  try
    DecodeDateTime(
      TimeStampToDateTime(timeStamp),
      yyyy,
      mm,
      dd,
      hh,
      mi,
      ss,
      tmpMsec
      );

    yyyy        := yyyy - 1980;
    dateBitmask := dd + (mm shl 5) + (yyyy shl 9);

    timeBitmask := (ss div 2) + (mi shl 5) + (hh shl 11);

    msec        := tmpMsec

  except
    // Dud date/timestamp
    dateBitmask := 0;
    timeBitmask := 0;
    msec        := 0;
  end;

end;

function TSDFilesystem_FAT.WORDToTDate(dateBitmask: Word): TDate;
var
  dd:     Integer;
  mm:     Integer;
  yyyy:   Integer;
  Result: TDate;
begin
  dd   := (dateBitmask and $1F);
  mm   := ((dateBitmask and $1E0) shr 5);
  yyyy := 1980 + ((dateBitmask and $FE00) shr 9);

  try
    Result := EncodeDate(yyyy, mm, dd);
  except
    // Dud date/timestamp
    Result := 0;
  end;


end;

function TSDFilesystem_FAT.TDateToWORD(date: TDate): Word;
var
  dd:     Word;
  mm:     Word;
  yyyy:   Word;
  Result: Word;
begin
  try
    DecodeDate(date, yyyy, mm, dd);
    yyyy   := yyyy - 1980;
    Result := dd + (mm shl 5) + (yyyy shl 9);
  except
    // Dud date/timestamp
    Result := 0;
  end;


end;

function TSDFilesystem_FAT.ParseDirectory(data: TSDUMemoryStream;
  var dirContent: TSDDirItemList): Boolean;
var
  currItem:       TSDDirItem_FAT;
  sfnFilename:    String;
  sfnFileExt:     String;
  recordOffset:   Int64;
  clusterHi:      DWORD;
  clusterLo:      DWORD;
  currAttributes: DWORD;
  lfn:            WideString;
  lfnPart:        WideString;
  lfnChkSum:      Byte;
  currChkSum:     Byte;
  seqNo:          DWORD;
  Result:         Boolean;
  caseByte:       Byte;
begin
  Result := True;

  lfn          := '';
  lfnChkSum    := 0;
  recordOffset := 0;
  while (recordOffset < data.Size) do begin
    sfnFilename := data.ReadString(DIR_ENTRY_LENGTH_DOSFILENAME,
      (recordOffset + DIR_ENTRY_OFFSET_DOSFILENAME));

    // Skip free
    if (Ord(sfnFilename[1]) = DIR_ENTRY_UNUSED) then begin
      lfnChkSum    := 0;
      lfn          := '';
      recordOffset := recordOffset + DIR_ENTRY_SIZE;
      continue;
    end;
    // Skip unused
    if (Ord(sfnFilename[1]) = DIR_ENTRY_DELETED) then begin
      lfnChkSum    := 0;
      lfn          := '';
      recordOffset := recordOffset + DIR_ENTRY_SIZE;
      continue;
    end;

    currAttributes := data.ReadByte(recordOffset + DIR_ENTRY_OFFSET_FILEATTRS);

    if ((currAttributes and VFAT_ATTRIB_VFAT_ENTRY) = VFAT_ATTRIB_VFAT_ENTRY) then begin
      seqNo      := data.ReadByte(recordOffset + DIR_ENTRY_OFFSET_VFAT_SEQ_NO);
      currChkSum := data.ReadByte(recordOffset + DIR_ENTRY_OFFSET_VFAT_CHECKSUM);
      if ((seqNo and DIR_ENTRY_VFAT_LAST_LONG_ENTRY) = DIR_ENTRY_VFAT_LAST_LONG_ENTRY) then begin
        // Next line commented out to prevent compiler error
        //        seqNo := seqNo and not(DIR_ENTRY_VFAT_LAST_LONG_ENTRY);
        lfnChkSum := currChkSum;
        lfn       := '';
      end;

      // Sanity check
      if (lfnChkSum <> currChkSum) then begin
        //lplp - handle - REJECT
      end;

      lfnPart := data.ReadWideString(10,
        (recordOffset + DIR_ENTRY_OFFSET_VFAT_NAME_PART_1));
      lfnPart := lfnPart + data.ReadWideString(12,
        (recordOffset + DIR_ENTRY_OFFSET_VFAT_NAME_PART_2));
      lfnPart := lfnPart + data.ReadWideString(
        4, (recordOffset + DIR_ENTRY_OFFSET_VFAT_NAME_PART_3));

      lfn := lfnPart + lfn;

      //showmessage('LFN CHECKSUM:'+SDUCRLF+inttostr(data.ReadByte((recordOffset+DIR_ENTRY_OFFSET_VFAT_CHECKSUM))));
    end else begin
      currItem := TSDDirItem_FAT.Create();

      sfnFilename          := trim(sfnFilename);
      sfnFileExt           := trim(data.ReadString(3, (recordOffset +
        DIR_ENTRY_OFFSET_DOSEXTENSION)));
      currItem.FilenameDOS := sfnFilename;
      if (sfnFileExt <> '') then begin
        currItem.FilenameDOS := currItem.FilenameDOS + '.' + sfnFileExt;
      end;

      //showmessage('CHECKSUM:'+SDUCRLF+currItem.FilenameDOS+SDUCRLF+inttostr(DOSFilenameCheckSum(currItem.FilenameDOS)));

      // Only use the first chars up to (WideChar) #0
      lfn := Copy(lfn, 1, SDUWStrLen(PWideChar(lfn)));

      currItem.Filename := lfn;
      if ((currItem.Filename = '') or (DOSFilenameCheckSum(currItem.FilenameDOS) <>
        lfnChkSum) // Abandon any LFN if it's checksum fails
        ) then begin
        caseByte := data.ReadByte(recordOffset + DIR_ENTRY_OFFSET_RESERVED);
        if ((caseByte and DIR_ENTRY_83CASE_BASENAME) = DIR_ENTRY_83CASE_BASENAME) then begin
          sfnFilename := lowercase(sfnFilename);
        end;
        if ((caseByte and DIR_ENTRY_83CASE_EXTN) = DIR_ENTRY_83CASE_EXTN) then begin
          sfnFileExt := lowercase(sfnFileExt);
        end;

        currItem.Filename := sfnFilename;
        if (sfnFileExt <> '') then begin
          currItem.Filename := currItem.Filename + '.' + sfnFileExt;
        end;
      end;

      currItem.Attributes := currAttributes;

      currItem.TimestampCreation     :=
        WORDToTTimeStamp(data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_CREATEDATE),
        data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_CREATETIME),
        data.ReadByte(recordOffset + DIR_ENTRY_OFFSET_CREATETIMEFINE));
      currItem.DatestampLastAccess   :=
        WORDToTDate(data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_LASTACCESSDATE));
      currItem.TimestampLastModified :=
        WORDToTTimeStamp(data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_LASTMODDATE),
        data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_LASTMODTIME), 0);

      clusterLo := data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_FIRSTCLUSTERLO);
      clusterHi := 0;
      if (FATType = ftFAT32) then begin
        clusterHi := data.ReadWORD_LE(recordOffset + DIR_ENTRY_OFFSET_EAINDEX);
      end;

      currItem.FirstCluster := (clusterHi shl 16) + clusterLo;

      currItem.Size := data.ReadDWORD_LE(recordOffset + DIR_ENTRY_OFFSET_FILESIZE);


      dirContent.Add(currItem);

      lfn       := '';
      lfnChkSum := 0;
    end;

    recordOffset := recordOffset + DIR_ENTRY_SIZE;
  end;


end;

function TSDFilesystem_FAT.SectorIDForCluster(clusterID: DWORD): DWORD;
var
  Result: DWORD;
begin
  if ((FATType = ftFAT12) or (FATType = ftFAT16)) then begin
    if (clusterID = CLUSTER_ZERO) then begin
      // FAT12/FAT16 - should never want sector ID for cluster zero; this
      // cluster refers to the root dir, which should be accessed via
      // ReadWriteFAT1216RootDir(...)
      Assert(
        (1 = 2),
        'SectorIDForCluster(...) called for FAT12/FAT16 volume with CLUSTER_ZERO'
        );
      Result := 0; // Ger rid of compiler error
    end else begin
      // -2 (CLUSTER_FIRST_DATA_CLUSTER) because the 2nd cluster is the zero'th
      // cluster from the start of the data beyond the FATs; clusters 0 and 1
      // are "skipped"
      Result := ReservedSectorCount +                                     // Reserved sectors
        (FATCount * SectorsPerFAT) +                              // FATs
        ((MaxRootEntries * DIR_ENTRY_SIZE) div BytesPerSector) +  // Root directory
        (SectorsPerCluster * (clusterID - CLUSTER_FIRST_DATA_CLUSTER)); // Cluster required
    end;
  end else begin
    // Subdirs off the root dir still use first cluster 0 (zero) for "..";
    // adjust this to be the root dir first cluster
    if (clusterID = 0) then begin
      clusterID := RootDirFirstCluster;
    end;

    // -2 because the 2nd cluster is the zero'th cluster from the start of
    // the data beyond the FATs; clusters 0 and 1 are "skipped"
    Result := ReservedSectorCount +                   // Reserved sectors
      (FATCount * SectorsPerFAT) +            // FATs
      (SectorsPerCluster * (clusterID - CLUSTER_FIRST_DATA_CLUSTER));  // Cluster required
  end;


end;

function TSDFilesystem_FAT.LoadContentsFromDisk(path: String; items: TSDDirItemList): Boolean;
var
  startClusterID: DWORD;
  Result:         Boolean;
begin
  Result := False;

  startClusterID := GetStartingClusterForItem(path);
  if (startClusterID <> ERROR_DWORD) then begin
    Result := _LoadContentsFromDisk(startClusterID, items);

    if not (Result) then begin
      LastErrorSet(SDUParamSubstitute(_('Unable to read contents of: %1'), [path]));
    end;
  end;


end;

function TSDFilesystem_FAT._LoadContentsFromDisk(dirStartCluster: DWORD;
  items: TSDDirItemList): Boolean;
var
  ms:     TSDUMemoryStream;
  Result: Boolean;
begin
  AssertMounted();

  Result := False;

  ms := TSDUMemoryStream.Create();
  try
    ms.Position := 0;
    if ExtractClusterChainData(dirStartCluster, ms) then begin
      ms.Position := 0;
      Result      := ParseDirectory(ms, items);
    end;
  finally
    ms.Free();
  end;


end;


// Returns ERROR_DWORD on failure
function TSDFilesystem_FAT.GetStartingClusterForItem(path: WideString): DWORD;
var
  item:   TSDDirItem_FAT;
  Result: DWORD;
begin
  Result := ERROR_DWORD;

  item := TSDDirItem_FAT.Create();
  try
    if GetItem_FAT(path, item) then begin
      Result := item.FirstCluster;
    end;
  finally
    item.Free();
  end;


end;

function TSDFilesystem_FAT.GetRootDirItem(item: TSDDirItem_FAT): Boolean;
begin
  inherited;

  item.Filename               := PATH_SEPARATOR;
  item.FilenameDOS            := PATH_SEPARATOR;
  item.Attributes             := VFAT_ATTRIB_FLAG_SUBDIR;
  item.TimestampCreation.Date := 0;
  item.TimestampCreation.Time := 0;
  item.DatestampLastAccess    := 0;
  item.TimestampLastModified  := item.TimestampCreation;
  item.FirstCluster           := RootDirFirstCluster;  // Note: This will be 0 (zero) for
  // FAT12/FAT16. The actual cluster
  // ID for FAT32
  item.Size                   := 0;

  Result := True;
end;

function TSDFilesystem_FAT.GetItem(path: WideString; item: TSDDirItem): Boolean;
var
  tmpItem: TSDDirItem_FAT;
  allOK:   Boolean;
begin
  tmpItem := TSDDirItem_FAT.Create();
  try
    allOK := GetItem_FAT(path, tmpItem);
    if allOK then begin
      item.Assign(tmpItem);
    end;

  finally
    tmpItem.Free();
  end;

  Result := allOK;
end;

// Returns ERROR_DWORD on failure
function TSDFilesystem_FAT.GetItem_FAT(path: WideString; item: TSDDirItem_FAT): Boolean;
var
  dirContent:    TSDDirItemList;
  foundPathPart: Boolean;
  nextPathPart:  WideString;
  pathRemaining: WideString;
  nextCluster:   DWORD;
  i:             Integer;
  dirData:       TSDUMemoryStream;
begin
  // Sanity check...
  if (path = '') then begin
    Result := False;
    exit;
  end;

  // Normalize by ensuring path is prefixed with a "\"
  if path[1] <> PATH_SEPARATOR then begin
    path := PATH_SEPARATOR + path;
  end;

  dirData := TSDUMemoryStream.Create();
  try
    dirData.SetSize(0);
    dirData.Position := 0;
    // Take out the leading "\"; setting nextCluster to the 1st cluster of the root
    // dir
    SDUSplitWideString(path, nextPathPart, pathRemaining, PATH_SEPARATOR);
    GetRootDirItem(item);
    nextCluster := item.FirstCluster;
    ExtractClusterChainData(nextCluster, dirData);

    dirContent := TSDDirItemList.Create();
    try
      ParseDirectory(dirData, dirContent);

      while ((pathRemaining <> '') and (nextCluster <> ERROR_DWORD)) do begin
        foundPathPart := False;

        SDUSplitWideString(pathRemaining, nextPathPart, pathRemaining, PATH_SEPARATOR);

        for i := 0 to (dirContent.Count - 1) do begin
          if (uppercase(dirContent[i].Filename) = uppercase(nextPathPart)) then begin
            // Ignore volume labels and devices; skip to next dir entry
            if (((TSDDirItem_FAT(dirContent[i]).Attributes and
              VFAT_ATTRIB_FLAG_VOLLABEL) = VFAT_ATTRIB_FLAG_VOLLABEL) or
              ((TSDDirItem_FAT(dirContent[i]).Attributes and VFAT_ATTRIB_FLAG_DEVICE) =
              VFAT_ATTRIB_FLAG_DEVICE)) then begin
              continue;
            end else begin
              foundPathPart := True;
              item.Assign(TSDDirItem_FAT(dirContent[i]));
              nextCluster := item.FirstCluster;

              // Found what we were looking for; just break out
              if (pathRemaining = '') then begin
                break;
              end else begin
                if TSDDirItem_FAT(dirContent[i]).IsDirectory then begin
                  // Setup for the next dir...
                  dirData.SetSize(0);
                  dirData.Position := 0;
                  ExtractClusterChainData(nextCluster, dirData);

                  dirContent.Free();
                  dirContent := TSDDirItemList.Create();
                  ParseDirectory(dirData, dirContent);
                  break;
                end else begin
                  // Problem - path segment matches file (or non-dir), but there's
                  // more on the path
                  nextCluster := ERROR_DWORD;
                  break;
                end;

              end;

            end;

          end;

        end;

        // Unable to locate...
        if not (foundPathPart) then begin
          nextCluster := ERROR_DWORD;
          break;
        end;

      end;
    finally
      dirContent.Free();
    end;

  finally
    dirData.Free();
  end;

  Result := (nextCluster <> ERROR_DWORD);
end;

function TSDFilesystem_FAT.GetItemContent(path: WideString; content: TStream): Boolean;
var
  item:   TSDDirItem_FAT;
  Result: Boolean;
begin
  Result := False;
  item   := TSDDirItem_FAT.Create();
  try
    if GetItem_FAT(path, item) then begin
      ExtractClusterChainData(item.FirstCluster, content);
      Result := True;
    end;
  finally
    item.Free();
  end;


end;


function TSDFilesystem_FAT.GetFileContent(path: WideString; fileContent: TStream): Boolean;
var
  item:   TSDDirItem_FAT;
  Result: Boolean;
begin
  Result := False;
  item   := TSDDirItem_FAT.Create();
  try
    if GetItem_FAT(path, item) then begin
      if (item.Size <> 0) then begin
        ExtractClusterChainData(item.FirstCluster, fileContent, item.Size);
      end;

      Result := True;
    end;
  finally
    item.Free();
  end;


end;

function TSDFilesystem_FAT.ExtractFile(srcPath: WideString; extractToFilename: String): Boolean;
var
  item:             TSDDirItem_FAT;
  Result:           Boolean;
  attrs:            Integer;
  fileHandle:       THandle;
  ftCreationTime:   TFileTime;
  ftLastAccessTime: TFileTime;
  ftLastWriteTime:  TFileTime;
begin
  Result := False;

  item := TSDDirItem_FAT.Create();
  try
    if GetItem_FAT(srcPath, item) then begin
      Result := ExtractClusterChainData(item.FirstCluster, extractToFilename, item.Size);

      // If extraction successful, set file attributes and date/timestamps
      if not (Result) then begin
        LastErrorSet(
          SDUParamSubstitute(_(
          'Unable to extract cluster chain data starting from cluster: %1'), [item.FirstCluster])
          );
      end else begin
        // Set file attributes...
        attrs := 0;
        // Disable useless warnings about faReadOnly, etc and FileSetAttr(...) being
        // platform-specific
{$WARN SYMBOL_PLATFORM OFF}
        if item.IsReadonly then begin
          attrs := attrs or faReadOnly;
        end;
        if ((item.Attributes and VFAT_ATTRIB_FLAG_HIDDEN) = VFAT_ATTRIB_FLAG_HIDDEN) then begin
          attrs := attrs or faHidden;
        end;
        if ((item.Attributes and VFAT_ATTRIB_FLAG_SYSTEM) = VFAT_ATTRIB_FLAG_SYSTEM) then begin
          attrs := attrs or faSysFile;
        end;

        FileSetAttr(extractToFilename, attrs);
{$WARN SYMBOL_PLATFORM ON}

        // Set file timestamps...
        if PreserveTimeDateStamps then begin
          fileHandle := FileOpen(extractToFilename, fmOpenWrite);
          if (fileHandle = 0) then begin
            LastErrorSet(
              SDUParamSubstitute(_(
              'Unable to open extracted file to set timestamps: %1'),
              [extractToFilename])
              );
          end else begin
            ftCreationTime   := SDUDateTimeToFileTime(TimeStampToDateTime(item.TimestampCreation));
            ftLastAccessTime := SDUDateTimeToFileTime(item.DatestampLastAccess);
            ftLastWriteTime  := SDUDateTimeToFileTime(TimeStampToDateTime(
              item.TimestampLastModified));

            SetFileTime(
              fileHandle, @ftCreationTime, @ftLastAccessTime, @ftLastWriteTime
              );

            FileClose(fileHandle);
          end;
        end;

      end;

    end;

  finally
    item.Free();
  end;


end;

function TSDFilesystem_FAT.FreeCluster(clusterID: DWORD): Boolean;
begin
  Result := SetFATEntry(clusterID, FFATEntryFree);
end;

function TSDFilesystem_FAT.FreeClusterChain(clusterID: DWORD): Boolean;
var
  clusterChain: TSDFATClusterChain;
begin
  clusterChain := ExtractClusterChain(clusterID);
  Result       := FreeClusterChain(clusterChain);
end;

function TSDFilesystem_FAT.FreeClusterChain(clusterChain: TSDFATClusterChain): Boolean;
var
  i:      Integer;
  Result: Boolean;
begin
  Result := True;

  for i := low(clusterchain) to high(clusterChain) do begin
    if not (FreeCluster(clusterChain[i])) then begin
      Result := False;
    end;
  end;


end;

// atm, this only does a rudimentary check that all of the FATs are identical
function TSDFilesystem_FAT.CheckFilesystem(): Boolean;
var
  allOK: Boolean;
begin
  allOK := True;

  if allOK then begin
    allOK := CheckFilesystem_ConsistentFATs();
  end;

  if allOK then begin
    allOK := CheckFilesystem_Crosslinks();
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.CheckFilesystem_ConsistentFATs(): Boolean;
var
  i:        Integer;
  j:        Int64;
  firstFAT: TSDUMemoryStream;
  checkFAT: TSDUMemoryStream;
  Result:   Boolean;
begin
  firstFAT := TSDUMemoryStream.Create();
  try
    Result := ReadFAT(1, firstFAT);
    if Result then begin
      // Start from 2; we've already got the first FAT
      for i := 2 to FATCount do begin
        checkFAT := TSDUMemoryStream.Create();
        try
          Result := ReadFAT(1, checkFAT);

          // Compare the FATs...

          if Result then begin
            Result := (firstFAT.Size = checkFAT.Size);
          end;

          if Result then begin
            firstFAT.Position := 0;
            checkFAT.Position := 0;
            j                 := 0;
            while (j < firstFAT.Size) do begin
              Result := (firstFAT.ReadByte = checkFAT.ReadByte);
              if not (Result) then begin
                LastErrorSet(SDUParamSubstitute(
                  _('FAT copy #%1 doesn''t match FAT copy #%2'), [1, i]));
                break;
              end;

              Inc(j);
            end;
          end;

        finally
          checkFAT.Free();
        end;

        if not (Result) then begin
          break;
        end;

      end;
    end;

  finally
    firstFAT.Free();
  end;


end;

function TSDFilesystem_FAT.CheckFilesystem_Crosslinks(): Boolean;
var
  Result: Boolean;
begin
  Result := True;

  //lplp - to implement


end;


function TSDFilesystem_FAT.GetFreeSpace(): ULONGLONG;
var
  freeTotal: ULONGLONG;
  tmpULL:    ULONGLONG;  // Use temp var to prevent inappropriate casting by Delphi
begin
  freeTotal := CountEmptyFATEntries();

  tmpULL    := SectorsPerCluster;
  freeTotal := (freeTotal * tmpULL);

  tmpULL    := BytesPerSector;
  freeTotal := (freeTotal * tmpULL);

  Result := freeTotal;
end;

function TSDFilesystem_FAT.GetSize(): ULONGLONG;
var
  freeTotal: ULONGLONG;
  tmpULL:    ULONGLONG;  // Use temp var to prevent inappropriate casting by Delphi
begin
  freeTotal := TotalSectors;

  tmpULL    := BytesPerSector;
  freeTotal := (freeTotal * tmpULL);

  Result := freeTotal;
end;

procedure TSDFilesystem_FAT.AssertSufficientData(data: TStream; maxSize: Int64 = -1);
var
  maxDataSize: Int64;
begin
  if (maxSize >= 0) then begin
    maxDataSize := (data.Size - data.Position);
    Assert(
      (maxDataSize >= maxSize),
      'Insufficient data supplied for operation (need: ' + SDUIntToStr(
      maxSize) + '; got: ' + SDUIntToStr(maxDataSize) + ')'
      );
  end;
end;

 // Given a specfied path, return the directory it's stored in.
 // e.g.:
 //   \fred\bert\joe  will return \fred\bert
 //   \fred\bert      will return \fred
 //   \fred           will return \
 // Note that:
 //   \               will return \
function TSDFilesystem_FAT.PathParent(path: WideString): WideString;
var
  Result: WideString;
  i:      Integer;
begin
  Result := '';
  if path = PATH_SEPARATOR then begin
    Result := PATH_SEPARATOR;
  end else begin
    for i := length(path) downto 1 do begin
      if (path[i] = PATH_SEPARATOR) then begin
        Result := Copy(path, 1, (i - 1));
        if (Result = '') then begin
          Result := PATH_SEPARATOR;
        end;

        break;
      end;
    end;
  end;


end;


// Convert filename from 8.3 format to "NNNNNNNNEEE" format
function TSDFilesystem_FAT.DOSFilenameTo11Chars(DOSFilename: Ansistring): Ansistring;
var
  i:      Integer;
  j:      Integer;
  Result: Ansistring;
begin
  // Special handling for "." and ".." so the other half of this process
  // doens't get confused by the "." and return a string containing just
  // spaces
  if ((DOSFilename = DIR_CURRENT_DIR) or (DOSFilename = DIR_PARENT_DIR)) then begin
    Result := DOSFilename;
  end else begin
    for i := 1 to length(DOSFilename) do begin
      if (DOSFilename[i] = '.') then begin
        // Pad out...
        for j := i to 8 do begin
          Result := Result + ' ';
        end;
      end else begin
        Result := Result + DOSFilename[i];
      end;

    end;
  end;

  Result := Result + StringOfChar(AnsiChar(' '), (11 - length(Result)));


end;

// This takes a notmal 8.3 filename (e.g. fred.txt)
function TSDFilesystem_FAT.DOSFilenameCheckSum(DOSFilename: Ansistring): Byte;
var
  i:           Integer;
  Result:      Byte;
  useFilename: Ansistring;
begin
  useFilename := DOSFilenameTo11Chars(DOSFilename);

  Result := 0;
  for i := 1 to 11 do begin
    Result := (((Result and $0000001) shl 7) + (Result shr 1) + Ord(useFilename[i])) mod 256;
  end;


end;


 // Write directory entry to the specified stream, at the *current* *position* in
 // the stream.
 // If "stream" is set to nil, this will just return the number of directory
 // entries required, without writing anything
 // Returns: The number of directory entries used/required
 // Returns -1 on error
function TSDFilesystem_FAT.WriteDirEntry(item: TSDDirItem_FAT; stream: TSDUMemoryStream): Integer;
const
  // The number of LFN chars per dir entry
  LFN_CHARS_PER_ENTRY = 5 + 6 + 2;
var
  tempLFN:  WideString;
  lfnParts: array of WideString;
  maxSeqNo: Byte;
  useSeqNo: Byte;
  checksum: Byte;
  Result:   Integer;
  i:        Integer;
begin
  tempLFN := item.Filename;
  // Terminate LFN with NULL
  tempLFN := tempLFN + WChar($0000);
  // Pad LFN with #FFFF chars, if needed
  if ((length(tempLFN) mod LFN_CHARS_PER_ENTRY) > 0) then begin
    tempLFN := tempLFN + SDUWideStringOfWideChar(WChar($FFFF),
      (LFN_CHARS_PER_ENTRY - (length(tempLFN) mod LFN_CHARS_PER_ENTRY)));
  end;

  // Calculate number of directory entries required to store LFN
  maxSeqNo := length(tempLFN) div LFN_CHARS_PER_ENTRY;
  // (Additional directory entry required for 8.3 entry)
  Result   := maxSeqNo + 1;

  if (stream = nil) then begin
    // Bail out

    exit;
  end;

  SetLength(lfnParts, maxSeqNo);
  for i := 0 to (maxSeqNo - 1) do begin
    lfnParts[i] := Copy(tempLFN, 1, LFN_CHARS_PER_ENTRY);
    Delete(tempLFN, 1, LFN_CHARS_PER_ENTRY);
  end;

  checksum := DOSFilenameCheckSum(item.FilenameDOS);

  // Write out the LFN entries...
  for i := (maxSeqNo - 1) downto 0 do begin
    useSeqNo := i + 1;
    // Mask the last segment (first one to be written)
    if (useSeqNo = maxSeqNo) then begin
      useSeqNo := useSeqNo or DIR_ENTRY_VFAT_LAST_LONG_ENTRY;
    end;

    stream.WriteByte(useSeqNo);
    stream.WriteWideString(Copy(lfnParts[i], 1, 5));
    stream.WriteByte(VFAT_ATTRIB_VFAT_ENTRY);
    stream.WriteByte($00);
    stream.WriteByte(checksum);
    stream.WriteWideString(Copy(lfnParts[i], 6, 6));
    stream.WriteWORD_LE($0000);
    stream.WriteWideString(Copy(lfnParts[i], 12, 2));
  end;

  WriteDirEntry_83(item, stream);


end;

procedure TSDFilesystem_FAT.WriteDirEntry_83(item: TSDDirItem_FAT; stream: TSDUMemoryStream);
var
  createDate:      Word;
  createTime:      Word;
  createTimeFine:  Byte;
  lastAccessDate:  Word;
  lastModDate:     Word;
  lastModTime:     Word;
  lastModTimeFine: Byte;
  useFirstCluster: DWORD;
  useSize:         DWORD;
begin
  TTimeStampToWORD(item.TimestampCreation, createDate, createTime, createTimeFine);
  lastAccessDate := TDateToWORD(item.DatestampLastAccess);
  TTimeStampToWORD(item.TimestampLastModified, lastModDate, lastModTime, lastModTimeFine);

  useFirstCluster := item.FirstCluster;
  if ((item.Size = 0) and not (item.IsDirectory)  // Directories have their size set to 0 anyway
    ) then begin
    useFirstCluster := 0;
  end;

  useSize := item.Size;
  if (item.IsDirectory or item.IsVolumeLabel) then begin
    useSize := 0;
  end;

  // Write out the 8.3 entry...
  stream.WriteString(DOSFilenameTo11Chars(item.FilenameDOS));
  stream.WriteByte(item.Attributes);
  stream.WriteByte($00);                // Reserved
  stream.WriteByte(createTimeFine);     // Create time (fine)
  stream.WriteWORD_LE(createTime);      // Create time
  stream.WriteWORD_LE(createDate);      // Create date
  stream.WriteWORD_LE(lastAccessDate);  // Last access date
  stream.WriteWORD_LE((useFirstCluster and $FFFF0000) shr 16);
  stream.WriteWORD_LE(lastModTime);     // Last modified time
  stream.WriteWORD_LE(lastModDate);     // Last modified date
  stream.WriteWORD_LE(useFirstCluster and $0000FFFF);
  stream.WriteDWORD_LE(useSize);        // File size; should be 0 for directories and volume labels
end;

 // Seek the first instance of cntNeeded directory entries in dirData which are
 // unused
 // Note: This starts searching from the start of dirData
function TSDFilesystem_FAT.SeekBlockUnusedDirEntries(cntNeeded: Integer;
  dirData: TSDUMemoryStream): Boolean;
var
  Result:          Boolean;
  currRunLength:   Integer;
  runStartOffset:  Int64;
  currEntryOffset: Int64;
  filenameChar:    Byte;
begin
  Result := False;

  currRunLength   := 0;
  runStartOffset  := 0;
  currEntryOffset := runStartOffset;
  while (currEntryOffset < dirData.Size) do begin
    dirData.Position := currEntryOffset + DIR_ENTRY_OFFSET_DOSFILENAME;
    filenameChar     := dirData.ReadByte();
    if ((filenameChar = DIR_ENTRY_UNUSED) or (filenameChar = DIR_ENTRY_DELETED)) then begin
      Inc(currRunLength);
      if (currRunLength >= cntNeeded) then begin
        dirData.Position := runStartOffset;
        Result           := True;
        break;
      end;
    end else begin
      currRunLength  := 0;
      runStartOffset := currEntryOffset + DIR_ENTRY_SIZE;
    end;

    currEntryOffset := currEntryOffset + DIR_ENTRY_SIZE;
  end;


end;


 // Seek an 8.3 DOS filename in raw directory data, setting dirData.Position to
 // the start of the dir entry
 // Note: This starts searching from the start
function TSDFilesystem_FAT.Seek83FileDirNameInDirData(filename: Ansistring;
  dirData: TSDUMemoryStream): Boolean;
var
  filename11Char: String;
  currFilename:   String;
  currAttributes: Byte;
  Result:         Boolean;
  recordOffset:   Int64;
begin
  Result := False;

  filename11Char := DOSFilenameTo11Chars(filename);

  recordOffset := 0;
  while (recordOffset < dirData.Size) do begin
    dirData.Position := recordOffset;

    currFilename := dirData.ReadString(11, (recordOffset + DIR_ENTRY_OFFSET_DOSFILENAME));
    if (currFilename = filename11Char) then begin
      currAttributes := dirData.ReadByte(recordOffset + DIR_ENTRY_OFFSET_FILEATTRS);
      // Only interested in dirs and files
      if (((currAttributes and VFAT_ATTRIB_VFAT_ENTRY) <> VFAT_ATTRIB_VFAT_ENTRY) and
        ((currAttributes and VFAT_ATTRIB_FLAG_VOLLABEL) <> VFAT_ATTRIB_FLAG_VOLLABEL) and
        ((currAttributes and VFAT_ATTRIB_FLAG_DEVICE) <> VFAT_ATTRIB_FLAG_DEVICE)) then begin
        // Reset position to start of dir entry
        dirData.Position := recordOffset;
        Result           := True;
        break;
      end;
    end;

    recordOffset := recordOffset + DIR_ENTRY_SIZE;
  end;


end;

// Extend the specified stream by a cluster filled with zeros
procedure TSDFilesystem_FAT.ExtendByEmptyCluster(stream: TSDUMemoryStream);
var
  buffer: Pointer;
begin
  buffer := AllocMem(ClusterSize());
  // AllocMem initializes the memory allocated to zeros
  try
    stream.Position := stream.Size;
    stream.Write(buffer^, ClusterSize());
  finally
    FreeMem(buffer);
  end;

end;

// Returns TRUE/FALSE, depending on whether clusterID appers in chain or not
function TSDFilesystem_FAT.IsClusterInChain(clusterID: DWORD; chain: TSDFATClusterChain): Boolean;
var
  Result: Boolean;
  i:      Integer;
begin
  Result := False;

  for i := low(chain) to high(chain) do begin
    if (chain[i] = clusterID) then begin
      Result := True;
      break;
    end;
  end;


end;

// Add the specified cluster to the given chain
procedure TSDFilesystem_FAT.AddClusterToChain(clusterID: DWORD; var chain: TSDFATClusterChain);
begin
  SetLength(chain, (length(chain) + 1));
  chain[(length(chain) - 1)] := clusterID;
end;

 // Note: This function will update item.FirstCluster to reflect the updated
 //       cluster ID
 // Note: This function will update item.FilenameDOS to a unique filename within
 //       the dir to be stored in, if it's set to '' on entry
 // If parentDir is not nil on entry, it will have the dirToStoreIn's details
 // *assigned* to it
function TSDFilesystem_FAT.StoreFileOrDir(dirToStoreIn: WideString;
  // The dir in which item/data is to be stored
  item: TSDDirItem_FAT; data: TStream; parentDir: TSDDirItem_FAT = nil): Boolean;
var
  allOK:             Boolean;
  dirToStoreInItem:  TSDDirItem_FAT;
  dirToStoreInChain: TSDFATClusterChain;
  dirToStoreInData:  TSDUMemoryStream;
  existingItem:      TSDDirItem_FAT;
  itemChain:         TSDFATClusterChain;
  itemChainUnused:   TSDFATClusterChain;
  useFirstCluster:   DWORD;
  clusterReserved:   DWORD;
  useDOSFilename:    String;
  datetimetamp:      TDateTime;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  existingItem     := nil;
  dirToStoreInItem := nil;
  dirToStoreInData := nil;
  useFirstCluster  := 0;
  clusterReserved  := ERROR_DWORD;
  try
    if allOK then begin
      existingItem := TSDDirItem_FAT.Create();
      if not (GetItem_FAT(IncludeTrailingPathDelimiter(dirToStoreIn) +
        item.Filename, existingItem)) then begin
        existingItem.Free();
        existingItem := nil;
      end;
    end;

    if allOK then begin
      useDOSFilename := item.FilenameDOS;
      if (existingItem <> nil) then begin
        useDOSFilename := existingItem.FilenameDOS;
      end;
      if (useDOSFilename = '') then begin
        useDOSFilename := GenerateNew83Filename(IncludeTrailingPathDelimiter(
          dirToStoreIn) + item.Filename);
        allOK          := (useDOSFilename <> '');
      end;
    end;

    // Sanity check - Can't overwrite file with dir and vice versa
    if allOK then begin
      if (existingItem <> nil) then begin
        allOK := ((item.IsFile = existingItem.IsFile) and
          (item.IsDirectory = existingItem.IsDirectory));
      end;
    end;

    // -------------------------
    // Retrieve details of directory item is to be stored in

    if allOK then begin
      dirToStoreInItem := TSDDirItem_FAT.Create();
      allOK            := GetItem_FAT(dirToStoreIn, dirToStoreInItem);
      if (allOK and (parentDir <> nil)) then begin
        parentDir.Assign(dirToStoreInItem);
      end;
    end;

    if allOK then begin
      dirToStoreInChain := ExtractClusterChain(dirToStoreInItem.FirstCluster);
      allOK             := (length(dirToStoreInChain) > 0);
    end;

    if allOK then begin
      dirToStoreInData := TSDUMemoryStream.Create();
      allOK            := ReadWriteClusterChainData(True, dirToStoreInChain, dirToStoreInData);
    end;


    // -------------------------
    // Reserve a single cluster, in case the directory needs to be extended
    // Note: This, in conjunction with the AllocateChainForData(...) call a bit
    //       later, ensures that there is enough storage space left to store
    //       the item
    // IMPORTANT NOTE: This is done *before* the call to
    //                 AllocateChainForData(...) as AllocateChainForData(...)
    //                 doesn't reserve the clusters it allocates. This has the
    //                 effect that this will fragment the filesystem slightly
    //                 when the next file is written - fix in later release
    if allOK then begin
      clusterReserved := ReserveFATEntry();
      allOK           := (clusterReserved <> ERROR_DWORD);
    end;


    // -------------------------
    // Retrieve chain for any item, if item already exists on filesystem

    if allOK then begin
      SetLength(itemChain, 0);
      if (existingItem <> nil) then begin
        if (existingItem.FirstCluster <> 0) then begin
          itemChain := ExtractClusterChain(existingItem.FirstCluster);
        end;
      end;
    end;

    // Extend/truncate item chain as needed
    // Note: This, in conjunction with the ReserveFATEntry(...) call a bit
    //       earlier, ensures that there is enough storage space left to store
    //       the item
    if allOK then begin
      data.Position   := 0;
      allOK           := AllocateChainForData(itemChain, itemChainUnused, data, item.Size);
      useFirstCluster := 0;
      if (length(itemChain) > 0) then begin
        useFirstCluster := itemChain[0];
      end;
    end;


    // -------------------------
    // Setup new item...

    if allOK then begin
      item.FilenameDOS  := useDOSFilename;
      item.FirstCluster := useFirstCluster;
    end;


    // -------------------------
    // Delete any existing dir entry, and store our new one

    if allOK then begin
      if (existingItem <> nil) then begin
        allOK := DeleteEntryFromDir(existingItem.FilenameDOS, dirToStoreInData);
      end;
    end;

    if allOK then begin
      if not (PreserveTimeDateStamps) then begin
        // Assign to local variable so there's no risk of separate calls
        // returning slightly different timestamps
        datetimetamp := Now();

        item.TimestampCreation     := DateTimeToTimeStamp(datetimetamp);
        item.TimestampLastModified := DateTimeToTimeStamp(datetimetamp);
        item.DatestampLastAccess   := datetimetamp;
      end;

      // Notice: This will set clusterReserved to ERROR_DWORD if it's needed
      //         (i.e. used)
      allOK := AddEntryToDir(item, dirToStoreInChain, dirToStoreInData,
        clusterReserved, (dirToStoreIn = PATH_SEPARATOR));
    end;



    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // At this point, nothing has been written out to the partition, nor has
    // anything written to the in-memory FAT copy
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    // Update FAT entries for item's and dir's data
    // Destination items's cluster chain...
    if allOK then begin
      allOK := StoreClusterChain(itemChain);
    end;
    // Any previously used part of the cluster chain which is no longer 
    // used (e.g. when truncating a file)
    if allOK then begin
      allOK := FreeClusterChain(itemChainUnused);
    end;
    // Cluster chain for the directory the item is stored to...
    if allOK then begin
      allOK := StoreClusterChain(dirToStoreInChain);
    end;
    // If reserved cluster not used, mark back as free
    // Note: This one is *intentionally* not protected by "if allOK then"
    // Note: This is intentionally here, as well as the same calls in the
    //       "try...finally...end" part
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      clusterReserved := ERROR_DWORD;
    end;
    if allOK then begin
      allOK := WriteFATToAllCopies();
    end;

    // Write updated dir contents
    if allOK then begin
      dirToStoreInData.Position := 0;
      allOK                     := StoreClusterChainData(dirToStoreInChain, dirToStoreInData);
    end;

    // Write file contents
    if allOK then begin
      data.Position := 0;
      allOK         := StoreClusterChainData(itemChain, data, item.Size);
    end;

  finally
    if (existingItem <> nil) then begin
      existingItem.Free();
    end;

    if (dirToStoreInData <> nil) then begin
      dirToStoreInData.Free();
    end;

    // This is included here as a sanity check in case of exception
    // This *should* never actually do anything, but it does make sure the
    // reserved cluster is free'd if it wasn't used in the in-memory FAT copy
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      // Next line commented out to prevent compiler hint      
      //      clusterReserved := ERROR_DWORD;
    end;

  end;

  Result := allOK;
end;

 // This function moves a file/directory from one location to another.
 // Can also be used as a "rename" function
 // Operates by creating a new directory item "destItemPath", setting it to point
 // to the same first cluster as "srcItemPath"'s dir item does, then deleting
 // "srcItemPath"'s directory entry
 // Note: "destItemPath" must be the full path and filename of the file/dir to
 //       be moved
function TSDFilesystem_FAT.MoveFileOrDir(srcItemPath: WideString;
                            // The path and filename of the file/dir to be moved
  destItemPath: WideString  // The new path and filename
  ): Boolean;
var
  allOK:                    Boolean;
  srcDirItemStoredInItem:   TSDDirItem_FAT;
  destDirItemStoredInItem:  TSDDirItem_FAT;
  srcDirItemStoredInChain:  TSDFATClusterChain;
  destDirItemStoredInChain: TSDFATClusterChain;
  srcDirItemStoredInData:   TSDUMemoryStream;
  destDirItemStoredInData:  TSDUMemoryStream;
  srcItem:                  TSDDirItem_FAT;
  testItem:                 TSDDirItem_FAT;
  clusterReserved:          DWORD;
  destDOSFilename:          String;
  sameDirFlag:              Boolean;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  sameDirFlag := ExtractFilePath(srcItemPath) = ExtractFilePath(destItemPath);

  srcItem                 := nil;
  srcDirItemStoredInItem  := nil;
  destDirItemStoredInItem := nil;
  srcDirItemStoredInData  := nil;
  destDirItemStoredInData := nil;
  clusterReserved         := ERROR_DWORD;
  try
    // Sanity check; destination doesn't already exist
    if allOK then begin
      testItem := TSDDirItem_FAT.Create();
      try
        if GetItem_FAT(destItemPath, testItem) then begin
          allOK := False;
        end;
      finally
        testItem.Free();
      end;
    end;

    // Get details of source file/directory
    if allOK then begin
      srcItem := TSDDirItem_FAT.Create();
      if not (GetItem_FAT(srcItemPath, srcItem)) then begin
        allOK := False;
      end;
    end;

    // Generate a dest 8.3 DOS filename, so it doesn't overwrite anything in the
    // destination dir
    if allOK then begin
      destDOSFilename := GenerateNew83Filename(destItemPath);
      allOK           := (destDOSFilename <> '');
    end;


    // -------------------------
    // Retrieve details of directory item is to be stored in (src location)

    if allOK then begin
      srcDirItemStoredInItem := TSDDirItem_FAT.Create();
      allOK                  := GetItem_FAT(ExtractFilePath(srcItemPath), srcDirItemStoredInItem);
    end;

    if allOK then begin
      srcDirItemStoredInChain := ExtractClusterChain(srcDirItemStoredInItem.FirstCluster);
      allOK                   := (length(srcDirItemStoredInChain) > 0);
    end;

    if allOK then begin
      srcDirItemStoredInData := TSDUMemoryStream.Create();
      allOK                  := ReadWriteClusterChainData(True, srcDirItemStoredInChain,
        srcDirItemStoredInData);
    end;


    // -------------------------
    // Retrieve details of directory item is to be stored in (dest location)

    if not (sameDirFlag) then begin
      if allOK then begin
        destDirItemStoredInItem := TSDDirItem_FAT.Create();
        allOK                   :=
          GetItem_FAT(ExtractFilePath(destItemPath), destDirItemStoredInItem);
      end;

      if allOK then begin
        destDirItemStoredInChain := ExtractClusterChain(destDirItemStoredInItem.FirstCluster);
        allOK                    := (length(destDirItemStoredInChain) > 0);
      end;

      if allOK then begin
        destDirItemStoredInData := TSDUMemoryStream.Create();
        allOK                   :=
          ReadWriteClusterChainData(True, destDirItemStoredInChain, destDirItemStoredInData);
      end;
    end;


    // -------------------------
    // Reserve a single cluster, in case the directory needs to be extended
    // Note: This, in conjunction with the AllocateChainForData(...) call a bit
    //       later, ensures that there is enough storage space left to store
    //       the item
    // IMPORTANT NOTE: This is done *before* the call to
    //                 AllocateChainForData(...) as AllocateChainForData(...)
    //                 doesn't reserve the clusters it allocates. This has the
    //                 effect that this will fragment the filesystem slightly
    //                 when the next file is written - fix in later release
    if allOK then begin
      clusterReserved := ReserveFATEntry();
      allOK           := (clusterReserved <> ERROR_DWORD);
    end;


    // -------------------------
    // Delete entry from source dir, create dest entry in target dir

    // Note that we're still working with in-memory copies here, so it's just
    // as safe to delete/add as to add/delete. But! By deleting first, we
    // (potentially) can reuse the same directory records in the dir structure
    //  - which could make a difference between successfully renaming an item
    // in the root dir of a FAT12/FAT16 volume - which has limited root dir
    // entries available

    if allOK then begin
      allOK := DeleteEntryFromDir(srcItem.FilenameDOS, srcDirItemStoredInData);
    end;

    // Change filename, so we can reuse the same structure
    if allOK then begin
      // Note that srcItem.FirstCluster is left alone here
      srcItem.Filename    := ExtractFilename(destItemPath);
      srcItem.FilenameDOS := destDOSFilename;
    end;

    if allOK then begin
      // If in the same dir, we only work with srcDir
      if sameDirFlag then begin
        // Notice: This will set clusterReserved to ERROR_DWORD if it's needed
        //         (i.e. used)
        allOK := AddEntryToDir(srcItem, srcDirItemStoredInChain,
          srcDirItemStoredInData, clusterReserved,
          (ExtractFilePath(destItemPath) = PATH_SEPARATOR));
      end else begin
        // Notice: This will set clusterReserved to ERROR_DWORD if it's needed
        //         (i.e. used)
        allOK := AddEntryToDir(srcItem, destDirItemStoredInChain,
          destDirItemStoredInData, clusterReserved,
          (ExtractFilePath(destItemPath) = PATH_SEPARATOR));
      end;
    end;



    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // At this point, nothing has been written out to the partition, nor has
    // anything written to the in-memory FAT copy
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    // Update FAT entries for item's and dir's data
    // Cluster chain for the directory the item is moved to...
    if allOK then begin
      // If in the same dir, we only work with srcDir
      if not (sameDirFlag) then begin
        allOK := StoreClusterChain(destDirItemStoredInChain);
      end;
    end;
    // Cluster chain for the directory the item is moved from...
    if allOK then begin
      allOK := StoreClusterChain(srcDirItemStoredInChain);
    end;
    // If reserved cluster not used, mark back as free
    // Note: This one is *intentionally* not protected by "if allOK then"
    // Note: This is intentionally here, as well as the same calls in the
    //       "try...finally...end" part
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      clusterReserved := ERROR_DWORD;
    end;
    if allOK then begin
      allOK := WriteFATToAllCopies();
    end;

    // Write updated dir contents
    if allOK then begin
      // If in the same dir, we only work with srcDir
      if not (sameDirFlag) then begin
        destDirItemStoredInData.Position := 0;
        allOK                            :=
          StoreClusterChainData(destDirItemStoredInChain, destDirItemStoredInData);
      end;
    end;
    if allOK then begin
      srcDirItemStoredInData.Position := 0;
      allOK                           :=
        StoreClusterChainData(srcDirItemStoredInChain, srcDirItemStoredInData);
    end;

  finally
    if (srcItem <> nil) then begin
      srcItem.Free();
    end;

    if (srcDirItemStoredInData <> nil) then begin
      srcDirItemStoredInData.Free();
    end;

    if (destDirItemStoredInData <> nil) then begin
      destDirItemStoredInData.Free();
    end;

    // This is included here as a sanity check in case of exception
    // This *should* never actually do anything, but it does make sure the
    // reserved cluster is free'd if it wasn't used in the in-memory FAT copy
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      // Next line commented out to prevent compiler hint      
      //      clusterReserved := ERROR_DWORD;
    end;

  end;

  Result := allOK;
end;


 // Copy file from one location to another
 // Note: "destItemPath" must be the full path and filename of the file to be
 //       created
function TSDFilesystem_FAT.CopyFile(srcItemPath: WideString;
                            // The path and filename of the file to be copied
  destItemPath: WideString  // The path and filename of the copy
  ): Boolean;
var
  allOK:                    Boolean;
  destDirItemStoredInItem:  TSDDirItem_FAT;
  destDirItemStoredInChain: TSDFATClusterChain;
  destDirItemStoredInData:  TSDUMemoryStream;
  srcItem:                  TSDDirItem_FAT;
  srcChain:                 TSDFATClusterChain;
  testItem:                 TSDDirItem_FAT;
  destChain:                TSDFATClusterChain;
  destChainUnused:          TSDFATClusterChain;
  useFirstCluster:          DWORD;
  clusterReserved:          DWORD;
  destDOSFilename:          String;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  srcItem                 := nil;
  destDirItemStoredInItem := nil;
  destDirItemStoredInData := nil;
  useFirstCluster         := 0;
  clusterReserved         := ERROR_DWORD;
  try
    // Sanity check; destination doesn't already exist
    if allOK then begin
      testItem := TSDDirItem_FAT.Create();
      try
        if GetItem_FAT(destItemPath, testItem) then begin
          allOK := False;
        end;
      finally
        testItem.Free();
      end;
    end;

    // Get details of source file/directory
    if allOK then begin
      srcItem := TSDDirItem_FAT.Create();
      if not (GetItem_FAT(srcItemPath, srcItem)) then begin
        allOK := False;
      end;
    end;

    // Sanity check; source isn't a directory (this function only handles
    // copying files)
    if allOK then begin
      allOK := not (srcItem.IsDirectory);
    end;

    // Generate a new 8.3 DOS filename, so it doesn't overwrite anything in the
    // destination dir
    if allOK then begin
      destDOSFilename := GenerateNew83Filename(destItemPath);
      allOK           := (destDOSFilename <> '');
    end;


    // -------------------------
    // Retrieve details of directory item is to be stored in (new location)

    if allOK then begin
      destDirItemStoredInItem := TSDDirItem_FAT.Create();
      allOK                   := GetItem_FAT(ExtractFilePath(destItemPath),
        destDirItemStoredInItem);
    end;

    if allOK then begin
      destDirItemStoredInChain := ExtractClusterChain(destDirItemStoredInItem.FirstCluster);
      allOK                    := (length(destDirItemStoredInChain) > 0);
    end;

    if allOK then begin
      destDirItemStoredInData := TSDUMemoryStream.Create();
      allOK                   := ReadWriteClusterChainData(True, destDirItemStoredInChain,
        destDirItemStoredInData);
    end;



    // -------------------------
    // Reserve a single cluster, in case the directory needs to be extended
    // Note: This, in conjunction with the AllocateChainForData(...) call a bit
    //       later, ensures that there is enough storage space left to store
    //       the item
    // IMPORTANT NOTE: This is done *before* the call to
    //                 AllocateChainForData(...) as AllocateChainForData(...)
    //                 doesn't reserve the clusters it allocates. This has the
    //                 effect that this will fragment the filesystem slightly
    //                 when the next file is written - fix in later release
    if allOK then begin
      clusterReserved := ReserveFATEntry();
      allOK           := (clusterReserved <> ERROR_DWORD);
    end;


    // -------------------------
    // Retrieve chain for the item to be copied

    if allOK then begin
      SetLength(srcChain, 0);
      if (srcItem.FirstCluster <> 0) then begin
        srcChain := ExtractClusterChain(srcItem.FirstCluster);
      end;
    end;

    // Create dest chain
    // Note: This, in conjunction with the ReserveFATEntry(...) call a bit
    //       earlier, ensures that there is enough storage space left to store
    //       the item
    if allOK then begin
      SetLength(destChain, 0);
      allOK           := AllocateChainForData(destChain, destChainUnused, nil, srcItem.Size);
      useFirstCluster := 0;
      if (length(destChain) > 0) then begin
        useFirstCluster := destChain[0];
      end;
    end;


    // -------------------------
    // Setup dest item...

    if allOK then begin
      // Note: Only the new filename, DOS filename (to ensure it's unique in
      //       the destination dir) and first cluster (to the copy) get changed
      srcItem.Filename     := ExtractFilename(destItemPath);
      srcItem.FilenameDOS  := destDOSFilename;
      srcItem.FirstCluster := useFirstCluster;
    end;


    // -------------------------
    // Store our dest dir entry

    if allOK then begin
      // Notice: This will set clusterReserved to ERROR_DWORD if it's needed
      //         (i.e. used)
      allOK := AddEntryToDir(srcItem, destDirItemStoredInChain,
        destDirItemStoredInData, clusterReserved,
        (ExtractFilePath(destItemPath) = PATH_SEPARATOR));
    end;



    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // At this point, nothing has been written out to the partition, nor has
    // anything written to the in-memory FAT copy
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    // Update FAT entries for item's and dir's data
    // Destination items's cluster chain...
    if allOK then begin
      allOK := StoreClusterChain(destChain);
    end;
    // Cluster chain for the directory the item is copied to...
    if allOK then begin
      allOK := StoreClusterChain(destDirItemStoredInChain);
    end;
    // If reserved cluster not used, mark back as free
    // Note: This one is *intentionally* not protected by "if allOK then"
    // Note: This is intentionally here, as well as the same calls in the
    //       "try...finally...end" part
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      clusterReserved := ERROR_DWORD;
    end;
    if allOK then begin
      allOK := WriteFATToAllCopies();
    end;

    // Write updated dir contents
    if allOK then begin
      destDirItemStoredInData.Position := 0;
      allOK                            :=
        StoreClusterChainData(destDirItemStoredInChain, destDirItemStoredInData);
    end;

    // Write file contents
    if allOK then begin
      allOK := CopyClusterChainData(srcChain, destChain);
    end;

  finally
    if (srcItem <> nil) then begin
      srcItem.Free();
    end;

    if (destDirItemStoredInData <> nil) then begin
      destDirItemStoredInData.Free();
    end;

    // This is included here as a sanity check in case of exception
    // This *should* never actually do anything, but it does make sure the
    // reserved cluster is free'd if it wasn't used in the in-memory FAT copy
    if (clusterReserved <> ERROR_DWORD) then begin
      UnreserveFATEntry(clusterReserved);
      // Next line commented out to prevent compiler hint      
      //      clusterReserved := ERROR_DWORD;
    end;

  end;

  Result := allOK;
end;


function TSDFilesystem_FAT.CreateDir(dirToStoreIn: WideString;
  // The dir in which item/data is to be stored
  newDirname: WideString; templateDirAttrs: TSDDirItem_FAT = nil): Boolean;
var
  newDirContent:   TSDUMemoryStream;
  parentDirItem:   TSDDirItem_FAT;
  newDirItem:      TSDDirItem_FAT;
  tmpDirItem:      TSDDirItem_FAT;
  existingDirItem: TSDDirItem_FAT;
  allOK:           Boolean;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  parentDirItem   := TSDDirItem_FAT.Create();
  newDirItem      := TSDDirItem_FAT.Create();
  newDirContent   := TSDUMemoryStream.Create();
  tmpDirItem      := TSDDirItem_FAT.Create();
  existingDirItem := TSDDirItem_FAT.Create();
  try
    if allOK then begin
      ExtendByEmptyCluster(newDirContent);
    end;

    // Sanity check; ensure dir doesn't already exist
    if allOK then begin
      allOK := not (GetItem_FAT((IncludeTrailingPathDelimiter(dirToStoreIn) + newDirname),
        existingDirItem));
    end;

    if allOK then begin
      if (templateDirAttrs <> nil) then begin
        newDirItem.Assign(templateDirAttrs);
      end else begin
        newDirItem.TimestampCreation     := DateTimeToTimeStamp(now);
        newDirItem.DatestampLastAccess   := Now;
        newDirItem.TimestampLastModified := DateTimeToTimeStamp(now);
      end;

      newDirItem.Filename    := newDirname;
      newDirItem.FilenameDOS := GenerateNew83Filename(
        IncludeTrailingPathDelimiter(dirToStoreIn) + newDirname);

      // Note: This will be reset to 0 before writing the dir entry
      newDirItem.Size := newDirContent.Size;

      newDirItem.IsDirectory := True;
    end;

    if allOK then begin
      // Store the (completely blank) directory placeholder content
      // Note: This will update newDirItem.FirstCluster as appropriate
      newDirContent.Position := 0;
      StoreFileOrDir(dirToStoreIn, newDirItem, newDirContent, parentDirItem);
    end;

    if allOK then begin
      // Update the directory contents, adding "." and ".." entries
      tmpDirItem.Assign(newDirItem);
      tmpDirItem.Filename    := DIR_CURRENT_DIR;
      tmpDirItem.FilenameDOS := DIR_CURRENT_DIR;
      tmpDirItem.Size        := 0;
      newDirContent.Position := 0;
      WriteDirEntry_83(tmpDirItem, newdirContent);

      tmpDirItem.Assign(parentDirItem);
      tmpDirItem.Filename    := DIR_PARENT_DIR;
      tmpDirItem.FilenameDOS := DIR_PARENT_DIR;
      tmpDirItem.Size        := 0;
      newDirContent.Position := DIR_ENTRY_SIZE;
      WriteDirEntry_83(tmpDirItem, newdirContent);
    end;

    if allOK then begin
      newDirContent.Position := 0;
      allOK                  := StoreClusterData(newDirItem.FirstCluster, newDirContent);
    end;

  finally
    existingDirItem.Free();
    newDirContent.Free();
    newDirItem.Free();
    parentDirItem.Free();
  end;

  Result := allOK;
end;


 // Note: This function will update item.FirstCluster to reflect the updated
 //       cluster ID
 // If parentDir is not nil, it will have the dirToStoreIn's details
 // *assigned* to it
function TSDFilesystem_FAT.DeleteItem(fullPathToItem: WideString): Boolean;
var
  allOK:            Boolean;
  dirStoredInPath:  WideString;
  dirStoredInItem:  TSDDirItem_FAT;
  dirStoredInChain: TSDFATClusterChain;
  dirStoredInData:  TSDUMemoryStream;
  itemToDelete:     TSDDirItem_FAT;
begin
  allOK := True;

  dirStoredInPath := PathParent(fullPathToItem);

  itemToDelete    := TSDDirItem_FAT.Create();
  dirStoredInItem := TSDDirItem_FAT.Create();
  dirStoredInData := TSDUMemoryStream.Create();
  try
    if allOK then begin
      allOK := GetItem_FAT(dirStoredInPath, dirStoredInItem);
    end;

    if allOK then begin
      allOK := GetItem_FAT(fullPathToItem, itemToDelete);
    end;

    if allOK then begin
      dirStoredInChain := ExtractClusterChain(dirStoredInItem.FirstCluster);
      allOK            := (length(dirStoredInChain) > 0);
    end;

    if allOK then begin
      allOK := ExtractClusterChainData(dirStoredInChain, dirStoredInData);
    end;

    if allOK then begin
      allOK := DeleteEntryFromDir(itemToDelete.FilenameDOS, dirStoredInData);
    end;

    if allOK then begin
      dirStoredInData.Position := 0;
      allOK                    := StoreClusterChainData(dirStoredInChain, dirStoredInData);
    end;

    if allOK then begin
      // Mark clusters as free in FAT
      allOK := FreeClusterChain(itemToDelete.FirstCluster);
    end;

    if allOK then begin
      // Write out updated FAT
      allOK := WriteFATToAllCopies();
    end;

  finally
    dirStoredInData.Free();
    dirStoredInItem.Free();
    itemToDelete.Free();
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.DeleteFile(fullPathToItem: WideString): Boolean;
var
  itemToDelete: TSDDirItem_FAT;
  allOK:        Boolean;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  itemToDelete := TSDDirItem_FAT.Create();
  try
    if allOK then begin
      allOK := GetItem_FAT(fullPathToItem, itemToDelete);
    end;

    // Sanity check; it *is* a file, right?
    if allOK then begin
      allOK := itemToDelete.IsFile;
    end;

    if allOK then begin
      allOK := DeleteItem(fullPathToItem);
    end;

  finally
    itemToDelete.Free();
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.DeleteDir(fullPathToItem: WideString): Boolean;
var
  subItems:     TSDDirItemList;
  itemToDelete: TSDDirItem_FAT;
  allOK:        Boolean;
  i:            Integer;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  itemToDelete := TSDDirItem_FAT.Create();
  subItems     := TSDDirItemList.Create();
  try
    if allOK then begin
      allOK := GetItem_FAT(fullPathToItem, itemToDelete);
    end;

    // Sanity check; it *is* a dir, right?
    if allOK then begin
      allOK := itemToDelete.IsDirectory;
    end;

    // Sanity check - skip stupid
    if allOK then begin
      allOK := ((itemToDelete.Filename <> DIR_CURRENT_DIR) and
        (itemToDelete.Filename <> DIR_PARENT_DIR));
    end;

    // Get dir contents
    if allOK then begin
      allOK := LoadContentsFromDisk(fullPathToItem, subItems);
    end;

    // Delete everything beneath the dir to be deleted
    if allOK then begin
      for i := 0 to (subItems.Count - 1) do begin
        if not (DeleteFileOrDir(IncludeTrailingPathDelimiter(fullPathToItem) +
          subItems[i].Filename)) then begin
          allOK := False;
          break;
        end;
      end;
    end;

    // Delete dir
    if allOK then begin
      allOK := DeleteItem(fullPathToItem);
    end;

  finally
    subItems.Free();
    itemToDelete.Free();
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.DeleteFileOrDir(fullPathToItem: WideString): Boolean;
var
  itemToDelete: TSDDirItem_FAT;
  subItems:     TSDDirItemList;
  allOK:        Boolean;
begin
  allOK := True;

  if not (CheckWritable()) then begin
    Result := False;
    exit;
  end;

  itemToDelete := TSDDirItem_FAT.Create();
  subItems     := TSDDirItemList.Create();
  try
    if allOK then begin
      allOK := GetItem_FAT(fullPathToItem, itemToDelete);
    end;

    // Sanity check; it *is* a file, right?
    if allOK then begin
      if itemToDelete.IsFile then begin
        allOK := DeleteFile(fullPathToItem);
      end else
      if (itemToDelete.IsDirectory and (itemToDelete.Filename <> DIR_CURRENT_DIR) and
        (itemToDelete.Filename <> DIR_PARENT_DIR)) then begin
        allOK := DeleteDir(fullPathToItem);
      end;
    end;

  finally
    subItems.Free();
    itemToDelete.Free();
  end;

  Result := allOK;
end;

 // Returns a new, unique, 8.3 DOS filename (without the path) that can be used
 // as for the DOS filename for the LFN supplied
function TSDFilesystem_FAT.GenerateNew83Filename(lfnFilename: WideString): String;
var
  dirToStoreIn:     WideString;
  dirToStoreInItem: TSDDirItem_FAT;
  allOK:            Boolean;
  dirToStoreInData: TSDUMemoryStream;
  x:                Integer;
  Result:           String;
  uniqueFound:      Boolean;
begin
  allOK := True;

  dirToStoreIn := PathParent(lfnFilename);

  dirToStoreInItem := TSDDirItem_FAT.Create();
  dirToStoreInData := TSDUMemoryStream.Create();
  try
    if allOK then begin
      allOK := GetItem_FAT(dirToStoreIn, dirToStoreInItem);
    end;

    // Sanity check; it *is* a dir, right?
    if allOK then begin
      allOK := dirToStoreInItem.IsDirectory;
    end;

    if allOK then begin
      allOK := ExtractClusterChainData(dirToStoreInItem.FirstCluster, dirToStoreInData);
    end;

    if allOK then begin
      x           := 1;
      uniqueFound := False;
      while not (uniqueFound) do begin
        Result := IntToStr(x);

        // Blowout if we couldn't find one. If we haven't found one by the time
        // this kicks out, we've checked 99999999 different DOS filenames - all
        // of which are in use. User needs to be tought how to organise their
        // data better.
        if (length(Result) > DIR_ENTRY_LENGTH_DOSFILENAME) then begin
          Result := '';
          break;
        end;

        uniqueFound := not (Seek83FileDirNameInDirData(Result, dirToStoreInData));
        Inc(x);
      end;
    end;

  finally
    dirToStoreInData.Free();
    dirToStoreInItem.Free();
  end;

  if not (allOK) then begin
    Result := '';
  end;


end;

function TSDFilesystem_FAT.Format(): Boolean;
var
  useFATType: TFATType;
  i:          Integer;
begin
  // Determine the best FAT filesystem to use, based on the size of the volume.
  useFATType := ftFAT16;
  for i := low(CLUSTERSIZE_BREAKDOWN) to high(CLUSTERSIZE_BREAKDOWN) do begin
    if (PartitionImage.Size <= CLUSTERSIZE_BREAKDOWN[i].MaxPartitionSize) then begin
      // Use FAT16, if OK...
      if (CLUSTERSIZE_BREAKDOWN[i].ClusterSize_FAT12FAT16 <> 0) then begin
        useFATType := ftFAT16;
      end;

      // ...Overriding with FAT32 if possible
      if (CLUSTERSIZE_BREAKDOWN[i].ClusterSize_FAT32 <> 0) then begin
        useFATType := ftFAT32;
      end;

      break;
    end;
  end;

  Result := _Format(useFATType);

end;

function TSDFilesystem_FAT._Format(fmtType: TFATType): Boolean;
const
  // We use "MSWIN4.1" as this is supposed to be the most common. *Some*
  // systems may check for this, even though they shouldn't
  DEFAULT_OEMNAME: Ansistring         = 'MSWIN4.1';
  //DEFAULT_OEMNAME: string = 'MSDOS5.0';
  // Various sensible defaults
  DEFAULT_FAT_COPIES                  = 2;
  DEFAULT_BYTES_PER_SECTOR            = 512;
  DEFAULT_MEDIA_DESCRIPTOR            = $F8; // Hard drive
  DEFAULT_RESERVED_SECTORS_FAT12FAT16 = 2;   // Should be 1??
  DEFAULT_RESERVED_SECTORS_FAT32      = 32;
  DEFAULT_MAX_ROOT_DIR_ENTRIES        = 512;  // Typical on FAT12/FAT16

  // Taken from: FreeOTFE.h; see this file for an explanation of this choice of values
  DEFAULT_TRACKS_PER_CYLINDER = 64;
  DEFAULT_SECTORS_PER_TRACK   = 32;

  DEFAULT_FAT_FLAGS                        = 0;
  DEFAULT_VERSION                          = 0;
  DEFAULT_SECTOR_NO_FS_INFO_SECTOR_FAT1216 = 0;
  DEFAULT_SECTOR_NO_FS_INFO_SECTOR_FAT32   = 1;
  DEFAULT_SECTOR_BOOT_SECTOR_COPY_FAT1216  = 0;
  DEFAULT_SECTOR_BOOT_SECTOR_COPY_FAT32    = 6;
  DEFAULT_PHYSICAL_DRIVE_NO                = 0;
  DEFAULT_EXTENDED_BOOT_SIG                = $29;
  DEFAULT_VOLUME_LABEL                     = '           ';

  FSINFO_SIG_LEAD          = $41615252;
  FSINFO_SIG_STRUCT        = $61417272;
  FSINFO_SIG_TRAIL         = $AA550000;
  FSINFO_UNKNOWN_FREECOUNT = $FFFFFFFF;
  FSINFO_UNKNOWN_NEXTFREE  = $FFFFFFFF;

  FSINFO_OFFSET_FSINFOSIG = 0;
  FSINFO_LENGTH_FSINFOSIG = 4;
  FSINFO_OFFSET_RESERVED1 = 4;
  FSINFO_LENGTH_RESERVED1 = 480;
  FSINFO_OFFSET_STRUCTSIG = 484;
  FSINFO_LENGTH_STRUCTSIG = 4;
  FSINFO_OFFSET_FREECOUNT = 488;
  FSINFO_LENGTH_FREECOUNT = 4;
  FSINFO_OFFSET_NEXTFREE  = 492;
  FSINFO_LENGTH_NEXTFREE  = 4;
  FSINFO_OFFSET_RESERVED2 = 496;
  FSINFO_LENGTH_RESERVED2 = 12;
  FSINFO_OFFSET_TRAILSIG  = 508;
  FSINFO_LENGTH_TRAILSIG  = 4;

var
  allOK:            Boolean;
  newBootSector:    TSDBootSector_FAT;
  i:                DWORD;
  //  j: DWORD;
  useClusterSize:   DWORD;
  FATStartSectorID: DWORD;
  stmFAT:           TSDUMemoryStream;
  prevMounted:      Boolean;
  newDirContent:    TSDUMemoryStream;
  tmpDirItem:       TSDDirItem_FAT;
  itemChain:        TSDFATClusterChain;
  itemChainUnused:  TSDFATClusterChain;
  FATEntry_0:       DWORD;
  FATEntry_1:       DWORD;
  RootDirSectors:   DWORD;
  TmpVal1:          DWORD;
  TmpVal2:          DWORD;
  FATSz:            DWORD;
  newFSInfo:        TSDUMemoryStream;
begin
  FSerializeCS.Acquire();
  try
    // =======================================================================
    // Part 1: Write boot sector

    // Boot sector information
    newBootSector.FATType := fmtType;

    // JMP instruction not used
    // The JMP instruction can only be one of:
    //   0xEB, 0x??, 0x90
    //   0xE9, 0x??, 0x??
    // The 0xEB (short jump) is more common, so we use that one
    newBootSector.JMP[1] := $EB;
    newBootSector.JMP[2] := $00;
    newBootSector.JMP[3] := $90;

    newBootSector.OEMName := DEFAULT_OEMNAME;

    newBootSector.BytesPerSector := DEFAULT_BYTES_PER_SECTOR;

    useClusterSize := 0;
    for i := low(CLUSTERSIZE_BREAKDOWN) to high(CLUSTERSIZE_BREAKDOWN) do begin
      if (PartitionImage.Size <= CLUSTERSIZE_BREAKDOWN[i].MaxPartitionSize) then begin
        if ((newBootSector.FATType = ftFAT12) or (newBootSector.FATType = ftFAT16)) then begin
          useClusterSize := CLUSTERSIZE_BREAKDOWN[i].ClusterSize_FAT12FAT16;
        end else begin
          useClusterSize := CLUSTERSIZE_BREAKDOWN[i].ClusterSize_FAT32;
        end;

        break;
      end;
    end;
    allOK := (useClusterSize <> 0);

    newBootSector.SectorsPerCluster := (useClusterSize div newBootSector.BytesPerSector);


    if ((newBootSector.FATType = ftFAT12) or (newBootSector.FATType = ftFAT16)) then begin
      newBootSector.ReservedSectorCount := DEFAULT_RESERVED_SECTORS_FAT12FAT16;
    end else begin
      newBootSector.ReservedSectorCount := DEFAULT_RESERVED_SECTORS_FAT32;
    end;

    newBootSector.FATCount := DEFAULT_FAT_COPIES;

    if ((newBootSector.FATType = ftFAT12) or (newBootSector.FATType = ftFAT16)) then begin
      // Note: This should be a multiple of the cluster size
      newBootSector.MaxRootEntries := DEFAULT_MAX_ROOT_DIR_ENTRIES;
    end else begin
      newBootSector.MaxRootEntries := 0;
    end;

    newBootSector.TotalSectors    := (PartitionImage.Size div newBootSector.BytesPerSector);
    newBootSector.MediaDescriptor := DEFAULT_MEDIA_DESCRIPTOR;

    // Only used for FAT12/FAT16
    newBootSector.SectorsPerFAT := 0;


    // From MS FAT spec:
    // Microsoft Extensible Firmware Initiative FAT32 File System Specification
    // FAT: General Overview of On-Disk Format
    // Version 1.03, December 6, 2000
    // http://www.microsoft.com/whdc/system/platform/firmware/fatgen.mspx       
    //
    // RootDirSectors = ((BPB_RootEntCnt * 32) + (BPB_BytsPerSec – 1)) / BPB_BytsPerSec;
    // TmpVal1 = DskSize – (BPB_ResvdSecCnt + RootDirSectors);
    // TmpVal2 = (256 * BPB_SecPerClus) + BPB_NumFATs;
    // If(FATType == FAT32)
    //     TmpVal2 = TmpVal2 / 2;
    // FATSz = (TMPVal1 + (TmpVal2 – 1)) / TmpVal2;
    // If(FATType == FAT32) {
    //     BPB_FATSz16 = 0;
    //     BPB_FATSz32 = FATSz;
    // } else {
    //     BPB_FATSz16 = LOWORD(FATSz);
    //     /* there is no BPB_FATSz32 in a FAT16 BPB */
    // }
    //
    //
    // However, this gives a fat size which is out by quite a bit?!!
    RootDirSectors := ((newBootSector.MaxRootEntries * 32) +
      (newBootSector.BytesPerSector - 1)) div newBootSector.BytesPerSector;
    TmpVal1        := newBootSector.TotalSectors -
      (newBootSector.ReservedSectorCount + RootDirSectors);
    TmpVal2        := (256 * newBootSector.SectorsPerCluster) + newBootSector.FATCount;
    if (newBootSector.FATType = ftFAT32) then begin
      TmpVal2 := TmpVal2 div 2;
    end;
    FATSz := (TMPVal1 + (TmpVal2 - 1)) div TmpVal2;
    if (newBootSector.FATType = ftFAT32) then begin
      newBootSector.SectorsPerFAT := FATSz;
    end else begin
      newBootSector.SectorsPerFAT := (FATSz and $FFFF);
      // there is no BPB_FATSz32 in a FAT16 BPB
    end;

    newBootSector.SectorsPerTrack := DEFAULT_SECTORS_PER_TRACK;
    newBootSector.NumberOfHeads   := DEFAULT_TRACKS_PER_CYLINDER;
    // See FreeOTFE.c for explanation of the value used for ".HiddenSectors"
    // (search for comment "An extra track for hidden sectors")
    newBootSector.HiddenSectors   := newBootSector.SectorsPerTrack;

    case newBootSector.FATType of
      ftFAT12:
      begin
        newBootSector.FATFilesystemType := SIGNATURE_FAT12;
      end;

      ftFAT16:
      begin
        newBootSector.FATFilesystemType := SIGNATURE_FAT16;
      end;

      ftFAT32:
      begin
        newBootSector.FATFilesystemType := SIGNATURE_FAT32;
      end;

    else
    begin
      allOK := False;
    end;
    end;

    if ((newBootSector.FATType = ftFAT12) or (newBootSector.FATType = ftFAT16)) then begin
      newBootSector.RootDirFirstCluster := FAT1216_ROOT_DIR_FIRST_CLUSTER;

      newBootSector.SectorNoFSInfoSector   := DEFAULT_SECTOR_NO_FS_INFO_SECTOR_FAT1216;
      newBootSector.SectorNoBootSectorCopy := DEFAULT_SECTOR_BOOT_SECTOR_COPY_FAT1216;
      newBootSector.PhysicalDriveNo        := DEFAULT_PHYSICAL_DRIVE_NO;
      newBootSector.ExtendedBootSig        := DEFAULT_EXTENDED_BOOT_SIG;
      newBootSector.VolumeLabel            := DEFAULT_VOLUME_LABEL;
      // .FATFilesystemType populated above
    end else begin
      // When we write the root dir on a FAT32 system, we'll write it to the first
      // cluster; cluster 2
      newBootSector.RootDirFirstCluster := FAT32_ENTRY_USED_START;

      newBootSector.FATFlags               := DEFAULT_FAT_FLAGS;
      newBootSector.Version                := DEFAULT_VERSION;
      newBootSector.SectorNoFSInfoSector   := DEFAULT_SECTOR_NO_FS_INFO_SECTOR_FAT32;
      newBootSector.SectorNoBootSectorCopy := DEFAULT_SECTOR_BOOT_SECTOR_COPY_FAT32;
      newBootSector.PhysicalDriveNo        := DEFAULT_PHYSICAL_DRIVE_NO;
      newBootSector.ExtendedBootSig        := DEFAULT_EXTENDED_BOOT_SIG;
      newBootSector.VolumeLabel            := DEFAULT_VOLUME_LABEL;
      // .FATFilesystemType populated above
    end;

    Randomize();
    // +1 because random returns values 0 <= X < Range
    // Note: This *should* be $FFFFFFFF, but random(...) only takes an
    //       integer, so we use $7FFFFFFF (the max value an intege can hold)
    //       instead
    newBootSector.SerialNumber := (random($7FFFFFFF) + 1);


    newBootSector.BootSectorSig := FAT_BOOTSECTORSIG;

    if allOK then begin
      allOK := WriteBootSector(newBootSector);
    end;

    // Read the boot sector just written out
    if allOK then begin
      prevMounted := Mounted;
      FMounted    := True;
      try
        allOK := ReadBootSector();
      finally
        FMounted := prevMounted;
      end;
    end;



    // =======================================================================
    // Part XXX: Write FSInfo sector (FAT32 only)
    if allOK then begin
      if (newBootSector.FATType = ftFAT32) then begin
        newFSInfo := TSDUMemoryStream.Create();
        try
          // Initialize with all zero bytes...
          newFSInfo.WriteByte(0, 0, newBootSector.BytesPerSector);

          newFSInfo.WriteDWORD_LE(FSINFO_SIG_LEAD, FSINFO_OFFSET_FSINFOSIG);
          newFSInfo.WriteDWORD_LE(FSINFO_SIG_STRUCT, FSINFO_OFFSET_STRUCTSIG);
          newFSInfo.WriteDWORD_LE(FSINFO_UNKNOWN_FREECOUNT, FSINFO_OFFSET_FREECOUNT);
          newFSInfo.WriteDWORD_LE(FSINFO_UNKNOWN_NEXTFREE, FSINFO_OFFSET_NEXTFREE);
          newFSInfo.WriteDWORD_LE(FSINFO_SIG_TRAIL, FSINFO_OFFSET_TRAILSIG);

          newFSInfo.Position := 0;
          allOK              := PartitionImage.WriteSector(newBootSector.SectorNoFSInfoSector,
            newFSInfo);

        finally
          newFSInfo.Free();
        end;

      end;
    end;


    // =======================================================================
    // Part XXX: Write backup sectors (FAT32 only)
    if allOK then begin
      if (newBootSector.FATType = ftFAT32) then begin
        // Backup boot sector...
        allOK := PartitionImage.CopySector(0, newBootSector.SectorNoBootSectorCopy);
        // Backup FSInfo sector...
        allOK := PartitionImage.CopySector(newBootSector.SectorNoFSInfoSector,
          (newBootSector.SectorNoBootSectorCopy + 1));
      end;
    end;


    // =======================================================================
    // Part 2: Write FAT

    if allOK then begin
      stmFAT := TSDUMemoryStream.Create();
      try
        // Fill FAT with zeros
        // Slow and crude, but since it's a one off and there's not much of it...
        // "div 4" because we're writing DWORDS (4 bytes)
        for i := 1 to ((newBootSector.SectorsPerFAT * newBootSector.BytesPerSector) div 4) do begin
          stmFAT.WriteDWORD_LE(0);
        end;

        for i := 1 to newBootSector.FATCount do begin
          stmFAT.Position  := 0;
          FATStartSectorID := newBootSector.ReservedSectorCount +
            ((i - 1) * newBootSector.SectorsPerFAT);
          allOK            := PartitionImage.WriteConsecutiveSectors(FATStartSectorID,
            stmFAT, (newBootSector.SectorsPerFAT * newBootSector.BytesPerSector));
        end;

      finally
        stmFAT.Free();
      end;
    end;

    // Read the FAT just written back in
    // This is done so we can use SetFATEntry(...) to set FAT entries for
    // cluster 0 and 1
    if allOK then begin
      prevMounted := Mounted;
      FMounted    := True;
      try
        allOK := ReadFAT(DEFAULT_FAT);
      finally
        FMounted := prevMounted;
      end;
    end;

    if allOK then begin
      // Before any SetFATEntry(...) if called
      SetupFFATEntryValues(newBootSector.FATType);

      // FAT entry 0: Media descriptor in LSB + remaining bits set to 1
      //              (except first 4 MSB bits in FAT32)
      // FAT entry 1: EOCStart marker - but better as all 1?
      FATEntry_0 := 0;
      FATEntry_1 := 0;
      if (newBootSector.FATType = ftFAT12) then begin
        FATEntry_0 := $F00;
        FATEntry_1 := $FFF;
      end else
      if (newBootSector.FATType = ftFAT16) then begin
        FATEntry_0 := $FF00;
        FATEntry_1 := $FFFF;
      end else
      if (newBootSector.FATType = ftFAT32) then begin
        FATEntry_0 := $0FFFFF00;
        FATEntry_1 := $FFFFFFFF;
      end;
      FATEntry_0 := FATEntry_0 + newBootSector.MediaDescriptor;

      SetFATEntry(0, FATEntry_0);
      // This *should* be the EOC marker.
      // However, for some reason, MS Windows uses 0xFFFFFFFF when
      // formatting?! (e.g. for FAT32)
      // SetFATEntry(1, FFATEntryEOCStart);
      // SetFATEntry(1, FFATEntryEOCEnd);
      SetFATEntry(1, FATEntry_1);

      WriteFATToAllCopies();
    end;

    // =======================================================================
    // Part 3: Write root directory

    if allOK then begin
      newDirContent := TSDUMemoryStream.Create();
      tmpDirItem    := TSDDirItem_FAT.Create();
      try
        if (newBootSector.MaxRootEntries > 0) then begin
          newDirContent.Size := (newBootSector.MaxRootEntries * DIR_ENTRY_SIZE);
        end else begin
          newDirContent.Size := useClusterSize;
        end;
        newDirContent.WriteByte(0, 0, newDirContent.Size);

        newDirContent.Position := 0;
        allOK                  :=
          StoreClusterData(newBootSector.RootDirFirstCluster, newDirContent);

        if (newBootSector.FATType = ftFAT32) then begin
          SetLength(itemChain, 1);
          itemChain[0]           := newBootSector.RootDirFirstCluster;
          newDirContent.Position := 0;
          allOK                  :=
            AllocateChainForData(itemChain, itemChainUnused, newDirContent, newDirContent.Size);
          StoreClusterChain(itemChain);
        end;

        WriteFATToAllCopies();

      finally
        tmpDirItem.Free();
        newDirContent.Free();
      end;
    end;


  finally
    FSerializeCS.Release();
  end;

  Result := allOK;
end;


procedure TSDFilesystem_FAT.SetupFFATEntryValues(SetupAsFATType: TFATType);
begin
  case SetupAsFATType of
    ftFAT12:
    begin
      //      FFATEntrySize      := FAT12_ENTRY_SIZE;
      FFATEntrySize      := 0;
      FFATEntryMask      := FAT12_ENTRY_MASK;
      FFATEntryFree      := FAT12_ENTRY_FREE;
      FFATEntryUsedStart := FAT12_ENTRY_USED_START;
      FFATEntryUsedEnd   := FAT12_ENTRY_USED_END;
      FFATEntryBadSector := FAT12_ENTRY_BAD_SECTOR;
      FFATEntryEOCStart  := FAT12_ENTRY_EOC_START;
      FFATEntryEOCEnd    := FAT12_ENTRY_EOC_END;
    end;

    ftFAT16:
    begin
      FFATEntrySize      := FAT16_ENTRY_SIZE;
      FFATEntryMask      := FAT16_ENTRY_MASK;
      FFATEntryFree      := FAT16_ENTRY_FREE;
      FFATEntryUsedStart := FAT16_ENTRY_USED_START;
      FFATEntryUsedEnd   := FAT16_ENTRY_USED_END;
      FFATEntryBadSector := FAT16_ENTRY_BAD_SECTOR;
      FFATEntryEOCStart  := FAT16_ENTRY_EOC_START;
      FFATEntryEOCEnd    := FAT16_ENTRY_EOC_END;
    end;

    ftFAT32:
    begin
      FFATEntrySize      := FAT32_ENTRY_SIZE;
      FFATEntryMask      := FAT32_ENTRY_MASK;
      FFATEntryFree      := FAT32_ENTRY_FREE;
      FFATEntryUsedStart := FAT32_ENTRY_USED_START;
      FFATEntryUsedEnd   := FAT32_ENTRY_USED_END;
      FFATEntryBadSector := FAT32_ENTRY_BAD_SECTOR;
      FFATEntryEOCStart  := FAT32_ENTRY_EOC_START;
      FFATEntryEOCEnd    := FAT32_ENTRY_EOC_END;
    end;

  end;
end;

 // Given a boot sector, attempt to identify filesystem type; one of
 // FAT12/FAT16/FAT32
function TSDFilesystem_FAT.DetermineFATType(stmBootSector: TSDUMemoryStream): TFATType;
var
  Result:          TFATType;
  //  fsTypeString: string;
  BPB_BytsPerSec:  DWORD;
  BPB_RootEntCnt:  DWORD;
  RootDirSectors:  DWORD;
  FATSz:           DWORD;
  TotSec:          DWORD;
  BPB_ResvdSecCnt: Word;
  BPB_NumFATs:     Byte;
  DataSec:         DWORD;
  CountofClusters: DWORD;
  BPB_SecPerClus:  Byte;
begin
{
  // Use the filesystem type string in the boot sector to determine.
  // Note: THIS METHOD IS WRONG!!! FAT filesystem type should actually be
  //       determined by the number of cluster count!
  fsTypeString := stmBootSector.ReadString(BOOTSECTOR_LENGTH_FAT32_FATFSTYPE, BOOTSECTOR_OFFSET_FAT32_FATFSTYPE);
  if (fsTypeString = SIGNATURE_FAT32) then
    begin
    Result := ftFAT32;
    end;
  fsTypeString := stmBootSector.ReadString(BOOTSECTOR_LENGTH_FAT1216_FATFSTYPE, BOOTSECTOR_OFFSET_FAT1216_FATFSTYPE);
  if (fsTypeString = SIGNATURE_FAT12) then
    begin
    Result := ftFAT12;
    end
  else if (fsTypeString = SIGNATURE_FAT16) then
    begin
    Result := ftFAT16;
    end;
}

  // This algorithm for determining FAT type taken from the MS spec:

  BPB_BytsPerSec := stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_BYTESPERSECTOR);
  BPB_RootEntCnt := stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_MAXROOTENTRIES);

  // Note: This rounds *down* - hence "div"
  RootDirSectors := ((BPB_RootEntCnt * 32) + (BPB_BytsPerSec - 1)) div BPB_BytsPerSec;

  FATSz := stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_SECTORSPERFAT);
  if (FATSz = 0) then begin
    FATSz := stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_FAT32_SECTORSPERFAT);
  end;

  TotSec := stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_TOTALSECTORS_SMALL);
  if (TotSec = 0) then begin
    TotSec := stmBootSector.ReadDWORD_LE(BOOTSECTOR_OFFSET_TOTALSECTORS_LARGE);
  end;

  BPB_ResvdSecCnt := stmBootSector.ReadWORD_LE(BOOTSECTOR_OFFSET_RESERVEDSECTORCOUNT);
  BPB_NumFATs     := stmBootSector.ReadByte(BOOTSECTOR_OFFSET_FATCOUNT);
  BPB_SecPerClus  := stmBootSector.ReadByte(BOOTSECTOR_OFFSET_SECTORSPERCLUSTER);

  DataSec         := TotSec - (BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors);
  // Note: This rounds *down* - hence "div"
  CountofClusters := DataSec div BPB_SecPerClus;

  if (CountofClusters < 4085) then begin
    // Volume is FAT12
    Result := ftFAT12;
  end else
  if (CountofClusters < 65525) then begin
    // Volume is FAT16
    Result := ftFAT16;
  end else begin
    // Volume is FAT32
    Result := ftFAT32;
  end;


end;

// Extract FAT12/FAT16 root directory contents
function TSDFilesystem_FAT.ExtractFAT1216RootDir(data: TStream): Boolean;
begin
  Result := ReadWriteFAT1216RootDir(True, data);
end;

// Store FAT12/FAT16 root directory contents
function TSDFilesystem_FAT.StoreFAT1216RootDir(data: TStream): Boolean;
begin
  Result := ReadWriteFAT1216RootDir(False, data);
end;

// Read/write FAT12/FAT16 root directory contents
function TSDFilesystem_FAT.ReadWriteFAT1216RootDir(readNotWrite: Boolean; data: TStream): Boolean;
var
  startSectorID:        DWORD;
  allOK:                Boolean;
  bytesRemaining:       Integer;
  sectorMax:            Integer;
  rootDirSizeInBytes:   Integer;
  rootDirSizeInSectors: Integer;
begin
  rootDirSizeInBytes   := (MaxRootEntries * DIR_ENTRY_SIZE);
  rootDirSizeInSectors := (rootDirSizeInBytes div BytesPerSector);

  // Sanity check on writing...
  if not (readNotWrite) then begin
    AssertSufficientData(data, rootDirSizeInBytes);
  end;

  bytesRemaining := rootDirSizeInBytes;

  startSectorID := ReservedSectorCount + (FATCount * SectorsPerFAT);
  sectorMax     := min(bytesRemaining, (BytesPerSector * rootDirSizeInSectors));

  if readNotWrite then begin
    allOK := PartitionImage.ReadConsecutiveSectors(startSectorID, data, sectorMax);
  end else begin
    allOK := PartitionImage.WriteConsecutiveSectors(startSectorID, data, sectorMax);
  end;

  bytesRemaining := bytesRemaining - sectorMax;

  if allOK then begin
    allOK := (bytesRemaining = 0);
  end;

  Result := allOK;
end;



// This function will delete DOSFilename from dirData
function TSDFilesystem_FAT.DeleteEntryFromDir(DOSFilename: String;
  dirData: TSDUMemoryStream): Boolean;
var
  allOK:          Boolean;
  currAttributes: Byte;
  recordOffset:   Int64;
begin
  allOK := True;

  if allOK then begin
    dirData.Position := 0;
    allOK            := Seek83FileDirNameInDirData(DOSFilename, dirData);
  end;

  if allOK then begin
    // Update the parent directories contents, marking 8.3 and LFN entries
    // associated with the deleted item as deleted

    recordOffset := dirData.Position;
    // Mark 8.3 dir entry as deleted...
    dirData.WriteByte(DIR_ENTRY_DELETED);

    // Work backwards, removing any LFNs associated with the 8.3 dir entry
    // +1 because the DIR_ENTRY_DELETED byte moved us forward one byte
    while (recordOffset > 0) do begin
      recordOffset := recordOffset - DIR_ENTRY_SIZE;

      dirData.Position := recordOffset;
      currAttributes   := dirData.ReadByte(recordOffset + DIR_ENTRY_OFFSET_FILEATTRS);
      if ((currAttributes and VFAT_ATTRIB_VFAT_ENTRY) <> VFAT_ATTRIB_VFAT_ENTRY) then begin
        break;
      end;

      // Mark LFN entry as deleted...
      dirData.Position := recordOffset;
      dirData.WriteByte(DIR_ENTRY_DELETED);
    end;

  end;

  Result := allOK;
end;


// This function will add a directory entry for newItem to dirData
function TSDFilesystem_FAT.AddEntryToDir(itemToAdd: TSDDirItem_FAT;
  var dirChain: TSDFATClusterChain; dirData: TSDUMemoryStream; var clusterReserved: DWORD;
  flagDirDataIsRootDirData: Boolean): Boolean;
var
  allOK:            Boolean;
  tmpPos:           Int64;
  cntDirEntriesReq: Integer;
begin
  cntDirEntriesReq := WriteDirEntry(itemToAdd, nil);
  allOK            := (cntDirEntriesReq >= 0);
  if allOK then begin
    if not (SeekBlockUnusedDirEntries(cntDirEntriesReq, dirData)) then begin
      // The root directory under FAT12/FAT16 can't be extended
      if (flagDirDataIsRootDirData and ((FATType = ftFAT12) or (FATType = ftFAT16)))
      then begin
        // That's it - full root dir; can't store it!
        allOK := False;
      end else begin
        tmpPos := dirData.Size;
        ExtendByEmptyCluster(dirData);
        AddClusterToChain(clusterReserved, dirChain);
        // Change clusterReserved so we don't unreserve it
        clusterReserved  := ERROR_DWORD;
        dirData.Position := tmpPos;
      end;
    end;
  end;

  if allOK then begin
    WriteDirEntry(itemToAdd, dirData);
  end;

  Result := allOK;
end;

function TSDFilesystem_FAT.IsValidFilename(filename: String): Boolean;
var
  i:              Integer;
  filenameNoDots: String;
begin
  Result := True;

  filename := trim(filename);

  // Filename must have *some* characters in it
  if Result then begin
    Result := (length(filename) > 0);
  end;

  // Filename can't just consist of a "." characters
  if Result then begin
    filenameNoDots := trim(StringReplace(filename, '.', '', [rfReplaceAll]));
    Result         := (length(filenameNoDots) > 0);
  end;

  if Result then begin
    for i := 1 to length(FAT_INVALID_FILENAME_CHARS) do begin
      if (Pos(FAT_INVALID_FILENAME_CHARS[i], filename) > 0) then begin
        Result := False;
        break;
      end;
    end;
  end;


end;

function TSDFilesystem_FAT.FilesystemTitle(): String;
begin
  Result := FATTypeTitle(FATType);
end;

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------

end.
