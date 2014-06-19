unit OTFEFreeOTFE_VolumeFileAPI;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  OTFEFreeOTFE_DriverAPI,
  Windows;  // Required for DWORD

const
  // The CDB version ID to be used in all new CDBs
  CDB_FORMAT_ID = 4;


  // Flags indicating whether various sector IV generation methods use the
  // sector ID
  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //            SCTRIVGEN_USES_CYPHER
  //          *must* all be kept in sync with each other at all times
  SCTRIVGEN_USES_SECTOR_ID : array [TFreeOTFESectorIVGenMethod] of boolean =
                                          (
                                           FALSE,  // foivgNone,
                                           TRUE,   // foivg32BitSectorID,
                                           TRUE,   // foivg64BitSectorID,
                                           TRUE,   // foivgHash32BitSectorID,
                                           TRUE,   // foivgHash64BitSectorID,
                                           TRUE,   // foivgESSIV,
                                           FALSE   // foivgUnknown
                                          );

  // Flags indicating whether various sector IV generation methods use a
  // a hash algorithm
  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //            SCTRIVGEN_USES_CYPHER
  //          *must* all be kept in sync with each other at all times
  SCTRIVGEN_USES_HASH : array [TFreeOTFESectorIVGenMethod] of boolean =
                                          (
                                           FALSE,  // foivgNone,
                                           FALSE,  // foivg32BitSectorID,
                                           FALSE,  // foivg64BitSectorID,
                                           TRUE,   // foivgHash32BitSectorID,
                                           TRUE,   // foivgHash64BitSectorID,
                                           TRUE,   // foivgESSIV,
                                           FALSE   // foivgUnknown
                                          );

  // Flags indicating whether various sector IV generation methods use a
  // a cypher
  // WARNING: The definition of:
  //            TFreeOTFESectorIVGenMethod
  //            FreeOTFESectorIVGenMethodTitle
  //            FreeOTFESectorIVGenMethodID
  //            SCTRIVGEN_USES_SECTOR_ID
  //            SCTRIVGEN_USES_HASH
  //            SCTRIVGEN_USES_CYPHER
  //          *must* all be kept in sync with each other at all times
  SCTRIVGEN_USES_CYPHER : array [TFreeOTFESectorIVGenMethod] of boolean =
                                          (
                                           FALSE,  // foivgNone,
                                           FALSE,  // foivg32BitSectorID,
                                           FALSE,  // foivg64BitSectorID,
                                           FALSE,  // foivgHash32BitSectorID,
                                           FALSE,  // foivgHash64BitSectorID,
                                           TRUE,   // foivgESSIV,
                                           FALSE   // foivgUnknown
                                          );


  
type
  // Note: If this is changed, the DumpCriticalDataToFile(...) function should
  //       probably also be changed
  PVolumeDetailsBlock = ^TVolumeDetailsBlock;
  TVolumeDetailsBlock = packed record
    CDBFormatID: byte;

    VolumeFlags: DWORD;
    PartitionLen: int64;  // In *bytes*

    MasterKeyLength: DWORD;  // In *bits*
    MasterKey: string;

    RequestedDriveLetter: char;

    SectorIVGenMethod: TFreeOTFESectorIVGenMethod;

    VolumeIVLength: DWORD;  // In *bits*
    VolumeIV: string;
  end;

  TVolumeDetailsBlockArray = array of TVolumeDetailsBlock;


  // Note: If this is changed, the DumpCriticalDataToFile(...) function should
  //       probably also be changed
  // This structure stores information that is not worked out automatically;
  // it's the information which the user *doens't* supply
  PCDBMetaData = ^TCDBMetaData;
  TCDBMetaData = packed record
    MACAlgorithm: TFreeOTFEMACAlgorithm;
    KDFAlgorithm: TFreeOTFEKDFAlgorithm;

    // driver - Kernel drivers: HashDriverKernelModeName
    //          DLL drivers:    HashLibFilename
    HashDriver: string;
    HashGUID: TGUID;
    // driver - Kernel drivers: CypherDriverKernelModeName
    //          DLL drivers:    CypherLibFilename
    CypherDriver: string;
    CypherGUID: TGUID;
  end;

  TCDBMetaDataArray = array of TCDBMetaData;


implementation

END.



