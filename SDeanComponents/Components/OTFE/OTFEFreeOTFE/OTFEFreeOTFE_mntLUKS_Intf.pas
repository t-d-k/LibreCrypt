
{$INCLUDE OTFEFreeOTFE_mntLUKS_AFS_Intf.pas}

// ------------------------
// "Private"

function  ParseLUKSHeader(rawData: Ansistring; var LUKSheader: TLUKSHeader): boolean;
function  IdentifyHash(hashSpec: string; var hashKernelModeDeviceName: string; var hashGUID: TGUID): boolean;
function  IdentifyCypher(
                         cypherName: string;
                         keySizeBits: integer;
                         cypherMode: string;
                         baseIVCypherOnHashLength: boolean;
                         var cypherKernelModeDeviceName: string;
                         var cypherGUID: TGUID;
                         var sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                         var IVHashKernelModeDeviceName: string;
                         var IVHashGUID: TGUID;
                         var IVCypherKernelModeDeviceName: string;
                         var IVCypherGUID: TGUID
                        ): boolean;
function IdentifyCypher_SearchCyphers(
                                      cypherDrivers: array of TFreeOTFECypherDriver;
                                      cypherName: string;
                                      keySizeBits: integer;
                                      useCypherMode: TFreeOTFECypherMode;
                                      var cypherKernelModeDeviceName: string;
                                      var cypherGUID: TGUID
                                     ): boolean;
function  PrettyHex(data: Pointer; bytesCount: integer): Ansistring;
function  StringHasNumbers(checkString: string): boolean;

// ------------------------
// "Public"

// Determine if the specified volume is a Linux LUKS volume
function  IsLUKSVolume(volumeFilename: string): boolean;

// Read a LUKS key in from a file
function ReadLUKSKeyFromFile(
  filename: string;
  treatAsASCII: boolean;
  ASCIINewline: TSDUNewline;
  out key: PasswordString
): boolean;


// If keyfile is set, ignore userKey and read the key from the file
function  DumpLUKSDataToFile(
  filename: string;
  userKey: Ansistring;
  keyfile: string;
  keyfileIsASCII: boolean;
  keyfileNewlineType: TSDUNewline;
  baseIVCypherOnHashLength: boolean;
  dumpFilename: string
): boolean;

// Note: This will only populate the LUKS members of the header; it will not
//       populate the FreeOTFE driver details
function  ReadLUKSHeader(filename: string; var LUKSheader: TLUKSHeader): boolean;

// If the hash and cypher members of the LUKS header passed in are populated,
// this function will populate struct with the the FreeOTFE driver details
// (e.g. driver name/GUID) that correspond to that hash/cypher
// Note: Must be called *after* ReadLUKSHeader(...)
function  MapToFreeOTFE(baseIVCypherOnHashLength: boolean; var LUKSheader: TLUKSHeader): boolean;

function  RecoverMasterKey(
                                filename: string;
                                userPassword: Ansistring;
                                baseIVCypherOnHashLength: boolean;
                                var LUKSHeader: TLUKSHeader;
                                var keySlot: integer;
                                var keyMaterial: Ansistring
                               ): boolean;


