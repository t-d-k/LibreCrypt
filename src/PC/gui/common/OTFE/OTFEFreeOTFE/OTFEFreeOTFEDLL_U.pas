unit OTFEFreeOTFEDLL_U;

    {
TOTFEFreeOTFEDLL is a wrapper round the DLL and also does all drive operations,
enc data, create new vols, etc
is a singleton class, only ever one instance - this is enforced by assertion in
base class ctor
set up instance by calling OTFEFreeOTFEBase_U.SetFreeOTFEType then get instance
by calling OTFEFreeOTFEBase_U.GetFreeOTFE or  TOTFEFreeOTFEDLL.GetFreeOTFE
}

interface

uses
 //delphi / libs
    Classes, Windows,
  //sdu & LibreCrypt utils
   lcTypes, sdugeneral,
  OTFE_U,
  OTFEFreeOTFEBase_U,
  pkcs11_session,
  PKCS11Lib,
  FreeOTFEDLLMainAPI,
  FreeOTFEDLLHashAPI,
  FreeOTFEDLLCypherAPI,
  DriverAPI,
  lcDebugLog,  PartitionTools,
  OTFEFreeOTFE_DriverHashAPI,
  OTFEFreeOTFE_DriverCypherAPI
   // LibreCrypt forms


  ;

type
  TMainAPI = record
    LibFilename: String;
    hDLL:        THandle;

    DSK_Init:      PDSK_Init;
    DSK_Deinit:    PDSK_Deinit;
    DSK_Open:      PDSK_Open;
    DSK_Close:     PDSK_Close;
    DSK_IOControl: PDSK_IOControl;
    DSK_Seek:      PDSK_Seek;
    DSK_Read:      PDSK_Read;
    DSK_Write:     PDSK_Write;
    DSK_PowerDown: PDSK_PowerDown;
    DSK_PowerUp:   PDSK_PowerUp;

    MainDLLFnDeriveKey: PMainDLLFnDeriveKey;
    MainDLLFnMACData:   PMainDLLFnMACData;
  end;

  THashAPI = record
    LibFilename: String;
    hDLL:        THandle;

    HashDLLFnIdentifyDriver: PHashDLLFnIdentifyDriver;
    HashDLLFnIdentifySupported: PHashDLLFnIdentifySupported;
    HashDLLFnGetHashDetails: PHashDLLFnGetHashDetails;
    HashDLLFnHash: PHashDLLFnHash;
  end;
  PHashAPI = ^THashAPI;

  TCypherAPI = record
    LibFilename: String;
    hDLL:        THandle;

    CypherDLLFnIdentifyDriver:   PCypherDLLFnIdentifyDriver;
    //    CypherDLLFnIdentifySupported_v1: PCypherDLLFnIdentifySupported_v1;
    CypherDLLFnIdentifySupported_v3: PCypherDLLFnIdentifySupported_v3;
    //    CypherDLLFnGetCypherDetails_v1: PCypherDLLFnGetCypherDetails_v1;
    CypherDLLFnGetCypherDetails_v3: PCypherDLLFnGetCypherDetails_v3;
    CypherDLLFnEncrypt:          PCypherDLLFnEncrypt;
    CypherDLLFnEncryptSector:    PCypherDLLFnEncryptSector;
    CypherDLLFnEncryptWithASCII: PCypherDLLFnEncryptWithASCII;
    CypherDLLFnEncryptSectorWithASCII: PCypherDLLFnEncryptSectorWithASCII;
    CypherDLLFnDecrypt:          PCypherDLLFnDecrypt;
    CypherDLLFnDecryptSector:    PCypherDLLFnDecryptSector;
    CypherDLLFnDecryptWithASCII: PCypherDLLFnDecryptWithASCII;
    CypherDLLFnDecryptSectorWithASCII: PCypherDLLFnDecryptSectorWithASCII;
  end;
  PCypherAPI = ^TCypherAPI;

  TMountedHandle = record
    DeviceHandle: DWORD;
    OpenHandle:   DWORD;

    BytesPerSector: DWORD;
  end;
  PMountedHandle = ^TMountedHandle;

  TOTFEFreeOTFEDLL = class (TOTFEFreeOTFEBase)
  private
    FExeDir: String;
    function GetDLLDir: String;

  protected
    fDriver_API:           TMainAPI;
    fMountedHandles:     TStringList;// in unicdoe
    fCachedDiskGeometry: TStringList;

    // Cached information: The strings in the list are DLL path and filenames,
    // the objects are THashAPI/TCypherAPI
    fCachedHashAPI:   TStringList;
    fCachedCypherAPI: TStringList;


    function Connect(): Boolean; override;
    procedure Disconnect(); override;

    procedure AddHandles(driveLetter: Char; handles: TMountedHandle);
    function GetHandles(driveLetter: Char; var handles: TMountedHandle): Boolean;
    procedure DeleteHandles(driveLetter: Char);

    //todo:move DiskGeometry fns to separate file eg PartitionTools
    procedure AddDiskGeometry(driveLetter: Char; diskGeometry: TSDUDiskGeometry);
    procedure DeleteDiskGeometry(driveLetter: Char);
    // Internal use only; callers should normally use GetDiskGeometry(...)
    // (without prefixing "_") instead
    function _GetDiskGeometry(
      DriveLetter: Char;
      var diskGeometry: TSDUDiskGeometry
      ): Boolean;

    //    function _GetCypherDriverCyphers_v1(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; override;
    function _GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
      var cypherDriverDetails: TFreeOTFECypherDriver): Boolean; override;

    procedure GetAllDriversUnderExeDir(driverFilenames: TStringList);

    function LoadLibraryMain(var api: TMainAPI): Boolean;
    procedure FreeLibraryMain(var api: TMainAPI);
    function LoadLibraryHash(const libFilename: String; var api: THashAPI): Boolean;
    function LoadLibraryCachedHash(const libFilename: String; var api: THashAPI): Boolean;
    procedure FreeLibraryHash(var api: THashAPI);
    function LoadLibraryCypher(const libFilename: String; var api: TCypherAPI): Boolean;
    function LoadLibraryCachedCypher(const libFilename: String; var api: TCypherAPI): Boolean;
    procedure FreeLibraryCypher(var api: TCypherAPI);

    function MainDLLFilename(): String;

    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations

    function MountDiskDevice(
      deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      volumeIV: TSDUBytes;
      ReadOnly: Boolean;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      metaData: TOTFEFreeOTFEVolumeMetaData;
      offset: Int64 = 0;
      size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType =
      mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; override;

    function DismountDiskDevice(
      deviceName: String;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      emergency: Boolean
      ): Boolean; override;


    // ---------
    // Internal caching functions
    procedure CachesCreate(); override;
    procedure CachesDestroy(); override;
    procedure CachesAddHashAPI(const libFilename: String; const api: THashAPI);
    function CachesGetHashAPI(const libFilename: String; var api: THashAPI): Boolean;
    procedure CachesAddCypherAPI(const libFilename: String; const api: TCypherAPI);
    function CachesGetCypherAPI(const libFilename: String; var api: TCypherAPI): Boolean;


    // Misc functions
    function DriverType(): String; override;

  public

    // TOTFE standard API
    constructor Create(); override;
    destructor Destroy; override;
    function Dismount(driveLetter: Char; emergency: Boolean = False): Boolean; overload; override;
    function Version(): Cardinal; overload; override;
    function DrivesMounted(): String; overload; override;


    // -----------------------------------------------------------------------
    // TOTFEFreeOTFEBase standard API

    function GetVolumeInfo(driveLetter: Char; var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
      override;

    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean; override;
    function GetHashDriverHashes(hashDriver: String;
      var hashDriverDetails: TFreeOTFEHashDriver): Boolean; override;

    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean; override;

       // ---------
    // Raw volume access functions

    function ReadWritePlaintextToVolume(
      readNotWrite: Boolean;
      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      mountMountAs: TFreeOTFEMountAs;

      dataOffset: Int64;
    // Offset from within mounted volume from where to read/write data
      dataLength: Integer;  // Length of data to read/write. In bytes
      var data: TSDUBytes;  // Data to read/write

      offset: Int64 = 0;
      size: Int64 = 0;
      storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
      ): Boolean; override;


    function _EncryptDecryptData(
      encryptFlag: Boolean;
      cypherDriver: Ansistring;
      cypherGUID: TGUID;
      var key: TSDUBytes;
      var IV: Ansistring;
      var inData: Ansistring;
      var outData: Ansistring
      ): Boolean; override;

    function EncryptDecryptSectorData(
      encryptFlag: Boolean;
      cypherDriver: Ansistring;
      cypherGUID: TGUID;
      SectorID: LARGE_INTEGER;
      SectorSize: Integer;
      var key: TSDUBytes;
      var IV: Ansistring;
      var inData: Ansistring;
      var outData: Ansistring
      ): Boolean; override;

    function HashData(
      hashDriver: Ansistring;
      hashGUID: TGUID;
      const data: TSDUBytes;
      out hashOut: TSDUBytes
      ): Boolean; override;

    function MACData(
      macAlgorithm: TFreeOTFEMACAlgorithm;
      HashDriver: Ansistring;
      HashGUID: TGUID;
      CypherDriver: Ansistring;
      CypherGUID: TGUID;
      var key: PasswordString;
      var data: Ansistring;
      var MACOut: Ansistring;
      tBits: Integer = -1
      ): Boolean; override;

    function DeriveKey(
      kdfAlgorithm: TFreeOTFEKDFAlgorithm;
      HashDriver: Ansistring;
      HashGUID: TGUID;
      CypherDriver: Ansistring;
      CypherGUID: TGUID;
      Password: TSDUBytes;
      Salt: array of Byte;
      Iterations: Integer;
      dkLenBits: Integer;  // In *bits*
      out DK: TSDUBytes
      ): Boolean; override;


    // -----------------------------------------------------------------------
    // Extended FreeOTFE DLL specific functions

    function Seek(driveLetter: Char; offset: DWORD): Boolean;
    function ReadData_Sectors(
      DriveLetter: Char;
      SectorNoStart: DWORD;
      CountSectors: DWORD;
      stm: TStream
      ): Boolean;
    function WriteData_Sectors(
      DriveLetter: Char;
      SectorNoStart: DWORD;
      CountSectors: DWORD;
      stm: TStream
      ): Boolean;
    function ReadData_Bytes(
      DriveLetter: Char;
      Offset: ULONGLONG;
      DataLength: ULONGLONG;
      Data: TStream
      ): Boolean;
    function WriteData_Bytes(
      DriveLetter: Char;
      Offset: ULONGLONG;
      DataLength: ULONGLONG;
      Data: TStream
      ): Boolean;
    function GetDiskGeometry(
      DriveLetter: Char;
      var diskGeometry: TSDUDiskGeometry
      ): Boolean;

    function CreateMountDiskDevice(
      volFilename: String;
      volumeKey: TSDUBytes;
      sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
      volumeIV: TSDUBytes;
      ReadOnly: Boolean;
      IVHashDriver: Ansistring;
      IVHashGUID: TGUID;
      IVCypherDriver: Ansistring;
      IVCypherGUID: TGUID;
      mainCypherDriver: Ansistring;
      mainCypherGUID: TGUID;
      VolumeFlags: Integer;
      DriveLetter: Char;
    // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
      offset: Int64 = 0;
      size: Int64 = 0;
      MetaData_LinuxVolume: Boolean = False;  // Linux volume
      MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
      MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;
    // PC kernel drivers *only* - ignored otherwise
      mountForAllUsers: Boolean =
      True  // PC kernel drivers *only* - ignored otherwise
      ): Boolean; override;

    function GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Char): Char;
      overload; override;
  published
    // The directory under which the "DLL" dir resides
    //    property ExeDir: string read FExeDir ;
  end;

{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFEDLL: TOTFEFreeOTFEDLL;

implementation

uses
 //delphi / libs
       Dialogs,
  SysUtils, Math, Controls, Forms,
  strutils,
  //sdu & LibreCrypt utils
      SDUSysUtils,
  SDUi18n,
  SDUClasses,
  SDUFileIterator_U,OTFEConsts_U,

  OTFEFreeOTFE_LUKSAPI,
  VolumeFileAPI,
  CommonSettings

   // LibreCrypt forms
  ;

const
  DLL_HASH_FILE_PREFIX   = 'FreeOTFEHash';
  DLL_CYPHER_FILE_PREFIX = 'FreeOTFECypher';


 // ----------------------------------------------------------------------------
 //procedure Register;
 //begin
 //  RegisterComponents('OTFE', [TOTFEFreeOTFEDLL]);
 //end;

// ----------------------------------------------------------------------------
constructor TOTFEFreeOTFEDLL.Create();
begin
  inherited;
  fMountedHandles     := TStringList.Create();
  fCachedDiskGeometry := TStringList.Create();

  fExeDir := ExtractFilePath(Application.ExeName);
end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFEDLL.Destroy();
var
  i:           Integer;
  currDrvStr:  String;
  currDrvChar: Char;
begin
  for i := 0 to (fCachedDiskGeometry.Count - 1) do begin
    currDrvStr := fCachedDiskGeometry[i];
    // unicode->ansi not a data loss because only ansistring stored
    if (length(currDrvStr) > 0) then // Sanity check
    begin
      currDrvChar := currDrvStr[1];
      DeleteDiskGeometry(currDrvChar);
    end;
  end;

  fMountedHandles.Free();
  fCachedDiskGeometry.Free();
  inherited;
end;


 // ----------------------------------------------------------------------------
 // This dismount routine is based on the MS "media eject" routine for NT/2k/XP
 // at:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q165/7/21.asp&NoWebContent=1
function TOTFEFreeOTFEDLL.Dismount(driveLetter: Char; emergency: Boolean = False): Boolean;
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  //  unableToOpenDrive: boolean;
begin
  CheckActive();

  Result := True;

  if (Result) then begin

    DebugMsg('getVolumeInfo');

    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if not Result then begin

      DebugMsg('getVolumeInfo NOT Result');

      emergency := False;
    end;
  end;


  if (Result or emergency) then begin

    DebugMsg('dismountdevice');

    Result := DismountDiskDevice(driveLetter, emergency);
  end;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Version(): Cardinal;
var
  majorVersion, minorVersion, revisionVersion, buildVersion: Integer;
begin
  Result := VERSION_ID_FAILURE;

  CheckActive();

  // If we have the version ID cached, use the cached information
  if (fCachedVersionID <> VERSION_ID_NOT_CACHED) then begin
    Result := fCachedVersionID;
  end else begin
    if SDUGetVersionInfo(MainDLLFilename(),
      majorVersion, minorVersion,
      revisionVersion, buildVersion
      ) then begin
      Result           := ((majorVersion shl 24) + (minorVersion shl 16) +
        (revisionVersion));
      fCachedVersionID := Result;
    end;

  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.DrivesMounted(): String;
var
  i: Integer;
begin
  // DLL version doesn't have concept of drive letters as in system drive
  // letters, but uses them to reference different mounted volumes
  Result := '';
  for i := 0 to (fMountedHandles.Count - 1) do begin
    Result := Result + fMountedHandles[i];
    // unicode->ansi not a data loss because only ansistring stored
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Connect(): Boolean;
begin
  Result := False;

  if LoadLibraryMain(fDriver_API) then begin
    Result := True;
  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
  end;

end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.Disconnect();
begin
  FreeLibraryMain(fDriver_API);
end;


 // ----------------------------------------------------------------------------
 // Use the FreeOTFE device driver to mount the specified volume, and either:
 //   *) Read in and decrypt specified data, returning the decrypted version
 //   *) Encrypt specified data, and write to volume
 // readNotWrite - Set to TRUE to read, FALSE to write
function TOTFEFreeOTFEDLL.ReadWritePlaintextToVolume(
  readNotWrite: Boolean;

  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  mountMountAs: TFreeOTFEMountAs;

  dataOffset: Int64;    // Offset from within mounted volume from where to read/write data
  dataLength: Integer;  // Length of data to read/write. In bytes
  var data: TSDUBytes;  // Data to read/write

  offset: Int64 = 0;  // Offset within volume where encrypted data starts
  size: Int64 = 0;    // Size of volume
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia): Boolean;
var
  dummyMetadata:        TOTFEFreeOTFEVolumeMetaData;
  tempMountDriveLetter: Char;
  stm:                  TSDUMemoryStream;
  // emptyIV : TSDUBytes;
  //    volumeKeyStr: AnsiString;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;

  CheckActive();

  PopulateVolumeMetadataStruct(
    False,              // Linux volume
    PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
    dummyMetadata
    );

  // Attempt to mount the device

  tempMountDriveLetter := GetNextDriveLetter();
  // SDUInitAndZeroBuffer(0,emptyIV);
  Result               := MountDiskDevice(tempMountDriveLetter,
    volFilename, volumeKey,
    sectorIVGenMethod, nil, readNotWrite,  // mount as read only if reading
    IVHashDriver, IVHashGUID,
    IVCypherDriver,    // IV cypher
    IVCypherGUID,      // IV cypher
    mainCypherDriver,  // Main cypher
    mainCypherGUID,    // Main cypher
    VolumeFlags, dummyMetadata,
    offset, size,
    FreeOTFEMountAsStorageMediaType[mountMountAs]);
  if Result then begin
    try
      stm := TSDUMemoryStream.Create();
      try
        // Read decrypted data from mounted device
        if (readNotWrite) then begin
          Result := ReadData_Bytes(tempMountDriveLetter,
            dataOffset, dataLength,
            stm);
          if Result then begin
            stm.Position := 0;
            data         := SDUStringToSDUBytes(stm.ReadString(dataLength, 0));
          end;

        end else begin
          stm.Position := 0;
          stm.WriteString(SDUBytesToString(data), dataLength, 0);

          Result := WriteData_Bytes(tempMountDriveLetter,
            dataOffset, dataLength,
            stm);
        end;

      finally
        stm.Free();
      end;

    finally
      // Unmount
      DismountDiskDevice(tempMountDriveLetter, True);
    end;

  end;  // if (Result) then

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.LoadLibraryMain(var api: TMainAPI): Boolean;
var
  libFilename: String;
begin
  Result := False;

  libFilename := MainDLLFilename();
  api.hDLL    := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then begin
    Result          := True;
    api.LibFilename := libFilename;
    @api.DSK_Init   := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Init
      );
    if (@api.DSK_Init = nil) then begin
      Result := False;
    end;
    @api.DSK_Deinit := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Deinit
      );
    if (@api.DSK_Deinit = nil) then begin
      Result := False;
    end;
    @api.DSK_Open := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Open
      );
    if (@api.DSK_Open = nil) then begin
      Result := False;
    end;
    @api.DSK_Close := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Close
      );
    if (@api.DSK_Close = nil) then begin
      Result := False;
    end;
    @api.DSK_IOControl := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_IOControl
      );
    if (@api.DSK_IOControl = nil) then begin
      Result := False;
    end;
    @api.DSK_Seek := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Seek
      );
    if (@api.DSK_Seek = nil) then begin
      Result := False;
    end;
    @api.DSK_Read := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Read
      );
    if (@api.DSK_Read = nil) then begin
      Result := False;
    end;
    @api.DSK_Write := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_Write
      );
    if (@api.DSK_Write = nil) then begin
      Result := False;
    end;
    @api.DSK_PowerDown := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_PowerDown
      );
    if (@api.DSK_PowerDown = nil) then begin
      Result := False;
    end;
    @api.DSK_PowerUp := GetProcAddress(api.hDLL,
      DLLEXPORT_MAIN_DSK_PowerUp
      );
    if (@api.DSK_PowerUp = nil) then begin
      Result := False;
    end;
    @api.MainDLLFnMACData := GetProcAddress(
      api.hDLL,
      DLLEXPORT_MAIN_MACDATA
      );
    if (@api.MainDLLFnMACData = nil) then begin
      Result := False;
    end;
    @api.MainDLLFnDeriveKey := GetProcAddress(
      api.hDLL,
      DLLEXPORT_MAIN_DERIVEKEY
      );
    if (@api.MainDLLFnDeriveKey = nil) then begin
      Result := False;
    end;


    if not (Result) then begin
      FreeLibrary(api.hDLL);
    end;
  end;

end;

procedure TOTFEFreeOTFEDLL.FreeLibraryMain(var api: TMainAPI);
begin
  if (api.hDLL <> 0) then begin
    FreeLibrary(api.hDLL);
    api.hDLL        := 0;
    api.LibFilename := '';
  end;
end;


function TOTFEFreeOTFEDLL.LoadLibraryHash(const libFilename: String; var api: THashAPI): Boolean;
begin
  Result := False;

  api.hDLL := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then begin
    Result          := True;
    api.LibFilename := libFilename;
    @api.HashDLLFnIdentifyDriver := GetProcAddress(
      api.hDLL,
      DLLEXPORT_HASH_IDENTIFYDRIVER
      );
    if (@api.HashDLLFnIdentifyDriver = nil) then begin
      Result := False;
    end;
    @api.HashDLLFnIdentifySupported :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_HASH_IDENTIFYSUPPORTED
      );
    if (@api.HashDLLFnIdentifySupported = nil) then begin
      Result := False;
    end;
    @api.HashDLLFnGetHashDetails := GetProcAddress(
      api.hDLL,
      DLLEXPORT_HASH_GETHASHDETAILS
      );
    if (@api.HashDLLFnGetHashDetails = nil) then begin
      Result := False;
    end;
    @api.HashDLLFnHash := GetProcAddress(api.hDLL,
      DLLEXPORT_HASH_HASH
      );
    if (@api.HashDLLFnHash = nil) then begin
      Result := False;
    end;


    if not (Result) then begin
      FreeLibrary(api.hDLL);
    end;
  end;

end;

function TOTFEFreeOTFEDLL.LoadLibraryCachedHash(const libFilename: String;
  var api: THashAPI): Boolean;
begin
  Result := CachesGetHashAPI(libFilename, api);

  if not (Result) then begin
    Result := LoadLibraryHash(libFilename, api);
    if Result then begin
      CachesAddHashAPI(libFilename, api);
    end;
  end;

end;

procedure TOTFEFreeOTFEDLL.FreeLibraryHash(var api: THashAPI);
begin
  if (api.hDLL <> 0) then begin
    FreeLibrary(api.hDLL);
    api.hDLL        := 0;
    api.LibFilename := '';
  end;
end;

function TOTFEFreeOTFEDLL.LoadLibraryCypher(const libFilename: String;
  var api: TCypherAPI): Boolean;
begin
  Result := False;

  api.hDLL := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then begin
    Result          := True;
    api.LibFilename := libFilename;
    @api.CypherDLLFnIdentifyDriver :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_IDENTIFYDRIVER
      );
    if (@api.CypherDLLFnIdentifyDriver = nil) then begin
      Result := False;
    end;

    //    @api.CypherDLLFnIdentifySupported_v1 := GetProcAddress(
    //                                                   api.hDLL,
    //                                                   DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1
    //                                                  );
    //    if (@api.CypherDLLFnIdentifySupported_v1 = nil) then
    //      begin
    //      Result := FALSE;
    //      end;

    @api.CypherDLLFnIdentifySupported_v3 :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3
      );
    if (@api.CypherDLLFnIdentifySupported_v3 = nil) then begin
      Result := False;
    end;

    //    @api.CypherDLLFnGetCypherDetails_v1 := GetProcAddress(
    //                                                   api.hDLL,
    //                                                   DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1
    //                                                  );
    //    if (@api.CypherDLLFnGetCypherDetails_v1 = nil) then
    //      begin
    //      Result := FALSE;
    //      end;

    @api.CypherDLLFnGetCypherDetails_v3 :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3
      );
    if (@api.CypherDLLFnGetCypherDetails_v3 = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnEncrypt := GetProcAddress(
      api.hDLL,
      DLLEXPORT_CYPHER_ENCRYPT
      );
    if (@api.CypherDLLFnEncrypt = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnEncryptSector :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_ENCRYPTSECTOR
      );
    if (@api.CypherDLLFnEncryptSector = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnEncryptWithASCII :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_ENCRYPTWITHASCII
      );
    if (@api.CypherDLLFnEncryptWithASCII = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnEncryptSectorWithASCII :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII
      );
    if (@api.CypherDLLFnEncryptSectorWithASCII = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnDecrypt := GetProcAddress(
      api.hDLL,
      DLLEXPORT_CYPHER_DECRYPT
      );
    if (@api.CypherDLLFnDecrypt = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnDecryptSector :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_DECRYPTSECTOR
      );
    if (@api.CypherDLLFnDecryptSector = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnDecryptWithASCII :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_DECRYPTWITHASCII
      );
    if (@api.CypherDLLFnDecryptWithASCII = nil) then begin
      Result := False;
    end;
    @api.CypherDLLFnDecryptSectorWithASCII :=
      GetProcAddress(api.hDLL,
      DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII
      );
    if (@api.CypherDLLFnDecryptSectorWithASCII = nil) then begin
      Result := False;
    end;


    if not (Result) then begin
      FreeLibrary(api.hDLL);
    end;
  end;

end;

function TOTFEFreeOTFEDLL.LoadLibraryCachedCypher(const libFilename: String;
  var api: TCypherAPI): Boolean;
begin
  Result := CachesGetCypherAPI(libFilename, api);

  if not (Result) then begin
    Result := LoadLibraryCypher(libFilename, api);
    if Result then begin
      CachesAddCypherAPI(libFilename, api);
    end;
  end;

end;

procedure TOTFEFreeOTFEDLL.FreeLibraryCypher(var api: TCypherAPI);
begin
  if (api.hDLL <> 0) then begin
    FreeLibrary(api.hDLL);
    api.hDLL        := 0;
    api.LibFilename := '';
  end;
end;


function TOTFEFreeOTFEDLL.GetDLLDir(): String;
const
  { Subdir which should be searched for drivers
      path is <exepath>/DLLs/<config>/<Platform>
      eg .\bin\PC\DLLs\Debug\Win32
      }
  CONFIG =
    // use debug DLLS or not, debug ones are much slower
  {$IFDEF DEBUG_DLLS}
     'Debug'
  {$ELSE}
    'Release'
  {$ENDIF}
  ;
begin
  //the dll version required is based on if the *app* is 32 or 64 bit - not the OS
  Result := IncludeTrailingPathDelimiter(fExeDir) + 'DLLs\' + CONFIG + '\' + Ifthen(
    SDUApp64bit(), 'x64', 'Win32');
end;

procedure TOTFEFreeOTFEDLL.GetAllDriversUnderExeDir(driverFilenames: TStringList);
var
  fileIterator: TSDUFileIterator;
  filename:     String;
begin
  // Compile a list of drivers in the ExeDir
  fileIterator := TSDUFileIterator.Create(nil);
  try
    fileIterator.Directory          := GetDLLDir();
    fileIterator.FileMask           := '*.dll';
    fileIterator.RecurseSubDirs     := False;
    fileIterator.OmitStartDirPrefix := False;
    fileIterator.IncludeDirNames    := False;

    fileIterator.Reset();
    filename := fileIterator.Next();
    while (filename <> '') do begin
      driverFilenames.Add(filename);
      filename := fileIterator.Next();
    end;

  finally
    fileIterator.Free();
  end;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): Boolean;
var
  dllNames: TStringList;
  j:        Integer;
begin
  Result := True;

  SetLength(hashDrivers, 0);

  dllNames := TStringList.Create();
  try
    GetAllDriversUnderExeDir(dllNames);
    for j := 0 to (dllNames.Count - 1) do begin
      if pos(DLL_HASH_FILE_PREFIX, ExtractFilename(dllNames[j])) > 0 then begin
        // Get the details for the device
        SetLength(hashDrivers, (Length(hashDrivers) + 1));
        Result := GetHashDriverHashes(dllNames[j], hashDrivers[high(hashDrivers)]);
        // unicode->ansi not a data loss because only ansistring stored

        // Note: This "break" condition differs between the DLL and kernel
        //       driver versions
        if not (Result) then begin
          break;
        end;
      end;

    end;

  finally
    dllNames.Free();
  end;

end;

function TOTFEFreeOTFEDLL.GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): Boolean;
var
  dllNames: TStringList;
  j:        Integer;
begin
  Result := True;

  SetLength(cypherDrivers, 0);

  dllNames := TStringList.Create();
  try
    GetAllDriversUnderExeDir(dllNames);
    for j := 0 to (dllNames.Count - 1) do begin
      if pos(DLL_CYPHER_FILE_PREFIX, ExtractFilename(dllNames[j])) > 0 then begin
        // Get the details for the device
        SetLength(cypherDrivers, (Length(cypherDrivers) + 1));
        Result := GetCypherDriverCyphers(dllNames[j], cypherDrivers[high(cypherDrivers)]);
        // unicode->ansi not a data loss because only ansistring stored

        // Note: This "break" condition differs between the DLL and kernel
        //       driver versions
        if not (Result) then begin
          break;
        end;
      end;

    end;

  finally
    dllNames.Free();
  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHashDriverHashes(hashDriver: String;
  var hashDriverDetails: TFreeOTFEHashDriver): Boolean;
var
  DIOCBuffer:       TDIOC_HASH_IDENTIFYDRIVER;
  ptrBuffer:        Pointer;
  ptrBufferOffset:  Pointer;
  DIOCBufferHashes: PDIOC_HASH_IDENTIFYSUPPORTED;
  bufferSize:       Cardinal;
  hashDetails:      PHASH;
  i:                Integer;
  arrIdx:           Integer;
  hashAPI:          THashAPI;
begin
  Result := False;

  if CachesGetHashDriver(hashDriver, hashDriverDetails) then begin
    Result := True;
  end else begin

    DebugMsg('Connecting to: ' + hashDriver);

    if not (LoadLibraryCachedHash(hashDriver, hashAPI)) then begin

      DebugMsg('Couldn''t connect to: ' + hashDriver);

    end else begin
      if (hashAPI.HashDLLFnIdentifyDriver(@DIOCBuffer) = ERROR_SUCCESS) then begin
        hashDriverDetails.DriverGUID           := DIOCBuffer.DriverGUID;
        hashDriverDetails.LibFNOrDevKnlMdeName := hashDriver;
        //        hashDriverDetails.DeviceName := GetDeviceName(hashKernelModeDeviceName);
        //        hashDriverDetails.DeviceUserModeName := DeviceUserModeName;
        hashDriverDetails.Title                := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
        hashDriverDetails.VersionID            := DIOCBuffer.VersionID;
        hashDriverDetails.HashCount            := DIOCBuffer.HashCount;


        bufferSize := sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
          sizeof(hashDetails^) + (sizeof(hashDetails^) * hashDriverDetails.HashCount);
        // Round up to next "DIOC boundry". Although this always results in an
        // oversized buffer, it's guaranteed to be big enough.
        bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
        ptrBuffer  := allocmem(bufferSize);

        SetLength(hashDriverDetails.Hashes, hashDriverDetails.HashCount);

        if (hashAPI.HashDLLFnIdentifySupported(bufferSize, ptrBuffer) = ERROR_SUCCESS) then begin

          DIOCBufferHashes := (PDIOC_HASH_IDENTIFYSUPPORTED(ptrBuffer));
          DebugMsg('hash details count: ' + IntToStr(DIOCBufferHashes.BufCount));

          ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
            sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
            sizeof(hashDetails^));
          arrIdx          := low(hashDriverDetails.Hashes);
          for i := 1 to hashDriverDetails.HashCount do begin
            hashDetails := (PHASH(ptrBufferOffset));
            hashDriverDetails.Hashes[arrIdx].HashGUID := hashDetails.HashGUID;
            hashDriverDetails.Hashes[arrIdx].Title :=
              Copy(hashDetails.Title, 1, StrLen(hashDetails.Title));
            hashDriverDetails.Hashes[arrIdx].VersionID := hashDetails.VersionID;
            hashDriverDetails.Hashes[arrIdx].Length := hashDetails.Length;
            hashDriverDetails.Hashes[arrIdx].BlockSize := hashDetails.BlockSize;

            // Setup for the next one...
            ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + sizeof(hashDetails^));
            Inc(arrIdx);
          end;

          // Cache the information retrieved
          CachesAddHashDriver(hashDriver, hashDriverDetails);

          Result := True;
        end else begin

          DebugMsg('gethashdrivers DIOC 2 FAIL');

        end;

        FreeMem(ptrBuffer);
      end;

    end;

  end;  // if CachesGetHashDriver(hashDriver, hashDriverDetails) then

end;

 (*
// ----------------------------------------------------------------------------
// Get cypher details - using FreeOTFE v3 and later API
// Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFEDLL._GetCypherDriverCyphers_v1(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  DIOCBuffer: TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v1;
{$ENDIF}
  bufferSize: cardinal;
  cypherDetails: PCYPHER_v1;
  i: integer;
  currCypherMode: TFreeOTFECypherMode;
  arrIdx: integer;
  cypherAPI: TCypherAPI;
begin
  Result := FALSE;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then
    begin
    Result := TRUE;
    end
  else
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+cypherDriver);
{$ENDIF}
    if not(LoadLibraryCachedCypher(cypherDriver, cypherAPI)) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+cypherDriver);
{$ENDIF}
      end
    else
      begin
      if (cypherAPI.CypherDLLFnIdentifyDriver(@DIOCBuffer) = ERROR_SUCCESS) then
        begin
        cypherDriverDetails.DriverGUID  := DIOCBuffer.DriverGUID;
        cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
//          cypherDriverDetails.DeviceName  := GetDeviceName(cypherKernelModeDeviceName);
//          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
        cypherDriverDetails.Title       := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
        cypherDriverDetails.VersionID   := DIOCBuffer.VersionID;
        cypherDriverDetails.CypherCount := DIOCBuffer.CypherCount;


        bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) -
                      sizeof(cypherDetails^) +
                      (sizeof(cypherDetails^) * cypherDriverDetails.CypherCount);
        // Round up to next "DIOC boundry". Although this always results in an
        // oversized buffer, it's guaranteed to be big enough.
        bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
        ptrBuffer := allocmem(bufferSize);

        SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

        if (cypherAPI.CypherDLLFnIdentifySupported_v1(bufferSize, ptrBuffer) = ERROR_SUCCESS) then
          begin
{$IFDEF FREEOTFE_DEBUG}
DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v1(ptrBuffer));
DebugMsg('cypher details count: '+inttostr(DIOCBufferCyphers.BufCount));
{$ENDIF}
          ptrBufferOffset := Pointer(
                                     PAnsiChar(ptrBuffer) +
                                     sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v1) -
                                     sizeof(cypherDetails^)
                                    );
          arrIdx := low(cypherDriverDetails.Cyphers);
          for i:=1 to cypherDriverDetails.CypherCount do
            begin
            cypherDetails := (PCYPHER_v1(ptrBufferOffset));
            cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
            cypherDriverDetails.Cyphers[arrIdx].Title := Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

            // Failsafe to "unknown"
            cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
            for currCypherMode:=low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do
              begin
              if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then
                begin
                cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
                end;

              end;

            cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying := cypherDetails.KeySizeUnderlying;
            cypherDriverDetails.Cyphers[arrIdx].BlockSize := cypherDetails.BlockSize;
            cypherDriverDetails.Cyphers[arrIdx].VersionID := cypherDetails.VersionID;

            cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired := cypherDetails.KeySizeUnderlying;

            // Setup for the next one...
            ptrBufferOffset := Pointer(
                                       PAnsiChar(ptrBufferOffset) +
                                       sizeof(cypherDetails^)
                                      );
            inc(arrIdx);
            end;

          // Cache the information retrieved
          CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

          Result := TRUE;
          end
        else
          begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getcypherdrivers DIOC 2 FAIL');
{$ENDIF}
          end;

        FreeMem(ptrBuffer);
        end;

      end;

    end;  // if CachesGetCypherDriver(driver, cypherDriverDetails) then


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting function...');
{$ENDIF}


end;    *)

 // ----------------------------------------------------------------------------
 // Get cypher details - using FreeOTFE v3 and later API
 // Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFEDLL._GetCypherDriverCyphers_v3(cypherDriver: Ansistring;
  var cypherDriverDetails: TFreeOTFECypherDriver): Boolean;
var
  DIOCBuffer:      TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer:       Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v3;
{$ENDIF}
  bufferSize:      Cardinal;
  cypherDetails:   PCYPHER_v3;
  i:               Integer;
  currCypherMode:  TFreeOTFECypherMode;
  arrIdx:          Integer;
  cypherAPI:       TCypherAPI;
begin
  Result := False;

  if CachesGetCypherDriver(cypherDriver, cypherDriverDetails) then begin
    Result := True;
  end else begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+cypherDriver);
{$ENDIF}
    if not (LoadLibraryCachedCypher(cypherDriver, cypherAPI)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+cypherDriver);
{$ENDIF}
    end else begin
      if (cypherAPI.CypherDLLFnIdentifyDriver(@DIOCBuffer) = ERROR_SUCCESS) then begin
        cypherDriverDetails.DriverGUID           := DIOCBuffer.DriverGUID;
        cypherDriverDetails.LibFNOrDevKnlMdeName := cypherDriver;
        //          cypherDriverDetails.DeviceName  := GetDeviceName(cypherKernelModeDeviceName);
        //          cypherDriverDetails.DeviceUserModeName   := DeviceUserModeName;
        cypherDriverDetails.Title                := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
        cypherDriverDetails.VersionID            := DIOCBuffer.VersionID;
        cypherDriverDetails.CypherCount          := DIOCBuffer.CypherCount;


        bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
          sizeof(cypherDetails^) + (sizeof(cypherDetails^) *
          cypherDriverDetails.CypherCount);
        // Round up to next "DIOC boundry". Although this always results in an
        // oversized buffer, it's guaranteed to be big enough.
        bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
        ptrBuffer  := allocmem(bufferSize);

        SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

        if (cypherAPI.CypherDLLFnIdentifySupported_v3(bufferSize, ptrBuffer) = ERROR_SUCCESS) then
        begin
{$IFDEF FREEOTFE_DEBUG}
DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v3(ptrBuffer));
DebugMsg('cypher details count: '+inttostr(DIOCBufferCyphers.BufCount));
{$ENDIF}
          ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
            sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
            sizeof(cypherDetails^));
          arrIdx          := low(cypherDriverDetails.Cyphers);
          for i := 1 to cypherDriverDetails.CypherCount do begin
            cypherDetails := (PCYPHER_v3(ptrBufferOffset));
            cypherDriverDetails.Cyphers[arrIdx].CypherGUID := cypherDetails.CypherGUID;
            cypherDriverDetails.Cyphers[arrIdx].Title :=
              Copy(cypherDetails.Title, 1, StrLen(cypherDetails.Title));

            // Failsafe to "unknown"
            cypherDriverDetails.Cyphers[arrIdx].Mode := focmUnknown;
            for currCypherMode := low(FreeOTFECypherModeID) to high(FreeOTFECypherModeID) do begin
              if (cypherDetails.Mode = FreeOTFECypherModeID[currCypherMode]) then begin
                cypherDriverDetails.Cyphers[arrIdx].Mode := currCypherMode;
              end;

            end;

            cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired := cypherDetails.KeySizeRequired;
            cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying :=
              cypherDetails.KeySizeUnderlying;
            cypherDriverDetails.Cyphers[arrIdx].BlockSize       := cypherDetails.BlockSize;
            cypherDriverDetails.Cyphers[arrIdx].VersionID       := cypherDetails.VersionID;

            // Setup for the next one...
            ptrBufferOffset := Pointer(
              PAnsiChar(ptrBufferOffset) +
              sizeof(
              cypherDetails^));
            Inc(arrIdx);
          end;

          // Cache the information retrieved
          CachesAddCypherDriver(cypherDriver, cypherDriverDetails);

          Result := True;
        end else begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getcypherdrivers DIOC 2 FAIL');
{$ENDIF}
        end;

        FreeMem(ptrBuffer);
      end;

    end;

  end;  // if CachesGetCypherDriver(driver, cypherDriverDetails) then


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting function...');
{$ENDIF}

end;


 // ----------------------------------------------------------------------------
 // Hash the specified data with the supplied hash device/hash GUID
function TOTFEFreeOTFEDLL.HashData(
  hashDriver: Ansistring;
  hashGUID: TGUID;
  const data: TSDUBytes;
  out hashOut: TSDUBytes
  ): Boolean;
var
  ptrDIOCBufferIn:      PDIOC_HASH_DATA_IN;
  bufferSizeIn:         DWORD;
  ptrDIOCBufferOut:     PDIOC_HASH_DATA_OUT;
  bufferSizeOut:        DWORD;
  hashDetails:          TFreeOTFEHash;
  hashByteCount:        Integer;
  expectedHashSizeBits: Integer;  // In *bits*
  hashAPI:              THashAPI;
  bitsReturned:         DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  if GetSpecificHashDetails(hashDriver, hashGUID, hashDetails) then begin
    if not (LoadLibraryCachedHash(hashDriver, hashAPI)) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+hashDriver);
{$ENDIF}
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
    end else begin
      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(Byte) + Length(Data);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        expectedHashSizeBits := hashDetails.Length;
        if (hashDetails.Length = -1) then begin
          expectedHashSizeBits := (Length(Data) * 8);
        end;
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(Byte) + (expectedHashSizeBits div 8);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.HashGUID := hashGUID;

          ptrDIOCBufferIn.DataLength := Length(Data) * 8;
          StrMove(@ptrDIOCBufferIn.Data, PAnsiChar(Data), Length(Data));

          bitsReturned := expectedHashSizeBits;
          if (hashAPI.HashDLLFnHash(@hashGUID,
            ptrDIOCBufferIn.DataLength,
            PByte(@(ptrDIOCBufferIn.Data)),
            @bitsReturned,
            PByte(
            @(ptrDIOCBufferOut.Hash))) = ERROR_SUCCESS) then begin
            hashByteCount := (bitsReturned div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hashByteCount: '+inttostr(hashByteCount));
{$ENDIF}

            // Set hashOut so that it has enough characters which can be
            // overwritten with StrMove
            SDUInitAndZeroBuffer(hashByteCount, hashOut);
            //            hashOut := StringOfChar(Ansichar(#0), hashByteCount);
            StrMove(PAnsiChar(hashOut), @ptrDIOCBufferOut.Hash, hashByteCount);

            Result := True;
          end else begin
            LastErrorCode := OTFE_ERR_HASH_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hash DIOC 2 FAIL');
{$ENDIF}
          end;

        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;

    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to GetSpecificHashDetails');
{$ENDIF}
  end;  // ELSE PART - if GetSpecificHashDetails(hashKernelModeDeviceName, hashGUID, hashDetails) then

end;


 // ----------------------------------------------------------------------------
 // Generate MAC of data using hash driver/encryption driver identified
 // Note: "tBits" is in *bits* not *bytes*
 // tBits - Set the the number of bits to return; set to -ve value for no
 //         truncation/padding
 //         If tBits is larger than the number of bits the MAC would normally
 //         be, then the MAC will be right-padded with NULLs
 //         If tBits is set to a -ve number, then this function will return
 //         an MAC of variable length
function TOTFEFreeOTFEDLL.MACData(
  macAlgorithm: TFreeOTFEMACAlgorithm;
  HashDriver: Ansistring;
  HashGUID: TGUID;
  CypherDriver: Ansistring;
  CypherGUID: TGUID;
  var key: PasswordString;
  var data: Ansistring;
  var MACOut: Ansistring;
  tBits: Integer = -1
  ): Boolean;
var
  ptrDIOCBufferIn:  PDIOC_GENERATE_MAC_IN_PC_DLL;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_GENERATE_MAC_OUT;
  bufferSizeOut:    DWORD;
  outputByteCount:  Integer;
  tmpSizeBytes:     Integer;
  minBufferSizeOut: Integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Key) +
    Length(key) - sizeof(ptrDIOCBufferIn^.Data) +
    Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full DK length depends on
    // the algorithm, hash, etc
    tmpSizeBytes := (max(MAX_MAC_LENGTH, tBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.MAC);
    bufferSizeOut    := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut    := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.MACAlgorithm := FreeOTFEMACID[macAlgorithm];

      StringToWideChar(HashDriver, ptrDIOCBufferIn.HashDeviceName,
        (length(ptrDIOCBufferIn.HashDeviceName) - 1));
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StringToWideChar(CypherDriver, ptrDIOCBufferIn.CypherDeviceName,
        (length(ptrDIOCBufferIn.CypherDeviceName) - 1));
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.LengthWanted := tBits;

      ptrDIOCBufferIn.KeyLength  := Length(key) * 8;
      ptrDIOCBufferIn.DataLength := Length(data) * 8;
      // This may seem a little weird, but we do this because the data is
      // immediatly after the key
      StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key), Length(key));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)), PAnsiChar(data), Length(data));

      if (fDriver_API.MainDLLFnMACData(bufferSizeIn,
        ptrDIOCBufferIn,
        bufferSizeOut,
        ptrDIOCBufferOut) =
        ERROR_SUCCESS) then begin
        outputByteCount := (ptrDIOCBufferOut.MACLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

        // Set MACOut so that it has enough characters which can be
        // overwritten with StrMove
        MACOut := StringOfChar(Ansichar(#0), outputByteCount);
        StrMove(PAnsiChar(MACOut), @ptrDIOCBufferOut.MAC, outputByteCount);

        Result := True;
      end else begin
        LastErrorCode := OTFE_ERR_MAC_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC DIOC 2 FAIL');
{$ENDIF}
      end;

    finally
      FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
      FreeMem(ptrDIOCBufferOut);
    end;

  finally
    FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
    FreeMem(ptrDIOCBufferIn);
  end;

end;


 // ----------------------------------------------------------------------------
 // dkLenBits - This must *always* be >= 0
function TOTFEFreeOTFEDLL.DeriveKey(
  kdfAlgorithm: TFreeOTFEKDFAlgorithm;
  HashDriver: Ansistring;
  HashGUID: TGUID;
  CypherDriver: Ansistring;
  CypherGUID: TGUID;
  Password: TSDUBytes;
  Salt: array of Byte;
  Iterations: Integer;
  dkLenBits: Integer;  // In *bits*
  out DK: TSDUBytes
  ): Boolean;
var
  ptrDIOCBufferIn:  PDIOC_DERIVE_KEY_IN_PC_DLL;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_DERIVE_KEY_OUT;
  bufferSizeOut:    DWORD;
  outputByteCount:  Integer;
  tmpSizeBytes:     Integer;
  minBufferSizeOut: Integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();


  bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(ptrDIOCBufferIn^.Password) +
    Length(Password) - sizeof(ptrDIOCBufferIn^.Salt) +
    Length(Salt);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    tmpSizeBytes := (max(MAX_DERIVED_KEY_LENGTH, dkLenBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.DerivedKey);
    bufferSizeOut    := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut    := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.KDFAlgorithm := FreeOTFEKDFID[kdfAlgorithm];

      StringToWideChar(HashDriver, ptrDIOCBufferIn.HashDeviceName,
        (length(ptrDIOCBufferIn.HashDeviceName) - 1));
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StringToWideChar(CypherDriver, ptrDIOCBufferIn.CypherDeviceName,
        (length(ptrDIOCBufferIn.CypherDeviceName) - 1));
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.Iterations   := Iterations;
      ptrDIOCBufferIn.LengthWanted := dkLenBits;

      ptrDIOCBufferIn.PasswordLength := (Length(Password) * 8);
      ptrDIOCBufferIn.SaltLength     := (Length(Salt) * 8);
      // This may seem a little weird, but we do this because the salt is
      // immediatly after the password
      StrMove(@ptrDIOCBufferIn.Password, PAnsiChar(Password), Length(Password));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Password)) + length(Password)),
        PAnsiChar(@Salt[0]), Length(Salt));


      if (fDriver_API.MainDLLFnDeriveKey(bufferSizeIn,
        ptrDIOCBufferIn,
        bufferSizeOut,
        ptrDIOCBufferOut) =
        ERROR_SUCCESS) then begin
        outputByteCount := (ptrDIOCBufferOut.DerivedKeyLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

        // Set DK so that it has enough characters which can be
        // overwritten with StrMove
        SDUInitAndZeroBuffer(outputByteCount, DK);
        //        DK := StringOfChar(AnsiChar(#0), outputByteCount);
        //        SDUCopyArrays(DK,ptrDIOCBufferOut.DerivedKey,);
        StrMove(PAnsiChar(DK), @ptrDIOCBufferOut.DerivedKey, outputByteCount);

        Result := True;
      end else begin
        LastErrorCode := OTFE_ERR_KDF_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MAC DIOC 2 FAIL');
{$ENDIF}
      end;

    finally
      FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
      FreeMem(ptrDIOCBufferOut);
    end;

  finally
    FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
    FreeMem(ptrDIOCBufferIn);
  end;

end;


 // ----------------------------------------------------------------------------
 // encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFEDLL._EncryptDecryptData(
  encryptFlag: Boolean;
  cypherDriver: Ansistring;
  cypherGUID: TGUID;
  var key: TSDUBytes;
  var IV: Ansistring;
  var inData: Ansistring;
  var outData: Ansistring
  ): Boolean;
var
  ptrDIOCBufferIn:  PDIOC_CYPHER_DATA_IN;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut:    DWORD;
  cypherDetails:    TFreeOTFECypher_v3;
  cypherAPI:        TCypherAPI;
  dllOpOK:          Boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then begin
    if LoadLibraryCachedCypher(cypherDriver, cypherAPI) then begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+cypherDriver);
{$ENDIF}
    end else begin
      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(
        ptrDIOCBufferIn^.Key) + Length(key) -
        sizeof(ptrDIOCBufferIn^.IV) + Length(IV) -
        sizeof(ptrDIOCBufferIn^.Data) + Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) +
          Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key),
            Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)),
            PAnsiChar(IV), Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key) + length(IV)),
            PAnsiChar(inData), Length(inData));

          if encryptFlag then begin
            dllOpOK := (cypherAPI.CypherDLLFnEncrypt(
              bufferSizeIn,
              ptrDIOCBufferIn, bufferSizeOut,
              ptrDIOCBufferOut                                           )
              = ERROR_SUCCESS);
          end else begin
            dllOpOK := (cypherAPI.CypherDLLFnDecrypt(
              bufferSizeIn,
              ptrDIOCBufferIn, bufferSizeOut,
              ptrDIOCBufferOut                                           )
              = ERROR_SUCCESS);
          end;
          if dllOpOK then begin
            // Set outData so that it has enough characters which can be
            // overwritten with StrMove
            outData := StringOfChar(AnsiChar(#0), ptrDIOCBufferIn.DataLength);

            StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, ptrDIOCBufferIn.DataLength);

            Result := True;
          end else begin
            LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('cypher DIOC 2 FAIL');
{$ENDIF}
          end;


        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;

    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
  end;

end;


 // ----------------------------------------------------------------------------
 // encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFEDLL.EncryptDecryptSectorData(
  encryptFlag: Boolean;
  cypherDriver: Ansistring;
  cypherGUID: TGUID;
  SectorID: LARGE_INTEGER;
  SectorSize: Integer;
  var key: TSDUBytes;
  var IV: Ansistring;
  var inData: Ansistring;
  var outData: Ansistring
  ): Boolean;
var
  ptrDIOCBufferIn:  PDIOC_CYPHER_SECTOR_DATA_IN;
  bufferSizeIn:     DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut:    DWORD;
  cypherDetails:    TFreeOTFECypher_v3;
  cypherAPI:        TCypherAPI;
  dllOpOK:          Boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result        := False;

  CheckActive();

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then begin
    if not (LoadLibraryCachedCypher(cypherDriver, cypherAPI)) then begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+cypherDriver);
{$ENDIF}
    end else begin
      bufferSizeIn    := sizeof(ptrDIOCBufferIn^) - sizeof(
        ptrDIOCBufferIn^.Key) + Length(key) -
        sizeof(ptrDIOCBufferIn^.IV) + Length(IV) -
        sizeof(ptrDIOCBufferIn^.Data) + Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn    := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut    := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) +
          Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.SectorID   := SectorID;
          ptrDIOCBufferIn.SectorSize := SectorSize;
          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key),
            Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key)),
            PAnsiChar(IV), Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key)) + length(key) + length(IV)),
            PAnsiChar(inData), Length(inData));

          if encryptFlag then begin
            dllOpOK := (cypherAPI.CypherDLLFnEncryptSector(
              bufferSizeIn,
              ptrDIOCBufferIn, bufferSizeOut,
              ptrDIOCBufferOut                                           )
              = ERROR_SUCCESS);
          end else begin
            dllOpOK := (cypherAPI.CypherDLLFnDecryptSector(
              bufferSizeIn,
              ptrDIOCBufferIn, bufferSizeOut,
              ptrDIOCBufferOut                                           )
              = ERROR_SUCCESS);
          end;
          if dllOpOK then begin
            // Set outData so that it has enough characters which can be
            // overwritten with StrMove
            outData := StringOfChar(AnsiChar(#0), ptrDIOCBufferIn.DataLength);

            StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, ptrDIOCBufferIn.DataLength);

            Result := True;
          end else begin
            LastErrorCode := OTFE_ERR_CYPHER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('cypher DIOC 2 FAIL');
{$ENDIF}
          end;


        finally
          FillChar(ptrDIOCBufferOut^, bufferSizeOut, 0);
          FreeMem(ptrDIOCBufferOut);
        end;

      finally
        FillChar(ptrDIOCBufferIn^, bufferSizeIn, 0);
        FreeMem(ptrDIOCBufferIn);
      end;

    end;

    // If there was a problem, fallback to using v1 cypher API
    if not (Result) then begin
      Result := _EncryptDecryptData(encryptFlag,
        cypherDriver, cypherGUID,
        key, IV,
        inData, outData
        );

    end;

  end else begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.MainDLLFilename(): String;
const
  DLL_MAIN_DRIVER = 'FreeOTFE.dll';
begin
  Result := GetDLLDir() + '\' + DLL_MAIN_DRIVER;
  Result := SDUGetFinalPath(Result);
end;


 // ----------------------------------------------------------------------------
 // Attempt to mount on an existing device
function TOTFEFreeOTFEDLL.MountDiskDevice(
  deviceName: String;
  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  volumeIV: TSDUBytes;
  ReadOnly: Boolean;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  metaData: TOTFEFreeOTFEVolumeMetaData;
  offset: Int64 = 0;
  size: Int64 = 0;
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
  ): Boolean;
var
  ptrDIOCBuffer:     PDIOC_MOUNT_PC_DLL;
  bufferSize:        Integer;
  mainCypherDetails: TFreeOTFECypher_v3;
  useVolumeFlags:    Integer;
  strMetaData:       Ansistring;
  mntHandles:        TMountedHandle;
  openAccessCode:    DWORD;
  openShareMode:     DWORD;
  diskGeometry:      TSDUDiskGeometry;
  useDriveLetter:    Char;
  volumeKeyStr:      Ansistring;
begin
  Result := False;

  // Sanity check
  Assert(
    (deviceName <> ''),
    'deviceName passed into MountDiskDevice was empty string'
    );
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In MountDiskDevice');
{$ENDIF}

  useDriveLetter := deviceName[1];

  VolumeMetadataToString(metaData, strMetaData);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('MountDiskDevice called with: ');
DebugMsg('  deviceName: '+deviceName);
DebugMsg('  filename: '+volFilename);
DebugMsg('  volumeKey: ');
DebugMsg(volumeKey);
DebugMsg('  IVHashDriver: '+IVHashDriver);
DebugMsg('  IVHashGUID: '+GUIDToString(IVHashGUID));
DebugMsg('  IVCypherDriver: '+IVCypherDriver);
DebugMsg('  IVCypherGUID: '+GUIDToString(IVCypherGUID));
DebugMsg('  mainCypherDriver: '+mainCypherDriver);
DebugMsg('  mainCypherGUID: '+GUIDToString(mainCypherGUID));
DebugMsg('  sectorIVGenMethod: '+FreeOTFESectorIVGenMethodTitle[sectorIVGenMethod]);
DebugMsg('  VolumeFlags: '+inttostr(VolumeFlags));
DebugMsg('  metaData: <not shown>');
DebugMsg('  strMetaData:');
DebugMsgBinary(strMetaData);
DebugMsg('  offset: '+inttostr(offset));
DebugMsg('  size: '+inttostr(size));
{$ENDIF}

  CheckActive();


  if not (GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then begin

    exit;
  end;

  // Ensure the volumeKey's length matches that of the cypher's keysize, if
  // the cypher's keysize is >= 0
  if (mainCypherDetails.KeySizeRequired >= 0) then begin
    // THIS IS CORRECT; caters for both volumeKey>keysize and
    // volumeKey<keysize
    // Copy as much of the hash value as possible to match the key length
    volumeKeyStr := SDUBytesToString(volumeKey);

    volumeKeyStr := Copy(volumeKeyStr, 1, min((mainCypherDetails.KeySizeRequired div 8),
      length(volumeKeyStr)));
    // If the hash wasn't big enough, pad out with zeros
    volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0),
      ((mainCypherDetails.KeySizeRequired div 8) - Length(volumeKeyStr)));

    SafeSetLength(volumeKey, (mainCypherDetails.KeySizeRequired div 8));
    assert(SDUBytestostring(volumeKey) = volumeKeyStr);
  end;


  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then begin
    IVHashDriver := '';
    IVHashGUID   := StringToGUID(NULL_GUID);
  end;

  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then begin
    IVCypherDriver := '';
    IVCypherGUID   := StringToGUID(NULL_GUID);
  end;


  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize    := sizeof(ptrDIOCBuffer^) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(volumeKey)) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(volumeIV)) - sizeof(Ansichar) +
    (sizeof(Ansichar) * Length(strMetaData));
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize    := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StringToWideChar(volFilename, ptrDIOCBuffer.Filename, (length(ptrDIOCBuffer.Filename) - 1));
    ptrDIOCBuffer.DataStart := offset;

    ptrDIOCBuffer.DataEnd := 0;
    if (size > 0) then begin
      ptrDIOCBuffer.DataEnd := offset + size;
    end;

    StringToWideChar(IVHashDriver, ptrDIOCBuffer.IVHashDeviceName,
      (length(ptrDIOCBuffer.IVHashDeviceName) - 1));
    ptrDIOCBuffer.IVHashGUID := IVHashGUID;
    StringToWideChar(IVCypherDriver, ptrDIOCBuffer.IVCypherDeviceName,
      (length(ptrDIOCBuffer.IVCypherDeviceName) - 1));
    ptrDIOCBuffer.IVCypherGUID := IVCypherGUID;

    StringToWideChar(mainCypherDriver, ptrDIOCBuffer.MainCypherDeviceName,
      (length(ptrDIOCBuffer.MainCypherDeviceName) - 1));
    ptrDIOCBuffer.MainCypherGUID := mainCypherGUID;

    ptrDIOCBuffer.ReadOnly    := ReadOnly;
    ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsFile];
    if IsPartition_UserModeName(volFilename) then begin
      ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsPartition];
    end;

    useVolumeFlags := VolumeFlags;
    // Yes, this timestamp reverting is the right way around; if the bit
    // *isn't* set, the timestamps get reverted
    if GetSettings().OptRevertVolTimestamps then begin
      // Strip off bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags and not (VOL_FLAGS_NORMAL_TIMESTAMPS);
    end else begin
      // Set bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags or VOL_FLAGS_NORMAL_TIMESTAMPS;
    end;
    ptrDIOCBuffer.VolumeFlags       := useVolumeFlags;
    ptrDIOCBuffer.SectorIVGenMethod := FreeOTFESectorIVGenMethodID[sectorIVGenMethod];

    ptrDIOCBuffer.MasterKeyLength := Length(volumeKey) * 8;
    ptrDIOCBuffer.VolumeIVLength  := Length(volumeIV) * 8;
    ptrDIOCBuffer.MetaDataLength  := Length(strMetaData);
    // This may seem a little weird, but we do this because the VolumeIV and metaData are
    // immediatly after the master key
    StrMove(@ptrDIOCBuffer.MasterKey, PAnsiChar(volumeKey), Length(volumeKey));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey)) + length(volumeKey)),
      PAnsiChar(volumeIV), Length(volumeIV));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey)) + length(volumeKey) + length(volumeIV)),
      PAnsiChar(strMetaData), Length(strMetaData));


    mntHandles.DeviceHandle := fDriver_API.DSK_Init(nil, ptrDIOCBuffer);
    if (mntHandles.DeviceHandle <> 0) then begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Mounted OK!');
{$ENDIF}
      openAccessCode := (GENERIC_READ or GENERIC_WRITE);
      if ReadOnly then begin
        openAccessCode := GENERIC_READ;
      end;
      openShareMode := 0; // Don't allow sharing

      mntHandles.OpenHandle := fDriver_API.DSK_Open(
        mntHandles.DeviceHandle,
        openAccessCode,
        openShareMode
        );
      if (mntHandles.OpenHandle = 0) then begin
        fDriver_API.DSK_Deinit(mntHandles.DeviceHandle);
      end else begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Opened OK!');
{$ENDIF}
        mntHandles.BytesPerSector := 0;
        // Store tmp handles...
        AddHandles(useDriveLetter, mntHandles);
        if _GetDiskGeometry(useDriveLetter, diskGeometry) then begin
          // Cache the disk geometry...
          AddDiskGeometry(useDriveLetter, diskGeometry);

          // Add the bytes per sector, then delete and re-store the handles
          mntHandles.BytesPerSector := diskGeometry.BytesPerSector;
          DeleteHandles(useDriveLetter);
          AddHandles(useDriveLetter, mntHandles);

          Result := True;
        end else begin
          DismountDiskDevice(useDriveLetter, False);
        end;
      end;
    end;

  finally
    FreeMem(ptrDIOCBuffer);
  end;


{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Exiting MountDiskDevice');
{$ENDIF}

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.CreateMountDiskDevice(
  volFilename: String;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  volumeIV: TSDUBytes;
  ReadOnly: Boolean;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: Integer;
  DriveLetter: Char;
  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
  offset: Int64 = 0;
  size: Int64 = 0;
  MetaData_LinuxVolume: Boolean = False;               // Linux volume
  MetaData_PKCS11SlotID: Integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
  MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;
  // PC kernel drivers *only* - ignored otherwise
  mountForAllUsers: Boolean =
  True  // PC kernel drivers *only* - ignored otherwise
  ): Boolean;
var
  mountMetadata: TOTFEFreeOTFEVolumeMetaData;
begin
  Result := False;

  PopulateVolumeMetadataStruct(
    MetaData_LinuxVolume,
    MetaData_PKCS11SlotID,
    mountMetadata
    );

  // Attempt to mount the device
  if MountDiskDevice(DriveLetter, volFilename,
    volumeKey, sectorIVGenMethod,
    volumeIV, ReadOnly, IVHashDriver,
    IVHashGUID, IVCypherDriver,
    IVCypherGUID, mainCypherDriver,
    mainCypherGUID, VolumeFlags,
    mountMetadata, offset, size,
    FreeOTFEMountAsStorageMediaType[MountMountAs]) then begin
    Result := True;
  end;

end;


 // ----------------------------------------------------------------------------
 // Attempt to dismount a device
function TOTFEFreeOTFEDLL.DismountDiskDevice(deviceName: String; emergency: Boolean): Boolean;
var
  DIOCBuffer:     TDIOC_FORCE_DISMOUNTS;
  bytesReturned:  DWORD;
  handles:        TMountedHandle;
  useDriveLetter: Char;
begin
  Result := False;

  CheckActive();

  // Sanity check...
  Assert(
    (deviceName <> ''),
    'deviceName set to empty string in call to DismountDiskDevice'
    );
  { TODO 1 -otdk -cclean : pass in just drive letter }
  useDriveLetter := deviceName[1];

  if GetHandles(useDriveLetter, handles) then begin
    if emergency then begin
      DIOCBuffer.ForceDismounts := emergency;
      fDriver_API.DSK_IOControl(
        handles.OpenHandle,
        IOCTL_FREEOTFE_SET_FORCE_DISMOUNT,
        @DIOCBuffer,
        sizeof(DIOCBuffer),
        nil,
        0, @bytesReturned
        );
    end;

    fDriver_API.DSK_Close(handles.OpenHandle);
    fDriver_API.DSK_Deinit(handles.DeviceHandle);

    DeleteDiskGeometry(useDriveLetter);
    DeleteHandles(useDriveLetter);

    Result := True;
  end;

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.AddHandles(driveLetter: Char; handles: TMountedHandle);
var
  ptrMountedHandles: PMountedHandle;
begin
  new(ptrMountedHandles);
  ptrMountedHandles^ := handles;
  fMountedHandles.AddObject(driveLetter, TObject(ptrMountedHandles));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHandles(driveLetter: Char; var handles: TMountedHandle): Boolean;
var
  ptrMountedHandles: PMountedHandle;
  idx:               Integer;
begin
  Result := False;

  idx := fMountedHandles.IndexOf(uppercase(driveLetter));
  if (idx >= 0) then begin
    ptrMountedHandles := PMountedHandle(fMountedHandles.Objects[idx]);
    handles           := ptrMountedHandles^;
    Result            := True;
  end;

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.DeleteHandles(driveLetter: Char);
var
  ptrMountedHandles: PMountedHandle;
  idx:               Integer;
begin
  idx := fMountedHandles.IndexOf(driveLetter);
  if (idx >= 0) then begin
    ptrMountedHandles := PMountedHandle(fMountedHandles.Objects[idx]);
    Dispose(ptrMountedHandles);
    fMountedHandles.Delete(idx);
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetNextDriveLetter(userDriveLetter, requiredDriveLetter: Char): Char;
var
  c: Char;
begin
  Result := #0;
  for c := 'A' to 'Z' do begin
    if (fMountedHandles.IndexOf(c) < 0) then begin
      Result := c;
      break;
    end;
  end;

  // Sanity check; although this isn't actually true (we could go onto numbers,
  // lowercase, etc), it is if we keep with the drive letter paradigm...
  Assert(
    (Result <> #0),
    'Unable to open more then 26 volumes at the same time'
    );

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.DriverType(): String;
begin
  Result := _('DLL driver');
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetVolumeInfo(driveLetter: Char;
  var volumeInfo: TOTFEFreeOTFEVolumeInfo): Boolean;
var
  BytesReturned:        DWORD;
  outBuffer:            TDIOC_DISK_DEVICE_STATUS_PC_DLL;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  handles:              TMountedHandle;
begin
  Result := False;

  CheckActive();

  driveLetter := upcase(driveLetter);

  if GetHandles(driveLetter, handles) then begin
    if fDriver_API.DSK_IOControl(handles.OpenHandle,
      IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS,
      nil, 0,
      @outBuffer, sizeof(outBuffer),
      @BytesReturned) then begin
      if (BytesReturned <= sizeof(outBuffer)) then begin
        volumeInfo.Filename         := Copy(outBuffer.Filename, 1, SDUWStrLen(outBuffer.Filename));
        { TODO 1 -otdk -cinvestigate : what if unicode filename? }
        volumeInfo.DeviceName       := driveLetter;
        volumeInfo.IVHashDevice     :=
          Copy(outBuffer.IVHashDeviceName, 1, SDUWStrLen(outBuffer.IVHashDeviceName));
        { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.IVHashGUID       := outBuffer.IVHashGUID;
        volumeInfo.IVCypherDevice   :=
          Copy(outBuffer.IVCypherDeviceName, 1, SDUWStrLen(outBuffer.IVCypherDeviceName));
        { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.IVCypherGUID     := outBuffer.IVCypherGUID;
        volumeInfo.MainCypherDevice :=
          Copy(outBuffer.MainCypherDeviceName, 1, SDUWStrLen(outBuffer.MainCypherDeviceName));
        { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.MainCypherGUID   := outBuffer.MainCypherGUID;
        volumeInfo.Mounted          := True;
        volumeInfo.ReadOnly         := outBuffer.ReadOnly;
        volumeInfo.VolumeFlags      := outBuffer.VolumeFlags;

        volumeInfo.SectorIVGenMethod := foivgUnknown;
        for tmpSectorIVGenMethod :=
          low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do begin
          if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = outBuffer.SectorIVGenMethod) then
          begin
            volumeInfo.SectorIVGenMethod := tmpSectorIVGenMethod;
          end;
        end;

        volumeInfo.DriveLetter := AnsiChar(driveLetter);
        // pos can be char - see todo in volinfo struct

        // Functionality in DLL/PDA driver to retrieve metadata not currently implemented!
        Result := True;
        //        Result := GetVolumeMetaData(driveLetter, outBuffer.MetaDataLength, volumeInfo.MetaData);
        //        volumeInfo.MetaDataStructValid := ParseVolumeMetadata(
        //                                                              volumeInfo.MetaData,
        //                                                              volumeInfo.MetaDataStruct
        //                                                             );
      end;

    end;

  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Seek(driveLetter: Char; offset: DWORD): Boolean;
var
  handles: TMountedHandle;
begin
  Result := False;

  CheckActive();

  if GetHandles(driveLetter, handles) then begin
    if (fDriver_API.DSK_Seek(handles.OpenHandle,
      offset, FILE_BEGIN
      ) =
      ERROR_SUCCESS) then begin
    end;
  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.ReadData_Sectors(
  DriveLetter: Char;
  SectorNoStart: DWORD;
  CountSectors: DWORD;
  stm: TStream): Boolean;
var
  handles:       TMountedHandle;
  ptrBuffer:     PByte;
  sgReq:         TSG_REQ;
  bufferSize:    DWORD;
  bytesReturned: DWORD;
begin
  Result := False;

  CheckActive();

  if GetHandles(driveLetter, handles) then begin
    bufferSize := (CountSectors * handles.BytesPerSector);
    ptrBuffer  := AllocMem(bufferSize);
    try
      sgReq.sr_start            := sectorNoStart;
      sgReq.sr_num_sec          := countSectors;
      sgReq.sr_num_sg           := 1;  // Only 1 SG buffer
      sgReq.sr_status           := ERROR_SUCCESS;
      sgReq.sr_callback         := nil;
      sgReq.sr_sglist[1].sb_len := bufferSize;
      sgReq.sr_sglist[1].sb_buf := ptrBuffer;

      if fDriver_API.DSK_IOControl(handles.OpenHandle,
        IOCTL_DISK_READ,
        // DISK_IOCTL_READ, - old const; changed to IOCTL_DISK_READ in WinCE 3.0
        @sgReq, sizeof(sgReq),
        nil, 0,
        @bytesReturned) then begin
        if (sgReq.sr_status = ERROR_SUCCESS) then begin
          stm.Write(ptrBuffer^, bufferSize);
          Result := True;
        end;
      end;
    finally
      FreeMem(ptrBuffer);
    end;
  end;

end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.WriteData_Sectors(
  DriveLetter: Char;
  SectorNoStart: DWORD;
  CountSectors: DWORD;
  stm: TStream): Boolean;
var
  handles:       TMountedHandle;
  ptrBuffer:     PByte;
  sgReq:         TSG_REQ;
  bufferSize:    DWORD;
  bytesReturned: DWORD;
begin
  Result := False;

  CheckActive();

  if GetHandles(driveLetter, handles) then begin
    bufferSize := (CountSectors * handles.BytesPerSector);
    ptrBuffer  := AllocMem(bufferSize);
    try
      stm.Read(ptrBuffer^, bufferSize);

      sgReq.sr_start            := sectorNoStart;
      sgReq.sr_num_sec          := countSectors;
      sgReq.sr_num_sg           := 1;  // Only 1 SG buffer
      sgReq.sr_status           := ERROR_SUCCESS;
      sgReq.sr_callback         := nil;
      sgReq.sr_sglist[1].sb_len := bufferSize;
      sgReq.sr_sglist[1].sb_buf := ptrBuffer;

      if fDriver_API.DSK_IOControl(handles.OpenHandle,
        IOCTL_DISK_WRITE,
        // DISK_IOCTL_WRITE, - old const; changed to IOCTL_DISK_WRITE in WinCE 3.0
        @sgReq, sizeof(sgReq),
        nil, 0,
        @bytesReturned) then begin
        if (sgReq.sr_status = ERROR_SUCCESS) then begin
          Result := True;
        end;
      end;
    finally
      FreeMem(ptrBuffer);
    end;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.ReadData_Bytes(
  DriveLetter: Char;
  Offset: ULONGLONG;
  DataLength: ULONGLONG;
  Data: TStream
  ): Boolean;
var
  diskGeometry: TSDUDiskGeometry;
  sectorID_i64: Int64;
  sectorID:     DWORD;

  cntSectors: DWORD;

  partialSector: Boolean;
  tmpStream:     TSDUMemoryStream;
begin
  Result := True;

  // Short circuit...
  if (DataLength = 0) then begin
    Result := True;
    exit;
  end;

  if Result then begin
    Result := GetDiskGeometry(DriveLetter, diskGeometry);
  end;

  sectorID := 0;
  if Result then begin
    if (Offset mod Int64(diskGeometry.BytesPerSector) <> 0) then begin
      //lplp - xxx - eliminate this restriction
      ShowMessage('NON-SECTOR (offset + length) READS must start from a sector offset in current implementation');
      Result := False;
    end;

    sectorID_i64 := Offset div Int64(diskGeometry.BytesPerSector);
    sectorID     := sectorID_i64 and $FFFFFFFF;
  end;

  if Result then begin
    // If the amount of data to transfer isn't a multiple of the sector size, we
    // have to use a temp stream to hold the data 
    partialSector := ((DataLength mod diskGeometry.BytesPerSector) <> 0);
    cntSectors    := (DataLength div diskGeometry.BytesPerSector);
    if partialSector then begin
      Inc(cntSectors);
      tmpStream := TSDUMemoryStream.Create();
      try
        Result             := ReadData_Sectors(DriveLetter, sectorID, cntSectors, tmpStream);
        tmpStream.Position := 0;
        Data.CopyFrom(tmpStream, DataLength);
      finally
        tmpStream.Free();
      end;

    end else begin
      Result := ReadData_Sectors(DriveLetter, sectorID, cntSectors, Data);
    end;

  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.WriteData_Bytes(
  DriveLetter: Char;
  Offset: ULONGLONG;
  DataLength: ULONGLONG;
  Data: TStream
  ): Boolean;
var
  diskGeometry: TSDUDiskGeometry;
  sectorID_i64: Int64;
  sectorID:     DWORD;

  cntSectors: DWORD;

  partialSector: Boolean;
  tmpStream:     TSDUMemoryStream;
begin
  Result := True;

  // Short circuit...
  if (DataLength = 0) then begin
    Result := True;
    exit;
  end;

  if Result then begin
    Result := GetDiskGeometry(DriveLetter, diskGeometry);
  end;

  sectorID := 0;
  if Result then begin
    if (Offset mod Int64(diskGeometry.BytesPerSector) <> 0) then begin
      //lplp - xxx - eliminate this restriction
      ShowMessage('NON-SECTOR (offset + length) WRITES must start from a sector offset in current implementation');
      Result := False;
    end;

    sectorID_i64 := Offset div Int64(diskGeometry.BytesPerSector);
    sectorID     := sectorID_i64 and $FFFFFFFF;
  end;

  if Result then begin
    // If the amount of data to transfer isn't a multiple of the sector size, we
    // have to use a temp stream to hold the data
    partialSector := ((DataLength mod diskGeometry.BytesPerSector) <> 0);
    cntSectors    := (DataLength div diskGeometry.BytesPerSector);
    if partialSector then begin
      tmpStream := TSDUMemoryStream.Create();
      try
        Inc(cntSectors);
        // In order to preserve the data previously written to the sector's slack
        // space, read in the previous data and overlay the new data onto it.
        if ReadData_Sectors(DriveLetter,
          sectorID, cntSectors,
          tmpStream) then begin
          tmpStream.Position := 0;
          tmpStream.CopyFrom(Data, DataLength);
          tmpStream.Position := 0;
          Result             := WriteData_Sectors(DriveLetter, sectorID, cntSectors, tmpStream);
        end;

      finally
        tmpStream.Free();
      end;

    end else begin
      Result := WriteData_Sectors(DriveLetter, sectorID, cntSectors, Data);
    end;
  end;

end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL._GetDiskGeometry(
  DriveLetter: Char;
  var diskGeometry: TSDUDiskGeometry): Boolean;
var
  handles:       TMountedHandle;
  bytesReturned: DWORD;
begin
  Result := False;

  CheckActive();

  if GetHandles(driveLetter, handles) then begin
    if fDriver_API.DSK_IOControl(handles.OpenHandle,
      IOCTL_DISK_GETINFO, nil,
      0, @diskGeometry,
      sizeof(diskGeometry),
      @bytesReturned) then begin
      Result := True;
    end;
  end;

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.AddDiskGeometry(driveLetter: Char; diskGeometry: TSDUDiskGeometry);
var
  ptrDiskGeometry: PSDUDiskGeometry;
begin
  new(ptrDiskGeometry);
  ptrDiskGeometry^ := diskGeometry;
  fCachedDiskGeometry.AddObject(driveLetter, TObject(ptrDiskGeometry));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetDiskGeometry(driveLetter: Char;
  var diskGeometry: TSDUDiskGeometry): Boolean;
var
  ptrDiskGeometry: PSDUDiskGeometry;
  idx:             Integer;
begin
  Result := False;

  idx := fCachedDiskGeometry.IndexOf(uppercase(driveLetter));
  if (idx >= 0) then begin
    ptrDiskGeometry := PSDUDiskGeometry(fCachedDiskGeometry.Objects[idx]);
    diskGeometry    := ptrDiskGeometry^;
    Result          := True;
  end;

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.DeleteDiskGeometry(driveLetter: Char);
var
  ptrDiskGeometry: PSDUDiskGeometry;
  idx:             Integer;
begin
  idx := fCachedDiskGeometry.IndexOf(driveLetter);
  if (idx >= 0) then begin
    ptrDiskGeometry := PSDUDiskGeometry(fCachedDiskGeometry.Objects[idx]);
    Dispose(ptrDiskGeometry);
    fCachedDiskGeometry.Delete(idx);
  end;

end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesCreate();
begin
  inherited;
  fCachedHashAPI   := TStringList.Create();
  fCachedCypherAPI := TStringList.Create();

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesDestroy();
var
  i: Integer;
begin
  inherited;

  for i := 0 to (fCachedHashAPI.Count - 1) do begin
    if (fCachedHashAPI.Objects[i] <> nil) then begin
      FreeLibraryHash((PHashAPI(fCachedHashAPI.Objects[i]))^);
      Dispose(PHashAPI(fCachedHashAPI.Objects[i]));
    end;
  end;
  fCachedHashAPI.Free();

  for i := 0 to (fCachedCypherAPI.Count - 1) do begin
    if (fCachedCypherAPI.Objects[i] <> nil) then begin
      FreeLibraryCypher((PCypherAPI(fCachedCypherAPI.Objects[i]))^);
      Dispose(PCypherAPI(fCachedCypherAPI.Objects[i]));
    end;
  end;
  fCachedCypherAPI.Free();

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesAddHashAPI(const libFilename: String; const api: THashAPI);
var
  ptrRec: PHashAPI;
begin
  new(ptrRec);
  ptrRec^ := api;
  fCachedHashAPI.AddObject(libFilename, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.CachesGetHashAPI(const libFilename: String; var api: THashAPI): Boolean;
var
  ptrRec:   PHashAPI;
  idx:      Integer;
  cacheHit: Boolean;
begin
  cacheHit := False;

  idx := fCachedHashAPI.IndexOf(libFilename);
  if (idx >= 0) then begin
    ptrRec   := PHashAPI(fCachedHashAPI.Objects[idx]);
    api      := ptrRec^;
    cacheHit := True;
  end;

  Result := cacheHit;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesAddCypherAPI(const libFilename: String; const api: TCypherAPI);
var
  ptrRec: PCypherAPI;
begin
  new(ptrRec);
  ptrRec^ := api;
  fCachedCypherAPI.AddObject(libFilename, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.CachesGetCypherAPI(const libFilename: String;
  var api: TCypherAPI): Boolean;
var
  ptrRec:   PCypherAPI;
  idx:      Integer;
  cacheHit: Boolean;
begin
  cacheHit := False;

  idx := fCachedCypherAPI.IndexOf(libFilename);
  if (idx >= 0) then begin
    ptrRec   := PCypherAPI(fCachedCypherAPI.Objects[idx]);
    api      := ptrRec^;
    cacheHit := True;
  end;

  Result := cacheHit;
end;

{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFEDLL: TOTFEFreeOTFEDLL;
begin
  assert(GetFreeOTFEBase is TOTFEFreeOTFEDLL, 'call SetFreeOTFEType with correct type');
  Result := GetFreeOTFEBase as TOTFEFreeOTFEDLL;
end;

end.
