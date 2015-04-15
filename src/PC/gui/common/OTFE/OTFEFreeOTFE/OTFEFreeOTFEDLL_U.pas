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
  Classes, Windows,
  SDUGeneral,
  OTFE_U,
  OTFEFreeOTFEBase_U,
  pkcs11_session,
  OTFEFreeOTFE_PKCS11,
  FreeOTFEDLLMainAPI,
  FreeOTFEDLLHashAPI,
  FreeOTFEDLLCypherAPI,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_DriverHashAPI,
  OTFEFreeOTFE_DriverCypherAPI;

type
  TMainAPI = record
    LibFilename: string;
    hDLL: THandle;

    DSK_Init: PDSK_Init;
    DSK_Deinit: PDSK_Deinit;
    DSK_Open: PDSK_Open;
    DSK_Close: PDSK_Close;
    DSK_IOControl: PDSK_IOControl;
    DSK_Seek: PDSK_Seek;
    DSK_Read: PDSK_Read;
    DSK_Write: PDSK_Write;
    DSK_PowerDown: PDSK_PowerDown;
    DSK_PowerUp: PDSK_PowerUp;

    MainDLLFnDeriveKey: PMainDLLFnDeriveKey;
    MainDLLFnMACData: PMainDLLFnMACData;
  end;

  THashAPI = record
    LibFilename: string;
    hDLL: THandle;

    HashDLLFnIdentifyDriver: PHashDLLFnIdentifyDriver;
    HashDLLFnIdentifySupported: PHashDLLFnIdentifySupported;
    HashDLLFnGetHashDetails: PHashDLLFnGetHashDetails;
    HashDLLFnHash: PHashDLLFnHash;
  end;
  PHashAPI = ^THashAPI;

  TCypherAPI = record
    LibFilename: string;
    hDLL: THandle;

    CypherDLLFnIdentifyDriver: PCypherDLLFnIdentifyDriver;
    CypherDLLFnIdentifySupported_v1: PCypherDLLFnIdentifySupported_v1;
    CypherDLLFnIdentifySupported_v3: PCypherDLLFnIdentifySupported_v3;
    CypherDLLFnGetCypherDetails_v1: PCypherDLLFnGetCypherDetails_v1;
    CypherDLLFnGetCypherDetails_v3: PCypherDLLFnGetCypherDetails_v3;
    CypherDLLFnEncrypt: PCypherDLLFnEncrypt;
    CypherDLLFnEncryptSector: PCypherDLLFnEncryptSector;
    CypherDLLFnEncryptWithASCII: PCypherDLLFnEncryptWithASCII;
    CypherDLLFnEncryptSectorWithASCII: PCypherDLLFnEncryptSectorWithASCII;
    CypherDLLFnDecrypt: PCypherDLLFnDecrypt;
    CypherDLLFnDecryptSector: PCypherDLLFnDecryptSector;
    CypherDLLFnDecryptWithASCII: PCypherDLLFnDecryptWithASCII;
    CypherDLLFnDecryptSectorWithASCII: PCypherDLLFnDecryptSectorWithASCII;
  end;
  PCypherAPI = ^TCypherAPI;

  TMountedHandle = record
    DeviceHandle: DWORD;
    OpenHandle: DWORD;

    BytesPerSector: DWORD;
  end;
  PMountedHandle = ^TMountedHandle;

  TOTFEFreeOTFEDLL = class(TOTFEFreeOTFEBase)
  private
    FExeDir: string;
    function GetDLLDir: string;

  protected
    DriverAPI: TMainAPI;
    fMountedHandles: TStringList;// in unicdoe
    fCachedDiskGeometry: TStringList;

    // Cached information: The strings in the list are DLL path and filenames,
    // the objects are THashAPI/TCypherAPI
    fCachedHashAPI: TStringList;
    fCachedCypherAPI: TStringList;


    function  Connect(): boolean; override;
    function  Disconnect(): boolean; override;

    procedure AddHandles(driveLetter: char; handles: TMountedHandle);
    function  GetHandles(driveLetter: char; var handles: TMountedHandle): boolean;
    procedure DeleteHandles(driveLetter: char);

    procedure AddDiskGeometry(driveLetter: char; diskGeometry: TSDUDiskGeometry);
    procedure DeleteDiskGeometry(driveLetter: char);
    // Internal use only; callers should normally use GetDiskGeometry(...)
    // (without prefixing "_") instead
    function _GetDiskGeometry(
                             DriveLetter: char;
                             var diskGeometry: TSDUDiskGeometry
                           ): boolean;

    function _GetCypherDriverCyphers_v1(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; override;
    function _GetCypherDriverCyphers_v3(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean; override;

    procedure GetAllDriversUnderExeDir(driverFilenames: TStringList);

    function  LoadLibraryMain(var api: TMainAPI): boolean;
    procedure FreeLibraryMain(var api: TMainAPI);
    function  LoadLibraryHash(const libFilename: string; var api: THashAPI): boolean;
    function  LoadLibraryCachedHash(const libFilename: string; var api: THashAPI): boolean;
    procedure FreeLibraryHash(var api: THashAPI);
    function  LoadLibraryCypher(const libFilename: string; var api: TCypherAPI): boolean;
    function  LoadLibraryCachedCypher(const libFilename: string; var api: TCypherAPI): boolean;
    procedure FreeLibraryCypher(var api: TCypherAPI);

    function  MainDLLFilename(): string;

    // ---------
    // FreeOTFE *disk* *device* management functions
    // These talk directly to the disk devices to carrry out operations

    function  MountDiskDevice(
                              deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              metaData: TOTFEFreeOTFEVolumeMetaData;
                              offset: int64 = 0;
                              size: int64 = 0;
                              storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; override;

    function  CreateMountDiskDevice(
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              DriveLetter: char;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              offset: int64 = 0;
                              size: int64 = 0;
                              MetaData_LinuxVolume: boolean = FALSE;  // Linux volume
                              MetaData_PKCS11SlotID: integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
                              MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;  // PC kernel drivers *only* - ignored otherwise
                              mountForAllUsers: boolean = TRUE  // PC kernel drivers *only* - ignored otherwise
                             ): boolean; override;

    function  DismountDiskDevice(
                                 deviceName: string;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                                 emergency: boolean
                                ): boolean; override;


    // ---------
    // Raw volume access functions

    function ReadWritePlaintextToVolume(
                    readNotWrite: boolean;
                    volFilename: string;
                    volumeKey: TSDUBytes;
                    sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                    IVHashDriver: Ansistring;
                    IVHashGUID: TGUID;
                    IVCypherDriver: Ansistring;
                    IVCypherGUID: TGUID;
                    mainCypherDriver: Ansistring;
                    mainCypherGUID: TGUID;
                    VolumeFlags: integer;
                    mountMountAs: TFreeOTFEMountAs;

                    dataOffset: int64;  // Offset from within mounted volume from where to read/write data
                    dataLength: integer;  // Length of data to read/write. In bytes
                    var data: TSDUBytes;  // Data to read/write

                    offset: int64 = 0;
                    size: int64 = 0;
                    storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                  ): boolean; override;



    // ---------
    // Internal caching functions
    procedure CachesCreate(); override;
    procedure CachesDestroy(); override;
    procedure CachesAddHashAPI(const libFilename: string; const api: THashAPI);
    function  CachesGetHashAPI(const libFilename: string; var api: THashAPI): boolean;
    procedure CachesAddCypherAPI(const libFilename: string; const api: TCypherAPI);
    function  CachesGetCypherAPI(const libFilename: string; var api: TCypherAPI): boolean;


    // ---------
    // Misc functions

    function  GetNextDriveLetter(userDriveLetter, requiredDriveLetter: char): char; overload; override;

    function  DriverType(): string; override;

  public
    // -----------------------------------------------------------------------
    // TOTFE standard API
    constructor Create(); override;
    destructor  Destroy; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    function  Version(): cardinal; overload; override;
    function  DrivesMounted(): string; overload; override;


    // -----------------------------------------------------------------------
    // TOTFEFreeOTFEBase standard API

    function  GetVolumeInfo(driveLetter: char; var volumeInfo: TOTFEFreeOTFEVolumeInfo): boolean; override;

    function GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): boolean; override;
    function GetHashDriverHashes(hashDriver: ansistring; var hashDriverDetails: TFreeOTFEHashDriver): boolean; override;

    function GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): boolean; override;


    function _EncryptDecryptData(
                                encryptFlag: boolean;
                                cypherDriver: ansistring;
                                cypherGUID: TGUID;
                                var key: TSDUBytes;
                                var IV: Ansistring;
                                var inData: Ansistring;
                                var outData: Ansistring
                               ): boolean; override;

    function EncryptDecryptSectorData(
                                encryptFlag: boolean;
                                cypherDriver: ansistring;
                                cypherGUID: TGUID;
                                SectorID: LARGE_INTEGER;
                                SectorSize: integer;
                                var key: TSDUBytes;
                                var IV: Ansistring;
                                var inData: Ansistring;
                                var outData: Ansistring
                               ): boolean; override;
                               
    function HashData(
                      hashDriver: Ansistring;
                      hashGUID: TGUID;
                      const data: TSDUBytes;
                      out hashOut: TSDUBytes
                     ): boolean; override;

    function MACData(
                      macAlgorithm: TFreeOTFEMACAlgorithm;
                      HashDriver: Ansistring;
                      HashGUID: TGUID;
                      CypherDriver: Ansistring;
                      CypherGUID: TGUID;
                      var key: PasswordString;
                      var data: Ansistring;
                      var MACOut: Ansistring;
                      tBits: integer = -1
                     ): boolean; override;

    function DeriveKey(
                    kdfAlgorithm: TFreeOTFEKDFAlgorithm;
                    HashDriver: Ansistring;
                    HashGUID: TGUID;
                    CypherDriver: Ansistring;
                    CypherGUID: TGUID;
                    Password: TSDUBytes;
                    Salt: array of byte;
                    Iterations: integer;
                    dkLenBits: integer;  // In *bits*
                    out DK: TSDUBytes
                   ): boolean; override;

                   
    // -----------------------------------------------------------------------
    // Extended FreeOTFE DLL specific functions

    function Seek(driveLetter: char; offset: DWORD): boolean;
    function ReadData_Sectors(
                      DriveLetter: char;
                      SectorNoStart: DWORD;
                      CountSectors: DWORD;
                      stm: TStream
                    ): boolean;
    function WriteData_Sectors(
                      DriveLetter: char;
                      SectorNoStart: DWORD;
                      CountSectors: DWORD;
                      stm: TStream
                    ): boolean;
    function ReadData_Bytes(
                      DriveLetter: char;
                      Offset: ULONGLONG;
                      DataLength: ULONGLONG;
                      Data: TStream
                    ): boolean;
    function WriteData_Bytes(
                      DriveLetter: char;
                      Offset: ULONGLONG;
                      DataLength: ULONGLONG;
                      Data: TStream
                    ): boolean;
    function GetDiskGeometry(
                             DriveLetter: char;
                             var diskGeometry: TSDUDiskGeometry
                           ): boolean;

  published
    // The directory under which the "DLL" dir resides
//    property ExeDir: string read FExeDir ;
  end;

{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFEDLL :  TOTFEFreeOTFEDLL;

implementation

uses
// delphi
dialogs,
  SysUtils, Math, Controls, Forms,
  strutils,
  //sdu
  SDUSysUtils,

  SDUi18n,
  SDUClasses,
  SDUFileIterator_U,
  //doxbox
   OTFEConsts_U,
  OTFEFreeOTFE_frmKeyEntryFreeOTFE,
  OTFEFreeOTFE_frmKeyEntryLinux,
  OTFEFreeOTFE_frmKeyEntryLUKS,
  OTFEFreeOTFE_LUKSAPI,
  OTFEFreeOTFE_VolumeFileAPI;

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
  fMountedHandles := TStringList.Create();
  fCachedDiskGeometry:= TStringList.Create();

  fExeDir := ExtractFilePath(Application.ExeName);
end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFEDLL.Destroy();
var
  i: integer;
  currDrvStr: string;
  currDrvChar: char;
begin
  for i:=0 to (fCachedDiskGeometry.Count-1) do
    begin
    currDrvStr := fCachedDiskGeometry[i];      // unicode->ansi not a data loss because only ansistring stored
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
function TOTFEFreeOTFEDLL.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
//  unableToOpenDrive: boolean;
begin
  CheckActive();

  Result := TRUE;

  if (Result) then
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo');
{$ENDIF}
    Result := GetVolumeInfo(driveLetter, volumeInfo);

    // If we couldn't get the drive info, then we can't even do an
    // emergency dismount; we don't know which FreeOTFE device to dismount
    if not Result then       begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('getVolumeInfo NOT Result');
{$ENDIF}
      emergency := FALSE;
      end;
    end;


  if (Result or emergency) then    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('dismountdevice');
{$ENDIF}
    Result := DismountDiskDevice(driveLetter, emergency);
    end;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Version(): cardinal;
var
  majorVersion, minorVersion, revisionVersion, buildVersion: integer;
begin
  Result := VERSION_ID_FAILURE;

  CheckActive();

  // If we have the version ID cached, use the cached information
  if (fCachedVersionID <> VERSION_ID_NOT_CACHED) then
    begin
    Result := fCachedVersionID;
    end
  else
    begin
    if SDUGetVersionInfo(
                         MainDLLFilename(),
                         majorVersion,
                         minorVersion,
                         revisionVersion,
                         buildVersion
                        ) then
      begin
      Result := (
                 (majorVersion shl 24) +
                 (minorVersion shl 16) +
                 (revisionVersion)
                );
      fCachedVersionID := Result;
      end;

    end;




end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.DrivesMounted(): string;
var
  i: integer;
begin
  // DLL version doesn't have concept of drive letters as in system drive
  // letters, but uses them to reference different mounted volumes
  Result := '';
  for i:=0 to (fMountedHandles.count - 1) do     begin
    Result := Result + fMountedHandles[i];   // unicode->ansi not a data loss because only ansistring stored
    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Connect(): boolean;
begin
  Result := FALSE;

  if LoadLibraryMain(DriverAPI) then
    begin
    Result := TRUE;
    end
  else
    begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.Disconnect(): boolean;
begin
  FreeLibraryMain(DriverAPI);
  Result:= TRUE;
end;


// ----------------------------------------------------------------------------
// Use the FreeOTFE device driver to mount the specified volume, and either:
//   *) Read in and decrypt specified data, returning the decrypted version
//   *) Encrypt specified data, and write to volume
// readNotWrite - Set to TRUE to read, FALSE to write
function TOTFEFreeOTFEDLL.ReadWritePlaintextToVolume(
  readNotWrite: boolean;

  volFilename: string;
  volumeKey: TSDUBytes;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  IVHashDriver: Ansistring;
  IVHashGUID: TGUID;
  IVCypherDriver: Ansistring;
  IVCypherGUID: TGUID;
  mainCypherDriver: Ansistring;
  mainCypherGUID: TGUID;
  VolumeFlags: integer;
  mountMountAs: TFreeOTFEMountAs;

  dataOffset: int64;  // Offset from within mounted volume from where to read/write data
  dataLength: integer;  // Length of data to read/write. In bytes
  var data: TSDUBytes;  // Data to read/write

  offset: int64 = 0;  // Offset within volume where encrypted data starts
  size: int64 = 0;  // Size of volume
  storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
): boolean;
var
  dummyMetadata: TOTFEFreeOTFEVolumeMetaData;
  tempMountDriveLetter: char;
  stm: TSDUMemoryStream;
  // emptyIV : TSDUBytes;
//    volumeKeyStr: AnsiString;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;

  CheckActive();

  PopulateVolumeMetadataStruct(
                               FALSE,  // Linux volume
                               PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
                               dummyMetadata
                              );
                                  
  // Attempt to mount the device

  tempMountDriveLetter:= GetNextDriveLetter();
  // SDUInitAndZeroBuffer(0,emptyIV);
  Result := MountDiskDevice(
                     tempMountDriveLetter,
                     volFilename,
                     volumeKey,
                     sectorIVGenMethod,
                     nil,
                     FALSE,
                     IVHashDriver,
                     IVHashGUID,
                     IVCypherDriver,  // IV cypher
                     IVCypherGUID,  // IV cypher
                     mainCypherDriver,  // Main cypher
                     mainCypherGUID,  // Main cypher
                     VolumeFlags,
                     dummyMetadata,
                     offset,
                     size,
                     FreeOTFEMountAsStorageMediaType[mountMountAs]
                    );
  if Result then    begin
    try
      stm := TSDUMemoryStream.Create();
      try
      // Read decrypted data from mounted device
      if (readNotWrite) then
        begin
        Result := ReadData_Bytes(
                                  tempMountDriveLetter,
                                  dataOffset,
                                  dataLength,
                                  stm
                                 );
        if Result then          begin
          stm.Position := 0;
          data := SDUStringToSDUBytes(stm.ReadString(dataLength, 0));
          end;

        end                 else        begin
        stm.Position := 0;
        stm.WriteString(SDUBytesToString(data), dataLength, 0);

        Result := WriteData_Bytes(
                                  tempMountDriveLetter,
                                  dataOffset,
                                  dataLength,
                                  stm
                                 );
        end;

      finally
        stm.Free();
      end;

    finally
      // Unmount
      DismountDiskDevice(tempMountDriveLetter, TRUE);
    end;

    end;  // if (Result) then


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.LoadLibraryMain(var api: TMainAPI): boolean;
var
  libFilename: string;
begin
  Result := FALSE;

  libFilename := MainDLLFilename();
  api.hDLL := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then
    begin
    Result := TRUE;
    api.LibFilename := libFilename;

    @api.DSK_Init := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Init
                                                  );
    if (@api.DSK_Init = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Deinit := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Deinit
                                                  );
    if (@api.DSK_Deinit = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Open := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Open
                                                  );
    if (@api.DSK_Open = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Close := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Close
                                                  );
    if (@api.DSK_Close = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_IOControl := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_IOControl
                                                  );
    if (@api.DSK_IOControl = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Seek := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Seek
                                                  );
    if (@api.DSK_Seek = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Read := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Read
                                                  );
    if (@api.DSK_Read = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_Write := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_Write
                                                  );
    if (@api.DSK_Write = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_PowerDown := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_PowerDown
                                                  );
    if (@api.DSK_PowerDown = nil) then
      begin
      Result := FALSE;
      end;

    @api.DSK_PowerUp := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DSK_PowerUp
                                                  );
    if (@api.DSK_PowerUp = nil) then
      begin
      Result := FALSE;
      end;


      
    @api.MainDLLFnMACData := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_MACDATA
                                                  );
    if (@api.MainDLLFnMACData = nil) then
      begin
      Result := FALSE;
      end;

    @api.MainDLLFnDeriveKey := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_MAIN_DERIVEKEY
                                                  );
    if (@api.MainDLLFnDeriveKey = nil) then
      begin
      Result := FALSE;
      end;


    if not(Result) then
      begin
      FreeLibrary(api.hDLL);
      end;
    end;


end;

procedure TOTFEFreeOTFEDLL.FreeLibraryMain(var api: TMainAPI);
begin
  if (api.hDLL <> 0) then
    begin
    FreeLibrary(api.hDLL);
    api.hDLL := 0;
    api.LibFilename := '';
    end;
end;


function TOTFEFreeOTFEDLL.LoadLibraryHash(const libFilename: string; var api: THashAPI): boolean;
begin
  Result := FALSE;

  api.hDLL := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then
    begin
    Result := TRUE;
    api.LibFilename := libFilename;

    @api.HashDLLFnIdentifyDriver := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_HASH_IDENTIFYDRIVER
                                                  );
    if (@api.HashDLLFnIdentifyDriver = nil) then
      begin
      Result := FALSE;
      end;

    @api.HashDLLFnIdentifySupported := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_HASH_IDENTIFYSUPPORTED
                                                  );
    if (@api.HashDLLFnIdentifySupported = nil) then
      begin
      Result := FALSE;
      end;

    @api.HashDLLFnGetHashDetails := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_HASH_GETHASHDETAILS
                                                  );
    if (@api.HashDLLFnGetHashDetails = nil) then
      begin
      Result := FALSE;
      end;

    @api.HashDLLFnHash := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_HASH_HASH
                                                  );
    if (@api.HashDLLFnHash = nil) then
      begin
      Result := FALSE;
      end;


    if not(Result) then
      begin
      FreeLibrary(api.hDLL);
      end;
    end;


end;

function TOTFEFreeOTFEDLL.LoadLibraryCachedHash(const libFilename: string; var api: THashAPI): boolean;
begin
  Result:= CachesGetHashAPI(libFilename, api);

  if not(Result) then
    begin
    Result := LoadLibraryHash(libFilename, api);
    if Result then
      begin
      CachesAddHashAPI(libFilename, api);
      end;
    end;


end;

procedure TOTFEFreeOTFEDLL.FreeLibraryHash(var api: THashAPI);
begin
  if (api.hDLL <> 0) then
    begin
    FreeLibrary(api.hDLL);
    api.hDLL := 0;
    api.LibFilename := '';
    end;
end;

function TOTFEFreeOTFEDLL.LoadLibraryCypher(const libFilename: string; var api: TCypherAPI): boolean;
begin
  Result := FALSE;

  api.hDLL := LoadLibrary(PChar(libFilename));
  if (api.hDLL <> 0) then
    begin
    Result := TRUE;
    api.LibFilename := libFilename;

    @api.CypherDLLFnIdentifyDriver := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_IDENTIFYDRIVER
                                                  );
    if (@api.CypherDLLFnIdentifyDriver = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnIdentifySupported_v1 := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1
                                                  );
    if (@api.CypherDLLFnIdentifySupported_v1 = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnIdentifySupported_v3 := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3
                                                  );
    if (@api.CypherDLLFnIdentifySupported_v3 = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnGetCypherDetails_v1 := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1
                                                  );
    if (@api.CypherDLLFnGetCypherDetails_v1 = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnGetCypherDetails_v3 := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3
                                                  );
    if (@api.CypherDLLFnGetCypherDetails_v3 = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnEncrypt := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_ENCRYPT
                                                  );
    if (@api.CypherDLLFnEncrypt = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnEncryptSector := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_ENCRYPTSECTOR
                                                  );
    if (@api.CypherDLLFnEncryptSector = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnEncryptWithASCII := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_ENCRYPTWITHASCII
                                                  );
    if (@api.CypherDLLFnEncryptWithASCII = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnEncryptSectorWithASCII := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII
                                                  );
    if (@api.CypherDLLFnEncryptSectorWithASCII = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnDecrypt := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_DECRYPT
                                                  );
    if (@api.CypherDLLFnDecrypt = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnDecryptSector := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_DECRYPTSECTOR
                                                  );
    if (@api.CypherDLLFnDecryptSector = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnDecryptWithASCII := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_DECRYPTWITHASCII
                                                  );
    if (@api.CypherDLLFnDecryptWithASCII = nil) then
      begin
      Result := FALSE;
      end;

    @api.CypherDLLFnDecryptSectorWithASCII := GetProcAddress(
                                                   api.hDLL,
                                                   DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII
                                                  );
    if (@api.CypherDLLFnDecryptSectorWithASCII = nil) then
      begin
      Result := FALSE;
      end;


    if not(Result) then
      begin
      FreeLibrary(api.hDLL);
      end;
    end;


end;

function TOTFEFreeOTFEDLL.LoadLibraryCachedCypher(const libFilename: string; var api: TCypherAPI): boolean;
begin
  Result:= CachesGetCypherAPI(libFilename, api);

  if not(Result) then
    begin
    Result := LoadLibraryCypher(libFilename, api);
    if Result then
      begin
      CachesAddCypherAPI(libFilename, api);
      end;
    end;


end;

procedure TOTFEFreeOTFEDLL.FreeLibraryCypher(var api: TCypherAPI);
begin
  if (api.hDLL <> 0) then
    begin
    FreeLibrary(api.hDLL);
    api.hDLL := 0;
    api.LibFilename := '';
    end;
end;


function TOTFEFreeOTFEDLL.GetDLLDir(): string;
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
  result := IncludeTrailingPathDelimiter(fExeDir) +'DLLs\'+ CONFIG+'\'+ Ifthen( SDUApp64bit(), 'x64', 'Win32');
end;

procedure TOTFEFreeOTFEDLL.GetAllDriversUnderExeDir(driverFilenames: TStringList);
var
  fileIterator: TSDUFileIterator;
  filename: string;
begin
  // Compile a list of drivers in the ExeDir
  fileIterator:= TSDUFileIterator.Create(nil);
  try
    fileIterator.Directory := GetDLLDir();
    fileIterator.FileMask := '*.dll';
    fileIterator.RecurseSubDirs := FALSE;
    fileIterator.OmitStartDirPrefix := FALSE;
    fileIterator.IncludeDirNames := FALSE;

    fileIterator.Reset();
    filename := fileIterator.Next();
    while (filename<>'') do  begin
      driverFilenames.Add(filename);
      filename := fileIterator.Next();
      end;

  finally
    fileIterator.Free();
  end;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHashDrivers(var hashDrivers: TFreeOTFEHashDriverArray): boolean;
var
  dllNames: TStringList;
  j: integer;
begin
  Result := TRUE;

  SetLength(hashDrivers, 0);

  dllNames:= TStringList.Create();
  try
    GetAllDriversUnderExeDir(dllNames);
    for j:=0 to (dllNames.count-1) do
      begin
      if pos(DLL_HASH_FILE_PREFIX, ExtractFilename(dllNames[j]))>0 then
        begin
        // Get the details for the device
        SetLength(hashDrivers, (Length(hashDrivers)+1));
        Result := GetHashDriverHashes(dllNames[j], hashDrivers[high(hashDrivers)]);       // unicode->ansi not a data loss because only ansistring stored

        // Note: This "break" condition differs between the DLL and kernel
        //       driver versions
        if not(Result) then
          begin
          break;
          end;
        end;

      end;

  finally
    dllNames.Free();
  end;


end;

function TOTFEFreeOTFEDLL.GetCypherDrivers(var cypherDrivers: TFreeOTFECypherDriverArray): boolean;
var
  dllNames: TStringList;
  j: integer;
begin
  Result := TRUE;

  SetLength(cypherDrivers, 0);

  dllNames:= TStringList.Create();
  try
    GetAllDriversUnderExeDir(dllNames);
    for j:=0 to (dllNames.count-1) do
      begin
      if pos(DLL_CYPHER_FILE_PREFIX, ExtractFilename(dllNames[j]))>0 then
        begin
        // Get the details for the device
        SetLength(cypherDrivers, (Length(cypherDrivers)+1));
        Result := GetCypherDriverCyphers(dllNames[j], cypherDrivers[high(cypherDrivers)]);   // unicode->ansi not a data loss because only ansistring stored

        // Note: This "break" condition differs between the DLL and kernel
        //       driver versions
        if not(Result) then
          begin
          break;
          end;
        end;

      end;

  finally
    dllNames.Free();
  end;


end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHashDriverHashes(hashDriver: ansistring; var hashDriverDetails: TFreeOTFEHashDriver): boolean;
var
  DIOCBuffer: TDIOC_HASH_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferHashes: PDIOC_HASH_IDENTIFYSUPPORTED;
{$ENDIF}
  bufferSize: cardinal;
  hashDetails: PHASH;
  i: integer;
  arrIdx: integer;
  hashAPI: THashAPI;
begin
  Result := FALSE;

  if CachesGetHashDriver(hashDriver, hashDriverDetails) then
    begin
    Result := TRUE;
    end
  else
    begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Connecting to: '+hashDriver);
{$ENDIF}
    if not(LoadLibraryCachedHash(hashDriver, hashAPI)) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Couldn''t connect to: '+hashDriver);
{$ENDIF}
      end
    else
      begin
      if (hashAPI.HashDLLFnIdentifyDriver(@DIOCBuffer) = ERROR_SUCCESS) then
        begin
        hashDriverDetails.DriverGUID := DIOCBuffer.DriverGUID;
        hashDriverDetails.LibFNOrDevKnlMdeName := hashDriver;
//        hashDriverDetails.DeviceName := GetDeviceName(hashKernelModeDeviceName);
//        hashDriverDetails.DeviceUserModeName := DeviceUserModeName;
        hashDriverDetails.Title := Copy(DIOCBuffer.Title, 1, StrLen(DIOCBuffer.Title));
        hashDriverDetails.VersionID := DIOCBuffer.VersionID;
        hashDriverDetails.HashCount := DIOCBuffer.HashCount;


        bufferSize := sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
                      sizeof(hashDetails^) + (sizeof(hashDetails^) * hashDriverDetails.HashCount);
        // Round up to next "DIOC boundry". Although this always results in an
        // oversized buffer, it's guaranteed to be big enough.
        bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
        ptrBuffer := allocmem(bufferSize);

        SetLength(hashDriverDetails.Hashes, hashDriverDetails.HashCount);

        if (hashAPI.HashDLLFnIdentifySupported(bufferSize, ptrBuffer) = ERROR_SUCCESS) then
          begin
{$IFDEF FREEOTFE_DEBUG}
DIOCBufferHashes := (PDIOC_HASH_IDENTIFYSUPPORTED(ptrBuffer));
DebugMsg('hash details count: '+inttostr(DIOCBufferHashes.BufCount));
{$ENDIF}
          ptrBufferOffset := Pointer(PAnsiChar(ptrBuffer) +
                             sizeof(TDIOC_HASH_IDENTIFYSUPPORTED) -
                             sizeof(hashDetails^));
          arrIdx := low(hashDriverDetails.Hashes);
          for i:=1 to hashDriverDetails.HashCount do
            begin
            hashDetails := (PHASH(ptrBufferOffset));
            hashDriverDetails.Hashes[arrIdx].HashGUID  := hashDetails.HashGUID;
            hashDriverDetails.Hashes[arrIdx].Title     := Copy(hashDetails.Title, 1, StrLen(hashDetails.Title));
            hashDriverDetails.Hashes[arrIdx].VersionID := hashDetails.VersionID;
            hashDriverDetails.Hashes[arrIdx].Length    := hashDetails.Length;
            hashDriverDetails.Hashes[arrIdx].BlockSize := hashDetails.BlockSize;

            // Setup for the next one...
            ptrBufferOffset := Pointer(PAnsiChar(ptrBufferOffset) + sizeof(hashDetails^));
            inc(arrIdx);
            end;

          // Cache the information retrieved
          CachesAddHashDriver(hashDriver, hashDriverDetails);
              
          Result := TRUE;
          end
        else
          begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('gethashdrivers DIOC 2 FAIL');
{$ENDIF}
          end;

        FreeMem(ptrBuffer);
        end;

      end;

    end;  // if CachesGetHashDriver(hashDriver, hashDriverDetails) then




end;


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


end;

// ----------------------------------------------------------------------------
// Get cypher details - using FreeOTFE v3 and later API
// Don't call this function directly! Call GetCypherDriverCyphers(...) instead!
function TOTFEFreeOTFEDLL._GetCypherDriverCyphers_v3(cypherDriver: ansistring; var cypherDriverDetails: TFreeOTFECypherDriver): boolean;
var
  DIOCBuffer: TDIOC_CYPHER_IDENTIFYDRIVER;
  ptrBuffer: Pointer;
  ptrBufferOffset: Pointer;
{$IFDEF FREEOTFE_DEBUG}
  DIOCBufferCyphers: PDIOC_CYPHER_IDENTIFYSUPPORTED_v3;
{$ENDIF}
  bufferSize: cardinal;
  cypherDetails: PCYPHER_v3;
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


        bufferSize := sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
                      sizeof(cypherDetails^) +
                      (sizeof(cypherDetails^) * cypherDriverDetails.CypherCount);
        // Round up to next "DIOC boundry". Although this always results in an
        // oversized buffer, it's guaranteed to be big enough.
        bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
        ptrBuffer := allocmem(bufferSize);

        SetLength(cypherDriverDetails.Cyphers, cypherDriverDetails.CypherCount);

        if (cypherAPI.CypherDLLFnIdentifySupported_v3(bufferSize, ptrBuffer) = ERROR_SUCCESS) then
          begin
{$IFDEF FREEOTFE_DEBUG}
DIOCBufferCyphers := (PDIOC_CYPHER_IDENTIFYSUPPORTED_v3(ptrBuffer));
DebugMsg('cypher details count: '+inttostr(DIOCBufferCyphers.BufCount));
{$ENDIF}
          ptrBufferOffset := Pointer(
                                     PAnsiChar(ptrBuffer) +
                                     sizeof(TDIOC_CYPHER_IDENTIFYSUPPORTED_v3) -
                                     sizeof(cypherDetails^)
                                    );
          arrIdx := low(cypherDriverDetails.Cyphers);
          for i:=1 to cypherDriverDetails.CypherCount do
            begin
            cypherDetails := (PCYPHER_v3(ptrBufferOffset));
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

            cypherDriverDetails.Cyphers[arrIdx].KeySizeRequired := cypherDetails.KeySizeRequired;
            cypherDriverDetails.Cyphers[arrIdx].KeySizeUnderlying := cypherDetails.KeySizeUnderlying;
            cypherDriverDetails.Cyphers[arrIdx].BlockSize := cypherDetails.BlockSize;
            cypherDriverDetails.Cyphers[arrIdx].VersionID := cypherDetails.VersionID;

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


end;


// ----------------------------------------------------------------------------
// Hash the specified data with the supplied hash device/hash GUID
function TOTFEFreeOTFEDLL.HashData(
                      hashDriver: Ansistring;
                      hashGUID: TGUID;
                      const data: TSDUBytes;
out hashOut: TSDUBytes
                     ): boolean;
var
  ptrDIOCBufferIn: PDIOC_HASH_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_HASH_DATA_OUT;
  bufferSizeOut: DWORD;
  hashDetails: TFreeOTFEHash;
  hashByteCount: integer;
  expectedHashSizeBits: integer;  // In *bits*
  hashAPI: THashAPI;
  bitsReturned: DWORD;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  if GetSpecificHashDetails(hashDriver, hashGUID, hashDetails) then
    begin
    if not(LoadLibraryCachedHash(hashDriver, hashAPI)) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+hashDriver);
{$ENDIF}
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
      end
    else
      begin
      bufferSizeIn := sizeof(ptrDIOCBufferIn^) - sizeof(byte) + Length(Data);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        expectedHashSizeBits := hashDetails.Length;
        if (hashDetails.Length = -1) then
          begin
          expectedHashSizeBits := (Length(Data) * 8);
          end;
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(byte) + (expectedHashSizeBits div 8);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.HashGUID := hashGUID;

          ptrDIOCBufferIn.DataLength := Length(Data) * 8;
          StrMove(@ptrDIOCBufferIn.Data, PAnsiChar(Data), Length(Data));

          bitsReturned := expectedHashSizeBits;
          if (hashAPI.HashDLLFnHash(
                                    @hashGUID,
                                    ptrDIOCBufferIn.DataLength,
                                    PByte(@(ptrDIOCBufferIn.Data)),
                                    @bitsReturned,
                                    PByte(@(ptrDIOCBufferOut.Hash))
                                   ) = ERROR_SUCCESS) then
            begin
            hashByteCount := (bitsReturned div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('hashByteCount: '+inttostr(hashByteCount));
{$ENDIF}

            // Set hashOut so that it has enough characters which can be
            // overwritten with StrMove
             SDUInitAndZeroBuffer(hashByteCount, hashOut );
//            hashOut := StringOfChar(Ansichar(#0), hashByteCount);
            StrMove(PAnsiChar(hashOut), @ptrDIOCBufferOut.Hash, hashByteCount);

            Result := TRUE;
            end
          else
            begin
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

    end
  else
    begin
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
                      tBits: integer = -1
                     ): boolean;
var
  ptrDIOCBufferIn: PDIOC_GENERATE_MAC_IN_PC_DLL;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_GENERATE_MAC_OUT;
  bufferSizeOut: DWORD;
  outputByteCount: integer;
  tmpSizeBytes: integer;
  minBufferSizeOut: integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                  sizeof(ptrDIOCBufferIn^.Key) +
                  Length(key) -
                  sizeof(ptrDIOCBufferIn^.Data) +
                  Length(data);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full DK length depends on
    // the algorithm, hash, etc
    tmpSizeBytes := (max(MAX_MAC_LENGTH, tBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.MAC);
    bufferSizeOut := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.MACAlgorithm := FreeOTFEMACID[macAlgorithm];

      StringToWideChar(HashDriver, ptrDIOCBufferIn.HashDeviceName, (length(ptrDIOCBufferIn.HashDeviceName)-1));
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StringToWideChar(CypherDriver, ptrDIOCBufferIn.CypherDeviceName, (length(ptrDIOCBufferIn.CypherDeviceName)-1));
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.LengthWanted := tBits;

      ptrDIOCBufferIn.KeyLength := Length(key) * 8;
      ptrDIOCBufferIn.DataLength := Length(data) * 8;
      // This may seem a little weird, but we do this because the data is
      // immediatly after the key
      StrMove(@ptrDIOCBufferIn.Key, PAnsiChar(key), Length(key));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)), PAnsiChar(data), Length(data));

      if (DriverAPI.MainDLLFnMACData(
                                     bufferSizeIn,
                                     ptrDIOCBufferIn,
                                     bufferSizeOut,
                                     ptrDIOCBufferOut
                                    ) = ERROR_SUCCESS) then
        begin
        outputByteCount := (ptrDIOCBufferOut.MACLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

        // Set MACOut so that it has enough characters which can be
        // overwritten with StrMove
        MACOut := StringOfChar(Ansichar(#0), outputByteCount);
        StrMove(PAnsiChar(MACOut), @ptrDIOCBufferOut.MAC, outputByteCount);

        Result := TRUE;
        end
      else
        begin
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
                    Salt: array of byte;
                    Iterations: integer;
                    dkLenBits: integer;  // In *bits*
                    out DK: TSDUBytes
                   ): boolean;
var
  ptrDIOCBufferIn: PDIOC_DERIVE_KEY_IN_PC_DLL;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_DERIVE_KEY_OUT;
  bufferSizeOut: DWORD;
  outputByteCount: integer;
  tmpSizeBytes: integer;
  minBufferSizeOut: integer;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();


  bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                  sizeof(ptrDIOCBufferIn^.Password) +
                  Length(Password) -
                  sizeof(ptrDIOCBufferIn^.Salt) +
                  Length(Salt);
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
  ptrDIOCBufferIn := allocmem(bufferSizeIn);
  try
    // We use the maximum buffer possible, as the full MAC length depends on
    // the MAC, hash, etc
    tmpSizeBytes := (max(MAX_DERIVED_KEY_LENGTH, dkLenBits) div 8);  // Convert to bytes

    minBufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.DerivedKey);
    bufferSizeOut := minBufferSizeOut + tmpSizeBytes;
    // Round up to next "DIOC boundry". Although this always results in an
    // oversized buffer, it's guaranteed to be big enough.
    bufferSizeOut := bufferSizeOut + (DIOC_BOUNDRY - (bufferSizeOut mod DIOC_BOUNDRY));
    ptrDIOCBufferOut := allocmem(bufferSizeOut);
    try
      ptrDIOCBufferIn.KDFAlgorithm := FreeOTFEKDFID[kdfAlgorithm];

      StringToWideChar(HashDriver, ptrDIOCBufferIn.HashDeviceName, (length(ptrDIOCBufferIn.HashDeviceName)-1));
      ptrDIOCBufferIn.HashGUID := HashGUID;

      StringToWideChar(CypherDriver, ptrDIOCBufferIn.CypherDeviceName, (length(ptrDIOCBufferIn.CypherDeviceName)-1));
      ptrDIOCBufferIn.CypherGUID := CypherGUID;

      ptrDIOCBufferIn.Iterations   := Iterations;
      ptrDIOCBufferIn.LengthWanted := dkLenBits;

      ptrDIOCBufferIn.PasswordLength := (Length(Password) * 8);
      ptrDIOCBufferIn.SaltLength     := (Length(Salt) * 8);
      // This may seem a little weird, but we do this because the salt is
      // immediatly after the password
      StrMove(@ptrDIOCBufferIn.Password, PAnsiChar(Password), Length(Password));
      StrMove(((PAnsiChar(@ptrDIOCBufferIn.Password))+length(Password)), PAnsiChar(@Salt[0]), Length(Salt));


      if (DriverAPI.MainDLLFnDeriveKey(
                                       bufferSizeIn,
                                       ptrDIOCBufferIn,
                                       bufferSizeOut,
                                       ptrDIOCBufferOut
                                      ) = ERROR_SUCCESS) then
        begin
        outputByteCount := (ptrDIOCBufferOut.DerivedKeyLength div 8);
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('outputByteCount: '+inttostr(outputByteCount));
{$ENDIF}

        // Set DK so that it has enough characters which can be
        // overwritten with StrMove
         SDUInitAndZeroBuffer(outputByteCount,DK);
//        DK := StringOfChar(AnsiChar(#0), outputByteCount);
//        SDUCopyArrays(DK,ptrDIOCBufferOut.DerivedKey,);
        StrMove(PAnsiChar(DK), @ptrDIOCBufferOut.DerivedKey, outputByteCount);

        Result := TRUE;
        end
      else
        begin
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
                                   encryptFlag: boolean;
                                   cypherDriver: ansistring;
                                   cypherGUID: TGUID;
                                   var key: TSDUBytes;
                                   var IV: Ansistring;
                                   var inData: Ansistring;
                                   var outData: Ansistring
                                  ): boolean;
var
  ptrDIOCBufferIn: PDIOC_CYPHER_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut: DWORD;
  cypherDetails: TFreeOTFECypher_v3;
  cypherAPI: TCypherAPI;
  dllOpOK: boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then
    begin
    if LoadLibraryCachedCypher(cypherDriver, cypherAPI) then
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+cypherDriver);
{$ENDIF}
      end
    else
      begin
      bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                      sizeof(ptrDIOCBufferIn^.Key) +
                      Length(key) -
                      sizeof(ptrDIOCBufferIn^.IV) +
                      Length(IV) -
                      sizeof(ptrDIOCBufferIn^.Data) +
                      Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) + Length(inData);
        ptrDIOCBufferOut := allocmem(bufferSizeOut);
        try
          ptrDIOCBufferIn.CypherGUID := cypherGUID;

          ptrDIOCBufferIn.KeyLength  := (Length(key) * 8);  //  In *bits*
          ptrDIOCBufferIn.IVLength   := (Length(IV) * 8);  //  In *bits*
          ptrDIOCBufferIn.DataLength := Length(inData);  //  In *bytes*
          // This may seem a little weird, but we do this because the data is
          // immediatly after the IV, which is immediatly after the key
          StrMove(@ptrDIOCBufferIn.Key,                                   PAnsiChar(key),    Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)),            PAnsiChar(IV),     Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)+length(IV)), PAnsiChar(inData), Length(inData));

          if encryptFlag then
            begin
            dllOpOK := (cypherAPI.CypherDLLFnEncrypt(
                                            bufferSizeIn,
                                            ptrDIOCBufferIn,
                                            bufferSizeOut,
                                            ptrDIOCBufferOut
                                           ) = ERROR_SUCCESS);
            end
          else
            begin
            dllOpOK := (cypherAPI.CypherDLLFnDecrypt(
                                            bufferSizeIn,
                                            ptrDIOCBufferIn,
                                            bufferSizeOut,
                                            ptrDIOCBufferOut
                                           ) = ERROR_SUCCESS);
            end;
          if dllOpOK then
            begin
            // Set outData so that it has enough characters which can be
            // overwritten with StrMove
            outData := StringOfChar(AnsiChar(#0), ptrDIOCBufferIn.DataLength);

            StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, ptrDIOCBufferIn.DataLength);

            Result := TRUE;
            end
          else
            begin
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


    end
  else
    begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
    end;



end;


// ----------------------------------------------------------------------------
// encryptFlag - set to TRUE to encrypt, FALSE to decrypt
function TOTFEFreeOTFEDLL.EncryptDecryptSectorData(
                                   encryptFlag: boolean;
                                   cypherDriver: ansistring;
                                   cypherGUID: TGUID;
                                   SectorID: LARGE_INTEGER;
                                   SectorSize: integer;
                                   var key: TSDUBytes;
                                   var IV: Ansistring;
                                   var inData: Ansistring;
                                   var outData: Ansistring
                                  ): boolean;
var
  ptrDIOCBufferIn: PDIOC_CYPHER_SECTOR_DATA_IN;
  bufferSizeIn: DWORD;
  ptrDIOCBufferOut: PDIOC_CYPHER_DATA_OUT;
  bufferSizeOut: DWORD;
  cypherDetails: TFreeOTFECypher_v3;
  cypherAPI: TCypherAPI;
  dllOpOK: boolean;
begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := FALSE;

  CheckActive();

  if GetSpecificCypherDetails(cypherDriver, cypherGUID, cypherDetails) then
    begin
    if not(LoadLibraryCachedCypher(cypherDriver, cypherAPI)) then
      begin
      LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('couldn''t connect to: '+cypherDriver);
{$ENDIF}
      end
    else
      begin
      bufferSizeIn := sizeof(ptrDIOCBufferIn^) -
                      sizeof(ptrDIOCBufferIn^.Key) +
                      Length(key) -
                      sizeof(ptrDIOCBufferIn^.IV) +
                      Length(IV) -
                      sizeof(ptrDIOCBufferIn^.Data) +
                      Length(inData);
      // Round up to next "DIOC boundry". Although this always results in an
      // oversized buffer, it's guaranteed to be big enough.
      bufferSizeIn := bufferSizeIn + (DIOC_BOUNDRY - (bufferSizeIn mod DIOC_BOUNDRY));
      ptrDIOCBufferIn := allocmem(bufferSizeIn);
      try
        // Just make sure its big enough
        bufferSizeOut := sizeof(ptrDIOCBufferOut^) - sizeof(ptrDIOCBufferOut^.Data) + Length(inData);
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
          StrMove(@ptrDIOCBufferIn.Key,                                   PAnsiChar(key),    Length(key));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)),            PAnsiChar(IV),     Length(IV));
          StrMove(((PAnsiChar(@ptrDIOCBufferIn.Key))+length(key)+length(IV)), PAnsiChar(inData), Length(inData));

          if encryptFlag then
            begin
            dllOpOK := (cypherAPI.CypherDLLFnEncryptSector(
                                            bufferSizeIn,
                                            ptrDIOCBufferIn,
                                            bufferSizeOut,
                                            ptrDIOCBufferOut
                                           ) = ERROR_SUCCESS);
            end
          else
            begin
            dllOpOK := (cypherAPI.CypherDLLFnDecryptSector(
                                            bufferSizeIn,
                                            ptrDIOCBufferIn,
                                            bufferSizeOut,
                                            ptrDIOCBufferOut
                                           ) = ERROR_SUCCESS);
            end;
          if dllOpOK then
            begin
            // Set outData so that it has enough characters which can be
            // overwritten with StrMove
            outData := StringOfChar(AnsiChar(#0), ptrDIOCBufferIn.DataLength);

            StrMove(PAnsiChar(outData), @ptrDIOCBufferOut.Data, ptrDIOCBufferIn.DataLength);

            Result := TRUE;
            end
          else
            begin
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
      if not(Result) then
        begin
        Result := _EncryptDecryptData(
                                encryptFlag,
                                cypherDriver,
                                cypherGUID,
                                key,
                                IV,
                                inData,
                                outData
                               );

        end;


    end
  else
    begin
    LastErrorCode := OTFE_ERR_DRIVER_FAILURE;
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Unable to EncryptDecryptData');
{$ENDIF}
    end;



end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.MainDLLFilename(): string;
const
  DLL_MAIN_DRIVER = 'FreeOTFE.dll';
begin
  Result := GetDLLDir() +  '\' + DLL_MAIN_DRIVER;
  Result :=SDUGetFinalPath(Result);
end;


// ----------------------------------------------------------------------------
// Attempt to mount on an existing device
function TOTFEFreeOTFEDLL.MountDiskDevice(
                                   deviceName: string;
                                   volFilename: string;
                                   volumeKey: TSDUBytes;
                                   sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                                   volumeIV: TSDUBytes;
                                   readonly: boolean;
                                   IVHashDriver: ansistring;
                                   IVHashGUID: TGUID;
                                   IVCypherDriver: ansistring;
                                   IVCypherGUID: TGUID;
                                   mainCypherDriver: ansistring;
                                   mainCypherGUID: TGUID;
                                   VolumeFlags: integer;
                                   metaData: TOTFEFreeOTFEVolumeMetaData;
                                   offset: int64 = 0;
                                   size: int64 = 0;
                                   storageMediaType: TFreeOTFEStorageMediaType = mtFixedMedia
                                  ): boolean;
var
  ptrDIOCBuffer: PDIOC_MOUNT_PC_DLL;
  bufferSize: integer;
  mainCypherDetails: TFreeOTFECypher_v3;
  useVolumeFlags: integer;
  strMetaData: Ansistring;
  mntHandles: TMountedHandle;
  openAccessCode: DWORD;
  openShareMode: DWORD;
  diskGeometry: TSDUDiskGeometry;
  useDriveLetter: char;
  volumeKeyStr :Ansistring;
begin
  Result := FALSE;

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
DebugMsgBinary(volumeKey);
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


  if not(GetSpecificCypherDetails(mainCypherDriver, mainCypherGUID, mainCypherDetails)) then
    begin

    exit;
    end;

  // Ensure the volumeKey's length matches that of the cypher's keysize, if
  // the cypher's keysize is >= 0
  if (mainCypherDetails.KeySizeRequired >= 0) then begin
    // THIS IS CORRECT; caters for both volumeKey>keysize and
    // volumeKey<keysize
    // Copy as much of the hash value as possible to match the key length
    volumeKeyStr := SDUBytesToString(volumeKey);

    volumeKeyStr := Copy(volumeKeyStr, 1, min((mainCypherDetails.KeySizeRequired div 8), length(volumeKeyStr)));
        // If the hash wasn't big enough, pad out with zeros
        volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0), ((mainCypherDetails.KeySizeRequired div 8) - Length(volumeKeyStr)));

    SafeSetLength(volumeKey,(mainCypherDetails.KeySizeRequired div 8));
    assert(SDUBytestostring(volumeKey) = volumeKeyStr) ;
    end;


  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not(SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then
    begin
    IVHashDriver := '';
    IVHashGUID := StringToGUID(NULL_GUID);
    end;

  // If the sector IV doesn't use hashing, don't pass a hash algorithm in
  if not(SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then
    begin
    IVCypherDriver := '';
    IVCypherGUID := StringToGUID(NULL_GUID);
    end;


  // Subtract sizeof(char) - once for the volume key, once for the volumeIV
  bufferSize := sizeof(ptrDIOCBuffer^)
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(volumeKey))
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(volumeIV))
                - sizeof(Ansichar) + (sizeof(Ansichar)*Length(strMetaData));
  // Round up to next "DIOC boundry". Although this always results in an
  // oversized buffer, it's guaranteed to be big enough.
  bufferSize := bufferSize + (DIOC_BOUNDRY - (bufferSize mod DIOC_BOUNDRY));
  ptrDIOCBuffer := allocmem(bufferSize);
  try
    StringToWideChar(volFilename, ptrDIOCBuffer.Filename, (length(ptrDIOCBuffer.Filename)-1));
    ptrDIOCBuffer.DataStart := offset;

    ptrDIOCBuffer.DataEnd := 0;
    if (size > 0) then
      begin
      ptrDIOCBuffer.DataEnd := offset + size;
      end;

    StringToWideChar(IVHashDriver, ptrDIOCBuffer.IVHashDeviceName, (length(ptrDIOCBuffer.IVHashDeviceName)-1));
    ptrDIOCBuffer.IVHashGUID := IVHashGUID;
    StringToWideChar(IVCypherDriver, ptrDIOCBuffer.IVCypherDeviceName, (length(ptrDIOCBuffer.IVCypherDeviceName)-1));
    ptrDIOCBuffer.IVCypherGUID := IVCypherGUID;

    StringToWideChar(mainCypherDriver, ptrDIOCBuffer.MainCypherDeviceName, (length(ptrDIOCBuffer.MainCypherDeviceName)-1));
    ptrDIOCBuffer.MainCypherGUID := mainCypherGUID;

    ptrDIOCBuffer.ReadOnly := readonly;
    ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsFile];
    if IsPartition_UserModeName(volFilename) then
      begin
      ptrDIOCBuffer.MountSource := FreeOTFEMountSourceID[fomsPartition];
      end;

    useVolumeFlags := VolumeFlags;
    // Yes, this timestamp reverting is the right way around; if the bit
    // *isn't* set, the timestamps get reverted
    if RevertVolTimestamps then
      begin
      // Strip off bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags and not(VOL_FLAGS_NORMAL_TIMESTAMPS);
      end
    else
      begin
      // Set bit VOL_FLAGS_NORMAL_TIMESTAMPS
      useVolumeFlags := useVolumeFlags or VOL_FLAGS_NORMAL_TIMESTAMPS
      end;
    ptrDIOCBuffer.VolumeFlags := useVolumeFlags;
    ptrDIOCBuffer.SectorIVGenMethod := FreeOTFESectorIVGenMethodID[sectorIVGenMethod];

    ptrDIOCBuffer.MasterKeyLength := Length(volumeKey) * 8;
    ptrDIOCBuffer.VolumeIVLength := Length(volumeIV) * 8;
    ptrDIOCBuffer.MetaDataLength := Length(strMetaData);
    // This may seem a little weird, but we do this because the VolumeIV and metaData are
    // immediatly after the master key
    StrMove(@ptrDIOCBuffer.MasterKey, PAnsiChar(volumeKey), Length(volumeKey));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey))+length(volumeKey)), PAnsiChar(volumeIV), Length(volumeIV));
    StrMove(((PAnsiChar(@ptrDIOCBuffer.MasterKey))+length(volumeKey)+length(volumeIV)), PAnsiChar(strMetaData), Length(strMetaData));


    mntHandles.DeviceHandle := DriverAPI.DSK_Init(nil, ptrDIOCBuffer);
    if (mntHandles.DeviceHandle <> 0) then
      begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Mounted OK!');
{$ENDIF}
      openAccessCode := (GENERIC_READ or GENERIC_WRITE);
      if readonly then
        begin
        openAccessCode := GENERIC_READ;
        end;
      openShareMode := 0; // Don't allow sharing

      mntHandles.OpenHandle := DriverAPI.DSK_Open(
                                                  mntHandles.DeviceHandle,
                                                  openAccessCode,
                                                  openShareMode
                                                 );
      if (mntHandles.OpenHandle = 0) then
        begin
        DriverAPI.DSK_Deinit(mntHandles.DeviceHandle);
        end
      else
        begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Opened OK!');
{$ENDIF}
        mntHandles.BytesPerSector := 0;
        // Store tmp handles...
        AddHandles(useDriveLetter, mntHandles);
        if _GetDiskGeometry(useDriveLetter, diskGeometry) then
          begin
          // Cache the disk geometry...
          AddDiskGeometry(useDriveLetter, diskGeometry);

          // Add the bytes per sector, then delete and re-store the handles
          mntHandles.BytesPerSector := diskGeometry.BytesPerSector;
          DeleteHandles(useDriveLetter);
          AddHandles(useDriveLetter, mntHandles);

          Result := TRUE;
          end
        else
          begin
          DismountDiskDevice(useDriveLetter, FALSE);
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
                              volFilename: string;
                              volumeKey: TSDUBytes;
                              sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                              volumeIV: TSDUBytes;
                              readonly: boolean;
                              IVHashDriver: ansistring;
                              IVHashGUID: TGUID;
                              IVCypherDriver: ansistring;
                              IVCypherGUID: TGUID;
                              mainCypherDriver: ansistring;
                              mainCypherGUID: TGUID;
                              VolumeFlags: integer;
                              DriveLetter: char;  // PC kernel drivers: disk device to mount. PC DLL: "Drive letter"
                              offset: int64 = 0;
                              size: int64 = 0;
                              MetaData_LinuxVolume: boolean = FALSE;  // Linux volume
                              MetaData_PKCS11SlotID: integer = PKCS11_NO_SLOT_ID;  // PKCS11 SlotID
                              MountMountAs: TFreeOTFEMountAs = fomaRemovableDisk;  // PC kernel drivers *only* - ignored otherwise
                              mountForAllUsers: boolean = TRUE  // PC kernel drivers *only* - ignored otherwise
                             ): boolean;
var
  mountMetadata: TOTFEFreeOTFEVolumeMetaData;
begin
  Result:= FALSE;

  PopulateVolumeMetadataStruct(
                               MetaData_LinuxVolume,
                               MetaData_PKCS11SlotID,
                               mountMetadata
                              );

  // Attempt to mount the device
  if MountDiskDevice(
                     DriveLetter,
                     volFilename,
                     volumeKey,
                     sectorIVGenMethod,
                     volumeIV,
                     readonly,
                     IVHashDriver,
                     IVHashGUID,
                     IVCypherDriver,
                     IVCypherGUID,
                     mainCypherDriver,
                     mainCypherGUID,
                     VolumeFlags,
                     mountMetadata,
                     offset,
                     size,
                     FreeOTFEMountAsStorageMediaType[MountMountAs]
                    ) then
    begin
      Result := TRUE;
    end;


end;


// ----------------------------------------------------------------------------
// Attempt to dismount a device
function TOTFEFreeOTFEDLL.DismountDiskDevice(deviceName: string; emergency: boolean): boolean;
var
  DIOCBuffer: TDIOC_FORCE_DISMOUNTS;
  bytesReturned: DWORD;
  handles: TMountedHandle;
  useDriveLetter: char;
begin
  Result := FALSE;

  CheckActive();

  // Sanity check...
  Assert(
         (deviceName <> ''),
         'deviceName set to empty string in call to DismountDiskDevice'
        );
    { TODO 1 -otdk -cclean : pass in just drive letter }
  useDriveLetter := deviceName[1];

  if GetHandles(useDriveLetter, handles) then
    begin
    if emergency then
      begin
      DIOCBuffer.ForceDismounts := emergency;
      DriverAPI.DSK_IOControl(
                              handles.OpenHandle,
                              IOCTL_FREEOTFE_SET_FORCE_DISMOUNT,
                              @DIOCBuffer,
                              sizeof(DIOCBuffer),
                              nil,
                              0,
                              @bytesReturned
                             );
      end;

    DriverAPI.DSK_Close(handles.OpenHandle);
    DriverAPI.DSK_Deinit(handles.DeviceHandle);

    DeleteDiskGeometry(useDriveLetter);
    DeleteHandles(useDriveLetter);

    Result := TRUE;
    end;


end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.AddHandles(driveLetter: char; handles: TMountedHandle);
var
  ptrMountedHandles: PMountedHandle;
begin
  new(ptrMountedHandles);
  ptrMountedHandles^ := handles;
  fMountedHandles.AddObject(driveLetter, TObject(ptrMountedHandles));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetHandles(driveLetter: char; var handles: TMountedHandle): boolean;
var
  ptrMountedHandles: PMountedHandle;
  idx: integer;
begin
  Result := FALSE;

  idx := fMountedHandles.IndexOf(uppercase(driveLetter));
  if (idx >= 0) then
    begin
    ptrMountedHandles := PMountedHandle(fMountedHandles.Objects[idx]);
    handles := ptrMountedHandles^;
    Result := TRUE;
    end;


end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.DeleteHandles(driveLetter: char);
var
  ptrMountedHandles: PMountedHandle;
  idx: integer;
begin
  idx := fMountedHandles.IndexOf(driveLetter);
  if (idx >= 0) then    begin
    ptrMountedHandles := PMountedHandle(fMountedHandles.Objects[idx]);
    Dispose(ptrMountedHandles);
    fMountedHandles.Delete(idx);
    end;
    
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetNextDriveLetter(userDriveLetter, requiredDriveLetter: char): char;
var
  c: char;
begin
  Result := #0;
  for c:='A' to 'Z' do      begin
    if (fMountedHandles.IndexOf(c) < 0) then      begin
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
function TOTFEFreeOTFEDLL.DriverType(): string;
begin
  Result := _('DLL driver');
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetVolumeInfo(driveLetter: char; var volumeInfo: TOTFEFreeOTFEVolumeInfo): boolean;
var
  BytesReturned: DWORD;
  outBuffer: TDIOC_DISK_DEVICE_STATUS_PC_DLL;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  handles: TMountedHandle;
begin
  Result := FALSE;

  CheckActive();

  driveLetter := upcase(driveLetter);

  if GetHandles(driveLetter, handles) then
    begin
    if DriverAPI.DSK_IOControl(
                            handles.OpenHandle,
                            IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS,
                            nil,
                            0,
                            @outBuffer,
                            sizeof(outBuffer),
                            @BytesReturned
                           ) then
      begin
      if (BytesReturned <= sizeof(outBuffer)) then
        begin
        volumeInfo.Filename         := Copy(outBuffer.Filename, 1, SDUWStrLen(outBuffer.Filename)); { TODO 1 -otdk -cinvestigate : what if unicode filename? }
        volumeInfo.DeviceName       := driveLetter;
        volumeInfo.IVHashDevice     := Copy(outBuffer.IVHashDeviceName, 1, SDUWStrLen(outBuffer.IVHashDeviceName));    { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.IVHashGUID       := outBuffer.IVHashGUID;
        volumeInfo.IVCypherDevice   := Copy(outBuffer.IVCypherDeviceName, 1, SDUWStrLen(outBuffer.IVCypherDeviceName)); { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.IVCypherGUID     := outBuffer.IVCypherGUID;
        volumeInfo.MainCypherDevice := Copy(outBuffer.MainCypherDeviceName, 1, SDUWStrLen(outBuffer.MainCypherDeviceName)); { TODO 1 -otdk -cinvestigate : what if unicode? }
        volumeInfo.MainCypherGUID   := outBuffer.MainCypherGUID;
        volumeInfo.Mounted          := TRUE;
        volumeInfo.ReadOnly         := outBuffer.ReadOnly;
        volumeInfo.VolumeFlags      := outBuffer.VolumeFlags;

        volumeInfo.SectorIVGenMethod:= foivgUnknown;
        for tmpSectorIVGenMethod := low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do
          begin
          if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = outBuffer.SectorIVGenMethod) then
            begin
            volumeInfo.SectorIVGenMethod:= tmpSectorIVGenMethod;
            end;
          end;

        volumeInfo.DriveLetter := AnsiChar( driveLetter); // pos can be char - see todo in volinfo struct

// Functionality in DLL/PDA driver to retrieve metadata not currently implemented!
        Result := TRUE;
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
function TOTFEFreeOTFEDLL.Seek(driveLetter: char; offset: DWORD): boolean;
var
  handles: TMountedHandle;
begin
  Result:= FALSE;

  CheckActive();

  if GetHandles(driveLetter, handles) then
    begin
    if (DriverAPI.DSK_Seek(
                           handles.OpenHandle,
                           offset,
                           FILE_BEGIN
                          ) = ERROR_SUCCESS) then
      begin
      end;
    end;


end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.ReadData_Sectors(
  DriveLetter: char;
  SectorNoStart: DWORD;
  CountSectors: DWORD;
  stm: TStream
): boolean;
var
  handles: TMountedHandle;
  ptrBuffer: PByte;
  sgReq: TSG_REQ;
  bufferSize: DWORD;
  bytesReturned: DWORD;
begin
  Result:= FALSE;

  CheckActive();

  if GetHandles(driveLetter, handles) then
    begin
    bufferSize:= (CountSectors * handles.BytesPerSector);
    ptrBuffer := AllocMem(bufferSize);
    try
      sgReq.sr_start:= sectorNoStart;
      sgReq.sr_num_sec:= countSectors;
      sgReq.sr_num_sg:= 1;  // Only 1 SG buffer
      sgReq.sr_status:= ERROR_SUCCESS;
      sgReq.sr_callback:= nil;
      sgReq.sr_sglist[1].sb_len := bufferSize;
      sgReq.sr_sglist[1].sb_buf := ptrBuffer;

      if DriverAPI.DSK_IOControl(
                             handles.OpenHandle,
                             IOCTL_DISK_READ,
                             // DISK_IOCTL_READ, - old const; changed to IOCTL_DISK_READ in WinCE 3.0
                             @sgReq,
                             sizeof(sgReq),
                             nil,
                             0,
                             @bytesReturned
                            ) then
        begin
        if (sgReq.sr_status = ERROR_SUCCESS) then
          begin
          stm.Write(ptrBuffer^, bufferSize);
          Result := TRUE;
          end;
        end;
    finally
      FreeMem(ptrBuffer);
    end;
    end;


end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.WriteData_Sectors(
  DriveLetter: char;
  SectorNoStart: DWORD;
  CountSectors: DWORD;
  stm: TStream
): boolean;
var
  handles: TMountedHandle;
  ptrBuffer: PByte;
  sgReq: TSG_REQ;
  bufferSize: DWORD;
  bytesReturned: DWORD;
begin
  Result:= FALSE;

  CheckActive();

  if GetHandles(driveLetter, handles) then
    begin
    bufferSize:= (CountSectors * handles.BytesPerSector);
    ptrBuffer := AllocMem(bufferSize);
    try
      stm.Read(ptrBuffer^, bufferSize);

      sgReq.sr_start:= sectorNoStart;
      sgReq.sr_num_sec:= countSectors;
      sgReq.sr_num_sg:= 1;  // Only 1 SG buffer
      sgReq.sr_status:= ERROR_SUCCESS;
      sgReq.sr_callback:= nil;
      sgReq.sr_sglist[1].sb_len := bufferSize;
      sgReq.sr_sglist[1].sb_buf := ptrBuffer;

      if DriverAPI.DSK_IOControl(
                             handles.OpenHandle,
                             IOCTL_DISK_WRITE,
                             // DISK_IOCTL_WRITE, - old const; changed to IOCTL_DISK_WRITE in WinCE 3.0
                             @sgReq,
                             sizeof(sgReq),
                             nil,
                             0,
                             @bytesReturned
                            ) then
        begin
        if (sgReq.sr_status = ERROR_SUCCESS) then
          begin
          Result := TRUE;
          end;
        end;
    finally
      FreeMem(ptrBuffer);
    end;
    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.ReadData_Bytes(
                      DriveLetter: char;
                      Offset: ULONGLONG;
                      DataLength: ULONGLONG;
                      Data: TStream
                    ): boolean;
var
  diskGeometry: TSDUDiskGeometry;
  sectorID_i64: int64;
  sectorID: DWORD;

  cntSectors: DWORD;

  partialSector: boolean;
  tmpStream: TSDUMemoryStream;
begin
  Result := TRUE;

  // Short circuit...
  if (DataLength = 0) then
    begin
    Result := TRUE;
    exit;
    end;

  if Result then
    begin
    Result := GetDiskGeometry(DriveLetter, diskGeometry);
    end;

  sectorID := 0;
  if Result then
    begin
    if (Offset mod int64(diskGeometry.BytesPerSector) <> 0) then
      begin
      //lplp - xxx - eliminate this restriction
      showmessage('NON-SECTOR (offset + length) READS must start from a sector offset in current implementation');
      Result := FALSE;
      end;

    sectorID_i64 := Offset div int64(diskGeometry.BytesPerSector);
    sectorID := sectorID_i64 and $FFFFFFFF;
    end;

  if Result then
    begin
    // If the amount of data to transfer isn't a multiple of the sector size, we
    // have to use a temp stream to hold the data 
    partialSector := ((DataLength mod diskGeometry.BytesPerSector) <> 0);
    cntSectors := (DataLength div diskGeometry.BytesPerSector);
    if partialSector then
      begin
      inc(cntSectors);
      tmpStream:= TSDUMemoryStream.Create();
      try
        Result := ReadData_Sectors(DriveLetter, sectorID, cntSectors, tmpStream);
        tmpStream.Position := 0;
        Data.CopyFrom(tmpStream, DataLength);
      finally
        tmpStream.Free();
      end;

      end
    else
      begin
      Result := ReadData_Sectors(DriveLetter, sectorID, cntSectors, Data);
      end;

    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.WriteData_Bytes(
                      DriveLetter: char;
                      Offset: ULONGLONG;
                      DataLength: ULONGLONG;
                      Data: TStream
                    ): boolean;
var
  diskGeometry: TSDUDiskGeometry;
  sectorID_i64: int64;
  sectorID: DWORD;

  cntSectors: DWORD;

  partialSector: boolean;
  tmpStream: TSDUMemoryStream;
begin
  Result := TRUE;

  // Short circuit...
  if (DataLength = 0) then
    begin
    Result := TRUE;
    exit;
    end;

  if Result then
    begin
    Result := GetDiskGeometry(DriveLetter, diskGeometry);
    end;

  sectorID := 0;
  if Result then
    begin
    if (Offset mod int64(diskGeometry.BytesPerSector) <> 0) then
      begin
      //lplp - xxx - eliminate this restriction
      showmessage('NON-SECTOR (offset + length) WRITES must start from a sector offset in current implementation');
      Result := FALSE;
      end;

    sectorID_i64 := Offset div int64(diskGeometry.BytesPerSector);
    sectorID := sectorID_i64 and $FFFFFFFF;
    end;

  if Result then
    begin
    // If the amount of data to transfer isn't a multiple of the sector size, we
    // have to use a temp stream to hold the data
    partialSector := ((DataLength mod diskGeometry.BytesPerSector) <> 0);
    cntSectors := (DataLength div diskGeometry.BytesPerSector);
    if partialSector then
      begin
      tmpStream:= TSDUMemoryStream.Create();
      try
        inc(cntSectors);
        // In order to preserve the data previously written to the sector's slack
        // space, read in the previous data and overlay the new data onto it.
        if ReadData_Sectors(
                            DriveLetter,
                            sectorID,
                            cntSectors,
                            tmpStream
                           ) then
          begin
          tmpStream.Position := 0;
          tmpStream.CopyFrom(Data, DataLength);
          tmpStream.Position := 0;
          Result := WriteData_Sectors(DriveLetter, sectorID, cntSectors, tmpStream);
          end;
          
      finally
        tmpStream.Free();
      end;

      end
    else
      begin
      Result := WriteData_Sectors(DriveLetter, sectorID, cntSectors, Data);
      end;
    end;


end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL._GetDiskGeometry(
  DriveLetter: char;
  var diskGeometry: TSDUDiskGeometry
): boolean;
var
  handles: TMountedHandle;
  bytesReturned: DWORD;
begin
  Result:= FALSE;

  CheckActive();

  if GetHandles(driveLetter, handles) then
    begin
    if DriverAPI.DSK_IOControl(
                           handles.OpenHandle,
                           IOCTL_DISK_GETINFO,
                           nil,
                           0,
                           @diskGeometry,
                           sizeof(diskGeometry),
                           @bytesReturned
                          ) then
      begin
      Result := TRUE;
      end;
    end;


end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.AddDiskGeometry(driveLetter: char; diskGeometry: TSDUDiskGeometry);
var
  ptrDiskGeometry: PSDUDiskGeometry;
begin
  new(ptrDiskGeometry);
  ptrDiskGeometry^ := diskGeometry;
  fCachedDiskGeometry.AddObject(driveLetter, TObject(ptrDiskGeometry));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.GetDiskGeometry(driveLetter: char; var diskGeometry: TSDUDiskGeometry): boolean;
var
  ptrDiskGeometry: PSDUDiskGeometry;
  idx: integer;
begin
  Result := FALSE;
  
  idx := fCachedDiskGeometry.IndexOf(uppercase(driveLetter));
  if (idx >= 0) then
    begin
    ptrDiskGeometry := PSDUDiskGeometry(fCachedDiskGeometry.Objects[idx]);
    diskGeometry := ptrDiskGeometry^;
    Result := TRUE;
    end;


end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.DeleteDiskGeometry(driveLetter: char);
var
  ptrDiskGeometry: PSDUDiskGeometry;
  idx: integer;
begin
  idx := fCachedDiskGeometry.IndexOf(driveLetter);
  if (idx >= 0) then    begin
    ptrDiskGeometry := PSDUDiskGeometry(fCachedDiskGeometry.Objects[idx]);
    Dispose(ptrDiskGeometry);
    fCachedDiskGeometry.Delete(idx);
    end;
    
end;


// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesCreate();
begin
  inherited;
  fCachedHashAPI:= TStringList.Create();
  fCachedCypherAPI:= TStringList.Create();

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesDestroy();
var
  i: integer;
begin
  inherited;

  for i:=0 to (fCachedHashAPI.Count-1) do
    begin
    if (fCachedHashAPI.Objects[i] <> nil) then
      begin
      FreeLibraryHash((PHashAPI(fCachedHashAPI.Objects[i]))^);
      Dispose(PHashAPI(fCachedHashAPI.Objects[i]));
      end;
    end;
  fCachedHashAPI.Free();

  for i:=0 to (fCachedCypherAPI.Count-1) do
    begin
    if (fCachedCypherAPI.Objects[i] <> nil) then
      begin
      FreeLibraryCypher((PCypherAPI(fCachedCypherAPI.Objects[i]))^);
      Dispose(PCypherAPI(fCachedCypherAPI.Objects[i]));
      end;
    end;
  fCachedCypherAPI.Free();

end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesAddHashAPI(const libFilename: string; const api: THashAPI);
var
  ptrRec: PHashAPI;
begin
  new(ptrRec);
  ptrRec^ := api;
  fCachedHashAPI.AddObject(libFilename, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.CachesGetHashAPI(const libFilename: string; var api: THashAPI): boolean;
var
  ptrRec: PHashAPI;
  idx: integer;
  cacheHit: boolean;
begin
  cacheHit := FALSE;

  idx := fCachedHashAPI.IndexOf(libFilename);
  if (idx >= 0) then
    begin
    ptrRec := PHashAPI(fCachedHashAPI.Objects[idx]);
    api := ptrRec^;
    cacheHit := TRUE;
    end;

  Result := cacheHit;
end;

// ----------------------------------------------------------------------------
procedure TOTFEFreeOTFEDLL.CachesAddCypherAPI(const libFilename: string; const api: TCypherAPI);
var
  ptrRec: PCypherAPI;
begin
  new(ptrRec);
  ptrRec^ := api;
  fCachedCypherAPI.AddObject(libFilename, Pointer(ptrRec));
end;

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEDLL.CachesGetCypherAPI(const libFilename: string; var api: TCypherAPI): boolean;
var
  ptrRec: PCypherAPI;
  idx: integer;
  cacheHit: boolean;
begin
  cacheHit := FALSE;

  idx := fCachedCypherAPI.IndexOf(libFilename);
  if (idx >= 0) then
    begin
    ptrRec := PCypherAPI(fCachedCypherAPI.Objects[idx]);
    api := ptrRec^;
    cacheHit := TRUE;
    end;

  Result := cacheHit;
end;

{returns an instance of the only object. call SetFreeOTFEType first}
function GetFreeOTFEDLL :  TOTFEFreeOTFEDLL;
begin
  assert (GetFreeOTFEBase is TOTFEFreeOTFEDLL,'call SetFreeOTFEType with correct type');
  result := GetFreeOTFEBase as TOTFEFreeOTFEDLL;
end;

END.



