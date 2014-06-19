unit pkcs11_token;

interface

uses
  Controls,
  pkcs11t,
  pkcs11_api,
  pkcs11_mechanism,
  pkcs11_session;

const
  NULL_TIME = 0;

type
  TPKCS11Token = class(TPKCS11API)
  private
  protected
    FSlotID: integer;

    FCacheTokenInfo: boolean;
    FCachedTokenInfoPresent: boolean;
    FCachedTokenInfo: CK_TOKEN_INFO;

    // xxx - change these and all like them to TList
    FCachedMechanismsArray: TPKCS11MechanismArray;

    procedure SetCacheTokenInfo(flag: boolean);

    function GetXLabel(): string;
    function GetManufacturerID(): string;
    function GetModel(): string;
    function GetSerialNumber(): string;
    function GetFlags(): CK_FLAGS;
    function GetMaxSessionCount(): cardinal;
    function GetSessionCount(): cardinal;
    function GetMaxRwSessionCount(): cardinal;
    function GetRwSessionCount(): cardinal;
    function GetMaxPinLen(): cardinal;
    function GetMinPinLen(): cardinal;
    function GetTotalPublicMemory(): cardinal;
    function GetFreePublicMemory(): cardinal;
    function GetTotalPrivateMemory(): cardinal;
    function GetFreePrivateMemory(): cardinal;
    function GetHardwareVersion(): CK_VERSION;
    function GetFirmwareVersion(): CK_VERSION;
    function GetUTCTime(): TDateTime;

    function _GetCountMechanisms(): integer;

    function InitToken(usePINPad: boolean; SOPIN: string; tokenLabel: string): boolean; overload;

  public
    constructor Create();
    destructor Destroy(); override;
    
    property SlotID: integer read FSlotID write FSlotID;

    function GetTokenInfo(var info: CK_TOKEN_INFO): boolean;

    function InitToken(tokenLabel: string): boolean; overload;
    function InitToken(SOPIN: string; tokenLabel: string): boolean; overload;

    function GetCountMechanisms(): integer;
    function GetMechanismList(var mechsArray: TPKCS11MechanismArray): boolean;
    function GetMechanismByIdx(idx: integer): TPKCS11Mechanism; overload;
    function GetMechanismByType(mechType: CK_MECHANISM_TYPE): TPKCS11Mechanism; overload;

    function  OpenSession(readOnly: boolean): TPKCS11Session; overload;
    function  OpenSession(flags: cardinal; callbackData: CK_VOID_PTR; callback: CK_NOTIFY): TPKCS11Session; overload;
    procedure CloseSession(session: TPKCS11Session);
    procedure CloseAllSessions();

    // If this flag is set, GetTokenInfo will return cached information
    property CacheTokenInfo: boolean read FCacheTokenInfo write SetCacheTokenInfo;
    procedure FlushCache();

    property XLabel: string read GetXLabel;
    property ManufacturerID: string read GetManufacturerID;
    property Model: string read GetModel;
    property SerialNumber: string read GetSerialNumber;
    property Flags: CK_FLAGS read GetFlags;
    property MaxSessionCount   : cardinal read GetMaxSessionCount;
    property SessionCount      : cardinal read GetSessionCount;
    property MaxRwSessionCount : cardinal read GetMaxRwSessionCount;
    property RwSessionCount    : cardinal read GetRwSessionCount;
    property MaxPinLen         : cardinal read GetMaxPinLen;
    property MinPinLen         : cardinal read GetMinPinLen;
    property TotalPublicMemory : cardinal read GetTotalPublicMemory;
    property FreePublicMemory  : cardinal read GetFreePublicMemory;
    property TotalPrivateMemory: cardinal read GetTotalPrivateMemory;
    property FreePrivateMemory : cardinal read GetFreePrivateMemory;
    property HardwareVersion   : CK_VERSION read GetHardwareVersion;
    property FirmwareVersion   : CK_VERSION read GetFirmwareVersion;
    property UTCTime: TDateTime read GetUTCTime;

    property CountMechanisms: integer read GetCountMechanisms;
    // Note: Mechanism(...) indexes from zero
    property Mechanism[idx: integer]: TPKCS11Mechanism read GetMechanismByIdx;

  end;

implementation

uses
  SysUtils,
  DateUtils;

constructor TPKCS11Token.Create();
begin
  inherited Create();

  FCachedMechanismsArray := nil;
  FCacheTokenInfo := TRUE;
end;


destructor TPKCS11Token.Destroy();
begin
  inherited;
end;

function TPKCS11Token.GetTokenInfo(var info: CK_TOKEN_INFO): boolean;
var
  retval: boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetTokenInfo, FN_NAME_C_GetTokenInfo);

  if (FCacheTokenInfo and FCachedTokenInfoPresent) then
    begin
    info := FCachedTokenInfo;
    retval := TRUE;
    end
  else
    begin
    LastRV := LibraryFunctionList.CK_C_GetTokenInfo(SlotID, @info);
    retval := RVSuccess(LastRV);

    if (retval and CacheTokenInfo) then
      begin
      FCachedTokenInfo := info;
      FCachedTokenInfoPresent := TRUE;
      end;
    end;

  Result := retval;
end;


function TPKCS11Token.OpenSession(readOnly: boolean): TPKCS11Session;
var
  flags: cardinal;
begin
  flags := CKF_SERIAL_SESSION;
  if not(readOnly) then
    begin
    flags := (flags + CKF_RW_SESSION);
    end;
  Result := OpenSession(
                        flags,
                        //(CKF_SERIAL_SESSION),
                        nil,
                        nil
                       );
end;

// Note: It's the CALLERS responsibility to free off the returned object
// Returns new session, or nil on failure
function TPKCS11Token.OpenSession(flags: cardinal; callbackData: CK_VOID_PTR; callback: CK_NOTIFY): TPKCS11Session;
var
  newSession: TPKCS11Session;
  tmpHandle: CK_SESSION_HANDLE;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_OpenSession, FN_NAME_C_OpenSession);

  newSession := nil;
  LastRV := LibraryFunctionList.CK_C_OpenSession(
                                             slotID,
                                             flags,
                                             callbackData,
                                             callback,
                                             @tmpHandle
                                           );
  if RVSuccess(LastRV) then
    begin
    newSession := TPKCS11Session.Create();
    newSession.FhSession := tmpHandle;
    newSession.SlotID := SlotID;
    newSession.LibraryFunctionList := LibraryFunctionList;
    end;

  Result := newSession;
end;

// Important: Keep implementation of:
//
//              TPKCS11Token.CloseSession(...);
//              TPKCS11Session.CloseSession();
//
//            in sync
procedure TPKCS11Token.CloseSession(session: TPKCS11Session);
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CloseSession, FN_NAME_C_CloseSession);
  LastRV := LibraryFunctionList.CK_C_CloseSession(session.FhSession);
end;

procedure TPKCS11Token.CloseAllSessions();
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CloseAllSessions, FN_NAME_C_CloseAllSessions);
  LastRV := LibraryFunctionList.CK_C_CloseAllSessions(slotID);
end;


function TPKCS11Token.GetXLabel(): string;
var
  retval: string;
  info: CK_TOKEN_INFO;
begin
  retval := '';

  if (GetTokenInfo(info)) then
    begin
    retval := UTF8CHARArrayToString(TCK_UTF8CHARArray_32(info.Xlabel));
    end;

  Result := retval;
end;

function TPKCS11Token.GetManufacturerID(): string;
var
  retval: string;
  info: CK_TOKEN_INFO;
begin
  retval := '';

  if (GetTokenInfo(info)) then
    begin
    retval := UTF8CHARArrayToString(TCK_UTF8CHARArray_32(info.manufacturerID));
    end;

  Result := retval;
end;

function TPKCS11Token.GetModel(): string;
var
  retval: string;
  info: CK_TOKEN_INFO;
begin
  retval := '';

  if (GetTokenInfo(info)) then
    begin
    retval := UTF8CHARArrayToString(TCK_UTF8CHARArray_16(info.model));
    end;

  Result := retval;
end;

function TPKCS11Token.GetSerialNumber(): string;
var
  retval: string;
  info: CK_TOKEN_INFO;
begin
  retval := '';

  if (GetTokenInfo(info)) then
    begin
    retval := UTF8CHARArrayToString(TCK_UTF8CHARArray_16(info.serialNumber));
    end;

  Result := retval;
end;

function TPKCS11Token.GetFlags(): CK_FLAGS;
var
  retval: CK_FLAGS;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.Flags;
    end;

  Result := retval;
end;

function TPKCS11Token.GetMaxSessionCount(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulMaxSessionCount;
    end;

  Result := retval;
end;

function TPKCS11Token.GetSessionCount(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulSessionCount;
    end;

  Result := retval;
end;

function TPKCS11Token.GetMaxRwSessionCount(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulMaxRwSessionCount;
    end;

  Result := retval;
end;

function TPKCS11Token.GetRwSessionCount(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulRwSessionCount;
    end;

  Result := retval;
end;

function TPKCS11Token.GetMaxPinLen(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulMaxPinLen;
    end;

  Result := retval;
end;

function TPKCS11Token.GetMinPinLen(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulMinPinLen;
    end;

  Result := retval;
end;

function TPKCS11Token.GetTotalPublicMemory(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulTotalPublicMemory;
    end;

  Result := retval;
end;

function TPKCS11Token.GetFreePublicMemory(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulFreePublicMemory;
    end;

  Result := retval;
end;

function TPKCS11Token.GetTotalPrivateMemory(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulTotalPrivateMemory;
    end;

  Result := retval;
end;

function TPKCS11Token.GetFreePrivateMemory(): cardinal;
var
  retval: cardinal;
  info: CK_TOKEN_INFO;
begin
  retval := 0;

  if (GetTokenInfo(info)) then
    begin
    retval := info.ulFreePrivateMemory;
    end;

  Result := retval;
end;

function TPKCS11Token.GetHardwareVersion(): CK_VERSION;
var
  retval: CK_VERSION;
  info: CK_TOKEN_INFO;
begin
  retval := NULL_VERSION;

  if (GetTokenInfo(info)) then
    begin
    retval := info.hardwareVersion;
    end;

  Result := retval;
end;

function TPKCS11Token.GetFirmwareVersion(): CK_VERSION;
var
  retval: CK_VERSION;
  info: CK_TOKEN_INFO;
begin
  retval := NULL_VERSION;

  if (GetTokenInfo(info)) then
    begin
    retval := info.firmwareVersion;
    end;

  Result := retval;
end;

function TPKCS11Token.GetUTCTime(): TDateTime;
var
  info: CK_TOKEN_INFO;
  strDate: string;
  tmpChar: char;
  retval: TDateTime;
  yyyy: integer;
  mo: integer;
  dd: integer;
  hh: integer;
  mi: integer;
  ss: integer;
  i: integer;
begin
  retval := NULL_TIME;

  if (GetTokenInfo(info)) then
    begin
    strDate:= '';
    for i:=low(info.utcTime) to high(info.utcTime) do
    begin
      tmpChar := char(info.utcTime[i]);

      // This code only so it's easier to see what the value is when debugging
      //if (
      //    (tmpChar < #32) or
      //    (tmpChar > #127)
      //   ) then
      //begin
      //  tmpChar := '0';
      //end;

      strDate := strDate + tmpChar;
    end;
    
    // Testing date; should give 2nd Jan 2007 03:04:05
    // strDate := '2007010203040500';
    if (
        TryStrToInt(Copy(strDate, 1, 4), yyyy) and
        TryStrToInt(Copy(strDate, 5, 2), mo) and
        TryStrToInt(Copy(strDate, 7, 2), dd) and
        TryStrToInt(Copy(strDate, 9, 2), hh) and
        TryStrToInt(Copy(strDate, 11, 2), mi) and
        TryStrToInt(Copy(strDate, 13, 2), ss)
       ) then
      begin
      retval := EncodeDateTime(yyyy, mo, dd, hh, mi, ss, 00);
      end;
    end;

  Result := retval;
end;

function TPKCS11Token.GetCountMechanisms(): integer;
begin
  Result := _GetCountMechanisms();
end;

function TPKCS11Token._GetCountMechanisms(): integer;
var
  retval: integer;
  mechanismsArray: TPKCS11MechanismArray;
begin
  retval := 0;
  if GetMechanismList(mechanismsArray) then
    begin
    retval := length(mechanismsArray);
    end;

  Result := retval;
end;

function TPKCS11Token.GetMechanismList(var mechsArray: TPKCS11MechanismArray): boolean;
var
  ulCount: CK_ULONG;
  mechType: CK_MECHANISM_TYPE;
  ptrBuffer: CK_MECHANISM_TYPE_PTR;
  ptrBufferOffset: CK_MECHANISM_TYPE_PTR;
  bufferSize: integer;
  i: integer;
  mechObj: TPKCS11Mechanism;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetMechanismList, FN_NAME_C_GetMechanismList);

  if (FCachedMechanismsArray <> nil) then
    begin
    mechsArray := FCachedMechanismsArray;
    Result := TRUE;
    exit;
    end;

  // Get count...
  ulCount := 0;
  LastRV := LibraryFunctionList.CK_C_GetMechanismList(FSlotID, nil, @ulCount);
  if RVSuccess(LastRV) then
    begin
    // Get...
    bufferSize := (sizeof(FSlotID) * ulCount);
    ptrBuffer := allocmem(bufferSize);
    try
      LastRV := LibraryFunctionList.CK_C_GetMechanismList(FSlotID, ptrBuffer, @ulCount);
      if RVSuccess(LastRV) then
        begin
        SetLength(mechsArray, ulCount);
        ptrBufferOffset := ptrBuffer;
        for i:=1 to ulCount do
          begin
          mechObj:= TPKCS11Mechanism.Create();

          mechType := ptrBufferOffset^;
          mechObj.LibraryFunctionList := LibraryFunctionList;
          mechObj.SlotID := FSlotID;
          mechObj.MechanismType := mechType;

          mechsArray[i-1] := mechObj;

          ptrBufferOffset := Pointer(PChar(ptrBufferOffset) + sizeof(ptrBufferOffset^));
          end;

        FCachedMechanismsArray := mechsArray;
        end;

    finally
      FreeMem(ptrBuffer);
    end;
    end;

  Result := RVSuccess(LastRV);
end;

function TPKCS11Token.GetMechanismByIdx(idx: integer): TPKCS11Mechanism;
var
  retval: TPKCS11Mechanism;
  mechanismsArray: TPKCS11MechanismArray;
begin
  retval := nil;
  if GetMechanismList(mechanismsArray) then
    begin
    // Sanity check
    if ((low(mechanismsArray) + idx) > high(mechanismsArray)) then
      begin
      raise EPKCS11ListIndexOutOFBounds.Create(E_EPKCS11_LISTINDEXOUTOFBOUNDS);
      end;

    retval := mechanismsArray[(low(mechanismsArray) + idx)];
    end;

  Result := retval;
end;

function TPKCS11Token.GetMechanismByType(mechType: CK_MECHANISM_TYPE): TPKCS11Mechanism;
var
  retval: TPKCS11Mechanism;
  mechanismsArray: TPKCS11MechanismArray;
  i: integer;
begin
  retval := nil;
  if GetMechanismList(mechanismsArray) then
    begin
    for i:=low(mechanismsArray) to high(mechanismsArray) do
      begin
      if (mechanismsArray[i].MechanismType = mechType) then
        begin
        retval := mechanismsArray[i];
        break;
        end;
      end;
    end;

  Result := retval;
end;

procedure TPKCS11Token.SetCacheTokenInfo(flag: boolean);
begin
  FCacheTokenInfo := flag;

  // Flush any cached information
  if not(FCacheTokenInfo) then
    begin
    FlushCache();
    end;
end;

procedure TPKCS11Token.FlushCache();
begin
  FCachedTokenInfoPresent := FALSE;
  FillChar(FCachedTokenInfo, 0, sizeof(FCachedTokenInfo));
end;

function TPKCS11Token.InitToken(usePINPad: boolean; SOPIN: string; tokenLabel: string): boolean;
var
  useTokenLabel: string;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_InitToken, FN_NAME_C_InitToken);

  // Pad label out to 32 chars
  useTokenLabel := tokenLabel + StringOfChar(' ', (32 - length(tokenLabel)));

  if usePINPad then
    begin
    LastRV := LibraryFunctionList.CK_C_InitToken(
                                                FSlotID,
                                                NULL_PTR,
                                                0,
                                                CK_UTF8CHAR_PTR(PChar(useTokenLabel))
                                               );
    end
  else
    begin
    LastRV := LibraryFunctionList.CK_C_InitToken(
                                            FSlotID,
                                            CK_UTF8CHAR_PTR(PChar(SOPIN)),
                                            length(SOPIN),
                                            CK_UTF8CHAR_PTR(PChar(useTokenLabel))
                                           );
    end;
    
  Result := RVSuccess(LastRV);
end;

function TPKCS11Token.InitToken(tokenLabel: string): boolean;
begin
  Result := InitToken(TRUE, '', tokenLabel);
end;

function TPKCS11Token.InitToken(SOPIN: string; tokenLabel: string): boolean;
begin
  Result := InitToken(FALSE, SOPIN, tokenLabel);
end;

END.


