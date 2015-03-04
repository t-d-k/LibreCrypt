unit pkcs11_slot;

interface

uses
  pkcs11t,
  pkcs11_api,
  pkcs11_token;

type
  TPKCS11Slot = class(TPKCS11API)
  private
    FSlotID: integer;

  protected
    function GetDescription(): string;
    function GetManufacturerID(): string;
    function GetFlags(): CK_FLAGS;
    function GetHardwareVersion(): CK_VERSION;
    function GetFirmwareVersion(): CK_VERSION;

    // Flags...
    function GetTokenPresent(): boolean;
    function GetRemovableDevice(): boolean;
    function GetHWSlot(): boolean;

    function CheckInfoFlagSet(testFlag: integer): boolean;

  public
    property SlotID: integer read FSlotID write FSlotID;

    // WARNING: Calling this function will return a *new* TPKCS11Token object
    //          each time it's called.
    //          It's the *callers* responsibility to free off this object after
    //          use
    function Token(): TPKCS11Token;
    function GetSlotInfo(var info: CK_SLOT_INFO): boolean;

    property Description: string read GetDescription;
    property ManufacturerID: string read GetManufacturerID;
    property Flags: CK_FLAGS read GetFlags;
    property HardwareVersion: CK_VERSION read GetHardwareVersion;
    property FirmwareVersion: CK_VERSION read GetFirmwareVersion;

    // Flags
    property TokenPresent: boolean read GetTokenPresent;
    property RemovableDevice: boolean read GetRemovableDevice;
    property HWSlot: boolean read GetHWSlot;
  end;

  TPKCS11SlotArray = array of TPKCS11Slot;

implementation

uses
  SysUtils,
  SDUGeneral;

// Note: *Caller* is responsible for freeing returned attribute object
function TPKCS11Slot.Token(): TPKCS11Token;
begin
  if (not(TokenPresent)) then
    begin
    raise EPKCS11TokenNotPresent.Create(E_EPKCS11_TOKENNOTPRESENT);
    end;

  Result := TPKCS11Token.Create();
  Result.SlotID := SlotID;
  Result.LibraryFunctionList := LibraryFunctionList;


end;

function TPKCS11Slot.GetSlotInfo(var info: CK_SLOT_INFO): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetSlotInfo, FN_NAME_C_GetSlotInfo);

  LastRV := LibraryFunctionList.CK_C_GetSlotInfo(SlotID, @info);
  Result := RVSuccess(LastRV);
end;

function TPKCS11Slot.GetDescription(): string;
var
  info: CK_SLOT_INFO;
begin
  Result := '';

  if (GetSlotInfo(info)) then
    begin
    Result := UTF8CHARArrayToString(TCK_UTF8CHARArray_64(info.slotDescription));
    end;


end;

function TPKCS11Slot.GetManufacturerID(): string;
var
  info: CK_SLOT_INFO;
begin
  Result := '';

  if (GetSlotInfo(info)) then
    begin
    Result := UTF8CHARArrayToString(TCK_UTF8CHARArray_32(info.manufacturerID));
    end;


end;

function TPKCS11Slot.GetFlags(): CK_FLAGS;
var
  info: CK_SLOT_INFO;
begin
  Result := 0;

  if (GetSlotInfo(info)) then
    begin
    Result := info.Flags;
    end;


end;

function TPKCS11Slot.GetHardwareVersion(): CK_VERSION;
var
  info: CK_SLOT_INFO;
begin
  Result := NULL_VERSION;

  if (GetSlotInfo(info)) then
    begin
    Result := info.hardwareVersion;
    end;


end;

function TPKCS11Slot.GetFirmwareVersion(): CK_VERSION;
var
  info: CK_SLOT_INFO;
begin
  Result := NULL_VERSION;

  if (GetSlotInfo(info)) then
    begin
    Result := info.firmwareVersion;
    end;


end;

function TPKCS11Slot.CheckInfoFlagSet(testFlag: integer): boolean;
var
  info: CK_SLOT_INFO;
begin
  Result := FALSE;

  if (GetSlotInfo(info)) then
    begin
    Result := SDUBitWiseTest(info.flags, testFlag);
    end;


end;

function TPKCS11Slot.GetTokenPresent(): boolean;
begin
  Result := CheckInfoFlagSet(CKF_TOKEN_PRESENT);
end;

function TPKCS11Slot.GetRemovableDevice(): boolean;
begin
  Result := CheckInfoFlagSet(CKF_REMOVABLE_DEVICE);
end;

function TPKCS11Slot.GetHWSlot(): boolean;
begin
  Result := CheckInfoFlagSet(CKF_HW_SLOT);
end;

END.

