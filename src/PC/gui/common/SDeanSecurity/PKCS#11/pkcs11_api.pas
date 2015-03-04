unit pkcs11_api;

interface

uses
  SysUtils,
  Controls,
  pkcs11t,
  pkcs11f;

const
  NULL_PTR = nil;
  NULL_VERSION: CK_VERSION = (major: 0; minor: 0);

  // Function names
  FN_NAME_C_Initialize           = 'C_Initialize';
  FN_NAME_C_Finalize             = 'C_Finalize';
  FN_NAME_C_GetInfo              = 'C_GetInfo';
  FN_NAME_C_GetFunctionList      = 'C_GetFunctionList';
  FN_NAME_C_GetSlotList          = 'C_GetSlotList';
  FN_NAME_C_GetSlotInfo          = 'C_GetSlotInfo';
  FN_NAME_C_GetTokenInfo         = 'C_GetTokenInfo';
  FN_NAME_C_GetMechanismList     = 'C_GetMechanismList';
  FN_NAME_C_GetMechanismInfo     = 'C_GetMechanismInfo';
  FN_NAME_C_InitToken            = 'C_InitToken';
  FN_NAME_C_InitPIN              = 'C_InitPIN';
  FN_NAME_C_SetPIN               = 'C_SetPIN';
  FN_NAME_C_OpenSession          = 'C_OpenSession';
  FN_NAME_C_CloseSession         = 'C_CloseSession';
  FN_NAME_C_CloseAllSessions     = 'C_CloseAllSessions';
  FN_NAME_C_GetSessionInfo       = 'C_GetSessionInfo';
  FN_NAME_C_GetOperationState    = 'C_GetOperationState';
  FN_NAME_C_SetOperationState    = 'C_SetOperationState';
  FN_NAME_C_Login                = 'C_Login';
  FN_NAME_C_Logout               = 'C_Logout';
  FN_NAME_C_CreateObject         = 'C_CreateObject';
  FN_NAME_C_CopyObject           = 'C_CopyObject';
  FN_NAME_C_DestroyObject        = 'C_DestroyObject';
  FN_NAME_C_GetObjectSize        = 'C_GetObjectSize';
  FN_NAME_C_GetAttributeValue    = 'C_GetAttributeValue';
  FN_NAME_C_SetAttributeValue    = 'C_SetAttributeValue';
  FN_NAME_C_FindObjectsInit      = 'C_FindObjectsInit';
  FN_NAME_C_FindObjects          = 'C_FindObjects';
  FN_NAME_C_FindObjectsFinal     = 'C_FindObjectsFinal';
  FN_NAME_C_EncryptInit          = 'C_EncryptInit';
  FN_NAME_C_Encrypt              = 'C_Encrypt';
  FN_NAME_C_EncryptUpdate        = 'C_EncryptUpdate';
  FN_NAME_C_EncryptFinal         = 'C_EncryptFinal';
  FN_NAME_C_DecryptInit          = 'C_DecryptInit';
  FN_NAME_C_Decrypt              = 'C_Decrypt';
  FN_NAME_C_DecryptUpdate        = 'C_DecryptUpdate';
  FN_NAME_C_DecryptFinal         = 'C_DecryptFinal';
  FN_NAME_C_DigestInit           = 'C_DigestInit';
  FN_NAME_C_Digest               = 'C_Digest';
  FN_NAME_C_DigestUpdate         = 'C_DigestUpdate';
  FN_NAME_C_DigestKey            = 'C_DigestKey';
  FN_NAME_C_DigestFinal          = 'C_DigestFinal';
  FN_NAME_C_SignInit             = 'C_SignInit';
  FN_NAME_C_Sign                 = 'C_Sign';
  FN_NAME_C_SignUpdate           = 'C_SignUpdate';
  FN_NAME_C_SignFinal            = 'C_SignFinal';
  FN_NAME_C_SignRecoverInit      = 'C_SignRecoverInit';
  FN_NAME_C_SignRecover          = 'C_SignRecover';
  FN_NAME_C_VerifyInit           = 'C_VerifyInit';
  FN_NAME_C_Verify               = 'C_Verify';
  FN_NAME_C_VerifyUpdate         = 'C_VerifyUpdate';
  FN_NAME_C_VerifyFinal          = 'C_VerifyFinal';
  FN_NAME_C_VerifyRecoverInit    = 'C_VerifyRecoverInit';
  FN_NAME_C_VerifyRecover        = 'C_VerifyRecover';
  FN_NAME_C_DigestEncryptUpdate  = 'C_DigestEncryptUpdate';
  FN_NAME_C_DecryptDigestUpdate  = 'C_DecryptDigestUpdate';
  FN_NAME_C_SignEncryptUpdate    = 'C_SignEncryptUpdate';
  FN_NAME_C_DecryptVerifyUpdate  = 'C_DecryptVerifyUpdate';
  FN_NAME_C_GenerateKey          = 'C_GenerateKey';
  FN_NAME_C_GenerateKeyPair      = 'C_GenerateKeyPair';
  FN_NAME_C_WrapKey              = 'C_WrapKey';
  FN_NAME_C_UnwrapKey            = 'C_UnwrapKey';
  FN_NAME_C_DeriveKey            = 'C_DeriveKey';
  FN_NAME_C_SeedRandom           = 'C_SeedRandom';
  FN_NAME_C_GenerateRandom       = 'C_GenerateRandom';
  FN_NAME_C_GetFunctionStatus    = 'C_GetFunctionStatus';
  FN_NAME_C_CancelFunction       = 'C_CancelFunction';
  FN_NAME_C_WaitForSlotEvent     = 'C_WaitForSlotEvent';

resourcestring
  E_EPKCS11_LISTINDEXOUTOFBOUNDS = 'List index out of bounds';
  E_EPKCS11_TOKENNOTPRESENT = 'Token not present';
  E_EPKCS11_DLLNOTFOUND = 'Unable to locate/load PKCS#11 library: %1';
  E_EPKCS11_DLLNOAPI = 'Library specified doesn''t support the PKCS#11 API';

type
  EPKCS11Error = Exception;

  EPKCS11DLLBad = EPKCS11Error;
  EPKCS11DLLNotFound = EPKCS11DLLBad;
  EPKCS11DLLNoAPI = EPKCS11DLLBad;

  EPKCS11FnNotAvailable = EPKCS11Error;
  EPKCS11ListIndexOutOFBounds = EPKCS11Error;
  EPKCS11TokenNotPresent = EPKCS11Error;

  TPKCS11SlotEvent = procedure (Sender: TObject; SlotID: integer) of object;

  TCK_UTF8CHARArray_16 = array [0..15] of CK_UTF8CHAR;
  TCK_UTF8CHARArray_32 = array [0..31] of CK_UTF8CHAR;
  TCK_UTF8CHARArray_64 = array [0..63] of CK_UTF8CHAR;

  TPKCS11API = class
  private
  protected
    FFunctionList: CK_FUNCTION_LIST;
    procedure CheckFnAvailable(fnPtr: Pointer; fnName: string);

    // Conversion functions...
    function UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_16): string; overload;
    function UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_32): string; overload;
    function UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_64): string; overload;
    function CK_DATEToDate(ckDate: CK_DATE): TDate;
    // Returns: CK_Date version of Delphi date, set to 00000000
    function DateToCK_DATE(date: TDate): CK_DATE;
  public
    LastRV: CK_RV;

    property LibraryFunctionList: CK_FUNCTION_LIST read FFunctionList write FFunctionList;

    constructor Create(api: TPKCS11API); overload;
    destructor  Destroy(); override;

    function RVSuccess(rv: CK_RV): boolean;
    function RVToString(rv: CK_RV): string;
    function LastRVString(): string;

    // Conversion functions...
    function VersionToString(version: CK_VERSION): string;

  end;


implementation

uses
  Windows;

constructor TPKCS11API.Create(api: TPKCS11API);
begin
  inherited Create();

  FFunctionList := api.LibraryFunctionList;
end;

destructor  TPKCS11API.Destroy();
begin
  inherited;
end;

procedure TPKCS11API.CheckFnAvailable(fnPtr: Pointer; fnName: string);
begin
  if (fnPtr = nil) then
    begin
    raise EPKCS11FnNotAvailable.Create(fnName+' API function not available');
    end;
end;

function TPKCS11API.RVSuccess(rv: CK_RV): boolean;
begin
  Result := (rv = CKR_OK);
end;


function TPKCS11API.LastRVString(): string;
begin
  Result := RVToString(LastRV);
end;

function TPKCS11API.RVToString(rv: CK_RV): string;
begin
  Result := '0x'+inttohex(rv, 8);

  case rv of
    CKR_OK:                               Result := 'CKR_OK';
    CKR_CANCEL:                           Result := 'CKR_CANCEL';
    CKR_HOST_MEMORY:                      Result := 'CKR_HOST_MEMORY';
    CKR_SLOT_ID_INVALID:                  Result := 'CKR_SLOT_ID_INVALID';
    CKR_GENERAL_ERROR:                    Result := 'CKR_GENERAL_ERROR';
    CKR_FUNCTION_FAILED:                  Result := 'CKR_FUNCTION_FAILED';
    CKR_ARGUMENTS_BAD:                    Result := 'CKR_ARGUMENTS_BAD';
    CKR_NO_EVENT:                         Result := 'CKR_NO_EVENT';
    CKR_NEED_TO_CREATE_THREADS:           Result := 'CKR_NEED_TO_CREATE_THREADS';
    CKR_CANT_LOCK:                        Result := 'CKR_CANT_LOCK';
    CKR_ATTRIBUTE_READ_ONLY:              Result := 'CKR_ATTRIBUTE_READ_ONLY';
    CKR_ATTRIBUTE_SENSITIVE:              Result := 'CKR_ATTRIBUTE_SENSITIVE';
    CKR_ATTRIBUTE_TYPE_INVALID:           Result := 'CKR_ATTRIBUTE_TYPE_INVALID';
    CKR_ATTRIBUTE_VALUE_INVALID:          Result := 'CKR_ATTRIBUTE_VALUE_INVALID';
    CKR_DATA_INVALID:                     Result := 'CKR_DATA_INVALID';
    CKR_DATA_LEN_RANGE:                   Result := 'CKR_DATA_LEN_RANGE';
    CKR_DEVICE_ERROR:                     Result := 'CKR_DEVICE_ERROR';
    CKR_DEVICE_MEMORY:                    Result := 'CKR_DEVICE_MEMORY';
    CKR_DEVICE_REMOVED:                   Result := 'CKR_DEVICE_REMOVED';
    CKR_ENCRYPTED_DATA_INVALID:           Result := 'CKR_ENCRYPTED_DATA_INVALID';
    CKR_ENCRYPTED_DATA_LEN_RANGE:         Result := 'CKR_ENCRYPTED_DATA_LEN_RANGE';
    CKR_FUNCTION_CANCELED:                Result := 'CKR_FUNCTION_CANCELED';
    CKR_FUNCTION_NOT_PARALLEL:            Result := 'CKR_FUNCTION_NOT_PARALLEL';
    CKR_FUNCTION_NOT_SUPPORTED:           Result := 'CKR_FUNCTION_NOT_SUPPORTED';
    CKR_KEY_HANDLE_INVALID:               Result := 'CKR_KEY_HANDLE_INVALID';
    CKR_KEY_SIZE_RANGE:                   Result := 'CKR_KEY_SIZE_RANGE';
    CKR_KEY_TYPE_INCONSISTENT:            Result := 'CKR_KEY_TYPE_INCONSISTENT';
    CKR_KEY_NOT_NEEDED:                   Result := 'CKR_KEY_NOT_NEEDED';
    CKR_KEY_CHANGED:                      Result := 'CKR_KEY_CHANGED';
    CKR_KEY_NEEDED:                       Result := 'CKR_KEY_NEEDED';
    CKR_KEY_INDIGESTIBLE:                 Result := 'CKR_KEY_INDIGESTIBLE';
    CKR_KEY_FUNCTION_NOT_PERMITTED:       Result := 'CKR_KEY_FUNCTION_NOT_PERMITTED';
    CKR_KEY_NOT_WRAPPABLE:                Result := 'CKR_KEY_NOT_WRAPPABLE';
    CKR_KEY_UNEXTRACTABLE:                Result := 'CKR_KEY_UNEXTRACTABLE';
    CKR_MECHANISM_INVALID:                Result := 'CKR_MECHANISM_INVALID';
    CKR_MECHANISM_PARAM_INVALID:          Result := 'CKR_MECHANISM_PARAM_INVALID';
    CKR_OBJECT_HANDLE_INVALID:            Result := 'CKR_OBJECT_HANDLE_INVALID';
    CKR_OPERATION_ACTIVE:                 Result := 'CKR_OPERATION_ACTIVE';
    CKR_OPERATION_NOT_INITIALIZED:        Result := 'CKR_OPERATION_NOT_INITIALIZED';
    CKR_PIN_INCORRECT:                    Result := 'CKR_PIN_INCORRECT';
    CKR_PIN_INVALID:                      Result := 'CKR_PIN_INVALID';
    CKR_PIN_LEN_RANGE:                    Result := 'CKR_PIN_LEN_RANGE';
    CKR_PIN_EXPIRED:                      Result := 'CKR_PIN_EXPIRED';
    CKR_PIN_LOCKED:                       Result := 'CKR_PIN_LOCKED';
    CKR_SESSION_CLOSED:                   Result := 'CKR_SESSION_CLOSED';
    CKR_SESSION_COUNT:                    Result := 'CKR_SESSION_COUNT';
    CKR_SESSION_HANDLE_INVALID:           Result := 'CKR_SESSION_HANDLE_INVALID';
    CKR_SESSION_PARALLEL_NOT_SUPPORTED:   Result := 'CKR_SESSION_PARALLEL_NOT_SUPPORTED';
    CKR_SESSION_READ_ONLY:                Result := 'CKR_SESSION_READ_ONLY';
    CKR_SESSION_EXISTS:                   Result := 'CKR_SESSION_EXISTS';
    CKR_SESSION_READ_ONLY_EXISTS:         Result := 'CKR_SESSION_READ_ONLY_EXISTS';
    CKR_SESSION_READ_WRITE_SO_EXISTS:     Result := 'CKR_SESSION_READ_WRITE_SO_EXISTS';
    CKR_SIGNATURE_INVALID:                Result := 'CKR_SIGNATURE_INVALID';
    CKR_SIGNATURE_LEN_RANGE:              Result := 'CKR_SIGNATURE_LEN_RANGE';
    CKR_TEMPLATE_INCOMPLETE:              Result := 'CKR_TEMPLATE_INCOMPLETE';
    CKR_TEMPLATE_INCONSISTENT:            Result := 'CKR_TEMPLATE_INCONSISTENT';
    CKR_TOKEN_NOT_PRESENT:                Result := 'CKR_TOKEN_NOT_PRESENT';
    CKR_TOKEN_NOT_RECOGNIZED:             Result := 'CKR_TOKEN_NOT_RECOGNIZED';
    CKR_TOKEN_WRITE_PROTECTED:            Result := 'CKR_TOKEN_WRITE_PROTECTED';
    CKR_UNWRAPPING_KEY_HANDLE_INVALID:    Result := 'CKR_UNWRAPPING_KEY_HANDLE_INVALID';
    CKR_UNWRAPPING_KEY_SIZE_RANGE:        Result := 'CKR_UNWRAPPING_KEY_SIZE_RANGE';
    CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT: Result := 'CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT';
    CKR_USER_ALREADY_LOGGED_IN:           Result := 'CKR_USER_ALREADY_LOGGED_IN';
    CKR_USER_NOT_LOGGED_IN:               Result := 'CKR_USER_NOT_LOGGED_IN';
    CKR_USER_PIN_NOT_INITIALIZED:         Result := 'CKR_USER_PIN_NOT_INITIALIZED';
    CKR_USER_TYPE_INVALID:                Result := 'CKR_USER_TYPE_INVALID';
    CKR_USER_ANOTHER_ALREADY_LOGGED_IN:   Result := 'CKR_USER_ANOTHER_ALREADY_LOGGED_IN';
    CKR_USER_TOO_MANY_TYPES:              Result := 'CKR_USER_TOO_MANY_TYPES';
    CKR_WRAPPED_KEY_INVALID:              Result := 'CKR_WRAPPED_KEY_INVALID';
    CKR_WRAPPED_KEY_LEN_RANGE:            Result := 'CKR_WRAPPED_KEY_LEN_RANGE';
    CKR_WRAPPING_KEY_HANDLE_INVALID:      Result := 'CKR_WRAPPING_KEY_HANDLE_INVALID';
    CKR_WRAPPING_KEY_SIZE_RANGE:          Result := 'CKR_WRAPPING_KEY_SIZE_RANGE';
    CKR_WRAPPING_KEY_TYPE_INCONSISTENT:   Result := 'CKR_WRAPPING_KEY_TYPE_INCONSISTENT';
    CKR_RANDOM_SEED_NOT_SUPPORTED:        Result := 'CKR_RANDOM_SEED_NOT_SUPPORTED';
    CKR_RANDOM_NO_RNG:                    Result := 'CKR_RANDOM_NO_RNG';
    CKR_DOMAIN_PARAMS_INVALID:            Result := 'CKR_DOMAIN_PARAMS_INVALID';
    CKR_BUFFER_TOO_SMALL:                 Result := 'CKR_BUFFER_TOO_SMALL';
    CKR_SAVED_STATE_INVALID:              Result := 'CKR_SAVED_STATE_INVALID';
    CKR_INFORMATION_SENSITIVE:            Result := 'CKR_INFORMATION_SENSITIVE';
    CKR_STATE_UNSAVEABLE:                 Result := 'CKR_STATE_UNSAVEABLE';
    CKR_CRYPTOKI_NOT_INITIALIZED:         Result := 'CKR_CRYPTOKI_NOT_INITIALIZED';
    CKR_CRYPTOKI_ALREADY_INITIALIZED:     Result := 'CKR_CRYPTOKI_ALREADY_INITIALIZED';
    CKR_MUTEX_BAD:                        Result := 'CKR_MUTEX_BAD';
    CKR_MUTEX_NOT_LOCKED:                 Result := 'CKR_MUTEX_NOT_LOCKED';
    CKR_NEW_PIN_MODE:                     Result := 'CKR_NEW_PIN_MODE';
    CKR_NEXT_OTP:                         Result := 'CKR_NEXT_OTP';
    CKR_FUNCTION_REJECTED:                Result := 'CKR_FUNCTION_REJECTED';
    CKR_VENDOR_DEFINED:                   Result := 'CKR_VENDOR_DEFINED';
  end;


end;

function TPKCS11API.UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_16): string;
var
  tmpChar: char;
  i: integer;
begin
  Result:= '';
  for i:=low(arr) to high(arr) do
    begin
    tmpChar := char(arr[i]);
    if (
        (tmpChar < #32) or
        (tmpChar > #127)
       ) then
    begin
      tmpChar := '.';
    end;

    Result:= Result + tmpChar;
    end;


end;

function TPKCS11API.UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_32): string;
const
  NON_PRINTABLE_CHAR_PLACEHOLDER = '.';
var
  tmpChar: char;
  i: integer;
begin
  Result:= '';
  for i:=low(arr) to high(arr) do
    begin
    tmpChar := char(arr[i]);
    if (
        (tmpChar < #32) or
        (tmpChar > #127)
       ) then
    begin
      tmpChar := NON_PRINTABLE_CHAR_PLACEHOLDER;
    end;

    Result:= Result + tmpChar;
    end;


end;

function TPKCS11API.UTF8CHARArrayToString(arr: TCK_UTF8CHARArray_64): string;
var
  tmpChar: char;
  i: integer;
begin
  Result:= '';
  for i:=low(arr) to high(arr) do
    begin
    tmpChar := char(arr[i]);
    if (
        (tmpChar < #32) or
        (tmpChar > #127)
       ) then
    begin
      tmpChar := '.';
    end;

    Result:= Result + tmpChar;
    end;


end;

function TPKCS11API.VersionToString(version: CK_VERSION): string;
begin
  Result := 'v'+
            inttostr(version.major)+
            '.'+
            inttostr(version.minor);
end;

// Returns: Delphi version of date, set to zero on failure
function TPKCS11API.CK_DATEToDate(ckDate: CK_DATE): TDate;
var
  stryyyy: string;
  strmo: string;
  strdd: string;
  yyyy: integer;
  mo: integer;
  dd: integer;
begin
  Result := 0;

  stryyyy := char(ckDate.year[0]) +
             char(ckDate.year[1]) +
             char(ckDate.year[2]) +
             char(ckDate.year[3]);
  strmo   := char(ckDate.month[0]) +
             char(ckDate.month[1]);
  strdd   := char(ckDate.day[0]) +
             char(ckDate.day[1]);

  if (
      TryStrToInt(stryyyy, yyyy) and
      TryStrToInt(strmo, mo) and
      TryStrToInt(strdd, dd)
     ) then
    begin
    Result := EncodeDate(yyyy, mo, dd);
    end;


end;

// Returns: CK_Date version of Delphi date, set to 00000000
function TPKCS11API.DateToCK_DATE(date: TDate): CK_DATE;
var
  yyyy: word;
  mo: word;
  dd: word;
  tmpStr: string;
begin
  yyyy := 0;
  mo   := 0;
  dd   := 0;
  if (date <> 0) then
    begin
    DecodeDate(date, yyyy, mo, dd);
    end;

  tmpStr := Format('%.4d', [integer(yyyy)]);
  Result.year[0] := ord(tmpStr[1]);
  Result.year[1] := ord(tmpStr[2]);
  Result.year[2] := ord(tmpStr[3]);
  Result.year[3] := ord(tmpStr[4]);

  tmpStr := Format('%.2d', [integer(mo)]);
  Result.month[0] := ord(tmpStr[1]);
  Result.month[1] := ord(tmpStr[2]);

  tmpStr := Format('%.2d', [integer(dd)]);
  Result.day[0] := ord(tmpStr[1]);
  Result.day[1] := ord(tmpStr[2]);


end;


END.

