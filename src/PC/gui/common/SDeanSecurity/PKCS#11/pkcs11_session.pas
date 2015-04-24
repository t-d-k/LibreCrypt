unit pkcs11_session;

interface

uses
  pkcs11t,
  pkcs11_api,
  pkcs11_object,
  pkcs11_attribute,
  pkcs11_mechanism,
  //sdu
  sdugeneral;

type
  TPKCS11UserType = (utUser, utSecurityOfficer, utContextSpecific);

const
  PKCS11_USER_TYPE: array [TPKCS11UserType] of CK_USER_TYPE = (
    CKU_USER,
    CKU_SO,
    CKU_CONTEXT_SPECIFIC
    );

type
  TPKCS11SessionState = (
    ssROPublic,
    ssROUserFunctions,
    ssRWPublic,
    ssRWUserFunctions,
    ssRWSOFunctions
    );

const
  PKCS11_SESSION_STATE: array [TPKCS11SessionState] of CK_STATE = (
    CKS_RO_PUBLIC_SESSION,
    CKS_RO_USER_FUNCTIONS,
    CKS_RW_PUBLIC_SESSION,
    CKS_RW_USER_FUNCTIONS,
    CKS_RW_SO_FUNCTIONS
    );

resourcestring
  SESSION_STATE_RO_PUBLIC = 'Read-only Public';
  SESSION_STATE_RO_USER   = 'Read-only User';
  SESSION_STATE_RW_PUBLIC = 'Read/write Public';
  SESSION_STATE_RW_USER   = 'Read/write User';
  SESSION_STATE_RW_SO     = 'Read/write Security Officer';

const
  PKCS11_SESSION_STATE_DESC: array [TPKCS11SessionState] of String = (
    SESSION_STATE_RO_PUBLIC,
    SESSION_STATE_RO_USER,
    SESSION_STATE_RW_PUBLIC,
    SESSION_STATE_RW_USER,
    SESSION_STATE_RW_SO
    );

type
  TPKCS11Session = class (TPKCS11API)
  private
  protected
    FSlotID: Integer;
    FLastEncryptDecryptLen: Integer;

    // Returns TRUE/FALSE, depending on whether the session is open/valid or
    // not
    function GetOpen(): Boolean;

    function GetState(): TPKCS11SessionState;
    function GetStateStr(): String;
    function GetFlags(): CK_ULONG;
    function GetDeviceError(): CK_ULONG;

    function GetReadOnly(): Boolean;

    function Login(userType: TPKCS11UserType; usePINPad: Boolean; PIN: Ansistring): Boolean;
      overload;
    function GetLoggedIn(): Boolean;

    function InitPIN(usePINPad: Boolean; PIN: Ansistring): Boolean; overload;
    function SetPIN(usePINPad: Boolean; oldPIN: Ansistring; newPIN: Ansistring): Boolean; overload;

    // Conversion functions...
    function CK_USER_TYPEToUserType(ckUserType: CK_USER_TYPE): TPKCS11UserType;
    function UserTypeToCK_USER_TYPE(userType: TPKCS11UserType): CK_USER_TYPE;
    function CK_STATEToState(ckState: CK_STATE): TPKCS11SessionState;
    function StateToCK_STATE(state: TPKCS11SessionState): CK_STATE;

  public
    FhSession: CK_SESSION_HANDLE;

    property Open: Boolean Read GetOpen;

    // Some PKCS#11 providers return incorrect slot ID in the info
    // This method returns the *real* slot ID...
    property SlotID: Integer Read FSlotID Write FSlotID;
    // ...and this method returns the slot ID from the session's info from the
    // PKCS#11 API
    function SlotIDFromInfo(): Integer;

    property State: TPKCS11SessionState Read GetState;
    property StateStr: String Read GetStateStr;
    property Flags: CK_FLAGS Read GetFlags;
    property DeviceError: CK_ULONG Read GetDeviceError;

    function GetSessionInfo(var info: CK_SESSION_INFO): Boolean;

    property ReadOnly: Boolean Read GetReadOnly;

    function SeedRandom(byteCount: Integer; seedData: Ansistring): Boolean;
    function GenerateRandom(byteCount: Integer; var randomData: TSDUBytes): Boolean;
    //    function GenerateRandom(byteCount: integer; var randomData: AnsiString): boolean;

    function GetOperationState(var operationState: Ansistring): Boolean;
    function SetOperationState(operationState: Ansistring; encryptionKeyObj: TPKCS11Object;
      authenticationKey: TPKCS11Object): Boolean;

    function Login(userType: TPKCS11UserType): Boolean; overload;
    function Login(userType: TPKCS11UserType; PIN: Ansistring): Boolean; overload;
    procedure Logout();

    property LoggedIn: Boolean Read GetLoggedIn;

    function InitPIN(): Boolean; overload;
    function InitPIN(PIN: Ansistring): Boolean; overload;
    function SetPIN(): Boolean; overload;
    function SetPIN(oldPIN: Ansistring; newPIN: Ansistring): Boolean; overload;

    function FindObjectsInit(): Boolean;
    function FindNextObject(): TPKCS11Object;
    function FindObjectsFinal(): Boolean;

    function GenerateKey(mechanism: CK_MECHANISM_TYPE;
      attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object; overload;
    function GenerateKey(mechanism: TPKCS11Mechanism;
      attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object; overload;
    function GenerateKeyPair(
      mechanism: CK_MECHANISM_TYPE;
      publicAttrTemplate: TPKCS11AttributeTemplate;
      privateAttrTemplate: TPKCS11AttributeTemplate;
      var publicKey: TPKCS11Object;
      var privateKey: TPKCS11Object
      ): Boolean; overload;
    function GenerateKeyPair(
      mechanism: TPKCS11Mechanism;
      publicAttrTemplate: TPKCS11AttributeTemplate;
      privateAttrTemplate: TPKCS11AttributeTemplate;
      var publicKey: TPKCS11Object;
      var privateKey: TPKCS11Object
      ): Boolean; overload;
    function CreateObject(attrObjs: array of TPKCS11Attribute): TPKCS11Object; overload;
    function CreateObject(attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object; overload;
    function CopyObject(obj: TPKCS11Object): TPKCS11Object;
    function DestroyObject(obj: TPKCS11Object): Boolean;

    function GetObject(application: String; objLabel: String): TPKCS11Object;
    function GetObjects(objClass: CK_OBJECT_CLASS): TPKCS11ObjectArray; overload;
    function GetObjects(application: String): TPKCS11ObjectArray; overload;
    function GetObjects(application: String; objLabel: String): TPKCS11ObjectArray; overload;

    function EncryptInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean; overload;
    function EncryptInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean; overload;
    function Encrypt(plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
    function EncryptUpdate(plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
    function EncryptFinal(var cyphertext: Ansistring): Boolean;
    function DecryptInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean; overload;
    function DecryptInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean; overload;
    function Decrypt(cyphertext: Ansistring; var plaintext: Ansistring): Boolean;
    function DecryptUpdate(cyphertext: Ansistring; var plaintext: Ansistring): Boolean;
    function DecryptFinal(var plaintext: Ansistring): Boolean;

    function DigestInit(mechanism: CK_MECHANISM_TYPE): Boolean; overload;
    function DigestInit(mechanism: TPKCS11Mechanism): Boolean; overload;
    function Digest(data: Ansistring; var digest: Ansistring): Boolean;
    function DigestUpdate(data: Ansistring): Boolean;
    function DigestKey(data: Ansistring; keyObj: TPKCS11Object): Boolean;
    function DigestFinal(var digest: Ansistring): Boolean;

    function SignInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean; overload;
    function SignInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean; overload;
    function Sign(data: Ansistring; var signature: Ansistring): Boolean;
    function SignUpdate(data: Ansistring): Boolean;
    function SignFinal(var signature: Ansistring): Boolean;
    function SignRecoverInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
      overload;
    function SignRecoverInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
      overload;
    function SignRecover(data: Ansistring; var signature: Ansistring): Boolean;
    function VerifyInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean; overload;
    function VerifyInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean; overload;
    function Verify(data: Ansistring; signature: Ansistring): Boolean;
    function VerifyUpdate(data: Ansistring): Boolean;
    function VerifyFinal(signature: Ansistring): Boolean;
    function VerifyRecoverInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
      overload;
    function VerifyRecoverInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
      overload;
    function VerifyRecover(data: Ansistring; var signature: Ansistring): Boolean;

    function DigestEncryptUpdate(part: Ansistring; var encryptedPart: Ansistring): Boolean;
    function DecryptDigestUpdate(pEncryptedPart: Ansistring; var Part: Ansistring): Boolean;
    function SignEncryptUpdate(part: Ansistring; var encryptedPart: Ansistring): Boolean;
    function DecryptVerifyUpdate(pEncryptedPart: Ansistring; var Part: Ansistring): Boolean;

    function WrapKey(mechanism: CK_MECHANISM_TYPE; wrappingKeyObj: TPKCS11Object;
      keyObj: TPKCS11Object; var wrappedKey: Ansistring): Boolean;
    function UnwrapKey(mechanism: CK_MECHANISM_TYPE; unwrappingKeyObj: TPKCS11Object;
      wrappedKey: Ansistring; attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
    function DeriveKey(mechanism: CK_MECHANISM_TYPE; baseKey: TPKCS11Object;
      attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;

    function GetFunctionStatus(): Boolean;
    function CancelFunction(): Boolean;

    procedure CloseSession();
  end;


implementation

uses
  Math, Shredder,
  SysUtils;

const
  PKCS11_IGNORE_PARAMETER = '!!IGNORE_THIS_PARAMETER!!';

  // Buffer lengths
  // These should be set to suitably high values to ensure the buffers used are
  // always sifficiently large enough
  // All values in *bytes*
  BUFFER_LEN_DIGEST              = 8192;
  BUFFER_LEN_SIGN                = 8192;
  BUFFER_LEN_WRAPPED_KEY         = 8192;
  BUFFER_LEN_MIN_ENCRYPT_DECRYPT = 8192;
  BUFFER_LEN_OP_STATE            = 8192;


function TPKCS11Session.GetSessionInfo(var info: CK_SESSION_INFO): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetSessionInfo, FN_NAME_C_GetSessionInfo);

  LastRV := LibraryFunctionList.CK_C_GetSessionInfo(FhSession, @info);
  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SeedRandom(byteCount: Integer; seedData: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SeedRandom, FN_NAME_C_SeedRandom);

  LastRV := LibraryFunctionList.CK_C_SeedRandom(
    FhSession,
    CK_BYTE_PTR(PByte(seedData)), byteCount
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.GenerateRandom(byteCount: Integer; var randomData: TSDUBytes): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GenerateRandom, FN_NAME_C_GenerateRandom);

  // Setup the returned string so it's large enough to store the random data
  SDUInitAndZeroBuffer(byteCount, randomData);
  //  randomData := StringOfChar(AnsiChar('X'), byteCount);

  LastRV := LibraryFunctionList.CK_C_GenerateRandom(
    FhSession,
    CK_BYTE_PTR(PByte(randomData)), byteCount
    );

  Result := RVSuccess(LastRV);
end;

{
function TPKCS11Session.GenerateRandom(byteCount: integer; var randomData: AnsiString): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GenerateRandom, FN_NAME_C_GenerateRandom);

  // Setup the returned string so it's large enough to store the random data
//   SDUInitAndZeroBuffer(byteCount, randomData );
  randomData := StringOfChar(AnsiChar('X'), byteCount);

  LastRV := LibraryFunctionList.CK_C_GenerateRandom(
                                                FhSession,
                                                CK_BYTE_PTR(PByte(randomData)),
                                                byteCount
                                               );

  Result := RVSuccess(LastRV);
end;    }

function TPKCS11Session.GetOperationState(var operationState: Ansistring): Boolean;
var
  tmpLen:  CK_ULONG;
  tmpData: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetOperationState, FN_NAME_C_DecryptVerifyUpdate);

  tmpData := StringOfChar(AnsiChar(#0), BUFFER_LEN_OP_STATE);
  tmpLen  := length(tmpData);

  LastRV := LibraryFunctionList.CK_C_GetOperationState(
    FhSession,
    CK_BYTE_PTR(PansiChar(tmpData)), @tmpLen
    );

  operationState := Copy(tmpData, 1, tmpLen);
  Overwrite(tmpData);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SetOperationState(operationState: Ansistring;
  encryptionKeyObj: TPKCS11Object; authenticationKey: TPKCS11Object): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SetOperationState, FN_NAME_C_SetOperationState);

  LastRV := LibraryFunctionList.CK_C_SetOperationState(
    FhSession,
    CK_BYTE_PTR(PansiChar(operationState)), length(
    operationState), encryptionKeyObj.FHandle,
    authenticationKey.FHandle
    );

  Result := RVSuccess(LastRV);
end;

// Log user in using PIN pad
function TPKCS11Session.Login(userType: TPKCS11UserType): Boolean;
begin
  Result := Login(userType, True, '');
end;

// Log user in using specified PIN
function TPKCS11Session.Login(userType: TPKCS11UserType; PIN: Ansistring): Boolean;
begin
  Result := Login(userType, False, PIN);
end;

function TPKCS11Session.Login(userType: TPKCS11UserType; usePINPad: Boolean;
  PIN: Ansistring): Boolean;
var
  pkcs11UserType: CK_USER_TYPE;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Login, FN_NAME_C_Login);

  pkcs11UserType := UserTypeToCK_USER_TYPE(userType);

  if usePINPad then begin
    LastRV := LibraryFunctionList.CK_C_Login(FhSession, pkcs11UserType, NULL_PTR, 0);
  end else begin
    LastRV := LibraryFunctionList.CK_C_Login(
      FhSession,
      pkcs11UserType,
      CK_UTF8CHAR_PTR(PansiChar(PIN)), length(
      PIN));
  end;

  Result := RVSuccess(LastRV);
end;

procedure TPKCS11Session.Logout();
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Logout, FN_NAME_C_Logout);
  LastRV := LibraryFunctionList.CK_C_Logout(FhSession);
end;

function TPKCS11Session.FindObjectsInit(): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_FindObjectsInit, FN_NAME_C_FindObjectsInit);
  LastRV := LibraryFunctionList.CK_C_FindObjectsInit(FhSession, NULL_PTR, 0);
  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.GetLoggedIn(): Boolean;
var
  currState: TPKCS11SessionState;
begin
  currState := State;
  Result    := (Open and (currState <> ssROPublic) and
    (currState <> ssRWPublic));
end;

function TPKCS11Session.GetReadOnly(): Boolean;
var
  currState: TPKCS11SessionState;
begin
  currState := State;
  Result    := (not (Open) or (currState = ssROPublic) or
    (currState = ssROUserFunctions));
end;

 // Important: It is the CALLERS responsibility to free off the object returned
 // Returns nil if no more/error
function TPKCS11Session.FindNextObject(): TPKCS11Object;
var
  hObject:       CK_OBJECT_HANDLE;
  ulObjectCount: CK_ULONG;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_FindObjects, FN_NAME_C_FindObjects);

  Result := nil;

  LastRV := LibraryFunctionList.CK_C_FindObjects(
    FhSession,
    @hObject, 1,
    @ulObjectCount
    );
  if (RVSuccess(LastRV) and (ulObjectCount <> 0)) then begin
    Result                     := TPKCS11Object.Create();
    Result.LibraryFunctionList := LibraryFunctionList;
    Result.FSessionHandle      := FhSession;
    Result.FHandle             := hObject;
  end;

  Result := Result;
end;

function TPKCS11Session.FindObjectsFinal(): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_FindObjectsFinal, FN_NAME_C_FindObjectsFinal);
  LastRV := LibraryFunctionList.CK_C_FindObjectsFinal(FhSession);
  Result := RVSuccess(LastRV);
end;


function TPKCS11Session.CreateObject(attrObjs: array of TPKCS11Attribute): TPKCS11Object;
var
  retval:       TPKCS11Object;
  newObjHandle: CK_OBJECT_HANDLE;
  ckAttributes: array of CK_ATTRIBUTE;
  bufferArray:  array of Ansistring;
  i:            Integer;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CreateObject, FN_NAME_C_CreateObject);

  retval := nil;

  setlength(ckAttributes, length(attrObjs));
  setlength(bufferArray, length(attrObjs));
  for i := low(attrObjs) to high(attrObjs) do begin
    bufferArray[i] := attrObjs[i].ValueAsRawData;

    ckAttributes[i].attrType   := attrObjs[i].AttribType;
    ckAttributes[i].pValue     := PansiChar(bufferArray[i]);
    ckAttributes[i].ulValueLen := length(bufferArray[i]);
  end;

  LastRV := LibraryFunctionList.CK_C_CreateObject(
    FhSession,
    @(ckAttributes[0]), length(attrObjs),
    @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.CreateObject(attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
var
  retval:           TPKCS11Object;
  newObjHandle:     CK_OBJECT_HANDLE;
  ckAttributes:     array of CK_ATTRIBUTE;
  bufferArray:      array of Ansistring;
  i:                Integer;
  attrPtr:          CK_ATTRIBUTE_PTR;
  currTemplateAttr: TPKCS11Attribute;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CreateObject, FN_NAME_C_CreateObject);

  retval := nil;

  attrPtr := NULL_PTR;
  if (attrTemplate.Count > 0) then begin
    setlength(ckAttributes, attrTemplate.Count);
    setlength(bufferArray, attrTemplate.Count);
    for i := 0 to (attrTemplate.Count - 1) do begin
      currTemplateAttr := attrTemplate.Attribute[i];

      bufferArray[i] := currTemplateAttr.ValueAsRawData;

      ckAttributes[i].attrType   := currTemplateAttr.AttribType;
      ckAttributes[i].pValue     := PansiChar(bufferArray[i]);
      ckAttributes[i].ulValueLen := length(bufferArray[i]);
    end;

    attrPtr := @(ckAttributes[0]);
  end;

  LastRV := LibraryFunctionList.CK_C_CreateObject(
    FhSession,
    attrPtr, attrTemplate.Count,
    @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.GenerateKey(mechanism: TPKCS11Mechanism;
  attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
begin
  Result := GenerateKey(mechanism.MechanismType, attrTemplate);
end;

function TPKCS11Session.GenerateKey(mechanism: CK_MECHANISM_TYPE;
  attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
var
  retval:           TPKCS11Object;
  newObjHandle:     CK_OBJECT_HANDLE;
  ckAttributes:     array of CK_ATTRIBUTE;
  bufferArray:      array of Ansistring;
  i:                Integer;
  mechStruct:       CK_MECHANISM;
  attrPtr:          CK_ATTRIBUTE_PTR;
  currTemplateAttr: TPKCS11Attribute;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GenerateKey, FN_NAME_C_GenerateKey);

  retval := nil;

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  attrPtr := NULL_PTR;
  if (attrTemplate.Count > 0) then begin
    setlength(ckAttributes, attrTemplate.Count);
    setlength(bufferArray, attrTemplate.Count);
    for i := 0 to (attrTemplate.Count - 1) do begin
      currTemplateAttr := attrTemplate.Attribute[i];

      bufferArray[i] := currTemplateAttr.ValueAsRawData;

      ckAttributes[i].attrType   := currTemplateAttr.AttribType;
      ckAttributes[i].pValue     := PansiChar(bufferArray[i]);
      ckAttributes[i].ulValueLen := length(bufferArray[i]);
    end;

    attrPtr := @(ckAttributes[0]);
  end;

  LastRV := LibraryFunctionList.CK_C_GenerateKey(
    FhSession,
    @mechStruct, attrPtr,
    attrTemplate.Count,
    @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.GenerateKeyPair(
  mechanism: TPKCS11Mechanism;
  publicAttrTemplate: TPKCS11AttributeTemplate;
  privateAttrTemplate: TPKCS11AttributeTemplate;
  var publicKey: TPKCS11Object;
  var privateKey: TPKCS11Object
  ): Boolean;
begin
  Result := GenerateKeyPair(mechanism.MechanismType,
    publicAttrTemplate, privateAttrTemplate,
    publicKey, privateKey
    );
end;

function TPKCS11Session.GenerateKeyPair(
  mechanism: CK_MECHANISM_TYPE;
  publicAttrTemplate: TPKCS11AttributeTemplate;
  privateAttrTemplate: TPKCS11AttributeTemplate;
  var publicKey: TPKCS11Object;
  var privateKey: TPKCS11Object
  ): Boolean;
var
  newObjHandlePublic:  CK_OBJECT_HANDLE;
  newObjHandlePrivate: CK_OBJECT_HANDLE;
  ckAttributesPublic:  array of CK_ATTRIBUTE;
  ckAttributesPrivate: array of CK_ATTRIBUTE;
  bufferArrayPublic:   array of Ansistring;
  bufferArrayPrivate:  array of Ansistring;
  i:                   Integer;
  mechStruct:          CK_MECHANISM;
  attrPtrPublic:       CK_ATTRIBUTE_PTR;
  attrPtrPrivate:      CK_ATTRIBUTE_PTR;
  currTemplateAttr:    TPKCS11Attribute;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GenerateKeyPair, FN_NAME_C_GenerateKeyPair);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  attrPtrPublic := NULL_PTR;
  if (publicAttrTemplate.Count > 0) then begin
    setlength(ckAttributesPublic, publicAttrTemplate.Count);
    setlength(bufferArrayPublic, publicAttrTemplate.Count);
    for i := 0 to (publicAttrTemplate.Count - 1) do begin
      currTemplateAttr := publicAttrTemplate.Attribute[i];

      bufferArrayPublic[i] := currTemplateAttr.ValueAsRawData;

      ckAttributesPublic[i].attrType   := currTemplateAttr.AttribType;
      ckAttributesPublic[i].pValue     := PansiChar(bufferArrayPublic[i]);
      ckAttributesPublic[i].ulValueLen := length(bufferArrayPublic[i]);
    end;

    attrPtrPublic := @(ckAttributesPublic[0]);
  end;

  attrPtrPrivate := NULL_PTR;
  if (privateAttrTemplate.Count > 0) then begin
    setlength(ckAttributesPrivate, privateAttrTemplate.Count);
    setlength(bufferArrayPrivate, privateAttrTemplate.Count);
    for i := 0 to (privateAttrTemplate.Count - 1) do begin
      currTemplateAttr := privateAttrTemplate.Attribute[i];

      bufferArrayPrivate[i] := currTemplateAttr.ValueAsRawData;

      ckAttributesPrivate[i].attrType   := currTemplateAttr.AttribType;
      ckAttributesPrivate[i].pValue     := PansiChar(bufferArrayPrivate[i]);
      ckAttributesPrivate[i].ulValueLen := length(bufferArrayPrivate[i]);
    end;

    attrPtrPrivate := @(ckAttributesPrivate[0]);
  end;

  LastRV := LibraryFunctionList.CK_C_GenerateKeyPair(
    FhSession,
    @mechStruct, attrPtrPublic,
    publicAttrTemplate.Count,
    attrPtrPrivate,
    privateAttrTemplate.Count,
    @newObjHandlePublic,
    @newObjHandlePrivate
    );
  if RVSuccess(LastRV) then begin
    publicKey                     := TPKCS11Object.Create();
    publicKey.LibraryFunctionList := LibraryFunctionList;
    publicKey.FSessionHandle      := FhSession;
    publicKey.FHandle             := newObjHandlePublic;

    privateKey                     := TPKCS11Object.Create();
    privateKey.LibraryFunctionList := LibraryFunctionList;
    privateKey.FSessionHandle      := FhSession;
    privateKey.FHandle             := newObjHandlePrivate;
  end;

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.CopyObject(obj: TPKCS11Object): TPKCS11Object;
var
  retval:       TPKCS11Object;
  newObjHandle: CK_OBJECT_HANDLE;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CopyObject, FN_NAME_C_CopyObject);

  retval := nil;
  LastRV := LibraryFunctionList.CK_C_CopyObject(
    FhSession,
    obj.FHandle, nil,
    0,
    @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.DestroyObject(obj: TPKCS11Object): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DestroyObject, FN_NAME_C_DestroyObject);
  LastRV := LibraryFunctionList.CK_C_DestroyObject(FhSession, obj.FHandle);
  Result := RVSuccess(LastRV);
end;

// Returns -1 on error
function TPKCS11Session.SlotIDFromInfo(): Integer;
var
  retval: Integer;
  info:   CK_SESSION_INFO;
begin
  retval := -1;

  if GetSessionInfo(info) then begin
    retval := info.SlotID;
  end;

  Result := retval;
end;

function TPKCS11Session.GetState(): TPKCS11SessionState;
var
  retval: TPKCS11SessionState;
  info:   CK_SESSION_INFO;
begin
  retval := ssROPublic;

  if GetSessionInfo(info) then begin
    retval := CK_STATEToState(info.State);
  end;

  Result := retval;
end;

function TPKCS11Session.GetStateStr(): String;
begin
  Result := PKCS11_SESSION_STATE_DESC[State];
end;

function TPKCS11Session.GetFlags(): CK_ULONG;
var
  retval: CK_FLAGS;
  info:   CK_SESSION_INFO;
begin
  retval := 0;

  if GetSessionInfo(info) then begin
    retval := info.Flags;
  end;

  Result := retval;
end;

function TPKCS11Session.GetDeviceError(): CK_ULONG;
var
  retval: CK_ULONG;
  info:   CK_SESSION_INFO;
begin
  retval := 0;

  if GetSessionInfo(info) then begin
    retval := info.ulDeviceError;
  end;

  Result := retval;
end;


function TPKCS11Session.UserTypeToCK_USER_TYPE(userType: TPKCS11UserType): CK_USER_TYPE;
begin
  Result := PKCS11_USER_TYPE[userType];
end;

function TPKCS11Session.CK_USER_TYPEToUserType(ckUserType: CK_USER_TYPE): TPKCS11UserType;
var
  retval: TPKCS11UserType;
  i:      TPKCS11UserType;
begin
  retval := utUser;

  for i := low(PKCS11_USER_TYPE) to high(PKCS11_USER_TYPE) do begin
    if (PKCS11_USER_TYPE[i] = ckUserType) then begin
      retval := i;
      break;
    end;
  end;

  Result := retval;
end;

function TPKCS11Session.StateToCK_STATE(state: TPKCS11SessionState): CK_STATE;
begin
  Result := PKCS11_SESSION_STATE[state];
end;

function TPKCS11Session.CK_STATEToState(ckState: CK_STATE): TPKCS11SessionState;
var
  retval: TPKCS11SessionState;
  i:      TPKCS11SessionState;
begin
  retval := ssROPublic;

  for i := low(PKCS11_SESSION_STATE) to high(PKCS11_SESSION_STATE) do begin
    if (PKCS11_SESSION_STATE[i] = ckState) then begin
      retval := i;
      break;
    end;
  end;

  Result := retval;
end;

 // Returns TRUE/FALSE, depending on whether the session is open/valid or
 // not
function TPKCS11Session.GetOpen(): Boolean;
var
  junk: CK_SESSION_INFO;
begin
  Result := GetSessionInfo(junk);
end;

function TPKCS11Session.InitPIN(usePINPad: Boolean; PIN: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_InitPIN, FN_NAME_C_InitPIN);

  LastRV := LibraryFunctionList.CK_C_InitPIN(
    FhSession,
    CK_UTF8CHAR_PTR(PansiChar(PIN)),
    length(
    PIN));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SetPIN(usePINPad: Boolean; oldPIN: Ansistring;
  newPIN: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SetPIN, FN_NAME_C_SetPIN);

  LastRV := LibraryFunctionList.CK_C_SetPIN(
    FhSession,
    CK_UTF8CHAR_PTR(pansichar(oldPIN)),
    length(oldPIN),
    CK_UTF8CHAR_PTR(pansichar(newPIN)),
    length(
    newPIN));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.InitPIN(): Boolean;
begin
  Result := InitPIN(True, '');
end;

function TPKCS11Session.InitPIN(PIN: Ansistring): Boolean;
begin
  Result := InitPIN(False, PIN);
end;

function TPKCS11Session.SetPIN(): Boolean;
begin
  Result := SetPIN(True, '', '');
end;

function TPKCS11Session.SetPIN(oldPIN: Ansistring; newPIN: Ansistring): Boolean;
begin
  Result := SetPIN(False, oldPIN, newPIN);
end;

 // Retrieve object with the application/label attributes set to those
 // specified
 // Note: *Caller* is responsible for freeing returned attribute object
function TPKCS11Session.GetObject(application: String; objLabel: String): TPKCS11Object;
var
  tmpObjs: TPKCS11ObjectArray;
  retval:  TPKCS11Object;
  i:       Integer;
begin
  retval := nil;

  tmpObjs := GetObjects(application, objLabel);
  if (length(tmpObjs) > 0) then begin
    retval := tmpObjs[low(tmpObjs)];

    // Ditch all remaining objects
    for i := (low(tmpObjs) + 1) to high(tmpObjs) do begin
      tmpObjs[i].Free();
    end;
  end;

  Result := retval;
end;

 // Retrieve object with the application/label attributes set to those
 // specified
 // Note: *Caller* is responsible for freeing *ALL* returned attribute objects
function TPKCS11Session.GetObjects(application: String): TPKCS11ObjectArray;
begin
  Result := GetObjects(application, PKCS11_IGNORE_PARAMETER);
end;

 // Retrieve object with the application/label attributes set to those
 // specified
 // Note: *Caller* is responsible for freeing *ALL* returned attribute objects
function TPKCS11Session.GetObjects(application: String; objLabel: String): TPKCS11ObjectArray;
var
  retval:    TPKCS11ObjectArray;
  currObj:   TPKCS11Object;
  attrApp:   TPKCS11Attribute;
  attrLabel: TPKCS11Attribute;
begin
  SetLength(retval, 0);

  if FindObjectsInit() then begin
    currObj := FindNextObject();
    while (currObj <> nil) do begin
      attrApp   := currObj.GetAttribute(CKA_APPLICATION);
      attrLabel := currObj.GetAttribute(CKA_LABEL);

      if ((attrApp <> nil) and (attrLabel <> nil)) then begin
        if (((application = PKCS11_IGNORE_PARAMETER) or
          (trim(attrApp.ValueAsString) = trim(application))) and
          ((objLabel = PKCS11_IGNORE_PARAMETER) or
          (trim(attrLabel.ValueAsString) = trim(objLabel)))) then begin
          SetLength(retval, (length(retval) + 1));
          retval[length(retval) - 1] := currObj;
        end else begin
          currObj.Free();
        end;
      end;

      if (attrApp <> nil) then begin
        attrApp.Free();
      end;
      if (attrLabel <> nil) then begin
        attrLabel.Free();
      end;

      currObj := FindNextObject();
    end;

    FindObjectsFinal();
  end;

  Result := retval;
end;

function TPKCS11Session.GetObjects(objClass: CK_OBJECT_CLASS): TPKCS11ObjectArray;
var
  retval:       TPKCS11ObjectArray;
  currObj:      TPKCS11Object;
  attrObjClass: TPKCS11Attribute;
begin
  SetLength(retval, 0);

  if FindObjectsInit() then begin
    currObj := FindNextObject();
    while (currObj <> nil) do begin
      attrObjClass := currObj.GetAttribute(CKA_CLASS);

      if (attrObjClass <> nil) then begin
        if (attrObjClass.ValueAsNumber = objClass) then begin
          SetLength(retval, (length(retval) + 1));
          retval[length(retval) - 1] := currObj;
        end else begin
          currObj.Free();
        end;
      end;

      if (attrObjClass <> nil) then begin
        attrObjClass.Free();
      end;

      currObj := FindNextObject();
    end;

    FindObjectsFinal();
  end;

  Result := retval;
end;

 // Important: Keep implementation of:
 //
 //              TPKCS11Token.CloseSession(...);
 //              TPKCS11Session.CloseSession();
 //
 //            in sync
procedure TPKCS11Session.CloseSession();
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CloseSession, FN_NAME_C_CloseSession);
  LastRV := LibraryFunctionList.CK_C_CloseSession(FhSession);
end;

function TPKCS11Session.EncryptInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_EncryptInit, FN_NAME_C_EncryptInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_EncryptInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.EncryptInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
begin
  Result := EncryptInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.Encrypt(plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
var
  tmpLen:        CK_ULONG;
  tmpCyphertext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Encrypt, FN_NAME_C_Encrypt);

  tmpLen                 := length(plaintext);
  FLastEncryptDecryptLen := tmpLen;
  tmpCyphertext          := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_Encrypt(
    FhSession,
    CK_BYTE_PTR(PAnsiChar(plaintext)), length(plaintext),
    CK_BYTE_PTR(PAnsiChar(tmpCyphertext)),
    @tmpLen);

  cyphertext := Copy(tmpCyphertext, 1, tmpLen);
  Overwrite(tmpCyphertext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.EncryptUpdate(plaintext: Ansistring; var cyphertext: Ansistring): Boolean;
var
  tmpLen:        CK_ULONG;
  tmpCyphertext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_EncryptUpdate, FN_NAME_C_EncryptUpdate);

  tmpLen                 := length(plaintext);
  FLastEncryptDecryptLen := tmpLen;
  tmpCyphertext          := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_EncryptUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(plaintext)), length(plaintext),
    CK_BYTE_PTR(pansichar(tmpCyphertext)),
    @tmpLen);

  cyphertext := Copy(tmpCyphertext, 1, tmpLen);
  Overwrite(tmpCyphertext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.EncryptFinal(var cyphertext: Ansistring): Boolean;
var
  tmpLen:        CK_ULONG;
  tmpCyphertext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_EncryptFinal, FN_NAME_C_EncryptFinal);

  tmpCyphertext := StringOfChar(AnsiChar(#0), max(FLastEncryptDecryptLen,
    BUFFER_LEN_MIN_ENCRYPT_DECRYPT));

  LastRV := LibraryFunctionList.CK_C_EncryptFinal(
    FhSession,
    CK_BYTE_PTR(pansichar(tmpCyphertext)), @tmpLen
    );

  cyphertext := Copy(tmpCyphertext, 1, tmpLen);
  Overwrite(tmpCyphertext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DecryptInit, FN_NAME_C_DecryptInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_DecryptInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
begin
  Result := DecryptInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.Decrypt(cyphertext: Ansistring; var plaintext: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpPlaintext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Decrypt, FN_NAME_C_Decrypt);

  tmpLen                 := length(cyphertext);
  FLastEncryptDecryptLen := tmpLen;
  tmpPlaintext           := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_Decrypt(
    FhSession,
    CK_BYTE_PTR(pansichar(cyphertext)), length(cyphertext),
    CK_BYTE_PTR(pansichar(tmpPlaintext)),
    @tmpLen);

  plaintext := Copy(tmpPlaintext, 1, tmpLen);
  Overwrite(tmpPlaintext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptUpdate(cyphertext: Ansistring; var plaintext: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpPlaintext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DecryptUpdate, FN_NAME_C_DecryptUpdate);

  tmpLen                 := length(cyphertext);
  FLastEncryptDecryptLen := tmpLen;
  tmpPlaintext           := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_DecryptUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(cyphertext)), length(cyphertext),
    CK_BYTE_PTR(pansichar(tmpPlaintext)),
    @tmpLen);

  plaintext := Copy(tmpPlaintext, 1, tmpLen);
  Overwrite(tmpPlaintext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptFinal(var plaintext: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpPlaintext: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DecryptFinal, FN_NAME_C_DecryptFinal);

  tmpPlaintext := StringOfChar(AnsiChar(#0), max(FLastEncryptDecryptLen,
    BUFFER_LEN_MIN_ENCRYPT_DECRYPT));

  LastRV := LibraryFunctionList.CK_C_DecryptFinal(
    FhSession,
    CK_BYTE_PTR(pansichar(tmpPlaintext)), @tmpLen
    );

  plaintext := Copy(tmpPlaintext, 1, tmpLen);
  Overwrite(tmpPlaintext);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DigestInit(mechanism: CK_MECHANISM_TYPE): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DigestInit, FN_NAME_C_DigestInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_DigestInit(
    FhSession,
    @mechStruct
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DigestInit(mechanism: TPKCS11Mechanism): Boolean;
begin
  Result := DigestInit(mechanism.MechanismType);
end;

function TPKCS11Session.Digest(data: Ansistring; var digest: Ansistring): Boolean;
var
  tmpLen:    CK_ULONG;
  tmpDigest: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Digest, FN_NAME_C_Digest);

  tmpLen    := length(data);
  tmpDigest := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_Digest(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(data),
    CK_BYTE_PTR(pansichar(tmpDigest)),
    @tmpLen);

  digest := Copy(tmpDigest, 1, tmpLen);
  Overwrite(tmpDigest);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DigestUpdate(data: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DigestUpdate, FN_NAME_C_DigestUpdate);

  LastRV := LibraryFunctionList.CK_C_DigestUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(
    data));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DigestKey(data: Ansistring; keyObj: TPKCS11Object): Boolean;
var
  tmpLen:    CK_ULONG;
  tmpDigest: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DigestKey, FN_NAME_C_DigestKey);

  tmpLen    := length(data);
  tmpDigest := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_DigestKey(
    FhSession,
    keyObj.FHandle);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DigestFinal(var digest: Ansistring): Boolean;
var
  tmpLen:    CK_ULONG;
  tmpDigest: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DigestFinal, FN_NAME_C_DigestFinal);

  tmpDigest := StringOfChar(AnsiChar(#0), BUFFER_LEN_DIGEST);

  LastRV := LibraryFunctionList.CK_C_DigestFinal(
    FhSession,
    CK_BYTE_PTR(pansichar(tmpDigest)), @tmpLen
    );

  digest := Copy(tmpDigest, 1, tmpLen);
  Overwrite(tmpDigest);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignInit, FN_NAME_C_SignInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_SignInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
begin
  Result := SignInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.Sign(data: Ansistring; var signature: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpSignature: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Sign, FN_NAME_C_Sign);

  tmpLen       := length(data);
  tmpSignature := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_Sign(FhSession,
    CK_BYTE_PTR(pansichar(data)),
    length(data),
    CK_BYTE_PTR(pansichar(tmpSignature)),
    @tmpLen);

  signature := Copy(tmpSignature, 1, tmpLen);
  Overwrite(tmpSignature);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignUpdate(data: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignUpdate, FN_NAME_C_SignUpdate);

  LastRV := LibraryFunctionList.CK_C_SignUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(
    data));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignFinal(var signature: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpSignature: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignFinal, FN_NAME_C_SignFinal);

  tmpSignature := StringOfChar(AnsiChar(#0), BUFFER_LEN_SIGN);

  LastRV := LibraryFunctionList.CK_C_SignFinal(
    FhSession,
    CK_BYTE_PTR(pansichar(tmpSignature)), @tmpLen
    );

  signature := Copy(tmpSignature, 1, tmpLen);
  Overwrite(tmpSignature);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignRecoverInit(mechanism: CK_MECHANISM_TYPE;
  keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignRecoverInit, FN_NAME_C_SignRecoverInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_SignRecoverInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignRecoverInit(mechanism: TPKCS11Mechanism;
  keyObj: TPKCS11Object): Boolean;
begin
  Result := SignRecoverInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.SignRecover(data: Ansistring; var signature: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpSignature: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignRecover, FN_NAME_C_SignRecover);

  tmpLen       := length(data);
  tmpSignature := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_SignRecover(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(data),
    CK_BYTE_PTR(pansichar(tmpSignature)),
    @tmpLen);

  signature := Copy(tmpSignature, 1, tmpLen);
  Overwrite(tmpSignature);

  Result := RVSuccess(LastRV);
end;


function TPKCS11Session.VerifyInit(mechanism: CK_MECHANISM_TYPE; keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_VerifyInit, FN_NAME_C_VerifyInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_VerifyInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.VerifyInit(mechanism: TPKCS11Mechanism; keyObj: TPKCS11Object): Boolean;
begin
  Result := VerifyInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.Verify(data: Ansistring; signature: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Verify, FN_NAME_C_Verify);

  LastRV := LibraryFunctionList.CK_C_Verify(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(data),
    CK_BYTE_PTR(pansichar(signature)),
    length(
    signature));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.VerifyUpdate(data: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_VerifyUpdate, FN_NAME_C_VerifyUpdate);

  LastRV := LibraryFunctionList.CK_C_VerifyUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(
    data));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.VerifyFinal(signature: Ansistring): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_VerifyFinal, FN_NAME_C_VerifyFinal);

  LastRV := LibraryFunctionList.CK_C_VerifyFinal(
    FhSession,
    CK_BYTE_PTR(pansichar(signature)), length(
    signature));

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.VerifyRecoverInit(mechanism: CK_MECHANISM_TYPE;
  keyObj: TPKCS11Object): Boolean;
var
  mechStruct: CK_MECHANISM;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_VerifyRecoverInit, FN_NAME_C_VerifyRecoverInit);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  LastRV := LibraryFunctionList.CK_C_VerifyRecoverInit(
    FhSession,
    @mechStruct,
    keyObj.FHandle
    );

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.VerifyRecoverInit(mechanism: TPKCS11Mechanism;
  keyObj: TPKCS11Object): Boolean;
begin
  Result := VerifyRecoverInit(mechanism.MechanismType, keyObj);
end;

function TPKCS11Session.VerifyRecover(data: Ansistring; var signature: Ansistring): Boolean;
var
  tmpLen:       CK_ULONG;
  tmpSignature: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_VerifyRecover, FN_NAME_C_VerifyRecover);

  tmpLen       := length(data);
  tmpSignature := StringOfChar(AnsiChar(#0), tmpLen);

  LastRV := LibraryFunctionList.CK_C_VerifyRecover(
    FhSession,
    CK_BYTE_PTR(pansichar(data)), length(data),
    CK_BYTE_PTR(pansichar(tmpSignature)),
    @tmpLen);

  signature := Copy(tmpSignature, 1, tmpLen);
  Overwrite(tmpSignature);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.WrapKey(mechanism: CK_MECHANISM_TYPE; wrappingKeyObj: TPKCS11Object;
  keyObj: TPKCS11Object; var wrappedKey: Ansistring): Boolean;
var
  mechStruct:    CK_MECHANISM;
  tmpLen:        CK_ULONG;
  tmpWrappedKey: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_WrapKey, FN_NAME_C_WrapKey);

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  tmpWrappedKey := StringOfChar(AnsiChar(#0), BUFFER_LEN_WRAPPED_KEY);

  LastRV := LibraryFunctionList.CK_C_WrapKey(
    FhSession,
    @mechStruct, wrappingKeyObj.FHandle,
    keyObj.FHandle,
    CK_BYTE_PTR(pansichar(tmpWrappedKey)),
    @tmpLen);

  wrappedKey := Copy(tmpWrappedKey, 1, tmpLen);
  Overwrite(tmpWrappedKey);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.UnwrapKey(mechanism: CK_MECHANISM_TYPE;
  unwrappingKeyObj: TPKCS11Object; wrappedKey: Ansistring;
  attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
var
  retval:           TPKCS11Object;
  newObjHandle:     CK_OBJECT_HANDLE;
  ckAttributes:     array of CK_ATTRIBUTE;
  bufferArray:      array of Ansistring;
  i:                Integer;
  mechStruct:       CK_MECHANISM;
  attrPtr:          CK_ATTRIBUTE_PTR;
  currTemplateAttr: TPKCS11Attribute;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_UnwrapKey, FN_NAME_C_UnwrapKey);

  retval := nil;

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  attrPtr := NULL_PTR;
  if (attrTemplate.Count > 0) then begin
    setlength(ckAttributes, attrTemplate.Count);
    setlength(bufferArray, attrTemplate.Count);
    for i := 0 to (attrTemplate.Count - 1) do begin
      currTemplateAttr := attrTemplate.Attribute[i];

      bufferArray[i] := currTemplateAttr.ValueAsRawData;

      ckAttributes[i].attrType   := currTemplateAttr.AttribType;
      ckAttributes[i].pValue     := pansichar(bufferArray[i]);
      ckAttributes[i].ulValueLen := length(bufferArray[i]);
    end;

    attrPtr := @(ckAttributes[0]);
  end;

  LastRV := LibraryFunctionList.CK_C_UnwrapKey(
    FhSession,
    @mechStruct, unwrappingKeyObj.FHandle,
    CK_BYTE_PTR(pansichar(wrappedKey)),
    length(wrappedKey),
    attrPtr,
    attrTemplate.Count, @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.DeriveKey(mechanism: CK_MECHANISM_TYPE; baseKey: TPKCS11Object;
  attrTemplate: TPKCS11AttributeTemplate): TPKCS11Object;
var
  retval:           TPKCS11Object;
  newObjHandle:     CK_OBJECT_HANDLE;
  ckAttributes:     array of CK_ATTRIBUTE;
  bufferArray:      array of Ansistring;
  i:                Integer;
  mechStruct:       CK_MECHANISM;
  attrPtr:          CK_ATTRIBUTE_PTR;
  currTemplateAttr: TPKCS11Attribute;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DeriveKey, FN_NAME_C_DeriveKey);

  retval := nil;

  mechStruct.mechanism      := mechanism;
  mechStruct.pParameter     := NULL_PTR;
  mechStruct.ulParameterLen := 0;

  attrPtr := NULL_PTR;
  if (attrTemplate.Count > 0) then begin
    setlength(ckAttributes, attrTemplate.Count);
    setlength(bufferArray, attrTemplate.Count);
    for i := 0 to (attrTemplate.Count - 1) do begin
      currTemplateAttr := attrTemplate.Attribute[i];

      bufferArray[i] := currTemplateAttr.ValueAsRawData;

      ckAttributes[i].attrType   := currTemplateAttr.AttribType;
      ckAttributes[i].pValue     := pansichar(bufferArray[i]);
      ckAttributes[i].ulValueLen := length(bufferArray[i]);
    end;

    attrPtr := @(ckAttributes[0]);
  end;

  LastRV := LibraryFunctionList.CK_C_DeriveKey(
    FhSession,
    @mechStruct, baseKey.FHandle,
    attrPtr,
    attrTemplate.Count, @newObjHandle
    );
  if RVSuccess(LastRV) then begin
    retval                     := TPKCS11Object.Create();
    retval.LibraryFunctionList := LibraryFunctionList;
    retval.FSessionHandle      := FhSession;
    retval.FHandle             := newObjHandle;
  end;

  Result := retval;
end;

function TPKCS11Session.GetFunctionStatus(): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetFunctionStatus, FN_NAME_C_GetFunctionStatus);

  LastRV := LibraryFunctionList.CK_C_GetFunctionStatus(FhSession);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.CancelFunction(): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_CancelFunction, FN_NAME_C_CancelFunction);

  LastRV := LibraryFunctionList.CK_C_CancelFunction(FhSession);

  Result := RVSuccess(LastRV);
end;


function TPKCS11Session.DigestEncryptUpdate(part: Ansistring;
  var encryptedPart: Ansistring): Boolean;
var
  tmpLen:  CK_ULONG;
  inData:  Ansistring;
  outData: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DigestEncryptUpdate, FN_NAME_C_DigestEncryptUpdate);

  inData := part;

  outData := StringOfChar(AnsiChar(#0), max(length(inData), BUFFER_LEN_MIN_ENCRYPT_DECRYPT));
  tmpLen  := length(outData);

  LastRV := LibraryFunctionList.CK_C_DigestEncryptUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(inData)), length(inData),
    CK_BYTE_PTR(pansichar(outData)),
    @tmpLen);

  encryptedPart := Copy(outData, 1, tmpLen);
  Overwrite(inData);
  Overwrite(outData);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptDigestUpdate(pEncryptedPart: Ansistring;
  var part: Ansistring): Boolean;
var
  tmpLen:  CK_ULONG;
  inData:  Ansistring;
  outData: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DecryptDigestUpdate, FN_NAME_C_DecryptDigestUpdate);

  inData := pEncryptedPart;

  outData := StringOfChar(AnsiChar(#0), max(length(inData), BUFFER_LEN_MIN_ENCRYPT_DECRYPT));
  tmpLen  := length(outData);

  LastRV := LibraryFunctionList.CK_C_DecryptDigestUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(inData)), length(inData),
    CK_BYTE_PTR(pansichar(outData)),
    @tmpLen);

  part := Copy(outData, 1, tmpLen);
  Overwrite(inData);
  Overwrite(outData);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.SignEncryptUpdate(part: Ansistring;
  var encryptedPart: Ansistring): Boolean;
var
  tmpLen:  CK_ULONG;
  inData:  Ansistring;
  outData: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SignEncryptUpdate, FN_NAME_C_SignEncryptUpdate);

  inData := part;

  outData := StringOfChar(AnsiChar(#0), max(length(inData), BUFFER_LEN_MIN_ENCRYPT_DECRYPT));
  tmpLen  := length(outData);

  LastRV := LibraryFunctionList.CK_C_SignEncryptUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(inData)), length(inData),
    CK_BYTE_PTR(pansichar(outData)),
    @tmpLen);

  encryptedPart := Copy(outData, 1, tmpLen);
  Overwrite(inData);
  Overwrite(outData);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Session.DecryptVerifyUpdate(pEncryptedPart: Ansistring;
  var part: Ansistring): Boolean;
var
  tmpLen:  CK_ULONG;
  inData:  Ansistring;
  outData: Ansistring;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_DecryptVerifyUpdate, FN_NAME_C_DecryptVerifyUpdate);

  inData := pEncryptedPart;

  outData := StringOfChar(AnsiChar(#0), max(length(inData), BUFFER_LEN_MIN_ENCRYPT_DECRYPT));
  tmpLen  := length(outData);

  LastRV := LibraryFunctionList.CK_C_DecryptVerifyUpdate(
    FhSession,
    CK_BYTE_PTR(pansichar(inData)), length(inData),
    CK_BYTE_PTR(pansichar(outData)),
    @tmpLen);

  part := Copy(outData, 1, tmpLen);
  Overwrite(inData);
  Overwrite(outData);

  Result := RVSuccess(LastRV);
end;

end.
