unit PKCS11Lib;

interface

uses
  //delphi
  Classes,
  StdCtrls,
  //3rd party
  pkcs11t,
  pkcs11_library,
  pkcs11_session,
  pkcs11_object,
  //sdu
lcTypes
  //LibreCrypt
  ;

const
  PKCS11_NO_SLOT_ID = -100; // -1 reserved for Windows.CB_ERR

type
  // !! IMPORTANT !!
  // If this definition is updated, DestroyAndFreeRecord_PKCS11CDB(...) should
  // also be updated
  TPKCS11CDB = record
    SlotID: Integer;
    Obj:    TPKCS11Object;
    Xlabel: Ansistring;
    CDB:    Ansistring;
  end;
  PPKCS11CDB = ^TPKCS11CDB;

  // Note: This is an array of POINTERS
  TPKCS11CDBPtrArray = array of PPKCS11CDB;

  TPKCS11SecretKeyType = (
    ktRC2,
    ktRC4,
    ktDES,
    kt2DES,
    kt3DES,
    ktCDMF,
    ktCAST,
    ktCAST3,
    ktCAST128,
    ktRC5,
    ktIDEA,
    ktCamellia,
    ktARIA,
    ktSkipjack,
    ktBaton,
    ktJuniper,
    ktAES,
    ktBlowfish,
    ktTwofish
    );

  TPKCS11SecretKeyTypeArr = array of TPKCS11SecretKeyType;

  // !! IMPORTANT !!
  // If this definition is updated, DestroyAndFreeRecord_PKCS11SecretKey(...) should
  // also be updated
  TPKCS11SecretKey = record
    SlotID: Integer;
    Obj:    TPKCS11Object;
    Xlabel: Ansistring;
    Size:   Integer; // In *bits*; set to -1 if this couldn't be retrieved
    XType:  TPKCS11SecretKeyType;
  end;
  PPKCS11SecretKey = ^TPKCS11SecretKey;

  // Note: This is an array of POINTERS
  TPKCS11SecretKeyPtrArray = array of PPKCS11SecretKey;

type
  TPKCS11SecretKeyInfo = record
    Name:        String;
    KeyType:     CK_KEY_TYPE;
    MechKeyGen:  CK_MECHANISM_TYPE;
    KGSpecifyKL: Boolean;  // Key generator needs key length specified
    MechCypher:  CK_MECHANISM_TYPE;
  end;

const
  // All of the key types which are defined in the PKCS#11 specification
  PKCS11_SECRET_KEY_TYPES: array [TPKCS11SecretKeyType] of TPKCS11SecretKeyInfo =
    (
    (
    Name: 'RC2';
    KeyType: CKK_RC2;
    MechKeyGen: CKM_RC2_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_RC2_CBC
    ),
    (
    Name: 'RC4';
    KeyType: CKK_RC4;
    MechKeyGen: CKM_RC4_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_RC4
    ),
    (
    Name: 'DES';
    KeyType: CKK_DES;
    MechKeyGen: CKM_DES_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_DES_CBC
    ),
    // 2DES uses the 3DES cypher mechanism
    (
    Name: '2DES';
    KeyType: CKK_DES2;
    MechKeyGen: CKM_DES2_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_DES3_CBC
    ),
    (
    Name: '3DES';
    KeyType: CKK_DES3;
    MechKeyGen: CKM_DES3_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_DES3_CBC
    ),
    (
    Name: 'CDMF';
    KeyType: CKK_CDMF;
    MechKeyGen: CKM_CDMF_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_CDMF_CBC
    ),
    (
    Name: 'CAST';
    KeyType: CKK_CAST;
    MechKeyGen: CKM_CAST_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_CAST_CBC
    ),
    (
    Name: 'CAST3';
    KeyType: CKK_CAST3;
    MechKeyGen: CKM_CAST3_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_CAST3_CBC
    ),
    (
    Name: 'CAST128';
    KeyType: CKK_CAST128;
    MechKeyGen: CKM_CAST128_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_CAST128_CBC
    ),
    (
    Name: 'RC5';
    KeyType: CKK_RC5;
    MechKeyGen: CKM_RC5_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_RC5_CBC
    ),
    (
    Name: 'IDEA';
    KeyType: CKK_IDEA;
    MechKeyGen: CKM_IDEA_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_IDEA_CBC
    ),
    (
    Name: 'Camellia';
    KeyType: CKK_CAMELLIA;
    MechKeyGen: CKM_CAMELLIA_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_CAMELLIA_CBC
    ),
    (
    Name: 'ARIA';
    KeyType: CKK_ARIA;
    MechKeyGen: CKM_ARIA_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_ARIA_CBC
    ),
    (
    Name: 'Skipjack';
    KeyType: CKK_SKIPJACK;
    MechKeyGen: CKM_SKIPJACK_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_SKIPJACK_CBC64
    ),
    (
    Name: 'Baton';
    KeyType: CKK_BATON;
    MechKeyGen: CKM_BATON_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_BATON_CBC128
    ),
    (
    Name: 'Juniper';
    KeyType: CKK_JUNIPER;
    MechKeyGen: CKM_JUNIPER_KEY_GEN;
    KGSpecifyKL: False;
    MechCypher: CKM_JUNIPER_CBC128
    ),
    (
    Name: 'AES';
    KeyType: CKK_AES;
    MechKeyGen: CKM_AES_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_AES_CBC
    ),
    (
    Name: 'Blowfish';
    KeyType: CKK_BLOWFISH;
    MechKeyGen: CKM_BLOWFISH_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_BLOWFISH_CBC
    ),
    (
    Name: 'Twofish';
    KeyType: CKK_TWOFISH;
    MechKeyGen: CKM_TWOFISH_KEY_GEN;
    KGSpecifyKL: True;
    MechCypher: CKM_TWOFISH_CBC
    )
    );

  // Default key type; this should exactly match one of the names above
  PKCS11_DEFAULT_KEYTYPE = 'AES';


function PKCS11LibraryReady(PKCS11Library: TPKCS11Library): Boolean;

 // Populate a TComboBox with a list of tokens available
 // The .Objects[idx] of the combobox items will be integers indicating the
 // slot ID
 // Returns the number of tokens listed; 0 if there are none
function PKCS11PopulateTokenList(PKCS11Library: TPKCS11Library; cbToken: TComboBox): Integer;
 // Gets the selected slot ID from the combobox, or PKCS11_NO_SLOT_ID on
 // error/none selected
function PKCS11TokenListSelected(cbToken: TComboBox): Integer;

// Note: PKCS#11 library passed in here *must* have already been initialized
function GetPKCS11RandomData(
  PKCS11Library: TPKCS11Library;
  SlotID: Integer;
  BytesWanted: Integer;
  var RandomData: TSDUBytes
  ): Boolean;


 // IMPORTANT: It is the CALLERS responsibility to free off all of the elements
 //            in CDBData, e.g. by calling DestroyAndFreeRecord_PKCS11CDB(...)
function GetAllPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  var CDBData: TPKCS11CDBPtrArray;
  var strError: String
  ): Boolean;
 // IMPORTANT: It is the CALLERS responsibility to free off the CDBData,
 //            e.g. by calling DestroyAndFreeRecord_PKCS11CDB(...)
function GetPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBLabel: String;
  var CDBData: PPKCS11CDB;
  var strError: String
  ): Boolean;
function DestroyPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBRecordPtr: PPKCS11CDB;
  var strError: String
  ): Boolean;
function CreatePKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBLabel: Ansistring;
  CDB: Ansistring;
  var strError: String
  ): Boolean;

procedure DestroyAndFreeRecord_PKCS11CDB(var rcd: PPKCS11CDB); overload;
procedure DestroyAndFreeRecord_PKCS11CDB(var rcdArr: TPKCS11CDBPtrArray); overload;


function GetAllPKCS11SecretKeyTypes(
  PKCS11Session: TPKCS11Session;
  var SecretKeyTypes: TPKCS11SecretKeyTypeArr;
  var strError: String
  ): Boolean;
 // IMPORTANT: It is the CALLERS responsibility to free off all of the elements
 //            in SecretKeyData, e.g. by calling DestroyAndFreeRecord_PKCS11SecretKey(...)
function GetAllPKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  var SecretKeyData: TPKCS11SecretKeyPtrArray;
  var strError: String
  ): Boolean;
function DestroyPKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKeyRecordPtr: PPKCS11SecretKey;
  var strError: String
  ): Boolean;
function CreatePKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKeyLabel: Ansistring;
  KeyType: TPKCS11SecretKeyType;
  var strError: String
  ): Boolean;

function CK_KEY_TYPEToTPKCS11SecretKeyType(ckKeyType: CK_KEY_TYPE): TPKCS11SecretKeyType;

procedure DestroyAndFreeRecord_PKCS11SecretKey(var rcd: PPKCS11SecretKey); overload;
procedure DestroyAndFreeRecord_PKCS11SecretKey(var rcdArr: TPKCS11SecretKeyPtrArray); overload;

function PKCS11EncryptCDBWithSecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKey: PPKCS11SecretKey;
  PlaintextData: Ansistring;
  var CyphertextData: Ansistring;
  var strError: String
  ): Boolean;
function PKCS11DecryptCDBWithSecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKey: PPKCS11SecretKey;
  CyphertextData: Ansistring;
  var PlaintextData: Ansistring;
  var strError: String
  ): Boolean;


implementation

uses
  SysUtils,
  //sdu
sdugeneral,
  SDUi18n,
  pkcs11_attribute,
  pkcs11_slot,
  pkcs11_token,
  pkcs11_mechanism,
  Shredder;

const
  FREEOTFE_PKCS11_APP_TITLE = 'FreeOTFE';

  PKCS11_DATA_OBJ_PREFIX_CDB = 'CDB::';
  PKCS11_SECRETKEY_PREFIX_ID = 'FreeOTFE::';


resourcestring
  RS_NO_TOKENS_DETECTED = '<No tokens detected>';


function PKCS11LibraryReady(PKCS11Library: TPKCS11Library): Boolean;
var
  retval: Boolean;
begin
  retval := (PKCS11Library <> nil);
  if retval then begin
    retval := PKCS11Library.InitializeCalled;
  end;

  Result := retval;
end;


 // Note: If more than one CDBs exists with the same label, this function will
 //       return the first one encountered
function GetPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBLabel: String;
  var CDBData: PPKCS11CDB;
  var strError: String
  ): Boolean;
var
  i:        Integer;
  foundOne: Boolean;
  rcdArr:   TPKCS11CDBPtrArray;
begin
  foundOne := False;

  if GetAllPKCS11CDB(PKCS11Session, rcdArr, strError) then begin
    for i := low(rcdArr) to high(rcdArr) do begin
      if (not (foundOne) and (rcdArr[i].XLabel = CDBLabel)) then begin
        foundOne := True;
        CDBData  := rcdArr[i];
      end else begin
        DestroyAndFreeRecord_PKCS11CDB(rcdArr[i]);
      end;
    end;

  end;

  Result := foundOne;
end;


function GetAllPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  var CDBData: TPKCS11CDBPtrArray;
  var strError: String
  ): Boolean;
var
  allOK:        Boolean;
  objArr:       TPKCS11ObjectArray;
  currObj:      TPKCS11Object;
  attrLabel:    TPKCS11Attribute;
  attrValue:    TPKCS11Attribute;
  i:            Integer;
  tmpPKCS11CDB: PPKCS11CDB;
  strCDB:       Ansistring;
begin
  allOK := True;

  SetLength(CDBData, 0);

  objArr := PKCS11Session.GetObjects(FREEOTFE_PKCS11_APP_TITLE);
  for i := low(objArr) to high(objArr) do begin
    currObj := objArr[i];

    attrLabel := currObj.GetAttribute(CKA_LABEL);
    if (attrLabel = nil) then begin
      strError := currObj.LastRVString();
      allOK    := False;
      break;
    end;

    attrValue := currObj.GetAttribute(CKA_VALUE);
    if (attrValue = nil) then begin
      strError := currObj.LastRVString();
      allOK    := False;
      attrLabel.Free();
      break;
    end;

    strCDB := attrValue.ValueAsByteArray;
    if (Pos(PKCS11_DATA_OBJ_PREFIX_CDB, strCDB) > 0) then begin
      strCDB := Copy(strCDB,
        (length(PKCS11_DATA_OBJ_PREFIX_CDB) + 1),
        (length(strCDB) - length(PKCS11_DATA_OBJ_PREFIX_CDB)));

      tmpPKCS11CDB        := new(PPKCS11CDB);
      tmpPKCS11CDB.XLabel := attrLabel.ValueAsByteArray;
      tmpPKCS11CDB.CDB    := strCDB;
      tmpPKCS11CDB.Obj    := currObj;

      SetLength(CDBData, (length(CDBData) + 1));
      CDBData[(length(CDBData) - 1)] := tmpPKCS11CDB;
    end;

    attrLabel.Free();
    attrValue.Free();
  end;

  Result := allOK;
end;

function DestroyPKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBRecordPtr: PPKCS11CDB;
  var strError: String
  ): Boolean;
var
  allOK: Boolean;
begin
  allOK := False;

  if (CDBRecordPtr <> nil) then begin
    if (CDBRecordPtr.Obj <> nil) then begin
      allOK    := PKCS11Session.DestroyObject(CDBRecordPtr.Obj);
      strError := PKCS11Session.LastRVString();
    end;
  end;

  Result := allOK;
end;

function CreatePKCS11CDB(
  PKCS11Session: TPKCS11Session;
  CDBLabel: Ansistring;
  CDB: Ansistring;
  var strError: String
  ): Boolean;
var
  allOK:    Boolean;
  obj:      TPKCS11Object;
  attrObjs: array of TPKCS11Attribute;
  i:        Integer;
begin
  allOK := False;

  CDB := PKCS11_DATA_OBJ_PREFIX_CDB + CDB;

  SetLength(attrObjs, 7);
  for i := low(attrObjs) to high(attrObjs) do begin
    attrObjs[i] := TPKCS11Attribute.Create();
  end;

  attrObjs[0].AttribType      := CKA_CLASS;
  attrObjs[0].ValueAsObjClass := CKO_DATA;

  attrObjs[1].AttribType     := CKA_TOKEN;
  attrObjs[1].ValueAsBoolean := True;

  attrObjs[2].AttribType     := CKA_PRIVATE;
  attrObjs[2].ValueAsBoolean := True;

  attrObjs[3].AttribType    := CKA_LABEL;
  attrObjs[3].ValueAsString := CDBLabel;

  attrObjs[4].AttribType    := CKA_APPLICATION;
  attrObjs[4].ValueAsString := FREEOTFE_PKCS11_APP_TITLE;

  attrObjs[5].AttribType       := CKA_VALUE;
  attrObjs[5].ValueAsByteArray := CDB;

  attrObjs[6].AttribType     := CKA_MODIFIABLE;
  attrObjs[6].ValueAsBoolean := True;

  obj := PKCS11Session.CreateObject(attrObjs);

  if (obj <> nil) then begin
    allOK := True;
    obj.Free();
  end else begin
    strError := PKCS11Session.LastRVString();
  end;

  for i := low(attrObjs) to high(attrObjs) do begin
    attrObjs[i].Free();
  end;


  Result := allOK;
end;

procedure DestroyAndFreeRecord_PKCS11CDB(var rcd: PPKCS11CDB);
begin
  Overwrite(rcd.Xlabel);
  Overwrite(rcd.CDB);
  rcd.Obj.Free();
  dispose(rcd);

end;

procedure DestroyAndFreeRecord_PKCS11CDB(var rcdArr: TPKCS11CDBPtrArray);
var
  i: Integer;
begin
  for i := low(rcdArr) to high(rcdArr) do begin
    DestroyAndFreeRecord_PKCS11CDB(rcdArr[i]);
  end;

  SetLength(rcdArr, 0);
end;


function GetAllPKCS11SecretKeyTypes(
  PKCS11Session: TPKCS11Session;
  var SecretKeyTypes: TPKCS11SecretKeyTypeArr;
  var strError: String
  ): Boolean;
var
  allOK:    Boolean;
  i:        TPKCS11SecretKeyType;
  j:        Integer;
  keyGenOK: Boolean;
  cypherOK: Boolean;
  token:    TPKCS11Token;
begin
  allOK := True;

  token := TPKCS11Token.Create();
  try
    token.SlotID              := PKCS11Session.SlotID;
    token.LibraryFunctionList := PKCS11Session.LibraryFunctionList;
    token.CacheTokenInfo      := True;

    SetLength(SecretKeyTypes, 0);

    for i := low(PKCS11_SECRET_KEY_TYPES) to high(PKCS11_SECRET_KEY_TYPES) do begin
      keyGenOK := False;
      cypherOK := False;
      for j := 0 to (token.CountMechanisms - 1) do begin
        // Only include key types if both the key generation and use mechanisms
        // are available on the token
        if (token.Mechanism[j].MechanismType = PKCS11_SECRET_KEY_TYPES[i].MechKeyGen) then begin
          keyGenOK := True;
        end;

        if (token.Mechanism[j].MechanismType = PKCS11_SECRET_KEY_TYPES[i].MechCypher) then begin
          cypherOK := True;
        end;

        if (keyGenOK and cypherOK) then begin
          break;
        end;
      end;

      if (keyGenOK and cypherOK) then begin
        SetLength(SecretKeyTypes, (length(SecretKeyTypes) + 1));
        SecretKeyTypes[(length(SecretKeyTypes) - 1)] := i;
      end;
    end;

  finally
    token.Free();
  end;

  Result := allOK;
end;

function GetAllPKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  var SecretKeyData: TPKCS11SecretKeyPtrArray;
  var strError: String
  ): Boolean;
var
  allOK:              Boolean;
  objArr:             TPKCS11ObjectArray;
  currObj:            TPKCS11Object;
  attrLabel:          TPKCS11Attribute;
  attrID:             TPKCS11Attribute;
  attrKeyType:        TPKCS11Attribute;
  attrKeySize:        TPKCS11Attribute;
  i:                  Integer;
  tmpPKCS11SecretKey: PPKCS11SecretKey;
  strSecretKey:       String;
begin
  allOK := True;

  SetLength(SecretKeyData, 0);

  objArr := PKCS11Session.GetObjects(CKO_SECRET_KEY);
  for i := low(objArr) to high(objArr) do begin
    currObj := objArr[i];

    attrLabel := currObj.GetAttribute(CKA_LABEL);
    if (attrLabel = nil) then begin
      strError := currObj.LastRVString();
      allOK    := False;
      break;
    end;

    attrID := currObj.GetAttribute(CKA_ID);
    if (attrID = nil) then begin
      strError := currObj.LastRVString();
      allOK    := False;
      attrLabel.Free();
      break;
    end;

    attrKeyType := currObj.GetAttribute(CKA_KEY_TYPE);
    if (attrKeyType = nil) then begin
      strError := currObj.LastRVString();
      allOK    := False;
      attrLabel.Free();
      break;
    end;

    attrKeySize := currObj.GetAttribute(CKA_VALUE_LEN);
    // Ignore if can't get the keysize; we just won't display it.

    strSecretKey := attrID.ValueAsByteArray;
    if (Pos(PKCS11_SECRETKEY_PREFIX_ID, strSecretKey) > 0) then begin
      strSecretKey := Copy(strSecretKey,
        (length(PKCS11_SECRETKEY_PREFIX_ID) + 1),
        (length(strSecretKey) -
        length(PKCS11_SECRETKEY_PREFIX_ID)));

      tmpPKCS11SecretKey        := new(PPKCS11SecretKey);
      tmpPKCS11SecretKey.XLabel := attrLabel.ValueAsByteArray;
      tmpPKCS11SecretKey.XType  := CK_KEY_TYPEToTPKCS11SecretKeyType(attrKeyType.ValueAsNumber);
      tmpPKCS11SecretKey.Obj    := currObj;

      tmpPKCS11SecretKey.Size := -1;
      if (attrKeySize <> nil) then begin
        // * 8 as it's returned in bytes
        tmpPKCS11SecretKey.Size := (attrKeySize.ValueAsNumber * 8);
      end;

      SetLength(SecretKeyData, (length(SecretKeyData) + 1));
      SecretKeyData[(length(SecretKeyData) - 1)] := tmpPKCS11SecretKey;
    end;

    attrKeySize.Free();
    attrKeyType.Free();
    attrID.Free();
    attrLabel.Free();
  end;

  Result := allOK;
end;

function CK_KEY_TYPEToTPKCS11SecretKeyType(ckKeyType: CK_KEY_TYPE): TPKCS11SecretKeyType;
var
  retval: TPKCS11SecretKeyType;
  i:      TPKCS11SecretKeyType;
begin
  retval := low(PKCS11_SECRET_KEY_TYPES);

  for i := low(PKCS11_SECRET_KEY_TYPES) to high(PKCS11_SECRET_KEY_TYPES) do begin
    if (PKCS11_SECRET_KEY_TYPES[i].KeyType = ckKeyType) then begin
      retval := i;
      break;
    end;
  end;

  Result := retval;
end;


function DestroyPKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKeyRecordPtr: PPKCS11SecretKey;
  var strError: String
  ): Boolean;
var
  allOK: Boolean;
begin
  allOK := False;

  if (SecretKeyRecordPtr <> nil) then begin
    if (SecretKeyRecordPtr.Obj <> nil) then begin
      allOK    := PKCS11Session.DestroyObject(SecretKeyRecordPtr.Obj);
      strError := PKCS11Session.LastRVString();
    end;
  end;

  Result := allOK;
end;

function CreatePKCS11SecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKeyLabel: Ansistring;
  KeyType: TPKCS11SecretKeyType;
  var strError: String
  ): Boolean;
var
  allOK:        Boolean;
  obj:          TPKCS11Object;
  attr:         TPKCS11Attribute;
  secureKey:    Boolean;
  mechKeyGen:   CK_MECHANISM_TYPE;
  attrTemplate: TPKCS11AttributeTemplate;
  token:        TPKCS11Token;
  mechObj:      TPKCS11Mechanism;
  useSize:      Integer;
begin
  allOK := False;

  secureKey := True;

  attrTemplate := TPKCS11AttributeTemplate.Create();
  try
{
    CKA_APPLICATION attribute not valid for keys
        
    attr := attrTemplate.Add();
    attr.AttribType      := CKA_APPLICATION;
    attr.ValueAsString   := 'my application';
}

    attr               := attrTemplate.Add();
    attr.AttribType    := CKA_LABEL;
    attr.ValueAsString := SecretKeyLabel;

    attr               := attrTemplate.Add();
    attr.AttribType    := CKA_ID;
    attr.ValueAsString := PKCS11_SECRETKEY_PREFIX_ID;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_TOKEN;
    attr.ValueAsBoolean := True;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_EXTRACTABLE;
    attr.ValueAsBoolean := not (secureKey);

{
    Adding this attribute causes a CKR_TEMPLATE_INCONSISTANT error
     - this attribute is created, handled and managed by the token

    attr := attrTemplate.Add();
    attr.AttribType      := CKA_NEVER_EXTRACTABLE;
    attr.ValueAsBoolean  := TRUE;
}

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_PRIVATE;
    attr.ValueAsBoolean := secureKey;

{
    Adding this attribute causes a CKR_TEMPLATE_INCONSISTANT error with
    Aladdin eTokens (and possibly others?)

    attr := attrTemplate.Add();
    attr.AttribType      := CKA_LOCAL;
    attr.ValueAsBoolean  := TRUE;
}

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_DERIVE;
    attr.ValueAsBoolean := False;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_SENSITIVE;
    attr.ValueAsBoolean := secureKey;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_ENCRYPT;
    attr.ValueAsBoolean := True;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_DECRYPT;
    attr.ValueAsBoolean := True;

    attr                := attrTemplate.Add();
    attr.AttribType     := CKA_MODIFIABLE;
    attr.ValueAsBoolean := True;

    mechKeyGen := PKCS11_SECRET_KEY_TYPES[keyType].MechKeyGen;

    if PKCS11_SECRET_KEY_TYPES[keyType].KGSpecifyKL then begin
      token := TPKCS11Token.Create();
      try
        token.SlotID              := PKCS11Session.SlotID;
        token.LibraryFunctionList := PKCS11Session.LibraryFunctionList;
        token.CacheTokenInfo      := True;

        mechObj := token.GetMechanismByType(mechKeyGen);
        if (mechObj <> nil) then begin
          attr            := attrTemplate.Add();
          attr.AttribType := CKA_VALUE_LEN;

          // We always specify CKA_VALUE_LEN in *bytes* - though the mechanism
          // may have reported the max key length in bits; so we convert to
          // bytes here
          useSize := (mechObj.MaxKeySizeInBits div 8);

          attr.ValueAsNumber := useSize;
        end;

      finally
        token.Free();
      end;

    end;

    obj := PKCS11Session.GenerateKey(mechKeyGen, attrTemplate);

  finally
    attrTemplate.Free();
  end;

  if (obj <> nil) then begin
    allOK := True;
    obj.Free();
  end else begin
    strError := PKCS11Session.LastRVString();
  end;

  Result := allOK;
end;

procedure DestroyAndFreeRecord_PKCS11SecretKey(var rcd: PPKCS11SecretKey); overload;
begin
  Overwrite(rcd.Xlabel);
  rcd.XType := low(PKCS11_SECRET_KEY_TYPES); // Arbitary value to overwrite
  rcd.Obj.Free();
  dispose(rcd);

end;

procedure DestroyAndFreeRecord_PKCS11SecretKey(var rcdArr: TPKCS11SecretKeyPtrArray); overload;
var
  i: Integer;
begin
  for i := low(rcdArr) to high(rcdArr) do begin
    DestroyAndFreeRecord_PKCS11SecretKey(rcdArr[i]);
  end;

  SetLength(rcdArr, 0);
end;


function PKCS11EncryptCDBWithSecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKey: PPKCS11SecretKey;
  PlaintextData: Ansistring;
  var CyphertextData: Ansistring;
  var strError: String
  ): Boolean;
var
  retval: Boolean;
begin
  retval := PKCS11Session.EncryptInit(
    PKCS11_SECRET_KEY_TYPES[SecretKey.XType].MechCypher,
    SecretKey.Obj);
  if retval then begin
    retval := PKCS11Session.Encrypt(PlaintextData, CyphertextData);
  end;

  if not (retval) then begin
    strError := PKCS11Session.LastRVString();
  end else begin
    if (length(PlaintextData) <> length(CyphertextData)) then begin
      retval   := False;
      strError := Format(
        _('Header length changed during encryption; from %d bytes to %d bytes'),
        [length(PlaintextData), length(CyphertextData)]);
    end;
  end;

  Result := retval;
end;

function PKCS11DecryptCDBWithSecretKey(
  PKCS11Session: TPKCS11Session;
  SecretKey: PPKCS11SecretKey;
  CyphertextData: Ansistring;
  var PlaintextData: Ansistring;
  var strError: String
  ): Boolean;
var
  retval: Boolean;
begin
  retval := PKCS11Session.DecryptInit(
    PKCS11_SECRET_KEY_TYPES[SecretKey.XType].MechCypher,
    SecretKey.Obj);
  if retval then begin
    retval := PKCS11Session.Decrypt(CyphertextData, PlaintextData);
  end;

  if not (retval) then begin
    strError := PKCS11Session.LastRVString();
  end else begin
    if (length(PlaintextData) <> length(CyphertextData)) then begin
      retval   := False;
      strError := Format(
        _('Header length changed during decryption; from %d bytes to %d bytes'),
        [length(CyphertextData), length(PlaintextData)]);
    end;
  end;

  Result := retval;
end;

// Note: PKCS#11 library passed in here *must* have already been initialized
function GetPKCS11RandomData(
  PKCS11Library: TPKCS11Library;
  SlotID: Integer;
  BytesWanted: Integer;
  var RandomData: TSDUBytes
  ): Boolean;
var
  retval:  Boolean;
  slot:    TPKCS11Slot;
  token:   TPKCS11Token;
  session: TPKCS11Session;
begin
  retval := False;

  slot := PKCS11Library.SlotByID[SlotID];
  if (slot <> nil) then begin
    if slot.TokenPresent then begin
      token := slot.Token();
      if (token <> nil) then begin
        session := token.OpenSession(True);
        if (session <> nil) then begin
          retval := session.GenerateRandom(BytesWanted, RandomData);

          token.CloseSession(session);
          session.Free();
        end;

        token.Free();
      end;
    end;
  end;

  Result := retval;
end;

function PKCS11PopulateTokenList(PKCS11Library: TPKCS11Library; cbToken: TCombobox): Integer;
var
  i:              Integer;
  tokenLabel:     String;
  slotObj:        TPKCS11Slot;
  tokenObj:       TPKCS11Token;
  tokensDetected: Boolean;
  retval:         Integer;
begin
  cbToken.Items.Clear();

  if PKCS11LibraryReady(PKCS11Library) then begin
    for i := 0 to (PKCS11Library.CountSlots - 1) do begin
      slotObj := PKCS11Library.Slot[i];
      if slotObj.TokenPresent then begin
        tokenObj   := slotObj.Token();
        tokenLabel := tokenObj.Xlabel;
        tokenLabel := trim(tokenLabel); // Junk any whitespace padding
        tokenObj.Free();

        cbToken.Items.AddObject(
          '[' + IntToStr(slotObj.SlotID) + '] ' + tokenLabel,
          TObject(slotObj.SlotID)
          );
      end;
    end;
  end;

  retval         := cbToken.items.Count;
  tokensDetected := (retval > 0);

  // Disable token selection if no tokens...
  SDUEnableControl(cbToken, tokensDetected);
  if not (tokensDetected) then begin
    cbToken.items.AddObject(
      RS_NO_TOKENS_DETECTED,
      TObject(PKCS11_NO_SLOT_ID)
      );
  end;

  // Select first item in list (this will either a token, or text saying "no
  // tokens")
  cbToken.ItemIndex := 0;

  // One token or less, don't bother letting the user change tokens
  SDUEnableControl(cbToken, (cbToken.items.Count > 1));

  Result := retval;
end;

function PKCS11TokenListSelected(cbToken: TComboBox): Integer;
begin
  Result := PKCS11_NO_SLOT_ID;

  if (cbToken.ItemIndex >= 0) then
    Result := Integer(cbToken.Items.Objects[cbToken.ItemIndex]);
end;

end.
