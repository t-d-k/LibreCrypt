unit pkcs11_attribute;

interface

uses
  Controls,
  pkcs11_api, pkcs11t;

type
  PCK_OBJECT_CLASS   = ^CK_OBJECT_CLASS;
  PCK_BBOOL          = ^CK_BBOOL;
  PCK_CERTIFICATE_TYPE = ^CK_CERTIFICATE_TYPE;
  PCK_ULONG          = ^CK_ULONG;
  PCK_KEY_TYPE       = ^CK_KEY_TYPE;
  PCK_DATE           = ^CK_DATE;
  PCK_MECHANISM_TYPE = ^CK_MECHANISM_TYPE;
  PCK_ATTRIBUTE_PTR  = ^CK_ATTRIBUTE_PTR;
  PCK_HW_FEATURE_TYPE = ^CK_HW_FEATURE_TYPE;

  TAttributeDataType = (
    adtObjClass,      // CK_OBJECT_CLASS
    adtBoolean,       // CK_BBOOL
    adtString,        // RFC 2279 string
    adtBigInteger,    // Big integer
    adtByteArray,     // byte array
    adtCertificateType, // CK_CERTIFICATE_TYPE
    adtNumber,        // CK_ULONG
    adtKeyType,       // CK_KEY_TYPE
    adtDate,          // CK_DATE
    adtMechanismType, // CK_MECHANISM_TYPE
    adtMechanismTypeSet, // CK_MECHANISM_TYPE_PTR
    adtUnknown,       // ???
    adtAttributeSet,  // CK_ATTRIBUTE_PTR
    adtHWFeatureType  // CK_HW_FEATURE_TYPE
    );


  TPKCS11Attribute = class (TPKCS11API)
  private
    FAttribType: CK_ATTRIBUTE_TYPE;
    FLength:     Cardinal;
    FRawData:    Ansistring;

    function MapPKCS11AttrTypeToDelphiAttrType(pkcs11AttrType: CK_ATTRIBUTE_TYPE):
      TAttributeDataType;

  protected
    function GetAttribTypeStr(): String;
    function GetLength(): Cardinal;

    procedure SetAttrStruct(struct: CK_ATTRIBUTE);

    function ObjectClassToString(objClass: CK_OBJECT_CLASS): String;
    function HWFeatureTypeToString(HWFeatureType: CK_HW_FEATURE_TYPE): String;
    function KeyTypeToString(keyType: CK_KEY_TYPE): String;
    function CertificateTypeToString(certType: CK_CERTIFICATE_TYPE): String;

    function CopyBinary(ptr: PByte; len: Integer): Ansistring;

    // Get methods...
    function GetValueAsObjClass(): CK_OBJECT_CLASS;
    function GetValueAsBoolean(): Boolean;
    function GetValueAsString(): Ansistring;
    function GetValueAsBigInteger(): Ansistring;
    function GetValueAsByteArray(): Ansistring;
    function GetValueAsCertificateType(): CK_CERTIFICATE_TYPE;
    function GetValueAsNumber(): CK_ULONG;
    function GetValueAsKeyType(): CK_KEY_TYPE;
    function GetValueAsDate(): TDate;
    function GetValueAsMechanismType(): CK_MECHANISM_TYPE;
    //    function GetValueAsMechanismTypeSet(): array of CK_MECHANISM_TYPE;
    //    function GetValueAsAttributeSet(): array of CK_ATTRIBUTE_TYPE;
    function GetValueAsHWFeatureType(): CK_HW_FEATURE_TYPE;

    // Set methods...
    procedure SetValueAsObjClass(Value: CK_OBJECT_CLASS);
    procedure SetValueAsBoolean(Value: Boolean);
    procedure SetValueAsString(Value: Ansistring);
    procedure SetValueAsBigInteger(Value: Ansistring);
    procedure SetValueAsByteArray(Value: Ansistring);
    procedure SetValueAsCertificateType(Value: CK_CERTIFICATE_TYPE);
    procedure SetValueAsNumber(Value: CK_ULONG);
    procedure SetValueAsKeyType(Value: CK_KEY_TYPE);
    procedure SetValueAsDate(Value: TDate);
    procedure SetValueAsMechanismType(Value: CK_MECHANISM_TYPE);
    //    procedure SetValueAsMechanismTypeSet(value: array of CK_MECHANISM_TYPE);
    //    procedure SetValueAsAttributeSet(value: array of CK_ATTRIBUTE_TYPE);
    procedure SetValueAsHWFeatureType(Value: CK_HW_FEATURE_TYPE);

    function DisplayValue_BigInteger(): String;

  public
    PKCS11API: TObject;

    property AttribType: CK_ATTRIBUTE_TYPE Read FAttribType Write FAttribType;
    property AttribTypeStr: String Read GetAttribTypeStr;
    property AttrLength: Cardinal Read GetLength;

    property ValueAsRawData: Ansistring Read FRawData Write FRawData;
    function DisplayValue(): String;

    function AttributeTypeToString(attrType: CK_ATTRIBUTE_TYPE): String;

    property AttrStruct: CK_ATTRIBUTE Write SetAttrStruct;

    // Note: Only one of the following members will be populated, depending on
    //       FAttribType
    property ValueAsObjClass: CK_OBJECT_CLASS Read GetValueAsObjClass Write SetValueAsObjClass;
    property ValueAsBoolean: Boolean Read GetValueAsBoolean Write SetValueAsBoolean;
    property ValueAsString: Ansistring Read GetValueAsString Write SetValueAsString;
    property ValueAsBigInteger: Ansistring Read GetValueAsBigInteger Write SetValueAsBigInteger;
    property ValueAsByteArray: Ansistring Read GetValueAsByteArray Write SetValueAsByteArray;
    property ValueAsCertificateType: CK_CERTIFICATE_TYPE
      Read GetValueAsCertificateType Write SetValueAsCertificateType;
    property ValueAsNumber: Cardinal Read GetValueAsNumber Write SetValueAsNumber;
    property ValueAsKeyType: CK_KEY_TYPE Read GetValueAsKeyType Write SetValueAsKeyType;
    property ValueAsDate: TDate Read GetValueAsDate Write SetValueAsDate;
    property ValueAsMechanismType: CK_MECHANISM_TYPE
      Read GetValueAsMechanismType Write SetValueAsMechanismType;
    //    property ValueAsMechanismTypeSet: array of CK_MECHANISM_TYPE read GetValueAsMechanismTypeSet write SetValueAsMechanismTypeSet;
    //    property ValueAsAttributeSet: array of CK_ATTRIBUTE_TYPE read GetValueAsAttributeSet write SetValueAsAttributeSet;
    property ValueAsHWFeatureType: CK_HW_FEATURE_TYPE
      Read GetValueAsHWFeatureType Write SetValueAsHWFeatureType;

  end;

  // NOTICE: When an attribute template is destroyed, all attribute objects it
  //         contains will also be destroyed.
  TPKCS11AttributeTemplate = class
  protected
    FAttributes: array of TPKCS11Attribute;

    function GetCount(): Integer;
    function GetAttribute(idx: Integer): TPKCS11Attribute;
    procedure SetAttribute(idx: Integer; attr: TPKCS11Attribute);
  public
    property Count: Integer Read GetCount;
    property Attribute[idx: Integer]: TPKCS11Attribute Read GetAttribute Write SetAttribute;

    constructor Create();
    destructor Destroy(); override;

    // Add an existing attribute/add a new attribute, return it
    function Add(attr: TPKCS11Attribute): TPKCS11Attribute; overload;
    function Add(): TPKCS11Attribute; overload;
  end;

implementation


uses
//delphi
 DateUtils,
  SysUtils,
//ls utils

  SDUGeneral, SDUi18n, lcConsts;

function TPKCS11Attribute.MapPKCS11AttrTypeToDelphiAttrType(pkcs11AttrType: CK_ATTRIBUTE_TYPE):
TAttributeDataType;

begin
  Result := adtUnknown;

  case pkcs11AttrType of
    // The following attribute types are defined:
    CKA_CLASS: Result       := adtObjClass;
    CKA_TOKEN: Result       := adtBoolean;
    CKA_PRIVATE: Result     := adtBoolean;
    CKA_LABEL: Result       := adtString;
    CKA_APPLICATION: Result := adtString;
    CKA_VALUE: Result       := adtByteArray; // or could be adtBigInteger

    // CKA_OBJECT_ID is new for v2.10
    CKA_OBJECT_ID: Result := adtByteArray;

    CKA_CERTIFICATE_TYPE: Result := adtCertificateType;
    CKA_ISSUER: Result           := adtByteArray;
    CKA_SERIAL_NUMBER: Result    := adtByteArray;

    // CKA_AC_ISSUER, CKA_OWNER, and CKA_ATTR_TYPES are new
    // for v2.10
    CKA_AC_ISSUER: Result  := adtByteArray;
    CKA_OWNER: Result      := adtByteArray;
    CKA_ATTR_TYPES: Result := adtByteArray;

    // CKA_TRUSTED is new for v2.11
    CKA_TRUSTED: Result := adtBoolean;

    // CKA_CERTIFICATE_CATEGORY ...
    // CKA_CHECK_VALUE are new for v2.20
    CKA_CERTIFICATE_CATEGORY: Result := adtNumber;
    CKA_JAVA_MIDP_SECURITY_DOMAIN: Result := adtByteArray;
    CKA_URL: Result         := adtString;
    CKA_HASH_OF_SUBJECT_PUBLIC_KEY: Result := adtByteArray;
    CKA_HASH_OF_ISSUER_PUBLIC_KEY: Result := adtByteArray;
    CKA_CHECK_VALUE: Result := adtByteArray;

    CKA_KEY_TYPE: Result         := adtKeyType;
    CKA_SUBJECT: Result          := adtByteArray;
    CKA_ID: Result               := adtByteArray;
    CKA_SENSITIVE: Result        := adtBoolean;
    CKA_ENCRYPT: Result          := adtBoolean;
    CKA_DECRYPT: Result          := adtBoolean;
    CKA_WRAP: Result             := adtBoolean;
    CKA_UNWRAP: Result           := adtBoolean;
    CKA_SIGN: Result             := adtBoolean;
    CKA_SIGN_RECOVER: Result     := adtBoolean;
    CKA_VERIFY: Result           := adtBoolean;
    CKA_VERIFY_RECOVER: Result   := adtBoolean;
    CKA_DERIVE: Result           := adtBoolean;
    CKA_START_DATE: Result       := adtDate;
    CKA_END_DATE: Result         := adtDate;
    CKA_MODULUS: Result          := adtBigInteger;
    CKA_MODULUS_BITS: Result     := adtNumber;
    CKA_PUBLIC_EXPONENT: Result  := adtBigInteger;
    CKA_PRIVATE_EXPONENT: Result := adtBigInteger;
    CKA_PRIME_1: Result          := adtBigInteger;
    CKA_PRIME_2: Result          := adtBigInteger;
    CKA_EXPONENT_1: Result       := adtBigInteger;
    CKA_EXPONENT_2: Result       := adtBigInteger;
    CKA_COEFFICIENT: Result      := adtBigInteger;
    CKA_PRIME: Result            := adtBigInteger;
    CKA_SUBPRIME: Result         := adtBigInteger;
    CKA_BASE: Result             := adtBigInteger;

    // CKA_PRIME_BITS and CKA_SUB_PRIME_BITS are new for v2.11
    CKA_PRIME_BITS: Result    := adtNumber;
    CKA_SUBPRIME_BITS: Result := adtNumber;
    // Value of CKA_SUBPRIME_BITS = CKA_SUB_PRIME_BITS
    // CKA_SUB_PRIME_BITS:              result := adtNumber;
    // (To retain backwards-compatibility)

    CKA_VALUE_BITS: Result := adtNumber;
    CKA_VALUE_LEN: Result  := adtNumber;

    // CKA_EXTRACTABLE, CKA_LOCAL, CKA_NEVER_EXTRACTABLE,
    // CKA_ALWAYS_SENSITIVE, CKA_MODIFIABLE, CKA_ECDSA_PARAMS,
    // and CKA_EC_POINT are new for v2.0
    CKA_EXTRACTABLE: Result       := adtBoolean;
    CKA_LOCAL: Result             := adtBoolean;
    CKA_NEVER_EXTRACTABLE: Result := adtBoolean;
    CKA_ALWAYS_SENSITIVE: Result  := adtBoolean;

    // CKA_KEY_GEN_MECHANISM is new for v2.11
    CKA_KEY_GEN_MECHANISM: Result := adtMechanismType;

    CKA_MODIFIABLE: Result := adtBoolean;

    // CKA_ECDSA_PARAMS is deprecated in v2.11,
    // CKA_EC_PARAMS is preferred.
    CKA_ECDSA_PARAMS: Result := adtByteArray;
    // Value of CKA_ECDSA_PARAMS = CKA_EC_PARAMS
    // CKA_EC_PARAMS:                   result := adtByteArray;

    CKA_EC_POINT: Result := adtByteArray;

    // CKA_SECONDARY_AUTH, CKA_AUTH_PIN_FLAGS,
    // are new for v2.10. Deprecated in v2.11 and onwards.
    CKA_SECONDARY_AUTH: Result := adtUnknown;
    CKA_AUTH_PIN_FLAGS: Result := adtUnknown;

    // CKA_ALWAYS_AUTHENTICATE ...
    // CKA_UNWRAP_TEMPLATE are new for v2.20
    CKA_ALWAYS_AUTHENTICATE: Result := adtBoolean;

    CKA_WRAP_WITH_TRUSTED: Result := adtBoolean;
    CKA_WRAP_TEMPLATE: Result     := adtAttributeSet;
    CKA_UNWRAP_TEMPLATE: Result   := adtAttributeSet;

    // CKA_OTP... atttributes are new for PKCS #11 v2.20 amendment 3.
    CKA_OTP_FORMAT: Result                := adtUnknown;
    CKA_OTP_LENGTH: Result                := adtUnknown;
    CKA_OTP_TIME_INTERVAL: Result         := adtUnknown;
    CKA_OTP_USER_FRIENDLY_MODE: Result    := adtUnknown;
    CKA_OTP_CHALLENGE_REQUIREMENT: Result := adtUnknown;
    CKA_OTP_TIME_REQUIREMENT: Result      := adtUnknown;
    CKA_OTP_COUNTER_REQUIREMENT: Result   := adtUnknown;
    CKA_OTP_PIN_REQUIREMENT: Result       := adtUnknown;
    CKA_OTP_COUNTER: Result               := adtUnknown;
    CKA_OTP_TIME: Result                  := adtUnknown;
    CKA_OTP_USER_IDENTIFIER: Result       := adtUnknown;
    CKA_OTP_SERVICE_IDENTIFIER: Result    := adtUnknown;
    CKA_OTP_SERVICE_LOGO: Result          := adtUnknown;
    CKA_OTP_SERVICE_LOGO_TYPE: Result     := adtUnknown;


    // CKA_HW_FEATURE_TYPE, CKA_RESET_ON_INIT, and CKA_HAS_RESET
    // are new for v2.10
    CKA_HW_FEATURE_TYPE: Result := adtHWFeatureType;
    CKA_RESET_ON_INIT: Result   := adtBoolean;
    CKA_HAS_RESET: Result       := adtBoolean;

    // The following attributes are new for v2.20
    CKA_PIXEL_X: Result                  := adtNumber;
    CKA_PIXEL_Y: Result                  := adtNumber;
    CKA_RESOLUTION: Result               := adtNumber;
    CKA_CHAR_ROWS: Result                := adtNumber;
    CKA_CHAR_COLUMNS: Result             := adtNumber;
    CKA_COLOR: Result                    := adtBoolean;
    CKA_BITS_PER_PIXEL: Result           := adtNumber;
    CKA_CHAR_SETS: Result                := adtString;
    CKA_ENCODING_METHODS: Result         := adtString;
    CKA_MIME_TYPES: Result               := adtString;
    CKA_MECHANISM_TYPE: Result           := adtMechanismType;
    CKA_REQUIRED_CMS_ATTRIBUTES: Result  := adtByteArray;
    CKA_DEFAULT_CMS_ATTRIBUTES: Result   := adtByteArray;
    CKA_SUPPORTED_CMS_ATTRIBUTES: Result := adtByteArray;
    CKA_ALLOWED_MECHANISMS: Result       := adtMechanismTypeSet;

    CKA_VENDOR_DEFINED: Result := adtUnknown;
  end;

end;

 // Note: This function is a slightly modified version of
 //       SDUGeneral.SDUPrettyPrintHexStrSimple(...)
function TPKCS11Attribute.DisplayValue_BigInteger(): String;
var
  i:             Integer;
  useBigInteger: String;
begin
  Result := '0x';

  // If the BigInteger has no data, render as "0" (zero)
  useBigInteger := ValueAsBigInteger;
  if (System.length(useBigInteger) = 0) then begin
    useBigInteger := #0;
  end;

  for i := 1 to System.length(useBigInteger) do begin
    Result := Result + inttohex(Ord(useBigInteger[i]), 2);
  end;

end;

function TPKCS11Attribute.DisplayValue(): String;
begin
  Result := '<unhandled attrib type>';

  case MapPKCS11AttrTypeToDelphiAttrType(AttribType) of
    adtObjClass: Result        := ObjectClassToString(ValueAsObjClass);
    adtBoolean: Result         := SDUBoolToStr(ValueAsBoolean);
    adtString: Result          := ValueAsString;
    adtBigInteger: Result      := DisplayValue_BigInteger();
    adtByteArray: Result       := SDUPrettyPrintHexStrSimple(ValueAsByteArray);
    adtCertificateType: Result := CertificateTypeToString(ValueAsCertificateType);
    adtNumber: Result          := IntToStr(ValueAsNumber);
    adtKeyType: Result         := KeyTypeToString(ValueAsKeyType);
    adtDate:
    begin
      Result := '<no date>';
      if (ValueAsDate <> 0) then begin
        Result := DateToStr(ValueAsDate);
      end;
    end;
    adtMechanismType: Result    := IntToStr(ValueAsMechanismType);
    adtMechanismTypeSet: Result := '<Mechanism set>';
    adtAttributeSet: Result     := '<Attribute set>';
    adtHWFeatureType: Result    := HWFeatureTypeToString(ValueAsHWFeatureType);
  end;

end;

function TPKCS11Attribute.GetAttribTypeStr(): String;
begin
  Result := AttributeTypeToString(FAttribType);
end;

function TPKCS11Attribute.AttributeTypeToString(attrType: CK_ATTRIBUTE_TYPE): String;
begin
  Result := RS_UNKNOWN;

  case attrType of
    CKA_CLASS: Result                  := 'CKA_CLASS';
    CKA_TOKEN: Result                  := 'CKA_TOKEN';
    CKA_PRIVATE: Result                := 'CKA_PRIVATE';
    CKA_LABEL: Result                  := 'CKA_LABEL';
    CKA_APPLICATION: Result            := 'CKA_APPLICATION';
    CKA_VALUE: Result                  := 'CKA_VALUE';
    CKA_OBJECT_ID: Result              := 'CKA_OBJECT_ID';
    CKA_CERTIFICATE_TYPE: Result       := 'CKA_CERTIFICATE_TYPE';
    CKA_ISSUER: Result                 := 'CKA_ISSUER';
    CKA_SERIAL_NUMBER: Result          := 'CKA_SERIAL_NUMBER';
    CKA_AC_ISSUER: Result              := 'CKA_AC_ISSUER';
    CKA_OWNER: Result                  := 'CKA_OWNER';
    CKA_ATTR_TYPES: Result             := 'CKA_ATTR_TYPES';
    CKA_TRUSTED: Result                := 'CKA_TRUSTED';
    CKA_CERTIFICATE_CATEGORY: Result   := 'CKA_CERTIFICATE_CATEGORY';
    CKA_JAVA_MIDP_SECURITY_DOMAIN: Result := 'CKA_JAVA_MIDP_SECURITY_DOMAIN';
    CKA_URL: Result                    := 'CKA_URL';
    CKA_HASH_OF_SUBJECT_PUBLIC_KEY: Result := 'CKA_HASH_OF_SUBJECT_PUBLIC_KEY';
    CKA_HASH_OF_ISSUER_PUBLIC_KEY: Result := 'CKA_HASH_OF_ISSUER_PUBLIC_KEY';
    CKA_CHECK_VALUE: Result            := 'CKA_CHECK_VALUE';
    CKA_KEY_TYPE: Result               := 'CKA_KEY_TYPE';
    CKA_SUBJECT: Result                := 'CKA_SUBJECT';
    CKA_ID: Result                     := 'CKA_ID';
    CKA_SENSITIVE: Result              := 'CKA_SENSITIVE';
    CKA_ENCRYPT: Result                := 'CKA_ENCRYPT';
    CKA_DECRYPT: Result                := 'CKA_DECRYPT';
    CKA_WRAP: Result                   := 'CKA_WRAP';
    CKA_UNWRAP: Result                 := 'CKA_UNWRAP';
    CKA_SIGN: Result                   := 'CKA_SIGN';
    CKA_SIGN_RECOVER: Result           := 'CKA_SIGN_RECOVER';
    CKA_VERIFY: Result                 := 'CKA_VERIFY';
    CKA_VERIFY_RECOVER: Result         := 'CKA_VERIFY_RECOVER';
    CKA_DERIVE: Result                 := 'CKA_DERIVE';
    CKA_START_DATE: Result             := 'CKA_START_DATE';
    CKA_END_DATE: Result               := 'CKA_END_DATE';
    CKA_MODULUS: Result                := 'CKA_MODULUS';
    CKA_MODULUS_BITS: Result           := 'CKA_MODULUS_BITS';
    CKA_PUBLIC_EXPONENT: Result        := 'CKA_PUBLIC_EXPONENT';
    CKA_PRIVATE_EXPONENT: Result       := 'CKA_PRIVATE_EXPONENT';
    CKA_PRIME_1: Result                := 'CKA_PRIME_1';
    CKA_PRIME_2: Result                := 'CKA_PRIME_2';
    CKA_EXPONENT_1: Result             := 'CKA_EXPONENT_1';
    CKA_EXPONENT_2: Result             := 'CKA_EXPONENT_2';
    CKA_COEFFICIENT: Result            := 'CKA_COEFFICIENT';
    CKA_PRIME: Result                  := 'CKA_PRIME';
    CKA_SUBPRIME: Result               := 'CKA_SUBPRIME';
    CKA_BASE: Result                   := 'CKA_BASE';
    CKA_PRIME_BITS: Result             := 'CKA_PRIME_BITS';
    CKA_SUBPRIME_BITS: Result          := 'CKA_SUBPRIME_BITS';
    //CKA_SUB_PRIME_BITS:             result := 'CKA_SUB_PRIME_BITS';
    CKA_VALUE_BITS: Result             := 'CKA_VALUE_BITS';
    CKA_VALUE_LEN: Result              := 'CKA_VALUE_LEN';
    CKA_EXTRACTABLE: Result            := 'CKA_EXTRACTABLE';
    CKA_LOCAL: Result                  := 'CKA_LOCAL';
    CKA_NEVER_EXTRACTABLE: Result      := 'CKA_NEVER_EXTRACTABLE';
    CKA_ALWAYS_SENSITIVE: Result       := 'CKA_ALWAYS_SENSITIVE';
    CKA_KEY_GEN_MECHANISM: Result      := 'CKA_KEY_GEN_MECHANISM';
    CKA_MODIFIABLE: Result             := 'CKA_MODIFIABLE';
    CKA_ECDSA_PARAMS: Result           := 'CKA_ECDSA_PARAMS';
    //CKA_EC_PARAMS:                  result := 'CKA_EC_PARAMS';
    CKA_EC_POINT: Result               := 'CKA_EC_POINT';
    CKA_SECONDARY_AUTH: Result         := 'CKA_SECONDARY_AUTH';
    CKA_AUTH_PIN_FLAGS: Result         := 'CKA_AUTH_PIN_FLAGS';
    CKA_ALWAYS_AUTHENTICATE: Result    := 'CKA_ALWAYS_AUTHENTICATE';
    CKA_WRAP_WITH_TRUSTED: Result      := 'CKA_WRAP_WITH_TRUSTED';
    CKA_WRAP_TEMPLATE: Result          := 'CKA_WRAP_TEMPLATE';
    CKA_UNWRAP_TEMPLATE: Result        := 'CKA_UNWRAP_TEMPLATE';
    CKA_OTP_FORMAT: Result             := 'CKA_OTP_FORMAT';
    CKA_OTP_LENGTH: Result             := 'CKA_OTP_LENGTH';
    CKA_OTP_TIME_INTERVAL: Result      := 'CKA_OTP_TIME_INTERVAL';
    CKA_OTP_USER_FRIENDLY_MODE: Result := 'CKA_OTP_USER_FRIENDLY_MODE';
    CKA_OTP_CHALLENGE_REQUIREMENT: Result := 'CKA_OTP_CHALLENGE_REQUIREMENT';
    CKA_OTP_TIME_REQUIREMENT: Result   := 'CKA_OTP_TIME_REQUIREMENT';
    CKA_OTP_COUNTER_REQUIREMENT: Result := 'CKA_OTP_COUNTER_REQUIREMENT';
    CKA_OTP_PIN_REQUIREMENT: Result    := 'CKA_OTP_PIN_REQUIREMENT';
    CKA_OTP_COUNTER: Result            := 'CKA_OTP_COUNTER';
    CKA_OTP_TIME: Result               := 'CKA_OTP_TIME';
    CKA_OTP_USER_IDENTIFIER: Result    := 'CKA_OTP_USER_IDENTIFIER';
    CKA_OTP_SERVICE_IDENTIFIER: Result := 'CKA_OTP_SERVICE_IDENTIFIER';
    CKA_OTP_SERVICE_LOGO: Result       := 'CKA_OTP_SERVICE_LOGO';
    CKA_OTP_SERVICE_LOGO_TYPE: Result  := 'CKA_OTP_SERVICE_LOGO_TYPE';
    CKA_HW_FEATURE_TYPE: Result        := 'CKA_HW_FEATURE_TYPE';
    CKA_RESET_ON_INIT: Result          := 'CKA_RESET_ON_INIT';
    CKA_HAS_RESET: Result              := 'CKA_HAS_RESET';
    CKA_PIXEL_X: Result                := 'CKA_PIXEL_X';
    CKA_PIXEL_Y: Result                := 'CKA_PIXEL_Y';
    CKA_RESOLUTION: Result             := 'CKA_RESOLUTION';
    CKA_CHAR_ROWS: Result              := 'CKA_CHAR_ROWS';
    CKA_CHAR_COLUMNS: Result           := 'CKA_CHAR_COLUMNS';
    CKA_COLOR: Result                  := 'CKA_COLOR';
    CKA_BITS_PER_PIXEL: Result         := 'CKA_BITS_PER_PIXEL';
    CKA_CHAR_SETS: Result              := 'CKA_CHAR_SETS';
    CKA_ENCODING_METHODS: Result       := 'CKA_ENCODING_METHODS';
    CKA_MIME_TYPES: Result             := 'CKA_MIME_TYPES';
    CKA_MECHANISM_TYPE: Result         := 'CKA_MECHANISM_TYPE';
    CKA_REQUIRED_CMS_ATTRIBUTES: Result := 'CKA_REQUIRED_CMS_ATTRIBUTES';
    CKA_DEFAULT_CMS_ATTRIBUTES: Result := 'CKA_DEFAULT_CMS_ATTRIBUTES';
    CKA_SUPPORTED_CMS_ATTRIBUTES: Result := 'CKA_SUPPORTED_CMS_ATTRIBUTES';
    CKA_ALLOWED_MECHANISMS: Result     := 'CKA_ALLOWED_MECHANISMS';
    CKA_VENDOR_DEFINED: Result         := 'CKA_VENDOR_DEFINED';
  end;

end;

function TPKCS11Attribute.GetLength(): Cardinal;
begin
  Result := FLength;
end;

function TPKCS11Attribute.CopyBinary(ptr: PByte; len: Integer): Ansistring;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to (len - 1) do begin
    Result := Result + Ansichar(PByte(ptr)^);
    Inc(ptr);
  end;
end;


procedure TPKCS11Attribute.SetAttrStruct(struct: CK_ATTRIBUTE);
begin
  FAttribType := struct.attrType;
  FLength     := struct.ulValueLen;
  FRawData    := CopyBinary(struct.pValue, struct.ulValueLen);
end;

function TPKCS11Attribute.ObjectClassToString(objClass: CK_OBJECT_CLASS): String;
begin
  Result := RS_UNKNOWN;

  case objClass of
    CKO_DATA: Result              := 'CKO_DATA';
    CKO_CERTIFICATE: Result       := 'CKO_CERTIFICATE';
    CKO_PUBLIC_KEY: Result        := 'CKO_PUBLIC_KEY';
    CKO_PRIVATE_KEY: Result       := 'CKO_PRIVATE_KEY';
    CKO_SECRET_KEY: Result        := 'CKO_SECRET_KEY';
    CKO_HW_FEATURE: Result        := 'CKO_HW_FEATURE';
    CKO_DOMAIN_PARAMETERS: Result := 'CKO_DOMAIN_PARAMETERS';
    CKO_MECHANISM: Result         := 'CKO_MECHANISM';
    CKO_OTP_KEY: Result           := 'CKO_OTP_KEY';
    CKO_VENDOR_DEFINED: Result    := 'CKO_VENDOR_DEFINED';
  end;

end;

function TPKCS11Attribute.HWFeatureTypeToString(HWFeatureType: CK_HW_FEATURE_TYPE): String;
begin
  Result := RS_UNKNOWN;

  case HWFeatureType of
    CKH_MONOTONIC_COUNTER: Result := 'CKH_MONOTONIC_COUNTER';
    CKH_CLOCK: Result             := 'CKH_CLOCK';
    CKH_USER_INTERFACE: Result    := 'CKH_USER_INTERFACE';
    CKH_VENDOR_DEFINED: Result    := 'CKH_VENDOR_DEFINED';
  end;

end;


function TPKCS11Attribute.KeyTypeToString(keyType: CK_KEY_TYPE): String;
begin
  Result := RS_UNKNOWN;

  case keyType of
    CKK_RSA: Result            := 'CKK_RSA';
    CKK_DSA: Result            := 'CKK_DSA';
    CKK_DH: Result             := 'CKK_DH';
    CKK_ECDSA: Result          := 'CKK_ECDSA';
    //CKK_EC:             result := 'CKK_EC';
    CKK_X9_42_DH: Result       := 'CKK_X9_42_DH';
    CKK_KEA: Result            := 'CKK_KEA';
    CKK_GENERIC_SECRET: Result := 'CKK_GENERIC_SECRET';
    CKK_RC2: Result            := 'CKK_RC2';
    CKK_RC4: Result            := 'CKK_RC4';
    CKK_DES: Result            := 'CKK_DES';
    CKK_DES2: Result           := 'CKK_DES2';
    CKK_DES3: Result           := 'CKK_DES3';
    CKK_CAST: Result           := 'CKK_CAST';
    CKK_CAST3: Result          := 'CKK_CAST3';
    CKK_CAST5: Result          := 'CKK_CAST5';
    //CKK_CAST128:        result := 'CKK_CAST128';
    CKK_RC5: Result            := 'CKK_RC5';
    CKK_IDEA: Result           := 'CKK_IDEA';
    CKK_SKIPJACK: Result       := 'CKK_SKIPJACK';
    CKK_BATON: Result          := 'CKK_BATON';
    CKK_JUNIPER: Result        := 'CKK_JUNIPER';
    CKK_CDMF: Result           := 'CKK_CDMF';
    CKK_AES: Result            := 'CKK_AES';
    CKK_BLOWFISH: Result       := 'CKK_BLOWFISH';
    CKK_TWOFISH: Result        := 'CKK_TWOFISH';
    CKK_SECURID: Result        := 'CKK_SECURID';
    CKK_HOTP: Result           := 'CKK_HOTP';
    CKK_ACTI: Result           := 'CKK_ACTI';
    CKK_CAMELLIA: Result       := 'CKK_CAMELLIA';
    CKK_ARIA: Result           := 'CKK_ARIA';
    CKK_VENDOR_DEFINED: Result := 'CKK_VENDOR_DEFINED';
  end;

end;

function TPKCS11Attribute.CertificateTypeToString(certType: CK_CERTIFICATE_TYPE): String;
begin
  Result := RS_UNKNOWN;

  case certType of
    CKC_X_509: Result           := 'CKC_X_509';
    CKC_X_509_ATTR_CERT: Result := 'CKC_X_509_ATTR_CERT';
    CKC_WTLS: Result            := 'CKC_WTLS';
    CKC_VENDOR_DEFINED: Result  := 'CKC_VENDOR_DEFINED';
  end;

end;

function TPKCS11Attribute.GetValueAsObjClass(): CK_OBJECT_CLASS;
begin
  Result := (PCK_OBJECT_CLASS(PAnsiChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsBoolean(): Boolean;
begin
  Result := (((PCK_BBOOL(PAnsiChar(FRawData)))^) = CK_TRUE);
end;

function TPKCS11Attribute.GetValueAsString(): Ansistring;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsBigInteger(): Ansistring;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsByteArray(): Ansistring;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsCertificateType(): CK_CERTIFICATE_TYPE;
begin
  Result := (PCK_CERTIFICATE_TYPE(PAnsiChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsNumber(): CK_ULONG;
begin
  if (length(FRawData) < sizeof(Result)) then begin
    Result := 0;
  end else begin
    Result := (PCK_ULONG(FRawData))^;
  end;

end;

function TPKCS11Attribute.GetValueAsKeyType(): CK_KEY_TYPE;
begin
  Result := (PCK_KEY_TYPE(PAnsiChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsDate(): TDate;
var
  tmpCK_DATE: CK_DATE;
begin
  Result := 0;
  if (length(FRawData) >= sizeof(tmpCK_DATE)) then begin
    tmpCK_DATE := (PCK_DATE(PAnsiChar(FRawData)))^;
    Result     := CK_DATEToDate(tmpCK_DATE);
  end;

end;

function TPKCS11Attribute.GetValueAsMechanismType(): CK_MECHANISM_TYPE;
begin
  Result := (PCK_MECHANISM_TYPE(PAnsiChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsHWFeatureType(): CK_HW_FEATURE_TYPE;
begin
  Result := (PCK_HW_FEATURE_TYPE(PAnsiChar(FRawData)))^;
end;


procedure TPKCS11Attribute.SetValueAsObjClass(Value: CK_OBJECT_CLASS);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;

procedure TPKCS11Attribute.SetValueAsBoolean(Value: Boolean);
var
  tmpCK_BBOOL: CK_BBOOL;
begin
  tmpCK_BBOOL := CK_TRUE;
  if not (Value) then begin
    tmpCK_BBOOL := CK_FALSE;
  end;

  FRawData := CopyBinary(@tmpCK_BBOOL, sizeof(tmpCK_BBOOL));
end;

procedure TPKCS11Attribute.SetValueAsString(Value: Ansistring);
begin
  FRawData := Value;
end;

procedure TPKCS11Attribute.SetValueAsBigInteger(Value: Ansistring);
begin
  FRawData := Value;
end;

procedure TPKCS11Attribute.SetValueAsByteArray(Value: Ansistring);
begin
  FRawData := Value;
end;

procedure TPKCS11Attribute.SetValueAsCertificateType(Value: CK_CERTIFICATE_TYPE);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;

procedure TPKCS11Attribute.SetValueAsNumber(Value: CK_ULONG);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;

procedure TPKCS11Attribute.SetValueAsKeyType(Value: CK_KEY_TYPE);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;

procedure TPKCS11Attribute.SetValueAsDate(Value: TDate);
var
  tmpCK_DATE: CK_DATE;
begin
  tmpCK_DATE := DateToCK_DATE(Value);
  FRawData   := CopyBinary(@tmpCK_DATE, sizeof(tmpCK_DATE));
end;

procedure TPKCS11Attribute.SetValueAsMechanismType(Value: CK_MECHANISM_TYPE);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;

procedure TPKCS11Attribute.SetValueAsHWFeatureType(Value: CK_HW_FEATURE_TYPE);
begin
  FRawData := CopyBinary(@Value, sizeof(Value));
end;


constructor TPKCS11AttributeTemplate.Create();
begin
  inherited;
  SetLength(FAttributes, 0);
end;

destructor TPKCS11AttributeTemplate.Destroy();
var
  i: Integer;
begin
  for i := low(FAttributes) to high(FAttributes) do begin
    FAttributes[i].Free();
  end;

  inherited;
end;

function TPKCS11AttributeTemplate.GetCount(): Integer;
begin
  Result := length(FAttributes);
end;

function TPKCS11AttributeTemplate.GetAttribute(idx: Integer): TPKCS11Attribute;
begin
  Assert(
    (idx >= 0) and (idx < Count),
    'Index out of bounds'
    );

  Result := FAttributes[idx];
end;

procedure TPKCS11AttributeTemplate.SetAttribute(idx: Integer; attr: TPKCS11Attribute);
begin
  Assert(
    (idx >= 0) and (idx < Count),
    'Index out of bounds'
    );

  FAttributes[idx] := attr;
end;

// Add an existing attribute/add a new attribute, return it
function TPKCS11AttributeTemplate.Add(attr: TPKCS11Attribute): TPKCS11Attribute;
begin
  SetLength(FAttributes, (length(FAttributes) + 1));
  // -1 because the array indexes from 0
  FAttributes[(length(FAttributes) - 1)] := attr;

  Result := attr;
end;

function TPKCS11AttributeTemplate.Add(): TPKCS11Attribute;
var
  newAttr: TPKCS11Attribute;
begin
  newAttr := TPKCS11Attribute.Create();

  // Junk data
  newAttr.AttribType      := CKA_CLASS;
  newAttr.ValueAsObjClass := CKO_DATA;

  Result := Add(newAttr);
end;


end.
