unit pkcs11_attribute;

interface

uses
  Controls,
  pkcs11t,
  pkcs11_api;

type
  PCK_OBJECT_CLASS     = ^CK_OBJECT_CLASS;
  PCK_BBOOL            = ^CK_BBOOL;
  PCK_CERTIFICATE_TYPE = ^CK_CERTIFICATE_TYPE;
  PCK_ULONG            = ^CK_ULONG;
  PCK_KEY_TYPE         = ^CK_KEY_TYPE;
  PCK_DATE             = ^CK_DATE;
  PCK_MECHANISM_TYPE   = ^CK_MECHANISM_TYPE;
  PCK_ATTRIBUTE_PTR    = ^CK_ATTRIBUTE_PTR;
  PCK_HW_FEATURE_TYPE  = ^CK_HW_FEATURE_TYPE;

  TAttributeDataType = (
                        adtObjClass, // CK_OBJECT_CLASS
                        adtBoolean, // CK_BBOOL
                        adtString, // RFC 2279 string
                        adtBigInteger, // Big integer
                        adtByteArray, // byte array
                        adtCertificateType, // CK_CERTIFICATE_TYPE
                        adtNumber, // CK_ULONG
                        adtKeyType, // CK_KEY_TYPE
                        adtDate, // CK_DATE
                        adtMechanismType, // CK_MECHANISM_TYPE
                        adtMechanismTypeSet, // CK_MECHANISM_TYPE_PTR
                        adtUnknown, // ???
                        adtAttributeSet, // CK_ATTRIBUTE_PTR
                        adtHWFeatureType // CK_HW_FEATURE_TYPE
                       );


  TPKCS11Attribute = class(TPKCS11API)
  private
    FAttribType: CK_ATTRIBUTE_TYPE;
    FLength: cardinal;
    FRawData: AnsiString;

    function MapPKCS11AttrTypeToDelphiAttrType(pkcs11AttrType: CK_ATTRIBUTE_TYPE): TAttributeDataType;

  protected
    function GetAttribTypeStr(): string;
    function GetLength(): cardinal;

    procedure SetAttrStruct(struct: CK_ATTRIBUTE);

    function ObjectClassToString(objClass: CK_OBJECT_CLASS): string;
    function HWFeatureTypeToString(HWFeatureType: CK_HW_FEATURE_TYPE): string;
    function KeyTypeToString(keyType: CK_KEY_TYPE): string;
    function CertificateTypeToString(certType: CK_CERTIFICATE_TYPE): string;

    function CopyBinary(ptr: PByte; len: integer): string;

    // Get methods...
    function GetValueAsObjClass(): CK_OBJECT_CLASS;
    function GetValueAsBoolean(): boolean;
    function GetValueAsString(): string;
    function GetValueAsBigInteger(): string;
    function GetValueAsByteArray(): string;
    function GetValueAsCertificateType(): CK_CERTIFICATE_TYPE;
    function GetValueAsNumber(): CK_ULONG;
    function GetValueAsKeyType(): CK_KEY_TYPE;
    function GetValueAsDate(): TDate;
    function GetValueAsMechanismType(): CK_MECHANISM_TYPE;
//    function GetValueAsMechanismTypeSet(): array of CK_MECHANISM_TYPE;
//    function GetValueAsAttributeSet(): array of CK_ATTRIBUTE_TYPE;
    function GetValueAsHWFeatureType(): CK_HW_FEATURE_TYPE;

    // Set methods...
    procedure SetValueAsObjClass(value: CK_OBJECT_CLASS);
    procedure SetValueAsBoolean(value: boolean);
    procedure SetValueAsString(value: string);
    procedure SetValueAsBigInteger(value: string);
    procedure SetValueAsByteArray(value: string);
    procedure SetValueAsCertificateType(value: CK_CERTIFICATE_TYPE);
    procedure SetValueAsNumber(value: CK_ULONG);
    procedure SetValueAsKeyType(value: CK_KEY_TYPE);
    procedure SetValueAsDate(value: TDate);
    procedure SetValueAsMechanismType(value: CK_MECHANISM_TYPE);
//    procedure SetValueAsMechanismTypeSet(value: array of CK_MECHANISM_TYPE);
//    procedure SetValueAsAttributeSet(value: array of CK_ATTRIBUTE_TYPE);
    procedure SetValueAsHWFeatureType(value: CK_HW_FEATURE_TYPE);

    function  DisplayValue_BigInteger(): string;

  public
    PKCS11API: TObject;

    property AttribType: CK_ATTRIBUTE_TYPE read FAttribType write FAttribType;
    property AttribTypeStr: string read GetAttribTypeStr;
    property AttrLength: cardinal read GetLength;

    property ValueAsRawData: AnsiString read FRawData write FRawData;
    function DisplayValue(): string;

    function AttributeTypeToString(attrType: CK_ATTRIBUTE_TYPE): string;

    property AttrStruct: CK_ATTRIBUTE write SetAttrStruct;

    // Note: Only one of the following members will be populated, depending on
    //       FAttribType
    property ValueAsObjClass: CK_OBJECT_CLASS read GetValueAsObjClass write SetValueAsObjClass;
    property ValueAsBoolean: boolean read GetValueAsBoolean write SetValueAsBoolean;
    property ValueAsString: string read GetValueAsString write SetValueAsString;
    property ValueAsBigInteger: string read GetValueAsBigInteger write SetValueAsBigInteger;
    property ValueAsByteArray: string read GetValueAsByteArray write SetValueAsByteArray;
    property ValueAsCertificateType: CK_CERTIFICATE_TYPE read GetValueAsCertificateType write SetValueAsCertificateType;
    property ValueAsNumber: cardinal read GetValueAsNumber write SetValueAsNumber;
    property ValueAsKeyType: CK_KEY_TYPE read GetValueAsKeyType write SetValueAsKeyType;
    property ValueAsDate: TDate read GetValueAsDate write SetValueAsDate;
    property ValueAsMechanismType: CK_MECHANISM_TYPE read GetValueAsMechanismType write SetValueAsMechanismType;
//    property ValueAsMechanismTypeSet: array of CK_MECHANISM_TYPE read GetValueAsMechanismTypeSet write SetValueAsMechanismTypeSet;
//    property ValueAsAttributeSet: array of CK_ATTRIBUTE_TYPE read GetValueAsAttributeSet write SetValueAsAttributeSet;
    property ValueAsHWFeatureType: CK_HW_FEATURE_TYPE read GetValueAsHWFeatureType write SetValueAsHWFeatureType;

  end;

  // NOTICE: When an attribute template is destroyed, all attribute objects it
  //         contains will also be destroyed.
  TPKCS11AttributeTemplate = class
  protected
    FAttributes: array of TPKCS11Attribute;

    function GetCount(): integer;
    function GetAttribute(idx: integer): TPKCS11Attribute;
    procedure SetAttribute(idx: integer; attr: TPKCS11Attribute);
  public
    property Count: integer read GetCount;
    property Attribute[idx: integer]: TPKCS11Attribute read GetAttribute write SetAttribute;

    constructor Create();
    destructor Destroy(); override;

    // Add an existing attribute/add a new attribute, return it
    function Add(attr: TPKCS11Attribute): TPKCS11Attribute; overload;
    function Add(): TPKCS11Attribute; overload;
  end;

implementation

uses
  SysUtils,
  DateUtils,
  SDUi18n,
  SDUGeneral;

function TPKCS11Attribute.MapPKCS11AttrTypeToDelphiAttrType(pkcs11AttrType: CK_ATTRIBUTE_TYPE): TAttributeDataType;
var
  retval: TAttributeDataType;
begin
  retval := adtUnknown;

  case pkcs11AttrType of
    // The following attribute types are defined:
    CKA_CLASS:                       retval := adtObjClass;
    CKA_TOKEN:                       retval := adtBoolean;
    CKA_PRIVATE:                     retval := adtBoolean;
    CKA_LABEL:                       retval := adtString;
    CKA_APPLICATION:                 retval := adtString;
    CKA_VALUE:                       retval := adtByteArray; // or could be adtBigInteger

    // CKA_OBJECT_ID is new for v2.10
    CKA_OBJECT_ID:                   retval := adtByteArray;

    CKA_CERTIFICATE_TYPE:            retval := adtCertificateType;
    CKA_ISSUER:                      retval := adtByteArray;
    CKA_SERIAL_NUMBER:               retval := adtByteArray;

    // CKA_AC_ISSUER, CKA_OWNER, and CKA_ATTR_TYPES are new
    // for v2.10
    CKA_AC_ISSUER:                   retval := adtByteArray;
    CKA_OWNER:                       retval := adtByteArray;
    CKA_ATTR_TYPES:                  retval := adtByteArray;

    // CKA_TRUSTED is new for v2.11
    CKA_TRUSTED:                     retval := adtBoolean;

    // CKA_CERTIFICATE_CATEGORY ...
    // CKA_CHECK_VALUE are new for v2.20
    CKA_CERTIFICATE_CATEGORY:        retval := adtNumber;
    CKA_JAVA_MIDP_SECURITY_DOMAIN:   retval := adtByteArray;
    CKA_URL:                         retval := adtString;
    CKA_HASH_OF_SUBJECT_PUBLIC_KEY:  retval := adtByteArray;
    CKA_HASH_OF_ISSUER_PUBLIC_KEY:   retval := adtByteArray;
    CKA_CHECK_VALUE:                 retval := adtByteArray;

    CKA_KEY_TYPE:                    retval := adtKeyType;
    CKA_SUBJECT:                     retval := adtByteArray;
    CKA_ID:                          retval := adtByteArray;
    CKA_SENSITIVE:                   retval := adtBoolean;
    CKA_ENCRYPT:                     retval := adtBoolean;
    CKA_DECRYPT:                     retval := adtBoolean;
    CKA_WRAP:                        retval := adtBoolean;
    CKA_UNWRAP:                      retval := adtBoolean;
    CKA_SIGN:                        retval := adtBoolean;
    CKA_SIGN_RECOVER:                retval := adtBoolean;
    CKA_VERIFY:                      retval := adtBoolean;
    CKA_VERIFY_RECOVER:              retval := adtBoolean;
    CKA_DERIVE:                      retval := adtBoolean;
    CKA_START_DATE:                  retval := adtDate;
    CKA_END_DATE:                    retval := adtDate;
    CKA_MODULUS:                     retval := adtBigInteger;
    CKA_MODULUS_BITS:                retval := adtNumber;
    CKA_PUBLIC_EXPONENT:             retval := adtBigInteger;
    CKA_PRIVATE_EXPONENT:            retval := adtBigInteger;
    CKA_PRIME_1:                     retval := adtBigInteger;
    CKA_PRIME_2:                     retval := adtBigInteger;
    CKA_EXPONENT_1:                  retval := adtBigInteger;
    CKA_EXPONENT_2:                  retval := adtBigInteger;
    CKA_COEFFICIENT:                 retval := adtBigInteger;
    CKA_PRIME:                       retval := adtBigInteger;
    CKA_SUBPRIME:                    retval := adtBigInteger;
    CKA_BASE:                        retval := adtBigInteger;

    // CKA_PRIME_BITS and CKA_SUB_PRIME_BITS are new for v2.11
    CKA_PRIME_BITS:                  retval := adtNumber;
    CKA_SUBPRIME_BITS:               retval := adtNumber;
    // Value of CKA_SUBPRIME_BITS = CKA_SUB_PRIME_BITS
    // CKA_SUB_PRIME_BITS:              retval := adtNumber;
    // (To retain backwards-compatibility)

    CKA_VALUE_BITS:                  retval := adtNumber;
    CKA_VALUE_LEN:                   retval := adtNumber;

    // CKA_EXTRACTABLE, CKA_LOCAL, CKA_NEVER_EXTRACTABLE,
    // CKA_ALWAYS_SENSITIVE, CKA_MODIFIABLE, CKA_ECDSA_PARAMS,
    // and CKA_EC_POINT are new for v2.0
    CKA_EXTRACTABLE:                 retval := adtBoolean;
    CKA_LOCAL:                       retval := adtBoolean;
    CKA_NEVER_EXTRACTABLE:           retval := adtBoolean;
    CKA_ALWAYS_SENSITIVE:            retval := adtBoolean;

    // CKA_KEY_GEN_MECHANISM is new for v2.11
    CKA_KEY_GEN_MECHANISM:           retval := adtMechanismType;

    CKA_MODIFIABLE:                  retval := adtBoolean;

    // CKA_ECDSA_PARAMS is deprecated in v2.11,
    // CKA_EC_PARAMS is preferred.
    CKA_ECDSA_PARAMS:                retval := adtByteArray;
    // Value of CKA_ECDSA_PARAMS = CKA_EC_PARAMS
    // CKA_EC_PARAMS:                   retval := adtByteArray;

    CKA_EC_POINT:                    retval := adtByteArray;

    // CKA_SECONDARY_AUTH, CKA_AUTH_PIN_FLAGS,
    // are new for v2.10. Deprecated in v2.11 and onwards.
    CKA_SECONDARY_AUTH:              retval := adtUnknown;
    CKA_AUTH_PIN_FLAGS:              retval := adtUnknown;

    // CKA_ALWAYS_AUTHENTICATE ...
    // CKA_UNWRAP_TEMPLATE are new for v2.20
    CKA_ALWAYS_AUTHENTICATE:         retval := adtBoolean;

    CKA_WRAP_WITH_TRUSTED:           retval := adtBoolean;
    CKA_WRAP_TEMPLATE:               retval := adtAttributeSet;
    CKA_UNWRAP_TEMPLATE:             retval := adtAttributeSet;

    // CKA_OTP... atttributes are new for PKCS #11 v2.20 amendment 3.
    CKA_OTP_FORMAT:                  retval := adtUnknown;
    CKA_OTP_LENGTH:                  retval := adtUnknown;
    CKA_OTP_TIME_INTERVAL:           retval := adtUnknown;
    CKA_OTP_USER_FRIENDLY_MODE:      retval := adtUnknown;
    CKA_OTP_CHALLENGE_REQUIREMENT:   retval := adtUnknown;
    CKA_OTP_TIME_REQUIREMENT:        retval := adtUnknown;
    CKA_OTP_COUNTER_REQUIREMENT:     retval := adtUnknown;
    CKA_OTP_PIN_REQUIREMENT:         retval := adtUnknown;
    CKA_OTP_COUNTER:                 retval := adtUnknown;
    CKA_OTP_TIME:                    retval := adtUnknown;
    CKA_OTP_USER_IDENTIFIER:         retval := adtUnknown;
    CKA_OTP_SERVICE_IDENTIFIER:      retval := adtUnknown;
    CKA_OTP_SERVICE_LOGO:            retval := adtUnknown;
    CKA_OTP_SERVICE_LOGO_TYPE:       retval := adtUnknown;


    // CKA_HW_FEATURE_TYPE, CKA_RESET_ON_INIT, and CKA_HAS_RESET
    // are new for v2.10
    CKA_HW_FEATURE_TYPE:             retval := adtHWFeatureType;
    CKA_RESET_ON_INIT:               retval := adtBoolean;
    CKA_HAS_RESET:                   retval := adtBoolean;

    // The following attributes are new for v2.20
    CKA_PIXEL_X:                     retval := adtNumber;
    CKA_PIXEL_Y:                     retval := adtNumber;
    CKA_RESOLUTION:                  retval := adtNumber;
    CKA_CHAR_ROWS:                   retval := adtNumber;
    CKA_CHAR_COLUMNS:                retval := adtNumber;
    CKA_COLOR:                       retval := adtBoolean;
    CKA_BITS_PER_PIXEL:              retval := adtNumber;
    CKA_CHAR_SETS:                   retval := adtString;
    CKA_ENCODING_METHODS:            retval := adtString;
    CKA_MIME_TYPES:                  retval := adtString;
    CKA_MECHANISM_TYPE:              retval := adtMechanismType;
    CKA_REQUIRED_CMS_ATTRIBUTES:     retval := adtByteArray;
    CKA_DEFAULT_CMS_ATTRIBUTES:      retval := adtByteArray;
    CKA_SUPPORTED_CMS_ATTRIBUTES:    retval := adtByteArray;
    CKA_ALLOWED_MECHANISMS:          retval := adtMechanismTypeSet;

    CKA_VENDOR_DEFINED:              retval := adtUnknown;
  end;

  Result := retval;
end;

// Note: This function is a slightly modified version of
//       SDUGeneral.SDUPrettyPrintHexStrSimple(...)
function TPKCS11Attribute.DisplayValue_BigInteger(): string;
var
  retval: string;
  i: integer;
  useBigInteger: string;
begin
  retval := '0x';

  // If the BigInteger has no data, render as "0" (zero)
  useBigInteger := ValueAsBigInteger;
  if (System.length(useBigInteger) = 0) then
    begin
    useBigInteger := #0;
    end;

  for i:=1 to System.length(useBigInteger) do
    begin
    retval := retval + inttohex(ord(useBigInteger[i]), 2);
    end;

  Result := retval;
end;

function TPKCS11Attribute.DisplayValue(): string;
var
  retval: string;
begin
  retval := '<unhandled attrib type>';
  
  case MapPKCS11AttrTypeToDelphiAttrType(AttribType) of
    adtObjClass:         retval := ObjectClassToString(ValueAsObjClass);
    adtBoolean:          retval := SDUBoolToStr(ValueAsBoolean);
    adtString:           retval := ValueAsString;
    adtBigInteger:       retval := DisplayValue_BigInteger();
    adtByteArray:        retval := SDUPrettyPrintHexStrSimple(ValueAsByteArray);
    adtCertificateType:  retval := CertificateTypeToString(ValueAsCertificateType);
    adtNumber:           retval := inttostr(ValueAsNumber);
    adtKeyType:          retval := KeyTypeToString(ValueAsKeyType);
    adtDate:
      begin
      retval := '<no date>';
      if (ValueAsDate <> 0) then
        begin
        retval := DateToStr(ValueAsDate);
        end;
      end;
    adtMechanismType:    retval := inttostr(ValueAsMechanismType);
    adtMechanismTypeSet: retval := '<Mechanism set>';
    adtAttributeSet:     retval := '<Attribute set>';
    adtHWFeatureType:    retval := HWFeatureTypeToString(ValueAsHWFeatureType);
  end;

  Result := retval;
end;

function TPKCS11Attribute.GetAttribTypeStr(): string;
begin
  Result := AttributeTypeToString(FAttribType);
end;

function TPKCS11Attribute.AttributeTypeToString(attrType: CK_ATTRIBUTE_TYPE): string;
var
  retval: string;
begin
  retval:= RS_UNKNOWN;

  case attrType of
    CKA_CLASS:                      retval := 'CKA_CLASS';
    CKA_TOKEN:                      retval := 'CKA_TOKEN';
    CKA_PRIVATE:                    retval := 'CKA_PRIVATE';
    CKA_LABEL:                      retval := 'CKA_LABEL';
    CKA_APPLICATION:                retval := 'CKA_APPLICATION';
    CKA_VALUE:                      retval := 'CKA_VALUE';
    CKA_OBJECT_ID:                  retval := 'CKA_OBJECT_ID';
    CKA_CERTIFICATE_TYPE:           retval := 'CKA_CERTIFICATE_TYPE';
    CKA_ISSUER:                     retval := 'CKA_ISSUER';
    CKA_SERIAL_NUMBER:              retval := 'CKA_SERIAL_NUMBER';
    CKA_AC_ISSUER:                  retval := 'CKA_AC_ISSUER';
    CKA_OWNER:                      retval := 'CKA_OWNER';
    CKA_ATTR_TYPES:                 retval := 'CKA_ATTR_TYPES';
    CKA_TRUSTED:                    retval := 'CKA_TRUSTED';
    CKA_CERTIFICATE_CATEGORY:       retval := 'CKA_CERTIFICATE_CATEGORY';
    CKA_JAVA_MIDP_SECURITY_DOMAIN:  retval := 'CKA_JAVA_MIDP_SECURITY_DOMAIN';
    CKA_URL:                        retval := 'CKA_URL';
    CKA_HASH_OF_SUBJECT_PUBLIC_KEY: retval := 'CKA_HASH_OF_SUBJECT_PUBLIC_KEY';
    CKA_HASH_OF_ISSUER_PUBLIC_KEY:  retval := 'CKA_HASH_OF_ISSUER_PUBLIC_KEY';
    CKA_CHECK_VALUE:                retval := 'CKA_CHECK_VALUE';
    CKA_KEY_TYPE:                   retval := 'CKA_KEY_TYPE';
    CKA_SUBJECT:                    retval := 'CKA_SUBJECT';
    CKA_ID:                         retval := 'CKA_ID';
    CKA_SENSITIVE:                  retval := 'CKA_SENSITIVE';
    CKA_ENCRYPT:                    retval := 'CKA_ENCRYPT';
    CKA_DECRYPT:                    retval := 'CKA_DECRYPT';
    CKA_WRAP:                       retval := 'CKA_WRAP';
    CKA_UNWRAP:                     retval := 'CKA_UNWRAP';
    CKA_SIGN:                       retval := 'CKA_SIGN';
    CKA_SIGN_RECOVER:               retval := 'CKA_SIGN_RECOVER';
    CKA_VERIFY:                     retval := 'CKA_VERIFY';
    CKA_VERIFY_RECOVER:             retval := 'CKA_VERIFY_RECOVER';
    CKA_DERIVE:                     retval := 'CKA_DERIVE';
    CKA_START_DATE:                 retval := 'CKA_START_DATE';
    CKA_END_DATE:                   retval := 'CKA_END_DATE';
    CKA_MODULUS:                    retval := 'CKA_MODULUS';
    CKA_MODULUS_BITS:               retval := 'CKA_MODULUS_BITS';
    CKA_PUBLIC_EXPONENT:            retval := 'CKA_PUBLIC_EXPONENT';
    CKA_PRIVATE_EXPONENT:           retval := 'CKA_PRIVATE_EXPONENT';
    CKA_PRIME_1:                    retval := 'CKA_PRIME_1';
    CKA_PRIME_2:                    retval := 'CKA_PRIME_2';
    CKA_EXPONENT_1:                 retval := 'CKA_EXPONENT_1';
    CKA_EXPONENT_2:                 retval := 'CKA_EXPONENT_2';
    CKA_COEFFICIENT:                retval := 'CKA_COEFFICIENT';
    CKA_PRIME:                      retval := 'CKA_PRIME';
    CKA_SUBPRIME:                   retval := 'CKA_SUBPRIME';
    CKA_BASE:                       retval := 'CKA_BASE';
    CKA_PRIME_BITS:                 retval := 'CKA_PRIME_BITS';
    CKA_SUBPRIME_BITS:              retval := 'CKA_SUBPRIME_BITS';
    //CKA_SUB_PRIME_BITS:             retval := 'CKA_SUB_PRIME_BITS';
    CKA_VALUE_BITS:                 retval := 'CKA_VALUE_BITS';
    CKA_VALUE_LEN:                  retval := 'CKA_VALUE_LEN';
    CKA_EXTRACTABLE:                retval := 'CKA_EXTRACTABLE';
    CKA_LOCAL:                      retval := 'CKA_LOCAL';
    CKA_NEVER_EXTRACTABLE:          retval := 'CKA_NEVER_EXTRACTABLE';
    CKA_ALWAYS_SENSITIVE:           retval := 'CKA_ALWAYS_SENSITIVE';
    CKA_KEY_GEN_MECHANISM:          retval := 'CKA_KEY_GEN_MECHANISM';
    CKA_MODIFIABLE:                 retval := 'CKA_MODIFIABLE';
    CKA_ECDSA_PARAMS:               retval := 'CKA_ECDSA_PARAMS';
    //CKA_EC_PARAMS:                  retval := 'CKA_EC_PARAMS';
    CKA_EC_POINT:                   retval := 'CKA_EC_POINT';
    CKA_SECONDARY_AUTH:             retval := 'CKA_SECONDARY_AUTH';
    CKA_AUTH_PIN_FLAGS:             retval := 'CKA_AUTH_PIN_FLAGS';
    CKA_ALWAYS_AUTHENTICATE:        retval := 'CKA_ALWAYS_AUTHENTICATE';
    CKA_WRAP_WITH_TRUSTED:          retval := 'CKA_WRAP_WITH_TRUSTED';
    CKA_WRAP_TEMPLATE:              retval := 'CKA_WRAP_TEMPLATE';
    CKA_UNWRAP_TEMPLATE:            retval := 'CKA_UNWRAP_TEMPLATE';
    CKA_OTP_FORMAT:                 retval := 'CKA_OTP_FORMAT';
    CKA_OTP_LENGTH:                 retval := 'CKA_OTP_LENGTH';
    CKA_OTP_TIME_INTERVAL:          retval := 'CKA_OTP_TIME_INTERVAL';
    CKA_OTP_USER_FRIENDLY_MODE:     retval := 'CKA_OTP_USER_FRIENDLY_MODE';
    CKA_OTP_CHALLENGE_REQUIREMENT:  retval := 'CKA_OTP_CHALLENGE_REQUIREMENT';
    CKA_OTP_TIME_REQUIREMENT:       retval := 'CKA_OTP_TIME_REQUIREMENT';
    CKA_OTP_COUNTER_REQUIREMENT:    retval := 'CKA_OTP_COUNTER_REQUIREMENT';
    CKA_OTP_PIN_REQUIREMENT:        retval := 'CKA_OTP_PIN_REQUIREMENT';
    CKA_OTP_COUNTER:                retval := 'CKA_OTP_COUNTER';
    CKA_OTP_TIME:                   retval := 'CKA_OTP_TIME';
    CKA_OTP_USER_IDENTIFIER:        retval := 'CKA_OTP_USER_IDENTIFIER';
    CKA_OTP_SERVICE_IDENTIFIER:     retval := 'CKA_OTP_SERVICE_IDENTIFIER';
    CKA_OTP_SERVICE_LOGO:           retval := 'CKA_OTP_SERVICE_LOGO';
    CKA_OTP_SERVICE_LOGO_TYPE:      retval := 'CKA_OTP_SERVICE_LOGO_TYPE';
    CKA_HW_FEATURE_TYPE:            retval := 'CKA_HW_FEATURE_TYPE';
    CKA_RESET_ON_INIT:              retval := 'CKA_RESET_ON_INIT';
    CKA_HAS_RESET:                  retval := 'CKA_HAS_RESET';
    CKA_PIXEL_X:                    retval := 'CKA_PIXEL_X';
    CKA_PIXEL_Y:                    retval := 'CKA_PIXEL_Y';
    CKA_RESOLUTION:                 retval := 'CKA_RESOLUTION';
    CKA_CHAR_ROWS:                  retval := 'CKA_CHAR_ROWS';
    CKA_CHAR_COLUMNS:               retval := 'CKA_CHAR_COLUMNS';
    CKA_COLOR:                      retval := 'CKA_COLOR';
    CKA_BITS_PER_PIXEL:             retval := 'CKA_BITS_PER_PIXEL';
    CKA_CHAR_SETS:                  retval := 'CKA_CHAR_SETS';
    CKA_ENCODING_METHODS:           retval := 'CKA_ENCODING_METHODS';
    CKA_MIME_TYPES:                 retval := 'CKA_MIME_TYPES';
    CKA_MECHANISM_TYPE:             retval := 'CKA_MECHANISM_TYPE';
    CKA_REQUIRED_CMS_ATTRIBUTES:    retval := 'CKA_REQUIRED_CMS_ATTRIBUTES';
    CKA_DEFAULT_CMS_ATTRIBUTES:     retval := 'CKA_DEFAULT_CMS_ATTRIBUTES';
    CKA_SUPPORTED_CMS_ATTRIBUTES:   retval := 'CKA_SUPPORTED_CMS_ATTRIBUTES';
    CKA_ALLOWED_MECHANISMS:         retval := 'CKA_ALLOWED_MECHANISMS';
    CKA_VENDOR_DEFINED:             retval := 'CKA_VENDOR_DEFINED';
  end;

  Result := retval;
end;

function TPKCS11Attribute.GetLength(): cardinal;
begin
  Result := FLength;
end;

function TPKCS11Attribute.CopyBinary(ptr: PByte; len: integer): string;
var
  retval: string;
  i: integer;
begin
  retval := '';
  for i:=0 to (len-1) do
    begin
    retval := retval + char(PByte(ptr)^);
    inc(ptr);
    end;

  Result := retval;
end;


procedure TPKCS11Attribute.SetAttrStruct(struct: CK_ATTRIBUTE);
begin
  FAttribType := struct.attrType;
  FLength := struct.ulValueLen;
  FRawData := CopyBinary(struct.pValue, struct.ulValueLen);
end;

function TPKCS11Attribute.ObjectClassToString(objClass: CK_OBJECT_CLASS): string;
var
  retval: string;
begin
  retval := RS_UNKNOWN;

  case objClass of
    CKO_DATA:              retval := 'CKO_DATA';
    CKO_CERTIFICATE:       retval := 'CKO_CERTIFICATE';
    CKO_PUBLIC_KEY:        retval := 'CKO_PUBLIC_KEY';
    CKO_PRIVATE_KEY:       retval := 'CKO_PRIVATE_KEY';
    CKO_SECRET_KEY:        retval := 'CKO_SECRET_KEY';
    CKO_HW_FEATURE:        retval := 'CKO_HW_FEATURE';
    CKO_DOMAIN_PARAMETERS: retval := 'CKO_DOMAIN_PARAMETERS';
    CKO_MECHANISM:         retval := 'CKO_MECHANISM';
    CKO_OTP_KEY:           retval := 'CKO_OTP_KEY';
    CKO_VENDOR_DEFINED:    retval := 'CKO_VENDOR_DEFINED';
  end;

  Result := retval;
end;

function TPKCS11Attribute.HWFeatureTypeToString(HWFeatureType: CK_HW_FEATURE_TYPE): string;
var
  retval: string;
begin
  retval := RS_UNKNOWN;

  case HWFeatureType of
    CKH_MONOTONIC_COUNTER: retval := 'CKH_MONOTONIC_COUNTER';
    CKH_CLOCK:             retval := 'CKH_CLOCK';
    CKH_USER_INTERFACE:    retval := 'CKH_USER_INTERFACE';
    CKH_VENDOR_DEFINED:    retval := 'CKH_VENDOR_DEFINED';
  end;
  
  Result := retval;
end;


function TPKCS11Attribute.KeyTypeToString(keyType: CK_KEY_TYPE): string;
var
  retval: string;
begin
  retval := RS_UNKNOWN;

  case keyType of
    CKK_RSA:            retval := 'CKK_RSA';
    CKK_DSA:            retval := 'CKK_DSA';
    CKK_DH:             retval := 'CKK_DH';
    CKK_ECDSA:          retval := 'CKK_ECDSA';
    //CKK_EC:             retval := 'CKK_EC';
    CKK_X9_42_DH:       retval := 'CKK_X9_42_DH';
    CKK_KEA:            retval := 'CKK_KEA';
    CKK_GENERIC_SECRET: retval := 'CKK_GENERIC_SECRET';
    CKK_RC2:            retval := 'CKK_RC2';
    CKK_RC4:            retval := 'CKK_RC4';
    CKK_DES:            retval := 'CKK_DES';
    CKK_DES2:           retval := 'CKK_DES2';
    CKK_DES3:           retval := 'CKK_DES3';
    CKK_CAST:           retval := 'CKK_CAST';
    CKK_CAST3:          retval := 'CKK_CAST3';
    CKK_CAST5:          retval := 'CKK_CAST5';
    //CKK_CAST128:        retval := 'CKK_CAST128';
    CKK_RC5:            retval := 'CKK_RC5';
    CKK_IDEA:           retval := 'CKK_IDEA';
    CKK_SKIPJACK:       retval := 'CKK_SKIPJACK';
    CKK_BATON:          retval := 'CKK_BATON';
    CKK_JUNIPER:        retval := 'CKK_JUNIPER';
    CKK_CDMF:           retval := 'CKK_CDMF';
    CKK_AES:            retval := 'CKK_AES';
    CKK_BLOWFISH:       retval := 'CKK_BLOWFISH';
    CKK_TWOFISH:        retval := 'CKK_TWOFISH';
    CKK_SECURID:        retval := 'CKK_SECURID';
    CKK_HOTP:           retval := 'CKK_HOTP';
    CKK_ACTI:           retval := 'CKK_ACTI';
    CKK_CAMELLIA:       retval := 'CKK_CAMELLIA';
    CKK_ARIA:           retval := 'CKK_ARIA';
    CKK_VENDOR_DEFINED: retval := 'CKK_VENDOR_DEFINED';
  end;

  Result := retval;
end;

function TPKCS11Attribute.CertificateTypeToString(certType: CK_CERTIFICATE_TYPE): string;
var
  retval: string;
begin
  retval := RS_UNKNOWN;

  case certType of
    CKC_X_509:           retval := 'CKC_X_509';
    CKC_X_509_ATTR_CERT: retval := 'CKC_X_509_ATTR_CERT';
    CKC_WTLS:            retval := 'CKC_WTLS';
    CKC_VENDOR_DEFINED:  retval := 'CKC_VENDOR_DEFINED';
  end;

  Result := retval;
end;

function TPKCS11Attribute.GetValueAsObjClass(): CK_OBJECT_CLASS;
begin
  Result := (PCK_OBJECT_CLASS(PChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsBoolean(): boolean;
begin
  Result := (((PCK_BBOOL(PChar(FRawData)))^) = CK_TRUE)
end;

function TPKCS11Attribute.GetValueAsString(): string;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsBigInteger(): string;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsByteArray(): string;
begin
  Result := FRawData;
end;

function TPKCS11Attribute.GetValueAsCertificateType(): CK_CERTIFICATE_TYPE;
begin
  Result := (PCK_CERTIFICATE_TYPE(PChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsNumber(): CK_ULONG;
var
  retval: CK_ULONG;
begin
  if (length(FRawData) < sizeof(retval)) then
    begin
    retval := 0;
    end
  else
    begin
    retval := (PCK_ULONG(FRawData))^;
    end;

  Result := retval;
end;

function TPKCS11Attribute.GetValueAsKeyType(): CK_KEY_TYPE;
begin
  Result := (PCK_KEY_TYPE(PChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsDate(): TDate;
var
  tmpCK_DATE: CK_DATE;
  retval: TDate;
begin
  retval := 0;
  if (length(FRawData) >= sizeof(tmpCK_DATE)) then
    begin
    tmpCK_DATE := (PCK_DATE(PChar(FRawData)))^;
    retval := CK_DATEToDate(tmpCK_DATE);
    end;

  Result := retval;
end;

function TPKCS11Attribute.GetValueAsMechanismType(): CK_MECHANISM_TYPE;
begin
  Result := (PCK_MECHANISM_TYPE(PChar(FRawData)))^;
end;

function TPKCS11Attribute.GetValueAsHWFeatureType(): CK_HW_FEATURE_TYPE;
begin
  Result := (PCK_HW_FEATURE_TYPE(PChar(FRawData)))^;
end;


procedure TPKCS11Attribute.SetValueAsObjClass(value: CK_OBJECT_CLASS);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;

procedure TPKCS11Attribute.SetValueAsBoolean(value: boolean);
var
  tmpCK_BBOOL: CK_BBOOL;
begin
  tmpCK_BBOOL := CK_TRUE;
  if not(value) then
    begin
    tmpCK_BBOOL := CK_FALSE;
    end;

  FRawData := CopyBinary(@tmpCK_BBOOL, sizeof(tmpCK_BBOOL))
end;

procedure TPKCS11Attribute.SetValueAsString(value: string);
begin
  FRawData := value;
end;

procedure TPKCS11Attribute.SetValueAsBigInteger(value: string);
begin
  FRawData := value;
end;

procedure TPKCS11Attribute.SetValueAsByteArray(value: string);
begin
  FRawData := value;
end;

procedure TPKCS11Attribute.SetValueAsCertificateType(value: CK_CERTIFICATE_TYPE);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;

procedure TPKCS11Attribute.SetValueAsNumber(value: CK_ULONG);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;

procedure TPKCS11Attribute.SetValueAsKeyType(value: CK_KEY_TYPE);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;

procedure TPKCS11Attribute.SetValueAsDate(value: TDate);
var
  tmpCK_DATE: CK_DATE;
begin
  tmpCK_DATE := DateToCK_DATE(value);
  FRawData := CopyBinary(@tmpCK_DATE, sizeof(tmpCK_DATE));
end;

procedure TPKCS11Attribute.SetValueAsMechanismType(value: CK_MECHANISM_TYPE);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;

procedure TPKCS11Attribute.SetValueAsHWFeatureType(value: CK_HW_FEATURE_TYPE);
begin
  FRawData := CopyBinary(@value, sizeof(value))
end;


constructor TPKCS11AttributeTemplate.Create();
begin
  inherited;
  SetLength(FAttributes, 0);
end;

destructor TPKCS11AttributeTemplate.Destroy();
var
  i: integer;
begin
  for i:=low(FAttributes) to high(FAttributes) do
    begin
    FAttributes[i].Free();
    end;

  inherited;
end;

function TPKCS11AttributeTemplate.GetCount(): integer;
begin
  Result := length(FAttributes);
end;

function TPKCS11AttributeTemplate.GetAttribute(idx: integer): TPKCS11Attribute;
begin
  Assert(
         (idx >= 0) and
         (idx < Count),
         'Index out of bounds'
        );

  Result := FAttributes[idx];
end;

procedure TPKCS11AttributeTemplate.SetAttribute(idx: integer; attr: TPKCS11Attribute);
begin
  Assert(
         (idx >= 0) and
         (idx < Count),
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


END.

