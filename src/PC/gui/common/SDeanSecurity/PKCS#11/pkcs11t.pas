unit pkcs11t;

// Written: 19/02/08
//
// This file is derived from the RSA Security Inc. PKCS #11 Cryptographic Token
// Interface (Cryptoki), and is a straight port of the RSA source (pkcs11t.h)


interface

// xxx - lplp - cdecl/safecall for the callbacks? assumed cdecl here
// xxx - lplp - cdecl/safecall for the callbacks? assumed cdecl here
// xxx - lplp - cdecl/safecall for the callbacks? assumed cdecl here
// xxx - lplp - cdecl/safecall for the callbacks? assumed cdecl here
// xxx - lplp - cdecl/safecall for the callbacks? assumed cdecl here

const CRYPTOKI_VERSION_MAJOR     =  2;
const CRYPTOKI_VERSION_MINOR     = 20;
const CRYPTOKI_VERSION_AMENDMENT =  3;

const CK_TRUE  = 1;
const CK_FALSE = 0;


// an unsigned 8-bit value
type CK_BYTE = byte;

// an unsigned 8-bit character
type CK_CHAR = CK_BYTE;

// an 8-bit UTF-8 character
type CK_UTF8CHAR = CK_BYTE;

// a BYTE-sized Boolean flag
type CK_BBOOL = CK_BYTE;

// an unsigned value, at least 32 bits long
type CK_ULONG = longword;

// a signed value, the same size as a CK_ULONG
// CK_LONG is new for v2.0
type CK_LONG = longint;

// at least 32 bits; each bit is a Boolean flag
type CK_FLAGS = CK_ULONG;


// some special values for certain CK_ULONG variables
const CK_UNAVAILABLE_INFORMATION = $ffffffff; // (~0UL);
const CK_EFFECTIVELY_INFINITE    = 0;


type CK_BYTE_PTR     = ^CK_BYTE;
type CK_CHAR_PTR     = ^CK_CHAR;
type CK_UTF8CHAR_PTR = ^CK_UTF8CHAR;
type CK_ULONG_PTR    = ^CK_ULONG;
type CK_VOID_PTR     = Pointer; // ^void;

// Pointer to a CK_VOID_PTR-- i.e., pointer to pointer to void
type CK_VOID_PTR_PTR = ^CK_VOID_PTR;


// The following value is always invalid if used as a session
// handle or object handle
const CK_INVALID_HANDLE = 0;


type CK_VERSION = packed record
  major: CK_BYTE;        // integer portion of version number
  minor: CK_BYTE;        // 1/100ths portion of version number
end;

type CK_VERSION_PTR = ^CK_VERSION; 


type CK_INFO = packed record 
  // manufacturerID and libraryDecription have been changed from
  // CK_CHAR to CK_UTF8CHAR for v2.10 
  cryptokiVersion: CK_VERSION;        // Cryptoki interface ver
  manufacturerID: array [0..31] of CK_UTF8CHAR;  // blank padded
  flags: CK_FLAGS;                    // must be zero

  // libraryDescription and libraryVersion are new for v2.0
  libraryDescription: array [0..31] of CK_UTF8CHAR;  // blank padded
  libraryVersion: CK_VERSION;             // version of library
end;

type CK_INFO_PTR = ^CK_INFO; 


// CK_NOTIFICATION enumerates the types of notifications that
// Cryptoki provides to an application
// CK_NOTIFICATION has been changed from an enum to a CK_ULONG
// for v2.0
type CK_NOTIFICATION = CK_ULONG;
const CKN_SURRENDER       = 0;

// The following notification is new for PKCS #11 v2.20 amendment 3
const CKN_OTP_CHANGED     = 1;


type CK_SLOT_ID = CK_ULONG;

type CK_SLOT_ID_PTR = ^CK_SLOT_ID;


// CK_SLOT_INFO provides information about a slot
type CK_SLOT_INFO = packed record 
  // slotDescription and manufacturerID have been changed from
  // CK_CHAR to CK_UTF8CHAR for v2.10
  slotDescription: array [0..63] of CK_UTF8CHAR;  // blank padded
  manufacturerID: array [0..31] of CK_UTF8CHAR;   // blank padded
  flags: CK_FLAGS;

  // hardwareVersion and firmwareVersion are new for v2.0
  hardwareVersion: CK_VERSION;     // version of hardware
  firmwareVersion: CK_VERSION;     // version of firmware
end;

// flags: bit flags that provide capabilities of the slot
//      Bit Flag                Mask        Meaning
const   CKF_TOKEN_PRESENT     = $00000001;  // a token is there
const   CKF_REMOVABLE_DEVICE  = $00000002;  // removable devices
const   CKF_HW_SLOT           = $00000004;  // hardware slot

type CK_SLOT_INFO_PTR = ^CK_SLOT_INFO; 


// CK_TOKEN_INFO provides information about a token 
type CK_TOKEN_INFO = packed record 
  // label, manufacturerID, and model have been changed from
  // CK_CHAR to CK_UTF8CHAR for v2.10
  // "X"label because label is a Delphi reserved word
  Xlabel: array [0..31] of CK_UTF8CHAR;           // blank padded
  manufacturerID: array [0..31] of CK_UTF8CHAR;  // blank padded
  model: array [0..15] of CK_UTF8CHAR;           // blank padded
  serialNumber: array [0..15] of CK_CHAR;    // blank padded
  flags: CK_FLAGS;                    // see below 

  // ulMaxSessionCount, ulSessionCount, ulMaxRwSessionCount,
  // ulRwSessionCount, ulMaxPinLen, and ulMinPinLen have all been
  // changed from CK_USHORT to CK_ULONG for v2.0
  ulMaxSessionCount: CK_ULONG;          // max open sessions
  ulSessionCount: CK_ULONG;             // sess. now open
  ulMaxRwSessionCount: CK_ULONG;        // max R/W sessions
  ulRwSessionCount: CK_ULONG;           // R/W sess. now open
  ulMaxPinLen: CK_ULONG;                // in bytes
  ulMinPinLen: CK_ULONG;                // in bytes
  ulTotalPublicMemory: CK_ULONG;        // in bytes
  ulFreePublicMemory: CK_ULONG;         // in bytes
  ulTotalPrivateMemory: CK_ULONG;       // in bytes
  ulFreePrivateMemory: CK_ULONG;        // in bytes

  // hardwareVersion, firmwareVersion, and time are new for
  // v2.0 
  hardwareVersion: CK_VERSION;          // version of hardware
  firmwareVersion: CK_VERSION;          // version of firmware
  utcTime: array [0..15] of CK_CHAR;           // time
end;

// The flags parameter is defined as follows:
//      Bit Flag                      Mask        Meaning
const CKF_RNG                     = $00000001;  // has random #
                                                // generator
const CKF_WRITE_PROTECTED         = $00000002;  // token is
                                                // write-
                                                // protected
const CKF_LOGIN_REQUIRED          = $00000004;  // user must
                                                // login
const CKF_USER_PIN_INITIALIZED    = $00000008;  // normal user's
                                                // PIN is set

// CKF_RESTORE_KEY_NOT_NEEDED is new for v2.0.  If it is set,
// that means that *every* time the state of cryptographic
// operations of a session is successfully saved, all keys
// needed to continue those operations are stored in the state
const CKF_RESTORE_KEY_NOT_NEEDED  = $00000020;

// CKF_CLOCK_ON_TOKEN is new for v2.0.  If it is set, that means
// that the token has some sort of clock.  The time on that
// clock is returned in the token info structure
const CKF_CLOCK_ON_TOKEN          = $00000040;

// CKF_PROTECTED_AUTHENTICATION_PATH is new for v2.0.  If it is
// set, that means that there is some way for the user to login
// without sending a PIN through the Cryptoki library itself
const CKF_PROTECTED_AUTHENTICATION_PATH = $00000100;

// CKF_DUAL_CRYPTO_OPERATIONS is new for v2.0.  If it is true,
// that means that a single session with the token can perform
// dual simultaneous cryptographic operations (digest and
// encrypt; decrypt and digest; sign and encrypt; and decrypt
// and sign)
const CKF_DUAL_CRYPTO_OPERATIONS  = $00000200;

// CKF_TOKEN_INITIALIZED if new for v2.10. If it is true, the
// token has been initialized using C_InitializeToken or an
// equivalent mechanism outside the scope of PKCS #11.
// Calling C_InitializeToken when this flag is set will cause
// the token to be reinitialized.
const CKF_TOKEN_INITIALIZED       = $00000400;

// CKF_SECONDARY_AUTHENTICATION if new for v2.10. If it is
// true, the token supports secondary authentication for
// private key objects. This flag is deprecated in v2.11 and
// onwards.
const CKF_SECONDARY_AUTHENTICATION  = $00000800;

// CKF_USER_PIN_COUNT_LOW if new for v2.10. If it is true, an
// incorrect user login PIN has been entered at least once
// since the last successful authentication.
const CKF_USER_PIN_COUNT_LOW       = $00010000;

// CKF_USER_PIN_FINAL_TRY if new for v2.10. If it is true,
// supplying an incorrect user PIN will it to become locked.
const CKF_USER_PIN_FINAL_TRY       = $00020000;

// CKF_USER_PIN_LOCKED if new for v2.10. If it is true, the
// user PIN has been locked. User login to the token is not
// possible.
const CKF_USER_PIN_LOCKED          = $00040000;

// CKF_USER_PIN_TO_BE_CHANGED if new for v2.10. If it is true,
// the user PIN value is the default value set by token
// initialization or manufacturing, or the PIN has been
// expired by the card.
const CKF_USER_PIN_TO_BE_CHANGED   = $00080000;

// CKF_SO_PIN_COUNT_LOW if new for v2.10. If it is true, an
// incorrect SO login PIN has been entered at least once since
// the last successful authentication.
const CKF_SO_PIN_COUNT_LOW         = $00100000;

// CKF_SO_PIN_FINAL_TRY if new for v2.10. If it is true,
// supplying an incorrect SO PIN will it to become locked.
const CKF_SO_PIN_FINAL_TRY         = $00200000;

// CKF_SO_PIN_LOCKED if new for v2.10. If it is true, the SO
// PIN has been locked. SO login to the token is not possible.
const CKF_SO_PIN_LOCKED            = $00400000;

// CKF_SO_PIN_TO_BE_CHANGED if new for v2.10. If it is true,
// the SO PIN value is the default value set by token
// initialization or manufacturing, or the PIN has been
// expired by the card.
const CKF_SO_PIN_TO_BE_CHANGED     = $00800000;

type CK_TOKEN_INFO_PTR = ^CK_TOKEN_INFO; 


// CK_SESSION_HANDLE is a Cryptoki-assigned value that
// identifies a session
type CK_SESSION_HANDLE = CK_ULONG;          

type CK_SESSION_HANDLE_PTR = ^CK_SESSION_HANDLE;


// CK_USER_TYPE enumerates the types of Cryptoki users
// CK_USER_TYPE has been changed from an enum to a CK_ULONG for
// v2.0
type CK_USER_TYPE = CK_ULONG;          
// Security Officer 
const CKU_SO    = 0;
// Normal user
const CKU_USER  = 1;
// Context specific (added in v2.20)
const CKU_CONTEXT_SPECIFIC   = 2;

// CK_STATE enumerates the session states
// CK_STATE has been changed from an enum to a CK_ULONG for
// v2.0
type CK_STATE = CK_ULONG;          
const CKS_RO_PUBLIC_SESSION  = 0;
const CKS_RO_USER_FUNCTIONS  = 1;
const CKS_RW_PUBLIC_SESSION  = 2;
const CKS_RW_USER_FUNCTIONS  = 3;
const CKS_RW_SO_FUNCTIONS    = 4;


// CK_SESSION_INFO provides information about a session
type CK_SESSION_INFO = packed record 
  slotID: CK_SLOT_ID;
  state: CK_STATE;
  flags: CK_FLAGS;               // see below

  // ulDeviceError was changed from CK_USHORT to CK_ULONG for
  // v2.0
  ulDeviceError: CK_ULONG;       // device-dependent error code
end;

// The flags are defined in the following table:
//      Bit Flag                Mask        Meaning
const CKF_RW_SESSION          = $00000002;  // session is r/w
const CKF_SERIAL_SESSION      = $00000004;  // no parallel

type CK_SESSION_INFO_PTR = ^CK_SESSION_INFO; 


// CK_OBJECT_HANDLE is a token-specific identifier for an
// object
type CK_OBJECT_HANDLE = CK_ULONG;          

type CK_OBJECT_HANDLE_PTR = ^CK_OBJECT_HANDLE; 


// CK_OBJECT_CLASS is a value that identifies the classes (or
// types) of objects that Cryptoki recognizes.  It is defined
// as follows:
// CK_OBJECT_CLASS was changed from CK_USHORT to CK_ULONG for
// v2.0
type CK_OBJECT_CLASS = CK_ULONG;          

// The following classes of objects are defined: 
// CKO_HW_FEATURE is new for v2.10 
// CKO_DOMAIN_PARAMETERS is new for v2.11 
// CKO_MECHANISM is new for v2.20 
const CKO_DATA              = $00000000;
const CKO_CERTIFICATE       = $00000001;
const CKO_PUBLIC_KEY        = $00000002;
const CKO_PRIVATE_KEY       = $00000003;
const CKO_SECRET_KEY        = $00000004;
const CKO_HW_FEATURE        = $00000005;
const CKO_DOMAIN_PARAMETERS = $00000006;
const CKO_MECHANISM         = $00000007;

// CKO_OTP_KEY is new for PKCS #11 v2.20 amendment 1 
const CKO_OTP_KEY           = $00000008;

const CKO_VENDOR_DEFINED    = $80000000;

type CK_OBJECT_CLASS_PTR = ^CK_OBJECT_CLASS; 

// CK_HW_FEATURE_TYPE is new for v2.10. CK_HW_FEATURE_TYPE is a
// value that identifies the hardware feature type of an object
// with CK_OBJECT_CLASS equal to CKO_HW_FEATURE.
type CK_HW_FEATURE_TYPE = CK_ULONG;          

// The following hardware feature types are defined 
// CKH_USER_INTERFACE is new for v2.20 
const CKH_MONOTONIC_COUNTER  = $00000001;
const CKH_CLOCK           = $00000002;
const CKH_USER_INTERFACE  = $00000003;
const CKH_VENDOR_DEFINED  = $80000000;

// CK_KEY_TYPE is a value that identifies a key type 
// CK_KEY_TYPE was changed from CK_USHORT to CK_ULONG for v2.0 
type CK_KEY_TYPE = CK_ULONG;          

// the following key types are defined: 
const CKK_RSA             = $00000000;
const CKK_DSA             = $00000001;
const CKK_DH              = $00000002;

// CKK_ECDSA and CKK_KEA are new for v2.0 
// CKK_ECDSA is deprecated in v2.11, CKK_EC is preferred. 
const CKK_ECDSA           = $00000003;
const CKK_EC              = $00000003;
const CKK_X9_42_DH        = $00000004;
const CKK_KEA             = $00000005;

const CKK_GENERIC_SECRET  = $00000010;
const CKK_RC2             = $00000011;
const CKK_RC4             = $00000012;
const CKK_DES             = $00000013;
const CKK_DES2            = $00000014;
const CKK_DES3            = $00000015;

// all these key types are new for v2.0 
const CKK_CAST            = $00000016;
const CKK_CAST3           = $00000017;
// CKK_CAST5 is deprecated in v2.11, CKK_CAST128 is preferred. 
const CKK_CAST5           = $00000018;
const CKK_CAST128         = $00000018;
const CKK_RC5             = $00000019;
const CKK_IDEA            = $0000001A;
const CKK_SKIPJACK        = $0000001B;
const CKK_BATON           = $0000001C;
const CKK_JUNIPER         = $0000001D;
const CKK_CDMF            = $0000001E;
const CKK_AES             = $0000001F;

// BlowFish and TwoFish are new for v2.20 
const CKK_BLOWFISH        = $00000020;
const CKK_TWOFISH         = $00000021;

// SecurID, HOTP, and ACTI are new for PKCS #11 v2.20 amendment 1 
const CKK_SECURID         = $00000022;
const CKK_HOTP            = $00000023;
const CKK_ACTI            = $00000024;

// Camellia is new for PKCS #11 v2.20 amendment 3 
const CKK_CAMELLIA                   = $00000025;
// ARIA is new for PKCS #11 v2.20 amendment 3 
const CKK_ARIA                       = $00000026;


const CKK_VENDOR_DEFINED  = $80000000;


// CK_CERTIFICATE_TYPE is a value that identifies a certificate
// type
// CK_CERTIFICATE_TYPE was changed from CK_USHORT to CK_ULONG
// for v2.0
type CK_CERTIFICATE_TYPE = CK_ULONG;          

// The following certificate types are defined:
// CKC_X_509_ATTR_CERT is new for v2.10
// CKC_WTLS is new for v2.20
const CKC_X_509           = $00000000;
const CKC_X_509_ATTR_CERT = $00000001;
const CKC_WTLS            = $00000002;
const CKC_VENDOR_DEFINED  = $80000000;


// CK_ATTRIBUTE_TYPE is a value that identifies an attribute
// type
// CK_ATTRIBUTE_TYPE was changed from CK_USHORT to CK_ULONG for
// v2.0
type CK_ATTRIBUTE_TYPE = CK_ULONG;          

// The CKF_ARRAY_ATTRIBUTE flag identifies an attribute which
// consists of an array of values.
const CKF_ARRAY_ATTRIBUTE    = $40000000;

// The following OTP-related defines are new for PKCS #11 v2.20 amendment 1
// and relates to the CKA_OTP_FORMAT attribute
const CK_OTP_FORMAT_DECIMAL      = 0;
const CK_OTP_FORMAT_HEXADECIMAL  = 1;
const CK_OTP_FORMAT_ALPHANUMERIC = 2;
const CK_OTP_FORMAT_BINARY       = 3;

// The following OTP-related defines are new for PKCS #11 v2.20 amendment 1
// and relates to the CKA_OTP_..._REQUIREMENT attributes
const CK_OTP_PARAM_IGNORED       = 0;
const CK_OTP_PARAM_OPTIONAL      = 1;
const CK_OTP_PARAM_MANDATORY     = 2;

// The following attribute types are defined:
const CKA_CLASS              = $00000000;
const CKA_TOKEN              = $00000001;
const CKA_PRIVATE            = $00000002;
const CKA_LABEL              = $00000003;
const CKA_APPLICATION        = $00000010;
const CKA_VALUE              = $00000011;

// CKA_OBJECT_ID is new for v2.10
const CKA_OBJECT_ID          = $00000012;

const CKA_CERTIFICATE_TYPE   = $00000080;
const CKA_ISSUER             = $00000081;
const CKA_SERIAL_NUMBER      = $00000082;

// CKA_AC_ISSUER, CKA_OWNER, and CKA_ATTR_TYPES are new
// for v2.10
const CKA_AC_ISSUER          = $00000083;
const CKA_OWNER              = $00000084;
const CKA_ATTR_TYPES         = $00000085;

// CKA_TRUSTED is new for v2.11
const CKA_TRUSTED            = $00000086;

// CKA_CERTIFICATE_CATEGORY ...
// CKA_CHECK_VALUE are new for v2.20
const CKA_CERTIFICATE_CATEGORY        = $00000087;
const CKA_JAVA_MIDP_SECURITY_DOMAIN   = $00000088;
const CKA_URL                         = $00000089;
const CKA_HASH_OF_SUBJECT_PUBLIC_KEY  = $0000008A;
const CKA_HASH_OF_ISSUER_PUBLIC_KEY   = $0000008B;
const CKA_CHECK_VALUE                 = $00000090;

const CKA_KEY_TYPE           = $00000100;
const CKA_SUBJECT            = $00000101;
const CKA_ID                 = $00000102;
const CKA_SENSITIVE          = $00000103;
const CKA_ENCRYPT            = $00000104;
const CKA_DECRYPT            = $00000105;
const CKA_WRAP               = $00000106;
const CKA_UNWRAP             = $00000107;
const CKA_SIGN               = $00000108;
const CKA_SIGN_RECOVER       = $00000109;
const CKA_VERIFY             = $0000010A;
const CKA_VERIFY_RECOVER     = $0000010B;
const CKA_DERIVE             = $0000010C;
const CKA_START_DATE         = $00000110;
const CKA_END_DATE           = $00000111;
const CKA_MODULUS            = $00000120;
const CKA_MODULUS_BITS       = $00000121;
const CKA_PUBLIC_EXPONENT    = $00000122;
const CKA_PRIVATE_EXPONENT   = $00000123;
const CKA_PRIME_1            = $00000124;
const CKA_PRIME_2            = $00000125;
const CKA_EXPONENT_1         = $00000126;
const CKA_EXPONENT_2         = $00000127;
const CKA_COEFFICIENT        = $00000128;
const CKA_PRIME              = $00000130;
const CKA_SUBPRIME           = $00000131;
const CKA_BASE               = $00000132;

// CKA_PRIME_BITS and CKA_SUB_PRIME_BITS are new for v2.11 
const CKA_PRIME_BITS         = $00000133;
const CKA_SUBPRIME_BITS      = $00000134;
const CKA_SUB_PRIME_BITS     = CKA_SUBPRIME_BITS;
// (To retain backwards-compatibility) 

const CKA_VALUE_BITS         = $00000160;
const CKA_VALUE_LEN          = $00000161;

// CKA_EXTRACTABLE, CKA_LOCAL, CKA_NEVER_EXTRACTABLE,
// CKA_ALWAYS_SENSITIVE, CKA_MODIFIABLE, CKA_ECDSA_PARAMS,
// and CKA_EC_POINT are new for v2.0
const CKA_EXTRACTABLE        = $00000162;
const CKA_LOCAL              = $00000163;
const CKA_NEVER_EXTRACTABLE  = $00000164;
const CKA_ALWAYS_SENSITIVE   = $00000165;

// CKA_KEY_GEN_MECHANISM is new for v2.11 
const CKA_KEY_GEN_MECHANISM  = $00000166;

const CKA_MODIFIABLE         = $00000170;

// CKA_ECDSA_PARAMS is deprecated in v2.11,
// CKA_EC_PARAMS is preferred.
const CKA_ECDSA_PARAMS       = $00000180;
const CKA_EC_PARAMS          = $00000180;

const CKA_EC_POINT           = $00000181;

// CKA_SECONDARY_AUTH, CKA_AUTH_PIN_FLAGS,
// are new for v2.10. Deprecated in v2.11 and onwards.
const CKA_SECONDARY_AUTH     = $00000200;
const CKA_AUTH_PIN_FLAGS     = $00000201;

// CKA_ALWAYS_AUTHENTICATE ...
// CKA_UNWRAP_TEMPLATE are new for v2.20
const CKA_ALWAYS_AUTHENTICATE  = $00000202;

const CKA_WRAP_WITH_TRUSTED    = $00000210;
const CKA_WRAP_TEMPLATE        = (CKF_ARRAY_ATTRIBUTE or $00000211);
const CKA_UNWRAP_TEMPLATE      = (CKF_ARRAY_ATTRIBUTE or $00000212);

// CKA_OTP... atttributes are new for PKCS #11 v2.20 amendment 3. 
const CKA_OTP_FORMAT                = $00000220;
const CKA_OTP_LENGTH                = $00000221;
const CKA_OTP_TIME_INTERVAL         = $00000222;
const CKA_OTP_USER_FRIENDLY_MODE    = $00000223;
const CKA_OTP_CHALLENGE_REQUIREMENT = $00000224;
const CKA_OTP_TIME_REQUIREMENT      = $00000225;
const CKA_OTP_COUNTER_REQUIREMENT   = $00000226;
const CKA_OTP_PIN_REQUIREMENT       = $00000227;
const CKA_OTP_COUNTER               = $0000022E;
const CKA_OTP_TIME                  = $0000022F;
const CKA_OTP_USER_IDENTIFIER       = $0000022A;
const CKA_OTP_SERVICE_IDENTIFIER    = $0000022B;
const CKA_OTP_SERVICE_LOGO          = $0000022C;
const CKA_OTP_SERVICE_LOGO_TYPE     = $0000022D;


// CKA_HW_FEATURE_TYPE, CKA_RESET_ON_INIT, and CKA_HAS_RESET
// are new for v2.10
const CKA_HW_FEATURE_TYPE    = $00000300;
const CKA_RESET_ON_INIT      = $00000301;
const CKA_HAS_RESET          = $00000302;

// The following attributes are new for v2.20
const CKA_PIXEL_X                     = $00000400;
const CKA_PIXEL_Y                     = $00000401;
const CKA_RESOLUTION                  = $00000402;
const CKA_CHAR_ROWS                   = $00000403;
const CKA_CHAR_COLUMNS                = $00000404;
const CKA_COLOR                       = $00000405;
const CKA_BITS_PER_PIXEL              = $00000406;
const CKA_CHAR_SETS                   = $00000480;
const CKA_ENCODING_METHODS            = $00000481;
const CKA_MIME_TYPES                  = $00000482;
const CKA_MECHANISM_TYPE              = $00000500;
const CKA_REQUIRED_CMS_ATTRIBUTES     = $00000501;
const CKA_DEFAULT_CMS_ATTRIBUTES      = $00000502;
const CKA_SUPPORTED_CMS_ATTRIBUTES    = $00000503;
const CKA_ALLOWED_MECHANISMS          = (CKF_ARRAY_ATTRIBUTE or $00000600);

const CKA_VENDOR_DEFINED     = $80000000;

// CK_ATTRIBUTE is a structure that includes the type, length
// and value of an attribute
type CK_ATTRIBUTE = packed record
  // "attrType" because "type" is a Delphi reserved word
  attrType: CK_ATTRIBUTE_TYPE;
  pValue: CK_VOID_PTR;

  // ulValueLen went from CK_USHORT to CK_ULONG for v2.0 
  ulValueLen: CK_ULONG           // in bytes 
end;

type CK_ATTRIBUTE_PTR = ^CK_ATTRIBUTE; 


// CK_DATE is a structure that defines a date 
type CK_DATE= packed record 
  year: array [0..3] of CK_CHAR;   // the year ("1900" - "9999")
  month: array [0..1] of CK_CHAR;  // the month ("01" - "12")
  day: array [0..1] of CK_CHAR;    // the day   ("01" - "31")
end;


// CK_MECHANISM_TYPE is a value that identifies a mechanism
// type 
// CK_MECHANISM_TYPE was changed from CK_USHORT to CK_ULONG for
// v2.0 
type CK_MECHANISM_TYPE = CK_ULONG;          

// the following mechanism types are defined: 
const CKM_RSA_PKCS_KEY_PAIR_GEN      = $00000000;
const CKM_RSA_PKCS                   = $00000001;
const CKM_RSA_9796                   = $00000002;
const CKM_RSA_X_509                  = $00000003;

// CKM_MD2_RSA_PKCS, CKM_MD5_RSA_PKCS, and CKM_SHA1_RSA_PKCS
// are new for v2.0.  They are mechanisms which hash and sign 
const CKM_MD2_RSA_PKCS               = $00000004;
const CKM_MD5_RSA_PKCS               = $00000005;
const CKM_SHA1_RSA_PKCS              = $00000006;

// CKM_RIPEMD128_RSA_PKCS, CKM_RIPEMD160_RSA_PKCS, and
// CKM_RSA_PKCS_OAEP are new for v2.10 
const CKM_RIPEMD128_RSA_PKCS         = $00000007;
const CKM_RIPEMD160_RSA_PKCS         = $00000008;
const CKM_RSA_PKCS_OAEP              = $00000009;

// CKM_RSA_X9_31_KEY_PAIR_GEN, CKM_RSA_X9_31, CKM_SHA1_RSA_X9_31,
// CKM_RSA_PKCS_PSS, and CKM_SHA1_RSA_PKCS_PSS are new for v2.11 
const CKM_RSA_X9_31_KEY_PAIR_GEN     = $0000000A;
const CKM_RSA_X9_31                  = $0000000B;
const CKM_SHA1_RSA_X9_31             = $0000000C;
const CKM_RSA_PKCS_PSS               = $0000000D;
const CKM_SHA1_RSA_PKCS_PSS          = $0000000E;

const CKM_DSA_KEY_PAIR_GEN           = $00000010;
const CKM_DSA                        = $00000011;
const CKM_DSA_SHA1                   = $00000012;
const CKM_DH_PKCS_KEY_PAIR_GEN       = $00000020;
const CKM_DH_PKCS_DERIVE             = $00000021;

// CKM_X9_42_DH_KEY_PAIR_GEN, CKM_X9_42_DH_DERIVE,
// CKM_X9_42_DH_HYBRID_DERIVE, and CKM_X9_42_MQV_DERIVE are new for
// v2.11 
const CKM_X9_42_DH_KEY_PAIR_GEN      = $00000030;
const CKM_X9_42_DH_DERIVE            = $00000031;
const CKM_X9_42_DH_HYBRID_DERIVE     = $00000032;
const CKM_X9_42_MQV_DERIVE           = $00000033;

// CKM_SHA256/384/512 are new for v2.20 
const CKM_SHA256_RSA_PKCS            = $00000040;
const CKM_SHA384_RSA_PKCS            = $00000041;
const CKM_SHA512_RSA_PKCS            = $00000042;
const CKM_SHA256_RSA_PKCS_PSS        = $00000043;
const CKM_SHA384_RSA_PKCS_PSS        = $00000044;
const CKM_SHA512_RSA_PKCS_PSS        = $00000045;

// SHA-224 RSA mechanisms are new for PKCS #11 v2.20 amendment 3 
const CKM_SHA224_RSA_PKCS            = $00000046;
const CKM_SHA224_RSA_PKCS_PSS        = $00000047;

const CKM_RC2_KEY_GEN                = $00000100;
const CKM_RC2_ECB                    = $00000101;
const CKM_RC2_CBC                    = $00000102;
const CKM_RC2_MAC                    = $00000103;

// CKM_RC2_MAC_GENERAL and CKM_RC2_CBC_PAD are new for v2.0 
const CKM_RC2_MAC_GENERAL            = $00000104;
const CKM_RC2_CBC_PAD                = $00000105;

const CKM_RC4_KEY_GEN                = $00000110;
const CKM_RC4                        = $00000111;
const CKM_DES_KEY_GEN                = $00000120;
const CKM_DES_ECB                    = $00000121;
const CKM_DES_CBC                    = $00000122;
const CKM_DES_MAC                    = $00000123;

// CKM_DES_MAC_GENERAL and CKM_DES_CBC_PAD are new for v2.0 
const CKM_DES_MAC_GENERAL            = $00000124;
const CKM_DES_CBC_PAD                = $00000125;

const CKM_DES2_KEY_GEN               = $00000130;
const CKM_DES3_KEY_GEN               = $00000131;
const CKM_DES3_ECB                   = $00000132;
const CKM_DES3_CBC                   = $00000133;
const CKM_DES3_MAC                   = $00000134;

// CKM_DES3_MAC_GENERAL, CKM_DES3_CBC_PAD, CKM_CDMF_KEY_GEN,
// CKM_CDMF_ECB, CKM_CDMF_CBC, CKM_CDMF_MAC,
// CKM_CDMF_MAC_GENERAL, and CKM_CDMF_CBC_PAD are new for v2.0 
const CKM_DES3_MAC_GENERAL           = $00000135;
const CKM_DES3_CBC_PAD               = $00000136;
const CKM_CDMF_KEY_GEN               = $00000140;
const CKM_CDMF_ECB                   = $00000141;
const CKM_CDMF_CBC                   = $00000142;
const CKM_CDMF_MAC                   = $00000143;
const CKM_CDMF_MAC_GENERAL           = $00000144;
const CKM_CDMF_CBC_PAD               = $00000145;

// the following four DES mechanisms are new for v2.20 
const CKM_DES_OFB64                  = $00000150;
const CKM_DES_OFB8                   = $00000151;
const CKM_DES_CFB64                  = $00000152;
const CKM_DES_CFB8                   = $00000153;

const CKM_MD2                        = $00000200;

// CKM_MD2_HMAC and CKM_MD2_HMAC_GENERAL are new for v2.0 
const CKM_MD2_HMAC                   = $00000201;
const CKM_MD2_HMAC_GENERAL           = $00000202;

const CKM_MD5                        = $00000210;

// CKM_MD5_HMAC and CKM_MD5_HMAC_GENERAL are new for v2.0 
const CKM_MD5_HMAC                   = $00000211;
const CKM_MD5_HMAC_GENERAL           = $00000212;

const CKM_SHA_1                      = $00000220;

// CKM_SHA_1_HMAC and CKM_SHA_1_HMAC_GENERAL are new for v2.0 
const CKM_SHA_1_HMAC                 = $00000221;
const CKM_SHA_1_HMAC_GENERAL         = $00000222;

// CKM_RIPEMD128, CKM_RIPEMD128_HMAC,
// CKM_RIPEMD128_HMAC_GENERAL, CKM_RIPEMD160, CKM_RIPEMD160_HMAC,
// and CKM_RIPEMD160_HMAC_GENERAL are new for v2.10
const CKM_RIPEMD128                  = $00000230;
const CKM_RIPEMD128_HMAC             = $00000231;
const CKM_RIPEMD128_HMAC_GENERAL     = $00000232;
const CKM_RIPEMD160                  = $00000240;
const CKM_RIPEMD160_HMAC             = $00000241;
const CKM_RIPEMD160_HMAC_GENERAL     = $00000242;

// CKM_SHA256/384/512 are new for v2.20 
const CKM_SHA256                     = $00000250;
const CKM_SHA256_HMAC                = $00000251;
const CKM_SHA256_HMAC_GENERAL        = $00000252;

// SHA-224 is new for PKCS #11 v2.20 amendment 3 
const CKM_SHA224                     = $00000255;
const CKM_SHA224_HMAC                = $00000256;
const CKM_SHA224_HMAC_GENERAL        = $00000257;

const CKM_SHA384                     = $00000260;
const CKM_SHA384_HMAC                = $00000261;
const CKM_SHA384_HMAC_GENERAL        = $00000262;
const CKM_SHA512                     = $00000270;
const CKM_SHA512_HMAC                = $00000271;
const CKM_SHA512_HMAC_GENERAL        = $00000272;

// SecurID is new for PKCS #11 v2.20 amendment 1 
const CKM_SECURID_KEY_GEN            = $00000280;
const CKM_SECURID                    = $00000282;

// HOTP is new for PKCS #11 v2.20 amendment 1 
const CKM_HOTP_KEY_GEN    = $00000290;
const CKM_HOTP            = $00000291;

// ACTI is new for PKCS #11 v2.20 amendment 1 
const CKM_ACTI            = $000002A0;
const CKM_ACTI_KEY_GEN    = $000002A1;

// All of the following mechanisms are new for v2.0 
// Note that CAST128 and CAST5 are the same algorithm 
const CKM_CAST_KEY_GEN               = $00000300;
const CKM_CAST_ECB                   = $00000301;
const CKM_CAST_CBC                   = $00000302;
const CKM_CAST_MAC                   = $00000303;
const CKM_CAST_MAC_GENERAL           = $00000304;
const CKM_CAST_CBC_PAD               = $00000305;
const CKM_CAST3_KEY_GEN              = $00000310;
const CKM_CAST3_ECB                  = $00000311;
const CKM_CAST3_CBC                  = $00000312;
const CKM_CAST3_MAC                  = $00000313;
const CKM_CAST3_MAC_GENERAL          = $00000314;
const CKM_CAST3_CBC_PAD              = $00000315;
const CKM_CAST5_KEY_GEN              = $00000320;
const CKM_CAST128_KEY_GEN            = $00000320;
const CKM_CAST5_ECB                  = $00000321;
const CKM_CAST128_ECB                = $00000321;
const CKM_CAST5_CBC                  = $00000322;
const CKM_CAST128_CBC                = $00000322;
const CKM_CAST5_MAC                  = $00000323;
const CKM_CAST128_MAC                = $00000323;
const CKM_CAST5_MAC_GENERAL          = $00000324;
const CKM_CAST128_MAC_GENERAL        = $00000324;
const CKM_CAST5_CBC_PAD              = $00000325;
const CKM_CAST128_CBC_PAD            = $00000325;
const CKM_RC5_KEY_GEN                = $00000330;
const CKM_RC5_ECB                    = $00000331;
const CKM_RC5_CBC                    = $00000332;
const CKM_RC5_MAC                    = $00000333;
const CKM_RC5_MAC_GENERAL            = $00000334;
const CKM_RC5_CBC_PAD                = $00000335;
const CKM_IDEA_KEY_GEN               = $00000340;
const CKM_IDEA_ECB                   = $00000341;
const CKM_IDEA_CBC                   = $00000342;
const CKM_IDEA_MAC                   = $00000343;
const CKM_IDEA_MAC_GENERAL           = $00000344;
const CKM_IDEA_CBC_PAD               = $00000345;
const CKM_GENERIC_SECRET_KEY_GEN     = $00000350;
const CKM_CONCATENATE_BASE_AND_KEY   = $00000360;
const CKM_CONCATENATE_BASE_AND_DATA  = $00000362;
const CKM_CONCATENATE_DATA_AND_BASE  = $00000363;
const CKM_XOR_BASE_AND_DATA          = $00000364;
const CKM_EXTRACT_KEY_FROM_KEY       = $00000365;
const CKM_SSL3_PRE_MASTER_KEY_GEN    = $00000370;
const CKM_SSL3_MASTER_KEY_DERIVE     = $00000371;
const CKM_SSL3_KEY_AND_MAC_DERIVE    = $00000372;

// CKM_SSL3_MASTER_KEY_DERIVE_DH, CKM_TLS_PRE_MASTER_KEY_GEN,
// CKM_TLS_MASTER_KEY_DERIVE, CKM_TLS_KEY_AND_MAC_DERIVE, and
// CKM_TLS_MASTER_KEY_DERIVE_DH are new for v2.11 
const CKM_SSL3_MASTER_KEY_DERIVE_DH  = $00000373;
const CKM_TLS_PRE_MASTER_KEY_GEN     = $00000374;
const CKM_TLS_MASTER_KEY_DERIVE      = $00000375;
const CKM_TLS_KEY_AND_MAC_DERIVE     = $00000376;
const CKM_TLS_MASTER_KEY_DERIVE_DH   = $00000377;

// CKM_TLS_PRF is new for v2.20 
const CKM_TLS_PRF                    = $00000378;

const CKM_SSL3_MD5_MAC               = $00000380;
const CKM_SSL3_SHA1_MAC              = $00000381;
const CKM_MD5_KEY_DERIVATION         = $00000390;
const CKM_MD2_KEY_DERIVATION         = $00000391;
const CKM_SHA1_KEY_DERIVATION        = $00000392;

// CKM_SHA256/384/512 are new for v2.20 
const CKM_SHA256_KEY_DERIVATION      = $00000393;
const CKM_SHA384_KEY_DERIVATION      = $00000394;
const CKM_SHA512_KEY_DERIVATION      = $00000395;

// SHA-224 key derivation is new for PKCS #11 v2.20 amendment 3 
const CKM_SHA224_KEY_DERIVATION      = $00000396;

const CKM_PBE_MD2_DES_CBC            = $000003A0;
const CKM_PBE_MD5_DES_CBC            = $000003A1;
const CKM_PBE_MD5_CAST_CBC           = $000003A2;
const CKM_PBE_MD5_CAST3_CBC          = $000003A3;
const CKM_PBE_MD5_CAST5_CBC          = $000003A4;
const CKM_PBE_MD5_CAST128_CBC        = $000003A4;
const CKM_PBE_SHA1_CAST5_CBC         = $000003A5;
const CKM_PBE_SHA1_CAST128_CBC       = $000003A5;
const CKM_PBE_SHA1_RC4_128           = $000003A6;
const CKM_PBE_SHA1_RC4_40            = $000003A7;
const CKM_PBE_SHA1_DES3_EDE_CBC      = $000003A8;
const CKM_PBE_SHA1_DES2_EDE_CBC      = $000003A9;
const CKM_PBE_SHA1_RC2_128_CBC       = $000003AA;
const CKM_PBE_SHA1_RC2_40_CBC        = $000003AB;

// CKM_PKCS5_PBKD2 is new for v2.10 
const CKM_PKCS5_PBKD2                = $000003B0;

const CKM_PBA_SHA1_WITH_SHA1_HMAC    = $000003C0;

// WTLS mechanisms are new for v2.20 
const CKM_WTLS_PRE_MASTER_KEY_GEN         = $000003D0;
const CKM_WTLS_MASTER_KEY_DERIVE          = $000003D1;
const CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC   = $000003D2;
const CKM_WTLS_PRF                        = $000003D3;
const CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE  = $000003D4;
const CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE  = $000003D5;

const CKM_KEY_WRAP_LYNKS             = $00000400;
const CKM_KEY_WRAP_SET_OAEP          = $00000401;

// CKM_CMS_SIG is new for v2.20 
const CKM_CMS_SIG                    = $00000500;

// CKM_KIP mechanisms are new for PKCS #11 v2.20 amendment 2 
const CKM_KIP_DERIVE	               = $00000510;
const CKM_KIP_WRAP	                 = $00000511;
const CKM_KIP_MAC	                   = $00000512;

// Camellia is new for PKCS #11 v2.20 amendment 3 
const CKM_CAMELLIA_KEY_GEN           = $00000550;
const CKM_CAMELLIA_ECB               = $00000551;
const CKM_CAMELLIA_CBC               = $00000552;
const CKM_CAMELLIA_MAC               = $00000553;
const CKM_CAMELLIA_MAC_GENERAL       = $00000554;
const CKM_CAMELLIA_CBC_PAD           = $00000555;
const CKM_CAMELLIA_ECB_ENCRYPT_DATA  = $00000556;
const CKM_CAMELLIA_CBC_ENCRYPT_DATA  = $00000557;
const CKM_CAMELLIA_CTR               = $00000558;

// ARIA is new for PKCS #11 v2.20 amendment 3 
const CKM_ARIA_KEY_GEN               = $00000560;
const CKM_ARIA_ECB                   = $00000561;
const CKM_ARIA_CBC                   = $00000562;
const CKM_ARIA_MAC                   = $00000563;
const CKM_ARIA_MAC_GENERAL           = $00000564;
const CKM_ARIA_CBC_PAD               = $00000565;
const CKM_ARIA_ECB_ENCRYPT_DATA      = $00000566;
const CKM_ARIA_CBC_ENCRYPT_DATA      = $00000567;

// Fortezza mechanisms 
const CKM_SKIPJACK_KEY_GEN           = $00001000;
const CKM_SKIPJACK_ECB64             = $00001001;
const CKM_SKIPJACK_CBC64             = $00001002;
const CKM_SKIPJACK_OFB64             = $00001003;
const CKM_SKIPJACK_CFB64             = $00001004;
const CKM_SKIPJACK_CFB32             = $00001005;
const CKM_SKIPJACK_CFB16             = $00001006;
const CKM_SKIPJACK_CFB8              = $00001007;
const CKM_SKIPJACK_WRAP              = $00001008;
const CKM_SKIPJACK_PRIVATE_WRAP      = $00001009;
const CKM_SKIPJACK_RELAYX            = $0000100a;
const CKM_KEA_KEY_PAIR_GEN           = $00001010;
const CKM_KEA_KEY_DERIVE             = $00001011;
const CKM_FORTEZZA_TIMESTAMP         = $00001020;
const CKM_BATON_KEY_GEN              = $00001030;
const CKM_BATON_ECB128               = $00001031;
const CKM_BATON_ECB96                = $00001032;
const CKM_BATON_CBC128               = $00001033;
const CKM_BATON_COUNTER              = $00001034;
const CKM_BATON_SHUFFLE              = $00001035;
const CKM_BATON_WRAP                 = $00001036;

// CKM_ECDSA_KEY_PAIR_GEN is deprecated in v2.11,
// CKM_EC_KEY_PAIR_GEN is preferred 
const CKM_ECDSA_KEY_PAIR_GEN         = $00001040;
const CKM_EC_KEY_PAIR_GEN            = $00001040;

const CKM_ECDSA                      = $00001041;
const CKM_ECDSA_SHA1                 = $00001042;

// CKM_ECDH1_DERIVE, CKM_ECDH1_COFACTOR_DERIVE, and CKM_ECMQV_DERIVE
// are new for v2.11 
const CKM_ECDH1_DERIVE               = $00001050;
const CKM_ECDH1_COFACTOR_DERIVE      = $00001051;
const CKM_ECMQV_DERIVE               = $00001052;

const CKM_JUNIPER_KEY_GEN            = $00001060;
const CKM_JUNIPER_ECB128             = $00001061;
const CKM_JUNIPER_CBC128             = $00001062;
const CKM_JUNIPER_COUNTER            = $00001063;
const CKM_JUNIPER_SHUFFLE            = $00001064;
const CKM_JUNIPER_WRAP               = $00001065;
const CKM_FASTHASH                   = $00001070;

// CKM_AES_KEY_GEN, CKM_AES_ECB, CKM_AES_CBC, CKM_AES_MAC,
// CKM_AES_MAC_GENERAL, CKM_AES_CBC_PAD, CKM_DSA_PARAMETER_GEN,
// CKM_DH_PKCS_PARAMETER_GEN, and CKM_X9_42_DH_PARAMETER_GEN are
// new for v2.11 
const CKM_AES_KEY_GEN                = $00001080;
const CKM_AES_ECB                    = $00001081;
const CKM_AES_CBC                    = $00001082;
const CKM_AES_MAC                    = $00001083;
const CKM_AES_MAC_GENERAL            = $00001084;
const CKM_AES_CBC_PAD                = $00001085;

// AES counter mode is new for PKCS #11 v2.20 amendment 3 
const CKM_AES_CTR                    = $00001086;

// BlowFish and TwoFish are new for v2.20 
const CKM_BLOWFISH_KEY_GEN           = $00001090;
const CKM_BLOWFISH_CBC               = $00001091;
const CKM_TWOFISH_KEY_GEN            = $00001092;
const CKM_TWOFISH_CBC                = $00001093;


// CKM_xxx_ENCRYPT_DATA mechanisms are new for v2.20 
const CKM_DES_ECB_ENCRYPT_DATA       = $00001100;
const CKM_DES_CBC_ENCRYPT_DATA       = $00001101;
const CKM_DES3_ECB_ENCRYPT_DATA      = $00001102;
const CKM_DES3_CBC_ENCRYPT_DATA      = $00001103;
const CKM_AES_ECB_ENCRYPT_DATA       = $00001104;
const CKM_AES_CBC_ENCRYPT_DATA       = $00001105;

const CKM_DSA_PARAMETER_GEN          = $00002000;
const CKM_DH_PKCS_PARAMETER_GEN      = $00002001;
const CKM_X9_42_DH_PARAMETER_GEN     = $00002002;

const CKM_VENDOR_DEFINED             = $80000000;

type CK_MECHANISM_TYPE_PTR = ^CK_MECHANISM_TYPE; 


// CK_MECHANISM is a structure that specifies a particular
// mechanism  
type CK_MECHANISM = packed record 
  mechanism: CK_MECHANISM_TYPE;
  pParameter: CK_VOID_PTR;

  // ulParameterLen was changed from CK_USHORT to CK_ULONG for
  // v2.0
  ulParameterLen: CK_ULONG;           // in bytes
end;

type CK_MECHANISM_PTR = ^CK_MECHANISM; 


// CK_MECHANISM_INFO provides information about a particular
// mechanism 
type CK_MECHANISM_INFO = packed record 
  ulMinKeySize: CK_ULONG;
  ulMaxKeySize: CK_ULONG;
  flags: CK_FLAGS;
end;

// The flags are defined as follows:
//      Bit Flag               Mask        Meaning 
const CKF_HW                 = $00000001;  // performed by HW 

// The flags CKF_ENCRYPT, CKF_DECRYPT, CKF_DIGEST, CKF_SIGN,
// CKG_SIGN_RECOVER, CKF_VERIFY, CKF_VERIFY_RECOVER,
// CKF_GENERATE, CKF_GENERATE_KEY_PAIR, CKF_WRAP, CKF_UNWRAP,
// and CKF_DERIVE are new for v2.0.  They specify whether or not
// a mechanism can be used for a particular task 
const CKF_ENCRYPT            = $00000100;
const CKF_DECRYPT            = $00000200;
const CKF_DIGEST             = $00000400;
const CKF_SIGN               = $00000800;
const CKF_SIGN_RECOVER       = $00001000;
const CKF_VERIFY             = $00002000;
const CKF_VERIFY_RECOVER     = $00004000;
const CKF_GENERATE           = $00008000;
const CKF_GENERATE_KEY_PAIR  = $00010000;
const CKF_WRAP               = $00020000;
const CKF_UNWRAP             = $00040000;
const CKF_DERIVE             = $00080000;

// CKF_EC_F_P, CKF_EC_F_2M, CKF_EC_ECPARAMETERS, CKF_EC_NAMEDCURVE,
// CKF_EC_UNCOMPRESS, and CKF_EC_COMPRESS are new for v2.11. They
// describe a token's EC capabilities not available in mechanism
// information. 
const CKF_EC_F_P             = $00100000;
const CKF_EC_F_2M            = $00200000;
const CKF_EC_ECPARAMETERS    = $00400000;
const CKF_EC_NAMEDCURVE      = $00800000;
const CKF_EC_UNCOMPRESS      = $01000000;
const CKF_EC_COMPRESS        = $02000000;

const CKF_EXTENSION          = $80000000; // FALSE for this version 

type CK_MECHANISM_INFO_PTR = ^CK_MECHANISM_INFO; 


// CK_RV is a value that identifies the return value of a
// Cryptoki function 
// CK_RV was changed from CK_USHORT to CK_ULONG for v2.0 
type CK_RV = CK_ULONG;          

const CKR_OK                                = $00000000;
const CKR_CANCEL                            = $00000001;
const CKR_HOST_MEMORY                       = $00000002;
const CKR_SLOT_ID_INVALID                   = $00000003;

// CKR_FLAGS_INVALID was removed for v2.0 

// CKR_GENERAL_ERROR and CKR_FUNCTION_FAILED are new for v2.0 
const CKR_GENERAL_ERROR                     = $00000005;
const CKR_FUNCTION_FAILED                   = $00000006;

// CKR_ARGUMENTS_BAD, CKR_NO_EVENT, CKR_NEED_TO_CREATE_THREADS,
// and CKR_CANT_LOCK are new for v2.01 
const CKR_ARGUMENTS_BAD                     = $00000007;
const CKR_NO_EVENT                          = $00000008;
const CKR_NEED_TO_CREATE_THREADS            = $00000009;
const CKR_CANT_LOCK                         = $0000000A;

const CKR_ATTRIBUTE_READ_ONLY               = $00000010;
const CKR_ATTRIBUTE_SENSITIVE               = $00000011;
const CKR_ATTRIBUTE_TYPE_INVALID            = $00000012;
const CKR_ATTRIBUTE_VALUE_INVALID           = $00000013;
const CKR_DATA_INVALID                      = $00000020;
const CKR_DATA_LEN_RANGE                    = $00000021;
const CKR_DEVICE_ERROR                      = $00000030;
const CKR_DEVICE_MEMORY                     = $00000031;
const CKR_DEVICE_REMOVED                    = $00000032;
const CKR_ENCRYPTED_DATA_INVALID            = $00000040;
const CKR_ENCRYPTED_DATA_LEN_RANGE          = $00000041;
const CKR_FUNCTION_CANCELED                 = $00000050;
const CKR_FUNCTION_NOT_PARALLEL             = $00000051;

// CKR_FUNCTION_NOT_SUPPORTED is new for v2.0 
const CKR_FUNCTION_NOT_SUPPORTED            = $00000054;

const CKR_KEY_HANDLE_INVALID                = $00000060;

// CKR_KEY_SENSITIVE was removed for v2.0 

const CKR_KEY_SIZE_RANGE                    = $00000062;
const CKR_KEY_TYPE_INCONSISTENT             = $00000063;

// CKR_KEY_NOT_NEEDED, CKR_KEY_CHANGED, CKR_KEY_NEEDED,
// CKR_KEY_INDIGESTIBLE, CKR_KEY_FUNCTION_NOT_PERMITTED,
// CKR_KEY_NOT_WRAPPABLE, and CKR_KEY_UNEXTRACTABLE are new for
// v2.0 
const CKR_KEY_NOT_NEEDED                    = $00000064;
const CKR_KEY_CHANGED                       = $00000065;
const CKR_KEY_NEEDED                        = $00000066;
const CKR_KEY_INDIGESTIBLE                  = $00000067;
const CKR_KEY_FUNCTION_NOT_PERMITTED        = $00000068;
const CKR_KEY_NOT_WRAPPABLE                 = $00000069;
const CKR_KEY_UNEXTRACTABLE                 = $0000006A;

const CKR_MECHANISM_INVALID                 = $00000070;
const CKR_MECHANISM_PARAM_INVALID           = $00000071;

// CKR_OBJECT_CLASS_INCONSISTENT and CKR_OBJECT_CLASS_INVALID
// were removed for v2.0 
const CKR_OBJECT_HANDLE_INVALID             = $00000082;
const CKR_OPERATION_ACTIVE                  = $00000090;
const CKR_OPERATION_NOT_INITIALIZED         = $00000091;
const CKR_PIN_INCORRECT                     = $000000A0;
const CKR_PIN_INVALID                       = $000000A1;
const CKR_PIN_LEN_RANGE                     = $000000A2;

// CKR_PIN_EXPIRED and CKR_PIN_LOCKED are new for v2.0 
const CKR_PIN_EXPIRED                       = $000000A3;
const CKR_PIN_LOCKED                        = $000000A4;

const CKR_SESSION_CLOSED                    = $000000B0;
const CKR_SESSION_COUNT                     = $000000B1;
const CKR_SESSION_HANDLE_INVALID            = $000000B3;
const CKR_SESSION_PARALLEL_NOT_SUPPORTED    = $000000B4;
const CKR_SESSION_READ_ONLY                 = $000000B5;
const CKR_SESSION_EXISTS                    = $000000B6;

// CKR_SESSION_READ_ONLY_EXISTS and
// CKR_SESSION_READ_WRITE_SO_EXISTS are new for v2.0 
const CKR_SESSION_READ_ONLY_EXISTS          = $000000B7;
const CKR_SESSION_READ_WRITE_SO_EXISTS      = $000000B8;

const CKR_SIGNATURE_INVALID                 = $000000C0;
const CKR_SIGNATURE_LEN_RANGE               = $000000C1;
const CKR_TEMPLATE_INCOMPLETE               = $000000D0;
const CKR_TEMPLATE_INCONSISTENT             = $000000D1;
const CKR_TOKEN_NOT_PRESENT                 = $000000E0;
const CKR_TOKEN_NOT_RECOGNIZED              = $000000E1;
const CKR_TOKEN_WRITE_PROTECTED             = $000000E2;
const CKR_UNWRAPPING_KEY_HANDLE_INVALID     = $000000F0;
const CKR_UNWRAPPING_KEY_SIZE_RANGE         = $000000F1;
const CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT  = $000000F2;
const CKR_USER_ALREADY_LOGGED_IN            = $00000100;
const CKR_USER_NOT_LOGGED_IN                = $00000101;
const CKR_USER_PIN_NOT_INITIALIZED          = $00000102;
const CKR_USER_TYPE_INVALID                 = $00000103;

// CKR_USER_ANOTHER_ALREADY_LOGGED_IN and CKR_USER_TOO_MANY_TYPES
// are new to v2.01 
const CKR_USER_ANOTHER_ALREADY_LOGGED_IN    = $00000104;
const CKR_USER_TOO_MANY_TYPES               = $00000105;

const CKR_WRAPPED_KEY_INVALID               = $00000110;
const CKR_WRAPPED_KEY_LEN_RANGE             = $00000112;
const CKR_WRAPPING_KEY_HANDLE_INVALID       = $00000113;
const CKR_WRAPPING_KEY_SIZE_RANGE           = $00000114;
const CKR_WRAPPING_KEY_TYPE_INCONSISTENT    = $00000115;
const CKR_RANDOM_SEED_NOT_SUPPORTED         = $00000120;

// These are new to v2.0 
const CKR_RANDOM_NO_RNG                     = $00000121;

// These are new to v2.11 
const CKR_DOMAIN_PARAMS_INVALID             = $00000130;

// These are new to v2.0 
const CKR_BUFFER_TOO_SMALL                  = $00000150;
const CKR_SAVED_STATE_INVALID               = $00000160;
const CKR_INFORMATION_SENSITIVE             = $00000170;
const CKR_STATE_UNSAVEABLE                  = $00000180;

// These are new to v2.01 
const CKR_CRYPTOKI_NOT_INITIALIZED          = $00000190;
const CKR_CRYPTOKI_ALREADY_INITIALIZED      = $00000191;
const CKR_MUTEX_BAD                         = $000001A0;
const CKR_MUTEX_NOT_LOCKED                  = $000001A1;

// The following return values are new for PKCS #11 v2.20 amendment 3 
const CKR_NEW_PIN_MODE                      = $000001B0;
const CKR_NEXT_OTP                          = $000001B1;

// This is new to v2.20 
const CKR_FUNCTION_REJECTED                 = $00000200;

const CKR_VENDOR_DEFINED                    = $80000000;


// CK_NOTIFY is an application callback that processes events
//typedef CK_CALLBACK_FUNCTION(CK_RV, CK_NOTIFY)(
//  CK_SESSION_HANDLE hSession,     // the session's handle
//  CK_NOTIFICATION   event,
//  CK_VOID_PTR       pApplication  // passed to C_OpenSession
//);
type CK_NOTIFY = function(
                          hSession: CK_SESSION_HANDLE;     // the session's handle
                          event: CK_NOTIFICATION;
                          pApplication: CK_VOID_PTR        // passed to C_OpenSession
                         ): CK_RV; cdecl; 


// CK_FUNCTION_LIST is a structure holding a Cryptoki spec
// version and pointers of appropriate types to all the
// Cryptoki functions 
// CK_FUNCTION_LIST is new for v2.0
// THIS IS DEFINED IN pkcs11f.pas
//type CK_FUNCTION_LIST = record
//end;
type CK_FUNCTION_LIST_PTR = Pointer; // ^CK_FUNCTION_LIST;
type CK_FUNCTION_LIST_PTR_PTR = ^CK_FUNCTION_LIST_PTR;


// CK_CREATEMUTEX is an application callback for creating a
// mutex object
//typedef CK_CALLBACK_FUNCTION(CK_RV, CK_CREATEMUTEX)(
//  CK_VOID_PTR_PTR ppMutex  // location to receive ptr to mutex
//);
type CK_CREATEMUTEX = function(
                          ppMutex: CK_VOID_PTR_PTR     // location to receive ptr to mutex
                         ): CK_RV; cdecl; 


// CK_DESTROYMUTEX is an application callback for destroying a
// mutex object
//typedef CK_CALLBACK_FUNCTION(CK_RV, CK_DESTROYMUTEX)(
//  CK_VOID_PTR pMutex  // pointer to mutex
//);
type CK_DESTROYMUTEX = function(
                          pMutex: CK_VOID_PTR     // pointer to mutex
                         ): CK_RV; cdecl; 


// CK_LOCKMUTEX is an application callback for locking a mutex
//typedef CK_CALLBACK_FUNCTION(CK_RV, CK_LOCKMUTEX)(
//  CK_VOID_PTR pMutex  // pointer to mutex
//);
type CK_LOCKMUTEX = function(
                          pMutex: CK_VOID_PTR     // pointer to mutex
                         ): CK_RV; cdecl; 


// CK_UNLOCKMUTEX is an application callback for unlocking a
// mutex
//typedef CK_CALLBACK_FUNCTION(CK_RV, CK_UNLOCKMUTEX)(
//  CK_VOID_PTR pMutex  // pointer to mutex
//);
type CK_UNLOCKMUTEX = function(
                          pMutex: CK_VOID_PTR     // pointer to mutex
                         ): CK_RV; cdecl;


// CK_C_INITIALIZE_ARGS provides the optional arguments to
// C_Initialize 
type CK_C_INITIALIZE_ARGS = packed record 
  CreateMutex: CK_CREATEMUTEX;
  DestroyMutex: CK_DESTROYMUTEX;
  LockMutex: CK_LOCKMUTEX;
  UnlockMutex: CK_UNLOCKMUTEX;
  flags: CK_FLAGS;
  pReserved: CK_VOID_PTR;
end;

// flags: bit flags that provide capabilities of the slot
//      Bit Flag                           Mask       Meaning
const CKF_LIBRARY_CANT_CREATE_OS_THREADS = $00000001;
const CKF_OS_LOCKING_OK                  = $00000002;

type CK_C_INITIALIZE_ARGS_PTR = ^CK_C_INITIALIZE_ARGS; 


// additional flags for parameters to functions 

// CKF_DONT_BLOCK is for the function C_WaitForSlotEvent 
const CKF_DONT_BLOCK     = 1;

// CK_RSA_PKCS_OAEP_MGF_TYPE is new for v2.10.
// CK_RSA_PKCS_OAEP_MGF_TYPE  is used to indicate the Message
// Generation Function (MGF) applied to a message block when
// formatting a message block for the PKCS #1 OAEP encryption
// scheme.
type CK_RSA_PKCS_MGF_TYPE = CK_ULONG; 

type CK_RSA_PKCS_MGF_TYPE_PTR = ^CK_RSA_PKCS_MGF_TYPE; 

// The following MGFs are defined 
// CKG_MGF1_SHA256, CKG_MGF1_SHA384, and CKG_MGF1_SHA512
// are new for v2.20 
const CKG_MGF1_SHA1         = $00000001;
const CKG_MGF1_SHA256       = $00000002;
const CKG_MGF1_SHA384       = $00000003;
const CKG_MGF1_SHA512       = $00000004;
// SHA-224 is new for PKCS #11 v2.20 amendment 3 
const CKG_MGF1_SHA224       = $00000005;

// CK_RSA_PKCS_OAEP_SOURCE_TYPE is new for v2.10.
// CK_RSA_PKCS_OAEP_SOURCE_TYPE  is used to indicate the source
// of the encoding parameter when formatting a message block
// for the PKCS #1 OAEP encryption scheme. 
type CK_RSA_PKCS_OAEP_SOURCE_TYPE = CK_ULONG; 

type CK_RSA_PKCS_OAEP_SOURCE_TYPE_PTR = ^CK_RSA_PKCS_OAEP_SOURCE_TYPE; 

// The following encoding parameter sources are defined 
const CKZ_DATA_SPECIFIED    = $00000001;

// CK_RSA_PKCS_OAEP_PARAMS is new for v2.10.
// CK_RSA_PKCS_OAEP_PARAMS provides the parameters to the
// CKM_RSA_PKCS_OAEP mechanism. 
type CK_RSA_PKCS_OAEP_PARAMS = packed record 
        hashAlg: CK_MECHANISM_TYPE;
        mgf: CK_RSA_PKCS_MGF_TYPE;
        source: CK_RSA_PKCS_OAEP_SOURCE_TYPE;
        pSourceData: CK_VOID_PTR;
        ulSourceDataLen: CK_ULONG;
end;

type CK_RSA_PKCS_OAEP_PARAMS_PTR = ^CK_RSA_PKCS_OAEP_PARAMS; 

// CK_RSA_PKCS_PSS_PARAMS is new for v2.11.
// CK_RSA_PKCS_PSS_PARAMS provides the parameters to the
// CKM_RSA_PKCS_PSS mechanism(s). 
type CK_RSA_PKCS_PSS_PARAMS = packed record
        hashAlg: CK_MECHANISM_TYPE;
        mgf: CK_RSA_PKCS_MGF_TYPE;
        sLen: CK_ULONG;
end;

type CK_RSA_PKCS_PSS_PARAMS_PTR = ^CK_RSA_PKCS_PSS_PARAMS; 

// CK_EC_KDF_TYPE is new for v2.11. 
type CK_EC_KDF_TYPE = CK_ULONG; 

// The following EC Key Derivation Functions are defined 
const CKD_NULL                 = $00000001;
const CKD_SHA1_KDF             = $00000002;

// CK_ECDH1_DERIVE_PARAMS is new for v2.11.
// CK_ECDH1_DERIVE_PARAMS provides the parameters to the
// CKM_ECDH1_DERIVE and CKM_ECDH1_COFACTOR_DERIVE mechanisms,
// where each party contributes one key pair.
 
type CK_ECDH1_DERIVE_PARAMS = packed record 
  kdf: CK_EC_KDF_TYPE;
  ulSharedDataLen: CK_ULONG;
  pSharedData: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
end;

type CK_ECDH1_DERIVE_PARAMS_PTR = ^CK_ECDH1_DERIVE_PARAMS; 


// CK_ECDH2_DERIVE_PARAMS is new for v2.11.
// CK_ECDH2_DERIVE_PARAMS provides the parameters to the
// CKM_ECMQV_DERIVE mechanism, where each party contributes two key pairs. 
type CK_ECDH2_DERIVE_PARAMS = packed record 
  kdf: CK_EC_KDF_TYPE;
  ulSharedDataLen: CK_ULONG;
  pSharedData: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
  ulPrivateDataLen: CK_ULONG;
  hPrivateData: CK_OBJECT_HANDLE;
  ulPublicDataLen2: CK_ULONG;
  pPublicData2: CK_BYTE_PTR;
end;

type CK_ECDH2_DERIVE_PARAMS_PTR = ^CK_ECDH2_DERIVE_PARAMS; 

type CK_ECMQV_DERIVE_PARAMS = packed record 
  kdf: CK_EC_KDF_TYPE;
  ulSharedDataLen: CK_ULONG;
  pSharedData: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
  ulPrivateDataLen: CK_ULONG;
  hPrivateData: CK_OBJECT_HANDLE;
  ulPublicDataLen2: CK_ULONG;
  pPublicData2: CK_BYTE_PTR;
  publicKey: CK_OBJECT_HANDLE;
end;

type CK_ECMQV_DERIVE_PARAMS_PTR = ^CK_ECMQV_DERIVE_PARAMS; 

// Typedefs and defines for the CKM_X9_42_DH_KEY_PAIR_GEN and the
// CKM_X9_42_DH_PARAMETER_GEN mechanisms (new for PKCS #11 v2.11) 
type CK_X9_42_DH_KDF_TYPE = CK_ULONG; 
type CK_X9_42_DH_KDF_TYPE_PTR = ^CK_X9_42_DH_KDF_TYPE; 

// The following X9.42 DH key derivation functions are defined
// (besides CKD_NULL already defined :
const CKD_SHA1_KDF_ASN1        = $00000003;
const CKD_SHA1_KDF_CONCATENATE = $00000004;

// CK_X9_42_DH1_DERIVE_PARAMS is new for v2.11.
// CK_X9_42_DH1_DERIVE_PARAMS provides the parameters to the
// CKM_X9_42_DH_DERIVE key derivation mechanism, where each party
// contributes one key pair 
type CK_X9_42_DH1_DERIVE_PARAMS = packed record 
  kdf: CK_X9_42_DH_KDF_TYPE;
  ulOtherInfoLen: CK_ULONG;
  pOtherInfo: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
end;

type CK_X9_42_DH1_DERIVE_PARAMS_PTR = ^CK_X9_42_DH1_DERIVE_PARAMS;

// CK_X9_42_DH2_DERIVE_PARAMS is new for v2.11.
// CK_X9_42_DH2_DERIVE_PARAMS provides the parameters to the
// CKM_X9_42_DH_HYBRID_DERIVE and CKM_X9_42_MQV_DERIVE key derivation
// mechanisms, where each party contributes two key pairs 
type CK_X9_42_DH2_DERIVE_PARAMS = packed record 
  kdf: CK_X9_42_DH_KDF_TYPE;
  ulOtherInfoLen: CK_ULONG;
  pOtherInfo: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
  ulPrivateDataLen: CK_ULONG;
  hPrivateData: CK_OBJECT_HANDLE;
  ulPublicDataLen2: CK_ULONG;
  pPublicData2: CK_BYTE_PTR;
end;

type CK_X9_42_DH2_DERIVE_PARAMS_PTR = ^CK_X9_42_DH2_DERIVE_PARAMS; 

type CK_X9_42_MQV_DERIVE_PARAMS = packed record 
  kdf: CK_X9_42_DH_KDF_TYPE;
  ulOtherInfoLen: CK_ULONG;
  pOtherInfo: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
  ulPrivateDataLen: CK_ULONG;
  hPrivateData: CK_OBJECT_HANDLE;
  ulPublicDataLen2: CK_ULONG;
  pPublicData2: CK_BYTE_PTR;
  publicKey: CK_OBJECT_HANDLE;
end;

type CK_X9_42_MQV_DERIVE_PARAMS_PTR = ^CK_X9_42_MQV_DERIVE_PARAMS; 

// CK_KEA_DERIVE_PARAMS provides the parameters to the
// CKM_KEA_DERIVE mechanism 
// CK_KEA_DERIVE_PARAMS is new for v2.0 
type CK_KEA_DERIVE_PARAMS = packed record
  isSender: CK_BBOOL;
  ulRandomLen: CK_ULONG;
  pRandomA: CK_BYTE_PTR;
  pRandomB: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
end;

type CK_KEA_DERIVE_PARAMS_PTR = ^CK_KEA_DERIVE_PARAMS; 


// CK_RC2_PARAMS provides the parameters to the CKM_RC2_ECB and
// CKM_RC2_MAC mechanisms.  An instance of CK_RC2_PARAMS just
// holds the effective keysize 
type CK_RC2_PARAMS = CK_ULONG;          

type CK_RC2_PARAMS_PTR = ^CK_RC2_PARAMS; 


// CK_RC2_CBC_PARAMS provides the parameters to the CKM_RC2_CBC
// mechanism 
type CK_RC2_CBC_PARAMS = packed record 
  // ulEffectiveBits was changed from CK_USHORT to CK_ULONG for
  // v2.0 
  ulEffectiveBits: CK_ULONG;      // effective bits (1-1024)

  iv: array [0..7] of CK_BYTE;            // IV for CBC mode
end;

type CK_RC2_CBC_PARAMS_PTR = ^CK_RC2_CBC_PARAMS; 


// CK_RC2_MAC_GENERAL_PARAMS provides the parameters for the
// CKM_RC2_MAC_GENERAL mechanism 
// CK_RC2_MAC_GENERAL_PARAMS is new for v2.0 
type CK_RC2_MAC_GENERAL_PARAMS = packed record 
  ulEffectiveBits: CK_ULONG;       // effective bits (1-1024)
  ulMacLength: CK_ULONG;           // Length of MAC in bytes
end;

type CK_RC2_MAC_GENERAL_PARAMS_PTR = ^CK_RC2_MAC_GENERAL_PARAMS; 


// CK_RC5_PARAMS provides the parameters to the CKM_RC5_ECB and
// CKM_RC5_MAC mechanisms 
// CK_RC5_PARAMS is new for v2.0 
type CK_RC5_PARAMS = packed record 
  ulWordsize: CK_ULONG;       // wordsize in bits
  ulRounds: CK_ULONG;         // number of rounds
end;

type CK_RC5_PARAMS_PTR = ^CK_RC5_PARAMS; 


// CK_RC5_CBC_PARAMS provides the parameters to the CKM_RC5_CBC
// mechanism 
// CK_RC5_CBC_PARAMS is new for v2.0 
type CK_RC5_CBC_PARAMS = packed record 
  ulWordsize: CK_ULONG;       // wordsize in bits
  ulRounds: CK_ULONG;         // number of rounds
  pIv: CK_BYTE_PTR;           // pointer to IV
  ulIvLen: CK_ULONG;          // length of IV in bytes
end;

type CK_RC5_CBC_PARAMS_PTR = ^CK_RC5_CBC_PARAMS; 


// CK_RC5_MAC_GENERAL_PARAMS provides the parameters for the
// CKM_RC5_MAC_GENERAL mechanism 
// CK_RC5_MAC_GENERAL_PARAMS is new for v2.0 
type CK_RC5_MAC_GENERAL_PARAMS = packed record 
  ulWordsize: CK_ULONG;        // wordsize in bits
  ulRounds: CK_ULONG;          // number of rounds
  ulMacLength: CK_ULONG;       // Length of MAC in bytes
end;

type CK_RC5_MAC_GENERAL_PARAMS_PTR = ^CK_RC5_MAC_GENERAL_PARAMS; 


// CK_MAC_GENERAL_PARAMS provides the parameters to most block
// ciphers' MAC_GENERAL mechanisms.  Its value is the length of
// the MAC 
// CK_MAC_GENERAL_PARAMS is new for v2.0 
type CK_MAC_GENERAL_PARAMS = CK_ULONG;          

type CK_MAC_GENERAL_PARAMS_PTR = ^CK_MAC_GENERAL_PARAMS;

// CK_DES/AES_ECB/CBC_ENCRYPT_DATA_PARAMS are new for v2.20 
type CK_DES_CBC_ENCRYPT_DATA_PARAMS = packed record 
  iv: array [0..7] of CK_BYTE;
  pData: CK_BYTE_PTR;
  length: CK_ULONG;
end;

type CK_DES_CBC_ENCRYPT_DATA_PARAMS_PTR = ^CK_DES_CBC_ENCRYPT_DATA_PARAMS;

type CK_AES_CBC_ENCRYPT_DATA_PARAMS = packed record
  iv: array [0..15] of CK_BYTE;
  pData: CK_BYTE_PTR;
  length: CK_ULONG;
end;

type CK_AES_CBC_ENCRYPT_DATA_PARAMS_PTR = ^CK_AES_CBC_ENCRYPT_DATA_PARAMS; 

// CK_SKIPJACK_PRIVATE_WRAP_PARAMS provides the parameters to the
// CKM_SKIPJACK_PRIVATE_WRAP mechanism 
// CK_SKIPJACK_PRIVATE_WRAP_PARAMS is new for v2.0 
type CK_SKIPJACK_PRIVATE_WRAP_PARAMS = packed record
  ulPasswordLen: CK_ULONG;
  pPassword: CK_BYTE_PTR;
  ulPublicDataLen: CK_ULONG;
  pPublicData: CK_BYTE_PTR;
  ulPAndGLen: CK_ULONG;
  ulQLen: CK_ULONG;
  ulRandomLen: CK_ULONG;
  pRandomA: CK_BYTE_PTR;
  pPrimeP: CK_BYTE_PTR;
  pBaseG: CK_BYTE_PTR;
  pSubprimeQ: CK_BYTE_PTR;
end;

type CK_SKIPJACK_PRIVATE_WRAP_PTR = ^CK_SKIPJACK_PRIVATE_WRAP_PARAMS; 


// CK_SKIPJACK_RELAYX_PARAMS provides the parameters to the
// CKM_SKIPJACK_RELAYX mechanism 
// CK_SKIPJACK_RELAYX_PARAMS is new for v2.0 
type CK_SKIPJACK_RELAYX_PARAMS = packed record 
  ulOldWrappedXLen: CK_ULONG;
  pOldWrappedX: CK_BYTE_PTR;
  ulOldPasswordLen: CK_ULONG;
  pOldPassword: CK_BYTE_PTR;
  ulOldPublicDataLen: CK_ULONG;
  pOldPublicData: CK_BYTE_PTR;
  ulOldRandomLen: CK_ULONG;
  pOldRandomA: CK_BYTE_PTR;
  ulNewPasswordLen: CK_ULONG;
  pNewPassword: CK_BYTE_PTR;
  ulNewPublicDataLen: CK_ULONG;
  pNewPublicData: CK_BYTE_PTR;
  ulNewRandomLen: CK_ULONG;
  pNewRandomA: CK_BYTE_PTR;
end;

type CK_SKIPJACK_RELAYX_PARAMS_PTR = ^CK_SKIPJACK_RELAYX_PARAMS; 


type CK_PBE_PARAMS = packed record 
  pInitVector: CK_BYTE_PTR;
  pPassword: CK_UTF8CHAR_PTR;
  ulPasswordLen: CK_ULONG;
  pSalt: CK_BYTE_PTR;
  ulSaltLen: CK_ULONG;
  ulIteration: CK_ULONG;
end;

type CK_PBE_PARAMS_PTR = ^CK_PBE_PARAMS; 


// CK_KEY_WRAP_SET_OAEP_PARAMS provides the parameters to the
// CKM_KEY_WRAP_SET_OAEP mechanism 
// CK_KEY_WRAP_SET_OAEP_PARAMS is new for v2.0 
type CK_KEY_WRAP_SET_OAEP_PARAMS = packed record 
  bBC: CK_BYTE;           // block contents byte
  pX: CK_BYTE_PTR;        // extra data
  ulXLen: CK_ULONG;       // length of extra data in bytes
end;

type CK_KEY_WRAP_SET_OAEP_PARAMS_PTR = ^CK_KEY_WRAP_SET_OAEP_PARAMS; 


type CK_SSL3_RANDOM_DATA = packed record 
  pClientRandom: CK_BYTE_PTR;
  ulClientRandomLen: CK_ULONG;
  pServerRandom: CK_BYTE_PTR;
  ulServerRandomLen: CK_ULONG;
end;


type CK_SSL3_MASTER_KEY_DERIVE_PARAMS = packed record 
  RandomInfo: CK_SSL3_RANDOM_DATA;
  pVersion: CK_VERSION_PTR;
end;

type CK_SSL3_MASTER_KEY_DERIVE_PARAMS_PTR = ^CK_SSL3_MASTER_KEY_DERIVE_PARAMS;


type CK_SSL3_KEY_MAT_OUT = packed record 
  hClientMacSecret: CK_OBJECT_HANDLE;
  hServerMacSecret: CK_OBJECT_HANDLE;
  hClientKey: CK_OBJECT_HANDLE;
  hServerKey: CK_OBJECT_HANDLE;
  pIVClient: CK_BYTE_PTR;
  pIVServer: CK_BYTE_PTR;
end;

type CK_SSL3_KEY_MAT_OUT_PTR = ^CK_SSL3_KEY_MAT_OUT; 


type CK_SSL3_KEY_MAT_PARAMS = packed record 
  ulMacSizeInBits: CK_ULONG;
  ulKeySizeInBits: CK_ULONG;
  ulIVSizeInBits: CK_ULONG;
  bIsExport: CK_BBOOL;
  RandomInfo: CK_SSL3_RANDOM_DATA;
  pReturnedKeyMaterial: CK_SSL3_KEY_MAT_OUT_PTR;
end;

type CK_SSL3_KEY_MAT_PARAMS_PTR = ^CK_SSL3_KEY_MAT_PARAMS; 

// CK_TLS_PRF_PARAMS is new for version 2.20 
type CK_TLS_PRF_PARAMS = packed record 
  pSeed: CK_BYTE_PTR;
  ulSeedLen: CK_ULONG;
  pLabel: CK_BYTE_PTR;
  ulLabelLen: CK_ULONG;
  pOutput: CK_BYTE_PTR;
  pulOutputLen: CK_ULONG_PTR;
end;

type CK_TLS_PRF_PARAMS_PTR = ^CK_TLS_PRF_PARAMS; 

// WTLS is new for version 2.20 
type CK_WTLS_RANDOM_DATA = packed record 
  pClientRandom: CK_BYTE_PTR;
  ulClientRandomLen: CK_ULONG;
  pServerRandom: CK_BYTE_PTR;
  ulServerRandomLen: CK_ULONG;
end;

type CK_WTLS_RANDOM_DATA_PTR = ^CK_WTLS_RANDOM_DATA;

type CK_WTLS_MASTER_KEY_DERIVE_PARAMS = packed record
  DigestMechanism: CK_MECHANISM_TYPE;
  RandomInfo: CK_WTLS_RANDOM_DATA;
  pVersion: CK_BYTE_PTR;
end;

type CK_WTLS_MASTER_KEY_DERIVE_PARAMS_PTR = ^CK_WTLS_MASTER_KEY_DERIVE_PARAMS; 

type CK_WTLS_PRF_PARAMS = packed record 
  DigestMechanism: CK_MECHANISM_TYPE;
  pSeed: CK_BYTE_PTR;
  ulSeedLen: CK_ULONG;
  pLabel: CK_BYTE_PTR;
  ulLabelLen: CK_ULONG;
  pOutput: CK_BYTE_PTR;
  pulOutputLen: CK_ULONG_PTR;
end;

type CK_WTLS_PRF_PARAMS_PTR = ^CK_WTLS_PRF_PARAMS;

type CK_WTLS_KEY_MAT_OUT = packed record
  hMacSecret: CK_OBJECT_HANDLE;
  hKey: CK_OBJECT_HANDLE;
  pIV: CK_BYTE_PTR;
end;

type CK_WTLS_KEY_MAT_OUT_PTR = ^CK_WTLS_KEY_MAT_OUT; 

type CK_WTLS_KEY_MAT_PARAMS = packed record 
  DigestMechanism: CK_MECHANISM_TYPE;
  ulMacSizeInBits: CK_ULONG;
  ulKeySizeInBits: CK_ULONG;
  ulIVSizeInBits: CK_ULONG;
  ulSequenceNumber: CK_ULONG;
  bIsExport: CK_BBOOL;
  RandomInfo: CK_WTLS_RANDOM_DATA;
  pReturnedKeyMaterial: CK_WTLS_KEY_MAT_OUT_PTR;
end;

type CK_WTLS_KEY_MAT_PARAMS_PTR = ^CK_WTLS_KEY_MAT_PARAMS;

// CMS is new for version 2.20
type CK_CMS_SIG_PARAMS = packed record
  certificateHandle: CK_OBJECT_HANDLE;
  pSigningMechanism: CK_MECHANISM_PTR;
  pDigestMechanism: CK_MECHANISM_PTR;
  pContentType: CK_UTF8CHAR_PTR;
  pRequestedAttributes: CK_BYTE_PTR;
  ulRequestedAttributesLen: CK_ULONG;
  pRequiredAttributes: CK_BYTE_PTR;
  ulRequiredAttributesLen: CK_ULONG;
end;

type CK_CMS_SIG_PARAMS_PTR = ^CK_CMS_SIG_PARAMS; 

type CK_KEY_DERIVATION_STRING_DATA = packed record 
  pData: CK_BYTE_PTR;
  ulLen: CK_ULONG;
end;

type CK_KEY_DERIVATION_STRING_DATA_PTR = ^CK_KEY_DERIVATION_STRING_DATA; 


// The CK_EXTRACT_PARAMS is used for the
// CKM_EXTRACT_KEY_FROM_KEY mechanism.  It specifies which bit
// of the base key should be used as the first bit of the
// derived key 
// CK_EXTRACT_PARAMS is new for v2.0 
type CK_EXTRACT_PARAMS = CK_ULONG;

type CK_EXTRACT_PARAMS_PTR = ^CK_EXTRACT_PARAMS; 

// CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE is new for v2.10.
// CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE is used to
// indicate the Pseudo-Random Function (PRF) used to generate
// key bits using PKCS #5 PBKDF2. 
type CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE = CK_ULONG; 

type CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE_PTR = ^CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE;

// The following PRFs are defined in PKCS #5 v2.0. 
const CKP_PKCS5_PBKD2_HMAC_SHA1 = $00000001;


// CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE is new for v2.10.
// CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE is used to indicate the
// source of the salt value when deriving a key using PKCS #5
// PBKDF2.
type CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE = CK_ULONG; 

type CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE_PTR = ^CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE;

// The following salt value sources are defined in PKCS #5 v2.0. 
const CKZ_SALT_SPECIFIED        = $00000001;

// CK_PKCS5_PBKD2_PARAMS is new for v2.10.
// CK_PKCS5_PBKD2_PARAMS is a structure that provides the
// parameters to the CKM_PKCS5_PBKD2 mechanism. 
type CK_PKCS5_PBKD2_PARAMS = packed record
        saltSource: CK_PKCS5_PBKDF2_SALT_SOURCE_TYPE;
        pSaltSourceData: CK_VOID_PTR;
        ulSaltSourceDataLen: CK_ULONG;
        iterations: CK_ULONG;
        prf: CK_PKCS5_PBKD2_PSEUDO_RANDOM_FUNCTION_TYPE;
        pPrfData: CK_VOID_PTR;
        ulPrfDataLen: CK_ULONG;
        pPassword: CK_UTF8CHAR_PTR;
        ulPasswordLen: CK_ULONG_PTR;
end;

type CK_PKCS5_PBKD2_PARAMS_PTR = ^CK_PKCS5_PBKD2_PARAMS; 

// All CK_OTP structs are new for PKCS #11 v2.20 amendment 3 

type CK_OTP_PARAM_TYPE = CK_ULONG; 
type CK_PARAM_TYPE = CK_OTP_PARAM_TYPE; // B/w compatibility 

type CK_OTP_PARAM = packed record
    // "paramType" as "type" is a Delphi reserved word
    paramType: CK_OTP_PARAM_TYPE;
    pValue: CK_VOID_PTR;
    ulValueLen: CK_ULONG;
end;

type CK_OTP_PARAM_PTR = ^CK_OTP_PARAM; 

type CK_OTP_PARAMS = packed record 
    pParams: CK_OTP_PARAM_PTR;
    ulCount: CK_ULONG;
end;

type CK_OTP_PARAMS_PTR = ^CK_OTP_PARAMS; 

type CK_OTP_SIGNATURE_INFO = packed record 
    pParams: CK_OTP_PARAM_PTR;
    ulCount: CK_ULONG;
end;

type CK_OTP_SIGNATURE_INFO_PTR = ^CK_OTP_SIGNATURE_INFO; 

// The following OTP-related defines are new for PKCS #11 v2.20 amendment 1 
const CK_OTP_VALUE          = 0;
const CK_OTP_PIN            = 1;
const CK_OTP_CHALLENGE      = 2;
const CK_OTP_TIME           = 3;
const CK_OTP_COUNTER        = 4;
const CK_OTP_FLAGS          = 5;
const CK_OTP_OUTPUT_LENGTH  = 6;
const CK_OTP_OUTPUT_FORMAT  = 7;

// The following OTP-related defines are new for PKCS #11 v2.20 amendment 1 
const CKF_NEXT_OTP          = $00000001;
const CKF_EXCLUDE_TIME      = $00000002;
const CKF_EXCLUDE_COUNTER   = $00000004;
const CKF_EXCLUDE_CHALLENGE = $00000008;
const CKF_EXCLUDE_PIN       = $00000010;
const CKF_USER_FRIENDLY_OTP = $00000020;

// CK_KIP_PARAMS is new for PKCS #11 v2.20 amendment 2 
type CK_KIP_PARAMS = packed record 
    pMechanism: CK_MECHANISM_PTR;
    hKey: CK_OBJECT_HANDLE;
    pSeed: CK_BYTE_PTR;
    ulSeedLen: CK_ULONG;
end;

type CK_KIP_PARAMS_PTR = ^CK_KIP_PARAMS;

// CK_AES_CTR_PARAMS is new for PKCS #11 v2.20 amendment 3
type CK_AES_CTR_PARAMS = packed record
    ulCounterBits: CK_ULONG;
    cb: array [0..15] of CK_BYTE;
end;

type CK_AES_CTR_PARAMS_PTR = ^CK_AES_CTR_PARAMS;

// CK_CAMELLIA_CTR_PARAMS is new for PKCS #11 v2.20 amendment 3
type CK_CAMELLIA_CTR_PARAMS = packed record
    ulCounterBits: CK_ULONG;
    cb: array [0..15] of CK_BYTE;
end;

type CK_CAMELLIA_CTR_PARAMS_PTR = ^CK_CAMELLIA_CTR_PARAMS;

// CK_CAMELLIA_CBC_ENCRYPT_DATA_PARAMS is new for PKCS #11 v2.20 amendment 3
type CK_CAMELLIA_CBC_ENCRYPT_DATA_PARAMS = packed record
    iv: array [0..15] of CK_BYTE;
    pData: CK_BYTE_PTR;
    length: CK_ULONG;
end;

type CK_CAMELLIA_CBC_ENCRYPT_DATA_PARAMS_PTR = ^CK_CAMELLIA_CBC_ENCRYPT_DATA_PARAMS;

// CK_ARIA_CBC_ENCRYPT_DATA_PARAMS is new for PKCS #11 v2.20 amendment 3
type CK_ARIA_CBC_ENCRYPT_DATA_PARAMS = packed record
    iv: array [0..15] of CK_BYTE;
    pData: CK_BYTE_PTR;
    length: CK_ULONG;
end;

type CK_ARIA_CBC_ENCRYPT_DATA_PARAMS_PTR = ^CK_ARIA_CBC_ENCRYPT_DATA_PARAMS;


implementation

END.

