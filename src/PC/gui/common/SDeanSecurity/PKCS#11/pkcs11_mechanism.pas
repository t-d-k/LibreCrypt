unit pkcs11_mechanism;

interface

uses
  pkcs11_api, pkcs11t;

type
  TPKCS11KeysizeUnits = (ksmBits, ksmBytes, ksmNA);

  TPKCS11Mechanism = class (TPKCS11API)
  private
  protected
    FSlotID:        Integer;
    FMechanismType: CK_MECHANISM_TYPE;

    function GetFlags(): CK_FLAGS;
    function GetMinKeySize(): Cardinal;
    function GetMaxKeySize(): Cardinal;
    function GetMinKeySizeInBits(): Cardinal;
    function GetMaxKeySizeInBits(): Cardinal;

    function GetMechanismTypeStr(): String;

    // Determine whether the ulMinKeySize/ulMaxKeySize fields returned by
    // CK_MECHANISM_INFO are measured in bits or bytes
    function KeysizeUnits(mechanism: CK_MECHANISM_TYPE): TPKCS11KeysizeUnits;

  public
    property SlotID: Integer Read FSlotID Write FSlotID;
    property MechanismType: CK_MECHANISM_TYPE Read FMechanismType Write FMechanismType;
    property MechanismTypeStr: String Read GetMechanismTypeStr;

    // Warning!
    // The values returned by these properties are those returned by
    // the mechanism's CK_MECHANISM_INFO; can be measured bits or bytes - it's
    // mechanism dependant!
    property MinKeySize: Cardinal Read GetMinKeySize;
    property MaxKeySize: Cardinal Read GetMaxKeySize;
    // Return the above, but always return the sizes as a number of *bits*
    property MinKeySizeInBits: Cardinal Read GetMinKeySizeInBits;
    property MaxKeySizeInBits: Cardinal Read GetMaxKeySizeInBits;
    property Flags: CK_FLAGS Read GetFlags;

    function GetMechanismInfo(var info: CK_MECHANISM_INFO): Boolean;

    function MechanismToString(mechanism: CK_MECHANISM_TYPE): String;
  end;

  TPKCS11MechanismArray = array of TPKCS11Mechanism;

implementation

uses
  pkcs11f, SDUGeneral,
  SDUi18n,
  SysUtils;

function TPKCS11Mechanism.GetMechanismInfo(var info: CK_MECHANISM_INFO): Boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetMechanismInfo, FN_NAME_C_GetMechanismInfo);

  LastRV := LibraryFunctionList.CK_C_GetMechanismInfo(FSlotID, FMechanismType, @info);
  Result := RVSuccess(LastRV);
end;

function TPKCS11Mechanism.GetFlags(): CK_FLAGS;
var
  info: CK_MECHANISM_INFO;
begin
  Result := 0;

  if (GetMechanismInfo(info)) then
    Result := info.Flags;

  Result := Result;
end;

function TPKCS11Mechanism.GetMinKeySize(): Cardinal;
var
  info: CK_MECHANISM_INFO;
begin
  Result := 0;

  if (GetMechanismInfo(info)) then
    Result := info.ulMinKeySize;

  Result := Result;
end;

function TPKCS11Mechanism.GetMaxKeySize(): Cardinal;
var
  info: CK_MECHANISM_INFO;
begin
  Result := 0;

  if (GetMechanismInfo(info)) then
    Result := info.ulMaxKeySize;

  Result := Result;
end;

function TPKCS11Mechanism.GetMinKeySizeInBits(): Cardinal;
begin
  Result := GetMinKeySize();

  if (KeysizeUnits(MechanismType) = ksmBytes) then begin
    Result := Result * 8;
  end;

  Result := Result;
end;

function TPKCS11Mechanism.GetMaxKeySizeInBits(): Cardinal;
begin
  Result := GetMaxKeySize();

  if (KeysizeUnits(MechanismType) = ksmBytes) then begin
    Result := Result * 8;
  end;

  Result := Result;
end;

function TPKCS11Mechanism.GetMechanismTypeStr(): String;
begin
  Result := MechanismToString(FMechanismType);
end;

function TPKCS11Mechanism.MechanismToString(mechanism: CK_MECHANISM_TYPE): String;
begin
  Result := RS_UNKNOWN + ' (0x' + inttohex(Ord(mechanism), 8) + ')';

  case mechanism of
    CKM_RSA_PKCS_KEY_PAIR_GEN: Result   := 'CKM_RSA_PKCS_KEY_PAIR_GEN';
    CKM_RSA_PKCS: Result                := 'CKM_RSA_PKCS';
    CKM_RSA_9796: Result                := 'CKM_RSA_9796';
    CKM_RSA_X_509: Result               := 'CKM_RSA_X_509';
    CKM_MD2_RSA_PKCS: Result            := 'CKM_MD2_RSA_PKCS';
    CKM_MD5_RSA_PKCS: Result            := 'CKM_MD5_RSA_PKCS';
    CKM_SHA1_RSA_PKCS: Result           := 'CKM_SHA1_RSA_PKCS';
    CKM_RIPEMD128_RSA_PKCS: Result      := 'CKM_RIPEMD128_RSA_PKCS';
    CKM_RIPEMD160_RSA_PKCS: Result      := 'CKM_RIPEMD160_RSA_PKCS';
    CKM_RSA_PKCS_OAEP: Result           := 'CKM_RSA_PKCS_OAEP';
    CKM_RSA_X9_31_KEY_PAIR_GEN: Result  := 'CKM_RSA_X9_31_KEY_PAIR_GEN';
    CKM_RSA_X9_31: Result               := 'CKM_RSA_X9_31';
    CKM_SHA1_RSA_X9_31: Result          := 'CKM_SHA1_RSA_X9_31';
    CKM_RSA_PKCS_PSS: Result            := 'CKM_RSA_PKCS_PSS';
    CKM_SHA1_RSA_PKCS_PSS: Result       := 'CKM_SHA1_RSA_PKCS_PSS';
    CKM_DSA_KEY_PAIR_GEN: Result        := 'CKM_DSA_KEY_PAIR_GEN';
    CKM_DSA: Result                     := 'CKM_DSA';
    CKM_DSA_SHA1: Result                := 'CKM_DSA_SHA1';
    CKM_DH_PKCS_KEY_PAIR_GEN: Result    := 'CKM_DH_PKCS_KEY_PAIR_GEN';
    CKM_DH_PKCS_DERIVE: Result          := 'CKM_DH_PKCS_DERIVE';
    CKM_X9_42_DH_KEY_PAIR_GEN: Result   := 'CKM_X9_42_DH_KEY_PAIR_GEN';
    CKM_X9_42_DH_DERIVE: Result         := 'CKM_X9_42_DH_DERIVE';
    CKM_X9_42_DH_HYBRID_DERIVE: Result  := 'CKM_X9_42_DH_HYBRID_DERIVE';
    CKM_X9_42_MQV_DERIVE: Result        := 'CKM_X9_42_MQV_DERIVE';
    CKM_SHA256_RSA_PKCS: Result         := 'CKM_SHA256_RSA_PKCS';
    CKM_SHA384_RSA_PKCS: Result         := 'CKM_SHA384_RSA_PKCS';
    CKM_SHA512_RSA_PKCS: Result         := 'CKM_SHA512_RSA_PKCS';
    CKM_SHA256_RSA_PKCS_PSS: Result     := 'CKM_SHA256_RSA_PKCS_PSS';
    CKM_SHA384_RSA_PKCS_PSS: Result     := 'CKM_SHA384_RSA_PKCS_PSS';
    CKM_SHA512_RSA_PKCS_PSS: Result     := 'CKM_SHA512_RSA_PKCS_PSS';
    CKM_SHA224_RSA_PKCS: Result         := 'CKM_SHA224_RSA_PKCS';
    CKM_SHA224_RSA_PKCS_PSS: Result     := 'CKM_SHA224_RSA_PKCS_PSS';
    CKM_RC2_KEY_GEN: Result             := 'CKM_RC2_KEY_GEN';
    CKM_RC2_ECB: Result                 := 'CKM_RC2_ECB';
    CKM_RC2_CBC: Result                 := 'CKM_RC2_CBC';
    CKM_RC2_MAC: Result                 := 'CKM_RC2_MAC';
    CKM_RC2_MAC_GENERAL: Result         := 'CKM_RC2_MAC_GENERAL';
    CKM_RC2_CBC_PAD: Result             := 'CKM_RC2_CBC_PAD';
    CKM_RC4_KEY_GEN: Result             := 'CKM_RC4_KEY_GEN';
    CKM_RC4: Result                     := 'CKM_RC4';
    CKM_DES_KEY_GEN: Result             := 'CKM_DES_KEY_GEN';
    CKM_DES_ECB: Result                 := 'CKM_DES_ECB';
    CKM_DES_CBC: Result                 := 'CKM_DES_CBC';
    CKM_DES_MAC: Result                 := 'CKM_DES_MAC';
    CKM_DES_MAC_GENERAL: Result         := 'CKM_DES_MAC_GENERAL';
    CKM_DES_CBC_PAD: Result             := 'CKM_DES_CBC_PAD';
    CKM_DES2_KEY_GEN: Result            := 'CKM_DES2_KEY_GEN';
    CKM_DES3_KEY_GEN: Result            := 'CKM_DES3_KEY_GEN';
    CKM_DES3_ECB: Result                := 'CKM_DES3_ECB';
    CKM_DES3_CBC: Result                := 'CKM_DES3_CBC';
    CKM_DES3_MAC: Result                := 'CKM_DES3_MAC';
    CKM_DES3_MAC_GENERAL: Result        := 'CKM_DES3_MAC_GENERAL';
    CKM_DES3_CBC_PAD: Result            := 'CKM_DES3_CBC_PAD';
    CKM_CDMF_KEY_GEN: Result            := 'CKM_CDMF_KEY_GEN';
    CKM_CDMF_ECB: Result                := 'CKM_CDMF_ECB';
    CKM_CDMF_CBC: Result                := 'CKM_CDMF_CBC';
    CKM_CDMF_MAC: Result                := 'CKM_CDMF_MAC';
    CKM_CDMF_MAC_GENERAL: Result        := 'CKM_CDMF_MAC_GENERAL';
    CKM_CDMF_CBC_PAD: Result            := 'CKM_CDMF_CBC_PAD';
    CKM_DES_OFB64: Result               := 'CKM_DES_OFB64';
    CKM_DES_OFB8: Result                := 'CKM_DES_OFB8';
    CKM_DES_CFB64: Result               := 'CKM_DES_CFB64';
    CKM_DES_CFB8: Result                := 'CKM_DES_CFB8';
    CKM_MD2: Result                     := 'CKM_MD2';
    CKM_MD2_HMAC: Result                := 'CKM_MD2_HMAC';
    CKM_MD2_HMAC_GENERAL: Result        := 'CKM_MD2_HMAC_GENERAL';
    CKM_MD5: Result                     := 'CKM_MD5';
    CKM_MD5_HMAC: Result                := 'CKM_MD5_HMAC';
    CKM_MD5_HMAC_GENERAL: Result        := 'CKM_MD5_HMAC_GENERAL';
    CKM_SHA_1: Result                   := 'CKM_SHA_1';
    CKM_SHA_1_HMAC: Result              := 'CKM_SHA_1_HMAC';
    CKM_SHA_1_HMAC_GENERAL: Result      := 'CKM_SHA_1_HMAC_GENERAL';
    CKM_RIPEMD128: Result               := 'CKM_RIPEMD128';
    CKM_RIPEMD128_HMAC: Result          := 'CKM_RIPEMD128_HMAC';
    CKM_RIPEMD128_HMAC_GENERAL: Result  := 'CKM_RIPEMD128_HMAC_GENERAL';
    CKM_RIPEMD160: Result               := 'CKM_RIPEMD160';
    CKM_RIPEMD160_HMAC: Result          := 'CKM_RIPEMD160_HMAC';
    CKM_RIPEMD160_HMAC_GENERAL: Result  := 'CKM_RIPEMD160_HMAC_GENERAL';
    CKM_SHA256: Result                  := 'CKM_SHA256';
    CKM_SHA256_HMAC: Result             := 'CKM_SHA256_HMAC';
    CKM_SHA256_HMAC_GENERAL: Result     := 'CKM_SHA256_HMAC_GENERAL';
    CKM_SHA224: Result                  := 'CKM_SHA224';
    CKM_SHA224_HMAC: Result             := 'CKM_SHA224_HMAC';
    CKM_SHA224_HMAC_GENERAL: Result     := 'CKM_SHA224_HMAC_GENERAL';
    CKM_SHA384: Result                  := 'CKM_SHA384';
    CKM_SHA384_HMAC: Result             := 'CKM_SHA384_HMAC';
    CKM_SHA384_HMAC_GENERAL: Result     := 'CKM_SHA384_HMAC_GENERAL';
    CKM_SHA512: Result                  := 'CKM_SHA512';
    CKM_SHA512_HMAC: Result             := 'CKM_SHA512_HMAC';
    CKM_SHA512_HMAC_GENERAL: Result     := 'CKM_SHA512_HMAC_GENERAL';
    CKM_SECURID_KEY_GEN: Result         := 'CKM_SECURID_KEY_GEN';
    CKM_SECURID: Result                 := 'CKM_SECURID';
    CKM_HOTP_KEY_GEN: Result            := 'CKM_HOTP_KEY_GEN';
    CKM_HOTP: Result                    := 'CKM_HOTP';
    CKM_ACTI: Result                    := 'CKM_ACTI';
    CKM_ACTI_KEY_GEN: Result            := 'CKM_ACTI_KEY_GEN';
    CKM_CAST_KEY_GEN: Result            := 'CKM_CAST_KEY_GEN';
    CKM_CAST_ECB: Result                := 'CKM_CAST_ECB';
    CKM_CAST_CBC: Result                := 'CKM_CAST_CBC';
    CKM_CAST_MAC: Result                := 'CKM_CAST_MAC';
    CKM_CAST_MAC_GENERAL: Result        := 'CKM_CAST_MAC_GENERAL';
    CKM_CAST_CBC_PAD: Result            := 'CKM_CAST_CBC_PAD';
    CKM_CAST3_KEY_GEN: Result           := 'CKM_CAST3_KEY_GEN';
    CKM_CAST3_ECB: Result               := 'CKM_CAST3_ECB';
    CKM_CAST3_CBC: Result               := 'CKM_CAST3_CBC';
    CKM_CAST3_MAC: Result               := 'CKM_CAST3_MAC';
    CKM_CAST3_MAC_GENERAL: Result       := 'CKM_CAST3_MAC_GENERAL';
    CKM_CAST3_CBC_PAD: Result           := 'CKM_CAST3_CBC_PAD';
    CKM_CAST5_KEY_GEN: Result           := 'CKM_CAST5_KEY_GEN';
    //CKM_CAST128_KEY_GEN:                Result := 'CKM_CAST128_KEY_GEN';
    CKM_CAST5_ECB: Result               := 'CKM_CAST5_ECB';
    //CKM_CAST128_ECB:                    Result := 'CKM_CAST128_ECB';
    CKM_CAST5_CBC: Result               := 'CKM_CAST5_CBC';
    //CKM_CAST128_CBC:                    Result := 'CKM_CAST128_CBC';
    CKM_CAST5_MAC: Result               := 'CKM_CAST5_MAC';
    //CKM_CAST128_MAC:                    Result := 'CKM_CAST128_MAC';
    CKM_CAST5_MAC_GENERAL: Result       := 'CKM_CAST5_MAC_GENERAL';
    //CKM_CAST128_MAC_GENERAL:            Result := 'CKM_CAST128_MAC_GENERAL';
    CKM_CAST5_CBC_PAD: Result           := 'CKM_CAST5_CBC_PAD';
    //CKM_CAST128_CBC_PAD:                Result := 'CKM_CAST128_CBC_PAD';
    CKM_RC5_KEY_GEN: Result             := 'CKM_RC5_KEY_GEN';
    CKM_RC5_ECB: Result                 := 'CKM_RC5_ECB';
    CKM_RC5_CBC: Result                 := 'CKM_RC5_CBC';
    CKM_RC5_MAC: Result                 := 'CKM_RC5_MAC';
    CKM_RC5_MAC_GENERAL: Result         := 'CKM_RC5_MAC_GENERAL';
    CKM_RC5_CBC_PAD: Result             := 'CKM_RC5_CBC_PAD';
    CKM_IDEA_KEY_GEN: Result            := 'CKM_IDEA_KEY_GEN';
    CKM_IDEA_ECB: Result                := 'CKM_IDEA_ECB';
    CKM_IDEA_CBC: Result                := 'CKM_IDEA_CBC';
    CKM_IDEA_MAC: Result                := 'CKM_IDEA_MAC';
    CKM_IDEA_MAC_GENERAL: Result        := 'CKM_IDEA_MAC_GENERAL';
    CKM_IDEA_CBC_PAD: Result            := 'CKM_IDEA_CBC_PAD';
    CKM_GENERIC_SECRET_KEY_GEN: Result  := 'CKM_GENERIC_SECRET_KEY_GEN';
    CKM_CONCATENATE_BASE_AND_KEY: Result := 'CKM_CONCATENATE_BASE_AND_KEY';
    CKM_CONCATENATE_BASE_AND_DATA: Result := 'CKM_CONCATENATE_BASE_AND_DATA';
    CKM_CONCATENATE_DATA_AND_BASE: Result := 'CKM_CONCATENATE_DATA_AND_BASE';
    CKM_XOR_BASE_AND_DATA: Result       := 'CKM_XOR_BASE_AND_DATA';
    CKM_EXTRACT_KEY_FROM_KEY: Result    := 'CKM_EXTRACT_KEY_FROM_KEY';
    CKM_SSL3_PRE_MASTER_KEY_GEN: Result := 'CKM_SSL3_PRE_MASTER_KEY_GEN';
    CKM_SSL3_MASTER_KEY_DERIVE: Result  := 'CKM_SSL3_MASTER_KEY_DERIVE';
    CKM_SSL3_KEY_AND_MAC_DERIVE: Result := 'CKM_SSL3_KEY_AND_MAC_DERIVE';
    CKM_SSL3_MASTER_KEY_DERIVE_DH: Result := 'CKM_SSL3_MASTER_KEY_DERIVE_DH';
    CKM_TLS_PRE_MASTER_KEY_GEN: Result  := 'CKM_TLS_PRE_MASTER_KEY_GEN';
    CKM_TLS_MASTER_KEY_DERIVE: Result   := 'CKM_TLS_MASTER_KEY_DERIVE';
    CKM_TLS_KEY_AND_MAC_DERIVE: Result  := 'CKM_TLS_KEY_AND_MAC_DERIVE';
    CKM_TLS_MASTER_KEY_DERIVE_DH: Result := 'CKM_TLS_MASTER_KEY_DERIVE_DH';
    CKM_TLS_PRF: Result                 := 'CKM_TLS_PRF';
    CKM_SSL3_MD5_MAC: Result            := 'CKM_SSL3_MD5_MAC';
    CKM_SSL3_SHA1_MAC: Result           := 'CKM_SSL3_SHA1_MAC';
    CKM_MD5_KEY_DERIVATION: Result      := 'CKM_MD5_KEY_DERIVATION';
    CKM_MD2_KEY_DERIVATION: Result      := 'CKM_MD2_KEY_DERIVATION';
    CKM_SHA1_KEY_DERIVATION: Result     := 'CKM_SHA1_KEY_DERIVATION';
    CKM_SHA256_KEY_DERIVATION: Result   := 'CKM_SHA256_KEY_DERIVATION';
    CKM_SHA384_KEY_DERIVATION: Result   := 'CKM_SHA384_KEY_DERIVATION';
    CKM_SHA512_KEY_DERIVATION: Result   := 'CKM_SHA512_KEY_DERIVATION';
    CKM_SHA224_KEY_DERIVATION: Result   := 'CKM_SHA224_KEY_DERIVATION';
    CKM_PBE_MD2_DES_CBC: Result         := 'CKM_PBE_MD2_DES_CBC';
    CKM_PBE_MD5_DES_CBC: Result         := 'CKM_PBE_MD5_DES_CBC';
    CKM_PBE_MD5_CAST_CBC: Result        := 'CKM_PBE_MD5_CAST_CBC';
    CKM_PBE_MD5_CAST3_CBC: Result       := 'CKM_PBE_MD5_CAST3_CBC';
    CKM_PBE_MD5_CAST5_CBC: Result       := 'CKM_PBE_MD5_CAST5_CBC';
    //CKM_PBE_MD5_CAST128_CBC:            Result := 'CKM_PBE_MD5_CAST128_CBC';
    CKM_PBE_SHA1_CAST5_CBC: Result      := 'CKM_PBE_SHA1_CAST5_CBC';
    //CKM_PBE_SHA1_CAST128_CBC:           Result := 'CKM_PBE_SHA1_CAST128_CBC';
    CKM_PBE_SHA1_RC4_128: Result        := 'CKM_PBE_SHA1_RC4_128';
    CKM_PBE_SHA1_RC4_40: Result         := 'CKM_PBE_SHA1_RC4_40';
    CKM_PBE_SHA1_DES3_EDE_CBC: Result   := 'CKM_PBE_SHA1_DES3_EDE_CBC';
    CKM_PBE_SHA1_DES2_EDE_CBC: Result   := 'CKM_PBE_SHA1_DES2_EDE_CBC';
    CKM_PBE_SHA1_RC2_128_CBC: Result    := 'CKM_PBE_SHA1_RC2_128_CBC';
    CKM_PBE_SHA1_RC2_40_CBC: Result     := 'CKM_PBE_SHA1_RC2_40_CBC';
    CKM_PKCS5_PBKD2: Result             := 'CKM_PKCS5_PBKD2';
    CKM_PBA_SHA1_WITH_SHA1_HMAC: Result := 'CKM_PBA_SHA1_WITH_SHA1_HMAC';
    CKM_WTLS_PRE_MASTER_KEY_GEN: Result := 'CKM_WTLS_PRE_MASTER_KEY_GEN';
    CKM_WTLS_MASTER_KEY_DERIVE: Result  := 'CKM_WTLS_MASTER_KEY_DERIVE';
    CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC: Result := 'CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC';
    CKM_WTLS_PRF: Result                := 'CKM_WTLS_PRF';
    CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE: Result := 'CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE';
    CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE: Result := 'CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE';
    CKM_KEY_WRAP_LYNKS: Result          := 'CKM_KEY_WRAP_LYNKS';
    CKM_KEY_WRAP_SET_OAEP: Result       := 'CKM_KEY_WRAP_SET_OAEP';
    CKM_CMS_SIG: Result                 := 'CKM_CMS_SIG';
    CKM_KIP_DERIVE: Result              := 'CKM_KIP_DERIVE';
    CKM_KIP_WRAP: Result                := 'CKM_KIP_WRAP';
    CKM_KIP_MAC: Result                 := 'CKM_KIP_MAC';
    CKM_CAMELLIA_KEY_GEN: Result        := 'CKM_CAMELLIA_KEY_GEN';
    CKM_CAMELLIA_ECB: Result            := 'CKM_CAMELLIA_ECB';
    CKM_CAMELLIA_CBC: Result            := 'CKM_CAMELLIA_CBC';
    CKM_CAMELLIA_MAC: Result            := 'CKM_CAMELLIA_MAC';
    CKM_CAMELLIA_MAC_GENERAL: Result    := 'CKM_CAMELLIA_MAC_GENERAL';
    CKM_CAMELLIA_CBC_PAD: Result        := 'CKM_CAMELLIA_CBC_PAD';
    CKM_CAMELLIA_ECB_ENCRYPT_DATA: Result := 'CKM_CAMELLIA_ECB_ENCRYPT_DATA';
    CKM_CAMELLIA_CBC_ENCRYPT_DATA: Result := 'CKM_CAMELLIA_CBC_ENCRYPT_DATA';
    CKM_CAMELLIA_CTR: Result            := 'CKM_CAMELLIA_CTR';
    CKM_ARIA_KEY_GEN: Result            := 'CKM_ARIA_KEY_GEN';
    CKM_ARIA_ECB: Result                := 'CKM_ARIA_ECB';
    CKM_ARIA_CBC: Result                := 'CKM_ARIA_CBC';
    CKM_ARIA_MAC: Result                := 'CKM_ARIA_MAC';
    CKM_ARIA_MAC_GENERAL: Result        := 'CKM_ARIA_MAC_GENERAL';
    CKM_ARIA_CBC_PAD: Result            := 'CKM_ARIA_CBC_PAD';
    CKM_ARIA_ECB_ENCRYPT_DATA: Result   := 'CKM_ARIA_ECB_ENCRYPT_DATA';
    CKM_ARIA_CBC_ENCRYPT_DATA: Result   := 'CKM_ARIA_CBC_ENCRYPT_DATA';
    CKM_SKIPJACK_KEY_GEN: Result        := 'CKM_SKIPJACK_KEY_GEN';
    CKM_SKIPJACK_ECB64: Result          := 'CKM_SKIPJACK_ECB64';
    CKM_SKIPJACK_CBC64: Result          := 'CKM_SKIPJACK_CBC64';
    CKM_SKIPJACK_OFB64: Result          := 'CKM_SKIPJACK_OFB64';
    CKM_SKIPJACK_CFB64: Result          := 'CKM_SKIPJACK_CFB64';
    CKM_SKIPJACK_CFB32: Result          := 'CKM_SKIPJACK_CFB32';
    CKM_SKIPJACK_CFB16: Result          := 'CKM_SKIPJACK_CFB16';
    CKM_SKIPJACK_CFB8: Result           := 'CKM_SKIPJACK_CFB8';
    CKM_SKIPJACK_WRAP: Result           := 'CKM_SKIPJACK_WRAP';
    CKM_SKIPJACK_PRIVATE_WRAP: Result   := 'CKM_SKIPJACK_PRIVATE_WRAP';
    CKM_SKIPJACK_RELAYX: Result         := 'CKM_SKIPJACK_RELAYX';
    CKM_KEA_KEY_PAIR_GEN: Result        := 'CKM_KEA_KEY_PAIR_GEN';
    CKM_KEA_KEY_DERIVE: Result          := 'CKM_KEA_KEY_DERIVE';
    CKM_FORTEZZA_TIMESTAMP: Result      := 'CKM_FORTEZZA_TIMESTAMP';
    CKM_BATON_KEY_GEN: Result           := 'CKM_BATON_KEY_GEN';
    CKM_BATON_ECB128: Result            := 'CKM_BATON_ECB128';
    CKM_BATON_ECB96: Result             := 'CKM_BATON_ECB96';
    CKM_BATON_CBC128: Result            := 'CKM_BATON_CBC128';
    CKM_BATON_COUNTER: Result           := 'CKM_BATON_COUNTER';
    CKM_BATON_SHUFFLE: Result           := 'CKM_BATON_SHUFFLE';
    CKM_BATON_WRAP: Result              := 'CKM_BATON_WRAP';
    CKM_ECDSA_KEY_PAIR_GEN: Result      := 'CKM_ECDSA_KEY_PAIR_GEN';
    //CKM_EC_KEY_PAIR_GEN:                Result := 'CKM_EC_KEY_PAIR_GEN';
    CKM_ECDSA: Result                   := 'CKM_ECDSA';
    CKM_ECDSA_SHA1: Result              := 'CKM_ECDSA_SHA1';
    CKM_ECDH1_DERIVE: Result            := 'CKM_ECDH1_DERIVE';
    CKM_ECDH1_COFACTOR_DERIVE: Result   := 'CKM_ECDH1_COFACTOR_DERIVE';
    CKM_ECMQV_DERIVE: Result            := 'CKM_ECMQV_DERIVE';
    CKM_JUNIPER_KEY_GEN: Result         := 'CKM_JUNIPER_KEY_GEN';
    CKM_JUNIPER_ECB128: Result          := 'CKM_JUNIPER_ECB128';
    CKM_JUNIPER_CBC128: Result          := 'CKM_JUNIPER_CBC128';
    CKM_JUNIPER_COUNTER: Result         := 'CKM_JUNIPER_COUNTER';
    CKM_JUNIPER_SHUFFLE: Result         := 'CKM_JUNIPER_SHUFFLE';
    CKM_JUNIPER_WRAP: Result            := 'CKM_JUNIPER_WRAP';
    CKM_FASTHASH: Result                := 'CKM_FASTHASH';
    CKM_AES_KEY_GEN: Result             := 'CKM_AES_KEY_GEN';
    CKM_AES_ECB: Result                 := 'CKM_AES_ECB';
    CKM_AES_CBC: Result                 := 'CKM_AES_CBC';
    CKM_AES_MAC: Result                 := 'CKM_AES_MAC';
    CKM_AES_MAC_GENERAL: Result         := 'CKM_AES_MAC_GENERAL';
    CKM_AES_CBC_PAD: Result             := 'CKM_AES_CBC_PAD';
    CKM_AES_CTR: Result                 := 'CKM_AES_CTR';
    CKM_BLOWFISH_KEY_GEN: Result        := 'CKM_BLOWFISH_KEY_GEN';
    CKM_BLOWFISH_CBC: Result            := 'CKM_BLOWFISH_CBC';
    CKM_TWOFISH_KEY_GEN: Result         := 'CKM_TWOFISH_KEY_GEN';
    CKM_TWOFISH_CBC: Result             := 'CKM_TWOFISH_CBC';
    CKM_DES_ECB_ENCRYPT_DATA: Result    := 'CKM_DES_ECB_ENCRYPT_DATA';
    CKM_DES_CBC_ENCRYPT_DATA: Result    := 'CKM_DES_CBC_ENCRYPT_DATA';
    CKM_DES3_ECB_ENCRYPT_DATA: Result   := 'CKM_DES3_ECB_ENCRYPT_DATA';
    CKM_DES3_CBC_ENCRYPT_DATA: Result   := 'CKM_DES3_CBC_ENCRYPT_DATA';
    CKM_AES_ECB_ENCRYPT_DATA: Result    := 'CKM_AES_ECB_ENCRYPT_DATA';
    CKM_AES_CBC_ENCRYPT_DATA: Result    := 'CKM_AES_CBC_ENCRYPT_DATA';
    CKM_DSA_PARAMETER_GEN: Result       := 'CKM_DSA_PARAMETER_GEN';
    CKM_DH_PKCS_PARAMETER_GEN: Result   := 'CKM_DH_PKCS_PARAMETER_GEN';
    CKM_X9_42_DH_PARAMETER_GEN: Result  := 'CKM_X9_42_DH_PARAMETER_GEN';
    CKM_VENDOR_DEFINED: Result          := 'CKM_VENDOR_DEFINED';
  end;

end;

function TPKCS11Mechanism.KeysizeUnits(mechanism: CK_MECHANISM_TYPE): TPKCS11KeysizeUnits;
begin
  Result := ksmNA;

  case mechanism of
    CKM_RSA_PKCS_KEY_PAIR_GEN: Result   := ksmBits;
    CKM_RSA_PKCS: Result                := ksmBits;
    CKM_RSA_9796: Result                := ksmBits;
    CKM_RSA_X_509: Result               := ksmBits;
    CKM_MD2_RSA_PKCS: Result            := ksmBits;
    CKM_MD5_RSA_PKCS: Result            := ksmBits;
    CKM_SHA1_RSA_PKCS: Result           := ksmBits;
    CKM_RIPEMD128_RSA_PKCS: Result      := ksmBits;
    CKM_RIPEMD160_RSA_PKCS: Result      := ksmBits;
    CKM_RSA_PKCS_OAEP: Result           := ksmBits;
    CKM_RSA_X9_31_KEY_PAIR_GEN: Result  := ksmBits;
    CKM_RSA_X9_31: Result               := ksmBits;
    CKM_SHA1_RSA_X9_31: Result          := ksmBits;
    CKM_RSA_PKCS_PSS: Result            := ksmBits;
    CKM_SHA1_RSA_PKCS_PSS: Result       := ksmBits;
    CKM_DSA_KEY_PAIR_GEN: Result        := ksmBits;
    CKM_DSA: Result                     := ksmBits;
    CKM_DSA_SHA1: Result                := ksmBits;
    CKM_DH_PKCS_KEY_PAIR_GEN: Result    := ksmBits;
    CKM_DH_PKCS_DERIVE: Result          := ksmBits;
    CKM_X9_42_DH_KEY_PAIR_GEN: Result   := ksmBits;
    CKM_X9_42_DH_DERIVE: Result         := ksmBits;
    CKM_X9_42_DH_HYBRID_DERIVE: Result  := ksmBits;
    CKM_X9_42_MQV_DERIVE: Result        := ksmBits;
    CKM_SHA256_RSA_PKCS: Result         := ksmBits;
    CKM_SHA384_RSA_PKCS: Result         := ksmBits;
    CKM_SHA512_RSA_PKCS: Result         := ksmBits;
    CKM_SHA256_RSA_PKCS_PSS: Result     := ksmBits;
    CKM_SHA384_RSA_PKCS_PSS: Result     := ksmBits;
    CKM_SHA512_RSA_PKCS_PSS: Result     := ksmBits;
    CKM_SHA224_RSA_PKCS: Result         := ksmBits;
    CKM_SHA224_RSA_PKCS_PSS: Result     := ksmBits;
    CKM_RC2_KEY_GEN: Result             := ksmBits;
    CKM_RC2_ECB: Result                 := ksmBits;
    CKM_RC2_CBC: Result                 := ksmBits;
    CKM_RC2_MAC: Result                 := ksmBits;
    CKM_RC2_MAC_GENERAL: Result         := ksmBits;
    CKM_RC2_CBC_PAD: Result             := ksmBits;
    CKM_RC4_KEY_GEN: Result             := ksmBits;
    CKM_RC4: Result                     := ksmBits;
    CKM_DES_KEY_GEN: Result             := ksmNA;
    CKM_DES_ECB: Result                 := ksmNA;
    CKM_DES_CBC: Result                 := ksmNA;
    CKM_DES_MAC: Result                 := ksmNA;
    CKM_DES_MAC_GENERAL: Result         := ksmNA;
    CKM_DES_CBC_PAD: Result             := ksmNA;
    CKM_DES2_KEY_GEN: Result            := ksmNA;
    CKM_DES3_KEY_GEN: Result            := ksmNA;
    CKM_DES3_ECB: Result                := ksmNA;
    CKM_DES3_CBC: Result                := ksmNA;
    CKM_DES3_MAC: Result                := ksmNA;
    CKM_DES3_MAC_GENERAL: Result        := ksmNA;
    CKM_DES3_CBC_PAD: Result            := ksmNA;
    CKM_CDMF_KEY_GEN: Result            := ksmNA;
    CKM_CDMF_ECB: Result                := ksmNA;
    CKM_CDMF_CBC: Result                := ksmNA;
    CKM_CDMF_MAC: Result                := ksmNA;
    CKM_CDMF_MAC_GENERAL: Result        := ksmNA;
    CKM_CDMF_CBC_PAD: Result            := ksmNA;
    CKM_DES_OFB64: Result               := ksmNA;
    CKM_DES_OFB8: Result                := ksmNA;
    CKM_DES_CFB64: Result               := ksmNA;
    CKM_DES_CFB8: Result                := ksmNA;
    CKM_MD2: Result                     := ksmNA;  // Not documented in spec
    CKM_MD2_HMAC: Result                := ksmNA;  // Not documented in spec
    CKM_MD2_HMAC_GENERAL: Result        := ksmNA;  // Not documented in spec
    CKM_MD5: Result                     := ksmNA;  // Not documented in spec
    CKM_MD5_HMAC: Result                := ksmNA;  // Not documented in spec
    CKM_MD5_HMAC_GENERAL: Result        := ksmNA;  // Not documented in spec
    CKM_SHA_1: Result                   := ksmNA;  // Not documented in spec
    CKM_SHA_1_HMAC: Result              := ksmNA;  // Not documented in spec
    CKM_SHA_1_HMAC_GENERAL: Result      := ksmNA;  // Not documented in spec
    CKM_RIPEMD128: Result               := ksmNA;  // Not documented in spec
    CKM_RIPEMD128_HMAC: Result          := ksmNA;  // Not documented in spec
    CKM_RIPEMD128_HMAC_GENERAL: Result  := ksmNA;  // Not documented in spec
    CKM_RIPEMD160: Result               := ksmNA;  // Not documented in spec
    CKM_RIPEMD160_HMAC: Result          := ksmNA;  // Not documented in spec
    CKM_RIPEMD160_HMAC_GENERAL: Result  := ksmNA;  // Not documented in spec
    CKM_SHA256: Result                  := ksmNA;  // Not documented in spec
    CKM_SHA256_HMAC: Result             := ksmNA;  // Not documented in spec
    CKM_SHA256_HMAC_GENERAL: Result     := ksmNA;  // Not documented in spec
    CKM_SHA224: Result                  := ksmNA;  // Not documented in spec
    CKM_SHA224_HMAC: Result             := ksmNA;  // Not documented in spec
    CKM_SHA224_HMAC_GENERAL: Result     := ksmNA;  // Not documented in spec
    CKM_SHA384: Result                  := ksmNA;  // Not documented in spec
    CKM_SHA384_HMAC: Result             := ksmNA;  // Not documented in spec
    CKM_SHA384_HMAC_GENERAL: Result     := ksmNA;  // Not documented in spec
    CKM_SHA512: Result                  := ksmNA;  // Not documented in spec
    CKM_SHA512_HMAC: Result             := ksmNA;  // Not documented in spec
    CKM_SHA512_HMAC_GENERAL: Result     := ksmNA;  // Not documented in spec
    CKM_SECURID_KEY_GEN: Result         := ksmNA;  // Not documented in spec
    CKM_SECURID: Result                 := ksmNA;  // Not documented in spec
    CKM_HOTP_KEY_GEN: Result            := ksmNA;  // Not documented in spec
    CKM_HOTP: Result                    := ksmNA;  // Not documented in spec
    CKM_ACTI: Result                    := ksmNA;  // Not documented in spec
    CKM_ACTI_KEY_GEN: Result            := ksmNA;  // Not documented in spec
    CKM_CAST_KEY_GEN: Result            := ksmBytes;
    CKM_CAST_ECB: Result                := ksmBytes;
    CKM_CAST_CBC: Result                := ksmBytes;
    CKM_CAST_MAC: Result                := ksmBytes;
    CKM_CAST_MAC_GENERAL: Result        := ksmBytes;
    CKM_CAST_CBC_PAD: Result            := ksmBytes;
    CKM_CAST3_KEY_GEN: Result           := ksmBytes;
    CKM_CAST3_ECB: Result               := ksmBytes;
    CKM_CAST3_CBC: Result               := ksmBytes;
    CKM_CAST3_MAC: Result               := ksmBytes;
    CKM_CAST3_MAC_GENERAL: Result       := ksmBytes;
    CKM_CAST3_CBC_PAD: Result           := ksmBytes;
    CKM_CAST5_KEY_GEN: Result           := ksmBytes;
    //CKM_CAST128_KEY_GEN:                Result := ksmBytes;
    CKM_CAST5_ECB: Result               := ksmBytes;
    //CKM_CAST128_ECB:                    Result := ksmBytes;
    CKM_CAST5_CBC: Result               := ksmBytes;
    //CKM_CAST128_CBC:                    Result := ksmBytes;
    CKM_CAST5_MAC: Result               := ksmBytes;
    //CKM_CAST128_MAC:                    Result := ksmBytes;
    CKM_CAST5_MAC_GENERAL: Result       := ksmBytes;
    //CKM_CAST128_MAC_GENERAL:            Result := ksmBytes;
    CKM_CAST5_CBC_PAD: Result           := ksmBytes;
    //CKM_CAST128_CBC_PAD:                Result := ksmBytes;
    CKM_RC5_KEY_GEN: Result             := ksmBits;
    CKM_RC5_ECB: Result                 := ksmBits;
    CKM_RC5_CBC: Result                 := ksmBits;
    CKM_RC5_MAC: Result                 := ksmBits;
    CKM_RC5_MAC_GENERAL: Result         := ksmBits;
    CKM_RC5_CBC_PAD: Result             := ksmBits;
    CKM_IDEA_KEY_GEN: Result            := ksmNA;
    CKM_IDEA_ECB: Result                := ksmNA;
    CKM_IDEA_CBC: Result                := ksmNA;
    CKM_IDEA_MAC: Result                := ksmNA;
    CKM_IDEA_MAC_GENERAL: Result        := ksmNA;
    CKM_IDEA_CBC_PAD: Result            := ksmNA;
    CKM_GENERIC_SECRET_KEY_GEN: Result  := ksmBits;
    CKM_CONCATENATE_BASE_AND_KEY: Result := ksmNA;   // Not documented in spec
    CKM_CONCATENATE_BASE_AND_DATA: Result := ksmNA;  // Not documented in spec
    CKM_CONCATENATE_DATA_AND_BASE: Result := ksmNA;  // Not documented in spec
    CKM_XOR_BASE_AND_DATA: Result       := ksmNA;    // Not documented in spec
    CKM_EXTRACT_KEY_FROM_KEY: Result    := ksmNA;    // Not documented in spec
    CKM_SSL3_PRE_MASTER_KEY_GEN: Result := ksmBytes;
    CKM_SSL3_MASTER_KEY_DERIVE: Result  := ksmBytes;
    CKM_SSL3_KEY_AND_MAC_DERIVE: Result := ksmNA;  // Not documented in spec
    CKM_SSL3_MASTER_KEY_DERIVE_DH: Result := ksmBytes;
    CKM_TLS_PRE_MASTER_KEY_GEN: Result  := ksmBytes;
    CKM_TLS_MASTER_KEY_DERIVE: Result   := ksmBytes;
    CKM_TLS_KEY_AND_MAC_DERIVE: Result  := ksmNA;  // Not documented in spec
    CKM_TLS_MASTER_KEY_DERIVE_DH: Result := ksmBytes;
    CKM_TLS_PRF: Result                 := ksmNA;  // Not documented in spec
    CKM_SSL3_MD5_MAC: Result            := ksmBits;
    CKM_SSL3_SHA1_MAC: Result           := ksmBits;
    CKM_MD5_KEY_DERIVATION: Result      := ksmNA;  // Not documented in spec
    CKM_MD2_KEY_DERIVATION: Result      := ksmNA;  // Not documented in spec
    CKM_SHA1_KEY_DERIVATION: Result     := ksmNA;  // Not documented in spec
    CKM_SHA256_KEY_DERIVATION: Result   := ksmNA;  // Not documented in spec
    CKM_SHA384_KEY_DERIVATION: Result   := ksmNA;  // Not documented in spec
    CKM_SHA512_KEY_DERIVATION: Result   := ksmNA;  // Not documented in spec
    CKM_SHA224_KEY_DERIVATION: Result   := ksmNA;  // Not documented in spec
    CKM_PBE_MD2_DES_CBC: Result         := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_DES_CBC: Result         := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST_CBC: Result        := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST3_CBC: Result       := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST5_CBC: Result       := ksmNA;  // Not documented in spec
    //CKM_PBE_MD5_CAST128_CBC:            Result := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_CAST5_CBC: Result      := ksmNA;  // Not documented in spec
    //CKM_PBE_SHA1_CAST128_CBC:           Result := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC4_128: Result        := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC4_40: Result         := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_DES3_EDE_CBC: Result   := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_DES2_EDE_CBC: Result   := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC2_128_CBC: Result    := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC2_40_CBC: Result     := ksmNA;  // Not documented in spec
    CKM_PKCS5_PBKD2: Result             := ksmNA;  // Not documented in spec
    CKM_PBA_SHA1_WITH_SHA1_HMAC: Result := ksmNA;  // Not documented in spec
    CKM_WTLS_PRE_MASTER_KEY_GEN: Result := ksmBytes;
    CKM_WTLS_MASTER_KEY_DERIVE: Result  := ksmBytes;
    CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC: Result := ksmBytes;
    CKM_WTLS_PRF: Result                := ksmNA;         // Not documented in spec
    CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE: Result := ksmNA;  // Not documented in spec
    CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE: Result := ksmNA;  // Not documented in spec
    CKM_KEY_WRAP_LYNKS: Result          := ksmNA;         // Not documented in spec
    CKM_KEY_WRAP_SET_OAEP: Result       := ksmNA;         // Not documented in spec
    CKM_CMS_SIG: Result                 := ksmNA;         // Not documented in spec
    CKM_KIP_DERIVE: Result              := ksmNA;         // Not documented in spec
    CKM_KIP_WRAP: Result                := ksmNA;         // Not documented in spec
    CKM_KIP_MAC: Result                 := ksmNA;         // Not documented in spec
    CKM_CAMELLIA_KEY_GEN: Result        := ksmBytes;
    CKM_CAMELLIA_ECB: Result            := ksmBytes;
    CKM_CAMELLIA_CBC: Result            := ksmBytes;
    CKM_CAMELLIA_MAC: Result            := ksmBytes;
    CKM_CAMELLIA_MAC_GENERAL: Result    := ksmBytes;
    CKM_CAMELLIA_CBC_PAD: Result        := ksmBytes;
    CKM_CAMELLIA_ECB_ENCRYPT_DATA: Result := ksmBytes;
    CKM_CAMELLIA_CBC_ENCRYPT_DATA: Result := ksmBytes;
    CKM_CAMELLIA_CTR: Result            := ksmBytes;
    CKM_ARIA_KEY_GEN: Result            := ksmBytes;
    CKM_ARIA_ECB: Result                := ksmBytes;
    CKM_ARIA_CBC: Result                := ksmBytes;
    CKM_ARIA_MAC: Result                := ksmBytes;
    CKM_ARIA_MAC_GENERAL: Result        := ksmBytes;
    CKM_ARIA_CBC_PAD: Result            := ksmBytes;
    CKM_ARIA_ECB_ENCRYPT_DATA: Result   := ksmBytes;
    CKM_ARIA_CBC_ENCRYPT_DATA: Result   := ksmBytes;
    CKM_SKIPJACK_KEY_GEN: Result        := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_ECB64: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CBC64: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_OFB64: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB64: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB32: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB16: Result          := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB8: Result           := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_WRAP: Result           := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_PRIVATE_WRAP: Result   := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_RELAYX: Result         := ksmNA;  // Not documented in spec
    CKM_KEA_KEY_PAIR_GEN: Result        := ksmBits;
    CKM_KEA_KEY_DERIVE: Result          := ksmBits;
    CKM_FORTEZZA_TIMESTAMP: Result      := ksmBits;
    CKM_BATON_KEY_GEN: Result           := ksmNA;  // Not documented in spec
    CKM_BATON_ECB128: Result            := ksmNA;  // Not documented in spec
    CKM_BATON_ECB96: Result             := ksmNA;  // Not documented in spec
    CKM_BATON_CBC128: Result            := ksmNA;  // Not documented in spec
    CKM_BATON_COUNTER: Result           := ksmNA;  // Not documented in spec
    CKM_BATON_SHUFFLE: Result           := ksmNA;  // Not documented in spec
    CKM_BATON_WRAP: Result              := ksmNA;  // Not documented in spec
    CKM_ECDSA_KEY_PAIR_GEN: Result      := ksmBits;
    //CKM_EC_KEY_PAIR_GEN:                Result := ksmBits;
    CKM_ECDSA: Result                   := ksmBits;
    CKM_ECDSA_SHA1: Result              := ksmBits;
    CKM_ECDH1_DERIVE: Result            := ksmBits;
    CKM_ECDH1_COFACTOR_DERIVE: Result   := ksmBits;
    CKM_ECMQV_DERIVE: Result            := ksmBits;
    CKM_JUNIPER_KEY_GEN: Result         := ksmNA;  // Not documented in spec
    CKM_JUNIPER_ECB128: Result          := ksmNA;  // Not documented in spec
    CKM_JUNIPER_CBC128: Result          := ksmNA;  // Not documented in spec
    CKM_JUNIPER_COUNTER: Result         := ksmNA;  // Not documented in spec
    CKM_JUNIPER_SHUFFLE: Result         := ksmNA;  // Not documented in spec
    CKM_JUNIPER_WRAP: Result            := ksmNA;  // Not documented in spec
    CKM_FASTHASH: Result                := ksmNA;  // Not documented in spec
    CKM_AES_KEY_GEN: Result             := ksmBytes;
    CKM_AES_ECB: Result                 := ksmBytes;
    CKM_AES_CBC: Result                 := ksmBytes;
    CKM_AES_MAC: Result                 := ksmBytes;
    CKM_AES_MAC_GENERAL: Result         := ksmBytes;
    CKM_AES_CBC_PAD: Result             := ksmBytes;
    CKM_AES_CTR: Result                 := ksmBytes;
    CKM_BLOWFISH_KEY_GEN: Result        := ksmBytes;
    CKM_BLOWFISH_CBC: Result            := ksmBytes;
    CKM_TWOFISH_KEY_GEN: Result         := ksmBytes;
    // Not documented in spec; assumption based on what Blowfish does
    CKM_TWOFISH_CBC: Result             := ksmBytes;
    // Not documented in spec; assumption based on what Blowfish does
    CKM_DES_ECB_ENCRYPT_DATA: Result    := ksmNA;  // Not documented in spec
    CKM_DES_CBC_ENCRYPT_DATA: Result    := ksmNA;  // Not documented in spec
    CKM_DES3_ECB_ENCRYPT_DATA: Result   := ksmNA;  // Not documented in spec
    CKM_DES3_CBC_ENCRYPT_DATA: Result   := ksmNA;  // Not documented in spec
    CKM_AES_ECB_ENCRYPT_DATA: Result    := ksmBytes;
    CKM_AES_CBC_ENCRYPT_DATA: Result    := ksmBytes;
    CKM_DSA_PARAMETER_GEN: Result       := ksmBits;
    CKM_DH_PKCS_PARAMETER_GEN: Result   := ksmBits;
    CKM_X9_42_DH_PARAMETER_GEN: Result  := ksmBits;
    CKM_VENDOR_DEFINED: Result          := ksmNA;  // Vendor specific
  end;

end;

end.
