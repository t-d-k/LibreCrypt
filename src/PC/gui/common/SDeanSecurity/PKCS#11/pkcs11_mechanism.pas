unit pkcs11_mechanism;

interface

uses
  pkcs11t,
  pkcs11_api;

type
  TPKCS11KeysizeUnits = (ksmBits, ksmBytes, ksmNA);

  TPKCS11Mechanism = class(TPKCS11API)
  private
  protected
    FSlotID: integer;
    FMechanismType: CK_MECHANISM_TYPE;

    function GetFlags(): CK_FLAGS;
    function GetMinKeySize(): cardinal;
    function GetMaxKeySize(): cardinal;
    function GetMinKeySizeInBits(): cardinal;
    function GetMaxKeySizeInBits(): cardinal;

    function GetMechanismTypeStr(): string;

    // Determine whether the ulMinKeySize/ulMaxKeySize fields returned by
    // CK_MECHANISM_INFO are measured in bits or bytes
    function KeysizeUnits(mechanism: CK_MECHANISM_TYPE): TPKCS11KeysizeUnits;

  public
    property SlotID: integer read FSlotID write FSlotID;
    property MechanismType: CK_MECHANISM_TYPE read FMechanismType write FMechanismType;
    property MechanismTypeStr: string read GetMechanismTypeStr;

    // Warning!
    // The values returned by these properties are those returned by
    // the mechanism's CK_MECHANISM_INFO; can be measured bits or bytes - it's
    // mechanism dependant!
    property MinKeySize: cardinal read GetMinKeySize;
    property MaxKeySize: cardinal read GetMaxKeySize;
    // Return the above, but always return the sizes as a number of *bits*
    property MinKeySizeInBits: cardinal read GetMinKeySizeInBits;
    property MaxKeySizeInBits: cardinal read GetMaxKeySizeInBits;
    property Flags: CK_FLAGS read GetFlags;

    function GetMechanismInfo(var info: CK_MECHANISM_INFO): boolean;

    function MechanismToString(mechanism: CK_MECHANISM_TYPE): string;
  end;

  TPKCS11MechanismArray = array of TPKCS11Mechanism;

implementation

uses
  SysUtils,
  SDUGeneral,
  SDUi18n,
  pkcs11f;

function TPKCS11Mechanism.GetMechanismInfo(var info: CK_MECHANISM_INFO): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetMechanismInfo, FN_NAME_C_GetMechanismInfo);

  LastRV := LibraryFunctionList.CK_C_GetMechanismInfo(FSlotID, FMechanismType, @info);
  Result := RVSuccess(LastRV);
end;

function TPKCS11Mechanism.GetFlags(): CK_FLAGS;
var
  retval: CK_FLAGS;
  info: CK_MECHANISM_INFO;
begin
  retval := 0;

  if (GetMechanismInfo(info)) then
    begin
    retval := info.Flags;
    end;

  Result := retval;
end;

function TPKCS11Mechanism.GetMinKeySize(): cardinal;
var
  retval: cardinal;
  info: CK_MECHANISM_INFO;
begin
  retval := 0;

  if (GetMechanismInfo(info)) then
    begin
    retval := info.ulMinKeySize;
    end;

  Result := retval;
end;

function TPKCS11Mechanism.GetMaxKeySize(): cardinal;
var
  retval: cardinal;
  info: CK_MECHANISM_INFO;
begin
  retval := 0;

  if (GetMechanismInfo(info)) then
    begin
    retval := info.ulMaxKeySize;
    end;

  Result := retval;
end;

function TPKCS11Mechanism.GetMinKeySizeInBits(): cardinal;
var
  retval: cardinal;
begin
  retval := GetMinKeySize();
  
  if (KeysizeUnits(MechanismType) = ksmBytes) then
    begin
    retval := retval * 8;
    end;

  Result := retval;
end;

function TPKCS11Mechanism.GetMaxKeySizeInBits(): cardinal;
var
  retval: cardinal;
begin
  retval := GetMaxKeySize();
  
  if (KeysizeUnits(MechanismType) = ksmBytes) then
    begin
    retval := retval * 8;
    end;

  Result := retval;
end;

function TPKCS11Mechanism.GetMechanismTypeStr(): string;
begin
  Result := MechanismToString(FMechanismType);
end;

function TPKCS11Mechanism.MechanismToString(mechanism: CK_MECHANISM_TYPE): string;
var
  retval: string;
begin
  retval:= RS_UNKNOWN + ' (0x'+ inttohex(ord(mechanism), 8)+')';

  case mechanism of
    CKM_RSA_PKCS_KEY_PAIR_GEN:          retval := 'CKM_RSA_PKCS_KEY_PAIR_GEN';
    CKM_RSA_PKCS:                       retval := 'CKM_RSA_PKCS';
    CKM_RSA_9796:                       retval := 'CKM_RSA_9796';
    CKM_RSA_X_509:                      retval := 'CKM_RSA_X_509';
    CKM_MD2_RSA_PKCS:                   retval := 'CKM_MD2_RSA_PKCS';
    CKM_MD5_RSA_PKCS:                   retval := 'CKM_MD5_RSA_PKCS';
    CKM_SHA1_RSA_PKCS:                  retval := 'CKM_SHA1_RSA_PKCS';
    CKM_RIPEMD128_RSA_PKCS:             retval := 'CKM_RIPEMD128_RSA_PKCS';
    CKM_RIPEMD160_RSA_PKCS:             retval := 'CKM_RIPEMD160_RSA_PKCS';
    CKM_RSA_PKCS_OAEP:                  retval := 'CKM_RSA_PKCS_OAEP';
    CKM_RSA_X9_31_KEY_PAIR_GEN:         retval := 'CKM_RSA_X9_31_KEY_PAIR_GEN';
    CKM_RSA_X9_31:                      retval := 'CKM_RSA_X9_31';
    CKM_SHA1_RSA_X9_31:                 retval := 'CKM_SHA1_RSA_X9_31';
    CKM_RSA_PKCS_PSS:                   retval := 'CKM_RSA_PKCS_PSS';
    CKM_SHA1_RSA_PKCS_PSS:              retval := 'CKM_SHA1_RSA_PKCS_PSS';
    CKM_DSA_KEY_PAIR_GEN:               retval := 'CKM_DSA_KEY_PAIR_GEN';
    CKM_DSA:                            retval := 'CKM_DSA';
    CKM_DSA_SHA1:                       retval := 'CKM_DSA_SHA1';
    CKM_DH_PKCS_KEY_PAIR_GEN:           retval := 'CKM_DH_PKCS_KEY_PAIR_GEN';
    CKM_DH_PKCS_DERIVE:                 retval := 'CKM_DH_PKCS_DERIVE';
    CKM_X9_42_DH_KEY_PAIR_GEN:          retval := 'CKM_X9_42_DH_KEY_PAIR_GEN';
    CKM_X9_42_DH_DERIVE:                retval := 'CKM_X9_42_DH_DERIVE';
    CKM_X9_42_DH_HYBRID_DERIVE:         retval := 'CKM_X9_42_DH_HYBRID_DERIVE';
    CKM_X9_42_MQV_DERIVE:               retval := 'CKM_X9_42_MQV_DERIVE';
    CKM_SHA256_RSA_PKCS:                retval := 'CKM_SHA256_RSA_PKCS';
    CKM_SHA384_RSA_PKCS:                retval := 'CKM_SHA384_RSA_PKCS';
    CKM_SHA512_RSA_PKCS:                retval := 'CKM_SHA512_RSA_PKCS';
    CKM_SHA256_RSA_PKCS_PSS:            retval := 'CKM_SHA256_RSA_PKCS_PSS';
    CKM_SHA384_RSA_PKCS_PSS:            retval := 'CKM_SHA384_RSA_PKCS_PSS';
    CKM_SHA512_RSA_PKCS_PSS:            retval := 'CKM_SHA512_RSA_PKCS_PSS';
    CKM_SHA224_RSA_PKCS:                retval := 'CKM_SHA224_RSA_PKCS';
    CKM_SHA224_RSA_PKCS_PSS:            retval := 'CKM_SHA224_RSA_PKCS_PSS';
    CKM_RC2_KEY_GEN:                    retval := 'CKM_RC2_KEY_GEN';
    CKM_RC2_ECB:                        retval := 'CKM_RC2_ECB';
    CKM_RC2_CBC:                        retval := 'CKM_RC2_CBC';
    CKM_RC2_MAC:                        retval := 'CKM_RC2_MAC';
    CKM_RC2_MAC_GENERAL:                retval := 'CKM_RC2_MAC_GENERAL';
    CKM_RC2_CBC_PAD:                    retval := 'CKM_RC2_CBC_PAD';
    CKM_RC4_KEY_GEN:                    retval := 'CKM_RC4_KEY_GEN';
    CKM_RC4:                            retval := 'CKM_RC4';
    CKM_DES_KEY_GEN:                    retval := 'CKM_DES_KEY_GEN';
    CKM_DES_ECB:                        retval := 'CKM_DES_ECB';
    CKM_DES_CBC:                        retval := 'CKM_DES_CBC';
    CKM_DES_MAC:                        retval := 'CKM_DES_MAC';
    CKM_DES_MAC_GENERAL:                retval := 'CKM_DES_MAC_GENERAL';
    CKM_DES_CBC_PAD:                    retval := 'CKM_DES_CBC_PAD';
    CKM_DES2_KEY_GEN:                   retval := 'CKM_DES2_KEY_GEN';
    CKM_DES3_KEY_GEN:                   retval := 'CKM_DES3_KEY_GEN';
    CKM_DES3_ECB:                       retval := 'CKM_DES3_ECB';
    CKM_DES3_CBC:                       retval := 'CKM_DES3_CBC';
    CKM_DES3_MAC:                       retval := 'CKM_DES3_MAC';
    CKM_DES3_MAC_GENERAL:               retval := 'CKM_DES3_MAC_GENERAL';
    CKM_DES3_CBC_PAD:                   retval := 'CKM_DES3_CBC_PAD';
    CKM_CDMF_KEY_GEN:                   retval := 'CKM_CDMF_KEY_GEN';
    CKM_CDMF_ECB:                       retval := 'CKM_CDMF_ECB';
    CKM_CDMF_CBC:                       retval := 'CKM_CDMF_CBC';
    CKM_CDMF_MAC:                       retval := 'CKM_CDMF_MAC';
    CKM_CDMF_MAC_GENERAL:               retval := 'CKM_CDMF_MAC_GENERAL';
    CKM_CDMF_CBC_PAD:                   retval := 'CKM_CDMF_CBC_PAD';
    CKM_DES_OFB64:                      retval := 'CKM_DES_OFB64';
    CKM_DES_OFB8:                       retval := 'CKM_DES_OFB8';
    CKM_DES_CFB64:                      retval := 'CKM_DES_CFB64';
    CKM_DES_CFB8:                       retval := 'CKM_DES_CFB8';
    CKM_MD2:                            retval := 'CKM_MD2';
    CKM_MD2_HMAC:                       retval := 'CKM_MD2_HMAC';
    CKM_MD2_HMAC_GENERAL:               retval := 'CKM_MD2_HMAC_GENERAL';
    CKM_MD5:                            retval := 'CKM_MD5';
    CKM_MD5_HMAC:                       retval := 'CKM_MD5_HMAC';
    CKM_MD5_HMAC_GENERAL:               retval := 'CKM_MD5_HMAC_GENERAL';
    CKM_SHA_1:                          retval := 'CKM_SHA_1';
    CKM_SHA_1_HMAC:                     retval := 'CKM_SHA_1_HMAC';
    CKM_SHA_1_HMAC_GENERAL:             retval := 'CKM_SHA_1_HMAC_GENERAL';
    CKM_RIPEMD128:                      retval := 'CKM_RIPEMD128';
    CKM_RIPEMD128_HMAC:                 retval := 'CKM_RIPEMD128_HMAC';
    CKM_RIPEMD128_HMAC_GENERAL:         retval := 'CKM_RIPEMD128_HMAC_GENERAL';
    CKM_RIPEMD160:                      retval := 'CKM_RIPEMD160';
    CKM_RIPEMD160_HMAC:                 retval := 'CKM_RIPEMD160_HMAC';
    CKM_RIPEMD160_HMAC_GENERAL:         retval := 'CKM_RIPEMD160_HMAC_GENERAL';
    CKM_SHA256:                         retval := 'CKM_SHA256';
    CKM_SHA256_HMAC:                    retval := 'CKM_SHA256_HMAC';
    CKM_SHA256_HMAC_GENERAL:            retval := 'CKM_SHA256_HMAC_GENERAL';
    CKM_SHA224:                         retval := 'CKM_SHA224';
    CKM_SHA224_HMAC:                    retval := 'CKM_SHA224_HMAC';
    CKM_SHA224_HMAC_GENERAL:            retval := 'CKM_SHA224_HMAC_GENERAL';
    CKM_SHA384:                         retval := 'CKM_SHA384';
    CKM_SHA384_HMAC:                    retval := 'CKM_SHA384_HMAC';
    CKM_SHA384_HMAC_GENERAL:            retval := 'CKM_SHA384_HMAC_GENERAL';
    CKM_SHA512:                         retval := 'CKM_SHA512';
    CKM_SHA512_HMAC:                    retval := 'CKM_SHA512_HMAC';
    CKM_SHA512_HMAC_GENERAL:            retval := 'CKM_SHA512_HMAC_GENERAL';
    CKM_SECURID_KEY_GEN:                retval := 'CKM_SECURID_KEY_GEN';
    CKM_SECURID:                        retval := 'CKM_SECURID';
    CKM_HOTP_KEY_GEN:                   retval := 'CKM_HOTP_KEY_GEN';
    CKM_HOTP:                           retval := 'CKM_HOTP';
    CKM_ACTI:                           retval := 'CKM_ACTI';
    CKM_ACTI_KEY_GEN:                   retval := 'CKM_ACTI_KEY_GEN';
    CKM_CAST_KEY_GEN:                   retval := 'CKM_CAST_KEY_GEN';
    CKM_CAST_ECB:                       retval := 'CKM_CAST_ECB';
    CKM_CAST_CBC:                       retval := 'CKM_CAST_CBC';
    CKM_CAST_MAC:                       retval := 'CKM_CAST_MAC';
    CKM_CAST_MAC_GENERAL:               retval := 'CKM_CAST_MAC_GENERAL';
    CKM_CAST_CBC_PAD:                   retval := 'CKM_CAST_CBC_PAD';
    CKM_CAST3_KEY_GEN:                  retval := 'CKM_CAST3_KEY_GEN';
    CKM_CAST3_ECB:                      retval := 'CKM_CAST3_ECB';
    CKM_CAST3_CBC:                      retval := 'CKM_CAST3_CBC';
    CKM_CAST3_MAC:                      retval := 'CKM_CAST3_MAC';
    CKM_CAST3_MAC_GENERAL:              retval := 'CKM_CAST3_MAC_GENERAL';
    CKM_CAST3_CBC_PAD:                  retval := 'CKM_CAST3_CBC_PAD';
    CKM_CAST5_KEY_GEN:                  retval := 'CKM_CAST5_KEY_GEN';
    //CKM_CAST128_KEY_GEN:                retval := 'CKM_CAST128_KEY_GEN';
    CKM_CAST5_ECB:                      retval := 'CKM_CAST5_ECB';
    //CKM_CAST128_ECB:                    retval := 'CKM_CAST128_ECB';
    CKM_CAST5_CBC:                      retval := 'CKM_CAST5_CBC';
    //CKM_CAST128_CBC:                    retval := 'CKM_CAST128_CBC';
    CKM_CAST5_MAC:                      retval := 'CKM_CAST5_MAC';
    //CKM_CAST128_MAC:                    retval := 'CKM_CAST128_MAC';
    CKM_CAST5_MAC_GENERAL:              retval := 'CKM_CAST5_MAC_GENERAL';
    //CKM_CAST128_MAC_GENERAL:            retval := 'CKM_CAST128_MAC_GENERAL';
    CKM_CAST5_CBC_PAD:                  retval := 'CKM_CAST5_CBC_PAD';
    //CKM_CAST128_CBC_PAD:                retval := 'CKM_CAST128_CBC_PAD';
    CKM_RC5_KEY_GEN:                    retval := 'CKM_RC5_KEY_GEN';
    CKM_RC5_ECB:                        retval := 'CKM_RC5_ECB';
    CKM_RC5_CBC:                        retval := 'CKM_RC5_CBC';
    CKM_RC5_MAC:                        retval := 'CKM_RC5_MAC';
    CKM_RC5_MAC_GENERAL:                retval := 'CKM_RC5_MAC_GENERAL';
    CKM_RC5_CBC_PAD:                    retval := 'CKM_RC5_CBC_PAD';
    CKM_IDEA_KEY_GEN:                   retval := 'CKM_IDEA_KEY_GEN';
    CKM_IDEA_ECB:                       retval := 'CKM_IDEA_ECB';
    CKM_IDEA_CBC:                       retval := 'CKM_IDEA_CBC';
    CKM_IDEA_MAC:                       retval := 'CKM_IDEA_MAC';
    CKM_IDEA_MAC_GENERAL:               retval := 'CKM_IDEA_MAC_GENERAL';
    CKM_IDEA_CBC_PAD:                   retval := 'CKM_IDEA_CBC_PAD';
    CKM_GENERIC_SECRET_KEY_GEN:         retval := 'CKM_GENERIC_SECRET_KEY_GEN';
    CKM_CONCATENATE_BASE_AND_KEY:       retval := 'CKM_CONCATENATE_BASE_AND_KEY';
    CKM_CONCATENATE_BASE_AND_DATA:      retval := 'CKM_CONCATENATE_BASE_AND_DATA';
    CKM_CONCATENATE_DATA_AND_BASE:      retval := 'CKM_CONCATENATE_DATA_AND_BASE';
    CKM_XOR_BASE_AND_DATA:              retval := 'CKM_XOR_BASE_AND_DATA';
    CKM_EXTRACT_KEY_FROM_KEY:           retval := 'CKM_EXTRACT_KEY_FROM_KEY';
    CKM_SSL3_PRE_MASTER_KEY_GEN:        retval := 'CKM_SSL3_PRE_MASTER_KEY_GEN';
    CKM_SSL3_MASTER_KEY_DERIVE:         retval := 'CKM_SSL3_MASTER_KEY_DERIVE';
    CKM_SSL3_KEY_AND_MAC_DERIVE:        retval := 'CKM_SSL3_KEY_AND_MAC_DERIVE';
    CKM_SSL3_MASTER_KEY_DERIVE_DH:      retval := 'CKM_SSL3_MASTER_KEY_DERIVE_DH';
    CKM_TLS_PRE_MASTER_KEY_GEN:         retval := 'CKM_TLS_PRE_MASTER_KEY_GEN';
    CKM_TLS_MASTER_KEY_DERIVE:          retval := 'CKM_TLS_MASTER_KEY_DERIVE';
    CKM_TLS_KEY_AND_MAC_DERIVE:         retval := 'CKM_TLS_KEY_AND_MAC_DERIVE';
    CKM_TLS_MASTER_KEY_DERIVE_DH:       retval := 'CKM_TLS_MASTER_KEY_DERIVE_DH';
    CKM_TLS_PRF:                        retval := 'CKM_TLS_PRF';
    CKM_SSL3_MD5_MAC:                   retval := 'CKM_SSL3_MD5_MAC';
    CKM_SSL3_SHA1_MAC:                  retval := 'CKM_SSL3_SHA1_MAC';
    CKM_MD5_KEY_DERIVATION:             retval := 'CKM_MD5_KEY_DERIVATION';
    CKM_MD2_KEY_DERIVATION:             retval := 'CKM_MD2_KEY_DERIVATION';
    CKM_SHA1_KEY_DERIVATION:            retval := 'CKM_SHA1_KEY_DERIVATION';
    CKM_SHA256_KEY_DERIVATION:          retval := 'CKM_SHA256_KEY_DERIVATION';
    CKM_SHA384_KEY_DERIVATION:          retval := 'CKM_SHA384_KEY_DERIVATION';
    CKM_SHA512_KEY_DERIVATION:          retval := 'CKM_SHA512_KEY_DERIVATION';
    CKM_SHA224_KEY_DERIVATION:          retval := 'CKM_SHA224_KEY_DERIVATION';
    CKM_PBE_MD2_DES_CBC:                retval := 'CKM_PBE_MD2_DES_CBC';
    CKM_PBE_MD5_DES_CBC:                retval := 'CKM_PBE_MD5_DES_CBC';
    CKM_PBE_MD5_CAST_CBC:               retval := 'CKM_PBE_MD5_CAST_CBC';
    CKM_PBE_MD5_CAST3_CBC:              retval := 'CKM_PBE_MD5_CAST3_CBC';
    CKM_PBE_MD5_CAST5_CBC:              retval := 'CKM_PBE_MD5_CAST5_CBC';
    //CKM_PBE_MD5_CAST128_CBC:            retval := 'CKM_PBE_MD5_CAST128_CBC';
    CKM_PBE_SHA1_CAST5_CBC:             retval := 'CKM_PBE_SHA1_CAST5_CBC';
    //CKM_PBE_SHA1_CAST128_CBC:           retval := 'CKM_PBE_SHA1_CAST128_CBC';
    CKM_PBE_SHA1_RC4_128:               retval := 'CKM_PBE_SHA1_RC4_128';
    CKM_PBE_SHA1_RC4_40:                retval := 'CKM_PBE_SHA1_RC4_40';
    CKM_PBE_SHA1_DES3_EDE_CBC:          retval := 'CKM_PBE_SHA1_DES3_EDE_CBC';
    CKM_PBE_SHA1_DES2_EDE_CBC:          retval := 'CKM_PBE_SHA1_DES2_EDE_CBC';
    CKM_PBE_SHA1_RC2_128_CBC:           retval := 'CKM_PBE_SHA1_RC2_128_CBC';
    CKM_PBE_SHA1_RC2_40_CBC:            retval := 'CKM_PBE_SHA1_RC2_40_CBC';
    CKM_PKCS5_PBKD2:                    retval := 'CKM_PKCS5_PBKD2';
    CKM_PBA_SHA1_WITH_SHA1_HMAC:        retval := 'CKM_PBA_SHA1_WITH_SHA1_HMAC';
    CKM_WTLS_PRE_MASTER_KEY_GEN:        retval := 'CKM_WTLS_PRE_MASTER_KEY_GEN';
    CKM_WTLS_MASTER_KEY_DERIVE:         retval := 'CKM_WTLS_MASTER_KEY_DERIVE';
    CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC:  retval := 'CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC';
    CKM_WTLS_PRF:                       retval := 'CKM_WTLS_PRF';
    CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE: retval := 'CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE';
    CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE: retval := 'CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE';
    CKM_KEY_WRAP_LYNKS:                 retval := 'CKM_KEY_WRAP_LYNKS';
    CKM_KEY_WRAP_SET_OAEP:              retval := 'CKM_KEY_WRAP_SET_OAEP';
    CKM_CMS_SIG:                        retval := 'CKM_CMS_SIG';
    CKM_KIP_DERIVE:                     retval := 'CKM_KIP_DERIVE';
    CKM_KIP_WRAP:                       retval := 'CKM_KIP_WRAP';
    CKM_KIP_MAC:                        retval := 'CKM_KIP_MAC';
    CKM_CAMELLIA_KEY_GEN:               retval := 'CKM_CAMELLIA_KEY_GEN';
    CKM_CAMELLIA_ECB:                   retval := 'CKM_CAMELLIA_ECB';
    CKM_CAMELLIA_CBC:                   retval := 'CKM_CAMELLIA_CBC';
    CKM_CAMELLIA_MAC:                   retval := 'CKM_CAMELLIA_MAC';
    CKM_CAMELLIA_MAC_GENERAL:           retval := 'CKM_CAMELLIA_MAC_GENERAL';
    CKM_CAMELLIA_CBC_PAD:               retval := 'CKM_CAMELLIA_CBC_PAD';
    CKM_CAMELLIA_ECB_ENCRYPT_DATA:      retval := 'CKM_CAMELLIA_ECB_ENCRYPT_DATA';
    CKM_CAMELLIA_CBC_ENCRYPT_DATA:      retval := 'CKM_CAMELLIA_CBC_ENCRYPT_DATA';
    CKM_CAMELLIA_CTR:                   retval := 'CKM_CAMELLIA_CTR';
    CKM_ARIA_KEY_GEN:                   retval := 'CKM_ARIA_KEY_GEN';
    CKM_ARIA_ECB:                       retval := 'CKM_ARIA_ECB';
    CKM_ARIA_CBC:                       retval := 'CKM_ARIA_CBC';
    CKM_ARIA_MAC:                       retval := 'CKM_ARIA_MAC';
    CKM_ARIA_MAC_GENERAL:               retval := 'CKM_ARIA_MAC_GENERAL';
    CKM_ARIA_CBC_PAD:                   retval := 'CKM_ARIA_CBC_PAD';
    CKM_ARIA_ECB_ENCRYPT_DATA:          retval := 'CKM_ARIA_ECB_ENCRYPT_DATA';
    CKM_ARIA_CBC_ENCRYPT_DATA:          retval := 'CKM_ARIA_CBC_ENCRYPT_DATA';
    CKM_SKIPJACK_KEY_GEN:               retval := 'CKM_SKIPJACK_KEY_GEN';
    CKM_SKIPJACK_ECB64:                 retval := 'CKM_SKIPJACK_ECB64';
    CKM_SKIPJACK_CBC64:                 retval := 'CKM_SKIPJACK_CBC64';
    CKM_SKIPJACK_OFB64:                 retval := 'CKM_SKIPJACK_OFB64';
    CKM_SKIPJACK_CFB64:                 retval := 'CKM_SKIPJACK_CFB64';
    CKM_SKIPJACK_CFB32:                 retval := 'CKM_SKIPJACK_CFB32';
    CKM_SKIPJACK_CFB16:                 retval := 'CKM_SKIPJACK_CFB16';
    CKM_SKIPJACK_CFB8:                  retval := 'CKM_SKIPJACK_CFB8';
    CKM_SKIPJACK_WRAP:                  retval := 'CKM_SKIPJACK_WRAP';
    CKM_SKIPJACK_PRIVATE_WRAP:          retval := 'CKM_SKIPJACK_PRIVATE_WRAP';
    CKM_SKIPJACK_RELAYX:                retval := 'CKM_SKIPJACK_RELAYX';
    CKM_KEA_KEY_PAIR_GEN:               retval := 'CKM_KEA_KEY_PAIR_GEN';
    CKM_KEA_KEY_DERIVE:                 retval := 'CKM_KEA_KEY_DERIVE';
    CKM_FORTEZZA_TIMESTAMP:             retval := 'CKM_FORTEZZA_TIMESTAMP';
    CKM_BATON_KEY_GEN:                  retval := 'CKM_BATON_KEY_GEN';
    CKM_BATON_ECB128:                   retval := 'CKM_BATON_ECB128';
    CKM_BATON_ECB96:                    retval := 'CKM_BATON_ECB96';
    CKM_BATON_CBC128:                   retval := 'CKM_BATON_CBC128';
    CKM_BATON_COUNTER:                  retval := 'CKM_BATON_COUNTER';
    CKM_BATON_SHUFFLE:                  retval := 'CKM_BATON_SHUFFLE';
    CKM_BATON_WRAP:                     retval := 'CKM_BATON_WRAP';
    CKM_ECDSA_KEY_PAIR_GEN:             retval := 'CKM_ECDSA_KEY_PAIR_GEN';
    //CKM_EC_KEY_PAIR_GEN:                retval := 'CKM_EC_KEY_PAIR_GEN';
    CKM_ECDSA:                          retval := 'CKM_ECDSA';
    CKM_ECDSA_SHA1:                     retval := 'CKM_ECDSA_SHA1';
    CKM_ECDH1_DERIVE:                   retval := 'CKM_ECDH1_DERIVE';
    CKM_ECDH1_COFACTOR_DERIVE:          retval := 'CKM_ECDH1_COFACTOR_DERIVE';
    CKM_ECMQV_DERIVE:                   retval := 'CKM_ECMQV_DERIVE';
    CKM_JUNIPER_KEY_GEN:                retval := 'CKM_JUNIPER_KEY_GEN';
    CKM_JUNIPER_ECB128:                 retval := 'CKM_JUNIPER_ECB128';
    CKM_JUNIPER_CBC128:                 retval := 'CKM_JUNIPER_CBC128';
    CKM_JUNIPER_COUNTER:                retval := 'CKM_JUNIPER_COUNTER';
    CKM_JUNIPER_SHUFFLE:                retval := 'CKM_JUNIPER_SHUFFLE';
    CKM_JUNIPER_WRAP:                   retval := 'CKM_JUNIPER_WRAP';
    CKM_FASTHASH:                       retval := 'CKM_FASTHASH';
    CKM_AES_KEY_GEN:                    retval := 'CKM_AES_KEY_GEN';
    CKM_AES_ECB:                        retval := 'CKM_AES_ECB';
    CKM_AES_CBC:                        retval := 'CKM_AES_CBC';
    CKM_AES_MAC:                        retval := 'CKM_AES_MAC';
    CKM_AES_MAC_GENERAL:                retval := 'CKM_AES_MAC_GENERAL';
    CKM_AES_CBC_PAD:                    retval := 'CKM_AES_CBC_PAD';
    CKM_AES_CTR:                        retval := 'CKM_AES_CTR';
    CKM_BLOWFISH_KEY_GEN:               retval := 'CKM_BLOWFISH_KEY_GEN';
    CKM_BLOWFISH_CBC:                   retval := 'CKM_BLOWFISH_CBC';
    CKM_TWOFISH_KEY_GEN:                retval := 'CKM_TWOFISH_KEY_GEN';
    CKM_TWOFISH_CBC:                    retval := 'CKM_TWOFISH_CBC';
    CKM_DES_ECB_ENCRYPT_DATA:           retval := 'CKM_DES_ECB_ENCRYPT_DATA';
    CKM_DES_CBC_ENCRYPT_DATA:           retval := 'CKM_DES_CBC_ENCRYPT_DATA';
    CKM_DES3_ECB_ENCRYPT_DATA:          retval := 'CKM_DES3_ECB_ENCRYPT_DATA';
    CKM_DES3_CBC_ENCRYPT_DATA:          retval := 'CKM_DES3_CBC_ENCRYPT_DATA';
    CKM_AES_ECB_ENCRYPT_DATA:           retval := 'CKM_AES_ECB_ENCRYPT_DATA';
    CKM_AES_CBC_ENCRYPT_DATA:           retval := 'CKM_AES_CBC_ENCRYPT_DATA';
    CKM_DSA_PARAMETER_GEN:              retval := 'CKM_DSA_PARAMETER_GEN';
    CKM_DH_PKCS_PARAMETER_GEN:          retval := 'CKM_DH_PKCS_PARAMETER_GEN';
    CKM_X9_42_DH_PARAMETER_GEN:         retval := 'CKM_X9_42_DH_PARAMETER_GEN';
    CKM_VENDOR_DEFINED:                 retval := 'CKM_VENDOR_DEFINED';
  end;

  Result := retval;
end;

function TPKCS11Mechanism.KeysizeUnits(mechanism: CK_MECHANISM_TYPE): TPKCS11KeysizeUnits;
var
  retval: TPKCS11KeysizeUnits;
begin
  retval:= ksmNA;

  case mechanism of                        
    CKM_RSA_PKCS_KEY_PAIR_GEN:          retval := ksmBits;
    CKM_RSA_PKCS:                       retval := ksmBits;
    CKM_RSA_9796:                       retval := ksmBits;
    CKM_RSA_X_509:                      retval := ksmBits;
    CKM_MD2_RSA_PKCS:                   retval := ksmBits;
    CKM_MD5_RSA_PKCS:                   retval := ksmBits;
    CKM_SHA1_RSA_PKCS:                  retval := ksmBits;
    CKM_RIPEMD128_RSA_PKCS:             retval := ksmBits;
    CKM_RIPEMD160_RSA_PKCS:             retval := ksmBits;
    CKM_RSA_PKCS_OAEP:                  retval := ksmBits;
    CKM_RSA_X9_31_KEY_PAIR_GEN:         retval := ksmBits;
    CKM_RSA_X9_31:                      retval := ksmBits;
    CKM_SHA1_RSA_X9_31:                 retval := ksmBits;
    CKM_RSA_PKCS_PSS:                   retval := ksmBits;
    CKM_SHA1_RSA_PKCS_PSS:              retval := ksmBits;
    CKM_DSA_KEY_PAIR_GEN:               retval := ksmBits;
    CKM_DSA:                            retval := ksmBits;
    CKM_DSA_SHA1:                       retval := ksmBits;
    CKM_DH_PKCS_KEY_PAIR_GEN:           retval := ksmBits;
    CKM_DH_PKCS_DERIVE:                 retval := ksmBits;
    CKM_X9_42_DH_KEY_PAIR_GEN:          retval := ksmBits;
    CKM_X9_42_DH_DERIVE:                retval := ksmBits;
    CKM_X9_42_DH_HYBRID_DERIVE:         retval := ksmBits;
    CKM_X9_42_MQV_DERIVE:               retval := ksmBits;
    CKM_SHA256_RSA_PKCS:                retval := ksmBits;
    CKM_SHA384_RSA_PKCS:                retval := ksmBits;
    CKM_SHA512_RSA_PKCS:                retval := ksmBits;
    CKM_SHA256_RSA_PKCS_PSS:            retval := ksmBits;
    CKM_SHA384_RSA_PKCS_PSS:            retval := ksmBits;
    CKM_SHA512_RSA_PKCS_PSS:            retval := ksmBits;
    CKM_SHA224_RSA_PKCS:                retval := ksmBits;
    CKM_SHA224_RSA_PKCS_PSS:            retval := ksmBits;
    CKM_RC2_KEY_GEN:                    retval := ksmBits;
    CKM_RC2_ECB:                        retval := ksmBits;
    CKM_RC2_CBC:                        retval := ksmBits;
    CKM_RC2_MAC:                        retval := ksmBits;
    CKM_RC2_MAC_GENERAL:                retval := ksmBits;
    CKM_RC2_CBC_PAD:                    retval := ksmBits;
    CKM_RC4_KEY_GEN:                    retval := ksmBits;
    CKM_RC4:                            retval := ksmBits;
    CKM_DES_KEY_GEN:                    retval := ksmNA;
    CKM_DES_ECB:                        retval := ksmNA;
    CKM_DES_CBC:                        retval := ksmNA;
    CKM_DES_MAC:                        retval := ksmNA;
    CKM_DES_MAC_GENERAL:                retval := ksmNA;
    CKM_DES_CBC_PAD:                    retval := ksmNA;
    CKM_DES2_KEY_GEN:                   retval := ksmNA;
    CKM_DES3_KEY_GEN:                   retval := ksmNA;
    CKM_DES3_ECB:                       retval := ksmNA;
    CKM_DES3_CBC:                       retval := ksmNA;
    CKM_DES3_MAC:                       retval := ksmNA;
    CKM_DES3_MAC_GENERAL:               retval := ksmNA;
    CKM_DES3_CBC_PAD:                   retval := ksmNA;
    CKM_CDMF_KEY_GEN:                   retval := ksmNA;
    CKM_CDMF_ECB:                       retval := ksmNA;
    CKM_CDMF_CBC:                       retval := ksmNA;
    CKM_CDMF_MAC:                       retval := ksmNA;
    CKM_CDMF_MAC_GENERAL:               retval := ksmNA;
    CKM_CDMF_CBC_PAD:                   retval := ksmNA;
    CKM_DES_OFB64:                      retval := ksmNA;
    CKM_DES_OFB8:                       retval := ksmNA;
    CKM_DES_CFB64:                      retval := ksmNA;
    CKM_DES_CFB8:                       retval := ksmNA;
    CKM_MD2:                            retval := ksmNA;  // Not documented in spec
    CKM_MD2_HMAC:                       retval := ksmNA;  // Not documented in spec
    CKM_MD2_HMAC_GENERAL:               retval := ksmNA;  // Not documented in spec
    CKM_MD5:                            retval := ksmNA;  // Not documented in spec
    CKM_MD5_HMAC:                       retval := ksmNA;  // Not documented in spec
    CKM_MD5_HMAC_GENERAL:               retval := ksmNA;  // Not documented in spec
    CKM_SHA_1:                          retval := ksmNA;  // Not documented in spec
    CKM_SHA_1_HMAC:                     retval := ksmNA;  // Not documented in spec
    CKM_SHA_1_HMAC_GENERAL:             retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD128:                      retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD128_HMAC:                 retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD128_HMAC_GENERAL:         retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD160:                      retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD160_HMAC:                 retval := ksmNA;  // Not documented in spec
    CKM_RIPEMD160_HMAC_GENERAL:         retval := ksmNA;  // Not documented in spec
    CKM_SHA256:                         retval := ksmNA;  // Not documented in spec
    CKM_SHA256_HMAC:                    retval := ksmNA;  // Not documented in spec
    CKM_SHA256_HMAC_GENERAL:            retval := ksmNA;  // Not documented in spec
    CKM_SHA224:                         retval := ksmNA;  // Not documented in spec
    CKM_SHA224_HMAC:                    retval := ksmNA;  // Not documented in spec
    CKM_SHA224_HMAC_GENERAL:            retval := ksmNA;  // Not documented in spec
    CKM_SHA384:                         retval := ksmNA;  // Not documented in spec
    CKM_SHA384_HMAC:                    retval := ksmNA;  // Not documented in spec
    CKM_SHA384_HMAC_GENERAL:            retval := ksmNA;  // Not documented in spec
    CKM_SHA512:                         retval := ksmNA;  // Not documented in spec
    CKM_SHA512_HMAC:                    retval := ksmNA;  // Not documented in spec
    CKM_SHA512_HMAC_GENERAL:            retval := ksmNA;  // Not documented in spec
    CKM_SECURID_KEY_GEN:                retval := ksmNA;  // Not documented in spec
    CKM_SECURID:                        retval := ksmNA;  // Not documented in spec
    CKM_HOTP_KEY_GEN:                   retval := ksmNA;  // Not documented in spec
    CKM_HOTP:                           retval := ksmNA;  // Not documented in spec
    CKM_ACTI:                           retval := ksmNA;  // Not documented in spec
    CKM_ACTI_KEY_GEN:                   retval := ksmNA;  // Not documented in spec
    CKM_CAST_KEY_GEN:                   retval := ksmBytes;
    CKM_CAST_ECB:                       retval := ksmBytes;
    CKM_CAST_CBC:                       retval := ksmBytes;
    CKM_CAST_MAC:                       retval := ksmBytes;
    CKM_CAST_MAC_GENERAL:               retval := ksmBytes;
    CKM_CAST_CBC_PAD:                   retval := ksmBytes;
    CKM_CAST3_KEY_GEN:                  retval := ksmBytes;
    CKM_CAST3_ECB:                      retval := ksmBytes;
    CKM_CAST3_CBC:                      retval := ksmBytes;
    CKM_CAST3_MAC:                      retval := ksmBytes;
    CKM_CAST3_MAC_GENERAL:              retval := ksmBytes;
    CKM_CAST3_CBC_PAD:                  retval := ksmBytes;
    CKM_CAST5_KEY_GEN:                  retval := ksmBytes;
    //CKM_CAST128_KEY_GEN:                retval := ksmBytes;
    CKM_CAST5_ECB:                      retval := ksmBytes;
    //CKM_CAST128_ECB:                    retval := ksmBytes;
    CKM_CAST5_CBC:                      retval := ksmBytes;
    //CKM_CAST128_CBC:                    retval := ksmBytes;
    CKM_CAST5_MAC:                      retval := ksmBytes;
    //CKM_CAST128_MAC:                    retval := ksmBytes;
    CKM_CAST5_MAC_GENERAL:              retval := ksmBytes;
    //CKM_CAST128_MAC_GENERAL:            retval := ksmBytes;
    CKM_CAST5_CBC_PAD:                  retval := ksmBytes;
    //CKM_CAST128_CBC_PAD:                retval := ksmBytes;
    CKM_RC5_KEY_GEN:                    retval := ksmBits;
    CKM_RC5_ECB:                        retval := ksmBits;
    CKM_RC5_CBC:                        retval := ksmBits;
    CKM_RC5_MAC:                        retval := ksmBits;
    CKM_RC5_MAC_GENERAL:                retval := ksmBits;
    CKM_RC5_CBC_PAD:                    retval := ksmBits;
    CKM_IDEA_KEY_GEN:                   retval := ksmNA;
    CKM_IDEA_ECB:                       retval := ksmNA;
    CKM_IDEA_CBC:                       retval := ksmNA;
    CKM_IDEA_MAC:                       retval := ksmNA;
    CKM_IDEA_MAC_GENERAL:               retval := ksmNA;
    CKM_IDEA_CBC_PAD:                   retval := ksmNA;
    CKM_GENERIC_SECRET_KEY_GEN:         retval := ksmBits;
    CKM_CONCATENATE_BASE_AND_KEY:       retval := ksmNA;  // Not documented in spec
    CKM_CONCATENATE_BASE_AND_DATA:      retval := ksmNA;  // Not documented in spec
    CKM_CONCATENATE_DATA_AND_BASE:      retval := ksmNA;  // Not documented in spec
    CKM_XOR_BASE_AND_DATA:              retval := ksmNA;  // Not documented in spec
    CKM_EXTRACT_KEY_FROM_KEY:           retval := ksmNA;  // Not documented in spec
    CKM_SSL3_PRE_MASTER_KEY_GEN:        retval := ksmBytes;
    CKM_SSL3_MASTER_KEY_DERIVE:         retval := ksmBytes;
    CKM_SSL3_KEY_AND_MAC_DERIVE:        retval := ksmNA;  // Not documented in spec
    CKM_SSL3_MASTER_KEY_DERIVE_DH:      retval := ksmBytes;
    CKM_TLS_PRE_MASTER_KEY_GEN:         retval := ksmBytes;
    CKM_TLS_MASTER_KEY_DERIVE:          retval := ksmBytes;
    CKM_TLS_KEY_AND_MAC_DERIVE:         retval := ksmNA;  // Not documented in spec
    CKM_TLS_MASTER_KEY_DERIVE_DH:       retval := ksmBytes;
    CKM_TLS_PRF:                        retval := ksmNA;  // Not documented in spec
    CKM_SSL3_MD5_MAC:                   retval := ksmBits;
    CKM_SSL3_SHA1_MAC:                  retval := ksmBits;
    CKM_MD5_KEY_DERIVATION:             retval := ksmNA;  // Not documented in spec
    CKM_MD2_KEY_DERIVATION:             retval := ksmNA;  // Not documented in spec
    CKM_SHA1_KEY_DERIVATION:            retval := ksmNA;  // Not documented in spec
    CKM_SHA256_KEY_DERIVATION:          retval := ksmNA;  // Not documented in spec
    CKM_SHA384_KEY_DERIVATION:          retval := ksmNA;  // Not documented in spec
    CKM_SHA512_KEY_DERIVATION:          retval := ksmNA;  // Not documented in spec
    CKM_SHA224_KEY_DERIVATION:          retval := ksmNA;  // Not documented in spec
    CKM_PBE_MD2_DES_CBC:                retval := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_DES_CBC:                retval := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST_CBC:               retval := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST3_CBC:              retval := ksmNA;  // Not documented in spec
    CKM_PBE_MD5_CAST5_CBC:              retval := ksmNA;  // Not documented in spec
    //CKM_PBE_MD5_CAST128_CBC:            retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_CAST5_CBC:             retval := ksmNA;  // Not documented in spec
    //CKM_PBE_SHA1_CAST128_CBC:           retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC4_128:               retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC4_40:                retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_DES3_EDE_CBC:          retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_DES2_EDE_CBC:          retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC2_128_CBC:           retval := ksmNA;  // Not documented in spec
    CKM_PBE_SHA1_RC2_40_CBC:            retval := ksmNA;  // Not documented in spec
    CKM_PKCS5_PBKD2:                    retval := ksmNA;  // Not documented in spec
    CKM_PBA_SHA1_WITH_SHA1_HMAC:        retval := ksmNA;  // Not documented in spec
    CKM_WTLS_PRE_MASTER_KEY_GEN:        retval := ksmBytes;
    CKM_WTLS_MASTER_KEY_DERIVE:         retval := ksmBytes;
    CKM_WTLS_MASTER_KEY_DERIVE_DH_ECC:  retval := ksmBytes;
    CKM_WTLS_PRF:                       retval := ksmNA;  // Not documented in spec
    CKM_WTLS_SERVER_KEY_AND_MAC_DERIVE: retval := ksmNA;  // Not documented in spec
    CKM_WTLS_CLIENT_KEY_AND_MAC_DERIVE: retval := ksmNA;  // Not documented in spec
    CKM_KEY_WRAP_LYNKS:                 retval := ksmNA;  // Not documented in spec
    CKM_KEY_WRAP_SET_OAEP:              retval := ksmNA;  // Not documented in spec
    CKM_CMS_SIG:                        retval := ksmNA;  // Not documented in spec
    CKM_KIP_DERIVE:                     retval := ksmNA;  // Not documented in spec
    CKM_KIP_WRAP:                       retval := ksmNA;  // Not documented in spec
    CKM_KIP_MAC:                        retval := ksmNA;  // Not documented in spec
    CKM_CAMELLIA_KEY_GEN:               retval := ksmBytes;
    CKM_CAMELLIA_ECB:                   retval := ksmBytes;
    CKM_CAMELLIA_CBC:                   retval := ksmBytes;
    CKM_CAMELLIA_MAC:                   retval := ksmBytes;
    CKM_CAMELLIA_MAC_GENERAL:           retval := ksmBytes;
    CKM_CAMELLIA_CBC_PAD:               retval := ksmBytes;
    CKM_CAMELLIA_ECB_ENCRYPT_DATA:      retval := ksmBytes;
    CKM_CAMELLIA_CBC_ENCRYPT_DATA:      retval := ksmBytes;
    CKM_CAMELLIA_CTR:                   retval := ksmBytes;
    CKM_ARIA_KEY_GEN:                   retval := ksmBytes;
    CKM_ARIA_ECB:                       retval := ksmBytes;
    CKM_ARIA_CBC:                       retval := ksmBytes;
    CKM_ARIA_MAC:                       retval := ksmBytes;
    CKM_ARIA_MAC_GENERAL:               retval := ksmBytes;
    CKM_ARIA_CBC_PAD:                   retval := ksmBytes;
    CKM_ARIA_ECB_ENCRYPT_DATA:          retval := ksmBytes;
    CKM_ARIA_CBC_ENCRYPT_DATA:          retval := ksmBytes;
    CKM_SKIPJACK_KEY_GEN:               retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_ECB64:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CBC64:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_OFB64:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB64:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB32:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB16:                 retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_CFB8:                  retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_WRAP:                  retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_PRIVATE_WRAP:          retval := ksmNA;  // Not documented in spec
    CKM_SKIPJACK_RELAYX:                retval := ksmNA;  // Not documented in spec
    CKM_KEA_KEY_PAIR_GEN:               retval := ksmBits;
    CKM_KEA_KEY_DERIVE:                 retval := ksmBits;
    CKM_FORTEZZA_TIMESTAMP:             retval := ksmBits;
    CKM_BATON_KEY_GEN:                  retval := ksmNA;  // Not documented in spec
    CKM_BATON_ECB128:                   retval := ksmNA;  // Not documented in spec
    CKM_BATON_ECB96:                    retval := ksmNA;  // Not documented in spec
    CKM_BATON_CBC128:                   retval := ksmNA;  // Not documented in spec
    CKM_BATON_COUNTER:                  retval := ksmNA;  // Not documented in spec
    CKM_BATON_SHUFFLE:                  retval := ksmNA;  // Not documented in spec
    CKM_BATON_WRAP:                     retval := ksmNA;  // Not documented in spec
    CKM_ECDSA_KEY_PAIR_GEN:             retval := ksmBits;
    //CKM_EC_KEY_PAIR_GEN:                retval := ksmBits;
    CKM_ECDSA:                          retval := ksmBits;
    CKM_ECDSA_SHA1:                     retval := ksmBits;
    CKM_ECDH1_DERIVE:                   retval := ksmBits;
    CKM_ECDH1_COFACTOR_DERIVE:          retval := ksmBits;
    CKM_ECMQV_DERIVE:                   retval := ksmBits;
    CKM_JUNIPER_KEY_GEN:                retval := ksmNA;  // Not documented in spec
    CKM_JUNIPER_ECB128:                 retval := ksmNA;  // Not documented in spec
    CKM_JUNIPER_CBC128:                 retval := ksmNA;  // Not documented in spec
    CKM_JUNIPER_COUNTER:                retval := ksmNA;  // Not documented in spec
    CKM_JUNIPER_SHUFFLE:                retval := ksmNA;  // Not documented in spec
    CKM_JUNIPER_WRAP:                   retval := ksmNA;  // Not documented in spec
    CKM_FASTHASH:                       retval := ksmNA;  // Not documented in spec
    CKM_AES_KEY_GEN:                    retval := ksmBytes;
    CKM_AES_ECB:                        retval := ksmBytes;
    CKM_AES_CBC:                        retval := ksmBytes;
    CKM_AES_MAC:                        retval := ksmBytes;
    CKM_AES_MAC_GENERAL:                retval := ksmBytes;
    CKM_AES_CBC_PAD:                    retval := ksmBytes;
    CKM_AES_CTR:                        retval := ksmBytes;
    CKM_BLOWFISH_KEY_GEN:               retval := ksmBytes;
    CKM_BLOWFISH_CBC:                   retval := ksmBytes;
    CKM_TWOFISH_KEY_GEN:                retval := ksmBytes;  // Not documented in spec; assumption based on what Blowfish does
    CKM_TWOFISH_CBC:                    retval := ksmBytes;  // Not documented in spec; assumption based on what Blowfish does
    CKM_DES_ECB_ENCRYPT_DATA:           retval := ksmNA;  // Not documented in spec
    CKM_DES_CBC_ENCRYPT_DATA:           retval := ksmNA;  // Not documented in spec
    CKM_DES3_ECB_ENCRYPT_DATA:          retval := ksmNA;  // Not documented in spec
    CKM_DES3_CBC_ENCRYPT_DATA:          retval := ksmNA;  // Not documented in spec
    CKM_AES_ECB_ENCRYPT_DATA:           retval := ksmBytes;
    CKM_AES_CBC_ENCRYPT_DATA:           retval := ksmBytes;
    CKM_DSA_PARAMETER_GEN:              retval := ksmBits;
    CKM_DH_PKCS_PARAMETER_GEN:          retval := ksmBits;
    CKM_X9_42_DH_PARAMETER_GEN:         retval := ksmBits;
    CKM_VENDOR_DEFINED:                 retval := ksmNA;  // Vendor specific
  end;

  Result := retval;
end;

END.


