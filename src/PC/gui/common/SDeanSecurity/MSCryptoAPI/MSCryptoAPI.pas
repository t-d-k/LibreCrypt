unit MSCryptoAPI;
// Description: Sarah Dean's MS CryptoAPI
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This file contains a Delphi port of *part* of the MS CryptoAPI
//
// It is *not* a full port, and only contains a very *limited* subset of
// definitions


interface

uses
  Windows;  // Required for various definitions (e.g. BOOL, LPCSTR)


const
  ADVAPI = 'advapi32.dll';




const
{
From WinCrypt.h (MS Platform SDK)
}
  // dwFlags definitions for CryptAcquireContext
  CRYPT_VERIFYCONTEXT     = $F0000000;
  CRYPT_NEWKEYSET         = $00000008;
  CRYPT_DELETEKEYSET      = $00000010;
  CRYPT_MACHINE_KEYSET    = $00000020;
  CRYPT_SILENT            = $00000040;


{
From WinCrypt.h (MS Platform SDK)

CryptSetProvParam section
}
  PP_CLIENT_HWND          = 1;
  PP_CONTEXT_INFO         = 11;
  PP_KEYEXCHANGE_KEYSIZE  = 12;
  PP_SIGNATURE_KEYSIZE    = 13;
  PP_KEYEXCHANGE_ALG      = 14;
  PP_SIGNATURE_ALG        = 15;
  PP_DELETEKEY            = 24;

  PROV_RSA_FULL           = 1;
  PROV_RSA_SIG            = 2;
  PROV_DSS                = 3;
  PROV_FORTEZZA           = 4;
  PROV_MS_EXCHANGE        = 5;
  PROV_SSL                = 6;
  PROV_RSA_SCHANNEL       = 12;
  PROV_DSS_DH             = 13;
  PROV_EC_ECDSA_SIG       = 14;
  PROV_EC_ECNRA_SIG       = 15;
  PROV_EC_ECDSA_FULL      = 16;
  PROV_EC_ECNRA_FULL      = 17;
  PROV_DH_SCHANNEL        = 18;
  PROV_SPYRUS_LYNKS       = 20;
  PROV_RNG                = 21;
  PROV_INTEL_SEC          = 22;
  PROV_REPLACE_OWF        = 23;
  PROV_RSA_AES            = 24;

{
From WinCrypt.h (MS Platform SDK)

Provider friendly names section
}
  MS_DEF_PROV              = 'Microsoft Base Cryptographic Provider v1.0';
  MS_ENHANCED_PROV         = 'Microsoft Enhanced Cryptographic Provider v1.0';
  MS_STRONG_PROV           = 'Microsoft Strong Cryptographic Provider';
  MS_DEF_RSA_SIG_PROV      = 'Microsoft RSA Signature Cryptographic Provider';
  MS_DEF_RSA_SCHANNEL_PROV = 'Microsoft RSA SChannel Cryptographic Provider';
  MS_DEF_DSS_PROV          = 'Microsoft Base DSS Cryptographic Provider';
  MS_DEF_DSS_DH_PROV       = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  MS_ENH_DSS_DH_PROV       = 'Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider';
  MS_DEF_DH_SCHANNEL_PROV  = 'Microsoft DH SChannel Cryptographic Provider';
  MS_SCARD_PROV            = 'Microsoft Base Smart Card Crypto Provider';
  MS_ENH_RSA_AES_PROV      = 'Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)';


type
{
From WinCrypt.h (MS Platform SDK):

typedef ULONG_PTR HCRYPTPROV;

}
  ULONG_PTR = ^ULONG;
  HCRYPTPROV = ULONG_PTR;

  PHCRYPTPROV = ^HCRYPTPROV;



{
From WinCrypt.h (MS Platform SDK):

WINADVAPI
BOOL
WINAPI
CryptAcquireContextA(
    HCRYPTPROV *phProv,
    LPCSTR szContainer,
    LPCSTR szProvider,
    DWORD dwProvType,
    DWORD dwFlags
    );
WINADVAPI
BOOL
WINAPI
CryptAcquireContextW(
    HCRYPTPROV *phProv,
    LPCWSTR szContainer,
    LPCWSTR szProvider,
    DWORD dwProvType,
    DWORD dwFlags
    );
#ifdef UNICODE
#define CryptAcquireContext  CryptAcquireContextW
#else
#define CryptAcquireContext  CryptAcquireContextA
#endif // !UNICODE
}
{$WARNINGS OFF}  // Useless warning about platform - this is the *published* API!
function CryptAcquireContext(phProv: PHCRYPTPROV; szContainer: LPCSTR; szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI name 'CryptAcquireContextA';
function CryptAcquireContextA(phProv: PHCRYPTPROV; szContainer: LPCSTR; szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI name 'CryptAcquireContextA';
function CryptAcquireContextW(phProv: PHCRYPTPROV; szContainer: LPCWSTR; szProvider: LPCWSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI name 'CryptAcquireContextW';
{$WARNINGS ON}


{
From WinCrypt.h (MS Platform SDK):

WINADVAPI
BOOL
WINAPI
CryptReleaseContext(
    HCRYPTPROV hProv,
    DWORD dwFlags
    );
}
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI name 'CryptReleaseContext';



{
From WinCrypt.h (MS Platform SDK):

WINADVAPI
BOOL
WINAPI
CryptGenRandom(
    HCRYPTPROV hProv,
    DWORD dwLen,
    BYTE *pbBuffer
    );
}
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: PByte): BOOL;
  stdcall; external ADVAPI name 'CryptGenRandom';



implementation

END.


