unit FreeOTFEDLLCypherAPI;

// API ported from FreeOTFE4PDACypherAPI.h

interface

uses
  FreeOTFECypherDriverAPI, Windows;


 //#define DLLEXPORT_CYPHER_IDENTIFYDRIVER TEXT("CypherIdentifyDriver")
 //typedef DWORD (* PCypherDLLFnIdentifyDriver)(
 //                                 DIOC_CYPHER_IDENTIFYDRIVER*
 //                                );
const
  DLLEXPORT_CYPHER_IDENTIFYDRIVER = 'CypherIdentifyDriver';

type
  PCypherDLLFnIdentifyDriver = function(Buffer: PDIOC_CYPHER_IDENTIFYDRIVER): DWORD; CDECL;


 // Note: "_v1" missing to give backward compatibility
 //#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 TEXT("CypherIdentifySupported")
 ////#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 TEXT("CypherIdentifySupported_v1")
 //typedef DWORD (* PCypherDLLFnIdentifySupported_v1)(
 //                                 unsigned int,
 //                                 DIOC_CYPHER_IDENTIFYSUPPORTED_v1*
 //                                );
//const
//  DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v1 = 'CypherIdentifySupported';

//type
//  PCypherDLLFnIdentifySupported_v1 = function(BufferSize: Integer;  // In bytes
//    Buffer: PDIOC_CYPHER_IDENTIFYSUPPORTED_v1): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3 TEXT("CypherIdentifySupported_v3")
 //typedef DWORD (* PCypherDLLFnIdentifySupported_v3)(
 //                                 unsigned int,
 //                                 DIOC_CYPHER_IDENTIFYSUPPORTED_v3*
 //                                );
const
  DLLEXPORT_CYPHER_IDENTIFYSUPPORTED_v3 = 'CypherIdentifySupported_v3';

type
  PCypherDLLFnIdentifySupported_v3 = function(BufferSize: Integer;  // In bytes
    Buffer: PDIOC_CYPHER_IDENTIFYSUPPORTED_v3): DWORD; CDECL;

 // Note: "_v1" missing to give backward compatibility
 //#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 TEXT("CypherGetCypherDetails")
 ////#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 TEXT("CypherGetCypherDetails_v1")
 //typedef DWORD (* PCypherDLLFnGetCypherDetails_v1)(
 //                                 GUID*,
 //                                 CYPHER_v1*
 //                                );//
//const
//  DLLEXPORT_CYPHER_GETCYPHERDETAILS_v1 = 'CypherGetCypherDetails';
//
//type
//  PCypherDLLFnGetCypherDetails_v1 = function(CypherGUID: PGUID;
//    CypherDetails: PCYPHER_v1): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3 TEXT("CypherGetCypherDetails_v3")
 //typedef DWORD (* PCypherDLLFnGetCypherDetails_v3)(
 //                                 GUID*,
 //                                 CYPHER_v3*
 //                                );
const
  DLLEXPORT_CYPHER_GETCYPHERDETAILS_v3 = 'CypherGetCypherDetails_v3';

type
  PCypherDLLFnGetCypherDetails_v3 = function(CypherGUID: PGUID;
    CypherDetails: PCYPHER_v3): DWORD; CDECL;


 //#define DLLEXPORT_CYPHER_ENCRYPT TEXT("CypherEncrypt")
 //typedef DWORD (* PCypherDLLFnEncrypt)(
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_IN*,
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_OUT*
 //                         );
const
  DLLEXPORT_CYPHER_ENCRYPT = 'CypherEncrypt';

type
  PCypherDLLFnEncrypt = function(BufferInLen: DWORD; BufferIn: PDIOC_CYPHER_DATA_IN;
    BufferOutLen: DWORD; BufferOut: PDIOC_CYPHER_DATA_OUT): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_ENCRYPTSECTOR TEXT("CypherEncryptSector")
 //typedef DWORD (* PCypherDLLFnEncryptSector)(
 //                            DWORD,
 //                            DIOC_CYPHER_SECTOR_DATA_IN*,
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_OUT*
 //                         );
const
  DLLEXPORT_CYPHER_ENCRYPTSECTOR = 'CypherEncryptSector';

type
  PCypherDLLFnEncryptSector = function(BufferInLen: DWORD; BufferIn: PDIOC_CYPHER_SECTOR_DATA_IN;
    BufferOutLen: DWORD; BufferOut: PDIOC_CYPHER_DATA_OUT): DWORD; CDECL;


 //#define DLLEXPORT_CYPHER_ENCRYPTWITHASCII TEXT("CypherEncryptWithASCII")
 //typedef DWORD (* PCypherDLLFnEncryptWithASCII)(
 //                            IN      GUID*,
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      char*,  // ASCII representation of "Key"
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      int,  // In bytes
 //                            IN      FREEOTFEBYTE*,
 //                            OUT     FREEOTFEBYTE*
 //                         );
const
  DLLEXPORT_CYPHER_ENCRYPTWITHASCII = 'CypherEncryptWithASCII';

type
  PCypherDLLFnEncryptWithASCII = function(CypherGUID: PGUID; KeyLength: Integer;  // In bits
    Key: PByte; KeyASCII: PChar;          // ASCII representation of "Key"
    IVLength: Integer;                    // In bits
    IV: PByte; PlaintextLength: Integer;  // In bytes
    PlaintextData: PByte; CyphertextData: PByte): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII TEXT("CypherEncryptSectorWithASCII")
 //typedef DWORD (* PCypherDLLFnEncryptSectorWithASCII)(
 //                            IN      GUID*,
 //                            IN      LARGE_INTEGER,
 //                            IN      int,
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      char*,  // ASCII representation of "Key"
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      int,  // In bytes
 //                            IN      FREEOTFEBYTE*,
 //                            OUT     FREEOTFEBYTE*
 //                         );
const
  DLLEXPORT_CYPHER_ENCRYPTSECTORWITHASCII = 'CypherEncryptSectorWithASCII';

type
  PCypherDLLFnEncryptSectorWithASCII = function(CypherGUID: PGUID;
    SectorID: LARGE_INTEGER; SectorSize: Integer; KeyLength: Integer;  // In bits
    Key: PByte; KeyASCII: PChar;          // ASCII representation of "Key"
    IVLength: Integer;                    // In bits
    IV: PByte; PlaintextLength: Integer;  // In bytes
    PlaintextData: PByte; CyphertextData: PByte): DWORD; CDECL;


 //#define DLLEXPORT_CYPHER_DECRYPT TEXT("CypherDecrypt")
 //typedef DWORD (* PCypherDLLFnDecrypt)(
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_IN*,
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_OUT*
 //                         );
const
  DLLEXPORT_CYPHER_DECRYPT = 'CypherDecrypt';

type
  PCypherDLLFnDecrypt = function(BufferInLen: DWORD; BufferIn: PDIOC_CYPHER_DATA_IN;
    BufferOutLen: DWORD; BufferOut: PDIOC_CYPHER_DATA_OUT): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_DECRYPTSECTOR TEXT("CypherDecryptSector")
 //typedef DWORD (* PCypherDLLFnDecryptSector)(
 //                            DWORD,
 //                            DIOC_CYPHER_SECTOR_DATA_IN*,
 //                            DWORD,
 //                            DIOC_CYPHER_DATA_OUT*
 //                         );
const
  DLLEXPORT_CYPHER_DECRYPTSECTOR = 'CypherDecryptSector';

type
  PCypherDLLFnDecryptSector = function(BufferInLen: DWORD; BufferIn: PDIOC_CYPHER_SECTOR_DATA_IN;
    BufferOutLen: DWORD; BufferOut: PDIOC_CYPHER_DATA_OUT): DWORD; CDECL;


 //#define DLLEXPORT_CYPHER_DECRYPTWITHASCII TEXT("CypherDecryptWithASCII")
 //typedef DWORD (* PCypherDLLFnDecryptWithASCII)(
 //                            IN      GUID*,
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      char*,  // ASCII representation of "Key"
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      int,  // In bytes
 //                            IN      FREEOTFEBYTE*,
 //                            OUT     FREEOTFEBYTE*
 //                         );
const
  DLLEXPORT_CYPHER_DECRYPTWITHASCII = 'CypherDecryptWithASCII';

type
  PCypherDLLFnDecryptWithASCII = function(CypherGUID: PGUID; KeyLength: Integer;  // In bits
    Key: PByte; KeyASCII: PChar;           // ASCII representation of "Key"
    IVLength: Integer;                     // In bits
    IV: PByte; CyphertextLength: Integer;  // In bytes
    CyphertextData: PByte; PlaintextData: PByte): DWORD; CDECL;

 //#define DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII TEXT("CypherDecryptSectorWithASCII")
 //typedef DWORD (* PCypherDLLFnDecryptSectorWithASCII)(
 //                            IN      GUID*,
 //                            IN      LARGE_INTEGER,
 //                            IN      int,
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      char*,  // ASCII representation of "Key"
 //                            IN      int,  // In bits
 //                            IN      FREEOTFEBYTE*,
 //                            IN      int,  // In bytes
 //                            IN      FREEOTFEBYTE*,
 //                            OUT     FREEOTFEBYTE*
 //                         );
const
  DLLEXPORT_CYPHER_DECRYPTSECTORWITHASCII = 'CypherDecryptSectorWithASCII';

type
  PCypherDLLFnDecryptSectorWithASCII = function(CypherGUID: PGUID;
    SectorID: LARGE_INTEGER; SectorSize: Integer; KeyLength: Integer;  // In bits
    Key: PByte; KeyASCII: PChar;           // ASCII representation of "Key"
    IVLength: Integer;                     // In bits
    IV: PByte; CyphertextLength: Integer;  // In bytes
    CyphertextData: PByte; PlaintextData: PByte): DWORD; CDECL;


implementation

end.
