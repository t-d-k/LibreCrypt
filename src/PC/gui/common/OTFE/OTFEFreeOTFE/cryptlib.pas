unit cryptlib;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org         
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// Note that the cryptlib Delphi module supplied with cryptlib isn't used;
// that unit staticly links to the cryptlib DLL, but we can't be sure the user
// will have this installed - therefore we have this unit to *dynamically*
// link to the cryptlib DLL.
//
// Note: This unit only includes those functions required to generate random
//       data.
                 

interface

uses
  Windows,  // Required for THandle
  SysUtils;  // Required for Exception


const
  // The filename of the DLL
  CRYPTLIB32_DLL = 'cryptlib\cl32.dll';
  CRYPTLIB64_DLL = 'cryptlib\cl64.dll';
type
  // #define TC_RET  __declspec( dllXXport )  int  __stdcall  /* DLL XXport ret.val.*/
  TC_RET = integer;

  // typedef int CRYPT_CONTEXT;
  TCRYPT_CONTEXT = integer;
  PCRYPT_CONTEXT = ^TCRYPT_CONTEXT;
  // typedef int CRYPT_USER;
  TCRYPT_USER = integer;


  Tcryptlib_CRYPT_ALGO_TYPE = (
    // No encryption
    catCRYPT_ALGO_NONE,            // No encryption

    // Conventional encryption
    catCRYPT_ALGO_DES,             // DES
    catCRYPT_ALGO_3DES,            // Triple DES
    catCRYPT_ALGO_IDEA,            // IDEA
    catCRYPT_ALGO_CAST,            // CAST-128
    catCRYPT_ALGO_RC2,             // RC2
    catCRYPT_ALGO_RC4,             // RC4
    catCRYPT_ALGO_RC5,             // RC5
    catCRYPT_ALGO_AES,             // AES
    catCRYPT_ALGO_BLOWFISH,        // Blowfish
    catCRYPT_ALGO_SKIPJACK,        // Skipjack

    // Public-key encryption
    catCRYPT_ALGO_DH,              // Diffie-Hellman
    catCRYPT_ALGO_RSA,             // RSA
    catCRYPT_ALGO_DSA,             // DSA
    catCRYPT_ALGO_ELGAMAL,         // ElGamal
    catCRYPT_ALGO_KEA,             // KEA

    // Hash algorithms
    catCRYPT_ALGO_MD2,             // MD2
    catCRYPT_ALGO_MD4,             // MD4
    catCRYPT_ALGO_MD5,             // MD5
    catCRYPT_ALGO_SHA,             // SHA/SHA1
    catCRYPT_ALGO_RIPEMD160,       // RIPE-MD 160
    catCRYPT_ALGO_SHA2,            // SHA2 (SHA-256/384/512)

    // MAC's
    catCRYPT_ALGO_HMAC_MD5,        // HMAC-MD5
    catCRYPT_ALGO_HMAC_SHA,        // HMAC-SHA
    catCRYPT_ALGO_HMAC_RIPEMD160,  // HMAC-RIPEMD-160

    // Vendors may want to use their own algorithms that aren't part of the
    // general cryptlib suite.  The following values are for vendor-defined
    // algorithms, and can be used just like the named algorithm types (it's
    // up to the vendor to keep track of what _VENDOR1 actually corresponds
    // to)
    catCRYPT_ALGO_VENDOR1,
    catCRYPT_ALGO_VENDOR2,
    catCRYPT_ALGO_VENDOR3
    );


const
  cryptlib_CRYPT_ALGO_TYPE_ID : array [Tcryptlib_CRYPT_ALGO_TYPE] of integer = (
                                        // No encryption
                                        0,      // No encryption

                                        // Conventional encryption
                                        1,      // DES
                                        2,      // Triple DES
                                        3,      // IDEA
                                        4,      // CAST-128
                                        5,      // RC2
                                        6,      // RC4
                                        7,      // RC5
                                        8,      // AES
                                        9,      // Blowfish
                                        10,     // Skipjack

                                        // Public-key encryption
                                        100,    // Diffie-Hellman
                                        101,    // RSA
                                        102,    // DSA
                                        103,    // ElGamal
                                        104,    // KEA

                                        // Hash algorithms
                                        200,    // MD2
                                        201,    // MD4
                                        202,    // MD5
                                        203,    // SHA/SHA1
                                        204,    // RIPE-MD 160
                                        205,    // SHA2 (SHA-256/384/512)

                                        // MAC's
                                        300,    // HMAC-MD5
                                        301,    // HMAC-SHA
                                        302,    // HMAC-RIPEMD-160

                                        // Vendors may want to use their own algorithms that aren't part of the
                                        // general cryptlib suite.  The following values are for vendor-defined
                                        // algorithms, and can be used just like the named algorithm types (it's
                                        // up to the vendor to keep track of what _VENDOR1 actually corresponds
                                        // to)
                                        10000,
                                        10001,
                                        10002
                                        );


  cryptlib_CRYPT_ALGO_TYPE_TITLE : array [Tcryptlib_CRYPT_ALGO_TYPE] of string = (
                                        // No encryption
                                        'No encryption',

                                        // Conventional encryption
                                        'DES',
                                        'Triple DES',
                                        'IDEA',
                                        'CAST-128',
                                        'RC2',
                                        'RC4',
                                        'RC5',
                                        'AES',
                                        'Blowfish',
                                        'Skipjack',

                                        // Public-key encryption
                                        'Diffie-Hellman',
                                        'RSA',
                                        'DSA',
                                        'ElGamal',
                                        'KEA',

                                        // Hash algorithms
                                        'MD2',
                                        'MD4',
                                        'MD5',
                                        'SHA/SHA1',
                                        'RIPE-MD 160',
                                        'SHA2 (SHA-256/384/512)',

                                        // MAC's
                                        'HMAC-MD5',
                                        'HMAC-SHA',
                                        'HMAC-RIPEMD-160',

                                        // Vendors may want to use their own algorithms that aren't part of the
                                        // general cryptlib suite.  The following values are for vendor-defined
                                        // algorithms, and can be used just like the named algorithm types (it's
                                        // up to the vendor to keep track of what _VENDOR1 actually corresponds
                                        // to)
                                        'Vendor algorithm #1',
                                        'Vendor algorithm #2',
                                        'Vendor algorithm #3'
                                        );

  // The maximum size of a text string (e.g.key owner name) 
  cryptlib_CRYPT_MAX_TEXTSIZE = 64;

  // Status Codes

  // No error in function call
  cryptlib_CRYPT_OK                  =  0;

  // Error in parameters passed to function
  cryptlib_CRYPT_ERROR_PARAM1        = -1;  // Bad argument, parameter 1
  cryptlib_CRYPT_ERROR_PARAM2        = -2;  // Bad argument, parameter 2
  cryptlib_CRYPT_ERROR_PARAM3        = -3;  // Bad argument, parameter 3
  cryptlib_CRYPT_ERROR_PARAM4        = -4;  // Bad argument, parameter 4
  cryptlib_CRYPT_ERROR_PARAM5        = -5;  // Bad argument, parameter 5
  cryptlib_CRYPT_ERROR_PARAM6        = -6;  // Bad argument, parameter 6
  cryptlib_CRYPT_ERROR_PARAM7        = -7;  // Bad argument, parameter 7

  // Errors due to insufficient resources
  cryptlib_CRYPT_ERROR_MEMORY        = -10;  // Out of memory
  cryptlib_CRYPT_ERROR_NOTINITED     = -11;  // Data has not been initialised
  cryptlib_CRYPT_ERROR_INITED        = -12;  // Data has already been init'd
  cryptlib_CRYPT_ERROR_NOSECURE      = -13;  // Opn.not avail.at requested sec.level
  cryptlib_CRYPT_ERROR_RANDOM        = -14;  // No reliable random data available
  cryptlib_CRYPT_ERROR_FAILED        = -15;  // Operation failed

  // Security violations
  cryptlib_CRYPT_ERROR_NOTAVAIL      = -20;  // This type of opn.not available
  cryptlib_CRYPT_ERROR_PERMISSION    = -21;  // No permiss.to perform this operation
  cryptlib_CRYPT_ERROR_WRONGKEY      = -22;  // Incorrect key used to decrypt data
  cryptlib_CRYPT_ERROR_INCOMPLETE    = -23;  // Operation incomplete/still in progress
  cryptlib_CRYPT_ERROR_COMPLETE      = -24;  // Operation complete/can't continue
  cryptlib_CRYPT_ERROR_TIMEOUT       = -25;  // Operation timed out before completion
  cryptlib_CRYPT_ERROR_INVALID       = -26;  // Invalid/inconsistent information
  cryptlib_CRYPT_ERROR_SIGNALLED     = -27;  // Resource destroyed by extnl.event

  // High-level function errors
  cryptlib_CRYPT_ERROR_OVERFLOW      = -30;  // Resources/space exhausted
  cryptlib_CRYPT_ERROR_UNDERFLOW     = -31;  // Not enough data available
  cryptlib_CRYPT_ERROR_BADDATA       = -32;  // Bad/unrecognised data format
  cryptlib_CRYPT_ERROR_SIGNATURE     = -33;  // Signature/integrity check failed

  // Data access function errors
  cryptlib_CRYPT_ERROR_OPEN          = -40;  // Cannot open object
  cryptlib_CRYPT_ERROR_READ          = -41;  // Cannot read item from object
  cryptlib_CRYPT_ERROR_WRITE         = -42;  // Cannot write item to object
  cryptlib_CRYPT_ERROR_NOTFOUND      = -43;  // Requested item not found in object
  cryptlib_CRYPT_ERROR_DUPLICATE     = -44;  // Item already present in object

  // Data enveloping errors
  cryptlib_CRYPT_ENVELOPE_RESOURCE   = -50;  // Need resource to proceed



type
  Pcryptlib_CRYPT_QUERY_INFO = ^Tcryptlib_CRYPT_QUERY_INFO;
  Tcryptlib_CRYPT_QUERY_INFO = record
    // Algorithm information
    algoName: array [0..(cryptlib_CRYPT_MAX_TEXTSIZE-1)] of Ansichar;  // Algorithm name
    blockSize: integer;   // Block size of the algorithm
    minKeySize: integer;  // Minimum key size in bytes
    keySize: integer;     // Recommended key size in bytes
    maxKeySize: integer;  //  Maximum key size in bytes
  end;


  // Initialise and shut down cryptlib
  // TC_RET cryptInit( void );
  Tcryptlib_cryptInit = function(): TC_RET;

  // TC_RET cryptEnd( void ): TC_RET;
  Tcryptlib_cryptEnd = function(): TC_RET;

  // Query cryptlibs capabilities
  //TC_RET cryptQueryCapability( C_IN CRYPT_ALGO_TYPE cryptAlgo,
  //							C_OUT CRYPT_QUERY_INFO C_PTR cryptQueryInfo );
  Tcryptlib_cryptQueryCapability = function(
                          cryptAlgo: integer;
                          cryptQueryInfo: Pcryptlib_CRYPT_QUERY_INFO
                         ): TC_RET; stdcall;


  // TC_RET cryptAddRandom( C_IN void C_PTR randomData, C_IN int randomDataLength );
  Tcryptlib_cryptAddRandom = function(
                          randomData: Pointer;
                          randomDataLength: integer
                         ): TC_RET; stdcall;

  // TC_RET cryptCreateContext( C_OUT CRYPT_CONTEXT C_PTR cryptContext,
  //                                                   C_IN CRYPT_USER cryptUser,
  //                                                   C_IN CRYPT_ALGO_TYPE cryptAlgo );
  Tcryptlib_cryptCreateContext = function(
                          cryptContext: PCRYPT_CONTEXT;
                          cryptUser: TCRYPT_USER;
                          cryptAlgo: integer
                         ): TC_RET; stdcall;

  // TC_RET cryptDestroyContext( C_IN CRYPT_CONTEXT cryptContext );
  Tcryptlib_cryptDestroyContext = function(
                          cryptContext: TCRYPT_CONTEXT
                         ): TC_RET; stdcall;

  // TC_RET cryptGenerateKey( C_IN CRYPT_CONTEXT cryptContext );
  Tcryptlib_cryptGenerateKey = function(
                          cryptContext: TCRYPT_CONTEXT
                         ): TC_RET; stdcall;

  // TC_RET cryptEncrypt( C_IN CRYPT_CONTEXT cryptContext, C_INOUT void C_PTR buffer,
  //                                         C_IN int length );
  Tcryptlib_cryptEncrypt = function(
                          cryptContext: TCRYPT_CONTEXT;
                          buffer: Pointer;
                          length: integer
                         ): TC_RET; stdcall;

  // TC_RET cryptDecrypt( C_IN CRYPT_CONTEXT cryptContext, C_INOUT void C_PTR buffer,
  //                                         C_IN int length );
  Tcryptlib_cryptDecrypt = function(
                          cryptContext: TCRYPT_CONTEXT;
                          buffer: Pointer;
                          length: integer
                         ): TC_RET; stdcall;


  // Exceptions...
  ECryptLibError = class(Exception);
  ECryptLibBadDLL = class(ECryptLibError);


const
  // A magic value for unused parameters
  cryptlib_CRYPT_UNUSED = -101;

  // The type of information polling to perform to get random seed information */
  cryptlib_CRYPT_RANDOM_FASTPOLL = -300;
  cryptlib_CRYPT_RANDOM_SLOWPOLL = -301;


var
  // When loaded, this holds the handle to the DLL
  hCryptLibDLL: THandle;

  // When the DLL is loaded, these are set to the function pointers
  cryptlib_cryptInit: Tcryptlib_cryptInit;
  cryptlib_cryptEnd: Tcryptlib_cryptEnd;
  cryptlib_cryptQueryCapability: Tcryptlib_cryptQueryCapability;
  cryptlib_cryptCreateContext: Tcryptlib_cryptCreateContext;
  cryptlib_cryptDestroyContext: Tcryptlib_cryptDestroyContext;
  cryptlib_cryptAddRandom: Tcryptlib_cryptAddRandom;
  cryptlib_cryptGenerateKey: Tcryptlib_cryptGenerateKey;
  cryptlib_cryptEncrypt: Tcryptlib_cryptEncrypt;
  cryptlib_cryptDecrypt: Tcryptlib_cryptDecrypt;

// Load/unload cryptlib DLL
function cryptlib_LoadDLL(): boolean;
function cryptlib_UnloadDLL(): boolean;

// Simple "helper" functions
function cryptlib_cryptStatusError(status: integer): boolean;
function cryptlib_cryptStatusOK(status: integer): boolean;

// This may *only* be called *after*:
//   cryptlib_LoadDLL(...)
//   cryptlib_cryptInit(...)
//   cryptlib_cryptAddRandom(...)
function cryptlib_RNG(lengthBytes: integer): ansistring;

implementation

uses
//delphi
  Math,// Required for min(...)
  strutils,//for ifthen
  //sdu
  sdugeneral
  ;

// Internal - populate the global variable function pointers
procedure _cryptlib_GetDLLProcAddresses(); forward;



const
  // Names of the various functions within the DLL
  DLL_FUNCTIONNAME_cryptInit            = 'cryptInit';
  DLL_FUNCTIONNAME_cryptEnd             = 'cryptEnd';
  DLL_FUNCTIONNAME_cryptQueryCapability = 'cryptQueryCapability';

  DLL_FUNCTIONNAME_cryptCreateContext   = 'cryptCreateContext';
  DLL_FUNCTIONNAME_cryptDestroyContext  = 'cryptDestroyContext';
  DLL_FUNCTIONNAME_cryptAddRandom       = 'cryptAddRandom';
  DLL_FUNCTIONNAME_cryptGenerateKey     = 'cryptGenerateKey';
  DLL_FUNCTIONNAME_cryptEncrypt         = 'cryptEncrypt';
  DLL_FUNCTIONNAME_cryptDecrypt         = 'cryptDecrypt';


  // Exception error messages
  E_CRYPTLIB_NO_DLL = 'Unable to load cryptlib DLL.';
  E_CRYPTLIB_BAD_DLL = 'cryptlib DLL did not have correct facilities!';


function cryptlib_cryptStatusError(status: integer): boolean;
begin
  Result := (status < cryptlib_CRYPT_OK);
end;

function cryptlib_cryptStatusOK(status: integer): boolean;
begin
  Result := (status = cryptlib_CRYPT_OK);
end;

function cryptlib_LoadDLL(): boolean;
var
  dll : widestring;
begin
  result := FALSE;

  // If the lib is already loaded, just return TRUE
  if (hCryptLibDLL <> 0) then     begin
    result := TRUE;
  end  else    begin
    //the dll version required is based on if the *app* is 32 or 64 bit - not the OS
    dll := Ifthen( SDUApp64bit(), CRYPTLIB64_DLL, CRYPTLIB32_DLL);

    hCryptLibDLL := LoadLibrary(PChar(dll));
    if (hCryptLibDLL <> 0) then
      begin
      // DLL loaded, get function addresses
      try
        _cryptlib_GetDLLProcAddresses();
        result := TRUE;
      except
        on ECryptLibBadDLL do
          begin
          // Unload DLL; there was a problem
          cryptlib_UnloadDLL();
          end;
      end;

      end;

    end;

end;


function cryptlib_UnloadDLL(): boolean;
begin
  if (hCryptLibDLL<>0) then
    begin
    FreeLibrary(hCryptLibDLL);
    hCryptLibDLL := 0;
    end;

  Result := TRUE;

end;


procedure _cryptlib_GetDLLProcAddresses();

  procedure CheckFnAvailable(fnPtr: Pointer; fnName: string);
  begin
    if (fnPtr = nil) then
      begin
      raise ECryptLibError.Create(E_CRYPTLIB_BAD_DLL+' ('+fnName+')');
      end;
  end;

begin
  @cryptlib_cryptInit := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptInit);
  CheckFnAvailable(@cryptlib_cryptInit, DLL_FUNCTIONNAME_cryptInit);

  @cryptlib_cryptEnd := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptEnd);
  CheckFnAvailable(@cryptlib_cryptEnd, DLL_FUNCTIONNAME_cryptEnd);

  @cryptlib_cryptQueryCapability := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptQueryCapability);
  CheckFnAvailable(@cryptlib_cryptQueryCapability, DLL_FUNCTIONNAME_cryptQueryCapability);

  @cryptlib_cryptCreateContext := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptCreateContext);
  CheckFnAvailable(@cryptlib_cryptCreateContext, DLL_FUNCTIONNAME_cryptCreateContext);

  @cryptlib_cryptDestroyContext := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptDestroyContext);
  CheckFnAvailable(@cryptlib_cryptDestroyContext, DLL_FUNCTIONNAME_cryptDestroyContext);

  @cryptlib_cryptAddRandom := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptAddRandom);
  CheckFnAvailable(@cryptlib_cryptAddRandom, DLL_FUNCTIONNAME_cryptAddRandom);

  @cryptlib_cryptGenerateKey := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptGenerateKey);
  CheckFnAvailable(@cryptlib_cryptGenerateKey, DLL_FUNCTIONNAME_cryptGenerateKey);

  @cryptlib_cryptEncrypt := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptEncrypt);
  CheckFnAvailable(@cryptlib_cryptEncrypt, DLL_FUNCTIONNAME_cryptEncrypt);

  @cryptlib_cryptDecrypt := GetProcAddress(hCryptLibDLL, DLL_FUNCTIONNAME_cryptDecrypt);
  CheckFnAvailable(@cryptlib_cryptDecrypt, DLL_FUNCTIONNAME_cryptDecrypt);

end;


{ TODO 1 -otdk -crefactor : convert to use bytes instead of ansichar }
// This may *only* be called *after*:
//   cryptlib_LoadDLL(...)
//   cryptlib_cryptInit(...)
//   cryptlib_cryptAddRandom(...)
// This function carries out the procedure described in the cryptlib manual for
// generating random data.
// Specifically, after cryptInit and cryptAddRandom are called, it generates a
// random key, and encrypts an arbitary block of data with it.
// The output from this encryption is the resulting random data.
// In terms of implementation, we use AES to encrypt. We only assume that
// cryptlib is using the minimum keysize (docs refer to AES-128). We can
// therefore only rely on this minimum keysize as being the amount of entropy
// supplied by any key generated
// Because of this, we repeat the procedure, only drawing that number of bits
// each time we obtain random data.
// lengthBytes - Set this to the number of *bytes* of random data to be generated
function cryptlib_RNG(lengthBytes: integer): ansistring;
const
  // The algorithm used is *relativly* arbitary
  CYPHER_ALG = catCRYPT_ALGO_AES;
var
  funcResult: integer;
  buffer: PAnsiChar;
  allOK: boolean;
  cryptContext: TCRYPT_CONTEXT;
  cypherKeysizeBytes: integer;  // In *bytes*
  info: Tcryptlib_CRYPT_QUERY_INFO;
begin
  Result := '';

  // Identify the minimum keysize of the encryption algorithm; this should be
  // the number of random bits cryptlib generates
  funcResult := cryptlib_cryptQueryCapability(
                                      cryptlib_CRYPT_ALGO_TYPE_ID[catCRYPT_ALGO_AES],
                                      @info
                                     );
  allOK := cryptlib_cryptStatusOK(funcResult);
  cypherKeysizeBytes := 0;
  if (allOK) then
    begin
    cypherKeysizeBytes := info.minKeySize; // In *bytes*
    end;

  buffer := AllocMem(cypherKeysizeBytes);
  try
    while (
           allOK AND
           (length(Result) < lengthBytes)
          ) do
      begin
      funcResult := cryptlib_cryptCreateContext(
                           @cryptContext,
                           cryptlib_CRYPT_UNUSED,
                           cryptlib_CRYPT_ALGO_TYPE_ID[CYPHER_ALG]
                          );
      allOK := cryptlib_cryptStatusOK(funcResult);

      if (allOK) then
        begin
        funcResult := cryptlib_cryptGenerateKey(cryptContext);
        allOK := cryptlib_cryptStatusOK(funcResult);

        if (allOK) then
          begin
          funcResult := cryptlib_cryptEncrypt(
                                  cryptContext,
                                  buffer,
                                  cypherKeysizeBytes
                                 );
          allOK := cryptlib_cryptStatusOK(funcResult);

          if (allOK) then
            begin
            Result := Result + Copy(buffer, 1, min(
                                             cypherKeysizeBytes,
                                             (lengthBytes - Length(Result))
                                            ));
            end;
          end;
        end;

      funcResult := cryptlib_cryptDestroyContext(cryptContext);
      allOK := cryptlib_cryptStatusOK(funcResult);

      // Clear buffer ready for next...
      FillChar(buffer^, cypherKeysizeBytes, $00);
      end;  // while loop


  finally
    FreeMem(buffer);
  end;


  // Return an empty string on error
  if not(allOK) then
    begin
    Result := '';
    end;

    
end;



initialization
  hCryptLibDLL := 0;

END.



