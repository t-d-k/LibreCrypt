unit SDURandPool;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

{
copyright tdk
my code dual licenced under FreeOTFE licence and LGPL
code marked as 'sdean' is freeotfe licence only

was OTFEFreeOTFE_WizardCommon
keeps a random pool - reads as needed from OS random sources ad internal 'mouse data' pool

}

interface

uses
  sysutils,
  Windows,//sdu
  SDUGeneral,
  //doxbox
  pkcs11_library;

type
  TRNG = (
    rngCryptoAPI,
    rngMouseMovement,
    rngcryptlib,
    rngPKCS11,
    rngGPG
    );

  TRNGSet             = set of TRNG;
  EInsufficientRandom = class (Exception);

  TRandPool = class (TObject)
  PRIVATE
    { private declarations }
  PROTECTED

    // Generate RNG data using GPG, assuming GPG is located under the specified
    // filename
    // function GenerateRNGDataGPG(bytesRequired: integer; GPGFilename: string; var randomData: TSDUBytes): boolean;

    // Generate RNG data using the MS CryptoAPI
    class function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer;
      out randomData: TSDUBytes): Boolean;
      OVERLOAD;
    class function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer;
      szContainer: LPCSTR; szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD;
      out randomData: TSDUBytes): Boolean;
      OVERLOAD;
    class function GenerateRandomData1Rng(rng: TRNG; bytesRequired: Integer;
    // Output
      out randomData: TSDUBytes): Boolean;
      OVERLOAD;
  PUBLIC
                          { public declarations }
    constructor Create(); // singleton - dont call directly - use GetRandPool
    destructor Destroy(); //noramly only called on app exit
                          // Generate RNG data using specified RNG
                          // !! WARNING !!
                          // If RNG_OPT_CRYPTLIB is specified as the RNG, cryptlibLoad must be called
                          // beforehand, and cryptlibUnload after use
    class procedure GetRandomData(bytesRequired: Integer;
    // Output
      out randomData: TSDUBytes);

    class function CanUseCryptLib(): Boolean;
    //obsolete
    class procedure GenerateRandomData(bytesRequired: Integer;
    // Output
      var randomData: Ansistring);
      OVERLOAD;

    // gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
    class procedure SetUpRandPool(rngset: TRNGSet;
    // PKCS#11 specific
      PKCS11Library: TPKCS11Library; SlotID: Integer;
    // GPG specific
      gpgFilename: String);



  PUBLISHED

  end;

// MouseRNG store
procedure InitMouseRNGData();
procedure AddToMouseRNGData(random: Byte);
function GetMouseRNGData(bytesRequired: Integer; var randomData: TSDUBytes): Boolean;
function CountMouseRNGData(): Integer;
procedure PurgeMouseRNGData();



function GetRandPool(): TRandPool;

resourcestring
  PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR =
    'Please report seeing this message using the email address specified in the DoxBox documentation, together with a brief description of what you were attempting to do.';

implementation

uses
  Dialogs, MSCryptoAPI,
  // Required for showmessage
  //sdu
  SDUDialogs,
  SDUi18n,
  //dosbox
  OTFEFreeOTFE_cryptlib,
  OTFEFreeOTFE_PKCS11;

{
// Load cryptlib, if possible, and initialise
function cryptlibLoad(): Boolean;
// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: Integer; var randomData: Ansistring): Boolean;
// Unload cryptlib
function cryptlibUnload(): Boolean;

function GenerateRNGDataPKCS11(PKCS11Library: TPKCS11Library; SlotID: Integer;
  bytesRequired: Integer; var randomData: Ansistring): Boolean;
}


var
  _MouseRNGStore:  TSDUBytes;
  _RandPool:       TRandPool;
  _rngset:         TRNGSet;
  _PKCS11Library:  TPKCS11Library;
  _PKCS11SlotID:   Integer;
  _gpgFilename:    String;
  _inited:         Boolean = False;
  _CanUseCryptlib: Boolean = False;

// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: Integer; var randomData: TSDUBytes): Boolean;
begin
  randomData := SDUStringToSDUBytes(cryptlib_RNG(bytesRequired));
  //  randomData := cryptlib_RNG(bytesRequired);
  Result     := (Length(randomData) = bytesRequired);
end;

function GenerateRNGDataPKCS11(PKCS11Library: TPKCS11Library; SlotID: Integer;
  bytesRequired: Integer; var randomData: TSDUBytes): Boolean;
begin
  Result := GetPKCS11RandomData(PKCS11Library, SlotID, bytesRequired, RandomData);
  if not Result then begin
    SDUMessageDlg(
      _('The selected PKCS#11 token could not be used to generate random data.') +
      SDUCRLF + SDUCRLF + _('Please select another RNG, and try again'),
      mtError
      );
  end;
end;


// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
class function TRandPool.GenerateRandomData1Rng(rng: TRNG; bytesRequired: Integer;
  // Output
  out randomData: TSDUBytes): Boolean;

begin
  { TODO -otdk -crefactor : raise exception on error instead of result }
  SDUInitAndZeroBuffer(0, randomData);
  //  randomData := '';

  // Generate random data, if this hasn't already been done...
  case rng of
    // Mouse RNG..
    rngMouseMovement:
    begin
      // Nothing to do; the random data has already been generated via the
      // interface
      Result := GetMouseRNGData(bytesRequired, randomData);
    end;

    // MS CryptoAPI...
    rngCryptoAPI:
    begin
      Result := GenerateRNGDataMSCryptoAPI(bytesRequired, randomData);
      Result := Result and (length(randomData) = bytesRequired);
      if not Result then begin
        SDUMessageDlg(
          _('The MS CryptoAPI could not be used to generate random data.') +
          SDUCRLF + SDUCRLF + _('Please select another RNG, and try again'),
          mtError
          );
      end;
    end;

    // cryptlib...
    rngcryptlib:
    begin
      Result := GenerateRNGDataCryptlib(bytesRequired, randomData);
      Result := Result and (length(randomData) = bytesRequired);
      if not Result then begin
        SDUMessageDlg(
          _('cryptlib could not be used to generate random data.') + SDUCRLF +
          SDUCRLF + _(
          'Please ensure the cryptlib DLL is in the correct location, and try again'),
          mtError
          );
      end;
    end;

    // PKCS#11 token...
    rngPKCS11:
    begin
      Result := GenerateRNGDataPKCS11(_PKCS11Library, _PKCS11SlotID, bytesRequired,
        randomData);
      Result := Result and (length(randomData) = bytesRequired);
      if not Result then begin
        SDUMessageDlg(
          _('PKCS#11 token could not be used to generate random data.') +
          SDUCRLF + SDUCRLF + _(
          'Please ensure token is inserted and configured correctly'),
          mtError
          );
      end;
    end;

    // GPG...
    rngGPG:
    begin
      // not implemented
      Result := False;
      //      allOK := GenerateRNGDataGPG(
      //                                  bytesRequired,
      //                                  gpgFilename,
      //                                  randomData
      //                                 );
      Result := Result and (length(randomData) = bytesRequired);
      if not Result then begin
        SDUMessageDlg(
          _('GPG could not be used to generate random data.') + SDUCRLF +
          SDUCRLF + _('Please doublecheck the path to the GPG executable, and try again.'),
          mtError
          );
      end;
    end;

  else
  begin
    raise Exception.Create('Unknown RNG selected' + SDUCRLF + SDUCRLF +
      PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR);
   { SDUMessageDlg(
      _('Unknown RNG selected?!') + SDUCRLF + SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
      mtError
      );}

    Result := False;
  end;

  end;

  // Sanity check
  if Result then begin
    Result := (length(randomData) = bytesRequired);
    if not Result then begin
      raise EInsufficientRandom.Create(Format('%d bytes required %d found',
        [bytesRequired, length(randomData)]));
     { SDUMessageDlg(
        _('Insufficient random data generated?!') + SDUCRLF + SDUCRLF +
        PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
        mtError
        );
      }
      Result := False;
    end;

  end;
end;

 //obsolete
 // gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
class procedure TRandPool.GenerateRandomData(bytesRequired: Integer;
  // Output
  var randomData: Ansistring);

var
  randomBytes: TSDUBytes;
begin
  GetRandomData(bytesRequired, randomBytes);
  randomData := SDUBytesToString(randomBytes);
end;

// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
class procedure TRandPool.SetUpRandPool(rngset: TRNGSet;
  // PKCS#11 specific
  PKCS11Library: TPKCS11Library; SlotID: Integer;
  // GPG specific
  gpgFilename: String);
begin
  _rngset        := rngset;
  _PKCS11Library := PKCS11Library;
  _PKCS11SlotID  := SlotID;
  _gpgFilename   := gpgFilename;
  _inited        := True;
end;

class procedure TRandPool.GetRandomData(bytesRequired: Integer;
  // Output
  out randomData: TSDUBytes);
var
  currRNG:    TRNG;
  currRandom: TSDUBytes;
  rngsUsed:   Integer;
  ok:         Boolean;
begin
  assert(_inited);

  rngsUsed := 0;

  for currRNG := low(TRNG) to high(TRNG) do begin
    if (currRNG in _rngset) then begin
      SDUInitAndZeroBuffer(0, currRandom);
      ok := GenerateRandomData1Rng(currRNG, bytesRequired, currRandom);

      if not ok then begin
        //        break;
        raise EInsufficientRandom.Create('');
      end else begin
        randomData := SDUXOR(currRandom, randomData);
        Inc(rngsUsed);
      end;

    end;

  end;

  if (rngsUsed <= 0) then
    raise Exception.Create('No RNG selected');
end;


  (*
function GenerateRNGDataGPG(bytesRequired: integer; GPGFilename: string; var randomData: TSDUBytes): boolean;
begin
// xxx - implement GPG integration
showmessage('Using GPG to generate RNG data has not yet been implemented. Please select another RNG option');
randomData:= '';
Result := FALSE;
end;
     *)

class function TRandPool.GenerateRNGDataMSCryptoAPI(bytesRequired: Integer;
  out randomData: TSDUBytes): Boolean;
begin

  Result := GenerateRNGDataMSCryptoAPI(bytesRequired, nil, MS_DEF_PROV,
    PROV_RSA_FULL, (CRYPT_VERIFYCONTEXT or CRYPT_MACHINE_KEYSET), randomData);

  if not (Result) then begin
    Result := GenerateRNGDataMSCryptoAPI(bytesRequired, nil, MS_DEF_PROV,
      PROV_RSA_FULL, (CRYPT_VERIFYCONTEXT or CRYPT_MACHINE_KEYSET or CRYPT_NEWKEYSET),
      randomData);
  end;
  if not (Result) then begin
    Result := GenerateRNGDataMSCryptoAPI(bytesRequired, '', '', PROV_RSA_FULL,
      CRYPT_VERIFYCONTEXT, randomData);
  end;
end;


class function TRandPool.GenerateRNGDataMSCryptoAPI(bytesRequired: Integer;
  szContainer: LPCSTR; szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD;
  out randomData: TSDUBytes): Boolean;
var
  hProv: HCRYPTPROV;
begin
  Result := False;

  if CryptAcquireContext(@hProv, szContainer, szProvider, dwProvType, dwFlags) then begin
    // Cleardown...
    // This is required in order that CrypGenRandom can overwrite with random
    // data
    SDUInitAndZeroBuffer(bytesRequired, randomData);
    //    randomData := StringOfChar(AnsiChar(#0), bytesRequired);
    Result := CryptGenRandom(hProv, bytesRequired, PByte(randomData));
    { TODO 1 -otdk -cinvestigate : PByte(string) can you do this? }

    CryptReleaseContext(hProv, 0);
  end;
end;


// Load cryptlib, if possible, and initialise
function cryptlibLoad(): Boolean;
var
  funcResult: Integer;
begin
  Result := cryptlib_LoadDLL();
  if Result then begin
    funcResult := cryptlib_cryptInit();
    Result     := cryptlib_cryptStatusOK(funcResult);
    if Result then begin
      funcResult := cryptlib_cryptAddRandom(nil, cryptlib_CRYPT_RANDOM_SLOWPOLL);
      Result     := cryptlib_cryptStatusOK(funcResult);
      // If there was a problem, call end
      if not Result then
        cryptlib_cryptEnd();
    end;

    // If there was a problem, unload the DLL
    if not Result then
      cryptlib_UnloadDLL();
  end;
end;


// Unload cryptlib
function cryptlibUnload(): Boolean;
var
  funcResult: Integer;
begin
  // Call "end" function on the DLL
  funcResult := cryptlib_cryptEnd();
  Result     := cryptlib_cryptStatusOK(funcResult);

  // Unload the DLL
  Result := Result and cryptlib_UnloadDLL();
end;



// Initialize MouseRNG random data store
procedure InitMouseRNGData();
begin
  SDUInitAndZeroBuffer(0, _MouseRNGStore);
  //  _MouseRNGStore := '';
end;

// Add byte to MouseRNG store
procedure AddToMouseRNGData(random: Byte);
begin
  SDUAddByte(_MouseRNGStore, random);
  //  _MouseRNGStore := _MouseRNGStore + Ansichar(random);
end;

// Get first bytesRequired bytes of data from the MouseRNG store
function GetMouseRNGData(bytesRequired: Integer; var randomData: TSDUBytes): Boolean;
begin
  Result := False;

  SDUInitAndZeroBuffer(0, randomData);
  //  randomData := '';
  if ((bytesRequired * 8) <= CountMouseRNGData()) then begin
    randomData := Copy(_MouseRNGStore, 0, bytesRequired);
    Result     := True;
  end;
end;

// Count the number of *bits* in the MouseRNG store
function CountMouseRNGData(): Integer;
begin
  Result := length(_MouseRNGStore) * 8;
end;

// Purge the current MouseRNG store
procedure PurgeMouseRNGData();
var
  i: Integer;
begin
  // Simple overwrite
  //  for i := 1 to length(_MouseRNGStore) do begin
  //      _MouseRNGStore[i] := Byte(i);
  ////    _MouseRNGStore := Ansichar(i);
  //  end;
  SDUInitAndZeroBuffer(0, _MouseRNGStore);
  //  _MouseRNGStore := '';
end;



// tdk code

{ TRandPool }
class function TRandPool.CanUseCryptLib: Boolean;
begin
  Result := _CanUseCryptlib;
end;

constructor TRandPool.Create;
begin
  assert(_RandPool = nil, 'dont call ctor - use GetRandPool');

  // Start cryptlib, if possible, as early as we can to allow it as much time
  // as possible to poll entropy
  _CanUseCryptlib := cryptlibLoad();
end;


destructor TRandPool.Destroy;
begin
  // Shutdown cryptlib, if used
  if _CanUseCryptlib then
    cryptlibUnload();
  _CanUseCryptlib := False;
end;

function GetRandPool(): TRandPool;
begin
  if _RandPool = nil then
    _RandPool := TRandPool.Create();
  Result := _RandPool;
end;

initialization
  _RandPool := nil;

finalization
  if _RandPool <> nil then
    FreeAndNil(_RandPool);

end.
