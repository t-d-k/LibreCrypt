unit OTFEFreeOTFE_WizardCommon;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

{ TODO 1 -otdk -crefactor : rename as random_utils }
interface

uses
  Windows,
  pkcs11_library
  //doxbox
  ;

type
  TRNG = (
    rngCryptoAPI,
    rngMouseMovement,
    rngcryptlib,
    rngPKCS11,
    rngGPG
    );

  TRNGSet = set of TRNG;


 // Generate RNG data using specified RNG
 // !! WARNING !!
 // If RNG_OPT_CRYPTLIB is specified as the RNG, cryptlibLoad must be called
 // beforehand, and cryptlibUnload after use
function GenerateRandomData(rngset: TRNGSet; bytesRequired: Integer;
  // PKCS#11 specific
  PKCS11Library: TPKCS11Library; SlotID: Integer;
  // GPG specific
  gpgFilename: String;
  // Output
  var randomData: Ansistring): Boolean;
  OVERLOAD;
function GenerateRandomData(rng: TRNG; bytesRequired: Integer;
  // PKCS#11 specific
  PKCS11Library: TPKCS11Library; SlotID: Integer;
  // GPG specific
  gpgFilename: String;
  // Output
  var randomData: Ansistring): Boolean;
  OVERLOAD;


 // Generate RNG data using GPG, assuming GPG is located under the specified
 // filename
// function GenerateRNGDataGPG(bytesRequired: integer; GPGFilename: string; var randomData: TSDUBytes): boolean;

// Generate RNG data using the MS CryptoAPI
function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer; var randomData: Ansistring): Boolean;
  OVERLOAD;
function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer; szContainer: LPCSTR;
  szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD; var randomData: Ansistring): Boolean;
  OVERLOAD;

// Load cryptlib, if possible, and initialise
function cryptlibLoad(): Boolean;
// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: Integer; var randomData: Ansistring): Boolean;
// Unload cryptlib
function cryptlibUnload(): Boolean;

function GenerateRNGDataPKCS11(PKCS11Library: TPKCS11Library; SlotID: Integer;
  bytesRequired: Integer; var randomData: Ansistring): Boolean;

// MouseRNG store
procedure InitMouseRNGData();
procedure AddToMouseRNGData(random: Byte);
function GetMouseRNGData(bytesRequired: Integer; var randomData: Ansistring): Boolean;
function CountMouseRNGData(): Integer;
procedure PurgeMouseRNGData();

implementation

uses
  MSCryptoAPI,
  Dialogs,  // Required for showmessage
            //dosbox
  SDUGeneral,
  OTFEFreeOTFE_cryptlib,
  OTFEFreeOTFE_PKCS11,
  SDUi18n,
  SDUDialogs;

resourcestring
  PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR =
    'Please report seeing this message using the email address specified in the DoxBox documentation, together with a brief description of what you were attempting to do.';

var
  _MouseRNGStore: Ansistring;


// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
function GenerateRandomData(rngset: TRNGSet; bytesRequired: Integer;
  // PKCS#11 specific
  PKCS11Library: TPKCS11Library; SlotID: Integer;
  // GPG specific
  gpgFilename: String;
  // Output
  var randomData: Ansistring): Boolean;
var
  allOK:      Boolean;
  currRNG:    TRNG;
  currRandom: Ansistring;
  rngsUsed:   Integer;
begin
  allOK    := True;
  rngsUsed := 0;

  for currRNG := low(TRNG) to high(TRNG) do begin
    if (currRNG in rngset) then begin
      // SDUInitAndZeroBuffer(0, currRandom );
      currRandom := '';
      allOK      := GenerateRandomData(currRNG, bytesRequired,
        PKCS11Library, SlotID, gpgFilename, currRandom);

      if not (allOK) then begin
        break;
      end else begin
        randomData := SDUXOR(currRandom, randomData);
        Inc(rngsUsed);
      end;

    end;

  end;

  Result := allOK and (rngsUsed > 0);
end;


// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
function GenerateRandomData(rng: TRNG; bytesRequired: Integer;
  // PKCS#11 specific
  PKCS11Library: TPKCS11Library; SlotID: Integer;
  // GPG specific
  gpgFilename: String;
  // Output
  var randomData: Ansistring): Boolean;
var
  allOK: Boolean;
begin
  // SDUInitAndZeroBuffer(0, randomData);
  randomData := '';

  // Generate random data, if this hasn't already been done...
  case rng of
    // Mouse RNG..
    rngMouseMovement:
    begin
      // Nothing to do; the random data has already been generated via the
      // interface
      allOK := GetMouseRNGData(bytesRequired, randomData);
    end;

    // MS CryptoAPI...
    rngCryptoAPI:
    begin
      allOK := GenerateRNGDataMSCryptoAPI(bytesRequired, randomData);
      allOK := allOK and (length(randomData) = bytesRequired);
      if not (allOK) then begin
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
      allOK := GenerateRNGDataCryptlib(bytesRequired, randomData);
      allOK := allOK and (length(randomData) = bytesRequired);
      if not (allOK) then begin
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
      allOK := GenerateRNGDataPKCS11(PKCS11Library, SlotID, bytesRequired,
        randomData);
      allOK := allOK and (length(randomData) = bytesRequired);
      if not (allOK) then begin
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
      allOK := False;
      //      allOK := GenerateRNGDataGPG(
      //                                  bytesRequired,
      //                                  gpgFilename,
      //                                  randomData
      //                                 );
      allOK := allOK and (length(randomData) = bytesRequired);
      if not (allOK) then begin
        SDUMessageDlg(
          _('GPG could not be used to generate random data.') + SDUCRLF +
          SDUCRLF + _('Please doublecheck the path to the GPG executable, and try again.'),
          mtError
          );
      end;
    end;

  else
  begin
    SDUMessageDlg(
      _('Unknown RNG selected?!') + SDUCRLF + SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
      mtError
      );

    allOK := False;
  end;

  end;


  // Sanity check
  if (allOK) then begin
    allOK := (length(randomData) = bytesRequired);
    if not (allOK) then begin
      SDUMessageDlg(
        _('Insufficient random data generated?!') + SDUCRLF + SDUCRLF +
        PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
        mtError
        );

      allOK := False;
    end;

  end;


  Result := allOK;
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

function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer; var randomData: Ansistring): Boolean;
var
  gotData: Boolean;
begin
  gotData := False;

  if not (gotData) then begin
    gotData := GenerateRNGDataMSCryptoAPI(bytesRequired, nil, MS_DEF_PROV,
      PROV_RSA_FULL, (CRYPT_VERIFYCONTEXT or CRYPT_MACHINE_KEYSET), randomData);
  end;
  if not (gotData) then begin
    gotData := GenerateRNGDataMSCryptoAPI(bytesRequired, nil, MS_DEF_PROV,
      PROV_RSA_FULL, (CRYPT_VERIFYCONTEXT or CRYPT_MACHINE_KEYSET or
      CRYPT_NEWKEYSET), randomData);
  end;
  if not (gotData) then begin
    gotData := GenerateRNGDataMSCryptoAPI(bytesRequired, '', '',
      PROV_RSA_FULL, CRYPT_VERIFYCONTEXT, randomData);
  end;

  Result := gotData;
end;


function GenerateRNGDataMSCryptoAPI(bytesRequired: Integer; szContainer: LPCSTR;
  szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD; var randomData: Ansistring): Boolean;
var
  hProv: HCRYPTPROV;
  allOK: Boolean;
begin
  allOK := False;

  if CryptAcquireContext(@hProv, szContainer, szProvider, dwProvType, dwFlags) then begin
    // Cleardown...
    // This is required in order that CrypGenRandom can overwrite with random
    // data
    // SDUInitAndZeroBuffer(bytesRequired, randomData );
    randomData := StringOfChar(AnsiChar(#0), bytesRequired);
    allOK      := CryptGenRandom(hProv, bytesRequired, PByte(randomData));
    { TODO 1 -otdk -cinvestigate : PByte(string) can you do this? }

    CryptReleaseContext(hProv, 0);
  end;


  Result := allOK;
end;


// Load cryptlib, if possible, and initialise
function cryptlibLoad(): Boolean;
var
  funcResult: Integer;
  allOK:      Boolean;
begin
  allOK := cryptlib_LoadDLL();
  if (allOK) then begin
    funcResult := cryptlib_cryptInit();
    allOK      := cryptlib_cryptStatusOK(funcResult);
    if (allOK) then begin
      funcResult := cryptlib_cryptAddRandom(nil, cryptlib_CRYPT_RANDOM_SLOWPOLL);
      allOK      := cryptlib_cryptStatusOK(funcResult);
      // If there was a problem, call end
      if (not (allOK)) then begin
        cryptlib_cryptEnd();
      end;

    end;

    // If there was a problem, unload the DLL
    if (not (allOK)) then begin
      cryptlib_UnloadDLL();
    end;
  end;

  Result := allOK;
end;


// Unload cryptlib
function cryptlibUnload(): Boolean;
var
  funcResult: Integer;
begin
  Result := True;

  // Call "end" function on the DLL
  funcResult := cryptlib_cryptEnd();
  Result      := Result and cryptlib_cryptStatusOK(funcResult);

  // Unload the DLL
  Result := Result and cryptlib_UnloadDLL();
end;


// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: Integer; var randomData: Ansistring): Boolean;
begin
  // randomData := SDUStringToSDUBytes(cryptlib_RNG(bytesRequired));
  randomData := cryptlib_RNG(bytesRequired);
  Result      := (Length(randomData) = bytesRequired);
end;

// Initialize MouseRNG random data store
procedure InitMouseRNGData();
begin
  //  SDUInitAndZeroBuffer(0, _MouseRNGStore);
  _MouseRNGStore := '';
end;

// Add byte to MouseRNG store
procedure AddToMouseRNGData(random: Byte);
begin
  // SDUAddByte(_MouseRNGStore, random);
  _MouseRNGStore := _MouseRNGStore + Ansichar(random);
end;

// Get first bytesRequired bytes of data from the MouseRNG store
function GetMouseRNGData(bytesRequired: Integer; var randomData: Ansistring): Boolean;
begin
  Result := False;

  // SDUInitAndZeroBuffer(0, randomData);
  randomData := '';
  if ((bytesRequired * 8) <= CountMouseRNGData()) then begin
    randomData := Copy(_MouseRNGStore, 1, bytesRequired);
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
  for i := 1 to length(_MouseRNGStore) do begin
    //  _MouseRNGStore[i] := Byte(i);
    _MouseRNGStore := Ansichar(i);
  end;

  // setlength(_MouseRNGStore,0);
  _MouseRNGStore := '';
end;


function GenerateRNGDataPKCS11(PKCS11Library: TPKCS11Library; SlotID: Integer;
  bytesRequired: Integer; var randomData: Ansistring): Boolean;
var
  allOK: Boolean;
begin
  allOK := GetPKCS11RandomData(PKCS11Library, SlotID, bytesRequired, RandomData);
  if not (allOK) then begin
    SDUMessageDlg(
      _('The selected PKCS#11 token could not be used to generate random data.') +
      SDUCRLF + SDUCRLF + _('Please select another RNG, and try again'),
      mtError
      );
  end;

  Result := allOK;
end;

end.
