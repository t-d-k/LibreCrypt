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
  pkcs11_library;

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
function GenerateRandomData(
                            rngset: TRNGSet;
                            bytesRequired: integer;
                            // PKCS#11 specific
                            PKCS11Library: TPKCS11Library;
                            SlotID: integer;
                            // GPG specific
                            gpgFilename: string;
                            // Output
                            var randomData: ansistring
                           ): boolean; overload;
function GenerateRandomData(
                            rng: TRNG;
                            bytesRequired: integer;
                            // PKCS#11 specific
                            PKCS11Library: TPKCS11Library;
                            SlotID: integer;
                            // GPG specific
                            gpgFilename: string;
                            // Output
                            var randomData: ansistring
                           ): boolean; overload;


// Generate RNG data using GPG, assuming GPG is located under the specified
// filename
function GenerateRNGDataGPG(bytesRequired: integer; GPGFilename: string; var randomData: ansistring): boolean;

// Generate RNG data using the MS CryptoAPI
function GenerateRNGDataMSCryptoAPI(
  bytesRequired: integer;
  var randomData: ansistring
): boolean; overload;
function GenerateRNGDataMSCryptoAPI(
  bytesRequired: integer;
  szContainer: LPCSTR;
  szProvider: LPCSTR;
  dwProvType: DWORD;
  dwFlags: DWORD;
  var randomData: ansistring
): boolean; overload;

// Load cryptlib, if possible, and initialise
function cryptlibLoad(): boolean;
// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: integer; var randomData: ansistring): boolean;
// Unload cryptlib
function cryptlibUnload(): boolean;

function GenerateRNGDataPKCS11(
                               PKCS11Library: TPKCS11Library;
                               SlotID: integer;
                               bytesRequired: integer;
                               var randomData: ansistring
                              ): boolean;

// MouseRNG store
procedure InitMouseRNGData();
procedure AddToMouseRNGData(random: Byte);
function GetMouseRNGData(bytesRequired: integer; var randomData: ansistring): boolean;
function CountMouseRNGData(): integer;
procedure PurgeMouseRNGData();

implementation

uses
  OTFEFreeOTFE_cryptlib,
  OTFEFreeOTFE_PKCS11,
  MSCryptoAPI,
  Dialogs,  // Required for showmessage
  SDUGeneral,
  SDUi18n,
  SDUDialogs;

resourcestring
  PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR = 'Please report seeing this message using the email address specified in the DoxBox documentation, together with a brief description of what you were attempting to do.';

var
  _MouseRNGStore: Ansistring;


// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
function GenerateRandomData(
                            rngset: TRNGSet;
                            bytesRequired: integer;
                            // PKCS#11 specific
                            PKCS11Library: TPKCS11Library;
                            SlotID: integer;
                            // GPG specific
                            gpgFilename: string;
                            // Output
                            var randomData: ansistring
                           ): boolean;
var
  allOK: boolean;
  currRNG: TRNG;
  currRandom: ansistring;
  rngsUsed: integer;
begin
  allOK := TRUE;
  rngsUsed := 0;

  for currRNG := low(TRNG) to high(TRNG) do
    begin
    if (currRNG in rngset) then
      begin
      currRandom := '';
      allOK := GenerateRandomData(
                                  currRNG,
                                  bytesRequired,
                                  PKCS11Library,
                                  SlotID,
                                  gpgFilename,
                                  currRandom
                                 );

      if not(allOK) then
        begin
        break;
        end
      else
        begin
        randomData := SDUXOR(currRandom, randomData);
        inc(rngsUsed);
        end;

      end;

    end;

  Result := allOK and (rngsUsed > 0);
end;


// gpgFilename - Only MANDATORY if RNG_OPT_GPG specified as RNG
function GenerateRandomData(
                            rng: TRNG;
                            bytesRequired: integer;
                            // PKCS#11 specific
                            PKCS11Library: TPKCS11Library;
                            SlotID: integer;
                            // GPG specific
                            gpgFilename: string;
                            // Output
                            var randomData: ansistring
                           ): boolean;
var
  allOK: boolean;
begin
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
      allOK := allOK AND
              (length(randomData) = bytesRequired);
      if not(allOK) then
        begin
        SDUMessageDlg(
                   _('The MS CryptoAPI could not be used to generate random data.')+SDUCRLF+
                   SDUCRLF+
                   _('Please select another RNG, and try again'),
                   mtError
                  );
        end;
      end;

    // cryptlib...
    rngcryptlib:
      begin
      allOK := GenerateRNGDataCryptlib(bytesRequired, randomData);
      allOK := allOK AND
              (length(randomData) = bytesRequired);
      if not(allOK) then
        begin
        SDUMessageDlg(
                   _('cryptlib could not be used to generate random data.')+SDUCRLF+
                   SDUCRLF+
                   _('Please ensure the cryptlib DLL is in the correct location, and try again'),
                   mtError
                  );
        end;
      end;

    // PKCS#11 token...
    rngPKCS11:
      begin
      allOK := GenerateRNGDataPKCS11(
                                     PKCS11Library,
                                     SlotID,
                                     bytesRequired,
                                     randomData
                                    );
      allOK := allOK AND
              (length(randomData) = bytesRequired);
      if not(allOK) then
        begin
        SDUMessageDlg(
                   _('PKCS#11 token could not be used to generate random data.')+SDUCRLF+
                   SDUCRLF+
                   _('Please ensure token is inserted and configured correctly'),
                   mtError
                  );
        end;
      end;

    // GPG...
    rngGPG:
      begin
      allOK := GenerateRNGDataGPG(
                                  bytesRequired,
                                  gpgFilename,
                                  randomData
                                 );
      allOK := allOK AND
              (length(randomData) = bytesRequired);
      if not(allOK) then
        begin
        SDUMessageDlg(
                   _('GPG could not be used to generate random data.')+SDUCRLF+
                   SDUCRLF+
                   _('Please doublecheck the path to the GPG executable, and try again.'),
                   mtError
                  );
        end;
        end;

    else
      begin
      SDUMessageDlg(
                 _('Unknown RNG selected?!')+SDUCRLF+
                 SDUCRLF+
                 PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
                 mtError
                );

      allOK := FALSE;
      end;
      
    end;


  // Sanity check
  if (allOK) then
    begin
    allOK := (length(randomData) = bytesRequired);
    if not(allOK) then
      begin
      SDUMessageDlg(
                 _('Insufficient random data generated?!')+SDUCRLF+
                 SDUCRLF+
                 PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
                 mtError
                );

      allOK := FALSE;
      end;

    end;


  Result := allOK;
end;


function GenerateRNGDataGPG(bytesRequired: integer; GPGFilename: string; var randomData: ansistring): boolean;
begin
// xxx - implement GPG integration
showmessage('Using GPG to generate RNG data has not yet been implemented. Please select another RNG option');
randomData:= '';
Result := FALSE;
end;


function GenerateRNGDataMSCryptoAPI(
  bytesRequired: integer;
  var randomData: ansistring
): boolean;
var
  gotData: boolean;
begin
  gotData := FALSE;

  if not(gotData) then
    begin
    gotData := GenerateRNGDataMSCryptoAPI(
                         bytesRequired,
                         nil,
                         MS_DEF_PROV,
                         PROV_RSA_FULL,
                         (
                          CRYPT_VERIFYCONTEXT or
                          CRYPT_MACHINE_KEYSET
                         ),
                         randomData
                        );
    end;
  if not(gotData) then
    begin
    gotData := GenerateRNGDataMSCryptoAPI(
                         bytesRequired,
                         nil,
                         MS_DEF_PROV,
                         PROV_RSA_FULL,
                         (
                          CRYPT_VERIFYCONTEXT or
                          CRYPT_MACHINE_KEYSET or
                          CRYPT_NEWKEYSET
                         ),
                         randomData
                        );
    end;
  if not(gotData) then
    begin
    gotData := GenerateRNGDataMSCryptoAPI(
                         bytesRequired,
                         '',
                         '',
                         PROV_RSA_FULL,
                         CRYPT_VERIFYCONTEXT,
                         randomData
                        );
    end;

  Result := gotData;
end;


function GenerateRNGDataMSCryptoAPI(
  bytesRequired: integer;
  szContainer: LPCSTR;
  szProvider: LPCSTR;
  dwProvType: DWORD;
  dwFlags: DWORD;
  var randomData: ansistring
): boolean;
var
  hProv: HCRYPTPROV;
  allOK: boolean;
begin
  allOK := FALSE;
  
  if CryptAcquireContext(
                         @hProv,
                         szContainer,
                         szProvider,
                         dwProvType,
                         dwFlags
                        ) then
    begin
    // Cleardown...
    // This is required in order that CrypGenRandom can overwrite with random
    // data
    randomData := StringOfChar(AnsiChar(#0), bytesRequired);

    allOK:= CryptGenRandom(hProv, bytesRequired, PByte(randomData));

    CryptReleaseContext(hProv, 0);
    end;


  Result := allOK;
end;


// Load cryptlib, if possible, and initialise
function cryptlibLoad(): boolean;
var
  funcResult: integer;
  allOK: boolean;
begin
  allOK := cryptlib_LoadDLL();
  if (allOK) then
    begin
    funcResult := cryptlib_cryptInit();
    allOK := cryptlib_cryptStatusOK(funcResult);
    if (allOK) then
      begin
      funcResult := cryptlib_cryptAddRandom(
                                            nil,
                                            cryptlib_CRYPT_RANDOM_SLOWPOLL
                                           );
      allOK := cryptlib_cryptStatusOK(funcResult);
      // If there was a problem, call end
      if (not(allOK) ) then
        begin
        cryptlib_cryptEnd();
        end;

      end;

    // If there was a problem, unload the DLL
    if (not(allOK)) then
      begin
      cryptlib_UnloadDLL();
      end;
    end;

  Result := allOK;
end;


// Unload cryptlib
function cryptlibUnload(): boolean;
var
  funcResult: integer;
  allOK: boolean;
begin
  allOK := TRUE;

  // Call "end" function on the DLL
  funcResult := cryptlib_cryptEnd();
  allOK := allOK AND cryptlib_cryptStatusOK(funcResult);

  // Unload the DLL
  allOK := allOK AND cryptlib_UnloadDLL();

  Result := allOK;
end;


// Generate RNG data using cryptlib
function GenerateRNGDataCryptlib(bytesRequired: integer; var randomData: ansistring): boolean;
var
  allOK: boolean;
begin
  randomData := cryptlib_RNG(bytesRequired);
  allOK := (Length(randomData) = bytesRequired);

  Result := allOK;
end;

// Initialize MouseRNG random data store
procedure InitMouseRNGData();
begin
  _MouseRNGStore := '';
end;

// Add byte to MouseRNG store
procedure AddToMouseRNGData(random: Byte);
begin
  _MouseRNGStore:= _MouseRNGStore + Ansichar(random);
end;

// Get first bytesRequired bytes of data from the MouseRNG store
function GetMouseRNGData(bytesRequired: integer; var randomData: ansistring): boolean;
var
  retval: boolean;
begin
  retval := FALSE;

  randomData := '';
  if ((bytesRequired * 8) <= CountMouseRNGData()) then
    begin
    randomData := Copy(_MouseRNGStore, 1, bytesRequired);
    retval := TRUE;
    end;

  Result := retval;
end;

// Count the number of *bits* in the MouseRNG store
function CountMouseRNGData(): integer;
begin
  Result := length(_MouseRNGStore) * 8;
end;

// Purge the current MouseRNG store
procedure PurgeMouseRNGData();
var
  i: integer;
begin
  // Simple overwrite
  for i:=1 to length(_MouseRNGStore) do
    begin
    _MouseRNGStore := Ansichar(i);
    end;

  _MouseRNGStore := '';
end;


function GenerateRNGDataPKCS11(
                               PKCS11Library: TPKCS11Library;
                               SlotID: integer;
                               bytesRequired: integer;
                               var randomData: ansistring
                              ): boolean;
var
  allOK: boolean;
begin
  allOK := GetPKCS11RandomData(
                               PKCS11Library,
                               SlotID,
                               bytesRequired,
                               RandomData
                              );
  if not(allOK) then
    begin
    SDUMessageDlg(
               _('The selected PKCS#11 token could not be used to generate random data.')+SDUCRLF+
               SDUCRLF+
               _('Please select another RNG, and try again'),
               mtError
              );
    end;

  Result := allOK;
end;

END.


