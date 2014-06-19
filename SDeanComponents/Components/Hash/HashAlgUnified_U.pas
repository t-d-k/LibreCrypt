unit HashAlgUnified_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  classes,
  HashAlg_U,
  HashValue_U;

type
  // !! WARNING !!
  // When updating this array; ensure that a corresponding hash object is
  // created in the constructor
  fhHashType = (
                hashMD2,
                hashMD4,
                hashMD5,
                hashSHA,
                hashSHA1,
                hashSHA256,
                hashSHA384,
                hashSHA512,
                hashRIPEMD128,
                hashRIPEMD160,
                hashRIPEMD256,
                hashRIPEMD320,
                hashGOST,
                hashTiger
               );

type
  THashAlgUnified = class(THashAlg)
  private
    { Private declarations }
  protected
    fHashObjs: array [fhHashType] of THashAlg;
    fActiveHash: fhHashType;

    procedure SetActiveHash(hash: fhHashType);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    // Additional "helper" function
    function GetHashTypeForTitle(hashTitle: string): fhHashType;
    function GetHashTitleForType(hashType: fhHashType): string;

    // ------------------------------------------------------------------
    // Main methods
    //
    // Most users will only need to use the following methods:

    // Simple function to hash a string
    function  HashString(const theString: string; digest: THashValue): boolean; override;

    // Simple function to hash a file's contents
    function  HashFile(const filename: string; digest: THashValue): boolean; override;

    // Simple function to hash a stream
    // Note: This will operate from the *current* *position* in the stream, right
    //       up to the end
    function  HashStream(const stream: TStream; digest: THashValue): boolean; override;

    // Generate a prettyprinted version of the supplied hash value, using the
    // conventional ASCII-hex byte order for displaying values by the hash
    // algorithm.
    // Note: ValueAsASCIIHex(...) can be called on hash value objects instead,
    //       though this will only give a non-prettyprinted version
    function  PrettyPrintHashValue(const theHashValue: THashValue): string; override;


    // ------------------------------------------------------------------
    // Lower level functions to incrementally generate a hash
    //
    // Most users probably won't need to use these
    // Note: Only Init(...), Update(<with byte array>), and Final(...) should
    //       be implemented in descendant classes. The others will call this in
    //       this class.
    // Usage: Call:
    //   Call Init(...)
    //   Call Update(...)
    //   Call Update(...)
    //     ...                (call Update(...) repeatedly until all  your data
    //     ...                is processed)
    //   Call Update(...)
    //   Call Final(...)
    procedure Init(); override;
    procedure Update(const input: string); override;
    procedure Update(const input: array of byte; const inputLen: cardinal); override;
    // Note: This will operate from the *current* *position* in the stream
    procedure Update(input: TStream; const inputLen: int64); override;
    // Note: This will operate from the *current* *position* in the stream, right
    //       up to the end
    procedure Update(input: TStream); override;
    procedure Final(digest: THashValue); override;


    // ------------------------------------------------------------------
    // Depreciated methods
    //
    // The following methods are depreciated, and should not be used in new
    // developments.
    function  HashString(theString: string): THashArray; overload; override; deprecated;
    function  HashFile(filename: string; var digest: THashArray): boolean; override; deprecated;
    function  HashToDisplay(theHash: THashArray): string; override; deprecated;
    function  HashToDataString(theHash: THashArray): string; override; deprecated;
    procedure ClearHash(var theHash: THashArray); override; deprecated;
    function  DigestSize(): integer; override; deprecated;
  published
    property ActiveHash: fhHashType read fActiveHash write SetActiveHash;

  end;

procedure Register;

implementation

uses
  sysutils,
  HashAlgMD2_U,
  HashAlgMD4_U,
  HashAlgMD5_U,
  HashAlgSHA_U,
  HashAlgSHA1_U,
  HashAlgSHA256_U,
  HashAlgSHA384_U,
  HashAlgSHA512_U,
  HashAlgRIPEMD_U,
  HashAlgGOST_U,
  HashAlgTiger_U;


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgUnified]);
end;


constructor THashAlgUnified.Create(AOwner: TComponent);
begin
  inherited;

  // Create objects
  fHashObjs[hashMD2] := THashAlgMD2.Create(nil);
  fHashObjs[hashMD4] := THashAlgMD4.Create(nil);
  fHashObjs[hashMD5] := THashAlgMD5.Create(nil);
  fHashObjs[hashSHA] := THashAlgSHA.Create(nil);
  fHashObjs[hashSHA1] := THashAlgSHA1.Create(nil);
  fHashObjs[hashSHA256] := THashAlgSHA256.Create(nil);
  fHashObjs[hashSHA384] := THashAlgSHA384.Create(nil);
  fHashObjs[hashSHA512] := THashAlgSHA512.Create(nil);
  fHashObjs[hashRIPEMD128] := THashAlgRIPEMD128.Create(nil);
  fHashObjs[hashRIPEMD160] := THashAlgRIPEMD160.Create(nil);
  fHashObjs[hashRIPEMD256] := THashAlgRIPEMD256.Create(nil);
  fHashObjs[hashRIPEMD320] := THashAlgRIPEMD320.Create(nil);
  fHashObjs[hashGOST] := THashAlgGOST.Create(nil);
  fHashObjs[hashTiger] := THashAlgTiger.Create(nil);

  // Default to SHA-1 (can be any...)
  ActiveHash := hashSHA1;

end;


destructor THashAlgUnified.Destroy();
var
  i: fhHashType;
begin
  for i:=low(fHashObjs) to high(fHashObjs) do
    begin
    fHashObjs[i].Free();
    end;

  inherited;
end;


function THashAlgUnified.GetHashTypeForTitle(hashTitle: string): fhHashType;
var
  i: fhHashType;
  retVal: fhHashType;
begin
  retVal := hashMD2; // There is no sensible default

  for i:=low(fHashObjs) to high(fHashObjs) do
    begin
    if (uppercase(fHashObjs[i].Title) = uppercase(hashTitle)) then
      begin
      retVal := i;
      break;
      end;
    end;

  Result := retVal;
end;


function THashAlgUnified.GetHashTitleForType(hashType: fhHashType): string;
begin
  Result := fHashObjs[hashType].Title;
end;


procedure THashAlgUnified.SetActiveHash(hash: fhHashType);
begin
  fActiveHash := hash;

  fTitle       := fHashObjs[ActiveHash].Title;
  fHashLength  := fHashObjs[ActiveHash].HashLength;
  fBlockLength := fHashObjs[ActiveHash].BlockLength;
end;


// The following methods are just passed through to the appropriate hash object

// -- begin pass-throughs to hash object --

function  THashAlgUnified.HashString(const theString: string; digest: THashValue): boolean;
begin
  Result := fHashObjs[ActiveHash].HashString(theString, digest);
end;

function  THashAlgUnified.HashFile(const filename: string; digest: THashValue): boolean;
begin
  Result := fHashObjs[ActiveHash].HashFile(filename, digest);
end;

function  THashAlgUnified.HashStream(const stream: TStream; digest: THashValue): boolean;
begin
  Result := fHashObjs[ActiveHash].HashStream(stream, digest);
end;

function  THashAlgUnified.PrettyPrintHashValue(const theHashValue: THashValue): string;
begin
  Result := fHashObjs[ActiveHash].PrettyPrintHashValue(theHashValue);
end;

procedure THashAlgUnified.Init();
begin
  fHashObjs[ActiveHash].Init();
end;

procedure THashAlgUnified.Update(const input: string);
begin
  fHashObjs[ActiveHash].Update(input);
end;

procedure THashAlgUnified.Update(const input: array of byte; const inputLen: cardinal);
begin
  fHashObjs[ActiveHash].Update(input, inputLen);
end;

procedure THashAlgUnified.Update(input: TStream; const inputLen: int64);
begin
  fHashObjs[ActiveHash].Update(input, inputLen);
end;

procedure THashAlgUnified.Update(input: TStream);
begin
  fHashObjs[ActiveHash].Update(input);
end;

procedure THashAlgUnified.Final(digest: THashValue);
begin
  fHashObjs[ActiveHash].Final(digest);
end;

function  THashAlgUnified.HashString(theString: string): THashArray;
begin
  Result := fHashObjs[ActiveHash].HashString(theString);
end;

function  THashAlgUnified.HashFile(filename: string; var digest: THashArray): boolean;
begin
  Result := fHashObjs[ActiveHash].HashFile(filename, digest);
end;

function  THashAlgUnified.HashToDisplay(theHash: THashArray): string;
begin
  Result := fHashObjs[ActiveHash].HashToDisplay(theHash);
end;

function  THashAlgUnified.HashToDataString(theHash: THashArray): string;
begin
  Result := fHashObjs[ActiveHash].HashToDataString(theHash);
end;

procedure THashAlgUnified.ClearHash(var theHash: THashArray);
begin
  fHashObjs[ActiveHash].ClearHash(theHash);
end;

function  THashAlgUnified.DigestSize(): integer;
begin
  Result := fHashObjs[ActiveHash].DigestSize();
end;

// -- end pass-throughs to hash object --

END.


