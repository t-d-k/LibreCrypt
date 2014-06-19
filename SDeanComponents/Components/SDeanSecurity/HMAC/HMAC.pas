unit HMAC;
// Description: HMAC implementation
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This implementation conforms to the RFC2104 specification for HMAC, and may
// be verified by the use of the test cases specificed in RFC2202


interface

uses
  Classes,
  HashAlg_U,
  HashValue_U;

type
  TMACValue = class(THashValue);

  // Generate HMAC
  function HMACString(const key: string; const data: string; const hashAlg: THashAlg; mac: TMACValue): boolean;
  function HMACFile(const key: string; const filename: string; const hashAlg: THashAlg; mac: TMACValue): boolean;
  // Note: This will operate from the *current* *position* in the stream, right
  //       up to the end
  function HMACStream(const key: string; stream: TStream; const hashAlg: THashAlg; mac: TMACValue): boolean;


implementation

uses
  sysutils;


// (Internal function only)
// Pre-process the key ("K") to produce the key with appropriate length for use
// in HMAC processing 
function _Determine_MAC_K(K: string; hashAlg: THashAlg): string;
var
  retVal: string;
  hashValue: THashValue;
begin
  if (Length(K) > (hashAlg.BlockLength div 8)) then
    begin
    hashValue:= THashValue.Create();
    try
      if hashAlg.HashString(K, hashValue) then
        begin
        retVal := hashValue.ValueAsBinary;
        end;
    finally
      hashValue.Free();
    end;
    end
  else
    begin
    retVal := K;
    end;

  // rfc2104 states that we should just use the hashlength bytes taken straight
  // from the hash - but the reference implementation right pads it if it's
  // less than the blocklength; so we do as well.
  if (Length(retVal) < (hashAlg.BlockLength div 8)) then
    begin
    retVal := retVal + StringOfChar(
                                    char(0),
                                    ((hashAlg.BlockLength div 8) - Length(retVal))
                                   );
    end;

  Result := retVal;
end;


// Generate HMAC for string data
function HMACString(const key: string; const data: string; const hashAlg: THashAlg; mac: TMACValue): boolean;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(data);
  try
    Result := HMACStream(key, stream, hashAlg, mac);
  finally
    stream.Free();
  end;

end;


// Generate HMAC for file
function HMACFile(const key: string; const filename: string; const hashAlg: THashAlg; mac: TMACValue): boolean;
var
  stream: TFileStream;
begin
  Result := FALSE;

  try
    stream := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      Result := HMACStream(key, stream, hashAlg, mac);
    finally
      stream.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


// Generate HMAC for stream
// !! IMPORTANT !!
// Note: This will operate from the *current* *position* in the stream, right
//       up to the end
function HMACStream(const key: string; stream: TStream; const hashAlg: THashAlg; mac: TMACValue): boolean;
var
  i: integer;
  processedK: string;
  Ki: string;
  Ko: string;
  retVal: boolean;
  hv: THashValue;
begin
  processedK := _Determine_MAC_K(key, hashAlg);

  Ki := '';
  for i:=1 to length(processedK) do
    begin
    Ki := Ki + char(byte(processedK[i]) XOR $36);
    end;

  Ko := '';
  for i:=1 to length(processedK) do
    begin
    Ko := Ko + char(byte(processedK[i]) XOR $5C);
    end;

  hv := THashValue.Create();
  try
    // Outer...
    hashAlg.Init();
    hashAlg.Update(Ki);
    hashAlg.Update(stream);
    hashAlg.Final(hv);

    // Inner...
    retVal := hashAlg.HashString(Ko + hv.ValueAsBinary, mac);

  finally
    hv.Free();
  end;

  // Overwrite temp variables...
  processedK := StringOfChar(char(random(256)), length(processedK));
  Ki := StringOfChar(char(random(256)), length(processedK));
  Ko := StringOfChar(char(random(256)), length(processedK));

  Result := retVal;
end;


END.


