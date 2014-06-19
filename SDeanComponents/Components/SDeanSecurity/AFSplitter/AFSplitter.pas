unit AFSplitter;

interface

uses
  HashAlg_U;


// "Standard" version; uses SHA-1
// n - Set to the number of stripes
function AFSplit(input: string; n: integer; var output: string): boolean; overload;
function AFMerge(input: string; n: integer; var output: string): boolean; overload;

// User specified hash algorithm
// n - Set to the number of stripes
function AFSplit(input: string; n: integer; hashAlg: THashAlg; var output: string): boolean; overload;
function AFMerge(input: string; n: integer; hashAlg: THashAlg; var output: string): boolean; overload;


implementation

uses
  HashValue_U,
  HashAlgSHA1_U,
  SDUEndianIntegers;

function GetRandom(len: integer): string; forward;
function RepeatedHash(input: string; hashAlg: THashAlg; var output: string): boolean; forward;
function Process(stripeData: string; totalBlockCount: integer; blockLength: integer; hashAlg: THashAlg; var output: string): boolean; forward;
function GetBlock(stripeData: string; totalBlockCount: integer; blockLength: integer; blockNumber: integer; var output: string): boolean; forward;



function GetRandom(len: integer): string;
var
  retVal: string;
  i: integer;
begin
  for i:=1 to len do
    begin
    retVal := retVal + char(random(256));
// xxx - debug use only -     retVal := retVal + char(0);
    end;

  Result:= retVal;
end;

// Hash input repeatedly until a string with the same length is produced
function RepeatedHash(input: string; hashAlg: THashAlg; var output: string): boolean;
var
  allOK: boolean;
  digestSizeBytes: integer;
  inputDivDigestBytes: integer;
  inputModDigestBytes: integer;
  i, j: integer;
  tmpString: string;
  hashOutput: THashValue;
begin
  allOK := TRUE;

  output := '';

  hashOutput := THashValue.Create();
  try
    // Hash output
    digestSizeBytes := (hashAlg.HashLength div 8);

    inputDivDigestBytes := (length(input) div digestSizeBytes);
    inputModDigestBytes := (length(input) mod digestSizeBytes);

    for i:=0 to (inputDivDigestBytes-1) do
      begin
      tmpString := SDUBigEndian32ToString(SDUDWORDToBigEndian32(i));
      for j:=0 to (digestSizeBytes-1) do
        begin
        // +1 because strings start from 1
        tmpString := tmpString + input[((i*digestSizeBytes) + j + 1)];
        end;

      hashAlg.HashString(tmpString, hashOutput);

      output := output + hashOutput.ValueAsBinary;
      end;

    if (inputModDigestBytes > 0) then
      begin
      tmpString := SDUBigEndian32ToString(SDUDWORDToBigEndian32(inputDivDigestBytes));
      for j:=0 to (inputModDigestBytes-1) do
        begin
        // +1 because strings start from 1
        tmpString := tmpString + input[((inputDivDigestBytes*digestSizeBytes) + j + 1)];
        end;

      hashAlg.HashString(tmpString, hashOutput);

      tmpString := hashOutput.ValueAsBinary;
      for j:=0 to (inputModDigestBytes-1) do
        begin
        // +1 because strings start from 1
        output := output + tmpString[j + 1];
        end;
      end;

  finally
    hashOutput.Free();
  end;

  Result := allOK;
end;



function AFSplit(input: string; n: integer; var output: string): boolean;
var
  hashAlg: THashAlg;
begin
  // Hash output
  hashAlg := THashAlgSHA1.Create(nil);
  try
    Result := AFSplit(input, n, hashAlg, output);

  finally
    hashAlg.Free();
  end;

end;


function AFSplit(input: string; n: integer; hashAlg: THashAlg; var output: string): boolean;
var
  allOK: boolean;
  i, j: integer;
  randomStripeData: string;
  calcStripe: string;
  processedStripes: string;
  blockLength: integer;
begin
  allOK := TRUE;

  blockLength := length(input);

  randomStripeData := '';
  for i:=1 to (n-1) do
    begin
    // Get random block "i"
    randomStripeData := randomStripeData + GetRandom(blockLength);
    end;

  Process(randomStripeData, n, blockLength, hashAlg, processedStripes);

  // XOR last hash output with input
  calcStripe := '';
  for j:=1 to blockLength do
    begin
    calcStripe := calcStripe + char((ord(processedStripes[j]) XOR ord(input[j])));
    end;

  // Store result as random block "n" as output
  output := randomStripeData + calcStripe;

  Result := allOK;
end;


// Process blocks 1 - (n-1)
function Process(stripeData: string; totalBlockCount: integer; blockLength: integer; hashAlg: THashAlg; var output: string): boolean;
var
  allOK: boolean;
  prev: string;
  i, j: integer;
  tmpData: string;
  hashedBlock: string;
  blockN: string;
begin
  allOK := TRUE;

  prev := StringOfChar(#0, blockLength);
  // -1 because we don't process the last block
  for i:=1 to (totalBlockCount-1) do
    begin
    // Get block "i"
    GetBlock(stripeData, totalBlockCount, blockLength, i, blockN);

    // XOR previous block output with current stripe
    tmpData := '';
    for j:=1 to length(prev) do
      begin
      tmpData := tmpData + char((ord(prev[j]) XOR ord(blockN[j])));
      end;

    // Hash output
    RepeatedHash(tmpData, hashAlg, hashedBlock);

    // Setup for next iteration
    prev := hashedBlock;
    end;

  // Store last hashing result as output
  output := prev;


  Result := allOK;
end;


// Get the "blockNumber" block out of "totalBlockCount"
function GetBlock(stripeData: string; totalBlockCount: integer; blockLength: integer; blockNumber: integer; var output: string): boolean;
var
  allOK: boolean;
  i: integer;
begin
  allOK := TRUE;

  // xxx - sanity checking

  // Get block "i"
  output := '';
  for i:=0 to (blockLength-1) do
    begin
    // -1 because block numbers start from 1
    // +1 because strings start from 1
    output := output + stripeData[(((blockNumber-1) * blockLength) + i + 1)];
    end;

  Result := allOK;
end;




function AFMerge(input: string; n: integer; var output: string): boolean;
var
  hashAlg: THashAlg;
begin
  // Hash output
  hashAlg := THashAlgSHA1.Create(nil);
  try
    Result := AFMerge(input, n, hashAlg, output);

  finally
    hashAlg.Free();
  end;

end;


function AFMerge(input: string; n: integer; hashAlg: THashAlg; var output: string): boolean;
var
  allOK: boolean;
  i: integer;
  processedStripes: string;
  blockLength: integer;
  lastBlock: string;
begin
  allOK := TRUE;

  blockLength := (length(input) div n);

  Process(input, n, blockLength, hashAlg,processedStripes);

  // Get block "i"
  GetBlock(input, n, blockLength, n, lastBlock);

  // XOR last hash output with last block to obtain original data
  output := '';
  for i:=1 to blockLength do
    begin
    output := output + char((ord(processedStripes[i]) XOR ord(lastBlock[i])));
    end;

  Result := allOK;
end;


END.



