
// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.GetRandom(len: integer): string;
var
  retVal: string;
  i: integer;
begin
  for i:=1 to len do
    begin
// xxx -     retVal := retVal + char(random(256));
{ TODO 3 -otdk -csecurity : wtf? is this used anywhere }
    retVal := retVal + char(0);
    end;

  Result:= retVal;
end;


// ----------------------------------------------------------------------------
// Hash input repeatedly until a string with the same length is produced
function TOTFEFreeOTFEBase.RepeatedHash(
                      hashKernelModeDeviceName: Ansistring;
                      hashGUID: TGUID;
                      input: Ansistring;
                      var output: Ansistring
                     ): boolean;
var
  allOK: boolean;
  hashOutput: Ansistring;
  digestSizeBytes: integer;
  inputDivDigestBytes: integer;
  inputModDigestBytes: integer;
  i, j: integer;
  tmpString: Ansistring;
  hashDetails: TFreeOTFEHash;
begin
  output := '';
  
  digestSizeBytes := 0;
  inputDivDigestBytes := 0;
  inputModDigestBytes := 0;

  allOK := GetSpecificHashDetails(
                         hashKernelModeDeviceName,
                         hashGUID,
                         hashDetails
                        );

  if (allOK) then
    begin
    // Hash output
    digestSizeBytes := (hashDetails.Length div 8);

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

      allOK := HashData(
                      hashKernelModeDeviceName,
                      hashGUID,
                      tmpString,
                      hashOutput
                     );

      if not(allOK) then
        begin
        break;
        end;

      output := output + hashOutput;
      end;  // for i:=0 to (inputDivDigestBytes-1) do

    end;

  if (allOK) then
    begin
    if (inputModDigestBytes > 0) then
      begin
      tmpString := SDUBigEndian32ToString(SDUDWORDToBigEndian32(inputDivDigestBytes));
      for j:=0 to (inputModDigestBytes-1) do
        begin
        // +1 because strings start from 1
        tmpString := tmpString + input[((inputDivDigestBytes*digestSizeBytes) + j + 1)];
        end;

      allOK := HashData(
                      hashKernelModeDeviceName,
                      hashGUID,
                      tmpString,
                      hashOutput
                     );

      for j:=0 to (inputModDigestBytes-1) do
        begin
        // +1 because strings start from 1
        output := output + hashOutput[j + 1];
        end;
      end;

    end;


  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.AFSplit(
                 input: AnsiString;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: AnsiString
                ): boolean;
var
  allOK: boolean;
  i, j: integer;
  randomStripeData: AnsiString ;
  calcStripe: AnsiString;
  processedStripes: AnsiString;
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

  Process(
          randomStripeData,
          n,
          blockLength,
          hashKernelModeDeviceName,
          hashGUID,
          processedStripes
         );

  // XOR last hash output with input
  calcStripe := '';
  for j:=1 to blockLength do
    begin
    calcStripe := calcStripe + Ansichar((ord(processedStripes[j]) XOR ord(input[j])));
    end;

  // Store result as random block "n" as output
  output := randomStripeData + calcStripe;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Process blocks 1 - (n-1)
function TOTFEFreeOTFEBase.Process(
                 stripeData: AnsiString;
                 totalBlockCount: integer;
                 blockLength: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: AnsiString
                ): boolean;
var
  allOK: boolean;
  prev: Ansistring;
  i, j: integer;
  tmpData: Ansistring;
  hashedBlock: Ansistring;
  blockN: Ansistring;
{$IFDEF FREEOTFE_DEBUG}
  blkNum: integer;
{$ENDIF}
begin
  allOK := TRUE;

  prev := StringOfChar(#0, blockLength);
  // -1 because we don't process the last block
{$IFDEF FREEOTFE_DEBUG}
  blkNum := 0;
{$ENDIF}
  for i:=1 to (totalBlockCount-1) do
    begin
{$IFDEF FREEOTFE_DEBUG}
inc(blkNum);
{$ENDIF}
    // Get block "i"
    GetBlock(stripeData, totalBlockCount, blockLength, i, blockN);

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('Before XOR ('+inttostr(blkNum)+'):');
DebugMsgBinary(prev);
{$ENDIF}

    // XOR previous block output with current stripe
    tmpData := '';
    for j:=1 to length(prev) do
      begin
      tmpData := tmpData + Ansichar((ord(prev[j]) XOR ord(blockN[j])));
      end;

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('In between XOR and diffuse ('+inttostr(blkNum)+'):');
DebugMsgBinary(tmpData);
{$ENDIF}

    // Hash output
    RepeatedHash(
                 hashKernelModeDeviceName,
                 hashGUID,
                 tmpData,
                 hashedBlock
                );

{$IFDEF FREEOTFE_DEBUG}
DebugMsg('After process ('+inttostr(blkNum)+'):');
DebugMsgBinary(hashedBlock);
{$ENDIF}

    // Setup for next iteration
    prev := hashedBlock;
    end;

  // Store last hashing result as output
  output := prev;


  Result := allOK;
end;


// ----------------------------------------------------------------------------
// Get the "blockNumber" block out of "totalBlockCount"
function TOTFEFreeOTFEBase.GetBlock(stripeData: Ansistring; totalBlockCount: integer; blockLength: integer; blockNumber: integer; var output: Ansistring): boolean;
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


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.AFMerge(
                 input: AnsiString;
                 n: integer;
                 hashKernelModeDeviceName: string;
                 hashGUID: TGUID;
                 var output: AnsiString
                ): boolean;
var
  allOK: boolean;
  i: integer;
  processedStripes: AnsiString;
  blockLength: integer;
  lastBlock: AnsiString;
begin
  allOK := TRUE;

  blockLength := (length(input) div n);

  Process(
          input,
          n,
          blockLength,
          hashKernelModeDeviceName,
          hashGUID,
          processedStripes
         );

  // Get block "i"
  GetBlock(input, n, blockLength, n, lastBlock);

  // XOR last hash output with last block to obtain original data
  output := '';
  for i:=1 to blockLength do
    begin
    output := output + Ansichar((ord(processedStripes[i]) XOR ord(lastBlock[i])));
    end;

  Result := allOK;
end;

// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------



