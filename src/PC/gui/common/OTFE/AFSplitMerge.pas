unit AFSplitMerge;

{
does AF split and merge for luks
extracted from OTFEFreeOTFEBase_U
there is also AFSplitter.pas in sdeanutils
prob AFSplitter.pas better than this -
todo: merge and test
}

interface

uses
  //delphi
  //3rd party
  //SDU ,lclibs
  lcTypes,
  lcDebugLog
  //librecrypt
  ;

procedure AFSplit(const input: TSDUBytes; n: Integer;
  hashKernelModeDeviceName: String; hashGUID: TGUID; out output: TSDUBytes);
procedure AFMerge(const input: Ansistring; n: Integer; hashKernelModeDeviceName: String;
  hashGUID: TGUID; out output: TSDUBytes);

implementation

uses
  //delphi
  SysUtils,
  //3rd party
  //SDU/ lc lib
  SDUEndianIntegers,
sduGeneral,
  //librecrypt
  OTFEFreeOTFEBase_U;

function _GetRandomUnsafe(len: Integer): Ansistring;
var
  i: Integer;
begin
  for i := 1 to len do
    Result := Result + AnsiChar(random(256));
    (* used in AFSplit - now used for creating luks volumes so use sprng or random instead
        but only affects recoverability of enxd data from disc - not safety of cypher
      *)
  { TODO 3 -otdk -csecurity : use sprng or random instead}
end;


 // ----------------------------------------------------------------------------
 // Get the "blockNumber" block out of "totalBlockCount"
function _GetBlock(const stripeData: Ansistring;
  blockLength: Integer; blockNumber: Integer; var output: Ansistring): Boolean;
var
  i: Integer;
begin
  Result := True;

  // xxx - sanity checking

  // Get block "i"
  output := '';
  for i := 0 to (blockLength - 1) do begin
    // -1 because block numbers start from 1
    // +1 because strings start from 1
    output := output + stripeData[(((blockNumber - 1) * blockLength) + i + 1)];
  end;

end;


 // ----------------------------------------------------------------------------
 // Hash input repeatedly until a string with the same length is produced
function _RepeatedHash(hashKernelModeDeviceName: Ansistring;
  hashGUID: TGUID; input: Ansistring; out output: TSDUBytes): Boolean;
var
  hashOutput:          TSDUBytes;
  digestSizeBytes:     Integer;
  inputDivDigestBytes: Integer;
  inputModDigestBytes: Integer;
  i, j:                Integer;
  tmpString:           TSDUBytes;
  hashDetails:         TFreeOTFEHash;
begin
  SDUInitAndZeroBuffer(0, output);
  //  output := '';

  digestSizeBytes     := 0;
  inputDivDigestBytes := 0;
  inputModDigestBytes := 0;
  { TODO 1 -otdk -crefactor : pass in hashDetails to reduce coupling  - see also afsplitter.pas}
  Result              := GetFreeOTFEBase().GetSpecificHashDetails(hashKernelModeDeviceName,
    hashGUID, hashDetails);

  if Result then begin
    // Hash output
    digestSizeBytes := (hashDetails.Length div 8);

    inputDivDigestBytes := (length(input) div digestSizeBytes);
    inputModDigestBytes := (length(input) mod digestSizeBytes);

    for i := 0 to (inputDivDigestBytes - 1) do begin
      tmpString := SDUStringToSDUBytes(SDUBigEndian32ToString(SDUDWORDToBigEndian32(i)));
      for j := 0 to (digestSizeBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(tmpString, Ord(input[((i * digestSizeBytes) + j + 1)]));
        //        tmpString := tmpString + input[((i * digestSizeBytes) + j + 1)];
      end;

      Result := GetFreeOTFEBase().HashData(hashKernelModeDeviceName, hashGUID,
        tmpString, hashOutput);

      if not (Result) then begin
        break;
      end;
      SDUAddArrays(output, hashOutput);

      //      output := output + hashOutput;
    end;  // for i:=0 to (inputDivDigestBytes-1) do

  end;

  if Result then begin
    if (inputModDigestBytes > 0) then begin
      tmpString := SDUStringToSDUBytes(
        SDUBigEndian32ToString(SDUDWORDToBigEndian32(inputDivDigestBytes)));
      for j := 0 to (inputModDigestBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(tmpString, Ord(input[((inputDivDigestBytes * digestSizeBytes) + j + 1)]));
        //        tmpString := tmpString + input[((inputDivDigestBytes * digestSizeBytes) + j + 1)];
      end;

      Result := GetFreeOTFEBase().HashData(hashKernelModeDeviceName, hashGUID,
        tmpString, hashOutput);

      for j := 0 to (inputModDigestBytes - 1) do begin
        // +1 because strings start from 1
        SDUAddByte(output, hashOutput[j]);
        //        output := output + hashOutput[j + 1];
      end;
    end;

  end;

end;

 // ----------------------------------------------------------------------------
 // Process blocks 1 - (n-1)
procedure _Process(stripeData: Ansistring; totalBlockCount: Integer;
  blockLength: Integer; hashKernelModeDeviceName: String; hashGUID: TGUID;
  out output: TSDUBytes);
var
  prev:        TSDUBytes;
  i, j:        Integer;
  tmpData:     Ansistring;
  hashedBlock: TSDUBytes;
  blockN:      Ansistring;
  blkNum:      Integer;
begin

  SDUInitAndZeroBuffer(blockLength, prev);


  DebugMsg('Process');
  DebugMsg(blockLength);
  DebugMsg(totalBlockCount);

  //  prev := StringOfChar(#0, blockLength);
  // -1 because we don't process the last block
  blkNum := 0;

  for i := 1 to (totalBlockCount - 1) do begin

    Inc(blkNum);

    // Get block "i"
    _GetBlock(stripeData, blockLength, i, blockN);


DebugMsg('Before XOR ('+inttostr(blkNum)+'):');
DebugMsgBinary( prev);


    // XOR previous block output with current stripe
    tmpData := '';
    for j := 1 to length(prev) do begin
      tmpData := tmpData + Ansichar((prev[j - 1] xor Ord(blockN[j])));
    end;

    DebugMsg('In between XOR and diffuse (' + IntToStr(blkNum) + '):');
    DebugMsgBinary(tmpData);

    // Hash output
    _RepeatedHash(
      hashKernelModeDeviceName,
      hashGUID,
      tmpData,
      hashedBlock
      );

    DebugMsg('After process (' + IntToStr(blkNum) + '):');
    DebugMsgBinary(SDUBytesToString(hashedBlock));

    // Setup for next iteration
    prev := hashedBlock;
  end;

  // Store last hashing result as output
  output := prev;

end;

// ----------------------------------------------------------------------------
procedure AFSplit(const input: TSDUBytes; n: Integer;
  hashKernelModeDeviceName: String; hashGUID: TGUID; out output: TSDUBytes);
var
  i, j:             Integer;
  randomStripeData: Ansistring;
  calcStripe:       TSDUBytes;
  processedStripes: TSDUBytes;
  blockLength:      Integer;
begin

  DebugMsg('AFSplit');
  DebugMsg(format('length(input) %d', [length(input)]));
  DebugMsg(format('n %d', [n]));


  blockLength := length(input);

  randomStripeData := '';
  for i := 1 to (n - 1) do
    // Get random block "i"
    randomStripeData := randomStripeData + _GetRandomUnsafe(blockLength);

  _Process(
    randomStripeData,
    n,
    blockLength,
    hashKernelModeDeviceName,
    hashGUID,
    processedStripes
    );

  // XOR last hash output with input
  SDUZeroBuffer(calcStripe);
  for j := 0 to blockLength - 1 do
    SDUAddByte(calcStripe, Ord(processedStripes[j]) xor input[j]);
  //    calcStripe := calcStripe + Ansichar();

  // Store result as random block "n" as output
  output := SDUStringToSDUBytes(randomStripeData);
  SDUAddArrays(output, calcStripe);
  //  output := randomStripeData + calcStripe;
  DebugMsg(format('length(output) %d', [length(output)]));
end;



{ TODO 1 -otdk -crefactor : store data in arrays not strings }
procedure AFMerge(const input: Ansistring; n: Integer;
  hashKernelModeDeviceName: String; hashGUID: TGUID; out output: TSDUBytes);
var
  i:                Integer;
  processedStripes: TSDUBytes;
  blockLength:      Integer;
  lastBlock:        Ansistring;
begin

  DebugMsg('AFMerge');
  DebugMsg(format('length(input) %d', [length(input)]));
  DebugMsg(format('n %d', [n]));

  blockLength := (length(input) div n);

  _Process(
    input,
    n,
    blockLength,
    hashKernelModeDeviceName,
    hashGUID,
    processedStripes
    );

  // Get block "n"
  _GetBlock(input, blockLength, n, lastBlock);

  // XOR last hash output with last block to obtain original data
  SDUInitAndZeroBuffer(0, output);
  for i := 1 to blockLength do
    SDUAddByte(output, processedStripes[i - 1] xor Ord(lastBlock[i]));
  DebugMsg(format('length(output) %d', [length(output)]));
end;


end.
