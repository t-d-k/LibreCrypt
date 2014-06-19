program SDHash;

uses
  SysUtils, // needed for Format
  Windows, // needed for SYSTEMTIME
  SDUGeneral,
  SDUFileIterator_U,
  HashValue_U,
  HashAlg_U,
  HashAlgGOST_U,
  HashAlgMD2_U,
  HashAlgMD4_U,
  HashAlgMD5_U,
  HashAlgSHA_U,
  HashAlgSHA1_U,
  HashAlgSHA256_U,
  HashAlgSHA384_U,
  HashAlgSHA512_U,
  HashAlgRIPEMD_U,
  HashAlgTiger_U;

type
  // IMPORTANT: KEEP THIS, fhHashNames, AND THE BODY OF procedure
  //            CreateHashObjects(...) IN SYNC!
  fhHashAlg  = (
                hashGOST,
                hashMD2,
                hashMD4,
                hashMD5,
                hashSHA,
                hashSHA1,
                hashSHA256,
                hashSHA384,
                hashSHA512,
                hashRIPEMD_128,
                hashRIPEMD_160,
                hashRIPEMD_256,
                hashRIPEMD_320,
                hashTiger
                );
                 
const
  // Hash names are also the "/name" parameters on the commandline; any number
  // of names may be specified, the first is the "official" name, the rest are
  // convenient strings to type in (e.g. less any "-")
  fhHashNames: array [fhHashAlg] of string = (
                                               'GOST',
                                               'MD2',
                                               'MD4',
                                               'MD5',
                                               'SHA',
                                               'SHA-1,SHA1',
                                               'SHA-256,SHA256',
                                               'SHA-384,SHA384',
                                               'SHA-512,SHA512',
                                               'RIPEMD-128,RIPEMD128,RMD-128,RMD128',
                                               'RIPEMD-160,RIPEMD160,RMD-160,RMD160',
                                               'RIPEMD-256,RIPEMD256,RMD-256,RMD256',
                                               'RIPEMD-320,RIPEMD320,RMD-320,RMD320',
                                               'Tiger'
                                               );


var
  hashObjects: array [fhHashAlg] of THashAlg;
  hashLoop: fhHashAlg;
  filename: string;
  hashValue: THashValue;
  doUnixStyle: boolean;
  doAllHashes: boolean;
  doTestVector: boolean;
  doRecursive: boolean;
  doLowercase: boolean;
  shaSpecified: boolean;
  doSuppressSHAWarn: boolean;
  numHashesToDo: integer;
  fileIterator: TSDUFileIterator;
  fileSpec: string;
  cntFiles: integer;
  exeFilename: string;
  exeFilenameExt: string;
  progStartTime: TTimeStamp;
  hashStartTime: TTimeStamp;
  doTimed: boolean;

procedure CreateHashObjects();
begin
  hashObjects[hashGOST] := THashAlgGOST.Create(nil);
  hashObjects[hashMD2] := THashAlgMD2.Create(nil);
  hashObjects[hashMD4] := THashAlgMD4.Create(nil);
  hashObjects[hashMD5] := THashAlgMD5.Create(nil);
  hashObjects[hashSHA]    := THashAlgSHA.Create(nil);
  hashObjects[hashSHA1]   := THashAlgSHA1.Create(nil);
  hashObjects[hashSHA256] := THashAlgSHA256.Create(nil);
  hashObjects[hashSHA384] := THashAlgSHA384.Create(nil);
  hashObjects[hashSHA512] := THashAlgSHA512.Create(nil);
  hashObjects[hashRIPEMD_128] := THashAlgRIPEMD128.Create(nil);
  hashObjects[hashRIPEMD_160] := THashAlgRIPEMD160.Create(nil);
  hashObjects[hashRIPEMD_256] := THashAlgRIPEMD256.Create(nil);
  hashObjects[hashRIPEMD_320] := THashAlgRIPEMD320.Create(nil);
  hashObjects[hashTiger] := THashAlgTiger.Create(nil);

end;


// Return the current timestamp (high-res)
function GetCurrentTimeStamp(): TTimeStamp;
var
  currTime: SYSTEMTIME;
begin
  GetSystemTime(currTime);

  Result.Date := 0;
  Result.Time := (currTime.wHour*60*60*1000)+
                 (currTime.wMinute*60*1000)+
                 (currTime.wSecond*1000)+
                 (currTime.wMilliseconds);

end;


// Return the difference between two times, prettily formatted.
function FormattedTimeDiff(beginTime: TTimeStamp; endTime: TTimeStamp): string;
var
  duration: cardinal;
begin
  duration := endTime.time - beginTime.time;
  Result := Format('%.2d:%.2d.%.3d', [((duration div 1000) div 60),
                                 ((duration div 1000) mod 60),
                                 (duration mod 1000)]);
end;


function IsHashInCommandLineParams(hash: fhHashAlg): boolean;
var
  retVal: boolean;
  remainingNames: string;
  testName: string;
  alternateNames: string;
begin
  retVal := FALSE;

  alternateNames := fhHashNames[hashLoop];
  while SDUSplitString(alternateNames, testName, remainingNames, ',') do
    begin
    retVal := retval OR SDUCommandLineSwitch(testName);
    alternateNames := remainingNames;
    end;

  retVal := retval OR SDUCommandLineSwitch(alternateNames);

  Result := retVal;

end;


var
  name: string;
  remainingNames: string;
  tmpStr: string;
  junkStr: string;
  outputHash: string;
begin
  if (ParamCount<1) OR
     SDUCommandLineSwitch('?') OR
     SDUCommandLineSwitch('help') then
    begin
    exeFilename := ExtractFilename(ParamStr(0));
    exeFilenameExt := ExtractFileExt(exeFilename);
    delete(exeFilename, length(exeFilename)-length(exeFilenameExt)+1, length(exeFilenameExt));

    writeln('SDHash: Sarah Dean''s Hash Generator (v2.6)');
    writeln('');
    writeln('Usage:');
    writeln('  '+exeFilename+' [/? | /help]');
    writeln('  '+exeFilename+' <hashalg>... [/nowarnsha] [/timed] [/lowercase] [/recursive] <filename>');
    writeln('  '+exeFilename+' <hashalg>... [/nowarnsha] [/timed] [/lowercase] /test <test vector>');
    writeln('');
    writeln('  Where <hashalg> is zero or more of:');
    for hashLoop:=low(hashObjects) to high(hashObjects) do
      begin
      if SDUSplitString(fhHashNames[hashLoop], name, remainingNames, ',') then
        begin
        write('    /'+lowercase(name));
        while SDUSplitString(remainingNames, name, tmpStr, ',') do
          begin
          write(' (or /'+lowercase(name)+')');
          remainingNames := tmpStr;
          end;
        write(' (or /'+lowercase(remainingNames)+')');
        writeln('');
        end
      else
        begin
        writeln('    /'+lowercase(fhHashNames[hashLoop]));
        end;
      end;
    writeln('');
    writeln('  If <hashalg> is omitted, the hash value for *all* of the supported hash');
    writeln('  algorithms will be generated');
    writeln('');
    writeln('  /nowarnsha - if "/sha" was specified, a warning will be displayed unless this');
    writeln('               flag is specified');
    writeln('');
    writeln('  /timed - display time taken to compute each hash, and total execution');
    writeln('           time. (Shown in [mins:secs.ms] format)');
    writeln('');
    writeln('  /lowercase - display hashes in lowercase');
    writeln('');
    writeln('  /test <test vector> - generate hash of the test vector supplied');
    writeln('');
    writeln('  /recursive - recursively scan through all subdirectories');
    writeln('');
    writeln('  /unix - output in sha1sum/md5sum/related style');
    writeln('');
    writeln('  <filename> is either a filename or filemask.');
    writeln('');
    writeln('  e.g.');
    writeln('    '+exeFilename+' /MD5 /SHA-1 aFile.txt');
    writeln('    '+exeFilename+' /MD5 /SHA-1 /recursive *.txt');
    writeln('    '+exeFilename+' /SHA256 /test abc');
    writeln('');
    exit;
    end
  else
    begin
    progStartTime := GetCurrentTimeStamp();
    doTestVector := SDUCommandLineSwitch('test');
    doRecursive := SDUCommandLineSwitch('recursive');
    doTimed := SDUCommandLineSwitch('timed');
    doLowercase := SDUCommandLineSwitch('lowercase');
    shaSpecified := SDUCommandLineSwitch('sha');
    doSuppressSHAWarn := SDUCommandLineSwitch('nowarnsha');
    doUnixStyle := SDUCommandLineSwitch('unix');

    if doUnixStyle then
      begin
      doLowercase := TRUE;
      end;

    fileSpec := ParamStr(ParamCount);

    
    // If "/sha" was specified as a command line parameter, warn the user in
    // case they *really* meant /sha-1 (/sha1)
    if (not(doSuppressSHAWarn)) and shaSpecified then
      begin
      writeln('WARNING: SHA hash algorithm specified - did you mean SHA-1?');
      writeln('');
      writeln('(Use /nowarnsha to suppress this warning in future.)');
      writeln('');
      writeln('');
      end;


    if doTestVector then
      begin
      fileIterator := nil;
      cntFiles := 0;
      end
    else
      begin
      fileIterator := TSDUFileIterator.Create(nil);
      fileIterator.Sorted := TRUE;
      fileIterator.RecurseSubDirs := doRecursive;

      fileIterator.FileMask := fileSpec;
      fileIterator.Reset();

      cntFiles := fileIterator.Count;

      if (cntFiles=0) then
        begin
        writeln('File not found.');
        exit;
        end;
      end;

    CreateHashObjects();

    numHashesToDo:= 0;
    for hashLoop:=low(hashObjects) to high(hashObjects) do
      begin
      if IsHashInCommandLineParams(hashLoop) then
        begin
        inc(numHashesToDo);
        end;
      end;

    doAllHashes := TRUE;
    if (numHashesToDo>0) then
      begin
      doAllHashes := FALSE;
      end
    else
      begin
      for hashLoop:=low(hashObjects) to high(hashObjects) do
        begin
        inc(numHashesToDo);
        end;
      end;


    hashValue := THashValue.Create();
    try
      if doTestVector then
        begin
        for hashLoop:=low(hashObjects) to high(hashObjects) do
          begin
          if IsHashInCommandLineParams(hashLoop) OR
             doAllHashes                         then
            begin
            hashStartTime := GetCurrentTimeStamp();
            hashObjects[hashLoop].HashString(fileSpec, hashValue);

            if (numHashesToDo > 1) then
              begin
              SDUSplitString(fhHashNames[hashLoop], name, junkStr, ',');
              write(Format('%-10s', [name])+': ');
              end;

            if doTimed then
              begin
              write(' ['+FormattedTimeDiff(hashStartTime, GetCurrentTimeStamp())+'] ');
              end;

            outputHash := hashObjects[hashLoop].PrettyPrintHashValue(hashValue);
            if doLowercase then
              begin
              outputHash := lowercase(outputHash);
              end
            else
              begin
              outputHash := uppercase(outputHash);
              end;

            if doUnixStyle then
              begin
              outputHash := StringReplace(outputHash, ' ', '', [rfReplaceAll]);
              end;

            writeln(outputHash);
            end;
          end;
        end
      else
        begin
        filename := fileIterator.Next();
        while (filename<>'') do
          begin
          for hashLoop:=low(hashObjects) to high(hashObjects) do
            begin
            if IsHashInCommandLineParams(hashLoop) OR
               doAllHashes                         then
              begin
              hashStartTime := GetCurrentTimeStamp();
              hashObjects[hashLoop].HashFile(filename, hashValue);

              if not(doUnixStyle) then
                begin
                if cntFiles>1 then
                  begin
                  write(filename+': ');
                  end;
                end;

              if numHashesToDo>1 then
                begin
                SDUSplitString(fhHashNames[hashLoop], name, junkStr, ',');
                write(Format('%-10s', [name])+': ');
                end;

              if doTimed then
                begin
                write(' ['+FormattedTimeDiff(hashStartTime, GetCurrentTimeStamp())+'] ');
                end;

              outputHash := hashObjects[hashLoop].PrettyPrintHashValue(hashValue);
              if doLowercase then
                begin
                outputHash := lowercase(outputHash);
                end
              else
                begin
                outputHash := uppercase(outputHash);
                end;

              if doUnixStyle then
                begin
                outputHash := StringReplace(outputHash, ' ', '', [rfReplaceAll]);
                end;

              write(outputHash);

              if doUnixStyle then
                begin
                // Ditch any "./" at the start of the filename
                if (
                    (Pos('.\', filename) = 0) or
                    (Pos('./', filename) = 0)
                   ) then
                  begin
                  filename := Copy(filename, 3, (length(filename)-2));
                  end;

                write('  '+filename);
                end;

              writeln('');
              end;
            end;
          filename := fileIterator.Next();
          end;
        end;

    finally
      hashValue.Free();
    end;

    for hashLoop:=low(hashObjects) to high(hashObjects) do
      begin
      hashObjects[hashLoop].Free();
      end;

    if doTimed then
      begin
      writeln('Program execution: '+FormattedTimeDiff(progStartTime, GetCurrentTimeStamp));
      end;

    end;

END.

