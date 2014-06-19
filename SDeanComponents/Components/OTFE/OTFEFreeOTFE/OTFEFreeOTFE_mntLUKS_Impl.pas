
{$INCLUDE OTFEFreeOTFE_mntLUKS_AFS_Impl.pas}

// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.StringHasNumbers(checkString: string): boolean;
var
  i: integer;
  retval: boolean;
begin
  retval := FALSE;

  for i:=1 to length(checkString) do
    begin
    if (
        (checkString[i] >= '0') and
        (checkString[i] <= '9')
       ) then
      begin
      retval := TRUE;
      break;
      end;
    end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
// Determine if the specified volume is a Linux LUKS volume
function TOTFEFreeOTFEBase.IsLUKSVolume(volumeFilename: string): boolean;
var
  rawData: string;
  retVal: boolean;
begin
  retVal := FALSE;

  // Read in what should be the volume's signature
  if ReadRawVolumeData(
                       volumeFilename,
                       0,  // LUKS volumes always start from the beginning
                       length(LUKS_MAGIC),
                       rawData
                      ) then
    begin
    // Compare with LUKS volume signature
    retVal := (rawData = LUKS_MAGIC);
    end;

  Result := retVal;
end;


// ----------------------------------------------------------------------------
// LUKS Header (v1)
// ================       
//
// LUKS PHDR Layout
// ----------------
// Offset  Field name      Length  Data type  Description
//     0   magic              6    bytes[]    magic for LUKS partition header
//     6   version            2    uint16_t   LUKS version
//     8   cipher-name       32    char[]     cipher name specification
//    40   cipher-mode       32    char[]     cipher mode specification
//    72   hash-spec         32    char[]     hash specification
//   104   payload-offset     4    uint32_t   start offset of the bulk data (in sectors)
//   108   key-bytes          4    uint32_t   number of key bytes
//   112   mk-digest         20    byte[]     master key checksum from PBKDF2
//   132   mk-digest-salt    32    byte[]     salt parameter for master key PBKDF2
//   164   mk-digest-iter     4    uint32_t   iteraions parameter for master key PBKDF2
//   168   uuid              40    char[]     UUID of the partition
//   208   key-slot-1        48    key_slot   key slot 1
//   256   key-slot-2        48    key_slot   key slot 2
//   ...   ...               ..    ...        ...
//   554   key-slot-8        48    key_slot   key slot 8
//  =====  ===============
//   592   total phdr size
//  =====  ===============
//
// NOTE THAT THE DOCUMENTATION HAS AN ERROR - The length of hash-spec is 32
// bytes long, though the offsets detailed in the specifications are correct. 
//
//
// LUKS Key Slot Layout
// --------------------
// Offset  Field name           Length  Data type  Description
//     0   active                  4    uint32_t   state of keyslot, enabled/disabled
//     4   iterations              4    uint32_t   iteration parameter for PBKDF2
//     8   salt                   32    byte[]     salt paramter for PBKDF2
//    40   key-material-offset     4    uint32_t   start sector of key material
//    44   stripes                 4    uint32_t   number of anti-forensic stripes
//  =====  ==================
//    48   total keyslot size
//  =====  ==================
function TOTFEFreeOTFEBase.ParseLUKSHeader(rawData: string; var LUKSheader: TLUKSHeader): boolean;
var
  rawLUKSheader: TLUKSHeaderRaw;
  allOK: boolean;
  strGUID: string;
  i: integer;
  faultFlag: boolean;
  keySlotActive: DWORD;
begin
  allOK := FALSE;

  if (length(rawData) = sizeof(rawLUKSheader)) then
    begin
    StrMove(@rawLUKSheader, PChar(rawData), Length(rawData));

    // OK, this loop is crude - but it works
    LUKSheader.magic := '';
    for i:=low(rawLUKSheader.magic) to high(rawLUKSheader.magic) do
      begin
      LUKSheader.magic := LUKSheader.magic + char(rawLUKSheader.magic[i]);
      end;

    if (LUKSheader.magic = LUKS_MAGIC) then
      begin
      LUKSheader.version        := rawLUKSheader.version[1] + (rawLUKSheader.version[0] shl 8);
      // As per the LUKS specifications: If the LUKS version is of a
      // later version than we support - we make no attempt to interpret
      // it, and simply return an error
      if (LUKSheader.version <= LUKS_VER_SUPPORTED) then
        begin
        LUKSheader.cipher_name    := Copy(rawLUKSheader.cipher_name, 1, StrLen(rawLUKSheader.cipher_name));
        LUKSheader.cipher_mode    := Copy(rawLUKSheader.cipher_mode, 1, StrLen(rawLUKSheader.cipher_mode));
        LUKSheader.hash_spec      := Copy(rawLUKSheader.hash_spec,   1, StrLen(rawLUKSheader.hash_spec));
        LUKSheader.payload_offset := SDUBigEndian32ToDWORD(rawLUKSheader.payload_offset);
        LUKSheader.key_bytes      := SDUBigEndian32ToDWORD(rawLUKSheader.key_bytes);
        CopyMemory(
                   @LUKSheader.mk_digest,
                   @rawLUKSheader.mk_digest,
                   sizeof(rawLUKSheader.mk_digest)
                  );
        CopyMemory(
                   @LUKSheader.mk_digest_salt,
                   @rawLUKSheader.mk_digest_salt,
                   sizeof(rawLUKSheader.mk_digest_salt)
                  );
        LUKSheader.mk_digest_iter := SDUBigEndian32ToDWORD(rawLUKSheader.mk_digest_iter);
        strGUID := Copy(rawLUKSheader.uuid, 1, StrLen(rawLUKSheader.uuid));
        strGUID := '{'+strGUID+'}';
        LUKSheader.uuid           := StringToGUID(strGUID);

        faultFlag := FALSE;
        for i:=0 to (LUKS_NUMKEYS-1) do
          begin
          keySlotActive := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].active);

          // Sanity check
          if (
              (keySlotActive <> LUKS_KEY_ENABLED) and
              (keySlotActive <> LUKS_KEY_DISABLED)
             ) then
            begin
            faultFlag := TRUE;
            break;
            end;

          LUKSheader.keySlot[i].active := (keySlotActive = LUKS_KEY_ENABLED);
          if (LUKSheader.keySlot[i].active) then
            begin
            LUKSheader.keySlot[i].iterations := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].iterations);

            CopyMemory(
                       @LUKSheader.keySlot[i].salt,
                       @rawLUKSheader.keySlot[i].salt,
                       sizeof(rawLUKSheader.keySlot[i].salt)
                      );

            LUKSheader.keySlot[i].key_material_offset := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].key_material_offset);
            LUKSheader.keySlot[i].stripes := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].stripes);
            end;

          end;

        allOK := not(faultFlag);
        end;

      end;

    end;  // if (length(rawData) = sizeof(rawLUKSheader)) then


  Result:= allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.ReadLUKSHeader(filename: string; var LUKSheader: TLUKSHeader): boolean;
var
  allOK: boolean;
  rawData: string;
begin
  allOK := FALSE;

  if ReadRawVolumeData(
                       filename,
                       0,  // LUKS volumes always start from the beginning
                       sizeof(TLUKSHeaderRaw),
                       rawData
                      ) then
    begin
    allOK := ParseLUKSHeader(rawData, LUKSheader);
    end;

  Result:= allOK;
end;


// ----------------------------------------------------------------------------
// If the hash and cypher members of the LUKS header passed in are populated,
// this function will populate struct with the the FreeOTFE driver details
// (e.g. driver name/GUID) that correspond to that hash/cypher
// Note: Must be called *after* ReadLUKSHeader(...)
function TOTFEFreeOTFEBase.MapToFreeOTFE(baseIVCypherOnHashLength: boolean; var LUKSheader: TLUKSHeader): boolean;
var
  allOK: boolean;
begin
  allOK := TRUE;

  // Identify the FreeOTFE hash driver to be used
  if (allOK) then
    begin
    allOK := IdentifyHash(LUKSHeader.hash_spec, LUKSHeader.hashKernelModeDeviceName, LUKSHeader.hashGUID);
    end;

  // Identify the FreeOTFE cypher driver to be used
  if (allOK) then
    begin
    allOK := IdentifyCypher(
                            LUKSHeader.cipher_name,
                            (LUKSHeader.key_bytes * 8),
                            LUKSHeader.cipher_mode,
                            baseIVCypherOnHashLength,
                            LUKSHeader.cypherKernelModeDeviceName,
                            LUKSHeader.cypherGUID,
                            LUKSHeader.sectorIVGenMethod,
                            LUKSHeader.IVHashKernelModeDeviceName,
                            LUKSHeader.IVHashGUID,
                            LUKSHeader.IVCypherKernelModeDeviceName,
                            LUKSHeader.IVCypherGUID
                           );

    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.DumpLUKSDataToFile(
  filename: string;
  userKey: string;
  keyfile: string;
  keyfileIsASCII: boolean;
  keyfileNewlineType: TSDUNewline;
  baseIVCypherOnHashLength: boolean;
  dumpFilename: string
): boolean;
var
  allOK: boolean;
  LUKSHeader: TLUKSHeader;
  i: integer;
  strFormattedGUID: string;
  hashTitle: string;
  cypherTitle: string;
  hashDetails: TFreeOTFEHash;
  cypherDetails: TFreeOTFECypher_v3;
  IVCypherDetails: TFreeOTFECypher_v3;
  keySlot: integer;
  keyMaterial: string;
  prettyPrintData: TStringList;
  dumpReport: TStringList;
  IVHashTitle: string;
  IVCypherTitle: string;
  userKeyOK: boolean;
begin
  CheckActive();

  dumpReport := TStringList.Create();
  try
    dumpReport.Add('LUKS Dump');
    dumpReport.Add('=========');
    dumpReport.Add('');
    dumpReport.Add('Dump Created By');
    dumpReport.Add('---------------');
    dumpReport.Add('Platform              : PC');
    dumpReport.Add('Application version   : v'+SDUGetVersionInfoString(ParamStr(0)));
    dumpReport.Add('Driver ID             : '+VersionStr());

    // Get the LUKS header from the volume...
    if not(ReadLUKSHeader(filename, LUKSHeader)) then
      begin
      if IsLUKSVolume(filename) then
        begin
        dumpReport.Add('ERROR: Unable to read LUKS header?!');
        end
      else
        begin
        dumpReport.Add('Unable to read LUKS header; this does not appear to be a LUKS volume.');
        end;

      end
    else
      begin
      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('cryptsetup Style Dump');
      dumpReport.Add('---------------------');
      dumpReport.Add('LUKS header information for '+filename);
      dumpReport.Add('');
      dumpReport.Add('Version:        '+inttostr(LUKSheader.version));
      dumpReport.Add('Cipher name:    '+LUKSheader.cipher_name);
      dumpReport.Add('Cipher mode:    '+LUKSheader.cipher_mode);
      dumpReport.Add('Hash spec:      '+LUKSheader.hash_spec);
      dumpReport.Add('Payload offset: '+inttostr(LUKSheader.payload_offset));
      dumpReport.Add('MK bits:        '+inttostr(LUKSheader.key_bytes * 8));  // Convert from bytes to bits
      dumpReport.Add('MK digest:      '+PrettyHex(
                                            @(LUKSheader.mk_digest),
                                            LUKS_DIGESTSIZE
                                           ));
      dumpReport.Add('MK salt:        '+PrettyHex(
                                            @(LUKSheader.mk_digest_salt[0]),
                                            (LUKS_SALTSIZE div 2)
                                           ));
      dumpReport.Add('                '+PrettyHex(
                                            @(LUKSheader.mk_digest_salt[(LUKS_SALTSIZE div 2)]),
                                            (LUKS_SALTSIZE div 2)
                                           ));
      dumpReport.Add('MK iterations:  '+inttostr(LUKSheader.mk_digest_iter));
      strFormattedGUID := GUIDToString(LUKSheader.uuid);
      // Remove leading and trailing "{" and "}"
      Delete(strFormattedGUID, 1, 1);
      Delete(strFormattedGUID, length(strFormattedGUID), 1);
      strFormattedGUID := lowercase(strFormattedGUID);
      dumpReport.Add('UUID:           '+strFormattedGUID);
      dumpReport.Add('');

      for i:=low(LUKSheader.keySlot) to high(LUKSheader.keySlot) do
        begin
        if (LUKSheader.keySlot[i].active = FALSE) then
          begin
          dumpReport.Add('Key Slot '+inttostr(i)+': DISABLED');
          end
        else
          begin
          dumpReport.Add('Key Slot '+inttostr(i)+': ENABLED');
          dumpReport.Add('        Iterations:             '+inttostr(LUKSheader.keySlot[i].iterations));
          dumpReport.Add('        Salt:                   '+PrettyHex(
                                                                @(LUKSheader.keySlot[i].salt[0]),
                                                                (LUKS_SALTSIZE div 2)
                                                               ));
          dumpReport.Add('                                '+PrettyHex(
                                                                @(LUKSheader.keySlot[i].salt[(LUKS_SALTSIZE div 2)]),
                                                                (LUKS_SALTSIZE div 2)
                                                               ));
          dumpReport.Add('        Key material offset:    '+inttostr(LUKSheader.keySlot[i].key_material_offset));
          dumpReport.Add('        AF stripes:             '+inttostr(LUKSheader.keySlot[i].stripes));
          end;
        end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Mapped FreeOTFE Drivers');
      dumpReport.Add('-----------------------');
      if not(MapToFreeOTFE(baseIVCypherOnHashLength, LUKSheader)) then
        begin
        dumpReport.Add('One or more of the following:');
        dumpReport.Add('');
        dumpReport.Add('  *) The hash algorithm');
        dumpReport.Add('  *) The cypher');
        dumpReport.Add('  *) The IV generation method');
        dumpReport.Add('');
        dumpReport.Add('specified in the LUKS header could not be mapped to a FreeOTFE equivalent.');
        dumpReport.Add('This may be because the required FreeOTFE driver hasn''t been installed and');
        dumpReport.Add('started, or because the required LUKS option is not supported in this');
        dumpReport.Add('version of FreeOTFE');
        end
      else
        begin
        hashTitle := 'ERROR: Unable to determine hash title?!';
        if GetSpecificHashDetails(
                                  LUKSheader.hashKernelModeDeviceName,
                                  LUKSheader.hashGUID,
                                  hashDetails
                                 ) then
          begin
          hashTitle := GetHashDisplayTechTitle(hashDetails);
          end;

        cypherTitle := 'ERROR: Unable to determine cypher title?!';
        if GetSpecificCypherDetails(
                                    LUKSheader.cypherKernelModeDeviceName,
                                    LUKSheader.cypherGUID,
                                    cypherDetails
                                   ) then
          begin
          cypherTitle := GetCypherDisplayTechTitle(cypherDetails);
          end;

        if (LUKSheader.sectorIVGenMethod <> foivgESSIV) then
          begin
          IVHashTitle := 'IV hash n/a';
          IVCypherTitle := 'IV cypher n/a';
          end
        else
          begin
          IVHashTitle := 'ERROR: Unable to determine IV hash title?!';
          if GetSpecificHashDetails(
                                    LUKSheader.IVHashKernelModeDeviceName,
                                    LUKSheader.IVHashGUID,
                                    hashDetails
                                   ) then
            begin
            IVHashTitle := GetHashDisplayTechTitle(hashDetails);
            end;

          IVCypherTitle := 'ERROR: Unable to determine IV cypher title?!';
          if GetSpecificCypherDetails(
                                      LUKSheader.IVCypherKernelModeDeviceName,
                                      LUKSheader.IVCypherGUID,
                                      IVCypherDetails
                                     ) then
            begin
            IVCypherTitle := GetCypherDisplayTechTitle(IVCypherDetails);
            end;
          end;

        dumpReport.Add('Hash pretty title         : '+hashTitle);
        dumpReport.Add('Hash driver KM name       : '+LUKSheader.hashKernelModeDeviceName);
        dumpReport.Add('Hash GUID                 : '+GUIDToString(LUKSheader.hashGUID));
        dumpReport.Add('Cypher pretty title       : '+cypherTitle);
        dumpReport.Add('Cypher driver KM name     : '+LUKSheader.cypherKernelModeDeviceName);
        dumpReport.Add('Cypher GUID               : '+GUIDToString(LUKSheader.cypherGUID));
        dumpReport.Add('Sector IV generation      : '+FreeOTFESectorIVGenMethodTitle[LUKSheader.sectorIVGenMethod]);
        if (LUKSheader.sectorIVGenMethod = foivgESSIV) then
          begin
          dumpReport.Add('  IV hash pretty title    : '+IVHashTitle);
          dumpReport.Add('  IV hash driver KM name  : '+LUKSheader.IVHashKernelModeDeviceName);
          dumpReport.Add('  IV hash GUID            : '+GUIDToString(LUKSheader.IVHashGUID));
          
          if baseIVCypherOnHashLength then
            begin
            dumpReport.Add('  IV cypher               : <based on IV hash length>');
            end
          else
            begin
            dumpReport.Add('  IV cypher               : <same as main cypher>');
            end;

          dumpReport.Add('  IV cypher pretty title  : '+IVCypherTitle);
          dumpReport.Add('  IV cypher driver KM name: '+LUKSheader.IVCypherKernelModeDeviceName);
          dumpReport.Add('  IV cypher GUID          : '+GUIDToString(LUKSheader.IVCypherGUID));
          end;
        end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Master Key');
      dumpReport.Add('----------');
      if (keyfile = '') then
        begin
        userKeyOK := TRUE;
        dumpReport.Add('User supplied password   : '+userKey);
        end
      else
        begin
        dumpReport.Add('Keyfile                  : '+keyfile);

        if keyfileIsASCII then
          begin
          dumpReport.Add('Treat keyfile as         : ASCII');
          dumpReport.Add('Newlines are             : '+SDUNEWLINE_TITLE[keyfileNewlineType]);
          end
        else
          begin
          dumpReport.Add('Treat keyfile as         : Binary');
          end;

        userKeyOK := ReadLUKSKeyFromFile(
                                         keyfile,
                                         keyfileIsASCII,
                                         keyfileNewlineType,
                                         userKey
                                        );

        if not(userKeyOK) then
          begin
          dumpReport.Add('Unable to read password from keyfile.');
          end
        else
          begin
          if keyfileIsASCII then
            begin
            dumpReport.Add('Password from keyfile    : '+userKey);
            end;

          dumpReport.Add('Password as binary       : ');
          prettyPrintData := TStringList.Create();
          try
            prettyPrintData.Clear();
            SDUPrettyPrintHex(
                              userKey,
                              0,
                              length(userKey),
                              prettyPrintData
                             );
            dumpReport.AddStrings(prettyPrintData);
          finally
            prettyPrintData.Free();
          end;

          end;

        end;

      if userKeyOK then
        begin
        if not(RecoverMasterKey(
                                filename,
                                userKey,
                                baseIVCypherOnHashLength,
                                LUKSHeader,
                                keySlot,
                                keyMaterial
                               )) then
          begin
          dumpReport.Add('No master key could be recovered with the specified password.');
          end
        else
          begin
          prettyPrintData := TStringList.Create();
          try
            dumpReport.Add('Password unlocks key slot: '+inttostr(keySlot));
            dumpReport.Add('Recovered master key     :');
            prettyPrintData.Clear();
            SDUPrettyPrintHex(
                              keyMaterial,
                              0,
                              length(keyMaterial),
                              prettyPrintData
                             );
            dumpReport.AddStrings(prettyPrintData);
          finally
            prettyPrintData.Free();
          end;

          end;
        end;

      end;

    // Save the report out to disk...
    dumpReport.SaveToFile(dumpFilename);
    allOK := TRUE;

  finally
    dumpReport.Free();
  end;


  if (allOK) then
    begin
    LastErrorCode := OTFE_ERR_SUCCESS;
    end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// This function is similar to SDUPrettyPrintHex from the SDUGeneral unit, but
// formats slightly differently
// This special function is required to generate the same format output as
// cryptsetup produces, in order to make diffing the output easier.
function TOTFEFreeOTFEBase.PrettyHex(data: Pointer; bytesCount: integer): string;
var
  i: integer;
  x: char;
  retVal: string;
begin
  retVal := '';

  for i:=0 to (bytesCount-1) do
    begin
    x := (PChar(data))[i];
    // Yeah, I know - this isn't too nice. There's always going to be an extra
    // space at the end of the line. Don't blame me, this is how
    // cryptosetup-luks does things - I'm just replicating it here so that the
    // output ban be diffed easily
    retVal := retVal + inttohex(ord(x), 2) + ' ';
    end;

  Result := lowercase(retVal);
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.RecoverMasterKey(
                                filename: string;
                                userPassword: string;
                                baseIVCypherOnHashLength: boolean;
                                var LUKSHeader: TLUKSHeader;
                                var keySlot: integer;
                                var keyMaterial: string
                               ): boolean;
var
  allOK: boolean;
  pwd_PBKDF2ed: string;
  foundValid: boolean;
  i, j:integer;
  keySlotSaltAsString: string;
  masterkeySaltAsString: string;
  generatedCheck: string;
  masterKey: string;
  splitDataLength: integer;
  splitData: string;
  tmpVolumeFlags: integer;
begin
  allOK := TRUE;

  keySlot := -1;


  // Get the LUKS header from the volume...
  if (allOK) then
    begin
    allOK := ReadLUKSHeader(filename, LUKSHeader);
    end;

  // Identify the FreeOTFE options to be used...
  if (allOK) then
    begin
    allOK := MapToFreeOTFE(baseIVCypherOnHashLength, LUKSHeader);
    end;



  if (allOK) then
    begin
    masterkeySaltAsString := '';
    for j:=low(LUKSHeader.mk_digest_salt) to high(LUKSHeader.mk_digest_salt) do
      begin
      masterkeySaltAsString := masterkeySaltAsString + char(LUKSHeader.mk_digest_salt[j]);
      end;
    end;


  if (allOK) then
    begin
    // For each keyslot...
    for i:=low(LUKSHeader.keySlot) to high(LUKSHeader.keySlot) do
      begin
      if (LUKSHeader.keySlot[i].Active) then
        begin
        keySlotSaltAsString := '';
        for j:=low(LUKSHeader.keySlot[i].salt) to high(LUKSHeader.keySlot[i].salt) do
          begin
          keySlotSaltAsString := keySlotSaltAsString + char(LUKSHeader.keySlot[i].salt[j]);
          end;

        // PBKDF2 the user's password with the salt
        if not(DeriveKey(
                    fokdfPBKDF2,
                    LUKSheader.hashKernelModeDeviceName,
                    LUKSheader.hashGUID,
                    LUKSheader.cypherKernelModeDeviceName,
                    LUKSheader.cypherGUID,
                    userPassword,
                    keySlotSaltAsString,
                    LUKSHeader.keySlot[i].iterations,
                    (LUKSHeader.key_bytes * 8),  // In *bits*
                    pwd_PBKDF2ed
                   )) then
          begin
          allOK := FALSE;
          break;
          end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS derived key follows:');
  DebugMsgBinary(pwd_PBKDF2ed);
{$ENDIF}

        splitDataLength := (LUKSheader.key_bytes * LUKSheader.keyslot[i].stripes);
        tmpVolumeFlags := 0;


        if not(ReadWritePlaintextToVolume(
                                  TRUE,  // Read, NOT write

                                  filename,

                                  pwd_PBKDF2ed,
                                  LUKSheader.sectorIVGenMethod,
                                  LUKSheader.IVHashKernelModeDeviceName,
                                  LUKSheader.IVHashGUID,
                                  LUKSheader.IVCypherKernelModeDeviceName,
                                  LUKSheader.IVCypherGUID,
                                  LUKSheader.cypherKernelModeDeviceName,
                                  LUKSheader.cypherGUID,
                                  tmpVolumeFlags,
                                  fomaFixedDisk,

                                  0,  // Offset from within mounted volume from where to read/write data
                                  splitDataLength,  // Length of data to read/write. In bytes
                                  splitData,  // Data to read/write

                                  (LUKS_SECTOR_SIZE * LUKSheader.KeySlot[i].key_material_offset)  // Offset within volume where encrypted data starts
                                  )) then
          begin
          allOK := FALSE;
          break;
          end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS decrypted split data follows:');
  DebugMsgBinary(splitData);
{$ENDIF}


        // Merge the split data
        if not(AFMerge(
                 splitData,
                 LUKSheader.keySlot[i].stripes,
                 LUKSheader.hashKernelModeDeviceName,
                 LUKSheader.hashGUID,
                 masterKey
                )) then
          begin
          allOK := FALSE;
          break;
          end;


{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS AF merged data follows:');
  DebugMsgBinary(masterKey);
{$ENDIF}


        // Generate PBKDF2 of the merged data
        if not(DeriveKey(
                    fokdfPBKDF2,
                    LUKSheader.hashKernelModeDeviceName,
                    LUKSheader.hashGUID,
                    LUKSheader.cypherKernelModeDeviceName,
                    LUKSheader.cypherGUID,
                    masterKey,
                    masterkeySaltAsString,
                    LUKSHeader.mk_digest_iter,
                    (sizeof(LUKSHeader.mk_digest) * 8),
                    generatedCheck
                   )) then
          begin
          allOK := FALSE;
          break;
          end;

{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('LUKS generated pbkdf2 checksum of recovered key follows:');
  DebugMsgBinary(generatedCheck);
{$ENDIF}

        // Check if the PBKDF2 of the merged data matches that of the MK diget
        foundValid := TRUE;
        for j:=1 to length(generatedCheck) do
          begin
          if (generatedCheck[j] <> char(LUKSHeader.mk_digest[j-1])) then
            begin
            foundValid := FALSE;
            break;
            end;

          end;

        if (foundValid) then
          begin
          // Valid keyslot found; break out of loop
          keySlot := i;
          keyMaterial := masterKey;
          break;
          end;

        end;

      end;
    
    end;


  allOK := allOK AND (keySlot <> -1);
  
  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyHash(hashSpec: string; var hashKernelModeDeviceName: string; var hashGUID: TGUID): boolean;
const
  // The Tiger hash is called "tgr" under Linux (why?!!)
  FREEOTFE_TIGER = 'TIGER';
  LUKS_TIGER     = 'TGR';
  // The Whirlpool hash is called "wp" under Linux (why?!!)
  FREEOTFE_WHIRLPOOL = 'WHIRLPOOL';
  LUKS_WHIRLPOOL     = 'WP';
var
  allOK: boolean;
  hashDrivers: array of TFreeOTFEHashDriver;
  hi, hj: integer;
  currDriver: TFreeOTFEHashDriver;
  currImpl: TFreeOTFEHash;
  idx: integer;
  validKernelDeviceName: TStringList;
  validGUID: TStringList;
  selectDlg: TfrmSelectHashCypher;
  i: integer;
  normalisedTitle: string;
  stillStripping: boolean;
  normalisedTitleLUKSified: string;
begin
  allOK := TRUE;

  // Standardise to uppercase; removes any potential problems with case
  // sensitivity
  hashSpec := uppercase(hashSpec);

  // Obtain details of all hashes...
  if (allOK) then
    begin
    SetLength(hashDrivers, 0);
    allOK := GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
    end;


  validKernelDeviceName:= TStringList.Create();
  try
    validGUID:= TStringList.Create();
    try
    // Locate the specific FreeOTFE hash to use...
    if (allOK) then
      begin
      // FOR ALL HASH DRIVERS...
      for hi:=low(hashDrivers) to high(hashDrivers) do
        begin
        currDriver := hashDrivers[hi];
        // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
        for hj:=low(currDriver.Hashes) to high(currDriver.Hashes) do
          begin
          currImpl := currDriver.Hashes[hj];


          // Strip out any "-" or whitespace in the hash's title; LUKS hashes
          // don't have these characters in their names
          normalisedTitle := currImpl.Title;
          stillStripping := TRUE;
          while (stillStripping) do
            begin
            stillStripping := FALSE;
            
            idx := Pos(' ', normalisedTitle);
            if (idx <= 0) then
              begin
              idx := Pos('-', normalisedTitle);
              end;

            if (idx > 0) then
              begin
              Delete(normalisedTitle, idx, 1);
              stillStripping := TRUE;
              end;

            end;

          normalisedTitle := uppercase(normalisedTitle);

          // The Tiger hash is called "tgr" under Linux (why?!!)
          // Because of this weirdness, we have to carry out an additional
          // check for LUKS volumes encrypted with "tgr" (Tiger) cypher
          // Note: The tiger hash isn't covered by the LUKS specification; it's
          //       support here (and in the Linux implementation) is an
          //       *optional* extension to the standard.
          // Note: Tiger isn't included in the v2.6.11.7 Linux kernel, but is
          //       included in the v2.6.19 kernel
          //
          // Examples of Tiger hash specs:
          //   tgr128
          //   tgr160
          //   tgr190
          normalisedTitleLUKSified := normalisedTitle;

          if (pos(FREEOTFE_TIGER, normalisedTitleLUKSified) > 0) then
            begin
            // If there's no numbers in our FreeOTFE's hash driver's title, add
            // on the hash length in bits; see the example above of this hash's
            // hashspec under Linux
            if not(StringHasNumbers(normalisedTitleLUKSified)) then
              begin
              normalisedTitleLUKSified := normalisedTitleLUKSified + inttostr(currImpl.Length);
              end;

            normalisedTitleLUKSified := StringReplace(
                                                      normalisedTitleLUKSified,
                                                      uppercase(FREEOTFE_TIGER),
                                                      uppercase(LUKS_TIGER),
                                                      []
                                                     );
            end;

          // A similar thing happens with Whirlpool...
          // Examples of Whirlpool hash specs:
          //   wp256
          //   wp384
          //   wp512
          if (pos(FREEOTFE_WHIRLPOOL, normalisedTitleLUKSified) > 0) then
            begin
            // If there's no numbers in our FreeOTFE's hash driver's title, add
            // on the hash length in bits; see the example above of this hash's
            // hashspec under Linux
            if not(StringHasNumbers(normalisedTitleLUKSified)) then
              begin
              normalisedTitleLUKSified := normalisedTitleLUKSified + inttostr(currImpl.Length);
              end;

            normalisedTitleLUKSified := StringReplace(
                                                      normalisedTitleLUKSified,
                                                      uppercase(FREEOTFE_WHIRLPOOL),
                                                      uppercase(LUKS_WHIRLPOOL),
                                                      []
                                                     );
            end;

          // Check current hash implementation details against volume's
          if (
              (normalisedTitle = hashSpec) or
              (normalisedTitleLUKSified = hashSpec)
             ) then
            begin
            // Valid cypher found; add to list
            validGUID.Add(GUIDToString(currImpl.HashGUID));
            validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then
        begin
        allOK := FALSE;
        end
      else if (validGUID.Count = 1) then
        begin
        hashKernelModeDeviceName := validKernelDeviceName[0];
        hashGUID := StringToGUID(validGUID[0]);
        end
      else
        begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          selectDlg.FreeOTFEObj := self;
          for i:=0 to (validGUID.count-1) do
            begin
            selectDlg.AddCombination(
                                     validKernelDeviceName[i],
                                     StringToGUID(validGUID[i]),
                                     '',
                                     StringToGUID(NULL_GUID)
                                    );
            end;

          allOK := (selectDlg.ShowModal = mrOK);
          if (allOK) then
            begin
            hashKernelModeDeviceName := selectDlg.SelectedHashDriverKernelModeName();
            hashGUID := selectDlg.SelectedHashGUID();
            end;

        finally
          selectDlg.Free();
        end;

        end;

    finally
      validGUID.Free();
    end;

  finally
    validKernelDeviceName.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyCypher_SearchCyphers(
                                                    cypherDrivers: array of TFreeOTFECypherDriver;
                                                    cypherName: string;
                                                    keySizeBits: integer;
                                                    useCypherMode: TFreeOTFECypherMode;
                                                    var cypherKernelModeDeviceName: string;
                                                    var cypherGUID: TGUID
                                                   ): boolean;
var
  allOK: boolean;
  validKernelDeviceName: TStringList;
  validGUID: TStringList;
  selectDlg: TfrmSelectHashCypher;
  ci, cj: integer;
  currDriver: TFreeOTFECypherDriver;
  currImpl: TFreeOTFECypher_v3;
  i: integer;
begin
  allOK := TRUE;

  validKernelDeviceName:= TStringList.Create();
  try
    validGUID:= TStringList.Create();
    try
    // Locate the specific FreeOTFE cypher to use...
    if (allOK) then
      begin
      // FOR ALL CYPHER DRIVERS...
      for ci:=low(cypherDrivers) to high(cypherDrivers) do
        begin
        currDriver := cypherDrivers[ci];
        // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
        for cj:=low(currDriver.Cyphers) to high(currDriver.Cyphers) do
          begin
          currImpl := currDriver.Cyphers[cj];

          // Check current cypher implementation details against volume's
          if (
              (uppercase(currImpl.Title) = cypherName) and
              (currImpl.KeySizeRequired = keySizeBits) and
              (currImpl.Mode = useCypherMode)
             ) then
            begin
            // Valid cypher found; add to list
            validGUID.Add(GUIDToString(currImpl.CypherGUID));
            validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then
        begin
        allOK := FALSE;
        end
      else if (validGUID.Count = 1) then
        begin
        cypherKernelModeDeviceName := validKernelDeviceName[0];
        cypherGUID := StringToGUID(validGUID[0]);
        end
      else
        begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          selectDlg.FreeOTFEObj := self;
          for i:=0 to (validGUID.count-1) do
            begin
            selectDlg.AddCombination(
                                     '',
                                     StringToGUID(NULL_GUID),
                                     validKernelDeviceName[i],
                                     StringToGUID(validGUID[i])
                                    );
            end;

          allOK := (selectDlg.ShowModal = mrOK);
          if (allOK) then
            begin
            cypherKernelModeDeviceName := selectDlg.SelectedCypherDriverKernelModeName();
            cypherGUID := selectDlg.SelectedCypherGUID();
            end;

        finally
          selectDlg.Free();
        end;

        end;

    finally
      validGUID.Free();
    end;

  finally
    validKernelDeviceName.Free();
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
function TOTFEFreeOTFEBase.IdentifyCypher(
                         cypherName: string;
                         keySizeBits: integer;
                         cypherMode: string;
                         baseIVCypherOnHashLength: boolean;
                         var cypherKernelModeDeviceName: string;
                         var cypherGUID: TGUID;
                         var sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
                         var IVHashKernelModeDeviceName: string;
                         var IVHashGUID: TGUID;
                         var IVCypherKernelModeDeviceName: string;
                         var IVCypherGUID: TGUID
                        ): boolean;
const
  PLAIN_IV = 'plain';
  ESSIV_IV = 'essiv';  // Not defined in the LUKS spec, but supported by LUKS anyway
  BENBI_IV = 'benbi';  // Not defined in the LUKS spec, but supported by LUKS anyway
var
  allOK: boolean;
  cypherDrivers: array of TFreeOTFECypherDriver;
  idx: integer;
  currCypherMode: TFreeOTFECypherMode;
  useCypherMode: TFreeOTFECypherMode;
  IVGenMethod: string;
  IVHash: string;
  tmpStr: string;
  IVHashDetails: TFreeOTFEHash;
//  tmpSplitOK: boolean;
begin
  allOK := TRUE;

  // Standardise to uppercase; removes any potential problems with case
  // sensitivity
  cypherMode := uppercase(cypherMode);
  cypherName := uppercase(cypherName);


  // Detect IV generation method
  // Examples: ecb
  //           cbc-plain
  //           lrw-plain
  //           cbc-essiv:sha256
  // The stuff before the "-" is the cypher mode; the stuff after is the IV
  // generation
  // Note: ESSIV is NOT specified in the LUKS specification, but LUKS implements
  //       this anyway as "-essiv:<hash>"
  if (allOK) then
    begin
    // Fallback to none... (e.g. ECB)
    sectorIVGenMethod := foivgNone;
    IVHashKernelModeDeviceName:= '';
    IVHashGUID:= StringToGUID(NULL_GUID);

    if SDUSplitString(cypherMode, tmpStr, IVGenMethod, '-') then
      begin
      cypherMode := tmpStr;

      // Search for "-plain" and strip off cypher mode
      idx := pos(uppercase(PLAIN_IV), IVGenMethod);
      if (idx > 0) then
        begin
        sectorIVGenMethod := foivg32BitSectorID;
        end;

      // Search for "-essiv" and strip off cypher mode
      idx := pos(uppercase(ESSIV_IV), IVGenMethod);
      if (idx > 0) then
        begin
        sectorIVGenMethod := foivgESSIV;

        // Get the hash
        allOK := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        if (allOK) then
          begin
          allOK := IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
          end;

        end;

      // Search for "-benbi" and strip off cypher mode
      idx := pos(uppercase(BENBI_IV), IVGenMethod);
      if (idx > 0) then
        begin
        sectorIVGenMethod := foivgNone;

        // Get the hash
        // Optional/not needed for "benbi"?
        //tmpSplitOK := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        //if (tmpSplitOK) then
        //  begin
        //  allOK := IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
        //  end;

        end;

      end;
    end;

  // Determine cypher mode
  useCypherMode := focmUnknown;
  if (allOK) then
    begin
    for currCypherMode := low(TFreeOTFECypherMode) to high(TFreeOTFECypherMode) do
      begin
      if (uppercase(FreeOTFECypherModeTitle(currCypherMode)) = cypherMode) then
        begin
        useCypherMode := currCypherMode;
        break;
        end;

      end;
      
    allOK := (useCypherMode <> focmUnknown);
    end;

  // Obtain details of all cyphers...
  if (allOK) then
    begin
    SetLength(cypherDrivers, 0);
    allOK := GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
    end;

  // Given the information we've determined, identify the FreeOTFE driver to
  // map the details to
  if (allOK) then
    begin
    allOK := IdentifyCypher_SearchCyphers(
                                          cypherDrivers,
                                          cypherName,
                                          keySizeBits,
                                          useCypherMode,
                                          cypherKernelModeDeviceName,
                                          cypherGUID
                                         );
    end;

  // Sort out the IV cypher driver
  if (allOK) then
    begin
    IVCypherKernelModeDeviceName:= cypherKernelModeDeviceName;
    IVCypherGUID:= cypherGUID;

    // Because of a bug in dm-crypt, the cypher used to generate ESSIV IVs may
    // ***NOT*** be the same cypher as used for the bulk encryption of data
    // This is because it passes the IV hash's length to the cypher when it
    // creates the ESSIV cypher - as a result, the ESSIV cypher may have a
    // different keylength to the bulk encryption cypher
    if (
        baseIVCypherOnHashLength and
        (sectorIVGenMethod = foivgESSIV)
       ) then
      begin
      allOK := GetSpecificHashDetails(
                                      IVHashKernelModeDeviceName,
                                      IVHashGUID,
                                      IVHashDetails
                                     );

      if (allOK) then
        begin
        allOK := IdentifyCypher_SearchCyphers(
                                              cypherDrivers,
                                              cypherName,
                                              IVHashDetails.Length,
                                              useCypherMode,
                                              IVCypherKernelModeDeviceName,
                                              IVCypherGUID
                                             );
        end;
      end;
      
    end;
    

  Result := allOK;
end;

// ----------------------------------------------------------------------------
// Read a LUKS key in from a file
function TOTFEFreeOTFEBase.ReadLUKSKeyFromFile(
  filename: string;
  treatAsASCII: boolean;
  ASCIINewline: TSDUNewline;
  out key: string
): boolean;
var
  retval: boolean;
  newlinePos: integer;
begin
  retval := SDUGetFileContent(filename, key);

  if retval then
    begin
    // If the input file was non-binary, only read in to the first newline
    if treatAsASCII then
      begin
      newlinePos := Pos(SDUNEWLINE_STRING[ASCIINewline], key);
      if (newlinePos > 0) then
        begin
        // -1 to actually *exclude* the newline
        key := Copy(Key, 1, (newlinePos-1));
        end;
      end;
    end;

  Result := retval;
end;


// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------


