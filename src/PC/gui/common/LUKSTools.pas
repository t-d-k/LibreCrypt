unit LUKSTools;

{
create open etc LUKS
(c) tdk
}
interface

uses
  //delphi
  //3rd party
  //SDU ,lclibs
  SDUGeneral,
  lcDebugLog,
  //librecrypt
  OTFEFreeOTFE_LUKSAPI,
  OTFEFreeOTFEBase_U, DriverAPI;

function CreateNewLUKS(fileName: String;
  password: TSDUBytes;
  contSizeBytes: Uint64;
  hash: String;
  out msg: String;
  out drive: DriveLetterChar;
  out refreshDrives: Boolean): Boolean;


{creates and mounts but doesn't format }
function CreateLUKS(volumeFilename: String;
  password: TSDUBytes;
  contSizeBytes: Uint64;
  hash: String;
  out errMsg: String; out drive: DriveLetterChar): Boolean;


function DumpLUKSDataToFile(filename: String; userKey: TSDUBytes;
  keyfile: String; keyfileIsASCII: Boolean; keyfileNewlineType: TSDUNewline;
  baseIVCypherOnHashLength: Boolean; dumpFilename: String): Boolean;


function MountLUKS(volumeFilename: String;
  var mountedAs: DriveLetterChar; ReadOnly: Boolean = False; password: TSDUBytes = nil;
  keyfile: String = ''; keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: Boolean = False): Boolean;


// Determine if the specified volume is a Linux LUKS volume
function IsLUKSVolume(volumeFilename: String): Boolean;

// Read a LUKS key in from a file
function ReadLUKSKeyFromFile(filename: String; treatAsASCII: Boolean;
  ASCIINewline: TSDUNewline; out key: TSDUBYtes): Boolean;


implementation

uses
  //delphi
  Winapi.Windows,
  System.SysUtils,//format
  system.Classes,
  vcl.Controls,
  //3rd party
  //SDU ,lclibs
  SDUSysUtils, SDUi18n,
  PKCS11Lib, SDUEndianIntegers,
  //librecrypt
  SDURandPool,
  OTFEFreeOTFE_U,
  OTFEConsts_U,
  AFSplitMerge, frmKeyEntryLUKS, frmSelectHashCypher;


{
creates luks volume with given parameters
this will be in new luks wizard}
function CreateNewLUKS(fileName: String;
  password: TSDUBytes;
  contSizeBytes: Uint64;
  hash: String;
  out msg: String;
  out drive: DriveLetterChar;
  out refreshDrives: Boolean): Boolean;
var
  prevMounted: DriveLetterString;

begin
  refreshDrives := False;

  prevMounted := GetFreeOTFEBase().DrivesMounted;
  Result      := CreateLUKS(fileName, password, contSizeBytes, hash, msg, drive);

  if Result then begin
    Result := Format_Drive(drive, True);
    if not Result then
      // Volumes couldn't be mounted for some reason...
      msg := FORMAT_ERR;
  end;

  if not Result then begin
    if (GetFreeOTFEBase().LastErrorCode <> OTFE_ERR_USER_CANCEL) then
      msg := _('LUKS container could not be created');
  end else begin
    // may be created but not mounted or formatted
    //    newMounted := GetFreeOTFEBase().DrivesMounted;
    if drive = #0 then begin
      msg := _('LUKS container created successfully.') + SDUCRLF + SDUCRLF +
        _('Please mount, and format this container''s free space before use.');
    end else begin
      // Get new drive on display...
      refreshDrives := True;
      msg           := format(_('LUKS container created successfully and opened as: %s.'),
        [drive]);
      // If the "drive letters" mounted have changed, the new volume was
      // automatically mounted; setup for this
  {  if (newMounted = prevMounted) then begin

    end else begin


      createdMountedAs := '';
      for i := 1 to length(newMounted) do begin
        if (Pos(newMounted[i], prevMounted) = 0) then begin
          if (createdMountedAs <> '') then begin
            // If the user mounted another volume during volume creation, we
            // can't tell from here which was the new volume
            createdMountedAs := createdMountedAs + ' / ';
          end;

          createdMountedAs := createdMountedAs + newMounted[i] + ':';
        end;
      end;

      msg := format(_('LUKS container created successfully and opened as: %s.'),
        [createdMountedAs]);
      }
    end;
  end;

end;



function _StringHasNumbers(checkString: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 1 to length(checkString) do begin
    if ((checkString[i] >= '0') and (checkString[i] <= '9')) then begin
      Result := True;
      break;
    end;
  end;
end;

function _IdentifyHash(hashSpec: String; var hashKernelModeDeviceName: String;
  var hashGUID: TGUID): Boolean;
const
  // The Tiger hash is called "tgr" under Linux (why?!!)
  FREEOTFE_TIGER     = 'TIGER';
  LUKS_TIGER         = 'TGR';
  // The Whirlpool hash is called "wp" under Linux (why?!!)
  FREEOTFE_WHIRLPOOL = 'WHIRLPOOL';
  LUKS_WHIRLPOOL     = 'WP';
var
  hashDrivers:           array of TFreeOTFEHashDriver;
  hi, hj:                Integer;
  currDriver:            TFreeOTFEHashDriver;
  currImpl:              TFreeOTFEHash;
  idx:                   Integer;
  validKernelDeviceName: TStringList;
  validGUID:             TStringList;
  selectDlg:             TfrmSelectHashCypher;
  i:                     Integer;
  normalisedTitle:       String;
  stillStripping:        Boolean;
  normalisedTitleLUKSified: String;
begin
  Result := True;

  // Standardise to uppercase; removes any potential problems with case
  // sensitivity
  hashSpec := uppercase(hashSpec);

  // Obtain details of all hashes...
  if Result then begin
    SetLength(hashDrivers, 0);
    Result := GetFreeOTFEBase().GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers));
  end;


  validKernelDeviceName := TStringList.Create();
  try
    validGUID := TStringList.Create();
    try
      // Locate the specific FreeOTFE hash to use...
      if Result then begin
        // FOR ALL HASH DRIVERS...
        for hi := low(hashDrivers) to high(hashDrivers) do begin
          currDriver := hashDrivers[hi];
          // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
          for hj := low(currDriver.Hashes) to high(currDriver.Hashes) do begin
            currImpl := currDriver.Hashes[hj];


            // Strip out any "-" or whitespace in the hash's title; LUKS hashes
            // don't have these characters in their names
            normalisedTitle := currImpl.Title;
            stillStripping  := True;
            while (stillStripping) do begin
              stillStripping := False;

              idx := Pos(' ', normalisedTitle);
              if (idx <= 0) then begin
                idx := Pos('-', normalisedTitle);
              end;

              if (idx > 0) then begin
                Delete(normalisedTitle, idx, 1);
                stillStripping := True;
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

            if (pos(FREEOTFE_TIGER, normalisedTitleLUKSified) > 0) then begin
              // If there's no numbers in our FreeOTFE's hash driver's title, add
              // on the hash length in bits; see the example above of this hash's
              // hashspec under Linux
              if not (_StringHasNumbers(normalisedTitleLUKSified)) then begin
                normalisedTitleLUKSified := normalisedTitleLUKSified + IntToStr(currImpl.Length);
              end;

              normalisedTitleLUKSified :=
                StringReplace(normalisedTitleLUKSified, uppercase(FREEOTFE_TIGER),
                uppercase(LUKS_TIGER), []);
            end;

            // A similar thing happens with Whirlpool...
            // Examples of Whirlpool hash specs:
            //   wp256
            //   wp384
            //   wp512
            if (pos(FREEOTFE_WHIRLPOOL, normalisedTitleLUKSified) > 0) then begin
              // If there's no numbers in our FreeOTFE's hash driver's title, add
              // on the hash length in bits; see the example above of this hash's
              // hashspec under Linux
              if not (_StringHasNumbers(normalisedTitleLUKSified)) then begin
                normalisedTitleLUKSified := normalisedTitleLUKSified + IntToStr(currImpl.Length);
              end;

              normalisedTitleLUKSified :=
                StringReplace(normalisedTitleLUKSified,
                uppercase(FREEOTFE_WHIRLPOOL), uppercase(LUKS_WHIRLPOOL), []);
            end;

            // Check current hash implementation details against volume's
            if ((normalisedTitle = hashSpec) or (normalisedTitleLUKSified = hashSpec))
            then begin
              // Valid cypher found; add to list
              validGUID.Add(GUIDToString(currImpl.HashGUID));
              validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then begin
        Result := False;
      end else
      if (validGUID.Count = 1) then begin
        hashKernelModeDeviceName := validKernelDeviceName[0];
        hashGUID                 := StringToGUID(validGUID[0]);
      end else begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          //          selectDlg.FreeOTFEObj := self;
          for i := 0 to (validGUID.Count - 1) do begin
            selectDlg.AddCombination(
              validKernelDeviceName[i],
              StringToGUID(validGUID[i]),
              '',
              StringToGUID(NULL_GUID)
              );
          end;

          Result := (selectDlg.ShowModal = mrOk);
          if Result then begin
            hashKernelModeDeviceName := selectDlg.SelectedHashDriverKernelModeName();
            hashGUID                 := selectDlg.SelectedHashGUID();
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

end;



function _IdentifyCypher_SearchCyphers(
  cypherDrivers: array of TFreeOTFECypherDriver; cypherName: String;
  keySizeBits: Integer; useCypherMode: TFreeOTFECypherMode;
  var cypherKernelModeDeviceName: String; var cypherGUID: TGUID): Boolean;
var
  validKernelDeviceName: TStringList;
  validGUID:             TStringList;
  selectDlg:             TfrmSelectHashCypher;
  ci, cj:                Integer;
  currDriver:            TFreeOTFECypherDriver;
  currImpl:              TFreeOTFECypher_v3;
  i:                     Integer;
begin
  Result := True;

  validKernelDeviceName := TStringList.Create();
  try
    validGUID := TStringList.Create();
    try
      // Locate the specific FreeOTFE cypher to use...
      if Result then begin
        // FOR ALL CYPHER DRIVERS...
        for ci := low(cypherDrivers) to high(cypherDrivers) do begin
          currDriver := cypherDrivers[ci];
          // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
          for cj := low(currDriver.Cyphers) to high(currDriver.Cyphers) do begin
            currImpl := currDriver.Cyphers[cj];

            // Check current cypher implementation details against volume's
            if ((uppercase(currImpl.Title) = cypherName) and
              (currImpl.KeySizeRequired = keySizeBits) and
              (currImpl.Mode = useCypherMode)) then begin
              // Valid cypher found; add to list
              validGUID.Add(GUIDToString(currImpl.CypherGUID));
              validKernelDeviceName.Add(currDriver.LibFNOrDevKnlMdeName);
            end;

          end;  // for cj:=low(currCypherDriver.Cyphers) to high(currCypherDriver.Cyphers) do
        end;  // for ci:=low(cypherDrivers) to high(cypherDrivers) do
      end;


      // Select the appropriate cypher details...
      if (validGUID.Count <= 0) then begin
        Result := False;
      end else
      if (validGUID.Count = 1) then begin
        cypherKernelModeDeviceName := validKernelDeviceName[0];
        cypherGUID                 := StringToGUID(validGUID[0]);
      end else begin
        selectDlg := TfrmSelectHashCypher.Create(nil);
        try
          //          selectDlg.FreeOTFEObj := self;
          for i := 0 to (validGUID.Count - 1) do begin
            selectDlg.AddCombination(
              '',
              StringToGUID(NULL_GUID),
              validKernelDeviceName[i],
              StringToGUID(validGUID[i])
              );
          end;

          Result := (selectDlg.ShowModal = mrOk);
          if Result then begin
            cypherKernelModeDeviceName := selectDlg.SelectedCypherDriverKernelModeName();
            cypherGUID                 := selectDlg.SelectedCypherGUID();
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

end;


function _IdentifyCypher(cypherName: String; keySizeBits: Integer;
  cypherMode: String; baseIVCypherOnHashLength: Boolean; var cypherKernelModeDeviceName: String;
  var cypherGUID: TGUID; var sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  var IVHashKernelModeDeviceName: String; var IVHashGUID: TGUID;
  var IVCypherKernelModeDeviceName: String; var IVCypherGUID: TGUID): Boolean;
const
  PLAIN_IV = 'plain';
  ESSIV_IV = 'essiv';  // Not defined in the LUKS spec, but supported by LUKS anyway
  BENBI_IV = 'benbi';  // Not defined in the LUKS spec, but supported by LUKS anyway
var
  cypherDrivers:  array of TFreeOTFECypherDriver;
  idx:            Integer;
  currCypherMode: TFreeOTFECypherMode;
  useCypherMode:  TFreeOTFECypherMode;
  IVGenMethod:    String;
  IVHash:         String;
  tmpStr:         String;
  IVHashDetails:  TFreeOTFEHash;
  //  tmpSplitOK: boolean;
begin
  Result := True;

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
  if Result then begin
    // Fallback to none... (e.g. ECB)
    sectorIVGenMethod          := foivgNone;
    IVHashKernelModeDeviceName := '';
    IVHashGUID                 := StringToGUID(NULL_GUID);

    if SDUSplitString(cypherMode, tmpStr, IVGenMethod, '-') then begin
      cypherMode := tmpStr;

      // Search for "-plain" and strip off cypher mode
      idx := pos(uppercase(PLAIN_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivg32BitSectorID;
      end;

      // Search for "-essiv" and strip off cypher mode
      idx := pos(uppercase(ESSIV_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivgESSIV;

        // Get the hash
        Result := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        if Result then begin
          Result := _IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
        end;

      end;

      // Search for "-benbi" and strip off cypher mode
      idx := pos(uppercase(BENBI_IV), IVGenMethod);
      if (idx > 0) then begin
        sectorIVGenMethod := foivgNone;

        // Get the hash
        // Optional/not needed for "benbi"?
        //tmpSplitOK := SDUSplitString(IVGenMethod, tmpStr, IVHash, ':');
        //if (tmpSplitOK) then
        //  begin
        //  Result := IdentifyHash(IVHash, IVHashKernelModeDeviceName, IVHashGUID);
        //  end;

      end;

    end;
  end;

  // Determine cypher mode
  useCypherMode := focmUnknown;
  if Result then begin
    for currCypherMode := low(TFreeOTFECypherMode) to high(TFreeOTFECypherMode) do begin
      if (uppercase(FreeOTFECypherModeTitle(currCypherMode)) = cypherMode) then begin
        useCypherMode := currCypherMode;
        break;
      end;

    end;

    Result := (useCypherMode <> focmUnknown);
  end;

  // Obtain details of all cyphers...
  if Result then begin
    SetLength(cypherDrivers, 0);
    Result := GetFreeOTFEBase().GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers));
  end;

  // Given the information we've determined, identify the FreeOTFE driver to
  // map the details to
  if Result then begin
    Result := _IdentifyCypher_SearchCyphers(cypherDrivers, cypherName, keySizeBits,
      useCypherMode, cypherKernelModeDeviceName, cypherGUID);
  end;

  // Sort out the IV cypher driver
  if Result then begin
    IVCypherKernelModeDeviceName := cypherKernelModeDeviceName;
    IVCypherGUID                 := cypherGUID;

    // Because of a bug in dm-crypt, the cypher used to generate ESSIV IVs may
    // ***NOT*** be the same cypher as used for the bulk encryption of data
    // This is because it passes the IV hash's length to the cypher when it
    // creates the ESSIV cypher - as a result, the ESSIV cypher may have a
    // different keylength to the bulk encryption cypher
    if (baseIVCypherOnHashLength and (sectorIVGenMethod = foivgESSIV)) then begin
      Result := GetFreeOTFEBase().GetSpecificHashDetails(IVHashKernelModeDeviceName,
        IVHashGUID, IVHashDetails);

      if Result then begin
        Result := _IdentifyCypher_SearchCyphers(cypherDrivers, cypherName,
          IVHashDetails.Length, useCypherMode, IVCypherKernelModeDeviceName, IVCypherGUID);
      end;
    end;

  end;

end;

 // ----------------------------------------------------------------------------
 // If the hash and cypher members of the LUKS header passed in are populated,
 // this function will populate struct with the the FreeOTFE driver details
 // (e.g. driver name/GUID) that correspond to that hash/cypher
 // Note: Must be called *after* _ReadLUKSHeader(...)
function _MapToFreeOTFE(baseIVCypherOnHashLength: Boolean;
  var LUKSheader: TLUKSHeader): Boolean;
begin
  Result := True;

  // Identify the FreeOTFE hash driver to be used
  if Result then begin
    Result := _IdentifyHash(LUKSHeader.hash_spec, LUKSHeader.hashKernelModeDeviceName,
      LUKSHeader.hashGUID);
  end;

  // Identify the FreeOTFE cypher driver to be used
  if Result then begin
    Result := _IdentifyCypher(LUKSHeader.cipher_name, (LUKSHeader.key_bytes * 8),
      LUKSHeader.cipher_mode, baseIVCypherOnHashLength, LUKSHeader.cypherKernelModeDeviceName,
      LUKSHeader.cypherGUID, LUKSHeader.sectorIVGenMethod, LUKSHeader.IVHashKernelModeDeviceName,
      LUKSHeader.IVHashGUID, LUKSHeader.IVCypherKernelModeDeviceName,
      LUKSHeader.IVCypherGUID);

  end;

end;


function _LUKSHeaderToLUKSRaw(
  const LUKSheader: TLUKSHeader; var rawLUKSheader: TLUKSHeaderRaw): Boolean;
var

  strGUID:       Ansistring;
  i:             Integer;
  keySlotActive: DWORD;
begin
  Result := False;

  if (LUKSheader.magic = LUKS_MAGIC) then begin
    // OK, this loop is crude - but it works
    assert(length(rawLUKSheader.magic) = length(LUKS_MAGIC));
    for i := low(rawLUKSheader.magic) to high(rawLUKSheader.magic) do
      rawLUKSheader.magic[i] := Byte(LUKS_MAGIC[i + 1]);
    // As per the LUKS specifications: If the LUKS version is of a
    // later version than we support - we make no attempt to interpret
    // it, and simply return an error
    if (LUKSheader.version <= LUKS_VER_SUPPORTED) then begin
      rawLUKSheader.version[0] := LUKSheader.version shr 8;
      rawLUKSheader.version[1] := LUKSheader.version and $FF;
      { TODO 1 -otdk -csecurity : wipe cipher names etc of garbage }
      StrPCopy(rawLUKSheader.cipher_name, Ansistring(LUKSheader.cipher_name));
      StrPCopy(rawLUKSheader.cipher_mode, Ansistring(LUKSheader.cipher_mode));
      StrPCopy(rawLUKSheader.hash_spec, Ansistring(LUKSheader.hash_spec));
      rawLUKSheader.payload_offset := SDUDWORDToBigEndian32(LUKSheader.payload_offset);
      rawLUKSheader.key_bytes      := SDUDWORDToBigEndian32(LUKSheader.key_bytes);

      CopyMemory(@rawLUKSheader.mk_digest, @LUKSheader.mk_digest,
        sizeof(LUKSheader.mk_digest));
      CopyMemory(@rawLUKSheader.mk_digest_salt, @LUKSheader.mk_digest_salt,
        sizeof(LUKSheader.mk_digest_salt)
        );
      rawLUKSheader.mk_digest_iter := SDUDWORDToBigEndian32(LUKSheader.mk_digest_iter);

      strGUID := GUIDToString(LUKSheader.uuid);
      // strip off '{ .. }' from ends
      strGUID := Copy(strGUID, 2, length(strGUID) - 2);
      //todo: is this index right?
      StrPCopy(rawLUKSheader.uuid, strGUID);

      for i := 0 to (LUKS_NUMKEYS - 1) do begin
        if LUKSheader.keySlot[i].active then
          keySlotActive := LUKS_KEY_ENABLED
        else
          keySlotActive := LUKS_KEY_DISABLED;
        rawLUKSheader.keySlot[i].active := SDUDWORDToBigEndian32(keySlotActive);

        { fill all keyslots as cryptsetup wants valid data }
        //        if (LUKSheader.keySlot[i].active) then begin
        rawLUKSheader.keySlot[i].iterations :=
          SDUDWORDToBigEndian32(LUKSheader.keySlot[i].iterations);
        CopyMemory(@rawLUKSheader.keySlot[i].salt, @LUKSheader.keySlot[i].salt,
          sizeof(LUKSheader.keySlot[i].salt)
          );

        rawLUKSheader.keySlot[i].key_material_offset :=
          SDUDWORDToBigEndian32(LUKSheader.keySlot[i].key_material_offset);
        rawLUKSheader.keySlot[i].stripes             :=
          SDUDWORDToBigEndian32(LUKSheader.keySlot[i].stripes);
        //        end;

      end;

      Result := True;
    end;

  end;

end;


function _WriteLUKSHeader(filename: String;
  const LUKSheader: TLUKSHeader): Boolean;
var
  rawLUKSheader: TLUKSHeaderRaw;
  rawData:       Ansistring;
begin
  Result := False;
  if _LUKSHeaderToLUKSRaw(LUKSheader, rawLUKSheader) then begin
    setlength(rawData, sizeof(TLUKSHeaderRaw));
    StrMove(PAnsiChar(rawData), PAnsiChar(@rawLUKSheader), sizeof(TLUKSHeaderRaw));

    Result := GetFreeOTFEBase().WriteRawVolumeData(filename, 0,
      // LUKS volumes always start from the beginning
      rawData);
  end;
end;


  (*
encrypts master key using given user password and writes it, or returns false
see luks spec - actual password is stored across split data

process is:
  populate luks header cyphers etc
  generate master salt
  with keyslot
    generate keyslot salt
    set keyslot.iterations etc
    hash_key(user password,keyslot.iterations,keyslot.bytes,keyslot.salt)
    split 'master key' to make 'split_data'
    encrypt 'split_data' using hashed key
    write keyslot.stripes bytes at keyslot.key_material_offset

    hash master key using master salt
    set LUKSHeader.mk_digest = hashed key
  write luks header

*)
function _EncryptAndWriteMasterKey(const filename: String;
  const userPassword: TSDUBytes;
  baseIVCypherOnHashLength: Boolean; var LUKSHeader: TLUKSHeader; keySlot: Integer;
  const masterkey: TSDUBytes): Boolean;
var
  slot, j:                 Integer;
  pwd_PBKDF2ed, splitData: TSDUBytes;
  splitDataLength:         Integer;
  generatedCheck:          TSDUBytes;
  ZER:                     _LARGE_INTEGER;
  //  cyphertext:              Ansistring;
  Empty:                   Ansistring;
  splitDataStr:            Ansistring;
  key_offset:              Cardinal;
const
  USE_KEY_SLOT = 0;
begin
  ZER.QuadPart := 0;
  Empty        := '';

  baseIVCypherOnHashLength := True;

  // populate LibreCrypt cyphers in struct from read strings
  Result := _MapToFreeOTFE(baseIVCypherOnHashLength, LUKSHeader);

  GetRandPool().FillRandomData(LUKSHeader.mk_digest_salt);
  // set other keyslots to default

  if Result then begin

    key_offset := 8; //todo:how to calculate this?

    LUKSheader.payload_offset := 4096;
    //    Inc(key_offset, 256);
    // For each keyslot...
    for slot := 0 to high(LUKSHeader.keySlot) do begin
      LUKSHeader.keySlot[slot].Active := slot = USE_KEY_SLOT;
      if slot = USE_KEY_SLOT then begin
        GetRandPool.FillRandomData(LUKSHeader.keySlot[slot].salt);
        LUKSHeader.keySlot[slot].iterations := 1000;
        LUKSHeader.keySlot[slot].stripes    := 2;
      end else begin
        SDUZeroBuffer(LUKSHeader.keySlot[slot].salt);
        LUKSHeader.keySlot[slot].iterations := 0;
        LUKSHeader.keySlot[slot].stripes    := 0;
      end;


      LUKSHeader.keySlot[slot].key_material_offset := key_offset;
      Inc(key_offset, 256);
    end;
    DebugFlush();
    // PBKDF2 the user's password with the salt
    if not (GetFreeOTFEBase().DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
      LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
      LUKSheader.cypherGUID, userPassword, LUKSHeader.keySlot[USE_KEY_SLOT].salt,
      LUKSHeader.keySlot[USE_KEY_SLOT].iterations, (LUKSHeader.key_bytes * 8),  // In *bits*
      pwd_PBKDF2ed)) then begin
      Result := False;
    end;
    if Result then begin

      DebugMsg('LUKS derived key follows:');
      DebugMsgBinary(pwd_PBKDF2ed);
      DebugMsg(LUKSheader.key_bytes);
      DebugMsg(LUKSheader.keyslot[USE_KEY_SLOT].stripes);

      DebugFlush();

      DebugMsg('LUKS AF data to split follows:');
      DebugMsgBinary(masterKey);


      // split the master key data
      AFSPlit(masterKey, LUKSheader.keySlot[USE_KEY_SLOT].stripes,
        LUKSheader.hashKernelModeDeviceName, LUKSheader.hashGUID, splitData);



      splitDataLength := (LUKSheader.key_bytes * LUKSheader.keyslot[USE_KEY_SLOT].stripes);

      //   Result := _WriteLUKSHeader(filename, LUKSHeader);
      //encrypt and write
      splitDataStr := SDUBytesToString(splitData);
   (*  if not (GetFreeOTFEBase().EncryptSectorData(LUKSheader.cypherKernelModeDeviceName,LUKSheader.cypherGUID,
    ZER, splitDataLength, pwd_PBKDF2ed, Empty,splitDataStr, cyphertext)) then begin
     (*
    end; *)
      DebugFlush();
      if not (GetFreeOTFEBase().ReadWritePlaintextToVolume(False,//write
        filename, pwd_PBKDF2ed, LUKSheader.sectorIVGenMethod,
        LUKSheader.IVHashKernelModeDeviceName, LUKSheader.IVHashGUID,
        LUKSheader.IVCypherKernelModeDeviceName, LUKSheader.IVCypherGUID,
        LUKSheader.cypherKernelModeDeviceName, LUKSheader.cypherGUID,
        {tmpVolumeFlags}0, fomaRemovableDisk, {(LUKS_SECTOR_SIZE *
        LUKSheader.KeySlot[i].key_material_offset)}0,
                          // Offset from within mounted volume from where to read/write data
        splitDataLength,  // Length of data to read/write. In bytes
        splitData,        // Data to read/write

        (LUKS_SECTOR_SIZE * LUKSheader.KeySlot[USE_KEY_SLOT].key_material_offset)
        // Offset within volume where encrypted data starts
        )) then begin

        Result := False;
        exit;
      end;

      // strmove( LUKS_SECTOR_SIZE * LUKSheader.KeySlot[i].key_material_offset
      // WriteRawVolumeData(filename, LUKS_SECTOR_SIZE *
      //        LUKSheader.KeySlot[i].key_material_offset, cyphertext);

      DebugFlush();
      // Generate PBKDF2 of the master key (for msg digest)
      if not (GetFreeOTFEBase().DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
        LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
        LUKSheader.cypherGUID, masterKey, LUKSHeader.mk_digest_salt,
        LUKSHeader.mk_digest_iter, (sizeof(LUKSHeader.mk_digest) * 8), generatedCheck)) then begin
        Result := False;

      end;
      DebugFlush();

      DebugMsg('LUKS generated pbkdf2 checksum of master key follows:');
      DebugMsgBinary(generatedCheck);

      if Result then begin

        for j := 0 to length(generatedCheck) - 1 do
          LUKSHeader.mk_digest[j] := generatedCheck[j];

        Result := _WriteLUKSHeader(filename, LUKSHeader);
      end;
    end;
  end;
end;



 // ----------------------------------------------------------------------------
 // This function is similar to SDUPrettyPrintHex from the SDUGeneral unit, but
 // formats slightly differently
 // This special function is required to generate the same format output as
 // cryptsetup produces, in order to make diffing the output easier.
function _PrettyHex(data: Pointer; bytesCount: Integer): Ansistring;
var
  i: Integer;
  x: Ansichar;
begin
  Result := '';

  for i := 0 to (bytesCount - 1) do begin
    x      := (PAnsiChar(data))[i];
    // Yeah, I know - this isn't too nice. There's always going to be an extra
    // space at the end of the line. Don't blame me, this is how
    // cryptosetup-luks does things - I'm just replicating it here so that the
    // output can be diffed easily
    Result := Result + inttohex(Ord(x), 2) + ' ';
  end;

  Result := lowercase(Result);
end;


{creates and mounts but doesn't format }
function CreateLUKS(volumeFilename: String;
  password: TSDUBytes;
  contSizeBytes: Uint64;
  hash: String;
  out errMsg: String; out drive: DriveLetterChar): Boolean;

var
  volumeKey: TSDUBytes;

  //  userKey:          TSDUBytes;
  //  fileOptSize:      Int64;
  //  mountDriveLetter: DriveLetterChar;
  //  mountReadonly:    Boolean;
  //  currDriveLetter: DriveLetterChar;
  //  mountMountAs:    TFreeOTFEMountAs;

  LUKSHeader: TLUKSHeader;
  userCancel: Boolean;

  //  keySlot:    Integer;
  //  baseIVCypherOnHashLength: Boolean;
  //  mountForAllUsers:         Boolean;

begin
  if FileExists(volumeFilename, True) then begin
    Result := False;
    exit;
  end;

  if not (SDUCreateLargeFile(volumeFilename, contSizeBytes{bytes}, True, userCancel)) then begin
    if not userCancel then
      errMsg := _('An error occurred while trying to create your LUKS container');
    Result := False;
  end else begin

    Result := True;
    GetRandPool.SetUpRandPool([rngcryptlib], PKCS11_NO_SLOT_ID, '');
    GetFreeOTFEBase().CheckActive();
    GetRandPool.GetRandomData(64, volumeKey);

    // dont use fillchar - as has ref  counted strings so wd et mem leaks
    LUKSHeader.magic       := LUKS_MAGIC;
    LUKSHeader.version     := LUKS_VER_SUPPORTED;
    LUKSHeader.cipher_name := 'aes';
    LUKSHeader.cipher_mode := 'xts-plain64';
    LUKSHeader.hash_spec   := hash;


    //    payload_offset: DWORD; in EncryptAndWriteMasterKey
    LUKSHeader.key_bytes := 64; // master key size * stripes// In *bytes*
    Fillchar(LUKSHeader.mk_digest, LUKS_DIGESTSIZE, 0);
    LUKSHeader.mk_digest_iter := 1000;
    //    mk_digest_salt: array [0..(LUKS_SALTSIZE-1)] of byte;   in EncryptAndWriteMasterKey
    //    mk_digest_iter: DWORD;  in EncryptAndWriteMasterKey
    { TODO 1 -otdk -ccleanup : get guid from OS - reveals OS instance? is privacy issue?}
    GetRandPool.GetRandomData(sizeof(LUKSHeader.uuid), PByteArray(@LUKSHeader.uuid));

    //    keySlot: array [0..(LUKS_NUMKEYS-1)] of TLUKSKeySlot;  // Note: Indexes from 1  in EncryptAndWriteMasterKey
    // in EncryptAndWriteMasterKey?
    LUKSHeader.cypherKernelModeDeviceName   := '';
    LUKSHeader.cypherGUID.D1                := 0;
    LUKSHeader.hashKernelModeDeviceName     := '';
    LUKSHeader.hashGUID.D1                  := 0;
    LUKSHeader.sectorIVGenMethod            := foivg32BitSectorID;
    LUKSHeader.IVHashKernelModeDeviceName   := '';
    LUKSHeader.IVHashGUID.D1                := 0;
    LUKSHeader.IVCypherKernelModeDeviceName := '';
    LUKSHeader.IVCypherGUID.D1              := 0;


    if not (_EncryptAndWriteMasterKey(volumeFilename, password,
      True{baseIVCypherOnHashLength}, LUKSHeader, 0{ keySlot}, volumeKey)) then
      Result := False;

    DebugMsg('Created key OK');
    DebugMsg('key slot ' + IntToStr(0) + ' unlocked.');
    DebugMsg('Master key: ' + _PrettyHex(PChar(volumeKey), length(volumeKey)));
    if Result then begin

      Result := MountLUKS(volumeFilename, drive, False, password, '', False,
        LINUX_KEYFILE_DEFAULT_NEWLINE, True);
    end;

  end;
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
 //     8   salt                   32    byte[]     salt parameter for PBKDF2
 //    40   key-material-offset     4    uint32_t   start sector of key material
 //    44   stripes                 4    uint32_t   number of anti-forensic stripes
 //  =====  ==================
 //    48   total keyslot size
 //  =====  ==================
function _ParseLUKSHeader(rawData: Ansistring;
  var LUKSheader: TLUKSHeader): Boolean;
var
  rawLUKSheader: TLUKSHeaderRaw;
  strGUID:       Ansistring;
  i:             Integer;
  faultFlag:     Boolean;
  keySlotActive: DWORD;
begin
  Result := False;

  if (length(rawData) = sizeof(rawLUKSheader)) then begin
    StrMove(@rawLUKSheader, PAnsiChar(rawData), Length(rawData));

    // OK, this loop is crude - but it works
    LUKSheader.magic := '';
    for i := low(rawLUKSheader.magic) to high(rawLUKSheader.magic) do
      LUKSheader.magic := LUKSheader.magic + Ansichar(rawLUKSheader.magic[i]);


    if (LUKSheader.magic = LUKS_MAGIC) then begin
      LUKSheader.version := rawLUKSheader.version[1] + (rawLUKSheader.version[0] shl 8);
      // As per the LUKS specifications: If the LUKS version is of a
      // later version than we support - we make no attempt to interpret
      // it, and simply return an error
      if (LUKSheader.version <= LUKS_VER_SUPPORTED) then begin
        LUKSheader.cipher_name    :=
          Copy(rawLUKSheader.cipher_name, 1, StrLen(rawLUKSheader.cipher_name));
        LUKSheader.cipher_mode    :=
          Copy(rawLUKSheader.cipher_mode, 1, StrLen(rawLUKSheader.cipher_mode));
        LUKSheader.hash_spec      :=
          Copy(rawLUKSheader.hash_spec, 1, StrLen(rawLUKSheader.hash_spec));
        LUKSheader.payload_offset := SDUBigEndian32ToDWORD(rawLUKSheader.payload_offset);
        LUKSheader.key_bytes      := SDUBigEndian32ToDWORD(rawLUKSheader.key_bytes);
        CopyMemory(@LUKSheader.mk_digest, @rawLUKSheader.mk_digest,
          sizeof(rawLUKSheader.mk_digest)
          );
        CopyMemory(@LUKSheader.mk_digest_salt, @rawLUKSheader.mk_digest_salt,
          sizeof(rawLUKSheader.mk_digest_salt)
          );
        LUKSheader.mk_digest_iter := SDUBigEndian32ToDWORD(rawLUKSheader.mk_digest_iter);
        strGUID                   := Copy(rawLUKSheader.uuid, 1, StrLen(rawLUKSheader.uuid));
        { TODO 1 -otdk -csecurity : where does this uuid come from ? garbage? }
        //todo: is this index right?
        strGUID                   := '{' + strGUID + '}';
        LUKSheader.uuid           := StringToGUID(strGUID);

        faultFlag := False;
        for i := 0 to (LUKS_NUMKEYS - 1) do begin
          keySlotActive := SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].active);

          // Sanity check
          if ((keySlotActive <> LUKS_KEY_ENABLED) and (keySlotActive <>
            LUKS_KEY_DISABLED)) then begin
            faultFlag := True;
            break;
          end;

          LUKSheader.keySlot[i].active     := (keySlotActive = LUKS_KEY_ENABLED);
          //          if (LUKSheader.keySlot[i].active) then begin
          //read even disabled slots  ones - useful for trouble shooting
          LUKSheader.keySlot[i].iterations :=
            SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].iterations);

          CopyMemory(@LUKSheader.keySlot[i].salt, @rawLUKSheader.keySlot[i].salt,
            sizeof(rawLUKSheader.keySlot[i].salt)
            );

          LUKSheader.keySlot[i].key_material_offset :=
            SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].key_material_offset);
          LUKSheader.keySlot[i].stripes             :=
            SDUBigEndian32ToDWORD(rawLUKSheader.keySlot[i].stripes);
          //          end;

        end;

        Result := not (faultFlag);
      end;

    end;

  end;  // if (length(rawData) = sizeof(rawLUKSheader)) then

end;



 // Note: This will only populate the LUKS members of the header; it will not
 //       populate the FreeOTFE driver details
function _ReadLUKSHeader(filename: String; var LUKSheader: TLUKSHeader): Boolean;
var
  { TODO 1 -otdk -csecurity : use TLUKSHeaderRaw instead of ansistring }
  rawData: Ansistring;
begin
  Result := False;

  if GetFreeOTFEBase().ReadRawVolumeData(filename, 0,
    // LUKS volumes always start from the beginning
    sizeof(TLUKSHeaderRaw), rawData) then begin
    Result := _ParseLUKSHeader(rawData, LUKSheader);
  end;

end;



(*
reads and decrypts master key using given user password, or returns false
see luks spec - actual password is stored across split data that has to be merged

process is:
  read master salt
  for each keyslot
    read keyslot salt
    hash_key(user password,keyslot.iterations,keyslot.bytes,keyslot.salt)
    read keyslot.stripes bytes from keyslot.key_material_offset and decrypt to 'split_data' using hashed key
    Merge the split data to make the 'master key'
    hash master key using master salt
    if = LUKSHeader.mk_digest
      use this slot
      break
      return master key
*)
function _ReadAndDecryptMasterKey(const filename: String;
  const userPassword: TSDUBytes;
  baseIVCypherOnHashLength: Boolean; out LUKSHeader: TLUKSHeader; out keySlot: Integer;
  out keyMaterial: TSDUBytes): Boolean;
var
  pwd_PBKDF2ed:    TSDUBytes;
  foundValid:      Boolean;
  i, j:            Integer;
  keySlotSalt:     TSDUBytes;
  masterkeySalt:   TSDUBytes;
  generatedCheck:  TSDUBytes;
  masterKey:       TSDUBytes;
  splitDataLength: Integer;
  splitData:       TSDUBytes;
  tmpVolumeFlags:  Integer;
begin
  keySlot := -1;

  // Get the LUKS header from the volume...
  Result := _ReadLUKSHeader(filename, LUKSHeader);

  // Identify the FreeOTFE options to be used...
  if Result then
    Result := _MapToFreeOTFE(baseIVCypherOnHashLength, LUKSHeader);

  // read overall salt
  if Result then begin
    SDUZeroBuffer(masterkeySalt);
    for j := low(LUKSHeader.mk_digest_salt) to high(LUKSHeader.mk_digest_salt) do
      SDUAddByte(masterkeySalt, LUKSHeader.mk_digest_salt[j]);
  end;

  if Result then begin
    // For each keyslot...
    for i := low(LUKSHeader.keySlot) to high(LUKSHeader.keySlot) do begin
      if (LUKSHeader.keySlot[i].Active) then begin
        SDUZeroBuffer(keySlotSalt);
        for j := low(LUKSHeader.keySlot[i].salt) to high(LUKSHeader.keySlot[i].salt) do
          SDUAddByte(keySlotSalt, LUKSHeader.keySlot[i].salt[j]);

        // PBKDF2 the user's password with the salt
        if not (GetFreeOTFEBase().DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, userPassword, keySlotSalt,
          LUKSHeader.keySlot[i].iterations, (LUKSHeader.key_bytes * 8),  // In *bits*
          pwd_PBKDF2ed)) then begin
          Result := False;
          break;
        end;

        DebugMsg('LUKS derived key follows:');
        DebugMsg(pwd_PBKDF2ed);
        DebugMsg(LUKSheader.key_bytes);
        DebugMsg(LUKSheader.keyslot[i].stripes);

        DebugFlush();

        splitDataLength := (LUKSheader.key_bytes * LUKSheader.keyslot[i].stripes);
        tmpVolumeFlags  := 0;

        if not (GetFreeOTFEBase().ReadWritePlaintextToVolume(True,
          // Read, NOT write

          filename, pwd_PBKDF2ed, LUKSheader.sectorIVGenMethod,
          LUKSheader.IVHashKernelModeDeviceName, LUKSheader.IVHashGUID,
          LUKSheader.IVCypherKernelModeDeviceName, LUKSheader.IVCypherGUID,
          LUKSheader.cypherKernelModeDeviceName, LUKSheader.cypherGUID,
          tmpVolumeFlags, fomaRemovableDisk, 0,
                            // Offset from within mounted volume from where to read/write data
          splitDataLength,  // Length of data to read/write. In bytes
          splitData,        // Data to read/write

          (LUKS_SECTOR_SIZE * LUKSheader.KeySlot[i].key_material_offset)
          // Offset within volume where encrypted data starts
          )) then begin
          Result := False;
          break;
        end;


        DebugMsg('LUKS decrypted split data follows:');
        DebugMsg(splitData);


        // Merge the split data
        AFMerge(SDUBytesToString(splitData), LUKSheader.keySlot[i].stripes,
          LUKSheader.hashKernelModeDeviceName, LUKSheader.hashGUID, masterKey);


        DebugMsg('LUKS AF merged data follows:');
        DebugMsg(masterKey);
        DebugFlush();

        // Generate PBKDF2 of the merged data (for msg digest)
        if not (GetFreeOTFEBase().DeriveKey(fokdfPBKDF2, LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, masterKey, masterkeySalt, LUKSHeader.mk_digest_iter,
          (sizeof(LUKSHeader.mk_digest) * 8), generatedCheck)) then begin
          Result := False;
          break;
        end;


        DebugMsg('LUKS generated pbkdf2 checksum of recovered key follows:');
        DebugMsgBinary(generatedCheck);

        DebugFlush();

        // Check if the PBKDF2 of the merged data matches that of the MK digest
        foundValid := True;
        for j := 1 to length(generatedCheck) do begin
          if (generatedCheck[j - 1] <> LUKSHeader.mk_digest[j - 1]) then begin
            foundValid := False;
            break;
          end;

        end;

        if (foundValid) then begin
          // Valid keyslot found; break out of loop
          keySlot     := i;
          keyMaterial := masterKey;
          break;
        end;

      end;

    end;

  end;
  SDUZeroBuffer(masterkeySalt);
  Result := Result and (keySlot <> -1);
  DebugFlush();
end;


function DumpLUKSDataToFile(filename: String; userKey: TSDUBytes;
  keyfile: String; keyfileIsASCII: Boolean; keyfileNewlineType: TSDUNewline;
  baseIVCypherOnHashLength: Boolean; dumpFilename: String): Boolean;
var
  LUKSHeader:       TLUKSHeader;
  i:                Integer;
  strFormattedGUID: String;
  hashTitle:        String;
  cypherTitle:      String;
  hashDetails:      TFreeOTFEHash;
  cypherDetails:    TFreeOTFECypher_v3;
  IVCypherDetails:  TFreeOTFECypher_v3;
  keySlot:          Integer;
  keyMaterial:      TSDUBytes;
  prettyPrintData:  TStringList;
  dumpReport:       TStringList;
  IVHashTitle:      String;
  IVCypherTitle:    String;
  userKeyOK:        Boolean;
begin
  GetFreeOTFEBase().CheckActive();

  dumpReport := TStringList.Create();
  try
    dumpReport.Add('LUKS Dump');
    dumpReport.Add('=========');
    dumpReport.Add('');
    dumpReport.Add('Dump Created By');
    dumpReport.Add('---------------');
    dumpReport.Add('Platform              : PC');
    dumpReport.Add('Application version   : v' + SDUGetVersionInfoString(ParamStr(0)));
    dumpReport.Add('Driver ID             : ' + GetFreeOTFEBase().VersionStr());

    // Get the LUKS header from the volume...
    if not (_ReadLUKSHeader(filename, LUKSHeader)) then begin
      if IsLUKSVolume(filename) then begin
        dumpReport.Add('ERROR: Unable to read LUKS header?!');
      end else begin
        dumpReport.Add('Unable to read LUKS header; this does not appear to be a LUKS container.');
      end;

    end else begin
      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('cryptsetup Style Dump');
      dumpReport.Add('---------------------');
      dumpReport.Add('LUKS header information for ' + filename);
      dumpReport.Add('');
      dumpReport.Add('Version:        ' + IntToStr(LUKSheader.version));
      dumpReport.Add('Cipher name:    ' + LUKSheader.cipher_name);
      dumpReport.Add('Cipher mode:    ' + LUKSheader.cipher_mode);
      dumpReport.Add('Hash spec:      ' + LUKSheader.hash_spec);
      dumpReport.Add('Payload offset: ' + IntToStr(LUKSheader.payload_offset));
      dumpReport.Add('MK bits:        ' + IntToStr(LUKSheader.key_bytes * 8));
      // Convert from bytes to bits
      dumpReport.Add('MK digest:      ' + _PrettyHex(
        @(LUKSheader.mk_digest), LUKS_DIGESTSIZE));
      dumpReport.Add('MK salt:        ' + _PrettyHex(
        @(LUKSheader.mk_digest_salt[0]), (LUKS_SALTSIZE div 2)));
      dumpReport.Add('                ' + _PrettyHex(
        @(LUKSheader.mk_digest_salt[(LUKS_SALTSIZE div 2)]), (LUKS_SALTSIZE div 2)));
      dumpReport.Add('MK iterations:  ' + IntToStr(LUKSheader.mk_digest_iter));
      strFormattedGUID := GUIDToString(LUKSheader.uuid);
      // Remove leading and trailing "{" and "}"
      Delete(strFormattedGUID, 1, 1);
      Delete(strFormattedGUID, length(strFormattedGUID), 1);
      strFormattedGUID := lowercase(strFormattedGUID);
      dumpReport.Add('UUID:           ' + strFormattedGUID);
      dumpReport.Add('');

      for i := low(LUKSheader.keySlot) to high(LUKSheader.keySlot) do begin
        if (LUKSheader.keySlot[i].active = False) then begin
          dumpReport.Add('Key Slot ' + IntToStr(i) + ': DISABLED');
        end else begin
          dumpReport.Add('Key Slot ' + IntToStr(i) + ': ENABLED');
        end;
        dumpReport.Add('        Iterations:             ' + IntToStr(
          LUKSheader.keySlot[i].iterations));
        dumpReport.Add('        Salt:                   ' + _PrettyHex(
          @(LUKSheader.keySlot[i].salt[0]), (LUKS_SALTSIZE div 2)));
        dumpReport.Add('                                ' + _PrettyHex(
          @(LUKSheader.keySlot[i].salt[(LUKS_SALTSIZE div 2)]), (LUKS_SALTSIZE div 2)));
        dumpReport.Add('        Key material offset:    ' + IntToStr(
          LUKSheader.keySlot[i].key_material_offset));
        dumpReport.Add('        AF stripes:             ' + IntToStr(
          LUKSheader.keySlot[i].stripes));
      end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Mapped FreeOTFE Drivers');
      dumpReport.Add('-----------------------');
      if not (_MapToFreeOTFE(baseIVCypherOnHashLength, LUKSheader)) then begin
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
      end else begin
        hashTitle := 'ERROR: Unable to determine hash title?!';
        if GetFreeOTFEBase().GetSpecificHashDetails(LUKSheader.hashKernelModeDeviceName,
          LUKSheader.hashGUID, hashDetails) then begin
          hashTitle := GetFreeOTFEBase().GetHashDisplayTechTitle(hashDetails);
        end;

        cypherTitle := 'ERROR: Unable to determine cypher title?!';
        if GetFreeOTFEBase().GetSpecificCypherDetails(LUKSheader.cypherKernelModeDeviceName,
          LUKSheader.cypherGUID, cypherDetails) then begin
          cypherTitle := GetFreeOTFEBase().GetCypherDisplayTechTitle(cypherDetails);
        end;

        if (LUKSheader.sectorIVGenMethod <> foivgESSIV) then begin
          IVHashTitle   := 'IV hash n/a';
          IVCypherTitle := 'IV cypher n/a';
        end else begin
          IVHashTitle := 'ERROR: Unable to determine IV hash title?!';
          if GetFreeOTFEBase().GetSpecificHashDetails(LUKSheader.IVHashKernelModeDeviceName,
            LUKSheader.IVHashGUID, hashDetails) then begin
            IVHashTitle := GetFreeOTFEBase().GetHashDisplayTechTitle(hashDetails);
          end;

          IVCypherTitle := 'ERROR: Unable to determine IV cypher title?!';
          if GetFreeOTFEBase().GetSpecificCypherDetails(LUKSheader.IVCypherKernelModeDeviceName,
            LUKSheader.IVCypherGUID, IVCypherDetails) then begin
            IVCypherTitle := GetFreeOTFEBase().GetCypherDisplayTechTitle(IVCypherDetails);
          end;
        end;

        dumpReport.Add('Hash pretty title         : ' + hashTitle);
        dumpReport.Add('Hash driver KM name       : ' + LUKSheader.hashKernelModeDeviceName);
        dumpReport.Add('Hash GUID                 : ' + GUIDToString(LUKSheader.hashGUID));
        dumpReport.Add('Cypher pretty title       : ' + cypherTitle);
        dumpReport.Add('Cypher driver KM name     : ' + LUKSheader.cypherKernelModeDeviceName);
        dumpReport.Add('Cypher GUID               : ' + GUIDToString(LUKSheader.cypherGUID));
        dumpReport.Add('Sector IV generation      : ' +
          FreeOTFESectorIVGenMethodTitle[LUKSheader.sectorIVGenMethod]);
        if (LUKSheader.sectorIVGenMethod = foivgESSIV) then begin
          dumpReport.Add('  IV hash pretty title    : ' + IVHashTitle);
          dumpReport.Add('  IV hash driver KM name  : ' + LUKSheader.IVHashKernelModeDeviceName);
          dumpReport.Add('  IV hash GUID            : ' + GUIDToString(LUKSheader.IVHashGUID));

          if baseIVCypherOnHashLength then begin
            dumpReport.Add('  IV cypher               : <based on IV hash length>');
          end else begin
            dumpReport.Add('  IV cypher               : <same as main cypher>');
          end;

          dumpReport.Add('  IV cypher pretty title  : ' + IVCypherTitle);
          dumpReport.Add('  IV cypher driver KM name: ' + LUKSheader.IVCypherKernelModeDeviceName);
          dumpReport.Add('  IV cypher GUID          : ' + GUIDToString(LUKSheader.IVCypherGUID));
        end;
      end;


      dumpReport.Add('');
      dumpReport.Add('');
      dumpReport.Add('Master Key');
      dumpReport.Add('----------');
      if (keyfile = '') then begin
        userKeyOK := True;
        dumpReport.Add('User supplied password   : ' + SDUBytesToString(userKey));
      end else begin
        dumpReport.Add('Keyfile                  : ' + keyfile);

        if keyfileIsASCII then begin
          dumpReport.Add('Treat keyfile as         : ASCII');
          dumpReport.Add('Newlines are             : ' + SDUNEWLINE_TITLE[keyfileNewlineType]);
        end else begin
          dumpReport.Add('Treat keyfile as         : Binary');
        end;

        userKeyOK := ReadLUKSKeyFromFile(keyfile, keyfileIsASCII,
          keyfileNewlineType, userKey);

        if not (userKeyOK) then begin
          dumpReport.Add('Unable to read password from keyfile.');
        end else begin
          if keyfileIsASCII then begin
            dumpReport.Add('Password from keyfile    : ' + SDUBytesToString(userKey));
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

      if userKeyOK then begin
        if not (_ReadAndDecryptMasterKey(filename, userKey, baseIVCypherOnHashLength,
          LUKSHeader, keySlot, keyMaterial)) then begin
          dumpReport.Add('No master key could be recovered with the specified password.');
        end else begin
          prettyPrintData := TStringList.Create();
          try
            dumpReport.Add('Password unlocks key slot: ' + IntToStr(keySlot));
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
    Result := True;

  finally
    dumpReport.Free();
  end;

end;



 //function TOTFEFreeOTFEBase.MountLUKS(volumeFilename: String; ReadOnly: Boolean = False;
 //  password: TSDUBytes = nil; keyfile: String = '';
 //  keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
 //  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
 //  silent: Boolean = False): DriveLetterChar;
 //var
 //  mountedAs: DriveLetterChar;
 //begin
 //  LastErrorCode := OTFE_ERR_SUCCESS;
 //  Result        := #0;
 //
 //  if MountLUKS(volumeFilename, mountedAs, ReadOnly, password, keyfile,
 //    keyfileIsASCII, keyfileNewlineType, silent) then begin
 //    Result := mountedAs;
 //  end;
 //
 //end;

function MountLUKS(volumeFilename: String;
  var mountedAs: DriveLetterChar; ReadOnly: Boolean = False; password: TSDUBytes = nil;
  keyfile: String = ''; keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
  silent: Boolean = False): Boolean;
var
  keyEntryDlg: TfrmKeyEntryLUKS;
  volumeKey:   TSDUBytes;

  userKey:          TSDUBytes;
  fileOptSize:      Int64;
  mountDriveLetter: DriveLetterChar;
  mountReadonly:    Boolean;
  currDriveLetter:  DriveLetterChar;
  mountMountAs:     TFreeOTFEMountAs;
  mr:               Integer;
  LUKSHeader:       TLUKSHeader;
  keySlot:          Integer;
  baseIVCypherOnHashLength: Boolean;
  mountForAllUsers: Boolean;
begin
  Result := True;

  GetFreeOTFEBase().CheckActive();

  keyEntryDlg := TfrmKeyEntryLUKS.Create(nil);
  try
    keyEntryDlg.Initialize();
    keyEntryDlg.SetReadonly(ReadOnly);
    keyEntryDlg.silent := silent;
    keyEntryDlg.SetKey(SDUBytesToString(password));
    keyEntryDlg.SetKeyfile(keyfile);
    keyEntryDlg.SetKeyfileIsASCII(keyfileIsASCII);
    keyEntryDlg.SetKeyfileNewlineType(keyfileNewlineType);
    keyEntryDlg.SetVolumeFilename(volumeFilename);

    mr := keyEntryDlg.ShowModal();

    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then begin
      Result := False;
    end else
    if (mr = mrOk) then begin

      if (
        // Key...
        (keyEntryDlg.GetKey(userKey)) and
        // Mount options...
        (keyEntryDlg.GetMountAs(mountMountAs))) then begin
        // Retrieve all data from the mount dialog...
        baseIVCypherOnHashLength := keyEntryDlg.GetIVCypherBase();
        // File options...
        fileOptSize              := keyEntryDlg.GetSizeLimit();
        mountDriveLetter         := keyEntryDlg.GetDriveLetter();
        mountReadonly            := keyEntryDlg.GetReadonly();
        mountForAllUsers         := keyEntryDlg.GetMountForAllUsers();

        currDriveLetter := GetFreeOTFEBase().GetNextDriveLetter(mountDriveLetter, #0);
        if (currDriveLetter = #0) then begin
          // No more drive letters following the user's specified drive
          // letter - don't mount further drives
          Result := False;
          // Bail out...
          exit;
        end;

        if not (_ReadAndDecryptMasterKey(volumeFilename, userKey,
          baseIVCypherOnHashLength, LUKSHeader, keySlot, volumeKey)) then begin
          Result := False;
          // Bail out...
          exit;
        end;
{$IFDEF FREEOTFE_DEBUG}
  DebugMsg('Recovered key OK');
  DebugMsg('key slot '+inttostr(keySlot)+' unlocked.');
  DebugMsg('Master key: '+_PrettyHex(
                                    PChar(volumeKey),
                                    length(volumeKey)
                                   ));
{$ENDIF}

        if GetFreeOTFEBase().CreateMountDiskDevice(volumeFilename, volumeKey,
          LUKSHeader.sectorIVGenMethod, nil,
          // Linux volumes don't have per-volume IVs
          mountReadonly, LUKSHeader.IVHashKernelModeDeviceName,
          LUKSHeader.IVHashGUID, LUKSHeader.IVCypherKernelModeDeviceName,
          LUKSHeader.IVCypherGUID, LUKSHeader.cypherKernelModeDeviceName,
          LUKSHeader.cypherGUID, 0,
          // Volume flags
          currDriveLetter, (LUKSHeader.payload_offset * LUKS_SECTOR_SIZE),
          fileOptSize, True,  // Linux volume
          PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
          mountMountAs, mountForAllUsers) then begin
          mountedAs := currDriveLetter;
        end else begin
          mountedAs := #0;
          Result    := False;
        end;
      end;
    end;
  finally
    keyEntryDlg.Free()
  end;

  // Unmounted volume files...
  // Yes, it is "+1"; if you have only 1 volume file, you want exactly one #0
  //  for i := (length(mountedAs) + 1) to (volumeFilenames.Count) do begin
  //    mountedAs := mountedAs + #0;
  //    // This should not be needed; included to ensure sanity...
  //    Result    := False;
  //  end;
end;



 // ----------------------------------------------------------------------------
 // Determine if the specified volume is a Linux LUKS volume
function IsLUKSVolume(volumeFilename: String): Boolean;
var
  rawData: Ansistring;
begin
  Result := False;

  // Read in what should be the volume's signature
  if GetFreeOTFEBase().ReadRawVolumeData(volumeFilename, 0,
    // LUKS volumes always start from the beginning
    length(LUKS_MAGIC), rawData) then
    // Compare with LUKS volume signature
    Result := (rawData = LUKS_MAGIC);
end;



 // ----------------------------------------------------------------------------
 // Read a LUKS key in from a file
function ReadLUKSKeyFromFile(filename: String; treatAsASCII: Boolean;
  ASCIINewline: TSDUNewline; out key: TSDUBYtes): Boolean;
var
  newlinePos: Integer;
  strKey:     Ansistring;
begin
  Result := SDUGetFileContent(filename, strKey);
  key    := SDUStringToSDUBytes(strKey);
  if Result then begin
    // If the input file was non-binary, only read in to the first newline
    if treatAsASCII then begin
      newlinePos := Pos(SDUNEWLINE_STRING[ASCIINewline], strKey);
      if (newlinePos > 0) then begin
        // -1 to actually *exclude* the newline
        strKey := Copy(strKey, 1, (newlinePos - 1));
        key    := SDUStringToSDUBytes(strKey);
      end;
    end;
  end;
  SDUZeroString(strKey);
end;

end.
