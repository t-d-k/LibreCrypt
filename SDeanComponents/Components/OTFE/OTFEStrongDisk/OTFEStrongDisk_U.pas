unit OTFEStrongDisk_U;
// Description: Delphi StrongDisk Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// TRUE if integer<>0
// FALSE if integer=0




interface

uses
  Classes, SysUtils, Windows, forms, controls,
  OTFE_U,
  OTFEStrongDiskConsts_U;

type
  TOTFEStrongDisk = class(TOTFE)
  private
    hStrongDiskSupportDLLCrypto: THandle;
    hStrongDiskSupportDLLLang: THandle;
    hStrongDiskDLL: THandle;
    fDefaultDrive: char;

    function  GetNextUnusedDrvLtr(afterDrv: char): char;
    function  GetUnusedDriveLetters(): string;
  protected
    StrongDiskDriverVersion: cardinal;

    function  Connect(): boolean;
    function  Disconnect(): boolean;
    procedure SetActive(AValue : Boolean); override;
    function  GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;

    function  GetInstallDir(): string;

    // DLL FUNCTIONS
    // SDMount(): mount a secure drive
    function CallSDMount(driveLetter: char;
                         filename: string;
                         readonly: boolean;
                         autolaunch: boolean;
                         keymode: integer;
                         keyFilename: string;
                         password: string): boolean;

    // SDDismount(): unmount a secure drive
    function CallSDDismount(driveLetter: char;
                            force: boolean): boolean;

    // SDDismountAll(): unmount a secure drive
    function CallSDDismountAll(force: boolean): boolean;

    // SDGetDrives(): get bitmask representing the secure drives;
    function CallSDGetDrives(var drives: string): boolean;

    // SDGetInfo(): get information about the secure drive;
    function CallSDGetInfo(driveLetter: char;
                           var filename: string;
                           var readonly: boolean): boolean;


  function TranslateDLLError(DLLerror: DWORD): integer;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    // TOTFE functions...
    function  Title(): string; overload; override;
    function  Mount(volumeFilename: string; readonly: boolean = FALSE): char; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean; override;
    function  MountDevices(): string; override;
    function  CanMountDevice(): boolean; override;
    function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean; overload; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  DrivesMounted(): string; override;
    function  GetVolFileForDrive(driveLetter: char): string; override;
    function  GetDriveForVolFile(volumeFilename: string): char; override;
    function  Version(): cardinal; override;
    function  VersionStr(): string; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  GetMainExe(): string; override;

  published
    property  DefaultDrive: char read fDefaultDrive write fDefaultDrive;
  end;

procedure Register;


implementation

uses ShellAPI,
     dialogs,
     Registry, RegStr,
     INIFiles,
     Math,
     OTFEConsts_U,
     OTFEStrongDiskPasswordEntry_U,
     SDUGeneral;

type
  // Function declarations for DLL functions

  //STRDISKAPI unsigned SDMount(char driveLetter, const char* fileName,
  // char readOnly, char autolaunch, unsigned keyMode,
  // const char* keyFileName, const char* password, unsigned passwordSize);
  TSDMountFunc = function(driveLetter: char;
                          filename: PChar;
                          readOnly: byte;
                          autolaunch: byte;
                          keyMode: DWORD;
                          keyFileName: PChar;
                          password: PChar;
                          passwordSize: DWORD): DWORD; stdcall;

  //STRDISKAPI unsigned SDDismount(char driveLetter, char force);
  TSDDismountFunc = function(driveLetter: char; force: byte): DWORD; stdcall;

  //STRDISKAPI unsigned SDDismountAll(char force);
  TSDDismountAllFunc = function(force: byte): DWORD; stdcall;

  //STRDISKAPI unsigned SDGetDrives(unsigned* drives);
  TSDGetDrivesFunc = function(drives: PDWORD): DWORD; stdcall;

  //STRDISKAPI unsigned SDGetInfo(char driveLetter,
  // char* fileName, unsigned* size, char* readOnly);
  TSDGetInfoFunc = function(driveLetter: char;
                        fileName: PChar;
                        size: PDWORD;
                        readonly: PByte): DWORD; stdcall;



procedure Register;
begin
  RegisterComponents('OTFE', [TOTFEStrongDisk]);
end;



constructor TOTFEStrongDisk.Create(AOwner : TComponent);
begin
  inherited create(AOwner);

  FActive := False;
  hStrongDiskDLL := 0;
  StrongDiskDriverVersion := $FFFFFFFF;

end;


destructor TOTFEStrongDisk.Destroy;
begin
  if FActive then
    begin
    Active := FALSE;
    end;

  inherited Destroy;
end;


function TOTFEStrongDisk.Connect(): boolean;
var
  loadFilename: string;
begin
  Result := Active;
  if not(Active) then
    begin
    // The STRONGDISK_MAIN_DLL requires STRONGDISK_SUPPORT_DLL_CRYPTO
    // Because STRONGDISK_MAIN_DLL can't automatically load STRONGDISK_SUPPORT_DLL_CRYPTO
    // by itsself, unless *this* *executable* is in the same directory, or
    // STRONGDISK_SUPPORT_DLL_CRYPTO is in the system's PATH environment - we load it
    // here
    // Note: We try on the path first, *then* in the StrongDisk install dir
    //       THIS IS THE REVERSE OF THE OTHER TWO DLLS - StrongDisk doesn't
    //       appear to install STRONGDISK_SUPPORT_DLL_CRYPTO in it's dir
    loadFilename := STRONGDISK_SUPPORT_DLL_CRYPTO;
    hStrongDiskSupportDLLCrypto := LoadLibrary(PChar(loadFilename));
    if hStrongDiskSupportDLLCrypto=0 then
      begin
      // DLL couldn't be loaded; maybe it isn't on the path? Try again, looking
      // in the StrongDisk directory
      loadFilename := ExtractFilePath(GetMainExe()) + STRONGDISK_SUPPORT_DLL_CRYPTO;
      hStrongDiskSupportDLLCrypto := LoadLibrary(PChar(loadFilename));
      if hStrongDiskSupportDLLCrypto=0 then
        begin
        FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
        raise EStrongDiskDLLNotFound.Create(E_STRONGDISK_DLL_NOT_FOUND+
                                            ' ('+
                                            STRONGDISK_SUPPORT_DLL_CRYPTO+
                                            ')');
        end;
      end;


    // THIS IS NEW - SEEMS TO HAVE BEEN ADDED TO StrongDisk v3
    // The STRONGDISK_MAIN_DLL requires STRONGDISK_SUPPORT_DLL_LANG
    // Because STRONGDISK_MAIN_DLL can't automatically load STRONGDISK_SUPPORT_DLL_LANG
    // by itsself, unless *this* *executable* is in the same directory, or
    // STRONGDISK_SUPPORT_DLL_LANG is in the system's PATH environment - we load it
    // here
    // STRONGDISK_SUPPORT_DLL_LANG *should* be in the StrongDisk install dir;
    // so we try there first
    loadFilename := ExtractFilePath(GetMainExe()) + STRONGDISK_SUPPORT_DLL_LANG;
    hStrongDiskSupportDLLCrypto := LoadLibrary(PChar(loadFilename));
    if hStrongDiskSupportDLLCrypto=0 then
      begin
      // DLL couldn't be loaded; fallback to any copy which may be on the path
      loadFilename := STRONGDISK_SUPPORT_DLL_LANG;
      hStrongDiskSupportDLLCrypto := LoadLibrary(PChar(loadFilename));
      if hStrongDiskSupportDLLCrypto=0 then
        begin
        FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;
        raise EStrongDiskDLLNotFound.Create(E_STRONGDISK_DLL_NOT_FOUND+
                                            ' ('+
                                            STRONGDISK_SUPPORT_DLL_LANG+
                                            ')');
        end;
      end;

      

    // Load the main StrongDisk DLL
    // STRONGDISK_MAIN_DLL *should* be in the StrongDisk install dir;
    // so we try there first
    loadFilename := ExtractFilePath(GetMainExe()) + STRONGDISK_MAIN_DLL;
    hStrongDiskDLL := LoadLibrary(PChar(loadFilename));
    if hStrongDiskDLL=0 then
      begin
      // DLL couldn't be loaded; fallback to any copy which may be on the path
      loadFilename := STRONGDISK_MAIN_DLL;
      hStrongDiskDLL := LoadLibrary(PChar(loadFilename));
      if hStrongDiskDLL=0 then
        begin
        FLastErrCode:= OTFE_ERR_DRIVER_FAILURE;

        // Free off the no-longer needed support DLL
        FreeLibrary(hStrongDiskDLL);

        raise EStrongDiskDLLNotFound.Create(E_STRONGDISK_DLL_NOT_FOUND+
                                            ' ('+
                                            STRONGDISK_MAIN_DLL+
                                            ')');
        end;
      end;


    Result := TRUE;
    end;

end;

function TOTFEStrongDisk.Disconnect(): boolean;
begin
  if Active then
    begin
    if (hStrongDiskDLL<>0) then
      begin
      FreeLibrary(hStrongDiskDLL);
      hStrongDiskDLL := 0;
      end;

    if (hStrongDiskSupportDLLCrypto<>0) then
      begin
      FreeLibrary(hStrongDiskSupportDLLCrypto);
      hStrongDiskSupportDLLCrypto := 0;
      end;

    if (hStrongDiskSupportDLLLang<>0) then
      begin
      FreeLibrary(hStrongDiskSupportDLLLang);
      hStrongDiskSupportDLLLang := 0;
      end;

    end;
    
  Result := TRUE;

end;


procedure TOTFEStrongDisk.SetActive(AValue : Boolean);
var
  allOK: boolean;
begin
  allOK := FALSE;
  if AValue <> Active then
    begin
      if AValue then
        begin
        allOK := Connect();
        end
      else
        begin
        allOK := Disconnect();
        end;
    end;

  if allOK then
    begin
    inherited;
    if Active then
      begin
      StrongDiskDriverVersion := Version();
      end;
    end;

end;

// Note that the emergency parameter is IGNORED
function TOTFEStrongDisk.Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
begin
  CheckActive();

  Result := FALSE;

  if pos(driveLetter, DrivesMounted())<1 then
    begin
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    exit;
    end;

  Result := CallSDDismount(upcase(driveLetter), emergency);

end;

function TOTFEStrongDisk.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
begin
  // Don't need to convert volumeFilename to SFN/LFN, this is done in
  // GetDriveForVolFile(...)
  Result := Dismount(GetDriveForVolFile(volumeFilename), emergency);

end;


function TOTFEStrongDisk.Mount(volumeFilename: string; readonly: boolean = FALSE): char;
var
  stlVolumes: TStringList;
  mountedAs: string;
begin
  CheckActive();

  Result := #0;

  stlVolumes:= TStringList.Create();
  try
    stlVolumes.Add(volumeFilename);
    if Mount(stlVolumes, mountedAs, readonly) then
      begin
      Result := mountedAs[1];
      end;
  finally
    stlVolumes.Free();
  end;

end;


function TOTFEStrongDisk.Mount(volumeFilenames: TStringList; var mountedAs: string; readonly: boolean = FALSE): boolean;
var
  driveLetter: char;
  thePassword: string;
  theKeyFilename: string;
  useHardwareKey: boolean;
  oneOK: boolean;
  drvLtrsFree: string;
  passwordDlg: TOTFEStrongDiskPasswordEntry_F;
  autolaunch: boolean;
  volumeLoop: integer;
  currVolFilename: string;
  mountedDrvLetter: char;
  currAllOK: boolean;
  keymode: integer;
begin

  CheckActive();
  Result := FALSE;

  oneOK := FALSE;
  mountedAs := '';

  passwordDlg:= TOTFEStrongDiskPasswordEntry_F.Create(nil);
  try
    // The user should be prompted for the drive letter, and offered the
    // default
    drvLtrsFree := GetUnusedDriveLetters();
    driveLetter := DefaultDrive;

    if driveLetter=#0 then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      // If we're on A or B, skip this drive
      if (driveLetter='A') or (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      // If we're on B, skip this drive
      if (driveLetter='B') then
        begin
        driveLetter := GetNextUnusedDrvLtr(driveLetter);
        end;
      end
    else if pos(driveLetter, drvLtrsFree)<1 then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      end;

    passwordDlg.DrivesAllowed := drvLtrsFree;
    passwordDlg.Drive := driveLetter;

    if passwordDlg.ShowModal()=mrCancel then
      begin
      passwordDlg.ClearEnteredPassword();
      // Result already = FALSE, so just set the error and exit
      FLastErrCode := OTFE_ERR_USER_CANCEL;
      exit;
      end;

    // Retrieve password information from password entry dialog
    thePassword := passwordDlg.mePassword.text;
    theKeyFilename:= passwordDlg.edKeyfile.text;
    useHardwareKey:= passwordDlg.ckElectronicKey.checked;
    driveLetter := passwordDlg.Drive;
    autolaunch := passwordDlg.ckAutostart.checked;

    passwordDlg.ClearEnteredPassword();
  finally
    passwordDlg.Free();
  end;


  if driveLetter=#0 then
    begin
    // Result already = FALSE, so just set the error and exit
    FLastErrCode := OTFE_ERR_INVALID_DRIVE;
    Result := FALSE;
    exit;
    end;

  DefaultDrive := driveLetter;


  // Work out which keys are to be used in mounting the volume
  keymode := 0;
  if thePassword<>'' then
    begin
    keymode := keymode OR SDKM_PASSWORD;
    end;
  if theKeyFilename<>'' then
    begin
    keymode := keymode OR SDKM_KEYFILE;
    end;
  if useHardwareKey then
    begin
    keymode := keymode OR SDKM_ELKEY;
    end;

  for volumeLoop:=0 to (volumeFilenames.count-1) do
    begin
    mountedDrvLetter := #0;

    currVolFilename := volumeFilenames[volumeLoop];

    if IsEncryptedVolFile(currVolFilename) then
      begin
      currAllOK := CallSDMount(driveLetter,
                               currVolFilename,
                               readonly,
                               autolaunch,
                               keymode,
                               theKeyFilename,
                               thePassword);

      if not(currAllOK) then
        begin
        FLastErrCode:= OTFE_ERR_WRONG_PASSWORD;
        end;

      if currAllOK then
        begin
        mountedDrvLetter := driveLetter;
        oneOK := TRUE;
        end;

      end
    else
      begin
      FLastErrCode:= OTFE_ERR_FILE_NOT_ENCRYPTED_VOLUME;
      end;


    if volumeLoop<(volumeFilenames.count-1) then
      begin
      driveLetter := GetNextUnusedDrvLtr(driveLetter);
      if driveLetter=#0 then
        begin
        FLastErrCode := OTFE_ERR_INVALID_DRIVE;
        break;
        end;
      end;

    mountedAs := mountedAs + mountedDrvLetter;
    end;

  // Pad out the string with #0, if needed
  while length(mountedAs)<volumeFilenames.count do
    begin
    mountedAs := mountedAs + #0;
    end;

  Result := oneOK;

end;


function TOTFEStrongDisk.DrivesMounted(): string;
var
  output: string;
begin
  GetDisksMounted(output, nil);
  Result := SortString(output);

end;


function TOTFEStrongDisk.GetDisksMounted(var mountedDrives: string; volumeFilenames: TStrings): boolean;
var
  i: integer;
  currVolFilename: string;
  retVal: boolean;
begin
  CheckActive();

  retVal := CallSDGetDrives(mountedDrives);

  if volumeFilenames<>nil then
    begin
    volumeFilenames.Clear;
    for i:=1 to length(mountedDrives) do
      begin
      currVolFilename := GetVolFileForDrive(mountedDrives[i]);
      retVal := retVal AND (currVolFilename<>'');
      volumeFilenames.Add(currVolFilename);
      end;

    end;

  Result := retVal;

end;


// !! WARNING !!
// Under NT, the StrongDisk driver won't tell us the full filename if it's more than
// 64 chars long, it will only return the first 60 followed by "..."
function TOTFEStrongDisk.GetVolFileForDrive(driveLetter: char): string;
var
  readonly: boolean;
  filename: string;
begin
  Result := '';

  driveLetter := upcase(driveLetter);

  if CallSDGetInfo(driveLetter, filename, readonly) then
    begin
    Result := filename;
    end;
    
end;


// !! WARNING !!
// If running under NT, then if the lanegth of "volumeFilename" is >64, this
// function may FAIL as the StrongDisk driver only gives us the first 60 chars
// followed by "..." to work with
function TOTFEStrongDisk.GetDriveForVolFile(volumeFilename: string): char;
var
  mountedFilenames: TStringList;
  mountedDrives: string;
begin
  Result := #0;

  mountedFilenames:= TStringList.Create();
  try
    GetDisksMounted(mountedDrives, mountedFilenames);

    if mountedFilenames.IndexOf(volumeFilename)>-1 then
      begin
      Result := mountedDrives[mountedFilenames.IndexOf(volumeFilename)+1];
      end;

  finally
    mountedFilenames.Free();
  end;
  
end;





function TOTFEStrongDisk.IsEncryptedVolFile(volumeFilename: string): boolean;
begin
  CheckActive();

  Result := TRUE;

end;


function TOTFEStrongDisk.Title(): string;
begin
  Result := 'StrongDisk';
end;


function TOTFEStrongDisk.Version(): cardinal;
const
  VERSIONINFO_PRODUCT_VERSION = 'ProductVersion';
var
  majorVersion, minorVersion, revisionVersion, buildVersion: integer;
  exeFilename: string;
  vsize: Integer;
  dwHandle: DWORD;
  pBlock: Pointer;

  aLocale : array[0..3] of Byte;
  versionStringName: string;
  pLocale : Pointer;
  iLenOfValue: DWORD;
  versionInformationValue: PChar;
  langCharset: string;
  versionPart: string;
  tmpStr: string;
  tmpByte: byte;
  tmpDriverVersion: cardinal;
  allOK: boolean;
begin
  CheckActive();

  Result := $FFFFFFFF;
  // OK - here's the plan! :)

  // Because the StrongDisk DLL doesn't appear to have any version information
  // (yuck!), we have to get it from the main StrongDisk executable
  // You'd expect the get this information from the executable in a nice'n'easy
  // way, just be using SDUGetVersionInfo, however for some reason PhysTechSoft
  // didn't store this information in the right place (select the executable
  // in Windows Explorer and press <ALT+ENTER> to get the file properties; goto
  // the "Version" tab, and you don't get version information right at the top
  // of the property sheet. Pity.
  // Nevermind - they do store this version information (check the "Other
  // version information" groupbox on this property sheet), we'll just have to
  // snag that instead!

  if StrongDiskDriverVersion=$FFFFFFFF then
    begin
    exeFilename:= GetMainExe();
    // Attempt to get the executable's version information the easy way
    if SDUGetVersionInfo(exeFilename, majorVersion, minorVersion, revisionVersion, buildVersion) then
      begin
      if (majorVersion<>0) OR
         (minorVersion<>0) OR
         (revisionVersion<>0) OR
         (buildVersion<>0) then
        begin
        // We've got useful version information; use it
        StrongDiskDriverVersion := (majorVersion    * $01000000) +
                                   (minorVersion    * $00010000) +
                                   (revisionVersion * $00000100) +
                                   (buildVersion    * $00000001);
        end;
      end;

  if (StrongDiskDriverVersion=$FFFFFFFF) then
    begin
    // No luck? Oh well, just have to do things the hard(er) way...
    
    vsize := GetFileVersionInfoSize(PChar(exeFilename), dwHandle);
    if vsize = 0 then
      begin
      exit;
      end;

    GetMem(pBlock,vsize);
    try
      if GetFileVersionInfo(PChar(exeFilename), dwHandle, vsize, pBlock) then
        begin
        // Determine the lang-charset to use (if possible)
        // For example:
        //   040904E4 = US English, Windows MultiLingual characterset
        //   Breaking that down:
        //   04......        = SUBLANG_ENGLISH_USA
        //   ..09....        = LANG_ENGLISH
        //   ....04E4 = 1252 = Codepage for Windows:Multilingual
        langCharset := '';
        if VerQueryValue(pBlock,
                         PChar('\VarFileInfo\Translation\'),
                         pLocale,
                         iLenOfValue) then
          begin
          if (iLenOfValue=4) then
            begin
            // Move locale to array
            Move(pLocale^, aLocale, 4);
            // Convert into a string representation
            langCharset:= IntToHex(aLocale[1], 2) +
                          IntToHex(aLocale[0], 2) +
                          IntToHex(aLocale[3], 2) +
                          IntToHex(aLocale[2], 2);
            end;

          end;

        // If that failed, we'll try the local system default lang
        if langCharset='' then
          begin
          langCharset := Format('%.4x', [GetUserDefaultLangID()]) + '04E4';
          end;

        versionStringName:= '\StringFileInfo\' + langCharset +'\' + VERSIONINFO_PRODUCT_VERSION + #0;
        if VerQueryValue(pBlock,
                         @versionStringName[1],
                         Pointer(versionInformationValue),
                         iLenOfValue) then
          begin
          tmpDriverVersion := 0;
          tmpStr := versionInformationValue;

          allOK := SDUSplitString(tmpStr, versionPart, tmpStr, '.');
          allOK := allOK AND (versionPart<>'');
          if allOK then
            begin
            // We transfer AND it with $FF to limit this part of the version
            // number to something a single bytes; that's all we have to
            // represent it in StrongDiskDriverVersion
            tmpByte := (strtoint(versionPart) AND $FF);
            tmpDriverVersion := tmpDriverVersion + (tmpByte * $01000000);
            end;

          allOK := SDUSplitString(tmpStr, versionPart, tmpStr, '.');
          allOK := allOK AND (versionPart<>'');
          if allOK then
            begin
            // We transfer AND it with $FF to limit this part of the version
            // number to something a single bytes; that's all we have to
            // represent it in StrongDiskDriverVersion
            tmpByte := (strtoint(versionPart) AND $FF);
            tmpDriverVersion := tmpDriverVersion + (tmpByte * $00010000);
            end;

          // We don't set allOK on the final call to SDUSplitString, as we
          // won't find a final "."
          // We only use the first 4 numbers from the version string, though
          // we could easily extend this to grab all 4 (if possible)
          // Because the version string *could*, but might *not* have a 4th
          // (build) number, we still do the SDUSplitString, but we ignore it's
          // result
          SDUSplitString(tmpStr, versionPart, tmpStr, '.');
          allOK := (versionPart<>'');
          if allOK then
            begin
            // We transfer AND it with $FF to limit this part of the version
            // number to something a single bytes; that's all we have to
            // represent it in StrongDiskDriverVersion
            tmpByte := (strtoint(versionPart) AND $FF);
            tmpDriverVersion := tmpDriverVersion + (tmpByte * $00000100);
            end;

          if allOK then
            begin
            StrongDiskDriverVersion := tmpDriverVersion;
            end;

          end;

        end; // GetFileVersionInfo(...) successful

    finally
      FreeMem(pBlock);
    end;

      end;

    end;

  Result := StrongDiskDriverVersion;

end;


function TOTFEStrongDisk.VersionStr(): string;
var
  verNo: cardinal;
  majorVer : integer;
  minorVer1: integer;
  minorVer2: integer;
begin
  verNo:= Version();
  majorVer  := (verNo AND $FF000000) div $FF;
  minorVer1 := (verNo AND $FF0000) div $FF;
  minorVer2 := (verNo AND $FF00);

  Result := Format('v%d.%d.%d', [majorVer, minorVer1, minorVer2]);

end;



function TOTFEStrongDisk.GetNextUnusedDrvLtr(afterDrv: char): char;
var
  finished: boolean;
  unusedDrvs: string;
begin
  Result := #0;
  finished := FALSE;

  unusedDrvs := GetUnusedDriveLetters();
  while not(finished) do
    begin
    afterDrv := chr(ord(afterDrv)+1);
    if ord(afterDrv)>ord('Z') then
      begin
      finished := TRUE;
      end
    else
      begin
      if pos(afterDrv, unusedDrvs)>0 then
        begin
        Result := afterDrv;
        finished := TRUE;
        end;
      end;

    end;

end;

// Returns a string containing the drive letters of "unallocated" drives
function TOTFEStrongDisk.GetUnusedDriveLetters(): string;
var
  driveLetters: string;
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  driveLetters := '';

  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
    begin
    if not(DriveNum in DriveBits) then
      begin
      driveLetters := driveLetters + Char(DriveNum + Ord('A'));
      end;
    end;

  Result := driveLetters;

end;


// Get the full path & filename to the executable
function TOTFEStrongDisk.GetMainExe(): string;
var
  dirRead: string;
begin
  Result := '';
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  dirRead := GetInstallDir();
  if (dirRead<>'') then
    begin
    Result := dirRead + '\' + STRONGDISK_EXE;
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

end;



// Get the installation dir from the registry
function TOTFEStrongDisk.GetInstallDir(): string;
var
  registry: TRegistry;
  keyName: string;
  dirRead: string;
begin
  Result := '';
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;

  registry := TRegistry.create();
  try
    registry.RootKey := HKEY_LOCAL_MACHINE;
    keyName := STRONGDISK_REGISTRY_ENTRY_PATH;
    if registry.OpenKeyReadOnly(keyName) then
      begin
      dirRead := registry.ReadString('');
      if DirectoryExists(dirRead) then
        begin
        Result := dirRead;
        end;
      registry.CloseKey;
      end;
  finally
    registry.Free();
  end;

  if Result<>'' then
    begin
    FLastErrCode:= OTFE_ERR_SUCCESS;
    end;

end;



// =============================================================================
// DLL FUNCTIONS ===============================================================

// From the API details:
// SDMount(): mount a secure drive
// return value:
//   an error code;
// arguments:
//   driveLetter - the letter of the mounted drive ('A'-'Z','a'-'z');
//   fileName - ASCIIZ file name of the image to be mounted;
//   readOnly - drive acess mode (nonzero-read only, zero-read/write);
//   autolaunch - run programs from 'Startup' folder after mounting
//                (nonzero-run the programs, zero-don't run the programs);
//   keyMode - a combination of SDKM_... constants;
//   keyFileName - ASCIIZ key file name (if SDKM_KEYFILE is used);
//   password - password with any characters (if SDKM_PASSWORD is used);
//   passwordSize - size of data pointed to by the 'password' argument
//                 (if password is zero terminated passwordSize must include the
//                 terminating zero for compatibility with the StrongDisk interface
//STRDISKAPI unsigned SDMount(char driveLetter, const char* fileName,
// char readOnly, char autolaunch, unsigned keyMode,
// const char* keyFileName, const char* password, unsigned passwordSize);
function TOTFEStrongDisk.CallSDMount(driveLetter: char;
                                     filename: string;
                                     readonly: boolean;
                                     autolaunch: boolean;
                                     keymode: integer;
                                     keyFilename: string;
                                     password: string): boolean;
var
  SDFunc: TSDMountFunc;
  funcResult: DWORD;
  readonlyByte: byte;
  autolaunchByte: byte;
  ptrKeyFilename: PChar;
  ptrPassword: PChar;
  lenPassword: DWORD;
begin
  Result := FALSE;

  driveLetter := upcase(driveLetter);

  // Call the DLL to mount the volume
  @SDFunc := GetProcAddress(hStrongDiskDLL, ''+DLL_FUNCTIONNAME_SDMOUNT+'');
  if @SDFunc=nil then
    begin
    raise EStrongDiskBadDLL.Create(E_STRONGDISK_BAD_DLL+' ('+DLL_FUNCTIONNAME_SDMOUNT+')');
    end;

  readonlyByte:= 0;
  if readonly then
    begin
    readonlyByte := 1;
    end;

  autolaunchByte:= 0;
  if autolaunch then
    begin
    autolaunchByte := 1;
    end;

  ptrKeyFilename := nil;
  if ((keymode AND SDKM_KEYFILE)>0) then
    begin
    ptrKeyFilename := PChar(keyFilename);
    end;

  ptrPassword := nil;
  lenPassword := 0;
  if ((keymode AND SDKM_PASSWORD)>0) then
    begin
    ptrPassword := PChar(password);
    lenPassword := length(password);
    end;

  funcResult := SDFunc(driveLetter,
                       PChar(filename),
                       readonlyByte,
                       autolaunchByte,
                       keyMode,
                       ptrKeyFilename,
                       ptrPassword,
                       lenPassword);
  if funcResult<>SDE_OK then
    begin
    // Set the LastErrorCode to something appropriate
    LastErrorCode := TranslateDLLError(funcResult);
    end
  else
    begin
    Result := TRUE;
    end;

end;


// From the API details:
// SDDismount(): unmount a secure drive
// return value : an error code;
// arguments :
//  driveLetter - the letter of a secure drive to be dismounted
//   ('A'-'Z','a'-'z');
//  force - check for open files or not (nonzero-don't check, zero-check) */
//STRDISKAPI unsigned SDDismount(char driveLetter, char force);
function TOTFEStrongDisk.CallSDDismount(driveLetter: char;
                                        force: boolean): boolean;
var
  SDFunc: TSDDismountFunc;
  funcResult: DWORD;
  forceByte: byte;
begin
  Result := FALSE;

  driveLetter:= upcase(driveLetter);

  // Call the DLL to mount the volume
  @SDFunc := GetProcAddress(hStrongDiskDLL, ''+DLL_FUNCTIONNAME_SDDISMOUNT+'');
  if @SDFunc=nil then
    begin
    raise EStrongDiskBadDLL.Create(E_STRONGDISK_BAD_DLL+' ('+DLL_FUNCTIONNAME_SDDISMOUNT+')');
    end;

  forceByte:= 0;
  if force then
    begin
    forceByte := 1;
    end;

  funcResult := SDFunc(driveLetter,
                       forceByte);
  if funcResult<>SDE_OK then
    begin
    // Set the LastErrorCode to something appropriate
    LastErrorCode := TranslateDLLError(funcResult);
    end
  else
    begin
    Result := TRUE;
    end;

end;


// From the API details:
// SDDismountAll(): unmount all secure drives;
// return value : an error code;
// arguments :
//  force - check for open files or not (nonzero-don't check, zero-check) */
//STRDISKAPI unsigned SDDismountAll(char force);
function TOTFEStrongDisk.CallSDDismountAll(force: boolean): boolean;
var
  SDFunc: TSDDismountAllFunc;
  funcResult: DWORD;
  forceByte: byte;
begin
  Result := FALSE;

  // Call the DLL to mount the volume
  @SDFunc := GetProcAddress(hStrongDiskDLL, ''+DLL_FUNCTIONNAME_SDDISMOUNTALL+'');
  if @SDFunc=nil then
    begin
    raise EStrongDiskBadDLL.Create(E_STRONGDISK_BAD_DLL+' ('+DLL_FUNCTIONNAME_SDDISMOUNTALL+')');
    end;

  forceByte:= 0;
  if force then
    begin
    forceByte := 1;
    end;

  funcResult := SDFunc(forceByte);
  if funcResult<>SDE_OK then
    begin
    // Set the LastErrorCode to something appropriate
    LastErrorCode := TranslateDLLError(funcResult);
    end
  else
    begin
    Result := TRUE;
    end;

end;


// SDGetDrives(): get bitmask representing the secure drives;
// return value : an error code;
// arguments :
//  drives - pointer to variable that will receive the bitmask
//  (bit 0 (the least-significant)-'A', bit 1-'B', ...) */
//STRDISKAPI unsigned SDGetDrives(unsigned* drives);
function TOTFEStrongDisk.CallSDGetDrives(var drives: string): boolean;
var
  i: integer;
  currBit: cardinal;
  drivesBitmask: DWORD;
  SDFunc: TSDGetDrivesFunc;
  funcResult: DWORD;
begin
  // Call the DLL to set "drivesBitmask"
  @SDFunc := GetProcAddress(hStrongDiskDLL, ''+DLL_FUNCTIONNAME_SDGETDRIVES+'');
  if @SDFunc=nil then
    begin
    raise EStrongDiskBadDLL.Create(E_STRONGDISK_BAD_DLL+' ('+DLL_FUNCTIONNAME_SDGETDRIVES+')');
    end;

  funcResult := SDFunc(@drivesBitmask);
  if funcResult<>SDE_OK then
    begin
    raise EStrongDiskDLLFailure.Create(E_STRONGDISK_DLL_FAILURE+' ('+DLL_FUNCTIONNAME_SDGETDRIVES+')');
    end;

  drives := '';
  currBit := 1;
  for i:=1 to 26 do
    begin
    if ((drivesBitmask AND currBit)>0) then
      begin
      drives := drives + chr(ord('A')+i-1);
      end;
    currBit := currBit * 2;
    end;

  Result := TRUE;

end;


// From the API details:
// SDGetInfo(): get information about the secure drive;
// return value : an error code;
// arguments :
//  driveLetter - the letter of a secure drive of interest ('A'-'Z','a'-'z');
//  fileName - buffer that will receive the drive image file name;
//  size - the size of the 'fileName' buffer; on return it will contain
//   the size of the file name (including terminating '\0'); you can set
//   *size to 0 to obtain the required buffer size;
//  readOnly - it will receive the access mode of the drive
//   (1-read only; 0-read/write) */
//STRDISKAPI unsigned SDGetInfo(char driveLetter,
// char* fileName, unsigned* size, char* readOnly);
function TOTFEStrongDisk.CallSDGetInfo(driveLetter: char;
                                       var filename: string;
                                       var readonly: boolean): boolean;
var
  SDFunc: TSDGetInfoFunc;
  funcResult: DWORD;
  filenameSize: DWORD;
  readonlyByte: byte;
  arrFilename: array of char;
begin
  Result := FALSE;

  driveLetter := upcase(driveLetter);

  // Call the DLL to mount the volume
  @SDFunc := GetProcAddress(hStrongDiskDLL, ''+DLL_FUNCTIONNAME_SDGETINFO+'');
  if @SDFunc=nil then
    begin
    raise EStrongDiskBadDLL.Create(E_STRONGDISK_BAD_DLL+' ('+DLL_FUNCTIONNAME_SDGETINFO+')');
    end;

  // Obtain length of "filename"
  funcResult := SDFunc(driveLetter,
                       nil,
                       @filenameSize,
                       @readonlyByte);
  if funcResult<>SDE_OK then
    begin
    // Set the LastErrorCode to something appropriate
    LastErrorCode := TranslateDLLError(funcResult);
    // Result already set to FALSE, so just exit
    exit;
    end
  else
    begin
    Result := TRUE;
    end;


  SetLength(arrFilename, filenameSize+2);

  funcResult := SDFunc(driveLetter,
                       @arrFilename[1],
                       @filenameSize,
                       @readonlyByte);
  if funcResult<>SDE_OK then
    begin
    // Set the LastErrorCode to something appropriate
    LastErrorCode := TranslateDLLError(funcResult);
    // Result already set to FALSE, so just exit
    exit;
    end
  else
    begin
    Result := TRUE;
    end;

end;


function TOTFEStrongDisk.TranslateDLLError(DLLerror: DWORD): integer;
var
  retVal: integer;
begin
  retVal := OTFE_ERR_UNKNOWN_ERROR;

  case DLLerror of
    // ERROR CODES
    // operation succeded
    SDE_OK                       : retVal := OTFE_ERR_SUCCESS;
    // operation failed; unexpected error
    SDE_ERROR                    : retVal := OTFE_ERR_UNKNOWN_ERROR;
    // unable load a driver; possible causes: not sufficient system resources; the driver is not installed
    SDE_UNABLE_LOAD_DRIVER       : retVal := OTFE_ERR_DRIVER_FAILURE;
    // not enough memory
    SDE_NOT_ENOUGH_MEMORY        : retVal := OTFE_ERR_DRIVER_FAILURE;
    // failed to open or to read a file
    SDE_ACCESS_DENIED            : retVal := OTFE_ERR_MOUNT_FAILURE;
    // unable unmount a secure drive due to open file
    SDE_UNMOUNT_OPEN_FILE        : retVal := OTFE_ERR_FILES_OPEN;
    // the drive specified is not a StrongDisk drive
    SDE_NOT_SDDRIVE              : retVal := OTFE_ERR_INVALID_DRIVE;
    // attempting to mount a StrongDisk image from a StrongDisk drive; StrongDisk drives don't support recursion
    SDE_MOUNT_ON_SDDRIVE         : retVal := OTFE_ERR_MOUNT_FAILURE;
    // mounting the StrongDisk drive is not allowed by the registration
    SDE_NOT_REGISTERED           : retVal := OTFE_ERR_DRIVER_FAILURE;
    // the file specified is not a StrongDisk drive image
    SDE_INVALID_IMAGE            : retVal := OTFE_ERR_FILE_NOT_ENCRYPTED_VOLUME;
    // the used cryptographic library does not support an algorithm that is used in the image
    SDE_UNSUPPORTED_ALGORITHM    : retVal := OTFE_ERR_DRIVER_FAILURE;
    // the password specified is invalid
    SDE_INVALID_PASSWORD         : retVal := OTFE_ERR_WRONG_PASSWORD;
    // device (a drive) is not ready
    SDE_KEYFILE_NOT_READY        : retVal := OTFE_ERR_DISMOUNT_FAILURE;
    // the file specified is too short to be a key
    SDE_KEYFILE_SHORT            : retVal := OTFE_ERR_MOUNT_FAILURE;
    // the key file is not found
    SDE_KEYFILE_NOT_FOUND        : retVal := OTFE_ERR_MOUNT_FAILURE;
    // unable to open or to read the key file
    SDE_KEYFILE_ERROR            : retVal := OTFE_ERR_UNABLE_TO_LOCATE_FILE;
    // unable to open an electronic key
    SDE_ELKEY_UNABLE_OPEN_DEVICE : retVal := OTFE_ERR_MOUNT_FAILURE;
    // unable to open key file on the electronic key
    SDE_ELKEY_UNABLE_OPEN_FILE   : retVal := OTFE_ERR_MOUNT_FAILURE;
    // unable to read key file on the electronic key
    SDE_ELKEY_UNABLE_READ_FILE   : retVal := OTFE_ERR_MOUNT_FAILURE;
    // access to electronic key failed
    SDE_ELKEY_ERROR              : retVal := OTFE_ERR_MOUNT_FAILURE;

    // SDE_INVALID_PARAMETER_1: the first parameter is invalid
    SDE_INVALID_PARAMETER_1   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_2   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_3   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_4   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_5   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_6   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_7   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_8   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_9   : retVal := OTFE_ERR_UNKNOWN_ERROR;
    SDE_INVALID_PARAMETER_10  : retVal := OTFE_ERR_UNKNOWN_ERROR;
    // ...
    SDE_INVALID_PARAMETER_100 : retVal := OTFE_ERR_UNKNOWN_ERROR;

 end;

 Result := retVal;

end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEStrongDisk.MountDevices(): string;
begin
  // Not supported...
  Result := #0;
end;

// -----------------------------------------------------------------------------
// Determine if OTFE component can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFEStrongDisk.CanMountDevice(): boolean;
begin
  // Not supported...
  Result := FALSE;
end;

// -----------------------------------------------------------------------------

END.


