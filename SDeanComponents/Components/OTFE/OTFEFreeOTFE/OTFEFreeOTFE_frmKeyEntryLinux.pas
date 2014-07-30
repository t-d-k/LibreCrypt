unit OTFEFreeOTFE_frmKeyEntryLinux;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, PasswordRichEdit, Spin64,
  SDUForms,
  OTFEFreeOTFE_U,  // Required for TFreeOTFEMountAs
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_DriverAPI, OTFEFreeOTFE_PasswordRichEdit, SDUFrames,
  SDUSpin64Units, SDUFilenameEdit_U, SDUDialogs, SDUStdCtrls;  // Required for TFreeOTFESectorIVGenMethod and NULL_GUID

type
  TfrmKeyEntryLinux = class(TSDUForm)
    pbCancel: TButton;
    pbOK: TButton;
    pcEntry: TPageControl;
    tsKey: TTabSheet;
    tsEncryption: TTabSheet;
    tsFileOptions: TTabSheet;
    tsMountOptions: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label16: TLabel;
    Label15: TLabel;
    Label14: TLabel;
    Label21: TLabel;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    cbKeyProcHash: TComboBox;
    seKeyProcCypherIterations: TSpinEdit64;
    pbKeyProcHashInfo: TButton;
    pbKeyProcCypherInfo: TButton;
    cbKeyProcCypher: TComboBox;
    edKeySeed: TEdit;
    GroupBox3: TGroupBox;
    Label23: TLabel;
    Label24: TLabel;
    cbMainCypher: TComboBox;
    pbMainCypherInfo: TButton;
    GroupBox4: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    GroupBox2: TGroupBox;
    lblDrive: TLabel;
    lblReadOnlySwitch: TLabel;
    cbDrive: TComboBox;
    ckMountReadonly: TSDUCheckBox;
    preUserkey: TOTFEFreeOTFE_PasswordRichEdit;
    pbLoad: TButton;
    pbSave: TButton;
    OpenSettingsFileDlg: TSDUOpenDialog;
    SaveSettingsFileDlg: TSDUSaveDialog;
    Label22: TLabel;
    GroupBox6: TGroupBox;
    Label5: TLabel;
    cbSectorIVGenMethod: TComboBox;
    lblIVHash: TLabel;
    Label12: TLabel;
    cbSectorIVHash: TComboBox;
    pbIVHashInfo: TButton;
    rgSectorIVSectorZeroPos: TRadioGroup;
    ckHashWithAs: TSDUCheckBox;
    lblIVCypher: TLabel;
    cbSectorIVCypher: TComboBox;
    pbIVCypherInfo: TButton;
    lblMountAs: TLabel;
    cbMediaType: TComboBox;
    ckMountForAllUsers: TSDUCheckBox;
    se64UnitOffset: TSDUSpin64Unit_Storage;
    se64UnitSizeLimit: TSDUSpin64Unit_Storage;
    feGPGExecutable: TSDUFilenameEdit;
    feGPGKeyfile: TSDUFilenameEdit;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
    procedure pbKeyProcHashInfoClick(Sender: TObject);
    procedure pbKeyProcCypherInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pbCancelClick(Sender: TObject);
    procedure pbIVHashInfoClick(Sender: TObject);
    procedure pbMainCypherInfoClick(Sender: TObject);
    procedure ckSelected(Sender: TObject);
    procedure pbLoadClick(Sender: TObject);
    procedure pbSaveClick(Sender: TObject);
    procedure pbIVCypherInfoClick(Sender: TObject);
  private

  protected
    // These are ordered lists corresponding to the items shown in the combobox
    hashKernelModeDriverNames: TStringList;
    hashGUIDs: TStringList;
    cypherKernelModeDriverNames: TStringList;
    cypherGUIDs: TStringList;

    procedure PopulateHashes();
    procedure PopulateCyphers();
    procedure PopulateSectorIVGenMethods();
    procedure PopulateDrives();
    procedure PopulateMountAs();

    procedure DefaultOptions();

    procedure EnableDisableControls();

    procedure DoCancel();

  public
    FreeOTFEObj: TOTFEFreeOTFEBase;

    // Key...
    function  GetKey(var userKey: ansistring): boolean;
    function  SetKey(userKey: ansistring): boolean;

    // Key processing...
    function  GetKeyProcSeed(var keyProcSeed: Ansistring): boolean;
    function  SetKeyProcSeed(keyProcSeed: string): boolean;
    function  GetKeyProcHashKernelDeviceName(var keyProcHashDriver: string): boolean;
    function  GetKeyProcHashGUID(var keyProcHashGUID: TGUID): boolean;
    function  SetKeyProcHash(keyProcHashDriver: string; keyProcHashGUID: TGUID): boolean;
    function  GetKeyProcHashWithAs(var hashWithAs: boolean): boolean;
    function  SetKeyProcHashWithAs(hashWithAs: boolean): boolean;
    function  GetKeyProcCypherKernelDeviceName(var keyProcCypherDriver: string): boolean;
    function  GetKeyProcCypherGUID(var keyProcCypherGUID: TGUID): boolean;
    function  SetKeyProcCypher(keyProcCypherDriver: string; keyProcCypherGUID: TGUID): boolean;
    // Note: GetKeyProcCypherIterationCount returns the number of iterations
    //       required; if the user entered "2" on the dialog, then 2000 will
    //       be returned by this function
    function  GetKeyProcCypherIterationCount(var keyProcCypherIterations: integer): boolean;
    function  SetKeyProcCypherIterationCount(keyProcCypherIterations: integer): boolean;

    // File options...
    function  GetOffset(var fileOptoffset: int64): boolean;
    function  SetOffset(fileOptoffset: int64): boolean;
    function  GetSizeLimit(var fileOptSizeLimit: int64): boolean;
    function  SetSizeLimit(fileOptSizeLimit: int64): boolean;

    // Encryption options...
    function  GetMainCypherKernelDeviceName(var mainCypherDriver: Ansistring): boolean;
    function  GetMainCypherGUID(var mainCypherGUID: TGUID): boolean;
    function  SetMainCypher(mainCypherDriver: string; mainCypherGUID: TGUID): boolean;
    function  GetMainIVSectorZeroPos(var startOfVolFile: boolean; var startOfEndData: boolean): boolean;
    // Note: At most *one* of the parameters passed to SetMainIVSectorZeroPos(...) can be set to TRUE
    function  SetMainIVSectorZeroPos(startOfVolFile: boolean; startOfEndData: boolean): boolean;
    function  GetMainSectorIVGenMethod(var sectorIVGenMethod: TFreeOTFESectorIVGenMethod): boolean;
    function  SetMainSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod): boolean;
    function  GetMainIVHashKernelDeviceName(var mainIVHashDriver: Ansistring): boolean;
    function  GetMainIVHashGUID(var mainIVHashGUID: TGUID): boolean;
    function  SetMainIVHash(mainIVHashDriver: string; mainIVHashGUID: TGUID): boolean;
    function  GetMainIVCypherKernelDeviceName(var mainIVCypherDriver: Ansistring): boolean;
    function  GetMainIVCypherGUID(var mainIVCypherGUID: TGUID): boolean;
    function  SetMainIVCypher(mainIVCypherDriver: string; mainIVCypherGUID: TGUID): boolean;

    // Mount options...
    function  GetDriveLetter(var mountDriveLetter: ansichar): boolean;
    function  SetDriveLetter(mountDriveLetter: ansichar): boolean;
    function  GetReadonly(var mountReadonly: boolean): boolean;
    function  SetReadonly(mountReadonly: boolean): boolean;
    function  GetMountAs(var mountAs: TFreeOTFEMountAs): boolean;
    function  SetMountAs(mountAs: TFreeOTFEMountAs): boolean;
    function  GetMountForAllUsers(): boolean;
    procedure SetMountForAllUsers(allUsers: boolean);

    procedure Initialize();
    procedure LoadSettings(filename: string);

  end;


implementation

{$R *.DFM}


uses
  ComObj,  // Required for StringToGUID
  OTFEFreeOTFE_VolumeFileAPI,  // Required for SCTRIVGEN_USES_SECTOR_ID and SCTRIVGEN_USES_HASH
  OTFEFreeOTFEDLL_U,
  SDUi18n,
  SDUGeneral,
  INIFiles;

const

  // If they're available, use these as defaults...
//  DEFAULT_KEYPROC_HASH_TITLE    = 'SHA-512 (512/1024)';
//  DEFAULT_KEYPROC_CYPHER_TITLE  = 'AES (CBC; 256/128)';
//  DEFAULT_MAIN_CYPHER_TITLE     = 'AES (CBC; 256/128)';
//  DEFAULT_MAIN_IV_HASH_TITLE    = 'MD5 (128/512)';
  DEFAULT_KEYPROC_HASH_TITLE    = 'SHA-512';
  DEFAULT_KEYPROC_CYPHER_TITLE  = 'AES (256 bit CBC)';
  DEFAULT_MAIN_CYPHER_TITLE     = 'AES (256 bit CBC)';
  DEFAULT_MAIN_IV_HASH_TITLE    = 'SHA-256';     //tdk change - make consistent with linux scripts
  DEFAULT_MAIN_IV_GEN_METHOD    = 'ESSIV';     //tdk change - make consistent with linux scripts

  // Settings file sections and values
  SETTINGS_SECTION_KEY                            = 'Key';
    SETTINGS_VALUE_KeyProcSeed                      = 'KeyProcSeed';
    SETTINGS_VALUE_KeyProcHashKernelDeviceName      = 'KeyProcHashKernelDeviceName';
    SETTINGS_VALUE_KeyProcHashGUID                  = 'KeyProcHashGUID';
    SETTINGS_VALUE_HashWithAs                       = 'KeyProcHashWithAs';
    SETTINGS_VALUE_KeyProcCypherKernelDeviceName    = 'KeyProcCypherKernelDeviceName';
    SETTINGS_VALUE_KeyProcCypherGUID                = 'KeyProcCypherGUID';
    SETTINGS_VALUE_KeyProcCypherIterationCount      = 'KeyProcCypherIterationCount';
  SETTINGS_SECTION_ENCRYPTION                     = 'Encryption';
    SETTINGS_VALUE_MainCypherKernelDeviceName       = 'MainCypherKernelDeviceName';
    SETTINGS_VALUE_MainCypherGUID                   = 'MainCypherGUID';
    SETTINGS_VALUE_MainIVGenMethod                  = 'MainIVGenMethod';
    SETTINGS_VALUE_MainIVSectorZeroPos              = 'MainIVSectorZeroPos';
    SETTINGS_VALUE_MainIVHashKernelDeviceName       = 'MainIVHashKernelDeviceName';
    SETTINGS_VALUE_MainIVHashGUID                   = 'MainIVHashGUID';
    SETTINGS_VALUE_MainIVCypherKernelDeviceName     = 'MainIVCypherKernelDeviceName';
    SETTINGS_VALUE_MainIVCypherGUID                 = 'MainIVCypherGUID';
  SETTINGS_SECTION_FILE_OPTIONS                   = 'FileOptions';
    SETTINGS_VALUE_Offset                           = 'Offset';
    SETTINGS_VALUE_Size                             = 'Size';
  SETTINGS_SECTION_MOUNT_OPTIONS                  = 'MountOptions';
    SETTINGS_VALUE_DriveLetter                      = 'DriveLetter';
    SETTINGS_VALUE_Readonly                         = 'Readonly';
    SETTINGS_VALUE_MountAs                          = 'MountAsEnum'; // Previously "MountAs" - a string


  // Flags to indicate if sector IDs used for IVs start at the beginning of the
  // file, or the start of the encrypted data
  // These are CONSTS - not RESOURCESTRINGS - they're stored in the .ini file
  IV_SECTOR_ID_START_UNSET            = 'Unset';
  IV_SECTOR_ID_START_ENCRYPTED_DATA   = 'EncryptedData';
  IV_SECTOR_ID_START_ENCRYPTED_VOLUME = 'File';

resourcestring
  FILTER_GPG_FILES = 'GPG files|*.gpg|All files|*.*';
  FILTER_GPG_EXE   = 'GPG Executable|GPG.exe|All files|*.*';

procedure TfrmKeyEntryLinux.FormCreate(Sender: TObject);
begin
  hashKernelModeDriverNames:= TStringList.Create();
  hashGUIDs:= TStringList.Create();
  cypherKernelModeDriverNames:= TStringList.Create();
  cypherGUIDs:= TStringList.Create();

  pcEntry.ActivePage := tsKey;

end;


procedure TfrmKeyEntryLinux.PopulateHashes();
var
  tmpDisplayTitles: TStringList;
begin
  tmpDisplayTitles:= TStringList.Create();
  try
    if (FreeOTFEObj.GetHashList(tmpDisplayTitles, hashKernelModeDriverNames, hashGUIDs)) then
      begin
      cbKeyProcHash.Items.Clear();
      cbKeyProcHash.Items.AddStrings(tmpDisplayTitles);

      cbSectorIVHash.Items.Clear();
      cbSectorIVHash.Items.AddStrings(tmpDisplayTitles);
      end
    else
      begin
      SDUMessageDlg(
                 _('Unable to obtain list of hashes.')+SDUCRLF+
                 SDUCRLF+
                 _('Please ensure that you have one or more FreeOTFE hash drivers installed and started.')+SDUCRLF+
                 SDUCRLF+
                 _('If you have only just installed FreeOTFE, you may need to restart your computer.'),
                 mtError
                 );
      end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmKeyEntryLinux.PopulateCyphers();
var
  tmpDisplayTitles: TStringList;
begin
  tmpDisplayTitles:= TStringList.Create();
  try
    if (FreeOTFEObj.GetCypherList(tmpDisplayTitles, cypherKernelModeDriverNames, cypherGUIDs)) then
      begin
      cbKeyProcCypher.Items.Clear();
      cbKeyProcCypher.Items.AddStrings(tmpDisplayTitles);

      cbMainCypher.Items.Clear();
      cbMainCypher.Items.AddStrings(tmpDisplayTitles);

      cbSectorIVCypher.Items.Clear();
      cbSectorIVCypher.Items.AddStrings(tmpDisplayTitles);
      end
    else
      begin
      SDUMessageDlg(
                 _('Unable to obtain list of cyphers.')+SDUCRLF+
                 SDUCRLF+
                 _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.')+SDUCRLF+
                 SDUCRLF+
                 _('If you have only just installed FreeOTFE, you may need to restart your computer.'),
                 mtError
                );
      end;
  finally
    tmpDisplayTitles.Free();
  end;
  
end;

procedure TfrmKeyEntryLinux.PopulateSectorIVGenMethods();
var
  currMethod: TFreeOTFESectorIVGenMethod;
begin
  cbSectorIVGenMethod.Items.Clear();
  for currMethod:=low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do
    begin
    // Skip "Unknown"
    if (currMethod <> foivgUnknown) then
      begin
      cbSectorIVGenMethod.Items.AddObject(FreeOTFESectorIVGenMethodTitle[currMethod], Pointer(ord(currMethod)));
      end;
    end;

end;


procedure TfrmKeyEntryLinux.PopulateDrives();
var
  driveLetters: ansistring;
  i: integer;
begin
  cbDrive.Items.Clear();
  cbDrive.Items.Add(_('Use default'));
  driveLetters := SDUGetUnusedDriveLetters();
  for i:=1 to length(driveLetters) do
    begin
    // Skip the drive letters traditionally reserved for floppy disk drives
//    if (
//        (driveLetters[i] <> 'A') AND
//        (driveLetters[i] <> 'B')
//       ) then
//      begin
      cbDrive.Items.Add(driveLetters[i]+':');
//      end;
    end;

end;


procedure TfrmKeyEntryLinux.PopulateMountAs();
var
  currMountAs: TFreeOTFEMountAs;
begin
  cbMediaType.Items.Clear();
  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (currMountAs <> fomaUnknown) then
      begin
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
      end;

    end;

end;


procedure TfrmKeyEntryLinux.DefaultOptions();
var
  idx: integer;
  i: integer;
  currDriveLetter: ansichar;
begin
  // Autoselect default, if available
  if (cbKeyProcHash.Items.Count>0) then
    begin
    idx := cbKeyProcHash.Items.IndexOf(DEFAULT_KEYPROC_HASH_TITLE);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbKeyProcHash.ItemIndex := idx;
    end;


  // Autoselect default, if available
  if (cbKeyProcCypher.Items.Count>0) then
    begin
    idx := cbKeyProcCypher.Items.IndexOf(DEFAULT_KEYPROC_CYPHER_TITLE);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbKeyProcCypher.ItemIndex := idx;
    end;


  ckHashWithAs.checked := TRUE;
  

  // Autoselect default, if available
  if (cbMainCypher.Items.Count>0) then
    begin
    idx := cbMainCypher.Items.IndexOf(DEFAULT_MAIN_CYPHER_TITLE);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbMainCypher.ItemIndex := idx;
    end;

    
  // Autoselect default, if available
  if (cbSectorIVCypher.Items.Count>0) then
    begin
    idx := cbSectorIVCypher.Items.IndexOf(DEFAULT_MAIN_CYPHER_TITLE);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbSectorIVCypher.ItemIndex := idx;
    end;


  // Autoselect default, if available
  if (cbSectorIVHash.Items.Count>0) then
    begin
    idx := cbSectorIVHash.Items.IndexOf(DEFAULT_MAIN_IV_HASH_TITLE);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbSectorIVHash.ItemIndex := idx;
    end;

    //tdk change - add default iv gen method of essiv = linux script default
    if (cbSectorIVGenMethod.Items.Count>0) then
    begin
    idx := cbSectorIVGenMethod.Items.IndexOf(DEFAULT_MAIN_IV_GEN_METHOD);
    if (idx = -1) then
      begin
      idx := 0;
      end;

    cbSectorIVGenMethod.ItemIndex := idx;
    end;


  if (FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    if (cbDrive.Items.Count>0) then
      begin
      cbDrive.ItemIndex := 0;

      if (TOTFEFreeOTFE(FreeOTFEObj).DefaultDriveLetter <> #0) then
        begin
        // Start from 1; skip the default
        for i:=1 to (cbDrive.items.count-1) do
          begin
          currDriveLetter := ansichar(cbDrive.Items[i][1]);
          if (currDriveLetter >= TOTFEFreeOTFE(FreeOTFEObj).DefaultDriveLetter) then
            begin
            cbDrive.ItemIndex := i;
            break;
            end;
          end;
        end;
      end;
    end;

  if (FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    SetMountAs(TOTFEFreeOTFE(FreeOTFEObj).DefaultMountAs);
    end
  else
    begin
    SetMountAs(fomaFixedDisk);
    end;

  preUserKey.Plaintext := TRUE;
  // Linux volumes CAN NOT have newlines in the user's password
  preUserKey.WantReturns := FALSE;
  preUserKey.WordWrap := TRUE;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := FreeOTFEObj.PasswordChar;
  preUserKey.WantReturns  := FreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey.WantTabs     := FreeOTFEObj.AllowTabsInPasswords;

  feGPGExecutable.Filename := '';
  feGPGKeyfile.Filename := '';

  edKeySeed.text := '';
  seKeyProcCypherIterations.Value := 0;

  se64UnitSizeLimit.Value := 0;
  se64UnitOffset.Value := 0;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.checked := TRUE;

end;


procedure TfrmKeyEntryLinux.pbOKClick(Sender: TObject);
var
  tmpKey: ansistring;
begin
  if GetKey(tmpKey) then
    begin
    if (Length(tmpKey)=0) then
      begin
      if (SDUMessageDlg(
                 _('You have not entered a Keyphrase.')+SDUCRLF+
                 SDUCRLF+
                 _('Are you sure you wish to proceed?'),
                 mtConfirmation,
                 [mbYes, mbNo],
                 0
                ) = mrYes) then
        begin
        ModalResult := mrOK;
        end;
      end
    else if (Length(tmpKey)<20) then
      begin
      if (SDUMessageDlg(
                     _('The Keyphrase you entered has less than 20 characters, and may not be compatible with some Linux volumes.')+SDUCRLF+
                     SDUCRLF+
                     _('Do you wish to proceed?'),
                      mtWarning,
                      [mbYes, mbNo],
                      0
                     ) = mrYes) then
        begin
        ModalResult := mrOK;
        end;
      end
    else
      begin
      // No problems with the password as entered; OK to close the dialog
      ModalResult := mrOK;
      end;

  end;  // if GetKey(tmpKey) then

end;


procedure TfrmKeyEntryLinux.EnableDisableControls();
var
  junkInt64: int64;
  filePresentGPGExe: boolean;
  filePresentGPGKeyfile: boolean;
  iterationCypherOK: boolean;
  IVStartSectorOK: boolean;
  junkChar: ansichar;
  IVHashOK: boolean;
  IVCypherOK: boolean;
  mountAsOK: boolean;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  tmpMountAs: TFreeOTFEMountAs;
begin
  SDUEnableControl(cbKeyProcCypher, (seKeyProcCypherIterations.Value>0));

  SDUEnableControl(rgSectorIVSectorZeroPos, FALSE);
  SDUEnableControl(cbSectorIVHash, FALSE);
  SDUEnableControl(cbSectorIVCypher, FALSE);
  sectorIVGenMethod := foivgUnknown;
  if GetMainSectorIVGenMethod(sectorIVGenMethod) then
    begin
    SDUEnableControl(rgSectorIVSectorZeroPos, SCTRIVGEN_USES_SECTOR_ID[sectorIVGenMethod]);
    SDUEnableControl(cbSectorIVHash, SCTRIVGEN_USES_HASH[sectorIVGenMethod]);
    SDUEnableControl(cbSectorIVCypher, SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]);
    end;

  SDUEnableControl(pbKeyProcHashInfo, (cbKeyProcHash.ItemIndex>=0));
  SDUEnableControl(pbKeyProcCypherInfo, (cbKeyProcCypher.ItemIndex>=0) and (cbKeyProcCypher.Enabled));
  SDUEnableControl(pbMainCypherInfo, (cbMainCypher.ItemIndex>=0));
  SDUEnableControl(pbIVHashInfo, (cbSectorIVHash.ItemIndex>=0) and (cbSectorIVHash.Enabled));
  SDUEnableControl(pbIVCypherInfo, (cbSectorIVCypher.ItemIndex>=0) and (cbSectorIVCypher.Enabled));

  SDUEnableControl(lblIVHash, cbSectorIVHash.enabled);
  SDUEnableControl(lblIVCypher, cbSectorIVCypher.enabled);

  filePresentGPGExe := TRUE;
  if (feGPGExecutable.Filename <> '') then
    begin
    filePresentGPGExe := FileExists(feGPGExecutable.Filename);
    end;

  filePresentGPGKeyfile := TRUE;
  if (feGPGKeyfile.Filename <> '') then
    begin
    filePresentGPGKeyfile := FileExists(feGPGKeyfile.Filename);
    end;

  iterationCypherOK := TRUE;
  if (seKeyProcCypherIterations.value > 0) then
    begin
    iterationCypherOK := (cbKeyProcCypher.ItemIndex>=0);
    end;


  // Ensure user specified sector ID zero location, if required
  IVStartSectorOK := TRUE;
  if (rgSectorIVSectorZeroPos.Enabled) then
    begin
    IVStartSectorOK := (rgSectorIVSectorZeroPos.ItemIndex>=0);
    end;


  // Ensure user selected a sector IV hash, if one is required
  IVHashOK := TRUE;
  if (cbSectorIVHash.Enabled) then
    begin
    IVHashOK := (cbSectorIVHash.ItemIndex>=0);
    end;

  // Ensure user selected a sector IV hash, if one is required
  IVCypherOK := TRUE;
  if (cbSectorIVCypher.Enabled) then
    begin
    IVCypherOK := (cbSectorIVCypher.ItemIndex>=0);
    end;


  // Ensure we know what to mount as
  mountAsOK := GetMountAs(tmpMountAs);
  ckMountReadonly.Enabled := FALSE;
  if (mountAsOK) then
    begin
    if not(FreeOTFEMountAsCanWrite[tmpMountAs]) then
      begin
      ckMountReadonly.checked := TRUE;
      end;
    SDUEnableControl(ckMountReadonly, FreeOTFEMountAsCanWrite[tmpMountAs]);
    end;
  mountAsOK := mountAsOK AND (tmpMountAs <> fomaUnknown);



  pbOK.Enabled := (
                   (filePresentGPGExe)           AND
                   (filePresentGPGKeyfile)       AND
                   (cbKeyProcHash.ItemIndex>=0)  AND
                   iterationCypherOK             AND
                   (GetOffset(junkInt64))        AND
                   (GetSizeLimit(junkInt64))     AND
                   (GetDriveLetter(junkChar))    AND
                   (cbMainCypher.ItemIndex>=0)   AND
                   (sectorIVGenMethod <> foivgUnknown) AND
                   IVStartSectorOK               AND
                   IVHashOK                      AND
                   IVCypherOK                    AND
                   mountAsOK
                  );

  // Items NOT YET IMPLEMENTED
  //  - after implementing; DON'T FORGET TO ENABLE THE ASSOCIATED LABELS!
  SDUEnableControl(feGPGExecutable, FALSE);
  SDUEnableControl(feGPGKeyfile, FALSE);
  SDUEnableControl(cbKeyProcCypher, FALSE);
  SDUEnableControl(pbKeyProcCypherInfo, FALSE);
  SDUEnableControl(seKeyProcCypherIterations, FALSE);

end;

procedure TfrmKeyEntryLinux.SelectionChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryLinux.pbKeyProcHashInfoClick(Sender: TObject);
var
  deviceName: string;
  GUID: TGUID;
begin
  GetKeyProcHashKernelDeviceName(deviceName);
  GetKeyProcHashGUID(GUID);
  FreeOTFEObj.ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmKeyEntryLinux.pbIVHashInfoClick(Sender: TObject);
var
  deviceName: Ansistring;
  GUID: TGUID;
begin
  GetMainIVHashKernelDeviceName(deviceName);
  GetMainIVHashGUID(GUID);
  FreeOTFEObj.ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmKeyEntryLinux.pbKeyProcCypherInfoClick(Sender: TObject);
var
  deviceName: string;
  GUID: TGUID;
begin
  GetKeyProcCypherKernelDeviceName(deviceName);
  GetKeyProcCypherGUID(GUID);
  FreeOTFEObj.ShowCypherDetailsDlg(deviceName, GUID);

end;

procedure TfrmKeyEntryLinux.pbMainCypherInfoClick(Sender: TObject);
var
  deviceName: Ansistring;
  GUID: TGUID;
begin
  GetMainCypherKernelDeviceName(deviceName);
  GetMainCypherGUID(GUID);
  FreeOTFEObj.ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmKeyEntryLinux.Initialize();
begin
  OpenSettingsFileDlg.Filter     := FILE_FILTER_FLT_LINUX_SETTINGS;
  OpenSettingsFileDlg.DefaultExt := FILE_FILTER_DFLT_LINUX_SETTINGS;
  OpenSettingsFileDlg.Options    := OpenSettingsFileDlg.Options + [ofDontAddToRecent];
  SaveSettingsFileDlg.Filter     := FILE_FILTER_FLT_LINUX_SETTINGS;
  SaveSettingsFileDlg.DefaultExt := FILE_FILTER_DFLT_LINUX_SETTINGS;
  SaveSettingsFileDlg.Options    := SaveSettingsFileDlg.Options + [ofDontAddToRecent];

  PopulateHashes();
  PopulateCyphers();
  PopulateSectorIVGenMethods();
  PopulateDrives();
  PopulateMountAs();

  DefaultOptions();
end;

procedure TfrmKeyEntryLinux.FormShow(Sender: TObject);
begin
  feGPGExecutable.Filter := FILTER_GPG_EXE;
  feGPGKeyfile.Filter    := FILTER_GPG_FILES;
  feGPGExecutable.OpenDialog.Options := feGPGExecutable.OpenDialog.Options + [ofDontAddToRecent];
  feGPGExecutable.SaveDialog.Options := feGPGExecutable.SaveDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.OpenDialog.Options := feGPGKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.SaveDialog.Options := feGPGKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);

  // Certain controls only visble if used in conjunction with drive mounting
  lblDrive.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  cbDrive.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  lblMountAs.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  cbMediaType.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  ckMountForAllUsers.Visible := FreeOTFEObj is TOTFEFreeOTFE;

  // Prevent making remaining control look odd, stuck in the middle
  if not(FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    ckMountReadonly.top := lblDrive.top;
    ckMountReadonly.left := lblDrive.left;

    lblReadOnlySwitch.top := ckMountReadonly.top;
    end;

  EnableDisableControls();

end;


procedure TfrmKeyEntryLinux.FormDestroy(Sender: TObject);
begin
  hashKernelModeDriverNames.Free();
  hashGUIDs.Free();
  cypherKernelModeDriverNames.Free();
  cypherGUIDs.Free();

end;


procedure TfrmKeyEntryLinux.preUserkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
    begin
    DoCancel();
    end;

end;

procedure TfrmKeyEntryLinux.pbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;

end;


procedure TfrmKeyEntryLinux.DoCancel();
begin
  ModalResult := mrCancel;

end;


function TfrmKeyEntryLinux.GetKey(var userKey: ansistring): boolean;
begin
{ TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  userKey := preUserkey.Text;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetKey(userKey: ansistring): boolean;
begin
  preUserkey.Text := userKey;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.GetKeyProcSeed(var keyProcSeed: Ansistring): boolean;
begin
  keyProcSeed := edKeySeed.Text;   { TODO 1 -otdk -cclean : allow unicode }
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetKeyProcSeed(keyProcSeed: string): boolean;
begin
  edKeySeed.Text := keyProcSeed;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.GetKeyProcHashKernelDeviceName(var keyProcHashDriver: string): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (cbKeyProcHash.ItemIndex >= 0) then
    begin
    keyProcHashDriver := hashKernelModeDriverNames[cbKeyProcHash.ItemIndex];
    retVal := TRUE;
    end;

  Result := retVal;
end;

function TfrmKeyEntryLinux.GetKeyProcHashGUID(var keyProcHashGUID: TGUID): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (cbKeyProcHash.ItemIndex >= 0) then
    begin
    keyProcHashGUID := StringToGUID(hashGUIDs[cbKeyProcHash.ItemIndex]);
    retVal := TRUE;
    end;

  Result := retVal;
end;


function TfrmKeyEntryLinux.SetKeyProcHash(keyProcHashDriver: string; keyProcHashGUID: TGUID): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate array idx
  for i:=0 to (hashKernelModeDriverNames.Count-1) do
    begin
    if (
        (hashKernelModeDriverNames[i] = keyProcHashDriver) and
        (hashGUIDs[i] = GUIDToString(keyProcHashGUID))
       ) then
      begin
      idx := i;
      break;
      end;

    end;

  cbKeyProcHash.ItemIndex := idx;

  Result := (idx <> -1);
end;

function TfrmKeyEntryLinux.GetKeyProcHashWithAs(var hashWithAs: boolean): boolean;
begin
  hashWithAs := ckHashWithAs.checked;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetKeyProcHashWithAs(hashWithAs: boolean): boolean;
begin
  ckHashWithAs.checked := hashWithAs;
  Result := TRUE;
end;


// This returns TRUE if a cypher is selected, or if no cypher is needed (in
// which case, an empty string is returned)
// Returns FALSE is a cypher is required, but one is not selected
function TfrmKeyEntryLinux.GetKeyProcCypherKernelDeviceName(var keyProcCypherDriver: string): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (seKeyProcCypherIterations.Value > 0) then
    begin
    if (cbKeyProcCypher.ItemIndex >= 0) then
      begin
      keyProcCypherDriver := cypherKernelModeDriverNames[cbKeyProcCypher.ItemIndex];
      retVal := TRUE;
      end;
    end
  else
    begin
    // No cypher selected, but none needed
    keyProcCypherDriver := '';
    retVal := TRUE;
    end;

  Result := retVal;
end;

// This returns TRUE if a cypher is selected, or if no cypher is needed (in
// which case, NULL_GUID is returned)
// Returns FALSE is a cypher is required, but one is not selected
function TfrmKeyEntryLinux.GetKeyProcCypherGUID(var keyProcCypherGUID: TGUID): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (seKeyProcCypherIterations.Value > 0) then
    begin
    if (cbKeyProcCypher.ItemIndex >= 0) then
      begin
      keyProcCypherGUID := StringToGUID(cypherGUIDs[cbKeyProcCypher.ItemIndex]);
      retVal := TRUE;
      end;
    end
  else
    begin
    // No cypher selected, but none needed
    keyProcCypherGUID := StringToGUID(NULL_GUID);
    retVal := TRUE;
    end;

  Result := retVal;
end;

function TfrmKeyEntryLinux.SetKeyProcCypher(keyProcCypherDriver: string; keyProcCypherGUID: TGUID): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate array idx
  for i:=0 to (cypherKernelModeDriverNames.Count-1) do
    begin
    if (
        (cypherKernelModeDriverNames[i] = keyProcCypherDriver) and
        (cypherGUIDs[i] = GUIDToString(keyProcCypherGUID))
       ) then
      begin
      idx := i;
      break;
      end;

    end;

  cbKeyProcCypher.ItemIndex := idx;
  
  Result := (idx <> -1);
end;


function TfrmKeyEntryLinux.GetKeyProcCypherIterationCount(var keyProcCypherIterations: integer): boolean;
begin
  keyProcCypherIterations := seKeyProcCypherIterations.Value * 1000;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetKeyProcCypherIterationCount(keyProcCypherIterations: integer): boolean;
begin
  // Note that if "1" passed in, this will be set to 0
  seKeyProcCypherIterations.Value := keyProcCypherIterations div 1000;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.GetOffset(var fileOptoffset: int64): boolean;
begin
  fileOptoffset := se64UnitOffset.Value;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetOffset(fileOptoffset: int64): boolean;
begin
  se64UnitOffset.Value := fileOptoffset;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.GetSizeLimit(var fileOptSizeLimit: int64): boolean;
begin
  fileOptSizeLimit := se64UnitSizeLimit.Value;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetSizeLimit(fileOptSizeLimit: int64): boolean;
begin
  se64UnitSizeLimit.Value := fileOptSizeLimit;
  Result := TRUE;
end;

// Note: This may return #0 as mountDriveLetter to indicate "any"
function TfrmKeyEntryLinux.GetDriveLetter(var mountDriveLetter: ansichar): boolean;
begin
  mountDriveLetter := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex>0) then
    begin
    mountDriveLetter :=AnsiChar( cbDrive.Items[cbDrive.ItemIndex][1]);
    end;

  Result := TRUE;
end;

// mountDriveLetter - Set to #0 to indicate "Use default"
function TfrmKeyEntryLinux.SetDriveLetter(mountDriveLetter: ansichar): boolean;
var
  idx: integer;
  retVal: boolean;
begin
  retVal := TRUE;

  if (mountDriveLetter = #0) then
    begin
    // The item at idx 0 will *always* be "Use default"
    idx := 0
    end
  else
    begin
    idx := cbDrive.Items.IndexOf(mountDriveLetter+':');
    end;

  if (idx < 0) then
    begin
    idx := 0;
    retVal := FALSE;
    end;
  cbDrive.ItemIndex := idx;

  Result := retVal;
end;

function TfrmKeyEntryLinux.GetReadonly(var mountReadonly: boolean): boolean;
begin
  mountReadonly := ckMountReadonly.checked;
  Result := TRUE;
end;

function TfrmKeyEntryLinux.SetReadonly(mountReadonly: boolean): boolean;
begin
  ckMountReadonly.checked := mountReadonly;
  Result := TRUE;
end;



function TfrmKeyEntryLinux.GetMountAs(var mountAs: TFreeOTFEMountAs): boolean;
var
  currMountAs: TFreeOTFEMountAs;
  allOK: boolean;
begin
  allOK := FALSE;

  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then
      begin
      mountAs := currMountAs;
      allOK := TRUE;
      break;
      end;
    end;

  Result := allOK;
end;


function TfrmKeyEntryLinux.SetMountAs(mountAs: TFreeOTFEMountAs): boolean;
var
  idx: integer;
  allOK: boolean;
begin
  idx := cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  cbMediaType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;


procedure TfrmKeyEntryLinux.SetMountForAllUsers(allUsers: boolean);
begin
  ckMountForAllUsers.checked := allUsers;
end;

function  TfrmKeyEntryLinux.GetMountForAllUsers(): boolean;
begin
  Result := ckMountForAllUsers.checked;
end;


function TfrmKeyEntryLinux.GetMainCypherKernelDeviceName(var mainCypherDriver: Ansistring): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (cbMainCypher.ItemIndex >= 0) then
    begin
    mainCypherDriver := cypherKernelModeDriverNames[cbMainCypher.ItemIndex];   // unicode->ansi not a data loss because only ansistring stored
    retVal := TRUE;
    end;

  Result := retVal;
end;

function TfrmKeyEntryLinux.GetMainCypherGUID(var mainCypherGUID: TGUID): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (cbMainCypher.ItemIndex >= 0) then
    begin
    mainCypherGUID := StringToGUID(cypherGUIDs[cbMainCypher.ItemIndex]);
    retVal := TRUE;
    end;

  Result := retVal;
end;

function TfrmKeyEntryLinux.SetMainCypher(mainCypherDriver: string; mainCypherGUID: TGUID): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate array idx
  for i:=0 to (cypherKernelModeDriverNames.Count-1) do
    begin
    if (
        (cypherKernelModeDriverNames[i] = mainCypherDriver) and
        (cypherGUIDs[i] = GUIDToString(mainCypherGUID))
       ) then
      begin
      idx := i;
      break;
      end;

    end;

  cbMainCypher.ItemIndex := idx;

  Result := (idx <> -1);
end;

function TfrmKeyEntryLinux.GetMainSectorIVGenMethod(var sectorIVGenMethod: TFreeOTFESectorIVGenMethod): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then
    begin
    sectorIVGenMethod := TFreeOTFESectorIVGenMethod(cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
    retVal := TRUE;
    end;

  Result := retVal;
end;

function TfrmKeyEntryLinux.SetMainSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate idx
  for i:=0 to (cbSectorIVGenMethod.Items.Count-1) do
    begin
    if (TFreeOTFESectorIVGenMethod(cbSectorIVGenMethod.Items.Objects[i]) = sectorIVGenMethod) then
      begin
      idx := i;
      break;
      end;
    end;

  cbSectorIVGenMethod.ItemIndex := idx;

  Result := (idx <> -1);
end;


function TfrmKeyEntryLinux.GetMainIVSectorZeroPos(var startOfVolFile: boolean; var startOfEndData: boolean): boolean;
begin
  // !! WARNING !!
  // Important that the radiogroup indexes are correct!
  startOfVolFile := (rgSectorIVSectorZeroPos.ItemIndex = 0);
  startOfEndData := (rgSectorIVSectorZeroPos.ItemIndex = 1);
  Result := TRUE;
end;


// Note: At most *one* of the parameters passed to SetMainIVSectorZeroPos(...) can be set to TRUE
function TfrmKeyEntryLinux.SetMainIVSectorZeroPos(startOfVolFile: boolean; startOfEndData: boolean): boolean;
begin
  rgSectorIVSectorZeroPos.ItemIndex := -1;

  // !! WARNING !!
  // Important that the radiogroup indexes are correct!
  if startOfVolFile then
    begin
    rgSectorIVSectorZeroPos.ItemIndex := 0;
    end;

  if startOfEndData then
    begin
    rgSectorIVSectorZeroPos.ItemIndex := 1;
    end;

  // Return TRUE if they're not *both* set, but at least one *is* set
  Result := not(startOfVolFile and startOfEndData) and (startOfVolFile or startOfEndData);
end;

// Note: This function will return '' if none is selected, and still return TRUE
// It will also return '' if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryLinux.GetMainIVHashKernelDeviceName(var mainIVHashDriver: Ansistring): boolean;
var
  retVal: boolean;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  retVal := FALSE;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then
    begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then
      begin
      // ...and the user has selected one, return it
      if (cbSectorIVHash.ItemIndex >= 0) then
        begin
        mainIVHashDriver := hashKernelModeDriverNames[cbSectorIVHash.ItemIndex];    // unicode->ansi not a data loss because only ansistring stored
        retVal := TRUE;
        end;
      end
    else
      begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVHashDriver := '';
      retVal := TRUE;
      end;

    end;


  Result := retVal;
end;

// Note: This function will return NULL_GUID if none is selected, and still return TRUE
// It will also return NULL_GUID if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryLinux.GetMainIVHashGUID(var mainIVHashGUID: TGUID): boolean;
var
  retVal: boolean;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  retVal := FALSE;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then
    begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then
      begin
      // ...and the user has selected one, return it
      if (cbSectorIVHash.ItemIndex >= 0) then
        begin
        mainIVHashGUID := StringToGUID(hashGUIDs[cbSectorIVHash.ItemIndex]);
        retVal := TRUE;
        end;
      end
    else
      begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVHashGUID := StringToGUID(NULL_GUID);
      retVal := TRUE;
      end;

    end;


  Result := retVal;
end;

function TfrmKeyEntryLinux.SetMainIVHash(mainIVHashDriver: string; mainIVHashGUID: TGUID): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate array idx
  for i:=0 to (hashKernelModeDriverNames.Count-1) do
    begin
    if (
        (hashKernelModeDriverNames[i] = mainIVHashDriver) and
        (hashGUIDs[i] = GUIDToString(mainIVHashGUID))
       ) then
      begin
      idx := i;
      break;
      end;

    end;

  cbSectorIVHash.ItemIndex := idx;

  Result := (idx <> -1);
end;


// Note: This function will return '' if none is selected, and still return TRUE
// It will also return '' if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryLinux.GetMainIVCypherKernelDeviceName(var mainIVCypherDriver: Ansistring): boolean;
var
  retVal: boolean;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  retVal := FALSE;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then
    begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then
      begin
      // ...and the user has selected one, return it
      if (cbSectorIVCypher.ItemIndex >= 0) then
        begin
        mainIVCypherDriver := cypherKernelModeDriverNames[cbSectorIVCypher.ItemIndex];  // unicode->ansi not a data loss because only ansistring stored
        retVal := TRUE;
        end;
      end
    else
      begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVCypherDriver := '';
      retVal := TRUE;
      end;

    end;


  Result := retVal;
end;


// Note: This function will return NULL_GUID if none is selected, and still return TRUE
// It will also return NULL_GUID if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryLinux.GetMainIVCypherGUID(var mainIVCypherGUID: TGUID): boolean;
var
  retVal: boolean;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  retVal := FALSE;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then
    begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then
      begin
      // ...and the user has selected one, return it
      if (cbSectorIVCypher.ItemIndex >= 0) then
        begin
        mainIVCypherGUID := StringToGUID(cypherGUIDs[cbSectorIVCypher.ItemIndex]);
        retVal := TRUE;
        end;
      end
    else
      begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVCypherGUID := StringToGUID(NULL_GUID);
      retVal := TRUE;
      end;

    end;


  Result := retVal;
end;

function TfrmKeyEntryLinux.SetMainIVCypher(mainIVCypherDriver: string; mainIVCypherGUID: TGUID): boolean;
var
  i: integer;
  idx: integer;
begin
  idx := -1;

  // Locate the appropriate array idx
  for i:=0 to (cypherKernelModeDriverNames.Count-1) do
    begin
    if (
        (cypherKernelModeDriverNames[i] = mainIVCypherDriver) and
        (cypherGUIDs[i] = GUIDToString(mainIVCypherGUID))
       ) then
      begin
      idx := i;
      break;
      end;

    end;

  cbSectorIVCypher.ItemIndex := idx;

  Result := (idx <> -1);
end;


procedure TfrmKeyEntryLinux.ckSelected(Sender: TObject);
begin
  EnableDisableControls();

end;



procedure TfrmKeyEntryLinux.pbLoadClick(Sender: TObject);
begin
  if (OpenSettingsFileDlg.Execute) then
    begin
    if not(FileExists(OpenSettingsFileDlg.Filename)) then
      begin
      SDUMessageDlg(_('Settings file not found.'), mtError);
      end
    else
      begin
      LoadSettings(OpenSettingsFileDlg.Filename);
      end;
    end;

end;

procedure TfrmKeyEntryLinux.LoadSettings(filename: string);
var
  allOK: boolean;
  settingsFile: TINIFile;
  tmpString: string;
  tmpStringName, tmpStringGUID: string;
  tmpBoolean: boolean;
  tmpInteger: integer;
  startOfVolFile, startOfEndData: boolean;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  found: boolean;
  ignoreIfCantSet: boolean;  // Set to TRUE for parameters which we don't
                             // really care about
                             // e.g. If not hash algorithm is required for the
                             //      IV, we can ignore it if we can't show the
                             //      hash read in
begin
  allOK := TRUE;

  settingsFile:= TINIFile.Create(filename);
  try
    // -------
    // Key processing...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcSeed, '');
    allOK := allOK and SetKeyProcSeed(tmpString);

    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashGUID, NULL_GUID);
    allOK := allOK and SetKeyProcHash(tmpStringName, StringToGUID(tmpStringGUID));

    tmpBoolean := settingsFile.ReadBool(SETTINGS_SECTION_KEY, SETTINGS_VALUE_HashWithAs, TRUE);
    allOK := allOK and SetKeyProcHashWithAs(tmpBoolean);

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherIterationCount, 0);
    allOK := allOK and SetKeyProcCypherIterationCount(tmpInteger);

    // Only pay attention to errors in getting the cypher if:
    // 1) We can get the cypher iteration count
    // 2) The cypher iteration count greater than zero
    ignoreIfCantSet := TRUE;
    if (GetKeyProcCypherIterationCount(tmpInteger)) then
      begin
      if (tmpInteger > 0) then
        begin
        ignoreIfCantSet := FALSE;
        end;
      end;
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherGUID, NULL_GUID);
    allOK := allOK and (SetKeyProcCypher(tmpStringName, StringToGUID(tmpStringGUID)) OR ignoreIfCantSet);


    // -------
    // Encryption options...
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainCypherGUID, NULL_GUID);
    allOK := allOK and SetMainCypher(tmpStringName, StringToGUID(tmpStringGUID));

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVGenMethod, FreeOTFESectorIVGenMethodID[foivgUnknown]);
    found := FALSE;
    for tmpSectorIVGenMethod:=low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do
      begin
      if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = tmpInteger) then
        begin
        allOK := allOK and SetMainSectorIVGenMethod(tmpSectorIVGenMethod);
        found := TRUE;
        break;
        end;
      end;
    allOK := allOK and found;


    // Ignore errors in the IV's sector zero location if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a sector zero location
    ignoreIfCantSet := TRUE;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
      begin
      if (SCTRIVGEN_USES_SECTOR_ID[tmpSectorIVGenMethod]) then
        begin
        ignoreIfCantSet := FALSE;
        end;
      end;
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_UNSET);
    startOfVolFile := FALSE;
    startOfEndData := FALSE;
    if (tmpString = IV_SECTOR_ID_START_ENCRYPTED_DATA) then
      begin
      startOfEndData := TRUE;
      end
    else if (tmpString = IV_SECTOR_ID_START_ENCRYPTED_VOLUME) then
      begin
      startOfVolFile := TRUE;
      end;
    allOK := allOK and (SetMainIVSectorZeroPos(startOfVolFile, startOfEndData) OR ignoreIfCantSet);

    // Ignore errors in the IV's hash algorithm if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a hash algorithm
    ignoreIfCantSet := TRUE;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
      begin
      if (SCTRIVGEN_USES_HASH[tmpSectorIVGenMethod]) then
        begin
        ignoreIfCantSet := FALSE;
        end;
      end;
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVHashKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVHashGUID, NULL_GUID);
    allOK := allOK and (SetMainIVHash(tmpStringName, StringToGUID(tmpStringGUID)) OR ignoreIfCantSet);

    // Ignore errors in the IV's cypher algorithm if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a cypher
    ignoreIfCantSet := TRUE;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
      begin
      if (SCTRIVGEN_USES_CYPHER[tmpSectorIVGenMethod]) then
        begin
        ignoreIfCantSet := FALSE;
        end;
      end;
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVCypherGUID, NULL_GUID);
    allOK := allOK and (SetMainIVCypher(tmpStringName, StringToGUID(tmpStringGUID)) OR ignoreIfCantSet);


    // -------
    // File options...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Offset, '0');
    allOK := allOK and SetOffset(strtoint64(tmpString));

    tmpString := settingsFile.ReadString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Size, '0');
    allOK := allOK and SetSizeLimit(strtoint64(tmpString));


    // -------
    // Mount options...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_DriveLetter, '-');
    // Just in case we attempt to get the 1st char from an empty string...
    if (tmpString = '') then
      begin
      tmpString := #0;
      end
    // Decode a stored "Use default"
    else if (tmpString = '-') then
      begin
      tmpString := #0;
      end;
    allOK := allOK and SetDriveLetter(AnsiChar(tmpString[1]));

    tmpBoolean := settingsFile.ReadBool(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_Readonly, FALSE);
    allOK := allOK and SetReadonly(tmpBoolean);

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_MountAs, ord(fomaFixedDisk));
    allOK := SetMountAs(TFreeOTFEMountAs(tmpInteger));


  finally
    settingsFile.Free();
  end;

  if not(allOK) then
    begin
    SDUMessageDlg(
                  _('One or more of your settings may not have loaded correctly; please check settings before continuing.'),
                  mtWarning
                 );
    end;

  EnableDisableControls();

end;


procedure TfrmKeyEntryLinux.pbSaveClick(Sender: TObject);
var
  settingsFile: TINIFile;
  tmpString: string;
    tmpAnsiString: Ansistring;
  tmpBoolean: boolean;
  tmpGUID: TGUID;
  tmpInteger: integer;
  tmpInt64: int64;
  tmpChar: ansichar;
  startOfVolFile, startOfEndData: boolean;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  tmpMountAs: TFreeOTFEMountAs;
begin
  if (SaveSettingsFileDlg.Execute) then
    begin
    settingsFile:= TINIFile.Create(SaveSettingsFileDlg.Filename);
    try
      // -------
      // Key processing...
      if (GetKeyProcSeed(tmpAnsiString)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcSeed, tmpAnsiString);
        end;

      if (GetKeyProcHashKernelDeviceName(tmpString)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashKernelDeviceName, tmpString);
        end;

      if (GetKeyProcHashGUID(tmpGUID)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashGUID, GUIDToString(tmpGUID));
        end;

      if (GetKeyProcHashWithAs(tmpBoolean)) then
        begin
        settingsFile.WriteBool(SETTINGS_SECTION_KEY, SETTINGS_VALUE_HashWithAs, tmpBoolean);
        end;

      if (GetKeyProcCypherIterationCount(tmpInteger)) then
        begin
        settingsFile.WriteInteger(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherIterationCount, tmpInteger);
        end;

      // Only store the key processing cypher if the key is to be processed
      // with > 0 iterations of the cypher
      if (GetKeyProcCypherIterationCount(tmpInteger)) then
        begin
        if (tmpInteger > 0) then
          begin
          if (GetKeyProcCypherKernelDeviceName(tmpString)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherKernelDeviceName, tmpString);
            end;

          if (GetKeyProcCypherGUID(tmpGUID)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherGUID, GUIDToString(tmpGUID));
            end;
          end;
        end;


      // -------
      // Encryption options...
      if (GetMainCypherKernelDeviceName(tmpAnsiString)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainCypherKernelDeviceName, tmpAnsiString);
        end;

      if (GetMainCypherGUID(tmpGUID)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainCypherGUID, GUIDToString(tmpGUID));
        end;

      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
        begin
        settingsFile.WriteInteger(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVGenMethod, FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod]);
        end;

      // Only store the IV sector zero location if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
        begin
        if (SCTRIVGEN_USES_SECTOR_ID[tmpSectorIVGenMethod]) then
          begin
          if (GetMainIVSectorZeroPos(startOfVolFile, startOfEndData)) then
            begin
            if (startOfVolFile) then
              begin
              settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_ENCRYPTED_VOLUME);
              end
            else if (startOfEndData) then
              begin
              settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_ENCRYPTED_DATA);
              end
            else
              begin
              settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_UNSET);
              end;

            end;
          end;
        end;

      // Only store the IV hash if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
        begin
        if (SCTRIVGEN_USES_HASH[tmpSectorIVGenMethod]) then
          begin
          if (GetMainIVHashKernelDeviceName(tmpAnsiString)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVHashKernelDeviceName, tmpAnsiString);
            end;

          if (GetMainIVHashGUID(tmpGUID)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVHashGUID, GUIDToString(tmpGUID));
            end;
          end;
        end;

      // Only store the IV cypher if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then
        begin
        if (SCTRIVGEN_USES_CYPHER[tmpSectorIVGenMethod]) then
          begin
          if (GetMainIVCypherKernelDeviceName(tmpAnsiString)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVCypherKernelDeviceName, tmpAnsiString);
            end;

          if (GetMainIVCypherGUID(tmpGUID)) then
            begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainIVCypherGUID, GUIDToString(tmpGUID));
            end;
          end;
        end;


      // -------
      // File options...
      if (GetOffset(tmpInt64)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Offset, inttostr(tmpInt64));
        end;

      if (GetSizeLimit(tmpInt64)) then
        begin
        settingsFile.WriteString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Size, inttostr(tmpInt64));
        end;


      // -------
      // Mount options...
      if (GetDriveLetter(tmpChar)) then
        begin
        // If the user selected "Use default", we encode the #0 to something
        // we can store
        if (tmpChar = #0) then
          begin
          tmpChar := '-'
          end;
        settingsFile.WriteString(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_DriveLetter, tmpChar);
        end;

      if (GetReadonly(tmpBoolean)) then
        begin
        settingsFile.WriteBool(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_Readonly, tmpBoolean);
        end;

      if (GetMountAs(tmpMountAs)) then
        begin
        settingsFile.WriteInteger(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_MountAs, ord(tmpMountAs));
        end;


    finally
      settingsFile.Free();
    end;

    end;

end;


procedure TfrmKeyEntryLinux.pbIVCypherInfoClick(Sender: TObject);
var
  deviceName: Ansistring;
  GUID: TGUID;
begin
  GetMainIVCypherKernelDeviceName(deviceName);
  GetMainIVCypherGUID(GUID);
  FreeOTFEObj.ShowCypherDetailsDlg(deviceName, GUID);

end;

END.


