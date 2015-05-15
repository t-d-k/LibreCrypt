unit frmKeyEntryLinux;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

//this is for new and existing dmcrypt volumes - not LUKS


interface

uses
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Messages,
  PasswordRichEdit,
  StdCtrls, SysUtils, Windows, // Required for TFreeOTFEMountAs
  Spin64,
  //SDU
  lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral,
  //LibreCrypt
  DriverAPI, OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUSpin64Units, SDUStdCtrls, SDUDialogs;
// Required for TFreeOTFESectorIVGenMethod and NULL_GUID

type
  TfrmKeyEntryPlainLinux = class (TSDUForm)
    pbCancel:       TButton;
    pbOK:           TButton;
    pcEntry:        TPageControl;
    tsKey:          TTabSheet;
    tsEncryption:   TTabSheet;
    tsFileOptions:  TTabSheet;
    tsMountOptions: TTabSheet;
    GroupBox1:      TGroupBox;
    Label1:         TLabel;
    Label16:        TLabel;
    Label15:        TLabel;
    Label14:        TLabel;
    Label21:        TLabel;
    GroupBox5:      TGroupBox;
    Label2:         TLabel;
    Label6:         TLabel;
    Label13:        TLabel;
    Label20:        TLabel;
    Label3:         TLabel;
    Label7:         TLabel;
    Label18:        TLabel;
    Label19:        TLabel;
    cbKeyProcHash:  TComboBox;
    seKeyProcCypherIterations: TSpinEdit64;
    pbKeyProcHashInfo: TButton;
    pbKeyProcCypherInfo: TButton;
    cbKeyProcCypher: TComboBox;
    edKeySeed:      TEdit;
    GroupBox3:      TGroupBox;
    Label23:        TLabel;
    Label24:        TLabel;
    cbMainCypher:   TComboBox;
    pbMainCypherInfo: TButton;
    GroupBox4:      TGroupBox;
    Label8:         TLabel;
    Label9:         TLabel;
    Label10:        TLabel;
    Label11:        TLabel;
    GroupBox2:      TGroupBox;
    lblDrive:       TLabel;
    lblReadOnlySwitch: TLabel;
    cbDrive:        TComboBox;
    ckMountReadonly: TSDUCheckBox;
    preUserkey:     TOTFEFreeOTFE_PasswordRichEdit;
    pbLoad:         TButton;
    pbSave:         TButton;
    OpenSettingsFileDlg: TSDUOpenDialog;
    SaveSettingsFileDlg: TSDUSaveDialog;
    Label22:        TLabel;
    GroupBox6:      TGroupBox;
    Label5:         TLabel;
    cbSectorIVGenMethod: TComboBox;
    lblIVHash:      TLabel;
    Label12:        TLabel;
    cbSectorIVHash: TComboBox;
    pbIVHashInfo:   TButton;
    rgSectorIVSectorZeroPos: TRadioGroup;
    ckHashWithAs:   TSDUCheckBox;
    lblIVCypher:    TLabel;
    cbSectorIVCypher: TComboBox;
    pbIVCypherInfo: TButton;
    lblMountAs:     TLabel;
    cbMediaType:    TComboBox;
    ckMountForAllUsers: TSDUCheckBox;
    se64UnitOffset: TSDUSpin64Unit_Storage;
    se64UnitSizeLimit: TSDUSpin64Unit_Storage;
    feGPGExecutable: TSDUFilenameEdit;
    feGPGKeyfile:   TSDUFilenameEdit;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
    procedure pbKeyProcHashInfoClick(Sender: TObject);
    procedure pbKeyProcCypherInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    fHashKernelModeDriverNames: TStringList;
    fHashGUIDs:   TStringList;
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDs: TStringList;

    procedure PopulateHashes();
    procedure PopulateCyphers();
    procedure PopulateSectorIVGenMethods();
    procedure PopulateDrives();
    procedure PopulateMountAs();

    procedure DefaultOptions();

    procedure EnableDisableControls();

    procedure DoCancel();

  public
    //    fFreeOTFEObj: TOTFEFreeOTFEBase;

    // Key...
    procedure GetKey(var userKey: Ansistring);
    procedure SetKey(userKey: PasswordString);

    // Key processing...
    procedure GetKeyProcSeed(var keyProcSeed: Ansistring);
    procedure SetKeyProcSeed(keyProcSeed: String);
    function GetKeyProcHashKernelDeviceName(var keyProcHashDriver: String): Boolean;
    function GetKeyProcHashGUID(var keyProcHashGUID: TGUID): Boolean;
    function SetKeyProcHash(keyProcHashDriver: String; keyProcHashGUID: TGUID): Boolean;
    procedure GetKeyProcHashWithAs(var hashWithAs: Boolean);
    procedure SetKeyProcHashWithAs(hashWithAs: Boolean);
    function GetKeyProcCypherKernelDeviceName(var keyProcCypherDriver: String): Boolean;
    function GetKeyProcCypherGUID(var keyProcCypherGUID: TGUID): Boolean;
    function SetKeyProcCypher(keyProcCypherDriver: String; keyProcCypherGUID: TGUID): Boolean;
    // Note: GetKeyProcCypherIterationCount returns the number of iterations
    //       required; if the user entered "2" on the dialog, then 2000 will
    //       be returned by this function
    procedure GetKeyProcCypherIterationCount(out keyProcCypherIterations: Integer);
    procedure SetKeyProcCypherIterationCount(keyProcCypherIterations: Integer);

    // File options...
    procedure GetOffset(out fileOptoffset: Int64);
    procedure SetOffset(fileOptoffset: Int64);
    procedure GetSizeLimit(out fileOptSizeLimit: Int64);
    procedure SetSizeLimit(fileOptSizeLimit: Int64);

    // Encryption options...
    function GetMainCypherKernelDeviceName(var mainCypherDriver: String): Boolean;
    function GetMainCypherGUID(var mainCypherGUID: TGUID): Boolean;
    function SetMainCypher(mainCypherDriver: String; mainCypherGUID: TGUID): Boolean;
    procedure GetMainIVSectorZeroPos(out startOfVolFile: Boolean; out startOfEndData: Boolean);
    // Note: At most *one* of the parameters passed to SetMainIVSectorZeroPos(...) can be set to TRUE
    function SetMainIVSectorZeroPos(startOfVolFile: Boolean; startOfEndData: Boolean): Boolean;
    function GetMainSectorIVGenMethod(var sectorIVGenMethod: TFreeOTFESectorIVGenMethod): Boolean;
    function SetMainSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod): Boolean;
    function GetMainIVHashKernelDeviceName(var mainIVHashDriver: String): Boolean;
    function GetMainIVHashGUID(var mainIVHashGUID: TGUID): Boolean;
    function SetMainIVHash(mainIVHashDriver: String; mainIVHashGUID: TGUID): Boolean;
    function GetMainIVCypherKernelDeviceName(var mainIVCypherDriver: Ansistring): Boolean;
    function GetMainIVCypherGUID(var mainIVCypherGUID: TGUID): Boolean;
    function SetMainIVCypher(mainIVCypherDriver: String; mainIVCypherGUID: TGUID): Boolean;

    // Mount options...
    procedure GetDriveLetter(out mountDriveLetter: DriveLetterChar);
    function SetDriveLetter(mountDriveLetter: DriveLetterChar): Boolean;
    procedure GetReadonly(var mountReadonly: Boolean);
    procedure SetReadonly(mountReadonly: Boolean);
    function GetMountAs(var mountAs: TFreeOTFEMountAs): Boolean;
    function SetMountAs(mountAs: TFreeOTFEMountAs): Boolean;
    function GetMountForAllUsers(): Boolean;
    procedure SetMountForAllUsers(allUsers: Boolean);

    procedure Initialize();
    procedure LoadSettings(filename: String);

  end;


implementation

{$R *.DFM}


uses
  ComObj,                      // Required for StringToGUID
  VolumeFileAPI,               // Required for SCTRIVGEN_USES_SECTOR_ID and SCTRIVGEN_USES_HASH
  INIFiles, OTFEFreeOTFEDLL_U,
  //sdu
  SDUi18n;

const

  // If they're available, use these as defaults...
  //  DEFAULT_KEYPROC_HASH_TITLE    = 'SHA-512 (512/1024)';
  //  DEFAULT_KEYPROC_CYPHER_TITLE  = 'AES (CBC; 256/128)';
  //  DEFAULT_MAIN_CYPHER_TITLE     = 'AES (CBC; 256/128)';
  //  DEFAULT_MAIN_IV_HASH_TITLE    = 'MD5 (128/512)';
  DEFAULT_KEYPROC_HASH_TITLE   = 'SHA-512';
  DEFAULT_KEYPROC_CYPHER_TITLE = 'AES (256 bit CBC)';
  DEFAULT_MAIN_CYPHER_TITLE    = 'AES (256 bit CBC)';
  DEFAULT_MAIN_IV_HASH_TITLE   = 'SHA-256';     //tdk change - make consistent with linux scripts
  DEFAULT_MAIN_IV_GEN_METHOD   = 'ESSIV';       //tdk change - make consistent with linux scripts

  // Settings file sections and values
  SETTINGS_SECTION_KEY               = 'Key';
  SETTINGS_VALUE_KeyProcSeed         = 'KeyProcSeed';
  SETTINGS_VALUE_KeyProcHashKernelDeviceName = 'KeyProcHashKernelDeviceName';
  SETTINGS_VALUE_KeyProcHashGUID     = 'KeyProcHashGUID';
  SETTINGS_VALUE_HashWithAs          = 'KeyProcHashWithAs';
  SETTINGS_VALUE_KeyProcCypherKernelDeviceName = 'KeyProcCypherKernelDeviceName';
  SETTINGS_VALUE_KeyProcCypherGUID   = 'KeyProcCypherGUID';
  SETTINGS_VALUE_KeyProcCypherIterationCount = 'KeyProcCypherIterationCount';
  SETTINGS_SECTION_ENCRYPTION        = 'Encryption';
  SETTINGS_VALUE_MainCypherKernelDeviceName = 'MainCypherKernelDeviceName';
  SETTINGS_VALUE_MainCypherGUID      = 'MainCypherGUID';
  SETTINGS_VALUE_MainIVGenMethod     = 'MainIVGenMethod';
  SETTINGS_VALUE_MainIVSectorZeroPos = 'MainIVSectorZeroPos';
  SETTINGS_VALUE_MainIVHashKernelDeviceName = 'MainIVHashKernelDeviceName';
  SETTINGS_VALUE_MainIVHashGUID      = 'MainIVHashGUID';
  SETTINGS_VALUE_MainIVCypherKernelDeviceName = 'MainIVCypherKernelDeviceName';
  SETTINGS_VALUE_MainIVCypherGUID    = 'MainIVCypherGUID';
  SETTINGS_SECTION_FILE_OPTIONS      = 'FileOptions';
  SETTINGS_VALUE_Offset              = 'Offset';
  SETTINGS_VALUE_Size                = 'Size';
  SETTINGS_SECTION_MOUNT_OPTIONS     = 'MountOptions';
  SETTINGS_VALUE_DriveLetter         = 'DriveLetter';
  SETTINGS_VALUE_Readonly            = 'Readonly';
  SETTINGS_VALUE_MountAs             = 'MountAsEnum';
  // Previously "MountAs" - a string


  // Flags to indicate if sector IDs used for IVs start at the beginning of the
  // file, or the start of the encrypted data
  // These are CONSTS - not RESOURCESTRINGS - they're stored in the .ini file
  IV_SECTOR_ID_START_UNSET            = 'Unset';
  IV_SECTOR_ID_START_ENCRYPTED_DATA   = 'EncryptedData';
  IV_SECTOR_ID_START_ENCRYPTED_VOLUME = 'File';

resourcestring
  FILTER_GPG_FILES = 'GPG files|*.gpg|All files|*.*';
  FILTER_GPG_EXE   = 'GPG Executable|GPG.exe|All files|*.*';

procedure TfrmKeyEntryPlainLinux.FormCreate(Sender: TObject);
begin
  fHashKernelModeDriverNames   := TStringList.Create();
  fHashGUIDs                   := TStringList.Create();
  fCypherKernelModeDriverNames := TStringList.Create();
  fCypherGUIDs                 := TStringList.Create();
  pcEntry.ActivePage := tsKey;
end;


procedure TfrmKeyEntryPlainLinux.PopulateHashes();
var
  tmpDisplayTitles: TStringList;
begin
  tmpDisplayTitles := TStringList.Create();
  try
    if (GetFreeOTFEBase().GetHashList(tmpDisplayTitles, fHashKernelModeDriverNames, fHashGUIDs))
    then begin
      cbKeyProcHash.Items.Clear();
      cbKeyProcHash.Items.AddStrings(tmpDisplayTitles);

      cbSectorIVHash.Items.Clear();
      cbSectorIVHash.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of hashes.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE hash drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed FreeOTFE, you may need to restart your computer.'),
        mtError
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmKeyEntryPlainLinux.PopulateCyphers();
var
  tmpDisplayTitles: TStringList;
begin
  tmpDisplayTitles := TStringList.Create();
  try
    if (GetFreeOTFEBase().GetCypherList(tmpDisplayTitles, fCypherKernelModeDriverNames,
      fCypherGUIDs)) then begin
      cbKeyProcCypher.Items.Clear();
      cbKeyProcCypher.Items.AddStrings(tmpDisplayTitles);

      cbMainCypher.Items.Clear();
      cbMainCypher.Items.AddStrings(tmpDisplayTitles);

      cbSectorIVCypher.Items.Clear();
      cbSectorIVCypher.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed FreeOTFE, you may need to restart your computer.'),
        mtError
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmKeyEntryPlainLinux.PopulateSectorIVGenMethods();
var
  currMethod: TFreeOTFESectorIVGenMethod;
begin
  cbSectorIVGenMethod.Items.Clear();
  for currMethod := low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do begin
    // Skip "Unknown"
    if (currMethod <> foivgUnknown) then begin
      cbSectorIVGenMethod.Items.AddObject(FreeOTFESectorIVGenMethodTitle[currMethod],
        Pointer(Ord(currMethod)));
    end;
  end;

end;


procedure TfrmKeyEntryPlainLinux.PopulateDrives();
var
  driveLetters: DriveLetterString;
  i:            Integer;
begin
  cbDrive.Items.Clear();
  cbDrive.Items.Add(_('Use default'));
  driveLetters := SDUGetUnusedDriveLetters();
  for i := 1 to length(driveLetters) do begin
    // Skip the drive letters traditionally reserved for floppy disk drives
    //    if (
    //        (driveLetters[i] <> 'A') AND
    //        (driveLetters[i] <> 'B')
    //       ) then
    //      begin
    cbDrive.Items.Add(driveLetters[i] + ':');
    //      end;
  end;

end;


procedure TfrmKeyEntryPlainLinux.PopulateMountAs();
var
  currMountAs: TFreeOTFEMountAs;
begin
  cbMediaType.Items.Clear();
  for currMountAs := low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    // do not allow dm-crypt vols as 'fixed discs' - work-around BSoD issue
    if not (currMountAs in [fomaUnknown, fomaFixedDisk]) then
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
end;


procedure TfrmKeyEntryPlainLinux.DefaultOptions();
var
  idx:             Integer;
  i:               Integer;
  currDriveLetter: DriveLetterChar;
begin
  // Autoselect default, if available
  if (cbKeyProcHash.Items.Count > 0) then begin
    idx := cbKeyProcHash.Items.IndexOf(DEFAULT_KEYPROC_HASH_TITLE);
    if (idx = -1) then
      idx := 0;

    cbKeyProcHash.ItemIndex := idx;
  end;


  // Autoselect default, if available
  if (cbKeyProcCypher.Items.Count > 0) then begin
    idx := cbKeyProcCypher.Items.IndexOf(DEFAULT_KEYPROC_CYPHER_TITLE);
    if (idx = -1) then
      idx := 0;

    cbKeyProcCypher.ItemIndex := idx;
  end;


  ckHashWithAs.Checked := True;

  // Autoselect default, if available
  if (cbMainCypher.Items.Count > 0) then begin
    idx := cbMainCypher.Items.IndexOf(DEFAULT_MAIN_CYPHER_TITLE);
    if (idx = -1) then
      idx := 0;

    cbMainCypher.ItemIndex := idx;
  end;

  // Autoselect default, if available
  if (cbSectorIVCypher.Items.Count > 0) then begin
    idx := cbSectorIVCypher.Items.IndexOf(DEFAULT_MAIN_CYPHER_TITLE);
    if (idx = -1) then begin
      idx := 0;
    end;

    cbSectorIVCypher.ItemIndex := idx;
  end;


  // Autoselect default, if available
  if (cbSectorIVHash.Items.Count > 0) then begin
    idx := cbSectorIVHash.Items.IndexOf(DEFAULT_MAIN_IV_HASH_TITLE);
    if (idx = -1) then begin
      idx := 0;
    end;

    cbSectorIVHash.ItemIndex := idx;
  end;

  //tdk change - add default iv gen method of essiv = linux script default
  if (cbSectorIVGenMethod.Items.Count > 0) then begin
    idx := cbSectorIVGenMethod.Items.IndexOf(DEFAULT_MAIN_IV_GEN_METHOD);
    if (idx = -1) then begin
      idx := 0;
    end;

    cbSectorIVGenMethod.ItemIndex := idx;
  end;


  if (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
    if (cbDrive.Items.Count > 0) then begin
      cbDrive.ItemIndex := 0;

      if (GetFreeOTFE().DefaultDriveLetter <> #0) then begin
        // Start from 1; skip the default
        for i := 1 to (cbDrive.items.Count - 1) do begin
          currDriveLetter := cbDrive.Items[i][1];
          if (currDriveLetter >= GetFreeOTFE().DefaultDriveLetter) then begin
            cbDrive.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;
  end;

  if (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
    SetMountAs(GetFreeOTFE().DefaultMountAs);
  end else begin
    SetMountAs(fomaRemovableDisk);
  end;

  preUserKey.Plaintext   := True;
  // Linux volumes CAN NOT have newlines in the user's password
  preUserKey.WantReturns := False;
  preUserKey.WordWrap    := True;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := GetFreeOTFEBase().PasswordChar;
  preUserKey.WantReturns  := GetFreeOTFEBase().AllowNewlinesInPasswords;
  preUserKey.WantTabs     := GetFreeOTFEBase().AllowTabsInPasswords;

  feGPGExecutable.Filename := '';
  feGPGKeyfile.Filename    := '';

  edKeySeed.Text                  := '';
  seKeyProcCypherIterations.Value := 0;

  se64UnitSizeLimit.Value := 0;
  se64UnitOffset.Value    := 0;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.Checked := True;

end;


procedure TfrmKeyEntryPlainLinux.pbOKClick(Sender: TObject);
var
  tmpKey: Ansistring;
begin
  GetKey(tmpKey);
  if (Length(tmpKey) = 0) then begin
    if (SDUMessageDlg(_('You have not entered a Keyphrase.') + SDUCRLF +
      SDUCRLF + _('Are you sure you wish to proceed?'), mtConfirmation, [mbYes, mbNo], 0) =
      mrYes) then begin
      ModalResult := mrOk;
    end;
  end else
  if (Length(tmpKey) < 20) then begin
    if (SDUMessageDlg(_(
      'The Keyphrase you entered has less than 20 characters, and may not be compatible with some Linux volumes.')
      + SDUCRLF + SDUCRLF + _('Do you wish to proceed?'), mtWarning, [mbYes, mbNo], 0) =
      mrYes) then begin
      ModalResult := mrOk;
    end;
  end else begin
    // No problems with the password as entered; OK to close the dialog
    ModalResult := mrOk;
  end;

end;


procedure TfrmKeyEntryPlainLinux.EnableDisableControls();
var
  junkInt64:             Int64;
  filePresentGPGExe:     Boolean;
  filePresentGPGKeyfile: Boolean;
  iterationCypherOK:     Boolean;
  IVStartSectorOK:       Boolean;
  junkChar:              DriveLetterChar;
  IVHashOK:              Boolean;
  IVCypherOK:            Boolean;
  mountAsOK:             Boolean;
  sectorIVGenMethod:     TFreeOTFESectorIVGenMethod;
  tmpMountAs:            TFreeOTFEMountAs;
begin
  SDUEnableControl(cbKeyProcCypher, (seKeyProcCypherIterations.Value > 0));

  SDUEnableControl(rgSectorIVSectorZeroPos, False);
  SDUEnableControl(cbSectorIVHash, False);
  SDUEnableControl(cbSectorIVCypher, False);
  sectorIVGenMethod := foivgUnknown;
  if GetMainSectorIVGenMethod(sectorIVGenMethod) then begin
    SDUEnableControl(rgSectorIVSectorZeroPos, SCTRIVGEN_USES_SECTOR_ID[sectorIVGenMethod]);
    SDUEnableControl(cbSectorIVHash, SCTRIVGEN_USES_HASH[sectorIVGenMethod]);
    SDUEnableControl(cbSectorIVCypher, SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]);
  end;

  SDUEnableControl(pbKeyProcHashInfo, (cbKeyProcHash.ItemIndex >= 0));
  SDUEnableControl(pbKeyProcCypherInfo, (cbKeyProcCypher.ItemIndex >= 0) and
    (cbKeyProcCypher.Enabled));
  SDUEnableControl(pbMainCypherInfo, (cbMainCypher.ItemIndex >= 0));
  SDUEnableControl(pbIVHashInfo, (cbSectorIVHash.ItemIndex >= 0) and (cbSectorIVHash.Enabled));
  SDUEnableControl(pbIVCypherInfo, (cbSectorIVCypher.ItemIndex >= 0) and
    (cbSectorIVCypher.Enabled));

  SDUEnableControl(lblIVHash, cbSectorIVHash.Enabled);
  SDUEnableControl(lblIVCypher, cbSectorIVCypher.Enabled);

  filePresentGPGExe := True;
  if (feGPGExecutable.Filename <> '') then begin
    filePresentGPGExe := FileExists(feGPGExecutable.Filename);
  end;

  filePresentGPGKeyfile := True;
  if (feGPGKeyfile.Filename <> '') then begin
    filePresentGPGKeyfile := FileExists(feGPGKeyfile.Filename);
  end;

  iterationCypherOK := True;
  if (seKeyProcCypherIterations.Value > 0) then begin
    iterationCypherOK := (cbKeyProcCypher.ItemIndex >= 0);
  end;


  // Ensure user specified sector ID zero location, if required
  IVStartSectorOK := True;
  if (rgSectorIVSectorZeroPos.Enabled) then begin
    IVStartSectorOK := (rgSectorIVSectorZeroPos.ItemIndex >= 0);
  end;


  // Ensure user selected a sector IV hash, if one is required
  IVHashOK := True;
  if (cbSectorIVHash.Enabled) then begin
    IVHashOK := (cbSectorIVHash.ItemIndex >= 0);
  end;

  // Ensure user selected a sector IV hash, if one is required
  IVCypherOK := True;
  if (cbSectorIVCypher.Enabled) then begin
    IVCypherOK := (cbSectorIVCypher.ItemIndex >= 0);
  end;


  // Ensure we know what to mount as
  mountAsOK               := GetMountAs(tmpMountAs);
  ckMountReadonly.Enabled := False;
  if (mountAsOK) then begin
    if not (FreeOTFEMountAsCanWrite[tmpMountAs]) then begin
      ckMountReadonly.Checked := True;
    end;
    SDUEnableControl(ckMountReadonly, FreeOTFEMountAsCanWrite[tmpMountAs]);
  end;
  mountAsOK := mountAsOK and (tmpMountAs <> fomaUnknown);


  GetOffset(junkInt64);
  GetSizeLimit(junkInt64);
  GetDriveLetter(junkChar);
  pbOK.Enabled := ((filePresentGPGExe) and (filePresentGPGKeyfile) and
    (cbKeyProcHash.ItemIndex >= 0) and iterationCypherOK and
    (cbMainCypher.ItemIndex >= 0) and (sectorIVGenMethod <> foivgUnknown) and
    IVStartSectorOK and IVHashOK and IVCypherOK and mountAsOK);

  // Items NOT YET IMPLEMENTED
  //  - after implementing; DON'T FORGET TO ENABLE THE ASSOCIATED LABELS!
  SDUEnableControl(feGPGExecutable, False);
  SDUEnableControl(feGPGKeyfile, False);
  SDUEnableControl(cbKeyProcCypher, False);
  SDUEnableControl(pbKeyProcCypherInfo, False);
  SDUEnableControl(seKeyProcCypherIterations, False);

end;

procedure TfrmKeyEntryPlainLinux.SelectionChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryPlainLinux.pbKeyProcHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetKeyProcHashKernelDeviceName(deviceName);
  GetKeyProcHashGUID(GUID);
  GetFreeOTFEBase().ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmKeyEntryPlainLinux.pbIVHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetMainIVHashKernelDeviceName(deviceName);
  GetMainIVHashGUID(GUID);
  GetFreeOTFEBase().ShowHashDetailsDlg(deviceName, GUID);
end;


procedure TfrmKeyEntryPlainLinux.pbKeyProcCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetKeyProcCypherKernelDeviceName(deviceName);
  GetKeyProcCypherGUID(GUID);
  GetFreeOTFEBase().ShowCypherDetailsDlg(deviceName, GUID);

end;

procedure TfrmKeyEntryPlainLinux.pbMainCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetMainCypherKernelDeviceName(deviceName);
  GetMainCypherGUID(GUID);
  GetFreeOTFEBase().ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmKeyEntryPlainLinux.Initialize();
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

procedure TfrmKeyEntryPlainLinux.FormShow(Sender: TObject);
begin
  feGPGExecutable.Filter             := FILTER_GPG_EXE;
  feGPGKeyfile.Filter                := FILTER_GPG_FILES;
  feGPGExecutable.OpenDialog.Options := feGPGExecutable.OpenDialog.Options + [ofDontAddToRecent];
  feGPGExecutable.SaveDialog.Options := feGPGExecutable.SaveDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.OpenDialog.Options    := feGPGKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.SaveDialog.Options    := feGPGKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);

  // Certain controls only visble if used in conjunction with drive mounting
  lblDrive.Visible           := GetFreeOTFEBase() is TOTFEFreeOTFE;
  cbDrive.Visible            := GetFreeOTFEBase() is TOTFEFreeOTFE;
  lblMountAs.Visible         := GetFreeOTFEBase() is TOTFEFreeOTFE;
  cbMediaType.Visible        := GetFreeOTFEBase() is TOTFEFreeOTFE;
  ckMountForAllUsers.Visible := GetFreeOTFEBase() is TOTFEFreeOTFE;

  // Prevent making remaining control look odd, stuck in the middle
  if not (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
    ckMountReadonly.top  := lblDrive.top;
    ckMountReadonly.left := lblDrive.left;

    lblReadOnlySwitch.top := ckMountReadonly.top;
  end;

  EnableDisableControls();
  preUserkey.SetFocus;

end;


procedure TfrmKeyEntryPlainLinux.FormDestroy(Sender: TObject);
begin
  fHashKernelModeDriverNames.Free();
  fHashGUIDs.Free();
  fCypherKernelModeDriverNames.Free();
  fCypherGUIDs.Free();

end;


procedure TfrmKeyEntryPlainLinux.preUserkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then begin
    DoCancel();
  end;

end;

procedure TfrmKeyEntryPlainLinux.pbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TfrmKeyEntryPlainLinux.DoCancel();
begin
  ModalResult := mrCancel;
end;


procedure TfrmKeyEntryPlainLinux.GetKey(var userKey: Ansistring);
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  userKey := preUserkey.Text;
end;

procedure TfrmKeyEntryPlainLinux.SetKey(userKey: PasswordString);
begin
  preUserkey.Text := userKey;
end;

procedure TfrmKeyEntryPlainLinux.GetKeyProcSeed(var keyProcSeed: Ansistring);
begin
  keyProcSeed := edKeySeed.Text;   { TODO 1 -otdk -cclean : allow unicode }
end;

procedure TfrmKeyEntryPlainLinux.SetKeyProcSeed(keyProcSeed: String);
begin
  edKeySeed.Text := keyProcSeed;
end;

function TfrmKeyEntryPlainLinux.GetKeyProcHashKernelDeviceName(
  var keyProcHashDriver: String): Boolean;
begin
  Result := False;

  if (cbKeyProcHash.ItemIndex >= 0) then begin
    keyProcHashDriver := fHashKernelModeDriverNames[cbKeyProcHash.ItemIndex];
    Result            := True;
  end;

end;

function TfrmKeyEntryPlainLinux.GetKeyProcHashGUID(var keyProcHashGUID: TGUID): Boolean;
begin
  Result := False;

  if (cbKeyProcHash.ItemIndex >= 0) then begin
    keyProcHashGUID := StringToGUID(fHashGUIDs[cbKeyProcHash.ItemIndex]);
    Result          := True;
  end;

end;


function TfrmKeyEntryPlainLinux.SetKeyProcHash(keyProcHashDriver: String;
  keyProcHashGUID: TGUID): Boolean;
var
  i: Integer;
begin
  cbKeyProcHash.ItemIndex := -1;

  // Locate the appropriate array idx
  for i := 0 to (fHashKernelModeDriverNames.Count - 1) do begin
       //  hashKernelModeDriverNames is dll name for explorer, else reg key so use 'or' GUIDs as well
       { TODO -otdk -cUI: are these GUIDs the same across PCs? if not use cypher name etc }
    if {((hashKernelModeDriverNames[i] = keyProcHashDriver) or}
    (fHashGUIDs[i] = GUIDToString(keyProcHashGUID)) then begin
      cbKeyProcHash.ItemIndex := i;
      break;
    end;

  end;

  Result := cbKeyProcHash.ItemIndex <> -1;
end;

procedure TfrmKeyEntryPlainLinux.GetKeyProcHashWithAs(var hashWithAs: Boolean);
begin
  hashWithAs := ckHashWithAs.Checked;
end;

procedure TfrmKeyEntryPlainLinux.SetKeyProcHashWithAs(hashWithAs: Boolean);
begin
  ckHashWithAs.Checked := hashWithAs;
end;


 // This returns TRUE if a cypher is selected, or if no cypher is needed (in
 // which case, an empty string is returned)
 // Returns FALSE is a cypher is required, but one is not selected
function TfrmKeyEntryPlainLinux.GetKeyProcCypherKernelDeviceName(
  var keyProcCypherDriver: String): Boolean;
begin
  Result := False;

  if (seKeyProcCypherIterations.Value > 0) then begin
    if (cbKeyProcCypher.ItemIndex >= 0) then begin
      keyProcCypherDriver := fCypherKernelModeDriverNames[cbKeyProcCypher.ItemIndex];
      Result              := True;
    end;
  end else begin
    // No cypher selected, but none needed
    keyProcCypherDriver := '';
    Result              := True;
  end;

end;

 // This returns TRUE if a cypher is selected, or if no cypher is needed (in
 // which case, NULL_GUID is returned)
 // Returns FALSE is a cypher is required, but one is not selected
function TfrmKeyEntryPlainLinux.GetKeyProcCypherGUID(var keyProcCypherGUID: TGUID): Boolean;
begin
  Result := False;

  if (seKeyProcCypherIterations.Value > 0) then begin
    if (cbKeyProcCypher.ItemIndex >= 0) then begin
      keyProcCypherGUID := StringToGUID(fCypherGUIDs[cbKeyProcCypher.ItemIndex]);
      Result            := True;
    end;
  end else begin
    // No cypher selected, but none needed
    keyProcCypherGUID := StringToGUID(NULL_GUID);
    Result            := True;
  end;

end;

function TfrmKeyEntryPlainLinux.SetKeyProcCypher(keyProcCypherDriver: String;
  keyProcCypherGUID: TGUID): Boolean;
var
  i: Integer;
begin
  cbKeyProcCypher.ItemIndex := -1;

  // Locate the appropriate array idx
  for i := 0 to (fCypherKernelModeDriverNames.Count - 1) do begin
       //  hashKernelModeDriverNames is dll name for explorer, else reg key so use 'or' GUIDs as well
       { TODO -otdk -cUI: are these GUIDs the same across PCs? if not use cypher name etc }
    if {((cypherKernelModeDriverNames[i] = keyProcCypherDriver) or}
    (fCypherGUIDs[i] = GUIDToString(keyProcCypherGUID)) then begin
      cbKeyProcCypher.ItemIndex := i;
      break;
    end;

  end;

  Result := cbKeyProcCypher.ItemIndex <> -1;
end;


procedure TfrmKeyEntryPlainLinux.GetKeyProcCypherIterationCount(
  out keyProcCypherIterations: Integer);
begin
  keyProcCypherIterations := seKeyProcCypherIterations.Value * 1000;
end;

procedure TfrmKeyEntryPlainLinux.SetKeyProcCypherIterationCount(keyProcCypherIterations: Integer);
begin
  // Note that if "1" passed in, this will be set to 0
  seKeyProcCypherIterations.Value := keyProcCypherIterations div 1000;

end;

procedure TfrmKeyEntryPlainLinux.GetOffset(out fileOptoffset: Int64);
begin
  fileOptoffset := se64UnitOffset.Value;
end;

procedure TfrmKeyEntryPlainLinux.SetOffset(fileOptoffset: Int64);
begin
  se64UnitOffset.Value := fileOptoffset;
end;

procedure TfrmKeyEntryPlainLinux.GetSizeLimit(out fileOptSizeLimit: Int64);
begin
  fileOptSizeLimit := se64UnitSizeLimit.Value;
end;

procedure TfrmKeyEntryPlainLinux.SetSizeLimit(fileOptSizeLimit: Int64);
begin
  se64UnitSizeLimit.Value := fileOptSizeLimit;
end;

// Note: This may return #0 as mountDriveLetter to indicate "any"
procedure TfrmKeyEntryPlainLinux.GetDriveLetter(out mountDriveLetter: DriveLetterChar);
begin
  mountDriveLetter := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex > 0) then begin
    mountDriveLetter := cbDrive.Items[cbDrive.ItemIndex][1];
  end;

end;

// mountDriveLetter - Set to #0 to indicate "Use default"
function TfrmKeyEntryPlainLinux.SetDriveLetter(mountDriveLetter: DriveLetterChar): Boolean;
var
  idx: Integer;
begin
  Result := True;

  if (mountDriveLetter = #0) then begin
    // The item at idx 0 will *always* be "Use default"
    idx := 0;
  end else begin
    idx := cbDrive.Items.IndexOf(mountDriveLetter + ':');
  end;

  if (idx < 0) then begin
    idx    := 0;
    Result := False;
  end;
  cbDrive.ItemIndex := idx;

end;

procedure TfrmKeyEntryPlainLinux.GetReadonly(var mountReadonly: Boolean);
begin
  mountReadonly := ckMountReadonly.Checked;
end;

procedure TfrmKeyEntryPlainLinux.SetReadonly(mountReadonly: Boolean);
begin
  ckMountReadonly.Checked := mountReadonly;
end;



function TfrmKeyEntryPlainLinux.GetMountAs(var mountAs: TFreeOTFEMountAs): Boolean;
var
  currMountAs: TFreeOTFEMountAs;
begin
  Result := False;

  for currMountAs := low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then begin
      mountAs := currMountAs;
      Result  := True;
      break;
    end;
  end;
end;


function TfrmKeyEntryPlainLinux.SetMountAs(mountAs: TFreeOTFEMountAs): Boolean;
begin
  cbMediaType.ItemIndex :=
    cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  Result                := cbMediaType.ItemIndex >= 0;
end;


procedure TfrmKeyEntryPlainLinux.SetMountForAllUsers(allUsers: Boolean);
begin
  ckMountForAllUsers.Checked := allUsers;
end;

function TfrmKeyEntryPlainLinux.GetMountForAllUsers(): Boolean;
begin
  Result := ckMountForAllUsers.Checked;
end;


function TfrmKeyEntryPlainLinux.GetMainCypherKernelDeviceName(
  var mainCypherDriver: String): Boolean;
begin
  Result := False;

  if (cbMainCypher.ItemIndex >= 0) then begin
    mainCypherDriver := fCypherKernelModeDriverNames[cbMainCypher.ItemIndex];
    // unicode->ansi not a data loss because only ansistring stored
    Result           := True;
  end;

end;

function TfrmKeyEntryPlainLinux.GetMainCypherGUID(var mainCypherGUID: TGUID): Boolean;
begin
  Result := False;

  if (cbMainCypher.ItemIndex >= 0) then begin
    mainCypherGUID := StringToGUID(fCypherGUIDs[cbMainCypher.ItemIndex]);
    Result         := True;
  end;

end;

function TfrmKeyEntryPlainLinux.SetMainCypher(mainCypherDriver: String;
  mainCypherGUID: TGUID): Boolean;
var
  i: Integer;
begin
  cbMainCypher.ItemIndex := -1;

  // Locate the appropriate array idx
  for i := 0 to (fCypherKernelModeDriverNames.Count - 1) do begin
       //  hashKernelModeDriverNames is dll name for explorer, else reg key so use 'or' GUIDs as well
       { TODO -otdk -cUI: are these GUIDs the same across PCs? if not use cypher name etc }
    if {(cypherKernelModeDriverNames[i] = mainCypherDriver) or}
    (fCypherGUIDs[i] = GUIDToString(mainCypherGUID)) then begin
      cbMainCypher.ItemIndex := i;
      break;
    end;

  end;

  Result := cbMainCypher.ItemIndex <> -1;
end;

function TfrmKeyEntryPlainLinux.GetMainSectorIVGenMethod(
  var sectorIVGenMethod: TFreeOTFESectorIVGenMethod): Boolean;
begin
  Result := False;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then begin
    sectorIVGenMethod := TFreeOTFESectorIVGenMethod(
      cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
    Result            := True;
  end;

end;

function TfrmKeyEntryPlainLinux.SetMainSectorIVGenMethod(sectorIVGenMethod:
  TFreeOTFESectorIVGenMethod): Boolean;
var
  i: Integer;
begin
  cbSectorIVGenMethod.ItemIndex := -1;

  // Locate the appropriate idx
  for i := 0 to (cbSectorIVGenMethod.Items.Count - 1) do begin
    if (TFreeOTFESectorIVGenMethod(cbSectorIVGenMethod.Items.Objects[i]) = sectorIVGenMethod) then
    begin
      cbSectorIVGenMethod.ItemIndex := i;
      break;
    end;
  end;


  Result := (cbSectorIVGenMethod.ItemIndex <> -1);
end;


procedure TfrmKeyEntryPlainLinux.GetMainIVSectorZeroPos(out startOfVolFile: Boolean;
  out startOfEndData: Boolean);
begin
  // !! WARNING !!
  // Important that the radiogroup indexes are correct!
  startOfVolFile := (rgSectorIVSectorZeroPos.ItemIndex = 0);
  startOfEndData := (rgSectorIVSectorZeroPos.ItemIndex = 1);

end;


// Note: At most *one* of the parameters passed to SetMainIVSectorZeroPos(...) can be set to TRUE
function TfrmKeyEntryPlainLinux.SetMainIVSectorZeroPos(startOfVolFile: Boolean;
  startOfEndData: Boolean): Boolean;
begin
  rgSectorIVSectorZeroPos.ItemIndex := -1;

  // !! WARNING !!
  // Important that the radiogroup indexes are correct!
  if startOfVolFile then begin
    rgSectorIVSectorZeroPos.ItemIndex := 0;
  end;

  if startOfEndData then begin
    rgSectorIVSectorZeroPos.ItemIndex := 1;
  end;

  // Return TRUE if they're not *both* set, but at least one *is* set
  Result := not (startOfVolFile and startOfEndData) and (startOfVolFile or startOfEndData);
end;

 // Note: This function will return '' if none is selected, and still return TRUE
 // It will also return '' if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryPlainLinux.GetMainIVHashKernelDeviceName(
  var mainIVHashDriver: String): Boolean;
var
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := False;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then begin
      // ...and the user has selected one, return it
      if (cbSectorIVHash.ItemIndex >= 0) then begin
        mainIVHashDriver := fHashKernelModeDriverNames[cbSectorIVHash.ItemIndex];
        // unicode->ansi not a data loss because only ansistring stored
        Result           := True;
      end;
    end else begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVHashDriver := '';
      Result           := True;
    end;

  end;

end;

 // Note: This function will return NULL_GUID if none is selected, and still return TRUE
 // It will also return NULL_GUID if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryPlainLinux.GetMainIVHashGUID(var mainIVHashGUID: TGUID): Boolean;
var
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := False;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_HASH[sectorIVGenMethod]) then begin
      // ...and the user has selected one, return it
      if (cbSectorIVHash.ItemIndex >= 0) then begin
        mainIVHashGUID := StringToGUID(fHashGUIDs[cbSectorIVHash.ItemIndex]);
        Result         := True;
      end;
    end else begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVHashGUID := StringToGUID(NULL_GUID);
      Result         := True;
    end;

  end;

end;

function TfrmKeyEntryPlainLinux.SetMainIVHash(mainIVHashDriver: String;
  mainIVHashGUID: TGUID): Boolean;
var
  i: Integer;
begin
  cbSectorIVHash.ItemIndex := -1;

  // Locate the appropriate array idx
  for i := 0 to (fHashKernelModeDriverNames.Count - 1) do begin
    //  hashKernelModeDriverNames is dll name for explorer, else reg key so use 'or' GUIDs as well
    { TODO -otdk -cUI: are these GUIDs the same across PCs? if not use cypher name etc }

    if {((hashKernelModeDriverNames[i] = mainIVHashDriver) or}
    (fHashGUIDs[i] = GUIDToString(mainIVHashGUID)) then begin
      cbSectorIVHash.ItemIndex := i;
      break;
    end;

  end;



  Result := (cbSectorIVHash.ItemIndex <> -1);
end;


 // Note: This function will return '' if none is selected, and still return TRUE
 // It will also return '' if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryPlainLinux.GetMainIVCypherKernelDeviceName(
  var mainIVCypherDriver: Ansistring): Boolean;
var
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := False;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then begin
      // ...and the user has selected one, return it
      if (cbSectorIVCypher.ItemIndex >= 0) then begin
        mainIVCypherDriver := fCypherKernelModeDriverNames[cbSectorIVCypher.ItemIndex];
        // unicode->ansi not a data loss because only ansistring stored
        Result             := True;
      end;
    end else begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVCypherDriver := '';
      Result             := True;
    end;

  end;

end;


 // Note: This function will return NULL_GUID if none is selected, and still return TRUE
 // It will also return NULL_GUID if no hash *can* be selected (the control is disabled)
function TfrmKeyEntryPlainLinux.GetMainIVCypherGUID(var mainIVCypherGUID: TGUID): Boolean;
var
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
begin
  Result := False;


  if GetMainSectorIVGenMethod(sectorIVGenMethod) then begin
    // If we're supposed to have a hash for the IV generation method...
    if (SCTRIVGEN_USES_CYPHER[sectorIVGenMethod]) then begin
      // ...and the user has selected one, return it
      if (cbSectorIVCypher.ItemIndex >= 0) then begin
        mainIVCypherGUID := StringToGUID(fCypherGUIDs[cbSectorIVCypher.ItemIndex]);
        Result           := True;
      end;
    end else begin
      // No hash required for IV generation, just return '' and TRUE
      mainIVCypherGUID := StringToGUID(NULL_GUID);
      Result           := True;
    end;
  end;
end;

function TfrmKeyEntryPlainLinux.SetMainIVCypher(mainIVCypherDriver: String;
  mainIVCypherGUID: TGUID): Boolean;
var
  i: Integer;
begin
  cbSectorIVCypher.ItemIndex := -1;

  // Locate the appropriate array idx
  for i := 0 to (fCypherKernelModeDriverNames.Count - 1) do begin
       //  hashKernelModeDriverNames is dll name for explorer, else reg key so use 'or' GUIDs as well
       { TODO -otdk -cUI: are these GUIDs the same across PCs? if not use cypher name etc }
    if {((cypherKernelModeDriverNames[i] = mainIVCypherDriver) or}
    (fCypherGUIDs[i] = GUIDToString(mainIVCypherGUID)) then begin
      cbSectorIVCypher.ItemIndex := i;
      break;
    end;

  end;

  Result := (cbSectorIVCypher.ItemIndex <> -1);
end;


procedure TfrmKeyEntryPlainLinux.ckSelected(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmKeyEntryPlainLinux.pbLoadClick(Sender: TObject);
begin
  if (OpenSettingsFileDlg.Execute) then begin
    if not (FileExists(OpenSettingsFileDlg.Filename)) then begin
      SDUMessageDlg(_('Settings file not found.'), mtError);
    end else begin
      LoadSettings(OpenSettingsFileDlg.Filename);
    end;
  end;

end;

procedure TfrmKeyEntryPlainLinux.LoadSettings(filename: String);
var
  settingsFile:    TINIFile;
  tmpString:       String;
  tmpStringName, tmpStringGUID: String;
  tmpBoolean:      Boolean;
  tmpInteger:      Integer;
  startOfVolFile, startOfEndData: Boolean;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  found:           Boolean;
  ignoreIfCantSet: Boolean;  // Set to TRUE for parameters which we don't
  Ok:              Boolean;
  // really care about
  // e.g. If not hash algorithm is required for the
  //      IV, we can ignore it if we can't show the
  //      hash read in
begin
  Ok := True;

  settingsFile := TINIFile.Create(filename);
  try
    // -------
    // Key processing...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcSeed, '');
    SetKeyProcSeed(tmpString);

    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_KEY,
      SETTINGS_VALUE_KeyProcHashKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_KEY,
      SETTINGS_VALUE_KeyProcHashGUID, NULL_GUID);
    //  still apply est of file even if one field cnat be set
    if not SetKeyProcHash(tmpStringName, StringToGUID(tmpStringGUID)) then
      Ok := False;

    tmpBoolean := settingsFile.ReadBool(SETTINGS_SECTION_KEY, SETTINGS_VALUE_HashWithAs, True);
    SetKeyProcHashWithAs(tmpBoolean);

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_KEY,
      SETTINGS_VALUE_KeyProcCypherIterationCount, 0);
    SetKeyProcCypherIterationCount(tmpInteger);

    // Only pay attention to errors in getting the cypher if:
    // 1) We can get the cypher iteration count
    // 2) The cypher iteration count greater than zero
    ignoreIfCantSet := True;
    GetKeyProcCypherIterationCount(tmpInteger);
    if (tmpInteger > 0) then
      ignoreIfCantSet := False;

    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_KEY,
      SETTINGS_VALUE_KeyProcCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_KEY,
      SETTINGS_VALUE_KeyProcCypherGUID, NULL_GUID);
    if not SetKeyProcCypher(tmpStringName, StringToGUID(tmpStringGUID)) and
      (not ignoreIfCantSet) then
      Ok := False;

    // -------
    // Encryption options...
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainCypherGUID, NULL_GUID);
    if not SetMainCypher(tmpStringName, StringToGUID(tmpStringGUID)) then
      Ok := False;

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVGenMethod, FreeOTFESectorIVGenMethodID[foivgUnknown]);
    found      := False;
    for tmpSectorIVGenMethod := low(TFreeOTFESectorIVGenMethod)
      to high(TFreeOTFESectorIVGenMethod) do begin
      if (FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod] = tmpInteger) then begin
        if not SetMainSectorIVGenMethod(tmpSectorIVGenMethod) then
          Ok := False;
        found := True;
        break;
      end;
    end;
    if not found then
      Ok := False;

    // Ignore errors in the IV's sector zero location if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a sector zero location
    ignoreIfCantSet := True;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
      if (SCTRIVGEN_USES_SECTOR_ID[tmpSectorIVGenMethod]) then begin
        ignoreIfCantSet := False;
      end;
    end;
    tmpString      := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_UNSET);
    startOfVolFile := False;
    startOfEndData := False;
    if (tmpString = IV_SECTOR_ID_START_ENCRYPTED_DATA) then begin
      startOfEndData := True;
    end else
    if (tmpString = IV_SECTOR_ID_START_ENCRYPTED_VOLUME) then begin
      startOfVolFile := True;
    end;
    if not SetMainIVSectorZeroPos(startOfVolFile, startOfEndData) and (not ignoreIfCantSet) then
      Ok := False;

    // Ignore errors in the IV's hash algorithm if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a hash algorithm
    ignoreIfCantSet := True;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
      if (SCTRIVGEN_USES_HASH[tmpSectorIVGenMethod]) then begin
        ignoreIfCantSet := False;
      end;
    end;
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVHashKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVHashGUID, NULL_GUID);
    if not SetMainIVHash(tmpStringName, StringToGUID(tmpStringGUID)) and
      (not ignoreIfCantSet) then
      Ok := False;

    // Ignore errors in the IV's cypher algorithm if:
    //   1) We couldn't get the sector IV generation method, or
    //   2) We can get it, but it doesn't need a cypher
    ignoreIfCantSet := True;
    if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
      if (SCTRIVGEN_USES_CYPHER[tmpSectorIVGenMethod]) then begin
        ignoreIfCantSet := False;
      end;
    end;
    tmpStringName := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVCypherKernelDeviceName, '');
    tmpStringGUID := settingsFile.ReadString(SETTINGS_SECTION_ENCRYPTION,
      SETTINGS_VALUE_MainIVCypherGUID, NULL_GUID);
    if not SetMainIVCypher(tmpStringName, StringToGUID(tmpStringGUID)) and
      (not ignoreIfCantSet) then
      Ok := False;

    // -------
    // File options...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_FILE_OPTIONS,
      SETTINGS_VALUE_Offset, '0');
    SetOffset(strtoint64(tmpString));

    tmpString := settingsFile.ReadString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Size, '0');
    SetSizeLimit(strtoint64(tmpString));

    // -------
    // Mount options...
    tmpString := settingsFile.ReadString(SETTINGS_SECTION_MOUNT_OPTIONS,
      SETTINGS_VALUE_DriveLetter, '-');
    // Just in case we attempt to get the 1st char from an empty string...
    if (tmpString = '') then begin
      tmpString := #0;
    end // Decode a stored "Use default"
    else
    if (tmpString = '-') then begin
      tmpString := #0;
    end;
    if not SetDriveLetter(tmpString[1]) then
      Ok := False;

    tmpBoolean := settingsFile.ReadBool(SETTINGS_SECTION_MOUNT_OPTIONS,
      SETTINGS_VALUE_Readonly, False);
    SetReadonly(tmpBoolean);

    tmpInteger := settingsFile.ReadInteger(SETTINGS_SECTION_MOUNT_OPTIONS,
      SETTINGS_VALUE_MountAs, Ord(fomaRemovableDisk));
    if not SetMountAs(TFreeOTFEMountAs(tmpInteger)) then
      Ok := False;


  finally
    settingsFile.Free();
  end;

  if not Ok then begin
    SDUMessageDlg(
      _('One or more of your settings may not have loaded correctly; please check settings before continuing.'),
      mtWarning
      );
  end;

  EnableDisableControls();

end;


procedure TfrmKeyEntryPlainLinux.pbSaveClick(Sender: TObject);
var
  settingsFile:         TINIFile;
  tmpString:            String;
  tmpAnsiString:        Ansistring;
  tmpBoolean:           Boolean;
  tmpGUID:              TGUID;
  tmpInteger:           Integer;
  tmpInt64:             Int64;
  tmpChar:              DriveLetterChar;
  startOfVolFile, startOfEndData: Boolean;
  tmpSectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  tmpMountAs:           TFreeOTFEMountAs;
begin
  if (SaveSettingsFileDlg.Execute) then begin
    settingsFile := TINIFile.Create(SaveSettingsFileDlg.Filename);
    try
      // -------
      // Key processing...
      GetKeyProcSeed(tmpAnsiString);
      settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcSeed, tmpAnsiString);


      if (GetKeyProcHashKernelDeviceName(tmpString)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashKernelDeviceName,
          tmpString);
      end;

      if (GetKeyProcHashGUID(tmpGUID)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashGUID,
          GUIDToString(tmpGUID));
      end;

      GetKeyProcHashWithAs(tmpBoolean);
      settingsFile.WriteBool(SETTINGS_SECTION_KEY, SETTINGS_VALUE_HashWithAs, tmpBoolean);

      GetKeyProcCypherIterationCount(tmpInteger);
      settingsFile.WriteInteger(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherIterationCount,
        tmpInteger);


      // Only store the key processing cypher if the key is to be processed
      // with > 0 iterations of the cypher
      GetKeyProcCypherIterationCount(tmpInteger);
      if (tmpInteger > 0) then begin
        if (GetKeyProcCypherKernelDeviceName(tmpString)) then begin
          settingsFile.WriteString(SETTINGS_SECTION_KEY,
            SETTINGS_VALUE_KeyProcCypherKernelDeviceName, tmpString);
        end;

        if (GetKeyProcCypherGUID(tmpGUID)) then begin
          settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherGUID,
            GUIDToString(tmpGUID));
        end;
      end;

      // -------
      // Encryption options...
      if (GetMainCypherKernelDeviceName(tmpString)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
          SETTINGS_VALUE_MainCypherKernelDeviceName, tmpString);
      end;

      if (GetMainCypherGUID(tmpGUID)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION, SETTINGS_VALUE_MainCypherGUID,
          GUIDToString(tmpGUID));
      end;

      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
        settingsFile.WriteInteger(SETTINGS_SECTION_ENCRYPTION,
          SETTINGS_VALUE_MainIVGenMethod, FreeOTFESectorIVGenMethodID[tmpSectorIVGenMethod]);
      end;

      // Only store the IV sector zero location if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
        if (SCTRIVGEN_USES_SECTOR_ID[tmpSectorIVGenMethod]) then begin
          GetMainIVSectorZeroPos(startOfVolFile, startOfEndData);
          if (startOfVolFile) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_ENCRYPTED_VOLUME);
          end else
          if (startOfEndData) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_ENCRYPTED_DATA);
          end else begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVSectorZeroPos, IV_SECTOR_ID_START_UNSET);
          end;

        end;
      end;

      // Only store the IV hash if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
        if (SCTRIVGEN_USES_HASH[tmpSectorIVGenMethod]) then begin
          if (GetMainIVHashKernelDeviceName(tmpString)) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVHashKernelDeviceName, tmpString);
          end;

          if (GetMainIVHashGUID(tmpGUID)) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVHashGUID, GUIDToString(tmpGUID));
          end;
        end;
      end;

      // Only store the IV cypher if it's needed
      if (GetMainSectorIVGenMethod(tmpSectorIVGenMethod)) then begin
        if (SCTRIVGEN_USES_CYPHER[tmpSectorIVGenMethod]) then begin
          if (GetMainIVCypherKernelDeviceName(tmpAnsiString)) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVCypherKernelDeviceName, tmpAnsiString);
          end;

          if (GetMainIVCypherGUID(tmpGUID)) then begin
            settingsFile.WriteString(SETTINGS_SECTION_ENCRYPTION,
              SETTINGS_VALUE_MainIVCypherGUID, GUIDToString(tmpGUID));
          end;
        end;
      end;


      // -------
      // File options...
      GetOffset(tmpInt64);
      settingsFile.WriteString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Offset,
        IntToStr(tmpInt64));


      GetSizeLimit(tmpInt64);
      settingsFile.WriteString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Size,
        IntToStr(tmpInt64));


      // -------
      // Mount options...
      GetDriveLetter(tmpChar);
      // If the user selected "Use default", we encode the #0 to something
      // we can store
      if (tmpChar = #0) then begin
        tmpChar := '-';
      end;
      settingsFile.WriteString(SETTINGS_SECTION_MOUNT_OPTIONS,
        SETTINGS_VALUE_DriveLetter, tmpChar);


      GetReadonly(tmpBoolean);
      settingsFile.WriteBool(SETTINGS_SECTION_MOUNT_OPTIONS, SETTINGS_VALUE_Readonly,
        tmpBoolean);


      if (GetMountAs(tmpMountAs)) then begin
        settingsFile.WriteInteger(SETTINGS_SECTION_MOUNT_OPTIONS,
          SETTINGS_VALUE_MountAs, Ord(tmpMountAs));
      end;


    finally
      settingsFile.Free();
    end;

  end;

end;


procedure TfrmKeyEntryPlainLinux.pbIVCypherInfoClick(Sender: TObject);
var
  deviceName: Ansistring;
  GUID:       TGUID;
begin
  GetMainIVCypherKernelDeviceName(deviceName);
  GetMainIVCypherGUID(GUID);
  GetFreeOTFEBase().ShowCypherDetailsDlg(deviceName, GUID);

end;

end.
