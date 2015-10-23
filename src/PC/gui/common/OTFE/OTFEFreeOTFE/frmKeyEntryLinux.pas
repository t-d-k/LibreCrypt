unit frmKeyEntryLinux;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

//this is for new and existing dmcrypt volumes - not LUKS
{ TODO 1 -crefactor : rename unit }

interface

uses
  //delphi & libs
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Messages,
  PasswordRichEdit,
  StdCtrls, SysUtils, Windows,
  Spin64,
  //sdu & LibreCrypt utils
  lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  lcTypes,   // Required for TFreeOTFEMountAs
  DriverAPI, OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUSpin64Units, SDUStdCtrls, SDUDialogs,

  // LibreCrypt forms
  fmeNewPassword, fmePassword, fmeVolumeSelect;

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
    Label16:        TLabel;
    Label15:        TLabel;
    Label14:        TLabel;
    Label21:        TLabel;
    GroupBox3:      TGroupBox;
    Label23:        TLabel;
    Label24:        TLabel;
    cbMainCypher:   TComboBox;
    pbMainCypherInfo: TButton;
    GroupBox4:      TGroupBox;
    lblOffset:      TLabel;
    lblOffsetO:     TLabel;
    Label10:        TLabel;
    Label11:        TLabel;
    GroupBox2:      TGroupBox;
    lblDrive:       TLabel;
    lblReadOnlySwitch: TLabel;
    cbDrive:        TComboBox;
    ckMountReadonly: TSDUCheckBox;
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
    tsKeyOptions:   TTabSheet;
    tsNewKey:       TTabSheet;
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
    ckHashWithAs:   TSDUCheckBox;
    frmeNewPassword1: TfrmeNewPassword;
    frmePassword1:  TfrmePassword;
    tsChooseContainer: TTabSheet;
    GroupBox7: TGroupBox;
    OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    function _MountVolume(): Boolean;
    function _SilencableMessageDlg(Content: String; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons): Integer;
  protected
    // These are ordered lists corresponding to the items shown in the combobox
    fHashKernelModeDriverNames: TStringList;
    fHashGUIDs:   TStringList;
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDs: TStringList;

    ffile_name:  String;          // file to create/mount
    fmounted_as: DriveLetterChar; //dive mounted or #0

    fsilent_result:   TModalResult;
    fsilent_password: PasswordString;
    // some bug whereby password is reset in silent mode { TODO 1 -otdk -crefactor : investigate }
    fcreate_vol:      Boolean;   //dialog is creating volume?
    fis_hidden:       Boolean;   //dialog is creating/ opening hidden volume?

    procedure _PopulateHashes();
    procedure _PopulateCyphers();
    procedure _PopulateSectorIVGenMethods();
    procedure _PopulateDrives();
    procedure _PopulateMountAs();
    procedure _DefaultOptions();
    procedure _EnableDisableControls();


  public
    // Key...
    function GetKey(): Ansistring;
    procedure SetKey(userKey: PasswordString);

    // Key processing...
    //    procedure GetKeyProcSeed(var keyProcSeed: Ansistring);

    function GetKeyProcHashKernelDeviceName(var keyProcHashDriver: String): Boolean;
    function GetKeyProcHashGUID(var keyProcHashGUID: TGUID): Boolean;

    procedure SetKeyProcSeed(keyProcSeed: String);
    function SetKeyProcHash(keyProcHashDriver: String; keyProcHashGUID: TGUID): Boolean;
    //    procedure GetKeyProcHashWithAs(var hashWithAs: Boolean);
    procedure SetKeyProcHashWithAs(hashWithAs: Boolean);
    { done 1 -otdk -crefactor : remove all getters only used in this class }
    function GetKeyProcCypherKernelDeviceName(var keyProcCypherDriver: String): Boolean;
    function GetKeyProcCypherGUID(var keyProcCypherGUID: TGUID): Boolean;
    function SetKeyProcCypher(keyProcCypherDriver: String; keyProcCypherGUID: TGUID): Boolean;
    // Note: GetKeyProcCypherIterationCount returns the number of iterations
    //       required; if the user entered "2" on the dialog, then 2000 will
    //       be returned by this function
    //    procedure GetKeyProcCypherIterationCount(out keyProcCypherIterations: Integer);
    procedure SetKeyProcCypherIterationCount(keyProcCypherIterations: Integer);

    // File options...
    //    procedure GetOffset(out fileOptoffset: Int64);
    procedure SetOffset(fileOptoffset: Int64);
    //    procedure GetSizeLimit(out fileOptSizeLimit: Int64);
    procedure SetSizeLimit(fileOptSizeLimit: Int64);

    // Encryption options...
    function GetMainCypherKernelDeviceName(var mainCypherDriver: String): Boolean;
    function GetMainCypherGUID(var mainCypherGUID: TGUID): Boolean;
    function SetMainCypher(mainCypherDriver: String; mainCypherGUID: TGUID): Boolean;
    //    procedure GetMainIVSectorZeroPos(out startOfVolFile: Boolean; out startOfEndData: Boolean);
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
    //    procedure GetReadonly(var mountReadonly: Boolean);
    procedure SetReadonly(mountReadonly: Boolean);
    function GetMountAs(var mountAs: TMountDiskType): Boolean;
    function SetMountAs(mountAs: TMountDiskType): Boolean;
    //    function GetMountForAllUsers(): Boolean;
    procedure SetMountForAllUsers(allUsers: Boolean);


    procedure LoadSettings(filename: String);

    function GetMountedAs: DriveLetterChar; //dive mounted

    // Generate a Linux volume's volume key
    class function GenerateLinuxVolumeKey(hashDriver: Ansistring; hashGUID: TGUID;
      userKey, seed: Ansistring; hashWithAs: Boolean;
      targetKeylengthBits: Integer; var volumeKey: TSDUBytes): Boolean;

    property fileName: String Write ffile_name;
    property createVol: Boolean Write fcreate_vol;
    //    property silent: boolean write fsilent;
    property isHidden: Boolean Write fis_hidden;
  end;

function MountPlainLinux(volumeFilename: String;
  var mountedAs: DriveLetterChar; ReadOnly: Boolean = False;
  lesFile: String = ''; password: TSDUBytes = nil;
  offset: ULONGLONG = 0; createVol: Boolean = False; isHidden: Boolean = False): TMountresult;

//function CreatePlainLinuxVolumeWizard(out fileName: String): Boolean;
function CreateFileWithPrompt(var fileName: String; out user_cancelled: Boolean): Boolean;



 // ---------
 // See function comment in body of function
{$IFDEF LINUX_DETECT}
    function  DetectLinux(
      volumeFilename: string;
      userKey: string;
      keyProcSeed: string;
      keyProcIterations: integer;
      fileOptOffset: int64;
      fileOptSize: int64;
      mountDriveLetter: char;
      mountMountAs: TFreeOTFEMountAs;
      testFilename: string
      ): string;
{$ENDIF}

 // Mount file(s) assuming specific volume type
 // Note: The volumeFilename(s) passed in can EITHER by files on the local
 //       filesystem (e.g. "C:\myfile.dat"), OR partitions
 //       (e.g. "\Device\Harddisk1\Partition3")
 // Note: "keyfile" is only supported for LUKS volumes atm
 // Note: If "keyfile" is supplied, the contents of this file will be used
 //       and "password" will be ignored
 //    function MountLinux(volumeFilename: String; ReadOnly: Boolean = False;
 //      lesFile: String = ''; password: TSDUBytes = nil; keyfile: String = '';
 //      keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
 //      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
 //      offset: ULONGLONG = 0; silent: Boolean = False;
 //      forceHidden: Boolean = False): DriveLetterChar;
 //      overload;

{ done 1 -otdk -crefaactor : split into mountLUKS and mountplainlinux fns }
    {
function MountLinux(volumeFilename: String; var mountedAs: DriveLetterChar;
      ReadOnly: Boolean = False; lesFile: String = ''; password: TSDUBytes = nil;
      keyfile: String = ''; keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
      offset: ULONGLONG = 0; silent: Boolean = False;
      forceHidden: Boolean = False; createVol: Boolean = False): Boolean;
    //      overload;
    // Note: The MountLUKS(...) functions will be called automatically if
    //       MountLinux(...) is called with a LUKS volume
    //    function MountLUKS(volumeFilename: String; ReadOnly: Boolean = False;
    //      password: TSDUBytes = nil; keyfile: String = '';
    //      keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
    //      keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE;
    //      silent: Boolean = False): DriveLetterChar; overload;    }


implementation

{$R *.DFM}


uses
  //delphi & libs
  ComObj,                      // Required for StringToGUID
  VolumeFileAPI,               // Required for SCTRIVGEN_USES_SECTOR_ID and SCTRIVGEN_USES_HASH
  INIFiles, Math,

  //sdu & LibreCrypt utils
  SDUi18n, Pkcs11Lib, lcConsts, sduGeneral, OTFEFreeOTFEDLL_U,
  LUKSTools,
  OTFEConsts_U,
  CommonSettings,
  MainSettings, lcCommandLine,
  // LibreCrypt forms
  frmNewVolumeSize,
  frmCypherInfo, frmHashInfo;

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

procedure TfrmKeyEntryPlainLinux.FormActivate(Sender: TObject);
begin
  inherited;
//   frmeNewPassword1.Show;
//   frmeNewPassword1.SetFocus;
//   frmeNewPassword1.preUserKeyFirst.SetFocus;


end;

procedure TfrmKeyEntryPlainLinux.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if GetCmdLine.isSilent then
    ModalResult := fsilent_result;
end;

procedure TfrmKeyEntryPlainLinux.FormCreate(Sender: TObject);
begin
  fHashKernelModeDriverNames   := TStringList.Create();
  fHashGUIDs                   := TStringList.Create();
  fCypherKernelModeDriverNames := TStringList.Create();
  fCypherGUIDs                 := TStringList.Create();
  pcEntry.ActivePage           := tsKey;


  frmePassword1.ClearPassword;
  frmeNewPassword1.ClearKeyPhrase;
  // Linux volumes CAN NOT have newlines in the user's password

  feGPGExecutable.TabStop     := False;
  feGPGExecutable.FilterIndex := 0;

  feGPGKeyfile.TabStop     := False;
  feGPGKeyfile.FilterIndex := 0;

  OpenSettingsFileDlg.Filter     := FILE_FILTER_FLT_LINUX_SETTINGS;
  OpenSettingsFileDlg.DefaultExt := FILE_FILTER_DFLT_LINUX_SETTINGS;
  OpenSettingsFileDlg.Options    := OpenSettingsFileDlg.Options + [ofDontAddToRecent];
  SaveSettingsFileDlg.Filter     := FILE_FILTER_FLT_LINUX_SETTINGS;
  SaveSettingsFileDlg.DefaultExt := FILE_FILTER_DFLT_LINUX_SETTINGS;
  SaveSettingsFileDlg.Options    := SaveSettingsFileDlg.Options + [ofDontAddToRecent];

  _PopulateHashes();
  _PopulateCyphers();
  _PopulateSectorIVGenMethods();
  _PopulateDrives();
  _PopulateMountAs();

  _DefaultOptions();

     frmeNewPassword1.OnChange:= SelectionChange;
//
//      frmeNewPassword1.SetFocus;
//   frmeNewPassword1.preUserKeyFirst.SetFocus;
end;


procedure TfrmKeyEntryPlainLinux._PopulateHashes();
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
      _SilencableMessageDlg(
        _('Unable to obtain list of hashes.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE hash drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed FreeOTFE, you may need to restart your computer.'),
        mtError, [mbOK]
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmKeyEntryPlainLinux._PopulateCyphers();
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
      _SilencableMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed FreeOTFE, you may need to restart your computer.'),
        mtError, [mbOK]
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmKeyEntryPlainLinux._PopulateSectorIVGenMethods();
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


procedure TfrmKeyEntryPlainLinux._PopulateDrives();
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


procedure TfrmKeyEntryPlainLinux._PopulateMountAs();
var
  currMountAs: TMountDiskType;
begin
  cbMediaType.Items.Clear();
  for currMountAs := low(TMountDiskType) to high(TMountDiskType) do
    // do not allow dm-crypt vols as 'fixed discs' - work-around BSoD issue
    if not (currMountAs in [fomaUnknown, fomaFixedDisk]) then
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
end;


procedure TfrmKeyEntryPlainLinux._DefaultOptions();
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

      if (GetSettings().DefaultDriveChar <> #0) then begin
        // Start from 1; skip the default
        for i := 1 to (cbDrive.items.Count - 1) do begin
          currDriveLetter := cbDrive.Items[i][1];
          if (currDriveLetter >= GetSettings().DefaultDriveChar) then begin
            cbDrive.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;
  end;

  if (GetSettings() is TMainSettings) then begin
    SetMountAs(GetMainSettings().DefaultMountDiskType);
  end else begin
    SetMountAs(fomaRemovableDisk);
  end;

  feGPGExecutable.Filename := '';
  feGPGKeyfile.Filename    := '';

  edKeySeed.Text                  := '';
  seKeyProcCypherIterations.Value := 0;

  se64UnitSizeLimit.Value := 0;
  se64UnitOffset.Value    := 0;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.Checked := True;

end;

function TfrmKeyEntryPlainLinux._MountVolume(): Boolean;
var
  keyProcSeed: Ansistring;

  keyProcHashDriver:   String;
  keyProcHashGUID:     TGUID;
  keyProcHashWithAs:   Boolean;
  keyProcCypherDriver: String;
  keyProcCypherGUID:   TGUID;
  //  keyProcIterations:   Integer;
  fileOptOffset:       Int64;
  fileOptSize:         Int64;
  mountDriveLetter:    DriveLetterChar;
  mountReadonly:       Boolean;
  mainCypherDriver:    String;
  mainCypherGUID:      TGUID;
  mainIVHashDriver:    String;
  mainIVHashGUID:      TGUID;
  mainIVCypherDriver:  Ansistring;
  mainIVCypherGUID:    TGUID;
  currDriveLetter:     DriveLetterChar;
  sectorIVGenMethod:   TFreeOTFESectorIVGenMethod;
  startOfVolFile:      Boolean;
  //  startOfEndData:      Boolean;
  mountMountAs:        TMountDiskType;
  VolumeFlags:         DWORD;
  mainCypherDetails:   TFreeOTFECypher_v3;
  mountForAllUsers:    Boolean;
  userKey:             Ansistring;
  volumeKey:           TSDUBytes;

begin
  { done -otdk -cclean : ugly - move CreateMountDiskDevice to dialog, or have record 'mountoptions' }
  Result      := True;
  // Key...
  //      GetKeyProcSeed();
  keyProcSeed := edKeySeed.Text;
  fmounted_as := #0;
  GetKeyProcHashGUID(keyProcHashGUID);
  //      GetKeyProcHashWithAs();
  keyProcHashWithAs := ckHashWithAs.Checked;
  //  keyProcIterations := seKeyProcCypherIterations.Value * 1000;
  fileOptOffset     := se64UnitOffset.Value;
  userKey           := GetKey();
  fileOptSize       := se64UnitSizeLimit.Value;

  GetDriveLetter(mountDriveLetter);
  mountReadonly  := ckMountReadonly.Checked;
  // Encryption options...
  startOfVolFile := (rgSectorIVSectorZeroPos.ItemIndex = 0);
  //  startOfEndData := (rgSectorIVSectorZeroPos.ItemIndex = 1);

  if (
    // Key processing...
    // File options...
    GetKeyProcHashKernelDeviceName(keyProcHashDriver) and
    GetKeyProcCypherKernelDeviceName(keyProcCypherDriver) and
    GetKeyProcCypherGUID(keyProcCypherGUID) and

    // Mount options...
    (GetMountAs(mountMountAs)) and
    // Encryption options...
    (GetMainCypherKernelDeviceName(mainCypherDriver)) and
    (GetMainCypherGUID(mainCypherGUID)) and (GetMainSectorIVGenMethod(sectorIVGenMethod)) and
    (GetMainIVHashKernelDeviceName(mainIVHashDriver)) and
    (GetMainIVHashGUID(mainIVHashGUID)) and
    (GetMainIVCypherKernelDeviceName(mainIVCypherDriver)) and
    (GetMainIVCypherGUID(mainIVCypherGUID))) then begin
    mountForAllUsers := ckMountForAllUsers.Checked;

    // Before we can generate the volume key for encryption/decryption, we
    // need to know the keysize of the cypher
    if not (GetFreeOTFEBase().GetSpecificCypherDetails(mainCypherDriver,
      mainCypherGUID, mainCypherDetails)) then
      Result := False;


    // Generate volume key for encryption/decryption
    if Result then begin
      if not (GenerateLinuxVolumeKey(keyProcHashDriver, keyProcHashGUID,
        userKey, keyProcSeed, keyProcHashWithAs, mainCypherDetails.KeySizeRequired,
        volumeKey)) then begin
        GetFreeOTFE().LastErrorCode := OTFE_ERR_HASH_FAILURE;
        Result := False;
      end;
    end;


    // Encode IV generation method to flags...
    VolumeFlags := 0;
    if (startOfVolFile) then
      VolumeFlags := VolumeFlags or VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART;


    if Result then begin

      currDriveLetter := GetFreeOTFEBase().GetNextDriveLetter(mountDriveLetter, Char(#0));
      if (currDriveLetter = #0) then begin
        // No more drive letters following the user's specified drive
        // letter - don't mount further drives
        Result := False;
        // Bail out...
        exit;
      end;
      // SDUInitAndZeroBuffer(0, emptyIV);
      if GetFreeOTFEBase().CreateMountDiskDevice(ffile_name, volumeKey,
        sectorIVGenMethod, nil,  // Linux volumes don't have per-volume IVs
        mountReadonly, mainIVHashDriver, mainIVHashGUID, mainIVCypherDriver,
        mainIVCypherGUID, mainCypherDriver, mainCypherGUID, VolumeFlags,
        currDriveLetter, fileOptOffset, fileOptSize, True,
                            // Linux volume
        PKCS11_NO_SLOT_ID,  // PKCS11 SlotID
        mountMountAs, mountForAllUsers) then begin
        fmounted_as := currDriveLetter;
      end else begin
        fmounted_as := #0;
        Result      := False;
      end;
    end;  // if Result then

  end;
end;


procedure TfrmKeyEntryPlainLinux.pbOKClick(Sender: TObject);
var
  tmpKey:         Ansistring;
  user_cancelled: Boolean;
begin
  ModalResult := mrCancel;


  if fcreate_vol then assert(frmeNewPassword1.IsPasswordValid);
  tmpKey      := GetKey();

  if (Length(tmpKey) = 0) then begin
    if (_SilencableMessageDlg(_('You have not entered a Keyphrase.') + SDUCRLF +
      SDUCRLF + _('Are you sure you wish to proceed?'), mtConfirmation, [mbYes, mbNo]) =
      mrYes) then begin
      ModalResult := mrOk;
    end;
  end else begin
    if (Length(tmpKey) < 20) then begin
      if (_SilencableMessageDlg(_(
        'The Keyphrase you entered has less than 20 characters, and may not be compatible with some Linux volumes.'),
        mtWarning, [mbIgnore, mbAbort]) = mrIgnore) then begin
        ModalResult := mrOk;
      end;

    end else begin
      // else No problems with the password as entered; OK to close the dialog
      ModalResult := mrOk;
    end;

  end;

  if ModalResult = mrOk then begin
    if (not fis_hidden) and FileExists(ffile_name) and (not fcreate_vol) then begin
      if IsLUKSVolume(ffile_name) then begin
        if (_SilencableMessageDlg(
          _('This container is a LUKS container, opening it as a dm-crypt container may corrupt it.')
          , mtWarning, [mbIgnore, mbAbort]) <> mrIgnore) then begin
          ModalResult := mrAbort;
        end;
      end;

    end;
  end;
  if ModalResult = mrOk then
    if fcreate_vol and (not fis_hidden) then begin
      if FileExists(ffile_name) then begin
        _SilencableMessageDlg(_('This file already exists, a new container cannot be created.'),
          mtError, [mbAbort]);
        ModalResult := mrCancel;
      end else begin

        // create empty file
        if CreateFileWithPrompt(ffile_name, user_cancelled) then begin
          SDUMessageDlg(
            _('dm-crypt container created successfully.') + SDUCRLF + SDUCRLF +
            _('Now this container will be mounted and formated before use.'),
            mtInformation);
          { TODO 1 -otdk -cenhance : wipe volume first }

        end else begin
          if user_cancelled then
            ModalResult := mrCancel
          else
            SDUMessageDlg(_('The dm-crypt container could not be created'), mtError);
        end;

      end;
      { TODO 1 -otdk -csecurity : overwrite with 'chaff' - need to set up cyphers etc. }
    end;


  if ModalResult = mrOk then
    if _MountVolume() then begin

      { TODO 2 -otdk -csecurity : ensure is overritten and formated if created }

    end else begin
      ModalResult := mrAbort;
    end;
end;


procedure TfrmKeyEntryPlainLinux._EnableDisableControls();
var
  //  junkInt64:             Int64;
  filePresentGPGExe:     Boolean;
  filePresentGPGKeyfile: Boolean;
  iterationCypherOK:     Boolean;
  IVStartSectorOK:       Boolean;
  junkChar:              DriveLetterChar;
  IVHashOK:              Boolean;
  IVCypherOK:            Boolean;
  mountAsOK:             Boolean;
  sectorIVGenMethod:     TFreeOTFESectorIVGenMethod;
  tmpMountAs:            TMountDiskType;
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


  //  junkInt64 := se64UnitOffset.Value;
  //  junkInt64 := se64UnitSizeLimit.Value;
  GetDriveLetter(junkChar);
  pbOK.Enabled := ((filePresentGPGExe) and (filePresentGPGKeyfile) and
    (cbKeyProcHash.ItemIndex >= 0) and iterationCypherOK and
    (cbMainCypher.ItemIndex >= 0) and (sectorIVGenMethod <> foivgUnknown) and
    IVStartSectorOK and IVHashOK and IVCypherOK and mountAsOK);

    if fcreate_vol and not frmeNewPassword1.IsPasswordValid then  pbOK.Enabled := false;



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
  _EnableDisableControls();
end;

procedure TfrmKeyEntryPlainLinux.pbKeyProcHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetKeyProcHashKernelDeviceName(deviceName);
  GetKeyProcHashGUID(GUID);
  frmHashInfo.ShowHashDetailsDlg(deviceName, GUID);
end;

procedure TfrmKeyEntryPlainLinux.pbIVHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetMainIVHashKernelDeviceName(deviceName);
  GetMainIVHashGUID(GUID);
  frmHashInfo.ShowHashDetailsDlg(deviceName, GUID);
end;


procedure TfrmKeyEntryPlainLinux.pbKeyProcCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetKeyProcCypherKernelDeviceName(deviceName);
  GetKeyProcCypherGUID(GUID);
  frmCypherInfo.ShowCypherDetailsDlg(deviceName, GUID);

end;

procedure TfrmKeyEntryPlainLinux.pbMainCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  GetMainCypherKernelDeviceName(deviceName);
  GetMainCypherGUID(GUID);
  frmCypherInfo.ShowCypherDetailsDlg(deviceName, GUID);

end;



procedure TfrmKeyEntryPlainLinux.FormShow(Sender: TObject);
begin
  feGPGExecutable.Filter             := FILTER_GPG_EXE;
  feGPGKeyfile.Filter                := FILTER_GPG_FILES;
  feGPGExecutable.OpenDialog.Options := feGPGExecutable.OpenDialog.Options + [ofDontAddToRecent];
  feGPGExecutable.SaveDialog.Options := feGPGExecutable.SaveDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.OpenDialog.Options    := feGPGKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feGPGKeyfile.SaveDialog.Options    := feGPGKeyfile.SaveDialog.Options + [ofDontAddToRecent];



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

  //show existing or new password tab, depending on if creating new vol.
  tsKey.TabVisible    := not fcreate_vol;
  tsNewKey.TabVisible := fcreate_vol;
  if fcreate_vol then begin
    { TODO 1 : no idea why but edits don't show unless the tab is shown twice }
  pcEntry.ActivePage           := tsNewKey;
      pcEntry.ActivePage           := tsKeyOptions;
pcEntry.ActivePage           := tsNewKey;
  end;

  { done 1 -otdk -ccomplete : hide offset unless hidden }
  { TODO 1 -otdk -ccomplete : hide filename if hidden }

  lblOffset.Visible      := fis_hidden;
  se64UnitOffset.Visible := fis_hidden;
  lblOffsetO.Visible     := fis_hidden;

  _EnableDisableControls();


  if GetCmdLine.isSilent then begin
    frmePassword1.SetKeyPhrase(fsilent_password);
    //    LoadSettings(OpenSettingsFileDlg.Filename);?
    if _MountVolume() then begin
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;

    fsilent_result := ModalResult;

    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;


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
    ModalResult := mrCancel;
  end;

end;

procedure TfrmKeyEntryPlainLinux.pbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmKeyEntryPlainLinux.GetKey(): Ansistring;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  if fcreate_vol then
    Result := SDUBytesToString(frmeNewPassword1.GetKeyPhrase)
  else
Result := SDUBytesToString(frmePassword1.GetKeyPhrase) ;
end;

procedure TfrmKeyEntryPlainLinux.SetKey(userKey: PasswordString);
begin
if fcreate_vol then
 frmeNewPassword1.SetKeyPhrase(userKey)
  else
  frmePassword1.SetKeyPhrase(userKey);
  fsilent_password := userKey;
end;

 //procedure TfrmKeyEntryPlainLinux.GetKeyProcSeed(var keyProcSeed: Ansistring);
 //begin
 //  keyProcSeed := edKeySeed.Text;   { TODO 1 -otdk -cclean : allow unicode }
 //end;

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

 //procedure TfrmKeyEntryPlainLinux.GetKeyProcHashWithAs(var hashWithAs: Boolean);
 //begin
 //  hashWithAs := ckHashWithAs.Checked;
 //end;

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
  Result := True;

  if (seKeyProcCypherIterations.Value > 0) then begin
    Result := cbKeyProcCypher.ItemIndex >= 0;
    if Result then begin
      keyProcCypherDriver := fCypherKernelModeDriverNames[cbKeyProcCypher.ItemIndex];
    end;
  end else begin
    // No cypher selected, but none needed
    keyProcCypherDriver := '';
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


 //procedure TfrmKeyEntryPlainLinux.GetKeyProcCypherIterationCount(
 //  out keyProcCypherIterations: Integer);
 //begin
 //  keyProcCypherIterations := seKeyProcCypherIterations.Value * 1000;
 //end;

procedure TfrmKeyEntryPlainLinux.SetKeyProcCypherIterationCount(keyProcCypherIterations: Integer);
begin
  // Note that if "1" passed in, this will be set to 0
  seKeyProcCypherIterations.Value := keyProcCypherIterations div 1000;

end;

 //procedure TfrmKeyEntryPlainLinux.GetOffset(out fileOptoffset: Int64);
 //begin
 //  fileOptoffset := se64UnitOffset.Value;
 //end;

procedure TfrmKeyEntryPlainLinux.SetOffset(fileOptoffset: Int64);
begin
  se64UnitOffset.Value := fileOptoffset;
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


 //procedure TfrmKeyEntryPlainLinux.GetReadonly(var mountReadonly: Boolean);
 //begin
 //  mountReadonly := ckMountReadonly.Checked;
 //end;

procedure TfrmKeyEntryPlainLinux.SetReadonly(mountReadonly: Boolean);
begin
  ckMountReadonly.Checked := mountReadonly;
end;



function TfrmKeyEntryPlainLinux.GetMountAs(var mountAs: TMountDiskType): Boolean;
var
  currMountAs: TMountDiskType;
begin
  Result := False;

  for currMountAs := low(TMountDiskType) to high(TMountDiskType) do begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then begin
      mountAs := currMountAs;
      Result  := True;
      break;
    end;
  end;
end;


function TfrmKeyEntryPlainLinux.GetMountedAs: DriveLetterChar;
begin
  Result := fmounted_as;
end;

function TfrmKeyEntryPlainLinux.SetMountAs(mountAs: TMountDiskType): Boolean;
begin
  cbMediaType.ItemIndex :=
    cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  Result                := cbMediaType.ItemIndex >= 0;
end;


procedure TfrmKeyEntryPlainLinux.SetMountForAllUsers(allUsers: Boolean);
begin
  ckMountForAllUsers.Checked := allUsers;
end;

 //function TfrmKeyEntryPlainLinux.GetMountForAllUsers(): Boolean;
 //begin
 //  Result := ckMountForAllUsers.Checked;
 //end;


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
  Result := cbSectorIVGenMethod.ItemIndex >= 0;
  if Result then begin
    sectorIVGenMethod := TFreeOTFESectorIVGenMethod(
      cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
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


 //procedure TfrmKeyEntryPlainLinux.GetMainIVSectorZeroPos(out startOfVolFile: Boolean;
 //  out startOfEndData: Boolean);
 //begin
 //  // !! WARNING !!
 //  // Important that the radiogroup indexes are correct!
 //  startOfVolFile := (rgSectorIVSectorZeroPos.ItemIndex = 0);
 //  startOfEndData := (rgSectorIVSectorZeroPos.ItemIndex = 1);
 //
 //end;


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
  _EnableDisableControls();
end;

procedure TfrmKeyEntryPlainLinux.pbLoadClick(Sender: TObject);
begin
  if (OpenSettingsFileDlg.Execute) then
    if not (FileExists(OpenSettingsFileDlg.Filename)) then
      _SilencableMessageDlg(_('Settings file not found.'), mtError, [mbOK])
    else
      LoadSettings(OpenSettingsFileDlg.Filename);

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
    tmpInteger      := seKeyProcCypherIterations.Value * 1000;
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
    if not SetMountAs(TMountDiskType(tmpInteger)) then
      Ok := False;


  finally
    settingsFile.Free();
  end;

  if not Ok then begin
    _SilencableMessageDlg(
      _('One or more of your settings may not have loaded correctly; please check settings before continuing.'),
      mtWarning, [mbOK]
      );
  end;

  _EnableDisableControls();

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
  tmpMountAs:           TMountDiskType;
begin
  if (SaveSettingsFileDlg.Execute) then begin
    settingsFile := TINIFile.Create(SaveSettingsFileDlg.Filename);
    try
      // -------
      // Key processing...
      tmpAnsiString := edKeySeed.Text;
      settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcSeed, tmpAnsiString);


      if (GetKeyProcHashKernelDeviceName(tmpString)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashKernelDeviceName,
          tmpString);
      end;

      if (GetKeyProcHashGUID(tmpGUID)) then begin
        settingsFile.WriteString(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcHashGUID,
          GUIDToString(tmpGUID));
      end;

      tmpBoolean := ckHashWithAs.Checked;
      settingsFile.WriteBool(SETTINGS_SECTION_KEY, SETTINGS_VALUE_HashWithAs, tmpBoolean);

      tmpInteger := seKeyProcCypherIterations.Value * 1000;
      settingsFile.WriteInteger(SETTINGS_SECTION_KEY, SETTINGS_VALUE_KeyProcCypherIterationCount,
        tmpInteger);


      // Only store the key processing cypher if the key is to be processed
      // with > 0 iterations of the cypher
      tmpInteger := seKeyProcCypherIterations.Value * 1000;
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

          startOfVolFile := (rgSectorIVSectorZeroPos.ItemIndex = 0);
          startOfEndData := (rgSectorIVSectorZeroPos.ItemIndex = 1);

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
      tmpInt64 := se64UnitOffset.Value;
      settingsFile.WriteString(SETTINGS_SECTION_FILE_OPTIONS, SETTINGS_VALUE_Offset,
        IntToStr(tmpInt64));


      tmpInt64 := se64UnitSizeLimit.Value;
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


      tmpBoolean := ckMountReadonly.Checked;
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
  frmCypherInfo.ShowCypherDetailsDlg(deviceName, GUID);

end;


 // ----------------------------------------------------------------------------
 // Generate a Linux volume's volume key
 // hashWithAs - If set to FALSE, the user's key will be seeded and hashed once
 //              the result will be right padded with 0x00 chars/truncated as
 //              appropriate
 //              If set to TRUE, then:
 //                If "targetKeylengthBits" is > -1, the user's key will be
 //                seeded and hashed once
 //                Otherwise, the volume key will be created by concatenating
 //                  Hashing the user's seeded key
 //                  Hashing 'A' + the user's seeded key
 //                  Hashing 'AA' + the user's seeded key
 //                  Hashing 'AAA' + the user's seeded key
 //                etc - until the required number of bits is reached
class function TfrmKeyEntryPlainLinux.GenerateLinuxVolumeKey(hashDriver: Ansistring;
  hashGUID: TGUID; userKey: Ansistring; seed: Ansistring; hashWithAs: Boolean;
  targetKeylengthBits: Integer; var volumeKey: TSDUBytes): Boolean;
var
  seededKey:    Ansistring;
  hashOutput:   TSDUBytes;
  len:          Integer;
  volumeKeyStr: Ansistring;
begin
  Result := True;

  // Salt the user's password
  seededKey := userKey + seed;


  // If the target key length is any size key, or we don't hash with A's, then
  // we simply hash the user's key once with the specified hash
  if ((targetKeylengthBits = -1) or not (hashWithAs)) then begin
    Result := GetFreeOTFEBase().HashData(hashDriver, hashGUID,
      SDUStringToSDUBytes(seededKey), volumeKey);
  end else begin
    // We're been asked to supply a volume key with a fixed, defined length
    while (Result and (Length(volumeKey) < (targetKeylengthBits div 8))) do begin
      // Note: We'll truncate later - don't worry if volumeKey is set to too
      //       many bits at this stage...
      Result := Result and GetFreeOTFEBase().HashData(hashDriver, hashGUID,
        SDUStringToSDUBytes(seededKey), hashOutput);
      //      volumeKey := volumeKey +  hashOutput;
      len    := length(volumeKey) + length(hashOutput);
      SDUAddArrays(volumeKey, hashOutput);
      assert(length(volumeKey) = len); //passed
      // Prepare for next...         SDUBytesToString(
      seededKey := 'A' + seededKey;
    end;

  end;


  // If the target key length is > -1, truncate or we right-pad with 0x00
  // as appropriate
  if Result then begin
    if (targetKeylengthBits > -1) then begin
      // Copy as much of the hash value as possible to match the key length

      // Right-pad if needed
      volumeKeyStr := SDUBytesToString(volumeKey);
      volumeKeyStr := Copy(volumeKeyStr, 1, min((targetKeylengthBits div 8),
        length(volumeKeyStr)));
      volumeKeyStr := volumeKeyStr + StringOfChar(AnsiChar(#0),
        ((targetKeylengthBits div 8) - Length(volumeKeyStr)));

      SafeSetLength(volumeKey, targetKeylengthBits div 8);
      assert(volumeKeyStr = SDUBytesToString(volumeKey)); //passed
    end;

  end;


  if not Result then
    GetFreeOTFEBase().LastErrorCode := OTFE_ERR_HASH_FAILURE;
end;

// Display message only if not Silent
function TfrmKeyEntryPlainLinux._SilencableMessageDlg(Content: String;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  Result := mrOk;
  if not GetCmdLine.isSilent then
    Result := SDUMessageDlg(Content, DlgType, Buttons, 0);
end;


function MountPlainLinux(volumeFilename: String; var mountedAs: DriveLetterChar;
  ReadOnly: Boolean = False; lesFile: String = ''; password: TSDUBytes = nil;
  offset: ULONGLONG = 0;{silent: Boolean = False;} createVol: Boolean = False;
  isHidden: Boolean = False): TMountresult;
var
  keyEntryDlg: TfrmKeyEntryPlainLinux;
  mr:          Integer;
begin
  Result := morOK;

  GetFreeOTFEBase().LastErrorCode := OTFE_ERR_SUCCESS;
  //  Result        := True;
  mountedAs := #0;
  GetFreeOTFEBase().CheckActive();


  keyEntryDlg := TfrmKeyEntryPlainLinux.Create(nil);
  try
    keyEntryDlg.SetOffset(offset); // do before keyfile as that has offset in
    if (lesFile <> '') then
      keyEntryDlg.LoadSettings(lesFile);
    keyEntryDlg.SetReadonly(ReadOnly);
    keyEntryDlg.SetKey(SDUBytesToString(password));
    keyEntryDlg.fileName := volumeFilename;

    keyEntryDlg.createVol := createVol;
    keyEntryDlg.isHidden  := isHidden;
    mr                    := keyEntryDlg.ShowModal();

    // Get user password, drive letter to use, etc
    if (mr = mrCancel) then begin
      //          GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
      Result := morCancel;
    end else begin
      if (mr = mrOk) then begin
        mountedAs := keyEntryDlg.GetmountedAs();
      end;
    end;

  finally
    keyEntryDlg.Free()
  end;
end;



// ----------------------------------------------------------------------------
{$IFDEF LINUX_DETECT}
// This function was included to allow the detection of Linux encrypted
// volume's settings
// This function is *not* normally built, and it's inclusion is only for
// development/diagnostic purposes; not for general use
// It operates by being given the volume's password, and the filename of a file
// stored on the encrypted volume.
// This function cycles through all possible combinations of Linux settings for
// that password, attempting to mount the volume using the given password.
// It does not terminate on a successful mount, as there may be more than one
// set of settings which mount correctly
// IMPORTANT: The encrypted volume *must* be formatted with a filesystem
//            MS Windows understands (e.g. FAT)
// WARNING: This procedure can take some time to complete(!)
// WARNING: Some parts of this function may be commented out in order to
//          increase it's speed, but at a cost of not checking all combinations
// NOTE: Makes use of GExpert's (3rd party) debugging tool to pump out details
//       of successful mounts before this function returns
function TOTFEFreeOTFE.DetectLinux(
  volumeFilename: string;
  userKey: string;
  keyProcSeed: string;
  keyProcIterations: integer;
  fileOptOffset: int64;
  fileOptSize: int64;
  mountDriveLetter: char;
  mountMountAs: TFreeOTFEMountAs;
  testFilename: string
  ): string;
var
  deviceName: string;
  i: integer;
  volumeKey: string;

  keyProcHashDriver: string;
  keyProcHashGUID: TGUID;
  keyProcHashWithAs: boolean;
  keyProcCypherDriver: string;
  keyProcCypherGUID: TGUID;
  mainCypherDriver: string;
  mainCypherGUID: TGUID;
  mainIVHashDriver: string;
  mainIVHashGUID: TGUID;
  mainIVCypherDriver: string;
  mainIVCypherGUID: TGUID;
  currDriveLetter: Ansichar;
  startOfVolFile: boolean;
  startOfEndData: boolean;
  VolumeFlags: DWORD;
  mr: integer;
  mainCypherDetails: TFreeOTFECypher;

  hashDrivers: array of TFreeOTFEHashDriver;
  cypherDrivers: array of TFreeOTFECypherDriver;

  kph_i, kph_j: integer;
  kpc_i, kpc_j: integer;
  mc_i, mc_j: integer;
  hashWithAsInt: integer;
  ivh_i, ivh_j: integer;
  ivc_i, ivc_j: integer;
  sectorIVGenMethod: TFreeOTFESectorIVGenMethod;
  ivStartSectorInt: integer;

  kph_currDriver: TFreeOTFEHashDriver;
  kph_currImpl: TFreeOTFEHash;
  kpc_currDriver: TFreeOTFECypherDriver;
  kpc_currImpl: TFreeOTFECypher;
  mc_currDriver: TFreeOTFECypherDriver;
  mc_currImpl: TFreeOTFECypher;
  ivh_currDriver: TFreeOTFEHashDriver;
  ivh_currImpl: TFreeOTFEHash;
  ivc_currDriver: TFreeOTFECypherDriver;
  ivc_currImpl: TFreeOTFECypher;

begin
  LastErrorCode := OTFE_ERR_SUCCESS;
  Result := '';

  CheckActive();

  // Sanity checking
  // Is the user attempting to mount Linux LUKS volumes?
  // Mount as LUKS if they are...
  if (IsLUKSVolume(volumeFilename)) then     begin
    Result := Result + 'LUKS container; no need to do exhaustive search';
    exit;
    end;


  if (Result = '') then
    begin
    // Obtain details of all hashes...
    SetLength(hashDrivers, 0);
    if not(GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers))) then      begin
      Result := Result + 'Unable to get list of hash drivers.';
      end;
    end;

  if (Result = '') then
    begin
    // Obtain details of all cyphers...
    SetLength(cypherDrivers, 0);
    if not(GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers))) then       begin
      Result := Result + 'Unable to get list of cypher drivers.';
      end;
    end;


  if (Result = '') then
    begin
    currDriveLetter := GetNextDriveLetter(mountDriveLetter, #0);
    if (currDriveLetter = #0) then
      begin
      // No more drive letters following the user's specified drive
      // letter - don't mount further drives
      Result := Result + 'Out of drive letters.';
      end;
    end;


  if (Result = '') then
    begin
    // KEY PROCESSING
    // FOR ALL HASH DRIVERS...
    SendDateTime('START', now);
    for kph_i:=low(hashDrivers) to high(hashDrivers) do
      begin
      SendDebug('-kph_i: '+inttostr(kph_i)+'/'+inttostr(high(hashDrivers) - low(hashDrivers)));
      kph_currDriver := hashDrivers[kph_i];
      // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
      for kph_j:=low(kph_currDriver.Hashes) to high(kph_currDriver.Hashes) do
        begin
        SendDebug('--kph_j: '+inttostr(kph_j)+'/'+inttostr(high(kph_currDriver.Hashes) - low(kph_currDriver.Hashes)));
        kph_currImpl := kph_currDriver.Hashes[kph_j];

        keyProcHashDriver := kph_currDriver.DeviceKernelModeName;
        keyProcHashGUID := kph_currImpl.HashGUID;

        // KEY PROCESSING
        // FOR ALL CYPHER DRIVERS...
        for kpc_i:=low(cypherDrivers) to high(cypherDrivers) do
          begin
          SendDebug('---kpc_i: '+inttostr(kpc_i)+'/'+inttostr(high(cypherDrivers) - low(cypherDrivers)));
          kpc_currDriver := cypherDrivers[kpc_i];
          // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
          for kpc_j:=low(kpc_currDriver.Cyphers) to high(kpc_currDriver.Cyphers) do
            begin
            SendDebug('----kpc_j: '+inttostr(kpc_j)+'/'+inttostr(high(kpc_currDriver.Cyphers) - low(kpc_currDriver.Cyphers)));
            kpc_currImpl := kpc_currDriver.Cyphers[kpc_j];

            keyProcCypherDriver := kpc_currDriver.DeviceKernelModeName;
            keyProcCypherGUID := kpc_currImpl.CypherGUID;

//            // MAIN CYPHER
//            // FOR ALL CYPHER DRIVERS...
//            for mc_i:=low(cypherDrivers) to high(cypherDrivers) do
//              begin
//              mc_currDriver := cypherDrivers[mc_i];
//              // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
//              for mc_j:=low(mc_currDriver.Cyphers) to high(mc_currDriver.Cyphers) do
//                begin
//                mc_currImpl := kpc_currDriver.Cyphers[mc_j];

// xxx
// xxx
mc_currDriver := kpc_currDriver;
mc_currImpl := kpc_currImpl;

                mainCypherDriver := mc_currDriver.DeviceKernelModeName;
                mainCypherGUID := mc_currImpl.CypherGUID;
                mainCypherDetails := mc_currImpl;


                for hashWithAsInt:=0 to 1 do
                  begin
                  SendDebug('-----hashWithAsInt: '+inttostr(hashWithAsInt)+'/(0-1)');

                  // Generate volume key for encryption/decryption
                  if not(GenerateLinuxVolumeKey(
                                                keyProcHashDriver,
                                                keyProcHashGUID,
                                                userKey,
                                                keyProcSeed,
                                                (hashWithAsInt = 1),
                                                mainCypherDetails.KeySize,
                                                volumeKey
                                               )) then
                    begin
                    Result := Result + 'HASH FAILURE: '+keyProcHashDriver+':'+GUIDToString(keyProcHashGUID);
                    continue;
                    end;


                  // IV GENERATION
                  // FOR ALL HASH DRIVERS...
                  for ivh_i:=low(hashDrivers) to high(hashDrivers) do
                    begin
                    SendDebug('------ivh_i: '+inttostr(ivh_i)+'/'+inttostr(high(hashDrivers) - low(hashDrivers)));
                    ivh_currDriver := hashDrivers[ivh_i];
                    // FOR ALL HASHES SUPPORTED BY THE CURRENT DRIVER...
                    for ivh_j:=low(ivh_currDriver.Hashes) to high(ivh_currDriver.Hashes) do
                      begin
                      SendDebug('-------ivh_j: '+inttostr(ivh_j)+'/'+inttostr(high(ivh_currDriver.Hashes) - low(ivh_currDriver.Hashes)));
                      ivh_currImpl := ivh_currDriver.Hashes[ivh_j];

                      mainIVHashDriver := ivh_currDriver.DeviceKernelModeName;
                      mainIVHashGUID := ivh_currImpl.HashGUID;

//                      // IV GENERATION
//                      // FOR ALL CYPHER DRIVERS...
//                      for ivc_i:=low(cypherDrivers) to high(cypherDrivers) do
//                        begin
//                        ivc_currDriver := cypherDrivers[ivc_i];
//                        // FOR ALL CYPHERS SUPPORTED BY THE CURRENT DRIVER...
//                        for ivc_j:=low(ivc_currDriver.Cyphers) to high(ivc_currDriver.Cyphers) do
//                          begin
//                          ivc_currImpl := ivc_currDriver.Cyphers[ivc_j];

// xxx
ivc_currDriver := kpc_currDriver;
ivc_currImpl := kpc_currImpl;

                          mainIVCypherDriver := ivc_currDriver.DeviceKernelModeName;
                          mainIVCypherGUID := ivc_currImpl.CypherGUID;

                          // IV GENERATION
//                          for sectorIVGenMethod := low(sectorIVGenMethod) to high(sectorIVGenMethod) do
sectorIVGenMethod := foivg32BitSectorID;
                            begin
//                            SendDebug('--------sectorIVGenMethod: '+inttostr(ord(sectorIVGenMethod))+'/'+inttostr(ord(high(sectorIVGenMethod)) - loword((sectorIVGenMethod))));

//                            for ivStartSectorInt:=0 to 1 do
                              begin
ivStartSectorInt := 1;
//                              SendDebug('---------ivStartSectorInt: '+inttostr(ivStartSectorInt)+'/(0-1)');

                              startOfVolFile := (ivStartSectorInt = 0);
                              startOfEndData := (ivStartSectorInt = 1);

                              // Encode IV generation method to flags...
                              VolumeFlags := 0;
                              if (startOfVolFile) then
                                begin
                                VolumeFlags := VolumeFlags OR VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART;
                                end;


                              // Attempt to create a new device to be used
                              deviceName := CreateDiskDevice(FreeOTFEMountAsDeviceType[mountMountAs]);
                              if (deviceName <> '') then
                                begin
                                // Attempt to mount the device
                                if MountDiskDevice(
                                                   deviceName,
                                                   volumeFilename,
                                                   volumeKey,
                                                   sectorIVGenMethod,
                                                   '',  // Linux volumes don't have per-volume IVs
                                                   TRUE,
                                                   mainIVHashDriver,
                                                   mainIVHashGUID,
                                                   mainIVCypherDriver,
                                                   mainIVCypherGUID,
                                                   mainCypherDriver,
                                                   mainCypherGUID,
                                                   VolumeFlags,
                                                   '',
                                                   fileOptOffset,
                                                   fileOptSize,
                                                   FreeOTFEMountAsStorageMediaType[mountMountAs]
                                                  ) then
                                  begin
                                  if DosDeviceSymlink(
                                                      TRUE,
                                                      deviceName,
                                                      currDriveLetter,
                                                      TRUE
                                                     ) then
                                    begin
                                    // Test if mounted OK
                                    if FileExists(currDriveLetter+':'+testFilename) then
                                      begin
                                      Result := Result + '---------------------------------------------------------------';
                                      Result := Result + 'OK!';
                                      Result := Result + 'mountedAs: '+currDriveLetter;
                                      Result := Result + 'kph: '+keyProcHashDriver +':'+GUIDToString(keyProcHashGUID );
                                      Result := Result + 'kpc: '+keyProcCypherDriver +':'+GUIDToString(keyProcCypherGUID );
                                      Result := Result + 'mc: '+mainCypherDriver +':'+GUIDToString(mainCypherGUID);
                                      Result := Result + 'hashWithAsInt: '+inttostr(hashWithAsInt);
                                      Result := Result + 'ivh: '+mainIVHashDriver +':'+GUIDToString(mainIVHashGUID );
                                      Result := Result + 'ivc: '+mainIVCypherDriver +':'+GUIDToString(mainIVCypherGUID );
                                      Result := Result + 'sectorIVGenMethod: '+inttostr(ord(sectorIVGenMethod));
                                      Result := Result + 'ivStartSectorInt: '+inttostr(ivStartSectorInt);
                                      Result := Result + '---------------------------------------------------------------';
SendDateTime('HIT', now);
SendDebug('Result: '+Result);
                                      end;

                                    Dismount(currDriveLetter);
                                    end
                                  else
                                    begin
                                    // Cleardown
                                    DismountDiskDevice(deviceName, TRUE);
                                    DestroyDiskDevice(deviceName);
                                    end;  // ELSE PART - if DefineDosDevice(

                                  end
                                else
                                  begin
                                  // Cleardown
                                  DestroyDiskDevice(deviceName);
                                  end;  // ELSE PART - if MountDiskDevice(

                                end;  // ELSE PART - if (deviceName <> '') then


                              end;  // for ivStartSectorInt:=0 to 1 do
                            end;  // for sectorIVGenMethod := low(sectorIVGenMethod) to high(sectorIVGenMethod) do
//                          end;  // for ivc_j:=low(ivc_currDriver.Cyphers) to high(ivc_currDriver.Cyphers) do
//                        end;  // for ivc_i:=low(cypherDrivers) to high(cypherDrivers) do
                      end;  // for ivh_j:=low(ivh_currDriver.Hashes) to high(ivh_currDriver.Hashes) do
                    end;  // for ivh_i:=low(hashDrivers) to high(hashDrivers) do
                  end;  // for hashWithAsInt:=0 to 1 do
//                end;  // for mc_j:=low(mc_currDriver.Cyphers) to high(mc_currDriver.Cyphers) do
//              end;  // for mc_i:=low(cypherDrivers) to high(cypherDrivers) do
            end;  // for kpc_j:=low(kpc_currDriver.Cyphers) to high(kpc_currDriver.Cyphers) do
          end;  // for kpc_i:=low(cypherDrivers) to high(cypherDrivers) do
        end;  // for kph_j:=low(kph_currDriver.Hashes) to high(kph_currDriver.Hashes) do
      end;  // for kph_i:=low(hashDrivers) to high(hashDrivers) do
    end;  //  if (Result = '') then

SendDateTime('FINISHED', now);


end;
{$ENDIF}


{done: afaict only ever 1 filename passed in, also dsn work if > 1}
(*
function MountLinux(volumeFilename: String;
  var mountedAs: DriveLetterChar; ReadOnly: Boolean = False; lesFile: String = '';
  password: TSDUBytes = nil; keyfile: String = '';
  keyfileIsASCII: Boolean = LINUX_KEYFILE_DEFAULT_IS_ASCII;
  keyfileNewlineType: TSDUNewline = LINUX_KEYFILE_DEFAULT_NEWLINE; offset: ULONGLONG = 0;
  silent: Boolean = False; forceHidden: Boolean = False; createVol: Boolean = False): Boolean;

  // emptyIV :TSDUBytes;
begin


  // Sanity checking
  //  if (volumeFilenames.Count = 0) then begin
  //    mountedAs := '';
  //    Result    := True;
  //    exit;
  //  end else begin
  // Is the user attempting to mount Linux LUKS volumes?
  // Mount as LUKS if they are...
  {  TDK change - test user option to force non-luks (for hidden vols) }
  if IsLUKSVolume(volumeFilename) and not forceHidden then
    Result := LUKSTools.MountLUKS(volumeFilename, mountedAs, ReadOnly, password,
      keyfile, keyfileIsASCII, keyfileNewlineType, silent)
  else
    Result  := MountPlainLinux(volumeFilename,mountedAs,ReadOnly,lesFile,password,offset,silent,createVol);

end;
 *)
function CreateFileWithPrompt(var fileName: String; out user_cancelled: Boolean): Boolean;
var
  volSizeDlg: TfrmNewVolumeSize;
  mr:         Integer;
begin
  Result              := False;
  user_cancelled      := False;
  volSizeDlg          := TfrmNewVolumeSize.Create(nil);
  volSizeDlg.Filename := fileName;
  try
    mr := volSizeDlg.ShowModal;
    if mr = mrCancel then begin
      user_cancelled := True;
    end else begin
      if mr = mrOk then begin
        fileName := volSizeDlg.Filename;
        if SDUCreateLargeFile(fileName, volSizeDlg.VolumeSize, True, user_cancelled) then begin
          Result := True;
        end;
      end;
    end;
  finally
    volSizeDlg.Free();
  end;

end;

end.
