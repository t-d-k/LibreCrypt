unit OTFEFreeOTFE_frmWizardCreateVolume;
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
  StdCtrls, ComCtrls, ExtCtrls, MouseRNG,
  Shredder,
  SDUGeneral,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_VolumeFileAPI,  // Required for TVOLUME_CRITICAL_DATA
  OTFEFreeOTFE_DriverAPI,  // Required for CRITICAL_DATA_LEN
  PasswordRichEdit, Spin64,
  OTFEFreeOTFE_WizardCommon, SDUStdCtrls, OTFEFreeOTFE_fmeSelectPartition,
  OTFEFreeOTFE_PasswordRichEdit, SDUForms, SDUFrames, SDUSpin64Units,
  OTFEFreeOTFE_frmWizard, OTFEFreeOTFE_InstructionRichEdit, SDUDialogs;


type
  TfrmWizardCreateVolume = class(TfrmWizard)
    tsWelcome: TTabSheet;
    tsFilename: TTabSheet;
    tsHashCypherIV: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    cbHash: TComboBox;
    cbCypher: TComboBox;
    tsRNGSelect: TTabSheet;
    tsSize: TTabSheet;
    Label6: TLabel;
    tsOffset: TTabSheet;
    pnlWarningOffset: TPanel;
    lblWarningOffset: TLabel;
    tsPassword: TTabSheet;
    reInstructFilename: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructOffset: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructSize: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructHashCypherIV: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructRNGSelect: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPassword: TOTFEFreeOTFE_InstructionRichEdit;
    Label13: TLabel;
    lblWelcomeBanner: TLabel;
    lblWelcomeClickNext: TLabel;
    GroupBox1: TGroupBox;
    pbBrowseFilename: TButton;
    lblFilename: TSDUFilenameLabel;
    Label17: TLabel;
    Label7: TLabel;
    SaveDialog: TSDUSaveDialog;
    tsRNGMouseMovement: TTabSheet;
    reInstructRNGMouseMovement: TOTFEFreeOTFE_InstructionRichEdit;
    tsSummary: TTabSheet;
    reSummary: TRichEdit;
    tsRNGGPG: TTabSheet;
    reInstructRNGGPG: TOTFEFreeOTFE_InstructionRichEdit;
    GroupBox2: TGroupBox;
    lblGPGFilename: TSDUFilenameLabel;
    pbBrowseGPG: TButton;
    GPGOpenDialog: TSDUOpenDialog;
    MouseRNG: TMouseRNG;
    tsMasterKeyLength: TTabSheet;
    reInstructMasterKeyLen: TOTFEFreeOTFE_InstructionRichEdit;
    Label2: TLabel;
    seMasterKeyLength: TSpinEdit64;
    Label9: TLabel;
    lblMouseRNGBits: TLabel;
    pbHashInfo: TButton;
    pbCypherInfo: TButton;
    preUserKey1: TOTFEFreeOTFE_PasswordRichEdit;
    preUserKey2: TOTFEFreeOTFE_PasswordRichEdit;
    Label3: TLabel;
    tsFileOrPartition: TTabSheet;
    tsPartitionSelect: TTabSheet;
    reInstructFileOrPartition: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPartitionSelect: TOTFEFreeOTFE_InstructionRichEdit;
    rgFileOrPartition: TRadioGroup;
    Label21: TLabel;
    ckPartitionHidden: TCheckBox;
    ckUsePerVolumeIV: TCheckBox;
    cbSectorIVGenMethod: TComboBox;
    lblSectorIVGenMethod: TLabel;
    tsPartitionWarning: TTabSheet;
    gbRNG: TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI: TCheckBox;
    ckRNGcryptlib: TCheckBox;
    ckRNGGPG: TCheckBox;
    ckRNGPKCS11: TCheckBox;
    tsRNGPKCS11: TTabSheet;
    cbToken: TComboBox;
    lblToken: TLabel;
    pbRefresh: TButton;
    reInstructRNGPKCS11: TOTFEFreeOTFE_InstructionRichEdit;
    ckSizeEntirePartitionDisk: TCheckBox;
    lblPartitionDiskSize: TLabel;
    fmeSelectPartition: TfmeSelectPartition;
    pbAdvanced: TButton;
    reInstructSummary: TOTFEFreeOTFE_InstructionRichEdit;
    pnlWarningBorder_L: TPanel;
    pnlWarningPartition: TPanel;
    lblWarningPartition: TLabel;
    Label28: TLabel;
    pnlWarningBorder_R: TPanel;
    pnlWarningBorder_T: TPanel;
    pnlWarningBorder_B: TPanel;
    se64UnitByteOffset: TSDUSpin64Unit_Storage;
    se64UnitSize: TSDUSpin64Unit_Storage;
    reInstructWelcome: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPartitionWarning: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructWarningOffset: TOTFEFreeOTFE_InstructionRichEdit;
    ckAutoMountAfterCreate: TCheckBox;
    procedure pbBrowseFilenameClick(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure edByteOffsetChange(Sender: TObject);
    procedure SizeChanged(Sender: TObject);
    procedure pbBrowseGPGClick(Sender: TObject);
    procedure MouseRngByteGenerated(Sender: TObject; random: Byte);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbHashCypherIVGenChange(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);
    procedure seMasterKeyLengthChange(Sender: TObject);
    procedure seMasterKeyLengthExit(Sender: TObject);
    procedure pbHashInfoClick(Sender: TObject);
    procedure pbCypherInfoClick(Sender: TObject);
    procedure rbCDBLocationClick(Sender: TObject);
    procedure ckPartitionHiddenClick(Sender: TObject);
    procedure rgFileOrPartitionClick(Sender: TObject);
    procedure seSizeExit(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure ckSizeEntirePartitionDiskClick(Sender: TObject);
    procedure seSizeChange(Sender: TObject);
    procedure pbAdvancedClick(Sender: TObject);
    procedure se64ByteOffsetChange(Sender: TObject);
  private
    // Advanced options...
    FKeyIterations: integer;
    FSaltLength: integer;  // Length in *bits*
    FRequestedDriveLetter: ansichar;
    FCDBFilename: string;
    FPaddingLength: ULONGLONG;
    FPadWithEncryptedData: boolean;

    FNewVolumeMountedAs: Ansichar;

    // These are ordered lists corresponding to the items shown in the combobox
    hashKernelModeDriverNames: TStringList;
    hashGUIDs: TStringList;
    cypherKernelModeDriverNames: TStringList;
    cypherGUIDs: TStringList;
    IVGenMethodIDs: TStringList;

    deviceList: TStringList;
    deviceTitle: TStringList;

    // Used for overwriting
    TempCypherDetails: TFreeOTFECypher_v3;
    TempCypherUseKeyLength: integer;  // In *bits*
    TempCypherKey: Ansistring;
    TempCypherEncBlockNo: int64;

    fCombinedRandomData: ansistring;

    CanUseCryptlib: boolean;
    FPKCS11TokensAvailable: boolean;

    // When the user selects a cypher, we cache it's keysize for later use.
    // This is set to CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE to indicate no keysize is cached
    fCachedCypherKeysize: integer;
    // When the user selects a cypher, we cache it's blocksize for later use.
    // This is set to CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE to indicate no blocksize is cached
    fCachedCypherBlocksize: integer;
    // When the user selects a cypher, we cache it's mode for later use.
    fCachedCypherMode: TFreeOTFECypherMode;


    // Returns the keysize for the user's selected cypher, as returned by the
    // driver
    function  SelectedCypherKeySize(): integer;
    // Returns the blocksize for the user's selected cypher, as returned by the
    // driver
    function  SelectedCypherBlockSize(): integer;
    // Returns the cypher mode for the user's selected cypher, as returned by
    // the driver
    function  SelectedCypherMode(): TFreeOTFECypherMode;

    function  GetRNGSet(): TRNGSet;

    function  GetIsPartition(): boolean;
    function  GetVolFilename(): Ansistring;
    procedure SetVolFilename(filename: Ansistring);
    function  GetOffset(): ULONGLONG;
    // Note: The size returned *excludes* the size of the critical data
    function  GetSize(): ULONGLONG;
    function  GetHashDriver(): Ansistring;
    function  GetHashGUID(): TGUID;
    function  GetCypherDriver(): Ansistring;
    function  GetCypherGUID(): TGUID;
    function  GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
    procedure SetSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod);
    function  GetUsePerVolumeIV(): boolean;
    function  GetMasterKeyLength(): integer;  // Returns length in *bits*
    function  GetRandomData_CDB(): Ansistring;
    function  GetRandomData_PaddingKey(): Ansistring;
    function  GetPassword(): Ansistring;

    function  GetIsHidden(): boolean;
    function  GetCDBInVolFile(): boolean;

    function  GetAutoMountAfterCreate(): boolean;

    procedure PopulateHashes();
    procedure PopulateCyphers();
    procedure PopulateSectorIVGenMethods();
    procedure PopulatePKCS11Tokens();

    procedure PopulateSummary();

    procedure GetCDBFileAndOffset(out cdbFile: string; out cdbOffset: ULONGLONG);

    function  OverwritePadding(paddingOffset: ULONGLONG): boolean;
    procedure GenerateOverwriteData(
                                    Sender: TObject;
                                    passNumber: integer;
                                    bytesRequired: cardinal;
                                    var generatedOK: boolean;
                                    var outputBlock: TShredBlock
                                   );

    function  CreateNewVolume(): boolean;
    procedure PostCreate();
    procedure PostMount_Format();

  protected
    procedure SetupInstructions(); override;

    procedure EnableDisableControls(); override;
    procedure CheckCurrentTabComplete(); override;
    function  CheckTabComplete(checkTab: TTabSheet): boolean;

    function  SkipTab(tabSheet: TTabSheet): boolean; override;

    function  RNGRequiredBits(): integer;

    procedure FormWizardStepChanged(Sender: TObject);
  public
    property IsPartition: boolean read GetIsPartition;
    property VolFilename: Ansistring read GetVolFilename write SetVolFilename;
    property Offset: ULONGLONG read GetOffset;
    // Note: The size returned *excludes* the size of the critical data
    property Size: ULONGLONG read GetSize;
    // HashDriver - Kernel drivers: HashKernelDeviceName
    //              DLL drivers:    HashLibFilename
    property HashDriver: Ansistring read GetHashDriver;
    property HashGUID: TGUID read GetHashGUID;
    // CypherDriver - Kernel drivers: CypherKernelDeviceName
    //                DLL drivers:    CypherLibFilename
    property CypherDriver: Ansistring read GetCypherDriver;
    property CypherGUID: TGUID read GetCypherGUID;
    property SectorIVGenMethod: TFreeOTFESectorIVGenMethod read GetSectorIVGenMethod;
    property UsePerVolumeIV: boolean read GetUsePerVolumeIV;
    property MasterKeyLength: integer read GetMasterKeyLength;  // In *bits*
    property RandomData_CDB: Ansistring read GetRandomData_CDB;
    property RandomData_PaddingKey: Ansistring read GetRandomData_PaddingKey;
    property Password: Ansistring read GetPassword;

    property KeyIterations: integer read FKeyIterations write FKeyIterations;
    property SaltLength: integer read FSaltLength write FSaltLength;  // In *bits*
    property RequestedDriveLetter: ansichar read FRequestedDriveLetter write FRequestedDriveLetter;
    property KeyFilename: string read FCDBFilename write FCDBFilename;
    property CDBInVolFile: boolean read GetCDBInVolFile;
    property PaddingLength: ULONGLONG read FPaddingLength write FPaddingLength;
    property PadWithEncryptedData: boolean read FPadWithEncryptedData write FPadWithEncryptedData;

    property AutoMountAfterCreate: boolean read GetAutoMountAfterCreate;
    property NewVolumeMountedAs: Ansichar read FNewVolumeMountedAs write FNewVolumeMountedAs;

    property IsHidden: boolean read GetIsHidden;

    procedure fmeSelectPartitionChanged(Sender: TObject);

  end;


implementation

{$R *.DFM}

uses
  Math,
  ActiveX,  // Required for IsEqualGUID
  ComObj,  // Required for StringToGUID
  SDUi18n,
  OTFEConsts_U, // Required for OTFE_ERR_USER_CANCEL
  OTFEFreeOTFEDLL_U,
  OTFEFreeOTFE_PKCS11,
  OTFEFreeOTFE_frmWizardCreateVolumeAdvanced,
  OTFEFreeOTFEDLL_PartitionImage,
  SDFilesystem,
  SDFilesystem_FAT;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  // If they're available, use these as defaults...
  DEFAULT_CYPHER_TITLE = 'AES (256 bit XTS)';
  DEFAULT_HASH_TITLE   = 'SHA-512';

  DEFAULT_SECTOR_IV_GEN_METHOD = foivg64BitSectorID;

  DUMMY_GUID = '{00000000-0000-0000-0000-000000000000}';

  CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE     = -9999;
  CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE = -9999;

  WARNING_BORDER_WIDTH = 10;

  // (4GB - 1 byte) is the max file a FAT32 system can support
  MAX_FAT_FILESIZE: ULONGLONG = ((ULONGLONG(4) * ULONGLONG(BYTES_IN_GIGABYTE)) - ULONGLONG(1));

resourcestring
  FILEORPART_OPT_VOLUME_FILE = 'Volume file';
  FILEORPART_OPT_PARTITION   = 'Partition/entire disk';


procedure TfrmWizardCreateVolume.FormShow(Sender: TObject);
var
  i: integer;
  idx: integer;
begin
  inherited;

  // Center label (done dynamically due to language translation size
  // differences)
  SDUCenterControl(lblWelcomeBanner, ccHorizontal);
  SDUCenterControl(lblWelcomeClickNext, ccHorizontal);
  SDUCenterControl(lblWarningPartition, ccHorizontal);
  SDUCenterControl(lblWarningOffset, ccHorizontal);

  fCombinedRandomData := '';

  KeyIterations:= DEFAULT_KEY_ITERATIONS;
  SaltLength:= DEFAULT_SALT_LENGTH;
  RequestedDriveLetter:= #0;
  KeyFilename:= '';
  PaddingLength:= 0;
  PadWithEncryptedData:= FALSE;
  TempCypherUseKeyLength := 0;

  NewVolumeMountedAs := #0;

  pnlWarningOffset.Caption := '';
  pnlWarningPartition.Caption := '';


  // Create a border around the warning
  // We can't just place a bigger panel under the warning panel, as on a
  // Windows system with themes, the top panel turns read, and the (red) text
  // can't be read. 
  SDUClearPanel(pnlWarningBorder_L);
  SDUClearPanel(pnlWarningBorder_R);
  SDUClearPanel(pnlWarningBorder_T);
  SDUClearPanel(pnlWarningBorder_B);
  pnlWarningBorder_L.Left   := pnlWarningPartition.left - WARNING_BORDER_WIDTH;
  pnlWarningBorder_L.Width  := WARNING_BORDER_WIDTH;
  pnlWarningBorder_L.Top    := pnlWarningPartition.Top - WARNING_BORDER_WIDTH;
  pnlWarningBorder_L.Height := pnlWarningPartition.Height + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_R.Left   := pnlWarningPartition.left + pnlWarningPartition.width;
  pnlWarningBorder_R.Width  := WARNING_BORDER_WIDTH;
  pnlWarningBorder_R.Top    := pnlWarningPartition.Top - WARNING_BORDER_WIDTH;
  pnlWarningBorder_R.Height := pnlWarningPartition.Height + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_T.Left   := pnlWarningPartition.left - WARNING_BORDER_WIDTH;
  pnlWarningBorder_T.Width  := pnlWarningPartition.width + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_T.Top    := pnlWarningPartition.Top - WARNING_BORDER_WIDTH;
  pnlWarningBorder_T.Height := WARNING_BORDER_WIDTH;
  pnlWarningBorder_B.Left   := pnlWarningPartition.left - WARNING_BORDER_WIDTH;
  pnlWarningBorder_B.Width  := pnlWarningPartition.width + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_B.Top    := pnlWarningPartition.Top + pnlWarningPartition.Height;
  pnlWarningBorder_B.Height := WARNING_BORDER_WIDTH;


  // tsFileOrPartition
  rgFileOrPartition.Items.Clear();
  rgFileOrPartition.Items.Add(FILEORPART_OPT_VOLUME_FILE);
  rgFileOrPartition.Items.Add(FILEORPART_OPT_PARTITION);
  rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  fmeSelectPartition.FreeOTFEObj := FreeOTFEObj;
  fmeSelectPartition.AllowCDROM := FALSE;
  fmeSelectPartition.OnChange := fmeSelectPartitionChanged;
  fmeSelectPartition.Initialize();
  fmeSelectPartition.Tag := 1;

  // tsFilename
  lblFilename.Caption := '';

  // tsOffset
  se64UnitByteOffset.Value := 0;

  // tsSize
  // Default to 25 MB
  se64UnitSize.Value := DEFAULT_VOLUME_SIZE;

  // tsHashCypherIV
  PopulateHashes();
  PopulateCyphers();
  PopulateSectorIVGenMethods();
  // Autoselect default, if available
  if (cbHash.Items.Count>0) then
    begin
    idx := cbHash.Items.IndexOf(DEFAULT_HASH_TITLE);
    if (idx = -1) then
      begin
      // If we can't find the default, force the user to select manually
      idx := -1;
      end;
    cbHash.ItemIndex := idx;
    end;
  // Autoselect default, if available
  if (cbCypher.Items.Count>0) then
    begin
    idx := cbCypher.Items.IndexOf(DEFAULT_CYPHER_TITLE);
    if (idx = -1) then
      begin
      // If we can't find the default, force the user to select manually
      idx := -1;
      end;
    cbCypher.ItemIndex := idx;
    end;

  // Default sector IV generation method
  SetSectorIVGenMethod(DEFAULT_SECTOR_IV_GEN_METHOD);

  // Default to using per-volume IVs
  ckUsePerVolumeIV.Checked := TRUE;

  // tsMasterKeyLen
  seMasterKeyLength.Increment := 8;
  seMasterKeyLength.Value := DEFAULT_CYPHER_KEY_LENGTH;  // In bits

  // tsRNG
  ckRNGCryptoAPI.checked := TRUE;
  ckRNGcryptlib.Enabled := CanUseCryptlib;
  ckRNGPKCS11.Enabled :=  PKCS11LibraryReady(FreeOTFEObj.PKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  PopulatePKCS11Tokens();

  // tsRNGGPG
  lblGPGFilename.Caption := '';

  // tsPassword
  preUserKey1.Plaintext := TRUE;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey1.WantReturns := TRUE;
  preUserKey1.WordWrap := TRUE;
  preUserKey1.Lines.Clear();
  preUserKey1.PasswordChar := FreeOTFEObj.PasswordChar;
  preUserKey1.WantReturns  := FreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey1.WantTabs     := FreeOTFEObj.AllowTabsInPasswords;

  preUserKey2.Plaintext := TRUE;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey2.WantReturns := TRUE;
  preUserKey2.WordWrap := TRUE;
  preUserKey2.Lines.Clear();
  preUserKey2.PasswordChar := FreeOTFEObj.PasswordChar;
  preUserKey2.WantReturns  := FreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey2.WantTabs     := FreeOTFEObj.AllowTabsInPasswords;


  // Set all tabs to show that they have not yet been completed
  for i:=0 to (pcWizard.PageCount-1) do
    begin
    pcWizard.Pages[i].TabVisible := FALSE;
    // Each tabsheet's tag indicates if that tab has been completed or not; 1 for
    // completed, 0 for not yet complete
    pcWizard.Pages[i].Tag := 0;
    end;


  // BECAUSE WE DEFAULTED TO USING A VOLUME *FILE*, WE MUST SET THE PARTITION
  // SELECT PAGE TO "DONE" HERE (otherwise, it'll never be flagged as complete
  // if the user doesn't switch)
  tsPartitionWarning.Tag := 1;
  tsPartitionSelect.Tag := 1;

  // BECAUSE WE DEFAULTED TO USING THE MOUSERNG, WE MUST SET THE GPG PAGE TO
  // "DONE" HERE (otherwise, it'll never be flagged as complete if the user
  // doens't switch RNGs)
  tsRNGGPG.Tag := 1;


  // Summary tab marked as completed
  tsSummary.Tag := 1;
  ckAutoMountAfterCreate.Checked := TRUE;
  // Note: ckAutoMountAfterCreate.Visible set in EnableDisableControls(...);
  //       if it's set to FALSE here, the control still appears for some
  //       reason?!


  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;

  CheckCurrentTabComplete();

end;


// Returns TRUE if the specified tab should be skipped, otherwise FALSE
function TfrmWizardCreateVolume.SkipTab(tabSheet: TTabSheet): boolean;
var
  sheetSkipped: boolean;
begin
  sheetSkipped:= inherited SkipTab(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then
    begin
    sheetSkipped := (FreeOTFEObj is TOTFEFreeOTFEDLL);
    end;

  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsFilename) then
    begin
    sheetSkipped := IsPartition;
    end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionWarning) then
    begin
    sheetSkipped := not(IsPartition);
    end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then
    begin
    sheetSkipped := not(IsPartition);
    end;

  // Skip offset if user is't creating a hidden volume
  if (tabSheet = tsOffset) then
    begin
    sheetSkipped := not(IsHidden);
    end;

  // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
  if (tabSheet = tsMasterKeyLength) then
    begin
    if (
        (cbHash.ItemIndex > -1) and
        (cbCypher.ItemIndex > -1)
       ) then
      begin
      sheetSkipped := (SelectedCypherKeySize() >= 0);
      end;
    end;

  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then
    begin
    sheetSkipped := not(rngMouseMovement in GetRNGSet());
    end;

  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
    begin
    sheetSkipped := not(rngPKCS11 in GetRNGSet());
    end;

  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then
    begin
    sheetSkipped := not(rngGPG in GetRNGSet());
    end;

  Result := sheetSkipped;
end;


procedure TfrmWizardCreateVolume.pbAdvancedClick(Sender: TObject);
var
  dlg: TfrmWizardCreateVolumeAdvanced;
begin
  dlg:= TfrmWizardCreateVolumeAdvanced.Create(self);
  try
    dlg.CypherBlocksize := SelectedCypherBlockSize();

    dlg.KeyIterations := KeyIterations;
    dlg.SaltLength := SaltLength;
    dlg.DriveLetter := RequestedDriveLetter;
    dlg.CDBFilename := KeyFilename;
    dlg.PaddingLength:= PaddingLength;
    dlg.PadWithEncryptedData:= PadWithEncryptedData;

    if (dlg.ShowModal = mrOK) then
      begin
      KeyIterations := dlg.KeyIterations;
      SaltLength := dlg.SaltLength;
      RequestedDriveLetter := dlg.DriveLetter;
      KeyFilename := dlg.CDBFilename;
      PaddingLength := dlg.PaddingLength;
      PadWithEncryptedData := dlg.PadWithEncryptedData;

      // If padding with encrypted data, note the size of they key required
      TempCypherUseKeyLength := 0;
      if (
          (PaddingLength > 0) and
          PadWithEncryptedData
         ) then
        begin
        FreeOTFEObj.GetSpecificCypherDetails(CypherDriver, CypherGUID, TempCypherDetails);

        // Get *real* random data for encryption key
        if (TempCypherDetails.KeySizeRequired < 0) then
          begin
          // -ve keysize = arbitary keysize supported - just use 512 bits
          TempCypherUseKeyLength := 512;
          end
        else if (TempCypherDetails.KeySizeRequired > 0) then
          begin
          // +ve keysize = specific keysize - get sufficient bits
          TempCypherUseKeyLength := TempCypherDetails.KeySizeRequired;
          end;

        // If MouseRNG is being used, check that enough random data is availabe...
        if not(CheckTabComplete(tsRNGMouseMovement)) then
          begin
          SDUMessageDlg(
                        _('Additional random data is required in order to generate an encryption key for use when producing the padding data.'),
                        mtInformation
                       );
          pcWizard.ActivePage := tsRNGMouseMovement;
          end;

        end;

      PopulateSummary();
      end;
  finally
    dlg.Free();
  end;

end;


procedure TfrmWizardCreateVolume.EnableDisableControls();
var
  useSectorIVs: boolean;
begin
  inherited;

  pbHashInfo.Enabled := (
                         (GetHashDriver <> '') and
                         (not(IsEqualGUID(GetHashGUID(), StringToGUID(DUMMY_GUID))))
                        );

  pbCypherInfo.Enabled := (
                           (GetCypherDriver <> '') and
                           (not(IsEqualGUID(GetCypherGUID(), StringToGUID(DUMMY_GUID))))
                          );


  // If:
  //  *) The selected cypher's blocksize is zero of variable, or
  //  *) The cypher uses LRW, or
  //  *) The cypher uses LRW
  // sector IVs aren't/cannot be used
  useSectorIVs := TRUE;
  if (cbCypher.ItemIndex >= 0) then
    begin
    useSectorIVs := (SelectedCypherBlockSize() > 0) and
                    (SelectedCypherMode() <> focmLRW) and
                    (SelectedCypherMode() <> focmXTS);
    end;

  if not(useSectorIVs) then
    begin
    ckUsePerVolumeIV.Checked := FALSE;
    SetSectorIVGenMethod(foivgNone);
    end;
  SDUEnableControl(cbSectorIVGenMethod, useSectorIVs);
  SDUEnableControl(ckUsePerVolumeIV, useSectorIVs);


  // Volume size related...
  
  // If the user is creating a volume on a partition/disk, give the user the
  // option of using the whole partition/disk's space
  // ...But if the user's creating a hidden partition - make sure they enter
  // the size

  // Weird, without this FALSE/TRUE, the ".Visible := " following it has no
  // effect; even though the ".Visible := " following *that* does?!!
  ckSizeEntirePartitionDisk.Visible := FALSE;
  ckSizeEntirePartitionDisk.Visible := TRUE;

  ckSizeEntirePartitionDisk.Visible := (
                                        IsPartition and
                                        not(ckPartitionHidden.checked) and
                                        not(fmeSelectPartition.SyntheticDriveLayout)
                                       );
  lblPartitionDiskSize.Visible := ckSizeEntirePartitionDisk.Visible;
  // If the control's not visible, don't let it be selected; the user enters
  // the size
  if not(ckSizeEntirePartitionDisk.Visible) then
    begin
    ckSizeEntirePartitionDisk.checked := FALSE;
    end;

  SDUEnableControl(se64UnitSize, not(ckSizeEntirePartitionDisk.checked));

  // Prevent user from disabling the automount when creating volumes using the
  // DLL version
  // Toggle the "automount after create" checkbox visible on/off.
  // For some reason, the Visible setting doesn't "take" if set to FALSE
  // in FormShow(...) (i.e. the control is still visible)
  ckAutoMountAfterCreate.Visible := TRUE;
  ckAutoMountAfterCreate.Visible := FALSE;
  ckAutoMountAfterCreate.Visible := not(FreeOTFEObj is TOTFEFreeOTFEDLL);

end;


procedure TfrmWizardCreateVolume.pbBrowseFilenameClick(Sender: TObject);
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
  SaveDialog.Options := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, VolFilename);
  if SaveDialog.execute then
    begin
    VolFilename := SaveDialog.Filename;
    se64UnitByteOffset.Value := 0;
    if (FileExists(VolFilename)) then
      begin
      // Force the user to reenter their offset, by marking the offset tabsheet
      // as incomplete
      tsOffset.Tag := 0;
      end
    else
      begin
      // New file; critical data automatically stored at the start of the file;
      // zero offset
      tsOffset.Tag := 1;
      end;

    end;

  CheckCurrentTabComplete();

end;


// This procedure will check to see if all items on the current tabsheet have
// been successfully completed
procedure TfrmWizardCreateVolume.CheckCurrentTabComplete();
begin
  CheckTabComplete(pcWizard.ActivePage);
end;

function TfrmWizardCreateVolume.CheckTabComplete(checkTab: TTabSheet): boolean;
var
  allOK: boolean;
  randomBitsGenerated: integer;
  testSize: int64;
  volSizeWithCDB: ULONGLONG;
  volSizeWithCDBAndPadding: ULONGLONG;
begin
  inherited;

  allOK := FALSE;

  if (checkTab = tsWelcome) then
    begin
    // Always complete...
    allOK := TRUE;
    end
  else if (checkTab = tsFileOrPartition) then
    begin
    // Ensure one option has been selected
    allOK := (rgFileOrPartition.ItemIndex >= 0);
    end
  else if (checkTab = tsFilename) then
    begin
    // If we're creating a volume within a volume file, a filename must be specified
    if (IsPartition) then
      begin
      allOK := TRUE;
      end
    else
      begin
      // Flag tabsheet complete if a filename has been specified
      allOK := (VolFilename <> '');
      end;

    end
  else if (checkTab = tsPartitionWarning) then
    begin
    // Warning only; nothing for the user to actually do on this tab
    allOK := TRUE;
    end
  else if (checkTab = tsPartitionSelect) then
    begin
    // If we're creating a volume on a partition, one must be selected
    if (not(IsPartition)) then
      begin
      allOK := TRUE;
      end
    else
      begin
      allOK := (fmeSelectPartition.SelectedDevice <> '');
      end;

    end
  else if (checkTab = tsOffset) then
    begin
    // Check that the number entered is less than the max size...
    if (IsPartition) then
      begin
      allOK := (Offset >= 0);
      end
    else
      begin
      allOK := (Offset < SDUGetFileSize(VolFilename)) AND (Offset >= 0);
      end;

    end
  else if (checkTab = tsSize) then
    begin
    // Some sensible minimum volume size
    // Use explicit int64 to prevent Delphi converting Size to int
    testSize := BYTES_IN_MEGABYTE;
    allOK := (Size >= testSize);

    if allOK then
      begin
      // Min sector size
      testSize := ASSUMED_HOST_SECTOR_SIZE;
      allOK := ((size mod testSize) = 0);
      end;

    if allOK then
      begin
      volSizeWithCDB := Size;
      if CDBInVolFile then
        begin
        volSizeWithCDB := volSizeWithCDB + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
        end;

      volSizeWithCDBAndPadding := volSizeWithCDB + PaddingLength;

      if IsPartition then
        begin
        if fmeSelectPartition.SyntheticDriveLayout then
          begin
          // We don't know the size of the selected drive/partition; assume user
          // input OK
          allOK := TRUE;
          end
        else
          begin
          // Note: fmeSelectPartition.SelectedSize() already takes into account
          //       whether or not the "entire disk" option was selected on the
          //       frame
          allOK := ((fmeSelectPartition.SelectedSize() - Offset) >= volSizeWithCDBAndPadding);
          end;
        end
      else if (FileExists(VolFilename)) then
        begin
        // If creating a hidden container, ensure that the existing file is large
        // enough to store the hidden container
        allOK := ((SDUGetFileSize(VolFilename) - Offset) >= volSizeWithCDBAndPadding);
        end
      else
        begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)
        allOK := TRUE;
        end;
      end;

    end
  else if (checkTab = tsHashCypherIV) then
    begin
    allOK := (
              (cbHash.ItemIndex > -1) and
              (cbCypher.ItemIndex > -1)
             );
    end
  else if (checkTab = tsMasterKeyLength) then
    begin
    // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
    if (SelectedCypherKeySize() >= 0) then
      begin
      allOK := TRUE;
      end
    else
      begin
      // Otherwise, the keysize must be a multiple of 8 bits
      allOK := ((seMasterKeyLength.Value mod 8) = 0);
      end;

    end
  else if (checkTab = tsRNGSelect) then
    begin
    // Must have at least one RNG selected
    allOK := (GetRNGSet() <> []);
    end
  else if (checkTab = tsRNGMouseMovement) then
    begin
    if not(rngMouseMovement in GetRNGSet) then
      begin
      // Mouse RNG not used - automatically completed
      allOK := TRUE;
      end
    else
      begin
      // This is a good place to update the display of the number of random bits
      // generated...
      randomBitsGenerated:= CountMouseRNGData();
      lblMouseRNGBits.Caption := SDUParamSubstitute(
                                                    _('Random bits generated: %1/%2'),
                                                    [
                                                     randomBitsGenerated,
                                                     RNGRequiredBits()
                                                    ]
                                                   );

      allOK := (randomBitsGenerated >= RNGRequiredBits());
      MouseRNG.Enabled := not(allOK);
      if MouseRNG.Enabled then
        begin
        MouseRNG.Color := clWindow;
        end
      else
        begin
        MouseRNG.Color := clBtnFace;
        end;
      end;

    end
  else if (checkTab = tsRNGPKCS11) then
    begin
    // Must have at least one RNG selected
    allOK := (
              FPKCS11TokensAvailable and
              (cbToken.ItemIndex >= 0)
             );
    end
  else if (checkTab = tsRNGGPG) then
    begin
    // Flag tabsheet complete if a GPG executable has been specified
    allOK := (lblGPGFilename.Caption <> '');
    end
  else if (checkTab = tsPassword) then
    begin
    // Ensure the password is confirmed, and something has been entered as a
    // password
    allOK := (preUserKey1.Text = preUserKey2.Text) and
             (preUserKey1.Text <> '');
    end
  else if (checkTab = tsSummary) then
    begin
    // Always completed
    allOK := TRUE;
    end;


  checkTab.Tag := 0;
  if allOK then
    begin
    checkTab.Tag := 1;
    end;

  EnableDisableControls();

  // This is a good time to update the stage X of Y display - any changes to
  // the tab's settings may have reduced/increased the number of stages
  UpdateStageDisplay();

  Result := allOK;
end;


procedure TfrmWizardCreateVolume.ckRNGClick(Sender: TObject);
begin
  InitMouseRNGData();

  // Set unused tabsheets as complete
  // ...RNG mouse movement
  tsRNGMouseMovement.Tag := 1;
  if ckRNGMouseMovement.checked then
    begin
    tsRNGMouseMovement.Tag := 0;
    end;

  // ...RNG GPG
  tsRNGGPG.Tag := 1;
  if ckRNGGPG.checked then
    begin
    if (lblGPGFilename.Caption = '') then
      begin
      tsRNGGPG.Tag := 0;
      end;
    end;

  // ...RNG PKCS#11 token
  tsRNGPKCS11.Tag := 1;
  if ckRNGPKCS11.checked then
    begin
    if not(
           FPKCS11TokensAvailable and
           (cbToken.ItemIndex >= 0)
          ) then
      begin
      tsRNGPKCS11.Tag := 0;
      end;
    end;


  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.ckSizeEntirePartitionDiskClick(Sender: TObject);
begin
  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.preUserKeyChange(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

function TfrmWizardCreateVolume.GetIsPartition(): boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_PARTITION));
end;

function TfrmWizardCreateVolume.GetIsHidden(): boolean;
begin
  // If:
  //   a) The user wants to create the volume on a partition, and has
  //      specified they want to create a hidden partition, or
  //   b) The user wants to create the volume within a volume file, but has
  //      specified the filename of an existing file
  Result := (
              (IsPartition) AND
              (ckPartitionHidden.checked)
            )
            OR
            (
              (not(IsPartition)) AND
              (FileExists(VolFilename))
            );
end;

function TfrmWizardCreateVolume.GetVolFilename(): Ansistring;
var
  retVal: Ansistring;
begin
  retVal := '';

  if (IsPartition) then
    begin
    retVal := fmeSelectPartition.SelectedDevice;
    end
  else
    begin
    // Note: With the label used, the Caption is the *full* filename, even
    //       though the it's display is the shortened version
    //       (e.g. "C:\..\filename.vol")
    retVal:= lblFilename.Caption;
    end;

  Result := retVal;
end;

procedure TfrmWizardCreateVolume.SetVolFilename(filename: Ansistring);
begin
  // Note: With the label used, the Caption is the *full* filename, even
  //       though the it's display is the shortened version
  //       (e.g. "C:\..\filename.vol")
  lblFilename.Caption := filename;
end;


function TfrmWizardCreateVolume.GetOffset(): ULONGLONG;
begin
  Result := se64UnitByteOffset.Value;
end;

// Note: The size returned *excludes* the size of the critical data
function TfrmWizardCreateVolume.GetSize(): ULONGLONG;
var
  tmpSize: ULONGLONG;
  critSizeULL: ULONGLONG;
begin
  if ckSizeEntirePartitionDisk.checked then
    begin
    tmpSize := fmeSelectPartition.SelectedSize();

    // If CDB forms part of the volume, reduce as appropriate
    if CDBInVolFile then
      begin
      critSizeULL:= (CRITICAL_DATA_LENGTH div 8);
      tmpSize := tmpSize - critSizeULL;
      end;

    end
  else
    begin
    // Calculate the number of bytes...
    // Note: The in64(...) cast is REQUIRED, otherwise Delphi will calculate the
    //       value in 32 bits, and assign it to the 64 bit VolumeSize
    tmpSize := ULONGLONG(se64UnitSize.Value);
    end;

  Result := tmpSize;
end;

function TfrmWizardCreateVolume.GetHashDriver(): Ansistring;
var
  retval: Ansistring;
begin
  retval := '';

  if (cbHash.ItemIndex >= 0) then
    begin
    retval := hashKernelModeDriverNames[cbHash.ItemIndex];
    end;

  Result := retval;
end;

function TfrmWizardCreateVolume.GetHashGUID(): TGUID;
var
  strGUID: string;
begin
  // Just some dummy GUID in case none selected
  strGUID := DUMMY_GUID;

  if (cbHash.ItemIndex >= 0) then
    begin
    strGUID := hashGUIDs[cbHash.ItemIndex];
    end;

  Result := StringToGUID(strGUID);
end;

function TfrmWizardCreateVolume.GetCypherDriver(): Ansistring;
var
  retval: Ansistring;
begin
  retval := '';

  if (cbCypher.ItemIndex >= 0) then
    begin
    retval := cypherKernelModeDriverNames[cbCypher.ItemIndex];
    end;

  Result := retval;
end;

function TfrmWizardCreateVolume.GetCypherGUID(): TGUID;
var
  strGUID: string;
begin
  // Just some dummy GUID in case none selected
  strGUID := DUMMY_GUID;

  if (cbCypher.ItemIndex >= 0) then
    begin
    strGUID := cypherGUIDs[cbCypher.ItemIndex];
    end;

  Result := StringToGUID(strGUID);
end;

function TfrmWizardCreateVolume.GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
var
  retVal: TFreeOTFESectorIVGenMethod;
begin
  retVal := foivgUnknown;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then
    begin
    retVal := TFreeOTFESectorIVGenMethod(cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
    end;

  Result := retVal;
end;

procedure TfrmWizardCreateVolume.SetSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod);
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
end;

function TfrmWizardCreateVolume.GetUsePerVolumeIV(): boolean;
begin
  Result := ckUsePerVolumeIV.checked;
end;


function TfrmWizardCreateVolume.GetRandomData_CDB(): Ansistring;
begin
  // Use the first CRITICAL_DATA_LENGTH bits as the CDB random data
  Result := Copy(fCombinedRandomData, 1, (CRITICAL_DATA_LENGTH div 8));
end;

function TfrmWizardCreateVolume.GetRandomData_PaddingKey(): Ansistring;
begin
  // Use the last FPadWithEncryptedDataKeyLen bits as the CDB random data,
  // after the CDB random data
  Result := Copy(fCombinedRandomData, (CRITICAL_DATA_LENGTH div 8) + 1, (TempCypherUseKeyLength div 8));
end;

function TfrmWizardCreateVolume.GetPassword(): Ansistring;
begin
{ TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  Result := preUserKey1.Text;
end;

function TfrmWizardCreateVolume.GetMasterKeyLength(): integer;
var
  retval: integer;
begin
  retval := SelectedCypherKeySize();
  if (retval < 0) then
    begin
    retval := seMasterKeyLength.Value;
    end;

  Result := retval;
end;


procedure TfrmWizardCreateVolume.edByteOffsetChange(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.SizeChanged(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.pbBrowseGPGClick(Sender: TObject);
begin
  GPGOpenDialog.Filter     := FILE_FILTER_FLT_EXECUTABLES;
  GPGOpenDialog.DefaultExt := FILE_FILTER_DFLT_EXECUTABLES;
  GPGOpenDialog.Options := GPGOpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(GPGOpenDialog, lblGPGFilename.caption);
  if GPGOpenDialog.execute then
    begin
    lblGPGFilename.caption := GPGOpenDialog.Filename;
    end;

  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.MouseRNGByteGenerated(Sender: TObject;
  random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < RNGRequiredBits()) then
    begin
    AddToMouseRNGData(random);
    end;

  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.PopulateHashes();
var
  tmpDisplayTitles: TStringList;
  i: integer;
  hashDetails: TFreeOTFEHash;
  hashAcceptable: boolean;
begin
  tmpDisplayTitles:= TStringList.Create();
  try
    if (FreeOTFEObj.GetHashList(tmpDisplayTitles, hashKernelModeDriverNames, hashGUIDs)) then
      begin
      // Strip out all hashes which have:
      //   length <= 0 or blocksize <= 0
      //  - they cannot be used to create new FreeOTFE volumes as they use
      // PBKDF2 (HMAC) to derive the critical data key; and PBKDF2 with HMAC
      // requires that the hash used has a defined length of greater than zero
      for i:=(tmpDisplayTitles.count-1) downto 0 do
        begin
        if not(FreeOTFEObj.GetSpecificHashDetails(hashKernelModeDriverNames[i], StringToGUID(hashGUIDs[i]), hashDetails)) then
          begin
          // Just warn user, and ignore...
          SDUMessageDlg(SDUParamSubstitute(_('Unable to obtain hash details for %1'), [tmpDisplayTitles[i]]), mtWarning);
          hashAcceptable := FALSE;
          end
        else
          begin
          hashAcceptable := (hashDetails.Length > 0) and (hashDetails.BlockSize > 0);
          end;


        // If the current hash algorithm is no use, don't add it to the list
        // offered to the user...
        if not(hashAcceptable) then
          begin
          tmpDisplayTitles.Delete(i);
          hashKernelModeDriverNames.Delete(i);
          hashGUIDs.Delete(i);
          end;

        end;

      // Populate the dropdown list offered to the user...
      cbHash.Items.Clear();
      cbHash.Items.AddStrings(tmpDisplayTitles);

      if (cbHash.Items.count = 0) then
        begin
        SDUMessageDlg(
                      _('You do not appear to have any FreeOTFE hash drivers that can be used to create new DoxBox volumes installed and started.')+SDUCRLF+
                      SDUCRLF+
                      _('If you have only just installed DoxBox, you may need to restart your computer.'),
                      mtError
                     );
        end;

      end
    else
      begin
      SDUMessageDlg(
                    _('Unable to obtain list of hashes.')+SDUCRLF+
                    SDUCRLF+
                    _('Please ensure that you have one or more FreeOTFE hash drivers installed and started.')+SDUCRLF+
                    SDUCRLF+
                    _('If you have only just installed DoxBox, you may need to restart your computer.'),
                    mtError
                   );
      end;
  finally
    tmpDisplayTitles.Free();
  end;

end;

procedure TfrmWizardCreateVolume.PopulateCyphers();
var
  tmpDisplayTitles: TStringList;
begin
  tmpDisplayTitles:= TStringList.Create();
  try
    if (FreeOTFEObj.GetCypherList(tmpDisplayTitles, cypherKernelModeDriverNames, cypherGUIDs)) then
      begin
      cbCypher.Items.Clear();
      cbCypher.Items.AddStrings(tmpDisplayTitles);
      end
    else
      begin
      SDUMessageDlg(
                 _('Unable to obtain list of cyphers.')+SDUCRLF+
                 SDUCRLF+
                 _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.')+SDUCRLF+
                 SDUCRLF+
                 _('If you have only just installed DoxBox, you may need to restart your computer.'),
                 mtError
                );
      end;
  finally
    tmpDisplayTitles.Free();
  end;
  
end;

procedure TfrmWizardCreateVolume.PopulateSectorIVGenMethods();
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

procedure TfrmWizardCreateVolume.FormWizardStepChanged(Sender: TObject);
begin
  inherited;

  if (pcWizard.ActivePage = tsSummary) then
    begin
    PopulateSummary();
    end
  else if (pcWizard.ActivePage = tsPartitionSelect) then
    begin
    // This shouldn't be needed, but without it, controls on this frame which
    // are set to "Visible := FALSE" by the frame remain visible.
    // Only call Initialize(...) the first time the tab is moved onto
    if (fmeSelectPartition.Tag = 1) then
      begin
      fmeSelectPartition.Initialize();
      fmeSelectPartition.Tag := 0;
      end;
    end;

end;

procedure TfrmWizardCreateVolume.FormCreate(Sender: TObject);
begin
  hashKernelModeDriverNames:= TStringList.Create();
  hashGUIDs:= TStringList.Create();
  cypherKernelModeDriverNames:= TStringList.Create();
  cypherGUIDs:= TStringList.Create();
  IVGenMethodIDs:= TStringList.Create();

  deviceList:= TStringList.Create();
  deviceTitle:= TStringList.Create();

  fCachedCypherKeysize := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  fCachedCypherMode:= focmUnknown;

  // Start cryptlib, if possible, as early as we can to allow it as much time
  // as possible to poll entropy
  CanUseCryptlib := cryptlibLoad();

  OnWizardStepChanged := FormWizardStepChanged;

end;

procedure TfrmWizardCreateVolume.FormDestroy(Sender: TObject);
var
  i: integer;
begin
{$IFDEF FREEOTFE_DEBUG}
showmessage('destroying wizard');
{$ENDIF}
  hashKernelModeDriverNames.Free();
  hashGUIDs.Free();
  cypherKernelModeDriverNames.Free();
  cypherGUIDs.Free();
  IVGenMethodIDs.Free();

  deviceList.Free();
  deviceTitle.Free();

  // Shutdown cryptlib, if used
  if CanUseCryptlib then
    begin
    cryptlibUnload();
    CanUseCryptlib := FALSE;
    end;

  PurgeMouseRNGData();

  { TODO 1 -otdk -ccleanup : whats this for? }
  for i:=1 to length(fCombinedRandomData) do
    begin
    fCombinedRandomData[i] := ansichar(i);
    end;

end;


procedure TfrmWizardCreateVolume.cbHashCypherIVGenChange(Sender: TObject);
begin
  // Cypher changed; clear any cached keysize
  fCachedCypherKeysize := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Cypher changed; clear any cached blocksize
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Cypher changed; clear any cached mode
  fCachedCypherMode:= focmUnknown;

  CheckCurrentTabComplete();

end;


procedure TfrmWizardCreateVolume.PopulateSummary();
var
  totalSize: ULONGLONG;
  driveLetter: string;
  RNGs: string;
  ULLZero: ULONGLONG;
begin
  // int64Zero to prevent 32/64 bit integer conversion
  ULLZero:= 0;

  reSummary.Lines.Clear();

  if (IsPartition) then
    begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Partition: %1'), [VolFilename]));
    end
  else
    begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Filename: %1'), [VolFilename]));
    end;

  if (IsHidden) then
    begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Hidden volume starting at offset: %1'), [Offset]));
    end;

  if CDBInVolFile then
    begin
    if (PaddingLength > ULLZero) then
      begin
      totalSize := Size + ULONGLONG(CRITICAL_DATA_LENGTH div 8) + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
                                             _('Volume size: %1 + %2 (for CDB) + %3 (padding) = %4 bytes'),
                                             [
                                              SDUIntToStr(Size),
                                              (CRITICAL_DATA_LENGTH div 8),
                                              SDUIntToStr(PaddingLength),
                                              SDUIntToStr(totalSize)
                                             ]
                                            ));
      end
    else
      begin
      totalSize := Size + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      reSummary.Lines.Add(SDUParamSubstitute(
                                 _('Volume size: %1 + %2 (for CDB) = %3 bytes'),
                                 [
                                  SDUIntToStr(Size),
                                  (CRITICAL_DATA_LENGTH div 8),
                                  SDUIntToStr(totalSize)
                                 ]
                                ));
      end;
      
    reSummary.Lines.Add(_('CDB stored: At start of volume file'));
    end
  else
    begin
    if (PaddingLength > ULLZero) then
      begin
      totalSize := Size + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
                                             _('Volume size: %1 + %2 (padding) = %3 bytes'),
                                             [
                                              SDUIntToStr(Size),
                                              SDUIntToStr(PaddingLength),
                                              SDUIntToStr(totalSize)
                                             ]
                                            ));
      end
    else
      begin
      reSummary.Lines.Add(SDUParamSubstitute(_('Volume size: %1 bytes'), [SDUIntToStr(Size)]));
      end;

    reSummary.Lines.Add(_('CDB stored: In separate keyfile'));
    reSummary.Lines.Add(SDUParamSubstitute(_('CDB keyfile: %1'), [KeyFilename]));
    end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Hash algorithm: %1'), [cbHash.Items[cbHash.ItemIndex]]));
  reSummary.Lines.Add('  '+SDUParamSubstitute(_('[Hash driver: %1]'), [GetHashDriver()]));
  reSummary.Lines.Add('  '+SDUParamSubstitute(_('[Hash GUID: %1]'), [GUIDToString(GetHashGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Key iterations: %1'), [KeyIterations]));


  if (SelectedCypherBlockSize() <= 0) then
    begin
    reSummary.Lines.Add(_('Sector IVs will not be used (cypher has zero or variable blocksize)'));
    end
  else
    begin
    reSummary.Lines.Add(_('Cypher has fixed, defined blocksize; sector IVs will be used'));
    reSummary.Lines.Add(SDUParamSubstitute(_('Sector IV generation method: %1'), [FreeOTFESectorIVGenMethodTitle[SectorIVGenMethod]]));

    if (UsePerVolumeIV) then
      begin
      reSummary.Lines.Add(_('Sector IVs will be XORd with a per-volume IV before use'));
      end
    else
      begin
      reSummary.Lines.Add(_('A per-volume IV will not be used'));
      end;

    end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Cypher algorithm: %1'), [cbCypher.Items[cbCypher.ItemIndex]]));
  reSummary.Lines.Add('  '+SDUParamSubstitute(_('[Cypher driver: %1]'), [GetCypherDriver()]));
  reSummary.Lines.Add('  '+SDUParamSubstitute(_('[Cypher GUID: %1]'), [GUIDToString(GetCypherGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Master key length: %1 bits'), [MasterKeyLength]));

  RNGs := '';
  if ckRNGCryptoAPI.checked then
    begin
    if (RNGs = '') then
      begin
      RNGS := RNGs + ckRNGCryptoAPI.caption;
      end
    else
      begin
      RNGS := RNGs + ', ' + ckRNGCryptoAPI.caption;
      end;
    end;
  if ckRNGMouseMovement.checked then
    begin
    if (RNGs = '') then
      begin
      RNGS := RNGs + ckRNGMouseMovement.caption;
      end
    else
      begin
      RNGS := RNGs + ', ' + ckRNGMouseMovement.caption;
      end;
    end;
  if ckRNGcryptlib.checked then
    begin
    if (RNGs = '') then
      begin
      RNGS := RNGs + ckRNGcryptlib.caption;
      end
    else
      begin
      RNGS := RNGs + ', ' + ckRNGcryptlib.caption;
      end;
    end;
  if ckRNGGPG.checked then
    begin
    if (RNGs = '') then
      begin
      RNGS := RNGs + ckRNGGPG.caption;
      end
    else
      begin
      RNGS := RNGs + ', ' + ckRNGGPG.caption;
      end;
    end;
  reSummary.Lines.Add(SDUParamSubstitute(_('RNG: %1'), [RNGs]));


  reSummary.Lines.Add(_('Password: <entered>'));
  reSummary.Lines.Add(SDUParamSubstitute(_('Salt length: %1 bits'), [SaltLength]));

  driveLetter := RequestedDriveLetter;
  if (driveLetter = #0) then
    begin
    driveLetter := _('Use default');
    end
  else
    begin
    driveLetter := driveLetter + ':';
    end;
  reSummary.Lines.Add(SDUParamSubstitute(_('Requested drive letter: %1'), [driveLetter]));

end;

function TfrmWizardCreateVolume.GetRNGSet(): TRNGSet;
var
  retval: TRNGSet;
begin
  retval := [];

  if ckRNGCryptoAPI.checked then
    begin
    retval := retval + [rngCryptoAPI];
    end;

  if ckRNGMouseMovement.checked then
    begin
    retval := retval + [rngMouseMovement];
    end;

  if ckRNGcryptlib.checked then
    begin
    retval := retval + [rngcryptlib];
    end;

  if ckRNGPKCS11.checked then
    begin
    retval := retval + [rngPKCS11];
    end;

  if ckRNGGPG.checked then
    begin
    retval := retval + [rngGPG];
    end;

  Result := retval;
end;

procedure TfrmWizardCreateVolume.pbFinishClick(Sender: TObject);
var
  allOK: boolean;
begin
  inherited;

  if IsPartition then
  begin
    if (SDUMessageDlg(
                      _('You are about to create a new DoxBox on a disk/partition.'+SDUCRLF+
                      SDUCRLF+
                      'This process will OVERWRITE that disk/partition.'+SDUCRLF+
                      SDUCRLF+
                      'Are you SURE you wish to continue?'),
                      mtWarning,
                      [mbYes, mbNo],
                      0
                     ) = mrNo) then
    begin
      // Bail out...
      exit;
    end;
  end;

  allOK := GenerateRandomData(
                              GetRNGSet(),
                              (RNGRequiredBits() div 8),
                              FreeOTFEObj.PKCS11Library,
                              PKCS11TokenListSelected(cbToken),
                              lblGPGFilename.Caption,
                              fCombinedRandomData
                             );


  // *All* information required to create the new volume acquired - so create
  // the new volume
  if (allOK) then
    begin
    if CreateNewVolume() then
      begin
      PostCreate();
      ModalResult := mrOK;
      end;
    end;

end;


function TfrmWizardCreateVolume.CreateNewVolume(): boolean;
var
  allOK: boolean;
  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;
  saltBytes: ansistring;
  randomPool: ansistring;
  volumeFileSize: ULONGLONG;
  cdbFile: string;
  cdbOffset: ULONGLONG;
  userCancel: boolean;
  junkBool: boolean;
  fileCreateProbMsg: string;
  paddingOffset: ULONGLONG;
begin
  allOK := TRUE;

  // Create the volume file, if not a partition
  if (allOK) then
    begin
    // If the new volume is on a partition; we can skip creating a large file
    // on the localfilesystem
    if not(IsPartition) then
      begin
      // Create the volume file (if it doesn't already exist)
      if (not(FileExists(VolFilename))) then
        begin
        volumeFileSize := Size;
        // Add on the size of the CDB, if stored in volume file
        if (CDBInVolFile) then
          begin
          volumeFileSize := volumeFileSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
          end;
        paddingOffset := volumeFileSize;
        // Add on the size of any padding
        volumeFileSize := volumeFileSize + PaddingLength;

        allOK := SDUCreateLargeFile(VolFilename, volumeFileSize, TRUE, userCancel);
        if allOK then
          begin
          // Overwrite any padding with random data.
          allOK := OverwritePadding(paddingOffset);
          end
        // If there was a problem, and not a user cancel, warn user
        else if userCancel then
          begin
          SDUMessageDlg(_('Box creation canceled'), mtInformation);
          end
        else
          begin
          fileCreateProbMsg := SDUParamSubstitute(_('Unable to create Box; please ensure you have %1 free on the relevant drive'), [SDUFormatAsBytesUnits(volumeFileSize)]);
          if (volumeFileSize >= MAX_FAT_FILESIZE) then
            begin
            fileCreateProbMsg := fileCreateProbMsg+SDUCRLF+
                                 SDUCRLF+
                                 _('Please note that FAT/FAT32 filesystems cannot store files that are 4GB or larger');
            end;
          SDUMessageDlg(fileCreateProbMsg, mtError);
          end;
        end;  // if (not(FileExists(filename))) then
      end;  // if not(IsPartition) then

    end;  // if (allOK) then



  // Create separate CDB file, if needed
  if (allOK) then
    begin
    if not(CDBInVolFile) then
      begin
      allOK := SDUCreateLargeFile(KeyFilename, (CRITICAL_DATA_LENGTH div 8), FALSE, junkBool);
      if not(allOK) then
        begin
        SDUMessageDlg(_('Unable to create separate CDB keyfile.'), mtError);
        end;
      end;
    end;


  if (allOK) then
    begin
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('Populating critical data structure...');
{$ENDIF}
    // Create the volume file header
    // Note: No need to populate CDBMetaData.MACAlgorithm or
    //       CDBMetaData.KDFAlgorithm as the function which writes the CDB out
    //       populates them automatically
    CDBMetaData.MACAlgorithm:= fomacUnknown;
    CDBMetaData.KDFAlgorithm:= fokdfUnknown;

    CDBMetaData.HashDriver   := HashDriver;
    CDBMetaData.HashGUID     := HashGUID;
    CDBMetaData.CypherDriver := CypherDriver;
    CDBMetaData.CypherGUID   := CypherGUID;
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('Using hash driver: '+CDBMetaData.HashDriver);
FreeOTFEObj.DebugMsg('Using hash: '+GUIDToString(CDBMetaData.HashGUID));
FreeOTFEObj.DebugMsg('Using cypher driver: '+CDBMetaData.CypherDriver);
FreeOTFEObj.DebugMsg('Using cypher: '+GUIDToString(CDBMetaData.CypherGUID));
{$ENDIF}


    volumeDetails.CDBFormatID  := CDB_FORMAT_ID;
    volumeDetails.PartitionLen := Size;

    volumeDetails.VolumeFlags := 0;
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('Volume flags: '+inttostr(volumeDetails.VolumeFlags)+' bits');
{$ENDIF}

    volumeDetails.SectorIVGenMethod := SectorIVGenMethod; 

    randomPool:= RandomData_CDB;

    volumeDetails.MasterKeyLength:= MasterKeyLength;
    // Grab 'n' bytes from the random pool to use as the master key
    volumeDetails.MasterKey := Copy(randomPool, 1, (volumeDetails.MasterKeyLength div 8));
    Delete(randomPool, 1, (volumeDetails.MasterKeyLength div 8));
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('Master key length: '+inttostr(volumeDetails.MasterKeyLength)+' bits');
FreeOTFEObj.DebugMsg('Master key follows:');
FreeOTFEObj.DebugMsgBinary(volumeDetails.MasterKey);
{$ENDIF}

    // Grab 'n' bytes from the random pool to use as the volume IV
    // *Only* if the cypher's blocksize is a fixed, +ve number of bits
    volumeDetails.VolumeIVLength:= 0;
    if (SelectedCypherBlockSize() > 0) then
      begin
      if (UsePerVolumeIV) then
        begin
        volumeDetails.VolumeIVLength:= SelectedCypherBlockSize();
        end;
      end;
    volumeDetails.VolumeIV := Copy(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
    Delete(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('Volume IV length: '+inttostr(volumeDetails.VolumeIVLength)+' bits');
FreeOTFEObj.DebugMsg('Volume IV follows:');
FreeOTFEObj.DebugMsgBinary(volumeDetails.VolumeIV);
{$ENDIF}


    volumeDetails.RequestedDriveLetter        := RequestedDriveLetter;

    // Grab 'n' bytes from the random pool to use as the salt
    saltBytes := Copy(randomPool, 1, (SaltLength div 8));
    Delete(randomPool, 1, (SaltLength div 8));

{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('About to write the critical data...');
FreeOTFEObj.DebugMsg('Using password: '+Password);
FreeOTFEObj.DebugMsg('Using salt... ');
FreeOTFEObj.DebugMsg('-- begin salt --');
FreeOTFEObj.DebugMsgBinary(saltBytes);
FreeOTFEObj.DebugMsg('-- end salt --');
{$ENDIF}

    // Determine filename and offset within file storing CDB
    GetCDBFileAndOffset(cdbFile, cdbOffset);

    // Write the header to the file, starting from the specified offset
    if not(FreeOTFEObj.WriteVolumeCriticalData(
                                   cdbFile,
                                   cdbOffset,
                                   Password,
                                   saltBytes,
                                   keyIterations,
                                   volumeDetails,
                                   CDBMetaData,
                                   randomPool
                                  )) then
      begin
      SDUMessageDlg(
                 _('Unable to write critical data block.'),
                 mtError
                );
      allOK := FALSE;
      end;  // if not(OTFEFreeOTFE.WriteVolumeCriticalData(

    end;  // if (allOK) then

  Result := allOK;
end;

procedure TfrmWizardCreateVolume.GetCDBFileAndOffset(out cdbFile: string; out cdbOffset: ULONGLONG);
begin
  cdbFile := VolFilename;
  cdbOffset := Offset;
  if not(CDBInVolFile) then
    begin
    // CDB stored in separate keyfile
    cdbFile := KeyFilename;
    cdbOffset := 0;
{$IFDEF FREEOTFE_DEBUG}
FreeOTFEObj.DebugMsg('CDB stored in separate keyfile: '+cdbFile);
{$ENDIF}
    end;

end;

// Post-creation functionality
procedure TfrmWizardCreateVolume.PostCreate();
var
  MountedDrives: Ansistring;
  errMsg: WideString;
  cntMountOK: integer;
  cntMountFailed: integer;
  mountedOK: boolean;
  tmpVolumeFiles: TStringList;
  cdbFile: string;
  cdbOffset: ULONGLONG;
begin
  if AutoMountAfterCreate then
    begin
    tmpVolumeFiles:= TStringList.Create();
    try
      mountedOK := FALSE;

      tmpVolumeFiles.Add(VolFilename);
      GetCDBFileAndOffset(cdbFile, cdbOffset);

      if (VolFilename = cdbFile) then
        begin
        cdbFile := '';
        end;

      MountedDrives := '';
      if FreeOTFEObj.MountFreeOTFE(
                            tmpVolumeFiles,
                            Password,
                            cdbFile,
                            '',  // Empty string - read the CDB
                            PKCS11_NO_SLOT_ID,
                            nil,  // PKCS#11 session not used
                            nil,  // PKCS#11 secret key not used
                            keyIterations,
                            RequestedDriveLetter,
                            FALSE, // Readonly
                            DEFAULT_MOUNTAS,
                            Offset,
                            CDBInVolFile,
                            SaltLength,
                            TRUE,  // Mount for all users
                            MountedDrives
                          ) then
        begin
        FreeOTFEObj.CountMountedResults(
                                        MountedDrives,
                                        cntMountOK,
                                        cntMountFailed
                                       );

        if (cntMountOK = 1) then
          begin
          NewVolumeMountedAs := MountedDrives[1];
          mountedOK := TRUE;
          end;
        end;
    finally
      tmpVolumeFiles.Free();
    end;

    if mountedOK then
      begin
      if (FreeOTFEObj is TOTFEFreeOTFEDLL) then
        begin
        PostMount_Format();
        end;
      end;

    if not(mountedOK) then
      begin
      // Volumes couldn't be mounted for some reason...
      errMsg := _('Unable to open box.');
      end;
    end;

end;

// Format the volume specified
procedure TfrmWizardCreateVolume.PostMount_Format();
var
  PartitionImage: TOTFEFreeOTFEDLL_PartitionImage;
  Filesystem: TSDFilesystem_FAT;
begin
  if (FreeOTFEObj is TOTFEFreeOTFEDLL) then
    begin
    PartitionImage := TOTFEFreeOTFEDLL_PartitionImage.Create();
    PartitionImage.FreeOTFEObj := TOTFEFreeOTFEDLL(FreeOTFEObj);
    PartitionImage.Filename := VolFilename;
    PartitionImage.MountedAs := NewVolumeMountedAs;
    PartitionImage.Mounted := TRUE;

    if not(PartitionImage.Mounted) then
      begin
      PartitionImage.Free();
      PartitionImage := nil;
      SDUMessageDlg('Box could be opened, but not mounted as a partition image?!', mtError);
      end;

    if (PartitionImage <> nil) then
      begin
      Filesystem := TSDFilesystem_FAT.Create();
      Filesystem.PartitionImage := PartitionImage;
      Filesystem.Format();
      PartitionImage.Mounted := FALSE;
      end;
      
    end;

end;


procedure TfrmWizardCreateVolume.seMasterKeyLengthChange(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.seMasterKeyLengthExit(Sender: TObject);
begin
  // Ensure master key length is a multiple of 8 bits
  if ((seMasterKeyLength.Value mod 8) <> 0) then
    begin
    SDUMessageDlg(_('The master key length must be a multiple of 8'), mtWarning);
    ActiveControl := seMasterKeyLength;
    end;

  CheckCurrentTabComplete();
end;


// Returns the keysize for the user's selected cyppher, as returned by the
// driver
// Returns CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE on error
function TfrmWizardCreateVolume.SelectedCypherKeySize(): integer;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval: integer;
begin
  retval := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Determine the keysize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherKeysize <> CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE) then
    begin
    retval := fCachedCypherKeysize;
    end
  else
    if not(FreeOTFEObj.GetSpecificCypherDetails(
                                                CypherDriver,
                                                CypherGUID,
                                                cypherDetails
                                               )) then
      begin
      SDUMessageDlg(_('Unable to determine the keysize for your selected cypher.'), mtError);
      end
    else
      begin
      // Cache keysize and return...
      fCachedCypherKeysize := cypherDetails.KeySizeRequired;
      retval := fCachedCypherKeysize;
      end;

  Result := retval;

end;

// Returns the blocksize for the user's selected cyppher, as returned by the
// driver
// Returns CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE on error
function TfrmWizardCreateVolume.SelectedCypherBlockSize(): integer;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval: integer;
begin
  retval := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Determine the blocksize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherBlocksize <> CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE) then
    begin
    retval := fCachedCypherBlocksize;
    end
  else
    if not(FreeOTFEObj.GetSpecificCypherDetails(
                                                CypherDriver,
                                                CypherGUID,
                                                cypherDetails
                                               )) then
      begin
      SDUMessageDlg(_('Unable to determine the blocksize for your selected cypher.'), mtError);
      end
    else
      begin
      // Cache blocksize and return...
      fCachedCypherBlocksize := cypherDetails.BlockSize;
      retval := fCachedCypherBlocksize;
      end;

  Result := retval;

end;


// Returns the cypher mode for the user's selected cypher, as returned by
// the driver
function TfrmWizardCreateVolume.SelectedCypherMode(): TFreeOTFECypherMode;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval: TFreeOTFECypherMode;
begin
  retval:= focmUnknown;

  // If we have it cached alredy, use it
  if (fCachedCypherMode <> focmUnknown) then
    begin
    retval := fCachedCypherMode;
    end
  else
    if not(FreeOTFEObj.GetSpecificCypherDetails(
                                                CypherDriver,
                                                CypherGUID,
                                                cypherDetails
                                               )) then
      begin
      SDUMessageDlg(_('Unable to determine the cypher mode for your selected cypher.'), mtError);
      end
    else
      begin
      // Cache blocksize and return...
      fCachedCypherMode := cypherDetails.Mode;
      retval := fCachedCypherMode;
      end;

  Result := retval;

end;


procedure TfrmWizardCreateVolume.pbHashInfoClick(Sender: TObject);
var
  deviceName: string;
  GUID: TGUID;
begin
  deviceName := GetHashDriver();
  GUID := GetHashGUID();
  FreeOTFEObj.ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.pbCypherInfoClick(Sender: TObject);
var
  deviceName: string;
  GUID: TGUID;
begin
  deviceName := GetCypherDriver();
  GUID := GetCypherGUID();
  FreeOTFEObj.ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.rbCDBLocationClick(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.ckPartitionHiddenClick(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;
  if (ckPartitionHidden.checked) then
    begin
    // Force the user to reenter their offset, by marking the offset tabsheet
    // as incomplete
    tsOffset.Tag := 0;
    end
  else
    begin
    // New file; critical data automatically stored at the start of the file;
    // zero offset
    tsOffset.Tag := 1;
    end;

  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.rgFileOrPartitionClick(Sender: TObject);
begin
  // Size tab must be marked as incomplete if the user switched to partition,
  // just in case the user previously entered a size value that's now too big
  if IsPartition then
    begin
    tsSize.Tag := 0;
    end;

  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.se64ByteOffsetChange(Sender: TObject);
begin
  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.seSizeChange(Sender: TObject);
begin
  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.seSizeExit(Sender: TObject);
var
  minSize: int64;
begin
  // Use explicit int64 to prevent Delphi converting Size to int
  minSize := (MIN_PDA_VOLUME_SIZE * BYTES_IN_MEGABYTE);

  // Warn user if volume size smaller than PDA version min
  if (Size < minSize) then
    begin
    SDUMessageDlg(
               SDUParamSubstitute(_('Please note: If you would like your DoxBox to be compatible with the PDA version of FreeOTFE, please specify a box size greater than %1 MB'), [MIN_PDA_VOLUME_SIZE]),
               mtWarning
              );
    end;

  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(
                                                     FreeOTFEObj.PKCS11Library,
                                                     cbToken
                                                    ) > 0);
end;

procedure TfrmWizardCreateVolume.pbRefreshClick(Sender: TObject);
begin
  PopulatePKCS11Tokens();
end;

procedure TfrmWizardCreateVolume.fmeSelectPartitionChanged(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;

  lblPartitionDiskSize.Caption := SDUParamSubstitute(_('(Approx: %1)'), [SDUFormatAsBytesUnits(fmeSelectPartition.SelectedSize())]);

  // Size tab must be marked as incomplete
  tsSize.Tag := 0;

  CheckCurrentTabComplete();

end;


function TfrmWizardCreateVolume.GetCDBInVolFile(): boolean;
begin
  Result := (Trim(KeyFilename) = '');
end;

function TfrmWizardCreateVolume.GetAutoMountAfterCreate(): boolean;
begin
  Result := ckAutoMountAfterCreate.Checked;
end;

procedure TfrmWizardCreateVolume.SetupInstructions();
begin
  inherited;

  reInstructWelcome.Text :=
                   _('This "wizard" will guide you through the process of either creating a new DoxBox, or a hidden box within an existing box.'+SDUCRLF+
                     SDUCRLF+
                     'If you are unsure as to how to answer any of the questions this wizard asks, simply accept the defaults.');

  reInstructFileOrPartition.Text :=
                   _('Please specify whether the new DoxBox should be created as a file stored on your disk, or take up a disk partition or an entire physical disk'+SDUCRLF+
                     SDUCRLF+
                     'For most users, it is recommended that you use a file; encrypted partitions or physical disks may give greater obscurity or speed.');

  reInstructFilename.Text :=
                   _('Please specify the filename and location of the new DoxBox, by clicking the browse button.'+SDUCRLF+
                     SDUCRLF+
                     'If you wish to create a "hidden" DoxBox within an existing DoxBox, please specify your existing DoxBox file to be used.');

  reInstructPartitionWarning.Text :=
                   _('1) Creating encrypted partitions is POTENTIALLY DESTRUCTIVE.'+SDUCRLF+
                     SDUCRLF+
                     '2) Data stored on the partition you select in the next stage may be OVERWRITTEN with encrypted data.'+SDUCRLF+
                     SDUCRLF+
                     '3) It is RECOMMENDED that you backup your data as appropriate before continuing.');

  reInstructPartitionSelect.Text :=
                   _('Please select the partition you wish to create the new DoxBox on, and whether you would like to create a "hidden" DoxBox.');

  reInstructOffset.Text :=
                   _('The DoxBox file you specified already exists. To create a hidden DoxBox within this file, please specify the byte offset within this file from where the new DoxBox should begin.'+SDUCRLF+
                     SDUCRLF+
                     'If you do NOT wish to create a hidden DoxBox within your existing file, please click "< Back" and enter a different filename.');

  reInstructWarningOffset.Text :=
                   _('1) Creating hidden Boxes is POTENTIALLY DESTRUCTIVE.'+SDUCRLF+
                     SDUCRLF+
                     '2) Data within the file/partition specified in the previous stage may be OVERWRITTEN, starting from byte offset specified, and running for the full length of the new hidden DoxBox.'+SDUCRLF+
                     SDUCRLF+
                     '3) Remember the offset entered! For security reasons, this information is not stored anywhere, nor can it be determined automatically; you will need to type in this offset whenever  you wish to mount your hidden DoxBox.');


  reInstructSize.Text :=
                   _('Please enter the required size of the new DoxBox.'+SDUCRLF+
                     SDUCRLF+
                     'This value must be more than 1 MB, and must be a multiple of 512 bytes.'+SDUCRLF+
                     SDUCRLF+
                     'If you are creating a hidden DoxBox, then the size of the original DoxBox file, less the byte offset entered in the previous stage, must be greater than the sum of the value entered here, and the size critical data area.'+SDUCRLF+
                     SDUCRLF+
                     'Note: The size you specified here will be the size of the data part of the DoxBox. The actual file created will be slightly larger than this.');

  reInstructHashCypherIV.Text :=
                   _('Please select which security options should be used in securing your new DoxBox.'+SDUCRLF+
                     SDUCRLF+
                     'You can simply accept the defaults here.');

  reInstructMasterKeyLen.Text :=
                   _('The cypher you selected supports arbitary key lengths.'+SDUCRLF+
                     SDUCRLF+
                     'Please specify the length of the master key which will be used to encrypt/decrypt your data.'+SDUCRLF+
                     SDUCRLF+
                     'Note: The value entered must be a multiple of 8.');

  reInstructRNGSelect.Text :=
                   _('In order to create your new DoxBox, a certain amount of random data is required.'+SDUCRLF+
                     SDUCRLF+
                     'This data will be used for the following:'+SDUCRLF+
                     SDUCRLF+
                     '1) Password salting'+SDUCRLF+
                     '2) The new DoxBox''s master key'+SDUCRLF+
                     '3) Random padding data'+SDUCRLF+
                     SDUCRLF+
                     'In order to generate this data, please select which random number generators you wish to use from the options below.');

  reInstructRNGMouseMovement.Text :=
                   _('You have selected mouse movement to generate random data.'+SDUCRLF+
                     SDUCRLF+
                     'Please "waggle" the mouse within the area below, until enough random data has been generated.');

  reInstructRNGPKCS11.Text :=
                   _('Please select the PKCS#11 token you wish to use to generate random data, from the list shown below');

  reInstructRNGGPG.Text :=
                   _('In order to use GPG to generate random data, please specify the location of "gpg.exe" by clicking the browse button.');

  reInstructPassword.Text :=
                   _('Please enter the keyphrase to be used for securing your DoxBox.'+SDUCRLF+
                     SDUCRLF+
                     'Try to enter one character for each bit of the cypher keysize. For example for a 256 bit cypher enter a 256 character keyphrase.'+SDUCRLF+
                     SDUCRLF+
                     'Note: If you forget your keyphrase, you can forget your data.'+SDUCRLF+
                     SDUCRLF+
                     'Note: Newlines (blank lines) are significant. It is recommended that you do not press <ENTER> after typing in your keyphrase, as this will add an extra newline to the end of your keyphrase.');

  reInstructSummary.Text :=
                   _('You have now entered all the information required to create a new DoxBox.'+SDUCRLF+
                     SDUCRLF+
                     'Please check the summary shown below and click "Finish" to create the new DoxBox, or use the "Back"/"Next" buttons to modify the details you have entered.'+SDUCRLF+
                     SDUCRLF+
                     'If you would like to configure more advanced options, please select "Advanced..."');

end;

function TfrmWizardCreateVolume.OverwritePadding(paddingOffset: ULONGLONG): boolean;
var
  shredder: TShredder;
  allOK: boolean;
  overwriteOK: TShredResult;
  failMsg: string;
  tmpZero: ULONGLONG;
begin
  allOK := TRUE;

  // Short-circuit - no padding data
  tmpZero := 0;
  if (PaddingLength <= tmpZero) then
    begin
    Result := TRUE;
    exit;
    end;

  // Short-circuit - padding currently only supported for file based volumes 
  if IsPartition then
    begin
    Result := TRUE;
    exit;
    end;

  if PadWithEncryptedData then
    begin
    // Initilize zeroed IV for encryption
    TempCypherEncBlockNo := 0;

    // Get *real* random data for encryption key
    TempCypherKey := RandomData_PaddingKey;
    end;


  if (allOK) then
    begin
    shredder:= TShredder.Create(nil);
    try
      shredder.FileDirUseInt := TRUE;
      shredder.IntMethod := smPseudorandom;
      shredder.IntPasses := 1;
      shredder.IntSegmentOffset := paddingOffset;
      shredder.IntSegmentLength := PaddingLength;

      if PadWithEncryptedData then
        begin
        // Note: Setting this event overrides shredder.IntMethod
        shredder.OnOverwriteDataReq := GenerateOverwriteData;
        end
      else
        begin
        shredder.OnOverwriteDataReq := nil;
        end;

      overwriteOK := shredder.DestroyFileOrDir(
                                               VolFilename,
                                               TRUE,   // quickShred
                                               FALSE,  // silent
                                               TRUE    // leaveFile
                                              );

      if (overwriteOK = srSuccess) then
        begin
        // Do nothing...
        end
      else if (overwriteOK = srError) then
        begin
        failMsg := _('Overwrite of padding data FAILED.');
        SDUMessageDlg(failMsg, mtError);
        allOK := FALSE;
        end
      else if (overwriteOK = srUserCancel) then
        begin
        SDUMessageDlg(_('Overwrite of padding data cancelled by user.'), mtInformation);
        allOK := FALSE;
        end;


    finally
      shredder.Free();
    end;

  end;  // if (allOK) then

  TempCypherKey := '';
  TempCypherEncBlockNo := 0;

  Result:= allOK;
end;

// The array passed in is zero-indexed; populate elements zero to "bytesRequired"
procedure TfrmWizardCreateVolume.GenerateOverwriteData(
                                    Sender: TObject;
                                    passNumber: integer;
                                    bytesRequired: cardinal;
                                    var generatedOK: boolean;
                                    var outputBlock: TShredBlock
                                   );
var
  i: integer;
  tempArraySize: cardinal;
  blocksizeBytes: cardinal;
  plaintext: Ansistring;
  cyphertext: Ansistring;
  IV: Ansistring;
  localIV: int64;
  sectorID: LARGE_INTEGER;
begin
  // Generate an array of random data containing "bytesRequired" bytes of data,
  // plus additional random data to pad out to the nearest multiple of the
  // cypher's blocksize bits
  // Cater for if the blocksize was -ve or zero
  if (TempCypherDetails.BlockSize < 1) then
    begin
    blocksizeBytes := 1;
    end
  else
    begin
    blocksizeBytes := (TempCypherDetails.BlockSize div 8);
    end;
  tempArraySize := bytesRequired + (blocksizeBytes - (bytesRequired mod blocksizeBytes));

  plaintext := '';
  for i:=1 to tempArraySize do
    begin
    plaintext := plaintext + Ansichar(random(256));
    { TODO 2 -otdk -csecurity : This is not secure PRNG - check }
    end;


  inc(TempCypherEncBlockNo);

  // Adjust the IV so that this block of encrypted pseudorandom data should be
  // reasonably unique
  IV := '';
  if (TempCypherDetails.BlockSize > 0) then
    begin
    IV := StringOfChar(AnsiChar(#0), (TempCypherDetails.BlockSize div 8));

    localIV := TempCypherEncBlockNo;

    for i:=1 to min(sizeof(localIV), length(IV)) do
      begin
      IV[i] := Ansichar((localIV AND $FF));
      localIV := localIV shr 8;
      end;

    end;

  // Adjust the sectorID so that this block of encrypted pseudorandom data
  // should be reasonably unique
  sectorID.QuadPart := TempCypherEncBlockNo;

  // Encrypt the pseudorandom data generated
  if not(FreeOTFEObj.EncryptSectorData(
                                  CypherDriver,
                                  CypherGUID,
                                  sectorID,
                                  FREEOTFE_v1_DUMMY_SECTOR_SIZE,
                                  TempCypherKey,
                                  IV,
                                  plaintext,
                                  cyphertext
                                 )) then
    begin
    SDUMessageDlg(
               _('Unable to encrypt pseudorandom data before using for overwrite buffer?!')+SDUCRLF+
               SDUCRLF+
               SDUParamSubstitute(_('Error #: %1'), [FreeOTFEObj.LastErrorCode]),
               mtError
              );

    generatedOK := FALSE;
    end
  else
    begin
    // Copy the encrypted data into the outputBlock
    for i:=0 to (bytesRequired-1) do
      begin
      outputBlock[i] := byte(cyphertext[i+1]);
      end;

    generatedOK := TRUE;
    end;

end;

function TfrmWizardCreateVolume.RNGRequiredBits(): integer;
begin
  Result := (CRITICAL_DATA_LENGTH + TempCypherUseKeyLength);
end;

END.


