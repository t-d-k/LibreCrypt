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
  // delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, MouseRNG,
  // sdu common
  Shredder,
  SDUGeneral,
  //freeotfe specific  
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_VolumeFileAPI,  // Required for TVOLUME_CRITICAL_DATA
  OTFEFreeOTFE_DriverAPI,      // Required for CRITICAL_DATA_LEN
  PasswordRichEdit, Spin64,
  OTFEFreeOTFE_WizardCommon, SDUStdCtrls, OTFEFreeOTFE_fmeSelectPartition,
  OTFEFreeOTFE_PasswordRichEdit, SDUForms, SDUFrames, SDUSpin64Units,
  OTFEFreeOTFE_frmWizard, OTFEFreeOTFE_InstructionRichEdit, SDUDialogs;

type
  TfrmWizardCreateVolume = class (TfrmWizard)
    tsWelcome:      TTabSheet;
    tsFilename:     TTabSheet;
    tsHashCypherIV: TTabSheet;
    Label4:         TLabel;
    Label5:         TLabel;
    cbHash:         TComboBox;
    cbCypher:       TComboBox;
    tsRNGSelect:    TTabSheet;
    tsSize:         TTabSheet;
    Label6:         TLabel;
    tsOffset:       TTabSheet;
    pnlWarningOffset: TPanel;
    lblWarningOffset: TLabel;
    tsPassword:     TTabSheet;
    reInstructFilename: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructOffset: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructSize: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructHashCypherIV: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructRNGSelect: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPassword: TOTFEFreeOTFE_InstructionRichEdit;
    Label13:        TLabel;
    lblWelcomeBanner: TLabel;
    lblWelcomeClickNext: TLabel;
    GroupBox1:      TGroupBox;
    pbBrowseFilename: TButton;
    lblFilename:    TSDUFilenameLabel;
    Label17:        TLabel;
    Label7:         TLabel;
    SaveDialog:     TSDUSaveDialog;
    tsRNGMouseMovement: TTabSheet;
    reInstructRNGMouseMovement: TOTFEFreeOTFE_InstructionRichEdit;
    tsSummary:      TTabSheet;
    reSummary:      TRichEdit;
    tsRNGGPG:       TTabSheet;
    reInstructRNGGPG: TOTFEFreeOTFE_InstructionRichEdit;
    GroupBox2:      TGroupBox;
    lblGPGFilename: TSDUFilenameLabel;
    pbBrowseGPG:    TButton;
    GPGOpenDialog:  TSDUOpenDialog;
    MouseRNG:       TMouseRNG;
    tsMasterKeyLength: TTabSheet;
    reInstructMasterKeyLen: TOTFEFreeOTFE_InstructionRichEdit;
    Label2:         TLabel;
    seMasterKeyLength: TSpinEdit64;
    Label9:         TLabel;
    lblMouseRNGBits: TLabel;
    pbHashInfo:     TButton;
    pbCypherInfo:   TButton;
    preUserKey1:    TOTFEFreeOTFE_PasswordRichEdit;
    preUserKey2:    TOTFEFreeOTFE_PasswordRichEdit;
    Label3:         TLabel;
    tsFileOrPartition: TTabSheet;
    tsPartitionSelect: TTabSheet;
    reInstructFileOrPartition: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPartitionSelect: TOTFEFreeOTFE_InstructionRichEdit;
    rgFileOrPartition: TRadioGroup;
    Label21:        TLabel;
    ckPartitionHidden: TCheckBox;
    ckUsePerVolumeIV: TCheckBox;
    cbSectorIVGenMethod: TComboBox;
    lblSectorIVGenMethod: TLabel;
    tsPartitionWarning: TTabSheet;
    gbRNG:          TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI: TCheckBox;
    ckRNGcryptlib:  TCheckBox;
    ckRNGGPG:       TCheckBox;
    ckRNGPKCS11:    TCheckBox;
    tsRNGPKCS11:    TTabSheet;
    cbToken:        TComboBox;
    lblToken:       TLabel;
    pbRefresh:      TButton;
    reInstructRNGPKCS11: TOTFEFreeOTFE_InstructionRichEdit;
    ckSizeEntirePartitionDisk: TCheckBox;
    lblPartitionDiskSize: TLabel;
    fmeSelectPartition: TfmeSelectPartition;
    pbAdvanced:     TButton;
    reInstructSummary: TOTFEFreeOTFE_InstructionRichEdit;
    pnlWarningBorder_L: TPanel;
    pnlWarningPartition: TPanel;
    lblWarningPartition: TLabel;
    Label28:        TLabel;
    pnlWarningBorder_R: TPanel;
    pnlWarningBorder_T: TPanel;
    pnlWarningBorder_B: TPanel;
    se64UnitByteOffset: TSDUSpin64Unit_Storage;
    se64UnitSize:   TSDUSpin64Unit_Storage;
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
    fkeyIterations:        Integer;
    fsaltLength:           Integer;  // Length in *bits*
    frequestedDriveLetter: ansichar;
    fCDBFilename:          String;
    fpaddingLength:        ULONGLONG;
    foverwriteWithChaff:   Boolean;

    FNewVolumeMountedAs: Ansichar;

    // These are ordered lists corresponding to the items shown in the combobox
    fHashKernelModeDriverNames: TStringList;
    fHashGUIDs:      TStringList;
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDs:    TStringList;
    fIVGenMethodIDs: TStringList;

    fDeviceList:  TStringList;
    fDeviceTitle: TStringList;

    // Used for overwriting
    ftempCypherDetails:      TFreeOTFECypher_v3;
    ftempCypherUseKeyLength: Integer;  // In *bits*
    ftempCypherKey:          Ansistring;
    ftempCypherEncBlockNo:   Int64;

    fCombinedRandomData: Ansistring;

    fcanUseCryptlib:        Boolean;
    fPKCS11TokensAvailable: Boolean;

    // When the user selects a cypher, we cache it's keysize for later use.
    // This is set to CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE to indicate no keysize is cached
    fcachedCypherKeysize:   Integer;
    // When the user selects a cypher, we cache it's blocksize for later use.
    // This is set to CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE to indicate no blocksize is cached
    fcachedCypherBlocksize: Integer;
    // When the user selects a cypher, we cache it's mode for later use.
    fcachedCypherMode:      TFreeOTFECypherMode;


    // Returns the keysize for the user's selected cypher, as returned by the
    // driver
    function selectedCypherKeySize(): Integer;
    // Returns the blocksize for the user's selected cypher, as returned by the
    // driver
    function selectedCypherBlockSize(): Integer;
    // Returns the cypher mode for the user's selected cypher, as returned by
    // the driver
    function selectedCypherMode(): TFreeOTFECypherMode;

    function GetRNGSet(): TRNGSet;

    function getIsPartition(): Boolean;
    function getVolFilename(): Ansistring;
    procedure SetVolFilename(filename: Ansistring);
    function getOffset(): ULONGLONG;
    // Note: The size returned *excludes* the size of the critical data
    function getSize(): ULONGLONG;
    function getHashDriver(): Ansistring;
    function getHashGUID(): TGUID;
    function getCypherDriver(): Ansistring;
    function getCypherGUID(): TGUID;
    function GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
    procedure SetSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod);
    function GetUsePerVolumeIV(): Boolean;
    function GetMasterKeyLength(): Integer;  // Returns length in *bits*
    function GetRandomData_CDB(): Ansistring;
    function GetRandomData_PaddingKey(): Ansistring;
    function GetPassword(): Ansistring;

    function GetIsHidden(): Boolean;
    function GetCDBInVolFile(): Boolean;

    function GetAutoMountAfterCreate(): Boolean;

    procedure PopulateHashes();
    procedure PopulateCyphers();
    procedure PopulateSectorIVGenMethods();
    procedure PopulatePKCS11Tokens();

    procedure PopulateSummary();

    procedure GetCDBFileAndOffset(out cdbFile: String; out cdbOffset: ULONGLONG);

    function OverwriteVolWithChaff(): Boolean;
    procedure GenerateOverwriteData(Sender: TObject;
      passNumber: Integer;
      bytesRequired: Cardinal;
      var generatedOK: Boolean;
      var outputBlock: TShredBlock
      );

    function CreateNewVolume(): Boolean;
    procedure PostCreate();
    procedure PostMount_Format_using_dll();
    procedure Update_tempCypherUseKeyLength;

  protected
    procedure SetupInstructions(); override;

    procedure EnableDisableControls(); override;
    procedure CheckCurrentTabComplete(); override;
    function CheckTabComplete(checkTab: TTabSheet): Boolean;

    function SkipTab(tabSheet: TTabSheet): Boolean; override;

    function RNG_requiredBits(): Integer;

    procedure FormWizardStepChanged(Sender: TObject);

  public
    property isPartition: Boolean Read GetIsPartition;
    property volFilename: Ansistring Read GetVolFilename Write SetVolFilename;
    property offset: ULONGLONG Read GetOffset;
    // Note: The size returned *excludes* the size of the critical data
    property size: ULONGLONG Read GetSize;
    // HashDriver - Kernel drivers: HashKernelDeviceName
    //              DLL drivers:    HashLibFilename
    property hashDriver: Ansistring Read GetHashDriver;
    property hashGUID: TGUID Read GetHashGUID;
    // CypherDriver - Kernel drivers: CypherKernelDeviceName
    //                DLL drivers:    CypherLibFilename
    property cypherDriver: Ansistring Read GetCypherDriver;
    property cypherGUID: TGUID Read GetCypherGUID;
    property sectorIVGenMethod: TFreeOTFESectorIVGenMethod Read GetSectorIVGenMethod;
    property usePerVolumeIV: Boolean Read GetUsePerVolumeIV;
    property masterKeyLength: Integer Read GetMasterKeyLength;  // In *bits*
    property randomData_CDB: Ansistring Read GetRandomData_CDB;
    property randomData_PaddingKey: Ansistring Read GetRandomData_PaddingKey;
    property password: Ansistring Read GetPassword;

    property keyIterations: Integer Read FKeyIterations Write FKeyIterations;
    property saltLength: Integer Read FSaltLength Write FSaltLength;  // In *bits*
    property requestedDriveLetter: ansichar Read FRequestedDriveLetter Write FRequestedDriveLetter;
    property keyFilename: String Read FCDBFilename Write FCDBFilename;
    property CDBInVolFile: Boolean Read GetCDBInVolFile;
    property paddingLength: ULONGLONG Read FPaddingLength Write FPaddingLength;
    property overwriteWithChaff: Boolean Read FOverwriteWithChaff Write FOverwriteWithChaff;

    property autoMountAfterCreate: Boolean Read GetAutoMountAfterCreate;
    property newVolumeMountedAs: Ansichar Read fnewVolumeMountedAs Write fnewVolumeMountedAs;

    property isHidden: Boolean Read GetIsHidden;

    procedure fmeSelectPartitionChanged(Sender: TObject);

  end;

procedure Format_drive(drivesToFormat: DriveLetterString; frm: TForm);

implementation

{$R *.DFM}

uses
  Math,
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for StringToGUID
  SDUi18n,
  OTFEConsts_U, // Required for OTFE_ERR_USER_CANCEL
  OTFEFreeOTFEDLL_U,
  OTFEFreeOTFE_PKCS11,
  OTFEFreeOTFE_frmWizardCreateVolumeAdvanced,
  OTFEFreeOTFEDLL_PartitionImage,
  SDPartitionImage,
  SDPartitionImage_File,
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

  // for shformat
  // Format disk related functions ripped from the Unofficial Delphi FAQ (UDF)
  SHFMT_ID_DEFAULT      = $FFFF;
  // Formating options
  SHFMT_OPT_QUICKFORMAT = $0000;
  SHFMT_OPT_FULL        = $0001;
  SHFMT_OPT_SYSONLY     = $0002;
  // Error codes
  SHFMT_ERROR           = $FFFFFFFF;
  SHFMT_CANCEL          = $FFFFFFFE;
  SHFMT_NOFORMAT        = $FFFFFFFD;


resourcestring
  FILEORPART_OPT_VOLUME_FILE = 'Volume file';
  FILEORPART_OPT_PARTITION   = 'Partition/entire disk';

// External function in shell32.dll
function SHFormatDrive(Handle: HWND; Drive, ID, Options: Word): Longint;
  stdcall; external 'shell32.dll' Name 'SHFormatDrive';

procedure TfrmWizardCreateVolume.FormShow(Sender: TObject);
var
  i:   Integer;
  idx: Integer;
begin
  inherited;

  // Center label (done dynamically due to language translation size
  // differences)
  SDUCenterControl(lblWelcomeBanner, ccHorizontal);
  SDUCenterControl(lblWelcomeClickNext, ccHorizontal);
  SDUCenterControl(lblWarningPartition, ccHorizontal);
  SDUCenterControl(lblWarningOffset, ccHorizontal);

  // SDUInitAndZeroBuffer(0, fCombinedRandomData );
  fCombinedRandomData := '';

  KeyIterations           := DEFAULT_KEY_ITERATIONS;
  SaltLength              := DEFAULT_SALT_LENGTH;
  RequestedDriveLetter    := #0;
  KeyFilename             := '';
  PaddingLength           := 0;
  FOverwriteWithChaff     := True;  // preserve plausible deniability by defaulting to true
  ftempCypherUseKeyLength := 0;

  NewVolumeMountedAs := #0;

  pnlWarningOffset.Caption    := '';
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
  pnlWarningBorder_R.Left   := pnlWarningPartition.left + pnlWarningPartition.Width;
  pnlWarningBorder_R.Width  := WARNING_BORDER_WIDTH;
  pnlWarningBorder_R.Top    := pnlWarningPartition.Top - WARNING_BORDER_WIDTH;
  pnlWarningBorder_R.Height := pnlWarningPartition.Height + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_T.Left   := pnlWarningPartition.left - WARNING_BORDER_WIDTH;
  pnlWarningBorder_T.Width  := pnlWarningPartition.Width + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_T.Top    := pnlWarningPartition.Top - WARNING_BORDER_WIDTH;
  pnlWarningBorder_T.Height := WARNING_BORDER_WIDTH;
  pnlWarningBorder_B.Left   := pnlWarningPartition.left - WARNING_BORDER_WIDTH;
  pnlWarningBorder_B.Width  := pnlWarningPartition.Width + (2 * WARNING_BORDER_WIDTH);
  pnlWarningBorder_B.Top    := pnlWarningPartition.Top + pnlWarningPartition.Height;
  pnlWarningBorder_B.Height := WARNING_BORDER_WIDTH;


  // tsFileOrPartition
  rgFileOrPartition.Items.Clear();
  rgFileOrPartition.Items.Add(FILEORPART_OPT_VOLUME_FILE);
  rgFileOrPartition.Items.Add(FILEORPART_OPT_PARTITION);
  rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  fmeSelectPartition.FreeOTFEObj := fFreeOTFEObj;
  fmeSelectPartition.AllowCDROM  := False;
  fmeSelectPartition.OnChange    := fmeSelectPartitionChanged;
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
  if (cbHash.Items.Count > 0) then begin
    idx := cbHash.Items.IndexOf(DEFAULT_HASH_TITLE);
    if (idx = -1) then begin
      // If we can't find the default, force the user to select manually
      idx := -1;
    end;
    cbHash.ItemIndex := idx;
  end;
  // Autoselect default, if available
  if (cbCypher.Items.Count > 0) then begin
    idx := cbCypher.Items.IndexOf(DEFAULT_CYPHER_TITLE);
    if (idx = -1) then begin
      // If we can't find the default, force the user to select manually
      idx := -1;
    end;
    cbCypher.ItemIndex := idx;
  end;

  // Default sector IV generation method
  SetSectorIVGenMethod(DEFAULT_SECTOR_IV_GEN_METHOD);

  // Default to using per-volume IVs
  ckUsePerVolumeIV.Checked := True;

  // tsMasterKeyLen
  seMasterKeyLength.Increment := 8;
  seMasterKeyLength.Value     := DEFAULT_CYPHER_KEY_LENGTH;  // In bits

  // tsRNG
  //MSCryptoAPI enabled as default (in dfm)
  ckRNGcryptlib.Enabled := fcanUseCryptlib;
  ckRNGPKCS11.Enabled   := PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  PopulatePKCS11Tokens();

  // tsRNGGPG
  lblGPGFilename.Caption := '';

  // tsPassword
  preUserKey1.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey1.WantReturns := True;
  preUserKey1.WordWrap    := True;
  preUserKey1.Lines.Clear();
  preUserKey1.PasswordChar := fFreeOTFEObj.PasswordChar;
  preUserKey1.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey1.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;

  preUserKey2.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey2.WantReturns := True;
  preUserKey2.WordWrap    := True;
  preUserKey2.Lines.Clear();
  preUserKey2.PasswordChar := fFreeOTFEObj.PasswordChar;
  preUserKey2.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey2.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;


  // Set all tabs to show that they have not yet been completed
  for i := 0 to (pcWizard.PageCount - 1) do begin
    pcWizard.Pages[i].TabVisible := False;
    // Each tabsheet's tag indicates if that tab has been completed or not; 1 for
    // completed, 0 for not yet complete
    pcWizard.Pages[i].Tag        := 0;
  end;


  // BECAUSE WE DEFAULTED TO USING A VOLUME *FILE*, WE MUST SET THE PARTITION
  // SELECT PAGE TO "DONE" HERE (otherwise, it'll never be flagged as complete
  // if the user doesn't switch)
  tsPartitionWarning.Tag := 1;
  tsPartitionSelect.Tag  := 1;

  // BECAUSE WE DEFAULTED TO USING THE MOUSERNG, WE MUST SET THE GPG PAGE TO
  // "DONE" HERE (otherwise, it'll never be flagged as complete if the user
  // doens't switch RNGs)
  tsRNGGPG.Tag := 1;


  // Summary tab marked as completed
  tsSummary.Tag                  := 1;
  ckAutoMountAfterCreate.Checked := True;
  // Note: ckAutoMountAfterCreate.Visible set in EnableDisableControls(...);
  //       if it's set to FALSE here, the control still appears for some
  //       reason?!


  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;

  CheckCurrentTabComplete();

end;


// Returns TRUE if the specified tab should be skipped, otherwise FALSE
function TfrmWizardCreateVolume.SkipTab(tabSheet: TTabSheet): Boolean;
var
  sheetSkipped: Boolean;
begin
  sheetSkipped := inherited SkipTab(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then begin
    sheetSkipped := (fFreeOTFEObj is TOTFEFreeOTFEDLL);
  end;

  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsFilename) then begin
    sheetSkipped := IsPartition;
  end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionWarning) then begin
    sheetSkipped := not (IsPartition);
  end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then begin
    sheetSkipped := not (IsPartition);
  end;

  // Skip offset if user is't creating a hidden volume
  if (tabSheet = tsOffset) then begin
    sheetSkipped := not (IsHidden);
  end;

  // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
  if (tabSheet = tsMasterKeyLength) then begin
    if ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1)) then begin
      sheetSkipped := (SelectedCypherKeySize() >= 0);
    end;
  end;

  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then begin
    sheetSkipped := not (rngMouseMovement in GetRNGSet());
  end;

  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then begin
    sheetSkipped := not (rngPKCS11 in GetRNGSet());
  end;

  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then begin
    sheetSkipped := not (rngGPG in GetRNGSet());
  end;

  Result := sheetSkipped;
end;

procedure TfrmWizardCreateVolume.Update_tempCypherUseKeyLength;
begin
  // If overwriting with encrypted data (chaff), find the size of the key required
  ftempCypherUseKeyLength := 0;
  if FOverwriteWithChaff then begin
    fFreeOTFEObj.GetSpecificCypherDetails(CypherDriver, CypherGUID, ftempCypherDetails);

    // Get *real* random data for encryption key
    if (ftempCypherDetails.KeySizeRequired < 0) then begin
      // -ve keysize = arbitary keysize supported - just use 512 bits
      ftempCypherUseKeyLength := 512;
    end else begin
      if (ftempCypherDetails.KeySizeRequired > 0) then begin
        // +ve keysize = specific keysize - get sufficient bits
        ftempCypherUseKeyLength := ftempCypherDetails.KeySizeRequired;
      end;
    end;
  end;
end;

procedure TfrmWizardCreateVolume.pbAdvancedClick(Sender: TObject);
var
  dlg: TfrmWizardCreateVolumeAdvanced;
begin
  dlg := TfrmWizardCreateVolumeAdvanced.Create(self);
  try
    dlg.CypherBlocksize := SelectedCypherBlockSize();

    dlg.KeyIterations      := KeyIterations;
    dlg.SaltLength         := SaltLength;
    dlg.DriveLetter        := RequestedDriveLetter;
    dlg.CDBFilename        := KeyFilename;
    dlg.PaddingLength      := PaddingLength;
    dlg.OverwriteWithChaff := FOverwriteWithChaff;

    if (dlg.ShowModal = mrOk) then begin
      KeyIterations        := dlg.KeyIterations;
      SaltLength           := dlg.SaltLength;
      RequestedDriveLetter := dlg.DriveLetter;
      KeyFilename          := dlg.CDBFilename;
      PaddingLength        := dlg.PaddingLength;
      FOverwriteWithChaff  := dlg.OverwriteWithChaff;

      // If overwriting with encrypted data (chaff), find the size of the key required , and ensure can get that random data
      ftempCypherUseKeyLength := 0;
      if FOverwriteWithChaff then begin
        Update_tempCypherUseKeyLength;

        // If MouseRNG is being used, check that enough random data is availabe...
        if not (CheckTabComplete(tsRNGMouseMovement)) then begin
          SDUMessageDlg(
            _(
            'Additional random data is required in order to generate an encryption key for use when producing the padding data.'),
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
  useSectorIVs: Boolean;
begin
  inherited;

  pbHashInfo.Enabled := ((GetHashDriver <> '') and
    (not (IsEqualGUID(GetHashGUID(), StringToGUID(DUMMY_GUID)))));

  pbCypherInfo.Enabled := ((GetCypherDriver <> '') and
    (not (IsEqualGUID(GetCypherGUID(), StringToGUID(DUMMY_GUID)))));


  // If:
  //  *) The selected cypher's blocksize is zero of variable, or
  //  *) The cypher uses LRW, or
  //  *) The cypher uses LRW
  // sector IVs aren't/cannot be used
  useSectorIVs := True;
  if (cbCypher.ItemIndex >= 0) then begin
    useSectorIVs := (SelectedCypherBlockSize() > 0) and (SelectedCypherMode() <> focmLRW) and
      (SelectedCypherMode() <> focmXTS);
  end;

  if not (useSectorIVs) then begin
    ckUsePerVolumeIV.Checked := False;
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
  ckSizeEntirePartitionDisk.Visible := False;
  ckSizeEntirePartitionDisk.Visible := True;

  ckSizeEntirePartitionDisk.Visible :=
    (IsPartition and not (ckPartitionHidden.Checked) and not
    (fmeSelectPartition.SyntheticDriveLayout));
  lblPartitionDiskSize.Visible      := ckSizeEntirePartitionDisk.Visible;
  // If the control's not visible, don't let it be selected; the user enters
  // the size
  if not (ckSizeEntirePartitionDisk.Visible) then begin
    ckSizeEntirePartitionDisk.Checked := False;
  end;

  SDUEnableControl(se64UnitSize, not (ckSizeEntirePartitionDisk.Checked));

  // Prevent user from disabling the automount when creating volumes using the
  // DLL version
  // Toggle the "automount after create" checkbox visible on/off.
  // For some reason, the Visible setting doesn't "take" if set to FALSE
  // in FormShow(...) (i.e. the control is still visible)
  ckAutoMountAfterCreate.Visible := True;
  ckAutoMountAfterCreate.Visible := False;
  ckAutoMountAfterCreate.Visible := not (fFreeOTFEObj is TOTFEFreeOTFEDLL);

end;


procedure TfrmWizardCreateVolume.pbBrowseFilenameClick(Sender: TObject);
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, VolFilename);
  if SaveDialog.Execute then begin
    VolFilename              := SaveDialog.Filename;
    se64UnitByteOffset.Value := 0;
    if (FileExists(VolFilename)) then begin
      // Force the user to reenter their offset, by marking the offset tabsheet
      // as incomplete
      tsOffset.Tag := 0;
    end else begin
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

function TfrmWizardCreateVolume.CheckTabComplete(checkTab: TTabSheet): Boolean;
var
  allOK:                    Boolean;
  randomBitsGenerated:      Integer;
  testSize:                 Int64;
  volSizeWithCDB:           ULONGLONG;
  volSizeWithCDBAndPadding: ULONGLONG;
begin
  inherited;

  allOK := False;

  if (checkTab = tsWelcome) then begin
    // Always complete...
    allOK := True;
  end else
  if (checkTab = tsFileOrPartition) then begin
    // Ensure one option has been selected
    allOK := (rgFileOrPartition.ItemIndex >= 0);
  end else
  if (checkTab = tsFilename) then begin
    // If we're creating a volume within a volume file, a filename must be specified
    if (IsPartition) then begin
      allOK := True;
    end else begin
      // Flag tabsheet complete if a filename has been specified
      allOK := (VolFilename <> '');
    end;

  end else
  if (checkTab = tsPartitionWarning) then begin
    // Warning only; nothing for the user to actually do on this tab
    allOK := True;
  end else
  if (checkTab = tsPartitionSelect) then begin
    // If we're creating a volume on a partition, one must be selected
    if (not (IsPartition)) then begin
      allOK := True;
    end else begin
      allOK := (fmeSelectPartition.SelectedDevice <> '');
    end;

  end else
  if (checkTab = tsOffset) then begin
    // Check that the number entered is less than the max size...
    if (IsPartition) then begin
      allOK := (Offset >= 0);
    end else begin
      allOK := (Offset < SDUGetFileSize(VolFilename)) and (Offset >= 0);
    end;

  end else
  if (checkTab = tsSize) then begin
    // Some sensible minimum volume size
    // Use explicit int64 to prevent Delphi converting Size to int
    testSize := BYTES_IN_MEGABYTE;
    allOK    := (Size >= testSize);

    if allOK then begin
      // Min sector size
      testSize := ASSUMED_HOST_SECTOR_SIZE;
      allOK    := ((size mod testSize) = 0);
    end;

    if allOK then begin
      volSizeWithCDB := Size;
      if CDBInVolFile then begin
        volSizeWithCDB := volSizeWithCDB + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      end;

      volSizeWithCDBAndPadding := volSizeWithCDB + PaddingLength;

      if IsPartition then begin
        if fmeSelectPartition.SyntheticDriveLayout then begin
          // We don't know the size of the selected drive/partition; assume user
          // input OK
          allOK := True;
        end else begin
          // Note: fmeSelectPartition.SelectedSize() already takes into account
          //       whether or not the "entire disk" option was selected on the
          //       frame
          allOK := ((fmeSelectPartition.SelectedSize() - Offset) >= volSizeWithCDBAndPadding);
        end;
      end else
      if (FileExists(VolFilename)) then begin
        // If creating a hidden container, ensure that the existing file is large
        // enough to store the hidden container
        allOK := ((SDUGetFileSize(VolFilename) - Offset) >= volSizeWithCDBAndPadding);
      end else begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)
        allOK := True;
      end;
    end;

  end else
  if (checkTab = tsHashCypherIV) then begin
    allOK := ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1));
  end else
  if (checkTab = tsMasterKeyLength) then begin
    // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
    if (SelectedCypherKeySize() >= 0) then begin
      allOK := True;
    end else begin
      // Otherwise, the keysize must be a multiple of 8 bits
      allOK := ((seMasterKeyLength.Value mod 8) = 0);
    end;

  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    allOK := (GetRNGSet() <> []);
  end else
  if (checkTab = tsRNGMouseMovement) then begin
    if not (rngMouseMovement in GetRNGSet) then begin
      // Mouse RNG not used - automatically completed
      allOK := True;
    end else begin
      // This is a good place to update the display of the number of random bits
      // generated...
      randomBitsGenerated     := CountMouseRNGData();
      lblMouseRNGBits.Caption := SDUParamSubstitute(
        _('Random bits generated: %1/%2'), [randomBitsGenerated,
        RNG_requiredBits()]);

      allOK            := (randomBitsGenerated >= RNG_requiredBits());
      MouseRNG.Enabled := not (allOK);
      if MouseRNG.Enabled then begin
        MouseRNG.Color := clWindow;
      end else begin
        MouseRNG.Color := clBtnFace;
      end;
    end;

  end else
  if (checkTab = tsRNGPKCS11) then begin
    // Must have at least one RNG selected
    allOK := (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0));
  end else
  if (checkTab = tsRNGGPG) then begin
    // Flag tabsheet complete if a GPG executable has been specified
    allOK := (lblGPGFilename.Caption <> '');
  end else
  if (checkTab = tsPassword) then begin
    // Ensure the password is confirmed, and something has been entered as a
    // password
    allOK := (preUserKey1.Text = preUserKey2.Text) and (preUserKey1.Text <> '');
  end else
  if (checkTab = tsSummary) then begin
    // Always completed
    allOK := True;
  end;


  checkTab.Tag := 0;
  if allOK then begin
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
  if ckRNGMouseMovement.Checked then begin
    tsRNGMouseMovement.Tag := 0;
  end;

  // ...RNG GPG
  tsRNGGPG.Tag := 1;
  if ckRNGGPG.Checked then begin
    if (lblGPGFilename.Caption = '') then begin
      tsRNGGPG.Tag := 0;
    end;
  end;

  // ...RNG PKCS#11 token
  tsRNGPKCS11.Tag := 1;
  if ckRNGPKCS11.Checked then begin
    if not (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0)) then begin
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

function TfrmWizardCreateVolume.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = rgFileOrPartition.Items.IndexOf(
    FILEORPART_OPT_PARTITION));
end;

function TfrmWizardCreateVolume.GetIsHidden(): Boolean;
begin
  // If:
  //   a) The user wants to create the volume on a partition, and has
  //      specified they want to create a hidden partition, or
  //   b) The user wants to create the volume within a volume file, but has
  //      specified the filename of an existing file
  Result := ((IsPartition) and (ckPartitionHidden.Checked)) or
    ((not (IsPartition)) and (FileExists(VolFilename)));
end;

function TfrmWizardCreateVolume.GetVolFilename(): Ansistring;
begin
  Result := '';

  if (IsPartition) then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    // Note: With the label used, the Caption is the *full* filename, even
    //       though the it's display is the shortened version
    //       (e.g. "C:\..\filename.vol")
    Result := lblFilename.Caption;
  end;

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
  tmpSize:     ULONGLONG;
  critSizeULL: ULONGLONG;
begin
  if ckSizeEntirePartitionDisk.Checked then begin
    tmpSize := fmeSelectPartition.SelectedSize();

    // If CDB forms part of the volume, reduce as appropriate
    if CDBInVolFile then begin
      critSizeULL := (CRITICAL_DATA_LENGTH div 8);
      tmpSize     := tmpSize - critSizeULL;
    end;

  end else begin
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

  if (cbHash.ItemIndex >= 0) then begin
    retval := fHashKernelModeDriverNames[cbHash.ItemIndex];
  end;

  Result := retval;
end;

function TfrmWizardCreateVolume.GetHashGUID(): TGUID;
var
  strGUID: String;
begin
  // Just some dummy GUID in case none selected
  strGUID := DUMMY_GUID;

  if (cbHash.ItemIndex >= 0) then begin
    strGUID := fHashGUIDs[cbHash.ItemIndex];
  end;

  Result := StringToGUID(strGUID);
end;

function TfrmWizardCreateVolume.GetCypherDriver(): Ansistring;
var
  retval: Ansistring;
begin
  retval := '';

  if (cbCypher.ItemIndex >= 0) then begin
    retval := fCypherKernelModeDriverNames[cbCypher.ItemIndex];
  end;

  Result := retval;
end;

function TfrmWizardCreateVolume.GetCypherGUID(): TGUID;
var
  strGUID: String;
begin
  // Just some dummy GUID in case none selected
  strGUID := DUMMY_GUID;

  if (cbCypher.ItemIndex >= 0) then begin
    strGUID := fCypherGUIDs[cbCypher.ItemIndex];
  end;

  Result := StringToGUID(strGUID);
end;

function TfrmWizardCreateVolume.GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
var
  retVal: TFreeOTFESectorIVGenMethod;
begin
  retVal := foivgUnknown;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then begin
    retVal := TFreeOTFESectorIVGenMethod(
      cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
  end;

  Result := retVal;
end;

procedure TfrmWizardCreateVolume.SetSectorIVGenMethod(sectorIVGenMethod:
  TFreeOTFESectorIVGenMethod);
var
  i:   Integer;
  idx: Integer;
begin
  idx := -1;

  // Locate the appropriate idx
  for i := 0 to (cbSectorIVGenMethod.Items.Count - 1) do begin
    if (TFreeOTFESectorIVGenMethod(cbSectorIVGenMethod.Items.Objects[i]) = sectorIVGenMethod) then
    begin
      idx := i;
      break;
    end;
  end;

  cbSectorIVGenMethod.ItemIndex := idx;
end;

function TfrmWizardCreateVolume.GetUsePerVolumeIV(): Boolean;
begin
  Result := ckUsePerVolumeIV.Checked;
end;


function TfrmWizardCreateVolume.GetRandomData_CDB(): Ansistring;
var
  len: Integer;
begin
  { TODO 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
  // Use the first CRITICAL_DATA_LENGTH bits as the CDB random data
  len := (CRITICAL_DATA_LENGTH div 8);
  assert(1 + len <= length(fCombinedRandomData), 'not enough rand data available');
  Result := Copy(fCombinedRandomData, 1, (CRITICAL_DATA_LENGTH div 8));
end;

function TfrmWizardCreateVolume.GetRandomData_PaddingKey(): Ansistring;
var
  i, len: Integer;
begin
  // Use the last FPadWithEncryptedDataKeyLen bits as the CDB random data,
  // after the CDB random data
  i   := (CRITICAL_DATA_LENGTH div 8) + 1;
  len := (ftempCypherUseKeyLength div 8);
  { TODO 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
  assert(i + len <= length(fCombinedRandomData), 'not enough rand data available');
  Result := Copy(fCombinedRandomData, i, len);

end;

function TfrmWizardCreateVolume.GetPassword(): Ansistring;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  Result := preUserKey1.Text;
end;

function TfrmWizardCreateVolume.GetMasterKeyLength(): Integer;
var
  retval: Integer;
begin
  retval := SelectedCypherKeySize();
  if (retval < 0) then begin
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
  GPGOpenDialog.Options    := GPGOpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(GPGOpenDialog, lblGPGFilename.Caption);
  if GPGOpenDialog.Execute then begin
    lblGPGFilename.Caption := GPGOpenDialog.Filename;
  end;

  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.MouseRNGByteGenerated(Sender: TObject; random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < RNG_requiredBits()) then begin
    AddToMouseRNGData(random);
  end;

  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.PopulateHashes();
var
  tmpDisplayTitles: TStringList;
  i:                Integer;
  hashDetails:      TFreeOTFEHash;
  hashAcceptable:   Boolean;
begin
  tmpDisplayTitles := TStringList.Create();
  try
    if (fFreeOTFEObj.GetHashList(tmpDisplayTitles, fHashKernelModeDriverNames, fHashGUIDs)) then
    begin
      // Strip out all hashes which have:
      //   length <= 0 or blocksize <= 0
      //  - they cannot be used to create new FreeOTFE volumes as they use
      // PBKDF2 (HMAC) to derive the critical data key; and PBKDF2 with HMAC
      // requires that the hash used has a defined length of greater than zero
      for i := (tmpDisplayTitles.Count - 1) downto 0 do begin
        if not (fFreeOTFEObj.GetSpecificHashDetails(fHashKernelModeDriverNames[i],
          StringToGUID(fHashGUIDs[i]), hashDetails)) then begin
          // Just warn user, and ignore...
          SDUMessageDlg(SDUParamSubstitute(_('Unable to obtain hash details for %1'),
            [tmpDisplayTitles[i]]), mtWarning);
          hashAcceptable := False;
        end else begin
          hashAcceptable := (hashDetails.Length > 0) and (hashDetails.BlockSize > 0);
        end;


        // If the current hash algorithm is no use, don't add it to the list
        // offered to the user...
        if not (hashAcceptable) then begin
          tmpDisplayTitles.Delete(i);
          fHashKernelModeDriverNames.Delete(i);
          fHashGUIDs.Delete(i);
        end;

      end;

      // Populate the dropdown list offered to the user...
      cbHash.Items.Clear();
      cbHash.Items.AddStrings(tmpDisplayTitles);

      if (cbHash.Items.Count = 0) then begin
        SDUMessageDlg(
          _(
          'You do not appear to have any FreeOTFE hash drivers that can be used to create new DoxBox volumes installed and started.') +
          SDUCRLF + SDUCRLF + _(
          'If you have only just installed DoxBox, you may need to restart your computer.'),
          mtError
          );
      end;

    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of hashes.') + SDUCRLF + SDUCRLF + _(
        'Please ensure that you have one or more FreeOTFE hash drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed DoxBox, you may need to restart your computer.'),
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
  tmpDisplayTitles := TStringList.Create();
  try
    if (fFreeOTFEObj.GetCypherList(tmpDisplayTitles, fCypherKernelModeDriverNames, fCypherGUIDs))
    then begin
      cbCypher.Items.Clear();
      cbCypher.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed DoxBox, you may need to restart your computer.'),
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
  for currMethod := low(TFreeOTFESectorIVGenMethod) to high(TFreeOTFESectorIVGenMethod) do begin
    // Skip "Unknown"
    if (currMethod <> foivgUnknown) then begin
      cbSectorIVGenMethod.Items.AddObject(FreeOTFESectorIVGenMethodTitle[currMethod],
        Pointer(Ord(currMethod)));
    end;
  end;

end;

procedure TfrmWizardCreateVolume.FormWizardStepChanged(Sender: TObject);
begin
  inherited;

  if (pcWizard.ActivePage = tsSummary) then begin
    PopulateSummary();
  end else
  if (pcWizard.ActivePage = tsPartitionSelect) then begin
    // This shouldn't be needed, but without it, controls on this frame which
    // are set to "Visible := FALSE" by the frame remain visible.
    // Only call Initialize(...) the first time the tab is moved onto
    if (fmeSelectPartition.Tag = 1) then begin
      fmeSelectPartition.Initialize();
      fmeSelectPartition.Tag := 0;
    end;
  end;

end;

procedure TfrmWizardCreateVolume.FormCreate(Sender: TObject);
begin
  fHashKernelModeDriverNames   := TStringList.Create();
  fHashGUIDs                   := TStringList.Create();
  fCypherKernelModeDriverNames := TStringList.Create();
  fCypherGUIDs                 := TStringList.Create();
  fIVGenMethodIDs              := TStringList.Create();

  fDeviceList  := TStringList.Create();
  fDeviceTitle := TStringList.Create();

  fCachedCypherKeysize   := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  fCachedCypherMode      := focmUnknown;

  // Start cryptlib, if possible, as early as we can to allow it as much time
  // as possible to poll entropy
  fcanUseCryptlib := cryptlibLoad();

  OnWizardStepChanged := FormWizardStepChanged;

end;

procedure TfrmWizardCreateVolume.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
{$IFDEF FREEOTFE_DEBUG}
showmessage('destroying wizard');
{$ENDIF}
  fHashKernelModeDriverNames.Free();
  fHashGUIDs.Free();
  fCypherKernelModeDriverNames.Free();
  fCypherGUIDs.Free();
  fIVGenMethodIDs.Free();

  fDeviceList.Free();
  fDeviceTitle.Free();

  // Shutdown cryptlib, if used
  if fcanUseCryptlib then begin
    cryptlibUnload();
    fcanUseCryptlib := False;
  end;

  PurgeMouseRNGData();

  //overwrite any seed asap
  for i := 1 to length(fCombinedRandomData) do begin
    fCombinedRandomData[i] := ansichar(i);
  end;

end;


procedure TfrmWizardCreateVolume.cbHashCypherIVGenChange(Sender: TObject);
begin
  // Cypher changed; clear any cached keysize
  fCachedCypherKeysize   := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Cypher changed; clear any cached blocksize
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Cypher changed; clear any cached mode
  fCachedCypherMode      := focmUnknown;

  CheckCurrentTabComplete();

end;


procedure TfrmWizardCreateVolume.PopulateSummary();
var
  totalSize:   ULONGLONG;
  driveLetter: String;
  RNGs:        String;
  ULLZero:     ULONGLONG;
begin
  // int64Zero to prevent 32/64 bit integer conversion
  ULLZero := 0;

  reSummary.Lines.Clear();

  if (IsPartition) then begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Partition: %1'), [VolFilename]));
  end else begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Filename: %1'), [VolFilename]));
  end;

  if (IsHidden) then begin
    reSummary.Lines.Add(SDUParamSubstitute(_('Hidden volume starting at offset: %1'), [Offset]));
  end;

  if CDBInVolFile then begin
    if (PaddingLength > ULLZero) then begin
      totalSize := Size + ULONGLONG(CRITICAL_DATA_LENGTH div 8) + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) + %3 (padding) = %4 bytes'),
        [SDUIntToStr(Size), (CRITICAL_DATA_LENGTH div 8),
        SDUIntToStr(PaddingLength), SDUIntToStr(totalSize)]));
    end else begin
      totalSize := Size + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) = %3 bytes'),
        [SDUIntToStr(Size), (CRITICAL_DATA_LENGTH div 8), SDUIntToStr(
        totalSize)]));
    end;

    reSummary.Lines.Add(_('CDB stored: At start of volume file'));
  end else begin
    if (PaddingLength > ULLZero) then begin
      totalSize := Size + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (padding) = %3 bytes'),
        [SDUIntToStr(Size), SDUIntToStr(PaddingLength), SDUIntToStr(
        totalSize)]));
    end else begin
      reSummary.Lines.Add(SDUParamSubstitute(_('Volume size: %1 bytes'), [SDUIntToStr(Size)]));
    end;

    reSummary.Lines.Add(_('CDB stored: In separate keyfile'));
    reSummary.Lines.Add(SDUParamSubstitute(_('CDB keyfile: %1'), [KeyFilename]));
  end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Hash algorithm: %1'),
    [cbHash.Items[cbHash.ItemIndex]]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Hash driver: %1]'), [GetHashDriver()]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Hash GUID: %1]'),
    [GUIDToString(GetHashGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Key iterations: %1'), [KeyIterations]));


  if (SelectedCypherBlockSize() <= 0) then begin
    reSummary.Lines.Add(_('Sector IVs will not be used (cypher has zero or variable blocksize)'));
  end else begin
    reSummary.Lines.Add(_('Cypher has fixed, defined blocksize; sector IVs will be used'));
    reSummary.Lines.Add(SDUParamSubstitute(_('Sector IV generation method: %1'),
      [FreeOTFESectorIVGenMethodTitle[SectorIVGenMethod]]));

    if (UsePerVolumeIV) then begin
      reSummary.Lines.Add(_('Sector IVs will be XORd with a per-volume IV before use'));
    end else begin
      reSummary.Lines.Add(_('A per-volume IV will not be used'));
    end;

  end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Cypher algorithm: %1'),
    [cbCypher.Items[cbCypher.ItemIndex]]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Cypher driver: %1]'), [GetCypherDriver()]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Cypher GUID: %1]'),
    [GUIDToString(GetCypherGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Master key length: %1 bits'), [MasterKeyLength]));

  RNGs := '';
  if ckRNGCryptoAPI.Checked then begin
    if (RNGs = '') then begin
      RNGS := RNGs + ckRNGCryptoAPI.Caption;
    end else begin
      RNGS := RNGs + ', ' + ckRNGCryptoAPI.Caption;
    end;
  end;
  if ckRNGMouseMovement.Checked then begin
    if (RNGs = '') then begin
      RNGS := RNGs + ckRNGMouseMovement.Caption;
    end else begin
      RNGS := RNGs + ', ' + ckRNGMouseMovement.Caption;
    end;
  end;
  if ckRNGcryptlib.Checked then begin
    if (RNGs = '') then begin
      RNGS := RNGs + ckRNGcryptlib.Caption;
    end else begin
      RNGS := RNGs + ', ' + ckRNGcryptlib.Caption;
    end;
  end;
  if ckRNGGPG.Checked then begin
    if (RNGs = '') then begin
      RNGS := RNGs + ckRNGGPG.Caption;
    end else begin
      RNGS := RNGs + ', ' + ckRNGGPG.Caption;
    end;
  end;
  reSummary.Lines.Add(SDUParamSubstitute(_('RNG: %1'), [RNGs]));


  reSummary.Lines.Add(_('Password: <entered>'));
  reSummary.Lines.Add(SDUParamSubstitute(_('Salt length: %1 bits'), [SaltLength]));

  driveLetter := RequestedDriveLetter;
  if (driveLetter = #0) then begin
    driveLetter := _('Use default');
  end else begin
    driveLetter := driveLetter + ':';
  end;
  reSummary.Lines.Add(SDUParamSubstitute(_('Requested drive letter: %1'), [driveLetter]));

end;

function TfrmWizardCreateVolume.GetRNGSet(): TRNGSet;
var
  retval: TRNGSet;
begin
  retval := [];

  if ckRNGCryptoAPI.Checked then begin
    retval := retval + [rngCryptoAPI];
  end;

  if ckRNGMouseMovement.Checked then begin
    retval := retval + [rngMouseMovement];
  end;

  if ckRNGcryptlib.Checked then begin
    retval := retval + [rngcryptlib];
  end;

  if ckRNGPKCS11.Checked then begin
    retval := retval + [rngPKCS11];
  end;

  if ckRNGGPG.Checked then begin
    retval := retval + [rngGPG];
  end;

  Result := retval;
end;

procedure TfrmWizardCreateVolume.pbFinishClick(Sender: TObject);
var
  allOK: Boolean;
begin
  inherited;

  if IsPartition then begin
    if (SDUMessageDlg(_('You are about to create a new DoxBox on a disk/partition.' +
      SDUCRLF + SDUCRLF + 'This process will OVERWRITE that disk/partition.' +
      SDUCRLF + SDUCRLF + 'Are you SURE you wish to continue?'), mtWarning,
      [mbYes, mbNo], 0) = mrNo) then begin
      // Bail out...
      exit;
    end;
  end;
  //ensure chaff data is included
  Update_tempCypherUseKeyLength;
  allOK := GenerateRandomData(GetRNGSet(), (RNG_requiredBits() div 8),
    fFreeOTFEObj.PKCS11Library, PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption, fCombinedRandomData);


  // *All* information required to create the new volume acquired - so create
  // the new volume
  if (allOK) then begin
    if CreateNewVolume() then begin
      PostCreate();
      ModalResult := mrOk;
    end;
  end;

end;


function TfrmWizardCreateVolume.CreateNewVolume(): Boolean;
var
  allOK:             Boolean;
  volumeDetails:     TVolumeDetailsBlock;
  CDBMetaData:       TCDBMetaData;
  saltBytes:         Ansistring;
  randomPool:        Ansistring;
  volumeFileSize:    ULONGLONG;
  cdbFile:           String;
  cdbOffset:         ULONGLONG;
  userCancel:        Boolean;
  junkBool:          Boolean;
  fileCreateProbMsg: String;

begin
  allOK := True;

  // Create the volume file, if not a partition
  if (allOK) then begin
    // If the new volume is on a partition; we can skip creating a large file
    // on the localfilesystem
    if not (IsPartition) then begin
      // Create the volume file (if it doesn't already exist)
      if (not (FileExists(VolFilename))) then begin
        volumeFileSize := Size;
        // Add on the size of the CDB, if stored in volume file
        if (CDBInVolFile) then begin
          volumeFileSize := volumeFileSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
        end;

        // Add on the size of any padding
        volumeFileSize := volumeFileSize + PaddingLength;

        allOK := SDUCreateLargeFile(VolFilename, volumeFileSize, True, userCancel);
        if not allOK then
          // If there was a problem, and not a user cancel, warn user
          if userCancel then begin
            SDUMessageDlg(_('Box creation canceled'), mtInformation);
          end else begin
            fileCreateProbMsg :=
              SDUParamSubstitute(_(
              'Unable to create Box; please ensure you have %1 free on the relevant drive'),
              [SDUFormatAsBytesUnits(volumeFileSize)]);
            if (volumeFileSize >= MAX_FAT_FILESIZE) then begin
              fileCreateProbMsg :=
                fileCreateProbMsg + SDUCRLF + SDUCRLF + _(
                'Please note that FAT/FAT32 filesystems cannot store files that are 4GB or larger');
            end;
            SDUMessageDlg(fileCreateProbMsg, mtError);
          end;
      end;  // if (not(FileExists(filename))) then
    end;  // if not(IsPartition) then

    if allOK then begin
      // Overwrite volume with SPRNG data, if required
      // this also overwrites cdb and padding, cdb is created below
        { if hidden also overwrite  from Offset? - this is arguably overkill bc should already have been overwritten,
          but perhaps if didnt do on outer vol then will stop attacker knowing size of data
          also less info leaked if attacker has b4/after snapshots
          for now not overwriting for performance
         }
      { TODO 1 -otdk -cenhance : instead default FOverwriteWithChaff to false for hidden vols so user can still change}
      if Offset = 0 then
        allOK := OverwriteVolWithChaff();
    end;
  end;  // if (allOK) then

  // Create separate CDB file, if needed
  if (allOK) then begin
    if not (CDBInVolFile) then begin
      allOK := SDUCreateLargeFile(KeyFilename, (CRITICAL_DATA_LENGTH div 8), False, junkBool);
      if not (allOK) then begin
        SDUMessageDlg(_('Unable to create separate CDB keyfile.'), mtError);
      end;
    end;
  end;


  if (allOK) then begin
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Populating critical data structure...');
{$ENDIF}
    // Create the volume file header
    // Note: No need to populate CDBMetaData.MACAlgorithm or
    //       CDBMetaData.KDFAlgorithm as the function which writes the CDB out
    //       populates them automatically
    CDBMetaData.MACAlgorithm := fomacUnknown;
    CDBMetaData.KDFAlgorithm := fokdfUnknown;

    CDBMetaData.HashDriver   := HashDriver;
    CDBMetaData.HashGUID     := HashGUID;
    CDBMetaData.CypherDriver := CypherDriver;
    CDBMetaData.CypherGUID   := CypherGUID;
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Using hash driver: '+CDBMetaData.HashDriver);
fFreeOTFEObj.DebugMsg('Using hash: '+GUIDToString(CDBMetaData.HashGUID));
fFreeOTFEObj.DebugMsg('Using cypher driver: '+CDBMetaData.CypherDriver);
fFreeOTFEObj.DebugMsg('Using cypher: '+GUIDToString(CDBMetaData.CypherGUID));
{$ENDIF}


    volumeDetails.CDBFormatID  := CDB_FORMAT_ID;
    volumeDetails.PartitionLen := Size;

    volumeDetails.VolumeFlags := 0;
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Volume flags: '+inttostr(volumeDetails.VolumeFlags)+' bits');
{$ENDIF}

    volumeDetails.SectorIVGenMethod := SectorIVGenMethod;

    { TODO 2 -otdk -csecurity : this copies data - but orig data not deleted so may be reused }
    randomPool := RandomData_CDB;

    volumeDetails.MasterKeyLength := MasterKeyLength;
    // Grab 'n' bytes from the random pool to use as the master key
    volumeDetails.MasterKey       := Copy(randomPool, 1, (volumeDetails.MasterKeyLength div 8));

    // SDUDeleteFromStart(randomPool, volumeDetails.MasterKeyLength div 8);
    Delete(randomPool, 1, (volumeDetails.MasterKeyLength div 8));
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Master key length: '+inttostr(volumeDetails.MasterKeyLength)+' bits');
fFreeOTFEObj.DebugMsg('Master key follows:');
fFreeOTFEObj.DebugMsgBinary(volumeDetails.MasterKey);
{$ENDIF}

    // Grab 'n' bytes from the random pool to use as the volume IV
    // *Only* if the cypher's blocksize is a fixed, +ve number of bits
    volumeDetails.VolumeIVLength := 0;
    if (SelectedCypherBlockSize() > 0) then begin
      if (UsePerVolumeIV) then begin
        volumeDetails.VolumeIVLength := SelectedCypherBlockSize();
      end;
    end;
    volumeDetails.VolumeIV := Copy(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
    // SDUDeleteFromStart(randomPool,volumeDetails.VolumeIVLength div 8 );

    Delete(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Volume IV length: '+inttostr(volumeDetails.VolumeIVLength)+' bits');
fFreeOTFEObj.DebugMsg('Volume IV follows:');
fFreeOTFEObj.DebugMsgBinary(volumeDetails.VolumeIV);
{$ENDIF}


    volumeDetails.RequestedDriveLetter := RequestedDriveLetter;

    // Grab 'n' bytes from the random pool to use as the salt
    saltBytes := Copy(randomPool, 1, (SaltLength div 8));
    // SDUDeleteFromStart(randomPool, (SaltLength div 8));
    { TODO 1 -otdk -crefactor : have randpool object }
    Delete(randomPool, 1, (SaltLength div 8));

{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('About to write the critical data...');
fFreeOTFEObj.DebugMsg('Using password: '+Password);
fFreeOTFEObj.DebugMsg('Using salt... ');
fFreeOTFEObj.DebugMsg('-- begin salt --');
fFreeOTFEObj.DebugMsgBinary(saltBytes);
fFreeOTFEObj.DebugMsg('-- end salt --');
{$ENDIF}

    // Determine filename and offset within file storing CDB
    GetCDBFileAndOffset(cdbFile, cdbOffset);

    // Write the header to the file, starting from the specified offset
    if not (fFreeOTFEObj.WriteVolumeCriticalData(cdbFile, cdbOffset,
      Password, saltBytes, keyIterations, volumeDetails, CDBMetaData, randomPool)) then
    begin
      SDUMessageDlg(
        _('Unable to write critical data block.'),
        mtError
        );
      allOK := False;
    end;  // if not(OTFEFreeOTFE.WriteVolumeCriticalData(

  end;  // if (allOK) then

  Result := allOK;
end;

procedure TfrmWizardCreateVolume.GetCDBFileAndOffset(out cdbFile: String;
  out cdbOffset: ULONGLONG);
begin
  cdbFile   := VolFilename;
  cdbOffset := Offset;
  if not (CDBInVolFile) then begin
    // CDB stored in separate keyfile
    cdbFile   := KeyFilename;
    cdbOffset := 0;
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('CDB stored in separate keyfile: '+cdbFile);
{$ENDIF}
  end;

end;

// Post-creation functionality
procedure TfrmWizardCreateVolume.PostCreate();
var
  MountedDrives:  Ansistring;
  errMsg:         WideString;
  cntMountOK:     Integer;
  cntMountFailed: Integer;
  mountedOK:      Boolean;
  tmpVolumeFiles: TStringList;
  cdbFile:        String;
  cdbOffset:      ULONGLONG;
begin
  if AutoMountAfterCreate then begin
    tmpVolumeFiles := TStringList.Create();
    try
      mountedOK := False;

      tmpVolumeFiles.Add(VolFilename);
      GetCDBFileAndOffset(cdbFile, cdbOffset);

      if (VolFilename = cdbFile) then begin
        cdbFile := '';
      end;

      MountedDrives := '';
      if fFreeOTFEObj.MountFreeOTFE(tmpVolumeFiles, Password, cdbFile,
        '',  // Empty string - read the CDB
        PKCS11_NO_SLOT_ID, nil,
              // PKCS#11 session not used
        nil,  // PKCS#11 secret key not used
        keyIterations, RequestedDriveLetter, False, // Readonly
        DEFAULT_MOUNTAS, Offset, CDBInVolFile, SaltLength, True,
        // Mount for all users
        MountedDrives) then begin
        fFreeOTFEObj.CountMountedResults(
          MountedDrives,
          cntMountOK,
          cntMountFailed
          );

        if (cntMountOK = 1) then begin
          NewVolumeMountedAs := MountedDrives[1];
          mountedOK          := True;
        end;
      end;
    finally
      tmpVolumeFiles.Free();
    end;

    if mountedOK then begin
      { TODO 2 -otdk -cinvestigate : why only DLL - should always format after creation, even if use windows dlg }
      if (fFreeOTFEObj is TOTFEFreeOTFEDLL) then begin
        PostMount_Format_using_dll();
      end else begin
        Format_Drive(FNewVolumeMountedAs, self);
      end;

    end;

    if not (mountedOK) then begin
      // Volumes couldn't be mounted for some reason...
      errMsg := _('Unable to open box.');
    end;
  end;

end;

// Format the volume specified
procedure TfrmWizardCreateVolume.PostMount_Format_using_dll();
var
  PartitionImage: TOTFEFreeOTFEDLL_PartitionImage;
  Filesystem:     TSDFilesystem_FAT;
begin
  if (fFreeOTFEObj is TOTFEFreeOTFEDLL) then begin
    PartitionImage             := TOTFEFreeOTFEDLL_PartitionImage.Create();
    PartitionImage.FreeOTFEObj := TOTFEFreeOTFEDLL(fFreeOTFEObj);
    PartitionImage.MountedAs   := NewVolumeMountedAs;
    // end else begin
    //   //  TODO test this   - could reformat wrong drive! -for now use format_drive for non dll volumes
    //
    //    PartitionImage := TSDPartitionImage_File.Create();
    ////    PartitionImage.fFreeOTFEObj := fFreeOTFEObj;
    //      (PartitionImage as TSDPartitionImage_File).Filename := VolFilename;
    //
    //    end;

    PartitionImage.Mounted := True;

    if not (PartitionImage.Mounted) then begin
      PartitionImage.Free();
      PartitionImage := nil;
      SDUMessageDlg('Box could be opened, but not mounted as a partition image?!', mtError);
    end;

    if (PartitionImage <> nil) then begin
      Filesystem                := TSDFilesystem_FAT.Create();
      Filesystem.PartitionImage := PartitionImage;
      Filesystem.Format();
      PartitionImage.Mounted := False;
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
  if ((seMasterKeyLength.Value mod 8) <> 0) then begin
    SDUMessageDlg(_('The master key length must be a multiple of 8'), mtWarning);
    ActiveControl := seMasterKeyLength;
  end;

  CheckCurrentTabComplete();
end;


 // Returns the keysize for the user's selected cyppher, as returned by the
 // driver
 // Returns CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE on error
function TfrmWizardCreateVolume.SelectedCypherKeySize(): Integer;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval:        Integer;
begin
  retval := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Determine the keysize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherKeysize <> CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE) then begin
    retval := fCachedCypherKeysize;
  end else
  if not (fFreeOTFEObj.GetSpecificCypherDetails(CypherDriver, CypherGUID,
    cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the keysize for your selected cypher.'), mtError);
  end else begin
    // Cache keysize and return...
    fCachedCypherKeysize := cypherDetails.KeySizeRequired;
    retval               := fCachedCypherKeysize;
  end;

  Result := retval;

end;

 // Returns the blocksize for the user's selected cyppher, as returned by the
 // driver
 // Returns CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE on error
function TfrmWizardCreateVolume.SelectedCypherBlockSize(): Integer;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval:        Integer;
begin
  retval := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Determine the blocksize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherBlocksize <> CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE) then begin
    retval := fCachedCypherBlocksize;
  end else
  if not (fFreeOTFEObj.GetSpecificCypherDetails(CypherDriver, CypherGUID,
    cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the blocksize for your selected cypher.'), mtError);
  end else begin
    // Cache blocksize and return...
    fCachedCypherBlocksize := cypherDetails.BlockSize;
    retval                 := fCachedCypherBlocksize;
  end;

  Result := retval;

end;


 // Returns the cypher mode for the user's selected cypher, as returned by
 // the driver
function TfrmWizardCreateVolume.SelectedCypherMode(): TFreeOTFECypherMode;
var
  cypherDetails: TFreeOTFECypher_v3;
  retval:        TFreeOTFECypherMode;
begin
  retval := focmUnknown;

  // If we have it cached alredy, use it
  if (fCachedCypherMode <> focmUnknown) then begin
    retval := fCachedCypherMode;
  end else
  if not (fFreeOTFEObj.GetSpecificCypherDetails(CypherDriver, CypherGUID,
    cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the cypher mode for your selected cypher.'), mtError);
  end else begin
    // Cache blocksize and return...
    fCachedCypherMode := cypherDetails.Mode;
    retval            := fCachedCypherMode;
  end;

  Result := retval;

end;


procedure TfrmWizardCreateVolume.pbHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetHashDriver();
  GUID       := GetHashGUID();
  fFreeOTFEObj.ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.pbCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetCypherDriver();
  GUID       := GetCypherGUID();
  fFreeOTFEObj.ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.rbCDBLocationClick(Sender: TObject);
begin
  CheckCurrentTabComplete();

end;

procedure TfrmWizardCreateVolume.ckPartitionHiddenClick(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;
  if (ckPartitionHidden.Checked) then begin
    // Force the user to reenter their offset, by marking the offset tabsheet
    // as incomplete
    tsOffset.Tag := 0;
  end else begin
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
  if IsPartition then begin
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
  minSize: Int64;
begin
  // Use explicit int64 to prevent Delphi converting Size to int
  minSize := (MIN_PDA_VOLUME_SIZE * BYTES_IN_MEGABYTE);

  // Warn user if volume size smaller than PDA version min
  if (Size < minSize) then begin
    SDUMessageDlg(
      SDUParamSubstitute(_(
      'Please note: If you would like your DoxBox to be compatible with the PDA version of FreeOTFE, please specify a box size greater than %1 MB'),
      [MIN_PDA_VOLUME_SIZE]),
      mtWarning
      );
  end;

  CheckCurrentTabComplete();
end;

procedure TfrmWizardCreateVolume.PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(
    fFreeOTFEObj.PKCS11Library, cbToken) > 0);
end;

procedure TfrmWizardCreateVolume.pbRefreshClick(Sender: TObject);
begin
  PopulatePKCS11Tokens();
end;

procedure TfrmWizardCreateVolume.fmeSelectPartitionChanged(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;

  lblPartitionDiskSize.Caption :=
    SDUParamSubstitute(_('(Approx: %1)'),
    [SDUFormatAsBytesUnits(fmeSelectPartition.SelectedSize())]);

  // Size tab must be marked as incomplete
  tsSize.Tag := 0;

  CheckCurrentTabComplete();

end;


function TfrmWizardCreateVolume.GetCDBInVolFile(): Boolean;
begin
  Result := (Trim(KeyFilename) = '');
end;

function TfrmWizardCreateVolume.GetAutoMountAfterCreate(): Boolean;
begin
  Result := ckAutoMountAfterCreate.Checked;
end;

procedure TfrmWizardCreateVolume.SetupInstructions();
begin
  inherited;

  reInstructWelcome.Text :=
    _('This "wizard" will guide you through the process of either creating a new DoxBox, or a hidden box within an existing box.' + SDUCRLF + SDUCRLF + 'If you are unsure as to how to answer any of the questions this wizard asks, simply accept the defaults.');

  reInstructFileOrPartition.Text :=
    _('Please specify whether the new DoxBox should be created as a file stored on your disk, or take up a disk partition or an entire physical disk' + SDUCRLF + SDUCRLF + 'For most users, it is recommended that you use a file; encrypted partitions or physical disks may give greater obscurity or speed.');

  reInstructFilename.Text :=
    _('Please specify the filename of the new DoxBox, by clicking the browse button.' +
    SDUCRLF + SDUCRLF +
    'If you wish to create a "hidden" DoxBox, please specify your existing DoxBox file to be used.');

  reInstructPartitionWarning.Text :=
    _('1) Creating encrypted partitions is POTENTIALLY DESTRUCTIVE.' + SDUCRLF +
    SDUCRLF + '2) Data stored on the partition you select in the next stage may be OVERWRITTEN with encrypted data.' + SDUCRLF + SDUCRLF + '3) It is RECOMMENDED that you backup your data as appropriate before continuing.');

  reInstructPartitionSelect.Text :=
    _('Please select the partition you wish to create the new DoxBox on, and whether you would like to create a "hidden" DoxBox.');

  reInstructOffset.Text :=
    _('The DoxBox file you specified already exists. To create a hidden DoxBox within this file, please specify the byte offset within this file from where the new DoxBox should begin.' + SDUCRLF + SDUCRLF + 'If you do NOT wish to create a hidden DoxBox within your existing file, please click "< Back" and enter a different filename.');

  reInstructWarningOffset.Text :=
    _('1) Creating hidden Boxes is POTENTIALLY DESTRUCTIVE.' + SDUCRLF +
    SDUCRLF + '2) Data within the file/partition specified in the previous stage may be OVERWRITTEN, starting from byte offset specified, and running for the full length of the new hidden DoxBox.' + SDUCRLF + SDUCRLF + '3) Remember the offset entered! For security reasons, this information is not stored anywhere, nor can it be determined automatically; you will need to type in this offset whenever  you wish to mount your hidden DoxBox.');


  reInstructSize.Text :=
    _('Please enter the required size of the new DoxBox.' + SDUCRLF + SDUCRLF +
    'This value must be more than 1 MB, and must be a multiple of 512 bytes.' +
    SDUCRLF + SDUCRLF +
    'If you are creating a hidden DoxBox, then the size of the original DoxBox file, less the byte offset entered in the previous stage, must be greater than the sum of the value entered here, and the size critical data area.' + SDUCRLF + SDUCRLF + 'Note: The size you specified here will be the size of the data part of the DoxBox. The actual file created will be slightly larger than this.');

  reInstructHashCypherIV.Text :=
    _('Please select which security options should be used in securing your new DoxBox.' +
    SDUCRLF + SDUCRLF + 'You can simply accept the defaults here.');

  reInstructMasterKeyLen.Text :=
    _('The cypher you selected supports arbitary key lengths.' + SDUCRLF +
    SDUCRLF + 'Please specify the length of the master key which will be used to encrypt/decrypt your data.'
    +
    SDUCRLF + SDUCRLF + 'Note: The value entered must be a multiple of 8.');

  reInstructRNGSelect.Text :=
    _('In order to create your new DoxBox, a certain amount of random data is required.' +
    SDUCRLF + SDUCRLF + 'This data will be used for the following:' + SDUCRLF +
    SDUCRLF + '1) Password salting' + SDUCRLF + '2) The new DoxBox''s master key' +
    SDUCRLF + '3) To seed random ''chaff'' data' + SDUCRLF + SDUCRLF +
    'In order to generate this data, please select which random number generators you wish to use from the options below.');

  reInstructRNGMouseMovement.Text :=
    _('You have selected mouse movement to generate random data.' + SDUCRLF +
    SDUCRLF + 'Please "waggle" the mouse within the area below, until enough random data has been generated.');

  reInstructRNGPKCS11.Text :=
    _('Please select the PKCS#11 token you wish to use to generate random data, from the list shown below');

  reInstructRNGGPG.Text :=
    _('In order to use GPG to generate random data, please specify the location of "gpg.exe" by clicking the browse button.');

  reInstructPassword.Text :=
    _('Please enter the keyphrase to be used for securing your DoxBox.' +
    SDUCRLF + SDUCRLF +
    'Try to enter one character for each bit of the cypher keysize. For example for a 256 bit cypher enter a 256 character keyphrase.'
    + SDUCRLF + SDUCRLF + 'Note: If you forget your keyphrase, you can forget your data.' +
    SDUCRLF + SDUCRLF +
    'Note: Newlines (blank lines) are significant. It is recommended that you do not press <ENTER> after typing in your keyphrase, as this will add an extra newline to the end of your keyphrase.');

  reInstructSummary.Text :=
    _('You have now entered all the information required to create a new DoxBox.' +
    SDUCRLF + SDUCRLF +
    'Please check the summary shown below and click "Finish" to create the new DoxBox, or use the "Back"/"Next" buttons to modify the details you have entered.' + SDUCRLF + SDUCRLF + 'If you would like to configure more advanced options, please select "Advanced..."');

end;

{overwrites volume with 'chaff' from 'Offset' -
uses seleced cypher and options as for enxn
}

function TfrmWizardCreateVolume.OverwriteVolWithChaff(): Boolean;
var
  shredder:    TShredder;
  allOK:       Boolean;
  overwriteOK: TShredResult;
  failMsg:     String;
  //  tmpZero: ULONGLONG;
begin
  allOK := True;

  // Short-circuit - no padding data
  //  tmpZero := 0;

 { if (PaddingLength <= tmpZero) then
    begin
    Result := TRUE;
    exit;
    end;}


{
  if IsPartition then
    begin
    Result := TRUE;
    exit;
    end;
     }
  if FOverwriteWithChaff then begin
    // Initilize zeroed IV for encryption
    ftempCypherEncBlockNo := 0;

    // Get *real* random data for encryption key
    ftempCypherKey := RandomData_PaddingKey;

    shredder := TShredder.Create(nil);
    try
      shredder.FileDirUseInt    := True;
      shredder.IntMethod        := smPseudorandom;
      shredder.IntPasses        := 1;
      shredder.IntSegmentOffset := GetOffset;
      // cdb is written after so can overwrite. if hidden dont overwrite main data
      // will be 0 for non hidden vols

      //      shredder.IntSegmentLength := todo; ignored as quickshred = false

      // if PadWithEncryptedData then
      //        begin
      // Note: Setting this event overrides shredder.IntMethod
      shredder.OnOverwriteDataReq := GenerateOverwriteData;
      //   end
      //      else
      //        begin
      //        shredder.OnOverwriteDataReq := nil;
      //        end;
      { TODO 2 -otdk -ctest : this has not been tested for devices }
      if IsPartition then
        overwriteOK := shredder.DestroyDevice(VolFilename, False, False)
      else
        overwriteOK := shredder.DestroyFileOrDir(VolFilename,
                   { TODO 1 -otdk -ccheck : check use of quickshred here }
          False,   // quickShred - do all
          False,   // silent
          True     // leaveFile
          );

      if (overwriteOK = srSuccess) then begin
        // Do nothing...
      end else
      if (overwriteOK = srError) then begin
        failMsg := _('Overwrite of data FAILED.');
        SDUMessageDlg(failMsg, mtError);
        allOK := False;
      end else
      if (overwriteOK = srUserCancel) then begin
        SDUMessageDlg(_('Overwrite of data cancelled.'), mtInformation);
        allOK := False;
      end;


    finally
      shredder.Free();
    end;

  end;  // if (allOK) then
        // SDUInitAndZeroBuffer(0,ftempCypherKey);

  ftempCypherKey        := '';
  ftempCypherEncBlockNo := 0;

  Result := allOK;
end;

// The array passed in is zero-indexed; populate elements zero to "bytesRequired"
procedure TfrmWizardCreateVolume.GenerateOverwriteData(
  Sender: TObject;
  passNumber: Integer;
  bytesRequired: Cardinal;
  var generatedOK: Boolean;
  var outputBlock: TShredBlock
  );
var
  i:              Integer;
  tempArraySize:  Cardinal;
  blocksizeBytes: Cardinal;
  plaintext:      Ansistring;
  cyphertext:     Ansistring;
  IV:             Ansistring;
  localIV:        Int64;
  sectorID:       LARGE_INTEGER;
begin
  // Generate an array of random data containing "bytesRequired" bytes of data,
  // plus additional random data to pad out to the nearest multiple of the
  // cypher's blocksize bits
  // Cater for if the blocksize was -ve or zero
  if (ftempCypherDetails.BlockSize < 1) then begin
    blocksizeBytes := 1;
  end else begin
    blocksizeBytes := (ftempCypherDetails.BlockSize div 8);
  end;
  tempArraySize := bytesRequired + (blocksizeBytes - (bytesRequired mod blocksizeBytes));

  plaintext := '';
  for i := 1 to tempArraySize do begin
    plaintext := plaintext + Ansichar(random(256));
    { DONE 2 -otdk -csecurity : This is not secure PRNG - but the key is random, so the result is secure }
  end;


  Inc(ftempCypherEncBlockNo);

  // Adjust the IV so that this block of encrypted pseudorandom data should be
  // reasonably unique
  IV := '';
  if (ftempCypherDetails.BlockSize > 0) then begin
    IV := StringOfChar(AnsiChar(#0), (ftempCypherDetails.BlockSize div 8));

    localIV := ftempCypherEncBlockNo;

    for i := 1 to min(sizeof(localIV), length(IV)) do begin
      IV[i]   := Ansichar((localIV and $FF));
      localIV := localIV shr 8;
    end;

  end;

  // Adjust the sectorID so that this block of encrypted pseudorandom data
  // should be reasonably unique
  sectorID.QuadPart := ftempCypherEncBlockNo;

  // Encrypt the pseudorandom data generated
  if not (fFreeOTFEObj.EncryptSectorData(CypherDriver, CypherGUID, sectorID,
    FREEOTFE_v1_DUMMY_SECTOR_SIZE, ftempCypherKey, IV, plaintext, cyphertext)) then begin
    SDUMessageDlg(
      _('Error: unable to encrypt pseudorandom data before using for overwrite buffer') +
      SDUCRLF + SDUCRLF + SDUParamSubstitute(_('Error #: %1'), [fFreeOTFEObj.LastErrorCode]),
      mtError
      );

    generatedOK := False;
  end else begin
    // Copy the encrypted data into the outputBlock
    for i := 0 to (bytesRequired - 1) do begin
      outputBlock[i] := Byte(cyphertext[i + 1]);
    end;

    generatedOK := True;
  end;

end;

function TfrmWizardCreateVolume.RNG_requiredBits(): Integer;
begin
  Result := CRITICAL_DATA_LENGTH + ftempCypherUseKeyLength;
  //also same cypher used for chaff
  if foverwriteWithChaff then
    Result := Result + ftempCypherUseKeyLength;
end;


procedure Format_drive(drivesToFormat: DriveLetterString; frm: TForm);
resourcestring
  FORMAT_CAPTION = 'Format';
var
  i:         Integer;
  currDrive: DriveLetterChar;
  driveNum:  Word;
begin

  for i := 1 to length(drivesToFormat) do begin
    currDrive := drivesToFormat[i];
    driveNum  := Ord(currDrive) - Ord('A');
    SHFormatDrive(frm.Handle, driveNum, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
  end;

  // This is *BIZARRE*.
  // If you are running under Windows Vista, and you mount a volume for
  // yourself only, and then try to format it, the format will fail - even if
  // you UAC escalate to admin
  // That's one thing - HOWEVER! After it fails, for form's title is set to:
  //   Format <current volume label> <drive letter>:
  // FREAKY!
  // OTOH, it does means we can detect and inform the user that the format
  // just failed...
  if (Pos(uppercase(FORMAT_CAPTION), uppercase(frm.Caption)) > 0) then begin
    frm.Caption := Application.Title;
    SDUMessageDlg(
      _('Your encrypted drive could not be formatted at this time.') + SDUCRLF +
      SDUCRLF + _(
      'Please lock this Box and re-open it with the "Mount for all users" option checked, before trying again.'),
      mtError
      );
  end;

end;

end.
