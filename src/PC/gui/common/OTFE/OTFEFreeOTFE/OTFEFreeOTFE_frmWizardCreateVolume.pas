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
  SDUGeneral, SDURandPool, SDUStdCtrls, SDUForms, SDUFrames, SDUSpin64Units,
  PasswordRichEdit, Spin64,
  //freeotfe specific
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_VolumeFileAPI,  // Required for TVOLUME_CRITICAL_DATA
  OTFEFreeOTFE_DriverAPI,      // Required for CRITICAL_DATA_LEN

  OTFEFreeOTFE_fmeSelectPartition,
  OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_frmWizard, OTFEFreeOTFE_InstructionRichEdit, lcDialogs,
  SDUDialogs, fmeNewPassword;

type
  TfrmWizardCreateVolume = class (TfrmWizard)
    tsFilename:      TTabSheet;
    tsHashCypherIV:  TTabSheet;
    Label4:          TLabel;
    Label5:          TLabel;
    cbHash:          TComboBox;
    cbCypher:        TComboBox;
    tsRNGSelect:     TTabSheet;
    tsSize:          TTabSheet;
    Label6:          TLabel;
    tsOffset:        TTabSheet;
    pnlWarningOffset: TPanel;
    lblWarningOffset: TLabel;
    tsPassword:      TTabSheet;
    reInstructFilename1: TLabel;
    reInstructOffset: TLabel;
    reInstructSize:  TLabel;
    reInstructHashCypherIV: TLabel;
    reInstructRNGSelect1: TLabel;
    Label13:         TLabel;
    GroupBox1:       TGroupBox;
    pbBrowseFilename: TButton;
    Label7:          TLabel;
    SaveDialog:      TSDUSaveDialog;
    tsRNGMouseMovement: TTabSheet;
    reInstructRNGMouseMovement1: TLabel;
    tsSummary:       TTabSheet;
    reSummary:       TRichEdit;
    tsRNGGPG:        TTabSheet;
    reInstructRNGGPG: TLabel;
    GroupBox2:       TGroupBox;
    lblGPGFilename:  TSDUFilenameLabel;
    pbBrowseGPG:     TButton;
    GPGOpenDialog:   TSDUOpenDialog;
    MouseRNG:        TMouseRNG;
    tsMasterKeyLength: TTabSheet;
    reInstructMasterKeyLen: TLabel;
    Label2:          TLabel;
    seMasterKeyLength: TSpinEdit64;
    Label9:          TLabel;
    lblMouseRNGBits: TLabel;
    pbHashInfo:      TButton;
    pbCypherInfo:    TButton;
    tsFileOrPartition: TTabSheet;
    tsPartitionSelect: TTabSheet;
    reInstructPartitionSelect: TLabel;
    rgFileOrPartition: TRadioGroup;
    Label21:         TLabel;
    ckPartitionHidden: TCheckBox;
    ckUsePerVolumeIV: TCheckBox;
    cbSectorIVGenMethod: TComboBox;
    lblSectorIVGenMethod: TLabel;
    tsPartitionWarning: TTabSheet;
    gbRNG:           TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI:  TCheckBox;
    ckRNGcryptlib:   TCheckBox;
    ckRNGPKCS11:     TCheckBox;
    tsRNGPKCS11:     TTabSheet;
    cbToken:         TComboBox;
    lblToken:        TLabel;
    pbRefresh:       TButton;
    reInstructRNGPKCS11: TLabel;
    ckSizeEntirePartitionDisk: TCheckBox;
    lblPartitionDiskSize: TLabel;
    fmeSelectPartition: TfmeSelectPartition;
    reInstructSummary: TLabel;
    se64UnitByteOffset: TSDUSpin64Unit_Storage;
    se64UnitSize:    TSDUSpin64Unit_Storage;
    reInstructWarningOffset1: TLabel;
    ckAutoMountAfterCreate: TCheckBox;
    tsKeyIterations: TTabSheet;
    reInstructKeyIterations1: TLabel;
    Label1:          TLabel;
    seKeyIterations: TSpinEdit64;
    tsSalt:          TTabSheet;
    seSaltLength:    TSpinEdit64;
    Label8:          TLabel;
    Label10:         TLabel;
    tsDriveLetter:   TTabSheet;
    lblInstructDriveLetter1: TLabel;
    Label11:         TLabel;
    cbDriveLetter:   TComboBox;
    tsCDBLocation:   TTabSheet;
    reInstructCDBLocation: TLabel;
    rbCDBInVolFile:  TRadioButton;
    rbCDBInKeyfile:  TRadioButton;
    gbKeyfile:       TGroupBox;
    lblKeyFilename:  TSDUFilenameLabel;
    pbBrowseKeyfile: TButton;
    keySaveDialog:   TSDUSaveDialog;
    tsPadding:       TTabSheet;
    reInstructPadding: TLabel;
    Label12:         TLabel;
    Label14:         TLabel;
    tsChaff:         TTabSheet;
    lblInstructChaff1: TLabel;
    ckRNGGPG:        TCheckBox;
    lblFilename:     TEdit;
    Label15:         TLabel;
    reInstructPartitionWarning1: TLabel;
    lblWarningPartition: TLabel;
    rgOverwriteType: TRadioGroup;
    se64Padding:     TSpinEdit64;
    lblFinished:     TLabel;
    lblWelcomeBanner: TLabel;
    lblWelcomeClickNext: TLabel;
    lblInstructSalt1: TLabel;
    lblInstructFileOrPartition1: TLabel;
    lblInstructDriveLetter2: TLabel;
    reInstructRNGSelect2: TLabel;
    reInstructPartitionWarning2: TLabel;
    reInstructPartitionWarning3: TLabel;
    reInstructWarningOffset2: TLabel;
    reInstructWarningOffset4: TLabel;
    reInstructWarningOffset3: TLabel;
    lblInstructChaff4: TLabel;
    lblInstructChaff3: TLabel;
    lblInstructChaff2: TLabel;
    reInstructRNGSelect4: TLabel;
    reInstructRNGSelect3: TLabel;
    reInstructRNGSelect5: TLabel;
    reInstructRNGMouseMovement2: TLabel;
    reInstructKeyIterations3: TLabel;
    reInstructKeyIterations2: TLabel;
    lblInstructSalt4: TLabel;
    lblInstructSalt3: TLabel;
    lblInstructSalt2: TLabel;
    lblInstructFileOrPartition3: TLabel;
    lblInstructFileOrPartition2: TLabel;
    reInstructFilename2: TLabel;
    frmeNewPassword: TfrmeNewPassword;
    procedure pbBrowseFilenameClick(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure fmePasswordChange(Sender: TObject);
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
    procedure se64ByteOffsetChange(Sender: TObject);
    procedure rbCDBLocationClick2(Sender: TObject);
    procedure pbBrowseKeyfileClick(Sender: TObject);
    procedure cbDriveLetterChange(Sender: TObject);

  private
    // Advanced options...
    //    fkeyIterations:        Integer;
    //    fsaltLength:           Integer;  // Length in *bits*
    //    frequestedDriveLetter: ansichar;
    //    fCDBFilename:          String;
    //    fpaddingLength:        ULONGLONG;
    //    foverwriteWithChaff:   Boolean;

    fNewVolumeMountedAs: Char;

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
    ftempCypherKey:          TSDUBytes;
    ftempCypherEncBlockNo:   Int64;


    //    fcanUseCryptlib:        Boolean;
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
    function getVolFilename(): String;
    procedure SetVolFilename(filename: String);
    function getOffset(): ULONGLONG;
    // Note: The size returned *excludes* the size of the critical data
    function getSize(): ULONGLONG;
    // HashDriver - Kernel drivers: HashKernelDeviceName
    //              DLL drivers:    HashLibFilename
    function getHashDriver(): String;
    function getHashGUID(): TGUID;
    // CypherDriver - Kernel drivers: CypherKernelDeviceName
    //                DLL drivers:    CypherLibFilename
    function getCypherDriver(): Ansistring;
    function getCypherGUID(): TGUID;
    function GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
    procedure SetSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod);
    function GetUsePerVolumeIV(): Boolean;
    function GetMasterKeyLength(): Integer;  // Returns length in *bits*
    // function GetRandomData_CDB(): Ansistring;
    function GetRandomData_PaddingKey(): TSDUBytes;

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
    function GetDriveLetter: Char;
    procedure SetDriveLetter(driveLetter: Char);
    function GetCDBFilename: String;
    procedure SetCDBFilename(CDBFilename: String);
    function GetPaddingLength: Int64;
    procedure SetPaddingLength(len: Int64);
    function GetOverwriteWithChaff: Boolean;
    procedure SetOverwriteWithChaff(secChaff: Boolean);

  protected

    procedure EnableDisableControls(); override;

    function IsTabComplete(checkTab: TTabSheet): Boolean; override;

    function IsTabSkipped(tabSheet: TTabSheet): Boolean; override;
    function IsTabRequired(tabSheet: TTabSheet): Boolean; override;

    function RNG_requiredBits(): Integer;

    procedure FormWizardStepChanged(Sender: TObject);

  public
    //    property isPartition: Boolean Read GetIsPartition;
    //    property volFilename: Ansistring Read GetVolFilename Write SetVolFilename;
    //    property offset: ULONGLONG Read GetOffset;
    //    property size: ULONGLONG Read GetSize;

    //    property hashDriver: Ansistring Read GetHashDriver;
    //    property hashGUID: TGUID Read GetHashGUID;

    //    property cypherDriver: Ansistring Read GetCypherDriver;
    //    property cypherGUID: TGUID Read GetCypherGUID;
    //    property sectorIVGenMethod: TFreeOTFESectorIVGenMethod Read GetSectorIVGenMethod;
    //    property usePerVolumeIV: Boolean Read GetUsePerVolumeIV;
    //    property masterKeyLength: Integer Read GetMasterKeyLength;  // In *bits*
    //    property randomData_CDB: Ansistring Read GetRandomData_CDB;
    //    property randomData_PaddingKey: Ansistring Read GetRandomData_PaddingKey;
    //    property password: Ansistring Read GetPassword;

    //    property keyIterations: Integer Read FKeyIterations Write FKeyIterations;
    //    property saltLength: Integer Read FSaltLength Write FSaltLength;  // In *bits*
    //    property requestedDriveLetter: ansichar Read FRequestedDriveLetter Write fRequestedDriveLetter;
    //    property keyFilename: String Read FCDBFilename Write FCDBFilename;
    //    property CDBInVolFile: Boolean Read GetCDBInVolFile;
    //    property paddingLength: ULONGLONG Read FPaddingLength Write FPaddingLength;
    //    property overwriteWithChaff: Boolean Read FOverwriteWithChaff Write FOverwriteWithChaff;

    //    property autoMountAfterCreate: Boolean Read GetAutoMountAfterCreate;
    //    property newVolumeMountedAs: Ansichar Read fnewVolumeMountedAs Write fnewVolumeMountedAs;

    //    property isHidden: Boolean Read GetIsHidden;

    procedure fmeSelectPartitionChanged(Sender: TObject);

  end;

procedure Format_drive(drivesToFormat: DriveLetterString; frm: TForm);

implementation

{$R *.DFM}

uses
  Math,
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for StringToGUID
            //sdu
  SDUi18n, SDPartitionImage,
  SDPartitionImage_File,
  SDFilesystem,
  SDFilesystem_FAT,
                //freeotfe/LibreCrypt
  OTFEConsts_U, // Required for OTFE_ERR_USER_CANCEL
  OTFEFreeOTFEDLL_U,
  OTFEFreeOTFE_PKCS11,
  //  OTFEFreeOTFE_frmWizardCreateVolumeAdvanced,
  OTFEFreeOTFEDLL_PartitionImage;

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
  idx: Integer;
begin
  inherited;
  SDUTranslateComp(reInstructKeyIterations1);
  SDUTranslateComp(reInstructKeyIterations2);
  SDUTranslateComp(reInstructKeyIterations3);
  SDUTranslateComp(lblInstructSalt1);
  SDUTranslateComp(lblInstructSalt2);
  SDUTranslateComp(lblInstructSalt3);
  SDUTranslateComp(lblInstructSalt4);
  SDUTranslateComp(lblInstructDriveLetter1);
  SDUTranslateComp(lblInstructDriveLetter2);
  SDUTranslateComp(reInstructPartitionSelect);
  SDUTranslateComp(reInstructPartitionWarning1);
  SDUTranslateComp(reInstructPartitionWarning2);
  SDUTranslateComp(reInstructPartitionWarning3);
  SDUTranslateComp(lblInstructFileOrPartition1);
  SDUTranslateComp(lblInstructFileOrPartition2);
  SDUTranslateComp(lblInstructFileOrPartition3);

  SDUTranslateComp(reInstructCDBLocation);

  SDUTranslateComp(reInstructPadding);

  SDUTranslateComp(lblInstructChaff1);
  SDUTranslateComp(lblInstructChaff2);
  SDUTranslateComp(lblInstructChaff3);
  SDUTranslateComp(lblInstructChaff4);
  SDUTranslateComp(reInstructOffset);
  SDUTranslateComp(reInstructWarningOffset1);
  SDUTranslateComp(reInstructWarningOffset2);
  SDUTranslateComp(reInstructWarningOffset3);
  SDUTranslateComp(reInstructWarningOffset4);
  SDUTranslateComp(reInstructSize);
  { TODO 1 -otdk -ceasify : why mutiple of 512? simplify text }
  SDUTranslateComp(reInstructHashCypherIV);
  SDUTranslateComp(reInstructMasterKeyLen);
  SDUTranslateComp(reInstructRNGSelect1);
  SDUTranslateComp(reInstructRNGSelect2);
  SDUTranslateComp(reInstructRNGSelect3);
  SDUTranslateComp(reInstructRNGSelect4);
  SDUTranslateComp(reInstructRNGSelect5);
  SDUTranslateComp(reInstructRNGMouseMovement1);
  SDUTranslateComp(reInstructRNGMouseMovement2);
  SDUTranslateComp(reInstructRNGPKCS11);
  SDUTranslateComp(reInstructRNGGPG);

  SDUTranslateComp(reInstructSummary);

  // Center label (done dynamically due to language translation size
  // differences)
  SDUCenterControl(lblWelcomeBanner, ccHorizontal);
  SDUCenterControl(lblWelcomeClickNext, ccHorizontal);
  SDUCenterControl(lblWarningPartition, ccHorizontal);
  SDUCenterControl(lblWarningOffset, ccHorizontal);


  seKeyIterations.Value := DEFAULT_KEY_ITERATIONS;
  seSaltLength.Value    := DEFAULT_SALT_LENGTH;
  SetDriveLetter(#0);
  SetCDBFilename('');
  SetPaddingLength(0);
  SetOverwriteWithChaff(True);  // preserve plausible deniability by defaulting to true
  ftempCypherUseKeyLength := 0;

  fnewVolumeMountedAs := #0;

  pnlWarningOffset.Caption := '';

  // Create a border around the warning
  // We can't just place a bigger panel under the warning panel, as on a
  // Windows system with themes, the top panel turns red, and the (red) text
  // can't be read.
  { DONE 1 -otdk -crefactor : clean this up, just use background, - so can use align property }
  { TODO 1 -otdk -ccomplete : check text doesnt dissappear with themes -if so just reset colour }

  // tsFileOrPartition
  rgFileOrPartition.Items.Clear();
  rgFileOrPartition.Items.Add(FILEORPART_OPT_VOLUME_FILE);
  rgFileOrPartition.Items.Add(FILEORPART_OPT_PARTITION);
  rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  fmeSelectPartition.AllowCDROM := False;
  fmeSelectPartition.OnChange   := fmeSelectPartitionChanged;
  //  fmeSelectPartition.Initialize();
  fmeSelectPartition.Tag        := 1;

  // tsFilename
  lblFilename.Text := '';

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
  ckRNGcryptlib.Enabled := GetRandPool().CanUseCryptLib;
  if ckRNGcryptlib.Enabled then
    ckRNGcryptlib.Checked := True;
  ckRNGPKCS11.Enabled := PKCS11LibraryReady(GetFreeOTFEBase().PKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  PopulatePKCS11Tokens();

  // tsRNGGPG
  lblGPGFilename.Caption := '';



  // BECAUSE WE DEFAULTED TO USING A VOLUME *FILE*, WE MUST SET THE PARTITION
  // SELECT PAGE TO "DONE" HERE (otherwise, it'll never be flagged as complete
  // if the user doesn't switch)
  tsPartitionWarning.Tag := 1;
  tsPartitionSelect.Tag  := 1;

  // BECAUSE WE DEFAULTED TO USING THE MOUSERNG, WE MUST SET THE GPG PAGE TO
  // "DONE" HERE (otherwise, it'll never be flagged as complete if the user
  // doens't switch RNGs)
  //  tsRNGGPG.Tag := 1; set to not req'd


  ckAutoMountAfterCreate.Checked := True;
  // Note: ckAutoMountAfterCreate.Visible set in EnableDisableControls(...);
  //       if it's set to FALSE here, the control still appears for some
  //       reason?!


  lblFinished.Visible := False;//'click finished or next'

  UpdateUIAfterChangeOnCurrentTab();

end;

//all non-skipped tabs that are not required to be completed are listed here
function TfrmWizardCreateVolume.IsTabRequired(tabSheet: TTabSheet): Boolean;
begin
  Result := not ((tabSheet = tsDriveLetter) or (tabSheet = tsHashCypherIV) or
    (tabSheet = tsChaff) or (tabSheet = tsMasterKeyLength) or (tabSheet = tsRNGSelect) or
    (tabSheet = tsRNGMouseMovement) or (tabSheet = tsRNGGPG) or
    (tabSheet = tsRNGPKCS11) or (tabSheet = tsKeyIterations) or (tabSheet = tsSalt) or
    (tabSheet = tsCDBLocation) or (tabSheet = tsPadding) or (tabSheet = tsSummary));
end;

// Returns TRUE if the specified tab should be skipped, otherwise FALSE
function TfrmWizardCreateVolume.IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited IsTabSkipped(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then
    Result := (GetFreeOTFEBase() is TOTFEFreeOTFEDLL);


  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsFilename) then
    Result := GetIsPartition();


  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionWarning) then
    Result := not GetIsPartition();


  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then
    Result := not GetIsPartition();


  // Skip offset if user isn't creating a hidden volume
  if (tabSheet = tsOffset) then
    Result := not GetIsHidden();


  // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
  if (tabSheet = tsMasterKeyLength) then begin
    if ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1)) then begin
      Result := (SelectedCypherKeySize() >= 0);
    end;
  end;

  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then
    Result := not (rngMouseMovement in GetRNGSet());


  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
    Result := not (rngPKCS11 in GetRNGSet());


  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then
    Result := not (rngGPG in GetRNGSet());

end;



procedure TfrmWizardCreateVolume.Update_tempCypherUseKeyLength;
begin
  // If overwriting with encrypted data (chaff), find the size of the key required
  ftempCypherUseKeyLength := 0;
  if GetOverwriteWithChaff() then begin
    GetFreeOTFEBase().GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(),
      ftempCypherDetails);

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
    (GetIsPartition() and not (ckPartitionHidden.Checked) and not
    (fmeSelectPartition.SyntheticDriveLayout));
  lblPartitionDiskSize.Visible      := ckSizeEntirePartitionDisk.Visible;
  // If the control's not visible, don't let it be selected; the user enters
  // the size
  if not (ckSizeEntirePartitionDisk.Visible) then
    ckSizeEntirePartitionDisk.Checked := False;

  SDUEnableControl(se64UnitSize, not (ckSizeEntirePartitionDisk.Checked));

  // Prevent user from disabling the automount when creating volumes using the
  // DLL version
  // Toggle the "automount after create" checkbox visible on/off.
  // For some reason, the Visible setting doesn't "take" if set to FALSE
  // in FormShow(...) (i.e. the control is still visible)
  ckAutoMountAfterCreate.Visible := True;
  ckAutoMountAfterCreate.Visible := False;
  ckAutoMountAfterCreate.Visible := not (GetFreeOTFEBase() is TOTFEFreeOTFEDLL);

  SDUEnableControl(gbKeyfile, rbCDBInKeyfile.Checked);
  { TODO 1 -otdk -crefactor : disable chaff if hidden - not used }
end;

procedure TfrmWizardCreateVolume.pbBrowseFilenameClick(Sender: TObject);
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, GetVolFilename);
  if SaveDialog.Execute then begin
    SetVolFilename(SaveDialog.Filename);
    se64UnitByteOffset.Value := 0;
    if (FileExists(GetVolFilename)) then begin
      // Force the user to reenter their offset, by marking the offset tabsheet
      // as incomplete
      tsOffset.Tag := 0;
    end else begin
      // New file; critical data automatically stored at the start of the file;
      // zero offset
      tsOffset.Tag := 1;
    end;

  end;

  UpdateUIAfterChangeOnCurrentTab();

end;

 // This procedure will check to see if all items on the current tabsheet have
 // been successfully completed
function TfrmWizardCreateVolume.IsTabComplete(checkTab: TTabSheet): Boolean;
var
  randomBitsGenerated:      Integer;
  testSize:                 Int64;
  volSizeWithCDB:           ULONGLONG;
  volSizeWithCDBAndPadding: ULONGLONG;
  aSelectedCypherBlockSize: Integer;
  int64Zero:                Int64;
begin
  inherited;

  //if not req'd assume complete
  Result := not IsTabRequired(checkTab);

  if (checkTab = tsFileOrPartition) then begin
    // Ensure one option has been selected
    Result := (rgFileOrPartition.ItemIndex >= 0);
  end else
  if (checkTab = tsFilename) then begin
    // If we're creating a volume within a volume file, a filename must be specified
    if GetIsPartition() then
      Result := True
    else
      // Flag tabsheet complete if a filename has been specified
      Result := (GetVolFilename <> '');

  end else
  if (checkTab = tsPartitionWarning) then begin
    // Warning only; nothing for the user to actually do on this tab
    Result := True;
  end else
  if (checkTab = tsPartitionSelect) then begin
    // If we're creating a volume on a partition, one must be selected
    if not GetIsPartition() then begin
      Result := True;
    end else begin
      Result := (fmeSelectPartition.SelectedDevice <> '');
    end;

  end else
  if (checkTab = tsOffset) then begin
    // Check that the number entered is less than the max size...
    if GetIsPartition() then begin
      Result := (GetOffset() >= 0);
    end else begin
      Result := (GetOffset() < SDUGetFileSize(GetVolFilename())) and (GetOffset() >= 0);
    end;

  end else
  if (checkTab = tsSize) then begin
    // Some sensible minimum volume size
    // Use explicit int64 to prevent Delphi converting Size to int
    testSize := BYTES_IN_MEGABYTE;
    Result   := (GetSize() >= testSize);

    if Result then begin
      // Min sector size
      testSize := ASSUMED_HOST_SECTOR_SIZE;
      Result   := ((GetSize() mod testSize) = 0);
    end;

    if Result then begin
      volSizeWithCDB := GetSize();
      if GetCDBInVolFile() then begin
        volSizeWithCDB := volSizeWithCDB + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      end;

      volSizeWithCDBAndPadding := volSizeWithCDB + GetPaddingLength();

      if GetIsPartition() then begin
        if fmeSelectPartition.SyntheticDriveLayout then begin
          // We don't know the size of the selected drive/partition; assume user
          // input OK
          Result := True;
        end else begin
          // Note: fmeSelectPartition.SelectedSize() already takes into account
          //       whether or not the "entire disk" option was selected on the
          //       frame
          Result := ((fmeSelectPartition.SelectedSize() - GetOffset()) >=
            volSizeWithCDBAndPadding);
        end;
      end else
      if (FileExists(GetVolFilename)) then begin
        // If creating a hidden container, ensure that the existing file is large
        // enough to store the hidden container
        Result := ((SDUGetFileSize(GetVolFilename) - GetOffset()) >= volSizeWithCDBAndPadding);
      end else begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)
        Result := True;
      end;
    end;

  end else
  if (checkTab = tsHashCypherIV) then begin
    Result := ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1));
  end else
  if (checkTab = tsMasterKeyLength) then begin
    // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
    if (SelectedCypherKeySize() >= 0) then begin
      Result := True;
    end else begin
      // Otherwise, the keysize must be a multiple of 8 bits
      Result := ((seMasterKeyLength.Value mod 8) = 0);
    end;

  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    Result := (GetRNGSet() <> []);
  end else
  if (checkTab = tsRNGMouseMovement) then begin
    if not (rngMouseMovement in GetRNGSet) then begin
      // Mouse RNG not used - automatically completed
      Result := True;
    end else begin
      // This is a good place to update the display of the number of random bits
      // generated...
      randomBitsGenerated     := CountMouseRNGData();
      lblMouseRNGBits.Caption := Format(_('Random bits generated: %d/%d'),
        [randomBitsGenerated, RNG_requiredBits()]);

      Result           := (randomBitsGenerated >= RNG_requiredBits());
      MouseRNG.Enabled := not (Result);
      if MouseRNG.Enabled then begin
        MouseRNG.Color := clWindow;
      end else begin
        MouseRNG.Color := clBtnFace;
      end;
    end;

  end else
  if (checkTab = tsRNGPKCS11) then begin
    // Must have at least one RNG selected
    Result := (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0));
  end else
  if (checkTab = tsRNGGPG) then begin
    // Flag tabsheet complete if a GPG executable has been specified
    Result := (lblGPGFilename.Caption <> '');
  end else
  if (checkTab = tsPassword) then begin
    // Ensure the password is confirmed, and something has been entered as a
    // password
    Result              := frmeNewPassword.IsPasswordValid;
    //enable 'finished' text iff done
    lblFinished.Visible := Result;
  end else
  if (checkTab = tsSummary) then begin
    // Always completed
    Result := True;
  end;
  if (checkTab = tsKeyIterations) then begin
    // Ensure number of key iterations is > 0
    if (seKeyIterations.Value <= 0) then begin
      SDUMessageDlg(_('The number of key iterations must be 1 or more'), mtError);
      //      pcAdvancedOpts.ActivePage := tsKeyIterations;
      ActiveControl := seKeyIterations;
      Result        := False;
    end;
  end;

  if (checkTab = tsSalt) and Result then begin
    // Ensure salt length is a multiple of 8 bits
    if ((seSaltLength.Value mod 8) <> 0) then begin
      SDUMessageDlg(_('The salt length must be a multiple of 8'), mtError);
      //      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl := seSaltLength;
      Result        := False;
    end;
    // Ensure salt length is a multiple of the cypher's blocksize
    aSelectedCypherBlockSize := SelectedCypherBlockSize();
    if ((aSelectedCypherBlockSize > 0) and
      ((seSaltLength.Value mod aSelectedCypherBlockSize) <> 0)) then begin
      SDUMessageDlg(Format(_(
        'The salt length must be a multiple of the cypher''s blocksize (%d)'),
        [aSelectedCypherBlockSize]),
        mtError);
      //      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl := seSaltLength;
      Result        := False;
    end;
  end;

  if (checkTab = tsCDBLocation) and Result then begin
    // Either the user's selected to include the CDB in their volume file, or
    // they've specified a keyfilename
    Result := rbCDBInVolFile.Checked or (rbCDBInKeyfile.Checked and (GetCDBFilename() <> ''));
    if not Result then begin
      SDUMessageDlg(_(
        'If you wish to store the volume''s CDB in a keyfile, you must specify the file to use'), mtError);
      ActiveControl := tsCDBLocation;
      Result        := False;
    end;
  end;

  if (checkTab = tsPadding) and Result then begin
    // Padding can't be less than zero
    int64Zero := 0;
    Result    := (se64Padding.Value >= int64Zero);
    if not Result then begin
      SDUMessageDlg(_('The amount of padding data added to a volume must be zero or greater'),
        mtError);
      ActiveControl := tsPadding;
      Result        := False;
    end;
  end;


  if (checkTab = tsChaff) and Result then begin
    // If overwriting with encrypted data (chaff), find the size of the key required , and ensure can get that random data
    ftempCypherUseKeyLength := 0;
    if GetOverwriteWithChaff() then begin
      Update_tempCypherUseKeyLength;

      // If MouseRNG is being used, check that enough random data is availabe...
      if not (IsTabComplete(tsRNGMouseMovement)) then begin
        SDUMessageDlg(
          _(
          'Additional random data is required in order to generate an encryption key for use when producing the padding data.'),
          mtInformation
          );
        ActiveControl := tsRNGMouseMovement;
        Result        := False;
      end;

    end;
  end;
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


  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardCreateVolume.ckSizeEntirePartitionDiskClick(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.fmePasswordChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
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
  Result := ((GetIsPartition()) and (ckPartitionHidden.Checked)) or
    ((not (GetIsPartition())) and (FileExists(GetVolFilename)));
end;

function TfrmWizardCreateVolume.GetVolFilename(): String;
begin
  Result := '';

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := lblFilename.Text;
  end;
end;

procedure TfrmWizardCreateVolume.SetVolFilename(filename: String);
begin
  lblFilename.Text := filename;
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
    if GetCDBInVolFile() then begin
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

function TfrmWizardCreateVolume.GetHashDriver(): String;
begin
  Result := '';

  if (cbHash.ItemIndex >= 0) then begin
    Result := fHashKernelModeDriverNames[cbHash.ItemIndex];
  end;
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
begin
  Result := '';

  if (cbCypher.ItemIndex >= 0) then begin
    Result := fCypherKernelModeDriverNames[cbCypher.ItemIndex];
  end;
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
begin
  Result := foivgUnknown;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then begin
    Result := TFreeOTFESectorIVGenMethod(
      cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
  end;
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

function TfrmWizardCreateVolume.GetRandomData_PaddingKey(): TSDUBytes;
var
  len: Integer;
begin
  // Use the last FPadWithEncryptedDataKeyLen bits as the CDB random data,
  // after the CDB random data
  //  i   := (CRITICAL_DATA_LENGTH div 8) + 1;
  len := (ftempCypherUseKeyLength div 8);
  { TODO 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
  GetRandPool().GetRandomData(len, Result);

end;



function TfrmWizardCreateVolume.GetMasterKeyLength(): Integer;
begin
  Result := SelectedCypherKeySize();
  if (Result < 0) then
    Result := seMasterKeyLength.Value;
end;


procedure TfrmWizardCreateVolume.edByteOffsetChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardCreateVolume.SizeChanged(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

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

  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardCreateVolume.MouseRNGByteGenerated(Sender: TObject; random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < RNG_requiredBits()) then begin
    AddToMouseRNGData(random);
  end;

  UpdateUIAfterChangeOnCurrentTab();

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
    if (GetFreeOTFEBase().GetHashList(tmpDisplayTitles, fHashKernelModeDriverNames, fHashGUIDs))
    then begin
      // Strip out all hashes which have:
      //   length <= 0 or blocksize <= 0
      //  - they cannot be used to create new FreeOTFE volumes as they use
      // PBKDF2 (HMAC) to derive the critical data key; and PBKDF2 with HMAC
      // requires that the hash used has a defined length of greater than zero
      for i := (tmpDisplayTitles.Count - 1) downto 0 do begin
        if not (GetFreeOTFEBase().GetSpecificHashDetails(fHashKernelModeDriverNames[i],
          StringToGUID(fHashGUIDs[i]), hashDetails)) then begin
          // Just warn user, and ignore...
          SDUMessageDlg(Format(_('Unable to obtain hash details for %s'),
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
          'You do not appear to have any FreeOTFE hash drivers that can be used to create new LibreCrypt volumes installed and started.') +
          SDUCRLF + SDUCRLF + _(
          'If you have only just installed LibreCrypt, you may need to restart your computer.'),
          mtError
          );
      end;

    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of hashes.') + SDUCRLF + SDUCRLF + _(
        'Please ensure that you have one or more FreeOTFE hash drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed LibreCrypt, you may need to restart your computer.'),
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
    if (GetFreeOTFEBase().GetCypherList(tmpDisplayTitles, fCypherKernelModeDriverNames,
      fCypherGUIDs)) then begin
      cbCypher.Items.Clear();
      cbCypher.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more FreeOTFE cypher drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed LibreCrypt, you may need to restart your computer.'),
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
  if (pcWizard.ActivePage = tsPassword) then begin
    frmeNewPassword.SetFocus;
    frmeNewPassword.preUserKeyFirst.SetFocus;
  end;
end;

procedure TfrmWizardCreateVolume.FormCreate(Sender: TObject);
var
  dl: ansichar;
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
  GetRandPool();
  //  fcanUseCryptlib :=


  // tsKeyIterations

  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;

  OnWizardStepChanged := FormWizardStepChanged;

  // tsRequestedDriveLetter
  cbDriveLetter.Items.Clear();
  cbDriveLetter.Items.Add(_('Use default'));
  //  for dl:='C' to 'Z' do
  for dl := 'A' to 'Z' do
    cbDriveLetter.Items.Add(dl + ':');

  // Autoselect the "default" (first) entry
  cbDriveLetter.ItemIndex := 0;

  // tsCDBLocation
  rbCDBInKeyfile.Checked   := False;
  rbCDBInVolFile.Checked   := True;
  lblKeyFilename.Caption   := '';
  frmeNewPassword.OnChange := fmePasswordChange;
end;

procedure TfrmWizardCreateVolume.FormDestroy(Sender: TObject);
begin

  fHashKernelModeDriverNames.Free();
  fHashGUIDs.Free();
  fCypherKernelModeDriverNames.Free();
  fCypherGUIDs.Free();
  fIVGenMethodIDs.Free();

  fDeviceList.Free();
  fDeviceTitle.Free();

  PurgeMouseRNGData();

  //overwrite any seed asap
  {
  for i := 1 to length(fCombinedRandomData) do begin
    fCombinedRandomData[i] := ansichar(i);
  end;
   }
end;

procedure TfrmWizardCreateVolume.rbCDBLocationClick2(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmWizardCreateVolume.cbDriveLetterChange(Sender: TObject);
begin
  inherited;
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.cbHashCypherIVGenChange(Sender: TObject);
begin
  // Cypher changed; clear any cached keysize
  fCachedCypherKeysize   := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Cypher changed; clear any cached blocksize
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Cypher changed; clear any cached mode
  fCachedCypherMode      := focmUnknown;

  UpdateUIAfterChangeOnCurrentTab();

end;


procedure TfrmWizardCreateVolume.PopulateSummary();
var
  totalSize:     ULONGLONG;
  driveLetter:   String;
  RNGs:          String;
  ULLZero:       ULONGLONG;
  PaddingLength: Int64;
  VolFilename:   TFilename;
  volSize:       ULONGLONG;
begin
  // int64Zero to prevent 32/64 bit integer conversion
  ULLZero       := 0;
  PaddingLength := GetPaddingLength();
  VolFilename   := GetVolFilename();
  reSummary.Lines.Clear();
  volSize := GetSize();
  if GetIsPartition() then
    reSummary.Lines.Add(Format(_('Partition: %s'), [VolFilename]))
  else
    reSummary.Lines.Add(Format(_('Filename: %s'), [VolFilename]));


  if GetIsHidden() then
    reSummary.Lines.Add(Format(_('Hidden volume starting at offset: %d'), [GetOffset()]));

  if GetCDBInVolFile() then begin
    if (PaddingLength > ULLZero) then begin
      totalSize := volSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8) + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) + %3 (padding) = %4 bytes'),
        [SDUIntToStr(volSize), (CRITICAL_DATA_LENGTH div 8), SDUIntToStr(PaddingLength),
        SDUIntToStr(totalSize)]));
    end else begin
      totalSize := volSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) = %3 bytes'),
        [SDUIntToStr(volSize), (CRITICAL_DATA_LENGTH div 8), SDUIntToStr(totalSize)]));
    end;

    reSummary.Lines.Add(_('CDB stored: At start of volume file'));
  end else begin
    if (PaddingLength > ULLZero) then begin
      totalSize := volSize + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (padding) = %3 bytes'),
        [SDUIntToStr(volSize), SDUIntToStr(PaddingLength), SDUIntToStr(totalSize)]));
    end else begin
      reSummary.Lines.Add(SDUParamSubstitute(_('Volume size: %1 bytes'), [SDUIntToStr(volSize)]));
    end;

    reSummary.Lines.Add(_('CDB stored: In separate keyfile'));
    reSummary.Lines.Add(SDUParamSubstitute(_('CDB keyfile: %1'), [GetCDBFilename()]));
  end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Hash algorithm: %1'),
    [cbHash.Items[cbHash.ItemIndex]]));
  reSummary.Lines.Add('  ' + Format(_('[Hash driver: %s]'), [GetHashDriver()]));
  reSummary.Lines.Add('  ' + Format(_('[Hash GUID: %s]'), [GUIDToString(GetHashGUID())]));

  reSummary.Lines.Add(Format(_('Key iterations: %d'), [seKeyIterations.Value]));


  if (SelectedCypherBlockSize() <= 0) then begin
    reSummary.Lines.Add(_('Sector IVs will not be used (cypher has zero or variable blocksize)'));
  end else begin
    reSummary.Lines.Add(_('Cypher has fixed, defined blocksize; sector IVs will be used'));
    reSummary.Lines.Add(SDUParamSubstitute(_('Sector IV generation method: %1'),
      [FreeOTFESectorIVGenMethodTitle[GetSectorIVGenMethod()]]));

    if GetUsePerVolumeIV() then
      reSummary.Lines.Add(_('Sector IVs will be XORd with a per-volume IV before use'))
    else
      reSummary.Lines.Add(_('A per-volume IV will not be used'));
  end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Cypher algorithm: %1'),
    [cbCypher.Items[cbCypher.ItemIndex]]));
  reSummary.Lines.Add('  ' + Format(_('[Cypher driver: %s]'), [GetCypherDriver()]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Cypher GUID: %1]'),
    [GUIDToString(GetCypherGUID())]));

  reSummary.Lines.Add(Format(_('Master key length: %s bits'), [GetMasterKeyLength()]));

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
  reSummary.Lines.Add(Format(_('RNG: %s'), [RNGs]));


  reSummary.Lines.Add(_('Password: <entered>'));
  reSummary.Lines.Add(Format(_('Salt length: %d bits'), [seSaltLength.Value]));

  driveLetter := GetDriveLetter();
  if (driveLetter = #0) then begin
    driveLetter := _('Use default');
  end else begin
    driveLetter := driveLetter + ':';
  end;
  reSummary.Lines.Add(Format(_('Requested drive letter: %s'), [driveLetter]));

end;

function TfrmWizardCreateVolume.GetRNGSet(): TRNGSet;
begin
  Result := [];

  if ckRNGCryptoAPI.Checked then
    Result := Result + [rngCryptoAPI];

  if ckRNGMouseMovement.Checked then
    Result := Result + [rngMouseMovement];

  if ckRNGcryptlib.Checked then
    Result := Result + [rngcryptlib];

  if ckRNGPKCS11.Checked then
    Result := Result + [rngPKCS11];

  if ckRNGGPG.Checked then
    Result := Result + [rngGPG];
end;

procedure TfrmWizardCreateVolume.pbFinishClick(Sender: TObject);
begin
  inherited;

  if GetIsPartition() then begin
    if (SDUMessageDlg(_('You are about to create a new container on a disk/partition.' +
      SDUCRLF + SDUCRLF + 'This process will OVERWRITE that disk/partition.' +
      SDUCRLF + SDUCRLF + 'Are you SURE you wish to continue?'), mtWarning,
      [mbYes, mbNo], 0) = mrNo) then begin
      // Bail out...
      exit;
    end;
  end;
  //ensure chaff data is included
  Update_tempCypherUseKeyLength;
  GetRandPool.SetUpRandPool(GetRNGSet(),
    PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption);

  //  Result := GetRandPool.GetRandomData( (RNG_requiredBits() div 8), fCombinedRandomData);


  // *All* information required to create the new volume acquired - so create
  // the new volume
  try

    if CreateNewVolume() then begin
      PostCreate();
      ModalResult := mrOk;
    end;
  except
    on E: EInsufficientRandom do
      SDUMessageDlg(
        _('Insufficient random data generated: ' + E.message) + SDUCRLF +
        SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
        mtError
        );
  end;

  //  end;

end;


function TfrmWizardCreateVolume.CreateNewVolume(): Boolean;
var
  volumeDetails:     TVolumeDetailsBlock;
  CDBMetaData:       TCDBMetaData;
  saltBytes:         TSDUBytes;
  //  randomPool:        Ansistring;
  volumeFileSize:    ULONGLONG;
  cdbFile:           String;
  cdbOffset:         ULONGLONG;
  userCancel:        Boolean;
  junkBool:          Boolean;
  fileCreateProbMsg: String;
  VolFilename:       TFilename;
  //  randBytes:TSDUBytes;
  saltStr:           Ansistring;
begin
  Result      := True;
  VolFilename := GetVolFilename();
  // Create the volume file, if not a partition

  // If the new volume is on a partition; we can skip creating a large file
  // on the localfilesystem
  if not GetIsPartition() then begin
    // Create the volume file (if it doesn't already exist)
    if (not (FileExists(VolFilename))) then begin
      volumeFileSize := GetSize();
      // Add on the size of the CDB, if stored in volume file
      if GetCDBInVolFile() then begin
        volumeFileSize := volumeFileSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      end;

      // Add on the size of any padding
      volumeFileSize := volumeFileSize + GetPaddingLength();

      Result := SDUCreateLargeFile(VolFilename, volumeFileSize, True, userCancel);
      if not Result then
        // If there was a problem, and not a user cancel, warn user
        if userCancel then begin
          SDUMessageDlg(_('Container creation canceled'), mtInformation);
        end else begin
          fileCreateProbMsg :=
            Format(_(
            'Unable to create Container; please ensure you have %s free on the relevant drive'),
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

  if Result then begin
    // Overwrite volume with SPRNG data, if required
    // this also overwrites cdb and padding, cdb is created below
        { if hidden also overwrite  from Offset? - this is arguably overkill bc should already have been overwritten,
          but perhaps if didnt do on outer vol then will stop attacker knowing size of data
          also less info leaked if attacker has b4/after snapshots
          for now not overwriting for performance
         }
    { TODO 1 -otdk -cenhance : instead default FOverwriteWithChaff to false for hidden vols so user can still change}
    if GetOffset() = 0 then
      Result := OverwriteVolWithChaff();
  end;


  // Create separate CDB file, if needed
  if Result then begin
    if not GetCDBInVolFile() then begin
      Result := SDUCreateLargeFile(GetCDBFilename(), (CRITICAL_DATA_LENGTH div 8),
        False, junkBool);
      if not Result then begin
        SDUMessageDlg(_('Unable to create separate CDB keyfile.'), mtError);
      end;
    end;
  end;


  if Result then begin
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('Populating critical data structure...');
{$ENDIF}
    // Create the volume file header
    // Note: No need to populate CDBMetaData.MACAlgorithm or
    //       CDBMetaData.KDFAlgorithm as the function which writes the CDB out
    //       populates them automatically
    CDBMetaData.MACAlgorithm := fomacUnknown;
    CDBMetaData.KDFAlgorithm := fokdfUnknown;

    CDBMetaData.HashDriver   := GetHashDriver();
    CDBMetaData.HashGUID     := GetHashGUID();
    CDBMetaData.CypherDriver := GetCypherDriver();
    CDBMetaData.CypherGUID   := GetCypherGUID();
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('Using hash driver: '+CDBMetaData.HashDriver);
GetFreeOTFEBase().DebugMsg('Using hash: '+GUIDToString(CDBMetaData.HashGUID));
GetFreeOTFEBase().DebugMsg('Using cypher driver: '+CDBMetaData.CypherDriver);
GetFreeOTFEBase().DebugMsg('Using cypher: '+GUIDToString(CDBMetaData.CypherGUID));
{$ENDIF}


    volumeDetails.CDBFormatID  := CDB_FORMAT_ID;
    volumeDetails.PartitionLen := GetSize();

    volumeDetails.VolumeFlags := 0;
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('Volume flags: '+inttostr(volumeDetails.VolumeFlags)+' bits');
{$ENDIF}

    volumeDetails.SectorIVGenMethod := GetSectorIVGenMethod();

    { TODO 2 -otdk -csecurity : this copies data - but orig data not deleted so may be reused }
    //    randomPool := GetRandomData_CDB();

    volumeDetails.MasterKeyLength := GetMasterKeyLength();
    // Grab 'n' bytes from the random pool to use as the master key
    GetRandPool().GetRandomData(volumeDetails.MasterKeyLength div 8, volumeDetails.MasterKey);
    //volumeDetails.MasterKey       := SDUBytesToString( randBytes);
    assert(length(volumeDetails.MasterKey) = volumeDetails.MasterKeyLength div 8);
    //    volumeDetails.MasterKey       := Copy(randomPool, 1, (volumeDetails.MasterKeyLength div 8));

    // SDUDeleteFromStart(randomPool, volumeDetails.MasterKeyLength div 8);
    //    Delete(randomPool, 1, (volumeDetails.MasterKeyLength div 8));
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('Master key length: '+inttostr(volumeDetails.MasterKeyLength)+' bits');
GetFreeOTFEBase().DebugMsg('Master key follows:');
GetFreeOTFEBase().DebugMsgBinary(volumeDetails.MasterKey);
{$ENDIF}

    // Grab 'n' bytes from the random pool to use as the volume IV
    // *Only* if the cypher's blocksize is a fixed, +ve number of bits
    volumeDetails.VolumeIVLength := 0;
    if (SelectedCypherBlockSize() > 0) then
      if GetUsePerVolumeIV() then
        volumeDetails.VolumeIVLength := SelectedCypherBlockSize();

    GetRandPool().GetRandomData(volumeDetails.VolumeIVLength div 8, volumeDetails.VolumeIV);
    //  volumeDetails.VolumeIV       := SDUBytesToString( randBytes);
    assert(length(volumeDetails.VolumeIV) = volumeDetails.VolumeIVLength div 8);
    //    volumeDetails.VolumeIV := Copy(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
    // SDUDeleteFromStart(randomPool,volumeDetails.VolumeIVLength div 8 );

    //    Delete(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('Volume IV length: '+inttostr(volumeDetails.VolumeIVLength)+' bits');
GetFreeOTFEBase().DebugMsg('Volume IV follows:');
GetFreeOTFEBase().DebugMsgBinary(volumeDetails.VolumeIV);
{$ENDIF}


    volumeDetails.RequestedDriveLetter := GetDriveLetter();

    // Grab 'n' bytes from the random pool to use as the salt
    GetRandPool().GetRandomData(seSaltLength.Value div 8, saltBytes);
    saltStr := SDUBytesToString(saltBytes);
    assert(length(saltStr) = seSaltLength.Value div 8);
    //    saltBytes := Copy(randomPool, 1, (seSaltLength.Value div 8));
    // SDUDeleteFromStart(randomPool, (seSaltLength.Value div 8));
    { DONE 1 -otdk -crefactor : have randpool object }
    //    Delete(randomPool, 1, (seSaltLength.Value div 8));

{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('About to write the critical data...');
GetFreeOTFEBase().DebugMsg('Using password: '+preUserKey1.Text);
GetFreeOTFEBase().DebugMsg('Using salt... ');
GetFreeOTFEBase().DebugMsg('-- begin salt --');
GetFreeOTFEBase().DebugMsgBinary(saltBytes);
GetFreeOTFEBase().DebugMsg('-- end salt --');
{$ENDIF}

    // Determine filename and offset within file storing CDB
    GetCDBFileAndOffset(cdbFile, cdbOffset);

    // Write the header to the file, starting from the specified offset
    if not (GetFreeOTFEBase().WriteVolumeCriticalData(cdbFile, cdbOffset,
      frmeNewPassword.GetKeyPhrase(), saltBytes, seKeyIterations.Value,
      volumeDetails, CDBMetaData)) then begin
      SDUMessageDlg(
        _('Unable to write critical data block.'),
        mtError
        );
      Result := False;
    end;  // if not(OTFEFreeOTFE.WriteVolumeCriticalData(

  end;  // if result then

  //wipe sesitive data allocated
  SDUZeroBuffer(volumeDetails.MasterKey);
  SDUZeroBuffer(volumeDetails.VolumeIV);
  SDUZeroBuffer(saltBytes);

end;

procedure TfrmWizardCreateVolume.GetCDBFileAndOffset(out cdbFile: String;
  out cdbOffset: ULONGLONG);
begin
  cdbFile   := GetVolFilename();
  cdbOffset := GetOffset();
  if not GetCDBInVolFile() then begin
    // CDB stored in separate keyfile
    cdbFile   := GetCDBFilename();
    cdbOffset := 0;
{$IFDEF FREEOTFE_DEBUG}
GetFreeOTFEBase().DebugMsg('CDB stored in separate keyfile: '+cdbFile);
{$ENDIF}
  end;

end;

// Post-creation functionality
procedure TfrmWizardCreateVolume.PostCreate();
var
  MountedDrives:  String;
  errMsg:         WideString;
  cntMountOK:     Integer;
  cntMountFailed: Integer;
  mountedOK:      Boolean;
  tmpVolumeFiles: TStringList;
  cdbFile:        String;
  cdbOffset:      ULONGLONG;
  VolFilename:    tfilename;
begin
  if GetAutoMountAfterCreate() then begin
    VolFilename    := GetVolFilename();
    tmpVolumeFiles := TStringList.Create();
    try
      mountedOK := False;

      tmpVolumeFiles.Add(VolFilename);
      GetCDBFileAndOffset(cdbFile, cdbOffset);

      if (VolFilename = cdbFile) then begin
        cdbFile := '';
      end;

      MountedDrives := '';
      if GetFreeOTFEBase().MountFreeOTFE(tmpVolumeFiles, frmeNewPassword.GetKeyPhrase(),
        cdbFile, '',  // Empty string - read the CDB
        PKCS11_NO_SLOT_ID, nil,
              // PKCS#11 session not used
        nil,  // PKCS#11 secret key not used
        seKeyIterations.Value, GetDriveLetter(), False, // Readonly
        DEFAULT_MOUNTAS, GetOffset(), GetCDBInVolFile(), seSaltLength.Value, True,
        // Mount for all users
        MountedDrives) then begin
        GetFreeOTFEBase().CountMountedResults(
          MountedDrives,
          cntMountOK,
          cntMountFailed
          );

        if (cntMountOK = 1) then begin
          fnewVolumeMountedAs := MountedDrives[1];
          mountedOK           := True;
        end;
      end;
    finally
      tmpVolumeFiles.Free();
    end;

    if mountedOK then begin
      { TODO 2 -otdk -cinvestigate : why only DLL - should always format after creation, even if use windows dlg }
      if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
        PostMount_Format_using_dll();
      end else begin
        Format_Drive(FNewVolumeMountedAs, self);
      end;

    end;

    if not (mountedOK) then begin
      // Volumes couldn't be mounted for some reason...
      errMsg := _('Unable to open container.');
    end;
  end;

end;

// Format the volume specified
procedure TfrmWizardCreateVolume.PostMount_Format_using_dll();
var
  PartitionImage: TOTFEFreeOTFEDLL_PartitionImage;
  Filesystem:     TSDFilesystem_FAT;
begin
  if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
    PartitionImage             := TOTFEFreeOTFEDLL_PartitionImage.Create();
    PartitionImage.FreeOTFEObj := TOTFEFreeOTFEDLL(GetFreeOTFEBase());
    PartitionImage.MountedAs   := fnewVolumeMountedAs;
    // end else begin
    //   //  TODO test this   - could reformat wrong drive! -for now use format_drive for non dll volumes
    //
    //    PartitionImage := TSDPartitionImage_File.Create();
    ////    PartitionImage.GetFreeOTFEBase() := fFreeOTFEObj;
    //      (PartitionImage as TSDPartitionImage_File).Filename := VolFilename;
    //
    //    end;

    PartitionImage.Mounted := True;

    if not (PartitionImage.Mounted) then begin
      PartitionImage.Free();
      PartitionImage := nil;
      SDUMessageDlg('Container could be opened, but not mounted as a partition image?!', mtError);
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
  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardCreateVolume.seMasterKeyLengthExit(Sender: TObject);
begin
  // Ensure master key length is a multiple of 8 bits
  if ((seMasterKeyLength.Value mod 8) <> 0) then begin
    SDUMessageDlg(_('The master key length must be a multiple of 8'), mtWarning);
    ActiveControl := seMasterKeyLength;
  end;

  UpdateUIAfterChangeOnCurrentTab();
end;


 // Returns the keysize for the user's selected cyppher, as returned by the
 // driver
 // Returns CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE on error
function TfrmWizardCreateVolume.SelectedCypherKeySize(): Integer;
var
  cypherDetails: TFreeOTFECypher_v3;
begin
  Result := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Determine the keysize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherKeysize <> CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE) then begin
    Result := fCachedCypherKeysize;
  end else
  if not (GetFreeOTFEBase().GetSpecificCypherDetails(GetCypherDriver(),
    GetCypherGUID(), cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the keysize for your selected cypher.'), mtError);
  end else begin
    // Cache keysize and return...
    fCachedCypherKeysize := cypherDetails.KeySizeRequired;
    Result               := fCachedCypherKeysize;
  end;
end;

 // Returns the blocksize for the user's selected cyppher, as returned by the
 // driver
 // Returns CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE on error
function TfrmWizardCreateVolume.SelectedCypherBlockSize(): Integer;
var
  cypherDetails: TFreeOTFECypher_v3;
begin
  Result := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Determine the blocksize of the cypher selected

  // If we have it cached alredy, use it
  if (fCachedCypherBlocksize <> CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE) then begin
    Result := fCachedCypherBlocksize;
  end else
  if not (GetFreeOTFEBase().GetSpecificCypherDetails(GetCypherDriver(),
    GetCypherGUID(), cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the blocksize for your selected cypher.'), mtError);
  end else begin
    // Cache blocksize and return...
    fCachedCypherBlocksize := cypherDetails.BlockSize;
    Result                 := fCachedCypherBlocksize;
  end;
end;


 // Returns the cypher mode for the user's selected cypher, as returned by
 // the driver
function TfrmWizardCreateVolume.SelectedCypherMode(): TFreeOTFECypherMode;
var
  cypherDetails: TFreeOTFECypher_v3;
begin
  Result := focmUnknown;

  // If we have it cached alredy, use it
  if (fCachedCypherMode <> focmUnknown) then begin
    Result := fCachedCypherMode;
  end else
  if not (GetFreeOTFEBase().GetSpecificCypherDetails(GetCypherDriver(),
    GetCypherGUID(), cypherDetails)) then begin
    SDUMessageDlg(_('Unable to determine the cypher mode for your selected cypher.'), mtError);
  end else begin
    // Cache blocksize and return...
    fCachedCypherMode := cypherDetails.Mode;
    Result            := fCachedCypherMode;
  end;
end;


procedure TfrmWizardCreateVolume.pbHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetHashDriver();
  GUID       := GetHashGUID();
  GetFreeOTFEBase().ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.pbCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetCypherDriver();
  GUID       := GetCypherGUID();
  GetFreeOTFEBase().ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmWizardCreateVolume.rbCDBLocationClick(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

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

  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardCreateVolume.rgFileOrPartitionClick(Sender: TObject);
begin
  // Size tab must be marked as incomplete if the user switched to partition,
  // just in case the user previously entered a size value that's now too big
  if GetIsPartition() then
    tsSize.Tag := 0;

  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.se64ByteOffsetChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.seSizeChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.seSizeExit(Sender: TObject);
var
  minSize: Int64;
begin
  // Use explicit int64 to prevent Delphi converting Size to int
  minSize := (MIN_REC_VOLUME_SIZE * BYTES_IN_MEGABYTE);

  // Warn user if volume size smaller than PDA version/ hidden min
  if (GetSize() < minSize) then begin
    SDUMessageDlg(
      Format(_('Please note: If you would like to be able to add a hidden container to your container later, please use a size greater than %d MB'), [MIN_REC_VOLUME_SIZE]),
      mtWarning
      );
  end;

  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardCreateVolume.PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(
    GetFreeOTFEBase().PKCS11Library, cbToken) > 0);
end;

procedure TfrmWizardCreateVolume.pbRefreshClick(Sender: TObject);
begin
  PopulatePKCS11Tokens();
end;

procedure TfrmWizardCreateVolume.fmeSelectPartitionChanged(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;

  lblPartitionDiskSize.Caption :=
    Format(_('(Approx: %s)'), [SDUFormatAsBytesUnits(fmeSelectPartition.SelectedSize())]);

  // Size tab must be marked as incomplete
  tsSize.Tag := 0;

  UpdateUIAfterChangeOnCurrentTab();

end;


function TfrmWizardCreateVolume.GetCDBInVolFile(): Boolean;
begin
  Result := (Trim(GetCDBFilename()) = '');
end;

function TfrmWizardCreateVolume.GetAutoMountAfterCreate(): Boolean;
begin
  Result := ckAutoMountAfterCreate.Checked;
end;


{overwrites volume with 'chaff' from 'Offset' -
uses seleced cypher and options as for enxn
}

function TfrmWizardCreateVolume.OverwriteVolWithChaff(): Boolean;
var
  shredder: TShredder;

  overwriteOK: TShredResult;
  failMsg:     String;
  //  tmpZero: ULONGLONG;
begin
  Result := True;

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
  if GetOverwriteWithChaff() then begin
    // Initilize zeroed IV for encryption
    ftempCypherEncBlockNo := 0;

    // Get *real* random data for encryption key
    ftempCypherKey := GetRandomData_PaddingKey();

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
      if GetIsPartition() then
        overwriteOK := shredder.DestroyDevice(GetVolFilename(), False, False)
      else
        overwriteOK := shredder.DestroyFileOrDir(GetVolFilename(),
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
        Result := False;
      end else
      if (overwriteOK = srUserCancel) then begin
        SDUMessageDlg(_('Overwrite of data cancelled.'), mtInformation);
        Result := False;
      end;


    finally
      shredder.Free();
    end;

  end;  // if (Result) then
  SDUInitAndZeroBuffer(0, ftempCypherKey);

  //  ftempCypherKey        := '';
  assert(SDUBytesToString(ftempCypherKey) = '');
  ftempCypherEncBlockNo := 0;

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
  i:                Integer;
  tempArraySize:    Cardinal;
  blocksizeBytes:   Cardinal;
  plaintext:        Ansistring;
  cyphertext:       Ansistring;
  IV:               Ansistring;
  localIV:          Int64;
  sectorID:         LARGE_INTEGER;
  tempCipherkeyStr: Ansistring;
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
    { DONE 2 -otdk -csecurity : This is not secure PRNG - but is encrypted below, so the result is secure }
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
  tempCipherkeyStr := SDUBytesToString(ftempCypherKey);
  if not (GetFreeOTFEBase().EncryptSectorData(GetCypherDriver(), GetCypherGUID(),
    sectorID, FREEOTFE_v1_DUMMY_SECTOR_SIZE, ftempCypherKey, IV, plaintext, cyphertext)) then begin
    SDUMessageDlg(
      _('Error: unable to encrypt pseudorandom data before using for overwrite buffer') +
      SDUCRLF + SDUCRLF + Format(_('Error #: %d'), [GetFreeOTFEBase().LastErrorCode]),
      mtError
      );

    generatedOK := False;
  end else begin
    // Copy the encrypted data into the outputBlock
    for i := 0 to (bytesRequired - 1) do
      outputBlock[i] := Byte(cyphertext[i + 1]);

    generatedOK := True;
  end;

end;

function TfrmWizardCreateVolume.RNG_requiredBits(): Integer;
begin
  Result := CRITICAL_DATA_LENGTH + ftempCypherUseKeyLength;
  // also same cypher used for chaff
  if GetOverwriteWithChaff() then
    Result := Result + ftempCypherUseKeyLength;
end;

function TfrmWizardCreateVolume.GetDriveLetter(): Char;
begin
  Result := #0;
  if (cbDriveLetter.ItemIndex > 0) then
    Result := cbDriveLetter.Items[cbDriveLetter.ItemIndex][1];
end;

procedure TfrmWizardCreateVolume.SetDriveLetter(driveLetter: Char);
begin
  cbDriveLetter.ItemIndex := 0;
  if (driveLetter <> #0) then begin
    cbDriveLetter.ItemIndex := cbDriveLetter.Items.IndexOf(driveLetter + ':');
  end;
end;

function TfrmWizardCreateVolume.GetCDBFilename(): String;
begin
  Result := '';
  if rbCDBInKeyfile.Checked then
    Result := trim(lblKeyFilename.Caption);
end;

procedure TfrmWizardCreateVolume.SetCDBFilename(CDBFilename: String);
begin
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := CDBFilename;
  if (CDBFilename <> '') then
    rbCDBInKeyfile.Checked := True;

  EnableDisableControls();
end;

procedure TfrmWizardCreateVolume.pbBrowseKeyfileClick(Sender: TObject);
begin
  keySaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  keySaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  keySaveDialog.Options    := keySaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(keySaveDialog, GetCDBFilename());
  if keySaveDialog.Execute then
    SetCDBFilename(keySaveDialog.Filename);

end;

function TfrmWizardCreateVolume.GetPaddingLength(): Int64;
begin
  Result := se64Padding.Value;
end;

procedure TfrmWizardCreateVolume.SetPaddingLength(len: Int64);
begin
  se64Padding.Value := len;
end;

function TfrmWizardCreateVolume.GetOverwriteWithChaff(): Boolean;
begin
  Result := rgOverwriteType.ItemIndex = 0;// first one is enxd
end;

procedure TfrmWizardCreateVolume.SetOverwriteWithChaff(secChaff: Boolean);
begin
  rgOverwriteType.ItemIndex := Ifthen(secChaff, 0, 1);
end;

procedure Format_drive(drivesToFormat: DriveLetterString; frm: TForm);
resourcestring
  FORMAT_CAPTION = 'Format';
var
  i:         Integer;
  currDrive: DriveLetterChar;
  driveNum:  Word;
  formatRet: DWORD;
  ok:        Boolean;
  //  hndle:HWND;
begin
  ok := True;
  for i := 1 to length(drivesToFormat) do begin
    currDrive := drivesToFormat[i];
    driveNum  := Ord(currDrive) - Ord('A');
    {TODO: use cmd line - se below}
    //http://stackoverflow.com/questions/2648305/format-drive-by-c
    formatRet := SHFormatDrive(frm.Handle, driveNum, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);

    if formatRet <> 0 then begin
      ok := False;
      break;
    end else begin
    {  dialog only returns when done - use cmd line as above
      // close format dlg
      // see also http://stackoverflow.com/questions/15469657/why-is-findwindow-not-100-reliable
      hndle := FindWindow('IEFrame', NIL);
      if hndle>0 then begin
      PostMessage(hndle, WM_CLOSE, 0, 0);
      end;
      }
    end;
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
  if (Pos(uppercase(FORMAT_CAPTION), uppercase(frm.Caption)) > 0) or not ok then begin
    frm.Caption := Application.Title;
    SDUMessageDlg(
      _('Your encrypted drive could not be formatted.') + SDUCRLF + SDUCRLF +
      _('Please lock this container and re-open it with the "Mount for all users" option checked, before trying again.'),
      mtError
      );
  end;

end;


end.
