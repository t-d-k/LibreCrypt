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
  SDUGeneral,  SDURandPool, SDUStdCtrls, SDUForms, SDUFrames, SDUSpin64Units,   PasswordRichEdit, Spin64,
  //freeotfe specific
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_VolumeFileAPI,  // Required for TVOLUME_CRITICAL_DATA
  OTFEFreeOTFE_DriverAPI,      // Required for CRITICAL_DATA_LEN

   OTFEFreeOTFE_fmeSelectPartition,
  OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_frmWizard, OTFEFreeOTFE_InstructionRichEdit, SDUDialogs;

type
  TfrmWizardCreateVolume = class (TfrmWizard)
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
    reInstructFilename: TRichEdit;
    reInstructOffset: TRichEdit;
    reInstructSize: TRichEdit;
    reInstructHashCypherIV: TRichEdit;
    reInstructRNGSelect: TRichEdit;
    reInstructPassword: TRichEdit;
    Label13:        TLabel;
    GroupBox1:      TGroupBox;
    pbBrowseFilename: TButton;
    Label17:        TLabel;
    Label7:         TLabel;
    SaveDialog:     TSDUSaveDialog;
    tsRNGMouseMovement: TTabSheet;
    reInstructRNGMouseMovement: TRichEdit;
    tsSummary:      TTabSheet;
    reSummary:      TRichEdit;
    tsRNGGPG:       TTabSheet;
    reInstructRNGGPG: TRichEdit;
    GroupBox2:      TGroupBox;
    lblGPGFilename: TSDUFilenameLabel;
    pbBrowseGPG:    TButton;
    GPGOpenDialog:  TSDUOpenDialog;
    MouseRNG:       TMouseRNG;
    tsMasterKeyLength: TTabSheet;
    reInstructMasterKeyLen: TRichEdit;
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
    reInstructPartitionSelect: TRichEdit;
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
    ckRNGPKCS11:    TCheckBox;
    tsRNGPKCS11:    TTabSheet;
    cbToken:        TComboBox;
    lblToken:       TLabel;
    pbRefresh:      TButton;
    reInstructRNGPKCS11: TRichEdit;
    ckSizeEntirePartitionDisk: TCheckBox;
    lblPartitionDiskSize: TLabel;
    fmeSelectPartition: TfmeSelectPartition;
    reInstructSummary: TRichEdit;
    se64UnitByteOffset: TSDUSpin64Unit_Storage;
    se64UnitSize:   TSDUSpin64Unit_Storage;
    reInstructWarningOffset: TRichEdit;
    ckAutoMountAfterCreate: TCheckBox;
    tsKeyIterations: TTabSheet;
    reInstructKeyIterations: TRichEdit;
    Label1: TLabel;
    seKeyIterations: TSpinEdit64;
    tsSalt: TTabSheet;
    seSaltLength: TSpinEdit64;
    Label8: TLabel;
    Label10: TLabel;
    tsDriveLetter: TTabSheet;
    reInstructDriveLetter: TRichEdit;
    Label11: TLabel;
    cbDriveLetter: TComboBox;
    tsCDBLocation: TTabSheet;
    reInstructCDBLocation: TRichEdit;
    rbCDBInVolFile: TRadioButton;
    rbCDBInKeyfile: TRadioButton;
    gbKeyfile: TGroupBox;
    lblKeyFilename: TSDUFilenameLabel;
    pbBrowseKeyfile: TButton;
    keySaveDialog: TSDUSaveDialog;
    tsPadding: TTabSheet;
    reInstructPadding: TRichEdit;
    Label12: TLabel;
    Label14: TLabel;
    tsChaff: TTabSheet;
    lblInstructChaff: TLabel;
    ckRNGGPG: TCheckBox;
    lblFilename: TEdit;
    Label15: TLabel;
    reInstructPartitionWarning: TRichEdit;
    lblWarningPartition: TLabel;
    rgOverwriteType: TRadioGroup;
    se64Padding: TSpinEdit64;
    lblFinished: TLabel;
    lblWelcomeBanner: TLabel;
    lblWelcomeClickNext: TLabel;
    lblInstructSalt: TLabel;
    lblInstructFileOrPartition: TLabel;
    lblInstructDriveLetter: TLabel;
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

    FnewVolumeMountedAs: Ansichar;

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

//    fCombinedRandomData: Ansistring;//todo: use global randpool rather than this

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
    function getVolFilename(): string;
    procedure SetVolFilename(filename: string);
    function getOffset(): ULONGLONG;
    // Note: The size returned *excludes* the size of the critical data
    function getSize(): ULONGLONG;
        // HashDriver - Kernel drivers: HashKernelDeviceName
    //              DLL drivers:    HashLibFilename
    function getHashDriver(): string;
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
    function GetDriveLetter: Ansichar;
    procedure SetDriveLetter(driveLetter: Ansichar);
    function GetCDBFilename: String;
    procedure SetCDBFilename(CDBFilename: String);
    function GetPaddingLength: Int64;
    procedure SetPaddingLength(len: Int64);
    function GetOverwriteWithChaff: Boolean;
    procedure SetOverwriteWithChaff(secChaff: Boolean);

  protected
    procedure SetupInstructions(); override;

    procedure EnableDisableControls(); override;

    function IsTabComplete(checkTab: TTabSheet): Boolean;override;

    function IsTabSkipped(tabSheet: TTabSheet): Boolean; override;
    function IsTabRequired(tabSheet: TTabSheet): Boolean;   override;

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
  SDUi18n,   SDPartitionImage,
  SDPartitionImage_File,
  SDFilesystem,
  SDFilesystem_FAT  ,
  //freeotfe/doxbox
  OTFEConsts_U, // Required for OTFE_ERR_USER_CANCEL
  OTFEFreeOTFEDLL_U,
  OTFEFreeOTFE_PKCS11,
//  OTFEFreeOTFE_frmWizardCreateVolumeAdvanced,
  OTFEFreeOTFEDLL_PartitionImage  ;

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
    SDUTranslateComp(reInstructKeyIterations);
    SDUTranslateComp(lblInstructSalt);
    SDUTranslateComp(lblInstructDriveLetter);
  //  reInstructDriveLetter.Text :=
//    _('Please enter the drive letter you would normally like this volume mounted under.' +
//    SDUCRLF + SDUCRLF + 'Note: You will be able to change this after the volume has been created.');

  reInstructCDBLocation.Text :=
    _('Please specify if you would like your volume''s critical data block (CDB) to be written at the start of the new volume, or stored in a separate keyfile.' + SDUCRLF + SDUCRLF +
    'The CDB is where metadata about your volume is stored, together with the master key used to carry out the actual encryption/decryption of your data.' + SDUCRLF + SDUCRLF +
    'For most users, including the CDB as part of the volume file is probably the best option.' + SDUCRLF + SDUCRLF +
    'If you store this information in a keyfile, you will need both your volume file AND a keyfile in order to mount your volume.' + SDUCRLF + SDUCRLF +
    'Note: Storing the CDB as part of the volume file will increase your volume''s size by 512 bytes.');


     reInstructPadding.Text :=
    _('"Padding" is additional random data added to the end of a volume file.'+SDUCRLF + SDUCRLF +
    'Any padding added will not be available for use as part of the mounted volume, and serves to increase the size of the volume.' + SDUCRLF + SDUCRLF +
     'Encrypted volumes typically have a file size that is a multiple of 512 bytes, or a "signature size" beyond the last 1MB boundry.'+
     ' To prevent this, you may wish to append random "padding" data to the new volume.' + SDUCRLF + SDUCRLF +
     'Padding also reduces the amount of information available to an attacker with respect to the maximum amount of the encrypted that may '+
     'actually be held within the volume.');

  SDUTranslateComp(lblInstructChaff);

  // Center label (done dynamically due to language translation size
  // differences)
  SDUCenterControl(lblWelcomeBanner, ccHorizontal);
  SDUCenterControl(lblWelcomeClickNext, ccHorizontal);
  SDUCenterControl(lblWarningPartition, ccHorizontal);
  SDUCenterControl(lblWarningOffset, ccHorizontal);

  // SDUInitAndZeroBuffer(0, fCombinedRandomData );
//  fCombinedRandomData := '';

  seKeyIterations.Value           := DEFAULT_KEY_ITERATIONS;
  seSaltLength.Value              := DEFAULT_SALT_LENGTH;
  SetDriveLetter(#0);
  SetCDBFilename('');
  SetPaddingLength(0);
  SetOverwriteWithChaff( True);  // preserve plausible deniability by defaulting to true
  ftempCypherUseKeyLength := 0;

  fnewVolumeMountedAs := #0;

  pnlWarningOffset.Caption    := '';


  // Create a border around the warning
  // We can't just place a bigger panel under the warning panel, as on a
  // Windows system with themes, the top panel turns red, and the (red) text
  // can't be read.
  { DONE 1 -otdk -crefactor : clean this up, just use background, - so can use align property }
  { TODO 1 -otdk -ccomplete : check text doesnt dissappear with themes -if so just reset colour }
 { SDUClearPanel(pnlWarningBorder_L);
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
  pnlWarningBorder_B.Height := WARNING_BORDER_WIDTH;   }


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
  ckRNGcryptlib.Enabled :=  GetRandPool().CanUseCryptLib;
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


  lblFinished.Visible := false;//'click finished or next'

  UpdateUIAfterChangeOnCurrentTab();

end;

//all non-skipped tabs that are not required to be completed are listed here
function TfrmWizardCreateVolume.IsTabRequired(tabSheet: TTabSheet): Boolean;
begin
  result := not ( (tabSheet  = tsDriveLetter)
                  or (tabSheet  = tsHashCypherIV) or (tabSheet  = tsChaff) or (tabSheet  = tsMasterKeyLength)
                  or (tabSheet  = tsRNGSelect) or (tabSheet  = tsRNGMouseMovement) or (tabSheet  = tsRNGGPG)
                  or (tabSheet  = tsRNGPKCS11) or  (tabSheet = tsKeyIterations)  or (tabSheet = tsSalt)
                  or (tabSheet = tsCDBLocation) or (tabSheet = tsPadding)
                  or (tabSheet  = tsSummary) );
end;

// Returns TRUE if the specified tab should be skipped, otherwise FALSE
function TfrmWizardCreateVolume.IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  result := inherited IsTabSkipped(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then
    result := (fFreeOTFEObj is TOTFEFreeOTFEDLL);


  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsFilename) then
    result := GetIsPartition();


  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionWarning) then
    result := not GetIsPartition();


  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then
    result := not GetIsPartition();


  // Skip offset if user isn't creating a hidden volume
  if (tabSheet = tsOffset) then
    result := not GetIsHidden();


  // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
  if (tabSheet = tsMasterKeyLength) then begin
    if ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1)) then begin
      result := (SelectedCypherKeySize() >= 0);
    end;
  end;

  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then
    result := not (rngMouseMovement in GetRNGSet());


  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
    result := not (rngPKCS11 in GetRNGSet());


  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then
    result := not (rngGPG in GetRNGSet());


end;



procedure TfrmWizardCreateVolume.Update_tempCypherUseKeyLength;
begin
  // If overwriting with encrypted data (chaff), find the size of the key required
  ftempCypherUseKeyLength := 0;
  if GetOverwriteWithChaff() then begin
    fFreeOTFEObj.GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(), ftempCypherDetails);

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
  if not (ckSizeEntirePartitionDisk.Visible) then     ckSizeEntirePartitionDisk.Checked := False;

  SDUEnableControl(se64UnitSize, not (ckSizeEntirePartitionDisk.Checked));

  // Prevent user from disabling the automount when creating volumes using the
  // DLL version
  // Toggle the "automount after create" checkbox visible on/off.
  // For some reason, the Visible setting doesn't "take" if set to FALSE
  // in FormShow(...) (i.e. the control is still visible)
  ckAutoMountAfterCreate.Visible := True;
  ckAutoMountAfterCreate.Visible := False;
  ckAutoMountAfterCreate.Visible := not (fFreeOTFEObj is TOTFEFreeOTFEDLL);

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
    SetVolFilename( SaveDialog.Filename);
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
  aSelectedCypherBlockSize:      Integer;
    int64Zero: Int64;
begin
  inherited;

  //if not req'd assume complete
  result :=not IsTabRequired(checkTab);

  if (checkTab = tsFileOrPartition) then begin
    // Ensure one option has been selected
    result := (rgFileOrPartition.ItemIndex >= 0);
  end else
  if (checkTab = tsFilename) then begin
    // If we're creating a volume within a volume file, a filename must be specified
    if GetIsPartition() then
      result := True
    else
      // Flag tabsheet complete if a filename has been specified
      result := (GetVolFilename <> '');

  end else
  if (checkTab = tsPartitionWarning) then begin
    // Warning only; nothing for the user to actually do on this tab
    result := True;
  end else
  if (checkTab = tsPartitionSelect) then begin
    // If we're creating a volume on a partition, one must be selected
    if not GetIsPartition() then begin
      result := True;
    end else begin
      result := (fmeSelectPartition.SelectedDevice <> '');
    end;

  end else
  if (checkTab = tsOffset) then begin
    // Check that the number entered is less than the max size...
    if GetIsPartition() then begin
      result := (GetOffset() >= 0);
    end else begin
      result := (GetOffset() < SDUGetFileSize(GetVolFilename())) and (GetOffset() >= 0);
    end;

  end else
  if (checkTab = tsSize) then begin
    // Some sensible minimum volume size
    // Use explicit int64 to prevent Delphi converting Size to int
    testSize := BYTES_IN_MEGABYTE;
    result    := (GetSize() >= testSize);

    if result then begin
      // Min sector size
      testSize := ASSUMED_HOST_SECTOR_SIZE;
      result    := ((GetSize() mod testSize) = 0);
    end;

    if result then begin
      volSizeWithCDB := GetSize();
      if GetCDBInVolFile() then begin
        volSizeWithCDB := volSizeWithCDB + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      end;

      volSizeWithCDBAndPadding := volSizeWithCDB + GetPaddingLength();

      if GetIsPartition() then begin
        if fmeSelectPartition.SyntheticDriveLayout then begin
          // We don't know the size of the selected drive/partition; assume user
          // input OK
          result := True;
        end else begin
          // Note: fmeSelectPartition.SelectedSize() already takes into account
          //       whether or not the "entire disk" option was selected on the
          //       frame
          result := ((fmeSelectPartition.SelectedSize() - GetOffset()) >= volSizeWithCDBAndPadding);
        end;
      end else
      if (FileExists(GetVolFilename)) then begin
        // If creating a hidden container, ensure that the existing file is large
        // enough to store the hidden container
        result := ((SDUGetFileSize(GetVolFilename) - GetOffset()) >= volSizeWithCDBAndPadding);
      end else begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)
        result := True;
      end;
    end;

  end else
  if (checkTab = tsHashCypherIV) then begin
    result := ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1));
  end else
  if (checkTab = tsMasterKeyLength) then begin
    // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
    if (SelectedCypherKeySize() >= 0) then begin
      result := True;
    end else begin
      // Otherwise, the keysize must be a multiple of 8 bits
      result := ((seMasterKeyLength.Value mod 8) = 0);
    end;

  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    result := (GetRNGSet() <> []);
  end else
  if (checkTab = tsRNGMouseMovement) then begin
    if not (rngMouseMovement in GetRNGSet) then begin
      // Mouse RNG not used - automatically completed
      result := True;
    end else begin
      // This is a good place to update the display of the number of random bits
      // generated...
      randomBitsGenerated     := CountMouseRNGData();
      lblMouseRNGBits.Caption := SDUParamSubstitute(
        _('Random bits generated: %1/%2'), [randomBitsGenerated,
        RNG_requiredBits()]);

      result            := (randomBitsGenerated >= RNG_requiredBits());
      MouseRNG.Enabled := not (result);
      if MouseRNG.Enabled then begin
        MouseRNG.Color := clWindow;
      end else begin
        MouseRNG.Color := clBtnFace;
      end;
    end;

  end else
  if (checkTab = tsRNGPKCS11) then begin
    // Must have at least one RNG selected
    result := (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0));
  end else
  if (checkTab = tsRNGGPG) then begin
    // Flag tabsheet complete if a GPG executable has been specified
    result := (lblGPGFilename.Caption <> '');
  end else
  if (checkTab = tsPassword) then begin
    // Ensure the password is confirmed, and something has been entered as a
    // password
    result := (preUserKey1.Text = preUserKey2.Text) and (preUserKey1.Text <> '');
    //enable 'finished' text iff done
    lblFinished.Visible := result;
  end else
  if (checkTab = tsSummary) then begin
    // Always completed
    result := True;
  end;
  if (checkTab = tsKeyIterations) then begin
      // Ensure number of key iterations is > 0
    if (seKeyIterations.Value <= 0) then begin
      SDUMessageDlg(_('The number of key iterations must be 1 or more'), mtError);
//      pcAdvancedOpts.ActivePage := tsKeyIterations;
      ActiveControl             := seKeyIterations;
//      allOK                     := False;
       result := false;
    end;
   end;

if (checkTab = tsSalt) and result then begin
    // Ensure salt length is a multiple of 8 bits
    if ((seSaltLength.Value mod 8) <> 0) then begin
      SDUMessageDlg(_('The salt length must be a multiple of 8'), mtError);
//      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl             := seSaltLength;
      result                     := False;
    end;
    // Ensure salt length is a multiple of the cypher's blocksize
    aSelectedCypherBlockSize := SelectedCypherBlockSize();
    if ((aSelectedCypherBlockSize > 0) and ((seSaltLength.Value mod aSelectedCypherBlockSize) <> 0)) then begin
      SDUMessageDlg(SDUParamSubstitute(
        _('The salt length must be a multiple of the cypher''s blocksize (%1)'),
        [aSelectedCypherBlockSize]),
        mtError);
//      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl             := seSaltLength;
      result                     := False;
    end;
  end;

  if (checkTab = tsCDBLocation) and result  then begin
       // Either the user's selected to include the CDB in their volume file, or
    // they've specified a keyfilename
    result := rbCDBInVolFile.Checked or (rbCDBInKeyfile.Checked and (GetCDBFilename() <> ''));
    if not result then begin
      SDUMessageDlg(_(
        'If you wish to store the volume''s CDB in a keyfile, you must specify the file to use'), mtError);
      ActiveControl := tsCDBLocation;
      result                     := False;
    end;
  end;

  if (checkTab = tsPadding) and result  then begin
    // Padding can't be less than zero
    int64Zero := 0;
    result     := (se64Padding.Value >= int64Zero);
    if not result then begin
      SDUMessageDlg(_('The amount of padding data added to a volume must be zero or greater'),
        mtError);
      ActiveControl := tsPadding;
      result                     := False;
    end;
  end;


   if (checkTab = tsChaff) and result  then begin
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
          result                     := False;
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

procedure TfrmWizardCreateVolume.preUserKeyChange(Sender: TObject);
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

function TfrmWizardCreateVolume.GetVolFilename(): string;
begin
  Result := '';

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := lblFilename.Text;
  end;
end;

procedure TfrmWizardCreateVolume.SetVolFilename(filename: string);
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

function TfrmWizardCreateVolume.GetHashDriver(): string;
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

   (*
function TfrmWizardCreateVolume.GetRandomData_CDB(): Ansistring;
var
  len: Integer;
begin
  { TODO 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
  // Use the first CRITICAL_DATA_LENGTH bits as the CDB random data
  len := (CRITICAL_DATA_LENGTH div 8);
//  assert(1 + len <= length(fCombinedRandomData), 'not enough rand data available');
//  Result := Copy(fCombinedRandomData, 1, (CRITICAL_DATA_LENGTH div 8));
  GetRandPool().GetRandomData(len,Result) ;
  //  raises Exception if fails
end;   *)

function TfrmWizardCreateVolume.GetRandomData_PaddingKey(): TSDUBytes;
var
  len: Integer;
begin
  // Use the last FPadWithEncryptedDataKeyLen bits as the CDB random data,
  // after the CDB random data
//  i   := (CRITICAL_DATA_LENGTH div 8) + 1;
  len := (ftempCypherUseKeyLength div 8);
  { TODO 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
//  assert(i + len <= length(fCombinedRandomData), 'not enough rand data available');
//  Result := Copy(fCombinedRandomData, i, len);
GetRandPool().GetRandomData(len,Result) ;

end;

function TfrmWizardCreateVolume.GetPassword(): Ansistring;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  Result := preUserKey1.Text;
end;

function TfrmWizardCreateVolume.GetMasterKeyLength(): Integer;
begin
  Result := SelectedCypherKeySize();
  if (Result < 0) then begin
    Result := seMasterKeyLength.Value;
  end;
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
  for dl := 'A' to 'Z' do     cbDriveLetter.Items.Add(dl + ':');

  // Autoselect the "default" (first) entry
  cbDriveLetter.ItemIndex := 0;

    // tsCDBLocation
  rbCDBInKeyfile.Checked := False;
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := '';

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
  totalSize:   ULONGLONG;
  driveLetter: String;
  RNGs:        String;
  ULLZero:     ULONGLONG;
  PaddingLength : Int64;
  VolFilename :TFilename;
  volSize:   ULONGLONG;
begin
  // int64Zero to prevent 32/64 bit integer conversion
  ULLZero := 0;
  PaddingLength:= GetPaddingLength();
  VolFilename:= GetVolFilename();
  reSummary.Lines.Clear();
  volSize := GetSize();
  if GetIsPartition() then
    reSummary.Lines.Add(SDUParamSubstitute(_('Partition: %1'), [VolFilename]))
   else
    reSummary.Lines.Add(SDUParamSubstitute(_('Filename: %1'), [VolFilename]));


  if GetIsHidden() then
    reSummary.Lines.Add(SDUParamSubstitute(_('Hidden volume starting at offset: %1'), [GetOffset()]));

  if GetCDBInVolFile() then begin
    if (PaddingLength > ULLZero) then begin
      totalSize := volSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8) + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) + %3 (padding) = %4 bytes'),
        [SDUIntToStr(volSize), (CRITICAL_DATA_LENGTH div 8),
        SDUIntToStr(PaddingLength), SDUIntToStr(totalSize)]));
    end else begin
      totalSize := volSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (for CDB) = %3 bytes'),
        [SDUIntToStr(volSize), (CRITICAL_DATA_LENGTH div 8), SDUIntToStr(
        totalSize)]));
    end;

    reSummary.Lines.Add(_('CDB stored: At start of volume file'));
  end else begin
    if (PaddingLength > ULLZero) then begin
      totalSize := volSize + PaddingLength;
      reSummary.Lines.Add(SDUParamSubstitute(
        _('Volume size: %1 + %2 (padding) = %3 bytes'),
        [SDUIntToStr(volSize), SDUIntToStr(PaddingLength), SDUIntToStr(
        totalSize)]));
    end else begin
      reSummary.Lines.Add(SDUParamSubstitute(_('Volume size: %1 bytes'), [SDUIntToStr(volSize)]));
    end;

    reSummary.Lines.Add(_('CDB stored: In separate keyfile'));
    reSummary.Lines.Add(SDUParamSubstitute(_('CDB keyfile: %1'), [GetCDBFilename()]));
  end;

  reSummary.Lines.Add(SDUParamSubstitute(_('Hash algorithm: %1'),
    [cbHash.Items[cbHash.ItemIndex]]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Hash driver: %1]'), [GetHashDriver()]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Hash GUID: %1]'),
    [GUIDToString(GetHashGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Key iterations: %1'), [seKeyIterations.Value]));


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
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Cypher driver: %1]'), [GetCypherDriver()]));
  reSummary.Lines.Add('  ' + SDUParamSubstitute(_('[Cypher GUID: %1]'),
    [GUIDToString(GetCypherGUID())]));

  reSummary.Lines.Add(SDUParamSubstitute(_('Master key length: %1 bits'), [GetMasterKeyLength()]));

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
  reSummary.Lines.Add(SDUParamSubstitute(_('Salt length: %1 bits'), [seSaltLength.Value]));

  driveLetter := GetDriveLetter();
  if (driveLetter = #0) then begin
    driveLetter := _('Use default');
  end else begin
    driveLetter := driveLetter + ':';
  end;
  reSummary.Lines.Add(SDUParamSubstitute(_('Requested drive letter: %1'), [driveLetter]));

end;

function TfrmWizardCreateVolume.GetRNGSet(): TRNGSet;
begin
  Result := [];

  if ckRNGCryptoAPI.Checked then begin
    Result := Result + [rngCryptoAPI];
  end;

  if ckRNGMouseMovement.Checked then begin
    Result := Result + [rngMouseMovement];
  end;

  if ckRNGcryptlib.Checked then begin
    Result := Result + [rngcryptlib];
  end;

  if ckRNGPKCS11.Checked then begin
    Result := Result + [rngPKCS11];
  end;

  if ckRNGGPG.Checked then begin
    Result := Result + [rngGPG];
  end;
end;

procedure TfrmWizardCreateVolume.pbFinishClick(Sender: TObject);
begin
  inherited;

  if GetIsPartition() then begin
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
  GetRandPool.SetUpRandPool(GetRNGSet(),
    fFreeOTFEObj.PKCS11Library, PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption);

//  allOK := GetRandPool.GetRandomData( (RNG_requiredBits() div 8), fCombinedRandomData);


  // *All* information required to create the new volume acquired - so create
  // the new volume
try

    if CreateNewVolume() then begin
      PostCreate();
      ModalResult := mrOk;
    end;
    except
on E:EInsufficientRandom do
  SDUMessageDlg(
        _('Insufficient random data generated: '+E.message) + SDUCRLF + SDUCRLF +
        PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
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
  randomPool:        Ansistring;
  volumeFileSize:    ULONGLONG;
  cdbFile:           String;
  cdbOffset:         ULONGLONG;
  userCancel:        Boolean;
  junkBool:          Boolean;
  fileCreateProbMsg: String;
  VolFilename :TFilename;
  randBytes:TSDUBytes;
   saltStr: ansistring;
begin
  result := True;
  VolFilename:= GetVolFilename();
  // Create the volume file, if not a partition
  if result then begin
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

        result := SDUCreateLargeFile(VolFilename, volumeFileSize, True, userCancel);
        if not result then
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

    if result then begin
      // Overwrite volume with SPRNG data, if required
      // this also overwrites cdb and padding, cdb is created below
        { if hidden also overwrite  from Offset? - this is arguably overkill bc should already have been overwritten,
          but perhaps if didnt do on outer vol then will stop attacker knowing size of data
          also less info leaked if attacker has b4/after snapshots
          for now not overwriting for performance
         }
      { TODO 1 -otdk -cenhance : instead default FOverwriteWithChaff to false for hidden vols so user can still change}
      if GetOffset() = 0 then  result := OverwriteVolWithChaff();
    end;
  end;  // if (allOK) then

  // Create separate CDB file, if needed
  if result then begin
    if not GetCDBInVolFile() then begin
      result := SDUCreateLargeFile(GetCDBFilename(), (CRITICAL_DATA_LENGTH div 8), False, junkBool);
      if not result then begin
        SDUMessageDlg(_('Unable to create separate CDB keyfile.'), mtError);
      end;
    end;
  end;


  if result then begin
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Populating critical data structure...');
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
fFreeOTFEObj.DebugMsg('Using hash driver: '+CDBMetaData.HashDriver);
fFreeOTFEObj.DebugMsg('Using hash: '+GUIDToString(CDBMetaData.HashGUID));
fFreeOTFEObj.DebugMsg('Using cypher driver: '+CDBMetaData.CypherDriver);
fFreeOTFEObj.DebugMsg('Using cypher: '+GUIDToString(CDBMetaData.CypherGUID));
{$ENDIF}


    volumeDetails.CDBFormatID  := CDB_FORMAT_ID;
    volumeDetails.PartitionLen := GetSize();

    volumeDetails.VolumeFlags := 0;
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Volume flags: '+inttostr(volumeDetails.VolumeFlags)+' bits');
{$ENDIF}

    volumeDetails.SectorIVGenMethod := GetSectorIVGenMethod();

    { TODO 2 -otdk -csecurity : this copies data - but orig data not deleted so may be reused }
//    randomPool := GetRandomData_CDB();

    volumeDetails.MasterKeyLength := GetMasterKeyLength();
    // Grab 'n' bytes from the random pool to use as the master key
    GetRandPool().GetRandomData(volumeDetails.MasterKeyLength div 8,randBytes) ;
  volumeDetails.MasterKey       := SDUBytesToString( randBytes);
     assert(length(volumeDetails.MasterKey) = volumeDetails.MasterKeyLength div 8);
//    volumeDetails.MasterKey       := Copy(randomPool, 1, (volumeDetails.MasterKeyLength div 8));

    // SDUDeleteFromStart(randomPool, volumeDetails.MasterKeyLength div 8);
//    Delete(randomPool, 1, (volumeDetails.MasterKeyLength div 8));
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Master key length: '+inttostr(volumeDetails.MasterKeyLength)+' bits');
fFreeOTFEObj.DebugMsg('Master key follows:');
fFreeOTFEObj.DebugMsgBinary(volumeDetails.MasterKey);
{$ENDIF}

    // Grab 'n' bytes from the random pool to use as the volume IV
    // *Only* if the cypher's blocksize is a fixed, +ve number of bits
    volumeDetails.VolumeIVLength := 0;
    if (SelectedCypherBlockSize() > 0) then
      if GetUsePerVolumeIV() then
        volumeDetails.VolumeIVLength := SelectedCypherBlockSize();

    GetRandPool().GetRandomData(volumeDetails.VolumeIVLength div 8,randBytes) ;
   volumeDetails.VolumeIV       := SDUBytesToString( randBytes);
   assert(length(volumeDetails.VolumeIV) = volumeDetails.VolumeIVLength div 8);
//    volumeDetails.VolumeIV := Copy(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
    // SDUDeleteFromStart(randomPool,volumeDetails.VolumeIVLength div 8 );

//    Delete(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('Volume IV length: '+inttostr(volumeDetails.VolumeIVLength)+' bits');
fFreeOTFEObj.DebugMsg('Volume IV follows:');
fFreeOTFEObj.DebugMsgBinary(volumeDetails.VolumeIV);
{$ENDIF}


    volumeDetails.RequestedDriveLetter := GetDriveLetter();

    // Grab 'n' bytes from the random pool to use as the salt
    GetRandPool().GetRandomData(seSaltLength.Value div 8,saltBytes) ;
    saltStr := SDUBytesToString(saltBytes);
    assert(length(saltStr) = seSaltLength.Value div 8);
//    saltBytes := Copy(randomPool, 1, (seSaltLength.Value div 8));
    // SDUDeleteFromStart(randomPool, (seSaltLength.Value div 8));
    { DONE 1 -otdk -crefactor : have randpool object }
//    Delete(randomPool, 1, (seSaltLength.Value div 8));

{$IFDEF FREEOTFE_DEBUG}
fFreeOTFEObj.DebugMsg('About to write the critical data...');
fFreeOTFEObj.DebugMsg('Using password: '+preUserKey1.Text);
fFreeOTFEObj.DebugMsg('Using salt... ');
fFreeOTFEObj.DebugMsg('-- begin salt --');
fFreeOTFEObj.DebugMsgBinary(saltStr);
fFreeOTFEObj.DebugMsg('-- end salt --');
{$ENDIF}

    // Determine filename and offset within file storing CDB
    GetCDBFileAndOffset(cdbFile, cdbOffset);

    // Write the header to the file, starting from the specified offset
    if not (fFreeOTFEObj.WriteVolumeCriticalData(cdbFile, cdbOffset,
      GetPassword(), saltBytes, seKeyIterations.Value, volumeDetails, CDBMetaData{, randomPool})) then
    begin
      SDUMessageDlg(
        _('Unable to write critical data block.'),
        mtError
        );
      result := False;
    end;  // if not(OTFEFreeOTFE.WriteVolumeCriticalData(

  end;  // if result then
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
  VolFilename   :tfilename;
begin
  if GetAutoMountAfterCreate() then begin
  VolFilename:= GetVolFilename();
    tmpVolumeFiles := TStringList.Create();
    try
      mountedOK := False;

      tmpVolumeFiles.Add(VolFilename);
      GetCDBFileAndOffset(cdbFile, cdbOffset);

      if (VolFilename = cdbFile) then begin
        cdbFile := '';
      end;

      MountedDrives := '';
      if fFreeOTFEObj.MountFreeOTFE(tmpVolumeFiles, GetPassword(), cdbFile,
        '',  // Empty string - read the CDB
        PKCS11_NO_SLOT_ID, nil,
              // PKCS#11 session not used
        nil,  // PKCS#11 secret key not used
        seKeyIterations.Value, GetDriveLetter(), False, // Readonly
        DEFAULT_MOUNTAS, GetOffset(), GetCDBInVolFile(), seSaltLength.Value, True,
        // Mount for all users
        MountedDrives) then begin
        fFreeOTFEObj.CountMountedResults(
          MountedDrives,
          cntMountOK,
          cntMountFailed
          );

        if (cntMountOK = 1) then begin
          fnewVolumeMountedAs := MountedDrives[1];
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
    PartitionImage.MountedAs   := fnewVolumeMountedAs;
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
  if not (fFreeOTFEObj.GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(),
    cypherDetails)) then begin
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
  if not (fFreeOTFEObj.GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(),
    cypherDetails)) then begin
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
  if not (fFreeOTFEObj.GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(),
    cypherDetails)) then begin
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
  minSize := (MIN_PDA_VOLUME_SIZE * BYTES_IN_MEGABYTE);

  // Warn user if volume size smaller than PDA version min
  if (GetSize() < minSize) then begin
    SDUMessageDlg(
      SDUParamSubstitute(_(
      'Please note: If you would like your DoxBox to be compatible with the PDA version of FreeOTFE, please specify a box size greater than %1 MB'),
      [MIN_PDA_VOLUME_SIZE]),
      mtWarning
      );
  end;

  UpdateUIAfterChangeOnCurrentTab();
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

procedure TfrmWizardCreateVolume.SetupInstructions();
begin
  inherited;
  { TODO 1 -otdk -crefactor : use design time contents of labels as master text }

  SDUTranslateComp(lblInstructFileOrPartition);

  reInstructFilename.Text :=
    _('Please give the filename of the new DoxBox, by clicking the browse button.' +
    SDUCRLF + SDUCRLF +
    'If you wish to create a hidden DoxBox, please choose the existing DoxBox file to be used.');

  reInstructPartitionWarning.Text :=
    _('1) Creating encrypted partitions is POTENTIALLY DESTRUCTIVE.' + SDUCRLF +
    SDUCRLF + '2) Data stored on the partition you select in the next stage may be OVERWRITTEN with encrypted data.' + SDUCRLF + SDUCRLF +
     '3) It is RECOMMENDED that you backup your data as appropriate before continuing.');

  reInstructPartitionSelect.Text :=
    _('Please select the partition you wish to create the new DoxBox on, and whether you would like to create a "hidden" DoxBox.');

  reInstructOffset.Text :=
    _('The DoxBox file you specified already exists. To create a hidden DoxBox within this file, please specify the byte offset within this file from where the new DoxBox should begin.' + SDUCRLF + SDUCRLF + 'If you do NOT wish to create a hidden DoxBox within your existing file, please click "< Back" and enter a different filename.');

  reInstructWarningOffset.Text :=
    _('1) Creating hidden Boxes is POTENTIALLY DESTRUCTIVE.' + SDUCRLF +    SDUCRLF +
    '2) Data within the file/partition specified in the previous stage may be OVERWRITTEN, starting from byte offset specified,'+
    ' and running for the full length of the new hidden DoxBox.' + SDUCRLF + SDUCRLF +
    '3) If you ae not using the default offset, remember the offset entered! If in doubt use the ''Default Offset'' value. '+
    'For security reasons, this information is not stored anywhere; you will need to type in this offset whenever  you wish to mount your hidden DoxBox.');

          { TODO 1 -otdk -ceasify : why mutiple of 512? simplify text }
  reInstructSize.Text :=
    _('Please enter the required size of the new DoxBox.' + SDUCRLF + SDUCRLF +
    'This value must be more than 1 MB, and must be a multiple of 512 bytes.' +
    SDUCRLF + SDUCRLF +
    'If you are creating a hidden DoxBox, then this is the size of the hidden Box. Please see the help for minium sizes of hidden boxes' + SDUCRLF + SDUCRLF +
     'Note: The size you give here will be the amount available for your data. The actual file created will be slightly larger than this.');

{ was     'If you are creating a hidden DoxBox, then this is the size of the hidden Box. Please see the help for minium sizes of hidden boxes size of the original DoxBox file, less the byte offset entered in the'+
    ' previous stage, must be greater than the sum of the value entered here, and the size critical data area.' + SDUCRLF + SDUCRLF +
 }


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
    SDUCRLF + '1) Password salting' + SDUCRLF +
    '2) The new DoxBox''s master key' + SDUCRLF +
     '3) To seed pseudorandom ''chaff'' data' + SDUCRLF + SDUCRLF +
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
         SDUInitAndZeroBuffer(0,ftempCypherKey);

//  ftempCypherKey        := '';
assert(SDUBytesToString(ftempCypherKey) = '');
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
  tempCipherkeyStr  :             Ansistring;
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
  tempCipherkeyStr := SDUBytesToString(  ftempCypherKey);
  if not (fFreeOTFEObj.EncryptSectorData(GetCypherDriver(), GetCypherGUID(), sectorID,
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
  // also same cypher used for chaff
  if GetOverwriteWithChaff() then Result := Result + ftempCypherUseKeyLength;
end;

function TfrmWizardCreateVolume.GetDriveLetter(): Ansichar;
begin
  Result := #0;
  if (cbDriveLetter.ItemIndex > 0) then
    Result := Ansichar(cbDriveLetter.Items[cbDriveLetter.ItemIndex][1]);
end;

procedure TfrmWizardCreateVolume.SetDriveLetter(driveLetter: Ansichar);
begin
  cbDriveLetter.ItemIndex := 0;
  if (driveLetter <> #0) then begin
    cbDriveLetter.ItemIndex := cbDriveLetter.Items.IndexOf(driveLetter + ':');
  end;
end;

function TfrmWizardCreateVolume.GetCDBFilename(): String;
begin
  Result := '';
  if rbCDBInKeyfile.Checked then Result := trim(lblKeyFilename.Caption);
end;

procedure TfrmWizardCreateVolume.SetCDBFilename(CDBFilename: String);
begin
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := CDBFilename;
  if (CDBFilename <> '') then     rbCDBInKeyfile.Checked := True;

  EnableDisableControls();
end;

procedure TfrmWizardCreateVolume.pbBrowseKeyfileClick(Sender: TObject);
begin
  keySaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  keySaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  keySaveDialog.Options    := keySaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(keySaveDialog, GetCDBFilename());
  if keySaveDialog.Execute then   SetCDBFilename(keySaveDialog.Filename);


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
  Result := rgOverwriteType.ItemIndex = 0;// rbDataEncrypted.Checked;
end;

procedure TfrmWizardCreateVolume.SetOverwriteWithChaff(secChaff: Boolean);
begin
  rgOverwriteType.ItemIndex  := Ifthen(secChaff,1,0);
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
