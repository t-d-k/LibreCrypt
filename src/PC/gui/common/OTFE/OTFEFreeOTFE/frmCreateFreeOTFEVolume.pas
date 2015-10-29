unit frmCreateFreeOTFEVolume;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

{ DONE 1 -otdk -ccleanup : rename unit }
interface

uses
  //delphi & libs
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  //sdu & LibreCrypt utils
  MouseRNG, Shredder,
  lcTypes, SDURandPool, SDUStdCtrls, SDUForms, SDUFrames, SDUSpin64Units,
  PasswordRichEdit, Spin64, SDUDialogs, lcDialogs, OTFEFreeOTFE_InstructionRichEdit,
  lcDebugLog, OTFEFreeOTFEBase_U,
  VolumeFileAPI,  // Required for TVOLUME_CRITICAL_DATA
  DriverAPI,      // Required for CRITICAL_DATA_LEN
  OTFEFreeOTFE_PasswordRichEdit,
  // LibreCrypt forms
  fmeSelectPartition,
  frmWizard,
  fmeNewPassword, fmeContainerSize;

{ TODO 1 -otdk -ccleanup : use sdufilename edit for container }
type
  TfrmCreateFreeOTFEVolume = class (TfrmWizard)
    tsFilename:      TTabSheet;
    tsHashCypherIV:  TTabSheet;
    Label4:          TLabel;
    Label5:          TLabel;
    cbHash:          TComboBox;
    cbCypher:        TComboBox;
    tsRNGSelect:     TTabSheet;
    tsSize:          TTabSheet;
    tsOffset:        TTabSheet;
    pnlWarningOffset: TPanel;
    lblWarningOffset: TLabel;
    tsPassword:      TTabSheet;
    lblInstructFilenameNotHidden: TLabel;
    reInstructOffset: TLabel;
    reInstructHashCypherIV: TLabel;
    lblInstructRNGSelect1: TLabel;
    Label13:         TLabel;
    GroupBox1:       TGroupBox;
    pbBrowseFilename: TButton;
    Label7:          TLabel;
    SaveDialog:      TSDUSaveDialog;
    tsRNGMouseMovement: TTabSheet;
    lblInstructRNGMouseMovement1: TLabel;
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
    lblInstructMasterKeyLen: TLabel;
    Label2:          TLabel;
    seMasterKeyLength: TSpinEdit64;
    Label9:          TLabel;
    lblMouseRNGBits: TLabel;
    pbHashInfo:      TButton;
    pbCypherInfo:    TButton;
    tsFileOrPartition: TTabSheet;
    tsPartitionSelect: TTabSheet;
    lblInstructPartitionSelectNotHidden: TLabel;
    rgFileOrPartition: TRadioGroup;
    Label21:         TLabel;
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
    fmeSelectPartition: TfmeSelectPartition;
    reInstructSummary: TLabel;
    se64UnitByteOffset: TSDUSpin64Unit_Storage;
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
    edtFilename:     TEdit;
    Label15:         TLabel;
    lblInstructPartitionWarning1: TLabel;
    lblWarningPartition: TLabel;
    rgOverwriteType: TRadioGroup;
    se64Padding:     TSpinEdit64;
    lblFinished:     TLabel;
    lblWelcomeBanner: TLabel;
    lblWelcomeClickNext: TLabel;
    lblInstructSalt1: TLabel;
    lblInstructFileNotHidden1: TLabel;
    lblInstructDriveLetter2: TLabel;
    lblInstructRNGSelect2: TLabel;
    lblInstructPartitionWarning2: TLabel;
    lblInstructPartitionWarning3: TLabel;
    reInstructWarningOffset2: TLabel;
    reInstructWarningOffset4: TLabel;
    reInstructWarningOffset3: TLabel;
    lblInstructChaff4: TLabel;
    lblInstructChaff3: TLabel;
    lblInstructChaff2: TLabel;
    reInstructRNGSelect4: TLabel;
    reInstructRNGSelect3: TLabel;
    reInstructRNGSelect5: TLabel;
    lblInstructRNGMouseMovement2: TLabel;
    reInstructKeyIterations3: TLabel;
    reInstructKeyIterations2: TLabel;
    lblInstructSalt4: TLabel;
    lblInstructSalt3: TLabel;
    lblInstructSalt2: TLabel;
    lblInstructFileOrPartition3: TLabel;
    lblInstructFileHidden2: TLabel;
    lblInstructFilenameHidden: TLabel;
    frmeNewPassword: TfrmeNewPassword;
    fmeContainerSize1: TTfmeContainerSize;
    lblInstructFileHidden1: TLabel;
    lblInstructFileNotHidden2: TLabel;
    lblInstructPartitionSelectHidden: TLabel;
    lblFileExistsWarning: TLabel;
    cbOverwrite: TCheckBox;
    procedure pbBrowseFilenameClick(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
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

    procedure rgFileOrPartitionClick(Sender: TObject);
    procedure seSizeExit(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure ckSizeEntirePartitionDiskClick(Sender: TObject);
    procedure seSizeChange(Sender: TObject);
    procedure se64ByteOffsetChange(Sender: TObject);
    procedure rbCDBLocationClick2(Sender: TObject);
    procedure pbBrowseKeyfileClick(Sender: TObject);
    procedure cbDriveLetterChange(Sender: TObject);
    procedure edtFilenameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  published
    procedure fmeSelectPartitionChanged(Sender: TObject);
  private
    // Advanced options...
    //    fsaltLength:           Integer;  // Length in *bits*
    //    frequestedDriveLetter: ansichar;
    //    fCDBFilename:          String;
    //    fpaddingLength:        ULONGLONG;
    //    foverwriteWithChaff:   Boolean;

    fnewVolumeMountedAs: DriveLetterChar;

    // These are ordered lists corresponding to the items shown in the combobox
    fHashKernelModeDriverNames: TStringList;
    fHashGUIDs:      TStringList;
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDs:    TStringList;
    fIVGenMethodIDs: TStringList;

    fDeviceList:  TStringList;
    fDeviceTitle: TStringList;

    // Used for overwriting
    fchaffCypherDetails:      TFreeOTFECypher_v3;
    fchaffCypherUseKeyLength: Integer;  // In *bits*


    //    fcanUseCryptlib:        Boolean;
    fPKCS11TokensAvailable: Boolean;

    // When the user selects a cypher, we cache it's keysize for later use.
    // This is set to CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE to indicate no keysize is cached
    fcachedCypherKeysize: Integer;
    // When the user selects a cypher, we cache it's blocksize for later use.
    // This is set to CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE to indicate no blocksize is cached
    fcachedCypherBlocksize: Integer;
    // When the user selects a cypher, we cache it's mode for later use.
    fcachedCypherMode: TFreeOTFECypherMode;
    fis_hidden: Boolean; // are we ceating hidden volume

    // Returns the keysize for the user's selected cypher, as returned by the
    // driver
    function _selectedCypherKeySize(): Integer;
    // Returns the blocksize for the user's selected cypher, as returned by the
    // driver
    function _selectedCypherBlockSize(): Integer;
    // Returns the cypher mode for the user's selected cypher, as returned by
    // the driver
    function _selectedCypherMode(): TFreeOTFECypherMode;

    function getRNGSet(): TRNGSet;

    function getIsPartition(): Boolean;
    function getVolFilename(): String;

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
    function getSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
    procedure SetSectorIVGenMethod(sectorIVGenMethod: TFreeOTFESectorIVGenMethod);
    function getUsePerVolumeIV(): Boolean;
    function getMasterKeyLength(): Integer;  // Returns length in *bits*
    // function GetRandomData_CDB(): Ansistring;
    function getRandomData_ChaffKey(): TSDUBytes;



    procedure _PopulateHashes();
    procedure _PopulateCyphers();
    procedure _PopulateSectorIVGenMethods();
    procedure _PopulatePKCS11Tokens();

    procedure _PopulateSummary();



    // overwrite mounted drive if 'drive' set
    function _OverwriteVolWithChaff(drive: DriveLetterChar = #0): Boolean;


    function _CreateNewVolume(): Boolean;
    function _PostCreate(out errMsg: String): Boolean;

    procedure _UpdateChaffCypherKeyLength;

    function getDriveLetter: Char;
    procedure SetDriveLetter(driveLetter: Char);
    function getCDBFilename: String;
    procedure SetCDBFilename(CDBFilename: String);
    function getPaddingLength: Int64;
    procedure SetPaddingLength(len: Int64);
    function getOverwriteWithChaff: Boolean;
    procedure SetOverwriteWithChaff(secChaff: Boolean);
    //    function getIsHidden(): Boolean;
    function getCDBInVolFile(): Boolean;

    //    function getAutoMountAfterCreate(): Boolean;
    procedure getCDBFileAndOffset(out cdbFile: String; out cdbOffset: ULONGLONG);


  protected
    fsilent:       Boolean;
    fsilentResult: TModalResult;


    procedure _EnableDisableControls(); override;

    function _IsTabComplete(checkTab: TTabSheet): Boolean; override;

    function _IsTabSkipped(tabSheet: TTabSheet): Boolean; override;
    function _IsTabRequired(tabSheet: TTabSheet): Boolean; override;

    function _RNG_requiredBits(): Integer;

    procedure _FormWizardStepChanged(Sender: TObject);

  public

    property IsHidden: Boolean Read fis_hidden Write fis_hidden;
  end;

function CreateFreeOTFEVolume(isHidden: Boolean): TMountResult;

implementation

{$R *.DFM}

uses
            // delphi
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for StringToGUID
  Math,

  //sdu
  SDUi18n, SDPartitionImage,
  SDPartitionImage_File,
  SDFilesystem,
  sdusysutils,  // format_drive
  SDFilesystem_FAT,
  pkcs11_library,
  lcConsts,
  sduGeneral,
  OTFEConsts_U, // Required for OTFE_ERR_USER_CANCEL
  OTFEFreeOTFEDLL_U,
  PKCS11Lib,
  PartitionImageDLL,
  lcCommandLine,
  PartitionTools,//for IsPartitionPath
                 //LibreCrypt forms

  frmKeyEntryFreeOTFE // for MountFreeOTFE
  , frmCypherInfo, frmHashInfo;

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

  MIN_REC_VOLUME_SIZE = 4;

  FILEORPART_OPT_PARTITION_INDEX   = 1; //index of 'partition' item in rgFileOrPartition
  FILEORPART_OPT_VOLUME_FILE_INDEX = 0; // index of 'file' item



procedure TfrmCreateFreeOTFEVolume.FormShow(Sender: TObject);
var
  i: Integer;
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
  SDUTranslateComp(lblInstructPartitionSelectNotHidden);
  SDUTranslateComp(lblInstructPartitionSelectHidden);

  SDUTranslateComp(lblInstructPartitionWarning1);
  SDUTranslateComp(lblInstructPartitionWarning2);
  SDUTranslateComp(lblInstructPartitionWarning3);
  SDUTranslateComp(lblInstructFileNotHidden1);
  SDUTranslateComp(lblInstructFileNotHidden2);
  SDUTranslateComp(lblInstructFileHidden1);
  SDUTranslateComp(lblInstructFileHidden2);


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
  fmeContainerSize1.IsHidden := fis_hidden;
  fmeContainerSize1.Initialise();

  { TODO 1 -otdk -ceasify : why mutiple of 512? simplify text }
  SDUTranslateComp(reInstructHashCypherIV);
  SDUTranslateComp(lblInstructMasterKeyLen);
  SDUTranslateComp(lblInstructRNGSelect1);
  SDUTranslateComp(lblInstructRNGSelect2);
  SDUTranslateComp(reInstructRNGSelect3);
  SDUTranslateComp(reInstructRNGSelect4);
  SDUTranslateComp(reInstructRNGSelect5);
  SDUTranslateComp(lblInstructRNGMouseMovement1);
  SDUTranslateComp(lblInstructRNGMouseMovement2);
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
  fchaffCypherUseKeyLength := 0;

  fnewVolumeMountedAs := #0;

  pnlWarningOffset.Caption := '';

  // Create a border around the warning
  // We can't just place a bigger panel under the warning panel, as on a
  // Windows system with themes, the top panel turns red, and the (red) text
  // can't be read.
  { DONE 1 -otdk -crefactor : clean this up, just use background, - so can use align property }
  { TODO 1 -otdk -ccomplete : check text doesnt dissappear with themes -if so just reset colour }

  // tsFileOrPartition
  rgFileOrPartition.ItemIndex := FILEORPART_OPT_VOLUME_FILE_INDEX;

  // tsFilename

  if GetCmdLine.VolumeArg <> '' then
    if IsPartitionPath(GetCmdLine.VolumeArg) then begin

      fmeSelectPartition.Initialize();
      fmeSelectPartition.Tag := 0;

      fmeSelectPartition.SelectedDevice := GetCmdLine.VolumeArg;
      rgFileOrPartition.ItemIndex       := FILEORPART_OPT_PARTITION_INDEX;
      // if no size set - use entre partition
      fmeContainerSize1.SetIsSizeEntirePartitionDisk(True);

    end else begin
      edtFilename.Text := GetCmdLine.VolumeArg;
      //todo: set size
    end;


  frmeNewPassword.SetKeyPhrase(GetCmdLine.PasswordArg);

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  fmeSelectPartition.AllowCDROM := False;
  fmeSelectPartition.OnChange   := fmeSelectPartitionChanged;
  //  fmeSelectPartition.Initialize();
  fmeSelectPartition.Tag        := 1;


  // tsOffset
  se64UnitByteOffset.Value := 0;

  // tsSize
  // Default to 25 MB
  //  se64UnitSize.Value := DEFAULT_VOLUME_SIZE;

  // tsHashCypherIV
  _PopulateHashes();
  _PopulateCyphers();
  _PopulateSectorIVGenMethods();
  // Autoselect default, if available
  if (cbHash.Items.Count > 0) then begin
    i := cbHash.Items.IndexOf(DEFAULT_HASH_TITLE);
    if (i = -1) then begin
      // If we can't find the default, force the user to select manually
      i := -1;
    end;
    cbHash.ItemIndex := i;
  end;
  // Autoselect default, if available
  if (cbCypher.Items.Count > 0) then begin
    i := cbCypher.Items.IndexOf(DEFAULT_CYPHER_TITLE);
    if (i = -1) then begin
      // If we can't find the default, force the user to select manually
      i := -1;
    end;
    cbCypher.ItemIndex := i;
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
  ckRNGPKCS11.Enabled := PKCS11LibraryReady(GPKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  _PopulatePKCS11Tokens();

  // tsRNGGPG
  lblGPGFilename.Caption := '';



  // BECAUSE WE DEFAULTED TO USING A VOLUME *FILE*, WE MUST SET THE PARTITION
  // SELECT PAGE TO "DONE" HERE (otherwise, it'll never be flagged as complete
  // if the user doesn't switch)
  tsPartitionWarning.Tag := 1;
  tsPartitionSelect.Tag  := 1;
  tsOffset.Tag           := 1;   // only reset if new file chosen (hidden)

  // BECAUSE WE DEFAULTED TO USING THE MOUSERNG, WE MUST SET THE GPG PAGE TO
  // "DONE" HERE (otherwise, it'll never be flagged as complete if the user
  // doens't switch RNGs)
  //  tsRNGGPG.Tag := 1; set to not req'd


  ckAutoMountAfterCreate.Checked := True;
  // Note: ckAutoMountAfterCreate.Visible set in EnableDisableControls(...);
  //       if it's set to FALSE here, the control still appears for some
  //       reason?!


  lblFinished.Visible := False;//'click finished or next'

  //  tsOffset.Visible := fisHidden;

  lblInstructFileNotHidden1.Visible           := not fis_hidden;
  lblInstructFileNotHidden2.Visible           := not fis_hidden;
  lblInstructPartitionSelectNotHidden.Visible := not fis_hidden;
  lblInstructFileHidden1.Visible              := fis_hidden;
  lblInstructFileHidden2.Visible              := fis_hidden;
  lblInstructPartitionSelectHidden.Visible    := fis_hidden;
  lblInstructFilenameNotHidden.Visible        := not fis_hidden;
  lblInstructFilenameHidden.Visible           := fis_hidden;



  // if hidden then file must exist, else should not exist
  if fis_hidden then
    SaveDialog.Options := SaveDialog.Options + [ofFileMustExist]
  else
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];

  _UpdateUIAfterChangeOnCurrentTab();
//  pcWizard.ActivePage := tsFileOrPartition;

  if fSilent then begin
    pbFinishClick(self); //sets  ModalResult
    //      ModalResult := mrOk;
    //    end else begin
    //      ModalResult := mrCancel;
    //    end;

    FSilentResult := ModalResult;

    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

end;

//all non-skipped tabs that are required to be completed return true
function TfrmCreateFreeOTFEVolume._IsTabRequired(tabSheet: TTabSheet): Boolean;
begin
  Result := not ((tabSheet = tsDriveLetter) or (tabSheet = tsHashCypherIV) or
    (tabSheet = tsChaff) or (tabSheet = tsMasterKeyLength) or (tabSheet = tsRNGSelect) or
    (tabSheet = tsRNGMouseMovement) or (tabSheet = tsRNGGPG) or
    (tabSheet = tsRNGPKCS11) or (tabSheet = tsKeyIterations) or (tabSheet = tsSalt) or
    (tabSheet = tsCDBLocation) or (tabSheet = tsPadding) or (tabSheet = tsSummary));
  // if hidden, tsOffset required
  if tabSheet = tsOffset then
    Result := fis_hidden;

end;

// Returns TRUE if the specified tab should be skipped, otherwise FALSE
function TfrmCreateFreeOTFEVolume._IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited _IsTabSkipped(tabSheet);

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
    Result := not fis_hidden;


  // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
  if (tabSheet = tsMasterKeyLength) then begin
    if ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1)) then begin
      Result := (_selectedCypherKeySize() >= 0);
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


procedure TfrmCreateFreeOTFEVolume._UpdateChaffCypherKeyLength;
begin
  // If overwriting with encrypted data (chaff), find the size of the key required
  fchaffCypherUseKeyLength := 0;
  if GetOverwriteWithChaff() then begin
    GetFreeOTFEBase().GetSpecificCypherDetails(GetCypherDriver(), GetCypherGUID(),
      fchaffCypherDetails);

    // Get *real* random data for encryption key
    if (fchaffCypherDetails.KeySizeRequired < 0) then begin
      // -ve keysize = arbitary keysize supported - just use 512 bits
      fchaffCypherUseKeyLength := 512;
    end else begin
      if (fchaffCypherDetails.KeySizeRequired > 0) then begin
        // +ve keysize = specific keysize - get sufficient bits
        fchaffCypherUseKeyLength := fchaffCypherDetails.KeySizeRequired;
      end;
    end;
  end;
end;

procedure TfrmCreateFreeOTFEVolume._EnableDisableControls();
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
    useSectorIVs := (_selectedCypherBlockSize() > 0) and (_selectedCypherMode() <> focmLRW) and
      (_selectedCypherMode() <> focmXTS);
  end;

  if not (useSectorIVs) then begin
    ckUsePerVolumeIV.Checked := False;
    SetSectorIVGenMethod(foivgNone);
  end;
  SDUEnableControl(cbSectorIVGenMethod, useSectorIVs);
  SDUEnableControl(ckUsePerVolumeIV, useSectorIVs);


  // Volume size related...
  fmeContainerSize1.EnableDisableControls(GetIsPartition(), fis_hidden,
    fmeSelectPartition.SyntheticDriveLayout);


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

procedure TfrmCreateFreeOTFEVolume.pbBrowseFilenameClick(Sender: TObject);
begin
  { TODO 1 -otdk -crefactor : move these properties  into designer }
  SaveDialog.Filter     := FILE_FILTER_FLT_VOLUMES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];


  SDUOpenSaveDialogSetup(SaveDialog, GetVolFilename);
  if SaveDialog.Execute then begin
    edtFilename.Text := SaveDialog.Filename;
    // warning label updated, and  _UpdateUIAfterChangeOnCurrentTab() called in edit change event
  end;

end;

 // This procedure will check to see if all items on the current tabsheet have
 // been successfully completed
function TfrmCreateFreeOTFEVolume._IsTabComplete(checkTab: TTabSheet): Boolean;
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
  Result := not _IsTabRequired(checkTab);

  if (checkTab = tsFileOrPartition) then begin
    // Ensure one option has been selected
    Result := (rgFileOrPartition.ItemIndex >= 0);
  end else
  if (checkTab = tsFilename) then begin
    // If we're creating a volume file, a filename must be specified
    if GetIsPartition() then
      Result := True
    else begin
      // Flag tabsheet complete if a filename has been specified
      Result := (GetVolFilename <> '');
      if Result then begin
        // if hidden file must exist and if not, not
        Result := (fis_hidden = FileExists(GetVolFilename()));
      end;
    end;

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
    if fis_hidden then begin // only req'd for hidden volumes
      // Check that the number entered is less than the max size...
      if GetIsPartition() then begin
        Result := (GetOffset() > 0);
      end else begin
        Result := (GetOffset() < SDUGetFileSize(GetVolFilename())) and (GetOffset() >= 0);
      end;
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
      if {FileExists(GetVolFilename)}fis_hidden then begin
        // If creating a hidden container, ensure that the existing file is large
        // enough to store the hidden container
        Result := FileExists(GetVolFilename) and
          ((SDUGetFileSize(GetVolFilename) - GetOffset()) >= volSizeWithCDBAndPadding);
      end else begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)

      end;
    end;

  end else
  if (checkTab = tsHashCypherIV) then begin
    Result := ((cbHash.ItemIndex > -1) and (cbCypher.ItemIndex > -1));
  end else
  if (checkTab = tsMasterKeyLength) then begin
    // If the user's selected cypher has a keysize >= 0, skip the tsMasterKeyLength tabsheet
    if (_selectedCypherKeySize() >= 0) then begin
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
        [randomBitsGenerated, _RNG_requiredBits()]);

      Result           := (randomBitsGenerated >= _RNG_requiredBits());
      MouseRNG.Enabled := not (Result);
      if MouseRNG.Enabled then
        MouseRNG.Color := clWindow
      else
        MouseRNG.Color := clBtnFace;

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
    aSelectedCypherBlockSize := _selectedCypherBlockSize();
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
        'If you wish to store the container''s CDB in a keyfile, you must specify the file to use'),
        mtError);
      ActiveControl := tsCDBLocation;
      Result        := False;
    end;
  end;

  if (checkTab = tsPadding) and Result then begin
    // Padding can't be less than zero
    int64Zero := 0;
    Result    := (se64Padding.Value >= int64Zero);
    if not Result then begin
      SDUMessageDlg(_('The amount of padding data added to a container must be zero or greater'),
        mtError);
      ActiveControl := tsPadding;
      Result        := False;
    end;
  end;


  if (checkTab = tsChaff) and Result then begin
    // If overwriting with encrypted data (chaff), find the size of the key required , and ensure can get that random data
    fchaffCypherUseKeyLength := 0;
    if GetOverwriteWithChaff() then begin
      _UpdateChaffCypherKeyLength;

      // If MouseRNG is being used, check that enough random data is availabe...
      if not (_IsTabComplete(tsRNGMouseMovement)) then begin
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

procedure TfrmCreateFreeOTFEVolume.ckRNGClick(Sender: TObject);
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


  _UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmCreateFreeOTFEVolume.ckSizeEntirePartitionDiskClick(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.ControlChanged(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

function TfrmCreateFreeOTFEVolume.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = FILEORPART_OPT_PARTITION_INDEX);
end;

 //function TfrmCreateFreeOTFEVolume.GetIsHidden(): Boolean;
 //begin
 //  // If:
 //  //   a) The user wants to create the volume on a partition, and has
 //  //      specified they want to create a hidden partition, or
 //  //   b) The user wants to create the volume within a volume file, but has
 //  //      specified the filename of an existing file
 //  Result := ((GetIsPartition()) and (ckPartitionHidden.Checked)) or
 //    ((not (GetIsPartition())) and (FileExists(GetVolFilename)));
 //end;

function TfrmCreateFreeOTFEVolume.GetVolFilename(): String;
begin
  Result := '';

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := edtFilename.Text;
  end;
end;

procedure TfrmCreateFreeOTFEVolume.edtFilenameChange(Sender: TObject);
begin
  inherited;
  // update ui
  se64UnitByteOffset.Value     := 0;
  lblFileExistsWarning.Visible := FileExists(GetVolFilename) and not fis_hidden;
  if (FileExists(GetVolFilename)) then begin
    // Force the user to reenter their offset, by marking the offset tabsheet
    // as incomplete
    if fis_hidden then
      tsOffset.Tag := 0;
  end else begin
    // New file; critical data automatically stored at the start of the file;
    // zero offset
    //      tsOffset.Tag := 1;
  end;
  _UpdateUIAfterChangeOnCurrentTab();
end;

function TfrmCreateFreeOTFEVolume.GetOffset(): ULONGLONG;
begin
  Result := se64UnitByteOffset.Value;
end;

// Note: The size returned *excludes* the size of the critical data
function TfrmCreateFreeOTFEVolume.GetSize(): ULONGLONG;
var
  critSizeULL: ULONGLONG;
begin
  if fmeContainerSize1.GetIsSizeEntirePartitionDisk() then begin
    Result := fmeSelectPartition.SelectedSize();

    // If CDB forms part of the volume, reduce as appropriate
    if GetCDBInVolFile() then begin
      critSizeULL := (CRITICAL_DATA_LENGTH div 8);
      Result      := Result - critSizeULL;
    end;

  end else begin

    Result := fmeContainerSize1.GetSize();
  end;
end;

function TfrmCreateFreeOTFEVolume.GetHashDriver(): String;
begin
  Result := '';

  if (cbHash.ItemIndex >= 0) then begin
    Result := fHashKernelModeDriverNames[cbHash.ItemIndex];
  end;
end;

function TfrmCreateFreeOTFEVolume.GetHashGUID(): TGUID;
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

function TfrmCreateFreeOTFEVolume.GetCypherDriver(): Ansistring;
begin
  Result := '';

  if (cbCypher.ItemIndex >= 0) then begin
    Result := fCypherKernelModeDriverNames[cbCypher.ItemIndex];
  end;
end;

function TfrmCreateFreeOTFEVolume.GetCypherGUID(): TGUID;
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

function TfrmCreateFreeOTFEVolume.GetSectorIVGenMethod(): TFreeOTFESectorIVGenMethod;
begin
  Result := foivgUnknown;

  if (cbSectorIVGenMethod.ItemIndex >= 0) then begin
    Result := TFreeOTFESectorIVGenMethod(
      cbSectorIVGenMethod.Items.Objects[cbSectorIVGenMethod.ItemIndex]);
  end;
end;

procedure TfrmCreateFreeOTFEVolume.SetSectorIVGenMethod(sectorIVGenMethod:
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

function TfrmCreateFreeOTFEVolume.GetUsePerVolumeIV(): Boolean;
begin
  Result := ckUsePerVolumeIV.Checked;
end;

function TfrmCreateFreeOTFEVolume.getRandomData_ChaffKey(): TSDUBytes;
var
  len: Integer;
begin
  // Use the last FPadWithEncryptedDataKeyLen bits as the CDB random data,
  // after the CDB random data
  //  i   := (CRITICAL_DATA_LENGTH div 8) + 1;
  len := (fchaffCypherUseKeyLength div 8);
  { DONE 2 -otdk -crefactor : use randpool object that asserts if data isnt available - for now check not empty }
  GetRandPool().GetRandomData(len, Result);

end;



function TfrmCreateFreeOTFEVolume.GetMasterKeyLength(): Integer;
begin
  Result := _selectedCypherKeySize();
  if (Result < 0) then
    Result := seMasterKeyLength.Value;
end;


procedure TfrmCreateFreeOTFEVolume.edByteOffsetChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmCreateFreeOTFEVolume.SizeChanged(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmCreateFreeOTFEVolume.pbBrowseGPGClick(Sender: TObject);
begin
  GPGOpenDialog.Filter     := FILE_FILTER_FLT_EXECUTABLES;
  GPGOpenDialog.DefaultExt := FILE_FILTER_DFLT_EXECUTABLES;
  GPGOpenDialog.Options    := GPGOpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(GPGOpenDialog, lblGPGFilename.Caption);
  if GPGOpenDialog.Execute then begin
    lblGPGFilename.Caption := GPGOpenDialog.Filename;
  end;

  _UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmCreateFreeOTFEVolume.MouseRNGByteGenerated(Sender: TObject; random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < _RNG_requiredBits()) then begin
    AddToMouseRNGData(random);
  end;

  _UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmCreateFreeOTFEVolume._PopulateHashes();
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

procedure TfrmCreateFreeOTFEVolume._PopulateCyphers();
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

procedure TfrmCreateFreeOTFEVolume._PopulateSectorIVGenMethods();
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

procedure TfrmCreateFreeOTFEVolume._FormWizardStepChanged(Sender: TObject);
begin
  inherited;

  if (pcWizard.ActivePage = tsSummary) then begin
    _PopulateSummary();
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

procedure TfrmCreateFreeOTFEVolume.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here, note will only close automatically if mr = mrok anyway
  if fsilent then
    ModalResult := FSilentResult;
end;

procedure TfrmCreateFreeOTFEVolume.FormCreate(Sender: TObject);
var
  dl: ansichar;
begin
  fsilent := GetCmdLine.isSilent;

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

  OnWizardStepChanged := _FormWizardStepChanged;

  // tsRequestedDriveLetter
  cbDriveLetter.Items.Clear();
  cbDriveLetter.Items.Add(_('Use default'));
  //  for dl:='C' to 'Z' do
  for dl := 'A' to 'Z' do
    cbDriveLetter.Items.Add(dl + ':');

  // Autoselect the "default" (first) entry
  cbDriveLetter.ItemIndex := 0;

  // tsCDBLocation
  rbCDBInKeyfile.Checked     := False;
  rbCDBInVolFile.Checked     := True;
  lblKeyFilename.Caption     := '';
  frmeNewPassword.OnChange   := ControlChanged;
  fmeContainerSize1.OnChange := ControlChanged;



end;

procedure TfrmCreateFreeOTFEVolume.FormDestroy(Sender: TObject);
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

procedure TfrmCreateFreeOTFEVolume.rbCDBLocationClick2(Sender: TObject);
begin
  _EnableDisableControls();
end;

procedure TfrmCreateFreeOTFEVolume.cbDriveLetterChange(Sender: TObject);
begin
  inherited;
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.cbHashCypherIVGenChange(Sender: TObject);
begin
  // Cypher changed; clear any cached keysize
  fCachedCypherKeysize   := CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE;
  // Cypher changed; clear any cached blocksize
  fCachedCypherBlocksize := CYPHER_BLOCKSIZE_NOT_CACHED_FLAG_BLOCKSIZE;
  // Cypher changed; clear any cached mode
  fCachedCypherMode      := focmUnknown;

  _UpdateUIAfterChangeOnCurrentTab();
end;


procedure TfrmCreateFreeOTFEVolume._PopulateSummary();
var
  totalSize:     ULONGLONG;
  driveLetter:   String;
  RNGs:          String;
  ULLZero:       ULONGLONG;
  PaddingLength: Int64;
  VolFilename:   TFilename;
  volSize:       ULONGLONG;
  cont_size:     String;

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


  if fis_hidden then
    reSummary.Lines.Add(Format(_('Hidden container starting at offset: %d'), [GetOffset()]));

  cont_size := Format(_('Container size: %d '), [volSize]);
  totalSize := volSize;
  if (PaddingLength > ULLZero) then begin
    totalSize := totalSize + PaddingLength;
    cont_size := cont_size + Format(_('+ %d (padding) '), [PaddingLength]);
  end;

  if GetCDBInVolFile() then begin
    cont_size := cont_size + Format(_('+ %d (for header) '), [(CRITICAL_DATA_LENGTH div 8)]);
    totalSize := totalSize + ULONGLONG(CRITICAL_DATA_LENGTH div 8);
    reSummary.Lines.Add(_('Header stored: At start of container file'));
  end else begin
    reSummary.Lines.Add(_('Header stored: In separate keyfile'));
    reSummary.Lines.Add(Format(_('Header keyfile: %s'), [GetCDBFilename()]));
  end;

  if (totalSize = volSize) then begin
    cont_size := cont_size + _('bytes');
  end else begin
    cont_size := cont_size + Format(_('= %d bytes'), [totalSize]);
  end;

  reSummary.Lines.Add(cont_size);

  reSummary.Lines.Add(Format(_('Hash algorithm: %s'), [cbHash.Items[cbHash.ItemIndex]]));
  reSummary.Lines.Add('  ' + Format(_('[Hash driver: %s]'), [GetHashDriver()]));
  reSummary.Lines.Add('  ' + Format(_('[Hash GUID: %s]'), [GUIDToString(GetHashGUID())]));

  reSummary.Lines.Add(Format(_('Key iterations: %d'), [seKeyIterations.Value]));


  if (_selectedCypherBlockSize() <= 0) then begin
    reSummary.Lines.Add(_('Sector IVs will not be used (cypher has zero or variable blocksize)'));
  end else begin
    reSummary.Lines.Add(_('Cypher has fixed, defined blocksize; sector IVs will be used'));
    reSummary.Lines.Add(Format(_('Sector IV generation method: %s'),
      [FreeOTFESectorIVGenMethodTitle[GetSectorIVGenMethod()]]));

    if GetUsePerVolumeIV() then
      reSummary.Lines.Add(_('Sector IVs will be XORd with a per-container IV before use'))
    else
      reSummary.Lines.Add(_('A per-container IV will not be used'));
  end;

  reSummary.Lines.Add(Format(_('Cypher algorithm: %s'), [cbCypher.Items[cbCypher.ItemIndex]]));
  reSummary.Lines.Add('  ' + Format(_('[Cypher driver: %s]'), [GetCypherDriver()]));
  reSummary.Lines.Add('  ' + Format(_('[Cypher GUID: %s]'), [GUIDToString(GetCypherGUID())]));

  reSummary.Lines.Add(Format(_('Master key length: %d bits'), [GetMasterKeyLength()]));

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

function TfrmCreateFreeOTFEVolume.GetRNGSet(): TRNGSet;
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

procedure TfrmCreateFreeOTFEVolume.pbFinishClick(Sender: TObject);
var
  errMsg: String;
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
  _UpdateChaffCypherKeyLength;
  GetRandPool.SetUpRandPool(GetRNGSet(),
    PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption);

  //  Result := GetRandPool.GetRandomData( (RNG_requiredBits() div 8), fCombinedRandomData);


  // *All* information required to create the new volume acquired - so create
  // the new volume
  try

    if _CreateNewVolume() then begin
      if _PostCreate(errMsg) then
        ModalResult := mrOk
      else
        SDUMessageDlg(errMsg + SDUCRLF + SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
          mtError
          );
    end;
  except
    on E: EInsufficientRandom do
      SDUMessageDlg(
        _('Insufficient random data generated: ') + E.message + SDUCRLF +
        SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
        mtError
        );
  end;
end;


function TfrmCreateFreeOTFEVolume._CreateNewVolume(): Boolean;
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

  if not fis_hidden then begin

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
              'Unable to create container; please ensure you have %s free on the relavant drive'),
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
      if not getIsPartition then
        {unfortuanely low level overwrite doesnt yet work for partitions- so partitions are overwritten after mount }
        Result := _OverwriteVolWithChaff();
    end;
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
    DebugMsg('Is Hidden: ' + BoolToStr(fis_hidden));
    DebugMsg('Populating FreeOTFE header ...');

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

    DebugMsg('Using hash driver: ' + CDBMetaData.HashDriver);
    DebugMsg('Using hash: ' + GUIDToString(CDBMetaData.HashGUID));
    DebugMsg('Using cypher driver: ' + CDBMetaData.CypherDriver);
    DebugMsg('Using cypher: ' + GUIDToString(CDBMetaData.CypherGUID));


    volumeDetails.CDBFormatID  := CDB_FORMAT_ID;
    volumeDetails.PartitionLen := GetSize();

    volumeDetails.VolumeFlags := 0;
    DebugMsg('Container flags: ' + IntToStr(volumeDetails.VolumeFlags) + ' bits');

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

    DebugMsg('Master key length: ' + IntToStr(volumeDetails.MasterKeyLength) + ' bits');
    DebugMsg('Master key follows:');
    DebugMsg(volumeDetails.MasterKey);


    // Grab 'n' bytes from the random pool to use as the volume IV
    // *Only* if the cypher's blocksize is a fixed, +ve number of bits
    volumeDetails.VolumeIVLength := 0;
    if (_selectedCypherBlockSize() > 0) then
      if GetUsePerVolumeIV() then
        volumeDetails.VolumeIVLength := _selectedCypherBlockSize();

    GetRandPool().GetRandomData(volumeDetails.VolumeIVLength div 8, volumeDetails.VolumeIV);
    //  volumeDetails.VolumeIV       := SDUBytesToString( randBytes);
    assert(length(volumeDetails.VolumeIV) = volumeDetails.VolumeIVLength div 8);
    //    volumeDetails.VolumeIV := Copy(randomPool, 1, (volumeDetails.VolumeIVLength div 8));
    // SDUDeleteFromStart(randomPool,volumeDetails.VolumeIVLength div 8 );

    //    Delete(randomPool, 1, (volumeDetails.VolumeIVLength div 8));

    DebugMsg('Volume IV length: ' + IntToStr(volumeDetails.VolumeIVLength) + ' bits');
    DebugMsg('Volume IV follows:');
    DebugMsg(volumeDetails.VolumeIV);



    volumeDetails.RequestedDriveLetter := GetDriveLetter();

    // Grab 'n' bytes from the random pool to use as the salt
    GetRandPool().GetRandomData(seSaltLength.Value div 8, saltBytes);
    saltStr := SDUBytesToString(saltBytes);
    assert(length(saltStr) = seSaltLength.Value div 8);
    //    saltBytes := Copy(randomPool, 1, (seSaltLength.Value div 8));
    // SDUDeleteFromStart(randomPool, (seSaltLength.Value div 8));
    { DONE 1 -otdk -crefactor : have randpool object }
    //    Delete(randomPool, 1, (seSaltLength.Value div 8));


    DebugMsg('About to write the FreeOTFE header ...');
    DebugMsg(frmeNewPassword.GetKeyPhrase());
    DebugMsg('Using salt... ');
    DebugMsg('-- begin salt --');
    DebugMsg(saltBytes);
    DebugMsg('-- end salt --');


    // Determine filename and offset within file storing CDB
    GetCDBFileAndOffset(cdbFile, cdbOffset);

    // Write the header to the file, starting from the specified offset
    if not (GetFreeOTFEBase().WriteVolumeCriticalData(cdbFile, cdbOffset,
      frmeNewPassword.GetKeyPhrase(), saltBytes, seKeyIterations.Value,
      volumeDetails, CDBMetaData)) then begin
      SDUMessageDlg(
        _('Unable to write FreeOTFE header.'),
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

procedure TfrmCreateFreeOTFEVolume.GetCDBFileAndOffset(out cdbFile: String;
  out cdbOffset: ULONGLONG);
begin
  cdbFile   := GetVolFilename();
  cdbOffset := GetOffset();
  if not GetCDBInVolFile() then begin
    // CDB stored in separate keyfile
    cdbFile   := GetCDBFilename();
    cdbOffset := 0;

    DebugMsg('Header stored in separate keyfile: ' + cdbFile);
  end;

end;

// Post-creation functionality
function TfrmCreateFreeOTFEVolume._PostCreate(out errMsg: String): Boolean;
var
  MountedDrive: DriveLetterChar;

  cntMountOK:     Integer;
  cntMountFailed: Integer;
  //  tmpVolumeFiles: TStringList;
  cdbFile:        String;
  cdbOffset:      ULONGLONG;
  VolFilename:    tfilename;
  failMsg  : string;

begin
  Result := True;
  { TODO 2 -otdk -csecurity : if not automount - not wiped }
  if ckAutoMountAfterCreate.Checked then begin
    VolFilename := GetVolFilename();

    Result := False;

    GetCDBFileAndOffset(cdbFile, cdbOffset);

    if (VolFilename = cdbFile) then
      cdbFile := '';


    MountedDrive := #0;
    if MountFreeOTFE(VolFilename, frmeNewPassword.GetKeyPhrase(), cdbFile,
      '',  // Empty string - read the CDB
      PKCS11_NO_SLOT_ID, nil,
            // PKCS#11 session not used
      nil,  // PKCS#11 secret key not used
      seKeyIterations.Value, GetDriveLetter(), False, // Readonly
      DEFAULT_MOUNTAS, GetOffset(), GetCDBInVolFile(), seSaltLength.Value, True,
      // Mount for all users
      MountedDrive) = morOK then begin
      GetFreeOTFEBase().CountMountedResults(
        MountedDrive,
        cntMountOK,
        cntMountFailed
        );

      if cntMountOK = 1 then begin
        fnewVolumeMountedAs := MountedDrive;
        Result              := True;
      end;
    end;

    if not Result then
      // Volumes couldn't be mounted for some reason...
      errMsg := 'Can''t open container';
    if Result then begin
      { TODO 2 -otdk -cinvestigate : why only DLL - should always format after creation, even if use windows dlg }
      if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
        Result := PostMount_Format_using_dll(fnewVolumeMountedAs);
      end else begin
        Application.ProcessMessages;
        Result := Format_Drive(FNewVolumeMountedAs);
        if not Result then
          // Volumes couldn't be mounted for some reason...
          errMsg := FORMAT_ERR;
      end;
      if Result and getIsPartition then begin
        _OverwriteVolWithChaff(fnewVolumeMountedAs);
      end;
    end;

  end else begin
    // if partitin not wiped if not mounted
    if getIsPartition() then begin
    failMsg :=
        _('Wiping of data not possible for a partition until it is opened. You should manually wipe the drive to ensure security');
      SDUMessageDlg(failMsg, mtError);
    end;

  end;

end;

procedure TfrmCreateFreeOTFEVolume.seMasterKeyLengthChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.seMasterKeyLengthExit(Sender: TObject);
begin
  // Ensure master key length is a multiple of 8 bits
  if ((seMasterKeyLength.Value mod 8) <> 0) then begin
    SDUMessageDlg(_('The master key length must be a multiple of 8'), mtWarning);
    ActiveControl := seMasterKeyLength;
  end;

  _UpdateUIAfterChangeOnCurrentTab();
end;


 // Returns the keysize for the user's selected cyppher, as returned by the
 // driver
 // Returns CYPHER_KEYSIZE_NOT_CACHED_FLAG_KEYSIZE on error
function TfrmCreateFreeOTFEVolume._selectedCypherKeySize(): Integer;
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
function TfrmCreateFreeOTFEVolume._selectedCypherBlockSize(): Integer;
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
function TfrmCreateFreeOTFEVolume._selectedCypherMode(): TFreeOTFECypherMode;
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


procedure TfrmCreateFreeOTFEVolume.pbHashInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetHashDriver();
  GUID       := GetHashGUID();
  frmHashInfo.ShowHashDetailsDlg(deviceName, GUID);

end;


procedure TfrmCreateFreeOTFEVolume.pbCypherInfoClick(Sender: TObject);
var
  deviceName: String;
  GUID:       TGUID;
begin
  deviceName := GetCypherDriver();
  GUID       := GetCypherGUID();
  frmCypherInfo.ShowCypherDetailsDlg(deviceName, GUID);

end;


procedure TfrmCreateFreeOTFEVolume.rbCDBLocationClick(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();

end;

 //procedure TfrmCreateFreeOTFEVolume.ckPartitionHiddenClick(Sender: TObject);
 //begin
 //  se64UnitByteOffset.Value := 0;
 //  if (ckPartitionHidden.Checked) then begin
 //    // Force the user to reenter their offset, by marking the offset tabsheet
 //    // as incomplete
 //    tsOffset.Tag := 0;
 //  end else begin
 //    // New file; critical data automatically stored at the start of the file;
 //    // zero offset
 //    tsOffset.Tag := 1;
 //  end;
 //
 //  _UpdateUIAfterChangeOnCurrentTab();
 //
 //end;

procedure TfrmCreateFreeOTFEVolume.rgFileOrPartitionClick(Sender: TObject);
begin
  // Size tab must be marked as incomplete if the user switched to partition,
  // just in case the user previously entered a size value that's now too big
  if GetIsPartition() then
    tsSize.Tag := 0;

  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.se64ByteOffsetChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.seSizeChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume.seSizeExit(Sender: TObject);
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

  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateFreeOTFEVolume._PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(GPKCS11Library, cbToken) > 0);
end;

procedure TfrmCreateFreeOTFEVolume.pbRefreshClick(Sender: TObject);
begin
  _PopulatePKCS11Tokens();
end;

procedure TfrmCreateFreeOTFEVolume.fmeSelectPartitionChanged(Sender: TObject);
begin
  se64UnitByteOffset.Value := 0;
  fmeContainerSize1.SetPartitionSize(fmeSelectPartition.SelectedSize());

  // Size tab must be marked as incomplete
  tsSize.Tag := 0;

  _UpdateUIAfterChangeOnCurrentTab();
end;


function TfrmCreateFreeOTFEVolume.GetCDBInVolFile(): Boolean;
begin
  Result := GetCDBFilename() = '';
end;

 //function TfrmCreateFreeOTFEVolume.GetAutoMountAfterCreate(): Boolean;
 //begin
 //  Result := ckAutoMountAfterCreate.Checked;
 //end;


{overwrites volume with 'chaff' from 'Offset' -
uses selected cypher and options as for enxn
}


function TfrmCreateFreeOTFEVolume._OverwriteVolWithChaff(drive: DriveLetterChar = #0): Boolean;
var
  shredder: TShredder;

  overwriteOK:    TShredResult;
  failMsg:        String;
  //  tmpZero: ULONGLONG;
  //  partInfo:TPartitionInformationEx;
  chaffCypherKey: TSDUBytes;
begin
  Result := True;

  // Short-circuit - no padding data
  //  tmpZero := 0;

 { if (PaddingLength <= tmpZero) then
    begin
    Result := TRUE;
    exit;
    end;}

  if cbOverwrite.Checked then begin

  shredder := TShredder.Create();
  try
    shredder.FileDirUseInt := True;
    if GetOverwriteWithChaff() then begin
      // Initilize zeroed IV for encryption
      //    ftempCypherEncBlockNo := 0;

      // Get *real* random data for encryption key
      chaffCypherKey                    := getRandomData_ChaffKey();
      shredder.IntMethod                := smPseudorandom;
      // Note: Setting this event overrides shredder.IntMethod
      shredder.OnTweakEncryptDataEvent  := GetFreeOTFEBase().EncryptSectorData;
      shredder.WipeCypherBlockSize := fchaffCypherDetails.BlockSize;
      shredder.WipeCypherKey            := chaffCypherKey;
      shredder.wipeCypherDriver         := GetCypherDriver();
      shredder.WipeCypherGUID           := GetCypherGUID();
    end else begin
      shredder.IntMethod := smZeros;
    end;

    shredder.IntPasses        := 1;
    shredder.IntSegmentOffset := GetOffset;
    // cdb is written after so can overwrite. if hidden dont overwrite main data
    // will be 0 for non hidden vols

    //      shredder.IntSegmentLength := todo; ignored as quickshred = false


    { done 2 -otdk -ctest : this has not been tested for devices }
    if GetIsPartition() then begin
      //       partInfo := fmeselectpartition.SDUDiskPartitionsPanel1.PartitionInfo[fmeselectpartition.SDUDiskPartitionsPanel1.Selected];
      { TODO 2 -otdk -cfix : this doesnt work for partitions - need mounted drive filename }
      // think need to use WriteRawVolumeData - as otherwise cant get low level access to drive.
      // for now create vol first then shred opened volume (slower)
      //        overwriteOK := shredder.DestroyPart(GetVolFilename(), False, False);
      assert(drive <> #0);
      overwriteOK := shredder.WipeDriveFreeSpace(drive);

    end else begin
      overwriteOK := shredder.DestroyFileOrDir(GetVolFilename(),
                 { TODO 1 -otdk -ccheck : check use of quickshred here }
        False,   // quickShred - do all
        False,   // silent
        True     // leaveFile
        );
    end;


    if (overwriteOK = srError) then begin
      failMsg :=
        _('Wipe of data FAILED. You should manually wipe the drive to ensure security');
      SDUMessageDlg(failMsg, mtError);
      Result := False;
    end else begin
      if (overwriteOK = srUserCancel) then begin
        SDUMessageDlg(_('Wipe of data cancelled. You should manually wipe the drive to ensure security'), mtInformation);
        Result := False;
      end;
    end;


  finally
    shredder.Free();
  end;
  end;

end;//

    //
    //// The array passed in is zero-indexed; populate elements zero to "bytesRequired"
    //procedure TfrmCreateFreeOTFEVolume._GenerateOverwriteData(
    //  Sender: TObject;
    //  passNumber: Integer;
    //  bytesRequired: Cardinal;
    //  var generatedOK: Boolean;
    //  var outputBlock: TShredBlock
    //  );
    //var
    //  i:                Integer;
    //  tempArraySize:    Cardinal;
    //  blocksizeBytes:   Cardinal;
    //  plaintext:        Ansistring;
    //  cyphertext:       Ansistring;
    //  IV:               Ansistring;
    //  localIV:          Int64;
    //  sectorID:         LARGE_INTEGER;
    //  tempCipherkeyStr: Ansistring;
    //begin
    //  // Generate an array of random data containing "bytesRequired" bytes of data,
    //  // plus additional random data to pad out to the nearest multiple of the
    //  // cypher's blocksize bits
    //  // Cater for if the blocksize was -ve or zero
    //  if (fchaffCypherDetails.BlockSize < 1) then begin
    //    blocksizeBytes := 1;
    //  end else begin
    //    blocksizeBytes := (fchaffCypherDetails.BlockSize div 8);
    //  end;
    //  tempArraySize := bytesRequired + (blocksizeBytes - (bytesRequired mod blocksizeBytes));
    //
    //  plaintext := '';
    //  for i := 1 to tempArraySize do begin
    //    plaintext := plaintext + Ansichar(random(256));
//    { DONE 2 -otdk -csecurity : This is not secure PRNG - but is encrypted below, so the result is secure }
 //  end;
 //
 //
 //  Inc(ftempCypherEncBlockNo);
 //
 //  // Adjust the IV so that this block of encrypted pseudorandom data should be
 //  // reasonably unique
 //  IV := '';
 //  if (fchaffCypherDetails.BlockSize > 0) then begin
 //    IV := StringOfChar(AnsiChar(#0), (fchaffCypherDetails.BlockSize div 8));
 //
 //    localIV := ftempCypherEncBlockNo;
 //
 //    for i := 1 to min(sizeof(localIV), length(IV)) do begin
 //      IV[i]   := Ansichar((localIV and $FF));
 //      localIV := localIV shr 8;
 //    end;
 //
 //  end;
 //
 //  // Adjust the sectorID so that this block of encrypted pseudorandom data
 //  // should be reasonably unique
 //  sectorID.QuadPart := ftempCypherEncBlockNo;
 //  // Encrypt the pseudorandom data generated
 //  tempCipherkeyStr := SDUBytesToString(ftempCypherKey);
 //  if not (GetFreeOTFEBase().EncryptSectorData(GetCypherDriver(), GetCypherGUID(),
//    sectorID, FREEOTFE_v1_DUMMY_SECTOR_SIZE, ftempCypherKey, IV, plaintext, cyphertext)) then begin
 //    SDUMessageDlg(
 //      _('Error: unable to encrypt pseudorandom data before using for overwrite buffer') +
 //      SDUCRLF + SDUCRLF + Format(_('Error #: %d'), [GetFreeOTFEBase().LastErrorCode]),
 //      mtError
 //      );
 //
 //    generatedOK := False;
 //  end else begin
 //    // Copy the encrypted data into the outputBlock
 //    for i := 0 to (bytesRequired - 1) do
 //      outputBlock[i] := Byte(cyphertext[i + 1]);
 //
 //    generatedOK := True;
 //  end;
 //end;

function TfrmCreateFreeOTFEVolume._RNG_requiredBits(): Integer;
begin
  Result := CRITICAL_DATA_LENGTH + fchaffCypherUseKeyLength;
  // also same cypher used for chaff
  if GetOverwriteWithChaff() then
    Result := Result + fchaffCypherUseKeyLength;
end;

function TfrmCreateFreeOTFEVolume.GetDriveLetter(): Char;
begin
  Result := #0;
  if (cbDriveLetter.ItemIndex > 0) then
    Result := cbDriveLetter.Items[cbDriveLetter.ItemIndex][1];
end;

procedure TfrmCreateFreeOTFEVolume.SetDriveLetter(driveLetter: Char);
begin
  cbDriveLetter.ItemIndex := 0;
  if (driveLetter <> #0) then begin
    cbDriveLetter.ItemIndex := cbDriveLetter.Items.IndexOf(driveLetter + ':');
  end;
end;

function TfrmCreateFreeOTFEVolume.GetCDBFilename(): String;
begin
  Result := '';
  if rbCDBInKeyfile.Checked then
    Result := trim(lblKeyFilename.Caption);
end;

procedure TfrmCreateFreeOTFEVolume.SetCDBFilename(CDBFilename: String);
begin
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := CDBFilename;
  if (CDBFilename <> '') then
    rbCDBInKeyfile.Checked := True;

  _EnableDisableControls();
end;

procedure TfrmCreateFreeOTFEVolume.pbBrowseKeyfileClick(Sender: TObject);
begin
  keySaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  keySaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  keySaveDialog.Options    := keySaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(keySaveDialog, GetCDBFilename());
  if keySaveDialog.Execute then
    SetCDBFilename(keySaveDialog.Filename);
end;

function TfrmCreateFreeOTFEVolume.GetPaddingLength(): Int64;
begin
  Result := se64Padding.Value;
end;

procedure TfrmCreateFreeOTFEVolume.SetPaddingLength(len: Int64);
begin
  se64Padding.Value := len;
end;

function TfrmCreateFreeOTFEVolume.GetOverwriteWithChaff(): Boolean;
begin
  Result := rgOverwriteType.ItemIndex = 0;// first one is enxd
end;

procedure TfrmCreateFreeOTFEVolume.SetOverwriteWithChaff(secChaff: Boolean);
begin
  rgOverwriteType.ItemIndex := Ifthen(secChaff, 0, 1);
end;

function CreateFreeOTFEVolume(isHidden: Boolean): TMountResult;
var
  frmWizard: TfrmCreateFreeOTFEVolume;
  mr:        Integer;
begin
  Result := morFail;
  GetFreeOTFEBase().LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  GetFreeOTFEBase().CheckActive();

  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    frmWizard := TfrmCreateFreeOTFEVolume.Create(nil);
    try
      frmWizard.isHidden := isHidden; // other cmd line values set in formcreate

      mr := frmWizard.ShowModal();

      if mr = mrOk then begin
        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_SUCCESS;
        Result := morOK;
      end else
      if (mr = mrCancel) then
        //        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
        Result := morCancel;
    finally
      frmWizard.Free();
    end;

  end;

end;


end.
