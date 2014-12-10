unit OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages, MouseRNG,
  PasswordRichEdit, Spin64, StdCtrls, SysUtils, Windows, //sdu
  SDUDialogs
  , sdurandpool, SDUStdCtrls,
                           //doxbox
  OTFEFreeOTFE_DriverAPI,  // Required for CRITICAL_DATA_LEN
  OTFEFreeOTFE_fmeSelectPartition,
  OTFEFreeOTFE_frmWizard, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUBlocksPanel,
  SDUDiskPartitionsPanel, SDUForms, SDUFrames, SDUSpin64Units;

type
  TChangePasswordCreateKeyfile = (opChangePassword, opCreateKeyfile);

  TfrmWizardChangePasswordCreateKeyfile = class (TfrmWizard)
    tsSrcDetails:               TTabSheet;
    tsDestDetails:              TTabSheet;
    reInstructDestDetails:      TOTFEFreeOTFE_InstructionRichEdit;
    reInstructSrcDetails:       TOTFEFreeOTFE_InstructionRichEdit;
    tsRNGMouseMovement:         TTabSheet;
    reInstructRNGMouseMovement: TOTFEFreeOTFE_InstructionRichEdit;
    MouseRNG:                   TMouseRNG;
    lblMouseRNGBits:            TLabel;
    tsDestFile:                 TTabSheet;
    reInstructDestFile:         TOTFEFreeOTFE_InstructionRichEdit;
    tsRNGGPG:                   TTabSheet;
    reInstructRNGGPG:           TOTFEFreeOTFE_InstructionRichEdit;
    GroupBox5:                  TGroupBox;
    lblGPGFilename:             TSDUFilenameLabel;
    pbBrowseGPG:                TButton;
    OpenDialog:                 TSDUOpenDialog;
    SaveDialog:                 TSDUSaveDialog;
    GPGOpenDialog:              TSDUOpenDialog;
    tsRNGSelect:                TTabSheet;
    reInstructRNGSelect:        TOTFEFreeOTFE_InstructionRichEdit;
    Label9:                     TLabel;
    Label1:                     TLabel;
    Label4:                     TLabel;
    Label6:                     TLabel;
    preSrcUserKey:              TOTFEFreeOTFE_PasswordRichEdit;
    seSrcSaltLength:            TSpinEdit64;
    Label2:                     TLabel;
    preDestUserKey1:            TOTFEFreeOTFE_PasswordRichEdit;
    preDestUserKey2:            TOTFEFreeOTFE_PasswordRichEdit;
    Label3:                     TLabel;
    Label5:                     TLabel;
    seDestSaltLength:           TSpinEdit64;
    Label7:                     TLabel;
    cbDestDriveLetter:          TComboBox;
    Label12:                    TLabel;
    GroupBox1:                  TGroupBox;
    lblDestFilename:            TSDUFilenameLabel;
    pbBrowseDest:               TButton;
    tsSrcFile:                  TTabSheet;
    reInstructSrcFile:          TOTFEFreeOTFE_InstructionRichEdit;
    GroupBox2:                  TGroupBox;
    lblSrcFilename:             TSDUFilenameLabel;
    pbBrowseSrc:                TButton;
    tsFileOrPartition:          TTabSheet;
    tsPartitionSelect:          TTabSheet;
    rgFileOrPartition:          TRadioGroup;
    reInstructFileOrPartition:  TOTFEFreeOTFE_InstructionRichEdit;
    reInstructPartitionSelect:  TOTFEFreeOTFE_InstructionRichEdit;
    Label21:                    TLabel;
    seDestKeyIterations:        TSpinEdit64;
    Label8:                     TLabel;
    seSrcKeyIterations:         TSpinEdit64;
    Label11:                    TLabel;
    gbRNG:                      TGroupBox;
    ckRNGMouseMovement:         TCheckBox;
    ckRNGCryptoAPI:             TCheckBox;
    ckRNGcryptlib:              TCheckBox;
    ckRNGGPG:                   TCheckBox;
    ckRNGPKCS11:                TCheckBox;
    tsRNGPKCS11:                TTabSheet;
    reInstructRNGPKCS11:        TOTFEFreeOTFE_InstructionRichEdit;
    cbToken:                    TComboBox;
    lblToken:                   TLabel;
    pbRefresh:                  TButton;
    fmeSelectPartition:         TfmeSelectPartition;
    se64UnitOffset:             TSDUSpin64Unit_Storage;
    TSDUDiskPartitionsPanel1:   TSDUDiskPartitionsPanel;

    procedure FormShow(Sender: TObject);
    procedure edSrcFilenameChange(Sender: TObject);
    procedure se64OffsetChange(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure seSaltLengthChange(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure MouseRNGByteGenerated(Sender: TObject; random: Byte);
    procedure pbBrowseGPGClick(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);
    procedure pbBrowseDestClick(Sender: TObject);
    procedure pbBrowseSrcClick(Sender: TObject);
    procedure rgFileOrPartitionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
  PRIVATE

    //    fCombinedRandomData: Ansistring;

    deviceList:  TStringList;
    deviceTitle: TStringList;

    CanUseCryptlib:         Boolean;
    FPKCS11TokensAvailable: Boolean;

    function GetRNGSet(): TRNGSet;

    function GetIsPartition(): Boolean;
    function GetSrcFilename(): String;
    function GetOffset(): Int64;
    function GetSrcUserKey(): Ansistring;
    function GetSrcSaltLength(): Integer;
    function GetSrcKeyIterations(): Integer;
    function GetDestFilename(): String;
    procedure SetDestFilename(filename: String);
    function GetDestUserKey(): Ansistring;
    function GetDestSaltLength(): Integer;
    function GetDestKeyIterations(): Integer;
    function GetDestRequestedDriveLetter(): ansiChar;
    //    function GetRandomData(): Ansistring;

    procedure PopulatePKCS11Tokens();

    procedure SetupInstructionsCommon();
    procedure SetupInstructionsCreateKeyfile();
    procedure SetupInstructionsChangePassword();

  PROTECTED
    procedure SetupInstructions(); OVERRIDE;

    procedure EnableDisableControls(); OVERRIDE;

    function IsTabSkipped(tabSheet: TTabSheet): Boolean; OVERRIDE;

    procedure FormWizardStepChanged(Sender: TObject);
    function IsTabComplete(checkTab: TTabSheet): Boolean; OVERRIDE;
  PUBLIC
    ChangePasswordCreateKeyfile: TChangePasswordCreateKeyfile;

    procedure fmeSelectPartitionChanged(Sender: TObject);

  PUBLISHED
  {  property IsPartition: Boolean Read GetIsPartition;
    property SrcFilename: String Read GetSrcFilename;
    property Offset: Int64 Read GetOffset;
    property SrcUserKey: Ansistring Read GetSrcUserKey;
    property SrcSaltLength: Integer Read GetSrcSaltLength;
    property SrcKeyIterations: Integer Read GetSrcKeyIterations;
    property DestFilename: String Read GetDestFilename Write SetDestFilename;
    property DestUserKey: Ansistring Read GetDestUserKey;
    property DestSaltLength: Integer Read GetDestSaltLength;
    property DestKeyIterations: Integer Read GetDestKeyIterations;
    property DestRequestedDriveLetter: ansichar Read GetDestRequestedDriveLetter; }
    //    property RandomData: Ansistring Read GetRandomData;

  end;


implementation

{$R *.DFM}

uses
  MSCryptoAPI,
  OTFEFreeOTFE_PKCS11, OTFEFreeOTFEDLL_U,
  SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  FILEORPART_OPT_VOLUME_FILE = 'File';
  FILEORPART_OPT_PARTITION   = 'Partition';

{ TODO 2 -otdk -crefactor : this uses a lot of same code and GUI as create volume - turn into frames }
function TfrmWizardChangePasswordCreateKeyfile.GetSrcFilename(): String;
begin
  Result := '';

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := lblSrcFilename.Caption;
  end;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetOffset(): Int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetSrcUserKey(): Ansistring;
begin
  { TODO 1 -otdk -cfix : warn user - no unicode }
  Result := preSrcUserKey.Text;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetSrcSaltLength(): Integer;
begin
  Result := seSrcSaltLength.Value;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetDestFilename(): String;
begin
  Result := lblDestFilename.Caption;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetDestFilename(filename: String);
begin
  lblDestFilename.Caption := filename;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetDestUserKey(): Ansistring;
begin
  Result := preDestUserKey1.Text; { TODO 1 -otdk -cfix : warn user - no unicode }
end;

function TfrmWizardChangePasswordCreateKeyfile.GetDestSaltLength(): Integer;
begin
  Result := seDestSaltLength.Value;  { TODO 1 -otdk -cfix : warn user - no unicode }
end;

function TfrmWizardChangePasswordCreateKeyfile.GetSrcKeyIterations(): Integer;
begin
  Result := seSrcKeyIterations.Value;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetDestKeyIterations(): Integer;
begin
  Result := seDestKeyIterations.Value;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetDestRequestedDriveLetter(): ansichar;
var
  driveLetter: ansichar;
begin
  driveLetter := #0;
  if (cbDestDriveLetter.ItemIndex > 0) then begin
    driveLetter := AnsiChar(cbDestDriveLetter.Items[cbDestDriveLetter.ItemIndex][1]);
  end;

  Result := driveLetter;
end;


 //function TfrmWizardChangePasswordCreateKeyfile.GetRandomData(): Ansistring;
 //begin
 //  Result := fCombinedRandomData;
 //end;


// Returns TRUE if the specified tab should be skipped in 'next' progression, otherwise FALSE
function TfrmWizardChangePasswordCreateKeyfile.IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited IsTabSkipped(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then begin
    Result := (fFreeOTFEObj is TOTFEFreeOTFEDLL);
  end;

  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsSrcFile) then begin
    Result := GetIsPartition();
  end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then begin
    Result := not (GetIsPartition());
  end;

  // If the user isn't creating a keyfile, skip the destination keyfile tabsheet
  if (tabSheet = tsDestFile) then begin
    Result := (ChangePasswordCreateKeyfile <> opCreateKeyfile);
  end;

  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then begin
    Result := not (rngMouseMovement in GetRNGSet());
  end;

  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then begin
    Result := not (rngPKCS11 in GetRNGSet());
  end;

  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then begin
    Result := not (rngGPG in GetRNGSet());
  end;

end;


procedure TfrmWizardChangePasswordCreateKeyfile.EnableDisableControls();
begin
  inherited;

  // (No wizard specific code for *this* wizard.)

end;


 // This procedure will check to see if all items on the current tabsheet have
 // been successfully completed
function TfrmWizardChangePasswordCreateKeyfile.IsTabComplete(checkTab: TTabSheet): Boolean;
var
  randomBitsGenerated: Integer;
begin
  inherited;

  Result := False;

  if (checkTab = tsFileOrPartition) then begin
    // Ensure one option has been selected
    Result := (rgFileOrPartition.ItemIndex >= 0);
  end else
  if (checkTab = tsSrcFile) then begin
    // Flag tabsheet complete if a valid filename has been specified
    if (GetIsPartition()) then begin
      Result := True;
    end else begin
      Result := FileExists(GetSrcFilename());
    end;

  end else
  if (checkTab = tsPartitionSelect) then begin
    // If we're creating a volume on a partition, one must be selected
    if (not (GetIsPartition())) then begin
      Result := True;
    end else begin
      Result := (fmeSelectPartition.SelectedDevice <> '');
    end;

  end else
  if (checkTab = tsSrcDetails) then begin
    // Check that the number entered is less than the max size...
    if (GetIsPartition()) then begin
      Result := (GetOffset() >= 0);
    end else begin
      Result := (GetOffset() < SDUGetFileSize(GetSrcFilename())) and (GetOffset() >= 0);
    end;

    // Check that the number entered is less than the size of the file...
    // Src salt length >= 0
    // Note that the password *can* be blank - e.g. with the NULL encryption
    // cypher
    Result := Result and (GetSrcSaltLength() >= 0) and (GetSrcKeyIterations() > 0);
  end else
  if (checkTab = tsDestFile) then begin
    // Dest filename entered
    Result := (GetDestFilename() <> '');

    // Sanity check - if we're creating a keyfile, ensure user's not trying to
    // overwrite the original volume file!
    if (ChangePasswordCreateKeyfile = opCreateKeyfile) then begin
      Result := Result and (GetSrcFilename() <> GetDestFilename());
    end;

  end else
  if (checkTab = tsDestDetails) then begin
    // Check that user password is confirmed
    // Note that RNG is always set to something, and others always default
    // Note that the password *can* be blank - e.g. with the NULL encryption
    // cypher
    Result := (preDestUserKey1.Text = preDestUserKey2.Text) and
      (GetDestSaltLength() >= 0) and (GetDestKeyIterations() > 0);
  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    Result := (GetRNGSet() <> []);
  end else
  if (checkTab = tsRNGMouseMovement) then begin
    // This is a good place to update the display of the number of random bits
    // generated...
    randomBitsGenerated     := CountMouseRNGData();
    lblMouseRNGBits.Caption := SDUParamSubstitute(_('Random bits generated: %1/%2'),
      [randomBitsGenerated, CRITICAL_DATA_LENGTH]);

    Result           := (randomBitsGenerated >= CRITICAL_DATA_LENGTH);
    MouseRNG.Enabled := not (Result);
    if MouseRNG.Enabled then begin
      MouseRNG.Color := clWindow;
    end else begin
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
  end;
end;



procedure TfrmWizardChangePasswordCreateKeyfile.FormShow(Sender: TObject);
var
  i:  Integer;
  dl: Char;
begin
  inherited;
  //SDUInitAndZeroBuffer(0,fCombinedRandomData);

  //  fCombinedRandomData := '';

  // tsFileOrPartition
  rgFileOrPartition.Items.Clear();
  rgFileOrPartition.Items.Add(FILEORPART_OPT_VOLUME_FILE);
  rgFileOrPartition.Items.Add(FILEORPART_OPT_PARTITION);
  rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);

  // tsSrcFile
  lblSrcFilename.Caption       := '';
  seSrcKeyIterations.MinValue  := 1;
  seSrcKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seSrcKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seSrcKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  fmeSelectPartition.FreeOTFEObj := fFreeOTFEObj;
  // Only list CDROMs if we're creating a keyfile
  //  - CDROMs can't have their passwords changed
  fmeSelectPartition.AllowCDROM  := (ChangePasswordCreateKeyfile = opCreateKeyfile);
  fmeSelectPartition.OnChange    := fmeSelectPartitionChanged;
  fmeSelectPartition.Tag         := 1;
  fmeSelectPartition.Initialize();

  // tsSrcDetails
  preSrcUserKey.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preSrcUserKey.WantReturns := True;
  preSrcUserKey.WordWrap    := True;
  preSrcUserKey.Lines.Clear();
  preSrcUserKey.PasswordChar := fFreeOTFEObj.PasswordChar;
  preSrcUserKey.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preSrcUserKey.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;

  se64UnitOffset.Value := 0;

  seSrcSaltLength.Increment := 8;
  seSrcSaltLength.Value     := DEFAULT_SALT_LENGTH;

  // tsDestFile
  lblDestFilename.Caption := '';

  // tsDestDetails
  preDestUserKey1.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preDestUserKey1.WantReturns := True;
  preDestUserKey1.WordWrap    := True;
  preDestUserKey1.Lines.Clear();
  preDestUserKey1.PasswordChar := fFreeOTFEObj.PasswordChar;
  preDestUserKey1.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preDestUserKey1.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;

  preDestUserKey2.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preDestUserKey2.WantReturns := True;
  preDestUserKey2.WordWrap    := True;
  preDestUserKey2.Lines.Clear();
  preDestUserKey2.PasswordChar := fFreeOTFEObj.PasswordChar;
  preDestUserKey2.PasswordChar := fFreeOTFEObj.PasswordChar;
  preDestUserKey2.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preDestUserKey2.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;

  seDestSaltLength.Increment := 8;
  seDestSaltLength.Value     := DEFAULT_SALT_LENGTH;

  seDestKeyIterations.MinValue  := 1;
  seDestKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seDestKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seDestKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

  cbDestDriveLetter.Items.Clear();
  cbDestDriveLetter.Items.Add(_('Use default'));
  //  for dl:='C' to 'Z' do
  for dl := 'A' to 'Z' do begin
    cbDestDriveLetter.Items.Add(dl + ':');
  end;
  // Autoselect the "default" (first) entry
  cbDestDriveLetter.ItemIndex := 0;

  // tsRNGSelect
  ckRNGCryptoAPI.Checked := True;
  ckRNGcryptlib.Enabled  := CanUseCryptlib;
  ckRNGPKCS11.Enabled    := PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  PopulatePKCS11Tokens();

  // tsRNGGPG
  lblGPGFilename.Caption := '';


  // Set all tabs to show that they have not yet been completed
  for i := 0 to (pcWizard.PageCount - 1) do begin
    pcWizard.Pages[i].TabVisible := False;
    // Each tabsheet's tag indicates if that tab has been completed or not; 1 for
    // completed, 0 for not yet complete
    pcWizard.Pages[i].Tag        := 0;
  end;


  // BECAUSE WE DEFAULTED TO USING A VOLUME *FILE*, WE MUST SET THE PARTITION
  // SELECT PAGE TO "DONE" HERE (otherwise, it'll never be flagged as complete
  // if the user doens't switch)
  tsPartitionSelect.Tag := 1;

  // BECAUSE WE DEFAULTED TO USING THE MOUSERNG, WE MUST SET THE GPG PAGE TO
  // "DONE" HERE (otherwise, it'll never be flagged as complete if the user
  // doens't switch RNGs)
  tsRNGGPG.Tag := 1;

  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;

  // DLL doesn't currently support partitions
  if (fFreeOTFEObj is TOTFEFreeOTFEDLL) then begin
    pcWizard.ActivePageIndex := 1;
    // Mark as completed
    tsFileOrPartition.Tag    := 1;
  end;


  UpdateUIAfterChangeOnCurrentTab();
end;


procedure TfrmWizardChangePasswordCreateKeyfile.edSrcFilenameChange(Sender: TObject);
begin
  if (ChangePasswordCreateKeyfile = opChangePassword) then begin
    SetDestFilename(GetSrcFilename());
  end;
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.se64OffsetChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.preUserKeyChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardChangePasswordCreateKeyfile.seSaltLengthChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardChangePasswordCreateKeyfile.ckRNGClick(Sender: TObject);
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

procedure TfrmWizardChangePasswordCreateKeyfile.MouseRNGByteGenerated(Sender: TObject;
  random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < CRITICAL_DATA_LENGTH) then begin
    AddToMouseRNGData(random);
  end;

  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardChangePasswordCreateKeyfile.pbBrowseGPGClick(Sender: TObject);
begin
  GPGOpenDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  GPGOpenDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  GPGOpenDialog.Options    := GPGOpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(GPGOpenDialog, lblGPGFilename.Caption);
  if GPGOpenDialog.Execute then begin
    lblGPGFilename.Caption := GPGOpenDialog.Filename;
  end;

  UpdateUIAfterChangeOnCurrentTab();

end;


function TfrmWizardChangePasswordCreateKeyfile.GetRNGSet(): TRNGSet;
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


procedure TfrmWizardChangePasswordCreateKeyfile.pbFinishClick(Sender: TObject);
var
  allOK:     Boolean;
  saltBytes: Ansistring;
  i:         Integer;
begin
  inherited;

  GetRandPool.SetUpRandPool(GetRNGSet(),
    fFreeOTFEObj.PKCS11Library, PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption);


  // Grab 'n' bytes from the random pool to use as the salt
  allOK := GetRandPool.GenerateRandomData(GetDestSaltLength() div 8, saltBytes);
  if (allOK) then begin
    if (ChangePasswordCreateKeyfile = opChangePassword) then begin
      allOK := fFreeOTFEObj.ChangeVolumePassword(GetSrcFilename(), GetOffset(),
        GetSrcUserKey(), GetSrcSaltLength(),  // In bits
        GetSrcKeyIterations(), GetDestUserKey(), saltBytes, GetDestKeyIterations(),
        GetDestRequestedDriveLetter());
    end else begin
      allOK := fFreeOTFEObj.CreateKeyfile(GetSrcFilename(), GetOffset(),
        GetSrcUserKey(), GetSrcSaltLength(),  // In bits
        GetSrcKeyIterations(), GetDestFilename(), GetDestUserKey(), saltBytes,
        GetDestKeyIterations(), GetDestRequestedDriveLetter());
    end;

  end;

  if (allOK) then begin
    SDUMessageDlg(_('Completed successfully.'), mtInformation);
    ModalResult := mrOk;
  end else begin
    SDUMessageDlg(
      _('Unable to complete requested operation: please ensure that your Box is not already open or otherwise in use, and that the keyphrase, salt and offset entered are correct.'),
      mtError
      );
  end;

  { TODO 1 -otdk -ccleanup : whats this for? }
  for i := 1 to length(saltBytes) do begin
    saltBytes[i] := AnsiChar(i);
  end;

end;


procedure TfrmWizardChangePasswordCreateKeyfile.SetupInstructionsChangePassword();
begin
  self.Caption := _('Change Keyphrase/Other Details');

  reInstructFileOrPartition.Text :=
    _('If you would like to modify a Box, please specify whether it is file or partition based.'
    + SDUCRLF + SDUCRLF + 'If you would like to modify a keyfile, please select "file" below.');

  reInstructSrcFile.Text :=
    _('Please enter the full path and filename of the Box/keyfile you wish to change.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" DoxBox held within another DoxBox, please specify the filename of the outer box which stores your hidden box');

  reInstructPartitionSelect.Text :=
    _('Please specify the partition your box is stored on.');

  reInstructSrcDetails.Text :=
    _('Please enter the full details of the box/keyfile you wish to change.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" DoxBox held within another DoxBox, please specify the offset within the outer box where your hidden box is located');

  reInstructDestFile.Text :=
    _('n/a');

  reInstructDestDetails.Text :=
    _('Please enter new details for your box/keyfile.');

  reInstructRNGSelect.Text :=
    _('In order to change your box/keyfile''s details, a certain amount of random data is required.'
    + SDUCRLF + SDUCRLF + 'This data will be used for the following:' +
    SDUCRLF + SDUCRLF + '1) Password salting' + SDUCRLF + '2) Random padding data' +
    SDUCRLF + SDUCRLF +
    'In order to generate this data, please select which random number generators you wish to use from the options below.');

end;


procedure TfrmWizardChangePasswordCreateKeyfile.SetupInstructionsCreateKeyfile();
begin
  self.Caption := _('Create Keyphrase for Box');

  reInstructFileOrPartition.Text :=
    _('Please specify whether the box you wish to create a keyfile for is file or partition based.');

  reInstructSrcFile.Text :=
    _('Please enter the full path and filename of either:' + SDUCRLF + SDUCRLF +
    '1) A box containing a CDB, or' + SDUCRLF + '2) An existing keyfile' +
    SDUCRLF + SDUCRLF + 'for the box you wish to create a keyfile for.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" box which is inside another box, and your hidden box includes a CDB, please specify the filename of the outer box which stores your hidden box.');

  reInstructPartitionSelect.Text :=
    _('Please specify the partition your box is stored on.');

  reInstructSrcDetails.Text :=
    _('Please enter the full details of the box you wish to create a keyfile for.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" box which is inside another box, and your hidden volume includes a CDB, please specify the offset within the outer box where your hidden box is located.');

  reInstructDestFile.Text :=
    _('Please specify the full path and filename where the keyfile should be written to.');

  reInstructDestDetails.Text :=
    _('Please specify the details for your new keyfile.');

  reInstructRNGSelect.Text :=
    _('In order to create a new keyfile, a certain amount of random data is required.' +
    SDUCRLF + SDUCRLF + 'This data will be used for the following:' + SDUCRLF +
    SDUCRLF + '1) Password salting' + SDUCRLF + '2) Random padding data' +
    SDUCRLF + SDUCRLF +
    'In order to generate this data, please select which random number generators you wish to use from the options below.');

end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetupInstructions();
begin
  inherited;

  SetupInstructionsCommon();
  if (ChangePasswordCreateKeyfile = opChangePassword) then begin
    SetupInstructionsChangePassword();
  end else begin
    SetupInstructionsCreateKeyfile();
  end;

end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetupInstructionsCommon();
begin
  reInstructRNGMouseMovement.Text :=
    _('You have selected mouse movement to generate random data.' + SDUCRLF +
    SDUCRLF + 'Please "wiggle" the mouse within the area below, until enough random data has been generated.');

  reInstructRNGGPG.Text :=
    _('In order to use GPG to generate random data, please specify the location of "gpg.exe" by clicking the browse button.');

  reInstructRNGPKCS11.Text :=
    _('Please select the PKCS#11 token you wish to use to generate random data, from the list shown below');

end;


procedure TfrmWizardChangePasswordCreateKeyfile.pbBrowseDestClick(Sender: TObject);
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, lblDestFilename.Caption);

  if SaveDialog.Execute() then begin
    if (FileExists(SaveDialog.Filename)) then begin
      SDUMessageDlg(
        _('A file with the filename you specified already exists.' + SDUCRLF +
        SDUCRLF + 'For safety reasons, you must specify a file which doesn''t already exist.'
        + SDUCRLF + SDUCRLF +
        'Please either delete the existing file, or specify a new filename.'),
        mtError
        );
    end else begin
      lblDestFilename.Caption := SaveDialog.Filename;
    end;

  end;

  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardChangePasswordCreateKeyfile.pbBrowseSrcClick(Sender: TObject);
begin
  OpenDialog.Filter     := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
  OpenDialog.DefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
  OpenDialog.Options    := OpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(OpenDialog, lblSrcFilename.Caption);

  if OpenDialog.Execute() then begin
    lblSrcFilename.Caption := OpenDialog.Filename;
  end;

  UpdateUIAfterChangeOnCurrentTab();

end;

procedure TfrmWizardChangePasswordCreateKeyfile.rgFileOrPartitionClick(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();

end;


function TfrmWizardChangePasswordCreateKeyfile.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = rgFileOrPartition.Items.IndexOf(
    FILEORPART_OPT_PARTITION));
end;

procedure TfrmWizardChangePasswordCreateKeyfile.FormWizardStepChanged(Sender: TObject);
begin
  inherited;

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

procedure TfrmWizardChangePasswordCreateKeyfile.FormCreate(Sender: TObject);
begin
  deviceList  := TStringList.Create();
  deviceTitle := TStringList.Create();

  // Start cryptlib, if possible, as early as we can to allow it as much time
  // as possible to poll entropy
  CanUseCryptlib := cryptlibLoad();

  fmeSelectPartition.OnChange := fmeSelectPartitionChanged;

  OnWizardStepChanged := FormWizardStepChanged;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.FormDestroy(Sender: TObject);
begin
  deviceList.Free();
  deviceTitle.Free();

  // Shutdown cryptlib, if used
  if CanUseCryptlib then begin
    cryptlibUnload();
    CanUseCryptlib := False;
  end;

  PurgeMouseRNGData();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(fFreeOTFEObj.PKCS11Library, cbToken) > 0);
end;

procedure TfrmWizardChangePasswordCreateKeyfile.pbRefreshClick(Sender: TObject);
begin
  PopulatePKCS11Tokens();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.fmeSelectPartitionChanged(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

end.
