unit frmWizardChangePasswordCreateKeyfile;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //
 { TODO 1 -otdk -crefactor : shares many fields with frmKeyEntryFreeOTFE - extract to frames}

interface

uses

 //delphi & libs
   Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages, MouseRNG,
  PasswordRichEdit, Spin64, StdCtrls, SysUtils, Windows,
  //sdu & LibreCrypt utils
     lcDialogs, sdurandpool, SDUStdCtrls,  fmeSDUDiskPartitions,
lcTypes,
DriverAPI,   // Required for CRITICAL_DATA_LEN
  OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,  SDUForms, SDUFrames, SDUSpin64Units, SDUDialogs,
   // LibreCrypt forms and frames
  fmeSelectPartition,
  frmWizard,
  fmeSDUBlocks,
  fmeNewPassword, fmePassword;

type
  TChangePasswordCreateKeyfile = (opChangePassword, opCreateKeyfile);

  TfrmWizardChangePasswordCreateKeyfile = class (TfrmWizard)
    tsSrcDetails:    TTabSheet;
    tsDestDetails:   TTabSheet;
    reInstructSrcDetails: TLabel;
    tsRNGMouseMovement: TTabSheet;
    reInstructRNGMouseMovement: TLabel;
    MouseRNG:        TMouseRNG;
    lblMouseRNGBits: TLabel;
    tsDestFile:      TTabSheet;
    reInstructDestFile: TLabel;
    tsRNGGPG:        TTabSheet;
    reInstructRNGGPG: TLabel;
    GroupBox5:       TGroupBox;
    lblGPGFilename:  TSDUFilenameLabel;
    pbBrowseGPG:     TButton;
    OpenDialog:      TSDUOpenDialog;
    SaveDialog:      TSDUSaveDialog;
    GPGOpenDialog:   TSDUOpenDialog;
    tsRNGSelect:     TTabSheet;
    reInstructRNGSelect1: TLabel;
    Label9:          TLabel;
    Label4:          TLabel;
    Label6:          TLabel;
    seSrcSaltLength: TSpinEdit64;
    Label5:          TLabel;
    seDestSaltLength: TSpinEdit64;
    Label7:          TLabel;
    cbDestDriveLetter: TComboBox;
    Label12:         TLabel;
    gbKeyFile:       TGroupBox;
    lblDestFilename: TSDUFilenameLabel;
    pbBrowseDest:    TButton;
    tsSrcFile:       TTabSheet;
    reInstructSrcFile: TLabel;
    GroupBox2:       TGroupBox;
    lblSrcFilename:  TSDUFilenameLabel;
    pbBrowseSrc:     TButton;
    tsFileOrPartition: TTabSheet;
    tsPartitionSelect: TTabSheet;
    rgFileOrPartition: TRadioGroup;
    reInstructFileOrPartition: TLabel;
    reInstructPartitionSelect: TLabel;
    Label21:         TLabel;
    seDestKeyIterations: TSpinEdit64;
    Label8:          TLabel;
    seSrcKeyIterations: TSpinEdit64;
    Label11:         TLabel;
    gbRNG:           TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI:  TCheckBox;
    ckRNGcryptlib:   TCheckBox;
    ckRNGGPG:        TCheckBox;
    ckRNGPKCS11:     TCheckBox;
    tsRNGPKCS11:     TTabSheet;
    reInstructRNGPKCS11: TLabel;
    cbToken:         TComboBox;
    lblToken:        TLabel;
    pbRefresh:       TButton;
    fmeSelectPartition: TfmeSelectPartition;
    se64UnitOffset:  TSDUSpin64Unit_Storage;
    TSDUDiskPartitionsPanel1: TfmeSDUDiskPartitions;
    frmeNewPassword: TfrmeNewPassword;
    lblInstructDestDetails: TLabel;
    reInstructRNGSelect2: TLabel;
    reInstructRNGSelect3: TLabel;
    reInstructRNGSelect4: TLabel;
    frmePassword1:   TfrmePassword;

    procedure FormShow(Sender: TObject);
    procedure edSrcFilenameChange(Sender: TObject);
    procedure se64OffsetChange(Sender: TObject);
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure frmeNewPasswordChange(Sender: TObject);
    procedure preSrcUserKeyChange(Sender: TObject);

  private

    fdeviceList:  TStringList;
    fdeviceTitle: TStringList;
    FPKCS11TokensAvailable: Boolean;

    function GetRNGSet(): TRNGSet;

    function GetIsPartition(): Boolean;
    function GetSrcFilename(): String;
    function GetOffset(): Int64;
    function GetSrcUserKey(): TSDUBytes;
    function GetSrcSaltLength(): Integer;
    function GetSrcKeyIterations(): Integer;
    procedure SetSrcFilename(const Value: String);
    procedure SetSrcUserKey(const Value: TSDUBytes);

    function GetDestFilename(): String;
    procedure SetDestFilename(filename: String);
    function GetDestUserKey(): TSDUBytes;
    function GetDestSaltLength(): Integer;
    function GetDestKeyIterations(): Integer;
    function GetDestRequestedDriveLetter(): DriveLetterChar;
    procedure SetDestUserKey(const Value: TSDUBytes);

    procedure _PopulatePKCS11Tokens();
    procedure _SetupInstructionsCreateKeyfile();
    procedure SetIsPartition(const Value: Boolean);

  protected
    fsilent:       Boolean;
    fsilentResult: TModalResult;
    fChangePasswordCreateKeyfile: TChangePasswordCreateKeyfile;

    procedure _SetupInstructions(); override;
    function _IsTabSkipped(tabSheet: TTabSheet): Boolean; override;
    procedure _FormWizardStepChanged(Sender: TObject);
    function _IsTabComplete(checkTab: TTabSheet): Boolean; override;

  public

    procedure fmeSelectPartitionChanged(Sender: TObject);

  published
    property IsPartition: Boolean Read GetIsPartition Write SetIsPartition;
    property SrcFilename: String Read GetSrcFilename Write SetSrcFilename;
    {    property Offset: Int64 Read GetOffset;}
    property SrcUserKey: TSDUBytes Read GetSrcUserKey Write SetSrcUserKey;
{    property SrcSaltLength: Integer Read GetSrcSaltLength;
    property SrcKeyIterations: Integer Read GetSrcKeyIterations;
    property DestFilename: String Read GetDestFilename Write SetDestFilename;}
    property DestUserKey: TSDUBytes Read GetDestUserKey Write SetDestUserKey;
{    property DestSaltLength: Integer Read GetDestSaltLength;
    property DestKeyIterations: Integer Read GetDestKeyIterations;
    property DestRequestedDriveLetter: ansichar Read GetDestRequestedDriveLetter; }


  end;

      function WizardChangePassword(SrcFilename: String = ''; OrigKeyPhrase: Ansistring = '';
      NewKeyPhrase: Ansistring = ''): Boolean;
    function WizardCreateKeyfile(): Boolean;


implementation

{$R *.DFM}

uses
  //delphi & libs (0)
   MSCryptoAPI,
  //sdu & LibreCrypt utils (1)
    lcConsts, sduGeneral,lcCommandLine,
  OTFEFreeOTFEDLL_U,
  PKCS11Lib, SDUi18n,
  pkcs11_library
   // LibreCrypt forms and frames (2)
    //main form  (3)
;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  FILEORPART_OPT_PARTITION_INDEX = 1; //index of 'partition' item in rgFileOrPartition
  FILEORPART_OPT_VOLUME_FILE_INDEX = 0;// index of 'file' item

{ TODO 2 -otdk -crefactor : this uses a lot of same code and GUI as create volume - turn into frames }
function TfrmWizardChangePasswordCreateKeyfile.GetSrcFilename(): String;
begin

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := lblSrcFilename.Caption;
  end;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetSrcFilename(const Value: String);
begin
  if GetIsPartition() then begin
    //    fmeSelectPartition.SelectedDevice := Value;
    assert(False, 'this feature not supported');
  end else begin
    lblSrcFilename.Caption := Value;
  end;
end;



function TfrmWizardChangePasswordCreateKeyfile.GetOffset(): Int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmWizardChangePasswordCreateKeyfile.GetSrcUserKey(): TSDUBytes;
begin
  { TODO 1 -otdk -cfix : warn user - no unicode }
  Result := frmePassword1.GetKeyPhrase;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetSrcUserKey(const Value: TSDUBytes);
begin
  frmePassword1.SetKeyPhrase(Value);
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

function TfrmWizardChangePasswordCreateKeyfile.GetDestUserKey(): TSDUBytes;
begin
  Result := frmeNewPassword.GetKeyPhrase;
  { TODO 1 -otdk -cfix : warn user - no unicode }
end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetDestUserKey(const Value: TSDUBytes);
begin
  frmeNewPassword.SetKeyPhrase(Value);
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

function TfrmWizardChangePasswordCreateKeyfile.GetDestRequestedDriveLetter(): DriveLetterChar;

begin
  Result := #0;
  if (cbDestDriveLetter.ItemIndex > 0) then begin
    Result := cbDestDriveLetter.Items[cbDestDriveLetter.ItemIndex][1];
  end;

end;

// Returns TRUE if the specified tab should be skipped in 'next' progression, otherwise FALSE
function TfrmWizardChangePasswordCreateKeyfile._IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited _IsTabSkipped(tabSheet);

  // DLL doesn't currently support partitions
  if (tabSheet = tsFileOrPartition) then begin
    Result := (GetFreeOTFEBase() is TOTFEFreeOTFEDLL);
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
  if (tabSheet = tsDestFile) then
    Result := (fChangePasswordCreateKeyfile <> opCreateKeyfile);


  // If the user *isn't* using the mouse movement RNG, skip the tsRNGMouseMovement tabsheet
  if (tabSheet = tsRNGMouseMovement) then
    Result := not (rngMouseMovement in GetRNGSet());


  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
    Result := not (rngPKCS11 in GetRNGSet());


  // If the user *isn't* using the GPG RNG, skip the tsRNGGPG tabsheet
  if (tabSheet = tsRNGGPG) then begin
    Result := not (rngGPG in GetRNGSet());
  end;

end;



 // This procedure will check to see if all items on the current tabsheet have
 // been successfully completed
function TfrmWizardChangePasswordCreateKeyfile._IsTabComplete(checkTab: TTabSheet): Boolean;
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
    if (fChangePasswordCreateKeyfile = opCreateKeyfile) then
      Result := Result and (GetSrcFilename() <> GetDestFilename());

  end else
  if (checkTab = tsDestDetails) then begin
    // Check that user password is confirmed
    // Note that RNG is always set to something, and others always default
    // Note that the password *can* be blank - e.g. with the NULL encryption
    // cypher
    Result := (frmeNewPassword.IsPasswordValid) and (GetDestSaltLength() >= 0) and
      (GetDestKeyIterations() > 0);
  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    Result := (GetRNGSet() <> []);
  end else
  if (checkTab = tsRNGMouseMovement) then begin
    // This is a good place to update the display of the number of random bits
    // generated...
    randomBitsGenerated     := CountMouseRNGData();
    lblMouseRNGBits.Caption := Format(_('Random bits generated: %d/%d'),
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
  { TODO -otdk -crefactor : review what should be in formcreate  }
  // tsSrcFile
  if not fsilent then
    lblSrcFilename.Caption := '';
  seSrcKeyIterations.MinValue := 1;
  seSrcKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seSrcKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seSrcKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

  // tsPartitionSelect
  // Setup and make sure nothing is selected
  //  fmeSelectPartition.FreeOTFEObj := fFreeOTFEObj;
  // Only list CDROMs if we're creating a keyfile
  //  - CDROMs can't have their passwords changed
  fmeSelectPartition.AllowCDROM := (fChangePasswordCreateKeyfile = opCreateKeyfile);
  fmeSelectPartition.OnChange   := fmeSelectPartitionChanged;
  fmeSelectPartition.Tag        := 1;
  //  fmeSelectPartition.Initialize();

  // tsSrcDetails

  if not fsilent then
    frmePassword1.ClearPassword;

  se64UnitOffset.Value := 0;

  seSrcSaltLength.Increment := 8;
  seSrcSaltLength.Value     := DEFAULT_SALT_LENGTH;

  // tsDestFile
  lblDestFilename.Caption := '';

  // tsDestDetails
  //
  //  if not fsilent then
  //    preDestUserKey1.Lines.Clear();
  //
  //  if not fsilent then
  //    preDestUserKey2.Lines.Clear();

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
  for dl := 'A' to 'Z' do
    cbDestDriveLetter.Items.Add(dl + ':');

  // Autoselect the "default" (first) entry
  cbDestDriveLetter.ItemIndex := 0;

  // tsRNGSelect
  ckRNGCryptoAPI.Checked := True;
  ckRNGcryptlib.Enabled  := GetRandPool().CanUseCryptlib;
  if ckRNGcryptlib.Enabled then
    ckRNGcryptlib.Checked := True;

  ckRNGPKCS11.Enabled := PKCS11LibraryReady(GPKCS11Library);

  // tsRNGMouseMovement
  InitMouseRNGData();

  // tsRNGPKCS11
  _PopulatePKCS11Tokens();

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
  if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
    pcWizard.ActivePageIndex := 1;
    // Mark as completed
    tsFileOrPartition.Tag    := 1;
  end;


  _UpdateUIAfterChangeOnCurrentTab();

  if fSilent then begin
    ModalResult := mrCancel;
    pbFinishClick(self);
    FSilentResult := ModalResult;
    // if testing and no errors, then close dlg
    if ModalResult = mrOk then
      PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

  //tag is used as flag if translated already
  if self.tag = 0 then
    self.Caption := SDUTranslate(self.Caption);
  self.tag := 1;

  SDUTranslateComp(reInstructSrcFile);
  SDUTranslateComp(reInstructFileOrPartition);
  SDUTranslateComp(reInstructSrcDetails);
  SDUTranslateComp(reInstructDestFile);
  SDUTranslateComp(reInstructRNGSelect1);
  SDUTranslateComp(reInstructRNGSelect2);
  SDUTranslateComp(reInstructRNGSelect3);
  SDUTranslateComp(reInstructRNGSelect4);
  SDUTranslateComp(reInstructDestFile);

  SDUTranslateComp(reInstructRNGMouseMovement);

  SDUTranslateComp(reInstructRNGGPG);

  SDUTranslateComp(reInstructRNGPKCS11);

end;


procedure TfrmWizardChangePasswordCreateKeyfile.edSrcFilenameChange(Sender: TObject);
begin
  if (fChangePasswordCreateKeyfile = opChangePassword) then
    SetDestFilename(GetSrcFilename());

  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.se64OffsetChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.frmeNewPasswordChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.seSaltLengthChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
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

  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.MouseRNGByteGenerated(Sender: TObject;
  random: Byte);
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if (CountMouseRNGData() < CRITICAL_DATA_LENGTH) then begin
    AddToMouseRNGData(random);
  end;

  _UpdateUIAfterChangeOnCurrentTab();
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

  _UpdateUIAfterChangeOnCurrentTab();

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
  saltBytes: TSDUBytes;
  // salt:      Ansistring;
begin
  inherited;

  GetRandPool.SetUpRandPool(GetRNGSet(),
    PKCS11TokenListSelected(cbToken),
    lblGPGFilename.Caption);

  try
    // Grab 'n' bytes from the random pool to use as the salt
    GetRandPool.GetRandomData(GetDestSaltLength() div 8, saltBytes);
    //  salt := SDUBytesToString(saltBytes);
    if (fChangePasswordCreateKeyfile = opChangePassword) then begin
      allOK := GetFreeOTFEBase().ChangeVolumePassword(GetSrcFilename(),
        GetOffset(), GetSrcUserKey(), GetSrcSaltLength(),  // In bits
        GetSrcKeyIterations(), GetDestUserKey(), saltBytes, GetDestKeyIterations(),
        GetDestRequestedDriveLetter());
    end else begin
      allOK := GetFreeOTFEBase().CreateKeyfile(GetSrcFilename(), GetOffset(),
        GetSrcUserKey(), GetSrcSaltLength(),  // In bits
        GetSrcKeyIterations(), GetDestFilename(), GetDestUserKey(), saltBytes,
        GetDestKeyIterations(), GetDestRequestedDriveLetter());
    end;
  except
    on E: EInsufficientRandom do begin
      allOK := False;
      SDUMessageDlg(
        _('Insufficient random data generated: ') + E.message + SDUCRLF +
        SDUCRLF + PLEASE_REPORT_TO_FREEOTFE_DOC_ADDR,
        mtError
        );
    end;
  end;

  if (allOK) then begin
    if not fsilent then
      SDUMessageDlg(_('Completed successfully.'), mtInformation);
    ModalResult := mrOk;
  end else begin

    SDUMessageDlg(
      _('Unable to complete requested operation: please ensure that your container is not already open or otherwise in use, and that the keyphrase, salt and offset entered are correct.'),
      mtError
      );
  end;

  { TODO 1 -otdk -ccleanup : whats this for? }
 (* for i := 1 to length(saltBytes) do begin
    saltBytes[i] := AnsiChar(i);
  end;*)

  SDUInitAndZeroBuffer(0, saltBytes);
end;


procedure TfrmWizardChangePasswordCreateKeyfile._SetupInstructionsCreateKeyfile();
begin
  self.Caption := _('Create Keyphrase for container');

  reInstructFileOrPartition.Caption :=
    _('Please specify whether the container you wish to create a keyfile for is file or partition based.');

  reInstructSrcFile.Caption :=
    _('Please enter the full path and filename of either:' + SDUCRLF + SDUCRLF +
    '1) A container containing a FreeOTFE header, or' + SDUCRLF + '2) An existing keyfile' +
    SDUCRLF + SDUCRLF + 'for the container you wish to create a keyfile for.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" container which is inside another container, and your hidden container includes a CDB, please '+
    'specify the filename of the outer container which stores your hidden container.');


  reInstructSrcDetails.Caption :=
    _('Please enter the full details of the container you wish to create a keyfile for.' +
    SDUCRLF + SDUCRLF +
    'If you wish to update a "hidden" container which is inside another container, and your hidden container includes a header, '+
    'please specify the offset within the outer container where your hidden container is located.');



  SDUTranslateComp(lblInstructDestDetails);

  //  reInstructRNGSelect.Caption :=
  //    _('In order to create a new keyfile, a certain amount of random data is required.' +
  //    SDUCRLF + SDUCRLF + 'This data will be used for the following:' + SDUCRLF +
  //    SDUCRLF + '1) Password salting' + SDUCRLF + '2) Random padding data' +
  //    SDUCRLF + SDUCRLF +
  //    'In order to generate this data, please select which random number generators you wish to use from the options below.');

end;

procedure TfrmWizardChangePasswordCreateKeyfile._SetupInstructions();
begin
  inherited;

  if not (fChangePasswordCreateKeyfile = opChangePassword) then
    _SetupInstructionsCreateKeyfile();
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

  _UpdateUIAfterChangeOnCurrentTab();
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

  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.rgFileOrPartitionClick(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

function TfrmWizardChangePasswordCreateKeyfile.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = FILEORPART_OPT_PARTITION_INDEX);
end;

procedure TfrmWizardChangePasswordCreateKeyfile.SetIsPartition(const Value: Boolean);
begin
  if Value then
    rgFileOrPartition.ItemIndex := FILEORPART_OPT_PARTITION_INDEX
  else
    rgFileOrPartition.ItemIndex := FILEORPART_OPT_VOLUME_FILE_INDEX;
end;

procedure TfrmWizardChangePasswordCreateKeyfile._FormWizardStepChanged(Sender: TObject);
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

procedure TfrmWizardChangePasswordCreateKeyfile.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here, note will only close automatically if mr = mrok anyway
  if fsilent then begin
    ModalResult := FSilentResult;
  end;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.FormCreate(Sender: TObject);
begin
  fdeviceList                 := TStringList.Create();
  fdeviceTitle                := TStringList.Create();
  fmeSelectPartition.OnChange := fmeSelectPartitionChanged;

  OnWizardStepChanged := _FormWizardStepChanged;
  { done -otdk -crefactor : populate in designer }

  // tsFileOrPartition
  rgFileOrPartition.ItemIndex := FILEORPART_OPT_VOLUME_FILE_INDEX;
  frmeNewPassword.OnChange    := frmeNewPasswordChange;
  fsilent := GetCmdLine.isSilent;
end;

procedure TfrmWizardChangePasswordCreateKeyfile.FormDestroy(Sender: TObject);
begin
  fdeviceList.Free();
  fdeviceTitle.Free();

  PurgeMouseRNGData();
end;

procedure TfrmWizardChangePasswordCreateKeyfile._PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(GPKCS11Library,
    cbToken) > 0);
end;

procedure TfrmWizardChangePasswordCreateKeyfile.preSrcUserKeyChange(
  Sender: TObject);
begin
  inherited;
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.pbRefreshClick(Sender: TObject);
begin
  _PopulatePKCS11Tokens();
end;

procedure TfrmWizardChangePasswordCreateKeyfile.fmeSelectPartitionChanged(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

function WizardChangePassword(SrcFilename: String = '';
  OrigKeyPhrase: Ansistring = ''; NewKeyPhrase: Ansistring = ''): Boolean;
var
  dlg: TfrmWizardChangePasswordCreateKeyfile;
begin
  Result := False;

  if GetFreeOTFE().WarnIfNoHashOrCypherDrivers() then begin
    dlg := TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.fChangePasswordCreateKeyfile := opChangePassword;
      dlg.IsPartition := False;
      dlg.SrcFileName := SrcFilename;
      dlg.SrcUserKey := SDUStringToSDUBytes(OrigKeyPhrase);
      dlg.DestUserKey := SDUStringToSDUBytes(NewKeyPhrase);
      Result     := (dlg.ShowModal() = mrOk);
    finally
      dlg.Free();
    end;
  end;
end;

function WizardCreateKeyfile(): Boolean;
var
  dlg: TfrmWizardChangePasswordCreateKeyfile;
begin
  Result := False;

  if GetFreeOTFE().WarnIfNoHashOrCypherDrivers() then begin
    dlg := TfrmWizardChangePasswordCreateKeyfile.Create(nil);
    try
      dlg.fChangePasswordCreateKeyfile := opCreateKeyfile;
      Result     := (dlg.ShowModal() = mrOk);
    finally
      dlg.Free();
    end;

  end;
end;


end.
