unit frmCreateLUKSVolumeWizard;
 // Description:
 // (c) tdk
 // dual GPL and freeotfe licence

interface

uses
  //delphi
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages,
  Spin64, StdCtrls, SysUtils, Windows,
  //sdu, lcutils
  PasswordRichEdit, lcDialogs, sdurandpool, SDUStdCtrls, SDUGeneral, SDUForms,
  SDUFrames, SDUSpin64Units, SDUDialogs, SDUFilenameEdit_U,
  //LibreCrypt
  DriverAPI,   // Required for CRITICAL_DATA_LEN
  fmeDiskPartitionsPanel, fmeSelectPartition,
  frmWizard, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  fmeSDUBlocks,
  fmeSDUDiskPartitions,
  fmeNewPassword;

type

  TfrmCreateLUKSVolumeWizard = class (TfrmWizard)
    tsEncDetails:    TTabSheet;
    OpenDialog:      TSDUOpenDialog;
    SaveDialog:      TSDUSaveDialog;
    GPGOpenDialog:   TSDUOpenDialog;
    tsRNGSelect:     TTabSheet;
    reInstructRNGSelect1: TLabel;
    Label5:          TLabel;
    seDestSaltLength: TSpinEdit64;
    Label7:          TLabel;
    cbDestDriveLetter: TComboBox;
    Label12:         TLabel;
    tsVolFile:       TTabSheet;
    tsPartitionSelect: TTabSheet;
    reInstructPartitionSelect: TLabel;
    Label21:         TLabel;
    seDestKeyIterations: TSpinEdit64;
    Label8:          TLabel;
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
    frmeNewPassword: TfrmeNewPassword;
    lblInstructDestDetails: TLabel;
    reInstructRNGSelect2: TLabel;
    reInstructRNGSelect3: TLabel;
    reInstructRNGSelect4: TLabel;
    GroupBox1:       TGroupBox;
    rgFileOrPartition: TRadioGroup;
    feVolFilename:   TSDUFilenameEdit;

    procedure FormShow(Sender: TObject);
    procedure seSaltLengthChange(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);


    procedure rgFileOrPartitionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure frmeNewPasswordChange(Sender: TObject);

    procedure feVolFilenameChange(Sender: TObject);

  private

    fDeviceList:  TStringList;
    fDeviceTitle: TStringList;

    fPKCS11TokensAvailable: Boolean;

    function GetRNGSet(): TRNGSet;

    function GetIsPartition(): Boolean;
    function GetVolFilename(): String;
    procedure SetVolFilename(const Value: String);


    function GetUserKey(): TSDUBytes;
    function GetSaltLength(): Integer;
    function GetKeyIterations(): Integer;
    function GetRequestedDriveLetter(): DriveLetterChar;

    procedure PopulatePKCS11Tokens();

    procedure SetUserKey(const Value: TSDUBytes);
    procedure SetIsPartition(const Value: Boolean);


  protected
    fsilent:       Boolean;
    fsilentResult: TModalResult;

    procedure EnableDisableControls(); override;

    function IsTabSkipped(tabSheet: TTabSheet): Boolean; override;

    procedure FormWizardStepChanged(Sender: TObject);
    function IsTabComplete(checkTab: TTabSheet): Boolean; override;
  public

    procedure fmeSelectPartitionChanged(Sender: TObject);

  published
    property silent: Boolean Read fsilent Write fsilent;
    property IsPartition: Boolean Read GetIsPartition Write SetIsPartition;
    property UserKey: TSDUBytes Read GetUserKey Write SetUserKey;

  end;

function CreateLUKSVolume(): Boolean;

implementation

{$R *.DFM}

uses
  //delphi
  MSCryptoAPI,
  OTFEFreeOTFEDLL_U, OTFEConsts_U,
  PKCS11Lib, SDUi18n,

  //sdu, lcutils
  LUKSTools
  //LibreCrypt
  ;

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
function TfrmCreateLUKSVolumeWizard.GetVolFilename(): String;
begin

  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := feVolFilename.Filename;
  end;
end;

procedure TfrmCreateLUKSVolumeWizard.SetVolFilename(const Value: String);
begin
  if GetIsPartition() then begin
    //    fmeSelectPartition.SelectedDevice := Value;
    assert(False, 'this feature not supported');
  end else begin
    feVolFilename.Filename := Value;
  end;
end;



function TfrmCreateLUKSVolumeWizard.GetUserKey(): TSDUBytes;
begin
  Result := frmeNewPassword.GetKeyPhrase;
  { TODO 1 -otdk -cfix : warn user - no unicode }
end;

procedure TfrmCreateLUKSVolumeWizard.SetUserKey(const Value: TSDUBytes);
begin
  frmeNewPassword.SetKeyPhrase(Value);
end;

function TfrmCreateLUKSVolumeWizard.GetSaltLength(): Integer;
begin
  Result := seDestSaltLength.Value;
end;

function TfrmCreateLUKSVolumeWizard.GetKeyIterations(): Integer;
begin
  Result := seDestKeyIterations.Value;
end;

function TfrmCreateLUKSVolumeWizard.GetRequestedDriveLetter(): DriveLetterChar;

begin
  Result := #0;
  if (cbDestDriveLetter.ItemIndex > 0) then begin
    Result := cbDestDriveLetter.Items[cbDestDriveLetter.ItemIndex][1];
  end;

end;


// Returns TRUE if the specified tab should be skipped in 'next' progression, otherwise FALSE
function TfrmCreateLUKSVolumeWizard.IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited IsTabSkipped(tabSheet);

  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsVolFile) then begin
    Result := GetIsPartition();
  end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then begin
    Result := not (GetIsPartition());
  end;


  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
    Result := not (rngPKCS11 in GetRNGSet());
end;


procedure TfrmCreateLUKSVolumeWizard.EnableDisableControls();
begin
  inherited;

  // (No wizard specific code for *this* wizard.)

end;


 // This procedure will check to see if all items on the current tabsheet have
 // been successfully completed
function TfrmCreateLUKSVolumeWizard.IsTabComplete(checkTab: TTabSheet): Boolean;
var
  randomBitsGenerated: Integer;
begin
  inherited;

  Result := False;

  if (checkTab = tsVolFile) then begin
    // Ensure one option has been selected
    Result := (rgFileOrPartition.ItemIndex >= 0);
    if Result then begin

      // Flag tabsheet complete if a valid filename has been specified
      if (GetIsPartition()) then begin
        Result := True;
      end else begin
        Result := FileExists(GetVolFilename());
      end;
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
  if (checkTab = tsVolFile) then begin
    // Dest filename entered
    Result := (GetVolFilename() <> '');

  end else
  if (checkTab = tsEncDetails) then begin
    // Check that user password is confirmed
    // Note that RNG is always set to something, and others always default
    // Note that the password *can* be blank - e.g. with the NULL encryption
    // cypher
    Result := (frmeNewPassword.IsPasswordValid) and (GetSaltLength() >= 0) and
      (GetKeyIterations() > 0);
  end else
  if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    Result := (GetRNGSet() <> []);
  end else

  if (checkTab = tsRNGPKCS11) then begin
    // Must have at least one RNG selected
    Result := (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0));

  end;
end;



procedure TfrmCreateLUKSVolumeWizard.FormShow(Sender: TObject);
var
  i:  Integer;
  dl: Char;
begin
  inherited;
  //SDUInitAndZeroBuffer(0,fCombinedRandomData);

  { TODO -otdk -crefactor : this should all be in formcreate - but need to have global TOTFEFreeOTFEBase object first }


  // tsPartitionSelect
  // Setup and make sure nothing is selected
  //  fmeSelectPartition.FreeOTFEObj := fFreeOTFEObj;
  // Only list CDROMs if we're creating a keyfile
  //  - CDROMs can't have their passwords changed
  fmeSelectPartition.AllowCDROM := True;
  fmeSelectPartition.OnChange   := fmeSelectPartitionChanged;
  fmeSelectPartition.Tag        := 1;
  //  fmeSelectPartition.Initialize();


  if not fsilent then
    frmeNewPassword.ClearKeyPhrase;

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

  ckRNGPKCS11.Enabled := PKCS11LibraryReady(GetFreeOTFEBase().PKCS11Library);


  // tsRNGPKCS11
  PopulatePKCS11Tokens();



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


  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;

  // DLL doesn't currently support partitions
  if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
    pcWizard.ActivePageIndex := 1;
    SetIsPartition(False);
    // Mark as completed
    rgFileOrPartition.Enabled := False;
  end;


  UpdateUIAfterChangeOnCurrentTab();

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


  SDUTranslateComp(reInstructRNGSelect1);
  SDUTranslateComp(reInstructRNGSelect2);
  SDUTranslateComp(reInstructRNGSelect3);
  SDUTranslateComp(reInstructRNGSelect4);

  SDUTranslateComp(reInstructRNGPKCS11);
end;


procedure TfrmCreateLUKSVolumeWizard.frmeNewPasswordChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateLUKSVolumeWizard.seSaltLengthChange(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateLUKSVolumeWizard.ckRNGClick(Sender: TObject);
begin

  // ...RNG PKCS#11 token
  tsRNGPKCS11.Tag := 1;
  if ckRNGPKCS11.Checked then begin
    if not (FPKCS11TokensAvailable and (cbToken.ItemIndex >= 0)) then begin
      tsRNGPKCS11.Tag := 0;
    end;
  end;

  UpdateUIAfterChangeOnCurrentTab();
end;



function TfrmCreateLUKSVolumeWizard.GetRNGSet(): TRNGSet;
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


procedure TfrmCreateLUKSVolumeWizard.pbFinishClick(Sender: TObject);
var
  allOK:          Boolean;
  saltBytes:      TSDUBytes;
  amsg:           String;
  adrive:         DriveLetterChar;
  arefreshDrives: Boolean;
begin
  inherited;

  GetRandPool.SetUpRandPool(GetRNGSet(),
    PKCS11TokenListSelected(cbToken),
    '');

  try
    // Grab 'n' bytes from the random pool to use as the salt
    GetRandPool.GetRandomData(GetSaltLength() div 8, saltBytes);
    //  salt := SDUBytesToString(saltBytes);
    // do process
    CreateNewLUKS(GetVolFilename(), GetUserkey(), 4 * 1024 * 1024, 'SHA256',
      amsg, adrive, arefreshDrives);


  except
    on E: EInsufficientRandom do begin
      allOK := False;
      SDUMessageDlg(
        _('Insufficient random data generated: ' + E.message) + SDUCRLF +
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



procedure TfrmCreateLUKSVolumeWizard.rgFileOrPartitionClick(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;


function TfrmCreateLUKSVolumeWizard.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = rgFileOrPartition.Items.IndexOf(
    FILEORPART_OPT_PARTITION));
end;

procedure TfrmCreateLUKSVolumeWizard.SetIsPartition(const Value: Boolean);
begin
  if Value then
    rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_PARTITION)
  else
    rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);
end;

procedure TfrmCreateLUKSVolumeWizard.FormWizardStepChanged(Sender: TObject);
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

procedure TfrmCreateLUKSVolumeWizard.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here, note will only close automatically if mr = mrok anyway
  if fsilent then begin
    ModalResult := FSilentResult;
  end;
end;

procedure TfrmCreateLUKSVolumeWizard.FormCreate(Sender: TObject);
begin
  fdeviceList                 := TStringList.Create();
  fdeviceTitle                := TStringList.Create();
  fmeSelectPartition.OnChange := fmeSelectPartitionChanged;

  OnWizardStepChanged := FormWizardStepChanged;
  { done -otdk -crefactor : populate in designer }

  // tsFileOrPartition
  rgFileOrPartition.Items.Clear();
  rgFileOrPartition.Items.Add(FILEORPART_OPT_VOLUME_FILE);
  rgFileOrPartition.Items.Add(FILEORPART_OPT_PARTITION);
  rgFileOrPartition.ItemIndex := rgFileOrPartition.Items.IndexOf(FILEORPART_OPT_VOLUME_FILE);
  frmeNewPassword.OnChange    := frmeNewPasswordChange;
end;

procedure TfrmCreateLUKSVolumeWizard.FormDestroy(Sender: TObject);
begin
  fdeviceList.Free();
  fdeviceTitle.Free();

end;

procedure TfrmCreateLUKSVolumeWizard.PopulatePKCS11Tokens();
begin
  FPKCS11TokensAvailable := (PKCS11PopulateTokenList(GetFreeOTFEBase().PKCS11Library,
    cbToken) > 0);
end;



procedure TfrmCreateLUKSVolumeWizard.pbRefreshClick(Sender: TObject);
begin
  PopulatePKCS11Tokens();
end;

procedure TfrmCreateLUKSVolumeWizard.feVolFilenameChange(Sender: TObject);
begin
  inherited;
  UpdateUIAfterChangeOnCurrentTab();
end;



procedure TfrmCreateLUKSVolumeWizard.fmeSelectPartitionChanged(Sender: TObject);
begin
  UpdateUIAfterChangeOnCurrentTab();
end;



function CreateLUKSVolume(): Boolean;
var
  frmWizard: TfrmCreateLUKSVolumeWizard;
  mr:        Integer;
begin
  Result := False;
  GetFreeOTFEBase().LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;

  GetFreeOTFEBase().CheckActive();

  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    frmWizard := TfrmCreateLUKSVolumeWizard.Create(nil);
    try
      mr     := frmWizard.ShowModal();
      Result := mr = mrOk;
      if Result then
        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_SUCCESS
      else
      if (mr = mrCancel) then
        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
    finally
      frmWizard.Free();
    end;

  end;

end;

end.
