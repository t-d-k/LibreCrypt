unit frmCreateLUKSVolumeWizard;
// Description:
// (c) tdk
// dual GPL and freeotfe licence

interface

uses
  // delphi & libs
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages,
  Spin64, StdCtrls, SysUtils, Windows,
  // sdu & LibreCrypt utils
  PasswordRichEdit, lcDialogs, sdurandpool, SDUStdCtrls, SDUGeneral, SDUForms,
  SDUFrames, SDUSpin64Units, SDUDialogs, SDUFilenameEdit_U,
  lcTypes, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_PasswordRichEdit,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  DriverAPI, // Required for CRITICAL_DATA_LEN

  // LibreCrypt forms
  fmeSDUDiskPartitions, fmeSelectPartition,
  frmWizard,
  fmeSDUBlocks,
  fmeNewPassword, fmeContainerSize;

type

  TfrmCreateLUKSVolumeWizard = class(TfrmWizard)
    tsEncDetails: TTabSheet;
    tsRNGSelect: TTabSheet;
    reInstructRNGSelect1: TLabel;
    Label5: TLabel;
    seDestSaltLength: TSpinEdit64;
    Label7: TLabel;
    cbDestDriveLetter: TComboBox;
    Label12: TLabel;
    tsVolFile: TTabSheet;
    tsPartitionSelect: TTabSheet;
    reInstructPartitionSelect: TLabel;
    Label21: TLabel;
    seDestKeyIterations: TSpinEdit64;
    Label8: TLabel;
    gbRNG: TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI: TCheckBox;
    ckRNGcryptlib: TCheckBox;
    ckRNGGPG: TCheckBox;
    ckRNGPKCS11: TCheckBox;
    tsRNGPKCS11: TTabSheet;
    reInstructRNGPKCS11: TLabel;
    cbToken: TComboBox;
    lblToken: TLabel;
    pbRefresh: TButton;
    fmeSelectPartition: TfmeSelectPartition;
    frmeNewPassword: TfrmeNewPassword;
    lblInstructDestDetails: TLabel;
    reInstructRNGSelect2: TLabel;
    reInstructRNGSelect3: TLabel;
    reInstructRNGSelect4: TLabel;
    GroupBox1: TGroupBox;
    rgFileOrPartition: TRadioGroup;
    feVolFilename: TSDUFilenameEdit;
    tsSize: TTabSheet;
    fmeContainerSize1: TTfmeContainerSize;
    lblFileInstruct: TLabel;
    lblInstruct2: TLabel;
    lblInstruct3: TLabel;
    lblFileExistsWarning: TLabel;

    procedure FormShow(Sender: TObject);
    procedure seSaltLengthChange(Sender: TObject);
    procedure ckRNGClick(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);


    procedure rgFileOrPartitionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject;var Action: TCloseAction);

    procedure feVolFilenameChange(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure feVolFilenameedFilenameChange(Sender: TObject);

  private

    fDeviceList:            TStringList;
    fDeviceTitle:           TStringList;

    fPKCS11TokensAvailable: Boolean;

    function GetRNGSet(): TRNGSet;

    function GetIsPartition(): Boolean;
    function GetVolFilename(): string;
    procedure SetVolFilename(const Value: string);


    function GetUserKey(): TSDUBytes;
    function GetSaltLength(): Integer;
    function GetKeyIterations(): Integer;
    function GetRequestedDriveLetter(): DriveLetterChar;
    procedure SetUserKey(const Value: TSDUBytes);
    procedure SetIsPartition(const Value: Boolean);
    function GetSize: ULONGLONG;

    procedure _PopulatePKCS11Tokens();

  protected
    fsilent:       Boolean;
    fsilentResult: TModalResult;

    procedure _EnableDisableControls();override;

    function _IsTabSkipped(tabSheet: TTabSheet): Boolean;override;

    procedure _FormWizardStepChanged(Sender: TObject);
    function _IsTabComplete(checkTab: TTabSheet): Boolean;override;

  public

    procedure fmeSelectPartitionChanged(Sender: TObject);

  published
    property silent:      Boolean read fsilent write fsilent;
    property isPartition: Boolean read GetIsPartition write SetIsPartition;
    property UserKey:     TSDUBytes read GetUserKey write SetUserKey;

  end;

function CreateLUKSVolume(out user_canceled: Boolean): Boolean;

implementation

{$R *.DFM}


uses
  // delphi
  MSCryptoAPI,

  // sdu, lcutils
  OTFEFreeOTFEDLL_U, OTFEConsts_U,
  PKCS11Lib, SDUi18n, pkcs11_library,
  lcConsts, LUKSTools
  // LibreCrypt forms

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
  FILEORPART_OPT_PARTITION_INDEX   = 1;
  // index of 'partition' item in rgFileOrPartition
  FILEORPART_OPT_VOLUME_FILE_INDEX = 0; // index of 'file' item


  { TODO 2 -otdk -crefactor : this uses a lot of same code and GUI as create volume - turn into frames }
function TfrmCreateLUKSVolumeWizard.GetVolFilename(): string;
begin
  if GetIsPartition() then begin
    Result := fmeSelectPartition.SelectedDevice;
  end else begin
    Result := feVolFilename.Filename;
  end;
end;

procedure TfrmCreateLUKSVolumeWizard.SetVolFilename(const Value: string);
begin
  if GetIsPartition() then begin
    // fmeSelectPartition.SelectedDevice := Value;
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
  Result   := #0;
  if (cbDestDriveLetter.ItemIndex > 0) then begin
    Result := cbDestDriveLetter.Items[cbDestDriveLetter.ItemIndex][1];
  end;
end;

// Returns TRUE if the specified tab should be skipped in 'next' progression, otherwise FALSE
function TfrmCreateLUKSVolumeWizard._IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := inherited _IsTabSkipped(tabSheet);

  // If the user isn't creating a volume within a volume file, skip that tab
  if (tabSheet = tsVolFile) then begin
    Result := GetIsPartition();
  end;

  // If the user isn't creating a volume on a partition, skip that tab
  if (tabSheet = tsPartitionSelect) then begin
    Result := not(GetIsPartition());
  end;


  // If the user *isn't* using the PKCS#11 token RNG, skip the tsRNGPKCS11 tabsheet
  if (tabSheet = tsRNGPKCS11) then
      Result := not(rngPKCS11 in GetRNGSet());
end;


procedure TfrmCreateLUKSVolumeWizard._EnableDisableControls();
begin
  inherited;
  fmeContainerSize1.EnableDisableControls(GetIsPartition(), False,
    fmeSelectPartition.SyntheticDriveLayout);

  feVolFilename.Visible := not GetIsPartition();
end;

function TfrmCreateLUKSVolumeWizard.GetIsPartition(): Boolean;
begin
  Result := (rgFileOrPartition.ItemIndex = FILEORPART_OPT_PARTITION_INDEX);
end;


function TfrmCreateLUKSVolumeWizard.GetSize(): ULONGLONG;
begin
  if fmeContainerSize1.GetIsSizeEntirePartitionDisk() then begin
    Result := fmeSelectPartition.SelectedSize();

  end else begin

    Result := fmeContainerSize1.GetSize();
  end;
end;

// This procedure will check to see if all items on the current tabsheet have
// been successfully completed . only called for not skipped tabs
function TfrmCreateLUKSVolumeWizard._IsTabComplete
  (checkTab: TTabSheet): Boolean;
var
  volSize: ULONGLONG;
  // randomBitsGenerated: Integer;
begin
  inherited;

  Result := False;

  if (checkTab = tsVolFile) then begin

    // Ensure one option has been selected
    Result := (rgFileOrPartition.ItemIndex >= 0);
    if Result then begin
      // Flag tabsheet complete if a valid filename has been specified
      if (GetIsPartition()) then begin
        Result                       := True;
      end else begin
        lblFileExistsWarning.Visible := FileExists(GetVolFilename);
        Result := (GetVolFilename() <> '') and not FileExists(GetVolFilename());
      end;
    end;

  end else if (checkTab = tsPartitionSelect) then begin
    // If we're creating a volume on a partition, one must be selected
    if (not(GetIsPartition())) then begin
      Result := True;
    end else begin
      Result := (fmeSelectPartition.SelectedDevice <> '');
    end;

  end else if (checkTab = tsEncDetails) then begin
    // Check that user password is confirmed
    // Note that RNG is always set to something, and others always default
    // Note that the password *can* be blank - e.g. with the NULL encryption
    // cypher
    Result := (frmeNewPassword.IsPasswordValid) and (GetSaltLength() >= 0) and
      (GetKeyIterations() > 0);
  end else if (checkTab = tsRNGSelect) then begin
    // Must have at least one RNG selected
    Result := (GetRNGSet() <> []);
  end else
    if (checkTab = tsRNGPKCS11) then begin
    // Must have at least one RNG selected
    Result := (fPKCS11TokensAvailable and (cbToken.ItemIndex >= 0));

  end else if (checkTab = tsSize) then begin


    volSize := GetSize();
    // minimum volume size = 1mb
    Result  := volSize >= BYTES_IN_MEGABYTE;

    if Result then
      // mutiple of sectors
        Result := (volSize mod ASSUMED_HOST_SECTOR_SIZE) = 0;

    if Result then begin

      if GetIsPartition() then begin
        if fmeSelectPartition.SyntheticDriveLayout then begin
          // We don't know the size of the selected drive/partition; assume user
          // input OK
          Result := True;
        end else begin
          // Note: fmeSelectPartition.SelectedSize() already takes into account
          // whether or not the "entire disk" option was selected on the
          // frame
          Result := fmeSelectPartition.SelectedSize() >= volSize;
        end;
      end
      else
        // if (FileExists(GetVolFilename)) then begin
        // // If creating a hidden container, ensure that the existing file is large
        // // enough to store the hidden container
        // Result := ((SDUGetFileSize(GetVolFilename) - GetOffset()) >= volSizeWithCDBAndPadding);
        // end else begin
        // It would be *nice* to check that the volume the filename is stored on
        // has enough storage for the requested size volume, but that is not
        // a priority to implement right now...

        // Always completed (seSize always returns a minimum of "1", and the
        // units must always be set to something)
          Result := True;
      // end;
    end;

  end
  else assert(False, 'add tab');
end;


procedure TfrmCreateLUKSVolumeWizard.FormShow(Sender: TObject);
var
  i:  Integer;
  dl: Char;
begin
  inherited;
  // SDUInitAndZeroBuffer(0,fCombinedRandomData);

  { TODO -otdk -crefactor : review what needs to be in formcreate }


  // tsPartitionSelect
  // Setup and make sure nothing is selected
  // Only list CDROMs if we're creating a keyfile
  // - CDROMs can't have their passwords changed
  fmeSelectPartition.AllowCDROM := True;
  fmeSelectPartition.OnChange   := fmeSelectPartitionChanged;
  fmeSelectPartition.Tag        := 1;
  fmeContainerSize1.IsHidden    := False;

  if not fsilent then
      frmeNewPassword.ClearKeyPhrase;

  // tsDestDetails
  //
  // if not fsilent then
  // preDestUserKey1.Lines.Clear();
  //
  // if not fsilent then
  // preDestUserKey2.Lines.Clear();

  seDestSaltLength.Increment    := 8;
  seDestSaltLength.Value        := DEFAULT_SALT_LENGTH;

  seDestKeyIterations.MinValue  := 1;
  seDestKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seDestKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seDestKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

  cbDestDriveLetter.Items.Clear();
  cbDestDriveLetter.Items.Add(_('Use default'));
  // for dl:='C' to 'Z' do
  for dl := 'A' to 'Z' do
      cbDestDriveLetter.Items.Add(dl + ':');

  // Autoselect the "default" (first) entry
  cbDestDriveLetter.ItemIndex := 0;

  // tsRNGSelect
  ckRNGCryptoAPI.Checked      := True;
  ckRNGcryptlib.Enabled       := GetRandPool().CanUseCryptlib;
  if ckRNGcryptlib.Enabled then
      ckRNGcryptlib.Checked   := True;

  ckRNGPKCS11.Enabled         := PKCS11LibraryReady(GPKCS11Library);


  // tsRNGPKCS11
  _PopulatePKCS11Tokens();


  // Set all tabs to show that they have not yet been completed
  for i                          := 0 to (pcWizard.PageCount - 1) do begin
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
    pcWizard.ActivePageIndex  := 1;
    SetIsPartition(False);
    // Mark as completed
    rgFileOrPartition.Enabled := False;
  end;


  _UpdateUIAfterChangeOnCurrentTab();

  if fsilent then begin
    ModalResult   := mrCancel;
    pbFinishClick(self);
    fsilentResult := ModalResult;
    // if testing and no errors, then close dlg
    if ModalResult = mrOk then
        PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

  // tag is used as flag if translated already
  if self.Tag = 0 then
      self.Caption := SDUTranslate(self.Caption);
  self.Tag         := 1;


  SDUTranslateComp(reInstructRNGSelect1);
  SDUTranslateComp(reInstructRNGSelect2);
  SDUTranslateComp(reInstructRNGSelect3);
  SDUTranslateComp(reInstructRNGSelect4);

  SDUTranslateComp(reInstructRNGPKCS11);


end;


procedure TfrmCreateLUKSVolumeWizard.ControlChanged(Sender: TObject);
begin


  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateLUKSVolumeWizard.seSaltLengthChange(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateLUKSVolumeWizard.ckRNGClick(Sender: TObject);
begin

  // ...RNG PKCS#11 token
  tsRNGPKCS11.Tag     := 1;
  if ckRNGPKCS11.Checked then begin
    if not(fPKCS11TokensAvailable and (cbToken.ItemIndex >= 0)) then begin
      tsRNGPKCS11.Tag := 0;
    end;
  end;

  _UpdateUIAfterChangeOnCurrentTab();
end;


function TfrmCreateLUKSVolumeWizard.GetRNGSet(): TRNGSet;
begin
  Result   := [];

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
  amsg:           string;
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
    // salt := SDUBytesToString(saltBytes);
    // do process
    CreateNewLUKS(GetVolFilename(), GetUserKey(), GetSize(), 'SHA256', amsg,
      adrive, arefreshDrives);
    allOK := True;

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
      _('Unable to complete requested operation: please ensure that your container is not already open or otherwise in use'
          +
          ', and that the keyphrase, salt and offset entered are correct.'),
      mtError
      );
  end;

  { TODO 1 -otdk -ccleanup : whats this for? }
  (* for i := 1 to length(saltBytes) do begin
    saltBytes[i] := AnsiChar(i);
    end; *)

  SDUInitAndZeroBuffer(0, saltBytes);
end;


procedure TfrmCreateLUKSVolumeWizard.rgFileOrPartitionClick(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();
end;


procedure TfrmCreateLUKSVolumeWizard.SetIsPartition(const Value: Boolean);
begin
  if Value then
      rgFileOrPartition.ItemIndex := FILEORPART_OPT_PARTITION_INDEX
  else
      rgFileOrPartition.ItemIndex := FILEORPART_OPT_VOLUME_FILE_INDEX;
end;

procedure TfrmCreateLUKSVolumeWizard._FormWizardStepChanged(Sender: TObject);
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
    ModalResult := fsilentResult;
  end;
end;

procedure TfrmCreateLUKSVolumeWizard.FormCreate(Sender: TObject);
begin
  fDeviceList                    := TStringList.Create();
  fDeviceTitle                   := TStringList.Create();
  fmeSelectPartition.OnChange    := fmeSelectPartitionChanged;
  feVolFilename.FilenameEditType := fetSave;
  feVolFilename.OnChange         := ControlChanged;
  OnWizardStepChanged            := _FormWizardStepChanged;
  { done -otdk -crefactor : populate in designer }

  // tsFileOrPartition
  rgFileOrPartition.ItemIndex := FILEORPART_OPT_VOLUME_FILE_INDEX;
  frmeNewPassword.OnChange    := ControlChanged;
  fmeContainerSize1.OnChange  := ControlChanged;
  fmeContainerSize1.IsHidden  := False;
  fmeContainerSize1.Initialise;
  { values used
    cypher: aes 256
    hash: SHA256
    number of AF stripes    = 2
    cipher mode = 'xts-plain64'
    master key digest iterations = 1000
    sector IV Gen Method         = 32 bit sector ID
    base IV Cypher On Hash Length =  True
    key Slot = 0
  }
end;

procedure TfrmCreateLUKSVolumeWizard.FormDestroy(Sender: TObject);
begin
  fDeviceList.Free();
  fDeviceTitle.Free();
end;

procedure TfrmCreateLUKSVolumeWizard._PopulatePKCS11Tokens();
begin
  fPKCS11TokensAvailable := (PKCS11PopulateTokenList(GPKCS11Library,
      cbToken) > 0);
end;

procedure TfrmCreateLUKSVolumeWizard.pbRefreshClick(Sender: TObject);
begin
  _PopulatePKCS11Tokens();
end;

procedure TfrmCreateLUKSVolumeWizard.feVolFilenameChange(Sender: TObject);
begin
  inherited;
  _UpdateUIAfterChangeOnCurrentTab();
end;

procedure TfrmCreateLUKSVolumeWizard.feVolFilenameedFilenameChange(
  Sender: TObject);
begin
  inherited;
  feVolFilename.edFilenameChange(Sender);

end;

procedure TfrmCreateLUKSVolumeWizard.fmeSelectPartitionChanged(Sender: TObject);
begin
  _UpdateUIAfterChangeOnCurrentTab();

  fmeContainerSize1.SetPartitionSize(fmeSelectPartition.SelectedSize());

  // Size tab must be marked as incomplete
  tsSize.Tag := 0;

  _UpdateUIAfterChangeOnCurrentTab();
end;

function CreateLUKSVolume(out user_canceled: Boolean): Boolean;
var
  frmWizard: TfrmCreateLUKSVolumeWizard;
  mr:        Integer;
begin
  Result        := False;
  user_canceled := False;

  GetFreeOTFEBase().CheckActive();

  if GetFreeOTFEBase().WarnIfNoHashOrCypherDrivers() then begin
    frmWizard       := TfrmCreateLUKSVolumeWizard.Create(nil);
    try
      mr            := frmWizard.ShowModal();
      Result        := mr = mrOk;
      user_canceled := mr = mrCancel;
    finally
      frmWizard.Free();
    end;

  end;

end;

end.
