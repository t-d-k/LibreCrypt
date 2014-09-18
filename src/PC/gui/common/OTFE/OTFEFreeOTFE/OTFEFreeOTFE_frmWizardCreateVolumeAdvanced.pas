unit OTFEFreeOTFE_frmWizardCreateVolumeAdvanced;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFEBase_U,
  SDUDialogs, SDUForms,
  SDUStdCtrls, Spin64, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmWizardCreateVolumeAdvanced = class (TSDUForm)
    pcAdvancedOpts:          TPageControl;
    tsSalt:                  TTabSheet;
    seSaltLength:            TSpinEdit64;
    Label15:                 TLabel;
    Label18:                 TLabel;
    tsDriveLetter:           TTabSheet;
    Label1:                  TLabel;
    cbDriveLetter:           TComboBox;
    tsCDBLocation:           TTabSheet;
    rbCDBInVolFile:          TRadioButton;
    rbCDBInKeyfile:          TRadioButton;
    gbKeyfile:               TGroupBox;
    lblKeyFilename:          TSDUFilenameLabel;
    pbBrowseKeyfile:         TButton;
    pbOK:                    TButton;
    pbCancel:                TButton;
    SaveDialog:              TSDUSaveDialog;
    tsKeyIterations:         TTabSheet;
    Label22:                 TLabel;
    seKeyIterations:         TSpinEdit64;
    reInstructSalt:          TOTFEFreeOTFE_InstructionRichEdit;
    reInstructKeyIterations: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructDriveLetter:   TOTFEFreeOTFE_InstructionRichEdit;
    reInstructCDBLocation:   TOTFEFreeOTFE_InstructionRichEdit;
    tsPadding:               TTabSheet;
    reInstructPadding:       TOTFEFreeOTFE_InstructionRichEdit;
    se64Padding:             TSpinEdit64;
    Label2:                  TLabel;
    Label3:                  TLabel;
    tsChaff:                 TTabSheet;
    Label4:                  TLabel;
    rbDataEncrypted:         TRadioButton;
    rbZeros:                 TRadioButton;
    Label5:                  TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure pbBrowseKeyfileClick(Sender: TObject);
    procedure rbCDBLocationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    FCypherBlocksize: Integer;

    function GetKeyIterations(): Integer;
    procedure SetKeyIterations(keyIterations: Integer);
    function GetSaltLength(): Integer;
    procedure SetSaltLength(saltLength: Integer);
    function GetDriveLetter(): Ansichar;
    procedure SetDriveLetter(driveLetter: Ansichar);
    function GetCDBFilename(): String;
    procedure SetCDBFilename(CDBFilename: String);
    function GetPaddingLength(): Int64;
    procedure SetPaddingLength(len: Int64);
    function GetOverwriteWithChaff(): Boolean;
    procedure SetOverwriteWithChaff(secChaff: Boolean);
  PROTECTED
    procedure EnableDisableControls();

  PUBLIC
    // Used in validation
    // Selected cypher blocksize in bits
    property CypherBlocksize: Integer Read FCypherBlocksize Write FCypherBlocksize;

    property KeyIterations: Integer Read GetKeyIterations Write SetKeyIterations;
    property SaltLength: Integer Read GetSaltLength Write SetSaltLength;
    property DriveLetter: Ansichar Read GetDriveLetter Write SetDriveLetter;
    property CDBFilename: String Read GetCDBFilename Write SetCDBFilename;
    property PaddingLength: Int64 Read GetPaddingLength Write SetPaddingLength;
    property OverwriteWithChaff: Boolean Read GetOverwriteWithChaff Write SetOverwriteWithChaff;

  end;


implementation

{$R *.dfm}

uses
  OTFEFreeOTFE_U, SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmWizardCreateVolumeAdvanced.pbBrowseKeyfileClick(Sender: TObject);
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, CDBFilename);
  if SaveDialog.Execute then begin
    CDBFilename := SaveDialog.Filename;
  end;

end;

procedure TfrmWizardCreateVolumeAdvanced.pbOKClick(Sender: TObject);
var
  allOK:     Boolean;
  int64Zero: Int64;
begin
  allOK := True;

  if allOK then begin
    // Ensure number of key iterations is > 0
    if (KeyIterations <= 0) then begin
      SDUMessageDlg(_('The number of key iterations must be 1 or more'), mtError);
      pcAdvancedOpts.ActivePage := tsKeyIterations;
      ActiveControl             := seKeyIterations;
      allOK                     := False;
    end;
  end;

  if allOK then begin
    // Ensure salt length is a multiple of 8 bits
    if ((SaltLength mod 8) <> 0) then begin
      SDUMessageDlg(_('The salt length must be a multiple of 8'), mtError);
      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl             := seSaltLength;
      allOK                     := False;
    end;
    // Ensure salt length is a multiple of the cypher's blocksize
    if ((FCypherBlocksize > 0) and ((SaltLength mod FCypherBlocksize) <> 0)) then begin
      SDUMessageDlg(SDUParamSubstitute(
        _('The salt length must be a multiple of the cypher''s blocksize (%1)'),
        [FCypherBlocksize]),
        mtError);
      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl             := seSaltLength;
      allOK                     := False;
    end;
  end;

  if allOK then begin
    // Either the user's selected to include the CDB in their volume file, or
    // they've specified a keyfilename
    allOK := rbCDBInVolFile.Checked or (rbCDBInKeyfile.Checked and (CDBFilename <> ''));
    if not (allOK) then begin
      SDUMessageDlg(_(
        'If you wish to store the volume''s CDB in a keyfile, you must specify the file to use'), mtError);
      pcAdvancedOpts.ActivePage := tsCDBLocation;
      allOK                     := False;
    end;
  end;

  if allOK then begin
    // Padding can't be less than zero
    int64Zero := 0;
    allOK     := (se64Padding.Value >= int64Zero);
    if not (allOK) then begin
      SDUMessageDlg(_('The amount of padding data added to a volume must be zero or greater'),
        mtError);
      pcAdvancedOpts.ActivePage := tsPadding;
      allOK                     := False;
    end;
  end;


  if allOK then begin
    ModalResult := mrOk;
  end;

end;


procedure TfrmWizardCreateVolumeAdvanced.rbCDBLocationClick(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmWizardCreateVolumeAdvanced.GetKeyIterations(): Integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetKeyIterations(keyIterations: Integer);
begin
  seKeyIterations.Value := keyIterations;
end;

function TfrmWizardCreateVolumeAdvanced.GetSaltLength(): Integer;
begin
  Result := seSaltLength.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetSaltLength(saltLength: Integer);
begin
  seSaltLength.Value := saltLength;
end;

function TfrmWizardCreateVolumeAdvanced.GetDriveLetter(): Ansichar;
var
  driveLetter: Ansichar;
begin
  driveLetter := #0;
  if (cbDriveLetter.ItemIndex > 0) then begin
    driveLetter := Ansichar(cbDriveLetter.Items[cbDriveLetter.ItemIndex][1]);
  end;

  Result := driveLetter;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetDriveLetter(driveLetter: Ansichar);
begin
  cbDriveLetter.ItemIndex := 0;
  if (driveLetter <> #0) then begin
    cbDriveLetter.ItemIndex := cbDriveLetter.Items.IndexOf(driveLetter + ':');
  end;
end;

procedure TfrmWizardCreateVolumeAdvanced.FormCreate(Sender: TObject);
var
  dl: ansichar;
begin
  CypherBlocksize := 0;

  // tsKeyIterations
  seKeyIterations.MinValue  := 1;
  seKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;

  // tsSalt
  seSaltLength.Increment := 8;

  // tsRequestedDriveLetter
  cbDriveLetter.Items.Clear();
  cbDriveLetter.Items.Add(_('Use default'));
  //  for dl:='C' to 'Z' do
  for dl := 'A' to 'Z' do begin
    cbDriveLetter.Items.Add(dl + ':');
  end;
  // Autoselect the "default" (first) entry
  cbDriveLetter.ItemIndex := 0;

  // tsCDBLocation
  rbCDBInKeyfile.Checked := False;
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := '';


  // Set to first page
  pcAdvancedOpts.ActivePage := tsKeyIterations;
end;

procedure TfrmWizardCreateVolumeAdvanced.FormShow(Sender: TObject);
begin
  reInstructKeyIterations.Text :=
    _('Please enter the number of PBKDF2 key iterations to be carried out on your password to generate an encryption key'
    + SDUCRLF + SDUCRLF +
    'The higher the number, the greater the security offered - but the slower mounting will be (especially if mounting the volume on a PDA)'
    + SDUCRLF + SDUCRLF +
    'Note: If this value is changed from its default, you will be required to enter it value every time you mount your volume. For this reason it is recommended that most users leave it at its default value, unless there is a particular need to do otherwise.');

  reInstructSalt.Text :=
    _('Please enter the amount of "salt" to be combined with your password to generate an encryption key.'
    + SDUCRLF + SDUCRLF +
    '"Salting" your password increases security, and will slow down dictionary attacks against your volume.'
    + SDUCRLF + SDUCRLF +
    'Note: The value entered must be a multiple of both 8 and the cypher''s blocksize.' +
    SDUCRLF + SDUCRLF +
    'Note: If this value is changed from its default, you will be required to enter it value every time you mount your volume. For this reason it is recommended that most users leave it at its default value, unless there is a particular need to do otherwise.');

  reInstructDriveLetter.Text :=
    _('Please enter the drive letter you would normally like this volume mounted under.' +
    SDUCRLF + SDUCRLF + 'Note: You will be able to change this after the volume has been created.');

  reInstructCDBLocation.Text :=
    _('Please specify if you would like your volume''s critical data block (CDB) to be written at the start of the new volume, or stored in a separate keyfile.' + SDUCRLF + SDUCRLF + 'The CDB is where metadata about your volume is stored, together with the master key used to carry out the actual encryption/decryption of your data.' + SDUCRLF + SDUCRLF + 'For most users, including the CDB as part of the volume file is probably the best option.' + SDUCRLF + SDUCRLF + 'If you store this information in a keyfile, you will need both your volume file AND a keyfile in order to mount your volume.' + SDUCRLF + SDUCRLF + 'Note: Storing the CDB as part of the volume file will increase your volume''s size by 512 bytes.');

  reInstructPadding.Text :=
    _('"Padding" is additional random data added to the end of a volume file. Any padding added will not be available for use as part of the mounted volume, and serves to increase the size of the volume.' + SDUCRLF + SDUCRLF + 'Encrypted volumes typically have a file size that is a multiple of 512 bytes, or a "signature size" beyond the last 1MB boundry. To prevent this, you may wish to append random "padding" data to the new volume.' + SDUCRLF + SDUCRLF + 'Padding also reduces the amount of information available to an attacker with respect to the maximum amount of the encrypted that may actually be held within the volume.');

end;

function TfrmWizardCreateVolumeAdvanced.GetCDBFilename(): String;
var
  retval: String;
begin
  retval := '';
  if rbCDBInKeyfile.Checked then begin
    retval := trim(lblKeyFilename.Caption);
  end;

  Result := retval;
end;



procedure TfrmWizardCreateVolumeAdvanced.SetCDBFilename(CDBFilename: String);
begin
  rbCDBInVolFile.Checked := True;
  lblKeyFilename.Caption := CDBFilename;
  if (CDBFilename <> '') then begin
    rbCDBInKeyfile.Checked := True;
  end;

  EnableDisableControls();

end;


function TfrmWizardCreateVolumeAdvanced.GetPaddingLength(): Int64;
begin
  Result := se64Padding.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetPaddingLength(len: Int64);
begin
  se64Padding.Value := len;
end;

function TfrmWizardCreateVolumeAdvanced.GetOverwriteWithChaff(): Boolean;
begin
  Result := rbDataEncrypted.Checked;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetOverwriteWithChaff(secChaff: Boolean);
begin
  rbDataEncrypted.Checked := not (secChaff);
  rbDataEncrypted.Checked := secChaff;
end;

procedure TfrmWizardCreateVolumeAdvanced.EnableDisableControls();
begin
  SDUEnableControl(gbKeyfile, rbCDBInKeyfile.Checked);
  { TODO 1 -otdk -crefactor : disable chaff if hidden - not used }
end;

end.
