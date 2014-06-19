unit OTFEFreeOTFE_frmWizardCreateVolumeAdvanced;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SDUStdCtrls, ComCtrls, Spin64, SDUForms,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_InstructionRichEdit, SDUDialogs;

type
  TfrmWizardCreateVolumeAdvanced = class(TSDUForm)
    pcAdvancedOpts: TPageControl;
    tsSalt: TTabSheet;
    seSaltLength: TSpinEdit64;
    Label15: TLabel;
    Label18: TLabel;
    tsDriveLetter: TTabSheet;
    Label1: TLabel;
    cbDriveLetter: TComboBox;
    tsCDBLocation: TTabSheet;
    rbCDBInVolFile: TRadioButton;
    rbCDBInKeyfile: TRadioButton;
    gbKeyfile: TGroupBox;
    lblKeyFilename: TSDUFilenameLabel;
    pbBrowseKeyfile: TButton;
    pbOK: TButton;
    pbCancel: TButton;
    SaveDialog: TSDUSaveDialog;
    tsKeyIterations: TTabSheet;
    Label22: TLabel;
    seKeyIterations: TSpinEdit64;
    reInstructSalt: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructKeyIterations: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructDriveLetter: TOTFEFreeOTFE_InstructionRichEdit;
    reInstructCDBLocation: TOTFEFreeOTFE_InstructionRichEdit;
    tsPadding: TTabSheet;
    reInstructPadding: TOTFEFreeOTFE_InstructionRichEdit;
    rbDataEncrypted: TRadioButton;
    rbDataPseudorandom: TRadioButton;
    se64Padding: TSpinEdit64;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure pbBrowseKeyfileClick(Sender: TObject);
    procedure rbCDBLocationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCypherBlocksize: integer;

    function  GetKeyIterations(): integer;
    procedure SetKeyIterations(keyIterations: integer);
    function  GetSaltLength(): integer;
    procedure SetSaltLength(saltLength: integer);
    function  GetDriveLetter(): char;
    procedure SetDriveLetter(driveLetter: char);
    function  GetCDBFilename(): string;
    procedure SetCDBFilename(CDBFilename: string);
    function  GetPaddingLength(): int64;
    procedure SetPaddingLength(len: int64);
    function  GetPadWithEncryptedData(): boolean;
    procedure SetPadWithEncryptedData(encryptPadding: boolean);
  protected
    procedure EnableDisableControls();

  public
    // Used in validation
    // Selected cypher blocksize in bits
    property CypherBlocksize: integer read FCypherBlocksize write FCypherBlocksize;

    property KeyIterations: integer read GetKeyIterations write SetKeyIterations;
    property SaltLength: integer read GetSaltLength write SetSaltLength;
    property DriveLetter: char read GetDriveLetter write SetDriveLetter;
    property CDBFilename: string read GetCDBFilename write SetCDBFilename;
    property PaddingLength: int64 read GetPaddingLength write SetPaddingLength;
    property PadWithEncryptedData: boolean read GetPadWithEncryptedData write SetPadWithEncryptedData;
  end;


implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  OTFEFreeOTFE_U;

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
  SaveDialog.Options := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, CDBFilename);
  if SaveDialog.execute then
    begin
    CDBFilename := SaveDialog.Filename;
    end;

end;

procedure TfrmWizardCreateVolumeAdvanced.pbOKClick(Sender: TObject);
var
  allOK: boolean;
  int64Zero: int64;
begin
  allOK:= TRUE;

  if allOK then
    begin
    // Ensure number of key iterations is > 0
    if (KeyIterations <= 0) then
      begin
      SDUMessageDlg(_('The number of key iterations must be 1 or more'), mtError);
      pcAdvancedOpts.ActivePage := tsKeyIterations;
      ActiveControl := seKeyIterations;
      allOK := FALSE
      end;
    end;

  if allOK then
    begin
    // Ensure salt length is a multiple of 8 bits
    if ((SaltLength mod 8) <> 0) then
      begin
      SDUMessageDlg(_('The salt length must be a multiple of 8'), mtError);
      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl := seSaltLength;
      allOK := FALSE
      end;
    // Ensure salt length is a multiple of the cypher's blocksize
    if (
        (FCypherBlocksize > 0) and
        ((SaltLength mod FCypherBlocksize) <> 0)
       ) then
      begin
      SDUMessageDlg(SDUParamSubstitute(
                                       _('The salt length must be a multiple of the cypher''s blocksize (%1)'),
                                       [FCypherBlocksize]
                                      ), mtError);
      pcAdvancedOpts.ActivePage := tsSalt;
      ActiveControl := seSaltLength;
      allOK := FALSE
      end;
    end;

  if allOK then
    begin
    // Either the user's selected to include the CDB in their volume file, or
    // they've specified a keyfilename
    allOK := rbCDBInVolFile.checked OR (rbCDBInKeyfile.checked AND (CDBFilename <> ''));
    if not(allOK) then
      begin
      SDUMessageDlg(_('If you wish to store the volume''s CDB in a keyfile, you must specify the file to use'), mtError);
      pcAdvancedOpts.ActivePage := tsCDBLocation;
      allOK := FALSE
      end;
    end;

  if allOK then
    begin
    // Padding can't be less than zero
    int64Zero := 0;
    allOK := (se64Padding.Value >= int64Zero);
    if not(allOK) then
      begin
      SDUMessageDlg(_('The amount of padding data added to a volume must be zero or greater'), mtError);
      pcAdvancedOpts.ActivePage := tsPadding;
      allOK := FALSE
      end;
    end;


  if allOK then
    begin
    ModalResult := mrOK;
    end;

end;


procedure TfrmWizardCreateVolumeAdvanced.rbCDBLocationClick(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmWizardCreateVolumeAdvanced.GetKeyIterations(): integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetKeyIterations(keyIterations: integer);
begin
  seKeyIterations.Value := keyIterations;
end;

function TfrmWizardCreateVolumeAdvanced.GetSaltLength(): integer;
begin
  Result := seSaltLength.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetSaltLength(saltLength: integer);
begin
  seSaltLength.Value := saltLength;
end;

function TfrmWizardCreateVolumeAdvanced.GetDriveLetter(): char;
var
  driveLetter: char;
begin
  driveLetter := #0;
  if (cbDriveLetter.ItemIndex > 0) then
    begin
    driveLetter := cbDriveLetter.Items[cbDriveLetter.ItemIndex][1];
    end;

  Result := driveLetter;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetDriveLetter(driveLetter: char);
begin
  cbDriveLetter.ItemIndex := 0;
  if (driveLetter <> #0) then
    begin
    cbDriveLetter.ItemIndex := cbDriveLetter.Items.IndexOf(driveLetter+':');
    end;
end;

procedure TfrmWizardCreateVolumeAdvanced.FormCreate(Sender: TObject);
var
  dl: char;
begin
  CypherBlocksize := 0;

  // tsKeyIterations
  seKeyIterations.MinValue := 1;
  seKeyIterations.MaxValue := 999999; // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;

  // tsSalt
  seSaltLength.Increment := 8;

  // tsRequestedDriveLetter
  cbDriveLetter.Items.Clear();
  cbDriveLetter.Items.Add(_('Use default'));
//  for dl:='C' to 'Z' do
  for dl:='A' to 'Z' do
    begin
    cbDriveLetter.Items.Add(dl+':');
    end;
  // Autoselect the "default" (first) entry
  cbDriveLetter.ItemIndex := 0;

  // tsCDBLocation
  rbCDBInKeyfile.checked := FALSE;
  rbCDBInVolFile.checked := TRUE;
  lblKeyFilename.Caption := '';


  // Set to first page
  pcAdvancedOpts.ActivePage := tsKeyIterations;
end;

procedure TfrmWizardCreateVolumeAdvanced.FormShow(Sender: TObject);
begin
  reInstructKeyIterations.Text :=
    _('Please enter the number of PBKDF2 key iterations to be carried out on your password to generate an encryption key'+SDUCRLF+
      SDUCRLF+
      'The higher the number, the greater the security offered - but the slower mounting will be (especially if mounting the volume on a PDA)'+SDUCRLF+
      SDUCRLF+
      'Note: If this value is changed from its default, you will be required to enter it value every time you mount your volume. For this reason it is recommended that most users leave it at its default value, unless there is a particular need to do otherwise.');

  reInstructSalt.Text :=
    _('Please enter the amount of "salt" to be combined with your password to generate an encryption key.'+SDUCRLF+
      SDUCRLF+
      '"Salting" your password increases security, and will slow down dictionary attacks against your volume.'+SDUCRLF+
      SDUCRLF+
      'Note: The value entered must be a multiple of both 8 and the cypher''s blocksize.'+SDUCRLF+
      SDUCRLF+
      'Note: If this value is changed from its default, you will be required to enter it value every time you mount your volume. For this reason it is recommended that most users leave it at its default value, unless there is a particular need to do otherwise.');

  reInstructDriveLetter.Text :=
    _('Please enter the drive letter you would normally like this volume mounted under.'+SDUCRLF+
      SDUCRLF+
      'Note: You will be able to change this after the volume has been created.');

  reInstructCDBLocation.Text :=
    _('Please specify if you would like your volume''s critical data block (CDB) to be written at the start of the new volume, or stored in a separate keyfile.'+SDUCRLF+
      SDUCRLF+
      'The CDB is where metadata about your volume is stored, together with the master key used to carry out the actual encryption/decryption of your data.'+SDUCRLF+
      SDUCRLF+
      'For most users, including the CDB as part of the volume file is probably the best option.'+SDUCRLF+
      SDUCRLF+
      'If you store this information in a keyfile, you will need both your volume file AND a keyfile in order to mount your volume.'+SDUCRLF+
      SDUCRLF+
      'Note: Storing the CDB as part of the volume file will increase your volume''s size by 512 bytes.');

  reInstructPadding.Text :=
    _('"Padding" is additional random data added to the end of a volume file. Any padding added will not be available for use as part of the mounted volume, and serves to increase the size of the volume.'+SDUCRLF+
      SDUCRLF+
      'Encrypted volumes typically have a file size that is a multiple of 512 bytes, or a "signature size" beyond the last 1MB boundry. To prevent this, you may wish to append random "padding" data to the new volume.'+SDUCRLF+
      SDUCRLF+
      'Padding also reduces the amount of information available to an attacker with respect to the maximum amount of the encrypted that may actually be held within the volume.');

end;

function TfrmWizardCreateVolumeAdvanced.GetCDBFilename(): string;
var
  retval: string;
begin
  retval := '';
  if rbCDBInKeyfile.checked then
    begin
    retval := trim(lblKeyFilename.caption);
    end;

  Result := retval;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetCDBFilename(CDBFilename: string);
begin
  rbCDBInVolFile.checked := TRUE;
  lblKeyFilename.caption := CDBFilename;
  if (CDBFilename <> '') then
    begin
    rbCDBInKeyfile.checked := TRUE;
    end;

  EnableDisableControls();

end;

function TfrmWizardCreateVolumeAdvanced.GetPaddingLength(): int64;
begin
  Result := se64Padding.Value;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetPaddingLength(len: int64);
begin
  se64Padding.Value := len;
end;

function TfrmWizardCreateVolumeAdvanced.GetPadWithEncryptedData(): boolean;
begin
  Result := rbDataEncrypted.checked;
end;

procedure TfrmWizardCreateVolumeAdvanced.SetPadWithEncryptedData(encryptPadding: boolean);
begin
  rbDataPseudorandom.checked := not(encryptPadding);
  rbDataEncrypted.checked := encryptPadding;
end;

procedure TfrmWizardCreateVolumeAdvanced.EnableDisableControls();
begin
  SDUEnableControl(gbKeyfile, rbCDBInKeyfile.checked);
end;

END.


