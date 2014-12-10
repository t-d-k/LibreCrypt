unit FreeOTFEfrmSelectOverwriteMethod;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, ComCtrls, Controls, Dialogs,
  Forms, Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_U, SDUForms, StdCtrls,
  SysUtils, Windows;

type
  TfrmFreeOTFESelectOverwriteMethod = class (TSDUForm)
    pbOK:                    TButton;
    pbCancel:                TButton;
    GroupBox1:               TGroupBox;
    rbDataEncrypted:         TRadioButton;
    rbDataPseudorandom:      TRadioButton;
    cbCypher:                TComboBox;
    pbCypherDetails:         TButton;
    reInstructOverwriteType: TOTFEFreeOTFE_InstructionRichEdit;
    procedure pbOKClick(Sender: TObject);
    procedure pbCypherDetailsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ControlChanged(Sender: TObject);
  PRIVATE
    cypherKernelModeDriverNames: TStringList;
    cypherGUIDs:                 TStringList;

    procedure EnableDisableControls();
  PUBLIC
    OTFEFreeOTFEObj: TOTFEFreeOTFE;

    OverwriteWithEncryptedData: Boolean;
    CypherDriver:               Ansistring;
    CypherGUID:                 TGUID;
  end;


implementation

{$R *.DFM}

uses
  ComObj,  // Required for StringToGUID
  SDUDialogs,
  SDUGeneral, SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmFreeOTFESelectOverwriteMethod.pbOKClick(Sender: TObject);
begin
  OverwriteWithEncryptedData := rbDataEncrypted.Checked;
  if OverwriteWithEncryptedData then begin
    CypherDriver := cypherKernelModeDriverNames[cbCypher.ItemIndex];
    CypherGUID   := StringToGUID(cypherGUIDs[cbCypher.ItemIndex]);
  end;

  ModalResult := mrOk;

end;


procedure TfrmFreeOTFESelectOverwriteMethod.pbCypherDetailsClick(Sender: TObject);
begin
  OTFEFreeOTFEObj.ShowCypherDetailsDlg(
    cypherKernelModeDriverNames[cbCypher.ItemIndex],
    StringToGUID(cypherGUIDs[cbCypher.ItemIndex])
    );

end;

procedure TfrmFreeOTFESelectOverwriteMethod.FormShow(Sender: TObject);
var
  tmpDisplayTitles: TStringList;
begin
  { TODO 2 -otdk -csecurity : default to secure wipe }
  reInstructOverwriteType.Text :=
    _('Please select the type of data that should be used to overwrite the free space:' +
    SDUCRLF + SDUCRLF +
    'Pseudorandom data - This is faster, but less secure if you wish to create a hidden Box within this Box later.'
    + SDUCRLF + SDUCRLF +
    'Encrypted data - This is more secure, but slower. Pseudorandom data will be encrypted with your choice of cypher before being written to the drive.');

  tmpDisplayTitles := TStringList.Create();
  try
    if (OTFEFreeOTFEObj.GetCypherList(tmpDisplayTitles, cypherKernelModeDriverNames, cypherGUIDs))
    then begin
      cbCypher.Items.Clear();
      cbCypher.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more DoxBox cypher drivers installed and started.') +
        SDUCRLF + SDUCRLF + _(
        'If you have only just installed DoxBox, you may need to restart your computer.'),
        mtError
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

  EnableDisableControls();

end;

procedure TfrmFreeOTFESelectOverwriteMethod.FormCreate(Sender: TObject);
begin
  cypherKernelModeDriverNames := TStringList.Create();
  cypherGUIDs                 := TStringList.Create();

end;

procedure TfrmFreeOTFESelectOverwriteMethod.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  cypherKernelModeDriverNames.Free();
  cypherGUIDs.Free();

end;

procedure TfrmFreeOTFESelectOverwriteMethod.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();

end;


procedure TfrmFreeOTFESelectOverwriteMethod.EnableDisableControls();
begin
  cbCypher.Enabled        := rbDataEncrypted.Checked;
  pbCypherDetails.Enabled := (cbCypher.ItemIndex >= 0) and cbCypher.Enabled;

  // Either the pseudorandom radiobutton is checked, or a cypher has been
  // selected (if the pseudorandom radiobutton *isn't* checked, it's implicit
  // that the encrypted data radiobutton *is* checked)
  pbOK.Enabled := (rbDataPseudorandom.Checked or (cbCypher.ItemIndex >= 0));

end;


end.
