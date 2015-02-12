unit OTFEFreeOTFE_frmSelectVolumeAndOffset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin64,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUForms, SDUFrames, SDUSpin64Units, SDUDialogs,
  OTFEFreeOTFE_VolumeSelect;

type
  TfrmSelectVolumeFileAndOffset = class(TSDUForm)
    lblFileDescSrc: TLabel;
    lblOffsetSrc: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    lblInstructions: TLabel;
    se64UnitOffset: TSDUSpin64Unit_Storage;
    OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
  protected
    procedure EnableDisableControls();

  public
//    OTFEFreeOTFE: TOTFEFreeOTFEBase;
    procedure SetDlgPurpose(encryptNotDecrypt: boolean);
    function Volume(): string;
    function Offset(): int64;
  end;


implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral;

procedure TfrmSelectVolumeFileAndOffset.SetDlgPurpose(encryptNotDecrypt: boolean);
begin
  if encryptNotDecrypt then
    begin
    self.Caption := _('Secure with PKCS#11 Secret Key');
    lblInstructions.caption := _('Please enter the volume/keyfile you wish to encrypt using the selected PKCS#11 secret key.');
    end
  else
    begin
    self.Caption := _('Remove PKCS#11 Secret Key Security');
    lblInstructions.caption := _('Please enter the volume/keyfile you wish to decrypt using the selected PKCS#11 secret key.');
    end;

end;

function TfrmSelectVolumeFileAndOffset.Volume(): string;
begin
  Result := OTFEFreeOTFEVolumeSelect1.Filename;
end;

function TfrmSelectVolumeFileAndOffset.Offset(): int64;
begin
  Result := se64UnitOffset.Value;
end;

procedure TfrmSelectVolumeFileAndOffset.OTFEFreeOTFEVolumeSelect1Change(
  Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmSelectVolumeFileAndOffset.FormCreate(Sender: TObject);
begin
  // Note: This must be in the FormCreate(...) event, and not FormShow(...)
  // since the width must be set *before* the caption is in order for the
  // label's autosize to work correctly, and not adjust the label so that it's
  // height is to great as to obscure the "Volume/keyfile" label below it
  lblInstructions.Width := (Self.Width - (lblInstructions.left * 2));
  OTFEFreeOTFEVolumeSelect1.OnChange := OTFEFreeOTFEVolumeSelect1Change;
end;



procedure TfrmSelectVolumeFileAndOffset.FormShow(Sender: TObject);
begin
  OTFEFreeOTFEVolumeSelect1.Filename := '';
//  OTFEFreeOTFEVolumeSelect1.OTFEFreeOTFE := OTFEFreeOTFE;
  OTFEFreeOTFEVolumeSelect1.SelectFor := fndOpen;
  OTFEFreeOTFEVolumeSelect1.FileSelectFilter     := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
  OTFEFreeOTFEVolumeSelect1.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;

  se64UnitOffset.Value := 0;

  EnableDisableControls();
end;

procedure TfrmSelectVolumeFileAndOffset.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (trim(OTFEFreeOTFEVolumeSelect1.Filename) <> ''));
end;

END.


