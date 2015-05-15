unit frmPKCS11Management;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  pkcs11_session,
  fmePKCS11_MgrKeyfile,
  fmePKCS11_MgrBase,
  fmePKCS11_MgrSecretKey,
  OTFEFreeOTFEBase_U, SDUForms;

type
  TfrmPKCS11Management = class(TSDUForm)
    pbClose: TButton;
    PageControl1: TPageControl;
    tsSecretKeys: TTabSheet;
    tsKeyfiles: TTabSheet;
    fmePKCS11_MgrSecretKey1: TfmePKCS11_MgrSecretKey;
    fmePKCS11_MgrKeyfile1: TfmePKCS11_MgrKeyfile;
    procedure FormShow(Sender: TObject);
    procedure fmePKCS11_MgrKeys1Resize(Sender: TObject);
  private
    FPKCS11Session: TPKCS11Session;

  public
//    FreeOTFEObj: TOTFEFreeOTFEBase;
    property PKCS11Session: TPKCS11Session read FPKCS11Session write FPKCS11Session;
  end;


implementation

{$R *.dfm}

uses
  SDUGeneral;


procedure TfrmPKCS11Management.fmePKCS11_MgrKeys1Resize(Sender: TObject);
begin
  SDUCenterControl(fmePKCS11_MgrSecretKey1, ccHorizontal);
  SDUCenterControl(fmePKCS11_MgrSecretKey1, ccVertical, 25);

  SDUCenterControl(fmePKCS11_MgrKeyfile1, ccHorizontal);
  SDUCenterControl(fmePKCS11_MgrKeyfile1, ccVertical, 25);
end;

procedure TfrmPKCS11Management.FormShow(Sender: TObject);
begin
//  fmePKCS11_MgrSecretKey1.FreeOTFEObj := FreeOTFEObj;
  fmePKCS11_MgrSecretKey1.PKCS11Session := PKCS11Session;
  fmePKCS11_MgrSecretKey1.Initialize();

  fmePKCS11_MgrKeyfile1.PKCS11Session := PKCS11Session;
  fmePKCS11_MgrKeyfile1.Initialize();

  PageControl1.ActivePage := tsSecretKeys;
end;

END.

