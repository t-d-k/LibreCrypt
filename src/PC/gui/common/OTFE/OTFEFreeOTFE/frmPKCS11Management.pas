unit frmPKCS11Management;

interface

uses
     //delphi & libs
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  //sdu & LibreCrypt utils
        pkcs11_session,  OTFEFreeOTFEBase_U,
   // LibreCrypt forms
  fmePKCS11_MgrKeyfile,
  fmePKCS11_MgrBase,
  fmePKCS11_MgrSecretKey,
  SDUForms;

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

    procedure ShowPKCS11ManagementDlg();


implementation

{$R *.dfm}

uses
     //delphi & libs

  //sdu & LibreCrypt utils
      SDUGeneral,PKCS11Lib, pkcs11_library, OTFEConsts_U,
   // LibreCrypt forms
 frmPKCS11Session;


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


 // ----------------------------------------------------------------------------
 // Display control for managing PKCS#11 tokens
procedure ShowPKCS11ManagementDlg();
var
  pkcs11session:       TPKCS11Session;
  dlgPKCS11Session:    TfrmPKCS11Session;
  dlgPKCS11Management: TfrmPKCS11Management;
begin
  // Setup PKCS11 session, as appropriate
  pkcs11session := nil;
  if PKCS11LibraryReady(GPKCS11Library) then begin
    dlgPKCS11Session := TfrmPKCS11Session.Create(nil);
    try
      dlgPKCS11Session.PKCS11LibObj := GPKCS11Library;
      dlgPKCS11Session.AllowSkip    := False;
      if (dlgPKCS11Session.ShowModal = mrCancel) then begin
        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
      end else begin
        pkcs11session := dlgPKCS11Session.Session;
      end;

    finally
      dlgPKCS11Session.Free();
    end;
  end;

  if (pkcs11session <> nil) then begin
    dlgPKCS11Management := TfrmPKCS11Management.Create(nil);
    try
      //      dlgPKCS11Management.FreeOTFEObj   := self;
      dlgPKCS11Management.PKCS11Session := pkcs11session;
      dlgPKCS11Management.ShowModal();
    finally
      dlgPKCS11Management.Free();
    end;

    pkcs11session.Logout();
    pkcs11session.CloseSession();
    pkcs11session.Free();
  end;

end;

END.

