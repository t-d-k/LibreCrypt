unit frmPKCS11NewSecretKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  pkcs11_session,
  PKCS11Lib, SDUForms;

type
  TfrmPKCS11NewSecretKey = class(TSDUForm)
    pbCancel: TButton;
    pbOK: TButton;
    cbKeyType: TComboBox;
    Label1: TLabel;
    edLabel: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edLabelChange(Sender: TObject);
    procedure cbKeyTypeChange(Sender: TObject);
  private
    { Private declarations }
  protected
    FPKCS11Session: TPKCS11Session;

    function CreateNewKey(): boolean;
    function GetKeyType(): TPKCS11SecretKeyType;
  public
    ExistingKeys: TStringList;
    property PKCS11Session: TPKCS11Session read FPKCS11Session write FPKCS11Session;

    procedure PopulateKeyTypes();
    procedure DefaultKeyType();

    procedure EnableDisableControls();
  end;

implementation

{$R *.dfm}

uses
 //lc Utils
  SDUi18n,
  SDUGeneral,
  lcDialogs,

  lcConsts;

// Populate list of key types with all those available on the token
procedure TfrmPKCS11NewSecretKey.PopulateKeyTypes();
var
  i: integer;
  secretKeyTypes: TPKCS11SecretKeyTypeArr;
  errMsg: string;
begin
  cbKeyType.Items.clear();

  if not(GetAllPKCS11SecretKeyTypes(
                                    PKCS11Session,
                                    secretKeyTypes,
                                    errMsg
                                   )) then
    begin
    SDUMessageDlg(_('Unable to get list of key types for Token')+SDUCRLF+SDUCRLF+errMsg, mtError);
    end
  else
    begin
    for i:=low(secretKeyTypes) to high(secretKeyTypes) do
      begin
      cbKeyType.items.add(PKCS11_SECRET_KEY_TYPES[secretKeyTypes[i]].Name);
      end;
    end;

end;

procedure TfrmPKCS11NewSecretKey.DefaultKeyType();
var
  i: integer;
begin
  // None selected
  cbKeyType.itemindex := -1;

  if (cbKeyType.items.count = 1) then
    begin
    cbKeyType.itemindex := 0;
    end
  else
    begin
    for i:=0 to (cbKeyType.items.count - 1) do
      begin
      if (cbKeyType.items[i] = PKCS11_DEFAULT_KEYTYPE) then
        begin
        cbKeyType.itemindex := i;
        break;
        end;
      end;
    end;

end;

procedure TfrmPKCS11NewSecretKey.edLabelChange(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmPKCS11NewSecretKey.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ExistingKeys.Free();
end;

procedure TfrmPKCS11NewSecretKey.FormCreate(Sender: TObject);
begin
  ExistingKeys := TStringList.Create();
end;

procedure TfrmPKCS11NewSecretKey.FormShow(Sender: TObject);
begin
  edLabel.text := '';
  
  PopulateKeyTypes();
  DefaultKeyType();

  EnableDisableControls();
end;

procedure TfrmPKCS11NewSecretKey.cbKeyTypeChange(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmPKCS11NewSecretKey.CreateNewKey(): boolean;
var
  retval: boolean;
  errMsg: string;
  newKeyLabel: string;
  csrPrev: TCursor;
begin
  newKeyLabel := trim(edLabel.text);
  if (ExistingKeys.Indexof(newKeyLabel) >= 0) then
    begin
    SDUMessageDlg(
                  _('A key with the name specified already exists.')+SDUCRLF+
                  SDUCRLF+
                  _('Please enter a unique key name'),
                  mtError);
    retval := FALSE;
    end
  else
    begin
    csrPrev := screen.Cursor;
    screen.Cursor := crHourglass;
    try
      retval := CreatePKCS11SecretKey(
                                      PKCS11Session,
                                      newKeyLabel,
                                      GetKeyType(),
                                      errMsg
                                     );
    finally
      screen.Cursor := csrPrev;
    end;

    if not(retval) then
      begin
      SDUMessageDlg(_('Unable to create new key')+SDUCRLF+SDUCRLF+errMsg, mtError);
      end
    else
      begin
      SDUMessageDlg(Format(_('Key "%s" created'), [edLabel.text]), mtInformation);
      end;
    end;

  Result := retval;
end;

procedure TfrmPKCS11NewSecretKey.pbOKClick(Sender: TObject);
begin
  if CreateNewKey() then
    begin
    ModalResult := mrOK;
    end;

end;

procedure TfrmPKCS11NewSecretKey.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (
                          (cbKeyType.itemindex >= 0) and
                          (edLabel.text <> '')
                         ));

  SDUEnableControl(cbKeyType, (cbKeyType.items.count > 1));
end;

function TfrmPKCS11NewSecretKey.GetKeyType(): TPKCS11SecretKeyType;
var
  retval: TPKCS11SecretKeyType;
  i: TPKCS11SecretKeyType;
begin
  retval := low(PKCS11_SECRET_KEY_TYPES);

  for i:=low(PKCS11_SECRET_KEY_TYPES) to high(PKCS11_SECRET_KEY_TYPES) do
    begin
    if (PKCS11_SECRET_KEY_TYPES[i].Name = cbKeyType.Items[cbKeyType.itemindex]) then
      begin
      retval := i;
      break;
      end;
    end;

  Result := retval;
end;

END.

