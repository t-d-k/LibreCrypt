unit OTFEFreeOTFE_frmPKCS11Session;


// If the result of ShowModal = mrOK, then "Session" will either be set to nil
// (don't use PKCS11 token), or a logged in PKCS#11 session
//
// It is up to the CALLER to close this session, and to free it off after use

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  pkcs11_library,
  pkcs11_session, SDUForms;

type
  TfrmPKCS11Session = class(TSDUForm)
    cbToken: TComboBox;
    edPIN: TEdit;
    lblPIN: TLabel;
    lblToken: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    pbRefresh: TButton;
    Label3: TLabel;
    ckUseSecureAuthPath: TCheckBox;
    pbSkip: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ckUseSecureAuthPathClick(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAllowSkip: boolean;
    FPKCS11LibObj: TPKCS11Library;
    FPKCS11Session: TPKCS11Session;

    procedure SetAllowSkip(skip: boolean);
  protected
    procedure PopulateTokens();
  public
    property PKCS11LibObj: TPKCS11Library read FPKCS11LibObj write FPKCS11LibObj;
    property Session: TPKCS11Session read FPKCS11Session;
    // Allow/supress the "Skip" button
    property AllowSkip: boolean read FAllowSkip write SetAllowSkip;
  end;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  SDUDialogs,
  SDUi18n,
  pkcs11_slot,
  pkcs11_token,
  OTFEFreeOTFE_PKCS11;

resourcestring
  RS_SLOT_REMOVED = 'Slot removed; token no longer available - please reselect and try again';
  RS_TOKEN_REMOVED = 'Token no longer available; please reselect and try again';

procedure TfrmPKCS11Session.FormCreate(Sender: TObject);
begin
  FPKCS11Session := nil;

  edPIN.PasswordChar := '*';
  edPIN.text := '';

  FAllowSkip := TRUE;

end;

procedure TfrmPKCS11Session.FormShow(Sender: TObject);
begin
  PopulateTokens();

end;

procedure TfrmPKCS11Session.pbOKClick(Sender: TObject);
var
  slot: TPKCS11Slot;
  token: TPKCS11Token;
  allOK: boolean;
  loginOK: boolean;
  slotID: integer;
begin
  // Test to see if the user's entered a valid *user* pin    
  FPKCS11Session := nil;

  allOK := TRUE;
  slot := nil;
  token := nil;

  slotID := PKCS11TokenListSelected(cbToken);

  if allOK then
    begin
    slot := PKCS11LibObj.SlotByID[slotID];
    if (slot = nil) then
      begin
      SDUMessageDlg(RS_SLOT_REMOVED, mtError);

      // Be helpful - refresh the list
      PopulateTokens();

      allOK := FALSE;
      end;
    end;

  if allOK then
    begin
    try
      token := slot.Token();
    except
      on E:Exception do
        begin
        token := nil;
        end;
    end;

    if (token = nil) then
      begin
      SDUMessageDlg(RS_TOKEN_REMOVED, mtError);

      // Be helpful - refresh the list
      PopulateTokens();

      allOK := FALSE;
      end;
    end;


  if allOK then
    begin
    FPKCS11Session := token.OpenSession(FALSE);
    if (FPKCS11Session = nil) then
      begin
      SDUMessageDlg(
                    SDUParamSubstitute(_('Unable to open session for token in slot %1'), [slotID])+SDUCRLF+
                    SDUCRLF+
                    FPKCS11Session.RVToString(FPKCS11Session.LastRV),
                    mtError
                   );

      allOK := FALSE;
      end;
    end;

  if allOK then
    begin
    // Logout of any previous session (in case already logged in)
    FPKCS11Session.Logout();

    // Login to session...
    if ckUseSecureAuthPath.checked then
      begin
      loginOK := FPKCS11Session.Login(utUser);
      end
    else
      begin
      loginOK := FPKCS11Session.Login(utUser, edPIN.text); { TODO 1 -otdk -cbug : alert user if unicode pin }
      end;

    if not(loginOK) then
      begin
      SDUMessageDlg(
                    _('Login failed')+SDUCRLF+
                    SDUCRLF+
                    session.RVToString(session.LastRV),
                    mtError
                   );

      token.CloseSession(FPKCS11Session);
      FPKCS11Session.Free();
      FPKCS11Session := nil;
      token.Free();

      allOK := FALSE;
      end;
    end;

  if allOK then
    begin
    ModalResult := mrOK;
    end;

end;

procedure TfrmPKCS11Session.pbRefreshClick(Sender: TObject);
begin
  PopulateTokens();
end;

procedure TfrmPKCS11Session.PopulateTokens();
var
  tokensDetected: boolean;
begin
  tokensDetected := (PKCS11PopulateTokenList(PKCS11LibObj, cbToken) > 0);

  // No tokens, no point in PIN entry
  SDUEnableControl(lblPIN, (
                            tokensDetected and
                            not(ckUseSecureAuthPath.checked)
                           ));
  SDUEnableControl(edPIN, (
                            tokensDetected and
                            not(ckUseSecureAuthPath.checked)
                           ));
  SDUEnableControl(ckUseSecureAuthPath, tokensDetected);

  // Setup buttons...
  SDUEnableControl(pbOK, tokensDetected);
  pbOK.Default := tokensDetected;
  pbSkip.Default := not(tokensDetected);

  // Select most useful control
  if not(tokensDetected) then
    begin
    FocusControl(pbRefresh);
    end
  else if cbToken.enabled then
    begin
    FocusControl(cbToken);
    end
  else
    begin
    FocusControl(edPIN);
    end;

end;

procedure TfrmPKCS11Session.ckUseSecureAuthPathClick(Sender: TObject);
begin
  SDUEnableControl(edPIN, not(ckUseSecureAuthPath.checked));
end;

procedure TfrmPKCS11Session.SetAllowSkip(skip: boolean);
var
  adjust: integer;
begin
  // Don't do anything if it's already set as required
  if (skip <> FAllowSkip) then
    begin
    // Adjust button positioning a bit...
    adjust := 1;
    if skip then
      begin
      adjust := -1;
      end;

    adjust := adjust * (pbSkip.width div 2);

    pbOK.left := pbOK.left + adjust;
    pbCancel.left := pbCancel.left - adjust;

    pbSkip.visible := skip;

    FAllowSkip := skip;
    end;

end;


END.


