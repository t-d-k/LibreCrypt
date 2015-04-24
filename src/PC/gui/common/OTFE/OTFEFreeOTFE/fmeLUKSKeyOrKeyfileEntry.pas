unit fmeLUKSKeyOrKeyfileEntry;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, StdCtrls,
  SysUtils, Variants, Windows, //LibreCrypt
  ComCtrls,
  OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFEBase_U,
  PasswordRichEdit, SDUDropFiles,
  SDUFilenameEdit_U, SDUFrames, SDUGeneral, SDUStdCtrls;

type
  TfrmeLUKSKeyOrKeyfileEntry = class (TFrame)
    lblTreatNewlineAsEOF_1: TLabel;
    preUserKey:       TOTFEFreeOTFE_PasswordRichEdit;
    rbKeyFromUser:    TRadioButton;
    rbKeyFromKeyfile: TRadioButton;
    feKeyfile:        TSDUFilenameEdit;
    ckKeyfileContainsASCII: TSDUCheckBox;
    cbNewlineType:    TComboBox;
    SDUDropFiles_Keyfile: TSDUDropFiles;
    lblTreatNewlineAsEOF_2: TLabel;
    procedure rbKeyFromClick(Sender: TObject);
    procedure feKeyfileChange(Sender: TObject);
    procedure ckKeyfileContainsASCIIClick(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: String;
      DropPoint: TPoint);
  private
    procedure PopulateNewlineType();
  public
    //    FreeOTFEObj: TOTFEFreeOTFEBase;

    procedure Initialize();
    procedure DefaultOptions();

    procedure EnableDisableControls();

    function GetKey(var userKey: TSDUBYtes): Boolean;
    function GetKeyRaw(var userKey: String): Boolean;
    function SetKey(userKey: PasswordString): Boolean;
    function SetKeyfile(filename: String): Boolean;
    function GetKeyfile(var filename: String): Boolean;
    function GetKeyfileIsASCII(var isASCII: Boolean): Boolean;
    function SetKeyfileIsASCII(isASCII: Boolean): Boolean;
    function GetKeyfileNewlineType(var nlType: TSDUNewline): Boolean;
    function SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;

    function KeyIsUserEntered(): Boolean;

    procedure CursorToEndOfPassword();
  end;

//procedure Register;

implementation

{$R *.dfm}

 //procedure Register;
 //begin
 //  RegisterComponents('FreeOTFE', [TfrmeLUKSKeyOrKeyfileEntry]);
 //end;

procedure TfrmeLUKSKeyOrKeyfileEntry.feKeyfileChange(Sender: TObject);
begin
  rbKeyFromKeyfile.Checked := True;
  EnableDisableControls();

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.preUserKeyChange(Sender: TObject);
begin
  rbKeyFromUser.Checked := True;
  EnableDisableControls();

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.rbKeyFromClick(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.SDUDropFiles_KeyfileFileDrop(Sender: TObject;
  DropItem: String; DropPoint: TPoint);
begin
  feKeyfile.Filename := DropItem;
  EnableDisableControls();
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.ckKeyfileContainsASCIIClick(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.EnableDisableControls();
begin
  SDUEnableControl(ckKeyfileContainsASCII, rbKeyFromKeyfile.Checked);
  SDUEnableControl(lblTreatNewlineAsEOF_1, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));
  SDUEnableControl(cbNewlineType, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));
  SDUEnableControl(lblTreatNewlineAsEOF_2, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.PopulateNewlineType();
var
  currNewline: TSDUNewline;
begin
  cbNewlineType.Items.Clear();
  for currNewline := low(TSDUNewline) to high(TSDUNewline) do begin
    cbNewlineType.Items.Add(SDUNEWLINE_TITLE[currNewline]);
  end;

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.Initialize();
begin
  PopulateNewlineType();

  feKeyfile.OpenDialog.Options := feKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feKeyfile.SaveDialog.Options := feKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  SDUDropFiles_Keyfile.Active := True;

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.DefaultOptions();
begin
  preUserKey.Plaintext   := True;
  // Linux volumes CAN NOT have newlines in the user's password
  preUserKey.WantReturns := False;
  preUserKey.WordWrap    := True;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := GetFreeOTFEBase().PasswordChar;
  preUserKey.WantReturns  := GetFreeOTFEBase().AllowNewlinesInPasswords;
  preUserKey.WantTabs     := GetFreeOTFEBase().AllowTabsInPasswords;

  SetKeyfileIsASCII(LINUX_KEYFILE_DEFAULT_IS_ASCII);
  SetKeyfileNewlineType(LINUX_KEYFILE_DEFAULT_NEWLINE);

  rbKeyFromUser.Checked := True;

end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyRaw(var userKey: String): Boolean;
begin
  userKey := preUserkey.Text;
  Result  := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKey(var userKey: TSDUBYtes): Boolean;
var
  keyfileIsASCII:     Boolean;
  keyfileNewlineType: TSDUNewline;
begin
  Result := False;

  if rbKeyFromUser.Checked then begin
    { TODO 1 -otdk -cfix : warn user about losing unicde chars }
    userKey := SDUStringToSDUBytes(preUserkey.Text);
    Result  := True;
  end else begin
    if (feKeyfile.Filename <> '') then begin
      GetKeyfileIsASCII(keyfileIsASCII);
      GetKeyfileNewlineType(keyfileNewlineType);

      Result := GetFreeOTFEBase().ReadLUKSKeyFromFile(feKeyfile.Filename,
        keyfileIsASCII, keyfileNewlineType, userKey);
    end;
  end;

end;

function TfrmeLUKSKeyOrKeyfileEntry.SetKey(userKey: PasswordString): Boolean;
begin
  preUserkey.Text := userKey;
  Result          := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.SetKeyfile(filename: String): Boolean;
begin
  feKeyfile.Filename := filename;
  Result             := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyfile(var filename: String): Boolean;
begin
  filename := feKeyfile.Filename;
  Result   := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyfileIsASCII(var isASCII: Boolean): Boolean;
begin
  isASCII := ckKeyfileContainsASCII.Checked;
  Result  := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.SetKeyfileIsASCII(isASCII: Boolean): Boolean;
begin
  ckKeyfileContainsASCII.Checked := isASCII;
  Result := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyfileNewlineType(
  var nlType: TSDUNewline): Boolean;
var
  currNewline: TSDUNewline;
begin
  Result := False;

  for currNewline := low(TSDUNewline) to high(TSDUNewline) do begin
    if (cbNewlineType.Items[cbNewlineType.ItemIndex] = SDUNEWLINE_TITLE[currNewline]) then begin
      nlType := currNewline;
      Result := True;
      break;
    end;
  end;

end;

function TfrmeLUKSKeyOrKeyfileEntry.SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;
var
  idx: Integer;
begin
  idx                     := cbNewlineType.Items.IndexOf(SDUNEWLINE_TITLE[nlType]);
  cbNewlineType.ItemIndex := idx;

  Result := (idx >= 0);

end;

function TfrmeLUKSKeyOrKeyfileEntry.KeyIsUserEntered(): Boolean;
begin
  Result := rbKeyFromUser.Checked;
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.CursorToEndOfPassword();
begin
  // Position cufrmeLUKSKeyOrKeyfileEntry
  preUserKey.SelStart := length(preUserKey.Text);
end;

end.
