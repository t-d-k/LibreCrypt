unit OTFEFreeOTFE_LUKSKeyOrKeyfileEntry;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, StdCtrls,
  SysUtils, Variants, Windows, //doxbox
  ComCtrls,
  OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFEBase_U,
  PasswordRichEdit, SDUDropFiles,
  SDUFilenameEdit_U, SDUFrames, SDUGeneral, SDUStdCtrls;

type
  TOTFEFreeOTFELUKSKeyOrKeyfileEntry = class (TFrame)
    lblTreatNewlineAsEOF_1: TLabel;
    preUserKey:             TOTFEFreeOTFE_PasswordRichEdit;
    rbKeyFromUser:          TRadioButton;
    rbKeyFromKeyfile:       TRadioButton;
    feKeyfile:              TSDUFilenameEdit;
    ckKeyfileContainsASCII: TSDUCheckBox;
    cbNewlineType:          TComboBox;
    SDUDropFiles_Keyfile:   TSDUDropFiles;
    lblTreatNewlineAsEOF_2: TLabel;
    procedure rbKeyFromClick(Sender: TObject);
    procedure feKeyfileChange(Sender: TObject);
    procedure ckKeyfileContainsASCIIClick(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: String;
      DropPoint: TPoint);
  PRIVATE
    procedure PopulateNewlineType();
  PUBLIC
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

procedure Register;

implementation

{$R *.dfm}

procedure Register;
begin
  RegisterComponents('FreeOTFE', [TOTFEFreeOTFELUKSKeyOrKeyfileEntry]);
end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.feKeyfileChange(Sender: TObject);
begin
  rbKeyFromKeyfile.Checked := True;
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.preUserKeyChange(Sender: TObject);
begin
  rbKeyFromUser.Checked := True;
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.rbKeyFromClick(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SDUDropFiles_KeyfileFileDrop(Sender: TObject;
  DropItem: String; DropPoint: TPoint);
begin
  feKeyfile.Filename := DropItem;
  EnableDisableControls();
end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.ckKeyfileContainsASCIIClick(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.EnableDisableControls();
begin
  SDUEnableControl(ckKeyfileContainsASCII, rbKeyFromKeyfile.Checked);
  SDUEnableControl(lblTreatNewlineAsEOF_1, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));
  SDUEnableControl(cbNewlineType, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));
  SDUEnableControl(lblTreatNewlineAsEOF_2, (rbKeyFromKeyfile.Checked and
    ckKeyfileContainsASCII.Checked));

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.PopulateNewlineType();
var
  currNewline: TSDUNewline;
begin
  cbNewlineType.Items.Clear();
  for currNewline := low(TSDUNewline) to high(TSDUNewline) do begin
    cbNewlineType.Items.Add(SDUNEWLINE_TITLE[currNewline]);
  end;

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.Initialize();
begin
  PopulateNewlineType();

  feKeyfile.OpenDialog.Options := feKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feKeyfile.SaveDialog.Options := feKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  SDUDropFiles_Keyfile.Active := True;

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.DefaultOptions();
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

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyRaw(var userKey: String): Boolean;
begin
  userKey := preUserkey.Text;
  Result  := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKey(var userKey: TSDUBYtes): Boolean;
var
  retval:             Boolean;
  keyfileIsASCII:     Boolean;
  keyfileNewlineType: TSDUNewline;
begin
  retval := False;

  if rbKeyFromUser.Checked then begin
    { TODO 1 -otdk -cfix : warn user about losing unicde chars }
    userKey := SDUStringToSDUBytes( preUserkey.Text);
    retval  := True;
  end else begin
    if (feKeyfile.Filename <> '') then begin
      GetKeyfileIsASCII(keyfileIsASCII);
      GetKeyfileNewlineType(keyfileNewlineType);

      retval := GetFreeOTFEBase().ReadLUKSKeyFromFile(
        feKeyfile.Filename,
        keyfileIsASCII,
        keyfileNewlineType,
        userKey);
    end;
  end;

  Result := retval;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKey(userKey: PasswordString): Boolean;
begin
  preUserkey.Text := userKey;
  Result          := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfile(filename: String): Boolean;
begin
  feKeyfile.Filename := filename;
  Result             := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfile(var filename: String): Boolean;
begin
  filename := feKeyfile.Filename;
  Result   := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfileIsASCII(var isASCII: Boolean): Boolean;
begin
  isASCII := ckKeyfileContainsASCII.Checked;
  Result  := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfileIsASCII(isASCII: Boolean): Boolean;
begin
  ckKeyfileContainsASCII.Checked := isASCII;
  Result                         := True;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfileNewlineType(
  var nlType: TSDUNewline): Boolean;
var
  currNewline: TSDUNewline;
  allOK:       Boolean;
begin
  allOK := False;

  for currNewline := low(TSDUNewline) to high(TSDUNewline) do begin
    if (cbNewlineType.Items[cbNewlineType.ItemIndex] = SDUNEWLINE_TITLE[currNewline]) then begin
      nlType := currNewline;
      allOK  := True;
      break;
    end;
  end;

  Result := allOK;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;
var
  idx:   Integer;
  allOK: Boolean;
begin
  idx                     := cbNewlineType.Items.IndexOf(SDUNEWLINE_TITLE[nlType]);
  cbNewlineType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.KeyIsUserEntered(): Boolean;
begin
  Result := rbKeyFromUser.Checked;
end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.CursorToEndOfPassword();
begin
  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);
end;

end.
