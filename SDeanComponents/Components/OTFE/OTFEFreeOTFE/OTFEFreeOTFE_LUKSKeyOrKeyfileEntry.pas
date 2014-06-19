unit OTFEFreeOTFE_LUKSKeyOrKeyfileEntry;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, SDUStdCtrls, SDUFrames, SDUFilenameEdit_U, ComCtrls,
  PasswordRichEdit, OTFEFreeOTFE_PasswordRichEdit, SDUDropFiles,
  OTFEFreeOTFEBase_U,
  SDUGeneral;

type
  TOTFEFreeOTFELUKSKeyOrKeyfileEntry = class(TFrame)
    lblTreatNewlineAsEOF_2: TLabel;
    lblTreatNewlineAsEOF_1: TLabel;
    preUserKey: TOTFEFreeOTFE_PasswordRichEdit;
    rbKeyFromUser: TRadioButton;
    rbKeyFromKeyfile: TRadioButton;
    feKeyfile: TSDUFilenameEdit;
    ckKeyfileContainsASCII: TSDUCheckBox;
    cbNewlineType: TComboBox;
    SDUDropFiles_Keyfile: TSDUDropFiles;
    procedure rbKeyFromClick(Sender: TObject);
    procedure feKeyfileChange(Sender: TObject);
    procedure ckKeyfileContainsASCIIClick(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: string;
      DropPoint: TPoint);
  private
    procedure PopulateNewlineType();
  public
    FreeOTFEObj: TOTFEFreeOTFEBase;

    procedure Initialize();
    procedure DefaultOptions();

    procedure EnableDisableControls();

    function  GetKey(var userKey: string): boolean;
    function  GetKeyRaw(var userKey: string): boolean;
    function  SetKey(userKey: string): boolean;
    function  SetKeyfile(filename: string): boolean;
    function  GetKeyfile(var filename: string): boolean;
    function  GetKeyfileIsASCII(var isASCII: boolean): boolean;
    function  SetKeyfileIsASCII(isASCII: boolean): boolean;
    function  GetKeyfileNewlineType(var nlType: TSDUNewline): boolean;
    function  SetKeyfileNewlineType(nlType: TSDUNewline): boolean;

    function  KeyIsUserEntered(): boolean;

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
  rbKeyFromKeyfile.checked := TRUE;
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.preUserKeyChange(Sender: TObject);
begin
  rbKeyFromUser.checked := TRUE;
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.rbKeyFromClick(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SDUDropFiles_KeyfileFileDrop(
  Sender: TObject; DropItem: string; DropPoint: TPoint);
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
  SDUEnableControl(ckKeyfileContainsASCII, rbKeyFromKeyfile.checked);
  SDUEnableControl(lblTreatNewlineAsEOF_1, (rbKeyFromKeyfile.checked and ckKeyfileContainsASCII.checked));
  SDUEnableControl(cbNewlineType,          (rbKeyFromKeyfile.checked and ckKeyfileContainsASCII.checked));
  SDUEnableControl(lblTreatNewlineAsEOF_2, (rbKeyFromKeyfile.checked and ckKeyfileContainsASCII.checked));

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.PopulateNewlineType();
var
  currNewline: TSDUNewline;
begin
  cbNewlineType.Items.Clear();
  for currNewline:=low(TSDUNewline) to high(TSDUNewline) do
    begin
    cbNewlineType.Items.Add(SDUNEWLINE_TITLE[currNewline]);
    end;

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.Initialize();
begin
  PopulateNewlineType();

  feKeyfile.OpenDialog.Options := feKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feKeyfile.SaveDialog.Options := feKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  SDUDropFiles_Keyfile.Active := TRUE;

end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.DefaultOptions();
begin
  preUserKey.Plaintext := TRUE;
  // Linux volumes CAN NOT have newlines in the user's password
  preUserKey.WantReturns := FALSE;
  preUserKey.WordWrap := TRUE;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := FreeOTFEObj.PasswordChar;
  preUserKey.WantReturns  := FreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey.WantTabs     := FreeOTFEObj.AllowTabsInPasswords;

  SetKeyfileIsASCII(LINUX_KEYFILE_DEFAULT_IS_ASCII);
  SetKeyfileNewlineType(LINUX_KEYFILE_DEFAULT_NEWLINE);

  rbKeyFromUser.checked := TRUE;

end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyRaw(var userKey: string): boolean;
begin
  userKey := preUserkey.Text;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKey(var userKey: string): boolean;
var
  retval: boolean;
  keyfileIsASCII: boolean;
  keyfileNewlineType: TSDUNewline;
begin
  retval := FALSE;
  
  if rbKeyFromUser.checked then
    begin
    userKey := preUserkey.Text;
    retval := TRUE;
    end
  else
    begin
    if (feKeyfile.Filename <> '') then
      begin
      GetKeyfileIsASCII(keyfileIsASCII);
      GetKeyfileNewlineType(keyfileNewlineType);

      retval := FreeOTFEObj.ReadLUKSKeyFromFile(
                                    feKeyfile.Filename,
                                    keyfileIsASCII,
                                    keyfileNewlineType,
                                    userKey
                                   );
      end;
    end;

  Result := retval;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKey(userKey: string): boolean;
begin
  preUserkey.Text := userKey;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfile(filename: string): boolean;
begin
  feKeyfile.Filename := filename;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfile(var filename: string): boolean;
begin
  filename := feKeyfile.Filename;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfileIsASCII(var isASCII: boolean): boolean;
begin
  isASCII := ckKeyfileContainsASCII.checked;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfileIsASCII(isASCII: boolean): boolean;
begin
  ckKeyfileContainsASCII.checked := isASCII;
  Result := TRUE;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.GetKeyfileNewlineType(var nlType: TSDUNewline): boolean;
var
  currNewline: TSDUNewline;
  allOK: boolean;
begin
  allOK := FALSE;

  for currNewline:=low(TSDUNewline) to high(TSDUNewline) do
    begin
    if (cbNewlineType.Items[cbNewlineType.ItemIndex] = SDUNEWLINE_TITLE[currNewline]) then
      begin
      nlType := currNewline;
      allOK := TRUE;
      break;
      end;
    end;

  Result := allOK;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.SetKeyfileNewlineType(nlType: TSDUNewline): boolean;
var
  idx: integer;
  allOK: boolean;
begin
  idx := cbNewlineType.Items.IndexOf(SDUNEWLINE_TITLE[nlType]);
  cbNewlineType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;

function TOTFEFreeOTFELUKSKeyOrKeyfileEntry.KeyIsUserEntered(): boolean;
begin
  Result:= rbKeyFromUser.checked;
end;

procedure TOTFEFreeOTFELUKSKeyOrKeyfileEntry.CursorToEndOfPassword();
begin
  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);
end;

END.

