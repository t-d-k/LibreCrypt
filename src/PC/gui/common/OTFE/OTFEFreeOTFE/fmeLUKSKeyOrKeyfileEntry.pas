unit fmeLUKSKeyOrKeyfileEntry;

interface

uses
  //delphi
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, StdCtrls,
  SysUtils, Variants, Windows, ComCtrls,
   //3rd party
  //SDU ,lclibs
lcTypes,

   PasswordRichEdit, SDUDropFiles,
  SDUFilenameEdit_U, SDUFrames, SDUGeneral, SDUStdCtrls,
  //librecrypt
  OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFEBase_U   ;

type
  TfrmeLUKSKeyOrKeyfileEntry = class (TFrame)
    lblTreatNewlineAsEOF_1: TLabel;
    rbKeyFromUser:    TRadioButton;
    rbKeyFromKeyfile: TRadioButton;
    feKeyfile:        TSDUFilenameEdit;
    ckKeyfileContainsASCII: TSDUCheckBox;
    cbNewlineType:    TComboBox;
    SDUDropFiles_Keyfile: TSDUDropFiles;
    ckBaseIVCypherOnHashLength: TCheckBox;
    preUserKey: TPasswordRichEdit;
    procedure rbKeyFromClick(Sender: TObject);
    procedure feKeyfileChange(Sender: TObject);
    procedure ckKeyfileContainsASCIIClick(Sender: TObject);
    procedure preUserKeyChange(Sender: TObject);
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: String;
      DropPoint: TPoint);
  private

    procedure PopulateNewlineType();
    procedure SetbaseIVCypherOnHashLength(const Value: boolean);
    function GetbaseIVCypherOnHashLength: boolean;
  public

    procedure Initialize();
    procedure DefaultOptions();

    procedure EnableDisableControls();


    function GetKey(var userKey: TSDUBYtes): Boolean;
    function GetKeyRaw( ): String;
    procedure SetKey(userKey: PasswordString);
    procedure SetKeyfile(filename: String);
    function GetKeyfile( ): String;
    function GetKeyfileIsASCII(): Boolean;
    procedure SetKeyfileIsASCII(isASCII: Boolean);
    function GetKeyfileNewlineType(var nlType: TSDUNewline): Boolean;
    function SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;

    function KeyIsUserEntered(): Boolean;

    procedure CursorToEndOfPassword();

    property baseIVCypherOnHashLength: boolean read GetbaseIVCypherOnHashLength write SetbaseIVCypherOnHashLength;
  end;


implementation

uses
  //delphi
  //3rd party
  //SDU ,lclibs
lcConsts,

  //librecrypt
  CommonSettings,
LUKSTools;
{$R *.dfm}



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


  feKeyfile. TabStop := False                ;
  feKeyfile.  FilterIndex := 0               ;
  feKeyfile.  OnChange := feKeyfileChange    ;

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.DefaultOptions();
begin
  preUserKey.Plaintext   := True;
  // Linux volumes CAN NOT have newlines in the user's password
  preUserKey.WantReturns := False;
  preUserKey.WordWrap    := True;
  preUserKey.Lines.Clear();

  if not GetSettings().OptShowPasswords then
    preUserKey.PasswordChar := '*';//defaults to #0

  preUserKey.WantReturns  := GetSettings().OptAllowNewlinesInPasswords;
  preUserKey.WantTabs     := GetSettings().OptAllowTabsInPasswords;

  SetKeyfileIsASCII(LINUX_KEYFILE_DEFAULT_IS_ASCII);
  SetKeyfileNewlineType(LINUX_KEYFILE_DEFAULT_NEWLINE);

  rbKeyFromUser.Checked := True;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyRaw( ): String;
begin
  result := preUserkey.Text;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetbaseIVCypherOnHashLength: boolean;
begin
  result := ckBaseIVCypherOnHashLength.Checked;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKey(var userKey: TSDUBYtes): Boolean;
var
  keyfileIsASCII:     Boolean;
  keyfileNewlineType: TSDUNewline;
begin
  Result := False;

  if rbKeyFromUser.Checked then begin
    { TODO 1 -otdk -cfix : warn user about losing unicode chars }
    userKey := SDUStringToSDUBytes(preUserkey.Text);
    Result  := True;
  end else begin
    if (feKeyfile.Filename <> '') then begin
      keyfileIsASCII:= GetKeyfileIsASCII();
      GetKeyfileNewlineType(keyfileNewlineType);

      Result := LUKSTools.ReadLUKSKeyFromFile(feKeyfile.Filename,
        keyfileIsASCII, keyfileNewlineType, userKey);
    end;
  end;

end;

procedure TfrmeLUKSKeyOrKeyfileEntry.SetbaseIVCypherOnHashLength(
  const Value: boolean);
begin
ckBaseIVCypherOnHashLength.Checked :=  Value;
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.SetKey(userKey: PasswordString);
begin
  preUserkey.Text := userKey;
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.SetKeyfile(filename: String);
begin
  feKeyfile.Filename := filename;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyfile( ): String;
begin
  Result := feKeyfile.Filename;
end;

function TfrmeLUKSKeyOrKeyfileEntry.GetKeyfileIsASCII(): Boolean;
begin
  result := ckKeyfileContainsASCII.Checked;
end;

procedure TfrmeLUKSKeyOrKeyfileEntry.SetKeyfileIsASCII(isASCII: Boolean);
begin
  ckKeyfileContainsASCII.Checked := isASCII;
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
