unit fmePassword;

{
copyright
  (c) tdk
  my code dual licenced under FreeOTFE licence and LGPL
  code marked as 'sdean' is freeotfe licence only

description
  new kephrase, get KeyPhrase from KeyPhrase property
  IsValid true if is not empty

  tmemo doesnt expose it's password property.
  tpasswordrichedit doesn't seem to show if passwordchar set after created (and why is it a richedit not a memo?)
  need to use memo not edit so can get newlines (true?)
  so uses shadow hidden 'real' memo that gets all same keypresses (for home, end, up , down etc)
  and shown memo that shows password char

}

interface

uses
  //delphi
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SDUFrames, Vcl.StdCtrls, Vcl.ComCtrls,  Vcl.Samples.Gauges,
  //sdu
    lcTypes,  SDUComCtrls
  //librecrypt
  ;

type
  TfrmePassword = class (TSDUFrame)
    lblKeyPhrase: TLabel;
    mmShown: TMemo;
    mmReal: TMemo;

    procedure mmShownKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mmShownKeyPress(Sender: TObject; var Key: Char);
    procedure mmShownChange(Sender: TObject);
    procedure mmShownKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnChange:     TNotifyEvent;
    fkey_phrase_set: Boolean;     // has keyphrase been set manually ('silent' mode)
  protected
    procedure DoShow(); override;
    procedure DoChange();
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyPhrase(Value: TSDUBytes);  overload;
    procedure SetKeyPhrase(Value: string); overload;
    function IsPasswordValid: Boolean;
    function GetKeyPhrase: TSDUBytes;
    function GetKeyPhraseAsString: string;
    procedure ClearPassword();
  published
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;

  end;


implementation

{$R *.dfm}

uses
  strutils,{CommCtrl,uxTheme,}
//sdu
  SDUi18n,
  SDUGeneral,
  //librecrypt
  CommonSettings;

//const
//  G_ALLCHARS: TSysCharSet              = [#0..#127];
//  G_ALLBYTES: TSysCharSet              = [#0..#255];
//  G_ALPHASYMBOLCHARS: TSysCharSet      = [#33..#126];// all alphanumeric + nonspace specials
//  G_SYMBOLCHARS: TSysCharSet           = [#33..#47, #58..#64, #91..#96, #123..#126];
//  // all nonspace specials xcept numbers
//  G_WHITESPACEANDNONPRINT: TSysCharSet = [#0..#32, #127]; // all ascii controls + space
//  G_NONPRINT: TSysCharSet              = [#0..#8, #14..#31, #127]; // all ascii controls
//  G_WHITESPACE: TSysCharSet            = [#9..#13, ' ']; //  printable whitespace
//  G_ALPHANUM: TSysCharSet              = ['0'..'9', 'A'..'Z', 'a'..'z'];//  alphanumeric
//  G_ALPHANUM_APPOS: TSysCharSet        = ['0'..'9', 'A'..'Z', 'a'..'z', ''''];
//  //  alphanumeric  + appostrophe
//  G_ALPHA: TSysCharSet                 = ['A'..'Z', 'a'..'z'];//  alphabet
//  G_NUMCHARS: TSysCharSet              = ['0'..'9'];//  numeric
//  G_CAPITALS: TSysCharSet              = ['A'..'Z'];
//  G_VALID_FILENAME: TSysCharSet        = [#43..#46, #48..#57, #65..#90, #95, #97..#122];

{ TfrmePassword }

procedure TfrmePassword.ClearPassword;
begin
  mmShown.Lines.Clear;
  mmReal.Lines.Clear;
end;

constructor TfrmePassword.Create(AOwner: TComponent);
begin
  inherited;
  fkey_phrase_set := False;
end;

procedure TfrmePassword.DoChange;
begin
  if Assigned(fonchange) then
    fonchange(self);
end;

procedure TfrmePassword.DoShow;
begin
  inherited;

//   FreeOTFE volumes CAN have newlines in the user's password
{ done -otdk -csecurity : implement password char in keypress event }

{ in order to make sure that arrow keys act identically in 'real' and 'shown'
memos when word wrap is on. widths have to be identical
}
  mmReal.width := mmshown.Width;
  mmReal.Height := mmshown.Height;
  mmReal.left:=  mmshown.left;
  mmReal.top:=  mmshown.top;
//   mmReal.Visible := true;//debugging

  mmShown.WordWrap    := True;
  mmShown.WantReturns  := GetSettings().AllowNewlinesInPasswords;
  mmShown.WantTabs     := GetSettings().AllowTabsInPasswords;

  mmReal.WordWrap    := mmShown.WordWrap;
  mmReal.WantReturns  := mmShown.WantReturns;
  mmReal.WantTabs     := mmShown.WantTabs;


  // if manually set ('silent' mode) don't clear
  if not fkey_phrase_set then begin
    mmShown.Lines.Clear();
    mmReal.Lines.Clear();
  end;

    // Position cursor to the *end* of any password
  mmShown.SelStart := length(mmShown.Text);
  mmShown.SetFocus;

  mmReal.SelStart := mmShown.SelStart;
end;

function TfrmePassword.IsPasswordValid(): Boolean;
begin
  Result := mmReal.Text <> '';
end;


procedure TfrmePassword.mmShownChange(Sender: TObject);
begin
  inherited;
  DoChange();
end;

procedure TfrmePassword.mmShownKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  //echo to 'real' memo - keeps key
    // keep cursor movements, and selections in sync as well.
  mmReal.Perform(WM_KEYDOWN, Key, 0);
//  mmShown.Perform(wm_keydown, key, 0);
end;
procedure TfrmePassword.mmShownKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
    mmReal.Perform(WM_KEYUP, Key, 0);
end;

procedure TfrmePassword.mmShownKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;


  // doesnt work perfectly if user moves cursor up and down a lot, but ok for most, eg BS select all, etc.
   mmReal.SelStart := mmShown.SelStart;
   mmReal.SelLength := mmShown.SelLength;


  mmReal.Perform(wm_char, Byte(Key), 0);
  // allow backspace, CR and LF thru, others replace
//  if not (Key in [#10, #13, #8]) then
  if Key >= #32 then
    if not GetSettings().ShowPasswords then
        Key := '*';
end;




procedure TfrmePassword.SetKeyPhrase(Value: string);
begin
  SetKeyPhrase(SDUStringToSDUBytes(Value));
end;

function TfrmePassword.GetKeyPhrase(): TSDUBytes;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }

  Result := SDUStringToSDUBytes(GetKeyPhraseAsString());
end;

function TfrmePassword.GetKeyPhraseAsString: string;
begin
//  if GetSettings().OptShowPasswords then
//  result :=  mmShown.Text
//  else
result :=  mmReal.Text;
end;

procedure TfrmePassword.SetKeyPhrase(Value: TSDUBytes);
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  mmReal.Text   := SDUBytesToString(Value);
  if GetSettings().ShowPasswords then
    mmShown.Text := mmReal.Text
  else
    mmShown.Text := StringOfChar('*',length(mmReal.Text));

  fkey_phrase_set          := True;
end;



end.
