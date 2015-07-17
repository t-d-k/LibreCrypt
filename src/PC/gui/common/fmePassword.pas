unit fmePassword;

{
copyright
  (c) tdk
  my code dual licenced under FreeOTFE licence and LGPL
  code marked as 'sdean' is freeotfe licence only

description
  new kephrase, get KeyPhrase from KeyPhrase property
  IsValid true iff match
}

interface

uses
  //delphi
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SDUFrames, Vcl.StdCtrls, Vcl.ComCtrls,  Vcl.Samples.Gauges,
  //sdu
  SDUGeneral,  PasswordRichEdit, OTFEFreeOTFE_PasswordRichEdit, SDUComCtrls
  //librecrypt
  ;

type
  TfrmePassword = class (TSDUFrame)
    lblKeyPhrase: TLabel;
    preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit;

    procedure preUserKeyFirstChange(Sender: TObject);
  private
    FOnChange:     TNotifyEvent;
    fKeyPhraseSet: Boolean;     // has kephrase been set manually ('silent' mode)


  protected
    procedure DoShow(); override;
    procedure DoChange();
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyPhrase(Value: TSDUBytes);  overload;
        procedure SetKeyPhrase(Value: string); overload;
    function IsPasswordValid: Boolean;
    function GetKeyPhrase: TSDUBytes;
    procedure ClearPassword();
  published
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;

  end;

 //var
 //  frmeNewPassword: TfrmePassword;

implementation

{$R *.dfm}

uses
  strutils,{CommCtrl,uxTheme,}
//sdu
  SDUi18n,

  //librecrypt
  OTFEFreeOTFEBase_U;

const
  G_ALLCHARS: TSysCharSet              = [#0..#127];
  G_ALLBYTES: TSysCharSet              = [#0..#255];
  G_ALPHASYMBOLCHARS: TSysCharSet      = [#33..#126];// all alphanumeric + nonspace specials
  G_SYMBOLCHARS: TSysCharSet           = [#33..#47, #58..#64, #91..#96, #123..#126];
  // all nonspace specials xcept numbers
  G_WHITESPACEANDNONPRINT: TSysCharSet = [#0..#32, #127]; // all ascii controls + space
  G_NONPRINT: TSysCharSet              = [#0..#8, #14..#31, #127]; // all ascii controls
  G_WHITESPACE: TSysCharSet            = [#9..#13, ' ']; //  printable whitespace
  G_ALPHANUM: TSysCharSet              = ['0'..'9', 'A'..'Z', 'a'..'z'];//  alphanumeric
  G_ALPHANUM_APPOS: TSysCharSet        = ['0'..'9', 'A'..'Z', 'a'..'z', ''''];
  //  alphanumeric  + appostrophe
  G_ALPHA: TSysCharSet                 = ['A'..'Z', 'a'..'z'];//  alphabet
  G_NUMCHARS: TSysCharSet              = ['0'..'9'];//  numeric
  G_CAPITALS: TSysCharSet              = ['A'..'Z'];
  G_VALID_FILENAME: TSysCharSet        = [#43..#46, #48..#57, #65..#90, #95, #97..#122];

{ TfrmePassword }

procedure TfrmePassword.ClearPassword;
begin
  preUserKeyFirst.Lines.Clear;
end;

constructor TfrmePassword.Create(AOwner: TComponent);
begin
  inherited;
  fKeyPhraseSet := False;
end;

procedure TfrmePassword.DoChange;
begin
  if Assigned(fonchange) then
    fonchange(self);
end;

procedure TfrmePassword.DoShow;
begin
  inherited;

    preUserKeyFirst.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKeyFirst.PasswordChar := GetFreeOTFEBase().PasswordChar;
    preUserKeyFirst.WordWrap    := True;
  preUserKeyFirst.WantReturns  := GetFreeOTFEBase().AllowNewlinesInPasswords;
  preUserKeyFirst.WantTabs     := GetFreeOTFEBase().AllowTabsInPasswords;


  // if manually set ('silent' mode) don't clear
  if not fKeyPhraseSet then begin

    preUserKeyFirst.Lines.Clear();
  end;
    // Position cursor to the *end* of any password
  preUserKeyFirst.SelStart := length(preUserKeyFirst.Text);
  preUserKeyFirst.SetFocus;
end;

function TfrmePassword.IsPasswordValid(): Boolean;
begin
  Result := preUserKeyFirst.Text <> '';
end;


procedure TfrmePassword.preUserKeyFirstChange(Sender: TObject);
begin
  inherited;
  DoChange();
end;

procedure TfrmePassword.SetKeyPhrase(Value: string);
begin
  SetKeyPhrase(SDUStringToSDUBytes(Value));
end;

function TfrmePassword.GetKeyPhrase(): TSDUBytes;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  Result := SDUStringToSDUBytes(preUserKeyFirst.Text);
end;

procedure TfrmePassword.SetKeyPhrase(Value: TSDUBytes);
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  preUserKeyFirst.Text   := SDUBytesToString(Value);
  fKeyPhraseSet          := True;
end;



end.
