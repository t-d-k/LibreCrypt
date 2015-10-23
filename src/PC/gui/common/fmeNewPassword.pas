unit fmeNewPassword;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,  Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Samples.Gauges,
  //sdu
lcTypes,  SDUFrames,
  //librecrypt
  PasswordRichEdit, OTFEFreeOTFE_PasswordRichEdit, SDUComCtrls;

type
  TfrmeNewPassword = class (TSDUFrame)
    lblInstructPassword: TLabel;
    lblKeyphrase: TLabel;
    preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit;
    lblConfirm: TLabel;
    preUserKeyConfirm: TOTFEFreeOTFE_PasswordRichEdit;
    ProgressBar1:    TProgressBar;
    lblStrength:     TLabel;

    procedure preUserKeyConfirmChange(Sender: TObject);
    procedure preUserKeyFirstChange(Sender: TObject);
  private
    FOnChange:     TNotifyEvent;
    fKeyPhraseSet: Boolean; // has kephrase been set manually ('silent' mode)
    procedure UpdateColours;
    procedure UpdateStrength;

  protected
    procedure DoShow(); override;
    procedure DoChange();
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyPhrase(Value: TSDUBytes);  overload;
        procedure SetKeyPhrase(Value  : string   );   overload;
    function IsPasswordValid: Boolean;
    function GetKeyPhrase: TSDUBytes;
    procedure ClearKeyPhrase();
  published
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
  end;

 //var
 //  frmeNewPassword: TfrmeNewPassword;

implementation

{$R *.dfm}

uses
  SDUi18n, strutils,{CommCtrl,uxTheme,}
   SDUGeneral,
  //librecrypt
  CommonSettings,
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

{ TfrmeNewPassword }

procedure TfrmeNewPassword.ClearKeyPhrase;
begin
  preUserKeyFirst.Text   := '';
  preUserKeyConfirm.Text := preUserKeyFirst.Text;
end;

constructor TfrmeNewPassword.Create(AOwner: TComponent);
begin
  inherited;
  fKeyPhraseSet := False;
end;

procedure TfrmeNewPassword.DoChange;
begin
  if Assigned(fonchange) then
    fonchange(self);
end;

procedure TfrmeNewPassword.DoShow;
begin
  inherited;
  SDUTranslateComp(lblInstructPassword);


  // tsPassword
  preUserKeyFirst.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKeyFirst.WantReturns := True;
  preUserKeyFirst.WordWrap    := True;


  if not GetSettings().ShowPasswords then
    preUserKeyFirst.PasswordChar := '*';//defaults to #0

  preUserKeyFirst.WantReturns  := GetSettings().AllowNewlinesInPasswords;
  preUserKeyFirst.WantTabs     := GetSettings().AllowTabsInPasswords;
  preUserKeyFirst.SetFocus;

  preUserKeyConfirm.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKeyConfirm.WantReturns := True;
  preUserKeyConfirm.WordWrap    := True;
  // if manually set ('silent' mode) don;t clear
  if not fKeyPhraseSet then begin
    preUserKeyConfirm.Lines.Clear();
    preUserKeyFirst.Lines.Clear();
  end;

  if not GetSettings().ShowPasswords then
    preUserKeyConfirm.PasswordChar := '*';//defaults to #0

  preUserKeyConfirm.WantReturns  := GetSettings().AllowNewlinesInPasswords;
  preUserKeyConfirm.WantTabs     := GetSettings().AllowTabsInPasswords;

end;

function TfrmeNewPassword.IsPasswordValid(): Boolean;
begin
  Result := (preUserKeyFirst.Text = preUserKeyConfirm.Text) and (preUserKeyFirst.Text <> '');
end;


procedure TfrmeNewPassword.UpdateColours();
var
  aCol: TColor;
const
  // colours from system.UITypes
  Lightblue  = TColor($E6D8AD);
  Lightpink  = TColor($C1B6FF);
  Lightgreen = TColor($90EE90);
begin
  // if password complete and same then green, if different then red. if same up to length entered in 2nd, then blue
  if preUserKeyConfirm.Text = '' then
    aCol := clWhite
  else
  if (preUserKeyFirst.Text = preUserKeyConfirm.Text) then
    aCol := Lightgreen
  else
  if AnsiStartsStr(preUserKeyConfirm.Text, preUserKeyFirst.Text) then
    aCol := Lightblue
  else
    aCol := Lightpink;

  //too subtle - change background
  //  preUserKeyFirst.Font.Color := aCol;
  //   preUserKeyConfirm.Font.Color := aCol;
  preUserKeyConfirm.Color := aCol;
end;

procedure TfrmeNewPassword.UpdateStrength();
var
  aBits: Double;
  i:     Integer;
  aCol:  TColor;
  stren: String;
  ch:    Char;
begin
  aBits := 0;
  //very crude estimate of ntropy - helps shoulder surfers so add option to disable
  for i := 1 to length(preUserKeyFirst.Text) - 1 do begin
    ch := preUserKeyFirst.Text[i];
    if charinset(ch, G_ALPHA) or charinset(ch , G_WHITESPACE) then
      aBits := aBits + 1.1
    else
    if charinset(ch , G_NUMCHARS) then
      aBits := aBits + 1.5
    else
      aBits := aBits + 2;

  end;
  ProgressBar1.Position := trunc(aBits);

  if preUserKeyFirst.Text = '' then begin
    aCol  := clBlack;
    stren := _('No keyphrase');
  end else begin
    if aBits < 56 then begin
      aCol  := clRed;
      stren := _('Keyphrase weak');
    end else begin
      if aBits < 128 then begin
        aCol  := clBlue;
        stren := _('Keyphrase OK');
      end else begin
        aCol  := clGreen;
        stren := _('Keyphrase strong');
      end;
    end;
  end;


  //this doesnt work with theme
  ProgressBar1.BarColor  := aCol;
  //    ProgressBar1.BackgroundColor  := clYellow;
  lblStrength.Font.color := aCol;
  lblStrength.Caption    := stren;
 {  SetWindowTheme(self.Handle, nil, nil);
    ProgressBar1.Brush.Color:= clRed; // Set Background colour
SendMessage (ProgressBar1.Handle, PBM_SETBARCOLOR, 0, clBlue); // Set bar colour   }
end;

procedure TfrmeNewPassword.preUserKeyConfirmChange(Sender: TObject);
begin
  inherited;
  UpdateColours;
  DoChange();
end;

procedure TfrmeNewPassword.preUserKeyFirstChange(Sender: TObject);
begin
  inherited;
  UpdateColours;
  UpdateStrength();
  DoChange();
end;

function TfrmeNewPassword.GetKeyPhrase(): TSDUBytes;
begin
  { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  Result := SDUStringToSDUBytes(preUserKeyFirst.Text);
end;

 procedure TfrmeNewPassword.SetKeyPhrase(Value  : string   );
 begin
   { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
  preUserKeyFirst.Text   := Value;
  preUserKeyConfirm.Text := preUserKeyFirst.Text;
  fKeyPhraseSet          := True;
 end;

procedure TfrmeNewPassword.SetKeyPhrase(Value: TSDUBytes);
begin
  SetKeyPhrase(SDUBytesToString(Value));
end;

end.
