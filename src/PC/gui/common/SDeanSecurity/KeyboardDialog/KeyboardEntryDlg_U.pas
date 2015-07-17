unit KeyboardEntryDlg_U;
// Description: "Secure Keyboard Entry" Dialog
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This is a simple dialog that permits the user to easily add a slightly more
// secure method of entering passwords and other sensitive data into a Delphi
// application
//
// Features include:
//   The use of images, rather that text captions for the keytops to reduce the success rate of snooping software
//   "Speckled" images for buttons to reduce the effectiveness of "key" recognition software
//   Random "key" placement, again to reduce the success rate of snooping software
//
// Note: This component is not perfect; the RNG used is the standard Delphi
// pseudorandom one, and the "speckling" of the keytops provides minimum
// protection ("Speckling" was introduced to prevent an attacker from just
// reading the text straight off the screen, and forces him/her to stick an
// extra "despeckle" operation in there! ;)
//
// Attacking the protection that this dialog provides would be trivial; just
// write a program that detects a window being displayed with a "Scramble
// Keys" button on it. After that, hook the mouse messages, and every time the
// left mousebutton is pressed, take a screenshot of the dialog, and with it
// record the X & Y co-ords of the mouse within the dialog.
// Later, recover the images and info, and just watch what "keys" are being
// pressed on the dialog.


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, SDUForms;

const
  KEYS = 'aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ1!2"3£4$5%6^7&8*9(0)-_=+`¬[{]};:''@#~\|,<.>/?    ';

type
  TKeyboardEntryDlg = class(TSDUForm)
    pbScramble: TButton;
    edPassword: TEdit;
    pnlKey1: TPanel;
    pnlKey3: TPanel;
    pnlKey2: TPanel;
    pnlKey10: TPanel;
    pnlKey9: TPanel;
    pnlKey8: TPanel;
    pnlKey7: TPanel;
    pnlKey6: TPanel;
    pnlKey5: TPanel;
    pnlKey4: TPanel;
    pnlKey11: TPanel;
    pnlKey13: TPanel;
    pnlKey12: TPanel;
    pnlKey20: TPanel;
    pnlKey19: TPanel;
    pnlKey18: TPanel;
    pnlKey17: TPanel;
    pnlKey16: TPanel;
    pnlKey15: TPanel;
    pnlKey14: TPanel;
    pnlKey21: TPanel;
    pnlKey23: TPanel;
    pnlKey22: TPanel;
    pnlKey30: TPanel;
    pnlKey29: TPanel;
    pnlKey28: TPanel;
    pnlKey27: TPanel;
    pnlKey26: TPanel;
    pnlKey25: TPanel;
    pnlKey24: TPanel;
    pnlKey31: TPanel;
    pnlKey33: TPanel;
    pnlKey32: TPanel;
    pnlKey40: TPanel;
    pnlKey39: TPanel;
    pnlKey38: TPanel;
    pnlKey37: TPanel;
    pnlKey36: TPanel;
    pnlKey35: TPanel;
    pnlKey34: TPanel;
    pnlKey41: TPanel;
    pnlKey43: TPanel;
    pnlKey42: TPanel;
    pnlKey50: TPanel;
    pnlKey49: TPanel;
    pnlKey48: TPanel;
    pnlKey47: TPanel;
    pnlKey46: TPanel;
    pnlKey45: TPanel;
    pnlKey44: TPanel;
    ckShift: TCheckBox;
    pbCancel: TButton;
    Label1: TLabel;
    pbClear: TButton;
    igKey1: TImage;
    igKey2: TImage;
    igKey3: TImage;
    igKey4: TImage;
    igKey5: TImage;
    igKey6: TImage;
    igKey7: TImage;
    igKey8: TImage;
    igKey9: TImage;
    igKey10: TImage;
    igKey20: TImage;
    igKey19: TImage;
    igKey18: TImage;
    igKey17: TImage;
    igKey16: TImage;
    igKey15: TImage;
    igKey14: TImage;
    igKey13: TImage;
    igKey12: TImage;
    igKey11: TImage;
    igKey21: TImage;
    igKey22: TImage;
    igKey23: TImage;
    igKey24: TImage;
    igKey25: TImage;
    igKey26: TImage;
    igKey27: TImage;
    igKey28: TImage;
    igKey29: TImage;
    igKey30: TImage;
    igKey40: TImage;
    igKey39: TImage;
    igKey38: TImage;
    igKey37: TImage;
    igKey36: TImage;
    igKey35: TImage;
    igKey34: TImage;
    igKey33: TImage;
    igKey32: TImage;
    igKey31: TImage;
    igKey41: TImage;
    igKey42: TImage;
    igKey43: TImage;
    igKey44: TImage;
    igKey45: TImage;
    igKey46: TImage;
    igKey47: TImage;
    igKey48: TImage;
    igKey49: TImage;
    igKey50: TImage;
    pbOK: TButton;
    procedure pbScrambleClick(Sender: TObject);
    procedure ckShiftClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure igKeyClick(Sender: TObject);
    procedure igKeyDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    KeysArray: array [1..100] of char;
    procedure DefaultKeys();
    procedure PaintPanelBitmap(theString: string; thePicture: TPicture);
    procedure RandomizeCaption();
    procedure RefreshKeyTops();
  public
    Password: string;
    procedure BlankPassword();
  end;

implementation

{$R *.DFM}

procedure TKeyboardEntryDlg.pbScrambleClick(Sender: TObject);
var
  i: integer;
  firstKey: integer;
  secondKey: integer;
  tempA: char;
  tempB: char;
begin
  randomize;
  for i:=1 to 1000 do
    begin
    firstKey := (random(50)*2)+1;
    secondKey := (random(50)*2)+1;

    tempA := KeysArray[firstKey];
    tempB := KeysArray[firstKey+1];
    KeysArray[firstKey] := KeysArray[secondKey];
    KeysArray[firstKey+1] := KeysArray[secondKey+1];
    KeysArray[secondKey] := tempA;
    KeysArray[secondKey+1] := tempB;
    end;

  RefreshKeyTops();

end;

procedure TKeyboardEntryDlg.DefaultKeys();
var
  i: integer;
begin
  for i:=1 to 100 do
    begin
    KeysArray[i] := KEYS[i];
    end;

  ckShiftClick(nil);

end;

procedure TKeyboardEntryDlg.ckShiftClick(Sender: TObject);
begin
  RefreshKeyTops();

end;

procedure TKeyboardEntryDlg.FormCreate(Sender: TObject);
begin
  DefaultKeys();
  RandomizeCaption();

end;

procedure TKeyboardEntryDlg.pbClearClick(Sender: TObject);
begin
  edPassword.text := '';
  BlankPassword();

  RefreshKeyTops();

end;

procedure TKeyboardEntryDlg.PaintPanelBitmap(theString: string; thePicture: TPicture);
var
  bitmap: TBitmap;
  P : PByteArray;
  rNum: integer;
  x, y: integer;
begin
  if theString=' ' then
    begin
    theString:='<sp>';
    end;

  bitmap := thePicture.bitmap;
  bitmap.width := 19;
  bitmap.height := 19;

  // Ensure that the background for the image is clBtnFace
  bitmap.canvas.brush.color := clBtnFace;
  bitmap.canvas.FloodFill(0, 0, clBtnFace, fsBorder);

  // Set the font colour to a random colour that looks like black
  bitmap.canvas.Font.Color := $00ffffff - (random($00101010) OR $00efefef); //clBlack;
  bitmap.canvas.Font.CharSet := ANSI_CHARSET;

  bitmap.PixelFormat := pf24bit;

  // Write the caption on to the bitmap
  x := (bitmap.width - bitmap.canvas.TextExtent(theString).cx) div 2;
  y := (bitmap.height - bitmap.canvas.TextExtent(theString).cy) div 2;
  bitmap.canvas.textout(x, y, theString);

  // Speckle the bitmap
  for y := 0 to bitmap.height -1 do
    begin
    P := bitmap.ScanLine[y];
    for x := 0 to bitmap.width -1 do
      begin
      rNum :=random(200);
      if (rNum=0) then
        begin
        P[(x*3)]   := random($10); // blue
        P[(x*3)+1] := random($10); //green
        P[(x*3)+2] := random($10); //red
        end;

      end;
    end;

end;

procedure TKeyboardEntryDlg.igKeyClick(Sender: TObject);
var
  shifted: integer;
  number: integer;
begin
  if ckShift.checked then
    begin
    shifted := 1;
    end
  else
    begin
    shifted := 0;
    end;

  number:= ((TImage(Sender).tag-1)*2) +1;
  edPassword.text := edPassword.text + '*';
  Password := Password + KeysArray[number+shifted];

  RefreshKeyTops();

end;

procedure TKeyboardEntryDlg.BlankPassword();
var
  i: integer;
begin
  randomize;
  for i:=1 to length(Password) do
    begin
{$WARNINGS OFF}  // Disable useless warning
    Password[i] := chr(random(255));
    Password[i] := #0;
{$WARNINGS ON}
    end;

  Password := '';
end;

procedure TKeyboardEntryDlg.igKeyDblClick(Sender: TObject);
begin
  // Doubleclicking should add two chars, the first is already added on the
  // first click, so just add a second for the second click
  igKeyClick(Sender);
end;

procedure TKeyboardEntryDlg.RandomizeCaption();
var
  i: integer;
begin
  self.caption := '';
  for i:=1 to random(50) do
    begin
    self.caption := self.caption + KEYS[random(length(keys))];
    end;

end;

procedure TKeyboardEntryDlg.RefreshKeyTops();
var
  shifted: integer;
begin
  if ckShift.checked then
    begin
    shifted := 1;
    end
  else
    begin
    shifted := 0;
    end;

  randomize();

  PaintPanelBitmap(KeysArray[1+shifted], igKey1.picture);
  PaintPanelBitmap(KeysArray[3+shifted], igKey2.picture);
  PaintPanelBitmap(KeysArray[5+shifted], igKey3.picture);
  PaintPanelBitmap(KeysArray[7+shifted], igKey4.picture);
  PaintPanelBitmap(KeysArray[9+shifted], igKey5.picture);
  PaintPanelBitmap(KeysArray[11+shifted], igKey6.picture);
  PaintPanelBitmap(KeysArray[13+shifted], igKey7.picture);
  PaintPanelBitmap(KeysArray[15+shifted], igKey8.picture);
  PaintPanelBitmap(KeysArray[17+shifted], igKey9.picture);
  PaintPanelBitmap(KeysArray[19+shifted], igKey10.picture);
  PaintPanelBitmap(KeysArray[21+shifted], igKey11.picture);
  PaintPanelBitmap(KeysArray[23+shifted], igKey12.picture);
  PaintPanelBitmap(KeysArray[25+shifted], igKey13.picture);
  PaintPanelBitmap(KeysArray[27+shifted], igKey14.picture);
  PaintPanelBitmap(KeysArray[29+shifted], igKey15.picture);
  PaintPanelBitmap(KeysArray[31+shifted], igKey16.picture);
  PaintPanelBitmap(KeysArray[33+shifted], igKey17.picture);
  PaintPanelBitmap(KeysArray[35+shifted], igKey18.picture);
  PaintPanelBitmap(KeysArray[37+shifted], igKey19.picture);
  PaintPanelBitmap(KeysArray[39+shifted], igKey20.picture);
  PaintPanelBitmap(KeysArray[41+shifted], igKey21.picture);
  PaintPanelBitmap(KeysArray[43+shifted], igKey22.picture);
  PaintPanelBitmap(KeysArray[45+shifted], igKey23.picture);
  PaintPanelBitmap(KeysArray[47+shifted], igKey24.picture);
  PaintPanelBitmap(KeysArray[49+shifted], igKey25.picture);
  PaintPanelBitmap(KeysArray[51+shifted], igKey26.picture);
  PaintPanelBitmap(KeysArray[53+shifted], igKey27.picture);
  PaintPanelBitmap(KeysArray[55+shifted], igKey28.picture);
  PaintPanelBitmap(KeysArray[57+shifted], igKey29.picture);
  PaintPanelBitmap(KeysArray[59+shifted], igKey30.picture);
  PaintPanelBitmap(KeysArray[61+shifted], igKey31.picture);
  PaintPanelBitmap(KeysArray[63+shifted], igKey32.picture);
  PaintPanelBitmap(KeysArray[65+shifted], igKey33.picture);
  PaintPanelBitmap(KeysArray[67+shifted], igKey34.picture);
  PaintPanelBitmap(KeysArray[69+shifted], igKey35.picture);
  PaintPanelBitmap(KeysArray[71+shifted], igKey36.picture);
  PaintPanelBitmap(KeysArray[73+shifted], igKey37.picture);
  PaintPanelBitmap(KeysArray[75+shifted], igKey38.picture);
  PaintPanelBitmap(KeysArray[77+shifted], igKey39.picture);
  PaintPanelBitmap(KeysArray[79+shifted], igKey40.picture);
  PaintPanelBitmap(KeysArray[81+shifted], igKey41.picture);
  PaintPanelBitmap(KeysArray[83+shifted], igKey42.picture);
  PaintPanelBitmap(KeysArray[85+shifted], igKey43.picture);
  PaintPanelBitmap(KeysArray[87+shifted], igKey44.picture);
  PaintPanelBitmap(KeysArray[89+shifted], igKey45.picture);
  PaintPanelBitmap(KeysArray[91+shifted], igKey46.picture);
  PaintPanelBitmap(KeysArray[93+shifted], igKey47.picture);
  PaintPanelBitmap(KeysArray[95+shifted], igKey48.picture);
  PaintPanelBitmap(KeysArray[97+shifted], igKey49.picture);
  PaintPanelBitmap(KeysArray[99+shifted], igKey50.picture);
end;

procedure TKeyboardEntryDlg.FormShow(Sender: TObject);
begin
  randomize;
  self.left   := random(screen.width - self.width);
  self.top    := random(screen.height - self.height);
  self.height := self.height + random(10);
  self.width  := self.width + random(10);

end;

END.

