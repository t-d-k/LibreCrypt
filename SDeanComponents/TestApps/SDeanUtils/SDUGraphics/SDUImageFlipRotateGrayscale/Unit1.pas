unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  SDUForms, SDUGraphics;

type
  TForm1 = class(TSDUForm)
    Image_Original: TImage;
    Image_Flip_None: TImage;
    Image_Flip_H: TImage;
    Image_Flip_V: TImage;
    Image_Flip_HV: TImage;
    Image_Rotate_0: TImage;
    Image_Rotate_90: TImage;
    Image_Rotate_180: TImage;
    Image_Rotate_270: TImage;
    pbLoad: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Image_Grayscale: TImage;
    Label12: TLabel;
    Label13: TLabel;
    pnlSplitOne: TPanel;
    pnlSplitTwo: TPanel;
    pnlSplitThree: TPanel;
    procedure pbLoadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure DemoImage(filename: string);

    procedure DemoImage_Flip(filename: string; flip: TImageFlip; dispImage: TImage);
    procedure DemoImage_Rotate(filename: string; rotate: TImageRotate; dispImage: TImage);
    procedure DemoImage_Grayscale(filename: string; dispImage: TImage);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  // Files containing test images
  TEST_IMAGE_FILE_pf1bit  = 'TestImages\pf1bit.bmp';
  TEST_IMAGE_FILE_pf4bit  = 'TestImages\pf4bit.bmp';
  TEST_IMAGE_FILE_pf8bit  = 'TestImages\pf8bit.bmp';
  TEST_IMAGE_FILE_pf24bit = 'TestImages\pf24bit.bmp';

procedure TForm1.DemoImage(filename: string);
begin
  Image_Original.Picture.Bitmap.LoadFromFile(filename);

  DemoImage_Flip(filename, ifNone,       Image_Flip_None);
  DemoImage_Flip(filename, ifHorizontal, Image_Flip_H);
  DemoImage_Flip(filename, ifVertical,   Image_Flip_V);
  DemoImage_Flip(filename, ifBoth,       Image_Flip_HV);

  DemoImage_Rotate(filename, rot0,   Image_Rotate_0);
  DemoImage_Rotate(filename, rot90,  Image_Rotate_90);
  DemoImage_Rotate(filename, rot180, Image_Rotate_180);
  DemoImage_Rotate(filename, rot270, Image_Rotate_270);

  DemoImage_Grayscale(filename, Image_Grayscale);
end;

procedure TForm1.DemoImage_Flip(filename: string; flip: TImageFlip; dispImage: TImage);
var
  t: TBitmap;
begin
  t:= TBitmap.Create();
  try
    t.LoadFromFile(filename);
    SDUImageFlip(t, flip);
    dispImage.Picture.Assign(t);
  finally
    t.Free();
  end;

end;

procedure TForm1.DemoImage_Rotate(filename: string; rotate: TImageRotate; dispImage: TImage);
var
  t: TBitmap;
begin
  t:= TBitmap.Create();
  try
    t.LoadFromFile(filename);
    SDUImageRotate(t, rotate);
    dispImage.Picture.Assign(t);
  finally
    t.Free();
  end;

end;

procedure TForm1.DemoImage_Grayscale(filename: string; dispImage: TImage);
var
  t: TBitmap;
begin
  t:= TBitmap.Create();
  try
    t.LoadFromFile(filename);
    SDUImageGrayscale(t);
    dispImage.Picture.Assign(t);
  finally
    t.Free();
  end;

end;

procedure TForm1.pbLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute() then
  begin
    DemoImage(OpenDialog1.Filename);
  end;

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  pnlSplitOne.Height := 3;
  pnlSplitOne.Caption := '';
  pnlSplitOne.bevelouter := bvLowered;
  pnlSplitTwo.Height := 3;
  pnlSplitTwo.Caption := '';
  pnlSplitTwo.bevelouter := bvLowered;
  pnlSplitThree.Height := 3;
  pnlSplitThree.Caption := '';
  pnlSplitThree.bevelouter := bvLowered;

  self.caption := Application.Title;

  // Load and demo some initial imagef
  DemoImage(TEST_IMAGE_FILE_pf24bit);
end;

END.


