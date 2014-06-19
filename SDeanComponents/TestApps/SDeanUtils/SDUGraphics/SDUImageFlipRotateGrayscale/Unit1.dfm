object Form1: TForm1
  Left = 212
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 633
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image_Flip_H: TImage
    Left = 216
    Top = 196
    Width = 105
    Height = 105
  end
  object Image_Flip_V: TImage
    Left = 324
    Top = 196
    Width = 105
    Height = 105
  end
  object Image_Rotate_90: TImage
    Left = 216
    Top = 356
    Width = 105
    Height = 105
  end
  object Image_Flip_HV: TImage
    Left = 436
    Top = 196
    Width = 105
    Height = 105
  end
  object Image_Rotate_180: TImage
    Left = 324
    Top = 356
    Width = 105
    Height = 105
  end
  object Image_Rotate_270: TImage
    Left = 436
    Top = 356
    Width = 105
    Height = 105
  end
  object Image_Original: TImage
    Left = 272
    Top = 56
    Width = 105
    Height = 105
  end
  object Image_Rotate_0: TImage
    Left = 104
    Top = 356
    Width = 105
    Height = 105
  end
  object Image_Flip_None: TImage
    Left = 104
    Top = 196
    Width = 105
    Height = 105
  end
  object Label1: TLabel
    Left = 476
    Top = 312
    Width = 22
    Height = 13
    Caption = 'Both'
  end
  object Label2: TLabel
    Left = 360
    Top = 312
    Width = 35
    Height = 13
    Caption = 'Vertical'
  end
  object Label3: TLabel
    Left = 244
    Top = 312
    Width = 47
    Height = 13
    Caption = 'Horizontal'
  end
  object Label4: TLabel
    Left = 352
    Top = 468
    Width = 59
    Height = 13
    Caption = '180 degrees'
  end
  object Label5: TLabel
    Left = 144
    Top = 312
    Width = 26
    Height = 13
    Caption = 'None'
  end
  object Label6: TLabel
    Left = 132
    Top = 468
    Width = 47
    Height = 13
    Caption = '0 degrees'
  end
  object Label7: TLabel
    Left = 456
    Top = 468
    Width = 59
    Height = 13
    Caption = '270 degrees'
  end
  object Label8: TLabel
    Left = 240
    Top = 468
    Width = 53
    Height = 13
    Caption = '90 degrees'
  end
  object Label9: TLabel
    Left = 12
    Top = 400
    Width = 39
    Height = 13
    Caption = 'Rotate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 12
    Top = 236
    Width = 21
    Height = 13
    Caption = 'Flip'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel
    Left = 12
    Top = 96
    Width = 44
    Height = 13
    Caption = 'Original'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Image_Grayscale: TImage
    Left = 272
    Top = 512
    Width = 105
    Height = 105
  end
  object Label12: TLabel
    Left = 12
    Top = 544
    Width = 57
    Height = 13
    Caption = 'Grayscale'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label13: TLabel
    Left = 142
    Top = 12
    Width = 186
    Height = 13
    Caption = 'Click here to load a different test image:'
  end
  object pbLoad: TButton
    Left = 342
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Load...'
    TabOrder = 0
    OnClick = pbLoadClick
  end
  object pnlSplitOne: TPanel
    Left = 104
    Top = 176
    Width = 437
    Height = 9
    Caption = 'pnlSplitOne'
    TabOrder = 1
  end
  object pnlSplitTwo: TPanel
    Left = 104
    Top = 336
    Width = 437
    Height = 9
    Caption = 'pnlSplitTwo'
    TabOrder = 2
  end
  object pnlSplitThree: TPanel
    Left = 104
    Top = 492
    Width = 437
    Height = 9
    Caption = 'pnlSplitThree'
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Left = 60
    Top = 4
  end
end
