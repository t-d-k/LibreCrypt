object Form2: TForm2
  Left = 277
  Top = 179
  Width = 203
  Height = 179
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 8
    Width = 185
    Height = 97
    Caption = 'Programatic test operations'
    TabOrder = 0
    object pbAppMinimise: TButton
      Left = 12
      Top = 24
      Width = 75
      Height = 25
      Caption = 'App minimize'
      TabOrder = 0
      OnClick = pbAppMinimiseClick
    end
    object pbClose: TButton
      Left = 96
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = pbCloseClick
    end
    object pbWSMinimise: TButton
      Left = 12
      Top = 56
      Width = 75
      Height = 25
      Caption = 'WS minimize'
      TabOrder = 2
      OnClick = pbWSMinimiseClick
    end
    object pbHide: TButton
      Left = 96
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Hide'
      TabOrder = 3
      OnClick = pbHideClick
    end
  end
  object pbOK: TButton
    Left = 60
    Top = 116
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
  end
end
