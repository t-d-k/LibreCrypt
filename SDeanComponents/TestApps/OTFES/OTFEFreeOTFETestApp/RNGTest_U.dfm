object RNGTest: TRNGTest
  Left = 232
  Top = 139
  Caption = 'RNGTest'
  ClientHeight = 470
  ClientWidth = 835
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblGPGFilename: TLabel
    Left = 136
    Top = 284
    Width = 68
    Height = 13
    Caption = 'GPG filename:'
  end
  object lblMouseRNGBits: TLabel
    Left = 128
    Top = 235
    Width = 169
    Height = 13
    Caption = 'Random bits generated: 9999/9999'
  end
  object pbClose: TButton
    Left = 748
    Top = 432
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 0
    OnClick = pbCloseClick
  end
  object reReport: TRichEdit
    Left = 504
    Top = 8
    Width = 321
    Height = 409
    Lines.Strings = (
      'reReport')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object pbGenerateRandomData: TButton
    Left = 376
    Top = 437
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 2
    OnClick = pbGenerateRandomDataClick
  end
  object pbClear: TButton
    Left = 560
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = pbClearClick
  end
  object edGPGFilename: TEdit
    Left = 216
    Top = 280
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'gpg.exe'
  end
  object MouseRNG: TMouseRNG
    Left = 8
    Top = 8
    Width = 472
    Height = 222
    TrailLines = 5
    LineWidth = 5
    LineColor = clNavy
    UseDockManager = False
    Enabled = False
    ParentColor = False
    OnByteGenerated = MouseRNGByteGenerated
  end
  object Panel1: TPanel
    Left = 24
    Top = 264
    Width = 441
    Height = 3
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 6
  end
  object Panel2: TPanel
    Left = 24
    Top = 316
    Width = 441
    Height = 3
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 7
  end
  object Panel3: TPanel
    Left = 488
    Top = 24
    Width = 3
    Height = 413
    BevelOuter = bvLowered
    Caption = 'Panel3'
    TabOrder = 8
  end
  object gbRNG: TGroupBox
    Left = 136
    Top = 325
    Width = 187
    Height = 125
    Caption = 'Random number generator (RNG)'
    TabOrder = 9
    object ckRNGMouseMovement: TCheckBox
      Left = 16
      Top = 48
      Width = 141
      Height = 17
      Caption = 'Mouse movement'
      TabOrder = 1
    end
    object ckRNGCryptoAPI: TCheckBox
      Left = 16
      Top = 24
      Width = 125
      Height = 17
      Caption = 'Microsoft CryptoAPI'
      TabOrder = 0
    end
    object ckRNGcryptlib: TCheckBox
      Left = 16
      Top = 72
      Width = 97
      Height = 17
      Caption = 'cryptlib'
      TabOrder = 2
    end
    object ckRNGGPG: TCheckBox
      Left = 16
      Top = 96
      Width = 97
      Height = 17
      Caption = 'GPG'
      TabOrder = 3
    end
  end
end
