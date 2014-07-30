object cryptlibAPITest: TcryptlibAPITest
  Left = 347
  Top = 201
  Caption = 'Test cryptlib API'
  ClientHeight = 613
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 60
    Top = 392
    Width = 40
    Height = 13
    Caption = 'Step: 10'
  end
  object Label5: TLabel
    Left = 60
    Top = 356
    Width = 34
    Height = 13
    Caption = 'Step: 9'
  end
  object Label6: TLabel
    Left = 60
    Top = 320
    Width = 34
    Height = 13
    Caption = 'Step: 8'
  end
  object Label7: TLabel
    Left = 60
    Top = 284
    Width = 34
    Height = 13
    Caption = 'Step: 7'
  end
  object Label8: TLabel
    Left = 60
    Top = 248
    Width = 34
    Height = 13
    Caption = 'Step: 6'
  end
  object Label9: TLabel
    Left = 60
    Top = 212
    Width = 34
    Height = 13
    Caption = 'Step: 5'
  end
  object Label10: TLabel
    Left = 60
    Top = 176
    Width = 34
    Height = 13
    Caption = 'Step: 4'
  end
  object Label11: TLabel
    Left = 60
    Top = 104
    Width = 34
    Height = 13
    Caption = 'Step: 2'
  end
  object Label12: TLabel
    Left = 60
    Top = 68
    Width = 34
    Height = 13
    Caption = 'Step: 1'
  end
  object Label1: TLabel
    Left = 300
    Top = 504
    Width = 25
    Height = 13
    Caption = 'bytes'
  end
  object Label2: TLabel
    Left = 60
    Top = 140
    Width = 34
    Height = 13
    Caption = 'Step: 3'
  end
  object pbLOADDLL: TButton
    Left = 120
    Top = 64
    Width = 121
    Height = 25
    Caption = 'LOAD DLL'
    TabOrder = 0
    OnClick = pbLOADDLLClick
  end
  object pbCreateContext: TButton
    Left = 144
    Top = 208
    Width = 121
    Height = 25
    Caption = 'cryptCreateContext'
    TabOrder = 1
    OnClick = pbCreateContextClick
  end
  object pbUNLOADDLL: TButton
    Left = 120
    Top = 388
    Width = 121
    Height = 25
    Caption = 'UNLOAD DLL'
    TabOrder = 2
    OnClick = pbUNLOADDLLClick
  end
  object pbCryptInit: TButton
    Left = 132
    Top = 100
    Width = 121
    Height = 25
    Caption = 'cryptInit'
    TabOrder = 3
    OnClick = pbCryptInitClick
  end
  object pbCryptEnd: TButton
    Left = 132
    Top = 352
    Width = 121
    Height = 25
    Caption = 'cryptEnd'
    TabOrder = 4
    OnClick = pbCryptEndClick
  end
  object pbDestroyContext: TButton
    Left = 144
    Top = 316
    Width = 121
    Height = 25
    Caption = 'cryptDestroyContext'
    TabOrder = 5
    OnClick = pbDestroyContextClick
  end
  object pbCryptGenerateKey: TButton
    Left = 156
    Top = 244
    Width = 121
    Height = 25
    Caption = 'cryptGenerateKey'
    TabOrder = 6
    OnClick = pbCryptGenerateKeyClick
  end
  object pbCryptEncrypt: TButton
    Left = 156
    Top = 280
    Width = 121
    Height = 25
    Caption = 'cryptEncrypt'
    TabOrder = 7
    OnClick = pbCryptEncryptClick
  end
  object pbCryptAddRandom: TButton
    Left = 144
    Top = 172
    Width = 121
    Height = 25
    Caption = 'cryptAddRandom'
    TabOrder = 8
    OnClick = pbCryptAddRandomClick
  end
  object reReport: TRichEdit
    Left = 372
    Top = 36
    Width = 449
    Height = 513
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'reReport')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object pbClear: TButton
    Left = 600
    Top = 568
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object pbGenerateRandom: TButton
    Left = 212
    Top = 528
    Width = 117
    Height = 25
    Caption = 'Generate random'
    TabOrder = 11
    OnClick = pbGenerateRandomClick
  end
  object seGenerateBytes: TSpinEdit64
    Left = 212
    Top = 500
    Width = 81
    Height = 22
    Increment = 1
    TabOrder = 12
  end
  object RichEdit1: TRichEdit
    Left = 36
    Top = 476
    Width = 165
    Height = 101
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'Note: Click:'
      ''
      '*) LOAD DLL'
      '*) cryptInit'
      '*) cryptAddRandom'
      ''
      'before generating random data.')
    ParentFont = False
    ReadOnly = True
    TabOrder = 13
  end
  object Panel1: TPanel
    Left = 84
    Top = 440
    Width = 185
    Height = 3
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 14
  end
  object pbCryptQueryCapability: TButton
    Left = 144
    Top = 136
    Width = 121
    Height = 25
    Caption = 'cryptQueryCapability'
    TabOrder = 15
    OnClick = pbCryptQueryCapabilityClick
  end
end
