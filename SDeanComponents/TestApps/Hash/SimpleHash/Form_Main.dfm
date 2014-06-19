object frmMain: TfrmMain
  Left = 246
  Top = 192
  Width = 341
  Height = 414
  Caption = 'APPLICATION TITLE SET AUTOMATICALLY'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 5
    Width = 290
    Height = 26
    Caption = 
      'This sample application demonstrates how to use the "Hash" packa' +
      'ge for generating hash values.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 10
    Top = 40
    Width = 304
    Height = 39
    Caption = 
      'Although this test application only generates MD5 hashes, by usi' +
      'ng the other components supplied, other hash algorithms may be u' +
      'sed'
    WordWrap = True
  end
  object edText: TEdit
    Left = 65
    Top = 90
    Width = 256
    Height = 21
    TabOrder = 0
    Text = 'edText'
  end
  object pbHashInput: TButton
    Left = 129
    Top = 150
    Width = 75
    Height = 25
    Caption = 'Hash input'
    Default = True
    TabOrder = 1
    OnClick = pbHashInputClick
  end
  object edFilename: TEdit
    Left = 65
    Top = 115
    Width = 231
    Height = 21
    TabOrder = 2
    Text = 'edFilename'
  end
  object pbBrowse: TButton
    Left = 300
    Top = 115
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = pbBrowseClick
  end
  object rbHashText: TRadioButton
    Left = 10
    Top = 90
    Width = 51
    Height = 17
    Caption = 'Text'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object rbHashFile: TRadioButton
    Left = 10
    Top = 115
    Width = 51
    Height = 17
    Caption = 'File'
    TabOrder = 5
  end
  object mmoReport: TMemo
    Left = 10
    Top = 185
    Width = 311
    Height = 161
    Lines.Strings = (
      'mmoReport')
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object pbClose: TButton
    Left = 245
    Top = 355
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 7
    OnClick = pbCloseClick
  end
  object hashAlg: THashAlgMD5
    Left = 200
    Top = 140
  end
  object OpenDialog1: TOpenDialog
    Left = 280
    Top = 125
  end
end
