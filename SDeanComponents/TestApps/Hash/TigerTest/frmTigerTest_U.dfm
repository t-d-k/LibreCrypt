object frmTigerTest: TfrmTigerTest
  Left = 583
  Top = 412
  Width = 556
  Height = 432
  Caption = 'frmTigerTest'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    548
    405)
  PixelsPerInch = 96
  TextHeight = 13
  object reResults: TRichEdit
    Left = 4
    Top = 4
    Width = 449
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    Lines.Strings = (
      'reResults')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object pbClear: TButton
    Left = 464
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 2
    OnClick = pbClearClick
  end
  object pbClose: TButton
    Left = 464
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = pbCloseClick
  end
  object GroupBox1: TGroupBox
    Left = 216
    Top = 300
    Width = 197
    Height = 97
    Anchors = [akLeft, akBottom]
    Caption = 'NESSIE Test sets'
    TabOrder = 1
    object pbNESSIETestSet1: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Test set &1'
      TabOrder = 0
      OnClick = pbNESSIETestSet1Click
    end
    object pbNESSIETestSet2: TButton
      Left = 104
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Test set &2'
      TabOrder = 1
      OnClick = pbNESSIETestSet2Click
    end
    object pbNESSIETestSet4: TButton
      Left = 104
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Test set &4'
      TabOrder = 3
      OnClick = pbNESSIETestSet4Click
    end
    object pbNESSIETestSet3: TButton
      Left = 16
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Test set &3'
      TabOrder = 2
      OnClick = pbNESSIETestSet3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 48
    Top = 300
    Width = 109
    Height = 97
    Anchors = [akLeft, akBottom]
    Caption = 'Tiger standard set'
    TabOrder = 0
    object pbStdTest: TButton
      Left = 16
      Top = 28
      Width = 75
      Height = 25
      Caption = '&Test'
      TabOrder = 0
      OnClick = pbStdTestClick
    end
  end
  object HashAlgTiger: THashAlgTiger
    Left = 484
    Top = 80
  end
end
