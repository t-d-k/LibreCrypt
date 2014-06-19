object Form2: TForm2
  Left = 410
  Top = 349
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 271
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 20
    Top = 12
    Width = 161
    Height = 117
    Caption = 'SDUProgressDialog'
    TabOrder = 0
    object pbWithStatusText: TButton
      Left = 43
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Test two'
      TabOrder = 1
      OnClick = pbWithStatusTextClick
    end
    object pbNoStatusText: TButton
      Left = 43
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Test one'
      TabOrder = 0
      OnClick = pbNoStatusTextClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 20
    Top = 144
    Width = 161
    Height = 73
    Caption = 'SDUWindowsProgressDialog'
    TabOrder = 1
    object pbTestSDUWindowsProgressDialog: TButton
      Left = 43
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 0
      OnClick = pbTestSDUWindowsProgressDialogClick
    end
  end
  object pbClose: TButton
    Left = 64
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = pbCloseClick
  end
end
