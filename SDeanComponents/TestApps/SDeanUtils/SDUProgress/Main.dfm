object Form2: TForm2
  Left = 410
  Top = 349
  Caption = 'test app for progress dialogs used in lc'
  ClientHeight = 307
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    234
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 20
    Top = 12
    Width = 204
    Height = 142
    Anchors = [akLeft, akTop, akRight]
    Caption = 'dlgProgress'
    TabOrder = 0
    object pbprogress: TButton
      Left = 67
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 0
      OnClick = pbprogressClick
    end
    object cbShowTimeRemaining: TCheckBox
      Left = 51
      Top = 81
      Width = 132
      Height = 17
      Caption = 'Show Time Remaining'
      TabOrder = 1
    end
    object cbShowStatusText: TCheckBox
      Left = 51
      Top = 120
      Width = 132
      Height = 17
      Caption = 'Show Status Text'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 22
    Top = 160
    Width = 204
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'SDUWindowsProgressDialog'
    TabOrder = 1
    object pbTestSDUWindowsProgressDialog: TButton
      Left = 65
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 0
      OnClick = pbTestSDUWindowsProgressDialogClick
    end
  end
  object pbClose: TButton
    Left = 87
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = pbCloseClick
  end
end
