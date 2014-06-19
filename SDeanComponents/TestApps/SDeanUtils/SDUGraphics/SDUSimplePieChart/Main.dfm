object frmMain: TfrmMain
  Left = 336
  Top = 229
  BorderStyle = bsDialog
  Caption = 'APPLICATION TITLE SET AUTOMATICALLY'
  ClientHeight = 488
  ClientWidth = 184
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
    Left = 24
    Top = 16
    Width = 58
    Height = 13
    Caption = '&Percentage:'
    FocusControl = sePercent
  end
  object Label2: TLabel
    Left = 152
    Top = 16
    Width = 8
    Height = 13
    Caption = '%'
  end
  object ListBox1: TListBox
    Left = 32
    Top = 52
    Width = 121
    Height = 381
    ItemHeight = 13
    TabOrder = 1
    OnDrawItem = ListBox1DrawItem
  end
  object sePercent: TSpinEdit64
    Left = 88
    Top = 12
    Width = 57
    Height = 22
    Increment = 1
    TabOrder = 0
    OnChange = sePercentChange
  end
  object pbClose: TButton
    Left = 54
    Top = 448
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 2
    OnClick = pbCloseClick
  end
end
