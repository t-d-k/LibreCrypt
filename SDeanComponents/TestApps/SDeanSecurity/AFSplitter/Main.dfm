object frmMain: TfrmMain
  Left = 307
  Top = 221
  Caption = 'APPLICATION TITLE SET AUTOMATICALLY'
  ClientHeight = 174
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 76
    Width = 85
    Height = 13
    Caption = '&Number of stripes:'
    FocusControl = seStripeCount
  end
  object Label2: TLabel
    Left = 12
    Top = 20
    Width = 43
    Height = 13
    Caption = '&Input file:'
    FocusControl = edFilenameInput
  end
  object Label3: TLabel
    Left = 12
    Top = 104
    Width = 51
    Height = 13
    Caption = '&Output file:'
    FocusControl = edFilenameOutput
  end
  object Label4: TLabel
    Left = 12
    Top = 48
    Width = 73
    Height = 13
    Caption = '&Hash algorithm:'
    FocusControl = cbHash
  end
  object seStripeCount: TSpinEdit64
    Left = 104
    Top = 72
    Width = 121
    Height = 22
    Increment = 1
    TabOrder = 3
  end
  object edFilenameInput: TEdit
    Left = 104
    Top = 16
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'edFilenameInput'
  end
  object edFilenameOutput: TEdit
    Left = 104
    Top = 100
    Width = 273
    Height = 21
    TabOrder = 4
    Text = 'edFilenameOutput'
  end
  object pbSplit: TButton
    Left = 121
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Split'
    TabOrder = 6
    OnClick = pbSplitClick
  end
  object pbMerge: TButton
    Left = 217
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Merge'
    TabOrder = 7
    OnClick = pbMergeClick
  end
  object pbBrowseInput: TButton
    Left = 380
    Top = 16
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = pbBrowseInputClick
  end
  object pbBrowseOutput: TButton
    Left = 380
    Top = 100
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = pbBrowseOutputClick
  end
  object cbHash: TComboBox
    Left = 104
    Top = 44
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object OpenDialog1: TSDUOpenDialog
    PreserveCWD = False
    Left = 368
    Top = 32
  end
  object SaveDialog1: TSDUSaveDialog
    PreserveCWD = False
    Left = 368
    Top = 112
  end
end
