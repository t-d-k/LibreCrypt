object Main_F: TMain_F
  Left = 286
  Top = 207
  BorderStyle = bsDialog
  Caption = 'FORM CAPTION SET AUTOMATICALLY ON START UP'
  ClientHeight = 138
  ClientWidth = 308
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
    Left = 8
    Top = 16
    Width = 45
    Height = 13
    Caption = 'Filename:'
    FocusControl = edFilename
  end
  object Label2: TLabel
    Left = 12
    Top = 44
    Width = 40
    Height = 13
    Caption = 'File size:'
    FocusControl = se64FileSize
  end
  object pbCreate: TButton
    Left = 72
    Top = 100
    Width = 75
    Height = 25
    Caption = '&Create'
    TabOrder = 0
    OnClick = pbCreateClick
  end
  object edFilename: TEdit
    Left = 60
    Top = 12
    Width = 217
    Height = 21
    TabOrder = 1
    Text = 'edFilename'
  end
  object se64FileSize: TSpinEdit64
    Left = 60
    Top = 40
    Width = 169
    Height = 22
    Increment = 1
    TabOrder = 2
  end
  object cbUnits: TComboBox
    Left = 236
    Top = 40
    Width = 61
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object pbClose: TButton
    Left = 164
    Top = 100
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = pbCloseClick
  end
  object cbShowProgress: TCheckBox
    Left = 60
    Top = 68
    Width = 97
    Height = 17
    Caption = 'Show progress'
    TabOrder = 5
  end
  object pbBrowse: TButton
    Left = 276
    Top = 12
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = pbBrowseClick
  end
  object SaveDialog1: TSDUSaveDialog
    Left = 260
  end
end
