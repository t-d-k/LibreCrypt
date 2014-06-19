object OTFEE4MPasswordEntry_F: TOTFEE4MPasswordEntry_F
  Left = 444
  Top = 396
  BorderStyle = bsDialog
  Caption = 'Enter password'
  ClientHeight = 123
  ClientWidth = 285
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
  object Label1: TLabel
    Left = 20
    Top = 12
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = mePassword
  end
  object Label2: TLabel
    Left = 4
    Top = 56
    Width = 73
    Height = 13
    Caption = 'Mount as &drive:'
    FocusControl = cbDrives
  end
  object mePassword: TMaskEdit
    Left = 80
    Top = 8
    Width = 193
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object cbDrives: TComboBox
    Left = 80
    Top = 52
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object ckHidePassword: TCheckBox
    Left = 180
    Top = 32
    Width = 97
    Height = 17
    Caption = '&Hide password'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = ckHidePasswordClick
  end
  object pbOK: TButton
    Left = 116
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object pbCancel: TButton
    Left = 200
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
