object OTFETrueCryptPasswordEntry_F: TOTFETrueCryptPasswordEntry_F
  Left = 444
  Top = 396
  BorderStyle = bsDialog
  Caption = 'Enter password'
  ClientHeight = 204
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 12
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = mePassword
  end
  object Label2: TLabel
    Left = 4
    Top = 56
    Width = 47
    Height = 13
    Caption = '&Mount as:'
    FocusControl = cbDrives
  end
  object mePassword: TMaskEdit
    Left = 60
    Top = 8
    Width = 217
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object cbDrives: TComboBox
    Left = 60
    Top = 52
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object ckHidePassword: TCheckBox
    Left = 180
    Top = 32
    Width = 97
    Height = 17
    Caption = '&Hide password'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = ckHidePasswordClick
  end
  object pbOK: TButton
    Left = 63
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object pbCancel: TButton
    Left = 147
    Top = 168
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ckCachePasswordInDriver: TCheckBox
    Left = 60
    Top = 120
    Width = 141
    Height = 17
    Caption = '&Cache password in driver'
    TabOrder = 3
  end
  object ckForceMount: TCheckBox
    Left = 60
    Top = 140
    Width = 97
    Height = 17
    Caption = '&Force mount'
    TabOrder = 4
  end
  object ckMountReadonly: TCheckBox
    Left = 60
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Mount &readonly'
    TabOrder = 7
  end
  object ckMountAsRemovable: TCheckBox
    Left = 60
    Top = 100
    Width = 157
    Height = 17
    Caption = 'Mount as removable &device'
    TabOrder = 8
  end
end
