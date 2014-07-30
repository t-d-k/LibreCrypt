object OTFECrossCrypt_PasswordEntry_F: TOTFECrossCrypt_PasswordEntry_F
  Left = 513
  Top = 426
  Caption = 'CrossCrypt Password'
  ClientHeight = 250
  ClientWidth = 329
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
    Left = 24
    Top = 36
    Width = 60
    Height = 13
    Caption = '&Password(s):'
    FocusControl = ckMultipleKey
  end
  object Label2: TLabel
    Left = 48
    Top = 12
    Width = 33
    Height = 13
    Caption = '&Cipher:'
    FocusControl = cbCipher
  end
  object Label3: TLabel
    Left = 8
    Top = 148
    Width = 73
    Height = 13
    Caption = 'Mount as &drive:'
    FocusControl = cbDrive
  end
  object lblPasswordCount: TLabel
    Left = 200
    Top = 36
    Width = 125
    Height = 13
    Caption = 'Passwords entered: 64/64'
  end
  object cbCipher: TComboBox
    Left = 88
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbCipherChange
  end
  object pbOK: TButton
    Left = 89
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 7
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 173
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = pbCancelClick
  end
  object cbDrive: TComboBox
    Left = 88
    Top = 144
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 4
  end
  object ckReadonly: TCheckBox
    Left = 88
    Top = 192
    Width = 121
    Height = 17
    Caption = 'Mount as &readonly'
    TabOrder = 6
  end
  object ckMountAsCD: TCheckBox
    Left = 88
    Top = 172
    Width = 97
    Height = 17
    Caption = '&Mount as CD'
    TabOrder = 5
    OnClick = ckMountAsCDClick
  end
  object ckMultipleKey: TCheckBox
    Left = 88
    Top = 36
    Width = 97
    Height = 17
    Caption = '&Multiple keys'
    TabOrder = 1
    OnClick = ckMultipleKeyClick
  end
  object rePasswords: TRichEdit
    Left = 88
    Top = 56
    Width = 237
    Height = 53
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'rePasswords')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    OnChange = rePasswordsChange
  end
  object pbLoadFromFile: TButton
    Left = 252
    Top = 112
    Width = 75
    Height = 25
    Caption = '&Load...'
    TabOrder = 3
    OnClick = pbLoadFromFileClick
  end
  object OpenKeyfileDlg: TOpenDialog
    Options = [ofEnableSizing]
    Left = 280
    Top = 132
  end
end
