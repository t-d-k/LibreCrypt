object CrossCrypt_PasswordConfirm_F: TCrossCrypt_PasswordConfirm_F
  Left = 408
  Top = 336
  Caption = 'Password confirmation'
  ClientHeight = 167
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblMultipleKeyMode: TLabel
    Left = 100
    Top = 32
    Width = 91
    Height = 13
    Caption = 'lblMultipleKeyMode'
  end
  object lblPasswordCount: TLabel
    Left = 83
    Top = 112
    Width = 125
    Height = 13
    Caption = 'Passwords entered: 64/64'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 275
    Height = 13
    Caption = 'Please confirm your password(s) by reentering them below:'
  end
  object rePasswords: TRichEdit
    Left = 27
    Top = 52
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
    TabOrder = 0
    OnChange = rePasswordsChange
  end
  object pbCancel: TButton
    Left = 150
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pbOK: TButton
    Left = 66
    Top = 140
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
  end
end
