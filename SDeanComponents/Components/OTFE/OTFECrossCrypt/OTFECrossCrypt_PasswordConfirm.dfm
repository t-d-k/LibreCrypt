object OTFECrossCrypt_PasswordConfirm_F: TOTFECrossCrypt_PasswordConfirm_F
  Left = 408
  Top = 336
  Caption = 'Password Confirmation'
  ClientHeight = 174
  ClientWidth = 284
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
  object lblMultipleKeyMode: TLabel
    Left = 28
    Top = 32
    Width = 91
    Height = 13
    Caption = 'lblMultipleKeyMode'
  end
  object lblPasswordCount: TLabel
    Left = 27
    Top = 108
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
  object Label1: TLabel
    Left = 132
    Top = 24
    Width = 106
    Height = 26
    Caption = '<<< This label gets automatically centered'
    Color = clRed
    ParentColor = False
    Visible = False
    WordWrap = True
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
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = rePasswordsChange
  end
  object pbCancel: TButton
    Left = 150
    Top = 148
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = pbCancelClick
  end
  object pbOK: TButton
    Left = 66
    Top = 148
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object pbLoadFromFile: TButton
    Left = 188
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 3
    OnClick = pbLoadFromFileClick
  end
  object OpenKeyfileDlg: TOpenDialog
    Options = [ofEnableSizing]
    Left = 232
    Top = 128
  end
end
