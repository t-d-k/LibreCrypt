object frmPKCS11Session: TfrmPKCS11Session
  Left = 299
  Top = 227
  BorderStyle = bsDialog
  Caption = 'Security Token/Smartcard PIN'
  ClientHeight = 177
  ClientWidth = 289
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
  object lblPIN: TLabel
    Left = 11
    Top = 89
    Width = 21
    Height = 13
    Caption = '&PIN:'
    FocusControl = edPIN
  end
  object lblToken: TLabel
    Left = 11
    Top = 59
    Width = 34
    Height = 13
    Caption = '&Token:'
    FocusControl = cbToken
  end
  object Label3: TLabel
    Left = 25
    Top = 12
    Width = 239
    Height = 26
    Caption = 
      'Please select appropriate security token/smartcard and enter its' +
      ' PIN'
    WordWrap = True
  end
  object cbToken: TComboBox
    Left = 51
    Top = 54
    Width = 145
    Height = 21
    TabOrder = 1
    Text = 'cbToken'
  end
  object edPIN: TEdit
    Left = 51
    Top = 84
    Width = 141
    Height = 21
    TabOrder = 2
    Text = 'edPIN'
  end
  object pbOK: TButton
    Left = 23
    Top = 140
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 191
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object pbRefresh: TButton
    Left = 202
    Top = 49
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 0
    OnClick = pbRefreshClick
  end
  object ckUseSecureAuthPath: TCheckBox
    Left = 51
    Top = 109
    Width = 182
    Height = 17
    Caption = 'Use &secure authentication path'
    TabOrder = 3
    OnClick = ckUseSecureAuthPathClick
  end
  object pbSkip: TButton
    Left = 107
    Top = 140
    Width = 75
    Height = 25
    Caption = 'Skip'
    ModalResult = 1
    TabOrder = 5
  end
end
