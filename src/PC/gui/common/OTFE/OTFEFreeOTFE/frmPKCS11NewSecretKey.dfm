object frmPKCS11NewSecretKey: TfrmPKCS11NewSecretKey
  Left = 479
  Top = 421
  BorderStyle = bsDialog
  Caption = 'New PKCS#11 Secret Key'
  ClientHeight = 146
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 76
    Width = 39
    Height = 13
    Caption = '&Cypher:'
    FocusControl = cbKeyType
  end
  object Label2: TLabel
    Left = 12
    Top = 44
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = edLabel
  end
  object Label3: TLabel
    Left = 12
    Top = 8
    Width = 279
    Height = 13
    Caption = 'Please specify a name and cypher for the new secret key:'
    WordWrap = True
  end
  object pbCancel: TButton
    Left = 166
    Top = 108
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object pbOK: TButton
    Left = 66
    Top = 108
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object cbKeyType: TComboBox
    Left = 68
    Top = 72
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbKeyTypeChange
  end
  object edLabel: TEdit
    Left = 68
    Top = 40
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'edLabel'
    OnChange = edLabelChange
  end
end
