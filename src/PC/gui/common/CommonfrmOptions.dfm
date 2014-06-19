object frmOptions: TfrmOptions
  Left = 435
  Top = 197
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 488
  ClientWidth = 513
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
  object lblSettingsLocation: TLabel
    Left = 132
    Top = 396
    Width = 112
    Height = 13
    Caption = '&Save above settings to:'
    FocusControl = cbSettingsLocation
  end
  object imgNoSaveWarning: TImage
    Left = 412
    Top = 394
    Width = 16
    Height = 16
    Picture.Data = {
      055449636F6E0000010001001010100000000000280100001600000028000000
      10000000200000000100040000000000C0000000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000000003BBBBBBBBBBBBBB003BBBBB00BBBBB0003BBBBB0
      0BBBBB00003BBBBBBBBBB000003BBBBBBBBBB0000003BBB00BBB00000003BBB0
      0BBB000000003BB00BB0000000003BB00BB00000000003B00B000000000003BB
      BB0000000000003BB00000000000003BB0000000000000030000000000000003
      0000000000000000000000008001000080010000C0030000C0030000E0070000
      E0070000F00F0000F00F0000F81F0000F81F0000FC3F0000FC3F0000FE7F0000
      FE7F0000}
  end
  object pbOK: TButton
    Left = 175
    Top = 452
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 263
    Top = 452
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = pbCancelClick
  end
  object cbSettingsLocation: TComboBox
    Left = 256
    Top = 392
    Width = 150
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ControlChanged
  end
  object ckAssociateFiles: TSDUCheckBox
    Left = 12
    Top = 424
    Width = 209
    Caption = 'CAPTION SET AUTOMATICALLY'
    TabOrder = 2
    AutoSize = True
  end
  object pcOptions: TPageControl
    Left = 5
    Top = 5
    Width = 500
    Height = 376
    ActivePage = tsPKCS11
    TabOrder = 0
    object tsPKCS11: TTabSheet
      Caption = 'PKCS#11'
      ImageIndex = 4
      inline fmeOptions_PKCS11: TfmeOptions_PKCS11
        Left = 0
        Top = 0
        Width = 492
        Height = 348
        TabOrder = 0
        inherited gbPKCS11: TGroupBox
          inherited lblLibrary: TLabel
            Width = 80
          end
        end
      end
    end
  end
end
