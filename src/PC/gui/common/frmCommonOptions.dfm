object frmCommonOptions: TfrmCommonOptions
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
  OnCreate = FormCreate
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
    TabOrder = 1
    OnChange = ControlChanged
  end
  object ckAssociateFiles: TSDUCheckBox
    Left = 12
    Top = 424
    Width = 158
    Height = 13
    Caption = 'Associate %s with ".vol" &files'
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
      object gbPKCS11: TGroupBox
        Left = 16
        Top = 16
        Width = 457
        Height = 241
        Caption = 'Security Token/Smartcard support'
        TabOrder = 0
        object lblLibrary: TLabel
          Left = 36
          Top = 52
          Width = 80
          Height = 13
          Caption = 'PKCS#11 &library:'
          FocusControl = feLibFilename
        end
        object ckEnablePKCS11: TCheckBox
          Left = 16
          Top = 24
          Width = 421
          Height = 17
          Caption = '&Enable PKCS#11 support'
          TabOrder = 0
          OnClick = ControlChanged
        end
        object pbVerify: TButton
          Left = 232
          Top = 76
          Width = 75
          Height = 25
          Caption = '&Verify'
          TabOrder = 2
          OnClick = pbVerifyClick
        end
        object gbPKCS11AutoActions: TGroupBox
          Left = 16
          Top = 120
          Width = 425
          Height = 105
          Caption = 'Automount/dismount'
          TabOrder = 3
          object lblAutoMountVolume: TLabel
            Left = 32
            Top = 48
            Width = 38
            Height = 13
            Caption = 'V&olume:'
          end
          object ckPKCS11AutoDismount: TCheckBox
            Left = 12
            Top = 72
            Width = 397
            Height = 17
            Caption = 'Auto &dismount PKCS#11 volumes when associated token is removed'
            TabOrder = 1
            OnClick = ControlChanged
          end
          object ckPKCS11AutoMount: TCheckBox
            Left = 12
            Top = 20
            Width = 397
            Height = 17
            Caption = 'Auto &mount specified container on token insertion'
            TabOrder = 0
            OnClick = ControlChanged
          end
          inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
            Left = 104
            Top = 44
            Width = 310
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            ExplicitLeft = 104
            ExplicitTop = 44
            ExplicitWidth = 310
            ExplicitHeight = 21
            DesignSize = (
              310
              21)
            inherited bbBrowsePartition: TBitBtn
              Left = 306
              ExplicitLeft = 306
            end
            inherited bbBrowseFile: TBitBtn
              Left = 282
              ExplicitLeft = 282
            end
            inherited edFilename: TEdit
              Width = 279
              ExplicitWidth = 279
            end
          end
        end
        object feLibFilename: TSDUFilenameEdit
          Left = 148
          Top = 48
          Width = 293
          Height = 21
          Constraints.MaxHeight = 21
          Constraints.MinHeight = 21
          TabOrder = 1
          TabStop = False
          FilterIndex = 0
          OnChange = ControlChanged
          DesignSize = (
            293
            21)
        end
        object pbAutoDetect: TButton
          Left = 148
          Top = 76
          Width = 75
          Height = 25
          Caption = '&Autodetect'
          TabOrder = 4
          OnClick = pbAutoDetectClick
        end
      end
    end
  end
end
