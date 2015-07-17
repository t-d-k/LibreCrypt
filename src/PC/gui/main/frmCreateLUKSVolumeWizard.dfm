inherited frmCreateLUKSVolumeWizard: TfrmCreateLUKSVolumeWizard
  Left = 440
  Top = 266
  Caption = 'Change Keyphrase/Other Details'
  ClientHeight = 437
  ClientWidth = 610
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 616
  ExplicitHeight = 465
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    Width = 610
    Height = 437
    ExplicitWidth = 610
    ExplicitHeight = 437
    inherited bvlLine: TBevel
      Top = 373
      Width = 608
      ExplicitTop = 373
      ExplicitWidth = 595
    end
    inherited pcWizard: TPageControl
      Width = 608
      Height = 372
      ActivePage = tsVolFile
      ExplicitWidth = 608
      ExplicitHeight = 373
      object tsVolFile: TTabSheet
        Caption = 'Container file'
        ImageIndex = 6
        ExplicitHeight = 345
        DesignSize = (
          600
          344)
        object GroupBox1: TGroupBox
          Left = 0
          Top = 277
          Width = 600
          Height = 67
          Align = alBottom
          Caption = 'Container filename'
          TabOrder = 0
          ExplicitTop = 278
          object feVolFilename: TSDUFilenameEdit
            Left = 176
            Top = 24
            Width = 307
            Height = 21
            Constraints.MaxHeight = 21
            Constraints.MinHeight = 21
            TabOrder = 0
            TabStop = False
            FilterIndex = 0
            DesignSize = (
              307
              21)
          end
        end
        object rgFileOrPartition: TRadioGroup
          Left = 4
          Top = 178
          Width = 580
          Height = 57
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'File or Partition'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 1
          ExplicitTop = 179
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 8
        ExplicitHeight = 345
        DesignSize = (
          600
          344)
        object Label21: TLabel
          Left = 212
          Top = 31
          Width = 97
          Height = 13
          Caption = '&Select disk/partition:'
        end
        object reInstructPartitionSelect: TLabel
          Left = 8
          Top = 12
          Width = 256
          Height = 13
          Caption = 'Please specify the partition your container is stored on.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        inline fmeSelectPartition: TfmeSelectPartition
          Left = 8
          Top = 50
          Width = 585
          Height = 279
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          ExplicitLeft = 8
          ExplicitTop = 50
          ExplicitWidth = 585
          ExplicitHeight = 280
          inherited lblErrorWarning: TLabel
            Left = 176
            Top = 263
            ExplicitLeft = 176
            ExplicitTop = 198
          end
          inherited imgErrorWarning: TImage
            Left = 146
            Top = 260
            ExplicitLeft = 146
            ExplicitTop = 260
          end
          inherited TabControl1: TTabControl
            Width = 585
            Height = 234
            ExplicitWidth = 585
            ExplicitHeight = 235
            inherited SDUDiskPartitionsPanel1: TfmeDiskPartitionsPanel
              Height = 206
              ExplicitHeight = 207
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 396
              Height = 206
              ExplicitLeft = 396
              ExplicitHeight = 207
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 352
            Top = 240
            ExplicitLeft = 352
            ExplicitTop = 241
          end
          inherited ckEntireDisk: TCheckBox
            Left = 496
            Top = 240
            ExplicitLeft = 496
            ExplicitTop = 241
          end
          inherited ilErrorWarning: TImageList
            Bitmap = {
              494C010103000400EC0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
              0000000000003600000028000000400000001000000001002000000000000010
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000080800000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000080800000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF00000000000000000000000000000000000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF0000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000080800000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF000000000000000000000000000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000000000000000000080800000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF00000000000000000000000000000000000000FF000000FF000000
              FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000000000000000000080800000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF000000000000000000000000000000FF000000FF000000FF000000
              FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000080
              800000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF00000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000080
              800000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF00000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000080800000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000080800000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000080800000FFFF00000000000000000000FFFF00000000000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000080800000FFFF0000FFFF0000FFFF0000FFFF00000000000000
              000000000000000000000000000000000000000000000000FF000000FF000000
              FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000080800000FFFF0000FFFF0000000000000000000000
              000000000000000000000000000000000000000000000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000080800000FFFF0000FFFF0000000000000000000000
              00000000000000000000000000000000000000000000000000000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF0000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000008080000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000008080000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000424D3E000000000000003E000000
              2800000040000000100000000100010000000000800000000000000000000000
              000000000000000000000000FFFFFF00FFFF0000F81F0000FFFF0000E0070000
              FFFF8001C0030000FFFF800180010000FFFFC00380010000FFFFC00300000000
              FFFFE00700000000FFFFE00700000000FFFFF00F00000000FFFFF00F00000000
              FFFFF81F00000000FFFFF81F80010000FFFFFC3F80010000FFFFFC3FC0030000
              FFFFFE7FE0070000FFFFFE7FF81F000000000000000000000000000000000000
              000000000000}
          end
        end
      end
      object tsEncDetails: TTabSheet
        Caption = 'Encryption details'
        ImageIndex = 3
        ExplicitHeight = 345
        object Label5: TLabel
          Left = 8
          Top = 271
          Width = 76
          Height = 13
          Caption = 'New &salt length:'
          FocusControl = seDestSaltLength
        end
        object Label7: TLabel
          Left = 264
          Top = 271
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object Label12: TLabel
          Left = 8
          Top = 324
          Width = 107
          Height = 13
          Caption = '&Requested drive letter:'
          FocusControl = cbDestDriveLetter
        end
        object Label8: TLabel
          Left = 8
          Top = 297
          Width = 66
          Height = 13
          Caption = '&Key iterations:'
          FocusControl = seDestKeyIterations
        end
        object lblInstructDestDetails: TLabel
          Left = 3
          Top = 3
          Width = 238
          Height = 13
          Caption = 'Please enter new details for your container/keyfile.'
        end
        object seDestSaltLength: TSpinEdit64
          Left = 128
          Top = 266
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 0
          OnChange = seSaltLengthChange
        end
        object cbDestDriveLetter: TComboBox
          Left = 128
          Top = 320
          Width = 89
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
        object seDestKeyIterations: TSpinEdit64
          Left = 128
          Top = 292
          Width = 121
          Height = 22
          Increment = 1
          MinValue = 1
          TabOrder = 1
        end
        inline frmeNewPassword: TfrmeNewPassword
          Left = 8
          Top = 22
          Width = 567
          Height = 243
          Constraints.MinHeight = 178
          Constraints.MinWidth = 223
          TabOrder = 3
          ExplicitLeft = 8
          ExplicitTop = 22
          ExplicitWidth = 567
          ExplicitHeight = 243
          inherited lblInstructPassword: TLabel
            Width = 567
            ExplicitLeft = 0
            ExplicitWidth = 547
          end
          inherited lblStrength: TLabel
            Top = 212
            ExplicitTop = 212
          end
          inherited ProgressBar1: TProgressBar
            Top = 211
            Width = 431
            ExplicitTop = 211
            ExplicitWidth = 431
          end
          inherited preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit
            Top = 84
            Width = 431
            Height = 58
            ExplicitTop = 84
            ExplicitWidth = 431
            ExplicitHeight = 58
          end
          inherited preUserKeyConfirm: TOTFEFreeOTFE_PasswordRichEdit
            Top = 148
            Width = 431
            Height = 58
            Anchors = [akLeft, akTop, akRight]
            ExplicitTop = 148
            ExplicitWidth = 431
            ExplicitHeight = 58
          end
        end
      end
      object tsRNGSelect: TTabSheet
        Caption = 'RNG Select'
        ImageIndex = 6
        ExplicitHeight = 345
        object reInstructRNGSelect1: TLabel
          Left = 3
          Top = 12
          Width = 568
          Height = 26
          Caption = 
            'In order to change your container/keyfile'#39's details, a certain a' +
            'mount of random data is required. This data will be used for the' +
            ' following:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect2: TLabel
          Left = 3
          Top = 44
          Width = 91
          Height = 13
          Caption = '1) Password salting'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect3: TLabel
          Left = 3
          Top = 58
          Width = 117
          Height = 13
          Caption = '2) Random padding data'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect4: TLabel
          Left = 3
          Top = 77
          Width = 552
          Height = 13
          Caption = 
            'In order to generate this data, please select which random numbe' +
            'r generators you wish to use from the options below.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object gbRNG: TGroupBox
          Left = 3
          Top = 108
          Width = 214
          Height = 125
          Caption = 'Random number generator (RNG)'
          TabOrder = 1
          object ckRNGMouseMovement: TCheckBox
            Left = 16
            Top = 48
            Width = 141
            Height = 17
            Caption = 'Mouse movement'
            TabOrder = 1
            OnClick = ckRNGClick
          end
          object ckRNGCryptoAPI: TCheckBox
            Left = 16
            Top = 24
            Width = 125
            Height = 17
            Caption = 'Microsoft CryptoAPI'
            TabOrder = 0
            OnClick = ckRNGClick
          end
          object ckRNGcryptlib: TCheckBox
            Left = 16
            Top = 72
            Width = 97
            Height = 17
            Caption = 'cryptlib'
            TabOrder = 2
            OnClick = ckRNGClick
          end
          object ckRNGPKCS11: TCheckBox
            Left = 16
            Top = 96
            Width = 97
            Height = 17
            Caption = 'PKCS#11 token'
            TabOrder = 3
            OnClick = ckRNGClick
          end
        end
        object ckRNGGPG: TCheckBox
          Left = 282
          Top = 132
          Width = 97
          Height = 17
          Caption = 'GPG - INVISIBLE CHECKBOX'
          TabOrder = 0
          Visible = False
          OnClick = ckRNGClick
        end
      end
      object tsRNGPKCS11: TTabSheet
        Caption = 'RNG PKCS#11 token'
        ImageIndex = 9
        ExplicitHeight = 345
        object lblToken: TLabel
          Left = 11
          Top = 51
          Width = 34
          Height = 13
          Caption = '&Token:'
          FocusControl = cbToken
        end
        object reInstructRNGPKCS11: TLabel
          Left = 8
          Top = 12
          Width = 480
          Height = 13
          Caption = 
            'Please select the PKCS#11 token you wish to use to generate rand' +
            'om data, from the list shown below'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object cbToken: TComboBox
          Left = 62
          Top = 47
          Width = 145
          Height = 21
          TabOrder = 0
          Text = 'cbToken'
        end
        object pbRefresh: TButton
          Left = 213
          Top = 45
          Width = 75
          Height = 25
          Caption = '&Refresh'
          TabOrder = 1
          OnClick = pbRefreshClick
        end
      end
    end
    inherited pnlButtons: TPanel
      Top = 394
      Width = 608
      ExplicitTop = 395
      ExplicitWidth = 608
      inherited lblCompleteIndicator: TLabel
        Width = 95
        ExplicitWidth = 95
      end
      inherited pbNext: TButton
        Left = 354
        ExplicitLeft = 354
      end
      inherited pbBack: TButton
        Left = 270
        ExplicitLeft = 270
      end
      inherited pbFinish: TButton
        Left = 438
        ExplicitLeft = 438
      end
      inherited pbCancel: TButton
        Left = 522
        ExplicitLeft = 522
      end
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 412
    Top = 72
  end
  object SaveDialog: TSDUSaveDialog
    PreserveCWD = False
    Left = 336
    Top = 72
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 368
    Top = 76
  end
end
