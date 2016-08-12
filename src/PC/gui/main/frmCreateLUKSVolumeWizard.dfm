inherited frmCreateLUKSVolumeWizard: TfrmCreateLUKSVolumeWizard
  Left = 440
  Top = 266
  Caption = 'Create LUKS Container'
  ClientHeight = 437
  ClientWidth = 610
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnClose = FormClose
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
      Top = 374
      Width = 608
      ExplicitTop = 373
      ExplicitWidth = 595
    end
    inherited pcWizard: TPageControl
      Width = 608
      Height = 373
      ActivePage = tsVolFile
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 608
      ExplicitHeight = 373
      object tsVolFile: TTabSheet
        Caption = 'Container file'
        ImageIndex = 6
        DesignSize = (
          600
          345)
        object lblFileInstruct: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 594
          Height = 39
          Align = alTop
          Caption = 
            'LUKS container creation is a beta feature.'#13#10'Some values that wil' +
            'l be configurable later are constant in this version to ease tro' +
            'uble shooting.'#13#10'The values that will be used for the container a' +
            're:'
          ExplicitWidth = 446
        end
        object lblInstruct2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 48
          Width = 594
          Height = 104
          Align = alTop
          Caption = 
            'Cypher:                        aes 256 '#13#10'Hash:                  ' +
            '        SHA256'#13#10'Number of AF Stripes:          2'#13#10'Cypher Mode:  ' +
            '                 xts-plain64'#13#10'Master Key Digest Iterations:  100' +
            '0'#13#10'Sector IV Gen Method:          32 bit sector ID'#13#10'Base IV Cyph' +
            'er on Hash Length: True'#13#10'Key Slot:                      0 '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 282
        end
        object lblInstruct3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 158
          Width = 594
          Height = 13
          Align = alTop
          Caption = 'The container is NOT wiped with chaff before use'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 235
        end
        object lblFileExistsWarning: TLabel
          Left = 3
          Top = 259
          Width = 204
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'This file cannot be used as it already exists.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 278
          Width = 600
          Height = 67
          Align = alBottom
          Caption = 'Container filename'
          TabOrder = 0
          inline feVolFilename: TSDUFilenameEdit
            Left = 3
            Top = 16
            Width = 594
            Height = 48
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 16
            ExplicitWidth = 594
            ExplicitHeight = 48
            DesignSize = (
              594
              48)
            inherited edFilename: TEdit
              Top = 11
              Width = 498
              OnChange = feVolFilenameedFilenameChange
              ExplicitTop = 11
              ExplicitWidth = 498
            end
            inherited pbBrowse: TButton
              Left = 515
              Top = 11
              ExplicitLeft = 515
              ExplicitTop = 11
            end
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
            'File'
            'Partition or Whole Disk')
          TabOrder = 1
          OnClick = ControlChanged
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 8
        DesignSize = (
          600
          345)
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
          ExplicitHeight = 279
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
            ExplicitHeight = 234
            inherited SDUDiskPartitionsPanel1: TfmeSDUDiskPartitions
              Height = 206
              ExplicitHeight = 206
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 396
              Height = 206
              ExplicitLeft = 396
              ExplicitHeight = 206
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 352
            Top = 240
            ExplicitLeft = 352
            ExplicitTop = 240
          end
          inherited ckEntireDisk: TCheckBox
            Left = 496
            Top = 240
            ExplicitLeft = 496
            ExplicitTop = 240
          end
          inherited ilErrorWarning: TImageList
            Bitmap = {
              494C010103000400640110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      object tsSize: TTabSheet
        Caption = 'Size'
        ImageIndex = 5
        inline fmeContainerSize1: TTfmeContainerSize
          Left = 0
          Top = 0
          Width = 600
          Height = 345
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 600
          ExplicitHeight = 345
          inherited lblPartitionDiskSize: TLabel
            Left = 137
            Top = 260
            Width = 52
            ExplicitLeft = 137
            ExplicitTop = 260
            ExplicitWidth = 52
          end
          inherited Label6: TLabel
            Left = 3
            Top = 260
            Width = 105
            ExplicitLeft = 3
            ExplicitTop = 260
            ExplicitWidth = 105
          end
          inherited lblInstructSizeCommon: TLabel
            Width = 600
            ExplicitWidth = 584
          end
          inherited lblInstructSizeHidden: TLabel
            Width = 600
          end
          inherited lblInstructSizeNotHidden: TLabel
            Width = 600
          end
          inherited ckSizeEntirePartitionDisk: TCheckBox
            Left = 3
            Top = 229
            ExplicitLeft = 3
            ExplicitTop = 229
          end
          inherited se64UnitSize: TSDUSpin64Unit_Storage
            Top = 295
            MinValue = 1
            ExplicitTop = 295
          end
        end
      end
      object tsEncDetails: TTabSheet
        Caption = 'Encryption details'
        ImageIndex = 3
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
      Top = 395
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
end
