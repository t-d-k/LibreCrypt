inherited frmWizardChangePasswordCreateKeyfile: TfrmWizardChangePasswordCreateKeyfile
  Left = 440
  Top = 266
  Caption = 'THIS CAPTION WILL BE SET AUTOMATICALLY'
  ClientHeight = 436
  ClientWidth = 597
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 603
  ExplicitHeight = 464
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    Width = 597
    Height = 436
    ExplicitWidth = 597
    ExplicitHeight = 436
    inherited bvlLine: TBevel
      Top = 373
      Width = 595
      ExplicitTop = 373
      ExplicitWidth = 595
    end
    inherited pcWizard: TPageControl
      Width = 595
      Height = 372
      ActivePage = tsRNGSelect
      ExplicitWidth = 595
      ExplicitHeight = 372
      object tsFileOrPartition: TTabSheet
        Caption = 'Volume File or Partition'
        ImageIndex = 7
        DesignSize = (
          587
          344)
        object reInstructFileOrPartition: TLabel
          Left = 3
          Top = 3
          Width = 581
          Height = 26
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'If you would like to modify a container, please specify whether ' +
            'it is file or partition based. If you would like to modify a key' +
            'file, please select "file" below.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object rgFileOrPartition: TRadioGroup
          Left = 3
          Top = 274
          Width = 567
          Height = 57
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'File or Partition'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 1
          OnClick = rgFileOrPartitionClick
        end
        inline TSDUDiskPartitionsPanel1: TSDUDiskPartitionsPanel
          Left = 3
          Top = 178
          Width = 577
          Height = 90
          Anchors = [akLeft, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          ExplicitLeft = 3
          ExplicitTop = 178
          ExplicitWidth = 577
          ExplicitHeight = 90
        end
      end
      object tsSrcFile: TTabSheet
        Caption = 'Source file'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructSrcFile: TLabel
          Left = 14
          Top = 12
          Width = 545
          Height = 39
          Caption = 
            'Please enter the full path and filename of the container/keyfile' +
            ' you wish to change.If you wish to update a "hidden" container h' +
            'eld within another container, please specify the filename of the' +
            ' outer container which stores your hidden container'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object GroupBox2: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'Source volume/keyfile'
          TabOrder = 0
          object lblSrcFilename: TSDUFilenameLabel
            Left = 8
            Top = 20
            Width = 377
            Height = 37
            AutoSize = False
            Caption = 'lblSrcFilename'
            WordWrap = True
          end
          object pbBrowseSrc: TButton
            Left = 356
            Top = 36
            Width = 75
            Height = 25
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseSrcClick
          end
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 8
        object Label21: TLabel
          Left = 196
          Top = 92
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
          Top = 120
          Width = 473
          Height = 144
          TabOrder = 0
          ExplicitLeft = 8
          ExplicitTop = 120
          ExplicitWidth = 473
          ExplicitHeight = 144
          inherited lblErrorWarning: TLabel
            Top = 125
            ExplicitTop = 125
          end
          inherited imgErrorWarning: TImage
            Top = 123
            ExplicitTop = 123
          end
          inherited TabControl1: TTabControl
            Width = 473
            Height = 97
            ExplicitWidth = 473
            ExplicitHeight = 97
            inherited SDUDiskPartitionsPanel1: TOTFEFreeOTFEDiskPartitionsPanel
              Height = 69
              ExplicitHeight = 69
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 284
              Height = 69
              ExplicitLeft = 284
              ExplicitHeight = 69
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 240
            Top = 105
            ExplicitLeft = 240
            ExplicitTop = 105
          end
          inherited ckEntireDisk: TCheckBox
            Left = 384
            Top = 105
            ExplicitLeft = 384
            ExplicitTop = 105
          end
          inherited ilErrorWarning: TImageList
            Bitmap = {
              494C010103000400B40010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      object tsSrcDetails: TTabSheet
        Caption = 'Src Details'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label9: TLabel
          Left = 8
          Top = 220
          Width = 31
          Height = 13
          Caption = '&Offset:'
        end
        object Label1: TLabel
          Left = 8
          Top = 92
          Width = 89
          Height = 13
          Caption = 'Current key&phrase:'
          FocusControl = preSrcUserKey
        end
        object Label4: TLabel
          Left = 8
          Top = 164
          Width = 88
          Height = 13
          Caption = 'Current &salt length:'
          FocusControl = seSrcSaltLength
        end
        object Label6: TLabel
          Left = 272
          Top = 164
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object Label11: TLabel
          Left = 8
          Top = 192
          Width = 102
          Height = 13
          Caption = 'Current &key iterations:'
          FocusControl = seSrcKeyIterations
        end
        object reInstructSrcDetails: TLabel
          Left = 8
          Top = 12
          Width = 564
          Height = 26
          Caption = 
            'Please enter the full details of the container/keyfile you wish ' +
            'to change. If you wish to update a "hidden" container held withi' +
            'n another container, please specify the offset within the outer ' +
            'container where your hidden container is located'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object preSrcUserKey: TOTFEFreeOTFE_PasswordRichEdit
          Left = 144
          Top = 88
          Width = 337
          Height = 65
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'preSrcUserKey')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
          OnChange = preSrcUserKeyChange
        end
        object seSrcSaltLength: TSpinEdit64
          Left = 144
          Top = 160
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 1
          OnChange = seSaltLengthChange
        end
        object seSrcKeyIterations: TSpinEdit64
          Left = 144
          Top = 188
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 3
        end
        object se64UnitOffset: TSDUSpin64Unit_Storage
          Left = 144
          Top = 216
          Width = 226
          Height = 29
          TabOrder = 2
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          OnChange = se64OffsetChange
        end
      end
      object tsDestFile: TTabSheet
        Caption = 'Destination file'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructDestFile: TLabel
          Left = 8
          Top = 12
          Width = 370
          Height = 13
          Caption = 
            'Please specify the full path and filename where the keyfile shou' +
            'ld be written to.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object GroupBox1: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'Keyfile'
          TabOrder = 0
          object lblDestFilename: TSDUFilenameLabel
            Left = 8
            Top = 20
            Width = 377
            Height = 37
            AutoSize = False
            Caption = 'lblDestFilename'
            WordWrap = True
          end
          object pbBrowseDest: TButton
            Left = 356
            Top = 36
            Width = 75
            Height = 25
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseDestClick
          end
        end
      end
      object tsDestDetails: TTabSheet
        Caption = 'Destination file details'
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
          inherited Label17: TLabel
            Top = 84
            ExplicitTop = 84
          end
          inherited Label3: TLabel
            Top = 148
            ExplicitTop = 148
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
        ExplicitLeft = 28
        ExplicitTop = 22
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructRNGSelect: TLabel
          Left = 8
          Top = 12
          Width = 568
          Height = 39
          Caption = 
            'In order to change your container/keyfile'#39's details, a certain a' +
            'mount of random data is required. This data will be used for the' +
            ' following: 1) Password salting 2) Random padding data In order ' +
            'to generate this data, please select which random number generat' +
            'ors you wish to use from the options below.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object gbRNG: TGroupBox
          Left = 139
          Top = 172
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
          Left = 16
          Top = 196
          Width = 97
          Height = 17
          Caption = 'GPG - INVISIBLE CHECKBOX'
          TabOrder = 0
          Visible = False
          OnClick = ckRNGClick
        end
      end
      object tsRNGMouseMovement: TTabSheet
        Caption = 'RNG Mouse movement'
        ImageIndex = 4
        DesignSize = (
          587
          344)
        object lblMouseRNGBits: TLabel
          Left = 160
          Top = 325
          Width = 169
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Random bits generated: 9999/9999'
          ExplicitTop = 299
        end
        object reInstructRNGMouseMovement: TLabel
          Left = 8
          Top = 12
          Width = 558
          Height = 26
          Caption = 
            'You have selected mouse movement to generate random data. Please' +
            ' "wiggle" the mouse within the area below, until enough random d' +
            'ata has been generated.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object MouseRNG: TMouseRNG
          Left = 8
          Top = 68
          Width = 462
          Height = 248
          TrailLines = 5
          LineWidth = 5
          LineColor = clNavy
          Anchors = [akLeft, akTop, akRight, akBottom]
          UseDockManager = False
          Enabled = False
          ParentColor = False
          OnByteGenerated = MouseRNGByteGenerated
        end
      end
      object tsRNGPKCS11: TTabSheet
        Caption = 'RNG PKCS#11 token'
        ImageIndex = 9
        object lblToken: TLabel
          Left = 123
          Top = 175
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
          Left = 163
          Top = 170
          Width = 145
          Height = 21
          TabOrder = 0
          Text = 'cbToken'
        end
        object pbRefresh: TButton
          Left = 314
          Top = 165
          Width = 75
          Height = 25
          Caption = '&Refresh'
          TabOrder = 1
          OnClick = pbRefreshClick
        end
      end
      object tsRNGGPG: TTabSheet
        Caption = 'RNG GPG'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructRNGGPG: TLabel
          Left = 8
          Top = 12
          Width = 551
          Height = 13
          Caption = 
            'In order to use GPG to generate random data, please specify the ' +
            'location of "gpg.exe" by clicking the browse button.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object GroupBox5: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'GPG Executable'
          TabOrder = 0
          object lblGPGFilename: TSDUFilenameLabel
            Left = 8
            Top = 20
            Width = 377
            Height = 37
            AutoSize = False
            Caption = 'lblGPGFilename'
            WordWrap = True
          end
          object pbBrowseGPG: TButton
            Left = 356
            Top = 36
            Width = 75
            Height = 25
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseGPGClick
          end
        end
      end
    end
    inherited pnlButtons: TPanel
      Top = 394
      Width = 595
      ExplicitTop = 394
      ExplicitWidth = 595
      inherited lblCompleteIndicator: TLabel
        Width = 95
        ExplicitWidth = 95
      end
      inherited pbNext: TButton
        Left = 341
        ExplicitLeft = 341
      end
      inherited pbBack: TButton
        Left = 257
        ExplicitLeft = 257
      end
      inherited pbFinish: TButton
        Left = 425
        ExplicitLeft = 425
      end
      inherited pbCancel: TButton
        Left = 509
        ExplicitLeft = 509
      end
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 380
    Top = 304
  end
  object SaveDialog: TSDUSaveDialog
    PreserveCWD = False
    Left = 416
    Top = 288
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 320
    Top = 308
  end
end
