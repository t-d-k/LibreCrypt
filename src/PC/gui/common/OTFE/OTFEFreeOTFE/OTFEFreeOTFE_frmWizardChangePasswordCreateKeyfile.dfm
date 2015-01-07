inherited frmWizardChangePasswordCreateKeyfile: TfrmWizardChangePasswordCreateKeyfile
  Left = 440
  Top = 266
  Caption = 'THIS CAPTION WILL BE SET AUTOMATICALLY'
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 613
  ExplicitHeight = 438
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsRNGGPG
      object tsFileOrPartition: TTabSheet
        Caption = 'Volume File or Partition'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          597
          318)
        object rgFileOrPartition: TRadioGroup
          Left = 3
          Top = 248
          Width = 577
          Height = 57
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'File or Partition'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 1
          OnClick = rgFileOrPartitionClick
        end
        object reInstructFileOrPartition: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 582
          Height = 77
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        inline TSDUDiskPartitionsPanel1: TSDUDiskPartitionsPanel
          Left = 32
          Top = 199
          Width = 320
          Height = 90
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          ExplicitLeft = 32
          ExplicitTop = 199
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
        object reInstructSrcFile: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 125
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object GroupBox2: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'Source volume/keyfile'
          TabOrder = 1
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
        object reInstructPartitionSelect: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        inline fmeSelectPartition: TfmeSelectPartition
          Left = 8
          Top = 120
          Width = 473
          Height = 144
          TabOrder = 1
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
              494C010103000400580010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
          Width = 85
          Height = 13
          Caption = 'Current &password:'
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
        object reInstructSrcDetails: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
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
          TabOrder = 1
          OnChange = preUserKeyChange
        end
        object seSrcSaltLength: TSpinEdit64
          Left = 144
          Top = 160
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 2
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
          TabOrder = 4
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
        object reInstructDestFile: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object GroupBox1: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'Keyfile'
          TabOrder = 1
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
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label2: TLabel
          Left = 8
          Top = 92
          Width = 73
          Height = 13
          Caption = 'New &password:'
          FocusControl = preDestUserKey1
        end
        object Label3: TLabel
          Left = 8
          Top = 164
          Width = 109
          Height = 13
          Caption = 'Confirm new p&assword:'
          FocusControl = preDestUserKey2
        end
        object Label5: TLabel
          Left = 8
          Top = 236
          Width = 76
          Height = 13
          Caption = 'New &salt length:'
          FocusControl = seDestSaltLength
        end
        object Label7: TLabel
          Left = 272
          Top = 236
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object Label12: TLabel
          Left = 8
          Top = 292
          Width = 107
          Height = 13
          Caption = '&Requested drive letter:'
          FocusControl = cbDestDriveLetter
        end
        object Label8: TLabel
          Left = 8
          Top = 264
          Width = 66
          Height = 13
          Caption = '&Key iterations:'
          FocusControl = seDestKeyIterations
        end
        object reInstructDestDetails: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object preDestUserKey1: TOTFEFreeOTFE_PasswordRichEdit
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
            'preDestUserKey1')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 1
          OnChange = preUserKeyChange
        end
        object preDestUserKey2: TOTFEFreeOTFE_PasswordRichEdit
          Left = 144
          Top = 160
          Width = 337
          Height = 65
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'preDestUserKey2')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
          OnChange = preUserKeyChange
        end
        object seDestSaltLength: TSpinEdit64
          Left = 144
          Top = 232
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 3
          OnChange = seSaltLengthChange
        end
        object cbDestDriveLetter: TComboBox
          Left = 144
          Top = 288
          Width = 89
          Height = 21
          Style = csDropDownList
          TabOrder = 5
        end
        object seDestKeyIterations: TSpinEdit64
          Left = 144
          Top = 260
          Width = 121
          Height = 22
          Increment = 1
          MinValue = 1
          TabOrder = 4
        end
      end
      object tsRNGSelect: TTabSheet
        Caption = 'RNG Select'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructRNGSelect: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 125
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
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
          TabOrder = 2
          Visible = False
          OnClick = ckRNGClick
        end
      end
      object tsRNGMouseMovement: TTabSheet
        Caption = 'RNG Mouse movement'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          597
          318)
        object lblMouseRNGBits: TLabel
          Left = 160
          Top = 299
          Width = 169
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Random bits generated: 9999/9999'
        end
        object reInstructRNGMouseMovement: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 49
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object MouseRNG: TMouseRNG
          Left = 8
          Top = 68
          Width = 472
          Height = 222
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
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblToken: TLabel
          Left = 123
          Top = 175
          Width = 34
          Height = 13
          Caption = '&Token:'
          FocusControl = cbToken
        end
        object reInstructRNGPKCS11: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object cbToken: TComboBox
          Left = 163
          Top = 170
          Width = 145
          Height = 21
          TabOrder = 2
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
        object reInstructRNGGPG: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
        end
        object GroupBox5: TGroupBox
          Left = 22
          Top = 208
          Width = 445
          Height = 73
          Caption = 'GPG Executable'
          TabOrder = 1
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
      inherited lblCompleteIndicator: TLabel
        Width = 95
        ExplicitWidth = 95
      end
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 388
    Top = 224
  end
  object SaveDialog: TSDUSaveDialog
    PreserveCWD = False
    Left = 416
    Top = 224
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 392
    Top = 316
  end
end
