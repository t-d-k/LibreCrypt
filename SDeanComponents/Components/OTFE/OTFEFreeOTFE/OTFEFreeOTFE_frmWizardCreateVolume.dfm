inherited frmWizardCreateVolume: TfrmWizardCreateVolume
  Left = 388
  Top = 258
  Caption = 'FreeOTFE Volume Creation Wizard'
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsSummary
      object tsWelcome: TTabSheet
        Caption = 'Welcome'
        object lblWelcomeBanner: TLabel
          Left = 36
          Top = 52
          Width = 417
          Height = 20
          Caption = 'Welcome to the FreeOTFE Volume Creation Wizard.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblWelcomeClickNext: TLabel
          Left = 164
          Top = 216
          Width = 160
          Height = 13
          Caption = 'Please click "Next >" to continue.'
        end
        object reInstructWelcome: TOTFEFreeOTFE_InstructionRichEdit
          Left = 77
          Top = 96
          Width = 333
          Height = 113
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
      end
      object tsFileOrPartition: TTabSheet
        Caption = 'Volume File or Partition'
        ImageIndex = 14
        object reInstructFileOrPartition: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object rgFileOrPartition: TRadioGroup
          Left = 162
          Top = 136
          Width = 168
          Height = 57
          Caption = 'File or Partition/Entire Disk'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 1
          OnClick = rgFileOrPartitionClick
        end
      end
      object tsFilename: TTabSheet
        Caption = 'Volume filename'
        ImageIndex = 1
        object reInstructFilename: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object GroupBox1: TGroupBox
          Left = 20
          Top = 224
          Width = 445
          Height = 73
          Caption = 'Volume filename'
          TabOrder = 1
          object lblFilename: TSDUFilenameLabel
            Left = 8
            Top = 20
            Width = 377
            Height = 37
            AutoSize = False
            Caption = 'lblFilename'
            WordWrap = True
          end
          object pbBrowseFilename: TButton
            Left = 356
            Top = 36
            Width = 75
            Height = 25
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseFilenameClick
          end
        end
      end
      object tsPartitionWarning: TTabSheet
        Caption = 'Partition Warning'
        ImageIndex = 16
        object pnlWarningBorder_L: TPanel
          Left = 61
          Top = 236
          Width = 64
          Height = 37
          Caption = 'pnlWarningBorder_L'
          Color = clRed
          ParentBackground = False
          TabOrder = 1
        end
        object pnlWarningPartition: TPanel
          Left = 54
          Top = 40
          Width = 380
          Height = 189
          Caption = 'pnlWarningPartition'
          TabOrder = 0
          object lblWarningPartition: TLabel
            Left = 159
            Top = 16
            Width = 61
            Height = 13
            Alignment = taCenter
            Caption = 'WARNING'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold, fsUnderline]
            ParentColor = False
            ParentFont = False
          end
          object Label28: TLabel
            Left = 12
            Top = 136
            Width = 352
            Height = 26
            Alignment = taCenter
            Caption = 
              'DO NOT PROCEED UNLESS YOU UNDERSTAND THE FULL IMPLICATIONS OF CR' +
              'EATING ENCRYPTED PARTITIONS.'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
            WordWrap = True
          end
          object reInstructPartitionWarning: TOTFEFreeOTFE_InstructionRichEdit
            Left = 15
            Top = 36
            Width = 346
            Height = 97
            Lines.Strings = (
              'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
            TabOrder = 0
          end
        end
        object pnlWarningBorder_R: TPanel
          Left = 165
          Top = 240
          Width = 64
          Height = 37
          Caption = 'pnlWarningBorder_L'
          Color = clRed
          ParentBackground = False
          TabOrder = 2
        end
        object pnlWarningBorder_T: TPanel
          Left = 269
          Top = 240
          Width = 64
          Height = 37
          Caption = 'pnlWarningBorder_L'
          Color = clRed
          ParentBackground = False
          TabOrder = 3
        end
        object pnlWarningBorder_B: TPanel
          Left = 345
          Top = 240
          Width = 64
          Height = 37
          Caption = 'pnlWarningBorder_L'
          Color = clRed
          ParentBackground = False
          TabOrder = 4
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 15
        object Label21: TLabel
          Left = 138
          Top = 92
          Width = 213
          Height = 13
          Caption = '&Disk/partition to create encrypted volume on:'
        end
        object reInstructPartitionSelect: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object ckPartitionHidden: TCheckBox
          Left = 124
          Top = 272
          Width = 241
          Height = 17
          Caption = '&Create hidden volume within selected partition'
          TabOrder = 2
          OnClick = ckPartitionHiddenClick
        end
        inline fmeSelectPartition: TfmeSelectPartition
          Left = 8
          Top = 120
          Width = 473
          Height = 144
          TabOrder = 1
          inherited lblErrorWarning: TLabel
            Top = 125
          end
          inherited imgErrorWarning: TImage
            Top = 123
          end
          inherited TabControl1: TTabControl
            Width = 473
            Height = 97
            inherited SDUDiskPartitionsPanel1: TOTFEFreeOTFEDiskPartitionsPanel
              Width = 280
              Height = 69
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 284
              Height = 69
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 240
            Top = 105
          end
          inherited ckEntireDisk: TCheckBox
            Left = 384
            Top = 105
          end
        end
      end
      object tsOffset: TTabSheet
        Caption = 'Offset'
        ImageIndex = 7
        object Label7: TLabel
          Left = 41
          Top = 288
          Width = 120
          Height = 13
          Caption = '&Byte offset within volume:'
        end
        object pnlWarningOffset: TPanel
          Left = 5
          Top = 92
          Width = 477
          Height = 177
          Caption = 'pnlWarningOffset'
          TabOrder = 1
          object lblWarningOffset: TLabel
            Left = 208
            Top = 8
            Width = 61
            Height = 13
            Caption = 'WARNING'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
          end
          object Label13: TLabel
            Left = 12
            Top = 140
            Width = 443
            Height = 26
            Alignment = taCenter
            Caption = 
              'DO NOT PROCEED UNLESS YOU UNDERSTAND THE FULL IMPLICATIONS OF CR' +
              'EATING HIDDEN VOLUMES.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object reInstructWarningOffset: TOTFEFreeOTFE_InstructionRichEdit
            Left = 15
            Top = 28
            Width = 446
            Height = 105
            Lines.Strings = (
              'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
            TabOrder = 0
          end
        end
        object reInstructOffset: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object se64UnitByteOffset: TSDUSpin64Unit_Storage
          Left = 226
          Top = 284
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
          OnChange = se64ByteOffsetChange
        end
      end
      object tsSize: TTabSheet
        Caption = 'Size'
        ImageIndex = 6
        object Label6: TLabel
          Left = 63
          Top = 188
          Width = 95
          Height = 13
          Caption = '&Size of new volume:'
        end
        object lblPartitionDiskSize: TLabel
          Left = 219
          Top = 232
          Width = 89
          Height = 13
          Caption = 'lblPartitionDiskSize'
        end
        object reInstructSize: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 157
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object ckSizeEntirePartitionDisk: TCheckBox
          Left = 199
          Top = 212
          Width = 141
          Height = 17
          Caption = '&Use entire partition/disk'
          TabOrder = 2
          OnClick = ckSizeEntirePartitionDiskClick
        end
        object se64UnitSize: TSDUSpin64Unit_Storage
          Left = 199
          Top = 184
          Width = 226
          Height = 29
          TabOrder = 1
          OnExit = seSizeExit
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          OnChange = SizeChanged
        end
      end
      object tsHashCypherIV: TTabSheet
        Caption = 'Hash, cypher and IV'
        ImageIndex = 2
        object Label4: TLabel
          Left = 57
          Top = 128
          Width = 59
          Height = 13
          Caption = 'Select &hash:'
          FocusControl = cbHash
        end
        object Label5: TLabel
          Left = 57
          Top = 156
          Width = 68
          Height = 13
          Caption = 'Select &cypher:'
          FocusControl = cbCypher
        end
        object lblSectorIVGenMethod: TLabel
          Left = 57
          Top = 184
          Width = 52
          Height = 13
          Caption = 'Sector &IVs:'
          FocusControl = cbSectorIVGenMethod
        end
        object cbHash: TComboBox
          Left = 193
          Top = 124
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
          OnChange = cbHashCypherIVGenChange
        end
        object cbCypher: TComboBox
          Left = 193
          Top = 152
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 3
          OnChange = cbHashCypherIVGenChange
        end
        object reInstructHashCypherIV: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object pbHashInfo: TButton
          Left = 337
          Top = 124
          Width = 21
          Height = 21
          Caption = '?'
          TabOrder = 2
          OnClick = pbHashInfoClick
        end
        object pbCypherInfo: TButton
          Left = 337
          Top = 152
          Width = 21
          Height = 21
          Caption = '?'
          TabOrder = 4
          OnClick = pbCypherInfoClick
        end
        object ckUsePerVolumeIV: TCheckBox
          Left = 193
          Top = 204
          Width = 260
          Height = 17
          Caption = 'Additionally &XOR sector IV with per volume IV'
          TabOrder = 6
        end
        object cbSectorIVGenMethod: TComboBox
          Left = 193
          Top = 180
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 5
          OnChange = cbHashCypherIVGenChange
        end
      end
      object tsMasterKeyLength: TTabSheet
        Caption = 'Master Key Length'
        ImageIndex = 11
        object Label2: TLabel
          Left = 113
          Top = 156
          Width = 89
          Height = 13
          Caption = '&Size of master key:'
          FocusControl = seMasterKeyLength
        end
        object Label9: TLabel
          Left = 300
          Top = 156
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object reInstructMasterKeyLen: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object seMasterKeyLength: TSpinEdit64
          Left = 233
          Top = 152
          Width = 61
          Height = 22
          Increment = 1
          MaxValue = 9999
          MinValue = 1
          TabOrder = 1
          Value = 9999
          OnChange = seMasterKeyLengthChange
          OnExit = seMasterKeyLengthExit
        end
      end
      object tsRNGSelect: TTabSheet
        Caption = 'RNG Select'
        ImageIndex = 4
        object reInstructRNGSelect: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 141
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
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
          Left = 8
          Top = 188
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
        ImageIndex = 8
        DesignSize = (
          492
          318)
        object lblMouseRNGBits: TLabel
          Left = 159
          Top = 294
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
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
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
        Caption = 'RNG PKCS token'
        ImageIndex = 17
        object lblToken: TLabel
          Left = 123
          Top = 175
          Width = 34
          Height = 13
          Caption = '&Token:'
          FocusControl = cbToken
        end
        object cbToken: TComboBox
          Left = 163
          Top = 170
          Width = 145
          Height = 21
          ItemHeight = 0
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
        object reInstructRNGPKCS11: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 61
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
      end
      object tsRNGGPG: TTabSheet
        Caption = 'RNG GPG'
        ImageIndex = 10
        object reInstructRNGGPG: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 77
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object GroupBox2: TGroupBox
          Left = 20
          Top = 224
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
      object tsPassword: TTabSheet
        Caption = 'Password'
        ImageIndex = 6
        object Label17: TLabel
          Left = 41
          Top = 148
          Width = 49
          Height = 13
          Caption = '&Password:'
          FocusControl = preUserKey1
        end
        object Label3: TLabel
          Left = 41
          Top = 220
          Width = 86
          Height = 13
          Caption = '&Confirm password:'
          FocusControl = preUserKey2
        end
        object reInstructPassword: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 113
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object preUserKey1: TOTFEFreeOTFE_PasswordRichEdit
          Left = 173
          Top = 144
          Width = 277
          Height = 61
          Lines.Strings = (
            'preUserKey1')
          ScrollBars = ssBoth
          TabOrder = 1
          OnChange = preUserKeyChange
        end
        object preUserKey2: TOTFEFreeOTFE_PasswordRichEdit
          Left = 173
          Top = 216
          Width = 277
          Height = 61
          Lines.Strings = (
            'preUserKey2')
          ScrollBars = ssBoth
          TabOrder = 2
          OnChange = preUserKeyChange
        end
      end
      object tsSummary: TTabSheet
        Caption = 'Summary'
        ImageIndex = 9
        DesignSize = (
          492
          318)
        object reSummary: TRichEdit
          Left = 8
          Top = 112
          Width = 473
          Height = 155
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clBtnFace
          Lines.Strings = (
            'reSummary')
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
        object pbAdvanced: TButton
          Left = 404
          Top = 278
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Advanced...'
          TabOrder = 3
          OnClick = pbAdvancedClick
        end
        object reInstructSummary: TOTFEFreeOTFE_InstructionRichEdit
          Left = 8
          Top = 12
          Width = 473
          Height = 89
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          TabOrder = 0
        end
        object ckAutoMountAfterCreate: TCheckBox
          Left = 8
          Top = 284
          Width = 309
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = '&Mount volume after creation'
          TabOrder = 2
        end
      end
    end
    inherited pnlButtons: TPanel
      inherited lblCompleteIndicator: TLabel
        Width = 95
      end
    end
  end
  object SaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 80
    Top = 304
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 48
    Top = 304
  end
end
