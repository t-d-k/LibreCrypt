object frmKeyEntryPlainLinux: TfrmKeyEntryPlainLinux
  Left = 388
  Top = 281
  Caption = 'LibreCrypt dm-crypt Key Entry'
  ClientHeight = 370
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    485
    370)
  PixelsPerInch = 96
  TextHeight = 13
  object pbCancel: TButton
    Left = 402
    Top = 334
    Width = 75
    Height = 28
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = pbCancelClick
    ExplicitLeft = 372
    ExplicitTop = 332
  end
  object pbOK: TButton
    Left = 315
    Top = 334
    Width = 81
    Height = 28
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = pbOKClick
    ExplicitLeft = 285
    ExplicitTop = 332
  end
  object pcEntry: TPageControl
    Left = 0
    Top = 0
    Width = 485
    Height = 317
    ActivePage = tsChooseContainer
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsNewKey: TTabSheet
      Caption = 'New Key'
      ImageIndex = 5
      inline frmeNewPassword1: TfrmeNewPassword
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Constraints.MinHeight = 223
        Constraints.MinWidth = 223
        TabOrder = 0
        ExplicitWidth = 477
        ExplicitHeight = 289
        inherited lblInstructPassword: TLabel
          Width = 477
          ExplicitLeft = 0
          ExplicitWidth = 475
        end
        inherited lblStrength: TLabel
          Top = 264
          Width = 94
          Anchors = [akLeft, akBottom]
          ExplicitTop = 264
          ExplicitWidth = 94
        end
        inherited ProgressBar1: TProgressBar
          Top = 264
          Width = 342
          Anchors = [akLeft, akRight, akBottom]
          ExplicitTop = 262
          ExplicitWidth = 312
        end
        inherited preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit
          Width = 341
          ExplicitWidth = 341
        end
        inherited preUserKeyConfirm: TOTFEFreeOTFE_PasswordRichEdit
          Width = 341
          Height = 70
          ExplicitWidth = 341
          ExplicitHeight = 70
        end
      end
    end
    object tsKey: TTabSheet
      Caption = 'Key'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'Key'
        TabOrder = 0
        object Label16: TLabel
          Left = 375
          Top = 256
          Width = 16
          Height = 13
          Caption = '(-K)'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label15: TLabel
          Left = 375
          Top = 220
          Width = 17
          Height = 13
          Caption = '(-G)'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label14: TLabel
          Left = 12
          Top = 220
          Width = 81
          Height = 13
          Caption = '&GPG executable:'
          Enabled = False
        end
        object Label21: TLabel
          Left = 11
          Top = 256
          Width = 59
          Height = 13
          Caption = 'G&PG keyfile:'
          Enabled = False
          FocusControl = feGPGKeyfile
        end
        inline feGPGExecutable: TSDUFilenameEdit
          Left = 108
          Top = 216
          Width = 261
          Height = 21
          Constraints.MaxHeight = 21
          Constraints.MinHeight = 21
          TabOrder = 0
          ExplicitLeft = 108
          ExplicitTop = 216
          ExplicitWidth = 261
          ExplicitHeight = 21
          DesignSize = (
            261
            21)
        end
        inline feGPGKeyfile: TSDUFilenameEdit
          Left = 108
          Top = 251
          Width = 261
          Height = 21
          Constraints.MaxHeight = 21
          Constraints.MinHeight = 21
          TabOrder = 1
          ExplicitLeft = 108
          ExplicitTop = 251
          ExplicitWidth = 261
          ExplicitHeight = 21
          DesignSize = (
            261
            21)
        end
        inline frmePassword1: TfrmePassword
          Left = 3
          Top = 16
          Width = 443
          Height = 194
          TabOrder = 2
          ExplicitLeft = 3
          ExplicitTop = 16
          ExplicitWidth = 443
          ExplicitHeight = 194
          inherited lblKeyPhrase: TLabel
            Left = 9
            Width = 56
            ExplicitLeft = 9
            ExplicitWidth = 56
          end
          inherited mmShown: TMemo
            Left = 71
            Width = 369
            Height = 174
            ExplicitLeft = 71
            ExplicitWidth = 369
            ExplicitHeight = 174
          end
        end
      end
    end
    object tsKeyOptions: TTabSheet
      Caption = 'Key Options'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 447
      ExplicitHeight = 287
      object GroupBox5: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'Key processing'
        TabOrder = 0
        ExplicitWidth = 447
        ExplicitHeight = 287
        DesignSize = (
          477
          289)
        object Label2: TLabel
          Left = 12
          Top = 52
          Width = 28
          Height = 13
          Caption = '&Hash:'
          FocusControl = cbKeyProcHash
        end
        object Label6: TLabel
          Left = 11
          Top = 100
          Width = 71
          Height = 13
          Caption = '&Iteration count:'
          Enabled = False
        end
        object Label13: TLabel
          Left = 435
          Top = 51
          Width = 17
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-H)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 407
        end
        object Label20: TLabel
          Left = 184
          Top = 100
          Width = 32
          Height = 13
          Caption = 'x 1000'
          Enabled = False
        end
        object Label3: TLabel
          Left = 107
          Top = 127
          Width = 36
          Height = 13
          Caption = '&Cypher:'
          Enabled = False
          FocusControl = cbKeyProcCypher
        end
        object Label7: TLabel
          Left = 435
          Top = 99
          Width = 16
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-C)'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 407
        end
        object Label18: TLabel
          Left = 12
          Top = 24
          Width = 28
          Height = 13
          Caption = '&Seed:'
          FocusControl = edKeySeed
        end
        object Label19: TLabel
          Left = 436
          Top = 23
          Width = 16
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-S)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 408
        end
        object cbKeyProcHash: TComboBox
          Left = 108
          Top = 48
          Width = 237
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akBottom]
          TabOrder = 1
          OnChange = SelectionChange
        end
        object seKeyProcCypherIterations: TSpinEdit64
          Left = 108
          Top = 96
          Width = 69
          Height = 22
          Enabled = False
          Increment = 1
          TabOrder = 5
          Value = 9999
          OnChange = SelectionChange
        end
        object pbKeyProcHashInfo: TButton
          Left = 391
          Top = 47
          Width = 21
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '?'
          TabOrder = 2
          OnClick = pbKeyProcHashInfoClick
          ExplicitLeft = 361
        end
        object pbKeyProcCypherInfo: TButton
          Left = 391
          Top = 124
          Width = 21
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '?'
          Enabled = False
          TabOrder = 6
          OnClick = pbKeyProcCypherInfoClick
          ExplicitLeft = 361
        end
        object cbKeyProcCypher: TComboBox
          Left = 177
          Top = 124
          Width = 196
          Height = 21
          Style = csDropDownList
          Anchors = [akTop, akRight]
          Enabled = False
          TabOrder = 4
          OnChange = SelectionChange
        end
        object edKeySeed: TEdit
          Left = 108
          Top = 20
          Width = 313
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'edKeySeed'
          OnChange = SelectionChange
          ExplicitWidth = 283
        end
        object ckHashWithAs: TSDUCheckBox
          Left = 108
          Top = 72
          Width = 249
          Height = 17
          Caption = 'Hash with "A"s, if hash output is too short'
          TabOrder = 3
          AutoSize = True
        end
      end
    end
    object tsEncryption: TTabSheet
      Caption = 'Encryption'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 447
      ExplicitHeight = 287
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'Encryption options'
        TabOrder = 0
        ExplicitWidth = 447
        ExplicitHeight = 287
        DesignSize = (
          477
          289)
        object Label23: TLabel
          Left = 12
          Top = 28
          Width = 36
          Height = 13
          Caption = '&Cypher:'
          FocusControl = cbMainCypher
        end
        object Label24: TLabel
          Left = 452
          Top = 28
          Width = 15
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-e)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 424
        end
        object cbMainCypher: TComboBox
          Left = 64
          Top = 24
          Width = 342
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = SelectionChange
        end
        object pbMainCypherInfo: TButton
          Left = 412
          Top = 24
          Width = 21
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '?'
          TabOrder = 1
          OnClick = pbMainCypherInfoClick
          ExplicitLeft = 382
        end
        object GroupBox6: TGroupBox
          Left = 12
          Top = 51
          Width = 455
          Height = 222
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'IV Generation'
          TabOrder = 2
          ExplicitWidth = 425
          ExplicitHeight = 220
          object Label5: TLabel
            Left = 12
            Top = 24
            Width = 52
            Height = 13
            Caption = '&Sector IVs:'
            FocusControl = cbSectorIVGenMethod
          end
          object lblIVHash: TLabel
            Left = 12
            Top = 124
            Width = 41
            Height = 13
            Caption = '&IV Hash:'
            FocusControl = cbSectorIVHash
          end
          object Label12: TLabel
            Left = 276
            Top = 60
            Width = 29
            Height = 13
            Caption = '(-o @)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblIVCypher: TLabel
            Left = 12
            Top = 152
            Width = 49
            Height = 13
            Caption = 'I&V Cypher:'
            FocusControl = cbSectorIVCypher
          end
          object cbSectorIVGenMethod: TComboBox
            Left = 84
            Top = 20
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = SelectionChange
          end
          object cbSectorIVHash: TComboBox
            Left = 84
            Top = 120
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnChange = SelectionChange
          end
          object pbIVHashInfo: TButton
            Left = 235
            Top = 121
            Width = 21
            Height = 21
            Caption = '?'
            TabOrder = 3
            OnClick = pbIVHashInfoClick
          end
          object rgSectorIVSectorZeroPos: TRadioGroup
            Left = 84
            Top = 52
            Width = 173
            Height = 57
            Caption = 'Sector &zero location'
            ItemIndex = 1
            Items.Strings = (
              'Start of host file'
              'Start of encrypted data')
            TabOrder = 1
            OnClick = SelectionChange
          end
          object cbSectorIVCypher: TComboBox
            Left = 84
            Top = 148
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 4
            OnChange = SelectionChange
          end
          object pbIVCypherInfo: TButton
            Left = 235
            Top = 148
            Width = 21
            Height = 21
            Caption = '?'
            TabOrder = 5
            OnClick = pbIVCypherInfoClick
          end
        end
      end
    end
    object tsFileOptions: TTabSheet
      Caption = 'File options'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 447
      ExplicitHeight = 287
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'File options'
        TabOrder = 0
        ExplicitWidth = 447
        ExplicitHeight = 287
        DesignSize = (
          477
          289)
        object lblOffset: TLabel
          Left = 11
          Top = 82
          Width = 31
          Height = 13
          Caption = '&Offset:'
        end
        object lblOffsetO: TLabel
          Left = 451
          Top = 82
          Width = 15
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-o)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 423
        end
        object Label10: TLabel
          Left = 11
          Top = 28
          Width = 40
          Height = 13
          Caption = 'Si&zelimit:'
        end
        object Label11: TLabel
          Left = 451
          Top = 28
          Width = 14
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '(-s)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitLeft = 423
        end
        object Label22: TLabel
          Left = 91
          Top = 59
          Width = 123
          Height = 13
          Caption = '("0" indicates no sizelimit.)'
        end
        object se64UnitOffset: TSDUSpin64Unit_Storage
          Left = 91
          Top = 78
          Width = 317
          Height = 29
          Anchors = [akLeft, akTop, akBottom]
          TabOrder = 0
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          ExplicitHeight = 27
        end
        object se64UnitSizeLimit: TSDUSpin64Unit_Storage
          Left = 91
          Top = 24
          Width = 317
          Height = 29
          Anchors = [akLeft, akTop, akBottom]
          TabOrder = 1
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          ExplicitHeight = 27
        end
      end
    end
    object tsMountOptions: TTabSheet
      Caption = 'Open options'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 447
      ExplicitHeight = 287
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'Open options'
        TabOrder = 0
        ExplicitWidth = 447
        ExplicitHeight = 287
        object lblDrive: TLabel
          Left = 12
          Top = 24
          Width = 28
          Height = 13
          Caption = '&Drive:'
          FocusControl = cbDrive
        end
        object lblReadOnlySwitch: TLabel
          Left = 236
          Top = 78
          Width = 12
          Height = 13
          Caption = '(-r)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblMountAs: TLabel
          Left = 12
          Top = 52
          Width = 43
          Height = 13
          Caption = '&Open as:'
          FocusControl = cbMediaType
        end
        object cbDrive: TComboBox
          Left = 92
          Top = 20
          Width = 113
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = SelectionChange
          Items.Strings = (
            'Z:')
        end
        object ckMountReadonly: TSDUCheckBox
          Left = 92
          Top = 76
          Width = 129
          Height = 17
          Caption = 'Open &readonly'
          TabOrder = 2
          OnClick = SelectionChange
          AutoSize = True
        end
        object cbMediaType: TComboBox
          Left = 92
          Top = 48
          Width = 113
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = SelectionChange
        end
        object ckMountForAllUsers: TSDUCheckBox
          Left = 92
          Top = 96
          Width = 145
          Height = 17
          Caption = 'Open for all &users'
          TabOrder = 3
          AutoSize = True
        end
      end
    end
    object tsChooseContainer: TTabSheet
      Caption = 'tsChooseContainer'
      ImageIndex = 6
      object GroupBox7: TGroupBox
        Left = 0
        Top = 0
        Width = 477
        Height = 289
        Align = alClient
        Caption = 'Choose Container'
        TabOrder = 0
        inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 467
          Height = 266
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          ExplicitLeft = 5
          ExplicitTop = 18
          ExplicitWidth = 467
          ExplicitHeight = 266
          DesignSize = (
            467
            266)
          inherited bbBrowsePartition: TBitBtn
            Left = 442
            Top = 3
            ExplicitLeft = 336
            ExplicitTop = 3
          end
          inherited bbBrowseFile: TBitBtn
            Left = 415
            Top = 3
            ExplicitLeft = 382
            ExplicitTop = 3
          end
          inherited edFilename: TEdit
            Left = 3
            Top = 3
            Width = 406
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 373
          end
        end
      end
    end
  end
  object pbLoad: TButton
    Left = 13
    Top = 334
    Width = 75
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = '&Load...'
    TabOrder = 1
    OnClick = pbLoadClick
    ExplicitTop = 332
  end
  object pbSave: TButton
    Left = 94
    Top = 334
    Width = 75
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = 'Sa&ve...'
    TabOrder = 2
    OnClick = pbSaveClick
    ExplicitTop = 332
  end
  object OpenSettingsFileDlg: TSDUOpenDialog
    PreserveCWD = False
    Left = 212
    Top = 328
  end
  object SaveSettingsFileDlg: TSDUSaveDialog
    PreserveCWD = False
    Left = 248
    Top = 328
  end
end
