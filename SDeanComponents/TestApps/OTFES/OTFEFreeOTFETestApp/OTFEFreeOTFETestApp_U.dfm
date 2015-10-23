object OTFEFreeOTFETestApp_F: TOTFEFreeOTFETestApp_F
  Left = 250
  Top = 267
  Caption = 'FreeOTFE Delphi Component Demo'
  ClientHeight = 564
  ClientWidth = 905
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    905
    564)
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 476
    Top = 484
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = pbCloseClick
  end
  object pbVersion: TButton
    Left = 292
    Top = 44
    Width = 93
    Height = 25
    Caption = '&Version'
    TabOrder = 2
    OnClick = pbVersionClick
  end
  object pbIsEncryptedVolFile: TButton
    Left = 384
    Top = 288
    Width = 105
    Height = 25
    Caption = 'IsEncryptedVolFile'
    TabOrder = 3
    OnClick = pbIsEncryptedVolFileClick
  end
  object edTestIfMtdVolFile: TEdit
    Left = 8
    Top = 288
    Width = 125
    Height = 21
    TabOrder = 4
  end
  object DriveComboBox1: TDriveComboBox
    Left = 8
    Top = 164
    Width = 145
    Height = 19
    TabOrder = 5
  end
  object pbBrowse: TButton
    Left = 132
    Top = 288
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = pbBrowseClick
  end
  object pbDisountVolume: TButton
    Left = 292
    Top = 320
    Width = 85
    Height = 25
    Caption = 'DismountVolume'
    TabOrder = 7
    OnClick = pbDisountVolumeClick
  end
  object pbMountVolume: TButton
    Left = 292
    Top = 288
    Width = 85
    Height = 25
    Caption = 'MountVolume'
    TabOrder = 8
    OnClick = pbMountVolumeClick
  end
  object pbDismountDrive: TButton
    Left = 360
    Top = 160
    Width = 81
    Height = 25
    Caption = 'DismountDrive'
    TabOrder = 9
    OnClick = pbDismountDriveClick
  end
  object pbClear: TButton
    Left = 473
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object rgActive: TRadioGroup
    Left = 148
    Top = 12
    Width = 125
    Height = 65
    Caption = 'FreeOTFE Component'
    ItemIndex = 1
    Items.Strings = (
      'Connect'
      'Disconnect')
    TabOrder = 0
    OnClick = rgActiveClick
  end
  object ckDismountDriveEmergency: TCheckBox
    Left = 284
    Top = 244
    Width = 125
    Height = 17
    Caption = 'Emergency dismount'
    TabOrder = 11
  end
  object pbGetFileMountedForDrive: TButton
    Left = 160
    Top = 200
    Width = 125
    Height = 25
    Caption = 'GetFileMountedForDrive'
    TabOrder = 12
    OnClick = pbGetFileMountedForDriveClick
  end
  object pbGetDriveMountedForFile: TButton
    Left = 160
    Top = 320
    Width = 125
    Height = 25
    Caption = 'GetDriveMountedForFile'
    TabOrder = 13
    OnClick = pbGetDriveMountedForFileClick
  end
  object pbNumDrivesMounted: TButton
    Left = 208
    Top = 96
    Width = 101
    Height = 25
    Caption = 'NumDrivesMounted'
    TabOrder = 14
    OnClick = pbNumDrivesMountedClick
  end
  object pbRefresh: TButton
    Left = 8
    Top = 188
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 15
    OnClick = pbRefreshClick
  end
  object pbGetDrivesMounted: TButton
    Left = 208
    Top = 128
    Width = 101
    Height = 25
    Caption = 'GetDrivesMounted'
    TabOrder = 16
    OnClick = pbGetDrivesMountedClick
  end
  object pbIsDriverInstalled: TButton
    Left = 292
    Top = 12
    Width = 93
    Height = 25
    Caption = 'IsDriverInstalled'
    TabOrder = 17
    OnClick = pbIsDriverInstalledClick
  end
  object pbDismountAll: TButton
    Left = 320
    Top = 96
    Width = 75
    Height = 25
    Caption = 'DismountAll'
    TabOrder = 18
    OnClick = pbDismountAllClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 356
    Width = 454
    Height = 199
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 19
    WordWrap = False
  end
  object pbDriveInfo: TButton
    Left = 488
    Top = 160
    Width = 75
    Height = 25
    Caption = 'DriveInfo'
    TabOrder = 20
    OnClick = pbDriveInfoClick
  end
  object gbVolInfo: TGroupBox
    Left = 572
    Top = 4
    Width = 325
    Height = 233
    Caption = 'Volume Information'
    TabOrder = 21
    object lblReadOnly: TLabel
      Left = 104
      Top = 68
      Width = 213
      Height = 13
      AutoSize = False
      Caption = 'lblReadOnly'
    end
    object lblDriveMountedAs: TLabel
      Left = 104
      Top = 52
      Width = 213
      Height = 13
      AutoSize = False
      Caption = 'lblDriveMountedAs'
    end
    object Label15: TLabel
      Left = 12
      Top = 68
      Width = 60
      Height = 13
      Caption = 'Mounted for:'
    end
    object Label22: TLabel
      Left = 12
      Top = 52
      Width = 86
      Height = 13
      Caption = 'Drive mounted as:'
    end
    object Label1: TLabel
      Left = 12
      Top = 84
      Width = 74
      Height = 13
      Caption = 'IV hash device:'
    end
    object lblIVHashDevice: TLabel
      Left = 104
      Top = 84
      Width = 79
      Height = 13
      Caption = 'lblIVHashDevice'
    end
    object Label9: TLabel
      Left = 12
      Top = 100
      Width = 69
      Height = 13
      Caption = 'IV hash GUID:'
    end
    object lblIVHashGUID: TLabel
      Left = 104
      Top = 100
      Width = 72
      Height = 13
      Caption = 'lblIVHashGUID'
    end
    object Label2: TLabel
      Left = 12
      Top = 20
      Width = 54
      Height = 13
      Caption = 'Volume file:'
    end
    object lblVolumeFile: TLabel
      Left = 104
      Top = 20
      Width = 213
      Height = 13
      AutoSize = False
      Caption = 'lblVolumeFile'
    end
    object lblDevice: TLabel
      Left = 104
      Top = 36
      Width = 213
      Height = 13
      AutoSize = False
      Caption = 'lblDevice'
    end
    object Label7: TLabel
      Left = 12
      Top = 36
      Width = 37
      Height = 13
      Caption = 'Device:'
    end
    object Label3: TLabel
      Left = 12
      Top = 148
      Width = 71
      Height = 13
      Caption = 'Cypher device:'
    end
    object Label8: TLabel
      Left = 12
      Top = 164
      Width = 66
      Height = 13
      Caption = 'Cypher GUID:'
    end
    object lblMainCypherGUID: TLabel
      Left = 104
      Top = 164
      Width = 93
      Height = 13
      Caption = 'lblMainCypherGUID'
    end
    object lblMainCypherDevice: TLabel
      Left = 104
      Top = 148
      Width = 100
      Height = 13
      Caption = 'lblMainCypherDevice'
    end
    object Label6: TLabel
      Left = 12
      Top = 180
      Width = 48
      Height = 13
      Caption = 'Metadata:'
    end
    object Label11: TLabel
      Left = 12
      Top = 132
      Width = 78
      Height = 13
      Caption = 'IV cypher GUID:'
    end
    object Label35: TLabel
      Left = 12
      Top = 116
      Width = 83
      Height = 13
      Caption = 'IV cypher device:'
    end
    object lblIVCypherDevice: TLabel
      Left = 104
      Top = 116
      Width = 87
      Height = 13
      Caption = 'lblIVCypherDevice'
    end
    object lblIVCypherGUID: TLabel
      Left = 104
      Top = 132
      Width = 80
      Height = 13
      Caption = 'lblIVCypherGUID'
    end
    object reMetaData: TRichEdit
      Left = 104
      Top = 180
      Width = 209
      Height = 45
      Color = clBtnFace
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'reMetaData')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pbVersionStr: TButton
    Left = 392
    Top = 44
    Width = 93
    Height = 25
    Caption = '&VersionStr'
    TabOrder = 22
    OnClick = pbVersionStrClick
  end
  object pbGetCypherDrivers: TButton
    Left = 488
    Top = 44
    Width = 91
    Height = 25
    Caption = 'GetCypherDrivers'
    TabOrder = 23
    OnClick = pbGetCypherDriversClick
  end
  object pbGetHashDrivers: TButton
    Left = 488
    Top = 72
    Width = 91
    Height = 25
    Caption = 'GetHashDrivers'
    TabOrder = 24
    OnClick = pbGetHashDriversClick
  end
  object pcTest: TPageControl
    Left = 556
    Top = 240
    Width = 341
    Height = 317
    ActivePage = tsCypher
    TabOrder = 25
    object tsCypher: TTabSheet
      Caption = 'Cypher'
      object GroupBox2: TGroupBox
        Left = 4
        Top = 4
        Width = 325
        Height = 282
        Caption = 'Cypher test'
        TabOrder = 0
        object Label12: TLabel
          Left = 44
          Top = 20
          Width = 31
          Height = 13
          Caption = 'Driver:'
        end
        object Label13: TLabel
          Left = 8
          Top = 40
          Width = 66
          Height = 13
          Caption = 'Cypher GUID:'
        end
        object Label14: TLabel
          Left = 20
          Top = 100
          Width = 54
          Height = 13
          Caption = 'In filename:'
        end
        object Label16: TLabel
          Left = 12
          Top = 120
          Width = 65
          Height = 13
          Caption = 'Out filename: '
        end
        object Label17: TLabel
          Left = 52
          Top = 140
          Width = 21
          Height = 13
          Caption = 'Key:'
        end
        object Label19: TLabel
          Left = 59
          Top = 160
          Width = 13
          Height = 13
          Caption = 'IV:'
        end
        object Label36: TLabel
          Left = 26
          Top = 60
          Width = 48
          Height = 13
          Caption = 'Sector ID:'
        end
        object Label37: TLabel
          Left = 19
          Top = 80
          Width = 55
          Height = 13
          Caption = 'Sector size:'
        end
        object Label38: TLabel
          Left = 24
          Top = 192
          Width = 19
          Height = 13
          Caption = 'File:'
        end
        object Label39: TLabel
          Left = 24
          Top = 224
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object edCypherDriver: TEdit
          Left = 80
          Top = 16
          Width = 237
          Height = 21
          TabOrder = 0
          Text = '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000010001}'
        end
        object edCypherGUID: TEdit
          Left = 80
          Top = 36
          Width = 237
          Height = 21
          TabOrder = 1
          Text = '{00000000-0000-0000-0000-000000010004}'
        end
        object edCypherInFilename: TEdit
          Left = 80
          Top = 97
          Width = 237
          Height = 21
          TabOrder = 2
          Text = 'c:\cc_twofish_512 block 01.dat'
        end
        object pbEncryptFile: TButton
          Left = 87
          Top = 185
          Width = 75
          Height = 25
          Caption = 'Encrypt'
          TabOrder = 6
          OnClick = pbEncryptFileClick
        end
        object edCypherOutFilename: TEdit
          Left = 80
          Top = 116
          Width = 237
          Height = 21
          TabOrder = 3
          Text = 'c:\cc_twofish_512 block 01.out'
        end
        object edCypherKey: TEdit
          Left = 79
          Top = 136
          Width = 237
          Height = 21
          TabOrder = 4
          Text = '2C08E8F5884750A7B99F6F2F342FC638DB25FF3100000000'
        end
        object edCypherIV: TEdit
          Left = 79
          Top = 156
          Width = 237
          Height = 21
          TabOrder = 5
          Text = '00000000000000000000000000000000'
        end
        object pbDecryptFile: TButton
          Left = 168
          Top = 185
          Width = 75
          Height = 25
          Caption = 'Decrypt'
          TabOrder = 7
          OnClick = pbDecryptFileClick
        end
        object edCypherSectorID: TEdit
          Left = 80
          Top = 56
          Width = 237
          Height = 21
          TabOrder = 8
          Text = '0000000000000000'
        end
        object edCypherSectorSize: TEdit
          Left = 80
          Top = 76
          Width = 237
          Height = 21
          TabOrder = 9
          Text = '512'
        end
        object pbLRWTest: TButton
          Left = 119
          Top = 250
          Width = 75
          Height = 25
          Caption = 'pbLRWTest'
          TabOrder = 10
          OnClick = pbLRWTestClick
        end
        object pbEncryptData: TButton
          Left = 87
          Top = 216
          Width = 75
          Height = 25
          Caption = 'Encrypt'
          TabOrder = 11
          OnClick = pbEncryptDataClick
        end
        object pbDecryptData: TButton
          Left = 168
          Top = 216
          Width = 75
          Height = 25
          Caption = 'Decrypt'
          TabOrder = 12
          OnClick = pbDecryptDataClick
        end
        object Button1: TButton
          Left = 220
          Top = 248
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 13
          OnClick = Button1Click
        end
      end
    end
    object tsHash: TTabSheet
      Caption = 'Hash'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 4
        Top = 4
        Width = 325
        Height = 113
        Caption = 'Hash test'
        TabOrder = 0
        object Label4: TLabel
          Left = 44
          Top = 20
          Width = 31
          Height = 13
          Caption = 'Driver:'
        end
        object Label5: TLabel
          Left = 16
          Top = 40
          Width = 58
          Height = 13
          Caption = 'Hash GUID:'
        end
        object Label10: TLabel
          Left = 48
          Top = 60
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object edHashDriver: TEdit
          Left = 80
          Top = 16
          Width = 237
          Height = 21
          TabOrder = 0
          Text = '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-0000000D0001}'
        end
        object edHashGUID: TEdit
          Left = 80
          Top = 36
          Width = 237
          Height = 21
          TabOrder = 1
          Text = '{00000000-0000-0000-0000-0000000D0002}'
        end
        object edHashData: TEdit
          Left = 80
          Top = 56
          Width = 237
          Height = 21
          TabOrder = 2
          Text = 'password'
        end
        object pbHashData: TButton
          Left = 124
          Top = 80
          Width = 75
          Height = 25
          Caption = 'HashData'
          TabOrder = 3
          OnClick = pbHashDataClick
        end
      end
    end
    object tsMAC: TTabSheet
      Caption = 'MAC'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TGroupBox
        Left = 4
        Top = 4
        Width = 325
        Height = 245
        Caption = 'MAC test'
        TabOrder = 0
        object Label18: TLabel
          Left = 16
          Top = 20
          Width = 57
          Height = 13
          Caption = 'Hash driver:'
        end
        object Label20: TLabel
          Left = 16
          Top = 40
          Width = 58
          Height = 13
          Caption = 'Hash GUID:'
        end
        object Label21: TLabel
          Left = 52
          Top = 60
          Width = 21
          Height = 13
          Caption = 'Key:'
        end
        object Label23: TLabel
          Left = 48
          Top = 104
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object Label24: TLabel
          Left = 28
          Top = 124
          Width = 46
          Height = 13
          Caption = 'Truncate:'
        end
        object Label32: TLabel
          Left = 48
          Top = 148
          Width = 26
          Height = 13
          Caption = 'MAC:'
        end
        object Label34: TLabel
          Left = 208
          Top = 124
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object edMACHashDriver: TEdit
          Left = 80
          Top = 16
          Width = 237
          Height = 21
          TabOrder = 0
          Text = '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-0000000D0001}'
        end
        object edMACHashGUID: TEdit
          Left = 80
          Top = 36
          Width = 237
          Height = 21
          TabOrder = 1
          Text = '{00000000-0000-0000-0000-0000000D0002}'
        end
        object pbMACData: TButton
          Left = 116
          Top = 176
          Width = 75
          Height = 25
          Caption = 'MACData'
          TabOrder = 5
          OnClick = pbMACDataClick
        end
        object edMACData: TEdit
          Left = 80
          Top = 100
          Width = 237
          Height = 21
          TabOrder = 3
          Text = 'Sample #3'
        end
        object seMACTruncate: TSpinEdit64
          Left = 80
          Top = 120
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 4
          Value = -1
        end
        object reMACKey: TRichEdit
          Left = 80
          Top = 56
          Width = 237
          Height = 45
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            '50515253 54555657 58595a5b 5c5d5e5f'
            '60616263 64656667 68696a6b 6c6d6e6f'
            '70717273 74757677 78797a7b 7c7d7e7f'
            '80818283 84858687 88898a8b 8c8d8e8f'
            '90919293 94959697 98999a9b 9c9d9e9f'
            'a0a1a2a3 a4a5a6a7 a8a9aaab acadaeaf'
            'b0b1b2b3')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
        end
        object cbMAC: TComboBox
          Left = 80
          Top = 144
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 6
        end
      end
    end
    object tsKDF: TTabSheet
      Caption = 'KDF'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox4: TGroupBox
        Left = 4
        Top = 4
        Width = 325
        Height = 281
        Caption = 'KDF test'
        TabOrder = 0
        object Label25: TLabel
          Left = 16
          Top = 20
          Width = 57
          Height = 13
          Caption = 'Hash driver:'
        end
        object Label26: TLabel
          Left = 16
          Top = 40
          Width = 58
          Height = 13
          Caption = 'Hash GUID:'
        end
        object Label27: TLabel
          Left = 24
          Top = 60
          Width = 49
          Height = 13
          Caption = 'Password:'
        end
        object Label28: TLabel
          Left = 52
          Top = 132
          Width = 21
          Height = 13
          Caption = 'Salt:'
        end
        object Label29: TLabel
          Left = 40
          Top = 200
          Width = 33
          Height = 13
          Caption = 'dkLen:'
        end
        object Label30: TLabel
          Left = 28
          Top = 176
          Width = 46
          Height = 13
          Caption = 'Iterations:'
        end
        object Label31: TLabel
          Left = 204
          Top = 200
          Width = 25
          Height = 13
          Caption = 'bytes'
        end
        object Label33: TLabel
          Left = 48
          Top = 224
          Width = 24
          Height = 13
          Caption = 'KDF:'
        end
        object edKDFHashDriver: TEdit
          Left = 80
          Top = 16
          Width = 237
          Height = 21
          TabOrder = 0
          Text = '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-0000000D0001}'
        end
        object edKDFHashGUID: TEdit
          Left = 80
          Top = 36
          Width = 237
          Height = 21
          TabOrder = 1
          Text = '{00000000-0000-0000-0000-0000000D0002}'
        end
        object pbDeriveKey: TButton
          Left = 120
          Top = 244
          Width = 75
          Height = 25
          Caption = 'DeriveKey'
          TabOrder = 5
          OnClick = pbDeriveKeyClick
        end
        object edKDFSalt: TEdit
          Left = 80
          Top = 128
          Width = 237
          Height = 21
          TabOrder = 3
          Text = '78 57 8E 5A 5D 63 CB 06'
        end
        object seKDFdkLen: TSpinEdit64
          Left = 80
          Top = 196
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 4
          Value = 64
        end
        object reKDFPassword: TRichEdit
          Left = 80
          Top = 56
          Width = 237
          Height = 45
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            '70 61 73 73 77 6F 72 64')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
        end
        object seKDFIterations: TSpinEdit64
          Left = 80
          Top = 172
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 6
          Value = 2
        end
        object ckKDFPasswordHex: TCheckBox
          Left = 80
          Top = 100
          Width = 97
          Height = 17
          Caption = 'Hex rep'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object ckKDFSaltHex: TCheckBox
          Left = 80
          Top = 148
          Width = 97
          Height = 17
          Caption = 'Hex rep'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object cbKDF: TComboBox
          Left = 80
          Top = 220
          Width = 165
          Height = 21
          Style = csDropDownList
          TabOrder = 9
        end
      end
    end
  end
  object pbCreateFreeOTFEVolumeWizard: TButton
    Left = 8
    Top = 108
    Width = 181
    Height = 25
    Caption = 'Create FreeOTFE Volume Wizard...'
    TabOrder = 26
    OnClick = pbCreateFreeOTFEVolumeWizardClick
  end
  object pbCDBTests: TButton
    Left = 8
    Top = 136
    Width = 89
    Height = 25
    Caption = 'CDB Tests...'
    TabOrder = 27
    OnClick = pbCDBTestsClick
  end
  object ckDebugShowmessage: TCheckBox
    Left = 8
    Top = 4
    Width = 129
    Height = 17
    Caption = 'Debug Showmessage'
    TabOrder = 28
    OnClick = ckDebugShowmessageClick
  end
  object pbTestHCSelect: TButton
    Left = 8
    Top = 80
    Width = 137
    Height = 25
    Caption = 'Test hash/cypher select...'
    TabOrder = 29
    OnClick = pbTestHCSelectClick
  end
  object pbSelectPartition: TButton
    Left = 8
    Top = 52
    Width = 93
    Height = 25
    Caption = 'Select Partition...'
    TabOrder = 30
    OnClick = pbSelectPartitionClick
  end
  object DbgClr: TButton
    Left = 8
    Top = 24
    Width = 53
    Height = 25
    Caption = 'DbgClr'
    TabOrder = 31
    OnClick = DbgClrClick
  end
  object DbgDisp: TButton
    Left = 64
    Top = 24
    Width = 53
    Height = 25
    Caption = 'DbgDisp'
    TabOrder = 32
    OnClick = DbgDispClick
  end
  object pbCryptlibTests: TButton
    Left = 104
    Top = 136
    Width = 85
    Height = 25
    Caption = 'cryptlib Tests...'
    TabOrder = 33
    OnClick = pbCryptlibTestsClick
  end
  object pbRNGTests: TButton
    Left = 192
    Top = 156
    Width = 85
    Height = 25
    Caption = 'RNG Tests...'
    TabOrder = 34
    OnClick = pbRNGTestsClick
  end
  object pbDLLREADMounted: TButton
    Left = 452
    Top = 220
    Width = 75
    Height = 25
    Caption = 'pbDLLREADMounted'
    TabOrder = 35
    OnClick = pbDLLREADMountedClick
  end
  object pbDLLWRITEMounted: TButton
    Left = 452
    Top = 252
    Width = 75
    Height = 25
    Caption = 'pbDLLWRITEMounted'
    TabOrder = 36
    OnClick = pbDLLWRITEMountedClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 'FreeOTFE volume files (*.vol)|*.vol|All files (*.*)|*.*'
    Left = 84
    Top = 292
  end
end
