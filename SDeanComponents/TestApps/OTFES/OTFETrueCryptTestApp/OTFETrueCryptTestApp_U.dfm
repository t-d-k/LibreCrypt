object OTFETrueCryptTestApp_F: TOTFETrueCryptTestApp_F
  Left = 135
  Top = 324
  Width = 913
  Height = 545
  Caption = 'TrueCryptDelphi Component Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 484
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
    Enabled = False
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
    Caption = 'TrueCrypt Component'
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
    Enabled = False
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
    Height = 153
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
    Top = 24
    Width = 313
    Height = 441
    Caption = 'Volume Information'
    TabOrder = 21
    object lblReadOnly: TLabel
      Left = 112
      Top = 148
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblReadOnly'
    end
    object lblDriveMountedAs: TLabel
      Left = 112
      Top = 20
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblDriveMountedAs'
    end
    object Label15: TLabel
      Left = 12
      Top = 148
      Width = 60
      Height = 13
      Caption = 'Mounted for:'
    end
    object Label22: TLabel
      Left = 12
      Top = 20
      Width = 86
      Height = 13
      Caption = 'Drive mounted as:'
    end
    object lblCipher: TLabel
      Left = 112
      Top = 84
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblCipher'
    end
    object Label8: TLabel
      Left = 12
      Top = 84
      Width = 33
      Height = 13
      Caption = 'Cipher:'
    end
    object Label1: TLabel
      Left = 12
      Top = 164
      Width = 56
      Height = 13
      Caption = 'Disk length:'
    end
    object lblDiskLength: TLabel
      Left = 112
      Top = 164
      Width = 64
      Height = 13
      Caption = 'lblDiskLength'
    end
    object Label6: TLabel
      Left = 12
      Top = 180
      Width = 77
      Height = 13
      Caption = 'Volume created:'
    end
    object lblVolumeCreated: TLabel
      Left = 112
      Top = 180
      Width = 82
      Height = 13
      Caption = 'lblVolumeCreated'
    end
    object Label9: TLabel
      Left = 12
      Top = 52
      Width = 78
      Height = 13
      Caption = 'Volume location:'
    end
    object lblVolumeLocation: TLabel
      Left = 112
      Top = 52
      Width = 86
      Height = 13
      Caption = 'lblVolumeLocation'
    end
    object Label2: TLabel
      Left = 12
      Top = 36
      Width = 54
      Height = 13
      Caption = 'Volume file:'
    end
    object lblVolumeFile: TLabel
      Left = 112
      Top = 36
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblVolumeFile'
    end
    object PKCS5: TLabel
      Left = 12
      Top = 116
      Width = 37
      Height = 13
      Caption = 'PKCS5:'
    end
    object lblpkcs5: TLabel
      Left = 112
      Top = 116
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblpkcs5'
    end
    object lblpkcs5Iterations: TLabel
      Left = 112
      Top = 132
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblpkcs5Iterations'
    end
    object Label4: TLabel
      Left = 20
      Top = 132
      Width = 82
      Height = 13
      Caption = 'PKCS5 iterations:'
    end
    object lblPasswordChanged: TLabel
      Left = 112
      Top = 196
      Width = 99
      Height = 13
      Caption = 'lblPasswordChanged'
    end
    object Label5: TLabel
      Left = 12
      Top = 196
      Width = 94
      Height = 13
      Caption = 'Password changed:'
    end
    object Label3: TLabel
      Left = 20
      Top = 100
      Width = 62
      Height = 13
      Caption = 'Cipher mode:'
    end
    object lblCipherMode: TLabel
      Left = 112
      Top = 100
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblCipherMode'
    end
    object Label10: TLabel
      Left = 12
      Top = 68
      Width = 61
      Height = 13
      Caption = 'Volume type:'
    end
    object lblVolumeType: TLabel
      Left = 112
      Top = 68
      Width = 193
      Height = 13
      AutoSize = False
      Caption = 'lblVolumeType'
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
  object OpenDialog1: TOpenDialog
    Filter = 'TrueCrypt Volume files (*.tc)|*.tc|All files (*.*)|*.*'
    Left = 84
    Top = 292
  end
  object OTFETrueCrypt1: TOTFETrueCrypt
    Active = False
    PasswordPrompt = 'Enter password for %s'
    CloseExplorerWindowsOnDismount = True
    SaveMountedVolumesHistory = True
    LastSelectedDrive = 'Q'
    Left = 56
    Top = 36
  end
end
