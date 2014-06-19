object OTFEE4MTestApp_F: TOTFEE4MTestApp_F
  Left = 192
  Top = 286
  Width = 803
  Height = 552
  Caption = 'E4M Delphi Component Demo'
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
    Left = 712
    Top = 500
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = pbCloseClick
  end
  object pbVersion: TButton
    Left = 296
    Top = 40
    Width = 93
    Height = 25
    Caption = '&Version'
    TabOrder = 2
    OnClick = pbVersionClick
  end
  object pbIsEncryptedVolFile: TButton
    Left = 388
    Top = 288
    Width = 105
    Height = 25
    Caption = 'IsEncryptedVolFile'
    TabOrder = 3
    OnClick = pbIsPGPDiskVolumeClick
  end
  object edTestIfMtdVolFile: TEdit
    Left = 8
    Top = 288
    Width = 125
    Height = 21
    TabOrder = 4
    Text = 'edTestIfMtdVolFile'
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
    Left = 296
    Top = 320
    Width = 85
    Height = 25
    Caption = 'DismountVolume'
    TabOrder = 7
    OnClick = pbDisountVolumeClick
  end
  object pbMountVolume: TButton
    Left = 296
    Top = 288
    Width = 85
    Height = 25
    Caption = 'MountVolume'
    TabOrder = 8
    OnClick = pbMountVolumeClick
  end
  object pbDismountDrive: TButton
    Left = 364
    Top = 164
    Width = 81
    Height = 25
    Caption = 'DismountDrive'
    TabOrder = 9
    OnClick = pbDismountDriveClick
  end
  object pbClear: TButton
    Left = 504
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object pbGetDriveInfo: TButton
    Left = 164
    Top = 164
    Width = 81
    Height = 25
    Caption = 'GetDriveInfo'
    TabOrder = 11
    OnClick = pbGetDriveInfoClick
  end
  object rgActive: TRadioGroup
    Left = 148
    Top = 12
    Width = 125
    Height = 65
    Caption = 'E4M Component'
    ItemIndex = 1
    Items.Strings = (
      'Connect'
      'Disconnect')
    TabOrder = 0
    OnClick = rgActiveClick
  end
  object pbGetFileMountedForDrive: TButton
    Left = 164
    Top = 196
    Width = 125
    Height = 25
    Caption = 'GetFileMountedForDrive'
    TabOrder = 12
    OnClick = pbGetFileMountedForDriveClick
  end
  object pbGetDriveMountedForFile: TButton
    Left = 164
    Top = 320
    Width = 125
    Height = 25
    Caption = 'GetDriveMountedForFile'
    TabOrder = 13
    OnClick = pbGetDriveMountedForFileClick
  end
  object pbNumDrivesMounted: TButton
    Left = 208
    Top = 92
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
  object pbGetMountedDrives: TButton
    Left = 208
    Top = 124
    Width = 101
    Height = 25
    Caption = 'GetMountedDrives'
    TabOrder = 16
    OnClick = pbGetMountedDrivesClick
  end
  object pbDismountAll: TButton
    Left = 320
    Top = 92
    Width = 75
    Height = 25
    Caption = 'DismountAll'
    TabOrder = 17
    OnClick = pbDismountAllClick
  end
  object gbVolInfo: TGroupBox
    Left = 500
    Top = 20
    Width = 285
    Height = 133
    Caption = 'Volume Information'
    TabOrder = 18
    object lblReadOnly: TLabel
      Left = 104
      Top = 52
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblReadOnly'
    end
    object lblDriveMountedAs: TLabel
      Left = 104
      Top = 36
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblDriveMountedAs'
    end
    object lblFilename: TLabel
      Left = 104
      Top = 20
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblFilename'
    end
    object Label15: TLabel
      Left = 11
      Top = 52
      Width = 60
      Height = 13
      Caption = 'Mounted for:'
    end
    object Label22: TLabel
      Left = 11
      Top = 36
      Width = 86
      Height = 13
      Caption = 'Drive mounted as:'
    end
    object Label23: TLabel
      Left = 11
      Top = 20
      Width = 45
      Height = 13
      Caption = 'Filename:'
    end
    object lblAlgorithm: TLabel
      Left = 104
      Top = 68
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblAlgorithm'
    end
    object lblKeyGen: TLabel
      Left = 104
      Top = 84
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblKeyGen'
    end
    object Label8: TLabel
      Left = 11
      Top = 68
      Width = 33
      Height = 13
      Caption = 'Cipher:'
    end
    object Label11: TLabel
      Left = 11
      Top = 84
      Width = 69
      Height = 13
      Caption = 'Key generator:'
    end
    object Label1: TLabel
      Left = 12
      Top = 100
      Width = 61
      Height = 13
      Caption = 'Volume type:'
    end
    object lblVolumeType: TLabel
      Left = 104
      Top = 100
      Width = 69
      Height = 13
      Caption = 'lblVolumeType'
    end
  end
  object ckScramDiskSupport: TCheckBox
    Left = 296
    Top = 72
    Width = 153
    Height = 17
    Caption = 'ScramDisk support enabled'
    TabOrder = 19
    OnClick = ckScramDiskSupportClick
  end
  object pbIsDriverInstalled: TButton
    Left = 296
    Top = 12
    Width = 93
    Height = 25
    Caption = 'IsDriverInstalled'
    TabOrder = 20
    OnClick = pbIsDriverInstalledClick
  end
  object pbGetAvailableRawDevices: TButton
    Left = 52
    Top = 96
    Width = 129
    Height = 25
    Caption = 'GetAvailableRawDevices'
    TabOrder = 21
    OnClick = pbGetAvailableRawDevicesClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 356
    Width = 481
    Height = 161
    ScrollBars = ssBoth
    TabOrder = 22
    WordWrap = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'E4M Volume files (*.vol, *.svl)|*.vol;*.svl|All files (*.*)|*.*'
    Left = 84
    Top = 292
  end
  object OTFEE4M1: TOTFEE4M
    Active = False
    PasswordPrompt = 'Enter password for %s'
    MountWarning = True
    DefaultDrive = #0
    HidePasswordsEnteredCk = False
    Left = 60
    Top = 28
  end
end
