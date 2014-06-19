object BestCryptTestApp_F: TBestCryptTestApp_F
  Left = 133
  Top = 111
  Width = 803
  Height = 552
  Caption = 'BestCrypt Delphi Component Demo'
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
    Top = 492
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
    Left = 388
    Top = 288
    Width = 105
    Height = 25
    Caption = 'IsEncryptedVolFile'
    TabOrder = 3
    OnClick = pbIsEncryptedVolumeClick
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
    Left = 500
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object pbGetVolumeInfo2: TButton
    Left = 164
    Top = 164
    Width = 81
    Height = 25
    Caption = 'GetVolumeInfo'
    TabOrder = 11
    OnClick = pbGetVolumeInfo2Click
  end
  object rgActive: TRadioGroup
    Left = 148
    Top = 12
    Width = 125
    Height = 65
    Caption = 'BestCrypt Component'
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
  object pbGetMountedDrives: TButton
    Left = 208
    Top = 128
    Width = 101
    Height = 25
    Caption = 'GetMountedDrives'
    TabOrder = 16
    OnClick = pbGetMountedDrivesClick
  end
  object pbDismountAll: TButton
    Left = 320
    Top = 96
    Width = 75
    Height = 25
    Caption = 'DismountAll'
    TabOrder = 17
    OnClick = pbDismountAllClick
  end
  object gbVolInfo: TGroupBox
    Left = 500
    Top = 8
    Width = 285
    Height = 329
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
    object Label3: TLabel
      Left = 12
      Top = 164
      Width = 56
      Height = 13
      Caption = 'Description:'
    end
    object lblExtent: TLabel
      Left = 104
      Top = 68
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblExtent'
    end
    object lblVersion: TLabel
      Left = 104
      Top = 84
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblVersion'
    end
    object lblAlgorithm: TLabel
      Left = 104
      Top = 116
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblAlgorithm'
    end
    object lblFileSystemID: TLabel
      Left = 104
      Top = 100
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblFileSystemID'
    end
    object lblKeyGen: TLabel
      Left = 104
      Top = 132
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblKeyGen'
    end
    object Label4: TLabel
      Left = 11
      Top = 68
      Width = 33
      Height = 13
      Caption = 'Extent:'
    end
    object Label5: TLabel
      Left = 11
      Top = 84
      Width = 38
      Height = 13
      Caption = 'Version:'
    end
    object Label8: TLabel
      Left = 11
      Top = 116
      Width = 33
      Height = 13
      Caption = 'Cipher:'
    end
    object Label10: TLabel
      Left = 11
      Top = 100
      Width = 68
      Height = 13
      Caption = 'File system ID:'
    end
    object Label11: TLabel
      Left = 11
      Top = 132
      Width = 69
      Height = 13
      Caption = 'Key generator:'
    end
    object Label1: TLabel
      Left = 11
      Top = 148
      Width = 89
      Height = 13
      Caption = 'Errors In Mounting:'
    end
    object lblErrsInMounting: TLabel
      Left = 104
      Top = 148
      Width = 81
      Height = 13
      Caption = 'lblErrsInMounting'
    end
    object reDescription: TRichEdit
      Left = 12
      Top = 180
      Width = 261
      Height = 137
      Color = clBtnFace
      Lines.Strings = (
        'reDescription')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pbGetKeyGenIDs: TButton
    Left = 72
    Top = 96
    Width = 117
    Height = 25
    Caption = 'Get KeyGen Details'
    TabOrder = 19
    OnClick = pbGetKeyGenIDsClick
  end
  object pbGetAlgIDs: TButton
    Left = 72
    Top = 128
    Width = 117
    Height = 25
    Caption = 'Get Algorithm Details'
    TabOrder = 20
    OnClick = pbGetAlgIDsClick
  end
  object pbGetVolumeInfo: TButton
    Left = 164
    Top = 288
    Width = 81
    Height = 25
    Caption = 'GetVolumeInfo'
    TabOrder = 21
    OnClick = pbGetVolumeInfoClick
  end
  object pbIsDriverInstalled: TButton
    Left = 292
    Top = 12
    Width = 93
    Height = 25
    Caption = 'IsDriverInstalled'
    TabOrder = 22
    OnClick = pbIsDriverInstalledClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 352
    Width = 481
    Height = 165
    ScrollBars = ssBoth
    TabOrder = 23
    WordWrap = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'BestCrypt Volume files|*.jbc|All files (*.*)|*.*'
    Left = 84
    Top = 292
  end
  object OTFEBestCrypt1: TOTFEBestCrypt
    Active = False
    Left = 64
    Top = 28
  end
end
