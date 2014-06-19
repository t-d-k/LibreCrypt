object OTFEPGPDiskTestApp_F: TOTFEPGPDiskTestApp_F
  Left = 245
  Top = 129
  Width = 572
  Height = 545
  Caption = 'PGPDisk Delphi Component Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    564
    518)
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 480
    Top = 488
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
    Left = 472
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object pbHasDriveOpenFiles: TButton
    Left = 452
    Top = 164
    Width = 101
    Height = 25
    Caption = 'HasDriveOpenFiles'
    TabOrder = 11
    OnClick = pbHasDriveOpenFilesClick
  end
  object rgActive: TRadioGroup
    Left = 148
    Top = 12
    Width = 125
    Height = 65
    Caption = 'PGPDisk Component'
    ItemIndex = 1
    Items.Strings = (
      'Connect'
      'Disconnect')
    TabOrder = 0
    OnClick = rgActiveClick
  end
  object ckDismountDriveEmergency: TCheckBox
    Left = 272
    Top = 244
    Width = 125
    Height = 17
    Caption = 'Emergency dismount'
    TabOrder = 12
  end
  object pbGetFileMountedForDrive: TButton
    Left = 160
    Top = 196
    Width = 125
    Height = 25
    Caption = 'GetFileMountedForDrive'
    TabOrder = 13
    OnClick = pbGetFileMountedForDriveClick
  end
  object pbGetDriveMountedForFile: TButton
    Left = 164
    Top = 320
    Width = 125
    Height = 25
    Caption = 'GetDriveMountedForFile'
    TabOrder = 14
    OnClick = pbGetDriveMountedForFileClick
  end
  object pbNumDrivesMounted: TButton
    Left = 208
    Top = 96
    Width = 101
    Height = 25
    Caption = 'NumDrivesMounted'
    TabOrder = 15
    OnClick = pbNumDrivesMountedClick
  end
  object pbRefresh: TButton
    Left = 8
    Top = 188
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 16
    OnClick = pbRefreshClick
  end
  object pbGetDrivesMounted: TButton
    Left = 208
    Top = 128
    Width = 101
    Height = 25
    Caption = 'GetDrivesMounted'
    TabOrder = 17
    OnClick = pbGetDrivesMountedClick
  end
  object pbIsDriverInstalled: TButton
    Left = 292
    Top = 12
    Width = 93
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'IsDriverInstalled'
    TabOrder = 18
    OnClick = pbIsDriverInstalledClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 356
    Width = 453
    Height = 153
    ScrollBars = ssBoth
    TabOrder = 19
    WordWrap = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PGPDisk Volume files (*.pgd)|*.pgd|All files (*.*)|*.*'
    Left = 84
    Top = 292
  end
  object OTFEPGPDisk1: TOTFEPGPDisk
    Active = False
    Left = 60
    Top = 28
  end
end
