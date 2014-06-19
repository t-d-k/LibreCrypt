object OTFEScramDiskTestApp_F: TOTFEScramDiskTestApp_F
  Left = 286
  Top = 177
  Caption = 'ScramDisk Delphi Component Demo'
  ClientHeight = 531
  ClientWidth = 796
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
    Left = 292
    Top = 44
    Width = 93
    Height = 25
    Caption = '&Version'
    TabOrder = 2
    OnClick = pbVersionClick
  end
  object pbIsEncryptedVolFile: TButton
    Left = 156
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
    Left = 464
    Top = 480
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = pbClearClick
  end
  object pbGetVolumeInfo2: TButton
    Left = 160
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
    Caption = 'ScramDisk Component'
    ItemIndex = 1
    Items.Strings = (
      'Connect'
      'Disconnect')
    TabOrder = 0
    OnClick = rgActiveClick
  end
  object pbGetFileMountedForDrive: TButton
    Left = 160
    Top = 196
    Width = 125
    Height = 25
    Caption = 'GetFileMountedForDrive'
    TabOrder = 12
    OnClick = pbGetFileMountedForDriveClick
  end
  object pbGetDriveMountedForFile: TButton
    Left = 156
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
    Top = 20
    Width = 285
    Height = 441
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
      Left = 12
      Top = 52
      Width = 60
      Height = 13
      Caption = 'Mounted for:'
    end
    object Label22: TLabel
      Left = 12
      Top = 36
      Width = 86
      Height = 13
      Caption = 'Drive mounted as:'
    end
    object lblFilePartitionNameStr: TLabel
      Left = 12
      Top = 20
      Width = 105
      Height = 13
      Caption = 'lblFilePartitionNameStr'
    end
    object lblDescriptionStr: TLabel
      Left = 12
      Top = 276
      Width = 56
      Height = 13
      Caption = 'Description:'
    end
    object lblSlotNo: TLabel
      Left = 104
      Top = 100
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblSlotNo'
    end
    object lblHostDrive: TLabel
      Left = 104
      Top = 116
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblHostDrive'
    end
    object lblCipher: TLabel
      Left = 104
      Top = 132
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblCipher'
    end
    object lblMountType: TLabel
      Left = 104
      Top = 148
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblMountType'
    end
    object Label4: TLabel
      Left = 12
      Top = 100
      Width = 59
      Height = 13
      Caption = 'Slot number:'
    end
    object Label5: TLabel
      Left = 12
      Top = 116
      Width = 51
      Height = 13
      Caption = 'Host drive:'
    end
    object Label8: TLabel
      Left = 12
      Top = 132
      Width = 33
      Height = 13
      Caption = 'Cipher:'
    end
    object Label11: TLabel
      Left = 12
      Top = 148
      Width = 56
      Height = 13
      Caption = 'Mount type:'
    end
    object Label1: TLabel
      Left = 12
      Top = 228
      Width = 60
      Height = 13
      Caption = 'Last access:'
    end
    object lblLastAccess: TLabel
      Left = 104
      Top = 228
      Width = 65
      Height = 13
      Caption = 'lblLastAccess'
    end
    object Label6: TLabel
      Left = 12
      Top = 260
      Width = 72
      Height = 13
      Caption = 'Preferred drive:'
    end
    object lblPreferredDrive: TLabel
      Left = 104
      Top = 260
      Width = 78
      Height = 13
      Caption = 'lblPreferredDrive'
    end
    object Label9: TLabel
      Left = 12
      Top = 244
      Width = 70
      Height = 13
      Caption = '# of accesses:'
    end
    object lblNoAccesses: TLabel
      Left = 104
      Top = 244
      Width = 70
      Height = 13
      Caption = 'lblNoAccesses'
    end
    object lblWavBitsUsedStr: TLabel
      Left = 20
      Top = 164
      Width = 73
      Height = 13
      Caption = 'WAV bits used:'
    end
    object lblWavBitsUsed: TLabel
      Left = 104
      Top = 164
      Width = 75
      Height = 13
      Caption = 'lblWavBitsUsed'
    end
    object lblROSoftmountStr: TLabel
      Left = 20
      Top = 68
      Width = 68
      Height = 13
      Caption = 'RO softmount:'
    end
    object lblROSoftmount: TLabel
      Left = 104
      Top = 68
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblROSoftmount'
    end
    object lblROMediaStr: TLabel
      Left = 20
      Top = 84
      Width = 82
      Height = 13
      Caption = 'RO media/volfile:'
    end
    object lblROMedia: TLabel
      Left = 104
      Top = 84
      Width = 173
      Height = 13
      AutoSize = False
      Caption = 'lblROMedia'
    end
    object lblPartRemovableStr: TLabel
      Left = 20
      Top = 180
      Width = 77
      Height = 13
      Caption = 'Part. removable:'
    end
    object lblPartRemovable: TLabel
      Left = 104
      Top = 180
      Width = 83
      Height = 13
      Caption = 'lblPartRemovable'
    end
    object Label10: TLabel
      Left = 12
      Top = 196
      Width = 41
      Height = 13
      Caption = 'Via SKF:'
    end
    object lblViaSKF: TLabel
      Left = 104
      Top = 196
      Width = 45
      Height = 13
      Caption = 'lblViaSKF'
    end
    object lblBFSStr: TLabel
      Left = 12
      Top = 212
      Width = 23
      Height = 13
      Caption = 'BFS:'
    end
    object lblBFS: TLabel
      Left = 104
      Top = 212
      Width = 30
      Height = 13
      Caption = 'lblBFS'
    end
    object reDescription: TRichEdit
      Left = 12
      Top = 292
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
  object pbIsDriverInstalled: TButton
    Left = 292
    Top = 12
    Width = 93
    Height = 25
    Caption = 'IsDriverInstalled'
    TabOrder = 19
    OnClick = pbIsDriverInstalledClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 360
    Width = 449
    Height = 161
    ScrollBars = ssBoth
    TabOrder = 20
    WordWrap = False
  end
  object pbMountPartitionsPrompted: TButton
    Left = 352
    Top = 204
    Width = 137
    Height = 25
    Caption = 'MountPartitionsPrompted'
    TabOrder = 21
    OnClick = pbMountPartitionsPromptedClick
  end
  object pbValidPartitions: TButton
    Left = 352
    Top = 232
    Width = 137
    Height = 25
    Caption = 'ValidPartitions'
    TabOrder = 22
    OnClick = pbValidPartitionsClick
  end
  object GroupBox1: TGroupBox
    Left = 80
    Top = 224
    Width = 253
    Height = 49
    Caption = 'NT Partitions Debug'
    TabOrder = 23
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 28
      Height = 13
      Caption = 'Drive:'
    end
    object seDriveNo: TSpinEdit64
      Left = 44
      Top = 16
      Width = 49
      Height = 22
      Increment = 1
      TabOrder = 0
    end
    object pbDEBUG_GetDriveInfo: TButton
      Left = 100
      Top = 16
      Width = 145
      Height = 25
      Caption = 'DEBUG_GetDriveInfo'
      TabOrder = 1
      OnClick = pbDEBUG_GetDriveInfoClick
    end
  end
  object ckSetEnableVolumeDeletion: TCheckBox
    Left = 8
    Top = 112
    Width = 145
    Height = 17
    Caption = 'SetEnableVolumeDeletion'
    TabOrder = 24
    OnClick = ckSetEnableVolumeDeletionClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'ScramDisk volumes (*.svl)|*.svl|Wave files (*.wav)|*.wav|All fil' +
      'es (*.*)|*.*'
    Left = 84
    Top = 292
  end
  object OTFEScramDisk1: TOTFEScramDisk
    Active = False
    RedScreenCaption = 'Enter Passwords'
    HidePasswordsEnteredCk = False
    PreferredDrive = 'S'
    Left = 56
    Top = 28
  end
end
