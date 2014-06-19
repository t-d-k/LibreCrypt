object frmCDBBackupRestore: TfrmCDBBackupRestore
  Left = 294
  Top = 227
  BorderStyle = bsDialog
  Caption = 'frmCDBBackupRestore'
  ClientHeight = 233
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbCancel: TButton
    Left = 248
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object pbOK: TButton
    Left = 160
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object gbDest: TGroupBox
    Left = 8
    Top = 104
    Width = 465
    Height = 85
    Caption = 'DEST'
    TabOrder = 1
    object lblFileDescDest: TLabel
      Left = 8
      Top = 24
      Width = 54
      Height = 13
      Caption = 'DEST FILE'
      FocusControl = SelectDestFile
    end
    object lblOffsetDest: TLabel
      Left = 8
      Top = 52
      Width = 31
      Height = 13
      Caption = 'O&ffset:'
    end
    object se64UnitOffsetDest: TSDUSpin64Unit_Storage
      Left = 116
      Top = 48
      Width = 226
      Height = 29
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
      OnChange = ControlChange
    end
    object SelectDestFile: TOTFEFreeOTFEVolumeSelect
      Left = 116
      Top = 16
      Width = 342
      Height = 21
      TabOrder = 0
      OnChange = ControlChange
      SelectFor = fndOpen
      AllowPartitionSelect = True
      DesignSize = (
        342
        21)
    end
  end
  object gbSrc: TGroupBox
    Left = 8
    Top = 8
    Width = 465
    Height = 85
    Caption = 'SRC'
    TabOrder = 0
    object lblFileDescSrc: TLabel
      Left = 8
      Top = 24
      Width = 47
      Height = 13
      Caption = 'SRC FILE'
      FocusControl = SelectSrcFile
    end
    object lblOffsetSrc: TLabel
      Left = 8
      Top = 52
      Width = 31
      Height = 13
      Caption = '&Offset:'
    end
    object se64UnitOffsetSrc: TSDUSpin64Unit_Storage
      Left = 116
      Top = 48
      Width = 226
      Height = 29
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
      OnChange = ControlChange
    end
    object SelectSrcFile: TOTFEFreeOTFEVolumeSelect
      Left = 116
      Top = 20
      Width = 342
      Height = 21
      TabOrder = 0
      OnChange = ControlChange
      SelectFor = fndOpen
      AllowPartitionSelect = True
      DesignSize = (
        342
        21)
    end
  end
end
