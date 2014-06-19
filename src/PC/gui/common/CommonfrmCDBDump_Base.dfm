object frmCDBDump_Base: TfrmCDBDump_Base
  Left = 493
  Top = 263
  BorderStyle = bsDialog
  Caption = 'TITLE SET AUTOMATICALLY'
  ClientHeight = 342
  ClientWidth = 483
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
  object pbOK: TButton
    Left = 160
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 248
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 465
    Height = 213
    Caption = 'Volume/keyfile details'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 73
      Height = 13
      Caption = '&Volume/keyfile:'
      FocusControl = OTFEFreeOTFEVolumeSelect1
    end
    object Label2: TLabel
      Left = 8
      Top = 52
      Width = 49
      Height = 13
      Caption = '&Password:'
    end
    object OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect
      Left = 116
      Top = 20
      Width = 342
      Height = 21
      TabOrder = 0
      OnChange = OTFEFreeOTFEVolumeSelect1Change
      SelectFor = fndOpen
      AllowPartitionSelect = True
      DesignSize = (
        342
        21)
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 232
    Width = 465
    Height = 57
    Caption = 'Dump file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 59
      Height = 13
      Caption = '&Dump to file:'
      FocusControl = feDumpFilename
    end
    object feDumpFilename: TSDUFilenameEdit
      Left = 116
      Top = 20
      Width = 337
      Height = 21
      Constraints.MaxHeight = 21
      Constraints.MinHeight = 21
      TabOrder = 0
      TabStop = False
      FilenameEditType = fetSave
      FilterIndex = 0
      Filename = 'edFilename'
      OnChange = ControlChanged
      DesignSize = (
        337
        21)
    end
  end
end
