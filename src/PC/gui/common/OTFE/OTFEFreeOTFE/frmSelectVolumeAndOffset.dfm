object frmSelectVolumeFileAndOffset: TfrmSelectVolumeFileAndOffset
  Left = 348
  Top = 450
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 144
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
  object lblFileDescSrc: TLabel
    Left = 8
    Top = 44
    Width = 83
    Height = 13
    Caption = '&Container/keyfile:'
    FocusControl = OTFEFreeOTFEVolumeSelect1
  end
  object lblOffsetSrc: TLabel
    Left = 8
    Top = 76
    Width = 31
    Height = 13
    Caption = '&Offset:'
  end
  object lblInstructions: TLabel
    Left = 8
    Top = 8
    Width = 197
    Height = 13
    Caption = 'INSTRUCTIONS SET AUTOMATICALLY'
    WordWrap = True
  end
  object pbOK: TButton
    Left = 161
    Top = 108
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object pbCancel: TButton
    Left = 249
    Top = 108
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object se64UnitOffset: TSDUSpin64Unit_Storage
    Left = 136
    Top = 72
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
  end
  inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
    Left = 136
    Top = 40
    Width = 342
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 136
    ExplicitTop = 40
    ExplicitWidth = 342
    ExplicitHeight = 21
    DesignSize = (
      342
      21)
  end
end
