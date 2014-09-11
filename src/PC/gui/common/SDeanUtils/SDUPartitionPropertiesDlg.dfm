object SDUPartitionPropertiesDialog: TSDUPartitionPropertiesDialog
  Left = 449
  Top = 267
  BorderStyle = bsDialog
  Caption = 'Partition Properties'
  ClientHeight = 292
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 65
    Width = 74
    Height = 13
    Caption = 'Starting offset:'
  end
  object Label4: TLabel
    Left = 15
    Top = 90
    Width = 126
    Height = 13
    Caption = 'Partition length (in bytes):'
  end
  object Label5: TLabel
    Left = 15
    Top = 140
    Width = 75
    Height = 13
    Caption = 'Hidden sectors:'
  end
  object Label6: TLabel
    Left = 15
    Top = 15
    Width = 83
    Height = 13
    Caption = 'Partition number:'
  end
  object Label7: TLabel
    Left = 15
    Top = 165
    Width = 69
    Height = 13
    Caption = 'Partition type:'
  end
  object Label9: TLabel
    Left = 15
    Top = 190
    Width = 29
    Height = 13
    Caption = 'Flags:'
  end
  object Label11: TLabel
    Left = 15
    Top = 40
    Width = 60
    Height = 13
    Caption = 'Mounted as:'
  end
  object Label8: TLabel
    Left = 15
    Top = 115
    Width = 152
    Height = 13
    Caption = 'Partition length (sensible units):'
  end
  object edStartingOffset: TEdit
    Left = 180
    Top = 60
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    Text = 'edStartingOffset'
  end
  object pbClose: TButton
    Left = 171
    Top = 255
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object edPartitionLengthBytes: TEdit
    Left = 180
    Top = 85
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'edPartitionLengthBytes'
  end
  object edHiddenSectors: TEdit
    Left = 180
    Top = 135
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = 'edHiddenSectors'
  end
  object edPartitionNumber: TEdit
    Left = 180
    Top = 10
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'edPartitionNumber'
  end
  object edPartitionType: TEdit
    Left = 180
    Top = 160
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'edPartitionType'
  end
  object edMountedAs: TEdit
    Left = 180
    Top = 35
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    Text = 'edMountedAs'
  end
  object edPartitionLengthUnits: TEdit
    Left = 180
    Top = 110
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    Text = 'edPartitionLengthUnits'
  end
end
