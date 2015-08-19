object frmDiskProperties: TfrmDiskProperties
  Left = 20
  Top = 387
  BorderStyle = bsDialog
  Caption = 'Disk Properties'
  ClientHeight = 307
  ClientWidth = 399
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
    Top = 140
    Width = 94
    Height = 13
    Caption = 'Tracks per cylinder:'
  end
  object Label4: TLabel
    Left = 15
    Top = 165
    Width = 86
    Height = 13
    Caption = 'Sectors per track:'
  end
  object Label5: TLabel
    Left = 15
    Top = 190
    Width = 83
    Height = 13
    Caption = 'Bytes per sector:'
  end
  object Label6: TLabel
    Left = 15
    Top = 90
    Width = 52
    Height = 13
    Caption = 'MediaType'
  end
  object Label7: TLabel
    Left = 15
    Top = 215
    Width = 72
    Height = 13
    Caption = 'Size (in bytes):'
  end
  object Label11: TLabel
    Left = 15
    Top = 115
    Width = 48
    Height = 13
    Caption = 'Cylinders:'
  end
  object Label8: TLabel
    Left = 15
    Top = 15
    Width = 62
    Height = 13
    Caption = 'Disk number:'
  end
  object Label10: TLabel
    Left = 15
    Top = 40
    Width = 71
    Height = 13
    Caption = 'Disk signature:'
  end
  object Label12: TLabel
    Left = 15
    Top = 65
    Width = 74
    Height = 13
    Caption = 'Partition count:'
  end
  object Label2: TLabel
    Left = 15
    Top = 240
    Width = 98
    Height = 13
    Caption = 'Size (sensible units):'
  end
  object edTracksPerCylinder: TEdit
    Left = 160
    Top = 135
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'edTracksPerCylinder'
  end
  object pbClose: TButton
    Left = 182
    Top = 270
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object edSectorsPerTrack: TEdit
    Left = 160
    Top = 160
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    Text = 'edSectorsPerTrack'
  end
  object edBytesPerSector: TEdit
    Left = 160
    Top = 185
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
    Text = 'edPartitionLength'
  end
  object edMediaType: TEdit
    Left = 160
    Top = 85
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'edMediaType'
  end
  object edSizeBytes: TEdit
    Left = 160
    Top = 210
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
    Text = 'edSizeBytes'
  end
  object edCylinders: TEdit
    Left = 160
    Top = 110
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = 'edCylinders'
  end
  object edDiskNumber: TEdit
    Left = 160
    Top = 10
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'edDiskNumber'
  end
  object edDiskSignature: TEdit
    Left = 160
    Top = 35
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    Text = 'edDiskSignature'
  end
  object edDiskPartitionCount: TEdit
    Left = 160
    Top = 60
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    Text = 'edDiskPartitionCount'
  end
  object edSizeUnits: TEdit
    Left = 160
    Top = 235
    Width = 225
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 10
    Text = 'edSizeUnits'
  end
end
