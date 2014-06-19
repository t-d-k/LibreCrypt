object SDFATBootSectorPropertiesDialog: TSDFATBootSectorPropertiesDialog
  Left = 278
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Boot Sector Properties'
  ClientHeight = 400
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 268
    Width = 84
    Height = 13
    Caption = 'Sectors per track:'
  end
  object Label2: TLabel
    Left = 16
    Top = 296
    Width = 84
    Height = 13
    Caption = 'Number of heads:'
  end
  object Label3: TLabel
    Left = 16
    Top = 240
    Width = 80
    Height = 13
    Caption = 'Sectors per FAT:'
  end
  object Label4: TLabel
    Left = 16
    Top = 212
    Width = 81
    Height = 13
    Caption = 'Media descriptor:'
  end
  object Label5: TLabel
    Left = 16
    Top = 16
    Width = 56
    Height = 13
    Caption = 'OEM name:'
  end
  object Label6: TLabel
    Left = 16
    Top = 100
    Width = 111
    Height = 13
    Caption = 'Reserved sector count:'
  end
  object Label7: TLabel
    Left = 16
    Top = 184
    Width = 64
    Height = 13
    Caption = 'Total sectors:'
  end
  object Label8: TLabel
    Left = 16
    Top = 44
    Width = 79
    Height = 13
    Caption = 'Bytes per sector:'
  end
  object Label9: TLabel
    Left = 16
    Top = 128
    Width = 80
    Height = 13
    Caption = 'Number of FATs:'
  end
  object Label10: TLabel
    Left = 16
    Top = 72
    Width = 91
    Height = 13
    Caption = 'Sectors per cluster:'
  end
  object Label11: TLabel
    Left = 16
    Top = 156
    Width = 152
    Height = 13
    Caption = 'Maximum number of root entries:'
  end
  object Label12: TLabel
    Left = 16
    Top = 324
    Width = 74
    Height = 13
    Caption = 'Hidden sectors:'
  end
  object edOEMName: TEdit
    Left = 212
    Top = 12
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'edOEMName'
  end
  object edBytesPerSector: TEdit
    Left = 212
    Top = 40
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    Text = 'edBytesPerSector'
  end
  object edSectorsPerCluster: TEdit
    Left = 212
    Top = 68
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    Text = 'edSectorsPerCluster'
  end
  object edReservedSectorCount: TEdit
    Left = 212
    Top = 96
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'edReservedSectorCount'
  end
  object edFATCount: TEdit
    Left = 212
    Top = 124
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = 'edFATCount'
  end
  object edMaxRootEntries: TEdit
    Left = 212
    Top = 152
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'edMaxRootEntries'
  end
  object edTotalSectors: TEdit
    Left = 212
    Top = 180
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    Text = 'edTotalSectors'
  end
  object edMediaDescriptor: TEdit
    Left = 212
    Top = 208
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
    Text = 'edMediaDescriptor'
  end
  object edSectorsPerFAT: TEdit
    Left = 212
    Top = 236
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
    Text = 'edSectorsPerFAT'
  end
  object edSectorsPerTrack: TEdit
    Left = 212
    Top = 264
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
    Text = 'edSectorsPerTrack'
  end
  object edNumberOfHeads: TEdit
    Left = 212
    Top = 292
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 10
    Text = 'edNumberOfHeads'
  end
  object edHiddenSectors: TEdit
    Left = 212
    Top = 320
    Width = 185
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
    Text = 'edHiddenSectors'
  end
  object pbClose: TButton
    Left = 170
    Top = 360
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 12
    OnClick = pbCloseClick
  end
end
