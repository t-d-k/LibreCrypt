object Form1: TForm1
  Left = 212
  Top = 134
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 108
  ClientWidth = 328
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
  object Label1: TLabel
    Left = 10
    Top = 20
    Width = 62
    Height = 13
    Caption = 'Disk &number:'
    FocusControl = seDiskNumber
  end
  object Label2: TLabel
    Left = 10
    Top = 70
    Width = 31
    Height = 13
    Caption = '&Drive: '
    FocusControl = dcbDrive
  end
  object pbPropertiesDisk: TButton
    Left = 240
    Top = 15
    Width = 75
    Height = 25
    Caption = '&Properties...'
    TabOrder = 1
    OnClick = pbPropertiesDiskClick
  end
  object pbPropertiesPartition: TButton
    Left = 240
    Top = 60
    Width = 75
    Height = 25
    Caption = 'P&roperties...'
    TabOrder = 2
    OnClick = pbPropertiesPartitionClick
  end
  object seDiskNumber: TSpinEdit
    Left = 85
    Top = 15
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object dcbDrive: TDriveComboBox
    Left = 85
    Top = 65
    Width = 145
    Height = 19
    TabOrder = 3
  end
end
