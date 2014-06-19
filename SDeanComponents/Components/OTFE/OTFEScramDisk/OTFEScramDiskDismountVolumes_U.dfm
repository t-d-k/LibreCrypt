object OTFEScramDiskDismountVolumes_F: TOTFEScramDiskDismountVolumes_F
  Left = 341
  Top = 334
  BorderStyle = bsDialog
  Caption = 'Dismount ScramDisk Volumes'
  ClientHeight = 173
  ClientWidth = 323
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
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 120
    Height = 13
    Caption = 'Drives currently &mounted:'
    FocusControl = lbDrivesMounted
  end
  object lbDrivesMounted: TListBox
    Left = 8
    Top = 24
    Width = 221
    Height = 117
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object pbDismount: TButton
    Left = 240
    Top = 36
    Width = 75
    Height = 25
    Caption = '&Dismount'
    Default = True
    TabOrder = 1
    OnClick = pbDismountClick
  end
  object pbCancel: TButton
    Left = 240
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ckDismountBrutal: TCheckBox
    Left = 8
    Top = 148
    Width = 97
    Height = 17
    Caption = 'Dismount &brutal'
    TabOrder = 3
  end
end
