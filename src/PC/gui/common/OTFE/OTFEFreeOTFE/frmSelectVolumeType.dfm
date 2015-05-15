object frmSelectVolumeType: TfrmSelectVolumeType
  Left = 544
  Top = 406
  BorderStyle = bsDialog
  Caption = 'Container Type'
  ClientHeight = 150
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 157
    Height = 29
    AutoSize = False
    Caption = 'Please select the type of container:'
    WordWrap = True
  end
  object rgVolumeType: TRadioGroup
    Left = 44
    Top = 40
    Width = 93
    Height = 57
    Caption = 'Container Type'
    ItemIndex = 0
    Items.Strings = (
      'LibreCrypt'
      'Linux')
    TabOrder = 0
  end
  object pbCancel: TButton
    Left = 96
    Top = 116
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pbOK: TButton
    Left = 8
    Top = 116
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
