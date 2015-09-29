object frmSelectVolumeType: TfrmSelectVolumeType
  Left = 544
  Top = 406
  BorderStyle = bsDialog
  Caption = 'Container Type'
  ClientHeight = 158
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    181
    158)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 119
    Height = 26
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Please select the type of container:'
    WordWrap = True
  end
  object rgVolumeType: TRadioGroup
    Left = 8
    Top = 48
    Width = 161
    Height = 70
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Container Type'
    ItemIndex = 0
    Items.Strings = (
      'LibreCrypt'
      'Plain dm-crypt (Linux)')
    TabOrder = 0
  end
  object pbCancel: TButton
    Left = 98
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pbOK: TButton
    Left = 12
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
