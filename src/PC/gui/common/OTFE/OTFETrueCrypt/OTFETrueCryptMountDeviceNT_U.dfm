object OTFETrueCryptMountDeviceNT_F: TOTFETrueCryptMountDeviceNT_F
  Left = 541
  Top = 321
  BorderStyle = bsDialog
  Caption = 'Open Device'
  ClientHeight = 210
  ClientWidth = 198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    198
    210)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 68
    Height = 13
    Caption = '&Select device:'
    FocusControl = lbDevices
  end
  object pbOK: TButton
    Left = 20
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 104
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object lbDevices: TListBox
    Left = 8
    Top = 24
    Width = 181
    Height = 145
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = pbOKClick
  end
end
