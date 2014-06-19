object PKCS11LibrarySelectDialog: TPKCS11LibrarySelectDialog
  Left = 212
  Top = 137
  BorderStyle = bsDialog
  Caption = 'PKCS#11 Library Select'
  ClientHeight = 254
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 515
    Height = 27
    AutoSize = False
    Caption = 
      'The following PKCS#11 libraries were detected; please select whi' +
      'ch one should be used:'
    WordWrap = True
  end
  object pbOK: TButton
    Left = 183
    Top = 215
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 278
    Top = 215
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object lvLibraries: TSDListView
    Left = 10
    Top = 44
    Width = 515
    Height = 161
    Columns = <>
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = lvLibrariesSelectItem
  end
end
