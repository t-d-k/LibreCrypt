object frmNewDirDlg: TfrmNewDirDlg
  Left = 284
  Top = 332
  BorderStyle = bsDialog
  Caption = 'Folder Name'
  ClientHeight = 103
  ClientWidth = 270
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
    Left = 10
    Top = 10
    Width = 121
    Height = 13
    Caption = 'Enter name of new folder;'
  end
  object edDirName: TEdit
    Left = 10
    Top = 30
    Width = 246
    Height = 21
    TabOrder = 0
    Text = 'edDirName'
    OnChange = edDirNameChange
  end
  object pbOK: TButton
    Left = 52
    Top = 65
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pbCancel: TButton
    Left = 142
    Top = 65
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
