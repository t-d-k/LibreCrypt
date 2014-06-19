object frmSelectDirectoryDlg: TfrmSelectDirectoryDlg
  Left = 188
  Top = 81
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 395
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    343
    395)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 328
    Width = 227
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'To view any subfolders, click a plus sign above.'
  end
  object SDFilesystemTreeView1: TSDFilesystemTreeView
    Left = 16
    Top = 60
    Width = 309
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    ReadOnly = True
    ShowRoot = False
    TabOrder = 1
    OnChange = SDFilesystemTreeView1Change
    OnDblClick = SDFilesystemTreeView1DblClick
  end
  object pbOK: TButton
    Left = 168
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 252
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object reInstructions: TOTFEFreeOTFE_InstructionRichEdit
    Left = 16
    Top = 12
    Width = 309
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'reInstructions')
    TabOrder = 0
  end
end
