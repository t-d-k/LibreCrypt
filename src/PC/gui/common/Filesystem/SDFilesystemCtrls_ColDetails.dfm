object SDFilesystemListView_ColDetails: TSDFilesystemListView_ColDetails
  Left = 417
  Top = 301
  BorderStyle = bsDialog
  Caption = 'Choose Details'
  ClientHeight = 416
  ClientWidth = 322
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
    Left = 12
    Top = 12
    Width = 175
    Height = 13
    Caption = 'Select the details you wish to display:'
  end
  object Label2: TLabel
    Left = 12
    Top = 44
    Width = 35
    Height = 13
    Caption = 'Details:'
  end
  object Label3: TLabel
    Left = 12
    Top = 344
    Width = 169
    Height = 13
    Caption = '&Width of selected column (in pixels):'
  end
  object pbMoveUp: TButton
    Left = 240
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Move &Up'
    TabOrder = 0
    OnClick = pbMoveUpClick
  end
  object pbMoveDown: TButton
    Left = 240
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Move &Down'
    TabOrder = 1
    OnClick = pbMoveDownClick
  end
  object pbShow: TButton
    Left = 240
    Top = 128
    Width = 75
    Height = 25
    Caption = '&Show'
    TabOrder = 2
    OnClick = pbShowClick
  end
  object pbHide: TButton
    Left = 240
    Top = 160
    Width = 75
    Height = 25
    Caption = '&Hide'
    TabOrder = 3
    OnClick = pbHideClick
  end
  object pbOK: TButton
    Left = 148
    Top = 384
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 232
    Top = 384
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object pnlSplitter: TPanel
    Left = 12
    Top = 372
    Width = 297
    Height = 13
    Caption = 'pnlSplitter'
    TabOrder = 5
  end
end
