object frmMain: TfrmMain
  Left = 388
  Top = 145
  Width = 750
  Height = 500
  Caption = 'APPLICATION TITLE SET AUTOMATICALLY'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    742
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 129
    Height = 13
    Caption = 'File containing test vectors:'
  end
  object pbTestHashes: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Test Hashes'
    Default = True
    TabOrder = 0
    OnClick = pbTestHashesClick
  end
  object mmoReport: TMemo
    Left = 8
    Top = 76
    Width = 725
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'mmoReport')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object edFilename: TEdit
    Left = 148
    Top = 8
    Width = 561
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'edFilename'
  end
  object pbBrowse: TButton
    Left = 712
    Top = 8
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = pbBrowseClick
  end
  object pbClose: TButton
    Left = 656
    Top = 440
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = pbCloseClick
  end
  object OpenDialog1: TOpenDialog
    Left = 516
    Top = 24
  end
end
