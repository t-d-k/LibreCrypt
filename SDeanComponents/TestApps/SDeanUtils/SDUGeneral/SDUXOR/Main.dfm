object Form1: TForm1
  Left = 424
  Top = 214
  Width = 415
  Height = 297
  Caption = 'TITLE SET AUTOMATICALLY'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    407
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 40
    Height = 13
    Caption = 'String A:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 40
    Height = 13
    Caption = 'String B:'
  end
  object edStringA: TEdit
    Left = 56
    Top = 12
    Width = 341
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edStringA'
  end
  object edStringB: TEdit
    Left = 56
    Top = 44
    Width = 341
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'edStringB'
  end
  object pbXOR: TButton
    Left = 164
    Top = 76
    Width = 75
    Height = 25
    Caption = 'XOR'
    Default = True
    TabOrder = 2
    OnClick = pbXORClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 112
    Width = 389
    Height = 117
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'reReport')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object pbClose: TButton
    Left = 324
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = pbCloseClick
  end
end
