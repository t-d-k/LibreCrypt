object frmMain: TfrmMain
  Left = 380
  Top = 288
  Width = 389
  Height = 490
  Caption = 'CAPTION SET AUTOMATICALLY'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    381
    463)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPermutations: TLabel
    Left = 85
    Top = 30
    Width = 71
    Height = 13
    Caption = 'lblPermutations'
  end
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 72
    Height = 13
    Caption = '&Character pool:'
    FocusControl = edPool
  end
  object pbPermutate: TButton
    Left = 153
    Top = 50
    Width = 75
    Height = 25
    Caption = '&Permutate'
    TabOrder = 0
    OnClick = pbPermutateClick
  end
  object reReport: TRichEdit
    Left = 10
    Top = 85
    Width = 361
    Height = 336
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    Lines.Strings = (
      'reReport')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object edPool: TEdit
    Left = 85
    Top = 5
    Width = 286
    Height = 21
    TabOrder = 2
    Text = 'abc'
    OnChange = edPoolChange
  end
  object pbSaveAs: TButton
    Left = 295
    Top = 430
    Width = 75
    Height = 25
    Caption = 'Save as...'
    TabOrder = 3
    OnClick = pbSaveAsClick
  end
  object SaveDialog1: TSDUSaveDialog
    Left = 250
    Top = 425
  end
end
