object frmMain: TfrmMain
  Left = 353
  Top = 211
  Width = 415
  Height = 350
  Caption = 'This caption set automatically...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbGenerate: TButton
    Left = 8
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Generate'
    Default = True
    TabOrder = 0
    OnClick = pbGenerateClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 8
    Width = 389
    Height = 274
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Pitch = fpFixed
    Font.Style = []
    Lines.Strings = (
      'reReport')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object pbClose: TButton
    Left = 324
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = pbCloseClick
  end
  object pbClear: TButton
    Left = 92
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = pbClearClick
  end
end
