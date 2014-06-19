object Form1: TForm1
  Left = 212
  Top = 134
  Width = 240
  Height = 308
  Caption = 'CAPTION SET AUTOMATICALLY'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    232
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 211
    Height = 26
    Caption = 
      'Pressing the multimedia keys should result in the key pressed be' +
      'ing displayed here:'
    WordWrap = True
  end
  object reReport: TRichEdit
    Left = 10
    Top = 50
    Width = 211
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'RichEdit1')
    TabOrder = 0
  end
  object pbClose: TButton
    Left = 144
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = pbCloseClick
  end
  object SDUMultimediaKeys1: TSDUMultimediaKeys
    Active = False
    OnMultimediaKeypress = SDUMultimediaKeys1MultimediaKeypress
    Left = 80
    Top = 100
  end
end
