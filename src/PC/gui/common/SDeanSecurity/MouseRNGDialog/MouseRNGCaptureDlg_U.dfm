object MouseRNGCaptureDlg: TMouseRNGCaptureDlg
  Left = 298
  Top = 214
  Caption = 'Random Data Generation'
  ClientHeight = 373
  ClientWidth = 594
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    594
    373)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStored: TLabel
    Left = 169
    Top = 295
    Width = 41
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStored'
  end
  object lblRequired: TLabel
    Left = 169
    Top = 315
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblRequired'
  end
  object lblInstructions: TLabel
    Left = 128
    Top = 8
    Width = 329
    Height = 26
    Caption = 
      'In order to generate sufficient random data, please waggle the m' +
      'ouse randomly over the area below until it changes color, then c' +
      'lick "OK":'
    WordWrap = True
  end
  object pbOK: TButton
    Left = 106
    Top = 339
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 194
    Top = 339
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = pbCancelClick
  end
  object MouseRNG: TMouseRNG
    Left = 8
    Top = 44
    Width = 576
    Height = 242
    TrailLines = 5
    LineWidth = 5
    LineColor = clNavy
    Anchors = [akLeft, akTop, akRight, akBottom]
    UseDockManager = False
    Enabled = False
    ParentColor = False
    OnByteGenerated = MouseRNGByteGenerated
  end
end
