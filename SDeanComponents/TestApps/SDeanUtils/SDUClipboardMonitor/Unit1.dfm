object Form1: TForm1
  Left = 264
  Top = 158
  Caption = 'FORM CAPTION SET AUTOMATICALLY'
  ClientHeight = 433
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    554
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 351
    Height = 13
    Caption = 
      'As the contents of the clipboard change, the changes will be sho' +
      'wn below'
    WordWrap = True
  end
  object reReport: TRichEdit
    Left = 8
    Top = 32
    Width = 535
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'reReport')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object pbClose: TButton
    Left = 469
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 4
    OnClick = pbCloseClick
  end
  object pbClear: TButton
    Left = 8
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 2
    OnClick = pbClearClick
  end
  object ckStayOnTop: TCheckBox
    Left = 236
    Top = 412
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Stay on top'
    TabOrder = 5
    OnClick = ckStayOnTopClick
  end
  object ckMonitorClipboard: TCheckBox
    Left = 236
    Top = 392
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Monitor clipboard'
    TabOrder = 1
    OnClick = ckMonitorClipboardClick
  end
  object pbClearClipboard: TButton
    Left = 96
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Empty clipbrd'
    TabOrder = 3
    OnClick = pbClearClipboardClick
  end
  object SDUClipboardMonitor1: TSDUClipboardMonitor
    OnChanged = SDUClipboardMonitor1Changed
    Left = 252
    Top = 376
  end
end
