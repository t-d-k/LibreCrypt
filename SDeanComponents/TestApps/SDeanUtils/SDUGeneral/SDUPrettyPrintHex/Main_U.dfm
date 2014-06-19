object Main_F: TMain_F
  Left = 192
  Top = 107
  Caption = 'SDUPrettyPrintHex Test'
  ClientHeight = 436
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    714
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 36
    Width = 45
    Height = 13
    Caption = '&Filename:'
    FocusControl = edFilename
  end
  object Label2: TLabel
    Left = 312
    Top = 36
    Width = 50
    Height = 13
    Caption = '&Bytes/line:'
    FocusControl = seWidth
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 475
    Height = 13
    Caption = 
      'Enter a filename and click "Display" to show a pretty-printed he' +
      'x representation of that file'#39's contents.'
  end
  object pbDisplayFile: TButton
    Left = 452
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Display'
    Default = True
    TabOrder = 1
    OnClick = pbDisplayFileClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 72
    Width = 697
    Height = 352
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object edFilename: TEdit
    Left = 60
    Top = 32
    Width = 217
    Height = 21
    TabOrder = 0
    Text = '.\testfile.dat'
  end
  object seWidth: TSpinEdit64
    Left = 368
    Top = 32
    Width = 45
    Height = 22
    Increment = 1
    MaxValue = 9999
    MinValue = 1
    TabOrder = 3
    Value = 16
  end
end
