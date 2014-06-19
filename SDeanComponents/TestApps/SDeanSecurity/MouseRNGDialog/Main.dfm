object frmMain: TfrmMain
  Left = 218
  Top = 116
  BorderStyle = bsDialog
  Caption = 'This caption will be set automatically...'
  ClientHeight = 461
  ClientWidth = 426
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
  object Label1: TLabel
    Left = 109
    Top = 48
    Width = 119
    Height = 13
    Caption = 'Random bits to generate:'
  end
  object Label2: TLabel
    Left = 301
    Top = 48
    Width = 16
    Height = 13
    Caption = 'bits'
  end
  object Label3: TLabel
    Left = 8
    Top = 12
    Width = 380
    Height = 13
    Caption = 
      'This test application demonstrates the use of the TMouseRNGDialo' +
      'g component'
  end
  object pbCapture: TButton
    Left = 175
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Capture'
    Default = True
    TabOrder = 0
    OnClick = pbCaptureClick
  end
  object seRandomCount: TSpinEdit64
    Left = 233
    Top = 44
    Width = 61
    Height = 22
    Increment = 8
    TabOrder = 1
    Value = 128
  end
  object pbClear: TButton
    Left = 252
    Top = 428
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = pbClearClick
  end
  object pbCancel: TButton
    Left = 340
    Top = 428
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = pbCancelClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 116
    Width = 409
    Height = 301
    Color = clBtnFace
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
    TabOrder = 4
  end
  object MouseRNGDialog1: TMouseRNGDialog
    RequiredBits = 0
    Left = 316
    Top = 80
  end
end
