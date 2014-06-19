object frmMain: TfrmMain
  Left = 247
  Top = 214
  Width = 640
  Height = 610
  Caption = 'MouseRNG Test App'
  Color = clBtnFace
  Constraints.MinHeight = 610
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 320
    Top = 380
    Width = 109
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Random data as bytes:'
    FocusControl = reBytes
  end
  object Label2: TLabel
    Left = 8
    Top = 380
    Width = 167
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Random data as bits in &CSV format:'
    FocusControl = reBitsCSV
  end
  object Label4: TLabel
    Left = 435
    Top = 96
    Width = 143
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'DEBUG: &Sample co-ordinates:'
    FocusControl = reReport
  end
  object Label1: TLabel
    Left = 8
    Top = 552
    Width = 113
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Random bits generated:'
  end
  object lblBitCount: TLabel
    Left = 128
    Top = 552
    Width = 36
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '<none>'
  end
  object reReport: TRichEdit
    Left = 435
    Top = 112
    Width = 185
    Height = 249
    Anchors = [akTop, akRight]
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object reBytes: TRichEdit
    Left = 320
    Top = 396
    Width = 297
    Height = 149
    Anchors = [akLeft, akBottom]
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object reBitsCSV: TRichEdit
    Left = 8
    Top = 396
    Width = 297
    Height = 149
    Anchors = [akLeft, akBottom]
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object pbWriteTextFile: TButton
    Left = 364
    Top = 552
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save &text...'
    TabOrder = 4
    OnClick = pbWriteTextFileClick
  end
  object pbWriteBinaryFile: TButton
    Left = 484
    Top = 552
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save &binary...'
    TabOrder = 5
    OnClick = pbWriteBinaryFileClick
  end
  object ckEnableRNG: TCheckBox
    Left = 8
    Top = 96
    Width = 97
    Height = 17
    Caption = '&Enable RNG'
    TabOrder = 1
    OnClick = ckEnableRNGClick
  end
  object RichEdit1: TRichEdit
    Left = 189
    Top = 4
    Width = 249
    Height = 85
    Color = clBtnFace
    Lines.Strings = (
      'Instructions:'
      ''
      '1) Enable the RNG'
      '2) Move the mouse over the RNG component'
      '3) Examine the random output.')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object MouseRNG1: TMouseRNG
    Left = 8
    Top = 116
    Width = 416
    Height = 244
    TrailLines = 5
    LineWidth = 5
    LineColor = clNavy
    Anchors = [akLeft, akTop, akRight, akBottom]
    UseDockManager = False
    Enabled = False
    ParentColor = False
    OnDEBUGSampleTakenEvent = MouseRNG1DEBUGSampleTakenEvent
    OnBitGenerated = MouseRNG1BitGenerated
    OnByteGenerated = MouseRNG1ByteGenerated
  end
  object SaveDialog1: TSDUSaveDialog
    PreserveCWD = False
    Left = 448
    Top = 552
  end
end
