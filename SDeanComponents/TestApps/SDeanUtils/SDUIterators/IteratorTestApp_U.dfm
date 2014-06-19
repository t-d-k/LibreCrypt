object IteratorTestApp_F: TIteratorTestApp_F
  Left = 348
  Top = 208
  Width = 460
  Height = 550
  Caption = 'Dir & File Iterator Test Harness'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 460
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    452
    523)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 62
    Top = 12
    Width = 65
    Height = 13
    Caption = 'Start location:'
  end
  object edStartDir: TEdit
    Left = 138
    Top = 8
    Width = 229
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'c:\'
  end
  object GroupBox1: TGroupBox
    Left = 235
    Top = 40
    Width = 209
    Height = 157
    Anchors = [akTop, akRight]
    Caption = 'File Iterator'
    TabOrder = 1
    object pbFileIterator: TButton
      Left = 25
      Top = 116
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 0
      OnClick = pbFileIteratorClick
    end
    object pbCreateFileIterator: TButton
      Left = 52
      Top = 84
      Width = 105
      Height = 25
      Caption = 'Reset File Iterator'
      TabOrder = 1
      OnClick = pbCreateFileIteratorClick
    end
    object cbRecurseSubDirs: TCheckBox
      Left = 26
      Top = 20
      Width = 157
      Height = 17
      Caption = 'Recurse into subdirectories'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object pbCount: TButton
      Left = 109
      Top = 116
      Width = 75
      Height = 25
      Caption = '&Count'
      TabOrder = 3
      OnClick = pbCountClick
    end
    object ckIncludeDirNames: TCheckBox
      Left = 26
      Top = 40
      Width = 109
      Height = 17
      Caption = 'Include directories'
      TabOrder = 4
    end
    object ckOmitStartDirPrefix: TCheckBox
      Left = 26
      Top = 60
      Width = 109
      Height = 17
      Caption = 'Omit start directory prefix'
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 40
    Width = 209
    Height = 157
    Caption = 'Dir Iterator'
    TabOrder = 2
    object pbCreateDirIterator: TButton
      Left = 52
      Top = 44
      Width = 105
      Height = 25
      Caption = 'Reset Dir Iterator'
      TabOrder = 0
      OnClick = pbCreateDirIteratorClick
    end
    object pbDirIterator: TButton
      Left = 67
      Top = 76
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 1
      OnClick = pbDirIteratorClick
    end
    object ckReverseFormat: TCheckBox
      Left = 12
      Top = 20
      Width = 185
      Height = 17
      Caption = 'Dir iterator output in reverse format'
      TabOrder = 2
    end
  end
  object pbClear: TButton
    Left = 285
    Top = 490
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = pbClearClick
  end
  object pbExit: TButton
    Left = 369
    Top = 490
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'E&xit'
    TabOrder = 4
    OnClick = pbExitClick
  end
  object pbBrowse: TButton
    Left = 365
    Top = 8
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = pbBrowseClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 212
    Width = 437
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object SDUDirIterator1: TSDUDirIterator
    DirMask = '*.*'
    IncludeStartDir = False
    Left = 8
    Top = 84
  end
  object SDUFileIterator1: TSDUFileIterator
    Directory = '.'
    FileMask = '*.*'
    Sorted = False
    RecurseSubDirs = False
    OmitStartDirPrefix = False
    IncludeDirNames = False
    Left = 235
    Top = 124
  end
end
