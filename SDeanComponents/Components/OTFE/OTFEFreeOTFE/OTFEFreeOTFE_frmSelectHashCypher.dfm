object frmSelectHashCypher: TfrmSelectHashCypher
  Left = 458
  Top = 376
  BorderStyle = bsDialog
  Caption = 'Hash/Cypher Combination'
  ClientHeight = 377
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 119
    Top = 8
    Width = 258
    Height = 26
    Caption = 
      'It appears that the volume specified may be decrypted with any o' +
      'f the following hash/cypher combinations:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 109
    Top = 316
    Width = 294
    Height = 13
    Caption = 'Please select the combination you wish to use, and click "OK"'
  end
  object Label3: TLabel
    Left = 118
    Top = 280
    Width = 270
    Height = 26
    Caption = 
      'By rightclicking on a particular hash/cypher combination, you ma' +
      'y view further details on the selected combination.'
    WordWrap = True
  end
  object sgCombinations: TStringGrid
    Left = 8
    Top = 44
    Width = 493
    Height = 229
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    PopupMenu = miPopup
    TabOrder = 0
    OnClick = sgCombinationsClick
    OnDblClick = sgCombinationsDblClick
    OnDrawCell = sgCombinationsDrawCell
    ColWidths = (
      64)
  end
  object pbOK: TButton
    Left = 174
    Top = 340
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 262
    Top = 340
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object miPopup: TPopupMenu
    Left = 20
    Top = 292
    object miHashDetails: TMenuItem
      Caption = '&Hash details...'
      OnClick = miHashDetailsClick
    end
    object miCypherDetails: TMenuItem
      Caption = '&Cypher details...'
      OnClick = miCypherDetailsClick
    end
  end
end
