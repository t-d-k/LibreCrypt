object frmSelectHashCypher: TfrmSelectHashCypher
  Left = 458
  Top = 376
  BorderStyle = bsDialog
  Caption = 'Hash/Cypher Combination'
  ClientHeight = 493
  ClientWidth = 544
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
  DesignSize = (
    544
    493)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 528
    Height = 30
    Caption = 
      'The Box can be decrypted with any of the following hash/cypher c' +
      'ombinations:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 432
    Width = 514
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Please select the combination you wish to use, and click "OK"'
    ExplicitTop = 421
    ExplicitWidth = 745
  end
  object Label3: TLabel
    Left = 16
    Top = 396
    Width = 520
    Height = 26
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Right-clicking on a hash/cypher combination to see more details.'
    WordWrap = True
    ExplicitTop = 385
    ExplicitWidth = 751
  end
  object sgCombinations: TStringGrid
    Left = 8
    Top = 44
    Width = 527
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    ExplicitWidth = 493
    ExplicitHeight = 229
    ColWidths = (
      64)
  end
  object pbOK: TButton
    Left = 174
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
    ExplicitTop = 340
  end
  object pbCancel: TButton
    Left = 262
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitTop = 340
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
