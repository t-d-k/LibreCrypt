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
    Width = 400
    Height = 13
    Caption = 
      'The Container can be decrypted with any of the following hash/cy' +
      'pher combinations:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 432
    Width = 294
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Please select the combination you wish to use, and click "OK"'
  end
  object Label3: TLabel
    Left = 16
    Top = 396
    Width = 305
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Right-clicking on a hash/cypher combination to see more details.'
    WordWrap = True
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
