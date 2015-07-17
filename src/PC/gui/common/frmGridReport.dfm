object frmGridReport: TfrmGridReport
  Left = 327
  Top = 229
  Caption = 'FORM CAPTION AUTOMATICALLY SET'
  ClientHeight = 422
  ClientWidth = 659
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 667
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    659
    422)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 12
    Top = 8
    Width = 184
    Height = 13
    Caption = 'LABEL CAPTION AUTOMATICALLY SET'
  end
  object pbSave: TButton
    Left = 12
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save...'
    TabOrder = 2
    OnClick = pbSaveClick
  end
  object pbClose: TButton
    Left = 573
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 4
    OnClick = pbCloseClick
  end
  object pnlBetweenButtons: TPanel
    Left = 181
    Top = 377
    Width = 387
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'pnlBetweenButtons'
    TabOrder = 1
    DesignSize = (
      387
      41)
    object pnlBetweenButtonsCenter: TPanel
      Left = 16
      Top = 0
      Width = 349
      Height = 41
      Anchors = [akLeft, akBottom]
      Caption = 'pnlBetweenButtonsCenter'
      TabOrder = 0
      DesignSize = (
        349
        41)
      object lblTotalDrivers: TLabel
        Left = 6
        Top = 4
        Width = 78
        Height = 13
        Caption = 'TOTAL DRIVERS'
      end
      object lblTotalAlgorithms: TLabel
        Left = 6
        Top = 20
        Width = 100
        Height = 13
        Caption = 'TOTAL ALGORITHMS'
      end
      object ckShowAdvanced: TCheckBox
        Left = 184
        Top = 12
        Width = 153
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Show &advanced'
        TabOrder = 0
        OnClick = ckShowAdvancedClick
      end
    end
  end
  object lvReport: TListView
    Left = 12
    Top = 27
    Width = 636
    Height = 342
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    FullDrag = True
    PopupMenu = mnuPopup
    TabOrder = 0
    OnColumnClick = lvReportColumnClick
    OnCompare = lvReportCompare
  end
  object pbDrivers: TButton
    Left = 100
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Drivers...'
    TabOrder = 3
    OnClick = pbDriversClick
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 384
    Top = 8
    object actSelectAll: TAction
      Caption = 'Select &all'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actCopy: TAction
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
  end
  object mnuPopup: TPopupMenu
    Left = 348
    Top = 8
    object Selectall1: TMenuItem
      Action = actSelectAll
    end
    object Copy1: TMenuItem
      Action = actCopy
    end
  end
  object SaveDialog: TSDUSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    PreserveCWD = False
    Left = 92
    Top = 312
  end
end
