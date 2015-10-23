object FileList_F: TfrmFileList
  Left = 284
  Top = 369
  Caption = 'Report'
  ClientHeight = 357
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    443
    357)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 8
    Top = 4
    Width = 210
    Height = 13
    Caption = 'This is a placeholder for the title of this report'
  end
  object pbOK: TButton
    Left = 360
    Top = 324
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object pnlPlaceholder: TPanel
    Left = 8
    Top = 24
    Width = 429
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlPlaceholder'
    TabOrder = 1
    object lbFiles: TListBox
      Left = 16
      Top = 16
      Width = 105
      Height = 85
      Color = clBtnFace
      ItemHeight = 13
      TabOrder = 0
      Visible = False
    end
    object reReport: TRichEdit
      Left = 164
      Top = 176
      Width = 185
      Height = 89
      Color = clBtnFace
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      Visible = False
    end
  end
end
