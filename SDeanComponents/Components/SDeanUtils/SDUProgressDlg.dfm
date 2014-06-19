object SDUProgressDialog: TSDUProgressDialog
  Left = 458
  Top = 332
  BorderIcons = [biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Progress'
  ClientHeight = 101
  ClientWidth = 383
  Color = clBtnFace
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
  object pnlProgressBar: TPanel
    Left = 0
    Top = 29
    Width = 383
    Height = 72
    Align = alClient
    Caption = 'pnlProgressBar'
    TabOrder = 0
    DesignSize = (
      383
      72)
    object lblEstTimeRemainText: TLabel
      Left = 12
      Top = 40
      Width = 119
      Height = 13
      Caption = 'Estimated time remaining:'
    end
    object lblEstTimeRemaining: TLabel
      Left = 140
      Top = 40
      Width = 98
      Height = 13
      Caption = 'lblEstTimeRemaining'
    end
    object pbCancel: TButton
      Left = 301
      Top = 37
      Width = 75
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      Default = True
      TabOrder = 0
      OnClick = pbCancelClick
    end
    object pnlProgressBarPlaceholder: TPanel
      Left = 5
      Top = 10
      Width = 371
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'pnlProgressBarPlaceholder'
      TabOrder = 1
      DesignSize = (
        371
        21)
      object pgbOverall: TProgressBar
        Left = 15
        Top = 5
        Width = 96
        Height = 11
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
      object pgbIndeterminate: TSDProgressBarIndeterminate
        Left = 260
        Top = 5
        Width = 91
        Height = 11
        TabOrder = 1
        Marquee = False
        MarqueeUpdate = 200
      end
    end
  end
  object pnlStatusText: TPanel
    Left = 0
    Top = 0
    Width = 383
    Height = 29
    Align = alTop
    Caption = 'pnlStatusText'
    TabOrder = 1
    object lblStatus: TSDUTruncatingLabel
      Left = 5
      Top = 8
      Width = 40
      Height = 13
      Caption = 'lblStatus'
    end
  end
end
