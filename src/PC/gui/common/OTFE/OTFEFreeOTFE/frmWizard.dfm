object frmWizard: TfrmWizard
  Left = 242
  Top = 157
  BorderStyle = bsDialog
  Caption = 'frmWizard'
  ClientHeight = 410
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRight: TPanel
    Left = 0
    Top = 0
    Width = 607
    Height = 410
    Align = alClient
    Caption = 'pnlRight'
    TabOrder = 0
    object bvlLine: TBevel
      Left = 1
      Top = 347
      Width = 605
      Height = 21
      Align = alBottom
      ExplicitWidth = 500
    end
    object pcWizard: TPageControl
      Left = 1
      Top = 1
      Width = 605
      Height = 346
      Align = alClient
      TabOrder = 0
    end
    object pnlButtons: TPanel
      Left = 1
      Top = 368
      Width = 605
      Height = 41
      Align = alBottom
      Caption = 'pnlButtons'
      TabOrder = 1
      DesignSize = (
        605
        41)
      object lblStage: TLabel
        Left = 8
        Top = 4
        Width = 117
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Stage 999 of 999'
      end
      object lblCompleteIndicator: TLabel
        Left = 116
        Top = 0
        Width = 98
        Height = 13
        Caption = 'lblCompleteIndicator'
      end
      object pbNext: TButton
        Left = 351
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Next >'
        TabOrder = 1
        OnClick = pbNextClick
      end
      object pbBack: TButton
        Left = 267
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '< &Back'
        TabOrder = 0
        OnClick = pbBackClick
      end
      object pbFinish: TButton
        Left = 435
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Finish'
        TabOrder = 2
        OnClick = pbFinishClick
      end
      object pbCancel: TButton
        Left = 519
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 3
      end
      object pbStage: TProgressBar
        Left = 8
        Top = 20
        Width = 117
        Height = 13
        Smooth = True
        TabOrder = 4
      end
    end
  end
end
