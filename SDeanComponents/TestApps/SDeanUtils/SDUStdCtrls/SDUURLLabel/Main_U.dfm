object frmMain: TfrmMain
  Left = 289
  Top = 307
  BorderStyle = bsDialog
  Caption = 'Form title set automatically...'
  ClientHeight = 160
  ClientWidth = 347
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
    Left = 8
    Top = 12
    Width = 39
    Height = 13
    Caption = '&Caption:'
    FocusControl = edCaption
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 25
    Height = 13
    Caption = '&URL:'
    FocusControl = edURL
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 68
    Width = 329
    Height = 45
    Caption = 'Test TSDUURLLabel'
    TabOrder = 2
    object SDUURLLabel: TSDUURLLabel
      Left = 8
      Top = 20
      Width = 71
      Height = 13
      Cursor = crHandPoint
      Caption = 'SDUURLLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      NormalColor = clBlue
      HoverColor = clRed
    end
  end
  object edURL: TEdit
    Left = 52
    Top = 32
    Width = 281
    Height = 21
    TabOrder = 1
    Text = 'edURL'
    OnChange = edChange
  end
  object edCaption: TEdit
    Left = 52
    Top = 8
    Width = 281
    Height = 21
    TabOrder = 0
    Text = 'edCaption'
    OnChange = edChange
  end
  object pbClose: TButton
    Left = 260
    Top = 124
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = pbCloseClick
  end
end
