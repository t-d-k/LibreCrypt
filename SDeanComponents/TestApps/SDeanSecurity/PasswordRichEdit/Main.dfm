object frmMain: TfrmMain
  Left = 250
  Top = 268
  Width = 766
  Height = 386
  Caption = 'Form caption set automatically...'
  Color = clBtnFace
  Constraints.MinHeight = 386
  Constraints.MinWidth = 766
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
    Left = 10
    Top = 12
    Width = 73
    Height = 13
    Caption = '&Password char:'
    FocusControl = edPasswordChar
  end
  object Label2: TLabel
    Left = 118
    Top = 12
    Width = 94
    Height = 13
    Caption = '(Delete to set to #0)'
  end
  object edPasswordChar: TEdit
    Left = 90
    Top = 8
    Width = 21
    Height = 21
    MaxLength = 1
    TabOrder = 0
    Text = 'edPasswordChar'
    OnChange = edPasswordCharChange
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 44
    Width = 365
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Test Area'
    TabOrder = 1
    object Label3: TLabel
      Left = 12
      Top = 20
      Width = 74
      Height = 13
      Caption = '&Standard TEdit:'
      FocusControl = edTest
    end
    object Label4: TLabel
      Left = 12
      Top = 72
      Width = 170
      Height = 13
      Caption = 'S&DeanSecurity TPasswordRichEdit:'
      FocusControl = preTest
    end
    object edTest: TEdit
      Left = 12
      Top = 36
      Width = 341
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'edTest'
      OnChange = edTestChange
    end
    object preTest: TPasswordRichEdit
      Left = 12
      Top = 88
      Width = 341
      Height = 165
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'preTest')
      ScrollBars = ssBoth
      TabOrder = 1
      OnChange = preTestChange
    end
  end
  object pbClose: TButton
    Left = 673
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = pbCloseClick
  end
  object GroupBox2: TGroupBox
    Left = 384
    Top = 44
    Width = 365
    Height = 265
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Hex representation of input'
    TabOrder = 2
    object Label5: TLabel
      Left = 12
      Top = 132
      Width = 170
      Height = 13
      Caption = 'SDeanSecurity T&PasswordRichEdit:'
      FocusControl = reReportTPasswordRichEdit
    end
    object Label6: TLabel
      Left = 12
      Top = 20
      Width = 74
      Height = 13
      Caption = 'Standard T&Edit:'
      FocusControl = reReportTEdit
    end
    object reReportTEdit: TRichEdit
      Left = 12
      Top = 36
      Width = 341
      Height = 85
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Terminal'
      Font.Style = []
      Lines.Strings = (
        'reReportTEdit')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object reReportTPasswordRichEdit: TRichEdit
      Left = 12
      Top = 148
      Width = 341
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Terminal'
      Font.Style = []
      Lines.Strings = (
        'reReportTPasswordRichEdit')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
end
