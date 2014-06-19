object frmAbout: TfrmAbout
  Left = 443
  Top = 498
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 182
  ClientWidth = 319
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
  object lblBeta: TLabel
    Left = 31
    Top = 40
    Width = 257
    Height = 111
    Caption = 'BETA'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -96
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object imgIcon: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
    Stretch = True
  end
  object lblAppID: TLabel
    Left = 148
    Top = 8
    Width = 98
    Height = 13
    Caption = 'App version no. here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblDescription: TLabel
    Left = 60
    Top = 60
    Width = 163
    Height = 13
    Caption = 'CAPTION AUTOMATICALLY SET'
    Transparent = True
    WordWrap = True
  end
  object lblTitle: TLabel
    Left = 56
    Top = 4
    Width = 84
    Height = 20
    Caption = 'FreeOTFE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblDriverVersion: TLabel
    Left = 75
    Top = 124
    Width = 60
    Height = 13
    Caption = 'v99.99.9999'
    Transparent = True
  end
  object lblAuthor: TLabel
    Left = 56
    Top = 24
    Width = 71
    Height = 13
    Caption = 'by Sarah Dean'
    Transparent = True
    WordWrap = True
  end
  object Label3: TLabel
    Left = 144
    Top = 120
    Width = 106
    Height = 26
    Caption = '<<< This item will be automatically centered'
    Color = clRed
    ParentColor = False
    Visible = False
    WordWrap = True
  end
  object SDUURLLabel1: TSDUURLLabel
    Left = 95
    Top = 92
    Width = 130
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://www.FreeOTFE.org/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'http://www.FreeOTFE.org/'
  end
  object lblTranslatorCredit: TLabel
    Left = 56
    Top = 40
    Width = 149
    Height = 13
    Caption = 'TRANSLATOR CREDIT HERE'
    Transparent = True
    WordWrap = True
  end
  object pbOK: TButton
    Left = 123
    Top = 148
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object pnlDividerUpper: TPanel
    Left = 57
    Top = 80
    Width = 205
    Height = 2
    BevelOuter = bvLowered
    Caption = 'pnlDividerUpper'
    TabOrder = 1
  end
  object pnlDividerLower: TPanel
    Left = 57
    Top = 112
    Width = 205
    Height = 2
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 2
  end
end
