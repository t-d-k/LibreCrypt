object frmVersionCheck: TfrmVersionCheck
  Left = 451
  Top = 343
  BorderStyle = bsDialog
  Caption = 'Update check..'
  ClientHeight = 141
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 26
    Top = 11
    Width = 60
    Height = 13
    Caption = 'This version:'
  end
  object Label3: TLabel
    Left = 26
    Top = 32
    Width = 69
    Height = 13
    Caption = 'Latest version:'
  end
  object lblVersionCurrent: TLabel
    Left = 107
    Top = 11
    Width = 97
    Height = 13
    Caption = 'lblVersionCurrent'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SDUURLLabel1: TSDUURLLabel
    Left = 20
    Top = 53
    Width = 191
    Height = 13
    Cursor = crHandPoint
    Caption = 'Click here to download the latest version'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    NormalColor = clBlue
    HoverColor = clRed
  end
  object lblVersionLatest: TLabel
    Left = 107
    Top = 32
    Width = 91
    Height = 13
    Caption = 'lblVersionLatest'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object pbClose: TButton
    Left = 78
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = pbCloseClick
  end
  object ckSuppressNotifyingThisVersion: TCheckBox
    Left = 13
    Top = 74
    Width = 206
    Height = 17
    Caption = 'Do not notify about this version again.'
    TabOrder = 1
  end
end
