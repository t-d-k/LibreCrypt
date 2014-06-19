object SDUAboutDialog: TSDUAboutDialog
  Left = 602
  Top = 341
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'TITLE SET AUTOMATICALLY'
  ClientHeight = 224
  ClientWidth = 319
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
  DesignSize = (
    319
    224)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBeta: TLabel
    Left = 23
    Top = -16
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
  object imIcon: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
    Stretch = True
  end
  object lblVersion: TLabel
    Left = 56
    Top = 28
    Width = 152
    Height = 13
    Caption = 'APP VERSION ID GOES HERE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblTitle: TLabel
    Left = 56
    Top = 4
    Width = 199
    Height = 20
    Caption = 'APP TITLE GOES HERE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lblAuthor: TLabel
    Left = 56
    Top = 44
    Width = 146
    Height = 13
    Caption = 'AUTHOR NAME GOES HERE'
    Transparent = True
    WordWrap = True
  end
  object lblURL: TSDUURLLabel
    Left = 6
    Top = 158
    Width = 163
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'CAPTION SET AUTOMATICALLY'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'URL SET AUTOMATICALLY'
  end
  object pbOK: TButton
    Left = 123
    Top = 190
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object pnlSeparatorUpper: TPanel
    Left = 57
    Top = 146
    Width = 205
    Height = 2
    Anchors = [akLeft, akBottom]
    BevelOuter = bvLowered
    Caption = 'pnlSeparatorUpper'
    TabOrder = 1
  end
  object pnlSeparatorLower: TPanel
    Left = 57
    Top = 178
    Width = 205
    Height = 2
    Anchors = [akLeft, akBottom]
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 2
  end
  object reBlub: TRichEdit
    Left = 29
    Top = 76
    Width = 261
    Height = 59
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'BLURB GOES HERE'
      ''
      'NOTE: ENSURE BLURB CAN FIT IN THIS'
      'CONTROL WITHOUT THE VERTICAL SCROLLBAR')
    TabOrder = 3
  end
end
