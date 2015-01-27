object frmAbout: TfrmAbout
  Left = 443
  Top = 498
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'About DoxBox'
  ClientHeight = 293
  ClientWidth = 435
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
    435
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBeta: TLabel
    Left = 8
    Top = 46
    Width = 33
    Height = 13
    Caption = 'BETA'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
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
    Left = 47
    Top = 46
    Width = 381
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'App version no. here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblDescription: TLabel
    Left = 47
    Top = 8
    Width = 370
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    Caption = 'DoxBox: Open-Source Transparent Encryption'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    GlowSize = 2
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object lblDriverVersion: TLabel
    Left = 8
    Top = 164
    Width = 419
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'The main DoxBox driver is either not installed, or not started'
    Transparent = True
    ExplicitTop = 157
    ExplicitWidth = 338
  end
  object lblAuthor: TLabel
    Left = 47
    Top = 65
    Width = 380
    Height = 16
    Caption = 'by Sarah Dean, additions by TDK'
    Transparent = True
    WordWrap = True
  end
  object SDUURLLabel1: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 145
    Width = 419
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'http://DoxBox.eu'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'http://doxbox.eu/'
    ExplicitTop = 138
    ExplicitWidth = 338
  end
  object lblTranslatorCredit: TLabel
    Left = 8
    Top = 88
    Width = 419
    Height = 51
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'TRANSLATOR CREDIT HERE'
    Transparent = True
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 202
    Width = 419
    Height = 39
    Hint = 
      'This software is based on FreeOTFE and/or FreeOTFE4PDA, the free' +
      ' disk encryption system for PCs and PDAs, available at <A HREF="' +
      'http://www.FreeOTFE.org/">www.FreeOTFE.org</A>'
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'This software is based on FreeOTFE and/or FreeOTFE4PDA, the free' +
      ' disk encryption system for PCs and PDAs, available at <A HREF="' +
      'http://www.FreeOTFE.org/">www.FreeOTFE.org</A>'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
    ExplicitTop = 157
    ExplicitWidth = 369
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 186
    Width = 413
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Icons based on the Nuvola icon set'
    ExplicitTop = 176
    ExplicitWidth = 338
  end
  object pbOK: TButton
    Left = 177
    Top = 247
    Width = 81
    Height = 30
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
