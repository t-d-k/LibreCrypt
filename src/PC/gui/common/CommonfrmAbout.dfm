object frmAbout: TfrmAbout
  Left = 443
  Top = 498
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'About DoxBox'
  ClientHeight = 315
  ClientWidth = 470
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
    470
    315)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBeta: TLabel
    Left = 8
    Top = 46
    Width = 33
    Height = 16
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
    Width = 405
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
    Width = 405
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
    ExplicitWidth = 370
  end
  object lblDriverVersion: TLabel
    Left = 8
    Top = 84
    Width = 454
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'The main DoxBox driver is either not installed, or not started'
    Transparent = True
  end
  object lblAuthor: TLabel
    Left = 47
    Top = 65
    Width = 415
    Height = 13
    Caption = 'Copyright Sarah Dean, additions by TDK'
    Transparent = True
    WordWrap = True
  end
  object SDUURLLabel1: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 133
    Width = 454
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Click here to visit the forums'
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
    ExplicitTop = 201
    ExplicitWidth = 466
  end
  object lblTranslatorCredit: TLabel
    Left = 8
    Top = 106
    Width = 454
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'No translations active (Default is English) '
    Transparent = True
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 224
    Width = 454
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
  object SDUURLLabel2: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 169
    Width = 454
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Icons are based on the Nuvola icon set'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'http://www.icon-king.com/projects/nuvola/'
    ExplicitTop = 246
    ExplicitWidth = 451
  end
  object SDUURLLabel3: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 187
    Width = 454
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'DoxBox uses the FastMM4 memory manager'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'http://sourceforge.net/projects/fastmm/'
    ExplicitTop = 264
    ExplicitWidth = 451
  end
  object SDUURLLabel4: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 205
    Width = 454
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'DoxBox uses Peter Gutmann'#39's cryptlib library, click here for com' +
      'plete source code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'https://www.cs.auckland.ac.nz/~pgut001/cryptlib/index.html'
    ExplicitTop = 247
    ExplicitWidth = 447
  end
  object SDUURLLabel5: TSDUURLLabel
    AlignWithMargins = True
    Left = 8
    Top = 151
    Width = 454
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'The source to DoxBox is available from GitHub.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    NormalColor = clBlue
    HoverColor = clRed
    URL = 'https://github.com/t-d-k/doxbox'
    ExplicitTop = 228
    ExplicitWidth = 451
  end
  object pbOK: TButton
    Left = 194
    Top = 269
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
