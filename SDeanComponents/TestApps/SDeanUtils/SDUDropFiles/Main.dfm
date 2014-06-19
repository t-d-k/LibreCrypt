object Form1: TForm1
  Left = 330
  Top = 168
  BorderStyle = bsDialog
  Caption = 'APPLICATION TITLE SET AUTOMATICALLY'
  ClientHeight = 455
  ClientWidth = 513
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
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 428
    Height = 26
    Caption = 
      'Drop files and/or directories onto any of the controls below (or' +
      ' the form) to demonstrate TSDUDropFiles component'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 12
    Top = 48
    Width = 126
    Height = 13
    Caption = 'TSDUDropFiles with panel:'
  end
  object Label3: TLabel
    Left = 248
    Top = 48
    Width = 144
    Height = 13
    Caption = 'TSDUDropFiles with TListView:'
  end
  object Label4: TLabel
    Left = 12
    Top = 300
    Width = 314
    Height = 13
    Caption = 
      'TSDUDropFiles with TRichEdit/results of dropping files/directori' +
      'es:'
  end
  object Label5: TLabel
    Left = 84
    Top = 240
    Width = 349
    Height = 13
    Caption = 
      'TSDUDropFiles with no component; defaults to owning control (thi' +
      's form)'
  end
  object Panel1: TPanel
    Left = 12
    Top = 68
    Width = 185
    Height = 149
    Caption = 'Panel1'
    TabOrder = 0
  end
  object ListView1: TListView
    Left = 248
    Top = 64
    Width = 250
    Height = 150
    Columns = <>
    TabOrder = 1
  end
  object RichEdit1: TRichEdit
    Left = 12
    Top = 316
    Width = 485
    Height = 89
    Lines.Strings = (
      'RichEdit1')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object pbClose: TButton
    Left = 424
    Top = 416
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 3
    OnClick = pbCloseClick
  end
  object SDUDropFiles1: TSDUDropFiles
    Active = False
    DropControl = Panel1
    OnFileDrop = SDUDropFilesFileDrop
    OnDirectoryDrop = SDUDropFilesDirectoryDrop
    Left = 148
    Top = 40
  end
  object SDUDropFiles2: TSDUDropFiles
    Active = False
    DropControl = ListView1
    OnFileDrop = SDUDropFilesFileDrop
    OnDirectoryDrop = SDUDropFilesDirectoryDrop
    Left = 408
    Top = 40
  end
  object SDUDropFiles3: TSDUDropFiles
    Active = False
    OnFileDrop = SDUDropFilesFileDrop
    OnDirectoryDrop = SDUDropFilesDirectoryDrop
    Left = 232
    Top = 260
  end
  object SDUDropFiles4: TSDUDropFiles
    Active = False
    DropControl = RichEdit1
    OnFileDrop = SDUDropFilesFileDrop
    OnDirectoryDrop = SDUDropFilesDirectoryDrop
    Left = 336
    Top = 288
  end
end
