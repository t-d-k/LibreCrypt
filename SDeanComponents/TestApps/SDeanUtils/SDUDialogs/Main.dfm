object Form1: TForm1
  Left = 334
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 335
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 60
    Top = 16
    Width = 23
    Height = 13
    Caption = 'Title:'
  end
  object Label2: TLabel
    Left = 8
    Top = 44
    Width = 77
    Height = 13
    Caption = 'Main instruction:'
  end
  object Label3: TLabel
    Left = 44
    Top = 68
    Width = 40
    Height = 13
    Caption = 'Content:'
  end
  object edTitle: TEdit
    Left = 92
    Top = 12
    Width = 237
    Height = 21
    TabOrder = 0
    Text = 'edTitle'
  end
  object edMainInstruction: TEdit
    Left = 92
    Top = 40
    Width = 237
    Height = 21
    TabOrder = 1
    Text = 'edMainInstruction'
  end
  object edContent: TEdit
    Left = 92
    Top = 68
    Width = 237
    Height = 21
    TabOrder = 2
    Text = 'edContent'
  end
  object pbClose: TButton
    Left = 134
    Top = 293
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 5
    OnClick = pbCloseClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 111
    Width = 145
    Height = 162
    Caption = 'Standard API'
    TabOrder = 3
    object pbShowMessage: TButton
      Left = 12
      Top = 120
      Width = 109
      Height = 25
      Caption = 'ShowMessage'
      TabOrder = 2
      OnClick = pbShowMessageClick
    end
    object pbMessageDlg: TButton
      Left = 16
      Top = 72
      Width = 109
      Height = 25
      Caption = 'MessageDlg'
      TabOrder = 1
      OnClick = pbMessageDlgClick
    end
    object pbMessageBox: TButton
      Left = 16
      Top = 24
      Width = 109
      Height = 25
      Caption = 'MessageBox'
      TabOrder = 0
      OnClick = pbMessageBoxClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 176
    Top = 111
    Width = 145
    Height = 162
    Caption = 'SDU Extended'
    TabOrder = 4
    object pbSDUMessageBox: TButton
      Left = 16
      Top = 24
      Width = 109
      Height = 25
      Caption = 'SDUMessageBox'
      TabOrder = 0
      OnClick = pbSDUMessageBoxClick
    end
    object pbSDUVistaTaskDialog: TButton
      Left = 16
      Top = 120
      Width = 109
      Height = 25
      Caption = 'SDUVistaTaskDialog'
      TabOrder = 2
      OnClick = pbSDUVistaTaskDialogClick
    end
    object pbSDUMessageDlg: TButton
      Left = 16
      Top = 72
      Width = 109
      Height = 25
      Caption = 'SDUMessageDlg'
      TabOrder = 1
      OnClick = pbSDUMessageDlgClick
    end
  end
  object XPManifest1: TXPManifest
    Left = 4
    Top = 96
  end
end
