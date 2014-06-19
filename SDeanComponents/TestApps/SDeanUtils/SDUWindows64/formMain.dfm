object frmMain: TfrmMain
  Left = 212
  Top = 137
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 464
  ClientWidth = 514
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
    Left = 88
    Top = 16
    Width = 337
    Height = 13
    Caption = 'IMPORTANT: RUN THIS SOFTWARE WITH ADMIN PRIVS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 26
    Top = 40
    Width = 461
    Height = 13
    Caption = 
      'i.e. Rightclick the executable as select "Run as administrator" ' +
      '(even if logged in as an admin user)'
  end
  object Label3: TLabel
    Left = 157
    Top = 60
    Width = 199
    Height = 13
    Caption = 'All operations will fail if executed otherwise'
  end
  object Label4: TLabel
    Left = 240
    Top = 176
    Width = 264
    Height = 32
    Caption = 
      '"Copy file" will place a copy of this executable into the Window' +
      's\System - or \Windows\SysWOW64 - dir'
    WordWrap = True
  end
  object pbCopyFile: TButton
    Left = 332
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Copy file'
    TabOrder = 1
    OnClick = pbCopyFileClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 244
    Width = 496
    Height = 171
    Lines.Strings = (
      'reReport')
    TabOrder = 2
  end
  object pbClose: TButton
    Left = 429
    Top = 428
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
  end
  object pbClear: TButton
    Left = 8
    Top = 428
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 88
    Width = 217
    Height = 145
    Caption = 'WOW64 Filesystem Redirection'
    TabOrder = 0
    object pbWow64EnableWow64FsRedirection: TButton
      Left = 16
      Top = 100
      Width = 183
      Height = 25
      Caption = 'Wow64EnableWow64FsRedirection'
      TabOrder = 2
      OnClick = pbWow64EnableWow64FsRedirectionClick
    end
    object pbWow64RevertWow64FsRedirection: TButton
      Left = 16
      Top = 64
      Width = 183
      Height = 25
      Caption = 'Wow64RevertWow64FsRedirection'
      TabOrder = 1
      OnClick = pbWow64RevertWow64FsRedirectionClick
    end
    object pbWow64DisableWow64FsRedirection: TButton
      Left = 16
      Top = 25
      Width = 183
      Height = 25
      Caption = 'Wow64DisableWow64FsRedirection'
      TabOrder = 0
      OnClick = pbWow64DisableWow64FsRedirectionClick
    end
  end
end
