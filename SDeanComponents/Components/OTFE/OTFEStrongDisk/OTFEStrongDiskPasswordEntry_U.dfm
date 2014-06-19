object OTFEStrongDiskPasswordEntry_F: TOTFEStrongDiskPasswordEntry_F
  Left = 418
  Top = 210
  BorderStyle = bsDialog
  Caption = 'Enter password'
  ClientHeight = 191
  ClientWidth = 285
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
    Left = 8
    Top = 12
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = mePassword
  end
  object Label2: TLabel
    Left = 8
    Top = 108
    Width = 73
    Height = 13
    Caption = 'Mount as &drive:'
    FocusControl = cbDrives
  end
  object Label3: TLabel
    Left = 24
    Top = 40
    Width = 34
    Height = 13
    Caption = '&Keyfile:'
    FocusControl = edKeyfile
  end
  object mePassword: TMaskEdit
    Left = 64
    Top = 8
    Width = 213
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
    Text = 'mePassword'
  end
  object cbDrives: TComboBox
    Left = 84
    Top = 104
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object pbOK: TButton
    Left = 116
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object pbCancel: TButton
    Left = 200
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ckElectronicKey: TCheckBox
    Left = 64
    Top = 64
    Width = 97
    Height = 17
    Caption = '&Electronic key'
    TabOrder = 4
  end
  object edKeyfile: TEdit
    Left = 64
    Top = 36
    Width = 193
    Height = 21
    TabOrder = 5
    Text = 'edKeyfile'
  end
  object pbBrowse: TButton
    Left = 256
    Top = 36
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = pbBrowseClick
  end
  object ckAutostart: TCheckBox
    Left = 176
    Top = 108
    Width = 73
    Height = 17
    Caption = '&Autostart'
    TabOrder = 7
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'kcp'
    Filter = 'StrongDisk keyfiles (*.kcp)|*.kcp|All files|*.*'
    Left = 232
    Top = 52
  end
end
