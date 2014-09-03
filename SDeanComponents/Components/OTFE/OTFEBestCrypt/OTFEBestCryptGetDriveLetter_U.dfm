object GetDriveLetter_F: TGetDriveLetter_F
  Left = 363
  Top = 347
  BorderStyle = bsDialog
  Caption = 'Mount As Drive...'
  ClientHeight = 142
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 33
    Height = 13
    Caption = 'Mount:'
  end
  object Label2: TLabel
    Left = 12
    Top = 36
    Width = 41
    Height = 13
    Caption = 'As &drive:'
    FocusControl = cbDriveLetter
  end
  object lblVolFilename: TLabel
    Left = 64
    Top = 12
    Width = 189
    Height = 13
    AutoSize = False
    Caption = 'lblVolFilename'
  end
  object Label3: TLabel
    Left = 240
    Top = 28
    Width = 49
    Height = 13
    Caption = 'Password:'
    Visible = False
  end
  object Label4: TLabel
    Left = 164
    Top = 28
    Width = 82
    Height = 26
    Caption = 'This bit is under development -->>'
    Color = clRed
    ParentColor = False
    Visible = False
    WordWrap = True
  end
  object cbDriveLetter: TComboBox
    Left = 64
    Top = 32
    Width = 45
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object pbOK: TButton
    Left = 100
    Top = 108
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pbCancel: TButton
    Left = 180
    Top = 108
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbAlwaysThisDrive: TCheckBox
    Left = 12
    Top = 60
    Width = 205
    Height = 17
    Caption = '&Always mount this volume as this drive'
    TabOrder = 3
    OnClick = cbAlwaysThisDriveClick
  end
  object cbAutomount: TCheckBox
    Left = 12
    Top = 80
    Width = 125
    Height = 17
    Caption = 'Automount this drive'
    Enabled = False
    TabOrder = 4
  end
  object MaskEdit1: TMaskEdit
    Left = 244
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'MaskEdit1'
    Visible = False
  end
end
