object OTFEE4MScramDiskPasswordEntry_F: TOTFEE4MScramDiskPasswordEntry_F
  Left = 311
  Top = 213
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Password Entry'
  ClientHeight = 179
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000010000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000999000000000000000000
    0000000000099900000000000000000000000000000999000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000FFFFFFFFFFFF00999000000000000000FFF
    FFFFFFFFF00999000000000000000FFFFFFFFFFFF00999000000000000000000
    0000000000099900800000000000088888888888800999000800000000000000
    0000000000099900008000000000000000000000000999000008000000000000
    8777777770099990000080000000000008777777700999990000080000000000
    0082222220009999900000800000000000082222200009999900000800000000
    0000822222000099999000008000000000000822222000099999008008000000
    0000000000880000999900080080000000000000000000000999000080000000
    0000099900000000099900000000000000000999000000000999000000000000
    0000099900000000099900000000000000000999000000000999000000000000
    0000099990000000999900000000000000000999990000099999000000000000
    0000009999999999999000000000000000000009999999999900000000000000
    000000009999999990000000000000000000000000000000000000000000FFFF
    FFFFFFFE3FFFFFFE3FFFFFFE3FFFFFFFFFFFFFFFFFFF0007FFFF00063FFF0006
    37FF000633FF000631FF000630FF0006387F80063C3FC0061E1FE0060F0FF007
    0787F80783C3FC03C1C1FE01E0C0FFFCF0C0FFFC78C0FF8C38C0FF8C18C3FF8E
    38C3FF8F78CFFF87F0FFFF83E0FFFFC001FFFFE003FFFFF007FFFFFFFFFF}
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Password &1:'
    FocusControl = mePassword1
  end
  object Label2: TLabel
    Left = 20
    Top = 32
    Width = 58
    Height = 13
    Caption = 'Password &2:'
    FocusControl = mePassword2
  end
  object Label3: TLabel
    Left = 20
    Top = 56
    Width = 58
    Height = 13
    Caption = 'Password &3:'
    FocusControl = mePassword3
  end
  object Label4: TLabel
    Left = 20
    Top = 80
    Width = 58
    Height = 13
    Caption = 'Password &4:'
    FocusControl = mePassword4
  end
  object Label5: TLabel
    Left = 8
    Top = 112
    Width = 73
    Height = 13
    Caption = 'Mount as &drive:'
    FocusControl = cbDrives
  end
  object pbOK: TButton
    Left = 152
    Top = 144
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object pbCancel: TButton
    Left = 232
    Top = 144
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object mePassword1: TMaskEdit
    Left = 84
    Top = 8
    Width = 185
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object mePassword3: TMaskEdit
    Left = 84
    Top = 56
    Width = 185
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object mePassword2: TMaskEdit
    Left = 84
    Top = 32
    Width = 185
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object mePassword4: TMaskEdit
    Left = 84
    Top = 80
    Width = 185
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object pbKeydisk: TButton
    Left = 44
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Use &Keyfile...'
    TabOrder = 8
    OnClick = pbKeydiskClick
  end
  object pbKeyboard1: TButton
    Left = 268
    Top = 8
    Width = 37
    Height = 21
    Caption = 'kbd...'
    TabOrder = 4
    OnClick = pbKeyboard1Click
  end
  object pbKeyboard3: TButton
    Left = 268
    Top = 56
    Width = 37
    Height = 21
    Caption = 'kbd...'
    TabOrder = 6
    OnClick = pbKeyboard3Click
  end
  object pbKeyboard4: TButton
    Left = 268
    Top = 80
    Width = 37
    Height = 21
    Caption = 'kbd...'
    TabOrder = 7
    OnClick = pbKeyboard4Click
  end
  object pbKeyboard2: TButton
    Left = 268
    Top = 32
    Width = 37
    Height = 21
    Caption = 'kbd...'
    TabOrder = 5
    OnClick = pbKeyboard2Click
  end
  object ckHidePasswords: TCheckBox
    Left = 172
    Top = 108
    Width = 97
    Height = 17
    Caption = '&Hide passwords'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = ckHidePasswordsClick
  end
  object cbDrives: TComboBox
    Left = 84
    Top = 108
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
  end
  object KeyboardDialog: TKeyboardDialog
    Left = 124
    Top = 140
  end
end
