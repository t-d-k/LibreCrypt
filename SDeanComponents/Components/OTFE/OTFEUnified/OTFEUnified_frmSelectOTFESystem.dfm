object frmSelectOTFESystem: TfrmSelectOTFESystem
  Left = 458
  Top = 399
  BorderStyle = bsDialog
  Caption = 'Please select OTFE system...'
  ClientHeight = 178
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 249
    Height = 26
    Caption = 
      'It appears that more than one enabled OTFE system can be used to' +
      ' carry out the requested operation.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 44
    Width = 259
    Height = 26
    Caption = 
      'Please select which of the following OTFE systems you wish to us' +
      'e:'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 108
    Width = 238
    Height = 26
    Caption = 
      'You may prevent this dialog appearing in future by disabling unu' +
      'sed OTFE systems'
    WordWrap = True
  end
  object cbOTFESystem: TComboBox
    Left = 64
    Top = 76
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbOTFESystemChange
  end
  object pbOK: TButton
    Left = 53
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 145
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
