object frmeLUKSKeyOrKeyfileEntry: TfrmeLUKSKeyOrKeyfileEntry
  Left = 0
  Top = 0
  Width = 464
  Height = 138
  TabOrder = 0
  object lblTreatNewlineAsEOF_1: TLabel
    Left = 148
    Top = 120
    Width = 26
    Height = 13
    Caption = '&Treat'
  end
  object lblTreatNewlineAsEOF_2: TLabel
    Left = 336
    Top = 120
    Width = 34
    Height = 13
    Caption = 'as EOF'
  end
  object preUserKey: TOTFEFreeOTFE_PasswordRichEdit
    Left = 128
    Top = 0
    Width = 333
    Height = 61
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'preUserKey')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = preUserKeyChange
  end
  object rbKeyFromUser: TRadioButton
    Left = 0
    Top = 4
    Width = 125
    Height = 17
    Caption = '&User entered:'
    TabOrder = 4
    OnClick = rbKeyFromClick
  end
  object rbKeyFromKeyfile: TRadioButton
    Left = 0
    Top = 72
    Width = 125
    Height = 17
    Caption = 'Read from ke&yfile:'
    TabOrder = 1
    OnClick = rbKeyFromClick
  end
  object feKeyfile: TSDUFilenameEdit
    Left = 128
    Top = 68
    Width = 333
    Height = 21
    Constraints.MaxHeight = 21
    Constraints.MinHeight = 21
    TabOrder = 2
    TabStop = False
    FilterIndex = 0
    OnChange = feKeyfileChange
    DesignSize = (
      333
      21)
  end
  object ckKeyfileContainsASCII: TSDUCheckBox
    Left = 128
    Top = 96
    Width = 175
    Height = 13
    Caption = 'Keyfile contains ASC&II password'
    TabOrder = 3
    OnClick = ckKeyfileContainsASCIIClick
    AutoSize = True
  end
  object cbNewlineType: TComboBox
    Left = 184
    Top = 116
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 5
  end
  object SDUDropFiles_Keyfile: TSDUDropFiles
    Active = False
    OnFileDrop = SDUDropFiles_KeyfileFileDrop
    Left = 384
    Top = 80
  end
end
