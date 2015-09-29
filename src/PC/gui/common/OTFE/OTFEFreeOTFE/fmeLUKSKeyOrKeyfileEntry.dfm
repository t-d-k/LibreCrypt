object frmeLUKSKeyOrKeyfileEntry: TfrmeLUKSKeyOrKeyfileEntry
  Left = 0
  Top = 0
  Width = 429
  Height = 156
  TabOrder = 0
  DesignSize = (
    429
    156)
  object lblTreatNewlineAsEOF_1: TLabel
    Left = 128
    Top = 108
    Width = 63
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Treat as EOF'
    ExplicitTop = 124
  end
  object rbKeyFromUser: TRadioButton
    Left = 0
    Top = 4
    Width = 125
    Height = 17
    Caption = '&User entered:'
    TabOrder = 2
    OnClick = rbKeyFromClick
  end
  object rbKeyFromKeyfile: TRadioButton
    Left = 0
    Top = 56
    Width = 114
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Read from ke&yfile:'
    TabOrder = 3
    OnClick = rbKeyFromClick
  end
  inline feKeyfile: TSDUFilenameEdit
    Left = 120
    Top = 52
    Width = 306
    Height = 29
    Anchors = [akLeft, akRight, akBottom]
    Constraints.MinHeight = 21
    TabOrder = 1
    ExplicitLeft = 120
    ExplicitTop = 52
    ExplicitWidth = 306
    ExplicitHeight = 29
    DesignSize = (
      306
      29)
  end
  object ckKeyfileContainsASCII: TSDUCheckBox
    Left = 128
    Top = 82
    Width = 175
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Keyfile contains ASC&II password'
    TabOrder = 4
    OnClick = ckKeyfileContainsASCIIClick
    AutoSize = True
  end
  object cbNewlineType: TComboBox
    Left = 208
    Top = 105
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 5
  end
  object ckBaseIVCypherOnHashLength: TCheckBox
    Left = 128
    Top = 127
    Width = 177
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Base IV cypher on hash length'
    TabOrder = 6
  end
  object preUserKey: TPasswordRichEdit
    Left = 120
    Top = 3
    Width = 306
    Height = 44
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 0
  end
  object SDUDropFiles_Keyfile: TSDUDropFiles
    Active = False
    OnFileDrop = SDUDropFiles_KeyfileFileDrop
    Left = 384
    Top = 112
  end
end
