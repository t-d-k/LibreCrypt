inherited frmePassword: TfrmePassword
  Width = 498
  Height = 81
  ExplicitWidth = 498
  ExplicitHeight = 81
  DesignSize = (
    498
    81)
  object lblKeyPhrase: TLabel
    Left = 8
    Top = 3
    Width = 58
    Height = 13
    Caption = '&Key phrase:'
  end
  object preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit
    Left = 71
    Top = 3
    Width = 424
    Height = 75
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = preUserKeyFirstChange
  end
end
