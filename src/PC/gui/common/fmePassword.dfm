inherited frmePassword: TfrmePassword
  Width = 498
  Height = 81
  Color = clWindow
  ParentBackground = False
  ParentColor = False
  ParentDoubleBuffered = False
  ExplicitWidth = 498
  ExplicitHeight = 81
  object lblKeyPhrase: TLabel
    Left = 8
    Top = 3
    Width = 58
    Height = 13
    Caption = '&Key phrase:'
  end
  object preUserKey: TPasswordRichEdit
    AlignWithMargins = True
    Left = 70
    Top = 5
    Width = 423
    Height = 71
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    Color = clMoneyGreen
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    PlainText = True
    TabOrder = 0
    OnChange = preUserKeyChange
    ExplicitLeft = 72
    ExplicitTop = 3
    ExplicitHeight = 75
  end
end
