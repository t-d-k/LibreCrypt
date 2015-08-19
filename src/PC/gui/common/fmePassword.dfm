inherited frmePassword: TfrmePassword
  Width = 498
  Height = 81
  ExplicitWidth = 498
  ExplicitHeight = 81
  object lblKeyPhrase: TLabel
    Left = 8
    Top = 3
    Width = 58
    Height = 13
    Caption = '&Key phrase:'
  end
  object mmShown: TMemo
    Left = 80
    Top = 3
    Width = 401
    Height = 65
    Lines.Strings = (
      '')
    TabOrder = 0
    OnChange = mmShownChange
    OnKeyDown = mmShownKeyDown
    OnKeyPress = mmShownKeyPress
    OnKeyUp = mmShownKeyUp
  end
  object mmReal: TMemo
    Left = 184
    Top = 19
    Width = 145
    Height = 22
    Lines.Strings = (
      'mmReal')
    TabOrder = 1
    Visible = False
  end
end
