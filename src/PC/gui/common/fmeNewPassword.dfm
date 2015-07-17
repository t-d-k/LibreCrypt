inherited frmeNewPassword: TfrmeNewPassword
  Width = 511
  Height = 292
  Constraints.MinHeight = 223
  Constraints.MinWidth = 223
  ExplicitWidth = 511
  ExplicitHeight = 292
  DesignSize = (
    511
    292)
  object lblInstructPassword: TLabel
    Left = 0
    Top = 0
    Width = 511
    Height = 78
    Align = alTop
    Caption = 
      'Please enter the keyphrase to be used for securing your containe' +
      'r'#10#13'Try to enter one character for each bit of the cypher keysize' +
      '. For example for a 256 bit cypher enter a 256 character keyphra' +
      'se.'#10#13'Note: If you forget your keyphrase, you can forget your dat' +
      'a.'#10#13'Note: Newlines (blank lines) are significant. It is recommen' +
      'ded that you do not press <ENTER> after typing in your keyphrase' +
      ', as this will add an extra newline to the end of your keyphrase' +
      '.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitLeft = -207
    ExplicitWidth = 514
  end
  object lblKeyphrase: TLabel
    Left = 3
    Top = 99
    Width = 111
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Keyphrase:'
    Constraints.MinWidth = 111
  end
  object lblConfirm: TLabel
    Left = 3
    Top = 187
    Width = 111
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Confirm Keyphrase:'
    Constraints.MinWidth = 111
    FocusControl = preUserKeyConfirm
  end
  object lblStrength: TLabel
    Left = 3
    Top = 263
    Width = 111
    Height = 17
    AutoSize = False
    Caption = 'No Keyphrase'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCaptionText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object ProgressBar1: TProgressBar
    Left = 120
    Top = 263
    Width = 375
    Height = 17
    Max = 128
    MarqueeInterval = 1
    BarColor = clAqua
    BackgroundColor = clLime
    TabOrder = 2
    TabStop = True
  end
  object preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit
    Left = 120
    Top = 96
    Width = 375
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = preUserKeyFirstChange
  end
  object preUserKeyConfirm: TOTFEFreeOTFE_PasswordRichEdit
    Left = 120
    Top = 184
    Width = 375
    Height = 73
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    OnChange = preUserKeyConfirmChange
  end
end
