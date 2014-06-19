inherited fmeOptions_Hotkeys: TfmeOptions_Hotkeys
  Width = 337
  Height = 174
  object gbHotkeys: TGroupBox
    Left = 11
    Top = 20
    Width = 290
    Height = 129
    Caption = 'Hotkeys'
    TabOrder = 0
    object Label1: TLabel
      Left = 32
      Top = 48
      Width = 38
      Height = 13
      Caption = '&Hotkey:'
      FocusControl = hkDismount
    end
    object Label2: TLabel
      Left = 32
      Top = 100
      Width = 38
      Height = 13
      Caption = 'H&otkey:'
      FocusControl = hkDismountEmerg
    end
    object hkDismount: THotKey
      Left = 124
      Top = 44
      Width = 141
      Height = 19
      HotKey = 32833
      InvalidKeys = [hcNone]
      TabOrder = 1
    end
    object ckHotkeyDismount: TSDUCheckBox
      Left = 12
      Top = 24
      Width = 217
      Height = 17
      Caption = '&Enable dismount all hotkey'
      TabOrder = 0
      OnClick = ckCheckBoxClick
      AutoSize = True
    end
    object ckHotkeyDismountEmerg: TSDUCheckBox
      Left = 12
      Top = 76
      Width = 269
      Height = 17
      Caption = 'E&nable emergency dismount all hotkey'
      TabOrder = 2
      OnClick = ckCheckBoxClick
      AutoSize = True
    end
    object hkDismountEmerg: THotKey
      Left = 124
      Top = 96
      Width = 141
      Height = 19
      HotKey = 32833
      InvalidKeys = [hcNone]
      TabOrder = 3
    end
  end
end
