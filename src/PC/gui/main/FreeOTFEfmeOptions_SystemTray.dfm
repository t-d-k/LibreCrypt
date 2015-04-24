inherited fmeOptions_SystemTray: TfmeOptions_SystemTray
  Width = 424
  Height = 314
  ExplicitWidth = 424
  ExplicitHeight = 314
  object gbSystemTrayIcon: TGroupBox
    Left = 12
    Top = 20
    Width = 353
    Height = 257
    Caption = 'System Tray Icon'
    TabOrder = 0
    object ckUseSystemTrayIcon: TSDUCheckBox
      Left = 12
      Top = 20
      Width = 245
      Height = 17
      Caption = '&Display system tray icon'
      TabOrder = 3
      OnClick = ckUseSystemTrayIconClick
      AutoSize = True
    end
    object ckMinToIcon: TSDUCheckBox
      Left = 28
      Top = 43
      Width = 137
      Height = 13
      Caption = '&Mimimise to system tray'
      TabOrder = 1
      AutoSize = True
    end
    object ckCloseToIcon: TSDUCheckBox
      Left = 28
      Top = 64
      Width = 122
      Height = 13
      Caption = '&Close to system tray'
      TabOrder = 2
      AutoSize = True
    end
    object gbClickActions: TGroupBox
      Left = 28
      Top = 92
      Width = 309
      Height = 149
      Caption = 'Click actions'
      TabOrder = 0
      object Label1: TLabel
        Left = 12
        Top = 24
        Width = 121
        Height = 13
        Caption = 'When the system tray is:'
      end
      object Label2: TLabel
        Left = 12
        Top = 88
        Width = 275
        Height = 13
        Caption = 'with the left mousebutton, carry out the following action:'
      end
      object rbSingleClick: TRadioButton
        Left = 24
        Top = 44
        Width = 113
        Height = 17
        Caption = 'Single clicked'
        TabOrder = 0
      end
      object rbDoubleClick: TRadioButton
        Left = 24
        Top = 64
        Width = 113
        Height = 17
        Caption = 'Double clicked'
        TabOrder = 1
      end
      object cbClickAction: TComboBox
        Left = 24
        Top = 108
        Width = 185
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
    end
  end
end
