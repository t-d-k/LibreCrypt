inherited fmeOptions_FreeOTFEGeneral: TfmeOptions_FreeOTFEGeneral
  Width = 504
  Height = 382
  object gbGeneral: TGroupBox
    Left = 16
    Top = 16
    Width = 465
    Height = 345
    Caption = 'General'
    TabOrder = 0
    object lblDefaultDriveLetter: TLabel
      Left = 264
      Top = 80
      Width = 95
      Height = 13
      Caption = 'Default drive &letter:'
      FocusControl = cbDrive
    end
    object lblLanguage: TLabel
      Left = 264
      Top = 24
      Width = 51
      Height = 13
      Caption = '&Language:'
      FocusControl = cbLanguage
      WordWrap = True
    end
    object lblChkUpdatesFreq: TLabel
      Left = 264
      Top = 132
      Width = 92
      Height = 13
      Caption = 'Check for &updates:'
      FocusControl = cbChkUpdatesFreq
    end
    object ckDisplayToolbar: TSDUCheckBox
      Left = 12
      Top = 44
      Width = 221
      Height = 17
      Caption = 'Display &toolbar'
      TabOrder = 2
      OnClick = ControlChanged
      AutoSize = True
    end
    object ckDisplayStatusbar: TSDUCheckBox
      Left = 12
      Top = 104
      Width = 221
      Height = 17
      Caption = 'Display &statusbar'
      TabOrder = 8
      AutoSize = True
    end
    object ckExploreAfterMount: TSDUCheckBox
      Left = 12
      Top = 24
      Width = 221
      Height = 17
      Caption = '&Explore after mount'
      TabOrder = 0
      AutoSize = True
    end
    object pnlVertSplit: TPanel
      Left = 240
      Top = 28
      Width = 8
      Height = 293
      Caption = 'pnlVertSplit'
      TabOrder = 1
    end
    object cbDrive: TComboBox
      Left = 280
      Top = 96
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
    end
    object ckShowPasswords: TSDUCheckBox
      Left = 12
      Top = 124
      Width = 221
      Height = 17
      Caption = 'Display &passwords when entered'
      TabOrder = 9
      AutoSize = True
    end
    object cbLanguage: TComboBox
      Left = 280
      Top = 44
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbLanguageChange
    end
    object pbLangDetails: TButton
      Left = 428
      Top = 44
      Width = 21
      Height = 21
      Caption = '?'
      TabOrder = 4
      OnClick = pbLangDetailsClick
    end
    object ckPromptMountSuccessful: TSDUCheckBox
      Left = 12
      Top = 144
      Width = 221
      Height = 17
      Caption = 'Display &message on successful mount'
      TabOrder = 10
      AutoSize = True
    end
    object ckDisplayToolbarLarge: TSDUCheckBox
      Left = 32
      Top = 64
      Width = 201
      Height = 17
      Caption = 'Large &icons'
      TabOrder = 5
      OnClick = ControlChanged
      AutoSize = True
    end
    object ckDisplayToolbarCaptions: TSDUCheckBox
      Left = 32
      Top = 84
      Width = 201
      Height = 17
      Caption = 'Display &captions'
      TabOrder = 6
      AutoSize = True
    end
    object cbChkUpdatesFreq: TComboBox
      Left = 280
      Top = 148
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
    end
  end
end
