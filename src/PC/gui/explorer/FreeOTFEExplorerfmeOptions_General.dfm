inherited fmeOptions_FreeOTFEExplorerGeneral: TfmeOptions_FreeOTFEExplorerGeneral
  Width = 504
  Height = 321
  object gbGeneral: TGroupBox
    Left = 16
    Top = 16
    Width = 469
    Height = 285
    Caption = 'General'
    TabOrder = 0
    object tlab: TLabel
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
      Top = 160
      Width = 92
      Height = 13
      Caption = 'Check for &updates:'
      FocusControl = cbChkUpdatesFreq
    end
    object Label1: TLabel
      Left = 264
      Top = 76
      Width = 172
      Height = 13
      Caption = '&When storing files/folders, perform:'
      FocusControl = cbDefaultStoreOp
    end
    object ckDisplayToolbar: TSDUCheckBox
      Left = 12
      Top = 24
      Width = 221
      Height = 17
      Caption = 'Display &toolbar'
      TabOrder = 0
      OnClick = ControlChanged
      AutoSize = True
    end
    object pnlVertSplit: TPanel
      Left = 240
      Top = 28
      Width = 8
      Height = 221
      Caption = 'pnlVertSplit'
      TabOrder = 1
    end
    object ckShowPasswords: TSDUCheckBox
      Left = 12
      Top = 144
      Width = 221
      Height = 17
      Caption = 'Display &passwords when entered'
      TabOrder = 6
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
    object ckDisplayToolbarLarge: TSDUCheckBox
      Left = 32
      Top = 44
      Width = 201
      Height = 17
      Caption = 'Large &icons'
      TabOrder = 2
      OnClick = ControlChanged
      AutoSize = True
    end
    object ckDisplayToolbarCaptions: TSDUCheckBox
      Left = 32
      Top = 64
      Width = 201
      Height = 17
      Caption = 'Display &captions'
      TabOrder = 5
      AutoSize = True
    end
    object cbChkUpdatesFreq: TComboBox
      Left = 280
      Top = 180
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
    end
    object ckShowHiddenItems: TSDUCheckBox
      Left = 12
      Top = 164
      Width = 163
      Height = 13
      Caption = 'Show &hidden files and folders'
      TabOrder = 7
      AutoSize = True
    end
    object ckHideKnownFileExtns: TSDUCheckBox
      Left = 12
      Top = 184
      Width = 193
      Height = 13
      Caption = 'Hide extensions of &known file types'
      TabOrder = 9
      AutoSize = True
    end
    object ckStoreLayout: TSDUCheckBox
      Left = 12
      Top = 204
      Width = 155
      Height = 13
      Caption = 'Save &window layout on exit'
      TabOrder = 10
      OnClick = ckStoreLayoutClick
      AutoSize = True
    end
    object cbDefaultStoreOp: TComboBox
      Left = 280
      Top = 96
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
    end
  end
end
