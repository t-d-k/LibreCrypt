inherited fmeOptions_FreeOTFEExplorerAdvanced: TfmeOptions_FreeOTFEExplorerAdvanced
  Width = 504
  Height = 382
  object gbAdvanced: TGroupBox
    Left = 16
    Top = 16
    Width = 469
    Height = 345
    Caption = 'Advanced'
    TabOrder = 0
    object lblMRUMaxItemCountInst: TLabel
      Left = 280
      Top = 240
      Width = 136
      Height = 13
      Caption = '(Set to 0 to disable MRU list)'
    end
    object lblMRUMaxItemCount: TLabel
      Left = 264
      Top = 184
      Width = 162
      Height = 26
      Caption = '&Max. number of volumes to show in most recently used list:'
      FocusControl = seMRUMaxItemCount
      WordWrap = True
    end
    object lblOverwritePasses: TLabel
      Left = 264
      Top = 135
      Width = 88
      Height = 13
      Caption = 'Overwrite &passes:'
      FocusControl = seOverwritePasses
    end
    object lblOverwriteMethod: TLabel
      Left = 264
      Top = 88
      Width = 91
      Height = 13
      Caption = 'O&verwrite method:'
      FocusControl = cbOverwriteMethod
    end
    object lblMoveDeletionMethod: TLabel
      Left = 264
      Top = 28
      Width = 149
      Height = 26
      Caption = 'When &moving files/folders to a mounted volume:'
      FocusControl = cbMoveDeletionMethod
      WordWrap = True
    end
    object ckAdvancedMountDlg: TSDUCheckBox
      Left = 12
      Top = 28
      Width = 221
      Height = 17
      Caption = 'Display advanced &options when mounting'
      TabOrder = 1
      WordWrap = True
      AutoSize = True
    end
    object ckRevertVolTimestamps: TSDUCheckBox
      Left = 12
      Top = 52
      Width = 221
      Height = 17
      Caption = '&Revert volume timestamps on dismount'
      TabOrder = 3
      WordWrap = True
      AutoSize = True
    end
    object pnlVertSplit: TPanel
      Left = 240
      Top = 28
      Width = 8
      Height = 293
      Caption = 'pnlVertSplit'
      TabOrder = 0
    end
    object seMRUMaxItemCount: TSpinEdit64
      Left = 280
      Top = 216
      Width = 145
      Height = 22
      Increment = 1
      MaxValue = 10
      TabOrder = 9
    end
    object seOverwritePasses: TSpinEdit64
      Left = 280
      Top = 152
      Width = 145
      Height = 22
      Increment = 1
      TabOrder = 8
    end
    object cbOverwriteMethod: TComboBox
      Left = 280
      Top = 104
      Width = 144
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = ControlChanged
    end
    object cbMoveDeletionMethod: TComboBox
      Left = 280
      Top = 56
      Width = 144
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = ControlChanged
    end
    object ckPreserveTimestampsOnStoreExtract: TSDUCheckBox
      Left = 12
      Top = 76
      Width = 222
      Height = 13
      Caption = 'Pre&serve file timestamps on store/extract'
      TabOrder = 4
      WordWrap = True
      AutoSize = True
    end
    object ckAllowTabsInPasswords: TSDUCheckBox
      Left = 12
      Top = 120
      Width = 137
      Height = 13
      Caption = 'Allow ta&bs in passwords'
      TabOrder = 7
      WordWrap = True
      AutoSize = True
    end
    object ckAllowNewlinesInPasswords: TSDUCheckBox
      Left = 12
      Top = 96
      Width = 157
      Height = 13
      Caption = 'Allow &newlines in passwords'
      TabOrder = 6
      WordWrap = True
      AutoSize = True
    end
  end
end
