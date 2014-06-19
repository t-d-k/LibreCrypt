inherited fmeOptions_FreeOTFEAdvanced: TfmeOptions_FreeOTFEAdvanced
  Width = 504
  Height = 382
  object gbAdvanced: TGroupBox
    Left = 16
    Top = 16
    Width = 469
    Height = 345
    Caption = 'Advanced'
    TabOrder = 0
    object lblDragDrop: TLabel
      Left = 264
      Top = 24
      Width = 151
      Height = 26
      Caption = 'Assume files &dragged onto FreeOTFE are encrypted using:'
      FocusControl = cbDragDrop
      WordWrap = True
    end
    object lblMRUMaxItemCountInst: TLabel
      Left = 280
      Top = 152
      Width = 136
      Height = 13
      Caption = '(Set to 0 to disable MRU list)'
    end
    object lblMRUMaxItemCount: TLabel
      Left = 264
      Top = 96
      Width = 162
      Height = 26
      Caption = '&Max. number of volumes to show in most recently used list:'
      FocusControl = seMRUMaxItemCount
      WordWrap = True
    end
    object lblOnNormalDismountFail: TLabel
      Left = 12
      Top = 288
      Width = 195
      Height = 13
      Caption = 'If u&nable to dismount a volume normally:'
      FocusControl = cbOnNormalDismountFail
      WordWrap = True
    end
    object lblDefaultMountAs: TLabel
      Left = 264
      Top = 176
      Width = 97
      Height = 13
      Caption = 'Default mount t&ype:'
      FocusControl = cbDefaultMountAs
      WordWrap = True
    end
    object lblOnExitWhenMounted: TLabel
      Left = 12
      Top = 192
      Width = 163
      Height = 13
      Caption = 'If volumes &mounted when exiting:'
      FocusControl = cbOnExitWhenMounted
      WordWrap = True
    end
    object lblOnExitWhenPortableMode: TLabel
      Left = 12
      Top = 240
      Width = 159
      Height = 13
      Caption = 'If in portable mode when &exiting:'
      FocusControl = cbOnExitWhenPortableMode
      WordWrap = True
    end
    object ckAllowMultipleInstances: TSDUCheckBox
      Left = 12
      Top = 24
      Width = 221
      Height = 17
      Caption = '&Allow multiple instances'
      TabOrder = 0
      WordWrap = True
      AutoSize = True
    end
    object cbDragDrop: TComboBox
      Left = 280
      Top = 56
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object ckAutoStartPortable: TSDUCheckBox
      Left = 12
      Top = 112
      Width = 221
      Height = 30
      Caption = 
        'Autostart &portable mode if FreeOTFE drivers not installed/runni' +
        'ng'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 6
      WordWrap = True
      AutoSize = True
    end
    object ckAdvancedMountDlg: TSDUCheckBox
      Left = 12
      Top = 44
      Width = 221
      Height = 17
      Caption = 'Display advanced &options when mounting'
      TabOrder = 2
      WordWrap = True
      AutoSize = True
    end
    object ckRevertVolTimestamps: TSDUCheckBox
      Left = 12
      Top = 68
      Width = 221
      Height = 17
      Caption = '&Revert volume timestamps on dismount'
      TabOrder = 4
      WordWrap = True
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
    object seMRUMaxItemCount: TSpinEdit64
      Left = 280
      Top = 128
      Width = 145
      Height = 22
      Increment = 1
      MaxValue = 10
      TabOrder = 7
    end
    object ckWarnBeforeForcedDismount: TSDUCheckBox
      Left = 12
      Top = 92
      Width = 169
      Height = 13
      Caption = 'Warn before &forced dismounts'
      TabOrder = 5
      WordWrap = True
      AutoSize = True
    end
    object cbOnNormalDismountFail: TComboBox
      Left = 28
      Top = 308
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 13
    end
    object cbDefaultMountAs: TComboBox
      Left = 280
      Top = 196
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
    end
    object cbOnExitWhenMounted: TComboBox
      Left = 28
      Top = 212
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
    end
    object cbOnExitWhenPortableMode: TComboBox
      Left = 28
      Top = 260
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 12
    end
    object ckAllowNewlinesInPasswords: TSDUCheckBox
      Left = 12
      Top = 148
      Width = 157
      Height = 13
      Caption = 'Allow &newlines in passwords'
      TabOrder = 8
      WordWrap = True
      AutoSize = True
    end
    object ckAllowTabsInPasswords: TSDUCheckBox
      Left = 12
      Top = 168
      Width = 137
      Height = 13
      Caption = 'Allow ta&bs in passwords'
      TabOrder = 9
      WordWrap = True
      AutoSize = True
    end
  end
end
