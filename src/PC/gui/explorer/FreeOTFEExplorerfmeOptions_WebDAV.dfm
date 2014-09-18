inherited fmeOptions_FreeOTFEExplorerWebDAV: TfmeOptions_FreeOTFEExplorerWebDAV
  Width = 486
  Height = 344
  object gbWebDAV: TGroupBox
    Left = 16
    Top = 8
    Width = 373
    Height = 301
    Caption = 'Drive mount'
    TabOrder = 0
    object lblDefaultDriveLetter: TLabel
      Left = 32
      Top = 88
      Width = 95
      Height = 13
      Caption = 'Default drive &letter:'
      FocusControl = cbDrive
    end
    object ckWebDAV: TSDUCheckBox
      Left = 12
      Top = 20
      Width = 194
      Height = 13
      Caption = '&Map mounted volume to drive letter'
      TabOrder = 0
      WordWrap = True
      OnClick = ckWebDAVClick
      AutoSize = True
    end
    object cbDrive: TComboBox
      Left = 136
      Top = 84
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object gbWebDAVAdvanced: TGroupBox
      Left = 32
      Top = 112
      Width = 329
      Height = 173
      Caption = 'Advanced'
      TabOrder = 4
      object Label6: TLabel
        Left = 12
        Top = 44
        Width = 87
        Height = 13
        Caption = '&Share base name:'
        FocusControl = edWebDAVShareName
      end
      object fedWebDAVLogDebug: TSDUFilenameEdit
        Left = 32
        Top = 136
        Width = 283
        Height = 21
        Constraints.MaxHeight = 21
        Constraints.MinHeight = 21
        TabOrder = 3
        TabStop = False
        FilenameEditType = fetSave
        FilterIndex = 0
        DesignSize = (
          283
          21)
      end
      object fedWebDAVLogAccess: TSDUFilenameEdit
        Left = 32
        Top = 88
        Width = 283
        Height = 21
        Constraints.MaxHeight = 21
        Constraints.MinHeight = 21
        TabOrder = 2
        TabStop = False
        FilenameEditType = fetSave
        FilterIndex = 0
        DesignSize = (
          283
          21)
      end
      object edWebDAVShareName: TEdit
        Left = 108
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'edWebDAVShareName'
      end
      object ckOverwriteCacheOnDismount: TSDUCheckBox
        Left = 12
        Top = 20
        Width = 163
        Height = 13
        Caption = '&Overwrite cache on dismount'
        TabOrder = 0
        WordWrap = True
        AutoSize = True
      end
      object ckWebDAVLogAccess: TSDUCheckBox
        Left = 12
        Top = 68
        Width = 73
        Height = 13
        Caption = '&Access log'
        TabOrder = 4
        WordWrap = True
        OnClick = ckWebDAVLogAccessClick
        AutoSize = True
      end
      object ckWebDAVLogDebug: TSDUCheckBox
        Left = 12
        Top = 116
        Width = 71
        Height = 13
        Caption = 'De&bug log'
        TabOrder = 5
        WordWrap = True
        OnClick = ckWebDAVLogDebugClick
        AutoSize = True
      end
    end
    object ckExploreAfterMount: TSDUCheckBox
      Left = 32
      Top = 40
      Width = 149
      Height = 17
      Caption = '&Explore after mount'
      TabOrder = 1
      AutoSize = False
    end
    object ckPromptMountSuccessful: TSDUCheckBox
      Left = 32
      Top = 60
      Width = 217
      Height = 17
      Caption = 'Display &message on successful mount'
      TabOrder = 2
      AutoSize = False
    end
  end
end
