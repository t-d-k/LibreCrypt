inherited frmExplorerOptions: TfrmExplorerOptions
  ExplicitWidth = 519
  ExplicitHeight = 516
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcOptions: TPageControl
    object tsGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      ExplicitLeft = 7
      ExplicitTop = 33
      object gbGeneral: TGroupBox
        Left = 0
        Top = 0
        Width = 492
        Height = 348
        Align = alClient
        Caption = 'General'
        TabOrder = 0
        ExplicitLeft = 16
        ExplicitTop = 16
        ExplicitWidth = 469
        ExplicitHeight = 285
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
          Width = 90
          Height = 13
          Caption = 'Check for &updates:'
          FocusControl = cbChkUpdatesFreq
        end
        object Label1: TLabel
          Left = 264
          Top = 76
          Width = 164
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
        object Panel1: TPanel
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
          TabOrder = 11
          OnClick = ControlChanged
        end
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      object gbAdvanced: TGroupBox
        Left = 16
        Top = 3
        Width = 469
        Height = 345
        Caption = 'Advanced'
        TabOrder = 0
        object lblMRUMaxItemCountInst: TLabel
          Left = 280
          Top = 240
          Width = 134
          Height = 13
          Caption = '(Set to 0 to disable MRU list)'
        end
        object lblMRUMaxItemCount: TLabel
          Left = 264
          Top = 184
          Width = 158
          Height = 26
          Caption = '&Max. number of volumes to show in most recently used list:'
          FocusControl = seMRUMaxItemCount
          WordWrap = True
        end
        object lblOverwritePasses: TLabel
          Left = 264
          Top = 135
          Width = 84
          Height = 13
          Caption = 'Overwrite &passes:'
          FocusControl = seOverwritePasses
        end
        object lblOverwriteMethod: TLabel
          Left = 264
          Top = 88
          Width = 86
          Height = 13
          Caption = 'O&verwrite method:'
          FocusControl = cbOverwriteMethod
        end
        object lblMoveDeletionMethod: TLabel
          Left = 264
          Top = 28
          Width = 147
          Height = 26
          Caption = 'When &moving files/folders to a mounted container:'
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
          Caption = '&Revert container timestamps on dismount'
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
          TabOrder = 5
          OnChange = ControlChanged
        end
        object cbMoveDeletionMethod: TComboBox
          Left = 280
          Top = 56
          Width = 144
          Height = 21
          Style = csDropDownList
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
    object tsWebDAV: TTabSheet
      Caption = 'Drive'
      ImageIndex = 3
      object gbWebDAV: TGroupBox
        Left = 0
        Top = 0
        Width = 492
        Height = 348
        Align = alClient
        Caption = 'Drive mount'
        TabOrder = 0
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 373
        ExplicitHeight = 301
        object lblDefaultDriveLetter: TLabel
          Left = 32
          Top = 88
          Width = 89
          Height = 13
          Caption = 'Default drive &letter:'
          FocusControl = cbDrive
        end
        object ckWebDAV: TSDUCheckBox
          Left = 12
          Top = 20
          Width = 194
          Height = 13
          Caption = '&Map mounted container to drive letter'
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
            Width = 86
            Height = 13
            Caption = '&Share base name:'
            FocusControl = edWebDAVShareName
          end
          inline fedWebDAVLogDebug: TSDUFilenameEdit
            Left = 32
            Top = 136
            Width = 283
            Height = 21
            Constraints.MaxHeight = 21
            Constraints.MinHeight = 21
            TabOrder = 3
            ExplicitLeft = 32
            ExplicitTop = 136
            ExplicitWidth = 283
            ExplicitHeight = 21
            DesignSize = (
              283
              21)
            inherited edFilename: TEdit
              Width = 171
              ExplicitWidth = 171
            end
            inherited pbBrowse: TButton
              Left = 193
              ExplicitLeft = 193
            end
          end
          inline fedWebDAVLogAccess: TSDUFilenameEdit
            Left = 32
            Top = 88
            Width = 283
            Height = 21
            Constraints.MaxHeight = 21
            Constraints.MinHeight = 21
            TabOrder = 2
            ExplicitLeft = 32
            ExplicitTop = 88
            ExplicitWidth = 283
            ExplicitHeight = 21
            DesignSize = (
              283
              21)
            inherited edFilename: TEdit
              Width = 171
              ExplicitWidth = 171
            end
            inherited pbBrowse: TButton
              Left = 193
              ExplicitLeft = 193
            end
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
            OnClick = ControlChanged
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
            OnClick = ControlChanged
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
    object tsAutorun: TTabSheet
      Caption = 'Autorun'
      ImageIndex = 4
      inline fmeOptions_Autorun1: TfmeAutorunOptions
        Left = 21
        Top = -10
        Width = 471
        Height = 358
        TabOrder = 0
        ExplicitLeft = 21
        ExplicitTop = -10
        inherited gbAutorun: TGroupBox
          Left = 0
          Top = 0
          Width = 471
          Height = 358
          Align = alClient
          inherited Label33: TLabel
            Width = 56
            ExplicitWidth = 56
          end
          inherited Label34: TLabel
            Width = 64
            ExplicitWidth = 64
          end
          inherited Label35: TLabel
            Width = 69
            ExplicitWidth = 69
          end
        end
      end
    end
  end
end
