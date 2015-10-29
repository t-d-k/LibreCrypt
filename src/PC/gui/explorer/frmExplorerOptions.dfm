inherited frmExplorerOptions: TfrmExplorerOptions
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcOptions: TPageControl
    inherited tsGeneral: TTabSheet
      inherited gbGeneralMain: TGroupBox
        inherited ckStoreLayout: TSDUCheckBox
          Top = 190
          ExplicitTop = 190
        end
      end
    end
    object tsExplorer: TTabSheet [2]
      Caption = 'Explorer'
      ImageIndex = 2
      object gbAdvanced: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 348
        Align = alClient
        Caption = 'Advanced'
        TabOrder = 0
        object lblOverwritePasses: TLabel
          Left = 20
          Top = 137
          Width = 84
          Height = 13
          Caption = 'Overwrite &passes:'
          FocusControl = seOverwritePasses
        end
        object lblOverwriteMethod: TLabel
          Left = 20
          Top = 98
          Width = 86
          Height = 13
          Caption = 'O&verwrite method:'
          FocusControl = cbOverwriteMethod
        end
        object lblMoveDeletionMethod: TLabel
          Left = 20
          Top = 60
          Width = 192
          Height = 26
          Caption = 'When &moving files/folders to an opened container:'
          FocusControl = cbMoveDeletionMethod
          WordWrap = True
        end
        object seOverwritePasses: TSpinEdit64
          Left = 344
          Top = 134
          Width = 145
          Height = 22
          Increment = 1
          TabOrder = 3
        end
        object cbOverwriteMethod: TComboBox
          Left = 344
          Top = 95
          Width = 144
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = ControlChanged
        end
        object cbMoveDeletionMethod: TComboBox
          Left = 344
          Top = 57
          Width = 144
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = ControlChanged
        end
        object ckPreserveTimestampsOnStoreExtract: TSDUCheckBox
          Left = 20
          Top = 29
          Width = 222
          Height = 13
          Caption = 'Pre&serve file timestamps on store/extract'
          TabOrder = 1
          WordWrap = True
          AutoSize = True
        end
      end
    end
    object tsWebDAV: TTabSheet [3]
      Caption = 'Drive'
      ImageIndex = 3
      object gbWebDAV: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 348
        Align = alClient
        Caption = 'Drive open'
        TabOrder = 0
        object Label1: TLabel
          Left = 17
          Top = 90
          Width = 164
          Height = 13
          Caption = '&When storing files/folders, perform:'
          FocusControl = cbDefaultStoreOp
        end
        object ckWebDAV: TSDUCheckBox
          Left = 17
          Top = 21
          Width = 194
          Height = 13
          Caption = '&Map opened container to drive letter'
          TabOrder = 0
          WordWrap = True
          OnClick = ckWebDAVClick
          AutoSize = True
        end
        object gbWebDAVAdvanced: TGroupBox
          Left = 2
          Top = 115
          Width = 501
          Height = 231
          Align = alBottom
          Caption = 'Advanced'
          TabOrder = 1
          object Label6: TLabel
            Left = 14
            Top = 52
            Width = 86
            Height = 13
            Caption = '&Share base name:'
            FocusControl = edWebDAVShareName
          end
          inline fedWebDAVLogDebug: TSDUFilenameEdit
            Left = 91
            Top = 128
            Width = 399
            Height = 33
            Constraints.MinHeight = 21
            TabOrder = 3
            ExplicitLeft = 91
            ExplicitTop = 128
            ExplicitWidth = 399
            ExplicitHeight = 33
            DesignSize = (
              399
              33)
            inherited edFilename: TEdit
              Width = 297
              ExplicitWidth = 297
            end
            inherited pbBrowse: TButton
              Left = 336
              Width = 48
              ExplicitLeft = 336
              ExplicitWidth = 48
            end
          end
          inline fedWebDAVLogAccess: TSDUFilenameEdit
            Left = 93
            Top = 83
            Width = 397
            Height = 30
            Constraints.MinHeight = 21
            TabOrder = 2
            ExplicitLeft = 93
            ExplicitTop = 83
            ExplicitWidth = 397
            ExplicitHeight = 30
            DesignSize = (
              397
              30)
            inherited edFilename: TEdit
              Width = 297
              ExplicitWidth = 297
            end
            inherited pbBrowse: TButton
              Left = 328
              Width = 54
              ExplicitLeft = 328
              ExplicitWidth = 54
            end
          end
          object edWebDAVShareName: TEdit
            Left = 110
            Top = 48
            Width = 294
            Height = 21
            TabOrder = 1
            Text = 'edWebDAVShareName'
          end
          object ckOverwriteCacheOnDismount: TSDUCheckBox
            Left = 12
            Top = 20
            Width = 163
            Height = 13
            Caption = '&Overwrite cache on lock'
            TabOrder = 0
            WordWrap = True
            AutoSize = True
          end
          object ckWebDAVLogAccess: TSDUCheckBox
            Left = 14
            Top = 86
            Width = 73
            Height = 13
            Caption = '&Access log'
            TabOrder = 4
            WordWrap = True
            OnClick = ControlChanged
            AutoSize = True
          end
          object ckWebDAVLogDebug: TSDUCheckBox
            Left = 14
            Top = 132
            Width = 71
            Height = 13
            Caption = 'De&bug log'
            TabOrder = 5
            WordWrap = True
            OnClick = ControlChanged
            AutoSize = True
          end
        end
        object cbDefaultStoreOp: TComboBox
          Left = 345
          Top = 87
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnClick = ControlChanged
        end
        object ckShowHiddenItems: TSDUCheckBox
          Left = 17
          Top = 59
          Width = 163
          Height = 13
          Caption = 'Show &hidden files and folders'
          TabOrder = 3
          AutoSize = True
        end
        object ckHideKnownFileExtns: TSDUCheckBox
          Left = 17
          Top = 40
          Width = 193
          Height = 13
          Caption = 'Hide extensions of &known file types'
          TabOrder = 4
          AutoSize = True
        end
      end
    end
    inherited tsAutorun: TTabSheet
      inherited gbAutorun: TGroupBox
        ExplicitLeft = 3
        inherited Label34: TLabel
          Top = 116
          ExplicitTop = 116
        end
        inherited Label35: TLabel
          Top = 160
          ExplicitTop = 160
        end
        inherited pbPreDismountBrowse: TButton
          Top = 112
          ExplicitTop = 112
        end
        inherited edPreDismountExe: TEdit
          Top = 112
          ExplicitTop = 112
        end
        inherited pbPostDismountBrowse: TButton
          Top = 156
          ExplicitTop = 156
        end
        inherited edPostDismountExe: TEdit
          Top = 156
          ExplicitTop = 156
        end
      end
    end
  end
  inherited OpenDialog: TSDUOpenDialog
    Left = 368
    Top = 320
  end
end
