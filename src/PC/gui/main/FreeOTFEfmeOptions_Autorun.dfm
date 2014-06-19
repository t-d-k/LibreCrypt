inherited fmeOptions_Autorun: TfmeOptions_Autorun
  Width = 471
  Height = 358
  object gbAutorun: TGroupBox
    Left = 7
    Top = 16
    Width = 430
    Height = 321
    Caption = 'Autorun pre/post-mount/dismount'
    TabOrder = 0
    object Label33: TLabel
      Left = 12
      Top = 192
      Width = 59
      Height = 13
      Caption = 'Post-&mount:'
      FocusControl = edPostMountExe
    end
    object Label34: TLabel
      Left = 12
      Top = 220
      Width = 67
      Height = 13
      Caption = '&Pre-dismount:'
      FocusControl = edPreDismountExe
    end
    object Label35: TLabel
      Left = 12
      Top = 248
      Width = 72
      Height = 13
      Caption = 'Post-&dismount:'
      FocusControl = edPostDismountExe
    end
    object edPostMountExe: TEdit
      Left = 104
      Top = 188
      Width = 289
      Height = 21
      TabOrder = 0
      Text = 'edPostMountExe'
    end
    object pbPostMountBrowse: TButton
      Left = 396
      Top = 188
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = pbPostMountBrowseClick
    end
    object pbPreDismountBrowse: TButton
      Left = 396
      Top = 216
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = pbPreDismountBrowseClick
    end
    object edPreDismountExe: TEdit
      Left = 104
      Top = 216
      Width = 289
      Height = 21
      TabOrder = 2
      Text = 'edPreDismountExe'
    end
    object pbPostDismountBrowse: TButton
      Left = 396
      Top = 244
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = pbPostDismountBrowseClick
    end
    object edPostDismountExe: TEdit
      Left = 104
      Top = 244
      Width = 289
      Height = 21
      TabOrder = 4
      Text = 'edPostDismountExe'
    end
    object ckPrePostExeWarn: TSDUCheckBox
      Left = 134
      Top = 288
      Width = 161
      Height = 17
      Caption = '&Warn user if file is not found.'
      TabOrder = 6
      AutoSize = True
    end
    object reInstructions: TOTFEFreeOTFE_InstructionRichEdit
      Left = 12
      Top = 20
      Width = 405
      Height = 161
      Lines.Strings = (
        'reInstructions')
      TabOrder = 7
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 344
    Top = 264
  end
end
