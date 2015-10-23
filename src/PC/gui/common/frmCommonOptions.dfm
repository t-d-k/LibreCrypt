object frmCommonOptions: TfrmCommonOptions
  Left = 435
  Top = 197
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 488
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblSettingsLocation: TLabel
    Left = 20
    Top = 405
    Width = 112
    Height = 13
    Caption = '&Save above settings to:'
    FocusControl = cbSettingsLocation
  end
  object imgNoSaveWarning: TImage
    Left = 452
    Top = 382
    Width = 16
    Height = 16
    Picture.Data = {
      055449636F6E0000010001001010100000000000280100001600000028000000
      10000000200000000100040000000000C0000000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000000003BBBBBBBBBBBBBB003BBBBB00BBBBB0003BBBBB0
      0BBBBB00003BBBBBBBBBB000003BBBBBBBBBB0000003BBB00BBB00000003BBB0
      0BBB000000003BB00BB0000000003BB00BB00000000003B00B000000000003BB
      BB0000000000003BB00000000000003BB0000000000000030000000000000003
      0000000000000000000000008001000080010000C0030000C0030000E0070000
      E0070000F00F0000F00F0000F81F0000F81F0000FC3F0000FC3F0000FE7F0000
      FE7F0000}
  end
  object pbOK: TButton
    Left = 175
    Top = 452
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 263
    Top = 452
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = pbCancelClick
  end
  object cbSettingsLocation: TComboBox
    Left = 169
    Top = 406
    Width = 169
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = ControlChanged
  end
  object ckAssociateFiles: TSDUCheckBox
    Left = 20
    Top = 433
    Width = 158
    Height = 13
    Caption = 'Associate %s with ".vol" &files'
    TabOrder = 2
    AutoSize = True
  end
  object pcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 513
    Height = 376
    ActivePage = tsGeneral
    Align = alTop
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbGeneralMain: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 348
        Align = alClient
        Caption = 'General'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 0
        object lblDefaultDriveLetter: TLabel
          Left = 264
          Top = 80
          Width = 89
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
          Width = 90
          Height = 13
          Caption = 'Check for &updates:'
          FocusControl = cbChkUpdatesFreq
        end
        object ckDisplayToolbar: TSDUCheckBox
          Left = 16
          Top = 48
          Width = 221
          Height = 17
          Caption = 'Show &toolbar'
          TabOrder = 10
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckDisplayStatusbar: TSDUCheckBox
          Left = 16
          Top = 120
          Width = 221
          Height = 17
          Caption = 'Show &statusbar'
          TabOrder = 7
          AutoSize = True
        end
        object ckExploreAfterMount: TSDUCheckBox
          Left = 16
          Top = 24
          Width = 221
          Height = 17
          Caption = '&Explore after open'
          TabOrder = 0
          AutoSize = True
        end
        object cbDrive: TComboBox
          Left = 264
          Top = 99
          Width = 185
          Height = 21
          Style = csDropDownList
          TabOrder = 6
        end
        object ckShowPasswords: TSDUCheckBox
          Left = 16
          Top = 144
          Width = 221
          Height = 17
          Caption = 'Show &passwords when entered'
          TabOrder = 8
          AutoSize = True
        end
        object cbLanguage: TComboBox
          Left = 264
          Top = 43
          Width = 185
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = cbLanguageChange
        end
        object pbLangDetails: TButton
          Left = 455
          Top = 43
          Width = 21
          Height = 21
          Caption = '?'
          TabOrder = 3
          OnClick = pbLangDetailsClick
        end
        object ckPromptMountSuccessful: TSDUCheckBox
          Left = 16
          Top = 167
          Width = 197
          Height = 13
          Caption = 'Show &message on successful open'
          TabOrder = 9
          AutoSize = True
        end
        object ckDisplayToolbarLarge: TSDUCheckBox
          Left = 34
          Top = 71
          Width = 201
          Height = 17
          Caption = 'Large &icons'
          TabOrder = 4
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckDisplayToolbarCaptions: TSDUCheckBox
          Left = 34
          Top = 94
          Width = 201
          Height = 17
          Caption = 'Show &captions'
          TabOrder = 5
          AutoSize = True
        end
        object cbChkUpdatesFreq: TComboBox
          Left = 264
          Top = 151
          Width = 185
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
        object ckStoreLayout: TSDUCheckBox
          Left = 16
          Top = 191
          Width = 155
          Height = 13
          Caption = 'Save &window layout on exit'
          TabOrder = 11
          OnClick = ckStoreLayoutClick
          AutoSize = True
        end
      end
    end
    object tsPKCS11: TTabSheet
      Caption = 'PKCS#11'
      ImageIndex = 4
      object gbPKCS11: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 348
        Align = alClient
        Caption = 'Security Token/Smartcard support'
        TabOrder = 0
        object lblLibrary: TLabel
          Left = 16
          Top = 47
          Width = 80
          Height = 13
          Caption = 'PKCS#11 &library:'
          FocusControl = feLibFilename
        end
        object ckEnablePKCS11: TCheckBox
          Left = 16
          Top = 24
          Width = 421
          Height = 17
          Caption = '&Enable PKCS#11 support'
          TabOrder = 0
          OnClick = ControlChanged
        end
        object pbVerify: TButton
          Left = 204
          Top = 89
          Width = 75
          Height = 25
          Caption = '&Verify'
          TabOrder = 2
          OnClick = pbVerifyClick
        end
        object gbPKCS11AutoActions: TGroupBox
          Left = 16
          Top = 120
          Width = 473
          Height = 105
          Caption = 'Autoopen/lock'
          TabOrder = 3
          object lblAutoMountVolume: TLabel
            Left = 12
            Top = 43
            Width = 38
            Height = 13
            Caption = 'V&olume:'
          end
          object ckPKCS11AutoDismount: TCheckBox
            Left = 12
            Top = 72
            Width = 397
            Height = 17
            Caption = 'Auto &lock PKCS#11 containers when associated token is removed'
            TabOrder = 1
            OnClick = ControlChanged
          end
          object ckPKCS11AutoMount: TCheckBox
            Left = 12
            Top = 20
            Width = 397
            Height = 17
            Caption = 'Auto &open specified container on token insertion'
            TabOrder = 0
            OnClick = ControlChanged
          end
          inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
            Left = 72
            Top = 44
            Width = 385
            Height = 29
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            ExplicitLeft = 72
            ExplicitTop = 44
            ExplicitWidth = 385
            ExplicitHeight = 29
            DesignSize = (
              385
              29)
            inherited bbBrowsePartition: TBitBtn
              Left = 346
              Top = 1
              Width = 20
              ExplicitLeft = 346
              ExplicitTop = 1
              ExplicitWidth = 20
            end
            inherited bbBrowseFile: TBitBtn
              Left = 319
              Top = 1
              ExplicitLeft = 319
              ExplicitTop = 1
            end
            inherited edFilename: TEdit
              Width = 313
              ExplicitWidth = 313
            end
          end
        end
        inline feLibFilename: TSDUFilenameEdit
          Left = 120
          Top = 48
          Width = 369
          Height = 35
          Constraints.MinHeight = 21
          TabOrder = 1
          ExplicitLeft = 120
          ExplicitTop = 48
          ExplicitWidth = 369
          ExplicitHeight = 35
          DesignSize = (
            369
            35)
          inherited edFilename: TEdit
            Left = 3
            Width = 302
            ExplicitLeft = 3
            ExplicitWidth = 302
          end
          inherited pbBrowse: TButton
            Left = 320
            Width = 46
            OnClick = feLibFilenamepbBrowseClick
            ExplicitLeft = 320
            ExplicitWidth = 46
          end
        end
        object pbAutoDetect: TButton
          Left = 123
          Top = 89
          Width = 75
          Height = 25
          Caption = '&Autodetect'
          TabOrder = 4
          OnClick = pbAutoDetectClick
        end
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblMRUMaxItemCount: TLabel
        Left = 16
        Top = 136
        Width = 270
        Height = 26
        Caption = '&Max. number of containers to show in most recently used list:'
        FocusControl = seMRUMaxItemCount
        WordWrap = True
      end
      object lblMRUMaxItemCountInst: TLabel
        Left = 167
        Top = 171
        Width = 134
        Height = 13
        Caption = '(Set to 0 to disable MRU list)'
      end
      object ckRevertVolTimestamps: TSDUCheckBox
        Left = 16
        Top = 43
        Width = 221
        Height = 17
        Caption = '&Reset container timestamps on lock'
        TabOrder = 0
        WordWrap = True
        AutoSize = True
      end
      object ckAllowNewlinesInPasswords: TSDUCheckBox
        Left = 16
        Top = 72
        Width = 157
        Height = 13
        Caption = 'Allow &newlines in passwords'
        TabOrder = 1
        WordWrap = True
        AutoSize = True
      end
      object ckAllowTabsInPasswords: TSDUCheckBox
        Left = 16
        Top = 104
        Width = 137
        Height = 13
        Caption = 'Allow ta&bs in passwords'
        TabOrder = 2
        WordWrap = True
        AutoSize = True
      end
      object seMRUMaxItemCount: TSpinEdit64
        Left = 16
        Top = 168
        Width = 145
        Height = 22
        Increment = 1
        MaxValue = 10
        TabOrder = 3
      end
      object ckAdvancedMountDlg: TSDUCheckBox
        Left = 16
        Top = 20
        Width = 221
        Height = 17
        Caption = 'Show advanced &options when opening'
        TabOrder = 4
        WordWrap = True
        AutoSize = True
      end
    end
    object tsAutorun: TTabSheet
      Caption = 'Autorun'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbAutorun: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 348
        Align = alClient
        Caption = 'Autorun pre/post-open/lock'
        TabOrder = 0
        object Label33: TLabel
          Left = 2
          Top = 135
          Width = 51
          Height = 13
          Caption = 'Post-&open:'
          FocusControl = edPostMountExe
        end
        object Label34: TLabel
          Left = 3
          Top = 175
          Width = 42
          Height = 13
          Caption = '&Pre-lock:'
          FocusControl = edPreDismountExe
        end
        object Label35: TLabel
          Left = 6
          Top = 216
          Width = 47
          Height = 13
          Caption = 'Post-&lock:'
          FocusControl = edPostDismountExe
        end
        object lblInstructions: TLabel
          Left = 2
          Top = 15
          Width = 407
          Height = 78
          Align = alTop
          Caption = 
            'LibreCrypt can be configured here to automatically run programs ' +
            #10#13'after opening, before locking, and after locking.'#10#13'Any post-op' +
            'en and pre-lock executables specified must use a relative path t' +
            'o the '#10#13'executable within the opened container.'#10#13'Executables wit' +
            'h spaces in their paths/names must be surrounded with double quo' +
            'tes.'#10#13'"%DRIVE" will be substituted with the drive letter of the ' +
            'opened drive'
        end
        object edPostMountExe: TEdit
          Left = 98
          Top = 132
          Width = 366
          Height = 21
          TabOrder = 0
          Text = 'Post Mount Exe'
        end
        object pbPostMountBrowse: TButton
          Left = 470
          Top = 132
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = pbPostMountBrowseClick
        end
        object pbPreDismountBrowse: TButton
          Left = 470
          Top = 172
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 3
          OnClick = pbPreDismountBrowseClick
        end
        object edPreDismountExe: TEdit
          Left = 98
          Top = 172
          Width = 366
          Height = 21
          TabOrder = 2
          Text = 'Pre Dismount Exe'
        end
        object pbPostDismountBrowse: TButton
          Left = 470
          Top = 212
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 5
          OnClick = pbPostDismountBrowseClick
        end
        object edPostDismountExe: TEdit
          Left = 98
          Top = 212
          Width = 366
          Height = 21
          TabOrder = 4
          Text = 'Post Dismount Exe'
        end
        object ckPrePostExeWarn: TSDUCheckBox
          Left = 5
          Top = 255
          Width = 161
          Height = 17
          Caption = '&Warn user if file is not found.'
          TabOrder = 6
          AutoSize = True
        end
      end
    end
  end
  object OpenDialog: TSDUOpenDialog
    Filter = 
      'Program files (*.exe, *.com, *.bat)|*.exe; *.bat; *.com|All file' +
      's|*.*'
    PreserveCWD = False
    Left = 384
    Top = 168
  end
end
