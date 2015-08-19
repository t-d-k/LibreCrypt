inherited frmOptions: TfrmOptions
  ClientHeight = 540
  ExplicitWidth = 519
  ExplicitHeight = 568
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblSettingsLocation: TLabel
    Top = 416
    ExplicitTop = 416
  end
  inherited imgNoSaveWarning: TImage
    Top = 414
    ExplicitTop = 414
  end
  inherited pbOK: TButton
    Top = 508
    ExplicitTop = 508
  end
  inherited pbCancel: TButton
    Top = 508
    ExplicitTop = 508
  end
  inherited cbSettingsLocation: TComboBox
    Top = 412
    ExplicitTop = 412
  end
  inherited ckAssociateFiles: TSDUCheckBox
    Top = 444
    OnClick = ControlChanged
    ExplicitTop = 444
  end
  inherited pcOptions: TPageControl
    Height = 396
    ActivePage = tcSystemTray
    ExplicitHeight = 396
    inherited tsPKCS11: TTabSheet
      ExplicitHeight = 368
      inherited gbPKCS11: TGroupBox
        Height = 368
        ExplicitHeight = 368
      end
    end
    object tsGeneral: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Left = 12
          Top = 44
          Width = 221
          Height = 17
          Caption = 'Display &toolbar'
          TabOrder = 11
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
        object Panel1: TPanel
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
          TabOrder = 2
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
          TabOrder = 3
        end
      end
    end
    object tsHotkeys: TTabSheet
      Caption = 'Hotkeys'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbHotkeys: TGroupBox
        Left = 0
        Top = 0
        Width = 492
        Height = 368
        Align = alClient
        Caption = 'Hotkeys'
        TabOrder = 0
        object Label1: TLabel
          Left = 32
          Top = 48
          Width = 37
          Height = 13
          Caption = '&Hotkey:'
          FocusControl = hkDismount
        end
        object Label2: TLabel
          Left = 32
          Top = 100
          Width = 37
          Height = 13
          Caption = 'H&otkey:'
          FocusControl = hkDismountEmerg
        end
        object hkDismount: THotKey
          Left = 124
          Top = 44
          Width = 141
          Height = 19
          HotKey = 32833
          InvalidKeys = [hcNone]
          TabOrder = 1
        end
        object ckHotkeyDismount: TSDUCheckBox
          Left = 12
          Top = 24
          Width = 217
          Height = 17
          Caption = '&Enable dismount all hotkey'
          TabOrder = 3
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckHotkeyDismountEmerg: TSDUCheckBox
          Left = 12
          Top = 76
          Width = 269
          Height = 17
          Caption = 'E&nable emergency dismount all hotkey'
          TabOrder = 2
          OnClick = ControlChanged
          AutoSize = True
        end
        object hkDismountEmerg: THotKey
          Left = 124
          Top = 96
          Width = 141
          Height = 19
          HotKey = 32833
          InvalidKeys = [hcNone]
          TabOrder = 0
        end
      end
    end
    object tcSystemTray: TTabSheet
      Caption = 'System Tray'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbSystemTrayIcon: TGroupBox
        Left = 0
        Top = 0
        Width = 492
        Height = 368
        Align = alClient
        Caption = 'System Tray Icon'
        TabOrder = 0
        ExplicitLeft = 12
        ExplicitTop = 20
        ExplicitWidth = 353
        ExplicitHeight = 257
        object ckUseSystemTrayIcon: TSDUCheckBox
          Left = 12
          Top = 20
          Width = 245
          Height = 17
          Caption = '&Display system tray icon'
          TabOrder = 3
          OnClick = ControlChanged
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
          object Label3: TLabel
            Left = 12
            Top = 24
            Width = 115
            Height = 13
            Caption = 'When the system tray is:'
          end
          object Label4: TLabel
            Left = 12
            Top = 88
            Width = 262
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
    object tsAutorun: TTabSheet
      Caption = 'Autorun'
      inline fmeOptions_Autorun1: TfmeAutorunOptions
        Left = 21
        Top = 10
        Width = 471
        Height = 358
        TabOrder = 0
        ExplicitLeft = 21
        ExplicitTop = 10
        inherited gbAutorun: TGroupBox
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
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Width = 127
          Height = 39
          Caption = 'Assume files &dragged onto LibreCrypt are encrypted using:'
          FocusControl = cbDragDrop
          WordWrap = True
        end
        object lblMRUMaxItemCountInst: TLabel
          Left = 280
          Top = 152
          Width = 134
          Height = 13
          Caption = '(Set to 0 to disable MRU list)'
        end
        object lblMRUMaxItemCount: TLabel
          Left = 264
          Top = 96
          Width = 158
          Height = 26
          Caption = '&Max. number of volumes to show in most recently used list:'
          FocusControl = seMRUMaxItemCount
          WordWrap = True
        end
        object lblOnNormalDismountFail: TLabel
          Left = 12
          Top = 288
          Width = 157
          Height = 26
          Caption = 'If u&nable to dismount a container normally:'
          FocusControl = cbOnNormalDismountFail
          WordWrap = True
        end
        object lblDefaultMountAs: TLabel
          Left = 264
          Top = 176
          Width = 92
          Height = 13
          Caption = 'Default mount t&ype:'
          FocusControl = cbDefaultMountAs
          WordWrap = True
        end
        object lblOnExitWhenMounted: TLabel
          Left = 12
          Top = 192
          Width = 157
          Height = 13
          Caption = 'If volumes &mounted when exiting:'
          FocusControl = cbOnExitWhenMounted
          WordWrap = True
        end
        object lblOnExitWhenPortableMode: TLabel
          Left = 12
          Top = 240
          Width = 152
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
          OnClick = ControlChanged
          AutoSize = True
        end
        object cbDragDrop: TComboBox
          Left = 280
          Top = 56
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = ControlChanged
        end
        object ckAutoStartPortable: TSDUCheckBox
          Left = 12
          Top = 112
          Width = 221
          Height = 30
          Caption = 
            'Autostart &portable mode if LibreCrypt drivers not installed/run' +
            'ning'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 6
          WordWrap = True
          OnClick = ControlChanged
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
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckRevertVolTimestamps: TSDUCheckBox
          Left = 12
          Top = 68
          Width = 221
          Height = 17
          Caption = '&Revert container timestamps on dismount'
          TabOrder = 4
          WordWrap = True
          OnClick = ControlChanged
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
          OnChange = ControlChanged
        end
        object ckWarnBeforeForcedDismount: TSDUCheckBox
          Left = 13
          Top = 93
          Width = 169
          Height = 13
          Caption = 'Warn before &forced dismounts'
          TabOrder = 5
          WordWrap = True
          OnClick = ControlChanged
          AutoSize = True
        end
        object cbOnNormalDismountFail: TComboBox
          Left = 28
          Top = 308
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 13
          OnChange = ControlChanged
        end
        object cbDefaultMountAs: TComboBox
          Left = 280
          Top = 195
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 10
          OnChange = ControlChanged
        end
        object cbOnExitWhenMounted: TComboBox
          Left = 28
          Top = 212
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 11
          OnChange = ControlChanged
        end
        object cbOnExitWhenPortableMode: TComboBox
          Left = 28
          Top = 260
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 12
          OnChange = ControlChanged
        end
        object ckAllowNewlinesInPasswords: TSDUCheckBox
          Left = 12
          Top = 148
          Width = 157
          Height = 13
          Caption = 'Allow &newlines in passwords'
          TabOrder = 8
          WordWrap = True
          OnClick = ControlChanged
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
          OnClick = ControlChanged
          AutoSize = True
        end
      end
    end
  end
  object ckLaunchAtStartup: TSDUCheckBox
    Left = 12
    Top = 464
    Width = 143
    Height = 13
    Caption = 'Start %s at system startup'
    TabOrder = 5
    OnClick = ckLaunchAtStartupClick
    AutoSize = True
  end
  object ckLaunchMinimisedAtStartup: TSDUCheckBox
    Left = 32
    Top = 484
    Width = 175
    Height = 13
    Caption = 'Start minimised at system startup'
    TabOrder = 6
    OnClick = ControlChanged
    AutoSize = True
  end
end
