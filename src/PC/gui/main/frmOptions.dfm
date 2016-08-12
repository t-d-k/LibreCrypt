inherited frmOptions: TfrmOptions
  ClientHeight = 482
  ClientWidth = 508
  ExplicitWidth = 514
  ExplicitHeight = 510
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblSettingsLocation: TLabel
    Left = 8
    Top = 412
    ExplicitLeft = 8
    ExplicitTop = 412
  end
  inherited imgNoSaveWarning: TImage
    Left = 417
    Top = 411
    Height = 19
    ExplicitLeft = 417
    ExplicitTop = 411
    ExplicitHeight = 19
  end
  inherited pbOK: TButton
    Left = 334
    Top = 450
    ExplicitLeft = 334
    ExplicitTop = 508
  end
  inherited pbCancel: TButton
    Left = 415
    Top = 450
    ExplicitLeft = 415
    ExplicitTop = 508
  end
  inherited cbSettingsLocation: TComboBox
    Left = 150
    Top = 409
    Width = 259
    ExplicitLeft = 150
    ExplicitTop = 409
    ExplicitWidth = 259
  end
  inherited pcOptions: TPageControl
    Width = 508
    Height = 396
    ActivePage = tcSystemTray
    ExplicitWidth = 508
    ExplicitHeight = 396
    inherited tsGeneral: TTabSheet
      ExplicitWidth = 500
      ExplicitHeight = 368
      inherited gbGeneralMain: TGroupBox
        Width = 500
        Height = 368
        ExplicitWidth = 500
        ExplicitHeight = 368
        inherited ckExploreAfterMount: TSDUCheckBox
          Width = 109
          Height = 13
          ExplicitWidth = 109
          ExplicitHeight = 13
        end
        inherited ckShowPasswords: TSDUCheckBox
          Top = 150
          ExplicitTop = 150
        end
        inherited ckPromptMountSuccessful: TSDUCheckBox
          Top = 179
          ExplicitTop = 179
        end
        inherited ckDisplayToolbarLarge: TSDUCheckBox
          Left = 16
          ExplicitLeft = 16
        end
        inherited ckDisplayToolbarCaptions: TSDUCheckBox
          Left = 16
          ExplicitLeft = 16
        end
        inherited ckStoreLayout: TSDUCheckBox
          Top = 208
          ExplicitTop = 208
        end
        inherited ckAssociateFiles: TSDUCheckBox
          Top = 237
          ExplicitTop = 237
        end
      end
    end
    object tsHotkeys: TTabSheet [1]
      Caption = 'Hotkeys'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 505
      ExplicitHeight = 0
      object gbHotkeys: TGroupBox
        Left = 0
        Top = 0
        Width = 500
        Height = 368
        Align = alClient
        Caption = 'Hotkeys'
        TabOrder = 0
        ExplicitWidth = 505
        object Label1: TLabel
          Left = 12
          Top = 47
          Width = 37
          Height = 13
          Caption = '&Hotkey:'
          FocusControl = hkDismount
        end
        object Label2: TLabel
          Left = 12
          Top = 116
          Width = 37
          Height = 13
          Caption = 'H&otkey:'
          FocusControl = hkDismountEmerg
        end
        object hkDismount: THotKey
          Left = 140
          Top = 47
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
          Caption = '&Enable lock-all hotkey'
          TabOrder = 3
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckHotkeyDismountEmerg: TSDUCheckBox
          Left = 12
          Top = 93
          Width = 269
          Height = 17
          Caption = 'E&nable emergency lock-all hotkey'
          TabOrder = 2
          OnClick = ControlChanged
          AutoSize = True
        end
        object hkDismountEmerg: THotKey
          Left = 140
          Top = 116
          Width = 141
          Height = 19
          HotKey = 32833
          InvalidKeys = [hcNone]
          TabOrder = 0
        end
      end
    end
    object tcSystemTray: TTabSheet [2]
      Caption = 'System Tray'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 505
      ExplicitHeight = 0
      object gbSystemTrayIcon: TGroupBox
        Left = 0
        Top = 0
        Width = 500
        Height = 368
        Align = alClient
        Caption = 'System Tray Icon'
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 3
        object Label3: TLabel
          Left = 13
          Top = 115
          Width = 251
          Height = 13
          Caption = 'Carry out the following action when the system tray is:'
        end
        object Label4: TLabel
          Left = 13
          Top = 145
          Width = 69
          Height = 13
          Caption = 'Single clicked:'
        end
        object Label5: TLabel
          Left = 13
          Top = 174
          Width = 74
          Height = 13
          Caption = 'Double clicked:'
        end
        object Label6: TLabel
          Left = 13
          Top = 203
          Width = 121
          Height = 13
          Caption = 'with the left mousebutton:'
        end
        object Label7: TLabel
          Left = 13
          Top = 233
          Width = 249
          Height = 13
          Caption = 'A double click will also perform the single click action'
        end
        object ckUseSystemTrayIcon: TSDUCheckBox
          Left = 13
          Top = 28
          Width = 245
          Height = 17
          Caption = '&Show system tray icon'
          TabOrder = 2
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckMinToIcon: TSDUCheckBox
          Left = 13
          Top = 57
          Width = 137
          Height = 13
          Caption = '&Mimimise to system tray'
          TabOrder = 0
          AutoSize = True
        end
        object ckCloseToIcon: TSDUCheckBox
          Left = 13
          Top = 86
          Width = 122
          Height = 13
          Caption = '&Close to system tray'
          TabOrder = 1
          AutoSize = True
        end
        object cbSingleClickAction: TComboBox
          Left = 192
          Top = 144
          Width = 294
          Height = 21
          Style = csDropDownList
          TabOrder = 3
        end
        object cbDbleClickAction: TComboBox
          Left = 192
          Top = 171
          Width = 294
          Height = 21
          Style = csDropDownList
          TabOrder = 4
        end
      end
    end
    object tsMain: TTabSheet [3]
      Caption = 'Main'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 505
      ExplicitHeight = 0
      object gbAdvanced: TGroupBox
        Left = 0
        Top = 0
        Width = 500
        Height = 368
        Align = alClient
        Caption = 'Advanced'
        TabOrder = 0
        ExplicitWidth = 505
        object lblDragDrop: TLabel
          Left = 12
          Top = 231
          Width = 273
          Height = 13
          Caption = 'Assume files &dragged onto LibreCrypt are encrypted using:'
          FocusControl = cbDragDrop
          WordWrap = True
        end
        object lblOnNormalDismountFail: TLabel
          Left = 12
          Top = 173
          Width = 176
          Height = 13
          Caption = 'If u&nable to lock a container normally:'
          FocusControl = cbOnNormalDismountFail
          WordWrap = True
        end
        object lblDefaultMountAs: TLabel
          Left = 12
          Top = 202
          Width = 87
          Height = 13
          Caption = 'Default open t&ype:'
          FocusControl = cbDefaultMountAs
          WordWrap = True
        end
        object lblOnExitWhenMounted: TLabel
          Left = 12
          Top = 116
          Width = 162
          Height = 13
          Caption = 'If containers &opened when exiting:'
          FocusControl = cbOnExitWhenMounted
          WordWrap = True
        end
        object lblOnExitWhenPortableMode: TLabel
          Left = 12
          Top = 144
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
          Left = 339
          Top = 228
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = ControlChanged
        end
        object ckAutoStartPortable: TSDUCheckBox
          Left = 12
          Top = 66
          Width = 413
          Height = 30
          Caption = 
            'Autostart &portable mode if LibreCrypt drivers not installed/run' +
            'ning'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 3
          WordWrap = True
          OnClick = ControlChanged
          AutoSize = True
        end
        object ckWarnBeforeForcedDismount: TSDUCheckBox
          Left = 12
          Top = 47
          Width = 169
          Height = 13
          Caption = 'Warn before &forced locks'
          TabOrder = 2
          WordWrap = True
          OnClick = ControlChanged
          AutoSize = True
        end
        object cbOnNormalDismountFail: TComboBox
          Left = 339
          Top = 170
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 7
          OnChange = ControlChanged
        end
        object cbDefaultMountAs: TComboBox
          Left = 339
          Top = 199
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 4
          OnChange = ControlChanged
        end
        object cbOnExitWhenMounted: TComboBox
          Left = 339
          Top = 113
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 5
          OnChange = ControlChanged
        end
        object cbOnExitWhenPortableMode: TComboBox
          Left = 339
          Top = 141
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 6
          OnChange = ControlChanged
        end
      end
    end
    inherited tsPKCS11: TTabSheet
      ExplicitWidth = 500
      ExplicitHeight = 368
      inherited gbPKCS11: TGroupBox
        Width = 500
        Height = 368
        ExplicitWidth = 500
        ExplicitHeight = 368
      end
    end
    inherited tsAdvanced: TTabSheet
      ExplicitHeight = 368
      inherited lblMRUMaxItemCount: TLabel
        Left = 18
        Top = 106
        ExplicitLeft = 18
        ExplicitTop = 106
      end
      inherited lblMRUMaxItemCountInst: TLabel
        Top = 128
        ExplicitTop = 128
      end
      inherited ckRevertVolTimestamps: TSDUCheckBox
        Left = 18
        Top = 17
        ExplicitLeft = 18
        ExplicitTop = 17
      end
      inherited ckAllowNewlinesInPasswords: TSDUCheckBox
        Left = 18
        Top = 44
        ExplicitLeft = 18
        ExplicitTop = 44
      end
      inherited ckAllowTabsInPasswords: TSDUCheckBox
        Left = 18
        Top = 71
        ExplicitLeft = 18
        ExplicitTop = 71
      end
      inherited seMRUMaxItemCount: TSpinEdit64
        Top = 125
        ExplicitTop = 125
      end
      object ckLaunchAtStartup: TSDUCheckBox
        Left = 18
        Top = 168
        Width = 395
        Height = 13
        Caption = 'Start %s at system startup'
        TabOrder = 4
        OnClick = ckLaunchAtStartupClick
        AutoSize = True
      end
      object ckLaunchMinimisedAtStartup: TSDUCheckBox
        Left = 18
        Top = 195
        Width = 377
        Height = 13
        Caption = 'Start minimised at system startup'
        TabOrder = 5
        OnClick = ControlChanged
        AutoSize = True
      end
    end
    inherited tsAutorun: TTabSheet
      ExplicitHeight = 368
      inherited gbAutorun: TGroupBox
        Width = 500
        Height = 368
        ExplicitWidth = 500
        ExplicitHeight = 368
        inherited Label33: TLabel
          Left = 3
          ExplicitLeft = 3
        end
        inherited Label34: TLabel
          Left = 5
          ExplicitLeft = 5
        end
        inherited pbPostMountBrowse: TButton
          OnClick = nil
        end
        inherited pbPreDismountBrowse: TButton
          OnClick = nil
        end
        inherited pbPostDismountBrowse: TButton
          OnClick = nil
        end
      end
    end
  end
  inherited OpenDialog: TSDUOpenDialog
    Left = 440
    Top = 80
  end
end
