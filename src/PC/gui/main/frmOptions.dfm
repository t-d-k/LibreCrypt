inherited frmOptions: TfrmOptions
  ClientHeight = 540
  ExplicitWidth = 519
  ExplicitHeight = 568
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblSettingsLocation: TLabel
    Left = 26
    Top = 422
    ExplicitLeft = 26
    ExplicitTop = 422
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
    Left = 144
    Top = 419
    Width = 273
    ExplicitLeft = 144
    ExplicitTop = 419
    ExplicitWidth = 273
  end
  inherited ckAssociateFiles: TSDUCheckBox
    Left = 22
    Top = 451
    Width = 395
    OnClick = ControlChanged
    ExplicitLeft = 22
    ExplicitTop = 451
    ExplicitWidth = 395
  end
  inherited pcOptions: TPageControl
    Height = 396
    ActivePage = tsAutorun
    ExplicitHeight = 396
    inherited tsGeneral: TTabSheet
      ExplicitHeight = 368
      inherited gbGeneralMain: TGroupBox
        Height = 368
        ExplicitHeight = 368
        inherited ckExploreAfterMount: TSDUCheckBox
          Width = 109
          Height = 13
          ExplicitWidth = 109
          ExplicitHeight = 13
        end
      end
    end
    object tsHotkeys: TTabSheet [1]
      Caption = 'Hotkeys'
      object gbHotkeys: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 368
        Align = alClient
        Caption = 'Hotkeys'
        TabOrder = 0
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
      object gbSystemTrayIcon: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 368
        Align = alClient
        Caption = 'System Tray Icon'
        TabOrder = 0
        object ckUseSystemTrayIcon: TSDUCheckBox
          Left = 12
          Top = 20
          Width = 245
          Height = 17
          Caption = '&Show system tray icon'
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
          Top = 72
          Width = 122
          Height = 13
          Caption = '&Close to system tray'
          TabOrder = 2
          AutoSize = True
        end
        object gbClickActions: TGroupBox
          Left = 22
          Top = 108
          Width = 469
          Height = 157
          Caption = 'Click actions'
          TabOrder = 0
          object Label3: TLabel
            Left = 12
            Top = 24
            Width = 251
            Height = 13
            Caption = 'Carry out the following action when the system tray is:'
          end
          object Label4: TLabel
            Left = 12
            Top = 47
            Width = 69
            Height = 13
            Caption = 'Single clicked:'
          end
          object Label5: TLabel
            Left = 12
            Top = 90
            Width = 74
            Height = 13
            Caption = 'Double clicked:'
          end
          object Label6: TLabel
            Left = 12
            Top = 118
            Width = 121
            Height = 13
            Caption = 'with the left mousebutton:'
          end
          object Label7: TLabel
            Left = 14
            Top = 137
            Width = 249
            Height = 13
            Caption = 'A double click will also perform the single click action'
          end
          object cbSingleClickAction: TComboBox
            Left = 251
            Top = 43
            Width = 185
            Height = 21
            Style = csDropDownList
            TabOrder = 0
          end
          object cbDbleClickAction: TComboBox
            Left = 251
            Top = 86
            Width = 185
            Height = 21
            Style = csDropDownList
            TabOrder = 1
          end
        end
      end
    end
    object tsMain: TTabSheet [3]
      Caption = 'Main'
      object gbAdvanced: TGroupBox
        Left = 0
        Top = 0
        Width = 505
        Height = 368
        Align = alClient
        Caption = 'Advanced'
        TabOrder = 0
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
          Width = 129
          Height = 26
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
      ExplicitHeight = 368
      inherited gbPKCS11: TGroupBox
        Height = 368
        ExplicitHeight = 368
      end
    end
    inherited tsAdvanced: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 505
      ExplicitHeight = 368
      inherited lblMRUMaxItemCountInst: TLabel
        Top = 165
        ExplicitTop = 165
      end
      inherited ckRevertVolTimestamps: TSDUCheckBox
        Top = 49
        ExplicitTop = 49
      end
      inherited ckAllowNewlinesInPasswords: TSDUCheckBox
        Top = 78
        ExplicitTop = 78
      end
      inherited ckAllowTabsInPasswords: TSDUCheckBox
        Top = 107
        ExplicitTop = 107
      end
      inherited seMRUMaxItemCount: TSpinEdit64
        Top = 162
        ExplicitTop = 162
      end
    end
    inherited tsAutorun: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 505
      ExplicitHeight = 368
      inherited gbAutorun: TGroupBox
        Height = 368
        ExplicitHeight = 368
        inherited Label33: TLabel
          Left = 3
          Top = 135
          ExplicitLeft = 3
          ExplicitTop = 135
        end
        inherited Label34: TLabel
          Left = 5
          Top = 175
          ExplicitLeft = 5
          ExplicitTop = 175
        end
        inherited Label35: TLabel
          Left = 6
          Top = 216
          ExplicitLeft = 6
          ExplicitTop = 216
        end
        inherited edPostMountExe: TEdit
          Left = 98
          Top = 132
          ExplicitLeft = 98
          ExplicitTop = 132
        end
        inherited pbPostMountBrowse: TButton
          Left = 470
          Top = 132
          OnClick = nil
          ExplicitLeft = 470
          ExplicitTop = 132
        end
        inherited pbPreDismountBrowse: TButton
          Left = 470
          Top = 172
          OnClick = nil
          ExplicitLeft = 470
          ExplicitTop = 172
        end
        inherited edPreDismountExe: TEdit
          Left = 98
          Top = 172
          ExplicitLeft = 98
          ExplicitTop = 172
        end
        inherited pbPostDismountBrowse: TButton
          Left = 470
          Top = 212
          OnClick = nil
          ExplicitLeft = 470
          ExplicitTop = 212
        end
        inherited edPostDismountExe: TEdit
          Left = 98
          Top = 212
          ExplicitLeft = 98
          ExplicitTop = 212
        end
        inherited ckPrePostExeWarn: TSDUCheckBox
          Left = 5
          Top = 255
          ExplicitLeft = 5
          ExplicitTop = 255
        end
      end
    end
  end
  object ckLaunchAtStartup: TSDUCheckBox [7]
    Left = 22
    Top = 470
    Width = 395
    Height = 13
    Caption = 'Start %s at system startup'
    TabOrder = 5
    OnClick = ckLaunchAtStartupClick
    AutoSize = True
  end
  object ckLaunchMinimisedAtStartup: TSDUCheckBox [8]
    Left = 40
    Top = 489
    Width = 377
    Height = 13
    Caption = 'Start minimised at system startup'
    TabOrder = 6
    OnClick = ControlChanged
    AutoSize = True
  end
  inherited OpenDialog: TSDUOpenDialog
    Left = 440
    Top = 80
  end
end
