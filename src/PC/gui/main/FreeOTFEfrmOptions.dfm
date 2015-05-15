inherited frmOptions_FreeOTFE: TfrmOptions
  ClientHeight = 540
  OnCreate = FormCreate
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
    ExplicitTop = 444
  end
  inherited pcOptions: TPageControl
    Height = 396
    ActivePage = tsAdvanced
    ExplicitHeight = 396
    inherited tsPKCS11: TTabSheet
      ExplicitHeight = 368
    end
    object tsGeneral: TTabSheet
      Caption = 'General'
      inline fmeOptions_FreeOTFEGeneral1: TfmeOptions_FreeOTFEGeneral
        Left = -12
        Top = -14
        Width = 504
        Height = 382
        TabOrder = 0
        ExplicitLeft = -12
        ExplicitTop = -14
        inherited gbGeneral: TGroupBox
          inherited lblDefaultDriveLetter: TLabel
            Width = 89
            ExplicitWidth = 89
          end
          inherited lblChkUpdatesFreq: TLabel
            Width = 90
            ExplicitWidth = 90
          end
        end
      end
    end
    object tsHotkeys: TTabSheet
      Caption = 'Hotkeys'
      inline fmeOptions_Hotkeys1: TfmeHotKeysOptions
        Left = 64
        Top = 28
        Width = 337
        Height = 174
        TabOrder = 0
        ExplicitLeft = 64
        ExplicitTop = 28
        inherited gbHotkeys: TGroupBox
          inherited Label1: TLabel
            Width = 37
            ExplicitWidth = 37
          end
          inherited Label2: TLabel
            Width = 37
            ExplicitWidth = 37
          end
        end
      end
    end
    object tcSystemTray: TTabSheet
      Caption = 'System Tray'
      inline fmeOptions_SystemTray1: TfmeSystemTrayOptions
        Left = 64
        Top = 36
        Width = 320
        Height = 240
        TabOrder = 0
        ExplicitLeft = 64
        ExplicitTop = 36
        ExplicitWidth = 320
        ExplicitHeight = 240
        inherited gbSystemTrayIcon: TGroupBox
          inherited ckMinToIcon: TSDUCheckBox
            Width = 130
            Caption = '&Minimise to system tray'
            ExplicitWidth = 130
          end
          inherited gbClickActions: TGroupBox
            inherited Label1: TLabel
              Width = 115
              ExplicitWidth = 115
            end
            inherited Label2: TLabel
              Width = 262
              ExplicitWidth = 262
            end
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
      inline fmeOptions_FreeOTFEAdvanced1: TfmeOptions_FreeOTFEAdvanced
        Left = -12
        Top = -14
        Width = 504
        Height = 382
        TabOrder = 0
        ExplicitLeft = -12
        ExplicitTop = -14
        inherited gbAdvanced: TGroupBox
          inherited lblDragDrop: TLabel
            Width = 127
            ExplicitWidth = 127
          end
          inherited lblMRUMaxItemCountInst: TLabel
            Width = 134
            ExplicitWidth = 134
          end
          inherited lblMRUMaxItemCount: TLabel
            Width = 158
            ExplicitWidth = 158
          end
          inherited lblOnNormalDismountFail: TLabel
            Width = 188
            ExplicitWidth = 188
          end
          inherited lblDefaultMountAs: TLabel
            Width = 92
            ExplicitWidth = 92
          end
          inherited lblOnExitWhenMounted: TLabel
            Width = 157
            ExplicitWidth = 157
          end
          inherited lblOnExitWhenPortableMode: TLabel
            Width = 152
            ExplicitWidth = 152
          end
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
    AutoSize = True
  end
end
