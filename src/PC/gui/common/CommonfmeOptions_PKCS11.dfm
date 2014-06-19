inherited fmeOptions_PKCS11: TfmeOptions_PKCS11
  Width = 492
  Height = 289
  object gbPKCS11: TGroupBox
    Left = 16
    Top = 16
    Width = 457
    Height = 241
    Caption = 'Security Token/Smartcard support'
    TabOrder = 0
    object lblLibrary: TLabel
      Left = 36
      Top = 52
      Width = 82
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
      Left = 232
      Top = 76
      Width = 75
      Height = 25
      Caption = '&Verify'
      TabOrder = 2
      OnClick = pbVerifyClick
    end
    object gbPKCS11AutoActions: TGroupBox
      Left = 16
      Top = 120
      Width = 425
      Height = 105
      Caption = 'Automount/dismount'
      TabOrder = 3
      object lblAutoMountVolume: TLabel
        Left = 32
        Top = 48
        Width = 38
        Height = 13
        Caption = 'V&olume:'
      end
      object ckPKCS11AutoDismount: TCheckBox
        Left = 12
        Top = 72
        Width = 397
        Height = 17
        Caption = 'Auto &dismount PKCS#11 volumes when associated token is removed'
        TabOrder = 1
        OnClick = ControlChanged
      end
      object ckPKCS11AutoMount: TCheckBox
        Left = 12
        Top = 20
        Width = 397
        Height = 17
        Caption = 'Auto &mount specified volume on token insertion'
        TabOrder = 0
        OnClick = ControlChanged
      end
      object OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect
        Left = 104
        Top = 44
        Width = 310
        Height = 21
        TabOrder = 2
        SelectFor = fndOpen
        AllowPartitionSelect = True
        DesignSize = (
          310
          21)
      end
    end
    object feLibFilename: TSDUFilenameEdit
      Left = 148
      Top = 48
      Width = 293
      Height = 21
      Constraints.MaxHeight = 21
      Constraints.MinHeight = 21
      TabOrder = 1
      TabStop = False
      FilterIndex = 0
      OnChange = ControlChanged
      DesignSize = (
        293
        21)
    end
    object pbAutoDetect: TButton
      Left = 148
      Top = 76
      Width = 75
      Height = 25
      Caption = '&Autodetect'
      TabOrder = 4
      OnClick = pbAutoDetectClick
    end
  end
end
