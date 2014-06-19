inherited frmCDBDump_LUKS: TfrmCDBDump_LUKS
  Left = 100
  Top = 400
  ClientWidth = 605
  PixelsPerInch = 96
  TextHeight = 13
  inherited pbOK: TButton
    Left = 221
  end
  inherited pbCancel: TButton
    Left = 309
  end
  inherited GroupBox1: TGroupBox
    Width = 589
    Height = 221
    inherited Label2: TLabel
      FocusControl = OTFEFreeOTFELUKSKeyOrKeyfileEntry1
    end
    object lblOptional: TLabel [2]
      Left = 8
      Top = 68
      Width = 43
      Height = 13
      Caption = '(optional)'
    end
    inherited OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect
      Width = 465
    end
    object ckBaseIVCypherOnHashLength: TCheckBox
      Left = 8
      Top = 192
      Width = 177
      Height = 17
      Caption = 'Base IV cypher on hash length'
      TabOrder = 2
    end
    object OTFEFreeOTFELUKSKeyOrKeyfileEntry1: TOTFEFreeOTFELUKSKeyOrKeyfileEntry
      Left = 116
      Top = 48
      Width = 464
      Height = 138
      TabOrder = 1
    end
  end
  inherited GroupBox2: TGroupBox
    Top = 236
    Width = 585
    inherited feDumpFilename: TSDUFilenameEdit
      Width = 457
    end
  end
end
