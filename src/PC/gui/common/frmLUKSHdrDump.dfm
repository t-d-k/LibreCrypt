inherited frmLUKSHdrDump: TfrmLUKSHdrDump
  Left = 100
  Top = 400
  ClientHeight = 395
  ClientWidth = 485
  ExplicitWidth = 491
  ExplicitHeight = 423
  PixelsPerInch = 96
  TextHeight = 13
  inherited pbOK: TButton
    Left = 314
    Top = 362
    Anchors = [akRight, akBottom]
    OnClick = pbOKClick
    ExplicitLeft = 314
    ExplicitTop = 362
  end
  inherited pbCancel: TButton
    Left = 402
    Top = 362
    Anchors = [akRight, akBottom]
    ExplicitLeft = 402
    ExplicitTop = 362
  end
  inherited GroupBox1: TGroupBox
    Width = 469
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 469
    ExplicitHeight = 273
    inherited OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
      Height = 29
      TabOrder = 1
      ExplicitHeight = 29
    end
    inline frmeLUKSKeyOrKeyfileEntry1: TfrmeLUKSKeyOrKeyfileEntry
      Left = 9
      Top = 71
      Width = 457
      Height = 186
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      ExplicitLeft = 9
      ExplicitTop = 71
      ExplicitWidth = 457
      ExplicitHeight = 186
      inherited lblTreatNewlineAsEOF_1: TLabel
        Left = 107
        Top = 143
        ExplicitLeft = 107
        ExplicitTop = 143
      end
      inherited rbKeyFromUser: TRadioButton
        Left = 2
        Top = 3
        Width = 99
        Anchors = [akTop]
        ExplicitLeft = 2
        ExplicitTop = 3
        ExplicitWidth = 99
      end
      inherited rbKeyFromKeyfile: TRadioButton
        Top = 86
        ExplicitTop = 86
      end
      inherited feKeyfile: TSDUFilenameEdit
        Left = 107
        Top = 82
        Width = 334
        ExplicitLeft = 107
        ExplicitTop = 82
        ExplicitWidth = 334
      end
      inherited ckKeyfileContainsASCII: TSDUCheckBox
        Left = 107
        Top = 109
        ExplicitLeft = 107
        ExplicitTop = 109
      end
      inherited cbNewlineType: TComboBox
        Left = 184
        Top = 135
        ExplicitLeft = 184
        ExplicitTop = 135
      end
      inherited ckBaseIVCypherOnHashLength: TCheckBox
        Left = 107
        Top = 162
        ExplicitLeft = 107
        ExplicitTop = 162
      end
      inherited preUserKey: TPasswordRichEdit
        Left = 107
        Width = 319
        ExplicitLeft = 107
        ExplicitWidth = 319
      end
    end
  end
  inherited GroupBox2: TGroupBox
    Top = 299
    Width = 469
    Anchors = [akLeft, akRight, akBottom]
    ExplicitTop = 299
    ExplicitWidth = 469
    inherited feDumpFilename: TSDUFilenameEdit
      Left = 103
      Width = 330
      Anchors = [akLeft, akRight, akBottom]
      ExplicitLeft = 103
      ExplicitWidth = 330
    end
  end
end
