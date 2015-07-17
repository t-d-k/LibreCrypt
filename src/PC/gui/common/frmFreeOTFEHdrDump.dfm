inherited frmFreeOTFEHdrDump: TfrmFreeOTFEHdrDump
  PixelsPerInch = 96
  TextHeight = 13
  inherited GroupBox1: TGroupBox
    object lblOffset: TLabel [1]
      Left = 8
      Top = 124
      Width = 31
      Height = 13
      Caption = '&Offset:'
    end
    object lblSaltLengthBits: TLabel [2]
      Left = 244
      Top = 152
      Width = 16
      Height = 13
      Caption = 'bits'
    end
    object lblSaltLength: TLabel [3]
      Left = 8
      Top = 152
      Width = 53
      Height = 13
      Caption = '&Salt length:'
      FocusControl = seSaltLength
    end
    object lblKeyIterations: TLabel [4]
      Left = 8
      Top = 180
      Width = 66
      Height = 13
      Caption = '&Key iterations:'
      FocusControl = seKeyIterations
    end
    object seSaltLength: TSpinEdit64
      Left = 116
      Top = 148
      Width = 121
      Height = 22
      Increment = 1
      TabOrder = 2
    end
    object seKeyIterations: TSpinEdit64
      Left = 116
      Top = 177
      Width = 121
      Height = 22
      Increment = 1
      TabOrder = 3
    end
    object se64UnitOffset: TSDUSpin64Unit_Storage
      Left = 116
      Top = 120
      Width = 226
      Height = 29
      TabOrder = 1
      Units.Strings = (
        'bytes'
        'KB'
        'MB'
        'GB'
        'TB')
      SelectedUnits = 'bytes'
      MaxLength = 0
      ReadOnly = False
    end
    inline frmePassword1: TfrmePassword
      Left = 8
      Top = 47
      Width = 454
      Height = 71
      TabOrder = 4
      ExplicitLeft = 8
      ExplicitTop = 47
      ExplicitWidth = 454
      ExplicitHeight = 71
      inherited lblKeyPhrase: TLabel
        Left = 0
        Width = 56
        ExplicitLeft = 0
        ExplicitWidth = 56
      end
      inherited preUserKeyFirst: TOTFEFreeOTFE_PasswordRichEdit
        Left = 108
        Width = 333
        Height = 65
        ExplicitLeft = 108
        ExplicitWidth = 333
        ExplicitHeight = 65
      end
    end
  end
end
