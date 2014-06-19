inherited frmCDBDump_FreeOTFE: TfrmCDBDump_FreeOTFE
  PixelsPerInch = 96
  TextHeight = 13
  inherited GroupBox1: TGroupBox
    inherited Label2: TLabel
      FocusControl = preUserKey
    end
    object lblOffset: TLabel [2]
      Left = 8
      Top = 124
      Width = 31
      Height = 13
      Caption = '&Offset:'
    end
    object lblSaltLengthBits: TLabel [3]
      Left = 244
      Top = 152
      Width = 16
      Height = 13
      Caption = 'bits'
    end
    object lblSaltLength: TLabel [4]
      Left = 8
      Top = 152
      Width = 53
      Height = 13
      Caption = '&Salt length:'
      FocusControl = seSaltLength
    end
    object lblKeyIterations: TLabel [5]
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
      TabOrder = 3
    end
    object seKeyIterations: TSpinEdit64
      Left = 116
      Top = 177
      Width = 121
      Height = 22
      Increment = 1
      TabOrder = 4
    end
    object se64UnitOffset: TSDUSpin64Unit_Storage
      Left = 116
      Top = 120
      Width = 226
      Height = 29
      TabOrder = 2
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
    object preUserKey: TPasswordRichEdit
      Left = 116
      Top = 48
      Width = 337
      Height = 65
      Lines.Strings = (
        'preUserKey')
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
end
