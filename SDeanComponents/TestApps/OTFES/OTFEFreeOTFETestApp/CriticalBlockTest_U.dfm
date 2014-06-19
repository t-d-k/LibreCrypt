object CriticalBlockTest_F: TCriticalBlockTest_F
  Left = 233
  Top = 232
  Caption = 'CriticalBlockTest_F'
  ClientHeight = 658
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    622
    658)
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 536
    Top = 620
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = pbCloseClick
  end
  object reReport: TRichEdit
    Left = 8
    Top = 436
    Width = 513
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 4
    Width = 605
    Height = 421
    Caption = 'COMMON'
    TabOrder = 2
    object Label1: TLabel
      Left = 40
      Top = 24
      Width = 45
      Height = 13
      Caption = 'Filename:'
    end
    object Label2: TLabel
      Left = 52
      Top = 52
      Width = 31
      Height = 13
      Caption = 'Offset:'
    end
    object Label3: TLabel
      Left = 36
      Top = 80
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object Label9: TLabel
      Left = 220
      Top = 52
      Width = 25
      Height = 13
      Caption = 'bytes'
    end
    object Label20: TLabel
      Left = 280
      Top = 52
      Width = 66
      Height = 13
      Caption = '&Key iterations:'
    end
    object edFilename: TEdit
      Left = 92
      Top = 20
      Width = 357
      Height = 21
      TabOrder = 0
      Text = 'C:\critical_data.dat'
    end
    object edUserPassword: TEdit
      Left = 92
      Top = 76
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'password'
    end
    object pbBrowse: TButton
      Left = 452
      Top = 20
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = pbBrowseClick
    end
    object pcSpecific: TPageControl
      Left = 12
      Top = 104
      Width = 581
      Height = 309
      ActivePage = tsWrite
      TabOrder = 3
      object tsWrite: TTabSheet
        Caption = 'WRITE'
        object Label6: TLabel
          Left = 4
          Top = 64
          Width = 73
          Height = 13
          Caption = 'Partition length:'
        end
        object Label7: TLabel
          Left = 364
          Top = 64
          Width = 63
          Height = 13
          Caption = 'Volume flags:'
        end
        object Label10: TLabel
          Left = 212
          Top = 64
          Width = 25
          Height = 13
          Caption = 'bytes'
        end
        object Label8: TLabel
          Left = 24
          Top = 92
          Width = 55
          Height = 13
          Caption = 'Master key:'
        end
        object Label11: TLabel
          Left = 20
          Top = 152
          Width = 57
          Height = 13
          Caption = 'Hash driver:'
        end
        object Label12: TLabel
          Left = 20
          Top = 172
          Width = 58
          Height = 13
          Caption = 'Hash GUID:'
        end
        object Label13: TLabel
          Left = 12
          Top = 200
          Width = 65
          Height = 13
          Caption = 'Cypher driver:'
        end
        object Label14: TLabel
          Left = 12
          Top = 220
          Width = 66
          Height = 13
          Caption = 'Cypher GUID:'
        end
        object Label15: TLabel
          Left = 12
          Top = 36
          Width = 67
          Height = 13
          Caption = 'Random data:'
        end
        object Label4: TLabel
          Left = 56
          Top = 8
          Width = 21
          Height = 13
          Caption = 'Salt:'
        end
        object Label18: TLabel
          Left = 464
          Top = 152
          Width = 54
          Height = 13
          Caption = 'Drive letter:'
        end
        object Label19: TLabel
          Left = 464
          Top = 172
          Width = 103
          Height = 13
          Caption = '(Blank for use default)'
        end
        object Label21: TLabel
          Left = 24
          Top = 120
          Width = 51
          Height = 13
          Caption = 'Volume IV:'
        end
        object sePartitionLength: TSpinEdit64
          Left = 84
          Top = 60
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 0
          Value = 1048576
        end
        object seVolumeFlags: TSpinEdit64
          Left = 444
          Top = 60
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 1
          Value = 1
        end
        object edMasterKey: TEdit
          Left = 84
          Top = 88
          Width = 481
          Height = 21
          TabOrder = 2
          Text = 'abcBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBcba'
        end
        object edHashDriver: TEdit
          Left = 84
          Top = 148
          Width = 237
          Height = 21
          TabOrder = 3
          Text = '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-000000000009}'
        end
        object edHashGUID: TEdit
          Left = 84
          Top = 168
          Width = 237
          Height = 21
          TabOrder = 4
          Text = '{00000000-0000-0000-0000-00000000000a}'
        end
        object edCypherDriver: TEdit
          Left = 84
          Top = 196
          Width = 237
          Height = 21
          TabOrder = 5
          Text = '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000010001}'
        end
        object edCypherGUID: TEdit
          Left = 84
          Top = 216
          Width = 237
          Height = 21
          TabOrder = 6
          Text = '{00000000-0000-0000-0000-000000010004}'
        end
        object edRandomData: TEdit
          Left = 84
          Top = 32
          Width = 481
          Height = 21
          TabOrder = 7
          Text = 
            'xyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzx' +
            'yzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxy' +
            'zxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyz' +
            'xyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyzxyz'
        end
        object edSalt: TEdit
          Left = 84
          Top = 4
          Width = 121
          Height = 21
          TabOrder = 8
        end
        object pbWriteCriticalData: TButton
          Left = 234
          Top = 248
          Width = 101
          Height = 25
          Caption = 'pbWriteCriticalData'
          TabOrder = 9
          OnClick = pbWriteCriticalDataClick
        end
        object edDriveLetter: TEdit
          Left = 528
          Top = 148
          Width = 37
          Height = 21
          TabOrder = 10
          Text = 'Q'
        end
        object edVolumeIV: TEdit
          Left = 84
          Top = 116
          Width = 481
          Height = 21
          TabOrder = 11
          Text = 'xyzVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVzyx'
        end
      end
      object tsRead: TTabSheet
        Caption = 'READ'
        ImageIndex = 1
        object Label5: TLabel
          Left = 228
          Top = 8
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object Label16: TLabel
          Left = 40
          Top = 8
          Width = 53
          Height = 13
          Caption = 'Salt length:'
        end
        object seSaltLength: TSpinEdit64
          Left = 100
          Top = 4
          Width = 121
          Height = 22
          Increment = 1
          TabOrder = 0
        end
        object pbReadCriticalData: TButton
          Left = 250
          Top = 212
          Width = 101
          Height = 25
          Caption = 'pbReadCriticalData'
          TabOrder = 1
          OnClick = pbReadCriticalDataClick
        end
      end
    end
    object pbCreateFile: TButton
      Left = 484
      Top = 16
      Width = 105
      Height = 25
      Caption = 'pbCreateFile'
      TabOrder = 4
      OnClick = pbCreateFileClick
    end
    object pbReadRawData: TButton
      Left = 484
      Top = 48
      Width = 105
      Height = 25
      Caption = 'pbReadRawData'
      TabOrder = 5
      OnClick = pbReadRawDataClick
    end
    object se64Offset: TSpinEdit64
      Left = 92
      Top = 48
      Width = 121
      Height = 22
      Increment = 1
      TabOrder = 6
    end
    object pbWriteRawData: TButton
      Left = 484
      Top = 76
      Width = 105
      Height = 25
      Caption = 'pbWriteRawData'
      TabOrder = 7
      OnClick = pbWriteRawDataClick
    end
    object seKeyIterations: TSpinEdit64
      Left = 352
      Top = 48
      Width = 121
      Height = 22
      Increment = 1
      TabOrder = 8
    end
  end
  object pbClear: TButton
    Left = 536
    Top = 436
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = pbClearClick
  end
  object OpenDialog1: TOpenDialog
    Left = 436
    Top = 36
  end
end
