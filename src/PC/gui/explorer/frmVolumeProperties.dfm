inherited frmVolumeProperties: TfrmVolumeProperties
  Caption = 'frmPropertiesDialog_Volume'
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited tsGeneral: TTabSheet
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inherited lblSizeOnDisk: TLabel
        Top = 376
        ExplicitTop = 376
      end
      inherited lblSize: TLabel
        Top = 352
        ExplicitTop = 352
      end
      inherited lblLocation: TLabel
        Top = 328
        ExplicitTop = 328
      end
      inherited edLocation: TLabel
        Top = 328
        ExplicitTop = 328
      end
      inherited edSizeOnDisk: TLabel
        Top = 376
        ExplicitTop = 376
      end
      inherited edSize: TLabel
        Top = 352
        ExplicitTop = 352
      end
      object lblFileSystem: TLabel [10]
        Left = 12
        Top = 88
        Width = 57
        Height = 13
        Caption = 'File system:'
        FocusControl = edFileSystem
      end
      object edFileSystem: TLabel [11]
        Left = 108
        Top = 88
        Width = 63
        Height = 13
        Caption = 'edFileSystem'
      end
      object lblUsedSpace: TLabel [12]
        Left = 32
        Top = 132
        Width = 59
        Height = 13
        Caption = 'Used space:'
        FocusControl = edUsedSpace_Bytes
      end
      object edUsedSpace_Bytes: TLabel [13]
        Left = 104
        Top = 132
        Width = 98
        Height = 13
        Alignment = taRightJustify
        Caption = 'edUsedSpace_Bytes'
      end
      object edUsedSpace_UsefulUnits: TLabel [14]
        Left = 256
        Top = 132
        Width = 125
        Height = 13
        Alignment = taRightJustify
        Caption = 'edUsedSpace_UsefulUnits'
      end
      object lblFreeSpace: TLabel [15]
        Left = 32
        Top = 156
        Width = 57
        Height = 13
        Caption = 'Free space:'
        FocusControl = edFreeSpace_Bytes
      end
      object edFreeSpace_Bytes: TLabel [16]
        Left = 104
        Top = 156
        Width = 96
        Height = 13
        Alignment = taRightJustify
        Caption = 'edFreeSpace_Bytes'
      end
      object edFreeSpace_UsefulUnits: TLabel [17]
        Left = 256
        Top = 156
        Width = 123
        Height = 13
        Alignment = taRightJustify
        Caption = 'edFreeSpace_UsefulUnits'
      end
      object lblCapacity: TLabel [18]
        Left = 32
        Top = 196
        Width = 46
        Height = 13
        Caption = 'Capacity:'
        FocusControl = edCapacity_Bytes
      end
      object edCapacity_Bytes: TLabel [19]
        Left = 104
        Top = 196
        Width = 87
        Height = 13
        Alignment = taRightJustify
        Caption = 'edCapacity_Bytes'
      end
      object edCapacity_UsefulUnits: TLabel [20]
        Left = 256
        Top = 196
        Width = 114
        Height = 13
        Alignment = taRightJustify
        Caption = 'edCapacity_UsefulUnits'
      end
      object chtSpaceChart: TGauge [21]
        Left = 102
        Top = 216
        Width = 136
        Height = 71
        BorderStyle = bsNone
        Kind = gkPie
        Progress = 0
      end
      object Panel1: TPanel
        Left = 8
        Top = 108
        Width = 317
        Height = 9
        Caption = 'Panel1'
        TabOrder = 6
      end
      object pnlColorFreeSpace: TPanel
        Left = 10
        Top = 155
        Width = 16
        Height = 16
        Caption = 'pnlColorUsedSpace'
        TabOrder = 7
      end
      object pnlColorUsedSpace: TPanel
        Left = 10
        Top = 130
        Width = 16
        Height = 16
        Caption = 'pnlColorUsedSpace'
        TabOrder = 8
      end
      object Panel4: TPanel
        Left = 8
        Top = 180
        Width = 317
        Height = 9
        Caption = 'Panel4'
        TabOrder = 9
      end
    end
    object tsTools: TTabSheet
      Caption = 'Tools'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbErrorChecking: TGroupBox
        Left = 12
        Top = 8
        Width = 317
        Height = 93
        Caption = 'Error-checking'
        TabOrder = 0
        object Label1: TLabel
          Left = 48
          Top = 24
          Width = 169
          Height = 26
          Caption = 'This option will check the container for errors.'
          WordWrap = True
        end
        object imgScandisk: TImage
          Left = 8
          Top = 24
          Width = 32
          Height = 32
        end
        object pbErrorCheckNow: TButton
          Left = 204
          Top = 56
          Width = 99
          Height = 25
          Caption = 'Check Now...'
          TabOrder = 0
          OnClick = pbErrorCheckNowClick
        end
      end
    end
  end
end
