object TfmeContainerSize: TTfmeContainerSize
  Left = 0
  Top = 0
  Width = 341
  Height = 202
  TabOrder = 0
  object lblPartitionDiskSize: TLabel
    Left = 113
    Top = 148
    Width = 58
    Height = 13
    Caption = 'Approx: %s'
  end
  object Label6: TLabel
    Left = 0
    Top = 148
    Width = 107
    Height = 13
    Caption = '&Size of new container:'
  end
  object lblInstructSizeCommon: TLabel
    Left = 0
    Top = 0
    Width = 341
    Height = 39
    Align = alTop
    Caption = 
      'Please enter the required size of the new container.'#10#13'This value' +
      ' must be more than 1 MB, and must be a multiple of 512 bytes.The' +
      ' size you give here will be the amount available for your data.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 333
  end
  object lblInstructSizeHidden: TLabel
    Left = 0
    Top = 39
    Width = 341
    Height = 26
    Align = alTop
    Caption = 
      'This is the size of the hidden container. '#13'Please see the help f' +
      'or minimum sizes of hidden containers '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 279
  end
  object lblInstructSizeNotHidden: TLabel
    Left = 0
    Top = 65
    Width = 341
    Height = 13
    Align = alTop
    Caption = 'The actual container created will be slightly larger than this.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 278
  end
  object ckSizeEntirePartitionDisk: TCheckBox
    Left = 0
    Top = 125
    Width = 147
    Height = 17
    Caption = '&Use entire partition/disk'
    TabOrder = 0
    OnClick = ckSizeEntirePartitionDiskClick
  end
  object se64UnitSize: TSDUSpin64Unit_Storage
    Left = 3
    Top = 167
    Width = 226
    Height = 26
    TabOrder = 1
    Units.Strings = (
      'bytes'
      'KB'
      'MB'
      'GB'
      'TB')
    SelectedUnits = 'MB'
    MaxLength = 0
    MaxValue = 65536
    Value = 26214400
    ReadOnly = False
    OnChange = se64UnitSizeChange
  end
end
