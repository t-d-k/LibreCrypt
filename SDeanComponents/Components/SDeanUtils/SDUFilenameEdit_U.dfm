inherited SDUFilenameEdit: TSDUFilenameEdit
  Width = 307
  Height = 122
  OnEnter = FrameEnter
  OnResize = FrameResize
  object edFilename: TEdit
    Left = 24
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'edFilename'
    OnChange = edFilenameChange
  end
  object pbBrowse: TButton
    Left = 172
    Top = 16
    Width = 75
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = pbBrowseClick
  end
  object OpenDialog1: TSDUOpenDialog
    PreserveCWD = False
    Left = 72
    Top = 56
  end
  object SaveDialog1: TSDUSaveDialog
    PreserveCWD = False
    Left = 116
    Top = 56
  end
end
