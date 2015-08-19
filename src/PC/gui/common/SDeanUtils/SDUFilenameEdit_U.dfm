inherited SDUFilenameEdit: TSDUFilenameEdit
  Width = 304
  Height = 43
  OnEnter = FrameEnter
  ExplicitWidth = 304
  ExplicitHeight = 43
  DesignSize = (
    304
    43)
  object edFilename: TEdit
    Left = 16
    Top = 3
    Width = 192
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edFilename'
    OnChange = edFilenameChange
  end
  object pbBrowse: TButton
    Left = 214
    Top = 3
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = pbBrowseClick
  end
  object OpenDialog1: TSDUOpenDialog
    PreserveCWD = False
    Left = 72
    Top = 8
  end
  object SaveDialog1: TSDUSaveDialog
    PreserveCWD = False
    Left = 100
    Top = 8
  end
end
