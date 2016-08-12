inherited SDUFilenameEdit: TSDUFilenameEdit
  Width = 304
  Height = 32
  OnEnter = FrameEnter
  ExplicitWidth = 304
  ExplicitHeight = 32
  DesignSize = (
    304
    32)
  object edFilename: TEdit
    Left = 3
    Top = 5
    Width = 247
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edFilename'
    OnChange = edFilenameChange
  end
  object pbBrowse: TButton
    Left = 256
    Top = 3
    Width = 45
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = pbBrowseClick
  end
  object OpenDialog1: TSDUOpenDialog
    PreserveCWD = False
    Left = 72
  end
  object SaveDialog1: TSDUSaveDialog
    PreserveCWD = False
    Left = 108
  end
end
