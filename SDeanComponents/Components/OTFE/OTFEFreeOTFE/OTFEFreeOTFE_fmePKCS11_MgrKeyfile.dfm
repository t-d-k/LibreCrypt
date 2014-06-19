inherited fmePKCS11_MgrKeyfile: TfmePKCS11_MgrKeyfile
  Width = 323
  Height = 266
  OnResize = FrameResize
  DesignSize = (
    323
    266)
  object Label1: TLabel [0]
    Left = 12
    Top = 8
    Width = 277
    Height = 13
    Caption = 'The following &keyfiles are stored on your PKCS#11 token:'
    FocusControl = lbCDB
  end
  object pbImport: TButton [1]
    Left = 12
    Top = 228
    Width = 75
    Height = 25
    Action = actImport
    Anchors = [akLeft, akBottom]
    TabOrder = 1
  end
  object pbDelete: TButton [2]
    Left = 232
    Top = 228
    Width = 78
    Height = 25
    Action = actDelete
    Anchors = [akRight, akBottom]
    TabOrder = 3
  end
  object pbExport: TButton [3]
    Left = 122
    Top = 228
    Width = 75
    Height = 25
    Action = actExport
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object lbCDB: TListBox [4]
    Left = 12
    Top = 32
    Width = 297
    Height = 181
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnClick = lbCDBClick
  end
  inherited ActionList1: TActionList
    object actImport: TAction
      Caption = '&Import...'
      OnExecute = actImportExecute
    end
    object actExport: TAction
      Caption = '&Export...'
      OnExecute = actExportExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
    end
  end
  inherited PopupMenu1: TPopupMenu
    object Import1: TMenuItem [0]
      Action = actImport
    end
    object Export1: TMenuItem [1]
      Action = actExport
    end
    object N2: TMenuItem [2]
      Caption = '-'
    end
    object Delete1: TMenuItem [3]
      Action = actDelete
    end
    object N1: TMenuItem [4]
      Caption = '-'
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 40
    Top = 212
  end
  object SaveDialog: TSDUSaveDialog
    PreserveCWD = False
    Left = 132
    Top = 212
  end
end
