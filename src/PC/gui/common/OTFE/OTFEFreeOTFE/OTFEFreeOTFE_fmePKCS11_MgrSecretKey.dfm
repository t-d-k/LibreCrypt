inherited fmePKCS11_MgrSecretKey: TfmePKCS11_MgrSecretKey
  Width = 323
  Height = 338
  ExplicitWidth = 323
  ExplicitHeight = 338
  DesignSize = (
    323
    338)
  object Label1: TLabel [0]
    Left = 12
    Top = 8
    Width = 296
    Height = 13
    Caption = 'The following secret &keys are stored on your PKCS#11 token:'
  end
  object lvSecretKeys: TListView [1]
    Left = 12
    Top = 32
    Width = 297
    Height = 181
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnSelectItem = lvSecretKeysSelectItem
  end
  object gbSecretKey: TGroupBox [2]
    Left = 12
    Top = 224
    Width = 141
    Height = 101
    Anchors = [akLeft, akBottom]
    Caption = 'Secret Key'
    TabOrder = 1
    object pbNew: TButton
      Left = 32
      Top = 24
      Width = 75
      Height = 25
      Action = actNew
      TabOrder = 0
    end
    object pbDelete: TButton
      Left = 32
      Top = 60
      Width = 75
      Height = 25
      Action = actDelete
      TabOrder = 1
    end
  end
  object gbKeyfile: TGroupBox [3]
    Left = 169
    Top = 224
    Width = 141
    Height = 101
    Anchors = [akRight, akBottom]
    Caption = 'Keyfile/volume'
    TabOrder = 2
    object pbEncrypt: TButton
      Left = 33
      Top = 24
      Width = 75
      Height = 25
      Action = actEncrypt
      TabOrder = 0
    end
    object pbDecrypt: TButton
      Left = 33
      Top = 60
      Width = 75
      Height = 25
      Action = actDecrypt
      TabOrder = 1
    end
  end
  inherited ActionList1: TActionList
    object actNew: TAction
      Caption = '&New...'
      OnExecute = actNewExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
    end
    object actEncrypt: TAction
      Caption = '&Secure...'
      OnExecute = actEncryptExecute
    end
    object actDecrypt: TAction
      Caption = 'D&esecure'
      OnExecute = actDecryptExecute
    end
  end
  inherited PopupMenu1: TPopupMenu
    object New1: TMenuItem [0]
      Action = actNew
    end
    object Delete1: TMenuItem [1]
      Action = actDelete
    end
    object N1: TMenuItem [2]
      Caption = '-'
    end
    object Secure1: TMenuItem [3]
      Action = actEncrypt
    end
    object Desecure1: TMenuItem [4]
      Action = actDecrypt
    end
    object N2: TMenuItem [5]
      Caption = '-'
    end
  end
end
