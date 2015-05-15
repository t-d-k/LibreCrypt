object fmePKCS11_MgrBase: TfmePKCS11Mgr
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object ActionList1: TActionList
    Left = 148
    Top = 84
    object actRefresh: TAction
      Caption = '&Refresh'
      ShortCut = 16466
      OnExecute = actRefreshExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 84
    object Refresh1: TMenuItem
      Action = actRefresh
    end
  end
end
