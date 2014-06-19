object Form1: TForm1
  Left = 485
  Top = 384
  Caption = 'TITLE SET AUTOMATICALLY'
  ClientHeight = 362
  ClientWidth = 203
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pbExit: TButton
    Left = 64
    Top = 328
    Width = 75
    Height = 25
    Action = actExit
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 112
    Width = 185
    Height = 97
    Caption = 'Programatic test operations'
    TabOrder = 1
    object pbAppMinimise: TButton
      Left = 12
      Top = 24
      Width = 75
      Height = 25
      Caption = 'App minimize'
      TabOrder = 0
      OnClick = pbAppMinimiseClick
    end
    object pbClose: TButton
      Left = 96
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = pbCloseClick
    end
    object pbWSMinimise: TButton
      Left = 12
      Top = 56
      Width = 75
      Height = 25
      Caption = 'WS minimize'
      TabOrder = 2
      OnClick = pbWSMinimiseClick
    end
    object pbHide: TButton
      Left = 96
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Hide'
      TabOrder = 3
      OnClick = pbHideClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 93
    Caption = 'System tray icon options'
    TabOrder = 0
    object ckDisplaySystemTrayIconIcon: TCheckBox
      Left = 12
      Top = 20
      Width = 153
      Height = 17
      Caption = 'Display system tray icon'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = UpdateSystemTrayIconSettings
    end
    object ckCloseToIcon: TCheckBox
      Left = 32
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Close to icon'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = UpdateSystemTrayIconSettings
    end
    object ckMinToIcon: TCheckBox
      Left = 32
      Top = 60
      Width = 121
      Height = 17
      Caption = 'Minimize to icon'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = UpdateSystemTrayIconSettings
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 220
    Width = 185
    Height = 97
    Caption = 'Child forms'
    TabOrder = 2
    object pbChildModal: TButton
      Left = 55
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Modal'
      TabOrder = 0
      OnClick = pbChildModalClick
    end
    object pbChildNonModal: TButton
      Left = 55
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Non modal'
      TabOrder = 1
      OnClick = pbChildNonModalClick
    end
  end
  object SDUSystemTrayIcon1: TSDUSystemTrayIcon
    Active = True
    MinimizeToIcon = False
    PopupMenu = PopupMenu1
    AnimateIcon = False
    AnimationDelay = 1000
    OnDblClick = actDisplayExecute
    Left = 4
    Top = 324
  end
  object PopupMenu1: TPopupMenu
    Left = 36
    Top = 324
    object miDisplay: TMenuItem
      Action = actDisplay
    end
    object miExit: TMenuItem
      Action = actExit
    end
  end
  object ActionList1: TActionList
    Left = 156
    Top = 324
    object actExit: TAction
      Caption = 'E&xit'
      OnExecute = actExitExecute
    end
    object actDisplay: TAction
      Caption = '&Display'
      OnExecute = actDisplayExecute
    end
  end
end
