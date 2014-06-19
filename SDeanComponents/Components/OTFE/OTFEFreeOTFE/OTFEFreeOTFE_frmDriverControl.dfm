object frmDriverControl: TfrmDriverControl
  Left = 341
  Top = 282
  BorderStyle = bsDialog
  Caption = 'FreeOTFE Driver Control'
  ClientHeight = 369
  ClientWidth = 498
  Color = clBtnFace
  Constraints.MinHeight = 388
  Constraints.MinWidth = 444
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    498
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 211
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = pbCloseClick
  end
  object gbInstallNew: TGroupBox
    Left = 8
    Top = 8
    Width = 481
    Height = 85
    Caption = 'Install new driver'
    TabOrder = 0
    object lblInstall: TLabel
      Left = 9
      Top = 20
      Width = 287
      Height = 13
      Caption = 
        'To install a new hash or cypher driver, please click "Install...' +
        '"'
    end
    object pbInstall: TButton
      Left = 203
      Top = 44
      Width = 75
      Height = 25
      Caption = '&Install...'
      TabOrder = 0
      OnClick = pbInstallClick
    end
  end
  object gbModifyExisting: TGroupBox
    Left = 8
    Top = 104
    Width = 481
    Height = 217
    Caption = 'Modify existing driver'
    TabOrder = 1
    DesignSize = (
      481
      217)
    object lblStart: TLabel
      Left = 272
      Top = 36
      Width = 66
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Driver &startup:'
      FocusControl = cbStartup
    end
    object Label1: TLabel
      Left = 12
      Top = 20
      Width = 73
      Height = 13
      Caption = '&Existing drivers:'
      FocusControl = lbDrivers
    end
    object Label3: TLabel
      Left = 272
      Top = 92
      Width = 100
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Change driver status:'
    end
    object Label4: TLabel
      Left = 272
      Top = 156
      Width = 72
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Remove driver:'
    end
    object lbDrivers: TListBox
      Left = 12
      Top = 40
      Width = 245
      Height = 165
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbDriversClick
      OnDrawItem = lbDriversDrawItem
    end
    object pbStart: TButton
      Left = 280
      Top = 112
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'St&art'
      TabOrder = 3
      OnClick = pbStartClick
    end
    object pbStop: TButton
      Left = 380
      Top = 112
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'St&op'
      TabOrder = 4
      OnClick = pbStopClick
    end
    object pbUninstall: TButton
      Left = 328
      Top = 176
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'U&ninstall'
      TabOrder = 5
      OnClick = pbUninstallClick
    end
    object Panel1: TPanel
      Left = 268
      Top = 84
      Width = 201
      Height = 2
      Anchors = [akTop, akRight]
      BevelOuter = bvLowered
      Caption = 'Panel1'
      TabOrder = 6
    end
    object Panel2: TPanel
      Left = 268
      Top = 148
      Width = 201
      Height = 2
      Anchors = [akTop, akRight]
      BevelOuter = bvLowered
      Caption = 'Panel1'
      TabOrder = 7
    end
    object pbUpdate: TButton
      Left = 392
      Top = 51
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Update'
      TabOrder = 2
      OnClick = pbUpdateClick
    end
    object cbStartup: TComboBox
      Left = 272
      Top = 52
      Width = 113
      Height = 22
      Style = csOwnerDrawFixed
      Anchors = [akTop, akRight]
      ItemHeight = 16
      TabOrder = 1
      OnChange = cbStartupChange
    end
  end
  object OpenDialog: TSDUOpenDialog
    PreserveCWD = False
    Left = 260
    Top = 56
  end
end
