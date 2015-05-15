object frmCypherInfo: TfrmCypherInfo
  Left = 360
  Top = 183
  BorderStyle = bsDialog
  Caption = 'Cypher Details'
  ClientHeight = 447
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbCypherDriver: TGroupBox
    Left = 12
    Top = 8
    Width = 521
    Height = 201
    Caption = 'Cypher Driver'
    TabOrder = 1
    object lblDeviceName: TLabel
      Left = 12
      Top = 48
      Width = 65
      Height = 13
      Caption = 'DeviceName:'
    end
    object lblDeviceUserModeName: TLabel
      Left = 12
      Top = 72
      Width = 114
      Height = 13
      Caption = 'DeviceUserModeName:'
    end
    object lblDeviceKernelModeName: TLabel
      Left = 12
      Top = 96
      Width = 122
      Height = 13
      Caption = 'DeviceKernelModeName:'
    end
    object Label4: TLabel
      Left = 12
      Top = 120
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object Label5: TLabel
      Left = 12
      Top = 144
      Width = 38
      Height = 13
      Caption = 'Version:'
    end
    object Label6: TLabel
      Left = 12
      Top = 168
      Width = 91
      Height = 13
      Caption = 'Cyphers supported:'
    end
    object Label12: TLabel
      Left = 12
      Top = 24
      Width = 30
      Height = 13
      Caption = 'GUID:'
    end
    object edDriverDeviceName: TEdit
      Left = 140
      Top = 44
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'edDriverDeviceName'
    end
    object edDriverDeviceUserModeName: TEdit
      Left = 140
      Top = 68
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'edDriverDeviceUserModeName'
    end
    object edDriverDeviceKernelModeName: TEdit
      Left = 140
      Top = 92
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'edDriverDeviceKernelModeName'
    end
    object edDriverTitle: TEdit
      Left = 140
      Top = 116
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'edDriverTitle'
    end
    object edDriverVersionID: TEdit
      Left = 140
      Top = 140
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'edDriverVersionID'
    end
    object edDriverCypherCount: TEdit
      Left = 140
      Top = 164
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
      Text = 'edDriverCypherCount'
    end
    object edDriverGUID: TEdit
      Left = 140
      Top = 20
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 6
      Text = 'edDriverGUID'
    end
  end
  object gbCypher: TGroupBox
    Left = 12
    Top = 216
    Width = 521
    Height = 177
    Caption = 'Cypher'
    TabOrder = 2
    object Label7: TLabel
      Left = 12
      Top = 48
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object Label8: TLabel
      Left = 12
      Top = 24
      Width = 30
      Height = 13
      Caption = 'GUID:'
    end
    object Label9: TLabel
      Left = 12
      Top = 72
      Width = 30
      Height = 13
      Caption = 'Mode:'
    end
    object Label10: TLabel
      Left = 12
      Top = 120
      Width = 48
      Height = 13
      Caption = 'Blocksize:'
    end
    object Label11: TLabel
      Left = 12
      Top = 144
      Width = 38
      Height = 13
      Caption = 'Version:'
    end
    object Label13: TLabel
      Left = 12
      Top = 96
      Width = 39
      Height = 13
      Caption = 'Keysize:'
    end
    object edCypherGUID: TEdit
      Left = 140
      Top = 20
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'edCypherGUID'
    end
    object edCypherTitle: TEdit
      Left = 140
      Top = 44
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'edCypherTitle'
    end
    object edCypherMode: TEdit
      Left = 140
      Top = 68
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'edCypherMode'
    end
    object edCypherBlockSize: TEdit
      Left = 140
      Top = 116
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'edCypherBlockSize'
    end
    object edCypherVersionID: TEdit
      Left = 140
      Top = 140
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'edCypherVersionID'
    end
    object edCypherKeySize: TEdit
      Left = 140
      Top = 92
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
      Text = 'edCypherKeySize'
    end
  end
  object pbClose: TButton
    Left = 235
    Top = 408
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = pbCloseClick
  end
end
