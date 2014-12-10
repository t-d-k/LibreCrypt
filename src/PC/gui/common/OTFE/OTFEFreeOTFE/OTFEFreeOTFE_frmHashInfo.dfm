object frmHashInfo: TfrmHashInfo
  Left = 312
  Top = 200
  BorderStyle = bsDialog
  Caption = 'Hash Details'
  ClientHeight = 418
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
  object gbHashDriver: TGroupBox
    Left = 12
    Top = 8
    Width = 521
    Height = 201
    Caption = 'Hash Driver'
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
      Width = 89
      Height = 13
      Caption = 'Hashes supported:'
    end
    object Label9: TLabel
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
      TabOrder = 1
      Text = 'edDriverDeviceName'
    end
    object edDriverDeviceUserModeName: TEdit
      Left = 140
      Top = 68
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'edDriverDeviceUserModeName'
    end
    object edDriverDeviceKernelModeName: TEdit
      Left = 140
      Top = 92
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'edDriverDeviceKernelModeName'
    end
    object edDriverTitle: TEdit
      Left = 140
      Top = 116
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'edDriverTitle'
    end
    object edDriverVersionID: TEdit
      Left = 140
      Top = 140
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
      Text = 'edDriverVersionID'
    end
    object edDriverHashCount: TEdit
      Left = 140
      Top = 164
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 6
      Text = 'edDriverHashCount'
    end
    object edDriverGUID: TEdit
      Left = 140
      Top = 20
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'edDriverGUID'
    end
  end
  object gbHash: TGroupBox
    Left = 12
    Top = 216
    Width = 521
    Height = 153
    Caption = 'Hash'
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
    object Label10: TLabel
      Left = 12
      Top = 96
      Width = 36
      Height = 13
      Caption = 'Length:'
    end
    object Label11: TLabel
      Left = 12
      Top = 72
      Width = 38
      Height = 13
      Caption = 'Version:'
    end
    object Label12: TLabel
      Left = 12
      Top = 120
      Width = 51
      Height = 13
      Caption = 'Block size:'
    end
    object edHashGUID: TEdit
      Left = 140
      Top = 20
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'edHashGUID'
    end
    object edHashTitle: TEdit
      Left = 140
      Top = 44
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'edHashTitle'
    end
    object edHashLength: TEdit
      Left = 140
      Top = 92
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'edHashLength'
    end
    object edHashVersionID: TEdit
      Left = 140
      Top = 68
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'edHashVersionID'
    end
    object edHashBlockSize: TEdit
      Left = 140
      Top = 116
      Width = 365
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'edHashBlockSize'
    end
  end
  object pbClose: TButton
    Left = 235
    Top = 380
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    OnClick = pbCloseClick
  end
end
