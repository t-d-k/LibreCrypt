object frmFreeOTFEVolProperties: TfrmFreeOTFEVolProperties
  Left = 322
  Top = 353
  BorderStyle = bsDialog
  Caption = 'DoxBox Volume Properties'
  ClientHeight = 264
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    415
    264)
  PixelsPerInch = 96
  TextHeight = 13
  object Label15: TLabel
    Left = 8
    Top = 60
    Width = 60
    Height = 13
    Caption = '&Mounted for:'
    FocusControl = edReadOnly
  end
  object Label22: TLabel
    Left = 8
    Top = 12
    Width = 28
    Height = 13
    Caption = '&Drive:'
    FocusControl = edDrive
  end
  object Label5: TLabel
    Left = 8
    Top = 36
    Width = 38
    Height = 13
    Caption = '&Volume:'
    FocusControl = edVolumeFile
  end
  object Label7: TLabel
    Left = 8
    Top = 180
    Width = 95
    Height = 13
    Caption = 'M&ounted on device:'
    FocusControl = edDeviceName
  end
  object Label6: TLabel
    Left = 8
    Top = 156
    Width = 61
    Height = 13
    Caption = '&Main cypher:'
    FocusControl = edMainCypher
  end
  object lblIVHash: TLabel
    Left = 8
    Top = 108
    Width = 84
    Height = 13
    Caption = '&IV hash algorithm:'
    FocusControl = edIVHash
  end
  object Label2: TLabel
    Left = 8
    Top = 84
    Width = 52
    Height = 13
    Caption = '&Sector IVs:'
    FocusControl = edSectorIVGenMethod
  end
  object lblIVCypher: TLabel
    Left = 8
    Top = 132
    Width = 48
    Height = 13
    Caption = 'I&V cypher:'
    FocusControl = edIVCypher
  end
  object Label1: TLabel
    Left = 8
    Top = 204
    Width = 105
    Height = 13
    Caption = 'Default Hidden Offset:'
    FocusControl = edDeviceName
  end
  object pbOK: TButton
    Left = 168
    Top = 233
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object edDrive: TEdit
    Left = 140
    Top = 8
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    Text = 'edDrive'
  end
  object edDeviceName: TEdit
    Left = 140
    Top = 176
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
    Text = 'edDeviceName'
  end
  object edVolumeFile: TEdit
    Left = 140
    Top = 32
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    Text = 'edVolumeFile'
  end
  object edReadOnly: TEdit
    Left = 140
    Top = 56
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'edReadOnly'
  end
  object edMainCypher: TEdit
    Left = 140
    Top = 152
    Width = 247
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
    Text = 'edMainCypher'
  end
  object pbInfoMainCypher: TButton
    Left = 387
    Top = 152
    Width = 20
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '?'
    TabOrder = 8
    OnClick = pbInfoMainCypherClick
  end
  object pbInfoIVHash: TButton
    Left = 387
    Top = 104
    Width = 20
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '?'
    TabOrder = 6
    OnClick = pbInfoIVHashClick
  end
  object edIVHash: TEdit
    Left = 140
    Top = 104
    Width = 247
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'edIVHash'
  end
  object edSectorIVGenMethod: TEdit
    Left = 140
    Top = 80
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = 'edSectorIVGenMethod'
  end
  object pbInfoIVCypher: TButton
    Left = 387
    Top = 128
    Width = 20
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '?'
    TabOrder = 10
    OnClick = pbInfoIVCypherClick
  end
  object edIVCypher: TEdit
    Left = 140
    Top = 128
    Width = 247
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
    Text = 'edIVCypher'
  end
  object edHiddenOffset: TEdit
    Left = 140
    Top = 200
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 12
    Text = 'edHiddenOffset'
  end
end
