object frmNewVolumeSize: TfrmNewVolumeSize
  Left = 344
  Top = 221
  BorderStyle = bsDialog
  Caption = 'Create Container'
  ClientHeight = 134
  ClientWidth = 311
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
    311
    134)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 19
    Width = 45
    Height = 13
    Caption = '&Filename:'
    FocusControl = feFilename
  end
  object Label3: TLabel
    Left = 11
    Top = 56
    Width = 69
    Height = 13
    Caption = 'Container &size:'
  end
  object pbCancel: TButton
    Left = 222
    Top = 101
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 269
    ExplicitTop = 85
  end
  object pbOK: TButton
    Left = 141
    Top = 101
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
    ExplicitLeft = 188
    ExplicitTop = 85
  end
  object se64UnitSize: TSDUSpin64Unit_Storage
    Left = 86
    Top = 55
    Width = 217
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Units.Strings = (
      'bytes'
      'KB'
      'MB'
      'GB'
      'TB')
    SelectedUnits = 'bytes'
    MaxLength = 0
    ReadOnly = False
    ExplicitWidth = 271
  end
  inline feFilename: TSDUFilenameEdit
    Left = 83
    Top = 8
    Width = 217
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitLeft = 83
    ExplicitTop = 8
    ExplicitWidth = 264
    ExplicitHeight = 34
    DesignSize = (
      217
      34)
    inherited edFilename: TEdit
      Width = 118
      ExplicitWidth = 118
    end
    inherited pbBrowse: TButton
      Left = 136
      Width = 78
      ExplicitLeft = 136
      ExplicitWidth = 78
    end
    inherited OpenDialog1: TSDUOpenDialog
      Left = 144
    end
    inherited SaveDialog1: TSDUSaveDialog
      Left = 180
    end
  end
end
