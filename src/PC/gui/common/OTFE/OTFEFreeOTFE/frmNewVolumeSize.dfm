object frmNewVolumeSize: TfrmNewVolumeSize
  Left = 344
  Top = 221
  BorderStyle = bsDialog
  Caption = 'Create Container'
  ClientHeight = 122
  ClientWidth = 355
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
  object Label2: TLabel
    Left = 8
    Top = 20
    Width = 45
    Height = 13
    Caption = '&Filename:'
    FocusControl = feFilename
  end
  object Label3: TLabel
    Left = 8
    Top = 52
    Width = 59
    Height = 13
    Caption = 'Container &size:'
  end
  object pbCancel: TButton
    Left = 182
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object pbOK: TButton
    Left = 98
    Top = 84
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object se64UnitSize: TSDUSpin64Unit_Storage
    Left = 112
    Top = 48
    Width = 226
    Height = 29
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
  end
  object feFilename: TSDUFilenameEdit
    Left = 112
    Top = 16
    Width = 233
    Height = 21
    Constraints.MaxHeight = 21
    Constraints.MinHeight = 21
    TabOrder = 0
    TabStop = False
    FilenameEditType = fetSave
    FilterIndex = 0
    DesignSize = (
      233
      21)
  end
end
