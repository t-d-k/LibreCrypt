object OTFETrueCryptMountDevice9x_F: TOTFETrueCryptMountDevice9x_F
  Left = 541
  Top = 321
  BorderStyle = bsDialog
  Caption = 'Mount Device'
  ClientHeight = 183
  ClientWidth = 174
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    174
    183)
  PixelsPerInch = 96
  TextHeight = 13
  object pbOK: TButton
    Left = 8
    Top = 151
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 92
    Top = 151
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 157
    Height = 137
    Caption = 'Select device'
    TabOrder = 0
    object lblDrive: TLabel
      Left = 40
      Top = 80
      Width = 28
      Height = 13
      Caption = '&Drive:'
      FocusControl = cbDrive
    end
    object lblPartition: TLabel
      Left = 40
      Top = 104
      Width = 41
      Height = 13
      Caption = 'P&artition:'
      FocusControl = cbPartition
    end
    object cbDrive: TComboBox
      Left = 88
      Top = 80
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbDriveChange
    end
    object cbPartition: TComboBox
      Left = 88
      Top = 104
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
    end
    object rbFloppyA: TRadioButton
      Left = 12
      Top = 20
      Width = 113
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'Floppy &A:'
      ParentBiDiMode = False
      TabOrder = 0
      OnClick = rbClick
    end
    object rbFloppyB: TRadioButton
      Left = 12
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Floppy &B:'
      TabOrder = 1
      OnClick = rbClick
    end
    object rbPartition: TRadioButton
      Left = 12
      Top = 60
      Width = 113
      Height = 17
      Caption = '&Partition'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbClick
    end
  end
end
