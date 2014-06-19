object frmFreeOTFESelectOverwriteMethod: TfrmFreeOTFESelectOverwriteMethod
  Left = 454
  Top = 330
  BorderStyle = bsDialog
  Caption = 'Select Type of Overwrite'
  ClientHeight = 305
  ClientWidth = 271
  Color = clBtnFace
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
  PixelsPerInch = 96
  TextHeight = 13
  object pbOK: TButton
    Left = 56
    Top = 268
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 144
    Top = 268
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 29
    Top = 156
    Width = 213
    Height = 97
    Caption = 'Type of overwrite data'
    TabOrder = 2
    object rbDataEncrypted: TRadioButton
      Left = 12
      Top = 40
      Width = 193
      Height = 17
      Caption = 'Encrypted data'
      TabOrder = 0
      OnClick = ControlChanged
    end
    object rbDataPseudorandom: TRadioButton
      Left = 12
      Top = 20
      Width = 193
      Height = 17
      Caption = 'Pseudorandom data'
      TabOrder = 1
      OnClick = ControlChanged
    end
    object cbCypher: TComboBox
      Left = 32
      Top = 60
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = ControlChanged
    end
    object pbCypherDetails: TButton
      Left = 176
      Top = 60
      Width = 21
      Height = 21
      Caption = '?'
      TabOrder = 3
      OnClick = pbCypherDetailsClick
    end
  end
  object reInstructOverwriteType: TOTFEFreeOTFE_InstructionRichEdit
    Left = 12
    Top = 12
    Width = 245
    Height = 137
    Lines.Strings = (
      'reInstructOverwriteType')
    TabOrder = 3
  end
end
