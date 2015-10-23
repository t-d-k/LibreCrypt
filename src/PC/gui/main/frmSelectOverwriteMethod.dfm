object frmSelectOverwriteMethod: TfrmSelectOverwriteMethod
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblInstructOverwriteType1: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 261
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 
      'Please select the type of data that should be used to overwrite ' +
      'the free space:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 251
  end
  object lblInstructOverwriteType2: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 41
    Width = 261
    Height = 39
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 
      'Pseudorandom data - This is faster, but less secure if you wish ' +
      'to create a hidden container within this container later.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 251
  end
  object lblInstructOverwriteType3: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 90
    Width = 261
    Height = 39
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 
      'Encrypted data - This is more secure, but slower. Pseudorandom d' +
      'ata will be encrypted with your choice of cypher before being wr' +
      'itten to the drive.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 260
  end
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
    Left = 8
    Top = 144
    Width = 255
    Height = 109
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
      Top = 17
      Width = 193
      Height = 17
      Caption = 'Pseudorandom data'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = ControlChanged
    end
    object cbCypher: TComboBox
      Left = 12
      Top = 63
      Width = 193
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = ControlChanged
    end
    object pbCypherDetails: TButton
      Left = 211
      Top = 63
      Width = 21
      Height = 21
      Caption = '?'
      TabOrder = 3
      OnClick = pbCypherDetailsClick
    end
  end
end
