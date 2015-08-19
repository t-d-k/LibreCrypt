object frmHdrDump: TfrmHdrDump
  Left = 493
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Dump LUKS Details'
  ClientHeight = 342
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbOK: TButton
    Left = 160
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
  end
  object pbCancel: TButton
    Left = 248
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 465
    Height = 213
    Caption = 'Container/keyfile details'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 83
      Height = 13
      Caption = '&Container/keyfile:'
    end
    inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
      Left = 116
      Top = 20
      Width = 342
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 116
      ExplicitTop = 20
      ExplicitWidth = 342
      ExplicitHeight = 21
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 241
    Width = 465
    Height = 57
    Caption = 'Dump file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 59
      Height = 13
      Caption = '&Dump to file:'
      FocusControl = feDumpFilename
    end
    inline feDumpFilename: TSDUFilenameEdit
      Left = 88
      Top = 20
      Width = 365
      Height = 34
      TabOrder = 0
      ExplicitLeft = 88
      ExplicitTop = 20
      ExplicitWidth = 365
      ExplicitHeight = 34
      DesignSize = (
        365
        34)
      inherited edFilename: TEdit
        Width = 257
        ExplicitWidth = 257
      end
      inherited pbBrowse: TButton
        Left = 287
        Top = 6
        ExplicitLeft = 287
        ExplicitTop = 6
      end
    end
  end
end
