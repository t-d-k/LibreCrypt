object frmHdrDump: TfrmHdrDump
  Left = 493
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Dump LUKS Details'
  ClientHeight = 353
  ClientWidth = 537
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
  DesignSize = (
    537
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object pbOK: TButton
    Left = 360
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
  end
  object pbCancel: TButton
    Left = 454
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 531
    Height = 213
    Align = alTop
    Caption = 'Container/keyfile details'
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 465
    inline OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect
      Left = 2
      Top = 15
      Width = 527
      Height = 196
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 3
      ExplicitTop = 20
      ExplicitWidth = 455
      ExplicitHeight = 45
      DesignSize = (
        527
        196)
      inherited bbBrowsePartition: TBitBtn
        Left = 503
        Top = 3
        ExplicitLeft = 431
        ExplicitTop = 3
      end
      inherited bbBrowseFile: TBitBtn
        Left = 476
        Top = 3
        ExplicitLeft = 404
        ExplicitTop = 3
      end
      inherited edFilename: TEdit
        Left = 3
        Top = 3
        Width = 467
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 395
      end
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 222
    Width = 531
    Height = 70
    Align = alTop
    Caption = 'Dump to file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitTop = 219
    ExplicitWidth = 516
    inline feDumpFilename: TSDUFilenameEdit
      Left = 2
      Top = 15
      Width = 527
      Height = 53
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 88
      ExplicitTop = 20
      ExplicitWidth = 401
      ExplicitHeight = 34
      DesignSize = (
        527
        53)
      inherited edFilename: TEdit
        Left = 3
        Width = 451
        Height = 22
        ExplicitLeft = 3
        ExplicitWidth = 430
        ExplicitHeight = 22
      end
      inherited pbBrowse: TButton
        Left = 469
        Width = 55
        Height = 22
        ExplicitLeft = 448
        ExplicitWidth = 55
        ExplicitHeight = 22
      end
    end
  end
end
