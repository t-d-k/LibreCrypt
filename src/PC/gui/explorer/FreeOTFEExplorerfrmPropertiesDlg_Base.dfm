object frmPropertiesDialog_Base: TfrmPropertiesDialog_Base
  Left = 290
  Top = 243
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 475
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbOK: TButton
    Left = 108
    Top = 444
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbApply: TButton
    Left = 276
    Top = 444
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = pbApplyClick
  end
  object pbCancel: TButton
    Left = 192
    Top = 444
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 4
    Width = 349
    Height = 433
    ActivePage = tsGeneral
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'General'
      object lblFileType: TLabel
        Left = 12
        Top = 64
        Width = 58
        Height = 13
        Caption = 'Type of file:'
        FocusControl = edFileType
      end
      object lblSizeOnDisk: TLabel
        Left = 12
        Top = 180
        Width = 59
        Height = 13
        Caption = 'Size on disk:'
        FocusControl = edSizeOnDisk
      end
      object lblSize: TLabel
        Left = 12
        Top = 156
        Width = 23
        Height = 13
        Caption = 'Size:'
        FocusControl = edSize
      end
      object lblLocation: TLabel
        Left = 12
        Top = 132
        Width = 44
        Height = 13
        Caption = 'Location:'
        FocusControl = edLocation
      end
      object lblAttributes: TLabel
        Left = 12
        Top = 308
        Width = 52
        Height = 13
        Caption = 'Attributes:'
      end
      object imgFileType: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
      end
      object edFileType: TLabel
        Left = 108
        Top = 64
        Width = 52
        Height = 13
        Caption = 'edFileType'
      end
      object edLocation: TLabel
        Left = 108
        Top = 132
        Width = 52
        Height = 13
        Caption = 'edLocation'
      end
      object edSizeOnDisk: TLabel
        Left = 108
        Top = 180
        Width = 64
        Height = 13
        Caption = 'edSizeOnDisk'
      end
      object edSize: TLabel
        Left = 108
        Top = 156
        Width = 31
        Height = 13
        Caption = 'edSize'
      end
      object ckReadOnly: TCheckBox
        Left = 88
        Top = 308
        Width = 81
        Height = 17
        Caption = '&Read-only'
        TabOrder = 3
      end
      object ckHidden: TCheckBox
        Left = 176
        Top = 308
        Width = 97
        Height = 17
        Caption = '&Hidden'
        TabOrder = 4
      end
      object edFilename: TEdit
        Left = 104
        Top = 20
        Width = 225
        Height = 21
        TabOrder = 0
        Text = 'edFilename'
      end
      object Panel2: TPanel
        Left = 12
        Top = 292
        Width = 317
        Height = 9
        Caption = 'Panel2'
        TabOrder = 2
      end
      object Panel3: TPanel
        Left = 12
        Top = 48
        Width = 317
        Height = 9
        Caption = 'Panel3'
        TabOrder = 1
      end
      object ckArchive: TCheckBox
        Left = 260
        Top = 308
        Width = 73
        Height = 17
        Caption = '&Archive'
        TabOrder = 5
      end
    end
  end
end
