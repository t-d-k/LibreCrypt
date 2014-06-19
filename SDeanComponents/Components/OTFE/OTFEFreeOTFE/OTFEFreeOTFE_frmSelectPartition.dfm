object frmSelectPartition: TfrmSelectPartition
  Left = 205
  Top = 147
  Width = 592
  Height = 242
  Caption = 'Select Partition'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    584
    215)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 225
    Height = 13
    Caption = 'Please select from the following disks/partitions:'
  end
  object pnlButtonCenter: TPanel
    Left = 48
    Top = 172
    Width = 189
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'pnlButtonCenter'
    TabOrder = 1
    object pbOK: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = pbOKClick
    end
    object pbCancel: TButton
      Left = 100
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  inline fmeSelectPartition: TfmeSelectPartition
    Left = 8
    Top = 28
    Width = 565
    Height = 144
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    inherited lblErrorWarning: TLabel
      Top = 125
    end
    inherited imgErrorWarning: TImage
      Top = 123
    end
    inherited TabControl1: TTabControl
      Height = 97
      inherited SDUDiskPartitionsPanel1: TOTFEFreeOTFEDiskPartitionsPanel
        Height = 69
        OnDblClick = fmeSelectPartitionSDUDiskPartitionsPanel1DblClick
      end
      inherited pnlNoPartitionDisplay: TPanel
        Height = 69
        OnDblClick = fmeSelectPartitionpnlNoPartitionDisplayDblClick
      end
    end
    inherited ckShowCDROM: TCheckBox
      Top = 105
    end
    inherited ckEntireDisk: TCheckBox
      Top = 105
    end
  end
end
