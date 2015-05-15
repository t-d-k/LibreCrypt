object frmSelectPartition_Simple: TfrmUserSelectPartitionSimple
  Left = 617
  Top = 273
  BorderStyle = bsDialog
  Caption = 'Select Partition'
  ClientHeight = 251
  ClientWidth = 220
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 196
    Height = 13
    Caption = 'Please select from the following &partitions:'
    FocusControl = lbPartition
  end
  object pbOK: TButton
    Left = 27
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 119
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object lbPartition: TListBox
    Left = 8
    Top = 28
    Width = 201
    Height = 161
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbPartitionClick
    OnDblClick = lbPartitionDblClick
  end
  object ckListRemovable: TCheckBox
    Left = 61
    Top = 193
    Width = 97
    Height = 17
    Caption = '&List removable'
    TabOrder = 1
    OnClick = ckListRemovableClick
  end
end
